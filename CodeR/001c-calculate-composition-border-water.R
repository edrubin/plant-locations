
# Goal -------------------------------------------------------------------------
#   Determine which parts of counties' borders are due to bodies of water.

# Notes ------------------------------------------------------------------------
#   - Currently define 'near' water as 0.05km
#   - Currently search in 2.5km-radius circle for 'moving-window average'
#   - For some counties, the linear water file adds a lot of 'erroneous'
#     points where the border is not determined by a body of water (e.g.,
#     Douglas County, NE (i=1073) or Hancock County, OH (i=10)). However,
#     there are counties for which the linear water is necessary, e.g.,
#     Renville County, MN (i=2531).
#   - Coastline may also be important for wide rivers, e.g., Wahkiakum
#     County, WA (i=2). However, the coastline file does not help in this
#     situation.

# Todo list --------------------------------------------------------------------
#   - Area files may need to be st_covers/st_covered_by/st_intersects rather
#     than geo_join. I'm thinking specifically about the case where a county
#     border reaches out into a large lake. The lake's borders will not be
#     'near' the county's borders, even if the lake creates the border. Should
#     check the Great Lakes on this issue (e.g., Huron County, MI, i=).
#   - Many county borders along coasts/lakes do not represent land well. Is
#     that fine?

# R setup ----------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(
    filesstrings, tidyverse,
    sf, fuzzyjoin, geosphere,
    data.table,
    parallel, future, furrr, tictoc,
    magrittr, here
  )

# Set directories --------------------------------------------------------------
  # Temporary directory for unzipping files
  dir.create("~/_tmp")
  dir_tmp = "~/_tmp/"
  # Directory for saving
  dir.create("~/BordersAndWater")
  dir_save = "~/BordersAndWater/"
  # Census data are in a separate directory
  dir_census = here("..", "Census")
  # Linear water directory
  dir_lwater = paste0(dir_census, "/tl_2016_linearwater/")
  # Area water directory
  dir_awater = paste0(dir_census, "/tl_2016_areawater/")
  # Coast line directory
  dir_coast = paste0(dir_census, "/tl_2016_us_coastline/")

# Read county shapefile --------------------------------------------------------
  # All county shapefiles
  co_shp = paste0(
    dir_census,
    "/tl_2016_us_county/tl_2016_us_county.shp"
  ) %>% read_sf()
  # Retain only the lower 48 + DC
  co_shp %<>% filter(
    as.numeric(STATEFP) %in% maps::state.fips$fips
  )
  # Add 5-digit FIPS
  co_shp %<>% mutate(FIPS = paste0(STATEFP, COUNTYFP))

# Read coastline shapefile -----------------------------------------------------
  # Full coast shapefile
  coast_shp = paste0(
    dir_coast,
    "/tl_2016_us_coastline.shp"
  ) %>% read_sf()
  # Retain only the lower 48 + DC
  coast_shp %<>% st_crop(y = st_bbox(co_shp)) %>% st_cast("MULTILINESTRING")

# Load county adjacency file ---------------------------------------------------
  # Load the file
  co_adj = paste0(
    dir_census,
    "/county_adjacency2010.csv"
  ) %>% fread()
  # Pad the FIPS codes to 5 digits
  co_adj[, `:=`(
    fipscounty = fipscounty %>% str_pad(5, "left", 0),
    fipsneighbor = fipsneighbor %>% str_pad(5, "left", 0)
  )]
  # Change FIPS in SD for Shannon to Oglala Lakota County from 46113 to 46102.
  # NOTE: Change occurred May 1, 2015, which is after the adjacency file
  # but before the geographic files.
  # https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html
  co_adj[fipscounty == "46113", `:=`(
    fipscounty = "46102",
    countyname = "Oglala Lakota County, SD"
  )]
  co_adj[fipsneighbor == "46113", `:=`(
    fipsneighbor = "46102",
    neighborname = "Oglala Lakota County, SD"
  )]
  # Drop independent city of Bedford Virginia, which was absorbed
  # by Bedford County, VA (51019)
  co_adj = co_adj[fipscounty != "51515" & fipsneighbor != "51515"]

# Iterate over counties --------------------------------------------------------
# i = 1073
# i = 10
# i = 2531
# i = 2
# i = 1888 # Huron County, MI
# i = 23 # Chatham County, NC
  # Figure out how many counties have finished
  fips_done = dir_save %>% dir() %>% str_extract("[0-9]{5}")
  fips_todo = setdiff(co_shp$FIPS, fips_done)
  rows_todo = (co_shp$FIPS %in% fips_todo) %>% which()
  # Iterate over finished files
  empty_list = mclapply(X = rows_todo, FUN = function(i) {
    # Subset to county i's outline
    i_shp = co_shp[i,]
    # Find the ifelength of i's border
    i_length = i_shp %>% st_cast("MULTILINESTRING") %>% st_length()
    # Convert i_shp to points
    set.seed(12345)
    i_points = i_shp$geometry %>%
      st_cast("MULTILINESTRING") %>%
  # NOTE: Currently using 20 points per km (~50m spacing) for sample
      st_sample(
        size = i_length %>% as.numeric() %>% multiply_by(20/1e3) %>% ceiling(),
        type = "regular"
      ) %>%
      st_cast("POINT") %>%
      st_coordinates() %>%
      data.frame() %>%
      st_as_sf(coords = 1:2)
    st_crs(i_points) = st_crs(i_shp)
    # Get i's 5-digit FIPS code
    i_fips = paste0(i_shp$STATEFP, i_shp$COUNTYFP)
    # Find the FIPS of i's neighbors (includes i's FIPS)
    nbr_fips = co_adj[fipscounty == i_fips, fipsneighbor]
    # File locations for linear and area water files
    l_files = map_chr(
      nbr_fips,
      ~ str_subset(string = dir(dir_lwater, full.names = T), pattern = .x)
    )
    a_files = map_chr(
      nbr_fips,
      ~ str_subset(string = dir(dir_awater, full.names = T), pattern = .x)
    )
    # Concatenate lists
    water_files = c(l_files, a_files)
    # Iterating over water files, find the border points 'near' water
    i_water_points = list()
    for (j in seq_along(water_files)) {
      # Clean up
      invisible(gc())
      # Find the file
      f = water_files[j]
      # Print to screen
      cat(f, "\n")
      # Find the filename of 'f'
      f_name = f %>%
        str_split("//") %>%
        unlist() %>%
        extract(2) %>%
        str_remove_all(".zip")
      # Find the fips of 'f'
      f_fips = str_extract(f, "[0-9]{5}")
      # Unzip file 'f'
      unzip(
        zipfile = f,
        exdir = dir_tmp,
        overwrite = T
      )
      # Load the unzipped shapefile
      f_shp = paste0(dir_tmp, f_name, ".shp") %>% st_read(quiet = T)
      # Covert f_shp to POINT
      f_points = f_shp$geometry %>% st_cast("POINT")
      # Grab the county outline for file f
      f_co = filter(co_shp, paste0(STATEFP, COUNTYFP) == f_fips)
      # Subset i's points to those within ~0.5km of f's outline
      i_subset = i_points %>% filter(
        st_intersects(
          .,
          st_buffer(f_co %>% st_cast("MULTILINESTRING"), dist = 1/150),
          sparse = F
        )
      )
      # Subset f's polygons to those within ~0.5km of i's outline
      f_subset = f_shp %>% filter(
        st_intersects(
          .,
          st_buffer(i_shp %>% st_cast("MULTILINESTRING"), dist = 1/150),
          sparse = F
        )
      )
      # Only continue if i_subset and f_subset are non-empty
      if ((nrow(i_subset) > 0) & (nrow(f_subset) > 0)) {
        # If the file is a linear water file, use distance-based geojoin
        if (str_detect(f, "linearwater")) {
          # Find any border points (in i_subset) that are within 0.05km of
          # water lines (in f_subset)
          #
          # If i_subset has many points (counties with long borders), then
          # break into chucks (geo_join was running into memory issues)
          if (nrow(i_subset) > 1e3) {
            # Create chunks
            a = seq(1, nrow(i_subset), 1e3)
            z = c(seq(1e3, nrow(i_subset), 1e3), nrow(i_subset))
            # Iterate over chunks
            water_border = lapply(
              X = seq_along(a),
              FUN = function(k) {
                geo_join(
                  x = st_coordinates(i_subset[a[k]:z[k],]) %>% as.data.frame(),
                  y = st_coordinates(f_subset) %>% as.data.frame(),
                  max_dist = 0.05,
                  method = "haversine",
                  unit = "km",
                  mode = "inner"
                ) %>%
                select(x = X.x, y = Y.x) %>%
                mutate(file_type = "linear") %T>%
                setDT()
              }
            ) %>% rbindlist() %>% unique()
          } else {
            # Same operation without breaking into chunks
            water_border = geo_join(
              x = st_coordinates(i_subset) %>% as.data.frame(),
              y = st_coordinates(f_subset) %>% as.data.frame(),
              max_dist = 0.05,
              method = "haversine",
              unit = "km",
              mode = "inner"
            ) %>%
            select(x = X.x, y = Y.x) %>%
            mutate(file_type = "linear") %T>%
            setDT()
          }
        }
        # If the file is an area water file, use the border's intersections
        # with the water areas
        if (str_detect(f, "areawater")) {
          # Find any border points (in i_subset) that intersect f_subset
# NOTE: Using a slightly buffered (~50m) union of f_subset here
          water_border = i_subset %>%
            filter(st_intersects(
              .,
              f_subset %>% st_union() %>% st_buffer(dist = 1/2000),
              sparse = F
            )) %>%
            st_coordinates() %>%
            as.data.frame() %>%
            mutate(file_type = "area") %T>%
            setDT() %>% setnames(c("x", "y", "file_type"))
        }
        # Return the border points flagged as proximate to water
        i_water_points[[length(i_water_points) + 1]] = water_border
      }
    }
    i_water_points %<>% rbindlist()
    # Delete temporary files (zipped files still exist)
    invisible(lapply(
      X = str_subset(
        string = dir_tmp %>% dir(full.names = T),
        pattern = water_files %>% str_extract("[0-9]{5}")
      ),
      FUN = file.remove
    ))
    # Repeat exercise IF 'i' has any coastline
    i_coast = st_crop(coast_shp, st_bbox(i_shp))
    if (nrow(i_coast) > 0) {
      # Keep only lines (dropping points and dimensionless objects)
      i_coast %<>%
        filter(st_dimension(.) > 0) %>%
        st_collection_extract(type = c("LINESTRING"))
    }
    if (nrow(i_coast) > 0) {
      # Covert i_coast to POINT via sampling
      coast_length = i_coast %>%
        st_length() %>% sum() %>% as.numeric() %>% divide_by(1e3)
      # Equally-spaced points every 50m (approximately)
      i_coast_p = i_coast$geometry %>%
        st_union() %>%
        st_sample(size = coast_length * 20, type = "regular") %>%
        st_cast("POINT")
      # Drop empty geometries (essentially lengths less than 50m)
      i_coast_p = i_coast_p[st_dimension(i_coast_p) %>% is.na() %>% not()]
      # Subset i's points to those within ~5km of the coastline
      i_subset = i_points %>% filter(
        st_intersects(
          .,
          st_buffer(i_coast, dist = 1/20) %>% st_union(),
          sparse = F
        )
      )
      # If both objects are non-empty: Find any border points (in i_subset)
      # that are within 0.05km of water lines (in f_subset)
      if ((nrow(i_subset) > 0) & (length(i_coast_p) > 0)) {
        water_border = geo_join(
          x = st_coordinates(i_subset) %>% as.data.frame(),
          y = st_coordinates(i_coast_p) %>% as.data.frame(),
          max_dist = 0.05,
          method = "haversine",
          unit = "km",
          mode = "inner"
        ) %>%
        select(x = X.x, y = Y.x) %>%
        mutate(file_type = "coastline")
        setDT(water_border)
        i_water_points = rbindlist(list(i_water_points, water_border))
      }
    }
    # Convert i_water_points to uniquen set of points
    water_points = i_water_points[,1:2] %>% unique()
    # For each point in i_points, calculate the share of neighbors near water
    # Step 1 (numerator): The number of near-water neighbors (within 2.5km)
    dist_water = distm(
      x = i_points %>% st_coordinates(),
      y = water_points
    ) %>% apply(FUN = . %>% is_less_than(2.5e3) %>% sum(na.rm = T), MARGIN = 1)
    # Step 2 (denominator): The number of neighbors (within 2.5km)
    dist_all = distm(
      x = i_points %>% st_coordinates()
    ) %>% apply(FUN = . %>% is_less_than(2.5e3) %>% sum(na.rm = T), MARGIN = 1)
    # Add FIPS, share and share-based indicator to i_points data
    i_points %<>% mutate(
      fips = i_fips,
      share_near_water = dist_water / dist_all,
      i_near_water = share_near_water > 0.5
    )
    # Save
    saveRDS(
      object = i_points,
      file = paste0(dir_save, "border", i_fips, ".rds")
    )
  }, mc.cores = 32, mc.preschedule = F)

# Move files upon completion ---------------------------------------------------
  # Move files from dir_save to within the project
  system2(
    command = "mv",
    args = c(dir_save, here("DataClean"))
  )
