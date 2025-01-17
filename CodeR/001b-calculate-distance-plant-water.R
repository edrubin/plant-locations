

# Notes ----------------------------------------------------------------------------------
#   Goal: Calculate distance from each unit (plant) to the nearest body of water.


# Data notes -----------------------------------------------------------------------------
#   - Exclude AK and HI plants
#   - Not picked up: Nearest body of water is across an international border.


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, fst, janitor,
    sf, units, data.table,
    parallel, furrr,
    magrittr, here
  )


# Directories ----------------------------------------------------------------------------
  # Census files
  dir_census = here("DataRaw", "Census")
  # Linear water directory
  dir_lwater = paste0(dir_census, "/tl_2016_linearwater")
  # Area water directory
  dir_awater = paste0(dir_census, "/tl_2016_areawater")
  # Coast line directory
  dir_coast = paste0(dir_census, "/tl_2016_us_coastline")
  # Temporary directory for unzipping files
  dir.create("~/_tmp")
  dir_tmp = "~/_tmp/"


# Function: Find UTM zone from lat/lon ---------------------------------------------------
  # Source: https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
  lonlat2UTM = function(lonlat) {
    utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if(lonlat[2] > 0) {
      utm + 32600
    } else{
      utm + 32700
    }
  }


# Load data: Cleaned generator data ------------------------------------------------------
  # Load the dataset
  gen_dt = read_fst(
    path = here("DataClean", "Plants", "All", "generator-egrid-all-2018.fst"),
    as.data.table = T
  )
  # Drop observations outside of the contiguous 48 (and DC)
  gen_dt = gen_dt[!(fips_state %in% c("02", "15", "60", "66", "69", "72", "78"))]


# Load data: County and state shapes -----------------------------------------------------
  # Load county shape file
  county_sf = here(
    "DataRaw", "Census", "tl_2016_us_county", "tl_2016_us_county.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP, COUNTYFP",
      "FROM \"tl_2016_us_county\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  )
  # Load county shape file
  state_sf = here(
    "DataRaw", "Census", "tl_2016_us_state", "tl_2016_us_state.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP", 
      "FROM \"tl_2016_us_state\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  )


# Load data: US coastline ----------------------------------------------------------------
  # Load the county shapefile
  coast_sf = here("../Census/tl_2016_us_coastline/tl_2016_us_coastline.shp") %>%
    st_read(quiet = T)


# Data work: Unique set of plants and locations ------------------------------------------
  # Add 5-digit FIPS
  gen_dt[, fips := paste0(fips_state, fips_county)]
  # Find unique set of plant codes and locations (location varies at plant level)
  # Only include operating plants
  plant_dt = gen_dt[i_operating == 1, .(oris, fips, lat, lon)] %>% unique()


# Data work: Convert plants to spatial ---------------------------------------------------
  # Convert to 'sf' object
  plant_sf = plant_dt %>% st_as_sf(coords = c("lon", "lat"))
  # Copy CRS from county map (assuming NAD83)
  st_crs(plant_sf) = st_crs(county_sf)


# Data work: Prep shapefile --------------------------------------------------------------
  # Clean names
  county_sf %<>% clean_names()
  state_sf %<>% rename(fips_state = STATEFP)
  # Create 5-digit fips
  setDT(county_sf)
  county_sf[, fips := paste0(statefp, countyfp)]
  # Drop unwanted variables
  county_sf = st_as_sf(county_sf[,.(fips_state = statefp, fips, geometry)])


# Data work: Distance to water ------------------------------------------------------------
  # Find ORIS codes for all plants
  plants_all = plant_dt[,oris] %>% unique()
  # Find plants that have finished (helpful if restarting after crash; empty in first run)
  plants_done = here("DataClean", "DistancePlantWater", "Plant") %>%
    dir() %>% str_extract("[0-9]+(?=\\.csv)")
  # Find plants to finish
  plants_todo = setdiff(plants_all, plants_done)
  # Iterate over each plant
  plan(multiprocess, workers = 32)
  blah = future_map(
    .x = plants_todo,
    .f = function(i) {
      # Grab plant 'i'
      plant_i = plant_sf %>% filter(oris == i)
      # Find FIPS of county that contains plant 'i'
      fips_i = st_join(plant_i, county_sf, left = T, st_within)$fips.y
      # Grab county of plant 'i'
      co_i = county_sf %>% filter(fips == fips_i)
      # If the county is missing, then use the eGRID county
      if(is.na(fips_i)) {
        # Create a flag (used later)
        spatial_flag = T
        # Define fips_i using eGRID dataset
        fips_i = plant_i$fips
        # Grab the eGRID-based county shape
        co_i = county_sf %>% filter(fips == fips_i)
      } else {
        spatial_flag = F
      }
# NOTE: Commenting out this section because I've already unzipped the files to tmp folder
      # # Load water files for county 'i'
      # f_lwater_i = dir(dir_lwater, pattern = fips_i, full.names = T)
      # f_awater_i = dir(dir_awater, pattern = fips_i, full.names = T)
      # # Unzip the water files
      # unzip(
      #   zipfile = f_lwater_i,
      #   exdir = dir_tmp,
      #   overwrite = T
      # )
      # unzip(
      #   zipfile = f_awater_i,
      #   exdir = dir_tmp,
      #   overwrite = T
      # )

      # Unzipped files
      unzipped_lwater_i = dir(
        dir_tmp, pattern = fips_i
      ) %>% str_subset("linearwater.shp$")
      unzipped_awater_i = dir(
        dir_tmp, pattern = fips_i
      ) %>% str_subset("areawater.shp$")
      # Load the unzipped shapefiles
      lwater_i = st_read(
        file.path(dir_tmp, unzipped_lwater_i),
        quiet = T,
        query = paste0(
          "SELECT LINEARID FROM \"",
          str_remove(unzipped_lwater_i, "\\.shp"),
          "\""
        )
      )
      awater_i = st_read(
        file.path(dir_tmp, unzipped_awater_i),
        quiet = T,
        query = paste0(
          "SELECT HYDROID FROM \"",
          str_remove(unzipped_awater_i, "\\.shp"),
          "\""
        )
      )
      # Convert area water polygons to (multi) lines
      awater_i %<>% st_cast("MULTILINESTRING")
      # Convert linear water to MULTILINESTRING
      lwater_i %<>% st_cast("MULTILINESTRING")
      # Bind the two water files together
      water_i = rbind(
        lwater_i %>% transmute(id = LINEARID, type = "linear"),
        awater_i %>% transmute(id = HYDROID, type = "area")
      )
      # Check if i's county has any coastline
      i_coast = st_crop(coast_sf, st_bbox(co_i))
      # Add coastline to water_i if i's county has coastline
      if (nrow(i_coast) > 0) {
        # If any i_coast features are 'GEOMETRYCOLLECTION' split them into geometries
        if ("GEOMETRYCOLLECTION" %in% st_geometry_type(i_coast)) {
          # Find the geometry-collection feature(s)
          i_gc = which(st_geometry_type(i_coast) == "GEOMETRYCOLLECTION")
          # Split the geometry collection and bind back with the other features
          i_coast = rbind(
            i_coast[i_gc,] %>% st_cast(),
            i_coast[setdiff(1:nrow(i_coast), i_gc),]
          )
        }
        # Convert non-points to 'MULTILINESTRING'
        if ("POINT" %in% st_geometry_type(i_coast)) {
          # Find the geometry-collection feature(s)
          i_pt = which(st_geometry_type(i_coast) == "POINT")
          # Split the geometry collection and bind back with the other features
          i_coast = rbind(
            i_coast[i_pt,],
            i_coast[setdiff(1:nrow(i_coast), i_pt),] %>% st_cast("MULTILINESTRING")
          )
        } else {
          # If no points: Cast coast to 'MULTILINESTRING'
          i_coast %<>% st_cast("MULTILINESTRING")
        }
        # Rename/add variables to match other water data (i.e., water_i)
        i_coast %<>% transmute(id = MTFCC, type = "coast")
        # Cast to coast to 'MULTILINESTRING' and combine with other water features
        water_i = rbind(water_i, i_coast)
      }
      # Find (approximate) centroid to use in UTM transformation
      center_i = co_i %>% st_centroid()
      # Grab longitude and latitude from centroid
      lonlat_i = center_i %>% st_coordinates() %>% as.numeric()
      # Find EPSG for i's UTM zone
      epsg_i = lonlat2UTM(lonlat_i)
      # Convert i's files to UTM
      co_i %<>% st_transform(crs = epsg_i) %>% select(fips)
      plant_i %<>% st_transform(crs = epsg_i)
      water_i %<>% st_transform(crs = epsg_i)
      # Calculate plants' distances to border
      dist_border = st_distance(
        plant_i,
        co_i %>% st_cast("MULTILINESTRING")
      ) %>% as.numeric()
      # Calculate plants' distances to border
      dists_water = st_distance(
        plant_i,
        water_i
      ) %>% as.numeric()
      # Find minimum
      dist_water = dists_water %>% min(na.rm = T)
      # Find the "type(s)" of body(ies) of water that form the minimum
      min_type = water_i[which(dists_water == dist_water),]$type %>% paste0(collapse = ",")
      # Indicator for whether the nearest body of water is within i's county
      own_county = T
      # If minimum distance to water is greater distance to border, then load
      # water from relevant counties
      if (round(dist_water, 1) > round(dist_border, 1)) {
        # Create radius around the plant equal to the smallest distance to water
        radius_i = st_buffer(plant_i, dist = dist_water, nQuadSegs = 90) %>% st_geometry()
        # Transform county_sf to i's UTM
        co_utm = county_sf %>% select(fips) %>% st_transform(crs = epsg_i)
        # Find all counties that intersect with radius_i
        intersection_i = st_intersects(
          x = co_utm,
          y = radius_i,
          sparse = F
        )
        # Grab the counties with non-empty intersection
        co_intersect = co_utm[which(intersection_i),]
        # Drop i's county (already checked its water)
        co_intersect %<>% filter(fips != fips_i)
        # Only proceed if the intersection is non-empty
# NOTE: This intersection could be empty if nearest border is international
        if (nrow(co_intersect) > 0) {
          
# NOTE: Commenting out because the files are already unzipped in ~/_tmp
          # # Find water files for intersecting counties
          # f_lwater_int = dir(
          #   dir_lwater,
          #   pattern = co_intersect$fips %>% paste0(collapse = "|"),
          #   full.names = T
          # )
          # f_awater_int = dir(
          #   dir_awater,
          #   pattern = co_intersect$fips %>% paste0(collapse = "|"),
          #   full.names = T
          # )
          # # Unzip and load
          # int_sf = mclapply(
          #   X = c(f_lwater_int, f_awater_int),
          #   FUN = function(f) {
          #     # Unzip the file
          #     unzip(
          #       zipfile = f,
          #       exdir = dir_tmp,
          #       overwrite = T
          #     )
          #     # Find the file name (in order to load the shapefile)
          #     f_name = str_extract(f, "tl_2016_[0-9]{5}_[a-z]+(?=\\.)")
          #     # Load the file, define file type, convert to MULTILINESTRING, and transform
          #     f_sf = dir(dir_tmp, paste0(f_name, ".shp$"), full.names = T) %>%
          #       st_read(quiet = T) %>%
          #       transmute(type = ifelse(str_detect(f_name, "linear"), "linear", "area")) %>%
          #       st_cast("MULTILINESTRING") %>%
          #       st_transform(crs = epsg_i) %T>%
          #       setDT()
          #     # Return the new object
          #     return(f_sf)
          #   },
          #   mc.cores = 2
          # ) %>% rbindlist()

# NOTE: Following chunk replaces the commented out section above (load without unzipping)
          # Find files for the intersected counties
          f_int = dir_tmp %>% dir(
            pattern = co_intersect$fips %>% paste0(collapse = "|"),
            full.names = T
          ) %>% str_subset("\\.shp$")
          # Load files
          int_sf = mclapply(
            mc.cores = 2,
            X = f_int,
            FUN = function(f) {
              # Find the file name (in order to load the shapefile)
              f_name = str_extract(f, "tl_2016_[0-9]{5}_[a-z]+(?=\\.)")
              # Load the file, define file type, convert to MULTILINESTRING, and transform
              f_sf = st_read(f, quiet = T) %>% 
                transmute(type = ifelse(str_detect(f_name, "linear"), "linear", "area")) %>%
                st_cast("MULTILINESTRING") %>%
                st_transform(crs = epsg_i) %T>%
                setDT()
              # Return the new object
              return(f_sf)
            }
          ) %>% rbindlist(use.names = T, fill = T)
          # Convert int_sf to 'sf'
          int_sf %<>% st_as_sf()

          # Transform the coast to i's UTM
          coast_utm = coast_sf %>% st_transform(crs = epsg_i) %>% transmute(type = "coast")
          # Find the intersection between coast_utm and radius_i
          coast_int = st_intersection(coast_utm, radius_i)
          # For non-empty coastal interscation: Add the intersection to int_sf
          if (nrow(coast_int) > 0) {
            # Bind together
            int_sf = rbind(int_sf, coast_int %>% st_cast("MULTILINESTRING"))
          }
          # Find distances to the intersecting bodies of water
          dists_int = st_distance(
            plant_i,
            int_sf
          ) %>% as.numeric()
          # Overwrite dist_water and min_type if the new minimum beats the prior min
          if (min(dists_int) < dist_water) {
            # Overwrite distance to water
            dist_water = min(dists_int)
            # Overwrite type
            min_type = int_sf[which(dists_int == min(dists_int)),]$type %>%
              paste0(collapse = ",")
            # Indicator for whether the nearest water is within the county
            own_county = F
          }
        }
      }
      # Create a data table with i's information
      i_dt = data.table(
        oris = plant_i$oris,
        fips_egrid = plant_i$fips,
        fips_spatial = fips_i,
        dist_water_m = dist_water,
        nearest_type = min_type,
        own_county
      )
      # If the spatial match failed, update i_dt
      if (spatial_flag == T) {
        i_dt[, fips_spatial := NA]
      }
      # Save plant-water dataset
      fwrite(
        x = i_dt,
        file = here(
          "DataClean", "DistancePlantWater", "Plant",
          paste0("dist-plant-water-", plant_i$oris, ".csv")
        )
      )
    },
    .progress = T
  )

warnings()

# # Data work: Aggregate -------------------------------------------------------------------
#   # Find all plant files
#   plant_files = here("DataClean", "DistancePlantWater", "Plant") %>% dir(full.names = T)
#   # Load the files
#   plant_dist = lapply(plant_files, fread) %>% rbindlist(use.names = T, fill = T)
#   # Merge with generator data
#   gen_dist = merge(
#     x = gen_dt,
#     y = plant_dist[,-"fips"],
#     by = "oris",
#     all.x = T,
#     all.y = F
#   )