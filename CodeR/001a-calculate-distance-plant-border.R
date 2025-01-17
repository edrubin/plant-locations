

# Notes ----------------------------------------------------------------------------------
#   Goals: 
#     1. Calculate distance from each unit (plant) to the nearest state & county borders.
#     2. Calculate distance from US grid points to the nearest state & county borders.


# Data notes -----------------------------------------------------------------------------
#   - Include plants operating in 2018
#   - Exclude plants outside of contiguous US
#   - Create a grid of 20K points (via hexagonal sampling) that covers that county


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, janitor, data.table, fst, vroom,
    sf, units, geosphere,
    parallel, magrittr, here
  )


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
      "SELECT STATEFP, COUNTYFP, GEOID", 
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
  # Create 5-digit fips
  setDT(county_sf)
  county_sf[, fips := paste0(statefp, countyfp)]
  # Drop unwanted variables
  county_sf = st_as_sf(county_sf[,.(fips_state = statefp, fips, geometry)])


# Data work: Distance to borders for US grid ---------------------------------------------
# NOTE: Takes about 2.5 hours with 11 cores
  # Create the US outline
  us_sf = state_sf %>% st_geometry() %>% st_union()
  # Create UTM polygons
  utm_x = seq(from = -126, to = -66, by = 6)
  utm_y = c(24, 50)
  utm_sf = lapply(
    X = seq_len(length(utm_x) - 1),
    FUN = function(i) {
      # Create a polygon for the UTM
      rbind(
        c(utm_x[i], utm_y[1]),
        c(utm_x[i + 1], utm_y[1]),
        c(utm_x[i + 1], utm_y[2]),
        c(utm_x[i], utm_y[2]),
        c(utm_x[i], utm_y[1])
      ) %>% list() %>% st_polygon() %>% st_sfc() %>% st_sf() %>% mutate(zone = i + 9)
    }
  ) %>% do.call(rbind, .)
  # Add projection
  st_crs(utm_sf) = st_crs(us_sf)
  # Loop over regions to create grids and calculate distances
  zones = 10:19
  lapply(X = zones, FUN = function(z) {
    # Find the bordering zones
    # zones_z = intersect(zones, c(z-1, z, z+1))
    # Grab polygons for zones in zone_z
    zones_sf = utm_sf %>% filter(zone == z)
    # Find EPSG for zone 'z'
    z_epsg = zones_sf %>%
      filter(zone == z) %>%
      st_centroid() %>%
      st_coordinates() %>%
      as.numeric() %>%
      lonlat2UTM()
    # Reproject zones_sf to UTM zone 'z'
    zones_sf %<>% st_transform(crs = z_epsg)
    # Reproject us outline to UTM zone 'z'
    us_z = us_sf %>% st_transform(crs = z_epsg)
    # Create a triangular grid that uniformly covers the UTM polygons
    z_grid = st_sample(
      x = zones_sf,
      size = zones_sf %>% st_area() %>% divide_by(1e6) %>% as.integer(),
      type = "hexagonal"
    )
    # Intersect the grid with the US outline
    test_point = z_grid %>% st_intersects(us_z, sparse = F)
    z_grid = z_grid[which(test_point)]
    # Transform state and county shapefiles into the UTM zone
    st_z = state_sf %>% st_transform(crs = z_epsg)
    co_z = county_sf %>% st_transform(crs = z_epsg)
    # Find counties and states that intersect with the UTM zone
    test_st = st_z %>% st_intersects(zones_sf, sparse = F)
    st_z = st_z[which(test_st),]
    test_co = co_z %>% st_intersects(zones_sf, sparse = F)
    co_z = co_z[which(test_co),]
    # Calculate each grid point's distance to the nearest state and county borders
    # Add IDs to the points
    z_grid %<>% st_as_sf()
    z_grid %<>% mutate(id = 1:nrow(z_grid))
    # Iterate over counties
    z_dt = mclapply(
      mc.cores = detectCores() - 1,
      mc.preschedule = F,
      X = co_z$fips,
      FUN = function(j) {
        sub_grid = st_intersection(z_grid, filter(co_z, fips == j))
        data.table(
          utm_zone = z,
          id = sub_grid$id,
          fips = j,
          dist_border_county = st_distance(
            sub_grid,
            filter(co_z, fips == j) %>% st_cast("MULTILINESTRING")
          ) %>% extract(,1) %>% units::set_units("km"),
          dist_border_state = st_distance(
            sub_grid,
            filter(
              st_z,
              STATEFP == filter(co_z, fips == j)$fips_state
            ) %>% st_cast("MULTILINESTRING")
          ) %>% extract(,1) %>% units::set_units("km")
        )
    }) %>% rbindlist(use.names = T, fill = T)
    # Save zone z's grid distances
    write_fst(
      x = z_dt,
      path = here(
        "DataClean", "DistancePlantBorder", "Grid",
        paste0("dist-border-grid-zone-", z, ".fst")
      ),
      compress = 100
    )
  })


# Figure: Example of grid calculations ---------------------------------------------------
  # Define the zone
  z = 10
  # Grab polygons for zones in zone_z
  zones_sf = utm_sf %>% filter(zone == z)
  # Find EPSG for zone 'z'
  z_epsg = zones_sf %>%
    filter(zone == z) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as.numeric() %>%
    lonlat2UTM()
  # Reproject zones_sf to UTM zone 'z'
  zones_sf %<>% st_transform(crs = z_epsg)
  # Reproject us outline to UTM zone 'z'
  us_z = us_sf %>% st_transform(crs = z_epsg)
  # Create a triangular grid that uniformly covers the UTM polygons
  z_grid = st_sample(
    x = zones_sf,
    size = 1e5,
    type = "hexagonal"
  )
  # Intersect the grid with the US outline
  test_point = z_grid %>% st_intersects(us_z, sparse = F)
  z_grid = z_grid[which(test_point)]
  # Transform state and county shapefiles into the UTM zone
  st_z = state_sf %>% st_transform(crs = z_epsg)
  co_z = county_sf %>% st_transform(crs = z_epsg)
  # Find counties and states that intersect with the UTM zone
  test_st = st_z %>% st_intersects(zones_sf, sparse = F)
  st_z = st_z[which(test_st),]
  test_co = co_z %>% st_intersects(zones_sf, sparse = F)
  co_z = co_z[which(test_co),]
  # Calculate each grid point's distance to the nearest state and county borders
  # Add IDs to the points
  z_grid %<>% st_as_sf()
  z_grid %<>% mutate(id = 1:nrow(z_grid))
  # Iterate over counties
  j = "41039"
  # Find the grid's intersection with the county
  sub_grid = st_intersection(z_grid, filter(co_z, fips == j))
  # Grab the county
  co_jz = filter(co_z, fips == j)
  # Create and save the plot
  pdf(here("Figures", "DistanceCalculation", "grid-example-41039.pdf"))
  plot(
    co_jz %>% st_geometry(),
    col = "grey90",
    border = "grey55",
    lwd = 0.5
  )
  plot(
    co_z %>% st_geometry(),
    col = NA,
    border = "grey85",
    lwd = 0.5,
    add = T
  )
  plot(
    co_jz %>% st_geometry(),
    col = "grey90",
    border = "grey55",
    lwd = 0.5,
    add = T
  )
  plot(
    z_grid %>% st_geometry(),
    pch = 1,
    cex = 0.5,
    lwd = 0.75,
    col = viridis::magma(6)[5],
    add = T
  )
  plot(
    sub_grid %>% st_geometry(),
    pch = 16,
    cex = 0.7,
    col = viridis::magma(6)[3],
    add = T
  )
  dev.off()
  system2(
    command = "pdfcrop", 
    args = c(
      here("Figures", "DistanceCalculation", "grid-example-41039.pdf"),
      here("Figures", "DistanceCalculation", "grid-example-41039.pdf")
    )
  )


# Data work: Distance from plant to county and state borders -----------------------------
# Helpful resource:
# https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/
  # Iterate over each county in the contiguous US
  blah = mclapply(
    mc.cores = 12,
    X = county_sf$fips %>% unique(),
    FUN = function(i) {
      # Grab county 'i'
      co_i = county_sf %>% filter(fips == i)
      # Find (approximate) centroid to use in UTM transformation
      center_i = co_i %>% st_centroid()
      # Grab longitude and latitude from centroid
      lonlat_i = center_i %>% st_coordinates() %>% as.numeric()
      # Find the UTM zone
      zone_i = lonlat2UTM(lonlat_i)
      # Convert co_i, its state, and the plants to i's UTM zone
      co_i %<>% st_transform(crs = zone_i)
      st_i = state_sf %>% filter(STATEFP == co_i$fips_state) %>% st_transform(crs = zone_i)
      plant_i = plant_sf %>% st_transform(crs = zone_i)
      # Find the plants in county i
      plant_i = plant_i[which(st_within(plant_i, co_i, sparse = F)),]
      # Calculate plants' distances to county and state borders
      dist_plants_county = st_distance(
        plant_i,
        co_i %>% st_cast("MULTILINESTRING")
      ) %>% units::set_units("km")
      dist_plants_state = st_distance(
        plant_i,
        st_i %>% st_cast("MULTILINESTRING")
      ) %>% units::set_units("km")
      # Add plant distances to the plant data
      i_dt = data.table(
        oris = plant_i$oris,
        dist_plant_county = dist_plants_county[,1],
        dist_plant_state = dist_plants_state[,1]
      )
      # Grab county i's generators and merge with plants' border distances
      i_dt = merge(
        x = gen_dt[oris %in% plant_i$oris],
        y = i_dt,
        by = "oris",
        all.x = T,
        all.y = F,
        sort = F
      )
      # Save generator data
      fwrite(
        x = i_dt,
        file = here(
          "DataClean", "DistancePlantBorder", "Generator",
          paste0("dist-generator-county-border-", i, ".csv")
        )
      )
    }
  )
