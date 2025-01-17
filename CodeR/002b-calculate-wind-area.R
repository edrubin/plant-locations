
# Notes ----------------------------------------------------------------------------------
#   Goal:   Calculate shares and areas in states and counties up/down/left/right-wind.
#   Steps:
#     1.    Write functions to translate lat and lon to UTM; trig doesn't work w/ lat/lon.
#     2.    Write a function to draw a big triangle triangle has vertex at a power plant;
#           rotate triangle by average wind dir.
#     3.    Intersect triangle with administrative shapefile area of intersection gives
#           area up/down/left/right-wind
#   Time:   7 minutes per run.


# Setup ----------------------------------------------------------------------------------
  # load data and packages
  library(pacman)
  p_load(
    tidyverse, sf, raster,
    data.table, fst, parallel,
    magrittr, here
  )


# Define parameters ----------------------------------------------------------------------
  # Define file type (Census cartographic boundaries vs. TigerLines; county vs. state)
  # file_type = "tl_2016_us"
  file_type = "cb_2016_us"
  # Detect border type and administrative level
  border_type = str_sub(file_type, 1, 2)


# Set directories ------------------------------------------------------------------------
  # Directory with county borders
  dir_borders = here("DataClean", "BordersAndWater")
  # Census data
  dir_census =  here("DataRaw", "Census")


# Load data: County and state shapes -----------------------------------------------------
  # Find the relevant state and county files
  f_st = dir_census %>% dir(pattern = paste0(file_type, ".state"))
  f_co = dir_census %>% dir(pattern = paste0(file_type, ".county"))
  # Load county shape file
  state_sf = file.path(
    dir_census, f_st, paste0(f_st, ".shp")
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP", 
      paste0(
        "FROM \"",
        f_st,
        "\""
      ),
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  )
  # Load county shape file
  county_sf = file.path(
    dir_census, f_co, paste0(f_co, ".shp")
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP, COUNTYFP, GEOID", 
      paste0(
        "FROM \"",
        f_co,
        "\""
      ),
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  )
  # Create an outline of the US
  us_sf = st_union(state_sf)


# Function: Calculate EPSG code of UTM from lat/lon --------------------------------------
# NOTE: From https://geocompr.robinlovelace.net/reproj-geo-data.html
  # The function:
  lonlat2UTM = function(lonlat) {
    utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if(lonlat[2] > 0) {
      utm + 32600
    } else{
      utm + 32700
    }
  }


# Function: Convert points from original CRS to UTM --------------------------------------
  # Takes dt or sf object with lon/lat points named "lon-lat" for plant conversion
  lat_lon_utm = function(obj){
    if(class(obj)[1] == "data.table"){
      # Extract coords
      coords_lonlat = obj[,.(lon,lat)]
      # Create simple features object
      point_sf = st_as_sf(
        coords_lonlat,
        coords = c("lon",  "lat"), 
        # crs = st_crs("+proj=longlat")
        crs = 4269
        )
    } else{ 
      # If input is already SF
      coords_lonlat = obj %>% 
        st_coordinates() %>% 
        as_tibble() %>%
        dplyr::rename(lon = X, lat = Y) %T>%
        setDT()
      # Points
      point_sf = st_as_sf(
        obj,
        coords = c("lon",  "lat"), 
        crs = st_crs(obj)
      ) 
    }
    # Get epsg of coords in UTM
    new_epsg = lonlat2UTM(c(
      coords_lonlat[,lon], 
      coords_lonlat[,lat]
    ))
    # New proj4 string
    new_proj = st_crs(new_epsg)$proj4string
    # Transform coords
    coords_utm = st_transform(
      point_sf,
      crs = new_proj
    )
    # Return utm coords
    return(coords_utm)
  }


 # Function: Draw 'downwind' triangles ----------------------------------------------------
#   - 'plant' gives coords
#   - 'theta' is triangle angle (in deg)
#   - 'phi' is angle of wind vector (in deg),
#      which rotates the triangle counterclockwise from due North
#   - 'size' gives radius of circle (size of triangle)
  # The function
  draw_triangle = function(plant, theta, phi, size){
    # Convert from degrees to radians
    # NOTE: All sin/cos functions take in args in rads
    theta_rad = theta * (pi/180)
    phi_rad = phi * (pi/180)
    # Convert plant to utm
    plant_utm = plant %>% lat_lon_utm()
    # Grab CRS from plant
    plant_crs = plant_utm %>% st_crs()
    # Calculate triangle's vertices relative to the plant
    # First vertex is at the plant
    v_1 = plant_utm %>%
      st_coordinates() %>%
      as.vector()
    # Force both non-theta angles to be equal (in degrees)
    other_angles = (180 - theta)/2
    # Use the law of sines to get length of triangle sides
    sides = (sin(other_angles*(pi/180)) / sin(theta_rad)) * size
    # Vertex 2:
    v_2 = c(
      v_1[1] + sides*cos(other_angles*(pi/180)), 
      v_1[2] + sides*sin(other_angles*(pi/180))
    )
    # Vertex 3:
    v_3 = c(
      v_1[1] - sides*cos(other_angles*(pi/180)), 
      v_1[2] + sides*sin(other_angles*(pi/180))
    )
    # Bind triangle coords together
    # NOTE: Repeat vertex 1 to close polygon
    triangle_coords = rbind(
      v_1, v_2, v_3, v_1
    )
    # Construct polygon
    triangle_pol = st_polygon(list(triangle_coords)) %>%
      st_sfc() %>%
      st_set_crs(value = plant_crs)
    # Now rotate based upon 'phi'...
    # Get coordinates
    tri_pol_coords_tmp = triangle_pol %>% st_coordinates()
    # Center coordinates about plant
    tri_pol_coords = data.frame(
      X =  tri_pol_coords_tmp[,1] - v_1[1], 
      Y = tri_pol_coords_tmp[,2] - v_1[2] 
    )
    # Function: Create rotation matrix
    rotation_mat = function(phi_rad){
      matrix(
        data = c(cos(phi_rad), sin(phi_rad), -sin(phi_rad), cos(phi_rad)), 
        nrow = 2,
        ncol = 2
      )
    }
    # Rotate the triangle!
    # NOTE: tri_pol_coords is centered around origin,
    # so multiply by rotation matrix then re-center at the plant coords
    # Rotation step 1: Create empty matrix
    rotated_mat_tmp = matrix(nrow = 4, ncol = 2)
    # Multiply by rotation matrix
    for (i in 1:4){
      rotated_mat_tmp[i,] = rotation_mat(phi_rad) %*% t(tri_pol_coords[i,]) 
    }
    # Add back original point
    rotated_coords = t(rbind(
      rotated_mat_tmp[,1] + v_1[1], 
      rotated_mat_tmp[,2] + v_1[2] 
    ))
    # Convert rotated coordinates to polygon and set CRS to that of plant
    triangle_rotated = st_polygon(list(rotated_coords)) %>%
      st_sfc() %>%
      st_set_crs(value = plant_crs)
    return(triangle_rotated)
  }


# Function: Downwind area ------------------------------------------------------
#   For a given plant 'p' and wind interior angle 'theta' we find county of 'p' 
#   Inputs: 'phi' rotates the triangle. Rotate it based on avg wind dir
#   Output: computes total area upwind
#   Notice the size argument
#   Returns a 4-element list:
#     1. polygon of intersected area
#     2. share of county area
#     3. area of county 'downwind'
#     4. area of county (total)
  # The function:
  downwind_area = function(p, size, theta, phi){
    # Find the county of plant p
    c = st_join(
      x = us_shp,
      y = p[,.(x = lon, y = lat)] %>% st_as_sf(coords = c("x", "y"), crs = 4269),
      join = st_intersects,
      left = F,
      largest = T
    )
    # Convert county 'c' and plant 'p' to UTM
    # Convert plant CRS
    plant_utm = lat_lon_utm(p)
    county_utm = c %>% st_transform(crs = st_crs(plant_utm))
    #construct large triangle (Note: size argument is fixed)
    plant_triangle = draw_triangle(p, theta, phi, size) %>% st_as_sf()
    #compute intersection
    joined = st_intersection(county_utm, plant_triangle)
    # Compute area of intersection
    area_int = st_area(joined)
    # Compute area of county
    area_c = st_area(county_utm)
    # Compute fraction of county down or upwind
    area_frac =  area_int/area_c
    # Returns a list where:
    # First element is intersected polygon
    # Second is the numeric percent area up/downwind
    return(list(joined, area_frac, area_int, area_c))
  }


# Load wind data -------------------------------------------------------------------------
  # Load the data (see 001d-join-wind.R for more notes)
  wind_levels = lapply(
    X = 1:3,
    FUN = function(l) {
      # Load data
      u = here("DataRaw", "Wind", "uwnd.mon.ltm.nc") %>% brick(level = l)
      v = here("DataRaw", "Wind", "vwnd.mon.ltm.nc") %>% brick(level = l)
      # 'Warm months': April through September
      months_w = c("1.04.29", "1.05.30", "1.06.29", "1.07.30", "1.08.30", "1.09.29")
      i_w = which(str_detect(
        names(u),
        paste0(months_w, collapse = "|")
      ))
      # Average for all months
      u_a = u %>% calc(fun = mean)
      v_a = v %>% calc(fun = mean)
      # Average for warm months
      u_w = subset(u, i_w) %>% calc(fun = mean)
      v_w = subset(v, i_w) %>% calc(fun = mean)
      # 'Cold months': October through March
      months_c = c( "1.10.30", "1.11.29", "0.12.30", "1.01.30", "1.02.27", "1.03.30")
      i_c = which(str_detect(
        names(u),
        paste0(months_c, collapse = "|")
      ))
      # Average for cold months
      u_c = subset(u, i_c) %>% calc(fun = mean)
      v_c = subset(v, i_c) %>% calc(fun = mean)
      # New stacks
      r = stack(u_a, u_w, u_c, v_a, v_w, v_c)
      names(r) <- c("u_all", "u_warm", "u_cold", "v_all", "v_warm", "v_cold")
      # Return
      return(r)
    }
  )
  # Means across first three pressure levels
  wind_avg = (wind_levels[[1]] + wind_levels[[2]] + wind_levels[[3]]) / 3
  # Trig calculations
  # http://tornado.sfsu.edu/geosciences/classes/m430/Wind/WindDirection.html
  # Calculate prevailing wind speeds (by season)
  wind_speed_all = sqrt(subset(wind_avg, "u_all")^2 + subset(wind_avg, "v_all")^2)
  wind_speed_cold = sqrt(subset(wind_avg, "u_cold")^2 + subset(wind_avg, "v_cold")^2)
  wind_speed_warm = sqrt(subset(wind_avg, "u_warm")^2 + subset(wind_avg, "v_warm")^2)
  # Calculate prevailing wind angles (by season)
# NOTE: Calculates "wind vector azimuth": 0 = blowing North; 90 = East (deg. clockwise)
  wind_angle_all = atan2(
    x = subset(wind_avg, "v_all"),
    y = subset(wind_avg, "u_all")
  ) / pi * 180
  wind_angle_cold = atan2(
    x = subset(wind_avg, "v_cold"),
    y = subset(wind_avg, "u_cold")
  ) / pi * 180
  wind_angle_warm = atan2(
    x = subset(wind_avg, "v_warm"),
    y = subset(wind_avg, "u_warm")
  ) / pi * 180


# Load plant data ------------------------------------------------------------------------
  # Load plants
  gen_dt = here(
    # "DataClean", "Plants", "plant-egrid-2010to2018.fst"
    "DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"
  ) %>% read_fst(as.data.table = T)
  # Grab desired variables
  gen_dt = gen_dt[, .(oris, lat, lon, fips_state, fips_county)] %>% unique()
  # Drop two plants missing their lon/lat
  gen_dt %<>% .[!is.na(lon)]
  # Limit to lower 48 + DC
  gen_dt = gen_dt[fips_state %in% (maps::state.fips$fips %>% str_pad(2, "left", 0))]
  # Create 5-digit fips
  gen_dt[, fips := paste0(fips_state, fips_county)]
  # To 'sf'
  gen_sf = gen_dt %>% st_as_sf(coords = c("lon", "lat"), crs = 4269)


# Reproject raster data ------------------------------------------------------------------
  # Set CRS for all rasters (Lambert Conformal Conic)
  crs_narr = paste(
    "+proj=lcc",
    "+lat_0=50 +lon_0=-107",
    "+lat_1=50 +lat_2=45",
    "+x_0=5632642.22547 +y_0=4612545.65137",
    "+datum=WGS84 +units=m +no_defs"
  ) %>% crs()
  crs(wind_speed_all) = crs_narr
  crs(wind_speed_cold) = crs_narr
  crs(wind_speed_warm) = crs_narr
  crs(wind_angle_all) = crs_narr
  crs(wind_angle_cold) = crs_narr
  crs(wind_angle_warm) = crs_narr
  # Stack speed and angle separately
  speed_stack = stack(list(
    wind_speed_all = wind_speed_all,
    wind_speed_cold = wind_speed_cold,
    wind_speed_warm = wind_speed_warm
  ))
  angle_stack = stack(list(
    wind_angle_all = wind_angle_all,
    wind_angle_cold = wind_angle_cold,
    wind_angle_warm = wind_angle_warm
  ))
  # Shift angles to match triangle-area function:
  # First: Move to 0-360 (from -180 to 180)
  angle_stack = (angle_stack + 360) %% 360
  # Second: Reflect angles to measure counterclockwise from North (currently clockwise)
  angle_stack = 360 - angle_stack
  # Create a raster stack of wind data
  wind_stack = stack(speed_stack, angle_stack)
  # Create an empty raster with WGS84 and our desired extent and resolution
  empty_raster = raster(
    # Buffered extent
    ext = gen_sf %>% st_geometry() %>% st_union() %>% st_convex_hull() %>%
      st_buffer(dist = 10) %>% st_as_sf() %>% extent(),
    # Resolution
    res = 0.1,
    # CRS
    # crs = st_crs(4269) %>% as.character() %>% extract(2) %>% crs()
    crs = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
  )
  # Now project to the empty raster
# NOTE: Do not use 'bilinear' method for data in radians.
  wind_stack %<>% projectRaster(from = ., crs = crs(4269), to = empty_raster, method = "ngb")
  # Clean up
  rm(speed_stack, angle_stack)


# Extract wind angle at plants -----------------------------------------------------------
  # Spatially join wind angles and generator locations
  gen_wind = gen_sf %>% mutate(
    wind_angle_all = raster::extract(x = wind_stack$wind_angle_all, y = gen_sf)
  )
  # Convert gen_wind to data table
  gen_wind %<>% as.data.table()
  gen_wind %<>% cbind(gen_sf %>% st_coordinates())
  gen_wind[, geometry := NULL]
  setnames(gen_wind, old = c("X", "Y"), new = c("lon", "lat"))


# Find area down wind for each plant (iterating) -----------------------------------------
  # Iterate over plants' counties. For each plant in each county:
  # Finding the areas down/up/left/right-wind in the plant's state and county
  # Table of counties
  fips_dt = gen_wind[, .(n = .N), fips]
  setorder(fips_dt, -n)
  # Iterate over counties
  gen_areas = mclapply(mc.cores = 64, X = fips_dt[,fips], FUN = function(co) {
    # Find the centroid of the state of county 'co'
    center_st = state_sf %>%
      filter(STATEFP == str_sub(co, 1, 2)) %>%
      st_geometry() %>% 
      st_centroid(of_largest_polygon = T) %>% 
      st_coordinates() %>% 
      as.vector()
    # Find the UTM zone for the centroid of the state of county 'co'
    co_utm_epsg = center_st %>% lonlat2UTM()
    # Grab the points in county 'co'
    co_dt = gen_wind[fips == co]
    # Convert to 'sf'
    co_sf = co_dt %>% st_as_sf(coords = c("lon", "lat"), crs = 4269)
    co_shape = county_sf %>%
      filter(GEOID == co) %>%
      st_geometry() %>% 
      st_transform(crs = co_utm_epsg)
    # Same for the state of 'co'
    co_state = state_sf %>% 
      filter(STATEFP == str_sub(co, 1, 2)) %>% 
      st_geometry() %>% 
      st_transform(crs = co_utm_epsg)
    # Find the area of the county (km^2)
    area_co = co_shape %>% st_area() %>% as.numeric() %>% divide_by(1e6)
    area_st = co_state %>% st_area() %>% as.numeric() %>% divide_by(1e6)
    # Iterate over the points in county 'co'
    c_dt = lapply(X = 1:co_dt[,.N], FUN = function(i) {
      # Check if coordinates of the county actually fall into (buffered) county outline
# NOTE 1: There are some data quality issues in the coordinates.
# NOTE 2: Buffer due to the fact that plants locate very near water, which can change borders over time.
# NOTE 3: County '01121' provides an example
      coord_check = st_within(
        x = co_sf[i,] %>% st_transform(st_crs(co_shape)),
        y = co_shape %>% st_buffer(dist = 1e3)
      ) %>% unlist() %>% as.logical() %>% identical(., T)
      # Replicate the check without any buffering
      coord_check_strict = st_within(
        x = co_sf[i,] %>% st_transform(st_crs(co_shape)),
        y = co_shape
      ) %>% unlist() %>% as.logical() %>% identical(., T)
      # If we pass the coordinates check, proceed to area calculations
      if (coord_check == T) {
        # Find triangles: Upwind, downwind 'leftwind', and 'rightwind'
        # (And bind them together in an sf data frame)
        tri_sf = rbind(
          # 'Downwind'
          draw_triangle(
            p = co_sf[i,],
            size = 1e7,
            theta = 90,
            phi = co_sf[i,]$wind_angle_all
          ) %>% st_sfc() %>% st_sf(),
          # 'Upwind'
          draw_triangle(
            p = co_sf[i,],
            size = 1e7,
            theta = 90,
            phi = (co_sf[i,]$wind_angle_all + 180 + 360) %% 360
          ) %>% st_sfc() %>% st_sf(),
          # 'Leftwind'
          draw_triangle(
            p = co_sf[i,],
            size = 1e7,
            theta = 90,
            phi = (co_sf[i,]$wind_angle_all + 90 + 360) %% 360
          ) %>% st_sfc() %>% st_sf(),
          # 'Rightwind'
          draw_triangle(
            p = co_sf[i,],
            size = 1e7,
            theta = 90,
            phi = (co_sf[i,]$wind_angle_all + 270 + 360) %% 360
          ) %>% st_sfc() %>% st_sf()
        )
        # First determine if any triangles do not intersect (can happen on edge cases)
        int_co = tri_sf %>% st_transform(
          crs = st_crs(co_shape)
        ) %>% st_intersects(
          x = co_shape,
          y = .
        ) %>% unlist()
        # Find areas of traingles' intersections with the state
        int_st = tri_sf %>% st_transform(
          crs = st_crs(co_state)
        ) %>% st_intersects(
          x = co_state,
          y = .
        ) %>% unlist()
        # Find areas up-/down-/left-/right-wind in the county and state
        areas_co_int = tri_sf %>% st_transform(
          crs = st_crs(co_shape)
        ) %>% st_intersection(
          x = co_shape,
          y = .
        ) %>% st_area() %>% as.numeric() %>% divide_by(1e6)
        # Find areas of traingles' intersections with the state
        areas_st_int = tri_sf %>% st_transform(
          crs = st_crs(co_state)
        ) %>% st_intersection(
          x = co_state,
          y = .
        ) %>% st_area() %>% as.numeric() %>% divide_by(1e6)
        # Fill vectors with calculated areas
        # (since there may be 0s that get dropped by st_intersection)
        areas_co = rep(0, 4)
        areas_st = rep(0, 4)
        areas_co[int_co] = areas_co_int
        areas_st[int_st] = areas_st_int
        # Return data table of desired information
        co_final = bind_cols(
          co_dt[i,],
          data.table(
            state_area = area_st,
            county_area = area_co,
            downwind_area_st = areas_st[1],
            downwind_area_co = areas_co[1],
            upwind_area_st = areas_st[2],
            upwind_area_co = areas_co[2],
            leftwind_area_st = areas_st[3],
            leftwind_area_co = areas_co[3],
            rightwind_area_st = areas_st[4],
            rightwind_area_co = areas_co[4],
            downwind_pct_st = areas_st[1] / area_st,
            downwind_pct_co = areas_co[1] / area_co,
            upwind_pct_st = areas_st[2] / area_st,
            upwind_pct_co = areas_co[2] / area_co,
            leftwind_pct_st = areas_st[3] / area_st,
            leftwind_pct_co = areas_co[3] / area_co,
            rightwind_pct_st = areas_st[4] / area_st,
            rightwind_pct_co = areas_co[4] / area_co,
            notes = ifelse(
              coord_check_strict == T,
              "",
              "Coordinates are in 1km buffer of listed county."
            )
          )
        )
      } else {
        # If coordinates check failed: Return data table with NAs and a note
        co_final = bind_cols(
          co_dt[i,],
          data.table(
            state_area = NA,
            county_area = NA,
            downwind_area_st = NA,
            downwind_area_co = NA,
            upwind_area_st = NA,
            upwind_area_co = NA,
            leftwind_area_st = NA,
            leftwind_area_co = NA,
            rightwind_area_st = NA,
            rightwind_area_co = NA,
            downwind_pct_st = NA,
            downwind_pct_co = NA,
            upwind_pct_st = NA,
            upwind_pct_co = NA,
            leftwind_pct_st = NA,
            leftwind_pct_co = NA,
            rightwind_pct_st = NA,
            rightwind_pct_co = NA,
            notes = "Reported coordinates do not match reported (buffered) county."
          )
        )
      }
      return(co_final)
    }) %>% rbindlist(use.names = T, fill = T)
    # Change names to emphasize the counter-clockwise-ness of the angles
    setnames(
      c_dt, 
      old = "wind_angle_all",
      new = "wind_angle_all_ccw"
    )
    # Return c_dt
    return(c_dt)
  }) %>% rbindlist(use.name = T, fill = T)
  # Save
  saveRDS(
    object = gen_areas,
    file = here(
      "DataClean", "PlantWind",
      paste0("plant-wind-areas-", border_type, ".rds")
    )
  )

