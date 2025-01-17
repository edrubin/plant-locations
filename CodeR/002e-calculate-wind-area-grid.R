

# Notes ----------------------------------------------------------------------------------
#   Goals: 
#     Calculate upwind and downwind area for every point in CONUS grid.
#   Time:
#     48 hours? 

# Data notes -----------------------------------------------------------------------------
#   - The population raster has 8675.695 people living outside of the TigerLines county
#     shapefile. These people (and their cells) get dropped in the merge. Accounts for 
#     0.0028% of population in CONUS and 0.035% of the raster cells.


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, huxtable, janitor, data.table, fst, vroom,
    raster, sf, fasterize, units, geosphere, fixest,
    parallel, magrittr, here
  )


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


# Function: Draw (downwind) triangles ----------------------------------------------------
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
  # Create an outline of the US
  us_sf = st_union(state_sf)


# Load data: Population rasters ----------------------------------------------------------
  # Load the population rasters
  pop_r = here("DataRaw", "Census", "usgrid_data_2010", "uspop10.tif") %>% raster()
  popnhw_r = here("DataRaw", "Census", "usgrid_data_2010", "usnhw10.tif") %>% raster()


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


# Data work: Rasterize the county polygon ------------------------------------------------
  # Rasterize county polygon shapefile (following population raster shape/extent)
  county_r = fasterize(
    sf = st_transform(
      county_sf,
      crs = crs(pop_r)
    ) %>% dplyr::mutate(fips = as.numeric(GEOID)),
    raster = pop_r,
    field = "fips"
  )


# Data work: Rotate, stack, and project wind data ----------------------------------------
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
  # Now project to the population raster (for merging)
# NOTE: Do not use 'bilinear' method for data in radians.
  wind_stack %<>% projectRaster(from = ., to = pop_r, method = "ngb")
  # Clean up
  rm(speed_stack, angle_stack)


# Data work: Convert raster cells to data table and merge --------------------------------
  # Grab non-NA points raster (land mass of CONUS)
  pop_dt = pop_r %>% rasterToPoints(spatial = F) %>% as.data.table()
  popnhw_dt = popnhw_r %>% rasterToPoints(spatial = F) %>% as.data.table()
  county_dt = county_r %>% rasterToPoints(spatial = F) %>% as.data.table()
  wind_dt = wind_stack %>% rasterToPoints(spatial = F) %>% as.data.table()
  # Change names
  setnames(pop_dt, c("lon", "lat", "pop"))
  setnames(popnhw_dt, c("lon", "lat", "pop_nhw"))
  setnames(county_dt, c("lon", "lat", "fips"))
  setnames(wind_dt, old = c("x", "y"), c("lon", "lat"))
  # Pad FIPS
  county_dt[, `:=`(
    fips = fips %>% str_pad(5, "left", 0)
  )]
  # Merge datasets
  setkey(pop_dt, lon, lat)
  setkey(popnhw_dt, lon, lat)
  setkey(county_dt, lon, lat)
  setkey(wind_dt, lon, lat)
  grid_dt = merge(
    x = pop_dt,
    y = popnhw_dt,
    by = c("lon", "lat"),
    all = T
  ) %>% merge(
    y = county_dt,
    by = c("lon", "lat"),
    all.x = F,
    all.y = T
  ) %>% merge(
    y = wind_dt,
    by = c("lon", "lat"),
    all.x = F,
    all.y = F
  )
  # Drop intermediate datasets
  rm(pop_dt, popnhw_dt, county_dt, wind_dt)
  # Add 'water' flag and replace NAs with 0s for population
  grid_dt[, flag_water := is.na(pop)]
  grid_dt[is.na(pop), `:=`(
    pop = 0,
    pop_nhw = 0
  )]
 

# Data work: Upwind/downwind area for points, by county ----------------------------------
  # Table of counties
  fips_dt = grid_dt[, .(n = .N), fips]
  # Find completed counties
  fips_done = here(
    "DataClean", "GridWind", "County"
  ) %>% dir() %>% str_extract("[0-9]{5}")
  # Order by number of grid cells
  setorder(fips_dt, -n)
  # Drop counties that have already finished
  fips_dt %<>% .[!(fips %in% fips_done)]
  # Iterate over counties, calculating upwind and downwind areas (in county and state)
  blah = mclapply(mc.cores = 8, X = fips_dt[,fips], FUN = function(co) {
    # Find the centroid of the state of county 'co'
    st_center = state_sf %>%
      filter(STATEFP == str_sub(co, 1, 2)) %>%
      st_geometry() %>% 
      st_centroid(of_largest_polygon = T) %>% 
      st_coordinates() %>% 
      as.vector()
    # Find the UTM zone for the centroid of the state of county 'co'
    co_utm_epsg = st_center %>% lonlat2UTM()
    # Grab the points in county 'co'
    co_dt = grid_dt[fips == co]
    # Convert to 'sf'
    co_sf = co_dt %>% st_as_sf(coords = c("lon", "lat"), crs = crs(pop_r))
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
    co_area = co_shape %>% st_area() %>% as.numeric() %>% divide_by(1e6)
    st_area = co_state %>% st_area() %>% as.numeric() %>% divide_by(1e6)
    # Iterate (in parallel) over the points in county 'co'
    c_dt = mclapply(mc.cores = 8, X = 1:co_dt[,.N], FUN = function(i) {
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
      # Find areas up-/down-/left-/right-wind in the county and state
      areas_co = tri_sf %>% st_transform(
        crs = st_crs(co_shape)
      ) %>% st_intersection(
        x = co_shape,
        y = .
      ) %>% st_area() %>% as.numeric() %>% divide_by(1e6)
      # Find areas of traingles' intersections with the state
      areas_st = tri_sf %>% st_transform(
        crs = st_crs(co_state)
      ) %>% st_intersection(
        x = co_state,
        y = .
      ) %>% st_area() %>% as.numeric() %>% divide_by(1e6)
      # Return data table of desired information
      return(bind_cols(
        co_dt[i,],
        data.table(
          state_area = st_area,
          county_area = co_area,
          downwind_area_st = areas_st[1],
          downwind_area_co = areas_co[1],
          upwind_area_st = areas_st[2],
          upwind_area_co = areas_co[2],
          leftwind_area_st = areas_st[3],
          leftwind_area_co = areas_co[3],
          rightwind_area_st = areas_st[4],
          rightwind_area_co = areas_co[4]
        )
      ))
    }) %>% rbindlist(use.names = T, fill = T)
    # Change names to emphasize the counter-clockwise-ness of the angles
    setnames(
      c_dt, 
      old = c("wind_angle_all", "wind_angle_cold", "wind_angle_warm"),
      new = c("wind_angle_all_ccw", "wind_angle_cold_ccw", "wind_angle_warm_ccw")
    )
    # Save the county's output
    write_fst(
      x = c_dt,
      path = here(
        "DataClean", "GridWind", "County",
        paste0("grid-downwind-upwind-tl-", co, ".fst")
      ),
      compress = 100
    )
    # Return "success"
    return("success")
  })
