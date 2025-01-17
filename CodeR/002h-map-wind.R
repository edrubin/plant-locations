

# Notes ----------------------------------------------------------------------------------
#   Goal:   Plot maps of wind direction (wind data from NARR)
#   Time:   ~5 minutes

# Data notes -----------------------------------------------------------------------------


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, scico, showtext,
    data.table, 
    raster, sf, fasterize, units, geosphere,
    parallel, magrittr, here
  )
  # Install Google's Merriweather font
  font_add_google("Merriweather", "Merriweather")


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


# Create maps: Prevailing wind direction and admin borders -------------------------------
	# CRS to use for mapping
	crs_plot = paste(
	    "+proj=lcc",
	    "+lat_0=50 +lon_0=-100",
	    "+lat_1=50 +lat_2=45",
	    "+x_0=5632642.22547 +y_0=4612545.65137",
	    "+datum=WGS84 +units=m +no_defs"
	  ) %>% crs()
	# Reproject wind-angle raster
	tmp = wind_angle_all %>% projectRaster(crs = crs_plot)
	# Mask the raster to CONUS (with 50km buffer)
	tmp %<>% mask(
		mask = st_transform(us_sf, crs_plot) %>% st_buffer(5e4) %>% as(Class = "Spatial")
	)
	# Crop wind raster to CONUS
	tmp %<>% crop(
		st_transform(us_sf, crs_plot) %>% st_buffer(5e4) %>% st_bbox()
	)
	# Start PNG: Only state borders
	png(
		file = here("Figures", "Wind", "us-wind-angle-all.png"),
		width = 8.5,
		height = 6,
		units = "in",
		res = 250,
		type = "cairo-png"
	)
	# Create map: outline of US, wind angle raster, state outlines
	plot(tmp, col = scico(1e3, palette = "romaO"), legend = F, bty = "n", axes = F, box = F)
	state_sf %>% st_geometry %>% st_transform(crs_plot) %>% plot(border = "black", lwd = 0.4, add = T)
	# Save map
	dev.off()
	# Start PNG: State and county borders
	png(
		file = here("Figures", "Wind", "us-wind-angle-all-co.png"),
		width = 8.5,
		height = 6,
		units = "in",
		res = 250,
		type = "cairo-png"
	)
	# Create map: outline of US, wind angle raster, state outlines
	plot(tmp, col = scico(1e3, palette = "romaO"), legend = F, bty = "n", axes = F, box = F)
	state_sf %>% st_geometry %>% st_transform(crs_plot) %>% plot(border = "black", lwd = 0.4, add = T)
	county_sf %>% st_geometry %>% st_transform(crs_plot) %>% plot(border = "black", lwd = 0.05, add = T)
	# Save map
	dev.off()


# Create legend for wind-angle maps ------------------------------------------------------
	# Create dataset for cyclical legend
	circle_dt = data.table(
		d1 = seq(-180, 180, 0.1) %>% head(-1),
		d2 = seq(-180, 180, 0.1) %>% tail(-1)
	)
	circle_dt[, `:=`(
		col = if_else(d1 < 360, d1, d1 %% 180), 
		r1 = d1 * pi / 180,
		r2 = d2 * pi / 180
	)]
	# Create the legend
	gg_legend = ggplot() + 
	geom_point(
		data = circle_dt[.N:1],
		aes(x = r1, y = 1, color = col),
		size = 20,
		shape = 17,
		alpha = 0.5
	) +
	scale_color_scico(palette = "romaO") +
	annotate("text", family = "Merriweather", size = 4.5, x = 0 * pi / 180, y = 1, label = "North", vjust = -4.25) +
	annotate("text", family = "Merriweather", size = 4.5, x = 90 * pi / 180, y = 1, label = "East", hjust = -1.25) +
	annotate("text", family = "Merriweather", size = 4.5, x = -180 * pi / 180, y = 1, label = "South", vjust = 4) +
	annotate("text", family = "Merriweather", size = 4.5, x = -90 * pi / 180, y = 1, label = "West", hjust = 2.25) +
	coord_polar(
		theta = "x",
		direction = 1,
		start = pi,
		clip = "on"
	) +
	scale_x_continuous(
		"",
		# breaks = c(-180, -90, 0, 90) * pi / 180,
		# labels = c("South", "West", "North", "East")
	) +
	scale_y_continuous("") +
	theme_minimal() +
	theme(
		legend.position = "none",
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		panel.grid = element_blank()
	)
  # Save the figure
  ggsave(
    plot = gg_legend,
    path = here("Figures", "Wind"),
    filename = "wind-angle-legend.pdf",
    height = 6.5, width = 6.5,
    device = cairo_pdf
  )
  # Save the figure
  ggsave(
    plot = gg_legend,
    path = here("Figures", "Wind"),
    filename = "wind-angle-legend.png",
    height = 6.5, width = 6.5,
    type = "cairo"
  )

