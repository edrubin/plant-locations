
# Notes ----------------------------------------------------------------------------------
  # https://www.esrl.noaa.gov/psd/data/gridded/data.narr.pressure.html

# R setup --------------------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(
    filesstrings, tidyverse, patchwork,
    sf, fuzzyjoin, geosphere, raster,
    data.table, fst,
    parallel, future, furrr, tictoc,
    magrittr, here
  )

# Set directories ------------------------------------------------------------------------
  # Directory with county borders
  dir_borders = here("DataClean", "BordersAndWater")
  # Census data
  dir_census =  here("../Census", "tl_2016_us_county")

# Load with data -------------------------------------------------------------------------
  wind_levels = lapply(
    X = 1:3,
    FUN = function(l) {
      # Load data
      u = here("DataRaw", "Wind", "uwnd.mon.ltm.nc") %>% brick(level = l)
      v = here("DataRaw", "Wind", "vwnd.mon.ltm.nc") %>% brick(level = l)
      # 'Warm months': April through September
      months_w = c("X1.04.29", "X1.05.30", "X1.06.29", "X1.07.30", "X1.08.30", "X1.09.29")
      i_w = which(names(u) %in% months_w)
      # Average for all months
      u_a = u %>% calc(fun = mean)
      v_a = v %>% calc(fun = mean)
      # Average for warm months
      u_w = subset(u, i_w) %>% calc(fun = mean)
      v_w = subset(v, i_w) %>% calc(fun = mean)
      # 'Cold months': October through March
      months_c = c( "X1.10.30", "X1.11.29", "X0.12.30", "X1.01.30", "X1.02.27", "X1.03.30")
      i_c = which(names(u) %in% months_c)
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
# NOTE: This calculation is "wind vector azimuth", where 0 is blowing North
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
  gen_dt = here("DataClean", "Plants", "generator-egrid-2016.fst") %>%
    read_fst(as.data.table = T)
  # Limit to coal, gas, and other fossil fuels
  gen_dt = gen_dt[fuel_cat_gen %in% c("COAL", "GAS", "OIL", "OTHER FOSSIL"), .(
    oris, lat, lon, fips_state, fips_county
  )] %>% unique()
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
  # Create an empty raster with NAD83 and our desired extent and resolution
  empty_raster = raster(
    # Buffered extent
    ext = gen_sf %>% st_geometry() %>% st_union() %>% st_convex_hull() %>%
      st_buffer(dist = 10) %>% st_as_sf() %>% extent(),
    # Resolution
    res = 0.1,
    # CRS
    crs = st_crs(4269) %>% as.character() %>% extract(2) %>% crs()
  )
  # Reproject wind rasters to 4269 (NAD83)
  wind_speed_all %<>% projectRaster(from = ., crs = st_crs(4269), to = empty_raster)
  wind_speed_cold %<>% projectRaster(from = ., crs = st_crs(4269), to = empty_raster)
  wind_speed_warm %<>% projectRaster(from = ., crs = st_crs(4269), to = empty_raster)
  wind_angle_all %<>% projectRaster(from = ., crs = st_crs(4269), to = empty_raster)
  wind_angle_cold %<>% projectRaster(from = ., crs = st_crs(4269), to = empty_raster)
  wind_angle_warm %<>% projectRaster(from = ., crs = st_crs(4269), to = empty_raster)

# Create plant-level figures -------------------------------------------------------------
  # Iterate over county
  blah = lapply(X = gen_dt[,fips] %>% unique(), FUN = function(co) {
    # Take county subset
    co_dt = gen_dt[fips == co]
    # Open open the county's shapefile
    co_sf = file.path(dir_census, "tl_2016_us_county.shp") %>%
      st_read() %>%
      transmute(fips = paste0(STATEFP, COUNTYFP)) %>%
      filter(fips == co)
    # Iterate over ORIS codes
    bleh = lapply(X = co_dt[,oris], FUN = function(plant) {
      # Convert the plant to 'sf'
      plant_sf = co_dt[oris == plant] %>% st_as_sf(coords = c("lon", "lat"), crs = 4269)
      # Wind angle at the plant
      plant_wind = data.table(
        angle = c(
          raster::extract(x = wind_angle_all, y = plant_sf),
          raster::extract(x = wind_angle_cold, y = plant_sf),
          raster::extract(x = wind_angle_warm, y = plant_sf)
        ),
        speed = c(
          raster::extract(x = wind_speed_all, y = plant_sf),
          raster::extract(x = wind_speed_cold, y = plant_sf),
          raster::extract(x = wind_speed_warm, y = plant_sf)
        ),
        season = c("All", "Winter", "Summer")
      ) %>% mutate(angle = if_else(angle < 0, 360 + angle, angle))
      # Plot county with plant's location
      gg_map = ggplot() +
      geom_sf(data = co_sf, color = "grey75", fill = "grey97", size = 0.2) +
      geom_sf(data = plant_sf, color = viridis::magma(3, end = 0.9)[2]) +
      theme_minimal()
      # Plot wind direction
      gg_wind = ggplot(
        data = plant_wind,
        aes(x = angle, xend = angle, y = 0, yend = speed, color = season)
      ) +
      geom_segment(
        size = 0.5,
        arrow = arrow(length = unit(0.1, "cm"), type = "closed")
      ) +
      scale_x_continuous(
        "",
        breaks = seq(0, 315, 45),
        labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        limits = c(0, 360)
      ) +
      scale_color_manual("", values = viridis::magma(3, end = 0.9)[c(1,3,2)]) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_polar()
      # Combine plots
      gg_joint = gg_map + gg_wind + ggtitle(paste0("FIPS ", co, " and ORIS code ", plant))
      # Save
      ggsave(
        path = here("Figures", "PlantWind"),
        filename = paste0("fips-", co, "-oris-", plant, ".pdf"),
        width = 11,
        height = 7
      )
    })
  })

# Great example for complexity: FIPS "01063" and ORIS 10
