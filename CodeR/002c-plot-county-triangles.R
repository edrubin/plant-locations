
# Notes ----------------------------------------------------------------------------------
#   Goal: Calculate share of county upwind
#   Steps:
#     1. Write functions to translate lat and lon to UTM
#        trig does not play nicely with lat/lon
#     2. Write a function to draw a big triangle
#        triangle has vertex at a power plant
#        rotate triangle by average wind dir
#     3. Intersect triangle with county shapefile
#        area of intersection gives area up/downwind


# Setup ----------------------------------------------------------------------------------
  # load data and packages
  library(pacman)
  p_load(
    rmapshaper, tidyverse, sf, raster,
    extrafont, viridis, patchwork,
    data.table, fst, parallel,
    magrittr, here
  )


# Load functions and data from area-calculation script -----------------------------------
  # Read the lines
  area_script = here("CodeR", "001e-calculate-wind-area.R") %>% readLines()
  # Find the line at which I want to stop
  stop_line = area_script %>% str_detect("mclapply") %>% which() %>% subtract(1)
  source(textConnection(area_script[1:stop_line]))


# Load data ------------------------------------------------------------------------------
  # Load upwind/downwind dataset
  wind_dt = here(
    "DataClean", "PlantWind", "plant-downwind-upwind.rds"
  ) %>% readRDS()
  # Load plant-level eGRID data
  plant_dt = here(
    "DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"
  ) %>% read_fst(as.data.table = T)
  # Join plant info to upwind/downwind data
  plant_wind = merge(
    x = plant_dt,
    y = wind_dt[, -c("fips_state", "fips_county", "lon", "lat")],
    by = "oris",
    all.x = F,
    all.y = T
  )
  # Drop the one TX hydro plant that failed upwind/downwind calculations
  plant_wind %<>% .[!is.na(upwind_area)]
  # Crop individual datasets
  rm(wind_dt, plant_dt)


# Generate figures -----------------------------------------------------------------------
  # Grab coal plants
  coal_dt = plant_wind[any_coal_gen == 1]
  # Order by capacity
  setorder(coal_dt, -max_capacity_plant)
  # for (i_oris in coal_dt[1:10, oris]) {
  for (i_oris in c("3118", "3122", "3130")) {
    # Downwind area
    i_down = downwind_area(
      p = coal_dt[oris == i_oris, ],
      size = 1e6,
      theta = 90,
      phi = coal_dt[oris == i_oris, wind_angle_all]
    )[[1]]
    # Upwind area
    i_up = downwind_area(
      p = coal_dt[oris == i_oris, ],
      size = 1e6,
      theta = 90,
      phi = (coal_dt[oris == i_oris, wind_angle_all] + 180) %% 360
    )[[1]]
    # Get i's location
    i_sf = gen_sf %>% dplyr::filter(oris == i_oris)
    # Find i's county
    i_co = st_join(
      x = us_shp,
      y = i_sf,
      join = st_intersects,
      left = F,
      largest = T
    )
    # Define colors
    plot_colors = magma(3, end = 0.8)
    # State-level plot
    plot_state = ggplot() +
    geom_sf(
      data = us_shp %>%
        filter(STATEFP == i_sf$fips_state) %>%
        ms_simplify(keep = 0.01),
      aes(
        color = COUNTYFP == i_sf$fips_county,
        fill = COUNTYFP == i_sf$fips_county
      ),
      size = 0.15,
      alpha = 0.9
    ) +
    scale_fill_manual(values = c("grey98", magma(10)[6])) +
    scale_color_manual(values = c("grey83", magma(10)[6])) +
    theme_void() +
    theme(legend.position = "none")
    # Plot
    plot_area = ggplot() +
    geom_sf(
      data = i_co %>% st_geometry(),
      fill = NA,
      color = "black",
      size = 0.05
    ) +
    geom_sf(
      data = i_down %>% st_geometry() %>% st_transform(4269),
      fill = plot_colors[2],
      color = plot_colors[2],
      alpha = 0.95,
      size = 0.1
    ) +
    geom_sf(
      data = i_up %>% st_geometry() %>% st_transform(4269),
      fill = "grey85",
      color = "grey85",
      alpha = 0.5,
      size = 0.1
    ) +
    geom_sf(
      data = i_sf %>% st_geometry(),
      color = plot_colors[1],
      size = 2.5,
      shape = 18
    ) +
    theme_void(base_size = 6.5, base_family = "Merriweather")
    # Grab wind data for plant 'i'
    i_wind = data.table(
      angle = raster::extract(x = wind_angle_all, y = i_sf),
      speed = raster::extract(x = wind_speed_all, y = i_sf)
    ) %>% mutate(angle = if_else(angle < 0, 360 + angle, angle))
    # Plot wind direction
    plot_wind = ggplot(
      data = i_wind,
      aes(x = angle, xend = angle, y = 0, yend = speed)
    ) +
    geom_segment(
      size = 0.75,
      arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
      color = plot_colors[2]
    ) +
    geom_point(
      data = data.table(angle  = 0, speed = 0, y = 0),
      aes(x = angle, y = y),
      size = 3.5,
      shape = 18
    ) +
    scale_x_continuous(
      "",
      # breaks = seq(0, 315, 45),
      # labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      breaks = seq(0, 315, 90),
      labels = c("N", "E", "S", "W"),
      limits = c(0, 360)
    ) +
    scale_y_continuous(
      "",
      breaks = c(0)
    ) +
    scale_color_manual("", values = viridis::magma(3, end = 0.9)[c(1,3,2)]) +
    theme_minimal(base_size = 20, base_family = "Merriweather") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "grey85", size = 0.1),
      panel.grid.major.y = element_line(colour = "grey85", size = 0.1)
    ) +
    coord_polar()
    # Combine plots
    plot_full = plot_area + plot_wind + plot_state +
    plot_layout(
      design = c(
        patchwork::area(t = 0, l = 0, b = 100, r = 100),
        patchwork::area(t = 0, l = 70, b = 30, r = 99),
        patchwork::area(t = 80, l = 80, b = 100, r = 99)
      )
    )
    # Save
    ggsave(
      plot = plot_full,
      # path = here("Figures", "PlantWind", "Top10"),
      path = here("Figures", "PlantWind", "Others"),
      filename = paste0("plant-wind-oris-", i_oris, ".pdf"),
      device = cairo_pdf,
      height = 9,
      width = 9
    )
  }

