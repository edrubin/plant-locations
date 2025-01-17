

# Notes ----------------------------------------------------------------------------------
#   Goal: Plot histograms


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, maps, readxl, janitor,
    patchwork, viridis, extrafont, latex2exp,
    fst, data.table, lubridate,
    sf, plyr, viridis, ggsn,
    parallel, here, magrittr
  )


# Figure: State-level water borders ------------------------------------------------------
  # Load water-border summary
  water_borders = here(
    "DataClean", "BorderWaterSummary", "state-border-water-summary.csv"
  ) %>% fread()
  # Merge to get state abbreviations
  water_borders %<>% merge(
    y = state.fips %>% select(state_fips = fips, abb) %>% unique(),
    by = "state_fips",
    all = T
  )
  # Add state- and county-level ranks
  water_borders[, `:=`(
    rank_state = rank(share_water_state_border),
    rank_county = rank(share_water_county_border)
  )]
  # Arrange dataset by county rank
  setorder(water_borders, rank_county)
  # Plot: Water share of county borders
  plot_county = ggplot(
    data = water_borders,
    aes(x = rank_county, y = share_water_county_border)
  ) + 
  geom_col(fill = magma(100)[15], color = NA) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_y_continuous(
    expression(atop(
      paste("", bold("County border")),
      "Share water"
    )), 
    labels = scales::percent,
    limits = c(0,1)
  ) +
  scale_x_continuous(
    "",
    breaks = 1:49,
    labels = water_borders[,abb]
  ) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )
  # Plot: Water share of state borders
  plot_state = ggplot(
      data = water_borders,
      aes(x = rank_county, y = share_water_state_border)
    ) +
    geom_col(fill = magma(100)[85], color = NA) +
    geom_hline(yintercept = 0, size = 1/4) +
    scale_y_continuous(
      expression(atop(
        paste("", bold("State border")),
        "Share water"
      )), 
      labels = scales::percent,
      limits = c(0,1)
    ) +
    scale_x_continuous(
      "",
      breaks = 1:49,
      labels = water_borders[,abb]
    ) +
    theme_minimal(base_size = 8, base_family = "Merriweather") +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
    )
    # Flip the state plot to add to the same figure
    plot_state = plot_state + 
    scale_y_reverse(
      expression(atop(
        paste("", bold("State border")),
        "Share water"
      )), 
      labels = scales::percent,
      limits = c(1,0)
    ) +
    theme(
      axis.text.x = element_blank(),
      plot.margin = margin(-10, 0, 0, 0, "pt"),
    )
    # Combine plots
    plot_final = plot_county / plot_state
    # Save the final plot
    ggsave(
      plot = plot_final,
      path = here("Figures", "Distributions"),
      filename = "border-share-water.pdf",
      device = cairo_pdf,
      height = 3,
      width = 6.5
    )
    # Clean up
    rm(plot_final, plot_state, plot_county, water_borders)

    
# Figure: Distances to borders -----------------------------------------------------------
  # Load plant-to-border distance data
  gen_border_dist = mclapply(
    X = here("DataClean", "DistancePlantBorder", "Generator") %>% dir(full.names = T), 
    FUN = fread, 
    mc.cores = 12
  ) %>% rbindlist(use.names = T, fill = T)
  # Load grid-point-to-border distance data
  grid_border_dist = lapply(
    X = here("DataClean", "DistancePlantBorder", "Grid") %>% dir(full.names = T), 
    FUN = read_fst
  ) %>% rbindlist(use.names = T, fill = T)
  # Restrict to operating generating units with at least 25MW capacity
  gen_border_dist %<>% .[i_operating == 1 & i_big == 1]
  # Restrict to coal, gas, hydro, solar, wind
  gen_border_dist %<>% .[fuel_cat_gen %in% c("COAL", "GAS", "HYDRO", "SOLAR", "WIND")]
  # New factor-based fuel category for plotting
  fuel_lvl = c("Coal", "Gas", "Hydro.", "Solar/Wind", "Uniform US Grid")
  gen_border_dist[, `:=`(
    fuel_plot = fcase(
      fuel_cat_gen == "COAL", factor("Coal", levels = fuel_lvl, ordered = T),
      fuel_cat_gen == "GAS", factor("Gas", levels = fuel_lvl, ordered = T),
      fuel_cat_gen == "HYDRO", factor("Hydro.", levels = fuel_lvl, ordered = T),
      fuel_cat_gen %in% c("SOLAR", "WIND"), factor("Solar/Wind", levels = fuel_lvl, ordered = T)
    )
  )]
  # Cap distance to county borders at 25km and to state border at 150km
  gen_border_dist[, `:=`(
    dist_county_plot = if_else(dist_plant_county > 25, 25, dist_plant_county),
    dist_state_plot = if_else(dist_plant_state > 150, 150, dist_plant_state)
  )]
  grid_border_dist[, `:=`(
    dist_county_plot = if_else(dist_border_county > 25, 25, dist_border_county),
    dist_state_plot = if_else(dist_border_state > 150, 150, dist_border_state)
  )]
  # Stack datasets
  border_dist = rbindlist(list(
    gen_border_dist[, .(fuel_plot, dist_county_plot, dist_state_plot)],
    grid_border_dist[, .(
      fuel_plot = factor("Uniform US Grid", levels = fuel_lvl, ordered = T),
      dist_county_plot, dist_state_plot
    )]
  ), use.names = T, fill = T)
  # Plot colors
  pc = magma(5, begin = 0.1, end = 0.9)
  # Plot: Distance from generating unit to county border by fuel type
  plot_county = ggplot(
    # data = gen_border_dist,
    data = border_dist,
    aes(x = dist_county_plot, fill = fuel_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel A:} Distance to nearest \\textbf{county} border"
    )),
    TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
  ) +
  scale_x_continuous(
    TeX("Distance to nearest \\textbf{county} border (km)"),
    breaks = seq(0, 25, 5),
    labels = c(seq(0, 20, 5), TeX("$\\geq 25$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  scale_fill_manual(values = pc[1:5]) +
  # scale_fill_manual(values = pc[2:5]) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(legend.position = "none") +
  facet_grid(vars(fuel_plot))
  # Plot: Distance from generating unit to state border by fuel type
  plot_state = ggplot(
    # data = gen_border_dist,
    data = border_dist,
    aes(x = dist_state_plot, fill = fuel_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel B:} Distance to nearest \\textbf{state} border"
    )),
    TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
  ) +
  scale_x_continuous(
    TeX("Distance to nearest \\textbf{state} border (km)"),
    breaks = seq(0, 150, 50),
    labels = c(seq(0, 100, 50), TeX("$\\geq 150$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  scale_fill_manual(values = pc[1:5]) +
  # scale_fill_manual(values = pc[2:5]) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(legend.position = "none") +
  facet_grid(vars(fuel_plot))
  # Plot: Distance from grid points to nearest county border
  plot_county_grid = ggplot(
    data = grid_border_dist,
    aes(x = dist_county_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50,
    fill = pc[1]
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    " ",
    TeX("Uniform grid covering the contiguous US")
  ) +
  scale_x_continuous(
    TeX("Distance to nearest \\textbf{county} border (km)"),
    breaks = seq(0, 25, 5),
    labels = c(seq(0, 20, 5), TeX("$\\geq 25$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  theme_minimal(base_size = 6.5, base_family = "Merriweather")
  # Plot: Distance from grid points to nearest state border
  plot_state_grid = ggplot(
    data = grid_border_dist,
    aes(x = dist_state_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50,
    fill = pc[1]
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    " ",
    TeX("Uniform grid covering the contiguous US")
  ) +
  scale_x_continuous(
    TeX("Distance to nearest \\textbf{state} border (km)"),
    breaks = seq(0, 150, 50),
    labels = c(seq(0, 100, 50), TeX("$\\geq 150$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  theme_minimal(base_size = 6.5, base_family = "Merriweather")
  # Combine plots
  # plot_final = (plot_county_gen | plot_county_grid) / (plot_state_gen | plot_state_grid)
  plot_final = plot_county | plot_state
  # Save the final plot
  ggsave(
    plot = plot_final,
    path = here("Figures", "Distributions"),
    filename = "hist-distance-border.pdf",
    device = cairo_pdf,
    height = 7.5,
    width = 6.5
  )
  # Clean up
  rm(
    plot_final, plot_state, plot_state_grid, plot_county, plot_county_grid,
    gen_border_dist, grid_border_dist
  )
  invisible(gc())


# Figure: Distance to water and capacity -------------------------------------------------
  # Load 2018 generator data
  gen_dt = read_fst(
    here::here("DataClean", "Plants", "All", "generator-egrid-all-2018.fst"),
    as.data.table = T
  ) 
  # Keep plants in contiguous US
  gen_dt %<>% .[!(fips_state %in% c("02", "15", "60", "66", "69", "72", "78"))]
  # Keey only (1) 'big' (>=25MW) and (2) 'operating' (and stand-by) units from 2018 and
  # grab desired variables: ORIS, fuel source, capacity (at generator level)
  gen_dt %<>% .[(i_big == 1) & (i_operating == 1), .(
    oris, fuel_cat_gen, fuel_cat_plant, capacity_gen
  )]
  # Restrict to coal, gas, hydro, solar, wind
  gen_dt %<>% .[fuel_cat_gen %in% c("COAL", "GAS", "HYDRO", "SOLAR", "WIND")]
  # Load water distance data
  water_distance = mclapply(
    X = here(
      "DataClean", "DistancePlantWater", "Plant",
      paste0("dist-plant-water-", gen_dt[,oris] %>% unique(), ".csv")
    ),
    FUN = fread, 
    mc.cores = 12
  ) %>% rbindlist(use.names = T, fill = T)
  # Join water distance to generator data
  gen_water = merge(
    x = gen_dt, 
    y = water_distance,
    by = "oris",
    all = T
  )
  # New factor-based fuel category for plotting
  fuel_lvl = c("Coal", "Gas", "Hydro.", "Solar/Wind")
  gen_water[, `:=`(
    fuel_plot = fcase(
      fuel_cat_gen == "COAL", factor("Coal", levels = fuel_lvl, ordered = T),
      fuel_cat_gen == "GAS", factor("Gas", levels = fuel_lvl, ordered = T),
      fuel_cat_gen == "HYDRO", factor("Hydro.", levels = fuel_lvl, ordered = T),
      fuel_cat_gen %in% c("SOLAR", "WIND"), factor("Solar/Wind", levels = fuel_lvl, ordered = T)
    )
  )]
  # Cap distance to water at 1km and capacity at 500 MW
  gen_water[, `:=`(
    dist_water_plot = if_else(dist_water_m > 1000, 1000, dist_water_m),
    capacity_gen_plot = if_else(capacity_gen > 500, 500, capacity_gen)
  )]
  # Plot colors
  pc = magma(5, begin = 0.1, end = 0.9)
  # Plot histogram of distance to water by fuel category
  plot_water = ggplot(
    data = gen_water,
    aes(x = dist_water_plot, fill = fuel_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel A:} Distance to nearest body of water"
    )),
    TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
  ) +
  scale_x_continuous(
    TeX("Distance (m)"),
    breaks = seq(0, 1e3, 200),
    labels = c(seq(0, 800, 200), TeX("$\\geq 1,000$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  scale_fill_manual(values = pc[1:4]) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(legend.position = "none") +
  facet_grid(vars(fuel_plot))
  # Plot histogram of capacity by fuel category
  plot_capacity = ggplot(
    data = gen_water,
    aes(x = capacity_gen_plot, fill = fuel_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    color = NA,
    bins = 50
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel B:} Generating-unit capacity"
    )),
    TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
  ) +
  scale_x_continuous(
    TeX("Capacity (MW)"),
    breaks = seq(0, 500, 100),
    labels = c(seq(0, 400, 100), TeX("$\\geq$ 500")), 
  ) +
  scale_y_continuous(
    "Count"
  ) +
  scale_fill_manual(values = pc[1:4]) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(legend.position = "none") +
  facet_grid(vars(fuel_plot))
  # Combine plots
  plot_final = plot_water | plot_capacity
  # Save the final plot
  ggsave(
    plot = plot_final,
    path = here("Figures", "Distributions"),
    filename = "hist-water-capacity.pdf",
    device = cairo_pdf,
    height = 7.5,
    width = 6.5
  )
  # Clean up
  rm(plot_water, plot_capacity, plot_final, gen_dt, gen_water, water_distance)
  invisible(gc())


