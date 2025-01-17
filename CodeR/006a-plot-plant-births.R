
# Notes ----------------------------------------------------------------------------------
#   Goal: Plot distribution of coal and nat. gas plant build dates. 
#   Output: 

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, data.table, lubridate, fst,
    extrafont, patchwork, latex2exp,
    here, magrittr
  )

# Load data: Plant and downwind/upwind data ----------------------------------------------
  # Load plant-level eGRID data
  plant_dt = here(
    "DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"
  ) %>% read_fst(as.data.table = T)
  # Join plant info to upwind/downwind data (keeping only plants with generators > 25MW)
  plant_dt %<>% .[any_gen_above_25mw == 1]
  # Create 5-digit FIPS
  plant_dt[, `:=`(fips = paste0(fips_state, fips_county))]
  # Drop observations with missing FIPS (one gas plant)
  plant_dt %<>% .[!is.na(fips_state)]

# Load data: County-state borders --------------------------------------------------------
  # Load dataset with indicator for whether counties are on their states' borders
  co_dt = here("DataClean", "Maps", "county_states_borders.csv") %>% fread()
  # Merge (and pad) FIPS
  co_dt[, `:=`(
    state = state %>% str_pad(2, "left", 0),
    county = county %>% str_pad(5, "left", 0)
  )]
  # Keep desired columns
  co_dt %<>% .[, .(fips = county, border_county = brdr_cb)]
  # Merge onto plant-wind data
  plant_dt %<>% merge(
    y = co_dt,
    by = "fips",
    all.x = T,
    all.y = F
  )
  # Drop observations missing their border status (AK and HI)
  plant_dt %<>% .[!is.na(border_county)]

# Figure: Distribution of build dates ----------------------------------------------------
  # Build plot dataset
  plot_dt = plant_dt[str_detect(fuel_cat_plant, "COAL|GAS"), .(
    is_coal = str_detect(fuel_cat_plant, "COAL"),
    is_gas = str_detect(fuel_cat_plant, "GAS"),
    border = if_else(
      border_county == T,
      "Counties on state borders",
      "Interior counties"
    ),
    year = year_online
  )]
  # Count annual builds by fuel and county-border status
  plot_coal = plot_dt[is_coal == T, .(
    count = .N
  ), by = .(border, year)]
  plot_gas = plot_dt[is_gas == T, .(
    count = .N
  ), by = .(border, year)]
  # Find years for plot
  plot_years = plot_dt[,year] %>% range()
  
  # Figure: Natural gas
# Note: As the 'bottom' plot, this plot needs the x-axis label and bottom legend
  gg_gas = ggplot(
    data = plot_gas,
    aes(x = year, y = count, fill = border)
  ) +
  geom_col(
    color = NA,
  ) +
  geom_hline(yintercept = 0, size = 1/4) +
  geom_vline(
    xintercept = 1963,
    size = 0.3,
    linetype = "dashed",
    alpha = 0.2
  ) +
  ggtitle(TeX(paste0("\\textbf{Panel B:} Natural gas plant births"))) +
  scale_x_continuous("Year the plant came online", limits = plot_years, breaks = seq(1910, 2010, by = 20)) +
  scale_y_continuous("Number of new plants") +
  scale_fill_viridis_d("", option = "magma", begin = 0.15, end = 0.85, direction = -1) +
  theme_minimal(base_family = "Merriweather", base_size = 8) +
  facet_wrap(~ border, nrow = 2, strip.position = "top") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(angle = 0, hjust = 0, face = "plain", size = 7),
    panel.grid.minor.y = element_blank()
  )
  # Figure: Coal
# Note: As the 'top' plot, this plot has no x-axis label or bottom legend
  gg_coal = ggplot(
    data = plot_coal,
    aes(x = year, y = count, fill = border)
  ) +
  geom_col(
    color = NA,
  ) +
  geom_hline(yintercept = 0, size = 1/4) +
  geom_vline(
    xintercept = 1963,
    size = 0.3,
    linetype = "dashed",
    alpha = 0.2
  ) +
  geom_text(
    data = data.frame(
      border = "Counties on state borders",
      year = 1963,
      count = plot_coal[,max(count)],
      text = "CAA of 1963"
    ),
    aes(x = year, y = count, label = text),
    size = 2.5,
    family = "Merriweather"
    # fontface = "bold"
  ) +
  ggtitle(TeX(paste0("\\textbf{Panel A:} Coal plant births"))) +
  scale_x_continuous("", limits = plot_years, breaks = seq(1910, 2010, by = 20)) +
  scale_y_continuous("Number of new plants") +
  scale_fill_viridis_d("", option = "magma", begin = 0.15, end = 0.85, direction = -1) +
  theme_minimal(base_family = "Merriweather", base_size = 8) +
  facet_wrap(~ border, nrow = 2, strip.position = "top") +
  theme(
    legend.position = "none",
    strip.text = element_text(angle = 0, hjust = 0, face = "plain", size = 7)
  )
  # Save the plots
  ggsave(
    plot = gg_coal / gg_gas,
    path = here("Figures", "Distributions"),
    filename = "births-borders-coal-gas.pdf",
    device = cairo_pdf,
    height = 7.5,
    width = 6.4
  )
