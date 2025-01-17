
# Notes ----------------------------------------------------------------------------------
#   Goal: Plot examples of non-contiguous non-attainment areas.

# Data notes -----------------------------------------------------------------------------

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, readxl, 
    raster, sf, rmapshaper,
    cowplot, grid, gridExtra, viridis, patchwork, extrafont, latex2exp,
    data.table, lubridate, splitstackshape,
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Load data: County and state shapes -----------------------------------------------------
  # Load county shape file (and transform to WGS84)
  county_sf = here(
    "DataRaw", "Census", "cb_2016_us_county_500k", "cb_2016_us_county_500k.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP, COUNTYFP, GEOID", 
      "FROM \"cb_2016_us_county_500k\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  ) %>% st_transform(4326)
  # Load county shape file (and transform to WGS84)
  state_sf = here(
    "DataRaw", "Census", "cb_2016_us_state_500k", "cb_2016_us_state_500k.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP", 
      "FROM \"cb_2016_us_state_500k\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  ) %>% st_transform(4326)

# Load data: Non-attainment areas --------------------------------------------------------
  # Load shapefile
  naa_sf = here(
    "DataRaw", "EPA", "pm25_1997std_naa_shapefile", "Pm25_1997Std_NAA.shp"
  ) %>% st_read()
  # Transform to WGS84
  naa_sf %<>% st_transform(4326)

# Load data: Plant info ------------------------------------------------------------------
  # Load plant data (for locations)
  plant_dt = here(
    "DataRaw", "PlantsEPA", "PlantInfo(2005-2019).csv"
  ) %>% read_csv() %>% janitor::clean_names() %T>% setDT()
  # Grab desired variables for coal plants operating in 2005
  plant_dt %<>% .[(
    str_detect(primary_fuel_type, "Coal") & op_status == "OPR" & op_year == 2005
  ), .(
    oris = oris_code,
    lat = latitude,
    lon = longitude
  )] %>% unique()
  # Convert to 'sf'
  plant_sf = plant_dt %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  # Get state FIPS for each plant
  plant_sf %<>% st_join(state_sf)

# Plot 1: "Huntington-Ashland, WV-KY-OH" -------------------------------------------------
  # Subset to the NAA 
  naa1 = naa_sf %>% filter(AREA_NAME == "Huntington-Ashland, WV-KY-OH")
  # Find the bounding box of the NAA
  bbox1 = st_bbox(naa1)
  # Create polygon from the bounding box
  bbox1_sf = bbox1 %>% st_as_sfc() %>% st_as_sf() %>% mutate(value = 1)
  # Find the counties and states that intersect with the bounding box
  co1 = st_join(county_sf, bbox1_sf) %>% filter(!is.na(value))
  st1 = st_join(state_sf, bbox1_sf) %>% filter(!is.na(value))
  # Add state abbreviations
  ab_df = maps::state.fips %>%
    mutate(fips = str_pad(fips, 2, "left", 0)) %>%
    dplyr::select(STATEFP = fips, abb) 
  st1 %<>% left_join(ab_df, by = "STATEFP")
  # Find the plants in the intersecting counties
  pl1 = st_join(plant_sf, bbox1_sf) %>% filter(!is.na(value))
  # Define state FIPS codes in the NAA
  st_fips1 = st1$STATEFP %>% unique()
  # Recast state as line and then intersect with counties
  st1_line = st_intersection(
    st1 %>% st_cast("MULTILINESTRING") %>% dplyr::select(STATEFP),
    co1 %>% dplyr::select(value)
  )
  # The plot
  plot1 = ggplot() +
  # Non-attainment areas
  geom_sf(
    data = naa1,
    fill = magma(100)[85],
    color = NA,
    alpha = 0.5
  ) +
  # Counties
  geom_sf(
    data = co1,
    # data = county_sf %>% filter(STATEFP %in% st_fips1),
    color = "grey60",
    fill = NA,
    size = 0.05
  ) +
  # State borders
  geom_sf(
    data = st1_line,
    # data = state_sf %>% filter(STATEFP %in% st_fips1),
    fill = NA,
    color = "black",
    size = 0.4
  ) +
  geom_sf(
    data = pl1,
    color = magma(100)[55],
    shape = 20,
    size = 1
  ) +
  geom_sf(
    data = pl1,
    color = magma(100)[55],
    shape = 1,
    size = 2.5,
    stroke = 0.4
  ) +
  theme_void() 
  # Thumbnail
  plot1_thumbnail =
  ggplot() +
  geom_sf(
    data = county_sf %>% filter(STATEFP %in% st_fips1) %>% ms_simplify(keep = 0.1),
    aes(
      color = GEOID %in% co1$GEOID,
      fill = GEOID %in% co1$GEOID
    ),
    size = 0.15,
  ) +
  geom_sf(
    data = state_sf %>% filter(STATEFP %in% st_fips1) %>% ms_simplify(keep = 0.1),
    fill = NA,
    color = "grey83",
    size = 0.15,
  ) +
  geom_sf(
    data = naa1,
    fill = magma(100)[85],
    color = NA,
    alpha = 1
  ) +
  geom_sf_text(
    data = st1,
    aes(label = abb),
    color = "grey15",
    family = "Charter",
    check_overlap =,
    size = 2.5,
    nudge_x = c(1.5, -2.5, 0),
    nudge_y = c(-1, 0.6, 1.6)
  ) +
  scale_fill_manual(values = c("grey98", magma(10)[6])) +
  scale_color_manual(values = c("grey83", magma(100)[50])) +
  theme_void() +
  theme(legend.position = "none")
  # Full plot
  plot1_full = plot1 + plot1_thumbnail +
  plot_layout(
    design = c(
      patchwork::area(t = 1, l = 1, b = 100, r = 100),
      patchwork::area(t = 80, l = 67, b = 98, r = 90)
    )
  )
  # Get dimensions and save
  dim_x = co1 %>% st_bbox %>% extract(c(1,3)) %>% diff() %>% multiply_by(0.85)
  dim_y = co1 %>% st_bbox %>% extract(c(2,4)) %>% diff()
  max_dim = max(dim_x, dim_y)
  ggsave(
    plot = plot1_full,
    path = here("Figures", "NonAttainmentAreas"),
    filename = "naa-ex-huntington-ashland.pdf",
    device = cairo_pdf,
    height = 3.5,
    width = 6.4
  )
  # Crop
  system2(
    command = "pdfcrop", 
    args = c(
      here("Figures", "NonAttainmentAreas", "naa-ex-huntington-ashland.pdf"), 
      here("Figures", "NonAttainmentAreas", "naa-ex-huntington-ashland.pdf")
    )
  )

# Plot 2: Evansville, IN -----------------------------------------------------------------
  # Subset to the NAA 
  naa2 = naa_sf %>% filter(AREA_NAME == "Evansville, IN")
  # Find the bounding box of the NAA
  bbox2 = st_bbox(naa2)
  # Create polygon from the bounding box
  bbox2_sf = bbox2 %>% st_as_sfc() %>% st_as_sf() %>% mutate(value = 2)
  # Find the counties and states that intersect with the bounding box
  co2 = st_join(county_sf, bbox2_sf) %>% filter(!is.na(value))
  st2 = st_join(state_sf, bbox2_sf) %>% filter(!is.na(value))
  # Add state abbreviations
  ab_df = maps::state.fips %>%
    mutate(fips = str_pad(fips, 2, "left", 0)) %>%
    dplyr::select(STATEFP = fips, abb) 
  st2 %<>% left_join(ab_df, by = "STATEFP")
  # Drop IL
# NOTE: In bounding box but irrelevant (one county; no plants; no NAA visible)
  st2 %<>% filter(abb != "IL")
  co2 %<>% filter(STATEFP != "17")
  # Find the plants in the intersecting counties
  pl2 = st_join(plant_sf, bbox2_sf) %>% filter(!is.na(value))
  # Define state FIPS codes in the NAA
  st_fips2 = st2$STATEFP %>% unique()
  # Recast state as line and then intersect with counties
  st2_line = st_intersection(
    st2 %>% st_cast("MULTILINESTRING") %>% dplyr::select(STATEFP),
    co2 %>% dplyr::select(value)
  )
  # The plot
  plot2 = ggplot() +
  # Non-attainment areas
  geom_sf(
    data = naa2,
    fill = magma(100)[85],
    color = NA,
    alpha = 0.5
  ) +
  # Counties
  geom_sf(
    data = co2,
    # data = county_sf %>% filter(STATEFP %in% st_fips1),
    color = "grey60",
    fill = NA,
    size = 0.05
  ) +
  # State borders
  geom_sf(
    data = st2_line,
    # data = state_sf %>% filter(STATEFP %in% st_fips1),
    fill = NA,
    color = "black",
    size = 0.4
  ) +
  geom_sf(
    data = pl2,
    color = magma(100)[55],
    shape = 20,
    size = 1
  ) +
  geom_sf(
    data = pl2,
    color = magma(100)[55],
    shape = 1,
    size = 2.5,
    stroke = 0.4
  ) +
  theme_void() 
  # Thumbnail
  plot2_thumbnail =
  ggplot() +
  geom_sf(
    data = county_sf %>% filter(STATEFP %in% st_fips2) %>% ms_simplify(keep = 0.1),
    aes(
      color = GEOID %in% co2$GEOID,
      fill = GEOID %in% co2$GEOID
    ),
    size = 0.15,
  ) +
  geom_sf(
    data = state_sf %>% filter(STATEFP %in% st_fips2) %>% ms_simplify(keep = 0.1),
    fill = NA,
    color = "grey83",
    size = 0.15,
  ) +
  geom_sf(
    data = naa2,
    fill = magma(100)[85],
    color = NA,
    alpha = 1
  ) +
  geom_sf_text(
    data = st2,
    aes(label = abb),
    color = "grey15",
    family = "Charter",
    size = 2,
    check_overlap = T,
    nudge_x = c(-1.9, -4.5),
    nudge_y = c(0.5, -0.5)
  ) +
  scale_fill_manual(values = c("grey98", magma(10)[6])) +
  scale_color_manual(values = c("grey83", magma(100)[50])) +
  theme_void() +
  theme(legend.position = "none")
  # Full plot
  plot2_full = plot2 + plot2_thumbnail +
  plot_layout(
    design = c(
      patchwork::area(t = 1, l = 1, b = 100, r = 100),
      patchwork::area(t = 78, l = 65, b = 96, r = 85)
    )
  )
  # Get dimensions and save
  dim_x = co2 %>% st_bbox %>% extract(c(1,3)) %>% diff() %>% multiply_by(0.85)
  dim_y = co2 %>% st_bbox %>% extract(c(2,4)) %>% diff()
  max_dim = max(dim_x, dim_y)
  ggsave(
    plot = plot2_full,
    path = here("Figures", "NonAttainmentAreas"),
    filename = "naa-ex-evansville.pdf",
    device = cairo_pdf,
    height = 3.5,
    width = 6.4,
    dpi = 100
  )
  system2(
    command = "pdfcrop", 
    args = c(
      here("Figures", "NonAttainmentAreas", "naa-ex-evansville.pdf"), 
      here("Figures", "NonAttainmentAreas", "naa-ex-evansville.pdf")
    )
  )

# Legend for figures ---------------------------------------------------------------------
  # Create a simple plot with the desired colorscale
  tmp = data.table(
    x = c(0,0),
    y = c(0,0),
    z = letters[1:2],
    w = "a"
  )
  # Legend for points (power plants)
  legend1 = ggplot(
    data = tmp,
    aes(x = x, y = x)
  ) +
  geom_point(aes(color = "w"), size = 1.5, shape = 20) +
  geom_point(aes(color = "w"), size = 3, shape = 1, stroke = 0.4) +
  scale_color_manual("", values = magma(100)[55], labels = "Coal-fueled power plant") +
  theme_void(base_size = 10, base_family = "Merriweather") +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-1,-1), ylim = c(-1,-1))
  # Legend for NAA
  legend2 = ggplot(
    data = tmp,
    aes(x = x)
  ) +
  geom_bar(aes(fill = "w"), color = NA) +
  scale_fill_manual("", values = magma(100)[85], labels = "Non-attainment area") +
  theme_void(base_size = 10, base_family = "Merriweather") +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-1,-1), ylim = c(-1,-1))
  # Legend for county/state lines
  legend3 = ggplot(
    data = tmp,
    aes(x = x, y = y)
  ) +
  geom_line(aes(color = z, size = z)) +
  scale_color_manual(
    "",
    values = c("grey40", "black"),
    labels = c("County border", "State border")
  ) +
  scale_size_manual(
    "",
    values = c(0.12, 0.6),
    labels = c("County border", "State border")
  ) +
  theme_void(base_size = 10, base_family = "Merriweather") +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-1,-1), ylim = c(-1,-1))
  # Combine legends
  legend_full = legend2 + legend1 + legend3 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.box = "horizontal")
  # Save
  ggsave(
    plot = legend_full,
    filename = here("Figures", "NonAttainmentAreas", "legend.pdf"), 
    device = cairo_pdf,
    width = 10,
    height = 1,
    dpi = 450
  )
  system2(
    command = "pdfcrop",
    args = c(
      here("Figures", "NonAttainmentAreas", "legend.pdf"), 
      here("Figures", "NonAttainmentAreas", "legend.pdf")
    )
  )
