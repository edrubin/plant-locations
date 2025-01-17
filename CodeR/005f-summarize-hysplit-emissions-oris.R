
# Notes ----------------------------------------------------------------------------------
#   Goal: Summarize month-plant emissions
#   Time: 


# Data notes -----------------------------------------------------------------------------


# Summaries ------------------------------------------------------------------------------


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, readxl, 
    raster, sf, fasterize, rmapshaper,
    cowplot, grid, gridExtra, viridis, patchwork,
    data.table, lubridate, splitstackshape,
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)


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


# Load data: Plant info ------------------------------------------------------------------
  # Load plant information (to link units to plants)
  plant_dt = here(
    "DataRaw", "PlantsEPA", "PlantInfo(2005-2019).csv"
  ) %>% read_csv() %>% janitor::clean_names() %T>% setDT()
  # Grab desired variables
  plant_dt %<>% .[, .(
    unique_id = key_unit_id,
    oris = oris_code
  )] %>% unique()


# Load data: Batch crosswalk -------------------------------------------------------------
  # Load batch crosswalk
  batch_xwalk = here(
    "DataClean", "hysplit-completed", "tracker", "batch_xwalk.csv"
  ) %>% fread()
  # Load batch data
  units_dt = here(
    "DataClean", "hysplit-completed", "tracker", "units_for_hysplit.fst"
  ) %>% read_fst(as.data.table = T)
  # Split unit IDs
  units_dt[, unique_id := unique_ids]
  units_dt %<>% cSplit(
    indt = .,
    splitCols = "unique_id",
    sep = ",",
    direction = "long",
    drop = F,
    type.convert = F
  )
  units_dt[, unique_id := unique_id %>% as.numeric()]
  # Merge with plant_dt to get plant ORIS code
  units_dt %<>% merge(
    y = plant_dt,
    by = "unique_id",
    all.x = F,
    all.y = F,
    sort = F
  )
  # Copy concatenated IDs
  units_dt[, unique_id := unique_ids]
  # Split concatenated units
  units_dt %<>% cSplit(
    indt = .,
    splitCols = "unique_id",
    sep = ",",
    direction = "long",
    drop = F,
    type.convert = F
  )
  units_dt[, unique_id := unique_id %>% as.numeric()]


# Load data: eGRID -----------------------------------------------------------------------
  # Load plant-level eGRID data
  egrid_dt = here(
    "DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"
  ) %>% read_fst(as.data.table = T)
  # Grab coal plants
  egrid_coal = egrid_dt[any_coal_gen == 1]
  # Order by capacity
  setorder(egrid_coal, -max_capacity_plant)


# Load data: Emissions for HYSPLIT runs --------------------------------------------------
  # Load prepared emissions data (at the day by unit-group level)
  em_dt = here(
    "DataClean", "hysplit-completed", "emissions", "emissions-for-hysplit.fst"
  ) %>% read_fst(as.data.table = T)
  # Add month
  em_dt[, mo := month(op_date)]


# Data work: Find desired ORIS codes and their HYSPLIT batches ---------------------------
  # Find target ORIS codes
  oris_v = here("Figures", "PlantWind") %>%
    dir(pattern = "plant-wind-.*.pdf", recursive = T) %>%
    str_extract("[0-9]+(?=\\.)")
  # Find units and batches associated with these ORIS codes batches
  target_dt = plant_dt[oris %in% oris_v]
  # Find these units' batches
  target_dt %<>% merge(
    y = units_dt,
    by = c("unique_id", "oris"),
    all.x = T,
    all.y = F
  )
  setorder(target_dt, oris, unique_id, op_date)
  # Drop missing (non-coal generating units)
  target_dt %<>% .[!is.na(op_date)]
  # Find the batches
  target_dt[, `:=`(
    batch = lapply(
      X = target_dt$iter,
      FUN = function(i) batch_xwalk[between(i, start, stop), n]
    ) %>% unlist()
  )]


# Load data: HYSPLIT particles' locations ------------------------------------------------
  # Iterate over desired ORIS codes (largest coal plants)
  for (oris_i in c(628, 703, 1378, 3470, 6257)) {
    # Find HYSPLIT particle-location files for oris_i
    hy_files = file.path(
      "~", "Documents", "hysplit-completed", "tracker",
      paste0(target_dt[oris == oris_i, batch] %>% unique(), ".fst")
    )
    # Iterate over ORIS codes
    hy_dt = mclapply(
      X = hy_files,
      mc.cores = min(12, length(hy_files)),
      FUN = function(f) {
        # Load the file's data
        read_fst(
          path = f,
          as.data.table = T
          # columns = c("iter", "op_date", "unique_ids", "co_location", "co_home")
        ) %>% .[iter %in% unique(target_dt[oris == oris_i, iter])]
      }
    ) %>% rbindlist(use.names = T, fill = T)
    # Join emissions
    hy_dt %<>% merge(
      y = em_dt,
      by = c("unique_ids", "op_date"),
      all = F
    )
    # Divide mass by '420' (currently: 420 particles modeled per day)
    hy_dt[, `:=`(
      mass_so2 = mass_so2 / 420,
      mass_nox = mass_nox / 420
    )]
    # Particles to 'sf'
    january_sf = hy_dt[op_date == ymd(20050101), .(
      lat, lon, hour
    )] %>% st_as_sf(coords = c("lon", "lat"), crs = 4269)
    july_sf = hy_dt[op_date == ymd(20050701), .(
      lat, lon, hour
    )] %>% st_as_sf(coords = c("lon", "lat"), crs = 4269)
    # Find counties that intersect with January and/or July particles
    # Afterward: Simplify the polygons
    st_i = st_join(
      x = state_sf,
      y = hy_dt[op_date %in% ymd(20050101, 20050701), .(lon, lat)] %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4269) %>% st_bbox() %>%
        st_as_sfc() %>% st_as_sf() %>% mutate(value = 1)
    ) %>% filter(!is.na(value)) %>% ms_simplify(keep = 0.05)
    # Find the state's counties and simplify their polygons
    co_i = county_sf %>% filter(STATEFP %in% st_i$STATEFP) %>% ms_simplify(keep = 0.01)
    # Create figures of particle paths on Jan 1 and July 1
    # January
    plot_jan = ggplot() +
    geom_sf(
      data = co_i %>% st_transform(102009),
      fill = NA,
      color = "grey80",
      size = 0.05
    ) +
    geom_sf(
      data = st_i %>% st_transform(102009),
      fill = NA,
      color = "black",
      size = 0.15
    ) +
    geom_sf(
      data = january_sf %>% st_transform(102009),
      aes(color = hour),
      alpha = 0.3
    ) +
    scale_color_viridis_c(option = "magma") +
    theme_void() +
    theme(legend.position = "none")
    # July
    plot_july = ggplot() +
    geom_sf(
      data = co_i %>% st_transform(102009),
      fill = NA,
      color = "grey80",
      size = 0.05
    ) +
    geom_sf(
      data = st_i %>% st_transform(102009),
      fill = NA,
      color = "black",
      size = 0.15
    ) +
    geom_sf(
      data = july_sf %>% st_transform(102009),
      aes(color = hour),
      alpha = 0.3
    ) +
    scale_color_viridis_c(option = "magma") +
    theme_void() +
    theme(legend.position = "none")
    # US-level plot
    plot_us = ggplot() +
    geom_sf(
      data = state_sf %>% ms_simplify(keep = 0.01) %>% st_transform(102009),
      aes(
        color = STATEFP %in% st_i$STATEFP,
        fill = STATEFP %in% st_i$STATEFP
      ),
      size = 0.15,
      alpha = 0.8
    ) +
    scale_fill_manual(values = c("grey98", magma(10)[6])) +
    scale_color_manual(values = c("grey83", magma(10)[6])) +
    theme_void() +
    theme(legend.position = "none")
    # Figure out area for thumbnail
    if (oris_i %in% c(628)) {
      sub_area = patchwork::area(t = 75, l = 10, b = 95, r = 35)
    }
    if (oris_i %in% c(703, 1378, 3470, 6257)) {
      sub_area = patchwork::area(t = 75, l = 65, b = 95, r = 90)
    }
    # if (oris_i %in% c(3470)) {
    #   sub_area = patchwork::area(t = 75, l = 45, b = 95, r = 70)
    # }
    # Combine plots
    jan_full = plot_jan + plot_us +
    plot_layout(
      design = c(
        patchwork::area(t = 1, l = 1, b = 100, r = 100),
        sub_area
      )
    )
    july_full = plot_july + plot_us +
    plot_layout(
      design = c(
        patchwork::area(t = 1, l = 1, b = 100, r = 100),
        sub_area
      )
    )
    # Save (first find approximate plot dimensions)
    dim_x = st_i %>% st_bbox %>% extract(c(1,3)) %>% diff() %>% multiply_by(0.85)
    dim_y = st_i %>% st_bbox %>% extract(c(2,4)) %>% diff()
    max_dim = max(dim_x, dim_y)
    ggsave(
      plot = jan_full,
      path = here("Figures", "hysplit", "Examples"),
      filename = paste0("hysplit-oris-", oris_i, "-january.pdf"),
      height = dim_y / max_dim * 10,
      width = dim_x / max_dim * 10
    )
    ggsave(
      plot = july_full,
      path = here("Figures", "hysplit", "Examples"),
      filename = paste0("hysplit-oris-", oris_i, "-july.pdf"),
      height = dim_y / max_dim * 10,
      width = dim_x / max_dim * 10
    )
    # Crop
    system2(
      command = "pdfcrop", 
      args = c(
        here(
          "Figures", "hysplit", "Examples",
          paste0("hysplit-oris-", oris_i, "-january.pdf")
        ), 
        here(
          "Figures", "hysplit", "Examples",
          paste0("hysplit-oris-", oris_i, "-january.pdf")
        )
      )
    )
    system2(
      command = "pdfcrop", 
      args = c(
        here(
          "Figures", "hysplit", "Examples",
          paste0("hysplit-oris-", oris_i, "-july.pdf")
        ), 
        here(
          "Figures", "hysplit", "Examples",
          paste0("hysplit-oris-", oris_i, "-july.pdf")
        )
      )
    )
  }


# Figure: Colorscale for HYSPLIT time plot -----------------------------------------------
  # Create a simple plot with the desired colorscale
  gg_scale =  ggplot(
    data = data.table(x = seq(0, 48, 0.1)),
    aes(x = x, y = x, color = x)
  ) +
  geom_point() +
  theme_void(base_size = 10, base_family = "Merriweather") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm")
  ) +
  scale_color_viridis_c(
    "Hours since release",
    option = "magma",
    breaks = c(0, 12, 24, 36, 48)
  ) +
  guides(colour = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    keywidth = 4,
    keyheight = 0.7
  )) +
  coord_cartesian(xlim = c(-1,-1), ylim = c(-1,-1))
  # Save
  ggsave(
    plot = gg_scale,
    path = here("Figures", "hysplit", "Examples"),
    filename = "colorbar.pdf",
    device = cairo_pdf,
    width = 5.5,
    height = 1
  )
  system2(
    command = "pdfcrop",
    args = c(
      here("Figures", "hysplit", "Examples", "colorbar.pdf"),
      here("Figures", "hysplit", "Examples", "colorbar.pdf")
    )
  )


# Rasters (not used) ---------------------------------------------------------------------
  # Create a raster where the bounding box using 5th/95th percentiles of lon/lat
  # winter_so2 = rasterize(
  #   x = hy_dt[mo == 1,] %>% st_as_sf(coords = c("lon", "lat")),
  #   y = raster(
  #     xmn = hy_dt[,lon] %>% quantile(0.05),
  #     xmx = hy_dt[,lon] %>% quantile(0.95),
  #     ymn = hy_dt[,lat] %>% quantile(0.05),
  #     ymx = hy_dt[,lat] %>% quantile(0.95),
  #     resolution = 0.05,
  #     vals = 0
  #   ),
  #   field = "mass_so2",
  #   fun = "sum",
  #   background = 0
  # )
  # summer_so2 = rasterize(
  #   x = hy_dt[mo == 7,] %>% st_as_sf(coords = c("lon", "lat")),
  #   y = raster(
  #     xmn = hy_dt[,lon] %>% quantile(0.05),
  #     xmx = hy_dt[,lon] %>% quantile(0.95),
  #     ymn = hy_dt[,lat] %>% quantile(0.05),
  #     ymx = hy_dt[,lat] %>% quantile(0.95),
  #     resolution = 0.05,
  #     vals = 0
  #   ),
  #   field = "mass_so2",
  #   fun = "sum",
  #   background = 0
  # )
  