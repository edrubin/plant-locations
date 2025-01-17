
# Notes ----------------------------------------------------------------------------------
#   Goals: 
#     - Summarize share of particles in each county by source county.
#     - Create shapefiles for chosen 'example' counties.
#   Direct dependencies:
#     - 005a-process-hysplit.R

# Data notes -----------------------------------------------------------------------------

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, readxl, sf, rmapshaper,
    data.table, lubridate, 
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Define example conuties ----------------------------------------------------------------
  # Counties of interest in IL, MO, and TN
  co_tn = "47157"
  # co_in = c("18051", "18125", "18037", "18163", "18173", "18147")
  # co_il = c("17133", "17163", "17157", "17119")
  # co_mo = c("29071", "29183", "29189", "29099")

# Load data: County shapefile ------------------------------------------------------------
  # State FIPS codes for continental US
  state_fips = maps::state.fips$fips %>% unique() %>% str_pad(2, "left", 0)
  # Load TL shapefile of counties
  co_sf = here(
    "DataRaw", "Census", "tl_2016_us_county", "tl_2016_us_county.shp"
  ) %>% st_read(
    query = paste(
      "SELECT GEOID FROM \"tl_2016_us_county\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  )
  # Rename GEOID to co
  co_sf %<>% rename(co = GEOID)
  # Sample 5% of the shapefile points (to reduce figure size later)
  # co_sf_small = co_sf %>% ms_simplify(keep = 0.05, keep_shapes = T)

# Load data: State shapefile -------------------------------------------------------------
  # State FIPS codes for continental US
  state_fips = maps::state.fips$fips %>% unique() %>% str_pad(2, "left", 0)
  # Load TL shapefile of counties
  st_sf = here(
    "DataRaw", "Census", "tl_2016_us_state", "tl_2016_us_state.shp"
  ) %>% st_read(
    query = paste(
      "SELECT GEOID FROM \"tl_2016_us_state\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  )
  # Rename GEOID to st
  st_sf %<>% rename(st = GEOID)

# Load data: Nonattainment designations --------------------------------------------------
  # Load dataset
  na_dt = here(
    "DataRaw", "NonAttainmentEPA", "nayro.xls"
  ) %>% read_xls() %T>% setDT()
  # Drop Alaska, Guam, and Puerto Rico
  na_dt %<>% .[!(st_abbr %in% c("AK", "GU", "PR"))]
  # Keep 2005 non-attainment counties 
  na_dt %<>% .[!is.na(yr2005), .(
    co = paste0(fips_state, fips_cnty),
    pollutant
  )] %>% unique()
  # Combine non-attainment reasons and force uniqueness
  na_dt[, pollutant := paste0(pollutant, collapse = ", "), by = co]
  na_dt %<>% unique()
  # Add an indicator for nonattainment (helpful later)
  na_dt[, i_na := 1]

# Load data: Emissions for HYSPLIT runs --------------------------------------------------
  # Load prepared emissions data (at the day by unit-group level)
  em_dt = here(
    "DataClean", "hysplit-completed", "emissions", "emissions-for-hysplit.fst"
  ) %>% read_fst(as.data.table = T)
  # Add month
  em_dt[, mo := month(op_date)]

# Load data: HYSPLIT particles' locations ------------------------------------------------
  # Find HYSPLIT particle-location files
  hy_files = here("DataClean", "hysplit-completed", "processed") %>% dir(full.names = T)
  # Iterate over files
  hy_dt = mclapply(
    X = hy_files,
    mc.cores = 10,
    FUN = function(f) {
      # Load the file's data
      f_dt = read_fst(
        path = f,
        as.data.table = T,
        columns = c(
          "op_date", "unique_ids", "particle_i", "hour",
          "co_location", "co_home"
        )
      )
      # Find the number of particles at hour 1 for each day-plant
      f_dt[hour == 1, `:=`(n_particles = .N), by = .(op_date, unique_ids)]
      # Extend n_particles to the rest of the hours 
      f_dt[, n_particles := mean(n_particles, na.rm = T), by = .(op_date, unique_ids)]
      # Merge emissions data with particles in the target counties
      f_dt %<>% merge(
        em_dt,
        by = c("op_date", "unique_ids"),
        all = F,
        sort = F
      )
      # Sum emissions by source county and month
      f_dt %>% .[, .(
        mass_so2 = sum(mass_so2 / n_particles, na.rm = T),
        mass_nox = sum(mass_nox / n_particles, na.rm = T)
      ), by = .(co_location, co_home, mo)]
    }
  ) %>% rbindlist(use.names = T, fill = T)
  # Sum again by destination, source, and month
  hy_dt %<>% .[, .(
    mass_so2 = sum(mass_so2, na.rm = T),
    mass_nox = sum(mass_nox, na.rm = T)
  ), by = .(co_location, co_home, mo)]
  # Extend to all possible month-county combinations
  hy_dt %<>% merge(
    y = CJ(
      mo = hy_dt[,unique(mo)],
      co_location = hy_dt[,unique(co_location)],
      co_home = co_sf$co
    ),
    by = c("co_location", "co_home", "mo"),
    sort = T,
    all = T
  )
  # Fill NAs with 0's
  hy_dt[is.na(mass_so2), mass_so2 := 0]
  hy_dt[is.na(mass_nox), mass_nox := 0]
  # Create shares (by destination-month)
  hy_dt[, `:=`(
    share_so2 = mass_so2 / sum(mass_so2),
    share_nox = mass_nox / sum(mass_nox)
  ), by = .(co_location, mo)]
  # Change name of 'co_location' to 'co_destination' and 'co_home' to 'co'
  setnames(
    hy_dt,
    old = c("co_location", "co_home"),
    new = c("co_destination", "co_source")
  )

# Create and save TN cluster -------------------------------------------------------------
  # TN cluster
  tn_dt = hy_dt[co_destination %in% co_tn]
  # Drop unwanted variables
  tn_dt[, `:=`(co_destination = NULL, share_so2 = NULL, share_nox = NULL)]
  # Sum within source counties and months
  tn_dt %<>% .[, .(
    mass_so2 = sum(mass_so2),
    mass_nox = sum(mass_nox)
  ), by = .(co_source, mo)]
  # Recalculate shares for the cluster
  tn_dt[, `:=`(
    share_so2 = mass_so2 / sum(mass_so2),
    share_nox = mass_nox / sum(mass_nox)
  ), by = .(mo)]
  # Change name of 'co_source' to 'co' to match spatial data
  setnames(tn_dt, old = "co_source", new = "co")
  # Join shapefile with nonattainment status
  tn_sf = left_join(x = co_sf, y = na_dt, by = "co")
  # Fill 'i_na' with 0 when NA
  tn_sf %<>% mutate(i_na = if_else(is.na(i_na), 0, 1))
  # Add HYSPLIT data
  tn_sf %<>% right_join(x = ., y = tn_dt, by = "co")
  # Add an indicator for 'own county'
  tn_sf %<>% mutate(i_own = as.numeric(co %in% co_tn))
  # Save shapefiles for each month 
  st_write(
    obj = tn_sf %>% filter(mo == 1),
    dsn = here(
      "DataClean", "county-month-shares",
      "co-coal-shares-tennessee-01",
      "co-coal-shares-tennessee-01.shp"
  )
  st_write(
    obj = tn_sf %>% filter(mo == 7),
    dsn = here(
      "DataClean", "county-month-shares",
      "co-coal-shares-tennessee-07",
      "co-coal-shares-tennessee-07.shp"
  )

# Figures: Summer TN ---------------------------------------------------------------------
  # Load data
  tn_dt = here(
    "DataClean", "county-month-shares",
    "co-coal-shares-tennessee-07",
    "co-coal-shares-tennessee-07.shp"
  ) %>% st_read()
  # Convert to data table
  setDT(tn_dt)
  # Drop unwanted columns
  tn_dt[, c("geometry", "pollutant", "mo") := NULL]

  # Histogram
  gg_tmp = ggplot(
    data = tn_dt[share_nox > 0.001],
    aes(x = share_nox, fill = ..x.., color = ..x..)
  ) +
  geom_histogram(bins = 150) +
  # geom_vline(xintercept = 0, size = 1/10) +
  geom_hline(yintercept = 0, size = 1/10) +
  scale_y_continuous("") +
  scale_x_continuous("", labels = scales::percent) +
  scale_color_viridis_c(option = "magma", direction = -1) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_minimal(base_size = 6.5, base_family = "Charter") +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
  # Save
  ggsave(
    plot = gg_tmp,
    path = here("Figures", "county-sources", "example-47157"),
    filename = "histogram-nox.pdf",
    device = cairo_pdf,
    height = 2.4,
    width = 2.3
  )
