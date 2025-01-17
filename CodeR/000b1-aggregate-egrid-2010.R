
# Notes ----------------------------------------------------------------------------------
#   Goal: Combine cleaned eGRID files (2010–2018) data.

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, data.table, lubridate, splitstackshape,
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Load data: Plant info ------------------------------------------------------------------
  # Plant data from eGRID
  egrid_2018 = here(
    "DataClean", "Plants", "All", "generator-egrid-all-2018.fst"
  ) %>% read_fst(as.data.table = T)
  egrid_2016 = here(
    "DataClean", "Plants", "All", "generator-egrid-all-2016.fst"
  ) %>% read_fst(as.data.table = T)
  egrid_2014 = here(
    "DataClean", "Plants", "All", "generator-egrid-all-2014.fst"
  ) %>% read_fst(as.data.table = T)
  egrid_2012 = here(
    "DataClean", "Plants", "All", "generator-egrid-all-2012.fst"
  ) %>% read_fst(as.data.table = T)
  egrid_2010 = here(
    "DataClean", "Plants", "All", "generator-egrid-all-2010.fst"
  ) %>% read_fst(as.data.table = T)

# Data work: Combine datasets ------------------------------------------------------------
  # Add year to eGRID files
  egrid_2018[, egrid_year := 2018]
  egrid_2016[, egrid_year := 2016]
  egrid_2014[, egrid_year := 2014]
  egrid_2012[, egrid_year := 2012]
  egrid_2010[, egrid_year := 2010]
  # Stack the datasets
  egrid_dt = rbindlist(
    list(egrid_2018, egrid_2016, egrid_2014, egrid_2012, egrid_2010),
    fill = T,
    use.names = T
  )
  # Convert lon/lat to numeric
  egrid_dt[, `:=`(
    lon = lon %>% as.numeric(),
    lat = lat %>% as.numeric()
  )]
  # Keep generators that are operating ('OP'), out of service ('OA' or 'OS'),
  # retired ('RE'), stand-by ('SB'), or in testing phase ('TS')
  egrid_dt = egrid_dt[status_gen %in% c("OP", "OA", "OS", "RE", "SB", "TS")]

# Create plant-level data for location p-values ------------------------------------------
  # Plant-level data
  plant_dt = egrid_dt[, .(
    fips_state = fips_state %>% first(),
    fips_county = fips_county %>% first(),
    lon = lon %>% median(),
    lat = lat %>% median(),
    year_online = min(year_online),
    fuel_cat_plant = fuel_cat_plant %>% unique() %>% str_sort() %>% paste0(collapse = ","),
    fuel_cat_gen = fuel_cat_gen %>% unique() %>% str_sort() %>% paste0(collapse = ","),
    max_capacity_plant = max(capacity_plant),
    any_coal_gen = max(fuel_cat_gen == "COAL"),
    any_gen_above_25mw = max(i_big)
  ), by = oris] %>% unique()
  # Save
  write_fst(
    x = plant_dt,
    path = here("DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"),
    compress = 50
  )
