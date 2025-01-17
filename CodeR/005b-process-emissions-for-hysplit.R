
# Notes ----------------------------------------------------------------------------------
#   Goal: Aggregate emissions to level of HYSPLIT runs

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, data.table, splitstackshape, lubridate, 
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Load data: HYSPLIT batches -------------------------------------------------------------
  # Load the dataset of units I ran together in HYSPLIT
  units_dt = here(
    "DataClean", "hysplit-completed", "tracker", "units_for_hysplit.fst"
  ) %>% read_fst(
    as.data.table = T,
    columns = c("op_date", "unique_ids")
  )

# Load data: Emissions -------------------------------------------------------------------
  # Load the 2005 emissions data
  em_dt = here("DataRaw", "EmissionsEPA", "Y2005DailyData.csv") %>% fread()
  # Clean names
  setnames(em_dt, em_dt[0,] %>% janitor::clean_names() %>% names())
  # Select and format variables
  em_dt %<>% .[, .(
    unique_id,
    op_date = op_date %>% mdy(),
    mass_so2 = daily_so2_mass_tons,
    mass_nox = daily_n_ox_mass_tons
  )]

# Data work: Aggregate emissions to HYSPLIT runs -----------------------------------------
  # Duplicate the concatenated unit ID column
  units_dt[, unique_id := unique_ids]
  # Split concatenated unit IDs into individual rows (to join to emissions data)
  units_dt %<>% cSplit(
    splitCols = "unique_id",
    sep = ",",
    direction = "long",
    drop = F
  )
  # Merge with emissions data by unique_id and op_date
  units_dt %<>% merge(
    y = em_dt,
    by = c("unique_id", "op_date"),
    all.x = T,
    all.y = F
  )
  # Aggregate emissions to HYSPLIT run (HYSPLIT run = unique_ids by op_date)
  units_dt %<>% .[, .(
    mass_so2 = sum(mass_so2, na.rm = T),
    mass_nox = sum(mass_nox, na.rm = T)
  ), by = .(unique_ids, op_date)]

# Save -----------------------------------------------------------------------------------
  # Save units_dt
  write_fst(
    x = units_dt,
    path = here(
      "DataClean", "hysplit-completed", "emissions",
      "emissions-for-hysplit.fst"
    ),
    compress = 100
  )