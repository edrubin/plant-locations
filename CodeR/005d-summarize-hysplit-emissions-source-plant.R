
# Notes ----------------------------------------------------------------------------------
#   Goal: Summarize share of particles still in home county or state.
#         Weight by emissions volume.
#   Direct dependencies:
#     - 005a-process-hysplit.R
#     - 005a-process-hysplit.R

# Data notes -----------------------------------------------------------------------------
#   - Dropped units that do not match to an ORIS code.

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

# Load data: Non-aggregated emissions data -----------------------------------------------
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
  # Add month
  em_dt[, mo := month(op_date)]
  # Add plant ORIS code to the emissions data
# NOTE: Dropping units that do not match to an ORIS code (7 units)
  em_dt %<>% merge(
    y = plant_dt,
    by = "unique_id",
    all.x = F,
    all.y = F,
    sort = F
  )
  # Replace NAs with 0s
  em_dt[is.na(mass_so2), mass_so2 := 0]
  em_dt[is.na(mass_nox), mass_nox := 0]
  # Add total emissions for each unit-month
  em_dt[, `:=`(
    total_so2 = sum(mass_so2, na.rm = T),
    total_nox = sum(mass_nox, na.rm = T)
  ), by = .(mo, oris)]

# Load data: HYSPLIT particles' locations ------------------------------------------------
  # Find HYSPLIT particle-location files
  hy_files = here("DataClean", "hysplit-completed", "processed") %>% dir(full.names = T)
  # Iterate over files
  hy_dt = mclapply(
    X = hy_files,
    mc.cores = 12,
    FUN = function(f) {
      # Load the file's data
      f_dt = read_fst(
        path = f,
        as.data.table = T,
        columns = c(
          "op_date", "unique_ids", "hour",
          "co_location", "co_home", "i_left_county", "i_left_state"
        )
      )
      # Aggregate across particles
      f_dt %<>% .[, .(
        pct_left_county = mean(i_left_county, na.rm = T),
        pct_left_state = mean(i_left_state, na.rm = T)
      ), by = .(unique_id = unique_ids, op_date, hour)]
      # Separate
      f_dt %<>% cSplit(
        indt = .,
        splitCols = "unique_id",
        sep = ",",
        direction = "long",
        drop = F,
        type.convert = F
      )
      f_dt[, unique_id := unique_id %>% as.numeric()]
      # Merge emissions data with f_dt
      f_dt %<>% merge(
        em_dt,
        by = c("op_date", "unique_id"),
        all = F,
        sort = F
      )
      # Calculate emissions-weighted averages of out-of-county
      # and -state by hour-plant-month
      f_dt[, .(
        pct_out_county_so2 = sum(pct_left_county * mass_so2 / total_so2, na.rm = T),
        pct_out_county_nox = sum(pct_left_county * mass_nox / total_nox, na.rm = T),
        pct_out_state_so2 = sum(pct_left_state * mass_so2 / total_so2, na.rm = T),
        pct_out_state_nox = sum(pct_left_state * mass_nox / total_nox, na.rm = T),
        so2_weight = sum(mass_so2 / total_so2, na.rm = T),
        nox_weight = sum(mass_nox / total_nox, na.rm = T)
      ), by = .(hour, mo, oris)]
    }
  ) %>% rbindlist(use.names = T, fill = T)

  # Aggregate up (weighting is already done: just need to add)
  hy_dt %<>% .[, .(
    pct_out_county_so2 = sum(pct_out_county_so2, na.rm = T),
    pct_out_county_nox = sum(pct_out_county_nox, na.rm = T),
    pct_out_state_so2 = sum(pct_out_state_so2, na.rm = T),
    pct_out_state_nox = sum(pct_out_state_nox, na.rm = T),
    so2_weight = sum(so2_weight, na.rm = T),
    nox_weight = sum(nox_weight, na.rm = T)
  ), by = .(mo, hour, oris)]
  # Divide by total weights (some don't quite hit '1')
  hy_dt[, `:=`(
    pct_out_county_so2 = pct_out_county_so2 / so2_weight,
    pct_out_county_nox = pct_out_county_nox / nox_weight,
    pct_out_state_so2 = pct_out_state_so2 / so2_weight,
    pct_out_state_nox = pct_out_state_nox / nox_weight
  )]
  # Remove weight columns
  hy_dt[, c("so2_weight", "nox_weight") := NULL]

# Save -----------------------------------------------------------------------------------
  # Save hy_dt
  write_fst(
    x = hy_dt,
    path = here(
      "DataClean", "hysplit-completed", "final",
      "hysplit-source-departures-plant.fst"
    ),
    compress = 75
  )
