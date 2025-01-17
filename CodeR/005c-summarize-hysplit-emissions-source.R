
# Notes ----------------------------------------------------------------------------------
#   Goal: Summarize share of particles still in home county or state.
#         Weight by emissions volume.
#   Direct dependencies:
#     - 005a-process-hysplit.R
#     - 005a-process-hysplit.R

# Data notes -----------------------------------------------------------------------------

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, data.table, lubridate, 
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Load data: Emissions for HYSPLIT runs --------------------------------------------------
  # Load prepared emissions data (at the day by unit-group level)
  em_dt = here(
    "DataClean", "hysplit-completed", "emissions", "emissions-for-hysplit.fst"
  ) %>% read_fst(as.data.table = T)
  # Add month
  em_dt[, mo := month(op_date)]
  # Add total emissions for each month
  em_dt[, `:=`(
    total_so2 = sum(mass_so2, na.rm = T),
    total_nox = sum(mass_nox, na.rm = T)
  ), by = mo]

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
          "op_date", "unique_ids", "hour",
          "co_location", "co_home", "i_left_county", "i_left_state"
        )
      )
      # Aggregate across particles
      f_dt %<>% .[, .(
        pct_left_county = mean(i_left_county, na.rm = T),
        pct_left_state = mean(i_left_state, na.rm = T)
      ), by = .(unique_ids, op_date, hour)]
      # Merge emissions data with f_dt
      f_dt %<>% merge(
        em_dt,
        by = c("op_date", "unique_ids"),
        all = F,
        sort = F
      )
      # Calculate emissions-weighted averages of out-of-county and -state by hour
      f_dt[, .(
        pct_out_county_so2 = sum(pct_left_county * mass_so2 / total_so2, na.rm = T),
        pct_out_county_nox = sum(pct_left_county * mass_nox / total_nox, na.rm = T),
        pct_out_state_so2 = sum(pct_left_state * mass_so2 / total_so2, na.rm = T),
        pct_out_state_nox = sum(pct_left_state * mass_nox / total_nox, na.rm = T),
        so2_weight = sum(mass_so2 / total_so2, na.rm = T),
        nox_weight = sum(mass_nox / total_nox, na.rm = T)
      ), by = .(hour, mo)]
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
  ), by = .(mo, hour)]
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
      "hysplit-source-departures.fst"
    ),
    compress = 75
  )
