
# Notes ----------------------------------------------------------------------------------
#   Goal: Summarize counties' shares of particles by origin county. 
#         Weight by emissions volume.
#   Direct dependencies:
#     - 005a-process-hysplit.R
#     - 005a-process-hysplit.R

# Data notes -----------------------------------------------------------------------------
#   - Used all pollutants.
#   - Does not apply any decay.

# Summaries ------------------------------------------------------------------------------
#   Stats for 2005 non-attainment counties/pollutants:
#     - 702 county-pollutant combinations out of attainment
#     - 485 unique counties in 37 states (excludes Alaska, Guam, and Puerto Rico)
#     - Counts by pollutant generating the non-attainment status:
#         - 8-Hour Ozone (1997): 422 
#         - PM-2.5 (1997): 208  
#         - PM-10 (1987): 49
#         - Carbon Monoxide (1971): 11
#         - Sulfur Dioxide (1971): 10
#         - Lead (1978): 2

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, readxl, data.table, lubridate, 
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Load data: Nonattainment counties ------------------------------------------------------
  # Load dataset
  na_dt = here(
    "DataRaw", "NonAttainmentEPA", "nayro.xls"
  ) %>% read_xls() %T>% setDT()
  # Drop Alaska, Guam, and Puerto Rico
  na_dt %<>% .[!(st_abbr %in% c("AK", "GU", "PR"))]
  # Keep 2005 non-attainment counties 
  na_dt %<>% .[!is.na(yr2005), .(
    st_abbr,
    st = fips_state,
    co = fips_cnty,
    county = paste0(fips_state, fips_cnty),
    pollutant
  )] %>% unique()
  # Find the pollutants for which counties were out of attainment
  na_dt[, .(county, pollutant)] %>% uniqueN()
  na_dt[, .(county)] %>% uniqueN()
  na_dt[, .(st_abbr)] %>% uniqueN()
  na_dt[, pollutant] %>% table()
  # Drop pollutant and force uniqueness
  na_dt[, pollutant := NULL]
  na_dt %<>% unique()

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
        columns = c("op_date", "unique_ids", "co_location", "co_home")
      )
      # Determine whether the particles are 'own county' or 'own state'
      # and whether the source was non-attainment
      f_dt[, `:=`(
        own_county = 1L * (co_location == co_home),
        own_state = 1L * (str_sub(co_location, 1, 2) == str_sub(co_home, 1, 2)),
        location_nonattainment = 1L * (co_location %in% na_dt[,county]),
        source_nonattainment = 1L * (co_home %in% na_dt[,county])
      )]
      # Merge emissions data with f_dt
      f_dt %<>% merge(
        em_dt,
        by = c("op_date", "unique_ids"),
        all = F,
        sort = F
      )
      # Aggregate emissions within county-month (by source group)
      f_dt[, .(
        mass_so2 = sum(mass_so2, na.rm = T),
        mass_nox = sum(mass_nox, na.rm = T)
      ), by = .(mo, county = co_location, own_county, own_state, source_nonattainment)]
    }
  ) %>% rbindlist(use.names = T, fill = T)
  # Aggregate across files' outputs
  hy_dt %<>% .[, .(
    mass_so2 = sum(mass_so2, na.rm = T),
    mass_nox = sum(mass_nox, na.rm = T)
  ), by = .(mo, county, own_county, own_state, source_nonattainment)]
  # Add indicator for whether the county is out of attainment
  hy_dt[, nonattainment := 1L * (county %in% na_dt[,county])]
  # Add groups for own_county/own_state/source_nonattainment combinations
  hy_dt[, `:=`(
    em_grp = fcase(
      own_county == 1, "1",
      (own_county == 0) & (own_state == 1), "2",
      (own_state == 0), "3"
    )
  )]
  hy_dt[, `:=`(
    em_grp_sub = fcase(
      em_grp == "1", "1",
      (em_grp == "2") & (source_nonattainment == 0), "2a",
      (em_grp == "2") & (source_nonattainment == 1), "2n",
      (em_grp == "3") & (source_nonattainment == 0), "3a",
      (em_grp == "3") & (source_nonattainment == 1), "3n"
    )
  )]
  # Aggregate by month, attainment, (sub)group
  grp_dt = hy_dt[, .(
    mass_so2 = sum(mass_so2, na.rm = T),
    mass_nox = sum(mass_nox, na.rm = T)
  ), by = .(mo, nonattainment, em_grp)] 
  sub_dt = hy_dt[, .(
    mass_so2 = sum(mass_so2, na.rm = T),
    mass_nox = sum(mass_nox, na.rm = T)
  ), by = .(mo, nonattainment, em_grp_sub)] 
  # Convert sums to percents
  grp_dt[, `:=`(
    share_so2 = mass_so2 / sum(mass_so2),
    share_nox = mass_nox / sum(mass_nox)
  ), by = .(mo, nonattainment)] 
  sub_dt[, `:=`(
    share_so2 = mass_so2 / sum(mass_so2),
    share_nox = mass_nox / sum(mass_nox)
  ), by = .(mo, nonattainment)] 
  # Groups to factors
  grp_dt[, em_grp := factor(
    em_grp,
    levels = c("1", "2", "3"),
    labels = c(
      "Same county",
      "Other county in same state",
      "Other county in other state"
    ),
    ordered = T
  )]
  sub_dt[, em_grp_sub := factor(
    em_grp_sub,
    levels = c("1", "2a", "2n", "3a", "3n"),
    labels = c(
      "Same county",
      "Other county in same state\nSource county: In attainment",
      "Other county in same state\nSource county: Non-attainment",
      "Other county in other state\nSource county: In attainment",
      "Other county in other state\nSource county: Non-attainment"
    ),
    ordered = T
  )]

# Save -----------------------------------------------------------------------------------
  # Save datasets
  write_fst(
    x = hy_dt,
    path = here(
      "DataClean", "hysplit-completed", "final",
      "hysplit-destination.fst"
    ),
    compress = 75
  )
  write_fst(
    x = grp_dt,
    path = here(
      "DataClean", "hysplit-completed", "final",
      "hysplit-destination-grp.fst"
    ),
    compress = 75
  )
  write_fst(
    x = sub_dt,
    path = here(
      "DataClean", "hysplit-completed", "final",
      "hysplit-destination-sub.fst"
    ),
    compress = 75
  )
