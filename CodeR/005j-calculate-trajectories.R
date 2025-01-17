
# Notes ----------------------------------------------------------------------------------
#   Goal: Find HYSPLIT trajectories for January and July 2005 for a given county.
#     - Define county
#     - Find trajectories of particles as move into the county.

# Data notes -----------------------------------------------------------------------------
#   - County shapefile includes the county's water area—e.g., water area in the 
#     Great Lakes still counts as the county (should be conservative in our measure).
#   - Using 100-meter resolution (and finer) raster of counties
#   - HYSPLIT failed for one unit in July 2005

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    raster, fasterize, sf,
    tidyverse, data.table, lubridate, 
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Define 'example' conuties --------------------------------------------------------------
  # County of interest: TN 47-157
  co = "47157"

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
  co_sf %<>% rename(county = GEOID)
  # Find distances between counties and our target county
  co_dist = st_distance(
    x = st_centroid(co_sf),
    y = st_centroid(filter(co_sf, county == co))
  )
  # Convert to data table
  dist_dt = data.table(
    county = co_sf$county,
    dist = as.numeric(co_dist[,1]) / 1e3
  )

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
    mc.cores = 11,
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
      # Find particles in county 'co'
      f_dt[, in_co := co_location == co]
      # Find maximal hour in 'co' for each day-unit-particle 
      f_dt[, max_hour_co := max(in_co * hour), by = .(op_date, unique_ids, particle_i)]
      # Keep particles up until their last moment in 'co'
      f_dt %<>% .[hour <= max_hour_co]
      # Merge emissions data with particles in the target counties
      f_dt %<>% merge(
        em_dt,
        by = c("op_date", "unique_ids"),
        all = F,
        sort = F
      )
      # Divide emissions by number of HYSPLIT particles in the hour (typically 420)
      f_dt[, `:=`(
        mass_so2 = mass_so2 / n_particles,
        mass_nox = mass_nox / n_particles
      )]
      # Return f_dt
      return(f_dt)
    }
  ) %>% rbindlist(use.names = T, fill = T)
  # Drop observations with no emissions (should be redundant)
  hy_dt %<>% .[mass_so2 > 0 | mass_nox > 0]

# Load data: Batch crosswalk -------------------------------------------------------------
  # Load batch crosswalk
  batch_xwalk = here(
    "DataClean", "hysplit-completed", "tracker", "batch_xwalk.csv"
  ) %>% fread()
  # Load batch data
  units_dt = here(
    "DataClean", "hysplit-completed", "tracker", "units_for_hysplit.fst"
  ) %>% read_fst(as.data.table = T)
  # Find the iterations relevant to the particles in hy_dt
  hy_dt %<>% merge(
    y = units_dt[, .(op_date, unique_ids, iter)],
    by = c("op_date", "unique_ids"),
    all.x = T,
    all.y = F
  )
  # Find the batches relevant to each of the iterations
  # (First: Inflate xwalk for merge)
  batch_xwalk[, n_iter := stop - start + 1]
  batch_xwalk %<>% .[rep(1:.N, n_iter)]
  batch_xwalk[, n_iter := NULL]
  batch_xwalk[, iter := start + 1:.N - 1, by = n]
  batch_xwalk[, `:=`(start = NULL, stop = NULL)]
  setnames(batch_xwalk, old = "n", new = "batch")
  hy_dt %<>% merge(
    y = batch_xwalk,
    by = "iter",
    all.x = T,
    all.y = F
  )

# Function: Process HYSPLIT data ---------------------------------------------------------
  # Function that processes a given file
  geo_dt = mclapply(
    X = hy_dt[,unique(batch)],
    FUN = function(b) {
      # Load HYSPLIT data in file 'f'
      f_dt = paste0(
        "~/Documents/hysplit-completed/tracker/", b, ".fst"
      ) %>% read_fst(as.data.table = T)
      # Join to hy_dt batch subset
      f_dt = merge(
        x = hy_dt[batch == b],
        y = f_dt,
        by = c("iter", "particle_i", "hour", "op_date", "unique_ids"),
        all.x = T,
        all.y = F
      )
      # Order
      setorder(f_dt, op_date, unique_ids, particle_i, hour)
      # Return
      return(f_dt)
    },
    mc.cores = 11
  ) %>% rbindlist(use.names = T, fill = T)
  # Drop unwanted variables
  geo_dt[, `:=`(batch = NULL, iter = NULL, max_hour_co = NULL)]
  # Add unique ID for lines
  geo_dt[, line_id := .GRP, by = .(op_date, unique_ids, particle_i)]
  # Add distance between source county and target county
  geo_dt %<>% merge(
    x = .,
    y = dist_dt,
    by.x = "co_home",
    by.y = "county",
    sort = F,
    all.x = T,
    all.y = F
  )
  # Create a list of individual lines: January
  jan_lines = mclapply(
    X = geo_dt[mo == 1, unique(line_id)],
    FUN = function(l) {
      geo_dt[line_id == l, .(x = lon, y = lat)] %>% as.matrix()
    },
    mc.cores = 12
  )
  # Create a list of individual lines: July
  july_lines = mclapply(
    X = geo_dt[mo == 7, unique(line_id)],
    FUN = function(l) {
      geo_dt[line_id == l, .(x = lon, y = lat)] %>% as.matrix()
    },
    mc.cores = 12
  )
  # Convert to sf linestring: January
  jan_lines %<>% st_multilinestring() %>% st_sfc() %>% st_sf()
  st_crs(jan_lines) = 4326
  jan_lines %<>% st_cast("LINESTRING")
  # Convert to sf linestring: July
  july_lines %<>% st_multilinestring() %>% st_sfc() %>% st_sf()
  st_crs(july_lines) = 4326
  july_lines %<>% st_cast("LINESTRING")
  # Add features
  jan_lines = bind_cols(
    jan_lines,
    geo_dt[
      mo == 1,
      lapply(.SD, first),
      by = .(line_id),
      .SDcols = c("op_date", "co_home", "mass_so2", "mass_nox", "dist")
    ]
  )
  july_lines = bind_cols(
    july_lines,
    geo_dt[
      mo == 7,
      lapply(.SD, first),
      by = .(line_id),
      .SDcols = c("op_date", "co_home", "mass_so2", "mass_nox", "dist")
    ]
  )
  # Save
  st_write(
    obj = jan_lines,
    dsn = here(
      "DataClean", "county-month-shares", "co-coal-sources-47157-01",
      "co-coal-sources-47157-01.shp"
    )
  )
  st_write(
    obj = july_lines,
    dsn = here(
      "DataClean", "county-month-shares", "co-coal-sources-47157-07",
      "co-coal-sources-47157-07.shp"
    )
  )

