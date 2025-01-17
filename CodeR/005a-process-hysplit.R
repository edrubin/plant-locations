
# Notes ----------------------------------------------------------------------------------
#   Goal: Aggregate HYSPLIT particle runs for January and July 2005: specifically,
#         (1) from which county did the particle originate?
#         (2) in which county is the particle at time t?

# Data notes -----------------------------------------------------------------------------
#   - County shapefile includes the county's water area—e.g., water area in the 
#     Great Lakes still counts as the county (should be conservative in our measure).
#   - Using 100-meter resolution (and finer) raster of counties
#   - HYSPLIT failed for one unit in July 2005

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    raster, fasterize, sf, velox,
    tidyverse, data.table, lubridate, 
    fst, parallel,
    magrittr, here
  )
  # Options
  options(stringsAsFactors = F)

# Load data: County shapefile ------------------------------------------------------------
  # State FIPS codes for continental US
  state_fips = maps::state.fips$fips %>% unique() %>% str_pad(2, "left", 0)
  # Load TL shapefile of counties
  co_sf = here(
    "tl_2015_us_county", "tl_2015_us_county.shp"
  ) %>% st_read(
    query = paste(
      "SELECT GEOID FROM \"tl_2015_us_county\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  )
  # Convert string GEOID to integer county
  co_sf %<>% transmute(county = GEOID %>% as.integer())

# Data work: Create velox raster of counties ---------------------------------------------
  # Create a 100-meter resolution velox raster that covers the contiguous US
  co_vx = fasterize(
    sf = co_sf,
    raster = raster(
      co_sf,
      res = 1 / (10/9 * 1e3),
      crs = st_crs(co_sf),
    ),
    field = "county"
  ) %>% velox()

# Load data: Batch crosswalk -------------------------------------------------------------
  # Load batch crosswalk
  batch_xwalk =
    here("DataClean", "hysplit-completed", "tracker", "batch_xwalk.csv") %>%
    fread()
  # Load batch data
  units_dt =
    here("DataClean", "hysplit-completed", "tracker", "units_for_hysplit.fst") %>%
    read_fst(as.data.table = TRUE)

# Data work: Match lat/lon of units to counties ------------------------------------------
  # Grab unique combinations of lat/lon from units
  unit_co = units_dt[, .(lon, lat)] %>% unique()
  # Add row ID
  unit_co[, row_id := 1:.N]
  # Convert to 'sf' for spatial merge to counties
  unit_co %<>% st_as_sf(coords = c("lon", "lat"), crs = 4269)
  # Spatial join; convert to data table
  proper_join = st_join(
    x = unit_co %>% st_transform(2163),
    y = co_sf %>% st_transform(2163),
    join = st_within
  ) %T>% setDT()
  proper_join[, geometry := NULL]
  # Convert unit_co to a data table
  setDT(unit_co)
  unit_co[, `:=`(
    lon = st_coordinates(geometry) %>% magrittr::extract(, "X"),
    lat = st_coordinates(geometry) %>% magrittr::extract(, "Y")
  )]
  unit_co[, geometry := NULL]
  # Joins to get back to original unit dataset
  units_dt = unit_co %>% merge(
    y = proper_join,
    by = "row_id"
  ) %>% merge(
    y = units_dt,
    by = c("lon", "lat"),
    sort = F
  )
  # Drop 'row_id' and intermediate dataset
  units_dt[, row_id := NULL]
  rm(unit_co, proper_join)

# Function: Process HYSPLIT data ---------------------------------------------------------
  # Function that processes a given file
  process_hysplit = function(f) {
    # Load HYSPLIT data in file 'f'
    f_dt = f %>% read_fst(as.data.table = T)
    # Convert particle paths to 'sf'
    f_dt %<>% st_as_sf(coords = c("lon", "lat"), crs = 4269)
    # Spatial join to find particle's county
    co_particle = co_vx$extract_points(f_dt) %>% magrittr::extract(,1)
    # Convert the particles data back to data table and drop geometry and other variables
    setDT(f_dt)
    f_dt[, `:=`(
      geometry = NULL,
      iter = NULL
    )]
    # Add particles' locations' counties
    f_dt[, co_location := co_particle]
    # Add particles' 'home' counties (the source plant's county)
    f_dt %<>% merge(
      y = units_dt[, .(co_home = county, op_date, unique_ids)],
      by = c("op_date", "unique_ids"),
      sort = F
    )
    # Change NAs to '99999'
    f_dt[is.na(co_location), co_location := 99999]
    # Change counties to 5-digit strings
    f_dt[, `:=`(
      co_location = co_location %>% str_pad(5, "left", 0),
      co_home = co_home %>% str_pad(5, "left", 0)
    )]
    # Add indicators for out of home county and out of home state
    f_dt[, `:=`(
      i_left_county = as.integer(co_location != co_home),
      i_left_state = as.integer(str_sub(co_location, 1, 2) != str_sub(co_home, 1, 2))
    )]
    # Save
    write_fst(
      x = f_dt,
      path = f %>% str_replace("tracker/", "processed/matched-"),
      compress = 100
    )
    return("Success")
  }

# Data work: HYSPLIT in/out of county ----------------------------------------------------
  # Find HYSPLIT simulation files
  hysplit_files = data.table(
    file = here("tracker") %>% dir(pattern = "^[0-9]+\\.fst$", full.names =  T)
  ) %T>% .[, size := file %>% file.size()]
  # Iterate over files
  f_catch = mclapply(
    X = hysplit_files[size > 0, file],
    FUN = process_hysplit,
    mc.cores = 12,
  )

# Run for missing files ------------------------------------------------------------------
  # Find all batches with non-zero size (one file had an error)
  full = hysplit_files[size > 0, file] %>% str_extract("[0-9]+")
  # Find completed batches
  done = here("processed") %>% dir() %>% str_extract("[0-9]+")
  # Find the remaining batches
  todo = setdiff(full, done)
  # Run function on remaining files
  f_catch = mclapply(
    X = here("tracker", paste0(todo, ".fst")),
    FUN = process_hysplit,
    mc.cores = 12
  )

