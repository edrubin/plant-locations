
# Notes ----------------------------------------------------------------------------------
#   Goal: Create a CSV and shapefile with all counties in US with 'border' indicator.
#         Indicator = 1 if county is on a state border; 0 otherwise.
#         Repeat for TigerLines and cartographic boundary files from Census (2016).
#   Inputs:
#     - here("DataRaw","usa48","usa_48.shp") shapefile of US 48
#   Outputs:
#     - here("DataClean","maps","county_states_borders.csv")

# Data notes -----------------------------------------------------------------------------

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, sf,
    data.table, parallel,
    magrittr, here
  )

# Load data: County shapefiles -----------------------------------------------------------
  # State FIPS codes for continental US
  state_fips = maps::state.fips$fips %>% unique() %>% str_pad(2, "left", 0)
  # TigerLines (TL) counties
  county_tl = here(
    "DataRaw", "Census", "tl_2016_us_county", "tl_2016_us_county.shp"
  ) %>% st_read(
    query = paste(
      "SELECT GEOID, STATEFP FROM \"tl_2016_us_county\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  ) %>% rename(county = GEOID, state = STATEFP)
  # Cartographic boundaries (CB) counties
  county_cb = here(
    "DataRaw", "Census", "cb_2016_us_county_500k", "cb_2016_us_county_500k.shp"
  ) %>% st_read(
    query = paste(
      "SELECT GEOID, STATEFP FROM \"cb_2016_us_county_500k\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  ) %>% rename(county = GEOID, state = STATEFP)

# Load data: State shapefiles ------------------------------------------------------------
  # State FIPS codes for continental US
  state_fips = maps::state.fips$fips %>% unique() %>% str_pad(2, "left", 0)
  # TigerLines (TL) counties
  state_tl = here(
    "DataRaw", "Census", "tl_2016_us_state", "tl_2016_us_state.shp"
  ) %>% st_read(
    query = paste(
      "SELECT STATEFP FROM \"tl_2016_us_state\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  ) %>% rename(state = STATEFP)
  # Cartographic boundaries (CB) counties
  state_cb = here(
    "DataRaw", "Census", "cb_2016_us_state_500k", "cb_2016_us_state_500k.shp"
  ) %>% st_read(
    query = paste(
      "SELECT STATEFP FROM \"cb_2016_us_state_500k\" WHERE STATEFP IN",
      state_fips %>% paste0("'", ., "'", collapse = ", ") %>% paste0("(", ., ")")
    )
  ) %>% rename(state = STATEFP)

# Data work: Find border counties --------------------------------------------------------
# Steps:
#   1. Convert state shape file to multi-line string.
#   2. Find intersection of state lines and county polygons. 
#   3. Create data with indicator: 1 if county in this intersection, zero if not.
  # Convert states to lines
  state_tl %<>% st_cast("MULTILINESTRING")
  state_cb %<>% st_cast("MULTILINESTRING")  
  # Find counties that touch the state borders (TigerLines)
  matched_tl = mclapply(
    X = state_tl$state %>% unique(),
    FUN = function(st) {
      st_join(
        x = county_tl %>% filter(state == st),
        y = state_tl %>% filter(state == st) %>% rename(state_match = state),
        join = st_touches
      ) 
    }, 
    mc.cores = 11
  ) %>% rbindlist(use.names = T)
  # Find counties that touch the state borders (cartographic boundaries)
  matched_cb = mclapply(
    X = state_cb$state %>% unique(),
    FUN = function(st) {
      st_join(
        x = county_cb %>% filter(state == st),
        y = state_cb %>% filter(state == st) %>% rename(state_match = state),
        join = st_touches
      ) 
    }, 
    mc.cores = 11
  ) %>% rbindlist()
  # Create indicators for boundaries
  matched_tl[, brdr_tl := !is.na(state_match)]
  matched_cb[, brdr_cb := !is.na(state_match)]
  # Merge to get indicators across files
  matched_tl %<>% merge(
    y = matched_cb[, .(county, brdr_cb)],
    by = "county",
    all = T
  )
  matched_cb %<>% merge(
    y = matched_tl[, .(county, brdr_tl)],
    by = "county",
    all = T
  )
  # Drop unwanted variables
  matched_tl[, c("state_match") := NULL]
  matched_cb[, c("state_match") := NULL]
  # Indicator for mismatch
  matched_tl[, mismatch := brdr_tl != brdr_cb]
  matched_cb[, mismatch := brdr_tl != brdr_cb]
  # Merge to a single dataset (dropping geometry)
  borders_dt = copy(matched_tl)
  borders_dt[, geometry := NULL]

# Save data ------------------------------------------------------------------------------
  # Save table
  write_csv(
    x = borders_dt,
    path = here("DataClean", "Maps", "county_states_borders.csv")
  )
  # Convert and save spatial data to shapefiles
  st_write(
    obj = matched_tl %>% st_as_sf() %>% rename(border = brdr_tl),
    dsn = here(
      "DataClean", "maps", "CensusBorders",
      "tl-borders", "tl-borders.shp"
    ),
    delete_layer = T
  )
  st_write(
    obj = matched_cb %>% st_as_sf() %>% rename(border = brdr_cb),
    dsn = here(
      "DataClean", "maps", "CensusBorders",
      "cb-borders", "cb-borders.shp"
    ),
    delete_layer = T
  )
