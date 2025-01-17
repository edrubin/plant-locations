
# Goal -----------------------------------------------------------------------------------
#   Clean up the eGRID (2014) data: locations, fuel, capacity, etc.

# Notes ----------------------------------------------------------------------------------
#   - Create crosswalk forH fuel and category using eGRID plant data
#   - Flag 'big' units (more than 25 MW nameplate capacity)
#     This choice matches many programs (Google "25 MW EPA")
#       - New Source Performance Standards
#       - Acid Rain Program, 1999
#       - Cross-StateAir Pollution Rule
#       - MATS
#   - Flag operating and stand-by units

# Setup ----------------------------------------------------------------------------------
   # Packages
   library(pacman)
   p_load(
     tidyverse, readxl, janitor,
     fst, data.table, lubridate,
     parallel, here, magrittr
   )

# Load data: eGRID 2014 ------------------------------------------------------------------
  # Generator-level data
  gen14 = "egrid2014_data.xlsx" %>%
    here::here("DataRaw", "eGRID", .) %>%
    read_xlsx(sheet = "GEN14", skip = 1) %>%
    clean_names() %T>%
    setDT()
  # Plant-level data
  plant14 = "egrid2014_data.xlsx" %>%
    here::here("DataRaw", "eGRID", .) %>%
    read_xlsx(sheet = "PLNT14", skip = 1) %>%
    clean_names() %T>%
    setDT()

# Data work: Fuel cross-walk -------------------------------------------------------------
  # eGRID crosswalk between fuel and fuel category (using plant-level data)
  fuel_xwalk = plant14[, .(plprmfl, plfuelct)] %>% unique() %>% na.omit()
  # Change categories' entries to be readable
  fuel_xwalk[plfuelct == "OFSL", plfuelct := "OTHER FOSSIL"]
  fuel_xwalk[plfuelct == "OTHF", plfuelct := "OTHER"]
  # Order
  setorder(fuel_xwalk, plfuelct)
  # Change names
  setnames(fuel_xwalk, c("fuel_gen", "fuel_cat_gen"))
  # Add missing fuel categories
  fuel_xwalk = rbindlist(list(
    fuel_xwalk,
    data.table(
      fuel_gen = c("PG", "SGP"),
      fuel_cat_gen = c("GAS", "OIL")
    )
  ), use.names = T, fill = T) %>% unique()

# Data work: Grab desired variables ------------------------------------------------------
  # Desired variables: Generator data
  gen14 = gen14[, .(
    oris = orispl,
    id_gen = genid,
    status_gen = genstat,
    mover_gen = prmvr,
    fuel_gen = fuelg1,
    capacity_gen = namepcap,
    generation_gen = genntan,
    year_online = genyronl
  )]
  # Desired variables: Plant data
  plant14 = plant14[, .(
    name_plant = pname,
    oris = orispl,
    id_operator = oprcode,
    lat,
    lon,
    state = pstatabb,
    fips_state = fipsst,
    fips_county = fipscnty,
    region_nerc = nerc,
    subregion_egrid = subrgn,
    fuel_cat_plant = plfuelct,
    capacity_plant = namepcap
  )]

# Data work: Merge crosswalk onto generator data -----------------------------------------
  # Merge fuel crosswalk with generator data
  gen14 %<>% merge(
    y = fuel_xwalk,
    by = "fuel_gen",
    all.x = T,
    all.y = F,
    sort = F
  )

# Data work: Merge generator and plant data ----------------------------------------------
  # Merge
  gen14 %<>% merge(
    y = plant14,
    by = "oris",
    all.x = T,
    all.y = F
  )

# Data work: Fill missing plant types ----------------------------------------------------
  # For plants missing fuel type, assign the fuel category by plurality capacity 
  # Flag plants missing their fuel category
  gen14[, is_missing := is.na(fuel_cat_plant) %>% max(), by = oris]
  # For plants missing fuel type: Sum each plant's generators' capacity by fuel category
  fix_dt = gen14[is_missing == 1, .(
    type_capacity = sum(capacity_gen)
  ), by = .(oris, fuel_cat_gen)]
  # Keep only the plurality categories (ties are broken alphabetically by category)
  setorder(fix_dt, oris, -type_capacity, fuel_cat_gen)
  fix_dt[, rank := 1:.N, by = oris]
  fix_dt %<>% .[rank == 1]
  # Fill in the actual dataset
  for (i in seq_along(fix_dt$oris)) {
    set(
      x = gen14,
      i = gen14[oris == fix_dt[i, oris], which = T],
      j = "fuel_cat_plant",
      value = fix_dt[i, fuel_cat_gen]
    )
  }
  # Drop 'is_missing' variable
  gen14[, is_missing := NULL]
  
# Data work: Flag small/closed generators ------------------------------------------------
  # Indicator for generators with more than 25 MW nameplate capacity
  gen14 = gen14[, i_big := 1 * (capacity_gen >= 25)]
  # Flag operating and stand-by units
  gen14 = gen14[, i_operating := 1 * (status_gen %in% c("OP", "SB"))]

# Save -----------------------------------------------------------------------------------
  # Save cleaned generator-level data
  write_fst(
    x = gen14,
    path = here("DataClean", "Plants", "All", "generator-egrid-all-2014.fst"),
    compress = 50
  )
  # Save crosswalk
  write_csv(
    x = fuel_xwalk,
    path = here("DataClean", "Plants", "All", "egrid-fuel-xwalk-2014.csv")
  )
