
# Goal -----------------------------------------------------------------------------------
#   Clean up the eGRID (2012) data: locations, fuel, capacity, etc.

# Notes ----------------------------------------------------------------------------------
#   - Create crosswalk forH fuel and category using eGRID plant data
#   - Drop small units (less than 25 MW nameplate capacity)
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

# Load data: eGRID 2012 ------------------------------------------------------------------
  # Generator-level data
  gen12 = "egrid2012_data.xlsx" %>%
    here::here("DataRaw", "eGRID", .) %>%
    read_xlsx(sheet = "GEN12", skip = 5) %>%
    clean_names() %T>%
    setDT()
  # Plant-level data
  plant12 = "egrid2012_data.xlsx" %>%
    here::here("DataRaw", "eGRID", .) %>%
    read_xlsx(sheet = "PLNT12", skip = 4, na = c("NA", "", "N/A")) %>%
    clean_names() %T>%
    setDT()

# Data work: Fuel cross-walk -------------------------------------------------------------
# NOTE: This crosswalk is one of the ways pre-2012 (<=2012) data differ from post 2012
  # eGRID crosswalk between fuel and fuel category (using plant-level data)
  fuel_xwalk = plant12[, .(plprmfl, plfuelct)] %>% unique()
  # Drop missing 'plprmfl'
  fuel_xwalk %<>% .[!is.na(plprmfl)]
  # Change names
  setnames(fuel_xwalk, c("fuel_gen", "fuel_cat_gen"))
  # Load 2014 cross walk (eGRID does not have non-fossil categories in 2012)
  fuel_xwalk14 = here(
    "DataClean", "Plants", "All", "egrid-fuel-xwalk-2014.csv"
  ) %>% read_csv() %T>% setDT()
  setnames(fuel_xwalk14, old = "fuel_cat_gen", new = "fuel_cat_gen14")
  # Merge 2012 and 2014 crosswalks
  fuel_xwalk %<>% merge(
    y = fuel_xwalk14,
    by = "fuel_gen",
    all.x = T,
    all.y = F
  )
  # Copy over missing categories
  fuel_xwalk[is.na(fuel_cat_gen), fuel_cat_gen := fuel_cat_gen14]
  # Drop 2014 data
  fuel_xwalk[, fuel_cat_gen14 := NULL]
  # Add missing categories
  fuel_xwalk[fuel_gen %in% c("BG", "DG", "MSB"), fuel_cat_gen := "BIOMASS"]
  fuel_xwalk[fuel_cat_gen == "OFSL", fuel_cat_gen := "OTHER FOSSIL"]
  fuel_xwalk = rbindlist(list(
    fuel_xwalk,
    data.table(fuel_gen = "PG", fuel_cat_gen = "GAS")
  ), use.names = T, fill = T)
  # Order
  setorder(fuel_xwalk, fuel_cat_gen)

# Data work: Grab desired variables ------------------------------------------------------
  # Desired variables: Generator data
  gen12 = gen12[, .(
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
  plant12 = plant12[, .(
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
    fuel_plant = plprmfl ,
    capacity_plant = namepcap
  )]

# Data work: Merge crosswalk onto generator data -----------------------------------------
  # Merge fuel crosswalk with generator data
  gen12 %<>% merge(
    y = fuel_xwalk,
    by = "fuel_gen",
    all.x = T,
    all.y = F,
    sort = F
  )

# Data work: Merge crosswalk onto plant data ---------------------------------------------
  # Merge fuel crosswalk with plant data
  plant12 %<>% merge(
    y = fuel_xwalk[, .(fuel_plant = fuel_gen, fuel_cat_plant = fuel_cat_gen)],
    by = "fuel_plant",
    all.x = T,
    all.y = F,
    sort = F
  )
  # Drop plant's fuel
  plant12[, fuel_plant := NULL]

# Data work: Merge generator and plant data ----------------------------------------------
  # Merge
  gen12 %<>% merge(
    y = plant12,
    by = "oris",
    all.x = T,
    all.y = F
  )

# Data work: Fill missing plant types ----------------------------------------------------
  # For plants missing fuel type, assign the fuel category by plurality capacity 
  # Flag plants missing their fuel category
  gen12[, is_missing := is.na(fuel_cat_plant) %>% max(), by = oris]
  # For plants missing fuel type: Sum each plant's generators' capacity by fuel category
  fix_dt = gen12[is_missing == 1, .(
    type_capacity = sum(capacity_gen)
  ), by = .(oris, fuel_cat_gen)]
  # Keep only the plurality categories (ties are broken alphabetically by category)
  setorder(fix_dt, oris, -type_capacity, fuel_cat_gen)
  fix_dt[, rank := 1:.N, by = oris]
  fix_dt %<>% .[rank == 1]
  # Fill in the actual dataset
  for (i in seq_along(fix_dt$oris)) {
    set(
      x = gen12,
      i = gen12[oris == fix_dt[i, oris], which = T],
      j = "fuel_cat_plant",
      value = fix_dt[i, fuel_cat_gen]
    )
  }
  # Drop 'is_missing' variable
  gen12[, is_missing := NULL]
  
# Data work: Flag small/closed generators ------------------------------------------------
  # Indicator for generators with more than 25 MW nameplate capacity
  gen12 = gen12[, i_big := 1 * (capacity_gen >= 25)]
  # Flag operating and stand-by units
  gen12 = gen12[, i_operating := 1 * (status_gen %in% c("OP", "SB"))]

# Save -----------------------------------------------------------------------------------
  # Save cleaned generator-level data
  write_fst(
    x = gen12,
    path = here("DataClean", "Plants", "All", "generator-egrid-all-2012.fst"),
    compress = 50
  )
