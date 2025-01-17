
# Notes ----------------------------------------------------------------------------------
#   Goal: Join units and stack stack heights; summarize for paper.
#   Note: Follows 'match_unit_stack.R' from previous project.

# Setup ----------------------------------------------------------------------------------
  # Package
  library(pacman)
  p_load(
    tidyverse, janitor, scales, latex2exp,
    vroom, fst, data.table, lubridate,
    xtable, magrittr, here
  )

# Load data: Stack heights ---------------------------------------------------------------
  stack_dt = here(
    "DataRaw", "PlantsEPA", "StackHeightInfo(20191202).csv"
  ) %>% fread() %>% clean_names()

# Load data: Units -----------------------------------------------------------------------
  unit_dt = here(
    "DataRaw", "PlantsEPA", "PlantInfo(2005-2019).csv"
  ) %>% vroom() %>% clean_names() %T>% setDT()

# Load data: Plants ----------------------------------------------------------------------
  # Load plant-level eGRID data
  plant_dt = here(
    "DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"
  ) %>% read_fst(as.data.table = T)

# Load data: Strategic location ----------------------------------------------------------
  # Load upwind/downwind dataset: County areas
  wind_co = here(
    # "DataClean", "PlantWind", "plant-downwind-upwind-tl-county.rds"
    "DataClean", "PlantWind", "plant-downwind-upwind-cb-county.rds"
  ) %>% readRDS()
  # Load upwind/downwind dataset: State areas
  wind_st = here(
    # "DataClean", "PlantWind", "plant-downwind-upwind-tl-state.rds"
    "DataClean", "PlantWind", "plant-downwind-upwind-cb-state.rds"
  ) %>% readRDS()

# Data work: Unit information ------------------------------------------------------------
  # Drop units missing nameplate capacity
  unit_dt = unit_dt[!is.na(nameplate_capacity)]
  # Keep operating units
  unit_dt = unit_dt[op_status == "OPR"]
  # Keep desired variables
  unit_dt = unit_dt[, .(
    op_year,
    key_unit_id,
    unit_id,
    stack_ids = stack_id_s,
    oris_code,
    lat = latitude,
    lon = longitude,
    capacity = nameplate_capacity,
    capacity_uom = nameplate_capacity_uom,
    primary_fuel_type
  )]
  # Convert everything to MW
  unit_dt[, capacity_mw := capacity]
  unit_dt[capacity_uom == "KLBHR", `:=`(
    capacity_mw = capacity * 1203.3 / 10300
  )]
  unit_dt[capacity_uom == "MMBTUHR", `:=`(
    capacity_mw = capacity * 1e3 / 10300
  )]
  # Keep only coal units with capacity of at least 25 MW
  unit_dt %<>% .[capacity_mw >= 25]
  # Determine whether the unit was ever coal powered
  unit_dt[, is_coal := str_detect(str_to_lower(primary_fuel_type), "coal")]
  unit_dt[, ever_coal := any(is_coal), by = .(key_unit_id, oris_code)]
  # Keep units that were ever coal
  unit_dt = unit_dt[ever_coal == T]

# Data work: Clean stack heights ---------------------------------------------------------
  # Grab desired variables
  stack_dt = stack_dt[, .(
    key_unit_id, oris_code, location_identifier,
    stack_height, begin_date, end_date
  )]
  # Convert dates (and change NA to far into the future)
  stack_dt[, `:=`(
    begin_date = begin_date %>% mdy(),
    end_date = end_date %>% mdy()
  )]
  stack_dt[is.na(end_date), end_date := "2999-12-31" %>% ymd()]
  # Drop stack-height 'observations' (rows) missing the height or cross-area exit
  stack_dt = stack_dt[!is.na(stack_height)]

# Data work: Join unit, stack, and emissions datasets ------------------------------------
  # Subset both datasets to 2018
  unit_dt %<>% .[op_year == 2018]
  stack_dt %<>% .[year(begin_date) <= 2018 & year(end_date) >= 2018]
  # Join data for coal plants
  coal_dt = merge(
    x = unit_dt[is_coal == T, .(
      key_unit_id, unit_id, oris_code, stack_ids, lat, lon, capacity_mw
    )],
    y = stack_dt[, .(key_unit_id, oris_code, location_identifier, stack_height)],
    by.x = c("key_unit_id", "oris_code"),
    by.y = c("key_unit_id", "oris_code"),
    all.x = T,
    all.y = F,
    allow.cartesian = T
  )
  # Merge plant-level data
  coal_dt %<>% merge(
    plant_dt[, .(
      fips = paste0(fips_state, fips_county),
      vintage = year_online,
      oris, fuel_cat_plant, max_capacity_plant, any_gen_above_25mw
    )],
    by.x = "oris_code",
    by.y = "oris",
    all.x = T,
    all.y = F
  )
  # Drop 3 observations missing vintages
  coal_dt %<>% .[!is.na(vintage)]
  # Drop 11 plants with no units above 25MW
  coal_dt %<>% .[any_gen_above_25mw == 1]
  coal_dt[, any_gen_above_25mw := NULL]
  # Add indicator for CAA and stack-regulation regimes
  coal_dt[, `:=`(
    regime = fcase(
      vintage < 1963, "A",
      between(vintage, 1963, 1984), "B",
      vintage >= 1985, "C"
    )
  )]
  # Order/label the factor
  coal_dt[, `:=`(
    regime = factor(
      regime,
      levels = c("A", "B", "C"), 
      labels = c("Pre CAA", "Post CAA; Pre stack reg.", "Post CAA; Post stack reg."),
      ordered = T
    )
  )]
  # Add 'strategic' locations
  coal_dt %<>% merge(
    y = wind_co[, .(oris, strategic_co = downwind_area < upwind_area)],
    by.x = "oris_code",
    by.y = "oris",
    all.x = T,
    all.y = F
  )
  coal_dt %<>% merge(
    y = wind_st[, .(oris, strategic_st = downwind_area < upwind_area)],
    by.x = "oris_code",
    by.y = "oris",
    all.x = T,
    all.y = F
  )
  # Aggregate to the plant level
  coal_plant = coal_dt[, .(
    vintage = min(vintage),
    fuel_cat_plant = first(fuel_cat_plant),
    capacity_plant = max(max_capacity_plant),
    fips = first(fips),
    stack_height = max(stack_height),
    regime = first(regime),
    strategic_co = max(strategic_co),
    strategic_st = max(strategic_st)
  ), by = oris_code]

# Summarize stack heights ----------------------------------------------------------------
  # At the EGU level
  coal_dt[,stack_height %>% summary()]
  # At the plant level
  coal_dt[, .(
    plant_mean = mean(stack_height),
    plant_min = min(stack_height),
    plant_max = max(stack_height)
  ), by = oris_code] %>% summary()
  # Summarize by regulatory regime
  coal_plant[, .(
    mean_height = stack_height %>% mean(),
    se_height = stack_height %>% sd() %>% divide_by(sqrt(.N)),
    median_height = stack_height %>% quantile(probs = 0.5),
    min_height = stack_height %>% min(),
    max_height = stack_height %>% max(),
    n = .N
  ), by = regime]
  # Summarize by regulatory regime and strategic designation
  coal_plant[!is.na(strategic_co), .(
    mean_height = stack_height %>% mean(),
    se_height = stack_height %>% sd() %>% divide_by(sqrt(.N)),
    median_height = stack_height %>% quantile(probs = 0.5),
    min_height = stack_height %>% min(),
    max_height = stack_height %>% max(),
    n = .N
  ), by = .(regime, strategic = strategic_co)] %T>% setorder(regime, strategic)

# Regression -----------------------------------------------------------------------------
  # Add desired variables
  coal_plant[, `:=`(
    strategic = as.numeric(strategic_st),
    regime1 = as.numeric(regime == "Pre CAA"),
    regime2 = as.numeric(regime == "Post CAA; Pre stack reg."),
    regime3 = as.numeric(regime == "Post CAA; Post stack reg.")
  )]
  fixest::feols(
    stack_height ~
    regime2 + regime3 | str_sub(fips, 1, 2),
    data = coal_plant
  ) 
  fixest::feols(
    stack_height ~
    -1 + regime1 + regime2 + regime3 +
    strategic:regime1 + strategic:regime2 + strategic:regime3 | str_sub(fips, 1, 2),,
    data = coal_plant
  ) 

# Figures --------------------------------------------------------------------------------
  # Plot median stack height by year
  ggplot(
    # data = coal_dt[, .(
    data = coal_plant[, .(
      median_height = median(stack_height),
      mean_height = mean(stack_height),
      max_height = max(stack_height),
      min_height = min(stack_height)
    ), .(year = vintage)],
    aes(x = year, y = median_height)
  ) +
  geom_col(fill = "grey80") + 
  geom_hline(yintercept = 0, size = 1/4) +
  geom_vline(xintercept = 1963, color = "blue") +
  geom_vline(xintercept = 1985, color = "red") +
  scale_x_continuous("Year of construction") +
  scale_y_continuous("Median stack height (ft)", labels = scales::comma) +
  theme_minimal()

  # Plot quantiles/means
  ggplot(
    data = coal_dt[vintage >= 1950 & !is.na(strategic_co)],
    # data = coal_plant[vintage >= 1950 & !is.na(strategic_co)],
    aes(x = vintage, y = stack_height, color = regime)
  ) +
  geom_point(shape = 1, stroke = 0.5) +
  geom_hline(yintercept = 0, size = 1/4) +
  # geom_vline(xintercept = 1963, color = "black", alpha = 0.1, size = 2.5) +
  # geom_vline(xintercept = 1985, color = "black", alpha = 0.1, size = 2.5) +
  annotate(
    geom = "rect",
    xmin = 1963, xmax = 1984, ymin = 0, ymax = coal_dt[,max(stack_height)],
    alpha = 0.1
  ) +
  # geom_quantile(quantiles = c(0.25, 0.75), method = "rq", linetype = "longdash", size = 0.25) +
  # geom_quantile(quantiles = c(0.5), method = "rq", linetype = "solid") +
  geom_smooth(method = lm, se = F) +
  annotate(
    x = 1956, y = Inf, label = "Pre CAA",
    geom = "text", family = "SF Compact Display Bold", size = 4, vjust = 1
  ) +
  annotate(
    x = 1974, y = Inf, label = "Post CAA; Pre stack reg.",
    geom = "text", family = "SF Compact Display Bold", size = 4, vjust = 1
  ) +
  annotate(
    x = 2001, y = Inf, label = "Post CAA; Post stack reg.",
    geom = "text", family = "SF Compact Display Bold", size = 4, vjust = 1
  ) +
  scale_x_continuous("Year of construction") +
  scale_y_continuous("Stack height (ft)", labels = scales::comma) +
  scale_color_viridis_d("Regulatory period", option = "magma", end = 0.8) +
  # ggtitle(
  #   "Quantiles of stack heights for new coal units, 1950–2018",
  #   "Quantiles: 25th (dashed), 50th (solid), 75th (dashed)"
  # ) +
  ggtitle(
    "Stack heights for new coal units, 1950–2018"
  ) +
  # facet_wrap(~ factor(
  #   strategic_st | strategic_co,
  #   labels = c("Non-strategically sited", "Strategically sited")
  # ), nrow = 2) +
  theme_minimal(base_family = "SF Compact Display") +
  theme(legend.position = "bottom")
