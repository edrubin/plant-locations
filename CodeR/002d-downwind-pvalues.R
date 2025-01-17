
# Notes ----------------------------------------------------------------------------------
#   Goal: Calculate exact-test p-values for share of area downwin in county/state.
#   Output: 
#     - Values of 'table-strategy-pvalues.tex'

# Data notes -----------------------------------------------------------------------------
#   - Using Census 'CB' definitions for upwind/downwind area and border counties as 
#     they better match our purpose (i.e., land area in county/state).
#   - Dropped 143 plants (1.4%) whose reported coordinates were not within 1km buffer of
#     reported counties. Affects 2 coal plants (0.3%) and 12 natural gas plant (0.6%).

# Setup ----------------------------------------------------------------------------------
	# Packages
	pacman::p_load(purrr, fastverse, here, magrittr)
  fastverse_extend(topics = c("IO", "DT", "ST", "VI"))

# Load data: Plant and downwind/upwind data ----------------------------------------------
	# Load upwind/downwind dataset: County areas
	wind_dt = here(
    # "DataClean", "PlantWind", "plant-wind-areas-tl.rds"
		"DataClean", "PlantWind", "plant-wind-areas-cb.rds"
	) %>% readRDS()
	# Load plant-level eGRID data
	plant_dt = here(
		"DataClean", "Plants", "All", "plant-egrid-2010to2018.fst"
	) %>% read_fst(as.data.table = T)
  # Join plant info to upwind/downwind data
  plant_wind = merge(
  	x = plant_dt,
  	y = wind_dt[, -c("fips_state", "fips_county", "lon", "lat")],
  	by = "oris",
  	all.x = FALSE,
  	all.y = TRUE
  )
  # Count plants whose coordinates don't match listed counties (even when buffered).
  plant_wind[,.(
    n = sum(notes == "Reported coordinates do not match reported (buffered) county."),
    pct = mean(notes == "Reported coordinates do not match reported (buffered) county.")
  )]
  plant_wind[str_detect(fuel_cat_plant, "COAL"),.(
    n = sum(notes == "Reported coordinates do not match reported (buffered) county."),
    pct = mean(notes == "Reported coordinates do not match reported (buffered) county.")
  )]
  plant_wind[str_detect(fuel_cat_plant, "GAS"),.(
    n = sum(notes == "Reported coordinates do not match reported (buffered) county."),
    pct = mean(notes == "Reported coordinates do not match reported (buffered) county.")
  )]
  # Drop the offending plants (whose coordinates don't match the listed counties)
  plant_wind %<>% .[notes != "Reported coordinates do not match reported (buffered) county."]
  
# Load data: County-state borders --------------------------------------------------------
  # Load dataset with indicator for whether counties are on their states' borders
  co_dt = here("DataClean", "Maps", "county_states_borders.csv") %>% fread()
  # Merge (and pad) FIPS
  co_dt[, `:=`(
    state = state %>% str_pad(2, "left", 0),
    county = county %>% str_pad(5, "left", 0)
  )]
  # Keep desired columns
  co_dt %<>% .[, .(fips = county, border_county = brdr_cb)]
  # Merge onto plant-wind data
  plant_wind %<>% merge(
    y = co_dt,
    by = "fips",
    all.x = TRUE,
    all.y = FALSE
  )

# Load data: Coastline counties ----------------------------------------------------------
  # Load dataset
  coast_dt = here(
    "DataRaw", "Census", "coastline-counties-list.xlsx"
  ) %>% readxl::read_xlsx(skip = 3) %>% janitor::clean_names() %T>% setDT()
  coast_dt %<>% .[1:255]
  # Merge with plant-wind dataset
  plant_wind %<>% merge(
    x = .,
    y = coast_dt[, .(fips = state_county_fips, coast = 1)],
    by = "fips",
    all.x = TRUE,
    all.y = FALSE
  )
  plant_wind[is.na(coast), coast := 0]

# Function: Upwind/downwind p-value calculations -----------------------------------------
# Arguments:
#   'area' = (Character) Area for up/downwind (i.e., 'state', 'county', 'or').
#   'fuel_cat' = (Character) The fuel category (e.g., 'COAL').
#   'yr' = ('pre','post','all') Subset to pre/post-Clean Air Act builds (or all builds).
#   'big' = (T,F) Subset to plants with capacities larger than 25 MW.
#   'negate' = (T,F) Flip the fuel category (i.e., non-coal rather than coal).
#   'border' = (T,F,NULL) Subset to plants in border (T) or nonborder (F) counties.
#   'coastal' = (T,F,NULL,'all') Subset to plants in coastal (T) or noncoastal (F) counties.
  wind_calc_plant = function(
    area, fuel_cat, yr = "all", big = NULL, negate = FALSE, border = NULL, coastal = NULL
  ) {
    # Capitalize 'fuel_cat'
    fuel_cat %<>% str_to_upper()
  	# Dataset defined by the 'query'
    if (area == "county") {
      q_dt = plant_wind[str_detect(fuel_cat_plant, fuel_cat, negate = negate), .(
        oris, fips, border_county, coast, year_online, any_gen_above_25mw,
        upwind_area = upwind_area_co,
        downwind_area = downwind_area_co
      )]
    }
  	if (area == "state") {
      q_dt = plant_wind[str_detect(fuel_cat_plant, fuel_cat, negate = negate), .(
        oris, fips, border_county, coast, year_online, any_gen_above_25mw,
        upwind_area = upwind_area_st,
        downwind_area = downwind_area_st
      )]
    }
    if (area %in% c("or", "and")) {
      q_dt = plant_wind[str_detect(fuel_cat_plant, fuel_cat, negate = negate), .(
        oris, fips, border_county, coast, year_online, any_gen_above_25mw,
        upwind_area_co,
        downwind_area_co,
        upwind_area_st,
        downwind_area_st
      )]
    }
    # If 'border' is non-NULL, then select border/non-border counties
    if (!is.null(border)) {
      q_dt %<>% .[border_county == border]
    }
    # If 'coast' is non-NULL, then select coastal/non-coastal counties
    if (!is.null(coastal)) {
      if (coastal != "all") {
        q_dt %<>% .[coast == coastal]
      }
    }
  	# Pre-/post-CAA if specified
  	if (yr == "pre") {
  		q_dt %<>% .[year_online < 1963]
  	}
    if (yr == "post") {
      q_dt %<>% .[year_online >= 1963]
    }
  	# Drop 'small' (sub-25MW) plants
  	if (!is.null(big)) {
  		q_dt %<>% .[(any_gen_above_25mw == 1) == big]
  	}
  	# Number of observations in specified group (fuel-era-size)
  	n = q_dt[, .N]
  	# Number of 'strategically located' plants
# NOTE: Changes based upon area, since 'or' uses EITHER county OR state
    if (area %in% c("state", "county")) {
    	n_strategic = q_dt[, downwind_area < upwind_area] %>% sum()
    }
    if (area == "or") {
      n_strategic = q_dt[, (
       (downwind_area_co < upwind_area_co) |
       (downwind_area_st < upwind_area_st)
      )] %>% sum()
    }
    if (area == "and") {
      n_strategic = q_dt[, (
       (downwind_area_co < upwind_area_co) &
       (downwind_area_st < upwind_area_st)
      )] %>% sum()
    }
  	# Exact p-value associated with distribution of plants
    if (area %in% c("state", "county")) {
  		pv = lapply(
        X = n_strategic:n, 
        FUN = function(k) dbinom(x = k, size = n, prob = 0.5)
      ) %>% unlist() %>% sum()
    }
    if (area %in% c("or")) {
      pv = lapply(
        X = n_strategic:n, 
        FUN = function(k) dbinom(x = k, size = n, prob = 0.75)
      ) %>% unlist() %>% sum()
    }
    if (area %in% c("and")) {
      pv = lapply(
        X = n_strategic:n, 
        FUN = function(k) dbinom(x = k, size = n, prob = 0.25)
      ) %>% unlist() %>% sum()
    }
		# Return data
		data.table(
      area,
			fuel_cat = ifelse(negate, paste0("NON-", fuel_cat), fuel_cat),
			border_sub = ifelse(
        is.null(border), "All",
        ifelse(border, "State-border counties", "Non-border counties")
      ),
      coast_sub = ifelse(
        is.null(coastal), 
        "All",
        ifelse(
          is.logical(coastal),
          ifelse(coastal == TRUE, "Coastal", "Non-coastal"),
          ifelse(str_to_lower(coastal) == "all", "All", NA)
        )
      ),
      yr,
			big,
			n,
			n_strategic,
      pct_strategic = n_strategic / n,
			p_value = pv
		)
	}

# Functions for tables -------------------------------------------------------------------
  # Formatting individual rows
  subtable_format = function(level, data) {
    # Take subset
    sub_table = data[area == level, 7:10]
    # Transform to matrix
    sub_mat = sub_table %>% t()
    # Counts
    c(
      "\n",
      paste("Panel", level),
      "\n",
      c(" \\: Count &&", rbind(
        comma(sub_mat[1,], accuracy = 1.0),
        c("&&", "\\\\ \n")
      )),
      # Strategic counts
      c(" \\: Count \\textit{strategic} &&", rbind(
        comma(sub_mat[2,], accuracy = 1.0),
        c("&&", "\\\\ \n")
      )),
      # Strategic percent
      c(" \\: Percent \\textit{strategic} &&", rbind(
        percent(sub_mat[3,], accuracy = 0.01) %>% str_replace_all("%", "\\\\%"),
        c("&&", "\\\\ \n")
      )),
      ifelse(
        level == "and",
        paste0(
          " \\multicolumn{5}{l}{\\small \\quad Fisher's exact test of H$_\\text{o}$: ",
          "Downwind area $\\leq$ upwind area} in \\textbf{county and state} \\\\ \n"
        ),
        paste0(
          " \\multicolumn{5}{l}{\\small \\quad Fisher's exact test of H$_\\text{o}$: In-\\textbf{",
          level,
          "} downwind area $\\leq$ upwind area} \\\\ \n"
        )
      ),
      paste0(
        " \\multicolumn{5}{l}{\\small \\quad \\textit{Under H$_o$: E[Percent strategic: ",
        ifelse(
          level == "and",
          "County $\\wedge$ State] = 25\\%}} \\\\",
          paste0(str_to_title(level), "] = 50\\%}} \\\\")
        )
      ),
      "\n",
      # P-values
      c(" \\: \\textit{P}-value &&", rbind(
        comma(sub_mat[4,], accuracy = 0.0001) %>% str_replace_all("0.0000", "$<$0.0001"),
        c("&&", "\\\\ \n")
      ))
    )
  }
  # Super function
  table_format = function(full_table) {
    # Run the function on each subset
    if ("and" %in% tmp_table[,area]) {
      out = map(
        c("county", "state", "and"),
        subtable_format,
        data = full_table
      ) %>% unlist()
    } else {
      out = map(
        c("county", "state"),
        subtable_format,
        data = full_table
      ) %>% unlist()
    }
    # Add panel titles
    out[out == "Panel county"] = 
      "\n \\midrule \n\n \\multicolumn{5}{@{}l}{\\textbf{Panel a:} Siting strategically within \\textbf{county}} \\\\"
    out[out == "Panel state"] = 
      "\n \\midrule \n\n \\multicolumn{5}{@{}l}{\\textbf{Panel b:} Siting strategically within \\textbf{state}} \\\\"
    out[out == "Panel and"] = 
      "\n \\midrule \n\n \\multicolumn{5}{@{}l}{\\textbf{Panel c:} Siting strategically within \\textbf{both county \\textit{and} state}} \\\\"
    cat(out)
  }

# Table: Coal vs. natural gas ------------------------------------------------------------
  tmp_table = pmap_dfr(
    CJ(
      area = c("county", "state", "and"),
      fuel_cat = c("COAL", "GAS"),
      yr = "all",
      big = TRUE,
      negate = FALSE,
      coastal = "all"
    ),
    wind_calc_plant
  )
  tmp_table %>% table_format()
  rm(tmp_table)

# Table: Coal vs. natural gas, non-coastal counties --------------------------------------
  tmp_table = pmap_dfr(
    CJ(
      area = c("county", "state", "and"),
      fuel_cat = c("COAL", "GAS"),
      yr = "all",
      big = TRUE,
      negate = FALSE,
      coastal = FALSE
    ),
    wind_calc_plant
  )
  tmp_table %>% table_format()
  rm(tmp_table)
