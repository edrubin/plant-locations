

# Notes ----------------------------------------------------------------------------------
#   Goal: Plot histograms


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, janitor,,
    fst, data.table, lubridate,
    parallel, here, magrittr
  )


# Load data: Distances to borders --------------------------------------------------------
  # Load plant-to-border distance data
  gen_border_dist = mclapply(
    X = here("DataClean", "DistancePlantBorder", "Generator") %>% dir(full.names = T), 
    FUN = fread, 
    mc.cores = 12
  ) %>% rbindlist(use.names = T, fill = T)
  # Load grid-point-to-border distance data
  grid_border_dist = lapply(
    X = here("DataClean", "DistancePlantBorder", "Grid") %>% dir(full.names = T), 
    FUN = read_fst
  ) %>% rbindlist(use.names = T, fill = T)
  # Restrict to operating generating units with at least 25MW capacity
  gen_border_dist %<>% .[i_operating == 1 & i_big == 1]
  # Restrict to coal, gas, hydro, solar, wind
  gen_border_dist %<>% .[fuel_cat_gen %in% c("COAL", "GAS", "HYDRO", "SOLAR", "WIND")]
  # New factor-based fuel category for plotting
  fuel_lvl = c("Coal", "Gas", "Hydro.", "Solar/Wind", "Uniform US Grid")
  gen_border_dist[, `:=`(
    fuel = fcase(
      fuel_cat_gen == "COAL", factor("Coal", levels = fuel_lvl, ordered = T),
      fuel_cat_gen == "GAS", factor("Gas", levels = fuel_lvl, ordered = T),
      fuel_cat_gen == "HYDRO", factor("Hydro.", levels = fuel_lvl, ordered = T),
      fuel_cat_gen %in% c("SOLAR", "WIND"), factor("Solar/Wind", levels = fuel_lvl, ordered = T)
    )
  )]
  # Bind together
  dist_dt = rbindlist(list(
    gen_border_dist[, .(fuel, dist_county = dist_plant_county, dist_state = dist_plant_state)],
    grid_border_dist[, .(
      fuel = factor("Uniform US Grid", levels = fuel_lvl, ordered = T),
      dist_county = dist_border_county,
      dist_state = dist_border_state
    )]
  ), use.names = T, fill = T)
  

# Function: K-S test ---------------------------------------------------------------------
  # To create K-S table
  ks_test = function(type, admin) {
    # K-S test
    ks_out = ks.test(
      x = dist_dt[fuel == type] %>% select(paste0("dist_", admin)) %>% unlist(),
      y = dist_dt[fuel == "Uniform US Grid"] %>% select(paste0("dist_", admin)) %>% unlist(),
      alternative = "two.sided",
      exact = F
    )
    # Return results in data table
    data.table(
      admin,
      fuel_type = type,
      test_stat = ks_out$statistic,
      p_value = if_else(ks_out$p.value == 0, "< 2.2e-16", as.character(ks_out$p.value))
    )
  }


# Run Kolmogorov-Smirnov tests -----------------------------------------------------------  
  # Create set of specifications
  set_df = expand_grid(
    admin = c("county", "state"),
    type = c("Coal", "Gas", "Hydro.", "Solar/Wind")
  )
  # Run in parallel
  ks_dt = mcmapply(
    FUN = ks_test,
    type = set_df$type,
    admin = set_df$admin,
    mc.cores = nrow(set_df),
    SIMPLIFY  = F
  ) %>% rbindlist(use.names = T, fill = T)
  # Round 
  ks_dt[, test_stat := test_stat %>% round(3)]
