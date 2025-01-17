
# Notes ----------------------------------------------------------------------------------
#   Goal: Create tables of the nonattainment counties with the
#         largest shares of external NOx.

# Setup ----------------------------------------------------------------------------------
  # Package
  library(pacman)
  p_load(stringr, scales, fst, data.table, xtable, magrittr, here)

# Load data: FIPS xwalk ------------------------------------------------------------------
  # Load
  fips_dt = here(
    "DataClean", "Maps", "fips-name-xwalk-2018.csv"
  ) %>% fread() %>% janitor::clean_names()
  # Grab desired columns (and reformat)
  fips_dt %<>% .[, .(
    name_co = county_name %>% stringr::str_to_title(),
    name_cbsa = cbsa_name,
    abb_st = state,
    fips = fips_county_code %>% stringr::str_pad(5, "left", 0)
  )]

# Load data: HYSPLIT ---------------------------------------------------------------------
  # Load data
  hy_dt = read_fst(
    path = here(
      "DataClean", "hysplit-completed", "final",
      "hysplit-destination.fst"
    ),
    as.data.table = T
  )

# Data work: Top 'N' counties for external emissions -------------------------------------
  # Find sources
  co_dt = hy_dt[, .(
    nox_total = sum(mass_nox),
    nox_own = sum((own_county == 1) * mass_nox),
    nox_attn_others = sum((source_nonattainment == 0) * (own_county == 0) * mass_nox),
    nox_nonattn_others = sum((source_nonattainment == 1) * (own_county == 0) * mass_nox),
    nox_attn_same_state = sum((source_nonattainment == 0) * (own_county == 0) * (own_state == 1) * mass_nox),
    nox_nonattn_same_state = sum((source_nonattainment == 1) * (own_county == 0) * (own_state == 1) * mass_nox),
    nox_attn_other_state = sum((source_nonattainment == 0) * (own_county == 0) * (own_state == 0) * mass_nox),
    nox_nonattn_other_state = sum((source_nonattainment == 1) * (own_county == 0) * (own_state == 0) * mass_nox),
    nonattainment
  ), by = .(county, mo)] %>% unique()
  # Restrict to counties operating coal plants in the given month
  co_dt %<>% .[nox_own > 0]
  # Calculate shares
  share_dt = co_dt[, .(
    mo,
    fips = county,
    nox_own,
    nox_total,
    share_nox_own = nox_own / nox_total,
    share_nox_attn_same_state = nox_attn_same_state / nox_total,
    share_nox_nonattn_same_state = nox_nonattn_same_state / nox_total,
    share_nox_attn_other_state = nox_attn_other_state / nox_total,
    share_nox_nonattn_other_state = nox_nonattn_other_state / nox_total,
    share_nox_attn_others = nox_attn_others / nox_total,
    share_nox_nonattn_others = nox_nonattn_others / nox_total,
    nonattainment
  )]
  # Join county names
  share_dt %<>% merge(
    x = fips_dt,
    y = .,
    by = "fips",
    all.x = F,
    all.y = T,
    sort = F
  )
  # Orrder by share coming from other sources IN ATTAINMENT
  setorder(share_dt, -share_nox_attn_others)
  # Create 'top-N' tables for January and July (separately) FOR NONATTAINMENT COUNTIES
  jan_table = share_dt[mo == 1 & nonattainment == 1][1:15][, .(
    County = paste(name_co, abb_st, sep = ", "),
    CBSA = name_cbsa,
    Own = share_nox_own %>% percent(0.1),
    `Same State, Attn.` = share_nox_attn_same_state %>% percent(0.1),
    `Same State, Non.` = share_nox_nonattn_same_state %>% percent(0.1),
    `Other State, Attn.` = share_nox_attn_other_state %>% percent(0.1),
    `Other State, Non.` = share_nox_nonattn_other_state %>% percent(0.1)
  )] %>% xtable()
  jul_table = share_dt[mo == 7 & nonattainment == 1][1:15][, .(
    County = paste(name_co, abb_st, sep = ", "),
    CBSA = name_cbsa,
    Own = share_nox_own %>% percent(0.1),
    `Same State, Attn.` = share_nox_attn_same_state %>% percent(0.1),
    `Same State, Non.` = share_nox_nonattn_same_state %>% percent(0.1),
    `Other State, Attn.` = share_nox_attn_other_state %>% percent(0.1),
    `Other State, Non.` = share_nox_nonattn_other_state %>% percent(0.1)
  )] %>% xtable()