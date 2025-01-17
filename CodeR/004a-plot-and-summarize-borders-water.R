
# Notes ----------------------------------------------------------------------------------
#   Goals:
#     1. Plot counties' borders as water/non-water
#     2. Summarize each state's state and county borders as share water
#   Time: About 1.5 minutes

# Setup ----------------------------------------------------------------------------------
  # load data and packages
  library(pacman)
  p_load(
    tidyverse, rmapshaper, sf,
    viridis, patchwork,
    data.table, fst, parallel,
    magrittr, here
  )

# Set directories ------------------------------------------------------------------------
  # Directory with county borders
  dir_borders = here("DataClean", "BordersAndWater")

# Load data: County and state shapes -----------------------------------------------------
  # Load county shape file
  county_sf = here(
    "DataRaw", "Census", "tl_2016_us_county", "tl_2016_us_county.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP, COUNTYFP, GEOID",
      "FROM \"tl_2016_us_county\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  )
  # Load county shape file
  state_sf = here(
    "DataRaw", "Census", "tl_2016_us_state", "tl_2016_us_state.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT STATEFP", 
      "FROM \"tl_2016_us_state\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  )

# Function: Find UTM zone from lat/lon ---------------------------------------------------
  # Source: https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
  lonlat2UTM = function(lonlat) {
    utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if(lonlat[2] > 0) {
      utm + 32600
    } else{
      utm + 32700
    }
  }

# Summarize states' borders --------------------------------------------------------------
  # Iterate over states
  state_dt = mclapply(
    mc.cores = 32,
    mc.preschedule = F,
    X = state_sf$STATEFP %>% unique(),
    FUN = function(st) {
    # Find files
    water_files = dir_borders %>% dir(full.names = T)
    # Read in the files
    w_dt = lapply(
      X = water_files %>% str_subset(paste0("border", st)),
      FUN = . %>% readRDS() %T>% setDT()
    ) %>% rbindlist()
    # Convert to 'near water' points back to 'sf' (and force uniqueness)
    w_sf = w_dt[i_near_water == T, .(geometry, i_near_water)] %>% st_as_sf()
    # Find state centroids' UTM zone
    st_utm = filter(state_sf, STATEFP == st) %>%
      st_centroid() %>%
      st_coordinates() %>%
      as.numeric() %>%
      lonlat2UTM()
    # Grab state border, cast as line (not polygon), and transform to UTM
    state_line = filter(state_sf, STATEFP == st) %>%
      st_cast("MULTILINESTRING") %>% 
      st_transform(crs = st_utm)
    # Find counties in state 'st' that touch its border
    st_co = filter(county_sf, STATEFP == st) %>%
      select(fips = GEOID) %>%
      st_transform(crs = st_utm)
    border_counties = st_touches(st_co, state_line, sparse = F)
    border_counties = st_co[border_counties,]
    # Find which points are on the border
    points_sf = w_dt[fips %in% border_counties$fips] %>%
      st_as_sf() %>% 
      st_transform(crs = st_utm)
    test_border = st_intersects(
      points_sf,
      state_line %>% st_buffer(dist = units::set_units(100, "m")),
      sparse = F
    )
    # Finally we can find the share of the state border that is water
    share_water_state_border = points_sf[test_border[,1],] %$% i_near_water %>% mean()
    # And its length
    length_state_border = state_line %>% st_length() %>% units::set_units("km")
    # State-level summary statistics
    data.table(
      state_fips = st,
      share_water_state_border = share_water_state_border,
      share_water_county_border = w_dt[,i_near_water] %>% mean(),
      length_state_border = length_state_border,
      length_county_border = county_sf %>% filter(STATEFP == st) %>%
        st_length() %>% units::set_units("km") %>% sum()
    )
  }) %>% rbindlist(use.names = T, fill = T)
  # Take length-weighted means to get national averages
  state_dt[, .(
    share_water_county_borders = weighted.mean(
      x = share_water_county_border,
      w = length_county_border
    ),
    share_water_state_borders = weighted.mean(
      x = share_water_state_border,
      w = length_state_border
    )
  )]

# Save summary table ---------------------------------------------------------------------
  # Save CSV
  fwrite(
    x = state_dt,
    file = here("DataClean", "BorderWaterSummary", "state-border-water-summary.csv")
  )
# Figure: Water-border calculations ------------------------------------------------------
  # States to plot: LA, OR, SC, SD
  # to_plot = state_sf$STATEFP %>% unique(),
  to_plot = c(22, 41, 45, 46) %>% as.character()
  # Iterate over states
  blah = mclapply(
    mc.cores = length(to_plot),
    mc.preschedule = F,
    X = to_plot,
    FUN = function(st) {
    # Find files
    water_files = dir_borders %>% dir(full.names = T)
    # Read in the files
    w_dt = lapply(
      X = water_files %>% str_subset(paste0("border", st)),
      FUN = . %>% readRDS() %T>% setDT()
    ) %>% rbindlist()
    # Convert to 'near water' points back to 'sf' (and force uniqueness)
    w_sf = w_dt[i_near_water == T, .(geometry, i_near_water)] %>% st_as_sf()
    # Plot
    plot_state = ggplot() +
    geom_sf(
      data = county_sf %>% filter(STATEFP == st),
      size = 0.20,
      color = "grey60",
      fill = "grey98"
    ) +
    geom_sf(
      data = w_sf,
      color = "#00008b",
      size = 0.5
    ) +
    theme_void()
    # Save the figure
    ggsave(
      plot = plot_state,
      path = here("Figures", "BordersAndWater"),
      filename = paste0("borders-water-", st, ".pdf"),
      width = 10,
      height = 10
    )
  }) %>% rbindlist(use.names = T, fill = T)

# Crop figures and convert to PNG --------------------------------------------------------
  # Crop the PDFs
  for (
    i in here("Figures", "BordersAndWater") %>%
      dir(full.names = T, pattern = "22|41|45|46")
  ) {
    system2(command = "pdfcrop", args = c(i, i))
  }
  # PDFs to PNGs
  for (
    i in here("Figures", "BordersAndWater") %>%
      dir(full.names = T, pattern = "22|41|45|46")
  ) {
    system(command = paste(
      "pdftoppm",
      i,
      str_remove(i, "\\.pdf"),
      "-png -rx 200 -ry 200"
    ))
  }
  # Fix names
  file.rename(
    from = here("Figures", "BordersAndWater") %>%
      dir(full.names = T),
    to = here("Figures", "BordersAndWater") %>%
      dir(full.names = T) %>%
      str_replace_all("-1.png", ".png")
  )
