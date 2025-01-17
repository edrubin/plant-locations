

# Notes ----------------------------------------------------------------------------------
#   Goals: 
#     Aggregate wind area data for every point in CONUS grid.
#   Time:

# Data notes -----------------------------------------------------------------------------


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, janitor, data.table, fst, vroom,
    raster, sf, fasterize, units, geosphere, fixest,
    parallel, magrittr, here
  )


# Load data: Raster-cell areas -----------------------------------------------------------
  # Load the area raster
  area_r = here("DataRaw", "Census", "usgrid_data_2010", "usarea10.tif") %>% raster()
  # Convert to data table
  area_dt = area_r %>% rasterToPoints(spatial = F) %>% as.data.table()
  # Update names
  setnames(area_dt, c("lon", "lat", "area"))
  # Key
  setkey(area_dt, lon, lat)


# Load data: Process upwind/downwind grid ------------------------------------------------
  # Find the files
  grid_files = here(
    "DataClean", "GridWind", "County"
  ) %>% dir(
    pattern = "grid-downwind-upwind-tl-[0-9]{5}.fst",
    full.names = T
  )
  # Load the files
  full_dt = mclapply(
    X = grid_files,
    FUN = read_fst,
    as.data.table = T,
    mc.cores = 64
  ) %>% rbindlist(use.names = T, fill = T)


# Data work: Merge processed and area datasets -------------------------------------------
  # Key the 'full' dataset
  setkey(full_dt, lon, lat)
  # Merge
  full_dt %<>% merge(
    y = area_dt,
    by = c("lon", "lat"),
    all.x = T,
    all.y = F
  )
  # Calculate population density
  full_dt[, `:=`(
    pop_density = pop / area,
    pop_nhw_density = pop_nhw / area
  )]
  # Fill 'fully above water' NAs with 0s for population density
  full_dt[flag_water == T, `:=`(
    pop_density = 0,
    pop_nhw_density = 0
  )]
  # Calculate share of population that is Hispanic or non-white
  full_dt[pop > 0, `:=`(
    share_hnw = (pop - pop_nhw) / pop
  )]
  # Create variables: share downwind and upwind
  full_dt[, `:=`(
    downwind_share_co = downwind_area_co / county_area,
    downwind_share_st = downwind_area_st / state_area,
    upwind_share_co = upwind_area_co / county_area,
    upwind_share_st = upwind_area_st / state_area,
    leftwind_share_co = leftwind_area_co / county_area,
    leftwind_share_st = leftwind_area_st / state_area,
    rightwind_share_co = rightwind_area_co / county_area,
    rightwind_share_st = rightwind_area_st / state_area
  )]
  # Add state fips
  full_dt[, `:=`(fips_state = str_sub(fips, 1, 2))]
  # Save the dataset
  write_fst(
    x = full_dt,
    path = here("DataClean", "GridWind", "Final", "us-grid.fst"),
    compress = 100
  )
  # # Create raster of final dataset
  # final_r = full_dt[, .(
  #   lon, lat,
  #   wind_angle_all_cw = 360 - wind_angle_all_ccw,
  #   upwind_share_co,
  #   downwind_share_co,
  #   leftwind_share_co,
  #   rightwind_share_co,
  #   upwind_share_st,
  #   downwind_share_st,
  #   leftwind_share_st,
  #   rightwind_share_st,
  #   diff_share_co = upwind_share_co - downwind_share_co,
  #   diff_share_st = upwind_share_st - downwind_share_st
  # )] %>% rasterFromXYZ(crs = crs(area_r))
  # # Save the raster
  # writeRaster(
  #   x = final_r,
  #   filename = here("DataClean", "GridWind", "Final", "us-grid-brick.grd"),
  #   format = "raster", 
  #   overwrite = T,
  #   options=c("INTERLEAVE=BAND","COMPRESS=LZW")
  # )

