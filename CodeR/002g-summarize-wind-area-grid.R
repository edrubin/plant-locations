

# Notes ----------------------------------------------------------------------------------
#   Goal: Summarize upwind and downwind area for every point in CONUS grid.


# Data notes -----------------------------------------------------------------------------


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, patchwork, latex2exp, huxtable,
    raster, data.table, collapse, fst, fixest,
    parallel, magrittr, here
  )


# Load data: Grid-based data table -------------------------------------------------------
  # Load the dataset
  full_dt = here(
    "DataClean", "GridWind", "Final", "us-grid.fst"
  ) %>%  read_fst(as.data.table = T)
  # full_dt = "~/Downloads/us-grid.fst" %>%  read_fst(as.data.table = T)


# Check data -----------------------------------------------------------------------------
  # Check if the four triangles' areas equal the state and county areas
  full_dt[, .(
    fips_state,
    st = (downwind_area_st+upwind_area_st+leftwind_area_st+rightwind_area_st)/state_area,
    co = (downwind_area_co+upwind_area_co+leftwind_area_co+rightwind_area_co)/county_area
  )] %>% summary()


# Data work: Summarize results -----------------------------------------------------------
  # Drop the one point that is missing upwind/downwind areas (appears to be outside US)
  full_dt %<>% .[!(is.na(upwind_area_co) | is.na(downwind_area_co))]
  # Share cells where area downwind is less than upwind: county, state, both
  full_dt[, .(
    "Share down < up, county" =
      fmean(downwind_area_co < upwind_area_co),
    "Share down < up, state" =
      fmean(downwind_area_st < upwind_area_st),
    "Share down < up, both" =
      fmean((downwind_area_co < upwind_area_co) & (downwind_area_st < upwind_area_st))
  )] 
  full_dt[, .(
    "Share down > up, county" =
      fmean(downwind_area_co > upwind_area_co),
    "Share down > up, state" =
      fmean(downwind_area_st > upwind_area_st),
    "Share down > up, both" =
      fmean((downwind_area_co > upwind_area_co) & (downwind_area_st > upwind_area_st))
  )] 
  # Means of downwind and upwind shares
  full_dt[, .(
    "Mean downwind share, county" = fmean(downwind_share_co),
    "Mean downwind share, state" = fmean(downwind_share_st),
    "Mean upwind share, county" = fmean(upwind_share_co),
    "Mean upwind share, state" = fmean(upwind_share_st)
  )] 
  # Does population density correlate with share of county/state that is down(up)wind?
  data.table(
    "Downwind share (co.)" = cor(full_dt$pop_density, full_dt$downwind_share_co),
    "Upwind share (co.)" = cor(full_dt$pop_density, full_dt$upwind_share_co),
    "Downwind share (st.)" = cor(full_dt$pop_density, full_dt$downwind_share_st),
    "Upwind share (st.)" = cor(full_dt$pop_density, full_dt$upwind_share_st)
  ) 
  # Does share Hispanic or non-White correlate with share of county/state that is down(up)wind?
  data.table(
    "Downwind share (co.)" =
      full_dt[, .(share_hnw, downwind_share_co)] %>% na.omit() %>% cor() %>% extract(1,2),
    "Upwind share (co.)" =
      full_dt[, .(share_hnw, upwind_share_co)] %>% na.omit() %>% cor() %>% extract(1,2),
    "Downwind share (st.)" =
      full_dt[, .(share_hnw, downwind_share_st)] %>% na.omit() %>% cor() %>% extract(1,2),
    "Upwind share (st.)" =
      full_dt[, .(share_hnw, upwind_share_st)] %>% na.omit() %>% cor() %>% extract(1,2)
  )
    # hux() %>%
    # set_caption("Correlation: Share non-white and wind") %>% 
    # set_bold(row = 1, col = everywhere) %>% 
    # set_top_border(row = 2, col = everywhere) %>% 
    # set_all_padding(4) %>% 
    # quick_html()
  # Regressions
  feols(log(pop_density) ~ downwind_share_st, data = full_dt, cluster = ~fips_state)
  feols(log(pop_density) ~ upwind_share_st, data = full_dt, cluster = ~fips_state)
  feols(log(pop_density) ~ downwind_share_co, data = full_dt, cluster = ~fips)
  feols(log(pop_density) ~ upwind_share_co, data = full_dt, cluster = ~fips)
  
  feols(share_hnw ~ downwind_share_st, data = full_dt, cluster = ~fips_state)
  feols(share_hnw ~ upwind_share_st, data = full_dt, cluster = ~fips_state)
  feols(share_hnw ~ downwind_share_co, data = full_dt, cluster = ~fips)
  feols(share_hnw ~ upwind_share_co, data = full_dt, cluster = ~fips)


# Figure: Share non-white and upwind/downwind area ---------------------------------------
  # Calculate correlations for each subfigure
  c1a = full_dt[, .(
    upwind_share_co, share_hnw
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c1b = full_dt[, .(
    downwind_share_co, share_hnw
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c1c = full_dt[, .(
    upwind_share_st, share_hnw
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c1d = full_dt[, .(
    downwind_share_st, share_hnw
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  # The plot(s)
  p1a = ggplot(
    data = full_dt,
    aes(x = upwind_share_co, y = share_hnw)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of county upwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c1a))
  p1b = ggplot(
    data = full_dt,
    aes(x = downwind_share_co, y = share_hnw)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of county downwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c1b))
  p1c = ggplot(
    data = full_dt,
    aes(x = upwind_share_st, y = share_hnw)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of state upwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c1c))
  p1d = ggplot(
    data = full_dt,
    aes(x = downwind_share_st, y = share_hnw)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of state downwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c1d))
  p1 = (
    (p1a + p1b) / (p1c + p1d)
  ) + plot_annotation(
    tag_levels = 'A',
    tag_suffix = '.'
  ) &
  scale_y_continuous("Share Hispanic or Non-White", labels = scales::percent) &
  scale_fill_gradientn(
    "Count",
    colors = c("#FCFDEF", viridis::magma(1e3, direction = -1)),
    labels = scales::comma,
    breaks = c(5e5, 1e6)
  ) &
  theme_minimal(base_size = 8, base_family = "Merriweather") &
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 10)) &
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.text = element_text(size = 6),
    plot.tag = element_text(size = 8, family = "Merriweather")
  )
  ggsave(
    plot = p1,
    path = here("Figures", "GridArea"),
    filename = "grid-area-hnw.pdf",
    device = cairo_pdf,
    height = 7,
    width = 6.5
  )


# Figure: Population density and upwind/downwind area ------------------------------------
  # Calculate correlations for each subfigure
  c2a = full_dt[, .(
    upwind_share_co, pop_density
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c2b = full_dt[, .(
    downwind_share_co, pop_density
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c2c = full_dt[, .(
    upwind_share_st, pop_density
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c2d = full_dt[, .(
    downwind_share_st, pop_density
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  # The figure(s)
  p2a = ggplot(
    data = full_dt,
    aes(x = upwind_share_co, y = pop_density)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of county upwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c2a))
  p2b = ggplot(
    data = full_dt,
    aes(x = downwind_share_co, y = pop_density)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of county downwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c2b))
  p2c = ggplot(
    data = full_dt,
    aes(x = upwind_share_st, y = pop_density)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of state upwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c2c))
  p2d = ggplot(
    data = full_dt,
    aes(x = downwind_share_st, y = pop_density)
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of state downwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c2d))
  p2 = (
    (p2a + p2b) / (p2c + p2d)
  ) + plot_annotation(
    tag_levels = 'A',
    tag_suffix = '.'
  ) &
  scale_y_continuous("Population density", labels = scales::comma) &
  scale_fill_gradientn(
    "Count",
    colors = c("#FCFDEF", viridis::magma(1e3, direction = -1)),
    labels = scales::comma,
    breaks = c(1e6, 3e6)
  ) &
  theme_minimal(base_size = 8, base_family = "Merriweather") &
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 10)) &
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.text = element_text(size = 6),
    plot.tag = element_text(size = 8, family = "Merriweather")
  )
  ggsave(
    plot = p2,
    path = here("Figures", "GridArea"),
    filename = "grid-area-pop-density.pdf",
    device = cairo_pdf,
    height = 7,
    width = 6.5
  )


# Figure: Log population density and upwind/downwind area --------------------------------
  # Calculate correlations for each subfigure
  c3a = full_dt[pop_density > 0, .(
    upwind_share_co, log10(pop_density)
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c3b = full_dt[pop_density > 0, .(
    downwind_share_co, log10(pop_density)
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c3c = full_dt[pop_density > 0, .(
    upwind_share_st, log10(pop_density)
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  c3d = full_dt[pop_density > 0, .(
    downwind_share_st, log10(pop_density)
  )] %>% na.omit() %>% coop::pcor() %>% extract(1,2) %>% format(digits = 2, nsmall = 3)
  # The figure(s)
  p3a = ggplot(
    data = full_dt,
    aes(x = upwind_share_co, y = log10(pop_density))
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of county upwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c3a))
  p3b = ggplot(
    data = full_dt,
    aes(x = downwind_share_co, y = log10(pop_density))
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of county downwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c3b))
  p3c = ggplot(
    data = full_dt,
    aes(x = upwind_share_st, y = log10(pop_density))
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of state upwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c3c))
  p3d = ggplot(
    data = full_dt,
    aes(x = downwind_share_st, y = log10(pop_density))
  ) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept = 0, size = 1/4) +
  geom_hline(yintercept = 0, size = 1/4) +
  scale_x_continuous("Share of state downwind", labels = scales::percent) +
  labs(subtitle = paste0("Correlation: ", c3d))
  p3 = (
    (p3a + p3b) / (p3c + p3d)
  ) + plot_annotation(
    tag_levels = 'A',
    tag_suffix = '.'
  ) &
  scale_y_continuous(TeX("Log$_{10}$(Population density)"), labels = scales::comma) &
  scale_fill_gradientn(
    "Count",
    colors = c("#FCFDEF", viridis::magma(1e3, direction = -1)),
    labels = scales::comma,
    breaks = c(5e5, 1e6)
  ) &
  theme_minimal(base_size = 8, base_family = "Merriweather") &
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 10)) &
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.text = element_text(size = 6.5),
    plot.tag = element_text(size = 8, family = "Merriweather")
  )
  ggsave(
    plot = p3,
    path = here("Figures", "GridArea"),
    filename = "grid-area-log-pop-density.pdf",
    device = cairo_pdf,
    height = 7,
    width = 6.5
  )