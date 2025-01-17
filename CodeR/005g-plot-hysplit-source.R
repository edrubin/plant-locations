

# Notes ----------------------------------------------------------------------------------
#   Goal: Illustrate share of emissions out of state/county by hour and season.
#   Direct dependencies:
#     - 005c-summarize-hysplit-emissions-source.R


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    extrafont, tidyverse, latex2exp, scales, patchwork,
    data.table, lubridate, fst,
    magrittr, here
  )


# Load data: HYPSLIT source --------------------------------------------------------------
  # Load source-based HYSPLIT summary
  source_dt = here(
    "DataClean", "hysplit-completed", "final", "hysplit-source-departures.fst"
  ) %>% read_fst(as.data.table = T)
  # Load source_based HYSPLIT summary at plant level
  source_plant = here(
    "DataClean", "hysplit-completed", "final", "hysplit-source-departures-plant.fst"
  ) %>% read_fst(as.data.table = T)
  # Add hour 0
  source_dt = rbindlist(list(
    source_dt,
    data.table(
      mo = c(1,7),
      hour = c(0,0),
      pct_out_county_so2 = 0,
      pct_out_county_nox = 0,
      pct_out_state_so2 = 0,
      pct_out_state_nox = 0
    )
  ))
  source_plant = rbindlist(list(
    source_plant,
    data.table(
      mo = c(1,7),
      hour = c(0,0),
      oris = source_plant[,oris] %>% unique(),
      pct_out_county_so2 = 0,
      pct_out_county_nox = 0,
      pct_out_state_so2 = 0,
      pct_out_state_nox = 0
    )
  ))
  # Month to factor
  source_dt[, mo := factor(mo, levels = c(1,7), labels = c("January", "July"))]
  source_plant[, mo := factor(mo, levels = c(1,7), labels = c("January", "July"))]


# Figure: Percent out of county, SO2 -----------------------------------------------------
  p1 = ggplot(
    data = source_dt,
    aes(x = hour, y = pct_out_county_so2, color = mo)
  ) +
  geom_line(
    data = source_plant,
    aes(x = hour, y = pct_out_county_so2, color = mo, group = interaction(oris, mo)),
    size = 0.05,
    alpha = 0.10
  ) +
  geom_point(size = 0.9) +
  geom_line(size = 0.65) +
  ggtitle(
    TeX("\\textbf{Panel A:} Percent of emissions outside of source's \\textit{county}—by hours since emission"),
    TeX("Weighted across plants by mass of SO$_2$ emissions")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  scale_x_continuous("Hours since emission", breaks = c(0, 12, 24, 36, 48)) +
  scale_color_viridis_d("Month of operation", option = "magma", end = 0.7) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  guides(color = "none")


# Figure: Percent out of county, NOx -----------------------------------------------------
  p2 = ggplot(
    data = source_dt,
    aes(x = hour, y = pct_out_county_nox, color = mo)
  ) +
  geom_line(
    data = source_plant,
    aes(x = hour, y = pct_out_county_nox, color = mo, group = interaction(oris, mo)),
    size = 0.05,
    alpha = 0.10
  ) +
  geom_point(size = 0.8) +
  geom_line(size = 0.65) +
  ggtitle(
    "",
    TeX("Weighted across plants by mass of NO$_{x}$ emissions")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  scale_x_continuous("Hours since emission", breaks = c(0, 12, 24, 36, 48)) +
  scale_color_viridis_d("Month of operation", option = "magma", end = 0.7) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  guides(color = "none")


# Figure: Percent out of state, SO2 ------------------------------------------------------
  p3 = ggplot(
    data = source_dt,
    aes(x = hour, y = pct_out_state_so2, color = mo)
  ) +
  geom_line(
    data = source_plant,
    aes(x = hour, y = pct_out_state_so2, color = mo, group = interaction(oris, mo)),
    size = 0.05,
    alpha = 0.10
  ) +
  geom_point(size = 0.8) +
  geom_line(size = 0.65) +
  ggtitle(
    TeX("\\textbf{Panel B:} Percent of emissions outside of source's \\textit{state}—by hours since emission"),
    TeX("Weighted across plants by mass of SO$_2$ emissions")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  scale_x_continuous("Hours since emission", breaks = c(0, 12, 24, 36, 48)) +
  scale_color_viridis_d("Month of operation", option = "magma", end = 0.7) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  guides(color = "none")


# Figure: Percent out of state, NOx ------------------------------------------------------
  p4 = ggplot(
    data = source_dt,
    aes(x = hour, y = pct_out_state_nox, color = mo)
  ) +
  geom_line(
    data = source_plant,
    aes(x = hour, y = pct_out_state_nox, color = mo, group = interaction(oris, mo)),
    size = 0.05,
    alpha = 0.10
  ) +
  geom_point(size = 0.8) +
  geom_line(size = 0.65) +
  ggtitle(
    "",
    TeX("Weighted across plants by mass of NO$_{x}$ emissions")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  scale_x_continuous("Hours since emission", breaks = c(0, 12, 24, 36, 48)) +
  scale_color_viridis_d("Month of operation", option = "magma", end = 0.7) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(legend.position = "bottom")


# Figure: Joint plot ---------------------------------------------------------------------
  # Combine plots
  final_plot = (p1 | p2) / (p3 | p4) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
  # Save the final plot
  ggsave(
    plot = final_plot,
    path = here("Figures", "hysplit"),
    filename = "hysplit-sources.pdf",
    device = cairo_pdf,
    height = 7.5,
    width = 6.5
  )
