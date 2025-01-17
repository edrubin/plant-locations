
# Notes ----------------------------------------------------------------------------------
#   Goal: Illustrate share of counties' emissions by source locations.
#   Direct dependencies:
#     - 005d-summarize-hysplit-emissions-destination.R

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
  grp_dt = here(
    "DataClean", "hysplit-completed", "final", "hysplit-destination-grp.fst"
  ) %>% read_fst(as.data.table = T)
  sub_dt = here(
    "DataClean", "hysplit-completed", "final", "hysplit-destination-sub.fst"
  ) %>% read_fst(as.data.table = T)
  # Month to factor
  grp_dt[, mo := factor(mo, levels = c(1,7), labels = c("January", "July"))]
  sub_dt[, mo := factor(mo, levels = c(1,7), labels = c("January", "July"))]
  # Status factor
  grp_dt[, status := factor(
    nonattainment,
    levels = c(0,1),
    labels = c("Attainment counties", "Non-attainment counties")
  )]
  sub_dt[, status := factor(
    nonattainment,
    levels = c(0,1),
    labels = c("Attainment counties", "Non-attainment counties")
  )]

# Figure: Source locations, SO2 ----------------------------------------------------------
  p1 = ggplot(
    data = grp_dt,
    aes(x = mo, y = share_so2, fill = em_grp)
  ) +
  geom_col() +
  geom_hline(yintercept = 0, size = 1/4) +
  ggtitle(
    "Sources of local coal-based particles",
    TeX("Weighted by mass of SO$_2$ emissions")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  xlab("") +
  scale_fill_viridis_d(
    "Location of source of emissions",
    option = "magma",
    end = 0.9,
  ) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(cols = vars(status))

# Figure: Source locations, NOx ----------------------------------------------------------
  p2 = ggplot(
    data = grp_dt,
    aes(x = mo, y = share_nox, fill = em_grp)
  ) +
  geom_col() +
  geom_hline(yintercept = 0, size = 1/4) +
  ggtitle(
    "Sources of local coal-based particles",
    TeX("Weighted by mass of NO$_x$ emissions")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  xlab("") +
  scale_fill_viridis_d(
    "Location of source of emissions",
    option = "magma",
    end = 0.9,
  ) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(cols = vars(status))

# Figure: Source locations and attainment status, SO2 ------------------------------------
  p3 = ggplot(
    data = sub_dt,
    aes(x = mo, y = share_so2, fill = em_grp_sub)
  ) +
  geom_col() +
  geom_hline(yintercept = 0, size = 1/4) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel A:} Sources of local coal-based particles, ",
      "weighted by mass of \\textbf{SO$_2$} emissions"
    )),
    TeX("Coal-fueled units in 2005 with capacity greater than 25 MW")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  xlab("") +
  scale_fill_viridis_d(
    "Location of emissions' source",
    option = "magma",
    end = 0.9,
  ) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 2, ncol = 3, byrow = T)) +
  facet_grid(cols = vars(status))

# Figure: Source locations and attainment status, NOx ------------------------------------
  p4 = ggplot(
    data = sub_dt,
    aes(x = mo, y = share_nox, fill = em_grp_sub)
  ) +
  geom_col() +
  geom_hline(yintercept = 0, size = 1/4) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel B:} Sources of local coal-based particles, ",
      "weighted by mass of \\textbf{NO$_x$} emissions"
    )),
    TeX("Coal-fueled units in 2005 with capacity greater than 25 MW")
  ) +
  scale_y_continuous("", labels = percent, limits = c(0,1)) +
  xlab("") +
  scale_fill_viridis_d(
    "Location of emissions' source",
    option = "magma",
    end = 0.9,
  ) +
  theme_minimal(base_size = 8, base_family = "Merriweather") +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 2, ncol = 3, byrow = T)) +
  facet_grid(cols = vars(status))

# Figure: Joint plot ---------------------------------------------------------------------
  # Combine plots
  final_plot = p3 / p4 + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
  # Save the final plot
  ggsave(
    plot = final_plot,
    path = here("Figures", "hysplit"),
    filename = "hysplit-destinations.pdf",
    device = cairo_pdf,
    height = 7.5,
    width = 6.5
  )
