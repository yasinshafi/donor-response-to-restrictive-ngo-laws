####################################################################################
# 04_vertex_attributes.R
# Distributions of vertex (node) attributes for the two modes of the network.
#
# Figure V1 (donors, n = 19):
#   (a) democracy aid as share of total aid, 2005-2019 (basis of democracy orientation)
#   (b) mean annual total aid commitments (USD millions)
#   (c) degree in the 2014 democracy-aid network (number of recipients funded)
# Figure V2 (recipients, 2014 cross-section, paper's analysis sample):
#   (a) log GDP per capita   (b) log population
#   (c) V-Dem liberal democracy index   (d) V-Dem CSO repression (higher = less repression)
#   (e) year of first restrictive NGO law (treated recipients only)
#   (f) degree in the 2014 democracy-aid network (number of donors)
#
# Input : output/network_grid.rds (02_build_network.R)
# Output: output/figures/fig_V1_donor_attributes.{pdf,png}
#         output/figures/fig_V2_recipient_attributes.{pdf,png}
####################################################################################

library(tidyverse)
library(patchwork)

grid <- readRDS(file.path("output", "network_grid.rds"))
y0   <- 2014
fill <- "#00796B"
th   <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())

hist_panel <- function(d, var, title, xlab, bins = 15) {
  ggplot(d, aes(.data[[var]])) +
    geom_histogram(bins = bins, fill = fill, color = "white", linewidth = 0.2) +
    labs(title = title, x = xlab, y = "Count") + th
}

# ---- Donors -------------------------------------------------------------------------------
donors <- grid %>%
  group_by(d_iso) %>%
  summarize(dem_share  = sum(commit_dem, na.rm = TRUE) / sum(commit_tot, na.rm = TRUE),
            mean_total = sum(commit_tot, na.rm = TRUE) / n_distinct(year) / 1e6,
            degree     = sum(tie_dem[year == y0]),
            .groups = "drop")

V1 <- hist_panel(donors, "dem_share",  "(a) Democracy-aid share, 2005-19", "Share of total aid", 10) +
      hist_panel(donors, "mean_total", "(b) Mean annual total aid",        "USD millions", 10) +
      hist_panel(donors, "degree",     paste0("(c) Degree, democracy aid ", y0), "Recipients funded", 10) +
      plot_annotation(title = "Donor attributes (n = 19)")

ggsave(file.path("output", "figures", "fig_V1_donor_attributes.pdf"), V1, width = 10, height = 3.4)
ggsave(file.path("output", "figures", "fig_V1_donor_attributes.png"), V1, width = 10, height = 3.4, dpi = 200)

# ---- Recipients -----------------------------------------------------------------------------
recips <- grid %>%
  filter(year == y0) %>%
  group_by(r_iso) %>%
  summarize(r_gdppc.ln = first(r_gdppc.ln), r_pop.ln = first(r_pop.ln),
            v2x_libdem = first(v2x_libdem), v2csreprss = first(v2csreprss),
            first_law  = first(first_law),  degree = sum(tie_dem),
            .groups = "drop")

V2 <- (hist_panel(recips, "r_gdppc.ln", "(a) GDP per capita",        "log USD") +
       hist_panel(recips, "r_pop.ln",   "(b) Population",            "log persons") +
       hist_panel(recips, "v2x_libdem", "(c) Liberal democracy",     "V-Dem index (0-1)")) /
      (hist_panel(recips, "v2csreprss", "(d) CSO repression",        "V-Dem (higher = less repression)") +
       ggplot(filter(recips, !is.na(first_law)), aes(first_law)) +
         geom_bar(fill = "#C0392B") + scale_x_continuous(breaks = seq(2005, 2019, 2)) +
         labs(title = "(e) Year of first NGO law", x = "Year", y = "Count") + th +
       hist_panel(recips, "degree",     paste0("(f) Degree, democracy aid ", y0), "Donors funding")) +
      plot_annotation(title = sprintf("Recipient attributes, %d (n = %d; NGO-law panel n = %d)",
                                      y0, nrow(recips), sum(!is.na(recips$first_law))))

ggsave(file.path("output", "figures", "fig_V2_recipient_attributes.pdf"), V2, width = 10, height = 6)
ggsave(file.path("output", "figures", "fig_V2_recipient_attributes.png"), V2, width = 10, height = 6, dpi = 200)
