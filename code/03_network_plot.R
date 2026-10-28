####################################################################################
# 03_network_plot.R
# Plots of the network studied in the extension.
#
# Figure N1: full democracy-aid network, 2014 vs 2019, identical layout in both panels.
#            Donors (inner ring, ordered by democracy-aid share) and recipients (outer
#            ring, grouped by region) never move, so differences are changes in ties.
# Figure N2: South Asia sub-network, 2014 vs 2019 (recipients labelled with law year).
#
# Input : output/network_grid.rds (02_build_network.R)
# Output: output/figures/fig_N1_network_2014_2019.{pdf,png}
#         output/figures/fig_N2_south_asia_2014_2019.{pdf,png}
####################################################################################

library(tidyverse)
library(igraph)

grid <- readRDS(file.path("output", "network_grid.rds"))
yrs  <- c(2014, 2019)
col_do <- "#00796B"; col_non <- "#80CBC4"; col_post <- "#C0392B"; col_pre <- "#BDBDBD"

# ---- Figure N1: full network ---------------------------------------------------------
d_ord <- grid %>% distinct(d_iso, dem_share) %>% arrange(desc(dem_share))
r_ord <- grid %>% filter(year %in% yrs) %>% distinct(r_iso, region, first_law) %>%
  arrange(region, first_law, r_iso)
nd <- nrow(d_ord); nr <- nrow(r_ord)

a_d <- seq(0, 2 * pi, length.out = nd + 1)[-(nd + 1)]
a_r <- seq(0, 2 * pi, length.out = nr + 1)[-(nr + 1)]
lay <- rbind(0.38 * cbind(cos(a_d), sin(a_d)), cbind(cos(a_r), sin(a_r)))

plot_full <- function(y) {
  g_y <- grid %>% filter(year == y)
  e   <- g_y %>% filter(tie_dem == 1, r_iso %in% r_ord$r_iso)
  g   <- make_empty_graph(nd + nr, directed = FALSE)
  V(g)$name <- c(d_ord$d_iso, r_ord$r_iso)
  g <- add_edges(g, as.vector(rbind(e$d_iso, e$r_iso)))

  DO_y   <- g_y %>% distinct(d_iso, DO)
  d_DO   <- DO_y$DO[match(d_ord$d_iso, DO_y$d_iso)]
  r_post <- g_y %>% distinct(r_iso, post_law) %>% { .$post_law[match(r_ord$r_iso, .$r_iso)] }
  r_post[is.na(r_post)] <- 0
  e_DO   <- DO_y$DO[match(e$d_iso, DO_y$d_iso)]

  plot(g, layout = lay, rescale = FALSE, xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15),
       vertex.shape = c(rep("square", nd), rep("circle", nr)),
       vertex.size  = c(rep(5.5, nd), rep(2.6, nr)),
       vertex.color = c(ifelse(d_DO == 1, col_do, col_non), ifelse(r_post == 1, col_post, col_pre)),
       vertex.frame.color = NA,
       vertex.label = c(d_ord$d_iso, r_ord$r_iso),
       vertex.label.cex = c(rep(0.45, nd), rep(0.32, nr)),
       vertex.label.color = c(rep("white", nd), rep("grey25", nr)),
       vertex.label.dist = c(rep(0, nd), rep(0.9, nr)),
       vertex.label.degree = c(rep(0, nd), -a_r),
       edge.color = ifelse(e_DO == 1, adjustcolor(col_do, 0.16), adjustcolor(col_non, 0.14)),
       edge.width = 0.5,
       main = sprintf("%d: %d democracy-aid ties, density %.2f\n%d of %d recipients post-law",
                      y, nrow(e), nrow(e) / (nd * nr), sum(r_post), nr))
}

draw_N1 <- function() {
  par(mfrow = c(1, 2), mar = c(0.5, 0.5, 3, 0.5))
  for (y in yrs) plot_full(y)
  legend("bottomright", bty = "n", cex = 0.6, pch = c(15, 15, 19, 19), pt.cex = 1.1,
         col = c(col_do, col_non, col_post, col_pre),
         legend = c("Donor, democracy-oriented (DOD)", "Donor, other",
                    "Recipient, after NGO law", "Recipient, no law yet"))
}
pdf(file.path("output", "figures", "fig_N1_network_2014_2019.pdf"), width = 12, height = 6.3); draw_N1(); dev.off()
png(file.path("output", "figures", "fig_N1_network_2014_2019.png"), width = 12, height = 6.3, units = "in", res = 200); draw_N1(); dev.off()

# ---- Figure N2: South Asia sub-network ------------------------------------------------
sa   <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK")
sa_g <- grid %>% filter(r_iso %in% sa, year %in% yrs)   # BTN, MDV drop out (population rule)

rcps    <- sa_g %>% distinct(r_iso, first_law) %>% arrange(first_law, r_iso)
donorsS <- sa_g %>% filter(tie_dem == 1) %>% distinct(d_iso) %>%
  left_join(distinct(grid, d_iso, dem_share), by = "d_iso") %>% arrange(desc(dem_share))
ndS <- nrow(donorsS); nrS <- nrow(rcps)
b_d <- seq(0, 2 * pi, length.out = ndS + 1)[-(ndS + 1)]
b_r <- seq(0, 2 * pi, length.out = nrS + 1)[-(nrS + 1)] + pi / nrS
layS <- rbind(cbind(cos(b_d), sin(b_d)), 0.42 * cbind(cos(b_r), sin(b_r)))
w_rng <- range(log1p(sa_g$commit_dem[sa_g$tie_dem == 1]))
w_of  <- function(x) 0.5 + 5 * (log1p(x) - w_rng[1]) / max(diff(w_rng), 1e-9)

draw_N2 <- function() {
  par(mfrow = c(1, 2), mar = c(1, 1, 3.5, 1))
  for (y in yrs) {
    g_y <- sa_g %>% filter(year == y)
    e   <- g_y %>% filter(tie_dem == 1)
    g   <- make_empty_graph(ndS + nrS, directed = FALSE)
    V(g)$name <- c(donorsS$d_iso, rcps$r_iso)
    g <- add_edges(g, as.vector(rbind(e$d_iso, e$r_iso)))
    DO_y     <- g_y %>% distinct(d_iso, DO)
    d_DO     <- DO_y$DO[match(donorsS$d_iso, DO_y$d_iso)]
    d_active <- donorsS$d_iso %in% e$d_iso
    post     <- !is.na(rcps$first_law) & y > rcps$first_law
    e_DO     <- DO_y$DO[match(e$d_iso, DO_y$d_iso)]
    plot(g, layout = layS, rescale = FALSE, xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35),
         vertex.color = c(ifelse(!d_active, "grey92", ifelse(d_DO == 1, col_do, col_non)),
                          ifelse(post, col_post, "#E6A49C")),
         vertex.frame.color = c(ifelse(d_active, "white", "grey60"), rep("white", nrS)),
         vertex.size = c(rep(11, ndS), rep(19, nrS)),
         vertex.label = c(donorsS$d_iso, paste0(rcps$r_iso, "\n",
                          ifelse(is.na(rcps$first_law), "no law", rcps$first_law))),
         vertex.label.color = c(ifelse(d_active, "grey15", "grey60"), ifelse(post, "white", "grey20")),
         vertex.label.cex = c(rep(0.65, ndS), rep(0.62, nrS)), vertex.label.font = 2,
         vertex.label.dist = c(rep(1.5, ndS), rep(0, nrS)), vertex.label.degree = c(-b_d, rep(0, nrS)),
         edge.width = w_of(e$commit_dem),
         edge.color = ifelse(e_DO == 1, adjustcolor(col_do, 0.45), adjustcolor(col_non, 0.6)),
         main = sprintf("South Asia, %d: %d democracy-aid ties from %d donors\nties from DO donors: %d",
                        y, nrow(e), sum(d_active), sum(e_DO == 1)))
    if (y == yrs[1])
      legend("bottomleft", bty = "n", cex = 0.6, pch = 21, pt.cex = 1.2,
             pt.bg = c(col_do, col_non, "grey92", col_post, "#E6A49C"),
             col = c("white", "white", "grey60", "white", "white"),
             legend = c("DO donor (DOD), tie", "Other donor, tie", "Donor, no tie this year",
                        "Recipient, after law", "Recipient, before law"))
  }
}
pdf(file.path("output", "figures", "fig_N2_south_asia_2014_2019.pdf"), width = 11, height = 5.8); draw_N2(); dev.off()
png(file.path("output", "figures", "fig_N2_south_asia_2014_2019.png"), width = 11, height = 5.8, units = "in", res = 200); draw_N2(); dev.off()
