####################################################################################
# 02_build_network.R
# Builds the yearly donor x recipient aid networks studied in the extension.
#
# Network definition
#   - Two-mode (bipartite) network, one per year, 2005-2019.
#   - Mode 1 (donors):     the 19 large OECD DAC donors in Right et al. (2025).
#   - Mode 2 (recipients): aid-receiving countries in the paper's analysis sample
#                          (countries with population < 2 million in every year are
#                          dropped, exactly as in the paper: r_small == 0).
#   - Tie: donor i -> recipient j in year t if i's democracy-aid commitment to j in t
#          is positive (commit_dem > 0). Edge attribute "amount" = commitment (USD).
#   - A parallel economic-aid network uses commit_econ > 0.
#   Ties exist only between modes; there are no donor-donor or recipient-recipient ties.
#
# Input : output/rsw_prepared.rds   (written by 01_replication.R; has DOR/DOD)
# Output: output/network_grid.rds   (dyad-year table with ties and attributes)
#         output/nets_dem.rds, output/nets_econ.rds (lists of network objects by year)
#         output/tables/tab_network_desc.tex (nodes, ties, density by year)
####################################################################################

library(tidyverse)
library(network)
library(stargazer)

prep <- readRDS(file.path("output", "rsw_prepared.rds"))

# The dataset is a balanced donor x recipient panel within each year
# (every donor appears with every recipient), so absent ties are recorded zeros.
stopifnot(!any(duplicated(prep[c("d_iso", "r_iso", "year")])))

grid <- prep %>%
  filter(r_small == 0) %>%
  transmute(d_iso, r_iso, year, DonorName, RecipientName,
            commit_dem, commit_econ, commit_tot,
            tie_dem  = as.integer(commit_dem  > 0),
            tie_econ = as.integer(commit_econ > 0),
            DOD,                                   # paper's donor orientation (2008-2019)
            first_law = first.dir.sig,
            post_law  = as.integer(dir.sig.event.l == 1),
            treated   = treated_dyad,
            r_gdppc.ln, r_pop.ln, v2x_libdem, v2csreprss, region)

# ---- Donor attributes ----------------------------------------------------------
# dem_share: democracy aid as share of total aid, 2005-2019 (the paper's Figure 2).
# DO_static: top quartile of dem_share; used only to colour plots in years before
#            the paper's time-varying DOD exists (2005-2007).
donors <- grid %>%
  group_by(d_iso, DonorName) %>%
  summarize(dem_share  = sum(commit_dem, na.rm = TRUE) / sum(commit_tot, na.rm = TRUE),
            mean_total = sum(commit_tot, na.rm = TRUE) / n_distinct(year),
            .groups = "drop") %>%
  mutate(DO_static = as.integer(dem_share >= quantile(dem_share, 0.75)))

grid <- grid %>% left_join(select(donors, d_iso, dem_share, DO_static), by = "d_iso") %>%
  mutate(DO = coalesce(DOD, DO_static))          # DOD where defined, else static flag

# ---- Yearly bipartite network objects -------------------------------------------
make_net <- function(y, tie_var = "tie_dem", amt_var = "commit_dem") {
  g <- grid %>% filter(year == y)
  M <- g %>%
    mutate(w = ifelse(.data[[tie_var]] == 1, .data[[amt_var]], 0)) %>%
    select(d_iso, r_iso, w) %>%
    pivot_wider(names_from = r_iso, values_from = w) %>%
    column_to_rownames("d_iso") %>% as.matrix()
  net <- network(M, matrix.type = "bipartite", directed = FALSE,
                 ignore.eval = FALSE, names.eval = "amount")
  d_att <- g %>% distinct(d_iso, DO, dem_share)
  r_att <- g %>% distinct(r_iso, post_law, treated, first_law, r_gdppc.ln, r_pop.ln,
                          v2x_libdem, v2csreprss)
  nd <- nrow(M); nr <- ncol(M)
  i_d <- match(rownames(M), d_att$d_iso); i_r <- match(colnames(M), r_att$r_iso)
  net %v% "mode"       <- c(rep("donor", nd), rep("recipient", nr))
  net %v% "DO"         <- c(d_att$DO[i_d],        rep(NA, nr))
  net %v% "dem_share"  <- c(d_att$dem_share[i_d], rep(NA, nr))
  for (v in c("post_law", "treated", "first_law", "r_gdppc.ln", "r_pop.ln",
              "v2x_libdem", "v2csreprss"))
    net %v% v <- c(rep(NA, nd), r_att[[v]][i_r])
  net
}

years     <- sort(unique(grid$year))
nets_dem  <- setNames(lapply(years, make_net), years)
nets_econ <- setNames(lapply(years, make_net, tie_var = "tie_econ", amt_var = "commit_econ"), years)

saveRDS(grid,      file.path("output", "network_grid.rds"))
saveRDS(nets_dem,  file.path("output", "nets_dem.rds"))
saveRDS(nets_econ, file.path("output", "nets_econ.rds"))

# ---- Descriptive table: size and density by year ------------------------------------
desc <- grid %>%
  group_by(year) %>%
  summarize(Donors = n_distinct(d_iso), Recipients = n_distinct(r_iso),
            `Dem. ties` = sum(tie_dem), `Dem. density` = round(mean(tie_dem), 3),
            `Econ. ties` = sum(tie_econ), `Econ. density` = round(mean(tie_econ), 3),
            `Recipients post-law` = n_distinct(r_iso[post_law == 1]),
            .groups = "drop") %>%
  rename(Year = year)
print(desc, n = Inf)

stargazer(as.data.frame(mutate(desc, across(everything(), as.character))), summary = FALSE, rownames = FALSE, type = "latex",
          title = "Democracy- and economic-aid networks: size and density by year",
          label = "tab:netdesc", font.size = "footnotesize",
          notes = "Density = ties / (donors x recipients). Bipartite: only donor-recipient ties are possible.",
          out = file.path("output", "tables", "tab_network_desc.tex"))
