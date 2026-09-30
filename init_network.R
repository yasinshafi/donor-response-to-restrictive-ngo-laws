# init_network.R
# Build yearly donor x recipient democracy-aid networks from the RSW replication data
# and plot exploratory descriptives. Input: rsw_dataset.csv. Output: nets.rds, network_descriptives.pdf

library(tidyverse)
library(network)

df <- read.csv("rsw_dataset.csv")
stopifnot(!any(duplicated(df[c("d_iso", "r_iso", "year")])))

# ---- 0. Sample: RSW small-country exclusion (pop < 2m every year) -------------
small <- df %>% distinct(r_iso, year, r_pop) %>%
  group_by(r_iso) %>%
  summarize(small = all(r_pop < 2e6 | is.na(r_pop))) %>%
  filter(small) %>% pull(r_iso)
df <- df %>% filter(!r_iso %in% small)

summary(df$commit_dem)   # check units (USD vs USD millions) before setting thr
thr <- 0                 # tie = democracy commitment > thr

# ---- 1. Balanced grid: every donor x every active recipient-year -------------
active <- df %>% distinct(r_iso, year)   # recipient appears in data that year

grid <- tidyr::crossing(d_iso = unique(df$d_iso), active) %>%
  left_join(df %>% select(d_iso, r_iso, year, commit_dem, commit_econ, commit_tot),
            by = c("d_iso", "r_iso", "year"))

cat("Dyad-years missing from data, filled as zero:",
    sum(is.na(grid$commit_dem)), "of", nrow(grid), "\n")

grid <- grid %>%
  mutate(across(c(commit_dem, commit_econ, commit_tot), ~replace_na(.x, 0)),
         tie_dem  = as.integer(commit_dem  > thr),
         tie_econ = as.integer(commit_econ > thr))

# ---- 2. Node attributes ---------------------------------------------------------
# Static DO flag for exploration only (top quartile of democracy share, 2005-19).
# RSW's DOD is time-varying; swap it in for inference.
dnr <- df %>% group_by(d_iso) %>%
  summarize(dem_share = sum(commit_dem, na.rm = TRUE) / sum(commit_tot, na.rm = TRUE)) %>%
  mutate(DO = as.integer(dem_share >= quantile(dem_share, 0.75)))

rcp <- df %>% group_by(r_iso) %>% summarize(first_law = suppressWarnings(min(first.dir.sig, na.rm = TRUE))) %>%
  mutate(first_law = ifelse(is.infinite(first_law), NA, first_law))

grid <- grid %>% left_join(dnr, by = "d_iso") %>% left_join(rcp, by = "r_iso") %>%
  mutate(post_law = as.integer(!is.na(first_law) & year > first_law))

# ---- 3. Yearly bipartite network objects (donors = first mode) --------------
make_net <- function(y) {
  g <- grid %>% filter(year == y)
  M <- g %>% mutate(w = ifelse(tie_dem == 1, commit_dem, 0)) %>%
    select(d_iso, r_iso, w) %>%
    pivot_wider(names_from = r_iso, values_from = w) %>%
    column_to_rownames("d_iso") %>% as.matrix()
  net <- network(M, matrix.type = "bipartite", directed = FALSE,
                 ignore.eval = FALSE, names.eval = "amount")
  r_attr <- g %>% distinct(r_iso, post_law, first_law)
  net %v% "DO"       <- c(dnr$DO[match(rownames(M), dnr$d_iso)], rep(NA, ncol(M)))
  net %v% "post_law" <- c(rep(NA, nrow(M)), r_attr$post_law[match(colnames(M), r_attr$r_iso)])
  net %v% "treated"  <- c(rep(NA, nrow(M)), as.integer(!is.na(r_attr$first_law[match(colnames(M), r_attr$r_iso)])))
  net
}

years <- sort(unique(grid$year))
nets  <- setNames(lapply(years, make_net), years)
saveRDS(nets, "nets.rds")
nets[["2016"]]   # sanity check

# ---- 4. Descriptives -------------------------------------------------------------
pdf("network_descriptives.pdf", width = 8, height = 5)

# (a) Density over time: democracy vs economic ties
p_a <- grid %>% group_by(year) %>%
  summarize(Democracy = mean(tie_dem), Economic = mean(tie_econ)) %>%
  pivot_longer(-year) %>%
  ggplot(aes(year, value, color = name)) + geom_line(linewidth = 1) +
  labs(title = "Network density by aid type", y = "Share of possible donor-recipient ties", x = NULL, color = NULL) +
  theme_minimal()
print(p_a)

# (b) Event time: share of donors funding democracy in law-passing recipients
p_b <- grid %>% filter(!is.na(first_law)) %>%
  mutate(t = year - first_law, type = ifelse(DO == 1, "DO donors", "Non-DO donors")) %>%
  filter(between(t, -4, 6)) %>%
  group_by(t, type) %>% summarize(share = mean(tie_dem), .groups = "drop") %>%
  ggplot(aes(t, share, color = type)) + geom_vline(xintercept = 0, color = "grey70") +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Democracy-aid ties around NGO law passage",
       subtitle = "Recipient composition changes across event time",
       x = "Years relative to law passage", y = "Share of donors with democracy tie", color = NULL) +
  theme_minimal()
print(p_b)

# (c) Donor degree: number of recipients with democracy tie, by year
p_c <- grid %>% group_by(d_iso, year, dem_share) %>% summarize(deg = sum(tie_dem), .groups = "drop") %>%
  ggplot(aes(year, reorder(d_iso, dem_share), fill = deg)) + geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Donor degree in democracy-aid network", x = NULL, y = "Donor (ordered by democracy share)", fill = "Recipients") +
  theme_minimal()
print(p_c)

# (d) Network plot, one year
y <- "2016"; net <- nets[[y]]; nd <- net %n% "bipartite"
cols <- c(ifelse(net %v% "DO" == 1, "darkblue", "steelblue1")[1:nd],
          ifelse((net %v% "post_law")[-(1:nd)] == 1, "darkorange", "grey75"))
set.seed(508)
plot(net, vertex.col = cols, vertex.cex = c(rep(1.6, nd), rep(0.8, network.size(net) - nd)),
     vertex.sides = c(rep(4, nd), rep(50, network.size(net) - nd)),
     label = c(network.vertex.names(net)[1:nd], rep("", network.size(net) - nd)),
     label.cex = 0.6, edge.col = adjustcolor("grey40", 0.25),
     main = paste("Democracy-aid network,", y))
legend("bottomleft", bty = "n", cex = 0.7, pch = c(15, 15, 19, 19),
       col = c("darkblue", "steelblue1", "darkorange", "grey75"),
       legend = c("DO donor", "Non-DO donor", "Recipient, post-law", "Recipient, no law yet"))

# (e) Donor co-funding projection: shared democracy-aid recipients, one year
B <- grid %>% filter(year == as.numeric(y)) %>%
  select(d_iso, r_iso, tie_dem) %>%
  pivot_wider(names_from = r_iso, values_from = tie_dem) %>%
  column_to_rownames("d_iso") %>% as.matrix()
P <- B %*% t(B)   # donor x donor shared recipients; diagonal = donor degree
p_e <- as.data.frame(as.table(P)) %>%
  ggplot(aes(Var1, Var2, fill = Freq)) + geom_tile() + scale_fill_viridis_c() +
  labs(title = paste("Donor co-funding (shared democracy-aid recipients),", y), x = NULL, y = NULL, fill = "Shared") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, size = 7))
print(p_e)

dev.off()


###########################################
############ BANGLADESH ###################
###########################################

# Bangladesh ego network: 2014 (pre-law) vs 2019 (post-law)
bgd <- grid %>%
  filter(r_iso == "BGD", year %in% c(2014, 2019)) %>%
  select(d_iso, year, DO, commit_dem, commit_econ, tie_dem) %>%
  pivot_wider(names_from = year, values_from = c(commit_dem, commit_econ, tie_dem)) %>%
  mutate(status = case_when(tie_dem_2014 == 1 & tie_dem_2019 == 1 ~ "stayed",
                            tie_dem_2014 == 1 & tie_dem_2019 == 0 ~ "exited",
                            tie_dem_2014 == 0 & tie_dem_2019 == 1 ~ "entered",
                            TRUE ~ "never"),
         dem_change = commit_dem_2019 - commit_dem_2014) %>%
  arrange(desc(DO), status)
print(bgd, n = Inf)

# Summary: democracy ties and amounts by donor type
bgd %>% group_by(DO) %>%
  summarize(ties_2014 = sum(tie_dem_2014), ties_2019 = sum(tie_dem_2019),
            dem_2014 = sum(commit_dem_2014), dem_2019 = sum(commit_dem_2019),
            econ_2014 = sum(commit_econ_2014), econ_2019 = sum(commit_econ_2019))

# BGD democracy-aid ego networks, 2014 vs 2019, drawn with the same layout and scale
yrs <- c(2014, 2019)
bg  <- grid %>% filter(r_iso == "BGD", year %in% yrs)

# Every donor tied in either year, fixed around a circle (DO donors grouped together)
donors <- bg %>% filter(tie_dem == 1) %>% distinct(d_iso) %>%
  left_join(dnr, by = "d_iso") %>% arrange(desc(DO), d_iso)
n   <- nrow(donors)
ang <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
lay <- rbind(c(0, 0), cbind(cos(ang), sin(ang)))

# One edge-width scale for both years, so widths are comparable
w_rng <- range(log1p(bg$commit_dem[bg$tie_dem == 1]))
w_of  <- function(x) 1 + 7 * (log1p(x) - w_rng[1]) / max(diff(w_rng), 1e-9)

par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
for (y in yrs) {
  e <- bg %>% filter(year == y, tie_dem == 1)
  g <- igraph::make_empty_graph(n + 1, directed = FALSE)
  igraph::V(g)$name <- c("BGD", donors$d_iso)
  g <- igraph::add_edges(g, as.vector(rbind("BGD", e$d_iso)))
  
  active <- donors$d_iso %in% e$d_iso
  fill <- c("darkorange", ifelse(!active, "grey92", ifelse(donors$DO == 1, "#1F3B73", "#7FA7D9")))
  
  plot(g, layout = lay, rescale = FALSE, xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35),
       vertex.color = fill,
       vertex.frame.color = c("darkorange", ifelse(active, "white", "grey60")),
       vertex.size = c(30, rep(14, n)),
       vertex.label.color = c("white", ifelse(active, "grey15", "grey60")),
       vertex.label.cex = c(0.9, rep(0.7, n)), vertex.label.font = 2,
       vertex.label.dist = c(0, rep(1.6, n)), vertex.label.degree = c(0, -ang),
       edge.width = w_of(e$commit_dem), edge.color = adjustcolor("grey35", 0.6),
       main = sprintf("%d: %d donors fund democracy aid\ntotal commit_dem = %s",
                      y, nrow(e), scales::comma(sum(e$commit_dem))))
  if (y == yrs[1])
    legend("bottomleft", bty = "n", cex = 0.7, pch = 21, pt.cex = 1.4,
           pt.bg = c("#1F3B73", "#7FA7D9", "grey92"), col = c("white", "white", "grey60"),
           legend = c("DO donor, tie", "Non-DO donor, tie", "No tie this year"))
}
par(mfrow = c(1, 1))

####################################################
#################### SOUTH ASIA ####################
####################################################

# South Asia democracy-aid network, 2014 vs 2019, drawn with the same layout and scale
sa  <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK")
yrs <- c(2014, 2019)
sa_g <- grid %>% filter(r_iso %in% sa, year %in% yrs)
setdiff(sa, unique(sa_g$r_iso))   # dropped by the RSW small-country rule (expect BTN, MDV)

# Recipients on an inner ring, ordered by NGO-law year
rcps <- sa_g %>% distinct(r_iso, first_law) %>% arrange(first_law, r_iso)
# Donors tied to any South Asian recipient in either year, on an outer ring (DO donors grouped)
donors <- sa_g %>% filter(tie_dem == 1) %>% distinct(d_iso) %>%
  left_join(dnr, by = "d_iso") %>% arrange(desc(DO), d_iso)
nd <- nrow(donors); nr <- nrow(rcps)

a_d <- seq(0, 2 * pi, length.out = nd + 1)[-(nd + 1)]
a_r <- seq(0, 2 * pi, length.out = nr + 1)[-(nr + 1)] + pi / nr
lay <- rbind(cbind(cos(a_d), sin(a_d)), 0.42 * cbind(cos(a_r), sin(a_r)))

# One edge-width scale for both years
w_rng <- range(log1p(sa_g$commit_dem[sa_g$tie_dem == 1]))
w_of  <- function(x) 0.5 + 5 * (log1p(x) - w_rng[1]) / max(diff(w_rng), 1e-9)

par(mfrow = c(1, 2), mar = c(1, 1, 3.5, 1))
for (y in yrs) {
  e <- sa_g %>% filter(year == y, tie_dem == 1) %>% left_join(select(dnr, d_iso, DO_e = DO), by = "d_iso")
  g <- igraph::make_empty_graph(nd + nr, directed = FALSE)
  igraph::V(g)$name <- c(donors$d_iso, rcps$r_iso)
  g <- igraph::add_edges(g, as.vector(rbind(e$d_iso, e$r_iso)))
  
  d_active <- donors$d_iso %in% e$d_iso
  post     <- !is.na(rcps$first_law) & y > rcps$first_law
  fill <- c(ifelse(!d_active, "grey92", ifelse(donors$DO == 1, "#1F3B73", "#7FA7D9")),
            ifelse(post, "darkorange", "#F3C9A8"))
  
  plot(g, layout = lay, rescale = FALSE, xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35),
       vertex.color = fill,
       vertex.frame.color = c(ifelse(d_active, "white", "grey60"), rep("white", nr)),
       vertex.size = c(rep(11, nd), rep(19, nr)),
       vertex.label = c(donors$d_iso, paste0(rcps$r_iso, "\n", ifelse(is.na(rcps$first_law), "no law", rcps$first_law))),
       vertex.label.color = c(ifelse(d_active, "grey15", "grey60"), ifelse(post, "white", "grey20")),
       vertex.label.cex = c(rep(0.65, nd), rep(0.62, nr)), vertex.label.font = 2,
       vertex.label.dist = c(rep(1.5, nd), rep(0, nr)), vertex.label.degree = c(-a_d, rep(0, nr)),
       edge.width = w_of(e$commit_dem),
       edge.color = ifelse(e$DO_e == 1, adjustcolor("#1F3B73", 0.45), adjustcolor("#7FA7D9", 0.6)),
       main = sprintf("South Asia, %d: %d democracy-aid ties from %d donors\nDO-donor ties: %d | total commit_dem = %s",
                      y, nrow(e), sum(d_active), sum(e$DO_e == 1), scales::comma(sum(e$commit_dem))))
  if (y == yrs[1])
    legend("bottomleft", bty = "n", cex = 0.62, pch = 21, pt.cex = 1.3,
           pt.bg = c("#1F3B73", "#7FA7D9", "grey92", "darkorange", "#F3C9A8"),
           col = c("white", "white", "grey60", "white", "white"),
           legend = c("DO donor, tie", "Non-DO donor, tie", "Donor, no SA tie this year",
                      "Recipient, post-law", "Recipient, pre-law"))
}
par(mfrow = c(1, 1))

####################################################
############### SOUTH ASIA SUMMARY #################
####################################################

sa   <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK")
yrs3 <- c(2005, 2014, 2019)
sa_g <- grid %>% filter(r_iso %in% sa) %>%
  mutate(donor_type = ifelse(DO == 1, "DO", "nonDO"))

# ---- 1. Recipient degree: ties by aid type x donor type ----------------------
deg_long <- sa_g %>%
  pivot_longer(c(tie_dem, tie_econ), names_to = "aid", values_to = "tie") %>%
  mutate(aid = recode(aid, tie_dem = "dem", tie_econ = "econ")) %>%
  group_by(r_iso, first_law, year, aid, donor_type) %>%
  summarize(deg = sum(tie), n_possible = n(), .groups = "drop")

# Recipient x year table (columns: dem_DO, dem_nonDO, econ_DO, econ_nonDO)
deg_tab <- deg_long %>% filter(year %in% yrs3) %>% select(-n_possible) %>%
  pivot_wider(names_from = c(aid, donor_type), values_from = deg, names_sep = "_") %>%
  arrange(r_iso, year)
print(deg_tab, n = Inf)

# Regional totals, with the number of possible ties for each donor type
deg_long %>% filter(year %in% yrs3) %>%
  group_by(year, aid, donor_type) %>%
  summarize(ties = sum(deg), possible = sum(n_possible), density = ties / possible, .groups = "drop") %>%
  print(n = Inf)

# Donor degree: number of South Asian recipients each donor funds
sa_g %>% filter(year %in% yrs3) %>%
  group_by(d_iso, donor_type, year) %>%
  summarize(dem = sum(tie_dem), econ = sum(tie_econ), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = c(dem, econ)) %>%
  arrange(donor_type, d_iso) %>% print(n = Inf)

# Trend plot, all years
deg_long %>%
  ggplot(aes(year, deg, color = donor_type, linetype = aid)) +
  geom_line(linewidth = 0.8) +
  geom_vline(aes(xintercept = first_law), color = "grey60", linetype = "dotted") +
  facet_wrap(~ r_iso) +
  scale_color_manual(values = c(DO = "#1F3B73", nonDO = "#7FA7D9")) +
  labs(title = "South Asia: donor ties by aid type and donor type",
       subtitle = "Dotted line = first NGO law", x = NULL, y = "Number of donors with a tie",
       color = "Donor", linetype = "Aid") +
  theme_minimal()

# ---- 2. Donor shared ties: South Asian recipients funded by both donors ------------
shared_ties <- function(y, tie_var) {
  B <- sa_g %>% filter(year == y) %>%
    select(d_iso, r_iso, t = all_of(tie_var)) %>%
    pivot_wider(names_from = r_iso, values_from = t) %>%
    column_to_rownames("d_iso") %>% as.matrix()
  P <- B %*% t(B)
  P[lower.tri(P, diag = TRUE)] <- NA              # keep each donor pair once
  as.data.frame(as.table(P), stringsAsFactors = FALSE) %>%
    filter(!is.na(Freq)) %>%
    transmute(d1 = Var1, d2 = Var2, shared = Freq, year = y,
              aid = ifelse(tie_var == "tie_dem", "dem", "econ"))
}

pairs <- expand_grid(y = yrs3, v = c("tie_dem", "tie_econ")) %>%
  pmap_dfr(~ shared_ties(..1, ..2)) %>%
  left_join(select(dnr, d_iso, DO1 = DO), by = c("d1" = "d_iso")) %>%
  left_join(select(dnr, d_iso, DO2 = DO), by = c("d2" = "d_iso")) %>%
  mutate(pair_type = case_when(DO1 + DO2 == 2 ~ "DO-DO",
                               DO1 + DO2 == 0 ~ "nonDO-nonDO",
                               TRUE ~ "DO-nonDO"))

# Summary by year, aid type and pair type
pairs %>% group_by(aid, year, pair_type) %>%
  summarize(pairs = n(), mean_shared = mean(shared), any_shared = mean(shared > 0),
            max_shared = max(shared), .groups = "drop") %>%
  print(n = Inf)

# Top co-funding pairs for democracy aid
pairs %>% filter(aid == "dem") %>% group_by(year) %>%
  slice_max(shared, n = 5, with_ties = FALSE) %>% print(n = Inf)

# Heatmap of shared democracy-aid recipients, one panel per year
pairs %>% filter(aid == "dem") %>%
  ggplot(aes(d1, d2, fill = shared)) + geom_tile(color = "white") +
  facet_wrap(~ year) + scale_fill_viridis_c() +
  labs(title = "Donor pairs: shared South Asian democracy-aid recipients",
       x = NULL, y = NULL, fill = "Shared") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, size = 6),
                          axis.text.y = element_text(size = 6))

####################################################
########### VISUALIZATION ##########################
####################################################

# viz_interactive.R
# Interactive and 3D views of the democracy-aid network.
# Run after init_network.R (needs `grid`). Output: HTML files you open in a browser.

if (!exists("grid")) source("init_network.R")

library(igraph)
library(visNetwork)
library(plotly)
library(htmlwidgets)

pal <- c("DO donor" = "#1F3B73", "Non-DO donor" = "#7FA7D9",
         "Recipient, post-law" = "#E07B39", "Recipient, no law yet" = "#BDBDBD")

# ---- Helper: nodes and edges for one year ------------------------------------
build_graph <- function(y) {
  g <- grid %>% filter(year == y)
  e <- g %>% filter(tie_dem == 1) %>% transmute(from = d_iso, to = r_iso, amount = commit_dem)
  deg <- bind_rows(count(e, id = from), count(e, id = to)) %>% rename(deg = n)
  nodes <- bind_rows(
    g %>% distinct(id = d_iso, DO) %>%
      mutate(mode = "donor", group = ifelse(DO == 1, "DO donor", "Non-DO donor")),
    g %>% distinct(id = r_iso, post_law, first_law) %>%
      mutate(mode = "recipient", group = ifelse(post_law == 1, "Recipient, post-law", "Recipient, no law yet"))
  ) %>%
    left_join(deg, by = "id") %>% mutate(deg = replace_na(deg, 0))
  list(nodes = nodes, edges = e)
}

# ---- 1. Interactive 2D network for one year (visNetwork) ---------------------
# Hover for details, click a node to highlight its ties, filter by group, drag nodes.
viz_year <- function(y) {
  x <- build_graph(y)
  nodes <- x$nodes %>%
    mutate(label = ifelse(mode == "donor", id, ""),
           shape = ifelse(mode == "donor", "square", "dot"),
           value = deg,
           title = paste0("<b>", id, "</b><br>", group,
                          ifelse(!is.na(first_law), paste0("<br>First NGO law: ", first_law), ""),
                          "<br>Democracy-aid ties: ", deg))
  edges <- x$edges %>%
    mutate(width = scales::rescale(log1p(amount), to = c(0.5, 5)),
           title = paste0(from, " → ", to, ": ", scales::comma(amount)),
           color = "rgba(110,110,110,0.25)")
  
  vn <- visNetwork(nodes, edges, main = paste("Democracy-aid network,", y), width = "100%", height = "750px")
  for (grp in names(pal)) vn <- vn %>% visGroups(groupname = grp, color = pal[[grp]])
  vn %>%
    visLegend(useGroups = TRUE, position = "right") %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
               selectedBy = "group", nodesIdSelection = TRUE) %>%
    visPhysics(solver = "forceAtlas2Based", stabilization = list(iterations = 300)) %>%
    visInteraction(navigationButtons = TRUE, tooltipDelay = 50)
}

saveWidget(viz_year(2016), "net_2016.html", selfcontained = TRUE)   # needs pandoc; else selfcontained = FALSE

# ---- 2. 3D time-stacked network (plotly) -------------------------------------
# Each horizontal layer is one year; node positions are fixed across years
# (layout from the pooled network), so exits and entries show up as lines
# disappearing or appearing between layers. `focus` limits edges and recipients
# to a few countries to keep it readable.
viz_stack <- function(years = 2005:2019, focus = NULL) {
  g <- grid %>% filter(year %in% years)
  
  all_nodes <- bind_rows(tibble(id = unique(g$d_iso), mode = "donor"),
                         tibble(id = unique(g$r_iso), mode = "recipient"))
  pooled <- g %>% filter(tie_dem == 1) %>% distinct(d_iso, r_iso)
  ig <- graph_from_data_frame(pooled, vertices = all_nodes, directed = FALSE)
  set.seed(508)
  L <- layout_with_fr(ig)
  pos <- tibble(id = V(ig)$name, X = L[, 1], Y = L[, 2])
  
  e <- g %>% filter(tie_dem == 1, is.null(focus) | r_iso %in% focus) %>%
    left_join(pos, by = c("d_iso" = "id")) %>%
    left_join(pos, by = c("r_iso" = "id"), suffix = c("0", "1"))
  seg <- function(a, b) as.vector(rbind(a, b, NA))   # NA breaks lines between edges
  
  pts <- bind_rows(
    g %>% distinct(id = d_iso, year, DO) %>%
      mutate(mode = "donor", group = ifelse(DO == 1, "DO donor", "Non-DO donor")),
    g %>% distinct(id = r_iso, year, post_law) %>%
      mutate(mode = "recipient", group = ifelse(post_law == 1, "Recipient, post-law", "Recipient, no law yet"))
  ) %>%
    filter(is.null(focus) | mode == "donor" | id %in% focus) %>%
    left_join(pos, by = "id")
  
  plot_ly() %>%
    add_trace(type = "scatter3d", mode = "lines",
              x = seg(e$X0, e$X1), y = seg(e$Y0, e$Y1), z = seg(e$year, e$year),
              line = list(color = "rgba(110,110,110,0.3)", width = 1),
              hoverinfo = "none", name = "Democracy-aid tie") %>%
    add_trace(data = pts, type = "scatter3d", mode = "markers",
              x = ~X, y = ~Y, z = ~year, color = ~group, colors = pal,
              marker = list(size = 3), text = ~paste0(id, " (", year, ")"), hoverinfo = "text") %>%
    layout(title = "Democracy-aid network over time (one layer per year)",
           scene = list(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                        zaxis = list(title = "Year", dtick = 1)))
}

saveWidget(viz_stack(), "net_3d_all.html", selfcontained = TRUE)
saveWidget(viz_stack(focus = c("BGD", "KHM", "ETH")), "net_3d_focus.html", selfcontained = TRUE)
