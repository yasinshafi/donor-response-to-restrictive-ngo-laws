# run_all.R -- regenerate every table and figure used in paper/main.tex.
# Run from the repository root (open PLSC508_replication.Rproj, or setwd() to this folder).
for (s in c("code/01_replication.R", "code/02_build_network.R",
            "code/03_network_plot.R", "code/04_vertex_attributes.R")) {
  message("Running ", s)
  source(s, echo = FALSE)
  rm(list = setdiff(ls(), "s"))   # each script is self-contained
}
