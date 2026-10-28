####################################################################################
# 05_icews_aggregate.R
# Aggregate raw ICEWS event files (one per year) to directed country-dyad-year counts.
# Run once (about 10 minutes); 06_icews_explore.Rmd reads only the output.
#
# Input : raw ICEWS files in ICEWS_DIR (events.YYYY.*.tab / .tsv, or the zipped copies)
# Output: data/icews_dyad_year.csv
#   one row per source ISO3 x target ISO3 x year x actor scope ("state" or "all"):
#   n_vcoop  verbal cooperation   (CAMEO roots 01-05)
#   n_mcoop  material cooperation (06-08)
#   n_aid    provide aid          (07; subset of n_mcoop)
#   n_vconf  verbal conflict      (09-13)
#   n_mconf  material conflict    (14-20)
#   n_all    all events;  sum_int  sum of intensity (Goldstein-style, -10 to 10)
#
# Decisions (see 06_icews_explore.Rmd for discussion):
#   - interstate events only (source country != target country, both non-missing)
#   - scope "state": both actors are states (actor named as the country itself, or a
#     government / executive / legislative / ministry / military / diplomatic sector)
#   - CAMEO codes in these files lost leading zeros (040 -> 40, 0311 -> 311); the root
#     code is recovered by parse_root() below
####################################################################################

library(data.table)
library(countrycode)

ICEWS_DIR <- "D:/Datasets/icews/events_data"   # <- edit to your raw ICEWS folder
YEARS     <- 2005:2019
OUT       <- file.path("data", "icews_dyad_year.csv")

parse_root <- function(code) {
  s <- as.character(code); n <- nchar(s)
  f2 <- suppressWarnings(as.integer(substr(s, 1, 2)))
  fifelse(n <= 2, as.integer(s) %/% 10L,                   # "40"  = 040  -> 04
  fifelse(n == 3 & f2 <= 20, f2,                           # "111" = 111  -> 11
  fifelse(n == 3, as.integer(substr(s, 1, 1)),             # "311" = 0311 -> 03
          f2)))                                            # "1031"       -> 10
}

to_iso <- function(x) {
  iso <- suppressWarnings(countrycode(x, "country.name", "iso3c",
                          custom_match = c(Kosovo = "XKX", Micronesia = "FSM")))
  iso
}

state_rx <- "Government|Executive|Legislative|Ministry|Military|Diplomat|Cabinet|Head of State"
cols <- c("Source Name", "Source Sectors", "Source Country", "CAMEO Code", "Intensity",
          "Target Name", "Target Sectors", "Target Country")

agg_year <- function(y) {
  f <- list.files(ICEWS_DIR, pattern = paste0("^[Ee]vents\\.", y, "\\..*\\.(tab|tsv)$"),
                  full.names = TRUE)
  stopifnot(length(f) == 1)
  message(y, ": ", basename(f))
  d <- fread(f, select = cols, quote = "", encoding = "UTF-8", showProgress = FALSE)
  setnames(d, make.names(sub("^\ufeff", "", names(d))))
  d <- d[Source.Country != "" & Target.Country != "" & Source.Country != Target.Country]
  d[, `:=`(root  = parse_root(CAMEO.Code),
           state = (Source.Name == Source.Country | grepl(state_rx, Source.Sectors)) &
                   (Target.Name == Target.Country | grepl(state_rx, Target.Sectors)))]
  summarize <- function(x) x[, .(n_vcoop = sum(root %in% 1:5),  n_mcoop = sum(root %in% 6:8),
                                 n_aid   = sum(root == 7),
                                 n_vconf = sum(root %in% 9:13), n_mconf = sum(root %in% 14:20),
                                 n_all   = .N, sum_int = sum(Intensity, na.rm = TRUE)),
                             by = .(src = Source.Country, tgt = Target.Country)]
  out <- rbind(summarize(d)[, scope := "all"], summarize(d[state == TRUE])[, scope := "state"])
  out[, year := y]
}

agg <- rbindlist(lapply(YEARS, agg_year))

# country names -> ISO3; drop non-state entities that do not map (e.g. "Palestinian Territory" maps to PSE)
nm  <- unique(c(agg$src, agg$tgt))
key <- data.table(name = nm, iso = to_iso(nm))
message("Unmatched names dropped: ", paste(key[is.na(iso), name], collapse = "; "))
agg <- merge(agg, key, by.x = "src", by.y = "name")[, src_iso := iso][, iso := NULL]
agg <- merge(agg, key, by.x = "tgt", by.y = "name")[, tgt_iso := iso][, iso := NULL]
agg <- agg[!is.na(src_iso) & !is.na(tgt_iso) & src_iso != tgt_iso,
           lapply(.SD, sum), by = .(src_iso, tgt_iso, year, scope),
           .SDcols = c("n_vcoop", "n_mcoop", "n_aid", "n_vconf", "n_mconf", "n_all", "sum_int")]

fwrite(agg, OUT)
message("Wrote ", OUT, ": ", nrow(agg), " rows")
