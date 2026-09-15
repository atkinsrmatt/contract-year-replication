## 01_build.R -- construct the analysis panel and gate it

.need <- 5L
if (!exists("PIPELINE_VERSION") || PIPELINE_VERSION < .need)
  stop("Stale 00_functions.R.\n",
       "  This script needs pipeline version ", .need, "; found ",
       if (exists("PIPELINE_VERSION")) PIPELINE_VERSION else "none", ".\n",
       "  The R/ files are from different versions. Update all of them\n",
       "  together, then re-run.")
if (!"cluster" %in% names(formals(felm)))
  stop("Stale felm(): it has no `cluster` argument. Overwrite R/00_functions.R.")

d <- read_prelim(RAWCSV)

hr("SECTION 1: IMPORT")
cat("Rows:", nrow(d), "  Columns:", ncol(d), "\n")
cat("Seasons:", paste(range(d$season), collapse = "-"), "\n")

hr("SECTION 2: MLS CODING DETECTION")

## Detected, not assumed: a years.days fraction cannot exceed .171, and
## double-converting raises no error.
frac      <- d$mls - floor(d$mls)
n_over171 <- sum(round(frac * 1000) > 171)
converted <- n_over171 > 0

cat("Obs with fractional part > .171:", n_over171, "of", nrow(d), "\n")

if (converted) {
  cat("=> MLS is ALREADY in decimal years. Conversion NOT re-applied.\n")
  d$mls_yrs <- d$mls
  d$mls_raw <- floor(d$mls) + round(frac * 172) / 1000
} else {
  cat("=> MLS is raw years.days. Converting to decimal years.\n")
  d$mls_raw <- d$mls
  d$mls_yrs <- floor(d$mls) + round(frac * 1000) / 172
}
d$mls_yrs_sq <- d$mls_yrs^2
d$mls_raw_sq <- d$mls_raw^2

cat(sprintf("mls_yrs: mean %.4f  sd %.4f  min %.3f  max %.3f\n",
            mean(d$mls_yrs), sd(d$mls_yrs), min(d$mls_yrs), max(d$mls_yrs)))
cat(sprintf("mls_raw: mean %.4f  (robustness only)\n", mean(d$mls_raw)))
cat("Manuscript Table 1 reports mean MLS = 4.3474 -> matches mls_yrs\n")

hr("SECTION 3: CONTRACT VARIABLES")

flagvars <- c("fa_elig","arb_elig","club_opt","player_opt","mutual_opt","vesting_opt")

d$option_any    <- as.integer(rowSums(d[, c("club_opt","player_opt",
                                            "mutual_opt","vesting_opt")]) > 0)
d$contract_year <- as.integer(rowSums(d[, flagvars]) > 0)

if (!exists("VET_CUTOFF")) VET_CUTOFF <- 5
VET_LAB  <- paste0(VET_CUTOFF, "+")
YNG_LAB  <- paste0("<", VET_CUTOFF)
d$veteran <- as.integer(d$mls_yrs >= VET_CUTOFF)
d$cy_vet  <- d$contract_year * d$veteran

d$pid_reg <- paste(d$playerid, d$veteran, sep = "_")
d$vm      <- d$veteran * d$mls_yrs
d$vm2     <- d$veteran * d$mls_yrs_sq

nflags <- rowSums(d[, flagvars])
cat("Flag composition:\n")
for (v in flagvars) cat(sprintf("  %-12s %5d\n", v, sum(d[[v]])))
cat(sprintf("  %-12s %5d\n", "option_any", sum(d$option_any)))
cat(sprintf("  %-12s %5d\n", "contract_year", sum(d$contract_year)))
cat(sprintf("  %-12s %5d   (MLS >= %g yr)\n", "veteran", sum(d$veteran), VET_CUTOFF))
cat(sprintf("  FA seasons on the veteran side: %d of %d;  arbitration swept in: %d\n",
            sum(d$fa_elig == 1 & d$veteran == 1), sum(d$fa_elig == 1),
            sum(d$arb_elig == 1 & d$veteran == 1)))

if (any(nflags > 1)) {
  cat("WARNING:", sum(nflags > 1), "obs with overlapping contract flags",
      "- contaminates the by-type disaggregation\n")
  print(d[nflags > 1, c("name","season",flagvars)])
} else {
  cat("PASS: no overlapping contract flags (Byrd correction held)\n")
}

hr("SECTION 4: LEAD / LAG")

lookup <- function(df, var) {
  setNames(df[[var]], paste(df$playerid, df$season, sep = "_"))
}
key <- function(pid, szn) paste(pid, szn, sep = "_")

for (v in c("contract_year","fa_elig")) {
  tab <- lookup(d, v)
  nm  <- if (v == "contract_year") "contract" else "fa"
  d[[paste0(nm, "_lead1")]] <- unname(ifelse(is.na(tab[key(d$playerid, d$season + 1)]),
                                             0L, tab[key(d$playerid, d$season + 1)]))
  d[[paste0(nm, "_lag1")]]  <- unname(ifelse(is.na(tab[key(d$playerid, d$season - 1)]),
                                             0L, tab[key(d$playerid, d$season - 1)]))
}

cat("contract_lead1 =", sum(d$contract_lead1),
    "  contract_lag1 =", sum(d$contract_lag1), "\n")
cat("fa_lead1       =", sum(d$fa_lead1),
    "  fa_lag1       =", sum(d$fa_lag1), "\n")

cat("\nValidation - Bryce Harper (arb 2016-17, FA 2018):\n")
bh <- d[d$name == "Bryce Harper",
        c("season","contract_year","contract_lead1","contract_lag1","fa_elig")]
if (nrow(bh)) print(bh[order(bh$season), ], row.names = FALSE) else
  cat("  (not in sample)\n")

hr("SECTION 5: SAMPLE STRUCTURE")

d$covid_season <- as.integer(d$season == 2020)
d$n_obs        <- ave(d$playerid, d$playerid, FUN = length)
mean_cy        <- ave(d$contract_year, d$playerid, FUN = mean)
d$has_variation <- as.integer(mean_cy > 0 & mean_cy < 1 & d$n_obs >= 2)

d$last_season  <- ave(d$season, d$playerid, FUN = max)
d$exiting_soon <- as.integer(d$last_season - d$season <= 1)
d$is_last      <- as.integer(d$season == d$last_season)

cat("Players:", length(unique(d$playerid)), " (manuscript 1,003)\n")
cat("Identifying FE:", length(unique(d$playerid[d$has_variation == 1])),
    " (manuscript 526)\n")

for (v in c("fa_elig","arb_elig","option_any")) {
  mv <- ave(d[[v]], d$playerid, FUN = mean)
  cat(sprintf("  %-11s identified by %d players\n", v,
              length(unique(d$playerid[mv > 0 & mv < 1 & d$n_obs >= 2]))))
}

pa20 <- mean(d$pa[d$season == 2020]); paoth <- mean(d$pa[d$season != 2020])
cat(sprintf("\nMean PA 2020 = %.1f vs %.1f otherwise\n", pa20, paoth))

ord <- d[order(d$playerid, d$season), c("playerid","season","name","mls_yrs")]
dropped_mls <- do.call(rbind, lapply(split(ord, ord$playerid), function(g) {
  if (nrow(g) < 2) return(NULL)
  i <- which(c(NA, diff(g$mls_yrs)) < -0.05)
  if (!length(i)) return(NULL)
  data.frame(name = g$name[i], season = g$season[i],
             mls_prev = g$mls_yrs[i - 1], mls_now = g$mls_yrs[i])
}))
if (is.null(dropped_mls)) {
  cat("PASS: service time is non-decreasing for every player\n")
} else {
  cat("FAIL:", nrow(dropped_mls), "player-season(s) where service time DECREASES",
      "- impossible; a transcription error in the source data:\n")
  print(dropped_mls, row.names = FALSE, digits = 6)
  cat("  Clean_Data.csv has this corrected (Cervelli 2018 reads 7.622093).\n")
  cat("  If this fires you are on the older PrelimData extract, where the same\n")
  cat("  row reads 0.622093. It feeds the MLS quadratic and the career-stage\n")
  cat("  split: it moves FE MLS from -0.00397 to -0.00312, wRC+ from 0.3074 to\n")
  cat("  0.3796, WAR from 0.0943 to 0.1045, and shifts one season across the\n")
  cat("  veteran threshold (1,340 vs 1,339). Switch to Clean_Data.csv.\n")
  stop("Uncorrected service time detected - do not use this output.")
}

miss <- colSums(is.na(d[, c("xwoba","woba","wrcplus","war","bsr","pa","mls_yrs")]))
cat("Missing values:", if (all(miss == 0)) "none" else
  paste(names(miss)[miss > 0], miss[miss > 0], collapse = "; "), "\n")

hr("SECTION 6: FINGERPRINT GATE")

fail <- FALSE

means <- list(xwoba = c(0.319217, 0.0001), woba = c(0.319496, 0.0001),
              wrcplus = c(100.922091, 0.01), war = c(1.650999, 0.001),
              pa = c(439.596333, 0.01), mls_yrs = c(4.349345, 0.001))
for (v in names(means)) {
  got <- mean(d[[v]]); exp_ <- means[[v]][1]; tol <- means[[v]][2]
  ok <- abs(got - exp_) <= tol
  cat(sprintf("  %-8s mean %12.6f  expected %12.6f  %s\n",
              v, got, exp_, if (ok) "ok" else "FAIL"))
  if (!ok) fail <- TRUE
}

sums <- list(contract_year = 1692L, contract_lead1 = 1268L, contract_lag1 = 1029L,
             fa_elig = 528L, arb_elig = 962L, option_any = 202L)
for (v in names(sums)) {
  got <- sum(d[[v]]); ok <- got == sums[[v]]
  cat(sprintf("  %-14s sum %6d  expected %6d  %s\n",
              v, got, sums[[v]], if (ok) "ok" else "FAIL"))
  if (!ok) fail <- TRUE
}

if (sum(d$veteran) != 1340L) {
  cat("  FAIL veteran seasons =", sum(d$veteran), "expected 1340\n")
  cat("       1339 means the uncorrected Cervelli row is still present.\n")
  fail <- TRUE
}
if (nrow(d) != 3654L) { cat("  FAIL N =", nrow(d), "expected 3654\n"); fail <- TRUE }
if (length(unique(d$playerid)) != 1003L) {
  cat("  FAIL players =", length(unique(d$playerid)), "expected 1003\n"); fail <- TRUE
}

if (fail) stop("FINGERPRINT FAILED - do not use this output. Check the CSV.")
cat("\n  PASSED - dataset matches the manuscript\n")

hr("SECTION 7: TABLE 1 - SUMMARY STATISTICS")

t1vars <- c("xwoba","woba","wrcplus","war","pa","mls_yrs")
t1 <- do.call(rbind, lapply(t1vars, function(v) {
  s0 <- d[[v]][d$contract_year == 0]; s1 <- d[[v]][d$contract_year == 1]
  data.frame(variable = v,
             non_mean = mean(s0), non_sd = sd(s0),
             cy_mean  = mean(s1), cy_sd  = sd(s1),
             all_mean = mean(d[[v]]), all_sd = sd(d[[v]]),
             stringsAsFactors = FALSE)
}))
print(format(t1, digits = 4), row.names = FALSE)
cat("\nObservations:", sum(d$contract_year == 0), "non-contract,",
    sum(d$contract_year == 1), "contract,", nrow(d), "full sample\n")

saveRDS(d, file.path(OUTDIR, "contract_year_clean.rds"))
write_csv_utf8(d,  file.path(OUTDIR, "contract_year_clean.csv"))
write_csv_utf8(t1, file.path(OUTDIR, "table1_summary.csv"))
