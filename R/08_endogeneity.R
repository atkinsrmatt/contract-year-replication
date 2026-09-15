## 08_endogeneity.R -- is contract year status predicted by prior performance?
##
## Selection equation, outcome equation with a lag control, and balance.

if (!exists("M") || !exists("d")) stop("Run 02_models.R first.")

hr("ENDOGENEITY OF CONTRACT YEAR TIMING")

## Matched on (player, season - 1), not row position: the panel has gaps, so a
## positional lag would pair non-adjacent seasons. NA stays NA here -- unlike
## the contract flags in 01_build.R -- so those rows drop from the regressions.

.key <- function(pid, szn) paste(pid, szn, sep = "_")
for (v in c("xwoba", "woba", "war")) {
  tab <- setNames(d[[v]], .key(d$playerid, d$season))
  d[[paste0(v, "_lag1")]] <- unname(tab[.key(d$playerid, d$season - 1)])
}

vet <- d$veteran == 1
cat(sprintf("\n  Veteran observations                    %5d\n", sum(vet)))
cat(sprintf("  ...with the prior season in the panel   %5d  (%.0f%%)\n",
            sum(vet & !is.na(d$xwoba_lag1)),
            100 * mean(!is.na(d$xwoba_lag1[vet]))))
cat("  Rows without an observed prior season drop from every test below.\n")

## (a) Selection equation. A linear probability model; the coefficient is the
## object of interest, not fitted probabilities.

cat("\n  (a) Does lagged performance predict contract year status?\n")
cat("      Within player, veterans only. Negative => clubs extend after good\n")
cat("      seasons, so walk years follow down years, and reversion inflates\n")
cat("      the main estimate.\n\n")
cat(sprintf("      %-34s %10s %10s %8s %6s\n", "", "coef", "se", "p", "N"))

sel <- list()
for (dv in c("contract_year", "fa_elig")) {
  for (lagv in c("xwoba_lag1", "woba_lag1", "war_lag1")) {
    m <- felm(as.formula(paste(dv, "~", lagv,
              "+ mls_yrs + mls_yrs_sq + factor(season)")),
              d, absorb = TRUE, subset = vet)
    lab <- paste0(sub("_lag1", "", lagv), " -> ",
                  ifelse(dv == "fa_elig", "free agency", "contract year"))
    cat(sprintf("      %-34s %10.5f %10.5f %8.3f %6d\n",
                lab, m$b[lagv], m$se[lagv], m$p[lagv], m$N))
    sel[[paste(dv, lagv)]] <- m
  }
}

sdx <- sd(d$xwoba)
b1  <- unname(sel[["contract_year xwoba_lag1"]]$b["xwoba_lag1"])
cat(sprintf("\n      A one-SD (%.1f-point) better prior xwOBA shifts the probability\n",
            sdx * 1000))
cat(sprintf("      of a contract year by %+.1f percentage points.\n", b1 * sdx * 100))

## (b) Outcome equation. A lagged dependent variable in a fixed effects model
## induces Nickell bias, so the coefficient ON THE LAG is not interpretable.
## The question here is only whether the contract year estimate moves.

cat("\n  (b) Does conditioning on the prior season move the estimate?\n")
cat(sprintf("      %-34s %10s %10s %8s %6s\n", "", "cy coef", "se", "p", "N"))

base_lagsample <- felm(f("xwoba", "contract_year"), d, absorb = TRUE,
                       subset = vet & !is.na(d$xwoba_lag1))
with_lag <- felm(f("xwoba", "contract_year + xwoba_lag1"), d, absorb = TRUE,
                 subset = vet)

show_b <- function(lab, m) cat(sprintf("      %-34s %10.5f %10.5f %8.3f %6d\n",
  lab, m$b["contract_year"], m$se["contract_year"], m$p["contract_year"], m$N))
show_b("as reported (full veteran sample)", M$fe_vet)
show_b("same, restricted to lag sample",   base_lagsample)
show_b("adding lagged xwOBA",              with_lag)
cat(sprintf("\n      Change attributable to the lag control: %+.5f\n",
            unname(with_lag$b["contract_year"] - base_lagsample$b["contract_year"])))
cat("      (Compare against the lag-sample row, not the full sample: the two\n")
cat("       differ in composition as well as specification.)\n")

cat("\n  (c) Prior-season performance by contract year status, veterans:\n")
ok <- vet & !is.na(d$xwoba_lag1)
m1 <- mean(d$xwoba_lag1[ok & d$contract_year == 1])
m0 <- mean(d$xwoba_lag1[ok & d$contract_year == 0])
cat(sprintf("      contract years      %.4f  (n = %d)\n", m1, sum(ok & d$contract_year == 1)))
cat(sprintf("      non-contract years  %.4f  (n = %d)\n", m0, sum(ok & d$contract_year == 0)))
cat(sprintf("      raw difference      %+.4f  (%+.1f points)\n", m1 - m0, (m1 - m0) * 1000))

dw <- d[ok, ]
dw$xl_dm <- dw$xwoba_lag1 - ave(dw$xwoba_lag1, dw$playerid)
w1 <- mean(dw$xl_dm[dw$contract_year == 1]); w0 <- mean(dw$xl_dm[dw$contract_year == 0])
cat(sprintf("      within-player diff  %+.4f  (%+.1f points)\n", w1 - w0, (w1 - w0) * 1000))

p_sel <- unname(sel[["contract_year xwoba_lag1"]]$p["xwoba_lag1"])
moved <- abs(unname(with_lag$b["contract_year"] -
                    base_lagsample$b["contract_year"]))
cat("\n  Reading:\n")
if (p_sel > .10 && moved < 5e-4) {
  cat("      Prior performance does not predict contract year status, and the\n")
  cat("      estimate is unchanged when the prior season is conditioned on.\n")
  cat("      The reversion-into-walk-years channel is not operating here.\n")
} else {
  cat("      At least one test is inconsistent with the null of no selection\n")
  cat("      on prior performance. Read the coefficients above before writing\n")
  cat("      anything about this in the manuscript.\n")
}
cat("      This addresses selection on OBSERVED prior output only. Clubs may\n")
cat("      extend on private information about future prospects; that channel\n")
cat("      would negatively select walk-year players and bias the contract\n")
cat("      year estimate toward zero, making it conservative.\n")

if (exists("FACTS")) {
  fact("6.7", "selection_lag_xwoba", sel[["contract_year xwoba_lag1"]]$b["xwoba_lag1"])
  fact("6.7", "selection_lag_xwoba_p", p_sel)
  fact("6.7", "vet_cy_with_lag_control", with_lag$b["contract_year"])
  fact("6.7", "vet_cy_lag_sample", base_lagsample$b["contract_year"])
  facts <- do.call(rbind, FACTS)
  write_csv_utf8(facts, file.path(OUTDIR, "manuscript_facts.csv"))
  cat(sprintf("\n  appended 4 quantities to manuscript_facts.csv\n"))
}
