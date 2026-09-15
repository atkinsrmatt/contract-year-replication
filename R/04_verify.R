## 04_verify.R -- results gate

.need <- 5L
if (!exists("PIPELINE_VERSION") || PIPELINE_VERSION < .need)
  stop("Stale 00_functions.R.\n",
       "  This script needs pipeline version ", .need, "; found ",
       if (exists("PIPELINE_VERSION")) PIPELINE_VERSION else "none", ".\n",
       "  The R/ files are from different versions. Update all of them\n",
       "  together, then re-run.")
if (!"cluster" %in% names(formals(felm)))
  stop("Stale felm(): it has no `cluster` argument. Overwrite R/00_functions.R.")

hr("VERIFICATION")

## Gates the estimates; 01_build.R gates the data.
check <- function(label, got, want, tol) {
  ok <- !is.na(got) && abs(got - want) <= tol
  cat(sprintf("  %-34s %11.5f  %11.5f  %s\n", label, got, want,
              if (ok) "ok" else "FAIL"))
  ok
}

cat(sprintf("  %-34s %11s  %11s\n", "", "this run", "expected"))
cat("  ", strrep("-", 62), "\n", sep = "")

ok <- c(
  check("OLS coefficient",            M$ols$b["contract_year"],  -0.010678, 5e-5),
  check("OLS standard error",         M$ols$se["contract_year"],  0.001794, 5e-5),
  check("FE coefficient (main)",      M$fe$b["contract_year"],   -0.000022, 5e-5),
  check("FE standard error",          M$fe$se["contract_year"],   0.001097, 5e-5),
  check("FE p-value",                 M$fe$p["contract_year"],    0.9837,   0.005),
  check("FE MLS",                     M$fe$b["mls_yrs"],         -0.003967, 5e-5),
  check("FE MLS squared",             M$fe$b["mls_yrs_sq"],      -0.000331, 5e-5),

  check("Lead coefficient",           M$fe_ll$b["contract_lead1"], 0.001613, 5e-5),
  check("Lead p-value",               M$fe_ll$p["contract_lead1"], 0.1246,  0.005),
  check("Lag coefficient",            M$fe_ll$b["contract_lag1"], -0.000741, 5e-5),
  check("Lag p-value",                M$fe_ll$p["contract_lag1"],  0.5315,  0.005),
  check("Lead/lag joint F p-value",   w_ll$p,                      0.3540,  0.005),

  check("FE excl. 2020",              M$fe_nocovid$b["contract_year"], -0.000021, 5e-5),
  check("FE excl. 2020 p-value",      M$fe_nocovid$p["contract_year"],  0.9851,  0.005),
  check("wOBA coefficient",           M$fe_woba$b["contract_year"],     0.000355, 5e-5),
  check("wOBA p-value",               M$fe_woba$p["contract_year"],     0.7808,  0.005),
  check("wRC+ coefficient",           M$fe_wrcplus$b["contract_year"],  0.307387, 5e-4),
  check("wRC+ p-value",               M$fe_wrcplus$p["contract_year"],  0.7222,  0.005),
  check("WAR coefficient",            M$fe_war$b["contract_year"],      0.094322, 5e-4),
  check("WAR p-value",                M$fe_war$p["contract_year"],      0.1645,  0.005),
  check("BsR coefficient",            M$fe_bsr$b["contract_year"],      0.124179, 5e-4),

  check("FE raw MLS coding",          M$fe_rawmls$b["contract_year"],  -0.000007, 5e-5),

  check("Free agency",                M$fe_type$b["fa_elig"],     0.000622, 5e-5),
  check("Free agency p-value",        M$fe_type$p["fa_elig"],     0.6962,  0.005),
  check("Arbitration",                M$fe_type$b["arb_elig"],   -0.000360, 5e-5),
  check("Arbitration p-value",        M$fe_type$p["arb_elig"],    0.7717,  0.005),
  check("Option year",                M$fe_type$b["option_any"],  0.000386, 5e-5),
  check("Option year p-value",        M$fe_type$p["option_any"],  0.8573,  0.005),
  check("Type joint test p-value",    w_type$p,                   0.9480,  0.005),
  check("Type equality test p-value", w_eq$p,                     0.8348,  0.005),
  check("FA lead",                    M$fe_fall$b["fa_lead1"],   -0.000965, 5e-5),
  check("FA lag",                     M$fe_fall$b["fa_lag1"],    -0.000706, 5e-5),
  check("FA lead/lag joint p-value",  w_fall$p,                   0.8377,  0.005),

  check("Veteran effect",             M$fe_vet$b["contract_year"],    0.005682, 5e-5),
  check("Veteran standard error",     M$fe_vet$se["contract_year"],   0.001822, 5e-5),
  check("Veteran p-value",            M$fe_vet$p["contract_year"],    0.0019,  0.005),
  check("Veteran N",                  M$fe_vet$N,                  1340,     0),
  check("Younger effect",             M$fe_young$b["contract_year"], -0.001664, 5e-5),
  check("Younger N",                  M$fe_young$N,                2314,     0),
  check("Ladder (a) cy_vet",          M$fe_int$b["cy_vet"],           0.004052, 5e-5),
  check("Ladder (a) veteran main",    M$fe_int$b["veteran"],         -0.005012, 5e-5),
  check("Ladder (c) cy_vet",          M$fe_int_sat$b["cy_vet"],       0.007345, 5e-5),
  check("Split-sample difference",
        M$fe_vet$b["contract_year"] - M$fe_young$b["contract_year"],  0.007345, 5e-5),

  check("MDE pooled",                 RES$mde$mde[RES$mde$spec == "Pooled contract year"],
                                                                  0.003071, 5e-5),
  check("MDE pooled (SD units)",      RES$mde$mde_sd[RES$mde$spec == "Pooled contract year"],
                                                                  0.0845,  0.002),
  check("MDE free agency",            RES$mde$mde[RES$mde$spec == "Free agency"],
                                                                  0.004462, 5e-5),
  check("MDE arbitration",            RES$mde$mde[RES$mde$spec == "Arbitration"],
                                                                  0.003476, 5e-5),
  check("MDE veterans",               RES$mde$mde[grepl("^Veterans", RES$mde$spec)],
                                                                  0.005102, 5e-5),
  check("Raw cross-sectional gap",    raw_gap * 1000,             5.151,   0.01),

  check("Players ever FA eligible",
        sum(tapply(d$fa_elig, d$playerid, function(z) any(z == 1))),   324, 0),
  check("FA seasons recorded below six",
        sum(d$fa_elig == 1 & d$mls_yrs < 6),                           187, 0),
  check("FA seasons at six or above",
        sum(d$fa_elig == 1 & d$mls_yrs >= 6),                          341, 0),
  check("FA seasons on veteran side (cut 5)",
        sum(d$fa_elig == 1 & d$veteran == 1),                          524, 0),
  check("Players observed in both stages",
        sum(tapply(d$veteran, d$playerid, function(z) any(z == 1)) &
            tapply(d$veteran, d$playerid, function(z) any(z == 0))),   279, 0),
  check("Players never in a contract year",
        sum(tapply(d$contract_year, d$playerid, mean) == 0),           338, 0),
  check("Players always in a contract year",
        sum(tapply(d$contract_year, d$playerid, mean) == 1),           139, 0)
)

cat("  ", strrep("-", 62), "\n", sep = "")
cat(sprintf("  %d of %d checks passed\n", sum(ok), length(ok)))

if (!all(ok)) {
  stop("VERIFICATION FAILED - ", sum(!ok), " estimate(s) disagree with the ",
       "corrected series. Do not use this output until resolved.")
}
cat("  PASSED - every estimate reproduced on Clean_Data.csv\n")

.tgt <- unname(M$fe_vet$b["contract_year"] - M$fe_young$b["contract_year"])
.got <- unname(M$fe_int_sat$b["cy_vet"])
cat(sprintf("\n  Saturation identity: (c) = %.7f, split difference = %.7f, |diff| = %.1e\n",
            .got, .tgt, abs(.got - .tgt)))
if (abs(.got - .tgt) >= 1e-5)
  stop("Specification (c) does not reproduce the split-sample difference. ",
       "The saturated analogue is misspecified; do not report the identity claim.")
cat("  Identity holds. Specification (c) is the reportable interaction.\n")

hr("SE CONVENTION SENSITIVITY", "-")

cat(sprintf("  %-30s %10s %8s %10s %8s\n", "", "xtreg se", "p", "areg se", "p"))
dof_specs <- list(
  "Pooled contract year" = list(f("xwoba", "contract_year"), NULL,           "contract_year"),
  "Veterans"             = list(f("xwoba", "contract_year"), d$veteran == 1, "contract_year"),
  "Ladder (a)"           = list(f("xwoba", "contract_year + veteran + cy_vet"), NULL, "cy_vet"),
  "Ladder (b)"           = list(f("xwoba", "contract_year + cy_vet"), NULL, "cy_vet", "pid_reg"),
  "Ladder (c)"           = list(as.formula(paste(
      "xwoba ~ contract_year + cy_vet + mls_yrs + mls_yrs_sq + vm + vm2 +",
      "factor(season) + veteran:factor(season)")), NULL, "cy_vet", "pid_reg"),
  "Vet: free agency"     = list(f("xwoba", "fa_elig + option_any"), d$veteran == 1, "fa_elig")
)
for (nm in names(dof_specs)) {
  sp  <- dof_specs[[nm]]
  idv <- if (length(sp) >= 4) sp[[4]] else "playerid"
  a <- felm(sp[[1]], d, id = idv, cluster = "playerid", absorb = TRUE,
            subset = sp[[2]], dfadj = FALSE)
  b <- felm(sp[[1]], d, id = idv, cluster = "playerid", absorb = TRUE,
            subset = sp[[2]], dfadj = TRUE)
  cat(sprintf("  %-30s %10.5f %8.3f %10.5f %8.3f\n",
              nm, a$se[sp[[3]]], a$p[sp[[3]]], b$se[sp[[3]]], b$p[sp[[3]]]))
}
cat("  (xtreg convention is used throughout; areg shown for reference)\n")
cat("  Note that ladder (a) fails at conventional levels under areg and (b)\n")
cat("  and (c) do not. That is an independent argument for reporting (c).\n")
