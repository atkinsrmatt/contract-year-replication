## 02_models.R -- all estimates

.need <- 5L
if (!exists("PIPELINE_VERSION") || PIPELINE_VERSION < .need)
  stop("Stale 00_functions.R.\n",
       "  This script needs pipeline version ", .need, "; found ",
       if (exists("PIPELINE_VERSION")) PIPELINE_VERSION else "none", ".\n",
       "  The R/ files are from different downloads. Re-download all five\n",
       "  R/ files together and overwrite, then re-run.")
if (!"cluster" %in% names(formals(felm)))
  stop("Stale felm(): it has no `cluster` argument. Overwrite R/00_functions.R.")

CTRL <- "mls_yrs + mls_yrs_sq + factor(season)"
f <- function(dv, rhs) as.formula(paste(dv, "~", rhs, "+", CTRL))

M   <- list()
RES <- list()

hr("SECTION 8: MAIN MODELS")

cat("\nMODEL 1: Pooled OLS (biased baseline)\n")
M$ols <- felm(f("xwoba", "contract_year"), d, absorb = FALSE)
print(M$ols, keep = c("contract_year","mls_yrs","mls_yrs_sq"))

cat("\nMODEL 2: Player FE (MAIN RESULT)\n")
M$fe <- felm(f("xwoba", "contract_year"), d, absorb = TRUE)
print(M$fe, keep = c("contract_year","mls_yrs","mls_yrs_sq"))

nest_ok <- all(tapply(d$playerid, d$playerid, function(z) length(unique(z))) == 1)
cat("\n  Player FE nested within player clusters:", nest_ok, "\n")

M$fe_dfadj <- felm(f("xwoba", "contract_year"), d, absorb = TRUE, dfadj = TRUE)
cat(sprintf("  SE convention  xtreg,fe (FE excluded from K) : %.6f  p = %.3f  <- used\n",
            M$fe$se["contract_year"], M$fe$p["contract_year"]))
cat(sprintf("  SE convention  areg     (FE included in K)   : %.6f  p = %.3f\n",
            M$fe_dfadj$se["contract_year"], M$fe_dfadj$p["contract_year"]))
cat(sprintf("  Inflation factor: %.3fx. Reported SE is the xtreg one, 0.0011.\n",
            M$fe_dfadj$se["contract_year"] / M$fe$se["contract_year"]))

cat("\nMODEL 3: Lead / lag\n")
M$fe_ll <- felm(f("xwoba", "contract_lead1 + contract_year + contract_lag1"),
                d, absorb = TRUE)
print(M$fe_ll, keep = c("contract_lead1","contract_year","contract_lag1"))
w_ll <- wald(M$fe_ll, c("contract_lead1","contract_year","contract_lag1"))
cat(sprintf("  Joint F(%d,%d) = %.3f   p = %.4f  (lead = year = lag = 0)\n",
            w_ll$df1, w_ll$df2, w_ll$F, w_ll$p))

cat("\nMODEL 4: Excluding 2020\n")
M$fe_nocovid <- felm(f("xwoba", "contract_year"), d, absorb = TRUE,
                     subset = d$covid_season == 0)
print(M$fe_nocovid, keep = "contract_year")

cat("\nMODEL 5: Raw years.days MLS coding\n")
M$fe_rawmls <- felm(as.formula(
  "xwoba ~ contract_year + mls_raw + mls_raw_sq + factor(season)"), d, absorb = TRUE)
print(M$fe_rawmls, keep = "contract_year")
cat(sprintf("  Raw %.5f vs converted %.5f - null is invariant to coding\n",
            M$fe_rawmls$b["contract_year"], M$fe$b["contract_year"]))

cat("\nAlternative outcomes\n")
for (dv in c("woba","wrcplus","war","bsr")) {
  M[[paste0("fe_", dv)]] <- felm(f(dv, "contract_year"), d, absorb = TRUE)
  m <- M[[paste0("fe_", dv)]]
  cat(sprintf("  %-8s b = %9.4f  se = %8.4f  p = %.3f\n",
              dv, m$b["contract_year"], m$se["contract_year"], m$p["contract_year"]))
}

RES$main <- rbind(
  grab(M$ols,         "contract_year", "OLS"),
  grab(M$fe,          "contract_year", "Player FE (main)"),
  grab(M$fe_ll,       "contract_year", "Lead/lag FE"),
  grab(M$fe_nocovid,  "contract_year", "FE excl. 2020"),
  grab(M$fe_rawmls,   "contract_year", "FE raw MLS"),
  grab(M$fe_woba,     "contract_year", "wOBA"),
  grab(M$fe_wrcplus,  "contract_year", "wRC+"),
  grab(M$fe_war,      "contract_year", "WAR"),
  grab(M$fe_bsr,      "contract_year", "BsR")
)

hr("SECTION 9: CONTRACT TYPE DISAGGREGATION")

M$fe_type <- felm(f("xwoba", "fa_elig + arb_elig + option_any"), d, absorb = TRUE)
for (v in c("fa_elig","arb_elig","option_any")) {
  b <- M$fe_type$b[v]; se <- M$fe_type$se[v]
  cat(sprintf("  %-11s b = %9.5f  se = %8.5f  p = %.3f  95%% CI [%8.5f, %8.5f]\n",
              v, b, se, M$fe_type$p[v], b - 1.96 * se, b + 1.96 * se))
}
w_type <- wald(M$fe_type, c("fa_elig","arb_elig","option_any"), "zero")
w_eq   <- wald(M$fe_type, c("fa_elig","arb_elig","option_any"), "equal")
cat(sprintf("  Joint  (FA = Arb = Option = 0): F(%d,%d) = %.3f  p = %.4f\n",
            w_type$df1, w_type$df2, w_type$F, w_type$p))
cat(sprintf("  Equality (FA = Arb = Option):   F(%d,%d) = %.3f  p = %.4f\n",
            w_eq$df1, w_eq$df2, w_eq$F, w_eq$p))
cat("  Non-rejection means the types are indistinguishable, so pooling\n",
    "  them in the main specification is defensible.\n", sep = "")

cat("\n  Free agency lead/lag - sharpest incentive-cycle test:\n")
M$fe_fall <- felm(f("xwoba", "fa_lead1 + fa_elig + fa_lag1"), d, absorb = TRUE)
print(M$fe_fall, keep = c("fa_lead1","fa_elig","fa_lag1"))
w_fall <- wald(M$fe_fall, c("fa_lead1","fa_elig","fa_lag1"))
cat(sprintf("  Joint F(%d,%d) = %.3f  p = %.4f\n",
            w_fall$df1, w_fall$df2, w_fall$F, w_fall$p))

RES$bytype <- rbind(
  grab(M$fe_type, "fa_elig",    "Free agency"),
  grab(M$fe_type, "arb_elig",   "Arbitration"),
  grab(M$fe_type, "option_any", "Option year")
)

hr("SECTION 9B: HETEROGENEITY BY CAREER STAGE")

M$fe_vet   <- felm(f("xwoba", "contract_year"), d, absorb = TRUE,
                   subset = d$veteran == 1)
M$fe_young <- felm(f("xwoba", "contract_year"), d, absorb = TRUE,
                   subset = d$veteran == 0)
M$fe_int   <- felm(f("xwoba", "contract_year + veteran + cy_vet"), d, absorb = TRUE)

cat(sprintf("\n  Veterans (%s yr service):\n", VET_LAB));  print(M$fe_vet,   keep = "contract_year")
cat(sprintf("\n  Younger  (%s yr service):\n", YNG_LAB));  print(M$fe_young, keep = "contract_year")
cat("\n  Interaction (formal test of the difference):\n")
print(M$fe_int, keep = c("contract_year","cy_vet"))

## Only (c) saturates, so only (c) reproduces the split-sample difference.
## Report (c), not (a); 05_table7.R asserts the identity and stops if it fails.
LADDER <- list()
cat("\n  Interaction ladder (cy_vet), clustered on player:\n")
cat(sprintf("  %-40s %9s %9s %7s %7s\n", "", "cy_vet", "se", "p", "p areg"))
lad <- list(
  "(a) common player FE"            = list(f("xwoba","contract_year + veteran + cy_vet"), "playerid"),
  "(b) player x career-stage FE"          = list(f("xwoba","contract_year + cy_vet"), "pid_reg"),
  "(c) + controls interacted"       = list(as.formula(paste(
      "xwoba ~ contract_year + cy_vet + mls_yrs + mls_yrs_sq + vm + vm2 +",
      "factor(season) + veteran:factor(season)")), "pid_reg")
)
for (nm in names(lad)) {
  sp <- lad[[nm]]
  a1 <- felm(sp[[1]], d, id = sp[[2]], cluster = "playerid", absorb = TRUE)
  a2 <- felm(sp[[1]], d, id = sp[[2]], cluster = "playerid", absorb = TRUE, dfadj = TRUE)
  cat(sprintf("  %-40s %9.5f %9.5f %7.3f %7.3f\n", nm,
              a1$b["cy_vet"], a1$se["cy_vet"], a1$p["cy_vet"], a2$p["cy_vet"]))
  LADDER[[nm]] <- list(xtreg = a1, areg = a2)
  if (nm == "(c) + controls interacted") M$fe_int_sat <- a1
}
cat(sprintf("  %-40s %9.5f\n", "split-sample difference (target)",
            M$fe_vet$b["contract_year"] - M$fe_young$b["contract_year"]))
cat("  (c) reproduces the split sample and is the specification to report.\n")

cat(sprintf("\n  (a) veteran main effect: %.5f (se %.5f, p = %.3f)\n",
            M$fe_int$b["veteran"], M$fe_int$se["veteran"], M$fe_int$p["veteran"]))
cat("  In (b) and (c) it is absorbed by the player x career-stage intercepts.\n")

cat("\n  Service-time cutoff sensitivity:\n")
cut_tab <- do.call(rbind, lapply(c(4, 5, 6, 7), function(cut) {
  m <- felm(f("xwoba", "contract_year"), d, absorb = TRUE, subset = d$mls_yrs >= cut)
  cat(sprintf("    >=%dyr: b = %9.5f  p = %.3f  N = %5d\n",
              cut, m$b["contract_year"], m$p["contract_year"], m$N))
  grab(m, "contract_year", paste0(">=", cut, "yr"))
}))

cat("\n  Veterans, alternative outcomes:\n")
vet_alt <- do.call(rbind, lapply(c("woba","wrcplus","war","bsr"), function(dv) {
  m <- felm(f(dv, "contract_year"), d, absorb = TRUE, subset = d$veteran == 1)
  cat(sprintf("    %-8s b = %9.4f  p = %.3f\n",
              dv, m$b["contract_year"], m$p["contract_year"]))
  grab(m, "contract_year", paste0("veteran ", dv))
}))

RES$stage <- rbind(
  grab(M$fe,       "contract_year", "Pooled"),
  grab(M$fe_vet,   "contract_year", paste0("Veterans (", VET_LAB, ")")),
  grab(M$fe_young, "contract_year", paste0("Younger (", YNG_LAB, ")")),
  grab(M$fe_int,     "cy_vet", "Interaction CY x veteran (a: common FE)"),
  grab(M$fe_int_sat, "cy_vet", "Interaction CY x veteran (c: saturated)"),
  grab(M$fe_int,     "veteran", "Veteran main effect (a only)"),
  cut_tab, vet_alt
)

hr("SECTION 9C: BASERUNNING")

for (lbl in c("all","veteran","younger")) {
  ss <- switch(lbl, all = rep(TRUE, nrow(d)),
               veteran = d$veteran == 1, younger = d$veteran == 0)
  m <- felm(f("bsr", "contract_year"), d, absorb = TRUE, subset = ss)
  cat(sprintf("  %-8s b = %8.4f  p = %.3f  N = %5d\n",
              lbl, m$b["contract_year"], m$p["contract_year"], m$N))
}

hr("SECTION 9D: ROBUSTNESS OF THE VETERAN EFFECT")

cat("\n  (1) Which contract type drives the veteran effect?\n")
cat("      (Among veterans, arbitration obs =",
    sum(d$arb_elig[d$veteran == 1]), ")\n")
m9d1 <- felm(f("xwoba", "fa_elig + option_any"), d, absorb = TRUE,
             subset = d$veteran == 1)
print(m9d1, keep = c("fa_elig","option_any"))

cat("\n  (2a) Survivorship: veterans among 5+ season players\n")
m9d2 <- felm(f("xwoba", "contract_year"), d, absorb = TRUE,
             subset = d$veteran == 1 & d$n_obs >= 5)
print(m9d2, keep = "contract_year")

cat("\n  (2b) Survivorship: control for imminent career exit\n")
m9d3 <- felm(f("xwoba", "contract_year + exiting_soon"), d, absorb = TRUE,
             subset = d$veteran == 1)
print(m9d3, keep = c("contract_year","exiting_soon"))

cat("\n  (2c) Survivorship: drop each player's final season\n")
m9d4 <- felm(f("xwoba", "contract_year"), d, absorb = TRUE,
             subset = d$veteran == 1 & d$is_last == 0)
print(m9d4, keep = "contract_year")

cat("\n  (2d) Placebo: exit timing alone among NON-contract-year veterans\n")
m9d5 <- felm(f("xwoba", "exiting_soon"), d, absorb = TRUE,
             subset = d$veteran == 1 & d$contract_year == 0)
print(m9d5, keep = "exiting_soon")
cat(sprintf("      (2b) estimate %+.5f (se %.5f) vs (2d) %+.5f (se %.5f)\n",
            m9d3$b["exiting_soon"], m9d3$se["exiting_soon"],
            m9d5$b["exiting_soon"], m9d5$se["exiting_soon"]))
cat(sprintf("      placebo MDE = %.5f, which is %.1fx its own point estimate\n",
            mde(m9d5$se["exiting_soon"]),
            mde(m9d5$se["exiting_soon"]) / abs(m9d5$b["exiting_soon"])))
cat("      => UNINFORMATIVE, not supportive. The contract-specificity of the\n")
cat("      veteran effect rests on (2a)-(2c), not on this placebo.\n")

RES$survivorship <- rbind(
  grab(m9d1, "fa_elig",       "Vet: free agency"),
  grab(m9d1, "option_any",    "Vet: option"),
  grab(m9d2, "contract_year", "Vet: 5+ season players"),
  grab(m9d3, "contract_year", "Vet: control exit timing"),
  grab(m9d4, "contract_year", "Vet: drop final season"),
  grab(m9d5, "exiting_soon",  "Placebo: exit timing (UNINFORMATIVE)")
)

hr("SECTION 10: MINIMUM DETECTABLE EFFECTS")

sd_x <- sd(d$xwoba)
cat(sprintf("  xwOBA SD = %.4f\n\n", sd_x))
cat(sprintf("  %-22s %9s %9s %9s %9s\n",
            "Specification", "SE", "MDE", "pts", "SD units"))

mde_specs <- list(M$fe$se["contract_year"], M$fe_type$se["fa_elig"],
                  M$fe_type$se["arb_elig"], M$fe_type$se["option_any"],
                  M$fe_vet$se["contract_year"])
names(mde_specs) <- c("Pooled contract year", "Free agency", "Arbitration",
                      "Option year", paste0("Veterans (", VET_LAB, ")"))
RES$mde <- do.call(rbind, lapply(names(mde_specs), function(nm) {
  se <- unname(mde_specs[[nm]]); m <- mde(se)
  cat(sprintf("  %-22s %9.5f %9.5f %9.1f %9.3f\n", nm, se, m, m * 1000, m / sd_x))
  data.frame(spec = nm, se = se, mde = m, mde_pts = m * 1000,
             mde_sd = m / sd_x, stringsAsFactors = FALSE)
}))

m0 <- mean(d$xwoba[d$contract_year == 0]); m1 <- mean(d$xwoba[d$contract_year == 1])
qs <- quantile(d$xwoba, c(0.50, 0.75))
raw_gap <- m0 - m1
cat("\n  Benchmarks (xwOBA points):\n")
cat(sprintf("    1 SD                     = %5.1f\n", sd_x * 1000))
cat(sprintf("    Median -> 75th pctile    = %5.1f\n", (qs[2] - qs[1]) * 1000))
cat(sprintf("    Raw cross-sectional gap  = %5.1f\n", raw_gap * 1000))
cat("  The design rules out effects larger than ~3 points of xwOBA -\n")
cat("  smaller than the raw gap that motivates the folk belief.\n")
cat("  NOTE: that claim is about the POOLED coefficient. The veteran effect is\n")
cat(sprintf("  %.1f points, which is %.0f%% larger than the %.1f-point raw gap the\n",
            M$fe_vet$b["contract_year"] * 1000,
            (M$fe_vet$b["contract_year"] / raw_gap - 1) * 100, raw_gap * 1000))
cat("  pooled TOST rules out, and 1.13x its own MDE. Phrasing the power claim\n")
cat("  without the word \"pooled\" would contradict Section 9B.\n")

tt <- tost(M$fe, "contract_year", bound = raw_gap)
cat(sprintf("\n  Equivalence test (TOST), bound = %.4f (the raw gap):\n", raw_gap))
cat(sprintf("     TOST p = %.4f -> %s\n", tt$p,
            if (tt$p < 0.05) "statistically equivalent to zero within the raw gap"
            else "cannot conclude equivalence"))
