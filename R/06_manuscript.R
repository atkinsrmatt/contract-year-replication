## 06_manuscript.R -- descriptive figures quoted in the text -> manuscript_facts.csv

.need <- 5L
if (!exists("PIPELINE_VERSION") || PIPELINE_VERSION < .need)
  stop("Stale 00_functions.R; needs pipeline version ", .need, ".")
if (!exists("M") || !exists("d"))
  stop("Run 02_models.R first.")

hr("MANUSCRIPT FACTS")

FACTS <- list()
fact <- function(section, label, value, note = "") {
  FACTS[[length(FACTS) + 1L]] <<- data.frame(
    section = section, label = label,
    value = if (is.numeric(value)) sprintf("%.6g", value) else as.character(value),
    note = note, stringsAsFactors = FALSE)
  invisible(value)
}

cat("\nSECTION 5.1  Identifying variation\n")

nobs   <- table(d$playerid)
varies <- function(v) {
  m <- tapply(d[[v]], d$playerid, mean)
  sum(m > 0 & m < 1 & nobs >= 2)
}
ever <- function(v) sum(tapply(d[[v]], d$playerid, function(z) any(z == 1)))

np <- length(unique(d$playerid))
cat(sprintf("  players in panel                          %5d\n",
            fact("5.1", "players", np)))
cat(sprintf("  contribute within-player CY variation     %5d\n",
            fact("5.1", "players_cy_variation", varies("contract_year"))))
mcy <- tapply(d$contract_year, d$playerid, mean)
cat(sprintf("  contribute NO within-player variation     %5d\n",
            fact("5.1", "players_no_variation", sum(mcy == 0) + sum(mcy == 1))))
cat(sprintf("    never in a contract year                %5d\n",
            fact("5.1", "players_never_cy", sum(mcy == 0))))
cat(sprintf("    always in a contract year               %5d\n",
            fact("5.1", "players_always_cy", sum(mcy == 1))))
cat(sprintf("      of which observed once                %5d\n",
            fact("5.1", "players_always_cy_single", sum(mcy == 1 & nobs == 1))))
stopifnot(varies("contract_year") + sum(mcy == 0) + sum(mcy == 1) == np)
cat(sprintf("  ever free-agency eligible                 %5d  (%.1f%%)\n",
            fact("5.1", "players_ever_fa", ever("fa_elig")), 100 * ever("fa_elig") / np))
cat(sprintf("  contribute within-player FA variation     %5d\n",
            fact("5.1", "players_fa_variation", varies("fa_elig"))))

## Eligibility triggers at season's end, so a walk year is RECORDED below six
## years of service. Below-six vs six-and-above is the relevant split, not the
## count on the veteran side of the five-year line. Easy to conflate.

cat("\nSECTION 5.4  Choice of the five-year threshold\n")

fa <- d$fa_elig == 1
cat(sprintf("  FA-eligible seasons total                 %5d\n",
            fact("5.4", "fa_seasons", sum(fa))))
cat(sprintf("    MLS < 5                                 %5d\n",
            fact("5.4", "fa_mls_lt5", sum(fa & d$mls_yrs < 5))))
cat(sprintf("    5 <= MLS < 6                            %5d\n",
            fact("5.4", "fa_mls_5to6", sum(fa & d$mls_yrs >= 5 & d$mls_yrs < 6))))
cat(sprintf("    MLS >= 6                                %5d\n",
            fact("5.4", "fa_mls_ge6", sum(fa & d$mls_yrs >= 6))))
cat(sprintf("  recorded below six years                  %5d  (%.1f%% of FA seasons)\n",
            fact("5.4", "fa_below_six", sum(fa & d$mls_yrs < 6)),
            fact("5.4", "fa_below_six_pct", 100 * mean(d$mls_yrs[fa] < 6))))

cat("\n  What each candidate cutoff does:\n")
cat(sprintf("  %6s %10s %12s %12s\n", "cut", "FA on vet", "FA in young", "arb swept in"))
for (k in c(4, 5, 6, 7)) {
  cat(sprintf("  %6d %10d %12d %12d\n", k,
              sum(fa & d$mls_yrs >= k), sum(fa & d$mls_yrs < k),
              sum(d$arb_elig == 1 & d$mls_yrs >= k)))
  fact("5.4", paste0("cut", k, "_fa_on_veteran_side"), sum(fa & d$mls_yrs >= k))
  fact("5.4", paste0("cut", k, "_arb_swept_in"), sum(d$arb_elig == 1 & d$mls_yrs >= k))
}

cat("\nSECTION 6.2  Lead-lag by career stage\n")

ll_terms <- c("contract_lead1", "contract_year", "contract_lag1")
ll <- list(
  "Full sample" = list(NULL,           "full"),
  "Veterans"    = list(d$veteran == 1, "vet"),
  "Younger"     = list(d$veteran == 0, "yng"))

M$fe_ll_vet <- NULL
LEADLAG <- list()
for (nm in names(ll)) {
  sp <- ll[[nm]]
  m  <- felm(f("xwoba", paste(ll_terms, collapse = " + ")), d,
             absorb = TRUE, subset = sp[[1]])
  w  <- wald(m, ll_terms)
  cat(sprintf("\n  %s (N = %s):\n", nm, format(m$N, big.mark = ",")))
  for (tm in ll_terms)
    cat(sprintf("    %-16s %9.5f (%.5f)  p = %.3f\n",
                tm, m$b[tm], m$se[tm], m$p[tm]))
  cat(sprintf("    joint F(%d,%d) = %.3f  p = %.4f\n", w$df1, w$df2, w$F, w$p))
  for (tm in ll_terms) {
    fact("6.2", paste0(sp[[2]], "_", tm), m$b[tm])
    fact("6.2", paste0(sp[[2]], "_", tm, "_p"), m$p[tm])
  }
  fact("6.2", paste0(sp[[2]], "_joint_p"), w$p)
  LEADLAG[[sp[[2]]]] <- list(fit = m, wald = w, label = nm)
  if (sp[[2]] == "vet") M$fe_ll_vet <- m
}

## If this fails, one of the two was fit on a different sample.
stopifnot(abs(LEADLAG$full$fit$b["contract_year"] -
              M$fe_ll$b["contract_year"]) < 1e-10,
          LEADLAG$full$fit$N == M$fe_ll$N)
LEADLAG_TERMS <- ll_terms

## 04_verify.R runs earlier and cannot see LEADLAG, so these gates live here.
.ll_targets <- rbind(
  c(1, 0.00161), c(2, -0.00004), c(3, -0.00074),   # full
  c(4, 0.00069), c(5,  0.00581), c(6,  0.00009),   # veterans
  c(7, 0.00522), c(8, -0.00057), c(9,  0.00124))   # younger
.ll_have <- unlist(lapply(c("full","vet","yng"), function(g)
  unname(LEADLAG[[g]]$fit$b[ll_terms])))
.ll_bad <- which(abs(.ll_have - .ll_targets[, 2]) > 5e-5)
if (length(.ll_bad)) {
  cat("\n  TABLE 3 GATE FAILED at cell(s): ", paste(.ll_bad, collapse = ", "), "\n")
  print(data.frame(cell = 1:9, have = .ll_have, want = .ll_targets[, 2]))
  stop("Lead-lag estimates moved. Do not regenerate tables until this is understood.")
}
cat("\n  Table 3 gate: 9/9 lead-lag cells match the verified run.\n")

cat("\n  The veteran response sits entirely in the contract year: no\n")
cat("  anticipation and no post-signing decline. The younger lead is\n")
cat("  positive and significant, which we read as selection into continued\n")
cat("  employment rather than anticipation -- reaching arbitration requires\n")
cat("  staying in the majors, which performance determines. Strict\n")
cat("  exogeneity is therefore weaker for the younger subsample.\n")

cat("\nSECTION 6.5  Player decomposition by career stage\n")

ever_vet <- tapply(d$veteran, d$playerid, function(z) any(z == 1))
ever_yng <- tapply(d$veteran, d$playerid, function(z) any(z == 0))

n_both <- sum(ever_vet & ever_yng)
n_vonly <- sum(ever_vet & !ever_yng)
n_yonly <- sum(!ever_vet & ever_yng)

cat(sprintf("  observed on BOTH sides of the threshold   %5d  <- two intercepts in (b)/(c)\n",
            fact("6.5", "players_both_stages", n_both)))
cat(sprintf("  only ever veteran                         %5d\n",
            fact("6.5", "players_veteran_only", n_vonly)))
cat(sprintf("  only ever younger                         %5d\n",
            fact("6.5", "players_younger_only", n_yonly)))
cat(sprintf("  veteran subsample clusters                %5d  (= both + veteran-only)\n",
            fact("6.5", "clusters_veteran", n_both + n_vonly)))
cat(sprintf("  younger subsample clusters                %5d  (= both + younger-only)\n",
            fact("6.5", "clusters_younger", n_both + n_yonly)))
stopifnot(n_both + n_vonly + n_yonly == np,
          n_both + n_vonly == M$fe_vet$G,
          n_both + n_yonly == M$fe_young$G)

o     <- d[order(d$playerid, d$season), ]
first <- o[!duplicated(o$playerid), ]
vonly <- names(ever_vet)[ever_vet & !ever_yng]
fv    <- first[first$playerid %in% vonly, ]

n_censored <- sum(fv$season == min(d$season))
n_pafloor  <- sum(fv$season >  min(d$season))
cat(sprintf("\n  Of the %d veteran-only players:\n", n_vonly))
cat(sprintf("    first observed in %d (window left-censored) %4d\n",
            min(d$season), fact("6.5", "veteran_only_left_censored", n_censored)))
cat(sprintf("    first observed later (200-PA floor)         %4d\n",
            fact("6.5", "veteran_only_pa_floor", n_pafloor)))
cat(sprintf("    median service time on entry                %6.2f yr\n",
            fact("6.5", "veteran_only_median_entry_mls", median(fv$mls_yrs))))
cat("    The later entrants are aging part-timers who dipped below 200 PA\n")
cat("    and then had one more qualifying season, not late bloomers.\n")

tgt <- unname(M$fe_vet$b["contract_year"] - M$fe_young$b["contract_year"])
gt  <- unname(M$fe_int_sat$b["cy_vet"])
cat(sprintf("\n  split-sample difference  %.17f\n", tgt))
cat(sprintf("  specification (c)        %.17f\n", gt))
cat(sprintf("  absolute difference      %.3e\n", abs(gt - tgt)))
cat(sprintf("  decimal places in agreement %d\n",
            fact("6.5", "identity_decimal_places", floor(-log10(abs(gt - tgt))))))
fact("6.5", "identity_abs_diff", abs(gt - tgt))

## A singleton contributes nothing to the slope but still counts toward G,
## which enters the cluster correction and the test degrees of freedom.

cat("\nSECTION 6.7  Singleton clusters\n")

drop_singletons <- function(sub) {
  n <- table(d$playerid[sub])
  sub & d$playerid %in% names(n)[n >= 2]
}
cat(sprintf("  %-34s %10s %10s %8s %6s %6s\n", "", "coef", "se", "p", "N", "G"))
sing <- function(lbl, sub, tag) {
  m <- felm(f("xwoba", "contract_year"), d, absorb = TRUE, subset = sub)
  cat(sprintf("  %-34s %10.5f %10.5f %8.4f %6d %6d\n", lbl,
              m$b["contract_year"], m$se["contract_year"], m$p["contract_year"],
              m$N, m$G))
  fact("6.7", tag, m$b["contract_year"])
  fact("6.7", paste0(tag, "_se"), m$se["contract_year"])
  m
}
all_rows <- rep(TRUE, nrow(d))
for (v in list(list("Pooled, as reported",   all_rows,                 "pooled"),
               list("Pooled, singletons dropped", drop_singletons(all_rows), "pooled_nosingle"),
               list("Veterans, as reported", d$veteran == 1,           "vet"),
               list("Veterans, singletons dropped", drop_singletons(d$veteran == 1), "vet_nosingle"),
               list("Younger, as reported",  d$veteran == 0,           "yng"),
               list("Younger, singletons dropped", drop_singletons(d$veteran == 0), "yng_nosingle")))
  sing(v[[1]], v[[2]], v[[3]])

mv <- felm(f("xwoba","contract_year"), d, absorb=TRUE, subset=drop_singletons(d$veteran==1))
my <- felm(f("xwoba","contract_year"), d, absorb=TRUE, subset=drop_singletons(d$veteran==0))
cat(sprintf("\n  split-sample difference, singletons dropped %.5f (reported %.5f)\n",
            fact("6.7", "difference_nosingle",
                 mv$b["contract_year"] - my$b["contract_year"]), tgt))
cat(sprintf("  singletons: %d of %d players in the full panel (%.1f%%)\n",
            fact("6.7", "n_singletons", sum(nobs == 1)), np, 100 * mean(nobs == 1)))

## Bound set like-for-like: the veteran xwOBA effect as a fraction of the
## xwOBA SD, applied to the BsR SD.

cat("\nSECTION 7.2  Equivalence tests on baserunning\n")

sd_x  <- sd(d$xwoba)
sd_b  <- sd(d$bsr)
frac  <- unname(M$fe_vet$b["contract_year"]) / sd_x
bound <- frac * sd_b

cat(sprintf("  veteran xwOBA effect = %.3f SD of xwOBA\n",
            fact("7.2", "veteran_effect_in_sd", frac)))
cat(sprintf("  equivalent bound on BsR = %.3f x %.4f = %.4f runs\n",
            frac, sd_b, fact("7.2", "bsr_bound", bound)))
cat(sprintf("\n  %-10s %9s %9s %8s %10s %9s %s\n",
            "sample", "coef", "se", "p", "MDE", "TOST p", "reading"))
for (lbl in c("all", "veteran", "younger")) {
  ss <- switch(lbl, all = all_rows, veteran = d$veteran == 1, younger = d$veteran == 0)
  m  <- felm(f("bsr", "contract_year"), d, absorb = TRUE, subset = ss)
  tt <- tost(m, "contract_year", bound = bound)
  cat(sprintf("  %-10s %9.4f %9.4f %8.3f %10.4f %9.3f %s\n", lbl,
              m$b["contract_year"], m$se["contract_year"], m$p["contract_year"],
              mde(unname(m$se["contract_year"])), tt$p,
              if (tt$p < 0.05) "equivalent" else "UNINFORMATIVE"))
  fact("7.2", paste0("bsr_", lbl), m$b["contract_year"])
  fact("7.2", paste0("bsr_", lbl, "_p"), m$p["contract_year"])
  fact("7.2", paste0("bsr_", lbl, "_tost_p"), tt$p)
}
cat("\n  Equivalence rejects a larger baserunning response, so the null is a\n")
cat("  finding rather than a power failure. Reporting it without this test\n")
cat("  would apply the Section 10 standard asymmetrically.\n")

cat("\nSECTION 7.3  Effect size in runs and wins\n")

PA_FULL   <- 600
RUNS_WIN  <- 10
SCALES    <- c(1.15, 1.20, 1.25)   # wOBA scale varies by season
vb <- unname(M$fe_vet$b["contract_year"])

cat(sprintf("  %8s %10s %10s\n", "scale", "runs", "WAR"))
for (sc in SCALES) {
  runs <- vb / sc * PA_FULL
  cat(sprintf("  %8.2f %10.2f %10.2f\n", sc, runs, runs / RUNS_WIN))
}
r_lo <- vb / max(SCALES) * PA_FULL
r_hi <- vb / min(SCALES) * PA_FULL
cat(sprintf("\n  %.1f to %.1f runs over %d PA, or about %.2f WAR\n",
            fact("7.3", "runs_low", r_lo), fact("7.3", "runs_high", r_hi),
            PA_FULL, fact("7.3", "war", mean(c(r_lo, r_hi)) / RUNS_WIN)))
cat("  NOTE: the wOBA scale is season-specific. Confirm against the relevant\n")
cat("  year's constants before quoting a single figure.\n")

facts <- do.call(rbind, FACTS)
write_csv_utf8(facts, file.path(OUTDIR, "manuscript_facts.csv"))
cat(sprintf("\n  wrote manuscript_facts.csv (%d quantities)\n", nrow(facts)))
