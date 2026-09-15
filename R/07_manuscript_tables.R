## 07_manuscript_tables.R -- Tables 1-8 -> tables_manuscript.tex, ready to
## \input. Plain \noindent\textbf{Table N}, so the preamble needs nothing new.

if (!exists("M") || !exists("LADDER"))
  stop("Run 02_models.R and 05_table7.R first.")

hr("MANUSCRIPT-STYLE TABLES")

nm4 <- function(x, dg = 4) {
  s <- formatC(x, format = "f", digits = dg)
  if (grepl("^-", s)) paste0("$-$", sub("^-", "", s)) else s
}
st  <- function(p) if (is.na(p)) "" else
  if (p < .01) "***" else if (p < .05) "**" else if (p < .10) "*" else ""
cf  <- function(m, t, dg = 4) paste0(nm4(unname(m$b[t]), dg), st(unname(m$p[t])))
se  <- function(m, t, dg = 4) paste0("(", formatC(unname(m$se[t]), format = "f", digits = dg), ")")
pv  <- function(p) sub("^0", "", formatC(p, format = "f", digits = 3))
r2  <- function(m) formatC(m$r2, format = "f", digits = 3)
NN  <- function(m) formatC(m$N, format = "d", big.mark = ",")
GG  <- function(m) formatC(m$G, format = "d", big.mark = ",")

## Leading {} is required: \\ skips whitespace looking for its optional *, so
## "\\" followed by "*$p<.10$" parses as \\* and swallows the first star.
STARS <- "{}*$p<.10$. **$p<.05$. ***$p<.01$."
L <- character(0)
add <- function(...) L <<- c(L, ...)

## Blank lines matter: \vspace does not end a paragraph, so without them the
## tabular runs inline with the title.
head_tbl <- function(n, title, gap = "0.4in") {
  add("", sprintf("\\vspace{%s}", gap), "",
      sprintf("\\noindent\\textbf{Table %s}\\\\", n),
      sprintf("\\textit{%s}", title), "",
      "\\vspace{4pt}", "", "\\begin{center}")
}
end_tbl <- function() add("\\end{center}", "")
note <- function(txt, stars = TRUE) {
  add("\\vspace{-6pt}", "",
      paste0("{\\footnotesize \\textit{Note.} ", txt,
             if (stars) "\\\\" else ""),
      if (stars) paste0(STARS, "}") else "}", "")
}

## Table 1
head_tbl(1, "Summary Statistics by Contract Year Status", "0.2in")
add("\\begin{tabular}{lcccccc}", "\\toprule",
    "& \\multicolumn{2}{c}{Non-Contract Years} & \\multicolumn{2}{c}{Contract Years}",
    "& \\multicolumn{2}{c}{Full Sample}\\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
    "Variable & $M$ & $SD$ & $M$ & $SD$ & $M$ & $SD$\\\\", "\\midrule")
t1lab <- c(xwoba = "xwOBA", woba = "wOBA", wrcplus = "wRC+",
           war = "WAR", pa = "PA", mls_yrs = "MLS")
t1dig <- c(xwoba = 4, woba = 4, wrcplus = 2, war = 3, pa = 2, mls_yrs = 3)
for (v in names(t1lab)) {
  s0 <- d[[v]][d$contract_year == 0]; s1 <- d[[v]][d$contract_year == 1]
  vals <- c(mean(s0), sd(s0), mean(s1), sd(s1), mean(d[[v]]), sd(d[[v]]))
  add(sprintf("%s & %s\\\\", t1lab[v],
      paste(formatC(vals, format = "f", digits = t1dig[v]), collapse = " & ")))
}
add("\\midrule",
    sprintf("Observations & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s}\\\\",
            formatC(sum(d$contract_year == 0), format = "d", big.mark = ","),
            formatC(sum(d$contract_year == 1), format = "d", big.mark = ","),
            formatC(nrow(d), format = "d", big.mark = ",")),
    "\\bottomrule", "\\end{tabular}")
end_tbl()
note(paste("The sample comprises MLB batters with at least 200 plate appearances,",
           "2015--2025. MLS denotes major league service time in decimal years,",
           "converted using a 172-day service year."), stars = FALSE)

## Table 2
head_tbl(2, "Contract Year Effects on xwOBA")
add("\\begin{tabular}{lcc}", "\\toprule",
    "Variable & (1) Pooled OLS & (2) Player FE\\\\", "\\midrule")
t2lab <- c(contract_year = "Contract year", mls_yrs = "MLS", mls_yrs_sq = "MLS squared")
for (k in names(t2lab))
  add(sprintf("%s & %s & %s\\\\", t2lab[[k]], cf(M$ols, k), cf(M$fe, k)),
      sprintf(" & %s & %s\\\\", se(M$ols, k), se(M$fe, k)))
add("\\midrule", "Season FE & Yes & Yes\\\\", "Player FE & No & Yes\\\\",
    sprintf("Observations & %s & %s\\\\", NN(M$ols), NN(M$fe)),
    sprintf("$R^2$ & %s & %s\\\\", r2(M$ols), r2(M$fe)),
    "\\bottomrule", "\\end{tabular}")
end_tbl()
note(paste0("Standard errors clustered at the player level (", GG(M$fe),
            " clusters) in parentheses. The $R^2$ in column 2 is the within-player $R^2$."))

## Table 3
if (!exists("LEADLAG")) stop("Run 06_manuscript.R first (LEADLAG is built there).")

t3 <- LEADLAG[c("full", "vet", "yng")]
t3m <- lapply(t3, `[[`, "fit")
t3w <- lapply(t3, `[[`, "wald")

head_tbl(3, "Contract Year Effects on xwOBA by Career Stage: Lead-Lag Specification")
add("\\begin{tabular}{lccc}", "\\toprule",
    "Variable & (1) Full sample & (2) Veterans & (3) Younger players\\\\",
    "\\midrule")
t3lab <- c(contract_lead1 = "Contract year lead",
           contract_year  = "Contract year",
           contract_lag1  = "Contract year lag")
for (k in names(t3lab)) {
  add(sprintf("%s & %s\\\\", t3lab[[k]],
              paste(vapply(t3m, function(m) cf(m, k), ""), collapse = " & ")),
      sprintf(" & %s\\\\",
              paste(vapply(t3m, function(m) se(m, k), ""), collapse = " & ")))
}
add("\\midrule",
    sprintf("Joint $F$-test & %s\\\\",
            paste(vapply(t3w, function(w)
              sprintf("$F$(%d, %s) = %s", w$df1,
                      formatC(w$df2, format = "d", big.mark = ","),
                      formatC(w$F, format = "f", digits = 3)), ""),
              collapse = " & ")),
    sprintf("Joint $F$-test $p$-value & %s\\\\",
            paste(vapply(t3w, function(w) pv(w$p), ""), collapse = " & ")),
    "\\midrule",
    sprintf("Observations & %s\\\\", paste(vapply(t3m, NN, ""), collapse = " & ")),
    sprintf("Players & %s\\\\",      paste(vapply(t3m, GG, ""), collapse = " & ")),
    sprintf("Within $R^2$ & %s\\\\", paste(vapply(t3m, r2, ""), collapse = " & ")),
    "\\bottomrule", "\\end{tabular}")
end_tbl()
note(paste0("Standard errors clustered at the player level in parentheses. All ",
            "columns include player and season fixed effects and service time in ",
            "quadratic form. Veterans are player-seasons with at least ",
            VET_CUTOFF, " years of major league service; younger players are the ",
            "complement. Lead and lag indicators are constructed unconditionally; ",
            "coefficients are interpreted conditional on one another. The joint test ",
            "evaluates the hypothesis that the lead, contract year, and lag ",
            "coefficients are jointly zero. Columns 2 and 3 partition the sample in ",
            "column 1, so the two career stages are estimated on disjoint ",
            "player-seasons; players observed on both sides of the threshold ",
            "contribute to both columns."))

## Table 4
head_tbl(4, "Robustness: COVID Exclusion and Alternative Outcomes")
t4 <- list(M$fe, M$fe_nocovid, M$fe_woba, M$fe_wrcplus, M$fe_war)
add("\\begin{tabular}{lccccc}", "\\toprule",
    "Variable & (1) xwOBA & (2) xwOBA excl. 2020 & (3) wOBA & (4) wRC+ & (5) WAR\\\\",
    "\\midrule",
    sprintf("Contract year & %s\\\\",
            paste(vapply(t4, function(m) cf(m, "contract_year"), ""), collapse = " & ")),
    sprintf(" & %s\\\\",
            paste(vapply(t4, function(m) se(m, "contract_year"), ""), collapse = " & ")),
    "\\midrule",
    sprintf("Observations & %s\\\\", paste(vapply(t4, NN, ""), collapse = " & ")),
    sprintf("Within $R^2$ & %s\\\\", paste(vapply(t4, r2, ""), collapse = " & ")),
    "\\bottomrule", "\\end{tabular}")
end_tbl()
note(paste("Standard errors clustered at the player level in parentheses. All models",
           "include player and season fixed effects and service time in quadratic form."))

## Table 5
head_tbl(5, "Robustness: Service Time Coding")
t5 <- list(M$fe, M$fe_rawmls)
add("\\begin{tabular}{lcc}", "\\toprule",
    "Variable & (1) Converted MLS & (2) Raw years.days MLS\\\\", "\\midrule",
    sprintf("Contract year & %s\\\\",
            paste(vapply(t5, function(m) cf(m, "contract_year"), ""), collapse = " & ")),
    sprintf(" & %s\\\\",
            paste(vapply(t5, function(m) se(m, "contract_year"), ""), collapse = " & ")),
    "\\midrule",
    sprintf("Observations & %s\\\\", paste(vapply(t5, NN, ""), collapse = " & ")),
    sprintf("Within $R^2$ & %s\\\\", paste(vapply(t5, r2, ""), collapse = " & ")),
    "\\bottomrule", "\\end{tabular}")
end_tbl()
note(paste("Standard errors clustered at the player level in parentheses. Both models",
           "include player and season fixed effects. Column 1 converts service time to",
           "decimal years using a 172-day service year; column 2 uses the raw recorded",
           "decimal."))

## Table 6
head_tbl(6, "Contract Year Effects by Contract Type, Full Sample")
blank <- function(m, k) if (k %in% names(m$b)) c(cf(m, k), se(m, k)) else c("", "")
r6 <- function(lab, k) {
  a <- blank(M$fe, k); b <- blank(M$fe_type, k); c3 <- blank(M$fe_fall, k)
  add(sprintf("%s & %s & %s & %s\\\\", lab, a[1], b[1], c3[1]),
      sprintf(" & %s & %s & %s\\\\", a[2], b[2], c3[2]))
}
add("\\begin{tabular}{lccc}", "\\toprule",
    "Variable & (1) Pooled & (2) By type & (3) FA lead-lag\\\\", "\\midrule")
r6("Contract year", "contract_year")
r6("Free agency", "fa_elig")
r6("Arbitration", "arb_elig")
r6("Option year", "option_any")
r6("Free agency lead", "fa_lead1")
r6("Free agency lag", "fa_lag1")
add("\\midrule",
    sprintf("Observations & %s & %s & %s\\\\", NN(M$fe), NN(M$fe_type), NN(M$fe_fall)),
    sprintf("Within $R^2$ & %s & %s & %s\\\\", r2(M$fe), r2(M$fe_type), r2(M$fe_fall)),
    "\\bottomrule", "\\end{tabular}")
end_tbl()
note(sprintf(paste("Standard errors clustered at the player level in parentheses. All",
                   "models include player and season fixed effects and service time in",
                   "quadratic form. Option year aggregates club, player, mutual, and",
                   "vesting options. A joint test that the three contract type",
                   "coefficients equal zero yields $p=%s$; a test of equality across",
                   "types yields $p=%s$."), pv(w_type$p), pv(w_eq$p)))

## Table 7
head_tbl(7, "Contract Year Effects by Career Stage")
C <- list(M$fe, M$fe_vet, M$fe_young,
          LADDER[["(a) common player FE"]]$xtreg,
          LADDER[["(b) player x career-stage FE"]]$xtreg,
          LADDER[["(c) + controls interacted"]]$xtreg)
A <- list(LADDER[["(a) common player FE"]]$areg,
          LADDER[["(b) player x career-stage FE"]]$areg,
          LADDER[["(c) + controls interacted"]]$areg)
tgt <- unname(M$fe_vet$b["contract_year"] - M$fe_young$b["contract_year"])
gotc <- unname(C[[6]]$b["cy_vet"])

row7 <- function(lab, k, dash = integer(0), absorbed = integer(0)) {
  v <- vapply(seq_along(C), function(j)
    if (j %in% dash) "---" else if (j %in% absorbed) "absorbed" else cf(C[[j]], k), "")
  e <- vapply(seq_along(C), function(j)
    if (j %in% c(dash, absorbed)) "" else se(C[[j]], k), "")
  add(sprintf("%s & %s\\\\", lab, paste(v, collapse = " & ")),
      sprintf(" & %s\\\\", paste(e, collapse = " & ")))
}
add("{\\footnotesize", "\\setlength{\\tabcolsep}{4.5pt}",
    "\\begin{tabular}{lcccccc}", "\\toprule",
    "& & \\multicolumn{2}{c}{Split sample} & \\multicolumn{3}{c}{Interaction ladder}\\\\",
    "\\cmidrule(lr){3-4}\\cmidrule(lr){5-7}",
    "Variable & (1) & (2) & (3) & (4) & (5) & (6)\\\\",
    " & Pooled & Veterans & Younger & (a) & (b) & (c)\\\\", "\\midrule")
row7("Contract year", "contract_year")
row7("Contract year $\\times$ veteran", "cy_vet", dash = 1:3)
row7("Veteran", "veteran", dash = 1:3, absorbed = 5:6)
add("\\midrule",
    "Player FE & Yes & Yes & Yes & Yes & --- & ---\\\\",
    "Player $\\times$ career-stage FE & No & --- & --- & No & Yes & Yes\\\\",
    "Controls $\\times$ career stage & No & --- & --- & No & No & Yes\\\\",
    "Season FE $\\times$ career stage & No & --- & --- & No & No & Yes\\\\",
    "\\midrule",
    sprintf("Observations & %s\\\\", paste(vapply(C, NN, ""), collapse = " & ")),
    sprintf("Players & %s\\\\", paste(vapply(C, GG, ""), collapse = " & ")),
    sprintf("Within $R^2$ & %s\\\\", paste(vapply(C, r2, ""), collapse = " & ")),
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{Difference in the contract year effect by career stage}}\\\\",
    sprintf("\\multicolumn{4}{l}{\\quad Split sample, column (2) $-$ column (3)} & \\multicolumn{3}{c}{%s}\\\\",
            formatC(tgt, format = "f", digits = 5)),
    sprintf("\\multicolumn{4}{l}{\\quad Interaction, column (6)} & \\multicolumn{3}{c}{%s}\\\\",
            formatC(gotc, format = "f", digits = 5)),
    sprintf("\\multicolumn{4}{l}{\\quad Cluster bootstrap, 95\\%% interval} & \\multicolumn{3}{c}{[%s, %s]}\\\\",
            formatC(ci[1], format = "f", digits = 5),
            formatC(ci[2], format = "f", digits = 5)),
    "\\bottomrule", "\\end{tabular}", "}")
end_tbl()
note(sprintf(paste(
  "Standard errors clustered at the player level in parentheses. The dependent",
  "variable is xwOBA. Veterans are players entering a season with five or more years",
  "of major league service, who become free agency eligible at season's end. All",
  "models include season fixed effects and service time in quadratic form. Columns 2",
  "and 3 split the sample. Columns 4 through 6 report an interaction ladder in which",
  "each rung relaxes one restriction: specification (a) retains common player fixed",
  "effects and a common aging profile; (b) assigns each player a separate intercept in",
  "each career stage; (c) additionally interacts the service time controls and the",
  "season effects with career stage. Because allowing every parameter to differ by",
  "group is the same estimator as estimating the two groups separately, (c) must",
  "reproduce the split-sample difference, and does. The veteran main effect is",
  "estimable only in (a), where one player intercept must span both career stages and",
  "the aging level shift has nowhere else to go. The bracketed interval is a",
  "%s-replicate cluster bootstrap over players (two-sided $p=%s$), which requires no",
  "residual degrees-of-freedom convention. Under the areg convention the interaction",
  "carries $p$-values of %s, %s, and %s in columns 4 through 6; under xtreg, %s, %s,",
  "and %s. Player fixed effects are nested within player clusters, so the xtreg",
  "convention is the appropriate one here (Cameron \\& Miller, 2015). See Note 5."),
  formatC(BOOT_B, format = "d", big.mark = ","), pv(p_boot),
  pv(A[[1]]$p["cy_vet"]), pv(A[[2]]$p["cy_vet"]), pv(A[[3]]$p["cy_vet"]),
  pv(C[[4]]$p["cy_vet"]), pv(C[[5]]$p["cy_vet"]), pv(C[[6]]$p["cy_vet"])))

## Panel E inputs. Lag matched on (player, season - 1), not row position: the
## panel has gaps, so a positional lag would pair non-adjacent seasons.
if (!"xwoba_lag1" %in% names(d)) {
  .tab <- setNames(d$xwoba, paste(d$playerid, d$season, sep = "_"))
  d$xwoba_lag1 <- unname(.tab[paste(d$playerid, d$season - 1, sep = "_")])
}
.haslag  <- d$veteran == 1 & !is.na(d$xwoba_lag1)
m_lagbase <- felm(f("xwoba", "contract_year"), d, absorb = TRUE, subset = .haslag)
m_lagctl  <- felm(f("xwoba", "contract_year + xwoba_lag1"), d, absorb = TRUE,
                  subset = d$veteran == 1)

## Table 8
head_tbl(8, "Robustness of the Career Stage Result")
add("\\begin{tabular}{lccc}", "\\toprule",
    "Specification & Coefficient & $p$-value & Observations\\\\", "\\midrule")
r8 <- function(lab, m, k = "contract_year", dg = 4) {
  p <- unname(m$p[k])
  add(sprintf("\\quad %s & %s & %s & %s\\\\", lab,
              nm4(unname(m$b[k]), dg),
              if (p < .001) "$<.001$" else pv(p), NN(m)))
}
add("\\multicolumn{4}{l}{\\textit{Panel A. Service time threshold}}\\\\")
panelA <- c("Four or more years", "Five or more years (main)",
            "Six or more years", "Seven or more years")
for (k in c(4, 5, 6, 7))
  r8(panelA[k - 3],
     felm(f("xwoba","contract_year"), d, absorb = TRUE, subset = d$mls_yrs >= k))
add("\\multicolumn{4}{l}{\\textit{Panel B. Alternative outcomes, veterans}}\\\\")
panelB <- c(woba = "wOBA", wrcplus = "wRC+", war = "WAR")
for (dv in names(panelB))
  r8(panelB[[dv]],
     felm(f(dv, "contract_year"), d, absorb = TRUE, subset = d$veteran == 1),
     dg = if (dv == "woba") 4 else 3)
add("\\multicolumn{4}{l}{\\textit{Panel C. Survivorship, veterans}}\\\\")
r8("Players observed five or more seasons",
   felm(f("xwoba","contract_year"), d, absorb = TRUE, subset = d$veteran == 1 & d$n_obs >= 5))
r8("Controlling for imminent exit",
   felm(f("xwoba","contract_year + exiting_soon"), d, absorb = TRUE, subset = d$veteran == 1))
r8("Dropping each player's final season",
   felm(f("xwoba","contract_year"), d, absorb = TRUE, subset = d$veteran == 1 & d$is_last == 0))
add("\\multicolumn{4}{l}{\\textit{Panel D. Contract type, veterans}}\\\\")
m6 <- felm(f("xwoba","fa_elig + option_any"), d, absorb = TRUE, subset = d$veteran == 1)
r8("Free agency", m6, "fa_elig"); r8("Option year", m6, "option_any")
add("\\multicolumn{4}{l}{\\textit{Panel E. Selection into contract year status}}\\\\")
r8("Veterans, prior season observed", m_lagbase)
r8("Adding lagged xwOBA", m_lagctl)
add("\\bottomrule", "\\end{tabular}")
end_tbl()
note(paste("Each row reports the contract year coefficient from a separate regression.",
           "All models include player and season fixed effects, service time in quadratic",
           "form, and standard errors clustered at the player level. The dependent",
           "variable is xwOBA except where noted in Panel B.",
           "Panel E restricts to veteran seasons for which the preceding season also",
           "appears in the panel, which is why its baseline differs from Panel A; the",
           "comparison of interest is between the two rows of Panel E, which share a",
           "sample. The lag is matched on player and season rather than row position.",
           "Adding a lagged dependent variable to a fixed effects model induces Nickell",
           "bias, so the coefficient on the lag itself is not reported or interpreted."),
     stars = FALSE)

writeLines(L, file.path(OUTDIR, "tables_manuscript.tex"))
cat("  wrote tables_manuscript.tex (", length(L), "lines, 8 tables )\n")
cat("  The exit-timing placebo row is NOT emitted; see Section 6.7.\n")
