## 03_tables.R -- machine-readable .tex and .csv exports

.need <- 5L
if (!exists("PIPELINE_VERSION") || PIPELINE_VERSION < .need)
  stop("Stale 00_functions.R.\n",
       "  This script needs pipeline version ", .need, "; found ",
       if (exists("PIPELINE_VERSION")) PIPELINE_VERSION else "none", ".\n",
       "  The R/ files are from different downloads. Re-download all five\n",
       "  R/ files together and overwrite, then re-run.")
if (!"cluster" %in% names(formals(felm)))
  stop("Stale felm(): it has no `cluster` argument. Overwrite R/00_functions.R.")

hr("EXPORTING TABLES")

LBL <- c(contract_year = "Contract Year", contract_lead1 = "Contract Lead",
         contract_lag1 = "Contract Lag", fa_elig = "Free Agency",
         arb_elig = "Arbitration", option_any = "Option Year",
         fa_lead1 = "FA Lead", fa_lag1 = "FA Lag",
         mls_yrs = "MLS", mls_yrs_sq = "MLS$^2$",
         mls_raw = "MLS (raw)", mls_raw_sq = "MLS$^2$ (raw)",
         cy_vet = "Contract Year $\\times$ Veteran", veteran = "Veteran",
         exiting_soon = "Exiting soon")

lab_of <- function(tm) if (tm %in% names(LBL)) unname(LBL[tm]) else tm

coef_table <- function(models, terms, digits = 4) {
  body <- lapply(terms, function(tm) {
    cf <- vapply(models, function(m) {
      if (!tm %in% names(m$b)) return(c(NA_real_, NA_real_, NA_real_))
      c(unname(m$b[tm]), unname(m$se[tm]), unname(m$p[tm]))
    }, numeric(3))
    cf <- matrix(cf, nrow = 3)
    list(
      coef = vapply(seq_along(models), function(j) {
        if (is.na(cf[1, j])) return("")
        st <- stars(cf[3, j])
        paste0(formatC(cf[1, j], format = "f", digits = digits),
               if (nzchar(st)) paste0("$^{", st, "}$") else "")
      }, character(1)),
      se = vapply(seq_along(models), function(j)
        if (is.na(cf[2, j])) "" else
          paste0("(", formatC(cf[2, j], format = "f", digits = digits), ")"),
        character(1))
    )
  })
  names(body) <- terms
  body
}

write_tex <- function(file, title, models, terms, mtitles, notes,
                      digits = 4, extra_rows = NULL) {
  body <- coef_table(models, terms, digits)
  k <- length(models)
  L <- c("\\begin{table}[htbp]\\centering",
         paste0("\\caption{", title, "}"),
         paste0("\\begin{tabular}{l", strrep("c", k), "}"),
         "\\toprule",
         paste0(" & ", paste(sprintf("(%d)", seq_len(k)), collapse = " & "), " \\\\"),
         paste0(" & ", paste(mtitles, collapse = " & "), " \\\\"),
         "\\midrule")
  for (tm in terms) {
    L <- c(L,
           paste0(lab_of(tm), " & ", paste(body[[tm]]$coef, collapse = " & "), " \\\\"),
           paste0(" & ", paste(body[[tm]]$se, collapse = " & "), " \\\\"))
  }
  L <- c(L, "\\midrule")
  if (!is.null(extra_rows))
    for (r in names(extra_rows))
      L <- c(L, paste0(r, " & ", paste(extra_rows[[r]], collapse = " & "), " \\\\"))
  L <- c(L,
    paste0("Observations & ",
           paste(formatC(vapply(models, `[[`, numeric(1), "N"),
                         format = "d", big.mark = ","), collapse = " & "), " \\\\"),
    paste0("$R^2$ & ",
           paste(sprintf("%.3f", vapply(models, `[[`, numeric(1), "r2")),
                 collapse = " & "), " \\\\"),
    paste0("Clusters (players) & ",
           paste(formatC(vapply(models, `[[`, numeric(1), "G"),
                         format = "d", big.mark = ","), collapse = " & "), " \\\\"),
    "\\bottomrule",
    paste0("\\multicolumn{", k + 1, "}{p{.9\\linewidth}}{\\footnotesize ",
           notes, "} \\\\"),
    "\\end{tabular}", "\\end{table}")
  writeLines(L, file.path(OUTDIR, file))
  cat("  wrote", file, "\n")
}

NOTE <- paste("Standard errors clustered at the player level in parentheses.",
              "$^*p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")

t1o <- t1
t1o$variable <- c("xwOBA","wOBA","wRC$+$","WAR","PA","MLS")
L <- c("\\begin{table}[htbp]\\centering",
       "\\caption{Summary Statistics by Contract Year Status}",
       "\\begin{tabular}{lcccccc}", "\\toprule",
       " & \\multicolumn{2}{c}{Non-Contract Years} & \\multicolumn{2}{c}{Contract Years} & \\multicolumn{2}{c}{Full Sample} \\\\",
       "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
       " & Mean & SD & Mean & SD & Mean & SD \\\\", "\\midrule")
for (i in seq_len(nrow(t1o))) {
  dg <- if (t1o$variable[i] %in% c("xwOBA","wOBA")) 4 else
    if (t1o$variable[i] %in% c("WAR","MLS")) 3 else 2
  vals <- as.numeric(t1o[i, 2:7])
  L <- c(L, paste0(t1o$variable[i], " & ",
                   paste(formatC(vals, format = "f", digits = dg), collapse = " & "),
                   " \\\\"))
}
L <- c(L, "\\midrule",
       paste0("Observations & \\multicolumn{2}{c}{",
              format(sum(d$contract_year == 0), big.mark = ","),
              "} & \\multicolumn{2}{c}{", format(sum(d$contract_year == 1), big.mark = ","),
              "} & \\multicolumn{2}{c}{", format(nrow(d), big.mark = ","), "} \\\\"),
       "\\bottomrule",
       "\\multicolumn{7}{p{.9\\linewidth}}{\\footnotesize MLB batters with at least 200 plate appearances, 2015--2025. MLS is service time in decimal years (172-day service year).} \\\\",
       "\\end{tabular}", "\\end{table}")
writeLines(L, file.path(OUTDIR, "table1_summary.tex"))
cat("  wrote table1_summary.tex\n")

write_tex("table2_main.tex", "Contract Year Effects on xwOBA",
          list(M$ols, M$fe), c("contract_year","mls_yrs","mls_yrs_sq"),
          c("Pooled OLS","Player FE"),
          paste(NOTE, "All models include season fixed effects."),
          extra_rows = list("Season FE" = c("Yes","Yes"),
                            "Player FE" = c("No","Yes")))

write_tex("table3_leadlag.tex",
          "Contract Year Effects on xwOBA: Lead/Lag Specification",
          list(M$fe_ll), c("contract_lead1","contract_year","contract_lag1",
                           "mls_yrs","mls_yrs_sq"),
          "Player FE (Lead/Lag)",
          paste(NOTE, "Lead and lag constructed unconditionally; coefficients",
                "interpreted conditional on one another. Joint test that all three",
                sprintf("equal zero: $F(%d,%d)=%.2f$, $p=%.2f$.",
                        w_ll$df1, w_ll$df2, w_ll$F, w_ll$p)))

write_tex("table4_robustness.tex",
          "Robustness: COVID Exclusion and Alternative Outcomes",
          list(M$fe, M$fe_nocovid, M$fe_woba, M$fe_wrcplus, M$fe_war),
          "contract_year",
          c("xwOBA","xwOBA (no 2020)","wOBA","wRC$+$","WAR"),
          paste(NOTE, "All models include player and season fixed effects and MLS",
                "in quadratic form."))

write_tex("table5_mlscoding.tex", "Robustness: Service Time Coding",
          list(M$fe, M$fe_rawmls), "contract_year",
          c("Converted MLS","Raw years.days MLS"),
          paste(NOTE, "Column (1) uses service time in decimal years (172-day",
                "service year); column (2) uses the raw years.days decimal."))

write_tex("tableA1_consolidated.tex", "Consolidated Results",
          list(M$ols, M$fe, M$fe_ll, M$fe_nocovid),
          c("contract_year","contract_lead1","contract_lag1","mls_yrs","mls_yrs_sq"),
          c("OLS","Player FE","Lead/Lag FE","FE (no 2020)"),
          paste(NOTE, "Season fixed effects suppressed for brevity."))

write_tex("table6_bytype.tex", "Contract Year Effects by Contract Type",
          list(M$fe, M$fe_type, M$fe_fall),
          c("contract_year","fa_elig","arb_elig","option_any","fa_lead1","fa_lag1"),
          c("Pooled","By Type","FA Lead/Lag"),
          paste(NOTE, "Option Year pools club, player, mutual, and vesting options,",
                "which are individually too rare to identify.",
                sprintf("Joint test that all three types equal zero: $p=%.2f$;", w_type$p),
                sprintf("test of equality across types: $p=%.2f$.", w_eq$p)))

L <- c("\\begin{table}[htbp]\\centering",
       "\\caption{Minimum Detectable Effects on xwOBA}",
       "\\begin{tabular}{lccc}", "\\toprule",
       "Specification & SE & MDE & MDE (SD units) \\\\", "\\midrule")
for (i in seq_len(nrow(RES$mde)))
  L <- c(L, sprintf("%s & %.4f & %.4f & %.3f \\\\", RES$mde$spec[i],
                    RES$mde$se[i], RES$mde$mde[i], RES$mde$mde_sd[i]))
L <- c(L, "\\bottomrule",
       sprintf("\\multicolumn{4}{p{.8\\linewidth}}{\\footnotesize MDE computed as $2.80\\times$SE (80\\%% power, 5\\%% two-sided). The standard deviation of xwOBA is %.4f.} \\\\", sd_x),
       "\\end{tabular}", "\\end{table}")
writeLines(L, file.path(OUTDIR, "table8_mde.tex"))
cat("  wrote table8_mde.tex\n")

ledger <- do.call(rbind, lapply(names(RES), function(nm) {
  x <- RES[[nm]]
  if (!"term" %in% names(x)) return(NULL)
  data.frame(block = nm, x[, c("term","b","se","p","N","G","r2")],
             stringsAsFactors = FALSE)
}))
write_csv_utf8(ledger, file.path(OUTDIR, "results_ledger.csv"))
write_csv_utf8(RES$mde, file.path(OUTDIR, "table8_mde.csv"))
cat("  wrote results_ledger.csv (", nrow(ledger), "rows)\n")
