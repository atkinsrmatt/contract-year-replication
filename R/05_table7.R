## 05_table7.R -- Table 7: contract year effects by career stage

.need <- 5L
if (!exists("PIPELINE_VERSION") || PIPELINE_VERSION < .need)
  stop("Stale 00_functions.R.\n",
       "  This script needs pipeline version ", .need, "; found ",
       if (exists("PIPELINE_VERSION")) PIPELINE_VERSION else "none", ".")
if (!exists("M") || !exists("d") || !exists("LADDER"))
  stop("Run 02_models.R first: this script needs `d`, `M` and `LADDER`.")

hr("TABLE 7: CAREER STAGE")

if (!exists("BOOT_B")) BOOT_B <- 2000L

gv <- function(fit, tm, what)
  if (tm %in% names(fit$b)) unname(fit[[what]][tm]) else NA_real_

cols <- list(M$fe, M$fe_vet, M$fe_young,
             LADDER[["(a) common player FE"]]$xtreg,
             LADDER[["(b) player x career-stage FE"]]$xtreg,
             LADDER[["(c) + controls interacted"]]$xtreg)
areg <- list(LADDER[["(a) common player FE"]]$areg,
             LADDER[["(b) player x career-stage FE"]]$areg,
             LADDER[["(c) + controls interacted"]]$areg)

target <- unname(M$fe_vet$b["contract_year"] - M$fe_young$b["contract_year"])
got    <- gv(cols[[6]], "cy_vet", "b")

## Resampling players sidesteps the xtreg/areg question: a percentile interval
## has no residual DoF to argue about.
.fo <- f("xwoba", "contract_year")
.mf <- model.frame(.fo, d)
.Y  <- model.response(.mf)
.X  <- model.matrix(.fo, .mf)
.X  <- .X[, colnames(.X) != "(Intercept)", drop = FALSE]

beta_only <- function(rows, fe, keep) {
  r <- rows[keep]
  g <- fe[keep]
  if (length(r) < 5L) return(NA_real_)
  g  <- match(g, unique(g))                 # dense 1..G codes
  V  <- cbind(.Y[r], .X[r, , drop = FALSE])
  mu <- rowsum(V, g, reorder = TRUE) / tabulate(g, nbins = max(g))
  V  <- V - mu[g, , drop = FALSE]
  y  <- V[, 1]
  Xd <- V[, -1, drop = FALSE]
  qx <- qr(Xd)
  Xd <- Xd[, sort(qx$pivot[seq_len(qx$rank)]), drop = FALSE]
  if (!"contract_year" %in% colnames(Xd)) return(NA_real_)
  unname(qr.solve(Xd, y)["contract_year"])
}

set.seed(12345)
by_pl   <- split(seq_len(nrow(d)), d$playerid)
lens    <- lengths(by_pl)
players <- names(by_pl)
vet     <- d$veteran

boot <- vapply(seq_len(BOOT_B), function(bb) {
  draw <- sample(players, length(players), replace = TRUE)
  rows <- unlist(by_pl[draw], use.names = FALSE)
  ## a player drawn twice must receive two distinct fixed effects
  fe   <- paste0(rep(seq_along(draw), lens[draw]), "_", vet[rows])
  v <- tryCatch(beta_only(rows, fe, vet[rows] == 1), error = function(e) NA_real_)
  y <- tryCatch(beta_only(rows, fe, vet[rows] == 0), error = function(e) NA_real_)
  v - y
}, numeric(1))

boot   <- boot[is.finite(boot)]
ci     <- unname(quantile(boot, c(0.025, 0.975)))
p_boot <- min(1, 2 * min(mean(boot <= 0), mean(boot >= 0)))

cat("\n  Ladder (cy_vet):\n")
cat(sprintf("  %-30s %10s %10s %8s %8s\n", "", "cy_vet", "se", "p", "p areg"))
for (i in 1:3)
  cat(sprintf("  (%s) %-26s %10.5f %10.5f %8.3f %8.3f\n",
              c("a","b","c")[i],
              c("common player FE","player x career-stage FE",
                "+ controls interacted")[i],
              gv(cols[[i + 3]], "cy_vet", "b"), gv(cols[[i + 3]], "cy_vet", "se"),
              gv(cols[[i + 3]], "cy_vet", "p"), gv(areg[[i]],     "cy_vet", "p")))

cat(sprintf("\n  Split-sample difference (target) %12.7f\n", target))
cat(sprintf("  Specification (c) interaction    %12.7f\n", got))
cat(sprintf("  Absolute discrepancy             %12.2e   %s\n", abs(got - target),
            if (abs(got - target) < 1e-5) "identity holds" else "*** FAILS ***"))
if (abs(got - target) >= 1e-5)
  stop("Specification (c) does not reproduce the split-sample difference.")

cat(sprintf("\n  Cluster bootstrap over players (%d replicates, %d usable):\n",
            BOOT_B, length(boot)))
cat(sprintf("    point estimate     %10.5f\n", target))
cat(sprintf("    bootstrap SE       %10.5f\n", sd(boot)))
cat(sprintf("    95%% percentile CI [%9.5f, %9.5f]\n", ci[1], ci[2]))
cat(sprintf("    two-sided p        %10.4f\n", p_boot))
cat("    (no residual degrees of freedom, so the xtreg/areg convention\n")
cat("     does not arise for this test)\n")

vet_b   <- unname(M$fe_vet$b["contract_year"])
vet_mde <- mde(unname(M$fe_vet$se["contract_year"]))
cat("\n  Veteran subsample power:\n")
cat(sprintf("    coefficient           %8.5f  (%.1f pts)\n", vet_b, vet_b * 1000))
cat(sprintf("    MDE (80%%, 5%% 2-sided) %8.5f  (%.1f pts)\n", vet_mde, vet_mde * 1000))
cat(sprintf("    ratio                 %8.2fx\n", vet_b / vet_mde))
cat("    Report this alongside the pooled MDE. Applying the power standard of\n")
cat("    Section 10 to the pooled null but not to the veteran effect would be\n")
cat("    asymmetric, and a referee will run it.\n")

cell <- function(col, spec, term, fit)
  data.frame(column = col, spec = spec, term = term,
             b = gv(fit, term, "b"), se = gv(fit, term, "se"),
             p = gv(fit, term, "p"), N = fit$N, players = fit$G,
             within_r2 = fit$r2, stringsAsFactors = FALSE)

fills <- rbind(
  cell("(1)", "Pooled",                "contract_year", cols[[1]]),
  cell("(2)", "Veterans",              "contract_year", cols[[2]]),
  cell("(3)", "Younger",               "contract_year", cols[[3]]),
  cell("(4)", "(a) common player FE",  "contract_year", cols[[4]]),
  cell("(4)", "(a) common player FE",  "cy_vet",        cols[[4]]),
  cell("(4)", "(a) common player FE",  "veteran",       cols[[4]]),
  cell("(5)", "(b) player x stage FE", "contract_year", cols[[5]]),
  cell("(5)", "(b) player x stage FE", "cy_vet",        cols[[5]]),
  cell("(6)", "(c) saturated",         "contract_year", cols[[6]]),
  cell("(6)", "(c) saturated",         "cy_vet",        cols[[6]]),
  data.frame(column = "memo", spec = "bootstrap difference", term = "cy_vet",
             b = target, se = sd(boot), p = p_boot, N = cols[[1]]$N,
             players = cols[[1]]$G, within_r2 = NA_real_,
             stringsAsFactors = FALSE))

cat("\n  Every cell in Table 7:\n\n")
print(format(fills, digits = 5), row.names = FALSE)

RES$table7 <- fills
write_csv_utf8(fills, file.path(OUTDIR, "table7_cells.csv"))

## Requires \usepackage{booktabs} and \usepackage[flushleft]{threeparttable}.

fm  <- function(x, dg = 4) if (is.na(x)) "---" else formatC(x, format = "f", digits = dg)
st  <- function(p) { s <- stars(p); if (nzchar(s)) paste0("$^{", s, "}$") else "" }
num <- function(fit, tm) paste0("$", fm(gv(fit, tm, "b")), "$", st(gv(fit, tm, "p")))
ses <- function(fit, tm)
  if (is.na(gv(fit, tm, "se"))) "" else paste0("(", fm(gv(fit, tm, "se")), ")")

rowpair <- function(tm, dash = integer(0), absorbed = integer(0)) {
  v <- vapply(seq_along(cols), function(j)
    if (j %in% dash) "---" else if (j %in% absorbed) "absorbed" else num(cols[[j]], tm),
    character(1))
  e <- vapply(seq_along(cols), function(j)
    if (j %in% c(dash, absorbed)) "" else ses(cols[[j]], tm), character(1))
  c(paste(v, collapse = " & "), paste(e, collapse = " & "))
}

r_cy  <- rowpair("contract_year")
r_int <- rowpair("cy_vet",  dash = 1:3)
r_vet <- rowpair("veteran", dash = 1:3, absorbed = 5:6)

tex <- c(
"\\begin{table}[htbp]\\centering",
"\\begin{threeparttable}",
"\\caption{Contract Year Effects by Career Stage}\\label{tab:career-stage}",
"\\small\\setlength{\\tabcolsep}{4.5pt}",
"\\begin{tabular}{lcccccc}",
"\\toprule",
" & & \\multicolumn{2}{c}{Split sample} & \\multicolumn{3}{c}{Interaction ladder} \\\\",
"\\cmidrule(lr){3-4}\\cmidrule(lr){5-7}",
"Variable & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
" & Pooled & Veterans & Younger & (a) & (b) & (c) \\\\",
"\\midrule",
paste0("Contract year & ", r_cy[1],  " \\\\"),
paste0(" & ",              r_cy[2],  " \\\\[2pt]"),
paste0("Contract year $\\times$ veteran & ", r_int[1], " \\\\"),
paste0(" & ",              r_int[2], " \\\\[2pt]"),
paste0("Veteran & ",       r_vet[1], " \\\\"),
paste0(" & ",              r_vet[2], " \\\\"),
"\\midrule",
"Player FE & Yes & Yes & Yes & Yes & --- & --- \\\\",
"Player $\\times$ career-stage FE & No & --- & --- & No & Yes & Yes \\\\",
"Controls $\\times$ career stage & No & --- & --- & No & No & Yes \\\\",
"Season FE $\\times$ career stage & No & --- & --- & No & No & Yes \\\\",
"\\midrule",
paste0("Observations & ", paste(formatC(vapply(cols, `[[`, numeric(1), "N"),
       format = "d", big.mark = ","), collapse = " & "), " \\\\"),
paste0("Players & ", paste(formatC(vapply(cols, `[[`, numeric(1), "G"),
       format = "d", big.mark = ","), collapse = " & "), " \\\\"),
paste0("Within $R^2$ & ", paste(sprintf("%.3f",
       vapply(cols, `[[`, numeric(1), "r2")), collapse = " & "), " \\\\"),
"\\midrule",
"\\multicolumn{7}{l}{\\textit{Difference in the contract year effect by career stage}} \\\\",
sprintf("\\multicolumn{4}{l}{\\quad Split sample, column (2) $-$ column (3)} & \\multicolumn{3}{c}{$%.5f$} \\\\", target),
sprintf("\\multicolumn{4}{l}{\\quad Interaction, column (6)} & \\multicolumn{3}{c}{$%.5f$} \\\\", got),
sprintf("\\multicolumn{4}{l}{\\quad Cluster bootstrap, 95\\%% interval} & \\multicolumn{3}{c}{$[%.5f,\\ %.5f]$} \\\\", ci[1], ci[2]),
"\\bottomrule",
"\\end{tabular}",
"\\begin{tablenotes}[flushleft]\\footnotesize",
"\\item \\textit{Note.} Standard errors clustered at the player level in parentheses.",
sprintf("The dependent variable is xwOBA. Veterans are players entering a season with %g or more", VET_CUTOFF),
"years of major league service, who become free agency eligible at season's end. All models",
"include season fixed effects and service time in quadratic form.",
"\\item Columns (2) and (3) split the sample. Columns (4) through (6) report an interaction",
"ladder in which each rung relaxes one restriction. Specification (a) retains common player",
"fixed effects and a common aging profile. Specification (b) assigns each player a separate",
"intercept in each career stage; specification (c) additionally interacts the service time",
"controls and the season effects with career stage. Because allowing every parameter to differ",
"by group is the same estimator as estimating the two groups separately, (c) must reproduce the",
"split-sample difference, and does.",
"\\item The veteran main effect is estimable only in (a), where one player intercept must span",
"both career stages and the aging level shift has nowhere else to go; it is absorbed by the",
"player $\\times$ career-stage intercepts in (b) and (c).",
sprintf("\\item The bracketed interval is a %s-replicate cluster bootstrap over players (two-sided $p=%.3f$),",
        format(BOOT_B, big.mark = ","), p_boot),
"which requires no residual degrees-of-freedom convention. Under the \\texttt{areg} convention the",
sprintf("interaction carries $p$-values of %.3f, %.3f and %.3f in columns (4) through (6); under \\texttt{xtreg},",
        gv(areg[[1]], "cy_vet", "p"), gv(areg[[2]], "cy_vet", "p"), gv(areg[[3]], "cy_vet", "p")),
sprintf("%.3f, %.3f and %.3f. See Note 5.",
        gv(cols[[4]], "cy_vet", "p"), gv(cols[[5]], "cy_vet", "p"), gv(cols[[6]], "cy_vet", "p")),
"\\item $^{*}p<.10$. $^{**}p<.05$. $^{***}p<.01$.",
"\\end{tablenotes}",
"\\end{threeparttable}",
"\\end{table}")

writeLines(tex, file.path(OUTDIR, "table7_careerstage.tex"))
cat("\n  wrote table7_careerstage.tex\n")
cat("  wrote table7_cells.csv\n")
