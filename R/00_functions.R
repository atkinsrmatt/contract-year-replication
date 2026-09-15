## 00_functions.R -- estimation engine and helpers (base R only)

PIPELINE_VERSION <- 5L

## CP850, not UTF-8 or Latin-1 -- do not switch to readLines(): R >= 4.2 on
## Windows is natively UTF-8 and mangles CP850 player names on the way in.
read_prelim <- function(path, encoding = "CP850") {
  if (!file.exists(path)) stop("CSV not found: ", path)

  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  bytes <- bytes[bytes != as.raw(0)]
  s <- rawToChar(bytes)
  Encoding(s) <- "bytes"

  txt <- iconv(s, from = encoding, to = "UTF-8")
  if (is.na(txt)) {
    for (alt in c("CP437", "latin1", "UTF-8")) {
      txt <- iconv(s, from = alt, to = "UTF-8")
      if (!is.na(txt)) {
        warning("Encoding '", encoding, "' failed; fell back to '", alt,
                "'. Check accented player names.", call. = FALSE)
        break
      }
    }
  }
  if (is.na(txt)) stop("Could not decode ", path, " under any known encoding.")

  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  lines <- lines[nzchar(lines)]
  d <- utils::read.csv(text = paste(lines, collapse = "\n"),
                       check.names = FALSE, stringsAsFactors = FALSE)

  expected <- c("PlayerId","MLBAMID","Team","Season","Name","PA","wOBA","xwOBA",
                "wRC+","BsR","Off","Def","WAR","MLS","FA Elig","Arb Elig",
                "Club","Player","Mutual","Vesting")
  if (!identical(names(d), expected)) {
    stop("Unexpected columns.\n  got:      ", paste(names(d), collapse = ", "),
         "\n  expected: ", paste(expected, collapse = ", "))
  }
  names(d) <- c("playerid","mlbamid","team","season","name","pa","woba","xwoba",
                "wrcplus","bsr","off","def","war","mls","fa_elig","arb_elig",
                "club_opt","player_opt","mutual_opt","vesting_opt")
  d
}

## Reproduces Stata `xtreg, fe cluster()`. Two details that must not change:
## K excludes the absorbed FE (`areg` includes them, returning SEs ~17%
## larger), and tests use df = G - 1, not N - K.
felm <- function(formula, data, id = "playerid", cluster = id, absorb = TRUE,
                 subset = NULL, dfadj = FALSE) {

  if (!is.null(subset)) {
    keep <- subset & !is.na(subset)
    data <- data[keep, , drop = FALSE]
  }
  data <- data[stats::complete.cases(data[, all.vars(formula), drop = FALSE]), ,
               drop = FALSE]

  mf <- stats::model.frame(formula, data)
  y  <- stats::model.response(mf)
  X  <- stats::model.matrix(formula, mf)
  g  <- droplevels(as.factor(data[[id]]))
  gc <- droplevels(as.factor(data[[cluster]]))

  if (!identical(id, cluster) && absorb) {
    nested <- all(tapply(as.integer(gc), g, function(z) length(unique(z))) == 1)
    if (!nested)
      warning("Absorbed effect '", id, "' is not nested within cluster '",
              cluster, "'; the DoF convention may not apply.", call. = FALSE)
  }

  if (absorb) {
    X  <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    dm <- function(v) v - ave(v, g)
    y <- dm(y)
    X <- apply(X, 2, dm)
  }

  qrX  <- qr(X)
  keep <- sort(qrX$pivot[seq_len(qrX$rank)])
  dropped <- setdiff(colnames(X), colnames(X)[keep])
  X <- X[, keep, drop = FALSE]

  XtXi <- solve(crossprod(X))
  b <- as.vector(XtXi %*% crossprod(X, y))
  names(b) <- colnames(X)
  e <- as.vector(y - X %*% b)

  N <- length(y); G <- nlevels(gc); nfe <- nlevels(g); k <- ncol(X)

  meat <- matrix(0, k, k)
  for (idx in split(seq_len(N), gc)) {
    if (!length(idx)) next
    u <- crossprod(X[idx, , drop = FALSE], e[idx])
    meat <- meat + tcrossprod(u)
  }

  Kadj <- k + if (absorb) (if (dfadj) nfe else 1L) else 0L
  q  <- (N - 1) / (N - Kadj) * G / (G - 1)
  V  <- q * XtXi %*% meat %*% XtXi
  dimnames(V) <- list(names(b), names(b))
  se <- sqrt(diag(V))
  df <- G - 1
  tv <- b / se
  p  <- 2 * stats::pt(abs(tv), df, lower.tail = FALSE)

  tss <- sum((y - mean(y))^2)
  r2  <- 1 - sum(e^2) / tss

  structure(list(b = b, se = se, t = tv, p = p, V = V, resid = e,
                 N = N, G = G, nfe = nfe, df = df, k = k, r2 = r2,
                 absorb = absorb, dfadj = dfadj, dropped = dropped,
                 formula = formula, id = id),
            class = "felm")
}

print.felm <- function(x, keep = NULL, ...) {
  v <- if (is.null(keep)) names(x$b) else intersect(keep, names(x$b))
  cat(sprintf("%-16s %10s %10s %8s\n", "", "coef", "se", "p"))
  for (nm in v)
    cat(sprintf("%-16s %10.5f %10.5f %8.3f\n", nm, x$b[nm], x$se[nm], x$p[nm]))
  cat(sprintf("N = %s   clusters = %s   %s = %.3f\n",
              format(x$N, big.mark = ","), format(x$G, big.mark = ","),
              if (x$absorb) "R2(within)" else "R2", x$r2))
  invisible(x)
}

wald <- function(fit, terms, type = c("zero", "equal")) {
  type  <- match.arg(type)
  terms <- intersect(terms, names(fit$b))
  if (!length(terms)) return(list(F = NA_real_, p = NA_real_, df1 = 0L, df2 = fit$df))

  if (type == "zero") {
    R <- matrix(0, length(terms), length(fit$b), dimnames = list(NULL, names(fit$b)))
    for (i in seq_along(terms)) R[i, terms[i]] <- 1
  } else {
    if (length(terms) < 2)
      return(list(F = NA_real_, p = NA_real_, df1 = 0L, df2 = fit$df))
    R <- matrix(0, length(terms) - 1, length(fit$b),
                dimnames = list(NULL, names(fit$b)))
    for (i in seq_len(length(terms) - 1)) {
      R[i, terms[1]]     <-  1
      R[i, terms[i + 1]] <- -1
    }
  }
  Rb  <- R %*% fit$b
  RVR <- R %*% fit$V %*% t(R)
  J   <- nrow(R)
  Fst <- as.numeric(t(Rb) %*% solve(RVR) %*% Rb) / J
  list(F = Fst, p = stats::pf(Fst, J, fit$df, lower.tail = FALSE),
       df1 = J, df2 = fit$df)
}

## write.csv substitutes "<U+00E1>" escapes silently. This writes UTF-8 bytes.
write_csv_utf8 <- function(x, path, digits = 15) {
  cols <- lapply(x, function(col) {
    v <- if (is.double(col)) format(col, digits = digits, trim = TRUE,
                                    scientific = FALSE)
         else as.character(col)
    v[is.na(col)] <- "NA"
    ifelse(grepl('[",\r\n]', v), paste0('"', gsub('"', '""', v), '"'), v)
  })
  hdr  <- paste(ifelse(grepl('[",\r\n]', names(x)),
                       paste0('"', gsub('"', '""', names(x)), '"'), names(x)),
                collapse = ",")
  body <- do.call(paste, c(cols, list(sep = ",")))
  con  <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(c(hdr, body)), con, useBytes = TRUE)
  invisible(path)
}

grab <- function(fit, term, label = term, extra = NULL) {
  has <- term %in% names(fit$b)
  out <- data.frame(term = label,
                    b  = if (has) unname(fit$b[term])  else NA_real_,
                    se = if (has) unname(fit$se[term]) else NA_real_,
                    p  = if (has) unname(fit$p[term])  else NA_real_,
                    N = fit$N, G = fit$G, r2 = fit$r2,
                    stringsAsFactors = FALSE)
  if (length(extra)) out <- cbind(out, as.data.frame(extra, stringsAsFactors = FALSE))
  out
}

stars <- function(p) {
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else
    if (p < 0.10) "*" else ""
}

mde <- function(se) 2.80 * se

tost <- function(fit, term, bound) {
  b <- unname(fit$b[term]); se <- unname(fit$se[term])
  p_up <- stats::pt((b - bound) / se, fit$df)
  p_lo <- stats::pt((b + bound) / se, fit$df, lower.tail = FALSE)
  list(b = b, se = se, bound = bound, p = max(p_up, p_lo))
}

fmt_p <- function(p) ifelse(is.na(p), "", sprintf("%.3f", p))

hr <- function(title = NULL, ch = "=") {
  cat("\n", strrep(ch, 70), "\n", sep = "")
  if (!is.null(title)) cat(title, "\n", strrep(ch, 70), "\n", sep = "")
}
