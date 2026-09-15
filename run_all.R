## The Contract Year Phenomenon Revisited -- Atkins & Dikgang
## Base R only, no packages. Needs Clean_Data.csv; see data/README.md.
##
##   Rscript run_all.R

rm(list = ls())
set.seed(12345)
options(stringsAsFactors = FALSE, width = 100)

## Forward slashes only: "C:\Users\..." is read as escape sequences and fails.
## Falls back to CY_PROJ, then getwd().
PROJ <- "C:/Users/atkin/OneDrive/Desktop/Stata"

## Career-stage cutoff in years of service. Five puts 524 of 528 free agency
## seasons on the veteran side; six puts 341.
VET_CUTOFF <- 5

BOOT_B <- 2000L

if (!dir.exists(PROJ)) {
  alt  <- Sys.getenv("CY_PROJ")
  PROJ <- if (nzchar(alt) && dir.exists(alt)) alt else getwd()
}
PROJ <- normalizePath(PROJ, winslash = "/", mustWork = TRUE)

RDIR <- if (dir.exists(file.path(PROJ, "R"))) file.path(PROJ, "R") else PROJ
need <- c("00_functions.R","01_build.R","02_models.R","03_tables.R",
          "04_verify.R","05_table7.R","06_manuscript.R","07_manuscript_tables.R","08_endogeneity.R")
missing <- need[!file.exists(file.path(RDIR, need))]
if (length(missing))
  stop("Missing script(s) in ", RDIR, ":\n  ", paste(missing, collapse = "\n  "),
       "\nPut the R/ folder inside ", PROJ)

## PrelimData*.csv is the older uncorrected extract: last resort, with a
## warning.
find_csv <- function(root) {
  dirs  <- c(root, file.path(root, "data"))
  dirs  <- dirs[dir.exists(dirs)]
  exact <- file.path(dirs, "Clean_Data.csv")
  hit   <- exact[file.exists(exact)]
  if (length(hit)) return(list(path = hit[1], legacy = FALSE))

  glob <- unlist(lapply(dirs, function(p)
    list.files(p, "^Clean[_ ]?Data.*\\.csv$", full.names = TRUE, ignore.case = TRUE)))
  if (length(glob)) return(list(path = glob[1], legacy = FALSE))

  old <- unlist(lapply(dirs, function(p)
    list.files(p, "^PrelimData.*\\.csv$", full.names = TRUE)))
  if (length(old)) return(list(path = old[1], legacy = TRUE))

  list(path = NA_character_, legacy = FALSE)
}
.csv <- find_csv(PROJ)
RAWCSV <- .csv$path
if (is.na(RAWCSV))
  stop("Clean_Data.csv not found. Searched:\n  ", PROJ,
       "\n  ", file.path(PROJ, "data"),
       "\nPlace the CSV in one of those, or set RAWCSV manually below.")
if (isTRUE(.csv$legacy))
  warning("Found only PrelimData*.csv, the older UNCORRECTED extract. Its ",
          "Cervelli 2018 service time is wrong and the fingerprint gate in ",
          "01_build.R will fail. Use Clean_Data.csv.", call. = FALSE)

OUTDIR <- file.path(PROJ, "output")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
if (file.access(OUTDIR, 2) != 0)
  stop("No write permission for ", OUTDIR,
       "\nIf this is a synced OneDrive folder, pause syncing or choose another OUTDIR.")

logfile <- file.path(OUTDIR, "contract_year_analysis.log")
con <- file(logfile, open = "wt", encoding = "UTF-8")
sink(con, split = TRUE)

cat("THE CONTRACT YEAR PHENOMENON REVISITED IN THE STATCAST ERA\n")
cat("Atkins & Dikgang | R replication\n")
cat("Run:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R:    ", R.version.string, "on", .Platform$OS.type, "\n")
cat("Proj: ", PROJ, "\n")
cat("Data: ", RAWCSV, "\n")
cat("Out:  ", OUTDIR, "\n")

source(file.path(RDIR, "00_functions.R"))
source(file.path(RDIR, "01_build.R"))
source(file.path(RDIR, "02_models.R"))
source(file.path(RDIR, "03_tables.R"))
source(file.path(RDIR, "04_verify.R"))
source(file.path(RDIR, "05_table7.R"))
source(file.path(RDIR, "06_manuscript.R"))
source(file.path(RDIR, "07_manuscript_tables.R"))
source(file.path(RDIR, "08_endogeneity.R"))

cat("\nDONE", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("  Log:    ", logfile, "\n")
cat("  Panel:  ", file.path(OUTDIR, "contract_year_clean.csv"), "\n")
cat("  Ledger: ", file.path(OUTDIR, "results_ledger.csv"), "\n")
cat("  Tables: ", file.path(OUTDIR, "table1 - table8 (.tex/.csv)"), "\n")
cat("  Facts:  ", file.path(OUTDIR, "manuscript_facts.csv"), "\n")

sink()
close(con)
cat("Complete. See", logfile, "\n")
