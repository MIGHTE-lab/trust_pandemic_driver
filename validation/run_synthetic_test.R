## ============================================================================ #
## run_synthetic_test.R
## Runs the whole pipeline on synthetic survey waves in a temporary folder, to
## check that the code runs without access to the survey data. The results are
## meaningless; the published values check will not match.
##
## Usage (from the repository root):  Rscript validation/run_synthetic_test.R
## ============================================================================ #

.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
.f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
        unlist(lapply(sys.frames(), function(e) e$ofile)))
REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                          winslash = "/") else normalizePath(getwd(), winslash = "/")
tmp <- file.path(tempdir(), "trust_synthetic_test")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")

status <- system2(rscript, c(shQuote(file.path(REPO_DIR, "validation", "simulate_chip50_waves.R")),
                             shQuote(file.path(tmp, "raw")), "800"))
stopifnot(status == 0)
Sys.setenv(C50_WEIGHTED_RDS_DIR = file.path(tmp, "raw"),
           C50_ANALYSIS_FILE = file.path(tmp, "data", "chip50_trust_analysis.rds"),
           C50_OUTPUT_DIR = file.path(tmp, "output"))
status <- system2(rscript, shQuote(file.path(REPO_DIR, "run_all.R")))
cat("\nrun_all.R finished with status", status, "\nOutputs in", file.path(tmp, "output"), ":\n")
print(list.files(file.path(tmp, "output"), recursive = TRUE))
