## ============================================================================ #
## run_all.R
## Runs the full analysis in order. From the repository root:
##     Rscript run_all.R
## or open the project in RStudio and source this file.
##
## Before the first run, point R to the raw CHIP50 waves (see data/README.md):
##     Sys.setenv(C50_WEIGHTED_RDS_DIR = "<folder with CSP_W<wave>_weighted.RDS>")
## or set C50_WEIGHTED_RDS_DIR permanently in ~/.Renviron and restart R.
## Step 1 is skipped when the analysis file (data/chip50_trust_analysis.rds)
## already exists, unless REBUILD_DATA is TRUE.
## ============================================================================ #

.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
.f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
        unlist(lapply(sys.frames(), function(e) e$ofile)))
REPO_DIR <- if (length(.f)) normalizePath(dirname(.f[length(.f)]), winslash = "/") else
    normalizePath(getwd(), winslash = "/")

# ---- Settings (defaults reproduce the published results; see README) ---------
SDH_TEST_WAVES    <- "published"   # or "trust_waves": SDH significance shares over 20 waves
LOBO_BRIER        <- "unweighted"  # or "weighted": survey-weighted Brier in Table S1
TABLE1_RD_AVERAGE <- "unweighted"  # or "weighted": survey-weighted standardization of the Table 1 RDs
RUN_PAIRWISE_RD   <- TRUE          # FALSE skips the pairwise RDs of script 03 (faster)
MOSAIC_MATCH_TRUST_WAVES <- TRUE   # FALSE shows all 23 waves in the SDH panels
N_CORES <- max(1L, min(8L, parallel::detectCores() - 2L), na.rm = TRUE)  # 1 = sequential
REBUILD_DATA      <- FALSE         # TRUE re-creates the analysis file from the raw waves

source(file.path(REPO_DIR, "R", "00_setup.R"))   # packages, paths, settings check

.steps <- c("01_build_analysis_data.R",    # raw waves -> data/chip50_trust_analysis.rds
            "02_descriptive_prevalence.R", # Figs 1-2, 5-6, S1-S10, S19; unadjusted gaps
            "03_descriptive_inference.R",  # % of waves with significant differences
            "04_adjusted_models.R",        # adjusted RDs and ORs (Figs 3-4, S11-S17)
            "05_sequential_models.R",      # Table 1; leave-one-block-out
            "06_summary_statistics.R",     # Table S1, S18 statistics, check vs paper
            "07_figures.R",                # all data figures
            "08_fig_S21_model_schematic.R")
if (file.exists(DATA_FILE) && !REBUILD_DATA) .steps <- .steps[-1]

for (.step in .steps) {
    .t0 <- Sys.time()
    cat("\n", strrep("=", 78), "\n== ", .step, "\n", strrep("=", 78), "\n", sep = "")
    source(file.path(REPO_DIR, "R", .step), echo = FALSE)
    cat(sprintf("== %s finished in %.1f min\n", .step,
                as.numeric(difftime(Sys.time(), .t0, units = "mins"))))
}

# Fig. S20 (DAG panel) is drawn with Python 3 (matplotlib, Pillow)
.py_ok <- FALSE
for (.py in Sys.which(c("python3", "python", "py"))) {
    if (!nzchar(.py)) next
    .status <- suppressWarnings(
        system2(.py, shQuote(file.path(REPO_DIR, "python", "fig_S20_dag_panel.py"))))
    if (identical(as.integer(.status), 0L)) { .py_ok <- TRUE; break }
}
if (!.py_ok) message("Fig. S20 was not drawn: run python/fig_S20_dag_panel.py ",
                     "with Python 3 (matplotlib and Pillow installed).")

# Record the R version and package versions used for this run
writeLines(utils::capture.output(utils::sessionInfo()), file.path(OUT_DIR, "session_info.txt"))
cat("\nAll done. Results are in ", OUT_DIR, "\n", sep = "")
