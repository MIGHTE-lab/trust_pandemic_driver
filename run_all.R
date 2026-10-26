## -----------------------------------------------------------------------------
## Script name: run_all.R
##
## Purpose: Run all analyses in order (scripts 01-08, then Fig S20)
##
## Author: ᚨᛃᛉ
## Date Created: 10.22.2026
## Updated:
##
## Notes: Run from the repository root (Rscript run_all.R) or source it in
##        RStudio. Step 01 runs only if the analysis file is missing or
##        REBUILD_DATA is TRUE; it needs the raw waves, set with
##        Sys.setenv(C50_WEIGHTED_RDS_DIR = "<folder>").
## -----------------------------------------------------------------------------

# Find the repository folder (Rscript, source() or RStudio)
.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
.f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
        unlist(lapply(sys.frames(), function(e) e$ofile)))
REPO_DIR <- if (length(.f)) normalizePath(dirname(.f[length(.f)]), winslash = "/") else
    normalizePath(getwd(), winslash = "/")

# ============================================================================ #
# SECTION 1: SETTINGS ----
# ============================================================================ #
# Defaults reproduce the published results (see README)
SDH_TEST_WAVES    <- "published"   # alternative "trust_waves": SDH significance shares over 20 waves
LOBO_BRIER        <- "unweighted"  # alternative "weighted": survey-weighted Brier in Table S1
TABLE1_RD_AVERAGE <- "unweighted"  # alternative "weighted": survey-weighted standardization of the Table 1 RDs
RUN_PAIRWISE_RD   <- TRUE          # FALSE skips the pairwise RDs of script 03 (faster runtime)
MOSAIC_MATCH_TRUST_WAVES <- TRUE   # FALSE shows all 23 waves in the SDH panels
N_CORES <- max(1L, min(8L, parallel::detectCores() - 2L), na.rm = TRUE)  # 1 = sequential
REBUILD_DATA      <- FALSE         # TRUE re-creates the analysis file from the raw survey data 

source(file.path(REPO_DIR, "R", "00_setup.R"))   # Packages, paths, settings

# ============================================================================ #
# SECTION 2: RUN SCRIPTS 01-08 ----
# ============================================================================ #
# Dot-prefixed names avoid clashes with objects in the sourced scripts
.steps <- c("01_build_analysis_data.R",    # Process raw waves -> analysis file
            "02_descriptive_prevalence.R", # Figs 1-2, 5-6, S1-S10, S19; unadjusted gaps (differences)
            "03_descriptive_inference.R",  # Significance shares
            "04_adjusted_models.R",        # Adjusted RDs and ORs
            "05_sequential_models.R",      # Table 1; leave-one-block-out
            "06_summary_statistics.R",     # Table S1, S18 statistics, check vs paper
            "07_figures.R",                # All figures
            "08_fig_S21_model_schematic.R")
if (file.exists(DATA_FILE) && !REBUILD_DATA) .steps <- .steps[-1] # step 1 is skipped if dataset exists

for (.step in .steps) {
    .t0 <- Sys.time()
    cat("\n", strrep("=", 78), "\n== ", .step, "\n", strrep("=", 78), "\n", sep = "")
    source(file.path(REPO_DIR, "R", .step), echo = FALSE)
    cat(sprintf("== %s finished in %.1f min\n", .step,
                as.numeric(difftime(Sys.time(), .t0, units = "mins"))))
}

# ============================================================================ #
# SECTION 3: FIG S20 AND SESSION INFO ----
# ============================================================================ #
# Fig. S20 (DAG panel) is generated with Python 3 (matplotlib, Pillow), so needs to be run separately
.py_ok <- FALSE
for (.py in Sys.which(c("python3", "python", "py"))) {
    if (!nzchar(.py)) next
    .status <- suppressWarnings(
        system2(.py, shQuote(file.path(REPO_DIR, "python", "fig_S20_dag_panel.py"))))
    if (identical(as.integer(.status), 0L)) { .py_ok <- TRUE; break }
}
if (!.py_ok) message("Fig. S20 was not generated: run python/fig_S20_dag_panel.py ",
                     "with Python 3 (matplotlib and Pillow installed).")

# R and package versions of this run
writeLines(utils::capture.output(utils::sessionInfo()), file.path(OUT_DIR, "session_info.txt"))
cat("\nAll done. Results are in ", OUT_DIR, "\n", sep = "")
