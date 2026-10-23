## ============================================================================ #
## compare_with_original_outputs.R
## Checks that this repository reproduces the outputs of the authors' original
## analysis scripts. Run it after run_all.R, on the computer that holds both the
## new outputs and the outputs of the original scripts (which are not
## distributed with the repository).
##
## Usage (from the repository root):
##   Rscript validation/compare_with_original_outputs.R
## Folders (defaults are relative to the repository root and assume that it sits
## in the authors' "Analytic code/PNAS manuscript" folder):
##   C50_OUTPUT_DIR            new outputs                 (default ./output)
##   C50_ORIGINAL_OUTPUT_DIR   original "output" folder    (default ../output)
##   C50_ORIGINAL_DISPARITY_DIR  outputs of disparity_inference_analysis.R
##                             (default ../../output/disparity_inference)
## Output: output/validation/comparison_summary.csv   one row per compared file
##         output/validation/comparison_details.csv   values outside tolerance
## ============================================================================ #

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr) })

.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
.f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
        unlist(lapply(sys.frames(), function(e) e$ofile)))
REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                          winslash = "/") else normalizePath(getwd(), winslash = "/")
env_dir <- function(var, default) {
    v <- Sys.getenv(var, unset = "")
    normalizePath(if (nzchar(v)) v else default, winslash = "/", mustWork = FALSE)
}
NEW  <- env_dir("C50_OUTPUT_DIR", file.path(REPO_DIR, "output"))
ORIG <- env_dir("C50_ORIGINAL_OUTPUT_DIR", file.path(REPO_DIR, "..", "output"))
DISP <- env_dir("C50_ORIGINAL_DISPARITY_DIR",
                file.path(REPO_DIR, "..", "..", "output", "disparity_inference"))
OUT  <- file.path(NEW, "validation")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
cat("New outputs:      ", NEW, "\nOriginal outputs: ", ORIG, "\nOriginal SDH tests:", DISP, "\n\n")

P1 <- file.path(ORIG, "trust_extensions_NHB", "phase1_adjusted_with_trust")
P3 <- file.path(ORIG, "trust_extensions_NHB", "phase3b_iterative_trust_model_comparison", "tables")
BT <- file.path(ORIG, "behavioral_trajectories", "data")
OUTCOME_MAP <- c(visit_1 = "visit_work", visit_3 = "visit_store", visit_9 = "visit_doctor")

#' Compare one new file with its original counterpart.
#'   keys     columns identifying a row (after renaming)
#'   values   numeric columns; they match if |new - original| <= tol * max(1, |original|)
#'            (relative tolerance; absolute for values below 1), or <= tol when
#'            absolute = TRUE
#'   strings  text columns compared exactly
#'   map_visits  rename visit_1/3/9 to visit_work/store/doctor in the original
compare_file <- function(label, new_file, orig_file, keys, values = character(0),
                         strings = character(0), tol = 1e-6, absolute = FALSE,
                         map_visits = FALSE, restrict_to_new = TRUE) {
    row <- tibble::tibble(comparison = label, new_file = basename(new_file),
                          original_file = basename(orig_file), status = NA_character_,
                          rows_new = NA_integer_, rows_original = NA_integer_,
                          rows_matched = NA_integer_, rows_only_new = NA_integer_,
                          rows_only_original = NA_integer_, values_compared = NA_integer_,
                          values_outside_tolerance = NA_integer_, max_abs_diff = NA_real_,
                          note = "")
    if (!file.exists(orig_file)) { row$status <- "SKIPPED"; row$note <- "original file not found"; return(list(summary = row)) }
    if (!file.exists(new_file)) { row$status <- "SKIPPED"; row$note <- "new file not found (run run_all.R)"; return(list(summary = row)) }
    a <- readr::read_csv(new_file, show_col_types = FALSE, guess_max = 1e5)
    b <- readr::read_csv(orig_file, show_col_types = FALSE, guess_max = 1e5)
    if (map_visits && "outcome" %in% names(b))
        b$outcome <- ifelse(b$outcome %in% names(OUTCOME_MAP), OUTCOME_MAP[b$outcome], b$outcome)
    for (k in keys) { a[[k]] <- as.character(a[[k]]); b[[k]] <- as.character(b[[k]]) }
    if (restrict_to_new && "outcome" %in% keys) b <- b[b$outcome %in% a$outcome, ]
    if (restrict_to_new && "stratifier" %in% keys) b <- b[b$stratifier %in% a$stratifier, ]
    if (restrict_to_new && "predictor" %in% keys) b <- b[b$predictor %in% a$predictor, ]
    cols <- c(values, strings)
    missing <- c(setdiff(c(keys, cols), names(a)), setdiff(c(keys, cols), names(b)))
    if (length(missing)) { row$status <- "ERROR"; row$note <- paste("missing columns:", paste(unique(missing), collapse = ", ")); return(list(summary = row)) }
    a <- a[, c(keys, cols)]; b <- b[, c(keys, cols)]
    a$.in_new <- TRUE; b$.in_orig <- TRUE
    j <- dplyr::full_join(a, b, by = keys, suffix = c(".new", ".orig"),
                          relationship = "many-to-many")
    in_new <- !is.na(j$.in_new); in_orig <- !is.na(j$.in_orig)
    both <- j[in_new & in_orig, ]
    only_new <- sum(in_new & !in_orig); only_orig <- sum(in_orig & !in_new)
    det <- list(); n_cmp <- 0; n_bad <- 0; max_d <- 0
    for (v in values) {
        x <- as.numeric(both[[paste0(v, ".new")]]); y <- as.numeric(both[[paste0(v, ".orig")]])
        ok <- !(is.na(x) & is.na(y))
        d <- abs(x - y)
        limit <- if (absolute) tol else tol * pmax(1, abs(y))
        bad <- ok & (is.na(d) | d > limit)
        n_cmp <- n_cmp + sum(ok); n_bad <- n_bad + sum(bad)
        if (any(is.finite(d))) max_d <- max(max_d, d[is.finite(d)])
        if (any(bad)) det[[v]] <- both[bad, keys] %>%
            dplyr::mutate(column = v, new = x[bad], original = y[bad], abs_diff = d[bad])
    }
    for (v in strings) {
        x <- as.character(both[[paste0(v, ".new")]]); y <- as.character(both[[paste0(v, ".orig")]])
        bad <- !(is.na(x) & is.na(y)) & (is.na(x) | is.na(y) | x != y)
        n_cmp <- n_cmp + sum(!(is.na(x) & is.na(y))); n_bad <- n_bad + sum(bad)
        if (any(bad)) det[[v]] <- both[bad, keys] %>%
            dplyr::mutate(column = v, new_text = x[bad], original_text = y[bad])
    }
    row$rows_new <- nrow(a); row$rows_original <- nrow(b); row$rows_matched <- nrow(both)
    row$rows_only_new <- only_new; row$rows_only_original <- only_orig
    row$values_compared <- n_cmp; row$values_outside_tolerance <- n_bad
    row$max_abs_diff <- max_d
    row$status <- if (n_bad == 0 && only_new == 0 && only_orig == 0) "MATCH" else "REVIEW"
    details <- dplyr::bind_rows(lapply(det, function(d) dplyr::mutate(d, dplyr::across(dplyr::all_of(keys), as.character))))
    if (nrow(details)) details <- dplyr::mutate(details, comparison = label, .before = 1)
    list(summary = row, details = details)
}

est_file <- function(f) file.path(NEW, "estimates", f)
tab_file <- function(f) file.path(NEW, "tables", f)
# The published mosaic table was recovered from the vector PDFs of the
# 2026-08-26 run (values rounded to 0.001 pp); a re-run of the original script
# gives full precision. Allow 0.01 percentage points (absolute) either way.
checks <- list(
    compare_file("Prevalence by stratum (Figs 1-2, 5-6, S1-S10)", est_file("prevalence_by_stratum.csv"),
                 file.path(BT, "mosaic_panel_estimates.csv"),
                 keys = c("outcome", "stratifier", "group", "wave"),
                 values = c("pct", "ci_lo", "ci_hi"), tol = 0.01, absolute = TRUE),
    compare_file("Trust-level shares (Fig S19)", est_file("trust_level_shares.csv"),
                 file.path(BT, "trust_trajectories_data.csv"),
                 keys = c("trust_var", "trust_level", "wave"),
                 values = c("weighted_prevalence_pct", "ci_lower_pct", "ci_upper_pct", "total_n")),
    compare_file("Adherence gaps by wave", est_file("adherence_gap_by_wave.csv"),
                 file.path(BT, "disparity_gap", "adherence_gap_all_outcomes.csv"),
                 keys = c("outcome", "predictor", "wave"),
                 values = c("max_pct", "min_pct", "gap_pct"), strings = c("max_group", "min_group")),
    compare_file("Omnibus tests by wave", est_file("omnibus_tests_by_wave.csv"),
                 file.path(DISP, "tables", "approach_A", "omnibus_per_wave.csv"),
                 keys = c("outcome", "stratifier", "wave"),
                 values = c("n_obs", "F_stat", "p_value", "p_adj_BH"), map_visits = TRUE),
    compare_file("Significance shares", tab_file("significance_share.csv"),
                 file.path(DISP, "tables", "summary", "significance_share_per_outcome_stratifier.csv"),
                 keys = c("outcome", "stratifier"),
                 values = c("n_waves", "n_sig_BH", "pct_sig_BH"), map_visits = TRUE),
    compare_file("Pairwise RDs by wave", est_file("pairwise_rd_by_wave.csv"),
                 file.path(DISP, "tables", "approach_A", "pairwise_per_wave.csv"),
                 keys = c("outcome", "stratifier", "wave", "contrast"),
                 values = c("risk_diff", "SE", "p_pair"), map_visits = TRUE)
)
for (f in c("trust_cdc", "trust_doc", "trust_state", "trust_banks")) {
    checks <- c(checks, list(
        compare_file(paste("Adjusted RDs by wave,", f), est_file(sprintf("adjusted_rd_by_wave_%s.csv", f)),
                     file.path(P1, "tables", sprintf("trust_risk_differences_by_wave_%s.csv", f)),
                     keys = c("outcome", "wave", "model_spec"),
                     values = c("estimate", "std_error", "conf_low", "conf_high", "p_value", "p_adj")),
        compare_file(paste("Adjusted ORs by wave,", f), est_file(sprintf("adjusted_or_by_wave_%s.csv", f)),
                     file.path(P1, "tables", sprintf("adjusted_ORs_by_wave_%s.csv", f)),
                     keys = c("outcome", "wave", "model_spec", "term"),
                     values = c("estimate", "std_error", "conf_low", "conf_high", "p_value", "n_obs"),
                     strings = c("sdh_group", "level_label")),
        compare_file(paste("Era-pooled ORs,", f), est_file(sprintf("pooled_era_or_%s.csv", f)),
                     file.path(P1, "tables", sprintf("pooled_era_ORs_%s.csv", f)),
                     keys = c("outcome", "era", "model_spec", "term"),
                     values = c("estimate", "conf_low", "conf_high", "n_obs"),
                     strings = c("waves_in_era", "sdh_group")),
        compare_file(paste("Era-pooled RDs,", f), est_file(sprintf("pooled_era_rd_%s.csv", f)),
                     file.path(P1, "tables", sprintf("pooled_era_trust_RDs_%s.csv", f)),
                     keys = c("outcome", "era", "model_spec"),
                     values = c("estimate", "std_error", "conf_low", "conf_high"))))
}
checks <- c(checks, list(
    compare_file("Generalized-trust comparison (Fig S18)", tab_file("sensitivity_general_trust_comparison.csv"),
                 file.path(P1, "sensitivity_checks", "tables", "sensitivity_RD_comparison_trust_cdc.csv"),
                 keys = c("outcome", "wave"),
                 values = c("RD_primary_pp", "RD_genTrust_pp", "abs_change_pp", "pct_change"),
                 tol = 1e-5),
    compare_file("Negative-control summary", tab_file("negative_control_summary.csv"),
                 file.path(P1, "tables", "negative_control_summary_trust_banks.csv"),
                 keys = "outcome", values = "banks_to_cdc_ratio_pct",
                 strings = c("banks_RD_median_IQR_pp", "banks_waves_sig_CI", "banks_waves_sig_BH",
                             "cdc_RD_median_IQR_pp", "cdc_waves_sig_CI")),
    compare_file("LOBO by wave", est_file("lobo_by_wave.csv"),
                 file.path(P3, "drop_one_block_from_M4_comparisons.csv"),
                 keys = c("outcome", "wave", "reduced_model_id"),
                 values = c("n", "delta_AIC_full_minus_reduced", "delta_Brier_full_minus_reduced",
                            "Wald_F", "Wald_p", "Wald_p_adj")),
    compare_file("Table 1", tab_file("table_1_sequential_adjustment.csv"),
                 file.path(P3, "manuscript_table_1_sequential_adjustment.csv"),
                 keys = c("outcome", "model_id"),
                 values = c("number_of_waves_analyzed", "median_n", "median_events"),
                 strings = c("trust_OR_median_IQR", "trust_probability_difference_median_IQR",
                             "pct_change_trust_logOR_vs_M0_median_IQR",
                             "delta_AIC_vs_M0_median_IQR", "delta_Tjur_R2_vs_M0_median_IQR",
                             "delta_Brier_vs_M0_median_IQR", "waves_with_same_trust_direction",
                             "waves_with_trust_q_lt_0_05", "block_tests_q_lt_0_05_n")),
    compare_file("Table S1 (script 17)", tab_file("table_S1_lobo_by_era.csv"),
                 file.path(ORIG, "pnas_supplementary_stats", "table_S1_LOBO_by_era.csv"),
                 keys = c("outcome", "era", "block"),
                 values = c("n_waves", "median_n", "dAIC_median", "dAIC_q25", "dAIC_q75",
                            "dBrier_median", "dBrier_q25", "dBrier_q75"))
))

overview <- dplyr::bind_rows(lapply(checks, `[[`, "summary"))
details <- dplyr::bind_rows(lapply(checks, function(x) x$details))
if (nrow(details) == 0)
    details <- tibble::tibble(comparison = character(0), column = character(0),
                              new = numeric(0), original = numeric(0), abs_diff = numeric(0))
readr::write_csv(overview, file.path(OUT, "comparison_summary.csv"), na = "")
readr::write_csv(details, file.path(OUT, "comparison_details.csv"), na = "")

cat(sprintf("%-50s %-8s %8s %8s %8s %10s\n", "Comparison", "Status", "matched",
            "only_new", "only_orig", "outside_tol"))
for (i in seq_len(nrow(overview))) with(overview[i, ], cat(sprintf(
    "%-50s %-8s %8s %8s %8s %10s  %s\n", substr(comparison, 1, 50), status,
    rows_matched, rows_only_new, rows_only_original, values_outside_tolerance, note)))
cat("\nMATCH:   all values agree within tolerance and no rows are missing.",
    "\nREVIEW:  some values or rows differ; see comparison_details.csv and",
    "\n         validation/README.md (section 2) for likely reasons.",
    "\nSKIPPED: the new or the original file was not found (see note).",
    "\nERROR:   expected columns are missing (see note).\n")
cat("Written to", OUT, "\n")
