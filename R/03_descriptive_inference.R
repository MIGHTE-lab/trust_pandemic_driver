## -----------------------------------------------------------------------------
## Script name: 03_descriptive_inference.R
##
## Purpose: Per-wave tests of differences in protective behaviors between levels of
##          each stratifier (design-based Wald F, BH-adjusted across waves) and
##          the share of waves with significant differences
##
## Author: ᚨᛃᛉ
## Date Created: 08.27.2025
## Updated: 10.31.2026
##
## Output: output/estimates/omnibus_tests_by_wave.csv, pairwise_rd_by_wave.csv;
##         output/tables/significance_share.csv
##
## Notes: SDH_TEST_WAVES sets the waves of the SDH stratifiers ("published": 23;
##        "trust_waves": 20). Trust items always use the 20 waves for which trust data is available. 
## -----------------------------------------------------------------------------

# Find the repository that contains scripts (Rscript, source() or RStudio)
if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))
use_packages("marginaleffects")

dat <- load_analysis_data()
# Restrict to the 20 waves with trust data, if requested
if (SDH_TEST_WAVES == "trust_waves") dat <- dat[dat$wave %in% TRUST_WAVES, ]
log_msg("Waves used for the stratifier tests: ", dplyr::n_distinct(dat$wave),
        " (SDH_TEST_WAVES = \"", SDH_TEST_WAVES, "\")")

# ============================================================================ #
# SECTION 1: OUTCOMES, STRATIFIERS AND TESTS ----
# ============================================================================ #
TEST_OUTCOMES <- c("always_wear_mask", "always_avoid_contact", "always_avoid_crowds",
                   "wash_hands_strict", "visit_work", "visit_store", "visit_doctor")
TEST_STRATIFIERS <- c("age_cat", "gender", "income_cat_5_fct", "education_cat",
                      "race_fct", "party3", "ideology_fct", "employ_fct",
                      "urban_type", "region_fct", "trust_cdc")
MIN_CELL_N <- 30    # Strata with fewer than 30 respondents per wave are dropped

#' Wald F tests and pairwise risk differences (RDs) for one outcome and stratifier, by wave
test_stratifier <- function(task) {
    o <- task$outcome; s <- task$stratifier
    d <- task$data
    d <- d[!is.na(d[[o]]) & !is.na(d[[s]]), , drop = FALSE]
    d[[s]] <- droplevels(as.factor(d[[s]]))
    omni <- list(); pairs <- list()
    for (w in sort(unique(d$wave))) {
        sub <- d[d$wave == w, , drop = FALSE]
        cell_n <- table(sub[[s]])
        keep <- names(cell_n)[cell_n >= MIN_CELL_N]
        if (nrow(sub) < 2 * MIN_CELL_N || length(keep) < 2) next
        sub <- sub[as.character(sub[[s]]) %in% keep, , drop = FALSE]
        sub[[s]] <- droplevels(sub[[s]])
        des <- survey::svydesign(ids = ~1, weights = stats::as.formula(paste0("~", WEIGHT_VAR)),
                                 data = sub)
        fit <- tryCatch(survey::svyglm(stats::as.formula(paste(o, "~", s)), design = des,
                                       family = stats::quasibinomial()),
                        error = function(e) NULL)
        if (is.null(fit)) next
        tt <- tryCatch(survey::regTermTest(fit, stats::as.formula(paste0("~", s)),
                                           method = "Wald"), error = function(e) NULL)
        if (is.null(tt)) next
        omni[[length(omni) + 1]] <- tibble::tibble(
            outcome = o, stratifier = s, wave = w, n_obs = nrow(sub),
            n_levels = nlevels(sub[[s]]), df1 = as.numeric(tt$df),
            df2 = as.numeric(tt$ddf), F_stat = as.numeric(tt$Ftest),
            p_value = as.numeric(tt$p))
        if (isTRUE(RUN_PAIRWISE_RD)) {
            # newdata may be needed for the weights in some marginaleffects versions
            ac <- tryCatch(as.data.frame(marginaleffects::avg_comparisons(
                fit, variables = stats::setNames(list("pairwise"), s),
                comparison = "difference", type = "response",
                vcov = stats::vcov(fit), wts = WEIGHT_VAR, newdata = sub)),
                error = function(e) NULL)
            if (!is.null(ac) && nrow(ac) > 0) {
                ccol <- intersect(c("contrast", paste0("contrast_", s)), names(ac))[1]
                pairs[[length(pairs) + 1]] <- tibble::tibble(
                    outcome = o, stratifier = s, wave = w,
                    contrast = as.character(ac[[ccol]]), risk_diff = ac$estimate,
                    SE = ac$std.error, ci_lower = ac$conf.low,
                    ci_upper = ac$conf.high, p_pair = ac$p.value)
            }
        }
    }
    list(omnibus = dplyr::bind_rows(omni), pairwise = dplyr::bind_rows(pairs))
}

# ============================================================================ #
# SECTION 2: RUN THE TESTS ----
# ============================================================================ #
tasks <- list()
for (o in TEST_OUTCOMES) for (s in TEST_STRATIFIERS) {
    tasks[[length(tasks) + 1]] <- list(outcome = o, stratifier = s,
                                       data = dat[, c("wave", WEIGHT_VAR, o, s)])
}
log_msg("Fitting ", length(tasks), " outcome x stratifier series on ", N_CORES, " core(s)")
res <- run_tasks(tasks, test_stratifier, export = "MIN_CELL_N")

# ============================================================================ #
# SECTION 3: WRITE THE RESULTS AND STORE ----
# ============================================================================ #
dates <- wave_dates(dat)
omnibus <- dplyr::bind_rows(lapply(res, `[[`, "omnibus")) %>%
    dplyr::group_by(outcome, stratifier) %>%
    dplyr::mutate(p_adj_BH = stats::p.adjust(p_value, method = "BH"),
                  significant = p_adj_BH < 0.05) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dates, by = "wave") %>%
    dplyr::mutate(era = get_era(wave))
write_out(omnibus, OUT_EST, "omnibus_tests_by_wave.csv")

pairwise <- dplyr::bind_rows(lapply(res, `[[`, "pairwise"))
if (isTRUE(RUN_PAIRWISE_RD) && nrow(pairwise) == 0)
    warning("No pairwise risk differences could be computed (see marginaleffects).")
if (isTRUE(RUN_PAIRWISE_RD) && nrow(pairwise) > 0) {
    pairwise <- pairwise %>%
        dplyr::group_by(outcome, stratifier, contrast) %>%
        dplyr::mutate(p_adj_BH = stats::p.adjust(p_pair, method = "BH"),
                      significant = p_adj_BH < 0.05) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(dates, by = "wave") %>%
        dplyr::mutate(era = get_era(wave))
    write_out(pairwise, OUT_EST, "pairwise_rd_by_wave.csv")
}
# Share of significant results 
share <- omnibus %>%
    dplyr::group_by(outcome, stratifier) %>%
    dplyr::summarise(n_waves = dplyr::n(), n_sig_BH = sum(significant, na.rm = TRUE),
                     pct_sig_BH = round(100 * n_sig_BH / n_waves, 1),
                     median_F = stats::median(F_stat, na.rm = TRUE),
                     median_p_adj_BH = stats::median(p_adj_BH, na.rm = TRUE),
                     first_wave = min(wave), last_wave = max(wave)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(match(outcome, TEST_OUTCOMES), dplyr::desc(pct_sig_BH))
write_out(share, OUT_TAB, "significance_share.csv")

log_msg("Done: 03_descriptive_inference.R")
