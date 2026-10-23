## ============================================================================ #
## 00_setup.R
## Shared settings, constants and helper functions. Every other script sources
## this file; it does not read any data itself.
##
## Zapf, Dewey et al. (2026) "Trust as a hidden driver of epidemic-relevant
## behavior: Institution- and behavior-specific associations across 20 national
## survey waves." PNAS.
## ============================================================================ #

# ---- Packages ---------------------------------------------------------------
use_packages <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0)
        stop("Please install: install.packages(c(\"",
             paste(missing, collapse = "\", \""), "\"))", call. = FALSE)
    for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
}
use_packages(c("dplyr", "tidyr", "readr", "tibble", "survey"))
options(dplyr.summarise.inform = FALSE)

# ---- Paths --------------------------------------------------------------------
if (!exists("REPO_DIR")) REPO_DIR <- normalizePath(getwd(), winslash = "/")
.env_or <- function(var, default) {
    v <- Sys.getenv(var, unset = "")
    if (nzchar(v)) normalizePath(v, winslash = "/", mustWork = FALSE) else default
}
# Raw CHIP50 waves (CSP_W<wave>_weighted.RDS); needed only by 01_build_analysis_data.R
RAW_DATA_DIR <- .env_or("C50_WEIGHTED_RDS_DIR", NA_character_)
# Slim analysis file written by 01_build_analysis_data.R and read by 02-05
DATA_FILE    <- .env_or("C50_ANALYSIS_FILE",
                        file.path(REPO_DIR, "data", "chip50_trust_analysis.rds"))
OUT_DIR      <- .env_or("C50_OUTPUT_DIR", file.path(REPO_DIR, "output"))
OUT_EST      <- file.path(OUT_DIR, "estimates")  # wave-level estimates (CSV)
OUT_TAB      <- file.path(OUT_DIR, "tables")     # tables and in-text numbers (CSV)
OUT_FIG      <- file.path(OUT_DIR, "figures")    # PDF, PNG and TIFF figures

# ---- Run settings -------------------------------------------------------------
# Defaults reproduce the published numbers. To change one, assign the variable
# before sourcing a script (run_all.R shows how).
setting <- function(name, default) {
    if (exists(name, envir = .GlobalEnv, inherits = FALSE)) get(name, envir = .GlobalEnv)
    else default
}
# Waves used for the significance shares of the SDH stratifiers (script 03).
#   "published":   all 23 analysis waves (Apr 2020 - early 2023), as in the
#                  published shares (e.g. 87/91/74% for doctor visits).
#   "trust_waves": the 20 waves with trust items (Apr 2020 - Aug 2022), as the
#                  Methods describe. Trust stratifiers always use these 20.
SDH_TEST_WAVES <- setting("SDH_TEST_WAVES", "published")
# Brier score used for the leave-one-block-out (LOBO) penalties (Table S1).
#   "unweighted": unweighted ML fits, as in published Table S1.
#   "weighted":   survey-weighted fits, as the Table S1 footnote states.
LOBO_BRIER <- setting("LOBO_BRIER", "unweighted")
# Averaging of the predicted probabilities behind the risk differences of
# Table 1 (script 05). The models are survey-weighted either way.
#   "unweighted": equal weight for each respondent, as in published Table 1.
#   "weighted":   survey weights, as for the wave-level RDs of script 04.
TABLE1_RD_AVERAGE <- setting("TABLE1_RD_AVERAGE", "unweighted")
# Pairwise risk differences between stratifier levels (script 03, Methods).
# They are not needed for any reported number and take most of the run time.
RUN_PAIRWISE_RD <- setting("RUN_PAIRWISE_RD", TRUE)
# Mosaic figures: show the SDH panels only for the 20 waves with trust data
# (TRUE, as in the final figures) or for all 23 waves (FALSE).
MOSAIC_MATCH_TRUST_WAVES <- setting("MOSAIC_MATCH_TRUST_WAVES", TRUE)
# Parallel workers for scripts 03-05 (1 = run sequentially).
N_CORES <- setting("N_CORES", max(1L, min(8L, parallel::detectCores() - 2L), na.rm = TRUE))

stopifnot(SDH_TEST_WAVES %in% c("published", "trust_waves"),
          LOBO_BRIER %in% c("unweighted", "weighted"),
          TABLE1_RD_AVERAGE %in% c("unweighted", "weighted"),
          isTRUE(RUN_PAIRWISE_RD) || isFALSE(RUN_PAIRWISE_RD),
          isTRUE(MOSAIC_MATCH_TRUST_WAVES) || isFALSE(MOSAIC_MATCH_TRUST_WAVES),
          is.numeric(N_CORES), length(N_CORES) == 1, N_CORES >= 1)

# ---- Study design constants -----------------------------------------------------
WEIGHT_VAR     <- "weight_state"          # post-stratification weights
EXCLUDED_WAVES <- c(4, 6, 8, 15)          # small samples (4, 6, 8; no weights) and wave 15
ANALYSIS_WAVES <- setdiff(1:27, EXCLUDED_WAVES)   # 23 waves kept in the analysis file
TRUST_WAVES    <- setdiff(1:24, EXCLUDED_WAVES)   # 20 waves with institutional-trust items
LANDMARK_WAVES <- c("Pre-vaccine" = 5L, "Rollout" = 16L, "Omicron" = 21L)
ERA_LEVELS     <- c("Pre-vaccine", "Rollout", "Omicron")

get_era <- function(wave) {
    dplyr::case_when(wave %in% 1:13  ~ "Pre-vaccine",   # Apr - Nov 2020
                     wave %in% 14:20 ~ "Rollout",       # Dec 2020 - Nov 2021
                     wave %in% 21:28 ~ "Omicron",       # Jan 2022 onward
                     TRUE ~ NA_character_)
}

TRUST_LEVELS <- c("No trust", "Not much trust", "Some trust", "High trust")
TRUST_EXPOSURES <- c(trust_cdc   = "Trust in CDC",
                     trust_doc   = "Trust in doctors & hospitals",
                     trust_state = "Trust in state government",
                     trust_banks = "Trust in banks (negative control)")

# Outcomes of the adjusted models (scripts 04-06). visit_9 is the raw doctor/
# hospital item (respondents not asked it are missing); visit_doctor (script
# 02-03) codes them 0, as the descriptive analyses did.
OUTCOME_LABELS <- c(always_wear_mask     = "Mask wearing",
                    always_avoid_contact = "Avoiding contact",
                    always_avoid_crowds  = "Avoiding crowds",
                    wash_hands_strict    = "Handwashing",
                    vaccine_1            = "Vaccination (1+ dose)",
                    visit_9              = "Doctor/hospital visit",
                    visit_doctor         = "Doctor/hospital visit",
                    visit_store          = "Store visit",
                    visit_work           = "Going to work")
PRIMARY_OUTCOMES <- c("always_wear_mask", "always_avoid_contact",
                      "always_avoid_crowds", "wash_hands_strict")

STRATIFIER_LABELS <- c(trust_cdc = "Trust in CDC", trust_banks = "Trust in Banks",
                       age_cat = "Age", gender = "Gender",
                       income_cat_5_fct = "Income Category", education_cat = "Education",
                       race_fct = "Race/Ethnicity", party3 = "Political Affiliation",
                       ideology_fct = "Ideology", employ_fct = "Employment Status",
                       urban_type = "Urbanicity", region_fct = "Region")

# ---- Helpers ------------------------------------------------------------------
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

#' Read the slim analysis file written by 01_build_analysis_data.R.
load_analysis_data <- function(file = DATA_FILE) {
    if (!file.exists(file))
        stop("Analysis file not found: ", file,
             "\nRun R/01_build_analysis_data.R first (see README).", call. = FALSE)
    dat <- readRDS(file)
    log_msg("Loaded ", format(nrow(dat), big.mark = ","), " respondents in ",
            dplyr::n_distinct(dat$wave), " waves from ", basename(file))
    dat
}

#' Field date of each wave: median start date of its respondents.
wave_dates <- function(dat) {
    dat %>%
        dplyr::group_by(wave) %>%
        dplyr::summarise(wave_date = stats::median(survey_date, na.rm = TRUE)) %>%
        dplyr::arrange(wave)
}

#' Survey-weighted prevalence (%) of a 0/1 outcome by wave and group, with
#' design-based (Taylor-linearization) 95% CIs from survey::svymean().
#' One single-stage design (ids = ~1, weights = weight_state) per wave x group.
weighted_prevalence <- function(dat, outcome, groups = character(0),
                                weight_var = WEIGHT_VAR) {
    d <- dat[!is.na(dat[[outcome]]) & !is.na(dat$wave), , drop = FALSE]
    for (g in groups) d <- d[!is.na(d[[g]]), , drop = FALSE]
    d <- d[!is.na(d[[weight_var]]) & d[[weight_var]] > 0, , drop = FALSE]
    d$.y <- as.numeric(d[[outcome]])
    d %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("wave", groups)))) %>%
        dplyr::group_modify(function(sub, key) {
            des <- survey::svydesign(ids = ~1, weights = stats::as.formula(paste0("~", weight_var)),
                                     data = sub)
            est <- survey::svymean(~.y, des, na.rm = TRUE)
            ci  <- as.numeric(stats::confint(est))
            tibble::tibble(total_n = nrow(sub), outcome_n = sum(sub$.y == 1),
                           percentage = 100 * as.numeric(stats::coef(est)),
                           se = 100 * as.numeric(survey::SE(est)),
                           ci_lower_pct = max(0, 100 * ci[1]),
                           ci_upper_pct = min(100, 100 * ci[2]))
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percentage = pmin(pmax(percentage, 0), 100))
}

#' Median [IQR] (quantile type 7) as text, ignoring non-finite values.
fmt_med_iqr <- function(x, digits = 2) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_character_)
    q <- stats::quantile(x, c(0.5, 0.25, 0.75), names = FALSE)
    sprintf(paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"), q[1], q[2], q[3])
}

#' Write a CSV to an output folder and report it.
write_out <- function(df, dir, file) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(dir, file)
    readr::write_csv(df, path, na = "")
    shown <- if (startsWith(path, OUT_DIR))
        paste0("output", substring(path, nchar(OUT_DIR) + 1)) else path
    log_msg("  wrote ", shown, " (", nrow(df), " rows)")
    invisible(path)
}

#' Read an output CSV written by an earlier script.
read_out <- function(dir, file) {
    path <- file.path(dir, file)
    if (!file.exists(path))
        stop("Missing ", path, ". Run the earlier scripts first (see README).",
             call. = FALSE)
    readr::read_csv(path, show_col_types = FALSE, guess_max = 1e5)
}

#' Apply FUN to every element of X, in parallel when N_CORES > 1.
#' Workers source this file, then receive the objects named in `export`.
run_tasks <- function(X, FUN, export = character(0), n_cores = N_CORES) {
    if (n_cores <= 1 || length(X) <= 1) return(lapply(X, FUN))
    cl <- parallel::makeCluster(min(n_cores, length(X)))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, c("SDH_TEST_WAVES", "LOBO_BRIER", "TABLE1_RD_AVERAGE",
                                  "RUN_PAIRWISE_RD", "MOSAIC_MATCH_TRUST_WAVES"),
                            envir = .GlobalEnv)
    parallel::clusterCall(cl, function(repo) {
        assign("REPO_DIR", repo, envir = .GlobalEnv)
        Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
        sys.source(file.path(repo, "R", "00_setup.R"), envir = .GlobalEnv)
        NULL
    }, REPO_DIR)
    if (length(export) > 0) parallel::clusterExport(cl, export, envir = .GlobalEnv)
    parallel::parLapplyLB(cl, X, FUN)
}

for (.d in c(OUT_EST, OUT_TAB, OUT_FIG)) dir.create(.d, recursive = TRUE, showWarnings = FALSE)
rm(.d)
