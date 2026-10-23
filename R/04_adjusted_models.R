## ============================================================================ #
## 04_adjusted_models.R
## Multivariable survey-weighted logistic regressions of each behavior on
## institutional trust plus sociodemographic and political covariates:
##   * per wave (20 waves; vaccination 10), for four trust exposures and up to
##     three specifications (primary; party and ideology removed; primary +
##     generalized trust);
##   * pooled by pandemic era (Spec A: era pooled; Spec B: + wave fixed effects).
## Trust in banks, the negative-control exposure, is always modeled jointly
## with trust in the CDC. The adjusted risk difference (RD) is the marginal
## (standardized) difference in predicted probability between high and no
## trust, averaged over the observed covariates with survey weights.
##
## Input : data/chip50_trust_analysis.rds
## Output (one file per exposure: trust_cdc, trust_doc, trust_state, trust_banks)
##   output/estimates/adjusted_rd_by_wave_<exposure>.csv   Figs 3-4, S17; RDs in text
##   output/estimates/adjusted_or_by_wave_<exposure>.csv   all model terms (ORs)
##   output/estimates/pooled_era_or_<exposure>.csv         Figs S11-S16; CDC ORs
##                                                         of the joint bank models
##   output/estimates/pooled_era_rd_<exposure>.csv         era RDs (bank RDs 2.7/4.5/
##                                                         6.2 pp; "13 to 27 pp")
## Original script: 12_AdjustedTimeVarying_with_trust.R
## ============================================================================ #

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))
use_packages(c("broom", "marginaleffects"))

# ---- 1. Model specifications ------------------------------------------------------
ADJ_OUTCOMES <- c("always_wear_mask", "always_avoid_contact", "always_avoid_crowds",
                  "wash_hands_strict", "vaccine_1", "visit_9")
COVARIATES <- c("age_cat4", "gender", "race_cat4", "education_hs_ref",
                "income_high_ref", "ruca_cat_fct", "party_ind_ref", "ideology_fct",
                "region_fct", "employ_fct")
PARTISAN <- c("party_ind_ref", "ideology_fct")

#' Right-hand side for one exposure x specification. The negative control
#' (trust_banks) is entered jointly with trust_cdc (Lipsitch et al. 2010) and
#' has no generalized-trust specification.
model_covariates <- function(focal, spec) {
    rhs <- switch(spec,
                  primary                   = c(focal, COVARIATES),
                  sensitivity_no_partisan   = c(focal, setdiff(COVARIATES, PARTISAN)),
                  sensitivity_general_trust = c(focal, COVARIATES, "trust"))
    if (focal == "trust_banks") rhs <- unique(c(rhs, "trust_cdc"))
    rhs
}
SPECS <- list(trust_cdc   = c("primary", "sensitivity_no_partisan", "sensitivity_general_trust"),
              trust_doc   = c("primary", "sensitivity_no_partisan", "sensitivity_general_trust"),
              trust_state = c("primary", "sensitivity_no_partisan", "sensitivity_general_trust"),
              trust_banks = c("primary", "sensitivity_no_partisan"))

# Term name -> covariate group (used to label and color the forest plots)
TERM_GROUPS <- list(
    c("trust_cdc", "Focal Trust Variable", "trust"),
    c("trust_doc", "Focal Trust Variable", "trust"),
    c("trust_state", "Focal Trust Variable", "trust"),
    c("trust_banks", "Focal Trust Variable", "trust"),
    c("race_cat4", "Race/Ethnicity", "race"), c("ideology_fct", "Ideology", "ideology"),
    c("ruca_cat_fct", "Urbanicity", "urbanicity"), c("education_hs_ref", "Education", "education"),
    c("income_high_ref", "Income", "income"), c("party_ind_ref", "Party", "party"),
    c("region_fct", "Region", "region"), c("employ_fct", "Employment", "employment"),
    c("age_cat4", "Age", "age"), c("gender", "Gender", "gender"))

label_terms <- function(td, focal) {
    td$sdh_group <- NA_character_; td$level_label <- NA_character_
    td$palette_key <- NA_character_
    for (g in TERM_GROUPS) {
        hit <- startsWith(td$term, g[1])
        td$sdh_group[hit] <- g[2]; td$palette_key[hit] <- g[3]
        td$level_label[hit] <- sub(paste0("^", g[1]), "", td$term[hit])
    }
    # trust_cdc as the adjustment variable of the negative-control model
    for (tv in setdiff(names(TRUST_EXPOSURES), focal)) {
        hit <- startsWith(td$term, tv)
        td$sdh_group[hit] <- "Primary Exposure (Adjustment)"
        td$palette_key[hit] <- "primary_adjust"
        td$level_label[hit] <- sub(paste0("^", tv), "", td$term[hit])
    }
    td$sdh_group[is.na(td$sdh_group)] <- "Other"          # generalized trust
    td$level_label <- ifelse(is.na(td$level_label), gsub("^[a-z_]+", "", td$term),
                             td$level_label)
    td
}

#' Fit one survey-weighted model and return its ORs and the high-vs-no-trust RD.
#' A task is one exposure x specification x outcome x wave (or era).
fit_adjusted <- function(task) {
    vars <- c(task$outcome, task$covariates)
    d <- ADJ_DAT[ADJ_DAT$wave %in% task$waves, c("wave", vars, WEIGHT_VAR)]
    d <- d[stats::complete.cases(d[, vars]), , drop = FALSE]      # complete cases
    if (nrow(d) < 200 || length(unique(d[[task$outcome]])) < 2) return(NULL)
    rhs <- task$covariates
    if (isTRUE(task$wave_fe)) {                                   # Spec B
        d$wave_fe <- factor(d$wave)
        if (nlevels(d$wave_fe) >= 2) rhs <- c(rhs, "wave_fe")
    }
    des <- survey::svydesign(ids = ~1, weights = stats::as.formula(paste0("~", WEIGHT_VAR)),
                             data = d)
    fit <- tryCatch(survey::svyglm(stats::as.formula(paste(task$outcome, "~",
                                                           paste(rhs, collapse = " + "))),
                                   design = des, family = stats::quasibinomial()),
                    error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    y <- d[[task$outcome]]
    meta <- list(outcome = task$outcome, n_obs = nrow(d), n_outcome = sum(y == 1),
                 prevalence = mean(y == 1), model_spec = task$spec)
    td <- tryCatch(broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE),
                   error = function(e) NULL)
    if (is.null(td)) return(NULL)
    td <- td[td$term != "(Intercept)" & !startsWith(td$term, "wave_fe"), ]
    # Marginal RD: Pr(behavior | High trust) - Pr(behavior | No trust), averaged
    # over the model sample with survey weights; delta-method SE from vcov(fit).
    ac <- tryCatch(as.data.frame(marginaleffects::avg_comparisons(
        fit, variables = stats::setNames(list(c("No trust", "High trust")), task$focal),
        comparison = "difference", type = "response", vcov = stats::vcov(fit),
        wts = WEIGHT_VAR, newdata = d)), error = function(e) NULL)
    rd <- if (is.null(ac)) NULL else tibble::tibble(
        contrast = "High trust - No trust", estimate = ac$estimate[1],
        std.error = ac$std.error[1], conf.low = ac$conf.low[1],
        conf.high = ac$conf.high[1], p.value = ac$p.value[1])
    list(task = task[c("focal", "spec", "outcome", "wave", "era")],
         meta = meta, tidy = td, rd = rd,
         waves_in_model = sort(unique(d$wave)))
}

# ---- 2. Data and tasks -------------------------------------------------------------
dat <- load_analysis_data()
dat$ideology_fct <- stats::relevel(dat$ideology_fct, ref = "Moderate")
ADJ_DAT <- dat[, unique(c("wave", WEIGHT_VAR, ADJ_OUTCOMES, names(TRUST_EXPOSURES),
                          "trust", COVARIATES))]
ADJ_DAT <- as.data.frame(ADJ_DAT)
waves <- sort(unique(ADJ_DAT$wave))
era_waves <- split(waves, get_era(waves))[ERA_LEVELS]

tasks <- list()
for (focal in names(SPECS)) for (spec in SPECS[[focal]]) for (o in ADJ_OUTCOMES) {
    for (w in waves) tasks[[length(tasks) + 1]] <- list(
        type = "wave", focal = focal, spec = spec, outcome = o, wave = w,
        era = get_era(w), waves = w, wave_fe = FALSE,
        covariates = model_covariates(focal, spec))
}
for (focal in names(SPECS)) for (o in ADJ_OUTCOMES) for (era in ERA_LEVELS) {
    for (pooled in c("pooled_specA_base", "pooled_specB_waveFE"))
        tasks[[length(tasks) + 1]] <- list(
            type = "pooled", focal = focal, spec = pooled, outcome = o, wave = NA,
            era = era, waves = era_waves[[era]],
            wave_fe = pooled == "pooled_specB_waveFE",
            covariates = model_covariates(focal, "primary"))
}
log_msg("Fitting ", length(tasks), " models (", sum(vapply(tasks, function(t) t$type == "wave", TRUE)),
        " wave-level, the rest era-pooled) on ", N_CORES, " core(s)")
res <- run_tasks(tasks, fit_adjusted, export = "ADJ_DAT")
ok <- !vapply(res, is.null, logical(1))
log_msg(sum(ok), " models fitted; ", sum(!ok),
        " cells skipped (no trust or vaccination item, or n < 200)")
res <- res[ok]; is_wave <- vapply(tasks[ok], function(t) t$type == "wave", TRUE)

# ---- 3. Assemble and write the tables ----------------------------------------------
or_rows <- function(r, pooled) {
    td <- label_terms(r$tidy, r$task$focal)
    out <- tibble::tibble(term = td$term, estimate = td$estimate, std_error = td$std.error,
                          statistic = td$statistic, p.value = td$p.value,
                          conf_low = td$conf.low, conf_high = td$conf.high,
                          outcome = r$meta$outcome)
    if (pooled) {
        out$era <- r$task$era
    } else {
        out$wave <- as.integer(r$task$wave); out$era <- r$task$era
    }
    out$n_obs <- r$meta$n_obs; out$n_outcome <- r$meta$n_outcome
    out$prevalence <- r$meta$prevalence
    if (pooled) {
        out$n_waves <- length(r$waves_in_model)
        out$waves_in_era <- paste(r$waves_in_model, collapse = ",")
    }
    out$model_spec <- r$meta$model_spec; out$weighted <- TRUE
    out$sdh_group <- td$sdh_group; out$level_label <- td$level_label
    out$palette_key <- td$palette_key
    out
}
rd_rows <- function(r, pooled) {
    if (is.null(r$rd)) return(NULL)
    out <- tibble::tibble(contrast = r$rd$contrast, estimate = r$rd$estimate,
                          std_error = r$rd$std.error, conf_low = r$rd$conf.low,
                          conf_high = r$rd$conf.high, p_value = r$rd$p.value,
                          outcome = r$meta$outcome)
    if (pooled) {
        out$era <- r$task$era; out$n_waves <- length(r$waves_in_model)
        out$waves_in_era <- paste(r$waves_in_model, collapse = ",")
    } else {
        out$wave <- as.integer(r$task$wave); out$era <- r$task$era
    }
    out$model_spec <- r$meta$model_spec
    out
}
spec_order <- c("primary", "sensitivity_no_partisan", "sensitivity_general_trust",
                "pooled_specA_base", "pooled_specB_waveFE")

for (focal in names(SPECS)) {
    rf <- res[vapply(res, function(r) r$task$focal == focal, TRUE)]
    wv <- vapply(rf, function(r) !is.na(r$task$wave), TRUE)
    or_w <- dplyr::bind_rows(lapply(rf[wv], or_rows, pooled = FALSE)) %>%
        dplyr::mutate(p_value = p.value) %>%
        dplyr::group_by(outcome, term, model_spec) %>%
        dplyr::mutate(p_adj = stats::p.adjust(p_value, method = "BH")) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(match(model_spec, spec_order), match(outcome, ADJ_OUTCOMES), wave)
    rd_w <- dplyr::bind_rows(lapply(rf[wv], rd_rows, pooled = FALSE)) %>%
        dplyr::group_by(outcome, contrast, model_spec) %>%
        dplyr::mutate(p_adj = stats::p.adjust(p_value, method = "BH")) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(match(model_spec, spec_order), match(outcome, ADJ_OUTCOMES), wave)
    or_p <- dplyr::bind_rows(lapply(rf[!wv], or_rows, pooled = TRUE)) %>%
        dplyr::mutate(p_value = p.value) %>%
        dplyr::group_by(outcome, term, era, model_spec) %>%
        dplyr::mutate(p_adj = stats::p.adjust(p_value, method = "BH")) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(match(outcome, ADJ_OUTCOMES), match(era, ERA_LEVELS),
                       match(model_spec, spec_order))
    rd_p <- dplyr::bind_rows(lapply(rf[!wv], rd_rows, pooled = TRUE)) %>%
        dplyr::group_by(outcome, contrast, era, model_spec) %>%
        dplyr::mutate(p_adj = stats::p.adjust(p_value, method = "BH")) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(match(outcome, ADJ_OUTCOMES), match(era, ERA_LEVELS),
                       match(model_spec, spec_order))
    write_out(rd_w, OUT_EST, sprintf("adjusted_rd_by_wave_%s.csv", focal))
    write_out(or_w, OUT_EST, sprintf("adjusted_or_by_wave_%s.csv", focal))
    write_out(or_p, OUT_EST, sprintf("pooled_era_or_%s.csv", focal))
    write_out(rd_p, OUT_EST, sprintf("pooled_era_rd_%s.csv", focal))
}

log_msg("Done: 04_adjusted_models.R")
