## ============================================================================ #
## 05_sequential_models.R
## Sequential block adjustment (Table 1) and leave-one-block-out (LOBO)
## comparisons (Table S1) for the four primary behaviors, per wave.
##
##   M0 trust in CDC -> M1 + demographics (age, race/ethnicity, gender)
##   -> M2 + SES (education, income, employment) -> M3 + ideology and party
##   -> M4 + geography (RUCA urbanicity, Census region)
##
## All models of an outcome x wave are fit on the same complete-case sample:
##   * survey-weighted quasibinomial (svyglm): trust OR, standardized risk
##     difference (g-computation; see TABLE1_RD_AVERAGE), weighted Brier score,
##     design-based Wald tests;
##   * unweighted binomial ML (glm): AIC, which is undefined for svyglm.
## LOBO drops each block from M4 in turn (unweighted AIC and Brier; Wald test
## of the block on the weighted M4).
##
## Input : data/chip50_trust_analysis.rds
## Output: output/estimates/sequential_models_by_wave.csv  per wave and model
##         output/estimates/lobo_by_wave.csv               per wave and block
##         output/tables/table_1_sequential_adjustment.csv  Table 1 (median [IQR]
##                                                          across waves)
## Settings: LOBO_BRIER ("unweighted" as in published Table S1, or "weighted");
##           TABLE1_RD_AVERAGE ("unweighted" as in published Table 1, or "weighted").
## Original script: 14b_iterative_trust_model_comparison_v2.R
## ============================================================================ #

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))

# ---- 1. Blocks and models ---------------------------------------------------------
BLOCKS <- list(trust          = "trust_cdc",
               demographics   = c("age_cat4", "race_cat4", "gender_fct"),
               ses            = c("education_hs_ref", "income_high_ref", "employ_fct"),
               ideology_party = c("ideology_fct", "party_dem_ref"),
               geography      = c("ruca_cat_fct", "region_fct"))
BLOCK_LABELS <- c(trust = "Trust in CDC", demographics = "Demographics",
                  ses = "Socioeconomic", ideology_party = "Ideology & party",
                  geography = "Urbanicity & region")
MODELS <- list(Mint = character(0),
               M0 = "trust",
               M1 = c("trust", "demographics"),
               M2 = c("trust", "demographics", "ses"),
               M3 = c("trust", "demographics", "ses", "ideology_party"),
               M4 = c("trust", "demographics", "ses", "ideology_party", "geography"))
MODEL_LABELS <- c(M0 = "Trust only", M1 = "Trust + demographics",
                  M2 = "Trust + demographics + SES",
                  M3 = "Trust + demographics + SES + ideology/party",
                  M4 = "Trust + demographics + SES + ideology/party + geography")
ADDED_BLOCK <- c(M0 = "Trust", M1 = "+ Demographics", M2 = "+ Socioeconomic",
                 M3 = "+ Ideology/party", M4 = "+ Urbanicity/region")
ALL_COVS <- unlist(BLOCKS, use.names = FALSE)

model_formula <- function(outcome, blocks) {
    vars <- unlist(BLOCKS[blocks], use.names = FALSE)
    stats::as.formula(paste(outcome, "~", if (length(vars)) paste(vars, collapse = " + ") else "1"))
}
brier <- function(y, p, w = rep(1, length(y))) sum(w * (y - p)^2) / sum(w)
tjur  <- function(y, p, w = rep(1, length(y)))
    sum(w * p * (y == 1)) / sum(w[y == 1]) - sum(w * p * (y == 0)) / sum(w[y == 0])
quiet <- function(expr) suppressWarnings(tryCatch(expr, error = function(e) NULL))

#' All models for one outcome x wave cell.
run_cell <- function(task) {
    o <- task$outcome
    vars <- c(o, ALL_COVS)
    d <- SEQ_DAT[SEQ_DAT$wave == task$wave, c(vars, WEIGHT_VAR)]
    cc <- droplevels(d[stats::complete.cases(d[, vars]), , drop = FALSE])
    if (nrow(cc) < 200 || length(unique(cc[[o]])) < 2) return(NULL)
    y <- cc[[o]]; w <- cc[[WEIGHT_VAR]]
    des <- survey::svydesign(ids = ~1, weights = stats::as.formula(paste0("~", WEIGHT_VAR)),
                             data = cc)
    drop_sets <- lapply(names(BLOCKS), function(b) setdiff(MODELS$M4, b))
    names(drop_sets) <- paste0("M4_drop_", names(BLOCKS))
    glm_sets <- c(MODELS, drop_sets)
    svy_sets <- MODELS[c("M0", "M1", "M2", "M3", "M4")]
    if (LOBO_BRIER == "weighted") svy_sets <- c(svy_sets, drop_sets)

    glms <- lapply(glm_sets, function(b) quiet(stats::glm(model_formula(o, b), data = cc,
                                                         family = stats::binomial())))
    svys <- lapply(svy_sets, function(b) quiet(survey::svyglm(model_formula(o, b), design = des,
                                                             family = stats::quasibinomial())))
    wald <- function(fit, block) {
        if (is.null(fit)) return(c(F = NA, df1 = NA, df2 = NA, p = NA))
        tt <- quiet(survey::regTermTest(fit, stats::as.formula(paste("~", paste(BLOCKS[[block]], collapse = " + "))),
                                        method = "Wald"))
        if (is.null(tt)) return(c(F = NA, df1 = NA, df2 = NA, p = NA))
        c(F = as.numeric(tt$Ftest), df1 = as.numeric(tt$df), df2 = as.numeric(tt$ddf),
          p = as.numeric(tt$p))
    }

    # --- per model: trust OR (High vs No), standardized RD, fit statistics ------
    model_rows <- lapply(names(svy_sets)[names(svy_sets) %in% names(MODELS)], function(m) {
        fit <- svys[[m]]; g <- glms[[m]]
        g_prior <- glms[[names(MODELS)[match(m, names(MODELS)) - 1]]]   # M0: intercept only
        out <- tibble::tibble(outcome = o, wave = task$wave, era = get_era(task$wave),
                              model_id = m, n = nrow(cc), events = sum(y == 1),
                              AIC = if (is.null(g)) NA_real_ else stats::AIC(g),
                              delta_AIC_vs_prior = if (is.null(g) || is.null(g_prior)) NA_real_
                                                   else stats::AIC(g) - stats::AIC(g_prior))
        if (is.null(fit)) return(out)
        p <- as.numeric(stats::fitted(fit))
        out$Brier_w <- brier(y, p, w)                # survey-weighted Brier score
        out$Tjur_w <- tjur(y, p, w)
        # Wald test of the block this model adds to the previous one (M0: trust)
        added <- setdiff(MODELS[[m]], MODELS[[match(m, names(MODELS)) - 1]])
        wt <- wald(fit, added)
        out$block_Wald_F <- wt[["F"]]; out$block_Wald_p <- wt[["p"]]
        nm <- "trust_cdcNo trust"                    # reference level: High trust
        if (!nm %in% names(stats::coef(fit)) || is.na(stats::coef(fit)[[nm]])) return(out)
        beta <- stats::coef(fit)[[nm]]
        se <- sqrt(stats::vcov(fit)[nm, nm])
        or <- exp(beta)
        out$trust_logOR <- -beta                     # High vs No trust
        out$trust_OR <- 1 / or
        out$trust_OR_lo <- 1 / exp(beta + 1.96 * se)
        out$trust_OR_hi <- 1 / exp(beta - 1.96 * se)
        out$trust_p <- 2 * stats::pnorm(-abs(beta / se))
        # Standardized RD (g-computation): set everyone to High, then No trust,
        # and compare the mean predicted probabilities. As in the published
        # Table 1, the mean is unweighted by default (TABLE1_RD_AVERAGE).
        lv <- levels(cc$trust_cdc)
        w_avg <- if (TABLE1_RD_AVERAGE == "weighted") w else rep(1, length(w))
        pp <- vapply(c("High trust", "No trust"), function(l) {
            nd <- cc; nd$trust_cdc <- factor(l, levels = lv)
            stats::weighted.mean(as.numeric(stats::predict(fit, newdata = nd, type = "response")),
                                 w_avg, na.rm = TRUE)
        }, numeric(1))
        out$prob_diff <- pp[["High trust"]] - pp[["No trust"]]
        out
    })

    # --- leave one block out of M4 ------------------------------------------------
    full <- glms$M4
    lobo_rows <- lapply(names(BLOCKS), function(b) {
        red_id <- paste0("M4_drop_", b); red <- glms[[red_id]]
        if (is.null(full) || is.null(red)) return(NULL)
        ll_f <- as.numeric(stats::logLik(full)); ll_r <- as.numeric(stats::logLik(red))
        df_f <- sum(!is.na(stats::coef(full))); df_r <- sum(!is.na(stats::coef(red)))
        wt <- wald(svys$M4, b)
        out <- tibble::tibble(
            outcome = o, outcome_label = unname(OUTCOME_LABELS[o]), wave = task$wave,
            era = get_era(task$wave), comparison_type = "drop_one",
            reduced_model_id = red_id, full_model_id = "M4",
            added_block = unname(BLOCK_LABELS[b]), n = nrow(cc), events = sum(y == 1),
            LR_chisq = 2 * (ll_f - ll_r), LR_df = df_f - df_r,
            LR_p = if (df_f > df_r) stats::pchisq(2 * (ll_f - ll_r), df_f - df_r,
                                                  lower.tail = FALSE) else NA_real_,
            AIC_reduced = stats::AIC(red), AIC_full = stats::AIC(full),
            delta_AIC_full_minus_reduced = stats::AIC(full) - stats::AIC(red),
            Brier_reduced = brier(y, stats::fitted(red)),
            Brier_full = brier(y, stats::fitted(full)),
            delta_Brier_full_minus_reduced = brier(y, stats::fitted(full)) -
                brier(y, stats::fitted(red)),
            test_terms = paste(BLOCKS[[b]], collapse = " + "),
            Wald_F = wt[["F"]], Wald_df1 = wt[["df1"]], Wald_df2 = wt[["df2"]],
            Wald_p = wt[["p"]])
        if (LOBO_BRIER == "weighted" && !is.null(svys[[red_id]]) && !is.null(svys$M4)) {
            out$Brier_w_reduced <- brier(y, as.numeric(stats::fitted(svys[[red_id]])), w)
            out$Brier_w_full <- brier(y, as.numeric(stats::fitted(svys$M4)), w)
            out$delta_Brier_w_full_minus_reduced <- out$Brier_w_full - out$Brier_w_reduced
        }
        out
    })
    list(models = dplyr::bind_rows(model_rows), lobo = dplyr::bind_rows(lobo_rows))
}

# ---- 2. Fit ---------------------------------------------------------------------------
dat <- load_analysis_data()
dat$trust_cdc <- stats::relevel(dat$trust_cdc, ref = "High trust")
SEQ_DAT <- as.data.frame(dat[, c("wave", WEIGHT_VAR, PRIMARY_OUTCOMES, ALL_COVS)])
tasks <- list()
for (o in PRIMARY_OUTCOMES) for (w in sort(unique(SEQ_DAT$wave)))
    tasks[[length(tasks) + 1]] <- list(outcome = o, wave = w)
log_msg("Fitting sequential and LOBO models for ", length(tasks),
        " outcome x wave cells on ", N_CORES, " core(s)")
res <- run_tasks(tasks, run_cell,
                 export = c("SEQ_DAT", "BLOCKS", "BLOCK_LABELS", "MODELS", "ALL_COVS",
                            "model_formula", "brier", "tjur", "quiet"))
res <- res[!vapply(res, is.null, logical(1))]
log_msg(length(res), " cells fitted (cells without trust data are skipped)")

# ---- 3. Per-wave results ----------------------------------------------------------------
per_wave <- dplyr::bind_rows(lapply(res, `[[`, "models")) %>%
    dplyr::group_by(outcome, wave) %>%
    dplyr::mutate(
        pct_change_logOR_vs_M0 = {
            b0 <- trust_logOR[model_id == "M0"][1]
            if (is.finite(b0) && abs(b0) > 1e-8) 100 * (trust_logOR - b0) / abs(b0) else NA_real_
        },
        delta_AIC_vs_M0   = AIC - AIC[model_id == "M0"][1],
        delta_Brier_vs_M0 = Brier_w - Brier_w[model_id == "M0"][1],
        delta_Tjur_vs_M0  = Tjur_w - Tjur_w[model_id == "M0"][1]) %>%
    dplyr::group_by(outcome, model_id) %>%          # trust term: across waves
    dplyr::mutate(trust_q = stats::p.adjust(trust_p, method = "BH")) %>%
    dplyr::group_by(model_id) %>%                   # added block: across outcomes and waves
    dplyr::mutate(block_Wald_q = stats::p.adjust(block_Wald_p, method = "BH")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(outcome_label = unname(OUTCOME_LABELS[outcome])) %>%
    dplyr::relocate(outcome_label, .after = outcome) %>%
    dplyr::arrange(match(outcome, PRIMARY_OUTCOMES), wave, model_id)
write_out(per_wave, OUT_EST, "sequential_models_by_wave.csv")

# BH adjustment across all outcomes and waves of each dropped block (as in 14b)
lobo <- dplyr::bind_rows(lapply(res, `[[`, "lobo")) %>%
    dplyr::group_by(comparison_type, reduced_model_id, full_model_id) %>%
    dplyr::mutate(LR_p_adj = stats::p.adjust(LR_p, method = "BH"),
                  Wald_p_adj = stats::p.adjust(Wald_p, method = "BH")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(match(outcome, PRIMARY_OUTCOMES), wave, match(reduced_model_id,
                                                                 paste0("M4_drop_", names(BLOCKS))))
write_out(lobo, OUT_EST, "lobo_by_wave.csv")

# ---- 4. Table 1: median [IQR] across waves ----------------------------------------
fmt_count <- function(hit, total) ifelse(total == 0, NA_character_,
                                         sprintf("%d / %d (%.0f%%)", hit, total, 100 * hit / total))
table1 <- per_wave %>%
    dplyr::filter(!is.na(trust_OR)) %>%
    dplyr::group_by(outcome, model_id) %>%
    dplyr::summarise(
        number_of_waves_analyzed = dplyr::n(),
        median_n = as.integer(round(stats::median(n))),
        median_events = as.integer(round(stats::median(events))),
        trust_OR_median_IQR = fmt_med_iqr(trust_OR, 2),
        trust_probability_difference_median_IQR = fmt_med_iqr(prob_diff, 3),
        pct_change_trust_logOR_vs_M0_median_IQR = fmt_med_iqr(pct_change_logOR_vs_M0, 1),
        delta_AIC_vs_M0_median_IQR = fmt_med_iqr(delta_AIC_vs_M0, 2),
        delta_Tjur_R2_vs_M0_median_IQR = fmt_med_iqr(delta_Tjur_vs_M0, 4),
        delta_Brier_vs_M0_median_IQR = fmt_med_iqr(delta_Brier_vs_M0, 4),
        waves_with_same_trust_direction = {
            pd <- prob_diff[is.finite(prob_diff)]
            np <- sum(pd > 0); nn <- sum(pd < 0)
            sprintf("%d / %d (%s)", max(np, nn), length(pd),
                    if (np >= nn) "positive (High trust > No trust)"
                    else "negative (High trust < No trust)")
        },
        waves_with_trust_q_lt_0_05 = fmt_count(sum(trust_q < 0.05, na.rm = TRUE),
                                               sum(!is.na(trust_q))),
        block_tests_q_lt_0_05_n = fmt_count(sum(block_Wald_q < 0.05, na.rm = TRUE),
                                            sum(!is.na(block_Wald_q)))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(outcome_label = unname(OUTCOME_LABELS[outcome]),
                  model_label = unname(MODEL_LABELS[model_id]),
                  added_block = unname(ADDED_BLOCK[model_id]),
                  block_test_type = "Survey-design Wald test (regTermTest)",
                  notes = ifelse(model_id == "M0",
                                 "block test compares M0 vs intercept-only; pct change vs M0 = 0 by definition",
                                 "")) %>%
    dplyr::select(outcome, outcome_label, model_id, model_label, added_block,
                  number_of_waves_analyzed, median_n, median_events,
                  trust_OR_median_IQR, trust_probability_difference_median_IQR,
                  pct_change_trust_logOR_vs_M0_median_IQR, delta_AIC_vs_M0_median_IQR,
                  delta_Tjur_R2_vs_M0_median_IQR, delta_Brier_vs_M0_median_IQR,
                  waves_with_same_trust_direction, waves_with_trust_q_lt_0_05,
                  block_tests_q_lt_0_05_n, block_test_type, notes) %>%
    dplyr::arrange(match(outcome, PRIMARY_OUTCOMES), model_id)
write_out(table1, OUT_TAB, "table_1_sequential_adjustment.csv")

log_msg("Done: 05_sequential_models.R")
