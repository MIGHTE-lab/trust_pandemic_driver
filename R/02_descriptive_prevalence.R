## ============================================================================ #
## 02_descriptive_prevalence.R
## Unadjusted, survey-weighted prevalence of each behavior by wave within the
## levels of each stratifier, the distribution of trust over time, and the
## adherence gap (highest minus lowest group) used in the Results.
##
## Input : data/chip50_trust_analysis.rds
## Output: output/estimates/prevalence_by_stratum.csv  Figs 1, 2, 5, 6 and S1-S10
##         output/estimates/trust_level_shares.csv     Fig S19; wave dates
##         output/estimates/adherence_gap_by_wave.csv  max - min prevalence by wave
##         output/tables/adherence_gap_summary.csv     mean gaps quoted in Results
##                                                     (38.8 pp masking by CDC trust,
##                                                     25.9 pp by party, 6-10 pp by
##                                                     age, education, income)
## Original scripts: mosaic_and_adherence_gap_plots.R, mosaic_trust_trajectories.R
## ============================================================================ #

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))

dat   <- load_analysis_data()           # 23 analysis waves, valid weights
dates <- wave_dates(dat)

# ---- 1. Prevalence by wave and stratum (mosaic figures) ------------------------
FIG_OUTCOMES <- c("always_wear_mask", "wash_hands_strict", "always_avoid_contact",
                  "always_avoid_crowds", "vaccine_1", "visit_doctor", "visit_store")
FIG_STRATIFIERS <- c("trust_cdc", "trust_banks",                       # focal panels
                     "age_cat", "gender", "income_cat_5_fct", "education_cat",   # core
                     "party3", "race_fct", "employ_fct", "urban_type")         # secondary

log_msg("Prevalence by stratum: ", length(FIG_OUTCOMES), " outcomes x ",
        length(FIG_STRATIFIERS), " stratifiers")
prev <- dplyr::bind_rows(lapply(FIG_OUTCOMES, function(o) {
    dplyr::bind_rows(lapply(FIG_STRATIFIERS, function(s) {
        weighted_prevalence(dat, o, s) %>%
            dplyr::transmute(outcome = o, stratifier = s,
                             group = as.character(.data[[s]]), wave,
                             pct = percentage, ci_lo = ci_lower_pct,
                             ci_hi = ci_upper_pct, n = total_n)
    }))
})) %>%
    dplyr::left_join(dates, by = "wave") %>%
    dplyr::relocate(wave_date, .after = wave)
write_out(prev, OUT_EST, "prevalence_by_stratum.csv")

# ---- 2. Share of respondents at each trust level (Fig S19) ----------------------
TRUST_PANELS <- c(trust_cdc = "Trust in CDC", trust_doc = "Trust in Doctors & Hospitals",
                  trust_state = "Trust in State Government", trust_banks = "Trust in Banks")
shares <- dplyr::bind_rows(lapply(names(TRUST_PANELS), function(tv) {
    d <- dat[!is.na(dat[[tv]]), c("wave", WEIGHT_VAR, tv)]
    dplyr::bind_rows(lapply(TRUST_LEVELS, function(lv) {
        d$level_ind <- as.integer(as.character(d[[tv]]) == lv)
        weighted_prevalence(d, "level_ind") %>%
            dplyr::transmute(trust_var = tv, trust_label = TRUST_PANELS[[tv]],
                             trust_level = lv, wave,
                             weighted_prevalence_pct = percentage,
                             ci_lower_pct, ci_upper_pct, total_n)
    }))
})) %>%
    dplyr::left_join(dates, by = "wave") %>%
    dplyr::relocate(wave_date, .after = wave)
write_out(shares, OUT_EST, "trust_level_shares.csv")

# ---- 3. Adherence gap: highest minus lowest group prevalence --------------------
GAP_STRATIFIERS <- c("age_cat", "gender", "income_cat_5_fct", "education_cat",
                     "race_fct", "party3", "employ_fct", "urban_type",
                     "trust_cdc", "trust_banks")
gap <- prev %>%
    dplyr::filter(stratifier %in% GAP_STRATIFIERS) %>%
    dplyr::group_by(outcome, predictor = stratifier, wave, wave_date) %>%
    dplyr::summarise(max_pct = max(pct), min_pct = min(pct),
                     max_group = group[which.max(pct)],
                     min_group = group[which.min(pct)]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gap_pct = max_pct - min_pct,
                  predictor_label = unname(STRATIFIER_LABELS[predictor]))
# The gap series end with the last wave that has trust data (wave 24).
last_trust <- gap %>% dplyr::filter(predictor == "trust_cdc") %>%
    dplyr::group_by(outcome) %>% dplyr::summarise(last_wave = max(wave))
gap <- gap %>%
    dplyr::inner_join(last_trust, by = "outcome") %>%
    dplyr::filter(wave <= last_wave) %>%
    dplyr::select(wave, wave_date, max_pct, min_pct, max_group, min_group, gap_pct,
                  outcome, predictor, predictor_label) %>%
    dplyr::arrange(outcome, predictor, wave)
write_out(gap, OUT_EST, "adherence_gap_by_wave.csv")

gap_summary <- gap %>%
    dplyr::group_by(outcome, predictor, predictor_label) %>%
    dplyr::summarise(n_waves = dplyr::n(), mean_gap_pp = mean(gap_pct),
                     median_gap_pp = stats::median(gap_pct),
                     min_gap_pp = min(gap_pct), max_gap_pp = max(gap_pct)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(match(outcome, FIG_OUTCOMES), dplyr::desc(mean_gap_pp))
write_out(gap_summary, OUT_TAB, "adherence_gap_summary.csv")

log_msg("Done: 02_descriptive_prevalence.R")
