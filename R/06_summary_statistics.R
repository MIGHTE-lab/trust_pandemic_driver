## ============================================================================ #
## 06_summary_statistics.R
## Summaries of the estimates from scripts 02-05 that are reported in the
## paper, and a check of every reported number against the published value.
## Reads CSV files only; fits nothing.
##
## Output (output/tables/):
##   table_S1_lobo_by_era.csv               Table S1, numeric (median [IQR] of the
##   table_S1_lobo_by_era_formatted.csv     wave-level LOBO penalties, by era)
##   lobo_all_waves.csv                     LOBO medians over all 20 waves (Results:
##                                          +921 trust vs +442 party & ideology, ...)
##   sensitivity_general_trust_comparison.csv  primary vs + generalized trust RDs (Fig S18)
##   fig_S18_concordance_stats.csv          r = 0.99; median change 0.8 pp; ...
##   negative_control_summary.csv           banks vs CDC across waves ("56 to 96%")
##   check_against_published.csv            every reproduced number vs the paper
## Original scripts: 17_reproduce_TableS1_LOBO_and_S18_stats.R,
##   12c_sensitivity_analysis_comparison.R, 12d_negative_control_assessment.R
## ============================================================================ #

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))

q7 <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE, type = 7))
BLOCK_NAMES <- c(trust          = "Trust in the CDC",
                 ideology_party = "Political identity (party & ideology)",
                 demographics   = "Demographics (age, race/ethnicity, gender)",
                 geography      = "Geographic context (urbanicity & region)",
                 ses            = "Socioeconomic status (education, income, employment)")

# ---- 1. Leave-one-block-out penalties (Table S1; Results) -------------------------
lobo_raw <- read_out(OUT_EST, "lobo_by_wave.csv")
brier_col <- if (LOBO_BRIER == "weighted") "delta_Brier_w_full_minus_reduced" else
    "delta_Brier_full_minus_reduced"
if (!brier_col %in% names(lobo_raw))
    stop("lobo_by_wave.csv has no column ", brier_col, ". Re-run ",
         "R/05_sequential_models.R with the same LOBO_BRIER setting (\"", LOBO_BRIER,
         "\") before this script.", call. = FALSE)
lobo <- lobo_raw %>%
    dplyr::filter(comparison_type == "drop_one", full_model_id == "M4") %>%
    dplyr::mutate(block = sub("^M4_drop_", "", reduced_model_id),
                  dAIC_penalty = -delta_AIC_full_minus_reduced,   # AIC(M4 - block) - AIC(M4)
                  dBrier_penalty = -.data[[brier_col]])
penalties <- function(df) dplyr::summarise(df,
    n_waves = dplyr::n_distinct(wave),
    dAIC_median = stats::median(dAIC_penalty), dAIC_q25 = q7(dAIC_penalty, .25),
    dAIC_q75 = q7(dAIC_penalty, .75),
    dBrier_median = stats::median(dBrier_penalty), dBrier_q25 = q7(dBrier_penalty, .25),
    dBrier_q75 = q7(dBrier_penalty, .75),
    waves_block_q_lt_0_05 = sum(Wald_p_adj < 0.05, na.rm = TRUE))
n_by_era <- lobo %>% dplyr::distinct(outcome, era, wave, n) %>%
    dplyr::group_by(outcome, era) %>% dplyr::summarise(median_n = stats::median(n))
table_s1 <- lobo %>% dplyr::group_by(outcome, era, block) %>% penalties() %>%
    dplyr::ungroup() %>%
    dplyr::left_join(n_by_era, by = c("outcome", "era")) %>%
    dplyr::mutate(outcome_label = unname(OUTCOME_LABELS[outcome]),
                  block_label = unname(BLOCK_NAMES[block])) %>%
    dplyr::arrange(match(outcome, PRIMARY_OUTCOMES), match(era, ERA_LEVELS),
                   dplyr::desc(dAIC_median)) %>%
    dplyr::select(outcome, outcome_label, era, block, block_label, n_waves, median_n,
                  dplyr::starts_with("dAIC"), dplyr::starts_with("dBrier"),
                  waves_block_q_lt_0_05)
write_out(table_s1, OUT_TAB, "table_S1_lobo_by_era.csv")

fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")
ERA_HEAD <- c("Pre-vaccine" = "Pre-vaccine era", "Rollout" = "Vaccine rollout era",
              "Omicron" = "Omicron era")
write_out(table_s1 %>% dplyr::transmute(
    group = sprintf("%s - %s (waves = %d; median n = %s)", outcome_label,
                    ERA_HEAD[era], n_waves, fmt_int(median_n)),
    block_removed = block_label,
    dAIC_penalty_median_IQR = sprintf("%s%s [%s, %s]", ifelse(dAIC_median < 0, "-", "+"),
                                      fmt_int(abs(dAIC_median)), fmt_int(dAIC_q25),
                                      fmt_int(dAIC_q75)),
    dBrier_penalty_median_IQR = sprintf("%+.4f [%.4f, %.4f]", dBrier_median, dBrier_q25,
                                        dBrier_q75),
    waves_block_q_lt_0_05 = sprintf("%d / %d", waves_block_q_lt_0_05, n_waves)),
    OUT_TAB, "table_S1_lobo_by_era_formatted.csv")

lobo_all <- lobo %>% dplyr::group_by(outcome, block) %>% penalties() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(outcome_label = unname(OUTCOME_LABELS[outcome]),
                  block_label = unname(BLOCK_NAMES[block])) %>%
    dplyr::arrange(match(outcome, PRIMARY_OUTCOMES), dplyr::desc(dAIC_median))
write_out(lobo_all, OUT_TAB, "lobo_all_waves.csv")

# ---- 2. Generalized-trust sensitivity: primary vs + generalized trust (Fig S18) ----
rd_cdc <- read_out(OUT_EST, "adjusted_rd_by_wave_trust_cdc.csv")
rd_spec <- function(spec) rd_cdc %>% dplyr::filter(model_spec == spec) %>%
    dplyr::transmute(outcome, wave, era, pp = 100 * estimate, lo = 100 * conf_low,
                     hi = 100 * conf_high)
cmp <- dplyr::inner_join(rd_spec("primary"), rd_spec("sensitivity_general_trust"),
                         by = c("outcome", "wave", "era"), suffix = c("_p", "_g")) %>%
    dplyr::transmute(outcome, outcome_label = unname(OUTCOME_LABELS[outcome]), wave, era,
                     RD_primary_pp = pp_p, RD_primary_lo = lo_p, RD_primary_hi = hi_p,
                     RD_genTrust_pp = pp_g, RD_genTrust_lo = lo_g, RD_genTrust_hi = hi_g,
                     abs_change_pp = pp_g - pp_p,                     # signed change
                     pct_change = 100 * (pp_g - pp_p) / pp_p) %>%
    dplyr::arrange(match(outcome, names(OUTCOME_LABELS)), wave)
write_out(cmp, OUT_TAB, "sensitivity_general_trust_comparison.csv")

rel <- cmp$pct_change[is.finite(cmp$pct_change)]
s18 <- tibble::tibble(
    n_estimates = nrow(cmp), n_outcomes = dplyr::n_distinct(cmp$outcome),
    pearson_r = stats::cor(cmp$RD_primary_pp, cmp$RD_genTrust_pp),
    median_abs_change_pp = stats::median(abs(cmp$abs_change_pp)),
    max_abs_change_pp = max(abs(cmp$abs_change_pp)),
    median_rel_change_pct = stats::median(rel),
    rel_change_q25_pct = q7(rel, .25), rel_change_q75_pct = q7(rel, .75))
write_out(s18, OUT_TAB, "fig_S18_concordance_stats.csv")

# ---- 3. Negative control: trust in banks vs trust in the CDC across waves -------------
rd_primary <- function(focal) read_out(OUT_EST, sprintf("adjusted_rd_by_wave_%s.csv", focal)) %>%
    dplyr::filter(model_spec == "primary", !is.na(estimate)) %>%
    dplyr::transmute(outcome, wave, pp = 100 * estimate, lo = 100 * conf_low,
                     hi = 100 * conf_high, sig_ci = lo > 0 | hi < 0,
                     sig_q = !is.na(p_adj) & p_adj < 0.05)
summ <- function(df) df %>% dplyr::group_by(outcome) %>% dplyr::summarise(
    n_waves = dplyr::n(), median_pp = stats::median(pp), q25_pp = q7(pp, .25),
    q75_pp = q7(pp, .75), min_pp = min(pp), max_pp = max(pp), n_sig_ci = sum(sig_ci),
    n_sig_q = sum(sig_q), n_pos = sum(pp > 0))
negctrl <- dplyr::left_join(summ(rd_primary("trust_banks")), summ(rd_primary("trust_cdc")),
                            by = "outcome", suffix = c("_banks", "_cdc")) %>%
    dplyr::mutate(
        outcome_label = unname(OUTCOME_LABELS[outcome]),
        cdc_mostly_sig = n_sig_ci_cdc >= n_waves_cdc / 2,
        banks_to_cdc_ratio_pct = ifelse(cdc_mostly_sig & median_pp_cdc != 0,
                                        round(100 * median_pp_banks / median_pp_cdc), NA_real_),
        banks_RD_median_IQR_pp = sprintf("%.1f [%.1f, %.1f]", median_pp_banks, q25_pp_banks,
                                         q75_pp_banks),
        banks_RD_range_pp = sprintf("%.1f to %.1f", min_pp_banks, max_pp_banks),
        banks_waves_sig_CI = sprintf("%d / %d", n_sig_ci_banks, n_waves_banks),
        banks_waves_sig_BH = sprintf("%d / %d", n_sig_q_banks, n_waves_banks),
        cdc_RD_median_IQR_pp = sprintf("%.1f [%.1f, %.1f]", median_pp_cdc, q25_pp_cdc,
                                       q75_pp_cdc),
        cdc_waves_sig_CI = sprintf("%d / %d", n_sig_ci_cdc, n_waves_cdc)) %>%
    dplyr::select(outcome, outcome_label, n_waves = n_waves_banks,
                  banks_median_pp = median_pp_banks, cdc_median_pp = median_pp_cdc,
                  banks_to_cdc_ratio_pct, banks_RD_median_IQR_pp, banks_RD_range_pp,
                  banks_waves_sig_CI, banks_waves_sig_BH, cdc_RD_median_IQR_pp,
                  cdc_waves_sig_CI) %>%
    dplyr::arrange(match(outcome, names(OUTCOME_LABELS)))
write_out(negctrl, OUT_TAB, "negative_control_summary.csv")

# ---- 4. Every reported number vs the published value -------------------------------
computed <- list()
put <- function(item, value) computed[[item]] <<- as.numeric(value)[1]

# Table 1
seq_fits <- read_out(OUT_EST, "sequential_models_by_wave.csv") %>% dplyr::filter(!is.na(trust_OR))
for (o in PRIMARY_OUTCOMES) {
    so <- seq_fits[seq_fits$outcome == o, ]
    put(sprintf("T1/%s/median_n", o), round(stats::median(so$n[so$model_id == "M4"])))
    put(sprintf("T1/%s/median_events", o), round(stats::median(so$events[so$model_id == "M4"])))
    for (m in c("M0", "M1", "M2", "M3", "M4")) {
        sm <- so[so$model_id == m, ]
        for (st in list(c("OR", "trust_OR", 1), c("RD", "prob_diff", 100),
                        c("dAIC", "delta_AIC_vs_M0", 1), c("dBrier", "delta_Brier_vs_M0", 1))) {
            x <- as.numeric(st[3]) * sm[[st[2]]]
            put(sprintf("T1/%s/%s/%s", o, m, st[1]), stats::median(x))
            put(sprintf("T1/%s/%s/%s_q25", o, m, st[1]), q7(x, .25))
            put(sprintf("T1/%s/%s/%s_q75", o, m, st[1]), q7(x, .75))
        }
    }
}
put("TXT/table1_mask_logOR_change_M3",
    -stats::median(seq_fits$pct_change_logOR_vs_M0[seq_fits$outcome == "always_wear_mask" &
                                                      seq_fits$model_id == "M3"]))
# Table S1 and LOBO in the Results
for (i in seq_len(nrow(table_s1))) {
    r <- table_s1[i, ]
    key <- sprintf("S1/%s/%s/%s/", r$outcome, r$era, r$block)
    for (st in c("dAIC", "dAIC_q25", "dAIC_q75", "dBrier", "dBrier_q25", "dBrier_q75")) {
        col <- if (st %in% c("dAIC", "dBrier")) paste0(st, "_median") else st
        put(paste0(key, st), r[[col]])
    }
    put(sprintf("S1/%s/%s/n_waves", r$outcome, r$era), r$n_waves)
    put(sprintf("S1/%s/%s/median_n", r$outcome, r$era), r$median_n)
}
for (i in seq_len(nrow(lobo_all))) {
    r <- lobo_all[i, ]
    key <- sprintf("LOBO/%s/%s/", r$outcome, r$block)
    put(paste0(key, "dAIC"), r$dAIC_median); put(paste0(key, "dAIC_q25"), r$dAIC_q25)
    put(paste0(key, "dAIC_q75"), r$dAIC_q75); put(paste0(key, "dBrier"), r$dBrier_median)
}
for (st in names(s18)) put(paste0("S18/", st), s18[[st]])

# Unadjusted differences and significance shares
gaps <- read_out(OUT_TAB, "adherence_gap_summary.csv")
gap <- function(o, p) gaps$mean_gap_pp[gaps$outcome == o & gaps$predictor == p]
put("TXT/gap_mask_trust_cdc", gap("always_wear_mask", "trust_cdc"))
put("TXT/gap_mask_party", gap("always_wear_mask", "party3"))
trust_gaps <- vapply(PRIMARY_OUTCOMES, gap, numeric(1), p = "trust_cdc")
put("TXT/gap_trust_4beh_min", min(trust_gaps)); put("TXT/gap_trust_4beh_max", max(trust_gaps))
demog <- vapply(c("age_cat", "education_cat", "income_cat_5_fct"), gap, numeric(1),
                o = "always_wear_mask")
put("TXT/gap_mask_demog_min", min(demog)); put("TXT/gap_mask_demog_max", max(demog))
gw <- read_out(OUT_EST, "adherence_gap_by_wave.csv") %>%
    dplyr::filter(outcome == "always_wear_mask", predictor %in% c("trust_cdc", "party3")) %>%
    dplyr::select(wave, predictor, gap_pct) %>%
    tidyr::pivot_wider(names_from = predictor, values_from = gap_pct)
put("TXT/waves_trust_gap_gt_party", sum(gw$trust_cdc > gw$party3, na.rm = TRUE))
share <- read_out(OUT_TAB, "significance_share.csv")
sh <- function(o, s, col = "pct_sig_BH") share[[col]][share$outcome == o & share$stratifier == s]
for (o in PRIMARY_OUTCOMES) put(paste0("TXT/sig_trust_", o), sh(o, "trust_cdc"))
put("TXT/sig_trust_visit_store", sh("visit_store", "trust_cdc"))
put("TXT/sig_trust_visit_work", sh("visit_work", "trust_cdc"))
put("TXT/nsig_trust_visit_doctor", sh("visit_doctor", "trust_cdc", "n_sig_BH"))
put("TXT/sig_income_visit_doctor", sh("visit_doctor", "income_cat_5_fct"))
put("TXT/sig_education_visit_doctor", sh("visit_doctor", "education_cat"))
put("TXT/sig_race_visit_doctor", sh("visit_doctor", "race_fct"))

# Adjusted risk differences (pp) at landmark waves
rd_at <- function(item, focal, o, spec, w) {
    d <- read_out(OUT_EST, sprintf("adjusted_rd_by_wave_%s.csv", focal))
    r <- d[d$outcome == o & d$model_spec == spec & d$wave == w, ]
    put(item, 100 * r$estimate); put(paste0(item, "_lo"), 100 * r$conf_low)
    put(paste0(item, "_hi"), 100 * r$conf_high)
}
for (w in LANDMARK_WAVES) {                        # waves 5, 16 and 21
    rd_at(sprintf("TXT/rd_mask_cdc_primary_w%d", w), "trust_cdc", "always_wear_mask", "primary", w)
    rd_at(sprintf("TXT/rd_mask_cdc_nopartisan_w%d", w), "trust_cdc", "always_wear_mask",
          "sensitivity_no_partisan", w)
    rd_at(sprintf("TXT/rd_mask_cdc_gentrust_w%d", w), "trust_cdc", "always_wear_mask",
          "sensitivity_general_trust", w)
    rd_at(sprintf("TXT/rd_doctor_cdc_w%d", w), "trust_cdc", "visit_9", "primary", w)
}
inc <- vapply(LANDMARK_WAVES, function(w) computed[[sprintf("TXT/rd_mask_cdc_nopartisan_w%d", w)]] -
                  computed[[sprintf("TXT/rd_mask_cdc_primary_w%d", w)]], numeric(1))
put("TXT/rd_mask_nopartisan_increase_min", min(inc))
put("TXT/rd_mask_nopartisan_increase_max", max(inc))
rd_at("TXT/rd_mask_doc_w16", "trust_doc", "always_wear_mask", "primary", 16)
rd_at("TXT/rd_mask_state_w16", "trust_state", "always_wear_mask", "primary", 16)
rd_at("TXT/rd_vax_cdc_w18", "trust_cdc", "vaccine_1", "primary", 18)
rd_at("TXT/rd_vax_cdc_w21", "trust_cdc", "vaccine_1", "primary", 21)

# Era-pooled estimates (Spec A)
prd_cdc <- read_out(OUT_EST, "pooled_era_rd_trust_cdc.csv") %>%
    dplyr::filter(model_spec == "pooled_specA_base",
                  outcome %in% c("always_avoid_contact", "always_avoid_crowds", "wash_hands_strict"))
put("TXT/rd_other3_cdc_pooledA_min", 100 * min(prd_cdc$estimate))
put("TXT/rd_other3_cdc_pooledA_max", 100 * max(prd_cdc$estimate))
prd_b <- read_out(OUT_EST, "pooled_era_rd_trust_banks.csv")
por_b <- read_out(OUT_EST, "pooled_era_or_trust_banks.csv")
ERA_KEYS <- c(prevax = "Pre-vaccine", rollout = "Rollout", omicron = "Omicron")
for (e in names(ERA_KEYS)) {
    era <- ERA_KEYS[[e]]
    r <- prd_b[prd_b$outcome == "always_wear_mask" & prd_b$era == era &
                   prd_b$model_spec == "pooled_specA_base", ]
    put(sprintf("TXT/rd_mask_banks_pooledA_%s", e), 100 * r$estimate)
    put(sprintf("TXT/rd_mask_banks_pooledA_%s_lo", e), 100 * r$conf_low)
    put(sprintf("TXT/rd_mask_banks_pooledA_%s_hi", e), 100 * r$conf_high)
    o <- por_b[por_b$outcome == "always_wear_mask" & por_b$era == era &
                   por_b$model_spec == "pooled_specA_base" & por_b$term == "trust_cdcHigh trust", ]
    put(sprintf("TXT/or_mask_cdc_jointbanks_pooledA_%s", e), o$estimate)
    put(sprintf("TXT/or_mask_cdc_jointbanks_pooledA_%s_lo", e), o$conf_low)
    put(sprintf("TXT/or_mask_cdc_jointbanks_pooledA_%s_hi", e), o$conf_high)
}
# Negative control
nv <- negctrl[negctrl$outcome == "vaccine_1", ]
put("TXT/negctrl_vax_banks_median", nv$banks_median_pp)
put("TXT/negctrl_vax_cdc_median", nv$cdc_median_pp)
put("TXT/negctrl_vax_attenuation", 100 - nv$banks_to_cdc_ratio_pct)
att <- 100 - negctrl$banks_to_cdc_ratio_pct[!is.na(negctrl$banks_to_cdc_ratio_pct)]
put("TXT/negctrl_attenuation_min", min(att)); put("TXT/negctrl_attenuation_max", max(att))
put("TXT/negctrl_attenuation_median", stats::median(att))

published <- readr::read_csv(file.path(REPO_DIR, "reference", "published_values.csv"),
                             show_col_types = FALSE)
check <- published %>%
    dplyr::mutate(reproduced = vapply(item, function(i)
                      if (is.null(computed[[i]])) NA_real_ else computed[[i]], numeric(1)),
                  reproduced_rounded = round(reproduced, digits),
                  # a value matches if it rounds to the printed value
                  match = !is.na(reproduced) &
                      abs(reproduced - published) <= 0.5 * 10^(-digits) + 1e-8)
write_out(check, OUT_TAB, "check_against_published.csv")

cat("\n== Reproduced vs published values ==\n")
smry <- check %>% dplyr::group_by(location) %>%
    dplyr::summarise(matched = sum(match), total = dplyr::n()) %>% dplyr::ungroup()
for (i in seq_len(nrow(smry)))
    cat(sprintf("  %-45s %4d / %4d match\n", smry$location[i], smry$matched[i], smry$total[i]))
miss <- check[!check$match, ]
if (nrow(miss) > 0) {
    cat("\n  Values that differ (see README, 'Notes on specific numbers'):\n")
    print(as.data.frame(utils::head(miss[, c("item", "published", "reproduced_rounded")], 40)),
          row.names = FALSE)
}
log_msg("Done: 06_summary_statistics.R")
