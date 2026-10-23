## ============================================================================ #
## 07_figures.R
## Draws every data figure of the paper and SI at its final printed size
## (vector PDF with embedded fonts, 600-dpi PNG and TIFF), so lettering is
## 6-9 pt on the page. Reads the CSV files of scripts 02, 04 and 06; fits nothing.
##
##   Main text (file names follow the published numbering)
##     Fig1_masking, Fig2_handwashing          prevalence by trust and SDH strata
##     Fig3_adjusted_RD_CDC, Fig4_adjusted_RD_banks   adjusted RDs by wave
##     Fig5_store_visits, Fig6_doctor_visits   prevalence by trust and SDH strata
##   SI Appendix
##     FigS01-FigS10   prevalence by trust and SDH strata  <- prevalence_by_stratum.csv
##     FigS11-FigS16   era-pooled adjusted ORs              <- pooled_era_or_<exposure>.csv
##     FigS17          RDs under three specifications       <- adjusted_rd_by_wave_trust_cdc.csv
##     FigS18          primary vs + generalized trust RDs   <- sensitivity_general_trust_comparison.csv
##     FigS19          distribution of trust over time      <- trust_level_shares.csv
##   FigS20 (DAGs) and FigS21 (model schematic) are drawn by
##   python/fig_S20_dag_panel.py and R/08_fig_S21_model_schematic.R.
##
## Setting: MOSAIC_MATCH_TRUST_WAVES (TRUE: SDH panels show the 20 trust waves,
##          as in the final figures; FALSE: all 23 waves, as in the proof, with
##          the time axis extended to the last wave).
## Optional: set RUN_SECTIONS to any of "mosaic", "forest", "rd", "scatter",
##          "trust" before sourcing this script to draw only those figures.
## Original script: pnas_figures/pnas_publication_figures.R (drawing code unchanged)
## ============================================================================ #

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))
use_packages(c("ggplot2", "patchwork", "scales", "forcats", "ragg", "systemfonts"))
source(file.path(REPO_DIR, "R", "figure_theme.R"))
OUT_DIR_FIG <- OUT_FIG
log_msg("Writing figures to ", OUT_DIR_FIG)

# ============================================================================ #
# SECTION 1: SHARED LOOKUPS ----
# ============================================================================ #

# Colours follow unified_palettes.R.  The first (lightest) step of the three
# sequential ramps (age, income, education) was nearly white (#FEE5D9,
# #EFF3FF, #E5F5E0: contrast ~1.1:1 on white) and its series could not be
# seen at print size, so those ramps are shifted one step darker along the
# same ColorBrewer hue.  Set USE_ORIGINAL_SEQUENTIAL <- TRUE to revert.
USE_ORIGINAL_SEQUENTIAL <- FALSE

STRAT <- list(
    trust_cdc = list(
        title = "Trust in CDC",
        levels = c("No trust", "Not much trust", "Some trust", "High trust"),
        labels = c("No trust", "Not much trust", "Some trust", "High trust"),
        colours = c("#B2182B", "#F4A582", "#92C5DE", "#2166AC")),
    trust_banks = list(
        title = "Trust in Banks",
        levels = c("No trust", "Not much trust", "Some trust", "High trust"),
        labels = c("No trust", "Not much trust", "Some trust", "High trust"),
        colours = c("#B2182B", "#F4A582", "#92C5DE", "#2166AC")),
    age_cat = list(
        title = "Age",
        levels = c("18 to 34", "35 to 54", "55 to 64", "65 and over"),
        labels = c("18\u201334", "35\u201354", "55\u201364", "65+"),
        colours = if (USE_ORIGINAL_SEQUENTIAL)
            c("#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D") else
            c("#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")),
    gender = list(
        title = "Gender",
        levels = c("Female", "Male"),
        labels = c("Female", "Male"),
        colours = c("#8B8946", "#7B6D8E")),
    income_cat_5_fct = list(
        title = "Income",
        levels = c("Under $25,000", "$25,000 - $49,999", "$50,000 - $74,999",
                   "$75,000 - $99,999", "$100,000 and over"),
        labels = c("<$25k", "$25\u201349k", "$50\u201374k", "$75\u201399k", "\u2265$100k"),
        colours = if (USE_ORIGINAL_SEQUENTIAL)
            c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C") else
            c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594")),
    education_cat = list(
        title = "Education",
        levels = c("Some High School or Less", "High School Graduate",
                   "Some College", "College Degree", "Graduate Degree"),
        labels = c("Some HS or less", "HS graduate", "Some college",
                   "College degree", "Graduate degree"),
        colours = if (USE_ORIGINAL_SEQUENTIAL)
            c("#E5F5E0", "#A1D99B", "#74C476", "#31A354", "#006D2C") else
            c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32")),
    party3 = list(
        title = "Political Affiliation",
        levels = c("Democrat", "Independent/Other", "Republican"),
        labels = c("Democrat", "Independent/Other", "Republican"),
        colours = c("#2166AC", "#999999", "#B2182B")),
    race_fct = list(
        title = "Race/Ethnicity",
        levels = c("White", "African American", "Asian/Pacific Islander",
                   "Hispanic", "Other"),
        labels = c("White", "African American", "Asian/Pacific Isl.",
                   "Hispanic", "Other"),
        colours = c("#4DAF4A", "#984EA3", "#377EB8", "#FF7F00", "#A65628")),
    employ_fct = list(
        title = "Employment Status",
        levels = c("Full-time", "Part-time", "Unemployed", "Retired", "Other"),
        labels = c("Full-time", "Part-time", "Unemployed", "Retired", "Other"),
        colours = c("#8C2D04", "#E6550D", "#FEC44F", "#66C2A4", "#238B8F")),
    urban_type = list(
        title = "Urbanicity",
        levels = c("Rural", "Suburban", "Urban"),
        labels = c("Rural", "Suburban", "Urban"),
        colours = c("#D94701", "#6BAED6", "#2171B5"))
)

OUTCOME_Y_TITLE <- c(
    always_wear_mask = "Always wearing a mask",
    wash_hands_strict = "Always washing hands",
    always_avoid_contact = "Always avoiding contact",
    always_avoid_crowds = "Always avoiding crowds",
    vaccine_1 = "Vaccinated (1+ dose)",
    visit_doctor = "Visited a doctor or hospital (past 24 h)",
    visit_store = "Visited a store (past 24 h)")

MOSAIC_CORE <- c(B = "age_cat", C = "gender", D = "income_cat_5_fct", E = "education_cat")
MOSAIC_SECONDARY <- c(B = "party3", C = "race_fct", D = "employ_fct", E = "urban_type")

# ============================================================================ #
# SECTION 2: MOSAIC TRAJECTORY FIGURES (Figs 1-2, 5-6, S1-S10) ----
# ============================================================================ #

mos <- read_out(OUT_EST, "prevalence_by_stratum.csv") %>%
    mutate(wave_date = as.Date(wave_date))

MOSAIC_TRUST_WAVES <- sort(unique(mos$wave[mos$stratifier == "trust_cdc"]))
# Right end of the time axis: Sep 2022 (last trust wave: Aug 2022), or the last
# wave when the SDH panels show all 23 waves.
MOSAIC_X_END <- if (MOSAIC_MATCH_TRUST_WAVES) as.Date("2022-09-30") else
    max(as.Date("2022-09-30"), max(mos$wave_date, na.rm = TRUE) + 31)

#' One trajectory panel (points + 95% CI bars + lines) on a fixed 0-100% axis.
#' big = TRUE: the focal trust panel (milestone labels in a band above 100%).
traj_panel <- function(outcome, stratifier, x_lim, panel_w_pt, big = FALSE,
                       y_title = NULL) {
    sp <- STRAT[[stratifier]]
    df <- mos %>% filter(.data$outcome == !!outcome, .data$stratifier == !!stratifier)
    if (MOSAIC_MATCH_TRUST_WAVES) df <- df %>% filter(wave %in% MOSAIC_TRUST_WAVES)
    if (nrow(df) == 0) stop("No estimates for ", outcome, " x ", stratifier)
    df <- df %>% mutate(group = factor(group, levels = sp$levels, labels = sp$labels))
    if (anyNA(df$group)) stop("Unmapped group label in ", stratifier)
    pal <- setNames(sp$colours, sp$labels)

    n_rows <- legend_rows_needed(sp$labels, avail_pt = panel_w_pt)
    ms_layout <- if (panel_w_pt < 200) "narrow" else "wide"
    y_top <- if (big) milestone_band(ms_layout)[2] + 1 else 100

    lw <- if (big) 0.42 else 0.32
    pt <- if (big) 1.05 else 0.62
    # 3-month ticks on the focal panel; 6-month ticks when it spans all 23 waves
    brk_step <- if (big && MOSAIC_MATCH_TRUST_WAVES) "3 months" else "6 months"
    x_breaks <- seq(as.Date(format(x_lim[1], "%Y-01-01")), x_lim[2], by = brk_step)
    x_breaks <- x_breaks[x_breaks >= x_lim[1] & x_breaks <= x_lim[2]]

    p <- ggplot(df, aes(wave_date, pct, colour = group, group = group)) +
        geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2)
    p <- add_milestones_pnas(p, x_range = x_lim, labels = big, layout = ms_layout,
                             line_width = if (big) 0.28 else 0.22,
                             line_alpha = if (big) 0.7 else 0.5)
    p <- p +
        geom_linerange(aes(ymin = ci_lo, ymax = ci_hi), linewidth = lw * 0.7,
                       alpha = 0.6, show.legend = FALSE) +
        geom_line(linewidth = lw, alpha = 0.9) +
        geom_point(size = pt, stroke = 0) +
        scale_colour_manual(values = pal, breaks = sp$labels, name = NULL) +
        scale_y_continuous(breaks = seq(0, 100, 25), labels = function(v) paste0(v, "%"),
                           expand = expansion(mult = c(0.02, if (big) 0 else 0.03))) +
        scale_x_date(breaks = x_breaks, date_labels = "%b\n%Y",
                     expand = expansion(0, 0)) +
        coord_cartesian(xlim = x_lim, ylim = c(0, y_top), clip = "on") +
        labs(title = sp$title, x = NULL, y = y_title) +
        guides(colour = guide_legend(nrow = n_rows, byrow = TRUE,
                                     override.aes = list(linewidth = 0.5, size = 1.1))) +
        theme_pnas() +
        theme(legend.position = "bottom",
              legend.justification = "left",
              legend.box.spacing = unit(2, "pt"),
              panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.18),
              plot.title = element_text(size = if (big) PT_TITLE else PT_TITLE - 0.5))
    if (!big) p <- p + theme(plot.margin = margin(3, 5, 2, 3))
    p
}

#' Assemble one mosaic: focal trust panel (left half) + 2x2 SDH panels.
build_mosaic <- function(outcome, focal, sdh, width, height) {
    x_lim <- c(if (outcome == "vaccine_1") as.Date("2020-12-01") else as.Date("2020-04-01"),
               MOSAIC_X_END)
    # layout "AAABBCC/AAADDEE": focal panel 3/7 of the width, SDH panels 2/7
    # legends sit under the panel area (patchwork), so subtract the y-axis
    # labels/title and plot margins from each column's width
    w_big_pt <- width * 72 * 3 / 7 - 38
    w_small_pt <- width * 72 * 2 / 7 - 30
    pA <- traj_panel(outcome, focal, x_lim, w_big_pt, big = TRUE,
                     y_title = OUTCOME_Y_TITLE[[outcome]])
    small <- lapply(sdh, function(s) traj_panel(outcome, s, x_lim, w_small_pt))
    wrap_plots(A = pA, B = small$B, C = small$C, D = small$D, E = small$E,
               design = "AAABBCC\nAAADDEE")
}

MOSAIC_FIGS <- list(
    # SI: same height as the image it replaces in the SI document
    list(id = "FigS01_masking_secondary_SDH", outcome = "always_wear_mask", focal = "trust_cdc", sdh = MOSAIC_SECONDARY, w = SI_TEXT_W, h = 4.3),
    list(id = "FigS02_handwashing_secondary_SDH", outcome = "wash_hands_strict", focal = "trust_cdc", sdh = MOSAIC_SECONDARY, w = SI_TEXT_W, h = 4.5),
    list(id = "FigS03_masking_banks_core_SDH", outcome = "always_wear_mask", focal = "trust_banks", sdh = MOSAIC_CORE, w = SI_TEXT_W, h = 4.5),
    list(id = "FigS04_masking_banks_secondary_SDH", outcome = "always_wear_mask", focal = "trust_banks", sdh = MOSAIC_SECONDARY, w = SI_TEXT_W, h = 4.5),
    list(id = "FigS05_avoid_contact_core_SDH", outcome = "always_avoid_contact", focal = "trust_cdc", sdh = MOSAIC_CORE, w = SI_TEXT_W, h = 4.5),
    list(id = "FigS06_avoid_contact_secondary_SDH", outcome = "always_avoid_contact", focal = "trust_cdc", sdh = MOSAIC_SECONDARY, w = SI_TEXT_W, h = 4.4),
    list(id = "FigS07_avoid_crowds_core_SDH", outcome = "always_avoid_crowds", focal = "trust_cdc", sdh = MOSAIC_CORE, w = SI_TEXT_W, h = 4.5),
    list(id = "FigS08_avoid_crowds_secondary_SDH", outcome = "always_avoid_crowds", focal = "trust_cdc", sdh = MOSAIC_SECONDARY, w = SI_TEXT_W, h = 4.4),
    list(id = "FigS09_vaccination_core_SDH", outcome = "vaccine_1", focal = "trust_cdc", sdh = MOSAIC_CORE, w = SI_TEXT_W, h = 4.5),
    list(id = "FigS10_vaccination_secondary_SDH", outcome = "vaccine_1", focal = "trust_cdc", sdh = MOSAIC_SECONDARY, w = SI_TEXT_W, h = 4.4),
    # Main text: double column (17.8 cm)
    list(id = "Fig1_masking", outcome = "always_wear_mask", focal = "trust_cdc", sdh = MOSAIC_CORE, w = PNAS_W_DOUBLE, h = 4.5),
    list(id = "Fig2_handwashing", outcome = "wash_hands_strict", focal = "trust_cdc", sdh = MOSAIC_CORE, w = PNAS_W_DOUBLE, h = 4.5),
    list(id = "Fig5_store_visits", outcome = "visit_store", focal = "trust_cdc", sdh = MOSAIC_CORE, w = PNAS_W_DOUBLE, h = 4.5),
    list(id = "Fig6_doctor_visits", outcome = "visit_doctor", focal = "trust_cdc", sdh = MOSAIC_CORE, w = PNAS_W_DOUBLE, h = 4.5)
)

RUN_SECTIONS <- if (exists("RUN_SECTIONS")) RUN_SECTIONS else c("mosaic", "forest", "rd", "scatter", "trust")

if ("mosaic" %in% RUN_SECTIONS) {
    cat("=== Mosaic trajectory figures ===\n")
    for (f in MOSAIC_FIGS) {
        fig <- build_mosaic(f$outcome, f$focal, f$sdh, f$w, f$h)
        save_pnas(fig, file.path(OUT_DIR_FIG, f$id), f$w, f$h)
        cat(sprintf("  %-8s %-22s %-11s %.2f x %.2f in\n", f$id, f$outcome, f$focal, f$w, f$h))
    }
}

# ============================================================================ #
# SECTION 3: POOLED-ERA ADJUSTED-OR FOREST PLOTS (S11-S16) ----
# ============================================================================ #
# Same estimates, row ordering, palette and Spec A/B encoding as the original
# 13_production_plots.R; redrawn at the full SI page width.  The SDH-group
# colour legend is omitted because every group is named by its row strip.

FOCAL_LABELS <- c(trust_cdc = "Trust in CDC", trust_banks = "Trust in Banks")
FOREST_OUTCOME_LABELS <- c(always_wear_mask = "Mask wearing",
                           always_avoid_contact = "Avoiding contact",
                           always_avoid_crowds = "Avoiding crowds",
                           wash_hands_strict = "Handwashing",
                           vaccine_1 = "Vaccination (1+ dose)",
                           visit_9 = "Doctor/hospital visit")
FOREST_SPLITS <- list(
    SplitA_avoidance = c("always_avoid_contact", "always_avoid_crowds"),
    SplitB_hygiene = c("always_wear_mask", "wash_hands_strict"),
    SplitC_engagement = c("vaccine_1", "visit_9"))
FOREST_FIG_IDS <- list(
    trust_cdc = c(SplitA_avoidance = "FigS11_pooled_OR_CDC_distancing",
                  SplitB_hygiene = "FigS12_pooled_OR_CDC_masking_handwashing",
                  SplitC_engagement = "FigS13_pooled_OR_CDC_vaccination_doctor"),
    trust_banks = c(SplitA_avoidance = "FigS14_pooled_OR_banks_distancing",
                    SplitB_hygiene = "FigS15_pooled_OR_banks_masking_handwashing",
                    SplitC_engagement = "FigS16_pooled_OR_banks_vaccination_doctor"))
FOREST_PALETTE_BASE <- c(
    "Education" = "forestgreen", "Income" = "royalblue", "Race/Ethnicity" = "cyan3",
    "Party" = "purple", "Ideology" = "plum4", "Age" = "darkred", "Region" = "sienna",
    "Gender" = "#7B3F99", "Urbanicity" = "#666666", "Employment" = "#1B9E77",
    "Other" = "gray40")
SDH_GROUP_ORDER_BASE <- c("Race/Ethnicity", "Party", "Region", "Age", "Urbanicity",
                          "Ideology", "Gender", "Income", "Employment", "Education")
ADJ_GROUP_RAW <- "Primary Exposure (Adjustment)"
ADJ_GROUP_LABEL <- "Trust in CDC"
ADJ_GROUP_COLOR <- "#4D4D4D"
SPEC_LABELS <- c(pooled_specA_base = "Specification A: pooled by era",
                 pooled_specB_waveFE = "Specification B: pooled by era + wave fixed effects")
FOREST_W <- SI_TEXT_W
FOREST_H <- 5.9

#' Hierarchical row sort (verbatim logic from 12_/13_production_plots.R).
sort_terms_hierarchically <- function(df, group_order_pref) {
    if (nrow(df) == 0) return(df)
    df <- df %>% mutate(importance_score = abs(log(pmax(estimate, 1e-3))))
    all_groups <- unique(as.character(df$sdh_group))
    group_order <- intersect(c(group_order_pref, sort(setdiff(all_groups, group_order_pref))),
                             all_groups)
    level_summary <- df %>%
        group_by(sdh_group, level_label) %>%
        summarise(level_max = max(importance_score, na.rm = TRUE),
                  level_med = median(importance_score, na.rm = TRUE), .groups = "drop") %>%
        mutate(sdh_group = factor(sdh_group, levels = group_order)) %>%
        arrange(sdh_group, desc(level_max), desc(level_med), level_label)
    level_order <- unique(as.character(level_summary$level_label))
    df %>% mutate(sdh_group = factor(sdh_group, levels = group_order),
                  level_label = forcats::fct_rev(factor(as.character(level_label),
                                                        levels = level_order)))
}

build_forest <- function(pooled, outcomes, palette, group_order) {
    fp <- pooled %>%
        filter(outcome %in% outcomes, !is.na(estimate), sdh_group != "Other") %>%
        mutate(estimate_plot = pmax(pmin(estimate, 20), 0.05),
               low_plot = pmax(pmin(conf_low, 20), 0.05),
               high_plot = pmax(pmin(conf_high, 20), 0.05),
               era_label = factor(era, levels = c("Pre-vaccine", "Rollout", "Omicron")),
               outcome_label = factor(FOREST_OUTCOME_LABELS[outcome],
                                      levels = FOREST_OUTCOME_LABELS[outcomes]),
               spec_label = factor(SPEC_LABELS[model_spec], levels = SPEC_LABELS))
    fp <- sort_terms_hierarchically(fp, group_order)
    ggplot(fp, aes(x = estimate_plot, y = level_label, colour = sdh_group,
                   shape = spec_label, group = spec_label)) +
        geom_vline(xintercept = 1, linetype = "22", colour = "grey55", linewidth = 0.3) +
        geom_pointrange(aes(xmin = low_plot, xmax = high_plot),
                        position = position_dodge(width = 0.7),
                        linewidth = 0.35, size = 0.3, fatten = 2.4, stroke = 0.3) +
        facet_grid(sdh_group ~ outcome_label + era_label, scales = "free_y",
                   space = "free_y", switch = "y") +
        scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8, 16),
                      labels = c("0.25", "0.5", "1", "2", "4", "8", "16")) +
        scale_colour_manual(values = palette, guide = "none") +
        scale_shape_manual(values = setNames(c(16, 17), SPEC_LABELS), name = NULL) +
        guides(shape = guide_legend(override.aes = list(colour = "grey25", linetype = 0,
                                                        size = 0.45))) +
        labs(x = "Adjusted odds ratio (log scale)", y = NULL) +
        theme_pnas() +
        theme(legend.position = "top",
              legend.justification = "left",
              legend.box.spacing = unit(1, "pt"),
              legend.margin = margin(0, 0, 1, 0),
              legend.key.width = unit(8, "pt"),
              strip.placement = "outside",
              strip.text.x = element_text(size = PT_BASE, face = "bold", lineheight = 0.9),
              strip.text.y.left = element_text(size = PT_BASE, face = "bold", angle = 0,
                                               hjust = 1),
              axis.text.y = element_text(size = PT_SMALL),
              panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.18),
              panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.18),
              panel.border = element_rect(colour = "grey80", linetype = "dashed",
                                          linewidth = 0.25, fill = NA),
              panel.spacing.x = unit(3, "pt"),
              panel.spacing.y = unit(2.5, "pt"))
}

if ("forest" %in% RUN_SECTIONS) {
    cat("=== Pooled-era forest plots ===\n")
    for (focal in names(FOREST_FIG_IDS)) {
        focal_label <- FOCAL_LABELS[[focal]]
        pooled <- read_out(OUT_EST, sprintf("pooled_era_or_%s.csv", focal)) %>%
            mutate(sdh_group = ifelse(sdh_group == "Focal Trust Variable", focal_label,
                                      sdh_group))
        if (ADJ_GROUP_RAW %in% pooled$sdh_group) {
            pooled <- pooled %>% mutate(sdh_group = ifelse(sdh_group == ADJ_GROUP_RAW,
                                                           ADJ_GROUP_LABEL, sdh_group))
            palette <- c(setNames("goldenrod3", focal_label),
                         setNames(ADJ_GROUP_COLOR, ADJ_GROUP_LABEL), FOREST_PALETTE_BASE)
            group_order <- c(focal_label, ADJ_GROUP_LABEL, SDH_GROUP_ORDER_BASE)
        } else {
            palette <- c(setNames("goldenrod3", focal_label), FOREST_PALETTE_BASE)
            group_order <- c(focal_label, SDH_GROUP_ORDER_BASE)
        }
        for (split in names(FOREST_SPLITS)) {
            fid <- FOREST_FIG_IDS[[focal]][[split]]
            fig <- build_forest(pooled, FOREST_SPLITS[[split]], palette, group_order)
            save_pnas(fig, file.path(OUT_DIR_FIG, fid), FOREST_W, FOREST_H)
            cat(sprintf("  %-8s %-12s %-18s %.2f x %.2f in\n", fid, focal, split,
                        FOREST_W, FOREST_H))
        }
    }
}

# ============================================================================ #
# SECTION 4: ADJUSTED RISK-DIFFERENCE TRAJECTORIES (Figs 3-4, S17) ----
# ============================================================================ #

RD_OUTCOMES <- c("always_wear_mask", "always_avoid_contact", "always_avoid_crowds",
                 "wash_hands_strict", "vaccine_1", "visit_9")
RD_SPEC_LABELS <- c(primary = "Primary model (full covariate set)",
                    sensitivity_no_partisan = "Party and ideology removed",
                    sensitivity_general_trust = "Primary + generalized trust")
RD_SPEC_COLOURS <- c(primary = "#2C7FB8", sensitivity_no_partisan = "#D95F02",
                     sensitivity_general_trust = "#1B9E77")
RD_SPEC_LTY <- c(primary = "solid", sensitivity_no_partisan = "42",
                 sensitivity_general_trust = "12")

# Wave -> field date (median survey date per wave, as in the mosaic figures)
WAVE_DATES <- read_out(OUT_EST, "trust_level_shares.csv") %>%
    distinct(wave, wave_date) %>% mutate(wave_date = as.Date(wave_date))

read_rd <- function(focal, specs) {
    read_out(OUT_EST, sprintf("adjusted_rd_by_wave_%s.csv", focal)) %>%
        filter(model_spec %in% specs, grepl("High trust", contrast),
               grepl("No trust", contrast)) %>%
        left_join(WAVE_DATES, by = "wave") %>%
        mutate(est = estimate * 100, lo = conf_low * 100, hi = conf_high * 100,
               model_spec = factor(model_spec, levels = specs))
}

rd_panel <- function(rd, outcome, specs, x_breaks_by = "6 months", pt_size = 0.55,
                     lw = 0.4) {
    d <- rd %>% filter(.data$outcome == !!outcome)
    x_lim <- if (outcome == "vaccine_1") as.Date(c("2020-12-01", "2022-09-30")) else
        as.Date(c("2020-04-01", "2022-09-30"))
    x_breaks <- seq(as.Date(format(x_lim[1], "%Y-01-01")), x_lim[2], by = x_breaks_by)
    x_breaks <- x_breaks[x_breaks >= x_lim[1]]
    ggplot(d, aes(wave_date, est, colour = model_spec, fill = model_spec,
                  linetype = model_spec, group = model_spec)) +
        geom_hline(yintercept = 0, linetype = "22", colour = "grey50", linewidth = 0.3) +
        geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.14, colour = NA,
                    show.legend = FALSE) +
        geom_line(linewidth = lw) +
        geom_point(size = pt_size, show.legend = FALSE) +
        scale_colour_manual(values = RD_SPEC_COLOURS[specs], labels = RD_SPEC_LABELS[specs],
                            name = NULL, drop = FALSE) +
        scale_fill_manual(values = RD_SPEC_COLOURS[specs], guide = "none", drop = FALSE) +
        scale_linetype_manual(values = RD_SPEC_LTY[specs], labels = RD_SPEC_LABELS[specs],
                              name = NULL, drop = FALSE) +
        scale_x_date(breaks = x_breaks, date_labels = "%b\n%Y", expand = expansion(0, 0)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 4),
                           expand = expansion(mult = c(0.06, 0.08))) +
        coord_cartesian(xlim = x_lim) +
        labs(title = FOREST_OUTCOME_LABELS[[outcome]], x = NULL, y = NULL) +
        theme_pnas() +
        theme(legend.position = "bottom",
              legend.key.width = unit(16, "pt"),
              plot.title = element_text(size = PT_BASE + 0.5),
              panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.18))
}

build_rd_figure <- function(focal, specs, ncol, y_title, pt_size, lw, legend_rows = 1) {
    rd <- read_rd(focal, specs)
    panels <- lapply(RD_OUTCOMES, function(o) rd_panel(rd, o, specs, pt_size = pt_size, lw = lw))
    panels <- lapply(panels, function(p) p + guides(colour = guide_legend(nrow = legend_rows),
                                                    linetype = guide_legend(nrow = legend_rows)))
    wrap_plots(panels, ncol = ncol) +
        plot_layout(guides = "collect") +
        plot_annotation(theme = theme(plot.margin = margin(1, 1, 1, 1))) &
        theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0)) &
        labs(y = NULL)
}

#' Add a shared y-axis title to the left of an assembled patchwork.
with_y_title <- function(p, title, size = PT_BASE) {
    lab <- ggplot() +
        annotate("text", x = 0, y = 0, label = title, angle = 90, size = pt2mm(size),
                 family = PNAS_FONT, colour = "grey10") +
        theme_void() + coord_cartesian(clip = "off")
    wrap_plots(lab, p, widths = c(0.035, 1))
}

if ("rd" %in% RUN_SECTIONS) {
    cat("=== Risk-difference trajectories ===\n")
    ytl <- "Adjusted risk difference, high trust \u2212 no trust (percentage points)"
    # S17: three specifications, trust in the CDC, SI page width
    f17 <- build_rd_figure("trust_cdc", names(RD_SPEC_LABELS), ncol = 3,
                           pt_size = 0.55, lw = 0.45)
    f17 <- with_y_title(f17, ytl)
    save_pnas(f17, file.path(OUT_DIR_FIG, "FigS17_adjusted_RD_three_specifications"), SI_TEXT_W, 6.0)
    cat(sprintf("  %-8s %.2f x %.2f in\n", "FigS17", SI_TEXT_W, 6.0))
    # Figs 3-4: primary vs party/ideology removed, single column
    for (ff in list(list(id = "Fig3_adjusted_RD_CDC", focal = "trust_cdc"),
                    list(id = "Fig4_adjusted_RD_banks", focal = "trust_banks"))) {
        fig <- build_rd_figure(ff$focal, c("primary", "sensitivity_no_partisan"), ncol = 2,
                               pt_size = 0.45, lw = 0.38, legend_rows = 2)
        fig <- with_y_title(fig, "Adjusted risk difference, high \u2212 no trust (pp)",
                            size = PT_BASE)
        save_pnas(fig, file.path(OUT_DIR_FIG, ff$id), PNAS_W_SINGLE, 5.0)
        cat(sprintf("  %-8s %.2f x %.2f in\n", ff$id, PNAS_W_SINGLE, 5.0))
    }
}

# ============================================================================ #
# SECTION 5: PRIMARY vs GENERALIZED-TRUST CONCORDANCE (S18) ----
# ============================================================================ #

SCATTER_COLOURS <- c("Mask wearing" = "#1B9E77", "Avoiding contact" = "#D95F02",
                     "Avoiding crowds" = "#7570B3", "Handwashing" = "#E7298A",
                     "Vaccination (1+ dose)" = "#66A61E",
                     "Doctor/Hospital Visit" = "#E6AB02")
SCATTER_LABELS <- c(always_wear_mask = "Mask wearing", always_avoid_contact = "Avoiding contact",
                    always_avoid_crowds = "Avoiding crowds", wash_hands_strict = "Handwashing",
                    vaccine_1 = "Vaccination (1+ dose)", visit_9 = "Doctor/Hospital Visit")
SCATTER_SHAPES <- c("Mask wearing" = 16, "Avoiding contact" = 17, "Avoiding crowds" = 15,
                    "Handwashing" = 18, "Vaccination (1+ dose)" = 1,
                    "Doctor/Hospital Visit" = 2)

if ("scatter" %in% RUN_SECTIONS) {
    cat("=== Concordance scatter ===\n")
    sc <- read_out(OUT_TAB, "sensitivity_general_trust_comparison.csv") %>%
        mutate(outcome_label = factor(unname(SCATTER_LABELS[outcome]),
                                      levels = names(SCATTER_COLOURS)))
    lims <- range(c(sc$RD_primary_pp, sc$RD_genTrust_pp)) + c(-2, 2)
    f18 <- ggplot(sc, aes(RD_primary_pp, RD_genTrust_pp, colour = outcome_label,
                          shape = outcome_label)) +
        geom_abline(slope = 1, intercept = 0, linetype = "22", colour = "grey45",
                    linewidth = 0.35) +
        geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.25) +
        geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.25) +
        geom_point(size = 1.5, stroke = 0.45, alpha = 0.9) +
        scale_colour_manual(values = SCATTER_COLOURS, name = NULL,
                            labels = c("Mask wearing", "Avoiding contact", "Avoiding crowds",
                                       "Handwashing", "Vaccination (1+ dose)",
                                       "Doctor/hospital visit")) +
        scale_shape_manual(values = SCATTER_SHAPES, name = NULL,
                           labels = c("Mask wearing", "Avoiding contact", "Avoiding crowds",
                                      "Handwashing", "Vaccination (1+ dose)",
                                      "Doctor/hospital visit")) +
        coord_equal(xlim = lims, ylim = lims, expand = FALSE) +
        scale_x_continuous(breaks = seq(0, 50, 10)) +
        scale_y_continuous(breaks = seq(0, 50, 10)) +
        labs(x = "Risk difference, primary model (percentage points)",
             y = "Risk difference, primary model + generalized trust (percentage points)") +
        guides(colour = guide_legend(nrow = 2, byrow = TRUE),
               shape = guide_legend(nrow = 2, byrow = TRUE)) +
        theme_pnas(base_size = 8) +
        theme(legend.position = "bottom",
              axis.text = element_text(size = 7.5),
              legend.text = element_text(size = 7.5),
              panel.grid.major = element_line(colour = "grey92", linewidth = 0.25))
    save_pnas(f18, file.path(OUT_DIR_FIG, "FigS18_general_trust_concordance"), 5.6, 5.9)
    cat(sprintf("  %-8s %.2f x %.2f in\n", "FigS18", 5.6, 5.9))
}

# ============================================================================ #
# SECTION 6: INSTITUTIONAL-TRUST TRAJECTORIES (S19) ----
# ============================================================================ #

if ("trust" %in% RUN_SECTIONS) {
    cat("=== Trust trajectories ===\n")
    tt <- read_out(OUT_EST, "trust_level_shares.csv") %>%
        mutate(wave_date = as.Date(wave_date),
               trust_level = factor(trust_level, levels = STRAT$trust_cdc$levels))
    titles <- c(trust_cdc = "Trust in CDC", trust_doc = "Trust in Doctors & Hospitals",
                trust_state = "Trust in State Government", trust_banks = "Trust in Banks")
    x_lim <- as.Date(c("2020-04-01", "2022-09-30"))
    x_breaks <- seq(as.Date("2020-07-01"), x_lim[2], by = "3 months")
    tpal <- setNames(STRAT$trust_cdc$colours, STRAT$trust_cdc$levels)
    panels <- lapply(names(titles), function(v) {
        d <- tt %>% filter(trust_var == v)
        p <- ggplot(d, aes(wave_date, weighted_prevalence_pct, colour = trust_level,
                           fill = trust_level, group = trust_level))
        p <- add_milestones_pnas(p, x_range = x_lim, labels = TRUE, layout = "wide",
                                 line_width = 0.28, line_alpha = 0.7)
        p + geom_ribbon(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), alpha = 0.18,
                        colour = NA, show.legend = FALSE) +
            geom_line(linewidth = 0.45) +
            geom_point(size = 0.8, stroke = 0) +
            scale_colour_manual(values = tpal, name = "Trust level") +
            scale_fill_manual(values = tpal, guide = "none") +
            scale_y_continuous(breaks = seq(0, 100, 25), labels = function(v) paste0(v, "%"),
                               expand = expansion(mult = c(0.02, 0))) +
            scale_x_date(breaks = x_breaks, date_labels = "%b\n%Y", expand = expansion(0, 0)) +
            coord_cartesian(xlim = x_lim, ylim = c(0, milestone_band("wide")[2] + 1)) +
            labs(title = titles[[v]], x = NULL, y = "Weighted prevalence") +
            theme_pnas() +
            theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.18))
    })
    f19 <- wrap_plots(panels, ncol = 2) + plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
    save_pnas(f19, file.path(OUT_DIR_FIG, "FigS19_trust_over_time"), SI_TEXT_W, 6.0)
    cat(sprintf("  %-8s %.2f x %.2f in\n", "FigS19", SI_TEXT_W, 6.0))
}

log_msg("Done: 07_figures.R (figures in ", OUT_DIR_FIG, ")")
