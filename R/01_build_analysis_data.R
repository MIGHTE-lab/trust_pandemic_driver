## ============================================================================ #
## 01_build_analysis_data.R
## Builds the slim analysis file from the raw CHIP50 survey waves.
##
## Input : CSP_W<wave>_weighted.RDS for waves 1-27 (minus 4, 6, 8, 15), in the
##         folder given by the environment variable C50_WEIGHTED_RDS_DIR.
## Output: data/chip50_trust_analysis.rds  one row per respondent, only the
##                                         variables the paper uses
##         data/codebook.csv              variable descriptions and coding
##         data/sample_sizes_by_wave.csv  respondents per wave
##
## Every later script reads only the analysis file. The recodes below repeat
## those of the original pipeline (01_data_ingest_fix_UPDATED.R and
## 02_Var_recode_UPDATED.R), restricted to these variables.
## ============================================================================ #

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))

if (is.na(RAW_DATA_DIR))
    stop("The environment variable C50_WEIGHTED_RDS_DIR, which tells R where the ",
         "raw waves (CSP_W<wave>_weighted.RDS) are, is not set in this R session.\n",
         "  For this session, run first:\n",
         "    Sys.setenv(C50_WEIGHTED_RDS_DIR = \"<folder>\")\n",
         "  To set it permanently, add the line C50_WEIGHTED_RDS_DIR=\"<folder>\" to\n    ",
         normalizePath("~/.Renviron", winslash = "/", mustWork = FALSE),
         "\n  and restart R. Write the folder with forward slashes. See data/README.md.",
         call. = FALSE)
if (!dir.exists(RAW_DATA_DIR))
    stop("C50_WEIGHTED_RDS_DIR is set to\n    ", RAW_DATA_DIR,
         "\nbut R cannot find that folder. Check the path (forward slashes, no typos).",
         call. = FALSE)

# ---- 1. Raw variables -----------------------------------------------------------
RAW_NUMERIC <- c(
    "weight_state",                               # post-stratification weight
    "age", "income_cat_5", "ideology", "ruca_usda",
    "cov_beh_1", "cov_beh_2", "cov_beh_3", "cov_beh_5",  # behaviors, 1-4
    "visit_1", "visit_3", "visit_9",              # 24-h activities (work, store, doctor)
    "vaccine_1",                                  # vaccinated (waves 14+)
    "cov_trust_cdc", "cov_trust_doctors", "cov_trust_state", "cov_trust_banks",
    "trust")                                      # generalized trust (sensitivity)
RAW_TEXT <- c("gender", "race", "education_cat", "employ", "party3",
              "urban_type", "state_code")
MISSING_CODES <- c(-99, -98, -97, -88, -77, 999, 998)

read_wave <- function(w) {
    f <- file.path(RAW_DATA_DIR, sprintf("CSP_W%d_weighted.RDS", w))
    if (!file.exists(f)) stop("Missing raw file: ", f, call. = FALSE)
    raw <- readRDS(f)
    miss <- setdiff(c(RAW_NUMERIC, RAW_TEXT, "StartDate"), names(raw))
    # Items not fielded in a wave (e.g. vaccine_1 before wave 14) are missing.
    for (v in miss) raw[[v]] <- NA
    out <- tibble::tibble(wave = as.integer(w),
                          survey_date = suppressWarnings(as.Date(raw$StartDate)))
    for (v in RAW_NUMERIC) {
        x <- as.numeric(haven::zap_labels(raw[[v]]))
        x[x %in% MISSING_CODES] <- NA                # survey missing-value codes
        out[[v]] <- x
    }
    for (v in RAW_TEXT) out[[v]] <- haven::zap_labels(raw[[v]])   # keeps factor levels
    log_msg("  wave ", w, ": ", format(nrow(out), big.mark = ","), " respondents")
    out
}

log_msg("Reading ", length(ANALYSIS_WAVES), " waves from ", RAW_DATA_DIR)
raw <- dplyr::bind_rows(lapply(ANALYSIS_WAVES, read_wave))

# ---- 2. Recodes -------------------------------------------------------------------
trust_factor <- function(x) {                    # 1 = not at all ... 4 = a lot
    factor(dplyr::case_when(x == 4 ~ "High trust", x == 3 ~ "Some trust",
                            x == 2 ~ "Not much trust", x == 1 ~ "No trust"),
           levels = TRUST_LEVELS)
}
visit_binary <- function(x) as.integer(dplyr::coalesce(x, 0) == 1)   # select-all item
EDU_LEVELS <- c("Some High School or Less", "High School Graduate",
                "Some College", "College Degree", "Graduate Degree")
CENSUS_REGION <- c(
    CT = "Northeast", ME = "Northeast", MA = "Northeast", NH = "Northeast",
    NJ = "Northeast", NY = "Northeast", PA = "Northeast", RI = "Northeast",
    VT = "Northeast",
    AL = "South", AR = "South", DE = "South", FL = "South", GA = "South",
    KY = "South", LA = "South", MD = "South", MS = "South", NC = "South",
    OK = "South", SC = "South", TN = "South", TX = "South", VA = "South",
    WV = "South", DC = "South",
    IL = "Midwest", IN = "Midwest", IA = "Midwest", KS = "Midwest", MI = "Midwest",
    MN = "Midwest", MO = "Midwest", NE = "Midwest", ND = "Midwest", OH = "Midwest",
    SD = "Midwest", WI = "Midwest",
    AK = "West", AZ = "West", CA = "West", CO = "West", HI = "West", ID = "West",
    MT = "West", NV = "West", NM = "West", OR = "West", UT = "West", WA = "West",
    WY = "West")

dat <- raw %>%
    dplyr::filter(!is.na(.data[[WEIGHT_VAR]]), .data[[WEIGHT_VAR]] > 0) %>%
    dplyr::mutate(
        era = factor(get_era(wave), levels = ERA_LEVELS),

        # Outcomes: "very closely" followed the recommendation in the past week
        # (top category, 4) vs any other answer.
        always_wear_mask     = as.integer(cov_beh_5 == 4),
        always_avoid_contact = as.integer(cov_beh_1 == 4),
        always_avoid_crowds  = as.integer(cov_beh_2 == 4),
        wash_hands_strict    = as.integer(cov_beh_3 == 4),
        # 24-h household activities. Descriptive analyses treat a missing
        # (unselected or not asked) item as 0; the adjusted models use visit_9
        # as recorded, which drops respondents who were not asked.
        visit_work   = visit_binary(visit_1),
        visit_store  = visit_binary(visit_3),
        visit_doctor = visit_binary(visit_9),

        # Institutional trust (exposures) and generalized trust
        trust_cdc   = trust_factor(cov_trust_cdc),
        trust_doc   = trust_factor(cov_trust_doctors),
        trust_state = trust_factor(cov_trust_state),
        trust_banks = trust_factor(cov_trust_banks),

        # Stratifiers used in the descriptive analyses (Figs 1-2, 5-6, S1-S10)
        age_cat = cut(age, breaks = c(18, 34, 54, 64, Inf), right = TRUE,
                      include.lowest = TRUE,
                      labels = c("18 to 34", "35 to 54", "55 to 64", "65 and over")),
        income_cat_5_fct = factor(income_cat_5, levels = 1:5,
                                  labels = c("Under $25,000", "$25,000 - $49,999",
                                             "$50,000 - $74,999", "$75,000 - $99,999",
                                             "$100,000 and over")),
        race_fct = relevel(factor(dplyr::case_when(
            race == "White" ~ "White",
            race == "Hispanic" ~ "Hispanic",
            race == "African American" ~ "African American",
            race %in% c("Asian", "Pacific Islander", "Asian American") ~ "Asian/Pacific Islander",
            TRUE ~ "Other")), ref = "White"),
        employ_fct = factor(dplyr::case_when(
            employ %in% c("Full-time", "Part-time", "Unemployed", "Retired") ~ as.character(employ),
            employ %in% c("Gig/Contract", "Home-maker", "Self-employed", "Student") ~ "Other"),
            levels = c("Full-time", "Part-time", "Unemployed", "Retired", "Other")),
        ideology_fct = factor(dplyr::case_when(ideology %in% 1:3 ~ "Liberal",
                                               ideology == 4 ~ "Moderate",
                                               ideology %in% 5:7 ~ "Conservative"),
                              levels = c("Liberal", "Moderate", "Conservative")),
        region_fct = factor(unname(CENSUS_REGION[as.character(state_code)]),
                            levels = c("Northeast", "Midwest", "South", "West")),

        # Covariates of the adjusted models (reference level listed first)
        age_cat4 = cut(age, breaks = c(18, 34, 54, 64, Inf), right = TRUE,
                       include.lowest = TRUE, labels = c("18-34", "35-54", "55-64", "65+")),
        gender_fct = relevel(factor(gender), ref = "Male"),
        race_cat4 = relevel(factor(dplyr::case_when(
            race == "White" ~ "White",
            race == "Hispanic" ~ "Hispanic",
            race == "African American" ~ "African American",
            TRUE ~ "Asian/PI/Other")), ref = "White"),
        education_hs_ref = relevel(factor(education_cat, levels = EDU_LEVELS),
                                   ref = "High School Graduate"),
        income_high_ref = relevel(factor(income_cat_5, levels = 1:5,
                                         labels = c("Under $25,000", "$25,000-$49,999",
                                                    "$50,000-$74,999", "$75,000-$99,999",
                                                    "$100,000 and over")),
                                  ref = "$100,000 and over"),
        party_ind_ref = relevel(factor(party3, levels = c("Democrat", "Republican",
                                                          "Independent/Other")),
                                ref = "Independent/Other"),
        party_dem_ref = factor(party3, levels = c("Democrat", "Republican",
                                                  "Independent/Other")),
        ruca_cat_fct = factor(dplyr::case_when(ruca_usda %in% 1:3 ~ "Urban",
                                               ruca_usda %in% 4:6 ~ "Suburban",
                                               ruca_usda %in% 7:10 ~ "Rural"),
                              levels = c("Urban", "Suburban", "Rural"))
    )

KEEP <- c("wave", "era", "survey_date", WEIGHT_VAR,
          PRIMARY_OUTCOMES, "vaccine_1", "visit_9", "visit_doctor", "visit_store",
          "visit_work",
          "trust_cdc", "trust_doc", "trust_state", "trust_banks", "trust",
          "age_cat", "gender", "income_cat_5_fct", "education_cat", "race_fct",
          "party3", "ideology_fct", "employ_fct", "urban_type", "region_fct",
          "age_cat4", "gender_fct", "race_cat4", "education_hs_ref",
          "income_high_ref", "party_ind_ref", "party_dem_ref", "ruca_cat_fct")
dat <- dat[, KEEP]

# ---- 3. Write the analysis file, codebook and sample sizes ---------------------
dir.create(dirname(DATA_FILE), recursive = TRUE, showWarnings = FALSE)
saveRDS(dat, DATA_FILE, compress = "xz")
log_msg("Wrote ", DATA_FILE, " (", format(nrow(dat), big.mark = ","), " rows, ",
        ncol(dat), " variables)")

describe <- c(
    wave = "CHIP50 survey wave (waves 4, 6, 8, 15 excluded)",
    era = "Pandemic era: Pre-vaccine (waves 1-13), Rollout (14-20), Omicron (21+)",
    survey_date = "Date the respondent started the survey",
    weight_state = "Post-stratification weight (rows with missing or zero weight dropped)",
    always_wear_mask = "Wore a mask outside the home 'very closely' in the past week (1/0)",
    always_avoid_contact = "Avoided contact with other people 'very closely' (1/0)",
    always_avoid_crowds = "Avoided public or crowded places 'very closely' (1/0)",
    wash_hands_strict = "Washed hands frequently 'very closely' (1/0)",
    vaccine_1 = "Received at least one COVID-19 vaccine dose (waves 14+; 1/0)",
    visit_9 = "Respondent or household member visited a doctor or hospital in the past 24 h (as recorded; adjusted models)",
    visit_doctor = "visit_9 with missing coded 0 (descriptive analyses)",
    visit_store = "Visited a store in the past 24 h (missing coded 0)",
    visit_work = "Went to work in the past 24 h (missing coded 0)",
    trust_cdc = "Trust in the CDC (No / Not much / Some / High trust)",
    trust_doc = "Trust in hospitals and doctors",
    trust_state = "Trust in state government",
    trust_banks = "Trust in banks (negative-control exposure)",
    trust = "Generalized trust, numeric as recorded (sensitivity analysis)",
    age_cat = "Age group (descriptive)", gender = "Gender as recorded",
    income_cat_5_fct = "Household income, 5 groups (descriptive)",
    education_cat = "Education, 5 groups", race_fct = "Race/ethnicity, 5 groups (descriptive)",
    party3 = "Party identification, 3 groups", ideology_fct = "Ideology, 3 groups",
    employ_fct = "Employment, 5 groups", urban_type = "Urbanicity as recorded (descriptive)",
    region_fct = "Census region (from state)",
    age_cat4 = "Age group (models; ref 18-34)", gender_fct = "Gender (sequential models; ref Male)",
    race_cat4 = "Race/ethnicity, 4 groups (models; ref White)",
    education_hs_ref = "Education (models; ref High School Graduate)",
    income_high_ref = "Household income (models; ref $100,000 and over)",
    party_ind_ref = "Party (wave-level models; ref Independent/Other)",
    party_dem_ref = "Party (sequential models; ref Democrat)",
    ruca_cat_fct = "Urbanicity from RUCA codes (models; ref Urban)")
codebook <- tibble::tibble(
    variable = names(dat),
    description = unname(describe[names(dat)]),
    type = vapply(dat, function(x) class(x)[1], character(1)),
    values = vapply(dat, function(x) {
        if (is.factor(x)) paste(levels(x), collapse = " | ")
        else if (is.character(x)) paste(sort(unique(stats::na.omit(x))), collapse = " | ")
        else if (inherits(x, "Date")) paste(range(x, na.rm = TRUE), collapse = " to ")
        else paste(signif(range(x, na.rm = TRUE), 4), collapse = " to ")
    }, character(1)),
    n_missing = vapply(dat, function(x) sum(is.na(x)), integer(1)))
readr::write_csv(codebook, file.path(dirname(DATA_FILE), "codebook.csv"))

# Respondents per wave, item nonresponse for the four behaviors and CDC trust
# (SI Text S2: < 1% per wave), and respondents without a recorded doctor/hospital
# item (not asked in parts of waves 11-13; SI Text S2).
sizes <- dat %>%
    dplyr::group_by(wave, era) %>%
    dplyr::summarise(n_respondents = dplyr::n(),
                     pct_missing_mask = 100 * mean(is.na(always_wear_mask)),
                     pct_missing_contact = 100 * mean(is.na(always_avoid_contact)),
                     pct_missing_crowds = 100 * mean(is.na(always_avoid_crowds)),
                     pct_missing_handwashing = 100 * mean(is.na(wash_hands_strict)),
                     pct_missing_trust_cdc = 100 * mean(is.na(trust_cdc)),
                     n_missing_visit_items = sum(is.na(visit_9))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(wave_dates(dat), by = "wave") %>%
    dplyr::relocate(wave_date, .after = era)
readr::write_csv(sizes, file.path(dirname(DATA_FILE), "sample_sizes_by_wave.csv"))
log_msg("Wrote codebook.csv and sample_sizes_by_wave.csv to ", dirname(DATA_FILE))
