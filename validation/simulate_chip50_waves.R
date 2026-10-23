## ============================================================================ #
## simulate_chip50_waves.R
## Writes SYNTHETIC survey waves with the same file names, variable names and
## codings as the raw CHIP50 files the pipeline reads. Use it to test the code
## without access to the survey data. The numbers it produces are meaningless.
##
## Usage (from the repository root):
##   Rscript validation/simulate_chip50_waves.R [output folder] [respondents per wave]
## then
##   C50_WEIGHTED_RDS_DIR=<output folder> C50_ANALYSIS_FILE=<tmp>.rds \
##   C50_OUTPUT_DIR=<tmp folder> Rscript run_all.R
## ============================================================================ #

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else file.path(tempdir(), "synthetic_chip50")
n_per_wave <- if (length(args) >= 2) as.integer(args[2]) else 1000L
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(20261022)

waves <- setdiff(1:27, c(4, 6, 8, 15))
field_start <- as.Date(c(`1` = "2020-04-17", `2` = "2020-05-01", `3` = "2020-05-15",
                         `5` = "2020-06-12", `7` = "2020-07-10", `9` = "2020-08-07",
                         `10` = "2020-09-04", `11` = "2020-10-02", `12` = "2020-10-23",
                         `13` = "2020-11-03", `14` = "2020-12-16", `16` = "2021-02-05",
                         `17` = "2021-04-01", `18` = "2021-06-09", `19` = "2021-08-26",
                         `20` = "2021-11-03", `21` = "2021-12-22", `22` = "2022-03-02",
                         `23` = "2022-06-08", `24` = "2022-08-11", `25` = "2022-10-06",
                         `26` = "2022-12-22", `27` = "2023-03-01"))
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID",
            "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
            "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
            "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
with_missing <- function(x, p = 0.01, code = NA) {
    x[stats::runif(length(x)) < p] <- code
    x
}

for (w in waves) {
    n <- n_per_wave
    party <- sample(c("Democrat", "Republican", "Independent/Other"), n, TRUE,
                    prob = c(0.38, 0.33, 0.29))
    ideology <- pmin(7, pmax(1, round(4 + 1.6 * (party == "Republican") -
                                        1.6 * (party == "Democrat") + stats::rnorm(n, 0, 1.2))))
    age <- round(stats::runif(n, 18, 88))
    # latent disposition to trust; declines over the pandemic, partisan gradient
    z <- stats::rnorm(n) + 0.6 * (party == "Democrat") - 0.6 * (party == "Republican") -
        0.025 * (w - 1)
    trust_item <- function(shift, slope = 1) as.numeric(cut(slope * z + shift + stats::rnorm(n, 0, 0.7),
                                                            c(-Inf, -1.2, -0.3, 0.6, Inf),
                                                            labels = FALSE))
    d <- data.frame(
        id = 100000L * w + seq_len(n),
        caseid = sprintf("W%02d_%05d", w, seq_len(n)),
        StartDate = as.POSIXct(field_start[as.character(w)] + sample(0:20, n, TRUE)) +
            stats::runif(n, 0, 86000),
        weight_state = with_missing(stats::rlnorm(n, 0, 0.45), 0.005),
        age = with_missing(age, 0.005),
        gender = sample(c("Female", "Male"), n, TRUE),
        race = with_missing(sample(c("White", "Hispanic", "African American", "Asian",
                                     "Pacific Islander", "Native American", "Other"), n, TRUE,
                                   prob = c(0.6, 0.16, 0.12, 0.06, 0.01, 0.03, 0.02)), 0.01),
        education_cat = sample(c("Some High School or Less", "High School Graduate",
                                 "Some College", "College Degree", "Graduate Degree"), n, TRUE,
                               prob = c(0.07, 0.27, 0.3, 0.22, 0.14)),
        income_cat_5 = with_missing(sample(1:5, n, TRUE, prob = c(0.22, 0.22, 0.18, 0.13, 0.25)),
                                    0.02, -99),
        employ = sample(c("Full-time", "Part-time", "Unemployed", "Retired", "Gig/Contract",
                          "Home-maker", "Self-employed", "Student"), n, TRUE,
                        prob = c(0.38, 0.1, 0.1, 0.22, 0.03, 0.07, 0.06, 0.04)),
        party3 = party, ideology = with_missing(ideology, 0.01),
        state_code = sample(states, n, TRUE),
        ruca_usda = sample(1:10, n, TRUE, prob = c(0.55, 0.1, 0.05, 0.08, 0.04, 0.03, 0.06,
                                                   0.03, 0.02, 0.04)),
        urban_type = sample(c("Urban", "Suburban", "Rural"), n, TRUE, prob = c(0.3, 0.5, 0.2)),
        stringsAsFactors = FALSE)
    tl <- if (w <= 24) trust_item(0) else round(stats::runif(n, 1, 4))
    if (w <= 24) {                                   # trust items fielded in waves 1-24
        d$cov_trust_cdc     <- with_missing(tl, 0.01)
        d$cov_trust_doctors <- with_missing(trust_item(0.9), 0.01)
        d$cov_trust_state   <- with_missing(trust_item(-0.3, 0.8), 0.01)
        d$cov_trust_banks   <- with_missing(trust_item(-0.2, 0.35), 0.01)
        d$trust             <- with_missing(pmin(4, pmax(1, round(2.5 + 0.4 * z +
                                                                  stats::rnorm(n, 0, 0.8)))), 0.02)
    }
    era_shift <- c(0.6, 0.9, -0.4)[findInterval(w, c(1, 14, 21))]
    behavior <- function(intercept, b_trust) {
        lin <- intercept + era_shift + b_trust * (tl - 2.5) + 0.35 * (party == "Democrat") -
            0.35 * (party == "Republican") + 0.012 * (age - 50) + stats::rnorm(n, 0, 0.3)
        always <- stats::runif(n) < stats::plogis(lin)
        with_missing(ifelse(always, 4, sample(1:3, n, TRUE)), 0.008)
    }
    d$cov_beh_1 <- behavior(-0.6, 0.55)            # avoid contact
    d$cov_beh_2 <- behavior(-0.2, 0.6)             # avoid crowds
    d$cov_beh_3 <- behavior(0.6, 0.45)             # wash hands
    d$cov_beh_5 <- haven::labelled(behavior(0.3, 0.8),   # mask (labelled, as in raw files)
                                   c(`Not at all closely` = 1, `Very closely` = 4))
    d$cov_beh_5[stats::runif(n) < 0.002] <- -99
    not_asked <- if (w %in% 11:13) stats::runif(n) < 0.08 else rep(FALSE, n)
    visit <- function(p) ifelse(not_asked, NA, as.integer(stats::runif(n) < p))
    d$visit_1 <- visit(stats::plogis(-0.4 - 0.1 * (tl - 2.5)))
    d$visit_3 <- visit(stats::plogis(0.1 - 0.15 * (tl - 2.5)))
    d$visit_9 <- visit(stats::plogis(-2.2 + 0.01 * (age - 50)))
    if (w >= 14) d$vaccine_1 <- with_missing(as.integer(stats::runif(n) <
        stats::plogis(-1.2 + 0.25 * (w - 14) + 1.0 * (tl - 2.5) + 0.02 * (age - 50))), 0.01)
    saveRDS(d, file.path(out_dir, sprintf("CSP_W%d_weighted.RDS", w)))
}
cat("Wrote", length(waves), "synthetic waves (", n_per_wave, "respondents each) to", out_dir, "\n")
