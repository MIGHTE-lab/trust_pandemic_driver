# Trust as a hidden driver of epidemic-relevant behavior

Analytic code and data for

> Zapf\*, Dewey\*, Ognyanova, Baum, Hanage, Lipsitch, Uslu, Druckman, Perlis, Lazer, Santillana. Trust as a hidden driver of epidemic-relevant behavior: Institution- and behavior-specific associations across 20 national survey waves. *Proc Natl Acad Sci USA* 123, e2622260123 (2026). <https://doi.org/10.1073/pnas.2622260123> (\*authors contributed equally)

The code in this repository reproduces the figures (Figs 1–6, Figs S1–S21), Table 1, Table S1 and the numbers reported in the main text, and checks each reported number against the published value. The repository also contains an anonymized and streamlined version of the analyzed dataset and all results of the published analyses (estimates, tables and figures in `output/`), so the results can be inspected or re-plotted without running any model. Special cases and exceptions are explained under [Notes on specific numbers](#notes-on-specific-numbers).

## The study in brief

Based on 20 waves of the CHIP50 national survey (April 2020 to August 2022; approximately 22,000 respondents per wave), the study investigates whether institutional trust, especially trust in the CDC, differentiates people's reported protective behaviors more than sociodemographic stratifiers that are conventionally used in infectious disease transmission models. It combines five analytic streams:

| Step | Question | Method | Script |
|------------------|------------------|------------------|------------------|
| Descriptive | How large are the crude (unadjusted) differences by trust levels and by each sociodemographic group? | Survey-weighted prevalence by wave and group; highest minus lowest group ("adherence gap") | `02` |
| Inference (statistical) | In how many survey waves are the observed differences significant? | Per wave, survey-weighted logistic regression on each stratifier (sociodemographic and trust); design-based Wald F test; Benjamini–Hochberg (BH) correction across waves | `03` |
| Adjusted | After adjustment, for which institutions and which behaviors does trust matter in differentiating behavioral adoption? | Per wave and per pandemic era (aggregates of waves by key events), survey-weighted logistic regression with all covariates; marginal (standardized) high- vs no-trust risk difference (RD); trust in banks as a negative-control exposure | `04` |
| Iterative model comparison | How much do included blocks of covariates change the trust estimate and model fit? | Nested models M0–M4 (trust, + demographics, + socioeconomic status, + party and ideology, + geography); leave-one-block-out (LOBO) approach | `05` |
| Summary of estimates | Table S1, the Fig S18 statistics, the negative-control summary, and a check of all reported numbers | Medians and IQRs across waves | `06` |

## Quick start {#quick-start}

1.  Install R (≥ 4.3; the published run used R 4.5.1) and the packages (versions are detailed under [Software](#software)):

    ``` r
    install.packages(c("dplyr", "tidyr", "readr", "tibble", "haven", "survey",
                       "marginaleffects", "broom", "ggplot2", "patchwork", "scales",
                       "forcats", "ragg", "systemfonts"))
    ```

    Fig S20 also uses Python 3 with matplotlib and Pillow (`pip install -r python/requirements.txt`).

2.  The streamlined dataset `data/chip50_trust_analysis.rds` is included in the repository, so the presented analyses can be replicated. These represent a compacted version of the CHIP50 survey waves, which were cleaned and condensed for these analyses (see [data/README.md](data/README.md)).

3.  To replicate the presented analyses, run the following from the repository root (or after opening `trust_pandemic_driver.Rproj` in RStudio):

    ``` r
    source("run_all.R")      # or, in a terminal: Rscript run_all.R
    ```

    This refits every model (estimated runtime is about 11 hours; see [Run time](#run-time)) and overwrites the files in `output/`.

4.  Open `output/tables/check_against_published.csv` to see each reproduced number next to the published one.

Each script can also be run separately once the scripts in the hierarchy before it have run (`Rscript R/04_adjusted_models.R`). Scripts 06–08 read the CSV files in `output/`, so the tables and figures can be re-created from the stored estimates in a few minutes, without refitting any model:

``` r
source("R/06_summary_statistics.R"); source("R/07_figures.R"); source("R/08_fig_S21_model_schematic.R")
```

To run a single script with a non-default setting, assign the setting in the R session before sourcing it (see [Run settings](#run-settings)).

## Repository layout

```         
README.md                      this file
CITATION.cff                   how to cite (used by GitHub's "Cite this repository")
run_all.R                      runs everything in order; run settings at the top
R/
  00_setup.R                   paths, settings, constants and shared helpers (sourced by all)
  01_build_analysis_data.R     process raw survey waves -> analytic data/chip50_trust_analysis.rds
  02_descriptive_prevalence.R  weighted prevalence by wave and group; adherence gaps
  03_descriptive_inference.R   omnibus tests by wave; % of waves significant
  04_adjusted_models.R         adjusted RDs and ORs by wave and by era
  05_sequential_models.R       Table 1 (M0-M4) and leave-one-block-out by wave
  06_summary_statistics.R      Table S1, Fig S18 statistics, negative control, check vs paper
  07_figures.R                 Figs 1-6 and S1-S19 at publication quality
  08_fig_S21_model_schematic.R Fig S21
  figure_theme.R               PNAS figure sizes, fonts and save function
python/
  fig_S20_dag_panel.py         Fig S20
  requirements.txt             Python packages for Fig S20
data/
  chip50_trust_analysis.rds    analytic dataset: one row per respondent, 36 variables
  codebook.csv                 variables, coding and missing values
  sample_sizes_by_wave.csv     respondents, field dates and item nonresponse per wave
  README.md                    description of the data
reference/published_values.csv every reproduced number as printed in the paper
validation/                    synthetic test data and comparison with the original outputs
output/                        results of the published run (re-created by run_all.R)
  estimates/                   wave-level and era-level estimates (CSV)
  tables/                      Table 1, Table S1, numbers in the text, checks against the published analyses (CSV)
  figures/                     all figures (PDF and PNG)
  session_info.txt             R and package versions of the published run
```

## Data and sample

- **Included survey waves.** Waves 1–24 contain the analyzed trust items. Waves 4, 6 and 8 (small samples, no weights) and wave 15 were excluded, leaving 20 analyzed waves; the vaccination item was asked from wave 14 on (over 10 waves). The descriptive scripts also read waves 25–27 (to early 2023), which are used only where noted below.
- **Weights and design.** Prevalence, tests, odds ratios and risk differences use the post-stratification weight on the US state level `weight_state` in a single-stage design (`survey::svydesign(ids = ~1, weights = ~weight_state)`), with variances estimated via Taylor-linearization. Respondents without a positive weight are removed from the analyses. Three quantities of script 05 are unweighted, as in the published results: AIC and the leave-one-block-out Brier scores (from unweighted maximum-likelihood fits) and the averaging of the predicted probabilities behind the Table 1 risk differences (see `TABLE1_RD_AVERAGE`).
- **Outcomes.** The four primary behaviors are coded 1 when the respondent followed the recommendation "very closely" in the past week (the top of four categories): wearing a mask outside the home, avoiding contact with other people, avoiding crowded places, and frequent handwashing. Secondary outcomes are vaccination (at least one dose of a COVID-19 vaccine) and three past-24-hour household activities: visiting a doctor or hospital, visiting a store, or going to work.
- **Exposures.** Trust in the CDC (primary exposure), trust in doctors and hospitals, trust in state government, and trust in banks (negative control exposure), each with four levels from no trust to high trust; the contrast of interest is sepcified as "high" vs "no" trust.
- **Covariates.** Age, gender, race/ethnicity, education, household income, employment, party identification, ideology (liberal, moderate, conservative), RUCA urbanicity and Census region.

The variable coding is in [data/README.md](data/README.md).

## Brief description of each script

### 01 Build the analysis file

Reads the weighted wave files, sets survey missing-value codes (−99, −98, −97, −88, −77, 998, 999) to missing values, derives and transforms the outcomes, trust levels and covariate groupings, keeps respondents with a positive weight, and writes `data/chip50_trust_analysis.rds`, a codebook and per-wave sample sizes with item non-response.

### 02 Weighted prevalence by wave and group

For each outcome and analyzed stratifier, the survey-weighted proportion of the reported behavior in each wave and group, with 95% CIs (`survey::svymean`). These are the points of Figs 1, 2, 5 and 6 and SI Figs S1–S10. The script also computes the share of respondents at each trust level by wave (Fig S19) and, for each analyzed stratifier, the gap between the highest and the lowest group per wave. Over the 20 analyzed waves with trust data, the average gap is "38.8 pp for masking" by trust in the CDC, "25.9 pp" by party, and "6 to 10 pp" by age, education and income in (cf. Results in the main article).

### 03 Tests of differences between groups

For each outcome, stratifier and wave: a survey-weighted logistic regression of the behavior on the stratifier (levels with fewer than 30 respondents in a wave are dropped), an omnibus design-based Wald F test (`survey::regTermTest`), and pairwise risk differences between levels (`marginaleffects::avg_comparisons`) are computed. P values are BH-adjusted across waves within each outcome and analyzed stratifier. The share of waves with p \< 0.05 gives the Results' "100% of waves" for trust and the four behaviors, 50% for store visits, 75% for going to work, 1 of 20 waves for doctor visits, and 87%, 91% and 74% for income, education and race/ethnicity and doctor visits.

### 04 Adjusted models

For each trust exposure, outcome and wave, a survey-weighted logistic regression (`survey::svyglm`, quasibinomial) of the behavior on trust plus all covariates is fitted. The adjusted RD is the difference between the survey-weighted average predicted probabilities with everyone set to high trust and everyone set to no trust (marginal standardization, `marginaleffects::avg_comparisons`, delta-method SEs from the design-based covariance matrix). Three specifications:

- **primary:** all covariates included in the model (the M4 set of Table 1);
- **sensitivity_no_partisan:** party and ideology removed for the respective model;
- **sensitivity_general_trust:** primary plus generalized trust (not run for banks).

The negative-control exposure, trust in banks, is always modeled jointly with trust in the CDC (Lipsitch et al. 2010). The same models are also fit with the waves of each era pooled together, without (Spec A) and with (Spec B) per-wave fixed effects. The wave-level RDs are Figs 3–4 and S17 and the RDs referenced in the Results; the era-pooled ORs are Figs S11–S16; the "period" estimates in the text (bank RDs of 2.7, 4.5 and 6.2 pp; CDC ORs of 4.2, 5.2 and 3.2 in the joint models; "13 to 27 pp" for the other behaviors) refer to Spec A.

### 05 Sequential adjustment and leave-one-block-out analyses

For each primary protective behavior and wave, models M0–M4 are fit on one complete-case sample, twice: survey-weighted (`svyglm`), to estimate the trust OR (high vs no trust), the standardized RD (g-computation; see `TABLE1_RD_AVERAGE` below) and the survey-weighted Brier score; and to compute an unweighted maximum likelihood (`glm`) – to estimate the AIC, which is undefined for survey-weighted pseudo-likelihood fits. Table 1 reports the median [IQR] of the wave-specific estimates. The leave-one-block-out analysis iteratively drops each block from M4 and records the change in AIC and Brier score (unweighted fits) and a design-based Wald test of including the block.

### 06 Summary statistics and check against the published results

Summarizes the leave-one-block-out (LOBO) results by era (Table S1) and over all 20 waves (Results: +921 for trust vs +442 for party and ideology for masking), the agreement between the primary and generalized-trust RDs (Fig S18: r = 0.99; median change 0.8 pp), and the banks-vs-CDC comparison across waves ("56 to 96% smaller, median 82%"). It then compares about 700 reproduced numbers with `reference/published_values.csv` and writes `output/tables/check_against_published.csv`. A value matches if it rounds to the printed value.

### 07, 08 and Python script: Generate figures

`07_figures.R` draws Figs 1–6 and S1–S19 from the CSV files at publication quality (PDF, 600-dpi PNG and TIFF); `08_fig_S21_model_schematic.R` draws the schematic of the proposed model structure (Fig S21) and `python/fig_S20_dag_panel.py` the hypothetical statistical DAGs (Fig S20). Neither of the last two uses data. To redraw only some figures, set `RUN_SECTIONS` (any of `"mosaic"`, `"forest"`, `"rd"`, `"scatter"`, `"trust"`) before sourcing `07_figures.R`.

## Origin of specific result

| Publication | Output file (under `output/`) | Script |
|------------------------|------------------------|------------------------|
| Fig 1 (masking), Fig 2 (handwashing), Fig 5 (store), Fig 6 (doctor) | `estimates/prevalence_by_stratum.csv` -\> `figures/Fig1_masking` ... `Fig6_doctor_visits` | 02, 07 |
| Fig 3 (CDC), Fig 4 (banks): adjusted RDs | `estimates/adjusted_rd_by_wave_trust_cdc.csv`, `..._trust_banks.csv` -\> `figures/Fig3_adjusted_RD_CDC`, `Fig4_adjusted_RD_banks` | 04, 07 |
| Table 1 | `tables/table_1_sequential_adjustment.csv` (per wave: `estimates/sequential_models_by_wave.csv`) | 05 |
| Figs S1–S10 | `estimates/prevalence_by_stratum.csv` -\> `figures/FigS01_...` to `FigS10_...` | 02, 07 |
| Figs S11–S16 | `estimates/pooled_era_or_trust_cdc.csv`, `..._trust_banks.csv` | 04, 07 |
| Fig S17 | `estimates/adjusted_rd_by_wave_trust_cdc.csv` | 04, 07 |
| Fig S18 and statistics presented in legends | `tables/sensitivity_general_trust_comparison.csv`, `tables/fig_S18_concordance_stats.csv` | 06, 07 |
| Fig S19 | `estimates/trust_level_shares.csv` | 02, 07 |
| Fig S20, Fig S21 | `figures/FigS20_dag_panel`, `figures/FigS21_model_schematic` | Python, 08 |
| Table S1 | `tables/table_S1_lobo_by_era.csv` (and `_formatted.csv`; per wave: `estimates/lobo_by_wave.csv`) | 05, 06 |
| Unadjusted gaps (38.8, 25.9, 21–39, 6–10 pp) | `tables/adherence_gap_summary.csv`, `estimates/adherence_gap_by_wave.csv` | 02 |
| \% of significant waves (100%, 50%, 75%, 1 of 20; 87/91/74%) | `tables/significance_share.csv`, `estimates/omnibus_tests_by_wave.csv` | 03 |
| Adjusted RDs at waves 5, 16, 18 and 21 (all three specifications; doctors and hospitals; state government; vaccination; doctor visits) | `estimates/adjusted_rd_by_wave_<exposure>.csv` | 04 |
| Bank and CDC "period" estimates | `estimates/pooled_era_rd_<exposure>.csv`, `estimates/pooled_era_or_trust_banks.csv` (Spec A) | 04 |
| LOBO medians presented in the Results | `tables/lobo_all_waves.csv` | 06 |
| Negative-control comparison ("56 to 96%", vaccination 1.5 vs 36.6 pp) | `tables/negative_control_summary.csv` | 06 |
| Sample sizes, item non-response (SI Text S2) | `data/sample_sizes_by_wave.csv` (next to the analysis file) | 01 |
| All of the above vs the published values | `tables/check_against_published.csv` | 06 |

## Run settings {#run-settings}

The default settings are specified at the top of `run_all.R`; the defaults reproduce the published numbers. The first three configurations switch between the published computation and alternatives closely related to analyses described in the article.

| Setting | Default (as published) | Alternative | Scripts affected |
|------------------|------------------|------------------|------------------|
| `SDH_TEST_WAVES` | `"published"`: the significance shares of the sociodemographic stratifiers use all 23 waves read by the descriptive scripts (through early 2023). The shares for trust use the 20 waves with trust data. | `"trust_waves"`: all estimated shares use the 20 trust waves | 03, 06 |
| `LOBO_BRIER` | `"unweighted"`: LOBO Brier scores from the unweighted fits, as the Methods state and as in Table S1. | `"weighted"`: survey-weighted Brier scores | 05, 06 |
| `TABLE1_RD_AVERAGE` | `"unweighted"`: the predicted probabilities behind the Table 1 risk differences | `"weighted"`: averaged with the survey weights, as for the wave-level RDs of script 04. | 05, 06 |
| `MOSAIC_MATCH_TRUST_WAVES` | `TRUE`: the sociodemographic panels of the prevalence figures show the 20 trust waves, as in the final figures. | `FALSE`: they also show waves 25–27; the time axis then extends to early 2023. | 07 |
| `RUN_PAIRWISE_RD` | `TRUE` | `FALSE` skips the pairwise RDs of script 03, which none of the checked numbers uses (saves most of its run time). | 03 |
| `N_CORES` | up to 8 (number of cores minus 2) | Number of parallel R processes for scripts 03–05; 1 runs them sequentially. | 03–05 |
| `REBUILD_DATA` | `FALSE`: step 01 runs only if the analysis file does not exist yet. | `TRUE`: rebuild the analysis file from the original survey waves. | 01 |

After changing a setting, rerun the scripts in the last column, in chronological order. Outside of `run_all.R`, assign the setting before sourcing the scripts, for example:

``` r
LOBO_BRIER <- "weighted"
source("R/05_sequential_models.R"); source("R/06_summary_statistics.R")
```

File locations are set with environment variables, either for one session with `Sys.setenv()` or permanently in `~/.Renviron` (see [data/README.md](data/README.md)):

| Variable | Meaning | Default |
|------------------------|------------------------|------------------------|
| `C50_WEIGHTED_RDS_DIR` | Folder with the original survey data `CSP_W<wave>_weighted.RDS` (script 01 only) | none – comprehensive survey data have not been published, see compacted analytic dataset below |
| `C50_ANALYSIS_FILE` | The analysis file written by script 01 and read by scripts 02–05; the codebook and sample sizes are written based on this dataset | `data/chip50_trust_analysis.rds` |
| `C50_OUTPUT_DIR` | Folder for all results, including Fig S20 | `output/` |

## Notes on specific numbers {#notes-on-specific-numbers}

`check_against_published.csv` flags any reproduced number that does not round to the printed value. The items below explain how specific numbers in the paper were produced.

- **"On average 6 to 10 pp for age, educational attainment, and household income"**: the mean masking gaps are 6.7, 9.4 and 5.9 pp.
- **Significance shares for sociodemographic stratifiers** (87%, 91% and 74% for income, education and race/ethnicity and doctor visits) were computed over the 23 descriptive waves (20/23, 21/23 and 17/23 waves). Analogous estimates ove the 20 waves where trust data was collected can be computed by setting `SDH_TEST_WAVES = "trust_waves"`.
- **"Significant in only 1 of 20 waves" for doctor visits** is the share of waves with a significant unadjusted difference by trust (cf. script 03). In the adjusted models (script 04), the 95% CI of the high- vs no-trust RD excludes zero in 4 of 20 waves.
- **Table 1 risk differences** standardize the survey-weighted model predictions with equal weight per respondent, whereas the wave-level RDs of script 04 (Figs 3–4, S17 and the RDs in the text) use the survey weights. The models are identical, so the ORs agree, but the RDs differ slightly: for masking, the M4 median in Table 1 is 31.1 pp and the median of the primary wave-level RDs is 31.4 pp. `TABLE1_RD_AVERAGE = "weighted"` gives the survey-weighted version of Table 1, whose M4 RDs equal the primary wave-level RDs.
- **Bank "period" estimates** are from the era-pooled models without wave fixed effects (Spec A); Spec B estimates are in the same files.
- **Generalized trust** is the survey variable `trust` entered as a numeric covariate; the paper calls it an "index: of general trust in others.
- **Trust in doctors and hospitals and in state government:** the wave-level and era-pooled estimates for these exposures (including the generalized-trust sensitivity analysis) are produced by script 04; the SI shows only their distributions (Fig S19).
- **"Pairwise risk differences below 2 pp" for doctor visits:** the unadjusted pairwise differences between trust levels are in `estimates/pairwise_rd_by_wave.csv` (stratifier `trust_cdc`, outcome `visit_doctor`).
- **Era-pooled models** (Spec A and B) are described in the legends of Figs S11–S16 rather than in the Methods.
- **Leave-one-block-out (LOBO) and trust estimates:** the LOBO output reports the change in AIC, Brier score and the Wald test of each block. How the trust estimate changes without a block can be read from M3 (M4 without geography, Table 1) and from the specification without party and ideology (script 04).
- **Text S1 cites "20 of 20 waves"** for the adjusted trust difference at every stage of adjustment: this is the column `waves_with_trust_q_lt_0_05` of `tables/table_1_sequential_adjustment.csv`, which is not printed in Table 1.

## Numbers in the paper that are not computed here

These come from cited work based on the same CHIP50 survey data, but are not derived from this analysis:

- the within-person Spearman correlation of 0.76 between the two trust item wordings (Perlis et al. 2024);
- the decline in high trust in physicians and hospitals of about 31 percentage points from April 2020 to January 2024 (Perlis et al. 2024);
- the roughly threefold decline in social distancing from April 2020 to May 2022, and the decomposition into a declining trend and a mortality-synchronized oscillation (Urmi et al. 2025);
- survey recruitment, quotas and weighting (Urmi et al. 2025, SI Appendix).

## Validation

- **Against the published results.** `output/tables/check_against_published.csv` compares 714 reproduced numbers with the paper. In the published run, 712 match, including every value of Table 1 and Table S1; the two exceptions are explained under [Notes on specific numbers](#notes-on-specific-numbers).

## Run time {#run-time}

The published run took about 11 hours on a Windows machine with the default `N_CORES`: script 04 about 10 hours, script 03 about 30 minutes, and every other step under 15 minutes. Setting `RUN_PAIRWISE_RD <- FALSE` shortens script 03. Scripts 06–08 need only the CSV files in `output/`, so the tables and figures can be re-created in minutes (see [Quick start](#quick-start)).

## Software {#software}

The published run used R 4.5.1 on Windows 11 with these CRAN packages (full list in `output/session_info.txt`):

| Package | Version | Used by |
|------------------------|------------------------|------------------------|
| dplyr, tidyr, readr, tibble | 1.2.1, 1.3.1, 2.1.5, 3.3.0 | all scripts |
| survey | 4.4-8 | 00, 02–05 |
| haven | 2.5.5 | 01 (raw waves only), synthetic test |
| marginaleffects | 0.32.0 | 03, 04 |
| broom | 1.0.10 | 04 |
| ggplot2, patchwork, scales, forcats | 3.5.2, 1.3.1, 1.4.0, 1.0.0 | 07, 08 |
| ragg, systemfonts | 1.4.0, 1.2.3 | 07, 08 |

The code was also tested with R 4.3.3 (survey 4.2-1, marginaleffects 0.18.0, dplyr 1.1.4, ggplot2 3.4.4). It needs no other packages or files: the shared helpers are in `R/00_setup.R` and `R/figure_theme.R`. Figures use the Arial font where installed, otherwise Liberation Sans. Fig S20 uses Python 3 with matplotlib and Pillow (tested with matplotlib 3.11 and Pillow 12).

## Citation and contact

Please cite the paper above when using this code or data (GitHub's "Cite this repository" button gives the citation). Questions about the code can be addressed to: A. Zapf ([azapf\@g.harvard.edu](mailto:azapf@g.harvard.edu){.email}); corresponding author of the paper: M. Santillana.
