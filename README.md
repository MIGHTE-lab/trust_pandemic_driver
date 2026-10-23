# Trust as a hidden driver of epidemic-relevant behavior

Analysis code and data for

> Zapf\*, Dewey\*, Ognyanova, Baum, Hanage, Lipsitch, Uslu, Druckman, Perlis, Lazer, Santillana. Trust as a hidden driver of epidemic-relevant behavior: Institution- and behavior-specific associations across 20 national survey waves. *Proc Natl Acad Sci USA* 123, e2622260123 (2026). https://doi.org/10.1073/pnas.2622260123 (\*equal contribution)

The code reproduces every figure (Figs 1–6, Figs S1–S21), Table 1, Table S1 and the numbers reported in the text, and checks each reported number against the published value. The repository also contains the anonymized analysis data set and all results of the published run (estimates, tables and figures in `output/`), so the results can be inspected or re-plotted without running any model. The published run reproduces 712 of the 714 numbers checked; the two exceptions are explained under [Notes on specific numbers](#notes-on-specific-numbers).

## The study in brief

Using 20 waves of the CHIP50 national survey (April 2020 to August 2022; about 22,000 respondents per wave), the paper asks whether institutional trust, especially trust in the CDC, separates people's protective behaviors more than the demographic stratifiers used in transmission models. It combines five analyses:

| Step | Question | Method | Script |
|---|---|---|---|
| Descriptive | How large are the raw differences by trust and by each sociodemographic group? | Survey-weighted prevalence by wave and group; highest minus lowest group ("adherence gap") | `02` |
| Inference | In how many waves are the differences significant? | Per wave, survey-weighted logistic regression on the stratifier; design-based Wald F test; Benjamini–Hochberg (BH) correction across waves | `03` |
| Adjusted | Does trust matter after adjustment, for which institutions and which behaviors? | Per wave and per pandemic era, survey-weighted logistic regression with all covariates; marginal (standardized) high- vs no-trust risk difference (RD); three specifications; trust in banks as a negative-control exposure | `04` |
| Model building | How much do covariate blocks change the trust estimate and model fit? | Nested models M0–M4 (trust, + demographics, + socioeconomic status, + party and ideology, + geography); leave-one-block-out (LOBO) | `05` |
| Summaries | Table S1, the Fig S18 statistics, the negative-control summary, and a check of all reported numbers | Medians and IQRs across waves | `06` |

## Quick start

1. Install R (≥ 4.3; the published run used R 4.5.1) and the packages (versions under [Software](#software)):

   ```r
   install.packages(c("dplyr", "tidyr", "readr", "tibble", "haven", "survey",
                      "marginaleffects", "broom", "ggplot2", "patchwork", "scales",
                      "forcats", "ragg", "systemfonts"))
   ```

   Fig S20 also needs Python 3 with matplotlib and Pillow (`pip install -r python/requirements.txt`).
2. The analysis data set `data/chip50_trust_analysis.rds` is included, so the pipeline starts from it. The raw CHIP50 waves are needed only to rebuild that file (see [data/README.md](data/README.md)).
3. From the repository root (or after opening `trust_pandemic_driver.Rproj` in RStudio):

   ```r
   source("run_all.R")      # or, in a terminal: Rscript run_all.R
   ```

   This refits every model (about 11 hours; see [Run time](#run-time)) and overwrites the files in `output/`.
4. Open `output/tables/check_against_published.csv` to see each reproduced number next to the published one.

Each script can also be run on its own once the scripts before it have run (`Rscript R/04_adjusted_models.R`). Scripts 06–08 read only the CSV files in `output/`, so the tables and figures can be re-created from the included estimates in a few minutes, without refitting any model:

```r
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
  01_build_analysis_data.R     raw survey waves -> data/chip50_trust_analysis.rds
  02_descriptive_prevalence.R  weighted prevalence by wave and group; adherence gaps
  03_descriptive_inference.R   omnibus tests by wave; % of waves significant
  04_adjusted_models.R         adjusted RDs and ORs by wave and by era
  05_sequential_models.R       Table 1 (M0-M4) and leave-one-block-out by wave
  06_summary_statistics.R      Table S1, Fig S18 statistics, negative control, check vs paper
  07_figures.R                 Figs 1-6 and S1-S19 at final printed size
  08_fig_S21_model_schematic.R Fig S21
  figure_theme.R               PNAS figure sizes, fonts and save function
python/
  fig_S20_dag_panel.py         Fig S20
  requirements.txt             Python packages for Fig S20
data/
  chip50_trust_analysis.rds    analysis data set: one row per respondent, 36 variables
  codebook.csv                 variables, coding and missing values
  sample_sizes_by_wave.csv     respondents, field dates and item nonresponse per wave
  README.md                    description of the data
reference/published_values.csv every reproduced number as printed in the paper
validation/                    synthetic test data and comparison with the original outputs
output/                        results of the published run (re-created by run_all.R)
  estimates/                   wave-level and era-level estimates (CSV)
  tables/                      Table 1, Table S1, numbers in the text, check vs paper (CSV)
  figures/                     all figures (PDF and PNG)
  session_info.txt             R and package versions of the published run
```

## Data and sample

* **Waves.** Waves 1–24 contain the trust items. Waves 4, 6 and 8 (small samples, no weights) and wave 15 are excluded, leaving 20 waves; the vaccination item was asked from wave 14 (10 waves). The descriptive scripts also read waves 25–27 (to early 2023), which are used only where noted below.
* **Weights and design.** Prevalences, tests, odds ratios and risk differences use the post-stratification weight `weight_state` in a single-stage design (`survey::svydesign(ids = ~1, weights = ~weight_state)`), with Taylor-linearization variances. Respondents without a positive weight are dropped. Three quantities of script 05 are unweighted, as in the published results: AIC and the leave-one-block-out Brier scores (from unweighted maximum-likelihood fits) and the averaging of the predicted probabilities behind the Table 1 risk differences (see `TABLE1_RD_AVERAGE`).
* **Outcomes.** The four primary behaviors are coded 1 when the respondent followed the recommendation "very closely" in the past week (the top of four categories): wearing a mask outside the home, avoiding contact with other people, avoiding crowded places, frequent handwashing. Secondary outcomes are vaccination (at least one dose) and three past-24-hour household activities: visiting a doctor or hospital, a store, or going to work.
* **Exposures.** Trust in the CDC (primary), in doctors and hospitals, in state government, and in banks (negative control), each with four levels from no trust to high trust; the contrast of interest is high vs no trust.
* **Covariates.** Age, gender, race/ethnicity, education, household income, employment, party identification, ideology, RUCA urbanicity and Census region.

The variable coding is in [data/README.md](data/README.md).

## What each script does

### 01 Build the analysis file
Reads the weighted wave files, sets survey missing-value codes (−99, −98, −97, −88, −77, 998, 999) to missing, derives the outcomes, trust levels and covariate groupings exactly as the original pipeline did, keeps respondents with a positive weight, and writes `data/chip50_trust_analysis.rds`, a codebook and per-wave sample sizes with item nonresponse.

### 02 Weighted prevalence by wave and group
For each outcome and stratifier, the survey-weighted share with the behavior in each wave and group, with 95% CIs (`survey::svymean`). These are the points of Figs 1, 2, 5 and 6 and SI Figs S1–S10. It also computes the share of respondents at each trust level by wave (Fig S19) and, for each stratifier, the gap between the highest and the lowest group in each wave. Averaged over the 20 trust waves, the gap is the "38.8 pp for masking" by trust in the CDC, "25.9 pp" by party, and "6 to 10 pp" by age, education and income in the Results.

### 03 Tests of differences between groups
For each outcome, stratifier and wave: a survey-weighted logistic regression of the behavior on the stratifier (levels with fewer than 30 respondents in the wave are dropped), an omnibus design-based Wald F test (`survey::regTermTest`), and pairwise risk differences between levels (`marginaleffects::avg_comparisons`). P values are BH-adjusted across waves within each outcome and stratifier. The share of waves with q < 0.05 gives the Results' "100% of waves" for trust and the four behaviors, 50% for store visits, 75% for going to work, 1 of 20 waves for doctor visits, and 87%, 91% and 74% for income, education and race/ethnicity and doctor visits.

### 04 Adjusted models
For each trust exposure, outcome and wave, a survey-weighted logistic regression (`survey::svyglm`, quasibinomial) of the behavior on trust plus all covariates. The adjusted RD is the difference between the survey-weighted average predicted probabilities with everyone set to high trust and to no trust (marginal standardization, `marginaleffects::avg_comparisons`, delta-method SEs from the design-based covariance matrix). Three specifications:

* **primary:** all covariates (the M4 set of Table 1);
* **sensitivity_no_partisan:** party and ideology removed;
* **sensitivity_general_trust:** primary plus generalized trust (not run for banks).

Trust in banks is always modeled jointly with trust in the CDC (negative-control exposure; Lipsitch et al. 2010). The same models are also fit with the waves of each era pooled, without (Spec A) and with (Spec B) wave fixed effects. The wave-level RDs are Figs 3–4 and S17 and the RDs quoted in the Results; the era-pooled ORs are Figs S11–S16; the "period" estimates in the text (bank RDs of 2.7, 4.5 and 6.2 pp; CDC ORs of 4.2, 5.2 and 3.2 in the joint models; "13 to 27 pp" for the other behaviors) are Spec A.

### 05 Sequential adjustment and leave-one-block-out
For each primary behavior and wave, models M0–M4 are fit on one complete-case sample, twice: survey-weighted (`svyglm`), for the trust OR (high vs no trust), the standardized RD (g-computation; see `TABLE1_RD_AVERAGE` below) and the survey-weighted Brier score; and unweighted maximum likelihood (`glm`), for AIC, which is undefined for survey-weighted pseudo-likelihood fits. Table 1 reports the median [IQR] of the wave-specific values. The leave-one-block-out analysis drops each block from M4 in turn and records the change in AIC and Brier score (unweighted fits) and a design-based Wald test of the block.

### 06 Summary statistics and check against the paper
Summarizes the LOBO results by era (Table S1) and over all 20 waves (Results: +921 for trust vs +442 for party and ideology for masking), the agreement between the primary and generalized-trust RDs (Fig S18: r = 0.99; median change 0.8 pp), and the banks-vs-CDC comparison across waves ("56 to 96% smaller, median 82%"). It then compares about 700 reproduced numbers with `reference/published_values.csv` and writes `output/tables/check_against_published.csv`. A value matches if it rounds to the printed value.

### 07, 08 and Python: figures
`07_figures.R` draws Figs 1–6 and S1–S19 from the CSV files at their printed size (PDF, 600-dpi PNG and TIFF); `08_fig_S21_model_schematic.R` draws the model schematic (Fig S21) and `python/fig_S20_dag_panel.py` the causal diagrams (Fig S20). Neither of the last two uses data. To redraw only some figures, set `RUN_SECTIONS` (any of `"mosaic"`, `"forest"`, `"rd"`, `"scatter"`, `"trust"`) before sourcing `07_figures.R`.

## Where each result comes from

| Paper | Output file (under `output/`, except the sample sizes) | Script |
|---|---|---|
| Fig 1 (masking), Fig 2 (handwashing), Fig 5 (store), Fig 6 (doctor) | `estimates/prevalence_by_stratum.csv` -> `figures/Fig1_masking` ... `Fig6_doctor_visits` | 02, 07 |
| Fig 3 (CDC), Fig 4 (banks): adjusted RDs | `estimates/adjusted_rd_by_wave_trust_cdc.csv`, `..._trust_banks.csv` -> `figures/Fig3_adjusted_RD_CDC`, `Fig4_adjusted_RD_banks` | 04, 07 |
| Table 1 | `tables/table_1_sequential_adjustment.csv` (per wave: `estimates/sequential_models_by_wave.csv`) | 05 |
| Figs S1–S10 | `estimates/prevalence_by_stratum.csv` -> `figures/FigS01_...` to `FigS10_...` | 02, 07 |
| Figs S11–S16 | `estimates/pooled_era_or_trust_cdc.csv`, `..._trust_banks.csv` | 04, 07 |
| Fig S17 | `estimates/adjusted_rd_by_wave_trust_cdc.csv` | 04, 07 |
| Fig S18 and legend statistics | `tables/sensitivity_general_trust_comparison.csv`, `tables/fig_S18_concordance_stats.csv` | 06, 07 |
| Fig S19 | `estimates/trust_level_shares.csv` | 02, 07 |
| Fig S20, Fig S21 | `figures/FigS20_dag_panel`, `figures/FigS21_model_schematic` | Python, 08 |
| Table S1 | `tables/table_S1_lobo_by_era.csv` (and `_formatted.csv`; per wave: `estimates/lobo_by_wave.csv`) | 05, 06 |
| Unadjusted gaps (38.8, 25.9, 21–39, 6–10 pp) | `tables/adherence_gap_summary.csv`, `estimates/adherence_gap_by_wave.csv` | 02 |
| % of waves significant (100%, 50%, 75%, 1 of 20; 87/91/74%) | `tables/significance_share.csv`, `estimates/omnibus_tests_by_wave.csv` | 03 |
| Adjusted RDs at waves 5, 16, 18 and 21 (all three specifications; doctors and hospitals; state government; vaccination; doctor visits) | `estimates/adjusted_rd_by_wave_<exposure>.csv` | 04 |
| Bank and CDC "period" estimates; "13 to 27 pp" | `estimates/pooled_era_rd_<exposure>.csv`, `estimates/pooled_era_or_trust_banks.csv` (Spec A) | 04 |
| LOBO medians in the Results | `tables/lobo_all_waves.csv` | 06 |
| Negative-control comparison ("56 to 96%", vaccination 1.5 vs 36.6 pp) | `tables/negative_control_summary.csv` | 06 |
| Sample sizes, item nonresponse (SI Text S2) | `data/sample_sizes_by_wave.csv` (next to the analysis file) | 01 |
| All of the above vs the printed values | `tables/check_against_published.csv` | 06 |

## Run settings

The settings are at the top of `run_all.R`; the defaults reproduce the published numbers. The first three switch between the published computation and an alternative that the paper's text describes or implies.

| Setting | Default (as published) | Alternative | Scripts affected |
|---|---|---|---|
| `SDH_TEST_WAVES` | `"published"`: the significance shares of the sociodemographic stratifiers use all 23 waves read by the descriptive scripts (through early 2023). The shares for trust always use the 20 trust waves. | `"trust_waves"`: all shares use the 20 trust waves, as the Methods describe. | 03, 06 |
| `LOBO_BRIER` | `"unweighted"`: LOBO Brier scores from the unweighted fits, as the Methods state and as in Table S1. | `"weighted"`: survey-weighted Brier scores, as the Table S1 footnote states. | 05, 06 |
| `TABLE1_RD_AVERAGE` | `"unweighted"`: the predicted probabilities behind the Table 1 risk differences are averaged with equal weight per respondent, as in published Table 1. | `"weighted"`: averaged with the survey weights, as for the wave-level RDs of script 04. | 05, 06 |
| `MOSAIC_MATCH_TRUST_WAVES` | `TRUE`: the sociodemographic panels of the prevalence figures show the 20 trust waves, as in the final figures. | `FALSE`: they also show waves 25–27; the time axis then extends to early 2023. | 07 |
| `RUN_PAIRWISE_RD` | `TRUE` | `FALSE` skips the pairwise RDs of script 03, which none of the checked numbers uses (saves most of its run time). | 03 |
| `N_CORES` | up to 8 (number of cores minus 2) | Number of parallel R processes for scripts 03–05; 1 runs them sequentially. | 03–05 |
| `REBUILD_DATA` | `FALSE`: step 01 runs only if the analysis file does not exist yet. | `TRUE`: rebuild the analysis file from the raw waves. | 01 |

After changing a setting, rerun the scripts in the last column, in order. Outside `run_all.R`, assign the setting before sourcing the scripts, for example:

```r
LOBO_BRIER <- "weighted"
source("R/05_sequential_models.R"); source("R/06_summary_statistics.R")
```

File locations are set with environment variables, either for one session with `Sys.setenv()` or permanently in `~/.Renviron` (see [data/README.md](data/README.md)):

| Variable | Meaning | Default |
|---|---|---|
| `C50_WEIGHTED_RDS_DIR` | Folder with the raw waves `CSP_W<wave>_weighted.RDS` (script 01 only) | none |
| `C50_ANALYSIS_FILE` | The analysis file written by script 01 and read by scripts 02–05; the codebook and sample sizes are written next to it | `data/chip50_trust_analysis.rds` |
| `C50_OUTPUT_DIR` | Folder for all results, including Fig S20 | `output/` |

## Notes on specific numbers

`check_against_published.csv` flags any reproduced number that does not round to the printed value. In the published run, 712 of the 714 checked numbers match; the two flagged numbers are the first two items below. The other items explain how specific numbers in the paper were produced.

* **"The trust contrast exceeded the partisan difference in every wave"** holds in 19 of the 20 waves for masking: in wave 24 the Democratic–Republican gap (23.1 pp) is larger than the high–no CDC-trust gap (21.6 pp).
* **"On average 6 to 10 pp for age, educational attainment, and household income"**: the mean masking gaps are 6.7, 9.4 and 5.9 pp.
* **Significance shares for sociodemographic stratifiers** (87%, 91% and 74% for income, education and race/ethnicity and doctor visits) were computed over the 23 descriptive waves (20/23, 21/23 and 17/23 waves). With `SDH_TEST_WAVES = "trust_waves"` they are computed over the 20 trust waves.
* **"Significant in only 1 of 20 waves" for doctor visits** is the share of waves with a significant unadjusted difference by trust (script 03). In the adjusted models (script 04), the 95% CI of the high- vs no-trust RD excludes zero in 4 of 20 waves.
* **Table 1 risk differences** standardize the survey-weighted model predictions with equal weight per respondent, whereas the wave-level RDs of script 04 (Figs 3–4, S17 and the RDs in the text) use the survey weights. The models are identical, so the ORs agree, but the RDs differ slightly: for masking, the M4 median in Table 1 is 31.1 pp and the median of the primary wave-level RDs is 31.4 pp. `TABLE1_RD_AVERAGE = "weighted"` gives the survey-weighted version of Table 1, whose M4 RDs equal the primary wave-level RDs.
* **Bank "period" estimates** are from the era-pooled models without wave fixed effects (Spec A); Spec B estimates are in the same files.
* **Generalized trust** is the survey variable `trust` entered as a numeric covariate; the paper calls it an index of general trust in others.
* **Gender** is part of every adjusted model (Table 1 M1 and the Fig 3 caption list it), although some covariate lists in the text omit it.
* **Trust in doctors and hospitals and in state government:** the wave-level and era-pooled estimates for these exposures (including the generalized-trust sensitivity analysis) are written by script 04, although the SI shows only their distributions (Fig S19).
* **"Pairwise risk differences below 2 pp" for doctor visits:** the unadjusted pairwise differences between trust levels are in `estimates/pairwise_rd_by_wave.csv` (stratifier `trust_cdc`, outcome `visit_doctor`); the adjusted high- vs no-trust RD reaches 2.9 pp in wave 21 (Fig 3).
* **Era-pooled models** (Spec A and B) are described in the legends of Figs S11–S16 rather than in the Methods.
* **Leave-one-block-out and the trust estimate:** the LOBO output reports the change in AIC, Brier score and the Wald test of each block. How the trust estimate changes without a block can be read from M3 (M4 without geography, Table 1) and from the specification without party and ideology (script 04).
* **Text S1 cites "20 of 20 waves"** for the adjusted trust difference at every stage of adjustment: this is the column `waves_with_trust_q_lt_0_05` of `tables/table_1_sequential_adjustment.csv`, which is not printed in Table 1.

## Numbers in the paper that are not computed here

These come from cited work, not from this analysis:

* the within-person Spearman correlation of 0.76 between the two trust item wordings (Perlis et al. 2024);
* the decline in high trust in physicians and hospitals of about 31 percentage points from April 2020 to January 2024 (Perlis et al. 2024);
* the roughly threefold decline in social distancing from April 2020 to May 2022, and the decomposition into a declining trend and a mortality-synchronized oscillation (Urmi et al. 2025);
* survey recruitment, quotas and weighting (Urmi et al. 2025, SI Appendix).

## Validation

* **Equivalence with the original scripts.** The code was condensed from the authors' original analysis scripts. On synthetic data with the same structure as the survey (`validation/simulate_chip50_waves.R`), the original scripts and this repository produce identical results for all 27 compared outputs (about 400,000 values); see `validation/README.md`.
* **Against the published results.** `output/tables/check_against_published.csv` compares 714 reproduced numbers with the paper. In the published run, 712 match, including every value of Table 1 and Table S1; the two exceptions are explained under [Notes on specific numbers](#notes-on-specific-numbers). Where the outputs of the original scripts are available (they are not distributed with this repository), `Rscript validation/compare_with_original_outputs.R` compares every output with them.

## Relation to the original analysis scripts

The scripts were condensed from the authors' working scripts, keeping every computation behind a reported result and dropping exploratory analyses, intermediate figures, caching and alternative specifications that the paper does not report.

| This repository | Original script(s) | What was dropped |
|---|---|---|
| `R/00_setup.R` | `script_helpers.R`, `00_config.R`, `02b_survey_design.R`, `unified_palettes.R` | Settings, labels and palettes of analyses not in the paper |
| `R/01_build_analysis_data.R` | `01_data_ingest_fix_UPDATED.R`, `02_Var_recode_UPDATED.R` | Recodes of variables the paper does not use; every script now reads one slim file instead of the raw waves |
| `R/02_descriptive_prevalence.R` | `mosaic_and_adherence_gap_plots.R`, `mosaic_trust_trajectories.R` | Outcomes and stratifiers not shown; drawing (moved to `07`) |
| `R/03_descriptive_inference.R` | `disparity_inference_analysis.R` (Approach A) | Approaches C–E (prevalence-difference CIs, spline and GAM models), formatted Word tables (`format_inference_tables.R`) |
| `R/04_adjusted_models.R` | `12_AdjustedTimeVarying_with_trust.R` | Exploratory figures, collinearity diagnostics (GVIF), caches and model objects |
| `R/05_sequential_models.R` | `14b_iterative_trust_model_comparison_v2.R` | Models and metrics not reported (trust added to each model, AUC, log loss, calibration, cross-validation, Tables 2–3), figures |
| `R/06_summary_statistics.R` | `17_reproduce_TableS1_LOBO_and_S18_stats.R`, `12c_sensitivity_analysis_comparison.R`, `12d_negative_control_assessment.R` | Summaries for trust in doctors and state government, exploratory figures; added the check of all reported numbers |
| `R/07_figures.R`, `R/figure_theme.R` | `pnas_figures/pnas_publication_figures.R`, `pnas_figures/pnas_figure_theme.R` | Nothing (input paths and file names changed; the time axis now extends when `MOSAIC_MATCH_TRUST_WAVES = FALSE`) |
| `R/08_fig_S21_model_schematic.R`, `python/fig_S20_dag_panel.py` | `pnas_figures/fig_S21_trust_stratified_SEIR_PNAS.R`, `pnas_figures/build_DAG_4_scenarios_panel_PNAS.py` | Nothing (paths only) |
| not needed | `13_production_plots.R` | Earlier versions of Figs 3–4 and S11–S16 |

## Run time

The published run took about 11 hours on a Windows laptop with the default `N_CORES`: script 04 about 10 hours, script 03 about 30 minutes, and every other step under 15 minutes. Setting `RUN_PAIRWISE_RD <- FALSE` shortens script 03. Scripts 06–08 need only the CSV files in `output/`, so the tables and figures can be re-created in minutes (see [Quick start](#quick-start)).

## Software

The published run used R 4.5.1 on Windows 11 with these CRAN packages (full list in `output/session_info.txt`):

| Package | Version | Used by |
|---|---|---|
| dplyr, tidyr, readr, tibble | 1.2.1, 1.3.1, 2.1.5, 3.3.0 | all scripts |
| survey | 4.4-8 | 00, 02–05 |
| haven | 2.5.5 | 01 (raw waves only), synthetic test |
| marginaleffects | 0.32.0 | 03, 04 |
| broom | 1.0.10 | 04 |
| ggplot2, patchwork, scales, forcats | 3.5.2, 1.3.1, 1.4.0, 1.0.0 | 07, 08 |
| ragg, systemfonts | 1.4.0, 1.2.3 | 07, 08 |

The code was also tested with R 4.3.3 (survey 4.2-1, marginaleffects 0.18.0, dplyr 1.1.4, ggplot2 3.4.4). It needs no other packages or files: the shared helpers are in `R/00_setup.R` and `R/figure_theme.R`. Figures use the Arial font where installed, otherwise Liberation Sans. Fig S20 uses Python 3 with matplotlib and Pillow (tested with matplotlib 3.11 and Pillow 12).

## Citation and contact

Please cite the paper above when using this code or data (GitHub's "Cite this repository" button gives the citation). Questions about the code: A. Zapf (azapf@g.harvard.edu); corresponding author of the paper: M. Santillana.
