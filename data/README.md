# Data

The analyses use 23 waves (April 2020 to early 2023) of the CHIP50 survey (Civic Health and Institutions Project, formerly the COVID States Project), a nonprobability online survey of US adults with quotas by state for age, gender and race/ethnicity and post-stratification weights. Each wave is an independent cross-section. Survey design, recruitment and weighting are described in the SI Appendix of Urmi et al., PNAS 2025 (doi:10.1073/pnas.2500655122), which the paper's SI Text S2 refers to.

## Files in this folder

| File | Content |
|---|---|
| `chip50_trust_analysis.rds` | The analysis data set: one row per respondent (533,195 respondents in the 23 analysis waves; 458,017 in the 20 waves with trust items) and only the 36 variables the paper uses. It contains no identifiers, free text or geography below the Census region (state was used only to derive the region). This is the anonymized survey data named in the paper's data availability statement. Read it with `readRDS()`. |
| `codebook.csv` | Each variable with its description, type, levels or range, and number of missing values. |
| `sample_sizes_by_wave.csv` | Respondents per wave, field date (median start date), item nonresponse for the four behaviors and trust in the CDC, and respondents without a recorded doctor/hospital item. |

Rows are restricted to the 23 analysis waves and to respondents with a positive survey weight, as in all analyses of the paper. All three files are written by `R/01_build_analysis_data.R` from the raw CHIP50 wave files, which are not part of this repository.

## Running the code

`run_all.R` reads the included analysis file and skips step 1. To use an analysis file stored elsewhere, point the environment variable `C50_ANALYSIS_FILE` to it.

## Rebuilding the analysis file from the raw waves

1. Obtain the weighted CHIP50 wave files `CSP_W<wave>_weighted.RDS` for waves 1–27 (waves 4, 6, 8 and 15 are not used).
2. Tell R where they are, with the environment variable `C50_WEIGHTED_RDS_DIR`. Write the folder with forward slashes, also on Windows. Either set it for the current R session, before running the scripts:

   ```r
   Sys.setenv(C50_WEIGHTED_RDS_DIR = "C:/path/to/RDS_weighted")
   ```

   or set it once for all sessions in your user `.Renviron` file. Run `file.edit("~/.Renviron")` in R to open (or create) that file, add the line

   ```
   C50_WEIGHTED_RDS_DIR="C:/path/to/RDS_weighted"
   ```

   end it with a line break, save, and restart R. `Sys.getenv("C50_WEIGHTED_RDS_DIR")` should then print the folder. On Windows, R's `~` is your Documents folder (inside OneDrive if Documents is synced there), not `C:\Users\<you>`; a file saved elsewhere, or saved as `.Renviron.txt`, is not read.

3. Run `run_all.R` with `REBUILD_DATA <- TRUE` (or `Rscript R/01_build_analysis_data.R`). This overwrites the three files above.

## Variables

| Role | Variable(s) | Coding |
|---|---|---|
| Outcomes | `always_wear_mask`, `always_avoid_contact`, `always_avoid_crowds`, `wash_hands_strict` | 1 = followed the recommendation "very closely" in the past week (top of four categories), 0 = any other answer |
| | `vaccine_1` | 1 = received at least one COVID-19 vaccine dose (asked from wave 14) |
| | `visit_9` | Respondent or household member visited a doctor or hospital in the past 24 h, as recorded (adjusted models) |
| | `visit_doctor`, `visit_store`, `visit_work` | Past-24-h activities; an unselected or unasked item is coded 0 (descriptive analyses) |
| Exposures | `trust_cdc`, `trust_doc`, `trust_state`, `trust_banks` | No trust / Not much trust / Some trust / High trust ("not at all" to "a lot"); asked in waves 1–24 |
| | `trust` | Generalized trust as recorded (numeric), used in one sensitivity analysis |
| Stratifiers (descriptive) | `age_cat`, `gender`, `income_cat_5_fct`, `education_cat`, `race_fct`, `party3`, `ideology_fct`, `employ_fct`, `urban_type`, `region_fct` | As recorded or grouped (see `codebook.csv`) |
| Covariates (models) | `age_cat4`, `gender` / `gender_fct`, `race_cat4`, `education_hs_ref`, `income_high_ref`, `employ_fct`, `party_ind_ref` / `party_dem_ref`, `ideology_fct`, `ruca_cat_fct`, `region_fct` | Reference levels: 18–34; female (wave-level models) or male (sequential models); White; high-school graduate; $100,000 and over; full-time; Independent/Other (wave-level models) or Democrat (sequential models); moderate (wave-level models) or liberal (sequential models); urban; Northeast |
| Design | `wave`, `era`, `survey_date`, `weight_state` | Eras: pre-vaccine (waves 1–13), vaccine rollout (14–20), Omicron (21 onward) |

The reference level of a covariate changes only how its own coefficients are expressed, not the trust estimates or any model-fit statistic.
