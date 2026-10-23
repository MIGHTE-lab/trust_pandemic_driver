# Validation

The scripts in `R/` are a condensed version of the authors' original analysis scripts. They keep the computations that produce the published figures, tables and numbers and drop exploratory analyses, intermediate figures and caching code. Three checks show that the results did not change: the first uses synthetic data; the other two need the survey data.

## 1. Same results as the original scripts (synthetic data)

`simulate_chip50_waves.R` writes synthetic survey waves with the file names, variable names and codings of the raw CHIP50 files (including survey missing-value codes, labelled variables, items missing in some waves and respondents without weights). Both the original scripts and this repository were run on the same synthetic waves (23 waves of 800 respondents) and their outputs compared with `compare_with_original_outputs.R`:

| Output (script) | Rows compared | Result |
|---|---|---|
| Prevalence by wave and group (02) | 5,872 | identical |
| Trust-level shares (02) | 320 | identical |
| Adherence gaps by wave (02) | 1,300 | identical |
| Omnibus tests by wave (03) | 1,750 | identical |
| Significance shares (03) | 77 | identical |
| Pairwise risk differences (03) | 10,822 | identical |
| Adjusted RDs by wave, four exposures (04) | 1,210 | identical |
| Adjusted ORs by wave, four exposures (04) | 36,740 | identical |
| Era-pooled ORs and RDs, four exposures (04) | 4,454 | identical |
| Primary vs generalized-trust RDs (06) | 110 | identical |
| Negative-control summary (06) | 6 | identical |
| Leave-one-block-out by wave (05) | 400 | identical |
| Table 1 (05) | 20 | identical |
| Table S1 (06 vs the original script 17) | 60 | identical |

"Identical" means that every compared value agreed exactly (the largest absolute difference was 0). Two adaptations were needed for the original scripts to run in the test environment (R 4.3.3, marginaleffects 0.18.0): an empty stand-in for the unused package `rchip50`, which `00_config.R` loads, and `newdata = sub` in the pairwise-RD call of `disparity_inference_analysis.R`, without which that marginaleffects version cannot find the weight column.

The comparison also brought out one detail that the paper does not describe and that the condensed code keeps by default: the risk differences of Table 1 average the predictions of the survey-weighted models with equal weight per respondent (see `TABLE1_RD_AVERAGE` in the main README).

The whole pipeline (`run_all.R`) was also run end to end on the same synthetic waves, from a folder whose path contains spaces and with two parallel workers: all 33 CSV outputs were identical to those of the sequential run above, and all figures were drawn.

To run the pipeline on synthetic data yourself (no survey data needed):

```
Rscript validation/run_synthetic_test.R
```

## 2. Same results as the original scripts (survey data)

After a full run on the survey data, compare the new outputs with those of the original scripts:

```
Rscript validation/compare_with_original_outputs.R
```

This check needs the outputs of the original scripts, which are not distributed with the repository. By default the script looks for them in `../output` and `../../output/disparity_inference`, relative to the repository root (the layout of the authors' project folder); set `C50_ORIGINAL_OUTPUT_DIR` and `C50_ORIGINAL_DISPARITY_DIR` otherwise. It writes `output/validation/comparison_summary.csv`, with one row per file and its status, and `comparison_details.csv`, with every value outside tolerance:

* **MATCH:** all values agree within tolerance and no rows are missing on either side;
* **REVIEW:** some values or rows differ (see `comparison_details.csv`);
* **SKIPPED:** the new or the original file was not found;
* **ERROR:** expected columns are missing.

Numeric values match if they differ by at most 1e-6 times the original value (1e-6 in absolute terms for values below 1). For the prevalence estimates the tolerance is 0.01 percentage points, because the authors' table of these estimates was recovered from the vector graphics of the figures and is rounded to 0.001 pp.

Differences can be expected when the original outputs were produced with other package versions (for example of `marginaleffects`, which computes the risk differences) or before the final version of a script; on identical inputs and software the results are identical (section 1).

## 3. Same numbers as the paper

`R/06_summary_statistics.R` compares 714 reproduced numbers (Table 1, Table S1, the Fig S18 legend and the numbers in the Results) with `reference/published_values.csv` and writes `output/tables/check_against_published.csv`.

In the published run on the survey data (R 4.5.1), 712 of the 714 numbers match, including all 224 values of Table 1 and all 384 values of Table S1. The two exceptions are statements in the text that the data do not support exactly ("in every wave" holds in 19 of 20 waves; "6 to 10 pp" is 5.9 to 9.4 pp); the main README explains both.
