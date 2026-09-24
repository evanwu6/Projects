# Umpire Impact

Measures ball/strike-call accuracy and estimates the run- and win-expectancy consequences of incorrect calls using counterfactual XGBoost predictions.

## Inputs
- Statcast pitch-level data.
- Supplied prepared data/model files permit later stages without rescraping or retraining.

## Workflow
1. `Data.R` classifies calls and labels zone IDs 1–9 as true strikes; writes `umpire_data.csv`.
2. `Umpire Data Scraping.R` retrieves home-plate officials, saves `ump_names.csv`, and joins umpire names into `umpire_data2.csv`.
3. `RE Model Creation.R` / `WP Model Creation.R` fit regressors for the corresponding target from inning, top/bottom, count, outs, base occupancy, fielding lead, and called-strike indicator.
4. `Modeling Implementation.R` selects taken balls/called strikes, identifies incorrect calls, substitutes the true strike/ball indicator, and predicts counterfactual outcomes.
5. `Graphics.R` produces rankings and selected best/worst-game location plots.

## Outputs
- Prepared CSVs and lookup listed above.
- `bsre.model`, `bswp.model`.
- `full_pitch_data.csv`: incorrect-call context, observed and counterfactual values, and umpire.
- `full_ump_data.csv`: call counts, correct/incorrect totals, accuracy, total absolute impact, impact per incorrect call, and impact per 100 calls.
- Existing charts in `Graphs/`; some graph sections require objects from earlier scripts in the same session.

## Usage
Use R with `tidyverse`, `lubridate`, `Metrics`, `xgboost`, `caret`, `jsonlite`, `ggforce`, and `patchwork`. Work from this folder, extract the raw CSV if rebuilding, and run stages in order. API scraping requires network access. Training overwrites saved model files; implementation overwrites result CSVs.
