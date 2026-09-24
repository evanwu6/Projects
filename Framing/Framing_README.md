# Framing

Estimates catcher framing value on pitches near the strike-zone boundary and explores whether hitters change their swing behavior with different catchers.

## Inputs
- Pitch-level savant data.
- `stats.csv`: hitter expected-wOBA data used in a comparison plot.

## Workflow
`Data and Model.R` fills missing zone boundaries using batter-season averages, selects a band around the strike-zone edge, and models the probability of a called strike on taken pitches. It trains separate XGBoost logistic models for 2024 and 2025.

Catcher value is calculated as actual strikes minus predicted strikes, summed by catcher-season. `SG100` is strikes gained per 100 qualifying taken pitches.

`Graphs.R` builds catcher rankings and compares swing rates, pitch locations, and hitter outcomes across catchers classified as positive/negative framers.

## Outputs
- `edge_data.csv`: selected edge pitches.
- `model_data.csv`: features, labels, catcher/season identifiers, and training flags.
- `strike24.model`, `strike25.model`: saved classifiers.
- `catcher_sv.csv`: catcher-season counts, strikes gained, and SG100.
- Interactive plots/tables, plus existing exported figures in `Graphics/`.

## Usage and requirements
Use R with `tidyverse`, `lubridate`, `caret`, and `xgboost`; graph code also uses `knitr::kable` and `scales` functionality. Resolve the issues below before treating this as a reproducible batch pipeline. Preparation/modeling precedes graphing. Tuning can be lengthy.

## Implementation issues to review
Training feature selection retains `game_year`, while graph-time feature selection removes it, creating a potential model feature mismatch. Both scripts also combine year-specific prediction vectors with a full-data `ifelse()`, which can recycle vectors and misalign predictions; predict into each matching season's rows instead. Recheck all derived framing totals after fixing these issues.

The metric is conditional on this edge-pitch sample and model specification.