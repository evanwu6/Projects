# Dominance+

Builds pitcher and individual-pitch Dominance+ ratings Using pitch-level metrics.

Uses an XGBoost model to predict xwOBA using HH%, Whiff%, IZW%, Chase%, CS%, Paint% (pitches in shadow zone). The predicted xwOBA is then scaled to 100 league average, giving one Dominance+ score to represent a pitcher's ability to force bad swing decisions and outputs from hitters.

## Inputs
- Data: Pitch-level savant data 2023-2025
- Intermediate summaries: `Data/pitch_stats.csv` and `Data/pitcher_stats.csv`.
- `fangraphs_pitching.csv`: comparison statistics used in the final tables.
- Existing models: `dominance_pitch.model` and `dominance_pitcher.model`.

The raw data must contain pitch outcomes, zone/location, batter/pitcher identifiers, batted-ball quality, season, and expected wOBA. The model features are whiff rate, in-zone whiff rate, chase rate, hard-hit rate, paint rate, and in-zone take rate.

## Workflow
1. `Data.R` combines source seasons and creates pitcher-season and pitcher-season-pitch summaries.
2. `Pitch Model.R` and `Pitcher Model.R` train and save separate xwOBA models.
3. `Dominance+.R` generates predictions, computes relative ratings, merges comparison statistics, and creates exploratory plots.
4. `App/App.R` displays pitcher and pitch leaderboards with filters and an optional traditional-statistics view.

Pitch ratings require at least 100 pitches; pitcher ratings require at least 500. Pitch-level Dominance+ uses pitch-class/season baselines; `gDominance+` uses the season baseline. Additional normalization makes the retained rating tables average 100.

## Outputs
- Model files and summary CSVs.
- Root-level `pitch_dominance.csv` and `pitcher_dominance.csv`.
- Feature-importance PNGs and analytical figures; existing figures are in `Graphs/`.
- An interactive Shiny app. It reads the copies of the two leaderboard CSVs in `App/`; those copies must be refreshed when results change.

## Usage and requirements

R packages: `tidyverse`, `lubridate`, `caret`, `xgboost`, `scales`, `shiny`, `DT`, and `rsconnect`.