# Dominance+

Builds pitcher and individual-pitch Dominance+ ratings using pitch-level metrics.

Uses an XGBoost model to predict xwOBA using HH%, Whiff%, IZW%, Chase%, CS%, Paint% (pitches in shadow zone). The predicted xwOBA is then scaled to 100 league average, giving one Dominance+ score to represent a pitcher's ability to force bad swing decisions and outputs from hitters.

Meant to evaluate a pitcher's ability in a way somewhere between Stuff+ and ERA+. I wanted a metric that would measure a pitcher's ability to force desirable outcomes from their opponents. 
I chose to train on xwOBA instead of run value (or even RV100) because RV gain can be impacted by pitch usage patterns (if only used in two strikes, could have potentially huge variance in values). xwOBA has similar problems because it only computes on plate appearance-ending pitches, but I thought its scale and stabilization made for a better independent variable.
The output, Dominance+, attempts to measure a pitcher’s ability to get poor swing decisions and outcomes from the hitter. I did this on a pitcher level by season (e.g. Josh Hader, 2025) and also on a pitcher’s pitch level (Justin Martinez, 2024 Splitter). The best pitchers/pitches made a lot of sense; many of them made a lot of sense, but there were also some surprises (which is perfect for a new stat, passing the eye test but also giving new information).


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