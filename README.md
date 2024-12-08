This is a coding repository for projects that I have completed. Each folder is a different project, which were done for Elon University's baseball team, Elon's Baseball Analytics Club, the SABR Diamond Dollars Case Competition, or the Carnegie Mellon Sports Analytics Camp.


OVERVIEW OF PROJECTS:


CMSAC Pitching Evaluation Project:

The final project deliverable for the Summer Undergraduate Research Experience at CMSAC. I worked with two other students, Ethan Park and Priyanka Kaul, as well as our research mentor, Sean Ahmed. We used pitch-by-pitch data from baseballsavant to create random forest and generalized additive models in R. The purpose of the project was to determine which pitching statistics and metrics were most predictive of success.


Comp App:

This comparison app was meant to be used for "Player A vs. Player B" questions. Lots of job questions and thought exercises give player archetypes and statistics and ask you to determine which you would prefer. This app uses random forest models to predict output based on the input values (meant to highlight the interaction between variables as well). It also can intake a variety of advanced statistics and provides comparable performances from recent seasons.


Diamond Dollars 2024:

Code and project for the SABR Diamond Dollars Competition in Phoenix, AZ. An R shiny app and the corresponding code that values foul balls based on pitch type/speed and pitcher/batter handedness.


Elon Baseball Pitching Reports:

Post-game pitching reports for the players and coaches of Elon University's D1 Baseball team. Uses R to output pdfs with pitch movement and metric plots and a variety of performance statistics including hard hit balls and xwOBA.


Elon Baseball Training Camp Reports:

Pitching reports for Elon Baseball's training camps for younger players. Uses R to create pdfs with pitch movement and metric information, as well as display each pitch's data.


xwOBA Model:

xwOBA model in R using all available Division 1 Trackman data. Computes probability of each outcome for a combination of exit velocity and launch angle using comparable batted balls (within 2 degrees LA and 2 mph EV). Probabilities of each outcome are then used as weighting coefficients to create xwOBA (along with the wOBA coefficients for each outcome). The output and xwOBA "model" is a csv with every observed combination of exit velocity and launch angle and the corresponding xwOBA value. To apply the model, a batted ball or end to an at bat is given its corresponding xwOBA value, which is then combined over a game or season.
