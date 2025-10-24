This is a coding repository for projects that I have completed. Each folder is a different project, which were done for Elon University's baseball team, Elon's Baseball Analytics Club, the SABR Diamond Dollars Case Competition, the Carnegie Mellon Sports Analytics Camp, or my baseball blog.


OVERVIEW OF PROJECTS:


CMSAC Pitching Evaluation Project:

The final project deliverable for the Summer Undergraduate Research Experience at CMSAC. I worked with two other students, Ethan Park and Priyanka Kaul, as well as our research mentor, Sean Ahmed. We used pitch-by-pitch data from baseballsavant to create random forest and generalized additive models in R. The purpose of the project was to determine which pitching statistics and metrics were most predictive of success.



Comp App:

This comparison app was meant to be used for "Player A vs. Player B" questions. Lots of job questions and thought exercises give player archetypes and statistics and ask you to determine which you would prefer. This app uses random forest models to predict output based on the input values (meant to highlight the interaction between variables as well). It also can intake a variety of advanced statistics and provides comparable performances from recent seasons.



Diamond Dollars 2024:

Code and project for the SABR Diamond Dollars Competition in Phoenix, AZ. An R shiny app and the corresponding code that values foul balls based on pitch type/speed and pitcher/batter handedness.



Dominance+:

Code and project for my Dominance+ statistic, which measures a pitcher's ability to induce swing and miss and limit hard contact. Dominance+ uses an XGBoost model and multiple pitching metrics (whiff rate, hard hit rate, chase rate, in-zone whiff rate, in-zone take rate, and strike zone edge rate) to predict xwOBA, which is then converted to a 100 scale plus stat (where 100 is league average). This is done on a pitcher and pitch level using different models.

Blog Post: https://medium.com/@Evanwu6/who-are-the-most-dominant-pitchers-in-baseball-my-new-statistic-dominance-138d1c9791a5



Expected Pitch Project:
Using their swing data, I project the hitter's predicted/perceived pitch location using a GAM and use that perceived location to determine which pitchers are the most difficult to predict.

Blog Post: https://medium.com/@Evanwu6/what-were-they-swinging-at-using-swing-data-to-see-through-a-hitters-eyes-4dff7a79f9a8

Blog Post 2: https://medium.com/@Evanwu6/how-do-batter-expectations-shift-based-on-pitch-usage-366b1e1f12e3



Elon Baseball Pitching Reports:

Post-game pitching reports for the players and coaches of Elon University's D1 Baseball team. Uses R to output pdfs with pitch movement and metric plots and a variety of performance statistics including hard hit balls and xwOBA.



Elon Baseball Hitter Reports:

Seasonal hitting reports reports for the players and coaches of Elon University's D1 Baseball team. Uses R to output pdfs with spray charts, heat maps, and swing decisions evaluations for hitters (based on pitcher handedness and pitch type).



Elon Baseball Training Camp Reports:

Pitching reports for Elon Baseball's training camps for younger players. Uses R to create pdfs with pitch movement and metric information, as well as display each pitch's data.



Framing:

Code and project for my blog about hitter swing decisions dependent on the catcher. The code has a catcher framing model, as well as hitter swing evaluations based upon the ability of the catcher. I use an XGBoost model using pitch characteristics to predict strike likelihoods.

Blog Post: https://medium.com/@Evanwu6/do-hitters-care-who-is-catching-a520831b5cb4



Hunter Brown:

Code and project for my blog about Hunter Brown. Focused around his addition of a Sinker mid way through the 2024 season, the code analyzes his location and quality of contact results and looks at how his Sinker improves his entire repertoire. 

Blog Post: https://medium.com/@Evanwu6/the-emergence-of-hunter-brown-71f60dc66551



Umpire Impact:

Code and project for my blog about umpire evaluation. Instead of merely using accuracy, I project the impact of each potential call and determine how much impact the umpire's extraneous calls had on the game (using run and win probability added, with 0 being perfect). I use an XGBoost model to predict the impact of a strike and ball in a given situation and juxtapose the call with the "correct" call to determine the umpire's impact on the game.

Blog Post: https://medium.com/@Evanwu6/how-do-batter-expectations-shift-based-on-pitch-usage-366b1e1f12e3



xwOBA Model:

xwOBA model in R using all available Division 1 Trackman data. Computes probability of each outcome for a combination of exit velocity and launch angle using comparable batted balls (within 2 degrees LA and 2 mph EV). Probabilities of each outcome are then used as weighting coefficients to create xwOBA (along with the wOBA coefficients for each outcome). The output and xwOBA "model" is a csv with every observed combination of exit velocity and launch angle and the corresponding xwOBA value. To apply the model, a batted ball or end to an at bat is given its corresponding xwOBA value, which is then combined over a game or season.
