**Football Madness!**

Dashboard available at: https://ccre.shinyapps.io/football/

Dashboard is updated "on demand"; we may convert this to automatic every few hours on Saturdays.

**Files:**
- run_once.R: code once run to identify all 1,048,576 (2^20) outcomes for our 20 games and who wins each, as well as process the submissions
- scenarioWinners.Rdata, qualtrics.csv, qualtrics_transformed.csv: output from run_once.R
- Rscript.R: run on demand to run the probabilities. runs the current probabilities as well as (re)calculates prior probabilities for previous days. this uses the API available at: https://collegefootballdata.com/. This API identifies game outcomes as well as ELO ratings indicating the strength of each team which is used to calculate probabilities.
- github.Rdata: the output of Rscript.R, used for the dashboard
- app.R: the dashboard code

**Methods:**
- The probability of a team exceeding the cutoff ("over" 7.5, for example) is calculated using a Poisson binomial distribution, where each unknown game probability is calculated based on ELO ratings available in the above named API. If you do not know what an ELO rating is, go here: https://en.wikipedia.org/wiki/Elo_rating_system. Known game probabilities are, of course, 1 if won and 0 if lost.
- The pregame ELO is used when available. If imputation is necessary, the following rules are used:
  - Sometimes, FCS teams play FBS teams where no ELO is reported. We assume an ELO of 1300 for those teams per standard practice (FiveThirtyEight).
  - Sometimes, future ELOs are unknown. The most recent ELO for the team is used. If the most recent ELO is a home game, 50 points are subtracted from the historical ELO. Then, 50 points are added for home team advantage if the team is playing home in the contest of interest.
- Then, the probability of each player winning is calculated by identifying the 2^20 possible scenarios, identifying the winner of the tournament under each scenario, assigning a probability on each scenario, and summing the scenario probabilities for each player across the scenarios (s)he wins. Ties are assumed to split the win probability of the scenario equally.
- The probability of each scenario occurring is calculated by multiplying the team-specific probabilities as independent events, ignoring that some teams will play each other. Therefore, probabilities may have more error towards the end of the tournament.
