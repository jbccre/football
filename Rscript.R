devtools::install_github("sportsdataverse/cfbfastR")
library(cfbfastR)
library(dplyr) 
library(poibin) # poisson normal distribution
library(tidyr) 
library(ggplot2)
library(DT)

load("scenarioWinners.Rdata")
qualtrics <- read.csv('qualtrics.csv')
qualtrics_transformed <- read.csv('qualtrics_transformed.csv')
Sys.getenv("CFBD_API_KEY")
current_date <- as.Date(gsub(" .*","",format(Sys.time(), tz = "America/Chicago", usetz=TRUE)))
team_ids <- c(145,344,66,235,99,2483,251,248,21,154,57,142,194,201,228,2633,333,2579,2294,77) # relevant teams in Qualtrics order
schedule <- load_cfb_schedules(2026)
all_teams <- 
schedule |> filter(season_type=='regular') |>
  select(season_type, start_date, id=home_id, team=home_team, elo=home_pregame_elo) |>
  mutate(type = 'home') |>
  bind_rows({schedule |> mutate(type='away') |> select(start_date, id = away_id, team = away_team, elo = away_pregame_elo, type)}) |>
  filter(!is.na(elo)) |>
  arrange(desc(start_date)) |>
  filter(!duplicated(team)) |>
  mutate(elo=case_when(type=='home' ~ elo - 50, .default = elo))  |>
  select(!c(start_date,season_type,type))

games <- schedule |>
  filter(home_id %in% team_ids | away_id %in% team_ids) |>
  filter(season_type == 'regular') |>
  select(game_id, start_date, completed, home_id, home_team, home_points, home_pregame_elo, away_id, away_team, away_points, away_pregame_elo) |>
  # grab elo rating
  left_join({all_teams |> select(home_id=id,home_elo=elo)}) |>
  left_join({all_teams |> select(away_id=id,away_elo=elo)}) |> 
  # fbs teams - impute elo of 1300
  mutate(home_elo = case_when(is.na(home_elo) ~ 1300, .default = home_elo)) |>
  mutate(away_elo = case_when(is.na(away_elo) ~ 1300, .default = away_elo)) |>
  # add home team advantage
  mutate(home_elo = home_elo + 50) |>
  # if pregame elo known, use that, otherwise use generic team elo
  mutate(home_elo = case_when(!is.na(home_pregame_elo) ~ home_pregame_elo, .default = home_elo)) |>
  mutate(away_elo = case_when(!is.na(away_pregame_elo) ~ away_pregame_elo, .default = away_elo)) |>
  # set probabilities
  mutate(home_prob = 1/(1 + 10^((away_elo-home_elo)/400))) |>
  mutate(away_prob = 1-home_prob) |>
  # configure dates
  mutate(start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")) |>
  mutate(start_date = format(start_date, tz = "America/Chicago", usetz = TRUE)) |>
  mutate(day = as.Date(gsub(" .*","",start_date)))

# get days
days <- games |>
  select(day) |>
  bind_rows(data.frame(day=current_date)) |>
  filter(day<=current_date) |>
  arrange(day)

# probability of each team exceeding cutoff each day
output <- all_teams |>
  filter(id %in% team_ids) |>
  arrange(match(id,team_ids)) |>
  mutate(points = c(rep(2,times=5),rep(1,times=15))) |>
  mutate(cutoff = c(7.5,4.5,5.5,7.5,8.5,10.5,9.5,7.5,7.5,5.5,7.5,6.5,9.5,7.5,7.5,7.5,8.5,6.5,7.5,5.5)) |>
  cross_join(days)

get_probs <- function(the_day = '2026-08-30', id = 235) {
  {games |> 
      filter(home_id==id | away_id==id) |>
      mutate(prob = ifelse(home_id==id, home_prob, away_prob)) |>
      mutate(winner = case_when(
        !completed ~ NA,
        home_id==id & home_points>away_points ~ 1,
        away_id==id & away_points>home_points ~ 1,
        .default = 0)) |>
      mutate(prob = case_when(
        (day >= the_day) ~ prob,
        !completed ~ prob,
        .default = winner)) |>
      mutate(the_day = the_day) |>
      select(prob) |>
      as.vector()}$prob
}

output$probs <- sapply(1:nrow(output), function(z){
  get_probs(the_day=output$day[z],id=output$id[z])
})

output$prob <- sapply(1:nrow(output), function(z){1 - ppoibin(kk = output$cutoff[z], pp = output$probs[z][[1]])})

# all scenarios
historicalprobs <- do.call(bind_rows, lapply(days$day,function(z){
  probs <- output$prob[output$day==z]
  scenarios <- expand.grid(rep(list(c(-1, 1)), 20))
  probmatrix <- scenarios
  for (i in 1:length(probs)) {probmatrix[,i] <- probmatrix[,i]*probs[i]}
  for (i in 1:length(probs)) {probmatrix[,i] <- probmatrix[,i] + ifelse(probmatrix[,i]<0,1,0)}
  scenario_probs <- apply(probmatrix,1,prod)
  scenario_winners$scenarioprob <- scenario_probs[scenario_winners$scenario]
  scenario_winners$totalprob <- scenario_winners$prob * scenario_winners$scenarioprob
  player_probs <- scenario_winners |> group_by(name) |> summarise(prob = sum(totalprob)) |> arrange(desc(prob))
  qualtrics |> select(bracketname = Q31) |> mutate(name = paste0("person",1:25)) |> left_join(player_probs) |> arrange(desc(prob)) |> mutate(day = z)
})) |>
  mutate(label = paste0(bracketname,": ",sprintf("%.2f",100*prob),"%"))

# team names
team_names<-paste0(all_teams$team[match(team_ids, all_teams$id)]," (",c(rep(2,times=5),rep(1,times=15)),"pt)")

winners_losers <- games |>
  mutate(winner = case_when(
    !completed ~ NA,
    home_points>away_points~home_team,
    away_points>home_points~away_team,
    .default=NA
  )) |>
  mutate(loser = case_when(
    !completed ~ NA,
    home_points<away_points~home_team,
    away_points>home_points~away_team,
    .default=NA
  )) |>
  filter(!is.na(winner)) |>
  select(winner,loser) |>
  pivot_longer(cols=1:2) |>
  group_by(value) |>
  summarise(won=sum(name=="winner"),lost=sum(name=="loser"))

historical_plot <- ggplot(historicalprobs, aes(x = day, y = prob, color=bracketname, group=bracketname,text = label)) +
  geom_line() +
  scale_y_continuous(labels=scales::percent) 

teams_table <- left_join(tibble(value=all_teams$team[match(team_ids, all_teams$id)]),winners_losers) |>
  mutate(won = ifelse(is.na(won),0,won)) |>
  mutate(lost = ifelse(is.na(lost),0,lost)) |>
  mutate(cutoff = c(7.5,4.5,5.5,7.5,8.5,10.5,9.5,7.5,7.5,5.5,7.5,6.5,9.5,7.5,7.5,7.5,8.5,6.5,7.5,5.5)) |>
  mutate(probability_over = paste0(sprintf("%.2f",100*output$prob[output$day==max(output$day)]),"%")) |>
  mutate(probability_under = paste0(sprintf("%.2f",100*(1-output$prob[output$day==max(output$day)])),"%")) |>
  mutate(points = c(rep(2,times=5),rep(1,times=15))) |>
  datatable(
    colnames = c("Team", "Games Won", "Games Lost", "Cutoff",  'Probability of Over', 'Probability of Under', "Points"),
    rownames = FALSE, escape = FALSE, options = list(paging=FALSE,searching=FALSE,info=FALSE,scrollY='300px', scrollX= TRUE, pageLength=10)
  ) |>
  formatStyle(columns=2:7,textAlign='right')

players_table <- qualtrics_transformed |>
  relocate(name, .before=1) |>
  mutate(across(-name, ~ifelse(.x==1,">",ifelse(.x==0,"<",.x)))) |>
  left_join({historicalprobs |> filter(day==max(day)) |> arrange(desc(prob)) |> mutate(rank = match(prob, prob)) |> mutate(label=gsub(".*:","",label)) |>  mutate(expected_value = prob*5*25) |> select(name=bracketname, rank, prob=label, expected_value)}) |>
  arrange(rank) |>
  relocate(c(name,rank,prob,expected_value),.before=1) |>
  mutate(expected_value = paste0("$",sprintf("%.2f",expected_value))) |>
  datatable(
    colnames=c("Bracket","Rank","Probability","Expected Value", team_names),
    rownames = FALSE, options = list(paging=FALSE,searching=FALSE,info=FALSE,scrollX=TRUE,scrollY='300px')
  ) |>
  formatStyle(
    columns = 2:29,
    textAlign = 'right',
    backgroundColor = styleEqual(c(">", "<"), c("lightgreen", "#ff9999"))
  )

save(players_table, teams_table, historical_plot, file = 'github.Rdata')
