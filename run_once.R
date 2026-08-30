
### ONE TIME SCRIPT - IDENTIFY ALL SCENARIOS FOR SELECTED TEAMS

rm(list = ls())
library(dplyr)
library(tidyr)

# load qualtrics data
qualtrics <- read.csv("qualtrics.csv")

# transform to 0/1
qualtrics <- qualtrics[,2:21] |>
  apply(c(1,2),function(z){ifelse(grepl("More than",z),1,0)}) |>
  bind_cols(name=qualtrics[,1]) # append name to end

# list all possible outcomes
scenarios <- expand.grid(rep(list(c(0, 1)), 20))

# calculate points for each bracket for each scenario
for (i in 1:nrow(qualtrics)) {
  scenarios$temp <-
    apply(sapply(1:5,function(z){2*as.numeric(scenarios[,z]==as.numeric(qualtrics[i,z]))}),1,sum)+
    apply(sapply(6:20,function(z){1*as.numeric(scenarios[,z]==as.numeric(qualtrics[i,z]))}),1,sum)
  colnames(scenarios) <- gsub("^temp$",paste0('person',i),colnames(scenarios))
}
scenarios$scenario <- 1:nrow(scenarios)

# identify winner(s) for each scenario
scenario_winners <- scenarios[,21:ncol(scenarios)] |>
  pivot_longer(cols=1:(ncol(scenarios)-21)) |>
  group_by(scenario) |>
  mutate(prob = value==max(value)) |>
  mutate(prob=prob/sum(prob)) |>
  ungroup() |>
  filter(prob>0) |>
  select(!c(value))

write.csv(scenarios, file = 'scenarios.csv', row.names = FALSE)
save(scenario_winners, file = 'scenarioWinners.Rdata')
write.csv(qualtrics, file = 'qualtrics_transformed.csv', row.names = FALSE)

