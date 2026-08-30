library(dplyr) 
library(ggplot2)
library(plotly)
library(shiny)
library(DT)

con <- url("https://raw.githubusercontent.com/jbccre/football/main/github.Rdata")
load(con)
close(con)

historical_plot <- ggplot(historicalprobs, aes(x = day, y = prob, color=bracketname, group=bracketname,text = label)) +
  geom_line() +
  scale_y_continuous(labels=scales::percent) 

teams_table <-   datatable(teams_table,
  colnames = c("Team", "Games Won", "Games Lost", "Cutoff",  'Probability of Over', 'Probability of Under', "Points"),
  rownames = FALSE, escape = FALSE, options = list(paging=FALSE,searching=FALSE,info=FALSE,scrollY='300px', scrollX= TRUE, pageLength=10)) |>
  formatStyle(columns=2:7,textAlign='right')

players_table <- datatable(players_table,
    colnames=c("Bracket","Rank","Probability","Expected Value", team_names),
    rownames = FALSE, options = list(paging=FALSE,searching=FALSE,info=FALSE,scrollX=TRUE,scrollY='300px')) |>
  formatStyle(
    columns = 2:29,
    textAlign = 'right',
    backgroundColor = styleEqual(c(">", "<"), c("lightgreen", "#ff9999"))
  )

ui <- fluidPage(
  titlePanel("Football Madness 2026"),
  p('A product of the University of Memphis, Center for Community Research and Evaluation. Click ',tags$a(href="https://github.com/jbccre/football",target="_blank","here"),"for code."),
  hr(),
  h3("Probability Over Time"),
  p("View each player's probability of winning Football Madness over the course of the tournament."),
  plotlyOutput("historical_plot"),
  hr(),
  h3("Standings & Selections"),
  p("View the current standings as well as the selections that each player made."),
  DTOutput("players_table"),
  hr(),
  h3("The Sports"),
  p("View the current status of each team, as well as which teams have already gone over/under."),
  DTOutput("teams_table")
)

server <- function(input,output,session) {
  output$historical_plot <- renderPlotly({ggplotly(historical_plot, tooltip = c('text'))})
  output$players_table <- renderDT({players_table})
  output$teams_table <- renderDT({teams_table})
}

shinyApp(ui, server)
