library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)
source("src/nba_functions.R")

# read in data
gl <- read_csv("data/nba_gamelogs.csv")
agg <- read_csv("data/nba_agg_data.csv")
szn <- unique(agg$Season)

# group by and summarize data by player, position and week
summarized_gl <- gl %>%
  group_by(Player, General_Position, Week) %>%
  summarise(
    Games = n() - sum(DNP),
    FG = sum(FG),
    FGA = sum(FGA),
    `FG%` = (FG / FGA),
    FT = sum(FT),
    FTA = sum(FTA),
    `FT%` = (FT / FTA),
    `3P` = sum(`3P`),
    PTS = sum(PTS),
    PTS = sum(PTS),
    REB = sum(REB),
    AST = sum(AST),
    STL = sum(STL),
    BLK = sum(BLK),
    TOV = sum(TOV)
  ) %>%
  mutate(
    `FG%` = replace_na(`FG%`, 0),
    `FT%` = replace_na(`FT%`, 0)
  )


ui <- navbarPage(
  theme = shinytheme("united"),
  "NBA Fantasy",
  # FIRST PANEL: PLAYER PROFILE
  tabPanel(
    "Player Profile",
    sidebarLayout(
      sidebarPanel(tags$style(".well {background-color:#fe5a1d;}"),

        selectInput(inputId = "player_input", label = "Player:", choices = agg$Player, selected = "LeBron James"),

        selectInput("category_input", "Category:", choices = list(
          "FG%" = "FG%", "FT%" = "FT%",
          "3P" = "3P", "PTS" = "PTS",
          "REB" = "REB", "AST" = "AST",
          "STL" = "STL", "BLK" = "BLK",
          "TOV" = "TOV"
        ), selected = "FG%"),
        sliderInput("min_games_input", "Minimum Games Played:", min = 0, max = max(agg$Games_Played), value = 10, step = 1),
        sliderInput("min_minutes_input", "Minutes per Game:", min = 0, max = 48, value = c(0, 48), step = 1),
        checkboxGroupInput("pos_compare", "Positions to Compare:",
          choices = c("G" = "G", "F" = "F", "C" = "C"),
          selected = c("G", "F", "C")
        ),
        width = 2
      ),
      mainPanel(
        titlePanel(h1(textOutput("selected_player"), align = "center", style = "color: #fe5a1d;font-size:70px")),
        h4(textOutput("selected_player_pos"), align = "center", style = "font-size:30px"),
        h4(textOutput("selected_player_team"), align = "center", style = "font-size:30px"),
        div(tableOutput("player_table"), style = "font-size:125%"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            h3("League Comparison"),
            plotlyOutput("league_comp", width = "auto", height = "auto")
          ),
          tabPanel(
            h3("Distribution"),
            plotlyOutput("stat_dist", width = "auto", height = "auto")
          ),
          tabPanel(h3("Week Summary"), div(dataTableOutput("week_summary"), style = "font-size:115%"), width = "auto"),
          tabPanel(h3("Game Logs"), div(dataTableOutput("gamelog"), style = "font-size:115%"), width = "auto")
        )
      )
    )
  ),
  # SECOND PANEL: TEAM BUILDER
  tabPanel(
    "Team Builder",
    titlePanel(h1(paste("Fantasy Team Builder", szn), align = "center", style = "color: #fe5a1d;font-size:70px")),
    h2("Enter up to 13 players to see how your team would perform, based on 2020-2021 stats", align = "center", style = "font-size:20px"),
    sidebarLayout(
      sidebarPanel(tags$style(".well {background-color:#fe5a1d;}"),
        selectizeInput("team_player_input", "Players:",
          choices = agg$Player, multiple = TRUE,
          options = list(maxItems = 13)
        ),
        selectInput("week_input", "Week:", choices = unique(summarized_gl$Week), multiple = FALSE, selected = 1),
        width = 3
      ), mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel(
          h3("Season Stats"),
          titlePanel(h2("Average Stats per Week", align = "center")),
          div(dataTableOutput("team_stat_avg", width = "auto"), style = "font-size:115%"),
          titlePanel(h2("Stats per Week", align = "center")),
          div(dataTableOutput("team_week_stats", width = "auto"), style = "font-size:115%")
        ),
        tabPanel(
          h3("Week Breakdown"),
          div(dataTableOutput("week_bkdwn", width = "auto"), style = "font-size:115%")
        )
      ))
    )
  ),
  # THIRD PANEL: PLAYER STATS
  tabPanel(
    "Player Stats ",
    titlePanel(h1(paste("Player Stats", szn, "Regular Season"), align = "left", style = "color: #fe5a1d;font-size:60px")),
    fluidRow(
      column(3, sliderInput("min_games_stats", "Minimum Games Played:", min = 0, max = max(agg$Games_Played), value = 10, step = 1)),
      column(4,
        offset = 1,
        sliderInput("min_minutes_stats", "Minutes per Game:", min = 0, max = 48, value = c(0, 48), step = 1)
      ),
      column(4, checkboxGroupInput("pos_stats", "Positions to Compare:",
        choices = c("G" = "G", "F" = "F", "C" = "C"),
        selected = c("G", "F", "C")
      )),
    ), mainPanel(
      fluidRow(div(dataTableOutput("league_stats", width = "auto"), style = "font-size:105%"))
    )
  )
)

server <- function(input, output) {
  output$league_comp <- renderPlotly(
    plot_stat_league(agg, input$player_input, input$category_input,
      input$pos_compare,
      min_games = input$min_games_input,
      min_minutes = input$min_minutes_input
    )
  )
  output$stat_dist <- renderPlotly(
    plot_stats_ind(gl, input$player_input, input$category_input)
  )
  output$player_table <- renderTable(
    agg %>%
      filter(Player == input$player_input) %>%
      select(-c(Player, General_Position, Position, Season)) %>%
      rename(Games = Games_Played, `Games Started` = Games_Started, `Games Missed` = Games_Missed) %>%
      mutate(
        Games = as.integer(Games),
        `Games Started` = as.integer(`Games Started`),
        `Games Missed` = as.integer(`Games Missed`)
      )
  )

  output$gamelog <- renderDataTable(
    gl %>%
      filter(Player == input$player_input) %>%
      select(-c(Player, Team, Position, General_Position, Season)) %>%
      mutate(
        Minutes = round(Minutes, 2),
        `FG%` = round(`FG%`, 2),
        `FT%` = round(`FT%`, 2),
        `3P%` = round(`3P%`, 2)
      ),
    rownames = FALSE,
    options = list(columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")))
  )

  output$selected_player <- renderText(input$player_input)
  output$selected_player_pos <- renderText(paste(
    "Position:",
    agg %>%
      filter(Player == input$player_input) %>%
      select(Position) %>%
      pull()
  ))

  output$selected_player_team <- renderText(
    paste("Team:", gl %>%
      filter(Player == input$player_input) %>%
      select(Team) %>%
      tail(1) %>%
      pull())
  )

  output$week_summary <- renderDataTable(
    summarized_gl %>%
      ungroup() %>%
      filter(Player == input$player_input) %>%
      select(-c(Player, General_Position)) %>%
      mutate_at(vars(c(`FG%`, `FT%`)), round, 3),
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")))
  )

  output$league_stats <- renderDataTable(
    agg %>%
      filter(Games_Played >= input$min_games_stats &
        General_Position %in% input$pos_stats &
        Minutes >= input$min_minutes_stats[1] &
        Minutes <= input$min_minutes_stats[2]) %>%
      rename(Games = Games_Played, `Games Started` = Games_Started, `Games Missed` = Games_Missed) %>%
      mutate(
        Games = as.integer(Games),
        `Games Started` = as.integer(`Games Started`),
        `Games Missed` = as.integer(`Games Missed`)
      ) %>%
      arrange(desc(PTS)) %>%
      select(-c(General_Position, Season)),
    rownames = FALSE,
    options = list(pageLength = 10, columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")))
  )

  output$team_week_stats <- renderDataTable(
    summarized_gl %>%
      filter(Player %in% input$team_player_input) %>%
      group_by(Week) %>%
      summarise(
        Games = sum(Games),
        FG = sum(FG),
        FGA = sum(FGA),
        `FG%` = (FG / FGA),
        FT = sum(FT),
        FTA = sum(FTA),
        `FT%` = (FT / FTA),
        `3P` = sum(`3P`),
        PTS = sum(PTS),
        PTS = sum(PTS),
        REB = sum(REB),
        AST = sum(AST),
        STL = sum(STL),
        BLK = sum(BLK),
        TOV = sum(TOV)
      ) %>%
      mutate_at(vars(c(`FG%`, `FT%`)), round, 3),
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")))
  )

  output$team_stat_avg <- renderDataTable(
    summarized_gl %>%
      filter(Player %in% input$team_player_input) %>%
      group_by(Week) %>%
      summarise(
        Games = sum(Games),
        FG = sum(FG),
        FGA = sum(FGA),
        `FG%` = (FG / FGA),
        FT = sum(FT),
        FTA = sum(FTA),
        `FT%` = (FT / FTA),
        `3P` = sum(`3P`),
        PTS = sum(PTS),
        PTS = sum(PTS),
        REB = sum(REB),
        AST = sum(AST),
        STL = sum(STL),
        BLK = sum(BLK),
        TOV = sum(TOV)
      ) %>%
      summarise(
        FG = mean(FG),
        FGA = mean(FGA),
        `FG%` = (FG / FGA),
        FT = mean(FT),
        FTA = mean(FTA),
        `FT%` = (FT / FTA),
        `3P` = mean(`3P`),
        PTS = mean(PTS),
        PTS = mean(PTS),
        REB = mean(REB),
        AST = mean(AST),
        STL = mean(STL),
        BLK = mean(BLK),
        TOV = mean(TOV)
      ) %>%
      mutate_at(vars(-c(`FG%`, `FT%`)), round, 1) %>%
      mutate_at(vars(c(`FG%`, `FT%`)), round, 3),
    rownames = FALSE,
    options = list(columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")))
  )

  output$week_bkdwn <- renderDataTable(
    summarized_gl %>%
      ungroup() %>%
      filter(Player %in% input$team_player_input & Week == input$week_input) %>%
      select(-c(Week, General_Position)) %>%
      mutate_at(vars(c(`FG%`, `FT%`)), round, 3),
    rownames = FALSE,
    options = list(pageLength = 13, columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")))
  )
}


shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))