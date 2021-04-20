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

week_per_game <- summarized_gl %>%
  ungroup() %>%
  mutate_at(
    c("FG", "FT", "3P", "PTS", "REB", "AST", "STL", "BLK", "TOV"),
    ~ replace_na(round(. / Games, 2), 0)
  ) %>%
  mutate(Week = as.factor(Week))

ui <- navbarPage(
  theme = shinytheme("united"),
  footer = includeHTML("footer.html"),
  paste("NBA Fantasy -", szn), 
  # FIRST PANEL: PLAYER PROFILE
  tabPanel(
    "Player Profile",
    sidebarLayout(
      sidebarPanel(tags$style(".well {background-color:#fe5a1d;}"),

        selectInput(inputId = "player_input", label = "Player:", choices = agg$Player, selected = "LeBron James"),
        br(),
        selectInput("category_input", "Category:", choices = list(
          "FG%" = "FG%", "FT%" = "FT%",
          "3P" = "3P", "PTS" = "PTS",
          "REB" = "REB", "AST" = "AST",
          "STL" = "STL", "BLK" = "BLK",
          "TOV" = "TOV"
        ), selected = "FG%"),
        br(),
        
        dateRangeInput("dateRange",
          label = "Date Range",
          start = min(gl$Date), end = max(gl$Date),
          separator = " - "
        ),
        br(),
        sliderInput("min_games_input", "Minimum Games Played:", min = 0, max = max(agg$Games_Played), value = 0, step = 1),
        br(),
        sliderInput("min_minutes_input", "Minutes per Game:", min = 0, max = 48, value = c(0, 48), step = 1),
        br(),
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
          tabPanel(h3("Week Summary"), plotlyOutput("week_summary"), width = "auto", height = "auto"),
          tabPanel(h3("Game Logs"), div(dataTableOutput("gamelog"), style = "font-size:115%"), width = "auto")
        )
      )
    ), h3(paste("Last Updated: ", max(gl$Date))),

  ),

  tabPanel(
    "Compare Players",
    sidebarLayout(
      sidebarPanel(
        tags$style(".well {background-color:#fe5a1d;}"),
        selectInput(inputId = "player_1", label = "Player 1:", choices = agg$Player, selected = "LeBron James"),
        br(),
        selectInput(inputId = "player_2", label = "Player 2:", choices = agg$Player, selected = "Kevin Durant"),
        br(),
        selectInput("category_input_comp", "Category:", choices = list(
          "FG%" = "FG%", "FT%" = "FT%",
          "3P" = "3P", "PTS" = "PTS",
          "REB" = "REB", "AST" = "AST",
          "STL" = "STL", "BLK" = "BLK",
          "TOV" = "TOV"
        ), selected = "FG%"),
        br(),
        dateRangeInput("dateRange_compare",
          label = "Date Range",
          start = min(gl$Date), end = max(gl$Date),
          separator = " - "
        ),
        br(),
        sliderInput("min_games_input_comp", "Minimum Games Played:", min = 0, max = max(agg$Games_Played), value = 0, step = 1),
        br(),
        sliderInput("min_minutes_input_comp", "Minutes per Game:", min = 0, max = 48, value = c(0, 48), step = 1),
        br(),
        checkboxGroupInput("pos_compare_comp", "Positions to Compare:",
          choices = c("G" = "G", "F" = "F", "C" = "C"),
          selected = c("G", "F", "C")
        ),
        width = 2
      ),


      mainPanel(
        tags$head(
          tags$style(HTML("
    #comp_player1_text {
      color: black;
    }
    #comp_player2_text {
      color: grey;
    }
   
    } 
  "))
        ),

        tags$div(h1(textOutput("comp_player1_text", inline = TRUE), style = "display:inline-block;color:black;font-size:70px;"),
          h1("vs.", style = "display:inline-block;color:#fe5a1d;font-size:50px;margin: 40px;"),
          h1(textOutput("comp_player2_text", inline = TRUE), style = "display:inline-block;color:grey;font-size:70px"),
          align = "center"
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(h3("Quick Comparison"),
            div(dataTableOutput("comp_table"), style = "font-size:175%"),
            width = "auto"
          ),
          tabPanel(
            h3("League Comparison"),
            plotlyOutput("comp_league_plot", width = "auto", height = "auto")
          ),
          tabPanel(
            h3("Distribution"),
            plotlyOutput("dist_compare", width = "auto", height = "auto")
          ),
          tabPanel(
            h3("Week Summary"),
            plotlyOutput("week_compare", width = "auto", height = "auto")
          )
        )
      )
    ), h3(paste("Last Updated: ", max(gl$Date)))
    
  ),
  # SECOND PANEL: TEAM BUILDER
  tabPanel(
    "Team Builder", 
    titlePanel(h1(paste("Fantasy Team Builder"), align = "center", style = "color: #fe5a1d;font-size:70px")),
    h2("Enter up to 13 players to see how your team would perform, based on 2020-2021 stats", align = "center", style = "font-size:20px"),
    sidebarLayout(
      sidebarPanel(tags$style(".well {background-color:#fe5a1d;}"),
        selectizeInput("team_player_input", "Players:",
          choices = agg$Player, multiple = TRUE,
          options = list(maxItems = 13)
        ),
        br(),
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
    ), h3(paste("Last Updated: ", max(gl$Date)))
  ),
  # THIRD PANEL: PLAYER STATS
  tabPanel(
    "Player Stats", 
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
    ), fluidRow(mainPanel(
      fluidRow(div(dataTableOutput("league_stats", width = "auto"), style = "font-size:105%")))
    ), h3(paste("Last Updated: ", max(gl$Date)))
  )
)

server <- function(input, output) {
  output$league_comp <- renderPlotly(
    plot_stat_league(gl, input$player_input, input$category_input,
      input$pos_compare,
      min_games = input$min_games_input,
      min_minutes = input$min_minutes_input,
      date = input$dateRange
    )
  )
  output$stat_dist <- renderPlotly(
    plot_stats_ind(gl, input$player_input, input$category_input, input$dateRange)
  )
  output$player_table <- renderTable(
    gl %>%
      filter(Player == input$player_input &
        Date >= input$dateRange[1] &
        Date <= input$dateRange[2]) %>%
      group_by(Player) %>%
      summarise(
        Games_Played = n() - sum(DNP),
        Games_Started = sum(Starter),
        Games_Missed = sum(DNP),
        Minutes = round(mean(Minutes[DNP == 0]), 2),
        FG = round(mean(FG[DNP == 0]), 2),
        FGA = round(mean(FGA[DNP == 0]), 2),
        `FG%` = round(FG / FGA, 3),
        `3P` = round(mean(`3P`[DNP == 0]), 2),
        `3PA` = round(mean(`3PA`[DNP == 0]), 2),
        `3P%` = round(`3P` / `3PA`, 3),
        FT = round(mean(FT[DNP == 0]), 2),
        FTA = round(mean(FTA[DNP == 0]), 2),
        `FT%` = round(FT / FTA, 3),
        REB = round(mean(REB[DNP == 0]), 2),
        AST = round(mean(AST[DNP == 0]), 2),
        STL = round(mean(STL[DNP == 0]), 2),
        BLK = round(mean(BLK[DNP == 0]), 2),
        TOV = round(mean(TOV[DNP == 0]), 2),
        PTS = round(mean(PTS[DNP == 0]), 2),
      ) %>%
      mutate_at(vars(-c(Player)), replace_na, 0) %>%
      select(-c(Player)) %>%
      rename(Games = Games_Played, `Games Started` = Games_Started, `Games Missed` = Games_Missed) %>%
      mutate(
        Games = as.integer(Games),
        `Games Started` = as.integer(`Games Started`),
        `Games Missed` = as.integer(`Games Missed`)
      )
  )

  output$gamelog <- renderDataTable(
    gl %>%
      filter(Player == input$player_input &
        Date >= input$dateRange[1] &
        Date <= input$dateRange[2]) %>%
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

  output$week_summary <- renderPlotly(
    week_plot(week_per_game, input$player_input, input$category_input)
  )


  output$comp_player1_text <- renderText(
    input$player_1
  )

  output$comp_player2_text <- renderText(
    input$player_2
  )

  output$comp_table <- renderDataTable(DT::datatable(
    compare_players_table(gl, input$player_1, input$player_2, input$dateRange_compare),
    rownames = F,
    options = list(
      pageLength = 25, ordering = F,
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      rowCallback = JS(
        'function(row, data) {
    var num_data = data.slice(1,data.length)
    var max_stat = Math.max.apply(Math,num_data);
    for(i=1;i < data.length; i++) {
      if(data[i]==max_stat) {
        $("td:eq("+i+")", row).css("background-color", "lightgreen")
      } 
    }
  }'
      )
    )
  ) %>%
    formatStyle(columns = c("Stat"), fontWeight = "bold") %>%
    formatStyle(columns = c(input$compare_player_input[1], input$compare_player_input[2]), `text-align` = "center"))



  output$comp_league_plot <- renderPlotly(
    plot_stat_league_compare(gl,
      c(input$player_1, input$player_2),
      category = input$category_input_comp,
      positions = input$pos_compare_comp,
      min_games = input$min_games_input_comp,
      min_minutes = input$min_minutes_input_comp,
      date = input$dateRange_compare
    )
  )

  output$dist_compare <- renderPlotly(
    plot_stats_compare(gl,
      c(input$player_1, input$player_2),
      category = input$category_input_comp,
      date = input$dateRange_compare
    )
  )


  output$week_compare <- renderPlotly(
    week_plot_compare(week_per_game,
      input$player_1, input$player_2,
      category = input$category_input_comp
    )
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