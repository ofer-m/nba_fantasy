library(ggplot2)
library(tidyverse)
library(plotly)

#' Round down decimal to floor to specified decimal places
#'
#' @param x A float
#' @param level A number representing number of decimal places to down round to (default is 1)
#' @return decimal rounded to floor

#' @examples
#' floor_decimal(0.24, 1)
floor_decimal <- function(x, level = 1) round(x - 5 * 10^(-level - 1), level)

#' Round up decimal to floor to specified decimal places
#'
#' @param x A float
#' @param level A number representing number of decimal places to round up to (default is 1)
#' @return decimal rounded to ceiling

#' @examples
#' ceiling_decimal(0.24, 1)
ceiling_decimal <- function(x, level = 1) round(x + 5 * 10^(-level - 1), level)


#' Create plot of distribution of particular stat category for an NBA player in a season
#' The following stat categories create bar plots:
#'    3P, PTS, REB, AST, STL, BLK, TOV
#' The following stat categories create scatter plots
#'    FG%, FT%

#' @param gl A tibble of gamelogs for NBA players
#' @param selected_player A string representing a name of an NBA player
#' @param category A string representing a stat category (options: 3P, PTS, REB, AST, STL, BLK, TOV, FG%, FT%)
#' @param date A chr of string representing the start and end date
#' @return ggplotly plot

#' @examples
#' gamelogs <- read_csv("../data/nba_gamelogs.csv")
#' plot_stats_ind(gamelogs, "LeBron James", "3P", c("2020-01-22", "2021-03-01"))
plot_stats_ind <- function(gl, selected_player, category, date) {
  # filter gamelog to find player and games he played
  gl_plot <- gl %>% filter(Player == selected_player & DNP == 0 &
    Date >= date[1] & Date <= date[2])
  szn <- unique(gl_plot$Season)

  if (category == "3P") {
    # get x axis scales
    min_amt <- min(gl_plot[["3P"]])
    max_amt <- max(gl_plot[["3P"]])

    plot <- gl_plot %>%
      ggplot(aes(x = `3P`, text = ..count..)) +
      geom_bar(fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, 1)) +
      ggtitle(paste("3P Distribution")) +
      labs(y = "Number of Games", x = "3P Made")
  } else if (category == "REB") {
    min_reb <- min(gl_plot[["REB"]])
    max_reb <- max(gl_plot[["REB"]])

    plot <- gl_plot %>%
      ggplot(aes(x = `REB`, text = ..count..)) +
      geom_bar(fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      coord_cartesian(xlim = c(min_reb, max_reb)) +
      scale_x_continuous(breaks = seq(min_reb, max_reb, 1)) +
      ggtitle(paste("REB Distribution")) +
      labs(y = "Number of Games", x = "REB")
  } else if (category == "AST") {
    min_ast <- min(gl_plot[["AST"]])
    max_ast <- max(gl_plot[["AST"]])

    plot <- gl_plot %>%
      ggplot(aes(x = `AST`, text = ..count..)) +
      geom_bar(fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      coord_cartesian(xlim = c(min_ast, max_ast)) +
      scale_x_continuous(breaks = seq(min_ast, max_ast, 1)) +
      ggtitle(paste("AST Distribution")) +
      labs(y = "Number of Games", x = "AST")
  } else if (category == "STL") {
    min_stl <- min(gl_plot[["STL"]])
    max_stl <- max(gl_plot[["STL"]])

    plot <- gl_plot %>%
      ggplot(aes(x = `STL`, text = ..count..)) +
      geom_bar(fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      coord_cartesian(xlim = c(min_stl, max_stl)) +
      scale_x_continuous(breaks = seq(min_stl, max_stl, 1)) +
      ggtitle(paste("STL Distribution")) +
      labs(y = "Number of Games", x = "STL")
  } else if (category == "BLK") {
    min_blk <- min(gl_plot[["BLK"]])
    max_blk <- max(gl_plot[["BLK"]])

    plot <- gl_plot %>%
      ggplot(aes(x = `BLK`, text = ..count..)) +
      geom_bar(fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      coord_cartesian(xlim = c(min_blk, max_blk)) +
      scale_x_continuous(breaks = seq(min_blk, max_blk, 1)) +
      ggtitle(paste("BLK Distribution")) +
      labs(y = "Number of Games", x = "BLK")
  } else if (category == "TOV") {
    min_tov <- min(gl_plot[["TOV"]])
    max_tov <- max(gl_plot[["TOV"]])

    plot <- gl_plot %>%
      ggplot(aes(x = `TOV`, text = ..count..)) +
      geom_bar(fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      coord_cartesian(xlim = c(min_tov, max_tov)) +
      scale_x_continuous(breaks = seq(min_tov, max_tov, 1)) +
      ggtitle(paste("TOV Distribution")) +
      labs(y = "Number of Games", x = "TOV")
  } else if (category == "FT%") {
    plot <- ggplot(gl_plot, aes(
      x = `FT%`, y = FT,
      text = paste(
        "</br>FT%:", round(`FT%`, 3),
        "</br>FT:", FT
      )
    )) +
      geom_jitter(colour = "#FE5A1D") +
      theme_bw(base_size = 12) +
      ggtitle(paste("FT% vs FT Made"))
  } else if (category == "FG%") {
    plot <- ggplot(gl_plot, aes(
      x = `FG%`, y = FG,
      text = paste(
        "</br>FG%:", round(`FG%`, 3),
        "</br>FG:", FG
      )
    )) +
      geom_jitter(colour = "#FE5A1D") +
      theme_bw(base_size = 12) +
      ggtitle(paste("FG% vs FG Made"))
  } else if (category == "PTS") {
    max_pts <- max(gl_plot$PTS)
    gl_plot$cut_pts <- cut(gl_plot$PTS, seq(0, max_pts + 5, 5), include.lowest = TRUE)

    plot <- gl_plot %>%
      group_by(`cut_pts`) %>%
      summarize(Count = n()) %>%
      ggplot(aes(x = `cut_pts`, y = Count, text = Count)) +
      geom_bar(stat = "identity", fill = "#FE5A1D") +
      theme_bw(base_size = 12) +
      ggtitle(paste("PTS Distribution")) +
      labs(y = "Number of Games", x = "PTS Scored")
  }

  # return the plot
  ggplotly(plot, tooltip = c("text"), width = 1300, height = 600)
}


#' Create plot of comparing how an NBA player performs in a stat category
#' compared to the rest of the league.
#' The following stat categories create bar plots:
#'    3P, PTS, REB, AST, STL, BLK, TOV
#' The following stat categories create scatter plots
#'    FG%, FT%

#' @param gl A tibble of gamelogs for NBA players
#' @param selected_player A string representing a name of an NBA player
#' @param category A string representing a stat category (options: 3P, PTS, REB, AST, STL, BLK, TOV, FG%, FT%)
#' @param positions A vector with positions to compare to (options: F, C, G)
#' @param min_games A number representing the minimum games the players played to appear in plot
#' @param min_minutes A vector containing the minimum and maximum minutes per game a player played to appear in plot
#' @param date A chr of string representing the start and end date
#' @return ggplotly plot

#' @examples
#' gamelogs <- read_csv("../data/nba_gamelogs.csv")
#' plot_stat_league(gamelogs, "LeBron James", "3P", c("F", "C", "G"), 20, c(20, 40), c("2020-01-22", "2021-03-01"))
plot_stat_league <- function(gl, selected_player, category, positions, min_games = 0, min_minutes = c(0, 48), date) {
  agg <- gl %>%
    filter(Date >= date[1] & Date <= date[2] & General_Position %in% positions) %>%
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
    mutate_at(vars(-c(Player)), replace_na, 0)



  agg_player <- agg %>% filter(Player == selected_player)
  # get player's name
  player_name <- agg_player %>%
    select(`Player`) %>%
    pull()
  # filter data for inputted parameters
  agg_plot <- agg %>% filter(Games_Played >= min_games &
    Minutes >= min_minutes[1] &
    Minutes <= min_minutes[2])
  szn <- unique(gl$Season)

  if (category == "3P") {
    player <- agg_player %>%
      select(`3P`) %>%
      pull()
    mean_amt <- mean(agg_plot[["3P"]])
    min_amt <- min(agg_plot[["3P"]])
    max_amt <- max(agg_plot[["3P"]])

    tick_size <- 0.2
    if (max_amt > 6) {
      tick_size <- 0.4
    }

    plot <- ggplot(agg_plot, aes(`3P`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 0.2, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +
      ggtitle(paste("3P Compared to League (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, tick_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)

    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "PTS") {
    player <- agg_player %>%
      select(`PTS`) %>%
      pull()
    mean_amt <- mean(agg_plot[["PTS"]])
    min_amt <- min(agg_plot[["PTS"]])
    max_amt <- max(agg_plot[["PTS"]])

    plot <- ggplot(agg_plot, aes(`PTS`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 2, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +
      ggtitle(paste("PTS Compared to League in", szn, "NBA Season (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, 2)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)
    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "REB") {
    player <- agg_player %>%
      select(`REB`) %>%
      pull()
    mean_amt <- mean(agg_plot[["REB"]])
    min_amt <- min(agg_plot[["REB"]])
    max_amt <- max(agg_plot[["REB"]])

    tick_size <- 0.5
    if (max_amt > 15) {
      tick_size <- 1
    }

    plot <- ggplot(agg_plot, aes(`REB`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 0.5, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +
      ggtitle(paste("REB Compared to League in (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, tick_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)
    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "AST") {
    player <- agg_player %>%
      select(`AST`) %>%
      pull()
    mean_amt <- mean(agg_plot[["AST"]])
    min_amt <- min(agg_plot[["AST"]])
    max_amt <- max(agg_plot[["AST"]])

    tick_size <- 0.5
    if (max_amt > 15) {
      tick_size <- 1
    }

    plot <- ggplot(agg_plot, aes(`AST`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 0.5, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +
      ggtitle(paste("AST Compared to League in (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, tick_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)
    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "STL") {
    player <- agg_player %>%
      select(`STL`) %>%
      pull()
    mean_amt <- mean(agg_plot[["STL"]])
    min_amt <- min(agg_plot[["STL"]])
    max_amt <- max(agg_plot[["STL"]])

    tick_size <- 0.1
    if (max_amt > 6) {
      tick_size <- 0.5
    } else if (max_amt > 3) {
      tick_size <- 0.2
    }

    plot <- ggplot(agg_plot, aes(`STL`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 0.1, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +
      ggtitle(paste("STL Compared to League in (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, tick_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)
    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "BLK") {
    player <- agg_player %>%
      select(`BLK`) %>%
      pull()
    mean_amt <- mean(agg_plot[["BLK"]])
    min_amt <- min(agg_plot[["BLK"]])
    max_amt <- max(agg_plot[["BLK"]])

    tick_size <- 0.2
    if (max_amt > 6) {
      tick_size <- 0.4
    }

    plot <- ggplot(agg_plot, aes(`BLK`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 0.2, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +
      ggtitle(paste("BLK Compared to League (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, tick_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)
    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "TOV") {
    player <- agg_player %>%
      select(`TOV`) %>%
      pull()
    mean_amt <- mean(agg_plot[["TOV"]])
    min_amt <- min(agg_plot[["TOV"]])
    max_amt <- max(agg_plot[["TOV"]])

    tick_size <- 0.2
    if (max_amt > 6) {
      tick_size <- 0.4
    }

    plot <- ggplot(agg_plot, aes(`TOV`,
      text = paste0(
        "Range: ", ..xmin.., "-", ..xmax..,
        "</br>", "</br>Number of Players: ",
        ..count..
      )
    )) +
      geom_histogram(binwidth = 0.2, boundary = 0, fill = "#FE5A1D", colour = "white") +
      geom_vline(aes(xintercept = player, color = player_name)) +
      geom_vline(aes(xintercept = mean_amt, color = "League Average"), lty = 2) +

      ggtitle(paste("TOV Compared to League (per game)")) +
      labs(y = "Number of Players") +
      coord_cartesian(xlim = c(min_amt, max_amt)) +
      scale_x_continuous(breaks = seq(min_amt, max_amt, tick_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual("", values = c("blue", "black")) +
      theme_bw(base_size = 12)
    ggplotly(plot, tooltip = "text", width = 1300, height = 600)
  } else if (category == "FT%") {
    ftp_player <- agg_player %>%
      select(`FT%`) %>%
      pull()
    ft_player <- agg_player %>%
      select(FT) %>%
      pull()
    mean_ftp <- mean(agg_plot[["FT%"]])
    max_ftp <- ceiling_decimal(max(agg_plot[["FT%"]]))
    if (max_ftp == 1.1) {
      max_ftp <- 1
    }
    min_ftp <- floor_decimal(min(agg_plot[["FT%"]]))
    max_ft <- ceiling_decimal(max(agg_plot[["FT"]]), 0)

    min_ft <- floor_decimal(min(agg_plot[["FT"]]), 0)

    agg_plot <- agg_plot %>% filter(Player != selected_player)

    plot <- ggplot(agg_plot, aes(x = `FT%`, y = FT, text = Player)) +
      geom_point(colour = "#FE5A1D") +
      geom_point(aes(x = ftp_player, y = ft_player, text = selected_player), colour = "black", shape = "star", size = 3) +
      geom_vline(aes(xintercept = mean_ftp, color = "Mean FT%"), lty = 2) +
      scale_color_manual(values = c(`Mean FT%` = "blue")) +
      scale_y_continuous(
        limits = c(min_ft, max_ft),
        breaks = seq(min_ft, max_ft, 1)
      ) +
      scale_x_continuous(
        limits = c(min_ftp, max_ftp),
        breaks = seq(min_ftp, max_ftp, 0.1)
      ) +
      labs(title = paste("FT% vs FT Comparison to League (per game)")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw(base_size = 12)


    ggplotly(plot, tooltip = c("Player", "FT", "FT%")) %>%
      layout(legend = list(orientation = "h", x = 0.45, y = -0.2), width = 1300, height = 600)
  } else if (category == "FG%") {
    fgp_player <- agg_player %>%
      select(`FG%`) %>%
      pull()
    fg_player <- agg_player %>%
      select(FG) %>%
      pull()
    mean_fgp <- mean(agg_plot[["FG%"]])

    mean_fgp <- mean(agg_plot[["FG%"]])
    max_fgp <- ceiling_decimal(max(agg_plot[["FG%"]]))
    if (max_fgp == 1.1) {
      max_fgp <- 1
    }
    min_fgp <- floor_decimal(min(agg_plot[["FG%"]]))
    max_fg <- ceiling_decimal(max(agg_plot[["FG"]]), 0)

    min_fg <- floor_decimal(min(agg_plot[["FG"]]), 0)

    agg_plot <- agg_plot %>% filter(Player != selected_player)

    plot <- ggplot(agg_plot, aes(x = `FG%`, y = FG, text = Player)) +
      geom_point(colour = "#FE5A1D") +
      geom_point(aes(x = fgp_player, y = fg_player, text = selected_player), colour = "black", shape = "star", size = 3) +
      geom_vline(aes(xintercept = mean_fgp, color = "Mean FG%"), lty = 2) +
      scale_color_manual(values = c(`Mean FG%` = "blue")) +
      scale_y_continuous(
        limits = c(min_fg, max_fg),
        breaks = seq(min_fg, max_fg, 1)
      ) +
      scale_x_continuous(
        limits = c(min_fgp, max_fgp),
        breaks = seq(min_fgp, max_fgp, 0.1)
      ) +
      labs(title = paste("FG% vs FG Comparison to League (per game)")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw(base_size = 12)

    ggplotly(plot, tooltip = c("Player", "FG", "FG%")) %>%
      layout(legend = list(orientation = "h", x = 0.45, y = -0.2), width = 1300, height = 600)
  }
}

#' Create plot of comparing how an NBA player performs in a stat category
#' in each fantasy week
#' The following stat categories create bar plots:
#'    3P, PTS, REB, AST, STL, BLK, TOV
#' The following stat categories create line plots
#'    FG%, FT%

#' @param week_stat A tibble summarized stats per week for each player
#' @param selected_player A string representing a name of an NBA player
#' @param category A string representing a stat category (options: 3P, PTS, REB, AST, STL, BLK, TOV, FG%, FT%)
#' @return ggplotly plot

#' @examples
#' week_plot(week_stat, "LeBron James", "3P")
week_plot <- function(week_stat, selected_player, category) {
  gl_plot <- week_stat %>% filter(Player == selected_player)

  if (category == "PTS") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = PTS,
      text = paste(
        "PTS per Game:", round(`PTS`, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "PTS per Game each Week in 2020-2021 NBA Season")
  } else if (category == "3P") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = `3P`,
      text = paste(
        "3P per Game:", round(`3P`, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "3P per Game each Week in 2020-2021 NBA Season")
  } else if (category == "REB") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = REB,
      text = paste(
        "REB per Game:", round(REB, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "REB per Game each Week in 2020-2021 NBA Season")
  } else if (category == "AST") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = AST,
      text = paste(
        "AST per Game:", round(AST, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "AST per Game each Week in 2020-2021 NBA Season")
  } else if (category == "STL") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = STL,
      text = paste(
        "STL per Game:", round(STL, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "STL per Game each Week in 2020-2021 NBA Season")
  } else if (category == "BLK") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = BLK,
      text = paste(
        "BLK per Game:", round(BLK, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "BLK per Game each Week in 2020-2021 NBA Season")
  } else if (category == "TOV") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = TOV,
      text = paste(
        "TOV per Game:", round(TOV, 2), "</br>",
        "</br>Games Played:", Games
      )
    )) +
      geom_bar(fill = "#FE5A1D", stat = "identity") +
      theme_bw(base_size = 12) +
      labs(title = "TOV per Game each Week in 2020-2021 NBA Season")
  } else if (category == "FG%") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = `FG%`, group = 1,
      text = paste(
        "FG per Game:", round(`FG`, 2), "</br>",
        "</br>FG% in Week:", round(`FG%`, 3),
        "</br>Games Played:", Games
      )
    )) +
      geom_line() +
      geom_point(aes(size = `FG`), color = "#FE5A1D") +
      coord_cartesian(ylim = c(0, 1)) +
      theme_bw(base_size = 12) +
      labs(title = "FG% Per Week in 2020-2021 NBA Season")
  } else if (category == "FT%") {
    plot <- ggplot(gl_plot, aes(
      x = Week, y = `FT%`, group = 1,
      text = paste(
        "FT per Game:", round(`FT`, 2), "</br>",
        "</br>FT% in Week:", round(`FT%`, 3),
        "</br>Games Played:", Games
      )
    )) +
      geom_line() +
      geom_point(aes(size = `FT`), color = "#FE5A1D") +
      coord_cartesian(ylim = c(0, 1)) +
      theme_bw(base_size = 12) +
      labs(title = "FT% Per Week in 2020-2021 NBA Season")
  }


  ggplotly(plot, tooltip = c("text"), width = 1300, height = 600)
}