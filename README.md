# NBA Fantasy Web App

## Overview

A web application that helps NBA fantasy players analyze NBA players to build and manage their team throughout the season. The app uses data scraped from [Basketball Reference](https://www.basketball-reference.com/), for the 2020-2021 NBA Regular Season. 

The app has 3 sections:

### Player Profile

The Player Profile section helps a user analyze how the player performs in each category, and how the player's performance compares to the league. As well, the profile includes the player's performance in each fantasy week, and his game logs. 

![](https://github.com/ofer-m/nba_fantasy/blob/main/img/player_profile.gif)


### Team Builder

The Team Builder section allows a user to build a team and analyze their performance in each category, and summarizes performances each fantasy week. The section can help a user to plan which players to draft, and strategies to drafting, such as punting specific categories. 

![](https://github.com/ofer-m/nba_fantasy/blob/main/img/team_builder.gif)

### Player Stats

The Player Stats section allows the user to query data of per game stats for every NBA player, with filters such as minimum games played, minimum minutes played and position.

![](https://github.com/ofer-m/nba_fantasy/blob/main/img/player_stats.gif)

## Usage
Clone or download this repo.

**To use the web application:**

Run the following command on the command line from the root directory of this project:

```
Rscript  app.R
``` 

OR

Open the `app.R` file in RStudio and click **Run App**.

**To update the data:**

This repository includes data up to March 27, 2021.

Run the following command on the command line from the root directory of this project to update the data for new games:

```
cd src
python update.py
```

**To scrape data for whole season:**

Run the following commands on the command line from the root directory of this project:

```
cd src
python scrape.py
```

*To scrape data form a different NBA season, change the season argument in `SeasonStats` within the `scrape.py` file*.


## Dependencies  

Python 3.7.3
- pandas==1.0.3
- numpy==1.18.1
- requests==2.23.0
- bs4==4.8.2
- unidecode==1.1.1  

R 3.6.2
- tidyverse==1.2.1
- ggplot2==3.2.1
- plotly==4.9.1
- shiny==1.4.0
- DT==0.13
- shinythemes 1.2.0


