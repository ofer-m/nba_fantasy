import pandas as pd
import numpy as np
from urllib.request import urlopen
import requests
from bs4 import BeautifulSoup
from unidecode import unidecode

class Player:
    """
    The class scrapes and stores stats for an individual NBA player
    """
    def __init__(self, url, name, season, position, gamelogs = None, last_date=None):
        """
        The method initializes the object type Player
        
        Attributes
        ----------
        url: str
            The URL to scrape
        name: str
            The name of the NBA player
        season: str
            The NBA season of the player's stats, in form "YYYY-YYYY"
        postion: str
            The NBA player's position
        table: BeautifulSoup
            The table of gamelogs to scrape
        gamelogs: None or Pandas Dataframe
            The player's gamelogs
            None if scraping whole season 
            Pandas dataframe if scraping games after a certain date 
        last_date: None or pd.DateTime
            The last date in gamelog data. 
            None if scraping whole season
            pd.DateTime if scraping games after a certain date (the day after this attribute)
        aggregated_data: None
            Pandas Dataframe with aggregated data of per game stats
        updated_games:None
            Pandas Dataframe with all of a player's gamelogs after a certain date
        """
        self.url = BeautifulSoup(requests.get(url).text, "lxml")
        self.name = name
        self.season = season
        self.position = position
        self.last_date = last_date
        self.table = self._find_table()
        self.gamelogs = gamelogs
        self.aggregated_data = None
        self.updated_games = None
        
    
    def _find_table(self):
        """
        Method finds table of regular season stats to scrape for player
        
        Returns
        ----------
        BeautifulSoup
            BeautifulSoup representing the table of gamelogs to scrape
        """
        i = 0
        idx = ""
        szn = self.season[:5] + self.season[-2:]
        # loop through tables until finding the regular season stats table
        for table in self.url.find_all("table"):
            if table.caption.text == szn + " Regular Season Table":
                idx = i
                break
            else:
                i+=1
        
        return self.url.find_all("table")[idx]
    
    def _scrape_gamelogs(self):
        """
        Method scrapes gamelogs for an NBA player for an NBA season
        """
        data = []
        for row in self.table.find_all("tr"):
            # collect stats for each game for a player
            game_data = {"Player":self.name, "Date":"", "Team":"", "Opp":"", "Venue":"", "Starter":0, "Position":self.position,
                         "General_Position": self._normalize_position(self.position), "Minutes": 0, "FG":0, "FGA":0, "FG%":0 ,"3P":0, 
                         "3PA":0, "3P%":0 ,"FT":0, "FTA":0, "FT%":0, "REB":0, "AST":0, "STL":0, "BLK":0, "TOV":0, "PTS":0, "DNP":0, 
                         "Week":"", "Season":self.season}

            for cell in row.find_all("td"):
                if cell['data-stat']=="date_game":
                    # if last_date is not passed in class
                    # scrape all rows and collect dates
                    if self.last_date == None:
                        game_data["Date"] = cell.text
                        game_data["Week"] = self._add_week(cell.text)
                    # if last_date is passed in class
                    # only scrape row if date of game
                    # is later than last_date
                    else:
                        date = pd.to_datetime(cell.text)
                        if date <= self.last_date:
                            break
                        else:
                            game_data["Date"] = cell.text
                            game_data["Week"] = self._add_week(cell.text)
                elif cell['data-stat'] == "team_id":
                    game_data["Team"] = cell.text
                elif cell['data-stat'] == "game_location":
                    if cell.text=="@":
                        game_data["Venue"] = "Away"
                    else:
                        game_data["Venue"] = "Home"
                elif cell['data-stat'] == "opp_id":
                    game_data["Opp"] = cell.text
                # if a reason for a player not playing
                # in game is in the row
                # set DNP to 1 and go to next row of table
                elif cell['data-stat'] == "reason":
                    game_data["DNP"] = 1
                    continue
                elif cell['data-stat'] == "gs":
                    game_data["Starter"] = int(cell.text)
                elif cell['data-stat'] == "mp":
                    game_data["Minutes"] = self._fix_minutes(cell.text)
                elif cell['data-stat'] == "fg":
                    game_data["FG"] = int(cell.text)
                elif cell['data-stat'] == "fga":
                    game_data["FGA"] = int(cell.text)
                elif cell['data-stat'] == "fg3":
                    game_data["3P"] = int(cell.text)
                elif cell['data-stat'] == "fg3a":
                    game_data["3PA"] = int(cell.text)
                elif cell['data-stat'] == "ft":
                    game_data["FT"] = int(cell.text)
                elif cell['data-stat'] == "fta":
                    game_data["FTA"] = int(cell.text)
                elif cell['data-stat'] == "trb":
                    game_data["REB"] = int(cell.text)
                elif cell['data-stat'] == "ast":
                    game_data["AST"] = int(cell.text)
                elif cell['data-stat'] == "stl":
                    game_data["STL"] = int(cell.text)
                elif cell['data-stat'] == "blk":
                    game_data["BLK"] = int(cell.text)
                elif cell['data-stat'] == "tov":
                    game_data["TOV"] = int(cell.text)
                elif cell['data-stat'] == "pts":
                    game_data["PTS"] = int(cell.text)
            
            # calculate percentages for FG, 3P and FT
            if game_data["FGA"] != 0:
                game_data["FG%"] = game_data["FG"] / game_data["FGA"]
            if game_data["3PA"] != 0:
                game_data["3P%"] = game_data["3P"] / game_data["3PA"]
            if game_data["FTA"] != 0:
                game_data["FT%"] = game_data["FT"] / game_data["FTA"]
            data.append(game_data)


        # if last_date is passed into class 
        if self.last_date != None:
            # add data to updated_games attribute
            self.updated_games = pd.DataFrame(data)
            self.updated_games = self.updated_games.query("Team != ''")
            # append data to gamelogs passed in, to be used in 
            # calculation for aggregated data
            self.gamelogs = self.gamelogs.append(self.updated_games)
        else:
            self.gamelogs = pd.DataFrame(data)
            
        
        self.gamelogs = self.gamelogs.query("Team != ''")

    def _fix_minutes(self, x):
        """
        Method converts MM:SS to minutes in decimals
        
        Parameters
        ----------
        x: str
            The minutes the players played in the game
        
        Returns
        ----------
        float
            float representing the minutes the player played
        """
        if x == 0:
            return 0
        
        x = x.split(":")
        return int(x[0]) + (int(x[1])/60)
    
    def _normalize_position(self, pos):
        """
        Method to normalize position. Ie SF and PF are F position.
        
        Parameters
        ----------
        pos: str
            The position of the player
            
        Returns
        ----------
        str
            The normalized position of the player
        """
        if "F" in pos:
            return "F"
        if "C" in pos:
            return "C"
        if "G" in pos:
            return "G"
    
    def _add_week(self, date):
        """
        Method returns what week (in Yahoo schedule) the game occured

        Parameters
        ----------
        date: str
            The date of the game
            
        Returns
        ----------
        int
            The week the game occured in

        """
        date = pd.to_datetime(date)
        
        # key: last date of the week
        # value: the week
        date_dic = {"2020-12-27":1, "2021-01-03":2,
                    "2021-01-10":3, "2021-01-17":4,
                    "2021-01-24":5, "2021-01-31":6,
                    "2021-02-07":7, "2021-02-14":8,
                    "2021-02-21":9, "2021-02-28":10,
                    "2021-03-14":11, "2021-03-21":12,
                    "2021-03-28":13, "2021-04-04":14,
                    "2021-04-11":15, "2021-04-18":16,
                    "2021-04-25":17, "2021-05-02":18,
                    "2021-05-09":19, "2021-05-16":20}
        
        
        for d,w in date_dic.items():
            if date <= pd.to_datetime(d):
                return date_dic[d]
    
    def get_gamelogs(self):
        """
        Method calls the scrape_gamelogs method and returns DataFrame of scraped gamelogs
        
        Returns
        ----------
        Pandas DataFrame
            Pandas DataFrame representing the player's gamelogs
        """
        self._scrape_gamelogs()
        # return all gamelogs if no last_date
        # only return updated games if last_date is passed
        if self.last_date == None:
            return self.gamelogs
        else:
            return self.updated_games
    
    def _aggregate_gamelogs(self):
        """
        Method aggregates data from gamelogs to get summary statistics per game
        """
        
        # aggregate stats by player
        # don't include games the player didn't play in
        agg = self.gamelogs.query("DNP==0").groupby(["Player"], as_index=False).aggregate({"Position": "size",
                                                                    "Starter":"sum", 
                                                                    "Minutes":"mean",
                                                                    "FG":"mean",
                                                                    "FGA":"mean",
                                                                    "3P":"mean",
                                                                    "3PA":"mean",
                                                                    "FT":"mean",
                                                                    "FTA":"mean",
                                                                    "REB":"mean",
                                                                    "AST":"mean",
                                                                    "STL":"mean",
                                                                    "BLK":"mean",
                                                                    "TOV":"mean",
                                                                    "PTS":"mean"})
        
        agg = agg.rename(columns={"Position":"Games_Played", "Starter":"Games_Started"})
        agg["Position"] = self.position
        agg["General_Position"] = self._normalize_position(self.position)
        # add percentages for FG, FT, and 3P
        # and fill NA with 0
        agg["FG%"] = agg["FG"] / agg["FGA"]
        agg["FG%"] = agg["FG%"].fillna(0)
        agg["FT%"] = agg["FT"] / agg["FTA"]
        agg["FT%"] = agg["FT%"].fillna(0)
        agg["3P%"] = agg["3P"] / agg["3PA"]
        agg["3P%"] = agg["3P%"].fillna(0)
        
        agg['Games_Missed'] = self.gamelogs.DNP.sum()
        agg["Season"] = self.season
        
        agg = agg.round({"Minutes":2, "FG":1, "FGA":1, "FG%": 3, "3P":1, "3PA":1, "3P%": 3, "FT":1, "FTA":1, "FT%": 3,
                         "REB":1, "AST":1, "STL":1, "BLK":1, "TOV":1, "PTS":1})
        
        self.aggregated_data = agg[["Player", "Position", "General_Position" ,"Games_Played", "Games_Started", 
                                    "Games_Missed", "Minutes", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "FT",
                                    "FTA", "FT%", "REB", "AST", "STL", "BLK", "TOV", "PTS", "Season"]]

    def get_aggregated_data(self):
        """
        Method calls the _aggregate_gamelogs method and returns the aggregated_data attribute
        
        Returns
        ----------
        Pandas DataFrame
            Pandas DataFrame representing the aggregated data for the NBA player
        """
        self._aggregate_gamelogs()
        return self.aggregated_data