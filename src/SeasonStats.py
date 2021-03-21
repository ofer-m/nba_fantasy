import pandas as pd
import numpy as np
from urllib.request import urlopen
import requests
from bs4 import BeautifulSoup
from unidecode import unidecode
from Player import Player


class SeasonStats:
    
    """
    The class scrapes and stores NBA Player Stats for a certain NBA season.
    """
    def __init__(self, season='2020-2021', update=False):
        """
        The method initializes the object type SeasonStats
        
        Attributes
        ----------
        season: str
            The NBA season of player stats, in form "YYYY-YYYY"
        update: bool
            Bool to represent whether to scrape data for new games that have happened
        url: str
            The URL to scrape
        link: BeautifulSoup
            The object to scrape the URL
        players: None
            Pandas DataFrame to be filled with player name, position and URL
            for all players who played in an NBA season
        gamelogs: Pandas DataFrame
            Pandas Dataframe with all gamelogs of all players in an NBA season
        agg_data: Pandas DataFrame
            Pandas Dataframe with aggregated data of per game stats
        updated_gamelogs: Pandas DataFrame
            Pandas DataFrame with all gamelogs of all players after a certain date.
            Only used if update attribute is True
        """
        
        
        
        if self._validate_season(season):
            self.season = season
            
        self.update = update
        if self.update == True:
            # read in data, find latest date
            # in order to scrape games that happened 
            # after this date
            self.gamelogs = pd.read_csv("../data/nba_gamelogs.csv", parse_dates=["Date"])
            self.last_date = self.gamelogs.Date.max()
            self.updated_gamelogs = pd.DataFrame()
        else:
            self.gamelogs = None
        
        self.url = self._generate_url()
        self.link = BeautifulSoup(requests.get(self.url).text, "lxml")
        self.players = None
        self.agg_data = pd.DataFrame()
    
    def _validate_season(self, season):
        """
        Method to validate the input season
        
        Parameters
        ----------
        season: str
            The NBA season of player stats, in form "YYYY-YYYY"
        
        Returns
        ----------
        bool
            bool representing if season is valid
        """
        # test type and length of input
        if not isinstance(season, str):
            raise TypeError("Input season must be a string")
            
        if len(season)!=9:
            raise ValueError("Length of input season must be 9")
        
        lower = season[:4]
        divider = season[4]
        upper = season[5:]
        
        # ensure input entered in valid manner ("YYYY-YYYY")
        if not lower.isdigit():
            raise ValueError("Lower range of season must be a digit")
        
        if not upper.isdigit():
            raise ValueError("Upper range of season must be a digit")
        
        if divider != "-":
            raise ValueError("Divider between season must be -")
            
        lower = int(lower)
        upper = int(upper)
        
        # test that years inputted are entered in order,
        # and are in a certain year range
        if lower >= upper:
            raise ValueError("Lower range of season must be less than upper range")
        
        if lower not in range(1989,2021):
            raise ValueError("Lower range of season must be between 1989 to 2020")
        
        if lower + 1 != upper:
            raise ValueError("Lower range of season must be 1 year less than upper")
        
        return True
            
    def _generate_url(self):
        """
        Method to generate url to scrape players
        
        Returns
        ----------
        str
            str representing the website to scrape
        """
        year = self.season[5:]
        return "https://www.basketball-reference.com/leagues/NBA_" + year + "_per_game.html"
        
    def _scrape_players(self):
        """
        Method scrapes player names and player URLs for all players in an NBA season and 
        stores in the players attribute.
        """
        
        table = self.link.find_all("table")[0]
        data = []
        for row in table.find_all("tr"):
            # for each row, scrape URL, name and position of player
            row_data = {"URL":"", "Name":"", "Position":""}
            for cell in row.find_all("td"):
                if cell["data-stat"] == "player":
                    # use player id for URL of player's stats
                    player_id = cell["data-append-csv"]
                    gl_url = "https://www.basketball-reference.com/players/a/" + player_id + "/gamelog/" + self.season[5:]
                    row_data["URL"] = gl_url
                    # remove accents from names
                    row_data["Name"] = unidecode(cell.text)

                elif cell["data-stat"] == "pos":
                    pos = cell.text
                    # take first position, if player has multiple positions
                    if "-" in pos:
                        p = pos.split("-")[0]
                        row_data["Position"] = p
                    else:
                        row_data["Position"] = pos
            data.append(row_data)
        
        self.players = pd.DataFrame(data)
        # drop NAs and duplicates
        self.players = self.players.query("URL!=''")
        self.players = self.players.drop_duplicates(subset=["URL"]).reset_index()
        self.players = self.players[["Name", "Position", "URL"]]
    
    def _scrape_player_data(self):
        """
        Method iterates through all players who played in an NBA season,
        creates an instance of the Player class, scrapes the player's gamelogs
        for the season, and appends to the data attribute.
        """
        
        # if update is true (updating data for new games)
        if isinstance(self.gamelogs, pd.DataFrame):
            for i in range(self.players.shape[0]):
                # get each row's URL, name and position
                # get the player's game logs
                # create instance of Player class
                # and retrieve game logs and aggregated data
                url = self.players.loc[i,"URL"]
                name = self.players.loc[i,"Name"]
                pos = self.players.loc[i,"Position"]
                player_data = self.gamelogs.query("Player==@name")
                player = Player(url, name, self.season, pos, player_data, self.last_date)
                self.updated_gamelogs = self.updated_gamelogs.append(player.get_gamelogs())
                self.agg_data = self.agg_data.append(player.get_aggregated_data())
        
        # if update is false (scraping all games)
        else:
            self.gamelogs = pd.DataFrame()
            for i in range(self.players.shape[0]):
                # get each row's URL, name and position
                # create instance of Player class
                # and retrieve game logs and aggregated data
                url = self.players.loc[i,"URL"]
                name = self.players.loc[i,"Name"]
                pos = self.players.loc[i,"Position"]
                player = Player(url, name, self.season, pos)
                self.gamelogs = self.gamelogs.append(player.get_gamelogs())
                self.agg_data = self.agg_data.append(player.get_aggregated_data())
        
    
    def scrape_data(self):
        """
        Method scrapes the players, their gamelogs and aggregated data.
        """
        self._scrape_players()
        self._scrape_player_data()
    
    def get_gamelogs(self):
        """
        Method returns the gamelogs attribute
        
        Returns
        ----------
        Pandas DataFrame
            Pandas DataFrame representing the gamelogs of all NBA players in season
        """
        if self.update==False:
            return self.gamelogs
        else:
            # only return updated gamelogs
            # as it gets appened to remaining data
            # in update.py script
            return self.updated_gamelogs
    
    
    def get_agg_data(self):
        """
        Method returns the agg_data attribute
        
        Returns
        ----------
        Pandas DataFrame
            Pandas DataFrame representing the aggregated data for each player
        """
        return self.agg_data