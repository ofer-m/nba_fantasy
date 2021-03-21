import pandas as pd
import numpy as np
from urllib.request import urlopen
import requests
from bs4 import BeautifulSoup
from unidecode import unidecode

from SeasonStats import SeasonStats
from Player import Player

if __name__ == '__main__':
    # scrape data for new games
    # and re-calculate aggregated data
    season = SeasonStats("2020-2021", update=True)
    season.scrape_data()
    gamelogs = season.get_gamelogs()
    agg_data = season.get_agg_data()

    # add updated games to existing gamelogs csv file
    gamelogs.to_csv("../data/nba_gamelogs.csv", mode='a', header=False, index = False)
    # write aggregated data to csv
    agg_data.to_csv("../data/nba_agg_data.csv", index=False)