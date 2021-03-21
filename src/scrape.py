import pandas as pd
import numpy as np
from urllib.request import urlopen
import requests
from bs4 import BeautifulSoup
from unidecode import unidecode

from SeasonStats import SeasonStats
from Player import Player

if __name__ == '__main__':
    # scrape gamelogs and calculate aggregated data
    season = SeasonStats("2020-2021")
    season.scrape_data()
    gamelogs = season.get_gamelogs()
    agg_data = season.get_agg_data()

    # write gamelogs and aggregated data to csv
    gamelogs.to_csv("../data/nba_gamelogs.csv", index=False)
    agg_data.to_csv("../data/nba_agg_data.csv", index=False)