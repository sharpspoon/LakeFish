import os
import sys
myPath = os.path.dirname(os.path.dirname(__file__))
sys.path.insert(0, myPath)

from weather.weatherscraper import WeatherScraper
