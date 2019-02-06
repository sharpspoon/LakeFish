from WeatherScraper import WeatherScraper
import os

user_loc = "Seattle, WA"
date = "3/2016"

scraper = WeatherScraper(user_loc, date)
scraper.run()

