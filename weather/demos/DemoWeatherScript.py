import os, sys
sys.path.append(os.path.dirname(sys.path[0]))

from WeatherScraper import WeatherScraper


user_loc = "Seattle, WA"
date = "3/2016"

scraper = WeatherScraper(user_loc, date)
scraper.run()

