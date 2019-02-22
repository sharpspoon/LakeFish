import time
from context import WeatherScraper

user_loc = input("Provide city and state abbreviation. ")
date = input("What month and year would you like to see? (Format MM/YYYY) ")
start = time.time()
scraper = WeatherScraper(user_loc, date)
scraper.run()

print("Script took", time.time() - start, " seconds to run")
