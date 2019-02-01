from WeatherScraper import WeatherScraper

user_loc = input("Provide city and state abbreviation. ")
date = input("What month and year would you like to see? (Format MM/YYYY) ")

scraper = WeatherScraper(user_loc, date)
scraper.run()