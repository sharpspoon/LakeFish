from WeatherScraper import WeatherScraper


user_loc = input("Provide city and state abbreviation. ").upper()

month, year = input("What month and year would you like to see? (Please provide in numerical format) ").split("/")

scraper = WeatherScraper(user_loc, month, year)
scraper.run()