from WeatherScraper import WeatherScraper


user_loc = input("Provide city and state abbreviation. ")

month, year = input("What month and year would you like to see? (Please provide in numerical format) ").split("/")

scrapper = WeatherScraper(user_loc, month, year)
scrapper.run()

