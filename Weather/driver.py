from WeatherScraper import WeatherScraper


#user_loc = input("Provide city and state abbreviation. ")

#month, year = input("What month and year would you like to see? (Please provide in numerical format) ").split("/")
user_loc = "Dallas, TX"

month = '2'
year = '2016'
scraper = WeatherScraper(user_loc, month, year)
scraper.run()