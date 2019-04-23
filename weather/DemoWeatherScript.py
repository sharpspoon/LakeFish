from context import WeatherScraper
user_loc = "Duluth Minnesota"
date = "12/2018"

scraper = WeatherScraper(user_loc, date)
scraper.run()
