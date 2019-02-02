from WeatherScraper import WeatherScraper

user_loc = "Seattle, WA"
date = "2/2016"

scraper = WeatherScraper(user_loc, date)
scraper.run()