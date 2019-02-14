from context import WeatherScraper
user_loc = "Seattle, WA"
date = "3/2015"

scraper = WeatherScraper(user_loc, date)
scraper.run()
