from context import WeatherScraper
user_loc = "Chicago Illinois"
date = "1/1997"

scraper = WeatherScraper(user_loc, date)
scraper.run()
