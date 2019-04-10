from context import WeatherScraper
user_loc = "Mobile Alabama"
date = "1/1997"

scraper = WeatherScraper(user_loc, date)
scraper.run()
