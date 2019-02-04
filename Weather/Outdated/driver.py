from WeatherScraperLegacy import WeatherScraperLegacy

user_loc = "Seattle, WA"
month = '1'
year = '2016'

scraper = WeatherScraperLegacy(user_loc, month, year)
scraper.run()