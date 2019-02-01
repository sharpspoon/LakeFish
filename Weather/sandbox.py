from DarkSky import MonthlyHistoricalData


import csv
user_loc = "Seattle, WA"

date = "2/2016"

scraper = MonthlyHistoricalData(user_loc, date)
scraper.generate_all_times_for_month()
scraper.grab_weather_data()
#scraper.output_daily_data()
#scraper.grab_two_day()
