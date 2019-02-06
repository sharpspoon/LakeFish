import os
import sys
import unittest
from weather.weatherscraper import WeatherScraper


class TestWeatherScrapper(unittest.TestCase):
    def test100_testDataAlreadyRetrieved(self):
        user_loc = "Mobile,AL"
        date = "1/1999"
        scraper = WeatherScraper(user_loc, date)
        pull_data = scraper._data_already_retrieved()
        self.assertEqual(pull_data, True)

    def test100_testDataAlreadyRetrievedForFileWithMultipleMonths(self):
        user_loc = "Mobile,AL"
        date = "2/1999"
        scraper = WeatherScraper(user_loc, date)
        pull_data = scraper._data_already_retrieved()
        self.assertEqual(pull_data, True)

if __name__ == '__main__':
    unittest.main()
