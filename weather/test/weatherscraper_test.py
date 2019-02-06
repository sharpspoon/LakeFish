import os
import sys
import unittest
from datetime import datetime

from weather.weatherscraper import WeatherScraper


class TestWeatherScrapper(unittest.TestCase):

    # Happy Paths

    # Tests _data_already_retrieved()

    def test100_testDataAlreadyRetrieved_DataAlreadyRetrieved(self):
        user_loc = "Mobile,AL"
        date = "1/1999"
        scraper = WeatherScraper(user_loc, date)
        data_existing = scraper._data_already_retrieved()
        self.assertEqual(data_existing, True)

    def test110_testDataAlreadyRetrieved_FileNotInExistence(self):
        user_loc = "Mobile,AL"
        date = "1/2017"
        scraper = WeatherScraper(user_loc, date)
        data_existing = scraper._data_already_retrieved()
        self.assertEqual(data_existing, False)

    def test115_testDataAlreadyRetrieved_NewMonth(self):
        user_loc = "Mobile,AL"
        date = "3/1999"
        scraper = WeatherScraper(user_loc, date)
        data_existing = scraper._data_already_retrieved()
        self.assertEqual(data_existing, False)

    def test120_testDataAlreadyRetrieved_FileWithMultipleMonths(self):
        user_loc = "Mobile,AL"
        date = "2/1999"
        scraper = WeatherScraper(user_loc, date)
        data_existing = scraper._data_already_retrieved()
        self.assertEqual(data_existing, True)

    def test130_testDataAlreadyRetrieved_PullingEarlierMonth(self):
        user_loc = "Auburn,AL"
        date = "4/2018"
        scraper = WeatherScraper(user_loc, date)
        data_existing = scraper._data_already_retrieved()
        self.assertEqual(data_existing, False)

    # Test __init__()

    def test200_testInit_CheckIfUserIsPullingCurrentMonth(self):
        user_loc = "Mobile,AL"
        date = datetime.today()
        current_month = date.month
        current_year = date.year
        date = "%s/%s" % (current_month, current_year)
        scraper = WeatherScraper(user_loc, date)
        self.assertEqual(scraper.go_ahead, False)
        self.assertEqual(scraper.run(), False)

    def test210_testInit_CheckVariableCreation(self):
        user_loc = "Dallas, TX"
        date = "1/2018"
        scraper = WeatherScraper(user_loc, date)
        self.assertEqual(scraper.city, "Dallas")
        self.assertEqual(scraper.directory, "./data/tx/")
        self.assertEqual(scraper.filename, "./data/tx/txdall18.dat")
        self.assertEqual(scraper.go_ahead, True)
        self.assertEqual(scraper.key, "2dd9e033bfb386fa272686e32b748dda")
        self.assertEqual(scraper.lat, "32.7762719")
        self.assertEqual(scraper.lng, "-96.7968559")
        self.assertEqual(scraper.max_days, 0)
        self.assertEqual(scraper.month, "1")
        self.assertEqual(scraper.state, "Texas")
        self.assertEqual(scraper.state_abbrev, "tx")
        self.assertEqual(scraper.year, "2018")

    def test215_testInit_CheckVariableCreation_DifferentData(self):
        user_loc = "Dallas, TX"
        date = "1 2018"
        scraper = WeatherScraper(user_loc, date)
        self.assertEqual(scraper.city, "Dallas")
        self.assertEqual(scraper.directory, "./data/tx/")
        self.assertEqual(scraper.filename, "./data/tx/txdall18.dat")
        self.assertEqual(scraper.go_ahead, True)
        self.assertEqual(scraper.key, "2dd9e033bfb386fa272686e32b748dda")
        self.assertEqual(scraper.lat, "32.7762719")
        self.assertEqual(scraper.lng, "-96.7968559")
        self.assertEqual(scraper.max_days, 0)
        self.assertEqual(scraper.month, "1")
        self.assertEqual(scraper.state, "Texas")
        self.assertEqual(scraper.state_abbrev, "tx")
        self.assertEqual(scraper.year, "2018")


if __name__ == '__main__':
    unittest.main()
