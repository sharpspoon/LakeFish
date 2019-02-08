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
        date = "3/2018"
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
        user_loc = "Dallas,TX"
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

    # Test _output_data

    def test300_testOutputData_MakeSureFileContainsAllDataPoints(self):
        user_loc = "Mobile,AL"
        date = "2/1980"
        scraper = WeatherScraper(user_loc, date)
        scraper.run()

        with open(scraper.filename, 'r') as f:
            lines = f.readlines()
        months = []
        end_of_list = False
        length = len(lines)
        index = 0
        correct_data = True
        while not end_of_list:
            if length == index:
                end_of_list = True
            elif length < index:
                end_of_list = True
                correct_data = False
            else:
                date = lines[index].split()
                months.append(date[0])
                max_days = int(date[1])
                index += max_days

        self.assertEqual(correct_data, True)

    def test310_testOutputData_CheckForMonthOrder(self):
        user_loc = "Auburn,AL"
        date = "4/2018"
        scraper = WeatherScraper(user_loc, date)
        scraper.run()

        with open(scraper.filename, 'r') as f:
            lines = f.readlines()
        months = []
        end_of_list = False
        length = len(lines)
        index = 0
        month_order_right = True
        while not end_of_list:
            if length <= index:
                end_of_list = True
            else:
                date = lines[index].split()
                months.append(date[0])
                max_days = int(date[1])
                index += max_days

                for x in range(1, len(months)):
                    if int(months[x]) > int(months[x-1]):
                        continue
                    else:
                        month_order_right = False

        self.assertEqual(month_order_right, True)

    def test310_testOutputData_CheckForMonthOrder(self):
        user_loc = "Auburn,AL"
        date = "4/2015"
        scraper = WeatherScraper(user_loc, date)
        scraper.run()

        with open(scraper.filename, 'r') as f:
            lines = f.readlines()
        months = []
        end_of_list = False
        length = len(lines)
        index = 0
        month_order_right = True
        while not end_of_list:
            if length <= index:
                end_of_list = True
            else:
                date = lines[index].split()
                months.append(date[0])
                max_days = int(date[1])
                index += max_days

                for x in range(1, len(months)):
                    if int(months[x]) > int(months[x-1]):
                        continue
                    else:
                        month_order_right = False

        self.assertEqual(month_order_right, True)

if __name__ == '__main__':
    unittest.main()
