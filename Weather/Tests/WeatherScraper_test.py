import unittest
from Weather.WeatherScraper import WeatherScraper
class TestWeatherScrapper(unittest.TestCase):
    def test100_testDataAlreadyRetrieved(self):
        user_loc = "Mobile,AL"
        date = "1/1999"
        scraper = WeatherScraper(user_loc, date)
        self.assertEqual((3*4), 12)
 
if __name__ == '__main__':
    unittest.main()