import calendar
import csv
import datetime
import errno
import os

from bs4 import BeautifulSoup
from geopy.geocoders import Nominatim
from requests import get
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from state_to_abbrev import state_to_abbrev
"""
    WeatherScraper.py
    
    Script will pull weather data based on the location and time provided by the user.
    After data is pulled it will store it in a file located in the under the data directory and under its state
    The file name will be named after the state, city and year that the data is pulled for
    i.e. Mobile Alabama 2019 -> "ALMOBI19" located in Data\AL
    
    Created on 1/20/2019
    Edited on 1/25/2019 Data outputs neatly to file
    Edited on 1/28/2019 Refactored code into classical approach
    @author Jordan Sosnowski, Jack Mullins
    
"""


class WeatherScraper:
    def __init__(self, user_input="Auburn Alabama", month=datetime.datetime.now().month,
                 year=datetime.datetime.now().year):
        self.city, self.state = user_input.split(',')
        self.city = self.city.strip()
        self.state = self.state.strip()
        if len(self.state) != 2:
            self.state_abbrev = state_to_abbrev(self.state)
        else:
            self.state_abbrev = self.state
        user_loc = self.city + "," + self.state_abbrev
        geolocator = Nominatim(user_agent="weatherApp")
        self.location = geolocator.geocode(user_loc)
        # gets users location's latitude and longitude
        self.lat = str(self.location.latitude)
        self.lng = str(self.location.longitude)
        self.month = month
        self.year = year
        self.weather_page = None
        self.airport_code = None
        self.max_days = None
        self.dataset = None

    def _pull_data(self):
        """
            First finds airport code based on location's lng and lat. Airport code is used to find the correct URL
            to pull the historical weather data
        """

        self._get_airport_code()
        self._get_weather_data()
        #self.check_airport_code()

    def _get_airport_code(self):
        """
            Finds airport code based on lng and lat provided by geopy
        """

        airport_url = "https://airport.globefeed.com/US_Nearest_Airport_Result.asp?lat=" + self.lat + "&lng=" + self.lng

        # using the requests library the html of the site is pulled and using bs4 the elements of 'td' are pulled
        airport_page = get(airport_url)
        airport_page.raise_for_status()

        airport_parsed = BeautifulSoup(airport_page.text, features="html.parser")
        airport_cells = airport_parsed.select('td')
        # inside the 9th td element contains the nearest airport's code to the users provided location
        self.airport_code = airport_cells[9].getText()

    def check_airport_code(self):
        heading = self.weather_page.find_all("h1")
        location = heading[0].text
        state = location.split(',')[1].strip()
        if self.state_abbrev == state:
            return True
        else:
            return False

    def _get_weather_data(self):
        """
            Based on the airport code and month and year wunderground will provide weather data. After the data loads
            BS4 will save the page
        """

        weather_url = "https://www.wunderground.com/history/monthly/" + self.airport_code + "/date/" + self.year + "-" \
                      + self.month

        # run selenium browser headless
        options = Options()
        options.headless = True

        browser = webdriver.Firefox(options=options)
        browser.get(weather_url)

        # selenium was used instead of requests due to the fact that the wunderground site is javascript heavy and
        # takes a few seconds to load which does not operate well with requests
        try:
            # wait = WebDriverWait(webdriver, 2)
            weather_html = browser.page_source
            self.weather_page = BeautifulSoup(weather_html, features='html.parser')

        finally:
            browser.quit()

    def _format_data(self):
        ##########################################################################
        #                      Formats User Data                                 #
        ##########################################################################
        days = self.weather_page.find("table", {"class": "days"})
        cells = days.findChildren("td", recursive=True)
        data = []
        for cell in cells:
            if "\n" not in cell:  # ignores garbage data containing \n
                data.append(cell.text.strip())
        now = datetime.datetime.now()
        self.max_days = calendar.monthrange(int(self.year), int(self.month))[1]
        # if month and year chosen is present then the max number of days
        # may not be available to be pulled
        if int(self.month) == now.month and int(self.year) == now.year:
            days = now.day
        else:
            days = self.max_days
        # sets up a dictionary to hold the needed data from the weather site
        self.dataset = [
            {'name': data[0]},  # day
            {'name': data[1]},  # temp
            {'name': data[2]},  # dew
            {'name': data[3]},  # humidity
            {'name': data[4]},  # wind speed
            {'name': data[5]},  # pressure
            {'name': data[6]}]
        data = data[8:]
        for column in self.dataset:

            if column == self.dataset[0]:  # time column only has one header where as the other columns have max,avg, and min
                section = days
                column['data'] = data[:section]
                data = data[section:]
            else:
                section = 3 * days + 3
                unsplit_data = data[:section]
                data = data[section:]
                column['data'] = {unsplit_data[0]: [],
                                  unsplit_data[1]: [],
                                  unsplit_data[2]: []}
                unsplit_data = unsplit_data[3:]
                length = len(unsplit_data)
                for n in range(0, length, 3):
                    column['data']['Max'].append(unsplit_data[n])
                    column['data']['Avg'].append(unsplit_data[n + 1])
                    column['data']['Min'].append(unsplit_data[n + 2])

    def _output_data(self):
        ##########################################################################
        #                        Outputs User Data                               #
        ##########################################################################
        
        city_abbrv = self.city[:4].upper()
        year_abbrv = self.year[2:]
        directory = "./Data/" + self.state_abbrev + "/"
        file = self.state_abbrev + city_abbrv + year_abbrv + ".dat"
        filename = directory + file
        check_for_path(filename)
        with open(filename, 'a+', newline='') as f:
            header = str(self.month) + " " + str(self.max_days) + " " + str(self.year) + "\n"
            f.write(header)
            writer = csv.writer(f, delimiter='\t')

            final = zip(
                self.dataset[1]['data']['Avg'],  # Temp
                self.dataset[2]['data']['Avg'],  # Dew
                self.dataset[3]['data']['Avg'],  # Hum
                self.dataset[4]['data']['Avg'],  # Wind
                self.dataset[5]['data']['Avg'],  # Pressure
                self.dataset[6]['data']['Avg'],  # Precipitation
            )
            writer.writerows(final)
        print("done")

    def run(self):
        print("Pulling Data...")
        self._pull_data()
        print("Formatting Data...")
        self._format_data()
        print("Outputting Data...")
        self._output_data()


def check_for_path(filename):
    if not os.path.exists(os.path.dirname(filename)):
        try:
            os.makedirs(os.path.dirname(filename))
        except OSError as exc:  # Guard against race condition
            if exc.errno != errno.EEXIST:
                raise
