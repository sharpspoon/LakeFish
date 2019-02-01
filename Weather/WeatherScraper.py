import calendar
import datetime
import errno
import os
import time

from geopy.geocoders import Nominatim
from requests import get

from Weather.StateToAbbrev import state_to_abbrev

"""
    WeatherScraper.py
    
    Script will pull weather data based on the location and time provided by the user.
    Achieves this by using the Dark Sky API

    After data is pulled it will store it in a file located in the under the data directory and under its state
    The file name will be named after the state, city and year that the data is pulled for
    i.e. Mobile Alabama 2019 -> "ALMOBI19" located in Data/AL
    
    Created on 2/1/2019
    @author Jordan Sosnowski, Jack Mullins
    
"""


class WeatherScraper:
    def __init__(self, user_loc, date):

        # sets up users request location
        geolocator = Nominatim(user_agent="weatherApp")
        location = geolocator.geocode(user_loc)
        self.lat = str(location.latitude)
        self.lng = str(location.longitude)
        self.state = location.address.split(',')[2].strip()
        self.city = location.address.split(',')[0].strip()

        # Dark Sky API Key
        self.key = "2dd9e033bfb386fa272686e32b748dda"

        # sets up users date information
        dates = [x.strip() for x in date.split('/')]
        self.month = dates[0]
        self.year = dates[1]
        self.max_days = 0

        # sets up data structures to hold weather info
        self.times = []
        self.weather_responses = []  # get(weather_url)
        self.weather_jsons = []  # self.weather_response.json()
        self.weather_headers = []  # self.weather_response.headers

        # if user did not provide the state abbrev, abbrev it
        if len(self.state) != 2:
            self.state_abbrev = state_to_abbrev(self.state)
        else:
            self.state_abbrev = self.state

        city_abbrv = self.city[:4].upper()
        year_abbrv = self.year[2:]

        # sets up directory that file will end up in
        self.directory = "./Data/" + self.state_abbrev + "/"
        self.filename = self.directory + self.state_abbrev + city_abbrv + year_abbrv + ".dat"

    def run(self):
        if self._data_already_retrieved():
            print("Data already retrieved.")
        else:
            self._generate_all_times_for_month()
            print("Pulling Data...")
            self._pull_data()
            print("Formatting Data...")
            days = self._format_data()
            print("Outputting Data...")
            self._output_data(days)

    def _generate_all_times_for_month(self):
        """
            Generates UNIX Timestamp for every day for the given month of a specific year
        """
        month_info = calendar.monthrange(int(self.year),
                                         int(self.month))  # contains month number[0] and month max days[1]
        self.max_days = month_info[1]  # gets max days from current month
        for day in range(1, self.max_days):
            dt = datetime.datetime(int(self.year), int(self.month),
                                   day)  # creates a datetime object for the users specific month and year
            self.times.append(str(time.mktime(dt.timetuple())))  # converts the datetime object into the UNIX format

    def _pull_data(self):
        """
            Grabs the weather data of the specified location for a whole month using Dark Sky's API
        """
        for t in self.times:
            weather_url = "https://api.darksky.net/forecast/%s/%s,%s,%s" % (self.key, self.lat, self.lng, t)
            weather_response = get(weather_url)
            self.weather_responses.append(weather_response)
            self.weather_jsons.append(weather_response.json())
            self.weather_headers.append(weather_response.headers)

    def _format_data(self):
        """
            Pulls data needed for lakefish application from the json objects provided by Dark Sky
        """
        days = []
        for day in self.weather_jsons:
            info = day['daily']['data'][0]
            keys = ['temperatureMax', 'dewPoint', 'windSpeed', 'windBearing', 'uvIndex', 'cloudCover',
                    'precipIntensity', 'precipAccumulation']
            data = []
            for key in keys:
                if key in info:
                    if key == 'cloudCover':
                        data.append(round((1 - info[key]) * 100.0, 1))
                    elif key == 'precipAccumulation':
                        data.append(round(info[key] * .0254, 1))  # converts inches to meters
                    else:
                        data.append(round(float(info[key]), 1))
                else:
                    data.append(0.0)
            days.append(data)
        return days

    def _output_data(self, days):
        """
            Outputs the data to a .dat file with the notation of STATECITYYEAR i.e ALMOBI99
        """
        check_for_path(self.filename)
        with open(self.filename, 'w+', newline='') as f:
            self.max_days = calendar.monthrange(int(self.year), int(self.month))[1]
            header = str(self.month) + " " + str(self.max_days) + " " + str(self.year) + "\n"
            f.write(header)
            for day in days:
                for value in day:
                    if value == day[0]:
                        f.write(str(value))
                    else:
                        f.write("%8s" % str(value))
                f.write("\n")

    def _data_already_retrieved(self):
        """
            Checks to make sure data is not already stored
        """
        if not os.path.isfile(self.filename):
            print("No such file.")
            return False

        with open(self.filename, "r") as datafile:
            lines = datafile.readlines()

        days_to_skip = 0
        for line in lines:
            if days_to_skip > 0:
                days_to_skip = days_to_skip - 1
                continue

            date = line.split()
            if date[0] == self.month:
                print("Matching month found.")
                return True
            else:
                print(f"Skipping month {date[1]}")
                days_to_skip = int(date[1])

        return False


def check_for_path(filename):
    """
        Checks to see if path exists for new file, if not create needed directories
    """
    if not os.path.exists(os.path.dirname(filename)):
        try:
            os.makedirs(os.path.dirname(filename))
        except OSError as exc:  # Guard against race condition
            if exc.errno != errno.EEXIST:
                raise
