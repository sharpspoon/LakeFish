import calendar
import errno
import os
import time
import random
from geopy.geocoders import Nominatim
from requests import get
from math import sin, asin, cos
from datetime import date, datetime


"""
    weatherscraper.py

    Script will pull weather data based on the location and time provided by the user.
    Script is provided latitude and longitude through the library geopy using Nominatim's API
    Achieves this by using the Dark Sky API

    After data is pulled it will store it in a file located in the under the data directory and under its state
    The file name will be named after the state, city and year that the data is pulled for
    i.e. Mobile Alabama 2019 -> "almobi9" located in weather_data/al

    Created on 2/1/2019
    @author Jordan Sosnowski, Jack Mullins

"""


class WeatherScraper:
    def __init__(self, user_loc, date):

        # sets up users request location
        geolocator = Nominatim(user_agent="test")
        location = geolocator.geocode(user_loc)
        self.lat = str(location.latitude)
        self.lng = str(location.longitude)
        self.state = location.address.split(',')[2].strip()
        self.city = location.address.split(',')[0].strip()

        # Dark Sky API Key
        a = random.randint(0, 2)
        if a == 0:
            # self.key = "2dd9e033bfb386fa272686e32b748dda"
            self.key = "bd13ad063cdbb5d416accbd8652ef28d"
        elif a == 1:
            self.key = "bd13ad063cdbb5d416accbd8652ef28d"
        elif a == 2:
            self.key = "d05065b58ee0775eb0a72fadb7d09aa9"

        # sets up users date information
        dates = [x.strip() for x in date.split('/')]
        if len(dates) != 2:
            dates = [x.strip() for x in date.split()]
        if len(dates) != 2:
            self.go_ahead = False
            print("Error: Incorrect Data Format")
            return
        self.month = dates[0]
        self.year = dates[1]
        self.max_days = 0
        date = datetime.today()
        if self.month == str(date.month) and self.year == str(date.year):
            print("Error: User cannot enter current date")
            self.go_ahead = False
            return

        self.go_ahead = True
        self.out_of_order = False

        # sets up data structures to hold weather info
        self.times = []
        self.weather_responses = []  # get(weather_url)
        self.weather_jsons = []  # self.weather_response.json()
        self.weather_headers = []  # self.weather_response.headers

        # if user did not provide the state abbrev, abbrev it
        if len(self.state) != 2:
            self.state_abbrev = state_to_abbrev(self.state).lower()
        else:
            self.state_abbrev = self.state.lower()

        city_abbrv = self.city[:4].lower()
        year_abbrv = self.year[2:]

        # sets up directory that file will end up in
        self.directory = "./weather_data/" + self.state_abbrev + "/"
        self.filename = self.directory + self.state_abbrev + \
            city_abbrv + year_abbrv + ".dat"

        self.data_retrieved = self._data_already_retrieved()

    def get_file_path(self):
        # if file is already existing pull its filename
        if self.data_retrieved:
            return self.filename
        else:
            return "File for this specific date and location not in database..."

    def run(self):
        if self.go_ahead:
            if self.data_retrieved:
                print("No need to pull additional data...")
            else:
                self._generate_all_times_for_month()
                print("Pulling data...")
                self._pull_data()
                print("Formatting data...")
                data = self._format_data()
                print("Outputting data...")
                self._output_data(data)
            return True
        else:
            print("Error: Go Ahead is not cleared, Check prior error message")
            return False

    def _generate_all_times_for_month(self):
        """
            Generates UNIX Timestamp for every day for the given month of a specific year
        """
        month_info = calendar.monthrange(int(self.year),
                                         int(self.month))  # contains month number[0] and month max days[1]
        self.max_days = month_info[1]  # gets max days from current month
        for day in range(1, self.max_days + 1):
            dt = datetime(int(self.year), int(self.month),
                          day)  # creates a datetime object for the users specific month and year
            # converts the datetime object into the UNIX format
            self.times.append(str(int(time.mktime(dt.timetuple()))))

    def _pull_data(self):
        """
            Grabs the weather data of the specified location for a whole month using Dark Sky's API
        """
        for time in self.times:
            weather_url = "https://api.darksky.net/forecast/%s/%s,%s,%s" % (
                self.key, self.lat, self.lng, time)
            # print(weather_url)
            weather_response = get(weather_url)
            self.weather_responses.append(weather_response)
            self.weather_jsons.append(weather_response.json())
            self.weather_headers.append(weather_response.headers)

    def _format_data(self):
        """
            Pulls data needed for lakefish application from the json objects provided by Dark Sky
        """
        weather_info = []
        day_counter = 1
        for day in self.weather_jsons:
            info = day['daily']['data'][0]
            keys = ['dewPoint', 'windSpeed', 'windBearing', 'solarRad', 'cloudCover',
                    'precipIntensity', 'precipAccumulation']
            data = []
            hours = day['hourly']['data']
            apparent_temp = 0
            solar_rad = []
            num_of_hours = len(hours)
            for hour in hours:
                if "temperature" in hour:
                    apparent_temp += float(hour['temperature'])
                elif "apparentTemperature" in hour:
                    apparent_temp += float(hour['apparentTemperature'])

                # calulate solar radition
                cloud_cov = hour['cloudCover']
                utc_time = hour['time']
                hour = datetime.fromtimestamp(int(utc_time)).strftime('%X').split(":")[0]
                rad = self.solar_rad_cal(cloud_cov, day_counter, hour)
                solar_rad.append(rad)
            avg_temp = round(apparent_temp / float(num_of_hours), 1)
            avg_rad = round(sum(solar_rad) / len(solar_rad), 1)
            data.append(avg_temp)
            for key in keys:
                if key in info:
                    if key == 'cloudCover':
                        data.append(round((1 - info[key]) * 100.0, 1))
                    elif key == 'precipAccumulation':
                        # converts inches to meters
                        data.append(round(info[key] * .0254, 1))
                    else:
                        data.append(round(float(info[key]), 1))
                else:
                    data.append(0.0)
            data.insert(4, avg_rad)
            weather_info.append(data)
            day_counter += 1
        return weather_info

    def jday_calc(self, day, hour):
        # Value used in calculation of solar raditaion
        unformatted_date = str(self.year) + str(self.month) + str(day)
        date = datetime.strptime(unformatted_date, "%Y%m%d")
        days = date.strftime("%j")

        jday1 = float(days) - 1 + (float(hour) / 24)

        return round(jday1, 2)

    def solar_rad_cal(self, solcov, day, hour):
        slong = float(self.lng)
        slat = float(self.lat)
        pi = 3.1419
        phi = 15 * round(slong / 15.0)

        jday1 = self.jday_calc(day, hour)
        hcloud = solcov
        pi = 3.1419
        eqt = 0.17 * sin(4 * pi * (int(jday1) - 80) / 373) - 0.129 * sin(2 * pi * int((jday1) - 8) / 355)
        hour1 = round(24 * (jday1 - int(jday1)))  # good
        hh = round(2 * pi / 24 * (hour1 - ((slong - phi) * 24 / 360) + eqt - 12.0), 6)
        taud = 2 * pi * (int(jday1) - 1) / 365
        val = round(0.006918 - 0.399912 * cos(taud) + 0.070257 * sin(taud) - 0.006758 * cos(2 * taud) + 0.000907 * sin(2 * taud) - 0.002697 * cos(3 * taud) + 0.00148 * sin(3 * taud), 7)
        f_sin = sin(slat * 0.01743)
        d_sin = sin(val)
        s_cos = cos(slat * 0.01743)
        d_cos = cos(val)
        h_cos = cos(hh)
        a_asin = f_sin * d_sin + s_cos * d_cos * h_cos
        ao = asin(a_asin)
        ao = ao * 180 / pi
        if ao < 0:
            phis = 0
            sro = 0
        else:
            phis = 24 * (2.044 * ao + 0.1296 * ao**2 - 1.941 * 0.001 * ao**3 + 7.591 * 0.000001 * ao**4) * 0.1314
            sro = (1 - 0.65 * (hcloud)**2) * phis
        return sro

    def _output_data(self, data):
        """
            Outputs the data to a .dat file with the notation of STATECITYYEAR i.e ALMOBI99
        """

        check_for_path(self.filename)
        months = [''] * 12
        if self.out_of_order:
            with open(self.filename, 'r') as f:
                file_info = f.readlines()
            while len(file_info) is not 0:      # sorts data in file
                date = file_info[0].split()
                month = int(date[0])
                max_days = int(date[1])
                months[month] = file_info[:max_days + 1]
                file_info = file_info[max_days + 1:]
            write_type = "w"
        else:
            write_type = "a+"

        temp_list = []
        self.max_days = calendar.monthrange(int(self.year), int(self.month))[1]
        header = str(self.month) + " " + str(self.max_days) + \
            " " + str(self.year) + "\n"
        temp_list.append(header)
        for day in data:
            temp_string = ""
            for value in day:
                if value == day[0]:
                    temp_string += (str(value))
                else:
                    temp_string += ("%8s" % str(value))
            temp_string += "\n"
            temp_list.append(temp_string)

        months[int(self.month)] = temp_list

        with open(self.filename, write_type, newline='') as f:
            for month in months:
                for line in month:
                    f.write(line)

    def _data_already_retrieved(self):
        """
            Checks to make sure data is not already stored
        """
        if not os.path.isfile(self.filename):
            print("No such file.")
            return False

        with open(self.filename, "r") as datafile:
            lines = datafile.readlines()

        file_not_done = True

        while file_not_done:
            if len(lines) == 0:  # month not already pulled, go ahead will pulling data
                print("Month not located in database...")
                return False
            date = lines[0].split()
            max_days = int(date[1])
            if date[0] == self.month:   # month located in database, dont pull new data
                print("Month located in database...")
                return True
            # month not currently located, but could be later on in the file, continue searching
            elif date[0] < self.month:
                lines = lines[max_days + 1:]
            elif date[0] > self.month:  # older or earlier months are in file, but not this month
                print("Month not located in database...")
                self.out_of_order = True
                return False

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


def state_to_abbrev(state):
    # Utitlity script, allows a conversion between state and its abbreviation
    us_state_abbrev = {
        'Alabama': 'AL',
        'Alaska': 'AK',
        'Arizona': 'AZ',
        'Arkansas': 'AR',
        'California': 'CA',
        'Colorado': 'CO',
        'Connecticut': 'CT',
        'Delaware': 'DE',
        'Florida': 'FL',
        'Georgia': 'GA',
        'Hawaii': 'HI',
        'Idaho': 'ID',
        'Illinois': 'IL',
        'Indiana': 'IN',
        'Iowa': 'IA',
        'Kansas': 'KS',
        'Kentucky': 'KY',
        'Louisiana': 'LA',
        'Maine': 'ME',
        'Maryland': 'MD',
        'Massachusetts': 'MA',
        'Michigan': 'MI',
        'Minnesota': 'MN',
        'Mississippi': 'MS',
        'Missouri': 'MO',
        'Montana': 'MT',
        'Nebraska': 'NE',
        'Nevada': 'NV',
        'New Hampshire': 'NH',
        'New Jersey': 'NJ',
        'New Mexico': 'NM',
        'New York': 'NY',
        'North Carolina': 'NC',
        'North Dakota': 'ND',
        'Ohio': 'OH',
        'Oklahoma': 'OK',
        'Oregon': 'OR',
        'Pennsylvania': 'PA',
        'Rhode Island': 'RI',
        'South Carolina': 'SC',
        'South Dakota': 'SD',
        'Tennessee': 'TN',
        'Texas': 'TX',
        'Utah': 'UT',
        'Vermont': 'VT',
        'Virginia': 'VA',
        'Washington': 'WA',
        'West Virginia': 'WV',
        'Wisconsin': 'WI',
        'Wyoming': 'WY',
    }
    return us_state_abbrev[state]
