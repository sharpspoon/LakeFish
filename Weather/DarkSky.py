from geopy.geocoders import Nominatim
import datetime
import time
from requests import get
import calendar
from state_to_abbrev import state_to_abbrev
import csv 
import os

class MonthlyHistoricalData:
    def __init__(self, user_loc, date):
        geolocator = Nominatim(user_agent="weatherApp")
        location = geolocator.geocode(user_loc)
        self.lat = str(location.latitude)
        self.lng = str(location.longitude)
        self.state = location.address.split(',')[2].strip()
        self.city = location.address.split(',')[0].strip()
        self.key = "2dd9e033bfb386fa272686e32b748dda"
        dates = [x.strip() for x in date.split('/')]
        self.month = dates[0]
        self.year = dates[1]
        self.weather_url = ""
        
        self.times = []

        self.weather_responses = [] #get(weather_url)
        self.weather_jsons = [] #self.weather_response.json()
        self.weather_headers = [] #self.weather_response.headers

        if len(self.state) != 2:
            self.state_abbrev = state_to_abbrev(self.state)
        else:
            self.state_abbrev = self.state
        city_abbrv = self.city[:4].upper()
        year_abbrv = self.year[2:]
        self.directory = "./Data/" + self.state_abbrev + "/"
        self.filename = self.directory + self.state_abbrev + city_abbrv + year_abbrv + ".dat"

    def generate_all_times_for_month(self):
        self.max_days = calendar.monthrange(int(self.year), int(self.month))[1]
        for day in range(1, self.max_days):
            dt = datetime.datetime(int(self.year), int(self.month), day)
            self.times.append(str(int(time.mktime(dt.timetuple()))))

    def grab_weather_data(self):
        start = time.time()
        for t in self.times:
            weather_url = "https://api.darksky.net/forecast/%s/%s,%s,%s" % (self.key, self.lat, self.lng, t)
            weather_response = get(weather_url)
            self.weather_responses.append(weather_response)
            self.weather_jsons.append(weather_response.json())
            self.weather_headers.append(weather_response.headers)
        days = []
        for day in self.weather_jsons:
            info = day['daily']['data'][0]
            keys = ['temperatureMax', 'dewPoint', 'windSpeed', 'windBearing', 'uvIndex', 'cloudCover', 'precipIntensity', 'precipAccumulation']
            data = [] 
            for key in keys:
                if key in info:
                    if key == 'cloudCover':
                        data.append(round((1 - info[key]) * 100.0,1))
                    elif key == 'precipAccumulation':
                        data.append(round(info[key] * .0254,1)) # converts inches to meters
                    else:
                        data.append(round(float(info[key]),1))
                else:
                    data.append(0.0)
            days.append(data)
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
                        f.write("%8s" %  str(value) )
                f.write("\n")

        print("Took ", time.time() - start, " to run")


    def output_daily_data(self):
        for day in self.weather_jsons:
            print(day['daily'])

    def demo_grab_two_day(self):
        start = time.time()
        for day in range(1, 3):
            dt = datetime.datetime(int(self.year), int(self.month), day)
            self.times.append(str(int(time.mktime(dt.timetuple()))))
        for t in self.times:
            weather_url = "https://api.darksky.net/forecast/%s/%s,%s,%s" % (self.key, self.lat, self.lng, t)
            weather_response = get(weather_url)
            self.weather_responses.append(weather_response)
            self.weather_jsons.append(weather_response.json())
            self.weather_headers.append(weather_response.headers)
        days = []
        for day in self.weather_jsons:
            info = day['daily']['data'][0]
            keys = ['temperatureMax', 'dewPoint', 'windSpeed', 'windBearing', 'uvIndex', 'cloudCover', 'precipIntensity', 'precipAccumulation']
            data = [] 
            for key in keys:
                if key in info:
                    if key == 'cloudCover':
                        data.append(round((1 - info[key]) * 100,1))
                    elif key == 'precipAccumulation':
                        data.append(round(info[key] * .0254,1)) # converts inches to meters
                    else:
                        data.append(round(info[key],1))
                else:
                    data.append(0.0)
            days.append(data)
        with open(self.filename, 'w+', newline='') as f:
            self.max_days = calendar.monthrange(int(self.year), int(self.month))[1]
            header = str(self.month) + " " + str(self.max_days) + " " + str(self.year) + "\n"
            f.write(header)
            for day in days:
                for value in day:
                    if value == day[0]:
                        f.write(str(value))
                    else:
                        f.write("%8s" %  str(value) )
                f.write("\n")

        print("Took ", time.time() - start, " to run")

    def _data_already_retrieved(self):
        if (not os.path.isfile(self.filename)):
            print("No such file.")
            return False
        
        with open(self.filename, "r") as datafile:
            lines = datafile.readlines()
        
        daysToSkip = 0
        for line in lines:
            if daysToSkip > 0:
                daysToSkip = daysToSkip - 1
                continue

            date = line.split()
            if date[0] == self.month:
                print("Matching month found.")
                return True
            else:
                print(f"Skipping month {date[1]}")
                daysToSkip = int(date[1])

        return False

def check_for_path(filename):
    if not os.path.exists(os.path.dirname(filename)):
        try:
            os.makedirs(os.path.dirname(filename))
        except OSError as exc:  # Guard against race condition
            if exc.errno != errno.EEXIST:
                raise