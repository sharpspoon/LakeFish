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

"""
    weatherData.py
    
    Script will pull weather data based on the location and time provided by the user.
    After data is pulled it will store it in a file located in the under the data directory and under its state
    The file name will be named after the state, city and year that the data is pulled for
    i.e. Mobile Alabama 2019 -> ALMOBI19 located in Data\AL
    
    Created on 1/20/2019
    @author Jordan Sosnowski, Jack Mullins
    
"""

##########################################################################
#             Requests User Intended location and time                   #
##########################################################################


''''userLoc = input("Provide city and state abbreviation. ").upper()
city, state = userLoc.split()
print(city, state)

month, year = input("What month and year would you like to see? (Please provide in numerical format) ").split("/")
print(month, year)
'''

city = 'MOBILE'
state = 'AL'
userLoc = city + " " + state
month = '1'
year = '2019'

geolocator = Nominatim(user_agent="weatherApp")
location = geolocator.geocode(userLoc)

# gets users location's latitude and longitude
lat = str(location.latitude)
lng = str(location.longitude)

print((location.latitude, location.longitude))

##########################################################################
#                        Pulls User Data                                 #
##########################################################################

# using the lat and lng acquired earlier it manipulates the URL of this airport site
airportURL = "https://airport.globefeed.com/US_Nearest_Airport_Result.asp?lat=" + lat + "&lng=" + lng  # "&place=Mobile,%20AL,%20USA

print(airportURL)

# using the requests library the html of the site is pulled and using bs4 the elements of 'td' are pulled
res = get(airportURL)
res.raise_for_status()
airport = BeautifulSoup(res.text, features="html.parser")
airportCells = airport.select('td')
airportCode = airportCells[9].getText() # inside the 9th td element contains the nearest airport's code to the users
                                        # provided location

# the airport code is used to find historical data
weatherURL = "https://www.wunderground.com/history/monthly/" + airportCode + "/date/" + year + "-" + month

print(weatherURL)

# run selenium browser headless
options = Options()
options.headless = True
browser = webdriver.Firefox(options=options)
browser.get(weatherURL)

# selenium was used instead of requests due to the fact that the wunderground site is javascript heavy and takes a
# few seconds to load which does not operate well with requests
try:
    # wait = WebDriverWait(webdriver, 2)
    weatherHTML = browser.page_source
    weather = BeautifulSoup(weatherHTML, features='html.parser')

finally:
    browser.quit()

##########################################################################
#                      Formats User Data                                 #
##########################################################################

days = weather.find("table", {"class": "days"})
cells = days.findChildren("td", recursive=True)
data = []
for cell in cells:
    if "\n" not in cell:    # ignores garbage data containing \n
        data.append(cell.text.strip())


now = datetime.datetime.now()
maxDays = calendar.monthrange(int(year), int(month))[1]

# if month and year choosen is present then the max number of days
# may not be available to be pulled
if int(month) == now.month and int(year) == now.year:
    day = now.day
else:
    day = maxDays

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

    if column == self.dataset[0]:      # time column only has one header where as the other columns have max,avg, and min
        section = day
        column['data'] = data[:section]
        data = data[section:]
    else:
        section = 3 * day + 3
        unsplitData = data[:section]
        data = data[section:]
        column['data'] = {unsplitData[0]: [],
                          unsplitData[1]: [],
                          unsplitData[2]: []}
        unsplitData = unsplitData[3:]
        length = len(unsplitData)
        for n in range(0, length, 3):
            column['data']['Max'].append(unsplitData[n])
            column['data']['Avg'].append(unsplitData[n + 1])
            column['data']['Min'].append(unsplitData[n + 2])

##########################################################################
#                        Outputs User Data                               #
##########################################################################

directory = "./Data/" + state + "/"
file = state + city[:4] + str(year)[2:] + ".dat"
filename = directory + file
if not os.path.exists(os.path.dirname(filename)):
    try:
        os.makedirs(os.path.dirname(filename))
    except OSError as exc:  # Guard against race condition
        if exc.errno != errno.EEXIST:
            raise

with open(filename, 'a+', newline='') as f:
    header = str(month) + " " + str(maxDays) + " " + str(year) + "\n"
    f.write(header)
    writer = csv.writer(f, delimiter='\t')

    final = zip(
        # self.dataset[0]['data'],         # time

        # self.dataset[1]['data']['Max'],
        self.dataset[1]['data']['Avg'],  # Temp
        # self.dataset[1]['data']['Min'],

        # self.dataset[2]['data']['Max'],
        self.dataset[2]['data']['Avg'],  # Dew
        # self.dataset[2]['data']['Min'],

        # self.dataset[3]['data']['Max'],
        self.dataset[3]['data']['Avg'],  # Hum
        # self.dataset[3]['data']['Min'],

        # self.dataset[4]['data']['Max'],
        self.dataset[4]['data']['Avg'],  # Wind
        # self.dataset[4]['data']['Min'],

        # self.dataset[5]['data']['Max'],
        self.dataset[5]['data']['Avg'],  # Pressure
        # self.dataset[5]['data']['Min'],

        # self.dataset[6]['data']['Max'],
        self.dataset[6]['data']['Avg'],  # Precipitation
        # self.dataset[6]['data']['Min'],
    )
    writer.writerows(final)

print("done")
