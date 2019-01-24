# Code Adapted From No Starch's "Automate Boring Stuff" https://automatetheboringstuff.com/chapter11/

import requests, bs4, os
from geopy.geocoders import Nominatim

# Asks user location and then prints elements with class myforecast-current-lrg
# i am not sure exactly how the geolocater works...i have been doing CITY, STATE i.e. Mobile Alabama and it works
geolocator = Nominatim(user_agent="weatherApp")
userLoc = input("What is your location? ")
location = geolocator.geocode(userLoc)
lat = str(location.latitude)
lon = str(location.longitude)
weatherURL = "https://forecast.weather.gov/MapClick.php?lat=" + lat + "&lon=" + lon
#print(weatherURL)
res = requests.get(weatherURL)
res.raise_for_status()
weather = bs4.BeautifulSoup(res.text, features="html.parser")
temp = weather.select('.myforecast-current-lrg')
print(temp[0].getText()) # prints temperature