# Code Adapted From No Starch's "Automate Boring Stuff" https://automatetheboringstuff.com/chapter11/

from bs4 import BeautifulSoup
from geopy.geocoders import Nominatim
from requests import get

# Asks user location and then prints elements with class myforecast-current-lrg
# i am not sure exactly how the geolocater works...i have been doing CITY, STATE i.e. Mobile Alabama and it works
geolocator = Nominatim(user_agent="weatherApp")
userLoc = input("What is your location? ")
location = geolocator.geocode(userLoc)
lat = str(location.latitude)
lon = str(location.longitude)
weatherURL = "https://forecast.weather.gov/MapClick.php?lat=" + lat + "&lon=" + lon
# print(weatherURL)
res = get(weatherURL)
res.raise_for_status()
weather = BeautifulSoup(res.text, features="html.parser")
temp = weather.select('.myforecast-current-lrg')
print(temp[0].getText())  # prints temperature
