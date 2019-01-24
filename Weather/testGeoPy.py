from geopy.geocoders import Nominatim

geolocator = Nominatim(user_agent="weatherApp")
userLoc = input("Where are you? ")
location = geolocator.geocode(userLoc)


print((location.latitude, location.longitude))