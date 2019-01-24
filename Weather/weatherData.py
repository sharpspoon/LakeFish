import requests, bs4, os, time
from geopy.geocoders import Nominatim
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait

# Asks user location and then prints elements with class myforecast-current-lrg
# i am not sure exactly how the geolocater works...i have been doing CITY, STATE i.e. Mobile Alabama and it works
''''userLoc = input("Provide city and state abbreviation. ")
city, state = userLoc.split()
print(city, state)

month, year = input("What month and year would you like to see? (Please provide in numerical format) ").split("/")
print(month, year)
'''

city = 'mobile'
state = 'al'
userLoc = 'mobile alabama'
month = '1'
year = '2019'

geolocator = Nominatim(user_agent="weatherApp")
location = geolocator.geocode(userLoc)

lat = str(location.latitude)
lng = str(location.longitude)

print((location.latitude, location.longitude))



airportURL = "https://airport.globefeed.com/US_Nearest_Airport_Result.asp?lat=" + lat +"&lng=" +lng #"&place=Mobile,%20AL,%20USA
print(airportURL)
res = requests.get(airportURL)
res.raise_for_status()
airport = bs4.BeautifulSoup(res.text, features="html.parser")
airportCells = airport.select('td')
airportCode = airportCells[9].getText()



weatherURL = "https://www.wunderground.com/history/monthly/" + airportCode + "/date/" + year + "-" + month
              
print(weatherURL)

browser = webdriver.Firefox()
browser.get(weatherURL)
try:
    wait = WebDriverWait(webdriver, 10)
    temp = browser.find_element_by_class_name('days')
    print(temp.text)
    print(len(temp.text))
finally:
    browser.quit()
