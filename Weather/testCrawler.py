# Code Adapted From No Starch's "Automate Boring Stuff" https://automatetheboringstuff.com/chapter11/

import requests, bs4, os

# pulls a local file and parses through it printing elements with id of author and elements with tag of p
exampleFile = open('example.html','r')
exampleSoup = bs4.BeautifulSoup(exampleFile.read(), features="html.parser")
elems = exampleSoup.select('#author')
print(elems[0].getText())

pElements = exampleSoup.select('p')
for p in pElements:
    print(p.getText()) # loops through p tags and prints their contents



# pulls a website down and prints elements with class myforecast-current-lrg
res = requests.get('https://forecast.weather.gov/MapClick.php?lat=32.60922640000007&lon=-85.49322619999998')
res.raise_for_status()
weather = bs4.BeautifulSoup(res.text, features="html.parser")
temp = weather.select('.myforecast-current-lrg')
print(temp[0].getText()) # prints temperature