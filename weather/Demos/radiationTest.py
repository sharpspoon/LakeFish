from geopy.geocoders import Nominatim
import pandas as pd

# limit: 300 requests/day
# 1 request per second

geolocator = Nominatim(user_agent="weatherApp")
location = geolocator.geocode("Dallas, TX")

latString = str(location.latitude)
longString = str(location.longitude)

api_key = 'G4wQHl0XIAUEvhEyTAkym5EWovi0ptlEo6QAkxE6'
attributes = 'ghi'
year = '2010'
leap = 'false'
interval = '60'
utc = 'false'
name = 'Jack+Mullins'
reason = 'lake+modeling'
affiliation = 'Auburn+University'
email = 'wjm0020@auburn.edu'
mailing_list = 'false'

url = '''http://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv?wkt=POINT({lon}%20{lat})&names={year}&leap_day={leap}&interval={interval}&utc={utc}&full_name={name}&email={email}&affiliation={affiliation}&mailing_list={mailing_list}&reason={reason}&api_key={api}&attributes={attr}'''.format(year=year,
    lat=latString, lon=longString, leap=leap, interval=interval, 
    utc=utc, name=name, email=email, mailing_list=mailing_list, 
    affiliation=affiliation, reason=reason, api=api_key, attr=attributes)

print(url)

info = pd.read_csv(url, nrows=1)
print(info['Local Time Zone'])