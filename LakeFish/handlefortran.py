'''
Created on Feb 17, 2019

@author: Tyler
'''
import sys, string, os, configparser
#os.system("C:/Documents and Settings/flow_model/flow.exe")


def createInitFile(parm={}):
    httpResponse = {}
    if((not('enddate' in parm)) | (not('startdate' in parm)) | (not('location' in parm))):
        httpResponse['status'] = 'error: missing parameters'
        return httpResponse
    startDate = parm['startdate']
    startDate = handleDates(startDate)
    endDate = parm['enddate']
    endDate = handleDates(endDate)
    location = parm['location']
    location = handleLocation(location, startDate[2])
    with open('INPUT.INI', 'w') as configfile:
        configfile.write("Simulation for lake : Waconia, for user : fangxu@hal.lamar.edu\r\n")
        configfile.write("%s\r\n" % location)
        configfile.write("32  1\r\n")
        configfile.write("%s  %s  %s  %s  %s  %s\r\n" % (startDate[0], startDate[1], startDate[2], endDate[0], endDate[1], endDate[2]))
    pathFile = open("path.txt", "w")
    pathFile.write("D:\Lake2019\\\r\n")
    pathFile.write("D:\Lake2019\MeteorologicalData\%s\\\r\n" % location[0:2])
    return httpResponse

def handleDates(date):
    newDate = [0 for _ in range(3)]
    if (date[:1] == '0'):
        newDate[0] = date[1:2]
    else:
        newDate[0] = date[:2]
    newDate[1] = date[2:4]
    newDate[2] = date[4:]
    return newDate

def handleLocation(location, startYear):
    spaceLoc = location.find(",")
    cityName = location[:min(spaceLoc, 4)].upper()
    stateName = location[spaceLoc+2:]
    states = {
        'AK': 'Alaska',
        'AL': 'Alabama',
        'AR': 'Arkansas',
        'AS': 'American Samoa',
        'AZ': 'Arizona',
        'CA': 'California',
        'CO': 'Colorado',
        'CT': 'Connecticut',
        'DC': 'District of Columbia',
        'DE': 'Delaware',
        'FL': 'Florida',
        'GA': 'Georgia',
        'GU': 'Guam',
        'HI': 'Hawaii',
        'IA': 'Iowa',
        'ID': 'Idaho',
        'IL': 'Illinois',
        'IN': 'Indiana',
        'KS': 'Kansas',
        'KY': 'Kentucky',
        'LA': 'Louisiana',
        'MA': 'Massachusetts',
        'MD': 'Maryland',
        'ME': 'Maine',
        'MI': 'Michigan',
        'MN': 'Minnesota',
        'MO': 'Missouri',
        'MP': 'Northern Mariana Islands',
        'MS': 'Mississippi',
        'MT': 'Montana',
        'NA': 'National',
        'NC': 'North Carolina',
        'ND': 'North Dakota',
        'NE': 'Nebraska',
        'NH': 'New Hampshire',
        'NJ': 'New Jersey',
        'NM': 'New Mexico',
        'NV': 'Nevada',
        'NY': 'New York',
        'OH': 'Ohio',
        'OK': 'Oklahoma',
        'OR': 'Oregon',
        'PA': 'Pennsylvania',
        'PR': 'Puerto Rico',
        'RI': 'Rhode Island',
        'SC': 'South Carolina',
        'SD': 'South Dakota',
        'TN': 'Tennessee',
        'TX': 'Texas',
        'UT': 'Utah',
        'VA': 'Virginia',
        'VI': 'Virgin Islands',
        'VT': 'Vermont',
        'WA': 'Washington',
        'WI': 'Wisconsin',
        'WV': 'West Virginia',
        'WY': 'Wyoming'
    }
    new_dict = dict (zip(states.values(),states.keys()))
    stateName = new_dict[stateName]
    newLocation = stateName + cityName + str(startYear)[2:]
    return newLocation

