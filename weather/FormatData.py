import win32com.client
import sys
import csv
import pandas as pd
from xlutils.copy import copy
import xlrd
import xlsxwriter
import os
from math import sin, asin, cos
from datetime import date, datetime
import shutil
import re

stations = pd.read_csv("stations.csv", header=None, error_bad_lines=False)
stations.columns = ['State', 'City', 'WBAN', 'Latitude', 'Longitude']

def main():
    for file in os.listdir('raw data'):
        fileNames = divideIntoYears(f"raw data\\{file}")
        for name in fileNames:
            wban = fileNames[0].split('_')[0]
            row = stations.loc[stations['WBAN'] == int(wban)]
            index = row.index[0]
            finalFilePrefix = f"{row.at[index, 'State']}{str(row.at[index, 'City'])[:4]}"

            if os.path.isfile(f'formatted data\\{finalFilePrefix}{name[-2:]}.dat'):
                print(f"skipping file {finalFilePrefix}{name[-2:]}.dat")
                continue

            rawYearToExcelFormat(f'{name}.csv')
            csv = pd.read_csv(f'{name}Formatted.csv', dtype='str')
            cloudCover = []
            computeCloudCover(csv, cloudCover)
            hourly = computeHourlyValues(csv, cloudCover, float(row.at[index, 'Latitude']), float(row.at[index, 'Longitude']))
            daily = computeDailyValues(hourly)

            writeDataframeToFile(daily, finalFilePrefix)

def divideIntoYears(bigFileNameIn):
    bigFileName = bigFileNameIn
    bigFile = open(bigFileName, "r")
    
    header = bigFile.readline()
    row = bigFile.readline()

    fileNames = []

    for i in range(5):
        try:
            wban = str(row).split(",")[0][-6:-1]
            year = str(row).split(",")[1][1:5]
        except:
            break

        littleFile = open(f"{wban}_{year}.csv", "w")
        fileNames.append(f'{wban}_{year}')
        littleFile.write(header)

        while(str(row.split(",")[1][1:5]) == year):
            littleFile.write(row)
            row = next(bigFile, None)
            if row == None:
                break
    
    return fileNames


def rawYearToExcelFormat(oldFileNameIn):
    oldFileName = oldFileNameIn
    newFileName = oldFileName[:-4] + "Formatted.csv"

    oldFile = open(oldFileName, "r")
    newFile = open(newFileName, "w")

    newFile.write("WBAN,Date,Time,StationType,SkyCondition,SkyConditionFlag,Visibility,")
    newFile.write("VisibilityFlag,WeatherType,WeatherTypeFlag,DryBulbFahrenheit,")
    newFile.write("DryBulbFahrenheitFlag,DryBulbCelsius,DryBulbCelsiusFlag,WetBulbFahrenheit,")
    newFile.write("WetBulbFahrenheitFlag,WetBulbCelsius,WetBulbCelsiusFlag,DewPointFahrenheit,")
    newFile.write("DewPointFahrenheitFlag,DewPointCelsius,DewPointCelsiusFlag,RelativeHumidity,")
    newFile.write("RelativeHumidityFlag,WindSpeed,WindSpeedFlag,WindDirection,WindDirectionFlag,")
    newFile.write("ValueForWindCharacter,ValueForWindCharacterFlag,StationPressure,")
    newFile.write("StationPressureFlag,PressureTendency,PressureTendencyFlag,PressureChange,")
    newFile.write("PressureChangeFlag,SeaLevelPressure,SeaLevelPressureFlag,RecordType,")
    newFile.write("RecordTypeFlag,HourlyPrecipitation,HourlyPrecipitationFlag,Altimeter,AltimeterFlag\n")
    
    reader = csv.reader(x.replace('\0', '') for x in oldFile)
    processingRowOne = True
    for row in reader:
        if processingRowOne:
            processingRowOne = False
            continue

        dailyData = row[41:57]
        if (dailyData.count('') == len(dailyData)):
            for i in range(22):
                row.insert(6, '')
            dailyData = row[41:57]
            if (dailyData.count('') == len(dailyData)):
                print("skipping")
                continue

        wban = row[0][5:]
        year = str(row[1][:4])
        month = str(row[1][5:7])
        day = str(row[1][8:10])
        date = year + month + day
        hours = row[1][11:13]
        minutes = row[1][14:16]
        time = hours + minutes
        stationType = '0' #?????? look into this
        skyConditions = row[50].replace(":", "0")
        visibility = row[52]
        weatherInputList = row[45].split()
        weatherOutputList = []
        for weather in weatherInputList:
            if ":" in weather:
                weather = weather.split(":")[0]
            elif "|" in weather:
                continue
            weatherOutputList.append(weather[:2])
        weatherType = ' '.join(weatherOutputList)
        try:
            dryTempF = float(row[43].strip())
        except ValueError:
            dryTempF = ""
            dryTempC = ""
        else:
            try:
                dryTempC = (dryTempF - 32) * (5/9)
            except ValueError:
                dryTempC = ""

        try:
            wetTempF = float(row[53].strip())
        except ValueError:
            wetTempF = ""
            wetTempC = ""
        else:
            try:
                wetTempC = (wetTempF - 32) * (5/9)
            except ValueError:
                wetTempC = ""

        dewPointF = row[42]
        
        try:
            if dewPointF[-1].isalpha():
                dewPointF = dewPointF[:-1]
            dewPointC = (float(dewPointF.strip()) - 32) * (5/9)
        except (ValueError, IndexError):
            dewPointC = ""
            dewPointF = ""


        relativeHumidity = row[48]
        windSpeed = row[56]
        windDirection = row[54]
        valueForWindCharacter = row[55]
        stationPressure = row[51]
        pressureTendency = row[47]
        pressureChange = row[46]
        seaLevelPressure = row[49]
        hourlyPrecipitation = row[44]
        altimeter = row[41]

        newFile.write(f'{wban},{date},{time},{stationType},{skyConditions},')
        newFile.write(f',{visibility},,{weatherType},,{dryTempF},,{dryTempC},')
        newFile.write(f',{wetTempF},,{wetTempC},,{dewPointF},,{dewPointC},')
        newFile.write(f',{relativeHumidity},,{windSpeed},,{windDirection},')
        newFile.write(f',{valueForWindCharacter},,{stationPressure},')
        newFile.write(f',{pressureTendency},,{pressureChange},,{seaLevelPressure},')
        newFile.write(f',AA,,{hourlyPrecipitation},,{altimeter},\n')


def computeCloudCover(hourlyData, cloudCover):
    for i in range(len(hourlyData.index)):
        temp = []
        skyConditions = str(hourlyData.at[i, 'SkyCondition']).split(' ')

        for j in range(len(skyConditions)):
            k = skyConditions[j]

            if k.isdigit():
                continue

            k = k[:3]

            if k == "OVC":
                temp.append(1)
            elif k == "CLR":
                temp.append(0)
            elif k == "FEW":
                temp.append(0.25)
            elif k == "SCT":
                temp.append(0.5)
            elif k == "BKN":
                temp.append(0.75)
            else:
                temp.append(0)

        if len(temp) > 0:
            cloudCover.append(max(temp))
        else:
            cloudCover.append(0)

def computeHourlyValues(hourlyData, cloudCover, latitude, longitude):
    usefulHourly = hourlyData[['Date', 'Time', 'DryBulbFahrenheit', 'DewPointFahrenheit', 'WindSpeed', 'WindDirection', 'HourlyPrecipitation']].copy()
    usefulHourly['CloudCover'] = pd.Series(cloudCover, index=usefulHourly.index)

    for i in range(len(usefulHourly.index)):
        if not str(usefulHourly.iat[i, 2]).replace('.', '1').isdigit():
            usefulHourly.iat[i, 2] = usefulHourly.iat[i - 1, 2]
        
        if not str(usefulHourly.iat[i, 3]).replace('.', '1').isdigit():
            usefulHourly.iat[i, 3] = usefulHourly.iat[i - 1, 3]

        if not str(usefulHourly.iat[i, 4]).replace('.', '1').isdigit():
            usefulHourly.iat[i, 4] = usefulHourly.iat[i - 1, 4]

        if not str(usefulHourly.iat[i, 5]).replace('.', '1').isdigit():
            usefulHourly.iat[i, 5] = usefulHourly.iat[i - 1, 5]

        if not str(usefulHourly.iat[i, 6]).replace('.', '1').isdigit():
            usefulHourly.iat[i, 6] = 0

    averageDryBulbs = []
    averageDewPoints = []
    averageWindSpeeds = []
    averageWindDirections = []
    averagePrecipitations = []
    averageCloudCovers = []
    hours = []
    dates = []

    i = 0
    while i in range(len(usefulHourly.index)):
        numEntries = 0
        
        for j in range(100):
            if (j + i) >= len(usefulHourly.index):
                break
            
            if int(int(usefulHourly.iat[i + j, 1]) / 100) == int(int(usefulHourly.iat[i, 1]) / 100):
                numEntries = j
            else:
                break
        
        vrbCount = 0

        dryBulb = float(usefulHourly.at[i, 'DryBulbFahrenheit'])
        dewPoint = float(usefulHourly.at[i, 'DewPointFahrenheit'])
        windSpeed = float(usefulHourly.at[i, 'WindSpeed'])
        
        try:
            windDirection = float(usefulHourly.at[i, 'WindDirection'])
        except ValueError:
            windDirection = 0.0
            vrbCount += 1

        try:
            precipitation = float(usefulHourly.at[i, 'HourlyPrecipitation'])
        except ValueError:
            if re.match(r'[0-9]*[.][0-9]+[.][0-9]+',usefulHourly.at[i, 'HourlyPrecipitation']):
                print(f"double precipitation: {usefulHourly.at[i, 'HourlyPrecipitation']}")
                midpoint = int(len(usefulHourly.at[i, 'HourlyPrecipitation'])/2)
                precipitation = float(usefulHourly.at[i, 'HourlyPrecipitation'][:midpoint])

        
        cloudCover = float(usefulHourly.at[i, 'CloudCover'])


        for j in range(1, numEntries + 1):
            dryBulb = dryBulb + float(usefulHourly.at[i + j, 'DryBulbFahrenheit'])
            dewPoint = dewPoint + float(usefulHourly.at[i + j, 'DewPointFahrenheit'])
            windSpeed = windSpeed + float(usefulHourly.at[i + j, 'WindSpeed'])
            try:
                windDirection = windDirection + float(usefulHourly.at[i + j, 'WindDirection'])
            except ValueError:
                vrbCount += 1

            try:
                precipitation = precipitation + float(usefulHourly.at[i + j, 'HourlyPrecipitation'])
            except ValueError:
                if re.match(r'[0-9]*[.][0-9]+[.][0-9]+',usefulHourly.at[i + j, 'HourlyPrecipitation']):
                    print(f"double precipitation: {usefulHourly.at[i + j, 'HourlyPrecipitation']}")
                    midpoint = int(len(usefulHourly.at[i + j, 'HourlyPrecipitation'])/2)
                    precipitation = precipitation + float(usefulHourly.at[i + j, 'HourlyPrecipitation'][:midpoint])
            cloudCover = cloudCover + float(usefulHourly.at[i + j, 'CloudCover'])


        dates.append(usefulHourly.at[i, 'Date'])
        hours.append(int(int(usefulHourly.at[i, 'Time']) / 100))
        averageDryBulbs.append(dryBulb / (numEntries + 1))
        averageDewPoints.append(dewPoint / (numEntries + 1))
        averageWindSpeeds.append(windSpeed / (numEntries + 1))
        try:
            averageWindDirections.append(windDirection / (numEntries + 1 - vrbCount))
        except ZeroDivisionError:
            try:
                averageWindDirections.append(averageWindDirections[-1])
            except IndexError:
                averageWindDirections.append(0)
        averagePrecipitations.append(precipitation / (numEntries + 1))
        averageCloudCovers.append(cloudCover / (numEntries + 1))

        i = i + numEntries + 1

    rainfall = []
    snowfall = []

    i = 0
    for i in range(len(averagePrecipitations)):
        precipitation = averagePrecipitations[i]
        dryBulb = averageDryBulbs[i]
        dewPoint = averageDewPoints[i]

        if precipitation == 0:
            rainfall.append(0)
            snowfall.append(0)
        elif dryBulb > 34:
            rainfall.append(precipitation)
            snowfall.append(0)
        elif dryBulb >= 28 and dewPoint <= 34:
            rainfall.append(0)
            snowfall.append(10 * precipitation)
        elif dryBulb >= 20 and dewPoint <= 27:
            rainfall.append(0)
            snowfall.append(14.997 * precipitation + 0.0221)
        elif dryBulb >= 15 and dewPoint <= 19:
            rainfall.append(0)
            snowfall.append(19.994 * precipitation + 0.075)
        elif dryBulb >= 10 and dewPoint <= 14:
            rainfall.append(0)
            snowfall.append(29.991 * precipitation + 0.0113)
        elif dryBulb >= 0 and dewPoint <= 9:
            rainfall.append(0)
            snowfall.append(39.988 * precipitation + 0.01551)
        elif dryBulb <= -1 and dewPoint >= -20:
            rainfall.append(0)
            snowfall.append(49.985 * precipitation + 0.0189)
        elif dryBulb >= -40 and dewPoint <= -21:
            rainfall.append(0)
            snowfall.append(99.97 * precipitation + 0.0377)
        else:
            rainfall.append(0)
            snowfall.append(99.97 * precipitation + 0.0377)

    solarRadiation = []

    j = 0
    for j in range(len(averageCloudCovers)):
        phi = 15 * round(longitude / 15.0)

        unformattedDate = dates[j]
        date = datetime.strptime(unformattedDate, "%Y%m%d")
        days =  date.strftime('%j')

        jday1 = float(days) - 1 + (float(hours[j]) / 24)
        hcloud = averageCloudCovers[j]
        pi = 3.1419
        eqt = 0.17 * sin(4 * pi * (int(jday1) - 80) / 373) - 0.129 * sin(2 * pi * int((jday1) - 8) / 355)
        hour1 = round(24 * (jday1 - int(jday1))) # good
        hh = round(2 * pi / 24 * (hour1 - ((longitude - phi) * 24 / 360) + eqt - 12.0),6) # good
        taud = 2 * pi * (int(jday1) - 1) / 365
        val = round(0.006918 - 0.399912 * cos(taud) + 0.070257 * sin(taud) - 0.006758 \
        * cos(2 * taud) + 0.000907 * sin(2 * taud) - 0.002697 * cos(3 * taud) + 0.00148 * sin(3 * taud),7)
        f_sin = sin(latitude * 0.01743)
        d_sin = sin(val)
        s_cos = cos(latitude * 0.01743)
        d_cos = cos(val)
        h_cos = cos(hh)
        a_asin = f_sin * d_sin + s_cos * d_cos * h_cos
        ao = asin(a_asin)
        ao = ao * 180 / pi
        if ao < 0:
            phis = 0
            sro = 0
        else:
            phis = 24 * (2.044 * ao + 0.1296 * ao**2 - 1.941 * 0.001 * ao**3 +  7.591 * 0.000001 * ao**4) * 0.1314
            sro = (1 - 0.65 * (hcloud)**2) * phis
        solarRadiation.append(sro)

    outputData = {
        'Date': dates,
        'Hour': hours,
        'DryBulbFahrenheit': averageDryBulbs,
        'DewPointFahrenheit': averageDewPoints,
        'WindSpeed': averageWindSpeeds,
        'WindDirection': averageWindDirections,
        'Precipitation': averagePrecipitations,
        'Rainfall': rainfall,
        'Snowfall': snowfall,
        'CloudCover': averageCloudCovers,
        'SolarRadiation': solarRadiation
    }

    output = pd.DataFrame(data=outputData)

    return output

def computeDailyValues(hourlyValues):    
    dailyDryBulbs = []
    dailyDewPoints = []
    dailyCloudCovers = []
    dailySolarRadiations = []
    dailyWindSpeeds = []
    dailyWindDirections = []
    dailyPrecipitations = []
    dailySnowfalls = []
    dates = []

    i = 0
    while i in range(len(hourlyValues.index)):
        dryBulb = 0
        dewPoint = 0
        cloudCover = 0
        solarRadiation = 0
        windSpeed = 0
        windDirection = 0
        precipitation = 0
        snowfall = 0

        numHours = 0
        for j in range(24):
            if i + j < len(hourlyValues.index):
                if hourlyValues.at[i, 'Date'] == hourlyValues.at[i + j, 'Date']:
                    numHours += 1

        for j in range(numHours):
            dryBulb += float(hourlyValues.at[i + j, 'DryBulbFahrenheit'])
            dewPoint += float(hourlyValues.at[i + j, 'DewPointFahrenheit'])
            windSpeed += float(hourlyValues.at[i + j, 'WindSpeed'])
            windDirection += float(hourlyValues.at[i + j, 'WindDirection'])
            precipitation += float(hourlyValues.at[i + j, 'Precipitation'])
            snowfall += float(hourlyValues.at[i + j, 'Snowfall'])
            solarRadiation += float(hourlyValues.at[i + j, 'SolarRadiation'])
            cloudCover += float(hourlyValues.at[i + j, 'CloudCover'])

        dailyDryBulbs.append(round(dryBulb / numHours, 2))
        dailyDewPoints.append(round(dewPoint / numHours, 2))
        dailyWindSpeeds.append(round(windSpeed / numHours, 2))
        dailyWindDirections.append(round(windDirection / numHours, 2))
        dailySolarRadiations.append(round(solarRadiation * 0.085985, 2))
        dailyCloudCovers.append(round(100 - (cloudCover * 100 / numHours), 2))
        dailyPrecipitations.append(int((precipitation + 0.005) * 100))
        dailySnowfalls.append(round(snowfall * 25.4, 2))
        dates.append(hourlyValues.at[i, 'Date'])
    
        i = i + numHours

    outputData = {
        'Date': dates,
        'DryBulbFahrenheit': dailyDryBulbs,
        'DewPointFahrenheit': dailyDewPoints,
        'WindSpeed': dailyWindSpeeds,
        'WindDirection': dailyWindDirections,
        'SolarRadiation': dailySolarRadiations,
        'CloudCover': dailyCloudCovers,
        'Precipitation': dailyPrecipitations,
        'Snowfall': dailySnowfalls
    }

    output = pd.DataFrame(data=outputData)

    return output

def writeDataframeToFile(dataframe, prefix):
    dataframe.to_csv(path_or_buf='tempWrite.csv', sep='\t', float_format='%.1f', header=False, index=False)

    f = open('tempWrite.csv', 'r')
    oldlines = f.readlines()
    f.close()

    newlines = []

    currentMonth = '01'
    try:
        currentYear = oldlines[0][:4]
    except IndexError:
        return
    
    daysInMonth = 0
    monthBeginning = 0

    for line in oldlines:
        newlines.append(line[9:])

    for line in oldlines:
        month = line[4:6]
        if month == currentMonth:
            daysInMonth += 1
        else:
            newlines.insert(monthBeginning, f"{currentMonth}\t{daysInMonth}\t{currentYear}\n")
            monthBeginning += (daysInMonth + 1)
            currentMonth = month
            daysInMonth = 1

    #for December
    newlines.insert(monthBeginning, f"{currentMonth}\t{daysInMonth}\t{currentYear}\n")

    f = open(f'formatted data\\{prefix}{currentYear[2:]}.dat', 'w')
    f.write("".join(newlines))
    print(f"{prefix}{currentYear[2:]}.dat completed")
    f.close()

if __name__ == '__main__':
    main()