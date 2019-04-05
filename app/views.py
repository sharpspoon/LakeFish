"""
Definition of views.
"""

from django.shortcuts import render
from LakeFish import createLakeInitFile
from django.http import HttpRequest
from django.template import RequestContext
from app.forms import DisplayWeatherDataForm
from app.forms import CreateInitFileForm
from datetime import datetime

from django.contrib.auth import login, authenticate
from django.contrib.auth import login as auth_login
from django.contrib.auth.forms import UserCreationForm
from django.shortcuts import render, redirect, render_to_response

import os
import time
import cgi
import calendar
import datetime

from createInit import createLakeInitFile as createInit
from weather import WeatherScraper as ws
from nldas2 import common

now = datetime.datetime.now()


def home(request):
    """Renders the home page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/index.html',
        {
            'title': 'Home Page',
            'year': now.year,
        }
    )


def contact(request):
    """Renders the contact page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/contact.html',
        {
            'title': 'Contact',
            'message': 'Xing Fang, Ph.D., P.E., D.WRE, F. EWRI, F.ASCE \n Arthur H. Feagin Chair Professor of Civil Engineering',
            'year': now.year,
        }
    )


def about(request):
    """Renders the about page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/about.html',
        {
            'title': 'About',
            'message': 'Your application description page.',
            'year': now.year,
        }
    )


def signup(request):
    if request.method == 'POST':
        form = UserCreationForm(request.POST)
        if form.is_valid():
            form.save()
            username = form.cleaned_data.get('username')
            raw_password = form.cleaned_data.get('password1')
            user = authenticate(username=username, password=raw_password)
            auth_login(request, user)

            try:
                os.makedirs("FortranModel\\"+username)
            except FileExistsError:
                pass

            return redirect('home')
    else:
        form = UserCreationForm()
    return render(request, 'app/register.html', {'form': form})


def weather(request):
    form = DisplayWeatherDataForm()
    return render(
        request,
        'app/weather.html',
        {
            'title': 'Weather Data',
            'message': 'Weather Data page.',
            'year': now.year,
            'form': form
        }
    )
    # assert isinstance(request, HttpRequest)
    # return render(
    #     request,
    #     'app/weather.html',
    #     {
    #         'title': 'Weather Data',
    #         'message': 'Weather Data page.',
    #         'year': datetime.now().year
    #     }
    # )


def displayWeather(request):
    if (request.method == 'POST'):
        abbrev_state = ws.state_to_abbrev(request.POST['state'])
        user_location = request.POST['city'] + ", " + abbrev_state

        wScraper = ws.WeatherScraper(user_location, request.POST['date'])
        wScraper.run()

        wScraper2 = ws.WeatherScraper(user_location, request.POST['date'])
        weather_file = wScraper2.get_file_path()

        weatherData = []
        temperature = []
        dewPoint = []
        windSpeed = []
        windBearing = []
        uvIndex = []
        cloudCover = []
        precipIntensity = []
        precipAccumulation = []
        formattedWeatherList = []

        if os.path.isfile(weather_file):
            with open(weather_file, newline='') as weatherFile:
                header_line = next(weatherFile)  # Format: Month #ofDays Year
                weatherMonth = int(header_line.split(' ')[0])
                numOfDays = int(header_line.split(' ')[1])
                weatherYear = int(header_line.split(' ')[2])
                weatherDate = datetime(weatherYear, weatherMonth, 1)
                formattedDate = weatherDate.strftime("%B %Y")
                # weatherFileReader = csv.reader(weatherFile, delimiter='\t')
                # for data in weatherFileReader:
                # weatherData.append(data)
                for data in weatherFile:
                    weatherData.append(data.split())

                for dailyData in weatherData:
                    temperature.append(dailyData[0])
                    dewPoint.append(dailyData[1])
                    windSpeed.append(dailyData[2])
                    windBearing.append(dailyData[3])
                    uvIndex.append(dailyData[4])
                    cloudCover.append(dailyData[5])
                    precipIntensity.append(dailyData[6])
                    precipAccumulation.append(dailyData[7])

                formattedWeatherList = [temperature, dewPoint, windSpeed,
                                        windBearing, uvIndex, cloudCover,
                                        precipIntensity, precipAccumulation]
                # testFileContent = dailyDataList
        else:
            raise ValueError("Error")

        return render(
            request,
            'app/displayweather.html',
            {
                'title': 'Display Weather Data',
                'message': 'Display Weather Data page.',
                'year': datetime.now().year,
                'file_content': formattedWeatherList,
                'weatherDate': formattedDate,
                'temperature': temperature,
                'dew_point': dewPoint,
                'wind_speed': windSpeed,
                'weatherMonth': weatherMonth,
                'numOfDays': numOfDays,
                'weatherYear': weatherYear,
                'user_loc': user_location
            }
        )


def nldas23(request):
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/nldas2.html',
        {
            'title': 'NLDAS-2',
            'message': 'Your application description page.',
            'year': now.year,
        }
    )


def nldas2(request):
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/nldas2.html',
        {
            'title': 'NLDAS-2',
            'year': now.year,
            'message': 'Your application description page.'
        }
    )


def displaynldas2(request):
    startDate = request.POST['date']
    endDate = request.POST['date2']
    myDate = startDate

    #loop over dates from startDate to endDate
    #while myDate <= endDate:
        #split out the parts of the year
        #tt = myDate.timetuple()
        #julianday = format(tt.tm_yday, '03')

        # get current path
        #fullPath = os.path.dirname(os.path.abspath(__file__)) + common.NLDASpath
        #call wget to download files for given year/day
        #os.system('wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -np -r --content-disposition https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_H.002/' + year + '/' + julianday + '/ -A grb')

        #create daily averages and output netCDF file
        #hourly_to_daily_NLDAS.hourly_to_daily_one_day(fullPath, year, julianday)
        #myDate += timedelta(days=1)
    form = DisplayWeatherDataForm()
    day, month, year = startDate.split("/", 2)
    dateFormat = '%d/%m/%Y'
    dt = datetime.datetime.strptime(startDate, dateFormat)
    tt = dt.timetuple()
    julianDay = tt.tm_yday
    julianDayStr = str(julianDay)
    fullPath = os.path.dirname(os.path.abspath(__file__)) + common.NLDASpath
    os.system('wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -np -r --content-disposition https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_H.002/' + year + '/' + julianDayStr + '/ -A grb')
    return render(
        request,
        'app/nldas2.html',
        {
            'dateRange': 'startDate='+startDate + ' endDate=' + endDate + ' year=' + year + ' julian=' + julianDayStr,
            'message': 'Weather Data page.',
            'year': now.year,
            'form': form
        }
    )


def simulateLake(request):
    if(request.method == "POST"):
        print("This has been hit")
        print(request.POST)
        form = CreateInitFileForm()
        userInput = {  
            'sim_title': request.POST['sim_title'],
            'LakeName': request.POST['lake_name'],
            'state': request.POST['state'],
            'surface_area': request.POST['surface_area'],
            'max_depth': request.POST['max_depth'],
            'elevation_': request.POST['elevation_']
        }
        createInit.gatherPost(userInput)
        return render(
            request,
            'app/simulatelake.html',
            {
                'title': 'Simulate Lake',
                'message': 'Simulate Lake page.',
                'year': now.year,
                'form': form
            }
        )
    else:
        return render(
            request,
            'app/simulatelake.html',
            {
                'title': 'Simulate Lake',
                'message': 'Simulate Lake page.',
                'year': now.year
            }
        )
