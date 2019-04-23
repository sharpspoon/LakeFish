"""
Definition of views.
"""

from django.shortcuts import render
from Lake2019.user import createLakeInitFile as createInitF
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
import sys
import time
import cgi
import calendar
import datetime
import csv

from weather import WeatherScraper as ws
from nldas2 import common
import pupygrib
from pydap.client import open_url
from pydap.cas.urs import setup_session
import cgitb
cgitb.enable()
now = datetime.datetime.now()

from django.shortcuts import render
from django.conf import settings
from django.core.files.storage import FileSystemStorage


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
                os.makedirs("user-nldas-"+username)
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
                weatherDate = datetime.datetime(weatherYear, weatherMonth, 1)
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
                'year': now.year,
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



def nldas2(request):
    form = DisplayWeatherDataForm()
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/nldas2.html',
        {
            'title': 'NLDAS-2',
            'year': now.year,
            'message': 'Your application description page.',
            'form': form
        }
    )

def simple_upload(request):
    try:
        if request.method == 'POST' and request.FILES['myfile']:
            username = str(request.user)
            savePath = "user-nldas-"+username+"//"
            myfile = request.FILES['myfile']
            completePath = str(savePath+myfile.name)
            #user = request.post['username']
            fs = FileSystemStorage()
            filename = fs.save(completePath, myfile)
            uploaded_file_url = fs.url(filename)
            #currentFiles = os.listdir(savePath)
            #currentFiles2 = ("\n".join(currentFiles))
            return render(request, 'app/nldas2.html', {
                'uploaded_file_url': myfile
            })
    except:
        pass
    return render(request, 'app/nldas2.html')


def displaynldas2(request):
    dateFormat = '%d/%m/%Y'
    startDate = request.POST['date']
    endDate = request.POST['date2']
    state = request.POST['state']
    city = request.POST['city']
    startDay, startMonth, startYear = startDate.split("/", 2)
    startDT = datetime.datetime.strptime(startDate, dateFormat)
    startTT = startDT.timetuple()
    startJulianDay = startTT.tm_yday
    startJulianDayStr = str(startJulianDay)
    endDay, endMonth, endYear = endDate.split("/", 2)
    endDT = datetime.datetime.strptime(endDate, dateFormat)
    endTT = endDT.timetuple()
    endJulianDay = endTT.tm_yday
    endJulianDayStr = str(endJulianDay)
    myDate = startDate

    form = DisplayWeatherDataForm()
    #dataset_url = ('https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_M.002/' + startYear +'/NLDAS_FORA0125_M.A' + startYear + startMonth+'.002.grb')
    dataset_url = 'http://test.opendap.org/dap/data/nc/coads_climatology.nc'
    session = setup_session('rcw0024', '', check_url=dataset_url)

    dataset = open_url(dataset_url, session=session)
    datasetType = type(dataset)
    fullPath = os.path.dirname(os.path.abspath(__file__)) + common.NLDASpath
    # loop over dates from startDate to endDate
    # while myDate <= endDate:
    # split out the parts of the year
    #tt = myDate.timetuple()
    #julianday = format(tt.tm_yday, '03')

    # get current path
    #fullPath = os.path.dirname(os.path.abspath(__file__)) + common.NLDASpath
    # call wget to download files for given year/day
    #os.system('wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -np -r --content-disposition https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_H.002/' + year + '/' + julianday + '/ -A grb')

    # create daily averages and output netCDF file
    #hourly_to_daily_NLDAS.hourly_to_daily_one_day(fullPath, year, julianday)
    #myDate += timedelta(days=1)

    #directoryPath = ('https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_M.002/' + year +'/NLDAS_FORA0125_M.A' + year + month+'.002.grb')
    # with open(''+directoryPath, 'rb') as stream:
    #   for i, msg in enumerate(pupygrib.read(stream), 1):
    #      lons, lats = msg.get_coordinates()
    #     values = msg.get_values()
    #    print("Message {}: {:.3f} {}".format(i, values.mean(), lons.shape))
    #os.system('wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -np -r --content-disposition https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_H.002/' + year + '/' + julianDayStr + '/ -A grb')
    return render(
        request,
        'app/nldas2.html',
        {
            'vars': 'vars:',
            'startDate': 'startDate= '+startDate,
            'startDay': 'startDay= '+startDay,
            'startMonth': 'startMonth= '+startMonth,
            'startYear': 'startYear= '+startYear,
            'startJulianDay': 'startJulianDay= '+startJulianDayStr,
            'endDate': 'endDate= '+endDate,
            'endDay': 'endDay= '+endDay,
            'endMonth': 'endMonth= '+endMonth,
            'endYear': 'endYear= '+endYear,
            'endJulianDay': 'endJulianDay= '+endJulianDayStr,
            'state': 'state= '+state,
            'city': 'city= '+city,
            'dataset_url': 'dataset_url= '+dataset_url,
            'message': 'Weather Data page.',
            'year': now.year,
            'form': form
        }
    )


def simulateLake(request):
    # Weather Station data
    #weather_station_data_filepath = "C:\\Github\\LakeFish\\LakeFish\\app\\static\\app\\station.csv"
    #weather_station_reader = csv.DictReader(
        #open(weather_station_data_filepath))
    #weather_station_dict = []
    #for row in weather_station_reader:
        #weather_station_dict.append(row)
    #print(weather_station_dict)

    if(request.method == "POST"):
        print("This has been hit")
        print(request.POST)
        form = CreateInitFileForm()
        userInput = {
            'sim_title': request.POST['sim_title'],
            'LakeName': request.POST['lake_name'],
            #'state': request.POST['state'],
            'num_horiz_layers': request.POST['num_horiz_layers'],
            'max_depth': request.POST['max_depth'],
            'elevation': request.POST['elevation'],
            'light_atten_water': request.POST['light_atten_water'],
            'light_atten_chlor': request.POST['light_atten_chlor'],
            'xk1': request.POST['xk1'],
            'wind_sheltering_fall': request.POST['wind_sheltering_fall'],
            'wind_sheltering_summer': request.POST['wind_sheltering_summer'],
            'temp_wind_sheltering': request.POST['temp_wind_sheltering'],
            'snow_ice_ratio': request.POST['snow_ice_ratio'],
            'std': request.POST['std'],
            'sediment_density': request.POST['sediment_density'],
            'snow_compact': request.POST['snow_compact'],
            'ice_conduct': request.POST['ice_conduct'],
            'snow_conduct': request.POST['snow_conduct'],
            'wcht_coefficient': request.POST['wcht_coefficient'],
            'max_snow_ice_thickness_ratio': request.POST['max_snow_ice_thickness_ratio'],
            'ice_abs_coefficient': request.POST['ice_abs_coefficient'],
            'ice_reflect_coefficient': request.POST['ice_reflect_coefficient'],
            'ice_attn_coefficient': request.POST['ice_attn_coefficient'],
            'snow_abs_coefficient': request.POST['snow_abs_coefficient'],
            'snow_reflect_coefficient': request.POST['snow_reflect_coefficient'],
            'snow_attn_coefficient': request.POST['snow_attn_coefficient'],
            'init_ice_thickness': request.POST['init_ice_thickness'],
            'init_snow_thickness': request.POST['init_snow_thickness'],
            'init_water_depths_div': request.POST['init_water_depths_div'],
            'init_water_temps': request.POST['init_water_temps'],
            'init_sus_solids_conc': request.POST['init_sus_solids_conc'],
            'total_diss_solids': request.POST['total_diss_solids'],
            'init_phos_conc': request.POST['init_phos_conc'],
            'init_diss_oxy': request.POST['init_diss_oxy'],
            'det_decay_rate': request.POST['det_decay_rate'],
            'alg_resp_rater': request.POST['alg_resp_rater'],
            'max_photo_ratio': request.POST['max_photo_ratio'],
            'is_outflow_file': request.POST['is_outflow_file'],
            'is_plot_file': request.POST['is_plot_file'],
            'num_depth_plots': request.POST['num_depth_plots'],
            'tab_data_interval': request.POST['tab_data_interval'],
            'inflow_outflow_source': request.POST['inflow_outflow_source'],
            'time_series_output': request.POST['time_series_output']
        }
        #createInit.gatherPost(userInput)
        # createInit.createInit(userInput)
        createInitF.gatherPost(userInput)
        #createInit.createInit(userInput)
        return render(
            request,
            'app/simulatelake.html',
            {
                'title': 'Simulate Lake',
                'message': 'Simulate Lake page.',
                'year': now.year,
                'form': form,
                #'data': weather_station_dict
            }
        )
    else:
        return render(
            request,
            'app/simulatelake.html',
            {
                'title': 'Simulate Lake',
                'message': 'Simulate Lake page.',
                'year': now.year,
                #'data': weather_station_dict
            }
        )
