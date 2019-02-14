"""
Definition of views.
"""

from django.shortcuts import render
from django.http import HttpRequest
from django.template import RequestContext
from app.forms import DisplayWeatherDataForm
from datetime import datetime

from django.contrib.auth import login, authenticate
from django.contrib.auth import login as auth_login
from django.contrib.auth.forms import UserCreationForm
from django.shortcuts import render, redirect


def home(request):
    """Renders the home page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'app/index.html',
        {
            'title': 'Home Page',
            'year': datetime.now().year,
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
            'year': datetime.now().year,
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
            'year': datetime.now().year,
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
            'year': datetime.now().year,
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
        print(request.POST)

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
        with open('./Weather/Data/AL/ALMOBI16.dat', newline='') as weatherFile:
            header_line = next(weatherFile)  # Format: Month #ofDays Year
            weatherMonth = int(header_line.split(' ')[0])
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
                'wind_speed': windSpeed
            }
        )
