"""
Definition of urls for LakeFish.
"""

from datetime import datetime
from django.conf.urls import url
import django.contrib.auth.views

from django.conf.urls import url
import app.views as core_views


import app.forms
import app.views

# Uncomment the next lines to enable the admin:
# from django.conf.urls import include
# from django.contrib import admin
# admin.autodiscover()

urlpatterns = [
    # Examples:
    url(r'^$', app.views.home, name='home'),
    url(r'^contact$', app.views.contact, name='contact'),
    url(r'^about', app.views.about, name='about'),
    url(r'^weather', app.views.weather, name='weather'),
    url(r'^displayweather', app.views.displayWeather, name='display weather'),
    url(r'^simulatelake', app.views.simulateLake, name='simulate lake'),
    url(r'^nldas2', app.views.nldas2, name='nldas2'),
    url(r'^displaynldas2', app.views.displaynldas2, name='displaynldas2'),
    url(r'^login/$',
        django.contrib.auth.views.login,
        {
            'template_name': 'app/login.html',
            'authentication_form': app.forms.BootstrapAuthenticationForm,
            'extra_context':
            {
                'title': 'Log in',
                'year': datetime.now().year,
            }
        },
        name='login'),



    url(r'^register/$',
        core_views.signup,
        name='register'),

    # url(r'^register/$',
    # django.contrib.auth.views.login,
    # {
    #    'template_name': 'app/register.html',
    #    'authentication_form': app.forms.BootstrapAuthenticationForm,
    #    'extra_context':
    #    {
    #        'title': 'Register',
    #        'year': datetime.now().year,
    #    }
    # },
    # name='register'),


    url(r'^logout$',
        django.contrib.auth.views.logout,
        {
            'next_page': '/',
        },
        name='logout'),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # url(r'^admin/', include(admin.site.urls)),
]
