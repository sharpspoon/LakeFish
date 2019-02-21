import os
import sys
from shutil import copyfile
from datetime import datetime
from weather.weatherscraper import WeatherScraper

# Happy Paths

# Tests _data_already_retrieved()


def test100_testDataAlreadyRetrieved_DataAlreadyRetrieved():
    user_loc = "Salem,OR"
    date = "1/1999"
    scraper = WeatherScraper(user_loc, date)
    data_existing = scraper._data_already_retrieved()
    assert data_existing == True


def test110_testDataAlreadyRetrieved_FileNotInExistence():
    user_loc = "Salem,OR"
    date = "1/2017"
    scraper = WeatherScraper(user_loc, date)
    data_existing = scraper._data_already_retrieved()
    assert data_existing == False


def test115_testDataAlreadyRetrieved_NewMonth():
    user_loc = "Salem,OR"
    date = "3/1999"
    scraper = WeatherScraper(user_loc, date)
    data_existing = scraper._data_already_retrieved()
    assert data_existing == False


def test120_testDataAlreadyRetrieved_FileWithMultipleMonths():
    user_loc = "Salem,OR"
    date = "2/1999"
    scraper = WeatherScraper(user_loc, date)
    data_existing = scraper._data_already_retrieved()
    assert data_existing == True


def test130_testDataAlreadyRetrieved_PullingEarlierMonth():
    user_loc = "Salem,OR"
    date = "3/2018"
    scraper = WeatherScraper(user_loc, date)
    data_existing = scraper._data_already_retrieved()
    assert data_existing == False

# Test __init__()


def test200_testInit_CheckIfUserIsPullingCurrentMonth():
    user_loc = "Salem,OR"
    date = datetime.today()
    current_month = date.month
    current_year = date.year
    date = "%s/%s" % (current_month, current_year)
    scraper = WeatherScraper(user_loc, date)
    assert scraper.go_ahead == False
    assert scraper.run() == False


def test210_testInit_CheckVariableCreation():
    user_loc = "Dallas,TX"
    date = "1/2018"
    scraper = WeatherScraper(user_loc, date)
    assert scraper.city == "Dallas"
    assert scraper.directory == "./weather_data/tx/"
    assert scraper.filename == "./weather_data/tx/txdall18.dat"
    assert scraper.go_ahead == True
    assert scraper.key == "2dd9e033bfb386fa272686e32b748dda"
    assert scraper.lat == "32.7762719"
    assert scraper.lng == "-96.7968559"
    assert scraper.max_days == 0
    assert scraper.month == "1"
    assert scraper.state == "Texas"
    assert scraper.state_abbrev == "tx"
    assert scraper.year == "2018"


def test215_testInit_CheckVariableCreation_DifferentData():
    user_loc = "Dallas, TX"
    date = "1 2018"
    scraper = WeatherScraper(user_loc, date)
    assert scraper.city == "Dallas"
    assert scraper.directory == "./weather_data/tx/"
    assert scraper.filename == "./weather_data/tx/txdall18.dat"
    assert scraper.go_ahead == True
    assert scraper.key == "2dd9e033bfb386fa272686e32b748dda"
    assert scraper.lat == "32.7762719"
    assert scraper.lng == "-96.7968559"
    assert scraper.max_days == 0
    assert scraper.month == "1"
    assert scraper.state == "Texas"
    assert scraper.state_abbrev == "tx"
    assert scraper.year == "2018"

# Test _output_data


def test300_testOutputData_MakeSureFileContainsAllDataPoints():
    user_loc = "Salem,OR"
    date = "2/1980"
    scraper = WeatherScraper(user_loc, date)
    scraper.run()

    with open(scraper.filename, 'r') as f:
        lines = f.readlines()
    months = []
    end_of_list = False
    length = len(lines)
    index = 0
    correct_data = True
    while not end_of_list:
        if length == index:
            end_of_list = True
        elif length < index:
            end_of_list = True
            correct_data = False
        else:
            date = lines[index].split()
            months.append(date[0])
            max_days = int(date[1])
            index += max_days

    assert correct_data == True


def test310_testOutputData_CheckForMonthOrder():
    backup = "./weather_data/backup.dat"
    user_loc = "Salem,OR"
    date = "4/2015"
    scraper = WeatherScraper(user_loc, date)
    copyfile(scraper.filename, backup)

    scraper.run()

    with open(scraper.filename, 'r') as f:
        lines = f.readlines()
    months = []
    end_of_list = False
    length = len(lines)
    index = 0
    month_order_right = True
    while not end_of_list:
        if length <= index:
            end_of_list = True
        else:
            date = lines[index].split()
            months.append(date[0])
            max_days = int(date[1])
            index += max_days

            for x in range(1, len(months)):
                if int(months[x]) > int(months[x - 1]):
                    continue
                else:
                    month_order_right = False

    assert month_order_right == True
    os.remove(scraper.filename)
    copyfile(backup, scraper.filename)
    os.remove(backup)


def test400_testGetPath_CheckForPath():
    expected_location = "./weather_data/or/orsale15.dat"
    user_loc = "Salem,OR"
    date = "3/2015"
    scraper = WeatherScraper(user_loc, date)
    actual_location = scraper.get_file_path()
    assert expected_location == actual_location
