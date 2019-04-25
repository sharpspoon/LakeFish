from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import ElementNotInteractableException
from selenium.webdriver.support import expected_conditions as ec
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
import time
import csv
import pandas as pd
import os

stationFile = open("stations.csv")
requestsFile = open("requests.csv", 'a')
writer = csv.writer(requestsFile)
reader = csv.reader(stationFile)

def oldmain():
    stations = getStations()
    #31, 41 last
    for i in range(191,214):
        firstOrder = requestDataForSite(stations[i], 1996, 2000)
        secondOrder = requestDataForSite(stations[i], 2001, 2005)
        thirdOrder = requestDataForSite(stations[i], 2006, 2010)
        fourthOrder = requestDataForSite(stations[i], 2011, 2015)
        fifthOrder = requestDataForSite(stations[i], 2016, 2018)
        print(i)
        print(stations[i])
        print(firstOrder)
        print(secondOrder)
        print(thirdOrder)
        print(fourthOrder)
        print(fifthOrder)
        print()
        writer.writerow([stations[i], firstOrder, secondOrder, thirdOrder, fourthOrder, fifthOrder])
    requestsFile.close()
    stationFile.close()

def retrieveMessedUpRequests():
    requests = pd.read_csv("messedUpRequests.csv", header=None, dtype='str')
    requests.columns = ['WBAN', 'RequestID', 'StartYear', 'EndYear']

    for i in range(len(requests.index)):
        order = requestDataForSite(requests.at[i, 'WBAN'], int(requests.at[i, 'StartYear']), int(requests.at[i, 'EndYear']))
        writer.writerow([requests.at[i, 'WBAN'], order])

def main():
    currentPrefix = 'alBirm'
    stationTotal = 0

    prefixes = ['alBirm']

    for filename in os.listdir('formatted data'):
        if filename[:5] == 'nvEly':
            continue
        if filename[:6] == 'gaSava':
            continue
        if filename[:6] == 'mnBrai':
            continue
        if filename[:6] != currentPrefix:
            prefixes.append(filename[:6])
            currentPrefix = filename[:6]

    print(f'list length: {len(prefixes)}')
    print(f'set length: {len(set(prefixes))}')

    years = ['96', '97', '99', '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18']

    missingFiles = 0
    requestsToMake = []

    stationDataframe = pd.read_csv("stations.csv", header=None, dtype="str")
    stationDataframe.columns = ['State', 'City', 'WBAN', 'Latitude', 'Longitude']

    for prefix in prefixes:
        for year in years:
            if not os.path.isfile(f'formatted data\\{prefix}{year}.dat'):
                stateRows = stationDataframe.loc[stationDataframe['State'] == prefix[:2]]
                for i in stateRows.index:
                    if stateRows.at[i, 'City'].startswith(prefix[-4]):
                        index = i
                requestsToMake.append((stateRows.at[index, 'WBAN'], int(f'19{year}') if int(year) > 20 else int(f'20{year}')))
                missingFiles += 1
    
    print(f'list length: {len(requestsToMake)}')
    print(f'set length: {len(set(requestsToMake))}')

    stations = getStations()
    #31, 41 last
    for (wban, year) in requestsToMake:
        firstOrder = requestDataForSite(wban, year, year)
        print(i)
        print(wban)
        print(firstOrder)
        print()
        writer.writerow([wban, firstOrder])
    requestsFile.close()
    stationFile.close()

def getStations():
    wbans = []
    for row in reader:
        if int(row[2]) == 0:
            continue
        wbans.append(row[2])
    return wbans

def requestDataForSiteTest(wban, year1, year2):
    print(wban)

def requestDataForSite(wban, start_year, end_year):
    driver = webdriver.Firefox()
    wait = WebDriverWait(driver, 20, poll_frequency=1, ignored_exceptions=[NoSuchElementException])
    url = f"https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:{wban}/detail"
    driver.get(url)
    addToCart = wait.until(ec.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div[2]/div/div[1]/div/a")))
    addToCart.click()
    time.sleep(1)
    goToCart = driver.find_element_by_xpath("//*[@id='cartWidget']")
    goToCart.click()
    selectCSV = wait.until(ec.element_to_be_clickable((By.XPATH, '//*[@id="LCD_CUSTOM_CSV"]')))
    selectCSV.click()
    dateEntry = driver.find_element_by_xpath("//*[@id='dateRangeContainer']")
    dateEntry.click()
    selectStartYear = driver.find_element_by_class_name("noaa-datepicker-start")
    selectStartYear = selectStartYear.find_element_by_class_name("ui-datepicker-year")
    selectStartYear.click()
    for option in selectStartYear.find_elements_by_tag_name('option'):
        if option.text == str(start_year):
            option.click()
            break
    selectMonth = driver.find_element_by_class_name("noaa-datepicker-start")
    selectMonth = selectMonth.find_element_by_class_name("ui-datepicker-month")
    selectMonth.click()
    for option in selectMonth.find_elements_by_tag_name('option'):
        if option.text == 'Jan':
            option.click()
            break
    days = driver.find_elements_by_class_name("noaa-daterange-start-day")
    for day in days:
        if day.text == "1":
            day.click()
            break
    endDatePicker = driver.find_element_by_class_name("noaa-datepicker-end")
    selectEndYear = endDatePicker.find_element_by_class_name("ui-datepicker-year")
    selectEndYear.click()
    for option in selectEndYear.find_elements_by_tag_name('option'):
        if option.text == str(end_year):
            option.click()
            break
    selectEndMonth = endDatePicker.find_element_by_class_name("ui-datepicker-month")
    selectEndMonth.click()
    for option in selectEndMonth.find_elements_by_tag_name('option'):
        if option.text == 'Dec':
            option.click()
            break
    days = driver.find_elements_by_class_name("noaa-daterange-end-day")
    for day in days:
        if day.text == "31":
            day.click()
            break 
    applyDate = driver.find_element_by_class_name("noaa-daterange-applybtn")
    applyDate.click()
    continueButton = wait.until(ec.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div[2]/div/form/div[1]/div[6]/button")))
    continueButton.click()
    email = wait.until(ec.element_to_be_clickable((By.ID, "email")))
    time.sleep(1)
    email.click()
    email.send_keys('wjm0020@auburn.edu')
    emailConfirmation = driver.find_element_by_id("emailConfirmation")
    time.sleep(1)
    emailConfirmation.click()
    emailConfirmation.send_keys('wjm0020@auburn.edu')
    submit = wait.until(ec.element_to_be_clickable((By.ID, "buttonSubmit")))
    submit.click()
    orderNumber = wait.until(ec.element_to_be_clickable((By.XPATH, "//*[@id='receipt']/table[1]/tbody/tr[2]/td[2]"))).text
    driver.close()
    return orderNumber

if __name__ == '__main__':
    main()