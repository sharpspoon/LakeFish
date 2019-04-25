from selenium import webdriver
import csv
import os
import shutil
import datetime
import time

requestsFile = open("requests2.csv", 'r')
reader = csv.reader(requestsFile)

downloadReport = open("download_report.txt", 'a')

driver = webdriver.Chrome()

readingColumnTitles = True
for row in reader:
    if readingColumnTitles == False:
        for orderNumber in row[1:]:
            if (os.path.isfile('raw data\\' + str(orderNumber) + '.csv')):
                print(f"{orderNumber}.csv already exists")
                continue
            driver.get(f"https://www.ncei.noaa.gov/orders/cdo/{orderNumber}.csv")
    readingColumnTitles = False

requestsFile.close()
time.sleep(120)
print("moving files...")

requestsFile = open("requests2.csv")
reader = csv.reader(requestsFile)
readingColumnTitles = True
for row in reader:
    if readingColumnTitles == False:
        for orderNumber in row[1:]:
            sourcePath = "C:\\Users\\wjack\\Downloads\\" + orderNumber + '.csv'
            destinationPath = "raw data\\" + str(orderNumber) + ".csv"
            if (os.path.isfile(sourcePath)):
                shutil.move(sourcePath, destinationPath)
            else:
                if (not os.path.isfile(destinationPath)):
                    downloadReport.write(f"{orderNumber}.csv not found: {datetime.datetime.now()}\n")
    readingColumnTitles = False