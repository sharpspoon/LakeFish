'''
Created on Feb 24, 2019

@author: Tyler
'''

if __name__ == '__main__':
    pass

def createInitFile(parm={}):
    pass

def handleStationName(cityName, stateName):
    statFile =  open("D:\Lake2019\#common\FIXED_INPUT\station.dat")
    stationList = []
    stateNum = 0
    cityNum = 0
    for line in statFile:
        stationList += [line.split()]
    for x in stationList:
        if(x[6] == stateName):
            stateNum = x[0]
            if(x[7] == cityName):
                cityNum = x[1]        
    return stateNum, cityNum