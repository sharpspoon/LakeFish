'''
Created on Feb 24, 2019

@author: Tyler
'''

if __name__ == '__main__':
    pass

def createInitFile(parm={}):
    pass
def gatherPost():
    return
    #takes in the post from the user and saves them as indevidual variables that can be accessed later
def createInit():
    return
#creates the file of init and then begins populating it line by line with the required information also using the variables we saved from GatherPost
def executeSim():
    return
#calls the fortran model to run and point to the init that we just created
def destroyInit():
    return
#(Might not be needed) deletes the init file created by CreateInit
def gatherOutput():
    return
#Takes the file created by ExecuteSim and parses it for the data that we need (either as an arraylist or dictionary)
def displayOutput():
    return
#Displays the parsed data gathered by GatherOutput
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