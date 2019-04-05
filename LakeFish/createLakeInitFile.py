'''
Created on Feb 24, 2019

@author: Tyler, Cameron
'''
import subprocess

def gatherPost(userInput):
    inputFromPost = {
        'LakeName' : 'Carl',
        'FutureSim' : 'N',
        'ScenarioNum' : 2,
        'ISTATE' : 'Minnesota',
        'ISTATION' : 'Duluth',
        'MONTH' : 4,
        'ISTART' : 16,
        'MYEAR' : 2007,
        'FMON' : 12,
        'FDAY' : 31,
        'FYEAR' : 2008,
        'MBOT' : 32,
        'ZDEPTH' : ['1.25','1.5','2','2.5','3','3.5','4','5','7','9','11','14','17','20','23','26','30','34','38','42','46','50'],
        'ZMAX' : 50.0,
        'ST' : 458,
        'IPROFILE' : 1,
        'XK' : .37,
        'EMCOE3' : 20,
        'CZS' : -1.84,
        'WSTR' : .95,
        'WSSF' : .95,
        'COEWIN' : 1,
        'SNCOE' : 1,
        'AHTBTM' : .035,
        'SRCP' : 550,
        'CFSNOW' : .6,
        'CDIS0' : 2.6,
        'CNDSNW0' : .27,
        'CNDWI' : 11.35,
        'BTICE' : .17,
        'ALFICE' : .55,
        'GMICE' : 1.6,
        'BTSNOW' : .34,
        'ALFSNOW' : .8,
        'GMSNOW' : 40,
        'THICKIS' : 0,
        'THICKSN' : 0,
        'T2' : 4,
        'C2' : 0,
        'CD2' : 0,
        'CHLA2' : 0.00385,
        'IPRNT4' : 1,
        'PA2' : 0,
        'BOD2' : 0.75,
        'DSO2' : 10,
        'BODK20' : .1,
        'SB20' : .5,
        'XKR1' : .1,
        'POMAX' : 9.6,
        'EMCOE2' : 2.2,
        'EMCOE1' : 1,
        'EMCOE4' : 1,
        'EMCOE5' : 1,
        'IPRNT2' : 0,
        'IPRNT5' : 0,
        'IPRNT6' : 0,
        'NPRINT' : 5,
        'INFLOW' : 0,
        'IFIELD' : 1,
        'ICHLA-ICHLA' : 1,
        'I_NO_FIELD_OUT_PROFILE' : 0,
        'NDEPTH' : 5,
        'FDEPTH' : [1,10,20,30,48],
        'NDAYO' : 1,
        'NYEAR' : 1}
    createInit(imputFromPost)

    #takes in the post from the user and saves them in a dictionary that can be accessed by createInit
def createInit(userInput):
    with open('D:/Lake2019/User/Lake_Input.ini', 'w') as configfile:
        configfile.write('201208,"Version number Year & Month Developed"\n')
        #Abbv Lake name
        configfile.write('%s\r\n' % userInput['LakeName'])
        #Y\n for future climate
        configfile.write('%s\r\n' % userInput['FutureSim'])
        #Future scenario number
        configfile.write('%s\r\n' % userInput['ScenarioNum'])
        #TAPE8 Number - unknown for now
        configfile.write('$\n\n\n\n\n\n$\n\n\n')
        #configfile.write('$\r\nTAPE8 - weather data file name is read from station.dat file now\r\n')
        state, city = handleStationName(userInput['ISTATION'], userInput['ISTATE'])
        #station state number and city number
        configfile.write('%d,%d\r\n$\r\n\n\n\n\n\n\n' % (state, city))
        #start date, all the way to end date. rewrite this if its just
        # 2 dates passed in
        configfile.write('%d,%d,%d,%d,%d,%d\r\n$\n\n\n\n\n' % (userInput['MONTH'], userInput['ISTART'], 
                         userInput['MYEAR'], userInput['FMON'], userInput['FDAY'], userInput['FYEAR']))
        # num horizontal layers, max depth, initial elevation,
        # iprofile = 1 if all same, =0 if different
        configfile.write('%d,%d,%d,%d\r\n$\r\n\n\n\n\n' % (userInput['MBOT'], 
                         userInput['ZMAX'], userInput['ST'], userInput['IPROFILE']))
        # xk1 - light attenuation coeff for water, xk2 - light attenuation coeff for chloro
        # emcoe(3) - constant for kw computation, czs - constant
        configfile.write('%.2f,%d,%.3f\r\n$\r\n\n\n' % (userInput['XK'], userInput['EMCOE3'], userInput['CZS']))
        # wstr wind coeff for summer, wssf wind coeff for fall
        configfile.write('%.2f,%.2f\r\n$\r\n\n\n' % (userInput['WSTR'], userInput['WSSF']))
        # coewin - wind sheltering param, #sncoe - snow to ice thickness ratio
        configfile.write('%d,%d\r\n$\r\n\n\n' % (userInput['COEWIN'], userInput['SNCOE']))
        # ahtbtm - thermal diffusivity of sediment, srcp - density for sediment
        configfile.write('%.3f,%d\r\n$\r\n\n\n\n\n\n' % (userInput['AHTBTM'], userInput['SRCP']))
        # cfsnow - snow compaction factor, cdis0 - ice conductivity, #cndsnw0 - snow conductivity
        # cndwi - turbulent conductive heat transfer coeff, #depthc = 0.1 M
        configfile.write('%.1f,%.1f,%.2f,%.2f,.1\r\n$\r\n\n\n\n\n\n\n' % (userInput['CFSNOW'], userInput['CDIS0'], 
                         userInput['CNDSNW0'], userInput['CNDWI']))
        # btice - absorption coeff for ice, alfice - reflectivity coeff for ice, gmice - attenu for ice
        # btsnow - absorption coeff for snow, alfsnow - reflectivity for snow, gmsnow - attenu for snow
        configfile.write('%.2f,%.2f,%.2f,%.2f,%.2f,%d\r\n$\r\n\n\n' % (userInput['BTICE'], userInput['ALFICE'], userInput['GMICE'],
                          userInput['BTSNOW'], userInput['ALFSNOW'], userInput['GMSNOW']))
        # thickis - initial ice thickness, thicksn - initial snow thickness
        configfile.write('%d,%d\r\n$\r\n' % (userInput['THICKIS'], userInput['THICKSN']))
        # Z(I), I=1, MBOT) - 10 depths per line. First line fixed value
        # Need to figure out better how these depths are calculated. Not final
        configfile.write('%12 (Z(I),I=1,MBOT) - Z-Initial water depth, 8 depths per line\r\n')
        configfile.write('%12 DO NOT change the first line - these small depths for predict ice formation\r\n')
        configfile.write('0.02,0.06,0.1,0.2,0.3,0.4,0.5,0.6,0.8,1\r\n')
        for i in range(0, userInput['MBOT'] - 10):
            configfile.write('%s' % userInput['ZDEPTH'][i])
            if ((i+1)%10 == 0) | (i == userInput['MBOT'] - 11):
                configfile.write('\r\n')
            else:
                configfile.write(',')
        configfile.write('$\r\n\n')
#         configfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\r\n' % (userInput['ZDEPTH'][0], userInput['ZDEPTH'][1],
#                          userInput['ZDEPTH'][2], userInput['ZDEPTH'][3], userInput['ZDEPTH'][4], userInput['ZDEPTH'][5],
#                          userInput['ZDEPTH'][6], userInput['ZDEPTH'][7], userInput['ZDEPTH'][8], userInput['ZDEPTH'][9]))
#         configfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\r\n' % (userInput['ZDEPTH'][10], userInput['ZDEPTH'][11],
#                          userInput['ZDEPTH'][12], userInput['ZDEPTH'][13], userInput['ZDEPTH'][14], userInput['ZDEPTH'][15],
#                          userInput['ZDEPTH'][16], userInput['ZDEPTH'][17], userInput['ZDEPTH'][18], userInput['ZDEPTH'][19]))
#         configfile.write('%s,%s\r\n$\r\n\n' % (userInput['ZDEPTH'][20], userInput['ZDEPTH'][21]))
        # T2 - initial water temp
        configfile.write('%d\r\n$\r\n\n' % userInput['T2'])
        # C2 - initial suspended solids
        configfile.write('%d\r\n$\r\n\n' % userInput['C2'])
        # CD2 - initial total dissolved solids
        configfile.write('%d\r\n$\r\n\n' % userInput['CD2'])
        # CHLA2 - initial chloro concentration
        configfile.write('%.5f\r\n$\r\n\n' % userInput['CHLA2'])
        # IPRNT(4) - 0 means temp only, 1 means temp and DO
        configfile.write('%d\r\n$\r\n\n' % userInput['IPRNT4'])
        # PA2 - available phosphorus concentration
        configfile.write('%d\r\n$\r\n\n' % userInput['PA2'])
        # BOD2 - total BOD (?) in mg/L
        configfile.write('%.2f\r\n$\r\n\n' % userInput['BOD2'])
        # DSO2 - dissolved oxygen in mg/L
        configfile.write('%d\r\n$\r\n\n\n\n\n' % userInput['DSO2'])
        # BODK20 - detrital decay rate, SB20 - sedimentary oxygen demand, XKR1 - algal respiration rate
        # POMAX - maximum photosynthesis ratio
        configfile.write('%.1f,%.1f,%.1f,%.1f\r\n$\r\n\n\n\n\n' % (userInput['BODK20'], userInput['SB20'],
                         userInput['XKR1'], userInput['POMAX']))
        # EMCOE(2) - multiplier for SOD below euphotic zone, EMCOE(1) - metalimnion calibration factor
        # EMCOE(4) - hypolimnion calibration factor, EMCOE(5) hypolimnion calibration factor for chla
        configfile.write('%.1f,%d,%d,%d\r\n$\r\n\n\n\n' % (userInput['EMCOE2'], userInput['EMCOE1'],
                         userInput['EMCOE4'], userInput['EMCOE5']))
        # IPRNT(2) - 0 = no output file, 1 = output file TAPE8.ou, IPRNT(5) - 0 = no plot file, 1 = plot file TAPE8.p1
        # IPRNT(6) - num depths where plot(I) was simulated
        configfile.write('%d,%d,%d\r\n$\r\n\n\n' % (userInput['IPRNT2'], userInput['IPRNT5'], userInput['IPRNT6']))
        # NPRINT - interval between days for tabular output, INFLOW - number of in/out sources in inflow data
        configfile.write('%d,%d\r\n$\r\n\n\n\n' % (userInput['NPRINT'], userInput['INFLOW']))
        # IFIELD - 0 if no field data, 1 if field data for temp/DO profiles
        # ICHLA-ICHLA - 0 if no field data, 1 if field data for chl-a
        # I_NO_FIELD_OUT_PROFILE - 1 - no field data, but output sim profiles. =0 auto if IFIELD = 1
        configfile.write('%d,%d,%d\r\n$\r\n\n' % (userInput['IFIELD'], userInput['ICHLA-ICHLA'], userInput['I_NO_FIELD_OUT_PROFILE']))
        # NDEPTH - number of depths to output time-series of simulated temperature
        configfile.write('%d\r\n$\r\n\n' % userInput['NDEPTH'])
        # FDEPTH - specify depths from NDEPTH parameter
        for i in range(0, userInput['NDEPTH']):
            configfile.write('%d' % userInput['FDEPTH'][i])
            if i+1 != userInput['NDEPTH']:
                configfile.write(',')
        configfile.write('\r\n$\r\n\n\n')
        # NDAYO, NYEAR - total number of dates and years where you have field data
        configfile.write('%d,%d\r\n\n\n' % (userInput['NDAYO'], userInput['NYEAR']))
        # KYEAR, NTDY - specifies the year and how many days you have field data
        # NPRNT(K) - specifies above months and dates
        with open('D:/Lake2019/user/Field_Data.txt') as fieldData:
            dataLine = fieldData.readline()
            while(dataLine):
                configfile.write('%s\r' % dataLine)
                dataLine = fieldData.readline()
    configfile.close()
    return True
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
            stateNum = int(x[0])
            if(x[7] == cityName):
                cityNum = int(x[1])        
    return stateNum, cityNum

if __name__ == "__main__":

    inputDictionary = {
        'LakeName' : 'Carl',
        'FutureSim' : 'N',
        'ScenarioNum' : 2,
        'ISTATE' : 'Minnesota',
        'ISTATION' : 'Duluth',
        'MONTH' : 4,
        'ISTART' : 16,
        'MYEAR' : 2007,
        'FMON' : 12,
        'FDAY' : 31,
        'FYEAR' : 2008,
        'MBOT' : 32,
        'ZDEPTH' : ['1.25','1.5','2','2.5','3','3.5','4','5','7','9','11','14','17','20','23','26','30','34','38','42','46','50'],
        'ZMAX' : 50.0,
        'ST' : 458,
        'IPROFILE' : 1,
        'XK' : .37,
        'EMCOE3' : 20,
        'CZS' : -1.84,
        'WSTR' : .95,
        'WSSF' : .95,
        'COEWIN' : 1,
        'SNCOE' : 1,
        'AHTBTM' : .035,
        'SRCP' : 550,
        'CFSNOW' : .6,
        'CDIS0' : 2.6,
        'CNDSNW0' : .27,
        'CNDWI' : 11.35,
        'BTICE' : .17,
        'ALFICE' : .55,
        'GMICE' : 1.6,
        'BTSNOW' : .34,
        'ALFSNOW' : .8,
        'GMSNOW' : 40,
        'THICKIS' : 0,
        'THICKSN' : 0,
        'T2' : 4,
        'C2' : 0,
        'CD2' : 0,
        'CHLA2' : 0.00385,
        'IPRNT4' : 1,
        'PA2' : 0,
        'BOD2' : 0.75,
        'DSO2' : 10,
        'BODK20' : .1,
        'SB20' : .5,
        'XKR1' : .1,
        'POMAX' : 9.6,
        'EMCOE2' : 2.2,
        'EMCOE1' : 1,
        'EMCOE4' : 1,
        'EMCOE5' : 1,
        'IPRNT2' : 0,
        'IPRNT5' : 0,
        'IPRNT6' : 0,
        'NPRINT' : 5,
        'INFLOW' : 0,
        'IFIELD' : 1,
        'ICHLA-ICHLA' : 1,
        'I_NO_FIELD_OUT_PROFILE' : 0,
        'NDEPTH' : 5,
        'FDEPTH' : [1,10,20,30,48],
        'NDAYO' : 100,
        'NYEAR' : 10}
    createInit(inputDictionary)
    subprocess.call([r'D:\lake2019\user\RunMinlake.bat'])
