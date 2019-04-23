'''
Created on Feb 24, 2019

@author: Tyler, Cameron
'''
import subprocess
import os
import shutil

def gatherPost(userInput):
    inputFromPost = {}
    #if(userInput['LakeName'] == ''):
    #    inputFromPost['LakeName'] = 'default'
    #else:
    #    inputFromPost['LakeName'] = userInput['LakeName']
    inputFromPost['LakeName'] = userInput['LakeName']
    inputFromPost['FutureSim'] = 'N'
    inputFromPost['ScenarioNum'] = 2 
    inputFromPost['ISTATE'] = userInput['ISTATE'] 
    inputFromPost['ISTATION'] = userInput['ISTATION']
    inputFromPost['MONTH'] = int(userInput['MONTH'])
    inputFromPost['ISTART'] = int(userInput['ISTART'])
    inputFromPost['MYEAR'] = int(userInput['MYEAR'])
    inputFromPost['FMON'] = int(userInput['FMON'])
    inputFromPost['FDAY'] = int(userInput['FDAY'])
    inputFromPost['FYEAR'] = int(userInput['FYEAR']) 
    try: 
        int(userInput['num_horiz_layers']) 
        inputFromPost['MBOT'] = int(userInput['num_horiz_layers'])
    except:
        inputFromPost['MBOT'] = 0
    #inputFromPost['MBOT'] = int(userInput['num_horiz_layers'])
    inputFromPost['ZDEPTH'] = ['1.25','1.5','2','2.5','3','3.5','4','5','7','9','11','14','17','20','23','26','30','34','38','42','46','50']
    try: 
        float(userInput['max_depth'])
        inputFromPost['ZMAX'] = float(userInput['max_depth']) 
    except:
        inputFromPost['ZMAX'] = 50.0
    try: 
        inputFromPost['ST'] = float(userInput['elevation']) 
    except: 
        inputFromPost['ST'] = 0
    inputFromPost['IPROFILE'] = 1 
    inputFromPost['XK'] = .37 
    inputFromPost['EMCOE3'] = 20 
    inputFromPost['CZS'] = -1.84 
    try:        
        inputFromPost['WSTR'] = float(userInput['wind_sheltering_summer'])
    except:
        inputFromPost['WSTR'] = .95
    try:       
        inputFromPost['WSSF'] = float(userInput['wind_sheltering_fall']) 
    except:
        inputFromPost['WSSF'] = .95
    try:        
        inputFromPost['COEWIN'] = float(userInput['temp_wind_sheltering']) 
    except:
        inputFromPost['COEWIN'] =1
    try:        
        inputFromPost['SNCOE'] = float(userInput['snow_ice_ratio']) 
    except:
        inputFromPost['SNCOE'] = 1
    inputFromPost['AHTBTM'] = .035 
    try:        
        inputFromPost['SRCP'] = float(userInput['sediment_density']) 
    except:
        inputFromPost['SRCP'] = 550
    try:        
        inputFromPost['CFSNOW'] = float(userInput['snow_compact']) 
    except:
        inputFromPost['CFSNOW'] =.6
    try:        
        inputFromPost['CDIS0'] = float(userInput['ice_conduct']) 
    except:
        inputFromPost['CDIS0'] = 2.6
    try:        
        inputFromPost['CNDSNW0'] = float(userInput['snow_conduct']) 
    except:
        inputFromPost['CNDSNW0'] = .27
    try:        
        inputFromPost['CNDWI'] = float(userInput['wcht_coefficient']) 
    except:
        inputFromPost['CNDWI'] = 11.35
    try:        
        inputFromPost['BTICE'] = float(userInput['ice_abs_coefficient'])
    except:
        inputFromPost['BTICE'] = .17
    inputFromPost['ALFICE'] = .55 
    try:        
        inputFromPost['GMICE'] = float(userInput['ice_attn_coefficient']) 
    except:
        inputFromPost['GMICE'] = 1.6
    try:        
        inputFromPost['BTSNOW'] = float(userInput['snow_abs_coefficient']) 
    except:
        inputFromPost['BTSNOW'] = .34
    try:        
        inputFromPost['ALFSNOW'] = float(userInput['snow_reflect_coefficient']) 
    except:
        inputFromPost['ALFSNOW'] = .8
    try:        
        inputFromPost['GMSNOW'] = float(userInput['snow_attn_coefficient']) 
    except:
        inputFromPost['GMSNOW'] = 40
    try:        
        inputFromPost['THICKIS'] = float(userInput['init_ice_thickness']) 
    except:
        inputFromPost['THICKIS'] = 0
    try:        
        inputFromPost['THICKSN'] = float(userInput['init_snow_thickness']) 
    except:
        inputFromPost['THICKSN'] =0
    try:        
        inputFromPost['T2'] = float(userInput['init_water_temps']) 
    except:
        inputFromPost['T2'] = 4
    try:        
        inputFromPost['C2'] = float(userInput['init_sus_solids_conc']) 
    except:
        inputFromPost['C2'] = 0
    try:        
        inputFromPost['CD2'] = float(userInput['total_diss_solids']) 
    except:
        inputFromPost['CD2'] = 0
    inputFromPost['CHLA2'] = 0.00385 
    inputFromPost['IPRNT4'] = 1 
    try:        
        inputFromPost['PA2'] = float(userInput['init_phos_conc']) 
    except:
        inputFromPost['PA2'] = 0
    inputFromPost['BOD2'] = 0.75 
    inputFromPost['DSO2'] = 10 
    try:
        inputFromPost['BODK20'] = float(userInput['det_decay_rate'])
    except:
        inputFromPost['BODK20'] = .1
    inputFromPost['SB20'] = .5 
    try:
        inputFromPost['XKR1'] = float(userInput['alg_resp_rater']) 
    except:
        inputFromPost['XKR1'] = .1
    try:
        inputFromPost['POMAX'] = float(userInput['max_photo_ratio'])  
    except:
        inputFromPost['POMAX'] = 9.6
    inputFromPost['EMCOE2'] = 2.2 
    inputFromPost['EMCOE1'] = 1        
    inputFromPost['EMCOE4'] = 1 
    inputFromPost['EMCOE5'] = 1 
        #inputFromPost['IPRNT2'] = int(userInput['is_outflow_file']) 
    inputFromPost['IPRNT2'] = 0 
    #inputFromPost['IPRNT5'] = int(userInput['is_plot_file'])  
    inputFromPost['IPRNT5'] = 0 
    #inputFromPost['IPRNT6'] = int(userInput['num_depth_plots'])
    inputFromPost['IPRNT6'] = 0        
    try:
        inputFromPost['NPRINT'] = int(userInput['tab_data_interval']) 
    except:
        inputFromPost['NPRINT'] = 5
        #inputFromPost['INFLOW'] = int(userInput['inflow_outflow_source'])
    inputFromPost['INFLOW'] = 0 
    inputFromPost['IFIELD'] = 1 
    inputFromPost['ICHLA-ICHLA'] = 1 
    inputFromPost['I_NO_FIELD_OUT_PROFILE'] = 0 
    try:        
        inputFromPost['NDEPTH'] = int(userInput['time_series_output'])
    except:
        inputFromPost['NDEPTH'] = 5
    inputFromPost['FDEPTH'] = [1,10,20,30,48] 
    inputFromPost['NDAYO'] = 1 
    inputFromPost['NYEAR'] = 1 

    createInit(inputFromPost)
    pathHere = os.path.abspath(os.path.dirname(__file__))
    batFile = os.path.join(pathHere, 'RunMinlake.bat')
    p = subprocess.Popen(batFile, shell=True, stdout = subprocess.PIPE)

    #takes in the post from the user and saves them in a dictionary that can be accessed by createInit
def createInit(userInput):
    pathHere = os.path.abspath(os.path.dirname(__file__))
    with open(os.path.join(pathHere,'Lake_Input.ini'), 'w') as configfile:
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
        state, city, stateAbbv = handleStationName(userInput['ISTATION'], userInput['ISTATE'])
        #station state number and city number
        configfile.write('%d,%d\r\n$\r\n\n\n\n\n\n\n' % (state, city))
        
        #write to path.txt file
        with open(os.path.join(pathHere, 'path.txt'), 'w') as pathfile:
            pathfile.write('%s\n' % os.path.abspath(os.path.join(pathHere, '#OUTPUT', 'TEMPDO')))
            pathfile.write('%s\\\n' % os.path.abspath(os.path.join(pathHere, '..', '#COMMON', 'MeteorologicalData', stateAbbv)))
            pathfile.write('%s\\' % os.path.abspath(os.path.join(pathHere, '..', '#COMMON', 'FIXED_INPUT')))
        pathfile.close()
  
        
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
        with open(os.path.join(pathHere, 'Field_Data.txt')) as fieldData:
            dataLine = fieldData.readline()
            while(dataLine):
                configfile.write('%s\r' % dataLine)
                dataLine = fieldData.readline()
        fieldData.close()
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

# Moves init file into user file location
def saveLake(username, filename):
    pathHere = os.path.abspath(os.path.dirname(__file__))
    if not os.path.exists(os.path.join(pathHere, 'userdata', username)):
        os.makedirs(os.path.join(pathHere, 'userdata', username))
    shutil.move(os.path.join(pathHere, 'Lake_Input.ini'), os.path.join(pathHere, 'userdata', username, filename, '.ini'))
    return

# Loads a list of lake names from the user's init files
def loadLakeList(username):
    pathHere = os.path.abspath(os.path.dirname(__file__)) 
    mypath = os.path.join(pathHere, 'userdata', username)
    if not os.path.exists(mypath):
        return
    onlyfiles = [f for f in os.listdir(mypath) if os.path.isfile(os.path.join(mypath, f))]
    lakeList = [''] * len(onlyfiles)
    for i in range(0, len(onlyfiles)):
        lakeList[i] = onlyfiles[i][0:len(onlyfiles[i]) - 4]
    return lakeList

# Loads a specific lake from the user's init files
def loadLake(username, lakename):
    pathHere = os.path.abspath(os.path.dirname(__file__)) 
    shutil.copy(os.path.join(pathHere, 'userdata', username, lakename,'.ini'), os.path.join(pathHere, 'Lake_Input.ini'))
    return

def handleStationName(cityName, stateName):
    pathHere = os.path.abspath(os.path.dirname(__file__))
    statFile =  open(os.path.join(pathHere, '..', '#common', 'FIXED_INPUT', 'station.dat'))
    stationList = []
    stateNum = 0
    cityNum = 0
    stateAbbv = ''
    for line in statFile:
        stationList += [line.split()]
    for x in stationList:
        if(x[6] == stateName):
            stateNum = int(x[0])
            stateAbbv = x[2]
            if(x[7] == cityName):
                cityNum = int(x[1])        
    return stateNum, cityNum, stateAbbv

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
#    loadLakeList('tms0050')
#    loadLake('tms0050', 'Lake1')
    pathHere = os.path.abspath(os.path.dirname(__file__))
    batFile = os.path.join(pathHere, 'RunMinlake.bat')
    p = subprocess.Popen(batFile, shell=True, stdout = subprocess.PIPE)
#    saveLake('tms0050', 'Lake1')
