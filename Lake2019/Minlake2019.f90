!************************************************************!
!                                                            !
!     UPDATE 08-01-93 FOR TESTING HYPOLIMNETIC Kz            !
!     UPDATE 05-30-97 FOR SNOW/ICE ALBEDO TESTS              !
!     UPDATE 12-16-99 FOR ATP PROJET                         !
!                                                            !
!     UPDATE 2008 - 2009 FOR MN DNR PROJECT                  !
!     Update 2009 for Lake Mead, NV (water surface changes)  !
!                                                            !
!         LAKE MAIN PROGRAM  UPDATED 09-03-1986              !
!         ATTATCH USER SUBROUTINE DURING LINKING             !
!         COMPILED ON Z-158 USING MS-FORTRAN 3.3             !
!************************************************************!
!!              Note : Shoeb Alam sep 2009
!!!        Current version we can run in following situation
!! Field data available - 1986, 1996,2006, 2008 and shown in input file
!! input file date- 4 15 1985- 12 31 2008 - ok
!! input file date- 4 15 1985- 12 31 1996 - ok
!! input file date- 4 15 2005- 12 31 2008 - ok

PROGRAM MINLAKE
REAL*8 A,V,TV,ATOP,SVOL,RHO,DY0,ASX
CHARACTER KNAME*20,ASTATE*2
INTEGER FMON,FDAY,FYEAR,T_FLAG,DO_FLAG,DAYBAK,YEARBAK
INTEGER JX0,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JULIANDAY,FIRSTDAY(12),EXTRADAY
COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)
COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
COMMON/ILAKEN/ILAKENAME,ILAKENAMES 
COMMON/CHLAP/NCDAY(120,2),GCHLA(120,2),ICHLASD,IFIELD,CHLTOP(90),CHLBOT(90),I_NO_FIELD_OUT_PROFILE
COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),PR(31),DRCT(31)
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/SOURCE/RM(3,120),PROD(120),XMR(3,120),PRODSUM(120)
COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
COMMON/PHYTO0/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),GROMAX(3),TMAX(3)
COMMON/PHYTO1/TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN(90),CHLMAX,SDY(90)
COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),REPRO,XI,XKMZ,GRAZE(3,120)
COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL  
COMMON/SUB/SDZ(180),SZ(180),LAY(120),AVGI(24,180),SVOL(180)
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
COMMON/INFLOWX/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
COMMON/NETHEAT/Q(120)
!  COMMON/HEATFACTOR_CHECK/HEATFACTOR
COMMON/US/VELTRE
COMMON/TEFX/T2K(120),TEHE(120),BMK(120),OLDHQ,ITERF 
COMMON/FIELD/IFLAG(10),FLDATA(10,120),DEPTH(120),NFLD(10),SD,NSKIPDAY,NSDAY(640),SDFIELD(640),TAPE64
COMMON/FILEX/DIN,MET,FLO,TAPE8,TAPE1,IREC
COMMON/TITL/TITLE1,TITLE2,TITLE3
COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(81,36),ELEV(81,36)
COMMON/BAKCUPTDO/T_FLAG,DO_FLAG,MONBAK,DAYBAK,YEARBAK,TBAK(120),DOBAK(120)
COMMON/FWEA/FTAR(12),FDEW(12),FWIN(12),FTCL(1200),FPRE(12),FSOL(12)
COMMON/GCM1/FUTR_PRECIP_CHANGE(12),FUTR_HUM_CHANGE(12),FUTR_TEMP_CHANGE(12),FUTR_SOLAR_CHANGE(12),FUTR_NWIND_CHANGE(12),FUTR_EWIND_CHANGE(12),FUTR_WIND_CHANGE(12)
COMMON/GCM2/GRID_PRECIP(400,400,12),GRID_HUM(400,400,12),GRID_TEMP(400,400,12),GRID_SOLAR(400,400,12),GRID_NWIND(400,400,12),GRID_EWIND(400,400,12),GRID_WIND(400,400,12)
COMMON/GCM3/GRID_LATITUDE(400),GRID_LONGITUDE(400)
COMMON/MODEL/MODELS_SCENARIO
COMMON/NEW1/ICESTAR
COMMON/WELEVATION/STW
COMMON/FXDEBUGE/IBUG1
COMMON/DATA50/ICOUNTER,SMDATA(2,120),ISD
COMMON/CHL_A/ICHL_A  
COMMON/MIXED/DMAXX,DMINN,DIVIDE_MAX,DIVIDE_MIN
COMMON/EMCO/EMCOE_READ
COMMON/NAMELAKE/TAPE514
COMMON/NDAYOCOUNTER/NDAYO_COUNTER
COMMON/FUTURE/FCO2
COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
COMMON/PICE/QNEG,THICE2,TBOUN,NP,QNET,HS,HA,HBR,HE,HC,YTAIR
DIMENSION ZTS(2,120),XTS(2,120)

DIMENSION STATVAR(120),ICX(4),TETH(5),DOTH(5),TEICE(5)
DIMENSION TECON(120),TEDIF(120),TEMIX(120),TSAVE(120)
! Latitude, longitude of stations and No. station in a state
DIMENSION SLATD(81,36),SLONG(81,36),JSTA(81),EVAP(20)
DIMENSION XJAIR(81,36),SUMTSL(10)
CHARACTER*100 FILENAME             
CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1,TAPE9,TAPE7
CHARACTER*1 FCO2          
CHARACTER*25 PATHFILE
CHARACTER*4 TAPE514
CHARACTER*20 TAPE14    
!Next for output files are created to act as input for 45 degree line (simulated-vs- data line) plot
CHARACTER*11 TAPE64   !Hill-SD.dat for Secchi depth
CHARACTER*80 TITLE1,TITLE2,TITLE3
CHARACTER*80 RUNPATH,LAKE_AREA_FILE,LAKE_INPUT_FILE
CHARACTER*80 PLOTPATH,METERPATH
CHARACTER*80 FIXED_FILE_PATH
CHARACTER*80 RUNPATH1,RUNPATH2,RUNPATH3
CHARACTER*1 PATH(80),PATHM(80),PATHP(80),PATH45(80),PATH_F(80)
CHARACTER*1 T8(40),XS,NUM(10),T9(40),T7(40),FFILE(100),T14(21),T64(11),T514(4)
CHARACTER*1 FILE4(40),FILE7(40),FILE13(40),FILE14(40),FILE15(40)
CHARACTER*10 PROFILE
EQUIVALENCE (STATVAR(1),SUMXY(1)),(TAPE8,T8(1))
EQUIVALENCE (TAPE64,T64(1))
EQUIVALENCE (TAPE7,T7(1)),(TAPE14,T14(1)),(TAPE9,T9(1)),(FLO,FILE4(1)),(FILENAME,FFILE(1)),(TAPE514,T514(1))
EQUIVALENCE (METERPATH,PATHM(1)),(PLOTPATH,PATHP(1))
EQUIVALENCE (RUNPATH,PATH(1))
EQUIVALENCE (FIXED_FILE_PATH,PATH_F(1))
LOGICAL LFILE

DATA FIRSTDAY/1,32,60,91,121,152,182,213,244,274,305,335/ 
DATA NUM/'0','1','2','3','4','5','6','7','8','9'/	
CHARACTER(len=201)  ::newfolder
CHARACTER StateStation*6

newfolder = './#OUTPUT/'     
CALL CREATDIRECTORY(newfolder)
newfolder = './#OUTPUT/PLOTIT/'     
CALL CREATDIRECTORY(newfolder)
newfolder = './#OUTPUT/TEMPDO/'     
CALL CREATDIRECTORY(newfolder)
newfolder = './#ANALYSIS/'     
CALL CREATDIRECTORY(newfolder)
       
!WRITE(*,*)'Sediment heat, 1.0 for Yes or 0.0 for No '
!READ(*,*) HEATFACTOR
!FCO2 - CHECKING RUN FOR PAST CLIMATE OF FUTURE CLIMATE
!WRITE(*,*)'Enter 0 or 1 (0 - Constant XK1(-1.84), 1 - Field SD value (+1.84))' 
!XK1 - LIGHT ATTENUATION COEFICIENT FOR WATER(1/M)
!WRITE(*,*)'Constant XK1(-1.84) '  
!IF(EMCOE_READ.EQ.0)EMCOE(3)=-1.84
!IF(EMCOE_READ.EQ.1)EMCOE(3)=1.84
!READ(*,*) EMCOE_READ
!EMCOE_READ=0

!ISD = 0 is XK1 and XK2 are constant from input data
!ISD = 1 XK1 is computed only at field data dates
!ISD = 2 XK1 is computed at some middle date between field data dates   
!READ(*,*) ISD
ISD=0
   
!WRITE(*,*) 'Enter 0 or 1, 1 for interpolated field Chl-a'
!READ(*,*)ICHL_A
ICHL_A=0  
    
!WRITE(*,*)'I_FULL_MIXING, ENTER 1 FOR MIXING 0 FOR NO MIXING'
!READ(*,*)I_FULL_MIXING 
I_FULL_MIXING=1         

! To print chlorophyll-a time series
JDYCHL=60
            
! FILE FOR READ            
OPEN(300,FILE="..\#COMMON\FIXED_INPUT\PARAMETER_MIXED.DAT")
READ(300,*)
READ(300,*) DMAXX,DMINN,DIVIDE_MAX,DIVIDE_MIN
! FILE FOR WRITE             
OPEN(310,FILE='#ANALYSIS\MIXED_DEPTH.DAT') 
WRITE(310,*)'DMAXX - Maximum temperature and DO difference within mixed layer'
WRITE(310,*)'DMINN - Minimum temperature and DO difference within mixed layer'
WRITE(310,*)'DIV_MAX - Maximum dividing constant (1/10th or 15th)'
WRITE(310,*)'DIV_MIN - Minimum dividing constant (1/10th or 15th)'
WRITE(310,*) DMAXX,DMINN,DIVIDE_MAX,DIVIDE_MIN
WRITE(310,*) '                        1st, 2nd MIXDEPTH, AVERAGE, SIMULATED, MONTH,MDAY,MYEAR'
             
! FILE FOR WRITE           
OPEN(800,FILE='#ANALYSIS\SD_CHL_ATTN_ANA.DAT')
WRITE(800,*)'        DAY   Atenua         SD(fld)         SD(sim)       Chl-a (sim)' 


!RUNPATH="#OUTPUT\TEMPDO\"
!METERPATH="..\#COMMON\MeteorologicalData\mn\"
!METERPATH="..\#COMMON\Weather\"

PLOTPATH="#OUTPUT\PLOTIT\" 

OPEN(316,FILE="PATH.TXT")
READ(316,'(A)') RUNPATH
READ(316,'(A)') METERPATH

!Open files to output intermediate results for Debuging or for advanced users
IBUG1=-1      
   
IF(IBUG1.GT.0) THEN
    OPEN(440,FILE='SOURCE.DAT')
    OPEN(450,FILE='BOD.DAT')
    OPEN(460,FILE='RESPIR.DAT')
    OPEN(470,FILE='SOD.DAT')
    OPEN(480,FILE='PHTOSYN.DAT') 
    OPEN(490,File='Reaeration.DAT')
    OPEN(465,File='SedTemp10m.DAT') ! near 1 m or surface layer
    OPEN(467,File='SedTempBot.DAT') ! at the deepest depth (layer)
    OPEN(485,File='SedHeatFlux.DAT')
ENDIF
              
!Alam May 2009, These files save text strings that can copy/paste into PlotIT profile plots. 
OPEN(471,FILE='#OUTPUT\PLOTIT\Plotit_help_Temp.dat')              
OPEN(473,FILE='#OUTPUT\PLOTIT\Plotit_help_DO.dat') 
OPEN(472,FILE='#OUTPUT\PLOTIT\Profile_help.dat')

FIXED_FILE_PATH="#OUTPUT\"

IRUN=0

!Fang, April 22, 2009 NUMFIELD=0 for tracking number of field data used
NUMFIELD=0
        
 !FILE FOR WRITE
OPEN(198,FILE='#ANALYSIS\warninfo.dat')
WRITE(198,1980)
    1980    FORMAT(1X,'This simulation started under the following conditions!')  
     
!Open the file "Temp_Gwater.dat" with yearly mean and January air temperatures
!YRAIR(I,J) - Yearly mean air temperature in (oC) - but Input is oF
!XJAIR(I,J) - January mean air temperature in (oC) - but Inpur is oF        
OPEN(42,FILE="..\#COMMON\FIXED_INPUT\Temp_Gwater.dat")
READ(42,*)
!JS =State   JN =WEATHER Station 
!YRAIR =AVG Air TEMP OF 12 MONTHS (YEAR)
!XJAIR =AVG Air TEMP OF JANUARY        
!ELEV = ELEVATION OF THE WEATHER STATION  
452 READ(42,*,END=454) JS,JN,YRAIR(JS,JN),XJAIR(JS,JN),ELEV(JS,JN)
GOTO 452
454 CLOSE(42)

!Fang, 2008 - Open a file ("Chla_timeseries.dat")to save information on Cholorophyll-a concentration
!FILE FOR WRITE             
OPEN(56,FILE="#ANALYSIS\Chla_timeseries.dat")

!YSCHL  =
!HSCSI  =
!CONST  =
!CHLMAX = Year to date maximum chloro -a concentration (mg/l). 
YSCHL=30.
HSCSI=0.03
CONST=0.5
CHLMAX=0.0

DO 995 I=1,6
995     IPRNT(I)=0
DO 996 I=1,10
996     IFLAG(I)=0
DO 997 I=1,80
997     STATVAR(I)=0.0

IF(IRUN.NE.1) THEN
    LAKE_INPUT_FILE="Lake_Input.ini"
    !Open main input file         
    OPEN (7,FILE=LAKE_INPUT_FILE)
    !Open analysis file                
    OPEN(199,FILE='#ANALYSIS\Input_parameters_infor.ini')
    READ(7,*)
    READ(7,'(A)') TAPE514
    READ(7,*) FCO2
    READ(7,*) MODELS_SCENARIO
    READ(7,*)
    READ(7,*)
    READ(7,*)
    READ(7,*)
    READ(7,*)             
    READ(7,*)
    READ(7,*)
    READ(7,*)
    READ(7,*) 
    READ(7,*) ISTATE,ISTATION
    READ(7,*)
    READ(7,*)
    READ(7,*)
    READ(7,*) 
    READ(7,*)
    READ(7,*)
    READ(7,*)
    READ(7,*) MONTH,ISTART,MYEAR,FMON,FDAY,FYEAR
    
    !Fang, 2008 - Set lake surface initial elevation as the elevation of weather station
    !Fang, 2000, STW is elevation of weather station          
    STW=ELEV(ISTATE,ISTATION)
    !Open Station.dat file
    OPEN (78,FILE="..\#COMMON\FIXED_INPUT\station.dat")                         
    READ(78,*)
    LAT_FLAG = 0   
    352     READ(78,*,END=354) JS,JN,ASTATE,SLAT,SLON,STATESTATION               
    IF(JS.EQ.ISTATE.AND.JN.EQ.ISTATION) THEN
        LAT_FLAG = 1
    ELSE
        GOTO 352
    ENDIF 
    354 CLOSE(78)
    TAPE8 = STATESTATION
    T8(7) = NUM(INT((MYEAR-100*INT(MYEAR/100.0))/10.0)+1)
    T8(8) = NUM(MYEAR-10*INT(MYEAR/10.0)+1)              
    IF(LAT_FLAG.EQ.0) THEN
        WRITE(99,*)"LATITUDE & LONGITUDE were not found in file","- station.dat!"
        STOP
    ENDIF
    !"P" indicate for past climate conditions
    T7(1)='P'
    T9(1)='P'        
    DO 405 I=1,16
        II=16-I+1
        DO 406 IK=2,II
	        T7(IK)=T8(IK-1)
	        T9(IK)=T8(IK-1)
        406 CONTINUE
        IF(T8(II).NE.' ') THEN
            T8(II+1)='.'
            T8(II+2)='D'
            T8(II+3)='A'
            T8(II+4)='T'
            !TAPE7 SIMULATED DO CONCENTRATION
		    T7(II+2)='.'
            T7(II+3)='D'
            T7(II+4)='O'
            T7(II+5)='X'
            !TAPE9 SIMULATED TEMPERATURE
		    T9(II+2)='.'
            T9(II+3)='T'
            T9(II+4)='E'
            T9(II+5)='P'
            INUM=II-1
    GOTO 402
        ENDIF
    405    CONTINUE
    402    CALL MAKEFILE(PATHM,T8,FFILE)
    INQUIRE(FILE=FILENAME,EXIST=LFILE)
    IF(LFILE) GOTO 412
    PAUSE 'ERROR ON METEOROLIGICAL DATA FILE NAME'
    STOP            
    ! FILE FOR READ WEATHER DAT
    412    OPEN (9,FILE=FILENAME,STATUS='OLD')
    !OPEN FILE "TAPE9" FOR WATER TEMPERATURE SIMULATION
    !File 76 save simulated water temperature year by year
    !Open the file for the first year   	 
    IF(FCO2.EQ.'n'.OR.FCO2.EQ.'N') THEN
		    CALL MAKEFILE(PATH,T9,FFILE)
		    OPEN (76,FILE=FILENAME)
    ENDIF
!This is for  IF(IRUN.NE.1) THEN ! IRUN = 0 at the beginning of the program
ENDIF
!FLIE FOR READ AREA-DEPTH RELATION        
!WRITE(*,*) "READ LAKE_AREA_FILE"
LAKE_AREA_FILE="Lake_Area.sdf"
OPEN (38,FILE=LAKE_AREA_FILE)           
WRITE(*,*)'__________________________________________________________'
WRITE(*,*)
WRITE(*,*)'The name of the lake running for Simulation :  ',TAPE514
WRITE(*,*)
WRITE(*,*)'__________________________________________________________'
TAPE14=TAPE514
TAPE64=TAPE14           
!FILE FOR WRITE       
!Read Climate warming data
!FILE FOR WRITE
OPEN(330,FILE='#ANALYSIS\PAST_WEATHER.TXT')
WRITE(330,*)' M  YR    DAY   TEMP     DEW      WIND   RAD      CC     RAIN    SNOW' 
   
IF(FCO2.EQ.'y'.OR.FCO2.EQ.'Y') THEN
!MODELS_SCENARIO -1 for CCCma AGCM2, 2 for CGCM3.1 A1B Scenario, 5 for MIROC3.2 A1B Scenario
    IF(MODELS_SCENARIO.EQ.1)THEN            
	    T7(1)='F'
	    T9(1)='F'
        CALL MAKEFILE(PATH,T9,FFILE)
        OPEN (76,FILE=FILENAME)
        OPEN(335,FILE='#ANALYSIS\FUTURE_WEATHER_CCC.TXT')
        WRITE(335,*)' M  YR    DAY   TEMP     DEW      WIND   RAD      CC     RAIN    SNOW'  
    ENDIF
    IF(MODELS_SCENARIO.EQ.2)THEN
        OPEN(370,FILE='#ANALYSIS\GCM_A1B_CHANGE.TXT')
        T7(1)='G'
	    T9(1)='G'
        CALL MAKEFILE(PATH,T9,FFILE)
        OPEN (76,FILE=FILENAME)
        OPEN(340,FILE='#ANALYSIS\FUTURE_WEATHER_GCM_A1B.TXT')
        WRITE(340,*)' M  YR    DAY   TEMP     DEW      WIND   RAD      CC     RAIN    SNOW'  
    ENDIF                 
    IF(MODELS_SCENARIO.EQ.5)THEN
        OPEN(370,FILE='#ANALYSIS\MIOR_A1B_CHANGE.TXT') 
        T7(1)='M'
	    T9(1)='M'
	    CALL MAKEFILE(PATH,T9,FFILE)
        OPEN (76,FILE=FILENAME)
        OPEN(340,FILE='#ANALYSIS\FUTURE_WEATHER_MIHR_A1B.TXT')
        WRITE(340,*)' M  YR    DAY   TEMP     DEW      WIND   RAD      CC     RAIN    SNOW'  
    ENDIF      
ENDIF              
!CFX
!C** FOR WINTER SIMULATION STUDY (FILE "TD-SE.SDF")!
!C open temp and DO simul file at 5 specified depth
T64(5)='_'
T64(6)='S'
T64(7)='D'
T64(8)='.'
T64(9)='D'
T64(10)='A'
T64(11)='T'
OPEN (13,FILE="#OUTPUT\PLOTIT\Sim_Tem_Tsrs.dat")
OPEN (14,FILE="#OUTPUT\PLOTIT\Sim_DO_Tsrs.dat")
OPEN (2000,FILE="#ANALYSIS\K-Diff.DAT")
! Open thr file containg the days when temperature changes
! from less than 4 degree to more than 4 degree celcius, and full misisng is enforced
OPEN (2100,FILE="#ANALYSIS\FULL_MIX.DAT")
WRITE(2100,*)' MYEAR       MONTH        MDAY        TEMP1     TEMP2'

 !    WRITE(2000,*)'T/DO TYEAR MONTH DAY  JDAY  I     Z(I)     N^2     AMK'    
 !    WRITE(2000,*)
OPEN (18,FILE='#OUTPUT\SNOWICE.DAT')
WRITE(18,*) "YEAR,MONTH,DAY,JDY,THICE(m),THSNOW(m),SNOWFL(MDAY),ILAY,MBOT"
!This files will write temp and DO  errror on dates for plotting the error vs day
OPEN (113,FILE="#ANALYSIS\T_ERR_PLT.DAT")
OPEN (114,FILE="#ANALYSIS\DO_ERR_PLT.DAT")
WRITE(113,*) "Temperature"
WRITE(113,*) "Error = (SIMulated - MEAsured) and |  | means absoluate error"
IF(EMCOE(3).LT.0.0.OR.ISD.EQ.0)THEN
    WRITE(113,1140)
ELSE
    WRITE(113,1130)
ENDIF
WRITE(114,*) "DO"
WRITE(114,*) "Error = (SIMulated - MEAsured) and |  | means absoluate error"
IF(EMCOE(3).LT.0.0.OR.ISD.EQ.0)THEN
    WRITE(114,1140)
ELSE
    WRITE(114,1130)
ENDIF
1130         FORMAT(1X,'Month',2X,'Day',2X,'Year',2X,'JDay',2X,'No.Depth',1X,'DepthEmax',3X,'Error_max',2X,'DepthEmin',3X,'Error_min',2X,'|Error|mean')     
1140         FORMAT(1X,'Month',2X,'Day',2X,'Year',2X,'JDay',2X,'No.Depth',1X,'DepthEmax',3X,'Error_max',2X,'DepthEmin',3X,'Error_min',2X,'|Error|mean',5X,'Sim_SD')     

OPEN (99,FILE='#ANALYSIS\CHECK.DAT')
WRITE(99,1980)

!Set sediment temperature at 10 m below - TSLAV
!Increase 2.0 + 1.0 more for the region with snow
!Only increase 2.0 oC for other regions - July, 1996
!C20**

IF(FCO2.EQ.'y'.OR.FCO2.EQ.'Y') THEN
    CALL FSCENA(KNAME,SLON,SLAT)
    !FTAR(1) is increment of January air temperature
    !for 2xCO2 GCM climate conditions
   
    ! XXAIR=XJAIR(ISTATE,ISTATION)+FTAR(1)
    
    YJAN=FTAR(1)
    YTAIR=0.0
    DO 857 KT=1,12
	    YTAIR=YTAIR+FTAR(KT)/12.0
857 CONTINUE
   
  !  IF(XXAIR.LE.-3.0) THEN
  !	    TSLAV=YRAIR(ISTATE,ISTATION)+3.0+YTAIR
  !  ELSE
  !	    TSLAV=YRAIR(ISTATE,ISTATION)+2.0+YTAIR
  !  ENDIF
  
ELSE
    YJAN=0.0
    YTAIR=0.0
ENDIF    
    
    !IF(XJAIR(ISTATE,ISTATION).LE.-3.0) THEN
        !TSLAV=YRAIR(ISTATE,ISTATION)+3.0
    !ELSE
        !TSLAV=YRAIR(ISTATE,ISTATION)+2.0
    !ENDIF

! Changes made on July 18, 2018    
    XYAIR=YRAIR(ISTATE,ISTATION)+YTAIR
    XYJAN=XJAIR(ISTATE,ISTATION)+YJAN
    
! Sediment temperature profiles at 10 or 20 m
    IF(XYJAN.LE.-3.0) THEN
        KJ1=2
    ELSE
        KJ1=1
    ENDIF
    
    IF(XYAIR.LE.3.0) THEN
        TSLAV=YRAIR(ISTATE,ISTATION)+8.0+YTAIR
    ELSEIF(XYAIR.LE.6.0) THEN
        TSLAV=YRAIR(ISTATE,ISTATION)+7.0+YTAIR
    ELSEIF(XYAIR.LE.10.0) THEN
        TSLAV=YRAIR(ISTATE,ISTATION)+6.0+YTAIR
    ELSE
       IF(XYJAN.LE.-3.0) THEN
        TSLAV=YRAIR(ISTATE,ISTATION)+3.0+YTAIR
       ELSE
        TSLAV=YRAIR(ISTATE,ISTATION)+2.0+YTAIR
       ENDIF
    ENDIF
    
! ENDIF

!C*** Call LAKE routine to READ geometry data *** 
NUMY=1
DY0=0.0
CALL LAKE(0.0,DY0,0,12)

CALL START(ST,S,FT,ISTART,INFLOW,ITER,II,PATH,PATH_F)
! CALL START(ST,S,FT,ISTART,INFLOW,ITER,II,TSLAV,PATH,PATH_F)
!C*** OPEN FILE "TAPE7" FOR D.O. SIMULATION
!C*** File 76 save simulated D.O. year by year   	 

IF(IPRNT(4).EQ.1) THEN
    CALL MAKEFILE(PATH,T7,FFILE)
    OPEN (75,FILE=FILENAME)
ENDIF
!C20**If you have field data, then IFIELD=1 - Data is from COMMON/CHLAP
IF(IFIELD.EQ.1.OR.I_NO_FIELD_OUT_PROFILE.EQ.1) THEN
    WRITE(472,7793) I_NO_FIELD_OUT_PROFILE
    7793 FORMAT(I6)
    !FOR INTERPOLATION OF WATER TEMPERATURE AND DO 
    !FOR FIELD DATA comparison !!!
    !OPEN FILE "Mea_Tsrs.dat"
    OPEN (48,FILE="#OUTPUT\PLOTIT\Mea_Tsrs.dat")
    !File "Error.DAT" STORES STATISTCIAL RESULTS
    OPEN (98,FILE="#OUTPUT\Lake_Error.dat")
    !File for error in report format and for sensitivity analysis     
    OPEN (398,FILE="#OUTPUT\Sens_Error.dat")
    WRITE(98,*) "Error = (Simulated - Measured) and |  | means absoluate error            ",TAPE514
    WRITE(98,981)
 981     FORMAT(1X,'Month',2X,'Day',2X,'Year',2X,'Parameter',2X,'DepthEmax',3X,'Error_max',2X,'DepthEmin',3X,'Error_min',2X,'|Error|mean',1X,'No. Depth')     
    OPEN (67,FILE="#ANALYSIS\Temp_Fld_Sim-Interp.dat")
    OPEN (68,FILE="#ANALYSIS\DO-Fld_Sim-Interp.dat")
    OPEN (69,FILE="#ANALYSIS\Mx_T_DO_Fl_S-Intp.dat")
    OPEN (567,FILE="#OUTPUT\StoreFLSMTEM.dat")
    OPEN (568,FILE="#OUTPUT\storeFLSMDO.dat")
    OPEN (569,FILE="#OUTPUT\storeMIXTDO.dat")
    WRITE(67,*)'FldT SimT  DEPTH Ice? MONTH DAY YEAR Tot_Fld_data data_srl Zmix'
    WRITE(68,*)'FldDO SimDO  DEPTH Ice? MONTH DAY YEAR Tot_Fld_data data_srl Zmix'
    WRITE(69,*)' FLD_T_MX SIM_T_MX F_DO_MIX  SIM_DO_MIX  F_MIX  S_MIX YR  MNTH  DAY'
    WRITE(567,*)'FldT SimT  DEPTH Ice? MONTH DAY YEAR Tot_Fld_data data_srl Zmix'
    WRITE(568,*)'FldDO SimDO  DEPTH Ice? MONTH DAY YEAR Tot_Fld_data data_srl Zmix'
    WRITE(569,*)' FLD_T_MX SIM_T_MX F_DO_MIX  SIM_DO_MIX  F_MIX  S_MIX YR  MNTH  DAY'     
    !C20** Save simulate/measured temperature/DO profiles "TemDOProfile.dat"
    OPEN (26,FILE="#ANALYSIS\T_DO_Prfl.dat")
    WRITE(26,*)'DMAXX - Maximum temperature and DO difference within mixed layer'
    WRITE(26,*)'DMINN - Minimum temperature and DO difference within mixed layer'
    WRITE(26,*)'DIV_MAX - Maximum dividing constant (1/10th or 15th)'
    WRITE(26,*)'DIV_MIN - Minimum dividing constant (1/10th or 15th)'
    WRITE(26,*)'DMAXX,DMINN,DIVIDE_MAX,DIVIDE_MIN'
    WRITE(26,*) DMAXX,DMINN,DIVIDE_MAX,DIVIDE_MIN
    WRITE(26,*) "DFMIX - Specified limit to check mixed layer depth"
    WRITE(26,*) "DFDIFF - Diff Tem or Diff DO is (SIMulated - MEAsured) at each field depth"
    WRITE(26,*) "TDOMIX - Mixed layer temperature or DO"
ENDIF

!CFX-- The inflow routine file is set as INPUT.DAT
IF(INFLOW.GT.0) THEN
    IF(IRUN.NE.1) THEN
        READ(17,*)
        READ(17,'(A)') FLO
        CALL MAKEFILE(PATH,FILE4,FFILE)
        INQUIRE(FILE=FILENAME,EXIST=LFILE)
        IF(LFILE) GOTO 404
        PAUSE 'ERROR ON INFLOW DATA FILE NAME'
        STOP
        404 OPEN (4,FILE=FILENAME)
    ENDIF
        REWIND 4
        READ(4,*) INFLOW
ENDIF

    CALL SETUP
    WRITE(198,*) "I      DZ(I)      Z(I)"
    DO IX6=1,MBOT
        WRITE(198,134) IX6,DZ(IX6),Z(IX6)
        134     FORMAT(I2,3X,F8.4,3X,F8.4)        
    ENDDO
    WRITE(198,*)          
    WRITE(198,*) 'Maximum layer thickness DZUL= ',DZUL
    WRITE(198,*) 
    WRITE(198,*)'+    I     DZ(I)'
    I=1
        11 IF(DZ(I).GT.DZUL.AND.MBOT.LT.120) THEN
        CALL SPLIT(I,ILAY)
        GO TO 11
    ENDIF
    WRITE(198,135) MBOT,I,DZ(I)
        135    FORMAT(I2,3X,I2,3X,F7.3)        
    I=I+1
    IF(I.GT.MBOT.OR.I.GT.120) GOTO 12
        GOTO 11
    12  CALL SETZ(MBOT)
    WRITE(198,*) 'The first SPLIT results: MBOT= ',MBOT
    CALL VOLUME(MBOT)
    CALL AREA
    CALL ABOUND
    CALL TVOL(MBOT)
    IF(MODEL.GT.1) ZP=ZP*TV(MBOT)
    !C...DETERMINATION OF INITIAL MIXED LAYER DEPTH
    ILAY=1
    DO 7 I=1,MBOT-1
        IF(T2(I).GT.(T2(I+1)+CONST)) GO TO 8
        ILAY=ILAY+1
    7   CONTINUE
        8   DMIX=Z(ILAY)+DZ(ILAY)*0.5
    DO 9 I=1,10
        RMS(I)=0.0
    9   CONTINUE
    !C20** NDAYS stands for number of data with field data - SET in START SUBROUTINE - 2009
    !        NDAYS=1
    MP=0
    IPRNT(1)=1
    KDAYS=0
    !Fang July, 2009
    !     IPRNT(5)=IPRNT(5)-1
    !     CALL PTABLE(0.,0.)
    !     IPRNT(5)=IPRNT(5)+1
    !C... Start simulation for each month
    EDAY=365.
    YEAR=REAL(MYEAR)
    IF(AMOD(YEAR,4.0).EQ.0.0) EDAY=366.
    ETSUM=0.0
    HTSUM=0.0
    
    WRITE(*,994) TAPE514
    994 FORMAT(24HSimulation lake name:   ,A4)
    WRITE(*,991) TAPE8  
    991 FORMAT(17HWeather station: ,A8)          
    WRITE(*,992) FCO2 
    992 FORMAT(21HFuture simulation?   ,A1)           
    WRITE(*,993) MODELS_SCENARIO  
    993 FORMAT(18HFuture scenario?  ,I1)                

    WRITE(*,*)
    WRITE(*,*)

! July 2018 -- This part was moved from START program since SETZ and SPLIT may result in spliting water layers that causes
! a problem to setting up the initial sediment temperature profiles. 
!C* Set initial sediment temperature profile
          DZSL=ZSLT/FLOAT(IZSLT-1)
          ETAB=2.0
        DO 6 I=1, IZSLT
           ZSL(I)=FLOAT(I-1)*DZSL
    6	CONTINUE

        TSHAL=TSLAV/2.0

!RATIOX is lake geometry ratio
          IF(RATIOX.GE.4.0.AND.RATIOX.LE.5.0) THEN
           TSLXX=TSHAL-1.0+(TSHAL+3.0)*ALOG10(RATIOX)
          ENDIF

    !     IF(RATIOX.GT.5.0) TSLXX=TSLAV

          IF(RATIOX.LT.4.0) THEN
           TSLXX=TSHAL-1.0+(TSHAL+3.0)*ALOG10(RATIOX)
           DXTS=TSLAV-TSLXX

    !C	TSLAV = TS10(0), TSLXX = TS10(Hmax) 
    !C	SD - ZS is Secchi disk depth in meters 

    !C20** Find ASX=ATOP(1) surface area
    	
           CALL LAKE(ZMAX,ASX,0,1)

           AVOLM=ASX/1000000.0*ZMAX
    !     CONVERT AREA IN KM**2
        
           AFACT=SD**0.35*AVOLM**0.125
           
!           FILE27="Ts10_profile.dat"  
!           CALL MAKEFILE(PATH_F,FILE27,FFILE)
           OPEN(27,FILE="..\#COMMON\FIXED_INPUT\Ts10_profile.dat")

     
           READ(27,*)
           DO 85 I=1,38
             READ(27,856) ZTS(1,I),XTS(1,I),XTS(2,I)
     856     FORMAT(3X,F5.2,4X,F5.2,4X,F5.2)
     85    CONTINUE

          ENDIF
          
           DO 10 J=1,MBOT
 
        IF(RATIOX.GT.5.0) THEN
            If(Z(J).LE.2.0) THEN
                  TSLXX=TSLAV-1
            ELSE
                  TSLXX=TSLAV
            ENDIF
        ENDIF
               
         IF(RATIOX.LT.4.0) THEN
             ZNORM=Z(J)/AFACT
     
           IF(J.EQ.1) K=1
             
    837	   IF(K.GT.37) GOTO 838
    	   
           IF(ZNORM.GE.ZTS(1,K).AND.ZNORM.LE.ZTS(1,K+1)) THEN
              DDZ=ZTS(1,K+1)-ZTS(1,K)
              DDX=ZNORM-ZTS(1,K)
              DDA=XTS(KJ1,K+1)-XTS(KJ1,K)
              XXTS=XTS(KJ1,K)+DDA*DDX/DDZ
            GOTO 838
           ELSE
              K=K+1
              GOTO 837
           ENDIF
    	  
     838   IF(K.GE.38) XXTS=1.0
           TSLXX=TSLAV-XXTS*DXTS
    !	XXTS = [TS10(0) - TS10(Z)]/[TS10(0) - TS10(Hmax)]
    !C	TSLXX = TS10(Z), RATIO < 4.0
         ENDIF
    	  
    	 
           DO 5 I=1,IZSLT
            TSL(I,J)=(T2(J)+0.5)*EXP(-ETAB*ZSL(I))+(1.0-EXP(-ETAB*ZSL(I)))*TSLXX           
!           TSL(I,J)=T2MBOT*EXP(-ETAB*ZSL(I))+(1.0-EXP(-ETAB*ZSL(I)))*TSLXX
                     
     5     CONTINUE
     
     10    CONTINUE
  
           CLOSE (27)
        
!CFX... FOR YEAR ROUND SIMULATIONS
666 CONTINUE
!START OF YEAR ROUND SIMULATION     
!C20 Reset Secchi depth as SDY year by year!
SD=SDY(NUMY)     
!CFX Change the value of XK1 for REGIONAL lakes when EMCOE(3) = 1.84 > 0.0
!C* EMCOE(3) < 0.0 -- IT IS NONFUNCTIONAL PART !!!
!      IF(EMCOE(3).GT.0.0.AND.IFIELD.NE.1) THEN
IF(EMCOE(3).GT.0.0) THEN
        TK=EMCOE(3)/SDY(NUMY)
        XK1=TK-XK2*CHLMEAN(NUMY)
        WRITE(56,*)' MYEAR,EMCOE(3),CHLMEAN(NUMY),SDY(NUMY),TK,XK1'
        WRITE(56,1070) MYEAR,EMCOE(3),CHLMEAN(NUMY),SDY(NUMY),TK,XK1
1070    FORMAT(2X,I4,2X,F6.3,2X,F9.6,2X,F5.2,2X,F6.3,2X,F6.3)      
ENDIF
WRITE(*,*) 'Running simulation for year',MYEAR    
!Fang, 2009 - The first simulation year typically does not have input data for Secchi depth
IF(MYEAR.NE.KYEAR(NYEAR).AND.NYEAR.EQ.1) XK1=XK1_INPUT 

!Temperature components output Jalil May, 9, 2018

!OPEN(183,file="Flux.dat")
!Write(183,*) 'Year, Month, Day, HS, HA, Hbr, He, Hc, Q-in, Q-out, Water_Temp'

!write(450,422)(Z(I), I=1,MBOT)
!write(460,422)(Z(I), I=1,MBOT)
!write(470,422)(Z(I), I=1,MBOT)
!write(480,422)(Z(I), I=1,MBOT)
!422 format(120F10.3)

!START OF YEAR ROUND SIMULATION   
!WRITE(465,*) MYEAR
!WRITE(467,*) MYEAR

DO 100 JX0=1,NM

        CALL MTHDATA(MONTH,KDAYS,MYEAR,JDY,FCO2)
  
        !C... Output simulating time Month, Days in this month, Year
        IF(MONTH.EQ.MONBAK.AND.MYEAR.EQ.YEARBAK) THEN
            WRITE(76,7605)MONTH,DAYBAK,KDAYS-DAYBAK+1,MYEAR,MBOT
            WRITE(76,7600)(Z(I), I=1,MBOT)
            IF(IPRNT(4).EQ.1) THEN
		        WRITE(75,7605)MONTH,DAYBAK,KDAYS-DAYBAK+1,MYEAR,MBOT
                WRITE(75,7600)(Z(I), I=1,MBOT)
	        ENDIF
	    ELSE IF((MYEAR.GT.YEARBAK).OR.(MONTH.GT.MONBAK.AND.MYEAR.EQ.YEARBAK)) THEN
	            WRITE(76,7605)MONTH,ISTART,KDAYS,MYEAR,MBOT
                WRITE(76,7600)(Z(I), I=1,MBOT)
                IF(IPRNT(4).EQ.1) THEN
		            WRITE(75,7605)MONTH,ISTART,KDAYS,MYEAR,MBOT
		            7605    FORMAT(1X,3I4,I6,I4)
	                WRITE(75,7600)(Z(I), I=1,MBOT)
                ENDIF
        ENDIF	    
        EDAY=365.
        YEAR=REAL(MYEAR)
        IF(AMOD(YEAR,4.0).EQ.0.) EDAY=366.

!C...START SIMULATION FOR EACH DAY

        
        DO 200 MDAY=ISTART,KDAYS

                WRITE(198,*)'Month, Day ',MONTH,MDAY,MYEAR                               
                IF(MONTH.EQ.1.AND.MDAY.EQ.1) IMIX_ONCE=-10               
                IF(MONTH.EQ.1.AND.MDAY.EQ.1) THEN
                    IF(MYEAR.NE.KYEAR(NYEAR)) XK1=XK1_INPUT
                ENDIF
                !FX, 2008.  JULIANDAY: Julian day for outputt
                !FX, 2008.  EXTRADAY is for leap-year in February
                !FX, 2008.  FIRSTDAY is the first Julian day of each month, see DATA      
                EXTRADAY=0
                IF(AMOD(YEAR,4.0).EQ.0.0) EXTRADAY=1
                IF(MONTH.LT.3) THEN
                    JULIANDAY=FIRSTDAY(MONTH)+MDAY-1
                ELSE
                    JULIANDAY=FIRSTDAY(MONTH)+MDAY-1+EXTRADAY
                ENDIF                              
                !restore initial T & DO data
                IF((YEARBAK.EQ.MYEAR).AND.(MONBAK.EQ.MONTH).AND.(DAYBAK.EQ.MDAY)) THEN
                    IF(T_FLAG.EQ.0.OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0)) THEN
                        IF(T_FLAG.EQ.1.OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.1)) THEN      
		                    WRITE(99,550) MONTH,MDAY,MYEAR
                            550 FORMAT(2X,'Data Restoring for this date : ',I4,'/',I4,'/',I6)
	                    ENDIF
	                    IF(T_FLAG.EQ.1) THEN
                                WRITE(99,*)" Inputed T for this date, T = ",(TBAK(IT), IT=1,MBOT)
                                WRITE(99,*)" Before temperature is restored :"
	                            WRITE(99,*)" T2 = ",(T2(IT),IT=1,MBOT)
	                            DO IT = 1,MBOT
	                                T2(IT) = TBAK(IT)
	                            ENDDO
                                WRITE(99,*)" After temperature is restored :"
	                            WRITE(99,*)" T2 = ",(T2(IT),IT=1,MBOT)
	                    ENDIF
                        IF(iprnt(4).eq.1.and.DO_FLAG.EQ.1) THEN
                                WRITE(99,*)" Inputed DO for this date, DOBAK = ",(DOBAK(IT), IT=1,MBOT)
                                WRITE(99,*)" Before Dissolved Oxygen is restored :"
	                            WRITE(99,*)" DSO2 = ",(DSO2(IT),IT=1,MBOT)
                                DO IT= 1,MBOT
	                                DSO2(IT) = DOBAK(IT)
	                            ENDDO
                                WRITE(99,*)" After Dissolved Oxygen is restored :"
	                            WRITE(99,*)" DSO2 = ",(DSO2(IT),IT=1,MBOT)
                
	                    ENDIF
	                ENDIF
	            ENDIF
                NFLOW=INFLOW
                CALL LAKE(0.0,DY0,0,5)
                !FOR ICE-SNOW MODEL - COMPUTE ICE OR SNOW THICKNESS, HEAT
                !FLUX FROM SEDIMENT - VERY IMPORTANT!!!
                DY0=0.0
                CALL LAKE(0.,DY0,0,14)
                PICE=THICE
                IREPEAT=-100

                !CFX***Specify total chlorophyll-a concentration in mg/L
                CALL PTOTALS(MAXMTH,MXDAY,MXYEAR,CHLMAX,CHLMEAN(NUMY),SDY(NUMY),NUMY,JDYCHL)
                IF(MDAY.EQ.KDAYS.OR.(MP/NPRINT)*NPRINT.EQ.MP) IPRNT(1)=1
                IF((MONTH*100+MDAY).EQ.NPRNT(NDAYS)) IPRNT(1)=1
                !CFX - Inflow routine!
                IF(NFLOW.GT.0) THEN
                    DO 203 L=1,NFLOW
                        205 READ(4,*) MTH,MD,QIN(L),TIN(L),PAIN(L),BODIN(L),DOIN(L),CIN(L),CDIN(L),XNOIN(L),XNHIN(L),CHLAIN(1,L)
                        !C... Search inflow file for first day of simulation
                        IF((MTH*100+MD).NE.(MONTH*100+MDAY)) GOTO 205
                        !C... Convert inflow from cfs to m**3 per day
                        QIN(L)=QIN(L)*2446.6
                    203     CONTINUE
                ENDIF
                P=PR(MDAY)*0.0254
                MP=MP+1
                TMIX=T2(1)
                !C...CALCULATION OF KINETIC ENERGY FROM WIND STRESS
                CALL WINEN(TAU,VC,WIND(MDAY))
                RKE=TAU*VC*ATOP(1)*WSTR*86400.0
                !C--- HEAT IS ABSORBED FIRST, THEN WATER COLUMN IS MIXED BY THE WIND
                !CFX  ITERF FOR HEATING AND WIND-MIXING PROCESS, ITERF<10.
                DO 864 IK=1,MBOT
                    T2K(IK)=T2(IK)
864             CONTINUE
                            
                ITERF=-100
865             CALL HEBUG(ILAY,TMIX,QNET,HS,HA,HBR,HE,HC,TAIR(MDAY),TDEW(MDAY),CR(MDAY),RAD(MDAY),WIND(MDAY),VC)

                IF(THICE.LE.0.0) THEN

                    DO 910 I=1,MBOT
                        TEDIF(I)=T2(I)
                    910     CONTINUE             

                    CALL PREICE(QNEG,TBOUN,NP)
                    
                    CALL CONMIX(ILAY,TMIX,MBOT)
                    DO 920 I=1,MBOT
                        TECON(I)=T2(I)
                    920     CONTINUE
                ENDIF
                !...CALCULATION OF EVAPORATION IN TERMS OF VOLUME
                !...CALCULATES LATENT HEAT OF VAPORIZATION ALV
                HED=HE/((597.31-0.5631*T2(1))*RHO(T2(1),C2(1),CD2(1)))
                HEV=HED*ATOP(1)
                !...  CALL ADVECT(P,HED,NFLOW,S,FT,WCHANL,ST,MYEAR)
 
                IF(THICE.LE.0.0) THEN
                    CALL WINMIX(RKE,TMIX,TSAVE,ILAY,MBOT,VC)
                    DMIX=Z(ILAY)+0.5*DZ(ILAY)
                     
                    DO 477 II=1,MBOT
                        TEMIX(II)=T2(II)
                    477 CONTINUE
                    CALL POSTMIX(TBOUN,THICE,QNEG,TECON,TSAVE,RKE,NP)
                ENDIF   
                 
                IF((THICE.GT.0.0).AND.(PICE.LE.0.0)) THEN
                    IF(IREPEAT.EQ.100) GOTO 946
                    IF(MONTH.LT.6) GOTO 946
                    DO 945 I=1,MBOT
                        T2(I)=T2K(I)
                    945 CONTINUE
                    IREPEAT=100
                    IF(IREPEAT.EQ.100) GOTO 865
                    946 CONTINUE
                ENDIF                                       
 
   
                !Assume to have complete mixing once in every spring - THIS may NOT be correct!!!
               ! Record ice thickness and surface water temperature for two days
               
               !May 7, 2013, no automatically forced mixing in spring for deep lakes
               !IF(Z(MBOT).GT.20.0) I_FULL_MIXING=0
               
               IF(I_FULL_MIXING.EQ.1)THEN
                    IF(MONTH.GE.2.AND.MONTH.LE.6) THEN
                        THICE2=THICE
                        TEMP2=T2(1)       
                        IF(IMIX_ONCE.LT.0) THEN
                            IF(THICE2.LE.0.0) THEN                
                                IF(TEMP1.LT.4.0.AND.TEMP2.GT.4.0) THEN
                                    !  WRITE(299,*) MYEAR,MONTH,MDAY,TEMP1,TEMP2
                                    !  WRITE(*,*) MYEAR,JX0,MDAY,TEMP1,TEMP2
                                    WRITE(2100,2101)MYEAR,MONTH,MDAY,TEMP1,TEMP2
                                    2101    FORMAT(I4,2X,I2,2X,I2,F6.2,1X,F6.2)    
                                    IMIX_ONCE=10
                                    !Fang, March 2009 - Testing Sparkling Lake
                                    !Assume to have complete mixing once in every spring - THIS may NOT be correct!!!
                                    TOTEMP=0.0
                                    VTOTEMP=0.0              
                                    DO 745 ITHM=1,MBOT
                                        TOTEMP=TOTEMP+T2(ITHM)*V(ITHM)
                                        VTOTEMP=VTOTEMP+V(ITHM)
                                    745     CONTINUE       
          
                                    TOMIX=TOTEMP/VTOTEMP
                                    DO 746 ITHM=1,MBOT
                                        T2(ITHM)=TOMIX
                                    746     CONTINUE            
                                    ILAY=MBOT
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF  
                ENDIF            
                IF(JDY.LE.EDAY) THEN
                    DY2=JDY
                    TDAY=EDAY
                ENDIF 
                IF((JDY.GT.TDAY).AND.(JDY.LE.(TDAY+EDAY))) DY2=JDY-TDAY
                IF(JDY.EQ.(TDAY+EDAY)) TDAY=TDAY+EDAY
                IDY=DY2 
                IF(JULIANDAY.GE.245.OR.JULIANDAY.LE.122) THEN
                    WRITE(18,262) MYEAR,MONTH,MDAY,JDY,THICE,THSNOW,SNOWFL(MDAY),ILAY,MBOT
                    262 FORMAT(I4,2X,I2,2X,I2,2X,I5,3X,E10.3,3X,E10.3,3X,E10.3,3X,I3,3X,I3)
                ELSEIF(THICE.GT.0.0) THEN
                    WRITE(18,262) MYEAR,MONTH,MDAY,JDY,THICE,THSNOW,SNOWFL(MDAY),ILAY,MBOT
                ENDIF
                IF(ITERF.GT.0) GOTO 865
                !Fang 3/31/2009
                !Move this part code from HEBUG, because we could have negative water temperature near the surface
                !before ice formation or just after ice melting when we solve heat budget equation.  We should NOT
                !have negative water temperature after the program goes through wind mixing and convective mixing. 
                !Checking negative temperature - should never happen
                DO IX=1,MBOT
                    IF(T2(I).LT.0.0) THEN
                        WRITE(*,*)
                        WRITE(*,*) "MONTH, MDAY",MONTH,MDAY
                        WRITE(*,*) "Negative simulated water temperature!!!"
                        WRITE(*,*) "Check lake volume data or weather data"
                        WRITE(198,*)
                        WRITE(198,*) "Negative simulated water temperature!!!"
                        WRITE(198,*) "MONTH, MDAY",MONTH,MDAY
                        !NOT stop the program, but write the information on screen or to the file "warninfo.dat"          
                    ENDIF
                END DO      
                
                !For temperature simulation only, skip dissolved and suspended substances routines
                IF(IPRNT(4).LT.1) GOTO 35
                !C************************************************************
                !
                !  THE SIMPLEST MODEL FOR DISSOLVED OXYGEN (X. FANG)
                !
                !C*************************************************************
                IF(THICE.GT.0.0)ILAY=1          ! ADDED ON FEB 2010, Fang
                IF(MODEL.EQ.4) THEN
                !Groundwater flows can be added (none presently)
                    DY0=0.0 
                    CALL LAKE(0.0,DY0,0,11)
                    ICV=6
                    CALL SETAMK(WIND(MDAY),VC,ILAY,MBOT,ICV)
                    DO 887 I=1,MBOT
                      T20(I)=T2(I)-20.0
                    887    CONTINUE       
                    TD=12.16+2.36*COS(0.0172*(172.0-DY2))
                    CALL COEF(MODEL,MBOT,NCLASS)
                    CALL DISOLID(DSO2,WIND(MDAY),RAD(MDAY),TD,ST)
                ENDIF
                DY0=0.0
                35    CALL LAKE(0.,DY0,0,13)
                !FOR INTERPOLATION OF WATER TEMPERATURE AND D.O.
                !FOR SIMUATIONS
                IF(IFIELD.EQ.1.OR.I_NO_FIELD_OUT_PROFILE.EQ.1) THEN 
                    DO 326 IM=1,NDEPTH
                        ZDIN=FDTH(IM) 
                        TETH(IM)=-580.0
                        IF(IPRNT(4).EQ.1) DOTH(IM)=-580.0
                        DO 328 JJ=1,MBOT-1
                            IF(ZDIN.LE.Z(JJ+1).AND.ZDIN.GE.Z(JJ)) THEN
                                !-WATER TEMPERATURE
                                FAD=T2(JJ+1)-T2(JJ)
                                FDZ=Z(JJ+1)-Z(JJ)
                                TETH(IM)=T2(JJ)+(ZDIN-Z(JJ))*FAD/FDZ
                                IF(IPRNT(4).EQ.1) THEN
                                    !-DISSOVED OXYGEN
                                    DAD=DSO2(JJ+1)-DSO2(JJ)
                                    DOTH(IM)=DSO2(JJ)+(ZDIN-Z(JJ))*DAD/FDZ
                                ENDIF
                            ENDIF
                        328 CONTINUE
                    326   CONTINUE
                    IF(IPRNT(4).EQ.1) THEN
                        !C*****Compute the saturated oxygen from Thomann    ************
                        !C*****Correct saturation concentration with elevation  ********
                        !C*****ST is the stage of the lake in meters above sea level ***
                        T2X=TETH(1)+273.15
                        DOXS=-139.3441+1.575701E5/T2X-6.642308E7/T2X**2.0+1.2438E10/T2X**3.0-8.621949E11/T2X**4.0
                        DOXS=EXP(DOXS)*(1-0.000035*ST*3.2808)
                    ENDIF
                    IF(IPRNT(4).EQ.1) THEN
                        IF(THICE.LE.0.0) THEN
                            WRITE(13,332) JULIANDAY,JDY,MYEAR,(TETH(K),K=1,NDEPTH),DMIX/ZMAX,DOXS
                            WRITE(14,332) JULIANDAY,JDY,MYEAR,(DOTH(K),K=1,NDEPTH),DMIX/ZMAX,DOXS
                        ELSE
                            WRITE(13,332) JULIANDAY,JDY,MYEAR,(TETH(K),K=1,NDEPTH),-1.0,DOXS
                            WRITE(14,332) JULIANDAY,JDY,MYEAR,(DOTH(K),K=1,NDEPTH),-1.0,DOXS
                        ENDIF
                            332 FORMAT(1X,I3,2X,I5,2X,I4,7(2X,F6.2))
                    ELSE
                        !TEMPERATURE SIMULATION ONLY !
                        IF(THICE.LE.0.0) THEN
                            WRITE(13,332)  JULIANDAY,JDY,MYEAR,(TETH(K),K=1,NDEPTH),DMIX/ZMAX
                        ELSE
                            WRITE(13,332)  JULIANDAY,JDY,MYEAR,(TETH(K),K=1,NDEPTH),1.0
                        ENDIF
                    ENDIF
                    !CSTORE DATA FOR ERROR ANALYSIS ON THE DAY WHICH WE HAVE FIELD DATA
                    !___________________________FILED DATA_____________________________
                    IF(MYEAR.EQ.KYEAR(NYEAR)) THEN 
                        IF((MDAY+MONTH*100).EQ.NPRNT(NDAYS)) THEN
                            CALL ERROR(1,1,TETH,DOTH)
                        ENDIF
                    ENDIF 
                    !Output to plot file (tape8.PLT)
                    IF(IPRNT(5).GT.0) CALL FPLOT(MYEAR,DMIX,SD,CHLMEAN(NUMY))
                    
                    IF((IFIELD.EQ.1.OR.I_NO_FIELD_OUT_PROFILE.EQ.1).AND.MYEAR.EQ.KYEAR(NYEAR)) THEN 
                        IF(EMCOE(3).GT.0.0.AND.ISD.EQ.2) THEN
                            IF((MDAY+MONTH*100).EQ.NSDAY(NDAYS)) THEN
                                !CFX  NEW METHOD FOR CHANGING THE XK1, ON JUNE 30, 1992
                                !C* EMCOE(3) < 0.0 -- IT IS NONFUNCTIONAL PART !!!
                                TK=EMCOE(3)/SDFIELD(NDAYS)
                                XK1=TK-XK2*CHLATOT(1)
                                !WRITE(800,*)JULIANDAY,XK1,SDFIELD(NDAYS),CHLATOT(1)
                            ENDIF
                        ENDIF
                        IF((MDAY+MONTH*100).EQ.NPRNT(NDAYS)) THEN
                            !Access and output field data
                            IF(I_NO_FIELD_OUT_PROFILE.EQ.1.AND.IFIELD.EQ.0) NDAYS=NDAYS+1
                            IF(IFIELD.EQ.1) THEN
                                INDAYO_COUNTER=0
                                CALL FDATA(NF,NPRFLE,NSKIP)
                            ENDIF
                            !CFX. FOR INTERPOLATION OF WATER TEMPERATURE AND DO 
                            !C... FOR FIELD DATA comparison !!!
                            IF(I_NO_FIELD_OUT_PROFILE.EQ.1) GO TO 7769
                            DO 426 IM=1,NDEPTH
                                ZDIN=FDTH(IM) 
                                TETH(IM)=-580.0
                                IF(IPRNT(4).EQ.1) DOTH(IM)=-580.0
                                DO 428 JJ=1,NF-1
                                    IF(ZDIN.LE.DEPTH(JJ+1).AND.ZDIN.GE.DEPTH(JJ)) THEN
                                        !WATER TEMPERATURE
                                        FAD=FLDATA(1,JJ+1)-FLDATA(1,JJ)
                                        FDZ=DEPTH(JJ+1)-DEPTH(JJ)
                                        TETH(IM)=FLDATA(1,JJ)+(ZDIN-DEPTH(JJ))*FAD/FDZ
                                        IF(IPRNT(4).EQ.1) THEN
                                            !DISSOVED OXYGEN
                                            DAD=FLDATA(6,JJ+1)-FLDATA(6,JJ)
                                            DOTH(IM)=FLDATA(6,JJ)+(ZDIN-DEPTH(JJ))*DAD/FDZ
                                        ENDIF
                                    ENDIF
                                428 CONTINUE
                            426 CONTINUE
                            IF(IPRNT(4).EQ.1) THEN
                                WRITE(48,436) JDY,MYEAR,(TETH(K),K=1,NDEPTH),(DOTH(K),K=1,NDEPTH) 
                                436  FORMAT(I5,2X,I4,2X,10(F7.2,2X))
                            ELSE
                                WRITE(48,436) JDY,MYEAR,(TETH(K),K=1,NDEPTH) 
                            ENDIF
                            !STORGE DATA FOR ERROR ANALYSIS
                            CALL ERROR(1,2,TETH,DOTH)
                            !Output simulation results and field data for Plot-IT
                            !Name of the out put file is "TemDOProfile.dat"
                            7769   WRITE(26,7777) MONTH,MDAY,MYEAR
                            !CFX SHOEB ALAM MAY4/2009
                            !THESE LINES ARE FOR WRITING THE OUTPUT FILE FOR DRAWING PROFILE WITH PLOTIT
                            !HERE THERE WILL BE ONE OUTPUT FILE FOR EACH FIELD DATA DAY
                            !NAME OF THE FILE WILL BE LIKE --- CARLOS_2008_07_28.DAT   
                            !-WHICH MEANS - CARLOS LAKE DATA OF JULY 28, 2008
                            NA1=INT((MYEAR-100*INT(MYEAR/100.0))/10.0)  ! NA1, NB1 FOR LAST TWO DIGIT OF YEAR
                            NB1=MYEAR-10*INT(MYEAR/10.0)
                            NC1=MYEAR-NA1*10-NB1
                            NF1=INT(NC1/1000)                             ! NF1 , NG1 FOR FIRST TWO DIGIT OF YEAR
                            NG1=iNT((NC1-NF1*1000)/100)
                            NC2=INT(MONTH-10*INT(MONTH/10.0))             !NC2 , ND2 FOR MONTH DIGIT
                            ND2=INT(MONTH/10.0)
                            NE2=INT(MDAY-10*INT(MDAY/10.0))                !NE2 , NG2 FOR DAY DIGIT
                            NG2=INT(MDAY/10.0)
                            T14(5)='_'    
                            T14(6)=NUM(NF1+1)
                            T14(7)=NUM(NG1+1)
                            T14(8)=NUM(NA1+1)
                            T14(9)=NUM(NB1+1) 
                            T14(10)='_'
                            T14(12)=NUM(NC2+1)
                            T14(11)=NUM(ND2+1)
                            T14(13)='_'
                            T14(15)=NUM(NE2+1)
                            T14(14)=NUM(NG2+1)
                            T14(16)='.'
                            T14(17)='D'
                            T14(18)='A'
                            T14(19)='T'
                            !FILE 70 IS TO OUTPUT EACH DAY DATA (SIMULATED AND FIELD) ON A SINGLE FILE ON AVAILABLE FIELD DATA DATE TO DRAW PROFILE WITH PLOTIT
                            CALL MAKEFILE(PATHP,TAPE14,FFILE)
                            OPEN (70,FILE=FILENAME)
                            WRITE(471,7796)T14(1),T14(2),T14(3),T14(4),T14(5),T14(6),T14(7),T14(8),T14(9),T14(10),T14(11),T14(12),T14(13),T14(14),T14(15),T14(16),T14(17),T14(18),T14(19)
                                7796    FORMAT('PlotITW|',19(A1),'!B7..B100!A7..A100')
                            WRITE(471,7797)T14(1),T14(2),T14(3),T14(4),T14(5),T14(6),T14(7),T14(8),T14(9),T14(10),T14(11),T14(12),T14(13),T14(14),T14(15),T14(16),T14(17),T14(18),T14(19)
                                7797    FORMAT('  PlotITW|',19(A1),'!E7..E100!D7..D100')
                            WRITE(473,7798)T14(1),T14(2),T14(3),T14(4),T14(5),T14(6),T14(7),T14(8),T14(9),T14(10),T14(11),T14(12),T14(13),T14(14),T14(15),T14(16),T14(17),T14(18),T14(19)
                                7798    FORMAT('PlotITW|',19(A1),'!C7..C100!A7..A100')
                            WRITE(473,7799)T14(1),T14(2),T14(3),T14(4),T14(5),T14(6),T14(7),T14(8),T14(9),T14(10),T14(11),T14(12),T14(13),T14(14),T14(15),T14(16),T14(17),T14(18),T14(19)
                                7799    FORMAT('  PlotITW|',19(A1),'!F7..F100!D7..D100')
                            WRITE(472,7795) T14(1),T14(2),T14(3),T14(4),T14(5),T14(6),T14(7),T14(8),T14(9),T14(10),T14(11),T14(12),T14(13),T14(14),T14(15)
                                7795    FORMAT(19(A1))
                            WRITE(472,7794) MBOT,NF,NPRFLE,NFLD(1)
                                7794    FORMAT(4I4)
                            WRITE(70,7777) MONTH,MDAY,MYEAR
7777                        FORMAT(5X/5X/I3,3X,I4,3X,I6/5X/)
                            
                            ICOUNTER_PROFILEFILE=1
                            IF(ICOUNTER_PROFILEFILE.EQ.1) then
                            WRITE(26,7770)
                            WRITE(70,7770)
                                7770    FORMAT(7X,'SIM Depth', 2X,'SIM T', 2X,'SIM DO',2X,'Field Depth',2X 'Field T',2X,'Field DO',2X,'Diff Tem',3X,'Diff DO')
                            ENDIF
                            ICOUNTER_PROFILEFILE=2 
                                
                            DO 266 I=1,MBOT
                                IF(I_NO_FIELD_OUT_PROFILE.EQ.1) THEN                                
                                    WRITE(26,3099)-Z(I),T2(I),DSO2(I)          
                                    WRITE(70,3099)-Z(I),T2(I),DSO2(I)  
                                    3099       FORMAT(5X,3(F7.2,3X))
                                    GOTO 266
                                ENDIF
                                
                                ! Xing Fang - fix errors with more than one profiles in the same date or only one profile available
                                IF(I.GT.1.AND.DEPTH(I).LT.0.0) THEN 
                                    WRITE(26,3099) -Z(I),T2(I),DSO2(I)
                                    WRITE(70,3099) -Z(I),T2(I),DSO2(I) 
                                ELSE
                                    IF(NPRFLE.EQ.2) THEN 
                                        WRITE(26,3096) -Z(I),T2(I),DSO2(I),-DEPTH(I),FLDATA(1,I),FLDATA(6,I),SMDATA(1,I),SMDATA(2,I)
                                        WRITE(70,3096) -Z(I),T2(I),DSO2(I),-DEPTH(I),FLDATA(1,I),FLDATA(6,I),SMDATA(1,I),SMDATA(2,I)
                                            3096    FORMAT(5X,8(F7.2,3X))
                                        !IF(FLDATA(6,I).EQ.3.0) THEN
                                        !TDO3DEPTH=DEPTH(I) 
                                        !TDO3=FLDATA(1,I)
                                        !ENDIF                                           
                                        !IF(Z(I).EQ.TDO3DEPTH)  WRITE(930,*) MONTH,MDAY,MYEAR,Z(I),DSO2(I),TDO3,T2(I)  
                                    ENDIF
                                    
                                    IF(NPRFLE.EQ.1.AND.NFLD(1).EQ.1) THEN
                                        WRITE(26,3097) -Z(I),T2(I),DSO2(I),-DEPTH(I),FLDATA(1,I),SMDATA(1,I),SMDATA(2,I)
                                        WRITE(70,3097) -Z(I),T2(I),DSO2(I),-DEPTH(I),FLDATA(1,I),SMDATA(1,I),SMDATA(2,I)
                                            3097    FORMAT(5X,5(F7.2,3X),'No Data',2(3X,F7.2)) 
                                    ENDIF
                                    
                                    IF(NPRFLE.EQ.1.AND.NFLD(1).EQ.6) THEN
                                        WRITE(26,3098) -Z(I),T2(I),DSO2(I),-DEPTH(I),FLDATA(6,I),SMDATA(1,I),SMDATA(2,I)
                                        WRITE(70,3098) -Z(I),T2(I),DSO2(I),-DEPTH(I),FLDATA(6,I),SMDATA(1,I),SMDATA(2,I)
                                            3098    FORMAT(5X,4(F7.2,3X),'No Data',3(3X,F7.2))
                                    ENDIF                                                      
                                ENDIF
                            266 CONTINUE
                            !Tracking number of field data used = NUMFIELD         
                            NUMFIELD=NUMFIELD+1
                            IF(NUMFIELD.EQ.NTDY(NYEAR)) THEN
                                  WRITE(198,*)
                                  WRITE(198,*) "MYEAR,MDAY,MONTH,NUMFIELD,NTDY(NYEAR),NYEAR"
                                  WRITE(198,*) MYEAR,MDAY,MONTH,NUMFIELD,NTDY(NYEAR),NYEAR
                                  WRITE(198,*)
                                  NYEAR=NYEAR+1
                                  NUMFIELD=0
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
                !C* ABOVE PART IS FOR SIMULATION WITH FIELD DATA
                JDY=JDY+1
                IPRNT(1)=0
                !COutput simulated Temperature & Dissolved Oxygen into files.
                IF(MONTH.EQ.MONBAK.AND.MYEAR.EQ.YEARBAK) THEN
                    IF(MDAY.GE.DAYBAK) THEN
                        WRITE(76,7600)(T2(I),I=1,MBOT)
                        IF(IPRNT(4).EQ.1) THEN
                            WRITE(75,7600)(DSO2(I),I=1,MBOT)
	                    ENDIF
	                ENDIF
	            ELSEIF((MYEAR.GT.YEARBAK).OR.(MONTH.GT.MONBAK.AND.MYEAR.EQ.YEARBAK)) THEN
                        WRITE(76,7600)(T2(I),I=1,MBOT)
                        IF(IPRNT(4).EQ.1) THEN
                            WRITE(75,7600)(DSO2(I),I=1,MBOT)
	                        CLOSE(70)
	                        7600      FORMAT(1X, 30F8.3) 
                        ENDIF
                ENDIF	    
                !Shoeb Alam sep 2009 for correting the spring overturn  Record ice thickness for two days
                IF(MONTH.GE.3.AND.MONTH.LE.6) THEN
                    THICE1=THICE2
                    TEMP1=T2(1)
                ENDIF  
200  CONTINUE
    ISTART=1                
    !END OF YEAR ROUND SIMULATION              
100 CONTINUE
    !FOR YEAR ROUND SIMULATION, NYTOT = TOTAL YEARS
NUMY=NUMY+1 
IF(NUMY.LE.NYTOT) THEN
        CLOSE (9)
        IF(IPRNT(4).EQ.1) CLOSE (75)
            CLOSE (76)
            MYEAR=MYEAR+1
            NA=INT((MYEAR-100*INT(MYEAR/100.0))/10.0)
            NB=MYEAR-10*INT(MYEAR/10.0)
            T8(INUM)=NUM(NA+1)
            T8(INUM+1)=NUM(NB+1)
            !OPEN MTHDATA OF THE NEXT YEAR      
            CALL MAKEFILE(PATHM,T8,FFILE)
            OPEN (9,FILE=FILENAME,STATUS='OLD')
            T7(INUM+1)=NUM(NA+1)
            T7(INUM+2)=NUM(NB+1)
            T9(INUM+1)=NUM(NA+1)
            T9(INUM+2)=NUM(NB+1)
            !File 75 save simulated DO year by year   	 
            IF(IPRNT(4).EQ.1) THEN
                CALL MAKEFILE(PATH,T7,FFILE)
                OPEN (75,FILE=FILENAME)
            ENDIF
            !File 76 save simulated water temperature year by year   	 
            CALL MAKEFILE(PATH,T9,FFILE)
            OPEN (76,FILE=FILENAME)
            !Fang April 22, 2009 -- Change NYEAR has been moved to after the program reads/processes field data        
            IF(MYEAR.EQ.KYEAR(NYEAR+1)) NYEAR=NYEAR+1
            NM=12
            IF(NUMY.EQ.NYTOT) NM=NMFIN
GOTO 666  
ENDIF

!Compute and list statistics:
!1) Absolute maximum deviations between model and field data and day of occurrence
!2) slope of regression of field data on simulation results
!3) regression coefficient  R**2
!4) standard error of the regression
IF(IFIELD.EQ.1.AND.MYEAR.GE.KYEAR(1)) THEN
    CALL ERROR(2,2,TETH,DOTH)
    WRITE(98,3030) MAXMTH,MXDAY,MXYEAR,CHLMAX
    3030    FORMAT(1X/1X,'MAXIMUM CHLOROPHYLL-A  :',10X,I2,'-',I2,'-',I4,10X,F8.5,'  (mg/l)')
ENDIF

WRITE(198,*)
WRITE(198,*)' End of MNLAKE simulation :  Normal exit'
WRITE(99,*)' End of MNLAKE simulation :  Normal exit'             
WRITE(*,*)
WRITE(*,*)' End of MINLAKE2012 simulation :  Normal exit'            
!WRITE(*,*)' Weather station and year ? ' // TAPE8            
!WRITE(*,*)' Future ? '// FCO2            
!WRITE(*,*)' Futute model ?', MODELS_SCENARIO                  
!WRITE(*,*)' Simulation done for:  ' // TAPE514
END

!C********************************************************
!     THE PROGRAM IS FOR RYAN LAKE IN WINTER STUDY 
!     THE ICE-SNOW MODEL IS INCOPORATED WITH MINLAKE      
!     (THERE IS HEAT INPUT FROM THE GROUNDWATER)
!     REVERSED TEMP GRADIENT (INSTABILITY) TREATMENT
!     THE PROGRAM WAS BASED ON GU'S STUDY WITH IMPROVEMENT
!     AND MODIFICATIONS (SEE FANG, 1994)
!C*********************************************************


!**********************************************************
!*                                                        *
!*                                                        *
!**********************************************************
SUBROUTINE LAKE(ZD,DUM,NFLOW,ID)
!Lake specific subroutine to configure the model to a specific lake
REAL*8 A,V,TV,ATOP,DUM
INTEGER ICESTAR
INTEGER FMON,FDAY,FYEAR,NK
COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),PR(31),DRCT(31)
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/SOURCE/RM(3,120),PROD(120),XMR(3,120),PRODSUM(120)
COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
COMMON/PHYTO0/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),GROMAX(3),TMAX(3)
COMMON/PHYTO1/TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN(90),CHLMAX,SDY(90)
COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),REPRO,XI,XKMZ,GRAZE(3,120)
COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/SUB/SDZ(180),SZ(180),LAY(120),AVGI(24,180),SVOL(180)
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
COMMON/STEPS2/MBOT,ILAY
COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
COMMON/INFLOWX/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
COMMON/FIELD/IFLAG(10),FLDATA(10,120),DEPTH(120),NFLD(10),SD,NSKIPDAY,NSDAY(640),SDFIELD(640),TAPE64
COMMON/FILEX/DIN,MET,FLO,TAPE8,TAPE1,IREC
COMMON/TITL/TITLE1,TITLE2,TITLE3
COMMON IPARAM,IGRAF
COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
COMMON/CONR/FAKW,SNCOE,COEWIN
COMMON/GEOMETRY/NK
COMMON/NEW1/ICESTAR
CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
DIMENSION AZ(80),AA(80),AV(80),VOLA(80)
             
!    WE WILL NOT USE THE AREA -- DEPTH AND VOLUME -- DEPTH
!    RELATIONSHIP, INSTEAD OF USING INTERPOLATION SCHEME FROM
!    THE ORIGINAL DATA (ZA -- DEPTH,  AA -- AREA, AV -- VOLUME)

!    IN THE MAIN PROGRAM WE SHOULD CALL LAKE ID=12 FIRST !!!
    
GOTO (100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400) ID

!****** AREA COMPUTATION SECTION ***********
100     CONTINUE
        DUM=FXIN(ZD,NK,AZ,AA)
        RETURN
!****** FETCH COMPUTATION SECTION ******
200     ZD=SQRT(4.0/3.14159*AA(NK))
        RETURN
!****** VOLUME COMPUTATION SECTION *****
300     CONTINUE
        DUM=FXIN(ZD,NK,AZ,AV)
        RETURN 
!****** COMPUTE DEPTH FROM VOLUME *****
400     CONTINUE 
        DO 150 I=2,NK
            IF(DUM.EQ.AV(I)) THEN
                ZD=AZ(I)
                RETURN
            ENDIF
            IF(DUM.LT.AV(I)) THEN
                RAT=(AZ(I)-AZ(I-1))/(AV(I)-AV(I-1))
                ZD=AZ(I-1)+(DUM-AV(I-1))*RAT
                RETURN
            ENDIF
        150 CONTINUE
        IF(DUM.GT.AV(NK).AND.I.GE.NK) THEN
            RAT=(AV(NK)-AV(NK-1))/(DUM-AV(NK-1))
            ZD=AZ(NK-1)+(AZ(NK)-AZ(NK-1))/RAT
            AV(NK)=DUM
            AZ(NK)=ZD
        ENDIF 
        AA(NK)=FXIN(AZ(NK),NK,AZ,AA)
        RETURN
!****** TREATMENT SECTION *****
500       CONTINUE
          IF(THICE.LE.0.0) THEN 
              WRITE(99,550) MONTH,MDAY,MYEAR
              550   FORMAT(2X,'Month',I4,3X,'Day',I4,3X,I6)
          ELSE
              WRITE(99,555) MONTH,MDAY,MYEAR,THICE,THSNOW
              555   FORMAT(2X,'Month',I4,3X,'Day',I4,3X,I6,3X,E9.2,3X,F5.2)        
          ENDIF
          RETURN
!****** PHOSPHORUS SOURCES/SINKS *****
 600    RETURN
!****** NO2-NO3 SOURCES SINKS *****
 700    RETURN
!****** NH4 SOURCES/SINKS *****
 800    RETURN
!****** O2 SOURCES/SINKS *****
 900    RETURN
!****** OUTFLOW COMPUTATION *****
1000    CONTINUE
        NFLOW=0
        RETURN
!****** GROUNDWATER FLOW COMPUTATION *****
1100    CONTINUE
        RETURN
        
!****** INPUT REQUEST OR MODIFICATION ****
!****** 1200 SUBROUTINE IS CALLED JUST ONCE,IN THE BEGINNING.
!****** IS CONVENIENT TO WRITE HERE ALL CONSTANT VALUES 
1200    CONTINUE
!****** READ THE ORIGINAL DATA FOR DEPTH, AREA, VOLUME
        READ(38,*)
        READ(38,*) NK
        READ(38,*)
        DO 66 I=1,NK
            READ(38,*) AZ(I),AA(I),AV(I)
        66  CONTINUE 
 !Fang 3/31/2009
 !Check lake volume computation
        VOLA(1)=0.0
        DO IY=2,NK
            VOLA(IY)=VOLA(IY-1)+(AA(IY)+AA(IY-1))/2.0*(AZ(IY)-AZ(IY-1))
            DIFF=ABS(AV(IY)-VOLA(IY))/VOLA(IY)
            IF(DIFF.GT.0.01) THEN
                WRITE(*,*)
                WRITE(*,*) "Correct volume, Input AV(I):",VOLA(IY),AV(IY)
                WRITE(*,*) "There are an error in volume computation!!!"
                IF(IY.EQ.NK) STOP
            ENDIF 
        END DO
        !OUTPUT SOME PARAMETERS FOR WINTER STUDY !     
        !WRITE(18,905)
        !WRITE(18,1613)
        !WRITE(18,1615) CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY
        !WRITE(18,1616) HKMXSM,WCFSM,WSSM,HKMXIS,WCFIS,WSIS
        !WRITE(18,1614) BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW
        !WRITE(18,1618)
        !WRITE(18,250) 
        !WRITE(18,255) 
        !WRITE(14,1621) ZSLT,IZSLT,AHTBTM,TSLMEAN,ETAB
        !WRITE(14,1629)
        !WRITE(14,1631) 
        !WRITE(14,1619) (ZSL(I),I=1,IZSLT)
        !WRITE(14,1620) JDY,(TSL(I),I=1,IZSLT)
        !250  FORMAT('JDAY',3X,'TAIR',4X,'QAIR',4X,'QWAT',3X,'HSED',3X,'RAD',5X,'HSA',3X,'HCNDISO')
        !255  FORMAT(2X,'JDAY',5X,'THICE',5X,'THSNOW',5X,'SNOWFL')
        !905  FORMAT('INPUT DATA:') 
        !1613 FORMAT(2X,'CFSNOW CDIS0 CNDSNW0 CNDWI DEPTHC ICEMON ICEDAY',1X,'MELMON MELDAY')
        !1614 FORMAT(2X/2X,'B, A, G(ICE,SNOW)= ',6F7.2//) 
        !1615 FORMAT(2X,F5.2,1X,F5.2,2X,F5.2,3X,F5.2,1X,F4.2,1X,I6,1X,I6,1X,I7,1X,I6/)
        !1616 FORMAT(2X,'HKMAX, WCOEF, WSTR = ',2X,3F9.2)
        !1618 FORMAT(2X,'RESULTS OF SIMULATION:')
        !1619 FORMAT(/'Z(I)',11F6.2) 
        !1620 FORMAT(I4,11F6.2) 
        !1629 FORMAT(2X,'TEMPERATURES IN THE SEDIMENT BELOW LAKE BOTTOM:'/)
        !1621 FORMAT(2X,'ZSLT   IZSLT   AHTBTM   TSLMEAN   ETAB '/2X,F4.1,2X,I4,3X,F6.2,5X,F6.2,2X,F6.2/)
        !1631 FORMAT(2X,'DEPTH BELOW BOTTOM')
        RETURN
        
!*******  POST DAILY SIMULATION TREATMENT AND COMPUTATIONS  *******
1300    CONTINUE
        RETURN
!******* ICE-SNOW MODEL BY GU WITH MODIFICATION FOR WINTER SIMULATIONS
1400    CONTINUE
        !SET WIND SHELTERING AND MAXIMUM DIFFUSION COEFFICIENTS ICE COVER - WINTER PERIOD
        IF(THICE.GT.0.0) THEN
            HKMAX=HKMXIS
            WCOEF=WCFIS
            WSTR=WSIS
        ENDIF
        !OPEN WATER SEASON - APRIL TO NOVEMBER
        IF(THICE.EQ.0) THEN
            IF(MONTH.GE.6.AND.MONTH.LE.8) THEN
                HKMAX=HKMXSM
                WCOEF=WCFSM
                WSTR=WSSM
            ELSE
                HKMAX=HKMXSM
                WCOEF=WCFSF
                WSTR=WSSF
            ENDIF
        ENDIF
        
        !I think the ice formation condition must be prior to call
        !subroutine ICE and SNOW.  That is certainly true!
        !Determine volume averaged lake water temperature!  
        !MELMON, MELDAY= The first month or day of ice melting.
        !ICEMON, ICEDAY= The first month or day of freezing.
        IF((MONTH.EQ.1).AND.(MDAY.EQ.1)) MELMON=4
        IF((MONTH.EQ.MELMON).AND.(MDAY.EQ.MELDAY)) THEN
            IF(THICE.GT.0.0) MELMON=MELMON+1
        ENDIF
        IF(THICE.GT.0.0) ICESTAR=1              
        IF(ICESTAR.EQ.1) GOTO 50
        IF(THICE.GT.0.0) THEN
                MELMON=MONTH
                MELDAY=MDAY
                ICESTAR=1
        ENDIF
        !During open water season
        50  IF(MONTH.EQ.MELMON.AND.MDAY.GT.MELDAY) THEN
                THICE=0.0
                ICESTAR=-1
                NSWSTAR=-1
        ENDIF 
        IF(MONTH.EQ.ICEMON.AND.MDAY.LT.ICEDAY) THEN
                THICE=0.0
                ICESTAR=-1
                NSWSTAR=-1
        ENDIF 
        IF(MONTH.LT.ICEMON.AND.MONTH.GT.MELMON) THEN
                THICE=0.0
                ICESTAR=-1
                NSWSTAR=-1
        ENDIF 
        !Call subroutine SNOW to determine snow thickness and heat flux
        IF(NSWSTAR.EQ.1) GOTO 55
        IF(SNOWFL(MDAY).GT.0.0.OR.THSNOW.GT.0.0) THEN
                MNSNOW=MONTH
                NDYSNOW=MDAY
                NSWSTAR=1
        ENDIF 
        55  IF(NSWSTAR.EQ.1) THEN
                CALL SNOW(SNOWMELT)
        ENDIF 
        !Call subroutine ICE to determine ice thickness and heat flux
        IF(ICESTAR.EQ.1) THEN
                CALL ICE(SNOWMELT)
        ENDIF
        RETURN
END

!**********************************************************
!*                                                        *
!*                                                        *
!**********************************************************
SUBROUTINE SNOW(SNOWMELT)
!To determine snow thickness and heat flux
REAL*8 A,V,TV,ATOP
INTEGER FMON,FDAY,FYEAR
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),PR(31),DRCT(31)
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
COMMON/STEPS2/MBOT,ILAY
COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
COMMON/CONR/FAKW,SNCOE,COEWIN

XCOMP=0.0
XRAD=0.0
XRAIN=0.0
XCOV=0.0
XEVA=0.0
!AVOID THE CASE WHEN SNOWFALL HAPPENS WITHOUT ICE COVER IN A LAKE
IF(THICE.LE.0.0) THEN
    THICE=0.0
    THSNOW=0.0
    SNOWMELT=0.0
    RETURN
ENDIF 
!SNOW ACCUMULATION FROM THE PRECIPITATION
!SNOWFL is given as meters!
THSNOW=THSNOW+SNOWFL(MDAY)*CFSNOW
XCOMP=SNOWFL(MDAY)*CFSNOW
!SNOW DEPTH IS ZERO, IGNORE SNOW MELTING PROCESSES
IF(THSNOW.LE.0.0) THEN
    THSNOW=0.0 
    SNOWMELT=0.0
    RETURN
ENDIF
!Time step = one day
DLTIME=1.0
!Latent heat of fusion of snow in kcal/kg 
HTLTNT=80.0
!Density of snow in kg/m**3
DENSNOW=300.0
DENICE=920.0
DENWAT=1000.0
DRATIO=DENWAT/DENSNOW
!The ratio of snow depth and ice thickness shoud be less
!than (DENwater-DENice)/DENsnow (GU and Stefan, 1990)
!RATSNIC=(DENWAT-DENICE)/DENSNOW = 80/300 = 26.67%, but is too small according
!to field data. So RATSNIC = SNCOE is an input parameter - NOT implemented.
RATSNIC=(DENWAT-DENICE)/DENSNOW
!RATSNICN=COEWIN
!Total short wave solar radiation within snow      
!RAD - solar radiation in cal/cm*cm/day - kcal/m*m/day
HTSNOW=RAD(MDAY)*10.0*(BTSNOW*(1.0-ALFSNOW)+(1.0-BTSNOW)*(1.0-ALFSNOW)*(1.0-EXP(-GMSNOW*THSNOW)))  
!Determine evaporative heat loss Qe = f(w)*(Ea-Es) 
!Ea and Es is vapor pressure of air and at snow surface
!Formula for both Ea and Es is the same as in MINLAKE 
!6.11 mb is vapor pressure at Ts=0 !or 32 F.
!ice or snow surface temperature is equal to 32 F during
!the active thawing period (Shen and Chiang, 1984).
!Wind function coefficients for Qe and Qr are different
!which are from Light, 1941.  DQE is in inches/day !
TDEWMLT=TDEW(MDAY)
CPW2=7.45*TDEWMLT/(235.0+TDEWMLT)
EA=6.035*10.0**CPW2
DQE=COEWIN*WIND(MDAY)*0.0231*(EA-6.11)
!Determine sensible (conductive or convective) heat flux
!into (atmosphere) air at snow surface,  Qa=ha*(Ts-Ta).
!Ta is temperature at the top of snow layer = 32 F.
!Ts is air temperature - TAIR(MDAY) in F - TSNW
!Ha=1.8*Den*Latent*Wind*10**(-0.0000512*Zm)*E-4 (from Light, 1941)!
!Wind speed WIND(MDAY) is in mph for MINLAKE, It should
!be m/s for calculating Ha, 1mph = 0.447m/s;
!Den*Latent=80kcal/kg*300kg/m**3=24000 kcal/m**3;
!Zm is lake elevation above sea level in m.
!Gu's formula (23b) in his paper was wrong!His program is RIGHT!
!CGU used equation (5)*4 (Light, 1941) - inches/6hrs = 4inches/day.  
!  SNOWMELT=WIND(MDAY)*(0.00736*(TSNW-32.0)*10**CPOWER
!    +                    +0.0231*(EA-6.11))           
CPOWER=-0.0000156*800.0
TSNW=32.0+9.0*TAIR(MDAY)/5.0
DQA=COEWIN*WIND(MDAY)*0.00736*10**CPOWER*(TSNW-32.0)
!Check equation (22b) in Gu&Stefan, We have Den*Latent/DLTIME
!in left side, DLTIME=1 for no effect for numerical values!  
!0.9 is due to different elevation of wind & temperature measurement!
!(This equation is actually from Light, 1941.)
SNOWMELT=0.9*(DQE+DQA)
!Change snowmelt depth from inches to meters
SNOWMELT=SNOWMELT*0.0254
IF(SNOWMELT.LT.0.0) SNOWMELT=0.0
!Snowmelt from solar radiation independent of air temperature!
!Set -1.0 to give the best results for Ryan Lake - Don't Change
IF(TAIR(MDAY).GT.0.0) THEN
    SNOWMELT=SNOWMELT+HTSNOW*DLTIME/(DENSNOW*HTLTNT)
ENDIF
!Consider rainfall effect PR(MDAY) in inches as Tair > 0.0
!DRATIO=DENw/DENs = 1000.0/300.0, GU USED DRATIO=3.0 !
IF(TAIR(MDAY).GT.0.0) THEN
    SNOWMELT=SNOWMELT+PR(MDAY)*TAIR(MDAY)*DRATIO/HTLTNT*0.0254
ENDIF
!Determine finally snow thinckness including snowmelt!      
IF(SNOWMELT.LE.0.0) SNOWMELT=0.0
IF(SNOWMELT.GT.THSNOW) SNOWMELT=THSNOW
THSNOW=THSNOW-SNOWMELT
!XCR=0.02 M Capillary rise above free-water-level
!SCR=800.0 average specifi!gravity of snow wetted
!capillary rise (range of 600 to 900 kg/m**3)
!SOF=800.0 average specific gravity of wetted snow
!from top of ice to free-water-level.
!Lake Ice Formation by Nelson, 1995
DENSOF=800.0
DENSCR=800.0
XCR=0.02
OTHS=THSNOW
OTHI=THICE
IF(THSNOW.GT.0.0) THEN
    RATO=THSNOW/THICE
    IF(RATO.GT.RATSNIC) THEN
        SWSS=DENWAT+DENSNOW-DENSOF
        XOF=THSNOW*DENSNOW+XCR*(DENSCR-DENSNOW)
        XOF=(XOF+THICE*(DENICE-DENWAT))/SWSS
        IF(XOF.GT.THSNOW) THEN
        !It occurs when THICE is small (e.g. 5 cm) and THSNOW is very 
        !close to SRAT*THICE.        
            THICE=0.0
            THSNOW=0.0      
        ELSE
            THICE=THICE+XOF
            THSNOW=THSNOW-XOF
        ENDIF
        !DSNOW=(THSNOW-RATICSN*THICE)/(1+RATICSN)
        !THSNOW=THSNOW-DSNOW
        !THICE=THICE+DSNOW
    ENDIF
ENDIF
RETURN 
END


!**********************************************************
!*                                                        *
!*                                                        *
!**********************************************************
SUBROUTINE ICE(SNOWMELT)
!To determine ice thickness and heat flux
REAL*8 A,V,TV,ATOP
INTEGER FMON,FDAY,FYEAR
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),PR(31),DRCT(31)
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
COMMON/STEPS2/MBOT,ILAY
COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
COMMON/INFLOWX/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
COMMON/CONR/FAKW,SNCOE,COEWIN
COMMON/NEW4/ACSFL
DIMENSION YI(5),XI(5),D1Y(5),D2Y(5),D3Y(5)

XGROW=0.0
XRAD=0.0
XRAIN=0.0
XREFRO=0.0
IF(THICE.EQ.0.0) ACSNOW=0.0
!CFX DLTIME - time step dt in day
DLTIME=1.0
!C2 Thermal conductivity of ice in kcal/day/m/C, CDIS0 in W/m/C      
CNDICE=0.2388*3600.0*24.0/1000.0*CDIS0
!C3 Density of ice in kg/m**3      
DENICE=920.0
DENWAT=1000.0
DRATIO=DENWAT/DENICE
!C3 Latent heat of fusion of ice or Snow in kcal/kg
!C3 1KJ = 0.2388 Kcal = 1 KW-S
HTLTNT=80.0
!C4 Density of snow in kg/m**3
DENSNOW=300.0
!C5 Thermal conductivity of snow in kcal/day/m/C - CNDSNOW
!C5 CNDSNW0 is in W/m/C for thermal condutivity (J=W-s).
CNDSNOW=CNDSNW0*0.2388*3600.0*24.0/1000.0
!C6 Temperature Tm at ice-water interface in C   
TMELT=0.0
!Solar radiation absorbed within ice and reaching bottom of ice
HTSNW=RAD(MDAY)*10.0*(1.0-BTSNOW)*(1.0-ALFSNOW)*EXP(-GMSNOW*THSNOW)
IF(THSNOW.EQ.0.0) HTSNW=RAD(MDAY)*10.0
HTICE=HTSNW*(BTICE*(1.0-ALFICE)+(1.0-BTICE)*(1.0-ALFICE)*(1.0-EXP(-GMICE*THICE)))

!Hsa=0.29-0.37U - bulk heat transfer coefficient as a function
!of wind speed (Fertuck et al, 1971) in BTU/(ft*ft)/hr/F.
!Gu used an average coefficient Hsa=0.33 U (U in mph)
!1BTU/(ft*ft)/hr/F=1055*0.2388/0.0929*24*1.8/1000
!=117 Kcal/(m*m)/day/C. (hr - hour, F = 1.8 C)
!Hsa includes any effects of heat exchange at ice surface 
HSA=0.33*117.0*COEWIN*WIND(MDAY)
!For equation 16 (Gu)to calculate Qi in Kcal/(M*M)/day
AC=THICE/CNDICE
BC=THSNOW/CNDSNOW
CC=1.0/HSA
EC=TMELT-TAIR(MDAY)
HCNDISO=EC/(AC+BC+CC)
DC=DLTIME/(DENICE*HTLTNT)
!For determining Qwi - QWATER at ice-water interface      
!CNDWI - Thermal conductivity of Water.
!Question - TCHAR is water temperature at DEPTHC
!DEPTHC = 0.1 m (Fang) or 0.75 (Gu)
!    DO 20 I=1,MBOT-1
!      IF(Z(1).GE.DEPTHC) THEN
!        TCHAR=T2(1)*(Z(1)-DEPTHC)/Z(1)
!      ENDIF      
!      IF(Z(I+1).GE.DEPTHC.AND.Z(I).LT.DEPTHC) THEN
!        TCHAR=T2(I)+(T2(I+1)-T2(I))*(DEPTHC-Z(I))/(Z(I+1)-Z(I))
!      ENDIF
!   20 CONTINUE
!QWATER=FAKW*CNDWI*(TCHAR-TMELT)/DEPTHC
!Determine the temperature gradient at z = 0
!using three or four simulated ponits.
!YI - WATER TEMPERATURE AND XI IIS DEPTH Z      
YI(1)=0.0
XI(1)=0.0
NUM=3
DO 20 I=1,NUM
    YI(I+1)=T2(I)
    XI(I+1)=Z(I)
20   CONTINUE
DO 22 I=1,NUM
    D1Y(I)=(YI(I+1)-YI(I))/(XI(I+1)-XI(I))       
22   CONTINUE
DO 24 I=1,NUM-1
    D2Y(I)=(D1Y(I+1)-D1Y(I))/(XI(I+2)-XI(I))
24   CONTINUE
IF(NUM.LT.3) THEN
    D3Y(1)=0.0
ELSE
    DO 26 I=1,NUM-2
        D3Y(I)=(D2Y(I+1)-D2Y(I))/(XI(I+3)-XI(I))
    26    CONTINUE
ENDIF

!NOTICE THAT XI(1) = 0.0, THEREFORE:
DTDZ=D1Y(1)-D2Y(1)*XI(2)-D3Y(1)*XI(2)*XI(3)
QWATER=FAKW*CNDWI*DTDZ
IF(QWATER.LE.0.0) QWATER=0.0
IF(THICE.LE.0.0) QWATER=0.0 
!THICE in right hand side is ice thickness at previous day
!THICE is left hand side is ice thickness at the end of today!
QAIR=EC/(AC+BC+CC)
THICE=THICE+DC*(QAIR-QWATER)
XGROW=DC*(QAIR-QWATER)

!More conditions - That is good!
IF(THICE.LE.0.0) THICE=0.0
IF(THICE.EQ.0.0)  GOTO 92
IF(TAIR(MDAY).GT.0.0) THEN
    THICE=THICE-HTICE*DC
    XRAD=-HTICE*DC 
ENDIF

IF(THSNOW.GT.0.0) GOTO 92
!Consider effect of rainfall PR(MDAY)
!For rainfall with having temperature = TAIR in C
!PR(MDAY) is in inches = 0.0254 meters !
!Heat balance: PR*As*DENw*Cp*(TAIR-0.0)=DHi*As*DENi*Latent
!DHi=PR*TAIR/80.0*(DENw*Cp/DENi) =PR*TAIR*0.0254/80.0
!IF DENw=1000.0, DENi=920.0kg/m**3, PR in inches and
!Cp=4.2 kJ/kg/C = 1.0 kcal/kg/C !!!
IF(TAIR(MDAY).GT.0.0) THEN
    THICE=THICE-PR(MDAY)*0.0254*TAIR(MDAY)*DRATIO/80.0
    XRAIN=-PR(MDAY)*0.0254*TAIR(MDAY)*DRATIO/80.0
ENDIF
92  IF(THICE.LT.0.0) THICE=0.0
OTHI=THICE
              
!The new algorithm for ice growth on the top of the ice
!    IF(SNOWMELT.GT.0.0) THEN
!       ACSNOW=ACSNOW+SNOWMELT
!    ENDIF
!Melted water from snow refroze during the cold night.      
!    IF(ACSNOW.GT.0.0.AND.TAIR(MDAY).LT.0.0) THEN
!       THICE=THICE+ACSNOW*DENSNOW/DENICE
!       XREFRO=ACSNOW*DENSNOW/DENICE
!       ACSNOW=0.0
!    ENDIF         
              
ACSFL=ACSFL+SNOWFL(MDAY)      
                     
!CALCULATE TOTAL HEAT FLUX !
!    IF(MDAY+MONTH*100.EQ.1216) THEN
!       HSTMSUM=0.0
!       QWATSUM=0.0
!       HTBTSUM=0.0
!       HTICSUM=0.0
!       IWRITE=6
!    ENDIF
!    QWATSUM=QWATSUM+QWATER*ATOP(1)/TV(MBOT)
!    HTBTSUM=HTBTSUM+HTBTM*ATOP(1)/TV(MBOT)
!    HSTMSUM=HSTMSUM+HSTMP
!    HTICSUM=HTICSUM+HCNDISO*ATOP(1)/TV(MBOT)
!    IF(IWRITE.EQ.6) THEN
!       WRITE(20,255) JDY,HSTMSUM,HTBTSUM,QWATSUM
!       255   FORMAT(I3,2X,3(F8.2,2X))     
!     ENDIF
!     DQFLUX=QWATER-HTBTM
         
RETURN
END

!**********************************************************
!*                                                        *
!*     FUNCTION FOR INTEPOLATION                          *
!*                                                        *
!**********************************************************
FUNCTION FXIN(ZDD,N,AD,AI)
DIMENSION AD(80),AI(80)
DO 150 I=2,N
    IF(ZDD.EQ.AD(I)) THEN
        FXIN=AI(I)
        RETURN
    ENDIF
    IF(ZDD.LT.AD(I)) THEN
        RAT=(AI(I)-AI(I-1))/(AD(I)-AD(I-1))
        FXIN=AI(I-1)+(ZDD-AD(I-1))*RAT
            RETURN
    ENDIF
150 CONTINUE
IF(ZDD.GT.AD(N).AND.I.GE.N) THEN      
    RAT=(AD(N)-AD(N-1))/(ZDD-AD(N-1))
    FXIN=AI(N-1)+(AI(N)-AI(N-1))/RAT
    AD(N)=ZDD
    AI(N)=FXIN
ENDIF

RETURN
END

!**********************************************************
!*                                                        *
!*                  SUBROUTINE BOTTOM                     *
!*                                                        *
!**********************************************************
SUBROUTINE BOTTOM(TSOL,TSNE,T2MBOT)
!Solve the heat conduction equation for sediment temperature distribition Ts

COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT

!COMMON/XING9/TSOL(21),TSNE(21)

DIMENSION PS(21),QS(21),AS(21),BS(21),CS(21),DS(21)

DIMENSION TSOL(21),TSNE(21)

!Constant DZSL=DZ in meters, DT=1.0 day, AHTBTM=Kb thermal
!diffusivity of sediment in m*m/day !  
!Stability criteria: PRMM >= 2.0
PRMM=DZSL**2/(AHTBTM*1.0)
!Set coefficient, What kind of scheme, TSL is previous day sediment
!temperature?  It seems I=1 is the bottom of sediment (deepest part)
!and I=IZSLT is sediment-water interface, ZSLT - depth of sediment!
DO 3 I=1,IZSLT
    AS(I)=-1.0/PRMM
    BS(I)=1.0
    CS(I)=-1.0/PRMM
    DS(I)=(1.0-2.0/PRMM)*TSOL(I)
3   CONTINUE

!What about boundary conditions !
!Solution techniques: (Please see Song's notes)
!As(j)*Ts(j+1)+Bs(j)*Ts(j)+Cs(j)*Ts(j-1)=Ds(j)
!Ts(j+1) = Ps(j)*Ts(j) + Qs(j)
!Therefore Ps(j+1)=-Cs(j)/[As(j)*Ps(j)+Bs(j)],
!Qs(j-1)=[Ds(j)-As(j)*Ps(j)]/[As(j)*Ps(j)+Bs(j)].
PS(1)=1.0
QS(1)=0.0
PS(2)=-CS(2)/(AS(2)+BS(2))
QS(2)=DS(2)/(AS(2)+BS(2))
DO 10 I=3,IZSLT
    PS(I)=-CS(I)/(AS(I)*PS(I-1)+BS(I))
    QS(I)=(DS(I)-AS(I)*QS(I-1))/(AS(I)*PS(I-1)+BS(I))
10  CONTINUE  
!Boundary condition at sediment-water interface, T2-water temperature
TSNE(IZSLT)=T2MBOT
DO 15 I=IZSLT-1,1,-1
    TSNE(I)=PS(I)*TSNE(I+1)+QS(I)
15  CONTINUE     
RETURN
END     

!**********************************************************
!*                                                        *
!*     SEDIMENT HEAT FLUX OF ALL LAYERS                   *
!*                                                        *
!**********************************************************   
SUBROUTINE SEDIMENT(HTBTM,T2MBOT,K,MYEAR)
!Call subroutine BOTTOM for Ts, then calculate heat flux
!from or to the lake sediment (HTBTM) 
INTEGER FMON,FDAY,FYEAR
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
COMMON/SEDIMENT0/ISTARTX,TSO(21,120),TSN(21,120)
!DIMENSION TSO(21,120),TSN(21,120)

IF(ISTARTX.LE.6) THEN
    DO 10 I=1,IZSLT
        DO 10 J=1,MBOT
            TSO(I,J)=TSL(IZSLT-I+1,J)
    10 CONTINUE
    ISTARTX=60
ENDIF

!TSO - Old/yesterday sediment temperature (TS), TSN - today or current TS

CALL BOTTOM(TSO(1,K),TSN(1,K),T2MBOT)

!To output the sediment temperature; j is the water layer; j=10 means at 1.0 m depth in Carrie Lake
!    IF(K.EQ.16) Then
!        Write(465,65) MYEAR,MONTH,MDAY,(TSO(IK,16),IK=1,IZSLT)
!        65  Format(I4,2X,I2,2X,I2,2X,23F7.3)
!    EndIf
    
!To output the sediment temperature; j is the water layer; J=10 means at 1.0 m depth in Carrie Lake
!    IF(K.EQ.10) Then
!        Write(465,65) MYEAR,MONTH,MDAY,ISTARTX,TSL(11,K),TSO(11,K),TSO(1,K)
!        Write(465,65) MYEAR,MONTH,MDAY,(TSO(IK,K),IK=1,IZSLT)
!        65  Format(1X,I4,2X,I2,2X,I2,2X,I8,2X,21F7.3)
!    EndIf
!   IF(K.EQ.MBOT) Then
!        Write(467,65) MYEAR,MONTH,MDAY,ISTARTX,TSO(11,K),TSO(1,K)
!        Write(467,65) MYEAR,MONTH,MDAY,(TSO(IK,K),IK=1,IZSLT)
!    EndIf

HTBTM=0.0

DO 16 I=1,IZSLT-1
    HTBTM=HTBTM+SRCP*DZSL*(TSO(I,K)-TSN(I,K))
    TSO(I,K)=TSN(I,K)
16  CONTINUE     
   TSO(IZSLT,K)=TSN(IZSLT,K)             
RETURN
END

!C**************************************************C
       
!C**************************************************C
FUNCTION ENTRAIN(I,DCF,RDC,RHOAMB,DELZ,S,SUMZ,Q,IHP,WIDTH,FT)

!Compute the entrainment from a layer into the density current (from Akiyama)

IF(I.GT.IHP) THEN
    EPSI=(RDC-RHOAMB)/RHOAMB
    IF(I.NE.IHP+1)  GO TO 3
    FD=1.875E-4+FT
    F43=((FD+SQRT(FD*FD+0.0045*S))/(1.5*S))**(1.3333)
    3   X=DCF/(86400*WIDTH)
    GAMAI=0.0015*DELZ*(9.81*EPSI/(X*X))**(.3333)/(F43*S)
    ENTRAIN=GAMAI*DCF
ELSE
    ENTRAIN=Q*DELZ/SUMZ
ENDIF
RETURN
END

!C**********************************************************C
!                                                           C
!                                                           C
!C**********************************************************C
SUBROUTINE ERROR(IST,IDO,TETH,DOTH)
!THE PROGRAM IS TO MAKE STATISTICAL PAPRAMETERS OF ERROR ANALYSIS
!CHECK THE GU'S OR FANG'S THESIS
INTEGER IST,IDO
REAL TETH,DOTH 
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
COMMON/NAMELAKE/TAPE514
DIMENSION X(1600,7),Y(1600,7),DEPTH(1600),SLOPE(12),R2(12),STD(12)
DIMENSION XDE(200,5,2),YDE(200,5,2),IN(5),TETH(5),DOTH(5),RMSMIX(3),BEMIX(2)
              
!STORE DTATA FOR ERROR ANALYSIS
!1 - TEMPERATURE, 2 - DISSOLVED OXYGEN
!XDE(I,Jdepth,IDO) - SIMULATED as IDO=1
!Jdepth=1,2,3,4,5 for each of five depths
!I - data point series
!YDE(I,Jdepth,IDO) - MEASURED as IDO=2

!IN(I) - number of data point at each of the 5 depths
IF(IN(1).LT.1) THEN
    DO 45 I=1,NDEPTH
        IN(I)=1
    45 CONTINUE
ENDIF

IF(IST.EQ.1) THEN
    DO 62 J=1,NDEPTH
        IN(J)=IN(J)+1
        II=IN(J)
        IF(IDO.EQ.1) THEN
        !C... SIMULATIONS as IDO=1
            XDE(II,J,1)=TETH(J)
            XDE(II,J,2)=DOTH(J)
            IN(J)=IN(J)-1
        ELSE
        !C... FIELD DATA as IDO=2      
            YDE(II,J,1)=TETH(J)
            YDE(II,J,2)=DOTH(J)
        ENDIF
    62  CONTINUE       
    DO 68 J=1,NDEPTH
        IF(TETH(J).LT.0.0.OR.DOTH(J).LT.0.0) THEN
            IN(J)=IN(J)-1
        ENDIF
    68  CONTINUE
    RETURN
ENDIF
              
!N -- NUMBER OF DATA
!X(I, Index), Y(I, Index) -- SIMUATED RESULTS AND MEASURED DATA
!Index =1: Temp for all depths
!Index =2: DO for all depths
!Index =3: Temp in the mixed layer
!Index =4: DO in the mixed layer
!Index =5: Mixed layer depth
!Index =6: Temp BELOW the mixed layer
!Index =7: DO BELOW the mixed layer 
!REWIND 66
  
REWIND 67
REWIND 68
REWIND 69
REWIND 567
REWIND 568
REWIND 569
             
READ(67,*)
READ(68,*)
READ(69,*)
READ(567,*)
READ(568,*)
READ(569,*)
               
DO MK=1,3
    RMSMIX(MK)=0.0
ENDDO

!Water Temperature Data Pairs
!Y(I,6) - DO below the mixed layer
IBELOW=1
BEMIX(1)=0
DO IL=1,1600
    READ(567,*,END=701) Y(IL,1),X(IL,1),DEPTH1,MFLAG1,MONTH1,MDAY1,MYEAR1,KK1,DMIX
    IF(DEPTH1.GT.DMIX) THEN
     Y(IBELOW,6)=Y(IL,1)
     X(IBELOW,6)=X(IL,1)
     BEMIX1=X(IBELOW,6)-Y(IBELOW,6)
     IF(ABS(BEMIX(1)).LT.ABS(BEMIX1)) BEMIX(1)=BEMIX1
     IBELOW=IBELOW+1
    ENDIF
ENDDO

!Dissolved Oxygen Data Pairs
!Y(I,7) - DO below the mixed layer 
701 NBELOW6=IBELOW-1
    IBELOW=1
    BEMIX(2)=0
DO IK=1,1600
    READ(568,*,END=702)Y(IK,2),X(IK,2),DEPTH1,MFLAG1,MONTH1,MDAY1,MYEAR1,KK1,DMIX
    IF(DEPTH1.GT.DMIX) THEN
     Y(IBELOW,7)=Y(IK,2)
     X(IBELOW,7)=X(IK,2)
     BEMIX2=X(IBELOW,7)-Y(IBELOW,7)
     IF(ABS(BEMIX(2)).LT.ABS(BEMIX2)) BEMIX(2)=BEMIX2
     IBELOW=IBELOW+1
    ENDIF
ENDDO

702   NBELOW7=IBELOW-1
               
   DO IK1=1,1600
        READ(569,*,END=70) Y(IK1,3),X(IK1,3),Y(IK1,4),X(IK1,4),Y(IK1,5),X(IK1,5)
        RMSMIX1=X(IK1,3)-Y(IK1,3)
        RMSMIX2=X(IK1,4)-Y(IK1,4)
        RMSMIX3=X(IK1,5)-Y(IK1,5)
        IF(ABS(RMSMIX(1)).LT.ABS(RMSMIX1)) RMSMIX(1)=RMSMIX1
        IF(ABS(RMSMIX(2)).LT.ABS(RMSMIX2)) RMSMIX(2)=RMSMIX2
        IF(ABS(RMSMIX(3)).LT.ABS(RMSMIX3)) RMSMIX(3)=RMSMIX3
   ENDDO
 
70  CONTINUE        
    DO 75 J=1,NDEPTH
        IN(J)=IN(J)-1
    75  CONTINUE         
        
NTEMP=IL-1
NDO=IK-1
NMIX=IK1-1
K=1    
      
40     CONTINUE
 
IF(K.EQ.1) N=NTEMP
IF(K.EQ.2) N=NDO
IF(K.GT.2) N=NMIX
IF(K.EQ.6) N=NBELOW6
IF(K.EQ.7) N=NBELOW7

! LEAST SQUARE ANALYSIS
! The program does not compute error parameters when there are only 4 or less data points
IF(N.LT.5) THEN
 K=K+1
 GOTO 40
ENDIF

SX=0.0
SY=0.0
DO 10 I=1,N
    SX=SX+X(I,K)
    SY=SY+Y(I,K)
10    CONTINUE

AX=SX/FLOAT(N)
AY=SY/FLOAT(N)
SXY=0.0
SXY2=0.0 
SXX=0.0
SYY=0.0
DO 20 I=1,N
    SXY=SXY+(X(I,K))*(Y(I,K)) 
    SXY2=SXY2+(X(I,K)-Y(I,K))**2.0
    SXX=SXX+(X(I,K))**2.0     
    SYY=SYY+(Y(I,K)-AY)**2.0
20  CONTINUE

IF(SXX.EQ.1E-3.OR.SYY.EQ.1E-3) THEN
!It happens when either simulated or measured DISSOLVED
!OXYGEN CONCENTRATIONS near the lake bottom ARE ZERO !!  
  SLOPE(K)=0.0
  R2(K)=0.0
  STD(K)=(SXY2/FLOAT(N))**0.5
ELSE   
 SLOPE(K)=SXY/SXX
 R2(K)=1.0-SXY2/SYY
 STD(K)=(SXY2/FLOAT(N))**0.5
ENDIF

K=K+1

!For Tmix and Zmix
IF(K.EQ.3.OR.K.EQ.5) GOTO  40

!For Temp below the mixed layer
IF(K.EQ.6) GOTO  40

!Exit the computation
IF(K.GT.7) GOTO 30

!Check whether to compute parameters for DO
IF(K.EQ.2) THEN
! IPRNT(4).EQ.1 --- Having DO simulations                
    IF(IPRNT(4).EQ.1) THEN                      
        GOTO 40                     
    ELSE
        RMS(2)=0.0
        SLOPE(2)=0.0
        R2(2)=0.0
        STD(2)=0.0
        K=3
        GOTO 40
    ENDIF
 ENDIF
 
!Check whether to compute parameters for DOmix
 IF(K.EQ.4) THEN
    IF(IPRNT(4).EQ.1) THEN                   
        GOTO 40                 
    ELSE
        SLOPE(4)=0.0
        R2(4)=0.0
        STD(4)=0.0                      
        K=5
        GOTO 40
   ENDIF
 ENDIF
 
!Check whether to compute parameters for DO below the mixed layer
 IF(K.EQ.7) THEN
    IF(IPRNT(4).EQ.1) THEN                   
        GOTO 40                 
    ELSE
        SLOPE(7)=0.0
        R2(7)=0.0
        STD(7)=0.0
!Exit the computation    
        GOTO 30
   ENDIF
 ENDIF             

!Output for Sens_Error.data
30 WRITE(398,*)
WRITE(398,*) 'TEMP&DO SIMULATED(1) OR TEMP SIMULATED ONLY(0):'
WRITE(398,*) IPRNT(4)
WRITE(398,*) 'Number of data pairs for Tempe and DO Below the mixed layer'
WRITE(398,*) NBELOW6,NBELOW7
WRITE(398,*)'Error values in report format and for sensitivity analysis'
WRITE(398,*)
WRITE(398,3063)
WRITE(398,3064)
WRITE(398,3066)NMIX,NTEMP,STD(1),R2(1),SLOPE(1),STD(2),R2(2),SLOPE(2),STD(5),R2(5),SLOPE(5),STD(3),R2(3),SLOPE(3),STD(4),R2(4),SLOPE(4),STD(6),R2(6),SLOPE(6),STD(7),R2(7),SLOPE(7)
WRITE(398,*)
  3063   FORMAT('                Temp               DO                 Depth Mix        Temp Mix            DO Mix            Temp Below        DO Below      ')
  3064   FORMAT(' DAY  NO |  SE     R2    Slope |  SE     R2    Slope |  SE     R2    Slope |  SE     R2    Slope |  SE     R2    Slope |  SE     R2    Slope |  SE     R2    Slope |')
  3066   FORMAT(I3,2X,I4,1X,3(F6.2,1X),1X,3(F6.2,1X),1X,3(F6.2,1X),1X,3(F6.2,1X),1X,3(F6.2,1X),1X,3(F6.2,1X),1X,3(F6.2,1X))
    

!Output for Lake_Error.data
WRITE(98,3015)
  3015  FORMAT(1X/1X,'ANALYSIS OF ERRORS BETWEEN DATA AND MODEL')
WRITE(98,3016)  
  3016  FORMAT(37X,'TEMP',6X,'DO',6X,'ZMIX',5X,'TMIX',4X,'DOMIX',4X,'TBelow',3X,'DOBelow')                        
WRITE(98,*)
WRITE(98,3030) RMS(1),RMS(6),RMSMIX(3),RMSMIX(1),RMSMIX(2),BEMIX(1),BEMIX(2)
  3030  FORMAT(1X,'MAXIMUM ABSOLUTE ERROR',9X,10(2X,F7.2))
WRITE(98,3017) SLOPE(1),SLOPE(2),SLOPE(5),SLOPE(3),SLOPE(4),SLOPE(6),SLOPE(7)
  3017  FORMAT(1X,'SLOPE: MODEL TO DATA REGRESSION',10(2X,F7.2))
WRITE(98,3018) R2(1),R2(2),R2(5),R2(3),R2(4),R2(6),R2(7)
  3018  FORMAT(1X,'REGRESSION COEFFICIENT (R**2)',2X,10(2X,F7.2))
WRITE(98,3019) STD(1),STD(2),STD(5),STD(3),STD(4),STD(6),STD(7)
  3019  FORMAT(1X,'STANDARD ERROR OF ESTIMATE',5X,10(2X,F7.2))

!Output for Excel reading
WRITE(98,*)
WRITE(98,3014) RMS(1),RMS(6), RMSMIX(3), RMSMIX(1), RMSMIX(2),BEMIX(1),BEMIX(2)
WRITE(98,3014) SLOPE(1),SLOPE(2),SLOPE(5),SLOPE(3),SLOPE(4),SLOPE(6),SLOPE(7)
WRITE(98,3014) R2(1),R2(2),R2(5),R2(3),R2(4),R2(6),R2(7)
WRITE(98,3014) STD(1),STD(2),STD(5),STD(3),STD(4),STD(6),STD(7)
  3014  FORMAT(32X, 10(2X,F7.2))

!WRITE(98,3020) NTEMP,NDO 
!3020  FORMAT(1X/1X,'TOTAL NUMBER OF FIELD DATA FOR TEMP and DO IS:',4X,I6,4X,I6)
!WRITE(98,3065) NMIX
!3065  FORMAT(1X,'TOTAL NUMBER OF FIELD DATA FOR MIXED LAYER DATA IS:',4X,I6)

WRITE(98,3025)
    3025  FORMAT(1X/1X,'ANALYSIS OF ERRORS BETWEEN DATA AND',1X,'MODEL AT CERTAIN DEPTHS')

! Find error parameters by depths
K=1
               
90  DO 50 J=1,NDEPTH
        IF(IN(J).LT.5) GOTO 50
        SX=0.0
        SY=0.0
        DO 52 I=1,IN(J)
            SX=SX+XDE(I,J,K)
            SY=SY+YDE(I,J,K)
        52    CONTINUE
        AX=SX/FLOAT(IN(J))
        AY=SY/FLOAT(IN(J))
        SXY=0.0
        SXY2=0.0 
        SXX=0.0
        SYY=0.0
        !jaugust 2009 --Shoeb Alam -- slope of regression is modified according to the Appendix A of DO model for regional
        ! lake analysis (by HG Sefan and X Fang) as it was in original MINLAKE MODEL (as seen in manual)            
        DO 56 I=1,IN(J)
        !SXY=SXY+(XDE(I,J,K)-AX)*(YDE(I,J,K)-AY)  ! this line was used lastly
            SXY=SXY+(XDE(I,J,K))*(YDE(I,J,K))  ! modified in August 2009
            SXY2=SXY2+(XDE(I,J,K)-YDE(I,J,K))**2.0
            !SXX=SXX+(XDE(I,J,K)-AX)**2.0  ! this line was used lastly
            SXX=SXX+(XDE(I,J,K))**2.0      ! modified in August 2009
            SYY=SYY+(YDE(I,J,K)-AY)**2.0
        56  CONTINUE
        IF(SXX.EQ.0.0.OR.SYY.EQ.0.0) THEN
            !It happens when either simulated or measured DISSOLVED
            !OXYGEN CONCENTRATIONS near the lake bottom ARE ZERO !!  
            SLOPE(J+7)=1.0
            R2(J+7)=1.0
            STD(J+7)=(SXY2/FLOAT(N))**0.5
        ELSE   
            SLOPE(J+7)=SXY/SXX
            R2(J+7)=1.0-SXY2/SYY
            STD(J+7)=(SXY2/FLOAT(IN(J)))**0.5
        ENDIF
50  CONTINUE
         
IF(K.EQ.1) THEN         
    WRITE(98,3026)
    3026  FORMAT(1X/37X,'WATER TEMPERATURES',/1X)
ELSE
    WRITE(98,3027)
    3027  FORMAT(1X/37X,'DISSOLVED OXYGEN',/1X)
ENDIF

WRITE(98,3028) (FDTH(II),II=1,NDEPTH)
    3028  FORMAT(1X,'DEPTHS (M) FOR ERROR ANALYSIS',2X,5(2X,F7.2))
WRITE(98,3017) (SLOPE(II+7),II=1,NDEPTH)
WRITE(98,3018) (R2(II+7),II=1,NDEPTH)
WRITE(98,3019) (STD(II+7),II=1,NDEPTH)
WRITE(98,3029) (IN(II),II=1,NDEPTH)
    3029  FORMAT(1X,'TOTAL NUMBER OF DATA POINTS',4X,5(2X,I7))
WRITE(98,*)
WRITE(98,3014) (FDTH(II),II=1,NDEPTH)
WRITE(98,3014) (SLOPE(II+7),II=1,NDEPTH)
WRITE(98,3014) (R2(II+7),II=1,NDEPTH)
WRITE(98,3014) (STD(II+7),II=1,NDEPTH)
WRITE(98,3013) (IN(II),II=1,NDEPTH)
    3013  FORMAT(32X,5(2X,I7))
IF(K.EQ.2) RETURN
IF(IPRNT(4).EQ.1) THEN
    K=2
    GOTO 90
ENDIF          

RETURN 
END       

!C**********************************************************C
!                                                           C
!                                                           C
!C**********************************************************C
SUBROUTINE ABOUND
!Computes the surface area of each layer (ATOP)
!using the depth area relationship in LAKE.
!ATOP(1) = surface area of the lake
!ATOP(MBOT+1) = 0.0

REAL*8 A,V,TV,ATOP,ADUM
INTEGER FMON,FDAY,FYEAR
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 

DUM=0.0
DO 100 I=1,MBOT
    ZDUM=ZMAX-DUM
    DUM=DUM+DZ(I)
    CALL LAKE(ZDUM,ADUM,0,1)
100     ATOP(I)=ADUM
        ATOP(MBOT+1)=0.0
RETURN
END
              
!C**************************************************C
!        
!C**************************************************C
SUBROUTINE ADVECT(P,HED,NFLOW,S,FT,WCHANL,ST,MYEAR)
!Inflow/outflow routines using plunging density
!current routines on inflow and surface outflow.
!Additional inflows and outflows can be included
!with calls to LAKE (see below).

REAL*8 A,V,TV,ATOP,ZERO
INTEGER FMON,FDAY,FYEAR
COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/INFLOWX/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/RESULTX/VAR(120,21)
!DIMENSION VOUT(11),IFL(11)
!DATA IFL/1,3,5,6,7,8,9,10,11,12,13/
ZERO=0.0
TRASH=MYEAR
!TAKE CARE OF INFLOW AND OUTFLOW
DELZ=P-HED
ZMAX=ZMAX+DELZ
DZ(1)=DZ(1)+DELZ
CALL SETZ(MBOT)
V(1)=V(1)+DELZ*ATOP(1)
!call to LAKE for additional surface inflows and outflows
CALL LAKE(0.,ZERO,NFLOW,10)
IF(NFLOW.GT.0) THEN
    IF(IPRNT(1).EQ.1) WRITE(8,2235)
    2235   FORMAT(//,5X,'INFORMATION ON INFLOW',/,5X,21('-'),/,6X, 'ISOPYCNIC',3X,'INITIAL VOLUME',7X,'INFLOW  (M3/DAY)',8X,'P',7X,  'NO3',7X,'NH4',6X,'DO',6X,'BOD',/,6X,'DEPTH (M)',7X,'OF LAYER (M3)',4X,'INITIAL',8X,'FINAL',5X,5('(MG/L)',3X),/)
    !C...QIN > 0.0 signifies inflow.  QIN < 0.0 signifies outflow.
    !C...this section skipped if QIN = 0.0
    DO 14 IW=1,NFLOW
        DO 15 I=1,MBOT
        15  QE(I)=0.0
        IF(QIN(IW).GT.0.0) THEN
            CALL DCFLOW(IPL,S,FT,WCHANL,IW)
            CALL CONSMAS(IW,IPL,QIN(IW))
        ENDIF
        IF(QIN(IW).LT.0.0) THEN
            CALL WDEPTH(ST,QIN(IW),LW)
        ENDIF
        DO 17 I=1,MBOT
        17 V(I)=V(I)-QE(I)
        I=1
        !C...merge low volume layers with next higher layer
        4   IF(V(I).LT.500.) THEN
                CALL MERGE(I,MBOT,ILAY)
                IF(I.GT.MBOT) GOTO 300
                GOTO 4
            ENDIF
        I=I+1
        IF(I.GT.MBOT) GOTO 300
                GOTO 4
        300  CALL THICKNS(MBOT)
        CALL SETZ(MBOT)
    14 CONTINUE
ENDIF
I=1
!determine that all layers are within the limits for
!maximum and minimum layer thickness
5   IF(DZ(I).LT.DZLL) THEN
        CALL MERGE(I,MBOT,ILAY)
        IF(I.GT.MBOT) GO TO 301
        GO TO 5
    END IF
I=I+1
IF(I.GT.MBOT) GO TO 301
GO TO 5
301  IF(MBOT.GE.120) GO TO 2
I=1
DZOLD=DZUL
3   IF(I.LE.3) THEN
        DZUL=DZOLD/3.0
    ELSE
    IF(I.LE.6) THEN
            DZUL=DZOLD/2.0
        ELSE
            DZUL=DZOLD
        ENDIF
    ENDIF
    IF((DZ(I).GT.DZUL).AND.(MBOT.LT.120)) THEN
        CALL SPLIT(I,ILAY)
        GO TO 3
    END IF
I=I+1
IF(I.GT.MBOT .OR. I.GT.120) GO TO 2
        GO TO 3
2  CALL SETZ(MBOT)
    DZUL=DZOLD
!DETERMINE LAKE STAGE FROM WATER BUDGET
    ZMAX=Z(MBOT)+DZ(MBOT)*0.5
    ST=DBL+ZMAX
    CALL VOLUME(MBOT)
    CALL AREA
    CALL ABOUND
    CALL TVOL(MBOT)
          
RETURN
END
          
!C********************************************************C
!                                                         C
!                                                         !
!C********************************************************C
          SUBROUTINE AREA

!C*** Compute the area through the middle of each layer
!C*** using the depth-area relationship in LAKE

REAL*8 A,V,TV,ATOP,ADUM
INTEGER FMON,FDAY,FYEAR
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 

DUM=0.0
DO 100 I=1,MBOT
    ZDUM=ZMAX-DUM-DZ(I)/2.
    DUM=DUM+DZ(I)
    CALL LAKE(ZDUM,ADUM,0,1)
100    A(I)=ADUM

RETURN
END
          
!C**********************************************************C
!                                                           C
!                                                           C
!C**********************************************************C
SUBROUTINE CHLORO(K,TD,IEUPH)

!C*** Compute the chlorophyll-a and BOD concentration
!C*** profiles using the constant volume finite difference
!C*** method (from Patankar) for models 2 and 3.  Model 1
!C*** computes the mixed layer chlorophyll-a concentration
!C*** following work by Forsberg and Shapiro.  Model 2 uses
!C*** Michaelis-Menton growth kinectics and Model 3 uses a 
!C*** cellular nutrient growth formulation (from Lehman, et. al.)

!MIKI REMOVE BB,UN,PDUM
REAL*8 A,V,TV,ATOP,AK,BK,CK,DK
INTEGER FMON,FDAY,FYEAR
COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),REPRO,XI,XKMZ,GRAZE(3,120)
COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
COMMON/PHYTO0/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),GROMAX(3),TMAX(3)
COMMON/PHYTO1/TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN(90),CHLMAX,SDY(90)
COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
COMMON/SOURCE/RM(3,120),PROD(120),XMR(3,120),PRODSUM(120)
COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
COMMON/SOLV/ AK(120),BK(120),CK(120),DK(120)
!DIMENSION BB(120),PDUM(120),UN(120)

K=K-1+1
TD=TD+1.0-1.0      
IF(MODEL.EQ.1) THEN
    DMIX=Z(ILAY)+DZ(ILAY)*0.5
    IDEPTH=ILAY
    IF(ILAY.LT.IEUPH) IDEPTH=IEUPH
    DO 23 I=1,IDEPTH
        X=1.-PMIN(1)*CHLA2(1,I)/PTSUM(I)
        IF(X.LT.0) THEN
            PROD(I)=0.0
        ELSE
            PROD(I)=1.9*PMAX(1)*X/((XK1+XK2*CHLA2(1,I))*DMIX*YCA)
        ENDIF
    23  CONTINUE
    IF(IDEPTH.LT.MBOT) THEN
        DO 24 I=IDEPTH+1,MBOT
        24  PROD(I)=0.0
    ENDIF             
    DO 25 I=1,MBOT
        CHLA2(1,I)=CHLA2(1,I)+CHLA2(1,I)*(PROD(I)-XKM(1))
        IF(CHLA2(1,I).LT.0.0001) CHLA2(1,I)=0.0001
        CHLATOT(I)=CHLA2(1,I)
    25  CHLADUM(1,I)=CHLA2(1,I)
    RETURN
ENDIF
END
          
!C**********************************************************C
!                                                           C
!                                                           C
!C**********************************************************C
SUBROUTINE COEF(MODEL,MBOT,NCLASS)

!C*** Compute some coefficients used in the constant
!C*** volume and finite difference solutions
REAL*8 A,V,TV,ATOP
COMMON/COEFF/DUM2(120),DUM3(120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
DO 100 I=2,MBOT-1
    DUM1= 2./(A(I)*DZ(I))
    DUM2(I)=DUM1*ATOP(I)*HMK(I)/(DZ(I)+DZ(I-1))
100 DUM3(I)=DUM1*ATOP(I+1)*HMK(I+1)/(DZ(I)+DZ(I+1))
KK=2
IF(MODEL.GT.1) KK=1
IF(MODEL.EQ.4) KK=2
DO 200 K=KK,NCLASS+1
    DO 200 I=2,MBOT
        X=FVCHLA(K)*(DZ(I-1)+DZ(I))*.5/HMK(I)
        !PE=(1.0-.1*ABS(X))**5/X 
        A0=1.0-0.1*ABS(X)
        A1=A0*A0
        PE(K,I)=A1*A1*A0/X
        PE(K,1)=0.0
        PE(K,MBOT+1)=0.0
200  CONTINUE
RETURN
END
          
!C**********************************************************C
!                                                           C
!                                                           C
!C**********************************************************C
SUBROUTINE CONMIX(ILAY,TMIX,MBOT)
          

!C*** Remove density instabilities by mixing unstable 
!C*** layers downward and merging with lower layers.

REAL*8 A,V,TV,ATOP,RHOT,RHODUM,RHO
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
DIMENSION RHOT(120)
          
DO 100 I=1,MBOT
    RHOT(I)=RHO(T2(I),0.,0.)
100  CONTINUE
6   IFLAG=0
I=0
M=MBOT-1
1   I=I+1
IF(I.EQ.MBOT) GO TO 5
IF(RHOT(I).LE.RHOT(I+1)) GO TO 1
IFLAG=0
IB=I
TVDUM=T2(I)*V(I)
VDUM=V(I)
TDUM=TVDUM/VDUM
 RHODUM=RHO(TDUM,0.,0.)
J=I-1
3   J=J+1
IF(RHODUM.LE.RHOT(J+1)) GO TO 2
IFLAG=IFLAG+1
TVDUM=TVDUM+T2(J+1)*V(J+1)
VDUM=VDUM+V(J+1)
TDUM=TVDUM/VDUM
RHODUM=RHO(TDUM,0.,0.)
IF(J.EQ.M) GO TO 2
GO TO 3
2   IE=IB+IFLAG
DO 200 K=IB,IE
    T2(K)=TDUM
200  RHOT(K)=RHODUM
4   IF(I.LT.M) GO TO 1
5   IF(IFLAG.NE.0) GO TO 6
!   DETERMINE MIXED LAYER DEPTH...
8    DO 700 I=1,MBOT-1
    IF(ABS(T2(I)-T2(I+1)).LE.0.001) GO TO 700
    ILAY=I
    GO TO 10
700  CONTINUE
ILAY=MBOT
10   TMIX=T2(1)
RETURN
END
          
!C**********************************************************C
!                                                           C
!                                                           C
!C**********************************************************C
SUBROUTINE MERGE(I,MBOT,LW)

!CMerge layers that are either low volume (V < 500 m3) or
!too thin (DZ < DZLL).  Negative layers are also handled
!by reducing the volume of the next lower layer by the negative volume.

REAL*8 A,V,TV,ATOP
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          
IF(V(I).LE.0.) THEN
    IF(I.EQ.MBOT) THEN
        II=MBOT
        2 V(II-1)=V(II-1)+V(II)
        MBOT=MBOT-1
        IF(V(II-1).LE.0.0) THEN
            II=II-1
            GOTO 2
        ENDIF
        RETURN
     ENDIF
     V(I+1)=V(I)+V(I+1)
     DZZ=DZ(I)
     KK=I
     MBOT=MBOT-1
ELSE
     II=I
     IF(I.EQ.MBOT) II=I-1
     KK=II+1
     VC=V(II)+V(KK)
     VCOMB=1./VC
     T2(II)=(T2(II)*V(II)+T2(KK)*V(KK))*VCOMB
     C2(II)=(C2(II)*V(II)+C2(KK)*V(KK))*VCOMB
     CD2(II)=(CD2(II)*V(II)+CD2(KK)*V(KK))*VCOMB
     DO 55 K=1,NCLASS
     55 CHLA2(K,II)=(CHLA2(K,II)*V(II)+CHLA2(K,KK)*V(KK))*VCOMB
     IF(MODEL.EQ.3) THEN
        DO 57 K=1,NCLASS
        57 PC2(K,II)=(PC2(K,II)*V(II)+PC2(K,KK)*V(KK))*VCOMB
     ENDIF
     PA2(II)=(PA2(II)*V(II)+PA2(KK)*V(KK))*VCOMB
     BOD2(II)=(BOD2(II)*V(II)+BOD2(KK)*V(KK))*VCOMB
     DSO2(II)=(DSO2(II)*V(II)+DSO2(KK)*V(KK))*VCOMB
     IF(NITRO.EQ.1) THEN
        XNH2(II)=(XNH2(II)*V(II)+XNH2(KK)*V(KK))*VCOMB
        XNO2(II)=(XNO2(II)*V(II)+XNO2(KK)*V(KK))*VCOMB
        DO 56 K=1,NCLASS
        56 XNC2(K,II)=(XNC2(K,II)*V(II)+XNC2(K,KK)*V(KK))*VCOMB
     ENDIF
     V(II)=VC
     DZ(II)=DZ(II)+DZ(KK)
     Z(II)=Z(II)+DZ(KK)*0.5
     DZZ=0.0
     MBOT=MBOT-1
     IF(LW.GT.I) LW=LW-1
     IF(I.EQ.MBOT) GO TO 3
END IF
DO 100 K=KK,MBOT
    T2(K)=T2(K+1)
    C2(K)=C2(K+1)
    CD2(K)=CD2(K+1)
    DO 150 KI=1,NCLASS
    150  CHLA2(KI,K)=CHLA2(KI,K+1)
    PA2(K)=PA2(K+1)
    BOD2(K)=BOD2(K+1)
    DSO2(K)=DSO2(K+1)
    IF(MODEL.EQ.3) THEN
        SI2(K)=SI2(K+1)
        DO 151 KI=1,3
        151 PC2(KI,K)=PC2(KI,K+1)
        IF(NITRO.EQ.1) THEN
            DO 152 KI=1,3
            152 XNC2(KI,K)=XNC2(KI,K+1)
            XNH2(K)=XNH2(K+1)
            XNO2(K)=XNO2(K+1)
        ENDIF
    ENDIF
    V(K)=V(K+1)
    DZ(K)=DZ(K+1)
    Z(K)=Z(K+1)-DZZ
100  CONTINUE
3   ZMAX= Z(MBOT) + 0.5*DZ(MBOT)
RETURN
END

          
!C**************************************************C
!    
!C**************************************************C
SUBROUTINE HEBUG(IL,TS,QN,HS,HA,HBR,HE,HC,TAIR,TDEW,CR,RAD,WIND,VC)

!Compute the temperature profile using routines FLXOUT and
!FLXIN for the surface heat exchange.  Solution is by the
!implicit central difference formulation.  CONMIX called to
!check for and resolve density instablities between layers.

REAL*8 A,V,TV,ATOP,AK,BK,CK,DK
INTEGER FMON,FDAY,FYEAR
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
COMMON/SOLV/ AK(120),BK(120),CK(120),DK(120)
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF   
COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
COMMON/TEFX/T2K(120),TEHE(120),BMK(120),OLDHQ,ITERF 
COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)
COMMON/TMAXP/TEMP(366,36),QX(7),DOXN(366,36)
COMMON/NEW5/XDAY
COMMON/FUTURE/FCO2
!   COMMON/HEATFACTOR_CHECK/HEATFACTOR
DIMENSION Q(120),QSED(120)

IF(THICE.LE.0.0) THEN
    !C...OPEN WATER SEASON STUDY
    !C...CALCULATION OF THE HEAT ABSORPTION FROM METEOROLOGICAL
    !C...PARAMETERS IN A COLUMN OF WATER
    !C...CALCULATION OF HEAT FLUXES INTO THE WATER BODY
    !Change C2 to C2(1) - surface suspended solid concentration
    CALL FLXIN(HS,HA,TAIR,RAD,CR,C2(1))
    CALL FLXOUT(TS,HBR,HE,HC,TAIR,TDEW,WIND,WCOEF)
    HQOUT=HBR+HE+HC
ELSE
    !C...SNOW OR ICE COVER PERIOD *****
    !C...CALCULATION OF HEAT ABSORBED IN SNOW AND ICE LAYERS
    IF(THSNOW.GT.0.0) THEN
        IF(SNOWFL(MDAY).GT.0.0) THEN
            ALFSNOW=0.85
            XDAY=0.0
        ELSE
            XDAY=XDAY+1.0
            IF(TAIR.LT.0.0)  THEN
               ALFSNOW=0.85-0.050*XDAY**0.5
              ELSE
               ALFSNOW=0.85-0.075*XDAY**0.5
              ENDIF
            ENDIF
            HSSNOW=(1.0-BTSNOW)*(1.0-ALFSNOW)*RAD*10.0*EXP(-GMSNOW*THSNOW)
          ELSE
            HSSNOW=RAD*10.0
          ENDIF
          
          IF(THICE.GT.0.0) THEN
            IF(THSNOW.GT.0.0) ALFICE=0.0
            IF(THSNOW.LE.0.0.AND.TAIR.GT.0.0) ALFICE=0.40
            IF(THSNOW.LE.0.0.AND.TAIR.LE.0.0) ALFICE=0.55
            HSICE=(1.0-BTICE)*(1.0-ALFICE)*HSSNOW*EXP(-GMICE*THICE)
          ELSE
            HSICE=HSSNOW
          ENDIF
          HS=HSICE

    !C... Exclude unnecessary terms used for the open water season
          HA=0.0
          HE=0.0
          HC=0.0
          HQOUT=0.0

ENDIF

    !C...CALCULATION OF EXTINCTION COEFF. (ETA) AS A FUNCTION OF SUSPENDED
    !C...SEDIMENT CONCENTRATION
       
          ETA=XK1+0.04317*C2(1)+XK2*CHLATOT(1)
 
    !C...CALCULATION OF HEAT ABSORBED IN EACH LAYER
    !C...HQIN(i) is the solar absorption at each horizontal 
    !C...layer in Kcal/day (whole lake as a control volume).
         
          HQ=(1.0-BETA)*HS
          EX=EXP(-ETA*DZ(1))
          Q(1)=((BETA*HS+HA-HQOUT)*ATOP(1)+HQ*(ATOP(1)-EX*ATOP(2)))/(1000.0*V(1))
                                                 
!Output the heat budget components for comparing with hourly model, Jalil May, 9, 2018.
!    Write(183,321) Myear,Month,Mday,HS,HA,HBR,HE,HC,HQ+HA,HQOUT,TS
!321 Format(I4,2x,I2,2x,I2,2x,F8.2,2x,F8.2,2x,F8.2,2x,F8.2,2x,F8.2,2x,F9.2,2x,F9.2,2x,F5.2)    

    !C...CONVERSION FACTOR OF 1000 USED FOR DENSITY*HEAT CAPACITY OF WATER
          HQ=HQ*EX

    !C...CALCULATE THE SOURCE TERM Q FOR EACH LAYER
   
          DO 10 I=2,MBOT
            ETA=XK1+0.04317*C2(I)+XK2*CHLATOT(I)
          
            
            EX=EXP(-ETA*DZ(I))
            Q(I)=HQ*(ATOP(I)-ATOP(I+1)*EX)/(1000.0*V(I))
            HQ=HQ*EX
      10  CONTINUE
         
           DO 16 II=1,MBOT
             CALL SEDIMENT(QSED(II),T2(II),II,MYEAR)

             QFLUX=QSED(II)*(ATOP(II)-ATOP(II+1))/(V(II)*1000.0)

             IF(II.EQ.MBOT) HTBTM=QSED(II)
        !     QFLUX=QFLUX*HEATFACTOR
             Q(II)=Q(II)+QFLUX
          
16         CONTINUE 

    !...Output the sediment heat flux
!        Write(485,385) Myear,Month,Mday,(QSED(I),I=1,MBOT)
!385     Format(I4,2x,I2,2x,I2,2x,120F10.5)  
        
        
    !C...CALCULATE DIFFUSION COEFFICIENT... IMPORTANT!!
          ICV=1
          CALL SETAMK(WIND,VC,IL,MBOT,ICV)

    !C...SET-UP COEFFICIENTS FOR TRI-DIAGONAL MATRIX
          DO 100 I=2,MBOT-1
            D1= 2.0/(A(I)*DZ(I))
            D2=D1*ATOP(I)*HMK(I)/(DZ(I)+DZ(I-1))
            D3= D1*ATOP(I+1)*HMK(I+1)/(DZ(I)+DZ(I+1))
            AK(I)=-D2
            BK(I)=1.0+D2+D3
            CK(I)=-D3
            DK(I)=T2(I)+Q(I)
     100  CONTINUE
     
    !CFX...FORCING WATER TEMPERATURE T(1)=0.0 AS THICE>0.0
    !CFX...SET THE FIRST CONTROL VOLUME IS ZERO DEPTH OR VOLUME!
          AK(1)=0.0
          CK(1)=-2.0*ATOP(2)*HMK(2)/(A(1)*DZ(1)*(DZ(1)+DZ(2)))

          IF(THICE.GT.0.0) THEN
            BK(1)=1.0-CK(1)
            BK(1)=BK(1)+2.0*ATOP(1)*HMK(1)/(A(1)*DZ(1)*DZ(1))
          ELSE      
            BK(1)=1.0-CK(1)
          ENDIF
          DK(1)=T2(1)+Q(1)
          
            
    !CFX...AT LAKE BOTTOM - CONTROL VOLUM -- MBOT
          I=MBOT
          AK(MBOT)=-2.0*ATOP(I)*HMK(I)/(A(I)*DZ(I)*(DZ(I)+DZ(I-1)))
    !    IF(THICE.EQ.0.0) THEN
    !C*  Post adiabtic conditions at the bottom
           CK(I)=0.0
    !    ELSE
    !     CK(MBOT)=-2.0*ATOP(I)*HMK(I+1)/(0.5*A(I)*DZ(I)*DZ(I))
    !    ENDIF
          BK(MBOT)=1.0-AK(I)-CK(I)
          DK(MBOT)=T2(I)+Q(I)  

          CALL SOLVE(T2,MBOT)

          
    !C...  INCLUDE SEDIMENT HEAT FLUX
    !C...  Densiity - kg/m*3, Heat capacity - kcal/kg-C
    !C...  SRCP = R*Cp = kcal/kg-c; FOR WATER SRCP=1000*1.0 KCAL/KG-C
    !C...  HMK(I),AHTBTM ARE IN M**2/DAY HERE!
    !CFX.. AT WATER - SEDIMENT INTERFACE --- BOUNDARY CONDITION
    !    I=MBOT+1
    !    CONDW=HMK(I)*1000.0*1.0
    !    CONDS=AHTBTM*SRCP
    !    AK(I)=-2.0*CONDW/DZ(I-1)
    !    BK(I)=2.0*(CONDW/DZ(I-1)+CONDS/DZSL)
    !    CK(I)=-2.0*CONDS/DZSL
    !    DK(I)=0.0 
    !C* We had difficult to estimate heat flux by temperature gradients.

    !Constant DZSL=DZ im meters, DT=1.0 day, AHTBTM=Kb thermal
    !diffusivity of sediment in m*m/day !  
    !Stability criteria: PRMM >= 2.0
    !    PCOE=(AHTBTM*1.0)/DZSL**2.0
    !    M=MBOT+1
    !    DO 150 I=M+1,M+11
    !      AK(I)=-PCOE
   !CX     BK(I)=1.0
    !      BK(I)=1.0+2.0*PCOE
    !      CK(I)=-PCOE
   !CX      DK(I)=(1.0-2.0*PCOE)*TSLP(I-M)
    !      DK(I)=TSLP(I-M)
    !C150  CONTINUE
     
    !CFX... AT THE FIRST SEDIMENT LAYER
    !    I=M+1
    !    AK(I)=AK(I)-PCOE
  !CX    DK(I)=DK(I)-PCOE*TSLP(I-M)
    !    BK(I)=BK(I)+PCOE
          
    !CFX... AT VERY FAR SEDIMENT BOTTOM
    !    I=M+11
    !    CK(I)=0.0
   !CX    DK(I)=DK(I)+PCOE*TSLP(I-M)
    !    BK(I)=BK(I)-PCOE

    !    DO 165 I=1,MBOT
    !      T2P(I)=T2(I)
    !C165  CONTINUE

    !    CALL SOLVET2(TE,M+11)

    !    DO 160 I=1,MBOT+1
    !      T2(I)=TE(I)
    !C160  CONTINUE

    !    DO 170 I=MBOT+2,MBOT+12
    !      TSL(I-M)=TE(I)
   !C170  CONTINUE

    !Determine heat flux from sediment to water at lake bottom Zmax
    !TSL(I) is sediment temperature, TSLP(1) is sediment
    !temperature at previous time step (T-1).
    !Heat flux at sediment-water interface is the summation
    !of (1.0m**2)*DZSL*DENse*Cpse*(TSLP(I)-TSL(I))
    !DENse is the sediment density = 2500.0 kg/m**3
    !Cpse is the sediment heat capacity = 1.0*0.2388 kcal/kg/C.
    !SRCP = DENse*Cpse = Kcal/m**3/!= 400 - 1000
    !That will get more accurate results (MIKI AND STEFAN, 1994)?
    !Bashar (1993) used Q=ks*dTs/dt, it is not so bad ? 

    !    HTBTM=0.0
    !    DO 1628 I=1,IZSLT
    !      HTBTM=HTBTM+SRCP*DZSL*(TSLP(I)-TSL(I))
    !      TSLP(I)=TSL(I)
    !  C1628 CONTINUE     

    !     IF(MYE.EQ.1958) THEN
    !       IF(MONTH.EQ.1.AND.MDAY.EQ.1) THEN
    !         KDY=1
    !       ELSE
    !         KDY=KDY+1
    !       ENDIF

    !C...SQD is sediment heat flux for bottom layer MBOT
    !        SQD=HTBTM*ATOP(MBOT)
    !        SQSED=SQSED+SQD
    !        QRA3=SQD/SQSED

    !        IF(SQD.GT.0.0) THEN
    !         SQ1=SQ1+SQD
    !        ELSE
    !         SQ2=SQ2+SQD
    !        ENDIF
    !C...To get volume averages of heat fluxes
    !        SQ1=SQ1/TV(MBOT)
    !        SQ2=SQ2/TV(MBOT)
    !        SQSED=SQSED/TV(MBOT)
    !        SQABS=SQABS/TV(MBOT)
    !        BANC=SQABS+SQSED
    !        QRA1=SQSED/SQABS
    !        QRA2=SQSED/BANC
    !        QXX(MBOT)=SQD/TV(MBOT)

    != 1.0   QRA3=SQABS/QXX(1) is very good !
    !SQSED, SQABS is in kcal/m**3/day
    !SQSED is total sediment heat flux, negative from water
    !to sediments, and positive from sediment to water.
    !SQABS is available solar energy in the lake in Kcal/m**3/day
    !SQABS is positive for solar heating, and negative for surface
    !cooling.   BAN!is net thermal energy within the water column
    !in kcal/m**3/day

    !      IXX=0
    !      IF(THICE.GT.0.0) IXX=10
    !      WRITE(12,977) KDY,SQSED,SQABS,
    !   +    BANC,QRA1,QRA2,QRA3,IXX,SQ1,SQ2
    !c977    FORMAT(I3,2X,3F12.3,2X,3F7.2,I4,2F9.3)

    !      WRITE(12,977) KDY,IXX,(QXX(K),K=1,MBOT)
    !c977    FORMAT(I3,2X,I3,2X,40F7.2)

    !     ENDIF

    !    ENDIF
                  
          TS =T2(1)
          QN=HS+HA-HQOUT
        
          RETURN
          END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE FLXIN(HSN,HAN,TC,RAD,CC,C2X)
    !Clj** computes total radiation flux at the water surface
                    
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
   

    !C...CALCULATION OF THE TOTAL RADIATION FLUX INTO A BODY OF WATER IN
    !C...FROM NET SOLAR RADIATION (HSR) AND NET ATMOSPHERIC RADIATION (HSN)
    !C...IN KCAL/M*M
    !C...IDSO JACKSON FORMULA USED FOR ATM. RADIATION
    !C...CONVERT AIR TEMPERATURE IN DEGREE TO DEGREE ABSOLUTE
          TCA=TC+273.
          HAN=1.171E-6*(1.-0.261*EXP(-7.77E-4*TC*TC))*(TCA**4)*(1.+0.17*CC*CC)
    
    !C...CALCULATION OF NET SOLAR RADIATION AND CONVERSION TO KCAL/M*M/DAY
    !C...CALCULATION OF REFLECTED SOLAR RADIATION HSR
    !C...HSRW---- DUE TO CLEAR WATER USING KOBERGS CURVES
    !C...HSRS---- DUE TO SUSPENDED SEDIMENTS AT THE WATER SURFACE
    
    !C2X is surface suspeded solid concentration
    
          HSR=(0.087-6.76E-5*RAD+0.11*(1.-EXP(-0.01*C2X)))*RAD

          IF(THICE.GT.0.0) HSR=0.0
          HSN=(RAD-HSR)*10.0
          RETURN
          END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE FLXOUT (TT,HBR,HE,HC,TAIR,TD,WIND,WCOEF)
  !Clj** Computes evaporative, convective, and back radiation fluxes
  !Clj** at the water surface     
          REAL*8 A,V,TV,ATOP
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
                 
          DALPHA=WCOEF
    !C...CALCULATES THE ENERGY FLUX OUT OF A BODY OF WATER FROM 
    !C...EVAPORATIVE HEAT LOSS (HE), CONVECTIVE HEAT LOSS (HC), AND
    !C...BACK RADIATION (HBR) IN KCAL/M*M/DAY.

    !C...ESTIMATE WIND FUCTION FROM ADAMS 1989

    !VAPOR AND ATM PRESSURE ESTIMATION (Pa)
          ESA=611.0*EXP((17.27*TT)/(237.3+TT))
          EA=611.0*EXP((17.27*TD)/(237.3+TD))

    !CONVERT Pa to milibars
          ESA=ESA*0.01
          EA=EA*0.01

    !CONVERT WIND FROM MI/H TO M/SEC
          WINDHF=WIND*0.44704

    !ESIMATE VIRTUAL TEMPERATURES
    !C...PTOT in mb
          PTOT=981.74
          TVSRF=((TT+273.15)/(1-0.378*(ESA/PTOT)))
          TVAIR=((TAIR+273.15)/(1-0.378*(EA/PTOT)))
          DTV=TVSRF-TVAIR
          IF (DTV.LT.0.) DTV=0.
    !C...CONVERSION OF TEMP. VALUES FROM DEG. C TO DEG. ABSOLUTE
          TSK=TT+273.15
          TAK=TAIR+273.15
    !C...Evaporation equation (Adams,1989)
    !C...He in W/m^2
    !C...0.6368 converts W(10 m) to W(2 m).
          FCN1=2.7*DTV**(1./3.)
          FCN2=3.1*0.6368*WINDHF
          HE=DALPHA*(FCN1+FCN2)*(ESA-EA)
    !C...Convert from W/m^2 to kcal/m^2*day
          HE=HE*20.63

    !C...CALCULATE CONDUCTIVE LOSS USING BOWENS RATIO
    !C...H!in W/m^2
          HC=0.61*(PTOT/1000.)*(TT-TAIR)*(FCN1+FCN2)*DALPHA
    !C...Convert from W/m^2 to kcal/m^2*day
          HC=HC*20.63

    !C...CALCULATES BACK RADIATION
          HBR=(1.171E-6*0.97*TSK**4)

          RETURN
          END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE SETAMK(W,VC,ILAY,MBOT,ICV)
    !C*** 
    !C*** Compute vertical diffusion coefficient in each layer.
    !C*** Diffusion coefficient between layers as the harmonic
    !C*** mean of the diffusion coefficients in adjacent layers.
    !C***
          REAL*8 A,V,TV,ATOP,RHO
          DIMENSION AMK(121)
          COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)
          COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
          COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
          COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
         
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
          COMMON/NEW3/IP
 
          DIMENSION PSQN(120)
        
          TRASH=VC
   !C
          
         
          IF(THICE.GT.0.0) GOTO 4144   
   !C
    !C...Vertical diffusion coefficient in the mixed layer       
    !C...computed as a function of the wind speed (mph)
    !     HKC=HKMAX*8.66E-3
    !c--MINLAKE MODEL (RILEY)       
    !     DKM=28.0*W**1.3
    !     HKMAX=28.0*W**1.3
    !c--FROM WALTER
    !      DKM=1.74*W
    !     IF(DKM.GT.HKMAX) DKM=HKMAX
    !CMIKI   DEN2=RHO(T2(J),C2(J),CD2(J))
    !      RI=9.81*(RHO(T2(J+1),C2(J+1),CD2(J+1))-RHO(T2(J),C2(J),CD2(J)))
    !    + *(Z(ILAY)+DZ(ILAY)/2.0)/((V!C**2.)*DEN2)
    !      RI=ABS(RI)
    !      IF(RI.EQ.0.0) THEN 
    !      DKM=HKMAX
    !      GOTO 16
    !      ENDIF
    !      C=ZMAX/34.0
    !      BETA=34*0.1/ZMAX
    !      DKM=VC/((1/C)+(BETA*RI))*86400.0
    !16   CONTINUE     

    !CFX --TEST DIFFERENT Kz (CONSTANT THROUGHOUT SEASON) IN THE
    !EPILIMNION -- TEST TEMP. DIFFERENCE IN EPIL. (FEB,1993)
    !     JULIAN=MONTH*100+MDAY      
    !     IF(JULIAN.EQ.401.AND.IREAD.GE.0) THEN
    !       IDAY=90
    !       WRITE(*,*) 'PLEASE INPUT KZ EPILIMNION ='
    !       READ(*,*) EPKZ
    !       DKM=EPKZ
    !       IREAD=-666       
    !     ELSE
    !       EKM=EPKZ
    !       IDAY=IDAY+1
    !     ENDIF

    !C...HMK TEMPORARILY USED TO STORE DENSITIES.
    !C...Diffusion coefficient below the mixed layer computed as
    !C...a function of the square of the Brunt-Vasala frequency (SQN)
    !C...SQN= (G/RHO) * d(RHO)/dZ
         
      !   IF(ATOP(1).GT.10000000.0)THEN
      !    AREA=EMCOE(1)*10000000.0/(10**6.0)
      !   ELSE
    
      ! AREA in km sqaure
          AREA=ATOP(1)/(10**6.0)
      !    ENDIF         
          ALFA=8.17*0.0001*(AREA)**0.56
      !   IF(AREA.GT.10.0)ALFA=8.17*0.0001*(10.0)**0.56
    
      ! HKMAX in m**2/day after multiplying 8.64
          HKMAX=ALFA/(0.000075**0.43)*8.64

    !C... FROM WALTERS et al. (1978) - W in mph
    !C... DISSOLVED OXYGEN MODEL !
          IF(ICV.EQ.6) THEN
            HKMAX=1.74*W
          ENDIF
              
     ! During the ice cover period, the program goes to 4144, so these are not effective code 
     !     IF(THICE.GT.0.0) THEN
     !       HKMAX=0.0125
     !       ALFA=ALFA/2.0 
     !       ILAY=1
     !     ENDIF

     ! 2013 Lake Kivu - ADD ON MAY 7, 2013 - W is mph (mile/hr)
     ! Test diffusion coefficient - The formular from Ford and Stefan - earliest temperature model
     !    HKSURFACE=28.0*W**1.3

         DO 100 I=1,ILAY
     !      AMK(I)=HKSURFACE
           AMK(I)=HKMAX
     100  CONTINUE

!Fang, 2009 HMK(I) in here is the density of water
          DO 200 I=ILAY,MBOT
     200  HMK(I)=RHO(T2(I),C2(I),CD2(I))

          DO 300 I=ILAY+1,MBOT-1
          AVRHO=(HMK(I-1)+HMK(I+1))*0.5
          SQN=ABS(HMK(I-1)-HMK(I+1))/((Z(I+1)-Z(I-1))*AVRHO)*9.81
        
          PSQN(I)=SQN
           
 !Fang, 2009 Testing diffusion calibration factors
         IF(ICV.EQ.1) THEN
          IF(SQN.LT.0.000075) THEN
           SQN=0.000075
           AMK(I)=(EMCOE(4)*ALFA/((SQN)**0.43))*8.64
          ELSE
           AMK(I)=(EMCOE(1)*ALFA/((SQN)**0.43))*8.64
          ENDIF
         
         ELSE  
          IF(SQN.LT.0.000075) THEN
           SQN=0.000075
          ENDIF
           AMK(I)=(ALFA/((SQN)**0.43))*8.64
  
         ENDIF    
         
          IF(MYEAR.EQ.KYEAR(NYEAR).AND.(MDAY+MONTH*100).EQ.NPRNT(NDAYS)) THEN
          IF(I.EQ.ILAY+1)THEN
           IF(I.EQ.ILAY+1.AND.ICV.EQ.1) WRITE(2000,*)'_________________________________________________________________'
          WRITE(2000,*)'  T/DO Year Month Day     JDAY  '  
          WRITE(2000,20001) ICV,MYEAR,MONTH,MDAY,JULIANDAY
   20001 FORMAT(6X,I1,1X,I4,2X,I2,2X,I2,7X,I3)        
          WRITE(2000,*)' I     Z(I)  T2(I)     Density   N^2     AMK'   
          ENDIF
          WRITE(2000,20002)I,Z(I),T2(I),HMK(I),SQN,AMK(I)
   20002  FORMAT(1X,I3,1X,F6.2,1X,F6.2,1X,F8.3,1X,F9.6,1X,F9.6)      
          ENDIF
     
     
     300  CONTINUE     

    !C--- ASSUME AMK(MBOT-1)=MAX AMK FOR SQN=0.000075
          AMK(MBOT)=AMK(MBOT-1)
          
          
           DO 400 I=2,MBOT
           
           HMK(I)=2.0*AMK(I)*AMK(I-1)/(AMK(I)+AMK(I-1))
           
           IF(HMK(I).LE.0.0125) HMK(I)=0.0125
    
          IF(MYEAR.EQ.KYEAR(NYEAR).AND.(MDAY+MONTH*100).EQ.NPRNT(NDAYS)) THEN
                      IF(I.EQ.2)THEN
           
                              WRITE(2000,*)'I  HMK  '  
                              
                       ENDIF
                       
                               WRITE(2000,20004)I,HMK(I)
                         20004 FORMAT(I3,1X,F7.3)      
          ENDIF
     
    
     400   CONTINUE

           HMK(1)=0.0125
    !      HMK(MBOT+1)=0.0125
           HMK(MBOT+1)=HMK(MBOT)
!C
     4144 CONTINUE
  
          IF (THICE.GT.0.0) THEN
!C
          IP=IP+1
          DO 202 I=1,MBOT
          HMK(I)=RHO(T2(I),C2(I),CD2(I))
     202  CONTINUE
 !C
          DO 302 I=2,MBOT-1
          AVRHO=(HMK(I-1)+HMK(I+1))*0.5
          SQN=ABS(HMK(I-1)-HMK(I+1))/((Z(I+1)-Z(I-1))*AVRHO)*9.81
          IF (SQN.LT.0.000075) SQN=0.000075
          PSQN(I)=SQN
     302  CONTINUE     
 !C
          PSQN(MBOT)=PSQN(MBOT-1)
 !C
    !BELOW THE ICE
    !      
          DAL=1.04*(0.00000001)
          DBETA=0.43
          DO 4149 I=2,MBOT
           AMK(I)=DAL*PSQN(I)**(-DBETA)
     
    !CONVERT M2/SEC IN M2/DAY
           
           AMK(I)=AMK(I)*86400
     
     4149 CONTINUE
   !C
   !ABOVE THE SEDIMENT SURFACE
   !     
          DQFLUX=ABS(HTBTM)/(10.0*86400.0)
          DO 4142 I=MBOT+1,2,-1
           IF(I.EQ.MBOT+1) THEN
            DEP=0.0
           ELSE
            DEP=(ZMAX-Z(I))*100.0
           ENDIF
           AMK(I)=(DQFLUX/(0.623*1.0))*(DEP+65)
           AMK(I)=AMK(I)*8.64
          IF(DEP.GE.300) GOTO 4146
     
          IF(AMK(I).GE.AMK(I-1)) THEN
           AMK(I)=AMK(I-1) 
           GOTO 4146
          ENDIF
     
     4142 CONTINUE
     4146 CONTINUE
   !C
   !ESTIMATE HARMONIC MEAN
   !C
          HMK(1)=0.0125
          AMK(1)=0.0125
          DO 4150 I=2,MBOT+1
          HMK(I)=0.0125+2.0*AMK(I)*AMK(I-1)/(AMK(I)+AMK(I-1))
     4150 CONTINUE
    !     
          ENDIF
     
          RETURN
          END
    !C**********************************************************C
    !                                                           !
    !                                                           C
    !C**********************************************************C
          SUBROUTINE DISOLID(VAR2,YWIND,RAD,TD,ST)
    !C***
    !C*** Compute the concentration of dissolved oxygen
    !C*** by solving the one dimensional diffusion equation
    !C*** by the fully implicit central difference scheme.
    !C***
          REAL*8 FAO,A,V,TV,ATOP,AK,BK,CK,DK,SVOL
          INTEGER FMON,FDAY,FYEAR
          DIMENSION VAR2(120)
    !CFX...For surface gas transfer study !
          DIMENSION FDO(4),FGAM(4),RIN(120),SOU(8,120),SCOE(2,120)
          COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)
          COMMON/FIELD/IFLAG(10),FLDATA(10,120),DEPTH(120),NFLD(10),SD,NSKIPDAY,NSDAY(640),SDFIELD(640),TAPE64
          COMMON/SOLV/AK(120),BK(120),CK(120),DK(120)
          COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
          COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/SOURCE/RM(3,120),PROD(120),XMR(3,120),PRODSUM(120)
          COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
          COMMON/PHYTO0/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),GROMAX(3),TMAX(3)
          COMMON/PHYTO1/TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN(90),CHLMAX,SDY(90)
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
          COMMON/COEFF/DUM2(120),DUM3(120)
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          COMMON/FXDEBUGE/IBUG1
      
          COMMON/SUB/SDZ(180),SZ(180),LAY(120),AVGI(24,180),SVOL(180)
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
          COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
          COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
          DO 100 I=2,MBOT-1
            AK(I)=-DUM2(I)
            BK(I)=1.0+DUM2(I)+DUM3(I)
            CK(I)=-DUM3(I)
            DK(I)=VAR2(I)
     100  CONTINUE
          DK(1)=VAR2(1)
          AK(1)=0.0
          CK(1)=-2.0*ATOP(2)*HMK(2)/(A(1)*DZ(1)*(DZ(1)+DZ(2)))
          BK(1)=1.0-CK(1)
          I=MBOT
          AK(I)=-2.0*ATOP(I)*HMK(I)/(A(I)*DZ(I)*(DZ(I)+DZ(I-1)))
          BK(I)=1.0-AK(I)
          CK(I)=0.0
          DK(I)=VAR2(I)

    !C*** Specify chlorophyll-a concentration in Main Program
    !C*** Before calling DISOLID subroutine !!

    !C*** Compute light distribution over a day in TD subsections.
           CALL LIGHT(RAD,TD,C2(1),ALBEDO)

    !C*** DETERMINE THE PHOTI!DEPTH ***************
           CALL EUPHZ(ZEUPH,IEUPH,ALBEDO)

    !C**  Divide each layer to several sublayer with 0.2 m thick
           CALL SUBLAY(IEUPH,NSLAY)         

    !C*** Compute average light in each sublayer
           CALL AVGLITE(IEUPH,TD)

    !C**  Determine light limitation and photosynthetic oxygen production.
           CALL PRODAVG(IEUPH,MBOT,1,TD)

    !C**  Specify the oxygen consumption term !
          DO 107 I=1,MBOT

    !C...Respiration and BOD in winter
           IF(THICE.GT.0.0) THEN
             RESP=0.00        
             BODK2=0.03

    !C...SOD in winter          
    !C...IN OLIGOTROPHIC LAKES
             IF(SD.GT.4.0) THEN
               SB20N=0.08
               IF(Z(I).GT.ZEUPH) SB20N=0.16

    !C...IN MESOTROPHIC LAKES
             ELSEIF(SD.GT.1.8) THEN
               SB20N=0.16
               IF(Z(I).GT.ZEUPH) SB20N=0.23

    !C...IN EUTROPHIC LAKES
             ELSE
               SB20N=0.23
               IF(Z(I).GT.ZEUPH) SB20N=0.30
             ENDIF 
           ELSE

    !C..FOR OPEN WATER SEASON SIMULATIONS          
    !BOD - Biochemical oxygen demand -- BODK20 = 0.1/day (fixed most of time)
              BODK2=BODK20*1.047**T20(I)
                
    !RESPIRATION - Plant respiration -- XKR1(1) = 0.1/day (fixed most of time)        
              RESP=XKR1(1)*1.047**T20(I)*CHLATOT(I)

    !C...SOD - Sediment oxygen demand
              SB20N=SB20*1.065**T20(I)

    !C...May 8, change for Carlos - Metalimnetic DO minimum
    !          ZMETAMIN=Z(ILAY)+4.0
              IF(Z(I).GT.Z(ILAY)) THEN
    !            IF(Z(I).LE.ZMETMIN) THEN
    !             SB20N=8.5*EMCOE(2)*SB20*1.065**T20(I)
    !            ELSE
                 SB20N=EMCOE(2)*SB20*1.065**T20(I)
    !            ENDIF
              ENDIF
              
    !C...Adjustment for low D.O. levels
    !C...Xing Fang, May 7, 2013, Edwards and Owens (1965) formula
              IF(DSO2(I).LT.1.0) THEN
                SB20N=SB20N*DSO2(I)**0.45
                BODK2=BODK2*DSO2(I)**0.45
                RESP=RESP*DSO2(I)**0.45
             ENDIF
             
    !         IF(DSO2(I).LT.0.2) THEN
    !            SB20N=0.0
    !            BODK2=0.0
    !            RESP=0.0
    !         ELSEIF(DSO2(I).LT.1.0) THEN
    !            SB20N=SB20N/2.0
    !            BODK2=BODK2/2.0
    !            RESP=RESP/2.0
    !         ENDIF
                 
           ENDIF
              
            IF(Z(I).GT.ZEUPH) THEN
    !        IF(Z(I).GT.Z(ILAY)) THEN
    !          RESP=0.0
    !        ENDIF
              RESP=0.0
              PRODSUM(I)=0.0
            ENDIF
               
            DK(I)=DK(I)-BODK2*BOD2(I)-RESP/Y2CHO2+PRODSUM(I)-SB20N*(ATOP(I)-ATOP(I+1))/V(I)
       
    !FOR OUTPUT RESULTS ONLY
    !      RIN(I)=-BODK2*BOD2(I)-RESP/Y2CHO2+PRODSUM(I)
    !   +                -SB20N*(ATOP(I)-ATOP(I+1))/V(I)
    
    ! Source/sink 1 - BOD, 2 - respiration, 3 - SOD, 4 - DO production
          SOU(1,I)=BODK2*BOD2(I)
          SCOE(1,I)=BODK2
          
          SOU(2,I)=RESP/Y2CHO2
        
          SOU(3,I)=SB20N*(ATOP(I)-ATOP(I+1))/V(I)
          SCOE(2,I)=SB20N
        
          SOU(4,I)=PRODSUM(I)
             
     107  CONTINUE
         
    !C**** For winter simuations with snow/ice cover
          IF(THICE.GT.0.001) THEN
            DOK0=0.0
          ELSE 

    !C***** Correct the wind speed by fetch *************
    !C***** Find wind speed in the center of lake in m/s
            W=YWIND*0.447
            FAO=0.0
            CALL LAKE(FTCH,FAO,0,2)
            ZB=0.8*ALOG(FTCH/2.0)-1.0718
            XWIND=W*1.666667*(ZB+4.6052)/(ZB+9.2103)
             
    !C*METHOD-1*******COMPUTE GAS TRANSFER VELOCITY (M/DAY)********
    !C***************VERIFY ON JUNE 24, 1992********************         
           DOK0=(0.02256*(0.10656*EXP(-0.0627*T2(1))+0.00495)**(-0.5)*XWIND**1.64)/DZ(1)
    
    !C*METHOD-3******* COMPUTE GAS TRANSFER VELOCITY (M/DAY)******
    !C*** OLD FORMULA FROM MINLAKE MODEL (PAGE 42)  ************* 
    !C*** WITHOUT CORRECTION FROM FETCH FUNCTION *****************
    !       DOK0=(0.641+0.128*(0.447*XWIND)**2)/DZ(1)
    !              
    !C*METHOD-2*******COMPUTE GAS TRANSFER VELOCITY (M/DAY)********
    !C*FROM THOMANN IN PAGE 282 --- BANKS (1975) ******************
    !      DOK0=(0.728*XWIND**0.5-0.317*XWIND+0.0372*XWIND**2)/DZ(1)
   
    !CFX* TEST DIFFERENT GAS TRANSFER VELOCITY K=CONSTANT IN THE
    !C* EPILIMNION -- TEST TEMP. DIFFERENCE IN EPIL. (FEB,1993)
    !     IF(JDY.EQ.JULIANDAY.AND.IREAD.GE.0) THEN
    !       WRITE(*,444)
    !444    FORMAT(/2X,'INPUT GAS RANSFER VELOCITY Ke = ',\)
    !       READ(*,*) DOKE
    !       DOK0=DOKE/DZ(1)
    !       IREAD=-666       
    !     ELSE
    !       DOK0=DOKE/DZ(1)
    !     ENDIF

          ENDIF

    !C**********COMPUTE THE SATURATED OXYGEN FROM MINLAKE (PAGE 42)*******
    !     DOXS=14.652-0.41022*T2(1)+7.99E-3*T2(1)**2-7.7774E-5*T2(1)**3

    !C**********COMPUTE THE SATURATED OXYGEN FROM THOMANN***************
          T2K=T2(1)+273.15
          DOXS=-139.3441+1.575701E5/T2K-6.642308E7/T2K**2.0+1.2438E10/T2K**3.0-8.621949E11/T2K**4.0
  
    !C***Correct saturation concentration with elevation ***************
    !C***ST is the stage of the lake in meters above sea level *********
          DOXS=EXP(DOXS)*(1-0.000035*ST*3.2808)
          
          DK(1)=DK(1)+DOK0*DOXS
          BK(1)=BK(1)+DOK0
      
    !C***CALL SOLVE SUBROUTINE****************
    !CFX---GAS TRANSFER
    !     DCI=VAR2(1) 
         IF(IBUG1.GT.0) THEN
          WRITE(440,*)'Year Month Day       Csat   Cs     Fs     Ke/Z1  Temp  T20' 
          WRITE(440,4441) MYEAR,MONTH,MDAY,DOXS,DSO2(1),DOK0*(DOXS-DSO2(1)),DOK0
      
           ! WRITE(450,4441)MYEAR,MONTH,MDAY,BOD2(1)
           ! WRITE(460,4441)MYEAR,MONTH,MDAY,Y2CHO2 
           ! WRITE(470,4441)MYEAR,MONTH,MDAY,SB20
           ! WRITE(480,4441)MYEAR,MONTH,MDAY,POMAX
         ENDIF   
      
          CALL SOLVE(VAR2,MBOT)
          
          DO 500 I=1,MBOT
            
            DVAR2=VAR2(I)
            
          IF(VAR2(I).LT.0.) VAR2(I)=0.0
          
          IF(IBUG1.GT.0) THEN
           IF(I.EQ.1) WRITE(440,*)'Layer Depth  BOD Resp  SOD PhtoSyn  DVAR2 DSO2(I)' 
            WRITE(440,4442) I,-Z(I),(SOU(IK,I), IK=1,4),DVAR2,DSO2(I),T2(I),T20(I)
            !WRITE(450,4442)I,-Z(I),SOU(1,I),SCOE(1,I),DSO2(I)   !BOD
            !WRITE(460,4442)I,-Z(I),SOU(2,I),DSO2(I)   !Algal Respiration             
            !WRITE(470,4442)I,-Z(I),SOU(3,I),DSO2(I),T2(I),SCOE(2,I),(ATOP(I)-ATOP(I+1)),V(I),(ATOP(I)-ATOP(I+1))/V(I)            
            !WRITE(480,4442)I,-Z(I),SOU(4,I),DSO2(I)   !Photosynthesis
          ENDIF
               
     500  CONTINUE
    
     4441 FORMAT(I4,3X,I2,3X,I3,4X,2(F8.4,1X),9(F9.4,1X))
      
     4442 FORMAT(I3,1X,F7.3,1X,4(F9.3,1X),2(F12.1,1X),F6.3)
          
        IF(IBUG1.GT.0) THEN
          write(450,721)Myear,Month,MDay,(Sou(1,I), I=1,MBOT)
          write(460,721)Myear,Month,MDay,(Sou(2,I), I=1,MBOT)
          write(470,721)Myear,Month,MDay,(Sou(3,I), I=1,MBOT)
          write(480,721)Myear,Month,MDay,(Sou(4,I), I=1,MBOT)
          write(490,721)Myear,Month,MDay,DOK0*(DOXS-DSO2(1)),DOK0,DOXS,DSO2(1)
    721   Format(I4,2x,I2,2x,I2,2x,120F10.5)
        ENDIF
        
    !C...CHECK INFORMATION FOR GAS TRANSFER... JUNE, 1993
    !      DT=1.0
    !      DC=DOXS-DCI
    !      ADC=VAR2(1)-DCI
    !      
    !      II=1
    !      FH=DOK0*DZ(1)/HMK(II+1)
    !      SDT=SQRT(HMK(II+1)*DT)
    !      FZX(II)=Z(II)/(2.0*SDT)
    !      FGAM(II)=FH*SDT
    !      IF(DC.EQ.0) THEN
    !       FDO(II)=0.0
    !      ELSE 
    !       FDO(II)=RIN(II)*DT/DC
    !      ENDIF
            
    !C..     WRITE(28,84) FZX(1),FGAM(1),FDO(1),DC,ADC
    !C84     FORMAT(3X,5(F12.6,2X))
    !C..     WRITE(99,85) JDY,DOXS,VAR2(1)
    !C85     FORMAT(3X,I3,3X,F8.3,3X,F8.3)        
    !C..     WRITE(99,85)  JDY,T2(1)
    !C85     FORMAT(3X,I3,3X,F8.3)       
            
    !       IF(KDY.EQ.NPRNT(NDAYS)) THEN
    !        WRITE(99,94) KDY,ILAY
    !94     FORMAT(2X/2X,I4,3X,I3/)         
    !        WRITE(99,95) (Z(I),PRODSUM(I),I=1,IEUPH) 
    !95     FORMAT(2X,F6.3,2X,F10.5)
    !       ENDIF
                
    !       WRITE(99,95)  JDY,RAD,PRODSUM(1),VAR2(1),DOK0*DZ(1)
    !95     FORMAT(1X,I5,2X,F9.4,2X,E10.3,2X,F8.3,2X,F8.2)

    !C***** CHECK PHOTOINHIBITION AT WATER SURFACE
    !      DO 234 I=1,NSLAY  
    !          IF(I.EQ.1) THEN
    !            PROMAX=PSUB(1)
    !            PZMAX=0.0
    !          ELSE 
    !            IF(PSUB(I).GT.PROMAX) THEN
    !              PROMAX=PSUB(I)
    !              PZMAX=SZ(I)
    !            ENDIF
    !          ENDIF
    !234   CONTINUE             
    !      WRITE(16,335) JDY,PZMAX,ZEUPH
    !335   FORMAT(3X,I4,3X,F7.2,3X,F7.2)        

    !      IF(MOD(JDY,10).EQ.0) THEN  
    !       WRITE(28,224) MONTH,MDAY
    !224    FORMAT(3X//3X,I3,3X,I3//)  
    !       DO 119 I=1,IEUPH         
    !         WRITE(28,333) Z(I),(SOU(K,I),K=1,3)
    !333      FORMAT(3X,F6.2,3X,3(E12.4,3X))
    !119    CONTINUE
    !      ENDIF 

    !         WRITE(28,224) MONTH,MDAY
    !224     FORMAT(3X//3X,I3,3X,I3//)  
    !       DO 115 II=1,NSLAY
    !         WRITE(28,222) SZ(II),PSUB(II)
    !222     FORMAT(3X,F7.2,5X,F12.6)       
    !115   CONTINUE    
    !       ENDIF
    !118   CONTINUE       

    !USE CONMIX IDEAS FOR DO IN MIXED LAYER
    !ILAY IS INDEX OF MIXED LAYER FROM CONMIX
        OVDUM=0.0
        VDUM=0.0
       ! Fang, 2010 ILAY=1 FOR WINTER PERIOD  
       IF(THICE.LE.0.0)THEN             !ADDED IN FEB 2010
       
        DO 66 I=1,ILAY
          OVDUM=OVDUM+VAR2(I)*V(I)
          VDUM=VDUM+V(I)
    66   CONTINUE
        ODOM=OVDUM/VDUM
        DO 68 I=1,ILAY
          VAR2(I)=ODOM
    68   CONTINUE      
        ENDIF
    600  RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE MTHDATA(MONTH,KDAYS,MYEAR,JDY,FCO2)
    !C***
    !C*** Read monthly meteorological data 
    !C***
          REAL SRO,CR,SINAL,H,D,A0,SRO1,SROWM,CLOUD
          REAL RADLONGLEY

          
          COMMON/FILEX/DIN,MET,FLO,TAPE8,TAPE1,IREC
          COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),PR(31),DRCT(31)
          COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
          COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(81,36),ELEV(81,36)
          COMMON/FWEA/FTAR(12),FDEW(12),FWIN(12),FTCL(1200),FPRE(12),FSOL(12)
          COMMON/GCM1/FUTR_PRECIP_CHANGE(12),FUTR_HUM_CHANGE(12),FUTR_TEMP_CHANGE(12),FUTR_SOLAR_CHANGE(12),FUTR_NWIND_CHANGE(12),FUTR_EWIND_CHANGE(12),FUTR_WIND_CHANGE(12)
          COMMON/GCM2/GRID_PRECIP(400,400,12),GRID_HUM(400,400,12),GRID_TEMP(400,400,12),GRID_SOLAR(400,400,12),GRID_NWIND(400,400,12),GRID_EWIND(400,400,12),GRID_WIND(400,400,12)
          COMMON/GCM3/GRID_LATITUDE(400),GRID_LONGITUDE(400)
          COMMON/MODEL/MODELS_SCENARIO
          DIMENSION RADLONGLEY(400,1200),CLOUD(400,1200)
         
          DIMENSION SUNSHINE(31),CCTEN(31)
          
          CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
          CHARACTER*1 FCO2

    !C...FIND MET DATA FOR FIRST MONTH OF SIMULATION
          IF(KDAYS.EQ.0) THEN
            MTH=MONTH
            MYR=MYEAR
            READ(9,*)MONTH,KDAYS,MYEAR
            
      30    IF(MONTH.EQ.MTH.AND.MYR.EQ.MYEAR) GO TO 20
            
            DO 35 IX=1,KDAYS
              READ(9,*)
      35    CONTINUE        
            
            READ(9,*) MONTH,KDAYS,MYEAR
            GO TO 30
      10    WRITE(99,1001)
            WRITE(8,1001)
            STOP
          ELSE
            READ(9,*) MONTH,KDAYS,MYEAR
          ENDIF
    
    !C...First Line- MONTH, NO. OF DAYS IN THE MONTH AND YEAR 
    !C...Next Line?
    !1. Air Temperature is in oF, then convert to Celsius - TAIR
    !2. Dew Point Temperature (CELSIUS) )-TDEW
    !3. Wind Velocity (MPH)- WIND
    !4. Wind Direction -DRCT
    !5. Solar Radiation (Langely/day)- RAD 
    !6. Sunshine Percentage(0-100) SUNSHINE, 100 is for perfect clear sky or no clouds
    !7. Precipitation (inches*100.0) (PR)in input data, then convert to inches 
    !8... Snowfall (mm) - SNOWFL in the input data it is mm, then convert to meters.
    !C  Cloud Cover CR(31) is from 0 to 1.0, 0 stands no cloud at all,
    !C  and 1.0 stands for complete cloud cover in the sky.
    !C...SUNSHINE(31)- Sunshine precetange, read from input data file,
    !C   SUNSHINE(31) is from 0 to 100.

     20   CONTINUE
          DO 104 K=1,KDAYS
            READ(9,*) TAIR(K),TDEW(K),WIND(K),DRCT(K),RAD(K),SUNSHINE(K),PR(K),SNOWFL(K) 
    
    !Fang, April 16, 2009, If wind is less than 2 mph, it is set as 2 mph
    !This was done for future climate scenario before
            IF(WIND(K).LT.2.0) WIND(K)=2.0
            
    !Fang, no longer read Cloud Cover
    !    +   RAD(K),SUN,CR(K),PR(K),SNOWFL(K)
    
    !Fang 4/2/2009 Check dew point temperature >> air temperature        
            IF(TDEW(K).GT.TAIR(K)) THEN
             
            IF((TDEW(K)-TAIR(K)).GT.2.0) THEN
             WRITE(*,*)
             WRITE(*,*) "Dew point temperature is GREATER than air temperature!"
             WRITE(*,*) "MONTH, MDAY, MYEAR, TAIR(K), TEDW(K) in oF" 
             WRITE(*,*) MONTH,K,MYEAR,TAIR(K),TDEW(K)
             
    !Fang, save the information into the file "warninfo.dat"      
             WRITE(198,*)
             WRITE(198,*) "Dew point temperature is GREATER than air temperature!"
             WRITE(198,*) "MONTH, MDAY, MYEAR, TAIR(K), TEDW(K)" 
             WRITE(198,*) MONTH,K,MYEAR,TAIR(K),TDEW(K)
            ENDIF
     
    !Fang, sometimes Tdew is only slightly greater than Tair         
             IF((TDEW(K)-TAIR(K)).LE.2.0) THEN
              TDEW(K)=TAIR(K)-2.0
             ENDIF
    !NOT stop the program, but write the information on screen or to the file "warninfo.dat"         
    !        STOP
            
            ENDIF
            
            SNOWFL(K)=SNOWFL(K)/1000.0 
            PR(K)=PR(K)/100.0
     104  CONTINUE     
          DO 100 K=1,KDAYS
            TAIR(K)=(TAIR(K)-32.0)*0.5556

            TDEW(K)=(TDEW(K)-32.0)*0.5556
            CR(K)=(100.0-SUNSHINE(K))*0.01
            
 !Fang 4/2/2009 Check dew point temperature << air temperature
            XEA=6.035*10**(7.45*TDEW(K)/(235.0+TDEW(K)))
            XES=6.035*10**(7.45*TAIR(K)/(235.0+TAIR(K)))
        WRITE(330,3305)MONTH,MYEAR,K,TAIR(K),TDEW(K),WIND(K),RAD(K),CR(K),PR(K),SNOWFL(K)
        
            IF((XEA/XES).LT.0.10) THEN
             
             WRITE(*,*)
             WRITE(*,*) "Dew point temperature or relative humidity is TOO LOW - Check warminfo.dat!"
             WRITE(198,*)
             WRITE(198,*) "MONTH, MDAY, MYEAR, TAIR(K), TEDW(K) in oC, XEA/XES - Dew point temperature is TOO LOW!!!" 
             WRITE(198,*) MONTH,K,MYEAR,TAIR(K),TDEW(K),(XEA/XES)
            
            ENDIF  
            
     100  CONTINUE
   
    !C... Compute solart wave radiation if not available
          DO K = 1, KDAYS
           IF(RAD(K).EQ.9999.) THEN
                RAD(K) = 0.0
                
   !C   CCTEN(31) is cloud cover from 0 to 10 (complete cloud cover)
                CCTEN(K)=CR(K)*10.0

              DO I = 1, 24 
                      
                 SLONG0=15.0*INT(SLON/15.0)
                      
                 EQTNEW=0.170*SIN(4*3.14159*(JDY-80)/373)-0.129*SIN(2*3.14159*(JDY-8)/355)
                 TAUD=(2*3.14159*(JDY-1))/365
                 DECL=0.006918-0.399912*COS(TAUD)+0.070257*SIN(TAUD)-0.006758*COS(2*TAUD)+0.000907*SIN(2*TAUD)-0.002697*COS(3*TAUD)+0.001480*SIN(3*TAUD)  
                 X=I - 1
                 H=0.261799*(X-(SLON-SLONG0)*0.0666667+EQTNEW-12.0)
                 SINAL=SIN(SLAT*.0174533)*SIN(DECL)+COS(SLAT*.0174533)*COS(DECL)*COS(H)
                 A0=57.2985*ASIN(SINAL)
                 SRO=2.044*A0+0.1296*A0**2-0.001941*A0**3+7.591E-6*A0**4
                 SRO=(1.0-0.0065*CCTEN(K)*CCTEN(K))*SRO*24.0
                 IF (A0.LT.0.0) SRO=0.0

    !C** SI units ***** RHOW=1000.0, CP=4186.0 *** SRO in W/M**2
                   SRO=SRO*3.14E-8*1000.0*4186.0
                   RAD(K)=RAD(K)+SRO*0.08598
              ENDDO
           ENDIF
        ENDDO

     1001 FORMAT(//,5X,70('*'),//,20X,'PROGRAM ABORTED.',/,10X,'METEOROLOGICAL DATA FILE DOES NOT',/,15X,'MATCH YEAR OF SIMULATION',//,5X,70('*'))
        
    !Corrections for future climate sceanrios 
    !based on CCC GCM output (1990)
    !Canadian Climate Centre, Atmospheric Environment Centre

          
          IF(FCO2.EQ.'y'.OR.FCO2.EQ.'Y') THEN
      
           DO 1287 K=1,KDAYS
        
            IF(MODELS_SCENARIO.EQ.1)THEN
            !Directly use given specific humidity from CCC GCM
            !RH -- Relative humidity, SPH - Specific humidity
            !Compute specific humidity for the past climate condition
                    PO=1.0133E+5
                    EA=611.0*EXP(17.3*TDEW(K)/(237.2+TDEW(K)))
                    SPH=0.622*EA/PO*FDEW(MONTH)

            !For GISS climate scenario where RH is given !
           !CG      EA=611.0*EXP(17.3*TDEW(K)/(237.2+TDEW(K)))
           !CG      ES=611.0*EXP(17.3*TTA/(237.2+TTA))
           !CG      RH=EA/ES*FDEW(MONTH)
              
           !CG      FEA=RH*FES/611.0
           !CG      TDEW(K)=237.2*ALOG(FEA)/(17.3-ALOG(FEA))

            !EA for future climate scenario !
            EA=SPH*PO/0.622
            EAV=EA/611.0
            TDEW(K)=237.2*ALOG(EAV)/(17.3-ALOG(EAV))
                      
            
  !  CG      TTA=TAIR(K)
  !  CG      TTD=TDEW(K)
            TAIR(K)=TAIR(K)+FTAR(MONTH)

           !Set condition for dew point temperatures
              
            IF(TDEW(K).GT.TAIR(K)) TDEW(K)=TAIR(K)
            WIND(K)=WIND(K)*FWIN(MONTH)
            IF(WIND(K).LT.2.0) WIND(K)=2.0
            RAD(K)=RAD(K)*FSOL(MONTH)
           
            CR(K)=CR(K)*FTCL(MONTH)
            PR(K)=PR(K)*FPRE(MONTH)
            SNOWFL(K)=SNOWFL(K)*FPRE(MONTH)
           
            WRITE(335,3305)MONTH,MYEAR,K,TAIR(K),TDEW(K),WIND(K),RAD(K),CR(K),PR(K),SNOWFL(K)
       
           
            ENDIF
           
           IF(MODELS_SCENARIO.GT.1)THEN
            
!!!_____________           
           ! EAIR_PRE = 4.596*EXP((17.27*TDEW(K))/(237.3+TDEW(K)))
           ! ESAT_PRE=  4.596*EXP((17.27*TAIR(K))/(237.3+TAIR(K)))
            
           ! RH_PRESENT=100.0*EAIR_PRE/ESAT_PRE
          
           ! RH_FUTURE=RH_PRESENT+FUTR_HUM_CHANGE(MONTH)*100.0
           
       
           ! EAIR_FUT=4.596*EXP(   (17.27*(TAIR(K)+FTAR(MONTH)))/(237.3+(TAIR(K)+FTAR(MONTH)))   ) 
            
          !  CALCUL=RH_FUTURE*(1/100.0)*EAIR_FUT
           
          !  TDEW(K)=(237.3*LOG(CALCUL/4.596))/(17.27-LOG(CALCUL/4.596))
          !  WRITE(360,3496)RH_PRESENT,RH_FUTURE,CALCUL,TDEW(K)
! 3496       FORMAT(4(F8.3,1X))     
           
!!_______________________________           
           
           
            !Directly use given specific humidity from CCC GCM
            !RH -- Relative humidity, SPH - Specific humidity
            !Compute specific humidity for the past climate condition
                    PO=1.0133E+5
                    EA=611.0*EXP(17.3*TDEW(K)/(237.2+TDEW(K)))
                    SPH=(0.622*EA/PO)+FUTR_HUM_CHANGE(MONTH)

            !For GISS climate scenario where RH is given !
           !CG      EA=611.0*EXP(17.3*TDEW(K)/(237.2+TDEW(K)))
           !CG      ES=611.0*EXP(17.3*TTA/(237.2+TTA))
           !CG      RH=EA/ES*FDEW(MONTH)
              
           !CG      FEA=RH*FES/611.0
           !CG      TDEW(K)=237.2*ALOG(FEA)/(17.3-ALOG(FEA))

            !EA for future climate scenario !
            EA=SPH*PO/0.622
            EAV=EA/611.0
            TDEW(K)=237.2*ALOG(EAV)/(17.3-ALOG(EAV))
            
  !!!____            
                   
            
  !  CG      TTA=TAIR(K)
  !  CG      TTD=TDEW(K)
            TAIR(K)=TAIR(K)+FTAR(MONTH)

           !Set condition for dew point temperatures
              
            IF(TDEW(K).GT.TAIR(K)) TDEW(K)=TAIR(K)

            RAD(K)=RAD(K)+FSOL(MONTH)
           
            IF(RAD(K).LT.0.0)RAD(K)=0.0
            WIND(K)=WIND(K)+FWIN(MONTH)
            IF(PR(K).GT.0.0)PR(K)=PR(K)+FPRE(MONTH)
            IF(PR(K).LT.0.0)PR(K)=0.0
            IF(SNOWFL(K).GT.0.0)THEN
            !SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)/39.37
       
       
       
        !   IF(PR(K).GT.0.0)SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)  ! Earlier I used this line when no snowfall 
                                                                !was not transformed like the next line
        
        !   IF(SNOWFL(K).GT.0.0)THEN                ! Wrong command made the simuation temp close to zero 
                        
         !   IF(PR(K).GT.0.0)THEN   
            
         !   SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)
            
         IF(TAIR(K).GE.-2.22)                   SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*10./39.37
         IF(TAIR(K).LT.-2.22.AND.TAIR(K).GE.-6.67) SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*15./39.37
         IF(TAIR(K).LT.-6.67.AND.TAIR(K).GE.-9.44) SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*20./39.37
         IF(TAIR(K).LT.-9.44.AND.TAIR(K).GE.-12.22) SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*30./39.37
         IF(TAIR(K).LT.-12.22.AND.TAIR(K).GE.-17.22)SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*40./39.37
         IF(TAIR(K).LT.-17.22.AND.TAIR(K).GE.-28.89) SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*50./39.37
         IF(TAIR(K).LT.-28.89.AND.TAIR(K).GE.-56.67) SNOWFL(K)=SNOWFL(K)+FPRE(MONTH)*60./39.37

         ENDIF
 
  !!_55555___ Cloud Cover Calculation from Solar Radiatioan for Future
              
              
              PI=3.14159
              LOCAL    =  SLON
              STANDARD =  15.0*INT(SLON/15.0)
                  
              CLOUD(K,1)=0.0
              DO 6570 ICLD=1,101
              CLOUD(K,(ICLD+1))=CLOUD(K,ICLD)+.1
             
              HOUR=0
              RAD_HOURLY=0.0
              DO 5070 JW=1,24
           
              HOUR=HOUR+1
              TAUD     = (2*PI*(JDY-1))/366
              EQTNEW   =  0.170*SIN(4*PI*(JDY-80)/373)-0.129*SIN(2*PI*(JDY-8)/355)
              HH   =  0.261799*(HOUR-(LOCAL-STANDARD)*0.0666667+EQTNEW-12.0)
              DECL =  0.006918-0.399912*COS(TAUD)+0.070257*SIN(TAUD)-0.006758*COS(2*TAUD)+0.000907*SIN(2*TAUD)-0.002697*COS(3*TAUD)+0.001480*SIN(3*TAUD)   
              SINAL    =  SIN(SLAT*.0174533)*SIN(DECL)+COS(SLAT*.0174533)*COS(DECL)*COS(HH)
              A00  =  57.2957795*ASIN(SINAL)
              A0       =  A00
      
              IF (A0 > 0.0) THEN
              SRON = (1.0-0.0065*CLOUD(K,ICLD)**2)*24.0*(2.044*A0+0.1296*A0**2-1.941E-3*A0**3+7.591E-6*A0**4)*3.14E-8*1000.0*4186.0
            
              ELSE
              SRON = 0.0
              END IF
      
              RAD_HOURLY=RAD_HOURLY+SRON 
             IF(JW.EQ.24)RAD_DAILY=RAD_HOURLY
5070            CONTINUE
               
             ! HERE THE VALUE WAS PROCESSED AS W-HOUR/SQ METER, BUT OUR FINAL WEATHER FILE WILL BE LANGLEY/DAY,
             ! SO IT WILL BE CONVERTED ON NEXT LINE
          
             RAD_DAILY=RAD_DAILY/11.628
       
             RADLONGLEY(K,ICLD)=RAD_DAILY
             
                
 6570           CONTINUE
              
                    
             DO 1785 JJK=1,101
             IF( RAD(K).GT.RADLONGLEY(K,1)) GO TO 8110
             IF( RAD(K).LT.RADLONGLEY(K,101))GO TO 8120
             IF( RAD(K).LT.RADLONGLEY(K,JJK).AND. RAD(K).GT.RADLONGLEY(K,JJK+1))GO TO 8130
               GO TO 1785
            
              
8110         FTCL(K)=0.0
             GO TO 1795
              
8120         FTCL(K)=1.0
             GO TO 1795    

8130         FTCL(K)=0.10*CLOUD(K,JJK)
             GO TO 1795    
 1785        CONTINUE
 
                   
  1795       CR(K)=FTCL(K)
            
 !!______________________________ Cloud Cover Calculation done           
          WRITE(340,3305)MONTH,MYEAR,K,TAIR(K),TDEW(K),WIND(K),RAD(K),CR(K),PR(K),SNOWFL(K)
            ENDIF
            
         
            IF(CR(K).GT.1.0) CR(K)=1.0
                    
                    IF(TAIR(K).GT.0.0.AND.SNOWFL(K).GT.0.0) THEN
                   !C* Transfer snow in meter to rainfall in inches
                    PR(K)=SNOWFL(K)/10.0*39.37
                    SNOWFL(K)=0.0
                    ENDIF
                   
            
            
 3305       FORMAT(3(I4,1X),7(F7.3,1X))
           
     1287 CONTINUE
          ENDIF
     
          RETURN
          END

    !C***************************************************C
    
    !C***************************************************C
          SUBROUTINE PDEPTH(RHOMIX,RHOIN,Q,S,IHP,QENIN,MBOT,SUMZ,FT,WIDTH,HP)

    !C***
    !C*** Compute depth at plunge point and initial entrainment at 
    !C*** the plunge point (GAMAIN). (by Akyama)
    !C***
          REAL*8 A,V,TV,ATOP
         !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
       
    !C...COMPUTE DEPTH AT PLUNGE POINT
          EPSIN=(RHOIN-RHOMIX)/RHOMIX
          IF(EPSIN.GT.0.) GO TO 7
          IHP=1
          HP=0.
          RETURN
     7    X=Q/(86400*WIDTH)
          IF(S.LE.6.66667E-3) THEN
            HP=1.1*(FT*X*X/(S*EPSIN*9.81))**(.3333)
            GAMAIN=0.15
           ELSE
            HP=1.6*(X*X/(EPSIN*9.81))**(.3333)
            GAMAIN=1.8
          ENDIF
          QENIN=GAMAIN*Q
          SUMZ=0.
          IHP=0
          DO 200 I=1,MBOT
            IHP=IHP+1
            SUMZ=SUMZ+DZ(I)
            IF(SUMZ.GE.HP) GO TO 5
     200    CONTINUE
          HP=SUMZ
     5    RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE SETUP
    !C***
    !C*** Determine the initial thickness, volume, and area of layers
    !C*** and the total volume of above each layer from the depths given
    !C*** in the input data file.
    !C***
          REAL*8 A,V,TV,ATOP,VDUM
          INTEGER FMON,FDAY,FYEAR
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
         
          DZ(MBOT)=ZMAX-(Z(MBOT)+Z(MBOT-1))*0.5
          Z(MBOT)=ZMAX-DZ(MBOT)*0.5
          CALL LAKE(DZ(MBOT),VDUM,0,3)
          V(MBOT)=VDUM
          AZ=DZ(MBOT)
          TV(MBOT)=V(MBOT)
          DO 10 I=1,MBOT-2
            II=MBOT-I
            DZ(II)=Z(II+1)-(DZ(II+1)+Z(II)+Z(II-1))*0.5
            Z(II)=(DZ(II)+Z(II)+Z(II-1))*0.5
            AZ=AZ+DZ(II)
            CALL LAKE(AZ,VDUM,0,3)
            TV(II)=VDUM
            V(II)=TV(II)-TV(II+1)
     10   CONTINUE
          DZ(1)=Z(2)-DZ(2)*.5
          AZ=AZ+DZ(1)
          CALL LAKE(AZ,VDUM,0,3)
          TV(1)=VDUM
          V(1)=TV(1)-TV(2)
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
        !  REAL*8 FUNCTION RHO (T,C,CD)
          
          FUNCTION RHO (T,C,CD)
          REAL*8 RHO
          
    !C...CALCULATES THE DENSITY OF WATER AS A FUNCTION OF TEMPERATURE PLUS
    !C...DENSITY DUE TO TOTAL SOLIDS (SUSPENDED AND DISSOLVED)
    !C... Michaelis Riley formula
    !    RHO=(.999878+T*(6.16608E-5+T*(-8.14577E-6+T*4.76102E-8)))*1000.
    !   +     +(C+CD)*0.001
    !C... Ellis Chris Formula !!
          RHO=999.869+T*(6.6741E-2+T*(-8.8556E-3+T*(8.2303E-5-T*5.516E-7)))+(C+CD)*0.001
    
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE SETZ(MBOT)
    !C***
    !C*** Compute Z from DZ for each layer
    !C***
          REAL*8 A,V,TV,ATOP
         !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
                  
          AZ=0.
          DO 100 I=1,MBOT
            Z(I)=AZ+DZ(I)*.5
            AZ=AZ+DZ(I)
     100  CONTINUE
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE SOLVE(VAR2,MBOT)
    !C***
    !C*** Tri-diagonal matrix solving routine
    !C***
          REAL*8 AK,BK,CK,DK,TT,TX(120)
          COMMON/SOLV/ AK(120),BK(120),CK(120),DK(120)
          DIMENSION VAR2(120)

          DO 60 I=2,MBOT
            TT=AK(I)/BK(I-1)
            BK(I)=BK(I)-CK(I-1)*TT
            DK(I)=DK(I)-DK(I-1)*TT
     60   CONTINUE
    !C********BACK SUBSTITUTION**************
          TX(MBOT)=DK(MBOT)/BK(MBOT)
          DO 70 I=1,MBOT-1
            J=MBOT-I
     70     TX(J)=(DK(J)-CK(J)*TX(J+1))/BK(J)
          DO 80 I=1,MBOT
     80     VAR2(I)=SNGL(TX(I))
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE SPLIT(I,LW)
    !C***
    !C*** Routine to split thick layers (DZ > DZUL) into two or more 
    !C*** layers of equal thickness.  All state variables are the same in 
    !C*** each new layer.  Volume is adjusted later.
    !C***
          INTEGER T_FLAG,DO_FLAG
          REAL*8 A,V,TV,ATOP
          INTEGER FMON,FDAY,FYEAR
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL
           
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/BAKCUPTDO/T_FLAG,DO_FLAG,MONBAK,DAYBAK,YEARBAK,TBAK(120),DOBAK(120)

          IF(T_FLAG.EQ.0.OR.(IPRNT(4).EQ.1.AND.DO_FLAG.EQ.0)) THEN
            WRITE(99,1991)I,MBOT,MBOT+1
          IF(T_FLAG.EQ.1) THEN
             WRITE(99,*)
             WRITE(99,*)'Initial inputted T  before Re-Split : '
             WRITE(99,1992)(TBAK(K),K=1,MBOT)
          ENDIF
            IF(iprnt(4).eq.1.and.DO_FLAG.EQ.1) THEN
             WRITE(99,*)
             WRITE(99,*)'Initial inputted DO  before Re-Split : '
	         WRITE(99,1993)(DOBAK(K),K=1,MBOT)
          ENDIF
     1991 FORMAT(1X,/,1X,'Re-Split No.',I4, ' Layer. Before Re-Split, '&
               'MBOT =',I4,'. After Re-split, MBOT =',I4)
     1992 FORMAT(1X,10F8.2) 
     1993 FORMAT(1X,10F8.3) 
          ENDIF

          DO 100 K=I,MBOT
            II=MBOT+I-K
            T2(II+1)=T2(II)
            C2(II+1)=C2(II)
            CD2(II+1)=CD2(II)
            DO 50 KI=1,NCLASS
      50      CHLA2(KI,II+1)=CHLA2(KI,II)
            PA2(II+1)=PA2(II)
            BOD2(II+1)=BOD2(II)
            DSO2(II+1)=DSO2(II)
          TBAK(II+1)=TBAK(I)
          DOBAK(II+1)=DOBAK(I)
            IF(MODEL.EQ.3) THEN
              SI2(II+1)=SI2(II)
              DO 51 KI=1,3
     51         PC2(KI,II+1)=PC2(KI,II)
              IF(NITRO.EQ.1) THEN
                DO 52 KI=1,3
     52           XNC2(KI,II+1)=XNC2(KI,II)
                XNO2(II+1)=XNO2(II)
                XNH2(II+1)=XNH2(II)
              ENDIF
            ENDIF
            DZ(II+1)=DZ(II)
     100  CONTINUE      
          
          MBOT=MBOT+1
          DZ(I+1)=DZ(I)*0.5
          DZ(I)=DZ(I+1)
          
          IF(LW.GE.I) LW=LW+1
          IF(T_FLAG.EQ.0.OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0)) THEN
          IF(T_FLAG.EQ.1) THEN
             WRITE(99,*)
             WRITE(99,*)'Initial inputted T  after Re-Split : '
             WRITE(99,1992)(TBAK(K),K=1,MBOT)
          ENDIF
            IF(iprnt(4).eq.1.and.DO_FLAG.EQ.1) THEN
             WRITE(99,*)
             WRITE(99,*)'Initial inputted DO  after Re-Split : '
	         WRITE(99,1993)(DOBAK(K),K=1,MBOT)
          ENDIF
          ENDIF
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                START PROGRAM                              C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE START(ST,S,FT,ISTART,INFLOW,ITER,II8,PATH,PATH_F)
    !     SUBROUTINE START(ST,S,FT,ISTART,INFLOW,ITER,II8,TSLAV,PATH,PATH_F)
    !C***
    !C*** Routine to read the input data file for initial -- (9898)
    !C*** conditions and input coefficients
    !C***
          REAL*8 A,V,TV,ATOP,ASX
          INTEGER FMON,FDAY,FYEAR,T_FLAG,DO_FLAG,DAYBAK,YEARBAK,FIRSTDAY,EXTRADAY
          DIMENSION FIRSTDAY(12),JDAY_FIELD(50,150),JDAY_FIELD_CHL(50,150)
       !  DIMENSION ZTS(2,120),XTS(2,120),FIRSTDAY(12),JDAY_FIELD(50,150),JDAY_FIELD_CHL(50,150)          
          DIMENSION CHLMEANX(90),SDYX(90),NYEARSD(90)
       !  DIMENSION CHL_FIELD(2008,366,100)
          DIMENSION DEPTHLYR(200),DEPTHFLD(200),TEMPFLD(200),DSOFLD(200),TEMPINT(500),DSOINT(500) 
          
          DATA FIRSTDAY/1,32,60,91,121,152,182,213,244,274,305,335/

    !CFX   USING CHIAI AND CHLHY(I)
    !CFX NSDAY(140) - The date to switch SD, SDFIELD(140) - measured Secchi depth
          COMMON/CHLAP/NCDAY(120,2),GCHLA(120,2),ICHLASD,IFIELD,CHLTOP(90),CHLBOT(90),I_NO_FIELD_OUT_PROFILE
          COMMON/FIELD/IFLAG(10),FLDATA(10,120),DEPTH(120),NFLD(10),SD,NSKIPDAY,NSDAY(640),SDFIELD(640),TAPE64

          COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)
          COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
          COMMON/NEW2/KYEAR_CHL(90),NTDY_CHL(90),FIELD_CHL(200),CHL_FIELD(2008,366,100),NDAYO_CHL
          COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
          COMMON/ILAKEN/ILAKENAME,ILAKENAMES 
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
    
          COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,CDIS0,CNDSNW0,DEPTHC,ICEMON,ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI,RATIOX,ZSLT
         !COMMON/BOTT/SNOWFL(31),TSL(21,120),ZSL(21)
          COMMON/CONR/FAKW,SNCOE,COEWIN
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          COMMON/CHOICE2/NPRNT_CHL(640)
          COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
          COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/NO_DAY_TOT/NDAYO
         !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
         
          COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
          COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
          COMMON/PHYTO0/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),GROMAX(3),TMAX(3)
          COMMON/PHYTO1/TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN(90),CHLMAX,SDY(90)
          COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),REPRO,XI,XKMZ,GRAZE(3,120)
          COMMON/FILEX/DIN,MET,FLO,TAPE8,TAPE1,IREC
          COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(81,36),ELEV(81,36)
          COMMON/BAKCUPTDO/T_FLAG,DO_FLAG,MONBAK,DAYBAK,YEARBAK,TBAK(120),DOBAK(120)
          COMMON/WELEVATION/STW
          COMMON/EMCO/EMCOE_READ,ISY
          COMMON/DATA50/ICOUNTER,SMDATA(2,120),ISD
          COMMON/NDAYOCOUNTER/NDAYO_COUNTER
          COMMON/DATA56/IPROFILE,MBOT1
          CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
          CHARACTER*16 FILE27
          CHARACTER*21 FILE45
          CHARACTER*1 T1(40),PATH(80),FFILE(100),FILE1(40),PATH_F(80)!,FILE45(21)
          EQUIVALENCE (T1(1),TAPE1),(FILENAME,FFILE(1))
          
          DATA FILE1 /'C','O','N','T','.','P','L','T',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
   
    !    &     FILE45/'c','h','l','a','.','p','a','t',' ',' ',' ',' ',' ',
    !    &	         ' ',' ',' '/,
    !    &     FILE27/'t','s','g','p','r','o','.','s','d','f',' ',' ',' ',' ',' ',' '/
       	         
     
           NITRO=0
          DO 200 I=1,100
            NPRNT(I)=0
     200  CONTINUE
    
    !Clj** Open another input file

          OPEN (8,FILE="..\#COMMON\FIXED_INPUT\fixed_input.dat") 
         
          READ(8,*)

    !C** INPUT MODEL OPTIONS AND INITIAL CONDITIONS ******
    !     READ(7,*) MODEL,NCLASS,IDIATOM
          READ(8,*)
          READ(8,*) MODEL,NCLASS,IDIATOM
    !     MODEL   = 4
    !	  NCLASS  = 1
    !	  IDIATOM = 0
    !Clj** model  : specifies level of biological simulation (1, 2, 3)
    !CFX** MODEL = 4 is for regional temperature and DO model developed by Xing Fang (1991 - 1996)
    !Clj** nclass : specifies number of phytoplankton types
    !Clj** idiatom: specifies number of silica limited phytoplankton groups (0 or 1)


          READ(8,*)
          READ(8,*) NITRO,ITER
    !     NITRO   = 0
    !C	  ITER    = 1
    !Clj** nitro  : specifies number of non-nitrogen limited phytoplankton groups (0 or 1)
    !Clj** iter   : number of iteration passes through the nutrient-biological subroutines
          READ(8,*)
          READ(8,*) DZLL,DZUL,BETA,EMISS
          
          IF(ILAKENAMES.EQ.13.OR.ILAKENAMES.EQ.13001) DZUL=1.5
          
    !Clj** DZLL, DZUL : minimum, maximum layer thickness (m)
    !Clj** BETA : surface adsorption fraction for solar radiation
    !Clj** EMISS : emmissivity of water
          READ(8,*)
          READ(8,*) WCHANL,WLAKE,S,FT
    !Clj** WCHANL : Width of the inflow channel 
    !Clj** WLAKE  : Maximum width of the lake perpendicular to the inflow channel
    !Clj** S      : downstream slope of inflow channel
    !Clj** FT     : mannings friction factor for the inflow channel
          READ(8,*)     
          READ(8,*) ALPHA,BW
    !Clj** ALPHA  : side slope of the outflow channel
    !Clj** BW     : bottom width of the outflow channel
          READ(8,*)
          READ(8,*) ZSLT,IZSLT
    !CFX  ZSLT=Total depth of sediment (m)
    !CFX  IZSLT=Number of sediment layers
          READ(8,*)
          READ(8,*) MELMON,MELDAY,ICEMON,ICEDAY
    !CFX  MELMON, MELDAY= The first month or day of ice melting.
    !CFX  ICEMON, ICEDAY= The first month or day of freezing.
          READ(8,*)
          READ(8,*) HKMAX,WCOEF,WCFSF
    !Clj** HKMAX : maximum hypolimnetic vertical turbulent diffusion coefficient (m^2/day)
    !Clj** WCOEF : wind coefficient for convective heat loss, sheltering for summer
    !Clj** WCFSF : wind coefficient for convective heat loss, sheltering for fall
          READ(8,*)
          READ(8,*) TSLMEAN,ETAB,T2MBOT
    !CFX  TSLMEAN= Annual mean temperature at 6 or 10M
    !CFX  ETAB=COEFFICIENT, T2MBOT=Initial bottom water temperature?
          READ(8,*)
          READ(8,*) HKMXIS,WCFIS,WSIS
    !CFX   HKMXIS=0.1 maximum hypolimnetic diffusion coefficient (m^2/day)
    !CFX   WCFIS=0.0 wind function coefficient with ice cover.
    !CFX   WSIS=0.0 wind sheltering coefficient with ice cover.
    !C** EMCOE(1) is percentage of surface area which contributes to mixing, typical 50%
    !Clj**
    
          READ(8,*)  
          READ(8,*) BRR,FVCHLA(NCLASS+1)
    !Clj** BRR    : benthic phosphorus release rate
    !Clj** FVCHLA : setting velocity for algae
          READ(8,*)
          READ(8,*) YCHO2,Y2CHO2,YCBOD,YPBOD,YNHBOD,YPZP,YZDO,YZW,YNHZP
    !Clj** YCHO2  : mass ratio of chlorophyll to oxygen in photosynthesis
    !Clj** Y2CHO2 : mass ratio of chlorophyll to oxygen in algal respiration
    !Clj** YCBOD  : mass ratio of chlorophyll to oxygen demand in detritus
    !Clj** YPBOD  : mass ratio of phosphorus to oxygen in detritus
    !Clj** YNHBOD : mass ratio of ammonium to oxygen in detritus
    !Clj** YPZP   : mass of phosphorus per individual zooplankter
    !Clj** YZDO   : oxygen demand per individual zooplankter in zooplankton decay
    !Clj** YZW    : mass of individual zooplankter
    !Clj** YNHZP  : mass of ammonium per individual zooplankter

          CLOSE(8)
         
     !     READ(7,*)
     !     READ(7,*)
     !     READ(7,*)
     !     READ(7,*) 
     !     READ(7,*)
     !     READ(7,*)
     !     READ(7,*)
     !     READ(7,*) MONTH,ISTART,MYEAR,FMON,FDAY,FYEAR
     !     Myear=1961
     !     Fyear=2008
          
          WRITE(199,*) "MONTH  - The first month of simulation:"
          WRITE(199,*) "ISTART - The first day of the month that simulation starts:"
          WRITE(199,*) "MYEAR  - Year at the beginning of the simulation:"
          WRITE(199,*) MONTH,ISTART,MYEAR,FMON,FDAY,FYEAR
 
    !Clj** MONTH  : The first month of simulation - MUST be LESS than the first month with field data
    !Clj** ISTART : The first day of the month that simulation starts
    !Clj** MYEAR  : Year at the beginning of the simulation
    !C**** FMON,FDAY,FYEAR : final simulation month, day & year
 
    !Clj** JDY: the Julian day of first simulation as constant
    !FX, 2008.  EXTRADAY is for leap-year in February
    !FX, 2008.  FIRSTDAY is the first Julian day of each month, see DATA      
          EXTRADAY=0
          IF(AMOD(REAL(MYEAR),4.0).EQ.0.0) EXTRADAY=1
          IF(MONTH.LT.3) THEN
           JDY=FIRSTDAY(MONTH)+ISTART-1
          ELSE
           JDY=FIRSTDAY(MONTH)+ISTART-1+EXTRADAY
          ENDIF
       
    !FX, 2008. NYTOT is total number of simulation years           
          NYTOT = FYEAR - MYEAR + 1
        
        IF(NYTOT.EQ.1) THEN
           NM = FMON - MONTH + 1
           NMFIN = NM
        ELSE
           NM = 13 - MONTH
           NMFIN = FMON
        ENDIF
          
    !Fang, 2008 - Read initial lake stage or set as 0.0    
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)          
          READ(7,*) MBOT,ZMAX,ST,IPROFILE

          
          WRITE(199,*) "MBOT,ZMAX,ST,IPROFILE"
          WRITE(199,*)MBOT,ZMAX,ST,IPROFILE
   
   !Fang, April, 2009 - ADD IPROFILE =1 for uniform initial profile or 0 for NON-unifrom initial profile      
         IF(IPROFILE.EQ.1) THEN
           MBOT1=1
          ELSE
           MBOT1=MBOT
          ENDIF
   
   !Fang, 4/2/2009
   !Change DZUL for deep lake
   !There are 7 (seven) small layer near the surface and maximum about 30 layers below
   !       DMXNEW=ZMAX/30.0
   !       IF(DMXNEW.GT.DZUL) DZUL=DMXNEW
          
   !Fang, 2008, for regional lake simulation, we will set ST as 0.0 elevation 
   !Fang, 2008.  STW = ELEV(JS,JN) in the main program   
          IF(ST.LT.0.1) THEN
           ST=STW
          ENDIF
          
    !Clj** MBOT   : number of layers in the initial conditions
    !Clj** ZMAX   : maximum depth of the lake
    !Clj** ST     : initial stage of lake in same datum as DBL (lake surface elevation)
          DBL = ST - ZMAX
          ELCB = ST + 1.0
          READ(7,*)
          READ(7,*)
          READ(7,*) 
          READ(7,*)  
          READ(7,*)        
          READ(7,*) XK1,XK2,EMCOE(3)
          
      !   IF(EMCOE_READ.EQ.0)EMCOE(3)=-1.84
      !   IF(EMCOE_READ.EQ.1)EMCOE(3)=1.84
 
          XK1_INPUT=XK1
          
      !   IF(EMCOE(3).GT.0.0) OPEN (64,FILE=TAPE64)

          IF(EMCOE(3).GT.0.0.AND.ISD.EQ.2) OPEN (64,FILE=TAPE64)
          
          WRITE(199,*) "XK1,XK2,EMCOE(3)"   
          WRITE(199,*)XK1,XK2,EMCOE(3)
!Light atteuation coefficient is an important parameter for water temperature and DO simulations
!Total attenuation is Kw = XK1 + XK2*CHLa = 1.84/SD, it may include effect of surfeace suspended solids.     
!XK2 is fixed as 20. If you input XK1 and WANT to use XK1, you MUST set EMCOE(3) < 0
!If you SET EMCOE(3) as a positve number, so Kw = EMCOE(3)/SD - Secchi depth must be given!
!We use EMCOE(3) > 0.0 for regional lake simulations or you only know information on Secchi depth.
!XK1 and XK2 : light attenuation coefficient for water (1/m) and cholorphyll-a (1/mg.m)
    
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*) WSTR,WSSF
          
          WRITE(199,*) "WSTR,WSSF"
          WRITE(199,*)WSTR,WSSF
    !!Clj** WSTR : wind coefficient for convective heat loss, sheltering for summer
    !!Clj** WSSF : wind coefficient for convective heat loss, sheltering for fall

    !CFX   For open water seasonal without ice cover!
    !CFX   SM - for summer OR OPEN WATER SEASON !
    !CFX   WE NEED TO STORE COEFFICIENTS FROM INPUT FILE !
          HKMXSM=HKMAX
          WCFSM=WCOEF
          WSSM=WSTR
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*) COEWIN,SNCOE
          
          WRITE(199,*) "COEWIN,SNCOE"
          WRITE(199,*)COEWIN,SNCOE
    !CFX  SNCOE - MAXIMUM RATO OF THsnow TO THice (Empirical)
    !CFX  COEWIN - TEMPORARILY PARAMETER FOR WIND SHELTERING 
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*) AHTBTM,SRCP
          
          WRITE(199,*) "COEWIN,SNCOE"
          WRITE(199,*)COEWIN,SNCOE
    !CFX  AHTBTM=Thermal diffusivity of sediment (m*m/day)
    !CFX  SRCP = DENSITY*Cp FOR SEDIMENT !

          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*) CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC
          
          WRITE(199,*) "CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC"
          WRITE(199,*)CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC
    !CFX  CFSNOW=snow compaction factor (0.2 - 0.4) = 0.2;
    !CFX  CDIS0=ice conductivity in W/m/C or cal/day/m/C;
    !CFX  CNDSNW0=snow conductivity in W/m/C; 
    !CFX  CNDWI=turbulent conductive heat transfer coefficient
    !CFX  in KCAL/DAY-M-C (11.35 Kcal/day-m-C=0.55 W/m-C),
    !CFX  Actually CNDWI is molecular heat diffusivity, Not turbulent!
    !CFX  DEPTHC= 0.1 M, CNDWI = K*11.35, K=1.5 - 2.0!(FANG, 1994)

          FAKW = 1.0
    !CFX  FAKW is a factor to increase dT/dz at z=DEPTHC and 
    !CFX  to finally obtain dT/dz at z=0 (ice-water interface).

          READ(7,*)
          READ(7,*)          
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*) BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW
          
          WRITE(199,*) "BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW"
          WRITE(199,*)BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW
    !CFX   BT-Absorption  coefficient, ALF-Reflectivity coefficient
    !CFX   GM-Attentuation coefficent for ICE, SNOW

          READ(7,*)
          READ(7,*)
          READ(7,*)          
          READ(7,*) THICKIS,THICKSN
          
          WRITE(199,*) "THICKIS,THICKSN"
          WRITE(199,*)THICKIS,THICKSN
    !CFX   Initial ice and snow thickness - THICKIS, THICKSN (M)

          THSNOW=THICKSN
          THICE=THICKIS
          NSWSTAR=0

         READ(7,*)
         READ(7,*)
         READ(7,*)
         READ(7,*)  (Z(I),I=1,MBOT)
         
         WRITE(199,*)"(Z(I),I=1,MBOT)"
         WRITE(199,*)(Z(I),I=1,MBOT)
         
         READ(7,*)
         READ(7,*)
         
         READ(7,*)  (T2(I),I=1,MBOT1)
         
         WRITE(199,*) "(T2(I),I=1,MBOT1)"
         WRITE(199,*)(T2(I),I=1,MBOT1)
    !Clj** Initial conditions of depths (Z), temperature (T2)


    !Clj** Initial conditions of Suspended solids concentration (C2),
    !Clj** Total dissolved solids concentration (CD2), Chlorophyll-a (CHLA2)      
         READ(7,*)
         READ(7,*)
         READ(7,*)  (C2(I),I=1,MBOT1)
         
         WRITE(199,*) "(C2(I),I=1,MBOT1)"
         WRITE(199,*)(C2(I),I=1,MBOT1)
         
         READ(7,*)
         READ(7,*)
         READ(7,*)  (CD2(I),I=1,MBOT1)
         
         WRITE(199,*) "(CD2(I),I=1,MBOT1)"
         WRITE(199,*)(CD2(I),I=1,MBOT1)
        
         READ(7,*)
         READ(7,*)
         
          DO 100 K=1,NCLASS
          
             READ(7,*)  (CHLA2(K,I),I=1,MBOT1)
             
             WRITE(199,*) "(CHLA2(K,I),I=1,MBOT1)"
             WRITE(199,*)(CHLA2(K,I),I=1,MBOT1)
     100  CONTINUE
          
          READ(7,*)
          READ(7,*)
          READ(7,*) IPRNT(4)
          
          WRITE(199,*) "IPRNT(4)"
          WRITE(199,*)IPRNT(4)

    !C20**IPRNT(4) = 0, Simulate temperature only !!
    !C20**IPRNT(4) = 1, Simulate Temp and DO concentration !!!
    	
        IF(IPRNT(4).EQ.1) THEN
             READ(7,*)
             READ(7,*)
             READ(7,*) (PA2(I),I=1,MBOT1)
             
             WRITE(199,*) "(PA2(I),I=1,MBOT1)"
             WRITE(199,*)(PA2(I),I=1,MBOT1)
             
             READ(7,*)
             READ(7,*)
             READ(7,*) (BOD2(I),I=1,MBOT1)
            
             WRITE(199,*) "(BOD2(I),I=1,MBOT1)"
             WRITE(199,*)(BOD2(I),I=1,MBOT1)
    !Clj** Available phosphorus concentration (PA2), Total BOD (BOD2), Dissolved Oxygen (DSO2).
    !C...If there is no initial DO ptofile, DSO2 readings were set to 0.0
             READ(7,*)
             READ(7,*)
             READ(7,*) (DSO2(I),I=1,MBOT1)
             
             WRITE(199,*) "(DSO2(I),I=1,MBOT1)"
             WRITE(199,*)(DSO2(I),I=1,MBOT1)
    
    !C*** INPUT PARAMETERS FOR BIOLOGICAL ROUTINES *****
          
             READ(7,*)
             READ(7,*)
             READ(7,*)
             READ(7,*)
             READ(7,*)
             READ(7,*) BODK20,SB20,XKR1(1),POMAX
             
             WRITE(199,*) "BODK20,SB20,XKR1(1),POMAX"
    !Clj** BODK20 : detrital decay rate (1/day)
    !Clj** SB20   : benthic oxygen demand coefficient
    !Clj** XKR1(1): algal respiration rate in the euphotic zone (1/day)
    !C** POmax  : maximum photosynthesis ratio to control D.O. production
         ENDIF 
             !SHOEB ALAM, MARCH 2010
             READ(7,*)
             READ(7,*)
             READ(7,*)
             READ(7,*)
             READ(7,*)
             READ(7,*) EMCOE(2),EMCOE(1),EMCOE(4),EMCOE(5)
            
             WRITE(199,*) "EMCOE(2),EMCOE(1),EMCOE(4),EMCOE(5)"
             WRITE(199,*) EMCOE(2),EMCOE(1),EMCOE(4),EMCOE(5)
 
 !Fang, 2009
    !C** EMCOE(1) is a calibration factor for diffusion coefficient in the Metalimnion !!!
    !C** EMCOE(4) is a calibration factor for diffusion coefficient in the Hypolimnion !!!
     !Shoeb Alam, March 2010 EMCOE(5) is a calibration factor - a multiplier in hypolimnion chla value
     !EMCOE(5) = 2.0 means Hypolimnion Chl-a is double of Epilimnion Chl-a value!!!         
             
                  
    !        READ(7,*) EMCOE(2),EMCOE(3)
    !        WRITE(199,*)EMCOE(2),EMCOE(3)
    !CFX
    !C** EMCOE(2) is a multiplier to increase SOD below the euphotic Zone
    !C** EMCOE(3) is a constant for computing the kw from CZS/Zs - Move to another line above

        
    
    !Fang April 16, 2009 - SET uniform initial profile
          IF(IPROFILE.EQ.1) THEN
             
             DO IPRO=2,MBOT
              T2(IPRO)=T2(1)
              C2(IPRO)=C2(1)
              CD2(IPRO)=CD2(1)
              
              DO K=1,NCLASS
                CHLA2(K,IPRO)=CHLA2(K,1)
              ENDDO
     
              IF(IPRNT(4).EQ.1) THEN
               PA2(IPRO)=PA2(1)
               BOD2(IPRO)=BOD2(1)
               DSO2(IPRO)=DSO2(1)
              ENDIF
             
             ENDDO
            
          ENDIF
   
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*) IPRNT(2),IPRNT(5),IPRNT(6)
          
          WRITE(199,*) "IPRNT(2),IPRNT(5),IPRNT(6)"
          WRITE(199,*)IPRNT(2),IPRNT(5),IPRNT(6)
    
    !C20**IPRNT(2) = 0, Do not create outflow file !!
    !C20**IPRNT(2) = 1, CREATE outflow file !!"tape8.ou"
    !C20**IPRNT(5) = 0, Do not create PLOT file !!
    !C20**IPRNT(5) = 1, CREATE PLOT file !!"TAPE8.PL"
    !C20** IPRNT(6) is the number of depths PLOT(I) where simulated
    !C20** results will be saved. IPRNT(6) <= 10
        
    !     WRITE(*,1006)
    !1006 FORMAT(' OUTFLOW FILE TO BE CREATED  (Y/N) ?    ',\)
    !     READ(*,'(A)') X
    !     IF(X.EQ.'Y' .OR. X.EQ.'y') THEN
    !       IPRNT(2)=1

          IF(IPRNT(2).EQ.1) THEN
            TAPE1=TAPE8
            T1(II8+2)='O'
            T1(II8+3)='U'
            CALL MAKEFILE(PATH,T1,FFILE)
            OPEN(2,FILE=FILENAME,STATUS='unknown')
          ENDIF

    !     WRITE(*,1000)
    !1000 FORMAT(' PLOT FILE TO BE CREATED  (Y/N) ?       ',\)
    !     READ(*,1001) X

          IF(IPRNT(5).EQ.1) THEN
            TAPE1=TAPE8
            T1(II8+2)='P'
            T1(II8+3)='L'
            CALL MAKEFILE(PATH,FILE1,FFILE)
            OPEN (1,FILE=FILENAME,ACCESS='DIRECT',STATUS='NEW',RECL=4)
            IREC=1

    !       WRITE(*,1004)
    !1004 FORMAT(' ENTER UP TO 10 DEPTHS TO BE SAVED ',/,
    !    + ' END WITH A CHARACTER (#,#,#,...,X) :  ',\)
           
            READ(7,*) (PLOT(I),I=1,IPRNT(6))
            
            WRITE(199,*) "(PLOT(I),I=1,IPRNT(6))"
            WRITE(199,*)(PLOT(I),I=1,IPRNT(6))
    !Clj** PLOT : a list of depths to be saved on the plot data file.

            WRITE(1,REC=IREC) REAL(IPRNT(6))
            IREC=IREC+1
            DO 201 I=1,IPRNT(6)
              WRITE(1,REC=IREC) PLOT(I)
     201      IREC=IREC+1
          ENDIF

    !     WRITE(*,1002)
    !1002 FORMAT(' TEMPERATURE SIMULATION ONLY  (Y/N) ?   ',\)
    !     READ(*,1001) X
    !     IPRNT(4)=1
    !     IF(X.EQ.'Y' .OR. X.EQ.'y') IPRNT(4)=0

    !CFX OPEN FILES FOR OUTPUTTING DATA OF D.O. CONTOUR
    !CFX IDNUM(1) -- CONTROL NUMBER FOR OPENING FILES
    !     WRITE(*,1007)
    !1007 FORMAT(' DO or TE CONTOUR FILES TO BE SAVED  (Y/N) ? ',\)
    !     READ(*,1001) X
    !1001 FORMAT(A1)
          
          IF(IPRNT(5).GT.0) THEN
            WRITE(1,REC=IREC) REAL(MONTH)
            IREC=IREC+1
            WRITE(1,REC=IREC) REAL(NM)
            IREC=IREC+1
            WRITE(1,REC=IREC) REAL(MYEAR)
            IREC=IREC+1
            WRITE(1,REC=IREC) REAL(NCLASS)
            IREC=IREC+1
            WRITE(1,REC=IREC) REAL(NITRO)
            IREC=IREC+1
            WRITE(1,REC=IREC) REAL(IPRNT(4))
            IREC=IREC+1
          ENDIF
          
          READ(7,*)
          READ(7,*)
          READ(7,*)
   
   !      READ(7,*) NPRINT,INFLOW,JDY
   !      WRITE(199,*) NPRINT,INFLOW,JDY
   
          READ(7,*) NPRINT,INFLOW
          
          WRITE(199,*) "NPRINT,INFLOW"
          WRITE(199,*) NPRINT,INFLOW
   
    !Clj** NPRINT : interval between days for tabular output
    !Clj** INFLOW : number of inflow and outflow sources in the inflow data
          READ(7,*)
          READ(7,*)
          READ(7,*)
          READ(7,*)          
          READ(7,*) IFIELD,ICHLASD,I_NO_FIELD_OUT_PROFILE
  
          WRITE(199,*) "IFIELD,ICHLASD,I_NO_FIELD_OUT_PROFILE"
          WRITE(199,*)IFIELD,ICHLASD,I_NO_FIELD_OUT_PROFILE
    
    !C20** Whether or not you have field and/or chl-a data 1=YES, 0=NO             
    !C20** If IFIELD = 1, You have measured field data - measured tempearture or other profiles for comparison
    !C20** IF IFIELD = 0, NO field data or profiles for comparison
   
    !C20** ICHLASD = 0 is NO chlorophyll-a field data - using general pattern and annual mean Chl-a concentration
    !Fang, 2008.  Retesting use original Chlorophyll-a data
    !C20** ICHLASD = 1, YES, having chl-a field data with general pattern
    !C20** ICHLASD= 2, YES, having and using chl-a field data ONLY (step function, 1994)
    
    !For for virtual or regional lakes, input one line data is enough because Chla and SD do not change
    !This happens when IFIELD = 0 and any ICHLASD = 0 or > 0
   
    !Typically, there are lots of SD measurments even there is no measured pofiles
    !by setting ICHLASD = -1 any integer less than 0, < 0
    !When EMCOE(3) = 1.84 > 0.0, you can determine XK1 year by year if user can give mean SDY(IK) year by year
                      
    !Shoeb, 2009*** I_NO_FIELD_OUT_PROFILE = 1, no field dat but still want to output simulated profiles at some days
    !Shoeb, 2009*** I_NO_FIELD_OUT_PROFILE = 0, no output simulated profiles if no field data
    
         
          IF(IDIATOM.EQ.3) READ(7,*) (SI2(I),I=1,MBOT)
          
    !Clj** SI2 : initial silica concentration
          IF(NITRO.EQ. 1) THEN
            READ(7,*) (XNH2(I),I=1,MBOT)
            
            WRITE(199,*) "(XNH2(I),I=1,MBOT)"
            WRITE(199,*)(XNH2(I),I=1,MBOT)
            
    !Clj** 
            READ(7,*) (XNO2(I),I=1,MBOT)
            
            WRITE(199,*)"(XNO2(I),I=1,MBOT)"
            WRITE(199,*)(XNO2(I),I=1,MBOT)
    !Clj** 
          ENDIF

    !C20** NDEPTH is currently equal to or less than 5
    !C20** Extract data at several depths for error analysis
         IF(IFIELD.EQ.1.OR.I_NO_FIELD_OUT_PROFILE.EQ.1) THEN
          READ(7,*)
          READ(7,*)
          READ(7,*) NDEPTH
          
          WRITE(199,*) "NDEPTH"
          WRITE(199,*)NDEPTH
      
     !  If zmax is less than 5 we need only three depths
         READ(7,*)
         READ(7,*)
         READ(7,*)(FDTH(II),II=1,NDEPTH)
         
         WRITE(199,*)"(FDTH(II),II=1,NDEPTH)"
         WRITE(199,*)(FDTH(II),II=1,NDEPTH)
     
     !---------------------------
     !    IF(ZMAX.gt.5.0)then
     !     FDTH(1)=1.0
     !     FDTH(5)=INT(ZMAX-1.0)
          
     !    DO II=2,4
     !       FDTH(II)=FDTH(1)+(II-1)*(FDTH(5)-FDTH(1))/4.0
     !       FDTH(II)=INT(FDTH(II))
     !    ENDDO
     !---------------------------
      
         WRITE(13,317) NDEPTH
         WRITE(14,318) NDEPTH

          
  317    FORMAT(1x,'Output time series of simulated temperature  at ',I2,' depths, mixed layer depth over Hmax') 
  318    FORMAT(1x,'Output time series of simulated  DO at ',I2, ' depths, mixed layer depth over Hmax') 
         WRITE(13,327) (FDTH(II),II=1,NDEPTH)

  327    FORMAT(1X,'The depths (m) are',5(2X,F5.1)) 
         WRITE(14,328)(FDTH(II),II=1,NDEPTH)

  328    FORMAT(1X,'and saturated DO concentration at 1st depth. The depths (m) are',5(2X,F5.1))         
!         else
!----------------------------------------       
!        IF Zmax is less than 5 here is new options
        
 !  2298  NDEPTH=3
 !       FDTH(1)=1.0
 !       FDTH(2)=2.0
 !       FDTH(3)=3.0
 !       WRITE(13,319)
 !       WRITE(14,320)
         
 !319    FORMAT(1x,'Output time series of simulated temperature  at three depths, mixed layer depth over Hmax') 
         
 !320    FORMAT(1x,'Output time series of simulated  DO at three depths, mixed layer depth over Hmax') 
 !        WRITE(13,329) (FDTH(II),II=1,NDEPTH)
 !329    FORMAT(1X,'The three depths (m) are',3(2X,F5.1))    
 !        WRITE(14,330) (FDTH(II),II=1,NDEPTH)
 !330    FORMAT(1X,'and saturated DO concentration at 1st depth. The three depths (m) are',3(2X,F5.1))          
 !        ENDIF
 !        ELSE
        
 !2300  READ(7,*)
       ENDIF
!-----------------------------------
        READ(7,*)
        READ(7,*)
    	
           IF(IFIELD.EQ.1.OR.I_NO_FIELD_OUT_PROFILE.EQ.1) THEN
            
    !C20**Number of dates and years which you have field data !
    !C20**NDAYO (local variable) was NDAYS stands for number of data with field data      
            READ(7,*)
            READ(7,*) NDAYO,NYEAR
            
          
            WRITE(199,*) "NDAYO,NYEAR"
            WRITE(199,*) NDAYO,NYEAR
            
            WRITE(56,*)"NDAYO,NYEAR"
            WRITE(56,*) NDAYO,NYEAR
            
            WRITE(56,*)
            WRITE(56,*)"KYEAR(I),NTDY(I)"
            
            NDAYX=0
    !CFX   FOR SPECIAL LAKE SIMULATION -- THRUSH LAKE
    !CFX   FOR EUTROPHIC LAKES -- CHLA(I)=CHLHY(I) EXACTLY
            IF(NDAYO.GT.0) THEN
               II=1
                READ(7,*)
                READ(7,*)
                
                IF(EMCOE(3).GT.0.0.AND.ISD.EQ.2) READ(64,*)
       !__________________________________________________________         
               
                DO 105 I=1,NYEAR
    !Clj** Specific year and corresponding number of dates
                 
                  READ(7,*) KYEAR(I),NTDY(I)
      
                  WRITE(56,*)KYEAR(I),NTDY(I)
      
                  WRITE(199,*) "KYEAR(I),NTDY(I)"
                  WRITE(199,*) KYEAR(I),NTDY(I)
                  NDAYX=NDAYX+NTDY(I)
               
    !Clj** number of month*100+days
                  READ(7,*) (NPRNT(K),K=II,II+NTDY(I)-1)
                 
                  WRITE(56,*)(NPRNT(K),K=II,II+NTDY(I)-1)
                  
                  WRITE(199,*)"(NPRNT(K),K=II,II+NTDY(I)-1)"
                  WRITE(199,*)(NPRNT(K),K=II,II+NTDY(I)-1)
 
    !August 2009 Shoeb Alam -   Following part of program is added (new subroutine MONTHDAY_TO_JULIANDAY) to convert field data date(i,e,516)
    !                           to Julian day (i,e, 516>136) for modifying Chlo-a calculation
                 
                 
                  DO 3005 K=II,II+NTDY(I)-1
                
                  NNPRNT=NPRNT(K)
                  
                  CALL MONTHDAY_TO_JULIANDAY(I,KYEAR(I),K,NNPRNT,JDAY_FIELD(I,K))
                
                
              !   WRITE(*,*)KYEAR(I),NPRNT(K),JDAY_FIELD(I,K)
                  
        3005   CONTINUE
        
    !CFX ** Read Secchi disk depth from field data       
                  IF(EMCOE(3).GT.0.0.AND.ISD.EQ.2) THEN 
                    READ(64,*) (NSDAY(K),K=II,II+NTDY(I)-1)
                    READ(64,*) (SDFIELD(K),K=II,II+NTDY(I)-1)
                    
                  ENDIF
                  
                 II=II+NTDY(I)
                
                                    
     105       CONTINUE
        ENDIF
   
        
                    IF(EMCOE(3).GT.0.0.AND.ISD.EQ.2) THEN 
                             
                            READ(64,*)
                            READ(64,*) NDAYO_CHL,NYEAR_CHL
                            
                             IF(NDAYO_CHL.GT.0) THEN
                             II=1
                             
                                  DO 1050 I=1,NYEAR_CHL
                                              !Clj** Specific year and corresponding number of dates for Chl-a available dates only
                                               
                                               READ(64,*) KYEAR_CHL(I),NTDY_CHL(I)
                                            
                                             ! NDAYX=NDAYX+NTDY(I)
                                             ! Clj** number of month*100+days for Chl-a dates only
                                
                                              READ(64,*) (NPRNT_CHL(K),K=II,II+NTDY_CHL(I)-1)
                                              READ(64,*) (FIELD_CHL(K),K=II,II+NTDY_CHL(I)-1) 
                                            
                                             ! WRITE(*,*) (K,K=II,II+NTDY_CHL(I)-1) 
                                            
                                              KK=0      
                                            
                                              DO 3015 K=II,II+NTDY_CHL(I)-1
                                                KK=KK+1
                                                NNPRNT_CHL=NPRNT_CHL(K)
                                                CALL MONTHDAY_TO_JULIANDAY(I,KYEAR_CHL(I),K,NNPRNT_CHL,JDAY_FIELD_CHL(I,KK))
                                        
                                            3015   CONTINUE
                                                              
                                                       II=II+NTDY_CHL(I)    
                        
                          
                              1050  CONTINUE      
                          
               ! This part is to store available chl-a data (CHL_FIELD(I,J,K) -- CHL_FIELD(1996,247,7)
               ! Here - I:Year, J: Julian day and K: Serial of data, 1,2,3,...           
                                    K=0
                                    DO 1060 II=1,NYEAR_CHL
                                       
                                       
                                              I=KYEAR_CHL(II)
                                      
                                          
                                          DO 1070 JJ=1,NTDY_CHL(II)
                                                   J=JDAY_FIELD_CHL(II,JJ)
                                          
                                         
                                            K=K+1
                                         
                                                     
                                            CHL_FIELD(I,J,K)=FIELD_CHL(K)
                                        !   WRITE(800,*) K,I,J
                                                                                                                                                                        
                                          1070 CONTINUE      
                              1060  CONTINUE        
                            
                     ENDIF    
           
               
           !___________________________________________________________________________________
           !Sep 2009 Shoeb Alam :  Interpolation of Chla Data on the available dates 
           !
                   !K values only change when field data are available, interpolated values will contain the same K values of the last data
                   K=0
                   DO 1210 II=1,NYEAR_CHL
                           
                   I=KYEAR_CHL(II)
                   DO 1220 JJ=1,NTDY_CHL(II)
                       K=K+1      
                             J=JDAY_FIELD_CHL(II,JJ)
                           
                   
                       IF(NTDY_CHL(II).EQ.1)GO TO 1210
                   
                   
                   !______________INTERPOLATION PART_________________
                   
                  DO 1230 LL=JDAY_FIELD_CHL(II,JJ)+1,JDAY_FIELD_CHL(II,JJ+1)-1
                          
                           LL1=JDAY_FIELD_CHL(II,JJ)
                           LL2=JDAY_FIELD_CHL(II,JJ+1)
                           LLL=JDAY_FIELD_CHL(II,JJ+1)
                     
                     
                           CHL_FIELD(I,LL,K)=CHL_FIELD(I,J,K)+ (CHL_FIELD(I,LLL,K+1)-CHL_FIELD(I,J,K))*(LL-JDAY_FIELD_CHL(II,JJ))/(LL2-LL1)
       
           
                1230 CONTINUE   
                   
               1220 CONTINUE    
                   
             1210 CONTINUE
           
           
                    
           
          ENDIF
   !Interpolation done     
         
         
           IF(NDAYX.NE.NDAYO) THEN
            WRITE(*,*) 'Number of days with field data NOT EQUAL TO Sum (FieldDataDay in each year)'
            STOP
           
           ENDIF

    !Keep NYEAR information, reset NDAYOX for actual number of field data!
          NYEARX=NYEAR
          NDAYOX=0
    

 

            IF(IFIELD.EQ.1.AND.NDAYO.GT.0) THEN    
                  
             READ(7,*)
 	                 
    !Read field data of Chlorophyll-a concentration in mg/l
    !in epilimnion CHLEP(I), and hypolimnion CHLHY(I) 
             READ(7,*)
             READ(7,*) (CHLEP(I),I=2,NDAYO+1)
             
             WRITE(56,*)
             WRITE(56,*) 'Chlorophyll-a concentration in the epilimnion'
             WRITE(56,*)
             WRITE(56,563)(CHLEP(I),NPRNT(I-1),I=2,NDAYO+1)
              
             READ(7,*)
             READ(7,*) (CHLHY(I),I=2,NDAYO+1)


    !Shoeb 2010 Double chla at hypolimnion
    !WRITE (*,*)'Double chla at hypolimnion ? 1 for yes or 0 for no'
    !WRITE (*,*)'Chla at hypolimnion? Iinput the multiplier, 1 for same as Epilimnion, >1 for higher values'
    !WRITE (*,*)'1 for same as Epilimnion, >1 for higher values'
    !READ(*,*)DOUBLE_CHLA
        
    !IF(DOUBLE_CHLA.NE.1)THEN
         
              DO I=2,NDAYO+1
             
               CHLHY(I)=EMCOE(5)*CHLHY(I)
 
              ENDDO
    !NDIF

             WRITE(56,*)             
             WRITE(56,*) 'Chlorophyll-a concentration in the hypolimnion'
             WRITE(56,*)
             WRITE(56,563)(CHLHY(I),NPRNT(I-1),I=2,NDAYO+1)
    560      FORMAT(1X,8F7.4)         
    563      FORMAT(1X,F7.4,3X,I4)
                       
    !Compute average SDY from chlorophyll-a concentration
    !In order to determine initial sediment temperature profile
             IW=1
             IXY=1
             SDAVG=0.0
             TOPCHL=0.0
             BOTCHL=0.0
              
   !Compute mean chlorophyll-a concentration using all available data in the past
             MYEAR1=MYEAR
   !Turn off this statement, you will not use the chlorophyll-a data BEFORE the first simulation year          
             IF(MYEAR.GT.KYEAR(1)) MYEAR1=KYEAR(1)
   
   !Compute mean chlorophyll-a concentration using all available data EVENT the future ones         
             FYEAR1=FYEAR
   !Turn off this statement, you will not use the chlorophyll-a data AFTER the last simulation year           
             IF(FYEAR.LT.KYEAR(NYEARX)) FYEAR1=KYEAR(NYEARX)

        DO 145 IX=MYEAR1,FYEAR1
    
    !   DO 145 IX=MYEAR,FYEAR
    !SDY(IXY) will be zero for those years without field data First!
    !CHLTOP is annual mean Chlorophyll-a cconcentration in eplimnion;
    !CHLBOT is annual mean Chlorophyll-a cconcentration in Hypolimnion;
           SDY(IXY)=0.0
           CHLTOP(IXY)=0.0
           CHLBOT(IXY)=0.0

           IF(IX.EQ.KYEAR(IW)) THEN
           
             IF(IW.EQ.1) THEN
           !Record IX for the next step     
               IXY2=IXY
               IXS2=2
	           IXE3=NTDY(IW)+1
              ELSE
               IXS2=IXE3+1
               IXE3=IXS2+NTDY(IW)-1
              ENDIF
              
               NDAYOX=NDAYOX+NTDY(IW)
              	   	   
              DO 175 IXK=IXS2,IXE3
                      
               SDY(IXY)=1.84/(XK1+XK2*CHLEP(IXK))+SDY(IXY)
               CHLTOP(IXY)=CHLTOP(IXY)+CHLEP(IXK)
               CHLBOT(IXY)=CHLBOT(IXY)+CHLHY(IXK)
     175	  CONTINUE 
   
    !DAVG, TOPCHL, BOTCHL are AVERAGE Secchi dish depth, eplimion
    !and hypolimnion chlorophyll-a concentration for all available data.
               
	         SDAVG=SDAVG+SDY(IXY)
             TOPCHL=TOPCHL+CHLTOP(IXY)
             BOTCHL=BOTCHL+CHLBOT(IXY)
    		 
             SDY(IXY)=SDY(IXY)/FLOAT(NTDY(IW))
             CHLTOP(IXY)=CHLTOP(IXY)/FLOAT(NTDY(IW))
             CHLBOT(IXY)=CHLBOT(IXY)/FLOAT(NTDY(IW))
             IW=IW+1

             ENDIF
             IXY=IXY+1
    145   CONTINUE

    !SDY(IXY)=0 for years without field data will be SET as average SD,
    !chlorophyll-a concentrations of those years with field data.
    !This is true for use all chlorophyll-a data
    
    !You may set NDAYO = actual number of field data for chlorophyll-a,
    !then the model would be the same as previous model with different input files.
        
            SDAVG=SDAVG/FLOAT(NDAYOX)
            TOPCHL=TOPCHL/FLOAT(NDAYOX)
            BOTCHL=BOTCHL/FLOAT(NDAYOX)
              
            IXY=1
            
            WRITE(56,*)
            WRITE(56,*)"CHLTOP - Annual mean chlorophyll-a concentration in epilimnon"
            WRITE(56,*)"CHLBOT - Annual mean chlorophyll-a concentration in hypolimnon"
            WRITE(56,*)"SDY - Annual mean Secchi Depth from mean epilimnion chlorophyll-a concentration"
            WRITE(56,*)"IX,IXZ,CHLTOP(IX),CHLBOT(IX),SDY(IX)"
                    
             DO 146 IX=MYEAR1,FYEAR1
              IF(SDY(IXY).EQ.0) SDY(IXY)=SDAVG
              IF(CHLTOP(IXY).EQ.0) CHLTOP(IXY)=TOPCHL
              IF(CHLBOT(IXY).EQ.0) CHLBOT(IXY)=BOTCHL
              IXY=IXY+1
              
              IXZ=IX-MYEAR1+1
             WRITE(56,562) IX,IXZ,CHLTOP(IXZ),CHLBOT(IXZ),SDY(IXZ)
    562      FORMAT(1X,I4,2X,I2,2X,F7.4,2X,F7.4,2X,F7.4)  
    146      CONTINUE
    
    	     WRITE(56,*)
    	     WRITE(56,*) "Actually used chlorophyll-a concentrations:"
    	     WRITE(56,*) "Year Month Day ChlTop, ChlBot (mg/L), SD (m)"
             
   !Reset chlorophyll-a for slected year simulations
               IXY=1
            DO 1460 IX=MYEAR,FYEAR
              
              IXZ=IXY2+(IX-KYEAR(1))
              CHLTOP(IXY)=CHLTOP(IXZ)
              CHLBOT(IXY)=CHLBOT(IXZ)
              SDY(IXY)=SDY(IXZ)
    
              WRITE(56,564) IX,IXY,IXZ,CHLTOP(IXY),CHLBOT(IXY),SDY(IXY)
    564       FORMAT(1X,I4,2X,I2,2X,I2,2X,F7.4,2X,F7.4,2X,F7.4)           
              IXY=IXY+1
     
    1460     CONTINUE
    
    	     WRITE(56,*)
    
         ENDIF

        ENDIF
      
    !If there is not field data for Temp/DO wether you set
    !yes or no for Chla data or (2)if you have field data
    !for Temp/DO, but there is no field data for Chla.
       
        IF(IFIELD.NE.1.OR.ICHLASD.GT.2) THEN
        
    !    IF(IFIELD.NE.1) THEN 
              
    !If there are no field data for chlorophyll-a, you input an 
    !annual mean chl-a concentration year by year (CHLMEAN), and
    !use a default seasonal pattern, SDY - Secchi Disk Depth
    !FOR EVERY YEAR!
    
    !Xing Fang 2010 - The following four lines are in input file for no field data 
    !NO FIELD DATA  or WITH and want to use annual measured SD - secchi depths
    !If only there is not field data, you input the following data lines.
    !CHLMEAN(IK),SDY(IK)--CHLEMEAN - annual mean chl-a concentration year by year, SDY -Secchi Disk Depth, one line data for each year
    !(IK=1,NYTOT, NYTOT is total number of simulation years = FYEAR - MYEAR + 1, number of data lines has to be equal to NYTOT 	
            READ(7,*)
            READ(7,*)
            READ(7,*)
            READ(7,*)
    
    !For for virtual or regional lakes, input one line data is enough because Chla and SD do not change
    !SET IFIELD = 0 (NOT 1) AND ICHLASD = 0 or any positive integer
    
    !If you set ICHLASD < 0, the program will read annual mean chlorophyll-a and SDY year by year
    !by setting ICHLASD = -1 any integer less than 0, < 0
    !When EMCOE(3) = 1.84 > 0.0, you can determine XK1 year by year if user can give mean SDY(IK) year by year
    
            IF(ICHLASD.GT.3) THEN
             NYTOTX=ICHLASD
            ELSE
             NYTOTX=1
            ENDIF
                           
            WRITE(199,*) "CHLMEAN(IK),SDY(IK)"
            WRITE(56,*) "CHLMEAN(IK),SDY(IK)"
            
           IF(NYTOTX.EQ.1) THEN
             READ(7,*) CHLMEAN(1),SDY(1)
             
             DO 1473 IK=2,NYTOT
              CHLMEAN(IK)=CHLMEAN(1)
              SDY(IK)=SDY(1)
      1473   CONTINUE
             
             WRITE(199,*)CHLMEAN(NYTOT),SDY(NYTOT),MYEAR,FYEAR
             WRITE(56,*)CHLMEAN(NYTOT),SDY(NYTOT),MYEAR,FYEAR
             
           ELSE
     
     !If there is no Chl_mean and annual mean SD values
             SUMCHLMEAN=0.0
             SUMSDY=0.0
             
            DO 147 IK=1,NYTOTX
             READ(7,*) CHLMEANX(IK),SDYX(IK),NYEARSD(IK)
             
             WRITE(199,*)CHLMEANX(IK),SDYX(IK),NYEARSD(IK)
             
             SUMCHLMEAN=SUMCHLMEAN+CHLMEANX(IK)
             SUMSDY=SUMSDY+SDYX(IK)
     147	CONTINUE
            
            AVGCHLMEAN=SUMCHLMEAN/FLOAT(NYTOTX)
            AVGSDY=SUMSDY/FLOAT(NYTOTX)
            
            IXSDY=1
     1474   IF(NYEARSD(IXSDY).LT.MYEAR) THEN
             IXSDY=IXSDY+1
             GOTO 1474
            ENDIF
            
              NYEARSD(ICHLASD+1)=0
              
              IKY=1           
            DO 1475 IK=MYEAR,FYEAR
             IF(NYEARSD(IXSDY).EQ.IK) THEN    
              CHLMEAN(IKY)=CHLMEANX(IXSDY)
              SDY(IKY)=SDYX(IXSDY)
              IXSDY=IXSDY+1
             ELSE
              CHLMEAN(IKY)=AVGCHLMEAN
              SDY(IKY)=AVGSDY
             ENDIF
              
              WRITE(199,*)CHLMEAN(IKY),SDY(IKY),IK
              WRITE(56,*)CHLMEAN(IKY),SDY(IKY),IK
               
              IKY=IKY+1
              
     1475	CONTINUE
     
           ENDIF
        ENDIF
        
    !Reset NYEAR = 1 STARTING AGAIN FOR CHECKING FIELD DATA!     
          NYEAR=1
          NSKIPDAY=0
          NDAYS=1
          ISY=1    
        
    ! FANG JUNE 2009 -- Determine skip the field data if the starting year is after the year with field data
    ! MSKIPY = No. of skip year
    ! NSKIPDAY = No. of skip field dates  
        
     IF(KYEAR(1).LT.MYEAR) THEN
             
             MSKIPY=0
             DO MSY=KYEAR(1),(MYEAR-1)
               IF(MSY.EQ.KYEAR(ISY)) THEN
                MSKIPY=MSKIPY+1
                ISY=ISY+1
               ENDIF
             ENDDO 
             
             DO ISK=1,MSKIPY
              NSKIPDAY=NSKIPDAY+NTDY(ISK)
             ENDDO
            
             NYEAR=NYEAR+MSKIPY
             NDAYS=NDAYS+NSKIPDAY
          
    ENDIF
 
    !FANG 2009 - SKIP FIELD DATA
    !IDAYSKIP - Skip number of days within the same year
       IF(IFIELD.EQ.1) THEN
       
          IDAYSKIP=0
          
          IF(NSKIPDAY.EQ.0) GOTO 679
             
  678     IF(ICOUNTER.LT.1) NSKIP=0
        
          DO I=1,120
           DEPTH(I)=-1.0
          ENDDO
                 
         IF(ICOUNTER.LT.1) READ(7,*)
         IF(ICOUNTER.LT.1) READ(7,*)
          
          READ(7,*) NF,NPRFLE
           
          WRITE(199,*) "NF,NPRFLE,NSKIP,NSKIPDAY,NDAYS"
          WRITE(199,*) NF,NPRFLE,NSKIP,NSKIPDAY,NDAYS
  
            If(ICOUNTER.LT.1) READ(7,*)
            READ(7,*) SD
           
            WRITE(199,*) "SD"
            WRITE(199,*) SD
                 
            IF(ICOUNTER.LT.1) READ(7,*)
            READ(7,*) (NFLD(I),I=1,NPRFLE)
           
            WRITE(199,*) "(NFLD(I),I=1,NPRFLE)"
            WRITE(199,*) (NFLD(I),I=1,NPRFLE)
            
            IF(ICOUNTER.LT.1) READ(7,*)
            READ(7,*) (DEPTH(I),I=1,NF)
           
            WRITE(199,*)"(DEPTH(I),I=1,NF)"
            WRITE(199,*)(DEPTH(I),I=1,NF)
            
            IF(ICOUNTER.LT.1) READ(7,*)
            
            DO I=1,NPRFLE
              READ(7,*) (FLDATA(NFLD(I),J),J=1,NF)
             
              WRITE(199,*) "(FLDATA(NFLD(I),J),J=1,NF)" 
              WRITE(199,*) (FLDATA(NFLD(I),J),J=1,NF)
            ENDDO
            
             IF(ICOUNTER.LT.1) READ(7,*)
           
             IF(ICOUNTER.LT.1) ICOUNTER=2
             
             NSKIP=NSKIP+1
             
             IF (NSKIPDAY.GT.NSKIP)     THEN 
                GOTO 678
             ENDIF
             
     679     IF(KYEAR(1).LE.MYEAR .AND. KYEAR(NYEARX).GE.MYEAR .AND. (MONTH*100+ISTART).GT.NPRNT(NDAYS)) THEN
                IDAYSKIP=IDAYSKIP+1
                NTDY(ISY)=NTDY(ISY)-1
                IF (NTDY(ISY) .LT. 0) GOTO 680
                NDAYS=NDAYS+1               
                WRITE(199,*) "NDAYS,NSKIPDAY,IDAYSKIP,NTDY(ISY),ISY"
                WRITE(199,*) NDAYS,NSKIPDAY,IDAYSKIP,NTDY(ISY),ISY
                GOTO 678 
    680      ENDIF
         
       ENDIF
     
  
     !FANG/SHOEB Sep 2009 - INTERPOLATION INITIAL PROFILE
     !This part will interpolate the field data to work as initial profile
     !      
           IF(NDAYS.GT.1) THEN
        
            IF((MONTH*100+ISTART).GE.NPRNT(NDAYS-1)) THEN
      
           MONTH=NPRNT(NDAYS-1)/100
           ISTART=NPRNT(NDAYS-1)-MONTH*100
           ISTART=ISTART+1
   
   
    
           DO 1000 J=1,MBOT
                  
                     DO  2000 I=1,NF
                         
                      IF(Z(J).EQ.DEPTH(I))THEN
                                           
                     DSOINT(J)=FLDATA(6,I) 
                     TEMPINT(J)=FLDATA(1,I)
                           
                     GO TO 1000
                     ENDIF
                       IF(Z(J).GT.DEPTH(I).AND.Z(J).LT.DEPTH(I+1))THEN
                           DSOINT(J)=FLDATA(6,I)-((Z(J)-DEPTH(I))/(DEPTH(I+1)-DEPTH(I)))*(FLDATA(6,I)-FLDATA(6,I+1))
                      TEMPINT(J)=FLDATA(1,I)-((Z(J)-DEPTH(I))/(DEPTH(I+1)-DEPTH(I)))*(FLDATA(1,I)-FLDATA(1,I+1))
                       ENDIF
              2000   CONTINUE
                     
                     IF(Z(J).GT.DEPTH(NF))THEN
                     DSOINT(J)=DSOINT(J-1)
                     TEMPINT(J)=TEMPINT(J-1)
                     ENDIF
    1000   CONTINUE
                    
                         
!                   WRITE(1200,*)
!                   WRITE(1200,*)'FLD DEPTH'
!                   WRITE(1200,1201)(DEPTH(I),I=1,NF)
                   
!                   WRITE(1200,*)
!                   WRITE(1200,*)'TEMPERATURE DATA'
!                   WRITE(1200,1201)(FLDATA(1,I),I=1,NF)
                  
!                   WRITE(1200,*)
!                   WRITE(1200,*)'DO DATA'
!                   WRITE(1200,1201)(FLDATA(6,I),I=1,NF)
                  
!                   WRITE(1200,*)
                  
!                   WRITE(1200,*)'LAYER DATA'
                  
!                   WRITE(1200,1201)(Z(I),I=1,MBOT)
                   
!                   WRITE(1200,*)
!                   WRITE(1200,*)'DO INTERP'
!                   WRITE(1200,1201)(DSOINT(I),I=1,MBOT)
!                   WRITE(1200,*)
!                   WRITE(1200,*)'TEMP INTP'
!                   WRITE(1200,1201)(TEMPINT(I),I=1,MBOT)
!           1201    FORMAT (10(F6.3,1X))
            
            
                  DO I=1,MBOT
                   T2(I)=TEMPINT(I)
                   DSO2(I)=DSOINT(I)
                   END DO
    
   
            
            ENDIF
           ENDIF
              
    !C2002** ALWAYS READ the chlorophyll-a seasonal distribution!!
!             FILE45="chl_a.dat"
!             CALL MAKEFILE(PATH_F,FILE45,FFILE)
             OPEN (45,FILE="..\#COMMON\FIXED_INPUT\chl_a.dat")    
         
           
      !      OPEN(45,FILE="chl_a.dat")
      
             READ(45,*)
             READ(45,*)
             DO 466 IK=1,37
               READ(45,*) NCDAY(IK,1),GCHLA(IK,1),NCDAY(IK,2),GCHLA(IK,2)
     466     CONTINUE
             CLOSE(45)

    !  THIS INITIAL CONDITION IS VALID FOR JAN-APRIL (MINIMUM BOTTOM TEMP)
    !  AND AUGUST-OCTOBER (MAXIMUM BOTTOM TEMPERATURE)  
    !  ZSL - DEPTH FROM THE LAKE BOTTOM

    !CFX USING SOME EXPONENTIAL FUNCTION TO SET INITIAL SEDIMENT TEMPERATURE
    !CFX ETAB - coefficient for EXP function, T2MBOT - initial bottom water
    !CFX temperature (3.42 C). TSLMEAN - annual mean sediment temperature.
    !CFX  TSLP(I) is water temperature at previous time step. 

    !CFX*** JULIANDAY is the Julian day of first simulation as constant
    !     JULIANDAY=JDY
        
    !With field data CHLEP(2) and CHLHY(2) are for the first date!
          CHLEP(1)=CHLA2(1,1)      
          CHLHY(1)=CHLEP(1)
          SD=SDY(1)      

    !C20** Find ASX=ATOP(1) surface area
    	
        CALL LAKE(ZMAX,ASX,0,1)

    !C	ASX - AREA IN M**2
          RATIOX=(ASX**0.25)/ZMAX
    	      
    !C... Set initial T & DO profiles if NO corresponding input data 
          T_FLAG = 0        
        IF(IPRNT(4).EQ.1 ) DO_FLAG = 0

    !C... Assume no given initial T (T_FLAG=0) & no given initial DO (DO_FLAG=0)
    !C... MONBAK,DAYBAK,YEARBAK store the starting simulation data
        MONBAK = MONTH
        DAYBAK = ISTART
        YEARBAK= MYEAR
   
    !C... TBAK,DOBAK store the input data of initial T & DO
        DO I = 1,MBOT
           TBAK(I) = T2(I)
    !         WRITE(1400,14001)I,TBAK(I)
    !14001    FORMAT(I4,F6.3)  
           IF(T2(I).NE.0.0) T_FLAG = 1
             IF( IPRNT(4).EQ.1) THEN
              DOBAK(I) = DSO2(I)
    !        WRITE(1400,14002)I,I,DOBAK(I)
    !14002   FORMAT(I4,2X,I4,2X,F6.3) 
              IF(DSO2(I).NE.0.0) DO_FLAG = 1
           ENDIF
        ENDDO

    !C... Output the information on input T&DO	
    !C... Reset the starting time of simulation if no inputted T & do 
          IF(T_FLAG.EQ.1.AND.(DO_FLAG.eq.0.and.iprnt(4).eq.1))WRITE(99,*)"power(As,0.25)/Hmax = ",RATIOX
              
          WRITE(99,*)
          WRITE(99,*)"The input simulation start time ",month,istart,myear
         IF((T_FLAG.EQ.0).OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0.AND.RATIOX.LT.5)) THEN
          MONTH = 1
          ISTART = 1
          MYEAR = MYEAR-1
    
    !C... Julian day must be reset
          JDY = 1
            IF(T_FLAG.EQ.0) THEN
              IF(IPRNT(4).EQ.1.and.DO_FLAG.EQ.0) THEN
                WRITE(198,1982)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK
                WRITE(99,1982)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK
              ELSE
                WRITE(198,1983)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK, &
                             MONBAK,DAYBAK,YEARBAK
                WRITE(99,1983)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,  &
                             MONBAK,DAYBAK,YEARBAK
              ENDIF
         ELSE
              IF(IPRNT(4).EQ.1.and.DO_FLAG.EQ.0) WRITE(198,1984)MONTH, &
                 ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,MONBAK,DAYBAK,YEARBAK
              IF(iprnt(4).eq.1.and.DO_FLAG.EQ.0) WRITE(99,1984)MONTH, &
                  ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,MONBAK,DAYBAK,YEARBAK
         ENDIF
         
     1982 FORMAT(1X,/,1X,'Since initial T (temperature) and DO ', &
              '(dissolved oxygen) is ',/,1X,'not available,', &
              ' the simulation actually started on ',I2,'/',I2,'/',I4,/,1X, &
              'in order to find appropriate T and DO profile for ',&
              I2,'/',I2,'/',I4,'.') 
     1983 FORMAT(1X,/,1X,'Since initial T (temperature) is not available,',/ &
           ,1X,'the simulation actually started on ',I2,'/',I2,'/',I4,/,1X,&
              'in order to find appropriate T profile for ',I2,'/',I2,'/',&
          I4,'.',/,1X,'Given initial DO (Dissolved Oxygen) profile on ',&
           I2,'/',I2,'/',I4,' was used.') 
     1984 FORMAT(1X,/,1X,'Since initial DO (dissolved oxygen) is not',&
                   ' available,',/,1X, &
              'the simulation actually started on ',I2,'/',I2,'/',I4,/,1X,&
             'in order to find appropriate DO profile for ',I2,'/',I2,'/',&
           I4,'.',/,1X,'Given initial T (temperature) profile on ',&
           I2,'/',I2,'/',I4,' was used.') 
            WRITE(99,*)" The beginning simulation YEAR = ", MYEAR
            WRITE(99,*)" ERROR ON METEOROLIGICAL DATA FILE NAME.!"
            WRITE(99,*)" Please give the correct MTHDATA filename!"
         
          IF(MYEAR.LT.1961) THEN
            WRITE(99,*)"No Temperature & DO DATA available before 1961!"
            STOP
          ENDIF
         
            NYTOT = FYEAR - MYEAR + 1
          IF(NYTOT.EQ.1) THEN
             NM = FMON - MONTH + 1
             NMFIN = NM
          ELSE
             NM = 13 - MONTH
             NMFIN = FMON
          ENDIF
        ENDIF
    	   
    !C... Select the Elevation based on the given state and station
          SELEV = ELEV(ISTATE,ISTATION)

    !C... Set initial T profile to be half of mean annual local air temperature
        IF((T_FLAG.EQ.0).OR.(IPRNT(4).EQ.1.AND.DO_FLAG.EQ.0.AND.RATIOX.LT.5)) THEN
         	
           TSLMN = YRAIR(ISTATE,ISTATION)
    !C20** Why YTAIR ---  TSLMN = YRAIR(ISTATE,ISTATION)+YTAIR
           TSLXX = TSLMN/2.0
           IF(TSLXX.LT.4.0) TSLXX = 4.0
           DO I = 1,MBOT
              T2(I) = TSLXX
           ENDDO
          WRITE(99,*)"The estimated T on start moment : "
          WRITE(99,*)(t2(i),i=1,MBOT)
        ENDIF

    !C... If Initial T OK & power(As,0.25)/HMAX>= 5, DO will be computed from  
    !C... the mean Temperature of inputted T.
        IF((T_FLAG.EQ.1).AND.(IPRNT(4).EQ.1.AND.DO_FLAG.EQ.0.AND.RATIOX.GE.5)) THEN
         
           T2X = 0.0
           T2MAX = 0.0
           T2MIN = 100.0
           DO I = 1,MBOT
              T2X = T2X + T2(I)
              IF(T2MAX.LT.T2(I)) THEN
	             T2MAX = T2(I)
                 IMAX  = I
              ENDIF
              IF(T2MIN.GT.T2(I)) THEN
	             T2MIN = T2(I)
                 IMIN  = I
              ENDIF
           ENDDO
             T2X = T2X/MBOT
           WRITE(99,1981)T2MAX,IMAX,T2MIN,IMIN,T2X,T2MAX-T2X,T2MIN-T2X
           WRITE(99,1985)MONTH,ISTART,MYEAR
           WRITE(198,1985)MONTH,ISTART,MYEAR
1981      FORMAT(/,3X,'The initial input Temperature : ',&
          /,6X,'Maximum T = ',F6.2,1X,'at',I4,' layer',&
              /,6X,'Minimum T = ',F6.2,1X,'at',I4,' layer',&
              /,6X,'Mean    T = ',F6.2,&
              /,6X,'Maximum standard derivation : ',f6.2,&
              /,6X,'Minimum standard derivation : ',f6.2)
!1985     FORMAT(/,1X,'Since this lake is more or less well mixed, 'the initial Dis Oxy profile',/,1X, &
!                'was estimated for given', 'initial T (temperature) profile',/,1X, &
!                 ' and the simulation started on ',I2,'/',I2,'/',I4)  
 1985     FORMAT(I2,'/',I2,'/',I4)                     
                    
           T2X = T2X + 273.15
           DOXS = -139.34411+1.575701E5/T2X-6.642308E7/T2X**2.0+1.2438E10/T2X**3.0-8.621949E11/T2X**4.0
                
           DOXS = 0.8*EXP(DOXS)*(1-0.000035*SELEV*3.2808)
           IF(DOXS.GT.10.0) DOXS = 10.0
           DO I = 1,MBOT
              DSO2(I) = DOXS
           ENDDO
        ENDIF

    !C... Set initial DO profile to be 80% of saturation DO concentration
    !C... which is dependent on temperature and eleveation. 
       IF((iprnt(4).eq.1.and.DO_FLAG.EQ.0).OR.T_FLAG.EQ.0) THEN
           DO I = 1,MBOT
              T2X = T2(I)+273.15
              DOXS = -139.34411+1.575701E5/T2X-6.642308E7/T2X**2.0+1.2438E10/T2X**3.0-8.621949E11/T2X**4.0
               
              DOXS = 0.8*EXP(DOXS)*(1-0.000035*SELEV*3.2808)
              IF(DOXS.GT.10.0) DOXS = 10.0
              DSO2(I) = DOXS
           ENDDO
          WRITE(99,*)"The estimated DO on start moment : "
          WRITE(99,*)(DSO2(I),I=1,MBOT)
       ENDIF

! This part has been moved to the main program
       !C* Set initial sediment temperature profile
!          DZSL=ZSLT/FLOAT(IZSLT-1)
!          ETAB=2.0
!        DO 6 I=1, IZSLT
!           ZSL(I)=FLOAT(I-1)*DZSL
!    6	CONTINUE

!        TSHAL=TSLAV/2.0

!RATIOX is lake geometry ratio
!          IF(RATIOX.GE.4.0.AND.RATIOX.LE.5.0) THEN
!           TSLXX=TSHAL-1.0+(TSHAL+3.0)*ALOG10(RATIOX)
!          ENDIF

!          IF(RATIOX.GT.5.0) TSLXX=TSLAV

!          IF(RATIOX.LT.4.0) THEN

!           TSLXX=TSHAL-1.0+(TSHAL+3.0)*ALOG10(RATIOX)
!           DXTS=TSLAV-TSLXX
    !C	TSLAV = TS10(0), TSLXX = TS10(Hmax) 
    !C	SD - ZS is Secchi disk depth in meters 

!           AVOLM=ASX/1000000.0*ZMAX
    !     CONVERT AREA IN KM**2
        
!           AFACT=SD**0.35*AVOLM**0.125
           
!           FILE27="Ts10_profile.dat"  
!           CALL MAKEFILE(PATH_F,FILE27,FFILE)
!           OPEN(27,FILE="..\#COMMON\FIXED_INPUT\Ts10_profile.dat")

     
!           READ(27,*)
!           DO 85 I=1,38
!            READ(27,856) ZTS(1,I),XTS(1,I),XTS(2,I)
!     856     FORMAT(3X,F5.2,5X,F4.2,4X,F5.2)
!     85    CONTINUE

!          ENDIF
          
!           DO 10 J=1,MBOT

!         IF(RATIOX.LT.4.0) THEN
!             ZNORM=Z(J)/AFACT
     
!           IF(J.EQ.1) K=1
             
!    837	   IF(K.GT.37) GOTO 838
    	   
!           IF(ZNORM.GE.ZTS(1,K).AND.ZNORM.LE.ZTS(1,K+1)) THEN
!              DDZ=ZTS(1,K+1)-ZTS(1,K)
!              DDX=ZNORM-ZTS(1,K)
!              DDA=XTS(1,K+1)-XTS(1,K)
!              XXTS=XTS(1,K)+DDA*DDX/DDZ
!            GOTO 838
!           ELSE
!              K=K+1
!              GOTO 837
!           ENDIF
    	  
!     838   IF(K.GE.38) XXTS=1.0
!           TSLXX=TSLAV-XXTS*DXTS
    !	XXTS = [TS10(0) - TS10(Z)]/[TS10(0) - TS10(Hmax)]
    !C	TSLXX = TS10(Z), RATIO < 4.0
!         ENDIF
    	  
    	 
!           DO 5 I=1,IZSLT
!            TSL(I,J)=(T2(J)+0.5)*EXP(-ETAB*ZSL(I))+(1.0-EXP(-ETAB*ZSL(I)))*TSLXX           
!           TSL(I,J)=T2MBOT*EXP(-ETAB*ZSL(I))+(1.0-EXP(-ETAB*ZSL(I)))*TSLXX
                     
!     5     CONTINUE
     
!     10    CONTINUE
           
!           CLOSE (27)


    !CDing  DO 5 I=1,IZSLT
    !       ZSL(I)=FLOAT(I-1)*DZSL
    !       TSL(I)=T2MBOT*EXP(-ETAB*ZSL(I))+
    !    +             (1.0-EXP(-ETAB*ZSL(I)))*TSLMEAN
    !       TSLP(I)=TSL(I)
    !   5 CONTINUE

    ! 1500 FORMAT( /,  5X,&
    !       /,5X,41HMINIMUM THICKNESS OF EACH LAYER (DZLL) = ,F5.2,9H METER(S)&
    !        /,5X,41HMAXIMUM THICKNESS OF EACH LAYER (DZUL) = ,F5.2,9H METER(S)&
    !         /,5X,40HSURFACE ABSORPTION COEFFICIENT (BETA) = ,F5.2,&
    !        /,5X,30HEMISSIVITY OF WATER (EMISS) = ,F5.2,&
    !        /,5X,'EXTINCTION COEFF. OF WATER (XK1) = ',F5.2,3X,'M**-1',&
    !        /,5X,'EXTINCTION COEFF. OF CHLA  (XK2) = ',F5.2,3X,'L/MG/M',&
    !        /,5X,'MAX. HYPOLIMNETIC DIFFUSIVITY (HKMAX) = ',F7.4,' M**2/D',&
    !        /,5X,'WIND FUNCTION COEFFICIENT (WCOEF) = ',F6.3,&
    !        /,5X,'WIND SHELTERING COEFFICIENT (WSTR)= ',F6.3,/,&
    !        /,5X,34HWIDTH OF INLET CHANNEL (WCHANL) = ,F6.2,9H METER(S))
    ! 1501 FORMAT(5X,'LONGITUDINAL LENGTH OF LAKE (WLAKE) =',F10.2,7H METERS,&
    !      /,5X,30HDEEPEST BED ELEVATION (DBL) = ,F8.2,17H METERS ABOVE MSL,&
    !      /,5X,26HINITIAL LAKE STAGE (ST) = ,F8.2,17H METERS ABOVE MSL,&
    !      /,5X,16HBED SLOPE (S) = ,F10.8,&
    !      /,5X,29HROUGHNESS COEFFICIENT (FT) = ,F6.4,&
    !      //,5X,48HELEVATION OF BOTTOM OF OUTFLOW CHANNEL (ELCB) = ,F6.2,&
    !      17H METERS ABOVE MSL,&
    !      /,5X,40HSIDE-SLOPE OF OUTFLOW CHANNEL (ALPHA) = ,F6.2,8H DEGREES,&
    !      /,5X,31HBOTTOM WIDTH OF CHANNEL (BW) = ,F6.2,7H METERS,&
    !      //,5X,34HINITIAL NUMBER OF LAYERS (MBOT) = ,I2,&
    !      /,5X,37HNUMBER OF MONTHS OF SIMULATION (NM) = ,I2,&
    !     /,5X,54HDAY OF MONTH OF THE FIRST DAY OF SIMULATION (ISTART) = ,I2&
    !      /,5X,53HINTERVAL AT WHICH RESULTS WILL BE PRINTED (NPRINT) = ,I3,&
    !      7H DAY(S))

          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
           SUBROUTINE STATS(FLDATA,XX,IFLAG,DEPTHX,I,NF)
    !C***
    !C*** Compute statistics and statisitical quantities with 
    !C*** Y designating field data and X designating model results.
    !C***
          INTEGER FMON,FDAY,FYEAR
          COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
          COMMON/STEPS2/MBOT,ILAY
          SUMXY(I)=SUMXY(I)+FLDATA*XX
          SUMX(I)=SUMX(I)+XX
          SUMY(I)=SUMY(I)+FLDATA
          XSQ(I)=XSQ(I)+XX*XX
          YSQ(I)=YSQ(I)+FLDATA*FLDATA
          XX=XX-FLDATA
          X3=ABS(XX) 
   !C	WRITE(*,*) X3," ",RS(I,1)," ",RS(I,2)," ",DEPTH," ",I
   !Find the depth where has the MAXimum erorr - RS(I,1)
         IF(X3.GT.ABS(RS(I,1))) THEN
            RS(I,1)=XX
            ZRS(I,1)=DEPTHX
         ENDIF
   !Find the depth where has the MINImum erorr - RS(I,2)      
         IF(X3.LT.ABS(RS(I,2))) THEN
            RS(I,2)=XX
   !C	WRITE(*,*) RS(I,2)," ",XX," ",I
            ZRS(I,2)=DEPTHX
          ENDIF
   !Find the MEAN erorr - RS(I,3)        
   !      RS(I,3)=0.5*(RS(I,1)+RS(I,2))
          RS(I,3)=RS(I,3)+ABS(XX)/REAL(NF)       
          
          IF(X3.GT.ABS(RMS(I))) THEN
            RMS(I)=XX
            MTHRMS(I)=MONTH
            MDAYRMS(I)=MDAY
            ZRMS(I)=DEPTHX
          ENDIF
          IFLAG=IFLAG+1
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE THICKNS(MBOT)
    !C***
    !C*** Compute thickness of each layer from the depth 
    !C*** area curve in LAKE.
    !C***
          REAL*8 A,V,TV,ATOP,AVOL
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          AZ=0.
          AVOL=0.
          DO 100 I=1,MBOT
            II=MBOT+1-I
            AVOL=AVOL+V(II)
            CALL LAKE(ZDUM,AVOL,0,4)
            DZ(II)=ZDUM-AZ
            AZ=AZ+DZ(II)
     100  CONTINUE
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE TVOL(MBOT)
    !C*** 
    !C*** Determine the volume of water above a layer
    !C***
          REAL*8 A,V,TV,ATOP,SUM
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          SUM=0.
          DO 100 I=1,MBOT
            SUM=SUM+V(I)
            TV(I)=SUM
     100  CONTINUE
          RETURN
          END

    !C***************************************************C
    
    !C***************************************************C
          SUBROUTINE WDEPTH(ST,Q,LW)
    !C***
    !C*** Determine layers contibuting to outflow.  Outflow from
    !C*** a layer is proportional to the thickness of the layer.
    !C*** Compute and send outflow concentrations to the outflow
    !C*** data file  (tape8.OUT).
          REAL*8 A,V,TV,ATOP,RHO
          COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
          COMMON/RESULTX/VAR(120,21)
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
                   
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
        
          !COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,DY
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
          DIMENSION VOUT(11),IFL(11)
          DATA IFL/1,3,5,6,7,8,9,10,11,12,13/
          QP=-Q
          ERR=QP*0.001
          DH=ST-ELCB
          AREA=(BW+DH/TAN(ALPHA*0.017453293))*DH
          X=Q/(86400*AREA)
          VH=X*X*.05097
      111 QPRIME=0.0
          SUMZ=0.0
          SUMV=0.
          SUMVRHO=0.
          DO 100 I=1,MBOT
            IF(V(I).LE.QE(I)) GOTO 100
            RHOWL=RHO(VAR(I,1),VAR(I,7),VAR(I,8))
            SUMZ=SUMZ+DZ(I)
            SUMV=SUMV+V(I)
            SUMVRHO=SUMVRHO+V(I)*RHOWL
            RHOM=SUMVRHO/SUMV
            IF(RHOWL.LE.RHOM)GO TO 100
            ZW=DH+RHOM*VH/(RHOWL-RHOM)
            IF(SUMZ.LE.ZW) GO TO 100
            LW=I
            GOTO 112
     100    CONTINUE
          LW=MBOT
     112  DO 110 I=1,LW
             IF(QE(I).GE.V(I)) GOTO 110
             QE(I)=QP*DZ(I)/SUMZ+QE(I)
             IF(QE(I).GT.V(I)) THEN
               QPRIME=QE(I)-V(I)+QPRIME
               QE(I)=V(I)
             ENDIF
      110 CONTINUE
          IF(QPRIME.GT.ERR) THEN
             QP=QPRIME
             GOTO 111
          ENDIF
    !C********* COMPUTATIONS FOR OUTFLOW FILE ***********
          IF(IPRNT(2).GT.0) THEN
    !C********** CONVERT OUTFLOW VOLUME TO CFS **********
                  QOUT=-Q*4.0873E-4
                  RQIN=-1/Q
                  DO 120 J=1,1
                    VOUT(J)=0.0
                    DO 120 I=1,MBOT
      120              VOUT(J)=VAR(I,IFL(J))*QE(I)*RQIN+VOUT(J)
                  WRITE(2,1002) MONTH,MDAY,QOUT
                  WRITE(2,1001) (VOUT(J),J=1,11)
     1002         FORMAT(1X,2I3,F8.1)
     1001         FORMAT(1X,F4.1,F7.4,F6.1,F5.1,F6.1,F7.4,5F7.4)
          ENDIF
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE WINEN(T,V,WIND)
    !C**
    !C...CALCULATION OF THE SHEAR VELOCITY AND THE WIND SHEAR STRESS
          REAL*8 FAO

    !C...CONVERSION OF WIND SPEED FROM M.P.H. TO M/S
    !C...DENSITY OF WATER AND AIR ASSUMED TO BE 1000 AND 1.177 KG/M3
    !C...
          W=WIND*0.447
          FAO=0.0
          CALL LAKE(FTCH,FAO,0,2)
          ZB=ALOG(FTCH)*0.8-1.0718
          W=W*1.666667*(ZB+4.6052)/(ZB+9.2103)

    !C...CALCULATE THE WIND COEFFICIENT FROM WU (1971)
    !C...SOLVE Cz BY GAUSS-NEWTON METHOD
    !      CZ=SQRT(WIND*0.447)*0.00036
    !      FS=(WIND*0.447)**2.0/(9.81*10.0)
    !10    FV=CZ**(-0.5)+2.5*ALOG(0.011*CZ*FS)
    !      DFV=-0.5*CZ**(-1.5)+2.5/CZ
    !      CZA=CZ-FV/DFV
    !      IF(CZA.LT.0.0) CZA=0.000001
    !      PRE=ABS(CZA-CZ)/CZ
    !      IF(PRE.LE.0.001) GOTO 40
    !      CZ=CZA
    !      GOTO 10
    !40    CZ=CZA

    !C...CALCULATION OF WIND SHEAR STRESS T or TAU in W/m**2
    !C...1.177 is the density of air in kg/m**3 at 30oC.
          CZ=SQRT(W)*0.0005
          IF(W.GE.15.) CZ=0.0026
          T=1.177*CZ*W*W
    !C...ASSIGNMENT OF CHARACTERISTICSURFACE VELOCITY
    !C...USING CALCULATION OF SHEAR VELOCITIES
          V=0.0343*SQRT(CZ)*W
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE WINMIX(ENG,TS,TSAVE,ILAY,MBOT,VC)
    !C...
    !C...CALCULATE THE AMOUNT OF ENTRAINMENT RESULTING FROM WIND MIXING.
    !C...USE THE DEPTH OF CENTER OF MASS OF MIXED LAYER TO DETERMINE THE 
    !C...POTENTIAL ENERGY THAT MUST BE OVERCOME BY THE KINECTIC ENERGY 
    !C...OF THE WIND FOR ENTRAINMENT TO OCCUR.
    !C...
          REAL*8 A,V,TV,ATOP,RHO
          COMMON/TEFX/T2K(120),TEHE(120),BMK(120),OLDHQ,ITERF 
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          DIMENSION TSAVE(120)
          
          IF(ILAY.GE.MBOT) THEN
   !Fang, 4//2/2009         
   !         ILAY=MBOT
           DO 55 I=1,MBOT
            TSAVE(I)=TS
     55    CONTINUE       
           GOTO 35
          ENDIF 
           
    !CFX  --  IF MIT>0, THE PROGRAM WORKS AS M.I.T. MODEL,
    !CFX  --  OTHERWISE MINLAKE MODEL FOR WIND-MIXING ALOGORITHM
          VC=VC
          MIT=-66 ! < 0 MINLAKE model, otherwise MIT model
          TPE=0.0
    !CFX  --  MODEL EXCLUDES CONMIX SUBROUTINE FROM MIT (HARLEMAN, 1977),
    !CFX  --  WIND MIXING STARTS FROM THE FIRST LAYER EACH DAY!!
         
          SUM1=0.0
          SUM2=0.0
          I=1
          RV=RHO(T2(1),C2(1),CD2(1))*V(1)
          SUM1=SUM1+RV
          SUM2=SUM2+RV*Z(1)
          TSAVE(I)=TS

    !-- FUNCTION OF RICHARDSON NUMBER FOR ENERGY RATIO 
    !-- d(PE)/d(KIN) = F(Ri)
          FRI=1.0
           
     20   DCM=SUM2/SUM1
          TSTEP=T2(I+1)
          DENH=RHO(TSTEP,C2(I+1),CD2(I+1))
          DENL=RHO(TS,C2(I),CD2(I))
                
          IF(MIT.LT.0) THEN
    !C...CALCULATION OF POTENTIAL ENERGY OF MIXED LAYER -- MINLAKE,1986
            TPE=9.81*TV(I)*(Z(I)+DZ(I)/2.0-DCM)*(DENH-DENL)
     
          ELSE
    !CFX -- MIT ORIGINAL MODEL (HARLEMEN AND OTHERS, 1977)      
            TPE=TPE+9.81*DZ(I+1)*A(I+1)*(DENH-DENL)*Z(I+1)/2.0
           
    !CFX -- MIT MODEL (BLOSS AND HARLEMAN, 1979) 
            IF(I.GT.1) THEN
              ENG=(1.0-FRI)*ENG*A(1)/A(I-1)
              ENG=ENG*A(1)/A(I)
            ENDIF
          
            RI=9.81*ABS(DENH-DENL)*(Z(I)+DZ(I)/2.0)/(DENL*VC**2.0)
            FRI=0.057*RI*((29.5-SQRT(RI))/(14.2+RI)) 
            FRI=AMIN1(FRI,1.0)
            FRI=AMAX1(FRI,0.0)
          ENDIF

    !CMIKI- IMBERGER
    !C... CALL ENERGY(VC,TS,ILAY,HS,ENG,PE,HE,HA,HBR,HC,RI)

    !C...CRITERIA FOR ENTRAINMENT (UNIVERSAL ALOGRITHM -- FANG, 1993)
          IF((FRI*ENG).LT.TPE) GOTO 40
          
    !C...ENTRAINMENT OF LAYER I+1
     30   I=I+1
          TS=(TS*TV(I-1)+TSTEP*V(I))/TV(I)
          TSAVE(I)=TS
          IF(I.GE.MBOT) GO TO 40
          RV=RHO(T2(I),C2(I),CD2(I))*V(I)
          SUM1=SUM1+RV
          SUM2=SUM2+RV*Z(I)
          GO TO 20
     35   I=ILAY
     40   ILAY=I
   
   !Fang, 4//2/2009
   !      IF(ILAY.GT.MBOT) ILAY=MBOT
          
          DO 50 K=1,ILAY
            T2(K)=TS
     50   CONTINUE
     
    !CFX
    !      DO 8168 KK=1,NDAYS
    !      IF(MDAY+MONTH*100.EQ.NPRNT(KK)) THEN
    
    !       WRITE(46,705) MONTH,MDAY
    !705    FORMAT(5X/5X/I3,2X,I5/5X/)
    
    !C...T2K -- PREVIOUS DAY WATER TEMPERATURE -- T2(i-1)
    !C...TEHE -- TEMPERATURE AFTER SOLVING HEAT TRANSPORT EQUATION
    !C...BMK -- DIFFUSION COEFFICIENT = K1*K2/(K1+K2)
    
    !       WRITE(46,735) (Z(K),T2K(K),TEHE(K)
    !    +   ,T2(K),BMK(K),K=1,MBOT) 
    !735    FORMAT(30(4(F9.5,3X),F10.5/))        
    !      
    !      ENDIF
    !8168  CONTINUE
     
     740  RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE VOLUME(MBOT)
    !C***
    !C*** Compute the volume of each layer based on the depth-volume 
    !C*** relationship found in LAKE.
    !C***
          REAL*8 A,V,TV,ATOP,VDUM,VZ
         !COMMON/VOL/ ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 

          AZZ=0.
          CALL LAKE(ZMAX,VDUM,0,3)
          VZ=VDUM
          DO 100 I=1,MBOT-1
            AZZ=DZ(I)+AZZ
            Z2=ZMAX-AZZ
            CALL LAKE(Z2,VDUM,0,3)
            V(I)= VZ-VDUM
            VZ=VDUM
     100  CONTINUE
          V(MBOT)=VZ
          RETURN
          END

!***********************************************************
!*                                                         *
!*                                                         *
!***********************************************************
          SUBROUTINE FDATA(NF,NPRFLE,NSKIP)
 
!Subroutine to read field data from the input data and compute 
!statistics and deviations between field data and simulation

REAL*8 A,V,TV,ATOP
INTEGER FMON,FDAY,FYEAR
COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)      
COMMON/FILEX/DIN,MET,FLO,TAPE8,TAPE1,IREC
COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10)
COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
COMMON/STEPS2/MBOT,ILAY
COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
COMMON/FIELD/IFLAG(10),FLDATA(10,120),DEPTH(120),NFLD(10),SD,NSKIPDAY,NSDAY(640),SDFIELD(640),TAPE64
COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
COMMON/DATA50/ICOUNTER,SMDATA(2,120),ISD
COMMON/MIXED/DMAXX,DMINN,DIVIDE_MAX,DIVIDE_MIN
COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
COMMON/NO_DAY_TOT/NDAYO
COMMON/NDAYOCOUNTER/NDAYO_COUNTER
DIMENSION COMP(120,10),OUTD(2,2)     
EQUIVALENCE (T2(1),COMP(1,1))
CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1

NDAYO_COUNTER=NDAYO_COUNTER+1
         
         
DO I=1,10
    DO J=1,3
        RS(I,J)=0.0
    ENDDO
    RS(I,2)=100.0
ENDDO
DO I=1,10
    DO J=1,120
        FLDATA(I,J)=0.0
    ENDDO
ENDDO
!print*,nmfirst
!if(nmfirst.eq.1)print*,'read nf,npr'
!print*,'nmfirst=',nmfirst
!if(nmfirst.eq.1)print*,'hope it is not there read nf,npr'
IF(ICOUNTER.LT.1) NSKIP=0
DO I=1,120
    DEPTH(I)=-1.0
ENDDO
IF(ICOUNTER.LT.1) READ(7,*)
IF(ICOUNTER.LT.1) READ(7,*)
READ(7,*) NF,NPRFLE
WRITE(199,*) "NF,NPRFLE,NSKIP,NSKIPDAY,NDAYS"
WRITE(199,*)NF,NPRFLE,NSKIP,NSKIPDAY,NDAYS

IF(NF.GT.0) THEN
    If(ICOUNTER.LT.1) READ(7,*)
    READ(7,*) SD
    WRITE(199,*) "SD"
    WRITE(199,*)SD
    !NEW METHOD FOR CHANGING THE XK1, ON JUNE 30, 1992
    !EMCOE(3) < 0.0 -- IT IS NONFUNCTIONAL PART
    IF(EMCOE(3).GT.0.0.AND.ISD.EQ.1) THEN
        TK=EMCOE(3)/SD
        XK1=TK-XK2*CHLATOT(1)
        WRITE(800,*)JULIANDAY,XK1,SD,CHLATOT(1)
    ENDIF 
    IF(EMCOE(3).LT.0.0.AND.ISD.EQ.0)THEN
        TK3=XK1+XK2*CHLATOT(1)
        SD3=1.84/TK3
        WRITE(800,*)JULIANDAY,XK1,SD,SD3,CHLATOT(1)
    ENDIF 
    IF(ICOUNTER.LT.1) READ(7,*)
    READ(7,*) (NFLD(I),I=1,NPRFLE)
    WRITE(199,*) "(NFLD(I),I=1,NPRFLE)"
    WRITE(199,*)(NFLD(I),I=1,NPRFLE)
    IF(ICOUNTER.LT.1) READ(7,*)
    READ(7,*) (DEPTH(I),I=1,NF)
    WRITE(199,*) "(DEPTH(I),I=1,NF)"
    WRITE(199,*)(DEPTH(I),I=1,NF)
    IF(ICOUNTER.LT.1) READ(7,*)
    DO 305 I=1,NPRFLE
        READ(7,*) (FLDATA(NFLD(I),J),J=1,NF)
        WRITE(199,*) "(FLDATA(NFLD(I),J),J=1,NF)"
        WRITE(199,*)(FLDATA(NFLD(I),J),J=1,NF)
    305  CONTINUE
    IF(ICOUNTER.LT.1) READ(7,*)
    ICOUNTER=2
    !NSKIP=NSKIP+1
    !IF(NSKIPDAY.GE.NSKIP) GOTO 678
    NDAYS=NDAYS+1 
              
    !Locate simulation values corresponding to sampled constituents and depth of field data
    !OUTD(1,*) is field data, OUTD(2,*) is model predictions
    !OUTD(*,1) is water temperature, OUTD(*,2) is Dissolved Oxygen
    !Z(I) is simulation depth, DEPTH(KK) is depth with field data
    !FLDATA(1,KK) =  Field temperature data
    !FLDATA(6,KK) = Field DO data
                  
    I=NPRFLE    
    LL=1
    DO 310 KK=1,NF
        OUTD(1,1)=FLDATA(1,KK)
        OUTD(1,2)=FLDATA(6,KK)
        L=LL
        DO 315 LL=L,MBOT
            IF(Z(LL).LT.DEPTH(KK)) GOTO 315
            !For water surface Temp or DO    
            IF(LL.EQ.1) THEN
                DO 320 I2=1,NPRFLE
                    I=NFLD(I2)
                    XX=COMP(LL,I)
                    !Fang SMDAT store difference simulated and measured parameters at field depth
                    !Fang 3/31/2009
                    IF(I.EQ.1) THEN
                        OUTD(2,1)=XX
                        SMDATA(1,KK)=OUTD(2,1)-OUTD(1,1)
                    ELSE
                        OUTD(2,2)=XX
                        SMDATA(2,KK)=OUTD(2,2)-OUTD(1,2)                       
                    ENDIF
                    CALL STATS(FLDATA(I,KK),XX,IFLAG(I),DEPTH(KK),I,NF)
                320 CONTINUE
            ELSE
                DO 330 I2=1,NPRFLE
                    I=NFLD(I2)
                    DZ1=DEPTH(KK)-Z(LL-1)
                    DZ2=Z(LL)-Z(LL-1)
                    XX=COMP(LL-1,I)+DZ1/DZ2*(COMP(LL,I)-COMP(LL-1,I))
                    IF(I.EQ.1) THEN
                        OUTD(2,1)=XX
                        SMDATA(1,KK)=OUTD(2,1)-OUTD(1,1)                       
                    ELSE
                        OUTD(2,2)=XX
                        SMDATA(2,KK)=OUTD(2,2)-OUTD(1,2)                        
                    ENDIF
                    CALL STATS(FLDATA(I,KK),XX,IFLAG(I),DEPTH(KK),I,NF)
                330 CONTINUE
            ENDIF
        GOTO 380
        315 CONTINUE
        
        !OUTPUT DATA FOR PLOT FIGURES (1994)
        380 MFLAG=0
        IF(THICE.GT.0.0) MFLAG=10
        !Measured, simulated TEMPerature and measured, simulated DO
        !When ICE>0, it sets MFLAG=10         
        !OUTD(1,1) -- FIELD DATA, TEMPERATURE
        !OUTD(2,1) -- SIMULATED DATA, TEMPERATURE -- Interpolated at the depth with field data
        !OUTD(1,2) -- FIELD DATA, DO
        !OUTD(2,2) -- SIMULATED DATA, DO  -- Interpolated at the depth with field data
      
        !IMPORTANT -- PREVIOUSLY ALL FOUR DATA ( SIM, FIELD OF Temp AND DO) WRITTEN IN SETD.SDF FILE
        !NOW WITH NEW PROVISION FOR EXCLUDING CALCULATION OF Temp OR DO WHEN FIELD DATA IS NOT AVAILABLE
        !WE ARE USING TWO SEPERATE FILE FOR TEMP AND DO DATA ( FILED AND Simulated AT MEASURED DEPTHS)
      
        DMIX=Z(ILAY)+0.5*DZ(ILAY)
        
        IF(NPRFLE.EQ.1.AND.NFLD(1).EQ.6)GO TO 6110
        !ABOVE STATEMENT MEANS NO TEMPERATURE VALUE AVAILABLE FOR THAT DATE
        !SO DON'T WRITE TEMPERATURE VALUE THAT DAY
        IF(NDAYO_COUNTER.EQ.1.AND.KK.EQ.1) WRITE(67,*)NDAYO
        IF(KK.EQ.1) WRITE(67,*)NDAYO_COUNTER,NF
   
        WRITE(67,616) OUTD(1,1),OUTD(2,1),DEPTH(KK),MFLAG,MONTH,MDAY,MYEAR,KK,DMIX
        
        WRITE(567,616) OUTD(1,1),OUTD(2,1),DEPTH(KK),MFLAG,MONTH,MDAY,MYEAR,KK,DMIX 
        
        !STORE DO FOR Statistaical Calculation
        IF(NPRFLE.EQ.1.AND.NFLD(1).EQ.1)GO TO 310
        !ABOVE STATEMENT MEANS NO DO VALUE AVAILABLE FOR THAT DATE
        !SO DON'T WRITE DO VALUE THAT DAY
        IF(NDAYO_COUNTER.EQ.1.AND.KK.EQ.1) WRITE(68,*)NDAYO
        IF(KK.EQ.1) WRITE(68,*)NDAYO_COUNTER,NF
        6110    WRITE(68,616) OUTD(1,2),OUTD(2,2),DEPTH(KK),MFLAG,MONTH,MDAY,MYEAR,KK,DMIX
        WRITE(568,616) OUTD(1,2),OUTD(2,2),DEPTH(KK),MFLAG,MONTH,MDAY,MYEAR,KK,DMIX
            616 FORMAT(1X,3(F6.2,2X),2X,I2,3X,I2,4X,I2,2X,I4,2X,I4,2X,F6.2)
    310 CONTINUE
    !Fang 3/31/2009    
    WRITE(98,3008) MONTH,MDAY,MYEAR,ZRS(1,1),RS(1,1),ZRS(1,2),RS(1,2),RS(1,3),NF
    3008 FORMAT(3X,I2,4X,I2,2X,I4,4X,'1   ',5(4X,F8.3),6X,I2) 

    !FILE 113 T_ERR_PLT.DAT
    IF(EMCOE(3).LT.0.0.AND.ISD.EQ.0)THEN
        WRITE(113,3010)MONTH,MDAY,MYEAR,JULIANDAY,NF,ZRS(1,1),RS(1,1),ZRS(1,2),RS(1,2),RS(1,3),SD3 
        3010 FORMAT(3X,I2,4X,I2,2X,I4,2X,I4,4X,I2,5(4X,F8.3),7X,F6.3) 
    ELSE
        WRITE(113,3010)MONTH,MDAY,MYEAR,JULIANDAY,NF,ZRS(1,1),RS(1,1),ZRS(1,2),RS(1,2),RS(1,3)
    ENDIF
    !FILE 114 DO_ERR_PLT.DAT
    IF(IPRNT(4).EQ.1) THEN
        WRITE(98,3009) MONTH,MDAY,MYEAR,ZRS(6,1),RS(6,1),ZRS(6,2),RS(6,2),RS(6,3),NF
        3009 FORMAT(3X,I2,4X,I2,2X,I4,4X,'2   ',5(4X,F8.3),6X,I2)
        IF(EMCOE(3).LT.0.0.AND.ISD.EQ.0)THEN
            WRITE(114,3011)MONTH,MDAY,MYEAR,JULIANDAY,NF,ZRS(6,1),RS(6,1),ZRS(6,2),RS(6,2),RS(6,3),SD3
            3011 FORMAT(3X,I2,4X,I2,2X,I4,2X,I4,4X,I2,5(4X,F8.3),7X,F6.3)
        ELSE
            WRITE(114,3011)MONTH,MDAY,MYEAR,JULIANDAY,NF,ZRS(6,1),RS(6,1),ZRS(6,2),RS(6,2),RS(6,3)
        ENDIF
    ENDIF

    !Fang July 8, 2009 - Determine mixed layer depth from measured Temp and DO profiles.
    DO IM=1,NPRFLE
        IP=NFLD(IM)
        !First, find out the minimum and maximum values for profiles
        FMIN=100.0
        FMAX=-100.0         
        DO MK=1,NF
            IF(FMIN.GT.FLDATA(IP,MK)) FMIN=FLDATA(IP,MK)
            IF(FMAX.LT.FLDATA(IP,MK)) FMAX=FLDATA(IP,MK)
        ENDDO
        !Set allowable difference for the mixed layer
        !Check input data in Mixed_depth.dat file
        DFMIX=(FMAX-FMIN)/DIVIDE_MAX
        IF(DFMIX.GT.DMAXX) DFMIX=DMAXX
        IF(DFMIX.LT.DMINN) DFMIX=DMINN
        IF((FMAX-FMIN).LT.DIVIDE_MIN)THEN
            DFMIX=(FMAX-FMIN)/DIVIDE_MIN
            IF(DFMIX.GT.0.75) DFMIX=0.75
            IF(DFMIX.LT.0.40) DFMIX=0.40
        ENDIF
        DINCRE=0.0
        DO MK=2,NF
            IF(MK.EQ.2) THEN
                TDOMIX=FLDATA(IP,1)
                DINCRE=FLDATA(IP,2)+FLDATA(IP,1)
            ELSE
                TDOMIX=DINCRE/FLOAT(MK-1)
                DINCRE=DINCRE+FLDATA(IP,MK)
            ENDIF
            !Check difference between data points     
            DFDIFF=FLDATA(IP,MK)-TDOMIX
            IF(MK.EQ.2) THEN
                WRITE(26,*)
                WRITE(26,*) 'FLD- DFMIX  DFDIFF  TDOMIX  DEPTH 1-DO/2-Temp.  MaxDiff_InPrfle ---Observed'
            ENDIF
            WRITE(26,3340) DFMIX,DFDIFF,TDOMIX,DEPTH(MK),IM,(FMAX-FMIN)
            3340  FORMAT(4X,F6.2,2X,F6.2,2X,F6.2,2X,F5.2,4X,I1,13X,F6.2)     
            IF(ABS(DFDIFF).GT.DFMIX) THEN
                FMIXDEPTH=(DEPTH(MK-1)+DEPTH(MK))/2.0
            GOTO 340
            ENDIF
            IF (MK.EQ.NF)THEN
                IF(ABS(DFDIFF).LE.DFMIX) FMIXDEPTH=DEPTH(MK)
            ENDIF
        ENDDO      
        340 IF(IM.EQ.1) THEN
            FMIX1=FMIXDEPTH
            TDOMIX1=TDOMIX
        ENDIF            
    ENDDO         
    !Fang July, 2009 - FMIX is average mixed layer depth from Temp and DO field profiles
    !TDOMIX1 - the mixed temperature or DO depending on which profile is listed first.
    !Mixed layer DO = FDOMIX in mg/l
    !Mixed layer Temp =  FTEMIX in oC
    IF(NPRFLE.GT.1) THEN
        FMIX=(FMIX1+FMIXDEPTH)/2.0
        IF(NFLD(1).EQ.1) THEN
            FTEMIX=TDOMIX1
            FDOMIX=TDOMIX
        ELSE
            FTEMIX=TDOMIX
            FDOMIX=TDOMIX1
        ENDIF
    ELSE
        FTEMIX=TDOMIX
        FMIX=FMIXDEPTH
    ENDIF
    WRITE(26,*)
    IF(NFLD(1).EQ.6) WRITE(26,*) 'OBSERVED mixed layer depths :      TEMP   DO    AVG     Actual Model Depth'                   
    IF(NFLD(1).EQ.1) WRITE(26,*) 'OBSERVED mixed layer depths :      DO    TEMP   AVG     Actual Model Depth'                    
    !This is simulated mixed layer depth       
    DMIX=Z(ILAY)+0.5*DZ(ILAY)
    WRITE(26,353) FMIXDEPTH,FMIX1,FMIX,DMIX
    353 FORMAT(1X/1X,'OBSERVED mixed layer depths :   ',3(1X,F6.2),6x,F6.2)    
    WRITE(310,354) FMIXDEPTH,FMIX1,FMIX,MONTH,MDAY,MYEAR
    354 FORMAT('Field mixed layer depths:',3(2X,F7.2),14X,I5,I5,I5)     

    !Mixed layer calculation for Simulaited value
    DO IM=1,2
        IP=NFLD(IM)
        IF(IP.EQ.6)THEN
            DO JJ=1,MBOT
                SMLDATA(IP,JJ)=DSO2(JJ)
            END DO
        ENDIF
        IF(IP.EQ.1)THEN
            DO JJ=1,MBOT
                SMLDATA(IP,JJ)=T2(JJ)
            END DO 
        ENDIF
        !First, find out the minimum and maximum values for profiles
        FMIN=100.0
        FMAX=-100.0         
        DO MK=1,MBOT
            IF(FMIN.GT.SMLDATA(IP,MK)) FMIN=SMLDATA(IP,MK)
            IF(FMAX.LT.SMLDATA(IP,MK)) FMAX=SMLDATA(IP,MK)
        ENDDO
        !Set allowable difference for the mixed layer
        !Check input data in Mixed_depth.dat file
        DFMIX=(FMAX-FMIN)/DIVIDE_MAX
        IF(DFMIX.GT.DMAXX) DFMIX=DMAXX
        IF(DFMIX.LT.DMINN) DFMIX=DMINN
        IF((FMAX-FMIN).LT.DIVIDE_MIN)THEN
            DFMIX=(FMAX-FMIN)/DIVIDE_MIN
            IF(DFMIX.GT.0.75) DFMIX=0.75
            IF(DFMIX.LT.0.40) DFMIX=0.40
            !IF(DFMIX.LT.0.50) DFMIX=0.50
        ENDIF
        DINCRE=0.0
        DO MK=2,MBOT
            IF(MK.EQ.2) THEN
                STDOMIX=SMLDATA(IP,1)
                DINCRE=SMLDATA(IP,2)+SMLDATA(IP,1)
            ELSE
                STDOMIX=DINCRE/FLOAT(MK-1)
                DINCRE=DINCRE+SMLDATA(IP,MK)
            ENDIF
            !Check difference between data points     
            DFDIFF=SMLDATA(IP,MK)-STDOMIX
            IF(MK.EQ.2) THEN
                WRITE(26,*)
                WRITE(26,*) 'SML- DFMIX  DFDIFF  STDOMIX  DEPTH 1-DO/2-Temp. MaxDiff_InPrfl --- Simulated'
            ENDIF
            WRITE(26,3341) DFMIX,DFDIFF,STDOMIX,Z(MK),IM,(FMAX-FMIN)
            3341  FORMAT(4X,F6.2,2X,F6.2,2X,F6.2,1X,F5.2,5X,I1,12X,F6.2)     
            IF(ABS(DFDIFF).GT.DFMIX) THEN
                SMIXDEPTH=(Z(MK-1)+Z(MK))/2.0
            GOTO 5340
            ENDIF
            IF (MK.EQ.MBOT)THEN
                IF(ABS(DFDIFF).LE.DFMIX) SMIXDEPTH=Z(MK)
            ENDIF
        ENDDO      
        5340 IF(IM.EQ.1) THEN
            SMIX1=SMIXDEPTH
            STDOMIX1=STDOMIX
        ENDIF
    ENDDO         
    !STDOMIX1 - the mixed temperature or DO depending on which profile is listed first OF Simulated values.
    !Mixed layer DO = SDOMIX in mg/l
    !Mixed layer Temp =  STEMIX in oC
    IF(NPRFLE.GT.1) THEN
        SMIX=(SMIX1+SMIXDEPTH)/2.0
        IF(NFLD(1).EQ.1) THEN
            STEMIX=STDOMIX1
            SDOMIX=STDOMIX
        ELSE
            STEMIX=STDOMIX
            SDOMIX=STDOMIX1
        ENDIF
    ELSE
        STEMIX=STDOMIX
        SMIX=SMIXDEPTH
    ENDIF
    WRITE(26,*)
    IF(NFLD(1).EQ.6) WRITE(26,*) 'Simulated mixed layer depths :          TEMP   DO    AVG     ActuAl Model Depth'                   
    IF(NFLD(1).EQ.1) WRITE(26,*) 'Simulated mixed layer depths :          DO     TEMP   AVG    ActuAl Model Depth'                    
    !This is actual simulated mixed layer depth calculated by MODEL      
    DMIX=Z(ILAY)+0.5*DZ(ILAY)
    WRITE(26,7353) SMIXDEPTH,SMIX1,SMIX,DMIX
    7353    FORMAT(1X/1X,'The Simulated mixed layer depths :   ',3(1X,F6.2),6x,F6.2)    
    WRITE(310,7354) SMIXDEPTH,SMIX1,SMIX,DMIX,MONTH,MDAY,MYEAR
    7354    FORMAT('SIM mixed layer depths:  ',4(2X,F7.2),5X,I5,I5,I5/)     
    !End of Simulated mixed layer calculation

    ILAY1=ILAY-1
    IF(ILAY1.EQ.0) ILAY1=1
    DMIX=Z(ILAY)+0.5*DZ(ILAY)
    IF(NRPFLE.EQ.1) THEN
        FNODATA=-1.0
        WRITE(69,355) FTEMIX,STEMIX,FNODATA,FNODATA,FMIX,SMIX,MYEAR,MONTH,MDAY
        WRITE(569,355) FTEMIX,STEMIX,FNODATA,FNODATA,FMIX,SMIX,MYEAR,MONTH,MDAY
        355    FORMAT(1X,3(F7.2,2X,F7.2),2X,I4,2X,I2,2X,I2)
    ELSE
        WRITE(69,355) FTEMIX,STEMIX,FDOMIX,SDOMIX,FMIX,SMIX,MYEAR,MONTH,MDAY
        WRITE(569,355) FTEMIX,STEMIX,FDOMIX,SDOMIX,FMIX,SMIX,MYEAR,MONTH,MDAY
    ENDIF
    !Store data on plot file (tape8.PLT)

    IF(IPRNT(5).GT.0) THEN
        WRITE(1,REC=IREC) REAL(NF)
        IREC=IREC+1
        WRITE(1,REC=IREC) REAL(NPRFLE)
        IREC=IREC+1
        DO 500 I=1,NF
            WRITE(1,REC=IREC) DEPTH(I)
            IREC=IREC+1
        500 CONTINUE
        DO 501 I=1,NPRFLE
            WRITE(1,REC=IREC) REAL(NFLD(I))
            IREC=IREC+1
        501 CONTINUE
        DO 502 I2=1,NPRFLE
        DO 502 I=1,NF
            WRITE(1,REC=IREC) FLDATA(NFLD(I2),I)
        502 IREC=IREC+1
        IF(NFLD(1).NE.1) THEN
            X=0.0
        ELSE
            !C...Mixed layer depth in field data taken at dT/dZ=1.0
            DO 503 J2=2,NF
                X=(FLDATA(1,J2-1)-FLDATA(1,J2))/(DEPTH(J2)-DEPTH(J2-1))
                IF(X.GT.1.0) GOTO 504
            503 CONTINUE
            504 X=(DEPTH(J2)+DEPTH(J2-1))*0.5
            IF(J2.GE.NF) X=ZMAX
        ENDIF
        WRITE(1,REC=IREC) X
        IREC=IREC+1
        WRITE(1,REC=IREC) SD
        IREC=IREC+1
    ENDIF
ELSE  
    NF=0
    IF(IPRNT(5).GT.0) THEN
        WRITE(1,REC=IREC) REAL(NF)
        IREC=IREC+1
    ENDIF
ENDIF
RETURN
END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE FPLOT(MYEAR,DMIX,SD,CHLMEANY)
    !C***SD = SECCHI
    !C*** Locate simulation values at same depth as field 
    !C*** data and send both to the plot data file (tape8.PLT)
    !C***
          CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
          REAL*8 A,V,TV,ATOP
          
          INTEGER FMON,FDAY,FYEAR
          COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),REPRO,XI,XKMZ,GRAZE(3,120)
          COMMON/FILEX/DIN,MET,FLO,TAPE8,TAPE1,IREC
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
       !   COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL       
          
          
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLT(90)
          DIMENSION VAR(120,13)
          EQUIVALENCE (T2(1),VAR(1,1))
   
          MYEAR=MYEAR+1-1
         
          WRITE(1,REC=IREC) REAL(JDY)
          IREC=IREC+1
          XNUL=-1.
          NSET=1
          IF(IPRNT(4).GT.0) NSET=8
          IF(NITRO.GT.0) NSET=10
          IF(IPRNT(6).GT.0) THEN
            LL=1
            DO 410 KK=1,IPRNT(6)
              L=LL
              DO 415 LL=L,MBOT
                IF(Z(LL).LT.PLT(KK)) GOTO 415
                X=Z(LL)-DZ(LL)*0.5
                L3=LL
                IF(PLT(KK).LT.X) L3=LL-1
                IF(L3.LT.1) L3=1
                IF(LL.EQ.MBOT) L3=MBOT
                WRITE(1,REC=IREC) PLT(KK)
                IREC=IREC+1
                DO 419 I=1,NSET
                  WRITE(1,REC=IREC) VAR(L3,I)
                  IREC=IREC+1
      419       CONTINUE
                IF(MODEL.EQ.3 .AND. IPRNT(4).GT.0) THEN
                  DO 420 K=1,NCLASS
                    WRITE(1,REC=IREC) CHLA2(K,L3)
                    IREC=IREC+1
      420         CONTINUE
                ENDIF
                GOTO 410
      415     CONTINUE
    !C********** IF REQUESTED PLOT DEPTH > Z(MBOT) USE
    !             DUMMY VALUE OF -1 ******************
              WRITE(1,REC=IREC) PLT(KK)
              IREC=IREC+1
              DO 405 I=1,NSET
                WRITE(1,REC=IREC) XNUL
                IREC=IREC+1
      405     CONTINUE
              IF(MODEL.EQ.3) THEN
                DO 406 K=1,NCLASS
                  WRITE(1,REC=IREC) XNUL
                  IREC=IREC+1
      406       CONTINUE
              ENDIF
      410   CONTINUE
          ENDIF
          WRITE(1,REC=IREC) CHLMEANY
          IREC=IREC+1
          X=ZP/TV(MBOT)*ZMAX
          WRITE(1,REC=IREC) X
          IREC=IREC+1
          WRITE(1,REC=IREC) DMIX
          IREC=IREC+1
          WRITE(1,REC=IREC) SD
          IREC=IREC+1
          IF(MDAY+MONTH*100.NE.NPRNT(NDAYS)) THEN
            NF=0
            WRITE(1,REC=IREC) REAL(NF)
            IREC=IREC+1
          ENDIF
          RETURN
          END

    !C***************************************************C
   
    !C***************************************************C
          SUBROUTINE SOLVET2(VAR2,NBOT)
    !C***
    !C*** Tri-diagonal matrix solving routine
    !C***
          REAL*8 AK,BK,CK,DK,TT,TX(120)
          COMMON/SOLV/ AK(120),BK(120),CK(120),DK(120)
          DIMENSION VAR2(120)

          DO 60 I=2,NBOT
            TT=AK(I)/BK(I-1)
            BK(I)=BK(I)-CK(I-1)*TT
     60     DK(I)=DK(I)-DK(I-1)*TT
    !C********BACK SUBSTITUTION**************
            TX(NBOT)=DK(NBOT)/BK(NBOT)
          DO 70 I=1,NBOT-1
            J=NBOT-I
     70     TX(J)=(DK(J)-CK(J)*TX(J+1))/BK(J)
          DO 80 I=1,NBOT
     80     VAR2(I)=SNGL(TX(I))
          RETURN
          END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE SUBLAY(IEUPH,NSLAY)
    !C...
    !C...SUBDIVIDE EACH LAYER IN EUPHOTIC ZONE INTO LAYERS OF 0.2 M OR LESS
    !C...FOR COMPUTING LIGHT LIMITATION ON GROWTH.
    !C...
          REAL*8 A,V,TV,ATOP,VDUM,TOV,SVOL
        !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL       
          
          COMMON/SUB/SDZ(180),SZ(180),LAY(120),AVGI(24,180),SVOL(180)

          NSLAY=0
          DO 100 I=1,IEUPH
            
            IF(DZ(I).GT.0.25) THEN
              LAY(I)=INT(DZ(I)*4.0+0.5)
            ELSE
              LAY(I)=1
            ENDIF
             
            DO 200 J=1,LAY(I)
              K=NSLAY+J
              
              IF(LAY(I).EQ.1) THEN
                SDZ(K)=DZ(I)
              ELSE
                SDZ(K)=0.25
              ENDIF
              
              IF(J.EQ.1) SDZ(K)=DZ(I)-FLOAT(LAY(I)-1)*0.25
              SVOL(K)=A(I)*SDZ(K)
     200    CONTINUE
            NSLAY=NSLAY+LAY(I)
     100  CONTINUE

    !C...ESTABLISH ARRAY OF DISTANCES OF EACH SUBLAYER BELOW WATER SURFACE
          SUMSZ=0.
          DO 300 J=1,NSLAY
            SZ(J)=SUMSZ+SDZ(J)/2.0
            SUMSZ=SUMSZ+SDZ(J)
     300  CONTINUE
    !C...ESTABLISH ARRY OF VOLUME OF EACH LAYER.....
          CALL LAKE(ZMAX,VDUM,0,3)
          TOV=VDUM
          ZDUM=ZMAX
          DO 400 I=1,NSLAY
            ZDUM=ZDUM-SDZ(I)
            CALL LAKE(ZDUM,VDUM,0,3)
            SVOL(I)=TOV-VDUM
            TOV=TOV-SVOL(I)
     400  CONTINUE 
          RETURN
          END
    !C**************************************************C
    
    !C**************************************************C
          SUBROUTINE CONSMAS(IW,IPL,DCF)
    !C***
    !C*** Compute the final concentration in a layer using the
    !C*** concentration in the density current and the
    !C*** concentration in the layer weighted by the volume
    !C*** in the layer and the volume in the density current
    !C***
          REAL*8 A,V,TV,ATOP
          INTEGER FMON,FDAY,FYEAR
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          
          COMMON/INFLOWX/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
      
          VRCP=1./(V(IPL)+DCF)
          T2(IPL)=(T2(IPL)*V(IPL)+TIN(IW)*DCF)*VRCP
          C2(IPL)=(C2(IPL)*V(IPL)+CIN(IW)*DCF)*VRCP
          CD2(IPL)=(CD2(IPL)*V(IPL)+CDIN(IW)*DCF)*VRCP
          DO 50 K=1,NCLASS
     50    CHLA2(K,IPL)=(CHLA2(K,IPL)*V(IPL)+CHLAIN(K,IW)*DCF)*VRCP
          PA2(IPL)=(PA2(IPL)*V(IPL)+PAIN(IW)*DCF)*VRCP
          BOD2(IPL)=(BOD2(IPL)*V(IPL)+BODIN(IW)*DCF)*VRCP
          DSO2(IPL)=(DSO2(IPL)*V(IPL)+DOIN(IW)*DCF)*VRCP
          IF(NITRO.EQ.1) THEN
           XNH2(IPL)=(XNH2(IPL)*V(IPL)+XNHIN(IW)*DCF)*VRCP
           XNO2(IPL)=(XNO2(IPL)*V(IPL)+XNOIN(IW)*DCF)*VRCP
          ENDIF
          V(IPL)=V(IPL)+DCF
          RETURN
          END
    !C**************************************************C
    
    !C**************************************************C
          SUBROUTINE DCFLOW(IPL,S,FT,WIDTH,IW)
    !C***
    !C*** Determine the density current volume and concentration
    !C*** with entrainment from each layer passed and the
    !C*** isopycnic layer that receives the density current.
    !C***
          REAL*8 A,V,TV,ATOP,RHO
          INTEGER FMON,FDAY,FYEAR
          COMMON/FILEX/ DIN,MET,FLO,TAPE8,TAPE1,IREC
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/FLOW/HMK(121),QE(120),FVCHLA(5),PE(5,121)
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
         
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
          COMMON/STEPS2/MBOT,ILAY
          COMMON/INFLOWX/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
          CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1

          X=QIN(IW)
          RHOMIX=RHO(T2(1),C2(1),CD2(1))
          RHOIN=RHO(TIN(IW),CIN(IW),CDIN(IW))
          CALL PDEPTH(RHOMIX,RHOIN,QIN(IW),S,IHP,QENIN,MBOT,SUMZ,FT,WIDTH,HP)
         
          DO 100 I=1,MBOT-1
    !C...COMPUTE THE DENSITY OF INFLOW AS IT ENTERS EACH LAYER
            RDC=RHO(TIN(IW),CIN(IW),CDIN(IW))
            RHOAMB=RHO(T2(I),C2(I),CD2(I))
    !C...LOCATE THE ISOPYCNI!LAYER
            IF(RDC.GT.RHOAMB) GO TO 5
            IPL=I-1
            IF(IPL.LT.1) IPL=1
            GO TO 3
    !C...CONTINUE THE SEARCH
    !C...COMPUTE THE ENTRAINMENT  FROM EACH LAYER
     5      QE(I)=ENTRAIN(I,QIN(IW),RDC,RHOAMB,DZ(I),S,SUMZ,QENIN,IHP,WIDTH,FT)
         
            IF(QE(I).GT.V(I)) QE(I)=V(I)
            RDCF=1./(QIN(IW)+QE(I))
            TIN(IW)=(TIN(IW)*QIN(IW)+T2(I)*QE(I))*RDCF
            CIN(IW)=(CIN(IW)*QIN(IW)+C2(I)*QE(I))*RDCF
            CDIN(IW)=(CDIN(IW)*QIN(IW)+CD2(I)*QE(I))*RDCF
            DO 50 K=1,NCLASS
     50       CHLAIN(K,IW)=(CHLAIN(K,IW)*QIN(IW)+CHLA2(K,I)*QE(I))*RDCF
            PAIN(IW)=(PAIN(IW)*QIN(IW)+PA2(I)*QE(I))*RDCF
            BODIN(IW)=(BODIN(IW)*QIN(IW)+BOD2(I)*QE(I))*RDCF
            DOIN(IW)=(DOIN(IW)*QIN(IW)+DSO2(I)*QE(I))*RDCF
            IF(NITRO.EQ.1) THEN
              XNHIN(IW)=(XNHIN(IW)*QIN(IW)+XNH2(I)*QE(I))*RDCF
              XNOIN(IW)=(XNOIN(IW)*QIN(IW)+XNO2(I)*QE(I))*RDCF
            ENDIF
            QIN(IW)=QIN(IW)+QE(I)
     100    CONTINUE
          RDC=RHO(TIN(IW),CIN(IW),CDIN(IW))
          IPL=MBOT
      3   IF(IW.EQ.1.AND.MONTH.EQ.3.AND.MDAY.GE.26) THEN
          DO 102 I=1,MBOT-1
          IF(Z(I).LE.0.35.AND.Z(I+1).GT.0.35) IPL=I
     102  CONTINUE
          ENDIF     
          IF(IW.EQ.1.AND.MONTH.EQ.4.AND.MDAY.LE.13) THEN
          DO 103 I=1,MBOT-1
          IF(Z(I).LE.0.75.AND.Z(I+1).GT.0.75) IPL=I
     103  CONTINUE
          ENDIF   
          IF(IW.EQ.3) IPL=MBOT  
          IF(IPRNT(1).EQ.1) WRITE(8,2000) Z(IPL),V(IPL),X,QIN(IW),PAIN(IW),&
         XNOIN(IW),XNHIN(IW),DOIN(IW),BODIN(IW)
     2000 FORMAT(7X,F5.2,6X,E12.5,3X,E12.5,2X,E12.5,2X,3(F6.3,3X),&
         2(F6.2,3X))
          RETURN
          END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE LIGHT(RAD,TD,C2X,ALBEDO)
    !C***
    !C*** Compute light distribution over a day in TD subsections.
    !C*** TD is photo period (hours).  The distribution is assumed
    !C*** to be sine function starting from sunrise.
    !C*** The light subsections are used in AVELITE.
    !C***
          COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,&
          BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP

    !C...CALCULATION OF LIGHT ABSORBED IN SNOW AND ICE COVERS
    !C...FOR YEAR-ROUND SIMULATION MODEL (FANG, 1994)

          IF(THSNOW.GT.0.0) THEN
            RDSNOW=(1.0-BTSNOW)*(1.0-ALFSNOW)*RAD*EXP(-GMSNOW*THSNOW)
          ENDIF
          IF(THSNOW.EQ.0.0) RDSNOW=RAD
     
          IF(THICE.GT.0.0) THEN
            RDICE=(1.0-BTICE)*(1.0-ALFICE)*RDSNOW*EXP(-GMICE*THICE)
          ENDIF
          IF(THICE.EQ.0.0) RDICE=RAD
          RAD=RDICE

    !PI IS CONSTANT, ALBEDO IS LIGHT REFLECTION AND XIAVE
    !IS THE AVERAGE PAR FROM SOLAR RADIATION DATA
    !XIAVE - uEINSTEIN/M**2/SECOND
    
    !C2X is surface suspended solid concentration

          PI=3.1415927
          ALBEDO=0.087-0.0000676*RAD+0.11*(1.0-EXP(-0.01*C2X))
          XIAVE=RAD*27.25/TD   
          ITD=INT(TD+0.5)

          DO 101 J=1,ITD

    !- USING STEP FUNCTION FOR I-AVERAGE
    !       XIT=XIAVE*PI/2.0*SIN(J*PI/FLOAT(ITD))

    !- USING LINEAR AVERAGE FOR I-AVERAGE
    !       XIT=XIAVE*PI/2.0*(SIN(J*PI/ITD)+SIN((J-1)*PI/ITD))/2.0

    !- USING COMPLETE INTEGRATION RESULTS TO GET I-AVERAGE
    !- THERE IS NO SIGNIFICANT DIFFERENCE FOR PRODUCTION CALCULATION
    !- PROVE IT ON NOVEMBER 27, 1992 (FANG)
            XIT=XIAVE*TD/2.0*(-COS(J*PI/ITD)+COS((J-1)*PI/ITD))
            PARI0(J)=XIT*(1.0-ALBEDO)
     101    CONTINUE       
          RETURN
          END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE AVGLITE(IEUPH,TD)
    !C***
    !C*** Compute average light in each sublayer for TD different
    !C*** time segments in a day. (follows work by J. Cardoni)
    !C*** TD is photo period in hours.
    !C***
          REAL*8 A,V,TV,ATOP,SVOL
          COMMON/TEMP6/PARI0(24),PCDUM(3,120),XNHD(120),XNOD(120),CHLADUM(3,120),XNCD(3,120),PADUM(120),SID(120)
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/SOURCE/RM(3,120),PROD(120),XMR(3,120),PRODSUM(120)
          COMMON/SUB/SDZ(180),SZ(180),LAY(120),AVGI(24,180),SVOL(180)
        ! COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF     
          COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
          DIMENSION XIZ1(24)

          JJ=0
          ITD=INT(TD+0.5)
          DO 50 K=1,ITD
      50  XIZ1(K)=PARI0(K)
      
          DO 100 I=1,IEUPH
            EK=XK1+XK2*CHLATOT(I)+0.043*C2(I)
          DO 100 J=1,LAY(I)
          JJ=JJ+1
          DO 100 K=1,ITD
            X=EK*SDZ(JJ)
            X1=EXP(-X)
            AVGI(K,JJ)=XIZ1(K)*(1.0-X1)/X
     100    XIZ1(K)=XIZ1(K)*X1
          RETURN
          END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
           SUBROUTINE PRODAVG(IEUPH,MBOT,KK,TD)
    !C***
    !C*** Compute light limitation coefficient on algal growth
    !C*** in each sublayer for TD light periods determined in
    !C*** AVELITE.  (Follows work by J. Cardoni with
    !C*** limitation/inihibition function from Megard).
    !C***
           REAL*8 A,V,TV,ATOP,SVOL
           COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)      
           COMMON/SUB/SDZ(180),SZ(180),LAY(120),AVGI(24,180),SVOL(180)
           COMMON/SOURCE/RM(3,120),PROD(120),XMR(3,120),PRODSUM(120)
           COMMON/PHYTO0/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),GROMAX(3),TMAX(3)
           COMMON/PHYTO1/TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN(90),CHLMAX,SDY(90)
         ! COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
           COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
           COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
           COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
           COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
           DIMENSION PSUB(180)

    !C**  K1 AND K2 are in uEinst m**-2 sec (On June 24, 1992)
    !C**  Transfer coefficient is 10**6/3600 = 1/0.0036           
    !C**  K1 - HSC1(1), K2 - HSC2(1) -- STEFAN & FANG (1993)
    !C**  PO2 is photosynthetic oxygen production at sz(J) over one hour
    !C**  PSUB(J) is photosynthetic oxugen production at sz(J) over a day 
    !C**  SUMPV is volumetric averaged oxygen production at layer I from 
    !C**  the surface layer to layer IEUPH in mg O2/L/day.

           ITD=INT(TD+0.5)
           JJ=0
           DO 200 I=1,IEUPH
            HSC1(KK)=0.687/0.0036*1.086**T20(I)
            HSC2(KK)=10.0/0.0036
            IF(T2(I).LE.10.0) HSC2(KK)=10.0/0.0036
 
            EK=XK1+XK2*CHLATOT(I)+0.043*C2(I)
            XCOE=1.0+2.0*SQRT(HSC1(KK)/HSC2(KK))
            CPMAX=POMAX*1.036**T20(I)*CHLATOT(I)
            
             SUMPV=0.0
             DO 300 J=1,LAY(I)
               JJ=JJ+1
               PSUB(JJ)=0.0
               DO 400 K=1,ITD
                 PO2=CPMAX*AVGI(K,JJ)*XCOE/(AVGI(K,JJ)+HSC1(KK)+AVGI(K,JJ)**2.0/HSC2(KK))
                 
      400        PSUB(JJ)=PSUB(JJ)+PO2
      300        SUMPV=SUMPV+PSUB(JJ)*SVOL(JJ)
      200        PRODSUM(I)=SUMPV/V(I)

         !  write(440,4400)i,sumpv,prodsum(i)
         !    4400 format(1(i6,2x),2(f10.3,3x)) 
      
           IF(IEUPH.LT.MBOT) THEN
            DO 500 I=IEUPH+1,MBOT
      500   PRODSUM(I)=0.0
           ENDIF
           RETURN
           END
    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE PTOTALS(MAXMTH,MXDAY,MXYEAR,CHLMAX,CHLMEANY,SDYY,NUMY,JDYCHL)
    !C***
    !C*** PTOTALS for year-round simulation model (1994) is to estimate
    !C*** or specify Total chlorophyll-a concentration in mg/l.
    !C*** It is different from PTOTALS in Riley's Program !!
    !C***
    !C*2001 Changed common block "CHLAP" and Chlorophyll-a seasonal distribution !!!
          REAL*8 A,V,TV,ATOP
          INTEGER FMON,FDAY,FYEAR,KFIRST,IST,T_DAY
          COMMON/CHLAP/NCDAY(120,2),GCHLA(120,2),ICHLASD,IFIELD,CHLTOP(90),CHLBOT(90),I_NO_FIELD_OUT_PROFILE
          COMMON/NEW/NYEAR,KYEAR(90),FDTH(5),NDEPTH,NTDY(90)
          COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT
          COMMON/NEW2/KYEAR_CHL(90),NTDY_CHL(90),FIELD_CHL(200),CHL_FIELD(2008,366,100),NDAYO_CHL
          COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,HKMXSM,WCFSM,WSSM,WCFSF,WSSF
          COMMON/DOCOE/EMCOE(6),CHLEP(640),CHLHY(640),POMAX,IDNUM(6)
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(640),NCLASS,PLOT(90)
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
         !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
          COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
          COMMON/FIELD/IFLAG(10),FLDATA(10,120),DEPTH(120),NFLD(10),SD,NSKIPDAY,NSDAY(640),SDFIELD(640),TAPE64
          COMMON/DATA40/KFIRST,IST,JZ,MFIRST,MLAST,KI
          COMMON/CHL_A/ICHL_A
    
    !Secchi Depth for eutrophic, mesotrophic, and oligotrophic lakes
    !are SD = 1.2, 2.5, 4.5 m, respectively, for regional lakes (1997).
    !For mesotrphic it is a different ratio for chl-a with same pattern
    !C20** SDYY is SD year by year
    
    !    IF(SDYY.LT.3.0) THEN

          IF(SDYY.LT.4.0) THEN
    !C20 M=1 for eutrophic and mesotrophic lakes
           M=1
          ELSE
    !C20 M=2 for oligotrophic lakes
           M=2
          ENDIF
    	
         KDY=MONTH*100+MDAY 

    !For the first year and first simulation data only 
    !Value of KFIRST and IST should be saved through Common/DATA40/!
         IF(NUMY.EQ.1.AND.KFIRST.LT.10) THEN
                
         DO 107 IH=1,37
    	  
          IF(JDY.LE.NCDAY(IH,M)) THEN
           IST=IH
           GOTO 109
          ENDIF

    107  CONTINUE
    	 
    109	   JZ=JDY-1
           KFIRST=100
          ENDIF

          IF(KDY.EQ.101) THEN
           IST=1
           JZ=1
          ELSE
           JZ=JZ+1
          ENDIF


    !C20** If we have field data WITH chl-a concentrations!!
    !     IF (IFIELD.EQ.1.AND.ICHLASD.EQ.1) THEN
    
        IF (IFIELD.EQ.1.AND.ICHLASD.NE.2) THEN
         
    !Step seasonal distribution/function for Chla concentration
    !CHLTOP OR CHLBOT are mean chlas year by year
    !NUMY is the number of simulation years starting 1 in the first year.
         
          IF(JZ.LE.NCDAY(IST+1,M)) THEN
           DDY=JZ-NCDAY(IST,M)
           DDT=NCDAY(IST+1,M)-NCDAY(IST,M)
           DCH=GCHLA(IST+1,M)-GCHLA(IST,M)
           RATIO=(GCHLA(IST,M)+DCH*DDY/DDT)/100.0
 
 !Fang, 2009 - Add temperature correction for chlorophyll-a growth - NOT USED!!!
           CHLATEMP=1.0
 !         IF(THICE.LE.0.0) CHLATEMP=1.036**(T2(1)-20.0)
 !Test for Thrush Lake with winter DO profiles
 !         IF(CHLATEMP.GT.1.0) CHLATEMP=1.0
         
 !Fang, 2009 - Add correction during ice cover period for chlorophyll-a growth NOT greater than mean!
 !         IF(THICE.GT.0.0.AND.RATIO.GT.0.0) RATIO=0.0
                     
           CHLATOT01=(1.0+RATIO)*CHLTOP(NUMY)*CHLATEMP
           CHLATOT02=(1.0+RATIO)*CHLBOT(NUMY)*CHLATEMP                        
        
  ! Shoeb Alam, Sep 2009 - Here the field Chl-a values will be used for those dates when field values are available
  ! The intermediate days between field values will be interpolated values
  ! Reading of field values ( deducting the missing values which were previously filled 
  ! with average values) and interpolation is done in START subroutine             
        
  ! If ICHL_A is 1.0, field data and interpolated values are used.            
                 IF(ICHL_A.EQ.1)THEN      
                   I=MYEAR
                                                            
                   J=JULIANDAY
                              
                     DO 1110 K=1,NDAYO_CHL      
                                      
                         IF(CHL_FIELD(I,J,K).GT.0.0.AND.I.EQ.MYEAR.AND.J.EQ.JULIANDAY)THEN
                                      
                         CHLATOT01=CHL_FIELD(I,J,K)
                         CHLATOT02=CHL_FIELD(I,J,K)
                                        
                          GO TO 1120
                                
                          ENDIF
             1110   CONTINUE
 
                 ENDIF
          
 
     
   1120       ENDIF
    	
!        IF(NUMY.EQ.2) THEN
!         WRITE(*,*) JZ,CHLATOT01,CHLATOT02
!        ENDIF

          IF(JZ.EQ.NCDAY(IST+1,M)) IST=IST+1
    	
    !C*2001* 01 in mixed layer & 02 in colder bottom layer.
    !EMCOE(5)-Chlorophylla- check
        
          DO 108 I=1,MBOT
              CHLATOT(I)=CHLATOT01
            IF(Z(I).GT.Z(ILAY)) THEN
              CHLATOT(I)=CHLATOT02            
            ENDIF
108       CONTINUE
         
         IF(JDYCHL.GT.0) THEN
          WRITE(56,1030)
1030      FORMAT('Year',1X,'Month',1X,'Day',1X'JDay',1x,'CumJDay',1X,'Chl-aTop',1X,'ChlaBot',1X,'Chl-Temp',1X,'XK1',5X,'SD (m)') 
          JDYCHL=-60
         ENDIF
        
         WRITE(56, 106) MYEAR,MONTH,MDAY,JULIANDAY,JDY,CHLATOT01,CHLATOT02,CHLATEMP,XK1,SD
106      FORMAT(I4,2X,I2,2X,I2,2X,I3,2X,I5,5(1X,F8.5))
         
         IF(KDY.EQ.NPRNT(NDAYS).AND.MYEAR.EQ.KYEAR(NYEAR)) THEN
          WRITE(198,*)
          WRITE(198,*) 'JULIANDAY,KDY,CHLATOT01,CHLATOT02'
          WRITE(198,*) JULIANDAY,KDY,CHLATOT01,CHLATOT02
         ENDIF
   
    	ENDIF
    	  
    	IF(ICHLASD.EQ.2) THEN
    !C*** ESTIMATE CHLOROPHYLL CONCENTRATION BEFORE 12/31/2001 
    !C*** CHLEP(1) is the initial data in input files, i.e. Chla=16 mg/l
    !C*** for eutrophic lakes. CHLEP(2) will be the first field data in
    !C*** the input data file. Please check "START" in lake2.for.
    !C*** CHLABOT is the chlorophyll-a concentration in the bottom layer.

    !     KDY=MONTH*100+MDAY 
          KI=NDAYS
          
    !C*Use the initial & constant chlorophyll-a concentration before you
    !C*have the first year with field data
          IF(MYEAR.LT.KYEAR(1)) THEN
           CHLATOT(1)=CHLEP(1)
           CHLABOT=CHLEP(1)
           GOTO 700
          ENDIF 
             
    !C*** The chlorophyll values will be the mean value of nearest field
    !C*** data, except after the last field data point.  IDNUM(2)>0
          IF(MYEAR.EQ.KYEAR(1)) THEN
            MFIRST=1
            MLAST=NTDY(1)
          ENDIF

         IF(KDY.EQ.101) THEN
  !C*Use the same step distribution for previous year for those years
  !C*without field data, but after a year with field data!!!
           IF(MYEAR.NE.KYEAR(NYEAR)) THEN
             MFIRST=KI-NTDY(NYEAR)
             MLAST=MFIRST+NTDY(NYEAR)-1
             NDAYS=KI-NTDY(NYEAR)
           ELSE
             MFIRST=KI
             MLAST=MFIRST+NTDY(NYEAR)-1
           ENDIF
           
           WRITE(56,*)"MYEAR,NYEAR,KYEAR(NYEAR)"
           WRITE(56,*) MYEAR,NYEAR,KYEAR(NYEAR)
           WRITE(56,*)"MFIRST,MLAST,NTDY(NYEAR),KI"
           WRITE(56,*) MFIRST,MLAST,NTDY(NYEAR),KI
           
         ENDIF

    !C*Take an half during winter ice cover season!
         IF(THICE.GT.0.0) THEN
           CHLATOT(1)=CHLEP(1)/2.0
           CHLABOT=CHLHY(1)/2.0
           GOTO 656
         ENDIF   

    !C*Use constant Chl-a before the last field data!
         IF(KDY.LE.NPRNT(MFIRST).AND.THICE.EQ.0.0) THEN
           CHLATOT(1)=CHLEP(MFIRST+1)
           CHLABOT=CHLHY(MFIRST+1)
           GOTO 656
         ENDIF  

    !C*Use constant Chl-a after the last field data!
         IF(KDY.GT.NPRNT(MLAST).AND.THICE.EQ.0.0) THEN
           CHLATOT(1)=CHLEP(MLAST+1)
           CHLABOT=CHLHY(MLAST+1)
           GOTO 656
         ENDIF  
           
    !C*The chlorophyll values will be the mean value of nearest field data
         IF(KDY.LE.NPRNT(KI).AND.KDY.GT.NPRNT(KI-1)) THEN
           CHLATOT(1)=(CHLEP(KI)+CHLEP(KI+1))/2.0
           CHLABOT=(CHLHY(KI)+CHLHY(KI+1))/2.0
           GOTO 656
         ENDIF
             
    656  CONTINUE

    !C*Use the same step distribution for previous year for those years
    !C*without field data, but after a year with field data!!!
          IF(MYEAR.NE.KYEAR(NYEAR).AND.KDY.EQ.NPRNT(KI)) NDAYS=NDAYS+1

    ! Shoeb 2010 , checking and found that Chla at bottom means from bottom upto mixed layer.

    700  DO 116 I=2,MBOT
             CHLATOT(I)=CHLATOT(1)
           IF(Z(I).GT.Z(ILAY)) THEN
             CHLATOT(I)=CHLABOT            
           ENDIF
    116  CONTINUE

         WRITE(56, 1060) JULIANDAY,JDY,CHLATOT(1),CHLABOT
    1060 FORMAT(1X,I5,2X,I5,1X,F8.5,1X,F8.5)    
        ENDIF
        
    !C20** If NO field data, we use a seasonal pattern !
        
        IF(IFIELD.NE.1) THEN
   
    !Use seasonal distribution/function for Chla concentration
    !C20 CHLMEANY = CHLMEAN(NUMY) is mean chlorophyll-a year by year
   
          IF(JZ.LE.NCDAY(IST+1,M)) THEN
           DDY=JZ-NCDAY(IST,M)
           DDT=NCDAY(IST+1,M)-NCDAY(IST,M)
           DCH=GCHLA(IST+1,M)-GCHLA(IST,M)
           RATIO=(GCHLA(IST,M)+DCH*DDY/DDT)/100.0
           CHLATOT(1)=(1.0+RATIO)*CHLMEANY
          ENDIF
    	
          IF(JZ.EQ.NCDAY(IST+1,M)) IST=IST+1
    	
        DO 118 I=2,MBOT
            CHLATOT(I)=CHLATOT(1)

!Fang, 2008 - This is from FRE96.FOR program
!C**** Doubling chlorophyll-a during the summer below
!C**** the mixed layer depth in the oligotrophic lakes.

!Fang 2010 - Use EMCOE(5) introduced in 2010 for cisco lake !!!!
!Fang 2010 - EMCOE(5) is used for CHLBOT in START subroutine when field data are available.
        IF(SDYY.GT.4.0) THEN
           IF(THICE.LE.0.0.AND.I.GT.ILAY) THEN
            CHLATOT(I)=EMCOE(5)*CHLATOT(1)
           ENDIF
        ENDIF
                    
   118  CONTINUE
        
          IF(SDYY.GT.4.0.AND.THICE.LE.0.0) THEN
           WRITE(56, 1060) JULIANDAY,JDY,CHLATOT(1),EMCOE(5)*CHLATOT(1)
          ELSE
           WRITE(56, 1060) JULIANDAY,JDY,CHLATOT(1),CHLATOT(1)
          ENDIF
                   
        ENDIF
    	
    !C*  Determine mximum chlorophyll-a concentration
          IF(CHLATOT(1).GT.CHLMAX) THEN
            CHLMAX=CHLATOT(1)
            MXDAY=MDAY
            MAXMTH=MONTH
            MXYEAR=MYEAR
          ENDIF

        RETURN
        END

    !C**********************************************************C
    !                                                           C
    !                                                           C
    !C**********************************************************C
          SUBROUTINE EUPHZ(ZEUPH,IEUPH,ALBEDO)
    !C***
    !C*** Determine the limit of the euphotic zone given as
    !C*** the layer in which the light intensity is less than
    !C*** one percnet of the surface value.  
    !C***
          REAL*8 A,V,TV,ATOP
          INTEGER FMON,FDAY,FYEAR
          COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
          COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
           COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
          COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
          COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR
          COMMON/STEPS2/MBOT,ILAY
          COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR,XK1_INPUT

    !C*  It is necessary to further test for winter assumption !
          IF(THICE.GT.0.0) THEN
          
    !Fang, 4/2/2009 - See output ILAY, MBOT in Snowice.dat file
    !Sometime mixed layer depth before freez or after melting, it could have ILAY = MBOT
    !     IF(ILAY.EQ.MBOT) WRITE(198,*) JDY,MONTH,MDAY,ILAY
          
          IF(ILAY.EQ.MBOT) THEN
           AVGCHL=(CHLATOT(1)+CHLATOT(ILAY))/2.0
          ELSE 
           AVGCHL=(CHLATOT(1)+CHLATOT(ILAY+1))/2.0
          ENDIF
  
            EK=XK1+XK2*AVGCHL+0.043*C2(1)
            ZEUPH=ALOG(0.01)/(-EK)
            DO 137 I=1,MBOT
            IF(Z(I).GE.ZEUPH) THEN
              ZEUPH=Z(I)
              IEUPH=I
              GOTO 139
            ENDIF
      137  CONTINUE
          ENDIF   
               
    !    EK=XK1+XK2*CHLATOT(1)+0.043*C2(1)
    !    EMIX=EXP(-EK*Z(ILAY))         
    !    IF(EMIX.GT.0.01) THEN
    !      EK=XK1+XK2*CHLATOT(ILAY+1)+0.043*C2(ILAY+1)
    !      ZEUPH=Z(ILAY)+ALOG(0.01/EMIX)/(-EK)
    !    ELSE
    !      ZEUPH=ALOG(0.01)/(-EK)
    !    ENDIF           
    !    IF(ZEUPH.GT.Z(MBOT)) ZEUPH=Z(MBOT)
    !C** CONVERT ZEUPH TO DEPTH IN SOME LAYER
    !    DO 138 I=1,MBOT
    !      IF(Z(I).GE.ZEUPH) THEN
    !        ZEUPH=Z(I)
    !        IEUPH=I
    !        GOTO 139
    !      ENDIF
    !138 CONTINUE

    !C*  Why did Riley need ALBEDO ? (Fang, 1994)

          EK=XK1+0.043*C2(1)+XK2*CHLATOT(1)
          EX=(1.0-ALBEDO)*EXP(-EK*(Z(1)+DZ(1)/2.0))
          EX=EXP(-EK*(Z(1)+DZ(1)/2.0))
          IF(EX.GT.0.01) GO TO 2
          ZEUPH=Z(1)+DZ(1)*0.5
          IEUPH=1
          GOTO 139
     2    DO 100 I=2,MBOT
    !C...ATTENUATION COEFF. (EK).....
 
          EK=XK1+0.043*C2(I)+XK2*CHLATOT(I)
    !C...DETERMINATION OF EUPHOTIC DEPTH (ZEUPH) .....
          EX=EX*EXP(-EK*DZ(I))
          IF(EX.LE.0.01) THEN
          ZEUPH=Z(I)+DZ(I)*0.5
          IEUPH=I
          GOTO 139
          END IF
     100  CONTINUE
          ZEUPH=ZMAX
          IEUPH=MBOT 

     139  RETURN
          END

    !C**********************************************************C
    !                                                               C
    !                                                               C
    !C**********************************************************C
           SUBROUTINE PREICE(QNEG,TBOUN,NP)
    !C** Abstract information from water temperature profile before
    !C** the ice formation on the lake surface

           REAL*8 A,V,TV,ATOP
           INTEGER FMON,FDAY,FYEAR
           !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
           COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
           COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
           COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
           COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
           COMMON/STEPS2/MBOT,ILAY
           COMMON/PICE2/TMEAN,TEMAX
    
    !C... TESTNG NATURAL CONVECTIVE MIXING
            QNEG=0.0
            TEMAX=0.0
            NP=0
            TMEAN=0
                          
    !C... Determine maximum temperature Tmax in the profile
    !C... Determine how many layers NP have negative temperature     
         IF(T2(1).LT.4.0) THEN
 
            DO 25 I=1,MBOT-1
             IF(TEMAX.LT.T2(I)) TEMAX=T2(I)
             IF(T2(I).LT.0.0.AND.T2(I+1).GT.0.0) NP=I
     25     CONTINUE

    !C... Determine volumetric average Tmean for positive temperature 
            
             JJ=NP+1
             TVDUM=T2(JJ)*V(JJ)
             VDUM=V(JJ)

             DO 410 MK=JJ,MBOT-1
               TVDUM=TVDUM+T2(MK+1)*V(MK+1)
               VDUM=VDUM+V(MK+1)
     410     CONTINUE
             TMEAN=TVDUM/VDUM
             
    !C... Force convective mixing !!      
             IF(TEMAX.GT.4.0.AND.TMEAN.GT.4.0) THEN    
              MJ=1
              TVDUM=T2(MJ)*V(MJ)
              VDUM=V(MJ)

     15       TVDUM=TVDUM+T2(MJ+1)*V(MJ+1)
              VDUM=VDUM+V(MJ+1)
              TDUM=TVDUM/VDUM
              MJ=MJ+1
           
              IF(TDUM.LT.4.0.AND.MJ.LT.MBOT) GOTO 15
                   
     28       DO 20 II=1,MJ
               T2(II)=TDUM
     20       CONTINUE
           
              IF(MJ.EQ.MBOT) GOTO 88        
             ENDIF

!C* One possiblity is TMAX < 0 for well mixed lakes, e.g. Mille Lacs Lake
             IF(TEMAX.LT.0.0) THEN
              
             TBOUN=0.0
            
	        DO 350 II=1,MBOT
               QNEG=QNEG+1000.0*1.0*V(II)*(TBOUN-T2(II))
     350      CONTINUE
     
              DO 365 II=1,MBOT
               T2(II)=0.0
     365      CONTINUE
           
             ENDIF
             
           IF(TMEAN.LE.4.0.AND.NP.NE.0) THEN
           
    !C...  TBOUN = 0.0 OR 4.0 is a boundary (Critiical) condition !
              TBOUN=0.0
                   
    !C... QEXTR is in kcal/day
    !C... QNEG is extra heat loss in kcal/(m*m) !
    

             DO 30 II=1,NP
              QNEG=QNEG+1000.0*1.0*V(II)*(TBOUN-T2(II))
     30      CONTINUE

             DO 435 II=1,NP
               T2(II)=TBOUN
     435     CONTINUE

             DO 445 II=NP+1,MBOT
              IF(T2(II).LT.0.0) T2(II)=0.0
     445     CONTINUE
           
             ENDIF
          
         ENDIF
        
     88     RETURN
            END        
    !C**********************************************************C
    !                                                                C
    !                                                                C
    !C**********************************************************C
           SUBROUTINE POSTMIX(TBOUN,THICE,QNEG,TECON,TSAVE,RKE,NP)
    !C** Determine the mixed layer depth for water temperature 
    !C** profile just before the ice formation   
           REAL*8 A,V,TV,ATOP,RHO
           INTEGER FMON,FDAY,FYEAR
          !COMMON/VOL/ZMAX,DZ(120),Z(120),A(120),V(120),TV(120),ATOP(121),DBL
           COMMON/VOL1/A(120),V(120),TV(120),ATOP(121)
           COMMON/VOL2/ZMAX,DZ(120),Z(120),DBL 
           COMMON/RESULTX/T2(120),CHLATOT(120),PA2(120),PTSUM(120),BOD2(120),DSO2(120),C2(120),CD2(120),XNO2(120),XNH2(120),CHLA2(3,120),PC2(3,120),XNC2(3,120),T20(120),SI2(120),SMLDATA(10,120)
           COMMON/STEPS1/DZLL,DZUL,NM,NPRINT,MDAY,MONTH,JDY,JULIANDAY,FMON,FDAY,FYEAR 
           COMMON/STEPS2/MBOT,ILAY
            DIMENSION TECON(120),TSAVE(120),DTE(120)
            DIMENSION TNEW(120),TPEL(120)

                IF(THICE.GT.0.0) RETURN
                IF(QNEG.LE.0.0)  RETURN
                                        
    !C...  HAVING ICE FORMATION IF THE WIND CAN NOT MIX DOWN
    !C...  TO ILAY, THEREFOR T2(1) = TBOUN
                ICHECK=-100
         
                IF(T2(1).EQ.TBOUN) THEN
                 QICE=QNEG
                 THICE=QICE/(1000.0*80.0*ATOP(1))
                 QICE=0.0
                 DO 475 II=1,ILAY
                   T2(II)=TBOUN
475              CONTINUE

                 GOTO 409
                ENDIF
  
  !C... T2(1) CAN NOT LESS THAN TBOUN !!
                IF(T2(1).GT.TBOUN) THEN
                
    !C...  CALCULATE TEMPERATURE DROP BY QNEG
    !C...  1000.0 IS WATER DENSITY IN KG/M**3
    !C...  1.0 IS Cp, Heat capacity in Kcal/kg-C
    !C...  TSAVE is the mixed layer temperature at each depth.
               
    !            WRITE(99,805)
    !805         FORMAT(1X/1X,'LAYER',6X,'DTE(i)',3X,'TNEW(i)',3X,'TMIX(i)')
                             
                 DO 411 II=ILAY,NP,-1 
                  DTE(II)=QNEG/(1.0*1000.0*TV(II))

    !C...  ADJUSTIFIED WATER TEMPERATURE TNEW
                  TNEW(II)=TSAVE(II)-DTE(II)
                  
    !            WRITE(99,800) II,DTE(II),TNEW(II),TSAVE(II)
    !800         FORMAT(3X,I3,3(3X,F7.3))
      
      411         CONTINUE
                 
                ENDIF  
              
    !C... TNEW < TBOUN, WE NEED FIND A NEW QICE FOR FREEZ-OVER
    !C... TO DETERMINE ICE THICKNESS

     422        IF(TNEW(ILAY).LE.TBOUN) THEN  
                 QICE=1000.0*1.0*TV(ILAY)*(TBOUN-TNEW(ILAY))
                 THICE=QICE/(1000.0*80.0*ATOP(1))

                 IF(ICHECK.GT.0) THEN
                  ICHECK=-100
                  GOTO 409
                 ENDIF 

                 DO 415 II=1,ILAY
                    T2(II)=TBOUN
     415         CONTINUE
                 GOTO 409
               ENDIF

                IF(ICHECK.GT.0) THEN
                 ICHECK=-100
                 GOTO 409
                ENDIF 

    !C... FURTHER CHECK MIXING DEPTH Zm, ILAY
    !C... CALCULATE OF POTENTIAL ENERGY OF MIXED LAYER
    !C... ZCM - Zg GRAVITY CENTER

            DO 433 IK=ILAY-1,NP,-1
              SUM1=V(1)
              SUM2=Z(1)*V(1)
             IF(IK.GT.1) THEN
              DO 430 II=2,IK
                SUM1=SUM1+V(II)
                SUM2=SUM2+Z(II)*V(II)
      430     CONTINUE
             ENDIF
              DCML=SUM2/SUM1

              DENH=RHO(TECON(IK+1),C2(IK+1),CD2(IK+1))

              IF(TNEW(IK).LT.TBOUN) THEN
               DENL=RHO(0.0,C2(IK),CD2(IK))
              ELSE
               DENL=RHO(TNEW(IK),C2(IK),CD2(IK))
              ENDIF

              TPEL(IK)=9.81*TV(IK)*(Z(IK)+DZ(IK)/2.0-DCML)*(DENH-DENL)
     433    CONTINUE        
            
    !       WRITE(99,810)
    ! 810     FORMAT(/3X,'Imix',8X,'TPE(i)',9X,'TKIN(i)')         
            
            DO 455 II=ILAY-1,2,-1
             
    !        WRITE(99,566) II,TPEL(II),RKE
    !566     FORMAT(3X,I3,3X,F12.1,3X,F12.1)         
             
             IF(TPEL(II).LT.RKE) THEN
             
              IF((II+1).EQ.ILAY) THEN
               ICHECK=-100
               DO 420 IK=1,ILAY
                 T2(IK)=TNEW(ILAY)
     420       CONTINUE
               GOTO 409
              ENDIF
               
              ILAY=II+1
     !Fang, 4/2/2009          
     !         IF(ILAY.GE.MBOT) ILAY=MBOT
              
              DO 408 IK=1,MBOT
               IF(IK.LE.ILAY) THEN
                
                IF(TNEW(ILAY).GT.0.0) THEN
                 T2(IK)=TNEW(ILAY)
                ELSE
                 T2(IK)=0.0
                ENDIF
                  
               ELSE
                T2(IK)=TECON(IK)
               ENDIF  
     408      CONTINUE
              ICHECK=100
              GOTO 409
             ENDIF
     455    CONTINUE
               
     409    IF(ICHECK.GT.0) GOTO 422
             
              RETURN
              END
    !C***********************************************
   
    !C***********************************************
        SUBROUTINE MAKEFILE(PATH,TMPFILE,FFILE)
!       CHARACTER*1 PATH(80),TMPFILE(40),FFILE(100)  modify by LiZhongshun 2012-07-11
    	CHARACTER*1 PATH(*),TMPFILE(*),FFILE(*)  
        DO I = 1, 80
           FFILE(I) = ' '
        ENDDO
        DO I = 1, 80
           IF(PATH(I).NE.' ') THEN
   	           FFILE(I) = PATH(I)
             ELSE
             J = I
             GOTO 2001
           ENDIF
        ENDDO
     2001	DO I = 1, 40
           FFILE(J+I-1) = TMPFILE(I)
        ENDDO

        RETURN
        END
  !C************************************************************
    
    !  Read the weather data for MINLAKE95 from CCC GCM output
    !  (2xCO2 and 1xCO2 climate scenarios)
    !  By Xing Fang (June, 1995)
   
    !C***********************************************************
           SUBROUTINE FSCENA(KNAME,SLON,SLAT)
           PARAMETER(IDM=30,JDM=19,IJDM=IDM*JDM)
           DIMENSION DAT(IDM,JDM),DDD(IJDM),SDX(4)
           DIMENSION ALON(30),ALAT(19),RES(12,7)
           COMMON/MODEL/MODELS_SCENARIO
           CHARACTER NLAB*80,NBF*80,LLAB*80,IFID*4,ITYP*4,LTYP*4
           CHARACTER KNAME*20,PIN(7)*4,PTL(7)*40,SBF*80
           EQUIVALENCE (DAT,DDD)
           DATA ALAT/20.14,24.12,27.83,31.54,35.26,38.97,42.68,46.39,50.10,53.81,57.52,61.23,64.94,68.65,72.36,76.07,79.78,83.48,87.16/
              
           
           IF(MODELS_SCENARIO.EQ.1)THEN 
            
           OPEN(376,FILE='#ANALYSIS\co1.dat')
           ICO=1
          
           PIN(1)='  ST'
           PIN(2)='  SQ'
           PIN(3)='SWMX'
           PIN(4)='CLDT'
           PIN(5)=' PCP'
           PIN(6)='BALG'
           PIN(7)=' SNO'
           
           PTL(1)='Air Temperature (C) '
           PTL(2)='Specific Humidity '
           PTL(3)='Mean Wind Speed (m/s) '
           PTL(4)='Total Cloud Cover '
           PTL(5)='Precipitation '
           PTL(6)='Incoming Solar Radiation '
           PTL(7)='Snow Mass/Area '
           
            ALON(1)=150          
           DO 100 I=2,IDM
            ALON(I)=ALON(I-1)-3.75
     100   CONTINUE	
           DO 105 I=1,IDM-1
            IF(SLON.LT.ALON(I).AND.SLON.GT.ALON(I+1)) THEN
              IA=I
              GOTO 106
            ENDIF
     105   CONTINUE          
     106   DO 108 I=1,JDM-1
            IF(SLAT.GT.ALAT(I).AND.SLAT.LT.ALAT(I+1)) THEN
              IB=I
              GOTO 109
            ENDIF
     108   CONTINUE

           !DETERMINE THE NEAREST GRID POINT !        
     109   SDX(1)=SQRT((SLON-ALON(IA))**2+(SLAT-ALAT(IB))**2)
           SDX(2)=SQRT((SLON-ALON(IA))**2+(SLAT-ALAT(IB+1))**2)
           SDX(3)=SQRT((SLON-ALON(IA+1))**2+(SLAT-ALAT(IB))**2)
           SDX(4)=SQRT((SLON-ALON(IA+1))**2+(SLAT-ALAT(IB+1))**2)
           
            SDM1=SDX(1)
             M=1
           DO 119 K=2,4
            IF(SDX(K).LT.SDM1) THEN
             SDM1=SDX(K)
             M=K
            ENDIF
     119   CONTINUE        
           
           IF(M.EQ.2) THEN
            IB=IB+1
           ENDIF 

           IF(M.EQ.3) THEN
            IA=IA+1
           ENDIF 

           IF(M.EQ.4) THEN
            IA=IA+1
            IB=IB+1
           ENDIF 

666        CONTINUE
           
     
    !C** OUTPUT INFORMATION
    
           WRITE(376,115) KNAME
     115   FORMAT(1X,'The station is  ',A20)       
           WRITE(376,120) SLON,SLAT
     120   FORMAT(1X,'Logitude ',F6.2,'W',5X,'Latitude ',F6.2,'N')
 
           WRITE(376,125) ALON(IA),ALAT(IB)
     125   FORMAT(/1X,'Numerical Grids Used (Nearest ponit):' /1X,'Longitude - ',F6.2,'W',8X,'Latitude  - ',F6.2,'N',5X)
        

           WRITE(376,130) 
     130   FORMAT(1X//1X,'Canadian Climate Centre GCM OUTPUT:'/1X)

           IF(ICO.EQ.1) THEN
            OPEN(71,FILE='..\#COMMON\FUTURE_CLIMATE\canada1.dat')
           ELSE
            OPEN(71,FILE='..\#COMMON\FUTURE_CLIMATE\canada2.dat')
           ENDIF
     
           IF(ICO.EQ.1) THEN
            OPEN(72,FILE='..\#COMMON\FUTURE_CLIMATE\sco1.dat')
           ELSE
            OPEN(72,FILE='..\#COMMON\FUTURE_CLIMATE\sco2.dat')
           ENDIF
     
           NR=0
           NF=0
           NRO=0
           NLAB=' '
        2  NF=NF+1
        5  CONTINUE
           READ(71,'(A)',END=90)NBF
        6  NCE=0
           LLAB=NLAB
           NLAB=NBF
           NR=NR+1
           READ(NBF,'(1X,A4,I10,1X,A4,5I10,10X)',ERR=80)IFID,IPER,ITYP,ILEV,MDM,NDM
           
   
           DO 12 II=1,IJDM,5
           IE=II+4
           IF(IE.GT.IJDM) IE=IJDM
           READ(71,'(A)')NBF
           READ(NBF,'(5E15.6,5X)',END=78)(DDD(N),N=II,IE)
       12  CONTINUE
           IF(ITYP.EQ.LTYP) GO TO 5
           LTYP=ITYP
           
           DO 150 K=1,7
            
            IF(ITYP.EQ.PIN(K)) THEN

    !READ THE INCOMING SOLAR RADAITION DATA
             IF(K.EQ.6) THEN
              READ(72,'(A)',END=90)SBF

              DO 122 II=1,IJDM,5
               IE=II+4
               IF(IE.GT.IJDM) IE=IJDM
               READ(72,'(A)')NBF
               READ(NBF,'(5E15.6,5X)',END=78)(DDD(N),N=II,IE)
     122     CONTINUE
            
             ENDIF

             RES(IPER,K)=DAT(IA,IB)

            ENDIF
     150   CONTINUE
     
           NRO=NRO+1
           GO TO 5
       78  CONTINUE
           WRITE(376,*)' ERROR IN RECORD DECODE '
           WRITE(376,1002)NR,NBF
           IF(NBF(21:22).EQ.'  ') GO TO 6
           GO TO 85
       80  CONTINUE
           WRITE(376,*)' ERROR IN RECORD DECODE '
           WRITE(376,1002)NR,NBF
     1002  FORMAT(1X,I8,A)
    !LOOK FOR NEXT HEADER
     
       85  READ(71,'(A)',END=90)NBF
           IF(NBF(21:22).NE.'  ') GO TO 85
           GO TO 6
       90  CONTINUE
           
           DO 160 K=1,7
             WRITE(376,148) PTL(K)
     148     FORMAT(1X/1X,A30/1X)         
            DO 160 J=1,12
             WRITE(376,155) J,RES(J,K)
     155     FORMAT(1X,I2,3X,E16.6)

     160   CONTINUE
     
           IF(ICO.EQ.1) THEN
            ICO=2
            CLOSE (376)
    
            OPEN(376,FILE='#ANALYSIS\co2.dat')
            GOTO 666
           ELSE
            CLOSE (376)
            CLOSE (71)
            CLOSE (72)
            CALL COMBINE
           ENDIF
           
           ENDIF
           
           IF(MODELS_SCENARIO.GE.2)CALL COMBINE 
           
           RETURN
           END
    !C**********************************************C
    
    !Combine the 1xCO2 and 2xCO2 output results
    !to get the difference or ratios
    !(By Xing Fang (June, 1995)
    
    !C*********************************************C
           SUBROUTINE COMBINE
           DIMENSION COE(7),CO1(12,7),CO2(12,7)
           COMMON/FWEA/FTAR(12),FDEW(12),FWIN(12),FTCL(1200),FPRE(12),FSOL(12)
           COMMON/GCM1/FUTR_PRECIP_CHANGE(12),FUTR_HUM_CHANGE(12),FUTR_TEMP_CHANGE(12),FUTR_SOLAR_CHANGE(12),FUTR_NWIND_CHANGE(12),FUTR_EWIND_CHANGE(12),FUTR_WIND_CHANGE(12)
           COMMON/GCM2/GRID_PRECIP(400,400,12),GRID_HUM(400,400,12),GRID_TEMP(400,400,12),GRID_SOLAR(400,400,12),GRID_NWIND(400,400,12),GRID_EWIND(400,400,12),GRID_WIND(400,400,12)
           COMMON/GCM3/GRID_LATITUDE(400),GRID_LONGITUDE(400)
           COMMON/MODEL/MODELS_SCENARIO
           COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(81,36),ELEV(81,36)
           CHARACTER KNAME*20,PIN(7)*30,PTL(7)*40,OUTPUT*60
        
          IF(MODELS_SCENARIO.EQ.1)THEN 
           OPEN(71,FILE='#ANALYSIS\co1.dat')
           OPEN(72,FILE='#ANALYSIS\co2.dat')

    !C*1XCO2 OUTPUT ****************

           READ(71,115) KNAME
     115   FORMAT(17X,A20)

           OPEN(376,FILE='#ANALYSIS\fco2.dat')

           READ(71,120) SLON,SLAT
     120   FORMAT(10X,F6.2,15X,F6.2)

           READ(71,125) ALON,ALAT
     125   FORMAT(/1X/13X,F6.2,21X,F6.2)

           READ(71,130) OUTPUT
     130   FORMAT(1X//1X,A60/1X)
           
           DO 160 K=1,7
             READ(71,148) PTL(K)
     148     FORMAT(1X/1X,A30/1X)         
            DO 160 J=1,12
             READ(71,155) JJ,CO1(J,K)
     !       IF(K.EQ.6)   WRITE(360,*)'CO1',CO1(J,K)
     155   FORMAT(1X,I2,3X,E16.6)
     160   CONTINUE


    !C*2XCO2 OUTPUT ****************
           READ(72,115) KNAME
           READ(72,120) SLON,SLAT
           READ(72,125) ALON,ALAT
           READ(72,130) OUTPUT

           DO 260 K=1,7
             READ(72,148) PTL(K)
            DO 260 J=1,12
             READ(72,155) JJ,CO2(J,K)
        !    IF(K.EQ.6) WRITE(360,*)'CO2',CO2(J,K)
     260   CONTINUE

    !C** OUTPUT INFORMATION
           WRITE(376,215) KNAME
     215   FORMAT(1X,'The station is  ',A20)
           WRITE(376,220) SLON,SLAT
     220   FORMAT(1X,'Logitude ',F6.2,'W',5X,'Latitude ',F6.2,'N')
        

           WRITE(376,225) ALON,ALAT
     225   FORMAT(/1X,'Numerical Grids Used (Nearest point):'/1X,'Longitude - ',F6.2,'W',8X,'Latitude  - ',F6.2,'N',5X)
          

           WRITE(376,230) 
     230   FORMAT(1X//1X,'Canadian Climate Centre GCM OUTPUT:'/1X)

           PTL(1)='Air Temperature (C) '
           PTL(2)='Dew Point Temperature (C)'
           PTL(3)='Mean Wind Speed (mph) '
           PTL(4)='Total Cloud Cover (%) '
           PTL(5)='Precipitation (inches/day)'
           PTL(6)='Incoming Solar Radiation (cal/cm**2/day)'
           PTL(7)='Snowfall (inches/day)'
           
           PIN(1)=' Difference = 2xCO2 - 1xCO2'
           PIN(3)=' Ratio = 2xCO2 / 1xCO2'
           PIN(2)=PIN(1)
           PIN(4)=PIN(3)
           PIN(5)=PIN(3)
           PIN(6)=PIN(3)
           PIN(7)=PIN(1)
           
           COE(1)=1.0
           COE(2)=1.0
           COE(3)=1.0
           COE(4)=1.0
           COE(5)=1.0
           COE(6)=1.0
           COE(7)=0.1312

           DO 360 K=1,7
             WRITE(376,248) PTL(K),PIN(K)
     248     FORMAT(1X/1X,A30,A30/1X)         
            DO 360 J=1,12

             IF(K.EQ.1.OR.K.EQ.7) THEN

              TTAR=COE(K)*(CO2(J,K)-CO1(J,K))
              IF(K.EQ.1) FTAR(J)=TTAR
              WRITE(376,255) J,TTAR
     255      FORMAT(1X,I2,3X,4(5X,F6.2,5X))

             ELSE

              IF(K.EQ.2) THEN
    !         TP=101330
    !         ECO1=CO1(J,K)*TP/0.622
    !         ECO1=ALOG(ECO1/611.0)
    !         DT1=237.3*ECO1/(17.27-ECO1)
    !         ECO2=CO2(J,K)*TP/0.622
    !         ECO2=ALOG(ECO2/611.0)
    !         DT2=237.3*ECO2/(17.27-ECO2)
    !         FDEW(J)=COE(K)*(DT2-DT1)
               FDEW(J)=COE(K)*CO2(J,K)/CO1(J,K)
              ENDIF
              IF(K.EQ.3) THEN
               FWIN(J)=COE(K)*CO2(J,K)/CO1(J,K)
              ENDIF
              IF(K.EQ.4) THEN
               FTCL(J)=COE(K)*CO2(J,K)/CO1(J,K)
              ENDIF
              IF(K.EQ.5) THEN
               FPRE(J)=COE(K)*CO2(J,K)/CO1(J,K)
              ENDIF
              IF(K.EQ.6) THEN
               FSOL(J)=COE(K)*(CO2(J,K)/CO1(J,K))
                
                 !WRITE(370,*)J,FSOL(J)
               
              ENDIF
              
    !        IF(K.EQ.2) THEN
    !         WRITE(376,255)  J,FDEW(J)
    !        ELSE
               WRITE(376,255) J,COE(K)*CO2(J,K)/CO1(J,K)
    !        ENDIF

             ENDIF 
     360   CONTINUE

           CLOSE (71)
           CLOSE (72)
           CLOSE (376)
           ENDIF
     
    ! Implemented by Alam Shoeb in 2009 - 2010           
      2910 IF(MODELS_SCENARIO.GT.1)THEN
    
           IF(MODELS_SCENARIO.EQ.2)THEN
          
            OPEN(120,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_grid_coord.cdl')
            OPEN(121,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_A1B_precipitation.cdl')
            OPEN(122,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_A1B_temperature.cdl')
            OPEN(123,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_A1B_solarRadiation.cdl')
            OPEN(124,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_A1B_sp_Humidity.cdl')
            OPEN(125,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_A1B_wind_north.cdl')
            OPEN(126,FILE='..\#COMMON\FUTURE_CLIMATE\gcm_A1B_wind_east.cdl')
           
            LAT_NO=48
            LONG_NO=96
          
           ENDIF
           
           IF(MODELS_SCENARIO.EQ.3)THEN
           
            OPEN(120,FILE='..\#COMMON\FUTURE_CLIMATE\Had_grid_coord.cdl')
            OPEN(121,FILE='..\#COMMON\FUTURE_CLIMATE\Had_A1B_precipitation.cdl')
            OPEN(122,FILE='..\#COMMON\FUTURE_CLIMATE\Had_A1B_temperature.cdl')
            OPEN(123,FILE='..\#COMMON\FUTURE_CLIMATE\HAd_A1B_solarRadiation.cdl')
            OPEN(124,FILE='..\#COMMON\FUTURE_CLIMATE\Had_A1B_sp_Humidity.cdl')
            OPEN(125,FILE='..\#COMMON\FUTURE_CLIMATE\HAd_A1B_wind_north.cdl')
            OPEN(126,FILE='..\#COMMON\FUTURE_CLIMATE\HAd_A1B_wind_east.cdl')
           
            LAT_NO=1600
            LONG_NO=3200
     
           ENDIF
          
           IF(MODELS_SCENARIO.EQ.5)THEN
            OPEN(120,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_grid_coord.cdl')
            OPEN(121,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_A1B_precipitation.cdl')
            OPEN(122,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_A1B_temperature.cdl')
            OPEN(123,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_A1B_solarRadiation.cdl')
            OPEN(124,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_A1B_sp_Humidity.cdl')
            OPEN(125,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_A1B_wind_north.cdl')
            OPEN(126,FILE='..\#COMMON\FUTURE_CLIMATE\MIHR_A1B_wind_east.cdl')
          
            LAT_NO=160
            LONG_NO=320
           
           ENDIF

            DO I=1,7
             READ(120,*)
            END DO

          
          STATION_LONGITUDE=SLON+180
          STATION_LATITUDE=SLAT
         
           
            READ(120,*)(GRID_LATITUDE(L),L=1,LAT_NO)
           
          
            READ(120,*)
           
            READ(120,*)(GRID_LONGITUDE(K),K=1,LONG_NO)
            
            DO I=1,8
            
             READ(121,*)
             READ(122,*)
             READ(123,*)
             READ(124,*)
             READ(125,*)
             READ(126,*)

            END DO
            
            DO K=1,12            
              
             DO I=1,LAT_NO
                    
           
             READ(121,*)(GRID_PRECIP(J,I,K),J=1,LONG_NO)
             READ(122,*)(GRID_TEMP(J,I,K),J=1,LONG_NO)
             READ(123,*)(GRID_SOLAR(J,I,K),J=1,LONG_NO)
             READ(124,*)(GRID_HUM(J,I,K),J=1,LONG_NO)
             READ(125,*)(GRID_NWIND(J,I,K),J=1,LONG_NO)
             READ(126,*)(GRID_EWIND(J,I,K),J=1,LONG_NO)
           
             END DO
            END DO
          
         
            DO 2450 LONG_CHECK=1,LONG_NO
                   IF(STATION_LONGITUDE.GT.GRID_LONGITUDE(LONG_CHECK).AND.STATION_LONGITUDE.LT.GRID_LONGITUDE(LONG_CHECK+1))THEN
                    DIFF1=ABS(STATION_LONGITUDE-GRID_LONGITUDE(LONG_CHECK))
                    
                    DIFF2=ABS(STATION_LONGITUDE-GRID_LONGITUDE(LONG_CHECK+1))
                  
                    IF(DIFF1.LT.DIFF2)THEN                    
                    INDEX_LONG=LONG_CHECK
                    ELSE
                    INDEX_LONG=LONG_CHECK+1
                    ENDIF
                  
                    !WRITE(*,*)'Station Longitude,Grid longitude'
                    !WRITE(*,3548)STATION_lONGITUDE,GRID_LONGITUDE(INDEX_LONG)
                  
                    GO TO 2550
                    
                    
                    ENDIF
      2450   CONTINUE
            
            
      2550      DO 2560 LAT_CHECK=1,LAT_NO
                    IF(STATION_LATITUDE.GT.GRID_LATITUDE(LAT_CHECK).AND.STATION_LATITUDE.LT.GRID_LATITUDE(LAT_CHECK+1))THEN
                    DIFF3=ABS(STATION_LATITUDE-GRID_LATITUDE(LAT_CHECK))
                    
                    DIFF4=ABS(STATION_LATITUDE-GRID_LATITUDE(LAT_CHECK+1))
                   
                    IF(DIFF3.LT.DIFF4)THEN                    
                    INDEX_LAT=LAT_CHECK
                     ELSE
                    INDEX_LAT=LAT_CHECK+1
                    
                    ENDIF
                    
                     !WRITE(*,*)'Station Latitude,Grid Latitude'
                     !WRITE(*,3548) STATION_LATITUDE,GRID_LATITUDE(INDEX_LAT)
                     
       3548          FORMAT(1X,F7.3,2X,F7.3)   
                    
                WRITE(370,3782)STATION_LONGITUDE,STATION_LATITUDE ,GRID_LONGITUDE(INDEX_LONG),GRID_LATITUDE(INDEX_LAT)
      3782      FORMAT('Weather St Long and Lat:',F6.2,1X,F6.2,4X,'Grid Long and Lat:',F6.2,1X,F6.2)         
               
                    
                      GO TO 2565
                      ENDIF
      2560   CONTINUE
          
          
   2565        DO  2675 K=1,12
       
               FUTR_PRECIP_CHANGE(K)=GRID_PRECIP(INDEX_LONG,INDEX_LAT,K)
               FUTR_HUM_CHANGE(K)=GRID_HUM(INDEX_LONG,INDEX_LAT,K)
               FUTR_TEMP_CHANGE(K)=GRID_TEMP(INDEX_LONG,INDEX_LAT,K)
               FUTR_SOLAR_CHANGE(K)=GRID_SOLAR(INDEX_LONG,INDEX_LAT,K)
               FUTR_NWIND_CHANGE(K)=GRID_NWIND(INDEX_LONG,INDEX_LAT,K)
               FUTR_EWIND_CHANGE(K)=GRID_EWIND(INDEX_LONG,INDEX_LAT,K)
             
            2675 CONTINUE
    
           
            DO K=1,12
               IF(K.EQ.1)WRITE(370,3786)
  3786         FORMAT(/'Month',3X,'Precipitation (in)')             
               FPRE(K)=FUTR_PRECIP_CHANGE(K)*86400.0/25.4
               WRITE(370,3787)K,FPRE(K)
  3787         FORMAT(I4,1X,F8.4)
              
            END DO  
           
            DO K=1,12
               IF(K.EQ.1)WRITE(370,3788)
  3788         FORMAT(/'Month',3X,'Relative Humidity')  
               FDEW(K)=FUTR_HUM_CHANGE(K)
               WRITE(370,3787)K,FDEW(K)
                 
           END DO
           
            DO K=1,12
               IF(K.EQ.1)WRITE(370,3789)
  3789         FORMAT(/'Month',3X,'Air Temperature (oKelvin)') 
               FTAR(K)=FUTR_TEMP_CHANGE(K)
               WRITE(370,3787)K,FTAR(K)
                 
            END DO
            

           
            DO K=1,12
                IF(K.EQ.1)WRITE(370,3790)
  3790         FORMAT(/'Month',3X,'Solar Radiation(Langley/day)')  
               FSOL(K)=  (FUTR_SOLAR_CHANGE(K)*24.)/11.628
             
               WRITE(370,3787)K,FSOL(K)
              
            END DO
                  
            DO  K=1,12
                IF(K.EQ.1)WRITE(370,3792)
  3792         FORMAT(/'Month',3X,'Wind Speed(mph)')  
               FWIN(K)= (SQRT(FUTR_EWIND_CHANGE(K)**2.+FUTR_NWIND_CHANGE(K)**2.))*2.23693629
           
           
               WRITE(370,3787)K,FWIN(K)
              
            END DO
           
           ENDIF  
                          
           
           RETURN
           END
   
      
       !------------------ 
      
       ! August 2009 Shoeb Alam - following subroutine is introduced to convert field data date(i,e,516)
       !                                   to Julian day (i,e, 516>136) for modifying Chlo-a calculation
          SUBROUTINE MONTHDAY_TO_JULIANDAY(I,KKYEAR,K,NNNPRNT,JDAY_FIELD)
          INTEGER MYEAR,T_DAY,DAY_FIELD
          DIMENSION MONTHDY(12),ANPRNT(150),NNNTDY(90)
        
          CALL LIPYEAR(KKYEAR,T_DAY)
           
           
           MONTH_FIELD=INT(NNNPRNT/100.)
           DAY_FIELD=NNNPRNT-MONTH_FIELD*100
           
           
           JDAY_FIELD=0
               
              
              DO   JMONTH=1,MONTH_FIELD
              
              
               DAYSINMONTH=MONTHDY(JMONTH)
               IF(JMONTH.EQ.2.AND.T_DAY.EQ.366)DAYSINMONTH=29
               
               IF(JMONTH.EQ.MONTH_FIELD)DAYSINMONTH=DAY_FIELD
               DO   JD=1,DAYSINMONTH
                 JDAY_FIELD=JDAY_FIELD+1
               ENDDO
             ENDDO  
      
           
           RETURN
           END
           
    
          SUBROUTINE LIPYEAR(MYEAR,T_DAY)
          INTEGER MYEAR,T_DAY
          FRACTION=(FLOAT(MYEAR)/4.)-INT(MYEAR/4.)
          IF(FRACTION.EQ.0)T_DAY=366
          IF(FRACTION.GT.0)T_DAY=365
          
          RETURN
             
          END
          
   SUBROUTINE CREATDIRECTORY(folder_name)

    USE IFPORT   
    IMPLICIT NONE
    INTEGER :: FEXIST, RES, STATUS
    CHARACTER(len=200)  ::folder_name
    CHARACTER(len=200)  ::command

    ! check if the folder exist, if not, creat it

    INQUIRE(DIRECTORY=TRIM(folder_name),EXIST=FEXIST)
    IF(.not.FEXIST) THEN
        WRITE(*,*) 'The folder does not existing, now create it...'
        res=MAKEDIRQQ(TRIM(folder_name))
        IF (res) THEN
!            WRITE (*,*) 'directory successfully created'
        ELSE
            WRITE (*,*) 'Failed to create directory'
            STOP
        END IF
     END IF
    END          
          
        
          
           
         