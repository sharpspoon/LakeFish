C************************************************************C
C                                                            C
C    UPDATE 08-01-93 FOR TESTING HYPOLIMNETIC Kz             C
C    UPDATE 05-30-97 FOR SNOW/ICE ALBEDO TESTS               C
C    UPDATE 12-16-99 FOR ATP PROJECT                         C 
C                                                            C
C         LAKE MAIN PROGRAM  UPDATED 09-03-1986              C
C         ATTATCH USER SUBROUTINE DURING LINKING             C
C         COMPILED ON Z-158 USING MS-FORTRAN 3.3             C
C************************************************************C
      PROGRAM MINLAKE
      REAL*8 A,V,TV,ATOP,SVOL,RHO
      INTEGER FMON,FDAY,FYEAR,T_FLAG,DO_FLAG,DAYBAK,YEARBAK
CFX
      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)
      COMMON/NEW/NYEAR,KYEAR(25),FDTH(5),NDEPTH,NTDY(25)

      COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),
     + PR(31),DRCT(31)
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/SOURCE/RM(3,40),PROD(40),XMR(3,40),PRODSUM(40)
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,
     + YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
      COMMON/PHYTO/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),
     + XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),
     + GROMAX(3),TMAX(3),TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),
     + HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN,CHLMAX,SECCHI
      COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,
     + XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),
     + REPRO,XI,XKMZ,GRAZE(3,40)
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/SUB/SDZ(90),SZ(90),LAY(40),AVGI(24,90),SVOL(90)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,PLT(30)
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),
     + RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
      COMMON/INFLOW/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),
     + CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
      COMMON/NETHEAT/Q(40)
      COMMON/US/VELTRE
      COMMON/TEFX/T2K(40),TEHE(40),BMK(40),OLDHQ,ITERF 
      COMMON/FIELD/ IFLAG(10),FLDATA(10,50),DEPTH(50),NFLD(10),SD
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
      COMMON/TITL/ TITLE
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/YROUND/ NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,
     + HKMXSM,WCFSM,WSSM,WCFSF,WSSF
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
	COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(61,16),ELEV(61,16)
      COMMON/BAKCUPTDO/T_FLAG,DO_FLAG,MONBAK,DAYBAK,YEARBAK,
     +            	TBAK(40),DOBAK(40)
      DIMENSION STATVAR(80),ICX(4),TETH(5),DOTH(5),TEICE(5)
      DIMENSION TECON(40),TEDIF(40),TEMIX(40),TSAVE(40)

C96** Latitude, longitude of stations and No. station in a state
      DIMENSION SLATD(61,16),SLONG(61,16),JSTA(61),EVAP(20)
      DIMENSION XJAIR(61,16),SUMTSL(10)
      CHARACTER*80 FILENAME
      CHARACTER*64 TITLE, RUNPATH, METERPATH
      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1,TAPE9,TAPE7
      CHARACTER*1 T8(16),XS,NUM(10),T9(16),T7(16),PATH(64),FFILE(80),
     &    	 FILE150(16),FILE151(16),FILE198(16),FILE42(16),FILE7(16),
     &    	 FILE78(16),FILE38(16),FILE48(16),FILE58(16),FILE14(16),
     &         FILE18(16),FILE15(16),FILE99(16),FILE98(16),FILE66(16),
     &         FILE26(16),FILE25(16),FILE36(16),FILE4(16),PATHM(64)
      EQUIVALENCE (STATVAR(1),SUMXY(1)),(TAPE8,T8(1))
	EQUIVALENCE (TAPE7,T7(1)),(TAPE9,T9(1)),(FLO,FILE4(1)),
     &       (RUNPATH,PATH(1)),(FILENAME,FFILE(1)),(METERPATH,PATHM(1))
      LOGICAL LFILE

      DATA NUM/'0','1','2','3','4','5','6','7','8','9'/,
     &	 FILE150/'f','e','d','s','i','m','.','t','e','m',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE151/'f','e','d','s','i','m','.','d','o','x',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE198/'w','a','r','n','i','n','f','o','.','d','a','t',' ',
     &	         ' ',' ',' '/,
C     &	 FILE42 /'g','w','a','t','e','r','.','s','d','f',' ',' ',' ',
C     &	         ' ',' ',' '/,
     &	 FILE7  /'i','n','p','u','t','.','i','n','i',' ',' ',' ',' ',
     &	         ' ',' ',' '/
C      DATA FILE78 /'s','t','a','t','i','o','n','.','s','d','f',' ',' ',
C     &	         ' ',' ',' '/,
      DATA FILE38 /'a','r','e','a','.','s','d','f',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE48 /'f','d','-','s','e','.','s','d','f',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE58 /'f','d','-','c','n','.','s','d','f',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE14 /'t','d','-','s','e','.','s','d','f',' ',' ',' ',' ',
     &	         ' ',' ',' '/
      DATA FILE18 /'s','w','i','c','e','.','d','a','t',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE15 /'t','s','e','d','.','d','a','t',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE99 /'c','h','e','c','k','.','d','a','t',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE98 /'e','r','r','o','r','.','d','a','t',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE66 /'s','e','t','d','.','s','d','f',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/
      DATA FILE26 /'t','d','s','f','.','d','a','t',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE25 /'d','o','s','t','.','s','d','f',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE36 /'d','o','f','t','.','s','d','f',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/
      open(17,file="D:\Inetpub\wwwroot\lake\FortranModel\path.txt")
      READ(17,'(A)') RUNPATH
      READ(17,'(A)') METERPATH
c      READ(*,'(A)') RUNPATH
c      READ(*,'(A)') METERPATH
      IRUN=0
C.... This file "fedsim.tem" contains measured- & simulated T data
      CALL MAKEFILE(PATH,FILE150,FFILE)
	OPEN(150,FILE=FILENAME)

C.... This file "fedsim.dox" contains measured- & simulated DO data
      CALL MAKEFILE(PATH,FILE151,FFILE)
	OPEN(151,FILE=FILENAME)
      
C.... This file "warninfo.dat" contains some outputted warning information
      CALL MAKEFILE(PATH,FILE198,FFILE)
	OPEN(198,FILE=FILENAME)
      WRITE(198,1980)
 1980 FORMAT(1X,'This simulation started under the following '
     +          'conditions!')  
C***** Open the file "gwater.sdf" with yearly mean and January air temperatures
C***** YRAIR(I,J) - Yearly mean air temperature in (oC)
C***** XJAIR(I,J) - January mean air temperature in (oC)
C      CALL MAKEFILE(PATH,FILE42,FFILE)
C      OPEN(42,FILE=FILENAME)
      OPEN(42,FILE="D:\Inetpub\wwwroot\lake\FortranModel\gwater.sdf")
      READ(42,*)
c 452  READ(42,479,END=454) JS,JN,YRAIR(JS,JN),XJAIR(JS,JN),
 452  READ(42,*,END=454) JS,JN,YRAIR(JS,JN),XJAIR(JS,JN),
     + ELEV(JS,JN)
C      write(*,*) JS,JN,YRAIR(JS,JN),XJAIR(JS,JN),
C     + ELEV(JS,JN)
 479  FORMAT(5X,I2,7X,I1,4X,F4.1,3X,F5.1,4X,F6.1)
      GOTO 452
 454  CLOSE(42)

      YSCHL=30.
      HSCSI=0.03
      CONST=.5
      CHLMAX=0.0
      DO 995 I=1,6
  995   IPRNT(I)=0
      DO 996 I=1,10
  996   IFLAG(I)=0
      DO 997 I=1,80
  997   STATVAR(I)=0.0

      IF(IRUN.NE.1) THEN

C20** Input file will be always "input.ini", So we need
C20** group input, geometry data for each lake in separate folder

C** Read input data file Name
C  400   WRITE(*,1001)
C 1001 FORMAT(' ENTER INPUT DATA FILE NAME :',10X,\)
C        READ(*,'(A)') DIN
C          INQUIRE(FILE=DIN,EXIST=LFILE)
C          IF(LFILE) GOTO 401
C          PAUSE 'ERROR ON INPUT FILE NAME'
C          GOTO 400
C 401    OPEN (7,FILE=DIN)
      CALL MAKEFILE(PATH,FILE7,FFILE)
        OPEN (7,FILE=FILENAME)

        READ(7,'(A)') TITLE
     
C** Read weather data file NAME for the first without ".DAT"
C       WRITE(*,1000)
C        READ(*,'(A)') TAPE8

        READ(7,'(A)') TAPE8
	  READ(7,*) ISTATE,ISTATION

C        CALL MAKEFILE(PATH,FILE78,FFILE)
C        OPEN(78,FILE=FILENAME)
        OPEN(78,FILE="D:\Inetpub\wwwroot\lake\FortranModel\station.sdf")
        READ(78,*)
        LAT_FLAG = 0
C 352    READ(78,379,END=354) JS,JN,STATM1,STATM2,
C     +   SLAT,SLON,WSTATETM,WSTATIONTM,WFNAMETM
 352    READ(78,*,END=354) JS,JN,SLAT,SLON
 379    FORMAT(3X,I2,7X,I1,1X,A1,A1,3X,F6.3,3X,F7.3,
     +   A16,A16,1X,A16)
        IF(JS.EQ.ISTATE.AND.JN.EQ.ISTATION) THEN
	     LAT_FLAG = 1
        ELSE
           GOTO 352
	  ENDIF
 354    CLOSE(40)
        IF(LAT_FLAG.EQ.0) THEN
	     WRITE(99,*)"LATITUDE & LONGITUDE were not found in file",
     &	  "- station.sdf!"
           STOP
        ENDIF
        DO 405 I=1,16
          II=16-I+1
	    
		DO 406 IK=1,II
		  T7(IK)=T8(IK)
		  T9(IK)=T8(IK)
 406		CONTINUE
          
		IF(T8(II).NE.' ') THEN
              T8(II+1)='.'
              T8(II+2)='D'
              T8(II+3)='A'
              T8(II+4)='T'
C20**TAPE7 SIMULATED DO CONCENTRATION
			T7(II+1)='.'
              T7(II+2)='D'
              T7(II+3)='O'
              T7(II+4)='X'
C20**TAPE9 SIMULATED TEMPERATURE
			T9(II+1)='.'
              T9(II+2)='T'
              T9(II+3)='E'
              T9(II+4)='P'
              INUM=II-1
              GOTO 402
          ENDIF

 405    CONTINUE
 402    CALL MAKEFILE(PATHM,T8,FFILE)
        INQUIRE(FILE=FILENAME,EXIST=LFILE)
        IF(LFILE) GOTO 412
        PAUSE 'ERROR ON METEOROLIGICAL DATA FILE NAME'
        STOP
C20**   GOTO 401
C *** OPEN FILE "TAPE8"
 412    OPEN (9,FILE=FILENAME,STATUS='OLD')
C *** OPEN FILE "TAPE7"
      CALL MAKEFILE(PATH,T7,FFILE)
        OPEN (75,FILE=FILENAME)
C *** OPEN FILE "TAPE9"
      CALL MAKEFILE(PATH,T9,FFILE)
	  OPEN (76,FILE=FILENAME)

      ENDIF

C     WRITE(*,1002)
C     READ(*,'(A)') MET
C     OPEN (99,FILE='GASDO.DAT')
C     OPEN (13,FILE='SIWOUT.DAT')
C     OPEN (20,FILE='HEATB.DAT')

C20** Lake Geometry file "AREA.SDF"
      CALL MAKEFILE(PATH,FILE38,FFILE)
      OPEN (38,FILE=FILENAME)

C *** OPEN FILE "FD-SE.SDF"
      CALL MAKEFILE(PATH,FILE48,FFILE)
      OPEN (48,FILE=FILENAME)
C *** OPEN FILE "FD-CN.SDF"
      CALL MAKEFILE(PATH,FILE58,FFILE)
      OPEN (58,FILE=FILENAME)
CFX
C**** FOR WINTER SIMULATION STUDY (FILE "TD-SE.SDF")!
      CALL MAKEFILE(PATH,FILE14,FFILE)
      OPEN (14,FILE=FILENAME)

C *** OPEN FILE "SWICE.DAT"
      CALL MAKEFILE(PATH,FILE18,FFILE)
      OPEN (18,FILE=FILENAME)

C *** OPEN FILE "TSED.DAT"
      CALL MAKEFILE(PATH,FILE15,FFILE)
      OPEN (15,FILE=FILENAME) 
C *** OPEN FILE "CHECK.DAT"
      CALL MAKEFILE(PATH,FILE99,FFILE)
      OPEN (99,FILE=FILENAME)
      WRITE(99,1980)

C Set sediment temperature at 10 m below - TSLAV
C Increase 2.0 + 1.0 more for the region with snow
C Only increase 2.0 oC for other regions - July, 1996
       YTAIR=0.0
       IF(XJAIR(ISTATE,ISTATION).LE.-3.0) THEN
        TSLAV=YRAIR(ISTATE,ISTATION)+3.0
       ELSE
        TSLAV=YRAIR(ISTATE,ISTATION)+2.0
       ENDIF

C***** Call LAKE routine to READ geometry data *** 
      NUMY=1 
      CALL LAKE(0.,0.,0,12)

c      write(*,*)"before start" 
      CALL START(ST,S,FT,ISTART,INFLOW,ITER,II,IFIELD,TSLAV,PATH)
c      write(*,*)"After  start" 

C20**If you have field data, then IFIELD=1
      IF(IFIELD.EQ.1) THEN
C20** File "Error.DAT" STORES STATISTCIAL RESULTS
      CALL MAKEFILE(PATH,FILE98,FFILE)
       OPEN (98,FILE=FILENAME)
	 WRITE(98,981)
  981 FORMAT(1X,'Month',2X,'Day',2X,'Year',2X,'Parameter',2X,
     +       'DepthEmax',2X,'|Error|max',2X,     
     +       'DepthEmin',2X,'|Error|min',2X,'|Error|mean')     
C20**Store data for error analysis "SET.SDF"
      CALL MAKEFILE(PATH,FILE66,FFILE)
       OPEN (66,FILE=FILENAME)

C20** Save simulate/measured temperature/DO profiles "TDSF.DAT"
      CALL MAKEFILE(PATH,FILE26,FFILE)
       OPEN (26,FILE=FILENAME)
      ENDIF

CFX-- The inflow routine file is set as INOUT.DAT
      IF(INFLOW.GT.0) THEN
        IF(IRUN.NE.1) THEN
  403     WRITE(*,1003)
 1003 FORMAT(' ENTER INFLOW DATA FILE NAME :',9X,\)
          READ(*,'(A)') FLO
          CALL MAKEFILE(PATH,FILE4,FFILE)
          INQUIRE(FILE=FILENAME,EXIST=LFILE)
          IF(LFILE) GOTO 404
          PAUSE 'ERROR ON INFLOW DATA FILE NAME'
          GOTO 403
C *** OPEN FILE FLO
  404     OPEN (4,FILE=FILENAME)
        ENDIF
        REWIND 4
        READ(4,*) INFLOW
      ENDIF

      ZMAX=ST-DBL
      CALL SETUP
      I=1
 11   IF(DZ(I).GT.DZUL.AND.MBOT.LT.40) THEN
        CALL SPLIT(I,ILAY)
        GO TO 11
      ENDIF
      I=I+1
      IF(I.GT.MBOT.OR.I.GT.40) GOTO 12
      GOTO 11
 12   CALL SETZ(MBOT)
      CALL VOLUME(MBOT)
      CALL AREA
      CALL ABOUND
      CALL TVOL(MBOT)
      IF(MODEL.GT.1) ZP=ZP*TV(MBOT)
C...DETERMINATION OF INITIAL MIXED LAYER DEPTH
      ILAY=1
      DO 7 I=1,MBOT-1
      IF(T2(I).GT.T2(I+1)+CONST) GO TO 8
 7    ILAY=ILAY+1
 8    DMIX=Z(ILAY)+DZ(ILAY)*0.5
      DO 9 I=1,10
        RMS(I)=0.0
 9    CONTINUE
      NDAYS=1
      MP=0
      IPRNT(1)=1
      KDAYS=0
      IPRNT(5)=IPRNT(5)-1
      MDAY=0

C      CALL PTABLE(0.,0.)
      IPRNT(5)=IPRNT(5)+1

C... Start simulation for each month
      EDAY=365.
      YEAR=REAL(MYEAR)
      IF(AMOD(YEAR,4.0).EQ.0.0) EDAY=366.
      ETSUM=0.0
      HTSUM=0.0
      
CFX... FOR YEAR ROUND SIMULATIONS
 666  CONTINUE     
      DO 100 J=1,NM
c	write(*,*)" begin simulation : j =  ", j
        CALL MTHDATA(MONTH,KDAYS,MYEAR,JDY)
   
C.... Output simulating time Month, Days in this month, Year
        MDAY=1
        IF(MONTH.EQ.MONBAK.AND.MYEAR.EQ.YEARBAK) THEN
           WRITE(76,7605)MONTH,DAYBAK,KDAYS-DAYBAK+1,MYEAR,MBOT
           WRITE(76,7600)(Z(I), I=1,MBOT)
           IF(IPRNT(4).EQ.1) THEN
		    WRITE(75,7605)MONTH,DAYBAK,KDAYS-DAYBAK+1,MYEAR,MBOT
              WRITE(75,7600)(Z(I), I=1,MBOT)
	     ENDIF
	  ELSE IF((MYEAR.GT.YEARBAK).
     +          OR.(MONTH.GT.MONBAK.AND.MYEAR.EQ.YEARBAK)) THEN
	     WRITE(76,7605)MONTH,MDAY,KDAYS,MYEAR,MBOT
           WRITE(76,7600)(Z(I), I=1,MBOT)
           IF(IPRNT(4).EQ.1) THEN
		    WRITE(75,7605)MONTH,MDAY,KDAYS,MYEAR,MBOT
	        WRITE(75,7600)(Z(I), I=1,MBOT)
           ENDIF
        ENDIF	    
 7605 FORMAT(1X,3I4,I6,I4)
        EDAY=365.
        YEAR=REAL(MYEAR)
        IF(AMOD(YEAR,4.0).EQ.0.) EDAY=366.

C...START SIMULATION FOR EACH DAY
        DO 200 MDAY=ISTART,KDAYS
c      write(*,*)" each day simulation , mday = ", MDAY
C... restore initial T & DO data
        IF(YEARBAK.EQ.MYEAR.AND.MONBAK.EQ.MONTH.AND.DAYBAK.EQ.MDAY) THEN
          IF(T_FLAG.EQ.0.OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0)) THEN
             IF(T_FLAG.EQ.1.OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.1)) THEN      
		      WRITE(99,550) MONTH,MDAY,MYEAR
 550  FORMAT(2X,'Data Restoring for this date : ',I4,'/',I4,'/',I6)
	       ENDIF

	       IF(T_FLAG.EQ.1) THEN
C.... test
      write(99,*)" Inputed T for this date, T = ",
     +        	(TBAK(it), it=1,mbot)
      write(99,*)" Before temperature is restored :"
	write(99,*)" T2 = ",(T2(it),it=1,mbot)
	          DO IT = 1,MBOT
	             T2(IT) = TBAK(IT)
	          ENDDO
C.... test 
      write(99,*)" After temperature is restored :"
	write(99,*)" T2 = ",(T2(it),it=1,mbot)
	       ENDIF
	       IF(iprnt(4).eq.1.and.DO_FLAG.EQ.1) THEN
C.... test
      write(99,*)" Inputed DO for this date, DOBAK = ",
     +            (DOBAK(it), it=1,mbot)
      write(99,*)" Before Dissolved Oxygen is restored :"
	write(99,*)" DSO2 = ",(DSO2(it),it=1,mbot)
	          DO IT= 1,MBOT
	             DSO2(IT) = DOBAK(IT)
	          ENDDO
      write(99,*)" After Dissolved Oxygen is restored :"
	write(99,*)" DSO2 = ",(DSO2(it),it=1,mbot)
	       ENDIF
	     ENDIF
	  ENDIF

          NFLOW=INFLOW
          CALL LAKE(0.,0.,0,5)

c      write(*,*)"after first lake call "
C...FOR ICE-SNOW MODEL - COMPUTE ICE OR SNOW THICKNESS, HEAT
C...FLUX FROM SEDIMENT - VERY IMPORTANT!!!
          CALL LAKE(0.,0.,0,14)
          PICE=THICE
          IREPEAT=-100

CFX***Specify total chlorophyll-a concentration in mg/L
          CALL PTOTALS(MAXMTH,MXDAY,CHLMAX,CHLMEAN)
c      write(*,*)"after second lake call "
CFX***  TO RESET THE INITIAL CONDITIONS AT MARCH 1 FOR TEST ONLY
          IF(EMCOE(3).EQ.-2.0) THEN
            IF(MONTH.EQ.3.AND.MDAY.EQ.1) THEN
              DO 202 KK=1,MBOT        
                T2(KK)=4.0
                DSO2(KK)=13.0
 202          CONTINUE       
              WRITE(99,*) T2(1),DSO2(1) 
            ENDIF
          ENDIF             

          IF(MDAY.EQ.KDAYS.OR.MP/NPRINT*NPRINT.EQ.MP) IPRNT(1)=1
          IF(MONTH*100+MDAY.EQ.NPRNT(NDAYS)) IPRNT(1)=1
c      write(*,*)"before first preice call "
CFX - Inflow routine!
          IF(NFLOW.GT.0) THEN
            DO 203 L=1,NFLOW
 205          READ(4,*) MTH,MD,QIN(L),TIN(L),PAIN(L),BODIN(L),
     +         DOIN(L),CIN(L),CDIN(L),XNOIN(L),XNHIN(L),CHLAIN(1,L)
C... Search inflow file for first day of simulation
              IF(MTH*100+MD.NE.MONTH*100+MDAY) GOTO 205
C... Convert inflow from cfs to m**3 per day
              QIN(L)=QIN(L)*2446.6
 203        CONTINUE
          ENDIF

          P=PR(MDAY)*0.0254
          MP=MP+1
          TMIX=T2(1)
C...CALCULATION OF KINETIC ENERGY FROM WIND STRESS
          CALL WINEN(TAU,VC,WIND(MDAY))
          RKE=TAU*VC*ATOP(1)*WSTR*86400.0

C--- HEAT IS ABSORBED FIRST, THEN WATER COLUMN IS MIXED BY THE WIND
CFX  ITERF FOR HEATING AND WIND-MIXING PROCESS, ITERF<10.
          DO 864 IK=1,MBOT
            T2K(IK)=T2(IK)
 864      CONTINUE
          ITERF=-100
 865      CALL HEBUG(ILAY,TMIX,QNET,HS,HA,HBR,HE,HC,TAIR(MDAY),
     +     TDEW(MDAY),CR(MDAY),RAD(MDAY),WIND(MDAY),VC)
C...TESTING 
       IF(THICE.LE.0.0) THEN

        DO 910 I=1,MBOT
         TEDIF(I)=T2(I)
 910    CONTINUE             

       CALL PREICE(QNEG,TBOUN,NP)
c      write(*,*)"after first preice call "

       CALL CONMIX(ILAY,TMIX,MBOT)
c      write(*,*)"after first conmix call "

	   DO 920 I=1,MBOT
           TECON(I)=T2(I)
 920     CONTINUE

C        IF(TECON(1).LT.4.0) THEN
C          WRITE(99,930) MYEAR,MONTH,MDAY,ILAY
C930       FORMAT(1X/1X,I4,3X,'MONTH ',I3,3X,'DAY ',I3,
C    +     3X,'Number of mixed layer',I3,3X,F7.3/5X)

C          WRITE(99,38) QNEG
C38        FORMAT(1X/1X,'QNEG = ',F12.2/5X)

C        ENDIF
         
        ENDIF

C...CALCULATION OF EVAPORATION IN TERMS OF VOLUME
C...CALCULATES LATENT HEAT OF VAPORIZATION ALV
          HED=HE/((597.31-0.5631*T2(1))*RHO(T2(1),C2(1),CD2(1)))
          HEV=HED*ATOP(1)
C...  CALL ADVECT(P,HED,NFLOW,S,FT,WCHANL,ST,MYEAR)

        IF(THICE.LE.0.0) THEN
         CALL WINMIX(RKE,TMIX,TSAVE,ILAY,MBOT,VC)
         DMIX=Z(ILAY)+0.5*DZ(ILAY)
         
         DO 477 II=1,MBOT
          TEMIX(II)=T2(II)
 477     CONTINUE
 
         CALL POSTMIX(TBOUN,THICE,QNEG,TECON,TSAVE,RKE,NP)

C        IF(TECON(1).LT.4.0) THEN
C          WRITE(99,359)
C359       FORMAT(/3X,'Depth',5X,'TEDIF',5X,'TECON',5X,'TEMIX'
C    +       ,5X,'T2(I)',5X,'LAYER')
        
C          DO 320 I=1,MBOT
C            WRITE(99,360) Z(I),TEDIF(I),TECON(I),TEMIX(I),T2(I),I
C360         FORMAT(1X,5(F7.3,3X),5X,I2)
C320       CONTINUE
C        ENDIF

        ENDIF
        
        IF(THICE.GT.0.0.AND.PICE.LE.0.0) THEN
         
         IF(IREPEAT.EQ.100) GOTO 946
         IF(MONTH.LT.6) GOTO 946
         
C         ITEST=1
C         WRITE(99,935) MYEAR,MONTH,MDAY,THICE
C935      FORMAT(1X/1X,I4,3X,'MONTH ',I3,3X,'DAY ',I3,
C    +     5X,'ICE FORMATION - Zice',F7.3/5X)
C        ENDIF 

C        IF(ITEST.LT.10.AND.MYEAR.EQ.1986) THEN
C          WRITE(99,7777) MONTH,MDAY,MYEAR
C          DO 366 II=1,MBOT
C            WRITE(99,3099) Z(II),T2(II)
C366       CONTINUE
C          ITEST=ITEST+1
C        ENDIF

          DO 945 I=1,MBOT
             T2(I)=T2K(I)
 945      CONTINUE
          IREPEAT=100
          IF (IREPEAT.EQ.100) GOTO 865
 946      CONTINUE
          
          ENDIF                                       

          IF(JDY.LE.EDAY) THEN
           DY2=JDY
           TDAY=EDAY
          ENDIF 

          IF(JDY.GT.TDAY.AND.JDY.LE.(TDAY+EDAY)) DY2=JDY-TDAY
          IF(JDY.EQ.(TDAY+EDAY)) TDAY=TDAY+EDAY
          IDY=DY2 

C       IF(NICE.EQ.0.0) THEN
C        TEICE(1)=THICE
C        NICE=100
C       ENDIF
        
C       IF(NICE.EQ.100) THEN
C        TEICE(2)=THICE
C       ENDIF  

C       IF(TEICE(1).EQ.0.0.AND.TEICE(2).NE.0.0) THEN
C        WRITE(18,260) MYEAR,IDY,THICE,THSNOW,SNOWFL(MDAY)
C       ENDIF

C       IF(TEICE(1).NE.0.0.AND.TEICE(2).EQ.0.0) THEN
C        WRITE(18,260) MYEAR,IDY,THICE,THSNOW,SNOWFL(MDAY)
C       ENDIF
         
C       TEICE(1)=TEICE(2) 
       
        IF(JDY.GE.288.OR.JDY.LE.105) THEN
         WRITE(18,260) JDY,THICE,THSNOW,SNOWFL(MDAY)
 260     FORMAT(1X,I4,3X,F8.3,3X,F8.3,3X,F8.3)
        ENDIF

      
C260    FORMAT(4X,I4,I6,3X,F8.3,3X,F8.3,3X,F8.3)

        IF(ITERF.GT.0) GOTO 865
      
C...For temperature simulation only, skip dissolved and 
C...suspended substances routines
          IF(IPRNT(4).LT.1) GOTO 35

C************************************************************
C
C  THE SIMPLEST MODEL FOR DISSOLVED OXYGEN (X. FANG)
C
C*************************************************************
          IF(MODEL.EQ.4) THEN
C...Groundwater flows can be added (none presently)
            CALL LAKE(0.0,0.0,0,11)
            ICV=6
            CALL SETAMK(WIND(MDAY),VC,ILAY,MBOT,ICV)
            DO 887 I=1,MBOT
              T20(I)=T2(I)-20.0
 887        CONTINUE       
            TD=12.16+2.36*COS(0.0172*(172.0-DY2))
            CALL COEF(MODEL,MBOT,NCLASS)
            CALL DISOLID(DSO2,WIND(MDAY),RAD(MDAY),TD,ST)
          ENDIF

 35       CALL LAKE(0.,0.,0,13)

C... FOR INTERPOLATION OF WATER TEMPERATURE AND D.O.
CFX..FOR SIMUATIONS
          DO 326 IM=1,NDEPTH
            ZDIN=FDTH(IM) 
            TETH(IM)=-80.0
            IF(IPRNT(4).EQ.1) DOTH(IM)=-80.0
            
            DO 328 JJ=1,MBOT-1
              IF(ZDIN.LE.Z(JJ+1).AND.ZDIN.GE.Z(JJ)) THEN
C-WATER TEMPERATURE
                FAD=T2(JJ+1)-T2(JJ)
                FDZ=Z(JJ+1)-Z(JJ)
                TETH(IM)=T2(JJ)+(ZDIN-Z(JJ))*FAD/FDZ

               IF(IPRNT(4).EQ.1) THEN
C-DISSOVED OXYGEN
                DAD=DSO2(JJ+1)-DSO2(JJ)
                DOTH(IM)=DSO2(JJ)+(ZDIN-Z(JJ))*DAD/FDZ
               ENDIF
                
              ENDIF
 328        CONTINUE
 326      CONTINUE


         IF(IPRNT(4).EQ.1) THEN
C*****Compute the saturated oxygen from Thomann    ************
C*****Correct saturation concentration with elevation  ********
C*****ST is the stage of the lake in meters above sea level ***
          T2X=TETH(1)+273.15
          DOXS=-139.3441+1.575701E5/T2X-6.642308E7/T2X**2.0
     +     +1.2438E10/T2X**3.0-8.621949E11/T2X**4.0
          DOXS=EXP(DOXS)*(1-0.000035*ST*3.2808)
         ENDIF
         
         IF(IPRNT(4).EQ.1) THEN
          IF(THICE.LE.0.0) THEN
            WRITE(14,330) JDY,(TETH(K),K=1,NDEPTH),(DOTH(K),K=1,NDEPTH),
     +       DMIX/ZMAX,DOXS
          ELSE
            WRITE(14,330) JDY,(TETH(K),K=1,NDEPTH),(DOTH(K),K=1,NDEPTH),
     +       1.0,DOXS
          ENDIF
 330  FORMAT(I5,12(2X,F6.2))
         ELSE
         
C TEMPERATURE SIMULATION ONLY ! 
          IF(THICE.LE.0.0) THEN
            WRITE(14,330) JDY,(TETH(K),K=1,NDEPTH),DMIX/ZMAX
          ELSE
            WRITE(14,330) JDY,(TETH(K),K=1,NDEPTH),1.0
          ENDIF
         ENDIF

C....STORE DATA FOR ERROR ANALYSIS ON THE DAY WHICH
C....WE HAVE FIELD DATA
          IF(MYEAR.EQ.KYEAR(NYEAR)) THEN 
           IF((MDAY+MONTH*100).EQ.NPRNT(NDAYS)) THEN
            CALL ERROR(1,1,TETH,DOTH)
           ENDIF
          ENDIF 
           

C...Output to plot file (tape8.PLT)
          IF(IPRNT(5).GT.0) CALL FPLOT(MYEAR,DMIX,SECCHI,CHLMEAN)

          IF(IFIELD.EQ.1.AND.MYEAR.EQ.KYEAR(NYEAR)) THEN 
            IF((MDAY+MONTH*100).EQ.NPRNT(NDAYS)) THEN

C...Access and output field data
              CALL FDATA(NF,NPRFLE)

CFX. FOR INTERPOLATION OF WATER TEMPERATURE AND DO 
C... FOR FIELD DATA comparison !!!
              DO 426 IM=1,NDEPTH
                ZDIN=FDTH(IM) 
                TETH(IM)=-80.0
                IF(IPRNT(4).EQ.1) DOTH(IM)=-80.0
                
                DO 428 JJ=1,NF-1
                  IF(ZDIN.LE.DEPTH(JJ+1).AND.ZDIN.GE.DEPTH(JJ)) THEN
C-WATER TEMPERATURE
                    FAD=FLDATA(1,JJ+1)-FLDATA(1,JJ)
                    FDZ=DEPTH(JJ+1)-DEPTH(JJ)
                    TETH(IM)=FLDATA(1,JJ)+(ZDIN-DEPTH(JJ))*FAD/FDZ

                   IF(IPRNT(4).EQ.1) THEN
C-DISSOVED OXYGEN
                    DAD=FLDATA(6,JJ+1)-FLDATA(6,JJ)
                    DOTH(IM)=FLDATA(6,JJ)+(ZDIN-DEPTH(JJ))*DAD/FDZ
                   ENDIF

                  ENDIF
 428            CONTINUE
 426          CONTINUE

            IF(IPRNT(4).EQ.1) THEN
             WRITE(48,436) JDY,(TETH(K),K=1,NDEPTH),
     +        (DOTH(K),K=1,NDEPTH) 
 436  FORMAT(I5,2X,10(F6.2,2X))
            ELSE
             WRITE(48,436) JDY,(TETH(K),K=1,NDEPTH) 
            ENDIF

C.... STORGE DATA FOR ERROR ANALYSIS
             CALL  ERROR(1,2,TETH,DOTH)
  
C**** Output simulation results and field data for Plot-IT
              WRITE(26,7777) MONTH,MDAY,MYEAR
 7777 FORMAT(5X/5X/I3,3X,I4,3X,I6/5X/)
 3099 FORMAT(5X,3(F7.3,3X))
 3096 FORMAT(5X,6(F7.3,3X))
              DO 266 I=1,MBOT
                IF(I.GT.1.AND.DEPTH(I).LT.0.0001) THEN 
                  WRITE(26,3099) -Z(I),T2(I),DSO2(I)
                ELSE
                  WRITE(26,3096) -Z(I),T2(I),DSO2(I),-DEPTH(I),
     +             FLDATA(1,I),FLDATA(6,I)
                ENDIF
  266         CONTINUE
            ENDIF
          ENDIF
      
          JDY=JDY+1
          IPRNT(1)=0

C.... Output simulated Temperature & Dissolved Oxygen into files.
        IF(MONTH.EQ.MONBAK.AND.MYEAR.EQ.YEARBAK) THEN
	     IF(MDAY.GE.DAYBAK) THEN
              WRITE(76,7600)(T2(I),I=1,MBOT)
              IF(IPRNT(4).EQ.1) THEN
                 WRITE(75,7600)(DSO2(I),I=1,MBOT)
	        ENDIF
	     ENDIF
	  ELSE IF((MYEAR.GT.YEARBAK).
     +          OR.(MONTH.GT.MONBAK.AND.MYEAR.EQ.YEARBAK)) THEN
           WRITE(76,7600)(T2(I),I=1,MBOT)
           IF(IPRNT(4).EQ.1) THEN
              WRITE(75,7600)(DSO2(I),I=1,MBOT)
	     ENDIF
        ENDIF	    
 7600 FORMAT(1X, 10F8.3) 
	     
  200   CONTINUE
        ISTART=1
      
  100 CONTINUE

C.... FOR YEAR ROUND SIMULATION, NYTOT = TOTAL YEARS
 
      NUMY=NUMY+1 
      IF(NUMY.LE.NYTOT) THEN
        CLOSE (9)
        CLOSE (75)
        CLOSE (76)
        MYEAR=MYEAR+1
        NA=INT((MYEAR-100*INT(MYEAR/100.0))/10.0)
        NB=MYEAR-10*INT(MYEAR/10.0)
        T8(INUM)=NUM(NA+1)
        T8(INUM+1)=NUM(NB+1)
        CALL MAKEFILE(PATHM,T8,FFILE)
        OPEN (9,FILE=FILENAME,STATUS='OLD')
        T7(INUM)=NUM(NA+1)
        T7(INUM+1)=NUM(NB+1)
        T9(INUM)=NUM(NA+1)
        T9(INUM+1)=NUM(NB+1)
        CALL MAKEFILE(PATH,T7,FFILE)
        OPEN (75,FILE=FILENAME)
        CALL MAKEFILE(PATH,T9,FFILE)
	  OPEN (76,FILE=FILENAME)
        IF(MYEAR.EQ.KYEAR(NYEAR+1)) NYEAR=NYEAR+1
        NM=12
        IF(NUMY.EQ.NYTOT) NM=NMFIN
        GOTO 666  
      ENDIF

C...Compute and list statistics:
C...     1) Absolute maximum deviations between model 
C...        and field data and day of occurrence
C...     2) slope of regression of field data on simulation results
C...     3) regression coefficient  R**2
C...     4) standard error of the regression

       IF(IFIELD.EQ.1) THEN
	  CALL ERROR(2,2,TETH,DOTH)

       WRITE(98,3030) MAXMTH,MXDAY,MYEAR,CHLMAX
 3030 FORMAT(1X/1X,'MAXIMUM CHLOROPHYLL-A  :',10X,I2,'-',I2,'-',I4,
     + 10X,F5.3,'  (mg/l)')
	 
	 ENDIF 
CFX
      IF(IPRNT(5).GT.0) THEN
        WRITE(*,5000)
 5000 FORMAT(//1X,'PRODUCE *.SDF FILES FOR TIME SERIES (Y/N) ?  ',\)
        READ(*,'(A)') XS
       
	  IF(XS.EQ.'Y' .OR. XS.EQ.'y') THEN
          IDNUM(3)=100
C ***  OPEN FILE "DOST.SDF"
          CALL MAKEFILE(PATH,FILE25,FFILE)
          OPEN (25,FILE=FILENAME)
C ***  OPEN FILE "DOFT.SDF"
          CALL MAKEFILE(PATH,FILE36,FFILE)
          OPEN (36,FILE=FILENAME)
        ENDIF
        
      ENDIF
	WRITE(198,*)
      WRITE(198,*)'End of MNLAKE simulation :  Normal exit'
      WRITE(99,*)' End of MNLAKE simulation :  Normal exit'
      
C      STOP ' End of MNLAKE simulation :  Normal exit'
      STOP
      END

C************************************************************
C      THE PROGRAM IS FOR RYAN LAKE IN WINTER STUDY 
C      THE ICE-SNOW MODEL IS INCOPORATED WITH MINLAKE      
C      (THERE IS HEAT INPUT FROM THE GROUNDWATER)
C      REVERSED TEMP GRADIENT (INSTABILITY) TREATMENT
C
C      THE PROGRAM WAS BASED ON GU'S STUDY WITH IMPROVEMENT
C      AND MODIFICATIONS (SEE FANG, 1994)
C*************************************************************
      SUBROUTINE LAKE(ZD,DUM,NFLOW,ID)
C**** Lake specific subroutine to configure the model to a specific lake 

      REAL*8 A,V,TV,ATOP,DUM
      INTEGER FMON,FDAY,FYEAR
      COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),
     + PR(31),DRCT(31)
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
       COMMON/SOURCE/RM(3,40),PROD(40),XMR(3,40),PRODSUM(40)
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,
     + YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
      COMMON/PHYTO/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),
     + XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),
     + GROMAX(3),TMAX(3),TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),
     + HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN,CHLMAX,SECCHI
      COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,
     + XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),
     + REPRO,XI,XKMZ,GRAZE(3,40)
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/SUB/SDZ(90),SZ(90),LAY(40),AVGI(24,90),SVOL(90)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),
     + RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
      COMMON/INFLOW/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),
     + CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
      COMMON/YROUND/ NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,
     + HKMXSM,WCFSM,WSSM,WCFSF,WSSF
      COMMON/FIELD/ IFLAG(10),FLDATA(10,50),DEPTH(50),NFLD(10),SD
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
      COMMON/TITL/ TITLE
      COMMON IPARAM,IGRAF
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     + CDIS0,CNDSNW0,DEPTHC,ICEMON,
     + ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
      COMMON/CONR/ FAKW,SNCOE,COEWIN

      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
      DIMENSION AZ(30),AA(30),AV(30)
C
C     WE WILL NOT USE THE AREA -- DEPTH AND VOLUME -- DEPTH
C     RELATIONSHIP, INSTEAD OF USING INTERPOLATION SCHEME FROM
C     THE ORIGINAL DATA (ZA -- DEPTH,  AA -- AREA, AV -- VOLUME)
C
C     IN THE MAIN PROGRAM WE SHOULD CALL LAKE ID=12 FIRST !!!
C
      GOTO (100,200,300,400,500,600,700,800,
     +                900,1000,1100,1200,1300,1400) ID

C********** AREA COMPUTATION SECTION ***********
 100  CONTINUE
      DUM=FXIN(ZD,NK,AZ,AA) 
      RETURN
C********** FETCH COMPUTATION SECTION ******
 200  ZD=SQRT(4.0/3.14159*AA(NK))
      RETURN
C********** VOLUME COMPUTATION SECTION *****
 300  CONTINUE
      DUM=FXIN(ZD,NK,AZ,AV)
      RETURN 
C***** COMPUTE DEPTH FROM VOLUME *****
 400  CONTINUE 
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
 150  CONTINUE
      IF(DUM.GT.AV(NK).AND.I.GE.NK) THEN
        RAT=(AV(NK)-AV(NK-1))/(DUM-AV(NK-1))
          ZD=AZ(NK-1)+(AZ(NK)-AZ(NK-1))/RAT
          AV(NK)=DUM
          AZ(NK)=ZD
      ENDIF 
      AA(NK)=FXIN(AZ(NK),NK,AZ,AA)
      RETURN
C***** TREATMENT SECTION *****
 500  CONTINUE
      IF(THICE.LE.0.0) THEN 
C        WRITE(*,550) MONTH,MDAY,MYEAR
C.... test	
	write(99,550) MONTH,MDAY,MYEAR

 550  FORMAT(2X,'Month',I4,3X,'Day',I4,3X,I6)
      ELSE
C        WRITE(*,555) MONTH,MDAY,MYEAR,THICE,THSNOW
C.... test
        WRITE(99,555) MONTH,MDAY,MYEAR,THICE,THSNOW
 555  FORMAT(2X,'Month',I4,3X,'Day',I4,3X,I6,3X,F5.2,3X,F5.2)
      ENDIF
      RETURN

C***** PHOSPHORUS SOURCES/SINKS *****
 600  RETURN

C***** NO2-NO3 SOURCES SINKS
 700  RETURN

C***** NH4 SOURCES/SINKS *****
 800  RETURN

C***** O2 SOURCES/SINKS *****
 900  RETURN

C***** OUTFLOW COMPUTATION *****
 1000 CONTINUE
      NFLOW=0
      RETURN

C***** GROUNDWATER FLOW COMPUTATION *****
 1100 CONTINUE
      RETURN

C***** INPUT REQUEST OR MODIFICATION ***********************
C....1200 SUBROUTINE IS CALLED JUST ONCE,IN THE BEGINNING.
C....IT IS CONVENIENT TO WRITE HERE ALL CONSTANT VALUES 
 1200 CONTINUE

C***** READ THE ORIGINAL DATA FOR DEPTH, AREA, VOLUME
      READ(38,*) NK
      DO 66 I=1,NK
        READ(38,*) AZ(I),AA(I),AV(I)
 66   CONTINUE  

CFX...OUTPUT SOME PARAMETERS FOR WINTER STUDY !     
C     WRITE(18,905)
C     WRITE(18,1613)
C     WRITE(18,1615) CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC,ICEMON,
C    +               ICEDAY,MELMON,MELDAY
C     WRITE(18,1616) HKMXSM,WCFSM,WSSM,HKMXIS,WCFIS,WSIS
C     WRITE(18,1614) BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW
C     WRITE(18,1618)
C     WRITE(18,250) 
C     WRITE(18,255) 
 
C      WRITE(14,1621) ZSLT,IZSLT,AHTBTM,TSLMEAN,ETAB
C      WRITE(14,1629)
C      WRITE(14,1631) 
      
C      WRITE(14,1619) (ZSL(I),I=1,IZSLT)
C      WRITE(14,1620) JDY,(TSL(I),I=1,IZSLT)
   

 250  FORMAT('JDAY',3X,'TAIR',4X,'QAIR',4X,'QWAT',3X,'HSED',
     + 3X,'RAD',5X,'HSA',3X,'HCNDISO')
 255  FORMAT(2X,'JDAY',5X,'THICE',5X,'THSNOW',5X,'SNOWFL')
 905  FORMAT('INPUT DATA:') 
 1613 FORMAT(2X,'CFSNOW CDIS0 CNDSNW0 CNDWI DEPTHC ICEMON ICEDAY',1X,
     + 'MELMON MELDAY')
 1614 FORMAT(2X/2X,'B, A, G(ICE,SNOW)= ',6F7.2//) 
 1615 FORMAT(2X,F5.2,1X,F5.2,2X,F5.2,3X,F5.2,1X,F4.2,1X,I6,1X,I6,1X,
     + I7,1X,I6/)
 1616 FORMAT(2X,'HKMAX, WCOEF, WSTR = ',2X,3F9.2)
 1618 FORMAT(2X,'RESULTS OF SIMULATION:')
 1619 FORMAT(/'Z(I)',11F6.2) 
 1620 FORMAT(I4,11F6.2) 
 1629 FORMAT(2X,'TEMPERATURES IN THE SEDIMENT BELOW LAKE BOTTOM:'/)
 1621 FORMAT(2X,'ZSLT   IZSLT   AHTBTM   TSLMEAN   ETAB '/2X,
     + F4.1,2X,I4,3X,F6.2,5X,F6.2,2X,F6.2/)
 1631 FORMAT(2X,'DEPTH BELOW BOTTOM')
      RETURN

C**** POST DAILY SIMULATION TREATMENT AND COMPUTATIONS  *******
 1300 CONTINUE
      RETURN

C**** ICE-SNOW MODEL BY GU WITH MODIFICATION FOR WINTER SIMULATIONS
 1400 CONTINUE

C.....SET WIND SHELTERING AND MAXIMUM DIFFUSION COEFFICIENTS
C.....ICE COVER - WINTER PERIOD
  
      IF(THICE.GT.0.0) THEN
        HKMAX=HKMXIS
        WCOEF=WCFIS
        WSTR=WSIS
      ENDIF

C.....OPEN WATER SEASON - APRIL TO NOVEMBER
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

C  I think the ice formation condition must be prior to call
C  subroutine ICE and SNOW.  That is certainly true!
C  Determine volume averaged lake water temperature!  
C  MELMON, MELDAY= The first month or day of ice melting.
C  ICEMON, ICEDAY= The first month or day of freezing.

      IF(MONTH.EQ.1.AND.MDAY.EQ.1) MELMON=4

      IF(MONTH.EQ.MELMON.AND.MDAY.EQ.MELDAY) THEN
        IF(THICE.GT.0.0) MELMON=MELMON+1
      ENDIF

      IF(THICE.GT.0.0) ICESTAR=1
      
      IF(ICESTAR.EQ.1) GOTO 50
      IF(THICE.GT.0.0) THEN
       MELMON=MONTH
       MELDAY=MDAY
       ICESTAR=1
      ENDIF

C...During open water season
 50   IF(MONTH.EQ.MELMON.AND.MDAY.GT.MELDAY) THEN
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

C Call subroutine SNOW to determine snow thickness and heat flux

      IF(NSWSTAR.EQ.1) GOTO 55
      IF(SNOWFL(MDAY).GT.0.0.OR.THSNOW.GT.0.0) THEN
        MNSNOW=MONTH
        NDYSNOW=MDAY
        NSWSTAR=1
      ENDIF 
   
 55   IF(NSWSTAR.EQ.1) THEN
        CALL SNOW(SNOWMELT)
      ENDIF 

C Call subroutine ICE to determine ice thickness and heat flux
     
      IF(ICESTAR.EQ.1) THEN
        CALL ICE(SNOWMELT)
      ENDIF
      
      RETURN
      END

C**********************************************************C
C
C
C**********************************************************C
      SUBROUTINE SNOW(SNOWMELT)
Clj** To determine snow thickness and heat flux

      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),
     + PR(31),DRCT(31)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     + CDIS0,CNDSNW0,DEPTHC,ICEMON,
     + ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
      COMMON/CONR/ FAKW,SNCOE,COEWIN

      XCOMP=0.0
      XRAD=0.0
      XRAIN=0.0
      XCOV=0.0
      XEVA=0.0
C  AVOID THE CASE WHEN SNOWFALL HAPPENS WITHOUT ICE COVER IN A LAKE
      IF(THICE.LE.0.0) THEN
       THICE=0.0
       THSNOW=0.0
       SNOWMELT=0.0
       RETURN
      ENDIF 
      
C  SNOW ACCUMULATION FROM THE PRECIPITATION
C  SNOWFL is given as meters!
      THSNOW=THSNOW+SNOWFL(MDAY)*CFSNOW
      XCOMP=SNOWFL(MDAY)*CFSNOW

C  SNOW DEPTH IS ZERO, IGNORE SNOW MELTING PROCESSES
      IF(THSNOW.LE.0.0) THEN
        THSNOW=0.0 
        SNOWMELT=0.0
        RETURN
      ENDIF
      
C  Time step = one day
      DLTIME=1.0

C  Latent heat of fusion of snow in kcal/kg 
      HTLTNT=80.0

C  Density of snow in kg/m**3
      DENSNOW=300.0
      DENICE=920.0
      DENWAT=1000.0
      DRATIO=DENWAT/DENSNOW

C  The ratio of snow depth and ice thickness shoud be less
C  than (DENwater-DENice)/DENsnow (GU and Stefan, 1990)
C  RATSNIC=(DENWAT-DENICE)/DENSNOW, but is too small according
C  to field data. So RATSNIC = SNCOE is an input parameter.

      RATSNIC=(DENWAT-DENICE)/DENSNOW
C     RATSNICN=COEWIN
           
C  Total short wave solar radiation within snow      
C  RAD - solar radiation in cal/cm*cm/day - kcal/m*m/day
      HTSNOW=RAD(MDAY)*10.0*(BTSNOW*(1.0-ALFSNOW)+(1.0-BTSNOW)*
     +   (1.0-ALFSNOW)*(1.0-EXP(-GMSNOW*THSNOW)))  

C  Determine evaporative heat loss Qe = f(w)*(Ea-Es) 
C  Ea and Es is vapor pressure of air and at snow surface
C  Formula for both Ea and Es is the same as in MINLAKE 
C  6.11 mb is vapor pressure at Ts=0 C or 32 F.
C  ice or snow surface temperature is equal to 32 F during
C  the active thawing period (Shen and Chiang, 1984).
C  Wind function coefficients for Qe and Qr are different
C  which are from Light, 1941.  DQE is in inches/day !
      TDEWMLT=TDEW(MDAY)
      CPW2=7.45*TDEWMLT/(235.0+TDEWMLT)
      EA=6.035*10.0**CPW2
      DQE=COEWIN*WIND(MDAY)*0.0231*(EA-6.11)

C  Determine sensible (conductive or convective) heat flux
C  into (atmosphere) air at snow surface,  Qa=ha*(Ts-Ta).
C  Ta is temperature at the top of snow layer = 32 F.
C  Ts is air temperature - TAIR(MDAY) in F - TSNW
C  Ha=1.8*Den*Latent*Wind*10**(-0.0000512*Zm)*E-4 (from Light, 1941)!
C  Wind speed WIND(MDAY) is in mph for MINLAKE, It should
C  be m/s for calculating Ha, 1mph = 0.447m/s;
C  Den*Latent=80kcal/kg*300kg/m**3=24000 kcal/m**3;
C  Zm is lake elevation above sea level in m.

C  Gu's formula (23b) in his paper was wrong! His program is RIGHT!

CGU used equation (5)*4 (Light, 1941) - inches/6hrs = 4inches/day.  
C   SNOWMELT=WIND(MDAY)*(0.00736*(TSNW-32.0)*10**CPOWER
C     +                    +0.0231*(EA-6.11))           
 
      CPOWER=-0.0000156*800.0
      TSNW=32.0+9.0*TAIR(MDAY)/5.0
      DQA=COEWIN*WIND(MDAY)*0.00736*10**CPOWER*(TSNW-32.0)
 
C  Check equation (22b) in Gu&Stefan, We have Den*Latent/DLTIME
C  in left side, DLTIME=1 for no effect for numerical values!  
C  0.9 is due to different elevation of wind & temperature measurement!
C  (This equation is actually from Light, 1941.)
      SNOWMELT=0.9*(DQE+DQA)

C  Change snowmelt depth from inches to meters
      SNOWMELT=SNOWMELT*0.0254
      IF(SNOWMELT.LT.0.0) SNOWMELT=0.0

C  Snowmelt from solar radiation independent of air temperature!
C  Set -1.0 to give the best results for Ryan Lake - Don't Change
      IF(TAIR(MDAY).GT.0.0) THEN
        SNOWMELT=SNOWMELT+HTSNOW*DLTIME/(DENSNOW*HTLTNT)
      ENDIF

C  Consider rainfall effect PR(MDAY) in inches as Tair > 0.0
C  DRATIO=DENw/DENs = 1000.0/300.0, GU USED DRATIO=3.0 !
      IF(TAIR(MDAY).GT.0.0) THEN
        SNOWMELT=SNOWMELT+PR(MDAY)*TAIR(MDAY)*DRATIO/HTLTNT*0.0254
      ENDIF

C Determine finally snow thinckness including snowmelt!      
      IF(SNOWMELT.LE.0.0) SNOWMELT=0.0
      IF(SNOWMELT.GT.THSNOW) SNOWMELT=THSNOW
       THSNOW=THSNOW-SNOWMELT
      
C XCR=0.02 M Capillary rise above free-water-level
C SCR=800.0 average specific gravity of snow wetted
C capillary rise (range of 600 to 900 kg/m**3)
C SOF=800.0 average specific gravity of wetted snow
C from top of ice to free-water-level.
C Lake Ice Formation by Nelson, 1995
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
C It occurs when THICE is small (e.g. 5 cm) and THSNOW is very 
C close to SRAT*THICE.        
         THICE=0.0
         THSNOW=0.0      
        ELSE
         THICE=THICE+XOF
         THSNOW=THSNOW-XOF
        ENDIF

C        WRITE(15,186) MONTH,MDAY,RATO,OTHS,OTHI,THSNOW,THICE,XOF
C 186    FORMAT(2X,I3,2X,I3,2X,F3.1,2X,5(F7.4,2X))
     
C       DSNOW=(THSNOW-RATICSN*THICE)/(1+RATICSN)
C       THSNOW=THSNOW-DSNOW
C       THICE=THICE+DSNOW
      ENDIF
      ENDIF
            
      RETURN 
      END

C**********************************************************C
C
C
C**********************************************************C
      SUBROUTINE ICE(SNOWMELT)
Clj** To determine ice thickness and heat flux

      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),
     + PR(31),DRCT(31)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     + CDIS0,CNDSNW0,DEPTHC,ICEMON,
     + ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/INFLOW/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),
     + CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
      COMMON/CONR/ FAKW,SNCOE,COEWIN
      DIMENSION YI(5),XI(5),D1Y(5),D2Y(5),D3Y(5)

      XGROW=0.0
      XRAD=0.0
      XRAIN=0.0
      XREFRO=0.0
      
      IF(THICE.EQ.0.0) ACSNOW=0.0
      
C1 DLTIME - time step dt in day
      DLTIME=1.0

C2 Thermal conductivity of ice in kcal/day/m/C, CDISO in W/m/C      
      CNDICE=0.2388*3600.0*24.0/1000.0*CDIS0

C3 Density of ice in kg/m**3      
      DENICE=920.0
      DENWAT=1000.0
      DRATIO=DENWAT/DENICE
      
C3 Latent heat of fusion of ice or Snow in kcal/kg
C3 1KJ = 0.2388 Kcal = 1 KW-S
      HTLTNT=80.0

C4 Density of snow in kg/m**3
      DENSNOW=300.0

C5 Thermal conductivity of snow in kcal/day/m/C - CNDSNOW
C5 CNDSNW0 is in W/m/C for thermal condutivity (J=W-s).
      CNDSNOW=CNDSNW0*0.2388*3600.0*24.0/1000.0

C6 Temperature Tm at ice-water interface in C   
      TMELT=0.0
      
C** Solar radiation absorbed within ice and reaching bottom of ice
      HTSNW=RAD(MDAY)*10.0*(1.0-BTSNOW)*(1.0-ALFSNOW)*
     +       EXP(-GMSNOW*THSNOW)

      IF(THSNOW.EQ.0.0) HTSNW=RAD(MDAY)*10.0
      
      HTICE=HTSNW*(BTICE*(1.0-ALFICE)+(1.0-BTICE)*(1.0-ALFICE)*
     +     (1.0-EXP(-GMICE*THICE)))

C  Hsa=0.29-0.37U - bulk heat transfer coefficient as a function
C  of wind speed (Fertuck et al, 1971) in BTU/(ft*ft)/hr/F.
C  Gu used an average coefficient Hsa=0.33 U (U in mph)
C  1BTU/(ft*ft)/hr/F=1055*0.2388/0.0929*24*1.8/1000
C  =117 Kcal/(m*m)/day/C. (hr - hour, F = 1.8 C)
C  Hsa includes any effects of heat exchange at ice surface 
      HSA=0.33*117.0*COEWIN*WIND(MDAY)

C  For equation 16 (Gu)to calculate Qi in Kcal/(M*M)/day
      AC=THICE/CNDICE
      BC=THSNOW/CNDSNOW
      CC=1.0/HSA
      EC=TMELT-TAIR(MDAY)
      HCNDISO=EC/(AC+BC+CC)
      DC=DLTIME/(DENICE*HTLTNT)

C  For determining Qwi - QWATER at ice-water interface      
C  CNDWI - Thermal conductivity of Water.
C  Question - TCHAR is water temperature at DEPTHC !
C  Depthc = 0.1 m (Fang) or 0.75 (Gu)

C     DO 20 I=1,MBOT-1
C       IF(Z(1).GE.DEPTHC) THEN
C         TCHAR=T2(1)*(Z(1)-DEPTHC)/Z(1)
C       ENDIF      
C       IF(Z(I+1).GE.DEPTHC.AND.Z(I).LT.DEPTHC) THEN
C         TCHAR=T2(I)+(T2(I+1)-T2(I))*(DEPTHC-Z(I))/(Z(I+1)-Z(I))
C       ENDIF
C  20 CONTINUE
C     QWATER=FAKW*CNDWI*(TCHAR-TMELT)/DEPTHC

C Determine the temperature gradient at z = 0
C using three or four simulated ponits.
C YI - WATER TEMPERATURE AND XI IIS DEPTH Z      
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
   
C NOTICE THAT XI(1) = 0.0, THEREFORE:
          
      DTDZ=D1Y(1)-D2Y(1)*XI(2)-D3Y(1)*XI(2)*XI(3)
              
      QWATER=FAKW*CNDWI*DTDZ

      IF(QWATER.LE.0.0) QWATER=0.0
      IF(THICE.LE.0.0) QWATER=0.0 

C  THICE in right hand side is ice thickness at previous day
C  THICE is left hand side is ice thickness at the end of today!
       QAIR=EC/(AC+BC+CC)
       THICE=THICE+DC*(QAIR-QWATER)
       XGROW=DC*(QAIR-QWATER)

C  More conditions - That is good!
      IF(THICE.LE.0.0) THICE=0.0
      IF(THICE.EQ.0.0)  GOTO 92

      IF(TAIR(MDAY).GT.0.0) THEN
        THICE=THICE-HTICE*DC
        XRAD=-HTICE*DC 
      ENDIF

      IF(THSNOW.GT.0.0) GOTO 92
      
C Consider effect of rainfall PR(MDAY)
C For rainfall with having temperature = TAIR in C
C PR(MDAY) is in inches = 0.0254 meters !
C Heat balance: PR*As*DENw*Cp*(TAIR-0.0)=DHi*As*DENi*Latent
C DHi=PR*TAIR/80.0*(DENw*Cp/DENi) =PR*TAIR*0.0254/80.0
C IF DENw=1000.0, DENi=920.0kg/m**3, PR in inches and
C Cp=4.2 kJ/kg/C = 1.0 kcal/kg/C !!!
      IF(TAIR(MDAY).GT.0.0) THEN
        THICE=THICE-PR(MDAY)*0.0254*TAIR(MDAY)*DRATIO/80.0
        XRAIN=-PR(MDAY)*0.0254*TAIR(MDAY)*DRATIO/80.0
      ENDIF

 92   IF(THICE.LT.0.0) THICE=0.0
      OTHI=THICE
      
C The new algorithm for ice growth on the top of the ice
C     IF(SNOWMELT.GT.0.0) THEN
C       ACSNOW=ACSNOW+SNOWMELT
C     ENDIF
C Melted water from snow refroze during the cold night.      
C     IF(ACSNOW.GT.0.0.AND.TAIR(MDAY).LT.0.0) THEN
C       THICE=THICE+ACSNOW*DENSNOW/DENICE
C       XREFRO=ACSNOW*DENSNOW/DENICE
C       ACSNOW=0.0
C     ENDIF 

       WRITE(15,186) MONTH,MDAY,QAIR,QWATER,XGROW,XRAD,
     + XRAIN,XREFRO,THICE,THSNOW
186   FORMAT(I3,1X,I3,1X,2(F8.1,1X),4(F7.5,1X),2(F5.3,1X))
      
       ACSFL=ACSFL+SNOWFL(MDAY)      
C       WRITE(15,186) MONTH,MDAY,SNOWFL(MDAY),ACSFL,THSNOW,
C     + SNOWMELT,ACSNOW,OTHI,THICE
C 186   FORMAT(2X,I3,2X,I3,2X,7(F7.4,3X))
             
C....CALCULATE TOTAL HEAT FLUX ! 
C     IF(MDAY+MONTH*100.EQ.1216) THEN
C      HSTMSUM=0.0
C      QWATSUM=0.0
C      HTBTSUM=0.0
C      HTICSUM=0.0
C      IWRITE=6
C     ENDIF

C     QWATSUM=QWATSUM+QWATER*ATOP(1)/TV(MBOT)
C     HTBTSUM=HTBTSUM+HTBTM*ATOP(1)/TV(MBOT)
C     HSTMSUM=HSTMSUM+HSTMP
C     HTICSUM=HTICSUM+HCNDISO*ATOP(1)/TV(MBOT)
      
C      IF(IWRITE.EQ.6) THEN
C       WRITE(20,255) JDY,HSTMSUM,HTBTSUM,QWATSUM
C 255   FORMAT(I3,2X,3(F8.2,2X))     
C      ENDIF

C      DQFLUX=QWATER-HTBTM
C      WRITE(13,250) JDY,TAIR(MDAY),QAIR,QWATER,HTBTM,
C    + RAD(MDAY),HSA,HCNDISO,DQFLUX
C250  FORMAT(I3,1X,F6.1,2X,F7.1,1X,F7.1,1X,F6.1,1X,F6.1,
C    + 1X,F6.1,1X,F8.1,1X,F8.1)
 
      RETURN
      END
C***********************************************
C         
C         FUNCTION FOR INTEPOLATION
C
C***********************************************
       FUNCTION FXIN(ZDD,N,AD,AI)
       DIMENSION AD(30),AI(30)
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
 150   CONTINUE
        IF(ZDD.GT.AD(N).AND.I.GE.N) THEN      
          RAT=(AD(N)-AD(N-1))/(ZDD-AD(N-1))
          FXIN=AI(N-1)+(AI(N)-AI(N-1))/RAT
          AD(N)=ZDD
          AI(N)=FXIN
        ENDIF 
        RETURN
        END
C**********************************************************C
C
C
C**********************************************************C
      SUBROUTINE BOTTOM(TSOL,TSNE,T2MBOT)
C**** Solve the heat conduction equation for sediment temperature
C**** distribition Ts
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     +           CDIS0,CNDSNW0,DEPTHC,ICEMON,
     +         ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      DIMENSION PS(21),QS(21),AS(21),BS(21),CS(21),DS(21)
      DIMENSION TSOL(21),TSNE(21)
      
C Constant DZSL=DZ in meters, DT=1.0 day, AHTBTM=Kb thermal
C diffusivity of sediment in m*m/day !  
C Stability criteria: PRMM >= 2.0
      PRMM=DZSL**2/(AHTBTM*1.0)

C Set coefficient, What kind of scheme, TSL is previous day sediment
C temperature?  It seems I=1 is the bottom of sediment (deepest part)
C and I=IZSLT is sediment-water interface, ZSLT - depth of sediment!

      DO 3 I=1,IZSLT
       AS(I)=-1.0/PRMM
       BS(I)=1.0
       CS(I)=-1.0/PRMM
       DS(I)=(1.0-2.0/PRMM)*TSOL(I)
    3 CONTINUE

C What about boundary conditions !

C Solution techniques: (Please see Song's notes)
C  As(j)*Ts(j+1)+Bs(j)*Ts(j)+Cs(j)*Ts(j-1)=Ds(j)
C  Ts(j+1) = Ps(j)*Ts(j) + Qs(j)
C  Therefore Ps(j+1)=-Cs(j)/[As(j)*Ps(j)+Bs(j)],
C  Qs(j-1)=[Ds(j)-As(j)*Ps(j)]/[As(j)*Ps(j)+Bs(j)].

       PS(1)=1.0
       QS(1)=0.0
       PS(2)=-CS(2)/(AS(2)+BS(2))
       QS(2)=DS(2)/(AS(2)+BS(2))

      DO 10 I=3,IZSLT
       PS(I)=-CS(I)/(AS(I)*PS(I-1)+BS(I))
       QS(I)=(DS(I)-AS(I)*QS(I-1))/(AS(I)*PS(I-1)+BS(I))
   10 CONTINUE  

C Boundary condition at sediment-water interface, T2-water temperature
      TSNE(IZSLT)=T2MBOT

      DO 15 I=IZSLT-1,1,-1
        TSNE(I)=PS(I)*TSNE(I+1)+QS(I)
   15 CONTINUE     

      RETURN
      END        
C***********************************************
C         
C     SEDIMENT HEAT FLUX OF ALL LAYERS
C
C***********************************************
      SUBROUTINE SEDIMENT(HTBTM,T2MBOT,K)
C**** Call subroutine BOTTOM for Ts, then calculate heat flux
C**** from or to the lake sediment (HTBTM) 
      INTEGER FMON,FDAY,FYEAR
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     +DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     +PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     +         CDIS0,CNDSNW0,DEPTHC,ICEMON,
     +         ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      DIMENSION TSO(21,40),TSN(21,40)


      IF(ISTART.LE.6) THEN
       DO 10 I=1,IZSLT
        DO 10 J=1,MBOT
         TSO(I,J)=TSL(IZSLT-I+1,J)
  10   CONTINUE
      ISTART=60
      ENDIF
C	TSO - Old/yesterday sediment temperature (TS), TSN - today or current TS
       
      CALL BOTTOM(TSO(21,K),TSN(21,K),T2MBOT)
    
        HTBTM=0.0
      DO 16 I=1,IZSLT-1
        HTBTM=HTBTM+SRCP*DZSL*(TSO(I,K)-TSN(I,K))
        TSO(I,K)=TSN(I,K)
 16   CONTINUE     
      
      RETURN
      END
C****************************************************C
C
C
C****************************************************C
      FUNCTION ENTRAIN(I,DCF,RDC,RHOAMB,DELZ,S,SUMZ,Q,IHP,WIDTH,FT)
C*****
C***** Compute the entrainment from a layer into the
C***** density current (from Akiyama)
C*****
      IF(I.GT.IHP) THEN
         EPSI=(RDC-RHOAMB)/RHOAMB
         IF(I.NE.IHP+1)  GO TO 3
         FD=1.875E-4+FT
         F43=((FD+SQRT(FD*FD+0.0045*S))/(1.5*S))**(1.3333)
 3       X=DCF/(86400*WIDTH)
         GAMAI=0.0015*DELZ*(9.81*EPSI/(X*X))**(.3333)/(F43*S)
         ENTRAIN=GAMAI*DCF
        ELSE
         ENTRAIN=Q*DELZ/SUMZ
      ENDIF
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
       SUBROUTINE ERROR(IST,IDO,TETH,DOTH)
C**** THE PROGRAM IS TO MAKE STATISTICAL
C**** PAPRAMETERS OF ERROR ANALYSIS
C**** CHECK THE GU'S OR FANG'S THESIS
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),
     + RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
      COMMON/NEW/NYEAR,KYEAR(25),FDTH(5),NDEPTH,NTDY(25)
      DIMENSION X(1600,2),Y(1600,2),DEPTH(1600),SLOPE(10),R2(10),STD(10)
      DIMENSION XDE(200,5,2),YDE(200,5,2),IN(5),TETH(5),DOTH(5)
      
CFX...STORE DTATA FOR ERROR ANALYSIS
C.....1 - TEMPERATURE, 2 - DISSOLVED OXYGEN
C.....XDE - SIMULATED, YDE - MEASURED

        IF(IN(1).LT.1) THEN
         DO 45 I=1,NDEPTH
          IN(I)=1
 45      CONTINUE
        ENDIF
    
      IF(IST.EQ.1) THEN
    
       DO 62 J=1,NDEPTH
        IN(J)=IN(J)+1
        II=IN(J)
        IF(IDO.EQ.1) THEN
C.... SIMULATIONS
          XDE(II,J,1)=TETH(J)
          XDE(II,J,2)=DOTH(J)
          IN(J)=IN(J)-1
        ELSE
C.... FIELD DATA       
         YDE(II,J,1)=TETH(J)
         YDE(II,J,2)=DOTH(J)
        ENDIF
 62    CONTINUE       
       DO 68 J=1,NDEPTH
       IF(TETH(J).LT.0.0.OR.DOTH(J).LT.0.0) THEN
        IN(J)=IN(J)-1
       ENDIF
 68    CONTINUE
       RETURN
      ENDIF
      
C***   N -- NUMBER OF DATA
C***   X(I), Y(I) -- SIMUATED RESULTS AND MEASURED DATA
        
        REWIND 66

        DO 60 I=1,1600
         READ(66,*,END=70) Y(I,1),X(I,1),Y(I,2),X(I,2),DEPTH(I)
 60     CONTINUE
 70     CONTINUE
        DO 75 J=1,NDEPTH
         IN(J)=IN(J)-1
 75     CONTINUE         
        N=I-1
        K=1
 40     CONTINUE

C***   LEAST SQUARE ANALYSIS
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
          SXY=SXY+(X(I,K)-AX)*(Y(I,K)-AY)
          SXY2=SXY2+(X(I,K)-Y(I,K))**2.0
          SXX=SXX+(X(I,K)-AX)**2.0
          SYY=SYY+(Y(I,K)-AY)**2.0
  20    CONTINUE
        
          SLOPE(K)=SXY/SXX
          R2(K)=1.0-SXY2/SYY
          STD(K)=(SXY2/FLOAT(N))**0.5
         
         IF(K.EQ.2) GOTO  30
         IF(IPRNT(4).EQ.1) THEN
          K=2
          GOTO 40
         ELSE
          RMS(6)=0.0
          SLOPE(2)=0.0
          R2(2)=0.0
          STD(2)=0.0 
         ENDIF
                  
 30      WRITE(98,3015)
         WRITE(98,3016)
         WRITE(98,3030) RMS(1),RMS(6)
         WRITE(98,3017) SLOPE(1),SLOPE(2)
         WRITE(98,3018) R2(1),R2(2)
         WRITE(98,3019) STD(1),STD(2)
         WRITE(98,3020) N

 3030  FORMAT(1X,'MAXIMUM ABSOLUTE ERROR',9X,10(2X,F7.2))
 3015  FORMAT(1X///1X,'ANALYSIS OF ERRORS BETWEEN DATA AND MODEL')
 3016  FORMAT(1X/37X,'TEMP',7X,'DO')
 3017  FORMAT(1X,'SLOPE: MODEL TO DATA REGRESSION',10(2X,F7.2))
 3018  FORMAT(1X,'REGRESSION COEFFICIENT (R**2)',2X,10(2X,F7.2))
 3019  FORMAT(1X,'STANDARD ERROR OF ESTIMATE',5X,10(2X,F7.2))
 3020  FORMAT(1X/1X,'TOTAL NUMBER OF FIELD DATA IS:',4X,I6//1X)

      
       WRITE(98,3025)
       K=1
 90    DO 50 J=1,NDEPTH
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
        
        DO 56 I=1,IN(J)
          SXY=SXY+(XDE(I,J,K)-AX)*(YDE(I,J,K)-AY)
          SXY2=SXY2+(XDE(I,J,K)-YDE(I,J,K))**2.0
          SXX=SXX+(XDE(I,J,K)-AX)**2.0
          SYY=SYY+(YDE(I,J,K)-AY)**2.0
 56     CONTINUE
        
         IF(SXX.EQ.0.0.OR.SYY.EQ.0.0) THEN
C**** It happens when either simulated or measured DISSOLVED
C**** OXYGEN CONCENTRATIONS near the lake bottom ARE ZERO !!!   
          SLOPE(J+2)=1.0
          R2(J+2)=1.0
          STD(J+2)=(SXY2/FLOAT(N))**0.5
         ELSE   
          SLOPE(J+2)=SXY/SXX
          R2(J+2)=1.0-SXY2/SYY
          STD(J+2)=(SXY2/FLOAT(IN(J)))**0.5
         ENDIF
              
 50     CONTINUE
 
   
         IF(K.EQ.1) THEN         
          WRITE(98,3026)
         ELSE
          WRITE(98,3027)
         ENDIF
         WRITE(98,3028) (FDTH(II),II=1,NDEPTH)
         WRITE(98,3017) SLOPE(3),SLOPE(4),SLOPE(5),SLOPE(6),SLOPE(7)
         WRITE(98,3018) R2(3),R2(4),R2(5),R2(6),R2(7)
         WRITE(98,3019) STD(3),STD(4),STD(5),STD(6),STD(7)
         WRITE(98,3029) IN(1),IN(2),IN(3),IN(4),IN(5)
                  
         IF(K.EQ.2) RETURN
         IF(IPRNT(4).EQ.1) THEN
          K=2
          GOTO 90
         ENDIF          
 
 3025  FORMAT(1X//1X,'ANALYSIS OF ERRORS BETWEEN DATA AND',1X,
     +  'MODEL AT CERTAIN DEPTHS')
 3026  FORMAT(1X//37X,'WATER TEMPERATURES',/1X)
 3027  FORMAT(1X//37X,'DISSOLVED OXYGEN',/1X)
 3028  FORMAT(1X,'DEPTHS (M) FOR ERROR ANALYSIS',2X,5(2X,F7.2))
 3029  FORMAT(1X,'TOTAL NUMBER OF DATA POINTS',4X,5(2X,I7)//1X)
 
        RETURN 
        END       

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE ABOUND
C*****
C***** Computes the surface area of each layer (ATOP)
C***** using the depth area relationship in LAKE.
C*****     ATOP(1) = surface area of the lake
C*****     ATOP(MBOT+1) = 0.0
C*****
      REAL*8 A,V,TV,ATOP,ADUM
      INTEGER FMON,FDAY,FYEAR
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR  
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL

      DUM=0.0
      DO 100 I=1,MBOT
        ZDUM=ZMAX-DUM
        DUM=DUM+DZ(I)
        CALL LAKE(ZDUM,ADUM,0,1)
 100    ATOP(I)=ADUM
      ATOP(MBOT+1)=0.0
      RETURN
      END
C****************************************************C
C
C
C****************************************************C
      SUBROUTINE ADVECT(P,HED,NFLOW,S,FT,WCHANL,ST,MYEAR)
C*****
C***** Inflow/outflow routines using plunging density
C***** current routines on inflow and surface outflow.
C***** Additional inflows and outflows can be included
C***** with calls to LAKE (see below).
C*****
      REAL*8 A,V,TV,ATOP,ZERO
      INTEGER FMON,FDAY,FYEAR
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/INFLOW/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),
     + CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/RESULT/ VAR(40,21)
      
C     DIMENSION VOUT(11),IFL(11)
C     DATA IFL/1,3,5,6,7,8,9,10,11,12,13/
      ZERO=0.0
      TRASH=MYEAR


C...TAKE CARE OF INFLOW AND OUTFLOW
      DELZ=P-HED
      ZMAX=ZMAX+DELZ
      DZ(1)=DZ(1)+DELZ
      CALL SETZ(MBOT)
      V(1)=V(1)+DELZ*ATOP(1)

C...call to LAKE for additional surface inflows and outflows
      CALL LAKE(0.,ZERO,NFLOW,10)
      IF(NFLOW.GT.0) THEN
        IF(IPRNT(1).EQ.1) WRITE(8,2235)
 2235   FORMAT(//,5X,'INFORMATION ON INFLOW',/,5X,21('-'),/,6X,
     +  'ISOPYCNIC',3X,'INITIAL VOLUME',7X,'INFLOW  (M3/DAY)',8X,'P',7X,
     +  'NO3',7X,'NH4',6X,'DO',6X,'BOD',/,6X,'DEPTH (M)',7X,
     +  'OF LAYER (M3)',4X,'INITIAL',8X,'FINAL',5X,5('(MG/L)',3X),/)

C...QIN > 0.0 signifies inflow.  QIN < 0.0 signifies outflow.
C...this section skipped if QIN = 0.0
        DO 14 IW=1,NFLOW
          DO 15 I=1,MBOT
 15         QE(I)=0.0
          IF(QIN(IW).GT.0.0) THEN
            CALL DCFLOW(IPL,S,FT,WCHANL,IW)
            CALL CONSMAS(IW,IPL,QIN(IW))
          ENDIF
          IF(QIN(IW).LT.0.0) THEN
            CALL WDEPTH(ST,QIN(IW),LW)
          ENDIF
          DO 17 I=1,MBOT
  17        V(I)=V(I)-QE(I)
          I=1

C...merge low volume layers with next higher layer
 4        IF(V(I).LT.500.) THEN
             CALL MERGE(I,MBOT,ILAY)
             IF(I.GT.MBOT) GOTO 300
             GOTO 4
          ENDIF
          I=I+1
          IF(I.GT.MBOT) GOTO 300
          GOTO 4
  300     CALL THICKNS(MBOT)
          CALL SETZ(MBOT)
 14       CONTINUE
      ENDIF
      I=1
C...determine that all layers are within the limits for
C...maximum and minimum layer thickness
 5    IF(DZ(I).LT.DZLL) THEN
           CALL MERGE(I,MBOT,ILAY)
           IF(I.GT.MBOT) GO TO 301
           GO TO 5
      END IF
      I=I+1
      IF(I.GT.MBOT) GO TO 301
      GO TO 5
 301  IF(MBOT.GE.40) GO TO 2
      I=1
      DZOLD=DZUL
 3    IF(I.LE.3) THEN
       DZUL=DZOLD/3.0
      ELSE
       IF(I.LE.6) THEN
        DZUL=DZOLD/2.0
       ELSE
        DZUL=DZOLD
       ENDIF
      ENDIF
         
      IF(DZ(I).GT.DZUL .AND. MBOT.LT.40) THEN
          CALL SPLIT(I,ILAY)
          GO TO 3
      END IF
      I=I+1
      IF(I.GT.MBOT .OR. I.GT.40) GO TO 2
      GO TO 3
   2  CALL SETZ(MBOT)
      
      DZUL=DZOLD
      
C...DETERMINE LAKE STAGE FROM WATER BUDGET
      ZMAX=Z(MBOT)+DZ(MBOT)*0.5
      ST=DBL+ZMAX
      CALL VOLUME(MBOT)
      CALL AREA
      CALL ABOUND
      CALL TVOL(MBOT)
      RETURN
      END
C**********************************************************C
C                                                          C
C                                                          C 
C**********************************************************C
      SUBROUTINE AREA
C***** 
C***** Compute the area through the middle of each layer
C***** using the depth-area relationship in LAKE
C*****
      REAL*8 A,V,TV,ATOP,ADUM
      INTEGER FMON,FDAY,FYEAR
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      
      DUM=0.0
      DO 100 I=1,MBOT
        ZDUM=ZMAX-DUM-DZ(I)/2.
        DUM=DUM+DZ(I)
        CALL LAKE(ZDUM,ADUM,0,1)
 100    A(I)=ADUM
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE CHLORO(K,TD,IEUPH)
C*****
C***** Compute the chlorophyll-a and BOD concentration
C***** profiles using the constant volume finite difference
C***** method (from Patankar) for models 2 and 3.  Model 1
C***** computes the mixed layer chlorophyll-a concentration
C***** following work by Forsberg and Shapiro.  Model 2 uses
C***** Michaelis-Menton growth kinectics and Model 3 uses a 
C***** cellular nutrient growth formulation (from Lehman, et. al.)
C*****
C MIKI REMOVE BB,UN,PDUM
      REAL*8 A,V,TV,ATOP,AK,BK,CK,DK
      INTEGER FMON,FDAY,FYEAR
      COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,
     + XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),
     + REPRO,XI,XKMZ,GRAZE(3,40)
      COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,
     + YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
      COMMON/PHYTO/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),
     + XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),
     + GROMAX(3),TMAX(3),TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),
     + HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN,CHLMAX,SECCHI
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS
     + ,PLOT(30)
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/SOURCE/RM(3,40),PROD(40),XMR(3,40),PRODSUM(40)
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/SOLV/ AK(60),BK(60),CK(60),DK(60)
CM     DIMENSION BB(40),PDUM(40),UN(40)
CM
      K=K-1+1
      TD=TD+1.0-1.0      
CM
      IF(MODEL.EQ.1) THEN
         DMIX=Z(ILAY)+DZ(ILAY)*0.5
         IDEPTH=ILAY
         IF(ILAY.LT.IEUPH) IDEPTH=IEUPH
         DO 23 I=1,IDEPTH
           X=1.-PMIN(1)*CHLA2(1,I)/PTSUM(I)
           IF(X.LT.0) THEN
              PROD(I)=0.0
            ELSE
              PROD(I)=1.9*PMAX(1)*X/
     +         ((XK1+XK2*CHLA2(1,I))*DMIX*YCA)
           ENDIF
 23      CONTINUE
         IF(IDEPTH.LT.MBOT) THEN
           DO 24 I=IDEPTH+1,MBOT
 24          PROD(I)=0.0
         ENDIF             
         DO 25 I=1,MBOT
           CHLA2(1,I)=CHLA2(1,I)+CHLA2(1,I)*(PROD(I)-XKM(1))
           IF(CHLA2(1,I).LT.0.0001) CHLA2(1,I)=0.0001
           CHLATOT(I)=CHLA2(1,I)
 25        CHLADUM(1,I)=CHLA2(1,I)
         RETURN
      ENDIF
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE COEF(MODEL,MBOT,NCLASS)
C***** 
C***** Compute some coefficients used in the constant
C***** volume and finite difference solutions
C*****
      REAL*8 A,V,TV,ATOP
      COMMON/COEFF/ DUM2(40),DUM3(40)
      COMMON/VOL/ ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/FLOW/ HMK(41),QE(40),FVCHLA(5),PE(5,41)

      DO 100 I=2,MBOT-1
        DUM1= 2./(A(I)*DZ(I))
        DUM2(I)=DUM1*ATOP(I)*HMK(I)/(DZ(I)+DZ(I-1))
 100    DUM3(I)=DUM1*ATOP(I+1)*HMK(I+1)/(DZ(I)+DZ(I+1))
      KK=2
      IF(MODEL.GT.1) KK=1
CFX
      IF(MODEL.EQ.4) KK=2
      DO 200 K=KK,NCLASS+1
        DO 200 I=2,MBOT
          X=FVCHLA(K)*(DZ(I-1)+DZ(I))*.5/HMK(I)
C******- PE=(1.0-.1*ABS(X))**5/X 
          A0=1.0-0.1*ABS(X)
          A1=A0*A0
          PE(K,I)=A1*A1*A0/X
          PE(K,1)=0.0
 200    PE(K,MBOT+1)=0.0
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE CONMIX(ILAY,TMIX,MBOT)
C***** 
C***** Remove density instabilities by mixing unstable 
C***** layers downward and merging with lower layers.
C*****
      REAL*8 A,V,TV,ATOP,RHOT,RHODUM,RHO
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      DIMENSION RHOT(40)
 
c      write(*,*)"Entering CONMIX ..... "
      DO 100 I=1,MBOT
c      write(*,*)"I= ", I, "   T2(I) = ", T2(I)
        RHOT(I)=RHO(T2(I),0.,0.)
c      write(*,*)"I= ", I, "   RHOT(I) = ", RHOT(I)
 100  CONTINUE
 6    IFLAG=0
      I=0
      M=MBOT-1
 1    I=I+1
      IF(I.EQ.MBOT) GO TO 5
      IF(RHOT(I).LE.RHOT(I+1)) GO TO 1
      IFLAG=0
      IB=I
      TVDUM=T2(I)*V(I)
      VDUM=V(I)
      TDUM=TVDUM/VDUM
      RHODUM=RHO(TDUM,0.,0.)
      J=I-1
 3    J=J+1
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
 4    IF(I.LT.M) GO TO 1
 5    IF(IFLAG.NE.0) GO TO 6
 
C...DETERMINE MIXED LAYER DEPTH...
 8    DO 700 I=1,MBOT-1
        IF(ABS(T2(I)-T2(I+1)).LE.0.001) GO TO 700
        ILAY=I
        GO TO 10
 700  CONTINUE
      ILAY=MBOT
 10   TMIX=T2(1)
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE MERGE(I,MBOT,LW)
C*****
C***** Merge layers that are either low volume (V < 500 m3) or
C***** too thin (DZ < DZLL).  Negative layers are also handled
C***** by reducing the volume of the next lower layer by the 
C***** negative volume.
C*****
      REAL*8 A,V,TV,ATOP
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      IF(V(I).LE.0.) THEN
           IF(I.EQ.MBOT) THEN
              II=MBOT
 2            V(II-1)=V(II-1)+V(II)
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
  55       CHLA2(K,II)=(CHLA2(K,II)*V(II)+CHLA2(K,KK)*V(KK))*VCOMB
           IF(MODEL.EQ.3) THEN
            DO 57 K=1,NCLASS
 57         PC2(K,II)=(PC2(K,II)*V(II)+PC2(K,KK)*V(KK))*VCOMB
           ENDIF
         PA2(II)=(PA2(II)*V(II)+PA2(KK)*V(KK))*VCOMB
         BOD2(II)=(BOD2(II)*V(II)+BOD2(KK)*V(KK))*VCOMB
         DSO2(II)=(DSO2(II)*V(II)+DSO2(KK)*V(KK))*VCOMB
            IF(NITRO.EQ.1) THEN
              XNH2(II)=(XNH2(II)*V(II)+XNH2(KK)*V(KK))*VCOMB
              XNO2(II)=(XNO2(II)*V(II)+XNO2(KK)*V(KK))*VCOMB
              DO 56 K=1,NCLASS
 56           XNC2(K,II)=(XNC2(K,II)*V(II)+XNC2(K,KK)*V(KK))*VCOMB
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
 151    PC2(KI,K)=PC2(KI,K+1)
       IF(NITRO.EQ.1) THEN
         DO 152 KI=1,3
 152     XNC2(KI,K)=XNC2(KI,K+1)
         XNH2(K)=XNH2(K+1)
         XNO2(K)=XNO2(K+1)
       ENDIF
      ENDIF
      V(K)=V(K+1)
      DZ(K)=DZ(K+1)
      Z(K)=Z(K+1)-DZZ
 100  CONTINUE
 3    ZMAX= Z(MBOT) + 0.5*DZ(MBOT)
      RETURN
      END
C****************************************************C
C
C
C****************************************************C
      SUBROUTINE HEBUG(IL,TS,QN,HS,HA,HBR,HE,HC,
     +   TAIR,TDEW,CR,RAD,WIND,VC)
C*****
C***** Compute the temperature profile using routines FLXOUT and
C***** FLXIN for the surface heat exchange.  Solution is by the
C***** implicit central difference formulation.  CONMIX called to
C***** check for and resolve density instablities between layers.
C*****
      REAL*8 A,V,TV,ATOP,AK,BK,CK,DK
      INTEGER FMON,FDAY,FYEAR
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF
     +FMON,FDAY,FYEAR
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/SOLV/ AK(60),BK(60),CK(60),DK(60)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     + CDIS0,CNDSNW0,DEPTHC,ICEMON,
     + ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
      COMMON/TEFX/T2K(40),TEHE(40),BMK(40),OLDHQ,ITERF 
      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)
      COMMON/TMAXP/ TEMP(366,36),QX(7),DOXN(366,36)
      DIMENSION Q(40),QSED(40)

      IF(THICE.LE.0.0) THEN
C...OPEN WATER SEASON STUDY
C...CALCULATION OF THE HEAT ABSORPTION FROM METEOROLOGICAL
C...PARAMETERS IN A COLUMN OF WATER
C...CALCULATION OF HEAT FLUXES INTO THE WATER BODY

      CALL FLXIN(HS,HA,TAIR,RAD,CR,C2)
      CALL FLXOUT(TS,HBR,HE,HC,TAIR,TDEW,WIND,WCOEF)
      HQOUT=HBR+HE+HC

      ELSE
C...SNOW OR ICE COVER PERIOD *****
C...CALCULATION OF HEAT ABSORBED IN SNOW AND ICE LAYERS

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

C... Exclude unnecessary terms used for the open water season
      HA=0.0
      HE=0.0
      HC=0.0
      HQOUT=0.0

      ENDIF

C...CALCULATION OF EXTINCTION COEFF. (ETA) AS A FUNCTION OF SUSPENDED
C...SEDIMENT CONCENTRATION

      ETA=XK1+0.04317*C2(1)+XK2*CHLATOT(1)

C...CALCULATION OF HEAT ABSORBED IN EACH LAYER
C...HQIN(i) is the solar absorption at each horizontal 
C...layer in Kcal/day (whole lake as a control volume).
     
      HQ=(1.0-BETA)*HS
      EX=EXP(-ETA*DZ(1))
      Q(1)=((BETA*HS+HA-HQOUT)*ATOP(1)+HQ*(ATOP(1)-EX*ATOP(2)))
     +                                            /(1000.0*V(1))

C...CONVERSION FACTOR OF 1000 USED FOR DENSITY*HEAT CAPACITY OF WATER
      HQ=HQ*EX

C...CALCULATE THE SOURCE TERM Q FOR EACH LAYER
      DO 10 I=2,MBOT
        ETA=XK1+0.04317*C2(I)+XK2*CHLATOT(I)
        EX=EXP(-ETA*DZ(I))
        Q(I)=HQ*(ATOP(I)-ATOP(I+1)*EX)/(1000.0*V(I))
        HQ=HQ*EX
  10  CONTINUE
        
       DO 16 II=1,MBOT
         CALL SEDIMENT(QSED(II),T2(II),II)

         QFLUX=QSED(II)*(ATOP(II)-ATOP(II+1))/(V(II)*1000.0)

         IF(II.EQ.MBOT) HTBTM=QSED(II)

         Q(II)=Q(II)+QFLUX
 16    CONTINUE        

C....CALCULATE DIFFUSION COEFFICIENT... IMPORTANT!!
      ICV=1
      CALL SETAMK(WIND,VC,IL,MBOT,ICV)

C...SET-UP COEFFICIENTS FOR TRI-DIAGONAL MATRIX
      DO 100 I=2,MBOT-1
        D1= 2.0/(A(I)*DZ(I))
        D2=D1*ATOP(I)*HMK(I)/(DZ(I)+DZ(I-1))
        D3= D1*ATOP(I+1)*HMK(I+1)/(DZ(I)+DZ(I+1))
        AK(I)=-D2
        BK(I)=1.0+D2+D3
        CK(I)=-D3
        DK(I)=T2(I)+Q(I)
 100  CONTINUE
 
CFX...FORCING WATER TEMPERATURE T(1)=0.0 AS THICE>0.0
CFX...SET THE FIRST CONTROL VOLUME IS ZERO DEPTH OR VOLUME!
      AK(1)=0.0
      CK(1)=-2.0*ATOP(2)*HMK(2)/(A(1)*DZ(1)*(DZ(1)+DZ(2)))

      IF(THICE.GT.0.0) THEN
        BK(1)=1.0-CK(1)
        BK(1)=BK(1)+2.0*ATOP(1)*HMK(1)/(A(1)*DZ(1)*DZ(1))
      ELSE      
        BK(1)=1.0-CK(1)
      ENDIF
      DK(1)=T2(1)+Q(1)
      
        
CFX...AT LAKE BOTTOM - CONTROL VOLUM -- MBOT
      I=MBOT
      AK(MBOT)=-2.0*ATOP(I)*HMK(I)/(A(I)*DZ(I)*(DZ(I)+DZ(I-1)))
C     IF(THICE.EQ.0.0) THEN
C***  Post adiabtic conditions at the bottom
       CK(I)=0.0
C     ELSE
C      CK(MBOT)=-2.0*ATOP(I)*HMK(I+1)/(0.5*A(I)*DZ(I)*DZ(I))
C     ENDIF
      BK(MBOT)=1.0-AK(I)-CK(I)
      DK(MBOT)=T2(I)+Q(I)   
c	write(*,*)"I= ", I,"     ak(mbot)= ", ak(mbot)
c	write(*,*)"I= ", I,"     bk(mbot)= ", bk(mbot)
c	write(*,*)"I= ", I,"     ck(mbot)= ", ck(mbot)
c	write(*,*)"I= ", I,"     dk(mbot)= ", dk(mbot)
c	write(*,*)"I= ", I,"     t2(mbot)= ", t2(mbot)
c     CALL SOLVE(T2,MBOT)
      
C...  INCLUDE SEDIMENT HEAT FLUX
C...  Densiity - kg/m*3, Heat capacity - kcal/kg-C
c...  SRCP = R*Cp = kcal/kg-c; FOR WATER SRCP=1000*1.0 KCAL/KG-C
C...  HMK(I),AHTBTM ARE IN M**2/DAY HERE!
CFX.. AT WATER - SEDIMENT INTERFACE --- BOUNDARY CONDITION
C     I=MBOT+1
C     CONDW=HMK(I)*1000.0*1.0
C     CONDS=AHTBTM*SRCP
C     AK(I)=-2.0*CONDW/DZ(I-1)
C     BK(I)=2.0*(CONDW/DZ(I-1)+CONDS/DZSL)
C     CK(I)=-2.0*CONDS/DZSL
C     DK(I)=0.0 
C*** We had difficult to estimate heat flux by temperature gradients.

C Constant DZSL=DZ im meters, DT=1.0 day, AHTBTM=Kb thermal
C diffusivity of sediment in m*m/day !  
C Stability criteria: PRMM >= 2.0
C     PCOE=(AHTBTM*1.0)/DZSL**2.0
C     M=MBOT+1
C     DO 150 I=M+1,M+11
C       AK(I)=-PCOE
CX      BK(I)=1.0
C       BK(I)=1.0+2.0*PCOE
C       CK(I)=-PCOE
CX      DK(I)=(1.0-2.0*PCOE)*TSLP(I-M)
C       DK(I)=TSLP(I-M)
C150  CONTINUE
 
CFX... AT THE FIRST SEDIMENT LAYER
C     I=M+1
C     AK(I)=AK(I)-PCOE
CX    DK(I)=DK(I)-PCOE*TSLP(I-M)
C     BK(I)=BK(I)+PCOE
      
CFX... AT VERY FAR SEDIMENT BOTTOM
C     I=M+11
C     CK(I)=0.0
CX    DK(I)=DK(I)+PCOE*TSLP(I-M)
C     BK(I)=BK(I)-PCOE

C     DO 165 I=1,MBOT
C       T2P(I)=T2(I)
C165  CONTINUE

C     CALL SOLVET2(TE,M+11)

C     DO 160 I=1,MBOT+1
C       T2(I)=TE(I)
C160  CONTINUE

C     DO 170 I=MBOT+2,MBOT+12
C       TSL(I-M)=TE(I)
C170  CONTINUE

C Determine heat flux from sediment to water at lake bottom Zmax
C TSL(I) is sediment temperature, TSLP(1) is sediment
C temperature at previous time step (T-1).
C Heat flux at sediment-water interface is the summation
C of (1.0m**2)*DZSL*DENse*Cpse*(TSLP(I)-TSL(I))
C DENse is the sediment density = 2500.0 kg/m**3
C Cpse is the sediment heat capacity = 1.0*0.2388 kcal/kg/C.
C SRCP = DENse*Cpse = Kcal/m**3/C = 400 - 1000
C That will get more accurate results (MIKI AND STEFAN, 1994)?
C Bashar (1993) used Q=ks*dTs/dt, it is not so bad ? 

C     HTBTM=0.0
C     DO 1628 I=1,IZSLT
C       HTBTM=HTBTM+SRCP*DZSL*(TSLP(I)-TSL(I))
C       TSLP(I)=TSL(I)
C1628 CONTINUE     

C      IF(MYE.EQ.1958) THEN
C        IF(MONTH.EQ.1.AND.MDAY.EQ.1) THEN
C          KDY=1
C        ELSE
C          KDY=KDY+1
C        ENDIF

C...SQD is sediment heat flux for bottom layer MBOT
C         SQD=HTBTM*ATOP(MBOT)
C         SQSED=SQSED+SQD
C         QRA3=SQD/SQSED

C         IF(SQD.GT.0.0) THEN
C          SQ1=SQ1+SQD
C         ELSE
C          SQ2=SQ2+SQD
C         ENDIF
C...To get volume averages of heat fluxes
C         SQ1=SQ1/TV(MBOT)
C         SQ2=SQ2/TV(MBOT)
C         SQSED=SQSED/TV(MBOT)
C         SQABS=SQABS/TV(MBOT)
C         BANC=SQABS+SQSED
C         QRA1=SQSED/SQABS
C         QRA2=SQSED/BANC
C         QXX(MBOT)=SQD/TV(MBOT)

C = 1.0   QRA3=SQABS/QXX(1) is very good !
C SQSED, SQABS is in kcal/m**3/day
C SQSED is total sediment heat flux, negative from water
C to sediments, and positive from sediment to water.
C SQABS is available solar energy in the lake in Kcal/m**3/day
C SQABS is positive for solar heating, and negative for surface
C cooling.   BANC is net thermal energy within the water column
C in kcal/m**3/day

c       IXX=0
c       IF(THICE.GT.0.0) IXX=10
c       WRITE(12,977) KDY,SQSED,SQABS,
c    +    BANC,QRA1,QRA2,QRA3,IXX,SQ1,SQ2
c977    FORMAT(I3,2X,3F12.3,2X,3F7.2,I4,2F9.3)

c       WRITE(12,977) KDY,IXX,(QXX(K),K=1,MBOT)
c977    FORMAT(I3,2X,I3,2X,40F7.2)

c      ENDIF

C     ENDIF
              
      TS =T2(1)
      QN=HS+HA-HQOUT
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE FLXIN(HSN,HAN,TC,RAD,CC,C)
Clj** computes total radiation flux at the water surface
      
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP

C...CALCULATION OF THE TOTAL RADIATION FLUX INTO A BODY OF WATER IN
C...FROM NET SOLAR RADIATION (HSR) AND NET ATMOSPHERIC RADIATION (HSN)
C...IN KCAL/M*M
C...IDSO JACKSON FORMULA USED FOR ATM. RADIATION
C...CONVERT AIR TEMPERATURE IN DEGREE C TO DEGREE ABSOLUTE
      TCA=TC+273.
      HAN=1.171E-6*(1.-0.261*EXP(-7.77E-4*TC*TC))
     + *(TCA**4)*(1.+0.17*CC*CC)
C...CALCULATION OF NET SOLAR RADIATION AND CONVERSION TO KCAL/M*M/DAY
C...CALCULATION OF REFLECTED SOLAR RADIATION HSR
C...HSRW---- DUE TO CLEAR WATER USING KOBERGS CURVES
C...HSRS---- DUE TO SUSPENDED SEDIMENTS AT THE WATER SURFACE
      HSR=(0.087-6.76E-5*RAD+0.11*(1.-EXP(-0.01*C)))*RAD

      IF(THICE.GT.0.0) HSR=0.0
      HSN=(RAD-HSR)*10.0
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE FLXOUT (TT,HBR,HE,HC,TAIR,TD,WIND,WCOEF)
Clj** Computes evaporative, convective, and back radiation fluxes
Clj** at the water surface     
      REAL*8 A,V,TV,ATOP
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      DALPHA=WCOEF
C...CALCULATES THE ENERGY FLUX OUT OF A BODY OF WATER FROM 
C...EVAPORATIVE HEAT LOSS (HE), CONVECTIVE HEAT LOSS (HC), AND
C...BACK RADIATION (HBR) IN KCAL/M*M/DAY.

C...ESTIMATE WIND FUCTION FROM ADAMS 1989

C VAPOR AND ATM PRESSURE ESTIMATION (Pa)
      ESA=611.0*EXP((17.27*TT)/(237.3+TT))
      EA=611.0*EXP((17.27*TD)/(237.3+TD))

C CONVERT Pa to milibars
      ESA=ESA*0.01
      EA=EA*0.01

C CONVERT WIND FROM MI/H TO M/SEC
      WINDHF=WIND*0.44704

C ESIMATE VIRTUAL TEMPERATURES
C...PTOT in mb
      PTOT=981.74
      TVSRF=((TT+273.15)/(1-0.378*(ESA/PTOT)))
      TVAIR=((TAIR+273.15)/(1-0.378*(EA/PTOT)))
      DTV=TVSRF-TVAIR
      IF (DTV.LT.0.) DTV=0.
C...CONVERSION OF TEMP. VALUES FROM DEG. C TO DEG. ABSOLUTE
      TSK=TT+273.15
      TAK=TAIR+273.15
C...Evaporation equation (Adams,1989)
C...He in W/m^2
C...0.6368 converts W(10 m) to W(2 m).
      FCN1=2.7*DTV**(1./3.)
      FCN2=3.1*0.6368*WINDHF
      HE=DALPHA*(FCN1+FCN2)*(ESA-EA)
C...Convert from W/m^2 to kcal/m^2*day
      HE=HE*20.63

C...CALCULATE CONDUCTIVE LOSS USING BOWENS RATIO
C...Hc in W/m^2
      HC=0.61*(PTOT/1000.)*(TT-TAIR)*(FCN1+FCN2)*DALPHA
C...Convert from W/m^2 to kcal/m^2*day
      HC=HC*20.63

C...CALCULATES BACK RADIATION
      HBR=(1.171E-6*0.97*TSK**4)

      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE SETAMK(W,VC,ILAY,MBOT,ICV)
C***** 
C***** Compute vertical diffusion coefficient in each layer.
C***** Diffusion coefficient between layers as the harmonic
C***** mean of the diffusion coefficients in adjacent layers.
C*****
      REAL*8 A,V,TV,ATOP,RHO
      DIMENSION AMK(41)
      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     +DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     +PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     +       BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      DIMENSION PSQN(40)

      TRASH=VC
C
      IF(THICE.GT.0.0) GOTO 4144   
C
C...Vertical diffusion coefficient in the mixed layer       
C...computed as a function of the wind speed
C      HKC=HKMAX*8.66E-3
C---MINLAKE MODEL (RILEY)       
C      DKM=28.0*W**1.3
C       HKMAX=28.0*W**1.3
C---FROM WALTER
C       DKM=1.74*W
C      IF(DKM.GT.HKMAX) DKM=HKMAX
CMIKI   DEN2=RHO(T2(J),C2(J),CD2(J))
C       RI=9.81*(RHO(T2(J+1),C2(J+1),CD2(J+1))-RHO(T2(J),C2(J),CD2(J)))
C     + *(Z(ILAY)+DZ(ILAY)/2.0)/((VC**2.)*DEN2)
C       RI=ABS(RI)
C       IF(RI.EQ.0.0) THEN 
C       DKM=HKMAX
C       GOTO 16
C       ENDIF
C       C=ZMAX/34.0
C       BETA=34*0.1/ZMAX
C       DKM=VC/((1/C)+(BETA*RI))*86400.0
C  16   CONTINUE     

CFX --TEST DIFFERENT Kz (CONSTANT THROUGHOUT SEASON) IN THE
C  EPILIMNION -- TEST TEMP. DIFFERENCE IN EPIL. (FEB,1993)
C      JULIAN=MONTH*100+MDAY      
C      IF(JULIAN.EQ.401.AND.IREAD.GE.0) THEN
C        IDAY=90
C        WRITE(*,*) 'PLEASE INPUT KZ EPILIMNION ='
C        READ(*,*) EPKZ
C        DKM=EPKZ
C        IREAD=-666       
C      ELSE
C        EKM=EPKZ
C        IDAY=IDAY+1
C      ENDIF

C...HMK TEMPORARILY USED TO STORE DENSITIES.
C...Diffusion coefficient below the mixed layer computed as
C...a function of the square of the Brunt-Vasala frequency (SQN)
C...SQN= (G/RHO) * d(RHO)/dZ
      
      AREA=EMCOE(1)*ATOP(1)/(10**6.0)
      ALFA=8.17*0.0001*(AREA)**0.56
      HKMAX=ALFA/(0.000075**0.43)*8.64

C... FROM WALTERS et al. (1978)
C... DISSOLVED OXYGEN MODEL !
      IF(ICV.EQ.6) THEN
       HKMAX=1.74*W
      ENDIF
          
      IF(THICE.GT.0.0) THEN
        HKMAX=0.0125
        ALFA=ALFA/2.0 
        ILAY=1
      ENDIF

      DO 100 I=1,ILAY
        AMK(I)=HKMAX
 100  CONTINUE

      DO 200 I=ILAY,MBOT
 200  HMK(I)=RHO(T2(I),C2(I),CD2(I))

      DO 300 I=ILAY+1,MBOT-1
      AVRHO=(HMK(I-1)+HMK(I+1))*0.5
      SQN=ABS(HMK(I-1)-HMK(I+1))/((Z(I+1)-Z(I-1))*AVRHO)*9.81
      PSQN(I)=SQN

      IF(SQN.LT.0.000075) THEN
       SQN=0.000075
      ENDIF

      AMK(I)=(ALFA/((SQN)**0.43))*8.64

 300  CONTINUE     

C---- ASSUME AMK(MBOT-1)=MAX AMK FOR SQN=0.000075
      AMK(MBOT)=AMK(MBOT-1)
      DO 400 I=2,MBOT
       HMK(I)=2.0*AMK(I)*AMK(I-1)/(AMK(I)+AMK(I-1))
       IF(HMK(I).LE.0.0125) HMK(I)=0.0125
 400  CONTINUE

       HMK(1)=0.0125
C       HMK(MBOT+1)=0.0125
       HMK(MBOT+1)=HMK(MBOT)
C
 4144 CONTINUE
      IF (THICE.GT.0.0) THEN
C
      IP=IP+1
      DO 202 I=1,MBOT
      HMK(I)=RHO(T2(I),C2(I),CD2(I))
 202  CONTINUE
C
      DO 302 I=2,MBOT-1
      AVRHO=(HMK(I-1)+HMK(I+1))*0.5
      SQN=ABS(HMK(I-1)-HMK(I+1))/((Z(I+1)-Z(I-1))*AVRHO)*9.81
      IF (SQN.LT.0.000075) SQN=0.000075
      PSQN(I)=SQN
 302  CONTINUE     
C
      PSQN(MBOT)=PSQN(MBOT-1)
C
C BELOW THE ICE
C       
      DAL=1.04*(0.00000001)
      DBETA=0.43
      DO 4149 I=2,MBOT
       AMK(I)=DAL*PSQN(I)**(-DBETA)
 
C CONVERT M2/SEC IN M2/DAY
       
       AMK(I)=AMK(I)*86400
 
 4149 CONTINUE
C
C ABOVE THE SEDIMENT SURFACE
C      
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
C
C ESTIMATE HARMONIC MEAN
C
      HMK(1)=0.0125
      AMK(1)=0.0125
      DO 4150 I=2,MBOT+1
      HMK(I)=0.0125+2.0*AMK(I)*AMK(I-1)/(AMK(I)+AMK(I-1))
 4150 CONTINUE
C      
      ENDIF
 
      RETURN
      END
C************************************************************C
C                                                            C 
C                                                            C
C************************************************************C
      SUBROUTINE DISOLID(VAR2,XWIND,RAD,TD,ST)
C*****
C***** Compute the concentration of dissolved oxygen
C***** by solving the one dimensional diffusion equation
C***** by the fully implicit central difference scheme.
C*****
      REAL*8 FAO,A,V,TV,ATOP,AK,BK,CK,DK,SVOL
      INTEGER FMON,FDAY,FYEAR
      DIMENSION VAR2(40)
CFX...For surface gas transfer study !
CFX...DIMENSION FDO(4),FGAM(4),RIN(40),SOU(4,40)
      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)
      COMMON/FIELD/ IFLAG(10),FLDATA(10,50),DEPTH(50),NFLD(10),SD
      COMMON/SOLV/ AK(60),BK(60),CK(60),DK(60)
      COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,
     + YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/SOURCE/RM(3,40),PROD(40),XMR(3,40),PRODSUM(40)
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/PHYTO/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),
     + XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),
     + GROMAX(3),TMAX(3),TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),
     + HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN,CHLMAX,SECCHI
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/COEFF/DUM2(40),DUM3(40)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/SUB/SDZ(90),SZ(90),LAY(40),AVGI(24,90),SVOL(90)
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP

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

C***** Specify chlorophyll-a concentration in Main Program
C***** Before calling DISOLID subroutine !!

C***** Compute light distribution over a day in TD subsections.
       CALL LIGHT(RAD,TD,C2(1),ALBEDO)

C***** DETERMINE THE PHOTIC DEPTH ***************
       CALL EUPHZ(ZEUPH,IEUPH,ALBEDO)

C****  Divide each layer to several sublayer with 0.2 m thick
       CALL SUBLAY(IEUPH,NSLAY)         

C***** Compute average light in each sublayer
       CALL AVGLITE(IEUPH,TD)

C****  Determine light limitation and photosynthetic oxygen production.
       CALL PRODAVG(IEUPH,MBOT,1,TD)

C****  Specify the oxygen consumption term !
      DO 107 I=1,MBOT

C...Respiration and BOD in winter
       IF(THICE.GT.0.0) THEN
         RESP=0.00        
         BODK2=0.03

C...SOD in winter          
C...IN OLIGOTROPHIC LAKES
         IF(SD.GT.4.5) THEN
           SB20N=0.08
           IF(Z(I).GT.ZEUPH) SB20N=0.16

C...IN MESOTROPHIC LAKES
         ELSEIF(SD.GT.1.8) THEN
           SB20N=0.16
           IF(Z(I).GT.ZEUPH) SB20N=0.23

C...IN EUTROPHIC LAKES
         ELSE
           SB20N=0.23
           IF(Z(I).GT.ZEUPH) SB20N=0.30
         ENDIF 
       ELSE

C..FOR OPEN WATER SEASON SIMULATIONS          
C  BOD - Biochemical oxygen demand
          BODK2=BODK20*1.047**T20(I)
            
C  RESPIRATION - Plant respiration          
          RESP=XKR1(1)*1.047**T20(I)*CHLATOT(I)

C...SOD - Sediment oxygen demand
          SB20N=SB20*1.065**T20(I)

          IF(Z(I).GT.Z(ILAY)) THEN
            SB20N=EMCOE(2)*SB20*1.065**T20(I)
          ENDIF
          
C...Adjustment for low D.O. levels
         IF(DSO2(I).LT.0.2) THEN
            SB20N=0.0
            BODK2=0.0
            RESP=0.0
         ELSEIF(DSO2(I).LT.1.0) THEN
            SB20N=SB20N/2.0
            BODK2=BODK2/2.0
            RESP=RESP/2.0
         ENDIF
             
       ENDIF
          
        IF(Z(I).GT.ZEUPH) THEN
C         IF(Z(I).GT.Z(ILAY)) THEN
C           RESP=0.0
C         ENDIF
          RESP=0.0
          PRODSUM(I)=0.0
        ENDIF
           
        DK(I)=DK(I)-BODK2*BOD2(I)-RESP/Y2CHO2+PRODSUM(I)
     +                     -SB20N*(ATOP(I)-ATOP(I+1))/V(I)
  
C  FOR OUTPUT RESULTS ONLY
C       RIN(I)=-BODK2*BOD2(I)-RESP/Y2CHO2+PRODSUM(I)
C    +                -SB20N*(ATOP(I)-ATOP(I+1))/V(I)
C       SOU(1,I)=BODK2*BOD2(I)+RESP/Y2CHO2
C       SOU(2,I)=SB20N*(ATOP(I)-ATOP(I+1))/V(I)
C       SOU(3,I)=PRODSUM(I)
         
 107  CONTINUE

C****** For winter simuations with snow/ice cover
      IF(THICE.GT.0.0) THEN
        DOK0=0.0
      ELSE 

C******* Correct the wind speed by fetch *************
C******* Find wind speed in the center of lake in m/s
        W=XWIND*0.447
        FAO=0.0
        CALL LAKE(FTCH,FAO,0,2)
        ZB=0.8*ALOG(FTCH/2.0)-1.0718
        XWIND=W*1.666667*(ZB+4.6052)/(ZB+9.2103)
         
C*METHOD-1*******COMPUTE GAS TRANSFER VELOCITY (M/DAY)********
C*****************VERIFY ON JUNE 24, 1992********************         
       DOK0=0.02256*(0.10656*EXP(-0.0627*T2(1))+0.00495)**(-0.5)
     +     *XWIND**1.64/DZ(1)

C*METHOD-3******* COMPUTE GAS TRANSFER VELOCITY (M/DAY)******
C***** OLD FORMULA FROM MINLAKE MODEL (PAGE 42)  ************* 
C***** WITHOUT CORRECTION FROM FETCH FUNCTION *****************
C        DOK0=(0.641+0.128*(0.447*XWIND)**2)/DZ(1)
C               
C*METHOD-2*******COMPUTE GAS TRANSFER VELOCITY (M/DAY)********
C*FROM THOMANN IN PAGE 282 --- BANKS (1975) ******************
C       DOK0=(0.728*XWIND**0.5-0.317*XWIND+0.0372*XWIND**2)/DZ(1)
C
CFX* TEST DIFFERENT GAS TRANSFER VELOCITY K=CONSTANT IN THE
C*** EPILIMNION -- TEST TEMP. DIFFERENCE IN EPIL. (FEB,1993)
C      IF(JDY.EQ.JUDF.AND.IREAD.GE.0) THEN
C        WRITE(*,444)
C 444    FORMAT(/2X,'INPUT GAS RANSFER VELOCITY Ke = ',\)
C        READ(*,*) DOKE
C        DOK0=DOKE/DZ(1)
C        IREAD=-666       
C      ELSE
C        DOK0=DOKE/DZ(1)
C      ENDIF

      ENDIF

C************COMPUTE THE SATURATED OXYGEN FROM MINLAKE (PAGE 42)*******
C      DOXS=14.652-0.41022*T2(1)+7.99E-3*T2(1)**2-7.7774E-5*T2(1)**3

C************COMPUTE THE SATURATED OXYGEN FROM THOMANN***************
      T2K=T2(1)+273.15
      DOXS=-139.3441+1.575701E5/T2K-6.642308E7/T2K**2.0
     +      +1.2438E10/T2K**3.0-8.621949E11/T2K**4.0

C*****Correct saturation concentration with elevation ***************
C*****ST is the stage of the lake in meters above sea level *********
      DOXS=EXP(DOXS)*(1-0.000035*ST*3.2808)
      
      DK(1)=DK(1)+DOK0*DOXS
      BK(1)=BK(1)+DOK0
  
C*****CALL SOLVE SUBROUTINE****************
CFX---GAS TRANSFER
      DCI=VAR2(1) 

      CALL SOLVE(VAR2,MBOT)
      DO 500 I=1,MBOT
      IF(VAR2(I).LT.0.) VAR2(I)=0.0
 500  CONTINUE

C.....CHECK INFORMATION FOR GAS TRANSFER... JUNE, 1993
C       DT=1.0
C       DC=DOXS-DCI
C       ADC=VAR2(1)-DCI
C       
C       II=1
C       FH=DOK0*DZ(1)/HMK(II+1)
C       SDT=SQRT(HMK(II+1)*DT)
C       FZX(II)=Z(II)/(2.0*SDT)
C       FGAM(II)=FH*SDT
C       IF(DC.EQ.0) THEN
C        FDO(II)=0.0
C       ELSE 
C        FDO(II)=RIN(II)*DT/DC
C       ENDIF
        
C..     WRITE(28,84) FZX(1),FGAM(1),FDO(1),DC,ADC
C84     FORMAT(3X,5(F12.6,2X))
C..     WRITE(99,85) JDY,DOXS,VAR2(1)
C85     FORMAT(3X,I3,3X,F8.3,3X,F8.3)        
C..     WRITE(99,85)  JDY,T2(1)
C85     FORMAT(3X,I3,3X,F8.3)       
        
C        IF(KDY.EQ.NPRNT(NDAYS)) THEN
C         WRITE(99,94) KDY,ILAY
C  94     FORMAT(2X/2X,I4,3X,I3/)         
C         WRITE(99,95) (Z(I),PRODSUM(I),I=1,IEUPH) 
C  95     FORMAT(2X,F6.3,2X,F10.5)
C        ENDIF
            
C        WRITE(99,95)  JDY,RAD,PRODSUM(1),VAR2(1),DOK0*DZ(1)
C 95     FORMAT(1X,I5,2X,F9.4,2X,E10.3,2X,F8.3,2X,F8.2)

C******* CHECK PHOTOINHIBITION AT WATER SURFACE
C       DO 234 I=1,NSLAY  
C           IF(I.EQ.1) THEN
C             PROMAX=PSUB(1)
C             PZMAX=0.0
C           ELSE 
C             IF(PSUB(I).GT.PROMAX) THEN
C               PROMAX=PSUB(I)
C               PZMAX=SZ(I)
C             ENDIF
C           ENDIF
C 234   CONTINUE             
C       WRITE(16,335) JDY,PZMAX,ZEUPH
C 335   FORMAT(3X,I4,3X,F7.2,3X,F7.2)        

C       IF(MOD(JDY,10).EQ.0) THEN  
C        WRITE(28,224) MONTH,MDAY
C 224    FORMAT(3X//3X,I3,3X,I3//)  
C        DO 119 I=1,IEUPH         
C          WRITE(28,333) Z(I),(SOU(K,I),K=1,3)
C 333      FORMAT(3X,F6.2,3X,3(E12.4,3X))
C 119    CONTINUE
C       ENDIF 

C          WRITE(28,224) MONTH,MDAY
C  224     FORMAT(3X//3X,I3,3X,I3//)  
C        DO 115 II=1,NSLAY
C          WRITE(28,222) SZ(II),PSUB(II)
C  222     FORMAT(3X,F7.2,5X,F12.6)       
C  115   CONTINUE    
C        ENDIF
C  118   CONTINUE       

C     USE CONMIX IDEAS FOR DO IN MIXED LAYER
C     ILAY IS INDEX OF MIXED LAYER FROM CONMIX
C     OVDUM=0.0
C     VDUM=0.0
C     DO 66 I=1,ILAY
C       OVDUM=OVDUM+VAR2(I)*V(I)
C       VDUM=VDUM+V(I)
C66   CONTINUE
C     ODOM=OVDUM/VDUM
C     DO 68 I=1,ILAY
C       VAR2(I)=ODOM
C68   CONTINUE      
 
 600  RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE MTHDATA(MONTH,KDAYS,MYEAR,JDY)
C*****
C***** Read monthly meteorological data 
C*****
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
      COMMON/MTHD/TAIR(31),TDEW(31),RAD(31),CR(31),WIND(31),
     + PR(31),DRCT(31)
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
	COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(61,16),ELEV(61,16)
      DIMENSION EQT(12)
      DATA EQT /-0.13, -0.23, -0.16, -0.02, 0.06, 0.00, -0.09,
     .            -0.08,  0.06,  0.22,  0.25, 0.10/
      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1

C...FIND MET DATA FOR FIRST MONTH OF SIMULATION
      IF(KDAYS.EQ.0) THEN
        MTH=MONTH
        MYR=MYEAR
        READ(9,*)MONTH,KDAYS,MYEAR
  30    IF(MONTH.EQ.MTH.AND.MYR.EQ.MYEAR) GO TO 20
        READ(9,*,END=10,ERR=30)MONTH,KDAYS,MYEAR
        GO TO 30
  10    WRITE(99,1001)
        WRITE(8,1001)
        STOP
      ELSE
        READ(9,*) MONTH,KDAYS,MYEAR
      ENDIF
C...READ IN MONTH, NO. OF DAYS IN THE MONTH AND YEAR
C...READ IN AIR TEMP. AND DEW PT. TEMP. IN CELSIUS, WIND VELOCITY IN
C...M.P.H., SOLAR RADIATION IN LANGELY/DAY
C...CLOUD COVER IN Percent --Currently CR = Sunshine percent -- 100 - CR = Cloud cover
C...Snowfall SNOWFL(K) is in mm in the input data, then convert to meters.
C...Precipitation PR(K) is inches*100.0 in the input data, then convert to inches
 20   CONTINUE
      DO 104 K=1,KDAYS
        READ(9,*) TAIR(K),TDEW(K),WIND(K),DRCT(K),
c     +   RAD(K),SUN,CR(K),PR(K),SNOWFL(K)
     +   RAD(K),CR(K),PR(K),SNOWFL(K) 
        SNOWFL(K)=SNOWFL(K)/1000.0 
        PR(K)=PR(K)/100.0
 104  CONTINUE     
      DO 100 K=1,KDAYS
        TAIR(K)=(TAIR(K)-32.0)*0.5556
        TDEW(K)=(TDEW(K)-32.0)*0.5556
        CR(K)=(100.0-CR(K))*0.01
 100  CONTINUE

C.... Compute short wave radiation if not available
      DO K = 1, KDAYS
	   IF(RAD(K).EQ.9999.) THEN
            RAD(K) = 0.0
	      DO I = 1, 24 
               SLONG0 = 15.0*INT(SLON/15.0)
               D     = 0.409280*COS(0.017214*(172.0-(JDY+K-1)))
	         X     = I - 1
               H   = 0.261799*(X-(SLON-SLONG0)*0.066667+EQT(MONTH)-12.0)
               SINAL = SIN(SLAT*.017453)*SIN(D)
     &    		       +COS(SLAT*.017453)*COS(D)*COS(H)
               A0    = 57.2985*ASIN(SINAL)
               SRO = 2.044*A0+0.1296*A0**2-0.001941*A0**3+7.591E-6*A0**4
               SRO   = (1.0-0.0065*CR(K)*CR(K))*SRO*24.0
               IF (A0.LT.0.0) SRO = 0.0

C**** SI units ***** RHOW=1000.0, CP=4186.0 *** SRO in W/M**2
               SRO  = SRO*3.14E-8*1000.0*4186.0
               RAD(K) = RAD(K) + SRO * 0.08598
	      ENDDO
	   ENDIF
	ENDDO

 1001 FORMAT(//,5X,70('*'),//,20X,'PROGRAM ABORTED.',/,10X,
     + 'METEOROLOGICAL DATA FILE DOES NOT',/,15X,
     + 'MATCH YEAR OF SIMULATION',//,5X,70('*'))
 
      RETURN
      END

C*****************************************************C
C
C
C*****************************************************C
      SUBROUTINE PDEPTH(RHOMIX,RHOIN,Q,S,IHP,QENIN,
     + MBOT,SUMZ,FT,WIDTH,HP)
C*****
C***** Compute depth at plunge point and initial entrainment at 
C***** the plunge point (GAMAIN). (by Akyama)
C*****
      REAL*8 A,V,TV,ATOP
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
C...COMPUTE DEPTH AT PLUNGE POINT
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

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE SETUP
C*****
C***** Determine the initial thickness, volume, and area of layers
C***** and the total volume of above each layer from the depths given
C***** in the input data file.
C*****
      REAL*8 A,V,TV,ATOP,VDUM
      INTEGER FMON,FDAY,FYEAR
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR

      DZ(MBOT)=ZMAX-(Z(MBOT)+Z(MBOT-1))*.5
      Z(MBOT)=ZMAX-DZ(MBOT)*.5
      CALL LAKE(DZ(MBOT),VDUM,0,3)
      V(MBOT)=VDUM
      AZ=DZ(MBOT)
      TV(MBOT)=V(MBOT)
      DO 10 I=1,MBOT-2
        II=MBOT-I
        DZ(II)=Z(II+1)-(DZ(II+1)+Z(II)+Z(II-1))*.5
        Z(II)=(DZ(II)+Z(II)+Z(II-1))*.5
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

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      REAL*8 FUNCTION RHO (T,C,CD)
C...CALCULATES THE DENSITY OF WATER AS A FUNCTION OF TEMPERATURE PLUS
C...DENSITY DUE TO TOTAL SOLIDS (SUSPENDED AND DISSOLVED)
c..... Michaelis Riley formula
C     RHO=(.999878+T*(6.16608E-5+T*(-8.14577E-6+T*4.76102E-8)))*1000.
C    +     +(C+CD)*0.001
C..... Ellis Chris Formula !!
      RHO=999.869+T*(6.6741E-2+T*(-8.8556E-3+T*(8.2303E-5-T*5.516E-7)))
     +     +(C+CD)*0.001
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE SETZ(MBOT)
C*****
C***** Compute Z from DZ for each layer
C*****
      REAL*8 A,V,TV,ATOP
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL

      AZ=0.
      DO 100 I=1,MBOT
        Z(I)=AZ+DZ(I)*.5
        AZ=AZ+DZ(I)
 100  CONTINUE
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE SOLVE(VAR2,MBOT)
C*****
C***** Tri-diagonal matrix solving routine
C*****
      REAL*8 AK,BK,CK,DK,TT,TX(40)
      COMMON/SOLV/ AK(60),BK(60),CK(60),DK(60)
      DIMENSION VAR2(40)

      DO 60 I=2,MBOT
        TT=AK(I)/BK(I-1)
        BK(I)=BK(I)-CK(I-1)*TT
        DK(I)=DK(I)-DK(I-1)*TT
 60   CONTINUE
C**********BACK SUBSTITUTION**************
      TX(MBOT)=DK(MBOT)/BK(MBOT)
      DO 70 I=1,MBOT-1
        J=MBOT-I
 70     TX(J)=(DK(J)-CK(J)*TX(J+1))/BK(J)
      DO 80 I=1,MBOT
 80     VAR2(I)=SNGL(TX(I))
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE SPLIT(I,LW)
C*****
C***** Routine to split thick layers (DZ > DZUL) into two or more 
C***** layers of equal thickness.  All state variables are the same in 
C***** each new layer.  Volume is adjusted later.
C*****
      INTEGER T_FLAG,DO_FLAG
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/BAKCUPTDO/T_FLAG,DO_FLAG,MONBAK,DAYBAK,YEARBAK,
     +            	TBAK(40),DOBAK(40)

      IF(T_FLAG.EQ.0.OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0)) THEN
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
 1991 FORMAT(1X,/,1X,'Re-Split No.',I4, ' Layer. Before Re-Split, '
     +       'MBOT =',I4,'. After Re-split, MBOT =',I4)
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



C************************************************************C
C                                                            C
C                 START PROGRAM                              C
C                                                            C
C************************************************************C
      SUBROUTINE START(ST,S,FT,ISTART,INFLOW,ITER,LEN8,IFIELD,TSLAV,
     &                 PATH)
C*****
C***** Routine to read the input data file for initial
C***** conditions and input coefficients
C*****
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR,T_FLAG,DO_FLAG,DAYBAK,YEARBAK
      DIMENSION ZTS(2,40),XTS(2,40)

CFX   USING CHIAI AND CHLHY(I)
	COMMON/CHLAP/NCDAY(40,2),GCHLA(40,2),ICHLA
	COMMON/FIELD/ IFLAG(10),FLDATA(10,50),DEPTH(50),NFLD(10),SD

      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)
      COMMON/NEW/NYEAR,KYEAR(25),FDTH(5),NDEPTH,NTDY(25)
      COMMON/YROUND/ NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,
     + HKMXSM,WCFSM,WSSM,WCFSF,WSSF
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/SNX/CFSNOW,MDYSNOW,DZSL,IZSLT,AHTBTM,SRCP,
     + CDIS0,CNDSNW0,DEPTHC,ICEMON,
     + ICEDAY,MELMON,MELDAY,NSWSTAR,MNSNOW,CNDWI
      COMMON/BOTT/ SNOWFL(31),TSL(21,40),ZSL(21)
      COMMON/CONR/ FAKW,SNCOE,COEWIN
               
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/YIELD/YCA,YCHO2,Y2CHO2,YCBOD,YPBOD,YZW,YPZP,YNZP,YZDO,
     + YSCHL,YNHBOD,BRNO,BRNH,XKNNH,THNNH,YPCHLA,BODK20,SB20,BRR
      COMMON/PHYTO/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),
     + XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),
     + GROMAX(3),TMAX(3),TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),
     + HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN,CHLMAX,SECCHI
      COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,
     + XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),
     + REPRO,XI,XKMZ,GRAZE(3,40)
 
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
	COMMON/LOCATION/ISTATE,ISTATION,SLAT,SLON,YRAIR(61,16),ELEV(61,16)
      COMMON/BAKCUPTDO/T_FLAG,DO_FLAG,MONBAK,DAYBAK,YEARBAK,
     +            	TBAK(40),DOBAK(40)
      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
      CHARACTER*1 T1(16),PATH(64),FFILE(80),
     &            FILE1(16),FILE45(16),FILE27(16)
      EQUIVALENCE (T1(1),TAPE1),(FILENAME,FFILE(1))

      DATA FILE1 /'C','O','N','T','.','P','L','T',' ',' ',' ',' ',' ',
C     &	         ' ',' ',' '/,
C     &     FILE45/'c','h','l','a','.','p','a','t',' ',' ',' ',' ',' ',
C     &	         ' ',' ',' '/,
C     &     FILE27/'t','s','g','p','r','o','.','s','d','f',' ',' ',' ',
     &	         ' ',' ',' '/
 
      NITRO=0
      DO 200 I=1,100
        NPRNT(I)=0
 200  CONTINUE
Clj** Open another input file
      OPEN (8,FILE='D:\Inetpub\wwwroot\lake\FortranModel\INPUTb.INI') 
      READ(8,*)

C**** INPUT MODEL OPTIONS AND INITIAL CONDITIONS ******
C      READ(7,*) MODEL,NCLASS,IDIATOM
      READ(8,*) MODEL,NCLASS,IDIATOM
C      MODEL   = 4
C  	 NCLASS  = 1
C 	 IDIATOM = 0
Clj** model  : specifies level of biological simulation (1, 2, 3)
Clj** nclass : specifies number of phytoplankton types
Clj** idiatom: specifies number of silica limited phytoplankton groups (0 or 1)

C      READ(7,*) NITRO,ITER
      READ(8,*) NITRO,ITER
C      NITRO   = 0
C	 ITER    = 1
Clj** nitro  : specifies number of non-nitrogen limited phytoplankton groups (0 or 1)
Clj** iter   : number of iteration passes through the nutrient-biological subroutines

      READ(8,*) DZLL,DZUL,BETA,EMISS
Clj** DZLL, DZUL : minimum, maximum layer thickness (m)
Clj** BETA : surface adsorption fraction for solar radiation
Clj** EMISS : emmissivity of water

      READ(8,*) WCHANL,WLAKE,S,FT
Clj** WCHANL : Width of the inflow channel 
Clj** WLAKE  : Maximum width of the lake perpendicular to the inflow channel
Clj** S      : downstream slope of inflow channel
Clj** FT     : mannings friction factor for the inflow channel
 
      READ(8,*) ALPHA,BW
Clj** ALPHA  : side slope of the outflow channel
Clj** BW     : bottom width of the outflow channel

      READ(8,*) ZSLT,IZSLT
Cfx  ZSLT=Total depth of sediment (m)
Cfx  IZSLT=Number of sediment layers

      READ(8,*) MELMON,MELDAY,ICEMON,ICEDAY
Cfx  MELMON, MELDAY= The first month or day of ice melting.
Cfx  ICEMON, ICEDAY= The first month or day of freezing.

      READ(8,*) HKMAX,WCOEF,WCFSF
Clj** HKMAX : maximum hypolimnetic vertical turbulent diffusion coefficient (m2/day)
Clj** WCOEF : wind coefficient for convective heat loss, sheltering for summer
Clj** WCFSF : ?????                                                 for fall

      READ(8,*) TSLMEAN,ETAB,T2MBOT
Cfx  TSLMEAN= Annual mean temperature at 6 or 10M
Cfx  ETAB=COEFFICIENT, T2MBOT=Initial bottom water temperature?

      READ(8,*) HKMXIS,WCFIS,WSIS,EMCOE(1)
Cfx   HKMXIS=0.1 maximum hypolimnetic diffusion coefficient,
Cfx   WCFIS=0.0 wind function coefficient with ice cover.
Cfx   WSIS=0.0 wind sheltering coefficient with ice cover.
C**** EMCOE(1) is percentage of surface area which contributes to mixing, typical 50%
Clj**
      READ(8,*) BRR,FVCHLA(NCLASS+1)
Clj** BRR    : benthic phosphorus release rate
Clj** FVCHLA : setting velocity for algae

      READ(8,*) YCHO2,Y2CHO2,YCBOD,YPBOD,YNHBOD,YPZP,YZDO,YZW,YNHZP
Clj** YCHO2  : mass ratio of chlorophyll to oxygen in photosynthesis
Clj** Y2CHO2 : mass ratio of chlorophyll to oxygen in algal respiration
Clj** YCBOD  : mass ratio of chlorophyll to oxygen demand in detritus
Clj** YPBOD  : mass ratio of phosphorus to oxygen in detritus
Clj** YNHBOD : mass ratio of ammonium to oxygen in detritus
Clj** YPZP   : mass of phosphorus per individual zooplankter
Clj** YZDO   : oxygen demand per individual zooplankter in zooplankton decay
Clj** YZW    : mass of individual zooplankter
Clj** YNHZP  : mass of ammonium per individual zooplankter

c     close( 8 )

      READ(7,*) MONTH,ISTART,MYEAR,FMON,FDAY,FYEAR
c      write(*,*) "Month = ", MONTH,ISTART,MYEAR,FMON,FDAY,FYEAR
      
Clj** MONTH  : first month of simulation
Clj** ISTART : day of the month that simulation starts
Clj** MYEAR  : Year at the beginning of the simulation
c**** FMON,FDAY,FYEAR    : final simulation month, day & year

      NYTOT = FYEAR - MYEAR + 1
	IF(NYTOT.EQ.1) THEN
	   NM = FMON - MONTH + 1
	   NMFIN = NM
	ELSE
	   NM = 13 - MONTH
	   NMFIN = FMON
	ENDIF

      READ(7,*) MBOT,ZMAX,ST
c      write(*,*) MBOT,ZMAX,ST
Clj** MBOT   : number of layers in the initial conditions
Clj** ZMAX   : maximum depth of the lake
Clj** ST     : initial stage of lake in same datum as DBL (lake surface elevation)
	DBL = ST - ZMAX
      ELCB = ST + 1.0

      READ(7,*) XK1,XK2
Clj** XK1 and XK2 : light attenuation coefficient for water (1/m) and cholorphyll-a (1/mg.m)
C**** Kw = XK1 + XK2*CHLa = 1.84/sd

      READ(7,*) WSTR,WSSF
Clj** WSTR : wind coefficient for convective heat loss, sheltering for summer
Clj** WSSF : wind coefficient for convective heat loss, sheltering for fall

Cfx   For open water seasonal without ice cover!
Cfx   SM - for summer OR OPEN WATER SEASON !
Cfx   WE NEED TO STORE COEFFICIENTS FROM INPUT FILE !
      HKMXSM=HKMAX
      WCFSM=WCOEF
      WSSM=WSTR

      READ(7,*) COEWIN,SNCOE
CfX  SNCOE - MAXIMUM RATO OF THsnow TO THice (Empirical)
Cfx  COEWIN - TEMPORARILY PARAMETER FOR WIND SHELTERING 

      READ(7,*) AHTBTM,SRCP
Cfx  AHTBTM=Thermal diffusivity of sediment (m*m/day)
Cfx  SRCP = DENSITY*Cp FOR SEDIMENT !

      READ(7,*) CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC
Cfx  CFSNOW=snow compaction factor (0.2 - 0.4) = 0.2;
Cfx  CDIS0=ice conductivity in W/m/C or cal/day/m/C;
Cfx  CNDSNW0=snow conductivity in W/m/C; 
Cfx  CNDWI=turbulent conductive heat transfer coefficient
Cfx  in KCAL/DAY-M-C (11.35 Kcal/day-m-C=0.55 W/m-C),
Cfx  Actually CNDWI is molecular heat diffusivity, Not turbulent!
Cfx  DEPTHC= 0.1 M, CNDWI = K*11.35, K=1.5 - 2.0! (FANG, 1994)

      FAKW = 1.0
Cfx  FAKW is a factor to increase dT/dz at z=DEPTHC and 
Cfs  to finally obtain dT/dz at z=0 (ice-water interface).

      READ(7,*) BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW
Cfx   BT-Absorption  coefficient, ALF-Reflectivity coefficient
Cfx   GM-Attentuation coefficent for ICE, SNOW

      READ(7,*) THICKIS,THICKSN
Cfx   Initial ice and snow thickness - THICKIS, THICKSN (M)

      THSNOW=THICKSN
      THICE=THICKIS
      NSWSTAR=0

      READ(7,*)  (Z(I),I=1,MBOT)

      READ(7,*)  (T2(I),I=1,MBOT)
Clj** Initial conditions of depths (Z), temperature (T2)


Clj** Initial conditions of Suspended solids concentration (C2),
Clj** Total dissolved solids concentration (CD2), Chlorophyll-a (CHLA2)      
      READ(7,*)  (C2(I),I=1,MBOT)
      READ(7,*)  (CD2(I),I=1,MBOT)
      DO 100 K=1,NCLASS
         READ(7,*)  (CHLA2(K,I),I=1,MBOT)
 100  CONTINUE     

      READ(7,*) IPRNT(4)
c      write(*,*) "iprint4 = ", iprnt(4)
C20**IPRNT(4) = 0, Simulate temperature only !!
C20**IPRNT(4) = 1, Simulate Temp and DO concentration !!!
	
	IF(IPRNT(4).EQ.1) THEN
         READ(7,*) (PA2(I),I=1,MBOT)
         READ(7,*) (BOD2(I),I=1,MBOT)

Clj** Available phosphorus concentration (PA2), Total BOD (BOD2), Dissolved Oxygen (DSO2).
C....If there is no initial DO ptofile, DSO2 readings were set to 0.0
         READ(7,*) (DSO2(I),I=1,MBOT)
 
C***** INPUT PARAMETERS FOR BIOLOGICAL ROUTINES *****
Clj**
         READ(7,*) BODK20,SB20,XKR1(1),POMAX
Clj** BODK20 : detrital decay rate (1/day)
Clj** SB20   : benthic oxygen demand coefficient
Clj** XKR1(1): algal respiration rate in the euphotic zone (1/day)
C**** POmax  : maximum photosynthesis ratio to control D.O. production

         READ(7,*) EMCOE(2),EMCOE(3)
c	write(*,*)"EMCOE(2)=", EMCOE(2),"   EMCOE(3)=",EMCOE(3)
CFX
C**** EMCOE(2) is a multiplier to increase SOD below the euphotic Zone
c**** EMCOE(3) is a constant for computing the kw from CZS/Zs

      ENDIF

      READ(7,*) IPRNT(2),IPRNT(5),IPRNT(6)
c	write(*,*)"IPRNT(2)=",IPRNT(2),"    IPRNT(5)=",IPRNT(5),
c     &"     IPRNT(6)=",IPRNT(6)
C20**IPRNT(2) = 0, Do not create outflow fiel !!
C20**IPRNT(2) = 1, CREATE outflow file !!! "tape8.ou"
C20**IPRNT(5) = 0, Do not create PLOT fiel !!
C20**IPRNT(5) = 1, CREATE PLOT file !!! "TAPE8.PL"
C20** IPRNT(6) is the number of depths PLOT(I) where simulated
C20** results will be saved. IPRNT(6) <= 10
    
C      WRITE(*,1006)
C 1006 FORMAT(' OUTFLOW FILE TO BE CREATED  (Y/N) ?    ',\)
C      READ(*,'(A)') X
C      IF(X.EQ.'Y' .OR. X.EQ.'y') THEN
C        IPRNT(2)=1

      IF(IPRNT(2).EQ.1) THEN
        TAPE1=TAPE8
        T1(LEN8+2)='O'
        T1(LEN8+3)='U'
        CALL MAKEFILE(PATH,T1,FFILE)
        OPEN(2,FILE=FILENAME,STATUS='NEW')
      ENDIF

C      WRITE(*,1000)
C 1000 FORMAT(' PLOT FILE TO BE CREATED  (Y/N) ?       ',\)
C      READ(*,1001) X

      IF(IPRNT(5).EQ.1) THEN
        TAPE1=TAPE8
        T1(LEN8+2)='P'
        T1(LEN8+3)='L'
        CALL MAKEFILE(PATH,FILE1,FFILE)
        OPEN (1,FILE=FILENAME,ACCESS='DIRECT',STATUS='NEW',RECL=4)
        IREC=1

C        WRITE(*,1004)
C 1004 FORMAT(' ENTER UP TO 10 DEPTHS TO BE SAVED ',/,
C     + ' END WITH A CHARACTER (#,#,#,...,X) :  ',\)

        READ(7,*) (PLOT(I),I=1,IPRNT(6))
Clj** PLOT : a list of depths to be saved on the plot data file.

        WRITE(1,REC=IREC) REAL(IPRNT(6))
        IREC=IREC+1
        DO 201 I=1,IPRNT(6)
          WRITE(1,REC=IREC) PLOT(I)
 201      IREC=IREC+1
      ENDIF

C      WRITE(*,1002)
C 1002 FORMAT(' TEMPERATURE SIMULATION ONLY  (Y/N) ?   ',\)
C      READ(*,1001) X
C      IPRNT(4)=1
C      IF(X.EQ.'Y' .OR. X.EQ.'y') IPRNT(4)=0

CFX OPEN FILES FOR OUTPUTTING DATA OF D.O. CONTOUR
CFX IDNUM(1) -- CONTROL NUMBER FOR OPENING FILES
C      WRITE(*,1007)
C 1007 FORMAT(' DO or TE CONTOUR FILES TO BE SAVED  (Y/N) ? ',\)
C      READ(*,1001) X
C 1001 FORMAT(A1)
      
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

      READ(7,*) NPRINT,INFLOW,JDY
c      write(*,*)"nprint =", nprint, "  inflow=", inflow, "  jdy=",jdy
Clj** NPRINT : interval between days for tabular output
Clj** INFLOW : number of inflow and outflow sources in the inflow data
Clj** JDY    : the Julian day of first simulation as constant
CM
      READ(7,*) IFIELD,ICHLA
c      write(*,*)"Ifield=",IFIELD,"ichla=",ICHLA
C20** Whether or not you have field and/or chl-a data 1=YES, 0=NO             
C20** If IFIELD = 1, You have measured field data for comparison
C20** IF IFIELD = 0, NO field data
      
      IF(IDIATOM.EQ.3) READ(7,*) (SI2(I),I=1,MBOT)
Clj** SI2 : initial silica concentration
      IF(NITRO.EQ. 1) THEN
        READ(7,*) (XNH2(I),I=1,MBOT)
Clj** 
        READ(7,*) (XNO2(I),I=1,MBOT)
Clj** 
      ENDIF

C20** Extract data at several depths for error analysis
      IF(IFIELD.EQ.1) THEN
	  READ(7,*) NDEPTH
	  FDTH(1)=1.0
	  FDTH(5)=ZMAX-1.0
	  DO II=2,4
	    FDTH(II)=FDTH(1)+(II-1)*(FDTH(5)-FDTH(1))/4.0
	  ENDDO
c	ELSE
c	 READ(7,*)
	ENDIF

      READ(7,*)     
      IF(IFIELD.EQ.1) THEN
        
C20**Number of dates and years which you have field data !       
        READ(7,*) NDAYS,NYEAR
c      write(*,*)"Ndays = ", NDAYS, "Nyear = ", NYEAR      
CFX   FOR SPECIAL LAKE SIMULATION -- THRUSH LAKE
CFX   FOR EUTROPHIC LAKES -- CHLA(I)=CHLHY(I) EXACTLY
        IF(NDAYS.GT.0) THEN
           II=1
           DO 105 I=1,NYEAR
Clj** Specific year and corresponding number of dates
              READ(7,*) KYEAR(I),NTDY(I)
Clj** number of month*100+days
              READ(7,*) (NPRNT(K),K=II,II+NTDY(I)-1)
           II=II+NTDY(I)
 105       CONTINUE
        ENDIF
      ENDIF
      
      READ(7,*)
clj      IF(ICHLA.EQ.1) THEN
      IF(IPRNT(4).eq.1.and.ICHLA.EQ.1) THEN
C20** Read field data of Chlorophyll-a concentration in mg/l
C20** in epilimnion CHLEP(I), and hypolimnion CHLHY(I) 
         READ(7,*) (CHLEP(I),I=2,NDAYS+1)
         READ(7,*) (CHLHY(I),I=2,NDAYS+1)

C20** Compute average SD from chlorophyll-a concentration
C20** In order to determine initial sediment temperature profile
	   SD=0.0
       DO 175 IXK=2,NDAYS+1
	   SD=1.84/(XK1+XK2*CHLEP(IXK))+SD
 175	 CONTINUE
	   SD=SD/FLOAT(NDAYS)
	
	ELSE if(IPRNT(4).eq.1) then

C20** If there are no field data for chlorophyll-a, you input an 
C20** annual mean chl-a concentration, use a default seasonal pattern
C	   READ (7,*) CHLMEAN,SD          10/20/2000
	   READ (7,*) CHLMEAN
c	write(*,*)"chlmean = ", CHLMEAN, "      EMCOE(3) =",EMCOE(3)
	   IF( EMCOE(3).GT.0.0) READ (7,*) SD
C         CALL MAKEFILE(PATH,FILE45,FFILE)
C	   OPEN (45,FILE=FILENAME)
         OPEN(45,FILE="D:\Inetpub\wwwroot\lake\FortranModel\chla.pat")
C READ the chlorophyll-a concentration data
         DO 466 IK=1,37
            READ(45,*) NCDAY(IK,1),GCHLA(IK,1),NCDAY(IK,2),GCHLA(IK,2)
 466     CONTINUE
      ENDIF

C   THIS INITIAL CONDITION IS VALID FOR JAN-APRIL (MINIMUM BOTTOM TEMP)
C   AND AUGUST-OCTOBER (MAXIMUM BOTTOM TEMPERATURE)  
C   ZSL - DEPTH FROM THE LAKE BOTTOM

Cfx USING SOME EXPONENTIAL FUNCTION TO SET INITIAL SEDIMENT TEMPERATURE
Cfx ETAB - coefficient for EXP function, T2MBOT - initial bottom water
Cfx temperature (3.42 C). TSLMEAN - annual mean sediment temperature.
Cfx  TSLP(I) is water temperature at previous time step. 

CFX*** JUDF is the Julian day of first simulation as constant
      JUDF=JDY
      NYEAR=1
      CHLEP(1)=CHLA2(1,1)      
      CHLHY(1)=CHLEP(1)      

      DZSL=ZSLT/FLOAT(IZSLT-1)
	ETAB=2.0

C     ASX=ATOP(1)
	
	CALL LAKE(ZMAX,ASX,0.,1)

C	ASX - AREA IN M**2
      RATIOX=(ASX**0.25)/ZMAX
      
C.... Set initial T & DO profiles if no corresponding input data 
	T_FLAG = 0        
	if( iprnt(4).eq.1 ) DO_FLAG = 0
C.... Assume no given initial T (T_FLAG=0) & no given initial DO (DO_FLAG=0)
C.... MONBAK,DAYBAK,YEARBAK store the starting simulation data
      MONBAK = MONTH
	DAYBAK = ISTART
	YEARBAK= MYEAR
C.... TBAK,DOBAK store the input data of initial T & DO
      DO I = 1,MBOT
	   TBAK(I) = T2(I)
	   IF(T2(I).NE.0.0) T_FLAG = 1
         if( iprnt(4).eq.1) then
	      DOBAK(I) = DSO2(I)
	      IF(DSO2(I).NE.0.0) DO_FLAG = 1
	   endif
	ENDDO

C.... Output the information on input T&DO	
C.... Reset the starting time of simulation if no inputted T & do 
      IF(T_FLAG.EQ.1.AND.(DO_FLAG.eq.0.and.iprnt(4).eq.1)) 
     +        write(99,*)"power(As,0.25)/Hmax = ",RATIOX
      write(99,*)
      write(99,*)"The input simulation start time ",month,istart,myear
	IF((T_FLAG.EQ.0).OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0
     +	.AND.RATIOX.LT.5)) THEN
        MONTH = 1
	  ISTART = 1
	  MYEAR = MYEAR-1
C.... Julian day must be reset
	  JDY = 1
        IF(T_FLAG.EQ.0) THEN
          IF(iprnt(4).eq.1.and.DO_FLAG.EQ.0) THEN
            WRITE(198,1982)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK
            WRITE(99,1982)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK
          ELSE
            WRITE(198,1983)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,
     +                     MONBAK,DAYBAK,YEARBAK
            WRITE(99,1983)MONTH,ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,
     +                     MONBAK,DAYBAK,YEARBAK
          ENDIF
        ELSE
          IF(iprnt(4).eq.1.and.DO_FLAG.EQ.0) WRITE(198,1984)MONTH,
     +        ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,MONBAK,DAYBAK,YEARBAK
          IF(iprnt(4).eq.1.and.DO_FLAG.EQ.0) WRITE(99,1984)MONTH,
     +         ISTART,MYEAR,MONBAK,DAYBAK,YEARBAK,MONBAK,DAYBAK,YEARBAK
        ENDIF
 1982 FORMAT(1X,/,1X,'Since initial T (temperature) and DO ',
     +     '(dissolved oxygen) is ',/,1X,'not available,',
     +     ' the simulation actually started on ',I2,'/',I2,'/',I4,/,1X,
     +     'in order to find appropriate T and DO profile for ',
     +     I2,'/',I2,'/',I4,'.') 
 1983 FORMAT(1X,/,1X,'Since initial T (temperature) is not available,',/ 
     +  ,1X,'the simulation actually started on ',I2,'/',I2,'/',I4,/,1X,
     +     'in order to find appropriate T profile for ',I2,'/',I2,'/',
     +  I4,'.',/,1X,'Given initial DO (Dissolved Oxygen) profile on ',
     +  I2,'/',I2,'/',I4,' was used.') 
 1984 FORMAT(1X,/,1X,'Since initial DO (dissolved oxygen) is not',
     +          ' available,',/,1X, 
     +     'the simulation actually started on ',I2,'/',I2,'/',I4,/,1X,
     +     'in order to find appropriate DO profile for ',I2,'/',I2,'/',
     +  I4,'.',/,1X,'Given initial T (temperature) profile on ',
     +  I2,'/',I2,'/',I4,' was used.') 
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
	   
C.... Select the Elevation based on the given state and station
	SELEV = ELEV(ISTATE,ISTATION)

C.... Set initial T profile to be half of mean annual local air temperature
      IF((T_FLAG.EQ.0).OR.(iprnt(4).eq.1.and.DO_FLAG.EQ.0
     +	.AND.RATIOX.LT.5)) THEN
         TSLMN = YRAIR(ISTATE,ISTATION)+YTAIR
	   TSLXX = TSLMN/2.0
	   IF(TSLXX.LT.4.0) TSLXX = 4.0
	   DO I = 1,MBOT
	      T2(I) = TSLXX
	   ENDDO
      write(99,*)"The estimated T on start moment : "
      write(99,*)(t2(i),i=1,mbot)
	ENDIF

C.... If Initial T OK & power(As,0.25)/HMAX>= 5, DO will be computed from  
C.... the mean Temperature of inputted T.
      IF((T_FLAG.EQ.1).AND.(iprnt(4).eq.1.and.DO_FLAG.EQ.0
     +	.AND.RATIOX.GE.5)) THEN
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
 1981 FORMAT(/,3X,'The initial input Temperature : ',
     +     /,6X,'Maximum T = ',F6.2,1X,'at',I4,' layer',
     +     /,6X,'Minimum T = ',F6.2,1X,'at',I4,' layer',
     +     /,6X,'Mean    T = ',F6.2,
     +     /,6X,'Maximum standard derivation : ',f6.2,
     +     /,6X,'Minimum standard derivation : ',f6.2)
 1985 FORMAT(/,1X,'Since this lake is more or less well mixed, the'
     +            ' initial DO profile',/,1X,' was estimated for given',
     +            ' initial T (temperature) profile,',/,1X,
     +            ' and the simulation started on ',I2,'/',I2,'/',I4)   
	   T2X = T2X + 273.15
	   DOXS = -139.34411+1.575701E5/T2X-6.642308E7/T2X**2.0
     +       +1.2438E10/T2X**3.0-8.621949E11/T2X**4.0
	   DOXS = 0.8*EXP(DOXS)*(1-0.000035*SELEV*3.2808)
	   IF(DOXS.GT.10.0) DOXS = 10.0
	   DO I = 1,MBOT
	      DSO2(I) = DOXS
	   ENDDO
	ENDIF

C.... Set initial DO profile to be 80% of saturation DO concentration
C.... which is dependent on temperature and eleveation. 
      IF((iprnt(4).eq.1.and.DO_FLAG.EQ.0).OR.T_FLAG.EQ.0) THEN
	   DO I = 1,MBOT
	      T2X = T2(I)+273.15
	      DOXS = -139.34411+1.575701E5/T2X-6.642308E7/T2X**2.0
     +        +1.2438E10/T2X**3.0-8.621949E11/T2X**4.0
	      DOXS = 0.8*EXP(DOXS)*(1-0.000035*SELEV*3.2808)
	      IF(DOXS.GT.10.0) DOXS = 10.0
	      DSO2(I) = DOXS
	   ENDDO
      write(99,*)"The estimated DO on start moment : "
      write(99,*)(dso2(i),i=1,mbot)
	ENDIF

C*** Set initial sediment temperature profile
      TSHAL=TSLAV/2.0

      IF(RATIOX.GE.4.0.AND.RATIOX.LE.5.0) THEN
       TSLXX=TSHAL-1.0+(TSHAL+3.0)*ALOG10(RATIOX)
      ENDIF

      IF(RATIOX.GT.5.0) TSLXX=TSLAV

      IF(RATIOX.LT.4.0) THEN

       TSLXX=TSHAL-1.0+(TSHAL+3.0)*ALOG10(RATIOX)
       DXTS=TSLAV-TSLXX
C	TSLAV = TS10(0), TSLXX = TS10(Hmax) 
C	SD - ZS is Secchi disk depth in meters 

       AVOLM=ASX/1000000.0*ZMAX
C      CONVERT AREA IN KM**2
       AFACT=SD**0.35*AVOLM**0.125
 
C       CALL MAKEFILE(PATH,FILE27,FFILE)
C       OPEN(27,FILE=FILENAME)
       OPEN (27,FILE="D:\Inetpub\wwwroot\lake\FortranModel\tsgpro.sdf")
       DO 85 I=1,38
	    READ(27,856) ZTS(1,I),XTS(1,I),XTS(2,I)
 856     FORMAT(3X,F5.2,5X,F4.2,4X,F5.2)
 85    CONTINUE

      ENDIF
      
       DO 10 J=1,MBOT

      IF(RATIOX.LT.4.0) THEN
         ZNORM=Z(J)/AFACT
 
	   IF(J.EQ.1) K=1
         
837	   IF(K.GT.37) GOTO 838
	   
	   IF(ZNORM.GE.ZTS(1,K).AND.ZNORM.LE.ZTS(1,K+1)) THEN
          DDZ=ZTS(1,K+1)-ZTS(1,K)
          DDX=ZNORM-ZTS(1,K)
          DDA=XTS(1,K+1)-XTS(1,K)
          XXTS=XTS(1,K)+DDA*DDX/DDZ
	    GOTO 838
         ELSE
          K=K+1
          GOTO 837
         ENDIF
	  
 838     IF(K.GE.38) XXTS=1.0
	   TSLXX=TSLAV-XXTS*DXTS
C	XXTS = [TS10(0) - TS10(Z)]/[TS10(0) - TS10(Hmax)]
C	TSLXX = TS10(Z), RATIO < 4.0
       ENDIF
	  
	 
	DO 5 I=1,IZSLT
        TSL(I,J)=T2MBOT*EXP(-ETAB*ZSL(I))+
     +             (1.0-EXP(-ETAB*ZSL(I)))*TSLXX
 5     CONTINUE
 
 10    CONTINUE
       
      CLOSE (25)


C      DO 5 I=1,IZSLT
C        ZSL(I)=FLOAT(I-1)*DZSL
C        TSL(I)=T2MBOT*EXP(-ETAB*ZSL(I))+
C     +             (1.0-EXP(-ETAB*ZSL(I)))*TSLMEAN
C        TSLP(I)=TSL(I)
C    5 CONTINUE

 1500 FORMAT( /,  5X,
     +/,5X,41HMINIMUM THICKNESS OF EACH LAYER (DZLL) = ,F5.2,9H METER(S)
     +/,5X,41HMAXIMUM THICKNESS OF EACH LAYER (DZUL) = ,F5.2,9H METER(S)
     + /,5X,40HSURFACE ABSORPTION COEFFICIENT (BETA) = ,F5.2,
     + /,5X,30HEMISSIVITY OF WATER (EMISS) = ,F5.2,
     + /,5X,'EXTINCTION COEFF. OF WATER (XK1) = ',F5.2,3X,'M**-1',
     + /,5X,'EXTINCTION COEFF. OF CHLA  (XK2) = ',F5.2,3X,'L/MG/M',
     + /,5X,'MAX. HYPOLIMNETIC DIFFUSIVITY (HKMAX) = ',F7.4,' M**2/D',
     + /,5X,'WIND FUNCTION COEFFICIENT (WCOEF) = ',F6.3,
     + /,5X,'WIND SHELTERING COEFFICIENT (WSTR)= ',F6.3,/,
     + /,5X,34HWIDTH OF INLET CHANNEL (WCHANL) = ,F6.2,9H METER(S))
 1501 FORMAT(5X,'LONGITUDINAL LENGTH OF LAKE (WLAKE) =',F10.2,7H METERS,
     + /,5X,30HDEEPEST BED ELEVATION (DBL) = ,F8.2,17H METERS ABOVE MSL,
     + /,5X,26HINITIAL LAKE STAGE (ST) = ,F8.2,17H METERS ABOVE MSL,
     + /,5X,16HBED SLOPE (S) = ,F10.8,
     + /,5X,29HROUGHNESS COEFFICIENT (FT) = ,F6.4,
     + //,5X,48HELEVATION OF BOTTOM OF OUTFLOW CHANNEL (ELCB) = ,F6.2,
     + 17H METERS ABOVE MSL,
     + /,5X,40HSIDE-SLOPE OF OUTFLOW CHANNEL (ALPHA) = ,F6.2,8H DEGREES,
     + /,5X,31HBOTTOM WIDTH OF CHANNEL (BW) = ,F6.2,7H METERS,
     + //,5X,34HINITIAL NUMBER OF LAYERS (MBOT) = ,I2,
     + /,5X,37HNUMBER OF MONTHS OF SIMULATION (NM) = ,I2,
     +/,5X,54HDAY OF MONTH OF THE FIRST DAY OF SIMULATION (ISTART) = ,I2
     + /,5X,53HINTERVAL AT WHICH RESULTS WILL BE PRINTED (NPRINT) = ,I3,
     + 7H DAY(S))

      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
       SUBROUTINE STATS(FLDATA,XX,IFLAG,DEPTH,I)
C*****
C***** Compute statistics and statisitical quantities with 
C***** Y designating field data and X designating model results.
C*****
      INTEGER FMON,FDAY,FYEAR
      COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),
     + RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR

      SUMXY(I)=SUMXY(I)+FLDATA*XX
      SUMX(I)=SUMX(I)+XX
      SUMY(I)=SUMY(I)+FLDATA
      XSQ(I)=XSQ(I)+XX*XX
      YSQ(I)=YSQ(I)+FLDATA*FLDATA
      XX=XX-FLDATA
      X3=ABS(XX) 
      IF(X3.GT.ABS(RS(I,1))) THEN
        RS(I,1)=XX
        ZRS(I,1)=DEPTH
	ELSE IF(X3.LT.ABS(RS(I,2))) THEN
        RS(I,2)=XX
        ZRS(I,2)=DEPTH
      ENDIF
      RS(I,3)=0.5*(RS(I,1)+RS(I,2))
      IF(X3.GT.ABS(RMS(I))) THEN
        RMS(I)=XX
        MTHRMS(I)=MONTH
        MDAYRMS(I)=MDAY
        ZRMS(I)=DEPTH
      ENDIF
      IFLAG=IFLAG+1
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE THICKNS(MBOT)
C*****
C***** Compute thickness of each layer from the depth 
C***** area curve in LAKE.
C*****
      REAL*8 A,V,TV,ATOP,AVOL
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL

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

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE TVOL(MBOT)
C***** 
C***** Determine the volume of water above a layer
C*****
      REAL*8 A,V,TV,ATOP,SUM
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL

      SUM=0.
      DO 100 I=1,MBOT
        SUM=SUM+V(I)
        TV(I)=SUM
 100  CONTINUE
      RETURN
      END

C*****************************************************C
C
C
C*****************************************************C
      SUBROUTINE WDEPTH(ST,Q,LW)
C*****
C***** Determine layers contibuting to outflow.  Outflow from
C***** a layer is proportional to the thickness of the layer.
C***** Compute and send outflow concentrations to the outflow
C***** data file  (tape8.OUT).
      REAL*8 A,V,TV,ATOP,RHO
      COMMON/CHANEL/WCHANL,ELCB,ALPHA,BW,WLAKE
      COMMON/RESULT/ VAR(40,21)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,DY
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
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
C*********** COMPUTATIONS FOR OUTFLOW FILE ***********
      IF(IPRNT(2).GT.0) THEN
C************ CONVERT OUTFLOW VOLUME TO CFS **********
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

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE WINEN(T,V,WIND)
C****
C...CALCULATION OF THE SHEAR VELOCITY AND THE WIND SHEAR STRESS
      REAL*8 FAO

C...CONVERSION OF WIND SPEED FROM M.P.H. TO M/S
C...DENSITY OF WATER AND AIR ASSUMED TO BE 1000 AND 1.177 KG/M3
C...
      W=WIND*0.447
      FAO=0.0
      CALL LAKE(FTCH,FAO,0,2)
      ZB=ALOG(FTCH)*0.8-1.0718
      W=W*1.666667*(ZB+4.6052)/(ZB+9.2103)

C...CALCULATE THE WIND COEFFICIENT FROM WU (1971)
C...SOLVE Cz BY GAUSS-NEWTON METHOD
C       CZ=SQRT(WIND*0.447)*0.00036
C       FS=(WIND*0.447)**2.0/(9.81*10.0)
C 10    FV=CZ**(-0.5)+2.5*ALOG(0.011*CZ*FS)
C       DFV=-0.5*CZ**(-1.5)+2.5/CZ
C       CZA=CZ-FV/DFV
C       IF(CZA.LT.0.0) CZA=0.000001
C       PRE=ABS(CZA-CZ)/CZ
C       IF(PRE.LE.0.001) GOTO 40
C       CZ=CZA
C       GOTO 10
C 40    CZ=CZA

C...CALCULATION OF WIND SHEAR STRESS
      CZ=SQRT(W)*0.0005
      IF(W.GE.15.) CZ=0.0026
      T=1.177*CZ*W*W
C...ASSIGNMENT OF CHARACTERISTIC SURFACE VELOCITY
C...USING CALCULATION OF SHEAR VELOCITIES
      V=0.0343*SQRT(CZ)*W
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE WINMIX(ENG,TS,TSAVE,ILAY,MBOT,VC)
C...
C...CALCULATE THE AMOUNT OF ENTRAINMENT RESULTING FROM WIND MIXING.
C...USE THE DEPTH OF CENTER OF MASS OF MIXED LAYER TO DETERMINE THE 
C...POTENTIAL ENERGY THAT MUST BE OVERCOME BY THE KINECTIC ENERGY 
C...OF THE WIND FOR ENTRAINMENT TO OCCUR.
C...
      REAL*8 A,V,TV,ATOP,RHO
      COMMON/TEFX/T2K(40),TEHE(40),BMK(40),OLDHQ,ITERF 
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,PLT(30)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      DIMENSION TSAVE(40)
      
      IF(ILAY.GE.MBOT) THEN
       DO 55 I=1,MBOT
        TSAVE(I)=TS
 55    CONTINUE       
       GOTO 35
      ENDIF 
       
CFX  --  IF MIT>0, THE PROGRAM WORKS AS M.I.T. MODEL,
CFX  --  OTHERWISE MINLAKE MODEL FOR WIND-MIXING ALOGORITHM
      VC=VC
      MIT=-66
      TPE=0.0
CFX  --  MODEL EXCLUDES CONMIX SUBROUTINE FROM MIT (HARLEMAN, 1977),
CFX  --  WIND MIXING STARTS FROM THE FIRST LAYER EACH DAY!!
     
      SUM1=0.0
      SUM2=0.0
      I=1
      RV=RHO(T2(1),C2(1),CD2(1))*V(1)
      SUM1=SUM1+RV
      SUM2=SUM2+RV*Z(1)
      TSAVE(I)=TS

C -- FUNCTION OF RICHARDSON NUMBER FOR ENERGY RATIO 
C -- d(PE)/d(KIN) = F(Ri)
       FRI=1.0
       
 20   DCM=SUM2/SUM1
      TSTEP=T2(I+1)
      DENH=RHO(TSTEP,C2(I+1),CD2(I+1))
      DENL=RHO(TS,C2(I),CD2(I))
            
      IF(MIT.LT.0) THEN
C...CALCULATION OF POTENTIAL ENERGY OF MIXED LAYER -- MINLAKE,1986
        TPE=9.81*TV(I)*(Z(I)+DZ(I)/2.0-DCM)*(DENH-DENL)
 
      ELSE
CFX -- MIT ORIGINAL MODEL (HARLEMEN AND OTHERS, 1977)      
        TPE=TPE+9.81*DZ(I+1)*A(I+1)*(DENH-DENL)*Z(I+1)/2.0
       
CFX -- MIT MODEL (BLOSS AND HARLEMAN, 1979) 
        IF(I.GT.1) THEN
          ENG=(1.0-FRI)*ENG*A(1)/A(I-1)
          ENG=ENG*A(1)/A(I)
        ENDIF
      
        RI=9.81*ABS(DENH-DENL)*(Z(I)+DZ(I)/2.0)/(DENL*VC**2.0)
        FRI=0.057*RI*((29.5-SQRT(RI))/(14.2+RI)) 
        FRI=AMIN1(FRI,1.0)
        FRI=AMAX1(FRI,0.0)
      ENDIF

CMIKI- IMBERGER
C..... CALL ENERGY(VC,TS,ILAY,HS,ENG,PE,HE,HA,HBR,HC,RI)

C...CRITERIA FOR ENTRAINMENT (UNIVERSAL ALOGRITHM -- FANG, 1993)
      IF((FRI*ENG).LT.TPE) GOTO 40
      
C...ENTRAINMENT OF LAYER I+1
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
      DO 50 K=1,ILAY
        T2(K)=TS
 50   CONTINUE
 
CFX
C       DO 8168 KK=1,NDAYS
C       IF(MDAY+MONTH*100.EQ.NPRNT(KK)) THEN
C
C        WRITE(46,705) MONTH,MDAY
C 705    FORMAT(5X/5X/I3,2X,I5/5X/)
C
C...T2K -- PREVIOUS DAY WATER TEMPERATURE -- T2(i-1)
C...TEHE -- TEMPERATURE AFTER SOLVING HEAT TRANSPORT EQUATION
C...BMK -- DIFFUSION COEFFICIENT = K1*K2/(K1+K2)
C
C        WRITE(46,735) (Z(K),T2K(K),TEHE(K)
C     +   ,T2(K),BMK(K),K=1,MBOT) 
C 735    FORMAT(30(4(F9.5,3X),F10.5/))        
C       
C       ENDIF
C 8168  CONTINUE
 
 740  RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE VOLUME(MBOT)
C*****
C***** Compute the volume of each layer based on the depth-volume 
C***** relationship found in LAKE.
C*****
      REAL*8 A,V,TV,ATOP,VDUM,VZ
      COMMON/VOL/ ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL

      AZZ=0.
      CALL LAKE(ZMAX,VDUM,0,3)
      VZ=VDUM
      DO 100 I=1,MBOT-1
        AZZ=DZ(I)+AZZ
        Z2=ZMAX-AZZ
        CALL LAKE(Z2,VDUM,0,3)
        V(I)= VZ-VDUM
 100    VZ=VDUM
      V(MBOT)=VZ
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE FDATA(NF,NPRFLE)
C***** 
C***** Subroutine to read field data from the input data
C***** and compute statistics and deviations between field
C***** data and simulation
C*****
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
CFX
      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)      
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/STAT/SUMXY(10),SUMX(10),SUMY(10),XSQ(10),YSQ(10),RSQ(10),
     + RMS(10),RS(10,3),MTHRMS(10),MDAYRMS(10),ZRS(10,2),ZRMS(10) 
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/FIELD/ IFLAG(10),FLDATA(10,50),DEPTH(50),NFLD(10),SD
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
      COMMON/YROUND/ NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,
     + HKMXSM,WCFSM,WSSM,WCFSF,WSSF
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      DIMENSION COMP(40,10),OUTD(2,2)     
      EQUIVALENCE (T2(1),COMP(1,1))
      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1

      DO I=1,10
	  DO J=1,3
          RS(I,J)=0.0
	  ENDDO
	  RS(I,2)=100.0
      ENDDO

      DO I=1,10
        DO J=1,50
          FLDATA(I,J)=0.0
	  ENDDO
      ENDDO

      DO I=1,50
        DEPTH(I)=0.0
      ENDDO
 
      READ(7,*) NF,NPRFLE
c      write(*,*)"NF =", NF, "   NPRFLE =", NPRFLE
      NDAYS=NDAYS+1
      IF(NF.GT.0) THEN
      
CFX  NEW METHOD FOR CHANGING THE XK1, ON JUNE 30, 1992
C*** EMCOE(3) < 0.0 -- IT IS NONFUNCTIONAL PART !!!
        IF(EMCOE(3).GT.0.0) THEN
          READ (7,*) SD
          TK=EMCOE(3)/SD
          XK1=TK-XK2*CHLATOT(1)
        ENDIF 

        READ (7,*) (NFLD(I),I=1,NPRFLE)
        READ (7,*) (DEPTH(I),I=1,NF)
        DO 305 I=1,NPRFLE
          READ(7,*) (FLDATA(NFLD(I),J),J=1,NF)
 305    CONTINUE

C.... Output measured- & simulated temperature data
        WRITE(150,1502) MONTH,MDAY,MYEAR,MBOT,NF
        WRITE(150,1503) (Z(I),I=1,MBOT)
        WRITE(150,1503) (T2(I),I=1,MBOT)
        WRITE(150,1503) (DEPTH(I),I=1,NF)
        WRITE(150,1503) (FLDATA(1,J),J=1,NF)
C.... Output measured- & simulated dissolved oxygen data
        WRITE(151,1502) MONTH,MDAY,MYEAR,MBOT,NF        
        WRITE(151,1503) (Z(I),I=1,MBOT)
        WRITE(151,1503) (DSO2(I),I=1,MBOT)
        WRITE(151,1503) (DEPTH(I),I=1,NF)
        WRITE(151,1503) (FLDATA(6,J),J=1,NF)
 1502 FORMAT(1X,I2,I4,I6,2I4)
 1503 FORMAT(1X,10F8.3)                       

C...Locate simulation values corresponding to sampled
C...constituents and depth of field data
C...OUTD(1,*) is field data, outd(2,*) is model predictions
c...OUTD(*,1) is water temperature, OUTD(*,2) is Dissolved Oxygen.
        LL=1
        DO 310 KK=1,NF
          OUTD(1,1)=FLDATA(1,KK)
          OUTD(1,2)=FLDATA(6,KK)
          L=LL
          DO 315 LL=L,MBOT
            IF(Z(LL).LT.DEPTH(KK)) GOTO 315
            IF(LL.EQ.1) THEN
              DO 320 I2=1,NPRFLE
                 I=NFLD(I2)
                 XX=COMP(LL,I)
                 IF(I.EQ.1) THEN
                   OUTD(2,1)=XX
                 ELSE
                   OUTD(2,2)=XX
                 ENDIF
                 CALL STATS(FLDATA(I,KK),XX,IFLAG(I),DEPTH(KK),I)
 320          CONTINUE
            ELSE
              DO 330 I2=1,NPRFLE
                 I=NFLD(I2)
                 DZ1=DEPTH(KK)-Z(LL-1)
                 DZ2=Z(LL)-Z(LL-1)
                 XX=COMP(LL-1,I)+DZ1/DZ2*(COMP(LL,I)-COMP(LL-1,I))
                 IF(I.EQ.1) THEN
                   OUTD(2,1)=XX
                 ELSE
                   OUTD(2,2)=XX
                 ENDIF
                 CALL STATS(FLDATA(I,KK),XX,IFLAG(I),DEPTH(KK),I)
 330          CONTINUE
            ENDIF
            GOTO 380
 315      CONTINUE

CFX...  OUTPUT DATA FOR PLOT FIGURES (1994)
 380    MFLAG=0
        IF(THICE.GT.0.0) MFLAG=10

C20**Measured, simulated TEMPerature and measured, simulated DO
C20**When ICE>0, it sets MFLAG=10         
        WRITE(66,615) OUTD(1,1),OUTD(2,1),OUTD(1,2),OUTD(2,2),
     +   DEPTH(KK),MFLAG
 615    FORMAT(1X,5(F6.2,2X),2X,I2)
        
 310    CONTINUE

C... Store statisitcal results in the consol and tape8.DAT
Clj..        IF(IPRNT(4).EQ.0) THEN
Clj..          DO I=1,3
Clj..            RS(6,I)=0.0
Clj..            ZRS(6,I)=0.0
Clj..	         ENDDO
Clj..        ENDIF 

        WRITE(98,3008) MONTH,MDAY,MYEAR,ZRS(1,1),RS(1,1),ZRS(1,2),
     +                 RS(1,2),RS(1,3)
        IF(IPRNT(4).EQ.1) THEN
          WRITE(98,3009) MONTH,MDAY,MYEAR,ZRS(6,1),RS(6,1),ZRS(6,2),
     +                   RS(6,2),RS(6,3)
	  ENDIF
 3008 FORMAT(3X,I2,4X,I2,2X,I4,4X,'TEMP',5(4X,F8.3)) 
 3009 FORMAT(3X,I2,4X,I2,2X,I4,4X,'DO  ',5(4X,F8.3))
        
C...Store data on plot file (tape8.PLT)
        IF(IPRNT(5).GT.0) THEN
          WRITE(1,REC=IREC) REAL(NF)
          IREC=IREC+1
          WRITE(1,REC=IREC) REAL(NPRFLE)
          IREC=IREC+1
          DO 500 I=1,NF
            WRITE(1,REC=IREC) DEPTH(I)
            IREC=IREC+1
 500      CONTINUE
          DO 501 I=1,NPRFLE
            WRITE(1,REC=IREC) REAL(NFLD(I))
            IREC=IREC+1
 501      CONTINUE
          DO 502 I2=1,NPRFLE
            DO 502 I=1,NF
              WRITE(1,REC=IREC) FLDATA(NFLD(I2),I)
 502          IREC=IREC+1
 
          IF(NFLD(1).NE.1) THEN
            X=0.0
          ELSE
C...Mixed layer depth in field data taken at dT/dZ=1.0
            DO 503 J2=2,NF
              X=(FLDATA(1,J2-1)-FLDATA(1,J2))/
     +                                  (DEPTH(J2)-DEPTH(J2-1))
              IF(X.GT.1.0) GOTO 504
 503        CONTINUE
 504        X=(DEPTH(J2)+DEPTH(J2-1))*0.5
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
 2999 FORMAT(1X,I4,3X,I5)


      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE FPLOT(MYEAR,DMIX,SECCHI,CHLMEAN)
C*****
C***** Locate simulation values at same depth as field 
C***** data and send both to the plot data file (tape8.PLT)
C*****
      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
      COMMON/ZOOPL/IZ,MINDAY,MAXDAY,ZP,ZPMIN,PRMIN,PRMAX,PREDMIN,XIMIN,
     + XIMAX,XKRZP,GRAZMAX(3),THGRAZ(3),ASM,THRZP,HSCGRAZ(3),CHLAMIN(3),
     + REPRO,XI,XKMZ,GRAZE(3,40)
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,PLT(30)
      DIMENSION VAR(40,13)
      EQUIVALENCE (T2(1),VAR(1,1))
CM
      MYEAR=MYEAR+1-1
CM      
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
C************ IF REQUESTED PLOT DEPTH > Z(MBOT) USE
C              DUMMY VALUE OF -1 ******************
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
      WRITE(1,REC=IREC) CHLMEAN
      IREC=IREC+1
      X=ZP/TV(MBOT)*ZMAX
      WRITE(1,REC=IREC) X
      IREC=IREC+1
      WRITE(1,REC=IREC) DMIX
      IREC=IREC+1
      WRITE(1,REC=IREC) SECCHI
      IREC=IREC+1
      IF(MDAY+MONTH*100.NE.NPRNT(NDAYS)) THEN
        NF=0
        WRITE(1,REC=IREC) REAL(NF)
        IREC=IREC+1
      ENDIF
      RETURN
      END

C*****************************************************C
C
C
C*****************************************************C
      SUBROUTINE SOLVET2(VAR2,NBOT)
C*****
C***** Tri-diagonal matrix solving routine
C*****
      REAL*8 AK,BK,CK,DK,TT,TX(60)
      COMMON/SOLV/ AK(60),BK(60),CK(60),DK(60)
      DIMENSION VAR2(60)

      DO 60 I=2,NBOT
        TT=AK(I)/BK(I-1)
        BK(I)=BK(I)-CK(I-1)*TT
 60     DK(I)=DK(I)-DK(I-1)*TT
C**********BACK SUBSTITUTION**************
      TX(NBOT)=DK(NBOT)/BK(NBOT)
      DO 70 I=1,NBOT-1
        J=NBOT-I
 70     TX(J)=(DK(J)-CK(J)*TX(J+1))/BK(J)
      DO 80 I=1,NBOT
 80     VAR2(I)=SNGL(TX(I))
      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE SUBLAY(IEUPH,NSLAY)
C.....
C.....SUBDIVIDE EACH LAYER IN EUPHOTIC ZONE INTO LAYERS OF 0.2 M OR LESS
C.....FOR COMPUTING LIGHT LIMITATION ON GROWTH.
C.....
      REAL*8 A,V,TV,ATOP,VDUM,TOV,SVOL
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/SUB/SDZ(90),SZ(90),LAY(40),AVGI(24,90),SVOL(90)

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

C.....ESTABLISH ARRAY OF DISTANCES OF EACH SUBLAYER BELOW WATER SURFACE
      SUMSZ=0.
      DO 300 J=1,NSLAY
        SZ(J)=SUMSZ+SDZ(J)/2.0
        SUMSZ=SUMSZ+SDZ(J)
 300  CONTINUE
C.....ESTABLISH ARRY OF VOLUME OF EACH LAYER.....
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
C****************************************************C
C
C
C****************************************************C
      SUBROUTINE CONSMAS(IW,IPL,DCF)
C*****
C***** Compute the final concentration in a layer using the
C***** concentration in the density current and the
C***** concentration in the layer weighted by the volume
C***** in the layer and the volume in the density current
C*****
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/INFLOW/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),
     + CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)

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
C****************************************************C
C
C
C****************************************************C
      SUBROUTINE DCFLOW(IPL,S,FT,WIDTH,IW)
C*****
C***** Determine the density current volume and concentration
C***** with entrainment from each layer passed and the
C***** isopycnic layer that receives the density current.
C*****
      REAL*8 A,V,TV,ATOP,RHO
      INTEGER FMON,FDAY,FYEAR
      COMMON/FILE/ DIN,MET,FLO,TAPE8,TAPE1,IREC
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/FLOW/HMK(41),QE(40),FVCHLA(5),PE(5,41)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/INFLOW/QIN(5),TIN(5),PAIN(5),BODIN(5),DOIN(5),CIN(5),
     + CDIN(5),XNHIN(5),XNOIN(5),CHLAIN(3,5)
      CHARACTER*16 DIN,MET,FLO,TAPE8,TAPE1

      X=QIN(IW)
      RHOMIX=RHO(T2(1),C2(1),CD2(1))
      RHOIN=RHO(TIN(IW),CIN(IW),CDIN(IW))
      CALL PDEPTH(RHOMIX,RHOIN,QIN(IW),S,IHP,QENIN,MBOT,SUMZ,
     +FT,WIDTH,HP)
      DO 100 I=1,MBOT-1
C...COMPUTE THE DENSITY OF INFLOW AS IT ENTERS EACH LAYER
        RDC=RHO(TIN(IW),CIN(IW),CDIN(IW))
        RHOAMB=RHO(T2(I),C2(I),CD2(I))
C...LOCATE THE ISOPYCNIC LAYER
        IF(RDC.GT.RHOAMB) GO TO 5
        IPL=I-1
        IF(IPL.LT.1) IPL=1
        GO TO 3
C...CONTINUE THE SEARCH
C...COMPUTE THE ENTRAINMENT  FROM EACH LAYER
 5      QE(I)=ENTRAIN(I,QIN(IW),RDC,RHOAMB,DZ(I),S,SUMZ,QENIN,IHP,
     +  WIDTH,FT)
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
      IF(IPRNT(1).EQ.1) WRITE(8,2000) Z(IPL),V(IPL),X,QIN(IW),PAIN(IW),
     +XNOIN(IW),XNHIN(IW),DOIN(IW),BODIN(IW)
 2000 FORMAT(7X,F5.2,7X,E11.5,4X,E11.5,3X,E11.5,2X,3(F6.3,3X),
     +2(F6.2,3X))
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE LIGHT(RAD,TD,C2,ALBEDO)
C*****
C***** Compute light distribution over a day in TD subsections.
C***** TD is photo period (hours).  The distribution is assumed
C***** to be sine function starting from sunrise.
C***** The light subsections are used in AVELITE.
C*****
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP

C.....CALCULATION OF LIGHT ABSORBED IN SNOW AND ICE COVERS
C.....FOR YEAR-ROUND SIMULATION MODEL (FANG, 1994)

      IF(THSNOW.GT.0.0) THEN
        RDSNOW=(1.0-BTSNOW)*(1.0-ALFSNOW)*RAD*EXP(-GMSNOW*THSNOW)
      ENDIF
      IF(THSNOW.EQ.0.0) RDSNOW=RAD
 
      IF(THICE.GT.0.0) THEN
        RDICE=(1.0-BTICE)*(1.0-ALFICE)*RDSNOW*EXP(-GMICE*THICE)
      ENDIF
      IF(THICE.EQ.0.0) RDICE=RAD
      RAD=RDICE

C PI IS CONSTANT, ALBEDO IS LIGHT REFLECTION AND XIAVE
C IS THE AVERAGE PAR FROM SOLAR RADIATION DATA
C XIAVE - uEINSTEIN/M**2/SECOND

      PI=3.1415927
      ALBEDO=0.087-0.0000676*RAD+0.11*(1.0-EXP(-0.01*C2))
      XIAVE=RAD*27.25/TD   
      ITD=INT(TD+0.5)

      DO 101 J=1,ITD

C - USING STEP FUNCTION FOR I-AVERAGE
C        XIT=XIAVE*PI/2.0*SIN(J*PI/FLOAT(ITD))

C - USING LINEAR AVERAGE FOR I-AVERAGE
C        XIT=XIAVE*PI/2.0*(SIN(J*PI/ITD)+SIN((J-1)*PI/ITD))/2.0

C - USING COMPLETE INTEGRATION RESULTS TO GET I-AVERAGE
C - THERE IS NO SIGNIFICANT DIFFERENCE FOR PRODUCTION CALCULATION
C - PROVE IT ON NOVEMBER 27, 1992 (FANG)
        XIT=XIAVE*TD/2.0*(-COS(J*PI/ITD)+COS((J-1)*PI/ITD))
        PARI0(J)=XIT*(1.0-ALBEDO)
 101    CONTINUE       
      RETURN
      END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE AVGLITE(IEUPH,TD)
C*****
C***** Compute average light in each sublayer for TD different
C***** time segments in a day. (follows work by J. Cardoni)
C***** TD is photo period in hours.
C*****
      REAL*8 A,V,TV,ATOP,SVOL
      COMMON/TEMP/PARI0(24),PCDUM(3,40),XNHD(40),XNOD(40),
     + CHLADUM(3,40),XNCD(3,40),PADUM(40),SID(40)
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/SOURCE/RM(3,40),PROD(40),XMR(3,40),PRODSUM(40)
      COMMON/SUB/SDZ(90),SZ(90),LAY(40),AVGI(24,90),SVOL(90)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
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
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
       SUBROUTINE PRODAVG(IEUPH,MBOT,KK,TD)
C*****
C***** Compute light limitation coefficient on algal growth
C***** in each sublayer for TD light periods determined in
C***** AVELITE.  (Follows work by J. Cardoni with
C***** limitation/inihibition function from Megard).
C*****
       REAL*8 A,V,TV,ATOP,SVOL
       COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)      
       COMMON/SUB/SDZ(90),SZ(90),LAY(40),AVGI(24,90),SVOL(90)
       COMMON/SOURCE/RM(3,40),PROD(40),XMR(3,40),PRODSUM(40)
       COMMON/PHYTO/PDEL(3),PMAX(3),PMIN(3),THR(3),THM(3),XKR1(3),
     + XKR2(3),XKM(3),HSCPA(3),HSC1(3),HSC2(3),UPMAX(3),THUP(3),
     + GROMAX(3),TMAX(3),TOPT(3),XNMAX(3),XNMIN(3),UNMAX(3),THUN(3),
     + HSCN(3),HSCNH(3),XNDEL(3),IDIATOM,CHLMEAN,CHLMAX,SECCHI
       COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
       COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
       COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR
       DIMENSION PSUB(90)

C****  K1 AND K2 are in uEinst m**-2 sec (On June 24, 1992)
C****  Transfer coefficient is 10**6/3600 = 1/0.0036           
C****  K1 - HSC1(1), K2 - HSC2(1) -- STEFAN & FANG (1993)
C****  PO2 is photosynthetic oxygen production at sz(J) over one hour
C****  PSUB(J) is photosynthetic oxugen production at sz(J) over a day 
C****  SUMPV is volumetric averaged oxygen production at layer I from 
C****  the surface layer to layer IEUPH in mg O2/L/day.

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
             PO2=CPMAX*AVGI(K,JJ)*XCOE/(AVGI(K,JJ)+HSC1(KK)
     +         +AVGI(K,JJ)**2.0/HSC2(KK))
  400        PSUB(JJ)=PSUB(JJ)+PO2
  300      SUMPV=SUMPV+PSUB(JJ)*SVOL(JJ)
  200  PRODSUM(I)=SUMPV/V(I)
       IF(IEUPH.LT.MBOT) THEN
        DO 500 I=IEUPH+1,MBOT
  500   PRODSUM(I)=0.0
       ENDIF
       RETURN
       END
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE PTOTALS(MAXMTH,MXDAY,CHLMAX,CHLMEAN)
C*****
C***** PTOTALS for year-round simulation model (1994) is to estimate
C***** or specify Total chlorophyll-a concentration in mg/l.
C***** It is different from PTOTALS in Riley's Program !!
C*****
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
	COMMON/CHLAP/NCDAY(40,2),GCHLA(40,2),ICHLA

      COMMON/NEW/NYEAR,KYEAR(25),FDTH(5),NDEPTH,NTDY(25)
      COMMON/YROUND/NYTOT,NMFIN,MYEAR,HKMXIS,WCFIS,WSIS,
     + HKMXSM,WCFSM,WSSM,WCFSF,WSSF
      COMMON/DOCOE/EMCOE(6),CHLEP(140),CHLHY(140),POMAX,IDNUM(6)
      COMMON/RESULT/T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     +DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     +PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/CHOICE/MODEL,NITRO,IPRNT(6),NDAYS,NPRNT(140),NCLASS,
     + PLOT(30)
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/SNICE/THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP

	COMMON/FIELD/ IFLAG(10),FLDATA(10,50),DEPTH(50),NFLD(10),SD

C20** If we have field data
      IF (IFIELD.EQ.1) THEN
	 
C***** ESTIMATE CHLOROPHYLL CONCENTRATION  
C***** CHLEP(1) is the initial data in input files, i.e. Chla=16mg/l
C***** for eutrophic lakes. CHLEP(2) will be the first field data in
C***** the input data file. Please check "START" in lake2.for.
C***** CHLBOT is the chlorophyll-a concentration in the bottom layer.

      KDY=MONTH*100+MDAY 
      KI=NDAYS 
      
      IF(MYEAR.LT.KYEAR(1)) THEN
       CHLATOT(1)=CHLEP(1)
       CHLBOT=CHLEP(1)
       GOTO 700
      ENDIF 
         
C***** The chlorophyll values will be the mean value of nearest field
C***** data, except after the last field data point.  IDNUM(2)>0
      IF(MYEAR.EQ.KYEAR(1)) THEN
        MFIRST=1
        MLAST=NTDY(1)
      ENDIF

      IF(KDY.EQ.101) THEN
        IF(MYEAR.NE.KYEAR(NYEAR)) THEN
          MFIRST=KI-NTDY(NYEAR)
          MLAST=MFIRST+NTDY(NYEAR)-1
          NDAYS=KI-NTDY(NYEAR)
        ELSE
          MFIRST=KI
          MLAST=MFIRST+NTDY(NYEAR)-1
        ENDIF
      ENDIF

      IF(THICE.GT.0.0) THEN
        CHLATOT(1)=CHLEP(1)/2.0
        CHLBOT=CHLHY(1)/2.0
        GOTO 656
      ENDIF   

      IF(KDY.LE.NPRNT(MFIRST).AND.THICE.EQ.0.0) THEN
        CHLATOT(1)=CHLEP(MFIRST+1)
        CHLBOT=CHLHY(MFIRST+1)
        GOTO 656
      ENDIF  

      IF(KDY.GT.NPRNT(MLAST).AND.THICE.EQ.0.0) THEN
        CHLATOT(1)=CHLEP(MLAST+1)
        CHLBOT=CHLHY(MLAST+1)
        GOTO 656
      ENDIF  
       
      IF(KDY.LE.NPRNT(KI).AND.KDY.GT.NPRNT(KI-1)) THEN
        CHLATOT(1)=(CHLEP(KI)+CHLEP(KI+1))/2.0
        CHLBOT=(CHLHY(KI)+CHLHY(KI+1))/2.0
        GOTO 656
      ENDIF
         
 656  CONTINUE

      IF(MYEAR.NE.KYEAR(NYEAR).AND.KDY.EQ.NPRNT(KI)) NDAYS=NDAYS+1

 700  DO 108 I=2,MBOT
        CHLATOT(I)=CHLATOT(1)
        IF(Z(I).GT.Z(ILAY)) THEN
          CHLATOT(I)=CHLBOT            
        ENDIF
 108  CONTINUE

C20** If NO field data, we use a seasonal pattern !
      ELSE

C***** ESTIMATE CHLOROPHYLL CONCENTRATION  
C***** CHLBOT is the chlorophyll-a concentration in the bottom layer.

      KDY=MONTH*100+MDAY 

C For the first year only !
      IF(KDY.EQ.416.AND.KFIRST.LT.10) THEN
       IST=15
       JZ=106
       KFIRST=100
      ENDIF

      IF(KDY.EQ.101) THEN
       IST=1
       JZ=1
      ELSE
       JZ=JZ+1
      ENDIF

c
C Secchi Depth for eutrophic, mesotrophic, and oligotrophic lakes
C are SD = 1.2, 2.5, 4.5 m, respectively.
C For mesotrphic it is a different ratio for chl-a with same pattern
C
      IF(SD.LT.3.0) THEN
       M=1
C      CHMEAN=15.0/1000.0
C      IF(SD.GT.2.0) CHMEAN=6.0/1000.0
      ELSE
       M=2
C      CHMEAN=2.0/1000.0
      ENDIF
	 
C
C Step function for Chla concentration
C
      IF(JZ.LE.NCDAY(IST+1,M)) THEN
       DDY=JZ-NCDAY(IST,M)
       DDT=NCDAY(IST+1,M)-NCDAY(IST,M)
       DCH=GCHLA(IST+1,M)-GCHLA(IST,M)
       RATIO=(GCHLA(IST,M)+DCH*DDY/DDT)/100.0
       CHLATOT(1)=(1.0+RATIO)*CHMEAN
      ENDIF
	
      IF(JZ.EQ.NCDAY(IST+1,M)) IST=IST+1
	
	DO 118 I=2,MBOT
        CHLATOT(I)=CHLATOT(1)
 118  CONTINUE
      
	ENDIF
	
C***  Determine mximum chlorophyll-a concentration
      IF(CHLATOT(1).GT.CHLMAX) THEN
        CHLMAX=CHLMEAN
        MXDAY=MDAY
        MAXMTH=MONTH
      ENDIF

      RETURN
      END

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
      SUBROUTINE EUPHZ(ZEUPH,IEUPH,ALBEDO)
C*****
C***** Determine the limit of the euphotic zone given as
C***** the layer in which the light intensity is less than
C***** one percnet of the surface value.  
C*****
      REAL*8 A,V,TV,ATOP
      INTEGER FMON,FDAY,FYEAR
      COMMON/SNICE/ THICE,THSNOW,BTICE,ALFICE,GMICE,
     + BTSNOW,ALFSNOW,GMSNOW,QWATER,HTBTM,HSTMP
      COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
      COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
      COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
      COMMON/WATER/BETA,EMISS,XK1,XK2,HKMAX,WCOEF,WSTR

C***  It is necessary to further test for winter assumption !
      IF(THICE.GT.0.0) THEN
        AVGCHL=(CHLATOT(1)+CHLATOT(ILAY+1))/2.0
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
           
C     EK=XK1+XK2*CHLATOT(1)+0.043*C2(1)
C     EMIX=EXP(-EK*Z(ILAY))         
C     IF(EMIX.GT.0.01) THEN
C       EK=XK1+XK2*CHLATOT(ILAY+1)+0.043*C2(ILAY+1)
C       ZEUPH=Z(ILAY)+ALOG(0.01/EMIX)/(-EK)
C     ELSE
C       ZEUPH=ALOG(0.01)/(-EK)
C     ENDIF           
C     IF(ZEUPH.GT.Z(MBOT)) ZEUPH=Z(MBOT)
C**** CONVERT ZEUPH TO DEPTH IN SOME LAYER
C     DO 138 I=1,MBOT
C       IF(Z(I).GE.ZEUPH) THEN
C         ZEUPH=Z(I)
C         IEUPH=I
C         GOTO 139
C       ENDIF
C 138 CONTINUE

C***  Why did Riley need ALBEDO ? (Fang, 1994)
      EK=XK1+0.043*C2(1)+XK2*CHLATOT(1)
      EX=(1.0-ALBEDO)*EXP(-EK*(Z(1)+DZ(1)/2.0))
      EX=EXP(-EK*(Z(1)+DZ(1)/2.0))
      IF(EX.GT.0.01) GO TO 2
      ZEUPH=Z(1)+DZ(1)*0.5
      IEUPH=1
      GOTO 139
 2    DO 100 I=2,MBOT
C.....ATTENUATION COEFF. (EK).....
      EK=XK1+0.043*C2(I)+XK2*CHLATOT(I)
C.....DETERMINATION OF EUPHOTIC DEPTH (ZEUPH) .....
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

C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
       SUBROUTINE PREICE(QNEG,TBOUN,NP)
C**** Abstract information from water temperature profile before
C**** the ice formation on the lake surface

       REAL*8 A,V,TV,ATOP
       INTEGER FMON,FDAY,FYEAR
       COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
       COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
       COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
 
C... TESTNG NATURAL CONVECTIVE MIXING
        QNEG=0.0

C... Determine maximum temperature Tmax in the profile
C... Determine how many layers NP have negative temperature     
       IF(T2(1).LT.4.0) THEN
        TEMAX=0.0
        NP=0
        DO 25 I=1,MBOT
         IF(TEMAX.LT.T2(I)) TEMAX=T2(I)
         IF(T2(I).LT.0.0.AND.T2(I+1).GT.0.0) NP=I
 25     CONTINUE

C... Determine volumetric average Tmean for positive temperature 
        
         JJ=NP+1
         TVDUM=T2(JJ)*V(JJ)
         VDUM=V(JJ)

         DO 410 MK=JJ,MBOT-1
           TVDUM=TVDUM+T2(MK+1)*V(MK+1)
           VDUM=VDUM+V(MK+1)
 410     CONTINUE
         TMEAN=TVDUM/VDUM
       
C... Force convective mixing !!!       
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
       
         IF(TMEAN.LE.4.0.AND.NP.NE.0) THEN
       
C...  TBOUN = 0.0 OR 4.0 is a boundary (Critiical) condition !
          TBOUN=0.0
               
C.... QEXTR is in kcal/day
C.... QNEG is extra heat loss in kcal/(m*m) !

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
C************************************************************C
C                                                            C
C                                                            C
C************************************************************C
       SUBROUTINE POSTMIX(TBOUN,THICE,QNEG,TECON,TSAVE,RKE,NP)
C**** Determine the mixed layer depth for water temperature 
C**** profile just before the ice formation   
       REAL*8 A,V,TV,ATOP,RHO
       INTEGER FMON,FDAY,FYEAR
       COMMON/VOL/ZMAX,DZ(40),Z(40),A(40),V(40),TV(40),ATOP(41),DBL
       COMMON/RESULT/ T2(40),CHLATOT(40),PA2(40),PTSUM(40),BOD2(40),
     + DSO2(40),C2(40),CD2(40),XNO2(40),XNH2(40),CHLA2(3,40),
     + PC2(3,40),XNC2(3,40),T20(40),SI2(40)
       COMMON/STEPS/DZLL,DZUL,MBOT,NM,NPRINT,MDAY,MONTH,ILAY,JDY,JUDF,
     +FMON,FDAY,FYEAR
        DIMENSION TECON(40),TSAVE(40),DTE(40)
        DIMENSION TNEW(40),TPEL(40)

            IF(THICE.GT.0.0) RETURN
            IF(QNEG.LE.0.0)  RETURN
                                    
C...  HAVING ICE FORMATION IF THE WIND CAN NOT MIX DOWN
C...  TO ILAY, THEREFOR T2(1) = TBOUN
            ICHECK=-100
            
            IF(T2(1).EQ.TBOUN) THEN
             QICE=QNEG
             THICE=QICE/(1000.0*80.0*ATOP(1))
             QICE=0.0
             DO 475 II=1,ILAY
               T2(II)=TBOUN
 475         CONTINUE
             GOTO 409
            ENDIF

C... T2(1) CAN NOT LESS THAN TBOUN !!
            IF(T2(1).GT.TBOUN) THEN
            
C...  CALCULATE TEMPERATURE DROP BY QNEG
C...  100.0 IS WATER DENSITY IN KG/M**3
C...  1.0 IS Cp, Heat capacity in Kcal/kg-C
C...  TSAVE is the mixed layer temperature at each depth.
           
C             WRITE(99,805)
  805         FORMAT(1X/1X,'LAYER',6X,'DTE(i)',3X,'TNEW(i)',3X
     +         ,'TMIX(i)')
                   
             DO 411 II=ILAY,NP,-1 
              DTE(II)=QNEG/(1.0*1000.0*TV(II))

C...  ADJUSTIFIED WATER TEMPERATURE TNEW
              TNEW(II)=TSAVE(II)-DTE(II)
              
C             WRITE(99,800) II,DTE(II),TNEW(II),TSAVE(II)
  800         FORMAT(3X,I3,3(3X,F7.3))
  
  411         CONTINUE
             
            ENDIF  
          
C... TNEW < TBOUN, WE NEED FIND A NEW QICE FOR FREEZ-OVER
C... TO DETERMINE ICE THICKNESS

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

C... FURTHER CHECK MIXING DEPTH Zm, ILAY
C... CALCULATE OF POTENTIAL ENERGY OF MIXED LAYER
C... ZCM - Zg GRAVITY CENTER

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
        
C        WRITE(99,810)
 810     FORMAT(/3X,'Imix',8X,'TPE(i)',9X,'TKIN(i)')         
        DO 455 II=ILAY-1,2,-1
         
C        WRITE(99,566) II,TPEL(II),RKE
 566     FORMAT(3X,I3,3X,F12.1,3X,F12.1)         
         
         IF(TPEL(II).LT.RKE) THEN
         
          IF((II+1).EQ.ILAY) THEN
           ICHECK=-100
           DO 420 IK=1,ILAY
             T2(IK)=TNEW(ILAY)
 420       CONTINUE
           GOTO 409
          ENDIF
           
          ILAY=II+1
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

	SUBROUTINE MAKEFILE(PATH,TMPFILE,FFILE)
      CHARACTER*1 PATH(64),TMPFILE(16),FFILE(80)
	
	DO I = 1, 80
	   FFILE(I) = ' '
	ENDDO
	DO I = 1, 64
	   IF(PATH(I).NE.' ') THEN
       	   FFILE(I) = PATH(I)
         ELSE
	     J = I
	     GOTO 2001
	   ENDIF
	ENDDO
 2001	DO I = 1, 16
	   FFILE(J+I-1) = TMPFILE(I)
	ENDDO
	RETURN
	END