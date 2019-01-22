C************************************************************C
C                                                            C
C    This program will interpolate the result of simulation  C
C    data produced by Minilake according to the field data   C
C    and reformat output data for time series plotting.      C
C                                                            C
C    According to the given depth for which the user will    C
C    draw time series plotting, this program will compute    C
C    the corresponding data (i.e. T & DO of that depth )     C
C    based on the simulated data and field data.             C
C                                                            C
C    Two options :                                           C
C      1. interpolate the corresponding field data (default) C
C      2. no interpolation on field data                     C
C														   C
C************************************************************C
      PROGRAM T_DO_INTERPOLATION

      INTEGER SMON,SDAY,SYEAR,FMON,FDAY,FYEAR,DATANO, NOUSE

C----- ZS(40),TS(40),DOS(40) : SIMULATED DEPTH, TEMPERATURE & DISSOLVED OXYGEN
C----- ZF(40),TF(40),DOF(40) : FIELD     DEPTH, TEMPERATURE & DISSOLVED OXYGEN
      DIMENSION ZF(40),TF(40),DOF(40),ZS(40),TS(40),DOS(40),
     &          DEPTH(20),S_INTERP(10) 
      CHARACTER*80 FILENAME
      CHARACTER*64 TITLE, PATHNAME
      CHARACTER*16 TAPE1,TAPE2
      CHARACTER*1 T1(16),T2(16),NUM(10),FILE7(16),FILE15(16),FILE99(16),
     &            FILE25(16),FILE150(16),FILE250(16),FILE151(16),
     &            FILE251(16),FFILE(80),PATH(64)
      EQUIVALENCE (TAPE1,T1(1)),(TAPE2,T2(1)),
     &        	(PATHNAME,PATH(1)),(FILENAME,FFILE(1))
      DATA NUM/'0','1','2','3','4','5','6','7','8','9'/,
     &	 FILE7  /'i','n','p','u','t','.','i','n','i',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE15 /'s','i','m','p','l','t','.','t','e','m',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE25 /'s','i','m','p','l','t','.','d','o','x',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE99 /'c','h','e','c','k','t','d','o','.','d','a','t',' ',
     &	         ' ',' ',' '/
      DATA FILE150/'f','e','d','s','i','m','.','t','e','m',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE151/'f','e','d','s','i','m','.','d','o','x',' ',' ',' ',
     &	         ' ',' ',' '/,
     &     FILE250/'f','e','d','p','l','t','.','t','e','m',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE251/'f','e','d','p','l','t','.','d','o','x',' ',' ',' ',
     &	         ' ',' ',' '/
	DATA DEPTH/0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./

C      open(17,file="D:\Project\Huang\test\lake\path.txt")
c      read(17,'(A)') PATHNAME
      read(*,'(A)') PATHNAME
	read(*,*) SYEAR
	read(*,*) FYEAR
	read(*,*) ND
C      WRITE(*,*) PATHNAME, SYEAR, FYEAR, ND
	DO I=0, ND
	  DEPTH(I)=I;
C	WRITE(*,*) DEPTH(I)
	ENDDO
C      close(17)
	
C----- PlotDepth : data corresponding to it will be interpolated
C      DEPTH = 3.0
c      ND = 3
	CALL SORT( ND, DEPTH )
C----- IP_FIELD :  0 - INTERPOLATE FIELD DATA; 1 - NO INTERPOLATION ON FIELD DATA
      IP_FIELD = 0;
      
      OPEN (8,FILE='D:\Inetpub\wwwroot\lake\FortranModel\INPUTb.INI',
     &        STATUS = 'OLD') 
      READ(8,*)
      READ(8,*) MODEL,NCLASS,IDIATOM
      CLOSE(8)
      CALL MAKEFILE(PATH,FILE99,FFILE)
      OPEN (99,FILE=FILENAME,STATUS='UNKNOWN')

C**** OPEN FILE "INPUT.INI"
      CALL MAKEFILE(PATH,FILE7,FFILE)
      OPEN (7,FILE=FILENAME,STATUS='OLD')

      READ(7,'(A)') 
      READ(7,'(A)') TAPE1
	READ(7,*) ISTATE,ISTATION
      READ(7,*) SMON,SDAY,NOUSE,FMON,FDAY,NOUSE
      
	READ(7,*) MBOT,ZMAX,ST
      READ(7,*) XK1,XK2
      READ(7,*) WSTR,WSSF
      READ(7,*) COEWIN,SNCOE
      READ(7,*) AHTBTM,SRCP
      READ(7,*) CFSNOW,CDIS0,CNDSNW0,CNDWI,DEPTHC
      READ(7,*) BTICE,ALFICE,GMICE,BTSNOW,ALFSNOW,GMSNOW
      READ(7,*) THICKIS,THICKSN
C----- READ THE TEMPERORY DATA : Z(I), T2(I), C2(I), CD2(I), CHLA2(K,I)
      READ(7,*) (ZS(I),I=1,MBOT)
      READ(7,*) (ZS(I),I=1,MBOT)
      READ(7,*) (ZS(I),I=1,MBOT) 
      READ(7,*) (ZS(I),I=1,MBOT)
      DO K=1,NCLASS
         READ(7,*)  (ZS(I),I=1,MBOT)
      ENDDO
      
	READ(7,*) IPRNT4
	IF(IPRNT4.EQ.1) THEN
         READ(7,*) (ZS(I),I=1,MBOT)
         READ(7,*) (ZS(I),I=1,MBOT)
         READ(7,*) (ZS(I),I=1,MBOT)
         READ(7,*) BODK20,SB20,XKR1,POMAX
         READ(7,*) EMCOE2,EMCOE3
      ENDIF

      READ(7,*) IPRNT2,IPRNT5,IPRNT6

      IF(IPRNT5.EQ.1) THEN
        READ(7,*) (ZS(I),I=1,IPRNT6)
      ENDIF

      READ(7,*) NPRINT,INFLOW,JDY

      READ(7,*) IFIELD,ICHLA

      CLOSE(7)

C----------------- SET THE INPUT & OUTPUT FILENAME -----------------------
      DO I = 1, 16
	   T2(I) = T1(I)
	ENDDO
      
      DO I=1,16
         II=16-I+1
    
         IF(T1(II).NE.' ') THEN
C****  TAPE1 --- SIMULATED TEMPERATURE INPUT FILENAME
              T1(II+1)='.'
              T1(II+2)='T'
              T1(II+3)='E'
              T1(II+4)='P'
C****  TAPE2 --- SIMULATED DO CONCENTRATION INPUT FILENAME
			T2(II+1)='.'
              T2(II+2)='D'
              T2(II+3)='O'
              T2(II+4)='X'
              INUM=II-1
              GOTO 80
         ENDIF
      
	ENDDO
 80   CONTINUE
C----- SET OUTPUT FILENAME "SIMPLT.TEM" FOR PLOTTING
      CALL MAKEFILE(PATH,FILE15,FFILE)
      OPEN(15,FILE=FILENAME,STATUS='unknown')
	WRITE(15,1510) ND,(DEPTH(I),I=1,ND)
	IF(IPRNT4.EQ.1) THEN
C----- SET OUTPUT FILENAME "SIMPLT.DOX" FOR PLOTTING
         CALL MAKEFILE(PATH,FILE25,FFILE)
	   OPEN(25,FILE=FILENAME,STATUS='unknown')
         WRITE(25,1510) ND,(DEPTH(I),I=1,ND)
	ENDIF
 1510 FORMAT(1X,I4,10F6.2)

C----- COMPUTE THE AVERAGE VALUE OF T & DO OVER THE GIVEN YEARS ------
      DO IYEAR = SYEAR, FYEAR

C****  SELECT IYEAR'S SIMULATION T & DATA FOR INPUT
         NA=INT((IYEAR-100*INT(IYEAR/100.0))/10.0)
         NB=IYEAR-10*INT(IYEAR/10.0)
         T1(INUM)=NUM(NA+1)
         T1(INUM+1)=NUM(NB+1)
         T2(INUM)=NUM(NA+1)
         T2(INUM+1)=NUM(NB+1)

C****  CHECK WHETHER SIMULATION T & DO DATA INPUT FILE EXIST ( THEN OPEN IT ) OR NOT
         CALL MAKEFILE(PATH,T1,FFILE)
         INQUIRE(FILE=FILENAME,EXIST=LFILE)
         IF(LFILE) THEN
            OPEN (12,FILE=FILENAME,STATUS='OLD')
            WRITE(99,*)" SIMULATION    T     INPUT FILE OPENED:", TAPE1
	   ELSE
            WRITE(99,*)"CAN'T OPEN FILE : ",TAPE1
            STOP
         ENDIF

         IF( IPRNT4.EQ.1 ) THEN
         CALL MAKEFILE(PATH,T2,FFILE)
            INQUIRE(FILE=FILENAME,EXIST=LFILE)
            IF(LFILE) THEN
               OPEN (24,FILE=FILENAME,STATUS='OLD')
              WRITE(99,*)" SIMULATION    DO    INPUT FILE OPENED:",TAPE2
	      ELSE
               WRITE(99,*)"CAN'T OPEN FILE : ",TAPE2
               STOP
            ENDIF
	   ENDIF

C----------------------- READ DATA FROM TEMP FILE ------------------

         DO IM=1,12
            READ (12,50,END=120)JMON,JSDAY,NDAY,JYEAR,DATANO
            READ(12,100)(ZS(J),J=1,DATANO)

C------------- READ AND ACCUMULATE THE DO VALUE ---------------
            DO J=1,NDAY
               READ(12,100)(TS(I),I=1,DATANO)
               JDAY = JULIANDAY(JMON,JSDAY+J-1,JYEAR)
               CALL ZTDOINTP(ND,DEPTH,DATANO,ZS,TS,S_INTERP)
               WRITE(15,2504)JYEAR,JDAY,(S_INTERP(I),I=1,ND)
            ENDDO
         ENDDO
 120     CLOSE(12)

 50      FORMAT(1X,3I4,I6,I4)
 100     FORMAT(1X, 10F8.3)
 
C------------------------READ DATA FROM DO FILE------------------------
         IF(IPRNT4.EQ.1) THEN
            DO IM=1,12
               READ (24,50, END=240)JMON,JSDAY,NDAY,JYEAR,DATANO
               READ(24,100)(ZS(J),J=1,DATANO)

C------------- READ AND ACCUMULATE THE DO VALUE ---------------
               DO J=1,NDAY
                  READ(24,100)(DOS(I),I=1,DATANO)
                  JDAY = JULIANDAY(JMON,JSDAY+J-1,JYEAR)
                  CALL ZTDOINTP(ND,DEPTH,DATANO,ZS,DOS,S_INTERP)
                  WRITE(25,2504)JYEAR,JDAY,(S_INTERP(I),I=1,ND)
               ENDDO
            ENDDO
 240        CLOSE(24)
         ENDIF
	ENDDO		   

      CLOSE(15)
	CLOSE(25)

C.... Field T ("FEDSIM.TEM","FEDPLT.TEM") & DO data interpolation on given depth
      IF( (IFIELD.EQ.1).AND.(IP_FIELD.EQ.0) ) THEN
         CALL MAKEFILE(PATH,FILE150,FFILE)
         OPEN(150,FILE=FILENAME,STATUS='OLD')
         CALL MAKEFILE(PATH,FILE250,FFILE)
         OPEN(250,FILE=FILENAME,STATUS='UNKNOWN')
	   WRITE(250,1510) ND,(DEPTH(I),I=1,ND)

 150	   READ(150,1502,END=125)MONTH,MDAY,MYEAR,MBOT,NF            
         READ(150,1503) (ZS(I),I=1,MBOT)
         READ(150,1503) (TS(I),I=1,MBOT)
         READ(150,1503) (ZF(I),I=1,NF)
         READ(150,1503) (TF(I),I=1,NF)
         
         JDAY = JULIANDAY(MONTH,MDAY,MYEAR)
         CALL ZTDOINTP(ND,DEPTH,NF,ZF,TF,S_INTERP)
	   IF ((MYEAR.GE.SYEAR).AND.(MYEAR.LE.FYEAR)) THEN
           WRITE(250,2504) MYEAR,JDAY,(S_INTERP(I),I=1,ND)
         ENDIF
         GOTO 150
 125     CLOSE( 150 )
         CLOSE( 250 )

C.... Output measured- & simulated dissolved oxygen data ("FEDSIM.DOX","FEDPLT.DOX")
         IF( IPRNT4.EQ.1 ) THEN
             CALL MAKEFILE(PATH,FILE151,FFILE)
      	   OPEN(151,FILE=FILENAME,STATUS= 'OLD')
             CALL MAKEFILE(PATH,FILE251,FFILE)
             OPEN(251,FILE=FILENAME,STATUS='UNKNOWN')
	       WRITE(251,1510) ND,(DEPTH(I),I=1,ND)

 151         READ(151,1502,END=140) MONTH,MDAY,MYEAR,MBOT,NF        
             READ(151,1503) (ZS(I),I=1,MBOT)
             READ(151,1503) (DOS(I),I=1,MBOT)
             READ(151,1503) (ZF(I),I=1,NF)
             READ(151,1503) (DOF(I),I=1,NF)

             JDAY = JULIANDAY(MONTH,MDAY,MYEAR)
             CALL ZTDOINTP(ND,DEPTH,NF,ZF,DOF,S_INTERP)
	       IF ((MYEAR.GE.SYEAR).AND.(MYEAR.LE.FYEAR)) THEN
                WRITE(251,2504) MYEAR,JDAY,(S_INTERP(I),I=1,ND)
             ENDIF

             GOTO 151
 140         CLOSE( 151 )
             CLOSE( 251 )
         ENDIF

	ENDIF

 1502 FORMAT(1X,I2,I4,I6,2I4)
 1503 FORMAT(1X,10F8.3)
 2504 FORMAT(1X,2I5,10F6.2)                       

      STOP 
	END
	   
C***********************************************
C         
C         SUBROUTINE FOR INTEPOLATION
C
C***********************************************
       SUBROUTINE ZTDOINTP(NZ,ZD,MBOT,ZS,S,S_INTERP)
       DIMENSION ZS(40),S(40),ZD(10),S_INTERP(10)

       IP = 0
 110	 IP = IP + 1 
	 Z = ZD(IP)     
       IF( Z.LT.ZS(1) ) THEN
	    S_INTERP(IP) = S(1) - (S(2)-S(1)) * (ZS(1)-Z)/(ZS(2)-ZS(1))
	    IF( IP.EQ.NZ ) THEN
      	   RETURN
	    ELSE
             GOTO 110
	    ENDIF
	 ENDIF

       IM = 0
 130   IM = IM + 1
 150   IF((Z.GE.ZS(IM)).AND.(Z.LT.ZS(IM+1))) THEN
          S_INTERP(IP) = S(IM) + (S(IM+1)-S(IM)) 
     &	               * (Z-ZS(IM))/(ZS(IM+1)-ZS(IM))
	    IF( IP.EQ.NZ ) THEN
      	   RETURN
	    ELSE
             IP = IP + 1
	       Z = ZD(IP)
             GOTO 150
	    ENDIF
	 ELSE
          IF( IM.LT.MBOT ) THEN
      	   GOTO 130
	    ELSE
	       GOTO 170
	    ENDIF
	 ENDIF

 170	 S_INTERP(IP) = S(MBOT) + (S(MBOT)-S(MBOT-1)) 
     &        	* (Z-ZS(MBOT))/(ZS(MBOT)-ZS(MBOT-1))
       IF( IP.EQ.NZ ) THEN
     	    RETURN
	 ELSE
          IP = IP + 1
	    Z = ZD(IP)
          GOTO 170
	 ENDIF

       END

C***********************************************
C         
C         FUNCTION FOR COMPUTING JULIAN DAY
C
C***********************************************
       FUNCTION JULIANDAY(MONTH,DAY,YEAR)
        
	 INTEGER DAY, YEAR
	 DIMENSION NJMON(12)
       DATA NJMON/31,28,31,30,31,30,31,31,30,31,30,31/
     
       FIP = YEAR / 4.0 - YEAR / 4
	 JULIANDAY = 0
       DO I = 1, MONTH - 1
          JULIANDAY = JULIANDAY + NJMON(I)
       ENDDO
       IF( (MONTH.GT.2).AND.(FIP.EQ.0.0) ) JULIANDAY = JULIANDAY + 1
	 JULIANDAY = JULIANDAY + DAY
       RETURN

       END

C******************************************************************
C         
C    SUBROUTINE FOR SORTING A GIVEN ARRAY AS INCREASING SQUENCE
C
C******************************************************************
       SUBROUTINE SORT(N,ARRAY)
        
       DIMENSION ARRAY(10)
      
	 DO I = N+1, 10
	    ARRAY(I) = 0.0
	 ENDDO

       DO I = 1, N
	    DO J = I+1, N
	       IF( ARRAY(I).GT.ARRAY(J) ) THEN
	          TMP = ARRAY(I)
	          ARRAY(I) = ARRAY(J)
	          ARRAY(J) = TMP
	       ENDIF
	    ENDDO
	 ENDDO

	 RETURN
	 END
C **** set file name
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