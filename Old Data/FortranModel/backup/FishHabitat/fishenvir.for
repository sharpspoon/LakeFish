C----------------------------------------------------------------------

C            THIS PROGRAM USES THE FOLLOWING INPUT FILES:
C     A)  STATION INFORMATION  (STATION.SDF)              ----- 1 FILE
C     B)  DEPTH VS DO  DATA FILE     (*.DOX)              ----- 1 FILE  
C     C)  DEPTH VS TEMP DATA         (*.TEM)              ----- 1 FILE
C     D)  DEPTH VS VOLUME FOR INTERPOLATION  (AREA.SDF)   ----- 1 FILE
C     D)  PARAMETERS FOR COMPUTING LAKE STYLE(INPUT.INI)  ----- 1 FILE
C                                                  --------------------
C                                                    TOTAL :    5 FILES

C       THEN IT PRODUCES A OUT PUT FILE WHICH CONTAIN A TABLE SHOWING
C       THE VALUES OF DEPTH AT LGGT, UGGT,LT AND CRITICAL DO AND 
C          THE VALUES OF GROWTH PARAMETERS (NSB,NSE,GSE.....) FOR COOL,
C       COLD OR WARM WATER FISH FOR  ALL (366) JULIAN DATES FOR 
C                      ANY OF THE 209 STATION THROUGH U.S.A.
C-----------------------------------------------------------------------

       REAL LTT,LGGT   
       INTEGER PASTFUTR,ENVRN,GSB,GSE,GSL,GZER,NSL,GSL1,GSL2,
     + COUNT1,DATANO,GGHAINT,GGHVINT         
       INTEGER SMON,SDAY,SYEAR,FMON,FDAY,FYEAR
       DIMENSION SLATD(61,16),SLONG(61,16),TEMP(400,50),DEPTH(40),
     + DLOX(400,50),D1(400),D2(400),D3(400),D4(400),JSTA(61),A(27),
     + AZ(30),AZL(30),AAL(30),AV(30),AVL(30),
     + AR(30),NJDAY(366),TT(50),DDO(50),NJMON(12)

       CHARACTER*80 FILENAME
       CHARACTER*64 TITLE, PATHNAME
       CHARACTER*16 TAPE1,TAPE2,TAPE3,TAPE4,TAPE5
       CHARACTER*1 T1(16),T2(16),T3(16),T5(16),STA(61,2),NUM(10),
     &             FILE6(16),FILE7(16),FILE38(16),FILE150(16),PATH(64)

       CHARACTER*16 WSTATE(61),WSTATION(61,16),WFNAME(61,16),FFILE(80)
       EQUIVALENCE (TAPE1,T1(1)),(TAPE2,T3(1)),(TAPE3,T3(1)),
     &             (PATHNAME,PATH(1)),(FILENAME,FFILE(1))

       DATA NUM/'0','1','2','3','4','5','6','7','8','9'/,
     &	 FILE6  /'S','T','A','T','I','O','N','.','S','D','F',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE7  /'i','n','p','u','t','.','i','n','i',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE38 /'A','R','E','A','.','S','D','F',' ',' ',' ',' ',' ',
     &	         ' ',' ',' '/,
     &	 FILE150/'c','h','e','c','k','f','i','s','.','d','a','t',' ',
     &	         ' ',' ',' '/

       DATA NJMON/31,28,31,30,31,30,31,31,30,31,30,31/

C-----------------------------------------------------------------------

C      open(17,file="D:\Inetpub\wwwroot\lake\FortranModel\path.txt")
C      read(17,'(A)') PATHNAME
	read(*,'(A)') PATHNAME
C      close(17)
C               READ THE DATA FILE FOR STATION INFORMATION
 
C**** OPEN FILE "checkfis.dat"
      CALL MAKEFILE(PATH,FILE150,FFILE)
      OPEN(150,FILE=FILENAME)
C**** OPEN FILE "STATION.SDF"
C      CALL MAKEFILE(PATH,FILE6,FFILE)
C      OPEN(6,FILE=FILENAME,STATUS='OLD')
      OPEN(6,FILE="D:\Inetpub\wwwroot\lake\FortranModel\station.sdf")
      READ(6,*)
C 10   READ(6,20,END=30) JS,JN,STA(JS,1),STA(JS,2),SLATD(JS,JN),
C     +SLONG(JS,JN),WSTATE(JS),WSTATION(JS,JN), WFNAME(JS,JN)
 10   READ(6,*,END=30) JS,JN,SLATD(JS,JN),SLONG(JS,JN)
      JSTA(JS)=JN
 
 20   FORMAT(3X,I2,7X,I1,1X,A1,A1,3X,F6.3,3X,F7.3,A16,A16,1X,A16)
      GOTO 10
 30   CLOSE(6)

C****   READ THE DATA FROM VOLUME FILE "AREA.SDF" ----------------------
      CALL MAKEFILE(PATH,FILE38,FFILE)
      OPEN (38,FILE=FILENAME,STATUS='OLD')

      READ(38,*) NK

      DO I=1,NK
         READ(38,*) AZL(I),AAL(I),AVL(I)
      ENDDO 
	     

      DO I=1,NK
         AZ(I)=AZL(NK)-AZL(NK-I+1)
	   AR(I)=AAL(NK-I+1)
         AV(I)=AVL(NK)-AVL(NK-I+1)
      ENDDO

      HMAX = AZ(NK)
      
C**** INPUT FILENAME FOR T & DO, START TIME AND END TIME
      CALL MAKEFILE(PATH,FILE7,FFILE)
      OPEN (7,FILE=FILENAME,STATUS='OLD')
      READ(7,*) 
      READ(7,'(A)') TAPE1
	READ(7,*) ISTATE,ISTATION
      READ(7,*) SMON,SDAY,SYEAR,FMON,FDAY,FYEAR

C**** SMON,SDAY,SYEAR : SIMULATION START DATE: MONTH, DAY, YEAR
C**** FMON,FDAY,FYEAR : SIMULATION END   DATE: MONTH, DAY, YEAR

C----------------- SET THE INPUT & OUTPUT FILENAME -----------------------
      DO I = 1, 16
	   T2(I) = T1(I)
	   T3(I) = T1(I)
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
C********************** INITIALIZE VARIABLES ***********************
 80   DO I = 1, 400
	   DO J = 1, 50
	      TEMP(I,J) = 0.0
            DLOX(I,J) = 0.0
	   ENDDO
	ENDDO
      DO I = 1, 366
	   NJDAY(I) = 0
	ENDDO

C----- COMPUTE THE AVERAGE VALUE OF T & DO OVER THE GIVEN YEARS ------
      DO IYEAR = SYEAR, FYEAR

C****  SELECT IYEAR'S SIMULATION T & DATA FOR INPUT
         NA=INT((IYEAR-100*INT(IYEAR/100.0))/10.0)
         NB=IYEAR-10*INT(IYEAR/10.0)
         T1(INUM)=NUM(NA+1)
         T1(INUM+1)=NUM(NB+1)
         T2(INUM)=NUM(NA+1)
         T2(INUM+1)=NUM(NB+1)

C****  CHECK WHETHER SIMULATION T & DO DATA INPUT FILE EXIST OR NOT
         CALL MAKEFILE(PATH,T1,FFILE)
         INQUIRE(FILE=FILENAME,EXIST=LFILE)
         IF(LFILE) THEN
            OPEN (12,FILE=FILENAME,STATUS='OLD')
C            WRITE(*,*)" SIMULATION    T     INPUT FILE OPENED:", TAPE1
            WRITE(150,*)" SIMULATION    T     INPUT FILE OPENED:", TAPE1
	   ELSE
C            WRITE(*,*)"CAN'T OPEN FILE : ",TAPE1
            WRITE(150,*)"CAN'T OPEN FILE : ",TAPE1
            STOP
         ENDIF
         CALL MAKEFILE(PATH,T2,FFILE)
         INQUIRE(FILE=FILENAME,EXIST=LFILE)
         IF(LFILE) THEN
            OPEN (24,FILE=FILENAME,STATUS='OLD')
C            WRITE(*,*)" SIMULATION    DO    INPUT FILE OPENED:", TAPE2
            WRITE(150,*)" SIMULATION    DO    INPUT FILE OPENED:", TAPE2
	   ELSE
C            WRITE(*,*)"CAN'T OPEN FILE : ",TAPE2
            WRITE(150,*)"CAN'T OPEN FILE : ",TAPE2
            STOP
         ENDIF

C----- SET START & END JULIAN DAY FOR THE YEAR'S DATA TO BE READ -----
         JDAYT = 0
         JDAYDO = 0
         IF(IYEAR.EQ.SYEAR) THEN
	      JDAYT = 0
	      DO I = 1, SMON - 1
	         JDAYT = JDAYT + NJMON(I)
	      ENDDO
	      JDAYT = JDAYT + SDAY
            JDAYT = JDAYT - 1
	      JDAYDO = JDAYT
	   ENDIF

C         WRITE(*,*)"IN ",IYEAR," T & DO AVAILABLE FROM THE DAY:",JDAYT+1
       WRITE(150,*)"IN ",IYEAR," T & DO AVAILABLE FROM THE DAY:",JDAYT+1

C----------------------- READ DATA FROM TEMP FILE ------------------

         DO IM=1,12
            READ (12,50,END=120)JMON,JSDAY,NDAY,JYEAR,DATANO
            READ(12,100)(DEPTH(J),J=1,DATANO)

C------------- READ AND ACCUMULATE THE DO VALUE ---------------
            DO J=1,NDAY
               READ(12,100)(TT(I),I=1,DATANO)
               IF((JMON.NE.2).OR.(JMON.EQ.2.AND.J.NE.29)) THEN
   	            DO I = 1, DATANO
			       TEMP(JDAYT+J,I) = TEMP(JDAYT+J,I) + TT(I) 
                  ENDDO
                  NJDAY(JDAYT+J) = NJDAY(JDAYT+J) + 1
	         ENDIF
            ENDDO
	      IF(JMON.EQ.2.AND.NDAY.EQ.29) NDAY =28
            JDAYT = JDAYT + NDAY
         ENDDO
 120     CLOSE(12)

 50      FORMAT(1X,3I4,I6,I4)
 100     FORMAT(1X, 10F8.3)
 
C------------------------READ DATA FROM DO FILE------------------------

         DO IM=1,12
            READ (24,50, END=240)JMON,JSDAY,NDAY,JYEAR,DATANO
            READ(24,100)(DEPTH(J),J=1,DATANO)

C------------- READ AND ACCUMULATE THE DO VALUE ---------------
            DO J=1,NDAY
               READ(24,100)(DDO(I),I=1,DATANO)
               IF((JMON.NE.2).OR.(JMON.EQ.2.AND.J.NE.29)) THEN
	            DO I = 1, DATANO
			       DLOX(JDAYDO+J,I) = DLOX(JDAYDO+J,I) + DDO(I) 
                  ENDDO
               ENDIF
            ENDDO
	      IF(JMON.EQ.2.AND.NDAY.EQ.29) NDAY = 28
            JDAYDO = JDAYDO + NDAY
         ENDDO
 240     CLOSE(24)

         IF(JDAYT.NE.JDAYDO) THEN
C	      WRITE(*,*)" JDAYT  =",JDAYT
C	      WRITE(*,*)" JDAYDO =",JDAYDO
C	      WRITE(*,*)" INPUT T & DO WRONG, PLEASE CHECK! "
	      WRITE(150,*)" JDAYT  =",JDAYT
	      WRITE(150,*)" JDAYDO =",JDAYDO
	      WRITE(150,*)" INPUT T & DO WRONG, PLEASE CHECK! "
	      STOP
	   ELSE
C	      WRITE(*,*)"                       UNTIL JULIAN DAY: ", JDAYT
	      WRITE(150,*)"                     UNTIL JULIAN DAY: ", JDAYT
         ENDIF
	ENDDO		   

C----- SET START & END JULIAN DAY FOR COMPUTING THE FISH HABITAT -----
         IF((SYEAR+1).LT.FYEAR) THEN
            JDAYS = 1
            JDAYE = 365
	   ELSE IF((SYEAR+1).EQ.FYEAR) THEN
	      JDAYS = 1
	      JDAYE = 365
	      IF(FMON.LT.SMON) THEN
C	         WRITE(*,*)"WRONG! PLEASE RE-INPUT SIMULATION TIME!"
	         WRITE(150,*)"WRONG! PLEASE RE-INPUT SIMULATION TIME!"
		     STOP
		  ENDIF 
         ELSE IF(SYEAR.EQ.FYEAR) THEN
	      JDAYS = 0
	      DO I = 1, SMON - 1
	         JDAYS = JDAYS + NJMON(I)
	      ENDDO
	      JDAYS = JDAYS + SDAY
            JDAYE = 0
            DO I = 1, FMON - 1
               JDAYE = JDAYE + NJMON(I)
            ENDDO
            JDAYE = JDAYE + FDAY
         ENDIF

C--------- CHECK JDAYS, JDAYE, NJDAY(365) ------------
         WRITE(150,*)"JDAYS =", JDAYS
	   WRITE(150,*)"JDAYE =", JDAYE
         WRITE(150,*)"      JULIAN DAY    YEARS"
	   DO I = 1,365
	      WRITE(150,*)I,NJDAY(I)
	   ENDDO

C--------------- COMPUTE AVERAGE T & DO OVER THE GIVEN YEARS --------

         DO I = JDAYS, JDAYE
	      IF(NJDAY(I).EQ.0) THEN
C		     WRITE(*,*)"COMPUTING ERROR ON START & END JULIAN DAY?"
		     WRITE(150,*)"COMPUTING ERROR ON START & END JULIAN DAY?"
			 STOP
		  ENDIF
            DO J = 1, DATANO
		     TEMP(I,J) = TEMP(I,J)/NJDAY(I)
		     DLOX(I,J) = DLOX(I,J)/NJDAY(I)
	      ENDDO
	   ENDDO   
	   		              
C****  SET START YEAR AND END YEAR FOR OUTPUT FILENAME **************
         NA=INT((SYEAR-100*INT(SYEAR/100.0))/10.0)
         NB=SYEAR-10*INT(SYEAR/10.0)
         T3(INUM)=NUM(NA+1)
         T3(INUM+1)=NUM(NB+1)
         T3(INUM+2)='-'
         NA=INT((FYEAR-100*INT(FYEAR/100.0))/10.0)
         NB=FYEAR-10*INT(FYEAR/10.0)
         T3(INUM+3)=NUM(NA+1)
         T3(INUM+4)=NUM(NB+1)

C-----THE FISH TYPE "ENTER 1 FOR COLD,2 FOR COOL OR 3 FOR WARM WATER FISHE"
      DO ENVRN = 1, 3

C---------------------- SET THE OUTPUT FILENAME  ---------------------
C
      II = INUM + 4
      IF(ENVRN.EQ.1)THEN
         T3(II+1) ='.'
         T3(II+2) ='C'
         T3(II+3) ='D'     
      ELSE IF(ENVRN.EQ.2)THEN
         T3(II+1) ='.'
         T3(II+2) ='C'
         T3(II+3) ='L'     
      ELSE IF(ENVRN.EQ.3)THEN
         T3(II+1) ='.'
         T3(II+2) ='W'
         T3(II+3) ='M'     
      ENDIF

C-----------------------  OPEN OUTPUT THE FILE  --------------------
       
      CALL MAKEFILE(PATH,T3,FFILE)
      OPEN (36,FILE=FILENAME)
C      WRITE(*,*)" ANALYSIS RESULT WILL BE OUTPUT INTO ", TAPE3
      WRITE(150,*)" ANALYSIS RESULT WILL BE OUTPUT INTO ", TAPE3

C------------ SPECIFY TEMPERATURE VALUES FOR FISH TYPES  -----------      
      
      IF(ENVRN.EQ.1)THEN
      
      LGGT=9.00
      UGGT=18.50
      LTT=23.40
      SURVL=3.00
      
      ELSE
      
          IF(ENVRN.EQ.2)THEN
          LGGT=16.30
          UGGT=28.20
          LTT=30.40
          SURVL=3.00
          ELSE
                LGGT=19.70
                UGGT=32.30
                LTT=99.00
                SURVL=2.50
         ENDIF
      ENDIF
                     

C---- INTER POLATION FOR THE DEPTH CORRESPONDING TO THE TEMPERATURE OF-- 
C --------------------       LGGT,UGGT,LT     --------------------------
  
C-------------------        INTER POLATION FOR LGGT   -----------------

         DO 300 J=JDAYS,JDAYE
            D1(J)=99.00
            L=0
            M=0
                DO 290 I=1,DATANO
                IF(TEMP(J,I).GT.LGGT)L=L+1
 290            CONTINUE    
                    DO 295 I=1,DATANO
                    IF(TEMP(J,I).LT.LGGT)M=M+1
 295                CONTINUE       
         IF(L.EQ.DATANO)D1(J)=HMAX
         IF(M.EQ.DATANO)D1(J)=0.00
 300     CONTINUE    

         DO 320 J=JDAYS,JDAYE
         IF(D1(J).EQ.HMAX.OR.D1(J).EQ.0.00)GO TO 320  
              DO 310 I=1,DATANO
              IF(LGGT.EQ.TEMP(J,I))THEN
              D1(J)=DEPTH(I)
              GO TO 320
              ENDIF                       
              IF(LGGT.LT.TEMP(J,I).AND.LGGT.GT.TEMP(J,I+1))THEN
              DEN =TEMP(J,I)-TEMP(J,I+1)
              D1(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(TEMP(J,I)-LGGT)/DEN
              GO TO 320
              ENDIF
              IF(LGGT.GT.TEMP(J,I).AND.LGGT.LT.TEMP(J,I+1))THEN
              DEN =TEMP(J,I+1)-TEMP(J,I)
              D1(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(LGGT-TEMP(J,I))/DEN
              GO TO 320
              ENDIF
 310          CONTINUE
 320      CONTINUE
                          
C------------------       INTER POLATION FOR UGGT     ------------------   
 
 
        DO 350 J=JDAYS,JDAYE
            D2(J)=99.00
            L=0
            M=0
                DO 330 I=1,DATANO
                IF(TEMP(J,I).GT.UGGT)L=L+1
 330            CONTINUE    
                    DO 340 I=1,DATANO
                    IF(TEMP(J,I).LT.UGGT)M=M+1
 340                CONTINUE       
         IF(L.EQ.DATANO)D2(J)=HMAX
         IF(M.EQ.DATANO)D2(J)=0.00
 350     CONTINUE    

         DO 370 J=JDAYS,JDAYE
	      IF(D2(J).EQ.HMAX.OR.D2(J).EQ.0.00)GO TO 370
               DO 360 I=1,DATANO
               IF(UGGT.EQ.TEMP(J,I))THEN
                  D2(J)=DEPTH(I)
                  GO TO 370
               ENDIF                       
              IF(UGGT.LT.TEMP(J,I).AND.UGGT.GT.TEMP(J,I+1))THEN
                    DEN =TEMP(J,I)-TEMP(J,I+1)
        D2(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(TEMP(J,I)-UGGT)/DEN
        GO TO 370
            ENDIF
            IF(UGGT.GT.TEMP(J,I).AND.UGGT.LT.TEMP(J,I+1))THEN
                   DEN =TEMP(J,I+1)-TEMP(J,I)
        D2(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(UGGT-TEMP(J,I))/DEN
        GO TO 370
            ENDIF
               
  360       CONTINUE
  370     CONTINUE
C------------------       INTER POLATION FOR LT       ------------------   

      DO 400 J=JDAYS,JDAYE
            D3(J)=99.00
            L=0
            M=0
                DO 380 I=1,DATANO
                IF(TEMP(J,I).GT.LTT)L=L+1
 380            CONTINUE    
                    DO 390 I=1,DATANO
                    IF(TEMP(J,I).LT.LTT)M=M+1
 390                CONTINUE       
         IF(L.EQ.DATANO)D3(J)=HMAX
         IF(M.EQ.DATANO)D3(J)=0.00
 400     CONTINUE    

         DO 420 J=JDAYS,JDAYE
         IF(D3(J).EQ.HMAX.OR.D3(J).EQ.0.00)GO TO 420  
               DO 410 I=1,DATANO
               IF(LTT.EQ.TEMP(J,I))THEN
               D3(J)=DEPTH(I)
               GO TO 420
               ENDIF                       
               IF(LTT.LT.TEMP(J,I).AND.LTT.GT.TEMP(J,I+1))THEN
                    DEN =TEMP(J,I)-TEMP(J,I+1)
        D3(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(TEMP(J,I)-LTT)/DEN          
                      GO TO 420
                   ENDIF
                IF(LTT.GT.TEMP(J,I).AND.LTT.LT.TEMP(J,I+1))THEN
                  DEN =TEMP(J,I+1)-TEMP(J,I)
        D3(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(LTT-TEMP(J,I))/DEN                                        
                      
                      GO TO 420
                    ENDIF
               
 410           CONTINUE
 420     CONTINUE

C------------------       INTER POLATION FOR DO       ------------------   

           DO 450 J=JDAYS,JDAYE
            D4(J)=99.00
            L=0
            M=0
                DO 430 I=1,DATANO
               
                   IF(DLOX(J,I).GT.SURVL)L=L+1
 430             CONTINUE    
                DO 440 I=1,DATANO
               IF(DLOX(J,I).LT.SURVL)M=M+1
 440             CONTINUE       
              IF(L.EQ.DATANO)D4(J)=HMAX
              IF(M.EQ.DATANO)D4(J)=0.00
 450         CONTINUE    

  
             DO 470 J=JDAYS,JDAYE
             IF(D4(J).EQ.HMAX.OR.D4(J).EQ.0.00)GO TO 470
               DO 460 I=1,DATANO
               IF(SURVL.EQ.DLOX(J,I))THEN
               D4(J)=DEPTH(I)
                          GO TO 470
               ENDIF                       
               IF(SURVL.LT.DLOX(J,I).AND.SURVL.GT.DLOX(J,I+1))THEN
                    DEN =DLOX(J,I)-DLOX(J,I+1)
        D4(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(DLOX(J,I)-SURVL)/DEN
        
                   GO TO 470
                   ENDIF
            IF(SURVL.GT.DLOX(J,I).AND.SURVL.LT.DLOX(J,I+1))THEN
                   DEN =DLOX(J,I+1)-DLOX(J,I)
        D4(J)=DEPTH(I)+(DEPTH(I+1)-DEPTH(I))*(SURVL-DLOX(J,I))/DEN                                        
                ENDIF
 460           CONTINUE
 470    CONTINUE
     
C----------------------------------------------------------------------      
C  D1 D2 D3 AND D4 ARE DEPTH VALUES CORRESPONDING TO THE TEMPERATURE OF
C           LGGT      [ D1 ]
C           UGGT      [ D2 ]  
C           LT        [ D3 ] 
C           DO LIMIT  [ D4 ]    
C
C-----------------------  NSB  --------------------------------------

 480    DO 490  J=JDAYS,JDAYE 
        IF(D3(J).GE.D4(J))THEN
        NSB=J
        GO TO 500
        ENDIF
        COUNT1=J
 490    CONTINUE      
       
C       IF DO AND LT LINE DOES NOT INTERSECT 
C            THEN NO NSB NSE AND NSL VALUE 
C
        NSB=0
        NSE=0
        NSL=0
        GO TO 540
C -----------------------------NSE ------------------------------        
 500    DO 530 J=JDAYS,JDAYE 
C
C         STATEMENT 505 USED TO PROCEED IN REVERSE DIRECTION
C        
 505    L=JDAYE-J+JDAYS
        IF(D3(L).GE.D4(L))THEN
        NSE=L

C       
C         STATEMENT NO 515 TO 520 IS FOR CHECKING THE FLACTUATION
C         OF DO OR LT VALUES IN SHALLOW LAKES 
C       
 510    NSL = NSE-NSB 
 515    DO 520 I=NSB,NSE
        IF(D3(I).LT.D4(I))NSL=NSL-1
 520    CONTINUE 
        GO TO 540
       
        ENDIF
 530    CONTINUE

C --------------------- GSB --------------------------------------    

 540    DO 550  J=JDAYS,JDAYE
        IF(D1(J).GT.0.0.AND.D4(J).GT.0.00)THEN
           GSB=J
           GO TO 560
        ENDIF
 550    CONTINUE
C --------------------------------- GSE ----------------------------
 560    DO 570 J=JDAYS,JDAYE
C
C           STATEMENT 565 IS TO PROCEED IN REVERSE ORDER
C        

C  " REV. ORDER DIRECTION HELPS TO FIND OUT FLACTUATIONS EASILY "    

 565       I=JDAYE-J+JDAYS 
        IF(D1(I).GT.0.0.AND.D4(I).GT.0.00)THEN
        GSE=I
        GO TO 580
        ENDIF
 570   CONTINUE

C -------------- GSL ------------------------------------------------

C              STATEMENT 580 TO 540 TO CALCULATE GSL1 
 
 580    DO 600 J=GSB,JDAYE
        IF(D2(J).GE.D4(J))THEN

C     -----          FOR GSL1 :                  

C     ----    GSB IS FIRST GOOD GROWTH DAY  -----
C     ---     K IS LAST GOOD GROWTH DAY   ----

         K=J-1  
         GSL1=K-GSB+1
        DO 590 L=GSB,K
        IF(D1(L).GT.0.00)GO TO 590
        GSL1=GSL1-1
 590    CONTINUE
 
        GO TO 620
        ENDIF
        COUNT2=J
 600    CONTINUE

C         COUNT1 USED TO CHECK WETHER UGGT AND DO LINE INTERSECT
C     STATEMENT 605 TO 615 TO CALAULATE GSL WHEN UGGT AND DO DONT MEET

 605    IF(COUNT2.EQ.JDAYE)THEN
        GSL=GSE-GSB+1
        GZER=0
        DO 610 J=GSB,GSE
        IF(D1(J).GT.0.00)GO TO 610
        GSL=GSL-1
 610    CONTINUE
        
        GO TO 670
 615    ENDIF 
    
C                 STATEMENT 620 TO 660 TO CALCULATE GSL2
C
C   --------          FOR   GSL2:      
C   -------     M IS FIRST GOOD GROWTH DAY        -----------------   
C   -------     GSE IS LAST GOOD GROWTH DAY       ------------------
      
 620    DO 660 J=JDAYS,JDAYE+1
        I=GSE+1-J
        IF(D2(I).GE.D4(I))THEN
        M=I+1 
        GSL2=GSE-M+1
 
        DO 630 L=M,GSE
        IF(D1(L).GT.0.00)GO TO 630
        GSL2=GSL2-1
 630    CONTINUE
 
         GSL=GSL1+GSL2
         GZER=M-K-1
 
C    STATEMENT 635 TO 650 TO CALCULATE FLACTUATION WITHIN GZER
 
 635     DO 650 I=K+1,M-1 
         IF(D2(I).LT.D4(I))THEN
         GZER=GZER-1
         GSL=GSL+1
         ENDIF
 650     CONTINUE
         GO TO 670
         ENDIF
 660     CONTINUE 
             
C---------------------------------------------------------------------
C                GGHA(GOOD GROWTH HABITAT AREA) = SUM(Z*Jd)
 670     GGHA=0.00

C --- GGHV (GOOD GROWTH HABITAT VOLUME ) = SUM ( VOL*Jd)/As  ------------

 690     GGHV=0.0     
        
       DO 760 J=GSB,GSE
	 
          IF(D4(J).LE.D2(J))GO TO 760
          IF(D4(J).GE.D1(J))THEN
                                                 
             ZD=D1(J)
              
             DO 700 I=1,NK
	          
		      IF(ZD.EQ.AZ(I)) THEN
	             GGHV=GGHV+AV(I)
	             
		    	 GGHA=GGHA-AR(I)
	             GOTO 710
	          ENDIF
        
	          IF(ZD.GT.AZ(I))GO TO 700
                DEN=AZ(I)-AZ(I-1)
                VOL=AV(I-1)+(ZD-AZ(I-1))*(AV(I)-AV(I-1))/DEN     
                GGHV=GGHV+ VOL

			  AREA=AR(I-1)+(ZD-AZ(I-1))*(AR(I)-AR(I-1))/DEN     
                GGHA=GGHA-AREA
      
			  GO TO 710
 700         CONTINUE
 710         ZD=D2(J)    
             DO 720 I=1,NK
			
		      IF(ZD.EQ.AZ(I)) THEN
	             GGHV=GGHV-AV(I)
				  
		    	 GGHA=GGHA+AR(I)
	             GOTO 760
	          ENDIF
                
			  IF(ZD.GT.AZ(I))GO TO 720
                DEN=AZ(I)-AZ(I-1)
   
                VOL=AV(I-1)+(ZD-AZ(I-1))*(AV(I)-AV(I-1))/DEN    
                GGHV=GGHV- VOL
	
			  AREA=AR(I-1)+(ZD-AZ(I-1))*(AR(I)-AR(I-1))/DEN    
                GGHA=GGHA+AREA
                
			  GO TO 760 
 720         CONTINUE

          ENDIF
          IF(D4(J).LE.D1(J))THEN
             ZD=D4(J)
             DO 730 I=1,NK
		   
		      IF(ZD.EQ.AZ(I)) THEN
	             GGHV=GGHV+AV(I)
			     GGHA=GGHA-AR(I)
	             GOTO 740
	          ENDIF
                 
			  IF(ZD.GT.AZ(I))GO TO 730
                DEN=AZ(I)-AZ(I-1)
   
                VOL=AV(I-1)+(ZD-AZ(I-1))*(AV(I)-AV(I-1))/DEN     
                GGHV=GGHV+ VOL
	
			  AREA=AR(I-1)+(ZD-AZ(I-1))*(AR(I)-AR(I-1))/DEN     
                GGHA=GGHA-AREA

                GO TO 740
 730         CONTINUE
 740         ZD=D2(J)    
             DO 750 I=1,NK
			
			  IF(ZD.EQ.AZ(I)) THEN
	             GGHV=GGHV-AV(I)
	             GGHA=GGHA+AR(I)
	             GOTO 760
	          ENDIF
                
		      IF(ZD.GT.AZ(I))GO TO 750
                DEN=AZ(I)-AZ(I-1)
   
                VOL=AV(I-1)+(ZD-AZ(I-1))*(AV(I)-AV(I-1))/DEN     
                GGHV=GGHV-VOL

		      AREA=AR(I-1)+(ZD-AZ(I-1))*(AR(I)-AR(I-1))/DEN     
                GGHA=GGHA+AREA

 	          GO TO 760
 750         CONTINUE
          ENDIF 
 760   CONTINUE

       GGHV=GGHV/(AR(1))
               
       GGHVINT=INT(GGHV+0.50)
	    	 
	 GGHA=GGHA/(AR(1))	
               
	 GGHAINT=INT(GGHA+0.50)
  
C-------------- GGHV-FRAC (FRACTION AVAILABLE OF GGHV)------------
           
	 IF(GSL.EQ.0.00)THEN
          GGHVFRAC=0.00
	     
	    GGHAFRAC=0.00
	 ELSE
          GGHVFRAC=(GGHV*AR(1))/(AV(NK)*GSL)
		  
	    GGHAFRAC=(GGHA/GSL)
	 ENDIF          

       ICNT=ICNT+1

C-----------------------------------------------------------------------

C                     WRITE THE RESULTS ON THE OUTPUT FILE
        WRITE(36,770)TAPE3
 770    FORMAT(/18X,'THE NAME OF THIS FILE IS:  ',A12/)  
        WRITE(36,780)ISTATION,ISTATE,T1(9),T1(10)
 780    FORMAT(3X,'STATION : ',I1,5X,'STATE : ',I2,'  ( ',A1,A1,' )'/)
        WRITE(36,790)
 790    FORMAT(3X,'LGGT',4X,'UGGT',5X,'LT',4X,'DO LIMIT',4X,
     +   'FISH TYPE')
         IF(ENVRN.EQ.1)THEN
         WRITE(36,800)LGGT,UGGT,LTT,SURVL
 800    FORMAT(2X,F5.2,3X,F5.2,3X,F5.2,3X,F5.2,5X,'COLD WATER FISH')      
         ENDIF
           IF(ENVRN.EQ.2)THEN
         WRITE(36,810)LGGT,UGGT,LTT,SURVL
 810    FORMAT(2X,F5.2,3X,F5.2,3X,F5.2,2X,F5.2,5X,'COOL WATER FISH')
         ENDIF
         IF(ENVRN.EQ.3)THEN
         WRITE(36,820)LGGT,UGGT,LTT,SURVL
 820    FORMAT(2X,F5.2,3X,F5.2,3X,F5.2,2X,F5.2,5X,'WARM WATER FISH')
         ENDIF
        WRITE(36,830)
 830  FORMAT (2X,'------------------------------------------------------
     +--------------')

        WRITE(36,840)
 840    FORMAT(1X,'JULIAN DAY',2X,'D1(LGGT)',2X,'D2(UGGT)',2X,'D3(LTT)',
     +   2X'D4(DISSLVD OXYGN)')
         DO  860    I=JDAYS,JDAYE
         WRITE(36,850)I,D1(I),D2(I),D3(I),D4(I)
 850     FORMAT(3X,I3,7X,F5.2,5X,F5.2,5X,F5.2,10X,F5.2)
 860     CONTINUE

C------------------ PREPARE THE TABLE -----------------------------  
        IF(COUNT1.EQ.JDAYE)THEN
        WRITE(36,870)
 870    FORMAT(/10X,'[ LT LINE AND DO LINE NOT INTERSECTED'//
     +  10X,'SO NO NSE , NSB AND NSL VALUE ]'/)
        ENDIF
 
        WRITE(36,880)
 880    FORMAT(/10X,'NSB',2X,'NSE',2X,'NSL',2X,'GSB',2X'GSE',
     +  2X,'GSL',2X,'GZER',2X/)
        WRITE(36,890)NSB,NSE,NSL,GSB,GSE,GSL,GZER
 890    FORMAT(10X,7(I3,2X)/)
        IF(COUNT2.EQ.JDAYE)THEN
        WRITE(36,900)
 900    FORMAT(10X,'[UGGT LINE AND DO LINE NOT INTERSECTED'/10X,
     +  'SO NO GSL1 AND GSL2 VALUE ]'/)
        GO TO 920       
        ENDIF         
        WRITE(36,910)GSL1,GSL2,K,M
 910    FORMAT(10X,'GSL1=',I3,1X,'GSL2='I3,/10X,
     +  '1ST INTERSECTING PT (BETWEEN UGGT AND DO LINE) = 'I3,/
     +  10X,'2ND INTERSECTING PT (BETWEEN UGGT AND DO LINE) = 'I3)
 920    WRITE(36,930)GGHAINT,GGHAFRAC,GGHVINT,GGHVFRAC
 930     FORMAT(/,10X,'GGHA=     ' ,I5/10X,'GGHAFRAC= ',F5.2/10X,
     +   'GGHV=     ',I5/10X,'GGHVFRAC= ',F5.2//)  
        CLOSE(36)       
      ENDDO 
    
          STOP      
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