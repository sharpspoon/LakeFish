REAL:: SCORE,TOTAL,G,AVG
TOTAL = 0.0
G = 0.0
READ *,SCORE
PRINT *,'SCORE=',SCORE
TOTAL=TOTAL+SCORE
G = G+1

READ *,SCORE
PRINT*,SCORE
TOTAL = TOTAL+SCORE
G = G+1

AVG=TOTAL/G

Data 89.00 79.00 68.00
     


PRINT*,'  AVERGAE=', AVG
END

