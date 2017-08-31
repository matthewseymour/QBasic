DIM y(1 TO 5) AS INTEGER
DIM trend(1 TO 5) AS INTEGER
DIM start(1 TO 5) AS INTEGER
DIM endd(1 TO 5) AS INTEGER
DIM chan(1 TO 5) AS INTEGER

RANDOMIZE TIMER
SCREEN 12
FOR i = 1 TO 5
y(i) = INT(RND * 200) - 100
NEXT i
start:
CLS

FOR d = 1 TO 5
start(d) = y(d)
NEXT d

FOR a = 0 TO 480 STEP 20
LINE (95, a)-(505, a), 8
NEXT a
FOR a = 100 TO 500 STEP 25
LINE (a, 0)-(a, 480), 8
NEXT a
FOR i = 100 TO 500 STEP 25
LINE (i, 235)-(i, 245), 7
NEXT i
LINE (100, 240)-(500, 240), 7
FOR i = 100 TO 499

FOR a = 0 TO 480 STEP 20
LINE (95, a)-(105, a), 7
LINE (495, a)-(505, a), 7
NEXT a
LINE (100, 0)-(100, 480), 7
LINE (500, 0)-(500, 480), 7

LINE (290, 119)-(310, 121), 2, BF
LINE (299, 110)-(301, 130), 2, BF
LINE (290, 359)-(310, 361), 4, BF

FOR d = 1 TO 5
'FOR s = 1 TO 1000
'NEXT s

IF INT(RND * 5) + 1 = 1 THEN trend(d) = trend(d) + INT(RND * 3) - 1
IF trend(d) > 4 THEN trend(d) = 4
IF trend(d) < -4 THEN trend(d) = -4

b& = INT(RND * 11) - 5
IF b& > trend(d) THEN y(d) = y(d) + 1
IF b& = trend(d) THEN y(d) = y(d) + 0
IF b& < trend(d) THEN y(d) = y(d) - 1
IF y(d) < -239 THEN y(d) = -239
IF y(d) > 240 THEN y(d) = 240

endd(d) = y(d)
chan(d) = -(start(d) - endd(d))
COLOR 15
LOCATE 1, 1
PRINT "Total  End"
LOCATE 2, 1
PRINT "Change Value"
COLOR d
LOCATE d + 2, 1
PRINT chan(d)
COLOR d
LOCATE d + 2, 8
PRINT y(d)

COLOR 15
LOCATE 8, 1
PRINT CLNG((chan(1) + chan(2) + chan(3) + chan(4) + chan(5)) / 5)
LOCATE 8, 8
PRINT CLNG((y(1) + y(2) + y(3) + y(4) + y(5)) / 5)

COLOR 15
LOCATE 10, 1
PRINT "Time"

COLOR 15
LOCATE 11, 1
PRINT i - 99

PSET (i, 240 - y(d)), d
LINE (i, 240 - CLNG((y(1) + y(2) + y(3) + y(4) + y(5)) / 5) - 1)-(i, 240 - CLNG((y(1) + y(2) + y(3) + y(4) + y(5)) / 5) + 1), 15, BF
NEXT d
NEXT i

FOR i = 0 TO 480 STEP 20
LINE (95, i)-(105, i), 7
LINE (495, i)-(505, i), 7
NEXT i
LINE (100, 0)-(100, 480), 7
LINE (500, 0)-(500, 480), 7
SLEEP
GOTO start

