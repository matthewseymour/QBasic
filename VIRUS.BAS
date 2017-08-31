SCREEN 12
RANDOMIZE TIMER
DIM celly(1 TO 200) AS INTEGER
DIM cellx(1 TO 200) AS INTEGER
DIM cellys(1 TO 200) AS INTEGER
DIM cellxs(1 TO 200) AS INTEGER
DIM celle(1 TO 200) AS INTEGER
DIM cellv(1 TO 200) AS INTEGER
DIM vx(1 TO 5000) AS INTEGER
DIM vy(1 TO 5000) AS INTEGER
DIM vxs(1 TO 5000) AS INTEGER
DIM vys(1 TO 5000) AS INTEGER
DIM ve(1 TO 5000) AS INTEGER
DIM va(1 TO 5000) AS INTEGER
DIM vi(1 TO 5000) AS INTEGER
DIM vage(1 TO 5000) AS INTEGER

start:
rr = INT(RND * 20) + 2
mh = INT(RND * 17) + 3
ls = INT(RND * 300) + 50
vc = INT(RND * 15) + 1
c = 200
v = 1

start2:

CLS
FOR i = 1 TO 5000
vx(i) = 0
vy(i) = 0
vxs(i) = 0
vys(i) = 0
ve(i) = 0
va(i) = 0
vi(i) = 0
vage(i) = 0
NEXT i

FOR i = 1 TO 200
celly(i) = 0
cellx(i) = 0
cellys(i) = 0
cellxs(i) = 0
celle(i) = 0
cellv(i) = 0
NEXT i

ve(1) = 1
vx(1) = INT(RND * 640) + 1
vy(1) = INT(RND * 480) + 1
vxs(1) = INT(RND * 9) - 4
vys(1) = INT(RND * 9) - 4

FOR i = 1 TO 200
celle(i) = 1
cellx(i) = INT(RND * 600) + 50
celly(i) = INT(RND * 440) + 50
cellxs(i) = INT(RND * 3) - 1
cellys(i) = INT(RND * 3) - 1
NEXT i

DO

LOCATE 1, 1
PRINT "Virus Population:"; v
LOCATE 2, 1
PRINT "Cell Population:"; c
LOCATE 1, 25
PRINT "Size: 1 /"; mh; "of Cell"
LOCATE 2, 25
PRINT "Multiplying Rate: + 1 /"; rr; "a Turn"
LOCATE 1, 60
PRINT "Lifespan:"; ls

IF (v = 0 AND c = 0) OR v = 0 THEN GOTO start
c = 0

LINE (0, 40)-(639, 479), 14, B

FOR i = 1 TO 200

IF cellv(i) > mh AND i < 151 THEN
        CIRCLE (cellx(i), celly(i)), 10, 0
        celle(i) = 0
END IF

IF celle(i) = 0 THEN GOTO scell
c = c + 1
cellv(i) = 0

CIRCLE (cellx(i), celly(i)), 10, 0
cellx(i) = cellx(i) + cellxs(i)
celly(i) = celly(i) + cellys(i)

IF cellx(i) < 10 OR cellx(i) > 630 THEN
        IF cellx(i) < 10 THEN cellx(i) = 10
        IF cellx(i) > 630 THEN cellx(i) = 630
        cellxs(i) = -cellxs(i)
END IF
IF celly(i) < 50 OR celly(i) > 470 THEN
        IF celly(i) < 50 THEN celly(i) = 50
        IF celly(i) > 470 THEN celly(i) = 470
        cellys(i) = -cellys(i)
END IF

IF i < 151 THEN CIRCLE (cellx(i), celly(i)), 10, 4 ELSE CIRCLE (cellx(i), celly(i)), 10, 15

scell:

NEXT i

v = 0
a$ = INKEY$
IF a$ = " " THEN GOTO start
IF a$ = "c" THEN GOTO custom
IF a$ = CHR$(27) THEN
        CLS
        END
END IF

FOR i = 1 TO 5000
IF ve(i) = 1 THEN vage(i) = vage(i) + 2
IF vage(i) > ls AND ve(i) > 0 THEN
        ve(i) = 0
        vage(i) = 0
        CIRCLE (vx(i), vy(i)), 1, 0
END IF
IF ve(i) = 0 THEN GOTO vskip

v = v + 1
va(i) = 0
FOR j = 1 TO 200
IF ABS(vx(i) - cellx(j)) < 8 AND ABS(vy(i) - celly(j)) < 8 AND celle(j) = 1 THEN
        CIRCLE (vx(i), vy(i)), 1, 0
        vx(i) = vx(i) + cellxs(j)
        vy(i) = vy(i) + cellys(j)
        vage(i) = vage(i) - 1
        IF j > 150 THEN vage(i) = vage(i) + 3
        va(i) = 1
        cellv(j) = cellv(j) + 1
        IF INT(RND * rr) + 1 = 1 AND j < 151 THEN
                FOR k = 1 TO 5000
                        IF ve(k) = 0 THEN
                                ve(k) = 1
                                vx(k) = (INT(RND * 15) - 7) + cellx(j)
                                vy(k) = (INT(RND * 15) - 7) + celly(j)
                                'vi(k) = 1
                                vxs(k) = INT(RND * 9) - 4
                                vys(k) = INT(RND * 9) - 4
                                vage(k) = 0
                                GOTO done
                        END IF
                NEXT k
done:
        END IF
END IF
IF va(i) = 1 THEN EXIT FOR
NEXT j
IF INT(RND * 20) + 1 = 1 THEN vxs(i) = vxs(i) + INT(RND * 3) - 1
IF INT(RND * 20) + 1 = 1 THEN vys(i) = vys(i) + INT(RND * 3) - 1
IF vys(i) > 4 THEN vys(i) = 4
IF vys(i) < -4 THEN vys(i) = -4
IF vxs(i) > 4 THEN vxs(i) = 4
IF vxs(i) < -4 THEN vxs(i) = -4

IF va(i) = 0 THEN
        CIRCLE (vx(i), vy(i)), 1, 0
        vx(i) = vx(i) + vxs(i)
        vy(i) = vy(i) + vys(i)
END IF
IF vx(i) < 1 OR vx(i) > 639 THEN
        IF vx(i) < 1 THEN vx(i) = 1
        IF vy(i) > 639 THEN vy(i) = 639
        vxs(i) = -vxs(i)
END IF
IF vy(i) < 41 OR vy(i) > 479 THEN
        IF vy(i) < 41 THEN vy(i) = 41
        IF vy(i) > 479 THEN vy(i) = 479
        vys(i) = -vys(i)
END IF

CIRCLE (vx(i), vy(i)), 1, vc

vskip:
NEXT i

LOOP

custom:
CLS
INPUT "Lifespan (50 - 350) ", ls
IF ls > 350 THEN ls = 350
IF ls < 50 THEN ls = 50
INPUT "Size (3 to 20) 1/", mh
IF mh > 20 THEN mh = 20
IF mh < 3 THEN mh = 3
INPUT "Multiplying Rate (2 to 22, higher = slower) ", rr
IF rr > 22 THEN rr = 22
IF rr < 2 THEN rr = 2
GOTO start2

