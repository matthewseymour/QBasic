SCREEN 12
RANDOMIZE TIMER
start& = 1
c& = start&
DIM ys(1 TO 10000) AS INTEGER
DIM tx(1 TO 10000) AS INTEGER
DIM ty(1 TO 10000) AS INTEGER
DIM age(1 TO 10000) AS INTEGER
p1bx& = 50
p2bx& = 590
p1by& = 50
p2by& = 430
p1x& = p1bx&
p1y& = p1by&
p1t& = INT(RND * start& + 1)
p1rx& = 0
p1ry& = 0
p2x& = p2bx&
p2y& = p2by&
p2t& = INT(RND * start& + 1)
p2rx& = 0
p2ry& = 0


FOR i = 1 TO 10000
tx(i) = -100
age(i) = 0
NEXT i
FOR i = 1 TO start&
tx(i) = INT(RND * 640) + 1
ty(i) = INT(RND * 480) + 1
NEXT i

DO
'CIRCLE (p1x&, p1y&), 1, 0
'FOR i = 1 TO 5
'IF tx(p1t&) > p1x& THEN p1x& = p1x& + 1
'IF tx(p1t&) < p1x& THEN p1x& = p1x& - 1
'IF ty(p1t&) > p1y& THEN p1y& = p1y& + 1
'IF ty(p1t&) < p1y& THEN p1y& = p1y& - 1
'NEXT i
'CIRCLE (p1x&, p1y&), 1, 1
'IF p1x& = tx(p1t&) AND p1y& = ty(p1t&) THEN
'        IF counter& > 10000 THEN counter& = 10000
'        tx(p1t&) = -100
'        p1t& = INT(RND * counter& + 1)
'END IF

'IF (ABS(tx(p1t&) - p1x&) > 50 OR ABS(tx(p1t&) - p1x&) < -50) OR (ABS(ty(p1t&) - p1y&) > 50 OR ABS(ty(p1t&) - p1y&) < -50) THEN
'        FOR i = 1 TO 10000
'        IF ABS(tx(i) - p1x&) < 50 AND ABS(tx(i) - p1x&) > -50 AND ABS(ty(i) - p1y&) < 50 AND ABS(ty(i) - p1y&) > -50 THEN
'                p1t& = i
'        END IF
'        NEXT i
'END IF
FOR i = 1 TO 10000
IF tx(i) = -100 THEN GOTO skip
CIRCLE (tx(i), ty(i)), 1, 2
age(i) = age(i) + 1

IF age(i) > 60 THEN
        age(i) = INT(RND * 60) - 10
        FOR j = 1 TO 10000
        IF tx(j) = -100 THEN
                DO
                x& = INT(RND * 200) - 100
                y& = INT(RND * 200) - 100
                IF tx(i) + x& < 640 AND tx(i) + x& > 0 AND ty(i) + y& < 480 AND ty(i) + y& > 0 THEN EXIT DO
                LOOP
                IF POINT((tx(i) + x&), ty(i) + y&) = 0 AND tx(i) + x& < 640 AND tx(i) + x& > 0 AND ty(i) + y& < 480 AND ty(i) + y& > 0 THEN
                        age(j) = INT(RND * 60) - 10
                        c& = c& + 1
                        tx(j) = tx(i) + x& + 1
                        ty(j) = ty(i) + y&
                        LOCATE 1, 1
                        PRINT c&
                        CIRCLE (tx(j), ty(j)), 1, 2
                        GOTO skip
                END IF
        END IF
        NEXT j
        ys(i) = ys(i) + 1
        IF ys(i) = 5 THEN
                CIRCLE (tx(i), ty(i)), 1, 0
                c& = c& - 1
                LOCATE 1, 1
                PRINT c&
                ys(i) = 0
                tx(i) = -100
                age(i) = 0
        END IF
END IF

skip:
NEXT i
LOOP

