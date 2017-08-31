SCREEN 12
RANDOMIZE TIMER
c& = 1
DIM tx(1 TO 10000) AS INTEGER
DIM ty(1 TO 10000) AS INTEGER
DIM age(1 TO 10000) AS INTEGER

FOR i = 1 TO 10000
tx(i) = -100
age(i) = 0
NEXT i
tx(1) = INT(RND * 640) + 1
ty(1) = INT(RND * 480) + 1

DO

FOR i = 1 TO 10000
IF tx(i) = -100 THEN GOTO skip
CIRCLE (tx(i), ty(i)), 1, 2
age(i) = age(i) + 1

IF age(i) = 60 THEN
        age(i) = INT(RND * 60) - 10
        FOR j = 1 TO 10000
        IF tx(j) = -100 THEN
                x& = INT(RND * 20) - 10
                y& = INT(RND * 20) - 10
                x& = x& * 3
                y& = y& * 3
                IF POINT((tx(i) + x& + 1), ty(i) + y&) = 0 THEN
                        age(j) = INT(RND * 60) - 10
                        c& = c& + 1
                        tx(j) = tx(i) + x&
                        ty(j) = ty(i) + y&
                        LOCATE 1, 1
                        PRINT c&
                        CIRCLE (tx(j), ty(j)), 1, 2
                        GOTO skip
                END IF
        END IF
        NEXT j
END IF

skip:
NEXT i
LOOP

