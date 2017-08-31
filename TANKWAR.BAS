SCREEN 12
CLS
RANDOMIZE TIMER
PRINT "How many fine warriors to you wish to battle?"
INPUT "", tankn&
IF tankn& > 10000 THEN tankn& = 10000
fin& = tankn&
CLS
LINE (0, 0)-(640, 480), 6, BF
DIM tx(tankn&)  AS INTEGER
DIM ty(tankn&)  AS INTEGER
DIM tor(tankn&)  AS INTEGER
DIM tt(tankn&)  AS INTEGER
DIM col(tankn&)  AS INTEGER
DIM x(tankn&)  AS INTEGER
DIM y(tankn&)  AS INTEGER
DIM t(tankn&)  AS INTEGER
DIM h(tankn&)  AS INTEGER
DIM w(tankn&)  AS INTEGER
DIM c(tankn&)  AS INTEGER

FOR i = 1 TO tankn&
col(i) = INT(RND * 16)
IF col(i) = 6 THEN col(i) = 0
x(i) = INT(RND * 640) + 1
y(i) = INT(RND * 480) + 1
t(i) = INT(RND * tankn&) + 1
h(i) = 50
NEXT i

DO
LOCATE 1, 1
PRINT fin&
LOCATE 2, 1
IF fin& < 2 THEN PRINT "This is the only surviver."
a& = a& + 1
IF a& = 100 THEN
        LINE (0, 0)-(640, 480), 6, BF
        a& = 0
END IF

FOR i = 1 TO tankn&

IF h(i) <= 0 AND w(i) = 25 THEN GOTO finish
IF h(t(i)) <= 0 THEN
        t(i) = INT(RND * tankn&) + 1
        IF tor(i) = 1 THEN CIRCLE (tx(i), ty(i)), 1, 6
        tor(i) = 0
END IF

IF tor(i) = 1 THEN CIRCLE (tx(i), ty(i)), 1, 6
LINE (x(i) - 1, y(i) - 1)-(x(i) + 1, y(i) + 1), 6, BF
IF fin& < 2 THEN GOTO suviver
IF h(i) <= 0 THEN
        w(i) = w(i) + 1
        co& = INT(RND * 2) + 1
        s& = INT(RND * 100) + 100
        IF co& = 1 THEN co& = 4
        IF co& = 2 THEN co& = 14
        IF w(i) > 20 THEN co& = 4
        SOUND s&, .1
        CIRCLE (x(i), y(i)), w(i), co&
        IF w(i) = 25 THEN
                fin& = fin& - 1
                LINE (x(i) - 25, y(i) - 25)-(x(i) + 25, y(i) + 25), 6, BF
                tor(i) = 0
                rand& = INT(RND * 4) + 1
                IF rand& = 1 THEN
                        GOTO finish
                        x(i) = INT(RND * 640) + 1
                ELSEIF rand& = 2 THEN
                        y(i) = INT(RND * 480) + 1
                ELSEIF rand& = 3 THEN
                        x(i) = INT(RND * 640) + 1
                        y(i) = 480
                ELSEIF rand& = 4 THEN
                        y(i) = INT(RND * 480) + 1
                        x(i) = 640
                END IF
                t(i) = INT(RND * tankn&) + 1
'                h( i) = 50
'                w( i) = 0
        END IF
        GOTO finish
END IF
suviver:
IF tor(i) = 1 THEN
        IF x(t(i)) < tx(i) THEN tx(i) = tx(i) - 1
        IF x(t(i)) > tx(i) THEN tx(i) = tx(i) + 1
        IF y(t(i)) < ty(i) THEN ty(i) = ty(i) - 1
        IF y(t(i)) > ty(i) THEN ty(i) = ty(i) + 1
END IF
IF tx(i) = x(tt(i)) AND ty(i) = y(tt(i)) AND tor(i) = 1 THEN
        t(tt(i)) = i
        h(tt(i)) = h(tt(i)) - 10
        tor(i) = 0
END IF

IF x(i) - x(t(i)) > 50 OR x(i) - x(t(i)) < -50 THEN
        IF x(t(i)) < x(i) THEN x(i) = x(i) - 1
        IF x(t(i)) > x(i) THEN x(i) = x(i) + 1
END IF
IF y(i) - y(t(i)) > 50 OR y(i) - y(t(i)) < -50 THEN
        IF y(t(i)) < y(i) THEN y(i) = y(i) - 1
        IF y(t(i)) > y(i) THEN y(i) = y(i) + 1
END IF
IF c(i) > 0 THEN c(i) = c(i) - 1
IF (x(i) - x(t(i)) < 60 AND x(i) - x(t(i)) > -60) AND (y(i) - y(t(i)) < 60 AND y(i) - y(t(i)) > -60) AND c(i) = 0 AND tor(i) = 0 THEN
        c(i) = 100
        tor(i) = 1
        tt(i) = t(i)
        tx(i) = x(i)
        ty(i) = y(i)
END IF

IF tor(i) = 1 THEN CIRCLE (tx(i), ty(i)), 1, 1
LINE (x(i) - 1, y(i) - 1)-(x(i) + 1, y(i) + 1), col(i), BF

finish:

NEXT i
LOOP

