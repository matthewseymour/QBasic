DIM col(1, tankn&) AS INTEGER
DIM x(1, tankn&) AS INTEGER
DIM y(1, tankn&) AS INTEGER
DIM t(1, tankn&) AS INTEGER
DIM h(1, tankn&) AS INTEGER
DIM w(1, tankn&) AS INTEGER
DIM g(1, tankn&) AS INTEGER
start:
SCREEN 12
CLS
RANDOMIZE TIMER
PRINT "How many fine warriors to you wish to battle?"
INPUT "", tankn&
IF tankn& > 3000 THEN tankn& = 3000
fin& = tankn&
CLS
LINE (0, 0)-(640, 480), 6, BF
REDIM col(1, tankn&) AS INTEGER
REDIM x(1, tankn&) AS INTEGER
REDIM y(1, tankn&) AS INTEGER
REDIM t(1, tankn&) AS INTEGER
REDIM h(1, tankn&) AS INTEGER
REDIM w(1, tankn&) AS INTEGER
REDIM g(1, tankn&) AS INTEGER

FOR i = 1 TO tankn&
col(1, i) = INT(RND * 16)
IF col(1, i) = 6 THEN col(1, i) = 0
x(1, i) = INT(RND * 640) + 1
y(1, i) = INT(RND * 480) + 1
t(1, i) = INT(RND * tankn&) + 1
h(1, i) = 50
g(1, i) = 1
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

IF h(1, i) <= 0 AND w(1, i) = 20 THEN GOTO finish
IF h(1, t(1, i)) <= 0 THEN
        t(1, i) = INT(RND * tankn&) + 1
END IF

LINE (x(1, i) - g(1, i) - 1, y(1, i) - g(1, i) - 1)-(x(1, i) + g(1, i) + 1, y(1, i) + g(1, i) + 1), 6, B
IF fin& < 2 THEN GOTO suviver
IF h(1, i) <= 0 THEN
        FOR ab = 1 TO 20
        w(1, i) = w(1, i) + 1
        co& = INT(RND * 2) + 1
        s& = INT(RND * 100) + 100
        IF co& = 1 THEN co& = 4
        IF co& = 2 THEN co& = 14
        IF w(1, i) > 4 THEN co& = 4
        SOUND s&, .1
        CIRCLE (x(1, i), y(1, i)), w(1, i), co&
        IF w(1, i) = 20 THEN
                fin& = fin& - 1
                LINE (x(1, i) - 25, y(1, i) - 25)-(x(1, i) + 25, y(1, i) + 25), 6, BF
                rand& = INT(RND * 4) + 1
                IF rand& = 1 THEN
                        x(1, i) = INT(RND * 640) + 1
                ELSEIF rand& = 2 THEN
                        y(1, i) = INT(RND * 480) + 1
                ELSEIF rand& = 3 THEN
                        x(1, i) = INT(RND * 640) + 1
                        y(1, i) = 480
                ELSEIF rand& = 4 THEN
                        y(1, i) = INT(RND * 480) + 1
                        x(1, i) = 640
                END IF
'                t(1, i) = INT(RND * tankn&) + 1
'                h(1, i) = 50
'                w(1, i) = 0
        END IF
        NEXT ab
        GOTO finish
END IF
suviver:
IF x(1, i) - x(1, t(1, i)) > 50 OR x(1, i) - x(1, t(1, i)) < -50 THEN
        IF x(1, t(1, i)) < x(1, i) THEN x(1, i) = x(1, i) - 1
        IF x(1, t(1, i)) > x(1, i) THEN x(1, i) = x(1, i) + 1
END IF
IF y(1, i) - y(1, t(1, i)) > 50 OR y(1, i) - y(1, t(1, i)) < -50 THEN
        IF y(1, t(1, i)) < y(1, i) THEN y(1, i) = y(1, i) - 1
        IF y(1, t(1, i)) > y(1, i) THEN y(1, i) = y(1, i) + 1
END IF
IF (x(1, i) - x(1, t(1, i)) < 60 AND x(1, i) - x(1, t(1, i)) > -60) AND (y(1, i) - y(1, t(1, i)) < 60 AND y(1, i) - y(1, t(1, i)) > -60) AND h(1, t(1, i)) > 0 THEN
        LINE (x(1, t(1, i)), y(1, t(1, i)))-(x(1, i), y(1, i)), 4
        FOR wa = 1 TO 200
        NEXT wa
        t(1, t(1, i)) = i
        h(1, t(1, i)) = h(1, t(1, i)) - g(1, i)
        LINE (x(1, t(1, i)), y(1, t(1, i)))-(x(1, i), y(1, i)), 6
        IF h(1, t(1, i)) < 1 AND fin& > 1 THEN
                g(1, i) = g(1, i) + 1
                h(1, i) = h(1, i) + 50
                t(1, i) = INT(RND * tankn&) + 1
        END IF
END IF

LINE (x(1, i) - g(1, i), y(1, i) - g(1, i))-(x(1, i) + g(1, i), y(1, i) + g(1, i)), col(1, i), BF

finish:

NEXT i
ww$ = INKEY$
IF ww$ = " " THEN GOTO start
IF ww$ = "q" THEN END

LOOP

