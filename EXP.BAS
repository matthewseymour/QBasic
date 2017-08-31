SCREEN 12
RANDOMIZE TIMER

DIM smokex(1 TO 500) AS INTEGER
DIM smokey(1 TO 500) AS INTEGER
DIM smokeage(1 TO 500) AS INTEGER

DIM explox(1 TO 50) AS INTEGER
DIM exploy(1 TO 50) AS INTEGER
DIM explomax(1 TO 50) AS INTEGER
DIM exploage(1 TO 50) AS INTEGER

DIM rubbx(1 TO 40) AS INTEGER
DIM rubby(1 TO 40) AS INTEGER
DIM rubbxs(1 TO 40) AS INTEGER
DIM rubbys(1 TO 40) AS INTEGER
DIM rubbage(1 TO 40) AS INTEGER
x = INT(RND * 640) + 1
y = INT(RND * 480) + 1
xs = INT(RND * 5) - 2
ys = INT(RND * 5) - 2
start:
expo = 1
explox(1) = x
exploy(1) = y
e = 0
b = 0
max = INT(RND * 10) + 5
FOR i = 1 TO 50
IF exploage(i) = 0 THEN
        explox(i) = INT(RND * 20) - 10 + explox(1)
        exploy(i) = INT(RND * 20) - 10 + exploy(1)
        explomax(i) = INT(RND * 10) + 5
        exploage(i) = INT(RND * 5) + 1
END IF
e = e + 1
IF e = max THEN EXIT FOR
NEXT i

r = INT(RND * 5) + 5
FOR i = 1 TO 40
rubbx(i) = explox(1)
rubby(i) = exploy(1)
rubbage(i) = INT(RND * 15) + 1
DO
rubbxs(i) = INT(RND * 10) - 5
rubbys(i) = INT(RND * 10) - 5
LOOP UNTIL (ABS(rubbxs(i)) > 2 OR ABS(rubbys(i)) > 2)
rubbxs(i) = rubbxs(i) * 2
rubbys(i) = rubbys(i) * 2
FOR a = 1 TO 4
IF rubbx(i) < 700 AND rubbx(i) > -40 THEN rubbx(i) = rubbx(i) + rubbxs(i)
IF rubby(i) < 520 AND rubby(i) > -40 THEN rubby(i) = rubby(i) + rubbys(i)
NEXT a
b = b + 1
IF b = r THEN EXIT FOR
NEXT i


DO
CIRCLE (x, y), 1, 0
x = x + xs
y = y + ys
IF expo = 1 THEN
        x = x + xs * 4
        y = y + ys * 4
END IF
IF expo = 0 THEN CIRCLE (x, y), 1, 4
IF x > 640 THEN
        CIRCLE (x, y), 1, 0
        x = 640
        xs = -xs
ELSEIF x < 0 THEN
        CIRCLE (x, y), 1, 0
        x = 0
        xs = -xs
END IF
IF y > 480 THEN
        CIRCLE (x, y), 1, 0
        y = 480
        ys = -ys
ELSEIF y < 0 THEN
        CIRCLE (x, y), 1, 0
        y = 0
        ys = -ys
END IF
FOR i = 1 TO 40
IF rubbage(i) = 0 THEN GOTO skip3
FOR s = 1 TO 500
IF smokeage(s) = 0 THEN
        smokeage(s) = 1
        smokex(s) = rubbx(i)
        smokey(s) = rubby(i)
        GOTO finish
END IF
NEXT s
finish:
rubbage(i) = rubbage(i) + 1
IF rubbage(i) > 20 THEN rubbage(i) = 0
IF rubbx(i) < 700 AND rubbx(i) > -40 THEN rubbx(i) = rubbx(i) + rubbxs(i)
IF rubby(i) < 520 AND rubby(i) > -40 THEN rubby(i) = rubby(i) + rubbys(i)
skip3:
NEXT i

FOR s = 1 TO 500
IF smokeage(s) = 0 THEN GOTO skip2
LINE (smokex(s) - smokeage(s), smokey(s) - smokeage(s))-(smokex(s) + smokeage(s), smokey(s) + smokeage(s)), 0, BF
       
IF smokeage(s) < 6 THEN
        FOR i = 1 TO smokeage(s) * 5
                PSET (smokex(s) + (INT(RND * (smokeage(s) * 2)) - smokeage(s)), smokey(s) + (INT(RND * (smokeage(s) * 2)) - smokeage(s))), 8
        NEXT i
END IF
smokeage(s) = smokeage(s) + 1
IF smokeage(s) = 8 THEN smokeage(s) = 0
skip2:
NEXT s
FOR e = 1 TO 50
IF exploage(e) = 0 THEN GOTO skip
FOR i = 1 TO exploage(e)
co& = 14
IF i >= exploage(e) - 3 THEN co& = 12
IF i >= exploage(e) - 2 THEN co& = 4
CIRCLE (explox(e), exploy(e)), i, co&
NEXT i
exploage(e) = exploage(e) + 1
IF exploage(e) > explomax(e) THEN
        FOR i = 1 TO exploage(e)
        CIRCLE (explox(e), exploy(e)), i, 0
        NEXT i
        exploage(e) = 0
END IF
skip:

NEXT e


a$ = INKEY$
IF a$ = " " THEN GOTO start
IF a$ = "c" THEN CLS
IF a$ = "q" THEN END
IF a$ = "n" THEN
        expo = 0
        CIRCLE (x, y), 1, 0
        x = INT(RND * 640) + 1
        y = INT(RND * 480) + 1
        xs = INT(RND * 5) - 2
        ys = INT(RND * 5) - 2
END IF
LOOP















