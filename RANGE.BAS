RANDOMIZE TIMER
SCREEN 12
DIM terrain(50, 50)
DIM road(50, 50)
DIM inrange(50, 50)
DIM inrangech(50, 50)

DIM xc(8)
DIM yc(8)

xc(1) = 0
xc(2) = 1
xc(3) = 1
xc(4) = 1
xc(5) = 0
xc(6) = -1
xc(7) = -1
xc(8) = -1
yc(1) = -1
yc(2) = -1
yc(3) = 0
yc(4) = 1
yc(5) = 1
yc(6) = 1
yc(7) = 0
yc(8) = -1

range = 1

FOR i = 1 TO 50
FOR j = 1 TO 50
terrain(i, j) = 2
NEXT
NEXT
lakes = INT(RND * 5)
lakesize = 30
deserts = INT(RND * 3)
desertsize = 350
typ = 1
FOR typ = 0 TO 1
IF typ = 1 THEN B = lakes
IF typ = 0 THEN B = deserts
FOR i = 1 TO B
x = INT(RND * 50) + 1
y = INT(RND * 50) + 1
IF typ = 1 THEN c = lakesize + INT(RND * 11) - 5
IF typ = 0 THEN c = desertsize + INT(RND * 11) - 5
FOR j = 1 TO c
x = x + INT(RND * 3) - 1
y = y + INT(RND * 3) - 1
IF x < 3 THEN x = 3
IF y < 3 THEN y = 3
IF x > 48 THEN x = 48
IF y > 48 THEN y = 48
FOR k = -2 TO 2: FOR l = -2 TO 2
IF ABS(k) + ABS(l) < 4 THEN
        IF typ = 1 THEN co = 1
        IF typ = 0 THEN co = 6
        terrain(x + k, y + l) = co
END IF
NEXT: NEXT
NEXT j
NEXT i
NEXT typ



FOR j = 1 TO INT(RND * 5) + 2
x = INT(RND * 50) + 1
y = INT(RND * 50) + 1
dir = INT(RND * 8) + 1
FOR i = 1 TO INT(RND * 100) + 1
IF terrain(x, y) <> 1 THEN road(x, y) = 1
dir = dir + INT(RND * 3) - 1
IF dir < 1 THEN dir = 8
IF dir > 8 THEN dir = 1
x = x + xc(dir)
y = y + yc(dir)
IF x > 50 THEN x = 50
IF y > 50 THEN y = 50
IF x < 1 THEN x = 1
IF y < 1 THEN y = 1
NEXT i
NEXT j

FOR i = 1 TO 50
FOR j = 1 TO 50
LINE (i * 8, j * 8)-(i * 8 + 7, j * 8 + 7), terrain(i, j), BF
NEXT
NEXT
FOR i = 1 TO 50
FOR j = 1 TO 50
IF road(i, j) = 1 THEN
        FOR k = i - 1 TO i + 1
        FOR l = j - 1 TO j + 1
        IF k > 0 AND k <= 50 AND l > 0 AND l <= 50 THEN
                IF road(k, l) = 1 THEN LINE (i * 8 + 4, j * 8 + 4)-(k * 8 + 4, l * 8 + 4), 14
        END IF
        NEXT l
        NEXT k
END IF
NEXT
NEXT

CIRCLE (25 * 8 + 4, 25 * 8 + 4), 2, 12
DO: LOOP UNTIL INKEY$ = CHR$(13)
start:
inrange(25, 25) = (range + 1) * 100
DO
didsomething = 0
FOR i = 1 TO 50
FOR j = 1 TO 50
IF inrange(i, j) > 1 THEN
        FOR k = i - 1 TO i + 1
        FOR l = j - 1 TO j + 1
        IF k > 0 AND k <= 50 AND l > 0 AND l <= 50 AND (k <> i OR l <> j) THEN
                IF inrange(k, l) <= 0 AND terrain(k, l) <> 1 THEN
                        t = 1
                        d = 1
                        IF terrain(k, l) = 6 THEN
                                t = t * 3
                                d = d * 2
                        END IF
                        IF road(i, j) = 1 AND road(k, l) = 1 THEN d = d * 2
                        IF (k = i OR l = j) THEN
                                inrangec = inrange(i, j) - 100 * t / d
                                IF inrangec > inrangech(k, l) THEN
                                        inrangech(k, l) = inrangec
                                        didsomething = 1
                                END IF
                        ELSE
                                inrangec = inrange(i, j) - 141 * t / d
                                IF inrangec > inrangech(k, l) THEN
                                        inrangech(k, l) = inrangec
                                        didsomething = 1
                                END IF
                        END IF
                END IF
        END IF
        NEXT
        NEXT
END IF
NEXT
NEXT
FOR i = 1 TO 50
FOR j = 1 TO 50
inrange(i, j) = inrangech(i, j)
NEXT
NEXT
LOOP UNTIL didsomething = 0

FOR i = 1 TO 50
FOR j = 1 TO 50

IF inrange(i, j) > 0 THEN LINE (i * 8, j * 8)-(i * 8 + 7, j * 8 + 7), 7, B
inrange(i, j) = 0
inrangech(i, j) = 0
NEXT j
NEXT i
LOCATE 28, 1
PRINT range
DO: LOOP UNTIL INKEY$ = CHR$(13)
range = range + 1
GOTO start


