RANDOMIZE TIMER
SCREEN 12
s = 9
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


FOR i = 1 TO 50
FOR j = 1 TO 50
terrain(i, j) = 2
NEXT
NEXT
lakes = 5
lakesize = 30
deserts = 1
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



FOR j = 1 TO INT(RND * 3) + 3
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
IF x < 0 THEN x = 0
IF y < 0 THEN y = 0
NEXT i
NEXT j

DO
dx = INT(RND * 50) + 1
dy = INT(RND * 50) + 1
LOOP UNTIL terrain(dx, dy) <> 1



FOR i = 1 TO 50
FOR j = 1 TO 50
LINE (i * s, j * s)-(i * s + s - 1, j * s + s - 1), terrain(i, j), BF
NEXT
NEXT
FOR i = 1 TO 50
FOR j = 1 TO 50
IF road(i, j) = 1 THEN
        FOR k = i - 1 TO i + 1
        FOR l = j - 1 TO j + 1
        IF k > 0 AND k <= 50 AND l > 0 AND l <= 50 THEN
                IF road(k, l) = 1 THEN LINE (i * s + s / 2, j * s + s / 2)-(k * s + s / 2, l * s + s / 2), 14
        END IF
        NEXT l
        NEXT k
END IF
NEXT
NEXT

CIRCLE (25 * s + s / 2, 25 * s + s / 2), 1, 12
CIRCLE (dx * s + s / 2, dy * s + s / 2), 1, 11
SLEEP
inrange(25, 25) = 20000
DO
didsomething = 0
FOR i = 1 TO 50
FOR j = 1 TO 50
IF inrange(i, j) > 1 THEN
        FOR k = i - 1 TO i + 1
        FOR l = j - 1 TO j + 1
        IF k > 0 AND k <= 50 AND l > 0 AND l <= 50 AND (k <> i OR l <> j) THEN
                IF inrange(k, l) = 0 AND terrain(k, l) <> 1 THEN
                        t = 1
                        d = 1
                        IF terrain(k, l) = 6 THEN t = 2
                        IF road(i, j) = 1 AND road(k, l) = 1 THEN d = 3
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
LOOP UNTIL didsomething = 0 OR inrange(dx, dy) > 0
IF didsomething = 0 AND inrange(dx, dy) <= 0 THEN END
inrange(25, 25) = 200000
x = dx
y = dy
DO
x2 = x
y2 = y
FOR k = x - 1 TO x + 1
FOR l = y - 1 TO y + 1
        IF k > 0 AND k <= 50 AND l > 0 AND l <= 50 THEN
                IF inrange(k, l) >= inrange(x2, y2) THEN
                        x2 = k
                        y2 = l
                END IF
        END IF
NEXT l
NEXT k
LINE (x2 * s + s / 2, y2 * s + s / 2)-(x * s + s / 2, y * s + s / 2), 15
x = x2
y = y2
LOOP UNTIL x = 25 AND y = 25

'FOR i = 1 TO 50
'FOR j = 1 TO 50
'IF inrange(i, j) > 0 THEN
'        LINE (i * s, j * s)-(i * s + s - 1, j * s + s - 1), 7, B'+ 1 + (inrange(i, j) / (100 * range / 15))
'END IF
'NEXT j
'NEXT i


