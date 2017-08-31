RANDOMIZE TIMER
SCREEN 13
s = 4
DIM terrain(50, 50)
DIM inrange(50, 50)
DIM inrangech(50, 50)

FOR i = 1 TO 50
FOR j = 1 TO 50
terrain(i, j) = INT(RND * 2) + 1
NEXT
NEXT
'lakes = 5
'lakesize = 30
'typ = 1
'FOR i = 1 TO lakes
'x = INT(RND * 50) + 1
'y = INT(RND * 50) + 1
'FOR j = 1 TO lakesize
'x = x + INT(RND * 3) - 1
'y = y + INT(RND * 3) - 1
'IF x < 3 THEN x = 3
'IF y < 3 THEN y = 3
'IF x > 48 THEN x = 48
'IF y > 48 THEN y = 48
'FOR k = -2 TO 2: FOR l = -2 TO 2
'IF ABS(k) + ABS(l) < 4 THEN
'        terrain(x + k, y + l) = 1
'END IF
'NEXT: NEXT
'NEXT j
'NEXT i

DO
dx = INT(RND * 50) + 1
dy = INT(RND * 50) + 1
LOOP UNTIL terrain(dx, dy) <> 1



FOR i = 1 TO 50
FOR j = 1 TO 50
LINE (i * s, j * s)-(i * s + s - 1, j * s + s - 1), terrain(i, j), BF
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
                        IF mode = 1 THEN
                                inrangec = inrange(i, j) - 1
                                IF inrangec > inrangech(k, l) THEN
                                        inrangech(k, l) = inrangec
                                        didsomething = 1
                                END IF
                        ELSE
                                IF (k = i OR l = j) THEN
                                        inrangec = inrange(i, j) - 10
                                        IF inrangec > inrangech(k, l) THEN
                                                inrangech(k, l) = inrangec
                                                didsomething = 1
                                        END IF
                                ELSE
                                        inrangec = inrange(i, j) - 11
                                        IF inrangec > inrangech(k, l) THEN
                                                inrangech(k, l) = inrangec
                                                didsomething = 1
                                        END IF
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


