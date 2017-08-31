RANDOMIZE TIMER
SCREEN 12
DIM c(100, 100) AS INTEGER
DIM b(5, 50) AS INTEGER

start:
CLS

LOCATE 10, 20
PRINT "Working..."

FOR i = 1 TO 50
FOR ii = 1 TO 5
b(ii, i) = 0
NEXT ii
NEXT i

t = INT(RND * 2) + 1
't = 1
FOR x = 0 TO 100
FOR y = 0 TO 100
c(x, y) = 3 - t
NEXT y
NEXT x

lakes = INT(RND * 20) + 1
lakes = lakes * t
FOR i = 1 TO lakes
lx = INT(RND * 100) + 1
ly = INT(RND * 100) + 1
ls = INT(RND * 260) + 40
ls = ls * t
c(lx, ly) = 1
FOR x = -2 TO 2
FOR y = -2 TO 2
IF lx + x <= 100 AND lx + x >= 0 AND ly + y <= 100 AND ly + y >= 0 THEN c(lx + x, ly + y) = t
NEXT y
NEXT x
x = 0
y = 0
FOR a = 1 TO ls
x = x + INT(RND * 3) - 1
y = y + INT(RND * 3) - 1
FOR x2 = -1 TO 1
FOR y2 = -1 TO 1
x3 = x + x2
y3 = y + y2
IF lx + x3 <= 100 AND lx + x3 >= 0 AND ly + y3 <= 100 AND ly + y3 >= 0 THEN c(lx + x3, ly + y3) = t
NEXT y2
NEXT x2
NEXT a
NEXT i

FOR x = 3 TO 15
FOR y = 3 TO 15
c(x, y) = 2
NEXT y
NEXT x

FOR x = 1 TO 99
FOR y = 1 TO 99
a = 0
IF c(x, y) < 2 THEN
        FOR x2 = -1 TO 1
        FOR y2 = -1 TO 1
        IF c(x + x2, y + y2) >= 2 THEN a = a + 1
        NEXT y2
        NEXT x2
        IF a > 0 THEN c(x, y) = 0
END IF
NEXT y
NEXT x

b(1, 1) = 5
b(2, 1) = 5
b(3, 1) = 1
b(4, 1) = 1000

CLS
change = 1
scx = 0
scy = 0
cx = 1
cy = 1
LINE (59, 19)-(161, 121), 7, B
LINE (60, 20)-(160, 120), 15, B
FOR y = 1 TO 99
FOR x = 1 TO 99
IF c(x, y) = 0 THEN c(x, y) = 9
PSET (x + 60, y + 20), c(x, y)
IF c(x, y) = 9 THEN c(x, y) = 0
NEXT x
NEXT y
LINE (61 + scx, 21 + scy)-(scx + 80, scy + 40), 4, B
DO
a$ = INKEY$
IF a$ = "4" THEN
        GOSUB c1
        cx = cx - 1
ELSEIF a$ = "6" THEN
        GOSUB c1
        cx = cx + 1
ELSEIF a$ = "8" THEN
        GOSUB c1
        cy = cy - 1
ELSEIF a$ = "2" THEN
        GOSUB c1
        cy = cy + 1
ELSEIF a$ = "5" THEN
        FOR i = 1 TO 50
                IF ABS((b(1, i) - scx) - (cx - scx)) < 2 AND ABS((b(2, i) - scy) - (cy - scy)) < 2 AND b(3, i) > 0 THEN
                        b(5, i) = 1
                        GOSUB building
                END IF
        NEXT i
ELSEIF a$ = "n" THEN
        VIEW
        GOTO start
ELSEIF a$ = "q" THEN
        END
END IF
IF cy > 20 THEN
        GOSUB c2
        scy = scy + 5
        cy = 20
        change = 1
ELSEIF cx > 20 THEN
        GOSUB c2
        scx = scx + 5
        cx = 20
        change = 1
ELSEIF cy < 1 THEN
        GOSUB c2
        scy = scy - 5
        cy = 1
        change = 1
ELSEIF cx < 1 THEN
        GOSUB c2
        scx = scx - 5
        cx = 1
        change = 1
END IF

VIEW
LINE (216, 16)-(623, 423), 8, B
LINE (217, 17)-(622, 422), 7, B
LINE (218, 18)-(621, 421), 7, B
LINE (219, 19)-(620, 420), 15, B
VIEW SCREEN (220, 20)-(620, 420)

LINE ((cx * 20) + 200, cy * 20)-((cx * 20) + 219, (cy * 20) + 19), 4, B
LINE ((cx * 20) + 201, (cy * 20) + 1)-((cx * 20) + 218, (cy * 20) + 18), 12, B

IF scx < 0 THEN scx = 0
IF scy < 0 THEN scy = 0
IF scx > 79 THEN scx = 79
IF scy > 79 THEN scy = 79

VIEW
LINE (61 + scx, 21 + scy)-(scx + 80, scy + 40), 4, B
VIEW SCREEN (220, 20)-(620, 420)

IF change = 1 THEN
        FOR x = 1 TO 20
        FOR y = 1 TO 20
        IF c((x + scx), (y + scy)) = 0 THEN c((x + scx), (y + scy)) = 9
        LINE ((x * 20) + 200, (y * 20))-((x * 20) + 219, (y * 20) + 19), c((x + scx), (y + scy)), BF
        IF c((x + scx), (y + scy)) = 9 THEN c((x + scx), (y + scy)) = 0

        IF c((x + scx), (y + scy)) < 2 THEN
                IF c((x + scx) + 1, (y + scy) + 1) >= 2 THEN LINE ((x * 20) + 219, (y * 20) + 19)-((x * 20) + 215, (y * 20) + 15), 14, BF
                IF c((x + scx) + 1, (y + scy) - 1) >= 2 THEN LINE ((x * 20) + 219, (y * 20) + 4)-((x * 20) + 215, (y * 20)), 14, BF
                IF c((x + scx) - 1, (y + scy) + 1) >= 2 THEN LINE ((x * 20) + 204, (y * 20) + 19)-((x * 20) + 200, (y * 20) + 15), 14, BF
                IF c((x + scx) - 1, (y + scy) - 1) >= 2 THEN LINE ((x * 20) + 204, (y * 20) + 4)-((x * 20) + 200, (y * 20)), 14, BF
              
                IF c((x + scx) + 1, (y + scy)) >= 2 THEN LINE ((x * 20) + 215, (y * 20))-((x * 20) + 219, (y * 20) + 19), 14, BF
                IF c((x + scx), (y + scy) + 1) >= 2 THEN LINE ((x * 20) + 200, (y * 20) + 15)-((x * 20) + 219, (y * 20) + 19), 14, BF
                IF c((x + scx) - 1, (y + scy)) >= 2 THEN LINE ((x * 20) + 204, (y * 20))-((x * 20) + 200, (y * 20) + 19), 14, BF
                IF c((x + scx), (y + scy) - 1) >= 2 THEN LINE ((x * 20) + 200, (y * 20) + 4)-((x * 20) + 219, (y * 20)), 14, BF
        END IF
        NEXT y
        NEXT x
        FOR i = 1 TO 50
        IF b(3, i) > 0 THEN
                n = i
                GOSUB building
        END IF
        NEXT i
END IF
change = 0
LOOP

c1:
        IF c((cx + scx), (cy + scy)) = 0 THEN c((cx + scx), (cy + scy)) = 9
        LINE ((cx * 20) + 200, (cy * 20))-((cx * 20) + 219, (cy * 20) + 19), c((cx + scx), (cy + scy)), BF
        IF c((cx + scx), (cy + scy)) = 9 THEN c((cx + scx), (cy + scy)) = 0

        IF c((cx + scx), (cy + scy)) < 2 THEN
                IF c((cx + scx) + 1, (cy + scy) + 1) = 2 THEN LINE ((cx * 20) + 219, (cy * 20) + 19)-((cx * 20) + 215, (cy * 20) + 15), 14, BF
                IF c((cx + scx) + 1, (cy + scy) - 1) = 2 THEN LINE ((cx * 20) + 219, (cy * 20) + 4)-((cx * 20) + 215, cy * 20), 14, BF
                IF c((cx + scx) - 1, (cy + scy) + 1) = 2 THEN LINE ((cx * 20) + 204, (cy * 20) + 19)-((cx * 20) + 200, (cy * 20) + 15), 14, BF
                IF c((cx + scx) - 1, (cy + scy) - 1) = 2 THEN LINE ((cx * 20) + 204, (cy * 20) + 4)-((cx * 20) + 200, cy * 20), 14, BF
             
                IF c((cx + scx) + 1, (cy + scy)) = 2 THEN LINE ((cx * 20) + 215, cy * 20)-((cx * 20) + 219, (cy * 20) + 19), 14, BF
                IF c((cx + scx), (cy + scy) + 1) = 2 THEN LINE ((cx * 20) + 200, (cy * 20) + 15)-((cx * 20) + 219, (cy * 20) + 19), 14, BF
                IF c((cx + scx) - 1, (cy + scy)) = 2 THEN LINE ((cx * 20) + 204, cy * 20)-((cx * 20) + 200, (cy * 20) + 19), 14, BF
                IF c((cx + scx), (cy + scy) - 1) = 2 THEN LINE ((cx * 20) + 200, (cy * 20) + 4)-((cx * 20) + 219, cy * 20), 14, BF
        END IF
        FOR i = 1 TO 50
        IF b(3, i) > 0 THEN
                n = i
                GOSUB building
        END IF
        NEXT i
RETURN
c2:
        VIEW
        LINE (59, 19)-(161, 121), 7, B
        LINE (60, 20)-(160, 120), 15, B
       
        FOR x = scx - 5 TO scx + 25
        FOR y = scy - 5 TO scy + 25
        IF x > 0 AND x < 100 AND y > 0 AND y < 100 THEN
                IF c(x, y) = 0 THEN c(x, y) = 9
                PSET (x + 60, y + 20), c(x, y)
                IF c(x, y) = 9 THEN c(x, y) = 0
        END IF
        NEXT y
        NEXT x
       
        'FOR x = scx TO scx + 19
        'IF c(x, scy + 19) = 0 THEN c(x, scy + 19) = 9
        'PSET (x + 461, scy + 40), c(x, scy + 19)
        'IF c(x, scy + 19) = 9 THEN c(x, scy + 19) = 0
        'NEXT x
       
        'FOR y = scy TO scy + 19
        'IF c(scx, y) = 0 THEN c(scx, y) = 9
        'PSET (scx + 461, y + 21), c(scx, y)
        'IF c(scx, y) = 9 THEN c(scx, y) = 0
        'NEXT y
        '
        'FOR y = scy TO scy + 19
        'IF c(scx + 19, y) = 0 THEN c(scx + 19, y) = 9
        'PSET (scx + 480, y + 21), c(scx + 19, y)
        'IF c(scx + 19, y) = 9 THEN c(scx + 19, y) = 0
        'NEXT y
        VIEW SCREEN (220, 20)-(620, 420)
RETURN
building:
IF b(1, n) < scx + 22 AND b(2, n) < scy + 22 AND b(1, n) > scx - 2 AND b(2, n) > scy - 2 THEN
        scx = scx * 20
        scy = scy * 20
        IF b(3, n) = 1 THEN
                LINE (((b(1, n) * 20) - 20) + 200 - scx, ((b(2, n) * 20) - 20) - scy)-(((b(1, n) * 20) + 39) + 200 - scx, ((b(2, n) * 20) + 39) - scy), 7, BF
                LINE (((b(1, n) * 20) - 13) + 200 - scx, ((b(2, n) * 20) - 13) - scy)-(((b(1, n) * 20) + 2) + 200 - scx, ((b(2, n) * 20) + 2) - scy), 8, BF
                PSET (((b(1, n) * 20) - 14) + 200 - scx, ((b(2, n) * 20) + 1) - scy), 8
                PSET (((b(1, n) * 20) + 1) + 200 - scx, ((b(2, n) * 20) - 14) - scy), 8
                LINE (((b(1, n) * 20) - 15) + 200 - scx, ((b(2, n) * 20) - 15) - scy)-((b(1, n) * 20) + 200 - scx, (b(2, n) * 20) - scy), 14, BF
                LINE (((b(1, n) * 20) + 30) + 200 - scx, ((b(2, n) * 20) - 12) - scy)-(((b(1, n) * 20) + 20) + 200 - scx, ((b(2, n) * 20) - 4) - scy), 8, BF
                PSET (((b(1, n) * 20) + 29) + 200 - scx, ((b(2, n) * 20) - 13) - scy), 8
                PSET (((b(1, n) * 20) + 19) + 200 - scx, ((b(2, n) * 20) - 5) - scy), 8
                LINE (((b(1, n) * 20) + 28) + 200 - scx, ((b(2, n) * 20) - 14) - scy)-(((b(1, n) * 20) + 18) + 200 - scx, ((b(2, n) * 20) - 6) - scy), 5, BF
                LINE (((b(1, n) * 20) + 5) + 200 - scx, ((b(2, n) * 20) + 20) - scy)-(((b(1, n) * 20) + 30) + 200 - scx, ((b(2, n) * 20) + 34) - scy), 8, BF
                PSET (((b(1, n) * 20) + 4) + 200 - scx, ((b(2, n) * 20) + 33) - scy), 8
                PSET (((b(1, n) * 20) + 29) + 200 - scx, ((b(2, n) * 20) + 19) - scy), 8
                LINE (((b(1, n) * 20) + 3) + 200 - scx, ((b(2, n) * 20) + 18) - scy)-(((b(1, n) * 20) + 28) + 200 - scx, ((b(2, n) * 20) + 32) - scy), 4, BF
                IF b(5, n) = 1 THEN
                        LINE (((b(1, n) * 20) - 20) + 200 - scx, ((b(2, n) * 20) - 20) - scy)-(((b(1, n) * 20) + 39) + 200 - scx, ((b(2, n) * 20) + 39) - scy), 15, B
                END IF
        END IF
        scx = scx / 20
        scy = scy / 20
END IF
RETURN

