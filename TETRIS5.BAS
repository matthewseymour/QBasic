SCREEN 13
RANDOMIZE TIMER
FOR i = 1 TO 160
PALETTE i, 0
NEXT i
DIM scores(5) AS INTEGER
DIM pname(5) AS STRING
DIM ox(7, 4, 4) AS INTEGER
DIM oy(7, 4, 4) AS INTEGER
DIM c(12, -4 TO 23)
DIM o(12, -4 TO 23)
DIM check(16) AS INTEGER
DIM alch(160) AS INTEGER
DIM co(4) AS INTEGER
DIM dr(7) AS STRING
DIM p(5) AS INTEGER
DIM r(5) AS INTEGER
DIM x(5) AS SINGLE
DIM y(5) AS INTEGER
DIM stage(1024) AS INTEGER
DIM sl(1024) AS INTEGER
DIM dsl(1024) AS INTEGER
DIM gd(1024) AS INTEGER
DIM dgd(1024) AS INTEGER
DIM ppbg(3200) AS INTEGER
DIM hpbg(3200) AS INTEGER
DIM bg(1200) AS INTEGER
DIM pbg(-490 TO 10829) AS INTEGER
DIM pi(701) AS INTEGER
DIM spi(701) AS INTEGER
DIM sbg(1200) AS INTEGER
DIM drop(49) AS INTEGER
DIM twist(49) AS INTEGER
DIM gameover(384) AS INTEGER
DIM gameoverm(384) AS INTEGER
DIM gameoverbg(384) AS INTEGER
OPEN "dat2.dat" FOR INPUT AS #1
FOR i = 1 TO 5
INPUT #1, scores(i)
INPUT #1, pname(i)
NEXT i
CLOSE

DATA 0,0,1,1,1,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,1,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0,0
DATA 0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,1,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0
DATA 1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0
DATA 1,0,0,0,1,1,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,0,0
DATA 1,0,0,0,0,0,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0
DATA 0,1,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0
DATA 0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,1,1,1,1,1,1,0,1,0,0,0,0,1,0,0

FOR y = 0 TO 6
FOR x = 0 TO 64
READ a
PSET (x, y), a * 20
PSET (x, y + 7), a * 20
IF a = 0 THEN PSET (x, y + 7), 255
NEXT x
NEXT y
GET (0, 0)-(64, 6), gameover
GET (0, 7)-(64, 13), gameoverm

FOR j = 0 TO 6
FOR i = 1 TO 12
a = i
b = i - 5
IF a > 6 THEN a = 6
IF b < 1 THEN b = 1
LINE (a, b)-(b, a), 15 - i + (j * 20)
NEXT i
LINE (7, 7)-(7, 0), 2 + (j * 20)
LINE (7, 7)-(0, 7), 2 + (j * 20)
LINE (0, 0)-(6, 0), 20 + (j * 20)
LINE (0, 0)-(0, 6), 20 + (j * 20)
PSET (0, 7), 11 + (j * 20)
PSET (7, 0), 11 + (j * 20)
GET (0, 0)-(7, 7), pi(49 * j + 1)
NEXT j

FOR j = 0 TO 6
FOR i = 1 TO 12
a = i
b = i - 5
IF a > 6 THEN a = 6
IF b < 1 THEN b = 1
LINE (a, b)-(b, a), (15 - i) / 2 + (j * 20)
NEXT i
LINE (7, 7)-(7, 0), 1 + (j * 20)
LINE (7, 7)-(0, 7), 1 + (j * 20)
LINE (0, 0)-(6, 0), 10 + (j * 20)
LINE (0, 0)-(0, 6), 10 + (j * 20)
PSET (0, 7), 6 + (j * 20)
PSET (7, 0), 6 + (j * 20)
GET (0, 0)-(7, 7), spi(49 * j + 1)
NEXT j

LINE (0, 0)-(0, 31), 150
LINE (31, 31)-(31, 0), 142
LINE (31, 31)-(0, 31), 142
LINE (0, 0)-(31, 0), 150
LINE (2, 1)-(1, 2), 160
PSET (1, 1), 160
FOR i = 3 TO 30
LINE (i, 1)-(1, i), 160 - INT(i / 3) + 1
NEXT i
FOR i = 1 TO 30
LINE (30, i)-(i, 30), 160 - INT((i + 29) / 3) + 1
NEXT i
PSET (30, 30), 142
a = 0
FOR x = 0 TO 31
FOR y = 0 TO 31
a = a + 1
dgd(a) = POINT(x, y)
NEXT y
NEXT x

GET (0, 0)-(31, 31), gd
LINE (0, 0)-(0, 31), 10
LINE (31, 31)-(31, 0), 2
LINE (31, 31)-(0, 31), 2
LINE (0, 0)-(31, 0), 10
LINE (2, 1)-(1, 2), 19
PSET (1, 1), 20
FOR i = 3 TO 30
LINE (i, 1)-(1, i), 20 - INT(i / 3) + 1
NEXT i
FOR i = 1 TO 30
LINE (30, i)-(i, 30), 20 - INT((i + 29) / 3) + 1
NEXT i
PSET (30, 30), 2
a = 0
FOR x = 0 TO 31
FOR y = 0 TO 31
a = a + 1
dsl(a) = POINT(x, y)
NEXT y
NEXT x

GET (0, 0)-(31, 31), sl
CLS

FOR i = 1 TO 3 STEP 2
ox(1, i, 1) = 0: ox(1, i, 2) = 0: ox(1, i, 3) = -1: ox(1, i, 4) = -1: oy(1, i, 1) = -1: oy(1, i, 2) = 0: oy(1, i, 3) = 0: oy(1, i, 4) = 1: ox(1, i + 1, 1) = -1: ox(1, i + 1, 2) = 0: ox(1, i + 1, 3) = 0: ox(1, i + 1, 4) = 1: oy(1, i + 1, 1) = 0: oy(1 _
, i + 1, 2) = 0: oy(1, i + 1, 3) = 1: oy(1, i + 1, 4) = 1
NEXT i
FOR i = 1 TO 3 STEP 2
ox(2, i, 1) = 0: ox(2, i, 2) = 0: ox(2, i, 3) = 1: ox(2, i, 4) = 1: oy(2, i, 1) = -1: oy(2, i, 2) = 0: oy(2, i, 3) = 0: oy(2, i, 4) = 1: ox(2, i + 1, 1) = 1: ox(2, i + 1, 2) = 0: ox(2, i + 1, 3) = 0: ox(2, i + 1, 4) = -1: oy(2, i + 1, 1) = 0: oy(2,  _
i + 1, 2) = 0: oy(2, i + 1, 3) = 1: oy(2, i + 1, 4) = 1
NEXT i
FOR i = 1 TO 3 STEP 2
ox(3, i, 1) = 0: ox(3, i, 2) = 0: ox(3, i, 3) = 0: ox(3, i, 4) = 0: oy(3, i, 1) = -1: oy(3, i, 2) = 0: oy(3, i, 3) = 1: oy(3, i, 4) = 2: ox(3, i + 1, 1) = -1: ox(3, i + 1, 2) = 0: ox(3, i + 1, 3) = 1: ox(3, i + 1, 4) = 2: oy(3, i + 1, 1) = 0: oy(3,  _
i + 1, 2) = 0: oy(3, i + 1, 3) = 0: oy(3, i + 1, 4) = 0
NEXT i
FOR i = 1 TO 4
ox(4, i, 1) = 0: ox(4, i, 2) = 1: ox(4, i, 3) = 0: ox(4, i, 4) = 1: oy(4, i, 1) = 0: oy(4, i, 2) = 0: oy(4, i, 3) = 1: oy(4, i, 4) = 1
NEXT i
ox(5, 1, 1) = 0: ox(5, 1, 2) = 0: ox(5, 1, 3) = 0: ox(5, 1, 4) = 1: oy(5, 1, 1) = -1: oy(5, 1, 2) = 0: oy(5, 1, 3) = 1: oy(5, 1, 4) = 1: ox(5, 2, 1) = -1: ox(5, 2, 2) = -1: ox(5, 2, 3) = 0: ox(5, 2, 4) = 1: oy(5, 2, 1) = 1: oy(5, 2, 2) = 0: oy(5, 2 _
, 3) = 0: oy(5, 2, 4) = 0: ox(5, 3, 1) = -1: ox(5, 3, 2) = 0: ox(5, 3, 3) = 0: ox(5, 3, 4) = 0: oy(5, 3, 1) = -1: oy(5, 3, 2) = -1: oy(5, 3, 3) = 0: oy(5, 3, 4) = 1: ox(5, 4, 1) = -1: ox(5, 4, 2) = 0: ox(5, 4, 3) = 1: ox(5, 4, 4) = 1: oy(5, 4, 1) =  _
0: oy(5, 4, 2) = 0: oy(5, 4, 3) = 0: oy(5, 4, 4) = -1
ox(6, 1, 1) = 0: ox(6, 1, 2) = 0: ox(6, 1, 3) = 0: ox(6, 1, 4) = -1: oy(6, 1, 1) = -1: oy(6, 1, 2) = 0: oy(6, 1, 3) = 1: oy(6, 1, 4) = 1: ox(6, 2, 1) = -1: ox(6, 2, 2) = -1: ox(6, 2, 3) = 0: ox(6, 2, 4) = 1: oy(6, 2, 1) = -1: oy(6, 2, 2) = 0: oy(6,  _
2, 3) = 0: oy(6, 2, 4) = 0: ox(6, 3, 1) = 1: ox(6, 3, 2) = 0: ox(6, 3, 3) = 0: ox(6, 3, 4) = 0: oy(6, 3, 1) = -1: oy(6, 3, 2) = -1: oy(6, 3, 3) = 0: oy(6, 3, 4) = 1: ox(6, 4, 1) = -1: ox(6, 4, 2) = 0: ox(6, 4, 3) = 1: ox(6, 4, 4) = 1: oy(6, 4, 1) =  _
0: oy(6, 4, 2) = 0: oy(6, 4, 3) = 0: oy(6, 4, 4) = 1
ox(7, 1, 1) = -1: ox(7, 1, 2) = 0: ox(7, 1, 3) = 0: ox(7, 1, 4) = 1: oy(7, 1, 1) = 0: oy(7, 1, 2) = 0: oy(7, 1, 3) = 1: oy(7, 1, 4) = 0: ox(7, 2, 1) = 0: ox(7, 2, 2) = -1: ox(7, 2, 3) = 0: ox(7, 2, 4) = 0: oy(7, 2, 1) = -1: oy(7, 2, 2) = 0: oy(7, 2 _
, 3) = 0: oy(7, 2, 4) = 1: ox(7, 3, 1) = -1: ox(7, 3, 2) = 0: ox(7, 3, 3) = 0: ox(7, 3, 4) = 1: oy(7, 3, 1) = 0: oy(7, 3, 2) = 0: oy(7, 3, 3) = -1: oy(7, 3, 4) = 0: ox(7, 4, 1) = 0: ox(7, 4, 2) = 0: ox(7, 4, 3) = 1: ox(7, 4, 4) = 0: oy(7, 4, 1) = -1 _
: oy(7, 4, 2) = 0: oy(7, 4, 3) = 0: oy(7, 4, 4) = 1

FOR i = 0 TO 20: PALETTE i, i * 3 + 256 * (i * 3) + 65536 * (i * 3): NEXT i
FOR i = 21 TO 40: PALETTE i, 65536 * ((i - 20) * 3): NEXT i
FOR i = 41 TO 60: PALETTE i, 256 * ((i - 40) * 3): NEXT i
FOR i = 61 TO 80: PALETTE i, 256 * ((i - 60) * 3) + 65536 * ((i - 60) * 3): NEXT i
FOR i = 81 TO 100: PALETTE i, ((i - 80) * 3): NEXT i
FOR i = 101 TO 120: PALETTE i, ((i - 100) * 3) + ((i - 100) * 3) * 65536: NEXT i
FOR i = 121 TO 140: PALETTE i, ((i - 119) * 3) + ((i - 121) * 3) * 256: NEXT i
FOR i = 141 TO 160: PALETTE i, ((i - 140) * 3) + INT((i - 140) * 1.5) * 256: NEXT i
GOTO menu

start:
OPEN "dat5.dat" FOR INPUT AS #1
FOR x = 0 TO 319
FOR y = 0 TO 199
INPUT #1, a
PSET (x, y), a
NEXT y
NEXT x
CLOSE
i = INT(RND * 8)

FOR y = -1 TO 21
FOR x = 1 TO 10
GET (30 + 8 * x, 16 + 8 * y)-(37 + 8 * x, 23 + 8 * y), pbg(49 * (10 * y + x))
NEXT x
NEXT y
lines& = 0
score& = 0
level = 150
FOR x = 0 TO 11
FOR y = -4 TO 21
c(x, y) = 0
o(x, y) = 0
NEXT y
NEXT x

FOR i = 0 TO 11
c(i, 21) = 1
NEXT i
FOR i = -4 TO 21
c(0, i) = 1
c(11, i) = 1
NEXT i
p(5) = INT(RND * 7) + 1
r(5) = 1
GET (6, 6)-(31, 39), hpbg
4 PUT (15 + ox(p(5), 1, 1) * 8, 15 + oy(p(5), 1, 1) * 8), pi(49 * (p(5) - 1) + 1), PSET
PUT (15 + ox(p(5), 1, 2) * 8, 15 + oy(p(5), 1, 2) * 8), pi(49 * (p(5) - 1) + 1), PSET
PUT (15 + ox(p(5), 1, 3) * 8, 15 + oy(p(5), 1, 3) * 8), pi(49 * (p(5) - 1) + 1), PSET
PUT (15 + ox(p(5), 1, 4) * 8, 15 + oy(p(5), 1, 4) * 8), pi(49 * (p(5) - 1) + 1), PSET

FOR i = 2 TO 4
p(i) = INT(RND * 7) + 1
IF p(i) = 7 THEN r(i) = 1 ELSE r(i) = 2
NEXT i
GET (127, 22)-(159, 121), ppbg
GOSUB newpiece
GET (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 21 + (y(1) + oy(p(1), r(1), 1)) * 8)-(38 + (x(1) + ox(p(1), r(1), 1)) * 8, 29 + (y(1) + oy(p(1), r(1), 1)) * 8), bg(0)
GET (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 21 + (y(1) + oy(p(1), r(1), 2)) * 8)-(38 + (x(1) + ox(p(1), r(1), 2)) * 8, 29 + (y(1) + oy(p(1), r(1), 2)) * 8), bg(49)
GET (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 21 + (y(1) + oy(p(1), r(1), 3)) * 8)-(38 + (x(1) + ox(p(1), r(1), 3)) * 8, 29 + (y(1) + oy(p(1), r(1), 3)) * 8), bg(98)
GET (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 21 + (y(1) + oy(p(1), r(1), 4)) * 8)-(38 + (x(1) + ox(p(1), r(1), 4)) * 8, 29 + (y(1) + oy(p(1), r(1), 4)) * 8), bg(147)
number& = 0
noswitch = 0
col = INT(RND * 6) + 1
startt = TIMER

DO
GOSUB clearsp
IF desfall > 0 THEN move = move + INT(level / 2): desfall = desfall - 1
IF desfall2 > 0 THEN move = level: desfall2 = desfall2 - 1
move = move + 1
IF move > level THEN move = level

GOSUB redraw
IF move >= level THEN
        GOSUB clearsp
        move = level
        a = 0
        b = 0
        FOR i = 1 TO 4
        IF c(x(1) + ox(p(1), r(1), i), y(1) + (oy(p(1), r(1), i) + 1)) <> 0 THEN a = a + 1
        NEXT i
        IF a = 0 THEN slide = 20
        IF desfall2 > 0 OR desfall > 0 THEN slide = 10
        IF a > 0 THEN slide = slide - 1
        IF slide > 0 AND a > 0 THEN
                desfall2 = 0
                desfall = 0
                'move = level
                GOSUB redraw
                GOTO slideskip
        END IF
        IF a > 0 THEN
                GOSUB redraw
                desfall = 0
                desfall2 = 0
                GOSUB twistcheck
                FOR i = 1 TO 4
                co = p(1)
                IF co = 6 THEN co = 14
                c(x(1) + ox(p(1), r(1), i), y(1) + oy(p(1), r(1), i)) = co
                o(x(1) + ox(p(1), r(1), i), y(1) + oy(p(1), r(1), i)) = number&
                NEXT i
                GOSUB scheck
supercheck:   
                FOR x3 = 5 TO 1 STEP -1
                subb = 0
                FOR y = 0 TO 20
                comm = 0
                FOR x = 1 TO 10
                IF c(x, y) <> 0 THEN
                        comm = comm + 1
                END IF
                NEXT x
                IF comm = 10 THEN
                        IF x3 = 5 THEN
                                FOR x4 = 1 TO 10
                                IF c(x4, y - 1) = 6 THEN
                                        LINE (30 + x4 * 8, 23 + y * 8)-(37 + x4 * 8, 23 + y * 8), 142
                                ELSEIF c(x4, y - 1) = 8 THEN
                                        LINE (30 + x4 * 8, 23 + y * 8)-(37 + x4 * 8, 23 + y * 8), 2
                                END IF
                                IF c(x4, y + 1) = 6 THEN
                                        LINE (30 + x4 * 8, 32 + y * 8)-(37 + x4 * 8, 32 + y * 8), 150
                                ELSEIF c(x4, y + 1) = 8 THEN
                                        LINE (30 + x4 * 8, 32 + y * 8)-(37 + x4 * 8, 32 + y * 8), 10
                                END IF
                                NEXT x4
                        END IF
                        subb = subb + 1
                        FOR y2 = y TO 0 + subb STEP -1
                        FOR i = 1 TO 2
                        IF i = 2 THEN x2 = 11 - x3 ELSE x2 = x3
                        PUT (30 + x2 * 8, 16 + (y2 + 1) * 8), pbg(49 * (10 * (y2 + 1) + x2)), PSET
                        IF c(x2, y2 - subb) > 0 THEN
                                GET (30 + x2 * 8, 16 + y2 * 8)-(37 + x2 * 8, 23 + y2 * 8), drop
                                PUT (30 + x2 * 8, 16 + (y2 + 1) * 8), drop, PSET
                                PUT (30 + x2 * 8, 16 + y2 * 8), pbg(49 * (10 * y2 + x2)), PSET
                        END IF
                        NEXT i
                        FOR delay = 1 TO 1000
                        NEXT delay
                        NEXT y2
                END IF
                NEXT y
                NEXT x3
check:                         
                FOR y = 0 TO 20
                comm = 0
                FOR x = 1 TO 10
                IF c(x, y) <> 0 THEN
                        comm = comm + 1
                END IF
                NEXT x
                IF comm = 10 THEN
                        IF twisted = 1 THEN twisted = 2
                        square = 0
                        FOR x2 = 1 TO 10
                        IF c(x2, y) = 8 THEN score& = score& + 1: square = 1
                        IF c(x2, y) = 6 THEN score& = score& + 2: square = 1
                        c(x2, y) = 0
                        NEXT x2
                        FOR y2 = y TO 0 STEP -1
                        FOR x2 = 1 TO 10
                        c(x2, y2) = c(x2, y2 - 1)
                        o(x2, y2) = o(x2, y2 - 1)
                        NEXT x2
                        NEXT y2
                        linedes = linedes + 1
                        lines = lines + 1
                        score& = score& + 1
                        IF level > 1 THEN level = level - (level / 50)
                        comm = 0
                        GOTO check
                END IF
                NEXT y
                IF linedes >= 4 THEN score& = score& + 1 + (4 * square)
                linedes = 0
                GOSUB newpiece
                IF twisted = 2 THEN GOTO fall
                twisted = 0
        END IF
        move = 0
        IF a = 0 THEN y(1) = y(1) + 1: GOSUB redraw
slideskip:
END IF

FOR i = 1 TO 10
IF c(i, 0) > 0 THEN
        a = 2001: b = -801: yy = 8: y = -1: ydir = 2: num = 1
        FOR x = 10 TO 1 STEP -1
        PUT (30 + x * 8, 8), pbg(49 * (-10 + x)), PSET
        NEXT x
        GET (46, yy)-(110, yy + 6), gameoverbg
        DO
        a = a + 1
        IF a > 150 AND y < 21 THEN
                a = 0
                y = y + 1
                FOR x = 10 TO 1 STEP -1
                PUT (30 + x * 8, 16 + y * 8), pbg(49 * (10 * y + x)), PSET
                NEXT x
        END IF
        FOR i = 1 TO 250
        NEXT i
        c = c + 1
        IF c > 81 THEN
                c = 0
                PUT (46, yy), gameoverbg, PSET
                yy = yy + ydir
                GET (46, yy)-(110, yy + 6), gameoverbg
                PUT (46, yy), gameoverm, AND
                PUT (46, yy), gameover, OR
                IF yy > 140 AND num = 1 THEN ydir = 1: num = 2
                IF yy > 150 AND num = 2 THEN ydir = -1: num = 3
                IF yy < 140 AND num = 3 THEN ydir = -2: num = 4
                IF yy < 90 AND num = 4 THEN ydir = -1: num = 5
                IF yy < 80 AND num = 5 THEN ydir = 1: num = 6
                IF yy > 90 AND num = 6 THEN ydir = 2: num = 7
                IF yy > 110 AND num = 7 THEN ydir = 1: num = 8
                IF yy > 120 AND num = 8 THEN ydir = -1: num = 9
                IF yy < 110 AND num = 9 THEN ydir = -2: num = 4
        END IF
        LOOP UNTIL y > 20 AND INKEY$ <> ""
        GOTO scorecheck
END IF
NEXT i

a = 0
IF score& >= 10 THEN a = a + 1
IF score& >= 100 THEN a = a + 1
IF score& >= 1000 THEN a = a + 1
COLOR 20: LOCATE 20, 25 - a: PRINT score&

current = TIMER
sec = current - startt
IF sec < 0 THEN GOTO skiptime
min = 0
DO
IF sec >= 60 THEN sec = sec - 60: min = min + 1 ELSE EXIT DO
LOOP
IF sec < 10 THEN LOCATE 22, 25: PRINT INT(sec) ELSE LOCATE 22, 24: PRINT INT(sec)
IF min < 10 THEN LOCATE 22, 22: PRINT INT(min)
IF min >= 10 AND min < 100 THEN LOCATE 22, 21: PRINT INT(min)
IF min >= 100 THEN LOCATE 22, 20: PRINT INT(min)
IF sec > 10 THEN LOCATE 22, 24: PRINT ":" ELSE LOCATE 22, 24: PRINT ":0"
skiptime:
scan = INP(96)
IF scan = 80 THEN desfall = 1'ELSEIF a$ = CHR$(0) + "P" THEN desfall = 8
a$ = INKEY$
IF a$ = CHR$(27) THEN GOTO menu
IF scan = 75 THEN 'a$ = CHR$(0) + "K" THEN
        num = 0
        FOR i = 1 TO 4
        IF c(x(1) + ox(p(1), r(1), i) - 1, y(1) + (oy(p(1), r(1), i))) <> 0 THEN num = num + 1
        NEXT i
        IF num = 0 THEN
                IF slide < 20 THEN slide = slide + 1
                FOR k = 1 TO 5
                GOSUB clearsp
                x(1) = x(1) - .2
                GOSUB redraw
                NEXT k
        END IF
ELSEIF scan = 77 THEN 'a$ = CHR$(0) + "M" THEN
        num = 0
        FOR i = 1 TO 4
        IF c(x(1) + ox(p(1), r(1), i) + 1, y(1) + (oy(p(1), r(1), i))) <> 0 THEN num = num + 1
        NEXT i
        IF num = 0 THEN
                IF slide < 20 THEN slide = slide + 1
                FOR k = 1 TO 5
                GOSUB clearsp
                x(1) = x(1) + .2
                GOSUB redraw
                NEXT k
        END IF
END IF
IF a$ = CHR$(0) + "H" AND slide = 20 THEN
        GOSUB clearsp
        a = 0
        FOR j = y(1) TO 21
        FOR i = 1 TO 4
        IF c(x(1) + ox(p(1), r(1), i), j + (oy(p(1), r(1), i) + 1)) <> 0 THEN a = a + 1
        NEXT i
        IF a >= 1 THEN yy = j: EXIT FOR
        NEXT j
        y(1) = yy
        move = level - 1
        slide = 10
        GOSUB redraw
ELSEIF a$ = "z" THEN
        dr = r(1) + 1
        IF dr > 4 THEN dr = dr - 4
        y = y(1)
        FOR y2 = 1 TO 0 STEP -1
        num = 0
        FOR i = 1 TO 4
        IF c(x(1) + ox(p(1), dr, i), y(1) + y2 + (oy(p(1), dr, i))) <> 0 THEN num = num + 1
        NEXT i
        IF num = 0 THEN
                GOSUB clearsp
                r(1) = dr
                y(1) = y + y2
                GOSUB redraw
        END IF
        NEXT y2
ELSEIF a$ = "x" THEN
        dr = r(1) - 1
        IF dr < 1 THEN dr = dr + 4
        y = y(1)
        FOR y2 = 1 TO 0 STEP -1
        num = 0
        FOR i = 1 TO 4
        IF c(x(1) + ox(p(1), dr, i), y(1) + y2 + (oy(p(1), dr, i))) <> 0 THEN num = num + 1
        NEXT i
        IF num = 0 THEN
                GOSUB clearsp
                r(1) = dr
                y(1) = y + y2
                GOSUB redraw
        END IF
        NEXT y2
ELSEIF a$ = " " AND noswitch = 0 THEN
        noswitch = 1
        desfall = 0
        desfall2 = 0
        GOSUB clearsp
        PUT (6, 6), hpbg, PSET
        SWAP p(1), p(5)
        GET (6, 6)-(31, 39), hpbg
        PUT (15 + ox(p(5), 1, 1) * 8, 15 + oy(p(5), 1, 1) * 8), pi(49 * (p(5) - 1) + 1), PSET
        PUT (15 + ox(p(5), 1, 2) * 8, 15 + oy(p(5), 1, 2) * 8), pi(49 * (p(5) - 1) + 1), PSET
        PUT (15 + ox(p(5), 1, 3) * 8, 15 + oy(p(5), 1, 3) * 8), pi(49 * (p(5) - 1) + 1), PSET
        PUT (15 + ox(p(5), 1, 4) * 8, 15 + oy(p(5), 1, 4) * 8), pi(49 * (p(5) - 1) + 1), PSET
        x(1) = 5
        IF p(1) = 3 OR p(1) = 6 THEN y(1) = 0 ELSE y(1) = -1
        IF p(1) = 7 THEN r(1) = 1 ELSE r(1) = 2
        GOSUB redraw
ELSEIF a$ = "p" THEN
        DO: LOOP UNTIL INKEY$ = "p"
END IF
LOOP

newpiece:
noswitch = 0
PUT (127, 22), ppbg, PSET
FOR i = 1 TO 3
p(i) = p(i + 1)
r(i) = r(i + 1)
NEXT i
p(4) = INT(RND * 7) + 1
IF p(4) = 7 THEN r(4) = 1 ELSE r(4) = 2
FOR i = 1 TO 4
PUT (135 + ox(p(2), r(2), i) * 8, 30 + oy(p(2), r(2), i) * 8), pi(49 * (p(2) - 1) + 1), PSET
PUT (135 + ox(p(3), r(3), i) * 8, 60 + oy(p(3), r(3), i) * 8), pi(49 * (p(3) - 1) + 1), PSET
PUT (135 + ox(p(4), r(4), i) * 8, 90 + oy(p(4), r(4), i) * 8), pi(49 * (p(4) - 1) + 1), PSET
NEXT i
x(1) = 5
IF p(1) = 6 THEN y(1) = 0 ELSE y(1) = -1
move = (level / 3) * 2
GET (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (y(1) + oy(p(1), r(1), 1)) * 8)-(38 + (x(1) + ox(p(1), r(1), 1)) * 8, 24 + (y(1) + oy(p(1), r(1), 1)) * 8), bg(0)
GET (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (y(1) + oy(p(1), r(1), 2)) * 8)-(38 + (x(1) + ox(p(1), r(1), 2)) * 8, 24 + (y(1) + oy(p(1), r(1), 2)) * 8), bg(49)
GET (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (y(1) + oy(p(1), r(1), 3)) * 8)-(38 + (x(1) + ox(p(1), r(1), 3)) * 8, 24 + (y(1) + oy(p(1), r(1), 3)) * 8), bg(98)
GET (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (y(1) + oy(p(1), r(1), 4)) * 8)-(38 + (x(1) + ox(p(1), r(1), 4)) * 8, 24 + (y(1) + oy(p(1), r(1), 4)) * 8), bg(147)
a = 0
FOR j = y(1) TO 21
FOR i = 1 TO 4
IF c(x(1) + ox(p(1), r(1), i), j + (oy(p(1), r(1), i) + 1)) <> 0 THEN a = a + 1
NEXT i
IF a >= 1 THEN yy = j + 1: EXIT FOR
NEXT j
GET (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (yy + oy(p(1), r(1), 1)) * 8)-(38 + (x(1) + ox(p(1), r(1), 1)) * 8, 24 + (yy + oy(p(1), r(1), 1)) * 8), sbg(0)
GET (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (yy + oy(p(1), r(1), 2)) * 8)-(38 + (x(1) + ox(p(1), r(1), 2)) * 8, 24 + (yy + oy(p(1), r(1), 2)) * 8), sbg(49)
GET (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (yy + oy(p(1), r(1), 3)) * 8)-(38 + (x(1) + ox(p(1), r(1), 3)) * 8, 24 + (yy + oy(p(1), r(1), 3)) * 8), sbg(98)
GET (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (yy + oy(p(1), r(1), 4)) * 8)-(38 + (x(1) + ox(p(1), r(1), 4)) * 8, 24 + (yy + oy(p(1), r(1), 4)) * 8), sbg(147)

slide = 20
number& = number& + 1
RETURN


scheck:
FOR i = 1 TO 4
co(i) = 0
NEXT i
FOR i = 1 TO 16
check(i) = 0
NEXT i

FOR y1 = 0 TO 17
FOR x1 = 1 TO 7
IF c(x1, y1) = 0 THEN GOTO skip
a = 0
FOR y2 = y1 TO y1 + 3
FOR x2 = x1 TO x1 + 3
a = a + 1
IF c(x2, y2) = 0 OR c(x2, y2) = 6 OR c(x2, y2) = 8 THEN GOTO skip
check(a) = o(x2, y2)
NEXT x2
NEXT y2
a = 0
c = 0
FOR i = 1 TO 16
a = check(i)
IF a = -1 THEN GOTO skip2
b = 0
FOR j = 1 TO 16
IF check(j) = a THEN b = b + 1: check(j) = -1
NEXT j
IF b = 4 THEN c = c + 1
skip2:
NEXT i
IF c = 4 THEN
        typ = 6
        c = c(x1, y1)
        FOR y2 = y1 TO y1 + 3
        FOR x2 = x1 TO x1 + 3
        IF c(x2, y2) <> c THEN typ = 8
        NEXT x2
        NEXT y2
        FOR i = 0 TO 1024
        stage(i) = 1
        NEXT i
        FOR i = 1 TO 40
        b = 0
        FOR j = 1 TO 160
        alch(j) = 0
        NEXT j
        FOR x2 = 30 + x1 * 8 TO 61 + x1 * 8
        FOR y2 = 24 + y1 * 8 TO 55 + y1 * 8
        b = b + 1
        a = POINT(x2, y2)
        IF stage(b) = 2 THEN
                IF typ = 8 THEN
                        IF dsl(b) < a THEN PSET (x2, y2), a - 1
                        IF dsl(b) > a THEN PSET (x2, y2), a + 1
                ELSEIF typ = 6 THEN
                        IF dgd(b) < a THEN PSET (x2, y2), a - 1
                        IF dgd(b) > a THEN PSET (x2, y2), a + 1
                END IF
        END IF
        IF (a = 160 OR a = 140 OR a = 120 OR a = 100 OR a = 80 OR a = 60 OR a = 40 OR a = 20) AND stage(b) = 1 THEN
                IF typ = 8 THEN PSET (x2, y2), 20
                IF typ = 6 THEN PSET (x2, y2), 160
                stage(b) = 2
        END IF
        IF stage(b) = 1 THEN PSET (x2, y2), a + 1
        NEXT y2
        NEXT x2
        NEXT i
        IF typ = 8 THEN PUT (30 + x1 * 8, 24 + y1 * 8), sl, PSET
        IF typ = 6 THEN PUT (30 + x1 * 8, 24 + y1 * 8), gd, PSET
        FOR y2 = y1 TO y1 + 3
        FOR x2 = x1 TO x1 + 3
        c(x2, y2) = typ
        NEXT x2
        NEXT y2
END IF
skip:
NEXT x1
NEXT y1
RETURN

twistcheck:
y = y(1) - 1
a = 0
FOR i = 1 TO 4
IF c(x(1) + ox(p(1), r(1), i), y + (oy(p(1), r(1), i))) <> 0 THEN a = a + 1
NEXT i
IF a > 0 THEN
        a = 0
        b = x(1) - 1
        c = x(1) + 1
        IF b < 1 THEN b = 1
        IF c > 10 THEN c = 10
        FOR x = b TO c
        FOR i = 1 TO 4
        IF c(x + ox(p(1), r(1), i), y(1) + (oy(p(1), r(1), i) + 1)) <> 0 THEN a = a + 1
        NEXT i
        NEXT x
        IF a <> 0 THEN twisted = 1
END IF
RETURN

fall:
changer = 1
FOR x2 = 1 TO 10
FOR y2 = 20 TO 0 STEP -1
IF c(x2, y2) = 8 THEN
        LINE (30 + x2 * 8, 16 + (y2 + 1) * 8)-(30 + x2 * 8, 23 + (y2 + 1) * 8), 10
        LINE (30 + x2 * 8, 23 + (y2 + 1) * 8)-(37 + x2 * 8, 23 + (y2 + 1) * 8), 2
        LINE (37 + x2 * 8, 16 + (y2 + 1) * 8)-(37 + x2 * 8, 23 + (y2 + 1) * 8), 2
        LINE (30 + x2 * 8, 16 + (y2 + 1) * 8)-(37 + x2 * 8, 16 + (y2 + 1) * 8), 10
ELSEIF c(x2, y2) = 6 THEN
        LINE (30 + x2 * 8, 16 + (y2 + 1) * 8)-(30 + x2 * 8, 23 + (y2 + 1) * 8), 150
        LINE (30 + x2 * 8, 23 + (y2 + 1) * 8)-(37 + x2 * 8, 23 + (y2 + 1) * 8), 142
        LINE (37 + x2 * 8, 16 + (y2 + 1) * 8)-(37 + x2 * 8, 23 + (y2 + 1) * 8), 142
        LINE (30 + x2 * 8, 16 + (y2 + 1) * 8)-(37 + x2 * 8, 16 + (y2 + 1) * 8), 150
END IF
DO
fell = 0
IF c(x2, y2) <> 0 AND c(x2, y2 + 1) = 0 THEN
        GET (30 + x2 * 8, 16 + (y2 + 1) * 8)-(37 + x2 * 8, 23 + (y2 + 1) * 8), twist
        PUT (30 + x2 * 8, 16 + (y2 + 2) * 8), twist, PSET
        PUT (30 + x2 * 8, 16 + (y2 + 1) * 8), pbg(49 * (10 * (y2 + 1) + x2)), PSET
        c(x2, y2 + 1) = c(x2, y2)
        o(x2, y2 + 1) = changer
        c(x2, y2) = 0
        o(x2, y2) = 0
        changer = changer + 1
        y2 = y2 + 1
        fell = 1
END IF
LOOP UNTIL fell = 0
NEXT y2
FOR delay = 1 TO 50000
NEXT delay
NEXT x2
twisted = 0
GOTO supercheck

clearsp:
a = 0
FOR j = y(1) TO 21
FOR i = 1 TO 4
IF c(x(1) + ox(p(1), r(1), i), j + (oy(p(1), r(1), i) + 1)) <> 0 THEN a = a + 1
NEXT i
IF a >= 1 THEN yy = j + 1: EXIT FOR
NEXT j
WAIT &H3DA, 8
PUT (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (y(1) + oy(p(1), r(1), 1)) * 8 + (8 * (move / level))), bg(0), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (y(1) + oy(p(1), r(1), 2)) * 8 + (8 * (move / level))), bg(49), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (y(1) + oy(p(1), r(1), 3)) * 8 + (8 * (move / level))), bg(98), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (y(1) + oy(p(1), r(1), 4)) * 8 + (8 * (move / level))), bg(147), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (yy + oy(p(1), r(1), 1)) * 8), sbg(0), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (yy + oy(p(1), r(1), 2)) * 8), sbg(49), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (yy + oy(p(1), r(1), 3)) * 8), sbg(98), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (yy + oy(p(1), r(1), 4)) * 8), sbg(147), PSET
RETURN

redraw:
a = 0
FOR j = y(1) TO 21
FOR i = 1 TO 4
IF c(x(1) + ox(p(1), r(1), i), j + (oy(p(1), r(1), i) + 1)) <> 0 THEN a = a + 1
NEXT i
IF a >= 1 THEN yy = j + 1: EXIT FOR
NEXT j
GET (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (yy + oy(p(1), r(1), 1)) * 8)-(38 + (x(1) + ox(p(1), r(1), 1)) * 8, 24 + (yy + oy(p(1), r(1), 1)) * 8), sbg(0)
GET (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (yy + oy(p(1), r(1), 2)) * 8)-(38 + (x(1) + ox(p(1), r(1), 2)) * 8, 24 + (yy + oy(p(1), r(1), 2)) * 8), sbg(49)
GET (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (yy + oy(p(1), r(1), 3)) * 8)-(38 + (x(1) + ox(p(1), r(1), 3)) * 8, 24 + (yy + oy(p(1), r(1), 3)) * 8), sbg(98)
GET (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (yy + oy(p(1), r(1), 4)) * 8)-(38 + (x(1) + ox(p(1), r(1), 4)) * 8, 24 + (yy + oy(p(1), r(1), 4)) * 8), sbg(147)
PUT (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (yy + oy(p(1), r(1), 1)) * 8), spi(49 * (p(1) - 1) + 1), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (yy + oy(p(1), r(1), 2)) * 8), spi(49 * (p(1) - 1) + 1), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (yy + oy(p(1), r(1), 3)) * 8), spi(49 * (p(1) - 1) + 1), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (yy + oy(p(1), r(1), 4)) * 8), spi(49 * (p(1) - 1) + 1), PSET

GET (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (y(1) + oy(p(1), r(1), 1)) * 8 + (8 * (move / level)))-(38 + (x(1) + ox(p(1), r(1), 1)) * 8, 24 + (y(1) + oy(p(1), r(1), 1)) * 8 + (8 * (move / level))), bg(0)
GET (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (y(1) + oy(p(1), r(1), 2)) * 8 + (8 * (move / level)))-(38 + (x(1) + ox(p(1), r(1), 2)) * 8, 24 + (y(1) + oy(p(1), r(1), 2)) * 8 + (8 * (move / level))), bg(49)
GET (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (y(1) + oy(p(1), r(1), 3)) * 8 + (8 * (move / level)))-(38 + (x(1) + ox(p(1), r(1), 3)) * 8, 24 + (y(1) + oy(p(1), r(1), 3)) * 8 + (8 * (move / level))), bg(98)
GET (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (y(1) + oy(p(1), r(1), 4)) * 8 + (8 * (move / level)))-(38 + (x(1) + ox(p(1), r(1), 4)) * 8, 24 + (y(1) + oy(p(1), r(1), 4)) * 8 + (8 * (move / level))), bg(147)
PUT (30 + (x(1) + ox(p(1), r(1), 1)) * 8, 16 + (y(1) + oy(p(1), r(1), 1)) * 8 + (8 * (move / level))), pi(49 * (p(1) - 1) + 1), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 2)) * 8, 16 + (y(1) + oy(p(1), r(1), 2)) * 8 + (8 * (move / level))), pi(49 * (p(1) - 1) + 1), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 3)) * 8, 16 + (y(1) + oy(p(1), r(1), 3)) * 8 + (8 * (move / level))), pi(49 * (p(1) - 1) + 1), PSET
PUT (30 + (x(1) + ox(p(1), r(1), 4)) * 8, 16 + (y(1) + oy(p(1), r(1), 4)) * 8 + (8 * (move / level))), pi(49 * (p(1) - 1) + 1), PSET
RETURN

menu:
CLS
COLOR 80: LOCATE 5, 15: PRINT "N"
LOCATE 6, 17: PRINT "S"
LOCATE 7, 18: PRINT "Q"
COLOR 70: LOCATE 5, 16: PRINT "ew Tetris"
LOCATE 6, 18: PRINT "cores"
LOCATE 7, 19: PRINT "uit"

DO
a$ = INKEY$
IF a$ = "n" THEN GOTO start
IF a$ = "s" THEN GOTO scores
IF a$ = "q" THEN END
LOOP

scorecheck:
FOR i = 1 TO 5
IF score& > scores(i) THEN
        CLS
        IF i < 5 THEN
                FOR j = 5 TO i + 1 STEP -1
                scores(j) = scores(j - 1)
                pname(j) = pname(j - 1)
                NEXT j
        END IF
        LOCATE 4, 10: COLOR 70: PRINT "You have achived a high score!"
        DO
        LOCATE 5, 14: COLOR 0: PRINT "Name: "; username$
        LOCATE 5, 14: COLOR 70: INPUT "Name: ", username$
        LOOP UNTIL LEN(username$) <= 7
        scores(i) = score&
        pname(i) = username$
        OPEN "dat2.dat" FOR OUTPUT AS #1
        FOR i = 1 TO 5
        WRITE #1, scores(i)
        WRITE #1, pname(i)
        NEXT i
        CLOSE
        GOTO scores
END IF
NEXT i
CLS
GOTO scores

scores:
CLS
COLOR 70
FOR i = 1 TO 5
LOCATE 4, 14: PRINT "Name       Score"
LOCATE 4 + i, 15: PRINT pname(i)
LOCATE 4 + i, 25: PRINT scores(i)
NEXT i
LOCATE 11, 15: COLOR 80: PRINT "R": LOCATE 11, 16: COLOR 70: PRINT "eturn"
DO
a$ = INKEY$
IF a$ = "r" THEN GOTO menu
LOOP




