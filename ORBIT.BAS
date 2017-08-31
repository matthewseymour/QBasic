SCREEN 12
PRINT "How many?"
INPUT "", num&
IF num& > 30 THEN num& = 30
IF num& < 1 THEN num& = 1
PRINT "Show orbits?"
INPUT "", o$
CLS
RANDOMIZE TIMER
zoom& = 1
add& = 0
DIM s(1 TO 30) AS INTEGER
DIM c(1 TO 30) AS INTEGER
DIM b(1 TO 30) AS INTEGER
DIM x(1 TO 30) AS INTEGER
DIM y(1 TO 30) AS INTEGER
DIM xs(1 TO 30) AS INTEGER
DIM ys(1 TO 30) AS INTEGER
DIM xc(1 TO 30) AS INTEGER
DIM yc(1 TO 30) AS INTEGER
DIM max(1 TO 30) AS INTEGER
FOR i = 1 TO num&
c(i) = INT(RND * 15) + 1
s(i) = INT(RND * 10) + 1
DO
max(i) = INT(RND * 30) + 7
stu& = 0
FOR a = 1 TO num&
IF max(i) = max(a) AND a <> i THEN stu& = 1
NEXT a
LOOP UNTIL stu& = 0
FOR a = 1 TO max(i)
b(i) = b(i) + a
NEXT a
b(i) = b(i) - max(i) / 2
x(i) = 320 + max(i) / 2
y(i) = 240 - b(i)
xs(i) = max(i)
ys(i) = 0
yc(i) = 1
xc(i) = -1
NEXT i
DO
FOR i = 1 TO 1000
NEXT i
FOR i = 1 TO 10 / zoom&
CIRCLE (320 + addx&, 240 + addy&), i, 14
CIRCLE (320 + addx&, 241 + addy&), i, 14
NEXT i
FOR d = 1 TO num&
a$ = INKEY$
IF a$ = "8" THEN
        addy& = addy& + 10
        CLS
ELSEIF a$ = "6" THEN
        addx& = addx& - 10
        CLS
ELSEIF a$ = "2" THEN
        addy& = addy& - 10
        CLS
ELSEIF a$ = "4" THEN
        addx& = addx& + 10
        CLS
END IF
IF o$ = "y" THEN
        CIRCLE (320, 240), b(d) / zoom&, 1
END IF
IF ys(d) = max(d) THEN yc(d) = -1
IF ys(d) = -max(d) THEN yc(d) = 1
ys(d) = ys(d) + yc(d)
IF xs(d) = max(d) THEN xc(d) = -1
IF xs(d) = -max(d) THEN xc(d) = 1
xs(d) = xs(d) + xc(d)
LINE ((x(d) - s(d)) / zoom& - 1 + addx&, (y(d) - s(d)) / zoom& - 1 + addy&)-((x(d) + s(d)) / zoom& + 1 + addx&, (y(d) + s(d)) / zoom& + 1 + addy&), 0, BF
y(d) = y(d) + ys(d)
x(d) = x(d) + xs(d)
FOR i = 1 TO s(d) / zoom&
CIRCLE (x(d) / zoom& + addx&, y(d) / zoom& + addy&), i, c(d)
CIRCLE ((x(d) + 1) / zoom& + addx&, y(d) / zoom& + addy&), i, c(d)
NEXT i
NEXT d
LOOP

