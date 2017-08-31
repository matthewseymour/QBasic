SCREEN 12
INPUT "Stars ", stars&
IF stars& > 3000 THEN stars& = 3000
IF stars& < 50 THEN stars& = 50
CLS
RANDOMIZE TIMER
DIM s(1 TO stars&) AS INTEGER
DIM b(1 TO stars&) AS INTEGER
DIM x(1 TO stars&) AS INTEGER
DIM y(1 TO stars&) AS INTEGER
DIM xs(1 TO stars&) AS INTEGER
DIM ys(1 TO stars&) AS INTEGER
DIM xc(1 TO stars&) AS INTEGER
DIM yc(1 TO stars&) AS INTEGER
DIM max(1 TO stars&) AS INTEGER


FOR i = 1 TO stars&
DO
max(i) = max(i) + 1
a& = INT(RND * 10) + 1
LOOP UNTIL a& = 1
b(i) = 0

FOR a = 1 TO max(i)
b(i) = b(i) + a
NEXT a

b(i) = b(i)
rand& = INT(RND * 10) - 2
rand2& = INT(RND * 10) - 2
x(i) = 320 + rand&
y(i) = 240 - b(i) + rand2&
xs(i) = max(i)
ys(i) = 0
yc(i) = 1
xc(i) = -1
s(i) = INT(RND * 2) + 1
NEXT i

FOR d = 1 TO stars&
rand& = INT(RND * 200) + 1
FOR i = 1 TO rand&
ys(d) = ys(d) + yc(d)
IF ys(d) = max(d) THEN yc(d) = -1
IF ys(d) = -max(d) THEN yc(d) = 1

xs(d) = xs(d) + xc(d)
IF xs(d) = max(d) THEN xc(d) = -1
IF xs(d) = -max(d) THEN xc(d) = 1

y(d) = y(d) + ys(d)
x(d) = x(d) + xs(d)

NEXT i
NEXT d



DO
FOR i = 1 TO 20000
NEXT i
FOR d = 1 TO stars&
FOR m = 1 TO s(d)
ys(d) = ys(d) + yc(d)
IF ys(d) = max(d) THEN yc(d) = -1
IF ys(d) = -max(d) THEN yc(d) = 1

xs(d) = xs(d) + xc(d)
IF xs(d) = max(d) THEN xc(d) = -1
IF xs(d) = -max(d) THEN xc(d) = 1

PSET (x(d) + 1, y(d)), 0

y(d) = y(d) + ys(d)
x(d) = x(d) + xs(d)

PSET (x(d) + 1, y(d)), 15
NEXT m
NEXT d

LOOP

