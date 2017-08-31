DIM an(100)
DIM x(8)
DIM y(8)

x(1) = -1
x(2) = 0
x(3) = 1
x(4) = 1
x(5) = 1
x(6) = 0
x(7) = -1
x(8) = -1

y(1) = 1
y(2) = 1
y(3) = 1
y(4) = 0
y(5) = -1
y(6) = -1
y(7) = -1
y(8) = 0
SCREEN 12
start:
CLS


INPUT "Num ", a
a = a - 1
FOR b = 0 TO a
INPUT an(b)
NEXT b


CLS
nx = 0
ny = 0
dir = 1
b = 0

DO

LINE (nx * 10 + 320, ny * 10 + 240)-(nx * 10 + x(dir) * 10 + 320, ny * 10 + y(dir) * 10 + 240), 15
nx = nx + x(dir)
ny = ny + y(dir)

IF an(b) = 45 THEN dir = dir + 1
IF an(b) = 90 THEN dir = dir + 2
IF an(b) = 135 THEN dir = dir + 3
IF an(b) = 180 THEN dir = dir + 4
IF an(b) = 225 THEN dir = dir + 5
IF an(b) = 270 THEN dir = dir + 6
IF an(b) = 315 THEN dir = dir + 7
IF an(b) = 360 THEN dir = dir + 8
IF dir > 8 THEN dir = dir - 8

b = b + 1
IF b > a THEN b = 0

SLEEP
LOOP UNTIL INKEY$ = CHR$(27)
GOTO start

