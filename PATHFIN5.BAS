'jb@hhm.k12.nf.ca
SCREEN 13
RANDOMIZE TIMER
DIM t(-1 TO 20, -1 TO 20) AS INTEGER
DIM chx(-1 TO 20, -1 TO 20), chy(-1 TO 20, -1 TO 20)
DIM ng(19, 19)
DIM num(19, 19)
DIM yy(2)
DIM xx(2)
DIM ax(8)
DIM ay(8)
DIM start(8)
DIM passed(19, 19) AS INTEGER
ax(1) = 0
ax(2) = 1
ax(3) = 1
ax(4) = 1
ax(5) = 0
ax(6) = -1
ax(7) = -1
ax(8) = -1
ay(1) = -1
ay(2) = -1
ay(3) = 0
ay(4) = 1
ay(5) = 1
ay(6) = 1
ay(7) = 0
ay(8) = -1
FOR y = 0 TO 19
FOR x = 0 TO 19
LINE (x * 10, y * 10)-(x * 10 + 10, y * 10 + 10), 7, B
READ t(x, y)
IF t(x, y) = 1 THEN LINE (x * 10 + 1, y * 10 + 1)-(x * 10 + 9, y * 10 + 9), t(x, y), BF
NEXT
NEXT
ux = 3
uy = 3
dx = 18
dy = 18
chx(ux, uy) = ux
chy(ux, uy) = uy
x = ux
y = uy
PSET (x * 10 + 5, y * 10 + 5), 2
PSET (dx * 10 + 5, dy * 10 + 5), 3
xmax = x
ymax = y
xmin = y
ymin = y
SLEEP
DO
a = 0
FOR x = xmin TO xmax
FOR y = ymin TO ymax
IF chx(x, y) <> 0 AND ng(x, y) = 0 THEN
        IF a = 0 THEN
                diffx = ABS(x - dx)
                diffy = ABS(y - dy)
                IF diffy > diffx THEN diff = diffy ELSE diff = diffx
                x2 = x
                y2 = y
                a = 1
        END IF
        IF a = 1 THEN
                diffx = ABS(x - dx)
                diffy = ABS(y - dy)
                IF diffy > diffx THEN diff2 = diffy ELSE diff2 = diffx
                IF diff2 < diff THEN
                        diff = diff2
                        x2 = x
                        y2 = y
                END IF
        END IF
END IF
NEXT y
NEXT x
FOR x3 = xmin TO xmax
FOR y3 = ymin TO ymax
IF chx(x3, y3) <> 0 AND ng(x3, y3) = 0 THEN
        diffx = ABS(x3 - dx)
        diffy = ABS(y3 - dy)
        IF diffy > diffx THEN diff2 = diffy ELSE diff2 = diffx
        IF diff2 <= diff THEN
                FOR x = x3 - 1 TO x3 + 1
                FOR y = y3 - 1 TO y3 + 1
                IF chx(x, y) = 0 AND t(x, y) = 0 THEN
                        chx(x, y) = x3
                        chy(x, y) = y3
                        LINE (x * 10 + 5, y * 10 + 5)-(x3 * 10 + 5, y3 * 10 + 5), 15
                        IF x < xmin THEN xmin = x
                        IF x > xmax THEN xmax = x
                        IF y < ymin THEN ymin = y
                        IF y > ymax THEN ymax = y
                        IF x = dx AND y = dy THEN GOTO done
                END IF
                NEXT y
                NEXT x
        END IF
END IF
NEXT y3
NEXT x3
ng(x2, y2) = 1
LOOP
done:
x = dx
y = dy
DO
LINE (x * 10 + 5, y * 10 + 5)-(chx(x, y) * 10 + 5, chy(x, y) * 10 + 5), 12
x2 = x
x = chx(x, y)
y = chy(x2, y)
LOOP UNTIL x = ux AND y = uy
SLEEP



REM  0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1
DATA 1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1


