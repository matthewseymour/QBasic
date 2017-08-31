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
SLEEP
xdiff = ABS(dx - ux)
ydiff = ABS(dy - uy)
IF xdiff >= ydiff THEN diff = xdiff ELSE diff = ydiff
diff2 = 0
DO
FOR x = ux - diff2 TO ux + diff
FOR y = ux - diff2 TO uy + diff
IF x < 19 AND y < 19 AND x > 0 AND y > 0 THEN
IF chx(x, y) <> 0 AND ng(x, y) = 0 THEN
        FOR x2 = x - 1 TO x + 1
        FOR y2 = y - 1 TO y + 1
        IF x2 < 19 AND y2 < 19 AND x2 > 0 AND y2 > 0 THEN
                IF chx(x2, y2) = 0 AND chy(x2, y2) = 0 AND t(x2, y2) = 0 THEN
                        chx(x2, y2) = x
                        chy(x2, y2) = y
                        LINE (x2 * 10 + 5, y2 * 10 + 5)-(x * 10 + 5, y * 10 + 5), 15
                END IF
                IF x2 = dx AND y2 = dy THEN GOTO done
        END IF
        NEXT y2
        NEXT x2
        ng(x, y) = 1
END IF
END IF
NEXT y
NEXT x
diff2 = diff2 + 1
diff = diff + 1
LOOP UNTIL diff > 40
done:
SLEEP
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,1,1,1,1,1,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,1,1,1,1,0,0,0,0,0,0,1
DATA 1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1
DATA 1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,1,0,1,0,1
DATA 1,0,0,0,1,1,1,1,1,1,1,0,0,0,0,1,0,1,0,1
DATA 1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,0,1,0,1
DATA 1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,0,1,0,1
DATA 1,0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,0,1,0,1
DATA 1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1


