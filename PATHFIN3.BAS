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
restart:
FOR ii = 0 TO diff * 2
i = ii
IF i > diff THEN i = (diff - (ii - diff))
FOR x = ux - i TO ux + i
yy(1) = uy - i
yy(2) = uy + i
FOR a = 1 TO 2
IF x > 19 OR x < 0 OR yy(a) > 19 OR yy(a) < 0 THEN GOTO skip2
IF chx(x, yy(a)) <> 0 OR chy(x, yy(a)) <> 0 THEN GOTO skip2
IF t(x, yy(a)) = 0 THEN
        IF yy(a) > uy THEN
                IF x < ux THEN start(1) = 2
                IF x = ux THEN start(1) = 1
                IF x > ux THEN start(1) = 8
        ELSEIF yy(a) < uy THEN
                IF x < ux THEN start(1) = 4
                IF x = ux THEN start(1) = 5
                IF x > ux THEN start(1) = 6
        ELSEIF yy(a) = uy THEN
                IF x < ux THEN start(1) = 3 ELSE start(1) = 7
        END IF
        b = 1
        FOR j = 2 TO 8 STEP 2
        start(j) = start(1) + b
        IF start(j) > 8 THEN start(j) = start(j) - 8
        IF j < 8 THEN
                start(j + 1) = start(1) - b
                IF start(j + 1) < 1 THEN start(j + 1) = start(j + 1) + 8
        END IF
        b = b + 1
        NEXT j
        FOR k = 1 TO 8
        j = start(k)
        IF x + ax(j) > 19 OR x + ax(j) < 0 OR yy(a) + ay(j) > 19 OR yy(a) + ay(j) < 0 THEN GOTO skip
        IF (chx(x, yy(a)) = 0 AND chy(x, yy(a)) = 0) AND ((chx(x + ax(j), yy(a) + ay(j)) <> 0 OR chy(x + ax(j), yy(a) + ay(j)) <> 0)) THEN
                chx(x, yy(a)) = x + ax(j)
                chy(x, yy(a)) = yy(a) + ay(j)
                IF x = dx AND yy(a) = dy THEN GOTO done
        END IF
skip:
        NEXT k
        IF (chx(x, yy(a)) <> 0 OR chy(x, yy(a)) <> 0) THEN LINE (x * 10 + 5, yy(a) * 10 + 5)-(chx(x, yy(a)) * 10 + 5, chy(x, yy(a)) * 10 + 5)
END IF
skip2:
NEXT a
NEXT x
FOR y = uy - i TO uy + i
xx(1) = ux - i
xx(2) = ux + i
FOR a = 1 TO 2
IF xx(a) > 19 OR xx(a) < 0 OR y > 19 OR y < 0 THEN GOTO skip4
IF chx(xx(a), y) <> 0 OR chy(xx(a), y) <> 0 THEN GOTO skip4
IF t(xx(a), y) = 0 THEN
        IF y > uy THEN
                IF xx(a) < ux THEN start(1) = 2
                IF xx(a) = ux THEN start(1) = 1
                IF xx(a) > ux THEN start(1) = 8
        ELSEIF y < uy THEN
                IF xx(a) < ux THEN start(1) = 4
                IF xx(a) = ux THEN start(1) = 5
                IF xx(a) > ux THEN start(1) = 6
        ELSEIF y = uy THEN
                IF xx(a) < ux THEN start(1) = 3 ELSE start(1) = 7
        END IF
        b = 1
        FOR j = 2 TO 8 STEP 2
        start(j) = start(1) + b
        IF start(j) > 8 THEN start(j) = start(j) - 8
        IF j < 8 THEN
                start(j + 1) = start(1) - b
                IF start(j + 1) < 1 THEN start(j + 1) = start(j + 1) + 8
        END IF
        b = b + 1
        NEXT j
        FOR k = 1 TO 8
        j = start(k)
        IF xx(a) + ax(j) > 19 OR xx(a) + ax(j) < 0 OR y + ay(j) > 19 OR y + ay(j) < 0 THEN GOTO skip3
        IF (chx(xx(a), y) = 0 AND chy(xx(a), y) = 0) AND ((chx(xx(a) + ax(j), y + ay(j)) <> 0 OR chy(xx(a) + ax(j), y + ay(j)) <> 0)) THEN
                chx(xx(a), y) = xx(a) + ax(j)
                chy(xx(a), y) = y + ay(j)
                IF xx(a) = dx AND y = dy THEN GOTO done
        END IF
        NEXT k
        IF (chx(xx(a), y) <> 0 OR chy(xx(a), y) <> 0) THEN LINE (xx(a) * 10 + 5, y * 10 + 5)-(chx(xx(a), y) * 10 + 5, chy(xx(a), y) * 10 + 5)
skip3:
END IF
skip4:
NEXT a
NEXT y
NEXT ii
IF chx(dx, dy) = 0 AND chy(dx, dy) = 0 AND diff < 40 THEN diff = diff + 1: GOTO restart
done:
x = dx
y = dy
DO
LINE (x * 10 + 5, y * 10 + 5)-(chx(x, y) * 10 + 5, chy(x, y) * 10 + 5), 2
x2 = x
x = chx(x, y)
y = chy(x2, y)
LOOP UNTIL x = ux AND y = uy
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
DATA 1,0,0,0,0,1,0,0,1,1,1,1,1,1,1,1,0,0,0,1
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


