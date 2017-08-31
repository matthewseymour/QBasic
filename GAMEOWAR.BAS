SCREEN 12
RANDOMIZE TIMER
DIM ux(300) AS INTEGER
DIM uy(300) AS INTEGER
DIM uh(300) AS INTEGER
DIM ut(300) AS INTEGER
DIM udx(300) AS INTEGER
DIM udy(300) AS INTEGER
DIM uj(300) AS INTEGER

DIM sx(300) AS INTEGER
DIM sy(300) AS INTEGER
DIM sxs(300) AS INTEGER
DIM sys(300) AS INTEGER
DIM st(300) AS INTEGER

DIM bx(4) AS INTEGER
DIM by(4) AS INTEGER
DIM bp(4) AS INTEGER
DIM bh(4) AS INTEGER
DIM bt(4) AS INTEGER

FOR i = 1 TO 4
bx(i) = INT(RND * 620) + 10
by(i) = INT(RND * 460) + 10
bh(i) = 500
bt(i) = i
NEXT i

LINE (0, 0)-(640, 480), 6, BF

DO
FOR i = 1 TO 4
LINE (bx(i) - 5, by(i) - 5)-(bx(i) + 5, by(i) + 5), bt(i), BF
bp(i) = bp(i) + 1
IF bp(i) = 50 THEN
        bp(i) = 0
        FOR j = 1 TO 300
        IF uh(j) <= 0 THEN
                uh(j) = 10
                ux(j) = bx(i) + 6
                uy(j) = by(i) + 6
                udx(j) = ux(i)
                udy(j) = uy(i)
                uj(j) = 0
                ut(j) = i
        END IF
        NEXT j
END IF
NEXT i
LOOP

