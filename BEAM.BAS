RANDOMIZE TIMER
SCREEN 12
max = 20
DIM p1shot(1 TO max) AS INTEGER
DIM i(1 TO max) AS INTEGER
DIM x(1 TO max) AS INTEGER
DIM y(1 TO max) AS INTEGER
DIM tim(1 TO max) AS INTEGER
DIM p1sy(1 TO max) AS INTEGER
DIM p1ws(1 TO max) AS INTEGER
DIM p1wav(1 TO max) AS INTEGER
DIM co(1 TO max) AS INTEGER
DIM exco(1 TO max) AS INTEGER
p1co& = INT(RND * 5) + 1
p1& = 240

DO
FOR i = 1 TO max
        IF p1shot(i) > 0 THEN GOSUB fire1
NEXT i
p1& = p1& + p1dir&
IF p1& < 3 THEN p1& = 3
IF p1& > 477 THEN p1& = 477
LINE (0, p1& - 3)-(5, p1& + 3), 8, BF
LINE (0, p1& - 4)-(5, p1& - 7), 0, BF
LINE (0, p1& + 4)-(5, p1& + 7), 0, BF

FOR i = 1 TO max
IF p1wav(i) = 1 THEN
        IF p1ws(i) < 51 THEN
                CIRCLE (634, p1sy(i)), p1ws(i), exco(i), , , 5
                p1ws(i) = p1ws(i) + 1
        ELSEIF p1ws(i) > 50 THEN
                CIRCLE (634, p1sy(i)), p1ws(i) - 50, 0, , , 5
                p1ws(i) = p1ws(i) + 1
        END IF
        IF p1ws(i) = 101 THEN
                p1ws(i) = 0
                p1wav(i) = 0
        END IF
END IF
NEXT i
a$ = INKEY$
IF a$ = "s" THEN
        FOR i = 1 TO max
        IF p1shot(i) = 0 THEN
                co(i) = p1co&
                exco(i) = co(i)
                p1shot(i) = 1
                p1wav(i) = 1
                p1ws(i) = 1
                p1sy(i) = p1&
                EXIT FOR
        END IF
        NEXT i
ELSEIF a$ = "q" THEN
        p1dir& = -1
ELSEIF a$ = "a" THEN
        p1dir& = 0
ELSEIF a$ = "z" THEN
        p1dir& = 1
END IF

LOOP

fire1:
FOR b = 1 TO max
IF p1shot(b) >= 1 AND p1shot(b) <= 8 THEN
        i(b) = i(b) + 1
        IF i(b) = 2 THEN co(b) = co(b) + 8
        IF i(b) = 3 THEN co(b) = 7
        FOR x = (p1shot(b) - 1) * 80 TO p1shot(b) * 80
        y(b) = y(b) + INT(RND * 3) - 1
        IF y(b) > 3 THEN y(b) = 3
        IF y(b) < -3 THEN y(b) = -3
        PSET (x, p1sy(b) + y(b)), co(b)
        NEXT x
        IF i(b) = 3 THEN
                p1shot(b) = p1shot(b) + 1
                co(b) = p1co&
                i(b) = 0
        END IF
END IF
IF p1shot(b) = 9 THEN
        tim(b) = tim(b) + 1
        IF tim(b) = 100 THEN
                tim(b) = 0
                p1shot(b) = 10
        END IF
END IF
IF p1shot(b) = 10 THEN
        FOR a = 1 TO 4
        i(b) = i(b) + 1
        x(b) = INT(RND * 635) + 5
        y(b) = INT(RND * 8)
        y(b) = y(b) - 3
        PSET (x(b), y(b) + p1sy(b)), 0
        NEXT a
        IF i(b) > 9999 THEN
                LINE (1, p1sy(b) - 3)-(640, p1sy(b) + 3), 0, BF
                i(b) = 0
                p1shot(b) = 0
        END IF
END IF
NEXT b
RETURN

