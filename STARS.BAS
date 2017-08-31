CLS
SCREEN 12
stars& = 400
DIM max(1, stars&) AS INTEGER
DIM a(1, stars&) AS INTEGER
DIM s(1, stars&) AS INTEGER
DIM o(1, stars&) AS INTEGER
DIM x(1, stars&) AS INTEGER
DIM y(1, stars&) AS INTEGER
DIM xs(1, stars&) AS INTEGER
DIM ys(1, stars&) AS INTEGER
DIM col(1, stars&) AS INTEGER
DIM d(1, stars&) AS INTEGER

DO
LINE (0, 0)-(5, 480), 0, BF
LINE (0, 0)-(640, 5), 0, BF
FOR i = 1 TO stars&
IF o(1, i) = 0 THEN
        RANDOMIZE TIMER
        col(1, i) = INT(RND * 15) + 1
        d(1, i) = INT(RND * 100)
        max(1, i) = INT(RND * 25) + 25
        DO
        xs(1, i) = INT(RND * 40) - 20
        ys(1, i) = INT(RND * 40) - 20
        LOOP UNTIL (xs(1, i) <> 0 OR ys(1, i) <> 0)
        s(1, i) = 0
        o(1, i) = 1
        a(1, i) = 0
        x(1, i) = 320
        y(1, i) = 240
END IF
NEXT i
FOR i = 1 TO stars&
co& = col(1, i)
IF d(1, i) > 0 THEN d(1, i) = d(1, i) - 1
IF d(1, i) > 1 THEN co& = 0
a(1, i) = a(1, i) + 1
IF a(1, i) = max(1, i) THEN s(1, i) = s(1, i) + 1
LINE (x(1, i), y(1, i))-(x(1, i) + s(1, i), y(1, i) + s(1, i)), 0, BF
x(1, i) = x(1, i) + xs(1, i)
y(1, i) = y(1, i) + ys(1, i)
LINE (x(1, i), y(1, i))-(x(1, i) + s(1, i), y(1, i) + s(1, i)), co&, BF
IF x(1, i) < 0 OR x(1, i) > 640 OR y(1, i) < 0 OR y(1, i) > 480 THEN o(1, i) = 0
NEXT i
LOOP


