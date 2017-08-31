CLS
SCREEN 12
RANDOMIZE TIMER
stars& = 700
DIM max(1, stars&) AS INTEGER
DIM a(1, stars&) AS INTEGER
DIM s(1, stars&) AS INTEGER
DIM o(1, stars&) AS INTEGER
DIM sx(1, stars&) AS INTEGER
DIM sy(1, stars&) AS INTEGER
DIM xs(1, stars&) AS INTEGER
DIM ys(1, stars&) AS INTEGER
DIM d(1, stars&) AS INTEGER

x& = 500
y& = 500
z& = 500
hd& = 1
vd& = 3

DO
LINE (0, 0)-(5, 480), 0, BF
LINE (0, 0)-(640, 5), 0, BF
FOR i = 1 TO stars&
IF o(1, i) = 0 THEN
        IF star& < 201 THEN
                d(1, i) = INT(RND * 320)
                max(1, i) = INT(RND * 25) + 25
                DO
                xs(1, i) = INT(RND * 16) - 8
                ys(1, i) = INT(RND * 16) - 8
                LOOP UNTIL (xs(1, i) <> 0 AND ys(1, i) <> 0 AND ABS(xs(1, i)) <> ABS(ys(1, i)))
                s(1, i) = 0
                o(1, i) = 1
                a(1, i) = 0
                sx(1, i) = 320
                sy(1, i) = 240
                star& = star& + 1
        END IF
END IF
NEXT i
FOR i = 1 TO stars&
LINE (sx(1, i), sy(1, i))-(sx(1, i) + s(1, i), sy(1, i) + s(1, i)), 0, BF
IF o(1, i) = 0 THEN GOTO fin
co& = 15
IF d(1, i) > 0 THEN d(1, i) = d(1, i) - 1
IF d(1, i) > 1 THEN co& = 0
a(1, i) = a(1, i) + 1
IF a(1, i) = max(1, i) THEN s(1, i) = s(1, i) + 1
sx(1, i) = sx(1, i) + xs(1, i)
sy(1, i) = sy(1, i) + ys(1, i)
IF sx(1, i) < 0 OR sx(1, i) > 640 OR sy(1, i) < 0 OR sy(1, i) > 480 THEN
        o(1, i) = 0
        star& = star& - 1
END IF
LINE (sx(1, i), sy(1, i))-(sx(1, i) + s(1, i), sy(1, i) + s(1, i)), co&, BF
fin:
NEXT i
LINE (600, 100)-(550, 75), 14
LINE (500, 100)-(550, 75), 14
LINE (600, 100)-(550, 125), 14
LINE (500, 100)-(550, 125), 14

LINE (600, 150)-(550, 125), 14
LINE (500, 150)-(550, 125), 14
LINE (600, 150)-(550, 175), 14
LINE (500, 150)-(550, 175), 14

LINE (600, 150)-(600, 100), 14
LINE (500, 150)-(500, 100), 14
LINE (550, 125)-(550, 75), 14
LINE (550, 175)-(550, 125), 14

LOCATE 2, 2
PRINT "x:"; x&; "y:"; y&; "z:"; z&
LOCATE 3, 2
PRINT "x:"; fx&; "y:"; fy&; "z:"; fz&
LOCATE 4, 2
PRINT hd&; vd&
LINE (600 - (fx& - CLNG(fx& / 2)), 175 - fz& + CLNG(fx& / 2))-(550 - (fx& - CLNG(fx& / 2)), 150 - fz& + CLNG(fx& / 2)), 0
LINE (500 + (fy& - CLNG(fy& / 2)), 175 - fz& + CLNG(fy& / 2))-(550 + (fy& - CLNG(fy& / 2)), 150 - fz& + CLNG(fy& / 2)), 0
'LINE (550 - (fx& - CLNG(fx& / 2)) + (fy& - CLNG(fy& / 2)), 75 + CLNG((fy& + fx&) / 2))-(550 - (fx& - CLNG(fx& / 2)) + (fy& - CLNG(fy& / 2)), 125 + CLNG((fy& + fx&) / 2)), 0
IF vd& <> 1 AND vd& <> 5 THEN
        IF vd& > 5 THEN hd& = hd& + 4
        IF hd& < 1 THEN hd& = hd& + 8
        IF hd& > 8 THEN hd& = hd& - 8
        IF hd& = 1 THEN
                y& = y& - 1
        ELSEIF hd& = 2 THEN
                y& = y& - 1
                x& = x& + 1
        ELSEIF hd& = 3 THEN
                x& = x& + 1
        ELSEIF hd& = 4 THEN
                y& = y& + 1
                x& = x& + 1
        ELSEIF hd& = 5 THEN
                y& = y& + 1
        ELSEIF hd& = 6 THEN
                y& = y& + 1
                x& = x& - 1
        ELSEIF hd& = 7 THEN
                x& = x& - 1
        ELSEIF hd& = 8 THEN
                y& = y& - 1
                x& = x& - 1
        END IF
        IF vd& > 5 THEN hd& = hd& - 4
        IF hd& < 1 THEN hd& = hd& + 8
        IF hd& > 8 THEN hd& = hd& - 8
END IF
IF vd& < 3 OR vd& = 8 THEN z& = z& - 1
IF vd& > 3 AND vd& < 7 THEN z& = z& + 1
fz& = CLNG(z& / 10)
fy& = CLNG(y& / 10)
fx& = CLNG(x& / 10)
fx& = CLNG(fx& / 2)
fy& = CLNG(fy& / 2)
LINE (600 - (fx& - CLNG(fx& / 2)), 175 - fz& + CLNG(fx& / 2))-(550 - (fx& - CLNG(fx& / 2)), 150 - fz& + CLNG(fx& / 2)), 1
LINE (500 + (fy& - CLNG(fy& / 2)), 175 - fz& + CLNG(fy& / 2))-(550 + (fy& - CLNG(fy& / 2)), 150 - fz& + CLNG(fy& / 2)), 1
'LINE (550 - (fx& - CLNG(fx& / 2)) + (fy& - CLNG(fy& / 2)), 75 + CLNG((fy& + fx&) / 2))-(550 - (fx& - CLNG(fx& / 2)) + (fy& - CLNG(fy& / 2)), 125 + CLNG((fy& + fx&) / 2)), 1
b$ = INKEY$
IF b$ = "8" THEN
        FOR i = 1 TO stars&
                IF d(1, i) < 1 THEN ys(1, i) = ys(1, i) - 20
        NEXT i
        vd& = vd& + 1
ELSEIF b$ = "2" THEN
        FOR i = 1 TO stars&
                IF d(1, i) < 1 THEN ys(1, i) = ys(1, i) + 20
        NEXT i
        vd& = vd& - 1
ELSEIF b$ = "4" THEN
        FOR i = 1 TO stars&
                IF d(1, i) < 1 THEN xs(1, i) = xs(1, i) + 20
        NEXT i
        hd& = hd& - 1
ELSEIF b$ = "6" THEN
        FOR i = 1 TO stars&
                IF d(1, i) < 1 THEN xs(1, i) = xs(1, i) - 20
        NEXT i
        hd& = hd& + 1
END IF
IF b$ = "q" THEN END

IF vd& = 9 THEN vd& = 1
IF vd& = 0 THEN vd& = 8
IF hd& < 1 THEN hd& = hd& + 8
IF hd& > 8 THEN hd& = hd& - 8
LOOP

