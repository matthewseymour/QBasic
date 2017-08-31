SCREEN 12
RANDOMIZE TIMER
SLEEP
DIM x(1 TO 10) AS INTEGER
DIM y(1 TO 10) AS INTEGER
start:
x = 320
y = 240
tx = INT(RND * 630) + 5
ty = INT(RND * 470) + 5
xs = INT(RND * 40) - 20
ys = INT(RND * 40) - 20
dxs = INT(RND * 40) - 20
dys = INT(RND * 40) - 20
DO
CIRCLE (tx, ty), 4
IF ABS(tx - x) > ABS(ty - y) THEN
        IF tx - x < 0 THEN dxs = -20
        IF tx - x > 0 THEN dxs = 20
        a = ABS(tx - x) / 20
        IF ty - y < 0 THEN dys = -(ABS(ty - y) / a)
        IF ty - y > 0 THEN dys = (ABS(ty - y) / a)
ELSEIF ABS(ty - y) > ABS(tx - x) THEN
        IF ty - y < 0 THEN dys = -20
        IF ty - y > 0 THEN dys = 20
        a = ABS(ty - y) / 20
        IF tx - x < 0 THEN dxs = -(ABS(tx - x) / a)
        IF tx - x > 0 THEN dxs = (ABS(tx - x) / a)
END IF
IF ABS(tx - x) < 20 AND ABS(ty - y) < 20 THEN
        dxs = tx - x
        dys = ty - y
END IF
IF ys < dys THEN ys = ys + 1
IF ys > dys THEN ys = ys - 1
IF xs < dxs THEN xs = xs + 1
IF xs > dxs THEN xs = xs - 1
CIRCLE (x(1), y(1)), 1, 0
x(1) = x
y(1) = y
CIRCLE (x(1), y(1)), 1, 2
CIRCLE (x, y), 1, 0
x = x + xs
y = y + ys
CIRCLE (x, y), 1, 2
FOR i = 2 TO 10
CIRCLE (x(i), y(i)), 1, 0
x(i) = x(i - 1)
y(i) = y(i - 1)
CIRCLE (x(i), y(i)), 1, 2
NEXT i
FOR i = 1 TO 2000
NEXT i
a$ = INKEY$
IF a$ = " " THEN GOTO start
LOOP

