SCREEN 12
RANDOMIZE TIMER
SLEEP
DIM x(1 TO 10) AS INTEGER
DIM y(1 TO 10) AS INTEGER
start:
CLS
tx = INT(RND * 630) + 5
ty = INT(RND * 470) + 5
xs = 0
ys = -1
stage = 1
DO
CIRCLE (tx, ty), 4
IF stage = 1 THEN
        dxs = 0
        dys = dys - 1
        IF dys <= -20 THEN stage = 2
END IF
IF stage = 2 THEN
        range = 20
        IF ABS(tx - x) > ABS((ty + 210) - y) THEN
                IF tx - x < 0 THEN dxs = -range
                IF tx - x > 0 THEN dxs = range
                a = ABS(tx - x) / range
                IF (ty + 210) - y < 0 THEN dys = -(ABS((ty + 210) - y) / a)
                IF (ty + 210) - y > 0 THEN dys = (ABS((ty + 210) - y) / a)
        ELSEIF ABS((ty + 210) - y) > ABS(tx - x) THEN
                IF (ty + 210) - y < 0 THEN dys = -range
                IF (ty + 210) - y > 0 THEN dys = range
                a = ABS((ty + 210) - y) / range
                IF tx - x < 0 THEN dxs = -(ABS(tx - x) / a)
                IF tx - x > 0 THEN dxs = (ABS(tx - x) / a)
        END IF
        IF ABS(tx - x) < 11 AND ABS((ty + 210) - y) < 11 THEN stage = 3
END IF
IF stage = 3 THEN
        IF ABS(tx - x) > ABS(ty - y) THEN
                range = ABS(tx - x) / 20
                IF range > 20 THEN range = 20
                IF tx - x < 0 THEN dxs = -range
                IF tx - x > 0 THEN dxs = range
                a = ABS(tx - x) / range
                IF ty - y < 0 THEN dys = -(ABS(ty - y) / a)
                IF ty - y > 0 THEN dys = (ABS(ty - y) / a)
        ELSEIF ABS(ty - y) > ABS(tx - x) THEN
                range = ABS(ty - y) / 20
                IF range > 20 THEN range = 20
                IF ty - y < 0 THEN dys = -range
                IF ty - y > 0 THEN dys = range
                a = ABS(ty - y) / range
                IF tx - x < 0 THEN dxs = -(ABS(tx - x) / a)
                IF tx - x > 0 THEN dxs = (ABS(tx - x) / a)
        END IF
END IF
IF ys < dys THEN ys = ys + 1
IF ys > dys THEN ys = ys - 1
IF xs < dxs THEN xs = xs + 1
IF xs > dxs THEN xs = xs - 1
CIRCLE (x, y), 1, 0
IF ABS(tx - x) < 20 AND ABS(ty - y) < 20 THEN
        x = tx
        y = ty
END IF
x = x + xs
y = y + ys
CIRCLE (x, y), 1, 2
FOR i = 1 TO 20000
NEXT i
a$ = INKEY$
IF a$ = " " THEN GOTO start
LOOP

