SCREEN 12
RANDOMIZE TIMER
SLEEP 1
startedd& = TIMER
timee = 180
ts = 9
sspeed = 10

start:

tx = INT(RND * 630) + 5
ty = INT(RND * 470) + 5
txs = INT(RND * (ts * 2)) - ts
tys = INT(RND * (ts * 2)) - ts
speed = sspeed
x = 320
y = 240

IF ABS(tx - x) > ABS(ty - y) THEN
        IF tx - x < 0 THEN xs = -speed
        IF tx - x > 0 THEN xs = speed
        a = ABS(tx - x) / speed
        IF ty - y < 0 THEN ys = -(ABS(ty - y) / a)
        IF ty - y > 0 THEN ys = (ABS(ty - y) / a)
ELSEIF ABS(ty - y) > ABS(tx - x) THEN
        IF ty - y < 0 THEN ys = -speed
        IF ty - y > 0 THEN ys = speed
        a = ABS(ty - y) / speed
        IF tx - x < 0 THEN xs = -(ABS(tx - x) / a)
        IF tx - x > 0 THEN xs = (ABS(tx - x) / a)
END IF
dxs = xs
dys = ys

DO
IF ABS(tx - x) < CLNG(speed / 2) + 2 AND ABS(ty - y) < CLNG(speed / 2) + 2 THEN
        GOTO start
END IF
IF ABS(tx - x) > ABS(ty - y) THEN
        IF tx - x < 0 THEN dxs = -speed
        IF tx - x > 0 THEN dxs = speed
        a = ABS(tx - x) / speed
        IF ty - y < 0 THEN dys = -(ABS(ty - y) / a)
        IF ty - y > 0 THEN dys = (ABS(ty - y) / a)
ELSEIF ABS(ty - y) > ABS(tx - x) THEN
        IF ty - y < 0 THEN dys = -speed
        IF ty - y > 0 THEN dys = speed
        a = ABS(ty - y) / speed
        IF tx - x < 0 THEN dxs = -(ABS(tx - x) / a)
        IF tx - x > 0 THEN dxs = (ABS(tx - x) / a)
END IF
skip:
dxs = CLNG(dxs)
dys = CLNG(dys)
IF ys < dys THEN ys = ys + 1
IF ys > dys THEN ys = ys - 1
IF xs < dxs THEN xs = xs + 1
IF xs > dxs THEN xs = xs - 1
xs = dxs
ys = dys
tx = tx + txs
ty = ty + tys
IF tx > 635 THEN txs = -txs
IF tx < 5 THEN txs = -txs
IF ty > 475 THEN tys = -tys
IF ty < 5 THEN tys = -tys

CIRCLE (320, 240), 1, 11
CIRCLE (320, 240), 2, 9
CIRCLE (320, 240), 3, 9
CIRCLE (320, 240), 4, 1
CIRCLE (320, 240), 5, 1

x = x + xs
y = y + ys
CIRCLE (x, y), 1, 11
CIRCLE (x, y), 2, 9
CIRCLE (x, y), 3, 9
CIRCLE (x, y), 4, 1
CIRCLE (x, y), 5, 1

FOR i = 1 TO 2000
NEXT i
aaa& = TIMER
IF aaa& - startedd& >= timee THEN
        aaa& = 0
        startedd& = TIMER
        CLS
        GOTO start
END IF

a$ = INKEY$
IF a$ = " " THEN
        CLS
        GOTO start
END IF
IF a$ = "q" THEN END
LOOP






