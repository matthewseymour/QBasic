SCREEN 12
RANDOMIZE TIMER
SLEEP

ts = 2
speed = 5

start:
CLS

tx = INT(RND * 630) + 5
ty = INT(RND * 470) + 5
tz = INT(RND * 470) + 5
txs = INT(RND * (ts * 2)) - ts
tys = INT(RND * (ts * 2)) - ts
tzs = INT(RND * (ts * 2)) - ts

x = 320
y = 240
z = 200

xs = INT(RND * (speed * 2)) - speed
ys = INT(RND * (speed * 2)) - speed
zs = INT(RND * (speed * 2)) - speed
dxs = INT(RND * (speed * 2)) - speed
dys = INT(RND * (speed * 2)) - speed
zys = INT(RND * (speed * 2)) - speed

DO
FOR i = 1 TO 10000
NEXT i

IF ABS(tz - z) < CLNG(speed / 2) + 2 AND ABS(tx - x) < CLNG(speed / 2) + 2 AND ABS(ty - y) < CLNG(speed / 2) + 2 THEN
        GOTO start
END IF
IF ABS(tx - x) > ABS(ty - y) AND ABS(tx - x) > ABS(tz - z) THEN
        IF tx - x < 0 THEN dxs = -speed
        IF tx - x > 0 THEN dxs = speed
        a = ABS(tx - x) / speed
        IF ty - y < 0 THEN dys = -(ABS(ty - y) / a)
        IF ty - y > 0 THEN dys = (ABS(ty - y) / a)
        IF tz - z < 0 THEN dzs = -(ABS(tz - y) / a)
        IF tz - z > 0 THEN dzs = (ABS(tz - y) / a)
ELSEIF ABS(ty - y) > ABS(tx - x) AND ABS(ty - y) > ABS(tz - z) THEN
        IF ty - y < 0 THEN dys = -speed
        IF ty - y > 0 THEN dys = speed
        a = ABS(ty - y) / speed
        IF tx - x < 0 THEN dxs = -(ABS(tx - x) / a)
        IF tx - x > 0 THEN dxs = (ABS(tx - x) / a)
        IF tz - z < 0 THEN dzs = -(ABS(tz - y) / a)
        IF tz - z > 0 THEN dzs = (ABS(tz - y) / a)
ELSEIF ABS(tz - z) > ABS(tx - x) AND ABS(tz - z) > ABS(ty - y) THEN
        IF tz - z < 0 THEN dzs = -speed
        IF tz - z > 0 THEN dzs = speed
        a = ABS(tz - z) / speed
        IF tx - x < 0 THEN dxs = -(ABS(tx - x) / a)
        IF tx - x > 0 THEN dxs = (ABS(tx - x) / a)
        IF ty - y < 0 THEN dys = -(ABS(ty - y) / a)
        IF ty - y > 0 THEN dys = (ABS(ty - y) / a)
END IF
skip:
dxs = CLNG(dxs)
dys = CLNG(dys)
dzs = CLNG(dzs)
IF zs < dzs THEN zs = zs + 1
IF zs > dzs THEN zs = zs - 1
IF ys < dys THEN ys = ys + 1
IF ys > dys THEN ys = ys - 1
IF xs < dxs THEN xs = xs + 1
IF xs > dxs THEN xs = xs - 1
CIRCLE (320, 180), 3, 4
CIRCLE (320, 380), 3, 8
CIRCLE (tx, ((ty / 2) + 240) - tz), 4, 0
CIRCLE (tx, ((ty / 2) + 240)), 4, 0
tx = tx + txs
ty = ty + tys
tz = tz + tzs
CIRCLE (tx, ((ty / 2) + 240) - tz), 4, 15
CIRCLE (tx, ((ty / 2) + 240)), 4, 8
IF tx > 635 THEN txs = -txs
IF tx < 5 THEN txs = -txs
IF ty > 475 THEN tys = -tys
IF ty < 5 THEN tys = -tys
IF tz > 295 THEN tzs = -tzs
IF tz < 5 THEN tzs = -tzs
CIRCLE (x, ((y / 2) + 240) - z), 1, 0
CIRCLE (x, ((y / 2) + 240)), 1, 0
x = x + xs
y = y + ys
z = z + zs
CIRCLE (x, ((y / 2) + 240) - z), 1, 2
CIRCLE (x, ((y / 2) + 240)), 1, 8
FOR i = 1 TO 2000
NEXT i
a$ = INKEY$
IF a$ = " " THEN GOTO start
IF a$ = "q" THEN END
LOOP


