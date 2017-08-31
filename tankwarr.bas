'TANKWAR.BAS - from http://www.qbasic.com
'By Stinky (gnvqit980002@coleg-powys.ac.uk)
'
TIMER ON
 ts = TIMER
FOR s = 1 TO 1000 STEP .1
ss = SIN(s)
NEXT s
ts = TIMER - ts
speed = CINT((ts / .269531) * 10000) / 10000
speed = speed * .6
speed = 1
clr = 63
SCREEN 9
COLOR 1, 15
PAINT (320, 240), 15, 15
INPUT "player 1 name"; plr1$
INPUT "player 2 name"; plr2$
snd:
i$ = INKEY$
IF i$ <> "" THEN IF ASC(i$) = 27 THEN SYSTEM
LOCATE 3, 1: PRINT "sound on?"
IF LCASE$(i$) = "y" THEN snd = 1: GOTO begin
IF LCASE$(i$) = "n" THEN snd = 0: GOTO begin
GOTO snd
begin:
COLOR 15
SCREEN 12
CLS
en1 = 20
en2 = 20
DIM expl(5, 8)
DIM expl2(2, 8)
tank = 0
c = 1
CONST pi = 3.141593
bom$ = "hl4d3r4e "
bit$ = "g2f2e2h2"
boom$ = "c4bl6e2u2r2e2f2r2d2f2g2d2l2g2h2l2u2h2br6p4,4c14bl3eurefrdfgdlghluhbr3s4p14,14"
arrow$ = "ta0c1h5u2r2u10r6d10r2d2g5bu4p1,1"
tank$ = "l7h2u2e2r14f2d2g2l11bu6u3er6fd3rg2df2l10e2uh2"
RANDOMIZE TIMER
by2 = 240
y = 240
lscape:
yr = 80 * RND(1)
xr = 6 * RND(1)
FOR la = 0 TO 6.283 STEP .1
by = y + SIN(la) * yr
x = x + xr
FOR p = 1 TO 5
LINE (x, by + p)-(x2, by2 + p), 2
NEXT p
sy = (480 - by) * RND(1)
PSET (x, by + sy), 2
by2 = by: x2 = x
IF x > 640 THEN GOTO fin
NEXT la
GOTO lscape
fin:
FOR d = 0 TO 30 STEP speed
PALETTE 8, INT(d)
PAINT (320, 470), 8, 2
NEXT
FOR skel = 1 TO 5
sk:
sx = INT(640 * RND(1))
sy = INT(100 * RND(1)) + 300
IF POINT(sx, sy) <> 8 THEN GOTO sk
PSET (sx, sy), 7: DRAW "c7dfr2dr2u6r2d8r2u8r2d8r2u6r2d3m+9,+2"
PSET (sx, sy), 7: DRAW "c7u2er2f2d4g4br15h4"
NEXT
LINE (1, 440)-(320, 460), 0, BF
LINE (0, 439)-(321, 461), 2, B
LINE (400, 420)-(500, 430), 6, BF
LINE (400, 433)-(500, 445), 6, BF
LINE (399, 419)-(501, 431), 2, B
LINE (399, 433)-(501, 445), 2, B
LINE (350 + en1 * 5, 420)-(450, 430), 0, BF
LINE (350 + en2 * 5, 433)-(450, 445), 0, BF
COLOR 2
LOCATE 27, 65: PRINT plr1$
LOCATE 28, 65: PRINT plr2$
tank2:
tx = INT(620 * RND(1)) + 10
FOR ty = 200 TO 480
IF POINT(tx, ty) > 0 THEN GOTO start
NEXT ty
start:
IF tank = 1 AND plr = 0 THEN IF SQR(((tx - qx) * (tx - qx)) + ((ty - qy) * (ty - qy))) < 200 THEN GOTO tank2 ELSE plr = 1
drw:
FOR fobj = ty - 20 TO 480
ft% = POINT(tx + 5 + (12 * SIN(ang + 1.570796)), fobj)
IF ft% > 0 THEN GOTO bk
NEXT fobj
bk:
FOR bobj = ty - 20 TO 480
bk% = POINT(tx - 5 - (12 * SIN(ang + 1.570796)), bobj)
IF bk% > 0 THEN GOTO calc
NEXT bobj
calc:
ang = -ATN((fobj - bobj) / ((tx + 12) - (tx - 12)))
ta$ = "ta" + STR$(INT(ang / (pi / 180)))
ty = ((bobj + fobj) / 2)
tch:
IF POINT(tx, ty) > 0 THEN ty = ty - 2: GOTO tch
IF POINT(tx, ty + 2) = 0 THEN ty = ty + 2: GOTO tch
IF tank = 1 THEN x = tx: y = ty: a$ = ta$
IF tank = 0 AND plr = 0 THEN tank = 1: qx = tx: qy = ty: qa$ = ta$:  GOTO tank2
IF tank = 0 AND plr > 0 THEN tank = 1: qx = tx: qy = ty: qa$ = ta$: tx = x: ty = y: GOTO drw:
PSET (qx, qy)
DRAW "c11" + STR$(c) + qa$ + tank$
PSET (x, y)
DRAW "c12" + STR$(c) + a$ + tank$
game:
IF plr = 1 THEN arx = qx: ary = qy - 20: ang$ = qa$: nary = y - 20: narx = x: plr$ = plr1$
IF plr = 2 THEN arx = x: ary = y - 20: ang$ = a$: nary = qy - 20: narx = qx: plr$ = plr2$
by = ary + 20: bx = arx
COLOR 12
LOCATE 25, 7: PRINT "WIND"
LOCATE 27, 35: PRINT "POWER"
wind = ((RND(1) - .5) / 60) * speed
LINE (20, 400)-(120, 410), 0, BF
LINE (19, 399)-(121, 411), 10, B
FOR w = 1 TO wind * 5000 / speed STEP (wind * 3) * speed
LINE (70, 400)-(70 + w, 405), 12
LINE (70, 410)-(70 + w, 405), 12
LINE (70, 400)-(70, 410), 10
NEXT w
arrow:
FOR ac = 63 TO 1 STEP -speed
PALETTE 1, ac
LINE (narx - 5, nary)-(narx + 5, nary - 20), 0, BF
PRESET (arx, ary), 0
DRAW arrow$
LOCATE INT((30 / 480) * ary) - 3, INT((80 / 640) * arx)
COLOR 1
PRINT plr$
COLOR 0
LOCATE INT((30 / 480) * nary) - 3, INT((80 / 640) * narx)
PRINT plr2$
i$ = INKEY$
IF i$ <> "" THEN IF ASC(i$) = 27 THEN SYSTEM
IF i$ <> "" THEN GOTO cont
NEXT
GOTO arrow
cont:
PALETTE 1, 0
PSET (arx, ary), 0
DRAW arrow$
LINE (arx - 5, ary)-(arx + 5, ary - 20), 0, BF
LOCATE INT((30 / 480) * ary) - 3, INT((80 / 640) * arx)
COLOR 0
PRINT plr$
ang$ = RIGHT$(ang$, 3)
ang = VAL(ang$) * (pi / 180)
gx = bx + COS(-ang - pi / 2) * 10
gy = by + SIN(-ang - pi / 2) * 10
a = ang
COLOR 11
gun:
bx2 = bx: by2 = by
bx = gx + COS(-a) * 14
by = gy + SIN(-a) * 14
LINE (gx, gy)-(bx, by), 15
IF bx2 <> bx OR by2 <> by THEN LINE (gx, gy)-(bx2, by2), 0
i$ = INKEY$
IF i$ <> "" THEN IF ASC(i$) = 27 THEN SYSTEM
IF i$ = "8" THEN a = a + .1 * speed
IF i$ = "2" THEN a = a - .1 * speed
LOCATE 27, 1: PRINT "ANGLE:"; INT((a / (pi / 180)))
IF i$ = " " THEN bx = gx + COS(-a) * 12: by = gy + SIN(-a) * 12: LINE (gx, gy)-(bx2, by2), 0: GOTO shoot
IF LCASE$(i$) = "t" THEN GOTO tport
IF (a - ang) < 0 THEN a = a + .1 * speed
IF (a - ang) > pi THEN a = a - .1 * speed
GOTO gun
shoot:
i$ = INKEY$
IF i$ <> "" THEN IF ASC(i$) = 27 THEN SYSTEM
IF pwr > 320 OR i$ = " " THEN byv = (SIN(a) * pwr / 750) * speed: bxv = (COS(a) * pwr / 750) * speed
IF pwr > 320 OR i$ = " " THEN IF snd = 1 THEN SOUND 200, 2
IF pwr > 320 OR i$ = " " THEN drag = bxv + wind: GOTO fire
pwr = pwr + .15 * speed
LINE (1, 440)-(pwr, 460), c, BF
c = 2
IF pwr > 100 THEN c = 12
IF pwr > 250 THEN c = 4
GOTO shoot:
fire:
bx2 = bx: by2 = by
bx = bx + bxv
bxv = bxv + wind / 100
IF bxv = drag THEN wind = 0
by = by - byv
byv = byv - (.0002 * speed)
IF bxv <> 0 AND byv <> 0 THEN ag = ATN(byv / bxv) + pi / 2
IF bxv < 0 THEN ag = ag + pi
ag2$ = ag$
ag$ = "ta" + STR$(INT((ag - (pi / 2)) / (pi / 180)))
PSET (bx, by), 13: DRAW ag$ + bom$
IF bx <> bx2 THEN PRESET (bx2, by2), 0: DRAW ag2$ + bom$
IF bx > 640 OR bx < 0 OR by > 420 OR POINT(bx + SIN(ag) * 4, by + COS(ag) * 4) > 0 THEN GOTO expl
GOTO fire
expl:
i$ = INKEY$: IF i$ <> "" THEN GOTO expl
IF bx > 640 THEN bx = 639
IF bx < 0 THEN bx = 1
IF snd = 1 THEN SOUND 100, 2
FOR boom = 4 TO 8 STEP 2 * speed
PSET (bx, by + 10), 0
DRAW "s" + STR$(INT(boom)) + "ta" + STR$(INT(boom * 44)) + boom$
FOR del = 1 TO 5000: NEXT del
CIRCLE (bx, by + 10), boom * 1.5, 0
PAINT (bx, by + 10), 0, 0
NEXT
pwr = 0
LINE (1, 440)-(320, 460), 0, BF
FOR shp = 1 TO 5
expl(1, shp) = bx
expl(2, shp) = by
expl(3, shp) = ((RND(1) - .5) / 2) * speed
expl(4, shp) = (-RND(1) - .25) * speed
expl(5, shp) = CINT(5 * RND(1)) + 7
NEXT shp
erk:
FOR xpl = 1 TO 5
expl(1, xpl) = expl(1, xpl) + expl(3, xpl)
expl(2, xpl) = expl(2, xpl) + expl(4, xpl)
expl(4, xpl) = expl(4, xpl) + .004 * speed
PSET (expl(1, xpl), expl(2, xpl)), expl(5, xpl)
DRAW bit$
PRESET (expl2(1, xpl), expl2(2, xpl)), 0
DRAW bit$
expl2(1, xpl) = expl(1, xpl)
expl2(2, xpl) = expl(2, xpl)
IF POINT(expl(1, xpl) + 4, expl(2, xpl) + 6) > 0 OR expl(2, xpl) > 400 OR expl(1, xpl) > 640 OR expl(1, xpl) < 0 THEN expl(3, xpl) = 0: expl(4, xpl) = 0: expl(5, xpl) = 1
IF expl(5, 1) = 1 AND expl(5, 2) = 1 AND expl(5, 3) = 1 AND expl(5, 4) = 1 AND expl(5, 5) = 1 THEN PRESET (expl(1, xpl), expl(2, xpl)), 0: DRAW bit$: gy% = by: GOTO ground
NEXT
GOTO erk
ground:
FOR gx = bx - 25 TO bx + 25
FOR gy = by + 25 TO by - 100 STEP -1
ns:
gd% = POINT(gx, gy)
IF gd% > 0 AND gd% < 15 THEN GOTO fall
NEXT
NEXT
GOTO plr
fall:
FOR fall% = gy TO 420
PSET (gx, fall%), 0:
PSET (gx, fall% + 1), gd%: ld% = POINT(gx, fall% + 2):
IF ld% > 0 AND ld% < 15 THEN GOTO ns
NEXT fall%
plr:
dist1 = SQR(((qx - bx) * (qx - bx)) + ((qy - by) * (qy - by)))
dist2 = SQR(((x - bx) * (x - bx)) + ((y - by) * (y - by)))
IF dist1 / 100 THEN en1 = en1 - (1 / (dist1 / 100))
IF dist2 / 100 THEN en2 = en2 - (1 / (dist2 / 100))
IF en < 0 THEN en = 0
IF en2 < 0 THEN en2 = 0
LINE (400 + en1 * 5, 420)-(500, 430), 0, BF
LINE (400 + en2 * 5, 433)-(500, 445), 0, BF
LINE (399, 419)-(501, 431), 2, B
LINE (399, 433)-(501, 446), 2, B
IF en2 = 0 AND en1 = 0 THEN dr = 1: GOTO win
IF en1 <= 0 THEN dx = qx: dy = qy - 10: dang$ = qang$: winner$ = plr2$: loser$ = plr1$: GOTO dead
IF en2 <= 0 THEN dx = x: dy = y - 10: dang$ = ang$: winner$ = plr1$: loser$ = plr2$: GOTO dead
GOTO xtport:
tport:
IF plr = 1 THEN PRESET (qx, qy): DRAW "c0" + qa$ + tank$:  ELSE PRESET (x, y): DRAW "c0" + a$ + tank$
LINE (gx, gy)-(bx, by), 0
px = INT(620 * RND(1)) + 10
IF snd = 1 THEN PLAY "l32o6c<g>f<<b>gc<e>>g"
FOR py = 160 TO 480
IF POINT(px, py + 5) > 0 THEN GOTO mov
NEXT
mov:
FOR cl = 1 TO 63 STEP 2
PALETTE 4, 64 - cl
CIRCLE (gx, gy - 5), 11 - cl2 / 6, 0
PAINT (gx, gy - 5), 0, 0
CIRCLE (gx, gy - 5), 11 - cl / 6, 4
PAINT (gx, gy - 5), 4, 4
PALETTE 4, cl
CIRCLE (px, py - 5), (cl / 6) + 1, 0
CIRCLE (px, py - 5), cl / 6, 4
PAINT (px, py - 5), 4, 4
CIRCLE (px, py - 5), cl / 6, 4
cl2 = cl
NEXT
CIRCLE (px, py - 5), (cl / 6), 0
PAINT (px, py - 4), 0, 0
PSET (gx, gy - 5), 0
PAINT (gx, gy - 5), 0, 0
IF plr = 1 THEN qx = px: qy = py:  ELSE x = px: y = py
xtport:
PRESET (qx, qy)
DRAW "c0" + qa$ + tank$
PRESET (x, y)
DRAW "c0" + a$ + tank$
IF plr = 1 THEN plr = 2: tank = 0: tx = qx: ty = qy: GOTO drw
IF plr = 2 THEN plr = 1: tank = 0: tx = qx: ty = qy: GOTO drw
dead:
FOR f = 1 TO 3
FOR d = 1 TO 63 STEP 5 * speed
PALETTE 1, d
IF snd = 1 THEN SOUND 37 + 20 * d, 1
LINE (dx - 7, dy - 10)-(dx + 7, dy - 25), 0, BF
PSET (dx, dy + 10), 1
DRAW "ta" + dang$ + tank$
NEXT
NEXT
PRESET (dx, dy + 10), 0
DRAW "ta" + dang$ + tank$
FOR shp = 1 TO 8
expl(1, shp) = dx
expl(2, shp) = dy
expl(3, shp) = (RND(1) - .5) * 4
expl(4, shp) = (-RND(1) - 1) * 2
expl(5, shp) = 3
NEXT shp
cr = 64
IF snd = 0 THEN GOTO derk
FOR blw = 1 TO 5
SOUND 200, 1
SOUND 37, 1
NEXT
derk:
cr = cr - .5
PALETTE 3, INT(cr)
FOR xpl = 1 TO 8
expl(1, xpl) = expl(1, xpl) + expl(3, xpl)
expl(2, xpl) = expl(2, xpl) + expl(4, xpl)
expl(4, xpl) = expl(4, xpl) + .04
PSET (expl(1, xpl), expl(2, xpl)), 3
1 DRAW bit$
expl2(1, xpl) = expl(1, xpl)
expl2(2, xpl) = expl(2, xpl)
IF cr = 1 THEN GOTO win
NEXT
GOTO derk
win:
SLEEP 1
CLS
COLOR 15
FOR cl = 1 TO 63 STEP 2 * speed
PALETTE 2, cl
COLOR 2
IF dr = 0 THEN LOCATE 10, 30: PRINT winner$ + " wins the game. Another game(y/n)?"
IF dr = 1 THEN LOCATE 10, 30: PRINT "round drawn: Another game?(y/n)"
i$ = INKEY$
IF i$ <> "" THEN IF ASC(i$) = 27 THEN SYSTEM
IF LCASE$(i$) = "y" OR LCASE$(i$) = "n" THEN GOTO dec
NEXT
GOTO win
dec:
IF LCASE$(i$) = "y" THEN RUN
SYSTEM

