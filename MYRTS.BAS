DECLARE SUB pfind (ux!, uy!, dx!, dy!)
'$INCLUDE: 'qb.bi'
COMMON SHARED cx, cy, cant
SCREEN 13
FOR i = 0 TO 20: PALETTE i, i * 3 + 256 * (i * 3) + 65536 * (i * 3): NEXT i
FOR i = 21 TO 30: PALETTE i, 65536 * ((i - 20) * 6): NEXT i
FOR i = 31 TO 40: PALETTE i, (i - 30) * 6 + 256 * (i - 30) * 6 + 65536 * 60: NEXT i
FOR i = 41 TO 50: PALETTE i, 256 * ((i - 40) * 6): NEXT i
FOR i = 51 TO 60: PALETTE i, (i - 50) * 6 + 256 * 60 + 65536 * (i - 50) * 6: NEXT i
FOR i = 61 TO 70: PALETTE i, 256 * ((i - 60) * 6) + 65536 * ((i - 60) * 6): NEXT i
FOR i = 71 TO 80: PALETTE i, (i - 70) * 6 + 256 * 60 + 65536 * 60: NEXT i
FOR i = 81 TO 90: PALETTE i, ((i - 80) * 6): NEXT i
FOR i = 91 TO 100: PALETTE i, 60 + 256 * (i - 90) * 6 + 65536 * (i - 90) * 6: NEXT i
FOR i = 101 TO 110: PALETTE i, ((i - 100) * 6) + ((i - 100) * 6) * 65536: NEXT i
FOR i = 111 TO 120: PALETTE i, 60 + 256 * (i - 110) * 6 + 65536 * 60: NEXT i
FOR i = 121 TO 130: PALETTE i, ((i - 120) * 6) + ((i - 120) * 6) * 256: NEXT i
FOR i = 131 TO 140: PALETTE i, 60 + 256 * 60 + 65536 * (i - 130) * 6: NEXT i
FOR i = 141 TO 150: PALETTE i, ((i - 140) * 6) + ((i - 140) * 3) * 256: NEXT i
FOR i = 151 TO 160: PALETTE i, 60 + 256 * (30 + ((i - 150) * 3)) + 65536 * (i - 150) * 6: NEXT i
FOR i = 161 TO 170: PALETTE i, 65536 * ((i - 160) * 6): NEXT i
RANDOMIZE TIMER
DIM regs AS RegType
DIM uexist(100), ux(100), uy(100), uxdiff(100), uydiff(100), uteam(100), uhp(100), desx(100), desy(100) AS INTEGER
DIM SHARED map(100, 100, 2) AS INTEGER
DIM occ(100, 100) AS INTEGER
DIM land(300) AS INTEGER
DIM minimap(324) AS INTEGER
DIM mousesp(25), mousespm(25), mousebg(25) AS INTEGER
DIM selected(30)
DIM l(128) AS STRING
DIM bg(100) AS INTEGER
DIM SHARED chx(100, 100), chy(100, 100), yy(2), xx(2), ax(8), ay(8), start(8) AS INTEGER
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
GOSUB letters
FOR x = 0 TO 4: FOR y = 0 TO 4
READ test
PSET (x, y), test
IF test = 0 THEN test = 255
PSET (x, y + 5), test
NEXT: NEXT
GET (0, 0)-(4, 4), mousesp
GET (0, 5)-(4, 9), mousespm
	
FOR x = 0 TO 9: FOR y = 0 TO 9
READ test
PSET (x, y), test
NEXT y: NEXT x
GET (0, 0)-(9, 9), land(0)
FOR x = 0 TO 9: FOR y = 0 TO 9
READ test
PSET (x, y), test + 20
NEXT y: NEXT x
GET (0, 0)-(9, 9), land(100)
FOR x = 0 TO 9: FOR y = 0 TO 9
READ test
PSET (x, y), test
NEXT y: NEXT x
GET (0, 0)-(9, 9), land(200)
CLS
FOR y = 0 TO 9: FOR x = 0 TO 9
READ test
PSET (x, y), test
NEXT: NEXT
GET (0, 0)-(9, 9), bg
FOR x2 = 31 TO 0 STEP -1: FOR y2 = 19 TO 0 STEP -1
PUT (x2 * 10, y2 * 10), bg, PSET
NEXT: NEXT

GOSUB mapgen

screenx = 1
screeny = 1
first = 1
GOTO redraw
begin:
first = 0
FOR i = 1 TO 5

ux(i) = INT(RND * 10) + 1
uy(i) = INT(RND * 10) + 1
desx(i) = ux(i)
desy(i) = uy(i)
uexist(i) = 1
uteam(i) = 1
uhp(i) = 1
NEXT i
playerteam = 1
GET (mx, my)-(mx + 4, my + 4), mousebg
DO
FOR i = 1 TO 100
IF uexist(i) = 1 THEN
	occ(ux(i), uy(i)) = i
	PSET (ux(i) + 9, uy(i) + 9), uteam(i) * 20 + 10
	IF (ux(i) <> desx(i) OR uy(i) <> desy(i)) AND uxdiff(i) = 0 AND uydiff(i) = 0 AND p(i) = 0 THEN
		ux = ux(i): uy = uy(i): dx = desx(i): dy = desy(i)
		occ(ux(i), uy(i)) = 0
		CALL pfind(ux, uy, dx, dy)
		IF cant = 1 THEN
			IF desx(i) > ux(i) THEN desx(i) = desx(i) - 1
			IF desx(i) < ux(i) THEN desx(i) = desx(i) + 1
			IF desy(i) > uy(i) THEN desy(i) = desy(i) - 1
			IF desy(i) < uy(i) THEN desy(i) = desy(i) + 1
		END IF
		PSET (ux(i) + 9, uy(i) + 9), map(ux(i), uy(i), 2)
		LINE (screenx + 9, screeny + 9)-(screenx + 27, screeny + 26), 20, B
		ox = ux(i): oy = uy(i)
		ux(i) = cx: uy(i) = cy
		IF ux(i) > ox THEN uxdiff(i) = -9
		IF ux(i) < ox THEN uxdiff(i) = 9
		IF uy(i) > oy THEN uydiff(i) = -9
		IF uy(i) < oy THEN uydiff(i) = 9
		occ(ux(i), uy(i)) = i
	END IF
	IF uxdiff(i) > 0 THEN
		uxdiff(i) = uxdiff(i) - 1
		IF ux(i) >= screenx - 1 AND ux(i) <= screenx + 18 AND uy(i) >= screeny AND uy(i) < screeny + 18 THEN
			FOR j = -1 TO 1
			x = (ux(i) + 1 - screenx)
			IF x < 0 THEN x = 0
			IF x > 18 THEN x = 18
			y = (uy(i) + j - screeny)
			IF y < 0 THEN y = 0
			IF y > 17 THEN y = 17
			PUT (x * 10 + 120, y * 10 + 10), land((map(x + screenx, y + screeny, 1) - 1) * 100), PSET
			NEXT j
			IF ux(i) >= screenx THEN PUT ((ux(i) - screenx) * 10 + 120, (uy(i) - screeny) * 10 + 10), land((map(ux(i), uy(i), 1) - 1) * 100), PSET
		END IF
	ELSEIF uxdiff(i) < 0 THEN
		uxdiff(i) = uxdiff(i) + 1
		IF ux(i) >= screenx AND ux(i) <= screenx + 19 AND uy(i) >= screeny AND uy(i) < screeny + 18 THEN
			FOR j = -1 TO 1
			x = (ux(i) - 1 - screenx)
			IF x < 0 THEN x = 0
			IF x > 18 THEN x = 18
			y = (uy(i) + j - screeny)
			IF y < 0 THEN y = 0
			IF y > 17 THEN y = 17
			PUT (x * 10 + 120, y * 10 + 10), land((map(x + screenx, y + screeny, 1) - 1) * 100), PSET
			NEXT j
			IF ux(i) <= screenx + 18 THEN PUT ((ux(i) - screenx) * 10 + 120, (uy(i) - screeny) * 10 + 10), land((map(ux(i), uy(i), 1) - 1) * 100), PSET
		END IF
	END IF
	IF uydiff(i) > 0 THEN
		uydiff(i) = uydiff(i) - 1
		IF ux(i) >= screenx AND ux(i) <= screenx + 18 AND uy(i) >= screeny - 1 AND uy(i) < screeny + 18 THEN
			FOR j = -1 TO 1
			x = (ux(i) + j - screenx)
			IF x < 0 THEN x = 0
			IF x > 18 THEN x = 18
			y = (uy(i) + 1 - screeny)
			IF y < 0 THEN y = 0
			IF y > 17 THEN y = 17
			PUT (x * 10 + 120, y * 10 + 10), land((map(x + screenx, y + screeny, 1) - 1) * 100), PSET
			NEXT j
			IF uy(i) >= screeny THEN PUT ((ux(i) - screenx) * 10 + 120, (uy(i) - screeny) * 10 + 10), land((map(ux(i), uy(i), 1) - 1) * 100), PSET
		END IF
	ELSEIF uydiff(i) < 0 THEN
		uydiff(i) = uydiff(i) + 1
		IF ux(i) >= screenx AND ux(i) <= screenx + 18 AND uy(i) >= screeny AND uy(i) < screeny + 19 THEN
			FOR j = -1 TO 1
			x = (ux(i) + j - screenx)
			IF x < 0 THEN x = 0
			IF x > 18 THEN x = 18
			y = (uy(i) - 1 - screeny)
			IF y < 0 THEN y = 0
			IF y > 17 THEN y = 17
			PUT (x * 10 + 120, y * 10 + 10), land((map(x + screenx, y + screeny, 1) - 1) * 100), PSET
			NEXT j
			IF uy(i) < screeny + 18 THEN PUT ((ux(i) - screenx) * 10 + 120, (uy(i) - screeny) * 10 + 10), land((map(ux(i), uy(i), 1) - 1) * 100), PSET
		END IF
	END IF
	IF (ux(i) - screenx) * 10 + 120 + uxdiff(i) >= 120 AND (ux(i) - screenx) * 10 + 129 + uxdiff(i) <= 309 AND (uy(i) - screeny) * 10 + 10 + uydiff(i) >= 10 AND (uy(i) - screeny) * 10 + 10 + uydiff(i) <= 180 THEN
		LINE ((ux(i) - screenx) * 10 + 120 + uxdiff(i), (uy(i) - screeny) * 10 + 10 + uydiff(i))-((ux(i) - screenx) * 10 + 129 + uxdiff(i), (uy(i) - screeny) * 10 + 19 + uydiff(i)), uteam(i) * 20 + 15, BF
		IF selected(1) = i THEN LINE ((ux(i) - screenx) * 10 + 120 + uxdiff(i), (uy(i) - screeny) * 10 + 10 + uydiff(i))-((ux(i) - screenx) * 10 + 129 + uxdiff(i), (uy(i) - screeny) * 10 + 19 + uydiff(i)), 20, B
	END IF
END IF
NEXT i
PUT (mx, my), mousebg, PSET
regs.ax = 3
CALL INTERRUPT(&H33, regs, regs)
mx = regs.cx / 2 - 5
my = regs.dx - 5
b1 = regs.bx AND 1
b2 = regs.bx AND 2

IF b1 THEN
	IF mx > 9 AND mx <= 109 AND my > 9 AND my <= 109 THEN
		olx = screenx
		oly = screeny
		GOSUB redrawminimap
		screenx = mx - 18
		screeny = my - 18
		IF screenx > 82 THEN screenx = 82
		IF screenx < 1 THEN screenx = 1
		IF screeny > 83 THEN screeny = 83
		IF screeny < 1 THEN screeny = 1
		IF (screenx <> olx OR screeny <> oly) THEN GOTO redraw ELSE LINE (screenx + 9, screeny + 9)-(screenx + 27, screeny + 26), 20, B
	END IF
	IF my >= 10 AND my <= 189 AND mx >= 120 AND mx <= 309 THEN
		x = INT((mx - 120) / 10)
		y = INT((my - 10) / 10)
		IF x > 18 THEN x = 18
		IF y > 17 THEN x = 17
		x = x + screenx
		y = y + screeny
		FOR xx = 0 TO 11
		FOR yy = 11 TO 19
		PUT (xx * 10, yy * 10), bg, PSET
		PUT (xx * 10, yy * 10), bg, PSET
		NEXT: NEXT
		selected(1) = occ(x, y)
	END IF
END IF
IF b2 THEN
	IF selected(1) <> 0 AND my >= 10 AND my <= 189 AND mx >= 120 AND mx <= 309 THEN
		x = INT((mx - 120) / 10)
		y = INT((my - 10) / 10)
		IF x > 18 THEN x = 18
		IF y > 17 THEN x = 17
		x = x + screenx
		y = y + screeny
		IF map(x, y, 1) = 1 THEN
			desx(selected(1)) = x
			desy(selected(1)) = y
		END IF
		p(selected(1)) = 0
		FOR xx = 0 TO 11
		FOR yy = 11 TO 19
		PUT (xx * 10, yy * 10), bg, PSET
		PUT (xx * 10, yy * 10), bg, PSET
		NEXT: NEXT
	END IF
	IF selected(1) <> 0 AND mx > 9 AND mx <= 109 AND my > 9 AND my <= 109 THEN
		desx(selected(1)) = mx - 9
		desy(selected(1)) = my - 9
		p(selected(1)) = 0
	END IF
END IF
IF selected(1) <> 0 THEN
	yy = 13
	FOR xx = 0 TO 11
	PUT (xx * 10, yy * 10), bg, PSET
	PUT (xx * 10, yy * 10), bg, PSET
	NEXT
	msg$ = STR$(selected(1))
	PSET (4, 116), POINT(4, 116)
	DRAW "C20"
	FOR i = 1 TO LEN(msg$)
	DRAW l(ASC(MID$(msg$, i, 1)))
	NEXT i
       
	msg$ = STR$(desx(selected(1))) + ","
	PSET (4, 124), POINT(4, 122)
	DRAW "C20"
	FOR i = 1 TO LEN(msg$)
	DRAW l(ASC(MID$(msg$, i, 1)))
	NEXT i
	msg$ = STR$(desy(selected(1)))
	DRAW "BL4"
	FOR i = 1 TO LEN(msg$)
	DRAW l(ASC(MID$(msg$, i, 1)))
	NEXT i
	msg$ = STR$(uxdiff(selected(1))) + "," + STR$(uydiff(selected(1)))
	PSET (4, 132), POINT(4, 128)
	DRAW "C20"
	FOR i = 1 TO LEN(msg$)
	DRAW l(ASC(MID$(msg$, i, 1)))
	NEXT i
END IF

IF mx >= 314 AND my >= 194 AND screenx < 82 AND screeny < 83 THEN GOSUB redrawminimap: screeny = screeny + 1: screenx = screenx + 1: GOTO redraw
IF mx <= 0 AND my >= 194 AND screenx > 1 AND screeny < 83 THEN GOSUB redrawminimap: screeny = screeny + 1: screenx = screenx - 1: GOTO redraw
IF mx >= 314 AND my <= 0 AND screenx < 82 AND screeny > 1 THEN GOSUB redrawminimap: screeny = screeny - 1: screenx = screenx + 1: GOTO redraw
IF mx <= 0 AND my <= 0 AND screenx > 1 AND screeny > 1 THEN GOSUB redrawminimap: screeny = screeny - 1: screenx = screenx - 1: GOTO redraw

IF mx >= 314 AND screenx < 82 THEN GOSUB redrawminimap: screenx = screenx + 1: GOTO redraw
IF mx <= 0 AND screenx > 1 THEN GOSUB redrawminimap: screenx = screenx - 1: GOTO redraw
IF my >= 194 AND screeny < 83 THEN GOSUB redrawminimap: screeny = screeny + 1: GOTO redraw
IF my <= 0 AND screeny > 1 THEN GOSUB redrawminimap: screeny = screeny - 1: GOTO redraw

back:

IF mx >= 314 THEN mx = 314
IF mx <= 0 THEN mx = 0
IF my >= 194 THEN my = 194
IF my <= 0 THEN my = 0
GET (mx, my)-(mx + 4, my + 4), mousebg
PUT (mx, my), mousespm, AND
PUT (mx, my), mousesp, OR
WAIT &H3DA, 8
LOOP UNTIL INKEY$ <> ""
END
redrawminimap:
FOR x = screenx TO screenx + 18
PSET (x + 9, screeny + 9), map(x, screeny, 2)
PSET (x + 9, screeny + 26), map(x, screeny + 17, 2)
NEXT
FOR y = screeny TO screeny + 17
PSET (screenx + 9, y + 9), map(screenx, y, 2)
PSET (screenx + 27, y + 9), map(screenx + 18, y, 2)
NEXT
RETURN

redraw:
LINE (screenx + 9, screeny + 9)-(screenx + 27, screeny + 26), 20, B
x2 = 0
FOR x = screenx TO screenx + 18
y2 = 0
FOR y = screeny TO screeny + 17

IF map(x, y, 1) = 1 THEN PUT (x2 * 10 + 120, y2 * 10 + 10), land(0), PSET
IF map(x, y, 1) = 2 THEN PUT (x2 * 10 + 120, y2 * 10 + 10), land(100), PSET
IF map(x, y, 1) = 3 THEN PUT (x2 * 10 + 120, y2 * 10 + 10), land(200), PSET
y2 = y2 + 1
NEXT y
x2 = x2 + 1
NEXT x
IF first = 1 THEN GOTO begin
GOTO back


mapgen:
FOR x = 1 TO 100
FOR y = 1 TO 100
map(x, y, 1) = 1
NEXT y
NEXT x
lakes = 3
lakesize = 400
forests = 5
forestsize = 1000

FOR typ = 1 TO 0 STEP -1
IF typ = 0 THEN B = lakes
IF typ = 1 THEN B = forests
FOR i = 1 TO B
x = INT(RND * 100) + 1
y = INT(RND * 100) + 1
IF typ = 0 THEN c = lakesize + INT(RND * 11) - 5
IF typ = 1 THEN c = forestsize + INT(RND * 11) - 5
FOR j = 1 TO c
x = x + INT(RND * 3) - 1
y = y + INT(RND * 3) - 1
IF x < 3 THEN x = 3
IF y < 3 THEN y = 3
IF x > 98 THEN x = 98
IF y > 98 THEN y = 98
FOR k = -2 TO 2: FOR l = -2 TO 2
IF ABS(k) + ABS(l) < 4 THEN
	IF map(x + k, y + l, 1) = 3 THEN
		IF INT(RND * 2) = 1 THEN map(x + k, y + l, 1) = 2 + typ
	ELSE
		map(x + k, y + l, 1) = 2 + typ
	END IF
END IF
NEXT: NEXT
NEXT j
NEXT i
NEXT typ
FOR x = 1 TO 100
FOR y = 1 TO 100
IF map(x, y, 1) = 1 THEN PSET (x + 9, y + 9), INT(RND * 3) + 46: map(x, y, 2) = POINT(x + 9, y + 9)
IF map(x, y, 1) = 2 THEN PSET (x + 9, y + 9), INT(RND * 5) + 28: map(x, y, 2) = POINT(x + 9, y + 9)
IF map(x, y, 1) = 3 THEN PSET (x + 9, y + 9), INT(RND * 5) + 42: map(x, y, 2) = POINT(x + 9, y + 9)
NEXT y
NEXT x

RETURN

letters:
l(57) = "R2 U4 L2 D2 R2 BD2 BR2"
l(56) = "R2 U4 L2 D4 BU2 R2 BD2 BR2"
l(55) = "BU4 R2 D4 BR2"
l(54) = "R2 U2 L2 ND2 U2 R2 BD4 BR2"
l(53) = "R2 U2 L2 U2 R2 BD4 BR2"
l(52) = "BR2 U2 L2 U2 BR2 D2 BD2 BR2"
l(51) = "R2 U2 NL2 U2 NL2 BD4 BR2"
l(50) = "BR2 L2 U2 R2 U2 NL2 BD4 BR2"
l(49) = "BR2 NU4 BR2"
l(48) = "U4 R2 D4 NL2 BR2"
l(65) = "U4 R2 D2 L R D2 BR2"
l(66) = "U4 R1 F G F G L BR4"
l(67) = "BR2 L H U2 E R BD4 BR2"
l(68) = "U4 R F D2 G BR3"
l(69) = "U4 NR2 BD2 NR2 BD2 R2 BR2"
l(70) = "U4 NR2 BD2 NR1 BD2 BR4"
l(71) = "BU3 E R F BD2 L R G L H U BD2 BR5"
l(72) = "U4 BD2 R2 BU2 D4 BR2"
l(73) = "R2 BL U4 BL R2 BD4 BR2"
l(74) = "BU F R U4 L BR3 BD4"
l(75) = "U4 BD2 NE2 F2 BR2"
l(76) = "NU4 R2 BR2"
l(77) = "U4 F2 E2 D4 BR2"
l(78) = "U4 F4 NU4 BR2"
l(79) = "BR1 H U2 E F D2 BD BR2"
l(80) = "U4 R F G BD2 BR3"
l(81) = "BR1 H U2 E F D2 BD NH BR2"
l(82) = "U4 R F G F D BR2"
l(83) = "R E H2 E R BD4 BR2"
l(84) = "BR U4 NL R BD4 BR2"
l(85) = "NU4 R2 NU4 BR2"
l(86) = "BU4 D3 F E U3 BD4 BR2"
l(87) = "NU4 E2 F2 NU4 BR2"
l(88) = "U E2 U BL2 D F2 D BR2"
l(89) = "BR U2 H NU BF E U BD4 BR2"
l(90) = "BU4 R2 D G2 D R2 BR2"
l(45) = "BU2 R2 BD2 BR2"
l(44) = "ND1 BR2"
l(32) = "BR4"
RETURN

DATA 20,18,16,14,12
DATA 18,00,00,14,00
DATA 16,00,16,00,00
DATA 14,14,00,00,00
DATA 12,00,00,00,00

DATA 46,47,47,48,46,46,47,48,46,47
DATA 46,47,46,46,47,48,48,47,45,46
DATA 47,46,47,47,48,47,46,48,46,47
DATA 48,47,47,46,46,46,47,48,46,47
DATA 46,47,48,46,47,47,46,47,48,46
DATA 46,47,48,47,47,46,48,47,46,48
DATA 47,48,47,46,47,46,47,46,48,47
DATA 48,46,45,46,48,47,47,46,47,48
DATA 46,47,48,47,47,48,46,46,47,46
DATA 47,48,46,47,48,47,48,46,47,48

DATA 04,05,06,05,06,07,07,07,06,05
DATA 05,04,05,06,05,05,06,06,07,08
DATA 06,05,06,05,05,04,05,06,07,08
DATA 07,06,07,06,05,05,04,05,06,07
DATA 06,07,08,07,06,05,04,05,05,06
DATA 05,06,07,06,05,06,05,06,06,06
DATA 04,05,06,05,06,07,06,06,07,06
DATA 05,06,05,06,07,08,07,07,06,07
DATA 06,07,06,07,08,07,08,07,06,06
DATA 05,06,07,06,07,08,07,06,05,06
		       
DATA 46,42,45,43,43,41,43,47,46,45
DATA 42,43,43,43,43,43,42,43,44,46
DATA 41,43,44,42,43,43,43,43,43,42
DATA 43,41,143,42,43,43,42,43,41,43
DATA 43,43,42,41,43,42,143,43,43,46
DATA 47,43,43,43,44,43,43,42,43,43
DATA 46,42,43,41,43,42,43,41,44,42
DATA 41,42,43,43,41,43,43,43,42,46
DATA 43,144,43,42,41,43,43,44,41,46
DATA 46,42,43,45,43,43,44,44,46,47

DATA 145,147,144,142,150,147,143,141,145,147
DATA 145,147,144,143,150,147,142,141,146,146
DATA 145,147,144,143,149,146,142,141,146,146
DATA 144,146,145,143,149,146,142,142,145,146
DATA 144,146,145,142,150,146,143,142,145,145
DATA 144,146,146,142,150,145,143,142,145,145
DATA 145,145,146,142,150,145,143,142,145,145
DATA 145,145,145,143,149,146,144,143,144,146
DATA 144,145,145,143,149,146,144,143,144,146
DATA 144,146,145,142,149,147,144,143,144,147

SUB pfind (ux, uy, dx, dy)
cant = 0
c = 0
FOR x = ux - 3 TO ux + 3
FOR y = uy - 3 TO uy + 3
IF x < 1 OR x > 100 OR y < 1 OR y > 100 THEN c = c + 1: GOTO skip5
IF map(x, y, 1) = 1 THEN c = c + 1
skip5:
NEXT y: NEXT x
IF c = 49 THEN
	cx = ux
	cy = uy
	IF dx > ux THEN cx = cx + 1
	IF dx < ux THEN cx = cx - 1
	IF dy > uy THEN cy = cy + 1
	IF dy < uy THEN cy = cy - 1
	GOTO nofind
END IF
xx = ux
yy = uy
DO
c = 0
IF dx > xx THEN xx = xx + 1
IF dx < xx THEN xx = xx - 1
IF dy > yy THEN yy = yy + 1
IF dy < yy THEN yy = yy - 1
FOR x = xx - 3 TO xx + 3
FOR y = yy - 3 TO yy + 3
IF x < 1 OR x > 100 OR y < 1 OR y > 100 THEN c = c + 1: GOTO skip6
IF map(x, y, 1) = 1 THEN c = c + 1
skip6:
NEXT y: NEXT x
LOOP UNTIL c = 49 OR (xx = dx AND yy = dy)
dx = xx
dy = yy

chx(ux, uy) = ux
chy(ux, uy) = uy
x = ux
y = uy
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
IF x > 100 OR x < 1 THEN GOTO skip2
IF yy(a) > 100 OR yy(a) < 1 THEN GOTO skip2
IF chx(x, yy(a)) <> 0 OR chy(x, yy(a)) <> 0 THEN GOTO skip2
IF map(x, yy(a), 1) = 1 THEN
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
	B = 1
	FOR j = 2 TO 8 STEP 2
	start(j) = start(1) + B
	IF start(j) > 8 THEN start(j) = start(j) - 8
	IF j < 8 THEN
		start(j + 1) = start(1) - B
		IF start(j + 1) < 1 THEN start(j + 1) = start(j + 1) + 8
	END IF
	B = B + 1
	NEXT j
	FOR k = 1 TO 8
	j = start(k)
	IF x + ax(j) > 100 OR x + ax(j) < 1 OR yy(a) + ay(j) > 100 OR yy(a) + ay(j) < 1 THEN GOTO skip
	IF (chx(x, yy(a)) = 0 AND chy(x, yy(a)) = 0) AND ((chx(x + ax(j), yy(a) + ay(j)) <> 0 OR chy(x + ax(j), yy(a) + ay(j)) <> 0)) THEN
		chx(x, yy(a)) = x + ax(j)
		chy(x, yy(a)) = yy(a) + ay(j)
		IF x = dx AND yy(a) = dy THEN GOTO done
	END IF
skip:
	NEXT k
END IF
skip2:
NEXT a
NEXT x

FOR y = uy - i TO uy + i
xx(1) = ux - i
xx(2) = ux + i
FOR a = 1 TO 2
IF xx(a) > 100 OR xx(a) < 1 THEN GOTO skip4
IF y > 100 OR y < 1 THEN GOTO skip4
IF chx(xx(a), y) <> 0 OR chy(xx(a), y) <> 0 THEN GOTO skip4
IF map(xx(a), y, 1) = 1 THEN
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
	B = 1
	FOR j = 2 TO 8 STEP 2
	start(j) = start(1) + B
	IF start(j) > 8 THEN start(j) = start(j) - 8
	IF j < 8 THEN
		start(j + 1) = start(1) - B
		IF start(j + 1) < 1 THEN start(j + 1) = start(j + 1) + 8
	END IF
	B = B + 1
	NEXT j
	FOR k = 1 TO 8
	j = start(k)
	IF xx(a) + ax(j) > 100 OR xx(a) + ax(j) < 1 OR y + ay(j) > 100 OR y + ay(j) < 1 THEN GOTO skip3
	IF (chx(xx(a), y) = 0 AND chy(xx(a), y) = 0) AND ((chx(xx(a) + ax(j), y + ay(j)) <> 0 OR chy(xx(a) + ax(j), y + ay(j)) <> 0)) THEN
		chx(xx(a), y) = xx(a) + ax(j)
		chy(xx(a), y) = y + ay(j)
		IF xx(a) = dx AND y = dy THEN GOTO done
	END IF
	NEXT k
skip3:
END IF
skip4:
NEXT a
NEXT y
NEXT ii
IF chx(dx, dy) = 0 AND chy(dx, dy) = 0 AND diff < 40 THEN diff = diff + 1: GOTO restart
IF chx(dx, dy) = 0 AND chy(dx, dy) = 0 THEN cx = ux: cy = uy: cant = 1: GOTO nofind
done:
x = dx
y = dy
DO
ox = x
oy = y
x = chx(x, y)
y = chy(ox, y)
LOOP UNTIL x = ux AND y = uy
cx = ox
cy = oy
nofind:
ERASE chx, chy
END SUB

