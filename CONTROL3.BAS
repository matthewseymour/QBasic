TYPE RegType
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
END TYPE
TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE
DECLARE SUB INTERRUPT (intnum AS INTEGER, inreg AS RegType, outreg AS RegType)
DECLARE SUB INTERRUPTX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB ABSOLUTE (address AS INTEGER)
DECLARE SUB INT86OLD (intnum AS INTEGER, inarray() AS INTEGER, outarray() AS INTEGER)
DECLARE SUB INT86XOLD (intnum AS INTEGER, inarray() AS INTEGER, outarray() AS INTEGER)
SCREEN 12
SLEEP 2
RANDOMIZE TIMER

DIM regs AS RegType
DIM selected(10) AS INTEGER
DIM am(150) AS INTEGER
DIM x(150) AS INTEGER
DIM y(150) AS INTEGER
DIM ba(150) AS INTEGER
DIM bda(150) AS INTEGER
DIM sp(150) AS INTEGER
DIM ta(150) AS INTEGER
DIM two(150) AS INTEGER
DIM tda(150) AS INTEGER
DIM ty(150) AS INTEGER
DIM eng(150) AS INTEGER
DIM tarx(150) AS INTEGER
DIM tary(150) AS INTEGER
DIM cool(150) AS INTEGER
DIM maxcool(150) AS INTEGER
DIM cool2(150) AS INTEGER
DIM col(150) AS INTEGER
DIM he(150) AS INTEGER

DIM sx(100) AS SINGLE
DIM sy(100) AS SINGLE
DIM sa(100) AS INTEGER
DIM sd(100) AS INTEGER
DIM so(100) AS INTEGER
DIM sl(100) AS INTEGER
DIM sat(100) AS INTEGER

DIM ex(50) AS INTEGER
DIM ey(50) AS INTEGER
DIM es(50) AS INTEGER
DIM emax(50) AS INTEGER

DIM cx(-92 TO 452)  AS DOUBLE
DIM cy(-92 TO 452) AS DOUBLE
Pi = 3.141592
FOR i = -92 TO 452
	B = i
	IF B > 360 THEN B = B - 360
	IF B < 0 THEN B = B + 360
	cx(i) = SIN(B * (Pi / 180))
	cy(i) = COS(B * (Pi / 180))
NEXT
FOR i = 1 TO 100
so(i) = -2
NEXT i
max = 15
bcolor = 6
'FOR i = 1 TO (INT(RND * 14) + 2)
FOR i = 1 TO 4
x(i) = INT(RND * 620) + 10
y(i) = INT(RND * 460) + 10
ty(i) = 0
col(i) = i
IF col(i) = bcolor THEN col(i) = 0
cool(i) = INT(RND * 100)
maxcool(i) = cool(i)
he(i) = 350
eng(i) = 1
bda(i) = INT(RND * 361)
NEXT i
LINE (0, 0)-(640, 480), bcolor, BF

DO
'timetoplay = timetoplay + 1
IF timetoplay >= 14 - ((speedodanotes - 5) / 2) THEN
	IF INT(RND * speedodanotes + 1) = 1 THEN speedodanotes = INT(RND * INT(RND * 28)) + 5: PLAY "L=" + VARPTR$(speedodanotes) + ""
	IF INT(RND * 5) = 1 THEN B = INT(RND * 6) + 1: PLAY "O=" + VARPTR$(B) + ""
	PLAY "MB" + CHR$(INT(RND * 7) + 65)
	timetoplay = 0
END IF
regs.ax = 3
CALL INTERRUPT(&H33, regs, regs)
x = regs.cx
y = regs.dx
b1 = regs.bx AND 1
b2 = regs.bx AND 2

LINE (ox - 5, oy)-(ox + 5, oy), 6
LINE (ox, oy - 5)-(ox, oy + 5), 6
IF olb1 <> 0 THEN LINE (ox, oy)-(stx, sty), 6, B
ox = x
oy = y
IF ox < 5 THEN ox = 5
IF ox > 634 THEN ox = 634
IF oy < 5 THEN oy = 5
IF oy > 474 THEN oy = 474
LINE (ox - 5, oy)-(ox + 5, oy), 15
LINE (ox, oy - 5)-(ox, oy + 5), 15
IF olb1 > 0 THEN
	IF olb1 > 0 THEN LINE (ox, oy)-(stx, sty), 15, B
END IF
IF b1 THEN
	attackmode = 0
	FOR i = 1 TO 10
	LINE (x(selected(i)) - 10, y(selected(i)) - 10)-(x(selected(i)) + 10, y(selected(i)) + 10), bcolor, B
	selected(i) = 0
	NEXT i
	FOR i = 1 TO 150
	IF col(i) = 1 AND he(i) > 0 THEN
		IF ABS(x(i) - ox) < 10 AND ABS(y(i) - oy) < 10 THEN selected(1) = i
	END IF
	NEXT i
	IF olb1 = 0 THEN stx = ox: sty = oy
	olb1 = 1
ELSE
	IF olb1 = 1 THEN
		FOR i = 1 TO 10
		LINE (x(selected(i)) - 10, y(selected(i)) - 10)-(x(selected(i)) + 10, y(selected(i)) + 10), bcolor, B
		NEXT i
		IF stx > ox THEN SWAP stx, ox: switchx = 1
		IF sty > oy THEN SWAP sty, oy: switchy = 1
		se = 1
		FOR i = 1 TO 150
		IF col(i) = 1 AND he(i) > 0 THEN
			IF x(i) < ox + 10 AND x(i) > stx - 10 AND y(i) < oy + 10 AND y(i) > sty - 10 THEN selected(se) = i: se = se + 1
		END IF
		IF se > 10 THEN EXIT FOR
		NEXT i
		IF switchx = 1 THEN SWAP stx, ox: switchx = 0
		IF switchy = 1 THEN SWAP sty, oy: switchy = 0
	END IF
	IF olb1 = 1 THEN LINE (ox, oy)-(stx, sty), 6, B
	stx = ox
	sty = oy
	olb1 = 0
END IF
IF b2 <> 0 AND olb2 = 0 THEN
	olb2 = 1
	FOR i = 1 TO 10
	IF selected(i) > 0 AND he(selected(i)) <> 0 THEN
		tarx(selected(i)) = ox
		tary(selected(i)) = oy
		am(selected(i)) = attackmode
	END IF
	NEXT i
	attackmode = 0
ELSEIF b2 = 0 THEN
	olb2 = 0
END IF

IF sleeper > -1 THEN sleeper = sleeper - 1
IF sleeper = 0 THEN SLEEP
FOR i = 1 TO 100
IF so(i) > -2 THEN
	PSET (sx(i), sy(i)), bcolor
	sx(i) = sx(i) + (cx(sa(i)) * 10)
	sy(i) = sy(i) + (cy(sa(i)) * 10)
	PSET (sx(i), sy(i)), 14
	IF bcolor = 14 THEN PSET (sx(i), sy(i)), 4
	sl(i) = sl(i) - 1
	IF sl(i) <= 0 THEN so(i) = -2
	IF sat(i) = 1 AND so(i) > -2 THEN GOTO skip2
	FOR j = 1 TO max
	IF ABS(x(j) - sx(i)) <= 10 + (5 * sat(i)) AND ABS(y(j) - sy(i)) <= 10 + (5 * sat(i)) AND he(j) > 1 AND col(j) <> so(i) THEN
		he(j) = he(j) - sd(i)
		IF ty(j) = 5 THEN tda(j) = INT(RND * 360)
		IF ty(j) = 0 THEN bda(j) = sa(i) + 180
		IF he(j) < 1 THEN he(j) = 1
		IF ty(j) = 4 THEN bda(j) = CLNG(sa(i) / 45) * 45
		so(i) = -2
		PSET (sx(i), sy(i)), bcolor
		IF sat(i) = 0 THEN EXIT FOR
	END IF
	NEXT j
skip2:   
	IF so(i) = -2 THEN
		PSET (sx(i), sy(i)), bcolor
		FOR l = 1 TO INT(RND * 3) + 1
		FOR k = 1 TO 50
		IF emax(k) = 0 THEN
			emax(k) = sd(i)
			es(k) = 0
			IF sat(i) = 1 THEN emax(k) = 12
			IF sat(i) = 1 THEN es(k) = 8
			ex(k) = sx(i) + INT(RND * 5) - 2
			ey(k) = sy(i) + INT(RND * 5) - 2
			EXIT FOR
		END IF
		NEXT k
		NEXT l
	END IF
END IF
NEXT i
FOR i = 1 TO 50
IF emax(i) > 0 THEN
	co = 4
	es(i) = es(i) + 1
	FOR j = es(i) TO 1 STEP -1
	CIRCLE (ex(i), ey(i)), j, co
	CIRCLE (ex(i) + 1, ey(i)), j, co
	IF co = 12 THEN co = 14
	IF co = 4 THEN co = 12
	NEXT j
	IF es(i) = emax(i) THEN LINE (ex(i) - emax(i), ey(i) - emax(i))-(ex(i) + emax(i) + 1, ey(i) + emax(i)), bcolor, BF: emax(i) = 0
END IF
NEXT i
FOR i = 1 TO 150
IF he(i) > 0 THEN
	FOR j = 1 TO 10
	IF selected(j) = i AND col(i) = 1 THEN LINE (x(i) - 10, y(i) - 10)-(x(i) + 10, y(i) + 10), bcolor, B
	NEXT j
	newmax = i
	IF ty(i) = 0 THEN
		LINE (x(i) - 5, y(i) - 5)-(x(i) + 5, y(i) + 5), col(i), BF
		LINE (x(i) - 6, y(i) + 8)-(x(i) - 2, y(i) + 8), 4
		LINE (x(i) - 1, y(i) + 8)-(x(i) + 2, y(i) + 8), 14
		LINE (x(i) + 2, y(i) + 8)-(x(i) + 6, y(i) + 8), 2
		LINE (x(i) + 7, y(i) + 8)-((x(i) - 5) + (12 * (he(i) / (250 + (100 * eng(i))))), y(i) + 8), 0
		LINE (x(i) - 7, y(i) + 7)-(x(i) + 7, y(i) + 9), 7, B
		IF maxcool(i) > 0 THEN
			LINE (x(i) - 6, y(i) + 10)-((x(i) + 6) - (12 * (cool(i) / maxcool(i))), y(i) + 10), 1
			LINE (x(i) + 7, y(i) + 10)-((x(i) + 7) - (12 * (cool(i) / maxcool(i))), y(i) + 10), bcolor
		END IF
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		IF cool2(i) > 0 THEN cool2(i) = cool2(i) - 1
		IF cool2(i) <= 0 AND he(i) < 250 THEN cool2(i) = 25: he(i) = he(i) + 1
		IF bda(i) > 360 THEN bda(i) = bda(i) - 360
		IF bda(i) < 0 THEN bda(i) = bda(i) + 360
		IF cool(i) <= 0 THEN
			FOR j = 1 TO 150
			IF he(j) = 0 THEN
				FOR k = 1 TO 10
				IF selected(k) = j THEN selected(k) = 0
				NEXT k
				max = max + 1
				cool(i) = 150 + (350 - he(i))
				maxcool(i) = cool(i)
				x(j) = x(i) + INT(RND * 15) - 7
				y(j) = y(i) + INT(RND * 15) - 7
				sp(j) = 1
				ba(j) = INT(RND * 8) * 45
				bda(j) = ba(j)
				two(j) = 0
				ta(j) = ba(j)
				tda(j) = ba(j)
				ty(j) = INT(RND * 3) + 1
				tarx(j) = tarx(i)
				tary(j) = tary(i)
				cool(j) = 10
				he(j) = 20 + (10 * ty(j))
				IF INT(RND * 15) = 1 THEN ty(j) = 4: cool(j) = 325: he(j) = 100
				IF INT(RND * 12) = 1 THEN ty(j) = 6: he(j) = 5
				IF INT(RND * 7) = 1 THEN
					ty(j) = 5
					he(j) = 100
					ann = (INT(RND * INT(RND * 360)) - 180) + bda(i)
					IF ann > 360 THEN ann = ann - 360
					IF ann < 0 THEN ann = ann + 360
					x(j) = x(i) + (cx(ann) * 30)
					y(j) = y(i) + (cy(ann) * 30)
				END IF
				IF ty(j) = 3 AND INT(RND * 2) = 1 THEN two(j) = 1
				col(j) = col(i)
				EXIT FOR
			END IF
			NEXT j
		END IF
	ELSEIF ty(i) >= 1 AND ty(i) <= 3 THEN
		LINE (x(i) - (ty(i) + 1), y(i) - (ty(i) + 1))-(x(i) + ty(i) + 1, y(i) + ty(i) + 1), bcolor, BF
		FOR j = 1 TO 2 + ty(i)
		CIRCLE (x(i), y(i)), j, bcolor
		NEXT j
		ba = ba(i)
		ta = ta(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), bcolor
		IF two(i) = 0 THEN DRAW "TA=" + VARPTR$(ta) + "D9" ELSE DRAW "TA=" + VARPTR$(ta) + "BL1 D9 BU9 BR2 D9"
		IF ty(i) = 2 THEN
			PSET (x(i), y(i)), bcolor
			DRAW "TA=" + VARPTR$(ta) + " BU2 BL4 U2 R8 D1 L8 D1 R8"
		END IF
		x(i) = x(i) + cx(ba(i)) * sp(i)
		y(i) = y(i) + cy(ba(i)) * sp(i)
		IF x(i) < 10 THEN x(i) = 10: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF x(i) > 630 THEN x(i) = 630: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF y(i) < 10 THEN y(i) = 10: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF y(i) > 470 THEN y(i) = 470: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF ba(i) <> bda(i) AND sp(i) > 0 THEN
			IF ABS(ba(i) - bda(i)) < 20 OR ABS(ba(i) - bda(i)) > 340 THEN ba(i) = bda(i)
			IF ABS(ba(i) - bda(i)) < 180 THEN
				IF bda(i) > ba(i) THEN ba(i) = ba(i) + 11.25
				IF bda(i) < ba(i) THEN ba(i) = ba(i) - 11.25
			ELSEIF ABS(ba(i) - bda(i)) >= 180 THEN
				IF bda(i) < ba(i) THEN ba(i) = ba(i) + 11.25
				IF bda(i) > ba(i) THEN ba(i) = ba(i) - 11.25
			END IF
		END IF
		IF ta(i) <> tda(i) THEN
			FOR j = 1 TO 10
			IF ABS(ta(i) - tda(i)) < 180 THEN
				IF tda(i) > ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) < ta(i) THEN ta(i) = ta(i) - 1
			ELSEIF ABS(ta(i) - tda(i)) >= 180 THEN
				IF tda(i) < ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) > ta(i) THEN ta(i) = ta(i) - 1
			END IF
			NEXT j
		END IF
		tda(i) = bda(i)
		IF tarx(i) <> 0 AND (eng(i) = 0 OR col(i) = 1) THEN
			sp(i) = 1
			IF ty(i) = 1 THEN sp(i) = 2
			diffx = x(i) - tarx(i)
			diffy = y(i) - tary(i)
			IF diffy = 0 THEN diffy = .1
			IF diffx = 0 THEN diffx = .1
			a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
			IF diffy < 0 AND diffx > 0 THEN a = 180 - a
			IF diffy < 0 AND diffx < 0 THEN a = 180 + a
			IF diffy > 0 AND diffx < 0 THEN a = 360 - a
			a = a + 180
			IF a > 360 THEN a = a - 360
			IF ty(i) = 1 THEN bda(i) = INT(a / 22.5) * 22.5 ELSE bda(i) = INT(a / 45) * 45
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
		END IF
		IF tarx(i) = 0 AND tary(i) = 0 AND col(i) = 1 THEN sp(i) = 0
		IF ty(i) = 1 AND sp(i) = 1 THEN bda(i) = CLNG(bda(i) / 45) * 45
		IF INT(RND * 50) = 1 AND col(i) <> 1 THEN IF ty(i) <> 1 THEN sp(i) = INT(RND * 2) ELSE sp(i) = INT(RND * 3)
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 150) = 1 AND col(i) <> 1 THEN IF ty(i) = 1 THEN bda(i) = INT(RND * 16) * 22.5 ELSE bda(i) = INT(RND * 8) * 45
		LINE (x(i) - (ty(i) + 1), y(i) - (ty(i) + 1))-(x(i) + ty(i) + 1, y(i) + ty(i) + 1), col(i), BF
		FOR j = 1 TO 2 + ty(i)
		CIRCLE (x(i), y(i)), j, col(i)
		NEXT j
		ba = ba(i)
		ta = ta(i)
		PSET (x(i), y(i)), col(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), col(i)
		IF two(i) = 0 THEN DRAW "TA=" + VARPTR$(ta) + "D9" ELSE DRAW "TA=" + VARPTR$(ta) + "BL1 D9 BU9 BR2 D9"
		IF ty(i) = 2 THEN
			PSET (x(i), y(i)), col(i)
			DRAW "TA=" + VARPTR$(ta) + " BU2 BL4 U2 R8 D1 L8 D1 R8"
		END IF
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		eng(i) = 0
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 50000 THEN
				eng(i) = 1
				IF col(i) <> 1 THEN tarx(i) = 0: tary(i) = 0
				IF col(i) <> 1 THEN sp(i) = 1
				IF ty(i) = 1 AND col(i) <> 1 THEN sp(i) = 2
				diffx = x(i) - x(j)
				diffy = y(i) - y(j)
				IF diffy = 0 THEN diffy = .1
				IF diffx = 0 THEN diffx = .1
				a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
				IF diffy < 0 AND diffx > 0 THEN a = 180 - a
				IF diffy < 0 AND diffx < 0 THEN a = 180 + a
				IF diffy > 0 AND diffx < 0 THEN a = 360 - a
				a = a + 180
				IF a > 360 THEN a = a - 360
				IF (col(i) <> 1 OR am(i) = 1) THEN IF ty(i) = 1 THEN bda(i) = CLNG(a / 22.5) * 22.5 ELSE bda(i) = CLNG(a / 45) * 45
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < (100 + (25 * (ty(i) - 1))) ^ 2 THEN
					IF col(i) <> 1 OR am(i) = 1 THEN sp(i) = 0
					tda(i) = a
					IF ta(i) = tda(i) AND cool(i) = 0 THEN
						diff = -90
restart:                                        FOR k = 1 TO 100
						IF so(k) = -2 THEN
							cool(i) = INT(RND * 3) + 14 + ty(i)
							sl(k) = 15 + ty(i)
							sat(k) = 0
							so(k) = col(i)
							sa(k) = ta(i)
							IF two(i) = 1 THEN sx(k) = x(i) + cx(ta(i) + diff) ELSE sx(k) = x(i)
							IF two(i) = 1 THEN sy(k) = y(i) + cy(ta(i) + diff) ELSE sy(k) = y(i)
							ran = INT(RND * 15)
							IF two(i) = 1 AND diff = -90 THEN sx(k) = sx(k) + cx(ta(i)) * ran: sy(k) = sy(k) + cy(ta(i)) * ran
							sd(k) = INT(RND * 3) + 3 + ty(i)
							EXIT FOR
						END IF
						NEXT k
						IF two(i) = 1 AND diff = -90 THEN diff = 90: GOTO restart
					END IF
				END IF
			END IF
		END IF
		IF he(j) > 0 AND col(i) = col(j) AND eng(j) = 0 AND tarx(j) = 0 AND eng(i) = 1 AND col(i) <> 1 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 250000 THEN
				sp(j) = 1
				tarx(j) = x(i)
				tary(j) = y(i)
			END IF
		END IF
		NEXT j
	ELSEIF ty(i) = 4 THEN
		PSET (x(i), y(i)), bcolor
		ba = ba(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		x(i) = x(i) + cx(ba(i)) * sp(i)
		y(i) = y(i) + cy(ba(i)) * sp(i)
		IF x(i) < 10 THEN x(i) = 10: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF x(i) > 630 THEN x(i) = 630: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF y(i) < 10 THEN y(i) = 10: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF y(i) > 470 THEN y(i) = 470: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF ba(i) <> bda(i) THEN
			IF ABS(ba(i) - bda(i)) < 20 OR ABS(ba(i) - bda(i)) > 340 THEN ba(i) = bda(i)
			IF ABS(ba(i) - bda(i)) < 180 THEN
				IF bda(i) > ba(i) THEN ba(i) = ba(i) + 11.25
				IF bda(i) < ba(i) THEN ba(i) = ba(i) - 11.25
			ELSEIF ABS(ba(i) - bda(i)) >= 180 THEN
				IF bda(i) < ba(i) THEN ba(i) = ba(i) + 11.25
				IF bda(i) > ba(i) THEN ba(i) = ba(i) - 11.25
			END IF
		END IF
		IF tarx(i) <> 0 AND col(i) = 1 THEN
			sp(i) = 1
			diffx = x(i) - tarx(i)
			diffy = y(i) - tary(i)
			IF diffy = 0 THEN diffy = .1
			IF diffx = 0 THEN diffx = .1
			a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
			IF diffy < 0 AND diffx > 0 THEN a = 180 - a
			IF diffy < 0 AND diffx < 0 THEN a = 180 + a
			IF diffy > 0 AND diffx < 0 THEN a = 360 - a
			a = a + 180
			IF a > 360 THEN a = a - 360
			bda(i) = CLNG(a / 45) * 45
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
		END IF
		IF tarx(i) = 0 AND tary(i) = 0 AND col(i) = 1 THEN sp(i) = 0
		IF col(i) <> 1 THEN sp(i) = 1
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF INT(RND * 50) = 1 AND col(i) <> 1 THEN bda(i) = INT(RND * 8) * 45
		ba = ba(i)
		PSET (x(i), y(i)), col(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		IF cool(i) = 0 THEN
			PSET (x(i), y(i)), bcolor
			DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
			ty(i) = 0
			bda(i) = INT(RND * 360)
			he(i) = 250
			eng(i) = 0
		END IF
	ELSEIF ty(i) = 5 THEN
		LINE (x(i) - 2, y(i) - 2)-(x(i) + 2, y(i) + 2), bcolor, BF
		FOR j = 1 TO 3
		CIRCLE (x(i), y(i)), j, bcolor
		NEXT j
		LINE (x(i) - 5, y(i) - 5)-(x(i) + 5, y(i) + 5), bcolor, B
		ta = ta(i)
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ta) + "D8"
		IF ta(i) <> tda(i) THEN
			FOR j = 1 TO 10
			IF ABS(ta(i) - tda(i)) < 180 THEN
				IF tda(i) > ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) < ta(i) THEN ta(i) = ta(i) - 1
			ELSEIF ABS(ta(i) - tda(i)) >= 180 THEN
				IF tda(i) < ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) > ta(i) THEN ta(i) = ta(i) - 1
			END IF
			NEXT j
		END IF
		IF x(i) < 10 THEN x(i) = 10
		IF x(i) > 630 THEN x(i) = 630
		IF y(i) < 10 THEN y(i) = 10
		IF y(i) > 470 THEN y(i) = 470
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 40) = 1 THEN tda(i) = INT(RND * 360) + 1
		LINE (x(i) - 2, y(i) - 2)-(x(i) + 2, y(i) + 2), col(i), BF
		FOR j = 1 TO 3
		CIRCLE (x(i), y(i)), j, col(i)
		NEXT j
		LINE (x(i) - 5, y(i) - 5)-(x(i) + 5, y(i) + 5), col(i), B
		ta = ta(i)
		PSET (x(i), y(i)), col(i)
		DRAW "TA=" + VARPTR$(ta) + "D8"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 22500 THEN
				diffx = x(i) - x(j)
				diffy = y(i) - y(j)
				IF diffy = 0 THEN diffy = .1
				IF diffx = 0 THEN diffx = .1
				a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
				IF diffy < 0 AND diffx > 0 THEN a = 180 - a
				IF diffy < 0 AND diffx < 0 THEN a = 180 + a
				IF diffy > 0 AND diffx < 0 THEN a = 360 - a
				a = a + 180
				IF a > 360 THEN a = a - 360
				tda(i) = a
				IF ta(i) = tda(i) AND cool(i) = 0 THEN
					FOR k = 1 TO 100
					IF so(k) = -2 THEN
						cool(i) = INT(RND * 3) + 14
						sl(k) = 15 + ty(i)
						so(k) = col(i)
						sa(k) = ta(i)
						sat(k) = 0
						sx(k) = x(i)
						sy(k) = y(i)
						sd(k) = INT(RND * 3) + 4
						EXIT FOR
					END IF
					NEXT k
				END IF
			END IF
		END IF
		NEXT j
	ELSEIF ty(i) = 6 THEN
		PSET (x(i), y(i)), bcolor
		ba = ba(i)
		ta = ta(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ta) + "D10"
		x(i) = x(i) + cx(ba(i)) * sp(i)
		y(i) = y(i) + cy(ba(i)) * sp(i)
		IF x(i) < 10 THEN x(i) = 10: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF x(i) > 630 THEN x(i) = 630: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF y(i) < 10 THEN y(i) = 10: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF y(i) > 470 THEN y(i) = 470: ba(i) = INT(RND * 8) * 45: bda(i) = ba(i)
		IF sp(i) = 1 AND ty(i) = 1 THEN bad(i) = INT(bda(i) / 45) * 45
		IF ba(i) <> bda(i) AND sp(i) > 0 THEN
			IF ABS(ba(i) - bda(i)) < 20 OR ABS(ba(i) - bda(i)) > 340 THEN ba(i) = bda(i)
			IF ABS(ba(i) - bda(i)) < 180 THEN
				IF bda(i) > ba(i) THEN ba(i) = ba(i) + 11.25
				IF bda(i) < ba(i) THEN ba(i) = ba(i) - 11.25
			ELSEIF ABS(ba(i) - bda(i)) >= 180 THEN
				IF bda(i) < ba(i) THEN ba(i) = ba(i) + 11.25
				IF bda(i) > ba(i) THEN ba(i) = ba(i) - 11.25
			END IF
		END IF
		IF ta(i) <> tda(i) THEN
			FOR j = 1 TO 10
			IF ABS(ta(i) - tda(i)) < 180 THEN
				IF tda(i) > ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) < ta(i) THEN ta(i) = ta(i) - 1
			ELSEIF ABS(ta(i) - tda(i)) >= 180 THEN
				IF tda(i) < ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) > ta(i) THEN ta(i) = ta(i) - 1
			END IF
			NEXT j
		END IF
		IF tarx(i) <> 0 AND (col(i) = 1 OR eng(i) = 0) THEN
			sp(i) = 1
			diffx = x(i) - tarx(i)
			diffy = y(i) - tary(i)
			IF diffy = 0 THEN diffy = .1
			IF diffx = 0 THEN diffx = .1
			a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
			IF diffy < 0 AND diffx > 0 THEN a = 180 - a
			IF diffy < 0 AND diffx < 0 THEN a = 180 + a
			IF diffy > 0 AND diffx < 0 THEN a = 360 - a
			a = a + 180
			IF a > 360 THEN a = a - 360
			bda(i) = CLNG(a / 45) * 45
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
		END IF
		IF col(i) = 1 AND tarx(i) = 0 AND tary(i) = 0 THEN sp(i) = 0
		tda(i) = bda(i)
		IF INT(RND * 50) = 1 AND col(i) <> 1 THEN sp(i) = INT(RND * 2)
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 100) = 1 AND col(i) <> 1 THEN bda(i) = INT(RND * 8) * 45
		PSET (x(i), y(i)), col(i)
		ba = ba(i)
		ta = ta(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), col(i)
		DRAW "TA=" + VARPTR$(ta) + "D10"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		eng(i) = 0
		stopp = 0
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 67600 THEN
				eng(i) = 1
				IF col(i) <> 1 THEN tarx(i) = 0: tary(i) = 0
				diffx = x(i) - x(j)
				diffy = y(i) - y(j)
				IF diffy = 0 THEN diffy = .1
				IF diffx = 0 THEN diffx = .1
				a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
				IF diffy < 0 AND diffx > 0 THEN a = 180 - a
				IF diffy < 0 AND diffx < 0 THEN a = 180 + a
				IF diffy > 0 AND diffx < 0 THEN a = 360 - a
				a = a + 180
				IF a > 360 THEN a = a - 360
				IF stopp = 0 AND (col(i) <> 1 OR am(i) = 1) THEN sp(i) = 1
				IF col(i) <> 1 OR am(i) = 1 THEN IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 36100 THEN bda(i) = (CLNG(a / 45) * 45) + 180 ELSE bda(i) = (CLNG(a / 45) * 45)
				IF bda(i) > 360 THEN bda(i) = bda(i) - 360
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 > 36100 AND (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 44100 THEN
					IF col(i) <> 1 OR am(i) = 1 THEN sp(i) = 0
					IF ty(j) = 0 OR ty(j) = 5 THEN stopp = 1
					tda(i) = a
					IF ta(i) = tda(i) AND cool(i) = 0 THEN
						FOR k = 1 TO 100
						IF so(k) = -2 THEN
							cool(i) = INT(RND * 3) + 24
							sl(k) = INT(RND * 5) + 18
							so(k) = -1
							sa(k) = ta(i) + INT(RND * 5) - 2
							IF sa(k) > 360 THEN sa(k) = sa(k) - 360
							IF sa(k) < 0 THEN sa(k) = sa(k) + 360
							sx(k) = x(i)
							sy(k) = y(i)
							sat(k) = 1
							sd(k) = INT(RND * 3) + 14
							EXIT FOR
						END IF
						NEXT k
						EXIT FOR
					END IF
				END IF
			END IF
		END IF
		IF he(j) > 0 AND col(i) = col(j) AND eng(j) = 0 AND tarx(j) = 0 AND eng(i) = 1 AND col(i) <> 1 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 250000 THEN
				sp(j) = 1
				tarx(j) = x(i)
				tary(j) = y(i)
			END IF
		END IF
		NEXT j
	END IF
	FOR j = 1 TO 10
	IF selected(j) = i AND col(i) = 1 THEN LINE (x(i) - 10, y(i) - 10)-(x(i) + 10, y(i) + 10), 15, B
	NEXT j
       
END IF
IF he(i) = 1 THEN
	LINE (x(i) - 10, y(i) - 10)-(x(i) + 10, y(i) + 10), bcolor, BF
	a = 10
	FOR j = 1 TO 50
	IF emax(j) = 0 THEN
		he(i) = 0
		emax(j) = INT(RND * 5) + 3
		es(j) = 0
		ex(j) = x(i) + INT(RND * 11) - 5
		ey(j) = y(i) + INT(RND * 11) - 5
		a = a - 1
		IF a <= 0 THEN EXIT FOR
	END IF
NEXT j
END IF
NEXT i
max = newmax
IF max > 150 THEN max = 150
a$ = INKEY$
IF a$ = CHR$(27) THEN END
IF a$ = "a" THEN attackmode = 1
LOOP

