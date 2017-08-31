'60117.21
'http://user.tninet.se/~prv247p/hatt/hatten.swf
start:
press = 1
CLEAR
CLS
SCREEN 12
INPUT "Random or Seed ", a$
IF a$ = "s" THEN INPUT "", aaaa ELSE aaaa = TIMER
RANDOMIZE aaaa

player = 0

DIM x(100) AS INTEGER
DIM y(100) AS INTEGER
DIM ba(100) AS INTEGER
DIM bda(100) AS INTEGER
DIM sp(100) AS INTEGER
DIM ta(100) AS INTEGER
DIM two(100) AS INTEGER
DIM tda(100) AS INTEGER
DIM ty(100) AS INTEGER
DIM eng(100) AS INTEGER
DIM tarx(100) AS INTEGER
DIM tary(100) AS INTEGER
DIM cool(100) AS INTEGER
DIM maxcool(100) AS INTEGER
DIM cool2(100) AS INTEGER
DIM col(100) AS INTEGER
DIM he(100) AS INTEGER

DIM sx(100) AS SINGLE
DIM sy(100) AS SINGLE
DIM sa(100) AS INTEGER
DIM sd(100) AS INTEGER
DIM so(100) AS INTEGER
DIM sl(100) AS INTEGER
DIM sat(100) AS INTEGER

DIM mx(100) AS SINGLE
DIM my(100) AS SINGLE
DIM ms(100) AS SINGLE
DIM ma(100) AS INTEGER
DIM mt(100) AS INTEGER
DIM mo(100) AS INTEGER
DIM mtar(100) AS INTEGER

DIM smx(200) AS INTEGER
DIM smy(200) AS INTEGER
DIM sma(200) AS INTEGER
DIM smt(200) AS INTEGER
DIM sms(200) AS INTEGER

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
mo(i) = -2
NEXT i
max = 15
bcolor = 6
'FOR i = 1 TO (INT(RND * 14) + 2)
FOR i = 0 TO 7
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

IF sleeper > -1 THEN sleeper = sleeper - 1
IF sleeper = 0 THEN SLEEP
FOR i = 1 TO 200
IF smt(i) > 0 THEN
	smt(i) = smt(i) - 1
	IF smt(i) > 0 THEN LINE (smx(i), smy(i))-(smx(i) + cx(sma(i)) * sms(i), smy(i) + cy(sma(i)) * sms(i)), 7 ELSE LINE (smx(i), smy(i))-(smx(i) + cx(sma(i)) * sms(i), smy(i) + cy(sma(i)) * sms(i)), bcolor
END IF
NEXT i
FOR i = 1 TO 100
IF so(i) > -2 THEN
	PSET (sx(i), sy(i)), bcolor
	IF sd(i) <> 1 THEN sx(i) = sx(i) + (cx(sa(i)) * 10) ELSE sx(i) = sx(i) + (cx(sa(i)) * 15)
	IF sd(i) <> 1 THEN sy(i) = sy(i) + (cy(sa(i)) * 10) ELSE sy(i) = sy(i) + (cy(sa(i)) * 15)
	PSET (sx(i), sy(i)), 14
	IF bcolor = 14 THEN PSET (sx(i), sy(i)), 4
	sl(i) = sl(i) - 1
	IF sl(i) <= 0 THEN so(i) = -2
	IF sat(i) = 1 AND so(i) > -2 THEN GOTO skip2
	FOR j = 1 TO max
	IF ABS(x(j) - sx(i)) <= 10 + (5 * sat(i)) AND ABS(y(j) - sy(i)) <= 10 + (5 * sat(i)) AND he(j) > 1 AND col(j) <> so(i) THEN
		he(j) = he(j) - sd(i)
		IF ty(j) = 6 AND (j <> 10 OR player = 0) THEN tda(j) = INT(RND * 360)
		IF ty(j) = 0 THEN bda(j) = sa(i) + 180
		IF he(j) < 1 THEN he(j) = 1
		IF ty(j) = 5 THEN bda(j) = CLNG(sa(i) / 45) * 45
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
FOR i = 1 TO 100
IF mo(i) >= 0 THEN
	IF ma(i) > 360 THEN ma(i) = ma(i) - 360
	IF ma(i) < 0 THEN ma(i) = ma(i) + 360
	PSET (mx(i), my(i)), bcolor
	FOR j = 1 TO 200
	IF smt(j) = 0 THEN
		smt(j) = 15
		smx(j) = mx(i): smy(j) = my(i)
		sma(j) = ma(i)
		sms(j) = ms(i)
		EXIT FOR
	END IF
	NEXT j
	mx(i) = mx(i) + cx(ma(i)) * ms(i)
	my(i) = my(i) + cy(ma(i)) * ms(i)
	IF ms(i) < 25 THEN ms(i) = ms(i) + 1
	FOR j = 1 TO max
	IF ABS(x(j) - mx(i)) <= 10 AND ABS(y(j) - my(i)) <= 10 AND he(j) > 1 AND col(j) <> mo(i) THEN
		dmg = 6 + INT(RND * 5)
		IF ty(j) = 3 THEN dmg = dmg - 3
		IF ty(j) = 2 THEN dmg = dmg - 2
		IF ty(j) = 0 OR ty(i) = 6 THEN dmg = dmg - 5
		he(j) = he(j) - dmg
		IF ty(j) = 6 AND (j <> 10 OR player = 0) THEN tda(j) = INT(RND * 360)
		IF ty(j) = 0 THEN bda(j) = ma(i) + 180
		IF he(j) < 1 THEN he(j) = 1
		IF ty(j) = 5 THEN bda(j) = CLNG(ma(i) / 45) * 45
		mo(i) = -1
		mt(i) = 0
	END IF
	NEXT j
	mt(i) = mt(i) - 1
	IF mt(i) <= 0 THEN
		mo(i) = -1
		FOR l = 1 TO INT(RND * 3) + 1
		FOR k = 1 TO 50
		IF emax(k) = 0 THEN
			emax(k) = 7 + INT(RND * 5) - 2
			es(k) = 0
			ex(k) = mx(i) + INT(RND * 5) - 2
			ey(k) = my(i) + INT(RND * 5) - 2
			EXIT FOR
		END IF
		NEXT k
		NEXT l
		GOTO died
	END IF
	PSET (mx(i), my(i)), 4
	IF mtar(i) >= 0 THEN
		diffx = mx(i) - x(mtar(i))
		diffy = my(i) - y(mtar(i))
		IF diffy = 0 THEN diffy = .1
		IF diffx = 0 THEN diffx = .1
		a = CINT(ATN(ABS(diffx / diffy)) * 57.2958)
		IF diffy < 0 AND diffx > 0 THEN a = 180 - a
		IF diffy < 0 AND diffx < 0 THEN a = 180 + a
		IF diffy > 0 AND diffx < 0 THEN a = 360 - a
		a = a + 180
		IF a > 360 THEN a = a - 360
	END IF
	IF mtar(i) >= 0 THEN IF he(mtar(i)) <= 1 THEN mtar(i) = -1
	IF mtar(i) = -1 THEN a = ma(i) + INT(RND * 10) - 5
	IF ms(i) < 12 THEN num = ms(i) ELSE num = 12
	FOR j = 1 TO num
	IF ABS(ma(i) - a) < 180 THEN
		IF ma(i) > a THEN ma(i) = ma(i) - 1
		IF ma(i) < a THEN ma(i) = ma(i) + 1
	ELSE
		IF ma(i) > a THEN ma(i) = ma(i) + 1
		IF ma(i) < a THEN ma(i) = ma(i) - 1
	END IF
	NEXT j
died:
END IF
NEXT i
FOR i = 1 TO 100
IF he(i) > 0 THEN
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
			FOR j = 1 TO 100
			IF he(j) = 0 THEN
				max = max + 1
				IF max > 100 THEN max = 100
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
				cool(j) = 10
				he(j) = 20 + (10 * ty(j))
				IF ty(j) = 4 THEN he(j) = 25
				IF INT(RND * 12) = 1 THEN ty(j) = 7: he(j) = 5
				IF INT(RND * 7) = 1 THEN
					ty(j) = 6
					he(j) = 100
					ann = (INT(RND * INT(RND * 360)) - 180) + bda(i)
					IF ann > 360 THEN ann = ann - 360
					IF ann < 0 THEN ann = ann + 360
					x(j) = x(i) + (cx(ann) * 30)
					y(j) = y(i) + (cy(ann) * 30)
				END IF
				IF INT(RND * 7) = 1 THEN
					ty(j) = 8
					he(j) = 30
				END IF
				IF INT(RND * 15) = 1 AND (player = 0 OR j <> 10) THEN ty(j) = 5: cool(j) = 325: he(j) = 100
				IF ty(j) = 3 AND INT(RND * 2) = 1 THEN two(j) = 1
				col(j) = col(i)
				IF j = 10 AND player = 1 THEN sleeper = 5
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
		IF (player = 0 OR i <> 10) THEN tda(i) = bda(i)
		IF tarx(i) <> 0 AND eng(i) = 0 AND (i <> 10 OR player = 0) THEN
			sp(i) = 1
			IF ty(i) = 1 THEN sp(i) = 2
			diffx = x(i) - tarx(i)
			diffy = y(i) - tary(i)
			IF ABS(diffx) <= 20 AND diffy > 20 THEN bda(i) = 180
			IF diffx > 20 AND diffy > 20 THEN bda(i) = 225
			IF diffx > 20 AND ABS(diffy) <= 20 THEN bda(i) = 270
			IF diffx > 20 AND diffy < -20 THEN bda(i) = 315
			IF ABS(diffx) <= 20 AND diffy < -20 THEN bda(i) = 0
			IF diffx < -20 AND diffy < -20 THEN bda(i) = 45
			IF diffx < -20 AND ABS(diffy) <= 20 THEN bda(i) = 90
			IF diffx < -20 AND diffy > 20 THEN bda(i) = 135
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
		END IF
		IF ty(i) = 1 AND sp(i) <= 1 THEN bda(i) = CLNG(bda(i) / 45) * 45
		IF INT(RND * 50) = 1 AND (player = 0 OR i <> 10) THEN IF ty(i) <> 1 THEN sp(i) = INT(RND * 2) ELSE sp(i) = INT(RND * 3)
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 150) = 1 AND (player = 0 OR i <> 10) THEN IF ty(i) = 1 THEN bda(i) = INT(RND * 16) * 22.5 ELSE bda(i) = INT(RND * 8) * 45
		LINE (x(i) - (ty(i) + 1), y(i) - (ty(i) + 1))-(x(i) + ty(i) + 1, y(i) + ty(i) + 1), col(i), BF
		FOR j = 1 TO 2 + ty(i)
		CIRCLE (x(i), y(i)), j, col(i)
		NEXT j
		ba = ba(i)
		ta = ta(i)
		coll = col(i)
		IF i = 10 AND player = 1 THEN coll = 15
		PSET (x(i), y(i)), coll
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), col(i)
		IF two(i) = 0 THEN DRAW "TA=" + VARPTR$(ta) + "D9" ELSE DRAW "TA=" + VARPTR$(ta) + "BL1 D9 BU9 BR2 D9"
		IF ty(i) = 2 THEN
			PSET (x(i), y(i)), col(i)
			DRAW "TA=" + VARPTR$(ta) + " BU2 BL4 U2 R8 D1 L8 D1 R8"
		END IF
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		eng(i) = 0
		IF i = 10 AND player = 1 THEN GOTO skipforplayer1
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 50000 THEN
				eng(i) = 1
				tarx(i) = 0
				tary(i) = 0
				sp(i) = 1
				IF ty(i) = 1 THEN sp(i) = 2
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
				IF ty(i) = 1 THEN bda(i) = CLNG(a / 22.5) * 22.5 ELSE bda(i) = CLNG(a / 45) * 45
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < (100 + (25 * (ty(i) - 1))) ^ 2 THEN
					sp(i) = 0
					tda(i) = a
					IF ta(i) = tda(i) AND cool(i) = 0 THEN
						diff = -90
restart:                   
						FOR k = 1 TO 100
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
		IF he(j) > 0 AND col(i) = col(j) AND eng(j) = 0 AND tarx(j) = 0 AND eng(i) = 1 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 250000 THEN
				sp(j) = 1
				tarx(j) = x(i)
				tary(j) = y(i)
			END IF
		END IF
		NEXT j
skipforplayer1:
	ELSEIF ty(i) = 4 THEN
		ba = ba(i)
		ta = ta(i)
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ta) + "L1 NU1 ND3 BR2 U1 D4"
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
			FOR j = 1 TO 30
			IF ABS(ta(i) - tda(i)) < 180 THEN
				IF tda(i) > ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) < ta(i) THEN ta(i) = ta(i) - 1
			ELSEIF ABS(ta(i) - tda(i)) >= 180 THEN
				IF tda(i) < ta(i) THEN ta(i) = ta(i) + 1
				IF tda(i) > ta(i) THEN ta(i) = ta(i) - 1
			END IF
			NEXT j
		END IF
		IF (player = 0 OR i <> 10) THEN tda(i) = bda(i)
		IF tarx(i) <> 0 AND eng(i) = 0 AND (i <> 10 OR player = 0) THEN
			sp(i) = 2
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
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
			bda(i) = INT(a / 22.5) * 22.5
		END IF
		IF sp(i) <= 1 THEN bda(i) = CLNG(bda(i) / 45) * 45
		IF INT(RND * 50) = 1 AND (player = 0 OR i <> 10) THEN sp(i) = INT(RND * 4)
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 150) = 1 AND (player = 0 OR i <> 10) THEN bda(i) = INT(RND * 16) * 22.5
		ba = ba(i)
		ta = ta(i)
		coll = col(i)
		IF i = 10 AND player = 1 THEN coll = 15
		PSET (x(i), y(i)), coll
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), col(i)
		DRAW "TA=" + VARPTR$(ta) + "L1 NU1 ND3 BR2 U1 D4"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		eng(i) = 0
		'IF i = 10 AND player = 1 THEN GOTO skipforplayer1
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 30000 THEN
				eng(i) = 1
				tarx(i) = 0
				tary(i) = 0
				sp(i) = 3
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
				bda(i) = CLNG(a / 22.5) * 22.5
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 10000 THEN
					sp(i) = 0
					tda(i) = a
					IF ta(i) = tda(i) AND cool(i) = 0 THEN
						diff = -90
						FOR l = 1 TO INT(RND * 3) + 1
						FOR k = 1 TO 100
						IF so(k) = -2 THEN
							cool(i) = 3
							sl(k) = 15
							sat(k) = 0
							so(k) = col(i)
							sa(k) = ta(i) + INT(RND * 3) - 1
							sx(k) = x(i) + INT(RND * 5) - 2
							sy(k) = y(i) + INT(RND * 5) - 2
							sd(k) = 1
							EXIT FOR
						END IF
						NEXT k
						NEXT l
					END IF
				END IF
			END IF
		END IF
		IF he(j) > 0 AND col(i) = col(j) AND eng(j) = 0 AND tarx(j) = 0 AND eng(i) = 1 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 250000 THEN
				sp(j) = 1
				tarx(j) = x(i)
				tary(j) = y(i)
			END IF
		END IF
		NEXT j
	ELSEIF ty(i) = 5 THEN
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
		sp(i) = 1
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF INT(RND * 50) = 1 THEN bda(i) = INT(RND * 8) * 45
		ba = ba(i)
		IF i <> 10 OR player = 0 THEN PSET (x(i), y(i)), col(i)
		IF i = 10 AND player = 1 THEN PSET (x(i), y(i)), 15
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
	ELSEIF ty(i) = 6 THEN
		LINE (x(i) - 2, y(i) - 2)-(x(i) + 2, y(i) + 2), bcolor, BF
		FOR j = 1 TO 3
		CIRCLE (x(i), y(i)), j, bcolor
		NEXT j
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
		IF INT(RND * 40) = 1 AND (player = 0 OR i <> 10) THEN tda(i) = INT(RND * 360) + 1
		LINE (x(i) - 2, y(i) - 2)-(x(i) + 2, y(i) + 2), col(i), BF
		coll = col(i)
		IF i = 10 AND player = 1 THEN coll = 15
		FOR j = 1 TO 3
		CIRCLE (x(i), y(i)), j, coll
		NEXT j
		ta = ta(i)
		PSET (x(i), y(i)), col(i)
		DRAW "TA=" + VARPTR$(ta) + "D8"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		IF i = 10 AND player = 1 THEN GOTO skipforplayer2
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
skipforplayer2:
	ELSEIF ty(i) = 7 THEN
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
		IF tarx(i) <> 0 AND eng(i) = 0 AND (player = 0 OR i <> 10) THEN
			sp(i) = 1
			IF ABS(diffx) <= 20 AND diffy > 20 THEN bda(i) = 180
			IF diffx > 20 AND diffy > 20 THEN bda(i) = 225
			IF diffx > 20 AND ABS(diffy) <= 20 THEN bda(i) = 270
			IF diffx > 20 AND diffy < -20 THEN bda(i) = 315
			IF ABS(diffx) <= 20 AND diffy < -20 THEN bda(i) = 0
			IF diffx < -20 AND diffy < -20 THEN bda(i) = 45
			IF diffx < -20 AND ABS(diffy) <= 20 THEN bda(i) = 90
			IF diffx < -20 AND diffy > 20 THEN bda(i) = 135
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
		END IF
		IF player = 0 OR i <> 10 THEN tda(i) = bda(i)
		IF INT(RND * 50) = 1 AND (player = 0 OR i <> 10) THEN sp(i) = INT(RND * 2)
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 100) = 1 AND (player = 0 OR i <> 10) THEN bda(i) = INT(RND * 8) * 45
		PSET (x(i), y(i)), col(i)
		ba = ba(i)
		ta = ta(i)
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), col(i)
		IF i = 10 AND player = 1 THEN PSET (x(i), y(i)), 15
		DRAW "TA=" + VARPTR$(ta) + "D10"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		eng(i) = 0
		stopp = 0
		IF i = 10 AND player = 1 THEN GOTO skipforplayer3
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 67600 THEN
				eng(i) = 1
				tarx(i) = 0
				tary(i) = 0
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
				IF stopp = 0 THEN sp(i) = 1
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 36100 THEN bda(i) = (CLNG(a / 45) * 45) + 180 ELSE bda(i) = (CLNG(a / 45) * 45)
				IF bda(i) > 360 THEN bda(i) = bda(i) - 360
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 > 36100 AND (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 44100 THEN
					sp(i) = 0
					IF ty(j) = 0 OR ty(j) = 5 THEN stopp = 1
					tda(i) = a
					IF ta(i) = tda(i) AND cool(i) = 0 THEN
						FOR k = 1 TO 100
						IF so(k) = -2 THEN
							cool(i) = INT(RND * 3) + 24
							sl(k) = INT(RND * 3) + 19
							so(k) = -1
							sa(k) = ta(i) + INT(RND * 3) - 1
							IF sa(k) > 360 THEN sa(k) = sa(k) - 360
							IF sa(k) < 0 THEN sa(k) = sa(k) + 360
							sx(k) = x(i)
							sy(k) = y(i)
							sat(k) = 1
							sd(k) = INT(RND * 5) + 14
							EXIT FOR
						END IF
						NEXT k
						EXIT FOR
					END IF
				END IF
			END IF
		END IF
		IF he(j) > 0 AND col(i) = col(j) AND eng(j) = 0 AND tarx(j) = 0 AND eng(i) = 1 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 250000 THEN
				sp(j) = 1
				tarx(j) = x(i)
				tary(j) = y(i)
			END IF
		END IF
		NEXT j
skipforplayer3:
	ELSEIF ty(i) = 8 THEN
		ba = ba(i)
		ta = ta(i)
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ba) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ta) + "BR4 U3 L8 D7 R8 U5"
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
		IF eng(i) = 0 THEN tda(i) = bda(i)
		IF tarx(i) <> 0 AND eng(i) = 0 AND (i <> 10 OR player = 0) THEN
			sp(i) = 1
			IF ty(i) = 1 THEN sp(i) = 2
			diffx = x(i) - tarx(i)
			diffy = y(i) - tary(i)
			IF ABS(diffx) <= 20 AND diffy > 20 THEN bda(i) = 180
			IF diffx > 20 AND diffy > 20 THEN bda(i) = 225
			IF diffx > 20 AND ABS(diffy) <= 20 THEN bda(i) = 270
			IF diffx > 20 AND diffy < -20 THEN bda(i) = 315
			IF ABS(diffx) <= 20 AND diffy < -20 THEN bda(i) = 0
			IF diffx < -20 AND diffy < -20 THEN bda(i) = 45
			IF diffx < -20 AND ABS(diffy) <= 20 THEN bda(i) = 90
			IF diffx < -20 AND diffy > 20 THEN bda(i) = 135
			IF ABS(tarx(i) - x(i)) < 20 AND ABS(tary(i) - y(i)) < 20 THEN tarx(i) = 0: tary(i) = 0
		END IF
		IF INT(RND * 50) = 1 THEN sp(i) = INT(RND * 2)
		IF ba(i) > 360 THEN ba(i) = ba(i) - 360
		IF ba(i) < 0 THEN ba(i) = ba(i) + 360
		IF ta(i) > 360 THEN ta(i) = ta(i) - 360
		IF ta(i) < 0 THEN ta(i) = ta(i) + 360
		IF INT(RND * 150) = 1 THEN bda(i) = INT(RND * 8) * 45
		ba = ba(i)
		ta = ta(i)
		coll = col(i)
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ba) + "C=" + VARPTR$(coll) + "BL5 BD8 U15 R10 D15 L10"
		PSET (x(i), y(i)), bcolor
		DRAW "TA=" + VARPTR$(ta) + "C=" + VARPTR$(coll) + "BR4 U3 L8 D7 R8 U5"
		IF cool(i) > 0 THEN cool(i) = cool(i) - 1
		eng(i) = 0
		FOR j = 1 TO max
		IF col(j) <> col(i) AND he(j) > 0 THEN
			IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 50000 THEN
				eng(i) = 1
				tarx(i) = 0
				tary(i) = 0
				sp(i) = 1
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
				bda(i) = CLNG(a / 45) * 45
				tda(i) = a
				IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 40000 THEN
					sp(i) = 0
					IF (x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2 < 25000 THEN sp(i) = 1: bda(i) = CLNG((a + 180) / 45) * 45
					diff = -90
					IF cool(i) = 0 THEN
						FOR l = -1 TO 1 STEP 2
						FOR k = 1 TO 100
						IF mo(k) <= -1 THEN
							cool(i) = 100
							mx(k) = x(i) + cx(ta(i) + (90 * l))
							my(k) = y(i) + cy(ta(i) + (90 * l))
							ma(k) = ta(i) + 10 * l
							ms(k) = 1
							mt(k) = 35
							mo(k) = col(i)
							mtar(k) = j
							EXIT FOR
						END IF
						NEXT k
						NEXT l
					END IF
					EXIT FOR
				END IF
			END IF
		END IF
		NEXT j
	END IF
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
IF max > 100 THEN max = 100
a$ = INKEY$
IF a$ = CHR$(27) THEN LOCATE 1, 1: PRINT aaaa: END
IF a$ = "8" THEN
	IF ty(10) = 1 AND sp(10) < 2 THEN sp(10) = sp(10) + 1 ELSE IF sp(10) < 1 THEN sp(10) = 1
ELSEIF a$ = "2" THEN
	IF sp(10) > 0 THEN sp(10) = sp(10) - 1
ELSEIF a$ = "4" THEN
	bda(10) = bda(10) + 45
	'bda(10) = CINT(bda(10) / 45) * 45
	IF bda(10) > 360 THEN bda(10) = bda(10) - 360
ELSEIF a$ = "6" THEN
	bda(10) = bda(10) - 45
	'bda(10) = CINT(bda(10) / 45) * 45
	IF bda(10) < 0 THEN bda(10) = bda(10) + 360
ELSEIF a$ = "7" THEN
	tda(10) = tda(10) + 10
	IF tda(10) > 360 THEN tda(10) = tda(10) - 360
ELSEIF a$ = "9" THEN
	tda(10) = tda(10) - 10
	IF tda(10) < 0 THEN tda(10) = tda(10) + 360
ELSEIF a$ = "1" THEN
	tda(10) = tda(10) + 4
	IF tda(10) > 360 THEN tda(10) = tda(10) - 360
ELSEIF a$ = "3" THEN
	tda(10) = tda(10) - 4
	IF tda(10) < 0 THEN tda(10) = tda(10) + 360
ELSEIF a$ = "z" THEN
	tda(10) = bda(10)
ELSEIF a$ = "d" THEN
	he(10) = 1
ELSEIF a$ = " " THEN
	IF cool(10) = 0 THEN
		diff = -90
restart2:
		FOR k = 1 TO 100
		IF so(k) = -2 THEN
			IF ty(10) = 6 THEN cool(10) = INT(RND * 3) + 24
			IF ty(10) = 5 THEN cool(10) = INT(RND * 3) + 14
			IF ty(10) <= 3 AND ty(10) >= 1 THEN cool(10) = INT(RND * 3) + 14 + ty(10)
			IF ty(10) = 6 THEN sl(k) = INT(RND * 5) + 18 ELSE sl(k) = 15 + ty(10)
			so(k) = -1
			sa(k) = ta(10)
			IF ty(10) = 6 THEN sa(k) = sa(k) + INT(RND * 5) - 2
			IF sa(k) > 360 THEN sa(k) = sa(k) - 360
			IF sa(k) < 0 THEN sa(k) = sa(k) + 360
			sx(k) = x(10)
			sy(k) = y(10)
			IF ty(10) <= 3 AND ty(10) >= 1 THEN
				IF two(10) = 1 THEN sx(k) = x(10) + cx(ta(10) + diff) ELSE sx(k) = x(10)
				IF two(10) = 1 THEN sy(k) = y(10) + cy(ta(10) + diff) ELSE sy(k) = y(10)
				ran = INT(RND * 15)
				IF two(10) = 1 AND diff = -90 THEN sx(k) = sx(k) + cx(ta(10)) * ran: sy(k) = sy(k) + cy(ta(10)) * ran
			END IF
			IF ty(10) = 6 THEN sat(k) = 1 ELSE sat(k) = 0
			IF ty(10) = 6 THEN sd(k) = INT(RND * 3) + 14
			IF ty(10) = 5 THEN sd(k) = INT(RND * 3) + 4
			IF ty(10) <= 3 AND ty(10) >= 1 THEN sd(k) = INT(RND * 3) + 3 + ty(10)
			so(k) = col(10)
			EXIT FOR
		END IF
		NEXT k
		IF two(10) = 1 AND diff = -90 THEN diff = 90: GOTO restart2
	END IF
ELSEIF a$ = "n" THEN
	press = press + 1
	FOR i = 1 TO 100
	IF he(i) > 0 THEN
		col(i) = i + 15
		IF ty(i) = 0 OR ty(i) = 5 THEN he(i) = 1: cool2(i) = 25
	END IF
	NEXT i
	IF press = 2 THEN GOTO start
END IF
LOOP





