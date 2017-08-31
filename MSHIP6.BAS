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
RANDOMIZE TIMER
SLEEP 2
nolaz = 1

DIM regs AS RegType

DIM starx(1 TO 40) AS INTEGER
DIM stary(1 TO 40) AS INTEGER
DIM starc(1 TO 40) AS INTEGER
DIM starmove(1 TO 40) AS INTEGER
DIM starmmove(1 TO 40) AS INTEGER

DIM cman(1 TO 5) AS INTEGER
DIM cmx(1 TO 5) AS INTEGER
DIM cmy(1 TO 5) AS INTEGER
DIM cmdx(1 TO 5) AS INTEGER
DIM cmdy(1 TO 5) AS INTEGER
DIM cmxs(1 TO 5) AS INTEGER
DIM cmys(1 TO 5) AS INTEGER
DIM cmdxs(1 TO 5) AS INTEGER
DIM cmdys(1 TO 5) AS INTEGER
DIM cmdan(1 TO 5) AS INTEGER
DIM cme(1 TO 5) AS INTEGER

DIM man(1 TO 50) AS INTEGER
DIM mx(1 TO 50) AS INTEGER
DIM my(1 TO 50) AS INTEGER
DIM mxs(1 TO 50) AS INTEGER
DIM mys(1 TO 50) AS INTEGER
DIM mdxs(1 TO 50) AS INTEGER
DIM mdys(1 TO 50) AS INTEGER
DIM mdan(1 TO 50) AS INTEGER
DIM mtar(1 TO 50) AS INTEGER
DIM mt(1 TO 50) AS INTEGER
DIM me(1 TO 50) AS INTEGER

DIM lx(1 TO 50) AS INTEGER
DIM ly(1 TO 50) AS INTEGER
DIM lxs(1 TO 50) AS INTEGER
DIM lys(1 TO 50) AS INTEGER
DIM le(1 TO 50) AS INTEGER

DIM slx(1 TO 50) AS INTEGER
DIM sly(1 TO 50) AS INTEGER
DIM slx2(1 TO 50) AS INTEGER
DIM sly2(1 TO 50) AS INTEGER
DIM slow(1 TO 50) AS INTEGER
DIM slta(1 TO 50) AS INTEGER
DIM slt(1 TO 50) AS INTEGER

DIM sx(1 TO 200) AS INTEGER
DIM sy(1 TO 200) AS INTEGER
DIM ss(1 TO 200) AS INTEGER

DIM sx2(1 TO 250) AS INTEGER
DIM sy2(1 TO 250) AS INTEGER
DIM ss2(1 TO 250) AS INTEGER
DIM sxs2(1 TO 250) AS INTEGER
DIM sys2(1 TO 250) AS INTEGER

DIM ex(1 TO 50) AS INTEGER
DIM ey(1 TO 50) AS INTEGER
DIM es(1 TO 50) AS INTEGER
DIM ems(1 TO 50) AS INTEGER

DIM shex(1 TO 10) AS INTEGER
DIM shey(1 TO 10) AS INTEGER
DIM shes(1 TO 10) AS INTEGER
DIM shems(1 TO 10) AS INTEGER

DIM trx(1 TO 50) AS INTEGER
DIM try(1 TO 50) AS INTEGER
DIM trs(1 TO 50) AS INTEGER
DIM tran(1 TO 50) AS INTEGER
DIM tra(1 TO 50) AS INTEGER

DIM an(1 TO 100) AS INTEGER
DIM x(1 TO 100) AS INTEGER
DIM y(1 TO 100) AS INTEGER
DIM xs(1 TO 100) AS INTEGER
DIM ys(1 TO 100) AS INTEGER
DIM dan(1 TO 100) AS INTEGER
DIM dxs(1 TO 100) AS INTEGER
DIM dys(1 TO 100) AS INTEGER
DIM c(1 TO 100) AS INTEGER
DIM cool(1 TO 100) AS INTEGER
DIM cool2(1 TO 100) AS INTEGER
DIM cool3(1 TO 100) AS INTEGER
DIM a(1 TO 100) AS INTEGER
DIM m(1 TO 100) AS INTEGER
DIM cx(360) AS DOUBLE
DIM cy(360) AS DOUBLE
Pi = 3.141592
FOR i = 0 TO 360
   cx(i) = SIN(i * (Pi / 180))
   cy(i) = COS(i * (Pi / 180))
NEXT

motherships& = 5
mothershiphealth = 100
DO
starxs& = INT(RND * 3) - 1
starys& = INT(RND * 3) - 1
LOOP UNTIL starxs& <> 0 OR starys& <> 0
FOR i = 1 TO 40
starx(i) = INT(RND * 640)
stary(i) = INT(RND * 480)
starc(i) = INT(RND * 2)
IF starc(i) = 1 THEN starc(i) = 7
IF starc(i) = 0 THEN starc(i) = 15
starmmove(i) = INT(RND * 3) + 1
NEXT i
new:
onn& = -1
player& = 1
CLS
die& = 0
FOR i = 1 TO motherships&
x(i) = INT(RND * 640) + 1
y(i) = INT(RND * 480) + 1
xs(i) = INT(RND * 5) - 2
ys(i) = INT(RND * 5) - 2
dxs(i) = xs(i)
dys(i) = ys(i)
a(i) = mothershiphealth
m(i) = 1
c(i) = i
cool(i) = INT(RND * 500) + 1
cool2(i) = INT(RND * 1000)
cool3(i) = INT(RND * 40)
NEXT i
DO
regs.ax = 3
CALL INTERRUPT(&H33, regs, regs)
x = regs.cx
y = regs.dx
b1 = regs.bx AND 1
b2 = regs.bx AND 2

IF show > 0 THEN
        show = show - 1
        LINE (ox - 5, oy)-(ox + 5, oy), 0
        LINE (ox, oy - 5)-(ox, oy + 5), 0
END IF
IF olb1 <> 0 THEN LINE (ox, oy)-(stx, sty), 6, B
IF x <> ox OR y <> oy THEN show = 50
ox = x
oy = y
IF ox < 5 THEN ox = 5: screenx = screenx - 50: CLS
IF ox > 634 THEN ox = 634: screenx = screenx + 50: CLS
IF oy < 5 THEN oy = 5: screeny = screeny - 50: CLS
IF oy > 474 THEN oy = 474: screeny = screeny + 50: CLS
IF screenx < 0 THEN screenx = 0
IF screenx > 639 THEN screenx = 639
IF screeny < 0 THEN screeny = 0
IF screeny > 489 THEN screeny = 489

IF show > 0 THEN
        LINE (ox - 5, oy)-(ox + 5, oy), 15
        LINE (ox, oy - 5)-(ox, oy + 5), 15
END IF
IF b1 <> 0 AND show > 0 THEN
        FOR j = 1 TO 5
        IF cme(j) <= 0 THEN
                cmx(j) = ox + INT(RND * 101) - 50
                cmy(j) = oy + INT(RND * 101) - 50
                cmdx(j) = ox
                cmdy(j) = oy
                cmxs(j) = 0
                cmys(j) = 0
                cmt(j) = -1
                cme(j) = 1
                EXIT FOR
        END IF
        NEXT j
END IF

IF back& <> 0 THEN
        back& = back& - 7
        IF back& < 0 THEN back& = 0
        PALETTE 0, back& + back& * 256 + back& * 65536
END IF

FOR t = 1 TO 50
IF tra(t) > 0 THEN
        trx(t) = trx(t) + cx(tran(t)) * trs(t)
        try(t) = try(t) + cy(tran(t)) * trs(t)
        tra(t) = tra(t) + 1
        IF tra(t) = 20 THEN tra(t) = 0
        FOR i = 1 TO 200
        IF ss(i) <= 0 THEN
                sx(i) = trx(t)
                sy(i) = try(t)
                ss(i) = 1
                EXIT FOR
        END IF
        NEXT i
END IF
NEXT t
FOR j = 1 TO 40
starmove(j) = starmove(j) + 1
IF starmove(j) >= starmmove(j) THEN
        starmove(j) = 0
        PSET (starx(j), stary(j)), 0
        starx(j) = starx(j) + starxs&
        stary(j) = stary(j) + starys&
        IF starx(j) > 640 OR starx(j) < 0 THEN stary(j) = INT(RND * 480)
        IF stary(j) > 480 OR stary(j) < 0 THEN starx(j) = INT(RND * 640)
        IF starx(j) > 640 THEN starx(j) = 0
        IF starx(j) < 0 THEN starx(j) = 640
        IF stary(j) > 480 THEN stary(j) = 0
        IF stary(j) < 0 THEN stary(j) = 480
END IF
PSET (starx(j), stary(j)), starc(j)
NEXT j
FOR s = 1 TO 200
IF ss(s) > 0 THEN
        FOR i = 1 TO 6
        DO
        xx = INT(RND * ss(s) * 2 + 1) - ss(s)
        yy = INT(RND * ss(s) * 2 + 1) - ss(s)
        LOOP UNTIL xx ^ 2 + yy ^ 2 <= (ss(s)) ^ 2
        PSET (sx(s) + xx - screenx, sy(s) + yy - screenx), 8
        NEXT i
        ss(s) = ss(s) + 1
        IF ss(s) >= 6 THEN
                LINE (sx(s) - 6 - screenx, sy(s) - 6 - screenx)-(sx(s) + 6 - screenx, sy(s) + 6 - screenx), 0, BF
                ss(s) = 0
        END IF
END IF
NEXT s
FOR e = 1 TO 50
IF es(e) > 0 THEN
        colo& = 0
        FOR i = es(e) TO 1 STEP -1
        colo& = colo& + 1
        co& = 4
        IF colo& > 1 THEN co& = 12
        IF colo& > 2 THEN co& = 14
        CIRCLE (ex(e) - screenx, ey(e) - screeny), i, co&
        CIRCLE (ex(e) + 1 - screenx, ey(e) - screeny), i, co&
        NEXT i
        es(e) = es(e) + 1
        IF es(e) >= ems(e) THEN
                LINE (ex(e) - ems(e) - screenx, ey(e) - ems(e) - screeny)-(ex(e) + ems(e) - screenx, ey(e) + ems(e) - screeny), 0, BF
                es(e) = 0
        END IF
END IF
NEXT e
FOR e = 1 TO 10
IF shes(e) > 0 THEN
        FOR i = shes(e) + 4 TO shes(e) + 6
        CIRCLE (shex(e) - screenx, shey(e) - screeny), i, 0
        NEXT i
        co& = 7
        shes(e) = shes(e) + 32
        IF shes(e) >= shems(e) THEN co& = 0
        FOR i = shes(e) + 4 TO shes(e) + 6
        CIRCLE (shex(e) - screenx, shey(e) - screeny), i, co&
        NEXT i
        IF shes(e) >= shems(e) THEN shes(e) = 0
END IF
NEXT e
FOR s = 1 TO 200
IF ss2(s) > 0 THEN
        ss2(s) = ss2(s) + 1
        LINE (sx2(s) - screenx, sy2(s) - screeny)-(sx2(s) + sxs2(s) - screenx, sy2(s) + sys2(s) - screeny), 8
        IF ss2(s) >= 10 THEN
                LINE (sx2(s) - screenx, sy2(s) - screeny)-(sx2(s) + sxs2(s) - screenx, sy2(s) + sys2(s) - screeny), 0
                ss2(s) = 0
        END IF
END IF
NEXT s
FOR l = 1 TO 50
IF le(l) > 0 THEN
        LINE (lx(l) - (lxs(l) / 2) - screenx, ly(l) - (lys(l) / 2) - screeny)-(lx(l) - screenx, ly(l) - screeny), 0
        lx(l) = lx(l) + lxs(l)
        ly(l) = ly(l) + lys(l)
        LINE (lx(l) - (lxs(l) / 2) - screenx, ly(l) - (lys(l) / 2) - screeny)-(lx(l) - screenx, ly(l) - screeny), 9
        FOR i = 1 TO 100
        IF ABS(lx(l) - x(i)) < 11 AND ABS(ly(l) - y(i)) < 11 AND a(i) > 19 THEN
                a(i) = a(i) - 1
                IF a(i) < 20 AND m(i) <> 1 AND INT(RND * 2) = 1 THEN a(i) = 1
                IF a(i) <= 0 THEN a(i) = 1
                le(l) = 0
        END IF
        NEXT i
        IF lx(l) < -10 OR lx(l) > 649 THEN le(l) = 0
        IF ly(l) < -10 OR ly(l) > 489 THEN le(l) = 0
        IF le(l) <= 0 THEN LINE (lx(l) - (lxs(l) / 2) - screenx, ly(l) - (lys(l) / 2) - screeny)-(lx(l) - screenx, ly(l) - screeny), 0
END IF
NEXT l
sounded = 0
IF nolaz = 1 THEN GOTO skiplaz
FOR l = 1 TO 50
IF slt(l) > 0 THEN
        slt(l) = slt(l) - 1
        IF slt(l) = 19 THEN GOTO show
        IF sounded = 0 THEN SOUND 3000, .05: sounded = 1
        LINE (slx(l) - screenx, sly(l) - screeny)-(slx2(l) - screenx, sly2(l) - screeny), 0
        LINE (slx(l) + 1 - screenx, sly(l) - screeny)-(slx2(l) + 1 - screenx, sly2(l) - screeny), 0
        LINE (slx(l) - screenx, sly(l) + 1 - screeny)-(slx2(l) - screenx, sly2(l) + 1 - screeny), 0
        LINE (slx(l) - 1 - screenx, sly(l) - screeny)-(slx2(l) - 1 - screenx, sly2(l) - screeny), 0
        LINE (slx(l) - screenx, sly(l) - 1 - screeny)-(slx2(l) - screenx, sly2(l) - 1 - screeny), 0
        IF slta(l) > 0 THEN
                FOR m = 1 TO 20
                IF x(slta(l)) > slx2(l) THEN slx2(l) = slx2(l) + 1
                IF x(slta(l)) < slx2(l) THEN slx2(l) = slx2(l) - 1
                IF y(slta(l)) > sly2(l) THEN sly2(l) = sly2(l) + 1
                IF y(slta(l)) < sly2(l) THEN sly2(l) = sly2(l) - 1
                IF x(slta(l)) = slx2(l) AND y(slta(l)) = sly2(l) THEN EXIT FOR
                NEXT m
                IF (slx(l) - x(slta(l))) ^ 2 + (sly(l) - y(slta(l))) ^ 2 > 10000 THEN slta(l) = 0
        END IF
        IF m(slow(l)) = 1 THEN slx(l) = x(slow(l))
        IF m(slow(l)) = 1 THEN sly(l) = y(slow(l))
res:
        FOR i = 2 TO 5
        xx = -(((slx(l) - slx2(l)) / 5) * i)
        yy = -(((sly(l) - sly2(l)) / 5) * i)
        FOR j = 1 TO 100
        IF ABS(x(j) - (slx(l) + xx)) < ABS((slx(l) - slx2(l)) / 5) AND ABS(y(j) - (sly(l) + yy)) < ABS((sly(l) - sly2(l)) / 5) AND a(j) > 19 THEN
                IF m(j) = 1 AND INT(RND * 5) = 1 THEN a(j) = a(j) - 2
                IF m(j) <> 1 THEN a(j) = 1
                IF m(j) <> 1 AND j = slta(l) THEN
                        slta(l) = 0
                        GOTO res
                END IF
        END IF
        IF slta(l) = 0 AND a(j) > 19 AND c(j) <> c(slow(l)) AND (slx(l) - x(j)) ^ 2 + (sly(l) - y(j)) ^ 2 < 10000 THEN slta(l) = j
        NEXT j
        NEXT i
        IF slta(l) = 0 THEN slt(l) = slt(l) - 5
        IF a(slow(l)) <= 19 THEN slt(l) = 0
show:
        IF slt(l) <= 0 THEN GOTO skipdraw
        LINE (slx(l) - screenx, sly(l) - screeny)-(slx2(l) - screenx, sly2(l) - screeny), 12
        LINE (slx(l) + 1 - screenx, sly(l) - screeny)-(slx2(l) + 1 - screenx, sly2(l) - screeny), 4
        LINE (slx(l) - screenx, sly(l) + 1 - screeny)-(slx2(l) - screenx, sly2(l) + 1 - screeny), 4
        LINE (slx(l) - 1 - screenx, sly(l) - screeny)-(slx2(l) - 1 - screenx, sly2(l) - screeny), 4
        LINE (slx(l) - screenx, sly(l) - 1 - screeny)-(slx2(l) - screenx, sly2(l) - 1 - screeny), 4

skipdraw:
END IF
NEXT l
skiplaz:
FOR m = 1 TO 50
IF me(m) > 0 THEN
        FOR i = 1 TO 200
        IF ss2(i) <= 0 THEN
                ss2(i) = 1
                sx2(i) = mx(m)
                sy2(i) = my(m)
                sxs2(i) = mxs(m)
                sys2(i) = mys(m)
                EXIT FOR
        END IF
        NEXT i
        PSET (mx(m) - screenx, my(m) - screenx), 0
        mx(m) = mx(m) + mxs(m)
        my(m) = my(m) + mys(m)
        me(m) = me(m) + 1
        IF me(m) > 80 AND INT(RND * 10) = 1 THEN
                FOR j = 1 TO 50
                        IF es(j) <= 0 THEN
                                me(m) = 0
                                es(j) = 1
                                ems(j) = 5
                                ex(j) = mx(m)
                                ey(j) = my(m)
                                EXIT FOR
                        END IF
                NEXT j
        END IF
        FOR i = 1 TO 2
        IF mdxs(m) > mxs(m) THEN mxs(m) = mxs(m) + 1
        IF mdys(m) > mys(m) THEN mys(m) = mys(m) + 1
        IF mdxs(m) < mxs(m) THEN mxs(m) = mxs(m) - 1
        IF mdys(m) < mys(m) THEN mys(m) = mys(m) - 1
        NEXT i
        IF me(m) > 0 THEN PSET (mx(m) - screenx, my(m) - screeny), 4
        FOR j = 1 TO 100
        IF ABS(mx(m) - x(j)) < 8 AND ABS(my(m) - y(j)) < 8 AND a(j) > 1 AND me(m) > 0 AND c(j) <> mt(m) THEN
                PSET (mx(m) - screenx, my(m) - screeny), 0
                a(j) = a(j) - 3
                IF a(j) <= 0 THEN a(j) = 1
                IF a(j) < 20 AND m(j) <> 1 AND INT(RND * 4) = 1 THEN a(j) = 1
                IF m(j) = 0 AND a(j) > 1 THEN xs(j) = mxs(m)
                IF m(j) = 0 AND a(j) > 1 THEN ys(j) = mys(m)
                IF m(j) = 2 AND a(j) > 1 THEN xs(j) = mxs(m) / 2
                IF m(j) = 2 AND a(j) > 1 THEN ys(j) = mys(m) / 2
                me(m) = 0
                FOR k = 1 TO 50
                        IF es(k) <= 0 THEN
                                es(k) = 1
                                ems(k) = 5
                                ex(k) = mx(m)
                                ey(k) = my(m)
                                EXIT FOR
                        END IF
                NEXT k
        END IF
        NEXT j
        IF mtar(m) = 0 THEN mtar(m) = INT(RND * 100) + 1
        FOR i = 1 TO 100
        IF a(mtar(m)) > 0 AND c(mtar(m)) <> mt(m) THEN EXIT FOR
        mtar(m) = INT(RND * 100) + 1
        NEXT i
        IF a(mtar(m)) > 0 AND c(mtar(m)) <> mt(m) THEN
                IF ABS(x(mtar(m)) - mx(m)) > ABS(y(mtar(m)) - my(m)) THEN
                        IF x(mtar(m)) - mx(m) < 0 THEN mdxs(m) = -15
                        IF x(mtar(m)) - mx(m) > 0 THEN mdxs(m) = 15
                        a = ABS(x(mtar(m)) - mx(m)) / 15
                        IF y(mtar(m)) - my(m) < 0 THEN mdys(m) = -(ABS(y(mtar(m)) - my(m)) / a)
                        IF y(mtar(m)) - my(m) > 0 THEN mdys(m) = (ABS(y(mtar(m)) - my(m)) / a)
                ELSEIF ABS(y(mtar(m)) - my(m)) > ABS(x(mtar(m)) - mx(m)) THEN
                        IF y(mtar(m)) - my(m) < 0 THEN mdys(m) = -15
                        IF y(mtar(m)) - my(m) > 0 THEN mdys(m) = 15
                        a = ABS(y(mtar(m)) - my(m)) / 15
                        IF x(mtar(m)) - mx(m) < 0 THEN mdxs(m) = -(ABS(x(mtar(m)) - mx(m)) / a)
                        IF x(mtar(m)) - mx(m) > 0 THEN mdxs(m) = (ABS(x(mtar(m)) - mx(m)) / a)
                END IF
        END IF
END IF
NEXT m
FOR m = 1 TO 5
IF cme(m) > 0 THEN
        FOR i = 1 TO 200
        IF ss2(i) <= 0 THEN
                ss2(i) = 1
                sx2(i) = cmx(m)
                sy2(i) = cmy(m)
                sxs2(i) = cmxs(m)
                sys2(i) = cmys(m)
                smoke = i
                EXIT FOR
        END IF
        NEXT i
        PSET (cmx(m) - screenx, cmy(m) - screenx), 0
        cmx(m) = cmx(m) + cmxs(m)
        cmy(m) = cmy(m) + cmys(m)
        FOR i = 1 TO 3
        IF cmdxs(m) > cmxs(m) THEN cmxs(m) = cmxs(m) + 1
        IF cmdys(m) > cmys(m) THEN cmys(m) = cmys(m) + 1
        IF cmdxs(m) < cmxs(m) THEN cmxs(m) = cmxs(m) - 1
        IF cmdys(m) < cmys(m) THEN cmys(m) = cmys(m) - 1
        NEXT i
        IF cme(m) > 0 THEN PSET (cmx(m) - screenx, cmy(m) - screenx), 4
        IF ABS(cmx(m) - cmdx(m)) < 16 AND ABS(cmy(m) - cmdy(m)) < 16 AND cme(m) > 0 THEN
                PSET (cmx(m) - screenx, cmy(m) - screenx), 0
                cme(m) = 0
                ss2(smoke) = 0
                back& = 40
                FOR i = 1 TO 50
                        IF es(i) <= 0 THEN
                                es(i) = 7
                                ems(i) = 20
                                ex(i) = cmx(m)
                                ey(i) = cmy(m)
                                EXIT FOR
                        END IF
                NEXT i
                FOR i = 1 TO 10
                        IF shes(i) = 0 THEN
                                shes(i) = 1
                                shems(i) = 750
                                shex(i) = cmx(m)
                                shey(i) = cmy(m)
                                EXIT FOR
                        END IF
                NEXT i
                FOR j = 1 TO 100
                        IF a(j) > 0 AND ((cmx(m) - x(j)) ^ 2 + (cmy(m) - y(j)) ^ 2) < 22500 THEN
                                IF m(j) <> 1 THEN
                                        a(j) = 19
                                        IF ((cmx(m) - x(j)) ^ 2 + (cmy(m) - y(j)) ^ 2) < 5625 THEN a(j) = 1
                                        IF ((cmx(m) - x(j)) ^ 2 + (cmy(m) - y(j)) ^ 2) < 400 THEN a(j) = 0
                                ELSEIF m(j) = 1 THEN
                                        a(j) = a(j) - 5
                                        IF ((cmx(m) - x(j)) ^ 2 + (cmy(m) - y(j)) ^ 2) < 5625 THEN a(j) = a(j) - 10
                                        IF ((cmx(m) - x(j)) ^ 2 + (cmy(m) - y(j)) ^ 2) < 400 THEN a(j) = a(j) - 45
                                        IF a(j) < 1 THEN a(j) = 1
                                END IF
                        END IF
                NEXT j
        END IF
        IF ABS(cmdx(m) - cmx(m)) > ABS(cmdy(m) - cmy(m)) THEN
                IF cmdx(m) - cmx(m) < 0 THEN cmdxs(m) = -30
                IF cmdx(m) - cmx(m) > 0 THEN cmdxs(m) = 30
                a = ABS(cmdx(m) - cmx(m)) / 30
                IF cmdy(m) - cmy(m) < 0 THEN cmdys(m) = -(ABS(cmdy(m) - cmy(m)) / a)
                IF cmdy(m) - cmy(m) > 0 THEN cmdys(m) = (ABS(cmdy(m) - cmy(m)) / a)
        ELSEIF ABS(cmdy(m) - cmy(m)) > ABS(cmdx(m) - cmx(m)) THEN
                IF cmdy(m) - cmy(m) < 0 THEN cmdys(m) = -30
                IF cmdy(m) - cmy(m) > 0 THEN cmdys(m) = 30
                a = ABS(cmdy(m) - cmy(m)) / 30
                IF cmdx(m) - cmx(m) < 0 THEN cmdxs(m) = -(ABS(cmdx(m) - cmx(m)) / a)
                IF cmdx(m) - cmx(m) > 0 THEN cmdxs(m) = (ABS(cmdx(m) - cmx(m)) / a)
        END IF
END IF
NEXT m

IF m1& = 0 AND m2& = 0 AND m3& = 0 AND m4& = 0 AND m5& = 0 THEN die& = die& + 1
IF die& >= 250 THEN GOTO new


LINE (0, 20)-(0, 20 + m1&), 1
LINE (0, 20 + m1&)-(0, 20 + m1& + m2&), 2
LINE (0, 20 + m1& + m2&)-(0, 20 + m1& + m2& + m4&), 4
LINE (0, 20 + m1& + m2& + m4&)-(0, 20 + m1& + m2& + m4& + m3&), 3
LINE (0, 20 + m1& + m2& + m4& + m3&)-(0, 20 + m1& + m2& + m4& + m3& + m5&), 5
LINE (0, 20 + m1& + m2& + m4& + m3& + m5&)-(0, 480), 0
LINE (2, 2)-(2, 16), 15
LINE (2, 2)-(5, 2), 15
LINE (6, 3)-(6, 8), 15
LINE (5, 9)-(2, 9), 15

m1& = 0
m2& = 0
m3& = 0
m4& = 0
m5& = 0
IF m(1) = 1 THEN LINE (631, 19)-(631, a(1)), 1
IF m(2) = 1 THEN LINE (633, 19)-(633, a(2)), 2
IF m(3) = 1 THEN LINE (635, 19)-(635, a(3)), 3
IF m(4) = 1 THEN LINE (637, 19)-(637, a(4)), 4
IF m(5) = 1 THEN LINE (639, 19)-(639, a(5)), 5
IF m(1) = 1 THEN LINE (631, mothershiphealth)-(631, a(1)), 0
IF m(2) = 1 THEN LINE (633, mothershiphealth)-(633, a(2)), 0
IF m(3) = 1 THEN LINE (635, mothershiphealth)-(635, a(3)), 0
IF m(4) = 1 THEN LINE (637, mothershiphealth)-(637, a(4)), 0
IF m(5) = 1 THEN LINE (639, mothershiphealth)-(639, a(5)), 0

LINE (633, 2)-(633, 14), 15
LINE (637, 2)-(637, 14), 15
LINE (633, 8)-(637, 8), 15

IF m(1) = 1 THEN LINE (1, 362)-(1, 462 - INT(cool(1) / 5)), 0 ELSE LINE (1, 362)-(1, 462), 0
IF m(1) = 1 THEN LINE (1, 462)-(1, 462 - INT(cool(1) / 5)), 1
IF m(2) = 1 THEN LINE (3, 362)-(3, 462 - INT(cool(2) / 5)), 0 ELSE LINE (3, 362)-(3, 462), 0
IF m(2) = 1 THEN LINE (3, 462)-(3, 462 - INT(cool(2) / 5)), 2
IF m(3) = 1 THEN LINE (5, 362)-(5, 462 - INT(cool(3) / 5)), 0 ELSE LINE (5, 362)-(5, 462), 0
IF m(3) = 1 THEN LINE (5, 462)-(5, 462 - INT(cool(3) / 5)), 3
IF m(4) = 1 THEN LINE (7, 362)-(7, 462 - INT(cool(4) / 5)), 0 ELSE LINE (7, 362)-(7, 462), 0
IF m(4) = 1 THEN LINE (7, 462)-(7, 462 - INT(cool(4) / 5)), 4
IF m(5) = 1 THEN LINE (9, 362)-(9, 462 - INT(cool(5) / 5)), 0 ELSE LINE (9, 362)-(9, 462), 0
IF m(5) = 1 THEN LINE (9, 462)-(9, 462 - INT(cool(5) / 5)), 5

LINE (3, 478)-(3, 466), 15
LINE (3, 478)-(7, 478), 15

IF m(1) = 1 THEN LINE (631, 362)-(631, 462 - INT(cool2(1) / 10)), 0 ELSE LINE (631, 362)-(631, 462), 0
IF m(1) = 1 THEN LINE (631, 462)-(631, 462 - INT(cool2(1) / 10)), 1
IF m(2) = 1 THEN LINE (633, 362)-(633, 462 - INT(cool2(2) / 10)), 0 ELSE LINE (633, 362)-(633, 462), 0
IF m(2) = 1 THEN LINE (633, 462)-(633, 462 - INT(cool2(2) / 10)), 2
IF m(3) = 1 THEN LINE (635, 362)-(635, 462 - INT(cool2(3) / 10)), 0 ELSE LINE (635, 362)-(635, 462), 0
IF m(3) = 1 THEN LINE (635, 462)-(635, 462 - INT(cool2(3) / 10)), 3
IF m(4) = 1 THEN LINE (637, 362)-(637, 462 - INT(cool2(4) / 10)), 0 ELSE LINE (637, 362)-(637, 462), 0
IF m(4) = 1 THEN LINE (637, 462)-(637, 462 - INT(cool2(4) / 10)), 4
IF m(5) = 1 THEN LINE (639, 362)-(639, 462 - INT(cool2(5) / 10)), 0 ELSE LINE (639, 362)-(639, 462), 0
IF m(5) = 1 THEN LINE (639, 462)-(639, 462 - INT(cool2(5) / 10)), 5

LINE (633, 478)-(633, 466), 15
LINE (633, 466)-(637, 478), 15
LINE (637, 466)-(637, 478), 15
IF INT(tim) < INT(TIMER) THEN
        tim = TIMER
        FOR i = 1 TO 100
        IF a(i) > 0 THEN
                IF m(i) = 1 THEN lifem& = lifem& + 1
                IF m(i) = 0 THEN lifei& = lifei& + 1
                IF m(i) = 2 THEN lifel& = lifel& + 1
        END IF
        NEXT i
END IF

FOR i = 1 TO 100
        IF a(i) > 0 THEN
                IF c(i) = 1 THEN m1& = m1& + 4
                IF c(i) = 2 THEN m2& = m2& + 4
                IF c(i) = 3 THEN m3& = m3& + 4
                IF c(i) = 4 THEN m4& = m4& + 4
                IF c(i) = 5 THEN m5& = m5& + 4
                IF m(i) = 1 THEN
                        IF INT(RND * 20) = 1 THEN
                                FOR j = 1 TO 100
                                        IF a(j) <= 0 THEN
                                                m(j) = 0
                                                IF INT(RND * 10) + 1 = 1 THEN m(j) = 2
                                                a(j) = 22
                                                IF m(j) = 2 THEN a(j) = 25
                                                x(j) = x(i)
                                                y(j) = y(i)
                                                c(j) = c(i)
                                                xs(j) = 0
                                                ys(j) = 0
                                                dxs(j) = INT(RND * 7) - 3
                                                dys(j) = INT(RND * 7) - 3
                                                IF m(j) = 2 THEN unitl& = unitl& + 1 ELSE uniti& = uniti& + 1
                                                EXIT FOR
                                        END IF
                                NEXT j
                        END IF
                        IF cool2(i) > 0 THEN cool2(i) = cool2(i) - 1
                        'IF cool3(i) > 0 THEN cool3(i) = cool3(i) - 1
                        IF cool3(i) = 0 THEN
                                cool3(i) = 6
                                FOR j = 100 TO 1 STEP -1
                                IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 40000 AND c(j) <> c(i) AND a(j) > 0 THEN
                                        IF ABS(x(j) - x(i)) > ABS(y(j) - y(i)) THEN
                                                IF x(j) - x(i) < 0 THEN shxs = -20
                                                IF x(j) - x(i) > 0 THEN shxs = 20
                                                a = ABS(x(j) - x(i)) / 20
                                                IF y(j) - y(i) < 0 THEN shys = -(ABS(y(j) - y(i)) / a)
                                                IF y(j) - y(i) > 0 THEN shys = (ABS(y(j) - y(i)) / a)
                                        ELSEIF ABS(y(j) - y(i)) > ABS(x(j) - x(i)) THEN
                                                IF y(j) - y(i) < 0 THEN shys = -20
                                                IF y(j) - y(i) > 0 THEN shys = 20
                                                a = ABS(y(j) - y(i)) / 20
                                                IF x(j) - x(i) < 0 THEN shxs = -(ABS(x(j) - x(i)) / a)
                                                IF x(j) - x(i) > 0 THEN shxs = (ABS(x(j) - x(i)) / a)
                                        END IF
                                        'SOUND 1250, .1
                                        FOR k = 1 TO 50
                                                IF le(k) <= 0 THEN
                                                        le(k) = 1
                                                        lx(k) = x(i)
                                                        ly(k) = y(i)
                                                        lxs(k) = shxs
                                                        lys(k) = shys
                                                        EXIT FOR
                                                END IF
                                        NEXT k
                                        shxs = 0
                                        shys = 0
                                        EXIT FOR
                                END IF
                                NEXT j
                        END IF
                        IF cool2(i) = 0 THEN
                                FOR j = 1 TO 5
                                        IF cme(j) <= 0 THEN
                                                cool2(i) = 1000
                                                cmx(j) = x(i)
                                                cmy(j) = y(i)
                                                cmdx(j) = INT(RND * 560) + 40
                                                cmdy(j) = INT(RND * 420) + 30
                                                cmxs(j) = -xs(i)
                                                cmys(j) = -ys(i)
                                                cmt(j) = c(i)
                                                cme(j) = 1
                                                EXIT FOR
                                        END IF
                                NEXT j
                        END IF
                        shxs = 0
                        shys = 0
                        IF cool(i) = 0 THEN
                                start& = INT(RND * 5) + 1
                                FOR j = start& TO 100
                                IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 160000 AND c(j) <> c(i) AND a(j) > 0 THEN
                                        IF ABS(x(j) - x(i)) > ABS(y(j) - y(i)) THEN
                                                IF x(j) - x(i) < 0 THEN shxs = -20
                                                IF x(j) - x(i) > 0 THEN shxs = 20
                                                a = ABS(x(j) - x(i)) / 20
                                                IF y(j) - y(i) < 0 THEN shys = -(ABS(y(j) - y(i)) / a)
                                                IF y(j) - y(i) > 0 THEN shys = (ABS(y(j) - y(i)) / a)
                                        ELSEIF ABS(y(j) - y(i)) > ABS(x(j) - x(i)) THEN
                                                IF y(j) - y(i) < 0 THEN shys = -20
                                                IF y(j) - y(i) > 0 THEN shys = 20
                                                a = ABS(y(j) - y(i)) / 20
                                                IF x(j) - x(i) < 0 THEN shxs = -(ABS(x(j) - x(i)) / a)
                                                IF x(j) - x(i) > 0 THEN shxs = (ABS(x(j) - x(i)) / a)
                                        END IF
                                        shots& = INT(RND * 15) + 10
                                        SOUND 1250, .1
                                        FOR k = 1 TO 50
                                                IF le(k) <= 0 THEN
                                                        cool(i) = 500
                                                        le(k) = 1
                                                        lx(k) = x(i) + INT(RND * 21) - 10
                                                        ly(k) = y(i) + INT(RND * 21) - 10
                                                        lxs(k) = shxs + INT(RND * 3) - 1
                                                        lys(k) = shys + INT(RND * 3) - 1
                                                        shots& = shots& - 1
                                                        IF shots& <= 0 THEN EXIT FOR
                                                END IF
                                        NEXT k
                                        EXIT FOR
                                END IF
                                NEXT j
                        END IF
                        IF INT(RND * 100) + 1 = 1 THEN
                                dxs(i) = INT(RND * 5) - 2
                                dys(i) = INT(RND * 5) - 2
                        END IF
                END IF
                IF m(i) = 0 THEN
                        IF INT(RND * 10) + 1 = 1 THEN
                                dxs(i) = INT(RND * 11) - 5
                                dys(i) = INT(RND * 11) - 5
                        END IF
                        IF cool(i) = 0 OR a(i) < 20 THEN
                                FOR j = 100 TO 1 STEP -1
                                        IF ABS(x(i) - x(j)) < 10 AND ABS(y(i) - y(j)) < 10 AND a(i) < 20 AND a(i) > 1 AND a(j) > 0 AND j <> i THEN
                                                a(i) = 1
                                                IF m(j) = 1 THEN a(j) = a(j) - 5 ELSE a(j) = 1
                                                IF a(j) = 0 THEN a(j) = 1
                                        END IF
                                        IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 22500 AND c(j) <> c(i) AND a(j) > 19 THEN
                                                IF ABS(x(j) - x(i)) > ABS(y(j) - y(i)) THEN
                                                        IF x(j) - x(i) < 0 THEN dxs(i) = -5
                                                        IF x(j) - x(i) > 0 THEN dxs(i) = 5
                                                        a = ABS(x(j) - x(i)) / 5
                                                        IF y(j) - y(i) < 0 THEN dys(i) = -(ABS(y(j) - y(i)) / a)
                                                        IF y(j) - y(i) > 0 THEN dys(i) = (ABS(y(j) - y(i)) / a)
                                                ELSEIF ABS(y(j) - y(i)) > ABS(x(j) - x(i)) THEN
                                                        IF y(j) - y(i) < 0 THEN dys(i) = -5
                                                        IF y(j) - y(i) > 0 THEN dys(i) = 5
                                                        a = ABS(y(j) - y(i)) / 5
                                                        IF x(j) - x(i) < 0 THEN dxs(i) = -(ABS(x(j) - x(i)) / a)
                                                        IF x(j) - x(i) > 0 THEN dxs(i) = (ABS(x(j) - x(i)) / a)
                                                END IF
                                                IF ((xs(i) = dxs(i) AND ys(i) = dys(i) OR (ABS(xs(i) - INT(dxs(i) / 2)) < 3) AND ABS(ys(i) - INT(dys(i) / 2)) < 3)) AND a(i) > 19 THEN
                                                        FOR k = 1 TO 50
                                                                IF le(k) <= 0 THEN
                                                                        SOUND 1250, .1
                                                                        le(k) = 1
                                                                        lx(k) = x(i)
                                                                        ly(k) = y(i)
                                                                        lxs(k) = dxs(i) * 4
                                                                        lys(k) = dys(i) * 4
                                                                        cool(i) = 5
                                                                        IF INT(RND * 2) = 1 THEN dxs(i) = -dxs(i) ELSE dys(i) = -dys(i)
                                                                        EXIT FOR
                                                                END IF
                                                        NEXT k
                                                END IF
                                        EXIT FOR
                                        END IF
                                NEXT j
                        END IF
                END IF
                IF m(i) = 2 THEN
                        IF INT(RND * 10) + 1 = 1 THEN
                                dxs(i) = INT(RND * 7) - 3
                                dys(i) = INT(RND * 7) - 3
                        END IF
                        IF cool(i) = 0 OR a(i) < 19 THEN
                                FOR j = 100 TO 1 STEP -1
                                        IF ABS(x(i) - x(j)) < 10 AND ABS(y(i) - y(j)) < 10 AND a(i) < 20 AND a(i) > 1 AND a(j) > 0 AND j <> i THEN
                                                a(i) = 1
                                                IF m(j) = 1 THEN a(j) = a(j) - 5 ELSE a(j) = 1
                                                IF a(j) = 0 THEN a(j) = 1
                                        END IF
                                        IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 50000 AND c(j) <> c(i) AND a(j) > 19 THEN
                                                IF a(i) > 19 THEN
                                                        dxs(i) = 0
                                                        dys(i) = 0
                                                        SOUND 100, .1
                                                        FOR k = 1 TO 50
                                                                IF me(k) <= 0 THEN
                                                                        cool(i) = 20
                                                                        mxs(k) = 0
                                                                        mys(k) = 0
                                                                        me(k) = 40
                                                                        mtar(k) = j
                                                                        mt(k) = c(i)
                                                                        mx(k) = x(i)
                                                                        my(k) = y(i)
                                                                        EXIT FOR
                                                                END IF
                                                        NEXT k
                                                END IF
                                        EXIT FOR
                                        END IF
                                NEXT j
                        END IF
                END IF
                IF cool(i) > 0 THEN cool(i) = cool(i) - 1
                IF m(i) = 0 THEN CIRCLE (x(i) - screenx, y(i) - screeny), 1, 0
                IF m(i) = 1 THEN
                        IF xs(i) = 0 AND ys(i) = 0 THEN
                                starfollow& = 1
                                xs(i) = -starxs&
                                ys(i) = -starys&
                        END IF
                        PSET (x(i) + (xs(i) * 3) - screenx, y(i) + (ys(i) * 3) - screeny), 0
                        IF (xs(i) = 0 AND ys(i) = -1) OR (xs(i) = 0 AND ys(i) = -2) OR (xs(i) = 0 AND ys(i) = 0) THEN DRAW  _
"A0 BU4 BM+8,+6 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF (xs(i) = 0 AND ys(i) = 1) OR (xs(i) = 0 AND ys(i) = 2) THEN DRAW  _
"A2 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF (xs(i) = 1 AND ys(i) = 0) OR (xs(i) = 2 AND ys(i) = 0) THEN DRAW  _
"A3 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4 A1"
                        IF (xs(i) = -1 AND ys(i) = 0) OR (xs(i) = -2 AND ys(i) = 0) THEN DRAW  _
"A1 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4 A3"
                        IF (xs(i) = ys(i) AND xs(i) = 1) OR (xs(i) = ys(i) AND xs(i) = 2) THEN DRAW  _
"TA225 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF (xs(i) = ys(i) AND xs(i) = -1) OR (xs(i) = ys(i) AND xs(i) = -2) THEN DRAW  _
"TA45 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF (xs(i) = -ys(i) AND xs(i) = 1) OR (xs(i) = -ys(i) AND xs(i) = 2) THEN DRAW  _
"TA315 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF (xs(i) = -ys(i) AND xs(i) = -1) OR (xs(i) = -ys(i) AND xs(i) = -2) THEN DRAW  _
"TA135 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = 1 AND ys(i) = -2 THEN DRAW  _
"TA338 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = 2 AND ys(i) = -1 THEN DRAW  _
"TA293 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = 1 AND ys(i) = 2 THEN DRAW  _
"TA203 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = 2 AND ys(i) = 1 THEN DRAW  _
"TA248 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = -1 AND ys(i) = 2 THEN DRAW  _
"TA153 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = -2 AND ys(i) = 1 THEN DRAW  _
"TA113 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = -1 AND ys(i) = -2 THEN DRAW  _
"TA23 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF xs(i) = -2 AND ys(i) = -1 THEN DRAW  _
"TA68 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 D6 L1 U5 D2 L13 U3 L1 U3 D3 L1 D6 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 L1 U4 R2 D4"
                        IF starfollow& = 1 THEN
                                starfollow& = 0
                                xs(i) = 0
                                ys(i) = 0
                        END IF
                END IF
                IF m(i) = 2 THEN CIRCLE (x(i) - screenx, y(i) - screeny), 3, 0
                IF dxs(i) < xs(i) AND turn& = 1 THEN xs(i) = xs(i) - 1
                IF dxs(i) > xs(i) AND turn& = 1 THEN xs(i) = xs(i) + 1
                IF dys(i) < ys(i) AND turn& = 1 THEN ys(i) = ys(i) - 1
                IF dys(i) > ys(i) AND turn& = 1 THEN ys(i) = ys(i) + 1
                IF onn& = 1 THEN xs(player&) = p1xs&
                IF onn& = 1 THEN ys(player&) = p1ys&
                x(i) = x(i) + xs(i)
                y(i) = y(i) + ys(i)
                IF x(i) < 10 OR x(i) > 629 THEN xs(i) = -xs(i)
                IF y(i) < 10 OR y(i) > 469 THEN ys(i) = -ys(i)
                IF x(i) < 10 OR x(i) > 629 THEN dxs(i) = -dxs(i)
                IF y(i) < 10 OR y(i) > 469 THEN dys(i) = -dys(i)
                IF x(i) < 10 THEN x(i) = 10
                IF y(i) < 10 THEN y(i) = 10
                IF x(i) > 629 THEN x(i) = 629
                IF y(i) > 469 THEN y(i) = 469
                IF m(i) = 0 THEN CIRCLE (x(i) - screenx, y(i) - screeny), 1, c(i)
                IF m(i) = 1 THEN
                        IF shxs <> 0 OR shys <> 0 THEN
                                xs(i) = shxs / 10
                                ys(i) = shys / 10
                                dxs(i) = shxs / 10
                                dys(i) = shys / 10
                        END IF
                        IF onn& = 1 THEN xs(player&) = p1xs&
                        IF onn& = 1 THEN ys(player&) = p1ys&
                        starfollow& = 0
                        IF xs(i) = 0 AND ys(i) = 0 THEN
                                starfollow& = 1
                                xs(i) = -starxs&
                                ys(i) = -starys&
                        END IF
                        PSET (x(i) + (xs(i) * 3) - screenx, y(i) + (ys(i) * 3) - screeny), c(i)
                        IF (xs(i) = 0 AND ys(i) = -1) OR (xs(i) = 0 AND ys(i) = -2) OR (xs(i) = 0 AND ys(i) = 0) THEN DRAW  _
"A0 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF (xs(i) = 0 AND ys(i) = 1) OR (xs(i) = 0 AND ys(i) = 2) THEN DRAW  _
"A2 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF (xs(i) = 1 AND ys(i) = 0) OR (xs(i) = 2 AND ys(i) = 0) THEN DRAW  _
"A3 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4 A1"
                        IF (xs(i) = -1 AND ys(i) = 0) OR (xs(i) = -2 AND ys(i) = 0) THEN DRAW  _
"A1 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4 A3"
                        IF (xs(i) = ys(i) AND xs(i) = 1) OR (xs(i) = ys(i) AND xs(i) = 2) THEN DRAW  _
"TA225 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF (xs(i) = ys(i) AND xs(i) = -1) OR (xs(i) = ys(i) AND xs(i) = -2) THEN DRAW  _
"TA45 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF (xs(i) = -ys(i) AND xs(i) = 1) OR (xs(i) = -ys(i) AND xs(i) = 2) THEN DRAW  _
"TA315 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF (xs(i) = -ys(i) AND xs(i) = -1) OR (xs(i) = -ys(i) AND xs(i) = -2) THEN DRAW  _
"TA135 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = 1 AND ys(i) = -2 THEN DRAW  _
"TA338 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = 2 AND ys(i) = -1 THEN DRAW  _
"TA293 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = 1 AND ys(i) = 2 THEN DRAW  _
"TA203 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = 2 AND ys(i) = 1 THEN DRAW  _
"TA248 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = -1 AND ys(i) = 2 THEN DRAW  _
"TA153 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = -2 AND ys(i) = 1 THEN DRAW  _
"TA113 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = -1 AND ys(i) = -2 THEN DRAW  _
"TA23 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF xs(i) = -2 AND ys(i) = -1 THEN DRAW  _
"TA68 BM+8,+6 BU4 D3 BL16 U3 BM+8,-6 C7 R2 L4 BM-2,+1 R8 BM+1,+1 L10 G1 R12 BM-4,+1 L4 D1 R4 D1 L4 G1 R6 D1 L6 D1 R6 F1 L8 G1 R11 U2 R1 U3 D3 R1 C8 D6 C7 L1 U5 D2 L13 U3 L1 U3 D3 L1 C8 D6 C7 R1 U5 D3 BR4 R6 G1 L4 BM+2,-1 C8 L1 U4 R2 D4"
                        IF starfollow& = 1 THEN
                                starfollow& = 0
                                xs(i) = 0
                                ys(i) = 0
                        END IF
                END IF
                IF m(i) = 2 THEN CIRCLE (x(i) - screenx, y(i) - screeny), 3, c(i)
                IF a(i) <= 19 THEN
                        FOR j = 1 TO 200
                        IF ss(j) <= 0 THEN
                                ss(j) = 1
                                sx(j) = x(i)
                                sy(j) = y(i)
                                IF m(i) = 1 THEN sx(j) = x(i)
                                IF m(i) = 1 THEN sy(j) = y(i)
                                EXIT FOR
                        END IF
                        NEXT j
                        a(i) = a(i) - 1
                        IF a(i) <= 0 THEN
                                CIRCLE (x(i) - screenx, y(i) - screeny), 1, 0
                                CIRCLE (x(i) - screenx, y(i) - screeny), 3, 0
                                FOR j = 1 TO 50
                                        IF es(j) <= 0 THEN
                                                es(j) = 1
                                                ex(j) = x(i)
                                                ey(j) = y(i)
                                                ems(j) = 10
                                                IF m(i) = 1 THEN ems(j) = 15
                                                EXIT FOR
                                        END IF
                                NEXT j
                                IF m(i) = 1 THEN
                                        FOR j = 1 TO 100
                                        IF a(j) > 0 AND ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 22500 THEN
                                                IF m(j) <> 1 AND a(j) > 19 THEN
                                                        a(j) = 19
                                                        IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 5625 THEN a(j) = 1
                                                        IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 400 THEN a(j) = 0
                                                ELSEIF m(j) = 1 THEN
                                                        a(j) = a(j) - 5
                                                        IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 5625 THEN a(j) = a(j) - 10
                                                        IF ((x(i) - x(j)) ^ 2 + (y(i) - y(j)) ^ 2) < 400 THEN a(j) = a(j) - 20
                                                        IF a(j) < 1 THEN a(j) = 1
                                                END IF
                                        END IF
                                        NEXT j
                                        back& = 63
                                        FOR j = 1 TO 10
                                                IF shes(j) <= 0 THEN
                                                        shes(j) = 1
                                                        shex(j) = x(i)
                                                        shey(j) = y(i)
                                                        shems(j) = 1000
                                                        EXIT FOR
                                                END IF
                                        NEXT j
                                END IF
                                trails& = INT(RND * 3) + 2
                                IF m(i) = 1 THEN trails& = trails& * 6
                                FOR j = 1 TO 50
                                IF tra(j) <= 0 THEN
                                        tra(j) = 1
                                        trx(j) = x(i)
                                        try(j) = y(i)
                                        tran(j) = INT(RND * 361)
                                        trs(j) = INT(RND * 4) + 4
                                        trails& = trails& - 1
                                        IF trails& <= 0 THEN EXIT FOR
                                END IF
                                NEXT j
                                m(i) = 0
                        END IF
                END IF
        END IF
NEXT i
turn& = turn& + 1
IF turn& = 2 THEN turn& = 0
ink$ = INKEY$
IF ink$ = "=" THEN player& = player& + 1
IF ink$ = "-" THEN player& = player& - 1
IF player& > 5 THEN player& = 5
IF player& < 1 THEN player& = 1
IF ink$ = "o" THEN onn& = -onn&
IF ink$ = "8" THEN p1ys& = p1ys& - 1
IF ink$ = "4" THEN p1xs& = p1xs& - 1
IF ink$ = "6" THEN p1xs& = p1xs& + 1
IF ink$ = "2" THEN p1ys& = p1ys& + 1
IF p1xs& > 2 THEN p1xs& = 2
IF p1xs& < -2 THEN p1xs& = -2
IF p1ys& > 2 THEN p1ys& = 2
IF p1ys& < -2 THEN p1ys& = -2
IF ink$ = "c" THEN CLS
IF ink$ = "s" THEN
        LOCATE 1, 1
        PRINT "Interceptors:"; uniti&
        PRINT "Avr Lifespan: "; CLNG((lifei& / uniti&) * 100) / 100; "sec"
        PRINT
        PRINT "Missle Launchers:"; unitl&
        PRINT "Avr Lifespan: "; CLNG((lifel& / unitl&) * 100) / 100; "sec"
        PRINT
        PRINT "Motherships: 5"
        PRINT "Avr Lifespan: "; CLNG((lifem& / 5) * 100) / 100; "sec"
        SLEEP
        CLS
END IF
IF ink$ = "n" THEN
        FOR i = 1 TO 100
        IF a(i) > 0 THEN a(i) = 19
        NEXT i
        lifel& = 0
        unitl& = 0
        lifei& = 0
        uniti& = 0
        lifem& = 0
END IF
IF ink$ = "m" THEN
        FOR i = 1 TO 5
        FOR k = 1 TO 50
        IF me(k) <= 0 THEN
                mxs(k) = 0
                mys(k) = 0
                me(k) = 40
                mt(k) = -1
                mx(k) = 320
                my(k) = -4000
                EXIT FOR
        END IF
        NEXT k
        NEXT i
END IF
IF show > 0 THEN
        show = show - 1
        LINE (deathx - 5, deathy)-(deathx + 5, deathy), 0
        LINE (deathx, deathy - 5)-(deathx, deathy + 5), 0
END IF
LOOP UNTIL ink$ = CHR$(27)

