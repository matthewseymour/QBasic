SCREEN 12
team& = -2
rs& = 650
bs& = 490
RANDOMIZE TIMER
DIM ux(1 TO 300) AS INTEGER
DIM uy(1 TO 300) AS INTEGER
DIM ub(1 TO 300) AS INTEGER
DIM ut(1 TO 300) AS INTEGER
DIM uh(1 TO 300) AS INTEGER
DIM uc(1 TO 300) AS INTEGER
DIM ud(1 TO 300) AS INTEGER
DIM ucd(1 TO 300) AS INTEGER
DIM uxs(1 TO 300) AS INTEGER
DIM uys(1 TO 300) AS INTEGER
DIM ubxs(1 TO 300) AS INTEGER
DIM ubys(1 TO 300) AS INTEGER
DIM udxs(1 TO 300) AS INTEGER
DIM udys(1 TO 300) AS INTEGER
DIM ugxs(1 TO 300) AS INTEGER
DIM ugys(1 TO 300) AS INTEGER
DIM utype(1 TO 300) AS INTEGER
DIM utarg(1 TO 300) AS INTEGER
DIM sx(1 TO 300) AS INTEGER
DIM sy(1 TO 300) AS INTEGER
DIM sd(1 TO 300) AS INTEGER
DIM ste(1 TO 300) AS INTEGER
DIM sxs(1 TO 300) AS INTEGER
DIM sys(1 TO 300) AS INTEGER
DIM st(1 TO 300) AS INTEGER
DIM ex(1 TO 300) AS INTEGER
DIM ey(1 TO 300) AS INTEGER
DIM es(1 TO 300) AS INTEGER
DIM ems(1 TO 300) AS INTEGER
a = 0
lx& = 0
ty& = 0
rx& = 640
by& = 480
a = -1
teams& = 8
FOR i = 300 TO (300 - teams&) + 1 STEP -1
a = a + 1
IF a = 6 THEN a = 7
uxs(i) = INT(RND * 3) - 1
uys(i) = INT(RND * 3) - 1
ux(i) = INT(RND * (rx& - 20)) + lx& + 10
uy(i) = INT(RND * (by& - 20)) + ty& + 10
uh(i) = 100
ut(i) = a
ub(i) = 1
NEXT i
ux(300) = 50
uy(300) = 50
ux(299) = 320
uy(299) = 50
ux(298) = 590
uy(298) = 50
ux(297) = 590
uy(297) = 240
ux(296) = 590
uy(296) = 430
ux(295) = 320
uy(295) = 430
ux(294) = 50
uy(294) = 430
ux(293) = 50
uy(293) = 240

CLS
LINE (lx& - 20, ty& - 20)-(rx& + 20, by& + 20), 6, BF
DO
FOR i = 1 TO 300
IF st(i) > 0 THEN
        PSET (sx(i), sy(i)), 6
        sx(i) = sx(i) + sxs(i)
        sy(i) = sy(i) + sys(i)
        PSET (sx(i), sy(i)), (INT(RND * 2) * 10) + 4
        st(i) = st(i) - 1
        FOR j = 1 TO 300
                IF uh(j) > 0 AND ut(j) <> ste(i) THEN
                        IF ABS(ux(j) - sx(i)) < 7 AND ABS(uy(j) - sy(i)) < 7 THEN
                                st(i) = 0
                                uh(j) = uh(j) - sd(i) + INT(RND * 3) - 1
                                IF utype(j) = 4 AND uh(j) < 10 THEN
                                        uc(j) = 0
                                        uh(j) = 100
                                        utype(j) = 0
                                        ub(j) = 1
                                END IF
                                IF uh(j) < 1 THEN uh(j) = 1
                                EXIT FOR
                        END IF
                END IF
        NEXT j
        IF st(i) = 0 THEN
                PSET (sx(i), sy(i)), 6
                FOR j = 1 TO sd(i) - 1
                FOR k = 1 TO 300
                IF es(k) = 0 THEN
                        es(k) = 1
                        ems(k) = 2 + sd(i)
                        ex(k) = sx(i) + INT(RND * 5) - 2
                        ey(k) = sy(i) + INT(RND * 5) - 2
                        EXIT FOR
                END IF
                NEXT k
                NEXT j
        END IF
END IF
NEXT i
FOR i = 1 TO 300
IF es(i) > 0 THEN
        es(i) = es(i) + 1
        IF ex(i) > -10 AND ey(i) > -10 AND ex(i) < rs& AND ey(i) < bs& THEN
                red& = 2
                FOR j = es(i) TO 1 STEP -1
                IF red& > 0 THEN co& = 4 ELSE co& = 14
                IF red& = 1 THEN co& = 12
                red& = red& - 1
                IF es(i) = ems(i) THEN co& = 6
                CIRCLE (ex(i), ey(i)), j, co&
                CIRCLE (ex(i) + 1, ey(i)), j, co&
                NEXT j
        END IF
        IF es(i) = ems(i) THEN
                es(i) = 0
        END IF
END IF
NEXT i
bases& = 0
tanks& = 0
FOR i = 1 TO 300
IF uh(i) = 0 THEN GOTO skip
IF ub(i) = 1 THEN
        bases& = bases& + 1
        IF uh(i) = 1 THEN
                FOR k = 1 TO 300
                IF es(k) = 0 THEN
                        es(k) = 1
                        ems(k) = 20
                        ex(k) = ux(i)
                        ey(k) = uy(i)
                        LINE (ux(i) - 5, uy(i) - 5)-(ux(i) + 5, uy(i) + 5), 6, BF
                        EXIT FOR
                END IF
                NEXT k
                uh(i) = 0
                GOTO skip
        END IF
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN LINE (ux(i) - 5, uy(i) - 5)-(ux(i) + 5, uy(i) + 5), ut(i), BF
        IF uc(i) > 0 THEN uc(i) = uc(i) - 1
        IF uc(i) = 0 THEN
                uc(i) = 120 + INT(RND * 11)
                FOR j = 1 TO 300
                IF uh(j) = 0 THEN
                        ub(j) = 0
                        IF ub(j) = 0 THEN uc(i) = 120 + INT(RND * 11) ELSE uc(i) = 250 + INT(RND * 11)
                        b& = INT(RND * 2) + 1
                        utype(j) = 1
                        IF INT(RND * 2) = 1 THEN utype(j) = 2
                        IF INT(RND * 3) = 1 THEN utype(j) = 3
                        IF INT(RND * 25) = 1 THEN utype(j) = 4
                        ud(j) = 1 + utype(j)
                        ucd(j) = 10 + (utype(j) * 5)
                        ux(j) = ux(i) + INT(RND * 10) + 1
                        uy(j) = uy(i) + INT(RND * 10) + 1
                        utarg(j) = INT(RND * 300) + 1
                        ubxs(j) = 1
                        IF b& = 1 THEN ux(j) = ux(i) + 10
                        IF b& = 2 THEN uy(j) = uy(i) + 10
                        IF INT(RND * 2) = 1 THEN
                                IF b& = 1 THEN ux(j) = ux(j) - 20
                                IF b& = 2 THEN uy(j) = uy(j) - 20
                        END IF
                        udxs(j) = uxs(i)
                        udys(j) = uys(i)
                        ut(j) = ut(i)
                        uh(j) = 10 * utype(j)
                        IF utype(j) = 4 THEN uh(j) = 60
                        IF utype(j) = 4 THEN uc(j) = 1000
                        EXIT FOR
                END IF
                NEXT j
        END IF
END IF
IF ub(i) = 0 AND utype(i) < 4 THEN
        tanks& = tanks& + 1
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN LINE (ux(i) + INT(ugxs(i) / 2), uy(i) + INT(ugys(i) / 2))-(ux(i) + ugxs(i), uy(i) + ugys(i)), 6
        FOR m = 1 TO 30
        j = utarg(i)
        b& = 0
        b2& = 0
        IF ut(j) <> ut(i) AND uh(j) > 0 THEN
                IF (ux(j) - ux(i)) ^ 2 + (uy(j) - uy(i)) ^ 2 <= (75 + (utype(i) * 25)) ^ 2 THEN
                        b& = 1
                        IF ABS(ux(j) - ux(i)) > ABS(uy(j) - uy(i)) THEN
                                IF ux(j) - ux(i) < 0 THEN ugxs(i) = -(9 + utype(i))
                                IF ux(j) - ux(i) > 0 THEN ugxs(i) = (9 + utype(i))
                                a = ABS(ux(j) - ux(i)) / (9 + utype(i))
                                IF uy(j) - uy(i) < 0 THEN ugys(i) = -(ABS(uy(j) - uy(i)) / a)
                                IF uy(j) - uy(i) > 0 THEN ugys(i) = (ABS(uy(j) - uy(i)) / a)
                        ELSEIF ABS(uy(j) - uy(i)) > ABS(ux(j) - ux(i)) THEN
                                IF uy(j) - uy(i) < 0 THEN ugys(i) = -(9 + utype(i))
                                IF uy(j) - uy(i) > 0 THEN ugys(i) = (9 + utype(i))
                                a = ABS(uy(j) - uy(i)) / (9 + utype(i))
                                IF ux(j) - ux(i) < 0 THEN ugxs(i) = -(ABS(ux(j) - ux(i)) / a)
                                IF ux(j) - ux(i) > 0 THEN ugxs(i) = (ABS(ux(j) - ux(i)) / a)
                        END IF
                        udxs(i) = 0
                        udys(i) = 0
                        IF uc(i) = 0 THEN
                                FOR k = 1 TO 300
                                        IF st(k) = 0 THEN
                                                uc(i) = ucd(i) + INT(RND * 5) - 2
                                                st(k) = 8 + (utype(i) * 2)
                                                ste(k) = ut(i)
                                                sd(k) = ud(i)
                                                sx(k) = ux(i) + ugxs(i)
                                                sy(k) = uy(i) + ugys(i)
                                                sxs(k) = ugxs(i)
                                                sys(k) = ugys(i)
                                                IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN SOUND INT(RND * 1000) + 37, .1
                                                utarg(j) = i
                                                GOTO skip3
                                        END IF
                                NEXT k
                        END IF
                        EXIT FOR
                ELSEIF (ux(j) - ux(i)) ^ 2 + (uy(j) - uy(i)) ^ 2 <= 40000 THEN
                        b2& = 1
                        udxs(i) = 0
                        udys(i) = 0
                        IF ux(j) - ux(i) > 20 THEN udxs(i) = 1
                        IF ux(j) - ux(i) < -20 THEN udxs(i) = -1
                        IF uy(j) - uy(i) > 20 THEN udys(i) = 1
                        IF uy(j) - uy(i) < -20 THEN udys(i) = -1
                END IF
        END IF
skip3:        
        IF b& = 0 AND b2& = 0 THEN
                utarg(i) = INT(RND * 300) + 1
        END IF
        NEXT m
        IF b& = 0 AND (uxs(i) <> 0 OR uys(i) <> 0) AND ugxs(i) <> uxs(i) AND ugys(i) <> uys(i) THEN LINE (ux(i), uy(i))-(ux(i) + ugxs(i), uy(i) + ugys(i)), 6
        IF b& = 0 AND (uxs(i) <> 0 OR uys(i) <> 0) THEN ugxs(i) = uxs(i) * 10
        IF b& = 0 AND (uxs(i) <> 0 OR uys(i) <> 0) THEN ugys(i) = uys(i) * 10
        IF uc(i) > 0 THEN uc(i) = uc(i) - 1
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN
                IF (ubxs(i) = 1 AND ubys(i) = 1) OR (ubxs(i) = -1 AND ubys(i) = -1) THEN
                        LINE (ux(i) - 2, uy(i) - 8)-(ux(i) + 8, uy(i) + 2), 6
                        LINE (ux(i) - 2, uy(i) - 8)-(ux(i) - 8, uy(i) - 2), 6
                        LINE (ux(i) + 2, uy(i) + 8)-(ux(i) + 8, uy(i) + 2), 6
                        LINE (ux(i) + 2, uy(i) + 8)-(ux(i) - 8, uy(i) - 2), 6
                ELSEIF (ubxs(i) = -1 AND ubys(i) = 1) OR (ubxs(i) = 1 AND ubys(i) = -1) THEN
                        LINE (ux(i) + 2, uy(i) - 8)-(ux(i) - 8, uy(i) + 2), 6
                        LINE (ux(i) + 2, uy(i) - 8)-(ux(i) + 8, uy(i) - 2), 6
                        LINE (ux(i) - 2, uy(i) + 8)-(ux(i) - 8, uy(i) + 2), 6
                        LINE (ux(i) - 2, uy(i) + 8)-(ux(i) + 8, uy(i) - 2), 6
                END IF
                IF ut(i) = team& OR team& = -1 THEN CIRCLE (ux(i), uy(i)), 75 + (utype(i) * 25), 6
                FOR j = 1 TO 1 + utype(i)
                CIRCLE (ux(i), uy(i)), j, 6
                NEXT j
                LINE (ux(i) - 1, uy(i) - 1)-(ux(i) + 1, uy(i) + 1), 6, BF
                IF (ubxs(i) = 1 AND ubys(i) = 0) OR (ubxs(i) = -1 AND ubys(i) = 0) THEN LINE (ux(i) - 8, uy(i) - 4)-(ux(i) + 8, uy(i) + 4), 6, B
                IF (ubxs(i) = 0 AND ubys(i) = 1) OR (ubxs(i) = 0 AND ubys(i) = -1) THEN LINE (ux(i) - 4, uy(i) - 8)-(ux(i) + 4, uy(i) + 8), 6, B
        END IF
        IF uh(i) = 1 THEN
                FOR k = 1 TO 300
                IF es(k) = 0 THEN
                        es(k) = 1
                        ems(k) = 10
                        ex(k) = ux(i)
                        ey(k) = uy(i)
                        EXIT FOR
                END IF
                NEXT k
                uh(i) = 0
                LINE (ux(i) - 10, uy(i) - 10)-(ux(i) + 10, uy(i) + 10), 6, BF
                GOTO skip
        END IF
        ux(i) = ux(i) + uxs(i)
        uy(i) = uy(i) + uys(i)
        FOR p = 1 TO 300
        IF uh(p) > 0 AND i <> p AND ABS(ux(i) - ux(p)) <= 6 AND ABS(uy(i) - uy(p)) <= 6 THEN
                DO
                ux(i) = ux(i) + (1 + -(2 * (INT(RND * 2))))
                uy(i) = uy(i) + (1 + -(2 * (INT(RND * 2))))
                LOOP UNTIL (ABS(ux(i) - ux(p)) > 5 AND ABS(uy(i) - uy(p)) > 5)
        END IF
        NEXT p
        IF ux(i) > rx& - 10 OR ux(i) < lx& + 10 THEN uxs(i) = -uxs(i)
        IF uy(i) > by& - 10 OR uy(i) < ty& + 10 THEN uys(i) = -uys(i)
        IF ux(i) > rx& - 10 OR ux(i) < lx& + 10 THEN udxs(i) = -udxs(i)
        IF uy(i) > by& - 10 OR uy(i) < ty& + 10 THEN udys(i) = -udys(i)
        IF ux(i) < lx& + 10 THEN ux(i) = lx& + 10
        IF uy(i) < ty& + 10 THEN uy(i) = ty& + 10
        IF ux(i) > rx& - 10 THEN ux(i) = rx& - 10
        IF uy(i) > by& - 10 THEN uy(i) = by& - 10
        IF INT(RND * 2) + 1 = 1 THEN
                IF uxs(i) < udxs(i) THEN uxs(i) = uxs(i) + 1
                IF uxs(i) > udxs(i) THEN uxs(i) = uxs(i) - 1
        END IF
        IF INT(RND * 2) + 1 = 1 THEN
                IF uys(i) < udys(i) THEN uys(i) = uys(i) + 1
                IF uys(i) > udys(i) THEN uys(i) = uys(i) - 1
        END IF
        IF udxs(i) = 0 AND udys(i) = 0 THEN
                udxs(i) = ugxs(i) / 10
                udys(i) = ugys(i) / 10
        END IF
        IF INT(RND * 100) = 1 THEN udxs(i) = INT(RND * 3) - 1
        IF INT(RND * 100) = 1 THEN udys(i) = INT(RND * 3) - 1
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN
                IF uxs(i) <> 0 OR uys(i) <> 0 THEN
                        ubxs(i) = uxs(i)
                        ubys(i) = uys(i)
                END IF
                IF (ubxs(i) = 1 AND ubys(i) = 1) OR (ubxs(i) = -1 AND ubys(i) = -1) THEN
                        LINE (ux(i) - 2, uy(i) - 8)-(ux(i) + 8, uy(i) + 2), ut(i)
                        LINE (ux(i) - 2, uy(i) - 8)-(ux(i) - 8, uy(i) - 2), ut(i)
                        LINE (ux(i) + 2, uy(i) + 8)-(ux(i) + 8, uy(i) + 2), ut(i)
                        LINE (ux(i) + 2, uy(i) + 8)-(ux(i) - 8, uy(i) - 2), ut(i)
                ELSEIF (ubxs(i) = -1 AND ubys(i) = 1) OR (ubxs(i) = 1 AND ubys(i) = -1) THEN
                        LINE (ux(i) + 2, uy(i) - 8)-(ux(i) - 8, uy(i) + 2), ut(i)
                        LINE (ux(i) + 2, uy(i) - 8)-(ux(i) + 8, uy(i) - 2), ut(i)
                        LINE (ux(i) - 2, uy(i) + 8)-(ux(i) - 8, uy(i) + 2), ut(i)
                        LINE (ux(i) - 2, uy(i) + 8)-(ux(i) + 8, uy(i) - 2), ut(i)
                END IF
                IF (ubxs(i) = 1 AND ubys(i) = 0) OR (ubxs(i) = -1 AND ubys(i) = 0) THEN LINE (ux(i) - 8, uy(i) - 4)-(ux(i) + 8, uy(i) + 4), ut(i), B
                IF (ubxs(i) = 0 AND ubys(i) = 1) OR (ubxs(i) = 0 AND ubys(i) = -1) THEN LINE (ux(i) - 4, uy(i) - 8)-(ux(i) + 4, uy(i) + 8), ut(i), B
                IF ut(i) = team& OR team& = -1 THEN CIRCLE (ux(i), uy(i)), 75 + (utype(i) * 25), ut(i)
                FOR j = 1 TO 1 + utype(i)
                CIRCLE (ux(i), uy(i)), j, ut(i)
                NEXT j
                LINE (ux(i) - 1, uy(i) - 1)-(ux(i) + 1, uy(i) + 1), ut(i), BF
                LINE (ux(i) + INT(ugxs(i) / 2), uy(i) + INT(ugys(i) / 2))-(ux(i) + ugxs(i), uy(i) + ugys(i)), ut(i)
        END IF
END IF
IF utype(i) = 4 THEN
        IF uc(i) > 0 THEN uc(i) = uc(i) - 1
        IF INT(RND * 1000) = 1 OR uc(i) = 0 THEN
                utype(i) = 0
                uc(i) = 0
                ub(i) = 1
                uh(i) = 100
                GOTO skip
        END IF
        j = utarg(i)
        IF (ux(j) - ux(i)) ^ 2 + (uy(j) - uy(i)) ^ 2 <= 40000 THEN
                b2& = 1
                udxs(i) = 0
                udys(i) = 0
                IF ux(j) - ux(i) > 20 THEN udxs(i) = -1
                IF ux(j) - ux(i) < -20 THEN udxs(i) = 1
                IF uy(j) - uy(i) > 20 THEN udys(i) = -1
                IF uy(j) - uy(i) < -20 THEN udys(i) = 1
        END IF
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN
                LINE (ux(i) + 2, uy(i) + 2)-(ux(i) - 2, uy(i) - 2), 6, BF
                CIRCLE (ux(i), uy(i)), 3, 6
                CIRCLE (ux(i), uy(i)), 5, 6
        END IF
        IF uh(i) = 1 THEN
                FOR k = 1 TO 300
                IF es(k) = 0 THEN
                        es(k) = 1
                        ems(k) = 10
                        ex(k) = ux(i)
                        ey(k) = uy(i)
                        EXIT FOR
                END IF
                NEXT k
                uh(i) = 0
                GOTO skip
        END IF
        ux(i) = ux(i) + uxs(i)
        uy(i) = uy(i) + uys(i)
        IF ux(i) > rx& - 10 OR ux(i) < lx& + 10 THEN uxs(i) = -uxs(i)
        IF uy(i) > by& - 10 OR uy(i) < ty& + 10 THEN uys(i) = -uys(i)
        IF ux(i) > rx& - 10 OR ux(i) < lx& + 10 THEN udxs(i) = -udxs(i)
        IF uy(i) > by& - 10 OR uy(i) < ty& + 10 THEN udys(i) = -udys(i)
        IF ux(i) < lx& + 10 THEN ux(i) = lx& + 10
        IF uy(i) < ty& + 10 THEN uy(i) = ty& + 10
        IF ux(i) > rx& - 10 THEN ux(i) = rx& - 10
        IF uy(i) > by& - 10 THEN uy(i) = by& - 10
        IF uxs(i) < udxs(i) THEN uxs(i) = uxs(i) + 1
        IF uxs(i) > udxs(i) THEN uxs(i) = uxs(i) - 1
        IF uys(i) < udys(i) THEN uys(i) = uys(i) + 1
        IF uys(i) > udys(i) THEN uys(i) = uys(i) - 1
        IF INT(RND * 100) = 1 THEN udxs(i) = INT(RND * 3) - 1
        IF INT(RND * 100) = 1 THEN udys(i) = INT(RND * 3) - 1
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN
                LINE (ux(i) + 2, uy(i) + 2)-(ux(i) - 2, uy(i) - 2), ut(i), BF
                CIRCLE (ux(i), uy(i)), 3, ut(i)
                CIRCLE (ux(i), uy(i)), 5, ut(i)
        END IF
END IF
IF ub(i) = 2 THEN
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN LINE (ux(i) + INT(ugxs(i) / 2), uy(i) + INT(ugys(i) / 2))-(ux(i) + ugxs(i), uy(i) + ugys(i)), 6
        FOR m = 1 TO 30
        j = utarg(i)
        b& = 0
        b2& = 0
        IF ut(j) <> ut(i) AND uh(j) > 0 THEN
                IF (ux(j) - ux(i)) ^ 2 + (uy(j) - uy(i)) ^ 2 <= (75 + (utype(i) * 25)) ^ 2 THEN
                        b& = 1
                        IF ABS(ux(j) - ux(i)) > ABS(uy(j) - uy(i)) THEN
                                IF ux(j) - ux(i) < 0 THEN ugxs(i) = -(9 + utype(i))
                                IF ux(j) - ux(i) > 0 THEN ugxs(i) = (9 + utype(i))
                                a = ABS(ux(j) - ux(i)) / (9 + utype(i))
                                IF uy(j) - uy(i) < 0 THEN ugys(i) = -(ABS(uy(j) - uy(i)) / a)
                                IF uy(j) - uy(i) > 0 THEN ugys(i) = (ABS(uy(j) - uy(i)) / a)
                        ELSEIF ABS(uy(j) - uy(i)) > ABS(ux(j) - ux(i)) THEN
                                IF uy(j) - uy(i) < 0 THEN ugys(i) = -(9 + utype(i))
                                IF uy(j) - uy(i) > 0 THEN ugys(i) = (9 + utype(i))
                                a = ABS(uy(j) - uy(i)) / (9 + utype(i))
                                IF ux(j) - ux(i) < 0 THEN ugxs(i) = -(ABS(ux(j) - ux(i)) / a)
                                IF ux(j) - ux(i) > 0 THEN ugxs(i) = (ABS(ux(j) - ux(i)) / a)
                        END IF
                        udxs(i) = 0
                        udys(i) = 0
                        IF uc(i) = 0 THEN
                                FOR k = 1 TO 300
                                        IF st(k) = 0 THEN
                                                uc(i) = ucd(i) + INT(RND * 5) - 2
                                                st(k) = 8 + (utype(i) * 2)
                                                ste(k) = ut(i)
                                                sd(k) = ud(i)
                                                sx(k) = ux(i) + ugxs(i)
                                                sy(k) = uy(i) + ugys(i)
                                                sxs(k) = ugxs(i)
                                                sys(k) = ugys(i)
                                                IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN SOUND INT(RND * 1000) + 37, .1
                                                utarg(j) = i
                                                GOTO skip4
                                        END IF
                                NEXT k
                        END IF
                        EXIT FOR
                ELSEIF (ux(j) - ux(i)) ^ 2 + (uy(j) - uy(i)) ^ 2 <= 40000 THEN
                        b2& = 1
                        udxs(i) = 0
                        udys(i) = 0
                        IF ux(j) - ux(i) > 20 THEN udxs(i) = 1
                        IF ux(j) - ux(i) < -20 THEN udxs(i) = -1
                        IF uy(j) - uy(i) > 20 THEN udys(i) = 1
                        IF uy(j) - uy(i) < -20 THEN udys(i) = -1
                END IF
        END IF
skip4:   
        IF b& = 0 AND b2& = 0 THEN
                utarg(i) = INT(RND * 300) + 1
        END IF
        NEXT m
        IF b& = 0 AND (uxs(i) <> 0 OR uys(i) <> 0) AND ugxs(i) <> uxs(i) AND ugys(i) <> uys(i) THEN LINE (ux(i), uy(i))-(ux(i) + ugxs(i), uy(i) + ugys(i)), 6
        IF b& = 0 AND (uxs(i) <> 0 OR uys(i) <> 0) THEN ugxs(i) = uxs(i) * 10
        IF b& = 0 AND (uxs(i) <> 0 OR uys(i) <> 0) THEN ugys(i) = uys(i) * 10
        IF uc(i) > 0 THEN uc(i) = uc(i) - 1
        IF ux(i) > -10 AND uy(i) > -10 AND ux(i) < rs& AND uy(i) < bs& THEN
                IF ut(i) = team& OR team& = -1 THEN CIRCLE (ux(i), uy(i)), 75 + (utype(i) * 25), 6
                FOR j = 1 TO 1 + utype(i)
                CIRCLE (ux(i), uy(i)), j, 6
                NEXT j
                LINE (ux(i) - 1, uy(i) - 1)-(ux(i) + 1, uy(i) + 1), 6, BF
        END IF
        IF uh(i) = 1 THEN
                FOR k = 1 TO 300
                IF es(k) = 0 THEN
                        es(k) = 1
                        ems(k) = 10
                        ex(k) = ux(i)
                        ey(k) = uy(i)
                        EXIT FOR
                END IF
                NEXT k
                uh(i) = 0
                LINE (ux(i) - 10, uy(i) - 10)-(ux(i) + 10, uy(i) + 10), 6, BF
                GOTO skip
        END IF
        FOR p = 1 TO 300
        NEXT p
END IF
skip:
NEXT i
LOCATE 1, 1
PRINT "Bases:"; bases&
PRINT "Tanks:"; tanks&
a$ = INKEY$
IF a$ = CHR$(27) THEN END
IF a$ = "=" THEN
        LINE (lx& - 7, ty& - 7)-(rx& + 7, by& + 7), 6, BF
        team& = team& + 1
        IF team& = 6 THEN team& = 7
ELSEIF a$ = "-" THEN
        LINE (lx& - 7, ty& - 7)-(rx& + 7, by& + 7), 6, BF
        team& = team& - 1
        IF team& = 6 THEN team& = 5
END IF
IF team& < -2 THEN team& = teams&
IF team& > teams& THEN team& = -2
IF a$ = CHR$(0) + "H" THEN movey& = 100
IF a$ = CHR$(0) + "K" THEN movex& = 100
IF a$ = CHR$(0) + "M" THEN movex& = -100
IF a$ = CHR$(0) + "P" THEN movey& = -100
IF movex& <> 0 OR movey& <> 0 THEN GOSUB move
LOOP

move:
lx& = lx& + movex&
rx& = rx& + movex&
ty& = ty& + movey&
by& = by& + movey&
FOR i = 1 TO 300
ux(i) = ux(i) + movex&
uy(i) = uy(i) + movey&
sx(i) = sx(i) + movex&
sy(i) = sy(i) + movey&
ex(i) = ex(i) + movex&
ey(i) = ey(i) + movey&
NEXT i
CLS
LINE (lx& - 7, ty& - 7)-(rx& + 7, by& + 7), 6, BF
movex& = 0
movey& = 0
RETURN





