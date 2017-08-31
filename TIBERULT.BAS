ultimatesupremebeginning:
CLS
SCREEN 12
VIEW
CLS
GOSUB intro

actualstart:
CLS
LINE (0, 0)-(640, 480), 6, BF
notstarted& = 0
RANDOMIZE TIMER
mx& = INT(RND * 634) + 6
my& = INT(RND * 441) + 39
gx& = INT(RND * 634) + 6
gy& = INT(RND * 441) + 39
basex& = INT(RND * 619) + 6
basey& = INT(RND * 426) + 39
compbasex& = INT(RND * 619) + 6
compbasey& = INT(RND * 426) + 39
wingrep& = 15
wingrepair& = 0
winghmax& = 50
winghealth& = winghmax&
wingrange& = 60
wingmove& = 1
wingshot& = 10
wingmax& = 5
wingmin& = 1
lockk& = 0
lockk2& = 0
wlockk& = 0
wlockk2& = 0
tiberiumspeed& = 1
cash& = 0
bank& = 100
gem& = 2000
mine& = 100
opp& = 3
p1& = basex& + 12
p2& = basey& + 12
wx& = p1& + 3
wy& = p2&
RANDOMIZE TIMER
enemy1& = INT(RND * 640) + 33
enemy2& = INT(RND * 480) + 33
enemy3& = INT(RND * 640) + 33
enemy4& = INT(RND * 480) + 33
enemy5& = INT(RND * 640) + 33
enemy6& = INT(RND * 480) + 33
healthmax& = 100
health& = healthmax&
speed& = 2
compmove& = 2
compshot& = 25
c1hmax& = 20
c1health& = c1hmax&
c2hmax& = 20
c2health& = c2hmax&
c3hmax& = 20
c3health& = c3hmax&
turrshot& = 50
turrange& = 100
turrhealth& = 50
comprange& = 50
tdmax& = 10
tdmin& = 5
enmax& = 2
enmin& = 1
maxcash& = 100
kills& = 0
bomb& = 0
bombdet& = 0
bombsize& = 0
bombcount& = 100
bombposs& = 1
moving& = 0

turr1h& = turrhealth&
turr2h& = turrhealth&

RANDOMIZE TIMER
a& = INT(RND * 640) + 1
B& = INT(RND * 447) + 33
c& = INT(RND * 640) + 1
d& = INT(RND * 447) + 33

LINE (0, 0)-(640, 33), 14, BF
LOCATE 1, 1
PRINT "          "
LOCATE 1, 70
PRINT "          "
LOCATE 2, 70
PRINT "          "
LOCATE 2, 12
PRINT "        "
LOCATE 2, 1
PRINT "          "

wingpossible& = 1

RANDOMIZE TIMER
c1targ& = INT(RND * 2) + 1
c2targ& = INT(RND * 2) + 1
c3targ& = INT(RND * 2) + 1

www& = 0

VIEW SCREEN (1, 33)-(639, 479)

the:

IF c1targ& = 1 THEN
        c1targx& = p1&
        c1targy& = p2&
ELSEIF c1targ& = 2 THEN
        c1targx& = wx&
        c1targy& = wy&
END IF

IF c2targ& = 1 THEN
        c2targx& = p1&
        c2targy& = p2&
ELSEIF c2targ& = 2 THEN
        c2targx& = wx&
        c2targy& = wy&
END IF

IF c3targ& = 1 THEN
        c3targx& = p1&
        c3targy& = p2&
ELSEIF c3targ& = 2 THEN
        c3targx& = wx&
        c3targy& = wy&
END IF

IF wingpossible& = 0 THEN wingrepair& = 1
IF wingrepair& = 1 THEN wingpossible& = 0
IF winghealth& < 0 THEN winghealth& = 0
IF winghealth& < 0 THEN wingrepair& = 1

4

IF health& <= 0 THEN GOTO die
IF winghealth& < 1 THEN GOSUB wingdie
IF c1health& <= 0 THEN GOSUB c1die
IF c2health& <= 0 THEN GOSUB c2die
IF c3health& <= 0 THEN GOSUB c3die

LINE (0, 33)-(640, 33), 14

VIEW

LOCATE 1, 30
COLOR 15
PRINT "TIBERIUM VINE"
LOCATE 2, 28
COLOR 15
PRINT "PRESS 'p' TO PAUSE"
IF bomb& = 0 THEN
        LOCATE 1, 12
        COLOR 7
        PRINT TIME$
ELSEIF bomb& = 1 THEN
        LOCATE 1, 12
        COLOR 7
        PRINT endcount& - bombcount&
END IF
LOCATE 2, 70
COLOR 2
PRINT cash&
LOCATE 1, 70
COLOR 3
PRINT bank&
LOCATE 1, 1
COLOR 5
PRINT health&
LOCATE 2, 1
COLOR 14
PRINT winghealth&
LOCATE 2, 12
COLOR 13
PRINT kills&

VIEW SCREEN (1, 33)-(639, 479)

LINE (mx&, my&)-(mx&, my& + 8), 0
CIRCLE (mx&, my&), 1, 0
CIRCLE (mx&, my& + 2), 2, 0
CIRCLE (mx&, my& + 4), 3, 0
CIRCLE (mx&, my& + 6), 4, 0
CIRCLE (mx&, my& + 8), 5, 0
LINE (gx&, gy&)-(gx&, gy& + 8), 12
CIRCLE (gx&, gy&), 1, 12
CIRCLE (gx&, gy& + 2), 2, 12
CIRCLE (gx&, gy& + 4), 3, 12
CIRCLE (gx&, gy& + 6), 4, 12
CIRCLE (gx&, gy& + 8), 5, 12

wingman:
GOSUB wprobe
LINE (wx& - 2, wy& - 2)-(wx& + 2, wy& + 2), 6, B
LINE (wx& - 1, wy& - 1)-(wx& + 1, wy& + 1), 7, BF
IF wingpossible& = 1 THEN
        RANDOMIZE TIMER
        z& = INT(RND * wingmove&) + 1
        IF z& = 1 THEN
                www& = www& + 1
                IF www& = 100 THEN www& = 0
                IF www& = 1 THEN
                        RANDOMIZE TIMER
                        wingtarg& = INT(RND * 6) + 1
                        IF wingtarg& = 1 THEN
                                wingtargx& = enemy1&
                                wingtargy& = enemy2&
                        ELSEIF wingtarg& = 2 THEN
                                wingtargx& = enemy3&
                                wingtargy& = enemy4&
                        ELSEIF wingtarg& = 3 THEN
                                wingtargx& = enemy5&
                                wingtargy& = enemy6&
                        ELSEIF wingtarg& = 4 THEN
                                wingtargx& = mx&
                                wingtargy& = my&
                        ELSEIF wingtarg& = 5 THEN
                                wingtargx& = gx&
                                wingtargy& = gy&
                        ELSE
                                wingtargx& = p1&
                                wingtargy& = p2&
                        END IF
                        IF beacon& = 1 THEN
                                wingtargx& = beaconx&
                                wingtargy& = beacony&
                        END IF
                END IF
        END IF
         
wingskip:
                IF winghealth& < wingrep& THEN
                        wingtargx& = basex& + 5
                        wingtargy& = basey& + 5
                END IF
   
                RANDOMIZE TIMER
                z& = INT(RND * 2) + 1
                IF z& = 1 THEN
                        IF wingtargx& = wx& THEN GOTO wsx
qaz:
                        IF wingtargx& < wx& THEN
                                LINE (wx& - 2, wy& - 2)-(wx& + 2, wy& + 2), 6, B
                                wx& = wx& - 1
                        ELSEIF wingtargx& > wx& THEN
                                LINE (wx& - 2, wy& - 2)-(wx& + 2, wy& + 2), 6, B
                                wx& = wx& + 1
                        END IF
                ELSEIF z& = 2 THEN
                        IF wingtargy& = wy& THEN GOTO qaz
wsx:
                        IF wingtargy& < wy& THEN
                                LINE (wx& - 2, wy& - 2)-(wx& + 2, wy& + 2), 6, B
                                wy& = wy& - 1
                        ELSEIF wingtargy& > wy& THEN
                                LINE (wx& - 2, wy& - 2)-(wx& + 2, wy& + 2), 6, B
                                wy& = wy& + 1
                        END IF
                END IF

        IF wx& < 0 THEN wx& = 1
        IF wx& > 640 THEN wx& = 640
        IF wy& < 35 THEN wy& = 35
        IF wy& > 480 THEN wy& = 480
      
        RANDOMIZE TIMER
        z& = INT(RND * wingmove&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                wingtarg& = INT(RND * 6) + 1
                IF wingtarg& = 1 THEN
                        qwingtargx& = enemy1&
                        qwingtargy& = enemy2&
                ELSEIF wingtarg& = 2 THEN
                        qwingtargx& = enemy3&
                        qwingtargy& = enemy4&
                ELSEIF wingtarg& = 3 THEN
                        qwingtargx& = enemy5&
                        qwingtargy& = enemy6&
                ELSEIF wingtarg& = 4 THEN
                        qwingtargx& = mx&
                        qwingtargy& = my&
                ELSEIF wingtarg& = 5 THEN
                        qwingtargx& = gx&
                        qwingtargy& = gy&
                ELSE
                        qwingtargx& = p1&
                        qwingtargy& = p2&

                END IF
        END IF

        IF qwingtargx& = p1& THEN
                IF qwingtargy& = p2& THEN GOTO noshot
        END IF
        targ& = 0
        IF -wingrange& <= wx& - qwingtargx& AND wx& - qwingtargx& <= wingrange& THEN targ& = targ& + 1
        IF -wingrange& <= wy& - qwingtargy& AND wy& - qwingtargy& <= wingrange& THEN targ& = targ& + 1
        IF targ& = 2 THEN
                RANDOMIZE TIMER
                z& = INT(RND * wingshot&) + 1
                IF z& = 1 THEN
                        FOR r& = 1 TO 15
                        RANDOMIZE TIMER
                        u& = INT(RND * 2) + 1
                        hdi& = INT(RND * 500) + 37
                        IF u& = 1 THEN u& = 4
                        IF u& = 2 THEN u& = 12
                        LINE (wx&, wy&)-(qwingtargx&, qwingtargy&), u&
                        SOUND hdi&, .1
                        LINE (wx&, wy&)-(qwingtargx&, qwingtargy&), 6
                        NEXT r&
                        LOCATE 2, 70
                        PRINT "          "
                        RANDOMIZE TIMER
                        damage& = INT(RND * wingmax&) + wingmin&
                        IF wingtarg& = 1 THEN
                                c1health& = c1health& - damage&
                        ELSEIF wingtarg& = 2 THEN
                                c2health& = c2health& - damage&
                        ELSEIF wingtarg& = 3 THEN
                                c3health& = c3health& - damage&
                        ELSEIF wingtarg& = 4 THEN
                                turr1h& = turr1h& - damage&
                        ELSEIF wingtarg& = 5 THEN
                                turr2h& = turr2h& - damage&
                        END IF
                END IF
        END IF
END IF

IF turr1h& <= 0 THEN
rerun:
        RANDOMIZE TIMER
        expcol1& = INT(RND * 16)
        expcol2& = INT(RND * 16)
        IF expcol1& = expcol2& THEN GOTO rerun
        FOR ring& = 1 TO 50
                RANDOMIZE TIMER
                expcol3& = INT(RND * 2) + 1
                IF expcol3& = 1 THEN ringcol& = expcol1&
                IF expcol3& = 2 THEN ringcol& = expcol2&
                CIRCLE (mx&, my&), ring&, ringcol&
        NEXT ring&
        RANDOMIZE TIMER
        mx& = INT(RND * 634) + 6
        my& = INT(RND * 441) + 39
        turr1h& = turrhealth&
END IF

IF turr2h& <= 0 THEN
rerun2:
        RANDOMIZE TIMER
        expcol1& = INT(RND * 16)
        expcol2& = INT(RND * 16)
        IF expcol1& = expcol2& THEN GOTO rerun
        FOR ring& = 1 TO 50
                RANDOMIZE TIMER
                expcol3& = INT(RND * 2) + 1
                IF expcol3& = 1 THEN ringcol& = expcol1&
                IF expcol3& = 2 THEN ringcol& = expcol2&
                CIRCLE (gx&, gy&), ring&, ringcol&
        NEXT ring&
        RANDOMIZE TIMER
        gx& = INT(RND * 634) + 6
        gy& = INT(RND * 441) + 39
        turr2h& = turrhealth&
END IF

noshot:

IF lockk& = 0 THEN GOSUB missile
IF lockk& = 1 THEN GOSUB fire
RANDOMIZE TIMER
smokecol& = INT(RND * 3) + 1
IF smokecol& = 1 THEN smokecol& = 7
IF smokecol& = 2 THEN smokecol& = 8
IF smokecol& = 3 THEN smokecol& = 15
PSET (tx&, ty&), smokecol&
CIRCLE (tx&, ty&), 19, 6
IF missound& < 38 THEN missound& = 38
missound& = missound& - 1
IF lockk& = 1 THEN GOSUB fire
PSET (tx&, ty&), 7
CIRCLE (tx&, ty&), 19, 6
IF missound& < 38 THEN missound& = 38
missound& = missound& - 1
IF lockk& = 1 THEN GOSUB fire
PSET (tx&, ty&), 7
CIRCLE (tx&, ty&), 19, 6
IF missound& < 38 THEN missound& = 38
missound& = missound& - 1

IF wlockk& = 0 THEN GOSUB wmissile
IF wlockk& = 1 THEN GOSUB wfire
RANDOMIZE TIMER
smokecol& = INT(RND * 3) + 1
IF smokecol& = 1 THEN smokecol& = 7
IF smokecol& = 2 THEN smokecol& = 8
IF smokecol& = 3 THEN smokecol& = 15
PSET (wtx&, wty&), smokecol&
CIRCLE (wtx&, wty&), 19, 6
IF wmissound& < 38 THEN wmissound& = 38
wmissound& = wmissound& - 1
IF wlockk& = 1 THEN GOSUB wfire
PSET (wtx&, wty&), 7
CIRCLE (wtx&, wty&), 19, 6
IF wmissound& < 38 THEN wmissound& = 38
wmissound& = wmissound& - 1
IF wlockk& = 1 THEN GOSUB wfire
PSET (wtx&, wty&), 7
CIRCLE (wtx&, wty&), 19, 6
IF wmissound& < 38 THEN wmissound& = 38
wmissound& = wmissound& - 1


IF lockk2& = 0 THEN GOSUB missile2
IF lockk2& = 1 THEN GOSUB fire2
RANDOMIZE TIMER
smokecol2& = INT(RND * 3) + 1
IF smokecol2& = 1 THEN smokecol2& = 7
IF smokecol2& = 2 THEN smokecol2& = 8
IF smokecol2& = 3 THEN smokecol2& = 15
PSET (ix&, iy&), smokecol2&
CIRCLE (ix&, iy&), 19, 6
IF missound2& < 38 THEN missound2& = 38
missound2& = missound2& - 1
IF lockk2& = 1 THEN GOSUB fire2
PSET (ix&, iy&), 7
CIRCLE (ix&, iy&), 19, 6
IF missound2& < 38 THEN missound2& = 38
missound2& = missound2& - 1
IF lockk2& = 1 THEN GOSUB fire2
PSET (ix&, iy&), 7
CIRCLE (ix&, iy&), 19, 6
IF missound2& < 38 THEN missound2& = 38
missound2& = missound2& - 1

IF wlockk2& = 0 THEN GOSUB wmissile2
IF wlockk2& = 1 THEN GOSUB wfire2
RANDOMIZE TIMER
smokecol2& = INT(RND * 3) + 1
IF smokecol2& = 1 THEN smokecol2& = 7
IF smokecol2& = 2 THEN smokecol2& = 8
IF smokecol2& = 3 THEN smokecol2& = 15
PSET (wix&, wiy&), smokecol2&
CIRCLE (wix&, wiy&), 19, 6
IF wmissound2& < 38 THEN wmissound2& = 38
wmissound2& = wmissound2& - 1
IF wlockk2& = 1 THEN GOSUB wfire2
PSET (wix&, wiy&), 7
CIRCLE (wix&, wiy&), 19, 6
IF wmissound2& < 38 THEN wmissound2& = 38
wmissound2& = wmissound2& - 1
IF wlockk2& = 1 THEN GOSUB wfire2
PSET (wix&, wiy&), 7
CIRCLE (wix&, wiy&), 19, 6
IF wmissound2& < 38 THEN wmissound2& = 38
wmissound2& = wmissound2& - 1

PSET (p1&, p2&), 15

GOSUB c1probe
LINE (enemy1& - 2, enemy2& - 2)-(enemy1& + 2, enemy2& + 2), 6, B
LINE (enemy1& - 1, enemy2& - 1)-(enemy1& + 1, enemy2& + 1), 0, BF
IF range& = 1 THEN
        GOTO skipper
ELSEIF range& = 0 THEN
        RANDOMIZE TIMER
        z& = INT(RND * compmove&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                z& = INT(RND * 2) + 1
                IF z& = 1 THEN
                        IF c1targx& < enemy1& THEN
                                enemy1& = enemy1& - 1
                        ELSEIF c1targx& > enemy1& THEN
                                enemy1& = enemy1& + 1
                        END IF
                ELSEIF z& = 2 THEN
                        IF c1targy& < enemy2& THEN
                                enemy2& = enemy2& - 1
                        ELSEIF c1targy& > enemy2& THEN
                                enemy2& = enemy2& + 1
                        END IF
                END IF
        END IF
END IF

IF enemy1& < 0 THEN enemy1& = 1
IF enemy1& > 640 THEN enemy1& = 640
IF enemy2& < 35 THEN enemy2& = 35
IF enemy2& > 480 THEN enemy2& = 480

skipper:
wtarg& = 0
targ& = 0
IF -comprange& <= enemy1& - p1& AND enemy1& - p1& <= comprange& THEN targ& = targ& + 1
IF -comprange& <= enemy2& - p2& AND enemy2& - p2& <= comprange& THEN targ& = targ& + 1
IF -comprange& <= enemy1& - wx& AND enemy1& - wx& <= comprange& THEN wtarg& = wtarg& + 1
IF -comprange& <= enemy2& - wy& AND enemy2& - wy& <= comprange& THEN wtarg& = wtarg& + 1
IF targ& = 2 OR wtarg& = 2 THEN
        range& = 1
        RANDOMIZE TIMER
        z& = INT(RND * compshot&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                entarg& = INT(RND * 2) + 1
                IF entarg& = 1 THEN
                        IF targ& = 2 THEN
                                FOR r& = 1 TO 15
                                RANDOMIZE TIMER
                                u& = INT(RND * 15) + 1
                                hdi& = INT(RND * 1000) + 37
                                LINE (enemy1&, enemy2&)-(p1&, p2&), u&
                                SOUND hdi&, .1
                                LINE (enemy1&, enemy2&)-(p1&, p2&), 6
                                NEXT r&
                                LOCATE 2, 70
                                PRINT "          "
                                RANDOMIZE TIMER
                                damage& = INT(RND * enmax&) + enmin&
                                health& = health& - damage&
                        END IF
                ELSEIF entarg& = 2 THEN
                        IF wtarg& = 2 THEN
                                FOR r& = 1 TO 15
                                RANDOMIZE TIMER
                                u& = INT(RND * 15) + 1
                                hdi& = INT(RND * 1000) + 37
                                LINE (enemy1&, enemy2&)-(wx&, wy&), u&
                                SOUND hdi&, .1
                                LINE (enemy1&, enemy2&)-(wx&, wy&), 6
                                NEXT r&
                                LOCATE 2, 70
                                PRINT "          "
                                RANDOMIZE TIMER
                                damage& = INT(RND * enmax&) + enmin&
                                winghealth& = winghealth& - damage&
                        END IF
                END IF
        END IF
ELSE range& = 0
END IF

IF opp& >= 2 THEN GOSUB opp2

RANDOMIZE TIMER
mineodds& = INT(RND * mine&) + 1
IF mineodds& = 1 THEN
        RANDOMIZE TIMER
        minex& = INT(RND * 637) + 3
        miney& = INT(RND * 477) + 35
        FOR minecon& = 1 TO 2
        RANDOMIZE TIMER
        minesound& = INT(RND * 3000) + 1000
        SOUND minesound&, .1
        CIRCLE (minex&, miney&), minecon&, 2
        NEXT minecon&
END IF

RANDOMIZE TIMER
tiberium1& = INT(RND * tiberiumspeed&) + 1
FOR tiberium2 = 1 TO tiberium1&
clee& = INT(RND * 8) + 1
fixer:
IF clee& = 1 THEN
        a& = a& + 1
        B& = B& + 1
ELSEIF clee& = 2 THEN
        a& = a& - 1
        B& = B& - 1
ELSEIF clee& = 3 THEN
        a& = a& - 1
        B& = B& + 1
ELSEIF clee& = 4 THEN
        a& = a& + 1
        B& = B& - 1
ELSEIF clee& = 5 THEN
        a& = a& + 1
ELSEIF clee& = 6 THEN
        a& = a& - 1
ELSEIF clee& = 7 THEN
        B& = B& + 1
ELSEIF clee& = 8 THEN
        B& = B& - 1
END IF
IF a& <= 0 THEN
        clee& = 5
        GOTO fixer
ELSEIF a& >= 640 THEN
        clee& = 6
        GOTO fixer
ELSEIF B& <= 34 THEN
        clee& = 7
        GOTO fixer
ELSEIF B& >= 480 THEN
        clee& = 8
        GOTO fixer
END IF

LINE (basex&, basey&)-(basex& + 10, basey& + 10), 13, BF
LINE (basex&, basey& + 11)-(basex& + 10, basey& + 13), 5, BF
LINE (compbasex&, compbasey&)-(compbasex& + 10, compbasey& + 10), 8, BF
LINE (compbasex&, compbasey& + 11)-(compbasex& + 10, compbasey& + 13), 0, BF

PSET (a&, B&), 10
NEXT tiberium2

RANDOMIZE TIMER
tiberium1& = INT(RND * tiberiumspeed&) + 1
FOR tiberium2 = 1 TO tiberium1&
blee& = INT(RND * 8) + 1
fixer2:
IF blee& = 1 THEN
        c& = c& + 1
        d& = d& + 1
ELSEIF blee& = 2 THEN
        c& = c& - 1
        d& = d& - 1
ELSEIF blee& = 3 THEN
        c& = c& - 1
        d& = d& + 1
ELSEIF blee& = 4 THEN
        c& = c& + 1
        d& = d& - 1
ELSEIF blee& = 5 THEN
        c& = c& + 1
ELSEIF blee& = 6 THEN
        c& = c& - 1
ELSEIF blee& = 7 THEN
        d& = d& + 1
ELSEIF blee& = 8 THEN
        d& = d& - 1
END IF
IF c& <= 0 THEN
        blee& = 5
        GOTO fixer2
ELSEIF a& >= 640 THEN
        blee& = 6
        GOTO fixer2
ELSEIF B& <= 34 THEN
        blee& = 7
        GOTO fixer2
ELSEIF B& >= 480 THEN
        blee& = 8
        GOTO fixer2
END IF

LINE (basex&, basey&)-(basex& + 10, basey& + 10), 13, BF
LINE (basex&, basey& + 11)-(basex& + 10, basey& + 13), 5, BF
LINE (compbasex&, compbasey&)-(compbasex& + 10, compbasey& + 10), 0, BF
LINE (compbasex&, compbasey& + 11)-(compbasex& + 10, compbasey& + 13), 0, BF

PSET (c&, d&), 11
NEXT tiberium2

RANDOMIZE TIMER
gemodd& = INT(RND * gem&) + 1
IF gemodd& = 1 THEN
        RANDOMIZE TIMER
        gemx& = INT(RND * 640) + 1
        gemy& = INT(RND * 470) + 44
        gemsize& = INT(RND * 10) + 1
        FOR gemcon& = 1 TO gemsize&
        RANDOMIZE TIMER
        gemcol& = INT(RND * 2) + 1
        gemsound& = INT(RND * 100) + 37
        IF gemcol& = 1 THEN gemcol& = 3
        IF gemcol& = 2 THEN gemcol& = 1
        CIRCLE (gemx&, gemy&), gemcon&, gemcol&
        SOUND gemsound&, .1
        NEXT gemcon&
END IF

drive$ = INKEY$
IF drive$ = "8" THEN
        moving& = 1
        dir& = 8
ELSEIF drive$ = "6" THEN
        moving& = 1
        dir& = 6
ELSEIF drive$ = "2" THEN
        moving& = 1
        dir& = 2
ELSEIF drive$ = "4" THEN
        moving& = 1
        dir& = 4
ELSEIF drive$ = "7" THEN
        moving& = 1
        dir& = 7
ELSEIF drive$ = "1" THEN
        moving& = 1
        dir& = 1
ELSEIF drive$ = "3" THEN
        moving& = 1
        dir& = 3
ELSEIF drive$ = "9" THEN
        moving& = 1
        dir& = 9
ELSEIF drive$ = "5" THEN
        moving& = 0
ELSEIF drive$ = "q" THEN GOTO ultimatesupremebeginning
ELSEIF drive$ = "/" THEN GOSUB wrskip
ELSEIF drive$ = "p" THEN GOSUB pause
ELSEIF drive$ = "-" THEN
        IF beacon& = 0 THEN
                beacon& = 1
                beaconx& = p1&
                beacony& = p2&
        ELSE
                beacon& = 0
                PSET (beaconx&, beacony&), 6
        END IF
ELSEIF drive$ = "+" THEN
        IF bombposs& = 1 THEN
                IF bank& >= 100 THEN
                        LOCATE 1, 12
                        PRINT "        "
                        bomb& = 1
                        bombsize& = 0
                        bombcount& = TIMER
                        endcount& = TIMER
                        endcount& = endcount& + 2
                        bombx& = p1&
                        bomby& = p2&
                        bombposs& = 0
                        bank& = bank& - 100
                END IF
        END IF
END IF

IF p2& < 34 THEN p2& = 34
IF p2& > 479 THEN p2& = 479
IF p1& < 0 THEN p1& = 0
IF p1& > 639 THEN p1& = 639

IF moving& = 1 THEN
        IF dir& = 1 THEN
                p1& = p1& - 1
                p2& = p2& + 1
        ELSEIF dir& = 2 THEN
                p2& = p2& + 1
        ELSEIF dir& = 3 THEN
                p1& = p1& + 1
                p2& = p2& + 1
        ELSEIF dir& = 4 THEN
                p1& = p1& - 1
        ELSEIF dir& = 6 THEN
                p1& = p1& + 1
        ELSEIF dir& = 7 THEN
                p1& = p1& - 1
                p2& = p2& - 1
        ELSEIF dir& = 8 THEN
                p2& = p2& - 1
        ELSEIF dir& = 9 THEN
                p1& = p1& + 1
                p2& = p2& - 1
        END IF
        GOSUB plow
END IF

IF bomb& = 1 THEN bombcount& = endcount& - bombcount&
IF bombcount& = 0 THEN
        bomb& = 0
        bombdet& = 1
        bombcount& = 100
        bombcol& = 2
END IF

IF bombdet& = 1 THEN
        bombsize& = bombsize& + 1
        CIRCLE (bombx&, bomby&), bombsize&, bombcol&
END IF

IF bombsize& = 50 THEN
        IF bombcol& = 6 THEN
                bombdet& = 0
                bombposs& = 1
        END IF
        IF bombcol& = 2 THEN
                bombsize& = 0
                bombcol& = 6
        END IF
END IF

FOR unload& = basex& TO basex& + 10
IF p1& = unload& THEN
        FOR unload2& = basey& TO basey& + 10
        IF p2& = unload2& THEN
                LOCATE 1, 70
                PRINT "          "
                LOCATE 2, 70
                PRINT "          "
                bank& = bank& + cash&
                cash& = 0
                IF drive$ = "*" THEN
                        DO
                        IF health& = healthmax& OR bank& <= 0 THEN EXIT DO
                        bank& = bank& - 5
                        health& = health& + 1
                        LOOP
                        IF bank& < 0 THEN bank& = 0
                END IF
        END IF
        NEXT unload2&
END IF
NEXT unload&

IF beacon& = 1 THEN
        cha& = cha& + 1
        IF cha& > 2 THEN cha& = 1
        IF cha& = 1 THEN bco& = 15
        IF cha& = 2 THEN bco& = 0
        PSET (beaconx&, beacony&), bco&
END IF

IF bomb& = 1 THEN
        cha2& = cha2& + 1
        IF cha2& > 20 THEN cha2& = 1
        IF cha2& < 11 THEN bom& = 12
        IF cha2& > 10 THEN bom& = 9
        PSET (bombx&, bomby&), bom&
ELSE
        PSET (bombx&, bomby&), 6
END IF
bombcount& = TIMER

GOTO the

plow:
plow& = POINT(p1& - 1, p2& - 1)
GOSUB probe
plow& = POINT(p1& - 1, p2&)
GOSUB probe
plow& = POINT(p1& - 1, p2& + 1)
GOSUB probe
plow& = POINT(p1&, p2& - 1)
GOSUB probe
plow& = POINT(p1&, p2& + 1)
GOSUB probe
plow& = POINT(p1& + 1, p2& - 1)
GOSUB probe
plow& = POINT(p1& + 1, p2&)
GOSUB probe
plow& = POINT(p1& + 1, p2& + 1)
GOSUB probe
GOSUB renew
IF cash& > maxcash& THEN cash& = maxcash&
RETURN

probe:
IF plow& = 10 OR plow& = 11 THEN
        cash& = cash& + 1
ELSEIF plow& = 1 THEN
        cash& = cash& + 5
ELSEIF plow& = 3 THEN
        cash& = cash& + 5
ELSEIF plow& = 2 THEN GOSUB mineexp
ELSEIF plow& = 11 THEN GOSUB mineexp
END IF
RETURN

LOCATE 1, 1
PRINT cash&
RETURN

renew:
LINE (p1& - 1, p2& - 1)-(p1& + 1, p2& + 1), 6, BF
PSET (p1&, p2&), 6
RETURN

opp2:
GOSUB c2probe
LINE (enemy3& - 2, enemy4& - 2)-(enemy3& + 2, enemy4& + 2), 6, B
LINE (enemy3& - 1, enemy4& - 1)-(enemy3& + 1, enemy4& + 1), 12, BF
IF range2& = 1 THEN
        GOTO skipper2
ELSEIF range2& = 0 THEN
        RANDOMIZE TIMER
        z& = INT(RND * compmove&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                z& = INT(RND * 2) + 1
                IF z& = 1 THEN
                        IF c2targx& < enemy3& THEN
                                enemy3& = enemy3& - 1
                        ELSEIF c2targx& > enemy3& THEN
                                enemy3& = enemy3& + 1
                        END IF
                ELSEIF z& = 2 THEN
                        IF c2targy& < enemy4& THEN
                                enemy4& = enemy4& - 1
                        ELSEIF c2targy& > enemy4& THEN
                                enemy4& = enemy4& + 1
                        END IF
                END IF
        END IF
END IF

IF enemy3& < 0 THEN enemy3& = 1
IF enemy3& > 640 THEN enemy3& = 640
IF enemy4& < 35 THEN enemy4& = 35
IF enemy4& > 480 THEN enemy4& = 480


skipper2:
wtarg& = 0
targ& = 0
IF -comprange& <= enemy3& - p1& AND enemy3& - p1& <= comprange& THEN targ& = targ& + 1
IF -comprange& <= enemy4& - p2& AND enemy4& - p2& <= comprange& THEN targ& = targ& + 1
IF -comprange& <= enemy3& - wx& AND enemy3& - wx& <= comprange& THEN wtarg& = wtarg& + 1
IF -comprange& <= enemy4& - wy& AND enemy4& - wy& <= comprange& THEN wtarg& = wtarg& + 1
IF targ& = 2 OR wtarg& = 2 THEN
        range2& = 1
        RANDOMIZE TIMER
        z& = INT(RND * compshot&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                entarg& = INT(RND * 2) + 1
                IF entarg& = 1 THEN
                        IF targ& = 2 THEN
                                FOR r& = 1 TO 15
                                RANDOMIZE TIMER
                                u& = INT(RND * 15) + 1
                                hdi& = INT(RND * 1000) + 37
                                LINE (enemy3&, enemy4&)-(p1&, p2&), u&
                                SOUND hdi&, .1
                                LINE (enemy3&, enemy4&)-(p1&, p2&), 6
                                NEXT r&
                                LOCATE 2, 70
                                PRINT "          "
                                RANDOMIZE TIMER
                                damage& = INT(RND * enmax&) + enmin&
                                health& = health& - damage&
                        END IF
                ELSEIF entarg& = 2 THEN
                        IF wtarg& = 2 THEN
                                FOR r& = 1 TO 15
                                RANDOMIZE TIMER
                                u& = INT(RND * 15) + 1
                                hdi& = INT(RND * 1000) + 37
                                LINE (enemy3&, enemy4&)-(wx&, wy&), u&
                                SOUND hdi&, .1
                                LINE (enemy3&, enemy4&)-(wx&, wy&), 6
                                NEXT r&
                                LOCATE 2, 70
                                PRINT "          "
                                RANDOMIZE TIMER
                                damage& = INT(RND * enmax&) + enmin&
                                winghealth& = winghealth& - damage&
                        END IF
                END IF
        END IF
ELSE range2& = 0
END IF

IF opp& = 3 THEN GOSUB opp3
RETURN

opp3:
GOSUB c3probe
LINE (enemy5& - 2, enemy6& - 2)-(enemy5& + 2, enemy6& + 2), 6, B
LINE (enemy5& - 1, enemy6& - 1)-(enemy5& + 1, enemy6& + 1), 14, BF
IF range3& = 1 THEN
        GOTO skipper3
ELSEIF range3& = 0 THEN
        RANDOMIZE TIMER
        z& = INT(RND * compmove&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                z& = INT(RND * 2) + 1
                IF z& = 1 THEN
                        IF c3targx& < enemy5& THEN
                                enemy5& = enemy5& - 1
                        ELSEIF c3targx& > enemy5& THEN
                                enemy5& = enemy5& + 1
                        END IF
                ELSEIF z& = 2 THEN
                        IF c3targy& < enemy6& THEN
                                enemy6& = enemy6& - 1
                        ELSEIF c3targy& > enemy6& THEN
                                enemy6& = enemy6& + 1
                        END IF
                END IF
        END IF
END IF

IF enemy5& < 0 THEN enemy5& = 1
IF enemy5& > 640 THEN enemy5& = 640
IF enemy6& < 35 THEN enemy6& = 35
IF enemy6& > 480 THEN enemy6& = 480


skipper3:
wtarg& = 0
targ& = 0
IF -comprange& <= enemy5& - p1& AND enemy5& - p1& <= comprange& THEN targ& = targ& + 1
IF -comprange& <= enemy6& - p2& AND enemy6& - p2& <= comprange& THEN targ& = targ& + 1
IF -comprange& <= enemy5& - wx& AND enemy5& - wx& <= comprange& THEN wtarg& = wtarg& + 1
IF -comprange& <= enemy6& - wy& AND enemy6& - wy& <= comprange& THEN wtarg& = wtarg& + 1
IF targ& = 2 OR wtarg& = 2 THEN
        range3& = 1
        RANDOMIZE TIMER
        z& = INT(RND * compshot&) + 1
        IF z& = 1 THEN
                RANDOMIZE TIMER
                entarg& = INT(RND * 2) + 1
                IF entarg& = 1 THEN
                        IF targ& = 2 THEN
                                FOR r& = 1 TO 15
                                RANDOMIZE TIMER
                                u& = INT(RND * 15) + 1
                                hdi& = INT(RND * 1000) + 37
                                LINE (enemy5&, enemy6&)-(p1&, p2&), u&
                                SOUND hdi&, .1
                                LINE (enemy5&, enemy6&)-(p1&, p2&), 6
                                NEXT r&
                                LOCATE 2, 70
                                PRINT "          "
                                RANDOMIZE TIMER
                                damage& = INT(RND * enmax&) + enmin&
                                health& = health& - damage&
                        END IF
                ELSEIF entarg& = 2 THEN
                        IF wtarg& = 2 THEN
                                FOR r& = 1 TO 15
                                RANDOMIZE TIMER
                                u& = INT(RND * 15) + 1
                                hdi& = INT(RND * 1000) + 37
                                LINE (enemy5&, enemy6&)-(wx&, wy&), u&
                                SOUND hdi&, .1
                                LINE (enemy5&, enemy6&)-(wx&, wy&), 6
                                NEXT r&
                                LOCATE 2, 70
                                PRINT "          "
                                RANDOMIZE TIMER
                                damage& = INT(RND * enmax&) + enmin&
                                winghealth& = winghealth& - damage&
                        END IF
                END IF
        END IF
ELSE range3& = 0
END IF

RETURN

missile:
ty& = my&
tx& = mx&
target& = 0
distxx& = mx& - p1&
distyy& = my& - p2&
IF -turrange& <= distxx& AND distxx& <= turrange& THEN target& = target& + 1
IF -turrange& <= distyy& AND distyy& <= turrange& THEN target& = target& + 1
RANDOMIZE TIMER
lockodd& = INT(RND * turrshot&) + 1
IF lockodd& = 1 THEN
        IF target& = 2 THEN
                lockk& = 1
                missound& = 1000
        END IF
END IF
RETURN

fire:
IF tx& = p1& AND ty& = p2& THEN GOSUB explode
RANDOMIZE TIMER
wi& = INT(RND * 2) + 1
IF ix& = p1& THEN GOTO hfo2
IF wi& = 1 THEN
        IF tx& < p1& THEN
                tx& = tx& + 1
        ELSEIF tx& > p1& THEN
                tx& = tx& - 1
        END IF
ELSEIF wi& = 2 THEN
hfo:
        IF ty& < p2& THEN
                ty& = ty& + 1
        ELSEIF ty& > p2& THEN
                ty& = ty& - 1
        END IF
END IF
SOUND missound&, .1
RETURN

x:
distx& = mx& - p1&
IF ix& = p1& THEN GOTO y2
IF distx& > 0 THEN tx& = tx& - 1
IF distx& < 0 THEN tx& = tx& + 1
RETURN

y:
disty& = my& - p2&
IF tx& = p1& AND ty& = p2& THEN GOTO explode
IF tx& = wx& AND ty& = wy& THEN GOTO wexplode
IF ty& = p2& THEN GOTO x
IF disty& > 0 THEN ty& = ty& - 1
IF disty& < 0 THEN ty& = ty& + 1
RETURN

explode:
si& = 0
DO
RANDOMIZE TIMER
ifd& = INT(RND * 100) + 37
SOUND ifd&, .1
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (p1&, p2&), si&, 4
LOOP
si& = 0
DO
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (p1&, p2&), si&, 6
LOOP
lockk& = 0
RANDOMIZE TIMER
missdamage& = INT(RND * tdmax&) + tdmin&
health& = health& - missdamage&
RETURN

wmissile:
wty& = my&
wtx& = mx&
target& = 0
distxx& = mx& - wx&
distyy& = my& - wy&
IF -turrange& <= distxx& AND distxx& <= turrange& THEN target& = target& + 1
IF -turrange& <= distyy& AND distyy& <= turrange& THEN target& = target& + 1
RANDOMIZE TIMER
lockodd& = INT(RND * turrshot&) + 1
IF lockodd& = 1 THEN
        IF target& = 2 THEN
                wlockk& = 1
                wmissound& = 1000
        END IF
END IF
RETURN

wfire:
IF wtx& = wx& AND wty& = wy& THEN GOSUB wexplode
RANDOMIZE TIMER
wi& = INT(RND * 2) + 1
IF ix& = wx& THEN GOTO whfo
IF wi& = 1 THEN
        IF wtx& < wx& THEN
                wtx& = wtx& + 1
        ELSEIF wtx& > wx& THEN
                wtx& = wtx& - 1
        END IF
ELSEIF wi& = 2 THEN
whfo:
        IF wty& < wy& THEN
                wty& = wty& + 1
        ELSEIF wty& > wy& THEN
                wty& = wty& - 1
        END IF
END IF
SOUND wmissound&, .1
RETURN

xw:
distx& = mx& - wy&
IF ix& = wx& THEN GOTO yw
IF distx& > 0 THEN wtx& = wtx& - 1
IF distx& < 0 THEN wtx& = wtx& + 1
RETURN

yw:
disty& = my& - wy&
IF wtx& = wx& AND ty& = wy& THEN GOTO wexplode
IF wtx& = p1& AND ty& = p2& THEN GOTO wexplode2
IF wty& = wy& THEN GOTO xw
IF disty& > 0 THEN wty& = wty& - 1
IF disty& < 0 THEN wty& = wty& + 1
RETURN

wexplode:
si& = 0
DO
RANDOMIZE TIMER
ifd& = INT(RND * 100) + 37
SOUND ifd&, .1
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (wx&, wy&), si&, 4
LOOP
si& = 0
DO
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (wx&, wy&), si&, 6
LOOP
wlockk& = 0
RANDOMIZE TIMER
missdamage& = INT(RND * tdmax&) + tdmin&
winghealth& = winghealth& - missdamage&
RETURN


missile2:
iy& = gy&
ix& = gx&
target& = 0
distxx& = gx& - p1&
distyy& = gy& - p2&
IF -turrange& <= distxx& AND distxx& <= turrange& THEN target& = target& + 1
IF -turrange& <= distyy& AND distyy& <= turrange& THEN target& = target& + 1
RANDOMIZE TIMER
lockodd& = INT(RND * turrshot&) + 1
IF lockodd& = 1 THEN
        IF target& = 2 THEN
                lockk2& = 1
                missound2& = 1000
        END IF
END IF
RETURN

fire2:
IF ix& = p1& AND iy& = p2& THEN GOSUB explode2
RANDOMIZE TIMER
wi& = INT(RND * 2) + 1
IF ix& = p1& THEN GOTO hfo2
IF wi& = 1 THEN
        IF ix& < p1& THEN
                ix& = ix& + 1
        ELSEIF ix& > p1& THEN
                ix& = ix& - 1
        END IF
ELSEIF wi& = 2 THEN
hfo2:
        IF iy& < p2& THEN
                iy& = iy& + 1
        ELSEIF iy& > p2& THEN
                iy& = iy& - 1
        END IF
END IF
SOUND missound2&, .1
RETURN

x2:
distx& = gx& - p1&
IF ix& = p1& THEN GOTO y2
IF distx& > 0 THEN ix& = ix& - 1
IF distx& < 0 THEN ix& = ix& + 1
RETURN

y2:
disty& = gy& - p2&
IF ix& = p1& AND iy& = p2& THEN GOTO explode2
IF ix& = wx& AND iy& = wy& THEN GOTO wexplode2
IF iy& = p2& THEN GOTO x2
IF disty& > 0 THEN iy& = iy& - 1
IF disty& < 0 THEN iy& = iy& + 1
RETURN

explode2:
si& = 0
DO
RANDOMIZE TIMER
ifd& = INT(RND * 100) + 37
SOUND ifd&, .1
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (p1&, p2&), si&, 4
LOOP
si& = 0
DO
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (p1&, p2&), si&, 6
LOOP
lockk2& = 0
RANDOMIZE TIMER
missdamage& = INT(RND * tdmax&) + tdmin&
health& = health& - missdamage&
RETURN

wmissile2:
wiy& = gy&
wix& = gx&
target& = 0
distxx& = gx& - wx&
distyy& = gy& - wy&
IF -turrange& <= distxx& AND distxx& <= turrange& THEN target& = target& + 1
IF -turrange& <= distyy& AND distyy& <= turrange& THEN target& = target& + 1
RANDOMIZE TIMER
lockodd& = INT(RND * turrshot&) + 1
IF lockodd& = 1 THEN
        IF target& = 2 THEN
                wlockk2& = 1
                wmissound2& = 1000
        END IF
END IF
RETURN

wfire2:
IF wix& = wx& AND wiy& = wy& THEN GOSUB wexplode2
RANDOMIZE TIMER
wi& = INT(RND * 2) + 1
IF wix& = wx& THEN GOTO whfo2
IF wi& = 1 THEN
        IF wix& < wx& THEN
                wix& = wix& + 1
        ELSEIF wix& > wx& THEN
                wix& = wix& - 1
        END IF
ELSEIF wi& = 2 THEN
whfo2:
        IF wiy& < wy& THEN
                wiy& = wiy& + 1
        ELSEIF wiy& > wy& THEN
                wiy& = wiy& - 1
        END IF
END IF
SOUND wmissound2&, .1
RETURN

wx2:
distx& = gx& - wx&
IF wix& = wx& THEN GOTO wy2
IF distx& > 0 THEN wix& = wix& - 1
IF distx& < 0 THEN wix& = wix& + 1
RETURN

wy2:
disty& = gy& - wy&
IF wix& = wx& AND wiy& = wy& THEN GOTO wexplode2
IF wix& = p1& AND wiy& = p2& THEN GOTO explode2
IF wiy& = wy& THEN GOTO wx2
IF disty& > 0 THEN wiy& = wiy& - 1
IF disty& < 0 THEN wiy& = wiy& + 1
RETURN

wexplode2:
si& = 0
DO
RANDOMIZE TIMER
ifd& = INT(RND * 100) + 37
SOUND ifd&, .1
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (wx&, wy&), si&, 4
LOOP
si& = 0
DO
si& = si& + 1
IF si& = 20 THEN EXIT DO
CIRCLE (wx&, wy&), si&, 6
LOOP
wlockk2& = 0
RANDOMIZE TIMER
missdamage& = INT(RND * tdmax&) + tdmin&
winghealth& = winghealth& - missdamage&
RETURN


die:
fie& = 0
RANDOMIZE TIMER
fkf& = INT(RND * 15) + 1
koy& = INT(RND * 15) + 1
IF fkf& = 6 THEN GOTO die
IF koy& = 6 THEN GOTO die
IF fkf& = kop& THEN GOTO die
lip& = 1
DO
RANDOMIZE TIMER
kop& = INT(RND * 2) + 1
sund& = INT(RND * 100) + 37
IF kop& = 1 THEN COLOR fkf&
IF kop& = 2 THEN COLOR koy&
SOUND sund&, .5
CIRCLE (p1&, p2&), fie&
fie& = fie& + 1
IF fie& = 50 THEN EXIT DO
LOOP
fie& = 0
DO
RANDOMIZE TIMER
sund& = INT(RND * 100) + 37
SOUND sund&, .5
CIRCLE (p1&, p2&), fie&, 6
fie& = fie& + 1
IF fie& = 50 THEN EXIT DO
LOOP
LOCATE 12, 30
COLOR 5
PRINT "YOU HAVE BEEN DESTROYED"
LOCATE 13, 37
COLOR 2
PRINT "GAME OVER"
SLEEP
GOTO ultimatesupremebeginning

mineexp:
fie& = 0
RANDOMIZE TIMER
fkf& = INT(RND * 15) + 1
koy& = INT(RND * 15) + 1
IF fkf& = 6 THEN GOTO mineexp
IF koy& = 6 THEN GOTO mineexp
IF fkf& = koy& THEN GOTO mineexp
lip& = 1
DO
RANDOMIZE TIMER
kop& = INT(RND * 2) + 1
sund& = INT(RND * 200) + 37
IF kop& = 1 THEN COLOR fkf&
IF kop& = 2 THEN COLOR koy&
SOUND sund&, .1
CIRCLE (p1&, p2&), fie&
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
fie& = 0
DO
RANDOMIZE TIMER
sund& = INT(RND * 100) + 37
SOUND sund&, .5
CIRCLE (p1&, p2&), fie&, 6
CIRCLE (p1& + 1, p2&), fie&, 6
CIRCLE (p1&, p2& + 1), fie&, 6
CIRCLE (p1& + 1, p2& + 1), fie&, 6
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
RANDOMIZE TIMER
minedamage& = INT(RND * 5) + 1
IF minedamage& = 1 THEN
        minedamage& = 2
ELSE minedamage& = 10
END IF
health& = health& - minedamage&
RETURN

help:
CLS
COLOR 15
LOCATE 1, 1
PRINT "W"
SOUND 500, 1
LOCATE 1, 2
PRINT "E"
SOUND 600, 1
LOCATE 1, 3
PRINT "L"
SOUND 600, 1
LOCATE 1, 4
PRINT "C"
SOUND 500, 1
LOCATE 1, 5
PRINT "O"
SOUND 600, 1
LOCATE 1, 6
PRINT "M"
SOUND 500, 1
LOCATE 1, 7
PRINT "E"
SOUND 600, 1
LOCATE 1, 8
PRINT " "
SOUND 500, 1
LOCATE 1, 9
PRINT "T"
SOUND 600, 1
LOCATE 1, 10
PRINT "O"
SOUND 500, 1
LOCATE 1, 11
PRINT " "
SOUND 600, 1
LOCATE 1, 12
PRINT "T"
SOUND 500, 1
LOCATE 1, 13
PRINT "I"
SOUND 600, 1
LOCATE 1, 14
PRINT "B"
SOUND 500, 1
LOCATE 1, 15
PRINT "E"
SOUND 600, 1
LOCATE 1, 16
PRINT "R"
SOUND 500, 1
LOCATE 1, 17
PRINT "I"
SOUND 600, 1
LOCATE 1, 18
PRINT "U"
SOUND 500, 1
LOCATE 1, 19
PRINT "M"
SOUND 600, 1
LOCATE 1, 20
PRINT " "
SOUND 500, 1
LOCATE 1, 21
PRINT "V"
SOUND 600, 1
LOCATE 1, 22
PRINT "I"
SOUND 500, 1
LOCATE 1, 23
PRINT "N"
SOUND 600, 1
LOCATE 1, 24
PRINT "E"
SOUND 500, 1
LOCATE 1, 25
PRINT " "
SOUND 600, 1
LOCATE 1, 26
PRINT "H"
SOUND 500, 1
LOCATE 1, 27
PRINT "E"
SOUND 600, 1
LOCATE 1, 28
PRINT "L"
SOUND 500, 1
LOCATE 1, 29
PRINT "P"
SOUND 600, 1
SOUND 100, 18
qqq& = 1000
DO
SOUND qqq&, .1
qqq& = qqq& - 10
IF qqq& = 40 THEN EXIT DO
LOOP
LOCATE 3, 1
PRINT "Select Help Category."
LOCATE 5, 1
COLOR 6
PRINT "I"
LOCATE 5, 2
COLOR 14
PRINT "ntroduction"
LOCATE 1, 35
COLOR 3
PRINT "Press ESC to exit help"
DO
help$ = INKEY$
IF help$ = "i" THEN
        CLS
        LOCATE 1, 1
        COLOR 2
        INPUT "Overview of TIBERIUM VINE", hlp$
        IF hlp$ = "" THEN GOTO help
END IF
IF help$ = CHR$(27) THEN
        CLS
        RETURN
END IF
LOOP
IF notstarted& = 1 THEN
        CLS
        LINE (0, 0)-(639, 479), 5, B
        LINE (10, 10)-(629, 469), 5, B
        PAINT (1, 1), 5, 5
        LOCATE 2, 3
        COLOR 7
        PRINT "TIBERIUM VINE"
        LOCATE 3, 3
        COLOR 14
        PRINT "BY GREG DALE"
        LOCATE 26, 58
        COLOR 15
        PRINT "PRESS 'n' to begin"
        LOCATE 27, 58
        COLOR 13
        PRINT "PRESS 'q' to quit"
        LOCATE 28, 58
        COLOR 12
        PRINT "PRESS 'h' for help"
ELSEIF notstarted& = 0 THEN
        LINE (0, 0)-(640, 480), 6, BF
END IF
RETURN

c1die:
FOR expsize& = 1 TO 5
RANDOMIZE TIMER
exp1& = INT(RND * 16) + 1
sou& = INT(RND * 50) + 37
CIRCLE (enemy1&, enemy2&), expsize&, exp1&
SOUND sou&, .1
NEXT expsize&
FOR expsize& = 1 TO 5
RANDOMIZE TIMER
sou& = INT(RND * 50) + 37
CIRCLE (enemy1&, enemy2&), expsize&, 6
CIRCLE (enemy1&, enemy2& + 1), expsize&, 6
CIRCLE (enemy1& + 1, enemy2&), expsize&, 6
CIRCLE (enemy1& + 1, enemy2& + 1), expsize&, 6
SOUND sou&, .1
NEXT expsize&
c1health& = c1hmax&
enemy1& = compbasex& + 12
enemy2& = compbasey& + 12
RANDOMIZE TIMER
c1targ& = INT(RND * 2) + 1
IF c1targ& = 1 THEN c1targx& = p1& AND c1targx& = p2&
IF c1targ& = 2 THEN c1targx& = wx& AND c1targy& = wy&
kills& = kills& + 1
bank& = bank& + 60
RETURN

c2die:
FOR expsize& = 1 TO 5
RANDOMIZE TIMER
exp1& = INT(RND * 16) + 1
sou& = INT(RND * 50) + 37
CIRCLE (enemy3&, enemy4&), expsize&, exp1&
CIRCLE (enemy3&, enemy4& + 1), expsize&, 6
CIRCLE (enemy3& + 1, enemy4&), expsize&, 6
CIRCLE (enemy3& + 1, enemy4& + 1), expsize&, 6
SOUND sou&, .1
NEXT expsize&
FOR expsize& = 1 TO 5
RANDOMIZE TIMER
sou& = INT(RND * 50) + 37
CIRCLE (enemy3&, enemy4&), expsize&, 6
SOUND sou&, .1
NEXT expsize&
c2health& = c2hmax&
enemy3& = compbasex& + 12
enemy4& = compbasey& + 12
RANDOMIZE TIMER
c2targ& = INT(RND * 2) + 1
IF c2targ& = 1 THEN c2targx& = p1& AND c2targx& = p2&
IF c2targ& = 2 THEN c2targx& = wx& AND c2targy& = wy&
kills& = kills& + 1
bank& = bank& + 60
RETURN

c3die:
FOR expsize& = 1 TO 5
RANDOMIZE TIMER
exp1& = INT(RND * 16) + 1
sou& = INT(RND * 50) + 37
CIRCLE (enemy5&, enemy6&), expsize&, exp1&
SOUND sou&, .1
NEXT expsize&
FOR expsize& = 1 TO 5
RANDOMIZE TIMER
sou& = INT(RND * 50) + 37
CIRCLE (enemy5&, enemy6&), expsize&, 6
CIRCLE (enemy5&, enemy6& + 1), expsize&, 6
CIRCLE (enemy5& + 1, enemy6&), expsize&, 6
CIRCLE (enemy5& + 1, enemy6& + 1), expsize&, 6
SOUND sou&, .1
NEXT expsize&
c3health& = c3hmax&
enemy5& = compbasex& + 12
enemy6& = compbasey& + 12
RANDOMIZE TIMER
c3targ& = INT(RND * 2) + 1
IF c3targ& = 1 THEN c3targx& = p1& AND c3targx& = p2&
IF c3targ& = 2 THEN c3targx& = wx& AND c3targy& = wy&
kills& = kills& + 1
bank& = bank& + 60
RETURN

wingdie:
wingpossible& = 0
IF wingrepair& = 1 THEN RETURN
FOR esize& = 1 TO 10
RANDOMIZE TIMER
sou& = INT(RND * 400) + 37
col& = INT(RND * 16)
CIRCLE (wx&, wy&), esize&, col&
SOUND sou&, .1
NEXT esize&
FOR esize& = 1 TO 10
CIRCLE (wx&, wy&), esize&, 6
CIRCLE (wx& + 1, wy&), esize&, 6
CIRCLE (wx&, wy& + 1), esize&, 6
CIRCLE (wx& + 1, wy& + 1), esize&, 6
NEXT esize&
wx& = basex& + 5
wy& = basey& + 5
RETURN

wrskip:
IF bank& > 0 AND ((wx& > basex& AND wx& < (basex& + 10)) AND (wy& > basey& AND wy& < (basey& + 10))) THEN
        DO
        IF winghealth& = winghmax& OR bank& < 1 THEN EXIT DO
        bank& = bank& - 2
        winghealth& = winghealth& + 1
        IF bank& < 0 THEN bank& = 0
        LOOP
        wingpossible& = 1
        IF winghealth& > 0 THEN wingrepair& = 0
        RETURN
ELSEIF wingpossible& = 0 THEN wingrepair& = 1
END IF
RETURN

wprobe:
probe& = POINT(wx& - 2, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 1, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 2, wy& - 1)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 2, wy&)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 2, wy& + 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 1, wy& + 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx&, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx&, wy& + 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 1, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 1, wy& + 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 2, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 2, wy& - 1)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 2, wy&)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 2, wy& + 1)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& + 2, wy& + 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 2, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
probe& = POINT(wx& - 2, wy& - 2)
IF probe& = 2 THEN GOSUB wmineexp
RETURN

wmineexp:
fie& = 0
RANDOMIZE TIMER
fkf& = INT(RND * 15) + 1
koy& = INT(RND * 15) + 1
IF fkf& = 6 THEN GOTO wmineexp
IF koy& = 6 THEN GOTO wmineexp
IF fkf& = koy& THEN GOTO wmineexp
lip& = 1
DO
RANDOMIZE TIMER
kop& = INT(RND * 2) + 1
sund& = INT(RND * 200) + 37
IF kop& = 1 THEN COLOR fkf&
IF kop& = 2 THEN COLOR koy&
SOUND sund&, .1
CIRCLE (wx&, wy&), fie&
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
fie& = 0
DO
RANDOMIZE TIMER
sund& = INT(RND * 100) + 37
SOUND sund&, .5
CIRCLE (wx&, wy&), fie&, 6
CIRCLE (wx& + 1, wy&), fie&, 6
CIRCLE (wx&, wy& + 1), fie&, 6
CIRCLE (wx& + 1, wy& + 1), fie&, 6
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
RANDOMIZE TIMER
minedamage& = INT(RND * 5) + 1
IF minedamage& = 1 THEN
        minedamage& = 2
ELSE minedamage& = 10
END IF
winghealth& = winghealth& - minedamage&
RETURN

c1probe:
probe& = POINT(enemy1& - 2, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 1, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 2, enemy2& - 1)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 2, enemy2&)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 2, enemy2& + 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 1, enemy2& + 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1&, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1&, enemy2& + 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 1, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 1, enemy2& + 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 2, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 2, enemy2& - 1)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 2, enemy2&)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 2, enemy2& + 1)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& + 2, enemy2& + 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 2, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
probe& = POINT(enemy1& - 2, enemy2& - 2)
IF probe& = 2 THEN GOSUB c1mineexp
RETURN

c1mineexp:
fie& = 0
RANDOMIZE TIMER
fkf& = INT(RND * 15) + 1
koy& = INT(RND * 15) + 1
IF fkf& = 6 THEN GOTO c1mineexp
IF koy& = 6 THEN GOTO c1mineexp
IF fkf& = koy& THEN GOTO c1mineexp
lip& = 1
DO
RANDOMIZE TIMER
kop& = INT(RND * 2) + 1
sund& = INT(RND * 200) + 37
IF kop& = 1 THEN COLOR fkf&
IF kop& = 2 THEN COLOR koy&
SOUND sund&, .1
CIRCLE (enemy1&, enemy2&), fie&
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
fie& = 0
DO
RANDOMIZE TIMER
sund& = INT(RND * 100) + 37
SOUND sund&, .5
CIRCLE (enemy1&, enemy2&), fie&, 6
CIRCLE (enemy1& + 1, enemy2&), fie&, 6
CIRCLE (enemy1&, enemy2& + 1), fie&, 6
CIRCLE (enemy1& + 1, enemy2& + 1), fie&, 6
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
RANDOMIZE TIMER
minedamage& = INT(RND * 5) + 1
IF minedamage& = 1 THEN
        minedamage& = 2
ELSE minedamage& = 10
END IF
c1health& = c1health& - minedamage&
RETURN

c2probe:
probe& = POINT(enemy3& - 2, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 1, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 2, enemy4& - 1)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 2, enemy4&)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 2, enemy4& + 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 1, enemy4& + 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3&, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3&, enemy4& + 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 1, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 1, enemy4& + 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 2, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 2, enemy4& - 1)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 2, enemy4&)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 2, enemy4& + 1)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& + 2, enemy4& + 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 2, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
probe& = POINT(enemy3& - 2, enemy4& - 2)
IF probe& = 2 THEN GOSUB c2mineexp
RETURN

c2mineexp:
fie& = 0
RANDOMIZE TIMER
fkf& = INT(RND * 15) + 1
koy& = INT(RND * 15) + 1
IF fkf& = 6 THEN GOTO c2mineexp
IF koy& = 6 THEN GOTO c2mineexp
IF fkf& = koy& THEN GOTO c2mineexp
lip& = 1
DO
RANDOMIZE TIMER
kop& = INT(RND * 2) + 1
sund& = INT(RND * 200) + 37
IF kop& = 1 THEN COLOR fkf&
IF kop& = 2 THEN COLOR koy&
SOUND sund&, .1
CIRCLE (enemy3&, enemy4&), fie&
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
fie& = 0
DO
RANDOMIZE TIMER
sund& = INT(RND * 100) + 37
SOUND sund&, .5
CIRCLE (enemy3&, enemy4&), fie&, 6
CIRCLE (enemy3& + 1, enemy4&), fie&, 6
CIRCLE (enemy3&, enemy4& + 1), fie&, 6
CIRCLE (enemy3& + 1, enemy4& + 1), fie&, 6
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
RANDOMIZE TIMER
minedamage& = INT(RND * 5) + 1
IF minedamage& = 1 THEN
        minedamage& = 2
ELSE minedamage& = 10
END IF
c2health& = c2health& - minedamage&
RETURN

c3probe:
probe& = POINT(enemy5& - 2, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 1, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 2, enemy6& - 1)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 2, enemy6&)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 2, enemy6& + 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 1, enemy6& + 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5&, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5&, enemy6& + 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 1, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 1, enemy6& + 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 2, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 2, enemy6& - 1)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 2, enemy6&)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 2, enemy6& + 1)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& + 2, enemy6& + 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 2, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
probe& = POINT(enemy5& - 2, enemy6& - 2)
IF probe& = 2 THEN GOSUB c3mineexp
RETURN

c3mineexp:
fie& = 0
RANDOMIZE TIMER
fkf& = INT(RND * 15) + 1
koy& = INT(RND * 15) + 1
IF fkf& = 6 THEN GOTO c3mineexp
IF koy& = 6 THEN GOTO c3mineexp
IF fkf& = koy& THEN GOTO c3mineexp
lip& = 1
DO
RANDOMIZE TIMER
kop& = INT(RND * 2) + 1
sund& = INT(RND * 200) + 37
IF kop& = 1 THEN COLOR fkf&
IF kop& = 2 THEN COLOR koy&
SOUND sund&, .1
CIRCLE (enemy5&, enemy6&), fie&
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
fie& = 0
DO
RANDOMIZE TIMER
sund& = INT(RND * 100) + 37
SOUND sund&, .5
CIRCLE (enemy5&, enemy6&), fie&, 6
CIRCLE (enemy5& + 1, enemy6&), fie&, 6
CIRCLE (enemy5&, enemy6& + 1), fie&, 6
CIRCLE (enemy5& + 1, enemy6& + 1), fie&, 6
fie& = fie& + 1
IF fie& = 10 THEN EXIT DO
LOOP
RANDOMIZE TIMER
minedamage& = INT(RND * 5) + 1
IF minedamage& = 1 THEN
        minedamage& = 2
ELSE minedamage& = 10
END IF
c3health& = c3health& - minedamage&
RETURN

intro:
SOUND 100, 18
LINE (0, 0)-(639, 479), 5, B
LINE (10, 10)-(629, 469), 5, B
PAINT (1, 1), 5, 5
SOUND 500, 1
LOCATE 2, 3
COLOR 7
PRINT "TIBERIUM VINE"
SOUND 600, 1
LOCATE 3, 3
COLOR 14
PRINT "BY GREG DALE"
SOUND 500, 1
LOCATE 25, 55
COLOR 15
PRINT "PRESS '1' for 1 player"
SOUND 600, 1
LOCATE 26, 55
COLOR 5
PRINT "PRESS '2' for 2 players"
SOUND 500, 1
LOCATE 27, 55
COLOR 13
PRINT "PRESS 'q' to quit"
SOUND 600, 1
LOCATE 28, 55
COLOR 12
PRINT "PRESS 'h' for help"
a& = 320
B& = 240
DO
LINE (0, 0)-(639, 479), 5, B
LINE (10, 10)-(629, 469), 5, B
PAINT (1, 1), 5, 5
LOCATE 2, 3
COLOR 7
PRINT "TIBERIUM VINE"
LOCATE 3, 3
COLOR 14
PRINT "BY GREG DALE"
LOCATE 25, 55
COLOR 15
PRINT "PRESS '1' for 1 player"
LOCATE 26, 55
COLOR 5
PRINT "PRESS '2' for 2 players"
LOCATE 27, 55
COLOR 13
PRINT "PRESS 'q' to quit"
LOCATE 28, 55
COLOR 12
PRINT "PRESS 'h' for help"

RANDOMIZE TIMER
tiberium1& = INT(RND * 50) + 1
FOR tiberium2 = 1 TO tiberium1&
d& = INT(RND * 8) + 1
sfixer:
IF d& = 1 THEN
        a& = a& + 1
        B& = B& + 1
ELSEIF d& = 2 THEN
        a& = a& - 1
        B& = B& - 1
ELSEIF d& = 3 THEN
        a& = a& - 1
        B& = B& + 1
ELSEIF d& = 4 THEN
        a& = a& + 1
        B& = B& - 1
ELSEIF d& = 5 THEN
        a& = a& + 1
ELSEIF d& = 6 THEN
        a& = a& - 1
ELSEIF d& = 7 THEN
        B& = B& + 1
ELSEIF d& = 8 THEN
        B& = B& - 1
END IF
IF a& <= 0 THEN
        d& = 5
        GOTO sfixer
ELSEIF a& >= 640 THEN
        d& = 6
        GOTO sfixer
ELSEIF B& <= 0 THEN
        d& = 7
        GOTO sfixer
ELSEIF B& >= 480 THEN
        d& = 8
        GOTO sfixer
END IF
PSET (a&, B&), 2
NEXT tiberium2

action$ = INKEY$
IF action$ = "1" THEN GOTO actualstart
IF action$ = "2" THEN GOTO twoplayers
IF action$ = "q" THEN END
IF action$ = "h" THEN
        notstarted& = 1
        GOSUB help
END IF
LOOP
RETURN

pause:
LOCATE 2, 28
COLOR 1
PRINT "   PRESS ANY KEY  "
SLEEP
LOCATE 2, 28
COLOR 15
PRINT "PRESS 'P' TO PAUSE"
RETURN







twoplayers:
CLS
SCREEN 12
'LINE (0, 0)-(640, 50), 15, BF
RANDOMIZE TIMER
x1& = INT(RND * 639) + 1
y1& = INT(RND * 429) + 50
x2& = INT(RND * 639) + 1
y2& = INT(RND * 429) + 50
tx& = INT(RND * 639) + 1
ty& = INT(RND * 429) + 50
move1& = 0
move2& = 0
laser1& = 0
laser2& = 0
miss1& = 0
miss2& = 0
health1& = 100
health2& = 100
laserlock1& = 0
laserlock2& = 0
laserrange1& = 150
laserrange2& = 150
misslaunch1& = 0
misslaunch2& = 0
missrange1& = 100
missrange2& = 100
spider1& = 1
spider2& = 1
spirange& = 50

LOCATE 1, 2
COLOR 13
PRINT "Laser:"
LOCATE 2, 2
COLOR 13
PRINT "Missile:"
LOCATE 3, 2
COLOR 13
PRINT "Damage:"
LOCATE 1, 30
COLOR 2
PRINT "Laser:"
LOCATE 2, 30
COLOR 2
PRINT "Missile:"
LOCATE 3, 30
COLOR 2
PRINT "Damage:"
LOCATE 1, 23
PRINT "    "
LOCATE 2, 11
PRINT "     "
LOCATE 2, 39
PRINT "     "
LOCATE 1, 52
PRINT "    "

DO
VIEW
LINE (70, 8)-((laser1& / 10) + 70, 12), 2, BF
LINE (170, 8)-((laser1& / 10) + 70, 12), 4, BF
IF laser1& > 1000 THEN laser1& = 1000
LINE (300, 8)-((laser2& / 10) + 300, 12), 2, BF
LINE (400, 8)-((laser2& / 10) + 300, 12), 4, BF
IF laser2& > 1000 THEN laser2& = 1000
laserlock1& = 0
IF (x1& - x2& > -laserrange1& AND x1& - x2& < laserrange1&) AND (y1& - y2& > -laserrange1& AND y1& - y2& < laserrange1&) THEN laserlock1& = 1
IF laserlock1& = 1 THEN
        LOCATE 1, 23
        COLOR 13
        PRINT "Lock"
ELSEIF laserlock1& = 0 THEN
        LOCATE 1, 23
        PRINT "    "
END IF

laserlock2& = 0
IF (x1& - x2& > -laserrange2& AND x1& - x2& < laserrange2&) AND (y1& - y2& > -laserrange2& AND y1& - y2& < laserrange2&) THEN laserlock2& = 1
IF laserlock2& = 1 THEN
        LOCATE 1, 52
        COLOR 2
        PRINT "Lock"
ELSEIF laserlock2& = 0 THEN
        LOCATE 1, 52
        PRINT "    "
END IF

misslock1& = 0
IF (x1& - x2& > -missrange1& AND x1& - x2& < missrange1&) AND (y1& - y2& > -missrange1& AND y1& - y2& < missrange1&) THEN misslock1& = 1
IF misslock1& = 1 THEN
        LOCATE 2, 20
        COLOR 2
        PRINT "Lock"
ELSE
        LOCATE 2, 20
        PRINT "    "
END IF

misslock2& = 0
IF (x1& - x2& > -missrange2& AND x1& - x2& < missrange2&) AND (y1& - y2& > -missrange2& AND y1& - y2& < missrange2&) THEN misslock2& = 1
IF misslock2& = 1 THEN
        LOCATE 2, 50
        COLOR 2
        PRINT "Lock"
ELSE
        LOCATE 2, 50
        PRINT "    "
END IF

LINE (70, 36)-(health1& + 70, 40), 2, BF
LINE (170, 36)-(health1& + 70, 40), 4, BF
LINE (300, 36)-(health2& + 300, 40), 2, BF
LINE (400, 36)-(health2& + 300, 40), 4, BF
miss1& = miss1& + 1
IF miss1& > 500 THEN miss1& = 500
IF miss1& = 500 THEN missarm1& = 1
IF missarm1& = 1 THEN
        LOCATE 2, 11
        COLOR 13
        PRINT "ARMED"
ELSE
        LOCATE 2, 11
        PRINT "     "
END IF

miss2& = miss2& + 1
IF miss2& > 500 THEN miss2& = 500
IF miss2& = 500 THEN missarm2& = 1
IF missarm2& = 1 THEN
        LOCATE 2, 39
        COLOR 2
        PRINT "ARMED"
ELSE
        LOCATE 2, 39
        PRINT "     "
END IF
LINE (1, 50)-(639, 479), 15, B
VIEW SCREEN (2, 51)-(638, 478)

LINE (x1& - 1, y1& - 1)-(x1& + 1, y1& + 1), 13, BF
LINE (x1& - 2, y1& - 2)-(x1& + 2, y1& + 2), 0, B
LINE (x2& - 1, y2& - 1)-(x2& + 1, y2& + 1), 2, BF
LINE (x2& - 2, y2& - 2)-(x2& + 2, y2& + 2), 0, B

IF x1& > 637 THEN x1& = 637
IF x1& < 3 THEN x1& = 3
IF y1& > 477 THEN y1& = 477
IF y1& < 52 THEN y1& = 52
IF x2& > 637 THEN x2& = 637
IF x2& < 3 THEN x2& = 3
IF y2& > 477 THEN y2& = 477
IF y2& < 52 THEN y2& = 52
IF tx& > 637 THEN tx& = 637
IF tx& < 3 THEN tx& = 3
IF ty& > 477 THEN ty& = 477
IF ty& < 52 THEN ty& = 52
IF health1& < 0 THEN health1& = 0
IF health2& < 0 THEN health2& = 0
IF health1& = 0 THEN GOSUB die1
IF health2& = 0 THEN GOSUB die2
IF spider1& > 3 THEN spider1& = 3
IF spider2& > 3 THEN spider2& = 3

drive$ = INKEY$
IF drive$ = "1" THEN
        move1& = 1
        dir1& = 1
ELSEIF drive$ = "2" THEN
        move1& = 1
        dir1& = 2
ELSEIF drive$ = "3" THEN
        move1& = 1
        dir1& = 3
ELSEIF drive$ = "4" THEN
        move1& = 1
        dir1& = 4
ELSEIF drive$ = "6" THEN
        move1& = 1
        dir1& = 6
ELSEIF drive$ = "7" THEN
        move1& = 1
        dir1& = 7
ELSEIF drive$ = "8" THEN
        move1& = 1
        dir1& = 8
ELSEIF drive$ = "9" THEN
        move1& = 1
        dir1& = 9
ELSEIF drive$ = "5" THEN
        move1& = 0
ELSEIF drive$ = "z" THEN
        move2& = 1
        dir2& = 1
ELSEIF drive$ = "x" THEN
        move2& = 1
        dir2& = 2
ELSEIF drive$ = "c" THEN
        move2& = 1
        dir2& = 3
ELSEIF drive$ = "a" THEN
        move2& = 1
        dir2& = 4
ELSEIF drive$ = "d" THEN
        move2& = 1
        dir2& = 6
ELSEIF drive$ = "q" THEN
        move2& = 1
        dir2& = 7
ELSEIF drive$ = "w" THEN
        move2& = 1
        dir2& = 8
ELSEIF drive$ = "e" THEN
        move2& = 1
        dir2& = 9
ELSEIF drive$ = "s" THEN
        move2& = 0
ELSEIF drive$ = "/" THEN
        GOSUB lasfire1
ELSEIF drive$ = " " THEN
        GOSUB lasfire2
ELSEIF drive$ = "`" THEN
        GOTO ultimatesupremebeginning
ELSEIF drive$ = "*" THEN
        GOSUB missilea
ELSEIF drive$ = "v" THEN
        GOSUB missileb
ELSEIF drive$ = "-" THEN
        GOSUB spider1
ELSEIF drive$ = "f" THEN
        GOSUB spider2
END IF

IF move1& = 1 THEN
        IF dir1& = 1 THEN
                x1& = x1& - 1
                y1& = y1& + 1
        ELSEIF dir1& = 2 THEN
                y1& = y1& + 1
        ELSEIF dir1& = 3 THEN
                x1& = x1& + 1
                y1& = y1& + 1
        ELSEIF dir1& = 4 THEN
                x1& = x1& - 1
        ELSEIF dir1& = 6 THEN
                x1& = x1& + 1
        ELSEIF dir1& = 7 THEN
                x1& = x1& - 1
                y1& = y1& - 1
        ELSEIF dir1& = 8 THEN
                y1& = y1& - 1
        ELSEIF dir1& = 9 THEN
                x1& = x1& + 1
                y1& = y1& - 1
        END IF
END IF

playercheck& = 1
xx& = x1&
yy& = y1&
dirdir& = dir1&
GOSUB pprobe

IF move2& = 1 THEN
        IF dir2& = 1 THEN
                x2& = x2& - 1
                y2& = y2& + 1
        ELSEIF dir2& = 2 THEN
                y2& = y2& + 1
        ELSEIF dir2& = 3 THEN
                x2& = x2& + 1
                y2& = y2& + 1
        ELSEIF dir2& = 4 THEN
                x2& = x2& - 1
        ELSEIF dir2& = 6 THEN
                x2& = x2& + 1
        ELSEIF dir2& = 7 THEN
                x2& = x2& - 1
                y2& = y2& - 1
        ELSEIF dir2& = 8 THEN
                y2& = y2& - 1
        ELSEIF dir2& = 9 THEN
                x2& = x2& + 1
                y2& = y2& - 1
        END IF
END IF

playercheck& = 2
xx& = x2&
yy& = y2&
dirdir& = dir2&
GOSUB pprobe

RANDOMIZE TIMER
tdir& = INT(RND * 8) + 1
IF tdir& = 1 THEN
        tx& = tx& + 1
ELSEIF tdir& = 2 THEN
        tx& = tx& - 1
ELSEIF tdir& = 3 THEN
        ty& = ty& + 1
ELSEIF tdir& = 4 THEN
        ty& = ty& - 1
ELSEIF tdir& = 5 THEN
        ty& = ty& + 1
        tx& = tx& + 1
ELSEIF tdir& = 6 THEN
        ty& = ty& - 1
        tx& = tx& - 1
ELSEIF tdir& = 7 THEN
        tx& = tx& + 1
        ty& = ty& - 1
ELSEIF tdir& = 8 THEN
        tx& = tx& - 1
        ty& = ty& + 1
END IF
PSET (tx&, ty&), 4

RANDOMIZE TIMER
circ& = INT(RND * 500) + 1
IF circ& = 1 THEN
        RANDOMIZE TIMER
        circsize& = INT(RND * 20) + 1
        circx& = INT(RND * 640) + 1
        circy& = INT(RND * 430) + 50
        circsize2& = 0
        FOR fig = 1 TO circsize&
        circsize2& = circsize2& + 1
        CIRCLE (circx&, circy&), circsize2&, 4
        NEXT fig
END IF

FOR i = 1 TO 2
IF misslaunch1& = 1 THEN
        CIRCLE (missx1&, missy1&), 8, 13
        CIRCLE (missx1&, missy1&), 7, 0
        CIRCLE (missx1&, missy1&), 9, 0
        LINE (missx1& - 10, missy1& - 10)-(missx1& + 10, missy1& + 10), 0, B
        PSET (missx1&, missy1&), 13
        RANDOMIZE TIMER
        missdir& = INT(RND * 2) + 1
bee:
        IF missdir& = 1 THEN
                IF missx1& < x2& THEN missx1& = missx1& + 1
                IF missx1& > x2& THEN missx1& = missx1& - 1
                IF missx1& = x2& THEN missdir& = 2
        END IF
        IF missdir& = 2 THEN
                IF missy1& < y2& THEN missy1& = missy1& + 1
                IF missy1& > y2& THEN missy1& = missy1& - 1
                IF missy1& = y2& AND missx1& = x2& THEN GOTO missexp1
                IF missy1& = y2& THEN
                        missdir& = 1
                        GOTO bee
                END IF
        END IF
END IF

missdone1:

IF misslaunch2& = 1 THEN
        CIRCLE (missx2&, missy2&), 8, 2
        CIRCLE (missx2&, missy2&), 7, 0
        CIRCLE (missx2&, missy2&), 9, 0
        LINE (missx2& - 10, missy2& - 10)-(missx2& + 10, missy2& + 10), 0, B
        PSET (missx2&, missy2&), 2
        RANDOMIZE TIMER
        missdir& = INT(RND * 2) + 1
bee2:
        IF missdir& = 1 THEN
                IF missx2& < x1& THEN missx2& = missx2& + 1
                IF missx2& > x1& THEN missx2& = missx2& - 1
                IF missx2& = x1& THEN missdir& = 2
        END IF
        IF missdir& = 2 THEN
                IF missy2& < y1& THEN missy2& = missy2& + 1
                IF missy2& > y1& THEN missy2& = missy2& - 1
                IF missy2& = y1& AND missx2& = x1& THEN GOTO missexp2
                IF missy2& = y1& THEN
                        missdir& = 1
                        GOTO bee2
                END IF
        END IF
END IF
missdone2:
NEXT i

RANDOMIZE TIMER
hprob& = INT(RND * 500) + 1
IF hprob& = 1 THEN
        RANDOMIZE TIMER
        hx& = INT(RND * 639) + 1
        hy& = INT(RND * 429) + 50
        PSET (hx&, hy&), 14
END IF

RANDOMIZE TIMER
mprob& = INT(RND * 500) + 1
IF mprob& = 1 THEN
        RANDOMIZE TIMER
        mx& = INT(RND * 639) + 1
        my& = INT(RND * 429) + 50
        PSET (mx&, my&), 11
END IF

LOCATE 3, 25
COLOR 2
PRINT spider1&
LOCATE 3, 54
COLOR 13
PRINT spider2&

IF spicount1& > -1 THEN
        spicount1& = spicount1& - 1
END IF

IF spicount1& > 0 THEN PSET (spix1&, spiy1&), 15
IF spicount1& = 0 THEN
        spisunk1& = 1
        PSET (spix1&, spiy1&), 0
END IF

spilock1& = 0
IF (spix1& - x2& > -spirange& AND spix1& - x2& < spirange&) AND (spiy1& - y2& > -spirange& AND spiy1& - y2& < spirange&) AND spisunk1& = 1 THEN spilock1& = 1
IF spilock1& = 1 THEN
        FOR spispeed = 1 TO 2
        IF spix1& < x2& THEN spix1& = spix1& + 1
        IF spix1& > x2& THEN spix1& = spix1& - 1
        IF spiy1& < y2& THEN spiy1& = spiy1& + 1
        IF spiy1& > y2& THEN spiy1& = spiy1& - 1
        IF spix1& = x2& AND spiy1& = y2& THEN
                spisunk1& = 0
                GOSUB spiexp1
        END IF
        LINE (spix1& - 2, spiy1& - 2)-(spix1& + 2, spiy1& + 2), 0, B
        CIRCLE (spix1&, spiy1&), 1, 15
        NEXT spispeed
END IF

IF spicount2& > -1 THEN
        spicount2& = spicount2& - 1
END IF

IF spicount2& > 0 THEN PSET (spix2&, spiy2&), 15
IF spicount2& = 0 THEN
        spisunk2& = 1
        PSET (spix2&, spiy2&), 0
END IF

spilock2& = 0
IF (spix2& - x1& > -spirange& AND spix2& - x1& < spirange&) AND (spiy2& - y1& > -spirange& AND spiy2& - y1& < spirange&) AND spisunk2& = 1 THEN spilock2& = 1
IF spilock2& = 1 THEN
        FOR spispeed = 1 TO 2
        IF spix2& < x1& THEN spix2& = spix2& + 1
        IF spix2& > x1& THEN spix2& = spix2& - 1
        IF spiy2& < y1& THEN spiy2& = spiy2& + 1
        IF spiy2& > y1& THEN spiy2& = spiy2& - 1
        IF spix2& = x1& AND spiy2& = y1& THEN
                spisunk2& = 0
                GOSUB spiexp2
        END IF
        LINE (spix2& - 2, spiy2& - 2)-(spix2& + 2, spiy2& + 2), 0, B
        CIRCLE (spix2&, spiy2&), 1, 15
        NEXT spispeed
END IF


LOOP

pprobe:
IF dirdir& = 1 THEN
        GOSUB checkleft
        GOSUB checkdown
ELSEIF dirdir& = 2 THEN
        GOSUB checkdown
ELSEIF dirdir& = 3 THEN
        GOSUB checkdown
        GOSUB checkright
ELSEIF dirdir& = 4 THEN
        GOSUB checkleft
ELSEIF dirdir& = 6 THEN
        GOSUB checkright
ELSEIF dirdir& = 7 THEN
        GOSUB checkleft
        GOSUB checkup
ELSEIF dirdir& = 8 THEN
        GOSUB checkup
ELSEIF dirdir& = 9 THEN
        GOSUB checkup
        GOSUB checkright
END IF
RETURN

checkup:
check& = POINT(xx& - 2, yy& - 2)
GOSUB colorcheck
check& = POINT(xx& - 1, yy& - 2)
GOSUB colorcheck
check& = POINT(xx&, yy& - 2)
GOSUB colorcheck
check& = POINT(xx& + 1, yy& - 2)
GOSUB colorcheck
check& = POINT(xx& + 2, yy& - 2)
GOSUB colorcheck
RETURN

checkright:
check& = POINT(xx& + 2, yy& - 2)
GOSUB colorcheck
check& = POINT(xx& + 2, yy& - 1)
GOSUB colorcheck
check& = POINT(xx& + 2, yy&)
GOSUB colorcheck
check& = POINT(xx& + 2, yy& + 1)
GOSUB colorcheck
check& = POINT(xx& + 2, yy& + 2)
GOSUB colorcheck
RETURN

checkdown:
check& = POINT(xx& + 2, yy& + 2)
GOSUB colorcheck
check& = POINT(xx& + 1, yy& + 2)
GOSUB colorcheck
check& = POINT(xx&, yy& + 2)
GOSUB colorcheck
check& = POINT(xx& - 1, yy& + 2)
GOSUB colorcheck
check& = POINT(xx& - 2, yy& + 2)
GOSUB colorcheck
RETURN

checkleft:
check& = POINT(xx& - 2, yy& + 2)
GOSUB colorcheck
check& = POINT(xx& - 2, yy& + 1)
GOSUB colorcheck
check& = POINT(xx& - 2, yy&)
GOSUB colorcheck
check& = POINT(xx& - 2, yy& - 1)
GOSUB colorcheck
check& = POINT(xx& - 2, yy& - 2)
GOSUB colorcheck
RETURN

colorcheck:
IF playercheck& = 1 THEN
        IF check& = 4 THEN
                laser1& = laser1& + 1
        ELSEIF check& = 14 THEN
                health1& = health1& + 5
        ELSEIF check& = 11 THEN
                spider1& = spider1& + 1
        END IF
ELSEIF playercheck& = 2 THEN
        IF check& = 4 THEN
                laser2& = laser2& + 1
        ELSEIF check& = 14 THEN
                health2& = health2& + 5
        ELSEIF check& = 11 THEN
                spider2& = spider2& + 1
        END IF
END IF
RETURN

lasfire1:
lasertest& = laser1&
RANDOMIZE TIMER
hit& = INT(RND * 10) + 1
IF laserlock1& = 1 THEN
        GOTO noskip1
ELSEIF laserlock1& = 0 THEN
        IF hit& = 10 THEN GOTO noskip1 ELSE GOTO skippera
END IF
noskip1:
FOR shotgrap = 1 TO 20
RANDOMIZE TIMER
lasercol& = INT(RND * 2) + 1
IF lasercol& = 1 THEN lasercol& = 13
IF lasercol& = 2 THEN lasercol& = 5
LINE (x1&, y1&)-(x2&, y2&), lasercol&
NEXT shotgrap
LINE (x1&, y1&)-(x2&, y2&), 0
IF laser1& = 0 THEN
        RANDOMIZE TIMER
        damage& = INT(RND * 2)
        IF laserlock1& = 0 THEN damage& = 0
        health2& = health2& - damage&
END IF
health2& = health2& - (laser1& / 50)
IF health2& < 0 THEN health2& = 0
skippera:
laser1& = 0
RETURN

lasfire2:
lasertest& = laser2&
RANDOMIZE TIMER
hit& = INT(RND * 10) + 1
IF laserlock2& = 1 THEN
        IF hit& = 10 THEN GOTO skipperb ELSE GOTO noskip2
ELSEIF laserlock2& = 0 THEN
        IF hit& = 10 THEN GOTO noskip2 ELSE GOTO skipperb
END IF
noskip2:
FOR shotgrap2 = 1 TO 20
RANDOMIZE TIMER
lasercol& = INT(RND * 2) + 1
IF lasercol& = 1 THEN lasercol& = 10
IF lasercol& = 2 THEN lasercol& = 2
LINE (x1&, y1&)-(x2&, y2&), lasercol&
NEXT shotgrap2
LINE (x1&, y1&)-(x2&, y2&), 0
IF laser2& = 0 THEN
        RANDOMIZE TIMER
        damage& = INT(RND * 2)
        IF laserlock2& = 0 THEN damage& = 0
        health1& = health1& - damage&
END IF
health1& = health1& - (laser2& / 50)
IF health1& < 0 THEN health1& = 0
skipperb:
laser2& = 0
RETURN

die1:
FOR expsize& = 1 TO 35
RANDOMIZE TIMER
expcol& = INT(RND * 16)
CIRCLE (x1&, y1&), expsize&, expcol&
NEXT expsize&
FOR expsize& = 1 TO 35
CIRCLE (x1&, y1&), expsize&, 0
CIRCLE (x1& + 1, y1&), expsize&, 0
NEXT expsize&
FOR expsize& = 1 TO 35
NEXT expsize&
health1& = 100
miss1& = 0
laser1& = 0
missarm1& = 0
move1& = 0
score2& = score2& + 1
LOCATE 2, 75
COLOR 2
PRINT score2&
RANDOMIZE TIMER
x1& = INT(RND * 639) + 1
y1& = INT(RND * 429) + 50
RETURN

die2:
FOR expsize& = 1 TO 35
RANDOMIZE TIMER
expcol& = INT(RND * 16)
CIRCLE (x2&, y2&), expsize&, expcol&
NEXT expsize&
FOR expsize& = 1 TO 35
CIRCLE (x2&, y2&), expsize&, 0
CIRCLE (x2& + 1, y2&), expsize&, 0
NEXT expsize&
health2& = 100
miss2& = 0
laser2& = 0
missarm2& = 0
move2& = 0
score1& = score1& + 1
LOCATE 1, 75
COLOR 13
PRINT score1&
RANDOMIZE TIMER
x2& = INT(RND * 639) + 1
y2& = INT(RND * 429) + 50
RETURN

missilea:
IF misslaunch1& = 1 THEN RETURN
IF misslock1& = 1 AND missarm1& = 1 THEN
        miss1& = 0
        missarm1& = 0
        missx1& = x1&
        missy1& = y1&
        misslaunch1& = 1
END IF
RETURN

missileb:
IF misslaunch2& = 1 THEN RETURN
IF misslock2& = 1 AND missarm2& = 1 THEN
        miss2& = 0
        missarm2& = 0
        missx2& = x2&
        missy2& = y2&
        misslaunch2& = 1
END IF
RETURN

missexp1:
FOR size = 1 TO 20
RANDOMIZE TIMER
col& = INT(RND * 16)
CIRCLE (missx1&, missy1&), size, col&
NEXT size
FOR size = 1 TO 20
CIRCLE (missx1&, missy1&), size, 0
NEXT size
RANDOMIZE TIMER
damage& = INT(RND * 20) + 10
health2& = health2& - damage&
IF health2& < 0 THEN health2& = 0
misslaunch1& = 0
GOTO missdone1

missexp2:
FOR size = 1 TO 20
RANDOMIZE TIMER
col& = INT(RND * 16)
CIRCLE (missx2&, missy2&), size, col&
NEXT size
FOR size = 1 TO 20
CIRCLE (missx2&, missy2&), size, 0
NEXT size
RANDOMIZE TIMER
damage& = INT(RND * 20) + 10
health1& = health1& - damage&
IF health1& < 0 THEN health1& = 0
misslaunch2& = 0
GOTO missdone2

spider1:
IF spider1& > 0 THEN
        spix1& = x1&
        spiy1& = y1&
        spicount1& = 100
        spider1& = spider1& - 1
END IF
RETURN

spider2:
IF spider2& > 0 THEN
        spix2& = x2&
        spiy2& = y2&
        spicount2& = 100
        spider2& = spider2& - 1
END IF
RETURN

spiexp1:
FOR expsize = 1 TO 10
CIRCLE (spix1&, spiy1&), expsize, 2
NEXT expsize
FOR expsize = 1 TO 10
CIRCLE (spix1&, spiy1&), expsize, 0
NEXT expsize
RANDOMIZE TIMER
damage& = INT(RND * 25) + 25
health2& = health2& - damage&
IF health2& < 0 THEN health2& = 0
RETURN

spiexp2:
FOR expsize = 1 TO 10
CIRCLE (spix2&, spiy2&), expsize, 1
NEXT expsize
FOR expsize = 1 TO 10
CIRCLE (spix2&, spiy2&), expsize, 0
NEXT expsize
RANDOMIZE TIMER
damage& = INT(RND * 25) + 25
health1& = health1& - damage&
IF health1& < 0 THEN health1& = 0
RETURN

