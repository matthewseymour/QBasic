start:
CLS
SCREEN 12
range& = 190
health1% = 500
health2% = 500
life2% = 1
life1% = 1
c1x% = 40
c1y% = 175
x% = 600
y% = 175
t% = 0
t2% = 0
t3% = 0
tx% = x%
ty% = y%
tx2% = x%
ty2% = y%
tx3% = x%
ty3% = y%
LINE (20, 430)-(520, 431), 4, BF
LINE (20, 450)-(520, 451), 2, BF

        DO
        DO
        
        LINE (0, 421)-(640, 429), 0, BF
        LINE (0, 21)-(637, 420), 14, B
        LINE (1, 22)-(636, 419), 0, B
        LINE (2, 23)-(635, 418), 0, B
        LINE (3, 24)-(634, 417), 0, B
        LINE (4, 25)-(633, 419), 0, B
        LINE (5, 24)-(632, 415), 0, B
        LINE (6, 27)-(631, 414), 0, B
        LINE (7, 28)-(630, 413), 0, B
        LINE (8, 29)-(629, 412), 0, B
       
        IF life1% = 0 THEN GOTO endd
        IF life2% = 0 THEN GOTO endd
       
        GOSUB tor
        GOSUB tor2
        GOSUB tor3
        GOSUB finder
        GOSUB enmeyfinder
       
        LINE (c1x% - 1, c1y% - 1)-(c1x% + 1, c1y% + 1), 4, BF
        LINE (x% - 1, y% - 1)-(x% + 1, y% + 1), 1, BF
        LINE (c1x% - 2, c1y% - 2)-(c1x% + 2, c1y% + 2), 0, B
        LINE (x% - 2, y% - 2)-(x% + 2, y% + 2), 0, B
       
        LINE (520, 430)-(health2% + 20, 431), 0, BF
        LINE (520, 450)-(health1% + 20, 451), 0, BF
       
        IF fin% = 1 THEN
                LOCATE 1, 35
                PRINT "LOCKED ON"
        ELSEIF fin% = 0 THEN
                LINE (0, 0)-(640, 20), 0, BF
                END IF
       
        IF find1% = 1 THEN LINE (x% - 5, y% - 5)-(x% + 5, y% + 5), 4, B
        IF fin% = 1 THEN LINE (c1x% - 5, c1y% - 5)-(c1x% + 5, c1y% + 5), 1, B
        LINE (c1x% - 6, c1y% - 6)-(c1x% + 6, c1y% + 6), 0, B
        LINE (c1x% - 4, c1y% - 4)-(c1x% + 4, c1y% + 4), 0, B
        LINE (x% - 6, y% - 6)-(x% + 6, y% + 6), 0, B
        LINE (x% - 4, y% - 4)-(x% + 4, y% + 4), 0, B
        IF find1% = 0 THEN LINE (x% - 5, y% - 5)-(x% + 5, y% + 5), 0, B
        IF fin% = 0 THEN LINE (c1x% - 5, c1y% - 5)-(c1x% + 5, c1y% + 5), 0, B
      
        IF t% = 1 THEN PSET (tx%, ty%), 2
        IF t% = 1 THEN CIRCLE (tx%, ty%), 8, 2
        IF t% = 1 THEN CIRCLE (tx%, ty%), 7, 0
        IF t% = 1 THEN CIRCLE (tx%, ty%), 9, 0
        IF t% = 0 THEN CIRCLE (tx%, ty%), 8, 0
       
        IF t2% = 1 THEN PSET (tx2%, ty2%), 2
        IF t2% = 1 THEN CIRCLE (tx2%, ty2%), 8, 2
        IF t2% = 1 THEN CIRCLE (tx2%, ty2%), 7, 0
        IF t2% = 1 THEN CIRCLE (tx2%, ty2%), 9, 0
        IF t2% = 0 THEN CIRCLE (tx2%, ty2%), 8, 0
       
        IF t3% = 1 THEN PSET (tx3%, ty3%), 2
        IF t3% = 1 THEN CIRCLE (tx3%, ty3%), 8, 2
        IF t3% = 1 THEN CIRCLE (tx3%, ty3%), 7, 0
        IF t3% = 1 THEN CIRCLE (tx3%, ty3%), 9, 0
        IF t3% = 0 THEN CIRCLE (tx3%, ty3%), 8, 0

       
        
       
move:
        IF life2% = 0 THEN GOTO endd
        IF dist1% > 0 THEN c1x% = c1x% - 1
        IF dist1% < 0 THEN c1x% = c1x% + 1
        IF dist12% > 0 THEN c1y% = c1y% - 1
        IF dist12% < 0 THEN c1y% = c1y% + 1
       
        IF x% > 639 THEN x% = 3
        IF x% < 2 THEN x% = 637
        IF y% > 420 THEN y% = 23
        IF y% < 22 THEN y% = 417
       
        IF c1x% > 639 THEN c1x% = 3
        IF c1x% < 2 THEN c1x% = 637
        IF c1y% > 420 THEN c1y% = 23
        IF c1y% < 22 THEN c1y% = 417
       
        IF tx% > 639 THEN tx% = 3
        IF tx% < 2 THEN tx% = 637
        IF ty% > 420 THEN ty% = 23
        IF ty% < 22 THEN ty% = 417
               
                ans1$ = INKEY$
                LOOP UNTIL (ans1$ = "w" OR ans1$ = " " OR ans1$ = "r" OR ans1$ = "t" OR ans1$ = "y" OR ans1$ = "h" OR ans1$ = "f" OR ans1$ = "n" OR ans1$ = "b" OR ans1$ = "v" OR ans1$ = "q")
        IF ans1$ = "r" THEN
                x% = x% - 1
                y% = y% - 1
        ELSEIF ans1$ = "t" THEN
                y% = y% - 1
        ELSEIF ans1$ = "y" THEN
                y% = y% - 1
                x% = x% + 1
        ELSEIF ans1$ = "h" THEN
                x% = x% + 1
        ELSEIF ans1$ = " " THEN GOSUB fire
        ELSEIF ans1$ = "f" THEN
                x% = x% - 1
        ELSEIF ans1$ = "n" THEN
                x% = x% + 1
                y% = y% + 1
        ELSEIF ans1$ = "b" THEN
                y% = y% + 1
        ELSEIF ans1$ = "w" THEN
                LINE (x% - 5, y% - 5)-(x% + 5, y% + 5), 0, B
                LINE (x% - 1, y% - 1)-(x% + 1, y% + 1), 0, BF
                RANDOMIZE TIMER
                x% = INT(RND * 637) + 2
                y% = INT(RND * 397) + 20
                hy% = INT(RND * 10) + 1
                IF hy% = 1 THEN GOSUB youexplode
        ELSEIF ans1$ = "v" THEN
                y% = y% + 1
                x% = x% - 1
        ELSEIF ans1$ = "q" THEN END
        END IF
        LOOP
        
        

finder:
fin% = 0
IF life1% = 0 THEN RETURN
dist% = x% - c1x%
dist2% = y% - c1y%
IF dist% > 200 THEN RETURN
IF dist% < -200 THEN RETURN
IF dist2% > 200 THEN RETURN
IF dist2% < -200 THEN RETURN
fin% = 1
RETURN
fire:
IF t% = 0 THEN tx% = x%
IF t% = 0 THEN ty% = y%
IF t2% = 0 THEN tx2% = x%
IF t2% = 0 THEN ty2% = y%
IF t3% = 0 THEN tx3% = x%
IF t3% = 0 THEN ty3% = y%
IF fin% = 0 THEN RETURN
IF life1% = 0 THEN RETURN
SOUND 3000, .1
IF t2% = 1 THEN t3% = 1
IF t% = 1 THEN t2% = 1
t% = 1
RETURN


tor:
        IF t% = 0 THEN RETURN
        RANDOMIZE TIMER
        df% = INT(RND * 2) + 1
        IF df% = 1 THEN GOTO x
        IF df% = 2 THEN GOTO y
x:
        dist2% = tx% - c1x%
        IF dist2% > 0 THEN tx% = tx% - 1
        IF dist2% < 0 THEN tx% = tx% + 1
        IF dist2% > 320 THEN tx% = tx% + 1
        IF dist2% < -320 THEN tx% = tx% - 1
        IF tx% = c1x% THEN GOTO test2
        RETURN
y:
        dist22% = ty% - c1y%
        IF ty% = c1y% THEN GOTO x
        IF dist22% > 0 THEN ty% = ty% - 1
        IF dist22% < 0 THEN ty% = ty% + 1
        IF dist22% > 210 THEN ty% = ty% + 1
        IF dist22% < -210 THEN ty% = ty% - 1
        RETURN
test2:
        IF ty% = c1y% THEN GOSUB hit
        IF health2% = 0 THEN GOSUB explode
        RETURN
hit:
        health2% = health2% - 10
        t% = 0
        si% = 0
        DO
        CIRCLE (c1x%, c1y%), si%, 4
        si% = si% + 1
        IF si% = 19 THEN EXIT DO
        LOOP
        si% = 0
        DO
        CIRCLE (c1x%, c1y%), si%, 0
        si% = si% + 1
        IF si% = 19 THEN EXIT DO
        LOOP
        RETURN


tor2:
        IF t2% = 0 THEN RETURN
        RANDOMIZE TIMER
        df2% = INT(RND * 2) + 1
        IF df2% = 1 THEN GOTO x2
        IF df2% = 2 THEN GOTO y2
x2:
        dist2% = tx2% - c1x%
        IF dist2% > 0 THEN tx2% = tx2% - 1
        IF dist2% < 0 THEN tx2% = tx2% + 1
        IF dist2% > 320 THEN tx2% = tx2% + 1
        IF dist2% < -320 THEN tx2% = tx2% - 1
        IF tx2% = c1x% THEN GOTO test22
        RETURN
y2:
        dist22% = ty2% - c1y%
        IF ty2% = c1y% THEN GOTO x2
        IF dist22% > 0 THEN ty2% = ty2% - 1
        IF dist22% < 0 THEN ty2% = ty2% + 1
        IF dist22% > 210 THEN ty2% = ty2% + 1
        IF dist22% < -210 THEN ty2% = ty2% - 1
        RETURN
test22:
        IF ty2% = c1y% THEN GOSUB hit2
        IF health2% = 0 THEN GOSUB explode
        RETURN
hit2:
        health2% = health2% - 10
        t2% = 0
        si% = 0
        DO
        CIRCLE (c1x%, c1y%), si%, 4
        si% = si% + 1
        IF si% = 19 THEN EXIT DO
        LOOP
        si% = 0
        DO
        CIRCLE (c1x%, c1y%), si%, 0
        si% = si% + 1
        IF si% = 19 THEN EXIT DO
        LOOP
        RETURN


tor3:
        IF t3% = 0 THEN RETURN
        RANDOMIZE TIMER
        df3% = INT(RND * 2) + 1
        IF df3% = 1 THEN GOTO x3
        IF df3% = 2 THEN GOTO y3
x3:
        dist2% = tx3% - c1x%
        IF dist2% > 0 THEN tx3% = tx3% - 1
        IF dist2% < 0 THEN tx3% = tx3% + 1
        IF dist2% > 320 THEN tx3% = tx3% + 1
        IF dist2% < -320 THEN tx3% = tx3% - 1
        IF tx3% = c1x% THEN GOTO test32
        RETURN
y3:
        dist22% = ty3% - c1y%
        IF ty3% = c1y% THEN GOTO x3
        IF dist22% > 0 THEN ty3% = ty3% - 1
        IF dist22% < 0 THEN ty3% = ty3% + 1
        IF dist22% > 210 THEN ty3% = ty3% + 1
        IF dist22% < -210 THEN ty3% = ty3% - 1
        RETURN
test32:
        IF ty3% = c1y% THEN GOSUB hit3
        IF health2% = 0 THEN GOSUB explode
        RETURN
hit3:
        health2% = health2% - 10
        t3% = 0
        si% = 0
        DO
        CIRCLE (c1x%, c1y%), si%, 4
        si% = si% + 1
        IF si% = 19 THEN EXIT DO
        LOOP
        si% = 0
        DO
        CIRCLE (c1x%, c1y%), si%, 0
        si% = si% + 1
        IF si% = 19 THEN EXIT DO
        LOOP
        RETURN


explode:
t% = 0
life1% = 0
si% = 0
DO
CIRCLE (c1x%, c1y%), si%, 15
si% = si% + 1
IF si% = 99 THEN EXIT DO
LOOP
si% = 0
DO
CIRCLE (c1x%, c1y%), si%, 0
si% = si% + 1
IF si% = 99 THEN EXIT DO
LOOP
RETURN
'http://www.pulu-pulu.com/milk/j-mi/j-milk.html
'Mai Hagiwara
'http://picturetrade.host.sk/c/sets.htm





enmeyfinder:
        find1% = 0
        IF life1% = 0 THEN RETURN
        dist1% = c1x% - x%
        dist12% = c1y% - y%
        IF dist1% > range& THEN RETURN
        IF dist1% < range& - (range& * 2) THEN RETURN
        IF dist12% > range& THEN RETURN
        IF dist12% < range& - (range& * 2) THEN RETURN
        find1% = 1
        GOSUB enmeyfire
        RETURN
enmeyfire:
        IF find1% = 0 THEN RETURN
        IF life1% = 0 THEN RETURN
        LINE (c1x%, c1y%)-(x%, y%), 4
        SOUND 3000, .1
        health1% = health1% - 1
        IF health1% <= 0 THEN GOSUB youexplode
        LINE (c1x%, c1y%)-(x%, y%), 0
        RETURN


youexplode:
        life1% = 0
        si% = 0
        DO
        CIRCLE (x%, y%), si%, 15
        si% = si% + 1
        IF si% = 99 THEN EXIT DO
        LOOP
        si% = 0
        DO
        CIRCLE (x%, y%), si%, 0
        si% = si% + 1
        IF si% = 99 THEN EXIT DO
        LOOP
        RETURN



endd:
CLS
PRINT "Would you like to play agian"
DO
ans2$ = INKEY$
LOOP UNTIL (ans2$ = "y" OR ans2$ = "n")
IF ans2$ = "y" THEN GOTO start
IF ans2$ = "n" THEN END









