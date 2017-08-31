CLS
start:
SCREEN 12
CLS
DO
LINE (50, 50)-(590, 390), 14, B
LOCATE 7, 30
PRINT "Welcome to"
COLOR 4
LOCATE 7, 41
PRINT "Space Attack"
COLOR 15
LOCATE 8, 32
PRINT "By Matthew Seymour"
LOCATE 11, 28
PRINT "Press N to start a new game"
LOCATE 12, 29
PRINT "or Q to quit at any time."
RANDOMIZE TIMER
stx& = INT(RND * 540) + 50
sty& = INT(RND * 340) + 50
stx2& = INT(RND * 540) + 50
sty2& = INT(RND * 340) + 50
stx3& = INT(RND * 540) + 50
sty3& = INT(RND * 340) + 50
stx4& = INT(RND * 540) + 50
sty4& = INT(RND * 340) + 50
stx5& = INT(RND * 540) + 50
sty5& = INT(RND * 340) + 50
stx6& = INT(RND * 540) + 50
sty6& = INT(RND * 340) + 50
PSET (stx&, sty&), 15
PSET (stx2&, sty2&), 15
PSET (stx3&, sty3&), 15
PSET (stx4&, sty4&), 7
PSET (stx5&, sty5&), 15
PSET (stx6&, sty6&), 7
LINE (319, 279)-(321, 281), 1, BF
LINE (321, 280)-(324, 280), 4
LINE (340, 213)-(342, 215), 4, BF
LINE (346, 220)-(335, 208), 1, B
LINE (320, 280)-(341, 214), 1
DO
ans3$ = INKEY$
LOOP UNTIL (ans3$ = "q" OR ans3$ = "n")
IF ans3$ = "q" THEN GOSUB quit
IF ans3$ = "n" THEN GOTO game
LOOP

game:
CLS
start2:
CLS
incpo& = 5
GOSUB drawing
GOSUB sensors2
x& = 320
y& = 170
RANDOMIZE TIMER
c1x& = INT(RND * 400) - 200
c1y& = INT(RND * 400) - 200
po& = 0
ra& = 1000
ra2& = 170
da& = 150
w& = 0
nw& = 0
max& = 4
tor& = 5
c1tor& = 5
units& = 1000
sen& = 0
eng& = 0
col& = 4
wea& = 0
sup& = 0
tox& = 0
ch1& = 500
life1& = 1
c1t& = 0
c1tx& = c1x&
c1ty& = c1y&
m& = 4
md& = 0

RANDOMIZE TIMER
stx& = INT(RND * 640) + 1
sty& = INT(RND * 330) + 21
stx2& = INT(RND * 640) + 1
sty2& = INT(RND * 330) + 21
stx3& = INT(RND * 640) + 1
sty3& = INT(RND * 330) + 21
stx4& = INT(RND * 640) + 1
sty4& = INT(RND * 330) + 21
stx5& = INT(RND * 640) + 1
sty5& = INT(RND * 330) + 21
stx6& = INT(RND * 640) + 1
sty6& = INT(RND * 330) + 21

begin:
        DO
        DO
        VIEW SCREEN (1, 1)-(638, 330)
        IF w& = 1 THEN GOTO warp
con6:
        VIEW
        LINE (0, 0)-(639, 331), 14, B
        VIEW SCREEN (1, 1)-(638, 330)

        IF life1& = 0 THEN GOTO youlose
        IF ch1& < 1 THEN GOTO comlose
       
        inctox& = 1
        IF w& = 1 THEN intox& = intox& + 6
        IF w& = 0 THEN intox& = intox& + po&
        tox& = tox& + inctox&
        retox& = (sup& - (sup& * 2)) + 100
        tox& = tox& - retox&
        IF tox& > 500 THEN
                RANDOMIZE TIMER
                live& = INT(RND * tox& / 10) + 1
                IF live& > 1 THEN life1& = 0
        END IF
        IF tox& < 0 THEN tox& = 0
       
        IF sen& < 0 THEN sen& = 0
        IF eng& < 0 THEN eng& = 0
        IF wea& < 0 THEN wea& = 0
        IF sup& < 0 THEN sup& = 0

        IF wea& + sen& + eng& + sup& = 400 THEN GOSUB youexplode
       
        GOSUB randoom
        GOSUB finder
        GOSUB c1finder
        GOSUB c1finder2
        VIEW
        GOSUB sensors
        GOSUB engines
        GOSUB weapons
        GOSUB lifesupport
        GOSUB repair
        GOSUB dammage
        VIEW SCREEN (1, 1)-(638, 330)
       
        LINE (c1x& - 1, c1y& - 1)-(c1x& + 1, c1y& + 1), 4, BF
        LINE (c1x& - 2, c1y& - 2)-(c1x& + 2, c1y& + 2), 0, B
        LINE (c1x& - 3, c1y& - 3)-(c1x& + 3, c1y& + 3), 0, B
        LINE (c1x& - 4, c1y& - 4)-(c1x& + 4, c1y& + 4), 0, B
        LINE (c1x& - 6, c1y& - 6)-(c1x& + 6, c1y& + 6), 0, B
        LINE (c1x& - 7, c1y& - 7)-(c1x& + 7, c1y& + 7), 0, B
        LINE (c1x& - 8, c1y& - 8)-(c1x& + 8, c1y& + 8), 0, B
        LINE (c1x& - 9, c1y& - 9)-(c1x& + 9, c1y& + 9), 0, B
        LINE (c1x& - 10, c1y& - 10)-(c1x& + 10, c1y& + 10), 0, B
        LINE (c1x& - 11, c1y& - 11)-(c1x& + 11, c1y& + 11), 0, B
        IF fin& = 0 AND fin2& = 0 THEN LINE (c1x& - 5, c1y& - 5)-(c1x& + 5, c1y& + 5), 0, B
       
        IF fin2& = 1 THEN LINE (targx& - 5, targy& - 5)-(targx& + 5, targy& + 5), 1, B

        LINE (x& - 2, y& - 2)-(x& + 2, y& + 2), 0, B
        IF findcom1tor& = 1 OR findcom1laz& = 1 THEN LINE (x& - 6, y& - 6)-(x& + 6, y& + 6), 4, B
        LINE (x& - 13, y& - 13)-(x& + 13, y& + 13), 0, B
        LINE (x& - 12, y& - 12)-(x& + 12, y& + 12), 0, B
        LINE (x& - 11, y& - 11)-(x& + 11, y& + 11), 0, B
        LINE (x& - 10, y& - 10)-(x& + 10, y& + 10), 0, B
        LINE (x& - 9, y& - 9)-(x& + 9, y& + 9), 0, B
        LINE (x& - 8, y& - 8)-(x& + 8, y& + 8), 0, B
        LINE (x& - 7, y& - 7)-(x& + 7, y& + 7), 0, B
        LINE (x& - 5, y& - 5)-(x& + 5, y& + 5), 0, B
        LINE (x& - 4, y& - 4)-(x& + 4, y& + 4), 0, B
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, B
        IF findcom1laz& = 0 AND findcom1tor& = 0 THEN LINE (x& - 6, y& - 6)-(x& + 6, y& + 6), 0, B
       
        IF c1po& = 100 OR c1po& = 100 THEN
                LINE (c1x& - 1, c1y& - 1)-(c1x& + 1, c1y& + 1), 0, BF
                LINE (c1x& - 5, c1y& - 5)-(c1x& + 5, c1y& + 5), 0, B
        END IF
       
        IF findcom1tor& = 1 THEN LINE (c1x&, c1y&)-(x&, y&), 0
        IF fin2& = 1 THEN LINE (targx& + 1, targy& + 1)-(lazx&, lazy&), 0
        IF fin2& = 1 THEN LINE (targx&, targy&)-(lazx&, lazy&), 0
        IF fin2& = 1 THEN LINE (targx& - 1, targy& - 1)-(lazx&, lazy&), 0
      
        targx& = c1x&
        targy& = c1y&

        dist& = ((c1x& - x&) + (c1y& - y&) / 2)

        IF dist& < 5000 AND dist& > -5000 THEN c1po& = 100
        IF dist& < 500 AND dist& > -500 THEN c1po& = po& + 1
        IF dist& < 50 AND dist& > -50 THEN c1po& = po&

        IF c1po& > 4 AND c1po& < 10 THEN c1po& = 4
        IF w& = 1 THEN c1po& = 100

        IF c1y& - y& < 170 AND c1y& - y& > -170 THEN GOTO con5
       
        GOSUB x1
        GOSUB y1
        GOTO con5
y1:
        dist22& = c1y& - y&
        IF dist22& > 0 THEN c1y& = c1y& - c1po&
        IF dist22& < 0 THEN c1y& = c1y& + c1po&
        RETURN
x1:
        dist2& = c1x& - x&
        IF dist2& > 0 THEN c1x& = c1x& - c1po&
        IF dist2& < 0 THEN c1x& = c1x& + c1po&
        RETURN
con5:
        IF ch1& = 0 THEN GOTO endd
       
        LINE (c1tx& + 1, c1ty& + 1)-(c1tx& - 1, c1ty& - 1), 0, BF
        LINE (tx& + 1, ty& + 1)-(tx& - 1, ty& - 1), 0, BF
       
        po& = po& * incpo&
        IF po& = 1 THEN a& = 1 ELSE a& = 2
        IF m& = 1 THEN
                LINE (x& - 5, y& + 1)-(x& - 5, y& + po&), 1
                LINE (x& + 5, y& + 1)-(x& + 5, y& + po&), 1
                LINE (x& - 5, y& + 1)-(x& - 5, y& + po&), 0
                LINE (x& + 5, y& + 1)-(x& + 5, y& + po&), 0
                CIRCLE (x& - 5, y& + 1), 1, 1
                CIRCLE (x& + 5, y& + 1), 1, 1
                LINE (x& - 5, y&)-(x& + 5, y&), 8
                LINE (x& - 3, y& - 1)-(x& + 3, y& - 1), 8
                LINE (x&, y& + 1)-(x&, y& - 3), 8
                LINE (x& - 5, y&)-(x& - 5, y& - 2), 8
                LINE (x& + 5, y&)-(x& + 5, y& - 2), 8
                PSET (x&, y&), 1
                lazx& = x&
                lazy& = y& - 3
                GOSUB yup
        ELSEIF m& = 2 THEN
                LINE (x& - 3, y& + 5)-(x& + (po&) - 3, y& + (po&) + 5), 1
                LINE (x& + 5, y& - 3)-(x& + (po&) + 5, y& + (po&) - 3), 1
                LINE (x& - 3, y& + 5)-(x& + (po&) - 3, y& + (po&) + 5), 0
                LINE (x& + 5, y& - 3)-(x& + (po&) + 5, y& + (po&) - 3), 0
                CIRCLE (x& - 3, y& + 5), 1, 1
                CIRCLE (x& + 5, y& - 3), 1, 1
                LINE (x& - 2, y& - 2)-(x& + 1, y& + 1), 8
                LINE (x& - 4, y& + 4)-(x& + 4, y& - 4), 8
                LINE (x& - 3, y& + 2)-(x& + 2, y& - 3), 8
                LINE (x& - 4, y& + 4)-(x& - 5, y& + 3), 8
                LINE (x& + 4, y& - 4)-(x& + 5, y& - 5), 8
                PSET (x&, y&), 1
                lazx& = x& - 2
                lazy& = y& - 2
                GOSUB yup
                GOSUB xup
        ELSEIF m& = 3 THEN
                LINE (x& + 1, y& - 5)-(x& + po&, y& - 5), 1
                LINE (x& + 1, y& + 5)-(x& + po&, y& + 5), 1
                LINE (x& + 1, y& - 5)-(x& + po&, y& - 5), 0
                LINE (x& + 1, y& + 5)-(x& + po&, y& + 5), 0
                CIRCLE (x& + 1, y& - 5), 1, 1
                CIRCLE (x& + 1, y& + 5), 1, 1
                LINE (x&, y& - 5)-(x&, y& + 5), 8
                LINE (x& - 1, y& - 3)-(x& - 1, y& + 3), 8
                LINE (x& + 1, y&)-(x& - 3, y&), 8
                LINE (x&, y& - 5)-(x& - 2, y& - 5), 8
                LINE (x&, y& + 5)-(x& - 2, y& + 5), 8
                PSET (x&, y&), 1
                lazx& = x& - 3
                lazy& = y&
                GOSUB xup
        ELSEIF m& = 4 THEN
                LINE (x& - 3, y& - 5)-(x& + (po&) - 3, y& - (po&) - 5), 1
                LINE (x& + 5, y& + 3)-(x& + (po&) + 5, y& - (po&) + 3), 1
                LINE (x& - 3, y& - 5)-(x& + (po&) - 3, y& - (po&) - 5), 0
                LINE (x& + 5, y& + 3)-(x& + (po&) + 5, y& - (po&) + 3), 0
                CIRCLE (x& - 3, y& - 5), 1, 1
                CIRCLE (x& + 5, y& + 3), 1, 1
                LINE (x& - 2, y& + 2)-(x& + 1, y& - 1), 8
                LINE (x& - 4, y& - 4)-(x& + 4, y& + 4), 8
                LINE (x& - 3, y& - 2)-(x& + 2, y& + 3), 8
                LINE (x& - 4, y& - 4)-(x& - 5, y& - 3), 8
                LINE (x& + 4, y& + 4)-(x& + 5, y& + 5), 8
                PSET (x&, y&), 1
                lazx& = x& - 2
                lazy& = y& + 2
                GOSUB ydown
                GOSUB xup
        ELSEIF m& = 5 THEN
                LINE (x& + 5, y& - 1)-(x& + 5, y& - po&), 1
                LINE (x& - 5, y& - 1)-(x& - 5, y& - po&), 1
                LINE (x& + 5, y& - 1)-(x& + 5, y& - po&), 0
                LINE (x& - 5, y& - 1)-(x& - 5, y& - po&), 0
                CIRCLE (x& - 5, y& - 1), 1, 1
                CIRCLE (x& + 5, y& - 1), 1, 1
                LINE (x& - 5, y&)-(x& + 5, y&), 8
                LINE (x& - 3, y& + 1)-(x& + 3, y& + 1), 8
                LINE (x&, y& - 1)-(x&, y& + 3), 8
                LINE (x& - 5, y&)-(x& - 5, y& + 2), 8
                LINE (x& + 5, y&)-(x& + 5, y& + 2), 8
                PSET (x&, y&), 1
                lazx& = x&
                lazy& = y& + 3
                GOSUB ydown
        ELSEIF m& = 6 THEN
                LINE (x& + 3, y& - 5)-(x& - (po&) + 3, y& - (po&) - 5), 1
                LINE (x& - 5, y& + 3)-(x& - (po&) - 5, y& - (po&) + 3), 1
                LINE (x& + 3, y& - 5)-(x& - (po&) + 3, y& - (po&) - 5), 0
                LINE (x& - 5, y& + 3)-(x& - (po&) - 5, y& - (po&) + 3), 0
                CIRCLE (x& + 3, y& - 5), 1, 1
                CIRCLE (x& - 5, y& + 3), 1, 1
                LINE (x& + 2, y& + 2)-(x& - 1, y& - 1), 8
                LINE (x& + 4, y& - 4)-(x& - 4, y& + 4), 8
                LINE (x& + 3, y& - 2)-(x& - 2, y& + 3), 8
                LINE (x& + 4, y& - 4)-(x& + 5, y& - 3), 8
                LINE (x& - 4, y& + 4)-(x& - 5, y& + 5), 8
                PSET (x&, y&), 1
                lazx& = x& + 2
                lazy& = y& + 2
                GOSUB ydown
                GOSUB xdown
        ELSEIF m& = 7 THEN
                LINE (x& - 1, y& - 5)-(x& - po&, y& - 5), 1
                LINE (x& - 1, y& + 5)-(x& - po&, y& + 5), 1
                LINE (x& - 1, y& - 5)-(x& - po&, y& - 5), 0
                LINE (x& - 1, y& + 5)-(x& - po&, y& + 5), 0
                CIRCLE (x& - 1, y& - 5), 1, 1
                CIRCLE (x& - 1, y& + 5), 1, 1
                LINE (x&, y& - 5)-(x&, y& + 5), 8
                LINE (x& + 1, y& - 3)-(x& + 1, y& + 3), 8
                LINE (x& - 1, y&)-(x& + 3, y&), 8
                LINE (x&, y& - 5)-(x& + 2, y& - 5), 8
                LINE (x&, y& + 5)-(x& + 2, y& + 5), 8
                PSET (x&, y&), 1
                lazx& = x& + 3
                lazy& = y&
                GOSUB xdown
        ELSEIF m& = 8 THEN
                LINE (x& + 3, y& + 5)-(x& - (po&) + 3, y& + (po&) + 5), 1
                LINE (x& - 5, y& - 3)-(x& - (po&) - 5, y& + (po&) - 3), 1
                LINE (x& + 3, y& + 5)-(x& - (po&) + 3, y& + (po&) + 5), 0
                LINE (x& - 5, y& - 3)-(x& - (po&) - 5, y& + (po&) - 3), 0
                CIRCLE (x& + 3, y& + 5), 1, 1
                CIRCLE (x& - 5, y& - 3), 1, 1
                LINE (x& + 2, y& - 2)-(x& - 1, y& + 1), 8
                LINE (x& + 4, y& + 4)-(x& - 4, y& - 4), 8
                LINE (x& + 3, y& + 2)-(x& - 2, y& - 3), 8
                LINE (x& + 4, y& + 4)-(x& + 5, y& + 3), 8
                LINE (x& - 4, y& - 4)-(x& - 5, y& - 5), 8
                PSET (x&, y&), 1
                lazx& = x& + 2
                lazy& = y& - 2
                GOSUB yup
                GOSUB xdown
        END IF
        po& = po& / incpo&
        IF po& > 0 THEN
                PSET (stx&, sty&), 15
                LINE (stx& - (po& * incpo&), sty& - (po& * incpo&))-(stx& + (po& * incpo&), sty& + (po& * incpo&)), 0, B
                PSET (stx2&, sty2&), 7
                LINE (stx2& - ((po& * incpo&) * 2), sty2& - ((po& * incpo&) * 2))-(stx2& + ((po& * incpo&) * 2), sty2& + ((po& * incpo&) * 2)), 0, B
                PSET (stx3&, sty3&), 7
                LINE (stx3& - ((po& * incpo&) / 2), sty3& - ((po& * incpo&) / 2))-(stx3& + ((po& * incpo&) / 2), sty3& + ((po& * incpo&) / 2)), 0, B
                PSET (stx4&, sty4&), 15
                LINE (stx4& - ((po& * incpo&) * 1.5), sty4& - ((po& * incpo&) * 1.5))-(stx4& + ((po& * incpo&) * 1.5), sty4& + ((po& * incpo&) * 1.5)), 0, B
                PSET (stx5&, sty5&), 7
                LINE (stx5& - (po& * incpo&), sty5& - (po& * incpo&))-(stx5& + (po& * incpo&), sty5& + (po& * incpo&)), 0, B
                PSET (stx6&, sty6&), 7
                LINE (stx6& - ((po& * incpo&) * 3), sty6& - ((po& * incpo&) * 3))-(stx6& + ((po& * incpo&) * 3), sty6& + ((po& * incpo&) * 3)), 0, B
        END IF
        PSET (stx&, sty&), 15
        PSET (stx2&, sty2&), 7
        PSET (stx3&, sty3&), 7
        PSET (stx4&, sty4&), 15
        PSET (stx5&, sty5&), 7
        PSET (stx6&, sty6&), 7
       
        IF lazer& > 0 THEN GOSUB lazer
        IF c1lazer& > 0 THEN GOSUB c1lazer
        GOSUB c1tor
        IF c1t& = 0 THEN GOTO con4
        LINE (c1tx& + 1, c1ty&)-(c1tx& - 1, c1ty&), 2
        LINE (c1tx&, c1ty& + 1)-(c1tx&, c1ty& - 1), 2
con4:
        GOSUB tor
        IF t& = 0 THEN GOTO con7
        LINE (tx& + 1, ty&)-(tx& - 1, ty&), 2
        LINE (tx&, ty& + 1)-(tx&, ty& - 1), 2
con7:
        IF eng& > 0 THEN max& = 4
        IF eng& > 25 THEN max& = 3
        IF eng& > 50 THEN max& = 2
        IF eng& > 75 THEN max& = 1
        IF eng& = 100 THEN max& = 0
        IF eng& < 70 THEN
                nw& = 0
        ELSEIF eng& > 70 THEN
                nw& = 1
        END IF
        IF nw& = 1 AND w& = 1 THEN
                w& = 0
                po& = 2
                CLS
                GOTO begin
        END IF
        IF po& > max& AND po& < 7 THEN po& = max&
        ans1$ = INKEY$
        LOOP UNTIL (ans1$ = "s" OR ans1$ = "e" OR ans1$ = "w" OR ans1$ = "u" OR ans1$ = "p" OR ans1$ = "x" OR ans1$ = " " OR ans1$ = "8" OR ans1$ = "2" OR ans1$ = "6" OR ans1$ = "4" OR ans1$ = "q" OR ans1$ = "." OR ans1$ = "/")
        IF ans1$ = "6" THEN
                m& = m& - 1
                IF m& < 1 THEN m& = 8
                IF w& = 1 THEN LINE (1, 22)-(638, 329), 0, BF
        ELSEIF ans1$ = "8" THEN
                IF w& = 1 THEN GOTO begin
                po& = po& + 1
                CLS
                IF po& > max& AND po& < 6 THEN po& = max&
        ELSEIF ans1$ = "2" THEN
                IF w& = 1 THEN GOTO begin
                CLS
                po& = po& - 1
                IF po& < 0 THEN po& = 0
        ELSEIF ans1$ = "4" THEN
                m& = m& + 1
                IF m& > 8 THEN m& = 1
                IF w& = 1 THEN LINE (1, 22)-(638, 329), 0, BF
        ELSEIF ans1$ = "x" THEN
                GOSUB fire2
        ELSEIF ans1$ = " " THEN
                GOSUB fire
        ELSEIF ans1$ = "s" THEN
                IF units& = 0 THEN GOTO begin
                units& = units& - 5
                sen& = sen& - 5
                PAINT (10, 471), 13, 0
                PAINT (10, 350), 13, 0
                IF sen& < 0 THEN
                        sen& = sen& + 5
                        units& = units& + 5
                END IF
        ELSEIF ans1$ = "e" THEN
                IF units& = 0 THEN GOTO begin
                units& = units& - 5
                eng& = eng& - 5
                PAINT (110, 471), 13, 0
                PAINT (110, 350), 13, 0
                IF eng& < 0 THEN
                        eng& = eng& + 5
                        units& = units& + 5
                END IF
        ELSEIF ans1$ = "w" THEN
                IF units& = 0 THEN GOTO begin
                units& = units& - 5
                wea& = wea& - 5
                PAINT (210, 471), 13, 0
                PAINT (210, 350), 13, 0
                IF wea& < 0 THEN
                        wea& = wea& + 5
                        units& = units& + 5
                END IF
        ELSEIF ans1$ = "u" THEN
                IF units& = 0 THEN GOTO begin
                units& = units& - 5
                sup& = sup& - 5
                PAINT (310, 471), 13, 0
                PAINT (310, 350), 13, 0
                IF sup& < 0 THEN
                        sup& = sup& + 5
                        units& = units& + 5
                END IF
        ELSEIF ans1$ = "p" THEN GOSUB paused
        ELSEIF ans1$ = "/" THEN
                IF w& = 0 AND nw& = 0 THEN
                        w& = 1
                        po& = 100
                        CLS
                        GOTO begin
                ELSEIF w& = 1 THEN
                        w& = 0
                        po& = 2
                        CLS
                        GOTO begin
                END IF
        ELSEIF ans1$ = "." THEN
                siz& = 1
                DO
                tox& = tox& + 1
                siz& = siz& + 1
                CIRCLE (x&, y&), siz&, 1
                CIRCLE (x&, y&), siz& + 1, 1
                CIRCLE (x&, y&), siz& + 2, 1
                CIRCLE (x&, y&), siz& + 3, 1
                CIRCLE (x&, y&), siz& - 1, 0
                IF siz& > 154 THEN EXIT DO
                LOOP
                LINE (1, 2)-(638, 329), 15, BF
                LINE (1, 2)-(638, 329), 0, BF
                LINE (x& - 5, y& - 5)-(x& + 5, y& + 5), 0, B
                LINE (x& - 1, y& - 1)-(x& + 1, y& + 1), 0, BF
                RANDOMIZE TIMER
                c1x& = INT(RND * 20000000) - 10000000
                c1y& = INT(RND * 20000000) - 10000000
                c1ty& = INT(RND * 20000000) - 10000000
                c1tx& = INT(RND * 20000000) - 10000000
                stx& = INT(RND * 640) + 1
                sty& = INT(RND * 330) + 21
                stx2& = INT(RND * 640) + 1
                sty2& = INT(RND * 330) + 21
                stx3& = INT(RND * 640) + 1
                sty3& = INT(RND * 330) + 21
                stx4& = INT(RND * 640) + 1
                sty4& = INT(RND * 330) + 21
                stx5& = INT(RND * 640) + 1
                sty5& = INT(RND * 330) + 21
                stx6& = INT(RND * 640) + 1
                sty6& = INT(RND * 330) + 21
                DO
                siz& = siz& - 1
                CIRCLE (x&, y&), siz&, 1
                CIRCLE (x&, y&), siz& - 1, 1
                CIRCLE (x&, y&), siz& - 2, 1
                CIRCLE (x&, y&), siz& - 3, 1
                CIRCLE (x&, y&), siz& + 1, 0
                tox& = tox& + 1
                IF siz& < 5 THEN EXIT DO
                LOOP
                hy& = INT(RND * eng& / 10) + 1
                IF hy& = 1 THEN GOTO begin
                IF w& = 1 THEN
                hy& = INT(RND * eng&) + 1
                IF hy& = 1 THEN GOTO begin
                END IF
                GOSUB youexplode
        ELSEIF ans1$ = "q" THEN GOSUB quit
        END IF
        LOOP

finder:
        fin2& = 0
        dist1& = x& - targx&
        dist2& = y& - targy&
        IF dist1& > ra& - wea& THEN GOTO finder2
        IF dist1& < -ra& + wea& THEN GOTO finder2
        IF dist2& > ra& - wea& THEN GOTO finder2
        IF dist2& < -ra& + wea& THEN GOTO finder2
        fin2& = 1
        GOTO finder2
finder2:
        fin& = 0
        dist1& = x& - targx&
        dist2& = y& - targy&
        IF dist1& > ra2& - wea& THEN RETURN
        IF dist1& < -ra2& - wea& THEN RETURN
        IF dist2& > ra2& - wea& THEN RETURN
        IF dist2& < -ra2& - wea& THEN RETURN
        fin& = 1
        RETURN
fire:
        pain& = 0
        IF fin& = 1 THEN lazer& = 5
lazer:
        IF fin& = 0 THEN RETURN
        lazer& = lazer& - 1
        LINE (targx& - 1, targy& - 1)-(x&, y&), 1
        LINE (targx&, targy&)-(lazx&, lazy&), 1
        LINE (targx& + 1, targy& + 1)-(lazx&, lazy&), 1
        SOUND 3000, .1
        ch1& = ch1& - pain&
        IF ch1& < 1 THEN GOSUB explode
        RETURN
fire2:
        IF t& = 1 THEN RETURN
        IF tor& = 0 THEN RETURN
        IF t& = 0 THEN tx& = x&
        IF t& = 0 THEN ty& = y&
        IF fin2& = 0 THEN RETURN
        IF life1& = 0 THEN RETURN
        SOUND 3000, .1
        tor& = tor& - 1
        t& = 1
        RETURN
tor:
        dist22& = c1tx& - targx&
        IF dist22& > 0 THEN
                IF dist22& > 0 THEN amx& = dist22&
                IF dist22& > 200 THEN amx& = 75
                IF dist22& > 1000 THEN amx& = 200
                IF dist22& > 10000 THEN amx& = 500
                IF dist22& > 100000 THEN amx& = 1000
                IF dist22& > 1000000 THEN amx& = 10000
                IF dist22& > 10000000 THEN amx& = 100000
                IF dist22& > 100000000 THEN amx& = 1000000
                IF dist22& > 1000000000 THEN amx& = 10000000
        ELSEIF dist22& < 0 THEN
                IF dist22& < 0 THEN amx& = dist22& - (dist22& * 2)
                IF dist22& < -200 THEN amx& = 75
                IF dist22& < -1000 THEN amx& = 200
                IF dist22& < -10000 THEN amx& = 500
                IF dist22& < -100000 THEN amx& = 1000
                IF dist22& < -1000000 THEN amx& = 10000
                IF dist22& < -10000000 THEN amx& = 100000
                IF dist22& < -100000000 THEN amx& = 1000000
                IF dist22& < -1000000000 THEN amx& = 10000000
        END IF
        dist22& = c1ty& - targy&
        IF dist22& > 0 THEN
                IF dist22& > 50 THEN amy& = 25
                IF dist22& > 200 THEN amy& = 75
                IF dist22& > 1000 THEN amy& = 200
                IF dist22& > 10000 THEN amy& = 500
                IF dist22& > 100000 THEN amy& = 1000
                IF dist22& > 1000000 THEN amy& = 10000
                IF dist22& > 10000000 THEN amy& = 100000
                IF dist22& > 100000000 THEN amy& = 1000000
                IF dist22& > 1000000000 THEN amy& = 10000000
        ELSEIF dist22& < 0 THEN
                IF dist22& < 0 THEN amy& = dist22& - (dist22& * 2)
                IF dist22& > -200 THEN amy& = 75
                IF dist22& < -1000 THEN amy& = 200
                IF dist22& < -10000 THEN amy& = 500
                IF dist22& < -100000 THEN amy& = 1000
                IF dist22& < -1000000 THEN amy& = 10000
                IF dist22& < -10000000 THEN amy& = 100000
                IF dist22& < -100000000 THEN amy& = 1000000
                IF dist22& < -1000000000 THEN amy& = 10000000
        END IF
        IF w& = 1 THEN amx& = amx& + 200
        IF w& = 1 THEN amy& = amy& + 200
        IF t& = 0 THEN RETURN
        GOSUB x
        GOSUB y
        RETURN
y:
        dist22& = ty& - targy&
        IF dist22& > 0 THEN ty& = ty& - amy&
        IF dist22& < 0 THEN ty& = ty& + amy&
        RETURN
x:
        dist2& = tx& - targx&
        IF dist2& > 0 THEN tx& = tx& - amx&
        IF dist2& < 0 THEN tx& = tx& + amx&
        IF tx& > targx& - 15 AND tx& < targx& + 15 THEN GOSUB test
        RETURN
test:
        IF ty& > targy& - 15 AND ty& < targy& + 15 THEN GOSUB hit
        IF ch1& < 1 THEN GOSUB explode
        RETURN
hit:
        ch1& = ch1& - da&
        LINE (tx& - 1, ty& - 1)-(tx& + 1, ty& + 1), 0, BF
        t& = 0
        RETURN
explode:
        c1t& = 0
        si& = 0
        DO
        CIRCLE (targx&, targy&), si&, 15
        si& = si& + 1
        IF si& = 99 THEN EXIT DO
        LOOP
        si& = 0
        DO
        CIRCLE (targx&, targy&), si&, 0
        si& = si& + 1
        IF si& = 99 THEN EXIT DO
        LOOP
        RETURN
c1finder:
        findcom1tor& = 0
        IF life1& = 0 THEN RETURN
        dist1& = c1x& - x&
        dist12& = c1y& - y&
        IF dist1& > ra& THEN RETURN
        IF dist1& < -ra& THEN RETURN
        IF dist12& > ra& THEN RETURN
        IF dist12& < -ra& THEN RETURN
        findcom1tor& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB c1fire ELSE RETURN
        RETURN
c1finder2:
        findcom1laz& = 0
        IF life1& = 0 THEN RETURN
        dist1& = c1x& - x&
        dist12& = c1y& - y&
        IF dist1& > 170 THEN RETURN
        IF dist1& < -170 THEN RETURN
        IF dist12& > 170 THEN RETURN
        IF dist12& < -170 THEN RETURN
        findcom1laz& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB c1firelaz ELSE RETURN
        RETURN
c1firelaz:
        IF c1lazer > 0 THEN RETURN
        RANDOMIZE TIMER
        chance& = INT(RND * 50) + 1
        IF chance& > 1 THEN RETURN
        IF findcom1laz& = 1 THEN c1lazer& = 30
        RETURN
c1lazer:
        IF findcom1laz& = 0 THEN RETURN
        c1lazer& = c1lazer& - 1
        IF life1& = 0 THEN RETURN
        LINE (c1x&, c1y&)-(x&, y&), 4
        SOUND 3000, .1
        tox& = tox& + 2
        RANDOMIZE TIMER
        cube& = INT(RND * 4) + 1
        IF cube& = 1 THEN sen& = sen& + 1
        IF cube& = 2 THEN eng& = eng& + 1
        IF cube& = 3 THEN wea& = wea& + 1
        IF cube& = 4 THEN sup& = sup& + 1
        IF sen& > 100 THEN sen& = 100
        IF eng& > 100 THEN eng& = 100
        IF wea& > 100 THEN wea& = 100
        IF sup& > 100 THEN sup& = 100
        IF wea& + sen& + eng& + sup& = 400 THEN GOSUB youexplode
        RETURN
c1fire:
        RANDOMIZE TIMER
        chance& = INT(RND * 50) + 1
        IF chance& > 1 THEN RETURN
        IF c1t& = 1 THEN RETURN
        IF c1tor& = 0 THEN RETURN
        IF c1t& = 0 THEN c1tx& = c1x&
        IF c1t& = 0 THEN c1ty& = c1y&
        IF findcom1tor& = 0 THEN RETURN
        SOUND 3000, .1
        c1t& = 1
        c1tor& = c1tor& - 1
        RETURN
c1tor:
        dist2& = c1tx& - x&
        dist22& = c1ty& - y&
        IF dist2& > 0 THEN
                IF dist2& > 0 THEN amx& = 8
                IF dist2& > 50 THEN amx& = 25
                IF dist2& > 100 THEN amx& = 75
                IF dist2& > 1000 THEN amx& = 200
                IF dist2& > 10000 THEN amx& = 500
                IF dist2& > 100000 THEN amx& = 1000
                IF dist2& > 1000000 THEN amx& = 10000
                IF dist2& > 10000000 THEN amx& = 100000
                IF dist2& > 100000000 THEN amx& = 1000000
                IF dist2& > 1000000000 THEN amx& = 10000000
        ELSEIF dist2& < 0 THEN
                IF dist2& < 0 THEN amx& = 8
                IF dist2& < -50 THEN amx& = 25
                IF dist2& < -100 THEN amx& = 75
                IF dist2& < -1000 THEN amx& = 200
                IF dist2& < -10000 THEN amx& = 500
                IF dist2& < -100000 THEN amx& = 1000
                IF dist2& < -1000000 THEN amx& = 10000
                IF dist2& < -10000000 THEN amx& = 100000
                IF dist2& < -100000000 THEN amx& = 1000000
                IF dist2& < -1000000000 THEN amx& = 10000000
        END IF
        IF dist22& > 0 THEN
                IF dist22& > 0 THEN amy& = 8
                IF dist22& > 50 THEN amy& = 25
                IF dist22& > 100 THEN amy& = 75
                IF dist22& > 1000 THEN amy& = 200
                IF dist22& > 10000 THEN amy& = 500
                IF dist22& > 100000 THEN amy& = 1000
                IF dist22& > 1000000 THEN amy& = 10000
                IF dist22& > 10000000 THEN amy& = 100000
                IF dist22& > 100000000 THEN amy& = 1000000
                IF dist22& > 1000000000 THEN amy& = 10000000
        ELSEIF dist22& < 0 THEN
                IF dist22& < 0 THEN amy& = 8
                IF dist22& < -50 THEN amy& = 25
                IF dist22& < -100 THEN amy& = 75
                IF dist22& < -1000 THEN amy& = 200
                IF dist22& < -10000 THEN amy& = 500
                IF dist22& < -100000 THEN amy& = 1000
                IF dist22& < -1000000 THEN amy& = 10000
                IF dist22& < -10000000 THEN amy& = 100000
                IF dist22& < -100000000 THEN amy& = 1000000
                IF dist22& < -1000000000 THEN amy& = 10000000
        END IF
        IF w& = 1 THEN amx& = amx& + 200
        IF w& = 1 THEN amy& = amy& + 200
        IF c1t& = 0 THEN RETURN
        RANDOMIZE TIMER
        di& = INT(RND * 2) + 1
        IF di& = 2 THEN GOSUB c1x
        IF di& = 1 THEN GOSUB c1y
        RETURN
c1y:
        dist22& = c1ty& - y&
        IF dist22& > 0 THEN c1ty& = c1ty& - amy&
        IF dist22& < 0 THEN c1ty& = c1ty& + amy&
        IF c1ty& > y& - 15 AND c1ty& < y& + 15 THEN GOTO c1x
        RETURN
c1x:
        dist2& = c1tx& - x&
        IF dist2& > 0 THEN c1tx& = c1tx& - amx&
        IF dist2& < 0 THEN c1tx& = c1tx& + amx&
        IF c1tx& > x& - 15 AND c1tx& < x& + 15 THEN GOSUB c1test2
        RETURN
c1test2:
        d& = 0
        IF c1ty& > y& - 15 AND c1ty& < y& + 15 THEN GOSUB c1hit
        RETURN
c1hit:
        DO
        d& = d& + 1
        tox& = tox& + 1
        RANDOMIZE TIMER
        cube& = INT(RND * 4) + 1
        IF cube& = 1 THEN sen& = sen& + 1
        IF cube& = 2 THEN eng& = eng& + 1
        IF cube& = 3 THEN wea& = wea& + 1
        IF cube& = 4 THEN sup& = sup& + 1
        IF sen& > 100 THEN
                sen& = 100
                d& = d& - 1
        ELSEIF eng& > 100 THEN
                eng& = 100
                d& = d& - 1
        ELSEIF wea& > 100 THEN
                wea& = 100
                d& = d& - 1
        ELSEIF sup& > 100 THEN
                sup& = 100
                d& = d& - 1
        END IF
        IF wea& + sen& + eng& + sup& = 400 THEN GOSUB youexplode
        IF wea& + sen& + eng& + sup& = 400 THEN GOTO begin
        IF d& = da& THEN EXIT DO
        LOOP
        LINE (c1tx& - 1, c1ty& - 1)-(c1tx& + 1, c1ty& + 1), 0, BF
        c1t& = 0
        si& = 0
        RETURN
youexplode:
        life1& = 0
        si& = 0
        DO
        CIRCLE (x&, y&), si&, 15
        si& = si& + 1
        IF si& = 99 THEN EXIT DO
        LOOP
        si& = 0
        DO
        CIRCLE (x&, y&), si&, 0
        si& = si& + 1
        IF si& = 99 THEN EXIT DO
        LOOP
        RETURN

youlose:
VIEW
CLS
PRINT "You lost!"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = " ")
GOTO endd

comlose:
CLS
ch1& = 500
c1tor& = 5
RANDOMIZE TIMER
c1x& = INT(RND * 400000) - 200000
c1y& = INT(RND * 400000) - 200000
GOTO begin

bothlose:
CLS
PRINT "You crashed!!!"
DO
ans6$ = INKEY$
LOOP UNTIL (ans6$ = " ")
GOTO endd

endd:
CLS
VIEW
PRINT "Would you like to play agian"
DO
ans2$ = INKEY$
LOOP UNTIL (ans2$ = "y" OR ans2$ = "n")
IF ans2$ = "y" THEN GOTO game
IF ans2$ = "n" THEN GOSUB quit
GOTO start2

quit:
CLS
4 LOCATE 12, 28
PRINT "Do you relly want to quit?"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = "y" OR ans4$ = "n")
IF ans4$ = "y" THEN END
IF ans4$ = "n" THEN RETURN
RETURN

paused:
LOCATE 1, 35
PRINT "PAUSED"
DO
ans7$ = INKEY$
LOOP UNTIL (ans7$ = " ")
LOCATE 1, 34
IF ans7$ = " " THEN PRINT "        "
RETURN

randoom:
RANDOMIZE TIMER
IF stx& > 640 OR stx& < 0 THEN
        PSET (stx&, sty&), 0
        sty& = INT(RND * 330) + 1
        IF stx& > 640 THEN stx& = 1
        IF stx& < 0 THEN stx& = 639

ELSEIF sty& > 330 OR sty& < 1 THEN
        PSET (stx&, sty&), 0
        stx& = INT(RND * 640) + 1
        IF sty& > 330 THEN sty& = 1
        IF sty& < 1 THEN sty& = 329
       
ELSEIF stx2& > 640 OR stx2& < 0 THEN
        PSET (stx2&, sty2&), 0
        sty2& = INT(RND * 330) + 1
        IF stx2& > 640 THEN stx2& = 1
        IF stx2& < 0 THEN stx2& = 639

ELSEIF sty2& > 330 OR sty2& < 1 THEN
        PSET (stx2&, sty2&), 0
        stx2& = INT(RND * 640) + 1
        IF sty2& > 330 THEN sty2& = 1
        IF sty2& < 1 THEN sty2& = 329
         
ELSEIF stx3& > 640 OR stx3& < 0 THEN
        PSET (stx3&, sty3&), 0
        sty3& = INT(RND * 330) + 1
        IF stx3& > 640 THEN stx3& = 1
        IF stx3& < 0 THEN stx3& = 639

ELSEIF sty3& > 330 OR sty3& < 1 THEN
        PSET (stx3&, sty3&), 0
        stx3& = INT(RND * 640) + 1
        IF sty3& > 330 THEN sty3& = 1
        IF sty3& < 1 THEN sty3& = 329
       
ELSEIF stx4& > 640 OR stx4& < 0 THEN
        PSET (stx4&, sty4&), 0
        sty4& = INT(RND * 330) + 1
        IF stx4& > 640 THEN stx4& = 1
        IF stx4& < 0 THEN stx4& = 639

ELSEIF sty4& > 330 OR sty4& < 1 THEN
        PSET (stx4&, sty4&), 0
        stx4& = INT(RND * 640) + 1
        IF sty4& > 330 THEN sty4& = 1
        IF sty4& < 1 THEN sty4& = 329
       
ELSEIF stx5& > 640 OR stx5& < 0 THEN
        PSET (stx5&, sty5&), 0
        sty5& = INT(RND * 330) + 1
        IF stx5& > 640 THEN stx5& = 1
        IF stx5& < 0 THEN stx5& = 639

ELSEIF sty5& > 330 OR sty5& < 1 THEN
        PSET (stx5&, sty5&), 0
        stx5& = INT(RND * 640) + 1
        IF sty5& > 330 THEN sty5& = 1
        IF sty5& < 1 THEN sty5& = 329
       
ELSEIF stx6& > 640 OR stx6& < 0 THEN
        PSET (stx6&, sty6&), 0
        sty6& = INT(RND * 330) + 1
        IF stx6& > 640 THEN stx6& = 1
        IF stx6& < 0 THEN stx6& = 639

ELSEIF sty6& > 330 OR sty6& < 1 THEN
        PSET (stx6&, sty6&), 0
        stx6& = INT(RND * 640) + 1
        IF sty6& > 330 THEN sty6& = 1
        IF sty6& < 1 THEN sty6& = 329
        

END IF
RETURN

warp:
IF m& = 1 THEN
        LINE (stx&, sty&)-(stx&, sty& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx&, sty&)-(stx& - 300, sty& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx&, sty&)-(stx& - 300, sty&), 7
ELSEIF m& = 4 THEN
        LINE (stx&, sty&)-(stx& - 300, sty& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx&, sty&)-(stx&, sty& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx&, sty&)-(stx& + 300, sty& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx&, sty&)-(stx& + 300, sty&), 7
ELSEIF m& = 8 THEN
        LINE (stx&, sty&)-(stx& + 300, sty& - 300), 7
END IF

LINE (stx&, sty&)-(stx& - 300, sty& - 300), 0
LINE (stx&, sty&)-(stx& + 300, sty& - 300), 0
LINE (stx&, sty&)-(stx& - 300, sty& + 300), 0
LINE (stx&, sty&)-(stx& + 300, sty& + 300), 0
LINE (stx&, sty&)-(stx& - 300, sty&), 0
LINE (stx&, sty&)-(stx&, sty& - 300), 0
LINE (stx&, sty&)-(stx& + 300, sty&), 0
LINE (stx&, sty&)-(stx&, sty& + 300), 0

IF m& = 1 THEN
        LINE (stx2&, sty2&)-(stx2&, sty2& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx2&, sty2&)-(stx2& - 300, sty2& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx2&, sty2&)-(stx2& - 300, sty2&), 7
ELSEIF m& = 4 THEN
        LINE (stx2&, sty2&)-(stx2& - 300, sty2& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx2&, sty2&)-(stx2&, sty2& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx2&, sty2&)-(stx2& + 300, sty2& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx2&, sty2&)-(stx2& + 300, sty2&), 7
ELSEIF m& = 8 THEN
        LINE (stx2&, sty2&)-(stx2& + 300, sty2& - 300), 7
END IF

LINE (stx2&, sty2&)-(stx2& - 300, sty2& - 300), 0
LINE (stx2&, sty2&)-(stx2& + 300, sty2& - 300), 0
LINE (stx2&, sty2&)-(stx2& - 300, sty2& + 300), 0
LINE (stx2&, sty2&)-(stx2& + 300, sty2& + 300), 0
LINE (stx2&, sty2&)-(stx2& - 300, sty2&), 0
LINE (stx2&, sty2&)-(stx2&, sty2& - 300), 0
LINE (stx2&, sty2&)-(stx2& + 300, sty2&), 0
LINE (stx2&, sty2&)-(stx2&, sty2& + 300), 0

IF m& = 1 THEN
        LINE (stx3&, sty3&)-(stx3&, sty3& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx3&, sty3&)-(stx3& - 300, sty3& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx3&, sty3&)-(stx3& - 300, sty3&), 7
ELSEIF m& = 4 THEN
        LINE (stx3&, sty3&)-(stx3& - 300, sty3& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx3&, sty3&)-(stx3&, sty3& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx3&, sty3&)-(stx3& + 300, sty3& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx3&, sty3&)-(stx3& + 300, sty3&), 7
ELSEIF m& = 8 THEN
        LINE (stx3&, sty3&)-(stx3& + 300, sty3& - 300), 7
END IF

LINE (stx3&, sty3&)-(stx3& - 300, sty3& - 300), 0
LINE (stx3&, sty3&)-(stx3& + 300, sty3& - 300), 0
LINE (stx3&, sty3&)-(stx3& - 300, sty3& + 300), 0
LINE (stx3&, sty3&)-(stx3& + 300, sty3& + 300), 0
LINE (stx3&, sty3&)-(stx3& - 300, sty3&), 0
LINE (stx3&, sty3&)-(stx3&, sty3& - 300), 0
LINE (stx3&, sty3&)-(stx3& + 300, sty3&), 0
LINE (stx3&, sty3&)-(stx3&, sty3& + 300), 0

IF m& = 1 THEN
        LINE (stx4&, sty4&)-(stx4&, sty4& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx4&, sty4&)-(stx4& - 300, sty4& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx4&, sty4&)-(stx4& - 300, sty4&), 7
ELSEIF m& = 4 THEN
        LINE (stx4&, sty4&)-(stx4& - 300, sty4& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx4&, sty4&)-(stx4&, sty4& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx4&, sty4&)-(stx4& + 300, sty4& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx4&, sty4&)-(stx4& + 300, sty4&), 7
ELSEIF m& = 8 THEN
        LINE (stx4&, sty4&)-(stx4& + 300, sty4& - 300), 7
END IF

LINE (stx4&, sty4&)-(stx4& - 300, sty4& - 300), 0
LINE (stx4&, sty4&)-(stx4& + 300, sty4& - 300), 0
LINE (stx4&, sty4&)-(stx4& - 300, sty4& + 300), 0
LINE (stx4&, sty4&)-(stx4& + 300, sty4& + 300), 0
LINE (stx4&, sty4&)-(stx4& - 300, sty4&), 0
LINE (stx4&, sty4&)-(stx4&, sty4& - 300), 0
LINE (stx4&, sty4&)-(stx4& + 300, sty4&), 0
LINE (stx4&, sty4&)-(stx4&, sty4& + 300), 0

IF m& = 1 THEN
        LINE (stx5&, sty5&)-(stx5&, sty5& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx5&, sty5&)-(stx5& - 300, sty5& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx5&, sty5&)-(stx5& - 300, sty5&), 7
ELSEIF m& = 4 THEN
        LINE (stx5&, sty5&)-(stx5& - 300, sty5& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx5&, sty5&)-(stx5&, sty5& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx5&, sty5&)-(stx5& + 300, sty5& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx5&, sty5&)-(stx5& + 300, sty5&), 7
ELSEIF m& = 8 THEN
        LINE (stx5&, sty5&)-(stx5& + 300, sty5& - 300), 7
END IF

LINE (stx5&, sty5&)-(stx5& - 300, sty5& - 300), 0
LINE (stx5&, sty5&)-(stx5& + 300, sty5& - 300), 0
LINE (stx5&, sty5&)-(stx5& - 300, sty5& + 300), 0
LINE (stx5&, sty5&)-(stx5& + 300, sty5& + 300), 0
LINE (stx5&, sty5&)-(stx5& - 300, sty5&), 0
LINE (stx5&, sty5&)-(stx5&, sty5& - 300), 0
LINE (stx5&, sty5&)-(stx5& + 300, sty5&), 0
LINE (stx5&, sty5&)-(stx5&, sty5& + 300), 0

IF m& = 1 THEN
        LINE (stx6&, sty6&)-(stx6&, sty6& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx6&, sty6&)-(stx6& - 300, sty6& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx6&, sty6&)-(stx6& - 300, sty6&), 7
ELSEIF m& = 4 THEN
        LINE (stx6&, sty6&)-(stx6& - 300, sty6& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx6&, sty6&)-(stx6&, sty6& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx6&, sty6&)-(stx6& + 300, sty6& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx6&, sty6&)-(stx6& + 300, sty6&), 7
ELSEIF m& = 8 THEN
        LINE (stx6&, sty6&)-(stx6& + 300, sty6& - 300), 7
END IF

LINE (stx6&, sty6&)-(stx6& - 300, sty6& - 300), 0
LINE (stx6&, sty6&)-(stx6& + 300, sty6& - 300), 0
LINE (stx6&, sty6&)-(stx6& - 300, sty6& + 300), 0
LINE (stx6&, sty6&)-(stx6& + 300, sty6& + 300), 0
LINE (stx6&, sty6&)-(stx6& - 300, sty6&), 0
LINE (stx6&, sty6&)-(stx6&, sty6& - 300), 0
LINE (stx6&, sty6&)-(stx6& + 300, sty6&), 0
LINE (stx6&, sty6&)-(stx6&, sty6& + 300), 0
GOTO con6

yup:
c1ty& = c1ty& + po&
c1y& = c1y& + po&
sty& = sty& + po&
sty2& = sty2& + po& * 2
sty3& = sty3& + po& / a&
sty4& = sty4& + po& * 1.5
sty5& = sty5& + po&
sty6& = sty6& + po& * 3
RETURN

xup:
c1tx& = c1tx& + po&
c1x& = c1x& + po&
stx& = stx& + po&
stx2& = stx2& + po& * 2
stx3& = stx3& + po& / a&
stx4& = stx4& + po& * 1.5
stx5& = stx5& + po&
stx6& = stx6& + po& * 3
RETURN

ydown:
c1ty& = c1ty& - po&
c1y& = c1y& - po&
sty& = sty& - po&
sty2& = sty2& - po& * 2
sty3& = sty3& - po& / a&
sty4& = sty4& - po& * 1.5
sty5& = sty5& - po&
sty6& = sty6& - po& * 3
RETURN

xdown:
c1tx& = c1tx& - po&
c1x& = c1x& - po&
stx& = stx& - po&
stx2& = stx2& - po& * 2
stx3& = stx3& - po& / a&
stx4& = stx4& - po& * 1.5
stx5& = stx5& - po&
stx6& = stx6& - po& * 3
RETURN

sensors:
        IF sen& = 100 THEN
                PAINT (10, 471), 5, 0
                PAINT (10, 350), 5, 0
                LOCATE 24, 4
                PRINT "        "
                LOCATE 25, 4
                PRINT "         "
                LOCATE 26, 4
                PRINT "         "
                LOCATE 27, 4
                PRINT "         "
                LOCATE 28, 4
                PRINT "         "
                RETURN
        END IF
        COLOR 14
        LOCATE 25, 4
        PRINT "Enemy 1"
        IF dist1& > 0 THEN
                LOCATE 26, 4
                PRINT dist1&
                LOCATE 26, 4
                PRINT CHR$(16)
        ELSEIF dist1& < 0 THEN
                LOCATE 26, 4
                PRINT dist1&
                LOCATE 26, 4
                PRINT CHR$(17)
        ELSEIF dist1& = 0 THEN
                LOCATE 26, 4
                PRINT dist1&
        END IF
        IF dist2& > 0 THEN
                LOCATE 27, 4
                PRINT dist2&
                LOCATE 27, 4
                PRINT CHR$(24)
        ELSEIF dist2& < 0 THEN
                LOCATE 27, 4
                PRINT dist2&
                LOCATE 27, 4
                PRINT CHR$(25)
        ELSEIF dist2& = 0 THEN
                LOCATE 27, 4
                PRINT dist2&
        END IF
        LOCATE 28, 4
        PRINT "DAMMAGE"
        LINE (((500 - ch1&) / 10) + 25, 450)-(75, 455), 2, BF
        LINE (25, 450)-(((500 - ch1&) / 10) + 25, 455), 4, BF
        COLOR 15
        RETURN
engines:
        IF eng& = 100 THEN
                PAINT (110, 471), 5, 0
                PAINT (110, 350), 5, 0
                LOCATE 24, 16
                PRINT "         "
                LOCATE 25, 16
                PRINT "         "
                LOCATE 26, 16
                PRINT "         "
                LOCATE 27, 16
                PRINT "         "
                LOCATE 28, 16
                PRINT "         "
                RETURN
        END IF
        COLOR 14
        LOCATE 25, 16
        PRINT "Max:"; max&
        IF nw& = 1 THEN
                LOCATE 26, 16
                PRINT "Warp:"
                LOCATE 27, 16
                PRINT "Disabled"
        ELSEIF w& = 0 THEN
                LOCATE 26, 16
                PRINT "Warp:"
                LOCATE 27, 16
                PRINT "Idle    "
        ELSEIF w& = 1 THEN
                LOCATE 26, 16
                PRINT "Warp:"
                LOCATE 27, 16
                PRINT "Acitve  "
        END IF
        LOCATE 24, 16
        IF w& = 0 THEN PRINT "Speed:"; po&
        IF w& = 1 THEN PRINT "Speed: -"
        COLOR 15
        RETURN
weapons:
        IF wea& = 100 THEN
                PAINT (210, 471), 5, 0
                PAINT (210, 350), 5, 0
                LOCATE 24, 29
                PRINT "          "
                LOCATE 25, 29
                PRINT "          "
                LOCATE 26, 29
                PRINT "          "
                LOCATE 27, 29
                PRINT "          "
                LOCATE 28, 29
                PRINT "          "
                RETURN
        END IF
        COLOR 14
        LOCATE 24, 29
        IF mode& = 0 THEN PRINT "Auto Aim"
        IF mode& = 1 THEN PRINT "Manual Aim"
        LOCATE 25, 37
        PRINT tor&
        LOCATE 25, 29
        PRINT "Torpedos:"
        LOCATE 26, 29
        IF fin2& = 1 THEN PRINT "LOCKED ON"
        IF fin2& = 0 THEN PRINT "NO LOCK  "
        LOCATE 27, 29
        PRINT "Lazer:"
        LOCATE 28, 29
        IF fin& = 1 THEN PRINT "LOCKED ON"
        IF fin& = 0 THEN PRINT "NO LOCK  "
        COLOR 15
        RETURN
lifesupport:
        IF sup& = 100 THEN
                PAINT (310, 471), 5, 0
                PAINT (310, 350), 5, 0
                LOCATE 25, 41
                PRINT "           "
                LOCATE 26, 41
                PRINT "          "
                LOCATE 27, 41
                PRINT "         "
                LOCATE 28, 41
                PRINT "         "
                RETURN
        END IF
        COLOR 14
        LOCATE 25, 41
        PRINT "Toxins:"; tox&; "  "
        LOCATE 26, 41
        PRINT "Detox:"; retox&
        LOCATE 25, 54
        PRINT "S-SENSORS"
        LOCATE 27, 41
        IF tox& > 200 THEN
                IF col& = 14 THEN
                        col& = 4
                ELSEIF col& = 4 THEN
                        col& = 14
                END IF
                COLOR col&
                PRINT "Dangerous "
                LOCATE 28, 41
                PRINT "Toxicity"
                COLOR 15
        ELSEIF tox& < 200 THEN
                COLOR 0
                PRINT "          "
                LOCATE 28, 41
                PRINT "          "
                COLOR 15
        END IF
        COLOR 15
        RETURN
repair:
        COLOR 14
        LOCATE 24, 54
        PRINT "UNITS"; units&
        COLOR 15
        RETURN
dammage:
        COLOR 14
        LOCATE 24, 66
        PRINT "SENSORS %"; sen&
        LOCATE 25, 66
        PRINT "ENGINES %"; eng&
        LOCATE 26, 66
        PRINT "WEAPONS %"; wea&
        LOCATE 27, 66
        PRINT "SUPPORT %"; sup&
        LOCATE 28, 66
        PRINT "TOTAL %"
        LOCATE 28, 74
        PRINT CINT((sen& + eng& + wea& + sup&) / 4)
        COLOR 15
        RETURN

sensors2:
        LOCATE 23, 4
        COLOR 14
        PRINT "SENSORS"
        LOCATE 24, 4
        PRINT "Object:"
        LOCATE 25, 4
        PRINT "Enemy 1"
        IF dist1& > 0 THEN
                LOCATE 26, 4
                PRINT dist1&
                LOCATE 26, 4
                PRINT CHR$(16)
        ELSEIF dist1& < 0 THEN
                LOCATE 26, 4
                PRINT dist1&
                LOCATE 26, 4
                PRINT CHR$(17)
        ELSEIF dist1& = 0 THEN
                LOCATE 26, 4
                PRINT dist1&
        END IF
        IF dist2& > 0 THEN
                LOCATE 27, 4
                PRINT dist2&
                LOCATE 27, 4
                PRINT CHR$(24)
        ELSEIF dist2& < 0 THEN
                LOCATE 27, 4
                PRINT dist2&
                LOCATE 27, 4
                PRINT CHR$(25)
        ELSEIF dist2& = 0 THEN
                LOCATE 27, 4
                PRINT dist2&
        END IF
        LOCATE 28, 4
        PRINT "DAMMAGE"
        LINE (((500 - ch1&) / 10) + 25, 450)-(75, 455), 2, BF
        LINE (25, 450)-(((500 - ch1&) / 10) + 25, 455), 4, BF
        COLOR 15
       
        LOCATE 23, 16
        COLOR 14
        PRINT "ENGINES"
        LOCATE 25, 16
        PRINT "Max:"; max&
        IF nw& = 1 THEN
                LOCATE 26, 16
                PRINT "Warp:"
                LOCATE 27, 16
                PRINT "Disabled"
        ELSEIF w& = 0 THEN
                LOCATE 26, 16
                PRINT "Warp:"
                LOCATE 27, 16
                PRINT "Idle    "
        ELSEIF w& = 1 THEN
                LOCATE 26, 16
                PRINT "Warp:"
                LOCATE 27, 16
                PRINT "Acitve  "
        END IF
       
        LOCATE 24, 16
        IF w& = 0 THEN PRINT "Speed:"; po&
        IF w& = 1 THEN PRINT "Speed: -"
        COLOR 15
        LOCATE 23, 29
        COLOR 14
        PRINT "WEAPONS"
        LOCATE 24, 29
        IF mode& = 0 THEN PRINT "Auto Aim"
        IF mode& = 1 THEN PRINT "Manual Aim"
        LOCATE 25, 37
        PRINT tor&
        LOCATE 25, 29
        PRINT "Torpedos:"
        LOCATE 26, 29
        IF fin2& = 1 THEN PRINT "LOCKED ON"
        IF fin2& = 0 THEN PRINT "NO LOCK  "
        LOCATE 27, 29
        PRINT "Lazer:"
        LOCATE 28, 29
        IF fin& = 1 THEN PRINT "LOCKED ON"
        IF fin& = 0 THEN PRINT "NO LOCK  "
        COLOR 15

        LOCATE 23, 41
        COLOR 14
        PRINT "LIFE"
        LOCATE 24, 41
        PRINT "SUPPORT"
        LOCATE 25, 41
        PRINT "Toxins:"; tox&
        LOCATE 26, 41
        PRINT "Detox:"; retox&
        LOCATE 27, 41
        IF tox& > 200 THEN
                IF col& = 14 THEN
                        col& = 4
                ELSEIF col& = 4 THEN
                        col& = 14
                END IF
                COLOR col&
                PRINT "Dangerous "
                LOCATE 28, 41
                PRINT "Toxicity"
                COLOR 15
        ELSEIF tox& < 200 THEN
                COLOR 0
                PRINT "          "
                LOCATE 28, 41
                PRINT "          "
                COLOR 15
        END IF
        COLOR 15

        LOCATE 23, 54
        COLOR 14
        PRINT "REPAIR"
        LOCATE 24, 54
        PRINT "UNITS"; units&
        LOCATE 25, 54
        PRINT "S-SENSORS"
        LOCATE 26, 54
        PRINT "E-ENGINES"
        LOCATE 27, 54
        PRINT "W-WEAPONS"
        LOCATE 28, 54
        PRINT "U-SUPPORT"
        COLOR 15
        

        LOCATE 23, 66
        COLOR 14
        PRINT "DAMMAGE"
        LOCATE 24, 66
        PRINT "SENSORS %"; sen&
        LOCATE 25, 66
        PRINT "ENGINES %"; eng&
        LOCATE 26, 66
        PRINT "WEAPONS %"; wea&
        LOCATE 27, 66
        PRINT "SUPPORT %"; sup&
        LOCATE 28, 66
        PRINT "TOTAL %"
        LOCATE 28, 74
        PRINT CINT((sen& + eng& + wea& + sup&) / 4)
        COLOR 15
        RETURN

drawing:
LINE (0, 331)-(640, 480), 0, BF
LINE (25, 340)-(100, 350), 13, BF
CIRCLE (25, 360), 10, 13, 1.3, 3
CIRCLE (25, 360), 20, 13, 1.3, 3.1
LINE (5, 360)-(15, 360), 13
PAINT (10, 350), 13, 13
LINE (25, 469)-(100, 479), 13, BF
CIRCLE (25, 459), 10, 13, 3.2, 5
CIRCLE (25, 459), 20, 13, 3.2, 5
LINE (5, 459)-(15, 459), 13
PAINT (10, 471), 13, 13
     
LINE (125, 340)-(200, 350), 13, BF
CIRCLE (125, 360), 10, 13, 1.3, 3
CIRCLE (125, 360), 20, 13, 1.3, 3.1
LINE (105, 360)-(115, 360), 13
PAINT (110, 350), 13, 13
LINE (125, 469)-(200, 479), 13, BF
CIRCLE (125, 459), 10, 13, 3.2, 5
CIRCLE (125, 459), 20, 13, 3.2, 5
LINE (105, 459)-(115, 459), 13
PAINT (110, 471), 13, 13

LINE (225, 340)-(300, 350), 13, BF
CIRCLE (225, 360), 10, 13, 1.3, 3
CIRCLE (225, 360), 20, 13, 1.3, 3.1
LINE (205, 360)-(215, 360), 13
PAINT (210, 350), 13, 13
LINE (225, 469)-(300, 479), 13, BF
CIRCLE (225, 459), 10, 13, 3.2, 5
CIRCLE (225, 459), 20, 13, 3.2, 5
LINE (205, 459)-(215, 459), 13
PAINT (210, 471), 13, 13
     
LINE (325, 340)-(400, 350), 13, BF
CIRCLE (325, 360), 10, 13, 1.3, 3
CIRCLE (325, 360), 20, 13, 1.3, 3.1
LINE (305, 360)-(315, 360), 13
PAINT (310, 350), 13, 13
LINE (325, 469)-(400, 479), 13, BF
CIRCLE (325, 459), 10, 13, 3.2, 5
CIRCLE (325, 459), 20, 13, 3.2, 5
LINE (305, 459)-(315, 459), 13
PAINT (310, 471), 13, 13

LINE (425, 340)-(500, 350), 13, BF
CIRCLE (425, 360), 10, 13, 1.3, 3
CIRCLE (425, 360), 20, 13, 1.3, 3.1
LINE (405, 360)-(415, 360), 13
PAINT (410, 350), 13, 13
LINE (425, 469)-(500, 479), 13, BF
CIRCLE (425, 459), 10, 13, 3.2, 5
CIRCLE (425, 459), 20, 13, 3.2, 5
LINE (405, 459)-(415, 459), 13
PAINT (410, 471), 13, 13
      
LINE (525, 340)-(600, 350), 13, BF
CIRCLE (525, 360), 10, 13, 1.3, 3
CIRCLE (525, 360), 20, 13, 1.3, 3.1
LINE (505, 360)-(515, 360), 13
PAINT (510, 350), 13, 13
LINE (525, 469)-(600, 479), 13, BF
CIRCLE (525, 459), 10, 13, 3.2, 5
CIRCLE (525, 459), 20, 13, 3.2, 5
LINE (505, 459)-(515, 459), 13
PAINT (510, 471), 13, 13
RETURN


