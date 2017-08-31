delay& = 1
stars& = 15
DIM stx(1, stars&) AS INTEGER
DIM sty(1, stars&) AS INTEGER
DIM sts(1, stars&) AS INTEGER
start:
SCREEN 12
WIDTH 80, 60
CLS
DO
VIEW SCREEN (50, 50)-(590, 390)
PSET (59, 76), 15
PSET (189, 71), 7
PSET (200, 100), 15
PSET (340, 200), 7
PSET (71, 260), 15
PSET (456, 84), 15
LINE (800, -400)-(800, 400), 2
FOR i = -400 TO 400 STEP 10
LINE (800, i)-(0 - i * 2, 400), 2
NEXT i
LINE (50, 50)-(590, 390), 14, B

COLOR 4
LOCATE 12, 8
PRINT "SPACE ATTACK"
COLOR 9
LOCATE 14, 8
PRINT "By Matthew Seymour"
COLOR 3
LOCATE 18, 9
PRINT "ew game"
LOCATE 20, 9
PRINT "ontinue an already started game"
LOCATE 22, 9
PRINT "ptions"
LOCATE 24, 9
PRINT "ave current game"
LOCATE 26, 9
PRINT "estore saved game"
LOCATE 28, 9
PRINT "uit"
LOCATE 18, 8
COLOR 11
PRINT "N"
LOCATE 20, 8
PRINT "C"
LOCATE 22, 8
PRINT "O"
LOCATE 24, 8
PRINT "S"
LOCATE 26, 8
PRINT "R"
LOCATE 28, 8
PRINT "Q"
COLOR 15
DO
ans3$ = INKEY$
LOOP UNTIL (ans3$ = "o" OR ans3$ = "c" OR ans3$ = "q" OR ans3$ = "n" OR ans3$ = "s" OR ans3$ = "r")
IF ans3$ = "r" THEN GOSUB opener
IF ans3$ = "q" THEN GOTO quit2
IF ans3$ = "s" THEN
        DO
        COLOR 3
        LOCATE 24, 27
        IF started& = 0 THEN
                PRINT "No game started!"
                GOTO start
        END IF
        INPUT "Name: ", name$
        LOOP UNTIL name$ <> ""
        a1$ = ""
        a2$ = ""
        OPEN "SAVED" FOR INPUT AS #1
        IF NOT EOF(1) THEN INPUT #1, a1$
        IF NOT EOF(1) THEN INPUT #1, a2$
        CLOSE #1
        OPEN "SAVED" FOR OUTPUT AS #1
        IF a1$ <> "" THEN WRITE #1, a1$
        IF a2$ <> "" THEN WRITE #1, a2$
        WRITE #1, name$
        CLOSE #1
        OPEN name$ FOR OUTPUT AS #2
        WRITE #2, x&, y&, laz&, lazer&, basex&, basey&, c1x&, c1y&, c2x&, c2y&, c3x&, c3y&, ra&, da&, w&, nw&, comsupermax&, com1max&, com2max&, com3max&, supmax&, senes&, max&, tor&, c1tor&, c2tor&, c3tor&, sen&, eng&, wea&, ene&, bat&, divide&, shields&, tax&, c1h&, c1l&, c2h&, c2l&, c3h&, c3l&, life1&, c1t&, c1tx&, c1ty&, c2t&, c2tx&, c2ty&, c3t&, c3tx&, c3ty&, object&, ma&, m&, po&, c1direction&, c1ddirection&, c2direction&, c2ddirection&, c3direction&, c3ddirection&, c1distance&
        CLOSE #2
        COLOR 0
        LOCATE 24, 27
        PRINT "Name: "; name$
        COLOR 15
END IF


IF ans3$ = "n" THEN GOTO game
IF ans3$ = "o" THEN GOTO options
IF ans3$ = "c" AND started& = 1 THEN
        CLS
        GOSUB drawing
        GOTO begin
END IF
LOOP
game:
CLS
GOSUB drawing
started& = 1
x& = 320
y& = 170
laz& = 0
lazer& = 0
basex& = 600
basey& = 300
RANDOMIZE TIMER
c1x& = INT(RND * 40000) - 20000
c1y& = INT(RND * 40000) - 20000
c2x& = INT(RND * 40000) - 20000
c2y& = INT(RND * 40000) - 20000
c3x& = INT(RND * 40000) - 20000
c3y& = INT(RND * 40000) - 20000
ra& = 1000
da& = 15
w& = 0
nw& = 0
comsupermax& = 4
com1max& = comsupermax&
com2max& = comsupermax&
com3max& = comsupermax&
supmax& = 4
senes& = 1
max& = supmax&
tor& = 5
c1tor& = 5
c2tor& = 5
c3tor& = 5
sen& = 100
eng& = 100
wea& = 100
ene& = 100
bat& = 10000
divide& = 500
shields& = 100
tox& = 0
c1h& = 200
c1l& = 1
c2h& = 200
c2l& = 1
c3h& = 200
c3l& = 1
life1& = 1
c1t& = 0
c1tx& = c1x&
c1ty& = c1y&
c2t& = 0
c2tx& = c1x&
c2ty& = c1y&
c3t& = 0
c3tx& = c1x&
c3ty& = c1y&
object& = 1
md& = 0
fixbug& = 20
RANDOMIZE TIMER
m& = INT(RND * 7) + 1
po& = INT(RND * max&)
c1direction& = INT(RND * comsupermax&) + 1
c1ddirection& = c1direction&
c2direction& = INT(RND * comsupermax&) + 1
c2ddirection& = c1direction&
c3direction& = INT(RND * comsupermax&) + 1
c3ddirection& = c1direction&

GOSUB ran
GOTO begin
ran:
REDIM stx(1, stars&) AS INTEGER
REDIM sty(1, stars&) AS INTEGER
REDIM sts(1, stars&) AS INTEGER
FOR i = 1 TO stars&
RANDOMIZE TIMER
stx(1, i) = INT(RND * 640) + 1
sty(1, i) = INT(RND * 330) + 1
sts(1, i) = INT(RND * 4) + 1
IF sts(1, i) = 1 THEN sts(1, i) = 1
IF sts(1, i) = 2 THEN sts(1, i) = 2
IF sts(1, i) = 3 THEN sts(1, i) = 3
IF sts(1, i) = 4 THEN sts(1, i) = 4
NEXT i
c1distance& = INT(RND * 75) + 25
RETURN

begin:
        DO
        DO
        VIEW SCREEN (1, 2)-(638, 329)
        FOR i = 1 TO stars&
        IF w& = 1 THEN GOTO warp
        PSET (stx(1, i), sty(1, i)), 15
        IF po& > 0 THEN LINE (stx(1, i) - (po& * sts(1, i)), sty(1, i) - (po& / 2) * sts(1, i))-(stx(1, i) + po& * sts(1, i), sty(1, i) + (po& / 2) * sts(1, i)), 0, B
        IF po& = 1 THEN LINE (stx(1, i) - (po& * sts(1, i)), sty(1, i) - po& * sts(1, i))-(stx(1, i) + po& * sts(1, i), sty(1, i) + po& * sts(1, i)), 0, B
        NEXT i
con6:
        IF fixbug& > -1 THEN fixbug& = fixbug& - 1
        IF fixbug& = 0 THEN CLS

        FOR i = 1 TO delay&
        NEXT i
        IF life1& = 0 THEN GOTO youlose
        IF c1l& = 0 THEN GOSUB comlose
        IF c2l& = 0 THEN GOSUB comlose
        IF c3l& = 0 THEN GOSUB comlose


'        shieldregen& = shieldregen& + 1
'        IF shieldregen& = 50 THEN
'                shieldregen& = 0
'                shields& = shields& + 1
'                IF shields& > 100 THEN shields& = 100
'        END IF
     
        IF sen& < 0 THEN sen& = 0
        IF eng& < 0 THEN eng& = 0
        IF wea& < 0 THEN wea& = 0
        IF ene& < 0 THEN ene& = 0
       
        GOSUB randoom
        GOSUB finder
        GOSUB c1finder
        GOSUB c1finder2
        GOSUB c2finder
        GOSUB c2finder2
        GOSUB c3finder
        GOSUB c3finder2
        VIEW
        LINE (0, 1)-(639, 330), 14, B
        GOSUB sensors
        GOSUB engines
        GOSUB weapons
        GOSUB lifesupport
        GOSUB shields
        GOSUB dammage
        IF senes& = 0 AND bat& > 0 THEN
                senes& = 1
                GOSUB drawing
        END IF
        VIEW SCREEN (1, 2)-(638, 329)
       
        LINE (c1x& - 12, c1y& - 12)-(c1x& + 12, c1y& + 12), 0, BF
        LINE (c2x& - 12, c2y& - 12)-(c2x& + 12, c2y& + 12), 0, BF
        LINE (c3x& - 12, c3y& - 12)-(c3x& + 12, c3y& + 12), 0, BF
        IF fin2& = 1 THEN LINE (c1x& - 5, c1y& - 5)-(c1x& + 5, c1y& + 5), 0, B
       
        LINE (lazx& - 15, lazy& - 15)-(lazx& + 15, lazy& + 15), 0, BF

        IF findcom1tor& = 1 THEN
                LINE (c1x&, c1y&)-(x&, y&), 0
                LINE (c1x& + 1, c1y& + 1)-(x&, y&), 0
                LINE (c1x& - 1, c1y& - 1)-(x&, y&), 0
        END IF
        IF findcom2tor& = 1 THEN
                LINE (c2x&, c2y&)-(x&, y&), 0
                LINE (c2x& + 1, c2y& + 1)-(x&, y&), 0
                LINE (c2x& - 1, c2y& - 1)-(x&, y&), 0
        END IF
        IF findcom3tor& = 1 THEN
                LINE (c3x&, c3y&)-(x&, y&), 0
                LINE (c3x& + 1, c3y& + 1)-(x&, y&), 0
                LINE (c3x& - 1, c3y& - 1)-(x&, y&), 0
        END IF
        IF c1direction& = 2 THEN
                LINE (c1x&, c1y& + 1)-(c1x&, (c1y& + 1) + com1speed&), 0
        ELSEIF c1direction& = 1 THEN
                LINE (c1x& + 2, c1y& + 1)-((c1x& + 2) + com1speed& * 2, (c1y& + 1) + com1speed&), 0
        ELSEIF c1direction& = 8 THEN
                LINE (c1x& + 2, c1y&)-((c1x& + 2) + com1speed& * 2, c1y&), 0
        ELSEIF c1direction& = 7 THEN
                LINE (c1x& + 2, c1y& - 1)-((c1x& + 2) + com1speed& * 2, (c1y& - 1) - com1speed&), 0
        ELSEIF c1direction& = 6 THEN
                LINE (c1x&, c1y& - 2)-(c1x&, (c1y& - 2) - com1speed&), 0
        ELSEIF c1direction& = 5 THEN
                LINE (c1x& - 2, c1y& - 1)-((c1x& - 2) - com1speed& * 2, (c1y& - 1) - com1speed&), 0
        ELSEIF c1direction& = 4 THEN
                LINE (c1x& - 2, c1y&)-(c1x& - com1speed& * 2, c1y&), 0
        ELSEIF c1direction& = 3 THEN
                LINE (c1x& - 2, c1y& + 1)-(c1x& - com1speed& * 2, c1y& + com1speed&), 0
        END IF
        IF c2direction& = 2 THEN
                LINE (c2x&, c2y& + 1)-(c2x&, (c2y& + 1) + com2speed&), 0
        ELSEIF c2direction& = 1 THEN
                LINE (c2x& + 2, c2y& + 1)-((c2x& + 2) + com2speed& * 2, (c2y& + 1) + com2speed&), 0
        ELSEIF c2direction& = 8 THEN
                LINE (c2x& + 2, c2y&)-((c2x& + 2) + com2speed& * 2, c2y&), 0
        ELSEIF c2direction& = 7 THEN
                LINE (c2x& + 2, c2y& - 1)-((c2x& + 2) + com2speed& * 2, (c2y& - 1) - com2speed&), 0
        ELSEIF c2direction& = 6 THEN
                LINE (c2x&, c2y& - 2)-(c2x&, (c2y& - 2) - com2speed&), 0
        ELSEIF c2direction& = 5 THEN
                LINE (c2x& - 2, c2y& - 1)-((c2x& - 2) - com2speed& * 2, (c2y& - 1) - com2speed&), 0
        ELSEIF c2direction& = 4 THEN
                LINE (c2x& - 2, c2y&)-(c2x& - com2speed& * 2, c2y&), 0
        ELSEIF c2direction& = 3 THEN
                LINE (c2x& - 2, c2y& + 1)-(c2x& - com2speed& * 2, c2y& + com2speed&), 0
        END IF
        IF c3direction& = 2 THEN
                LINE (c3x&, c3y& + 1)-(c3x&, (c3y& + 1) + com3speed&), 0
        ELSEIF c3direction& = 1 THEN
                LINE (c3x& + 2, c3y& + 1)-((c3x& + 2) + com3speed& * 2, (c3y& + 1) + com3speed&), 0
        ELSEIF c3direction& = 8 THEN
                LINE (c3x& + 2, c3y&)-((c3x& + 2) + com3speed& * 2, c3y&), 0
        ELSEIF c3direction& = 7 THEN
                LINE (c3x& + 2, c3y& - 1)-((c3x& + 2) + com3speed& * 2, (c3y& - 1) - com3speed&), 0
        ELSEIF c3direction& = 6 THEN
                LINE (c3x&, c3y& - 2)-(c3x&, (c3y& - 2) - com3speed&), 0
        ELSEIF c3direction& = 5 THEN
                LINE (c3x& - 2, c3y& - 1)-((c3x& - 2) - com3speed& * 2, (c3y& - 1) - com3speed&), 0
        ELSEIF c3direction& = 4 THEN
                LINE (c3x& - 2, c3y&)-(c3x& - com3speed& * 2, c3y&), 0
        ELSEIF c3direction& = 3 THEN
                LINE (c3x& - 2, c3y& + 1)-(c3x& - com3speed& * 2, c3y& + com3speed&), 0
        END IF
        CIRCLE (c1tx&, c1ty&), 1, 0
        CIRCLE (c2tx&, c2ty&), 1, 0
        CIRCLE (c3tx&, c3ty&), 1, 0
        CIRCLE (tx&, ty&), 1, 0
        LINE (basex& - 15, basey& - 15)-(basex& + 15, basey& + 15), 0, BF
       
        IF c1distance& = 0 THEN
                RANDOMIZE TIMER
                c1distance& = INT(RND * 75) + 25
                IF w& = 0 THEN com1speed& = INT(RND * (comsupermax& + 1)) ELSE com1speed& = com1max&
                IF com1speed& > com1max& THEN com1speed& = com1max&
                diangle& = INT(RND * 2) + 1
                distx& = x& - c1x&
                disty& = y& - c1y&
                IF c1direction& = c1ddirection& THEN
                        IF distx& > 0 AND disty& > 0 THEN
                                IF distx& > disty& THEN c1ddirection& = 4 ELSE c1ddirection& = 6
                                IF diangle& = 1 THEN c1ddirection& = 5
                        ELSEIF distx& < 0 AND disty& < 0 THEN
                                IF distx& < disty& THEN c1ddirection& = 8 ELSE c1ddirection& = 2
                                IF diangle& = 1 THEN c1ddirection& = 1
                        ELSEIF distx& < 0 AND disty& > 0 THEN
                                IF distx& < -disty& THEN c1ddirection& = 8 ELSE c1ddirection& = 6
                                IF diangle& = 1 THEN c1ddirection& = 7
                        ELSEIF distx& > 0 AND disty& < 0 THEN
                                IF -distx& < disty& THEN c1ddirection& = 4 ELSE c1ddirection& = 2
                                IF diangle& = 1 THEN c1ddirection& = 3
                        END IF
                END IF
                IF c1direction& <> c1ddirection& THEN
                        IF c1direction& - c1ddirection& > 3 AND c1direction& - c1ddirection& < 9 THEN c1direction& = c1direction& + 1
                        IF c1direction& - c1ddirection& < 4 AND c1direction& - c1ddirection& > -1 THEN c1direction& = c1direction& - 1
                        IF c1direction& - c1ddirection& > -4 AND c1direction& - c1ddirection& < 0 THEN c1direction& = c1direction& + 1
                        IF c1direction& - c1ddirection& > -8 AND c1direction& - c1ddirection& < -3 THEN c1direction& = c1direction& - 1
                        IF c1direction& < 1 THEN c1direction& = 8
                        IF c1direction& > 8 THEN c1direction& = 1
                END IF
        END IF
       
        c1distance& = c1distance& - 1
       
        IF c1direction& = 1 THEN
                c1x& = c1x& - com1speed& * 2
                c1y& = c1y& - com1speed&
        ELSEIF c1direction& = 2 THEN
                c1y& = c1y& - com1speed&
        ELSEIF c1direction& = 3 THEN
                c1x& = c1x& + com1speed& * 2
                c1y& = c1y& - com1speed&
        ELSEIF c1direction& = 4 THEN
                c1x& = c1x& + com1speed& * 2
        ELSEIF c1direction& = 5 THEN
                c1x& = c1x& + com1speed& * 2
                c1y& = c1y& + com1speed&
        ELSEIF c1direction& = 6 THEN
                c1y& = c1y& + com1speed&
        ELSEIF c1direction& = 7 THEN
                c1x& = c1x& - com1speed& * 2
                c1y& = c1y& + com1speed&
        ELSEIF c1direction& = 8 THEN
                c1x& = c1x& - com1speed& * 2
        END IF
       
        IF c2distance& = 0 THEN
                RANDOMIZE TIMER
                c2distance& = INT(RND * 75) + 25
                IF w& = 0 THEN com2speed& = INT(RND * (comsupermax& + 1)) ELSE com2speed& = com2max&
                IF com2speed& > com2max& THEN com2speed& = com2max&
                diangle& = INT(RND * 2) + 1
                distx& = x& - c2x&
                disty& = y& - c2y&
                IF c2direction& = c2ddirection& THEN
                        IF distx& > 0 AND disty& > 0 THEN
                                IF distx& > disty& THEN c2ddirection& = 4 ELSE c2ddirection& = 6
                                IF diangle& = 1 THEN c2ddirection& = 5
                        ELSEIF distx& < 0 AND disty& < 0 THEN
                                IF distx& < disty& THEN c2ddirection& = 8 ELSE c2ddirection& = 2
                                IF diangle& = 1 THEN c2ddirection& = 1
                        ELSEIF distx& < 0 AND disty& > 0 THEN
                                IF distx& < -disty& THEN c2ddirection& = 8 ELSE c2ddirection& = 6
                                IF diangle& = 1 THEN c2ddirection& = 7
                        ELSEIF distx& > 0 AND disty& < 0 THEN
                                IF -distx& < disty& THEN c2ddirection& = 4 ELSE c2ddirection& = 2
                                IF diangle& = 1 THEN c2ddirection& = 3
                        END IF
                END IF
                IF c2direction& <> c2ddirection& THEN
                        IF c2direction& - c2ddirection& > 3 AND c2direction& - c2ddirection& < 9 THEN c2direction& = c2direction& + 1
                        IF c2direction& - c2ddirection& < 4 AND c2direction& - c2ddirection& > -1 THEN c2direction& = c2direction& - 1
                        IF c2direction& - c2ddirection& > -4 AND c2direction& - c2ddirection& < 0 THEN c2direction& = c2direction& + 1
                        IF c2direction& - c2ddirection& > -8 AND c2direction& - c2ddirection& < -3 THEN c2direction& = c2direction& - 1
                        IF c2direction& < 1 THEN c2direction& = 8
                        IF c2direction& > 8 THEN c2direction& = 1
                END IF
        END IF
        c2distance& = c2distance& - 1
      
        IF c2direction& = 1 THEN
                c2x& = c2x& - com2speed& * 2
                c2y& = c2y& - com2speed&
        ELSEIF c2direction& = 2 THEN
                c2y& = c2y& - com2speed&
        ELSEIF c2direction& = 3 THEN
                c2x& = c2x& + com2speed& * 2
                c2y& = c2y& - com2speed&
        ELSEIF c2direction& = 4 THEN
                c2x& = c2x& + com2speed& * 2
        ELSEIF c2direction& = 5 THEN
                c2x& = c2x& + com2speed& * 2
                c2y& = c2y& + com2speed&
        ELSEIF c2direction& = 6 THEN
                c2y& = c2y& + com2speed&
        ELSEIF c2direction& = 7 THEN
                c2x& = c2x& - com2speed& * 2
                c2y& = c2y& + com2speed&
        ELSEIF c2direction& = 8 THEN
                c2x& = c2x& - com2speed& * 2
        END IF
        IF c3distance& = 0 THEN
                RANDOMIZE TIMER
                c3distance& = INT(RND * 75) + 25
                IF w& = 0 THEN com3speed& = INT(RND * (comsupermax& + 1)) ELSE com3speed& = com3max&
                IF com3speed& > 8 THEN com3speed& = com3max&
                diangle& = INT(RND * 2) + 1
                distx& = x& - c3x&
                disty& = y& - c3y&
                IF c3direction& = c3ddirection& THEN
                        IF distx& > 0 AND disty& > 0 THEN
                                IF distx& > disty& THEN c3ddirection& = 4 ELSE c3ddirection& = 6
                                IF diangle& = 1 THEN c3ddirection& = 5
                        ELSEIF distx& < 0 AND disty& < 0 THEN
                                IF distx& < disty& THEN c3ddirection& = 8 ELSE c3ddirection& = 2
                                IF diangle& = 1 THEN c3ddirection& = 1
                        ELSEIF distx& < 0 AND disty& > 0 THEN
                                IF distx& < -disty& THEN c3ddirection& = 8 ELSE c3ddirection& = 6
                                IF diangle& = 1 THEN c3ddirection& = 7
                        ELSEIF distx& > 0 AND disty& < 0 THEN
                                IF -distx& < disty& THEN c3ddirection& = 4 ELSE c3ddirection& = 2
                                IF diangle& = 1 THEN c3ddirection& = 3
                        END IF
                END IF
                IF c3direction& <> c3ddirection& THEN
                        IF c3direction& - c3ddirection& > 3 AND c3direction& - c3ddirection& < 9 THEN c3direction& = c3direction& + 1
                        IF c3direction& - c3ddirection& < 4 AND c3direction& - c3ddirection& > -1 THEN c3direction& = c3direction& - 1
                        IF c3direction& - c3ddirection& > -4 AND c3direction& - c3ddirection& < 0 THEN c3direction& = c3direction& + 1
                        IF c3direction& - c3ddirection& > -8 AND c3direction& - c3ddirection& < -3 THEN c3direction& = c3direction& - 1
                        IF c3direction& < 1 THEN c3direction& = 8
                        IF c3direction& > 8 THEN c3direction& = 1
                END IF
        END IF
        c3distance& = c3distance& - 1
      
        IF c3direction& = 1 THEN
                c3x& = c3x& - com3speed& * 2
                c3y& = c3y& - com3speed&
        ELSEIF c3direction& = 2 THEN
                c3y& = c3y& - com3speed&
        ELSEIF c3direction& = 3 THEN
                c3x& = c3x& + com3speed& * 2
                c3y& = c3y& - com3speed&
        ELSEIF c3direction& = 4 THEN
                c3x& = c3x& + com3speed& * 2
        ELSEIF c3direction& = 5 THEN
                c3x& = c3x& + com3speed& * 2
                c3y& = c3y& + com3speed&
        ELSEIF c3direction& = 6 THEN
                c3y& = c3y& + com3speed&
        ELSEIF c3direction& = 7 THEN
                c3x& = c3x& - com3speed& * 2
                c3y& = c3y& + com3speed&
        ELSEIF c3direction& = 8 THEN
                c3x& = c3x& - com3speed& * 2
        END IF

        IF object2& = 4 THEN object& = 4
        IF object& = 4 THEN
                object2& = 4
                cc1x& = c1x&
                cc1y& = c1y&
                cc2x& = c2x&
                cc2y& = c2y&
                cc3x& = c3x&
                cc3y& = c3y&
                IF c1x& > 0 THEN cc1x& = -c1x&
                IF c1x& > 0 THEN cc1x& = -c1x&
                IF c2x& > 0 THEN cc2x& = -c2x&
                IF c2x& > 0 THEN cc2x& = -c2x&
                IF c3x& > 0 THEN cc3x& = -c3x&
                IF c3x& > 0 THEN cc3x& = -c3x&
                IF cc1x& + cc1y& < cc2x& + cc2y& AND cc1x& + cc1y& < cc3x& + cc3y& THEN object& = 1
                IF cc2x& + cc2y& < cc1x& + cc1y& AND cc2x& + cc2y& < cc3x& + cc3y& THEN object& = 2
                IF cc3x& + cc3y& < cc1x& + cc1y& AND cc3x& + cc3y& < cc2x& + cc2y& THEN object& = 3
        END IF
        IF object& = 1 THEN
                ox& = c1x&
                oy& = c1y&
                oh& = c1h&
                ol& = c1l&
        ELSEIF object& = 2 THEN
                ox& = c2x&
                oy& = c2y&
                oh& = c2h&
                ol& = c2l&
        ELSEIF object& = 3 THEN
                ox& = c3x&
                oy& = c3y&
                oh& = c3h&
                ol& = c3l&
        END IF
       
        IF showshields& = 0 THEN LINE (x& - 15, y& - 15)-(x& + 15, y& + 15), 0, BF
        RANDOMIZE TIMER
        coo& = INT(RND * 2) + 1
        IF coo& = 2 THEN coo& = 9
        IF m& = 1 THEN
                LINE (x&, y& - 1)-(x&, y& - 2), 8
                LINE (x& - 1, y&)-(x& - 3, y&), 15
                LINE (x& - 3, y& + 1)-(x& - 5, y& + 1), 15
                LINE (x& + 1, y&)-(x& + 3, y&), 8
                LINE (x& + 3, y& + 1)-(x& + 5, y& + 1), 8
                LINE (x& + 1, y& - 1)-(x& + 1, y& - 2), 8
                LINE (x& - 1, y& - 1)-(x&, y& - 4), 15
                PSET (x&, y& - 2), 9
                LINE (x&, y& + 1)-(x&, y& + 12), 0
                IF w& = 1 THEN LINE (x&, y& + 1)-(x&, y& + 10), coo&
                IF w& = 0 THEN LINE (x&, y& + 1)-(x&, (y& + 1) + po&), coo&
                GOSUB yup
        ELSEIF m& = 2 THEN
                LINE (x&, y&)-(x& - 4, y& - 2), 15
                LINE (x& - 3, y& - 2)-(x& - 1, y& - 2), 9
                LINE (x& - 1, y& - 1)-(x& + 1, y& - 1), 15
                PSET (x& + 1, y& - 2), 15
                LINE (x& - 1, y&)-(x& - 2, y&), 15
                LINE (x& - 2, y& + 1)-(x& - 5, y& + 2), 15
                LINE (x& + 1, y&)-(x& + 3, y&), 8
                LINE (x& + 4, y& + 1)-(x& + 5, y& + 1), 8
                PSET (x& - 4, y& + 2), 15
                LINE (x& + 2, y& + 1)-(x& + 22, y& + 12), 0
                IF w& = 1 THEN LINE (x& + 2, y& + 1)-(x& + 20, y& + 10), coo&
                IF w& = 0 THEN LINE (x& + 2, y& + 1)-((x& + 2) + po& * 2, (y& + 1) + po&), coo&
                GOSUB yup
                GOSUB xup
        ELSEIF m& = 3 THEN
                LINE (x& + 1, y&)-(x& - 5, y&), 15
                LINE (x&, y& - 1)-(x& - 1, y& - 1), 15
                LINE (x& - 2, y& - 1)-(x& - 3, y& - 1), 9
                LINE (x&, y&)-(x&, y& - 2), 15
                LINE (x&, y&)-(x&, y& + 3), 15
                PSET (x& - 1, y& + 1), 15
                PSET (x& - 1, y& - 2), 15
                LINE (x&, y& - 3)-(x&, y& - 4), 15
                LINE (x&, y&)-(x& + 1, y&), 8
                PSET (x&, y& - 1), 8
                LINE (x& + 2, y&)-(x& + 22, y&), 0
                IF w& = 1 THEN LINE (x& + 2, y&)-(x& + 20, y&), coo&
                IF w& = 0 THEN LINE (x& + 2, y&)-((x& + 2) + po& * 2, y&), coo&
                GOSUB xup
        ELSEIF m& = 4 THEN
                LINE (x& - 1, y& - 1)-(x& - 3, y& + 1), 9
                LINE (x& + 1, y& - 3)-(x& + 1, y& + 1), 8
                LINE (x&, y&)-(x&, y& - 2), 8
                PSET (x& - 1, y&), 8
                LINE (x& - 1, y& + 1)-(x& - 2, y& + 1), 8
                LINE (x& - 3, y& + 2)-(x& - 4, y& + 2), 8
                LINE (x& + 2, y&)-(x& + 2, y& + 1), 8
                LINE (x& + 3, y& + 1)-(x& + 3, y& + 2), 8
                LINE (x& + 4, y& + 2)-(x& + 4, y& + 3), 8
                LINE (x& - 1, y& - 2)-(x& - 1, y& - 3), 15
                LINE (x& - 2, y& - 2)-(x& - 2, y& - 4), 15
                PSET (x& - 3, y& - 4), 15
                PSET (x& - 4, y& + 2), 15
                LINE (x&, y& - 2)-(x& + 1, y& - 3), 15
                LINE (x& + 2, y& - 1)-(x& + 22, y& - 12), 0
                IF w& = 1 THEN LINE (x& + 2, y& - 1)-(x& + 20, y& - 10), coo&
                IF w& = 0 THEN LINE (x& + 2, y& - 1)-((x& + 2) + po& * 2, (y& - 1) - po&), coo&
                GOSUB ydown
                GOSUB xup
        ELSEIF m& = 5 THEN
                LINE (x& - 4, y& + 1)-(x& - 5, y& + 1), 15
                LINE (x& + 4, y& + 1)-(x& + 5, y& + 1), 8
                LINE (x& - 3, y&)-(x& - 1, y&), 15
                PSET (x& - 1, y& + 2)
                LINE (x& - 2, y& + 1)-(x& - 3, y& + 1), 15
                LINE (x& + 2, y& + 1)-(x& + 3, y& + 1), 8
                LINE (x& - 1, y& + 1)-(x&, y& + 4), 15
                LINE (x&, y& + 1)-(x&, y& + 2), 9
                LINE (x&, y&)-(x&, y& - 1), 15
                LINE (x& + 1, y& + 1)-(x& + 1, y& + 2), 8
                LINE (x& + 1, y&)-(x& + 3, y&), 8
                LINE (x&, y& - 2)-(x&, y& - 12), 0
                IF w& = 1 THEN LINE (x&, y& - 2)-(x&, y& - 10), coo&
                IF w& = 0 THEN LINE (x&, y& - 2)-(x&, (y& - 2) - po&), coo&
                GOSUB ydown
        ELSEIF m& = 6 THEN
                LINE (x& - 1, y& - 2)-(x& - 1, y& + 1), 15
                LINE (x& - 2, y&)-(x& - 2, y& + 1), 15
                LINE (x&, y&)-(x&, y& - 2), 15
                PSET (x& + 1, y&), 15
                LINE (x& + 1, y& + 1)-(x& + 2, y& + 1), 15
                LINE (x& + 3, y& + 2)-(x& + 4, y& + 2), 15
                LINE (x& + 2, y&)-(x& + 2, y& + 1), 15
                LINE (x& - 3, y& + 1)-(x& - 3, y& + 2), 15
                LINE (x& - 4, y& + 2)-(x& - 4, y& + 3), 15
                LINE (x& + 1, y& - 2)-(x& + 1, y& - 3), 8
                LINE (x& + 2, y& - 2)-(x& + 2, y& - 4), 8
                PSET (x& + 3, y& - 4), 8
                PSET (x&, y& - 2), 8
                PSET (x& + 1, y&), 9
                LINE (x& + 1, y& - 1)-(x& + 3, y& + 1), 9
                LINE (x& - 2, y& - 1)-(x& - 22, y& - 12), 0
                IF w& = 1 THEN LINE (x& - 2, y& - 1)-(x& - 20, y& - 10), coo&
                IF w& = 0 THEN LINE (x& - 2, y& - 1)-((x& - 2) - po& * 2, (y& - 1) - po&), coo&
                GOSUB ydown
                GOSUB xdown
        ELSEIF m& = 7 THEN
                LINE (x&, y&)-(x& + 5, y&), 15
                PSET (x& - 1, y&), 15
                LINE (x& + 4, y&)-(x& + 5, y&), 15
                LINE (x&, y& - 1)-(x&, y& - 4), 15
                LINE (x& + 2, y& - 1)-(x& + 3, y& - 1), 9
                LINE (x&, y&)-(x&, y& + 3), 15
                PSET (x& + 1, y& + 1), 15
                PSET (x& + 1, y& - 2), 15
                PSET (x& + 1, y& - 1), 15
                LINE (x& - 2, y&)-(x& - 22, y&), 0
                IF w& = 1 THEN LINE (x& - 2, y&)-(x& - 20, y&), coo&
                IF w& = 0 THEN LINE (x& - 2, y&)-(x& - po& * 2, y&), coo&
                GOSUB xdown
        ELSEIF m& = 8 THEN
                LINE (x&, y&)-(x& + 3, y&), 8
                LINE (x& + 3, y& - 2)-(x& + 1, y& - 2), 9
                LINE (x& + 2, y& + 1)-(x& + 4, y& + 1), 8
                PSET (x& + 4, y& - 2), 15
                LINE (x& - 1, y& - 1)-(x& + 3, y& - 1), 15
                LINE (x& + 4, y& + 2)-(x& + 5, y& + 2), 8
                LINE (x& - 1, y&)-(x& - 3, y&), 15
                LINE (x& - 4, y& + 1)-(x& - 5, y& + 1), 15
                PSET (x& - 1, y& - 2), 15
                LINE (x& - 2, y& + 1)-(x& - 22, y& + 12), 0
                IF w& = 1 THEN LINE (x& - 2, y& + 1)-(x& - 20, y& + 10), coo&
                IF w& = 0 THEN LINE (x& - 2, y& + 1)-(x& - po& * 2, y& + po&), coo&
                GOSUB yup
                GOSUB xdown
        END IF
        PSET (basex&, basey&), 4
        FOR i = 1 TO 10
        CIRCLE (basex&, basey&), i, 8
        NEXT i
        LINE (basex& + 3, basey& - 15)-(basex& - 3, basey& - 8), 8, BF
        LINE (basex& + 3, basey& + 15)-(basex& - 3, basey& + 8), 8, BF
        LINE (basex& - 15, basey& + 3)-(basex& - 8, basey& - 3), 8, BF
        LINE (basex& + 15, basey& + 3)-(basex& + 8, basey& - 3), 8, BF
       
        IF showshields& = 0 THEN LINE (x& - 15, y& - 15)-(x& + 15, y& + 15), 0, BF
        showshields& = showshields& - 1
        IF showshields& < -1 THEN showshields& = -1
        IF showshields& > 0 THEN
                FOR i = 1 TO 10
                        RANDOMIZE TIMER
                        sx& = INT(RND * 30) + x& - 15
                        sy& = INT(RND * 30) + y& - 15
                        PSET (sx&, sy&), 1
                        CIRCLE (x&, y&), i + 9, 0
                        CIRCLE (x&, y& + 1), i + 9, 0
                NEXT i
        END IF
        IF c1direction& = 2 THEN
                LINE (c1x&, c1y& - 1)-(c1x&, c1y& - 2), 0
                LINE (c1x& - 1, c1y&)-(c1x& - 3, c1y&), 8
                LINE (c1x& - 3, c1y& + 1)-(c1x& - 5, c1y& + 1), 8
                LINE (c1x& + 1, c1y&)-(c1x& + 3, c1y&), 0
                LINE (c1x& + 3, c1y& + 1)-(c1x& + 5, c1y& + 1), 0
                LINE (c1x& + 1, c1y& - 1)-(c1x& + 1, c1y& - 2), 0
                LINE (c1x& - 1, c1y& - 1)-(c1x&, c1y& - 4), 0
                PSET (c1x&, c1y& - 2), 9
                LINE (c1x&, c1y& + 1)-(c1x&, c1y& + 12), 0
                LINE (c1x&, c1y& + 1)-(c1x&, (c1y& + 1) + com1speed&), coo&
        ELSEIF c1direction& = 1 THEN
                LINE (c1x&, c1y&)-(c1x& - 4, c1y& - 2), 8
                LINE (c1x& - 3, c1y& - 2)-(c1x& - 1, c1y& - 2), 9
                LINE (c1x& - 1, c1y& - 1)-(c1x& + 1, c1y& - 1), 8
                PSET (c1x& + 1, c1y& - 2), 8
                LINE (c1x& - 1, c1y&)-(c1x& - 2, c1y&), 8
                LINE (c1x& - 2, c1y& + 1)-(c1x& - 5, c1y& + 2), 8
                LINE (c1x& + 1, c1y&)-(c1x& + 3, c1y&), 0
                LINE (c1x& + 4, c1y& + 1)-(c1x& + 5, c1y& + 1), 0
                PSET (c1x& - 4, c1y& + 2), 8
                LINE (c1x& + 2, c1y& + 1)-(c1x& + 22, c1y& + 12), 0
                LINE (c1x& + 2, c1y& + 1)-((c1x& + 2) + com1speed& * 2, (c1y& + 1) + com1speed&), coo&
        ELSEIF c1direction& = 8 THEN
                LINE (c1x& + 1, c1y&)-(c1x& - 5, c1y&), 8
                LINE (c1x&, c1y& - 1)-(c1x& - 1, c1y& - 1), 8
                LINE (c1x& - 2, c1y& - 1)-(c1x& - 3, c1y& - 1), 9
                LINE (c1x&, c1y&)-(c1x&, c1y& - 2), 8
                LINE (c1x&, c1y&)-(c1x&, c1y& + 3), 8
                PSET (c1x& - 1, c1y& + 1), 8
                PSET (c1x& - 1, c1y& - 2), 8
                LINE (c1x&, c1y& - 3)-(c1x&, c1y& - 4), 8
                LINE (c1x&, c1y&)-(c1x& + 1, c1y&), 0
                PSET (c1x&, c1y& - 1), 0
                LINE (c1x& + 2, c1y&)-(c1x& + 22, c1y&), 0
                LINE (c1x& + 2, c1y&)-((c1x& + 2) + com1speed& * 2, c1y&), coo&
        ELSEIF c1direction& = 7 THEN
                LINE (c1x& - 1, c1y& - 1)-(c1x& - 3, c1y& + 1), 9
                LINE (c1x& + 1, c1y& - 3)-(c1x& + 1, c1y& + 1), 0
                LINE (c1x&, c1y&)-(c1x&, c1y& - 2), 0
                PSET (c1x& - 1, c1y&), 0
                LINE (c1x& - 1, c1y& + 1)-(c1x& - 2, c1y& + 1), 0
                LINE (c1x& - 3, c1y& + 2)-(c1x& - 4, c1y& + 2), 0
                LINE (c1x& + 2, c1y&)-(c1x& + 2, c1y& + 1), 0
                LINE (c1x& + 3, c1y& + 1)-(c1x& + 3, c1y& + 2), 0
                LINE (c1x& + 4, c1y& + 2)-(c1x& + 4, c1y& + 3), 0
                LINE (c1x& - 1, c1y& - 2)-(c1x& - 1, c1y& - 3), 8
                LINE (c1x& - 2, c1y& - 2)-(c1x& - 2, c1y& - 4), 8
                PSET (c1x& - 3, c1y& - 4), 8
                PSET (c1x& - 4, c1y& + 2), 8
                LINE (c1x&, c1y& - 2)-(c1x& + 1, c1y& - 3), 8
                LINE (c1x& + 2, c1y& - 1)-(c1x& + 22, c1y& - 12), 0
                LINE (c1x& + 2, c1y& - 1)-((c1x& + 2) + com1speed& * 2, (c1y& - 1) - com1speed&), coo&
        ELSEIF c1direction& = 6 THEN
                LINE (c1x& - 4, c1y& + 1)-(c1x& - 5, c1y& + 1), 8
                LINE (c1x& + 4, c1y& + 1)-(c1x& + 5, c1y& + 1), 0
                LINE (c1x& - 3, c1y&)-(c1x& - 1, c1y&), 8
                PSET (c1x& - 1, c1y& + 2), 8
                LINE (c1x& - 2, c1y& + 1)-(c1x& - 3, c1y& + 1), 8
                LINE (c1x& + 2, c1y& + 1)-(c1x& + 3, c1y& + 1), 0
                LINE (c1x& - 1, c1y& + 1)-(c1x&, c1y& + 4), 8
                LINE (c1x&, c1y& + 1)-(c1x&, c1y& + 2), 0
                LINE (c1x&, c1y&)-(c1x&, c1y& - 1), 8
                LINE (c1x& + 1, c1y& + 1)-(c1x& + 1, c1y& + 2), 0
                LINE (c1x& + 1, c1y&)-(c1x& + 3, c1y&), 0
                LINE (c1x&, c1y& - 2)-(c1x&, c1y& - 12), 0
                LINE (c1x&, c1y& - 2)-(c1x&, (c1y& - 2) - com1speed&), coo&
        ELSEIF c1direction& = 5 THEN
                LINE (c1x& - 1, c1y& - 2)-(c1x& - 1, c1y& + 1), 8
                LINE (c1x& - 2, c1y&)-(c1x& - 2, c1y& + 1), 8
                LINE (c1x&, c1y&)-(c1x&, c1y& - 2), 8
                PSET (c1x& + 1, c1y&), 8
                LINE (c1x& + 1, c1y& + 1)-(c1x& + 2, c1y& + 1), 8
                LINE (c1x& + 3, c1y& + 2)-(c1x& + 4, c1y& + 2), 8
                LINE (c1x& + 2, c1y&)-(c1x& + 2, c1y& + 1), 8
                LINE (c1x& - 3, c1y& + 1)-(c1x& - 3, c1y& + 2), 8
                LINE (c1x& - 4, c1y& + 2)-(c1x& - 4, c1y& + 3), 8
                LINE (c1x& + 1, c1y& - 2)-(c1x& + 1, c1y& - 3), 0
                LINE (c1x& + 2, c1y& - 2)-(c1x& + 2, c1y& - 4), 0
                PSET (c1x& + 3, c1y& - 4), 0
                PSET (c1x&, c1y& - 2), 0
                PSET (c1x& + 1, c1y&), 9
                LINE (c1x& + 1, c1y& - 1)-(c1x& + 3, c1y& + 1), 9
                LINE (c1x& - 2, c1y& - 1)-(c1x& - 22, c1y& - 12), 0
                LINE (c1x& - 2, c1y& - 1)-((c1x& - 2) - com1speed& * 2, (c1y& - 1) - com1speed&), coo&
        ELSEIF c1direction& = 4 THEN
                LINE (c1x&, c1y&)-(c1x& + 5, c1y&), 8
                PSET (c1x& - 1, c1y&), 8
                LINE (c1x& + 4, c1y&)-(c1x& + 5, c1y&), 8
                LINE (c1x&, c1y& - 1)-(c1x&, c1y& - 4), 8
                LINE (c1x& + 2, c1y& - 1)-(c1x& + 3, c1y& - 1), 9
                LINE (c1x&, c1y&)-(c1x&, c1y& + 3), 8
                PSET (c1x& + 1, c1y& + 1), 8
                PSET (c1x& + 1, c1y& - 2), 8
                PSET (c1x& + 1, c1y& - 1), 8
                LINE (c1x& - 2, c1y&)-(c1x& - 22, c1y&), 0
                LINE (c1x& - 2, c1y&)-(c1x& - com1speed& * 2, c1y&), coo&
        ELSEIF c1direction& = 3 THEN
                LINE (c1x&, c1y&)-(c1x& + 3, c1y&), 0
                LINE (c1x& + 3, c1y& - 2)-(c1x& + 1, c1y& - 2), 9
                LINE (c1x& + 2, c1y& + 1)-(c1x& + 4, c1y& + 1), 0
                PSET (c1x& + 4, c1y& - 2), 8
                LINE (c1x& - 1, c1y& - 1)-(c1x& + 3, c1y& - 1), 8
                LINE (c1x& + 4, c1y& + 2)-(c1x& + 5, c1y& + 2), 0
                LINE (c1x& - 1, c1y&)-(c1x& - 3, c1y&), 8
                LINE (c1x& - 4, c1y& + 1)-(c1x& - 5, c1y& + 1), 8
                PSET (c1x& - 1, c1y& - 2), 8
                LINE (c1x& - 2, c1y& + 1)-(c1x& - 22, c1y& + 12), 0
                LINE (c1x& - 2, c1y& + 1)-(c1x& - com1speed& * 2, c1y& + com1speed&), coo&
        END IF
        IF c2direction& = 2 THEN
                LINE (c2x&, c2y& - 1)-(c2x&, c2y& - 2), 0
                LINE (c2x& - 1, c2y&)-(c2x& - 3, c2y&), 8
                LINE (c2x& - 3, c2y& + 1)-(c2x& - 5, c2y& + 1), 8
                LINE (c2x& + 1, c2y&)-(c2x& + 3, c2y&), 0
                LINE (c2x& + 3, c2y& + 1)-(c2x& + 5, c2y& + 1), 0
                LINE (c2x& + 1, c2y& - 1)-(c2x& + 1, c2y& - 2), 0
                LINE (c2x& - 1, c2y& - 1)-(c2x&, c2y& - 4), 0
                PSET (c2x&, c2y& - 2), 9
                LINE (c2x&, c2y& + 1)-(c2x&, c2y& + 12), 0
                LINE (c2x&, c2y& + 1)-(c2x&, (c2y& + 1) + com2speed&), coo&
        ELSEIF c2direction& = 1 THEN
                LINE (c2x&, c2y&)-(c2x& - 4, c2y& - 2), 8
                LINE (c2x& - 3, c2y& - 2)-(c2x& - 1, c2y& - 2), 9
                LINE (c2x& - 1, c2y& - 1)-(c2x& + 1, c2y& - 1), 8
                PSET (c2x& + 1, c2y& - 2), 8
                LINE (c2x& - 1, c2y&)-(c2x& - 2, c2y&), 8
                LINE (c2x& - 2, c2y& + 1)-(c2x& - 5, c2y& + 2), 8
                LINE (c2x& + 1, c2y&)-(c2x& + 3, c2y&), 0
                LINE (c2x& + 4, c2y& + 1)-(c2x& + 5, c2y& + 1), 0
                PSET (c2x& - 4, c2y& + 2), 8
                LINE (c2x& + 2, c2y& + 1)-(c2x& + 22, c2y& + 12), 0
                LINE (c2x& + 2, c2y& + 1)-((c2x& + 2) + com2speed& * 2, (c2y& + 1) + com2speed&), coo&
        ELSEIF c2direction& = 8 THEN
                LINE (c2x& + 1, c2y&)-(c2x& - 5, c2y&), 8
                LINE (c2x&, c2y& - 1)-(c2x& - 1, c2y& - 1), 8
                LINE (c2x& - 2, c2y& - 1)-(c2x& - 3, c2y& - 1), 9
                LINE (c2x&, c2y&)-(c2x&, c2y& - 2), 8
                LINE (c2x&, c2y&)-(c2x&, c2y& + 3), 8
                PSET (c2x& - 1, c2y& + 1), 8
                PSET (c2x& - 1, c2y& - 2), 8
                LINE (c2x&, c2y& - 3)-(c2x&, c2y& - 4), 8
                LINE (c2x&, c2y&)-(c2x& + 1, c2y&), 0
                PSET (c2x&, c2y& - 1), 0
                LINE (c2x& + 2, c2y&)-(c2x& + 22, c2y&), 0
                LINE (c2x& + 2, c2y&)-((c2x& + 2) + com2speed& * 2, c2y&), coo&
        ELSEIF c2direction& = 7 THEN
                LINE (c2x& - 1, c2y& - 1)-(c2x& - 3, c2y& + 1), 9
                LINE (c2x& + 1, c2y& - 3)-(c2x& + 1, c2y& + 1), 0
                LINE (c2x&, c2y&)-(c2x&, c2y& - 2), 0
                PSET (c2x& - 1, c2y&), 0
                LINE (c2x& - 1, c2y& + 1)-(c2x& - 2, c2y& + 1), 0
                LINE (c2x& - 3, c2y& + 2)-(c2x& - 4, c2y& + 2), 0
                LINE (c2x& + 2, c2y&)-(c2x& + 2, c2y& + 1), 0
                LINE (c2x& + 3, c2y& + 1)-(c2x& + 3, c2y& + 2), 0
                LINE (c2x& + 4, c2y& + 2)-(c2x& + 4, c2y& + 3), 0
                LINE (c2x& - 1, c2y& - 2)-(c2x& - 1, c2y& - 3), 8
                LINE (c2x& - 2, c2y& - 2)-(c2x& - 2, c2y& - 4), 8
                PSET (c2x& - 3, c2y& - 4), 8
                PSET (c2x& - 4, c2y& + 2), 8
                LINE (c2x&, c2y& - 2)-(c2x& + 1, c2y& - 3), 8
                LINE (c2x& + 2, c2y& - 1)-(c2x& + 22, c2y& - 12), 0
                LINE (c2x& + 2, c2y& - 1)-((c2x& + 2) + com2speed& * 2, (c2y& - 1) - com2speed&), coo&
        ELSEIF c2direction& = 6 THEN
                LINE (c2x& - 4, c2y& + 1)-(c2x& - 5, c2y& + 1), 8
                LINE (c2x& + 4, c2y& + 1)-(c2x& + 5, c2y& + 1), 0
                LINE (c2x& - 3, c2y&)-(c2x& - 1, c2y&), 8
                PSET (c2x& - 1, c2y& + 2), 8
                LINE (c2x& - 2, c2y& + 1)-(c2x& - 3, c2y& + 1), 8
                LINE (c2x& + 2, c2y& + 1)-(c2x& + 3, c2y& + 1), 0
                LINE (c2x& - 1, c2y& + 1)-(c2x&, c2y& + 4), 8
                LINE (c2x&, c2y& + 1)-(c2x&, c2y& + 2), 0
                LINE (c2x&, c2y&)-(c2x&, c2y& - 1), 8
                LINE (c2x& + 1, c2y& + 1)-(c2x& + 1, c2y& + 2), 0
                LINE (c2x& + 1, c2y&)-(c2x& + 3, c2y&), 0
                LINE (c2x&, c2y& - 2)-(c2x&, c2y& - 12), 0
                LINE (c2x&, c2y& - 2)-(c2x&, (c2y& - 2) - com2speed&), coo&
        ELSEIF c2direction& = 5 THEN
                LINE (c2x& - 1, c2y& - 2)-(c2x& - 1, c2y& + 1), 8
                LINE (c2x& - 2, c2y&)-(c2x& - 2, c2y& + 1), 8
                LINE (c2x&, c2y&)-(c2x&, c2y& - 2), 8
                PSET (c2x& + 1, c2y&), 8
                LINE (c2x& + 1, c2y& + 1)-(c2x& + 2, c2y& + 1), 8
                LINE (c2x& + 3, c2y& + 2)-(c2x& + 4, c2y& + 2), 8
                LINE (c2x& + 2, c2y&)-(c2x& + 2, c2y& + 1), 8
                LINE (c2x& - 3, c2y& + 1)-(c2x& - 3, c2y& + 2), 8
                LINE (c2x& - 4, c2y& + 2)-(c2x& - 4, c2y& + 3), 8
                LINE (c2x& + 1, c2y& - 2)-(c2x& + 1, c2y& - 3), 0
                LINE (c2x& + 2, c2y& - 2)-(c2x& + 2, c2y& - 4), 0
                PSET (c2x& + 3, c2y& - 4), 0
                PSET (c2x&, c2y& - 2), 0
                PSET (c2x& + 1, c2y&), 9
                LINE (c2x& + 1, c2y& - 1)-(c2x& + 3, c2y& + 1), 9
                LINE (c2x& - 2, c2y& - 1)-(c2x& - 22, c2y& - 12), 0
                LINE (c2x& - 2, c2y& - 1)-((c2x& - 2) - com2speed& * 2, (c2y& - 1) - com2speed&), coo&
        ELSEIF c2direction& = 4 THEN
                LINE (c2x&, c2y&)-(c2x& + 5, c2y&), 8
                PSET (c2x& - 1, c2y&), 8
                LINE (c2x& + 4, c2y&)-(c2x& + 5, c2y&), 8
                LINE (c2x&, c2y& - 1)-(c2x&, c2y& - 4), 8
                LINE (c2x& + 2, c2y& - 1)-(c2x& + 3, c2y& - 1), 9
                LINE (c2x&, c2y&)-(c2x&, c2y& + 3), 8
                PSET (c2x& + 1, c2y& + 1), 8
                PSET (c2x& + 1, c2y& - 2), 8
                PSET (c2x& + 1, c2y& - 1), 8
                LINE (c2x& - 2, c2y&)-(c2x& - 22, c2y&), 0
                LINE (c2x& - 2, c2y&)-(c2x& - com2speed& * 2, c2y&), coo&
        ELSEIF c2direction& = 3 THEN
                LINE (c2x&, c2y&)-(c2x& + 3, c2y&), 0
                LINE (c2x& + 3, c2y& - 2)-(c2x& + 1, c2y& - 2), 9
                LINE (c2x& + 2, c2y& + 1)-(c2x& + 4, c2y& + 1), 0
                PSET (c2x& + 4, c2y& - 2), 8
                LINE (c2x& - 1, c2y& - 1)-(c2x& + 3, c2y& - 1), 8
                LINE (c2x& + 4, c2y& + 2)-(c2x& + 5, c2y& + 2), 0
                LINE (c2x& - 1, c2y&)-(c2x& - 3, c2y&), 8
                LINE (c2x& - 4, c2y& + 1)-(c2x& - 5, c2y& + 1), 8
                PSET (c2x& - 1, c2y& - 2), 8
                LINE (c2x& - 2, c2y& + 1)-(c2x& - 22, c2y& + 12), 0
                LINE (c2x& - 2, c2y& + 1)-(c2x& - com2speed& * 2, c2y& + com2speed&), coo&
        END IF
        IF c3direction& = 2 THEN
                LINE (c3x&, c3y& - 1)-(c3x&, c3y& - 2), 0
                LINE (c3x& - 1, c3y&)-(c3x& - 3, c3y&), 8
                LINE (c3x& - 3, c3y& + 1)-(c3x& - 5, c3y& + 1), 8
                LINE (c3x& + 1, c3y&)-(c3x& + 3, c3y&), 0
                LINE (c3x& + 3, c3y& + 1)-(c3x& + 5, c3y& + 1), 0
                LINE (c3x& + 1, c3y& - 1)-(c3x& + 1, c3y& - 2), 0
                LINE (c3x& - 1, c3y& - 1)-(c3x&, c3y& - 4), 0
                PSET (c3x&, c3y& - 2), 9
                LINE (c3x&, c3y& + 1)-(c3x&, c3y& + 12), 0
                LINE (c3x&, c3y& + 1)-(c3x&, (c3y& + 1) + com3speed&), coo&
        ELSEIF c3direction& = 1 THEN
                LINE (c3x&, c3y&)-(c3x& - 4, c3y& - 2), 8
                LINE (c3x& - 3, c3y& - 2)-(c3x& - 1, c3y& - 2), 9
                LINE (c3x& - 1, c3y& - 1)-(c3x& + 1, c3y& - 1), 8
                PSET (c3x& + 1, c3y& - 2), 8
                LINE (c3x& - 1, c3y&)-(c3x& - 2, c3y&), 8
                LINE (c3x& - 2, c3y& + 1)-(c3x& - 5, c3y& + 2), 8
                LINE (c3x& + 1, c3y&)-(c3x& + 3, c3y&), 0
                LINE (c3x& + 4, c3y& + 1)-(c3x& + 5, c3y& + 1), 0
                PSET (c3x& - 4, c3y& + 2), 8
                LINE (c3x& + 2, c3y& + 1)-(c3x& + 22, c3y& + 12), 0
                LINE (c3x& + 2, c3y& + 1)-((c3x& + 2) + com3speed& * 2, (c3y& + 1) + com3speed&), coo&
        ELSEIF c3direction& = 8 THEN
                LINE (c3x& + 1, c3y&)-(c3x& - 5, c3y&), 8
                LINE (c3x&, c3y& - 1)-(c3x& - 1, c3y& - 1), 8
                LINE (c3x& - 2, c3y& - 1)-(c3x& - 3, c3y& - 1), 9
                LINE (c3x&, c3y&)-(c3x&, c3y& - 2), 8
                LINE (c3x&, c3y&)-(c3x&, c3y& + 3), 8
                PSET (c3x& - 1, c3y& + 1), 8
                PSET (c3x& - 1, c3y& - 2), 8
                LINE (c3x&, c3y& - 3)-(c3x&, c3y& - 4), 8
                LINE (c3x&, c3y&)-(c3x& + 1, c3y&), 0
                PSET (c3x&, c3y& - 1), 0
                LINE (c3x& + 2, c3y&)-(c3x& + 22, c3y&), 0
                LINE (c3x& + 2, c3y&)-((c3x& + 2) + com3speed& * 2, c3y&), coo&
        ELSEIF c3direction& = 7 THEN
                LINE (c3x& - 1, c3y& - 1)-(c3x& - 3, c3y& + 1), 9
                LINE (c3x& + 1, c3y& - 3)-(c3x& + 1, c3y& + 1), 0
                LINE (c3x&, c3y&)-(c3x&, c3y& - 2), 0
                PSET (c3x& - 1, c3y&), 0
                LINE (c3x& - 1, c3y& + 1)-(c3x& - 2, c3y& + 1), 0
                LINE (c3x& - 3, c3y& + 2)-(c3x& - 4, c3y& + 2), 0
                LINE (c3x& + 2, c3y&)-(c3x& + 2, c3y& + 1), 0
                LINE (c3x& + 3, c3y& + 1)-(c3x& + 3, c3y& + 2), 0
                LINE (c3x& + 4, c3y& + 2)-(c3x& + 4, c3y& + 3), 0
                LINE (c3x& - 1, c3y& - 2)-(c3x& - 1, c3y& - 3), 8
                LINE (c3x& - 2, c3y& - 2)-(c3x& - 2, c3y& - 4), 8
                PSET (c3x& - 3, c3y& - 4), 8
                PSET (c3x& - 4, c3y& + 2), 8
                LINE (c3x&, c3y& - 2)-(c3x& + 1, c3y& - 3), 8
                LINE (c3x& + 2, c3y& - 1)-(c3x& + 22, c3y& - 12), 0
                LINE (c3x& + 2, c3y& - 1)-((c3x& + 2) + com3speed& * 2, (c3y& - 1) - com3speed&), coo&
        ELSEIF c3direction& = 6 THEN
                LINE (c3x& - 4, c3y& + 1)-(c3x& - 5, c3y& + 1), 8
                LINE (c3x& + 4, c3y& + 1)-(c3x& + 5, c3y& + 1), 0
                LINE (c3x& - 3, c3y&)-(c3x& - 1, c3y&), 8
                PSET (c3x& - 1, c3y& + 2), 8
                LINE (c3x& - 2, c3y& + 1)-(c3x& - 3, c3y& + 1), 8
                LINE (c3x& + 2, c3y& + 1)-(c3x& + 3, c3y& + 1), 0
                LINE (c3x& - 1, c3y& + 1)-(c3x&, c3y& + 4), 8
                LINE (c3x&, c3y& + 1)-(c3x&, c3y& + 2), 0
                LINE (c3x&, c3y&)-(c3x&, c3y& - 1), 8
                LINE (c3x& + 1, c3y& + 1)-(c3x& + 1, c3y& + 2), 0
                LINE (c3x& + 1, c3y&)-(c3x& + 3, c3y&), 0
                LINE (c3x&, c3y& - 2)-(c3x&, c3y& - 12), 0
                LINE (c3x&, c3y& - 2)-(c3x&, (c3y& - 2) - com3speed&), coo&
        ELSEIF c3direction& = 5 THEN
                LINE (c3x& - 1, c3y& - 2)-(c3x& - 1, c3y& + 1), 8
                LINE (c3x& - 2, c3y&)-(c3x& - 2, c3y& + 1), 8
                LINE (c3x&, c3y&)-(c3x&, c3y& - 2), 8
                PSET (c3x& + 1, c3y&), 8
                LINE (c3x& + 1, c3y& + 1)-(c3x& + 2, c3y& + 1), 8
                LINE (c3x& + 3, c3y& + 2)-(c3x& + 4, c3y& + 2), 8
                LINE (c3x& + 2, c3y&)-(c3x& + 2, c3y& + 1), 8
                LINE (c3x& - 3, c3y& + 1)-(c3x& - 3, c3y& + 2), 8
                LINE (c3x& - 4, c3y& + 2)-(c3x& - 4, c3y& + 3), 8
                LINE (c3x& + 1, c3y& - 2)-(c3x& + 1, c3y& - 3), 0
                LINE (c3x& + 2, c3y& - 2)-(c3x& + 2, c3y& - 4), 0
                PSET (c3x& + 3, c3y& - 4), 0
                PSET (c3x&, c3y& - 2), 0
                PSET (c3x& + 1, c3y&), 9
                LINE (c3x& + 1, c3y& - 1)-(c3x& + 3, c3y& + 1), 9
                LINE (c3x& - 2, c3y& - 1)-(c3x& - 22, c3y& - 12), 0
                LINE (c3x& - 2, c3y& - 1)-((c3x& - 2) - com3speed& * 2, (c3y& - 1) - com3speed&), coo&
        ELSEIF c3direction& = 4 THEN
                LINE (c3x&, c3y&)-(c3x& + 5, c3y&), 8
                PSET (c3x& - 1, c3y&), 8
                LINE (c3x& + 4, c3y&)-(c3x& + 5, c3y&), 8
                LINE (c3x&, c3y& - 1)-(c3x&, c3y& - 4), 8
                LINE (c3x& + 2, c3y& - 1)-(c3x& + 3, c3y& - 1), 9
                LINE (c3x&, c3y&)-(c3x&, c3y& + 3), 8
                PSET (c3x& + 1, c3y& + 1), 8
                PSET (c3x& + 1, c3y& - 2), 8
                PSET (c3x& + 1, c3y& - 1), 8
                LINE (c3x& - 2, c3y&)-(c3x& - 22, c3y&), 0
                LINE (c3x& - 2, c3y&)-(c3x& - com3speed& * 2, c3y&), coo&
        ELSEIF c3direction& = 3 THEN
                LINE (c3x&, c3y&)-(c3x& + 3, c3y&), 0
                LINE (c3x& + 3, c3y& - 2)-(c3x& + 1, c3y& - 2), 9
                LINE (c3x& + 2, c3y& + 1)-(c3x& + 4, c3y& + 1), 0
                PSET (c3x& + 4, c3y& - 2), 8
                LINE (c3x& - 1, c3y& - 1)-(c3x& + 3, c3y& - 1), 8
                LINE (c3x& + 4, c3y& + 2)-(c3x& + 5, c3y& + 2), 0
                LINE (c3x& - 1, c3y&)-(c3x& - 3, c3y&), 8
                LINE (c3x& - 4, c3y& + 1)-(c3x& - 5, c3y& + 1), 8
                PSET (c3x& - 1, c3y& - 2), 8
                LINE (c3x& - 2, c3y& + 1)-(c3x& - 22, c3y& + 12), 0
                LINE (c3x& - 2, c3y& + 1)-(c3x& - com3speed& * 2, c3y& + com3speed&), coo&
        END IF
       
        IF (x& > basex& - 15) AND (x& < basex& + 15) AND (y& > basey& - 15) AND (y& < basey& + 15) AND po& = 0 THEN GOSUB basee
       
        'IF lazer& > 0 THEN GOSUB lazer
        IF c1lazer& > 0 THEN GOSUB c1lazer
        IF c2lazer& > 0 THEN GOSUB c2lazer
        IF c3lazer& > 0 THEN GOSUB c3lazer
        IF fin& = 1 THEN LINE (ox& - 5, oy& - 5)-(ox& + 5, oy& + 5), 1, B
       
       
        FOR i = 1 TO 20
       
        IF c1t& = 1 THEN
                myx& = c1tx&
                myy& = c1ty&
                emx& = x&
                emy& = y&
                GOSUB tor
                c1tx& = myx&
                c1ty& = myy&
                IF hit& = 1 THEN
                        DO
                        d& = d& + 1
                        shields& = shields& - 1
                        IF shields& < 0 THEN shields& = 0
                        IF shields& = 0 THEN
                                RANDOMIZE TIMER
                                cube& = INT(RND * 4) + 1
                                IF cube& = 1 THEN sen& = sen& - 1
                                IF cube& = 2 THEN eng& = eng& - 1
                                IF cube& = 3 THEN wea& = wea& - 1
                                IF cube& = 4 THEN ene& = ene& - 1
                                IF sen& < 0 THEN
                                        sen& = 0
                                        d& = d& - 5
                                ELSEIF eng& < 0 THEN
                                        eng& = 0
                                        d& = d& - 5
                                ELSEIF wea& < 0 THEN
                                        wea& = 0
                                        d& = d& - 5
                                ELSEIF ene& < 0 THEN
                                        ene& = 0
                                        d& = d& - 5
                                END IF
                                IF wea& = 0 AND sen& = 0 AND eng& = 0 AND ene& = 0 THEN GOTO youexplode
                        END IF
                        IF d& = da& THEN EXIT DO
                        LOOP
                        IF shields& > 10 THEN showshields& = showshieds& + 10
                        LINE (c1tx& - 1, c1ty& - 1)-(c1tx& + 1, c1ty& + 1), 0, BF
                        d& = 0
                        c1t& = 0
                        hit& = 0
                END IF
        END IF
       
        IF c2t& = 1 THEN
                myx& = c2tx&
                myy& = c2ty&
                emx& = x&
                emy& = y&
                GOSUB tor
                c2tx& = myx&
                c2ty& = myy&
                IF hit& = 1 THEN
                        DO
                        d& = d& + 1
                        shields& = shields& - 1
                        IF shields& < 0 THEN shields& = 0
                        IF shields& = 0 THEN
                                RANDOMIZE TIMER
                                cube& = INT(RND * 4) + 1
                                IF cube& = 1 THEN sen& = sen& - 1
                                IF cube& = 2 THEN eng& = eng& - 1
                                IF cube& = 3 THEN wea& = wea& - 1
                                IF cube& = 4 THEN ene& = ene& - 1
                                IF sen& < 0 THEN
                                        sen& = 0
                                        d& = d& - 5
                                ELSEIF eng& < 0 THEN
                                        eng& = 0
                                        d& = d& - 5
                                ELSEIF wea& < 0 THEN
                                        wea& = 0
                                        d& = d& - 5
                                ELSEIF ene& < 0 THEN
                                        ene& = 0
                                        d& = d& - 5
                                END IF
                                IF wea& = 0 AND sen& = 0 AND eng& = 0 AND ene& = 0 THEN GOTO youexplode
                        END IF
                        IF d& = da& THEN EXIT DO
                        LOOP
                        IF shields& > 10 THEN showshields& = showshieds& + 10
                        LINE (c2tx& - 1, c2ty& - 1)-(c2tx& + 1, c2ty& + 1), 0, BF
                        d& = 0
                        c2t& = 0
                        hit& = 0
                END IF
        END IF
       
        IF c3t& = 1 THEN
                myx& = c3tx&
                myy& = c3ty&
                emx& = x&
                emy& = y&
                GOSUB tor
                c3tx& = myx&
                c3ty& = myy&
                IF hit& = 1 THEN
                        DO
                        d& = d& + 1
                        shields& = shields& - 1
                        IF shields& < 0 THEN shields& = 0
                        IF shields& = 0 THEN
                                RANDOMIZE TIMER
                                cube& = INT(RND * 4) + 1
                                IF cube& = 1 THEN sen& = sen& - 1
                                IF cube& = 2 THEN eng& = eng& - 1
                                IF cube& = 3 THEN wea& = wea& - 1
                                IF cube& = 4 THEN ene& = ene& - 1
                                IF sen& < 0 THEN
                                        sen& = 0
                                        d& = d& - 5
                                ELSEIF eng& < 0 THEN
                                        eng& = 0
                                        d& = d& - 5
                                ELSEIF wea& < 0 THEN
                                        wea& = 0
                                        d& = d& - 5
                                ELSEIF ene& < 0 THEN
                                        ene& = 0
                                        d& = d& - 5
                                END IF
                                IF wea& = 0 AND sen& = 0 AND eng& = 0 AND ene& = 0 THEN GOTO youexplode
                        END IF
                        IF d& = da& THEN EXIT DO
                        LOOP
                        IF shields& > 10 THEN showshields& = showshieds& + 10
                        LINE (c3tx& - 1, c3ty& - 1)-(c3tx& + 1, c3ty& + 1), 0, BF
                        d& = 0
                        c3t& = 0
                        hit& = 0
                END IF
        END IF
        NEXT i
      
        FOR i = 1 TO 20
        IF t& = 1 THEN
                myx& = tx&
                myy& = ty&
                emx& = ox&
                emy& = oy&
                GOSUB tor
                tx& = myx&
                ty& = myy&
                IF hit& = 1 THEN
                        t& = 0
                        oh& = oh& - da& * 5
                        IF object& = 1 THEN
                                c1h& = oh&
                                c1l& = ol&
                        ELSEIF object& = 2 THEN
                                c2h& = oh&
                                c2l& = ol&
                        ELSEIF object& = 3 THEN
                                c3h& = oh&
                                c3l& = ol&
                        END IF
                        IF c1h& < 1 THEN GOTO explode
                        IF c2h& < 1 THEN GOTO explode
                        IF c3h& < 1 THEN GOTO explode
                        hit& = 0
                END IF
        END IF
        NEXT i
       
        
        max& = CINT(supmax& * eng& / 100)
        IF eng& < 30 THEN nw& = 1
        IF eng& > 30 THEN nw& = 0
        
        ans1$ = INKEY$
        LOOP UNTIL (ans1$ = "7" OR ans1$ = "9" OR ans1$ = "c" OR ans1$ = "+" OR ans1$ = "=" OR ans1$ = "-" OR ans1$ = "r" OR ans1$ = "p" OR ans1$ = "0" OR ans1$ = "5" OR ans1$ = "8" OR ans1$ = "2" OR ans1$ = "6" OR ans1$ = "4" OR ans1$ = "q" OR ans1$ = "3" OR ans1$ = "1")
        IF ans1$ = "6" THEN
                CLS
                m& = m& - 1
                IF m& < 1 THEN m& = 8
                IF w& = 1 THEN LINE (1, 22)-(638, 329), 0, BF
        ELSEIF ans1$ = "8" THEN
                CLS
                IF w& = 1 THEN GOTO begin
                IF po& > max& THEN GOTO begin
                IF po& = max& THEN GOTO begin
                po& = po& + 1
        ELSEIF ans1$ = "2" THEN
                CLS
                IF w& = 1 THEN GOTO begin
                po& = po& - 1
                IF po& < 0 THEN po& = 0
        ELSEIF ans1$ = "4" THEN
                CLS
                m& = m& + 1
                IF m& > 8 THEN m& = 1
                IF w& = 1 THEN LINE (1, 22)-(638, 329), 0, BF
        ELSEIF ans1$ = "0" THEN
                GOSUB fire2
        ELSEIF ans1$ = "5" THEN
                'GOSUB fire
        ELSEIF ans1$ = "p" THEN GOSUB paused
        ELSEIF ans1$ = "3" THEN
                IF w& = 0 AND nw& = 0 THEN
                        w& = 1
                        ww& = 50
                        po& = 100
                        CLS
                        GOTO begin
                ELSEIF w& = 1 THEN
                        w& = 0
                        po& = 2
                        GOSUB ran
                        CLS
                        GOTO begin
                END IF
                GOSUB drawing
        ELSEIF ans1$ = "+" THEN
                 divide& = divide& - 500
                 IF divide& < 1 THEN divide& = 500
                 VIEW SCREEN (50, 435)-(80, 465)
                 CLS
                 VIEW
        ELSEIF ans1$ = "=" THEN
                divide& = divide& - 500
                IF divide& < 1 THEN divide& = 500
                VIEW SCREEN (50, 435)-(80, 465)
                CLS
                VIEW
        ELSEIF ans1$ = "-" THEN
                divide& = divide& + 500
                IF divide& > 2000000000 THEN divide& = 2000000000
                VIEW SCREEN (50, 435)-(80, 465)
                CLS
                VIEW
        ELSEIF ans1$ = "c" THEN
                IF object2& = 4 THEN object& = 4
                object& = object& + 1
                object2& = 0
                VIEW
                CLS
                GOSUB drawing
                VIEW SCREEN (1, 2)-(638, 329)
                IF object& > 4 THEN object& = 1
                IF object& = 4 THEN
                        object2& = 4
                        IF c1x& + c1y& < c2x& + c2y& AND c1x& + c1y& < c3x& + c3y& THEN object& = 1
                        IF c2x& + c2y& < c1x& + c1y& AND c2x& + c2y& < c3x& + c3y& THEN object& = 2
                        IF c3x& + c3y& < c1x& + c1y& AND c3x& + c3y& < c2x& + c2y& THEN object& = 3
                END IF
                IF object& = 1 THEN
                        ox& = c1x&
                        oy& = c1y&
                        oh& = c1h&
                        ol& = c1l&
                ELSEIF object& = 2 THEN
                        ox& = c2x&
                        oy& = c2y&
                        oh& = c2h&
                        ol& = c2l&
                ELSEIF object& = 3 THEN
                        ox& = c3x&
                        oy& = c3y&
                        oh& = c3h&
                        ol& = c3l&
                END IF
        ELSEIF ans1$ = "9" THEN
                IF m& = 1 THEN
                        GOSUB xdown
                ELSEIF m& = 2 THEN
                        GOSUB xdown
                        GOSUB yup
                ELSEIF m& = 3 THEN
                        GOSUB yup
                ELSEIF m& = 4 THEN
                        GOSUB xup
                        GOSUB yup
                ELSEIF m& = 5 THEN
                        GOSUB xup
                ELSEIF m& = 6 THEN
                        GOSUB ydown
                        GOSUB xup
                ELSEIF m& = 7 THEN
                        GOSUB ydown
                ELSEIF m& = 8 THEN
                        GOSUB ydown
                        GOSUB xdown
                END IF
                CLS
        ELSEIF ans1$ = "7" THEN
                IF m& = 5 THEN
                        GOSUB xdown
                ELSEIF m& = 6 THEN
                        GOSUB xdown
                        GOSUB yup
                ELSEIF m& = 7 THEN
                        GOSUB yup
                ELSEIF m& = 8 THEN
                        GOSUB yup
                        GOSUB xup
                ELSEIF m& = 1 THEN
                        GOSUB xup
                ELSEIF m& = 2 THEN
                        GOSUB ydown
                        GOSUB xup
                ELSEIF m& = 3 THEN
                        GOSUB ydown
                ELSEIF m& = 4 THEN
                        GOSUB ydown
                        GOSUB xdown
                END IF
                CLS
        ELSEIF ans1$ = "1" THEN
                siz& = 1
                DO
                siz& = siz& + 1
                CIRCLE (x&, y&), siz&, 1
                CIRCLE (x&, y&), siz& + 1, 1
                CIRCLE (x&, y&), siz& + 2, 1
                CIRCLE (x&, y&), siz& + 3, 1
                CIRCLE (x&, y&), siz& - 1, 0
                IF siz& > 154 THEN EXIT DO
                LOOP
                LINE (1, 2)-(638, 329), 0, BF
                LINE (x& - 5, y& - 5)-(x& + 5, y& + 5), 0, B
                LINE (x& - 1, y& - 1)-(x& + 1, y& + 1), 0, BF
                di1& = c1x& - c1tx&
                di2& = c1y& - c1ty&
                di3& = c2x& - c2tx&
                di4& = c2y& - c2ty&
                di5& = c3x& - c3tx&
                di6& = c3y& - c3ty&
                RANDOMIZE TIMER
                changex& = INT(RND * 2000000) - 1000000
                changey& = INT(RND * 2000000) - 1000000
                c1x& = c1x& + changex&
                c1y& = c1y& + changey&
                c1ty& = c1ty& + changey&
                c1tx& = c1tx& + changex&
                c2x& = c2x& + changex&
                c2y& = c2y& + changey&
                c2ty& = c2ty& + changey&
                c2tx& = c2tx& + changex&
                c3x& = c3x& + changex&
                c3y& = c3y& + changey&
                c3ty& = c3ty& + changey&
                c3tx& = c3tx& + changex&
                GOSUB ran
                DO
                siz& = siz& - 1
                CIRCLE (x&, y&), siz&, 1
                CIRCLE (x&, y&), siz& - 1, 1
                CIRCLE (x&, y&), siz& - 2, 1
                CIRCLE (x&, y&), siz& - 3, 1
                CIRCLE (x&, y&), siz& + 1, 0
                IF siz& < 5 THEN EXIT DO
                LOOP
                CLS
                GOSUB drawing
                hy& = INT(RND * (eng& - (eng& / 2)) + 50) + 1
                IF hy& = 1 THEN GOTO youexplode
                IF w& = 1 THEN
                hy& = INT(RND * (eng& - (eng& / 2)) + 10) + 1
                IF hy& = 1 THEN GOTO youexplode
                END IF
        ELSEIF ans1$ = "q" THEN
                VIEW
                CLS
                GOSUB ran
                FOR i = 1 TO stars&
                PSET (stx(1, i), sty(1, i)), 15
                NEXT i
                LOCATE 24, 24
                PRINT "Do you want quit to the Main Menu?"
                DO
                ans7$ = INKEY$
                LOOP UNTIL (ans7$ = "y" OR ans7$ = "n")
                        IF ans7$ = "n" THEN
                                CLS
                                GOSUB drawing
                                VIEW SCREEN (1, 2)-(638, 329)
                                GOTO eenndd
                        ELSEIF ans7$ = "y" THEN GOTO start
                        END IF
        END IF
eenndd:
        LOOP



finder:
        fin2& = 0
        IF life1& = 0 THEN RETURN
        dist1& = x& - ox&
        dist2& = y& - oy&
        IF dist1& > ra& THEN RETURN
        IF dist1& < -ra& THEN RETURN
        IF dist2& > ra& THEN RETURN
        IF dist2& < -ra& THEN RETURN
        fin2& = 1
        RETURN
fire2:
        IF t& = 1 THEN RETURN
        IF t& = 0 THEN tx& = x&
        IF t& = 0 THEN ty& = y&
        IF fin2& = 0 THEN RETURN
        IF life1& = 0 THEN RETURN
        SOUND 3000, .1
        t& = 1
        RETURN
tor:
        dist2& = myx& - emx&
        dist22& = myy& - emy&
        CIRCLE (myx&, myy&), 1, 0
        IF dist22& > 0 THEN myy& = myy& - 1
        IF dist22& < 0 THEN myy& = myy& + 1
        IF dist2& > 0 THEN myx& = myx& - 1
        IF dist2& < 0 THEN myx& = myx& + 1
        CIRCLE (myx&, myy&), 1, 2
        IF (myx& > emx& - 5 AND myx& < emx& + 5) AND (myy& > emy& - 5 AND myy& < emy& + 5) THEN GOSUB hit
        RETURN
hit:
        si& = 0
        DO
        si& = si& + 1
        CIRCLE (myx&, myy&), si&, 1
        LOOP UNTIL si& = 2
        si& = 0
        DO
        si& = si& + 1
        CIRCLE (myx&, myy&), si&, 0
        LOOP UNTIL si& = 2
        LINE (myx& - 1, myy& - 1)-(myx& + 1, myy& + 1), 0, BF
        hit& = 1
        RETURN
explode:
        ol& = 0
        si& = 0
        DO
        si& = si& + 1
        CIRCLE (myx&, myy&), si&, 1
        LOOP UNTIL si& = 50
        si& = 0
        DO
        si& = si& + 1
        CIRCLE (myx&, myy&), si&, 0
        LOOP UNTIL si& = 50
        LINE (myx& - 1, myy& - 1)-(myx& + 1, myy& + 1), 0, BF
        CLS
        IF object& = 1 THEN
                c1h& = oh&
                c1l& = ol&
        ELSEIF object& = 2 THEN
                c2h& = oh&
                c2l& = ol&
        ELSEIF object& = 3 THEN
                c3h& = oh&
                c3l& = ol&
        END IF
        GOSUB comlose
        GOTO begin
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
        IF findcom1laz& = 1 THEN c1lazer& = 5
        RETURN
c1lazer:
        IF findcom1laz& = 0 THEN RETURN
        c1lazer& = c1lazer& - 1
        IF life1& = 0 THEN RETURN
        LINE (c1x&, c1y&)-(x&, y&), 4
        SOUND 3000, .1
        shields& = shields& - 1
        IF shields& < 0 THEN shields& = 0
        IF shields& = 0 THEN
                RANDOMIZE TIMER
                cube& = INT(RND * 4) + 1
                IF cube& = 1 THEN sen& = sen& - 5
                IF cube& = 2 THEN eng& = eng& - 5
                IF cube& = 3 THEN wea& = wea& - 5
                IF cube& = 4 THEN ene& = ene& - 5
                IF sen& < 0 THEN sen& = 0
                IF eng& < 0 THEN eng& = 0
                IF wea& < 0 THEN wea& = 0
                IF ene& < 0 THEN ene& = 0
                IF wea& = 0 AND sen& = 0 AND eng& = 0 AND ene& = 0 THEN GOTO youexplode
        END IF
        IF shields& > 10 THEN showshields& = showshields& + 2
        RETURN
c1fire:
        IF c1t& = 1 THEN RETURN
        IF c1t& = 0 THEN c1tx& = c1x&
        IF c1t& = 0 THEN c1ty& = c1y&
        IF findcom1tor& = 0 THEN RETURN
        IF c1l& = 0 THEN RETURN
        SOUND 3000, .1
        c1t& = 1
        RETURN
c2finder:
        findcom2tor& = 0
        IF life1& = 0 THEN RETURN
        dist1& = c2x& - x&
        dist12& = c2y& - y&
        IF dist1& > ra& THEN RETURN
        IF dist1& < -ra& THEN RETURN
        IF dist12& > ra& THEN RETURN
        IF dist12& < -ra& THEN RETURN
        findcom2tor& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB c2fire ELSE RETURN
        RETURN
c2finder2:
        findcom2laz& = 0
        IF life1& = 0 THEN RETURN
        dist1& = c2x& - x&
        dist12& = c2y& - y&
        IF dist1& > 170 THEN RETURN
        IF dist1& < -170 THEN RETURN
        IF dist12& > 170 THEN RETURN
        IF dist12& < -170 THEN RETURN
        findcom1laz& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB c2firelaz ELSE RETURN
        RETURN
c2firelaz:
        IF c2lazer > 0 THEN RETURN
        IF findcom2laz& = 1 THEN c2lazer& = 5
        RETURN
c2lazer:
        IF findcom2laz& = 0 THEN RETURN
        c2lazer& = c2lazer& - 1
        IF life1& = 0 THEN RETURN
        LINE (c2x&, c2y&)-(x&, y&), 4
        SOUND 3000, .1
        shields& = shields& - 1
        IF shields& < 0 THEN shields& = 0
        IF shields& = 0 THEN
                RANDOMIZE TIMER
                cube& = INT(RND * 4) + 1
                IF cube& = 1 THEN sen& = sen& - 5
                IF cube& = 2 THEN eng& = eng& - 5
                IF cube& = 3 THEN wea& = wea& - 5
                IF cube& = 4 THEN ene& = ene& - 5
                IF sen& < 0 THEN sen& = 0
                IF eng& < 0 THEN eng& = 0
                IF wea& < 0 THEN wea& = 0
                IF ene& < 0 THEN ene& = 0
                IF wea& = 0 AND sen& = 0 AND eng& = 0 AND ene& = 0 THEN GOTO youexplode
        END IF
        IF shields& > 10 THEN showshields& = showshields& + 2
        RETURN
c2fire:
        IF c2t& = 1 THEN RETURN
        IF c2t& = 0 THEN c2tx& = c2x&
        IF c2t& = 0 THEN c2ty& = c2y&
        IF findcom2tor& = 0 THEN RETURN
        IF c2l& = 0 THEN RETURN
        SOUND 3000, .1
        c2t& = 1
        RETURN
c3finder:
        findcom3tor& = 0
        IF life1& = 0 THEN RETURN
        dist1& = c3x& - x&
        dist12& = c3y& - y&
        IF dist1& > ra& THEN RETURN
        IF dist1& < -ra& THEN RETURN
        IF dist12& > ra& THEN RETURN
        IF dist12& < -ra& THEN RETURN
        findcom3tor& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB c3fire ELSE RETURN
        RETURN
c3finder2:
        findcom3laz& = 0
        IF life1& = 0 THEN RETURN
        dist1& = c3x& - x&
        dist12& = c3y& - y&
        IF dist1& > 170 THEN RETURN
        IF dist1& < -170 THEN RETURN
        IF dist12& > 170 THEN RETURN
        IF dist12& < -170 THEN RETURN
        findcom3laz& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB c3firelaz ELSE RETURN
        RETURN
c3firelaz:
        IF c3lazer > 0 THEN RETURN
        IF findcom3laz& = 1 THEN c3lazer& = 5
        RETURN
c3lazer:
        IF findcom3laz& = 0 THEN RETURN
        c3lazer& = c3lazer& - 1
        IF life1& = 0 THEN RETURN
        LINE (c3x&, c3y&)-(x&, y&), 4
        SOUND 3000, .1
        shields& = shields& - 1
        IF shields& < 0 THEN shields& = 0
        IF shields& = 0 THEN
                RANDOMIZE TIMER
                cube& = INT(RND * 4) + 1
                IF cube& = 1 THEN sen& = sen& - 5
                IF cube& = 2 THEN eng& = eng& - 5
                IF cube& = 3 THEN wea& = wea& - 5
                IF cube& = 4 THEN ene& = ene& - 5
                IF sen& < 0 THEN sen& = 0
                IF eng& < 0 THEN eng& = 0
                IF wea& < 0 THEN wea& = 0
                IF ene& < 0 THEN ene& = 0
                IF wea& = 0 AND sen& = 0 AND eng& = 0 AND ene& = 0 THEN GOTO youexplode
        END IF
        IF shields& > 10 THEN showshields& = showshields& + 2
        RETURN
c3fire:
        IF c3t& = 1 THEN RETURN
        IF c3t& = 0 THEN c3tx& = c3x&
        IF c3t& = 0 THEN c3ty& = c3y&
        IF findcom3tor& = 0 THEN RETURN
        IF c1l& = 0 THEN RETURN
        SOUND 3000, .1
        c3t& = 1
        RETURN



youexplode:
        life1& = 0
        ex& = x&
        ey& = y&
        GOSUB explosion
        GOTO begin
explosion:
        CLS
        size& = 2
        RANDOMIZE TIMER
        coo& = INT(RND * 2) + 1
        DO
        FOR i = 1 TO stars&
        PSET (stx(1, i), sty(1, i)), 15
        NEXT i
        CIRCLE (ex&, ey&), size&, 0, , , .5
        CIRCLE (ex&, ey&), size& + 1, 0, , , .5
        CIRCLE (ex&, ey&), size& + 2, 0, , , .5
        CIRCLE (ex&, ey&), size& + 3, 0, , , .5
        CIRCLE (ex&, ey&), size& + 4, 0, , , .5
        CIRCLE (ex&, ey&), size& + 5, 0, , , .5
        CIRCLE (ex&, ey&), size& + 6, 0, , , .5
        CIRCLE (ex&, ey&), size& + 7, 0, , , .5
        CIRCLE (ex&, ey&), size& + 8, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 9, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 10, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 11, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 12, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 13, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 14, coo&, , , .5
        CIRCLE (ex&, ey&), size& + 15, coo&, , , .5
        IF size& > 100 AND size& < 200 THEN CIRCLE (ex&, ey&), (size& / 2) - 50, coo&
        IF size& > 100 AND size& < 200 THEN CIRCLE (ex&, ey&), (size& / 2) - 49, coo&
        IF size& > 100 AND size& < 200 THEN CIRCLE (ex& + 1, ey&), (size& / 2) - 50, coo&
        IF size& > 100 AND size& < 200 THEN CIRCLE (ex& + 1, ey&), (size& / 2) - 49, coo&
        IF size& > 159 THEN CIRCLE (x&, y&), 1, 0
        IF size& > 159 THEN CIRCLE (x&, y&), 2, 0
        IF size& > 159 THEN CIRCLE (x&, y&), 3, 0
        IF size& > 159 THEN CIRCLE (x&, y&), 4, 0
        IF size& > 159 THEN CIRCLE (ex& + 1, ey&), size& - 160, 0
        IF size& > 159 THEN CIRCLE (ex& + 1, ey&), size& - 159, 0
        IF size& > 159 THEN CIRCLE (ex& + 1, ey&), size& - 158, 0
        IF size& > 159 THEN CIRCLE (ex& + 1, ey&), size& - 157, 0
        IF size& > 159 THEN CIRCLE (ex&, ey&), size& - 160, 0
        IF size& > 159 THEN CIRCLE (ex&, ey&), size& - 159, 0
        IF size& > 159 THEN CIRCLE (ex&, ey&), size& - 158, 0
        IF size& > 159 THEN CIRCLE (ex&, ey&), size& - 157, 0
        LINE (0, 1)-(639, 330), 14, B
        size& = size& + 4
        IF size& > 212 THEN EXIT DO
        LOOP
        CLS
        RETURN

basee:
        LOCATE 3, 3
        FOR i = 1 TO 640
        LINE (i, 0)-(i, 480), 0
        LINE (i + 1, 0)-(i + 1, 480), 2
        LINE (i + 2, 0)-(i + 2, 480), 2
        LINE (i + 3, 0)-(i + 3, 480), 2
        NEXT i
       
        CLS
        COLOR 2
        LINE (10, 20)-(50, 50), 14, B
        PRINT "$"; money&
        SLEEP
        COLOR 15
        CLS
        po& = 1
RETURN

youlose:
VIEW
CLS
GOSUB ran
FOR i = 1 TO stars&
PSET (stx(1, i), sty(1, i)), 15
NEXT i
LOCATE 24, 34
PRINT "You lost!"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = " ")
GOTO endd

comlose:
money& = money& + 1000
IF c1l& = 0 THEN
        c1h& = 200
        c1l& = 1
        RANDOMIZE TIMER
        c1x& = INT(RND * 400000) - 200000
        c1y& = INT(RND * 400000) - 200000
END IF
IF c2l& = 0 THEN
        c2h& = 200
        c2l& = 1
        RANDOMIZE TIMER
        c2x& = INT(RND * 400000) - 200000
        c2y& = INT(RND * 400000) - 200000
END IF
IF c3l& = 0 THEN
        c3h& = 200
        c3l& = 1
        RANDOMIZE TIMER
        c3x& = INT(RND * 400000) - 200000
        c3y& = INT(RND * 400000) - 200000
END IF
VIEW SCREEN (50, 435)-(80, 465)
CLS
VIEW
RETURN

endd:
CLS
VIEW
GOSUB ran:
FOR i = 1 TO stars&
PSET (stx(1, i), sty(1, i)), 15
NEXT i
LOCATE 24, 28
PRINT "Would you like to play again?"
DO
ans2$ = INKEY$
LOOP UNTIL (ans2$ = "y" OR ans2$ = "n")
IF ans2$ = "y" THEN GOTO game
IF ans2$ = "n" THEN GOSUB quit
GOTO game

quit:
VIEW
CLS
GOSUB ran
FOR i = 1 TO stars&
PSET (stx(1, i), sty(1, i)), 15
NEXT i
LOCATE 24, 28
PRINT "Do you really want to quit?"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = "y" OR ans4$ = "n")
IF ans4$ = "y" THEN
        CLS
        GOSUB ran
        FOR i = 1 TO stars&
        PSET (stx(1, i), sty(1, i)), 15
        NEXT i
        LOCATE 24, 28
        PRINT "Press any key to continue. "
        COLOR 0
        END
ELSEIF ans4$ = "n" THEN
        CLS
        GOSUB drawing
        RETURN
END IF
RETURN

quit2:
VIEW
CLS
GOSUB ran
FOR i = 1 TO stars&
PSET (stx(1, i), sty(1, i)), 15
NEXT i
LOCATE 24, 28
PRINT "Do you really want to quit?"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = "y" OR ans4$ = "n")
IF ans4$ = "y" THEN
        CLS
        GOSUB ran
        FOR i = 1 TO stars&
        PSET (stx(1, i), sty(1, i)), 15
        NEXT i
        LOCATE 24, 28
        PRINT "Press any key to continue. "
        COLOR 0
        END
ELSEIF ans4$ = "n" THEN
        CLS
        GOTO start
END IF
GOTO start

paused:
LOCATE 4, 35
PRINT "PAUSED"
SLEEP
LOCATE 4, 34
PRINT "        "
RETURN

opener:
VIEW
CLS
file1$ = ""
file2$ = ""
file3$ = ""
OPEN "SAVED" FOR INPUT AS #1
DO WHILE NOT EOF(1)
IF NOT EOF(1) THEN INPUT #1, file1$
IF NOT EOF(1) THEN INPUT #1, file2$
IF NOT EOF(1) THEN INPUT #1, file3$
LOOP
CLOSE #1
COLOR 3
LOCATE 1, 1
PRINT "Which File(C to cancel)"
LOCATE 3, 3
PRINT "1. "; file1$
LOCATE 4, 3
PRINT "2. "; file2$
LOCATE 5, 3
PRINT "3. "; file3$
COLOR 15
DO
DO
a$ = INKEY$
LOOP UNTIL (a$ = "1" OR a$ = "2" OR a$ = "3" OR a$ = "c")
IF a$ = "1" AND file1$ <> "" THEN EXIT DO
IF a$ = "2" AND file2$ <> "" THEN EXIT DO
IF a$ = "3" AND file3$ <> "" THEN EXIT DO
IF a$ = "c" THEN
        CLS
        GOTO start
END IF
LOOP
CLS
COLOR 11
LOCATE 1, 1
PRINT "O"
COLOR 3
LOCATE 1, 2
PRINT "pen"
COLOR 11
LOCATE 1, 6
PRINT "D"
LOCATE 1, 13
PRINT "C"
COLOR 3
LOCATE 1, 7
PRINT "elete"
LOCATE 1, 14
PRINT "ancel"
LOCATE 3, 2
IF a$ = "1" THEN PRINT "File: "; file1$
IF a$ = "2" THEN PRINT "File: "; file2$
IF a$ = "3" THEN PRINT "File: "; file3$
DO
b$ = INKEY$
LOOP UNTIL (b$ = "o" OR b$ = "d" OR b$ = "c")
IF b$ = "o" THEN
        IF a$ = "1" THEN filed$ = file1$
        IF a$ = "2" THEN filed$ = file2$
        IF a$ = "3" THEN filed$ = file3$
        OPEN filed$ FOR INPUT AS #1
        INPUT #1, x&, y&, laz&, lazer&, basex&, basey&, c1x&, c1y&, c2x&, c2y&, c3x&, c3y&, ra&, da&, w&, nw&, comsupermax&, com1max&, com2max&, com3max&, supmax&, senes&, max&, tor&, c1tor&, c2tor&, c3tor&, sen&, eng&, wea&, ene&, bat&, divide&, shields&, tax&, c1h&, c1l&, c2h&, c2l&, c3h&, c3l&, life1&, c1t&, c1tx&, c1ty&, c2t&, c2tx&, c2ty&, c3t&, c3tx&, c3ty&, object&, ma&, m&, po&, c1direction&, c1ddirection&, c2direction&, c2ddirection&, c3direction&, c3ddirection&, c1distance&
        CLOSE #1
        CLS
        GOSUB ran
        GOSUB drawing
        GOTO begin
ELSEIF b$ = "d" THEN
        KILL "SAVED"
        OPEN "SAVED" FOR OUTPUT AS #1
        IF a$ = "1" THEN
                WRITE #1, file2$
                WRITE #1, file3$
        ELSEIF a$ = "2" THEN
                WRITE #1, file1$
                WRITE #1, file3$
        ELSEIF a$ = "3" THEN
                WRITE #1, file1$
                WRITE #1, file2$
        END IF
        CLOSE #1
        IF a$ = "1" THEN KILL file1$
        IF a$ = "2" THEN KILL file2$
        IF a$ = "3" THEN KILL file3$
ELSEIF b$ = "c" THEN
        CLS
        GOTO start
END IF
CLS
RETURN

options:
CLS
VIEW SCREEN (50, 50)-(590, 390)
PSET (59, 76), 15
PSET (189, 71), 7
PSET (200, 100), 15
PSET (340, 200), 7
PSET (71, 260), 15
PSET (456, 84), 15
size& = 400
size2& = 400
LINE (800, -400)-(800, 400), 2
FOR i = -400 TO 400 STEP 10
LINE (800, i)-(0 - i * 2, 400), 2
NEXT i
LINE (50, 50)-(590, 390), 14, B

COLOR 4
LOCATE 12, 8
PRINT "SPACE ATTACK"
COLOR 9
LOCATE 14, 8
PRINT "By Matthew Seymour"
COLOR 3
DO
d& = CLNG((delay& + 1) / 100)
LINE (178, 135)-(282, 135), 2

CIRCLE (176, 135), 2, 2
PSET (176, 135), 2
CIRCLE (284, 135), 2, 2
PSET (284, 135), 2

CIRCLE (200, 135), 1, 2
CIRCLE (220, 135), 1, 2
CIRCLE (240, 135), 1, 2
CIRCLE (260, 135), 1, 2

CIRCLE (280 - d&, 135), 1, 10
COLOR 3
LOCATE 18, 8
PRINT "Game Speed"
LOCATE 20, 9
PRINT "tars"
LOCATE 22, 9
PRINT "eturn"
LOCATE 20, 14
PRINT stars&
COLOR 11
LOCATE 18, 19
PRINT "+ -"
LOCATE 20, 8
PRINT "S"
LOCATE 22, 8
PRINT "R"
LOCATE 18, 20
COLOR 3
PRINT "/"
COLOR 15
DO
ans$ = INKEY$
LOOP UNTIL (ans$ = "r" OR ans$ = "=" OR ans$ = "-" OR ans$ = "s")
CIRCLE (280 - d&, 135), 1, 0
IF ans$ = "=" THEN delay& = delay& - 100
IF ans$ = "-" THEN delay& = delay& + 100
IF ans$ = "s" THEN
        COLOR 3
        LOCATE 20, 14
        INPUT "", stars&
        COLOR 15
END IF
IF ans$ = "r" THEN GOTO start

IF delay& > 10000 THEN delay& = 10000
IF delay& < 0 THEN delay& = 0
LOOP

randoom:
RANDOMIZE TIMER
FOR i = 1 TO stars&
IF stx(1, i) > 640 OR stx(1, i) < 0 THEN
        PSET (stx(1, i), sty(1, i)), 0
        sty(1, i) = INT(RND * 330) + 1
        IF stx(1, i) > 640 THEN stx(1, i) = 0
        IF stx(1, i) < 0 THEN stx(1, i) = 640

ELSEIF sty(1, i) > 330 OR sty(1, i) < 0 THEN
        PSET (stx(1, i), sty(1, i)), 0
        stx(1, i) = INT(RND * 640) + 1
        IF sty(1, i) > 330 THEN sty(1, i) = 0
        IF sty(1, i) < 0 THEN sty(1, i) = 330
END IF
NEXT i
RETURN

warp:

IF m& = 1 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i), sty(1, i) - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i) - 600, sty(1, i) - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i) - 600, sty(1, i)), 7
ELSEIF m& = 4 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i) - 600, sty(1, i) + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i), sty(1, i) + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i) + 600, sty(1, i) + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i) + 600, sty(1, i)), 7
ELSEIF m& = 8 THEN
        LINE (stx(1, i), sty(1, i))-(stx(1, i) + 600, sty(1, i) - 300), 7
END IF

LINE (stx(1, i), sty(1, i))-(stx(1, i) - 600, sty(1, i) - 300), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i) + 600, sty(1, i) - 300), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i) - 600, sty(1, i) + 300), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i) + 600, sty(1, i) + 300), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i) - 600, sty(1, i)), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i), sty(1, i) - 300), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i) + 600, sty(1, i)), 0
LINE (stx(1, i), sty(1, i))-(stx(1, i), sty(1, i) + 300), 0
GOTO con6

yup:
basey& = basey& + po&
c1ty& = c1ty& + po&
c1y& = c1y& + po&
c2ty& = c2ty& + po&
c2y& = c2y& + po&
c3ty& = c3ty& + po&
c3y& = c3y& + po&
FOR i = 1 TO stars&
        IF po& <> 1 THEN sty(1, i) = sty(1, i) + (po& / 2) * sts(1, i)
        IF po& = 1 THEN sty(1, i) = sty(1, i) + po& * sts(1, i)
NEXT i
RETURN

xup:
basex& = basex& + po& * 2
c1tx& = c1tx& + po& * 2
c1x& = c1x& + po& * 2
c2tx& = c2tx& + po& * 2
c2x& = c2x& + po& * 2
c3tx& = c3tx& + po& * 2
c3x& = c3x& + po& * 2
FOR i = 1 TO stars&
        stx(1, i) = stx(1, i) + po& * sts(1, i)
NEXT i
RETURN

ydown:
basey& = basey& - po&
c1ty& = c1ty& - po&
c1y& = c1y& - po&
c2ty& = c2ty& - po&
c2y& = c2y& - po&
c3ty& = c3ty& - po&
c3y& = c3y& - po&
FOR i = 1 TO stars&
        IF po& <> 1 THEN sty(1, i) = sty(1, i) - (po& / 2) * sts(1, i)
        IF po& = 1 THEN sty(1, i) = sty(1, i) - po& * sts(1, i)
NEXT i
RETURN

xdown:
basex& = basex& - po& * 2
c1tx& = c1tx& - po& * 2
c1x& = c1x& - po& * 2
c2tx& = c2tx& - po& * 2
c2x& = c2x& - po& * 2
c3tx& = c3tx& - po& * 2
c3x& = c3x& - po& * 2
FOR i = 1 TO stars&
        stx(1, i) = stx(1, i) - po& * sts(1, i)
NEXT i
RETURN

sensors:
        IF sen& < 10 OR senes& = 0 THEN
                PAINT (10, 471), 1, 0
                PAINT (10, 350), 1, 0
                LOCATE 48, 4
                PRINT "          "
                LOCATE 49, 4
                PRINT "          "
                LOCATE 50, 4
                PRINT "          "
                LOCATE 51, 4
                PRINT "          "
                LOCATE 52, 4
                PRINT "          "
                LINE (20, 435)-(26, 465), 0, BF
                LINE (20, 465)-(26, 465), 0, BF
                LINE (30, 435)-(36, 465), 0, BF
                LINE (30, 465)-(36, 465), 0, BF
                LINE (40, 435)-(46, 465), 0, BF
                LINE (40, 465)-(46, 465), 0, BF
                LINE (50, 435)-(80, 465), 0, BF
                RETURN
        END IF
        COLOR 14
        IF object& = 1 THEN
                LOCATE 49, 4
                PRINT "Enemy 1 "
        ELSEIF object& = 2 THEN
                LOCATE 49, 4
                PRINT "Enemy 2 "
        ELSEIF object& = 3 THEN
                LOCATE 49, 4
                PRINT "Enemy 3 "
        ELSEIF object2& = 4 THEN
                LOCATE 49, 4
                PRINT "Closest "
        END IF
        dist1& = ox& - x&
        dist2& = y& - oy&
       
        IF dist1& > 0 THEN
                LOCATE 50, 4
                PRINT dist1&
                LOCATE 50, 4
                PRINT CHR$(16)
        ELSEIF dist1& < 0 THEN
                LOCATE 50, 4
                PRINT dist1&
                LOCATE 50, 4
                PRINT CHR$(17)
        ELSEIF dist1& = 0 THEN
                LOCATE 50, 4
                PRINT dist1&
        END IF
        IF dist2& > 0 THEN
                LOCATE 51, 4
                PRINT dist2&
                LOCATE 51, 4
                PRINT CHR$(24)
        ELSEIF dist2& < 0 THEN
                LOCATE 51, 4
                PRINT dist2&
                LOCATE 51, 4
                PRINT CHR$(25)
        ELSEIF dist2& = 0 THEN
                LOCATE 51, 4
                PRINT dist2&
        END IF
        dist1& = c1x& - x&
        dist2& = y& - c1y&
        dist3& = c2x& - x&
        dist4& = y& - c2y&
        dist5& = c3x& - x&
        dist6& = y& - c3y&
        LINE (50, 435)-(80, 465), 14, B
        VIEW SCREEN (50, 435)-(80, 465)
        PSET (65 + CLNG(dist1& / divide&), 450 + CLNG(-dist2& / divide&)), 4
        LINE (64 + CLNG(dist1& / divide&), 449 + CLNG(-dist2& / divide&))-(66 + CLNG(dist1& / divide&), 451 + CLNG(-dist2& / divide&)), 0, B
        PSET (65 + CLNG(dist3& / divide&), 450 + CLNG(-dist4& / divide&)), 4
        LINE (64 + CLNG(dist3& / divide&), 449 + CLNG(-dist4& / divide&))-(66 + CLNG(dist3& / divide&), 451 + CLNG(-dist4& / divide&)), 0, B
        PSET (65 + CLNG(dist5& / divide&), 450 + CLNG(-dist6& / divide&)), 4
        LINE (64 + CLNG(dist5& / divide&), 449 + CLNG(-dist6& / divide&))-(66 + CLNG(dist5& / divide&), 451 + CLNG(-dist6& / divide&)), 0, B
        PSET (65, 450), 2
        IF object& = 1 THEN
                PSET (65 + CLNG(dist1& / divide&), 450 + CLNG(-dist2& / divide&)), 14
        ELSEIF object& = 2 THEN
                PSET (65 + CLNG(dist3& / divide&), 450 + CLNG(-dist4& / divide&)), 14
        ELSEIF object& = 3 THEN
                PSET (65 + CLNG(dist5& / divide&), 450 + CLNG(-dist6& / divide&)), 14
        END IF
        VIEW
        LINE (20, 435)-(26, 465 - (CLNG(c1h& / 6.6))), 0, BF
        LINE (20, 465)-(26, 465 - (CLNG(c1h& / 6.6))), 2, BF
        LINE (30, 435)-(36, 465 - (CLNG(c2h& / 6.6))), 0, BF
        LINE (30, 465)-(36, 465 - (CLNG(c2h& / 6.6))), 2, BF
        LINE (40, 435)-(46, 465 - (CLNG(c3h& / 6.6))), 0, BF
        LINE (40, 465)-(46, 465 - (CLNG(c3h& / 6.6))), 2, BF
        COLOR 15
        RETURN
engines:
        IF eng& < 10 THEN
                PAINT (110, 471), 8, 0
                PAINT (110, 350), 8, 0
                LOCATE 48, 16
                PRINT "          "
                LOCATE 49, 16
                PRINT "          "
                LOCATE 50, 16
                PRINT "          "
                LOCATE 51, 16
                PRINT "          "
                RETURN
        END IF
        COLOR 14
        LOCATE 48, 20
        PRINT max&
        IF w& = 0 THEN
                LOCATE 51, 16
                PRINT "Idle  "
        ELSEIF w& = 1 THEN
                LOCATE 51, 16
                PRINT "Active"
        END IF
        LOCATE 49, 22
        IF w& = 0 THEN PRINT po&
        IF w& = 1 THEN PRINT " -"
        COLOR 15
        RETURN
weapons:
        IF wea& < 10 THEN
                PAINT (210, 471), 5, 0
                PAINT (210, 350), 5, 0
                LOCATE 48, 29
                PRINT "          "
                LOCATE 49, 29
                PRINT "          "
                LOCATE 50, 29
                PRINT "          "
                LOCATE 51, 29
                PRINT "          "
                LOCATE 52, 29
                PRINT "          "
                RETURN
        END IF
        COLOR 14
        LOCATE 48, 37
        PRINT tor&
        LOCATE 49, 29
        IF fin2& = 1 THEN PRINT "LOCKED ON"
        IF fin2& = 0 THEN PRINT "NO LOCK  "
        LOCATE 51, 29
        IF fin& = 1 THEN PRINT "LOCKED ON"
        IF fin& = 0 THEN PRINT "NO LOCK  "
        COLOR 15
        RETURN
lifesupport:
        IF ene& < 10 THEN
                PAINT (310, 471), 8, 0
                PAINT (310, 350), 8, 0
                LOCATE 48, 41
                PRINT "          "
                LOCATE 49, 41
                PRINT "          "
                LOCATE 50, 41
                PRINT "          "
                LOCATE 51, 41
                PRINT "          "
                RETURN
        END IF
        COLOR 14
       
        IF total& > 1 THEN LINE (325, 404)-(325, 406), 2
        IF total& > 2 THEN LINE (327, 404)-(327, 406), 2
        IF total& > 4 THEN LINE (329, 404)-(329, 406), 2
        IF total& > 8 THEN LINE (331, 404)-(331, 406), 2
        IF total& > 10 THEN LINE (333, 404)-(333, 406), 2
        IF total& > 12 THEN LINE (335, 403)-(335, 406), 2
        IF total& > 14 THEN LINE (337, 403)-(337, 406), 14
        IF total& > 16 THEN LINE (339, 403)-(339, 406), 14
        IF total& > 18 THEN LINE (339, 403)-(339, 406), 14
        IF total& > 20 THEN LINE (341, 402)-(341, 406), 14
        IF total& > 22 THEN LINE (343, 402)-(343, 406), 14
        IF total& > 24 THEN LINE (345, 401)-(345, 406), 14
        IF total& > 26 THEN LINE (347, 400)-(347, 406), 14
        IF total& > 28 THEN LINE (349, 398)-(349, 406), 14
        IF total& > 30 THEN LINE (351, 396)-(351, 406), 4
        IF total& > 32 THEN LINE (353, 394)-(353, 406), 4
        IF total& > 34 THEN LINE (355, 392)-(355, 406), 4
        IF total& > 36 THEN LINE (357, 390)-(357, 406), 4
        IF total& > 38 THEN LINE (359, 388)-(359, 406), 4
        IF total& > 40 THEN LINE (361, 386)-(361, 406), 4

        IF (325 + total& < 361) THEN LINE (325 + total&, 385)-(361, 406), 0, BF

        LINE (326, 385)-(326, 406), 0
        LINE (328, 385)-(328, 406), 0
        LINE (330, 385)-(330, 406), 0
        LINE (332, 385)-(332, 406), 0
        LINE (334, 385)-(334, 406), 0
        LINE (336, 385)-(336, 406), 0
        LINE (338, 385)-(338, 406), 0
        LINE (340, 385)-(340, 406), 0
        LINE (342, 385)-(342, 406), 0
        LINE (344, 385)-(344, 406), 0
        LINE (346, 385)-(346, 406), 0
        LINE (348, 385)-(348, 406), 0
        LINE (350, 385)-(350, 406), 0
        LINE (352, 385)-(352, 406), 0
        LINE (354, 385)-(354, 406), 0
        LINE (356, 385)-(356, 406), 0
        LINE (358, 385)-(358, 406), 0
        LINE (360, 385)-(360, 406), 0
        LINE (362, 385)-(362, 406), 0
       
       
        IF 325 + CINT(bat& / 222) < 341 THEN
                LINE (325, 422)-(325 + CINT(bat& / 222), 425), 4, BF
        ELSEIF 325 + CINT(bat& / 222) < 356 THEN
                LINE (341, 422)-(325 + CINT(bat& / 222), 425), 14, BF
                LINE (325, 422)-(340, 425), 4, BF
        ELSEIF 325 + CINT(bat& / 222) > 355 THEN
                LINE (356, 422)-(325 + CINT(bat& / 222), 425), 2, BF
                LINE (341, 422)-(355, 425), 14, BF
                LINE (325, 422)-(340, 425), 4, BF
        END IF
        IF 325 + CLNG(bat& / 222) > 355 THEN tt& = 2
        IF 325 + CLNG(bat& / 222) < 356 THEN tt& = 14
        IF 325 + CLNG(bat& / 222) < 341 THEN tt& = 4
        LINE (325, 420)-(325, 427), 4
        LINE (325 + CINT(bat& / 222), 420)-(325 + CINT(bat& / 222), 427), tt&
        LINE ((325 + CINT(bat& / 222)) - 1, 420)-((325 + CINT(bat& / 222)) - 1, 421), 0, BF
        LINE ((325 + CINT(bat& / 222)) - 1, 427)-((325 + CINT(bat& / 222)) - 1, 426), 0, BF

        LINE (375, 420)-((325 + CINT(bat& / 222)) + 1, 427), 0, BF
       
        COLOR 15
        RETURN
shields:
        COLOR 14
        LOCATE 48, 66
        PRINT shields&; "%  "
        COLOR 15
dammage: 
        COLOR 14
        LOCATE 49, 54
        PRINT sen&; "%  "
        LOCATE 51, 54
        PRINT eng&; "%  "
        LOCATE 53, 54
        PRINT wea&; "%  "
        LOCATE 55, 54
        PRINT ene&; "%  "
        LOCATE 57, 54
        IF CINT((sen& + eng& + wea& + ene&) / 4) = 100 THEN PRINT " 100 %" ELSE PRINT CINT((sen& + eng& + wea& + ene&) / 4); "% "
        COLOR 15
        RETURN

drawing:
        VIEW
        LINE (0, 331)-(640, 480), 0, BF
        LINE (25, 340)-(100, 350), 9, BF
        CIRCLE (25, 360), 10, 9, 1.3, 3
        CIRCLE (25, 360), 20, 9, 1.3, 3.1
        LINE (5, 360)-(15, 360), 9
        PAINT (10, 350), 9, 9
        LINE (25, 469)-(100, 479), 9, BF
        CIRCLE (25, 459), 10, 9, 3.2, 5
        CIRCLE (25, 459), 20, 9, 3.2, 5
        LINE (5, 459)-(15, 459), 9
        PAINT (10, 471), 9, 9
       
        LINE (125, 340)-(200, 350), 14, BF
        CIRCLE (125, 360), 10, 14, 1.3, 3
        CIRCLE (125, 360), 20, 14, 1.3, 3.1
        LINE (105, 360)-(115, 360), 14
        PAINT (110, 350), 14, 14
        LINE (125, 469)-(200, 479), 14, BF
        CIRCLE (125, 459), 10, 14, 3.2, 5
        CIRCLE (125, 459), 20, 14, 3.2, 5
        LINE (105, 459)-(115, 459), 14
        PAINT (110, 471), 14, 14
       
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
       
        LINE (325, 340)-(400, 350), 2, BF
        CIRCLE (325, 360), 10, 2, 1.3, 3
        CIRCLE (325, 360), 20, 2, 1.3, 3.1
        LINE (305, 360)-(315, 360), 2
        PAINT (310, 350), 2, 2
        LINE (325, 469)-(400, 479), 2, BF
        CIRCLE (325, 459), 10, 2, 3.2, 5
        CIRCLE (325, 459), 20, 2, 3.2, 5
        LINE (305, 459)-(315, 459), 2
        PAINT (310, 471), 2, 2
       
        LINE (425, 340)-(500, 350), 4, BF
        CIRCLE (425, 360), 10, 4, 1.3, 3
        CIRCLE (425, 360), 20, 4, 1.3, 3.1
        LINE (405, 360)-(415, 360), 4
        PAINT (410, 350), 4, 4
        LINE (425, 469)-(500, 479), 4, BF
        CIRCLE (425, 459), 10, 4, 3.2, 5
        CIRCLE (425, 459), 20, 4, 3.2, 5
        LINE (405, 459)-(415, 459), 4
        PAINT (410, 471), 4, 4
       
        LINE (525, 340)-(600, 350), 3, BF
        CIRCLE (525, 360), 10, 3, 1.3, 3
        CIRCLE (525, 360), 20, 3, 1.3, 3.1
        LINE (505, 360)-(515, 360), 3
        PAINT (510, 350), 3, 3
        LINE (525, 469)-(600, 479), 3, BF
        CIRCLE (525, 459), 10, 3, 3.2, 5
        CIRCLE (525, 459), 20, 3, 3.2, 5
        LINE (505, 459)-(515, 459), 3
        PAINT (510, 471), 3, 3
        LOCATE 46, 4
        COLOR 14
        PRINT "SENSORS"
        LOCATE 48, 4
        PRINT "Object:"
        COLOR 15
        LOCATE 46, 16
        COLOR 14
        PRINT "ENGINES"
        LOCATE 48, 16
        PRINT "Max:"
        LOCATE 49, 16
        PRINT "Speed:"
        LOCATE 50, 16
        PRINT "Warp:"
        COLOR 15
        LOCATE 46, 29
        COLOR 14
        PRINT "WEAPONS"
        LOCATE 48, 29
        PRINT "Torpedos:"
        LOCATE 50, 29
        PRINT "Laser:"
        COLOR 15
        LOCATE 46, 41
        COLOR 14
        PRINT "REACTOR"
        LOCATE 48, 41
        PRINT "Engery Use"
        LOCATE 52, 41
        PRINT "Battery"
        COLOR 15
        LOCATE 46, 66
        COLOR 14
        PRINT "SHIELDS"
        LOCATE 46, 54
        COLOR 14
        PRINT "DAMMAGE"
        LOCATE 48, 54
        PRINT "SENSORS"
        LOCATE 50, 54
        PRINT "ENGINES"
        LOCATE 52, 54
        PRINT "WEAPONS"
        LOCATE 54, 54
        PRINT "REACTOR"
        LOCATE 56, 54
        PRINT "TOTAL"
        COLOR 15
        VIEW SCREEN (0, 1)-(639, 330)
RETURN



