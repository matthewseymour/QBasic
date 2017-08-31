start:
SCREEN 12
VIEW
CLS
DO
VIEW SCREEN (50, 50)-(590, 390)
PSET (59, 76), 15
PSET (189, 71), 7
PSET (200, 100), 15
PSET (340, 200), 7
PSET (71, 260), 15
PSET (456, 84), 15
size& = 400
size2& = 400
LINE (size& * 2, -size&)-(size& * 2, size&), 2
FOR i = -size2& TO size2& STEP 10
LINE (size& * 2, i)-(0 - i * 2, size&), 2
NEXT i
LINE (50, 50)-(590, 390), 14, B
'LINE (70, 70)-(80, 70), 15
'LINE (68, 71)-(82, 71), 15
'LINE (67, 72)-(83, 72), 15

'LINE (67, 73)-(72, 73), 15
'LINE (83, 73)-(78, 73), 15
'LINE (83, 74)-(78, 74), 15
'LINE (67, 73)-(72, 78), 15, BF

'LINE (70, 79)-(82, 79), 15
'LINE (68, 80)-(83, 80), 15
'LINE (67, 78)-(80, 78), 15
'LINE (67, 79)-(72, 79), 15
'LINE (83, 81)-(78, 81), 15
'LINE (83, 82)-(78, 82), 15

COLOR 4
LOCATE 7, 35
PRINT "Space Attack"
COLOR 15
LOCATE 8, 32
PRINT "By Matthew Seymour"
LOCATE 10, 28
PRINT "Press N to start a new game"
LOCATE 11, 29
PRINT "or Q to quit at any time."
DO
ans3$ = INKEY$
LOOP UNTIL (ans3$ = "q" OR ans3$ = "n")
IF ans3$ = "q" THEN GOTO quit2
IF ans3$ = "n" THEN GOTO game
LOOP
game:
CLS
GOSUB drawing
x& = 320
y& = 170
RANDOMIZE TIMER
c1x& = INT(RND * 320) - 320
c1y& = INT(RND * 240) - 240
c2x& = INT(RND * 320) - 320
c2y& = INT(RND * 240) - 240
c3x& = INT(RND * 320) - 320
c3y& = INT(RND * 240) - 240
c1direction& = INT(RND * 8) + 1
c2direction& = INT(RND * 8) + 1
c3direction& = INT(RND * 8) + 1
c1ddirection& = INT(RND * 8) + 1
c2ddirection& = INT(RND * 8) + 1
c3ddirection& = INT(RND * 8) + 1
ra& = 1000
a& = 1
da& = 15
w& = 0
nw& = 0
supmax& = 8
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
reactor& = 20
shields& = 100
ch1& = 200
cl1& = 1
ch2& = 200
cl2& = 1
ch3& = 200
cl3& = 1
life1& = 1
c1t& = 0
c1tx& = c1x&
c1ty& = c1y&
c2t& = 0
c2tx& = c2x&
c2ty& = c2y&
c3t& = 0
c3tx& = c3x&
c3ty& = c3y&
md& = 0
RANDOMIZE TIMER
m& = INT(RND * 7) + 1
po& = INT(RND * max&)
GOSUB ran
GOTO begin
ran:
RANDOMIZE TIMER
stx1& = INT(RND * 640) + 1
sty1& = INT(RND * 330) + 21
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
distance& = INT(RND * 75) + 25
RETURN

begin:
        DO
        DO
        VIEW SCREEN (1, 2)-(638, 329)
        IF w& = 1 THEN GOTO warp
        PSET (stx1&, sty1&), 15
        IF po& > 0 THEN LINE (stx1& - po& * 2, sty1& - po&)-(stx1& + po& * 2, sty1& + po&), 0, B
        PSET (stx2&, sty2&), 7
        IF po& > 0 THEN LINE (stx2& - (po& * 2) * 2, sty2& - po& * 2)-(stx2& + (po& * 2) * 2, sty2& + po& * 2), 0, B
        PSET (stx3&, sty3&), 7
        IF po& > 0 THEN LINE (stx3& - (po& / a&) * 2, sty3& - po& / a&)-(stx3& + (po& / a&) * 2, sty3& + po& / a&), 0, B
        PSET (stx4&, sty4&), 15
        IF po& > 0 THEN LINE (stx4& - (po& * 1.5) * 2, sty4& - po& * 1.5)-(stx4& + (po& * 1.5) * 2, sty4& + po& * 1.5), 0, B
        PSET (stx5&, sty5&), 7
        IF po& > 0 THEN LINE (stx5& - po& * 2, sty5& - po&)-(stx5& + po& * 2, sty5& + po&), 0, B
        PSET (stx6&, sty6&), 7
        IF po& > 0 THEN LINE (stx6& - (po& * 3) * 2, sty6& - po& * 3)-(stx6& + (po& * 3) * 2, sty6& + po& * 3), 0, B

con6:
        IF life1& = 0 THEN GOTO youlose
        IF cl1& = 0 THEN GOTO comlose
        IF cl2& = 0 THEN GOTO comlose
        IF cl3& = 0 THEN GOTO comlose

        total& = 0
        IF bat& > 0 THEN tested& = 0
        RANDOMIZE TIMER
        reaction& = INT(RND * CINT(ene& / 10)) + 1
        IF reaction& = 1 THEN reactor& = reactor& - 1
        IF reaction& = 2 THEN reactor& = reactor& - 1
        IF reaction& = 3 THEN reactor& = reactor& - 2
        IF reaction& = 4 THEN reactor& = reactor& - 2
        IF reaction& = 6 THEN reactor& = reactor& + 7
        IF reaction& = 7 THEN reactor& = reactor& + 9
        IF reaction& = 8 THEN reactor& = reactor& + 11
        IF reaction& = 9 THEN reactor& = reactor& + 13
        IF reaction& = 10 THEN reactor& = reactor& + 15
       
        IF reactor& < 0 THEN reactor& = 0
        IF reactor& > 40 THEN reactor& = 40
        IF w& = 1 THEN total& = total& + 50
        IF w& = 0 THEN total& = total& + CINT(po& / 2)
        IF lazer& > 0 THEN total& = total& + 1
        total& = total& + 2
        IF shields& < 100 THEN total& = total& + 2
       
        FOR i = 1 TO total&
        reactor& = reactor& - 1
        IF reactor& < 0 THEN
                bat& = bat& - 1
                reactor& = 0
        END IF
        bat& = bat& + reactor&
        IF bat& > 10000 THEN bat& = 10000
        IF bat& < 0 THEN GOSUB noengery
        NEXT i
       
        shieldregen& = shieldregen& + 1
        IF shieldregen& = 50 THEN
                shieldregen& = 0
                shields& = shields& + 1
                IF shields& > 100 THEN shields& = 100
        END IF
     
        IF sen& < 0 THEN sen& = 0
        IF eng& < 0 THEN eng& = 0
        IF wea& < 0 THEN wea& = 0
        IF ene& < 0 THEN ene& = 0
       
        GOSUB randoom
        GOSUB finder
        cx& = c1x&
        cy& = c1y&
        ct& = c1t&
        findcomtor& = findcom1tor&
        GOSUB cfinder
        c1x& = cx&
        c1y& = cy&
        c1t& = ct&
        findcom1tor& = findcomtor&
        cx& = c2x&
        cy& = c2y&
        ct& = c2t&
        findcomtor& = findcom2tor&
        GOSUB cfinder
        c2x& = cx&
        c2y& = cy&
        c2t& = ct&
        findcom2tor& = findcomtor&
        cx& = c3x&
        cy& = c3y&
        ct& = c3t&
        findcomtor& = findcom3tor&
        GOSUB cfinder
        c3x& = cx&
        c3y& = cy&
        c3t& = ct&
        findcom3tor& = findcomtor&
       
        cx& = c1x&
        cy& = c1y&
        ct& = c1t&
        findcomlaz& = findcom1laz&
        GOSUB cfinder2
        c1x& = cx&
        c1y& = cy&
        c1t& = ct&
        findcom1tor& = findcomlaz&
        cx& = c2x&
        cy& = c2y&
        ct& = c2t&
        findcomtor& = findcom2laz&
        GOSUB cfinder2
        c2x& = cx&
        c2y& = cy&
        c2t& = ct&
        findcom2tor& = findcomlaz&
        cx& = c3x&
        cy& = c3y&
        ct& = c3t&
        findcomtor& = findcom3laz&
        GOSUB cfinder2
        c3x& = cx&
        c3y& = cy&
        c3t& = ct&
        findcom3laz& = findcomlaz&
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
                LINE (c3x&, c3y& + 1)-(c3x&, (c3y& + 1) + com1speed&), 0
        ELSEIF c3direction& = 1 THEN
                LINE (c3x& + 2, c3y& + 1)-((c3x& + 2) + com1speed& * 2, (c3y& + 1) + com1speed&), 0
        ELSEIF c3direction& = 8 THEN
                LINE (c3x& + 2, c3y&)-((c3x& + 2) + com1speed& * 2, c3y&), 0
        ELSEIF c3direction& = 7 THEN
                LINE (c3x& + 2, c3y& - 1)-((c3x& + 2) + com1speed& * 2, (c3y& - 1) - com1speed&), 0
        ELSEIF c3direction& = 6 THEN
                LINE (c3x&, c3y& - 2)-(c3x&, (c3y& - 2) - com1speed&), 0
        ELSEIF c3direction& = 5 THEN
                LINE (c3x& - 2, c3y& - 1)-((c3x& - 2) - com1speed& * 2, (c3y& - 1) - com1speed&), 0
        ELSEIF c3direction& = 4 THEN
                LINE (c3x& - 2, c3y&)-(c3x& - com1speed& * 2, c3y&), 0
        ELSEIF c3direction& = 3 THEN
                LINE (c3x& - 2, c3y& + 1)-(c3x& - com1speed& * 2, c3y& + com1speed&), 0
        END IF

        CIRCLE (c1tx&, c1ty&), 1, 0
        CIRCLE (c2tx&, c2ty&), 1, 0
        CIRCLE (c3tx&, c3ty&), 1, 0
        CIRCLE (tx&, ty&), 1, 0

        cx& = c1x&
        cy& = c1y&
        comspeed& = com1speed&
        distance& = c1distance&
        direction& = c1direction&
        ddirection& = c1ddirection&
        GOSUB commove
        c1ddirection& = ddirection&
        c1direction& = direction&
        c1distance& = distance&
        com1speed& = comspeed&
        c1x& = cx&
        c1y& = cy&
       
        cx& = c2x&
        cy& = c2y&
        comspeed& = com2speed&
        distance& = c2distance&
        direction& = c2direction&
        ddirection& = c2ddirection&
        GOSUB commove
        c2ddirection& = ddirection&
        c2direction& = direction&
        c2distance& = distance&
        com2speed& = comspeed&
        c2x& = cx&
        c2y& = cy&
       
        cx& = c3x&
        cy& = c3y&
        comspeed& = com3speed&
        distance& = c3distance&
        direction& = c3direction&
        ddirection& = c3ddirection&
        GOSUB commove
        c3ddirection& = ddirection&
        c3direction& = direction&
        c3distance& = distance&
        com3speed& = comspeed&
        c3x& = cx&
        c3y& = cy&

con1:   IF showshields& = 0 THEN LINE (x& - 15, y& - 15)-(x& + 15, y& + 15), 0, BF
        RANDOMIZE TIMER
        coo& = INT(RND * 2) + 1
        IF coo& = 2 THEN coo& = 9
        IF po& = 1 THEN a& = 1 ELSE a& = 2
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
                LINE (c2x& + 2, c2y& - 1)-((c2x& + 2) + coms2peed& * 2, (c2y& - 1) - com2speed&), coo&
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
       
        IF lazer& > 0 THEN GOSUB lazer
        IF c1lazer& > 0 THEN
                cx& = c1x&
                cy& = c1y&
                GOSUB clazer
        END IF
        IF fin& = 1 THEN LINE (c1x& - 5, c1y& - 5)-(c1x& + 5, c1y& + 5), 1, B
       
        FOR i = 1 TO 20
        IF c1t& = 1 THEN
                myx& = c1tx&
                myy& = c1ty&
                emx& = x&
                emy& = y&
                GOSUB tor
                c1tx& = myx&
                c1ty& = myy&
                IF hit& = 1 THEN GOSUB comhit
                c1t& = ct&
        END IF
        IF c2t& = 1 THEN
                myx& = c2tx&
                myy& = c2ty&
                emx& = x&
                emy& = y&
                GOSUB tor
                c2tx& = myx&
                c2ty& = myy&
                IF hit& = 1 THEN GOSUB comhit
                c2t& = ct&
        END IF
        IF c3t& = 1 THEN
                myx& = c3tx&
                myy& = c3ty&
                emx& = x&
                emy& = y&
                GOSUB tor
                c3tx& = myx&
                c3ty& = myy&
                IF hit& = 1 THEN GOSUB comhit
                c3t& = ct&
        END IF

        NEXT i
      
        FOR i = 1 TO 20
        IF t& = 1 THEN
                myx& = tx&
                myy& = ty&
                emx& = c1x&
                emy& = c1y&
                GOSUB tor
                tx& = myx&
                ty& = myy&
                IF hit& = 1 THEN
                        t& = 0
                        ch1& = ch1& - da& * 5
                        IF ch1& < 0 THEN GOTO explode
                        hit& = 0
                END IF
        END IF
        NEXT i
       
        
        max& = CINT(supmax& * eng& / 100)
        IF eng& < 30 THEN nw& = 1
        IF eng& > 30 THEN nw& = 0
        
        ans1$ = INKEY$
        LOOP UNTIL (ans1$ = "+" OR ans1$ = "=" OR ans1$ = "-" OR ans1$ = "r" OR ans1$ = "s" OR ans1$ = "e" OR ans1$ = "w" OR ans1$ = "t" OR ans1$ = "p" OR ans1$ = "0" OR ans1$ = "5" OR ans1$ = "8" OR ans1$ = "2" OR ans1$ = "6" OR ans1$ = "4" OR ans1$ = "q" OR ans1$ = "z" OR ans1$ = "x")
        IF ans1$ = "6" THEN
                CLS
                m& = m& - 1
                IF m& < 1 THEN m& = 8
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
        ELSEIF ans1$ = "0" THEN
                GOSUB fire2
        ELSEIF ans1$ = "5" THEN
                GOSUB fire
        ELSEIF ans1$ = "p" THEN GOSUB paused
        ELSEIF ans1$ = "z" THEN
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
        ELSEIF ans1$ = "x" THEN
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
                RANDOMIZE TIMER
                c1x& = INT(RND * 20000000) - 10000000
                c1y& = INT(RND * 20000000) - 10000000
                c1ty& = INT(RND * 20000000) - 10000000
                c1tx& = INT(RND * 20000000) - 10000000
                stx1& = INT(RND * 640) + 1
                sty1& = INT(RND * 330) + 21
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
        ELSEIF ans1$ = "q" THEN GOSUB quit
        ELSEIF ans1$ = "r" THEN
                VIEW
                CLS
                GOSUB ran
                PSET (stx1&, sty1&), 15
                PSET (stx2&, sty2&), 7
                PSET (stx3&, sty3&), 7
                PSET (stx4&, sty4&), 15
                PSET (stx5&, sty5&), 7
                PSET (stx6&, sty6&), 7
                LOCATE 12, 28
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
comhit:
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
                        IF shields& > 10 THEN showshields& = showshieds& + 3
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
                        IF shields& > 10 THEN showshields& = showshieds& + 3
                        LINE (c1tx& - 1, c1ty& - 1)-(c1tx& + 1, c1ty& + 1), 0, BF
                        LINE (c2tx& - 1, c2ty& - 1)-(c2tx& + 1, c2ty& + 1), 0, BF
                        LINE (c3tx& - 1, c3ty& - 1)-(c3tx& + 1, c3ty& + 1), 0, BF
                        d& = 0
                        ct& = 0
                        hit& = 0

commove:
        IF distance& = 0 THEN
                RANDOMIZE TIMER
                distance& = INT(RND * 75) + 25
                IF w& = 0 THEN comspeed& = INT(RND * 8) + po& ELSE comspeed& = 8
                IF comspeed& > 8 THEN comspeed& = 8
                diangle& = INT(RND * 2) + 1
                distx& = x& - cx&
                disty& = y& - cy&
                IF direction& = ddirection& THEN
                        IF distx& > 0 AND disty& > 0 THEN
                                IF distx& > disty& THEN ddirection& = 4 ELSE ddirection& = 6
                                IF diangle& = 1 THEN ddirection& = 5
                        ELSEIF distx& < 0 AND disty& < 0 THEN
                                IF distx& < disty& THEN ddirection& = 8 ELSE ddirection& = 2
                                IF diangle& = 1 THEN ddirection& = 1
                        ELSEIF distx& < 0 AND disty& > 0 THEN
                                IF distx& < -disty& THEN ddirection& = 8 ELSE ddirection& = 6
                                IF diangle& = 1 THEN ddirection& = 7
                        ELSEIF distx& > 0 AND disty& < 0 THEN
                                IF -distx& < disty& THEN ddirection& = 4 ELSE ddirection& = 2
                                IF diangle& = 1 THEN ddirection& = 3
                        END IF
                END IF
                IF direction& <> ddirection& THEN
                        IF direction& - ddirection& > 3 AND direction& - ddirection& < 9 THEN direction& = direction& + 1
                        IF direction& - ddirection& < 4 AND direction& - ddirection& > -1 THEN direction& = direction& - 1
                        IF direction& - ddirection& > -4 AND direction& - ddirection& < 0 THEN direction& = direction& + 1
                        IF direction& - ddirection& > -8 AND direction& - ddirection& < -3 THEN direction& = direction& - 1
                        IF direction& < 1 THEN direction& = 8
                        IF direction& > 8 THEN direction& = 1
                END IF
        END IF
        distance& = distance& - 1
      
        IF direction& = 1 THEN
                cx& = cx& - comspeed& * 2
                cy& = cy& - comspeed&
        ELSEIF direction& = 2 THEN
                cy& = cy& - comspeed&
        ELSEIF direction& = 3 THEN
                cx& = cx& + comspeed& * 2
                cy& = cy& - comspeed&
        ELSEIF direction& = 4 THEN
                cx& = cx& + comspeed& * 2
        ELSEIF direction& = 5 THEN
                cx& = cx& + comspeed& * 2
                cy& = cy& + comspeed&
        ELSEIF direction& = 6 THEN
                cy& = cy& + comspeed&
        ELSEIF direction& = 7 THEN
                cx& = cx& - comspeed& * 2
                cy& = cy& + comspeed&
        ELSEIF direction& = 8 THEN
                cx& = cx& - comspeed& * 2
        END IF
RETURN
            
noengery:
        IF w& = 1 THEN
                w& = 0
                po& = 8
                tested& = tested& + 1
                total& = total& - 10
        ELSEIF tested& = 1 AND lazer& > 0 THEN
                tested& = tested& + 1
                total& = total& - 2
                lazer& = 0
        ELSEIF tested& = 2 AND shields& < 100 THEN
                tested& = tested& + 1
                shieldregen& = 0
                total& = total& - 2
        ELSEIF tested& = 3 THEN
                tested& = tested& + 1
                senes& = 0
                total& = total& - 2
        ELSEIF tested& = 4 THEN
                tested& = tested& + 1
                DO
                po& = po& - 2
                total& = total& - 1
                IF po& < 0 THEN
                        po& = 0
                        bat& = 0
                EXIT DO
                END IF
        LOOP
        END IF
        RETURN
finder:
        fin2& = 0
        IF life1& = 0 THEN RETURN
        dist1& = x& - c1x&
        dist2& = y& - c1y&
        IF dist1& > ra& THEN GOTO finder2
        IF dist1& < -ra& THEN GOTO finder2
        IF dist2& > ra& THEN GOTO finder2
        IF dist2& < -ra& THEN GOTO finder2
        fin2& = 1
        GOTO finder2
finder2:
        fin& = 0
        IF life1& = 0 THEN RETURN
        dist1& = x& - c1x&
        dist2& = y& - c1y&
        IF dist1& > 170 THEN RETURN
        IF dist1& < -170 THEN RETURN
        IF dist2& > 170 THEN RETURN
        IF dist2& < -170 THEN RETURN
        fin& = 1
        RETURN
fire:
        IF fin& = 1 THEN lazer& = 5
        RETURN
lazer:
        IF fin& = 0 THEN RETURN
        lazer& = lazer& - 1
        IF cl1& = 0 THEN RETURN
        LINE (c1x& - 1, c1y& - 1)-(x&, y&), 1
        LINE (c1x&, c1y&)-(x&, y&), 1
        LINE (c1x& + 1, c1y& + 1)-(x&, y&), 1
        SOUND 3000, .1
        ch1& = ch1& - 1
        IF ch1& <= 0 THEN GOSUB explode
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
        cl1& = 0
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
        GOSUB comlose
        GOTO begin
cfinder:
        findcomtor& = 0
        IF life1& = 0 THEN RETURN
        dist1& = cx& - x&
        dist12& = cy& - y&
        IF dist1& > ra& THEN RETURN
        IF dist1& < -ra& THEN RETURN
        IF dist12& > ra& THEN RETURN
        IF dist12& < -ra& THEN RETURN
        findcom1tor& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB cfire ELSE RETURN
        RETURN
cfinder2:
        findcomlaz& = 0
        IF life1& = 0 THEN RETURN
        dist1& = cx& - x&
        dist12& = cy& - y&
        IF dist1& > 170 THEN RETURN
        IF dist1& < -170 THEN RETURN
        IF dist12& > 170 THEN RETURN
        IF dist12& < -170 THEN RETURN
        findcom1laz& = 1
        RANDOMIZE TIMER
        will& = INT(RND * 10) + 1
        IF will& = 1 THEN GOSUB cfirelaz ELSE RETURN
        RETURN
cfirelaz:
        IF clazer > 0 THEN RETURN
        IF findcomlaz& = 1 THEN clazer& = 5
        RETURN
clazer:
        IF findcom1laz& = 0 THEN RETURN
        clazer& = clazer& - 1
        IF life1& = 0 THEN RETURN
        LINE (cx&, cy&)-(x&, y&), 4
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
cfire:
        IF ct& = 1 THEN RETURN
        IF ct& = 0 THEN ctx& = cx&
        IF ct& = 0 THEN cty& = cy&
        IF findcomtor& = 0 THEN RETURN
        IF cl& = 0 THEN RETURN
        SOUND 3000, .1
        ct& = 1
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
        PSET (stx1&, sty1&), 15
        PSET (stx2&, sty2&), 7
        PSET (stx3&, sty3&), 7
        PSET (stx4&, sty4&), 15
        PSET (stx5&, sty5&), 7
        PSET (stx6&, sty6&), 7
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
        IF size& > 210 THEN EXIT DO
        LOOP
        CLS
        RETURN

youlose:
VIEW
CLS
GOSUB ran
PSET (stx1&, sty1&), 15
PSET (stx2&, sty2&), 7
PSET (stx3&, sty3&), 7
PSET (stx4&, sty4&), 15
PSET (stx5&, sty5&), 7
PSET (stx6&, sty6&), 7
LOCATE 12, 34
PRINT "You lost!"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = " ")
GOTO endd

comlose:
ch1& = 200
cl1& = 1
RANDOMIZE TIMER
c1x& = INT(RND * 400000) - 200000
c1y& = INT(RND * 400000) - 200000
RETURN

endd:
CLS
VIEW
GOSUB ran:
PSET (stx1&, sty1&), 15
PSET (stx2&, sty2&), 7
PSET (stx3&, sty3&), 7
PSET (stx4&, sty4&), 15
PSET (stx5&, sty5&), 7
PSET (stx6&, sty6&), 7
LOCATE 12, 28
PRINT "Would you like to play again"
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
PSET (stx1&, sty1&), 15
PSET (stx2&, sty2&), 7
PSET (stx3&, sty3&), 7
PSET (stx4&, sty4&), 15
PSET (stx5&, sty5&), 7
PSET (stx6&, sty6&), 7
LOCATE 12, 28
PRINT "Do you really want to quit?"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = "y" OR ans4$ = "n")
IF ans4$ = "y" THEN
        CLS
        GOSUB ran
        PSET (stx1&, sty1&), 15
        PSET (stx2&, sty2&), 7
        PSET (stx3&, sty3&), 7
        PSET (stx4&, sty4&), 15
        PSET (stx5&, sty5&), 7
        PSET (stx6&, sty6&), 7
        LOCATE 12, 28
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
PSET (stx1&, sty1&), 15
PSET (stx2&, sty2&), 7
PSET (stx3&, sty3&), 7
PSET (stx4&, sty4&), 15
PSET (stx5&, sty5&), 7
PSET (stx6&, sty6&), 7
LOCATE 12, 28
PRINT "Do you really want to quit?"
DO
ans4$ = INKEY$
LOOP UNTIL (ans4$ = "y" OR ans4$ = "n")
IF ans4$ = "y" THEN
        CLS
        GOSUB ran
        PSET (stx1&, sty1&), 15
        PSET (stx2&, sty2&), 7
        PSET (stx3&, sty3&), 7
        PSET (stx4&, sty4&), 15
        PSET (stx5&, sty5&), 7
        PSET (stx6&, sty6&), 7
        LOCATE 12, 28
        PRINT "Press any key to continue. "
        COLOR 0
        END
ELSEIF ans4$ = "n" THEN
        CLS
        GOTO start
END IF
GOTO start

paused:
LOCATE 2, 35
PRINT "PAUSED"
SLEEP
LOCATE 2, 34
PRINT "        "
RETURN

randoom:
RANDOMIZE TIMER
IF stx1& > 640 OR stx1& < 0 THEN
        PSET (stx1&, sty1&), 0
        sty1& = INT(RND * 330) + 1
        IF stx1& > 640 THEN stx1& = 0
        IF stx1& < 0 THEN stx1& = 640

ELSEIF sty1& > 330 OR sty1& < 0 THEN
        PSET (stx1&, sty1&), 0
        stx1& = INT(RND * 640) + 1
        IF sty1& > 330 THEN sty1& = 0
        IF sty1& < 0 THEN sty1& = 330
       
ELSEIF stx2& > 640 OR stx2& < 0 THEN
        PSET (stx2&, sty2&), 0
        sty2& = INT(RND * 330) + 1
        IF stx2& > 640 THEN stx2& = 0
        IF stx2& < 0 THEN stx2& = 640

ELSEIF sty2& > 330 OR sty2& < 0 THEN
        PSET (stx2&, sty2&), 0
        stx2& = INT(RND * 640) + 1
        IF sty2& > 330 THEN sty2& = 0
        IF sty2& < 0 THEN sty2& = 330
         
ELSEIF stx3& > 640 OR stx3& < 0 THEN
        PSET (stx3&, sty3&), 0
        sty3& = INT(RND * 330) + 1
        IF stx3& > 640 THEN stx3& = 0
        IF stx3& < 0 THEN stx3& = 640

ELSEIF sty3& > 330 OR sty3& < 0 THEN
        PSET (stx3&, sty3&), 0
        stx3& = INT(RND * 640) + 1
        IF sty3& > 330 THEN sty3& = 0
        IF sty3& < 0 THEN sty3& = 330
       
ELSEIF stx4& > 640 OR stx4& < 0 THEN
        PSET (stx4&, sty4&), 0
        sty4& = INT(RND * 330) + 1
        IF stx4& > 640 THEN stx4& = 0
        IF stx4& < 0 THEN stx4& = 640

ELSEIF sty4& > 330 OR sty4& < 0 THEN
        PSET (stx4&, sty4&), 0
        stx4& = INT(RND * 640) + 1
        IF sty4& > 330 THEN sty4& = 0
        IF sty4& < 0 THEN sty4& = 330
       
ELSEIF stx5& > 640 OR stx5& < 0 THEN
        PSET (stx5&, sty5&), 0
        sty5& = INT(RND * 330) + 1
        IF stx5& > 640 THEN stx5& = 0
        IF stx5& < 0 THEN stx5& = 640

ELSEIF sty5& > 330 OR sty5& < 0 THEN
        PSET (stx5&, sty5&), 0
        stx5& = INT(RND * 640) + 1
        IF sty5& > 330 THEN sty5& = 0
        IF sty5& < 0 THEN sty5& = 330
       
ELSEIF stx6& > 640 OR stx6& < 0 THEN
        PSET (stx6&, sty6&), 0
        sty6& = INT(RND * 330) + 1
        IF stx6& > 640 THEN stx6& = 0
        IF stx6& < 0 THEN stx6& = 640

ELSEIF sty6& > 330 OR sty6& < 0 THEN
        PSET (stx6&, sty6&), 0
        stx6& = INT(RND * 640) + 1
        IF sty6& > 330 THEN sty6& = 0
        IF sty6& < 0 THEN sty6& = 330
        

END IF
RETURN

warp:

IF m& = 1 THEN
        LINE (stx1&, sty1&)-(stx1&, sty1& - 300), 7
ELSEIF m& = 2 THEN
        LINE (stx1&, sty1&)-(stx1& - 600, sty1& - 300), 7
ELSEIF m& = 3 THEN
        LINE (stx1&, sty1&)-(stx1& - 600, sty1&), 7
ELSEIF m& = 4 THEN
        LINE (stx1&, sty1&)-(stx1& - 600, sty1& + 300), 7
ELSEIF m& = 5 THEN
        LINE (stx1&, sty1&)-(stx1&, sty1& + 300), 7
ELSEIF m& = 6 THEN
        LINE (stx1&, sty1&)-(stx1& + 600, sty1& + 300), 7
ELSEIF m& = 7 THEN
        LINE (stx1&, sty1&)-(stx1& + 600, sty1&), 7
ELSEIF m& = 8 THEN
        LINE (stx1&, sty1&)-(stx1& + 600, sty1& - 300), 7
END IF
LINE (stx1&, sty1&)-(stx1& - 600, sty1& - 300), 0
LINE (stx1&, sty1&)-(stx1& + 600, sty1& - 300), 0
LINE (stx1&, sty1&)-(stx1& - 600, sty1& + 300), 0
LINE (stx1&, sty1&)-(stx1& + 600, sty1& + 300), 0
LINE (stx1&, sty1&)-(stx1& - 600, sty1&), 0
LINE (stx1&, sty1&)-(stx1&, sty1& - 300), 0
LINE (stx1&, sty1&)-(stx1& + 600, sty1&), 0
LINE (stx1&, sty1&)-(stx1&, sty1& + 300), 0
GOTO con6

yup:
c1ty& = c1ty& + po&
c1y& = c1y& + po&
c2ty& = c2ty& + po&
c2y& = c2y& + po&
c3ty& = c4ty& + po&
c3y& = c3y& + po&
sty1& = sty1& + po&
sty2& = sty2& + po& * 2
sty3& = sty3& + po& / a&
sty4& = sty4& + po& * 1.5
sty5& = sty5& + po&
sty6& = sty6& + po& * 3
RETURN

xup:
c1tx& = c1tx& + po& * 2
c1x& = c1xy& + po& * 2
c2tx& = c2tx& + po& * 2
c2x& = c2x& + po& * 2
c3tx& = c3tx& + po& * 2
c3x& = c3x& + po& * 2
stx1& = stx1& + po& * 2
stx2& = stx2& + (po& * 2) * 2
stx3& = stx3& + (po& / a&) * 2
stx4& = stx4& + (po& * 1.5) * 2
stx5& = stx5& + po& * 2
stx6& = stx6& + (po& * 3) * 2
RETURN

ydown:
c1ty& = c1ty& - po&
c1y& = c1y& - po&
c2tx& = c2ty& - po& * 2
c2x& = c2y& - po& * 2
c3tx& = c3ty& - po& * 2
c3x& = c3y& - po& * 2
sty1& = sty1& - po&
sty2& = sty2& - po& * 2
sty3& = sty3& - po& / a&
sty4& = sty4& - po& * 1.5
sty5& = sty5& - po&
sty6& = sty6& - po& * 3
RETURN

xdown:
c1tx& = c1tx& - po& * 2
c1x& = c1x& - po& * 2
c2tx& = c2tx& - po& * 2
c2x& = c2x& - po& * 2
c3tx& = c3tx& - po& * 2
c3x& = c3x& - po& * 2
stx1& = stx1& - po& * 2
stx2& = stx2& - (po& * 2) * 2
stx3& = stx3& - (po& / a&) * 2
stx4& = stx4& - (po& * 1.5) * 2
stx5& = stx5& - po& * 2
stx6& = stx6& - (po& * 3) * 2
RETURN

sensors:
        IF sen& < 10 OR senes& = 0 THEN
                PAINT (10, 471), 5, 0
                PAINT (10, 350), 5, 0
                LOCATE 24, 4
                PRINT "          "
                LOCATE 25, 4
                PRINT "          "
                LOCATE 26, 4
                PRINT "          "
                LOCATE 27, 4
                PRINT "          "
                LOCATE 28, 4
                PRINT "          "
                LINE (50, 435)-(80, 465), 0, BF
                RETURN
        END IF
        COLOR 14
        
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
        dist1& = c1x& - x&
        dist2& = c1y& - y&
        dist3& = c2x& - x&
        dist4& = c2y& - y&
        dist5& = c3x& - x&
        dist6& = c3y& - y&
       
        LINE (50, 435)-(80, 465), 14, B
        VIEW SCREEN (50, 435)-(80, 465)
        PSET (65 + CLNG(dist1& / divide&), 450 + CLNG(-dist2& / divide&)), 4
        LINE (64 + CLNG(dist1& / divide&), 449 + CLNG(-dist2& / divide&))-(66 + CLNG(dist1& / divide&), 451 + CLNG(-dist2& / divide&)), 0, B
        PSET (65 + CLNG(dist3& / divide&), 450 + CLNG(-dist4& / divide&)), 14
        LINE (64 + CLNG(dist3& / divide&), 449 + CLNG(-dist4& / divide&))-(66 + CLNG(dist3& / divide&), 451 + CLNG(-dist4& / divide&)), 0, B
        PSET (65 + CLNG(dist5& / divide&), 450 + CLNG(-dist6& / divide&)), 5
        LINE (64 + CLNG(dist5& / divide&), 449 + CLNG(-dist6& / divide&))-(66 + CLNG(dist5& / divide&), 451 + CLNG(-dist6& / divide&)), 0, B
        PSET (65, 450), 2
        'LINE (25, 450)-(75, 455), 2, BF
        'LINE (25, 450)-(((200 - ch1&) / 4) + 25, 455), 4, BF
        VIEW
        COLOR 15
        RETURN
engines:
        IF eng& < 10 THEN
                PAINT (110, 471), 5, 0
                PAINT (110, 350), 5, 0
                LOCATE 24, 16
                PRINT "          "
                LOCATE 25, 16
                PRINT "          "
                LOCATE 26, 16
                PRINT "          "
                LOCATE 27, 16
                PRINT "          "
                RETURN
        END IF
        COLOR 14
        LOCATE 25, 20
        PRINT max&
        IF w& = 0 THEN
                LOCATE 27, 16
                PRINT "Idle  "
        ELSEIF w& = 1 THEN
                LOCATE 27, 16
                PRINT "Active"
        END IF
        LOCATE 24, 22
        IF w& = 0 THEN PRINT po&
        IF w& = 1 THEN PRINT " -"
        COLOR 15
        RETURN
weapons:
        IF wea& < 10 THEN
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
        LOCATE 25, 37
        PRINT tor&
        LOCATE 26, 29
        IF fin2& = 1 THEN PRINT "LOCKED ON"
        IF fin2& = 0 THEN PRINT "NO LOCK  "
        LOCATE 28, 29
        IF fin& = 1 THEN PRINT "LOCKED ON"
        IF fin& = 0 THEN PRINT "NO LOCK  "
        COLOR 15
        RETURN
lifesupport:
        IF ene& < 10 THEN
                PAINT (310, 471), 5, 0
                PAINT (310, 350), 5, 0
                LOCATE 25, 41
                PRINT "          "
                LOCATE 26, 41
                PRINT "          "
                LOCATE 27, 41
                PRINT "          "
                RETURN
        END IF
        COLOR 14
       
        LINE (reactor& + 325, 396)-(366, 392), 4, BF
        LINE (325, 392)-(reactor& + 325, 396), 2, BF
        IF reactor& > total& - 1 THEN
                LINE (325 + reactor&, 392)-((reactor& - total&) + 325, 396), 14, BF
        ELSE
                LINE (325 + reactor&, 392)-(325, 396), 14, BF
        END IF
       
        tt& = POINT(325, 392)
        LINE (325, 390)-(325, 396), tt&
        tt& = POINT(335, 392)
        LINE (335, 390)-(335, 396), tt&
        tt& = POINT(345, 392)
        LINE (345, 390)-(345, 396), tt&
        tt& = POINT(355, 392)
        LINE (355, 390)-(355, 396), tt&
        tt& = POINT(365, 392)
        LINE (365, 389)-(365, 396), tt&
       
        LINE (326, 389)-(326, 396), 0
        LINE (328, 389)-(328, 396), 0
        LINE (330, 389)-(330, 396), 0
        LINE (332, 389)-(332, 396), 0
        LINE (334, 389)-(334, 396), 0
        LINE (336, 389)-(336, 396), 0
        LINE (338, 389)-(338, 396), 0
        LINE (340, 389)-(340, 396), 0
        LINE (342, 389)-(342, 396), 0
        LINE (344, 389)-(344, 396), 0
        LINE (346, 389)-(346, 396), 0
        LINE (348, 389)-(348, 396), 0
        LINE (350, 389)-(350, 396), 0
        LINE (352, 389)-(352, 396), 0
        LINE (354, 389)-(354, 396), 0
        LINE (356, 389)-(356, 396), 0
        LINE (358, 389)-(358, 396), 0
        LINE (360, 389)-(360, 396), 0
        LINE (362, 389)-(362, 396), 0
        LINE (364, 389)-(364, 396), 0
        LINE (366, 389)-(366, 396), 0
       
        LINE (325, 420)-(340, 427), 4, BF
        LINE (340, 420)-(355, 427), 14, BF
        LINE (355, 420)-(370, 427), 2, BF
        LINE (375, 420)-(325 + CINT(bat& / 222), 427), 0, BF
       
        LINE (326, 420)-(326, 427), 0
        LINE (328, 420)-(328, 427), 0
        LINE (330, 420)-(330, 427), 0
        LINE (332, 420)-(332, 427), 0
        LINE (334, 420)-(334, 427), 0
        LINE (336, 420)-(336, 427), 0
        LINE (338, 420)-(338, 427), 0
        LINE (340, 420)-(340, 427), 0
        LINE (342, 420)-(342, 427), 0
        LINE (344, 420)-(344, 427), 0
        LINE (346, 420)-(346, 427), 0
        LINE (348, 420)-(348, 427), 0
        LINE (350, 420)-(350, 427), 0
        LINE (352, 420)-(352, 427), 0
        LINE (354, 420)-(354, 427), 0
        LINE (356, 420)-(356, 427), 0
        LINE (358, 420)-(358, 427), 0
        LINE (360, 420)-(360, 427), 0
        LINE (362, 420)-(362, 427), 0
        LINE (364, 420)-(364, 427), 0
        LINE (366, 420)-(366, 427), 0
        LINE (368, 420)-(368, 427), 0
        LINE (370, 420)-(370, 427), 0
        COLOR 15
        RETURN
shields:
        COLOR 14
        LOCATE 25, 53
        PRINT shields&; "%  "
        COLOR 15
dammage:
        COLOR 14
        LOCATE 24, 73
        PRINT sen&; "%  "
        LOCATE 25, 73
        PRINT eng&; "%  "
        LOCATE 26, 73
        PRINT wea&; "%  "
        LOCATE 27, 73
        PRINT ene&; "%  "
        LOCATE 28, 72
        IF CINT((sen& + eng& + wea& + ene&) / 4) = 100 THEN PRINT "100 %" ELSE PRINT CINT((sen& + eng& + wea& + ene&) / 4); "% "
        COLOR 15
        RETURN

drawing:
        VIEW
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
        LOCATE 23, 4
        COLOR 14
        PRINT "SENSORS"
        LOCATE 24, 4
        PRINT "Object:"
        LOCATE 25, 4
        PRINT "Enemy 1"
        'LOCATE 28, 4
        'PRINT "Dammage"
        COLOR 15
        LOCATE 23, 16
        COLOR 14
        PRINT "ENGINES"
        LOCATE 25, 16
        PRINT "Max:"
        LOCATE 24, 16
        PRINT "Speed:"
        LOCATE 26, 16
        PRINT "Warp:"
        COLOR 15
        LOCATE 23, 29
        COLOR 14
        PRINT "WEAPONS"
        LOCATE 25, 29
        PRINT "Torpedos:"
        LOCATE 27, 29
        PRINT "Laser:"
        COLOR 15
        LOCATE 23, 41
        COLOR 14
        PRINT "REACTOR"
        LOCATE 24, 41
        PRINT "Reactor:"
        LOCATE 26, 41
        PRINT "Battery"
        COLOR 15
        LOCATE 23, 54
        COLOR 14
        PRINT "SHIELDS"
        LOCATE 23, 66
        COLOR 14
        PRINT "DAMMAGE"
        LOCATE 24, 66
        PRINT "SENSORS"
        LOCATE 25, 66
        PRINT "ENGINES"
        LOCATE 26, 66
        PRINT "WEAPONS"
        LOCATE 27, 66
        PRINT "REACTOR"
        LOCATE 28, 66
        PRINT "TOTAL"
        COLOR 15
        VIEW SCREEN (0, 1)-(639, 330)
        RETURN



