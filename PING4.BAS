SCREEN 12
WIDTH 80, 60
CLS
COLOR 3
PRINT "Is Player 1 a computer?"
INPUT "", a$
IF a$ = "y" THEN p1com& = 1
IF p1com& = 1 THEN
        PRINT "What is the difficulty?(1 to 480)"
        INPUT "", p1ai&
END IF
PRINT "Is Player 2 a computer?"
INPUT "", a$
IF a$ = "y" THEN p2com& = 1
IF p2com& = 1 THEN
        PRINT "What is the difficulty?(1 to 480)"
        INPUT "", p2ai&
END IF
PRINT "Is Player 3 a computer?"
INPUT "", a$
IF a$ = "y" THEN p3com& = 1
IF p3com& = 1 THEN
        PRINT "What is the difficulty?(1 to 480)"
        INPUT "", p3ai&
END IF
PRINT "Is Player 4 a computer?"
INPUT "", a$
IF a$ = "y" THEN p4com& = 1
IF p4com& = 1 THEN
        PRINT "What is the difficulty?(1 to 480)"
        INPUT "", p4ai&
END IF
COLOR 15
CLS
IF p1ai& > 480 THEN p1ai& = 480
IF p1ai& < 0 THEN p1ai& = 0
IF p2ai& > 480 THEN p2ai& = 480
IF p2ai& < 0 THEN p2ai& = 0
IF p3ai& > 480 THEN p3ai& = 480
IF p3ai& < 0 THEN p3ai& = 0
IF p4ai& > 480 THEN p4ai& = 480
IF p4ai& < 0 THEN p4ai& = 0

delay& = 0

p1des& = 240
p2des& = 240
p3des& = 240
p4des& = 240

p1pad& = 240
p2pad& = 240
p3pad& = 240
p4pad& = 240

p1size& = 25
p2size& = 25
p3size& = 25
p4size& = 25

p1speed& = 3
p2speed& = 3
p3speed& = 3
p4speed& = 3

ballspeed& = 4

different& = 6

RANDOMIZE TIMER
item1t& = INT(RND * different&) + 1
item1x& = INT(RND * 360) + 60
item1y& = INT(RND * 360) + 60
item2t& = INT(RND * different&) + 1
item2x& = INT(RND * 360) + 60
item2y& = INT(RND * 360) + 60
item3t& = INT(RND * different&) + 1
item3x& = INT(RND * 360) + 60
item3y& = INT(RND * 360) + 60
item4t& = INT(RND * different&) + 1
item4x& = INT(RND * 360) + 60
item4y& = INT(RND * 360) + 60
item5t& = INT(RND * different&) + 1
item5x& = INT(RND * 360) + 60
item5y& = INT(RND * 360) + 60
serve& = INT(RND * 4) + 1

lasthit& = serve&
combo& = 1

start:

DO

FOR i = 1 TO delay&
NEXT i

LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF

IF serve& = 1 THEN
        x& = p1pad&
        y& = 13
ELSEIF serve& = 2 THEN
        x& = 13
        y& = p2pad&
ELSEIF serve& = 3 THEN
        x& = p3pad&
        y& = 467
ELSEIF serve& = 4 THEN
        x& = 467
        y& = p4pad&
END IF

IF scombo& > -1 THEN
        LOCATE 30, 30
        COLOR 0
        PRINT "    "
        IF scombo& > 0 THEN
                LOCATE 30, 30
                COLOR 3
                PRINT combo&
        END IF
        scombo& = scombo& - 1
END IF

IF balldir& = 1 THEN
        y& = y& - ballspeed&
ELSEIF balldir& = 2 THEN
        y& = y& - ballspeed&
        x& = x& + (ballspeed& / 2)
ELSEIF balldir& = 3 THEN
        y& = y& - ballspeed&
        x& = x& + ballspeed&
ELSEIF balldir& = 4 THEN
        y& = y& - (ballspeed& / 2)
        x& = x& + ballspeed&
ELSEIF balldir& = 5 THEN
        x& = x& + ballspeed&
ELSEIF balldir& = 6 THEN
        x& = x& + ballspeed&
        y& = y& + (ballspeed& / 2)
ELSEIF balldir& = 7 THEN
        x& = x& + ballspeed&
        y& = y& + ballspeed&
ELSEIF balldir& = 8 THEN
        x& = x& + (ballspeed& / 2)
        y& = y& + ballspeed&
ELSEIF balldir& = 9 THEN
        y& = y& + ballspeed&
ELSEIF balldir& = 10 THEN
        y& = y& + ballspeed&
        x& = x& - (ballspeed& / 2)
ELSEIF balldir& = 11 THEN
        y& = y& + ballspeed&
        x& = x& - ballspeed&
ELSEIF balldir& = 12 THEN
        y& = y& + (ballspeed& / 2)
        x& = x& - ballspeed&
ELSEIF balldir& = 13 THEN
        x& = x& - ballspeed&
ELSEIF balldir& = 14 THEN
        x& = x& - ballspeed&
        y& = y& - (ballspeed& / 2)
ELSEIF balldir& = 15 THEN
        x& = x& - ballspeed&
        y& = y& - ballspeed&
ELSEIF balldir& = 16 THEN
        x& = x& - (ballspeed& / 2)
        y& = y& - ballspeed&
END IF

IF x& < 60 AND y& < 60 THEN
        FOR i = 1 TO 60
                IF x& < i AND y& < (60 - i) THEN
                        IF balldir& = 16 THEN balldir& = 6
                        IF balldir& = 15 THEN balldir& = 8
                        IF balldir& = 14 THEN balldir& = 8
                        x& = i
                        y& = 60 - i
                        x& = x& + 8
                        y& = y& + 8
                        SOUND 750, .1
                END IF
        NEXT i
        IF x& < 8 THEN x& = x& + 8
        IF y& < 8 THEN y& = y& + 8
END IF
IF x& > 420 AND y& < 60 THEN
        FOR i = 1 TO 60
                IF x& > (480 - i) AND y& < (60 - i) THEN
                        IF balldir& = 2 THEN balldir& = 12
                        IF balldir& = 3 THEN balldir& = 10
                        IF balldir& = 4 THEN balldir& = 10
                        x& = 480 - i
                        y& = 60 - i
                        x& = x& - 8
                        y& = y& + 8
                        SOUND 750, .1
                END IF
        NEXT i
        IF x& > 472 THEN x& = x& - 8
        IF y& < 8 THEN y& = y& + 8
END IF
IF x& < 60 AND y& > 420 THEN
        FOR i = 1 TO 60
                IF x& < i AND y& > (420 + i) THEN
                        IF balldir& = 12 THEN balldir& = 2
                        IF balldir& = 11 THEN balldir& = 4
                        IF balldir& = 10 THEN balldir& = 4
                        x& = i
                        y& = 420 + i
                        x& = x& + 8
                        y& = y& - 8
                        SOUND 750, .1
                END IF
        NEXT i
        IF x& < 8 THEN x& = x& + 8
        IF y& > 472 THEN y& = y& - 8
END IF
IF x& > 420 AND y& > 420 THEN
        FOR i = 1 TO 60
                IF x& > (480 - i) AND y& > (420 + i) THEN
                        IF balldir& = 6 THEN balldir& = 16
                        IF balldir& = 7 THEN balldir& = 14
                        IF balldir& = 8 THEN balldir& = 14
                        x& = 480 - i
                        y& = 420 + i
                        x& = x& - 8
                        y& = y& - 8
                        SOUND 750, .1
                END IF
        NEXT i
        IF x& > 472 THEN x& = x& - 8
        IF y& > 472 THEN y& = y& - 8
END IF

IF ballspeed& = 2 THEN bc& = 8
IF ballspeed& = 4 THEN bc& = 7
IF ballspeed& = 8 THEN bc& = 15

LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), bc&, BF

LOCATE 2, 62
COLOR 1
PRINT "Score:"; p1score&
LOCATE 3, 62
IF p1com& = 1 THEN PRINT "COMPUTER"
IF p1com& = 1 THEN GOTO pp2
PRINT "HUMAN"
LOCATE 4, 62
PRINT "Keys:"
LOCATE 5, 62
PRINT "G-Serve"
LOCATE 6, 62
PRINT "V-Left"
LOCATE 7, 62
PRINT "B-Stop"
LOCATE 8, 62
PRINT "N - Right"

pp2:

LOCATE 17, 62
COLOR 4
PRINT "Score:"; p2score&
LOCATE 18, 62
IF p2com& = 1 THEN PRINT "COMPUTER"
IF p2com& = 1 THEN GOTO pp3
PRINT "HUMAN"
LOCATE 19, 62
PRINT "Keys:"
LOCATE 20, 62
PRINT "S-Serve"
LOCATE 21, 62
PRINT "Q-Up"
LOCATE 22, 62
PRINT "A-Stop"
LOCATE 23, 62
PRINT "Z-Down"

pp3:

LOCATE 32, 62
COLOR 2
PRINT "Score:"; p3score&
LOCATE 33, 62
IF p3com& = 1 THEN PRINT "COMPUTER"
IF p3com& = 1 THEN GOTO pp4
PRINT "HUMAN"
LOCATE 34, 62
PRINT "Keys:"
LOCATE 35, 62
PRINT "5-Serve"
LOCATE 36, 62
PRINT "1-Left"
LOCATE 37, 62
PRINT "2-Stop"
LOCATE 38, 62
PRINT "3-Right"

pp4:

LOCATE 47, 62
COLOR 14
PRINT "Score:"; p4score&
LOCATE 48, 62
IF p4com& = 1 THEN PRINT "COMPUTER"
IF p4com& = 1 THEN GOTO ff
PRINT "HUMAN"
LOCATE 49, 62
PRINT "Keys:"
LOCATE 50, 62
PRINT "; -Serve"
LOCATE 51, 62
PRINT "] -Up"
LOCATE 52, 62
PRINT "' -Stop"
LOCATE 53, 62
PRINT "/ -Down"

ff:

COLOR 15

LINE (item1x& - 2, item1y& - 2)-(item1x& + 2, item1y& + 2), 9, BF
LINE (item2x& - 2, item2y& - 2)-(item2x& + 2, item2y& + 2), 9, BF
LINE (item3x& - 2, item3y& - 2)-(item3x& + 2, item3y& + 2), 9, BF
LINE (item4x& - 2, item4y& - 2)-(item4x& + 2, item4y& + 2), 9, BF
LINE (item5x& - 2, item5y& - 2)-(item5x& + 2, item5y& + 2), 9, BF
                                                             
IF x& < item1x& + 9 THEN
        IF x& > item1x& - 9 THEN
                IF y& < item1y& + 9 THEN
                        IF y& > item1y& - 9 THEN
                                GOSUB item1
                        END IF
                END IF
        END IF
END IF
IF x& < item2x& + 9 THEN
        IF x& > item2x& - 9 THEN
                IF y& < item2y& + 9 THEN
                        IF y& > item2y& - 9 THEN
                                GOSUB item2
                        END IF
                END IF
        END IF
END IF
IF x& < item3x& + 9 THEN
        IF x& > item3x& - 9 THEN
                IF y& < item3y& + 9 THEN
                        IF y& > item3y& - 9 THEN
                                GOSUB item3
                        END IF
                END IF
        END IF
END IF
IF x& < item4x& + 9 THEN
        IF x& > item4x& - 9 THEN
                IF y& < item4y& + 9 THEN
                        IF y& > item4y& - 9 THEN
                                GOSUB item4
                        END IF
                END IF
        END IF
END IF
IF x& < item5x& + 9 THEN
        IF x& > item5x& - 9 THEN
                IF y& < item5y& + 9 THEN
                        IF y& > item5y& - 9 THEN
                                GOSUB item5
                        END IF
                END IF
        END IF
END IF

LINE (481, 1)-(639, 119), 1, B
LINE (482, 2)-(638, 118), 1, B
LINE (481, 121)-(639, 239), 4, B
LINE (482, 122)-(638, 238), 4, B
LINE (481, 241)-(639, 359), 2, B
LINE (482, 242)-(638, 358), 2, B
LINE (481, 361)-(639, 479), 14, B
LINE (482, 362)-(638, 478), 14, B

IF lasthit& = 1 THEN c& = 1
IF lasthit& = 2 THEN c& = 4
IF lasthit& = 3 THEN c& = 2
IF lasthit& = 4 THEN c& = 14

LINE (1, 1)-(479, 479), c&, B

LINE (2, 2)-(2, 60), 1
LINE (2, 2)-(60, 2), 1
LINE (60, 2)-(2, 60), 1
LINE (3, 3)-(3, 59), 1
LINE (3, 3)-(59, 3), 1
LINE (59, 2)-(2, 59), 1

LINE (478, 2)-(478, 60), 14
LINE (478, 2)-(420, 2), 14
LINE (420, 2)-(478, 60), 14
LINE (477, 3)-(477, 59), 14
LINE (477, 3)-(421, 3), 14
LINE (421, 2)-(478, 59), 14

LINE (2, 478)-(2, 420), 4
LINE (2, 478)-(60, 478), 4
LINE (60, 478)-(2, 420), 4
LINE (3, 477)-(3, 421), 4
LINE (3, 477)-(59, 477), 4
LINE (59, 478)-(2, 421), 4

LINE (478, 478)-(478, 420), 2
LINE (478, 478)-(420, 478), 2
LINE (420, 478)-(478, 420), 2
LINE (477, 477)-(477, 421), 2
LINE (477, 477)-(421, 477), 2
LINE (421, 478)-(478, 421), 2

LINE (p1pad& - p1size&, 3)-(p1pad& + p1size&, 8), 1, BF
LINE (p1pad& - (p1size& + 1), 3)-(p1pad& - (p1size& + p1speed&), 8), 0, BF
LINE (p1pad& + (p1size& + 1), 3)-(p1pad& + (p1size& + p1speed&), 8), 0, BF
LINE (3, p2pad& - p2size&)-(8, p2pad& + p2size&), 4, BF
LINE (3, p2pad& - (p2size& + 1))-(8, p2pad& - (p2size& + p2speed&)), 0, BF
LINE (3, p2pad& + (p2size& + 1))-(8, p2pad& + (p2size& + p2speed&)), 0, BF
LINE (p3pad& - p3size&, 477)-(p3pad& + p3size&, 472), 2, BF
LINE (p3pad& - (p3size& + 1), 477)-(p3pad& - (p3size& + p3speed&), 472), 0, BF
LINE (p3pad& + (p3size& + 1), 477)-(p3pad& + (p3size& + p3speed&), 472), 0, BF
LINE (477, p4pad& - p4size&)-(472, p4pad& + p4size&), 14, BF
LINE (477, p4pad& - (p4size& + 1))-(472, p4pad& - (p4size& + p4speed&)), 0, BF
LINE (477, p4pad& + (p4size& + 1))-(472, p4pad& + (p4size& + p4speed&)), 0, BF

IF p1dir& = 2 THEN p1pad& = p1pad& - p1speed&
IF p1dir& = 1 THEN p1pad& = p1pad& + p1speed&

IF p2dir& = 2 THEN p2pad& = p2pad& - p2speed&
IF p2dir& = 1 THEN p2pad& = p2pad& + p2speed&

IF p3dir& = 2 THEN p3pad& = p3pad& - p3speed&
IF p3dir& = 1 THEN p3pad& = p3pad& + p3speed&

IF p4dir& = 2 THEN p4pad& = p4pad& - p4speed&
IF p4dir& = 1 THEN p4pad& = p4pad& + p4speed&

IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)

IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)

IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)

IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)

IF y& < 8 THEN
LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF x& > (p1pad& - p1size&) THEN
                IF x& < (p1pad& + p1size&) THEN
                        RANDOMIZE TIMER
                        rand& = INT(RND * 3) + 1
                        IF balldir& = 14 THEN
                                IF rand& = 1 THEN balldir& = 11
                                IF rand& = 2 THEN balldir& = 12
                                IF rand& = 3 THEN balldir& = 12
                        ELSEIF balldir& = 15 THEN
                                IF rand& = 1 THEN balldir& = 10
                                IF rand& = 2 THEN balldir& = 11
                                IF rand& = 3 THEN balldir& = 12
                        ELSEIF balldir& = 16 THEN
                                IF rand& = 1 THEN balldir& = 9
                                IF rand& = 2 THEN balldir& = 10
                                IF rand& = 3 THEN balldir& = 11
                        ELSEIF balldir& = 1 THEN
                                IF rand& = 1 THEN balldir& = 8
                                IF rand& = 2 THEN balldir& = 9
                                IF rand& = 3 THEN balldir& = 10
                        ELSEIF balldir& = 2 THEN
                                IF rand& = 1 THEN balldir& = 7
                                IF rand& = 2 THEN balldir& = 8
                                IF rand& = 3 THEN balldir& = 9
                        ELSEIF balldir& = 3 THEN
                                IF rand& = 1 THEN balldir& = 7
                                IF rand& = 2 THEN balldir& = 8
                                IF rand& = 3 THEN balldir& = 9
                        ELSEIF balldir& = 4 THEN
                                IF rand& = 1 THEN balldir& = 6
                                IF rand& = 2 THEN balldir& = 6
                                IF rand& = 3 THEN balldir& = 7
                        END IF
                        combo& = combo& + 1
                        scombo& = 50
                        lasthit& = 1
                        y& = 13
                        SOUND 700, .1
                END IF
        END IF
ELSEIF x& < 8 THEN
LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF y& > (p2pad& - p2size&) THEN
                IF y& < (p2pad& + p2size&) THEN
                        RANDOMIZE TIMER
                        rand& = INT(RND * 3) + 1
                        IF balldir& = 16 THEN
                                IF rand& = 1 THEN balldir& = 2
                                IF rand& = 2 THEN balldir& = 2
                                IF rand& = 3 THEN balldir& = 3
                        ELSEIF balldir& = 15 THEN
                                IF rand& = 1 THEN balldir& = 2
                                IF rand& = 2 THEN balldir& = 3
                                IF rand& = 3 THEN balldir& = 4
                        ELSEIF balldir& = 14 THEN
                                IF rand& = 1 THEN balldir& = 3
                                IF rand& = 2 THEN balldir& = 4
                                IF rand& = 3 THEN balldir& = 5
                        ELSEIF balldir& = 13 THEN
                                IF rand& = 1 THEN balldir& = 4
                                IF rand& = 2 THEN balldir& = 5
                                IF rand& = 3 THEN balldir& = 6
                        ELSEIF balldir& = 12 THEN
                                IF rand& = 1 THEN balldir& = 5
                                IF rand& = 2 THEN balldir& = 6
                                IF rand& = 3 THEN balldir& = 7
                        ELSEIF balldir& = 11 THEN
                                IF rand& = 1 THEN balldir& = 6
                                IF rand& = 2 THEN balldir& = 7
                                IF rand& = 3 THEN balldir& = 8
                        ELSEIF balldir& = 10 THEN
                                IF rand& = 1 THEN balldir& = 7
                                IF rand& = 2 THEN balldir& = 8
                                IF rand& = 3 THEN balldir& = 8
                        END IF
                        combo& = combo& + 1
                        scombo& = 50
                        lasthit& = 2
                        x& = 13
                        SOUND 700, .1
                END IF
        END IF
ELSEIF y& > 472 THEN
LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF x& > (p3pad& - p3size&) THEN
                IF x& < (p3pad& + p3size&) THEN
                        RANDOMIZE TIMER
                        rand& = INT(RND * 3) + 1
                        IF balldir& = 12 THEN
                                IF rand& = 1 THEN balldir& = 14
                                IF rand& = 2 THEN balldir& = 14
                                IF rand& = 3 THEN balldir& = 15
                        ELSEIF balldir& = 11 THEN
                                IF rand& = 1 THEN balldir& = 14
                                IF rand& = 2 THEN balldir& = 15
                                IF rand& = 3 THEN balldir& = 16
                        ELSEIF balldir& = 10 THEN
                                IF rand& = 1 THEN balldir& = 15
                                IF rand& = 2 THEN balldir& = 16
                                IF rand& = 3 THEN balldir& = 1
                        ELSEIF balldir& = 9 THEN
                                IF rand& = 1 THEN balldir& = 16
                                IF rand& = 2 THEN balldir& = 1
                                IF rand& = 3 THEN balldir& = 2
                        ELSEIF balldir& = 8 THEN
                                IF rand& = 1 THEN balldir& = 1
                                IF rand& = 2 THEN balldir& = 2
                                IF rand& = 3 THEN balldir& = 3
                        ELSEIF balldir& = 7 THEN
                                IF rand& = 1 THEN balldir& = 2
                                IF rand& = 2 THEN balldir& = 3
                                IF rand& = 3 THEN balldir& = 4
                        ELSEIF balldir& = 6 THEN
                                IF rand& = 1 THEN balldir& = 3
                                IF rand& = 2 THEN balldir& = 4
                                IF rand& = 3 THEN balldir& = 4
                        END IF
                        combo& = combo& + 1
                        scombo& = 50
                        lasthit& = 3
                        y& = 467
                        SOUND 700, .1
                END IF
        END IF
ELSEIF x& > 472 THEN
LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF y& > (p4pad& - p4size&) THEN
                IF y& < (p4pad& + p4size&) THEN
                        RANDOMIZE TIMER
                        rand& = INT(RND * 3) + 1
                        IF balldir& = 2 THEN
                                IF rand& = 1 THEN balldir& = 15
                                IF rand& = 2 THEN balldir& = 16
                                IF rand& = 3 THEN balldir& = 16
                        ELSEIF balldir& = 3 THEN
                                IF rand& = 1 THEN balldir& = 14
                                IF rand& = 2 THEN balldir& = 15
                                IF rand& = 3 THEN balldir& = 16
                        ELSEIF balldir& = 4 THEN
                                IF rand& = 1 THEN balldir& = 13
                                IF rand& = 2 THEN balldir& = 14
                                IF rand& = 3 THEN balldir& = 15
                        ELSEIF balldir& = 5 THEN
                                IF rand& = 1 THEN balldir& = 12
                                IF rand& = 2 THEN balldir& = 13
                                IF rand& = 3 THEN balldir& = 14
                        ELSEIF balldir& = 6 THEN
                                IF rand& = 1 THEN balldir& = 11
                                IF rand& = 2 THEN balldir& = 12
                                IF rand& = 3 THEN balldir& = 13
                        ELSEIF balldir& = 7 THEN
                                IF rand& = 1 THEN balldir& = 10
                                IF rand& = 2 THEN balldir& = 11
                                IF rand& = 3 THEN balldir& = 12
                        ELSEIF balldir& = 8 THEN
                                IF rand& = 1 THEN balldir& = 10
                                IF rand& = 2 THEN balldir& = 10
                                IF rand& = 3 THEN balldir& = 11
                        END IF
                        combo& = combo& + 1
                        scombo& = 50
                        lasthit& = 4
                        x& = 467
                        SOUND 700, .1
                END IF
        END IF
END IF

IF y& < 8 THEN GOSUB score1
IF x& < 8 THEN GOSUB score2
IF y& > 472 THEN GOSUB score3
IF x& > 472 THEN GOSUB score4

a$ = INKEY$

IF p1com& = 1 THEN GOTO p2

IF a$ = "v" THEN p1dir& = 2
IF a$ = "b" THEN p1dir& = 0
IF a$ = "n" THEN p1dir& = 1
IF a$ = "g" AND serve& = 1 THEN GOSUB serve

p2:

IF p2com& = 1 THEN GOTO p3

IF a$ = "q" THEN p2dir& = 2
IF a$ = "a" THEN p2dir& = 0
IF a$ = "z" THEN p2dir& = 1
IF a$ = "s" AND serve& = 2 THEN GOSUB serve

p3:

IF p3com& = 1 THEN GOTO p4

IF a$ = "1" THEN p3dir& = 2
IF a$ = "2" THEN p3dir& = 0
IF a$ = "3" THEN p3dir& = 1
IF a$ = "5" AND serve& = 3 THEN GOSUB serve

p4:

IF p4com& = 1 THEN GOTO finish

IF a$ = "]" THEN p4dir& = 2
IF a$ = "'" THEN p4dir& = 0
IF a$ = "/" THEN p4dir& = 1
IF a$ = ";" AND serve& = 4 THEN GOSUB serve

finish:

IF a$ = "e" THEN GOTO quit

IF p1com& = 1 THEN GOSUB player1
IF p2com& = 1 THEN GOSUB player2
IF p3com& = 1 THEN GOSUB player3
IF p4com& = 1 THEN GOSUB player4

LOOP

quit:
CLS
END

serve:
IF serve& = 1 THEN
        IF p1dir& = 2 THEN balldir& = 11
        IF p1dir& = 0 THEN balldir& = 9
        IF p1dir& = 1 THEN balldir& = 7
ELSEIF serve& = 2 THEN
        IF p2dir& = 2 THEN balldir& = 3
        IF p2dir& = 0 THEN balldir& = 5
        IF p2dir& = 1 THEN balldir& = 7
ELSEIF serve& = 3 THEN
        IF p3dir& = 2 THEN balldir& = 15
        IF p3dir& = 0 THEN balldir& = 1
        IF p3dir& = 1 THEN balldir& = 3
ELSEIF serve& = 4 THEN
        IF p4dir& = 2 THEN balldir& = 15
        IF p4dir& = 0 THEN balldir& = 13
        IF p4dir& = 1 THEN balldir& = 11
END IF
serve& = 0
scombo& = 50

RETURN

score1:
IF lasthit& = 2 THEN p2score& = p2score& + combo&
IF lasthit& = 3 THEN p3score& = p3score& + combo&
IF lasthit& = 4 THEN p4score& = p4score& + combo&

balldir& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
RETURN

score2:
IF lasthit& = 1 THEN p1score& = p1score& + combo&
IF lasthit& = 3 THEN p3score& = p3score& + combo&
IF lasthit& = 4 THEN p4score& = p4score& + combo&

balldir& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
RETURN

score3:
IF lasthit& = 1 THEN p1score& = p1score& + combo&
IF lasthit& = 2 THEN p2score& = p2score& + combo&
IF lasthit& = 4 THEN p4score& = p4score& + combo&

balldir& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
RETURN

score4:
IF lasthit& = 1 THEN p1score& = p1score& + combo&
IF lasthit& = 2 THEN p2score& = p2score& + combo&
IF lasthit& = 3 THEN p3score& = p3score& + combo&

balldir& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
RETURN

item1:
IF item1t& = 1 THEN
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 5
        IF lasthit& = 1 AND p1speed& = 2 THEN p1speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 5
        IF lasthit& = 2 AND p2speed& = 2 THEN p2speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 5
        IF lasthit& = 3 AND p3speed& = 2 THEN p3speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 5
        IF lasthit& = 4 AND p4speed& = 2 THEN p4speed& = 3
ELSEIF item1t& = 2 THEN
        IF lasthit& = 1 AND p1speed& = 5 THEN p1speed& = 3
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 2
        IF lasthit& = 2 AND p2speed& = 5 THEN p2speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 2
        IF lasthit& = 3 AND p3speed& = 5 THEN p3speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 2
        IF lasthit& = 4 AND p4speed& = 5 THEN p4speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 2
ELSEIF item1t& = 3 THEN
        IF ballspeed& = 4 THEN ballspeed& = 8
        IF ballspeed& = 2 THEN ballspeed& = 4
ELSEIF item1t& = 4 THEN
        IF ballspeed& = 4 THEN ballspeed& = 2
        IF ballspeed& = 8 THEN ballspeed& = 4
ELSEIF item1t& = 5 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 8) + 1
        IF rand& = 8 THEN rand& = 15
        IF rand& = 7 THEN rand& = 13
        IF rand& = 6 THEN rand& = 11
        IF rand& = 5 THEN rand& = 9
        IF rand& = 4 THEN rand& = 7
        IF rand& = 3 THEN rand& = 5
        IF rand& = 2 THEN rand& = 3
        balldir& = rand&
ELSEIF item1t& = 6 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 18) + 1
        IF rand& = 16 THEN padsize& = 100
        IF rand& = 15 THEN padsize& = 95
        IF rand& = 14 THEN padsize& = 90
        IF rand& = 13 THEN padsize& = 85
        IF rand& = 12 THEN padsize& = 80
        IF rand& = 11 THEN padsize& = 75
        IF rand& = 10 THEN padsize& = 70
        IF rand& = 9 THEN padsize& = 65
        IF rand& = 8 THEN padsize& = 60
        IF rand& = 7 THEN padsize& = 55
        IF rand& = 6 THEN padsize& = 50
        IF rand& = 5 THEN padsize& = 45
        IF rand& = 4 THEN padsize& = 40
        IF rand& = 3 THEN padsize& = 35
        IF rand& = 2 THEN padsize& = 30
        IF rand& = 1 THEN padsize& = 25
        IF rand& = 17 THEN padsize& = 20
        IF rand& = 18 THEN padsize& = 15
        IF lasthit& = 1 THEN p1size& = padsize&
        IF lasthit& = 2 THEN p2size& = padsize&
        IF lasthit& = 3 THEN p3size& = padsize&
        IF lasthit& = 4 THEN p4size& = padsize&
        IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
        IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)
        IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
        IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)
        IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
        IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)
        IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
        IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)
        CLS
END IF
LINE (item1x& - 2, item1y& - 2)-(item1x& + 2, item1y& + 2), 0, BF
RANDOMIZE TIMER
item1x& = INT(RND * 360) + 60
item1y& = INT(RND * 360) + 60
item1t& = INT(RND * different&) + 1
RETURN

item2:
IF item2t& = 1 THEN
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 5
        IF lasthit& = 1 AND p1speed& = 2 THEN p1speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 5
        IF lasthit& = 2 AND p2speed& = 2 THEN p2speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 5
        IF lasthit& = 3 AND p3speed& = 2 THEN p3speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 5
        IF lasthit& = 4 AND p4speed& = 2 THEN p4speed& = 3
ELSEIF item2t& = 2 THEN
        IF lasthit& = 1 AND p1speed& = 5 THEN p1speed& = 3
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 2
        IF lasthit& = 2 AND p2speed& = 5 THEN p2speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 2
        IF lasthit& = 3 AND p3speed& = 5 THEN p3speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 2
        IF lasthit& = 4 AND p4speed& = 5 THEN p4speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 2
ELSEIF item2t& = 3 THEN
        IF ballspeed& = 4 THEN ballspeed& = 8
        IF ballspeed& = 2 THEN ballspeed& = 4
ELSEIF item2t& = 4 THEN
        IF ballspeed& = 4 THEN ballspeed& = 2
        IF ballspeed& = 8 THEN ballspeed& = 4
ELSEIF item2t& = 5 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 8) + 1
        IF rand& = 8 THEN rand& = 15
        IF rand& = 7 THEN rand& = 13
        IF rand& = 6 THEN rand& = 11
        IF rand& = 5 THEN rand& = 9
        IF rand& = 4 THEN rand& = 7
        IF rand& = 3 THEN rand& = 5
        IF rand& = 2 THEN rand& = 3
        balldir& = rand&
ELSEIF item2t& = 6 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 18) + 1
        IF rand& = 16 THEN padsize& = 100
        IF rand& = 15 THEN padsize& = 95
        IF rand& = 14 THEN padsize& = 90
        IF rand& = 13 THEN padsize& = 85
        IF rand& = 12 THEN padsize& = 80
        IF rand& = 11 THEN padsize& = 75
        IF rand& = 10 THEN padsize& = 70
        IF rand& = 9 THEN padsize& = 65
        IF rand& = 8 THEN padsize& = 60
        IF rand& = 7 THEN padsize& = 55
        IF rand& = 6 THEN padsize& = 50
        IF rand& = 5 THEN padsize& = 45
        IF rand& = 4 THEN padsize& = 40
        IF rand& = 3 THEN padsize& = 35
        IF rand& = 2 THEN padsize& = 30
        IF rand& = 1 THEN padsize& = 25
        IF rand& = 17 THEN padsize& = 20
        IF rand& = 18 THEN padsize& = 15
        IF lasthit& = 1 THEN p1size& = padsize&
        IF lasthit& = 2 THEN p2size& = padsize&
        IF lasthit& = 3 THEN p3size& = padsize&
        IF lasthit& = 4 THEN p4size& = padsize&
        IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
        IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)
        IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
        IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)
        IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
        IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)
        IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
        IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)
        CLS
END IF
LINE (item2x& - 2, item2y& - 2)-(item2x& + 2, item2y& + 2), 0, BF
RANDOMIZE TIMER
item2x& = INT(RND * 360) + 60
item2y& = INT(RND * 360) + 60
item2t& = INT(RND * different&) + 1
RETURN

item3:
IF item3t& = 1 THEN
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 5
        IF lasthit& = 1 AND p1speed& = 2 THEN p1speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 5
        IF lasthit& = 2 AND p2speed& = 2 THEN p2speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 5
        IF lasthit& = 3 AND p3speed& = 2 THEN p3speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 5
        IF lasthit& = 4 AND p4speed& = 2 THEN p4speed& = 3
ELSEIF item3t& = 2 THEN
        IF lasthit& = 1 AND p1speed& = 5 THEN p1speed& = 3
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 2
        IF lasthit& = 2 AND p2speed& = 5 THEN p2speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 2
        IF lasthit& = 3 AND p3speed& = 5 THEN p3speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 2
        IF lasthit& = 4 AND p4speed& = 5 THEN p4speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 2
ELSEIF item3t& = 3 THEN
        IF ballspeed& = 4 THEN ballspeed& = 8
        IF ballspeed& = 2 THEN ballspeed& = 4
ELSEIF item3t& = 4 THEN
        IF ballspeed& = 4 THEN ballspeed& = 2
        IF ballspeed& = 8 THEN ballspeed& = 4
ELSEIF item3t& = 5 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 8) + 1
        IF rand& = 8 THEN rand& = 15
        IF rand& = 7 THEN rand& = 13
        IF rand& = 6 THEN rand& = 11
        IF rand& = 5 THEN rand& = 9
        IF rand& = 4 THEN rand& = 7
        IF rand& = 3 THEN rand& = 5
        IF rand& = 2 THEN rand& = 3
        balldir& = rand&
ELSEIF item3t& = 6 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 18) + 1
        IF rand& = 16 THEN padsize& = 100
        IF rand& = 15 THEN padsize& = 95
        IF rand& = 14 THEN padsize& = 90
        IF rand& = 13 THEN padsize& = 85
        IF rand& = 12 THEN padsize& = 80
        IF rand& = 11 THEN padsize& = 75
        IF rand& = 10 THEN padsize& = 70
        IF rand& = 9 THEN padsize& = 65
        IF rand& = 8 THEN padsize& = 60
        IF rand& = 7 THEN padsize& = 55
        IF rand& = 6 THEN padsize& = 50
        IF rand& = 5 THEN padsize& = 45
        IF rand& = 4 THEN padsize& = 40
        IF rand& = 3 THEN padsize& = 35
        IF rand& = 2 THEN padsize& = 30
        IF rand& = 1 THEN padsize& = 25
        IF rand& = 17 THEN padsize& = 20
        IF rand& = 18 THEN padsize& = 15
        IF lasthit& = 1 THEN p1size& = padsize&
        IF lasthit& = 2 THEN p2size& = padsize&
        IF lasthit& = 3 THEN p3size& = padsize&
        IF lasthit& = 4 THEN p4size& = padsize&
        IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
        IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)
        IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
        IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)
        IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
        IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)
        IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
        IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)
        CLS
END IF
LINE (item3x& - 2, item3y& - 2)-(item3x& + 2, item3y& + 2), 0, BF
RANDOMIZE TIMER
item3x& = INT(RND * 360) + 60
item3y& = INT(RND * 360) + 60
item3t& = INT(RND * different&) + 1
RETURN

item4:
IF item4t& = 1 THEN
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 5
        IF lasthit& = 1 AND p1speed& = 2 THEN p1speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 5
        IF lasthit& = 2 AND p2speed& = 2 THEN p2speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 5
        IF lasthit& = 3 AND p3speed& = 2 THEN p3speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 5
        IF lasthit& = 4 AND p4speed& = 2 THEN p4speed& = 3
ELSEIF item4t& = 2 THEN
        IF lasthit& = 1 AND p1speed& = 5 THEN p1speed& = 3
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 2
        IF lasthit& = 2 AND p2speed& = 5 THEN p2speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 2
        IF lasthit& = 3 AND p3speed& = 5 THEN p3speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 2
        IF lasthit& = 4 AND p4speed& = 5 THEN p4speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 2
ELSEIF item4t& = 3 THEN
        IF ballspeed& = 4 THEN ballspeed& = 8
        IF ballspeed& = 2 THEN ballspeed& = 4
ELSEIF item4t& = 4 THEN
        IF ballspeed& = 4 THEN ballspeed& = 2
        IF ballspeed& = 8 THEN ballspeed& = 4
ELSEIF item4t& = 5 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 8) + 1
        IF rand& = 8 THEN rand& = 15
        IF rand& = 7 THEN rand& = 13
        IF rand& = 6 THEN rand& = 11
        IF rand& = 5 THEN rand& = 9
        IF rand& = 4 THEN rand& = 7
        IF rand& = 3 THEN rand& = 5
        IF rand& = 2 THEN rand& = 3
        balldir& = rand&
ELSEIF item4t& = 6 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 18) + 1
        IF rand& = 16 THEN padsize& = 100
        IF rand& = 15 THEN padsize& = 95
        IF rand& = 14 THEN padsize& = 90
        IF rand& = 13 THEN padsize& = 85
        IF rand& = 12 THEN padsize& = 80
        IF rand& = 11 THEN padsize& = 75
        IF rand& = 10 THEN padsize& = 70
        IF rand& = 9 THEN padsize& = 65
        IF rand& = 8 THEN padsize& = 60
        IF rand& = 7 THEN padsize& = 55
        IF rand& = 6 THEN padsize& = 50
        IF rand& = 5 THEN padsize& = 45
        IF rand& = 4 THEN padsize& = 40
        IF rand& = 3 THEN padsize& = 35
        IF rand& = 2 THEN padsize& = 30
        IF rand& = 1 THEN padsize& = 25
        IF rand& = 17 THEN padsize& = 20
        IF rand& = 18 THEN padsize& = 15
        IF lasthit& = 1 THEN p1size& = padsize&
        IF lasthit& = 2 THEN p2size& = padsize&
        IF lasthit& = 3 THEN p3size& = padsize&
        IF lasthit& = 4 THEN p4size& = padsize&
        IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
        IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)
        IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
        IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)
        IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
        IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)
        IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
        IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)
        CLS
END IF
LINE (item4x& - 2, item4y& - 2)-(item4x& + 2, item4y& + 2), 0, BF
RANDOMIZE TIMER
item4x& = INT(RND * 360) + 60
item4y& = INT(RND * 360) + 60
item4t& = INT(RND * different&) + 1
RETURN

item5:
IF item5t& = 1 THEN
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 5
        IF lasthit& = 1 AND p1speed& = 2 THEN p1speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 5
        IF lasthit& = 2 AND p2speed& = 2 THEN p2speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 5
        IF lasthit& = 3 AND p3speed& = 2 THEN p3speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 5
        IF lasthit& = 4 AND p4speed& = 2 THEN p4speed& = 3
ELSEIF item5t& = 2 THEN
        IF lasthit& = 1 AND p1speed& = 5 THEN p1speed& = 3
        IF lasthit& = 1 AND p1speed& = 3 THEN p1speed& = 2
        IF lasthit& = 2 AND p2speed& = 5 THEN p2speed& = 3
        IF lasthit& = 2 AND p2speed& = 3 THEN p2speed& = 2
        IF lasthit& = 3 AND p3speed& = 5 THEN p3speed& = 3
        IF lasthit& = 3 AND p3speed& = 3 THEN p3speed& = 2
        IF lasthit& = 4 AND p4speed& = 5 THEN p4speed& = 3
        IF lasthit& = 4 AND p4speed& = 3 THEN p4speed& = 2
ELSEIF item5t& = 3 THEN
        IF ballspeed& = 4 THEN ballspeed& = 8
        IF ballspeed& = 2 THEN ballspeed& = 4
ELSEIF item5t& = 4 THEN
        IF ballspeed& = 4 THEN ballspeed& = 2
        IF ballspeed& = 8 THEN ballspeed& = 4
ELSEIF item5t& = 5 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 8) + 1
        IF rand& = 8 THEN rand& = 15
        IF rand& = 7 THEN rand& = 13
        IF rand& = 6 THEN rand& = 11
        IF rand& = 5 THEN rand& = 9
        IF rand& = 4 THEN rand& = 7
        IF rand& = 3 THEN rand& = 5
        IF rand& = 2 THEN rand& = 3
        balldir& = rand&
ELSEIF item5t& = 6 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 18) + 1
        IF rand& = 16 THEN padsize& = 100
        IF rand& = 15 THEN padsize& = 95
        IF rand& = 14 THEN padsize& = 90
        IF rand& = 13 THEN padsize& = 85
        IF rand& = 12 THEN padsize& = 80
        IF rand& = 11 THEN padsize& = 75
        IF rand& = 10 THEN padsize& = 70
        IF rand& = 9 THEN padsize& = 65
        IF rand& = 8 THEN padsize& = 60
        IF rand& = 7 THEN padsize& = 55
        IF rand& = 6 THEN padsize& = 50
        IF rand& = 5 THEN padsize& = 45
        IF rand& = 4 THEN padsize& = 40
        IF rand& = 3 THEN padsize& = 35
        IF rand& = 2 THEN padsize& = 30
        IF rand& = 1 THEN padsize& = 25
        IF rand& = 17 THEN padsize& = 20
        IF rand& = 18 THEN padsize& = 15
        IF lasthit& = 1 THEN p1size& = padsize&
        IF lasthit& = 2 THEN p2size& = padsize&
        IF lasthit& = 3 THEN p3size& = padsize&
        IF lasthit& = 4 THEN p4size& = padsize&
        IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
        IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)
        IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
        IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)
        IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
        IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)
        IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
        IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)
        CLS
END IF
LINE (item5x& - 2, item5y& - 2)-(item5x& + 2, item5y& + 2), 0, BF
RANDOMIZE TIMER
item5x& = INT(RND * 360) + 60
item5y& = INT(RND * 360) + 60
item5t& = INT(RND * different&) + 1
RETURN

player1:
IF balldir& = 14 OR balldir& = 15 OR balldir& = 16 OR balldir& = 1 OR balldir& = 2 OR balldir& = 3 OR balldir& = 4 THEN
        IF y& < p1ai& THEN
                IF balldir& = 14 THEN
                        fx& = x&
                        fy& = y&
                        fx& = fx& - fy& * 2
                        p1des& = fx&
                ELSEIF balldir& = 15 THEN
                        fx& = x&
                        fy& = y&
                        fx& = fx& - fy&
                        p1des& = fx&
                ELSEIF balldir& = 16 THEN
                        fx& = x&
                        fy& = y&
                        fx& = CLNG(fx& - fy& / 2)
                        p1des& = fx&
                ELSEIF balldir& = 1 THEN
                        fx& = x&
                        fy& = y&
                        p1des& = fx&
                ELSEIF balldir& = 2 THEN
                        fx& = x&
                        fy& = y&
                        fx& = CLNG(fx& + fy& / 2)
                        p1des& = fx&
                ELSEIF balldir& = 3 THEN
                        fx& = x&
                        fy& = y&
                        fx& = fx& + fy&
                        p1des& = fx&
                ELSEIF balldir& = 4 THEN
                        fx& = x&
                        fy& = y&
                        fx& = fx& + fy& * 2
                        p1des& = fx&
                END IF
        END IF
ELSE
        IF serve& = 0 THEN p1des& = 240
END IF

IF serve& = 1 THEN
        IF p2score& > p3score& AND p2score& > p4score& THEN p1des& = 0
        IF p3score& > p4score& AND p3score& > p2score& THEN p1des& = 240
        IF p4score& > p2score& AND p4score& > p3score& THEN p1des& = 480
END IF
IF p1des& > p1pad& THEN p1dir& = 1
IF p1des& < p1pad& THEN p1dir& = 2
IF p1des& > (p1pad& - p1speed&) AND p1des& < (p1pad& + p1speed&) THEN p1dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 1 AND waits& = 0 THEN GOSUB serve

RETURN

player2:
IF balldir& = 16 OR balldir& = 15 OR balldir& = 14 OR balldir& = 13 OR balldir& = 12 OR balldir& = 11 OR balldir& = 10 THEN
        IF x& < p2ai& THEN
                IF balldir& = 16 THEN
                        fx& = x&
                        fy& = y&
                        fy& = fy& - fx& / 2
                        p2des& = fy&
                ELSEIF balldir& = 15 THEN
                        fx& = x&
                        fy& = y&
                        fy& = fy& - fx&
                        p2des& = fy&
                ELSEIF balldir& = 14 THEN
                        fx& = x&
                        fy& = y&
                        fy& = CLNG(fy& - fx& / 2)
                        p2des& = fy&
                ELSEIF balldir& = 13 THEN
                        fx& = x&
                        fy& = y&
                        p2des& = fy&
                ELSEIF balldir& = 12 THEN
                        fx& = x&
                        fy& = y&
                        fy& = CLNG(fy& + fx& / 2)
                        p2des& = fy&
                ELSEIF balldir& = 11 THEN
                        fx& = x&
                        fy& = y&
                        fy& = fy& + fx&
                        p2des& = fy&
                ELSEIF balldir& = 10 THEN
                        fx& = x&
                        fy& = y&
                        fy& = fy& + fx& * 2
                        p2des& = fy&
                END IF
        END IF
ELSE
        IF serve& = 0 THEN p2des& = 240
END IF
IF serve& = 2 THEN
        IF p1score& > p3score& AND p1score& > p4score& THEN p2des& = 0
        IF p3score& > p4score& AND p3score& > p1score& THEN p2des& = 480
        IF p4score& > p1score& AND p4score& > p3score& THEN p2des& = 240
END IF

IF p2des& > p2pad& THEN p2dir& = 1
IF p2des& < p2pad& THEN p2dir& = 2
IF p2des& > (p2pad& - p2speed&) AND p2des& < (p2pad& + p2speed&) THEN p2dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 2 AND waits& = 0 THEN GOSUB serve

RETURN

player3:
IF balldir& = 12 OR balldir& = 11 OR balldir& = 10 OR balldir& = 9 OR balldir& = 8 OR balldir& = 7 OR balldir& = 6 THEN
        IF y& > (480 - p3ai&) THEN
                IF balldir& = 12 THEN
                        fx& = x&
                        fy& = 480 - y&
                        fx& = fx& - fy& * 2
                        p3des& = fx&
                ELSEIF balldir& = 11 THEN
                        fx& = x&
                        fy& = 480 - y&
                        fx& = fx& - fy&
                        p3des& = fx&
                ELSEIF balldir& = 10 THEN
                        fx& = x&
                        fy& = 480 - y&
                        fx& = CLNG(fx& - fy& / 2)
                        p3des& = fx&
                ELSEIF balldir& = 9 THEN
                        fx& = x&
                        fy& = 480 - y&
                        p3des& = fx&
                ELSEIF balldir& = 8 THEN
                        fx& = x&
                        fy& = 480 - y&
                        fx& = CLNG(fx& + fy& / 2)
                        p3des& = fx&
                ELSEIF balldir& = 7 THEN
                        fx& = x&
                        fy& = 480 - y&
                        fx& = fx& + fy&
                        p3des& = fx&
                ELSEIF balldir& = 6 THEN
                        fx& = x&
                        fy& = 480 - y&
                        fx& = fx& + fy& * 2
                        p3des& = fx&
                END IF
        END IF
ELSE
        IF serve& = 0 THEN p3des& = 240
END IF
IF serve& = 3 THEN
        IF p2score& > p1score& AND p2score& > p4score& THEN p3des& = 0
        IF p1score& > p4score& AND p1score& > p2score& THEN p3des& = 240
        IF p4score& > p2score& AND p4score& > p1score& THEN p3des& = 480
END IF

IF p3des& > p3pad& THEN p3dir& = 1
IF p3des& < p3pad& THEN p3dir& = 2
IF p3des& > (p3pad& - p3speed&) AND p3des& < (p3pad& + p3speed&) THEN p3dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 3 AND waits& = 0 THEN GOSUB serve
RETURN

player4:
IF balldir& = 2 OR balldir& = 3 OR balldir& = 4 OR balldir& = 5 OR balldir& = 6 OR balldir& = 7 OR balldir& = 8 THEN
        IF x& > (480 - p2ai&) THEN
                IF balldir& = 2 THEN
                        fx& = 480 - x&
                        fy& = y&
                        fy& = fy& - fx& / 2
                        p4des& = fy&
                ELSEIF balldir& = 3 THEN
                        fx& = 480 - x&
                        fy& = y&
                        fy& = fy& - fx&
                        p4des& = fy&
                ELSEIF balldir& = 4 THEN
                        fx& = 480 - x&
                        fy& = y&
                        fy& = CLNG(fy& - fx& / 2)
                        p4des& = fy&
                ELSEIF balldir& = 5 THEN
                        fx& = 480 - x&
                        fy& = y&
                        p4des& = fy&
                ELSEIF balldir& = 6 THEN
                        fx& = 480 - x&
                        fy& = y&
                        fy& = CLNG(fy& + fx& / 2)
                        p4des& = fy&
                ELSEIF balldir& = 7 THEN
                        fx& = 480 - x&
                        fy& = y&
                        fy& = fy& + fx&
                        p4des& = fy&
                ELSEIF balldir& = 8 THEN
                        fx& = 480 - x&
                        fy& = y&
                        fy& = fy& + fx& * 2
                        p4des& = fy&
                END IF
        END IF
ELSE
        IF serve& = 0 THEN p4des& = 240
END IF

IF serve& = 4 THEN
        IF p1score& > p3score& AND p1score& > p2score& THEN p4des& = 0
        IF p3score& > p2score& AND p3score& > p1score& THEN p4des& = 480
        IF p2score& > p1score& AND p2score& > p3score& THEN p4des& = 240
END IF

IF p4des& > p4pad& THEN p4dir& = 1
IF p4des& < p4pad& THEN p4dir& = 2
IF p4des& > (p4pad& - p4speed&) AND p4des& < (p4pad& + p4speed&) THEN p4dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 4 AND waits& = 0 THEN GOSUB serve

RETURN


