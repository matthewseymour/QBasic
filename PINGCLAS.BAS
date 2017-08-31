CLS
SCREEN 12
p1score& = 0
p2score& = 0
p1pad& = 240
p2pad& = 240
p1dir& = 0
p2dir& = 0
RANDOMIZE TIMER
rand& = INT(RND * 2) + 1
itemx& = INT(RND * 440) + 100
itemy& = INT(RND * 280) + 100
item2x& = INT(RND * 440) + 100
item2y& = INT(RND * 280) + 100
ballspeed& = 2
padsize1& = 25
padsize2& = 25
padspeed1& = 1
padspeed2& = 1
scoremode& = 1
combo& = 0
IF rand& = 1 THEN
        serve& = 1
ELSEIF rand& = 2 THEN
        serve& = 2
END IF

DO

LOCATE 12, 40
COLOR 4
PRINT p1score&
LOCATE 13, 40
COLOR 1
PRINT p2score&
LOCATE 14, 40
COLOR 15
PRINT combo&

IF serve& = 1 THEN
        ballx& = 16
        bally& = p1pad&
        LINE (ballx& - 6, bally& - 7)-(ballx& + 6, bally& + 7), 0, B
        combo& = 0
ELSEIF serve& = 2 THEN
        ballx& = 623
        bally& = p2pad&
        LINE (ballx& - 6, bally& - 7)-(ballx& + 6, bally& + 7), 0, B
        combo& = 0
END IF

LINE (0, 0)-(639, 479), 14, B

LINE (itemx& - 10, itemy& - 10)-(itemx& + 10, itemy& + 10), 5, BF
LINE (item2x& - 10, item2y& - 10)-(item2x& + 10, item2y& + 10), 13, BF

LINE (1, (p1pad& - padsize1&))-(10, (p1pad& + padsize1&)), 4, BF
LINE (1, (p1pad& - padsize1&))-(10, (p1pad& - 1 - padsize1&)), 0, B
LINE (1, (p1pad& + padsize1&))-(10, (p1pad& + 1 + padsize1&)), 0, B

LINE (638, (p2pad& - padsize2&))-(628, (p2pad& + padsize2&)), 1, BF
LINE (638, (p2pad& - padsize2&))-(628, (p2pad& - 1 - padsize2&)), 0, B
LINE (638, (p2pad& + padsize2&))-(628, (p2pad& + 1 + padsize2&)), 0, B

control$ = INKEY$
IF control$ = "q" THEN
        p1dir& = 1
ELSEIF control$ = "a" THEN
        IF serve& = 1 THEN
                GOSUB serve1
        ELSE p1dir& = 0
        END IF
ELSEIF control$ = "z" THEN
        p1dir& = 2
ELSEIF control$ = "]" THEN
        p2dir& = 1
ELSEIF control$ = "'" THEN
        IF serve& = 2 THEN
                GOSUB serve2
        ELSE p2dir& = 0
        END IF
ELSEIF control$ = "/" THEN
        p2dir& = 2
ELSEIF control$ = "`" THEN
        GOSUB MENU
END IF

IF p1dir& = 1 THEN
        p1pad& = p1pad& - padspeed1&
ELSEIF p1dir& = 2 THEN
        p1pad& = p1pad& + padspeed1&
END IF

IF p2dir& = 1 THEN
        p2pad& = p2pad& - padspeed2&
ELSEIF p2dir& = 2 THEN
        p2pad& = p2pad& + padspeed2&
END IF

IF p1pad& < padsize1& THEN p1pad& = padsize1&
IF p1pad& > 480 - padsize1& THEN p1pad& = 480 - padsize1&
IF p2pad& < padsize2& THEN p2pad& = padsize2&
IF p2pad& > 480 - padsize2& THEN p2pad& = 480 - padsize2&

FOR ball = 1 TO ballspeed&
IF ballspeed& = 1 THEN
        ballcol& = 8
ELSEIF ballspeed& = 2 THEN
        ballcol& = 7
ELSEIF ballspeed& = 3 THEN
        ballcol& = 15
END IF
LINE (ballx& - 5, bally& - 5)-(ballx& + 5, bally& + 5), ballcol&, BF
LINE (ballx& - 6, bally& - 6)-(ballx& + 6, bally& + 6), 0, B

IF balldir& = 1 THEN
        ballx& = ballx& + 1
        bally& = bally& - 1
ELSEIF balldir& = 2 THEN
        ballx& = ballx& + 1
        bally& = bally& + 1
ELSEIF balldir& = 3 THEN
        ballx& = ballx& - 1
        bally& = bally& + 1
ELSEIF balldir& = 4 THEN
        ballx& = ballx& - 1
        bally& = bally& - 1
END IF
NEXT ball

IF bally& < 7 THEN
        IF balldir& = 1 THEN
                balldir& = 2
                SOUND 900, 1
        ELSEIF balldir& = 4 THEN
                balldir& = 3
                SOUND 900, 1
        END IF
END IF

IF bally& > 473 THEN
        IF balldir& = 2 THEN
                balldir& = 1
                SOUND 900, 1
        ELSEIF balldir& = 3 THEN
                balldir& = 4
                SOUND 900, 1
        END IF
END IF

IF (ballx& - 5) < 12 THEN
        IF (p1pad& - padsize1&) < bally& AND bally& < (p1pad& + padsize1&) THEN
                IF balldir& = 4 THEN
                        balldir& = 1
                        SOUND 700, 1
                ELSEIF balldir& = 3 THEN
                        balldir& = 2
                        SOUND 700, 1
                END IF
                ballhit& = 1
                combo& = combo& + 1
        ELSE GOSUB score2
        END IF
END IF

IF (ballx& + 5) > 628 THEN
        IF (p2pad& - padsize2&) < bally& AND bally& < (p2pad& + padsize2&) THEN
                IF balldir& = 1 THEN
                        balldir& = 4
                        SOUND 700, 1
                ELSEIF balldir& = 2 THEN
                        balldir& = 3
                        SOUND 700, 1
                END IF
                ballhit& = 2
                combo& = combo& + 1
        ELSE GOSUB score1
        END IF
END IF

IF (itemx& - 19) < ballx& AND ballx& < (itemx& + 19) AND (itemy& - 19) < bally& AND bally& < (itemy& + 19) THEN
        item& = 1
        GOSUB item
END IF

IF (item2x& - 19) < ballx& AND ballx& < (item2x& + 19) AND (item2y& - 19) < bally& AND bally& < (item2y& + 19) THEN
        item& = 2
        GOSUB item
END IF
LOOP

serve1:
IF p1dir& = 0 THEN RETURN
IF p1dir& = 1 THEN
        balldir& = 1
ELSEIF p1dir& = 2 THEN
        balldir& = 2
END IF
combo& = combo& + 1
ballhit& = 1
serve& = 0
RETURN

serve2:
IF p2dir& = 0 THEN RETURN
IF p2dir& = 1 THEN
        balldir& = 4
ELSEIF p2dir& = 2 THEN
        balldir& = 3
END IF
combo& = combo& + 1
ballhit& = 2
serve& = 0
RETURN

score1:
CLS
SOUND 100, 5
IF scoremode& = 1 THEN
        p1score& = p1score& + 1
ELSEIF scoremode& = 2 THEN
        p1score& = p1score& + combo&
END IF
serve& = 1
balldir& = 0
combo& = 0
RETURN

score2:
CLS
SOUND 100, 5
IF scoremode& = 1 THEN
        p2score& = p2score& + 1
ELSEIF scoremode& = 2 THEN
        p2score& = p2score& + combo&
END IF
serve& = 2
balldir& = 0
combo& = 0
RETURN

item:
CLS
'LINE (itemx& - 10, itemy& - 10)-(itemx& + 10, itemy& + 10), 0, BF
SOUND 1500, 1
'ballspeed& = 2
'padsize1& = 25
'padsize2& = 25
'padspeed1& = 1
'padspeed2& = 1
RANDOMIZE TIMER
rand& = INT(RND * 4) + 1
IF rand& = 1 THEN
        RANDOMIZE TIMER
        rand2& = INT(RND * 4) + 1
        balldir& = rand2&
ELSEIF rand& = 2 THEN
        RANDOMIZE TIMER
        rand2& = INT(RND * 2) + 1
        IF ballspeed& = 1 THEN
                IF rand2& = 1 THEN
                        ballspeed& = 2
                ELSEIF rand2& = 2 THEN
                        ballspeed& = 3
                END IF
        ELSEIF ballspeed& = 2 THEN
                IF rand2& = 1 THEN
                        ballspeed& = 1
                ELSEIF rand2& = 2 THEN
                        ballspeed& = 3
                END IF
        ELSEIF ballspeed& = 3 THEN
                ballspeed& = rand2&
        END IF
ELSEIF rand& = 3 THEN
        RANDOMIZE TIMER
        rand2& = INT(RND * 80) + 20
        IF ballhit& = 1 THEN
                padsize1& = rand2&
        ELSEIF ballhit& = 2 THEN
                padsize2& = rand2&
        END IF
ELSEIF rand& = 4 THEN
        RANDOMIZE TIMER
        IF ballhit& = 1 THEN
                IF padspeed1& = 2 THEN
                        padspeed1& = 1
                ELSE padspeed1& = 2
                END IF
        ELSEIF ballhit& = 2 THEN
                IF padspeed2& = 2 THEN
                        padspeed2& = 1
                ELSE padspeed2& = 2
                END IF
        END IF
END IF
RANDOMIZE TIMER
IF item& = 1 THEN
        itemx& = INT(RND * 440) + 100
        itemy& = INT(RND * 280) + 100
ELSEIF item& = 2 THEN
        item2x& = INT(RND * 440) + 100
        item2y& = INT(RND * 280) + 100
END IF
RETURN

MENU:
CLS
mballx& = 182
mbally& = 24
mpad1& = 24
mpad2& = 24
RANDOMIZE TIMER
mballdir& = INT(RND * 4) + 1
DO
LOCATE 1, 1
COLOR 5
PRINT "PING!"
LOCATE 1, 7
COLOR 7
PRINT "OPTION MENU"
LOCATE 3, 1
COLOR 15
PRINT "S"
LOCATE 3, 2
COLOR 2
PRINT "CORING MODE"
LOCATE 4, 1
COLOR scnop&
PRINT "NORMAL"
LOCATE 4, 10
COLOR scuop&
PRINT "ULTRA"
LOCATE 6, 1
COLOR 15
PRINT "R"
LOCATE 6, 2
COLOR 1
PRINT "ETURN TO GAME"
LOCATE 8, 1
COLOR 15
PRINT "Q"
LOCATE 8, 2
COLOR 4
PRINT "UIT"

control$ = INKEY$
IF control$ = "s" THEN
        scoremode& = scoremode& + 1
ELSEIF control$ = "q" THEN
        END
ELSEIF control$ = "r" THEN
        CLS
        RETURN
END IF

IF scoremode& > 2 THEN scoremode& = 1

IF scoremode& = 1 THEN
        scnop& = 15
        scuop& = 8
ELSEIF scoremode& = 2 THEN
        scnop& = 8
        scuop& = 15
END IF

VIEW SCREEN (150, 0)-(214, 48)
LINE (150, 0)-(214, 48), 7, B
LINE (mballx& - 1, mbally& - 1)-(mballx& + 1, mbally& + 1), 7, BF
LINE (mballx& - 2, mbally& - 2)-(mballx& + 2, mbally& + 2), 0, B
LINE (151, mpad1& - 3)-(152, mpad1& + 3), 15, B
LINE (151, mpad1& - 4)-(152, mpad1& - 4), 0
LINE (151, mpad1& + 4)-(152, mpad1& + 4), 0
LINE (213, mpad2& - 3)-(212, mpad2& + 3), 15, B
LINE (213, mpad2& - 4)-(212, mpad2& - 4), 0
LINE (213, mpad2& + 4)-(212, mpad2& + 4), 0
IF mhit& = 1 THEN
        IF mbally& < mpad2& THEN
                mpad2& = mpad2& - 1
        ELSEIF mbally& > mpad2& THEN
                mpad2& = mpad2& + 1
        END IF
END IF
IF mhit& = 2 THEN
        IF mbally& < mpad1& THEN
                mpad1& = mpad1& - 1
        ELSEIF mbally& > mpad1& THEN
                mpad1& = mpad1& + 1
        END IF
END IF


IF mballdir& = 1 THEN
        mballx& = mballx& + 1
        mbally& = mbally& - 1
ELSEIF mballdir& = 2 THEN
        mballx& = mballx& + 1
        mbally& = mbally& + 1
ELSEIF mballdir& = 3 THEN
        mballx& = mballx& - 1
        mbally& = mbally& + 1
ELSEIF mballdir& = 4 THEN
        mballx& = mballx& - 1
        mbally& = mbally& - 1
END IF

IF mbally& < 2 THEN
        IF mballdir& = 1 THEN
                mballdir& = 2
                'SOUND 900, .1
        ELSEIF mballdir& = 4 THEN
                mballdir& = 3
                'SOUND 900, .1
        END IF
END IF

IF mbally& > 48 THEN
        IF mballdir& = 2 THEN
                mballdir& = 1
                'SOUND 900, .1
        ELSEIF mballdir& = 3 THEN
                mballdir& = 4
                'SOUND 900, .1
        END IF
END IF
RANDOMIZE TIMER
msound& = INT(RND * 1000) + 500

IF mballx& < 152 THEN
        IF mballdir& = 4 THEN
                mballdir& = 1
                SOUND msound&, .1
        ELSEIF mballdir& = 3 THEN
                mballdir& = 2
                SOUND msound&, .1
        END IF
END IF

IF mballx& > 214 THEN
        IF mballdir& = 2 THEN
                mballdir& = 3
                SOUND msound&, .1
        ELSEIF mballdir& = 1 THEN
                mballdir& = 4
                SOUND msound&, .1
        END IF
END IF
IF mballdir& = 1 OR mballdir& = 2 THEN mhit& = 1
IF mballdir& = 3 OR mballdir& = 4 THEN mhit& = 2
VIEW


LOOP

