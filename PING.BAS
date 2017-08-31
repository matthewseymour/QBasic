SCREEN 12
RANDOMIZE TIMER
GOSUB title
WIDTH 80, 60
itemnum& = 25
trailsize& = 7
DIM itemx(1 TO itemnum&) AS INTEGER
DIM itemy(1 TO itemnum&) AS INTEGER
DIM itcol(1 TO itemnum&) AS INTEGER
DIM isize(1 TO itemnum&) AS INTEGER
DIM tx(1 TO trailsize&) AS INTEGER
DIM ty(1 TO trailsize&) AS INTEGER
start& = 1
ballspeed& = 6
scoremode& = 1
itemop& = 5
soundmode& = 1
winmode& = 100
p1co& = 1
p2co& = 1
guns& = 0
delay& = 10000
ips& = 1
ibd& = 1
ibs& = 0
ifp& = 1
iub& = 1
itd& = 1
ici& = 1
ise& = 1
ipsp& = 1
ish& = 1
p1pad& = 240
p2pad& = 240
padsize1& = 25
padsize2& = 25
padspeed& = 10
pspeed1& = 10
pspeed2& = 10
GOSUB menu
start:
pspeed1& = padspeed&
pspeed2& = padspeed&
p1score& = 0
p2score& = 0
p1dir& = 0
p2dir& = 0
p1pad& = 240
p2pad& = 240
padsize1& = 25
padsize2& = 25
mag1& = 0
mag2& = 0
shield1& = 0
shield2& = 0
p1shot& = 0
rand& = INT(RND * 2) + 1
ballcol& = INT(RND * 15) + 1
bonus& = 0
padspeed1& = 1
padspeed2& = 1
combo& = 0
IF rand& = 1 THEN
        serve& = 1
ELSEIF rand& = 2 THEN
        serve& = 2
END IF
FOR i = 1 TO itemnum&
itcol(i) = INT(RND * 15) + 1
itemx(i) = INT(RND * 490) + 100
itemy(i) = INT(RND * 330) + 100
isize(i) = INT(RND * 5) + 5
NEXT i
refill& = 1

DO

IF INT(RND * 3) + 1 = 1 THEN PSET (INT(RND * 620) + 10, INT(RND * 470) + 5), INT(RND * 15) + 1

FOR delay = 1 TO 10000 - delay&
NEXT delay

LOCATE 29, 40: COLOR 4: PRINT p1score&
LOCATE 30, 40: COLOR 1: PRINT p2score&
LOCATE 31, 40: COLOR 15: PRINT combo&

control$ = INKEY$
IF control$ = "q" THEN
        p1dir& = 1
        speedcount1& = 0
ELSEIF control$ = "a" THEN
        IF serve& = 1 THEN
                GOSUB serve1
        ELSE
                p1dir& = 0
                speedcount1& = 0
        END IF
ELSEIF control$ = "z" THEN
        p1dir& = 2
        speedcount1& = 0
ELSEIF control$ = "s" AND p1shot& = 0 AND guns& = 1 THEN
        IF soundmode& = 1 THEN SOUND 500, 1
        p1shoty& = p1pad&
        p1shotx& = 10
        p1shot& = 1
ELSEIF control$ = "]" THEN
        p2dir& = 1
        speedcount2& = 0
ELSEIF control$ = "'" THEN
        IF serve& = 2 THEN
                GOSUB serve2
        ELSE
                p2dir& = 0
                speedcount2& = 0
        END IF
ELSEIF control$ = "/" THEN
        p2dir& = 2
        speedcount2& = 0
ELSEIF control$ = ";" AND p2shot& = 0 AND guns& = 1 THEN
        IF soundmode& = 1 THEN SOUND 500, 1
        p2shoty& = p2pad&
        p2shotx& = 628
        p2shot& = 1
ELSEIF control$ = "p" THEN
        LOCATE 5, 39: COLOR INT(RND * 15) + 1: PRINT "PAUSE"
        SLEEP
        LOCATE 5, 39: PRINT "     "
        IF INKEY$ = "p" THEN
        END IF
ELSEIF control$ = CHR$(27) THEN
        GOSUB menu
END IF

speedcount1& = speedcount1& + 1
padspeed1& = (speedcount1& / (21 - pspeed1&))
IF padspeed1& > pspeed1& / 2 THEN padspeed1& = pspeed1& / 2

speedcount2& = speedcount2& + 1
padspeed2& = (speedcount2& / (21 - pspeed2&))
IF padspeed2& > pspeed2& / 2 THEN padspeed2& = pspeed2& / 2

LINE (1, (p1pad& - padsize1& - 1))-(10, (p1pad& - 10 - padsize1& - padspeed1&)), 0, BF
LINE (1, (p1pad& + padsize1& + 1))-(10, (p1pad& + 10 + padsize1& + padspeed1&)), 0, BF
LINE (638, (p2pad& - padsize2& - 1))-(628, (p2pad& - 10 - padsize2& - padspeed2&)), 0, BF
LINE (638, (p2pad& + padsize2& + 1))-(628, (p2pad& + 10 + padsize2& + padspeed2&)), 0, BF

IF p1dir& > 0 THEN
        IF p1dir& = 1 THEN p1pad& = p1pad& - padspeed1&
        IF p1dir& = 2 THEN p1pad& = p1pad& + padspeed1&
        refill& = 1
END IF

IF p2dir& > 0 THEN
        IF p2dir& = 1 THEN p2pad& = p2pad& - padspeed2&
        IF p2dir& = 2 THEN p2pad& = p2pad& + padspeed2&
        refill& = 1
END IF

IF p1comp& > 0 THEN
        IF p1comp& = 1 THEN
                c1c& = c1c& + 1
                IF c1c& = 100 THEN
                        c1c& = 0
                        c1acc& = INT(RND * (padsize1& + 10)) + 1
                END IF
        END IF
        IF p1comp& = 2 THEN c1acc& = padspeed1&
        IF p1comp& = 1 THEN i& = 540
        IF p1comp& = 2 THEN i& = 640
        IF serve& = 0 AND ballx& < i& THEN
                IF bdirx& < 0 THEN
                        c1des& = ABS(bally& - bdiry& * ((10 + ballx&) / bdirx&))
                        IF c1des& > 480 THEN c1des& = ABS(480 - (c1des& - 480))
                ELSEIF bdirx& > 0 THEN
                        c1des& = 240
                END IF
        ELSEIF serve& = 1 THEN
                c1des& = c1tdes&
                IF ABS(p1pad& - c1des&) <= padsize1& THEN GOSUB serve1
        ELSEIF serve& = 2 THEN
                c1des& = 240
        END IF
        IF p1pad& < c1des& THEN
                IF p1dir& <> 2 THEN
                        speedcount1& = 0
                        p1dir& = 2
                END IF
        ELSEIF p1pad& > c1des& THEN
                IF p1dir& <> 1 THEN
                        speedcount1& = 0
                        p1dir& = 1
                END IF
        END IF
        IF ABS(p1pad& - c1des&) <= c1acc& AND serve& <> 1 THEN p1dir& = 0
        IF p1shot& = 0 AND guns& = 1 AND INT(RND * 50) + 1 = 1 THEN
                SOUND 500, 1
                p1shoty& = p1pad&
                p1shotx& = 10
                p1shot& = 1
        END IF
END IF

IF p2comp& > 0 THEN
        IF p2comp& = 1 THEN
                c2c& = c2c& + 1
                IF c2c& = 100 THEN
                        c2c& = 0
                        c2acc& = INT(RND * (padsize2& + 10)) + 1
                END IF
        END IF
        IF p2comp& = 2 THEN c2acc& = padspeed2&
        IF p2comp& = 1 THEN i& = 100
        IF p2comp& = 2 THEN i& = 0
        IF serve& = 0 AND ballx& > i& THEN
                IF bdirx& > 0 THEN
                        c2des& = ABS(bally& + bdiry& * ((630 - ballx&) / bdirx&))
                        IF c2des& > 480 THEN c2des& = ABS(480 - (c2des& - 480))
                ELSEIF bdirx& < 0 THEN
                        c2des& = 240
                END IF
        ELSEIF serve& = 2 THEN
                c2des& = c2tdes&
                IF ABS(p2pad& - c2des&) <= padsize2& THEN GOSUB serve2
        ELSEIF serve& = 1 THEN
                c2des& = 240
        END IF
        IF p2pad& < c2des& THEN
                IF p2dir& <> 2 THEN
                speedcount2& = 0
                p2dir& = 2
                END IF
        ELSEIF p2pad& > c2des& THEN
                IF p2dir& <> 1 THEN
                speedcount2& = 0
                p2dir& = 1
                END IF
        END IF
        IF ABS(p2pad& - c2des&) <= c2acc& AND serve& <> 2 THEN p2dir& = 0
        IF p2shot& = 0 AND guns& = 1 AND INT(RND * 50) + 1 = 1 THEN
                SOUND 500, 1
                p2shoty& = p2pad&
                p2shotx& = 628
                p2shot& = 1
        END IF
END IF

IF p1pad& < padsize1& THEN p1pad& = padsize1&
IF p1pad& > 480 - padsize1& THEN p1pad& = 480 - padsize1&
IF p2pad& < padsize2& THEN p2pad& = padsize2&
IF p2pad& > 480 - padsize2& THEN p2pad& = 480 - padsize2&

LINE (ballx& - 5, bally& - 5)-(ballx& + 5, bally& + 5), 0, BF
LINE (ballx&, bally&)-(tx(1), ty(1)), 0
FOR i = 2 TO trailsize&
IF ABS(tx(i - 1) - tx(i)) <= 100 THEN LINE (tx(i - 1), ty(i - 1))-(tx(i), ty(i)), 0
NEXT i

IF serve& = 1 THEN
        ballx& = 16
        bally& = p1pad&
ELSEIF serve& = 2 THEN
        ballx& = 623
        bally& = p2pad&
END IF

FOR i = trailsize& TO 2 STEP -1
tx(i) = tx(i - 1)
ty(i) = ty(i - 1)
NEXT i
tx(1) = ballx&
ty(1) = bally&

IF serve& = 0 THEN
        ballx& = ballx& + bdirx&
        bally& = bally& + bdiry&
END IF
   
IF bally& < 7 AND bdiry& <> 0 THEN
        bally& = 7
        bdiry& = -bdiry&
        IF soundmode& = 1 THEN SOUND 500, 1
        refill& = 1
END IF

IF bally& > 473 AND bdiry& <> 0 THEN
        bally& = 473
        bdiry& = -bdiry&
        IF soundmode& = 1 THEN SOUND 500, 1
        refill& = 1
END IF

LINE (ballx& - 4, bally& - 4)-(ballx& + 4, bally& + 4), ballcol&, BF
LINE (ballx&, bally&)-(tx(1), ty(1)), ballcol&
FOR i = 2 TO trailsize&
IF ABS(tx(i - 1) - tx(i)) <= 100 THEN LINE (tx(i - 1), ty(i - 1))-(tx(i), ty(i)), ballcol&
NEXT i

IF refill& > 0 THEN
        LINE (1, (p1pad& - padsize1&))-(10, (p1pad& + padsize1&)), 4, BF
        LINE (638, (p2pad& - padsize2&))-(628, (p2pad& + padsize2&)), 1, BF
        IF mag1& = 1 THEN LINE (1, (p1pad& - padsize1&))-(10, (p1pad& + padsize1&)), 12, B
        IF mag2& = 1 THEN LINE (638, (p2pad& - padsize2&))-(628, (p2pad& + padsize2&)), 9, B
        IF shield1& = 1 THEN
                LINE (5, 1)-(5, p1pad& - padsize1& - 10), 12
                LINE (5, 480)-(5, p1pad& + padsize1& + 10), 12
        END IF
        IF shield2& = 1 THEN
                LINE (632, 1)-(632, p2pad& - padsize2& - padspeed2& - 10), 9
                LINE (632, 480)-(632, p2pad& + padsize2& + padspeed2& + 10), 9
        END IF
        LINE (0, 0)-(639, 479), 14, B
        refill& = refill& - 1
END IF

IF bdirx& = 0 THEN bdirx& = INT(RND * ballspeed& * 2) - ballspeed&

IF (ballx& - 5) < 11 THEN
        IF (p1pad& - padsize1&) < bally& AND bally& < (p1pad& + padsize1&) OR shield1& = 1 THEN
                IF (p1pad& - padsize1&) >= bally& OR bally& >= (p1pad& + padsize1&) THEN
                        LINE (5, 1)-(5, p1pad& - padsize1& - padspeed1& - 10), 0
                        LINE (5, 480)-(5, p1pad& + padsize1& + padspeed1& + 10), 0
                        shield1& = 0
                END IF
                ballcol& = INT(RND * 15) + 1
                IF bdiry& = 0 THEN bdiry& = INT(RND * 2 * ballspeed&) - ballspeed&
                IF bdiry& < 0 THEN bdiry& = -(INT(RND * ballspeed&))
                IF bdiry& > 0 THEN bdiry& = INT(RND * ballspeed&)
                bdirx& = -bdirx&
                IF soundmode& = 1 THEN SOUND 700, 1
                ballhit& = 1
                combo& = combo& + 1
                IF mag1& = 1 THEN serve& = 1
        ELSE GOSUB score2
        END IF
        c2tdes& = INT(RND * 380) + 50
        refill& = 1
END IF

IF (ballx& + 5) > 628 THEN
        IF (p2pad& - padsize2&) < bally& AND bally& < (p2pad& + padsize2&) OR shield2& = 1 THEN
                IF (p2pad& - padsize2&) >= bally& OR bally& >= (p2pad& + padsize2&) THEN
                        LINE (632, 1)-(632, p2pad& - padsize2& - padspeed2& - 10), 0
                        LINE (632, 480)-(632, p2pad& + padsize2& + padspeed2& + 10), 0
                        shield2& = 0
                END IF
                ballcol& = INT(RND * 15) + 1
                IF bdiry& = 0 THEN bdiry& = INT(RND * ballspeed& * 2) - ballspeed&
                IF bdiry& < 0 THEN bdiry& = -(INT(RND * ballspeed&))
                IF bdiry& > 0 THEN bdiry& = INT(RND * ballspeed&)
                bdirx& = -bdirx&
                IF soundmode& = 1 THEN SOUND 700, 1
                ballhit& = 2
                combo& = combo& + 1
                IF mag2& = 1 THEN serve& = 2
        ELSE GOSUB score1
        END IF
        c1tdes& = INT(RND * 380) + 50
        refill& = 1
END IF

test& = test& + 1
IF test& > itemop& THEN test& = 1
FOR i = 1 TO itemop&
IF test& = i THEN LINE (itemx(i) - isize(i), itemy(i) - isize(i))-(itemx(i) + isize(i), itemy(i) + isize(i)), itcol(i), BF
IF (itemx(i) - isize(i)) < ballx& + 4 AND ballx& - 4 < (itemx(i) + isize(i)) AND (itemy(i) - isize(i)) < bally& + 4 AND bally& - 4 < (itemy(i) + isize(i)) THEN GOSUB item
NEXT i

IF bonus& = 1 THEN
        LINE (bonusx& - 20, bonusy& - 20)-(bonusx& + 20, bonusy& + 20), INT(RND * 16), BF
        IF (bonusx& - 29) < ballx& AND ballx& < (bonusx& + 29) AND (bonusy& - 29) < bally& AND bally& < (bonusy& + 29) THEN GOSUB bonus
END IF

IF p1shot& = 1 THEN
        LINE (p1shotx& - 1, p1shoty& - 1)-(p1shotx& + 1, p1shoty& + 1), 0, BF
        p1shotx& = p1shotx& + 2
        LINE (p1shotx& - 1, p1shoty& - 1)-(p1shotx& + 1, p1shoty& + 1), 4, BF
        IF p1shotx& > 629 THEN
                IF (p2pad& - padsize2&) < p1shoty& AND p1shoty& < (p2pad& + padsize2&) THEN
                        expx& = p1shotx&
                        expy& = p1shoty&
                        GOSUB shotexp
                        p1score& = p1score& + 1
                        p2score& = p2score& - 1
                END IF
                p1shot& = 0
                LINE (p1shotx& - 2, p1shoty& - 2)-(p1shotx& + 2, p1shoty& + 2), 0, BF
        END IF
END IF

IF p2shot& = 1 THEN
        LINE (p2shotx& - 1, p2shoty& - 1)-(p2shotx& + 1, p2shoty& + 1), 0, BF
        p2shotx& = p2shotx& - 2
        LINE (p2shotx& - 1, p2shoty& - 1)-(p2shotx& + 1, p2shoty& + 1), 1, BF
        IF p2shotx& < 11 THEN
                IF (p1pad& - padsize1&) < p2shoty& AND p2shoty& < (p1pad& + padsize1&) THEN
                        expx& = p2shotx&
                        expy& = p2shoty&
                        GOSUB shotexp
                        p2score& = p2score& + 1
                        p1score& = p1score& - 1
                END IF
                p2shot& = 0
                LINE (p2shotx& - 2, p2shoty& - 2)-(p2shotx& + 2, p2shoty& + 2), 0, BF
        END IF
END IF

IF p1score& >= winmode& THEN GOSUB win1
IF p2score& >= winmode& THEN GOSUB win2

LOOP

serve1:
IF p1dir& = 0 THEN RETURN
IF p1dir& = 1 THEN
        bdirx& = ballspeed&
        bdiry& = -ballspeed&
ELSEIF p1dir& = 2 THEN
        bdirx& = ballspeed&
        bdiry& = ballspeed&
ELSEIF p1dir& = 0 AND p1comp& > 0 THEN
        bdirx& = ballspeed&
        bdiry& = INT(RND * ballspeed * 2) - ballspeed&
END IF
IF combo& <= 1000000000 THEN combo& = combo& + 1
ballhit& = 1
serve& = 0
RETURN

serve2:
IF p2dir& = 0 THEN RETURN
IF p2dir& = 1 THEN
        bdirx& = -ballspeed&
        bdiry& = -ballspeed&
ELSEIF p2dir& = 2 THEN
        bdirx& = -ballspeed&
        bdiry& = ballspeed&
ELSEIF p2dir& = 0 AND p2comp& > 0 THEN
        bdirx& = ballspeed&
        bdiry& = INT(RND * ballspeed * 2) - ballspeed&
END IF
IF combo& <= 1000000000 THEN combo& = combo& + 1
ballhit& = 2
serve& = 0
RETURN

score1:
LINE (ballx& - 5, bally& - 5)-(ballx& + 5, ballx& + 5), 0, BF
LINE (ballx&, bally&)-(trailx&, traily&), 0
LOCATE 31, 40: PRINT "          "
IF soundmode& = 1 THEN SOUND 100, 5
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
LINE (ballx& - 5, bally& - 5)-(ballx& + 5, ballx& + 5), 0, BF
LINE (ballx&, bally&)-(trailx&, traily&), 0
LOCATE 31, 40: PRINT "          "
IF soundmode& = 1 THEN SOUND 100, 5
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
IF soundmode& = 1 THEN SOUND 1500, 1
IF ballhit& = 1 THEN mag1& = 0
IF ballhit& = 2 THEN mag2& = 0
i& = 30
IF ibd& = 0 AND ibs& = 0 AND ips& = 0 AND ifp& = 0 AND ici& = 0 AND ipsp& = 0 AND ish& = 0 THEN i& = 2
rand& = INT(RND * i&) + 1
IF ibd& = 0 AND ibs& = 0 AND ips& = 0 AND ifp& = 0 AND ici& = 0 AND ipsp& = 0 AND ish& = 0 AND iub& = 0 THEN rand& = 2
IF ibd& = 0 AND ibs& = 0 AND ips& = 0 AND ifp& = 0 AND ici& = 0 AND ipsp& = 0 AND ish& = 0 AND itd& = 0 THEN rand& = 1
IF iub& = 1 AND rand& = 1 THEN
        IF bonus& = 1 THEN LINE (bonusx& - 20, bonusy& - 20)-(bonusx& + 20, bonusy& + 20), 0, BF
        bonusx& = INT(RND * 640) + 1
        bonusy& = INT(RND * 480) + 1
        bonus& = 1
        GOTO afterskip
END IF
IF itd& = 1 AND rand& = 2 THEN
        LINE (0, 0)-(640, 480), 15, BF
        IF combo& <= 1000000000 THEN combo& = combo& * 2
        LINE (0, 0)-(640, 480), 0, BF
        refill& = 1
END IF
               
fixer:

IF ibd& = 0 AND ibs& = 0 AND ips& = 0 AND ifp& = 0 AND ici& = 0 AND ipsp& = 0 AND ish& = 0 THEN GOTO afterskip

rand& = INT(RND * 8) + 1
IF rand& = 1 THEN
        IF ibd& = 0 THEN GOTO fixer
        bdirx& = INT(RND * ballspeed& * 2) - ballspeed&
        bdiry& = INT(RND * ballspeed& * 2) - ballspeed&
ELSEIF rand& = 2 THEN
        IF ibs& = 0 THEN GOTO fixer
        ballspeed& = INT(RND * 4) + 2
        IF soundmode& = 1 THEN SOUND (ballspeed& * 200), 1
ELSEIF rand& = 3 THEN
        IF ips& = 0 THEN GOTO fixer
        IF soundmode& = 1 THEN SOUND 1250, 1
        IF ballhit& = 1 THEN
                LINE (1, 0)-(10, 480), 4, BF
                padsize1& = INT(RND * 80) + 20
                LINE (0, 0)-(10, 480), 0, BF
        ELSEIF ballhit& = 2 THEN
                LINE (628, 0)-(638, 480), 1, BF
                padsize2& = INT(RND * 80) + 20
                LINE (628, 0)-(638, 480), 0, BF
        END IF
ELSEIF rand& = 4 THEN
        IF ifp& = 0 THEN GOTO fixer
        IF ballhit& = 1 THEN p1score& = p1score& + INT(RND * 10) + 10
        IF ballhit& = 2 THEN p2score& = p2score& + INT(RND * 10) + 10
ELSEIF rand& = 5 THEN
        IF ici& = 0 THEN GOTO fixer
        IF combo& <= 1000000000 THEN combo& = combo& + INT(RND * 10) + 10
ELSEIF rand& = 6 THEN
        IF ise& = 0 THEN GOTO fixer
        IF ballhit& = 1 THEN mag1& = 1
        IF ballhit& = 2 THEN mag2& = 1
ELSEIF rand& = 7 THEN
        IF ipsp& = 0 THEN GOTO fixer
        rand2& = INT(RND * 16) + 5
        IF ballhit& = 1 THEN
                IF pspeed1& > rand2& THEN
                        a& = 25
                        b& = 1
                        c& = -1
                ELSE
                        a& = 1
                        b& = 25
                        c& = 1
                END IF
                pspeed1& = rand2&
        END IF
        IF ballhit& = 2 THEN
                IF pspeed2& > rand2& THEN
                        a& = 25
                        b& = 1
                        c& = -1
                ELSE
                        a& = 1
                        b& = 25
                        c& = 1
                END IF
                pspeed2& = rand2&
        END IF
        FOR soun = a& TO b& STEP c&
        IF soundmode& = 1 THEN SOUND soun * 100, .1
        NEXT soun
ELSEIF rand& = 8 THEN
        IF ish& = 0 THEN GOTO fixer
        IF ballhit& = 1 THEN shield1& = shield1& + 1
        IF ballhit& = 2 THEN shield2& = shield2& + 1
        IF shield1& = 2 THEN
                LINE (5, 1)-(5, p1pad& - padsize1& - padspeed1& - 10), 0
                LINE (5, 480)-(5, p1pad& + padsize1& + padspeed1& + 10), 0
                shield1& = 0
        END IF
        IF shield2& = 2 THEN
                LINE (632, 1)-(632, p2pad& - padsize2& - padspeed2& - 10), 0
                LINE (632, 480)-(632, p2pad& + padsize2& + padspeed2& + 10), 0
                shield2& = 0
        END IF
END IF
afterskip:
LINE (itemx(i) - isize(i), itemy(i) - isize(i))-(itemx(i) + isize(i), itemy(i) + isize(i)), 0, BF
itcol(i) = INT(RND * 15) + 1
itemx(i) = INT(RND * 490) + 100
itemy(i) = INT(RND * 330) + 100
isize(i) = INT(RND * 5) + 5
refill& = 1
RETURN

bonus:
FOR i = 1500 TO 1000 STEP -100
IF soundmode& = 1 THEN SOUND i, .1
NEXT i
x& = bonusx&
y& = bonusy&
FOR i& = 1 TO 100
CIRCLE (x&, y&), i&, 14
CIRCLE (x&, y&), i&, 0
NEXT i&
IF ballhit& = 1 THEN p1score& = p1score& + INT(RND * 100) + 1
IF ballhit& = 2 THEN p2score& = p2score& + INT(RND * 100) + 1
bonus& = 0
refill& = 1
CLS
RETURN

shotexp:
FOR i = 1 TO 4
FOR ii = 1 TO INT(RND * 9) + 6
IF INT(RND * 2) + 1 = 1 THEN col& = 4 ELSE col& = 14
CIRCLE (expx& + INT(RND * 30) - 15, expy& + INT(RND * 30) - 15), ii, col&
IF soundmode& = 1 THEN SOUND INT(RND * 1000) + 500, .1
NEXT ii
FOR delay = 1 TO 50
NEXT delay
NEXT i
CLS
refill& = 1
RETURN

win1:
LINE (0, 0)-(640, 480), 4, BF
LINE (250, 190)-(390, 290), 0, BF
LOCATE 26, 36: COLOR 4: PRINT "RED WINS!"
LOCATE 29, 37: COLOR 4: PRINT "p1:"
LOCATE 29, 41: PRINT p1score&
LOCATE 30, 37: COLOR 1: PRINT "p2:"
LOCATE 30, 41: PRINT p2score&
LOCATE 35, 35: COLOR 7: PRINT "PRESS A KEY"
IF soundmode& = 1 THEN SOUND 1000, 10
SLEEP
start& = 1
GOSUB menu

win2:
LINE (0, 0)-(640, 480), 1, BF
LINE (250, 190)-(390, 290), 0, BF
LOCATE 26, 36: COLOR 1: PRINT "BLUE WINS!"
LOCATE 29, 37: COLOR 4: PRINT "p1:"
LOCATE 29, 41: PRINT p1score&
LOCATE 30, 37: COLOR 1: PRINT "p2:"
LOCATE 30, 41: PRINT p2score&
LOCATE 35, 35: COLOR 7: PRINT "PRESS A KEY"
IF soundmode& = 1 THEN SOUND 1000, 10
SLEEP
start& = 1
GOSUB menu


menu:
CLS
mballx& = 182
mbally& = 24
mpad1& = 24
mpad2& = 24
mbdirx& = 1
mbdiry& = 1
itsw& = 0
DO
LOCATE 1, 1: COLOR 5: PRINT "PING!"
LOCATE 1, 7: COLOR 7: PRINT "OPTION MENU"
LOCATE 3, 1: COLOR 15: PRINT "S"
LOCATE 3, 2: COLOR 2: PRINT "CORING MODE"
LOCATE 4, 1: COLOR scnop&: PRINT "NORMAL"
LOCATE 4, 10: COLOR scuop&: PRINT "ULTRA"
LOCATE 6, 1: COLOR 3: PRINT "# OF "
LOCATE 6, 6: COLOR 15: PRINT "I"
LOCATE 6, 7: COLOR 3: PRINT "TEMS"
LOCATE 7, 1: COLOR 15: PRINT itemop&
LOCATE 9, 1: COLOR 15: PRINT "B"
LOCATE 9, 2: COLOR 6: PRINT "ALL SPEED"
LOCATE 9, 12: COLOR ballspeed&: PRINT ballspeed&
LOCATE 11, 1: COLOR 15: PRINT "W"
LOCATE 11, 2: COLOR 3: PRINT "IN"
LOCATE 11, 5: COLOR 11: PRINT winmode&
LOCATE 13, 1: COLOR 7: PRINT "PLAYER"
LOCATE 13, 8: COLOR 15: PRINT "1"
LOCATE 14, 1: COLOR 8: PRINT p1mode$
LOCATE 16, 1: COLOR 7: PRINT "PLAYER"
LOCATE 16, 8: COLOR 15: PRINT "2"
LOCATE 17, 1: COLOR 8: PRINT p2mode$
LOCATE 19, 1: COLOR 15: PRINT "G"
LOCATE 19, 2: COLOR 4: PRINT "UNS"
LOCATE 19, 6: COLOR 15: PRINT gunmode$
LOCATE 21, 1: COLOR 6: PRINT "GAME SPEE"
LOCATE 21, 10: COLOR 15: PRINT "D"
LOCATE 21, 11: COLOR 15: PRINT delay&
LOCATE 22, 1: COLOR 6: PRINT "(1-10000)"
LOCATE 24, 1: COLOR 15: PRINT "P"
LOCATE 24, 2: COLOR 9: PRINT "ADSPEED"
LOCATE 24, 10: COLOR 14: PRINT padspeed&
LOCATE 26, 1: COLOR 12: PRINT "S"
LOCATE 26, 2: COLOR 15: PRINT "O"
LOCATE 26, 3: COLOR 12: PRINT "UND"
LOCATE 26, 7: COLOR 8: IF soundmode& = 1 THEN PRINT "ON" ELSE PRINT "OFF"
LOCATE 28, 1: COLOR 15: PRINT "N"
LOCATE 28, 2: COLOR 14: PRINT "EW GAME"
IF start& = 0 THEN
        LOCATE 29, 1: COLOR 15: PRINT "R"
        LOCATE 29, 2: COLOR 1: PRINT "ETURN TO GAME"
END IF
LOCATE 30, 1: COLOR 15: PRINT "Q"
LOCATE 30, 2: COLOR 4: PRINT "UIT"
LOCATE 9, 21: COLOR 5: PRINT "I"
LOCATE 9, 22: COLOR 15: PRINT "T"
LOCATE 9, 23: COLOR 5: PRINT "EM SWITCH"
LOCATE 10, 20: COLOR 15: PRINT "1-"
LOCATE 10, 23: COLOR swcol&: PRINT "Pad Resizer"
LOCATE 10, 45: COLOR ips&: PRINT "!"
LOCATE 11, 20: COLOR 15: PRINT "2-"
LOCATE 11, 23: COLOR swcol&: PRINT "Free Points"
LOCATE 11, 45: COLOR ifp&: PRINT "!"
LOCATE 12, 20: COLOR 15: PRINT "3-"
LOCATE 12, 23: COLOR swcol&: PRINT "Ball Direction Changer"
LOCATE 12, 45: COLOR ibd&: PRINT "!"
LOCATE 13, 20: COLOR 15: PRINT "4-"
LOCATE 13, 23: COLOR swcol&: PRINT "Change Ball Speed"
LOCATE 13, 45: COLOR ibs&: PRINT "!"
LOCATE 14, 20: COLOR 15: PRINT "5-"
LOCATE 14, 23: COLOR swcol&: PRINT "Ultra Bonus Points"
LOCATE 14, 45: COLOR iub&: PRINT "!"
LOCATE 15, 20: COLOR 15: PRINT "6-"
LOCATE 15, 23: COLOR swcol&: PRINT "Texas Doubler"
LOCATE 15, 45: COLOR itd&: PRINT "!"
LOCATE 16, 20: COLOR 15: PRINT "7-"
LOCATE 16, 23: COLOR swcol&: PRINT "Combo Increase"
LOCATE 16, 45: COLOR ici&: PRINT "!"
LOCATE 17, 20: COLOR 15: PRINT "8-"
LOCATE 17, 23: COLOR swcol&: PRINT "Paddle Magnet"
LOCATE 17, 45: COLOR ise&: PRINT "!"
LOCATE 18, 20: COLOR 15: PRINT "9-"
LOCATE 18, 23: COLOR swcol&: PRINT "Pad Speed Changer"
LOCATE 18, 45: COLOR ipsp&: PRINT "!"
LOCATE 19, 20: COLOR 15: PRINT "A-"
LOCATE 19, 23: COLOR swcol&: PRINT "Shield Defense"
LOCATE 19, 45: COLOR ish&: PRINT "!"
LOCATE 3, 50: COLOR 4: PRINT "Controls:"
LOCATE 5, 56: PRINT "Up:":
LOCATE 6, 48: PRINT "Stop/Serve:"
LOCATE 7, 54: PRINT "Down:"
LOCATE 8, 55: PRINT "Gun:"
LOCATE 10, 53: PRINT "Pause:"
LOCATE 11, 54: PRINT "Exit:"
LOCATE 3, 63: COLOR 5: PRINT "P1"
LOCATE 5, 63: PRINT "q"
LOCATE 6, 63: PRINT "a"
LOCATE 7, 63: PRINT "z"
LOCATE 8, 63: PRINT "s"
LOCATE 3, 69: COLOR 6: PRINT "P2"
LOCATE 5, 69: PRINT "]"
LOCATE 6, 69: PRINT "'"
LOCATE 7, 69: PRINT "/"
LOCATE 8, 69: PRINT ";"
LOCATE 10, 66: COLOR 3: PRINT "p"
LOCATE 11, 65: PRINT "Esc"

control$ = INKEY$
IF itsw& = 0 THEN
        IF control$ = "s" THEN
                scoremode& = scoremode& + 1
        ELSEIF control$ = "i" THEN
                itemop& = itemop& + 1
        ELSEIF control$ = "b" THEN
                ballspeed& = ballspeed& + 1
        ELSEIF control$ = "w" THEN
                LOCATE 11, 1: PRINT "              "
                LOCATE 11, 1: COLOR 15: INPUT "WIN ", winmode&
        ELSEIF control$ = "d" THEN
                LOCATE 21, 1: PRINT "                "
                LOCATE 21, 1: COLOR 15: INPUT "GAME SPEED ", delay&
        ELSEIF control$ = "1" THEN
                LOCATE 14, 1: PRINT "            "
                p1co& = p1co& + 1
        ELSEIF control$ = "2" THEN
                LOCATE 17, 1: PRINT "            "
                p2co& = p2co& + 1
        ELSEIF control$ = "g" THEN
                LOCATE 19, 6: PRINT "   "
                guns& = guns& + 1
        ELSEIF control$ = "q" THEN
                END
        ELSEIF control$ = "r" THEN
                IF start& = 0 THEN
                        CLS
                        refill& = 1
                        RETURN
                END IF
        ELSEIF control$ = "n" THEN
                CLS
                p1pad& = 240
                p2pad& = 240
                padsize1& = 25
                padsize2& = 25
                refill& = 1
                start& = 0
                GOTO start
        ELSEIF control$ = "t" THEN
                itsw& = 1
        ELSEIF control$ = "p" THEN
                padspeed& = padspeed& + 1
                pspeed1& = padspeed&
                pspeed2& = padspeed&
        ELSEIF control$ = "o" THEN
                LOCATE 26, 7: PRINT "   "
                soundmode& = soundmode& + 1
                IF soundmode& > 1 THEN soundmode& = 0
        END IF
        swcol& = 8
ELSEIF itsw& = 1 THEN
        IF control$ = "1" THEN
                IF ips& = 0 THEN ips& = 1 ELSE ips& = 0
        ELSEIF control$ = "2" THEN
                IF ifp& = 0 THEN ifp& = 1 ELSE ifp& = 0
        ELSEIF control$ = "3" THEN
                IF ibd& = 0 THEN ibd& = 1 ELSE ibd& = 0
        ELSEIF control$ = "4" THEN
                IF ibs& = 0 THEN ibs& = 1 ELSE ibs& = 0
        ELSEIF control$ = "5" THEN
                IF iub& = 0 THEN iub& = 1 ELSE iub& = 0
        ELSEIF control$ = "6" THEN
                IF itd& = 0 THEN itd& = 1 ELSE itd& = 0
        ELSEIF control$ = "7" THEN
                IF ici& = 0 THEN ici& = 1 ELSE ici& = 0
        ELSEIF control$ = "8" THEN
                IF ise& = 0 THEN ise& = 1 ELSE ise& = 0
        ELSEIF control$ = "9" THEN
                IF ipsp& = 0 THEN ipsp& = 1 ELSE ipsp& = 0
        ELSEIF control$ = "a" THEN
                IF ish& = 0 THEN ish& = 1 ELSE ish& = 0
        ELSEIF control$ = "t" THEN
                itsw& = 0
        END IF
        swcol& = 13
END IF

IF ballspeed& > 10 THEN ballspeed& = 1
IF p1co& > 3 THEN p1co& = 1
IF p2co& > 3 THEN p2co& = 1
IF scoremode& > 2 THEN scoremode& = 1
IF guns& > 1 THEN guns& = 0
IF padspeed& > 20 THEN padspeed& = 1

IF scoremode& = 1 THEN
        scnop& = 15
        scuop& = 8
ELSEIF scoremode& = 2 THEN
        scnop& = 8
        scuop& = 15
END IF

IF p1co& = 1 THEN
        p1mode$ = "HUMAN"
        p1comp& = 0
ELSEIF p1co& = 2 THEN
        p1mode$ = "COMP (EASY)"
        p1comp& = 1
ELSEIF p1co& = 3 THEN
        p1mode$ = "COMP (HARD)"
        p1comp& = 2
END IF

IF p2co& = 1 THEN
        p2mode$ = "HUMAN"
        p2comp& = 0
ELSEIF p2co& = 2 THEN
        p2mode$ = "COMP (EASY)"
        p2comp& = 1
ELSEIF p2co& = 3 THEN
        p2mode$ = "COMP (HARD)"
        p2comp& = 2
END IF

IF itemop& > itemnum& THEN itemop& = 0

IF itemop& = 0 THEN
        it0& = 15
        it1& = 8
        it2& = 8
        it3& = 8
ELSEIF itemop& = 1 THEN
        it0& = 8
        it1& = 15
        it2& = 8
        it3& = 8
ELSEIF itemop& = 2 THEN
        it0& = 8
        it1& = 8
        it2& = 15
        it3& = 8
ELSEIF itemop& = 3 THEN
        it0& = 8
        it1& = 8
        it2& = 8
        it3& = 15
END IF

IF guns& = 0 THEN gunmode$ = "OFF"
IF guns& = 1 THEN gunmode$ = "ON"

VIEW SCREEN (150, 0)-(214, 48)
LINE (150, 0)-(214, 48), 14, B
LINE (mballx& - 1, mbally& - 1)-(mballx& + 1, mbally& + 1), 15, BF
LINE (mballx& - 2, mbally& - 2)-(mballx& + 2, mbally& + 2), 0, B
LINE (151, mpad1& - 3)-(152, mpad1& + 3), 1, B
LINE (151, mpad1& - 4)-(152, mpad1& - 4), 0
LINE (151, mpad1& + 4)-(152, mpad1& + 4), 0
LINE (213, mpad2& - 3)-(212, mpad2& + 3), 4, B
LINE (213, mpad2& - 4)-(212, mpad2& - 4), 0
LINE (213, mpad2& + 4)-(212, mpad2& + 4), 0

IF mhit& = 1 THEN IF mbally& < mpad2& THEN mpad2& = mpad2& - 1 ELSE IF mbally& > mpad2& THEN mpad2& = mpad2& + 1
IF mhit& = 2 THEN IF mbally& < mpad1& THEN mpad1& = mpad1& - 1 ELSE IF mbally& > mpad1& THEN mpad1& = mpad1& + 1

mballx& = mballx& + mbdirx&
mbally& = mbally& + mbdiry&

IF mbally& < 2 THEN mbdiry& = -mbdiry&
IF mbally& > 48 THEN mbdiry& = -mbdiry&

IF mballx& < 152 THEN
        IF soundmode& = 1 THEN SOUND INT(RND * 1000) + 500, .1
        mbdirx& = -mbdirx&
        mhit& = 1
END IF

IF mballx& > 214 THEN
        IF soundmode& = 1 THEN SOUND INT(RND * 1000) + 500, .1
        mbdirx& = -mbdirx&
        mhit& = 2
END IF

VIEW

LOOP

title:
DO
LOCATE INT(RND * 28) + 1, INT(RND * 75) + 1: COLOR INT(RND * 15) + 1: PRINT "Ping!"
LOCATE 1, 1: COLOR INT(RND * 15) + 1: PRINT "Ping!"
LOCATE 1, 75: COLOR INT(RND * 15) + 1: PRINT "Ping!"
LOCATE 28, 1: COLOR INT(RND * 15) + 1: PRINT "Ping!"
LOCATE 28, 75: COLOR INT(RND * 15) + 1: PRINT "Ping!"
i& = i& + 1
IF i& > 10 THEN i& = 0
IF i& = 1 THEN titlecolor& = INT(RND * 15) + 1
FOR e& = 0 TO 37
FOR f& = 0 TO 15
c& = e&
d& = f&
test& = POINT(e&, f&)
IF test& <> 0 THEN LINE ((c& * 10) - 9 + 135, (d& * 10) - 9 + 10)-(c& * 10 + 135, d& * 10 + 10), titlecolor&, BF
NEXT f&
NEXT e&
LOCATE 12, 32: COLOR INT(RND * 15) + 1: PRINT "By Funkmaster Q"
LOCATE 20, 33: COLOR INT(RND * 15) + 1: PRINT "PRESS A KEY!"
LINE (245, 175)-(369, 192), INT(RND * 15) + 1, B
IF INKEY$ <> "" THEN EXIT DO
LOOP
RETURN

