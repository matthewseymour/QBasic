'
'     €€€€€ €     €   € €€€€€ €€€€€ €€€€€ €    €
'     €     €     €   € €     €     €     €€   €
'     €€€€€ €     €   € €€€€€ €€€€€ €€€   € ﬂ‹ €
'         € €     €   €     €     € €     €   €€
'     €€€€€ €€€€€ €€€€€ €€€€€ €€€€€ €€€€€ €    €
'     ÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓ          /ÓÓÓÓÓÓÓÓÓÓ\
'            *                                            |   Hit    |
'     €€€€€  €  €€€€€ €   € €€€€€ €€€€€ €€€€  €€€€€       | shift+F5 |
'     €      €  €     €   €   €   €     €   € €           | to play! |
'     €€€    €  €  €€ €€€€€   €   €€€   €€€€  €€€€€       \__________/
'     €      €  €   € €   €   €   €     € ﬂ‹      €
'     €      €  €€€€€ €   €   €   €€€€€ €   € €€€€€
'     ÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓ
' The Last Great QBasic/QuickBasic Game of Otso Kassinen
' (Nowadays I use C++ more than QB, but I thought
' this cool game should be downloadable from the Web...)
' otkas@kastelli.edu.ouka.fi
'
'  CHEATS:
'  full energy for player 1: alt+1
'  full energy for player 2: alt+2
'  Secret fighter 1:  S H  at the 'choose your fighter' screen
'  Secret fighter 2:  Z R  at the 'choose your fighter' screen
'
'
Alku:
DECLARE SUB zalop ()
DECLARE SUB teezairar ()
DECLARE SUB shlop ()
DECLARE SUB verenlento ()
DECLARE SUB paketa ()
DECLARE SUB poistapelaajat ()
DECLARE SUB eulop ()
DECLARE SUB wilop ()
DECLARE SUB nulop ()
DECLARE SUB stlop ()
DECLARE SUB vilop ()
DECLARE SUB snlop ()
DECLARE SUB mulop ()
DECLARE SUB palop ()
DECLARE SUB polop ()
DECLARE SUB lalop ()
DECLARE SUB silop ()
DECLARE SUB telop ()
DECLARE SUB teetessuk ()
DECLARE SUB teesiliconiperse ()
DECLARE SUB teelaavi ()
DECLARE SUB teepaava ()
DECLARE SUB teepaeppa ()
DECLARE SUB teeMurg ()
DECLARE SUB poista2pkasi ()
DECLARE SUB poista2pjalka ()
DECLARE SUB poista1pkasi ()
DECLARE SUB poista1pjalka ()
DECLARE SUB f1 ()
DECLARE SUB f2 ()
DECLARE SUB f3 ()
DECLARE SUB f4 ()
DECLARE SUB f9 ()
DECLARE SUB f10 ()
DECLARE SUB f11 ()
DECLARE SUB f12 ()
DECLARE SUB teekaikki ()
DECLARE SUB teeaika ()
DECLARE SUB teeenergiat ()
DECLARE SUB poistasnailman ()
DECLARE SUB teesnailman ()
DECLARE SUB teenerasu ()
DECLARE SUB teekentta ()
DECLARE SUB poista1pelaaja ()
DECLARE SUB poista2pelaaja ()
DECLARE SUB teewille ()
DECLARE SUB tee1pelaaja ()
DECLARE SUB tee2pelaaja ()
DECLARE SUB teepelaajat ()
DECLARE SUB teestefan ()
DECLARE SUB teeVilhelmo ()
DECLARE SUB poistaerasu ()
DECLARE SUB poistawille ()
DECLARE SUB poistastefan ()
DECLARE SUB poistaVilhelmo ()
DECLARE SUB teeerasu ()
DECLARE SUB kuva ()
COMMON SHARED erasux, erasuy, p1$, p2$, p1x, p1y, p2x, p2y, called, valflag
COMMON SHARED p1energia, p2energia, f1n$, f2n$, f3n$, f4n$, f9n$, f10n$, f11n$, f12n$
COMMON SHARED printattavaaika, oikea$, vasen$, ylos$, p1hyppaa, p2hyppaa, p1vauhti, p2vauhti
COMMON SHARED p1pieree, p2pieree, p1pierx, p2pierx, p1piery, p2piery, p1suojaa
COMMON SHARED p2suojaa, p1tulee, p2tulee, p1tulix, p1tuliy, p2tulix, p2tuliy
COMMON SHARED p1pallox, p1palloy, p2pallox, p2palloy, p1pallottaa, p2pallottaa
COMMON SHARED palari1, palari2, p1viher, p2viher, p1sahkoo, p2sahkoo, sahx1
COMMON SHARED sahx2, saika1, saika2, taika, taika2, snak1, snak2, loppaa, voitx, havx
COMMON SHARED p1jahm, p2jahm, p1jaika, p2jaika, p1notjodlmaix, p2notjodlmaix, nopeaMode
COMMON SHARED p1njmxx, p2njmxx, naika1, naika2, p1suunta$, p2suunta$, nayt, duke
COMMON SHARED p1laavaa, p2laavaa, p1laax, p2laax, p1laay, p2laay, raika1, raika2
COMMON SHARED sjaika1, sjaika2, p1silii, p2silii, p1silix, p2silix, p1sily, p2sily
COMMON SHARED p1pysii, p2pysii, p1leijuu, p2leijuu, p1kuiluu, p2kuiluu, p1kuilux
COMMON SHARED p2kuilux, kuika1, kuika2, putii, fonnistu, voit$, hav$, voitti, tapetaan

'
' I'm sorry, the comments are in Finnish!
' didn't have time to translate 'em!
'

naytakuva = 0
nopeaMode = 0

oikea$ = CHR$(0) + "M"
vasen$ = CHR$(0) + "K"
ylos$ = CHR$(0) + "H"
f1n$ = CHR$(0) + ";"
f2n$ = CHR$(0) + "<"
f3n$ = CHR$(0) + "="
f4n$ = CHR$(0) + ">"
f9n$ = CHR$(0) + "C"
f10n$ = CHR$(0) + "D"
f11n$ = CHR$(0) + "Ö"
f12n$ = CHR$(0) + "Ü"
nayt = 1
RANDOMIZE TIMER
SCREEN 12
PLAY "mbo1l16ce-fl4f+l32ce-g>ce-g-a>cde-dl4e-"
CLS
IF naytakuva = 0 THEN
COLOR 9
LOCATE 14, 1
PRINT "                       ∞±≤€≤±∞"; : COLOR 11: PRINT "  SLUSSEN FIGHTERS  "; : COLOR 9: PRINT "∞±≤€≤±∞"
PRINT
PRINT
COLOR 5
PRINT "                                  Hit Any Key!"
FOR alkux = 0 TO 640 STEP 320
FOR oikx = 0 TO 640 STEP 10
LINE (alkux, 0)-(oikx, 150), 12
NEXT
NEXT
FOR alkux = 0 TO 640 STEP 320
FOR oikx = 0 TO 640 STEP 10
LINE (alkux, 480)-(oikx, 330), 12
NEXT
NEXT
FOR mol = 160 TO 290 STEP 130
FOR alky = mol TO mol + 30 STEP 2
LINE (0, alky)-(640, alky), 4
NEXT
NEXT
ELSE
CALL kuva
END IF
IF duke = 0 THEN
DO: LOOP WHILE INKEY$ = ""
END IF
PALETTE
ev = 4
GOSUB Teetausta
COLOR 11
LOCATE 2, 5: PRINT "∞±≤€≤±∞  The Story...  ∞±≤€≤±∞"
COLOR 3
LOCATE 4, 5
PRINT "Once upon a time there was a small kingdom of Slussen."
LOCATE 5, 5
PRINT "But oh shit, evil powers made the people of Slussen rude fighters."
LOCATE 6, 5
PRINT "All the inhabitants of Slussen City started to fight to solve who was"
LOCATE 7, 5
PRINT "the strongest one.  (A stupid story but better than nothing!)"
LOCATE 9, 5
PRINT "Now, two players can kill each other in this cooool game."
LOCATE 11, 5
PRINT "The screen will be covered with "; : COLOR 12: PRINT "BLOOD!"
COLOR 15
LOCATE 16, 5: PRINT "Hit Any Key!"
DO: LOOP WHILE INKEY$ = ""
CLS
ev = 1
GOSUB Teetausta
COLOR 11: LOCATE 2, 5: PRINT "∞±≤€≤±∞  Choose your Fighter, Player One!!!  ∞±≤€≤±∞"
GOSUB Naytavaihtoehdot
DO
a$ = INKEY$
IF UCASE$(a$) = "S" AND mer1 = 0 THEN mer1 = 1
IF UCASE$(a$) = "H" AND mer1 = 1 THEN fla1 = 1
IF UCASE$(a$) = "Z" AND zmer1 = 0 THEN zmer1 = 1
IF UCASE$(a$) = "R" AND zmer1 = 1 THEN zfla1 = 1
LOOP WHILE VAL(a$) < 1 AND UCASE$(a$) <> "A" AND UCASE$(a$) <> "B" AND UCASE$(a$) <> "C" AND fla1 = 0 AND zfla1 = 0
a$ = UCASE$(a$)
PLAY "mfo4l64ceg>c"
SELECT CASE a$
CASE "1"
p1$ = "SmileHead"
CASE "2"
p1$ = "Willex"
CASE "3"
p1$ = "Naigon"
CASE "4"
p1$ = "Stefan"
CASE "5"
p1$ = "Vilhelmo"
CASE "6"
p1$ = "Snailman"
CASE "7"
p1$ = "Murg"
CASE "8"
p1$ = "Paeblo the Old"
CASE "9"
p1$ = "Paagah"
CASE "A"
p1$ = "Larry the Glove"
CASE "B"
p1$ = "SiliconeAss"
CASE "C"
p1$ = "TessaquÇ"
END SELECT
IF fla1 = 1 THEN p1$ = "Slussen HyperSpirit Invisible"
IF zfla1 = 1 THEN p1$ = "Zairar"
restop1$ = p1$
CLS
ev = 8
GOSUB Teetausta
COLOR 11: LOCATE 2, 5: PRINT "∞±≤€≤±∞  Choose your Fighter, Player Two!!!  ∞±≤€≤±∞"
GOSUB Naytavaihtoehdot
DO
a$ = INKEY$
IF UCASE$(a$) = "S" AND mer2 = 0 THEN mer2 = 1
IF UCASE$(a$) = "H" AND mer2 = 1 THEN fla2 = 1
IF UCASE$(a$) = "Z" AND zmer2 = 0 THEN zmer2 = 1
IF UCASE$(a$) = "R" AND zmer2 = 1 THEN zfla2 = 1
LOOP WHILE VAL(a$) < 1 AND UCASE$(a$) <> "A" AND UCASE$(a$) <> "B" AND UCASE$(a$) <> "C" AND fla2 = 0 AND zfla2 = 0
a$ = UCASE$(a$)
PLAY "mfo4l64ceg>c"
SELECT CASE a$
CASE "1"
p2$ = "SmileHead"
CASE "2"
p2$ = "Willex"
CASE "3"
p2$ = "Naigon"
CASE "4"
p2$ = "Stefan"
CASE "5"
p2$ = "Vilhelmo"
CASE "6"
p2$ = "Snailman"
CASE "7"
p2$ = "Murg"
CASE "8"
p2$ = "Paeblo the Old"
CASE "9"
p2$ = "Paagah"
CASE "A"
p2$ = "Larry the Glove"
CASE "B"
p2$ = "SiliconeAss"
CASE "C"
p2$ = "TessaquÇ"
END SELECT
IF fla2 = 1 THEN p2$ = "Slussen HyperSpirit Invisible"
IF zfla2 = 1 THEN p2$ = "Zairar"
p1$ = restop1$
CLS
printattavaaika = 110
ev = 6
GOSUB Teetausta
LOCATE 2, 5: COLOR 11: PRINT "∞±≤€≤±∞  How to Play...  ∞±≤€≤±∞"
COLOR 3
LOCATE 4, 5: PRINT "Player one: left=A, right=D, jump=W"
LOCATE 5, 5: PRINT "Plyer two: use the arrow keys to move"
LOCATE 6, 5: PRINT "Every fighter has a kick, a hit, two specials and one finishing move."
LOCATE 8, 5: PRINT "Player one uses F1, F2, F3 and F4 to fight."
LOCATE 9, 5: PRINT "Player two uses F9, F10, F11 and F12 to fight."
LOCATE 11, 5: COLOR 3: PRINT "Player 1: "; : COLOR 14: PRINT p1$
LOCATE 12, 5: COLOR 3: PRINT "Player 2: "; : COLOR 14: PRINT p2$
COLOR 3
LOCATE 15, 5: PRINT "NOTE! Sometimes making a supergood move can affect your OWN energy!"
LOCATE 17, 5: PRINT "Esc quits. Hit P when you want to pause the game."
COLOR 2
LOCATE 19, 5: PRINT "CHEATS:"
LOCATE 20, 5: PRINT "Full energy for player 1: Alt+1"
LOCATE 21, 5: PRINT "Full energy for player 2: Alt+2"
LOCATE 22, 5: PRINT "Secret fighter 1:  S H  at the 'choose your fighter' screen"
LOCATE 23, 5: PRINT "Secret fighter 2:  Z R  at the 'choose your fighter' screen"
LOCATE 27, 5: COLOR 15: PRINT "Hit Any Key!"
COLOR 3
DO: LOOP WHILE INKEY$ = ""
nayt = 0
p1x = 100
p1y = 300
p2x = 540
p2y = 300
p1energia = 400
p2energia = 400
time = FIX(TIMER)
PLAY "o0l16ce-gl4f+>>>l32f+f+f+"
CLS : CALL teekaikki
IF p1$ = "Snailman" THEN snak1 = 30
IF p2$ = "Snailman" THEN snak2 = 30
p2suunta$ = "-"
p1suunta$ = ""

DO

olenk = p1energia + p2energia

IF POINT(100, 100) <> 0 AND POINT(540, 100) <> 0 THEN CLS : CALL teekaikki

IF p1hyppaa = 1 THEN
IF p1y > 210 AND p1laskee = 0 THEN CALL poista1pelaaja: p1y = p1y - 10: CALL tee1pelaaja
IF p1laskee = 1 THEN CALL poista1pelaaja: p1y = p1y + 10: CALL tee1pelaaja
IF p1y <= 220 THEN CALL poista1pelaaja: p1y = p1y + 10: p1laskee = 1: CALL tee1pelaaja
IF p1y >= 280 AND p1laskee = 1 THEN
CALL poista1pelaaja: p1y = 300: p1hyppaa = 0: p1laskee = 0
p1vauhti = 0: CALL tee1pelaaja: CALL tee2pelaaja
IF p1$ = "Snailman" AND p1suojaa = 1 THEN tippu1 = 1
IF p1$ = "Snailman" THEN
CIRCLE (p1x, p1y + 70), 60, 0, , , .5
CIRCLE (p1x - 14, p1y + 50), 10, 0
CIRCLE (p1x + 14, p1y + 50), 10, 0
FOR silr = 1 TO 3
CIRCLE (p1x - 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 0
CIRCLE (p1x + 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 0
NEXT
END IF
END IF
IF p2hyppaa = 0 THEN
IF LEFT$(p1$, 3) = "Sna" THEN
snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .02
ELSEIF LEFT$(p1$, 3) = "Vil" OR LEFT$(p1$, 3) = "Ste" OR LEFT$(p1$, 3) = "Mur" THEN
snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .005
ELSE
snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .01
END IF
IF p2suojaa = 0 THEN CALL tee2pelaaja
END IF
IF ABS(p1x - p2x) < 80 AND p2hyppaa = 0 AND p1hyppaa = 1 THEN CALL poista2pelaaja: CALL tee2pelaaja
END IF
IF p2hyppaa = 1 THEN
IF p2y > 210 AND p2laskee = 0 THEN CALL poista2pelaaja: p2y = p2y - 10: CALL tee2pelaaja
IF p2laskee = 1 THEN CALL poista2pelaaja: p2y = p2y + 10: CALL tee2pelaaja
IF p2y <= 220 THEN CALL poista2pelaaja: p2y = p2y + 10: p2laskee = 1: CALL tee2pelaaja
IF p2y >= 280 AND p2laskee = 1 THEN CALL poista2pelaaja: p2y = 300: p2hyppaa = 0: p2laskee = 0: p2vauhti = 0: CALL tee2pelaaja: CALL tee1pelaaja
IF p1hyppaa = 0 THEN
IF LEFT$(p2$, 3) = "Sna" THEN
snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .04
ELSEIF LEFT$(p2$, 3) = "Vil" OR LEFT$(p2$, 3) = "Ste" OR LEFT$(p2$, 3) = "Mur" THEN
snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .005
ELSE
snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .01
END IF
IF p1suojaa = 0 THEN CALL tee1pelaaja
ELSE
IF INT(RND * 2) = 1 THEN snaika = TIMER: DO: LOOP WHILE ABS(TIMER - snaika) < .01
END IF
END IF

IF tippu1 = 1 THEN
CIRCLE (p1x, p1y + 70), 60, 14, , , .5
CIRCLE (p1x - 14, p1y + 50), 10, 15
CIRCLE (p1x + 14, p1y + 50), 10, 15
FOR silr = 1 TO 3
CIRCLE (p1x - 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 3
CIRCLE (p1x + 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 3
NEXT
tippu1 = 0
END IF

IF p1notjodlmaix = 1 AND TIMER - naika1 >= .1 THEN
LINE (p1njmxx, 350)-(p1njmxx + 40, 400), 0, BF
IF p1njmxx >= 640 THEN p1njmxx = 0: p1notjodlmaix = 0: GOTO poismaix1
IF ABS(p2x - (p1njmxx + 20)) < 30 AND p2y >= 250 AND p2suojaa = 0 THEN
p2energia = p2energia - 15 - INT(RND * 6)
CALL teeenergiat
p1notjodlmaix = 0: GOTO poismaix1
END IF
p1njmxx = p1njmxx + 10
LINE (p1njmxx, 350)-(p1njmxx + 40, 400), 13, BF
naika1 = TIMER
poismaix1:
END IF

IF p2notjodlmaix = 1 AND TIMER - naika2 >= .1 THEN
LINE (p2njmxx, 350)-(p2njmxx + 40, 400), 0, BF
IF ABS(p2x - p2njmxx) < 70 THEN CALL tee2pelaaja
IF p2njmxx <= 0 THEN p2njmxx = 0: p2notjodlmaix = 0: GOTO poismaix2
IF ABS(p1x - (p2njmxx + 20)) < 30 AND p1y >= 250 AND p1suojaa = 0 THEN
p1energia = p1energia - 15 - INT(RND * 6)
CALL teeenergiat
p2notjodlmaix = 0: GOTO poismaix2
END IF
p2njmxx = p2njmxx - 10
LINE (p2njmxx, 350)-(p2njmxx + 40, 400), 13, BF
naika2 = TIMER
poismaix2:
END IF

IF p1kuiluu = 1 THEN
IF ABS(p2x - (p1kuilux + 40)) < 35 AND p2y >= 300 THEN
CALL poista2pelaaja
putii = 1
p2x = p1kuilux + 40
FOR p2y = 300 TO 400 STEP 10
CALL tee2pelaaja
SOUND 800 - p2y, .4
CALL poista2pelaaja
NEXT
CALL poista2pelaaja
putii = 0
p1kuiluu = 0: CALL teekentta
IF ABS(p2x - p1x) < 60 THEN p2x = p2x + 30: CALL tee1pelaaja
p2y = 300: CALL tee2pelaaja
p2energia = p2energia - 40: CALL teeenergiat
END IF
IF ABS(p1x - (p1kuilux + 40)) < 35 AND p1y >= 300 THEN
CALL poista1pelaaja
putii = 1
p1x = p1kuilux + 40
FOR p1y = 300 TO 400 STEP 10
CALL tee1pelaaja
SOUND 800 - p1y, .4
CALL poista1pelaaja
NEXT
CALL poista1pelaaja
putii = 0
p1kuiluu = 0: CALL teekentta
IF ABS(p2x - p1x) < 60 THEN p1x = p1x - 30: CALL tee2pelaaja
p1y = 300: CALL tee1pelaaja
p1energia = p1energia - 40: CALL teeenergiat
END IF
END IF
IF p2kuiluu = 1 THEN
IF ABS(p1x - (p2kuilux + 40)) < 35 AND p1y >= 300 THEN
CALL poista1pelaaja
putii = 1
p1x = p2kuilux + 40
FOR p1y = 300 TO 400 STEP 10
CALL tee1pelaaja
SOUND 800 - p1y, .4
CALL poista1pelaaja
NEXT
CALL poista1pelaaja
putii = 0
p2kuiluu = 0: CALL teekentta
IF ABS(p2x - p1x) < 60 THEN p1x = p1x - 30: CALL tee2pelaaja
p1y = 300: CALL tee1pelaaja
p1energia = p1energia - 40: CALL teeenergiat
END IF
IF ABS(p2x - (p2kuilux + 40)) < 35 AND p2y >= 300 THEN
CALL poista2pelaaja
putii = 1
p2x = p2kuilux + 40
FOR p2y = 300 TO 400 STEP 10
CALL tee2pelaaja
SOUND 800 - p2y, .4
CALL poista2pelaaja
NEXT
CALL poista2pelaaja
putii = 0
p2kuiluu = 0: CALL teekentta
IF ABS(p2x - p1x) < 60 THEN p2x = p2x + 30: CALL tee1pelaaja
p2y = 300: CALL tee2pelaaja
p2energia = p2energia - 40: CALL teeenergiat
END IF
END IF

IF p1kuiluu = 1 AND ABS(TIMER - kuika1) > 10 THEN p1kuiluu = 0: CALL teekentta
IF p2kuiluu = 1 AND ABS(TIMER - kuika2) > 10 THEN p2kuiluu = 0: CALL teekentta

IF p1jahm = 1 THEN LINE (p1x - 35, p1y - 35)-(p1x + 35, p1y + 85), 15, B
IF p2jahm = 1 THEN LINE (p2x - 35, p2y - 35)-(p2x + 35, p2y + 85), 15, B
IF p2jahm = 1 AND ABS(TIMER - p2jaika) >= 3 THEN
SOUND 500, .3
LINE (p2x - 35, p2y - 35)-(p2x + 35, p2y + 85), 0, B
p2jahm = 0
END IF
IF p1jahm = 1 AND ABS(TIMER - p1jaika) >= 3 THEN
SOUND 500, .3
LINE (p1x - 35, p1y - 35)-(p1x + 35, p1y + 85), 0, B
p1jahm = 0
END IF

IF p2pysii = 1 THEN
CIRCLE (p2silix, p2sily), 10, 12 + INT(RND * 2)
CIRCLE (p2silix, p2sily), 9, 12 + INT(RND * 2)
IF ABS(TIMER - p2pyaika) >= 1.6 THEN
FOR pyakku = 1 TO 100
CIRCLE (p2silix, p2sily), pyakku, 14 + INT(RND * 2)
IF INT(RND * 4) = 1 THEN SOUND 200 + INT(RND * 100), .1
NEXT
FOR pyakku = 1 TO 100
CIRCLE (p2silix, p2sily), pyakku, 0
NEXT
CALL teekentta
IF ABS(p1x - p2silix) < 140 AND p1suojaa = 0 THEN
p1energia = p1energia - 18 - INT(RND * 6)
CALL teeenergiat
END IF
IF ABS(p2x - p2silix) < 140 THEN
p2energia = p2energia - 18 - INT(RND * 6)
CALL teeenergiat
END IF
CIRCLE (p2silix, p2sily), 10, 0
p2pysii = 0
CALL teepelaajat
END IF
END IF
IF p2silii = 1 THEN
CIRCLE (p2silix, p2sily), 10, 14 + INT(RND * 2)
IF ABS(TIMER - sjaika2) > .08 THEN
CIRCLE (p2silix, p2sily), 10, 0
sx2lisa = 20 - INT(RND * 30)
p2silix = p2silix - sx2lisa
CIRCLE (p2silix, p2sily), 10, 14 + INT(RND * 2)
sjaika2 = TIMER
END IF
IF ABS(p1x - p2silix) < 40 THEN p2pysii = 1: PLAY "o5l64ce-f+": p2silii = 0: p2pyaika = TIMER
IF p2silix <= 20 THEN p2silii = 0: CIRCLE (p2silix, p2sily), 10, 0: CALL teepelaajat
END IF
IF p1pysii = 1 THEN
CIRCLE (p1silix, p1sily), 10, 12 + INT(RND * 2)
CIRCLE (p1silix, p1sily), 9, 12 + INT(RND * 2)
IF ABS(TIMER - p1pyaika) >= 1.6 THEN
FOR pyakku = 1 TO 100
CIRCLE (p1silix, p1sily), pyakku, 14 + INT(RND * 2)
IF INT(RND * 4) = 1 THEN SOUND 200 + INT(RND * 100), .1
NEXT
FOR pyakku = 1 TO 100
CIRCLE (p1silix, p1sily), pyakku, 0
NEXT
CALL teekentta
IF ABS(p2x - p1silix) < 140 AND p2suojaa = 0 THEN
p2energia = p2energia - 18 - INT(RND * 6)
CALL teeenergiat
END IF
IF ABS(p1x - p1silix) < 140 THEN
p1energia = p1energia - 18 - INT(RND * 6)
CALL teeenergiat
END IF
CIRCLE (p1silix, p1sily), 10, 0
p1pysii = 0
CALL teepelaajat
END IF
END IF
IF p1silii = 1 THEN
CIRCLE (p1silix, p1sily), 10, 14 + INT(RND * 2)
IF ABS(TIMER - sjaika1) > .08 THEN
CIRCLE (p1silix, p1sily), 10, 0
sx1lisa = 20 - INT(RND * 30)
p1silix = p1silix + sx1lisa
CIRCLE (p1silix, p1sily), 10, 14 + INT(RND * 2)
sjaika1 = TIMER
END IF
IF ABS(p2x - p1silix) < 40 THEN p1pysii = 1: PLAY "o5l64ce-f+": p1silii = 0: p1pyaika = TIMER
IF p1silix >= 620 THEN p1silii = 0: CIRCLE (p1silix, p1sily), 10, 0: CALL teepelaajat
END IF

IF p1laavaa = 1 THEN
IF ABS(TIMER - raika1) > .2 THEN
'heliumi Hanska tm
PSET (p1laax, p1laay), 0
DRAW "c0d3gd3gd3gd3gd3gd3gd3r25u12r10u4l10u4r12u4h4l26r8bd5c0r5d5l5u5br4c0bd2dlu"
DRAW "bd12br4d5u3l3d3u5"
p1laax = p1laax + 10
p1laay = p1laay + 5
'heliumi Hanska tm
PSET (p1laax, p1laay), 0
DRAW "c10d3gd3gd3gd3gd3gd3gd3r25u12r10u4l10u4r12u4h4l26r8bd5c7r5d5l5u5br4c15bd2dlu"
DRAW "bd12br4d5u3l3d3u5"
raika1 = TIMER
END IF
IF p1laax >= 620 OR p1laay > 360 OR (p2y - 55 - snak2 < p1laay AND ABS(p2x - p1laax) < 55 + (snak2 / 2)) THEN
PSET (p1laax, p1laay), 0
DRAW "c0d3gd3gd3gd3gd3gd3gd3r25u12r10u4l10u4r12u4h4l26r8bd5c0r5d5l5u5br4c0bd2dlu"
DRAW "bd12br4d5u3l3d3u5"
p1laavaa = 0
END IF
IF p2y - 55 - snak2 < p1laay AND ABS(p2x - p1laax) < 55 + (snak2 / 2) THEN
FOR lapaur = 1 TO 20
CIRCLE (p1laax + 15, p1laay + 15), lapaur, 10 + (4 * INT(RND * 2))
SOUND 500 + INT(RND * 200), .1
NEXT
FOR lapaur = 1 TO 20
CIRCLE (p1laax + 15, p1laay + 15), lapaur, 0
NEXT
IF p2suojaa = 0 THEN
p2energia = p2energia - 35 - INT(RND * 15)
CALL teeenergiat
END IF
END IF
END IF

IF p2laavaa = 1 THEN
IF ABS(TIMER - raika2) > .2 THEN
PSET (p2laax, p2laay), 0
DRAW "c0d3fd3fd3fd3fd3fd3fd3l25u12l10u4r10u4l12u4e4r26l8bd5l5d5r5u5bl4bd2dru"
DRAW "bd12bl4d5u3r3d3u5"
p2laax = p2laax - 10
p2laay = p2laay + 5
PSET (p2laax, p2laay), 0
DRAW "c10d3fd3fd3fd3fd3fd3fd3l25u12l10u4r10u4l12u4e4r26l8bd5c7l5d5r5u5bl4c15bd2dru"
DRAW "bd12bl4d5u3r3d3u5"
raika2 = TIMER
END IF
IF p2laax <= 20 OR p2laay > 360 OR (p1y - 55 - snak1 < p2laay AND ABS(p1x - p2laax) < 55 + (snak1 / 2)) THEN
PSET (p2laax, p2laay), 0
DRAW "c0d3fd3fd3fd3fd3fd3fd3l25u12l10u4r10u4l12u4e4r26l8bd5l5d5r5u5bl4bd2dru"
DRAW "bd12bl4d5u3r3d3u5"
p2laavaa = 0
END IF
IF p1y - 55 - snak1 < p2laay AND ABS(p1x - p2laax) < 55 + (snak1 / 2) THEN
FOR lapaur = 1 TO 20
CIRCLE (p2laax + 15, p2laay + 15), lapaur, 10 + (4 * INT(RND * 2))
SOUND 500 + INT(RND * 200), .1
NEXT
FOR lapaur = 1 TO 20
CIRCLE (p2laax + 15, p2laay + 15), lapaur, 0
NEXT
IF p1suojaa = 0 THEN
p1energia = p1energia - 35 - INT(RND * 15)
CALL teeenergiat
END IF
END IF
END IF

IF p1viher = 1 THEN
IF ABS(TIMER - vaika) > .1 THEN
vaika = TIMER
PALETTE 0, palari1 * 256
palari1 = palari1 - 1
END IF
IF palari1 <= 5 THEN PALETTE 0, 0: p1viher = 0
END IF
IF p2viher = 1 THEN
IF ABS(TIMER - vaika2) > .1 THEN
vaika2 = TIMER
PALETTE 0, palari2 * 256
palari2 = palari2 - 1
END IF
IF palari2 <= 5 THEN PALETTE 0, 0: p2viher = 0
END IF

IF p2sahkoo = 1 THEN
IF ABS(TIMER - saika2) > 2 THEN
IF p2x > 90 THEN
FOR sx = sahx2 TO 20 STEP -20
PSET (sx, 395), 15
DRAW "c15l4h2l3g4h3gl3"
SOUND 800 + INT(RND * 100), .1
NEXT
FOR sx = sahx2 TO 20 STEP -20
PSET (sx, 395), 0
DRAW "c0l4h2l3g4h3gl3"
NEXT
END IF
FOR sahr = 1 TO 5
CIRCLE (sahx2, 395), sahr, 0
NEXT
p2sahkoo = 0
IF p1y >= 285 AND p1x < sahx2 + 30 THEN p1energia = p1energia - 30 - INT(RND * 10): CALL teeenergiat
IF p2y >= 285 AND p2x < sahx2 + 30 THEN p2energia = p2energia - 30 - INT(RND * 10): CALL teeenergiat
END IF
END IF
IF p1sahkoo = 1 THEN
IF ABS(TIMER - saika1) > 2 THEN
IF p1x < 550 THEN
FOR sx = sahx1 TO 640 STEP 20
PSET (sx, 395), 15
DRAW "c15r4e2r3f4e3fr3"
SOUND 800 + INT(RND * 100), .1
NEXT
FOR sx = sahx1 TO 640 STEP 20
PSET (sx, 395), 0
DRAW "c0r4e2r3f4e3fr3"
NEXT
END IF
FOR sahr = 1 TO 5
CIRCLE (sahx1, 395), sahr, 0
NEXT
p1sahkoo = 0
IF p2y >= 285 AND p2x > sahx1 - 30 THEN p2energia = p2energia - 30 - INT(RND * 10): CALL teeenergiat
IF p1y >= 285 AND p1x > sahx1 - 30 THEN p1energia = p1energia - 30 - INT(RND * 10): CALL teeenergiat
END IF
END IF
IF p2pallottaa = 1 THEN
IF ABS(TIMER - laika2) > .1 THEN
laika2 = TIMER
FOR palr = 1 TO 7
CIRCLE (p2pallox, p2palloy), palr, 0
NEXT
p2palloy = p2palloy + 8
FOR palr = 1 TO 7
CIRCLE (p2pallox, p2palloy), palr, 2 + 8 * INT(RND * 2) + 5 * INT(RND * 2)
NEXT
END IF
IF p2palloy >= 270 THEN
FOR rajr = 1 TO 50
CIRCLE (p2pallox, p2palloy), rajr, 2 + 8 * INT(RND * 2) + 5 * INT(RND * 2)
SOUND 300 + INT(RND * 100), .1
NEXT
IF ABS(p2x - p2pallox) < 100 THEN
p2energia = p2energia - 30 - INT(RND * 10)
CALL teeenergiat
END IF
IF ABS(p1x - p2pallox) < 100 THEN
p1energia = p1energia - 30 - INT(RND * 10)
CALL teeenergiat
END IF
FOR rajr = 1 TO 50
CIRCLE (p2pallox, p2palloy), rajr, 0
NEXT
p2pallottaa = 0
CALL teepelaajat
END IF
END IF
IF p1pallottaa = 1 THEN
IF ABS(TIMER - laika) > .1 THEN
laika = TIMER
FOR palr = 1 TO 7
CIRCLE (p1pallox, p1palloy), palr, 0
NEXT
p1palloy = p1palloy + 8
FOR palr = 1 TO 7
CIRCLE (p1pallox, p1palloy), palr, 2 + 8 * INT(RND * 2) + 5 * INT(RND * 2)
NEXT
END IF
IF p1palloy >= 270 THEN
FOR rajr = 1 TO 50
CIRCLE (p1pallox, p1palloy), rajr, 2 + 8 * INT(RND * 2) + 5 * INT(RND * 2)
SOUND 300 + INT(RND * 100), .1
NEXT
IF ABS(p2x - p1pallox) < 100 THEN
p2energia = p2energia - 30 - INT(RND * 10)
CALL teeenergiat
END IF
IF ABS(p1x - p1pallox) < 100 THEN
p1energia = p1energia - 30 - INT(RND * 10)
CALL teeenergiat
END IF
FOR rajr = 1 TO 50
CIRCLE (p1pallox, p1palloy), rajr, 0
NEXT
p1pallottaa = 0
CALL teepelaajat
END IF
END IF
IF p1pieree = 1 THEN
IF ABS(TIMER - paika) > .1 THEN
paika = TIMER
FOR p1pierr = 2 TO 5
CIRCLE (p1pierx, p1piery), p1pierr, 0
NEXT
p1pierx = p1pierx + 10
FOR p1pierr = 2 TO 5
CIRCLE (p1pierx, p1piery), p1pierr, 6
NEXT
IF p1pierx >= 620 THEN LINE (p1pierx - 10, p1piery - 10)-(p1pierx + 10, p1piery + 10), 0, BF: p1pieree = 0
END IF
IF ABS(p2x - p1pierx) < 20 + snak2 AND p2y + 100 > p1piery AND p2suojaa = 0 THEN
p2energia = p2energia - 12 - INT(RND * 4)
CALL teeenergiat
LINE (p1pierx - 10, p1piery - 10)-(p1pierx + 10, p1piery + 10), 0, BF
p1pieree = 0
END IF
END IF
IF p2pieree = 1 THEN
IF ABS(TIMER - paika2) > .1 THEN
paika2 = TIMER
FOR p2pierr = 2 TO 5
CIRCLE (p2pierx, p2piery), p2pierr, 0
NEXT
p2pierx = p2pierx - 10
FOR p2pierr = 2 TO 5
CIRCLE (p2pierx, p2piery), p2pierr, 6
NEXT
IF p2pierx <= 20 THEN LINE (p2pierx - 10, p2piery - 10)-(p2pierx + 10, p2piery + 10), 0, BF: p2pieree = 0
END IF
IF ABS(p1x - p2pierx) < 20 + snak1 AND p1y + 100 > p2piery AND p1suojaa = 0 THEN
p1energia = p1energia - 12 - INT(RND * 4)
CALL teeenergiat
LINE (p2pierx - 10, p2piery - 10)-(p2pierx + 10, p2piery + 10), 0, BF
p2pieree = 0
END IF
END IF
IF p1tulee = 1 THEN
IF ABS(TIMER - taika) > .1 THEN
taika = TIMER
FOR p1tulir = 2 TO 5
CIRCLE (p1tulix, p1tuliy), p1tulir, 0
NEXT
p1tulix = p1tulix + 10
FOR p1tulir = 2 TO 5
CIRCLE (p1tulix, p1tuliy), p1tulir, 4 + (10 * INT(RND * 2))
NEXT
IF p1tulix >= 620 THEN LINE (p1tulix - 10, p1tuliy - 10)-(p1tulix + 10, p1tuliy + 10), 0, BF: p1tulee = 0
END IF
IF ABS(p2x - p1tulix) < 20 + snak2 AND p2y + 100 > p1tuliy AND p2suojaa = 0 THEN
p2energia = p2energia - 10 - INT(RND * 4)
CALL teeenergiat
LINE (p1tulix - 10, p1tuliy - 10)-(p1tulix + 10, p1tuliy + 10), 0, BF
p1tulee = 0
END IF
END IF
IF p2tulee = 1 THEN
IF ABS(TIMER - taika2) > .1 THEN
taika2 = TIMER
FOR p2tulir = 2 TO 5
CIRCLE (p2tulix, p2tuliy), p2tulir, 0
NEXT
p2tulix = p2tulix - 10
FOR p2tulir = 2 TO 5
CIRCLE (p2tulix, p2tuliy), p2tulir, 4 + (10 * INT(RND * 2))
NEXT
IF p2tulix <= 20 THEN LINE (p2tulix - 10, p2tuliy - 10)-(p2tulix + 10, p2tuliy + 10), 0, BF: p2tulee = 0
END IF
IF ABS(p1x - p2tulix) < 20 + snak1 AND p1y + 100 > p2tuliy AND p1suojaa = 0 THEN
p1energia = p1energia - 10 - INT(RND * 4)
CALL teeenergiat
LINE (p2tulix - 10, p2tuliy - 10)-(p2tulix + 10, p2tuliy + 10), 0, BF
p2tulee = 0
END IF
END IF

IF FIX(TIMER) <> time THEN time = FIX(TIMER): printattavaaika = printattavaaika - 1: CALL teeaika
IF printattavaaika <= 10 THEN PALETTE: GOTO aikaloppu
a$ = UCASE$(INKEY$)
IF a$ = CHR$(27) THEN
PLAY "o4l16eg": LOCATE 10, 29: COLOR 12: PRINT "Quit the Game? Y=Yes, N=No"
DO: B$ = INKEY$: LOOP WHILE UCASE$(B$) <> "Y" AND UCASE$(B$) <> "N"
IF UCASE$(B$) = "Y" THEN CLS : PLAY "o2l32ge-c<ge-c": END
LOCATE 10, 28: PRINT "                             "
END IF

IF p1suojaa = 0 AND p1jahm = 0 THEN
IF a$ = "A" AND p1suojaa = 0 AND p1x > 20 THEN CALL poista1pelaaja: p1x = p1x - 5 - p1vauhti: CALL tee1pelaaja
IF a$ = "D" AND p1suojaa = 0 AND p1x < p2x - 70 THEN CALL poista1pelaaja: p1x = p1x + 5 + p1vauhti: CALL tee1pelaaja
IF a$ = "W" AND p1leijuu = 0 AND p1suojaa = 0 AND p1hyppaa = 0 THEN p1hyppaa = 1: p1vauhti = 10
END IF
IF p2suojaa = 0 AND p2jahm = 0 THEN
IF a$ = vasen$ AND p2suojaa = 0 AND p2x > p1x + 70 THEN CALL poista2pelaaja: p2x = p2x - 5 - p2vauhti: CALL tee2pelaaja
IF a$ = oikea$ AND p2suojaa = 0 AND p2x < 620 THEN CALL poista2pelaaja: p2x = p2x + 5 + p2vauhti: CALL tee2pelaaja
IF a$ = ylos$ AND p2leijuu = 0 AND p2suojaa = 0 AND p2hyppaa = 0 THEN p2hyppaa = 1: p2vauhti = 10
END IF
IF UCASE$(a$) = "P" THEN
PLAY "o4l16eg"
LOCATE 10, 31: COLOR 12: PRINT "    GAME PAUSED!    "
DO: LOOP WHILE INKEY$ = ""
LOCATE 10, 31: PRINT "                    "
PLAY "o4l16ge"
END IF
IF p1jahm = 0 THEN
IF a$ = f1n$ THEN CALL f1
IF a$ = f2n$ THEN CALL f2
IF a$ = f3n$ THEN CALL f3
IF a$ = f4n$ THEN CALL f4
END IF
IF p2jahm = 0 THEN
IF a$ = f9n$ THEN CALL f9
IF a$ = f10n$ THEN CALL f10
IF a$ = f11n$ THEN CALL f11
IF a$ = f12n$ THEN CALL f12
END IF
IF loppaa <> 0 THEN CLEAR : GOTO Alku

IF LCASE$(a$) = CHR$(0) + "x" THEN p1energia = 400: CALL teeenergiat
IF LCASE$(a$) = CHR$(0) + "y" THEN p2energia = 400: CALL teeenergiat

IF a$ <> "" AND p1sahkoo = 1 THEN
FOR sahr = 1 TO 5
CIRCLE (sahx1, 395), sahr, 11 + (4 * INT(RND * 2))
NEXT
END IF
IF a$ <> "" AND p2sahkoo = 1 THEN
FOR sahr = 1 TO 5
CIRCLE (sahx2, 395), sahr, 11 + (4 * INT(RND * 2))
NEXT
END IF
IF a$ <> "" AND p1tulee = 1 THEN
FOR p1tulir = 2 TO 5
CIRCLE (p1tulix, p1tuliy), p1tulir, 4 + (10 * INT(RND * 2))
NEXT
END IF
IF a$ <> "" AND p2tulee = 1 THEN
FOR p2tulir = 2 TO 5
CIRCLE (p2tulix, p2tuliy), p2tulir, 4 + (10 * INT(RND * 2))
NEXT
END IF
IF p1suojaa = 1 AND p1$ = "Snailman" AND a$ <> "" THEN
CIRCLE (p1x, p1y + 70), 60, 14, , , .5
CIRCLE (p1x - 14, p1y + 50), 10, 15
CIRCLE (p1x + 14, p1y + 50), 10, 15
FOR silr = 1 TO 3
CIRCLE (p1x - 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 3
CIRCLE (p1x + 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 3
NEXT
END IF
IF p2suojaa = 1 AND p2$ = "Snailman" AND a$ <> "" THEN
CIRCLE (p2x, p2y + 70), 60, 14, , , .5
CIRCLE (p2x - 14, p2y + 50), 10, 15
CIRCLE (p2x + 14, p2y + 50), 10, 15
FOR silr = 1 TO 3
CIRCLE (p2x - 14 + VAL(p2suunta$ + STR$(5)), p2y + 50), silr, 3
CIRCLE (p2x + 14 + VAL(p2suunta$ + STR$(5)), p2y + 50), silr, 3
NEXT
END IF
IF p1notjodlmaix = 1 AND a$ <> "" THEN
LINE (p1njmxx, 350)-(p1njmxx + 40, 400), 13, BF
END IF
IF p2notjodlmaix = 1 AND a$ <> "" THEN
LINE (p2njmxx, 350)-(p2njmxx + 40, 400), 13, BF
END IF
IF p1laavaa = 1 AND (a$ <> "" OR p1hyppaa = 1 OR p2hyppaa = 1) THEN
PSET (p1laax, p1laay), 0
DRAW "c10d3gd3gd3gd3gd3gd3gd3r25u12r10u4l10u4r12u4h4l26r8bd5c7r5d5l5u5br4c15bd2dlu"
DRAW "bd12br4d5u3l3d3u5"
END IF
IF p2laavaa = 1 AND (a$ <> "" OR p1hyppaa = 1 OR p2hyppaa = 1) THEN
PSET (p2laax, p2laay), 0
DRAW "c10d3fd3fd3fd3fd3fd3fd3l25u12l10u4r10u4l12u4e4r26l8bd5c7l5d5r5u5bl4c15bd2dru"
DRAW "bd12bl4d5u3r3d3u5"
END IF
IF p1suojaa = 1 AND p1$ = "Larry the Glove" AND a$ <> "" THEN
PSET (p1x - 35, p1y + 99), 0
DRAW "c11nr70m" + STR$(p1x) + "," + STR$(p1y - 20) + "m" + STR$(p1x + 35) + "," + STR$(p1y + 99)
END IF
IF p2suojaa = 1 AND p2$ = "Larry the Glove" AND a$ <> "" THEN
PSET (p2x - 35, p2y + 99), 0
DRAW "c11nr70m" + STR$(p2x) + "," + STR$(p2y - 20) + "m" + STR$(p2x + 35) + "," + STR$(p2y + 99)
END IF

IF p1energia + p2energia <> olenk OR (a$ <> "" AND ABS(p2x - p1x) <= 80) THEN CALL tee1pelaaja: CALL tee2pelaaja
IF loppaa = 1 THEN CLEAR : GOTO Alku
a$ = ""

LOOP

END 'useless???
Teetausta:
CLS
FOR erasux = 0 TO 640 STEP 70
FOR erasuy = 0 TO 520 STEP 70
CIRCLE (erasux, erasuy), 30, ev
DRAW "c" + STR$(ev) + "bd39bl28bu30r57l10nd14l10nd20l10nd21l10nd19l10nd12 "
silmalku = erasux
IF eusuunta$ = "" THEN
silmalku = silmalku + VAL(eusuunta$ + STR$(1))
CIRCLE (silmalku, erasuy - 5), 8, ev
CIRCLE (silmalku + 20, erasuy - 5), 8, ev
CIRCLE (silmalku + 2, erasuy - 5), 2, ev
CIRCLE (silmalku + 22, erasuy - 5), 2, ev
PSET (erasux, erasuy - 30), ev
DRAW "c" + STR$(ev) + "r25"
DRAW "e4h4l5"
END IF
NEXT
NEXT
RETURN

Naytavaihtoehdot:
COLOR 3: LOCATE 4, 5: PRINT "Secret Fighters: "; : COLOR 11: PRINT "Slussen HyperSpirit Invisible "; : COLOR 3
PRINT "and cool "; : COLOR 11: PRINT "Zairar!"
FOR p1y = 120 TO 310 STEP 190
FOR p1x = 50 TO 650 STEP 105
READ p1$
CALL tee1pelaaja
COLOR 11
NEXT
NEXT
LOCATE 15, 2: PRINT "1. SmileHead"
LOCATE 15, 17: PRINT "2. Willex"
LOCATE 15, 29: PRINT "3. Naigon"
LOCATE 15, 42: PRINT "4. Stefan"
LOCATE 15, 54: PRINT "5. Vilhelmo"
LOCATE 15, 67: PRINT "6. Snailman"
LOCATE 27, 4: PRINT "7. Murg"
LOCATE 27, 16: PRINT "8. Paeblo"
LOCATE 28, 16: PRINT "   the Old"
LOCATE 27, 29: PRINT "9. Paagah"
LOCATE 27, 40: PRINT "A. Larry the"
LOCATE 28, 40: PRINT "   Glove"
LOCATE 27, 55: PRINT "B. Silicone-"
LOCATE 28, 55: PRINT "   Ass"
LOCATE 27, 69: PRINT "C. TessaquÇ"
DATA SmileHead,Willex,Naigon,Stefan,Vilhelmo,Snailman
DATA Murg,Paeblo the Old,Paagah,Larry the Glove,SiliconeAss,TessaquÇ
DATA SmileHead,Willex,Naigon,Stefan,Vilhelmo,Snailman
DATA Murg,Paeblo the Old,Paagah,Larry the Glove,SiliconeAss,TessaquÇ
RETURN

aikaloppu:
CLS
PAINT (100, 100), 9
FOR viivx2 = 0 TO 640 STEP 2
LINE (viivx2, 0)-(640 - viivx2, 480), 0
NEXT
FOR viivy2 = 0 TO 640 STEP 2
LINE (0, viivy2)-(640, 480 - viivy2), 0
NEXT
LOCATE 15, 37
COLOR 14
PRINT "TIME OUT"
LOCATE 19, 35
COLOR 15
PRINT "Hit Any Key!"
PLAY "mfo3l16cdegb-b-agcde-ec"
DO: LOOP WHILE INKEY$ = ""
CLS
CLEAR : GOTO Alku
'
'   {{{{{
'  / _ _ \
'  | ^ ^ |     /ÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓÓ\
'  |  C  |  --<  WHEW! I DID IT!!!! |
' >|     |<    \____________________/
'  | (0) |
'
'  the program is freeware. put my name somewhere if you use this code!
'  if you have a homepage or you know a good ftp site, you can freely
'  put this game there!

SUB eulop
CALL poistapelaajat
p1x = 100
p2x = 540
CALL teepelaajat
IF voitti = 1 THEN
PSET (p1x + 20, p1y + 25), 0
DRAW "c3r3er3er3nu2r50d3l40d2nd3l4dl3g2l3g2l3g2l4u10r3"
FOR pakat = 1 TO 30
randi = 5 + INT(RND * 10)
pay = 310 + INT(RND * 20)
FOR pax = 190 TO 540 STEP randi
CIRCLE (pax, pay), 3, 12 + (2 * INT(RND * 2))
IF INT(RND * 2) = 1 THEN SOUND 300 + INT(RND * 200), .1
NEXT
FOR pax = 190 TO 540 STEP randi
CIRCLE (pax, pay), 3, 0
NEXT
NEXT
ELSE
PSET (p2x - 20, p2y + 25), 0
DRAW "c3l3hl3hl3nu2l50d3r40d2nd3r4dr3f2r3f2r3f2r4u10l3"
FOR pakat = 1 TO 30
randi = 5 + INT(RND * 10)
pay = 310 + INT(RND * 20)
FOR pax = 460 TO 100 STEP -randi
CIRCLE (pax, pay), 3, 12 + (2 * INT(RND * 2))
IF INT(RND * 2) = 1 THEN SOUND 300 + INT(RND * 200), .1
NEXT
FOR pax = 460 TO 100 STEP -randi
CIRCLE (pax, pay), 3, 0
NEXT
NEXT
END IF
CALL paketa
fonnistu = 1
END SUB

SUB f1
IF p1$ <> "Snailman" AND p1$ <> "Larry the Glove" THEN
handlisa = 0
IF p1$ = "Stefan" OR p1$ = "Willex" OR p1$ = "SiliconeAss" THEN handlisa = 50
IF p1$ = "Paeblo the Old" OR p1$ = "Vilhelmo" THEN handlisa = 40
IF p1$ = "TessaquÇ" OR p1$ = "SmileHead" OR p1$ = "Zairar" THEN handlisa = 60
handvari = POINT(p1x + 10, p1y + 30)
CALL poista1pkasi
LINE (p1x + 10, p1y + 30)-(p1x + 70 + handlisa, p1y + 25), handvari
FOR s = 1 TO 7
SOUND 100 + INT(RND * 50), .1
NEXT
LINE (p1x + 10, p1y + 30)-(p1x + 70 + handlisa, p1y + 25), 0
LINE (p1x + 10, p1y + 30)-(p1x + 30, p1y + 30), handvari
IF p2x <= (p1x + 90 + handlisa + snak2) AND p1y + 30 < p2y + 130 AND p1y + 30 > p2y - 20 AND p2suojaa = 0 THEN
p2energia = p2energia - 4 - INT(RND * 3)
CALL teeenergiat
END IF
ELSEIF p1$ = "Snailman" AND p1suojaa = 0 THEN
PSET (p1x - 3, p1y + 28), 7
DRAW "c7bu42bl15 u15 br10d10 c0 br16u10br10d15"
CIRCLE (p1x + 14, p1y - 38), 10, 0
FOR silr = 1 TO 3
'CIRCLE (p1x - 14 + 5, p1y - 38), silr, 0 '2p varten vaiha +5 ---> -5
CIRCLE (p1x + 14 + 5, p1y - 38), silr, 0
NEXT
PSET (p1x + 40, p1y + 30), 14
DRAW "c7 r15bd10l14"
CIRCLE (p1x + 65, p1y + 37), 10, 15
FOR silr = 1 TO 3
'CIRCLE (p1x - 14 + 5, p1y - 38), silr, 0 '2p varten vaiha +5 ---> -5
CIRCLE (p1x + 65 + 5, p1y + 37), silr, 3
NEXT
CIRCLE (p1x - 14, p1y - 38), 10, 15
FOR s = 1 TO 4
SOUND 50 + INT(RND * 50), .1
NEXT
'seur. poistaa lisÑsilimÑn
PSET (p1x + 40, p1y + 30), 0
DRAW "c0 r15bd10l14"
CIRCLE (p1x + 65, p1y + 37), 10, 0
FOR silr = 1 TO 3
'CIRCLE (p1x - 14 + 5, p1y - 38), silr, 0 '2p varten vaiha +5 ---> -5
CIRCLE (p1x + 65 + 5, p1y + 37), silr, 0
NEXT
CALL poista1pelaaja
CALL tee1pelaaja
IF p2x <= (p1x + 85 + footlisa + snak2) AND p2suojaa = 0 THEN
p2energia = p2energia - 6 - INT(RND * 5)
CALL teeenergiat
END IF



ELSEIF p1$ = "Larry the Glove" AND p1suojaa = 0 THEN
LINE (p1x + VAL(p1suunta$ + STR$(25)), p1y - 10)-(p1x - VAL(p1suunta$ + STR$(10)), p1y), 0
LINE (p1x - VAL(p1suunta$ + STR$(10)), p1y)-(p1x + VAL(p1suunta$ + STR$(25)), p1y + 10), 0
LINE (p1x + VAL(p1suunta$ + STR$(25)), p1y - 10)-(p1x + VAL(p1suunta$ + STR$(80)), p1y), 11
LINE (p1x + VAL(p1suunta$ + STR$(80)), p1y)-(p1x + VAL(p1suunta$ + STR$(25)), p1y + 10), 11
FOR s = 1 TO 7
SOUND 100 + INT(RND * 200), .1
NEXT
LINE (p1x + VAL(p1suunta$ + STR$(25)), p1y - 10)-(p1x + VAL(p1suunta$ + STR$(80)), p1y), 0
LINE (p1x + VAL(p1suunta$ + STR$(80)), p1y)-(p1x + VAL(p1suunta$ + STR$(25)), p1y + 10), 0
LINE (p1x + VAL(p1suunta$ + STR$(25)), p1y - 10)-(p1x - VAL(p1suunta$ + STR$(10)), p1y), 11
LINE (p1x - VAL(p1suunta$ + STR$(10)), p1y)-(p1x + VAL(p1suunta$ + STR$(25)), p1y + 10), 11
IF ABS(p2x - p1x) < 110 + INT(snak2 / 3) AND p2suojaa = 0 THEN
p2energia = p2energia - 3 - INT(RND * 6)
CALL teeenergiat
END IF
END IF
END SUB

SUB f10
IF p1y - p2y >= 60 AND p1$ <> "Snailman" AND LEFT$(p1$, 5) <> "Laavi" THEN
paalisa = 15
IF p1$ = "Murg" THEN paalisa = paalisa - 5
END IF
IF p1y - p2y >= 60 AND p1$ = "Snailman" THEN paalisa = -15
footlisa = 0
'IF p2$ = "Vilhelmo" OR p2$ = "Stefan" THEN footlisa = 40
'IF p2$ = "Naigon" THEN footlisa = 20
IF p2$ = "Vilhelmo" OR p2$ = "Stefan" OR p2$ = "Paeblo the Old" THEN
footlisa = 40
ELSEIF p2$ = "Paagah" THEN
footlisa = 65
ELSEIF p2$ = "TessaquÇ" OR p2$ = "Zairar" THEN
footlisa = 55
ELSEIF p2$ = "Naigon" THEN
footlisa = 20
ELSE
IF p2$ <> "Snailman" THEN footlisa = 10
END IF
footvari = POINT(p2x - 6, p2y + 65)
IF p2$ = "SiliconeAss" THEN footvari = 15
IF p2$ = "SiliconeAss" THEN
PSET (p2x - 3, p2y + 65), 0
DRAW "c0 d34nl5"
END IF
IF p2$ <> "Snailman" AND p2$ <> "Murg" AND p2$ <> "Larry the Glove" THEN
CALL poista2pjalka
LINE (p2x - 8, p2y + 65)-(p2x - 60 - footlisa, p2y + 63), footvari: DRAW "c" + STR$(footvari) + "u5"
FOR s = 1 TO 7
SOUND 50 + INT(RND * 50), .1
NEXT
LINE (p2x - 8, p2y + 65)-(p2x - 60 - footlisa, p2y + 63), 0: DRAW "c0u5"
CALL tee2pelaaja
IF p1x >= (p2x - 75 - footlisa - snak1 - paalisa) AND p2y + 60 < p1y + 110 AND p2y + 60 > p1y - 20 AND p1suojaa = 0 THEN
p1energia = p1energia - 5 - INT(RND * 6)
IF p1x > 30 THEN CALL poista1pelaaja: p1x = p1x - 15: CALL tee1pelaaja
CALL teeenergiat
END IF
ELSEIF p2$ = "Snailman" AND p2suojaa = 0 THEN
LINE (p2x - 40, p2y + 50)-(p2x + 40, p2y + 100), 0, BF
LINE (p2x + 40, p2y + 50)-(p2x - 90, p2y + 100), 14
LINE (p2x - 40, p2y + 50)-(p2x - 90, p2y + 100), 14
FOR s = 1 TO 7
SOUND 200 + INT(RND * 50), .1
NEXT
LINE (p2x + 40, p2y + 50)-(p2x - 90, p2y + 100), 0
LINE (p2x - 40, p2y + 50)-(p2x - 90, p2y + 100), 0
called = 2: CALL teesnailman
IF p1x >= (p2x - 90 - snak1 - paalisa) AND p1y >= p2y - 15 AND p1suojaa = 0 THEN
p1energia = p1energia - 4 - INT(RND * 4)
IF p1x > 30 THEN CALL poista1pelaaja: p1x = p1x - 15: CALL tee1pelaaja
CALL teeenergiat
END IF
ELSEIF p2$ = "Murg" THEN
PSET (p2x - 17, p2y + 94), 0
DRAW "c0br9bdd3lu3d3l5"
LINE (p2x - 18, p2y + 94)-(p2x - 150, p2y + 98), 2
FOR s = 1 TO 7
SOUND 300 + INT(RND * 100), .1
NEXT
LINE (p2x - 18, p2y + 94)-(p2x - 150, p2y + 98), 0
IF ABS(p2x - p1x) < (160 + (snak1 / 2)) AND p1y + 100 > p2y + 94 THEN
p1energia = p1energia - 8 - INT(RND * 5)
IF p1x > 30 THEN CALL poista1pelaaja: p1x = p1x - 15: CALL tee1pelaaja
CALL teeenergiat
END IF
CALL tee2pelaaja
CALL tee1pelaaja
ELSEIF p2$ = "Larry the Glove" AND p2suojaa = 0 THEN
LINE (p2x - 26, p2y + 82)-(p2x - 30, p2y + 99), 0
LINE (p2x - 27, p2y + 82)-(p2x - 30, p2y + 99), 0
LINE (p2x - 26, p2y + 82)-(p2x - 80, p2y + 70), 11
LINE (p2x - 30, p2y + 99)-(p2x - 80, p2y + 70), 11
FOR s = 1 TO 7
SOUND 300 + INT(RND * 200), .1
NEXT
IF ABS(p2x - p1x) < 115 + paalisa + ((snak1 / 2) - (10 * SGN(snak1))) AND p1y > p2y - 30 AND p1suojaa = 0 THEN
CALL poista1pelaaja
p1x = p1x - 60
IF p1x < 20 THEN p1x = 20
CALL tee1pelaaja
'IF p1x > 30 THEN CALL poista1pelaaja: p1x = p1x - 15: CALL tee1pelaaja
p1energia = p1energia - 5 - INT(RND * 5): CALL teeenergiat
END IF
LINE (p2x - 26, p2y + 82)-(p2x - 80, p2y + 70), 0
LINE (p2x - 30, p2y + 99)-(p2x - 80, p2y + 70), 0
PSET (p2x - 15, p2y + 30), 11
DRAW "c11m " + STR$(p2x + VAL(p2suunta$ + STR$(30))) + ", " + STR$(p2y + 99)
CALL tee2pelaaja
END IF
END SUB

SUB f11
IF p2$ = "SmileHead" THEN
IF ABS(p2x) < 590 THEN
CALL poista2pelaaja
p2x = 580 - INT(RND * ABS(p1x - 580)) + 35
IF p2x > 540 THEN p2x = p2x - 30
IF p2x - p1x < 65 THEN p2x = p2x + 30
CALL tee1pelaaja
FOR s = 1000 TO 5000 STEP 1000
SOUND s, .1
NEXT
CALL tee2pelaaja
END IF
ELSEIF p2$ = "Willex" AND p2pieree = 0 THEN
'ELSEIF p2$ = "Willex" AND p2pieree = 0 THEN
CALL poista2pelaaja
restop1x = p1x
restop1y = p1y
restop1$ = p1$
p1x = p2x
p1y = p2y
p1$ = p2$
called = 1: CALL tee1pelaaja
p2pieree = 1
p2pierx = p2x - 30
p2piery = p2y + 60
FOR s = 50 TO 300 STEP 50
SOUND s, .1
NEXT
p1x = restop1x
p1y = restop1y
p1$ = restop1$
CALL poista2pelaaja: CALL tee2pelaaja
ELSEIF p2$ = "Naigon" AND p2x - 135 > p1x AND p1suojaa = 0 THEN
CALL poista1pelaaja
vp1x = p1x
DO
p1x = p1x + 10
CALL tee1pelaaja
FOR s = 1000 TO 2000 STEP 500
SOUND s, .1
NEXT
LOOP UNTIL ABS(p2x - p1x) <= 120
oikeap1x = p1x
p1x = vp1x
DO
p1x = p1x + 10
CALL poista1pelaaja
LOOP UNTIL ABS(p2x - p1x) <= 120
p1x = oikeap1x
CALL tee1pelaaja: CALL tee2pelaaja
ELSEIF p2$ = "Stefan" THEN
CALL poista2pkasi
LINE (p2x - 10, p2y + 30)-(p2x - 80, p2y + 30), 12
IF ABS(p2x - p1x) < 95 + (snak1 / 2) THEN
es = INT(RND * 6): p1energia = p1energia - 5 - es: p2energia = p2energia + 5 + es
END IF
FOR s = 3000 TO 2000 STEP -250
SOUND s, .1
NEXT
LOCATE 10, 30: PRINT "                          "
'INE (p1x + 10, p1y + 30)-(p1x + 80, p1y + 30), 0 ' mikÑ ine
LINE (p2x - 10, p2y + 30)-(p2x - 80, p2y + 30), 0
CALL poista1pelaaja: CALL tee1pelaaja
CALL teeenergiat
ELSEIF p2$ = "Vilhelmo" THEN
salamax = 40 + INT(RND * 560)
PSET (salamax, 101), 11
DRAW "c11 g10d20r5f15l4d5l6d10g3f3d14g5d16g5d7l2d3f9d4l5g3d12f13d5l3g2d3l3g6d7r3d7r2d20g5f2g8d3r2d15g6d3l4d10f5d20f3d10 bl3l2hl2br5bu2h5br15g5bd2r2ur3e4bl3bu2e7"
FOR s = 1 TO 6
SOUND 100 + INT(RND * 300), .1
NEXT
PSET (salamax, 101), 0
DRAW "c0 g10d20r5f15l4d5l6d10g3f3d14g5d16g5d7l2d3f9d4l5g3d12f13d5l3g2d3l3g6d7r3d7r2d20g5f2g8d3r2d15g6d3l4d10f5d20f3d10 bl3l2hl2br5bu2h5br15g5bd2r2ur3e4bl3bu2e7"
IF p1suojaa = 0 AND ABS(p1x - salamax) <= 30 THEN
p1energia = p1energia - 10 - INT(RND * 3)
CALL teeenergiat
END IF
IF ABS(p2x - salamax) <= 30 THEN p2energia = p2energia - 10 - INT(RND * 3): CALL teeenergiat
ELSEIF p2$ = "Snailman" THEN
IF p2suojaa = 0 THEN
CALL poista2pelaaja
CIRCLE (p2x, p2y + 70), 60, 14, , , .5
CIRCLE (p2x - 14, p2y + 50), 10, 15
CIRCLE (p2x + 14, p2y + 50), 10, 15
FOR silr = 1 TO 3
CIRCLE (p2x - 14 + VAL(p2suunta$ + STR$(5)), p2y + 50), silr, 3
CIRCLE (p2x + 14 + VAL(p2suunta$ + STR$(5)), p2y + 50), silr, 3
NEXT
p2suojaa = 1
'ja tÑhÑn poisto...
ELSEIF p2suojaa = 1 THEN
p2suojaa = 0
CIRCLE (p2x, p2y + 70), 60, 0, , , .5
CALL poista2pelaaja
CALL tee2pelaaja
END IF
ELSEIF p2$ = "Murg" THEN
IF p2x >= 90 THEN etaisyys = 80
IF p2x < 90 THEN etaisyys = p2x
CALL poista2pelaaja
p2x = p2x - etaisyys
FOR p2y = 200 TO 290 STEP 20
CALL tee2pelaaja
SOUND 100 + INT(RND * 200), .1
CALL poista2pelaaja: CALL tee1pelaaja
NEXT
IF snak1 = 0 THEN osumalisa = 10
IF ABS(p1x - p2x) < etaisyys - 20 + (snak1 / 2) - (5 * SGN(snak1)) + osumalisa AND p1suojaa = 0 THEN
p1energia = p1energia - 12 - INT(RND * 6)
CALL teeenergiat
END IF
p2x = p2x + etaisyys
CALL tee2pelaaja: CALL tee1pelaaja
ELSEIF p2$ = "Paeblo the Old" THEN
FOR hiukkaset = 1 TO 5
s = 500
hiukay = p2y + INT(RND * 80)
FOR hiukax = p2x - 20 + INT(RND * 40) TO p1x - 20 + INT(RND * 40) STEP -5
PSET (hiukax, hiukay), 15
PSET (hiukax + 1, hiukay), 15
PSET (hiukax + 2, hiukay), 15
s = s + 2
IF INT(RND * 3) = 1 THEN SOUND s, .1
PSET (hiukax, hiukay), 0
PSET (hiukax + 1, hiukay), 0
PSET (hiukax + 2, hiukay), 0
NEXT
NEXT
IF p1suojaa = 1 THEN GOTO ohiloo2
p1jahm = 1
LINE (p1x - 35, p1y - 35)-(p1x + 35, p1y + 85), 15, B
p1jaika = TIMER
ohiloo2:
p2energia = p2energia - 35: CALL teeenergiat
ELSEIF p2$ = "Paagah" AND p2notjodlmaix = 0 THEN
p2notjodlmaix = 1
p2njmxx = p2x - 40
naika2 = TIMER
LINE (p2njmxx, 350)-(p2njmxx + 40, 400), 13, BF
CALL tee2pelaaja
ELSEIF p2$ = "Larry the Glove" AND p2laavaa = 0 AND p2suojaa = 0 THEN
p2laavaa = 1
p2laax = p2x
p2laay = 110
'heliumi Hanska tm
PSET (p2laax, p2laay), 0
DRAW "c10d3fd3fd3fd3fd3fd3fd3l25u12l10u4r10u4l12u4e4r26l8bd5c7l5d5r5u5bl4c15bd2dru"
DRAW "bd12bl4d5u3r3d3u5"
raika2 = TIMER
ELSEIF p2$ = "SiliconeAss" AND p2silii = 0 AND p2pysii = 0 THEN
CALL poista2pelaaja
restop1x = p1x
restop1y = p1y
restop1$ = p1$
p1x = p2x
p1y = p2y
p1$ = p2$
called = 1: CALL tee1pelaaja
FOR s = 1 TO 10
SOUND 500 + INT(RND * 300), .1
NEXT
p2silii = 1
p2silix = p2x - 40
p2sily = p2y + 60
CIRCLE (p2silix, p2sily), 10, 14 + INT(RND * 2)
sjaika2 = TIMER
p1x = restop1x
p1y = restop1y
p1$ = restop1$
CALL poista2pelaaja: CALL tee2pelaaja
ELSEIF p2$ = "TessaquÇ" AND p2leijuu = 0 THEN
p2vauhti = 15
p2leijuu = 1
CALL poista2pelaaja
p2y = 270
CALL tee2pelaaja
p2hyppaa = 0
ELSEIF p2$ = "TessaquÇ" AND p2leijuu = 1 THEN
p2vauhti = 0
p2leijuu = 0
CALL poista2pelaaja
p2y = 300
CALL tee2pelaaja
ELSEIF p2$ = "Slussen HyperSpirit Invisible" THEN
IF ABS(p2x - p1x) < 90 THEN
CALL poista1pelaaja
IF p1x > 80 THEN
p1x = p1x - 50
ELSE
p1x = 25
END IF
CALL tee1pelaaja
END IF
FOR s = 100 TO 200 STEP 50
SOUND s, .1
NEXT
IF ABS(p2x - p1x) < 90 THEN p1energia = p1energia - 4 - INT(RND * 5): CALL teeenergiat
ELSEIF p2$ = "Zairar" THEN
IF ABS(p1x - 20) > 50 THEN
CALL poista1pelaaja
p1x = 60 + INT(RND * (p2x - 30 - 60))
IF p1x < 100 THEN p1x = p1x + 30
IF p2x - p1x < 65 THEN p1x = p1x - 30
CALL tee2pelaaja
FOR s = 100 TO 2000 STEP 100
SOUND s, .1
NEXT
CALL tee1pelaaja
END IF
END IF
END SUB

SUB f12
IF p2$ = "SmileHead" AND p2pallottaa = 0 THEN
p2pallottaa = 1
p2pallox = p1x
p2palloy = 120
FOR palr = 1 TO 7
CIRCLE (p2pallox, p2palloy), palr, 2 + 8 * INT(RND * 2) + 5 * INT(RND * 2)
NEXT
ELSEIF p2$ = "Willex" AND p2viher = 0 AND p1viher = 0 THEN
PSET (p2x + 25, p2y + 60), 14
DRAW "c14nr40e2r2e2r2e2r2e2r2bd15l4hl4hr2h2br5r8f2r6f2r3bu12l3gl3g2l3"
FOR s = 500 TO 50 STEP -10
SOUND s, .1
NEXT
PSET (p2x + 25, p2y + 60), 0
DRAW "c0nr40e2r2e2r2e2r2e2r2bd15l4hl4hr2h2br5r8f2r6f2r3bu12l3gl3g2l3"
palari2 = 60
PALETTE 0, palari2 * 256
p2viher = 1
p1energia = p1energia - 15 - INT(RND * 8)
CALL teeenergiat
ELSEIF p2$ = "Naigon" THEN
handvari = 13
CALL poista2pkasi
FOR handy = -30 TO 30 STEP 10
LINE (p2x - 10, p2y + 30)-(p2x - 60, p2y - handy), handvari + 2 * INT(RND * 2)
SOUND 100 + handy, .14
LINE (p2x - 10, p2y + 30)-(p2x - 60, p2y - handy), 0
NEXT
IF ABS(p2x - p1x) < 95 AND p1suojaa = 0 THEN
np1x = p1x
FOR p1x = np1x TO np1x - 100 STEP -10
CALL poista1pelaaja
IF p1x <= 40 THEN EXIT FOR
SOUND 50 + INT(RND * 50), .2
NEXT
CALL tee1pelaaja
p1energia = p1energia - 8 - INT(RND * 4): CALL teeenergiat
END IF
CALL poista2pelaaja: CALL tee2pelaaja
ELSEIF p2$ = "Stefan" AND p2tulee = 0 THEN
p2tulee = 1
p2tulix = p2x - 30
p2tuliy = p2y + 35
FOR s = 50 TO 300 STEP 25
SOUND s, .1
NEXT
CALL poista2pelaaja: CALL tee2pelaaja
ELSEIF p2$ = "Vilhelmo" AND p2hyppaa = 0 AND p2sahkoo = 0 THEN
'e-menetys vain jos ei ilmassa
sahx2 = p2x - 55
FOR sahr = 1 TO 5
CIRCLE (sahx2, 395), sahr, 11 + (4 * INT(RND * 2))
NEXT
FOR s = 2000 TO 500 STEP -500
SOUND s, .1
NEXT
saika2 = TIMER
p2sahkoo = 1
ELSEIF p2$ = "Snailman" AND p2suojaa = 0 THEN
CIRCLE (p2x - 14, p2y - 38), 10, 0
'CIRCLE (p2x + 14, p2y - 38), 10, 0
FOR silr = 1 TO 3
CIRCLE (p2x - 14 - 5, p2y - 38), silr, 0
'CIRCLE (p2x + 14 + 5, p2y - 38), silr, 0
NEXT
six = p2x - 44
siy = p2y - 68
s = 500
DO
s = s + 50
CIRCLE (six, siy), 10, 15
SOUND s, .1
'CIRCLE (six, siy), 10, 0
IF ABS(six - p1x) < 30 AND ABS(siy - p1y) < 35 + (snak1 / 2) AND p1suojaa = 0 THEN
p1energia = p1energia - 8 - INT(RND * 5)
IF p1x > 30 THEN CALL poista1pelaaja: p1x = p1x - 10: CALL tee1pelaaja
CALL teeenergiat
CALL tee2pelaaja: CALL poista1pelaaja: CALL tee1pelaaja
END IF
six = six - 30
siy = siy - 30
IF six < 40 THEN GOTO poisvain2
IF siy < 130 THEN GOTO seur2
LOOP
seur2:
DO
s = s + 50
CIRCLE (six, siy), 10, 15
SOUND s, .1
'CIRCLE (six, siy), 10, 0
IF ABS(six - p1x) < 40 AND ABS(siy - p1y) < 35 + (snak1 / 2) AND p1suojaa = 0 THEN
p1energia = p1energia - 8 - INT(RND * 5)
IF p1x > 30 THEN CALL poista1pelaaja: p1x = p1x - 10: CALL tee1pelaaja
CALL teeenergiat
CALL tee2pelaaja: CALL poista1pelaaja: CALL tee1pelaaja
END IF
six = six - 30
siy = siy + 30
IF six < 40 THEN GOTO poisvain2
IF siy >= 310 THEN GOTO poisvain2
LOOP
poisvain2:
CIRCLE (p2x - 14, p2y - 38), 10, 15
FOR silr = 1 TO 3
CIRCLE (p2x - 14 - 5, p2y - 38), silr, 3
NEXT
LINE (0, p2y + 80)-(640, 80), 0, BF: CALL teepelaajat
ELSEIF p2$ = "Murg" THEN
FOR pokx = 1 TO 50
CIRCLE (p2x, p2y + 30), pokx, 9 + (2 * INT(RND * 2))
IF INT(RND * 2) = 1 THEN SOUND 1000 + INT(RND * 500), .1
NEXT
FOR pokx = 1 TO 50
CIRCLE (p2x, p2y + 30), pokx, 0
NEXT
CALL poista2pelaaja
p2x = 600: CALL tee2pelaaja: CALL tee1pelaaja
ELSEIF p2$ = "Paeblo the Old" AND p2kuiluu = 0 THEN
FOR s = 1 TO 10
SOUND 100 + INT(RND * 50), .1
NEXT
p2kuiluu = 1
p2kuilux = INT(RND * 560)
LINE (p2kuilux, 400)-(p2kuilux + 80, 480), 0, BF
kuika2 = TIMER
ELSEIF p2$ = "Paagah" THEN
FOR lyy = 320 TO 370 STEP 10
LINE (0, lyy)-(640, lyy), 11 + (INT(RND * 2) * 4)
NEXT
FOR s = 1 TO 7
SOUND 400 + INT(RND * 200), .1
NEXT
FOR lyy = 320 TO 370 STEP 10
LINE (0, lyy)-(640, lyy), 0
NEXT
IF p1hyppaa = 1 AND p1x < 620 THEN
CALL poista1pelaaja
alaku = p1x
FOR p1x = alaku TO 20 STEP -20
CALL tee1pelaaja
oota = TIMER: DO: LOOP WHILE ABS(TIMER - oota) < .01
CALL poista1pelaaja
NEXT
FOR s = 1 TO 14
SOUND 100 + INT(RND * 100), .1
NEXT
p1energia = p1energia - 14 - INT(RND * 8): CALL teeenergiat
END IF
CALL tee2pelaaja: CALL tee1pelaaja
ELSEIF p2$ = "Larry the Glove" THEN
IF p2hyppaa = 1 THEN
CALL poista2pelaaja
p2hyppaa = 0
p2y = 300
END IF
IF p2suojaa = 0 THEN
CALL poista2pelaaja
PSET (p2x - 35, p2y + 99), 0
DRAW "c11nr70m" + STR$(p2x) + "," + STR$(p2y - 20) + "m" + STR$(p2x + 35) + "," + STR$(p2y + 99)
p2suojaa = 1
ELSE
p2suojaa = 0
CALL poista2pelaaja: CALL tee2pelaaja
END IF
ELSEIF p2$ = "SiliconeAss" THEN
CALL poista2pelaaja
restop1x = p1x
restop1y = p1y
restop1$ = p1$
p1x = p2x
p1y = p2y
p1$ = p2$
called = 1: CALL tee1pelaaja
CIRCLE (p1x - 10, p1y + 55), 9, 0, 1.7, 4.6
CIRCLE (p1x, p1y + 55), 112, 13, 1.7, 4.6, .05
FOR s = 1 TO 10
SOUND 100 + INT(RND * 100), .05
NEXT
CIRCLE (p1x, p1y + 55), 112, 0, 1.7, 4.6, .05
p1x = restop1x
p1y = restop1y
p1$ = restop1$
CALL poista2pelaaja: CALL tee2pelaaja
IF ABS(p2x - p1x) < 130 + snak1 AND p1suojaa = 0 AND p2y - p1y < 45 THEN
p1energia = p1energia - 7 - INT(RND * 6)
CALL teeenergiat
END IF
ELSEIF p2$ = "TessaquÇ" THEN
firex = 20 + INT(RND * 600)
FOR firey = 120 TO 360 STEP 10
FOR firer = 1 TO 10 STEP 5
CIRCLE (firex, firey), firer, 10 + INT(RND * 2)
NEXT
NEXT
FOR s = 1 TO 20
SOUND 100 + INT(RND * 100), .05
NEXT
FOR firey = 120 TO 360 STEP 10
FOR firer = 1 TO 10 STEP 5
CIRCLE (firex, firey), firer, 0
NEXT
NEXT
IF ABS(p2x - firex) < 40 AND p2suojaa = 0 THEN
p2energia = p2energia - 8 - INT(RND * 10)
CALL teeenergiat
END IF
IF ABS(p1x - firex) < 40 THEN
p1energia = p1energia - 14 - INT(RND * 10)
CALL teeenergiat
END IF
ELSEIF p2$ = "Slussen HyperSpirit Invisible" THEN
SOUND 1500, .5
CALL poista2pelaaja
p2x = p1x + 90 + INT(RND * (640 - 90 - p1x)) - 20
CALL tee2pelaaja
ELSEIF p2$ = "Zairar" THEN
LINE (p2x + 10, p2y + 30)-(p2x + 25, p2y + 50), 0
IF p2x + 10 > 330 THEN
LINE (p2x + 10, p2y + 30)-(640, p2y + 30), 11
ELSE
LINE (p2x + 10, p2y + 30)-(p2x + 310, p2y + 30), 11
END IF
IF p2x + 10 > 330 THEN
LINE (0, p2y + 30)-(310 - (640 - (p2x + 10)), p2y + 30), 11
IF 310 - (640 - (p2x + 10)) > p1x - 20 THEN
imi = 6 + INT(RND * 5)
p1energia = p1energia - imi
p2energia = p2energia + imi
CALL teeenergiat
END IF
END IF
FOR s = 1 TO 6
SOUND 600 + INT(RND * 400), .1
NEXT
LINE (0, p2y + 30)-(310 - (640 - (p2x + 10)), p2y + 30), 0
LINE (p2x + 10, p2y + 30)-(640, p2y + 30), 0
CALL teepelaajat
END IF
END SUB

SUB f2
IF p2y - p1y >= 60 AND p2$ <> "Snailman" AND LEFT$(p2$, 5) <> "Laavi" THEN
paalisa = 15
IF p2$ = "Murg" THEN paalisa = paalisa - 5
END IF
IF p2y - p1y >= 60 AND p2$ = "Snailman" THEN paalisa = -15
footlisa = 0
IF p1$ = "Vilhelmo" OR p1$ = "Stefan" OR p1$ = "Paeblo the Old" THEN
footlisa = 40
ELSEIF p1$ = "Naigon" THEN
footlisa = 20
ELSEIF p1$ = "TessaquÇ" OR p1$ = "Zairar" THEN
footlisa = 55
ELSEIF p1$ = "Paagah" THEN
footlisa = 65
ELSE
IF p1$ <> "Snailman" THEN footlisa = 10
END IF
footvari = POINT(p1x + 8, p1y + 65)
IF p1$ <> "Snailman" AND p1$ <> "Murg" AND p1$ <> "Larry the Glove" THEN
CALL poista1pjalka
LINE (p1x + 8, p1y + 65)-(p1x + 60 + footlisa, p1y + 63), footvari: DRAW "c" + STR$(footvari) + "u5"
FOR s = 1 TO 7
SOUND 50 + INT(RND * 50), .1
NEXT
LINE (p1x + 8, p1y + 65)-(p1x + 60 + footlisa, p1y + 63), 0: DRAW "c0u5"
CALL tee1pelaaja
IF p2x <= (p1x + 75 + footlisa + snak2 + paalisa) AND p1y + 60 < p2y + 110 AND p1y + 60 > p2y - 20 AND p2suojaa = 0 THEN
p2energia = p2energia - 5 - INT(RND * 6)
IF p2x < 610 THEN CALL poista2pelaaja: p2x = p2x + 15: CALL tee2pelaaja
CALL teeenergiat
END IF
ELSEIF p1$ = "Snailman" AND p1suojaa = 0 THEN
LINE (p1x - 40, p1y + 50)-(p1x + 40, p1y + 100), 0, BF
LINE (p1x - 40, p1y + 50)-(p1x + 90, p1y + 100), 14
LINE (p1x + 40, p1y + 50)-(p1x + 90, p1y + 100), 14
FOR s = 1 TO 7
SOUND 200 + INT(RND * 50), .1
NEXT
LINE (p1x - 40, p1y + 50)-(p1x + 90, p1y + 100), 0
LINE (p1x + 40, p1y + 50)-(p1x + 90, p1y + 100), 0
called = 1: CALL teesnailman
IF p2x <= (p1x + 90 + snak2 + paalisa) AND p2y >= p1y - 10 AND p2suojaa = 0 THEN
p2energia = p2energia - 4 - INT(RND * 4)
IF p2x < 610 THEN CALL poista2pelaaja: p2x = p2x + 15: CALL tee2pelaaja
CALL teeenergiat
END IF

ELSEIF p1$ = "Murg" THEN
PSET (p1x + 18, p1y + 94), 0
DRAW "c0bl9bdd3ru3d3r5"
LINE (p1x + 18, p1y + 94)-(p1x + 150, p1y + 98), 2
FOR s = 1 TO 7
SOUND 300 + INT(RND * 100), .1
NEXT
LINE (p1x + 18, p1y + 94)-(p1x + 150, p1y + 98), 0
IF ABS(p2x - p1x) < (160 + (snak2 / 2)) AND p2y + 100 > p1y + 94 THEN
p2energia = p2energia - 8 - INT(RND * 5)
IF p2x < 610 THEN CALL poista2pelaaja: p2x = p2x + 15: CALL tee2pelaaja
CALL teeenergiat
END IF
CALL tee1pelaaja
CALL tee2pelaaja
ELSEIF p1$ = "Larry the Glove" AND p1suojaa = 0 THEN
LINE (p1x + 26, p1y + 82)-(p1x + 30, p1y + 99), 0
LINE (p1x + 27, p1y + 82)-(p1x + 30, p1y + 99), 0
LINE (p1x + 26, p1y + 82)-(p1x + 80, p1y + 70), 11
LINE (p1x + 30, p1y + 99)-(p1x + 80, p1y + 70), 11
FOR s = 1 TO 7
SOUND 300 + INT(RND * 200), .1
NEXT
IF ABS(p2x - p1x) < 115 + paalisa + ((snak2 / 2) - (10 * SGN(snak2))) AND p2y > p1y - 30 AND p2suojaa = 0 THEN
CALL poista2pelaaja
p2x = p2x + 60
IF p2x > 620 THEN p2x = 620
CALL tee2pelaaja
'IF p2x < 610 THEN CALL poista2pelaaja: p2x = p2x + 15: CALL tee2pelaaja
p2energia = p2energia - 5 - INT(RND * 5): CALL teeenergiat
END IF
LINE (p1x + 26, p1y + 82)-(p1x + 80, p1y + 70), 0
LINE (p1x + 30, p1y + 99)-(p1x + 80, p1y + 70), 0
PSET (p1x + 15, p1y + 30), 11
DRAW "c11m " + STR$(p1x + VAL(p1suunta$ + STR$(30))) + ", " + STR$(p1y + 99)
CALL tee1pelaaja
END IF
END SUB

SUB f3
IF p1$ = "SmileHead" THEN
IF ABS(p1x - 20) > 50 THEN
CALL poista1pelaaja
p1x = 60 + INT(RND * (p2x - 30 - 60))
IF p1x < 100 THEN p1x = p1x + 30
IF p2x - p1x < 65 THEN p1x = p1x - 30
CALL tee2pelaaja
FOR s = 500 TO 2000 STEP 500
SOUND s, .1
NEXT
CALL tee1pelaaja
END IF
ELSEIF p1$ = "Willex" AND p1pieree = 0 THEN
CALL poista1pelaaja
restop2x = p2x
restop2y = p2y
restop2$ = p2$
p2x = p1x
p2y = p1y
p2$ = p1$
called = 2: CALL tee2pelaaja
p1pieree = 1
p1pierx = p1x + 30
p1piery = p1y + 60
FOR s = 50 TO 300 STEP 50
SOUND s, .1
NEXT
p2x = restop2x
p2y = restop2y
p2$ = restop2$
CALL poista1pelaaja: CALL tee1pelaaja
ELSEIF p1$ = "Naigon" AND p1x + 135 < p2x AND p2suojaa = 0 THEN
CALL poista2pelaaja
vp2x = p2x
DO
p2x = p2x - 10
CALL tee2pelaaja
FOR s = 1000 TO 2000 STEP 500
SOUND s, .1
NEXT
LOOP UNTIL ABS(p2x - p1x) <= 120
oikeap2x = p2x
p2x = vp2x
DO
p2x = p2x - 10
CALL poista2pelaaja
LOOP UNTIL (p2x - p1x) <= 120
p2x = oikeap2x
CALL tee2pelaaja: CALL tee1pelaaja
ELSEIF p1$ = "Stefan" THEN
CALL poista1pkasi
LINE (p1x + 10, p1y + 30)-(p1x + 80, p1y + 30), 12
IF ABS(p2x - p1x) < 95 + (snak2 / 2) THEN
es = INT(RND * 6): p2energia = p2energia - 5 - es: p1energia = p1energia + 5 + es
END IF
FOR s = 3000 TO 2000 STEP -250
SOUND s, .1
NEXT
LOCATE 10, 30: PRINT "                          "
LINE (p1x + 10, p1y + 30)-(p1x + 80, p1y + 30), 0
CALL poista1pelaaja: CALL tee1pelaaja
CALL teeenergiat
ELSEIF p1$ = "Vilhelmo" THEN
salamax = 40 + INT(RND * 560)
PSET (salamax, 101), 11
DRAW "c11 g10d20r5f15l4d5l6d10g3f3d14g5d16g5d7l2d3f9d4l5g3d12f13d5l3g2d3l3g6d7r3d7r2d20g5f2g8d3r2d15g6d3l4d10f5d20f3d10 bl3l2hl2br5bu2h5br15g5bd2r2ur3e4bl3bu2e7"
FOR s = 1 TO 6
SOUND 100 + INT(RND * 300), .1
NEXT
PSET (salamax, 101), 0
DRAW "c0 g10d20r5f15l4d5l6d10g3f3d14g5d16g5d7l2d3f9d4l5g3d12f13d5l3g2d3l3g6d7r3d7r2d20g5f2g8d3r2d15g6d3l4d10f5d20f3d10 bl3l2hl2br5bu2h5br15g5bd2r2ur3e4bl3bu2e7"
IF p2suojaa = 0 AND ABS(p2x - salamax) <= 30 THEN
p2energia = p2energia - 10 - INT(RND * 3)
CALL teeenergiat
END IF
IF ABS(p1x - salamax) <= 30 THEN p1energia = p1energia - 10 - INT(RND * 3): CALL teeenergiat
ELSEIF p1$ = "Snailman" THEN
IF p1suojaa = 0 THEN
CALL poista1pelaaja
CIRCLE (p1x, p1y + 70), 60, 14, , , .5
CIRCLE (p1x - 14, p1y + 50), 10, 15
CIRCLE (p1x + 14, p1y + 50), 10, 15
FOR silr = 1 TO 3
CIRCLE (p1x - 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 3
CIRCLE (p1x + 14 + VAL(p1suunta$ + STR$(5)), p1y + 50), silr, 3
NEXT
p1suojaa = 1
'ja tÑhÑn poisto...
ELSEIF p1suojaa = 1 THEN
p1suojaa = 0
CIRCLE (p1x, p1y + 70), 60, 0, , , .5
CALL poista1pelaaja
CALL tee1pelaaja
END IF




ELSEIF p1$ = "Murg" THEN
IF 640 - p1x >= 90 THEN etaisyys = 80
IF 640 - p1x < 90 THEN etaisyys = 640 - p1x
CALL poista1pelaaja
p1x = p1x + etaisyys
FOR p1y = 200 TO 290 STEP 20
CALL tee1pelaaja
SOUND 100 + INT(RND * 200), .1
CALL poista1pelaaja: CALL tee2pelaaja
NEXT
IF snak2 = 0 THEN osumalisa = 10
IF ABS(p2x - p1x) < etaisyys - 20 + (snak2 / 2) - (5 * SGN(snak2)) + osumalisa AND p2suojaa = 0 THEN
p2energia = p2energia - 12 - INT(RND * 6)
CALL teeenergiat
END IF
p1x = p1x - etaisyys
CALL tee1pelaaja: CALL tee2pelaaja
ELSEIF p1$ = "Paeblo the Old" THEN
FOR hiukkaset = 1 TO 5
s = 500
hiukay = p1y + INT(RND * 80)
FOR hiukax = p1x + 20 - INT(RND * 40) TO p2x + 20 - INT(RND * 40) STEP 5
PSET (hiukax, hiukay), 15
PSET (hiukax + 1, hiukay), 15
PSET (hiukax + 2, hiukay), 15
s = s + 2
IF INT(RND * 3) = 1 THEN SOUND s, .1
PSET (hiukax, hiukay), 0
PSET (hiukax + 1, hiukay), 0
PSET (hiukax + 2, hiukay), 0
NEXT
NEXT
IF p2suojaa = 1 THEN GOTO ohiloo1
p2jahm = 1
LINE (p2x - 35, p2y - 35)-(p2x + 35, p2y + 85), 15, B
p2jaika = TIMER
ohiloo1:
p1energia = p1energia - 35: CALL teeenergiat
ELSEIF p1$ = "Paagah" AND p1notjodlmaix = 0 THEN
p1notjodlmaix = 1
p1njmxx = p1x + 40
naika1 = TIMER
LINE (p1njmxx, 350)-(p1njmxx + 40, 400), 13, BF
ELSEIF p1$ = "Larry the Glove" AND p1laavaa = 0 AND p1suojaa = 0 THEN
p1laavaa = 1
p1laax = p1x
p1laay = 110
PSET (p1laax, p1laay), 0
'heliumi Hanska tm
DRAW "c10d3gd3gd3gd3gd3gd3gd3r25u12r10u4l10u4r12u4h4l26r8bd5c7r5d5l5u5br4c15bd2dlu"
DRAW "bd12br4d5u3l3d3u5"
raika1 = TIMER
ELSEIF p1$ = "SiliconeAss" AND p1silii = 0 AND p1pysii = 0 THEN
CALL poista1pelaaja
restop2x = p2x
restop2y = p2y
restop2$ = p2$
p2x = p1x
p2y = p1y
p2$ = p1$
called = 2: CALL tee2pelaaja
FOR s = 1 TO 10
SOUND 500 + INT(RND * 300), .1
NEXT
p1silii = 1
p1silix = p1x + 40
p1sily = p1y + 60
CIRCLE (p1silix, p1sily), 10, 14 + INT(RND * 2)
sjaika1 = TIMER
p2x = restop2x
p2y = restop2y
p2$ = restop2$
CALL poista1pelaaja: CALL tee1pelaaja
ELSEIF p1$ = "TessaquÇ" AND p1leijuu = 0 THEN
p1vauhti = 20
p1leijuu = 1
CALL poista1pelaaja
p1y = 270
CALL tee1pelaaja
p1hyppaa = 0
ELSEIF p1$ = "TessaquÇ" AND p1leijuu = 1 THEN
p1vauhti = 0
p1leijuu = 0
CALL poista1pelaaja
p1y = 300
CALL tee1pelaaja
ELSEIF p1$ = "Slussen HyperSpirit Invisible" THEN
FOR s = 100 TO 200 STEP 50
SOUND s, .1
NEXT
IF ABS(p2x - p1x) < 90 THEN
CALL poista2pelaaja
IF p2x < 560 THEN
p2x = p2x + 50
ELSE
p2x = 615
END IF
CALL tee2pelaaja
p2energia = p2energia - 4 - INT(RND * 5): CALL teeenergiat
END IF
ELSEIF p1$ = "Zairar" THEN
IF ABS(p2x) < 590 THEN
CALL poista2pelaaja
p2x = 580 - INT(RND * ABS(p1x - 580)) + 35
IF p2x > 540 THEN p2x = p2x - 30
IF p2x - p1x < 65 THEN p2x = p2x + 30
CALL tee1pelaaja
FOR s = 100 TO 2000 STEP 100
SOUND s, .1
NEXT
CALL tee2pelaaja
END IF
END IF
END SUB

SUB f4
IF p1$ = "SmileHead" AND p1pallottaa = 0 THEN
p1pallottaa = 1
p1pallox = p2x
p1palloy = 120
FOR palr = 1 TO 7
CIRCLE (p1pallox, p1palloy), palr, 2 + 8 * INT(RND * 2) + 5 * INT(RND * 2)
NEXT
ELSEIF p1$ = "Willex" AND p1viher = 0 AND p2viher = 0 THEN
PSET (p1x - 25, p1y + 60), 14
DRAW "c14nl40h2l2h2l2h2l2h2l2bd15r4er4er2e2bl5l8g2l6g2l3bu12r3fr3f2r3"
FOR s = 500 TO 50 STEP -10
SOUND s, .1
NEXT
PSET (p1x - 25, p1y + 60), 0
DRAW "c0nl40h2l2h2l2h2l2h2l2bd15r4er4er2e2bl5l8g2l6g2l3bu12r3fr3f2r3"
palari1 = 60
PALETTE 0, palari1 * 256
p1viher = 1
p2energia = p2energia - 15 - INT(RND * 8)
CALL teeenergiat
ELSEIF p1$ = "Naigon" THEN
handvari = POINT(p1x + 10, p1y + 30)
CALL poista1pkasi
FOR handy = -30 TO 30 STEP 10
LINE (p1x + 10, p1y + 30)-(p1x + 60, p1y - handy), handvari + 2 * INT(RND * 2)
SOUND 100 + handy, .14
LINE (p1x + 10, p1y + 30)-(p1x + 60, p1y - handy), 0
NEXT
IF ABS(p2x - p1x) < 95 AND p2suojaa = 0 THEN
np2x = p2x
FOR p2x = np2x TO np2x + 100 STEP 10
CALL poista2pelaaja
IF p2x >= 600 THEN EXIT FOR
SOUND 50 + INT(RND * 50), .2
NEXT
CALL tee2pelaaja
p2energia = p2energia - 8 - INT(RND * 4): CALL teeenergiat
END IF
CALL poista1pelaaja: CALL tee1pelaaja
ELSEIF p1$ = "Stefan" AND p1tulee = 0 THEN
p1tulee = 1
p1tulix = p1x + 30
p1tuliy = p1y + 35
FOR s = 50 TO 300 STEP 25
SOUND s, .1
NEXT
CALL poista1pelaaja: CALL tee1pelaaja
ELSEIF p1$ = "Vilhelmo" AND p1hyppaa = 0 AND p1sahkoo = 0 THEN
sahx1 = p1x + 55
FOR sahr = 1 TO 5
CIRCLE (sahx1, 395), sahr, 11 + (4 * INT(RND * 2))
NEXT
FOR s = 2000 TO 500 STEP -500
SOUND s, .1
NEXT
saika1 = TIMER
p1sahkoo = 1
ELSEIF p1$ = "Snailman" AND p1suojaa = 0 THEN
'CIRCLE (p1x - 14, p1y - 38), 10, 0
CIRCLE (p1x + 14, p1y - 38), 10, 0
FOR silr = 1 TO 3
'CIRCLE (p1x - 14 + 5, p1y - 38), silr, 0
CIRCLE (p1x + 14 + 5, p1y - 38), silr, 0
NEXT
six = p1x + 44
siy = p1y - 68
s = 500
DO
s = s + 50
CIRCLE (six, siy), 10, 15
SOUND s, .1: PSET (0, 0), 0
'CIRCLE (six, siy), 10, 0
IF ABS(six - p2x) < 30 AND ABS(siy - p2y) < 35 + (snak2 / 2) AND p2suojaa = 0 THEN
p2energia = p2energia - 8 - INT(RND * 5)
IF p2x < 610 THEN CALL poista2pelaaja: p2x = p2x + 10: CALL tee2pelaaja
CALL teeenergiat
CALL tee1pelaaja: CALL poista2pelaaja: CALL tee2pelaaja
END IF
six = six + 30
siy = siy - 30
IF six > 600 THEN GOTO poisvain1
IF siy < 130 THEN GOTO seur1
LOOP
seur1:
DO
s = s + 50
CIRCLE (six, siy), 10, 15
SOUND s, .1
'CIRCLE (six, siy), 10, 0
IF ABS(six - p2x) < 40 AND ABS(siy - p2y) < 35 + (snak2 / 2) AND p2suojaa = 0 THEN
p2energia = p2energia - 8 - INT(RND * 5)
IF p2x < 610 THEN CALL poista2pelaaja: p2x = p2x + 10: CALL tee2pelaaja
CALL teeenergiat
CALL tee1pelaaja: CALL poista2pelaaja: CALL tee2pelaaja
END IF
six = six + 30
siy = siy + 30
IF six > 600 THEN GOTO poisvain1
IF siy >= 310 THEN GOTO poisvain1
LOOP
poisvain1:
CIRCLE (p1x + 14, p1y - 38), 10, 15
FOR silr = 1 TO 3
CIRCLE (p1x + 14 + 5, p1y - 38), silr, 3
NEXT
LINE (0, p1y + 80)-(640, 80), 0, BF: CALL teepelaajat
ELSEIF p1$ = "Murg" THEN
FOR pokx = 1 TO 50
CIRCLE (p1x, p1y + 30), pokx, 9 + (2 * INT(RND * 2))
IF INT(RND * 2) = 1 THEN SOUND 1000 + INT(RND * 500), .1
NEXT
FOR pokx = 1 TO 50
CIRCLE (p1x, p1y + 30), pokx, 0
NEXT
CALL poista1pelaaja
p1x = 40: CALL tee1pelaaja: CALL tee2pelaaja
ELSEIF p1$ = "Paeblo the Old" AND p1kuiluu = 0 THEN
FOR s = 1 TO 10
SOUND 100 + INT(RND * 50), .1
NEXT
p1kuiluu = 1
p1kuilux = INT(RND * 560)
LINE (p1kuilux, 400)-(p1kuilux + 80, 480), 0, BF
kuika1 = TIMER
ELSEIF p1$ = "Paagah" THEN
FOR lyy = 320 TO 370 STEP 10
LINE (0, lyy)-(640, lyy), 11 + (INT(RND * 2) * 4)
NEXT
FOR s = 1 TO 7
SOUND 400 + INT(RND * 200), .1
NEXT
FOR lyy = 320 TO 370 STEP 10
LINE (0, lyy)-(640, lyy), 0
NEXT
IF p2hyppaa = 1 AND p2x < 620 THEN
CALL poista2pelaaja
alaku = p2x
FOR p2x = alaku TO 620 STEP 20
CALL tee2pelaaja
oota = TIMER: DO: LOOP WHILE ABS(TIMER - oota) < .01
CALL poista2pelaaja
NEXT
FOR s = 1 TO 14
SOUND 100 + INT(RND * 100), .1
NEXT
p2energia = p2energia - 14 - INT(RND * 8): CALL teeenergiat
END IF
CALL tee1pelaaja: CALL tee2pelaaja
ELSEIF p1$ = "Larry the Glove" THEN
IF p1hyppaa = 1 THEN
CALL poista1pelaaja
p1hyppaa = 0
p1y = 300
END IF
IF p1suojaa = 0 THEN
CALL poista1pelaaja
PSET (p1x - 35, p1y + 99), 0
DRAW "c11nr70m" + STR$(p1x) + "," + STR$(p1y - 20) + "m" + STR$(p1x + 35) + "," + STR$(p1y + 99)
p1suojaa = 1
ELSE
p1suojaa = 0
CALL poista1pelaaja: CALL tee1pelaaja
END IF
ELSEIF p1$ = "SiliconeAss" THEN
CALL poista1pelaaja
restop2x = p2x
restop2y = p2y
restop2$ = p2$
p2x = p1x
p2y = p1y
p2$ = p1$
called = 2: CALL tee2pelaaja
CIRCLE (p2x + 10, p2y + 55), 9, 0, 4.6, 1.5
CIRCLE (p1x + 10, p1y + 55), 112, 13, 4.7, 1.6, .05
FOR s = 1 TO 10
SOUND 100 + INT(RND * 100), .05
NEXT
CIRCLE (p1x + 10, p1y + 55), 112, 0, 4.7, 1.6, .05
p2x = restop2x
p2y = restop2y
p2$ = restop2$
CALL poista1pelaaja: CALL tee1pelaaja
IF ABS(p2x - p1x) < 140 + snak2 AND p2suojaa = 0 AND p1y - p2y < 45 THEN
p2energia = p2energia - 7 - INT(RND * 6)
CALL teeenergiat
END IF
ELSEIF p1$ = "TessaquÇ" THEN
firex = 20 + INT(RND * 600)
FOR firey = 120 TO 360 STEP 10
FOR firer = 1 TO 10 STEP 5
CIRCLE (firex, firey), firer, 10 + INT(RND * 2)
NEXT
NEXT
FOR s = 1 TO 20
SOUND 100 + INT(RND * 100), .05
NEXT
FOR firey = 120 TO 360 STEP 10
FOR firer = 1 TO 10 STEP 5
CIRCLE (firex, firey), firer, 0
NEXT
NEXT
IF ABS(p2x - firex) < 40 AND p2suojaa = 0 THEN
p2energia = p2energia - 8 - INT(RND * 10)
CALL teeenergiat
END IF
IF ABS(p1x - firex) < 40 THEN
p1energia = p1energia - 14 - INT(RND * 10)
CALL teeenergiat
END IF
ELSEIF p1$ = "Slussen HyperSpirit Invisible" THEN
SOUND 1500, .5
CALL poista1pelaaja
p1x = 20 + INT(RND * ABS(p2x - 90))
CALL tee1pelaaja
ELSEIF p1$ = "Zairar" THEN
LINE (p1x - VAL(p1suunta$ + STR$(10)), p1y + 30)-(p1x - VAL(p1suunta$ + STR$(25)), p1y + 50), 0
IF p1x - VAL(p1suunta$ + STR$(10)) < 310 THEN
LINE (p1x - VAL(p1suunta$ + STR$(10)), p1y + 30)-(0, p1y + 30), 11
ELSE
LINE (p1x - VAL(p1suunta$ + STR$(10)), p1y + 30)-(p1x - 310, p1y + 30), 11
END IF
IF p1x - VAL(p1suunta$ + STR$(10)) < 310 THEN
LINE (640, p1y + 30)-(640 - (310 - (p1x - VAL(p1suunta$ + STR$(10)))), p1y + 30), 11
IF (640 - (310 - (p1x - VAL(p1suunta$ + STR$(10))))) < p2x + 20 THEN
imi = 6 + INT(RND * 5)
p2energia = p2energia - imi
p1energia = p1energia + imi
CALL teeenergiat
END IF
END IF
FOR s = 1 TO 6
SOUND 600 + INT(RND * 400), .1
NEXT
LINE (p1x - VAL(p1suunta$ + STR$(10)), p1y + 30)-(0, p1y + 30), 0
LINE (640, p1y + 30)-(640 - (310 - (p1x - VAL(p1suunta$ + STR$(10)))), p1y + 30), 0
CALL teepelaajat
END IF
END SUB

SUB f9
IF p2$ <> "Snailman" AND p2$ <> "Larry the Glove" THEN
handlisa = 0
IF p2$ = "Stefan" OR p2$ = "Willex" OR p2$ = "SiliconeAss" THEN handlisa = 50
IF p2$ = "Paeblo the Old" OR p2$ = "Vilhelmo" THEN handlisa = 40
IF p2$ = "TessaquÇ" OR p2$ = "SmileHead" OR p2$ = "Zairar" THEN handlisa = 60
handvari = POINT(p2x - 10, p2y + 30)
CALL poista2pkasi
LINE (p2x - 10, p2y + 30)-(p2x - 70 - handlisa, p2y + 25), handvari
FOR s = 1 TO 7
SOUND 100 + INT(RND * 50), .1
NEXT
LINE (p2x - 10, p2y + 30)-(p2x - 70 - handlisa, p2y + 25), 0
LINE (p2x - 10, p2y + 30)-(p2x - 30, p2y + 30), handvari
IF p2x <= (p1x + 90 + handlisa + snak1) AND p2y + 30 < p1y + 130 AND p2y + 30 > p1y - 20 AND p1suojaa = 0 THEN
p1energia = p1energia - 4 - INT(RND * 3)
CALL teeenergiat
END IF
ELSEIF p2$ = "Snailman" AND p2suojaa = 0 THEN
PSET (p2x + 3, p2y + 28), 7
DRAW "c7bu42br15 u15 bl10d10 c0 bl16u10bl10d15"
CIRCLE (p2x - 14, p2y - 38), 10, 0
PSET (p2x - 40, p2y + 30), 14
DRAW "c7 l15bd10r14"
CIRCLE (p2x - 65, p2y + 37), 10, 15
FOR silr = 1 TO 3
CIRCLE (p2x - 14 - 5, p2y - 38), silr, 0
NEXT
FOR silr = 1 TO 3
CIRCLE (p2x - 65 - 5, p2y + 37), silr, 3
NEXT

'CIRCLE (p2x - 14, p2y - 38), 10, 15
FOR s = 1 TO 4
SOUND 50 + INT(RND * 50), .1
NEXT

'seur. poistaa lisÑsilimÑn
PSET (p2x - 40, p2y + 30), 0
DRAW "c0 l15bd10r14"
CIRCLE (p2x - 65, p2y + 37), 10, 0
FOR silr = 1 TO 3
CIRCLE (p2x - 65 - 5, p2y + 37), silr, 0
NEXT
CALL poista2pelaaja
CALL tee2pelaaja
IF p1x >= (p2x - 85 - footlisa - snak1) AND p1suojaa = 0 THEN
p1energia = p1energia - 6 - INT(RND * 5)
CALL teeenergiat
END IF
ELSEIF p2$ = "Larry the Glove" AND p2suojaa = 0 THEN
LINE (p2x - 25, p2y - 10)-(p2x + 10, p2y), 0
LINE (p2x + 10, p2y)-(p2x - 25, p2y + 10), 0
LINE (p2x - 25, p2y - 10)-(p2x - 80, p2y), 11
LINE (p2x - 80, p2y)-(p2x - 25, p2y + 10), 11
FOR s = 1 TO 7
SOUND 100 + INT(RND * 200), .1
NEXT
LINE (p2x - 25, p2y - 10)-(p2x - 80, p2y), 0
LINE (p2x - 80, p2y)-(p2x - 25, p2y + 10), 0
LINE (p2x - 25, p2y - 10)-(p2x + 10, p2y), 11
LINE (p2x + 10, p2y)-(p2x - 25, p2y + 10), 11
IF ABS(p1x - p2x) < 110 + INT(snak1 / 3) AND p1suojaa = 0 THEN
p1energia = p1energia - 3 - INT(RND * 6)
CALL teeenergiat
END IF
END IF
END SUB

SUB kuva
PRINT "Unused sub"
END SUB

SUB lalop
CALL poistapelaajat
p1x = 100
p2x = 540
CALL teepelaajat
IF voitti = 1 THEN lausuunta$ = p1suunta$
IF voitti = 2 THEN lausuunta$ = p2suunta$
IF voitti = 1 THEN PSET (p1x + 40, p1y), 0
IF voitti = 2 THEN PSET (p2x - 40, p2y), 0
FOR noot = 1 TO 14
SELECT CASE noot
CASE 1: no$ = "o4l4c"
CASE 2: no$ = "l4c"
CASE 3: no$ = "l4e"
CASE 4: no$ = "l4e"
CASE 5: no$ = "l4g"
CASE 6: no$ = "l4g"
CASE 7: no$ = "l2e"
CASE 8: no$ = "l4f"
CASE 9: no$ = "l4d"
CASE 10: no$ = "o3l4b"
CASE 11: no$ = "l4g"
CASE 12: no$ = "o4l4c"
CASE 13: no$ = "l4e"
CASE 14: no$ = "l2c"
END SELECT
PLAY no$
IF INSTR(no$, "l2") = 0 THEN
DRAW "c15e2r2f2g2l2h2r2urdrudrldl3r4eu18f3dfdfd2fd3br" + lausuunta$ + STR$(5 + (20 * SGN(LEN(lausuunta$))))
ELSE
DRAW "c15e2r2f2g2l2h2r2urdrudrldl3r4eu18c0f3dfdfd2fd3br" + lausuunta$ + STR$(5 + (20 * SGN(LEN(lausuunta$))))
END IF
NEXT
IF voitti = 1 THEN havx = p2x: havy = p2y
IF voitti = 2 THEN havx = p1x: havy = p1y
FOR paket = 1 TO 30
pax = havx + 50 - INT(RND * 100)
pay = havy + 80 - INT(RND * 110)
FOR par = 1 TO 30 + INT(RND * 30)
pvi = 9 + 2 * INT(RND * 2)
IF pvi = 11 THEN pvi = pvi + 3 * INT(RND * 2)
CIRCLE (pax, pay), par, pvi
IF INT(RND * 2) = 1 THEN SOUND 100 + INT(RND * 200), .1
NEXT
opar = par
FOR par = 1 TO opar
CIRCLE (pax, pay), par, 0
NEXT
NEXT
fonnistu = 1
END SUB

SUB mulop
CALL poistapelaajat
p1x = 100
p2x = 540
IF voitti = 1 THEN palku = 0: poppu = 640: peppi = 10: CALL tee2pelaaja: suu$ = p1suunta$
IF voitti = 2 THEN palku = 640: poppu = 0: peppi = -10: CALL tee1pelaaja: suu$ = p2suunta$
FOR psox = palku TO poppu STEP peppi
PSET (psox, 300), 0
DRAW "c3d40l60d40r20 br50r50br50r20u40l60u40l70bf5c9r60d35l60u35r29d35bd10bl25"

PSET (psox + 35 + VAL(suu$ + STR$(17)), 320), 0
DRAW "c15l3u3l3d6r3u3r6u3r3d6l3u3"
CIRCLE (psox - 15, 380), 20, 7
CIRCLE (psox + 85, 380), 20, 7
FOR psor = 17 TO 19
CIRCLE (psox - 15, 380), psor, 1
CIRCLE (psox + 85, 380), psor, 1
NEXT
FOR s = 1 TO 8
SOUND 100 + INT(RND * 200), .1
NEXT
PSET (psox, 300), 0
DRAW "c0d40l60d40r20br50r50br50r20u40l60u40l70bf5c0r60d35l60u35r29d35bd10bl25"
DRAW "c0nd20r10d10nl10br10u10r10bd10nl10d10nl10br10u20r10d20l10u20bu5br3u3br4d3"
PSET (psox + 35 + VAL(suu$ + STR$(17)), 320), 0
DRAW "c0l3u3l3d6r3u3r6u3r3d6l3u3"
FOR psor = 17 TO 20
CIRCLE (psox - 15, 380), psor, 0
CIRCLE (psox + 85, 380), psor, 0
NEXT
NEXT
fonnistu = 1
END SUB

SUB nulop
DIM ketti(10500)
CALL poistapelaajat
p1x = 100
p2x = 540
CALL teepelaajat
IF voitti = 1 THEN lox = 540: laix = 100
IF voitti = 2 THEN lox = 100: laix = 540
FOR tuly = 250 TO 0 STEP -10
PSET (laix, tuly), 0
DRAW "c13r2dl2"
SOUND 500 - tuly, .5
DRAW "c0r2ul2"
NEXT
FOR tuly = 0 TO 300 STEP 10
PSET (lox, tuly), 0
DRAW "c13r2dl2"
SOUND 500 - tuly, .5
DRAW "c0r2ul2"
NEXT
FOR potit = 1 TO 8
pox = lox + 5 - INT(RND * 10)
IF potit = 1 OR potit = 5 THEN kol = 15
IF potit = 2 OR potit = 6 THEN kol = 13
IF potit = 3 OR potit = 7 THEN kol = 13
IF potit = 4 OR potit = 8 THEN kol = 4
FOR por = 1 TO 700 STEP 3 + INT(RND * 5)
CIRCLE (pox, 300 + 5 - INT(RND * 10)), por, kol
IF INT(RND * 3) = 1 THEN SOUND 100 + INT(RND * 100), .1
NEXT
NEXT
fonnistu = 1
END SUB

SUB paketa
IF voitti = 1 THEN havx = p2x: havy = p2y
IF voitti = 2 THEN havx = p1x: havy = p1y
FOR paket = 1 TO 30
pax = havx + 50 - INT(RND * 100)
pay = havy + 80 - INT(RND * 110)
FOR par = 1 TO 30 + INT(RND * 50)
pvi = 12 + 2 * INT(RND * 2)
IF pvi = 14 THEN pvi = pvi + 1 * INT(RND * 2)
CIRCLE (pax, pay), par, pvi
IF INT(RND * 2) = 1 THEN SOUND 100 + INT(RND * 200), .1
NEXT
opar = par
FOR par = 1 TO opar
CIRCLE (pax, pay), par, 0
NEXT
NEXT
END SUB

SUB palop
CALL poistapelaajat
p1x = 280
p2x = 360
CALL teepelaajat
handvari = 12
IF voitti = 1 THEN
FOR ixut = 1 TO 30
CALL poista1pkasi
FOR handy = -30 TO 30 STEP 10
LINE (p1x + 10, p1y + 30)-(p1x + 60, p1y - handy), handvari + 2 * INT(RND * 2)
SOUND 100 + handy, .3
LINE (p1x + 10, p1y + 30)-(p1x + 60, p1y - handy), 0
NEXT
CALL tee2pelaaja: CALL poista1pelaaja: CALL tee1pelaaja
NEXT
CALL tee1pelaaja
CALL poista2pelaaja
FOR paur = 1 TO 70
CIRCLE (p2x, p2y + 40), paur, 4 + (8 * INT(RND * 2))
SOUND 100 + INT(RND * 100), .2
NEXT
FOR paur = 1 TO 70
CIRCLE (p2x, p2y + 40), paur, 0
NEXT
CALL tee1pelaaja
ELSE
FOR ixut = 1 TO 30
CALL poista2pkasi
FOR handy = -30 TO 30 STEP 10
LINE (p2x - 10, p2y + 30)-(p2x - 60, p2y - handy), handvari + 2 * INT(RND * 2)
SOUND 100 + handy, .3
LINE (p2x - 10, p2y + 30)-(p2x - 60, p2y - handy), 0
NEXT
CALL tee1pelaaja: CALL poista2pelaaja: CALL tee2pelaaja
NEXT
CALL tee2pelaaja
CALL poista1pelaaja
FOR paur = 1 TO 70
CIRCLE (p1x, p1y + 40), paur, 4 + (8 * INT(RND * 2))
SOUND 100 + INT(RND * 100), .2
NEXT
FOR paur = 1 TO 70
CIRCLE (p1x, p1y + 40), paur, 0
NEXT
CALL tee2pelaaja
END IF
CALL verenlento
PLAY "mfo1l8gl2f+"
oota = TIMER: DO: LOOP WHILE ABS(TIMER - oota) < 3
fonnistu = 1
END SUB

SUB poista1pelaaja
called = 1
IF p1$ = "Willex" THEN CALL poistawille
IF p1$ = "SmileHead" OR p1$ = "Naigon" THEN CALL poistaerasu
IF p1$ = "Stefan" OR p1$ = "Vilhelmo" OR p1$ = "Paeblo the Old" THEN CALL poistastefan
IF p1$ = "Snailman" OR p1$ = "Murg" THEN CALL poistasnailman
IF p1$ = "Paagah" OR p1$ = "Larry the Glove" THEN CALL poistastefan
IF p1$ = "SiliconeAss" OR p1$ = "TessaquÇ" OR p1$ = "Zairar" THEN CALL poistastefan
IF p1$ = "Slussen HyperSpirit Invisible" THEN LINE (p1x - 35, p1y - 35)-(p1x + 35, p1y + 85), 0, B
END SUB

SUB poista1pjalka
'PSET (p1x + 14, p1y + 64), 0
'DRAW "c0 l27r7d35r5"
PSET (p1x + 8, p1y + 65), 0
DRAW "c0 d34nr5"
END SUB

SUB poista1pkasi
'LINE (p1x - 10, p1y + 30)-(p1x - 25, p1y + 50), 0
LINE (p1x + 10, p1y + 30)-(p1x + 30, p1y + 30), 0
END SUB

SUB poista2pelaaja
called = 2
IF p2$ = "Willex" THEN CALL poistawille
IF p2$ = "SmileHead" OR p2$ = "Naigon" THEN CALL poistaerasu
IF p2$ = "Stefan" OR p2$ = "Vilhelmo" OR p2$ = "Paeblo the Old" THEN CALL poistastefan
IF p2$ = "Snailman" OR p2$ = "Murg" THEN CALL poistasnailman
IF p2$ = "Paagah" OR p2$ = "Larry the Glove" THEN CALL poistastefan
IF p2$ = "SiliconeAss" OR p2$ = "TessaquÇ" OR p2$ = "Zairar" THEN CALL poistastefan
IF p2$ = "Slussen HyperSpirit Invisible" THEN LINE (p2x - 35, p2y - 35)-(p2x + 35, p2y + 85), 0, B
END SUB

SUB poista2pjalka
'PSET (p1x + 14, p1y + 64), 0
'DRAW "c0 l27r7d35r5"
PSET (p2x - 6, p2y + 65), 0
DRAW "c0 d34nl5"
END SUB

SUB poista2pkasi
'LINE (p2x + 10, p2y + 30)-(p2x + 25, p2y + 50), 0
LINE (p2x - 10, p2y + 30)-(p2x - 30, p2y + 30), 0
END SUB

SUB poistaerasu
IF called = 1 THEN
eusuunta$ = ""
erasux = p1x
erasuy = p1y
euvauhti = p1vauhti
END IF
IF called = 2 THEN
eusuunta$ = "-"
erasux = p2x
erasuy = p2y
euvauhti = p2vauhti
END IF
LINE (erasux - 40 - euvauhti, erasuy - 40)-(erasux + 40 + euvauhti, erasuy + 100), 0, BF
END SUB

SUB poistapelaajat
CALL poista1pelaaja
CALL poista2pelaaja
END SUB

SUB poistasnailman
IF called = 1 THEN
snasuunta$ = ""
snax = p1x
snay = p1y
svauhti = p1vauhti
IF p1suojaa = 1 THEN snasuo = 1
END IF
IF called = 2 THEN
snasuunta$ = "-"
snax = p2x
snay = p2y
svauhti = p2vauhti
IF p2suojaa = 1 THEN snasuo = 2
END IF

IF snasuo = 0 THEN LINE (snax - 40 - svauhti, snay - 53)-(snax + 40 + svauhti, snay + 100), 0, BF
IF snasuo <> 0 THEN LINE (snax - 52 - svauhti, snay - 53)-(snax + 52 + svauhti, snay + 100), 0, BF
END SUB

SUB poistastefan
IF called = 1 THEN
stesuunta$ = ""
stex = p1x
stey = p1y
stevauhti = p1vauhti
END IF
IF called = 2 THEN
stesuunta$ = "-"
stex = p2x
stey = p2y
stevauhti = p2vauhti
END IF
LINE (stex - 35 - stevauhti, stey - 45)-(stex + 35 + stevauhti, stey + 99), 0, BF
END SUB

SUB poistaVilhelmo
'kÑyttÑmÑtîn subbi
END SUB

SUB poistawille
IF called = 1 THEN
wilsuunta$ = ""
wilx = p1x
wily = p1y
wvauhti = p1vauhti
END IF
IF called = 2 THEN
wilsuunta$ = "-"
wilx = p2x
wily = p2y
wvauhti = p2vauhti
END IF
LINE (wilx - 35 - wvauhti, wily - 45)-(wilx + 35 + wvauhti, wily + 100), 0, BF
END SUB

SUB polop
CALL poistapelaajat
p1x = 100
p2x = 540
CALL teepelaajat
IF voitti = 1 THEN
LINE (p1x - 30, p1y + 60)-(p1x + 30, p1y + 99), 0, BF
jaax = p1x - 10
jappi = 5
alaku = 0: loppis = 640
ELSE
LINE (p2x - 30, p2y + 60)-(p2x + 30, p2y + 99), 0, BF
jaax = p2x - 10
jappi = -5
alaku = 640: loppis = 0
END IF
FOR jalx = alaku TO loppis STEP jappi
LINE (jaax + 3, 360)-(jalx, 398), 10
LINE (jaax + 20, 360)-(jalx + 20, 398), 10
SOUND 200 + INT(RND * 200), .2
SOUND 200 + INT(RND * 200), .2
LINE (jaax + 3, 360)-(jalx, 398), 0
LINE (jaax + 20, 360)-(jalx + 20, 398), 0
NEXT
FOR jalx = loppis TO alaku STEP -jappi
LINE (jaax + 3, 360)-(jalx, 398), 10
LINE (jaax + 20, 360)-(jalx + 20, 398), 10
SOUND 200 + INT(RND * 200), .2
SOUND 200 + INT(RND * 200), .2
LINE (jaax + 3, 360)-(jalx, 398), 0
LINE (jaax + 20, 360)-(jalx + 20, 398), 0
NEXT
CALL teepelaajat
FOR paali = 60 TO 1 STEP -1
PALETTE 0, paali
SOUND 100 + INT(RND * 100), .1
NEXT
fonnistu = 1
END SUB

SUB shlop
CALL poistapelaajat
p1x = 200
p2x = 440
CALL teepelaajat
              
IF voitti = 1 THEN
FOR pax = 0 TO 640 STEP 20
FOR paat = 1 TO 2
poko = 10 + INT(RND * 20)
pay = 300 + INT(RND * 90)
FOR par = 1 TO poko
CIRCLE (pax, pay), par, 9 + INT(RND * 7)
SOUND 100 + INT(RND * 100), .1
NEXT
FOR par = 1 TO poko
CIRCLE (pax, pay), par, 0
NEXT
NEXT
NEXT
alx = p2x
yskin = 30
CALL poista2pelaaja
FOR p2x = alx TO 640 STEP 5'585 STEP 5
IF p2x < 590 THEN p2y = p2y - yskin: yskin = yskin - 2
IF p2x >= 590 THEN p2y = p2y + yskin: yskin = yskin + 2
CALL tee2pelaaja
SOUND 800 - yskin * 10, 1.2
CALL poista2pelaaja
NEXT
ELSE
FOR pax = 640 TO 0 STEP -20
FOR paat = 1 TO 2
poko = 10 + INT(RND * 20)
pay = 300 + INT(RND * 90)
FOR par = 1 TO poko
CIRCLE (pax, pay), par, 9 + INT(RND * 7)
SOUND 100 + INT(RND * 100), .1
NEXT
FOR par = 1 TO poko
CIRCLE (pax, pay), par, 0
NEXT
NEXT
NEXT
alx = 200
yskin = 30
CALL poista1pelaaja
FOR p1x = alx TO 0 STEP -5
IF p1x > 50 THEN p1y = p1y - yskin: yskin = yskin - 2
IF p1x <= 50 THEN p1y = p1y + yskin: yskin = yskin + 2
CALL tee1pelaaja
SOUND 800 - yskin * 10, 1.2
CALL poista1pelaaja
NEXT
END IF
LINE (0, 0)-(640, 480), 15, BF
FOR s = 600 TO 100 STEP -1
SOUND 50 + INT(RND * s), .1
NEXT
CLS : oota = TIMER: DO: LOOP WHILE ABS(TIMER - oota) < 3
fonnistu = 1
END SUB

SUB silop
CALL poistapelaajat
p1x = 270
p2x = 370
CALL teepelaajat
IF voitti = 1 THEN
CALL poista1pelaaja
restop2x = p2x
restop2y = p2y
restop2$ = p2$
p2x = p1x
p2y = p1y
p2$ = p1$
called = 2: CALL tee2pelaaja
CIRCLE (p2x + 10, p2y + 55), 9, 0, 4.6, 1.5
FOR monta = 1 TO 6
FOR per = 1 TO 40
CIRCLE (p2x + 15 + per, p2y + 60), per, 9 + INT(RND * 6)
SOUND 100 + INT(RND * 200), .3
CIRCLE (p2x + 15 + per, p2y + 60), per, 0
NEXT
NEXT
FOR pex = p2x + 15 TO 640 STEP 10
FOR pittaa = 1 TO 30
CIRCLE (pex + per, p2y + 60), per, 9 + INT(RND * 6)
SOUND 100 + INT(RND * 100), .1
NEXT
CIRCLE (pex + per, p2y + 60), per, 0
NEXT
FOR pex = 640 TO p2x + 15 STEP -10
FOR pittaa = 1 TO 20
CIRCLE (pex + per, p2y + 60), per, 9 + INT(RND * 6)
SOUND 100 + INT(RND * 100), .1
NEXT
CIRCLE (pex + per, p2y + 60), per, 0
NEXT
FOR s = 400 TO 700 STEP 10
SOUND s, .1
NEXT
CALL tee2pelaaja
PLAY "p2p8"
p2x = restop2x
p2y = restop2y
p2$ = restop2$
CALL poista1pelaaja: CALL tee1pelaaja
ELSE
CALL poista2pelaaja
restop1x = p1x
restop1y = p1y
restop1$ = p1$
p1x = p2x
p1y = p2y
p1$ = p2$
called = 1: CALL tee1pelaaja
CIRCLE (p1x - 10, p1y + 55), 9, 0, 1.7, 4.6
FOR monta = 1 TO 6
FOR per = 1 TO 40
CIRCLE (p1x - 15 - per, p1y + 60), per, 9 + INT(RND * 6)
SOUND 100 + INT(RND * 200), .3
CIRCLE (p1x - 15 - per, p1y + 60), per, 0
NEXT
NEXT
FOR pex = p1x - 15 TO 0 STEP -10
FOR pittaa = 1 TO 30
CIRCLE (pex - per, p1y + 60), per, 9 + INT(RND * 6)
SOUND 100 + INT(RND * 100), .1
NEXT
CIRCLE (pex - per, p1y + 60), per, 0
NEXT
FOR pex = 0 TO p1x - 15 STEP 10
FOR pittaa = 1 TO 20
CIRCLE (pex - per, p1y + 60), per, 9 + INT(RND * 6)
SOUND 100 + INT(RND * 100), .1
NEXT
CIRCLE (pex - per, p1y + 60), per, 0
NEXT
FOR s = 400 TO 700 STEP 10
SOUND s, .1
NEXT
CALL tee1pelaaja
PLAY "p2p8"
p1x = restop1x
p1y = restop1y
p1$ = restop1$
CALL poista2pelaaja: CALL tee2pelaaja

END IF
fonnistu = 1
END SUB

SUB snlop
CALL poistapelaajat
IF voitti = 1 THEN p2x = 500: CALL tee2pelaaja
IF voitti = 2 THEN p1x = 140: CALL tee1pelaaja
snax = 320
snay = 280
FOR ruz = 1 TO 5
sko = 60
FOR litte = 3 TO .1 STEP -.05
IF litte <= 1 THEN sko = sko + 8
CIRCLE (snax, snay + 40), sko, 14, , , litte
SOUND 300 + INT(RND * 100), .2
IF voitti = 1 THEN CALL tee2pelaaja
IF voitti = 2 THEN CALL tee1pelaaja
IF litte < .15 AND litte > 0 AND ruz <> 5 THEN
IF voitti = 1 THEN
FOR osur = 1 TO 15
CIRCLE (470, 330), osur, 14 + INT(RND * 2)
NEXT
FOR s = 1 TO 30: SOUND 300 + INT(RND * 100), .1: NEXT
FOR osur = 1 TO 15
CIRCLE (470, 330), osur, 0
NEXT
ELSE
FOR osur = 1 TO 15
CIRCLE (160, 330), osur, 14 + INT(RND * 2)
NEXT
FOR s = 1 TO 30: SOUND 300 + INT(RND * 100), .1: NEXT
FOR osur = 1 TO 15
CIRCLE (160, 330), osur, 0
NEXT
END IF
END IF
CIRCLE (snax, snay + 40), sko, 0, , , litte
CIRCLE (snax - 17, snay + 30), 10, 15
CIRCLE (snax + 17, snay + 30), 10, 15
FOR silr = 1 TO 3
CIRCLE (snax - 15, snay + 30), silr, 3
CIRCLE (snax + 15, snay + 30), silr, 3
NEXT
NEXT
NEXT
CIRCLE (snax - 17, snay + 30), 10, 0
CIRCLE (snax + 17, snay + 30), 10, 0
FOR silr = 1 TO 3
CIRCLE (snax - 15, snay + 30), silr, 0
CIRCLE (snax + 15, snay + 30), silr, 0
NEXT
IF voitti = 1 THEN CALL tee1pelaaja
IF voitti = 2 THEN CALL tee2pelaaja
CALL paketa
fonnistu = 1
END SUB

SUB stlop
CALL poistapelaajat
IF voitti = 1 THEN
p1x = 50
p2x = 170
CALL teepelaajat
FOR p1x = 50 TO 550 STEP 20
footvari = 9
footlisa = 80
CALL poista1pjalka
LINE (p1x - 12, p1y + 65)-(p1x + 60 + footlisa, p1y + 63), footvari: DRAW "c" + STR$(footvari) + "u5"
FOR s = 1 TO 6
SOUND 50 + INT(RND * 50), .1
NEXT
LINE (p1x - 12, p1y + 65)-(p1x + 60 + footlisa, p1y + 63), 0: DRAW "c0u5"
CALL tee2pelaaja: CALL tee1pelaaja
FOR s = 1 TO 12
SOUND 50 + INT(RND * 50), .1
NEXT
CALL poista1pelaaja: CALL poista2pelaaja: p2x = p1x + 120
NEXT
ELSE
p1x = 470
p2x = 590
CALL teepelaajat
FOR p2x = 590 TO 90 STEP -20
footvari = 9
footlisa = 80
CALL poista2pjalka
LINE (p2x + 4, p2y + 65)-(p2x - 60 - footlisa, p2y + 63), footvari: DRAW "c" + STR$(footvari) + "u5"
FOR s = 1 TO 6
SOUND 50 + INT(RND * 50), .1
NEXT
LINE (p2x + 4, p2y + 65)-(p2x - 60 - footlisa, p2y + 63), 0: DRAW "c0u5"
CALL tee1pelaaja: CALL tee2pelaaja
FOR s = 1 TO 12
SOUND 50 + INT(RND * 50), .1
NEXT
CALL poista2pelaaja: CALL poista1pelaaja: p1x = p2x - 120
NEXT
END IF
IF voitti = 1 THEN havx = p2x - 30: havy = p2y
IF voitti = 2 THEN havx = p1x + 30: havy = p1y
FOR paket = 1 TO 30
pax = havx + 50 - INT(RND * 100)
pay = havy + 80 - INT(RND * 110)
FOR par = 1 TO 30 + INT(RND * 50)
pvi = 9 + 2 * INT(RND * 2)
IF pvi = 11 THEN pvi = pvi + 4 * INT(RND * 2)
CIRCLE (pax, pay), par, pvi
IF INT(RND * 2) = 1 THEN SOUND 100 + INT(RND * 200), .1
NEXT
opar = par
FOR par = 1 TO opar
CIRCLE (pax, pay), par, 0
NEXT
NEXT
fonnistu = 1
END SUB

SUB tee1pelaaja
IF p1y > 300 AND putii = 0 THEN p1y = 300
called = 1
IF p1$ = "Willex" THEN CALL teewille
IF p1$ = "SmileHead" THEN CALL teeerasu
IF p1$ = "Stefan" THEN CALL teestefan
IF p1$ = "Vilhelmo" THEN CALL teeVilhelmo
IF p1$ = "Naigon" THEN CALL teenerasu
IF p1$ = "Snailman" THEN CALL teesnailman
IF p1$ = "Murg" THEN CALL teeMurg
IF p1$ = "Paeblo the Old" THEN CALL teepaeppa
IF p1$ = "Paagah" THEN CALL teepaava
IF p1$ = "Larry the Glove" THEN CALL teelaavi
IF p1$ = "SiliconeAss" THEN CALL teesiliconiperse
IF p1$ = "TessaquÇ" THEN CALL teetessuk
IF p1$ = "Zairar" THEN CALL teezairar
IF nayt = 0 AND p2kuiluu = 0 AND p1kuiluu = 0 AND p1hyppaa = 0 AND ((POINT(p1x - 20, 403) = 0 OR POINT(p1x + 20, 403) = 0)) THEN CALL teekentta

END SUB

SUB tee2pelaaja
IF p2y > 300 AND putii = 0 THEN p2y = 300
called = 2
IF p2$ = "Willex" THEN CALL teewille
IF p2$ = "SmileHead" THEN CALL teeerasu
IF p2$ = "Stefan" THEN CALL teestefan
IF p2$ = "Vilhelmo" THEN CALL teeVilhelmo
IF p2$ = "Naigon" THEN CALL teenerasu
IF p2$ = "Snailman" THEN CALL teesnailman
IF p2$ = "Murg" THEN CALL teeMurg
IF p2$ = "Paeblo the Old" THEN CALL teepaeppa
IF p2$ = "Paagah" THEN CALL teepaava
IF p2$ = "Larry the Glove" THEN CALL teelaavi
IF p2$ = "SiliconeAss" THEN CALL teesiliconiperse
IF p2$ = "TessaquÇ" THEN CALL teetessuk
IF p2$ = "Zairar" THEN CALL teezairar
IF nayt = 0 AND p2kuiluu = 0 AND p1kuiluu = 0 AND p2hyppaa = 0 AND ((POINT(p2x - 20, 403) = 0 OR POINT(p2x + 20, 403) = 0)) THEN CALL teekentta

END SUB

SUB teeaika
COLOR 11
LOCATE 3, 35: PRINT "           "
aika$ = "Time: " + STR$(printattavaaika - 10)
LOCATE 3, 40 - INT((LEN(aika$) / 2)) + 1
PRINT aika$
END SUB

SUB teeenergiat
IF p1energia > 400 THEN p1energia = 400
IF p2energia > 400 THEN p2energia = 400
IF p1energia <= 0 OR p2energia <= 0 THEN PALETTE: GOTO eloppu
LINE (0, 25)-(640, 62), 0, BF
LINE (234 - INT(p1energia / 2), 30)-(234, 60), 4, BF
LINE (406, 30)-(406 + INT(p2energia / 2), 60), 4, BF
CALL teeaika
EXIT SUB
eloppu:
IF tapetaan = 0 THEN
LINE (p1x - 35, p1y - 35)-(p1x + 35, p1y + 85), 0, B
LINE (p2x - 35, p2y - 35)-(p2x + 35, p2y + 85), 0, B
p1jahm = 0
p2jahm = 0
p1suojaa = 0
p2suojaa = 0
p1kuiluu = 0
p2kuiluu = 0
IF p1energia <= 0 THEN voit$ = p2$: hav$ = p1$: voitti = 2: havx = p1x: havy = p1y: voitx = p2x
IF p2energia <= 0 THEN voit$ = p1$: hav$ = p2$: voitti = 1: havx = p2x: havy = p2y: voitx = p1x
LINE (0, 0)-(640, 100), 0, BF
'CALL teeaika
CLS
IF p2energia <= 0 THEN p1y = 300: p2y = 300: CALL teekentta: CALL tee1pelaaja: CALL tee2pelaaja
IF p1energia <= 0 THEN p2y = 300: p1y = 300: CALL teekentta: CALL tee2pelaaja: CALL tee1pelaaja
PLAY "mfo4l8gl4f+": LOCATE 8, 30: COLOR 12: PRINT "Finish that bastard !!!!"
tiimer = TIMER: DO: LOOP WHILE ABS(TIMER - tiimer) < 1.4
LOCATE 8, 30: COLOR 12: PRINT "                            "
faika = TIMER
'fonnistu ilmottaa onnistuxesta
DO
IF ABS(TIMER - praika) > .1 THEN LOCATE 8, 32: COLOR 12 + INT(RND * 3): PRINT "Finish se surkimus !!!!": praika = TIMER
IF LEFT$(voit$, 3) = "Smi" THEN LOCATE 8, 32: PRINT "                       ": CALL eulop
IF LEFT$(voit$, 3) = "Wil" THEN LOCATE 8, 32: PRINT "                       ": CALL wilop
IF LEFT$(voit$, 3) = "Nai" THEN LOCATE 8, 32: PRINT "                       ": CALL nulop
IF LEFT$(voit$, 3) = "Ste" THEN LOCATE 8, 32: PRINT "                       ": CALL stlop
IF LEFT$(voit$, 3) = "Vil" THEN LOCATE 8, 32: PRINT "                       ": CALL vilop
IF LEFT$(voit$, 3) = "Sna" THEN LOCATE 8, 32: PRINT "                       ":   CALL snlop
IF LEFT$(voit$, 3) = "Mur" THEN LOCATE 8, 32: PRINT "                       ":   CALL mulop
IF LEFT$(voit$, 3) = "Pae" THEN LOCATE 8, 32: PRINT "                       ":   CALL palop
IF LEFT$(voit$, 3) = "Paa" THEN LOCATE 8, 32: PRINT "                       ": CALL polop
IF LEFT$(voit$, 3) = "Lar" THEN LOCATE 8, 32: PRINT "                       ": CALL lalop
IF LEFT$(voit$, 3) = "Sil" THEN LOCATE 8, 32: PRINT "                       ": CALL silop
IF LEFT$(voit$, 3) = "Tes" THEN LOCATE 8, 32: PRINT "                       ": CALL telop
IF LEFT$(voit$, 3) = "Slu" THEN LOCATE 8, 32: PRINT "                       ": CALL shlop
IF LEFT$(voit$, 3) = "Zai" THEN LOCATE 8, 32: PRINT "                       ": CALL zalop
IF fonnistu = 1 THEN EXIT DO
LOOP WHILE ABS(TIMER - faika) < 5
IF fonnistu = 0 THEN LOCATE 8, 32: PRINT "                       ": SOUND 100, 10: CALL paketa
CLS
IF p1energia > 0 THEN p1y = 300: CALL teekentta: CALL tee1pelaaja
IF p2energia > 0 THEN p2y = 300: CALL teekentta: CALL tee2pelaaja
IF LEFT$(voit$, 3) = "Smi" THEN
sanoo$ = "Don't my teeth look shiny and white?"
ELSEIF LEFT$(voit$, 3) = "Wil" THEN
sanoo$ = "I just like that brown smelly stuff."
ELSEIF LEFT$(voit$, 3) = "Nai" THEN
sanoo$ = "Gee I'm beautiful!"
ELSEIF LEFT$(voit$, 3) = "Ste" THEN
sanoo$ = "Mysterious Stefan of Night won again!"
ELSEIF LEFT$(voit$, 3) = "Vil" THEN
sanoo$ = "Kinda cool! But now I'm gonna go surfing."
ELSEIF LEFT$(voit$, 3) = "Sna" THEN
sanoo$ = "Well, everyone knows we slugs are the best fighters in the world."
ELSEIF LEFT$(voit$, 3) = "Mur" THEN
sanoo$ = "Did I win? What? I don't hear anything! Is this year 1863?"
ELSEIF LEFT$(voit$, 3) = "Zai" THEN
sanoo$ = "You can feel the funky atmosphere when Zairar is near!"
ELSEIF LEFT$(voit$, 3) = "Pae" THEN
sanoo$ = "Fighting keeps a man from getting old! Errr, heh..."
IF hav$ = "Willex" THEN sanoo$ = "Wow! Willex was 100% shit!"
IF hav$ = "SiliconeAss" THEN sanoo$ = "Yack, what smelling silicone!"
ELSEIF LEFT$(voit$, 3) = "Paa" THEN
sanoo$ = "Use Thin-O-Matic pills! I was 150 kilos and now I have anorexia!"
ELSEIF LEFT$(voit$, 3) = "Lar" THEN
sanoo$ = "Larry the Glove, simply the best voice in Slussen."
ELSEIF LEFT$(voit$, 3) = "Sil" THEN
sanoo$ = "Did my plastic suit get dirty?"
ELSEIF LEFT$(voit$, 3) = "Tes" THEN
sanoo$ = "Whatta fight! Could someone give me a delicious bone?"
ELSEIF LEFT$(voit$, 3) = "Slu" THEN
sanoo$ = "Invisible is invicible!"
END IF
IF voitti = 1 THEN voitx = p1x
IF voitti = 2 THEN voitx = p2x
CIRCLE (320, 135), LEN(sanoo$) * 4.5, 15, , , .2
LINE (voitx, 230)-(320, 135 + (LEN(sanoo$) - (LEN(sanoo$) / 10))), 15
LOCATE 9, 41 - ((LEN(sanoo$)) / 2): COLOR 15: PRINT sanoo$
PLAY "mfo3L16dd+el8>c<l16e>l8c<l16e>l4cl16dd+ecdecl8dl8cg>cp8"
CLS
FOR vix = 0 TO 640 STEP 8
LINE (320, 0)-(vix, 480), 1
LINE (320, 480)-(vix, 0), 1
NEXT
FOR viy = 0 TO 480 STEP 4
LINE (0, viy)-(320, 240), 1
LINE (320, 240)-(640, viy), 1
NEXT
IF p1energia <= 0 THEN
pr1$ = "Winner: " + p2$ + " (Player Two)"
pr2$ = "Loser: " + p1$ + " (Player One)"
COLOR 10
LOCATE 14, 40 - INT(LEN(pr1$) / 2): PRINT pr1$
LOCATE 15, 40 - INT(LEN(pr2$) / 2): PRINT pr2$
END IF
IF p2energia <= 0 THEN
pr1$ = "Winner: " + p1$ + " (Player One)"
pr2$ = "Loser: " + p2$ + " (Player Two)"
COLOR 10
LOCATE 14, 40 - INT(LEN(pr1$) / 2): PRINT pr1$
LOCATE 15, 40 - INT(LEN(pr2$) / 2): PRINT pr2$
END IF
COLOR 15
LOCATE 22, 29: PRINT "Play Again?!  Y=Yes, N=No"
tapetaan = 1
DO: B$ = INKEY$: LOOP WHILE UCASE$(B$) <> "Y" AND UCASE$(B$) <> "N"
IF UCASE$(B$) = "Y" THEN CLS : PLAY "o3l32ceff+g": loppaa = 1: EXIT SUB
CLS : SOUND 60, 10: END
END IF
END SUB

SUB teeerasu
IF called = 1 THEN
eusuunta$ = ""
erasux = p1x
erasuy = p1y
END IF
IF called = 2 THEN
eusuunta$ = "-"
erasux = p2x
erasuy = p2y
END IF

'SmileHead
CIRCLE (erasux, erasuy), 30, 12 'pÑÑ
LINE (erasux - 10, erasuy + 29)-(erasux - 14, erasuy + 64), 2'muu
LINE (erasux + 10, erasuy + 29)-(erasux + 14, erasuy + 64), 2
DRAW "c2 l27r7d35r" + eusuunta$ + "5"
PSET (erasux + 8, erasuy + 64), 2
DRAW "c2 d35nr" + eusuunta$ + "5"
DRAW "c15bu60bl36bu30r57l10nd14l10nd20l10nd21l10nd19l10nd12 " 'naama
silmalku = erasux
IF eusuunta$ = "" THEN
silmalku = silmalku + VAL(eusuunta$ + STR$(1))
CIRCLE (silmalku, erasuy - 5), 8, 10
CIRCLE (silmalku + 20, erasuy - 5), 8, 10
CIRCLE (silmalku + VAL(eusuunta$ + STR$(2)), erasuy - 5), 2, 10
CIRCLE (silmalku + VAL(eusuunta$ + STR$(22)), erasuy - 5), 2, 10
ELSE
CIRCLE (silmalku + VAL(eusuunta$ + STR$(2)) + 2, erasuy - 5), 8, 10
CIRCLE (silmalku + VAL(eusuunta$ + STR$(22)) + 2, erasuy - 5), 8, 10
CIRCLE (silmalku + VAL(eusuunta$ + STR$(2)), erasuy - 5), 2, 10
CIRCLE (silmalku + VAL(eusuunta$ + STR$(22)), erasuy - 5), 2, 10
END IF
PSET (erasux, erasuy - 30), 6
DRAW "c14 r" + eusuunta$ + "25"
IF eusuunta$ = "" THEN DRAW "e4h4l5"
IF eusuunta$ = "-" THEN DRAW "h4e4r5"
'hÑnz
LINE (erasux - VAL(eusuunta$ + STR$(10)), erasuy + 30)-(erasux - VAL(eusuunta$ + STR$(25)), erasuy + 50), 2
LINE (erasux + VAL(eusuunta$ + STR$(10)), erasuy + 30)-(erasux + VAL(eusuunta$ + STR$(30)), erasuy + 30), 2
END SUB

SUB teekaikki
CALL teekentta
CALL teepelaajat
CALL teeenergiat
CALL teeaika
COLOR 9
LOCATE 1, 5: PRINT p1$
LOCATE 1, 79 - LEN(p2$) - 2: PRINT p2$
DRAW "bm34,65c10r50c2r50c14r50c4r50bl200bd5c10d5br2u5r3d5l3r3br2u5r3d5l3br190c4r3u5l3d5"
DRAW "c4bm406,65r50c14r50c2r50c10r50bd5d5l3u5r3l3bl2d5l3u5r3l3bl2d5bl190c4r3u5l3d5"
END SUB

SUB teekentta
'CLS
LINE (0, 400)-(640, 480), 8, BF
LINE (0, 400)-(640, 400), 7
LINE (0, 402)-(640, 402), 7
LINE (0, 428)-(640, 428), 7
LINE (0, 430)-(640, 430), 7
LINE (0, 400)-(640, 400), 0
FOR lahto = 10 TO 580 + (580 / 4) STEP 580 / 4
LINE (lahto, 435)-(lahto + 40, 435), 7
FOR uurrex = lahto TO lahto + 40 STEP 4
LINE (uurrex, 440)-(uurrex, 480), 7
NEXT
NEXT
FOR korx = 0 TO 640 STEP 20
DRAW "c7bm" + STR$(korx) + ",420e10f10"
NEXT
IF p1kuiluu = 1 THEN LINE (p1kuilux, 400)-(p1kuilux + 80, 480), 0, BF
IF p2kuiluu = 1 THEN LINE (p2kuilux, 400)-(p2kuilux + 80, 480), 0, BF
END SUB

SUB teelaavi
IF called = 1 THEN
laasuunta$ = ""
laax = p1x
laay = p1y
IF p1suojaa = 1 THEN laasuojaa = 1
END IF
IF called = 2 THEN
laasuunta$ = "-"
laax = p2x
laay = p2y
IF p2suojaa = 1 THEN laasuojaa = 2
END IF

IF laasuojaa = 1 OR laasuojaa = 2 THEN
PSET (laax - 35, laay + 99), 0
DRAW "c11nr70m" + STR$(laax) + "," + STR$(laay - 20) + "m" + STR$(laax + 35) + "," + STR$(laay + 100)
ELSE
'laavii laavii laapaaneeen....
LINE (laax - VAL(laasuunta$ + STR$(25)), laay - 30)-(laax + VAL(laasuunta$ + STR$(25)), laay - 20), 11
DRAW "d10"
LINE (laax + VAL(laasuunta$ + STR$(25)), laay - 10)-(laax - VAL(laasuunta$ + STR$(10)), laay), 11
LINE (laax - VAL(laasuunta$ + STR$(10)), laay)-(laax + VAL(laasuunta$ + STR$(25)), laay + 10), 11
IF laasuunta$ = "" THEN DRAW "d10g10"
IF laasuunta$ = "-" THEN DRAW "d10f10"
DRAW "m " + STR$(laax + VAL(laasuunta$ + STR$(30))) + ", " + STR$(laay + 99)
DRAW "l" + laasuunta$ + "60m" + STR$(laax - VAL(laasuunta$ + STR$(25))) + "," + STR$(laay - 30)
CIRCLE (laax, laay - 15), 5, 3
CIRCLE (laax + VAL(laasuunta$ + STR$(2)), laay - 15), 2, 9
' L
PSET (laax + VAL(laasuunta$ + STR$(5)), laay + 50), 5
DRAW "c5d10r5dl6u11"
END IF

END SUB

SUB teeMurg
IF called = 1 THEN
musuunta$ = ""
Murgx = p1x
Murgy = p1y
END IF
IF called = 2 THEN
musuunta$ = "-"
Murgx = p2x
Murgy = p2y
END IF

'pÑÑ
PSET (Murgx, Murgy), 0
DRAW "c7bh30br5r50d10c12d35g10l30h10u35c7u10bd45br15bd11"
'kaulahelmet
IF nopeaMode = 0 THEN
IF musuunta$ = "" THEN
FOR helmet = 1 TO 4
DRAW "c15g2f2e2h2 bf4 "
NEXT
FOR helmet = 1 TO 6
DRAW "c15g2f2e2h2 be3bl2"
NEXT
ELSE
FOR helmet = 1 TO 6
DRAW "c15g2f2e2h2 bf3bl2"
NEXT
FOR helmet = 1 TO 5
IF helmet = 4 THEN helisa = 1
DRAW "c15g2f2e2h2 br4bu" + STR$(4 + helisa)
NEXT
END IF
ELSE
DRAW "c15bl3f13e13"
END IF
LINE (Murgx - 12, Murgy + 29)-(Murgx - 18, Murgy + 94), 14'muu
LINE (Murgx + 12, Murgy + 29)-(Murgx + 18, Murgy + 94), 14
'jalitsut
IF musuunta$ = "" THEN
DRAW "c14l35r10bdc2d3ru3d3r5br10bu3d3ru3d3r5"
ELSE
DRAW "c14bl35r35l10bdc2d3lu3d3l5bl10bu3d3lu3d3l5"
END IF
silmalku = Murgx
IF musuunta$ = "" THEN
silmalku = silmalku + VAL(musuunta$ + STR$(1))
CIRCLE (silmalku, Murgy - 5), 5, 7
CIRCLE (silmalku + 20, Murgy - 5), 5, 7
ELSE
CIRCLE (silmalku + VAL(musuunta$ + STR$(2)) + 2, Murgy - 5), 5, 7
CIRCLE (silmalku + VAL(musuunta$ + STR$(22)) + 2, Murgy - 5), 5, 7
END IF
'hÑnds
LINE (Murgx - VAL(musuunta$ + STR$(10)), Murgy + 30)-(Murgx - VAL(musuunta$ + STR$(25)), Murgy + 50), 14
LINE (Murgx + VAL(musuunta$ + STR$(10)), Murgy + 30)-(Murgx + VAL(musuunta$ + STR$(30)), Murgy + 30), 14
'the nenÑ
PSET (Murgx + VAL(musuunta$ + STR$(12)) + (1 * LEN(musuunta$)), Murgy), 12
DRAW "c12 d8l" + musuunta$ + "3"
'lasit  -(0)--(0)-
'           Ô`
'ja suu   (iiii)
PSET (Murgx + VAL(musuunta$ + STR$(6)) + (1 * LEN(musuunta$)), Murgy - 5), 3
DRAW "c3r" + musuunta$ + "10bl" + musuunta$ + "20l" + musuunta$ + "15"
END SUB

SUB teenerasu
IF called = 1 THEN
neusuunta$ = ""
nerasux = p1x
nerasuy = p1y
END IF
IF called = 2 THEN
neusuunta$ = "-"
nerasux = p2x
nerasuy = p2y
END IF

'Naigon
CIRCLE (nerasux, nerasuy), 30, 12 'pÑÑ
LINE (nerasux - 10, nerasuy + 29)-(nerasux - 14, nerasuy + 64), 13'muu
LINE (nerasux + 10, nerasuy + 29)-(nerasux + 14, nerasuy + 64), 13
DRAW "c13l27r7d35r" + neusuunta$ + "5"
PSET (nerasux + 8, nerasuy + 64), 13
DRAW "c13d35nr" + neusuunta$ + "5"
DRAW "c15bu60bl36bu30r57l10nd14l10nd20l10nd21l10nd19l10nd12 " 'naama
'silmÑt
silmalku = nerasux
IF neusuunta$ = "" THEN
silmalku = silmalku + VAL(neusuunta$ + STR$(1))
CIRCLE (silmalku, nerasuy - 5), 8, 7
'DRAW "c7bu8bl2nu4br2nu3br2nu3"
CIRCLE (silmalku + 20, nerasuy - 5), 8, 7
'DRAW "c7bu8bl2nu4br2nu3br2nu3"
CIRCLE (silmalku + VAL(neusuunta$ + STR$(2)), nerasuy - 5), 2, 3
CIRCLE (silmalku + VAL(neusuunta$ + STR$(22)), nerasuy - 5), 2, 3
'PSET (silmalku + VAL(neusuunta$ + STR$(2)), nerasuy - 5), 3
'PSET (silmalku + VAL(neusuunta$ + STR$(22)), nerasuy - 5), 3
ELSE
CIRCLE (silmalku + VAL(neusuunta$ + STR$(2)) + 2, nerasuy - 5), 8, 7
'DRAW "c7bu8bl2nu4br2nu3br2nu3"
CIRCLE (silmalku + VAL(neusuunta$ + STR$(22)) + 2, nerasuy - 5), 8, 7
'DRAW "c7bu8bl2nu4br2nu3br2nu3"
CIRCLE (silmalku + VAL(neusuunta$ + STR$(2)), nerasuy - 5), 2, 3
CIRCLE (silmalku + VAL(neusuunta$ + STR$(22)), nerasuy - 5), 2, 3
'PSET (silmalku + VAL(neusuunta$ + STR$(2)), nerasuy - 5), 3
'PSET (silmalku + VAL(neusuunta$ + STR$(22)), nerasuy - 5), 3
END IF
PSET (nerasux, nerasuy - 30), 14
DRAW "c14 r" + neusuunta$ + "25"
IF neusuunta$ = "" THEN DRAW "e4h4l3f2g2l35dgdgdgg15dgdgdgd10g2h2f5e3u10eueueuueeueeue7rererer3er5"'bu2p6 ,6 "
IF neusuunta$ = "-" THEN DRAW "brh4e4r3g2f2r35dfdfdff15dfdfdfd10f2e2g5h3u10huhuhuhhuuhhuh7lhlhlhl3hl5h"'bu2p6 ,6 "
'hÑnz
LINE (nerasux - VAL(neusuunta$ + STR$(10)), nerasuy + 30)-(nerasux - VAL(neusuunta$ + STR$(25)), nerasuy + 50), 13
LINE (nerasux + VAL(neusuunta$ + STR$(10)), nerasuy + 30)-(nerasux + VAL(neusuunta$ + STR$(30)), nerasuy + 30), 13
'helma
PSET (nerasux - VAL(neusuunta$ + STR$(14)), nerasuy + 62), 13
DRAW "c13r" + neusuunta$ + "28"
END SUB

SUB teepaava
IF called = 1 THEN
pvosuunta$ = ""
pvox = p1x
pvoy = p1y
END IF
IF called = 2 THEN
pvosuunta$ = "-"
pvox = p2x
pvoy = p2y
END IF

'Paagah
'pÑÑ
CIRCLE (pvox, pvoy), 34, 14, , , .6'pÑÑ
'jalkamîykyt
PSET (pvox + 14, pvoy + 20), 0
DRAW "bl20c10d79r" + pvosuunta$ + "5"
PSET (pvox + 8, pvoy + 20), 9
DRAW "c10d79nr" + pvosuunta$ + "5"
'simlÑt
CIRCLE (pvox - VAL(pvosuunta$ + STR$(8)), pvoy - 2), 8, 15
CIRCLE (pvox + VAL(pvosuunta$ + STR$(5)) + VAL(pvosuunta$ + STR$(15)), pvoy - 2), 8, 15
'sisÑsilmÑt
CIRCLE (pvox - VAL(pvosuunta$ + STR$(8)) - VAL(pvosuunta$ + STR$(4)), pvoy - 2), 2, 12
PSET (pvox - VAL(pvosuunta$ + STR$(8)) - VAL(pvosuunta$ + STR$(4)), pvoy - 2), 12
CIRCLE (pvox + VAL(pvosuunta$ + STR$(5)) + VAL(pvosuunta$ + STR$(15)) + VAL(pvosuunta$ + STR$(4)), pvoy - 2), 2, 12
PSET (pvox + VAL(pvosuunta$ + STR$(5)) + VAL(pvosuunta$ + STR$(15)) + VAL(pvosuunta$ + STR$(4)), pvoy - 2), 12
'hÑduli
LINE (pvox - VAL(pvosuunta$ + STR$(25)), pvoy + 50)-(pvox - VAL(pvosuunta$ + STR$(8)), pvoy + 20), 10
LINE (pvox + VAL(pvosuunta$ + STR$(30)), pvoy + 30)-(pvox + VAL(pvosuunta$ + STR$(10)), pvoy + 30), 10
DRAW "u5br" + pvosuunta$ + "1buu4"
'hiux
PSET (pvox, pvoy - 20), 7
DRAW "nh5nu6ne5"
'suu
CIRCLE (pvox + VAL(pvosuunta$ + STR$(8)), pvoy + 10), 3, 4
CIRCLE (pvox + VAL(pvosuunta$ + STR$(8)), pvoy + 10), 4, 4

END SUB

SUB teepaeppa
IF called = 1 THEN
paesuunta$ = ""
paex = p1x
paey = p1y
END IF
IF called = 2 THEN
paesuunta$ = "-"
paex = p2x
paey = p2y
END IF

'Paeblo the Old
CIRCLE (paex, paey - 9), 34, 7, , , .6'pÑÑ
PSET (paex - 10, paey + 29), 6
DRAW "u17"
PSET (paex + 10, paey + 29), 6
DRAW "u17"
LINE (paex - 10, paey + 29)-(paex - 14, paey + 64), 6'muu
LINE (paex + 10, paey + 29)-(paex + 14, paey + 64), 6
DRAW "c6l27r7c3d35r" + paesuunta$ + "5"
PSET (paex + 8, paey + 64), 6
DRAW "c3d35nr" + paesuunta$ + "5"
'hÑduli
LINE (paex - VAL(paesuunta$ + STR$(10)), paey + 30)-(paex - VAL(paesuunta$ + STR$(25)), paey + 50), 6
LINE (paex + VAL(paesuunta$ + STR$(10)), paey + 30)-(paex + VAL(paesuunta$ + STR$(30)), paey + 30), 6
'feici+fleda=naama&parta
PSET (paex + VAL(paesuunta$ + STR$(10)), paey - 1), 7
DRAW "c7d3r" + paesuunta$ + "18"
DRAW "c7bl" + paesuunta$ + "30bd4nd10"
FOR parta = 1 TO 8
DRAW "br" + paesuunta$ + "3nd10"
NEXT
'silm.
CIRCLE (paex + VAL(paesuunta$ + STR$(2)), paey - 12), 6, 2
CIRCLE (paex + VAL(paesuunta$ + STR$(5)) + VAL(paesuunta$ + STR$(15)), paey - 12), 6, 2
CIRCLE (paex + VAL(paesuunta$ + STR$(2)) + VAL(paesuunta$ + STR$(3)), paey - 12), 2, 11
CIRCLE (paex + VAL(paesuunta$ + STR$(5)) + VAL(paesuunta$ + STR$(15)) + VAL(paesuunta$ + STR$(3)), paey - 12), 2, 11

END SUB

SUB teepelaajat
CALL tee1pelaaja
CALL tee2pelaaja
END SUB

SUB teesiliconiperse
IF called = 1 THEN
scpsuunta$ = ""
scpx = p1x
scpy = p1y
END IF
IF called = 2 THEN
scpsuunta$ = "-"
scpx = p2x
scpy = p2y
END IF

'Silicooni Perse TM
CIRCLE (scpx, scpy), 30, 15 'pÑÑ
LINE (scpx - 10, scpy + 29)-(scpx - 10, scpy + 64), 15 'muu
LINE (scpx + 10, scpy + 29)-(scpx + 10, scpy + 64), 15
DRAW "c15 l20r7d35r" + scpsuunta$ + "5"
PSET (scpx + 8, scpy + 64), 2
DRAW "c15 d35nr" + scpsuunta$ + "5"
silmalku = scpx
IF nopeaMode <> 1 THEN
LINE (scpx - 9, scpy + 31)-(scpx + 9, scpy + 60), 0, BF
FOR taplat = 1 TO 5
PSET (scpx - 9 + INT(RND * 19), scpy + 31 + INT(RND * 29)), 9 + INT(RND * 7)
NEXT
END IF
IF scpsuunta$ = "" THEN
silmalku = silmalku + VAL(scpsuunta$ + STR$(1))
CIRCLE (silmalku, scpy - 5), 8, 14
CIRCLE (silmalku + 20, scpy - 5), 8, 14
CIRCLE (silmalku + VAL(scpsuunta$ + STR$(2)), scpy - 5), 2, 13
CIRCLE (silmalku + VAL(scpsuunta$ + STR$(22)), scpy - 5), 2, 13
ELSE
CIRCLE (silmalku + VAL(scpsuunta$ + STR$(2)) + 2, scpy - 5), 8, 14
CIRCLE (silmalku + VAL(scpsuunta$ + STR$(22)) + 2, scpy - 5), 8, 14
CIRCLE (silmalku + VAL(scpsuunta$ + STR$(2)), scpy - 5), 2, 13
CIRCLE (silmalku + VAL(scpsuunta$ + STR$(22)), scpy - 5), 2, 13
END IF
PSET (scpx - VAL(scpsuunta$ + STR$(10)), scpy - 15), 14
DRAW "r" + scpsuunta$ + "35"
DRAW "bd30c12l" + scpsuunta$ + "30u3"
'kÑtîset
LINE (scpx - VAL(scpsuunta$ + STR$(10)), scpy + 30)-(scpx - VAL(scpsuunta$ + STR$(25)), scpy + 40), 15
LINE (scpx + VAL(scpsuunta$ + STR$(10)), scpy + 30)-(scpx + VAL(scpsuunta$ + STR$(30)), scpy + 30), 15
'ja oikein helevetin iso PERSE
IF scpsuunta$ = "" THEN
CIRCLE (scpx - 10, scpy + 55), 9, 13, 1.7, 4.6
ELSE
CIRCLE (scpx + 10, scpy + 55), 9, 13, 4.6, 1.5
END IF
'muovikruunu
PSET (scpx - 10, scpy - 29), 11
DRAW "c11u5r20nd5nu2l10nu2l10u2"
'FOR kr = 1 TO 5: DRAW "e2f2": NEXT: DRAW "d7"
END SUB

SUB teesnailman
IF called = 1 THEN
snasuunta$ = ""
snax = p1x
snay = p1y
END IF
IF called = 2 THEN
snasuunta$ = "-"
snax = p2x
snay = p2y
END IF

IF (called = 1 AND p1suojaa = 0) OR (called = 2 AND p2suojaa = 0) THEN
'snailmÑni
'lîtkî
CIRCLE (snax, snay + 40), 60, 14, , , 1.5
PSET (snax, snay + 25), 7
'S-merkki
DRAW "h3u3r3f3bd3bl3f3d3l3h3"
'silmÑrit
DRAW "c7bu42bl15 u15 br10d10br16u10br10d15"
CIRCLE (snax - 14, snay - 38), 10, 15
CIRCLE (snax + 14, snay - 38), 10, 15
FOR silr = 1 TO 3
CIRCLE (snax - 14 + VAL(snasuunta$ + STR$(5)), snay - 38), silr, 3
CIRCLE (snax + 14 + VAL(snasuunta$ + STR$(5)), snay - 38), silr, 3
NEXT
ELSE
CIRCLE (snax, snay + 70), 60, 14, , , .5
CIRCLE (snax - 14, snay + 50), 10, 15
CIRCLE (snax + 14, snay + 50), 10, 15
FOR silr = 1 TO 3
CIRCLE (snax - 14 + VAL(snasuunta$ + STR$(5)), snay + 50), silr, 3
CIRCLE (snax + 14 + VAL(snasuunta$ + STR$(5)), snay + 50), silr, 3
NEXT
END IF
END SUB

SUB teestefan
IF called = 1 THEN
stesuunta$ = ""
stex = p1x
stey = p1y
END IF
IF called = 2 THEN
stesuunta$ = "-"
stex = p2x
stey = p2y
END IF

'Stefan the Great
IF nopeaMode = 0 THEN
FOR ster = 30 TO 35
CIRCLE (stex, stey), ster, 1, , , .6'pÑÑ
NEXT
ELSE
FOR ster = 34 TO 35
CIRCLE (stex, stey), ster, 1, , , .6'pÑÑ
NEXT
END IF
PSET (stex - 10, stey + 29), 9
DRAW "u8"
PSET (stex + 10, stey + 29), 9
DRAW "u8"
LINE (stex - 10, stey + 29)-(stex - 14, stey + 64), 9'muu
LINE (stex + 10, stey + 29)-(stex + 14, stey + 64), 9
DRAW "c9l27r7d35r" + stesuunta$ + "5"
PSET (stex + 8, stey + 64), 9
DRAW "c9d35nr" + stesuunta$ + "5"
'simmut kii
IF stesuunta$ = "" THEN
PSET (stex - 10, stey), 4
DRAW "u8frffrffrffrffl13br20r13u8lgglgglgglgg"
PAINT (stex - VAL(stesuunta$ + STR$(10)) + 2, stey - 2), 4
PAINT (stex - VAL(stesuunta$ + STR$(10)) + 28, stey - 2), 4
ELSE
PSET (stex - 25, stey), 4
DRAW "u8frffrffrffrffl13br20r13u8lgglgglgglgg"
PAINT (stex - 23, stey - 2), 4
PAINT (stex + 5, stey - 2), 4
END IF
'viitan solki
PSET (stex - 10, stey + 24), 14
DRAW "f10e10g10ldrru"
'hÑduli
LINE (stex - VAL(stesuunta$ + STR$(10)), stey + 30)-(stex - VAL(stesuunta$ + STR$(25)), stey + 50), 9
LINE (stex + VAL(stesuunta$ + STR$(10)), stey + 30)-(stex + VAL(stesuunta$ + STR$(30)), stey + 30), 9

END SUB

SUB teetessuk
IF called = 1 THEN
tesuunta$ = ""
tex = p1x
tey = p1y
END IF
IF called = 2 THEN
tesuunta$ = "-"
tex = p2x
tey = p2y
END IF

'TessaquÇ the dog
CIRCLE (tex, tey), 29, 6 'pÑÑ
CIRCLE (tex, tey), 30, 6 'pÑÑ
LINE (tex - 10, tey + 29)-(tex - 14, tey + 57), 6'muu
LINE (tex + 10, tey + 29)-(tex + 14, tey + 58), 6
DRAW "g5"
DRAW "c6 l15bl7bu5f5r2d36r" + tesuunta$ + "5"
PSET (tex + 8, tey + 64), 6
DRAW "c6 d35nr" + tesuunta$ + "5"
'silmuskit
silmalku = tex
IF tesuunta$ = "" THEN
silmalku = silmalku + VAL(tesuunta$ + STR$(1))
CIRCLE (silmalku, tey - 5), 6, 14
CIRCLE (silmalku + 20, tey - 5), 6, 14
CIRCLE (silmalku + VAL(tesuunta$ + STR$(2)), tey - 5), 2, 15
CIRCLE (silmalku + VAL(tesuunta$ + STR$(22)), tey - 5), 2, 15
ELSE
CIRCLE (silmalku + VAL(tesuunta$ + STR$(2)) + 2, tey - 5), 6, 14
CIRCLE (silmalku + VAL(tesuunta$ + STR$(22)) + 2, tey - 5), 6, 14
CIRCLE (silmalku + VAL(tesuunta$ + STR$(2)), tey - 5), 2, 15
CIRCLE (silmalku + VAL(tesuunta$ + STR$(22)), tey - 5), 2, 15
END IF
'hÑnz
LINE (tex - VAL(tesuunta$ + STR$(10)), tey + 30)-(tex - VAL(tesuunta$ + STR$(25)), tey + 50), 7
LINE (tex + VAL(tesuunta$ + STR$(10)), tey + 30)-(tex + VAL(tesuunta$ + STR$(30)), tey + 30), 7
'turparulla
PSET (tex + VAL(tesuunta$ + STR$(12)), tey + 10), 12
DRAW "h3r6g3c6g4l2h2bf2br2be4f4r2e2bdbl6c12d5glhu5"
PSET (tex - 10, tey - 30), 6
DRAW "u5e2r2f2d5lu4h2l2g2d4 br15 u5e2r2f2d5lu4h2l2g2d4"
PSET (tex - 3 + VAL(tesuunta$ + STR$(3)), tey + 40), 3
IF tesuunta$ = "" THEN
DRAW "c3r6l3d5c6bd14bl16g3f3e3"
ELSE
DRAW "c3r6l3d5c6bd14br16f3g3h3"
END IF

END SUB

SUB teeVilhelmo
IF called = 1 THEN
visuunta$ = ""
vix = p1x
viy = p1y
END IF
IF called = 2 THEN
visuunta$ = "-"
vix = p2x
viy = p2y
END IF

'Vilhelmo
'pÑÑ
IF nopeaMode = 1 THEN
CIRCLE (vix, viy), 35, 9, , , .6'pÑÑ
ELSE
FOR vir = 30 TO 35
CIRCLE (vix, viy), vir, 1, , , .6'pÑÑ
NEXT
END IF
'summu
CIRCLE (vix + VAL(visuunta$ + STR$(5)), viy + 7), 20, 7, 3.2, 0, .3
PSET (vix + VAL(visuunta$ + STR$(5)), viy + 13), 0
DRAW "c15d2ru2rd2ru2"
'kaula
PSET (vix - 10, viy + 29), 9
DRAW "u8"
PSET (vix + 10, viy + 29), 9
DRAW "u8"
LINE (vix - 10, viy + 29)-(vix - 14, viy + 64), 9'muu
LINE (vix + 10, viy + 29)-(vix + 14, viy + 64), 9
'jalkakalkaset
DRAW "c9l27r7c7d35r" + visuunta$ + "5"
PSET (vix + 8, viy + 64), 9
DRAW "c7d35nr" + visuunta$ + "5"
'simlÑt
CIRCLE (vix + VAL(visuunta$ + STR$(2)), viy - 2), 10, 5, , , .6
CIRCLE (vix + VAL(visuunta$ + STR$(5)) + VAL(visuunta$ + STR$(15)), viy - 2), 10, 5, , , .6
PSET (vix + VAL(visuunta$ + STR$(2)), viy - 3), 0
DRAW "c8bl" + visuunta$ + "11l" + visuunta$ + "25d2r" + visuunta$ + "25"
DRAW "bm" + STR$(vix - 4 + VAL(visuunta$ + STR$(3))) + "," + STR$(viy + 40) + "c2f2df2e2ue2"
'hÑduli
LINE (vix - VAL(visuunta$ + STR$(10)), viy + 30)-(vix - VAL(visuunta$ + STR$(25)), viy + 50), 9
LINE (vix + VAL(visuunta$ + STR$(10)), viy + 30)-(vix + VAL(visuunta$ + STR$(30)), viy + 30), 9
END SUB

SUB teewille
IF called = 1 THEN
wilsuunta$ = ""
wilx = p1x
wily = p1y
END IF
IF called = 2 THEN
wilsuunta$ = "-"
wilx = p2x
wily = p2y
END IF
'Willex
CIRCLE (wilx, wily), 35, 12, , , .6'pÑÑ
PSET (wilx - 10, wily + 29), 3
DRAW "u8"
PSET (wilx + 10, wily + 29), 3
DRAW "u8"
LINE (wilx - 10, wily + 29)-(wilx - 14, wily + 64), 3'muu
LINE (wilx + 10, wily + 29)-(wilx + 14, wily + 64), 3
DRAW "c3l27r7d35r" + wilsuunta$ + "5"
PSET (wilx + 8, wily + 64), 3
DRAW "c3d35nr" + wilsuunta$ + "5"
'naama
silmalku = wilx
IF wilsuunta$ = "" THEN
silmalku = silmalku + VAL(wilsuunta$ + STR$(1))
CIRCLE (silmalku, wily - 5), 10, 15
CIRCLE (silmalku + 20, wily - 5), 10, 15
CIRCLE (silmalku + VAL(wilsuunta$ + STR$(2)), wily - 5), 2, 13
CIRCLE (silmalku + VAL(wilsuunta$ + STR$(22)), wily - 5), 2, 10
ELSE
CIRCLE (silmalku + VAL(wilsuunta$ + STR$(2)) + 2, wily - 5), 10, 15
CIRCLE (silmalku + VAL(wilsuunta$ + STR$(22)) + 2, wily - 5), 10, 15
CIRCLE (silmalku + VAL(wilsuunta$ + STR$(2)), wily - 5), 2, 13
CIRCLE (silmalku + VAL(wilsuunta$ + STR$(22)), wily - 5), 2, 10
END IF
'suuvÑrkki
PSET (wilx + VAL(wilsuunta$ + STR$(15)), wily + 15), 2
DRAW "c2u2l" + wilsuunta$ + "15d2"
DRAW "c12 bu8br" + wilsuunta$ + "10r" + wilsuunta$ + "6d2l" + wilsuunta$ + "5"
'hius
'FOR hiux = 15 TO 16
PSET (wilx - 15, wily - 20), 6
DRAW "u12f10e8f8e8d12"
'NEXT
'hÑduli
LINE (wilx - VAL(wilsuunta$ + STR$(10)), wily + 30)-(wilx - VAL(wilsuunta$ + STR$(25)), wily + 50), 3
LINE (wilx + VAL(wilsuunta$ + STR$(10)), wily + 30)-(wilx + VAL(wilsuunta$ + STR$(30)), wily + 30), 3
END SUB

SUB teezairar
IF called = 1 THEN
zasuunta$ = ""
zax = p1x
zay = p1y
END IF
IF called = 2 THEN
zasuunta$ = "-"
zax = p2x
zay = p2y
END IF
'zairar
CIRCLE (zax, zay), 30, 12, 6.2, 3.18
LINE (zax + VAL(zasuunta$ + STR$(30)), zay + 3)-(zax + VAL(zasuunta$ + STR$(10)), zay + 31), 15
DRAW "c15l" + zasuunta$ + "20"
LINE (zax - VAL(zasuunta$ + STR$(10)), zay + 31)-(zax - VAL(zasuunta$ + STR$(30)), zay + 3), 15
IF zasuunta$ = "-" THEN DRAW "bl60"
DRAW "c15r60"
DRAW "l10nd15l10nd27l10nd27l10nd26l10d13"
IF nopeaMode = 0 THEN DRAW "c7br5nu3br10nu3br10nu3br10nu3"
LINE (zax - 10, zay + 29)-(zax - 14, zay + 64), 3'muu
LINE (zax + 10, zay + 29)-(zax + 14, zay + 64), 3
DRAW "c3 l27r7c11d35r" + zasuunta$ + "5"
PSET (zax + 8, zay + 64), 11
DRAW "c11d35nr" + zasuunta$ + "5"
silmalku = zax
IF zasuunta$ = "" THEN
silmalku = silmalku + VAL(zasuunta$ + STR$(1))
CIRCLE (silmalku, zay - 6), 8, 11
CIRCLE (silmalku + 20, zay - 6), 8, 11
CIRCLE (silmalku + VAL(zasuunta$ + STR$(2)), zay - 6), 2, 3
CIRCLE (silmalku + VAL(zasuunta$ + STR$(22)), zay - 6), 2, 3
ELSE
CIRCLE (silmalku + VAL(zasuunta$ + STR$(2)) + 2, zay - 6), 8, 11
CIRCLE (silmalku + VAL(zasuunta$ + STR$(22)) + 2, zay - 6), 8, 11
CIRCLE (silmalku + VAL(zasuunta$ + STR$(2)), zay - 6), 2, 3
CIRCLE (silmalku + VAL(zasuunta$ + STR$(22)), zay - 6), 2, 3
END IF
PSET (zax + VAL(zasuunta$ + STR$(4)), zay - 30), 14
DRAW "h10l10g10br30e10r10f10"
'hÑnz
LINE (zax - VAL(zasuunta$ + STR$(10)), zay + 30)-(zax - VAL(zasuunta$ + STR$(25)), zay + 50), 11
LINE (zax + VAL(zasuunta$ + STR$(10)), zay + 30)-(zax + VAL(zasuunta$ + STR$(30)), zay + 30), 11
PSET (zax - VAL(zasuunta$ + STR$(7)), zay - 5), 0
DRAW "c3l" + zasuunta$ + "20"
END SUB

SUB telop
CALL poistapelaajat
p1x = 100
p2x = 540
CALL teepelaajat
IF voitti = 1 THEN
CALL poista1pjalka
LINE (p1x + 8, p1y + 65)-(p1x + 70, p1y + 63), 6: DRAW "c6u5"
FOR kusy = 400 TO 250 STEP -1
kulo = 5 + INT(RND * 30)
IF kusy > p1y + 65 THEN LINE (p1x + 5, p1y + 65)-(p1x + kulo, kusy - 1), 14 + INT(RND * 2)
FOR s = 1 TO 13
SOUND 100 + INT(RND * 300), .1
NEXT
LINE (0, kusy)-(640, kusy), 14
NEXT
ELSE
CALL poista2pjalka
LINE (p2x - 8, p2y + 65)-(p2x - 70, p2y + 63), 6: DRAW "c6u5"
FOR kusy = 400 TO 250 STEP -1
kulo = 5 + INT(RND * 30)
IF kusy > p1y + 65 THEN LINE (p2x - 5, p2y + 65)-(p2x - kulo, kusy - 1), 14 + INT(RND * 2)
FOR s = 1 TO 20
SOUND 100 + INT(RND * 300), .1
NEXT
LINE (0, kusy)-(640, kusy), 14
NEXT
END IF
fonnistu = 1
END SUB

SUB verenlento
IF hav$ = "Willex" THEN
tv = 6
ELSEIF hav$ = "SiliconeAss" THEN
tv = 15
ELSE
tv = 4
END IF
FOR veret = 1 TO 100
VVAA = INT(RND * 639) + 1
VVYY = INT(RND * 360) + 80
apu = INT(RND * 100)
FOR lisa = 1 TO INT(RND * 40) STEP INT(RND * 3) + 1
FOR POOKO = 1 TO INT(RND * 5)
CIRCLE (VVAA, VVYY + lisa), POOKO, tv
NEXT POOKO
NEXT lisa
FOR lisa = 30 - INT(RND * 50) TO INT(RND * 40) STEP INT(RND * 4) + 1
FOR POOKO = 1 TO INT(RND * 20) STEP 1
CIRCLE (VVAA + 1, VVYY + lisa), POOKO, tv
se = INT(RND * 100)
FOR toisenpysty = VVYY TO se
CIRCLE (VVAA + 1, toisenpysty + lisa), INT(POOKO / 2), tv
NEXT
FOR toisenpysty = VVYY TO se
CIRCLE (VVAA, toisenpysty + lisa), INT(POOKO / 2), tv
NEXT
NEXT POOKO
NEXT lisa
NEXT veret
END SUB

SUB vilop
CALL poistapelaajat
p1x = 200
p2x = 440
CALL teepelaajat
IF voitti = 1 THEN
FOR raiske = 1 TO 50
salx = p2x + 30 - INT(RND * 60)
saly = 0
DO
osaly = saly
osalx = salx
saly = saly + 10 + INT(RND * 30)
salx = salx + 40 - INT(RND * 80)
LINE (osalx, osaly)-(salx, saly), 11 + (4 + INT(RND * 2))
SOUND 800 + INT(RND * 500), .1
LOOP WHILE saly < 380
NEXT
LINE (0, 0)-(640, 480), 15, BF
FOR s = 1 TO 70
SOUND 100 + INT(RND * 100), .1
NEXT
ELSE
FOR raiske = 1 TO 50
salx = p1x + 30 - INT(RND * 60)
saly = 0
DO
osaly = saly
osalx = salx
saly = saly + 10 + INT(RND * 30)
salx = salx + 40 - INT(RND * 80)
LINE (osalx, osaly)-(salx, saly), 11 + (4 + INT(RND * 2))
SOUND 800 + INT(RND * 500), .1
LOOP WHILE saly < 380
NEXT
LINE (0, 0)-(640, 480), 15, BF
FOR s = 1 TO 70
SOUND 100 + INT(RND * 100), .1
NEXT
END IF
fonnistu = 1
END SUB

SUB wilop
CALL poistapelaajat
p1x = 100
p2x = 540
CALL teepelaajat
IF voitti = 1 THEN
CALL poista1pelaaja
restop2x = p2x
restop2y = p2y
restop2$ = p2$
p2x = p1x
p2y = p1y
p2$ = p1$
called = 2: CALL tee2pelaaja
FOR pakat = 1 TO 15
koko = 3 + INT(RND * 10)
pay = 320 + INT(RND * 60)
raa = 3 + INT(RND * 8)
FOR pax = 120 TO 540 STEP raa
CIRCLE (pax, pay), koko, 6
CIRCLE (pax, pay), koko + 1, 6
IF INT(RND * 2) = 1 THEN SOUND 50 + INT(RND * 200), .1
NEXT
FOR pax = 120 TO 540 STEP raa
CIRCLE (pax, pay), koko, 0
CIRCLE (pax, pay), koko + 1, 0
NEXT
pumx = 540 - INT(RND * 30)
pumy = pay
loko = 30 + INT(RND * 70)
FOR pkoko = 1 TO loko
CIRCLE (pumx, pumy), pkoko, 6
IF INT(RND * 2) = 1 THEN SOUND 100 + INT(RND * 100), .1
NEXT
FOR pkoko = 1 TO loko
CIRCLE (pumx, pumy), pkoko, 0
NEXT
NEXT
p2x = restop2x
p2y = restop2y
p2$ = restop2$
ELSE
CALL poista2pelaaja
restop1x = p1x
restop1y = p1y
restop1$ = p1$
p1x = p2x
p1y = p2y
p1$ = p2$
called = 1: CALL tee1pelaaja
FOR pakat = 1 TO 15
koko = 3 + INT(RND * 10)
pay = 350 + INT(RND * 30)
raa = 3 + INT(RND * 8)
FOR pax = 520 TO 120 STEP -raa
CIRCLE (pax, pay), koko, 6
CIRCLE (pax, pay), koko + 1, 6
IF INT(RND * 2) = 1 THEN SOUND 50 + INT(RND * 200), .1
NEXT
FOR pax = 520 TO 120 STEP -raa
CIRCLE (pax, pay), koko, 0
CIRCLE (pax, pay), koko + 1, 0
NEXT
pumx = 100 + INT(RND * 30)
pumy = pay
loko = 30 + INT(RND * 70)
FOR pkoko = 1 TO loko
CIRCLE (pumx, pumy), pkoko, 6
IF INT(RND * 2) = 1 THEN SOUND 100 + INT(RND * 100), .1
NEXT
FOR pkoko = 1 TO loko
CIRCLE (pumx, pumy), pkoko, 0
NEXT
NEXT
p1x = restop1x
p1y = restop1y
p1$ = restop1$
END IF
fonnistu = 1
END SUB

SUB zalop
CALL poistapelaajat
p1x = 150
p2x = 390
CALL teepelaajat
IF voitti = 1 THEN ufox = 220
IF voitti = 2 THEN ufox = 320
ufoy = 330
xmu = 10
ymu = -10
GOSUB teeufo
'PLAY "mfl8o3cegl4b-p4"
FOR uhvo = 1 TO 600
GOSUB poiufo
ufox = ufox + xmu
ufoy = ufoy + ymu
IF ufox >= 620 THEN xmu = -10
IF ufox <= 20 THEN xmu = 10
IF ufoy >= 350 THEN ymu = -10
IF ufoy <= 20 THEN ymu = 10
GOSUB teeufo
'SOUND 800 - ufoy * 2, .4
SOUND 100 + INT(RND * 100), .1
NEXT
GOSUB poiufo
IF voitti = 1 THEN
pox = p2x
ELSE
pox = p1x
END IF
FOR por = 1 TO 200
CIRCLE (pox, 340), por, 15
SOUND 50 + INT(RND * 300), .2
NEXT
FOR por = 1 TO 200
CIRCLE (pox, 340), por, 0
NEXT
fonnistu = 1
EXIT SUB
teeufo:
FOR cir = 5 TO 10 + INT(RND * 20) STEP 2
CIRCLE (ufox, ufoy), cir, 11 + (4 * INT(RND * 2))
NEXT
'DRAW "c11u20d60u20ng20f20"  'forget this
RETURN
poiufo:
'CIRCLE (ufox, ufoy), 20, 0  'and this
'DRAW "c0u20d60u20ng20f20"  'and this
RETURN
END SUB

