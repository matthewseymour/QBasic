REM $DYNAMIC
ON ERROR GOTO handler
DIM w(100, 1000) AS INTEGER
DIM clip(500) AS INTEGER
tabs = 8
height = 20
hlcolor = 15
bgcolor = 0
fgcolor = 7

start:
CLS
selectstartx = 0
selectstarty = 0
selectend = 0
selected = 0
docname$ = ""
endofdoc = 4
oendofdoc = 4
x = 1
y = 1
oy = 1
ox = 1
fl2 = 1
scry = 0
scrx = 0
recolor:
COLOR fgcolor, bgcolor
CLS
FOR i = 1 TO 79: LOCATE 4, i: PRINT CHR$(205): NEXT i
LOCATE 4, 80: PRINT CHR$(187)
LOCATE height + 3, 80: PRINT CHR$(188)
FOR i = 1 TO height - 2: LOCATE i + 4, 80: PRINT CHR$(177): NEXT i
FOR i = 1 TO 79: LOCATE height + 3, i: PRINT CHR$(177): NEXT i
LOCATE height + 3, 79: PRINT CHR$(16)
LOCATE height + 3, 1: PRINT CHR$(17)
LOCATE 5, 80: PRINT CHR$(24)
LOCATE height + 2, 80: PRINT CHR$(25)
LOCATE 1, 1: PRINT "F1-NEW  F2-SAVE  F3-SAVE AS  F4-LOAD  F5-OPTIONS  F6-QUIT"
GOSUB redraw
errreturn:
DO
IF oy <> y OR oendofdoc <> endofdoc THEN LOCATE (oy / oendofdoc) * (height - 5) + 6, 80: COLOR fgcolor: PRINT CHR$(177)
IF ox <> x THEN LOCATE height + 3, (ox / 100) * 77 + 1: COLOR fgcolor: PRINT CHR$(177)
oy = y
ox = x
oendofdoc = endofdoc
LOCATE (oy / oendofdoc) * (height - 5) + 6, 80: COLOR fgcolor: PRINT CHR$(219)
LOCATE height + 3, (ox / 100) * 77 + 1: COLOR fgcolor: PRINT CHR$(219)

IF docname$ <> "" THEN LOCATE 4, 2: PRINT " "; docname$; " "
IF docname$ = "" THEN LOCATE 4, 2: PRINT " (Untitled) "
LOCATE 2, 57: PRINT "Pos: X -"; x; "Y -"; y; " "
LOCATE 3, 57: PRINT "Page:"; INT(y / (height - 2)) + 1; "of"; INT((endofdoc - 1) / (height - 2)) + 1
LOCATE y + 4 - scry, x - scrx, 1
FOR i = 1 TO 1000
NEXT i
LOCATE y + 4 - scry, x - scrx, 0
a$ = INKEY$
scan = INP(96)
IF scan = 42 OR scan = 54 THEN
        IF a$ = CHR$(0) + "P" THEN
                IF selected = 0 THEN selected = 1: selectstartx = x: selectstarty = y
                selectendx = x: selectendy = y
                y = y + 1
                IF y > endofdoc THEN y = endofdoc
                IF y > 1000 THEN y = 1000: BEEP
                IF y - scry >= (height - 1) THEN scry = scry + 1: GOSUB redraw
        ELSEIF a$ = CHR$(0) + "H" THEN
                IF selected = 0 THEN selected = 1: selectstartx = x: selectstarty = y
                selectendx = x: selectendy = y
                y = y - 1
                IF y - scry <= 0 AND scry > 0 THEN scry = scry - 1: GOSUB redraw
                IF y < 1 THEN y = 1
        ELSEIF a$ = CHR$(0) + "M" THEN
                IF selected = 0 THEN selected = 1: selectstartx = x: selectstarty = y
                selectendx = x: selectendy = y
                x = x + 1
                IF x = 101 THEN x = 100
                IF x - scrx > 79 THEN scrx = scrx + 1: GOSUB redraw
        ELSEIF a$ = CHR$(0) + "K" THEN
                IF selected = 0 THEN selected = 1: selectstartx = x: selectstarty = y
                selectendx = x: selectendy = y
                x = x - 1
                IF x = 0 THEN x = 1
                IF x - scrx < 1 THEN scrx = scrx - 1: GOSUB redraw
        END IF
END IF
IF a$ = CHR$(0) + ";" THEN
        LOCATE 1, 1: COLOR hlcolor: PRINT "F1-NEW"
        COLOR fgcolor
        FOR x = 1 TO 100
        FOR y = 1 TO 1000
        w(x, y) = 0
        NEXT y
        NEXT x
        GOTO start
ELSEIF a$ = CHR$(0) + "<" THEN
        LOCATE 1, 9: COLOR hlcolor: PRINT "F2-SAVE"
        COLOR fgcolor
        IF docname$ = "" THEN
                LOCATE 2, 1
                INPUT "NAME >", name$
                FOR i = 1 TO 80: LOCATE 4, i: PRINT CHR$(205): NEXT i
                docname$ = name$
        ELSE
                name$ = docname$
        END IF
        LOCATE 3, 1: PRINT "SAVING "; name$
        OPEN name$ FOR OUTPUT AS #1
        FOR y2 = 1 TO endofdoc
        FOR x2 = 1 TO 100
        IF w(x2, y2) <> 0 THEN
                IF x2 < 100 THEN PRINT #1, CHR$(w(x2, y2));  ELSE PRINT #1, CHR$(w(x2, y2))
        ELSE
                IF x2 < 100 THEN PRINT #1, " ";  ELSE PRINT #1, " "
        END IF
        NEXT x2
        NEXT y2
        LOCATE 2, 1: COLOR bgcolor
        PRINT "      "; name$
        PRINT "       "; name$
        COLOR fgcolor
        LOCATE 1, 9: PRINT "F2-SAVE"
        CLOSE #1
ELSEIF a$ = CHR$(0) + "=" THEN
        LOCATE 1, 18: COLOR hlcolor: PRINT "F3-SAVE AS"
        COLOR fgcolor
        LOCATE 2, 1
        INPUT "NAME >", name$
        FOR i = 1 TO 80: LOCATE 4, i: PRINT CHR$(205): NEXT i
        docname$ = name$
        LOCATE 3, 1: PRINT "SAVING "; name$
        OPEN name$ FOR OUTPUT AS #1
        FOR y2 = 1 TO endofdoc
        FOR x2 = 1 TO 100
        IF w(x2, y2) <> 0 THEN
                IF x2 < 100 THEN PRINT #1, CHR$(w(x2, y2));  ELSE PRINT #1, CHR$(w(x2, y2))
        ELSE
                IF x2 < 100 THEN PRINT #1, " ";  ELSE PRINT #1, " "
        END IF
        NEXT x2
        NEXT y2
        LOCATE 2, 1: COLOR bgcolor
        PRINT "      "; name$
        PRINT "       "; name$
        COLOR fgcolor
        LOCATE 1, 18: PRINT "F3-SAVE AS"
        CLOSE #1
ELSEIF a$ = CHR$(0) + ">" THEN
        LOCATE 1, 30: COLOR hlcolor: PRINT "F4-LOAD"
        COLOR fgcolor
        LOCATE 2, 1: INPUT "NAME >", name$
        LOCATE 3, 1: PRINT "OPENING "; name$
        OPEN name$ FOR INPUT AS #1
        FOR i = 1 TO 80: LOCATE 4, i: PRINT CHR$(205): NEXT i
        docname$ = name$
        FOR y2 = 1 TO 1000
        endofdoc = y2 + 4
        LINE INPUT #1, in$
        FOR x2 = 1 TO 100
        IF MID$(in$, x2, 1) <> "" THEN w(x2, y2) = ASC(MID$(in$, x2, 1)) ELSE w(x2, y2) = 0
        NEXT x2
        NEXT y2
skipload:
        LOCATE 2, 1: COLOR bgcolor
        PRINT "      "; name$
        PRINT "        "; name$
        COLOR fgcolor
        x = 1
        y = 1
        scry = 0
        scrx = 0
        GOSUB redraw
        LOCATE 1, 30: PRINT "F4-LOAD"
        CLOSE #1
ELSEIF a$ = CHR$(0) + "?" THEN
        LOCATE 1, 39: COLOR hlcolor: PRINT "F5-OPTIONS"
        LOCATE 2, 1: COLOR hlcolor: PRINT "C"
        PRINT "S"
        LOCATE 2, 2: COLOR fgcolor: PRINT "OLORS"
        LOCATE 3, 2: COLOR fgcolor: PRINT "CREEN MODE"
        DO: b$ = INKEY$: LOOP UNTIL b$ <> ""
        LOCATE 2, 1: PRINT "      "
        PRINT "           "
        IF b$ = "c" THEN
rechoose:
                ofg = fgcolor: obg = bgcolor: ohl = hlcolor
                FOR i = 0 TO 15
                IF i < 10 THEN LOCATE 2, i + 1: COLOR fgcolor: PRINT CHR$(i + 48)
                IF i >= 10 THEN LOCATE 2, i + 1: COLOR fgcolor: PRINT CHR$(i + 55)
                LOCATE 3, i + 1: COLOR i: PRINT CHR$(219)
                NEXT i
                LOCATE 3, 19: COLOR fgcolor: PRINT "ENTER = Current color"
                LOCATE 2, 19: PRINT "Input background color       "
                DO: c$ = INKEY$: LOOP UNTIL c$ <> ""
                IF (ASC(c$) >= 48 AND ASC(c$) <= 57) OR (ASC(c$) >= 97 AND ASC(c$) <= 102) THEN
                        IF ASC(c$) <= 57 THEN bgcolor = ASC(c$) - 48 ELSE bgcolor = ASC(c$) - 87
                END IF
                LOCATE 2, 19: PRINT "Input bright foreground color"
                DO: c$ = INKEY$: LOOP UNTIL c$ <> ""
                IF (ASC(c$) >= 48 AND ASC(c$) <= 57) OR (ASC(c$) >= 97 AND ASC(c$) <= 102) THEN
                        IF ASC(c$) <= 57 THEN hlcolor = ASC(c$) - 48 ELSE hlcolor = ASC(c$) - 87
                END IF
                LOCATE 2, 19: PRINT "Input foreground color       "
                DO: c$ = INKEY$: LOOP UNTIL c$ <> ""
                IF (ASC(c$) >= 48 AND ASC(c$) <= 57) OR (ASC(c$) >= 97 AND ASC(c$) <= 102) THEN
                        IF ASC(c$) <= 57 THEN fgcolor = ASC(c$) - 48 ELSE fgcolor = ASC(c$) - 87
                END IF
                LOCATE 2, 1: PRINT "                                                  ": PRINT "                                              "
                IF fgcolor = bgcolor OR fgcolor = hlcolor OR bgcolor = hlcolor THEN
                        LOCATE 2, 1: PRINT "2 of the colors you have chosen are the same"
                        LOCATE 3, 1: PRINT "Would you like to choose your colors again? (y/n)"
                        DO: c$ = INKEY$: LOOP UNTIL c$ <> ""
                        IF c$ = "y" THEN
                                fgcolor = ofg: bgcolor = obg: hlcolor = ohl
                                LOCATE 2, 1: PRINT "                                                  ": PRINT "                                              "
                                GOTO rechoose
                        END IF
                        LOCATE 2, 1: PRINT "                                                  ": PRINT "                                              "
                END IF
                LOCATE 1, 39: PRINT "F5-OPTIONS"
                GOTO recolor
        ELSEIF b$ = "s" THEN
                LOCATE 2, 1: COLOR hlcolor: PRINT "L"
                PRINT "H"
                LOCATE 2, 3: COLOR fgcolor: PRINT "25 ROWS"
                LOCATE 3, 3: PRINT "50 ROWS"
                DO: c$ = INKEY$: LOOP UNTIL c$ <> ""
                IF c$ = "h" THEN
                        WIDTH 80, 50
                        height = 40
                ELSE
                        WIDTH 80, 25
                        height = 20
                END IF
                LOCATE 2, 1: PRINT "             "
                PRINT "             "
                LOCATE 1, 39: PRINT "F5-OPTIONS"
                GOTO recolor
        END IF
ELSEIF a$ = CHR$(0) + "G" THEN
        x = 1
        scrx = 0
        GOSUB redraw
ELSEIF a$ = CHR$(0) + "O" THEN
        FOR i = 100 TO 0 STEP -1
        IF w(i, y) <> 0 OR i = 0 THEN x = i + 1: EXIT FOR
        NEXT i
        IF x = 101 THEN x = 100
        IF x - scrx > 79 THEN scrx = x - 79
        GOSUB redraw
ELSEIF a$ = CHR$(0) + "S" THEN
        FOR i = x TO 99
        w(i, y) = w(i + 1, y)
        NEXT i
        w(100, y) = 0
        GOSUB redraw
ELSEIF a$ = CHR$(0) + "Q" THEN
        IF y = 1000 THEN GOTO skippgdo
        y = y + (height - 2)
        scry = scry + (height - 2)
        IF y > 1000 THEN scry = 1000 - (y - 1000): y = 1000: BEEP
        IF y > endofdoc THEN scry = scry - (y - endofdoc): y = endofdoc
        IF scry < 0 THEN scry = 0
        GOSUB redraw
skippgdo:
ELSEIF a$ = CHR$(0) + "I" THEN
        y = y - (height - 2)
        IF y < 1 THEN y = 1
        scry = scry - (height - 2)
        IF scry < 0 THEN scry = 0
        GOSUB redraw
ELSEIF a$ = CHR$(27) OR a$ = CHR$(0) + "@" THEN
        LOCATE 1, 51: COLOR hlcolor: PRINT "F6-QUIT"
        COLOR fgcolor
        SYSTEM
ELSEIF a$ = CHR$(0) + "H" THEN
        selected = 0
        y = y - 1
        IF y - scry <= 0 AND scry > 0 THEN scry = scry - 1: GOSUB redraw
        IF y < 1 THEN y = 1
ELSEIF a$ = CHR$(0) + "P" THEN
        selected = 0
        y = y + 1
        IF y > endofdoc THEN y = endofdoc
        IF y > 1000 THEN y = 1000: BEEP
        IF y - scry >= (height - 1) THEN scry = scry + 1: GOSUB redraw
ELSEIF a$ = CHR$(0) + "M" THEN
        selected = 0
        x = x + 1
        IF x = 101 THEN x = 100
        IF x - scrx > 79 THEN scrx = scrx + 1: GOSUB redraw
ELSEIF a$ = CHR$(0) + "K" THEN
        selected = 0
        x = x - 1
        IF x = 0 THEN x = 1
        IF x - scrx < 1 THEN scrx = scrx - 1: GOSUB redraw
ELSEIF a$ = CHR$(13) THEN
        LOCATE y + 4 - scry, x - scrx
        PRINT " "
        IF y < 1000 THEN
                FOR y2 = endofdoc TO y
                x2 = 0
                IF y2 > 1000 THEN GOTO skipenter
                FOR i = x TO 100
                IF w(i, y2) = 0 THEN EXIT FOR
                x2 = x2 + 1
                w(x2, y2 + 1) = w(i, y2)
                w(i, y2) = 0
                NEXT i
skipenter:
                NEXT y2
               
                IF y2 > 1000 THEN GOTO skipenter
                FOR i = x TO 100
                IF w(i, y2) = 0 THEN EXIT FOR
                x2 = x2 + 1
                w(x2, y2 + 1) = w(i, y2)
                w(i, y2) = 0
                NEXT i
skipenter:
                NEXT y2
        END IF
        x = 1
        scrx = 0
        y = y + 1
        endofdoc = endofdoc + 1
        IF y > 1000 THEN y = 1000: endofdoc = endofdoc - 1: BEEP
        IF y - scry >= (height - 1) THEN scry = scry + 1
        GOSUB redraw
ELSEIF a$ = CHR$(8) THEN
        IF x = 1 AND y = 1 THEN a$ = "": GOTO skip
        x = x - 1
        IF x - scrx < 2 THEN scrx = scrx - 1
        IF scrx < 0 THEN scrx = 0
        IF x = 0 THEN
                y = y - 1
                endofdoc = endofdoc - 1
                FOR i = 100 TO 0 STEP -1
                IF w(i, y) <> 0 OR i = 0 THEN x = i + 1: EXIT FOR
                NEXT i
                IF x = 101 THEN x = 100
                IF x - scrx > 79 THEN scrx = x - 79
                w(x, y) = 0
                x2 = x
                DO
                w(x2, y) = w(1, y + 1)
                FOR i = 1 TO 99
                w(i, y + 1) = w(i + 1, y + 1)
                NEXT i
                w(100, y + 1) = 0
                x2 = x2 + 1
                LOOP UNTIL x2 >= 101
        ELSE
                w(x, y) = 0
                FOR i = x TO 99
                w(i, y) = w(i + 1, y)
                NEXT i
                w(100, y) = 0
        END IF
        IF y - scry <= 0 AND scry > 0 THEN scry = scry - 1
        GOSUB redraw
skip:
ELSEIF a$ = CHR$(9) THEN
        FOR i = 1 TO tabs
        LOCATE y + 4 - scry, x - scrx
        PRINT " "
        w(x, y) = 32
        x = x + 1
        IF x >= 101 THEN x = 1: y = y + 1: endofdoc = endofdoc + 1: scrx = 0
        IF y > 1000 THEN y = 1000: endofdoc = endofdoc + 1: BEEP
        IF x - scrx > 79 THEN scrx = x - 79
        IF y - scry >= (height - 1) THEN scry = scry + 1
        NEXT i
        GOSUB redraw
ELSEIF a$ <> "" THEN
        LOCATE y + 4 - scry, x - scrx
        PRINT a$
        w(x, y) = ASC(a$)
        x = x + 1
        IF x >= 101 THEN x = 1: y = y + 1: endofdoc = endofdoc + 1: scrx = 0
        IF y > 1000 THEN y = 1000: endofdoc = endofdoc + 1: BEEP
        IF x - scrx > 79 THEN scrx = x - 79
        IF y - scry >= (height - 1) THEN scry = scry + 1
        GOSUB redraw
END IF
LOOP

redraw:
FOR xx = scrx + 1 TO scrx + 79
FOR yy = scry + 1 TO scry + (height - 2)
LOCATE yy + 4 - scry, xx - scrx
IF yy > 1000 THEN PRINT " ": GOTO skipprob
IF w(xx, yy) >= 32 THEN PRINT CHR$(w(xx, yy))
IF w(xx, yy) = 0 OR w(xx, yy) = 9 THEN PRINT " "
skipprob:
NEXT yy
NEXT xx
RETURN
handler:
IF ERR = 62 THEN
        RESUME skipload
ELSEIF ERR = 53 THEN
        LOCATE 2, 1: COLOR bgcolor: PRINT "      "; name$
        LOCATE 3, 1: PRINT "        "; name$
        LOCATE 2, 1: COLOR fgcolor: PRINT "FILE NOT FOUND"
        LOCATE 3, 1: PRINT "PRESS ANY KEY"
        DO: LOOP UNTIL INKEY$ <> ""
        LOCATE 2, 1: PRINT "              "
        LOCATE 3, 1: PRINT "             "
        LOCATE 1, 30: PRINT "F4-LOAD"
        RESUME errreturn
ELSEIF ERR = 64 OR ERR = 52 OR ERR = 75 THEN
        LOCATE 2, 1: COLOR bgcolor: PRINT "      "; name$
        LOCATE 3, 1: PRINT "        "; name$
        LOCATE 2, 1: COLOR fgcolor: PRINT "BAD FILENAME"
        LOCATE 3, 1: PRINT "PRESS ANY KEY"
        DO: LOOP UNTIL INKEY$ <> ""
        LOCATE 2, 1: PRINT "            "
        LOCATE 3, 1: PRINT "             "
        LOCATE 1, 9: PRINT "F2-SAVE  F3-SAVE AS  F4-LOAD"
        RESUME errreturn
ELSE
        END
        CLS
        PRINT "ERROR #"; ERR
        PRINT "PRESS ANY KEY TO RESTART PROGRAM"
        PRINT "YOU WILL LOSE ANY UNSAVED INFORMATION"
        DO: LOOP UNTIL INKEY$ <> ""
        FOR x = 1 TO 100
        FOR y = 1 TO 1000
        w(x, y) = 0
        NEXT y
        NEXT x
        RESUME start
END IF
END

