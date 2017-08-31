REM $DYNAMIC
ON ERROR GOTO handler
DIM w(80, 1000) AS INTEGER
start:
CLS
docname$ = ""
endofdoc = 4
x = 1
y = 1
fl2 = 1
scry = 0
FOR i = 1 TO 80: LOCATE 4, i: PRINT CHR$(205): NEXT i
LOCATE 1, 1: PRINT "F1-NEW  F2-SAVE  F3-SAVE AS  F4-LOAD  F5-QUIT"
errreturn:
DO
IF docname$ <> "" THEN LOCATE 4, 2: PRINT " "; docname$; " "
IF docname$ = "" THEN LOCATE 4, 2: PRINT " (Untitled) "
LOCATE 2, 59: PRINT "Pos: X -"; x; "Y -"; y
LOCATE 3, 59: PRINT "Page:"; INT(scry / 19)
LOCATE y + 4 - scry, x, 1
FOR i = 1 TO 1000
NEXT i
LOCATE y + 4 - scry, x, 0
a$ = INKEY$
IF a$ = CHR$(0) + ";" THEN
        LOCATE 1, 1: COLOR 15: PRINT "F1-NEW"
        COLOR 7
        FOR x = 1 TO 80
        FOR y = 1 TO 1000
        w(x, y) = 0
        NEXT y
        NEXT x
        GOTO start
ELSEIF a$ = CHR$(0) + "<" THEN
        LOCATE 1, 9: COLOR 15: PRINT "F2-SAVE"
        COLOR 7
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
        FOR x2 = 1 TO 80
        IF w(x2, y2) <> 0 THEN
                IF x2 < 80 THEN PRINT #1, CHR$(w(x2, y2));  ELSE PRINT #1, CHR$(w(x2, y2))
        ELSE
                IF x2 < 80 THEN PRINT #1, " ";  ELSE PRINT #1, " "
        END IF
        NEXT x2
        NEXT y2
        LOCATE 2, 1: COLOR 0
        PRINT "      "; name$
        PRINT "       "; name$
        COLOR 7
        LOCATE 1, 9: PRINT "F2-SAVE"
        CLOSE #1
ELSEIF a$ = CHR$(0) + "=" THEN
        LOCATE 1, 18: COLOR 15: PRINT "F3-SAVE AS"
        COLOR 7
        LOCATE 2, 1
        INPUT "NAME >", name$
        FOR i = 1 TO 80: LOCATE 4, i: PRINT CHR$(205): NEXT i
        docname$ = name$
        LOCATE 3, 1: PRINT "SAVING "; name$
        OPEN name$ FOR OUTPUT AS #1
        FOR y2 = 1 TO endofdoc
        FOR x2 = 1 TO 80
        IF w(x2, y2) <> 0 THEN
                IF x2 < 80 THEN PRINT #1, CHR$(w(x2, y2));  ELSE PRINT #1, CHR$(w(x2, y2))
        ELSE
                IF x2 < 80 THEN PRINT #1, " ";  ELSE PRINT #1, " "
        END IF
        NEXT x2
        NEXT y2
        LOCATE 2, 1: COLOR 0
        PRINT "      "; name$
        PRINT "       "; name$
        COLOR 7
        LOCATE 1, 18: PRINT "F3-SAVE AS"
        CLOSE #1
ELSEIF a$ = CHR$(0) + ">" THEN
        LOCATE 1, 30: COLOR 15: PRINT "F4-LOAD"
        COLOR 7
        LOCATE 2, 1: INPUT "NAME >", name$
        LOCATE 3, 1: PRINT "OPENING "; name$
        OPEN name$ FOR INPUT AS #1
        FOR i = 1 TO 80: LOCATE 4, i: PRINT CHR$(205): NEXT i
        docname$ = name$
        FOR y2 = 1 TO 1000
        endofdoc = y2 + 4
        LINE INPUT #1, in$
        FOR x2 = 1 TO 80
        IF MID$(in$, x2, 1) <> "" THEN w(x2, y2) = ASC(MID$(in$, x2, 1)) ELSE w(x2, y2) = 0
        NEXT x2
        NEXT y2
skipload:
        LOCATE 2, 1: COLOR 0
        PRINT "      "; name$
        PRINT "        "; name$
        COLOR 7
        x = 1
        y = 1
        scry = 0
        GOSUB redraw
        LOCATE 1, 30: PRINT "F4-LOAD"
        CLOSE #1
ELSEIF a$ = CHR$(0) + "G" THEN
        x = 1
ELSEIF a$ = CHR$(0) + "O" THEN
        FOR i = 80 TO 0 STEP -1
        IF w(i, y) <> 0 OR i = 0 THEN x = i + 1: EXIT FOR
        NEXT i
        IF x = 81 THEN x = 80
ELSEIF a$ = CHR$(0) + "S" THEN
        FOR i = x TO 79
        w(i, y) = w(i + 1, y)
        NEXT i
        w(80, y) = 0
        GOSUB redraw
ELSEIF a$ = CHR$(0) + "Q" THEN
        y = y + 19
        scry = scry + 19
        IF y > endofdoc THEN y = endofdoc: scry = endofdoc - 19
        IF scry < 0 THEN scry = 0
        IF y > 1000 THEN y = 1000: scry = 981: BEEP
        GOSUB redraw
ELSEIF a$ = CHR$(0) + "I" THEN
        y = y - 19
        IF y < 1 THEN y = 1
        scry = scry - 19
        IF scry < 0 THEN scry = 0
        GOSUB redraw
ELSEIF a$ = CHR$(27) OR a$ = CHR$(0) + "?" THEN
        LOCATE 1, 39: COLOR 15: PRINT "F5-QUIT"
        COLOR 7
        SYSTEM
ELSEIF a$ = CHR$(0) + "H" THEN
        y = y - 1
        IF y - scry <= 0 AND scry > 0 THEN scry = scry - 1: GOSUB redraw
        IF y < 1 THEN y = 1
ELSEIF a$ = CHR$(0) + "P" THEN
        y = y + 1
        IF y > endofdoc THEN y = endofdoc
        IF y > 1000 THEN y = 1000: BEEP
        IF y - scry >= 20 THEN scry = scry + 1: GOSUB redraw
ELSEIF a$ = CHR$(0) + "M" THEN
        x = x + 1
        IF x = 81 THEN x = 80
ELSEIF a$ = CHR$(0) + "K" THEN
        x = x - 1
        IF x = 0 THEN x = 1
ELSEIF a$ = CHR$(13) THEN
        LOCATE y + 4 - scry, x
        PRINT " "
        IF y < 1000 THEN
                x2 = 0
                FOR i = x TO 80
                IF w(i, y) = 0 THEN EXIT FOR
                x2 = x2 + 1
                w(x2, y + 1) = w(i, y)
                w(i, y) = 0
                NEXT i
        END IF
        x = 1
        y = y + 1
        endofdoc = endofdoc + 1
        IF y > 1000 THEN y = 1000: endofdoc = endofdoc - 1: BEEP
        IF y - scry >= 20 THEN scry = scry + 1
        GOSUB redraw
ELSEIF a$ = CHR$(8) THEN
        IF x = 1 AND y = 1 THEN a$ = "": GOTO skip
        x = x - 1
        IF x = 0 THEN
                y = y - 1
                endofdoc = endofdoc - 1
                FOR i = 80 TO 0 STEP -1
                IF w(i, y) <> 0 OR i = 0 THEN x = i + 1: EXIT FOR
                NEXT i
        ELSE
                LOCATE y + 4 - scry, x
                PRINT " "
                w(x, y) = 0
                FOR i = x TO 79
                w(i, y) = w(i + 1, y)
                NEXT i
                w(80, y) = 0
                GOSUB redraw
        END IF
        IF y - scry <= 0 AND scry > 0 THEN scry = scry - 1: GOSUB redraw
skip:
ELSEIF a$ <> "" THEN
        LOCATE y + 4 - scry, x
        PRINT a$
        w(x, y) = ASC(a$)
        x = x + 1
        IF x = 81 THEN x = 1: y = y + 1: endofdoc = endofdoc + 1
        IF y > 1000 THEN y = 1000: endofdoc = endofdoc + 1: BEEP
        IF y - scry >= 20 THEN scry = scry + 1: GOSUB redraw
END IF
LOOP

redraw:
FOR xx = 1 TO 80
FOR yy = scry + 1 TO scry + 19
IF yy > 1000 THEN EXIT FOR
LOCATE yy + 4 - scry, xx
IF w(xx, yy) >= 32 THEN PRINT CHR$(w(xx, yy))
IF w(xx, yy) = 0 THEN PRINT " "
NEXT yy
NEXT xx
RETURN
handler:
IF ERR = 62 THEN
        RESUME skipload
ELSEIF ERR = 53 THEN
        LOCATE 2, 1: COLOR 0: PRINT "      "; name$
        LOCATE 3, 1: PRINT "        "; name$
        LOCATE 2, 1: COLOR 7: PRINT "FILE NOT FOUND"
        LOCATE 3, 1: PRINT "PRESS ANY KEY"
        DO: LOOP UNTIL INKEY$ <> ""
        LOCATE 2, 1: PRINT "              "
        LOCATE 3, 1: PRINT "             "
        LOCATE 1, 30: PRINT "F4-LOAD"
        RESUME errreturn
ELSEIF ERR = 64 OR ERR = 52 THEN
        LOCATE 2, 1: COLOR 0: PRINT "      "; name$
        LOCATE 3, 1: PRINT "        "; name$
        LOCATE 2, 1: COLOR 7: PRINT "BAD FILENAME"
        LOCATE 3, 1: PRINT "PRESS ANY KEY"
        DO: LOOP UNTIL INKEY$ <> ""
        LOCATE 2, 1: PRINT "            "
        LOCATE 3, 1: PRINT "             "
        LOCATE 1, 9: PRINT "F2-SAVE  F3-SAVE AS  F4-LOAD"
        RESUME errreturn
ELSE
        CLS
        PRINT "ERROR #"; ERR
        PRINT "PRESS ANY KEY TO RESTART PROGRAM"
        PRINT "YOU WILL LOSE ANY UNSAVED INFORMATION"
        DO: LOOP UNTIL INKEY$ <> ""
        FOR x = 1 TO 80
        FOR y = 1 TO 1000
        w(x, y) = 0
        NEXT y
        NEXT x
        RESUME start
END IF
END

