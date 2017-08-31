start:
CLS
SCREEN 12
RANDOMIZE TIMER
x1& = INT(RND * 640) + 1
y1& = INT(RND * 480) + 1
x2& = INT(RND * 640) + 1
y2& = INT(RND * 480) + 1
x3& = INT(RND * 640) + 1
y3& = INT(RND * 480) + 1
dir1& = INT(RND * 4) + 1
dir2& = INT(RND * 4) + 1
dir3& = INT(RND * 4) + 1
live1& = 1
live2& = 1
live3& = 1
tim = 250
DO
FOR delay = 1 TO 10000
NEXT delay
'IF tim > 0 THEN tim = tim - 1
xx = x3&
yy = y3&
olddir = dir3&
FOR i = 1 TO 15
        IF dir3& = 1 THEN
                yy = yy - 1
        ELSEIF dir3& = 2 THEN
                xx = xx + 1
        ELSEIF dir3& = 3 THEN
                yy = yy + 1
        ELSEIF dir3& = 4 THEN
                xx = xx - 1
        END IF
        IF xx > 640 THEN xx = 0
        IF xx < 0 THEN xx = 639
        IF yy > 480 THEN yy = 0
        IF yy < 0 THEN yy = 479
        IF POINT(xx, yy) > 0 AND live3& = 1 THEN dir3& = dir3& + (INT(RND * 2) * 2) - 1: EXIT FOR
NEXT i
'IF live2& = 0 THEN dir3& = olddir
IF dir3& < 1 THEN dir3& = 4
IF dir3& > 4 THEN dir3& = 1
IF dir3& <> olddir THEN
        xx = x3&
        yy = y3&
        IF dir3& = 1 THEN
                yy = yy - 1
        ELSEIF dir3& = 2 THEN
                xx = xx + 1
        ELSEIF dir3& = 3 THEN
                yy = yy + 1
        ELSEIF dir3& = 4 THEN
                xx = xx - 1
        END IF
        IF POINT(xx, yy) > 0 THEN dir3& = olddir
END IF

xx = x2&
yy = y2&
olddir = dir2&
FOR i = 1 TO 15
        IF dir2& = 1 THEN
                yy = yy - 1
        ELSEIF dir2& = 2 THEN
                xx = xx + 1
        ELSEIF dir2& = 3 THEN
                yy = yy + 1
        ELSEIF dir2& = 4 THEN
                xx = xx - 1
        END IF
        IF xx > 640 THEN xx = 0
        IF xx < 0 THEN xx = 639
        IF yy > 480 THEN yy = 0
        IF yy < 0 THEN yy = 479
        IF POINT(xx, yy) > 0 AND live2& = 1 THEN dir2& = dir2& + (INT(RND * 2) * 2) - 1: EXIT FOR
NEXT i
IF dir2& < 1 THEN dir2& = 4
IF dir2& > 4 THEN dir2& = 1
IF dir2& <> olddir THEN
        xx = x2&
        yy = y2&
        IF dir2& = 1 THEN
                yy = yy - 1
        ELSEIF dir2& = 2 THEN
                xx = xx + 1
        ELSEIF dir2& = 3 THEN
                yy = yy + 1
        ELSEIF dir2& = 4 THEN
                xx = xx - 1
        END IF
        IF POINT(xx, yy) > 0 THEN dir2& = olddir
END IF

xx = x1&
yy = y1&
olddir = dir1&
FOR i = 1 TO 15
        IF dir1& = 1 THEN
                yy = yy - 1
        ELSEIF dir1& = 2 THEN
                xx = xx + 1
        ELSEIF dir1& = 3 THEN
                yy = yy + 1
        ELSEIF dir1& = 4 THEN
                xx = xx - 1
        END IF
        IF xx > 640 THEN xx = 0
        IF xx < 0 THEN xx = 639
        IF yy > 480 THEN yy = 0
        IF yy < 0 THEN yy = 479
        IF POINT(xx, yy) > 0 AND live1& = 1 THEN dir1& = dir1& + (INT(RND * 2) * 2) - 1: EXIT FOR
NEXT i
IF dir1& < 1 THEN dir1& = 4
IF dir1& > 4 THEN dir1& = 1
'dir1& = olddir
IF dir1& <> olddir THEN
        xx = x1&
        yy = y1&
        IF dir1& = 1 THEN
                yy = yy - 1
        ELSEIF dir1& = 2 THEN
                xx = xx + 1
        ELSEIF dir1& = 3 THEN
                yy = yy + 1
        ELSEIF dir1& = 4 THEN
                xx = xx - 1
        END IF
        IF POINT(xx, yy) > 0 THEN dir1& = olddir
END IF

check& = POINT(x1&, y1&)
IF check& > 0 THEN live1& = 0
check& = POINT(x2&, y2&)
IF check& > 0 THEN live2& = 0
check& = POINT(x3&, y3&)
IF check& > 0 THEN live3& = 0

PSET (x1&, y1&), 11
PSET (x2&, y2&), 13
PSET (x3&, y3&), 14

move$ = INKEY$
IF move$ = "z" THEN
        dir1& = dir1& - 1
ELSEIF move$ = "x" THEN
        dir1& = dir1& + 1
ELSEIF move$ = "," THEN
        dir2& = dir2& - 1
        IF live2& = 0 THEN dir3& = dir3& - 1
ELSEIF move$ = "." THEN
        dir2& = dir2& + 1
        IF live2& = 0 THEN dir3& = dir3& + 1
ELSEIF move$ = "`" THEN
        END
END IF

IF dir1& < 1 THEN dir1& = 4
IF dir1& > 4 THEN dir1& = 1
IF dir2& < 1 THEN dir2& = 4
IF dir2& > 4 THEN dir2& = 1
IF dir3& < 1 THEN dir3& = 4
IF dir3& > 4 THEN dir3& = 1
IF x1& > 639 THEN x1& = 0
IF x1& < 0 THEN x1& = 639
IF y1& > 479 THEN y1& = 0
IF y1& < 0 THEN y1& = 479
IF x2& > 639 THEN x2& = 0
IF x2& < 0 THEN x2& = 639
IF y2& > 479 THEN y2& = 0
IF y2& < 0 THEN y2& = 479
IF x3& > 639 THEN x3& = 0
IF x3& < 0 THEN x3& = 639
IF y3& > 479 THEN y3& = 0
IF y3& < 0 THEN y3& = 479

IF live1& = 1 THEN
        IF dir1& = 1 THEN
                y1& = y1& - 1
        ELSEIF dir1& = 2 THEN
                x1& = x1& + 1
        ELSEIF dir1& = 3 THEN
                y1& = y1& + 1
        ELSEIF dir1& = 4 THEN
                x1& = x1& - 1
        END IF
END IF

IF live2& = 1 THEN
        IF dir2& = 1 THEN
                y2& = y2& - 1
        ELSEIF dir2& = 2 THEN
                x2& = x2& + 1
        ELSEIF dir2& = 3 THEN
                y2& = y2& + 1
        ELSEIF dir2& = 4 THEN
                x2& = x2& - 1
        END IF
END IF

IF live3& = 1 THEN
        IF dir3& = 1 THEN
                y3& = y3& - 1
        ELSEIF dir3& = 2 THEN
                x3& = x3& + 1
        ELSEIF dir3& = 3 THEN
                y3& = y3& + 1
        ELSEIF dir3& = 4 THEN
                x3& = x3& - 1
        END IF
END IF

IF live1& = 0 THEN
        IF live2& = 0 THEN
                LOCATE 12, 15
                PRINT "3 Wins"
                GOSUB ending
                END
        ELSEIF live3& = 0 THEN
                LOCATE 12, 25
                PRINT "2 Wins"
                GOSUB ending
                END
        END IF
ELSEIF live2& = 0 AND live3& = 0 THEN
        LOCATE 12, 25
        PRINT "1 Wins"
        GOSUB ending
        END
END IF

LOOP

ending:         
LOCATE 13, 25
PRINT "Press Return to Start Over"
SLEEP
GOTO start

