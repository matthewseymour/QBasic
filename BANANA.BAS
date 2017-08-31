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
IF tim > 0 THEN tim = tim - 1
xx = x3&
yy = y3&

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
ELSEIF move$ = "." THEN
        dir2& = dir2& + 1
ELSEIF move$ = "*" THEN
        dir3& = dir3& - 1
ELSEIF move$ = "-" THEN
        dir3& = dir3& + 1
ELSEIF move$ = "`" THEN
        END
END IF

IF dir1& < 1 THEN dir1& = 4
IF dir1& > 4 THEN dir1& = 1
IF dir2& < 1 THEN dir2& = 4
IF dir2& > 4 THEN dir2& = 1
IF dir3& < 1 THEN dir3& = 4
IF dir3& > 4 THEN dir3& = 1
IF x1& >= 640 THEN x1& = 0
IF x1& < 0 THEN x1& = 640
IF y1& >= 480 THEN y1& = 0
IF y1& < 0 THEN y1& = 480
IF x2& >= 640 THEN x2& = 0
IF x2& < 0 THEN x2& = 640
IF y2& >= 480 THEN y2& = 0
IF y2& < 0 THEN y2& = 480
IF x3& >= 640 THEN x3& = 0
IF x3& < 0 THEN x3& = 640
IF y3& >= 480 THEN y3& = 0
IF y3& < 0 THEN y3& = 480

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

