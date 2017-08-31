COMMON SHARED supergame&
CLS
start:
SCREEN 9
CIRCLE (320, 175), 100
CIRCLE (350, 135), 20
CIRCLE (350, 165), 20
CIRCLE (350, 150), 42
LOCATE 1, 1
PRINT "GREG DALE'S MAGIC 8-BALL!"
LOCATE 2, 1
INPUT "Press return to see the answer. Q to quit. >", go$
IF go$ = "" THEN GOTO begin
IF go$ = "q" THEN GOTO Theend

begin:
CLS
RANDOMIZE TIMER
 w% = INT(RND * 10) + 1
 LOCATE 14, 36
 IF w% = 1 THEN PRINT "Yes"
 IF w% = 2 THEN PRINT "No"
 IF w% = 3 THEN PRINT "Of course"
 IF w% = 4 THEN PRINT "No way"
 IF w% = 5 THEN PRINT "Ask later"
 IF w% = 6 THEN PRINT "Speak up."
 IF w% = 7 THEN PRINT "You bet"
 IF w% = 8 THEN PRINT "Yeah, right"
 IF w% = 9 THEN PRINT "Probably"
 IF w% = 10 THEN PRINT "Parhaps"
 GOTO start

Theend:
IF supergame& = 1 THEN CHAIN "C:\DOS\games.bas"
LOCATE 3, 1
INPUT "Quit? y/n> ", quit$
IF quit$ = "y" THEN END
IF quit$ = "n" THEN GOTO start






