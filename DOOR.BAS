topscore& = 0
topplayer$ = "-"
start:
CLS
SCREEN 12
a& = 0
score& = 0
DO
a& = a& + 1
PRINT "Open Door #"; a&; "?"
INPUT "", a$

IF a$ <> "n" THEN
        RANDOMIZE TIMER
        w& = INT(RND * 10) + 1
        IF w& <> 1 THEN
                RANDOMIZE TIMER
                s& = INT(RND * 1001)
                score& = score& + s&
                PRINT "Your score is now"; score&; "!"
        ELSEIF w& = 1 THEN
                PRINT "You have lost!"
                GOTO finish
        END IF
ELSEIF a$ = "n" THEN
        PRINT "You have quit with the score of"; score&; "."
        GOTO endd
END IF
LOOP
endd:
IF score& > topscore& THEN
        PRINT "You have beaten today's top player with your score of "; score&; "!"
        PRINT "What is your name?"
        INPUT "", topplayer$
        topscore& = score&
END IF
finish:
IF topscore& = 0 THEN PRINT "There is no top player for today."
IF topscore& > 0 THEN PRINT "The top player for today is "; topplayer$; " with a score of"; score&; "."
PRINT "Play again?"
INPUT "", q$
IF q$ = "y" THEN GOTO start

