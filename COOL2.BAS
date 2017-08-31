start:
speed& = 2
RANDOMIZE TIMER
x% = INT(RND * 640) + 1
y% = INT(RND * 480) + 1
SCREEN 12
INPUT "Color type 1, 2 ", ccc&
INPUT "Reset?, y/n ", reset$
INPUT "Solid?,y/n ", solid$
IF solid$ = "y" THEN speed& = 1
INPUT "Press space to stop at any time", hgds$
CLS
st:
        tip% = tip% + 1
        IF reset$ = "y" THEN
                IF tip% = 2000 THEN
                        CLS
                        tip% = 0
                END IF
        END IF
        RANDOMIZE TIMER
        b% = INT(RND * 50) + 25
        il% = 0
        RANDOMIZE TIMER
        cococ& = INT(RND * 15) + 1
        CIRCLE (x%, y%), 5, cococ&
        PSET (x%, y%), 1
        RANDOMIZE TIMER
        ans1% = INT(RND * 9) + 1
sti:
        IF ans1% = 5 THEN
                GOTO st
        ELSEIF ans1% = 7 THEN
                x% = x% - 1
                y% = y% - 1
        ELSEIF ans1% = 8 THEN
                y% = y% - speed&
        ELSEIF ans1% = 9 THEN
                y% = y% - 1
                x% = x% + 1
        ELSEIF ans1% = 6 THEN
                x% = x% + speed&
        ELSEIF ans1% = 4 THEN
                x% = x% - speed&
        ELSEIF ans1% = 3 THEN
                x% = x% + 1
                y% = y% + 1
        ELSEIF ans1% = 2 THEN
                y% = y% + speed&
        ELSEIF ans1% = 1 THEN
                y% = y% + 1
                x% = x% - 1
        END IF
        IF INKEY$ = " " THEN GOTO endd
        il% = il% + 1
        IF il% >= b% THEN GOTO st
        IF y% > 480 THEN y% = 478
        IF y% < 0 THEN y% = 2
        IF x% > 640 THEN x% = 638
        IF x% < 0 THEN x% = 2
        RANDOMIZE TIMER
        w% = INT(RND * 16)
        IF ccc& = 1 THEN
                CIRCLE (x%, y%), 5, w%
        ELSE
                CIRCLE (x%, y%), 5, cococ&
        END IF
        GOTO sti

endd:
INPUT "Start again or Quit? S, Q ", statr$
IF statr$ = "q" THEN END ELSE GOTO start

        

