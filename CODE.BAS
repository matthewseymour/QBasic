
CLS
COLOR 1
FOR y = 1 TO 21
FOR x = 1 TO 60
DO
a& = INT(RND * 255) + 1
LOOP UNTIL (a& <> 12 AND a& <> 7)
LOCATE y, x
PRINT CHR$(a&)
NEXT x
NEXT y

DO
DO
FOR x = 1 TO 60
DO
d& = d& + 1
IF d& = 50 THEN d& = 0
PALETTE 1, d&
'a& = INT(RND * 39) + 179
a& = INT(RND * 254) + 1
LOOP UNTIL (a& <> 12 AND (a& > 32 OR a& < 28) AND a& <> 7 AND a& <> 10 AND a& <> 13 AND a& <> 32 AND a& <> 26 AND a& <> 9)
d = d + 1
IF d = 16 THEN d = 1
'COLOR d
LOCATE 23, x
PRINT CHR$(a&)
IF x = 60 THEN PRINT
NEXT x
FOR i = 1 TO 5000
NEXT i
b$ = INKEY$
LOOP UNTIL (b$ = " ")
SLEEP
LOOP

