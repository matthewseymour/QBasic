SCREEN 12
RANDOMIZE TIMER
SLEEP
DO
DO
a = INT(RND * 6) + 1
b = INT(RND * 6) + 1
LOOP UNTIL (a <> b)
FOR i = 1 TO 238
CIRCLE (240, 240), i, (INT(RND * 2) * 8) + 1, b, a
CIRCLE (240, 240), i, 0, a, b
NEXT i
SWAP a, b
FOR i = 1 TO 238
CIRCLE (240, 240), i, (INT(RND * 2) * 8) + 1, b, a
CIRCLE (240, 240), i, 0, a, b
NEXT i
LOOP

