SCREEN 12
CLS
DIM n(1 TO 56) AS INTEGER
si = 10

DO
n(1) = n(1) + 1
t& = t& + 1
IF n(1) = 2 THEN
        t& = t& + 1
        n(2) = n(2) + 1
        n(1) = 0
END IF
IF n(1) = 1 THEN
        CIRCLE (1 * si, 5), 5, 0, , , 2
        LINE (1 * si, 1)-((1 * si), 10), 15
ELSE
        LINE (1 * si, 1)-((1 * si), 10), 0
        CIRCLE (1 * si, 5), 5, 15, , , 2
END IF

'LOCATE 1, 2
'PRINT n(1)
LOCATE 3, 1
PRINT t&
FOR i = 2 TO 56


IF n(56) = 1 THEN GOTO finish
IF n(i) = 2 THEN
        t& = t& + 1
        n(i + 1) = n(i + 1) + 1
        n(i) = 0
END IF

IF n(i) = 1 THEN
        CIRCLE (i * si, 5), 5, 0, , , 2
        LINE (i * si, 1)-((i * si), 10), 15
ELSE
        LINE (i * si, 1)-((i * si), 10), 0
        CIRCLE (i * si, 5), 5, 15, , , 2
END IF

LOCATE 3, 1
PRINT t&
NEXT i
LOOP
finish:
PRINT "Total number is"; t&
SLEEP

