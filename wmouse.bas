DECLARE SUB MouseDriver (AX%, bx%, CX%, DX%, lb%, RB%, EX%)

DIM SHARED mouse$
SCREEN 12
MouseDriver 1, bx%, CX%, DX%, lb%, RB%, 1
DO
MouseDriver 3, bx%, CX%, DX%, lb%, RB%, 0
COLOR 15
LOCATE 1, 1: PRINT lb%
LOCATE 2, 1: PRINT RB%
LOCATE 3, 1: PRINT CX%
LOCATE 4, 1: PRINT DX%
LOOP
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00

SUB MouseDriver (AX%, bx%, CX%, DX%, lb%, RB%, EX%)
IF EX% = 1 THEN
mouse$ = SPACE$(57)
FOR I% = 1 TO 57
READ A$
H$ = CHR$(VAL("&H" + A$))
MID$(mouse$, I%, 1) = H$
NEXT I%




CLS
END IF
DEF SEG = VARSEG(mouse$)
CALL Absolute(AX%, bx%, CX%, DX%, SADD(mouse$))
lb% = ((bx% AND 1) <> 0)
RB% = ((bx% AND 2) <> 0)
END SUB

