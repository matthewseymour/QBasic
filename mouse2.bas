'
'
'Mouse (which)
'      0 = Mouse Init
'      1 = Mouse Driver
'      2 = Mouse Put
'      3 = Mouse Status
'      4 = Mouse Hide
'      5 = Mouse Show
'
DECLARE SUB Mouse (which!)
DIM SHARED mouser$, mouseleftbutton, mouserightbutton, mousexpos, mouseypos, AX%, bx%, cx%, dx%
SCREEN 13
FOR i = 0 TO 20: PALETTE i, i * 3 + 256 * (i * 3) + 65536 * (i * 3): NEXT i
FOR i = 21 TO 40: PALETTE i, 65536 * ((i - 20) * 3): NEXT i
FOR i = 41 TO 60: PALETTE i, 256 * ((i - 40) * 3): NEXT i
FOR i = 61 TO 80: PALETTE i, 256 * ((i - 60) * 3) + 65536 * ((i - 60) * 3): NEXT i
FOR i = 81 TO 100: PALETTE i, ((i - 80) * 3): NEXT i
FOR i = 101 TO 120: PALETTE i, ((i - 100) * 3) + ((i - 100) * 3) * 65536: NEXT i
FOR i = 121 TO 140: PALETTE i, ((i - 120) * 3) + ((i - 120) * 3) * 256: NEXT i
FOR i = 141 TO 150: PALETTE i, ((i - 140) * 6) + ((i - 140) * 3) * 256: NEXT i

Mouse 0

CLS
FOR i = 0 TO 150
LINE (i, 191)-(i, 199), i
NEXT i
DO
LINE (-1, -1)-(270, 190), 20, B
LINE (164, 192)-(171, 199), 20, B
LINE (165, 193)-(170, 198), col1, BF
LINE (174, 192)-(181, 199), 20, B
LINE (175, 193)-(180, 198), col2, BF
Mouse 3
LOCATE 1, 35: COLOR 20
PRINT "X"; INT(mousexpos / 2)
LOCATE 2, 35: COLOR 20
PRINT "Y"; mouseypos
LOCATE 3, 35: COLOR 20
PRINT mouseleftbutton
LOCATE 4, 35: COLOR 20
PRINT mouserightbutton
IF mouseleftbutton = -1 AND mouseypos > 190 AND INT(mousexpos / 2) < 150 THEN col1 = INT(mousexpos / 2)
IF mouserightbutton = -1 AND mouseypos > 190 AND INT(mousexpos / 2) < 150 THEN col2 = INT(mousexpos / 2)

IF mouseleftbutton = -1 AND mouseypos < 190 AND INT(mousexpos / 2) < 270 THEN PSET (INT(mousexpos / 2), mouseypos), col1
IF mouserightbutton = -1 AND mouseypos < 190 AND INT(mousexpos / 2) < 270 THEN PSET (INT(mousexpos / 2), mouseypos), col2

a$ = INKEY$
IF a$ = CHR$(27) THEN END
LOOP





DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00

SUB Mouse (which)
'0 = Mouse Init
'1 = Mouse Driver
'2 = Mouse Put
'3 = Mouse Status
'4 = Mouse Hide
'5 = Mouse Show
IF which = 0 THEN
  mouser$ = SPACE$(57)
  FOR i% = 1 TO 57
    READ a$
    H$ = CHR$(VAL("&H" + a$))
    MID$(mouser$, i%, 1) = H$
  NEXT i%
  AX% = 0
  bx% = 0
  cx% = 0
  dx% = 0
  Mouse 1
  MouseInit% = AX%
  Mouse 5
END IF
IF which = 1 THEN
  DEF SEG = VARSEG(mouser$)
  mouser% = SADD(mouser$)
  CALL Absolute(AX%, bx%, cx%, dx%, mouser%)
END IF
IF which = 2 THEN
  AX% = 4
  cx% = X%
  dx% = y%
  bx% = 0
  Mouse 1
END IF
IF which = 3 THEN
  AX% = 3
  Mouse 1
  mouseleftbutton = ((bx% AND 1) <> 0)
  mouserightbutton = ((bx% AND 2) <> 0)
  mousexpos = cx%
  mouseypos = dx%
END IF
IF which = 4 THEN
 AX% = 2
 bx% = 0
 cx% = 0
 dx% = 0
 Mouse 1
END IF
IF which = 5 THEN
  AX% = 1
  bx% = 0
  cx% = 0
  dx% = 0
  Mouse 1
END IF
END SUB

