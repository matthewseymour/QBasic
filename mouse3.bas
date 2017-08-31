'***
' QB.BI - Assembly Support Include File
'
'       Copyright <C> 1987 Microsoft Corporation
'
' Purpose:
'      This include file defines the types and gives the DECLARE
'       statements for the assembly language routines ABSOLUTE,
'       INTERRUPT, INTERRUPTX, INT86OLD, and INT86XOLD.
'
'***************************************************************************
'
' Define the type needed for INTERRUPT
'
TYPE RegType
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
END TYPE
'
' Define the type needed for INTERUPTX
'
TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE
'
'                 DECLARE statements for the 5 routines
'                 -------------------------------------
'
' Generate a software interrupt, loading all but the segment registers
'
DECLARE SUB INTERRUPT (intnum AS INTEGER, inreg AS RegType, outreg AS RegType)
'
' Generate a software interrupt, loading all registers
'
DECLARE SUB INTERRUPTX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
'
' Call a routine at an absolute address.
' NOTE: If the routine called takes parameters, then they will have to
'       be added to this declare statement before the parameter given.
'
DECLARE SUB ABSOLUTE (address AS INTEGER)
'
' Generate a software interrupt, loading all but the segment registers
'       (old version)
'
DECLARE SUB INT86OLD (intnum AS INTEGER, inarray() AS INTEGER, outarray() AS INTEGER)
'
' Gemerate a software interrupt, loading all the registers
'       (old version)
'
DECLARE SUB INT86XOLD (intnum AS INTEGER, inarray() AS INTEGER, outarray() AS INTEGER)
DIM regs AS RegType
DIM mousebg(36) AS INTEGER
DIM mouse(36) AS INTEGER
DIM mousem(36) AS INTEGER
DIM cx(360) AS DOUBLE
DIM cy(360) AS DOUBLE
Pi = 3.141592
FOR i = 0 TO 360
   cx(i) = SIN(i * (Pi / 180))
   cy(i) = COS(i * (Pi / 180))
NEXT

SCREEN 13

DATA 39,37,35,33,31,29
DATA 37,29,27,25,23,00
DATA 35,27,25,00,00,00
DATA 33,25,00,00,00,00
DATA 31,23,00,00,00,00
DATA 29,00,00,00,00,00
FOR x = 1 TO 6
FOR y = 1 TO 6
READ c
PSET (x, y), c
IF c = 0 THEN c = 255
PSET (x, y + 6), c
NEXT y
NEXT x
GET (1, 1)-(6, 6), mouse
GET (1, 7)-(5, 12), mousem
CLS
ox = 10
oy = 10
GET (ox, oy)-(ox + 6, oy + 6), mousebg

FOR i = 0 TO 20: PALETTE i, i * 3 + 256 * (i * 3) + 65536 * (i * 3): NEXT i
FOR i = 21 TO 40: PALETTE i, 65536 * ((i - 20) * 3): NEXT i
FOR i = 41 TO 60: PALETTE i, 256 * ((i - 40) * 3): NEXT i
FOR i = 61 TO 80: PALETTE i, 256 * ((i - 60) * 3) + 65536 * ((i - 60) * 3): NEXT i
FOR i = 81 TO 100: PALETTE i, ((i - 80) * 3): NEXT i
FOR i = 101 TO 120: PALETTE i, ((i - 100) * 3) + ((i - 100) * 3) * 65536: NEXT i
FOR i = 121 TO 140: PALETTE i, ((i - 120) * 3) + ((i - 120) * 3) * 256: NEXT i
FOR i = 141 TO 160: PALETTE i, ((i - 140) * 3) + INT((i - 140) * 1.5) * 256: NEXT i

DO
regs.ax = 3
CALL INTERRUPT(&H33, regs, regs)
x = regs.cx / 2
y = regs.dx
b1 = regs.bx AND 1
b2 = regs.bx AND 2

PUT (ox, oy), mousebg, PSET
ox = x - 1
oy = y - 4
IF ox < 0 THEN ox = 0
IF oy < 0 THEN oy = 0
IF ox > 314 THEN ox = 314
IF oy > 194 THEN oy = 194
LOCATE 1, 1
PRINT ox; oy; "    "

GET (ox, oy)-(ox + 5, oy + 5), mousebg
PUT (ox, oy), mousem, AND
PUT (ox, oy), mouse, OR
FOR i = 1 TO 10000
NEXT i
a$ = INKEY$
IF a$ = CHR$(27) THEN END
LOOP

