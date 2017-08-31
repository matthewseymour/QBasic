DECLARE SUB SetColour (c%, r%, g%, B%)
'
'  - Affine Texture Mapper (TEXMAP.BAS)
'
'  - By Duncan Scott & Alan O'Hagan (CGI Joe)
'  - http://shimmer.zext.net/
'
'  - 7th Sept, 2001
'
'
DECLARE SUB tritexture (texture(), x1%, y1%, u1%, v1%, x2%, y2%, u2%, v2%, x3%, y3%, u3%, v3%, texnum)
DECLARE FUNCTION MYPOINT! (x%, y%)
DECLARE SUB HTLine (texture(), x1%, x2%, y%, u1&, u2&, v1&, v2&, texnum)
COMMON SHARED col0r%, col0g%, col0b%, col1r%, col1g%, col1b%, col2r%, col2g%, col2b%
REM $DYNAMIC
DIM texture(20, 20, 100)
DIM polyx(3, 100)
DIM polyy(3, 100)
DIM polyz(3, 100)
DIM texpolyx(3, 100)
DIM texpolyy(3, 100)
DIM polytex(100)
DIM cx(-360 TO 720) AS DOUBLE
DIM cy(-360 TO 720) AS DOUBLE
Pi = 3.141592
FOR i = -360 TO 720
   a = a + 1
   IF a > 360 THEN a = a - 360
   cx(i) = SIN(a * (Pi / 180))
   cy(i) = COS(a * (Pi / 180))
NEXT

RANDOMIZE TIMER

SCREEN 13
FOR i = 0 TO 20: PALETTE i, i * 3 + 256 * (i * 3) + 65536 * (i * 3): NEXT i
FOR i = 21 TO 30: PALETTE i, 65536 * ((i - 20) * 6): NEXT i
FOR i = 31 TO 40: PALETTE i, (i - 30) * 6 + 256 * (i - 30) * 6 + 65536 * 60: NEXT i
FOR i = 41 TO 50: PALETTE i, 256 * ((i - 40) * 6): NEXT i
FOR i = 51 TO 60: PALETTE i, (i - 50) * 6 + 256 * 60 + 65536 * (i - 50) * 6: NEXT i
FOR i = 61 TO 70: PALETTE i, 256 * ((i - 60) * 6) + 65536 * ((i - 60) * 6): NEXT i
FOR i = 71 TO 80: PALETTE i, (i - 70) * 6 + 256 * 60 + 65536 * 60: NEXT i
FOR i = 81 TO 90: PALETTE i, ((i - 80) * 6): NEXT i
FOR i = 91 TO 100: PALETTE i, 60 + 256 * (i - 90) * 6 + 65536 * (i - 90) * 6: NEXT i
FOR i = 101 TO 110: PALETTE i, ((i - 100) * 6) + ((i - 100) * 6) * 65536: NEXT i
FOR i = 111 TO 120: PALETTE i, 60 + 256 * (i - 110) * 6 + 65536 * 60: NEXT i
FOR i = 121 TO 130: PALETTE i, ((i - 120) * 6) + ((i - 120) * 6) * 256: NEXT i
FOR i = 131 TO 140: PALETTE i, 60 + 256 * 60 + 65536 * (i - 130) * 6: NEXT i
FOR i = 141 TO 150: PALETTE i, ((i - 140) * 6) + ((i - 140) * 3) * 256: NEXT i
FOR i = 151 TO 160: PALETTE i, 60 + 256 * (30 + ((i - 150) * 3)) + 65536 * (i - 150) * 6: NEXT i
LINE (1, 1)-(20, 20), 10, B
LINE (1, 1)-(20, 1), 15, B
LINE (1, 1)-(1, 20), 15, B
FOR x = 2 TO 19
FOR y = 2 TO 19
PSET (x, y), INT(RND * 10) + 10
NEXT
NEXT
texnum = 1
GOSUB maketex
CLS

'tritexture texture(), (x1), (y1), (tex-x1), (tex-y1), (x2), (y2), (tex-x2), (tex-y2), (x3),(y3), (tex-x3), (tex-y3), tex#
polytex(1) = 1
polyx(1, 2) = -100
polyy(1, 2) = 0
polyz(1, 2) = 100
polyx(2, 2) = -100
polyy(2, 2) = 100
polyz(2, 2) = 0
polyx(3, 2) = 100
polyy(3, 2) = 100
polyz(3, 2) = 100
texpolyx(1, 1) = 1
texpolyy(1, 1) = 1
texpolyx(2, 1) = 120
texpolyy(2, 1) = 1
texpolyx(3, 1) = 120
texpolyy(3, 1) = 120

polytex(1) = 1
polyx(1, 2) = -100
polyy(1, 2) = 0
polyz(1, 2) = 100
polyx(2, 2) = -100
polyy(2, 2) = 100
polyz(2, 2) = 100
polyx(3, 2) = 100
polyy(3, 2) = 100
polyz(3, 2) = 100
texpolyx(1, 2) = 1
texpolyy(1, 2) = 1
texpolyx(2, 2) = 1
texpolyy(2, 2) = 120
texpolyx(3, 2) = 120
texpolyy(3, 2) = 120

        
'maxdist = 1000
ch = 3
DO
FOR i = 1 TO 2

x1% = 160 + (polyx(1, i) - ((polyy(1, i) * ch)))
y1% = 100 + (polyz(1, i) - ((polyy(1, i) * ch)))
x2% = 160 + (polyx(2, i) - ((polyy(2, i) * ch)))
y2% = 100 + (polyz(2, i) - ((polyy(2, i) * ch)))
x3% = 160 + (polyx(3, i) - ((polyy(3, i) * ch)))
y3% = 100 + (polyz(3, i) - ((polyy(3, i) * ch)))

texx1% = texpolyx(1, i)
texy1% = texpolyy(1, i)
texx2% = texpolyx(2, i)
texy2% = texpolyy(2, i)
texx3% = texpolyx(3, i)
texy3% = texpolyy(3, i)

tritexture texture(), x1%, y1%, texx1%, texy1%, x2%, y2%, texx2%, texy2%, x3%, y3%, texx3%, texy3%, 1
NEXT i
a = a + 1
IF TIMER - B >= 1 THEN
        B = TIMER
        fps = a
        a = 0
END IF
LOCATE 1, 1
'PRINT fps
in$ = INKEY$
IF in$ = "x" THEN polyx(1, 1) = polyx(1, 1) + 1
IF in$ = "y" THEN polyy(1, 1) = polyy(1, 1) + 1
IF in$ = "z" THEN polyz(1, 1) = polyz(1, 1) + 1
IF in$ = CHR$(27) THEN EXIT DO
IF an > 360 THEN an = an - 360
IF an < 0 THEN an = an + 360
LOOP
SYSTEM
maketex:
FOR y% = 1 TO 21
FOR x% = 1 TO 21
texture(x% - 1, y% - 1, texnum) = POINT(x%, y%)
NEXT x%
NEXT y%
RETURN

REM $STATIC
SUB HTLine (texture(), x1%, x2%, y%, u1&, u2&, v1&, v2&, texnum)
IF x1% > x2% THEN SWAP x1%, x2%: SWAP u1&, u2&: SWAP v1&, v2&
xdiff% = 1 + x2% - x1%
du& = (u2& - u1&) \ xdiff%
dv& = (v2& - v1&) \ xdiff%
u& = u1&
v& = v1&
FOR l% = x1% TO x2%
u% = (u& / 65536)' AND 31
v% = (v& / 65536)' AND 31
DO
IF u% > 20 THEN u% = u% - 20
IF v% > 20 THEN v% = v% - 20
LOOP UNTIL u% <= 20 AND v% <= 20

col% = texture(u%, v%, texnum)
PSET (l%, y%), col%
u& = u& + du&
v& = v& + dv&
NEXT l%

END SUB

SUB SetColour (c%, r%, g%, B%)

r% = r% AND 63
g% = g% AND 63
B% = B% AND 63
c% = c% AND 255

OUT &H3C8, c%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, B%


END SUB

SUB SetGourandPal
FOR c% = 1 TO 63
r% = col0r% + (c% * (col1r% - col0r%) / 64)    ' Fade white to col1
g% = col0g% + (c% * (col1g% - col0g%) / 64)
B% = col0b% + (c% * (col1b% - col0b%) / 64)
SetColour c%, r%, g%, B%
NEXT c%

FOR c% = 64 TO 191
r% = col1r% + ((c% - 64) * (col2r% - col1r%) / 128)
g% = col1g% + ((c% - 64) * (col2g% - col1g%) / 128)
B% = col1b% + ((c% - 64) * (col2b% - col1b%) / 128)
SetColour c%, r%, g%, B%
NEXT c%

FOR c% = 192 TO 255
r% = col2r% + ((c% - 192) * (col0r% - col2r%) / 64)
g% = col2g% + ((c% - 192) * (col0g% - col2g%) / 64)
B% = col2b% + ((c% - 192) * (col0b% - col2b%) / 64)
SetColour c%, r%, g%, B%
NEXT c%

END SUB

SUB tritexture (texture(), x1%, y1%, u1%, v1%, x2%, y2%, u2%, v2%, x3%, y3%, u3%, v3%, texnum)

'DEF SEG = textseg%
'LINE (x1%, y1%)-(x2%, y2%), 2    '  triangle outline
'LINE (x2%, y2%)-(x3%, y3%), 2    '
'LINE (x3%, y3%)-(x1%, y1%), 2    '

IF y1% > y2% THEN SWAP y1%, y2%: SWAP x1%, x2%: SWAP u1%, u2%: SWAP v1%, v2%
IF y1% > y3% THEN SWAP y1%, y3%: SWAP x1%, x3%: SWAP u1%, u3%: SWAP v1%, v3%
IF y2% > y3% THEN SWAP y2%, y3%: SWAP x2%, x3%: SWAP u2%, u3%: SWAP v2%, v3%

'       (1)
'      /   \
'    (2)
'         \  (3)

' need to check for divide by zero - Not any more!
' call me mad! find more madness elsewhere for less and
' get double the difference refunded back, that's right
' double the difference ......... GUARANTEED!

d1& = 0: dg1 = 0

ydiffa% = y2% - y1%
IF ydiffa% THEN
d1& = ((x2% - x1%) * 65536) \ ydiffa%
ud1& = ((u2% - u1%) * 65536) \ ydiffa%
vd1& = ((v2% - v1%) * 65536) \ ydiffa%
END IF

ydiffb% = y3% - y2%
IF ydiffb% THEN
d2& = ((x3% - x2%) * 65536) \ ydiffb%
ud2& = ((u3% - u2%) * 65536) \ ydiffb%
vd2& = ((v3% - v2%) * 65536) \ ydiffb%
END IF

ydiffc% = y3% - y1%
IF ydiffc% THEN
d3& = ((x3% - x1%) * 65536) \ ydiffc%
ud3& = ((u3% - u1%) * 65536) \ ydiffc%
vd3& = ((v3% - v1%) * 65536) \ ydiffc%
END IF

lx& = x1% * 65536
rx& = x1% * 65536

lu& = u1% * 65536: ru& = lu&
lv& = v1% * 65536: rv& = lv&
FOR y% = y1% TO y2% - 1
HTLine texture(), (lx& \ 65536), (rx& \ 65536), y%, (lu&), (ru&), (lv&), (rv&), texnum
  lx& = lx& + d1&
  rx& = rx& + d3&
  lu& = lu& + ud1&
  ru& = ru& + ud3&
  lv& = lv& + vd1&
  rv& = rv& + vd3&
NEXT

lx& = (x2% * 65536)
lu& = u2% * 65536
lv& = v2% * 65536

' HERE d1 has been added (y2%-y1%)+1 times to lx  = x2
'      d3 has been added (y2%-y1%)+1 times to rx

FOR y% = y2% TO y3%

  HTLine texture(), (lx& \ 65536), (rx& \ 65536), y%, (lu&), (ru&), (lv&), (rv&), texnum

  lx& = lx& + d2&
  rx& = rx& + d3&
  lu& = lu& + ud2&
  ru& = ru& + ud3&
  lv& = lv& + vd2&
  rv& = rv& + vd3&
NEXT y%

END SUB

