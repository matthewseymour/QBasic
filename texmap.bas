'
'  - Affine Texture Mapper (TEXMAP.BAS)
'
'  - By Duncan Scott & Alan O'Hagan (CGI Joe)
'  - http://shimmer.zext.net/
'
'  - 7th Sept, 2001
'
'
DECLARE SUB SetGourandPal ()
DECLARE SUB tritexture (textseg%, x1%, y1%, u1%, v1%, x2%, y2%, u2%, v2%, x3%, y3%, u3%, v3%)
DECLARE FUNCTION MYPOINT! (x%, y%)
DECLARE SUB HTLine (x1%, x2%, y%, u1&, u2&, v1&, v2&)
DECLARE SUB SetColour (c%, r%, g%, B%)

COMMON SHARED col0r%, col0g%, col0b%, col1r%, col1g%, col1b%, col2r%, col2g%, col2b%
DIM texture%(32 * 16)

RANDOMIZE TIMER

SCREEN 13

col0r% = 63: col0g% = 63: col0b% = 63
col1r% = 63: col1g% = 63: col1b% = 0
col2r% = 0: col2g% = 0: col2b% = 63
SetGourandPal

LINE (0, 0)-(64, 64), 63, BF

FOR t% = 0 TO 5000
PSET (RND * 64, RND * 64), 50 + RND * 150
NEXT t%


'LINE (0, 0)-(31, 31), 170, B ' Subtle intentional mesh effect
                             ' to allow eye to distingusih motion
                             ' more readily - experiment with remming
                             ' this line out to see what it looks like


' Algorithmic softening routine:

DEF SEG = VARSEG(texture%(0))
FOR y% = 0 TO 31 '  012
FOR x% = 0 TO 31 '  3x4
of% = (y% * 32) + x%
p0% = MYPOINT(x% - 1, y% - 1)
p1% = MYPOINT(x%, y% - 1)
p2% = MYPOINT(x% + 1, y% - 1)
p3% = MYPOINT(x% - 1, y%)
p4% = MYPOINT(x% + 1, y%)
p5% = MYPOINT(x% - 1, y% + 1)
p6% = MYPOINT(x%, y% + 1)
p7% = MYPOINT(x% + 1, y% + 1)
px% = MYPOINT(x%, y%)
px% = (px% + p0% + p1% + p2% + p3% + p4% + p5% + p6% + p7%) / 9
POKE of%, px%
NEXT x%
NEXT y%

SetColour 2, 63, 63, 63

DO
tritexture VARSEG(texture%(0)), RND * 320, RND * 200, 0, 0, RND * 320, RND * 200, 63, 0, RND * 320, RND * 200, 0, 63
LOOP UNTIL INKEY$ <> ""

SCREEN 0: WIDTH 80

SUB HTLine (x1%, x2%, y%, u1&, u2&, v1&, v2&)

IF x1% > x2% THEN SWAP x1%, x2%: SWAP u1&, u2&: SWAP v1&, v2&
xdiff% = 1 + x2% - x1%

du& = (u2& - u1&) \ xdiff%
dv& = (v2& - v1&) \ xdiff%

u& = u1&
v& = v1&

FOR l% = x1% TO x2%
u% = (u& \ 65536) AND 31
v% = (v& \ 65536) AND 31
col% = PEEK(u% + v% * 32)
PSET (l%, y%), col%
u& = u& + du&
v& = v& + dv&
NEXT l%

END SUB

FUNCTION MYPOINT (x%, y%)
IF x% < 0 OR y% < 0 THEN MYPOINT = 125 ELSE MYPOINT = POINT(x%, y%)
END FUNCTION

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

SUB tritexture (textseg%, x1%, y1%, u1%, v1%, x2%, y2%, u2%, v2%, x3%, y3%, u3%, v3%)

DEF SEG = textseg%
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
HTLine (lx& \ 65536), (rx& \ 65536), y%, (lu&), (ru&), (lv&), (rv&)
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

  HTLine (lx& \ 65536), (rx& \ 65536), y%, (lu&), (ru&), (lv&), (rv&)

  lx& = lx& + d2&
  rx& = rx& + d3&
  lu& = lu& + ud2&
  ru& = ru& + ud3&
  lv& = lv& + vd2&
  rv& = rv& + vd3&
NEXT y%

END SUB

