' QFASTM3.BAS  -  QBasic Mandelbrot Set program.
'   Jim Phillips 1998
'   Draws black-on-white image of MSet.
'   Uses integer math for speed.
'   Runs on Pentium133 in 60 seconds.
'---------------------------------------------------------------------------

DEFSNG A-Z

SCREEN 13

LOCATE 24, 2
	PRINT "Ctrl+Break to exit";

ScrnWide% = 320
ScrnHigh% = 200
MaxIteration% = 255

MultFactor& = 1000     'Multiply decimals by this to convert them to integers.
Kickout& = 4 * MultFactor& * MultFactor&    'If size Z becomes greater than
																						'this, the point is outside set.
HalfMult& = MultFactor& \ 2     'Used in inner loop; calculate here for speed.
TopY& = 1.2 * MultFactor&       'Top of coordinate plane on screen.
LeftX& = -2.1 * MultFactor&     'Left side of coordinate plane.
YDist& = 2.388 * MultFactor&    'Length of y-side.
XDist& = 3.19 * MultFactor&     'Length of x-side.

XSpace& = XDist& / (ScrnWide% - 1)
YSpace& = YDist& / (ScrnHigh% - 1)

StartTime = TIMER

FOR Ypixel& = 0 TO ScrnHigh% - 1
	y& = TopY& - Ypixel& * YSpace&
	FOR Xpixel& = 0 TO ScrnWide% - 1
		x& = LeftX& + Xpixel& * XSpace&

		RealPart& = x&
		ImagPart& = y&

		PixColor% = 0

		FOR Iter% = 1 TO MaxIteration%

			RPartSq& = RealPart& * RealPart&
			IPartSq& = ImagPart& * ImagPart&

			IF (RPartSq& + IPartSq&) > Kickout& THEN
				PixColor% = 15
				EXIT FOR
			END IF

			ImagPart& = (RealPart& * ImagPart&) \ HalfMult& + y&
			RealPart& = (RPartSq& - IPartSq&) \ MultFactor& + x&

		NEXT Iter%

		PSET (Xpixel&, Ypixel&), PixColor%

	NEXT Xpixel&
NEXT Ypixel&

	ElapsedTime = TIMER - StartTime
		LOCATE 24, 2
		PRINT ElapsedTime; "seconds ";

END

