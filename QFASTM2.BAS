' QFASTM2.BAS  -  QBasic Mandelbrot Set program.
'   Jim Phillips 1998
'   Draws black-on-white image of MSet.
'   Some streamlining done to main loop to gain speed.
'   Runs on Pentium133 in 3 minutes 45 seconds.
'---------------------------------------------------------------------------

DEFSNG A-Z

SCREEN 13

LOCATE 24, 2
	PRINT "Ctrl+Break to exit";

ScrnWide% = 320
ScrnHigh% = 200
MaxIteration% = 255
TopY = 1.2
LeftX = -2.1
YDist = 2.388
XDist = 3.19

XSpace = XDist / (ScrnWide% - 1)
YSpace = YDist / (ScrnHigh% - 1)

StartTime = TIMER     'To see how fast this is.

	'When testing size of Z, it's faster to compare the sum of (Real part
	'squared + Imaginary part squared) to the value 4 and thereby avoid
	'calculating the square root.  Also, calculate the squares into
	'temporary variables so that we only do those multiplications once.
	'Further, note that * multiplying is much faster than ^2 squaring.
	'And, remove remarks from the loops so the QBasic interpreter doesn't
	'have to continually read them.
FOR Ypixel% = 0 TO ScrnHigh% - 1
	y = TopY - Ypixel% * YSpace
	FOR Xpixel% = 0 TO ScrnWide% - 1
		x = LeftX + Xpixel% * XSpace

		RealPart = x
		ImagPart = y

		PixColor% = 0
													 
		FOR Iter% = 1 TO MaxIteration%

			RPartSq = RealPart * RealPart
			IPartSq = ImagPart * ImagPart

			IF (RPartSq + IPartSq) > 4 THEN
				PixColor% = 15
				EXIT FOR
			END IF

			ImagPart = 2 * RealPart * ImagPart + y
			RealPart = RPartSq - IPartSq + x

		NEXT Iter%

		PSET (Xpixel%, Ypixel%), PixColor%

	NEXT Xpixel%
NEXT Ypixel%

	ElapsedTime = TIMER - StartTime
		LOCATE 24, 2
		PRINT ElapsedTime; "seconds ";

END

