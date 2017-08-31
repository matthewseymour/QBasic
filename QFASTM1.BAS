' QFASTM1.BAS  -  QBasic Mandelbrot Set program.
'   Jim Phillips 1998
'   Draws black-on-white image of Mandelbrot Set to illustrate how slow the
'     mathematically correct floating-point method is.
'   Runs on Pentium133 in 38 minutes 56 seconds.
'---------------------------------------------------------------------------

DEFSNG A-Z      'Variables default to single-precision floating point.

SCREEN 13      '320x200 256-color mode.

LOCATE 24, 2
	PRINT "Ctrl+Break to exit";

ScrnWide% = 320           'Number of pixels on x axis.
ScrnHigh% = 200           'Number of pixels on y axis.
MaxIteration% = 255    'Number of iterations beyond which we assume the
											 'coordinate is inside the MSet.
TopY = 1.2        'Top of coordinate plane on screen.
LeftX = -2.1      'Left side of coordinate plane.
YDist = 2.388     'Length of y-side.  (199 spaces at .012 each.  2.388
									'gives 0 on MSet axis for a more symmetrical image.)
XDist = 3.19      'Length of x-side.  (319 spaces at .010 each; screen 13
									'has 5:6 pixel size ratio, 4:3 x:y aspect ratio.)
XSpace = XDist / (ScrnWide% - 1)     'Separation between x pixels.
YSpace = YDist / (ScrnHigh% - 1)     'Separation between y pixels.

StartTime = TIMER     'To see how fast (or slow) this is.

		'Following is method to calculate the Mandelbrot Set:
		'Initial RealPart = x   (the x coordinate on complex number plane)
		'Initial ImaginaryPart = y   (the y coordinate on complex number plane)
		'Initial Z = x + y*i   (where i is the square root of -1)
		'New Z = Z^2 + Z
		'      = x^2 + y^2*(-1) + 2*x*y*i + x + y*i
		'      = (RealPart^2 - ImaginaryPart^2 + x)   (new Real part)
		'        + (2*RealPart*ImaginaryPart*i + y*i)   (new Imaginary part)
		'If the square root of (RealPart^2 + ImaginaryPart^2) is greater
		'  than 2, the point (x,y) is outside of the Mandelbrot set.
		'  If not, iterate again and calculate New Z.

FOR Ypixel% = 0 TO ScrnHigh% - 1
	y = TopY - Ypixel% * YSpace
	FOR Xpixel% = 0 TO ScrnWide% - 1
		x = LeftX + Xpixel% * XSpace

		RealPart = x
		ImagPart = y

		PixColor% = 0     'Use black for points inside MSet.

		'This loop calculates if coordinate is inside or outside of the MSet:
		FOR Iter% = 1 TO MaxIteration%

			IF SQR(RealPart ^ 2 + ImagPart ^ 2) > 2 THEN
				'This point is outside the set, so change color to white:
				PixColor% = 15
				EXIT FOR       'No need to iterate this point any further.
			END IF

			'If we've come to here then the coordinate has not yet kicked out.
			'Update the real and imaginary factors of Z and continue:
			TempR = RealPart ^ 2 - ImagPart ^ 2 + x
			ImagPart = 2 * RealPart * ImagPart + y
			RealPart = TempR

		NEXT Iter%

		PSET (Xpixel%, Ypixel%), PixColor%    'Color the coordinate on screen.

	NEXT Xpixel%
NEXT Ypixel%

	ElapsedTime = TIMER - StartTime
	 
LOCATE 24, 2
	PRINT ElapsedTime; "seconds ";

END

