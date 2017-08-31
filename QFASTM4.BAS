' QFASTM4.BAS  -  QBasic Mandelbrot Set program.
'   Jim Phillips 1998
'   Draws 255-color image of MSet.
'   Uses periodicity testing for coordinates in Mandelbrot "lake".
'   Runs on Pentium133 in 24.2 seconds.
'---------------------------------------------------------------------------

DEFSNG A-Z

SCREEN 13

LOCATE 24, 2
	PRINT "Ctrl+Break to exit";

ScrnWide% = 320
Scrnhigh% = 200
 
MaxIteration% = 255

MultFactor& = 1000
Kickout& = 4 * MultFactor& * MultFactor&
HalfMult& = MultFactor& \ 2
TopY& = 1.2 * MultFactor&
LeftX& = -2.1 * MultFactor&
YDist& = 2.388 * MultFactor&
XDist& = 3.19 * MultFactor&

XSpace& = XDist& / (ScrnWide% - 1)
YSpace& = YDist& / (Scrnhigh% - 1)

StartTime = TIMER

	'The IF...THEN test with InLakeFlag sets a value which we test against to
	'see if the iteration values have entered a periodic (infinite) loop.  At
	'a few selected iterations we update this test value because a periodic
	'loop has not always begun by an early iteration.  If the test value
	'repeats, we have a periodic loop; the coordinate is in the MSet lake.
	'Iteration values for testing against InLakeFlag were determined by trial
	'and error.
FOR Ypixel& = 0 TO Scrnhigh% - 1
	y& = TopY& - Ypixel& * YSpace&

	InLakeFlag% = 0

	FOR Xpixel& = 0 TO ScrnWide% - 1
		x& = LeftX& + Xpixel& * XSpace&

		RealPart& = x&
		ImagPart& = y&

		Iter% = 1
		DO
			RPartSq& = RealPart& * RealPart&
			IPartSq& = ImagPart& * ImagPart&
			SizeSq& = RPartSq& + IPartSq&

			IF SizeSq& > Kickout& THEN
				EXIT DO
			END IF

			IF InLakeFlag% THEN
				IF Iter% > 17 THEN
					IF Iter% = 18 OR Iter% = 37 OR Iter% = 60 OR Iter% = 112 THEN
						InLakeTest& = SizeSq& \ MultFactor&
					 ELSEIF SizeSq& \ MultFactor& = InLakeTest& THEN
						Iter% = MaxIteration%
						EXIT DO
					END IF
				END IF
			END IF

			ImagPart& = (RealPart& * ImagPart&) \ HalfMult& + y&
			RealPart& = (RPartSq& - IPartSq&) \ MultFactor& + x&
			 
			Iter% = Iter% + 1
		LOOP WHILE Iter% < MaxIteration%

		IF Iter% = MaxIteration% THEN
			InLakeFlag% = 1
		END IF

		PSET (Xpixel&, Ypixel&), Iter%
	 
	NEXT Xpixel&
NEXT Ypixel&

	ElapsedTime = TIMER - StartTime

	LOCATE 24, 2
		PRINT ElapsedTime; "seconds ";

END

