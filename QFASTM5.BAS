' QFASTM5.BAS  -  QBasic Mandelbrot Set program.
'   Jim Phillips 1998
'   Draws 255-color image of MSet.
'   Uses "paint box" method to speed coloring areas of same color.
'   Runs on Pentium133 in 11.3 seconds.
'---------------------------------------------------------------------------

DEFSNG A-Z

SCREEN 13

ScrnWide% = 320
ScrnHigh% = 200
MaxIteration% = 255
MultFactor& = 1000
Kickout& = 4 * MultFactor& * MultFactor&
HalfMultFactor& = MultFactor& \ 2
TopY& = 1.2 * MultFactor&
LeftX& = -2.1 * MultFactor&
YDist& = 2.388 * MultFactor&
XDist& = 3.19 * MultFactor&

XSpace& = XDist& / (ScrnWide% - 1)
YSpace& = YDist& / (ScrnHigh% - 1)

'CALL changepalette

BoxSide% = 3      'Number of pixels we step over or down when outlining
									'boxes to check for painting a single color.  Smaller is
									'more accurate, larger is faster, 3 is good compromise.
BoxRowsMem% = ScrnWide% * BoxSide%   'Distance in video memory from pixel on
									'box's 1st row to pixel in same column on box's last row.
BoxTotalMem% = BoxRowsMem% + BoxSide%   'Distance in video memory from upper
									'left corner of box to lower right corner of box.

DEF SEG = &HA000      'The color video segment.

StartTime = TIMER

''''''''''''''' Start of first pass. '''''''''''''''

'CalcRow is the screen row where we are calculating, CalcCol is the screen
'column.  In the nested IF tests and FOR loops where we PEEK and POKE to
'the screen, we check to see if the pixel box's lower right corner matches
'its lower left corner, then upper right, then upper left.  If all the
'corners match, we assume it is safe to color every pixel inside this box
'the same color.

CalcRow& = 0

DO
	y& = TopY& - CalcRow& * YSpace&

	InLakeFlag% = 0
	CalcCol% = 0

	DO
		x& = LeftX& + CalcCol% * XSpace&

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

			ImagPart& = (RealPart& * ImagPart&) \ HalfMultFactor& + y&
			RealPart& = (RPartSq& - IPartSq&) \ MultFactor& + x&

			Iter% = Iter% + 1
		LOOP WHILE Iter% < MaxIteration%

		IF Iter% = MaxIteration% THEN
			InLakeFlag% = 1
		END IF

		PixelOffset& = ScrnWide% * CalcRow& + CalcCol%
		POKE PixelOffset&, Iter%

	IF CalcRow& > 0 AND CalcCol% > 0 THEN
		IF Iter% = PEEK(PixelOffset& - BoxSide%) THEN
			IF Iter% = PEEK(PixelOffset& - BoxRowsMem%) THEN
				IF Iter% = PEEK(PixelOffset& - BoxTotalMem%) THEN
					PaintOffset& = PixelOffset& - BoxTotalMem%
					FOR PaintRow% = 0 TO BoxSide%
						FOR Stepper% = 0 TO BoxSide%
							POKE (PaintOffset& + Stepper%), Iter%
						NEXT Stepper%
						PaintOffset& = PaintOffset& + ScrnWide%
					NEXT PaintRow%
				END IF
			END IF
		END IF
	END IF

	CalcCol% = CalcCol% + BoxSide%
LOOP WHILE CalcCol% < ScrnWide%

	CalcRow& = CalcRow& + BoxSide%
LOOP WHILE CalcRow& < ScrnHigh%

''''''''''''''' End of first pass, start of second. '''''''''''''''

CalcRow& = 0

'In the following nested DO loops, we check each pixel to see if it is
'colored; if it is, skip it.  (If it's in the MSet lake, set the
'InLakeFlag.)  If it isn't colored, do it:
DO
	Ypixel& = CalcRow&
	y& = TopY& - Ypixel& * YSpace&

	VideoRowMem& = ScrnWide% * CalcRow&
	InLakeFlag% = 0
	CalcCol% = 0

	DO
		x& = LeftX& + CalcCol% * XSpace&

		ColorCheck% = PEEK(VideoRowMem& + CalcCol%)
		IF ColorCheck% = MaxIteration% THEN
			InLakeFlag% = 1
		 ELSEIF ColorCheck% = 0 THEN

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

				ImagPart& = (RealPart& * ImagPart&) \ HalfMultFactor& + y&
				RealPart& = (RPartSq& - IPartSq&) \ MultFactor& + x&

				Iter% = Iter% + 1
			LOOP WHILE Iter% < MaxIteration%

			IF Iter% = MaxIteration% THEN
				InLakeFlag% = 1
			END IF

			PixelOffset& = ScrnWide% * Ypixel& + CalcCol%
			POKE PixelOffset&, Iter%

		END IF

	CalcCol% = CalcCol% + 1
LOOP WHILE CalcCol% < ScrnWide%

	CalcRow& = CalcRow& + 1
LOOP WHILE CalcRow& < ScrnHigh%

''''''''''''''' End of second pass. '''''''''''''''

	ElapsedTime = TIMER - StartTime

	DEF SEG     'We are finished drawing....

LOCATE 24, 2
	PRINT ElapsedTime; "seconds ";

END

SUB changepalette
	'Change colors in the DAC palette:
	FOR PalColor% = 0 TO 63
		OUT &H3C8, PalColor%
		OUT &H3C9, 63 - PalColor%
		OUT &H3C9, 0
		OUT &H3C9, PalColor%
	NEXT PalColor%
	FOR PalColor% = 64 TO 127
		OUT &H3C8, PalColor%
		OUT &H3C9, 0
		OUT &H3C9, PalColor% - 64
		OUT &H3C9, 63
	NEXT PalColor%
	FOR PalColor% = 128 TO 191
		OUT &H3C8, PalColor%
		OUT &H3C9, PalColor% - 128
		OUT &H3C9, 63
		OUT &H3C9, 191 - PalColor%
	NEXT PalColor%
	FOR PalColor% = 192 TO 254
		OUT &H3C8, PalColor%
		OUT &H3C9, 63
		OUT &H3C9, 63
		OUT &H3C9, PalColor% - 191
	NEXT PalColor%
END SUB

