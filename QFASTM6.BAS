DECLARE SUB ChangePalette ()
' QFASTM6.BAS  -  QBasic Mandelbrot Set program.
'   Jim Phillips 1998
'   Draws 255-color low-power zoom into MSet.
'   Runs on Pentium133 in 25.2 seconds.
'---------------------------------------------------------------------------

DEFSNG A-Z

SCREEN 13

ScrnWide% = 320
ScrnHigh% = 200
MaxIteration% = 255
MultFactor& = 8000
Kickout& = 4 * MultFactor& * MultFactor&
HalfMultFactor& = MultFactor& \ 2
TopY& = .45 * MultFactor&
LeftX& = -1.35 * MultFactor&
YDist& = .2985 * MultFactor&
XDist& = .39875 * MultFactor&

XSpace& = XDist& / (ScrnWide% - 1)
YSpace& = YDist& / (ScrnHigh% - 1)

BoxSide% = 3
BoxRowsMem% = ScrnWide% * BoxSide%
BoxTotalMem% = BoxRowsMem% + BoxSide%

CALL ChangePalette

DEF SEG = &HA000

StartTime = TIMER

''''''''''''''' Start of first pass. '''''''''''''''

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

	WHILE INKEY$ = ""
	WEND

	DEF SEG

LOCATE 24, 2
	PRINT ElapsedTime; "seconds ";

END

SUB ChangePalette
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

