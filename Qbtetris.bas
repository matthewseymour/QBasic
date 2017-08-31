'ллл QBASIC TETRIS VERSION 1.0 ллл

'The first QBasic game that finds its data files by itself!
'If there are still errors THEN GOTO LINE 96

'Programmed by Dominik Kaspar
'              Ahornweg 15
'              5615 Fahrwangen
'              Switzerland
'              pedok@pop.agri.com

DECLARE SUB DetectWorkingDir ()
DECLARE SUB PlaySnd (Effect!)
DECLARE SUB CheckHighScore ()
DECLARE SUB CheckHighScoreFile ()
DECLARE SUB Logo (XPos!, YPos!, Size!, Text$, Colour, Frame)
DECLARE SUB TitleScreen ()
DECLARE SUB Prompt ()
DECLARE SUB PromptDifficulty ()
DECLARE SUB ViewHighScore ()
DECLARE SUB Description ()
DECLARE SUB ChooseShapes ()
DECLARE SUB CheckGameOver ()
DECLARE SUB EraseLines ()
DECLARE SUB ShowNextShape ()
DECLARE SUB CheckGrid ()
DECLARE SUB CheckShapeL ()
DECLARE SUB CheckShapeR ()
DECLARE SUB ReallyQuit ()
DECLARE SUB TurnShape ()
DECLARE SUB DrawShape (x!, y!, Colour)
DECLARE SUB ClearGrid ()
DECLARE SUB DrawGrid ()
DECLARE SUB PlayTetris ()
DECLARE SUB MainScreen ()
DECLARE SUB Intro ()
DECLARE SUB FontPrint (XPos!, YPos!, Text$, Colour!, Rotate)
DECLARE SUB InitializeFont ()
DECLARE SUB InitializeShapes ()

TYPE ShapeType
     Code       AS STRING * 25
     Difficulty AS INTEGER
END TYPE

TYPE HiScoreType
     PlayerName  AS STRING * 24
     PlayerScore AS LONG
END TYPE

DIM SHARED CurDir$

DIM SHARED HiScore AS HiScoreType
DIM SHARED Visitors AS LONG
DIM SHARED CurHiScore AS LONG
DIM SHARED CurHiName AS STRING * 24

DIM SHARED Shape(1 TO 44, -5 TO 10, -5 TO 10) AS INTEGER
DIM SHARED ShapeDiff(1 TO 44) AS INTEGER
DIM SHARED IniShape AS ShapeType
DIM SHARED ShapeNow, ShapeNext AS INTEGER
DIM SHARED ShapePosX, ShapePosY

DIM SHARED NewShape(1 TO 5, 1 TO 5)

DIM SHARED Grid(-5 TO 25, -5 TO 27)

DIM SHARED Font AS STRING * 30
DIM SHARED FontChar(0 TO 127, 1 TO 5, 1 TO 6) AS INTEGER
           
DIM SHARED SelectedItem(1 TO 7) AS STRING

DIM SHARED GameDifficulty AS INTEGER
DIM SHARED GameSpeed, GameSpeedBonus
DIM SHARED GameSpeed$
DIM SHARED RandomShapes AS INTEGER
DIM SHARED Lines AS INTEGER
DIM SHARED Score AS LONG
DIM SHARED GameOver AS INTEGER

DIM SHARED DelayTimer

DIM SHARED Overlap AS INTEGER

DIM SHARED HelpLine AS STRING * 46

DIM SHARED SoundFX AS INTEGER

ON KEY(1) GOSUB Help
ON KEY(2) GOSUB SoundEffects
   KEY(1) ON
   KEY(2) ON

CLS

'CurDir$ = "C:\QB45\QBTETRIS"        'if errors occur, enable this line
DetectWorkingDir                     'and delete this second line!

InitializeShapes
InitializeFont

RANDOMIZE TIMER

SCREEN 7
CLS

CheckHighScoreFile

TitleScreen

MainScreen

END

Help: Description
      RETURN

SoundEffects:
      KEY(2) OFF
      PCOPY 0, 2
      LINE (98, 85)-(222, 109), 0, BF
      LINE (98, 85)-(222, 109), 1, B
      LINE (99, 86)-(221, 108), 9, B
      IF SoundFX THEN
         PlaySnd 3
         SoundFX = 0
         FontPrint 109, 95, "Sound is now [OFF]", 6, 0
      ELSE
         SoundFX = 1
         PlaySnd 4
         FontPrint 109, 95, "Sound is now [ON]", 6, 0
      END IF
      DO: LOOP UNTIL LEN(INKEY$)
      PCOPY 2, 0
      KEY(2) ON
      RETURN

SUB CheckGameOver

 FOR y = 1 TO 5
     FOR x = 1 TO 5
         IF Shape(ShapeNow, x, y) AND Grid(ShapePosX + x - 1, ShapePosY + y - 1) THEN
           
            PlaySnd 8
            GameOver = 1

            LINE (98, 82)-(222, 112), 0, BF
            LINE (98, 82)-(222, 112), 1, B
            LINE (99, 83)-(221, 111), 9, B

            FontPrint 108, 90, "The Game Is Over!", 6, 0
            FontPrint 108, 100, "Press enter...", 6, 0
           
            DO: LOOP UNTIL INKEY$ = CHR$(13)

            PlaySnd 2

            LINE (58, 62)-(262, 142), 0, BF
            LINE (58, 62)-(262, 142), 1, B
            LINE (59, 63)-(261, 141), 9, B
            LINE (59, 122)-(261, 122), 9

            FontPrint 70, 70, "Score", 4, 0
            FontPrint 70, 80, "+ Level Bonus", 6, 0
            FontPrint 70, 90, "+ Lines Bonus", 6, 0
            FontPrint 70, 100, "+ Speed Bonus", 6, 0
            FontPrint 70, 110, "+ Random Shapes Bonus", 6, 0
            FontPrint 70, 130, "Total Score", 4, 0

            Spc$ = SPACE$(7 - LEN(STR$(Score)))
            FontPrint 210, 70, Spc$ + STR$(Score), 4, 0
           
            LevelBonus = INT(Score * (GameDifficulty ^ 2 / 83))
            Spc$ = SPACE$(7 - LEN(STR$(LevelBonus)))
            FontPrint 210, 80, Spc$ + STR$(LevelBonus), 6, 0
           
            LinesBonus = INT(Lines * GameDifficulty ^ 2 * (1.5 - GameSpeed))
            Spc$ = SPACE$(7 - LEN(STR$(INT(LinesBonus))))
            FontPrint 210, 90, Spc$ + STR$(LinesBonus), 6, 0

            SpeedBonus = CINT(Score * GameSpeedBonus)
            Spc$ = SPACE$(7 - LEN(STR$(SpeedBonus)))
            FontPrint 210, 100, Spc$ + STR$(SpeedBonus), 6, 0

            RndShpBonus = CINT(RandomShapes / 101 * Score)
            Spc$ = SPACE$(7 - LEN(STR$(RndShpBonus)))
            FontPrint 210, 110, Spc$ + STR$(RndShpBonus), 6, 0

            Score = Score + LevelBonus + LinesBonus + SpeedBonus + RndShpBonus
            Spc$ = SPACE$(7 - LEN(STR$(Score)))
            FontPrint 210, 130, Spc$ + STR$(Score), 4, 0

            DO: LOOP UNTIL LEN(INKEY$)

            CheckHighScore
            EXIT SUB
         END IF
     NEXT x
 NEXT y

END SUB

SUB CheckGrid
  
 Overlap = 0
 FOR y = 1 TO 5
     FOR x = 1 TO 5
         IF Shape(ShapeNow, x, y) AND Grid(ShapePosX + x - 1, ShapePosY + y) THEN
            Overlap = 1
         END IF
     NEXT x
 NEXT y
 
 IF Overlap THEN
    FOR y = 1 TO 5
        FOR x = 1 TO 5
            IF Shape(ShapeNow, x, y) THEN
               Grid(ShapePosX + x - 1, ShapePosY + y - 1) = 1
            END IF
        NEXT x
    NEXT y

    Score = Score + ShapeDiff(ShapeNow) * 10
    FontPrint 240, 90, STR$(Score), 3, 2

    EraseLines

    DrawGrid

    ChooseShapes

    ShapePosX = 8
    ShapePosY = 1
    DrawShape ShapePosX, ShapePosY, 2

    CheckGameOver
    IF GameOver = 1 THEN EXIT SUB

    ShowNextShape

 ELSE
    DrawShape ShapePosX, ShapePosY, 0
    ShapePosY = ShapePosY + 1
   
    DrawShape ShapePosX, ShapePosY, 2
 END IF

END SUB

SUB CheckHighScore

 OPEN CurDir$ + "QBTETRIS.HSC" FOR RANDOM AS #1 LEN = LEN(HiScore)

 FOR rec = 10 * GameDifficulty + 1 TO 10 * GameDifficulty + 10
     GET #1, rec, HiScore
     IF Score >= HiScore.PlayerScore THEN
        FOR i = 10 * GameDifficulty + 9 TO rec STEP -1
            GET #1, i, HiScore
            PUT #1, i + 1, HiScore
        NEXT i
       
        PlaySnd 7
        LINE (60, 64)-(260, 140), 0, BF
        FontPrint 70, 75, "Congratulations!", 3, 0
        FontPrint 70, 92, "You have reached the HighScore.", 2, 0
        FontPrint 70, 110, "Type in your name:", 4, 0

        YourName$ = ""
NamePrompt:
        FontPrint 70, 125, YourName$ + SPACE$(24 - LEN(YourName$)), 5, 2
        Key$ = ""
        DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

        SELECT CASE ASC(Key$)
          CASE 32 TO 127:
                  IF LEN(YourName$) < 24 THEN
                     YourName$ = YourName$ + Key$
                  END IF
                  GOTO NamePrompt
          CASE 8:
                  IF LEN(YourName$) > 0 THEN
                     YourName$ = LEFT$(YourName$, LEN(YourName$) - 1)
                  END IF
                  GOTO NamePrompt
          CASE 13:
          CASE ELSE: GOTO NamePrompt
        END SELECT
       
        HiScore.PlayerName = YourName$
        HiScore.PlayerScore = Score
        PUT #1, rec, HiScore
        EXIT FOR
     END IF
 NEXT rec

 FOR rec = 1 TO 10
     GET #1, rec, HiScore
     IF Score >= HiScore.PlayerScore THEN
        FOR i = 9 TO rec STEP -1
            GET #1, i, HiScore
            PUT #1, i + 1, HiScore
        NEXT i
        HiScore.PlayerName = YourName$
        HiScore.PlayerScore = Score
        PUT #1, rec, HiScore
        EXIT FOR
     END IF
 NEXT rec

 CLOSE #1

END SUB

SUB CheckHighScoreFile

 OPEN CurDir$ + "QBTETRIS.HSC" FOR RANDOM AS #1 LEN = LEN(HiScore)

 IF LOF(1) THEN
    GET #1, 71, HiScore
    Visitors = HiScore.PlayerScore
    HiScore.PlayerScore = Visitors + 1
    PUT #1, 71, HiScore
    CLOSE #1
    GOTO Anniversary
 END IF

 HiScore.PlayerName = "no name"
 HiScore.PlayerScore = 0
 FOR rec = 1 TO 70
     PUT #1, rec, HiScore
 NEXT rec
 HiScore.PlayerName = "QbTetris HighScore"
 HiScore.PlayerScore = 2
 PUT #1, 71, HiScore
 Visitors = 1

 CLOSE #1

Anniversary:
 v = Visitors
 IF v = 10 OR v = 25 OR v = 50 OR v = 100 OR v = 250 OR v = 500 OR v = 750 OR v = 1000 THEN
    Logo 20, 20, 4, "Anniversary!", 2, 8
    Logo 35, 70, 3, "You've started", 4, 8
    Logo 35, 100, 3, "this game for", 4, 8
    Logo 35, 130, 3, "the" + RTRIM$(STR$(Visitors)) + ". time!", 4, 8
    FontPrint 20, 170, "It really seems like you're into QbTetris!!", 1, 0
    FontPrint 20, 182, "Write comments to: pedok@pop.agri.ch", 1, 0
    DO: LOOP UNTIL LEN(INKEY$)
 ELSEIF v = 1 THEN
    Logo 16, 30, 6, "QBTETRIS", 2, 8
    FontPrint 60, 100, "This is the 1. time you're", 4, 0
    FontPrint 60, 115, "playing this game.", 4, 0
    FontPrint 60, 135, "Let's have a look at the", 1, 0
    FontPrint 60, 150, "game description...", 1, 0
    DO: LOOP UNTIL LEN(INKEY$)
    Description
 END IF

 CLS

END SUB

SUB CheckShapeL

 FOR y = 1 TO 5
     FOR x = 1 TO 5
         IF Shape(ShapeNow, x, y) AND Grid(ShapePosX + x - 2, ShapePosY + y - 1) THEN
            EXIT SUB
         END IF
     NEXT x
 NEXT y

 DrawShape ShapePosX, ShapePosY, 0
 ShapePosX = ShapePosX - 1
 DrawShape ShapePosX, ShapePosY, 2

END SUB

SUB CheckShapeR

 FOR y = 1 TO 5
     FOR x = 1 TO 5
         IF Shape(ShapeNow, x, y) AND Grid(ShapePosX + x, ShapePosY + y - 1) THEN
            EXIT SUB
         END IF
     NEXT x
 NEXT y

 DrawShape ShapePosX, ShapePosY, 0
 ShapePosX = ShapePosX + 1
 DrawShape ShapePosX, ShapePosY, 2

END SUB

SUB ChooseShapes
   
ShapeNow = ShapeNext

ChooseShapes:
 SELECT CASE GameDifficulty
        CASE 1: ShapeNext = INT(RND * 8) + 1
        CASE 2: ShapeNext = INT(RND * 15) + 1
        CASE 3: ShapeNext = INT(RND * 7) + 9
        CASE 4: ShapeNext = INT(RND * 26) + 1
        CASE 5: ShapeNext = INT(RND * 29) + 9
        CASE 6: ShapeNext = INT(RND * 36) + 9
 END SELECT

END SUB

SUB ClearGrid

 FOR y = 1 TO 22
     FOR x = 1 TO 20
         Grid(x, y) = 0
         IF x = 1 OR x = 20 THEN Grid(x, y) = 1
         IF y = 22 THEN Grid(x, y) = 1
     NEXT x
 NEXT y

 FOR i = 1 TO RandomShapes
SquareSet:
     x = INT(RND * 18) + 1
     y = INT(RND * 12) + 10
     IF Grid(x, y) THEN
        GOTO SquareSet
     ELSE
        Grid(x, y) = 1
     END IF
 NEXT i

END SUB

SUB Description

 KEY(1) OFF

 PCOPY 0, 1
 CLS
 
 OPEN CurDir$ + "QBTETRIS.DSC" FOR RANDOM AS #2 LEN = LEN(HelpLine)

 Logo 20, 15, 3, "Description:", 2, 8

 IF LOF(2) = 0 THEN
    CLOSE #2
    KILL CurDir$ + "QBTETRIS.DSC"
    FontPrint 20, 50, "ERROR! File missing: QBTETRIS.DSC", 4, 0
    FontPrint 20, 70, "Press a key to continue...", 1, 0
    DO: LOOP UNTIL LEN(INKEY$)
    GOTO ExitDescription
 END IF

 Selected = 1
DscPrompt:
 FOR rec = Selected * 15 - 14 TO Selected * 15
     GET #2, rec, HelpLine
     IF (rec - Selected * 15 + 15) = 1 THEN Colour = 4 ELSE Colour = 1
     FontPrint 20, 35 + (rec - Selected * 15 + 15) * 10, HelpLine, Colour, 2
 NEXT rec

 Key$ = "": DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

 SELECT CASE Key$
   CASE CHR$(0) + "H":
        Selected = Selected - 1
        IF Selected = 0 THEN Selected = 1 ELSE PlaySnd 1
        GOTO DscPrompt
   CASE CHR$(0) + "P":
        Selected = Selected + 1
        IF Selected = 5 THEN Selected = 4 ELSE PlaySnd 1
        GOTO DscPrompt
   CASE CHR$(13):
        Selected = Selected + 1
        IF Selected = 5 THEN Selected = 4 ELSE PlaySnd 1
        GOTO DscPrompt
   CASE CHR$(27):
   CASE ELSE: GOTO DscPrompt
 END SELECT

 CLOSE #2
 
ExitDescription:

 PCOPY 1, 0

 KEY(1) ON

END SUB

SUB DetectWorkingDir

 CurDir$ = ""

 OPEN "QBTETRIS.SHP" FOR RANDOM AS #1 LEN = LEN(IniShape)
      IF LOF(1) > 0 THEN
         CLOSE #1
         EXIT SUB
      END IF
 CLOSE #1
 KILL "QBTETRIS.SHP"

 PRINT "Detecting working directory...";

 SHELL "CD\"
 SHELL "DIR QBTETRIS.SHP /S /B > QBTETRIS.TMP"

 OPEN "QBTETRIS.TMP" FOR INPUT AS #1

 IF LOF(1) = 0 THEN
    PRINT : PRINT
    PRINT "ERROR: Could not detect working directory."
    PRINT "       There is a file missing (QBTETRIS.SHP)"
    PRINT
    SYSTEM
 END IF

 LINE INPUT #1, CurDir$
 CurDir$ = RTRIM$(LTRIM$(UCASE$(CurDir$)))

 FOR i = 1 TO LEN(CurDir$)
     IF MID$(CurDir$, i, 12) = "QBTETRIS.SHP" THEN
        CurDir$ = UCASE$(LEFT$(CurDir$, i - 1))
        EXIT FOR
     END IF
 NEXT i
 IF RIGHT$(CurDir$, 1) <> "\" THEN CurDir$ = CurDir$ + "\"

 CLOSE #1
 KILL "QBTETRIS.TMP"

 PRINT "OK."

END SUB

SUB DrawGrid

 FOR y = 1 TO 22
     FOR x = 1 TO 20
         IF Grid(x, y) = 1 THEN
            LINE (22 + 8 * x, 4 + 8 * y)-(30 + 8 * x - 1, 12 + 8 * y - 1), 6, BF
            LINE (22 + 8 * x, 4 + 8 * y)-(30 + 8 * x - 1, 12 + 8 * y - 1), 8, B
         ELSE
            LINE (22 + 8 * x, 4 + 8 * y)-(30 + 8 * x - 1, 12 + 8 * y - 1), 0, BF
         END IF
     NEXT x
 NEXT y

END SUB

SUB DrawShape (XPos, YPos, Colour)

 FOR y = 1 TO 5
     FOR x = 1 TO 5
         PosX = (22 + 8 * x) + (XPos * 8 - 8)
         PosY = (4 + 8 * y) + (YPos * 8 - 8)
         IF Shape(ShapeNow, x, y) THEN
            LINE (PosX, PosY)-(PosX + 7, PosY + 7), Colour, BF
            IF Colour THEN
               LINE (PosX, PosY)-(PosX + 7, PosY + 7), 8, B
            END IF
         END IF
     NEXT x
 NEXT y

END SUB

SUB EraseLines

 ErasedLines = 0
 FOR y = 1 TO 21
     Gap = 0
     FOR x = 2 TO 19
         IF Grid(x, y) = 0 THEN Gap = 1
     NEXT x

     IF Gap = 0 THEN
        Lines = Lines + 1
        GameSpeed = GameSpeed * .96
        ErasedLines = ErasedLines + 1
        FOR i = y TO 2 STEP -1
            FOR x = 2 TO 19
                Grid(x, i) = Grid(x, i - 1)
            NEXT x
        NEXT i
     END IF
 NEXT y

 IF ErasedLines THEN
    Score = Score + 300 * 2 ^ (ErasedLines - 1)
    Score = Score + 4 ^ ShapeDiff(ShapeNow) * ErasedLines
    PlaySnd 7
 ELSE
    PlaySnd 6
 END IF

END SUB

SUB FontPrint (XPos, YPos, Text$, Colour!, Rotate)

 FOR TextPos = 1 TO LEN(Text$)
     FOR x = 1 TO 5
         FOR y = 1 TO 6
             SELECT CASE FontChar(ASC(MID$(Text$, TextPos, 1)), x, y)
               CASE 0:
                 IF Rotate = 2 THEN
                    PSET (XPos + x + TextPos * 6 - 7, YPos + y - 1), 0
                 END IF
               CASE 1:
                 IF Rotate = 1 THEN
                    PSET (XPos + x - 1, YPos + y - 1 + TextPos * 7 - 7), Colour
                 ELSE
                    PSET (XPos + x + TextPos * 6 - 7, YPos + y - 1), Colour
                 END IF
             END SELECT
         NEXT y
     NEXT x
 NEXT TextPos

END SUB

SUB InitializeFont

 OPEN CurDir$ + "QBTETRIS.FNT" FOR RANDOM AS #1 LEN = LEN(Font)

 IF LOF(1) = 0 THEN
    PRINT "Fatal Error: Couldn't find file QBTETRIS.FNT"
    DO: LOOP UNTIL LEN(INKEY$)
    CLOSE #1
    KILL "QBTETRIS.FNT"
    END
 END IF

 PRINT "Loading Font...";

 FOR rec = 1 TO 127
     GET #1, rec, Font
     x = 0
     y = 1
     FOR CodePos = 1 TO 30
         IF x = 5 THEN x = 0: y = y + 1
         x = x + 1
         FontChar(rec, x, y) = VAL(MID$(Font, CodePos, 1))
     NEXT CodePos
 NEXT rec

 CLOSE #1

 PRINT "OK."

END SUB

SUB InitializeShapes

 OPEN CurDir$ + "QBTETRIS.SHP" FOR RANDOM AS #1 LEN = LEN(IniShape)

 IF LOF(1) = 0 THEN
    PRINT "Fatal Error: Couldn't find file QBTETRIS.SHP"
    DO: LOOP UNTIL LEN(INKEY$)
    CLOSE #1
    KILL "QBTETRIS.SHP"
    END
 END IF

 PRINT "Loading QbTetris Shapes...";

 FOR rec = 1 TO 44
     GET #1, rec, IniShape
     x = 0
     y = 1
     FOR i = 1 TO 25
         IF x = 5 THEN x = 0: y = y + 1
         x = x + 1
         Shape(rec, x, y) = VAL(MID$(IniShape.Code, i, 1))
     NEXT i
     ShapeDiff(rec) = IniShape.Difficulty
 NEXT rec

 CLOSE #1

 PRINT "OK."

END SUB

SUB Logo (XPos, YPos, Size, Text$, Colour, Frame)

 FOR TextPos = 1 TO LEN(Text$)
     FOR x = 1 TO 5
         FOR y = 1 TO 6
             SELECT CASE FontChar(ASC(MID$(Text$, TextPos, 1)), x, y)
               CASE 1: x1 = XPos + x * Size + TextPos * 6 * Size - 7 * Size
                       y1 = YPos + y * Size - 1 * Size
                       x2 = XPos + x * Size + TextPos * 6 * Size - 7 * Size + Size
                       y2 = YPos + y * Size - 1 * Size + Size
                       LINE (x1, y1)-(x2, y2), Colour, BF
                       LINE (x1, y1)-(x2, y2), Frame, B
             END SELECT
         NEXT y
     NEXT x
 NEXT TextPos


END SUB

SUB MainScreen

ReDrawMainScreen:
 CLS

 Logo 40, 20, 5, "QBTETRIS", 2, 8
 Logo -1, 0, 2, STRING$(27, "."), 4, 8
 Logo -1, 50, 2, STRING$(27, "."), 4, 8
 Logo -1, 150, 2, STRING$(27, "."), 4, 8
 Logo -1, 180, 2, STRING$(27, "."), 4, 8

 FontPrint 60, 172, "[F1] = Help  [F2] = Sound On/Off", 5, 0

 Selected = 1

 ERASE SelectedItem
       SelectedItem(1) = "[1] Play the Game"
       SelectedItem(2) = "[2] Description & Help"
       SelectedItem(3) = "[3] View HighScore"
       SelectedItem(4) = "[4] Quit"

MainPrompt:
 FOR i = 1 TO 4
     IF i = Selected THEN
        FontPrint 80, 58 + 20 * i, SelectedItem(i), 9, 0
     ELSE
        FontPrint 80, 58 + 20 * i, SelectedItem(i), 1, 0
     END IF
 NEXT i
 Key$ = ""
 DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

EvaluateKey:
 SELECT CASE Key$
       CASE CHR$(0) + "H":
            Selected = Selected - 1
            IF Selected = 0 THEN Selected = 1 ELSE PlaySnd 1
       CASE CHR$(0) + "P":
            Selected = Selected + 1
            IF Selected = 5 THEN Selected = 4 ELSE PlaySnd 1
       CASE CHR$(13): PlaySnd 2
            Key$ = RTRIM$(LTRIM$(STR$(Selected)))
            GOTO EvaluateKey
       CASE CHR$(27): ReallyQuit
       CASE "1": PlayTetris
       CASE "2": Description
       CASE "3": ViewHighScore
       CASE "4": ReallyQuit
       CASE ELSE: GOTO MainPrompt
 END SELECT

 IF GameOver THEN
    GameOver = 0
    GOTO ReDrawMainScreen
 END IF

 GOTO MainPrompt

END SUB

SUB PlaySnd (Effect)

 IF SoundFX = 0 THEN EXIT SUB

 SELECT CASE Effect
        CASE 1: SOUND 300, .1
        CASE 2: SOUND 400, .2: SOUND 300, .2
        CASE 3: FOR i = 2000 TO 200 STEP -200
                    SOUND i, .1
                NEXT i
        CASE 4: FOR i = 200 TO 2000 STEP 200
                    SOUND i, .1
                NEXT i
        CASE 5: SOUND 800, .05
        CASE 6: SOUND 400, .2: SOUND 800, .4
        CASE 7: FOR i = 200 TO 1700 STEP 250
                    SOUND i, .4
                NEXT i
        CASE 8: FOR i = 1700 TO 200 STEP -125
                    SOUND i, .4
                NEXT i
 END SELECT

END SUB

SUB PlayTetris

 Prompt

 CLS

 ClearGrid

 DrawGrid

 OPEN CurDir$ + "QBTETRIS.HSC" FOR RANDOM AS #2 LEN = LEN(HiScore)
      GET #2, 10 * GameDifficulty + 1, HiScore
      CurHiName = HiScore.PlayerName
      CurHiScore = HiScore.PlayerScore
 CLOSE #2

 FontPrint 13, 12, "QBasic Tetris Version 1.0", 1, 1
 FontPrint 12, 12, "QBasic Tetris Version 1.0", 4, 1
 FontPrint 195, 15, "Next Shape: no.", 1, 0
 FontPrint 250, 40, "Speed:", 1, 0
 FontPrint 250, 55, GameSpeed$, 3, 2
 FontPrint 195, 90, "Score:", 1, 0
 FontPrint 195, 110, "Lines:", 1, 0
 FontPrint 195, 140, "HighScore:", 1, 0
 FontPrint 255, 140, STR$(CurHiScore), 3, 0
 FontPrint 195, 155, LEFT$(CurHiName, 20), 1, 0

 Score = 0
 Lines = 0
 GameOver = 0

 ChooseShapes
 ChooseShapes

 ShowNextShape

 ShapePosX = 8
 ShapePosY = 1
 DrawShape ShapePosX, ShapePosY, 2
 
 DO
   FontPrint 285, 15, STR$(ShapeNext) + "  ", 1, 2
   FontPrint 240, 90, STR$(Score), 3, 2
   FontPrint 240, 110, STR$(Lines), 3, 2

   DelayTimer = TIMER
   DO
     SELECT CASE INKEY$
       CASE CHR$(0) + "H":
            PlaySnd 5
            TurnShape
       CASE CHR$(0) + "P":
            CountSteps = 0
            DO
               CheckGrid
               IF GameOver THEN EXIT SUB
               CountSteps = CountSteps + 1
            LOOP UNTIL Overlap
            Score = Score + 2 ^ (ShapeDiff(ShapeNow) - 1)
            Score = Score + CINT(CountSteps * ShapeDiff(ShapeNow) / 6) * 2
       CASE CHR$(0) + "K":
            PlaySnd 5
            CheckShapeL
       CASE CHR$(0) + "M":
            PlaySnd 5
            CheckShapeR
       CASE CHR$(27):
            ReallyQuit
       END SELECT
   LOOP UNTIL TIMER >= DelayTimer + GameSpeed

   CheckGrid

   IF GameOver THEN EXIT SUB

 LOOP

END SUB

SUB Prompt

 CLS
 Selected = 3

 FontPrint 20, 20, "Choose the structure of the shapes:", 4, 0

 ERASE SelectedItem
       SelectedItem(1) = "Extremely Simple Shapes"
       SelectedItem(2) = "Plain Structured Shapes"
       SelectedItem(3) = "Standard Tetris Shapes"
       SelectedItem(4) = "Shapes With Moderate Complexity"
       SelectedItem(5) = "Quite Troublesome Shapes"
       SelectedItem(6) = "Shapes With Extreme Intricacy"

ShapePrompt:
 FOR i = 1 TO 6
     IF i = Selected THEN
        FontPrint 20, 40 + 20 * i, SelectedItem(i), 9, 0
     ELSE
        FontPrint 20, 40 + 20 * i, SelectedItem(i), 1, 0
     END IF
 NEXT i
 Key$ = ""
 DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

 SELECT CASE Key$
       CASE CHR$(0) + "H":
            Selected = Selected - 1
            IF Selected = 0 THEN Selected = 1 ELSE PlaySnd 1
            GOTO ShapePrompt
       CASE CHR$(0) + "P":
            Selected = Selected + 1
            IF Selected = 7 THEN Selected = 6 ELSE PlaySnd 1
            GOTO ShapePrompt
       CASE CHR$(13): PlaySnd 2
            GameDifficulty = Selected
       CASE CHR$(27): ReallyQuit
                      GOTO ShapePrompt
 END SELECT


 CLS
 Selected = 2

 FontPrint 20, 20, "Please choose the speed as well:", 4, 0
 FontPrint 20, 35, "The faster the game, the higher the score!", 4, 0

 ERASE SelectedItem
       SelectedItem(1) = "Slow"
       SelectedItem(2) = "Normal"
       SelectedItem(3) = "Quick"
       SelectedItem(4) = "Fasted"

SpeedPrompt:
 FOR i = 1 TO 4
     IF i = Selected THEN
        FontPrint 20, 40 + 20 * i, SelectedItem(i), 9, 0
     ELSE
        FontPrint 20, 40 + 20 * i, SelectedItem(i), 1, 0
     END IF
 NEXT i
 Key$ = ""
 DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

 SELECT CASE Key$
       CASE CHR$(0) + "H":
            Selected = Selected - 1
            IF Selected = 0 THEN Selected = 1 ELSE PlaySnd 1
            GOTO SpeedPrompt
       CASE CHR$(0) + "P":
            Selected = Selected + 1
            IF Selected = 5 THEN Selected = 4 ELSE PlaySnd 1
            GOTO SpeedPrompt
       CASE CHR$(13): PlaySnd 2
            SELECT CASE Selected
                   CASE 1: GameSpeed = 1: GameSpeed$ = "Slow"
                           GameSpeedBonus = -.47
                   CASE 2: GameSpeed = .5: GameSpeed$ = "Normal"
                           GameSpeedBonus = .004
                   CASE 3: GameSpeed = .25: GameSpeed$ = "Quick"
                           GameSpeedBonus = .21
                   CASE 4: GameSpeed = 0: GameSpeed$ = "Fastest"
                           GameSpeedBonus = .29
            END SELECT
       CASE CHR$(27): ReallyQuit
                      GOTO SpeedPrompt
 END SELECT


 CLS
 Selected = 1

 FontPrint 20, 20, "Finally you have to enter the amount of randomly", 4, 0
 FontPrint 20, 35, "placed squares you want to have in the game.", 4, 0

 ERASE SelectedItem
       SelectedItem(1) = "None at all"
       SelectedItem(2) = "Just a few"
       SelectedItem(3) = "A Dozen"
       SelectedItem(4) = "Between 20 and 30"
       SelectedItem(5) = "Too many"

RandomPrompt:
 FOR i = 1 TO 5
     IF i = Selected THEN
        FontPrint 20, 40 + 20 * i, SelectedItem(i), 9, 0
     ELSE
        FontPrint 20, 40 + 20 * i, SelectedItem(i), 1, 0
     END IF
 NEXT i
 Key$ = ""
 DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

 SELECT CASE Key$
       CASE CHR$(0) + "H":
            Selected = Selected - 1
            IF Selected = 0 THEN Selected = 1 ELSE PlaySnd 1
            GOTO RandomPrompt
       CASE CHR$(0) + "P":
            Selected = Selected + 1
            IF Selected = 6 THEN Selected = 5 ELSE PlaySnd 1
            GOTO RandomPrompt
       CASE CHR$(13): PlaySnd 2
            SELECT CASE Selected
                   CASE 1: RandomShapes = 0
                   CASE 2: RandomShapes = INT(RND * 4) + 3
                   CASE 3: RandomShapes = 12
                   CASE 4: RandomShapes = INT(RND * 9) + 21
                   CASE 5: RandomShapes = 60
            END SELECT
       CASE CHR$(27): ReallyQuit
                      GOTO RandomPrompt
 END SELECT


END SUB

SUB ReallyQuit

 KEY(1) OFF: KEY(2) OFF
 PCOPY 0, 4

 LINE (98, 82)-(222, 112), 0, BF
 LINE (98, 82)-(222, 112), 1, B
 LINE (99, 83)-(221, 111), 9, B

 FontPrint 108, 90, "Do you really want", 6, 0
 FontPrint 108, 100, "to quit (Y/N)?", 6, 0

QuitPrompt:
 Key$ = ""
 DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

 SELECT CASE UCASE$(Key$)
        CASE "Y":
             LINE (100, 84)-(220, 110), 0, BF
             FontPrint 108, 95, "Press any key...", 2, 0
             DO
               x = INT(RND * 320)
               y = INT(RND * 200)
               Colour = POINT(x, y)
               IF Colour THEN LINE (x, y)-(x, y + 3), Colour
             LOOP UNTIL LEN(INKEY$)
             SYSTEM
        CASE "N": PCOPY 4, 0
                  KEY(1) ON: KEY(2) ON
        CASE ELSE: GOTO QuitPrompt
 END SELECT

END SUB

SUB ShowNextShape

 XPos = 22
 YPos = 3

 LINE (195, 25)-(240, 70), 8, B
 LINE (196, 26)-(239, 69), 7, B

 FOR y = 1 TO 5
     FOR x = 1 TO 5
         PosX = (22 + 8 * x) + (XPos * 8 - 8)
         PosY = (4 + 8 * y) + (YPos * 8 - 8)
         IF Shape(ShapeNext, x, y) THEN
            LINE (PosX, PosY)-(PosX + 7, PosY + 7), 2, BF
            LINE (PosX, PosY)-(PosX + 7, PosY + 7), 8, B
         ELSE
            LINE (PosX, PosY)-(PosX + 7, PosY + 7), 0, BF
         END IF
     NEXT x
 NEXT y

END SUB

SUB TitleScreen

 Logo 60, 20, 3, "Welcome to:", 1, 8
 Logo 16, 76, 6, "QBTETRIS", 2, 8
 Logo 77, 150, 2, "programmed by", 3, 8
 Logo 72, 170, 2, "Dominik Kaspar", 3, 8

 DIM Star(7) AS STRING * 30
     Star(1) = "rl2rud2"
     Star(2) = "r2l4r2u2d4"
     Star(3) = "r3l6r3u3d6u3hf2hge2"
     Star(4) = "r4l8r4u4d8u4hf2hge2"
     Star(5) = "r5l10r5u5d10u5h2f4h2g2e4"
     Star(6) = "r6l12r6u6d12u6h2f4h2g2e4"
     Star(7) = "r6l12r6u6d12u6h2f4h2g2e4"

 DO
   SELECT CASE INT(RND * 6) + 1
          CASE 1: x = 301: y = 73
          CASE 2: x = 15: y = 75
          CASE 3: x = 57: y = 17
          CASE 4: x = 74: y = 147
          CASE 5: x = 241: y = 167
          CASE 6: x = 252: y = 23
   END SELECT
   
   FOR i = 1 TO 7
       t = TIMER:
       DO
         Key$ = INKEY$
         IF Key$ = CHR$(27) THEN
            ReallyQuit
            Key$ = ""
         END IF
         IF LEN(Key$) THEN EXIT SUB
       LOOP UNTIL TIMER >= t + .2
       PSET (x, y)
       DRAW "c14" + Star(i)
   NEXT i
   PSET (x, y)
   DRAW "c0" + Star(7)
 LOOP UNTIL LEN(INKEY$)

END SUB

SUB TurnShape

 ERASE NewShape

 FOR x = 1 TO 5
     FOR y = 1 TO 5
         NewShape(x, y) = Shape(ShapeNow, 6 - y, x)
         IF NewShape(x, y) AND Grid(ShapePosX + x - 1, ShapePosY + y - 1) THEN
            EXIT SUB
         END IF
     NEXT y
 NEXT x

 DrawShape ShapePosX, ShapePosY, 0

 FOR y = 1 TO 5
     FOR x = 1 TO 5
         Shape(ShapeNow, x, y) = NewShape(x, y)
     NEXT x
 NEXT y

 DrawShape ShapePosX, ShapePosY, 2

END SUB

SUB ViewHighScore

PCOPY 0, 3
CLS

Logo 40, 20, 3, "HIGHSCORE", 2, 8

Selected = 1

ERASE SelectedItem
      SelectedItem(1) = "HighScore with all levels      "
      SelectedItem(2) = "Extremely Simple Shapes        "
      SelectedItem(3) = "Plain Structured Shapes        "
      SelectedItem(4) = "Standard Tetris Shapes         "
      SelectedItem(5) = "Shapes With Moderate Complexity"
      SelectedItem(6) = "Quite Troublesome Shapes       "
      SelectedItem(7) = "Shapes With Extreme Intricacy  "

Key$ = ""
GOTO KeyPressed

ViewHiScorePrompt:
Key$ = ""
DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)

KeyPressed:
SELECT CASE Key$
       CASE CHR$(0) + "P":
            Selected = Selected + 1
            IF Selected = 8 THEN
               Selected = 7
               GOTO ViewHiScorePrompt
            ELSE PlaySnd 1
            END IF
       CASE CHR$(0) + "H":
            Selected = Selected - 1
            IF Selected = 0 THEN
               Selected = 1
               GOTO ViewHiScorePrompt
            ELSE PlaySnd 1
            END IF
       CASE CHR$(13):
            Key$ = CHR$(0) + "P"
            GOTO KeyPressed
       CASE CHR$(27): GOTO ExitSubViewHiScore
       CASE "1" TO "7": PlaySnd 1
            Selected = VAL(Key$)
END SELECT

FontPrint 40, 50, SelectedItem(Selected), 6, 2
FontPrint 240, 50, "Page" + RTRIM$(STR$(Selected)) + "/7", 4, 2

OPEN CurDir$ + "QBTETRIS.HSC" FOR RANDOM AS #1 LEN = LEN(HiScore)
      YPos = 60
      FOR rec = Selected * 10 - 9 TO Selected * 10
          GET #1, rec, HiScore
          YPos = YPos + 12
          FontPrint 35, YPos, STR$(rec - (Selected - 1) * 10) + ".", 1, 2
          FontPrint 80, YPos, HiScore.PlayerName, 1, 2
          FontPrint 232, YPos, STR$(HiScore.PlayerScore) + SPACE$(10 - LEN(STR$(HiScore.PlayerScore))), 1, 2
      NEXT rec
CLOSE #1
GOTO ViewHiScorePrompt

ExitSubViewHiScore:

PCOPY 3, 0

ERASE SelectedItem
      SelectedItem(1) = "[1] Play the Game"
      SelectedItem(2) = "[2] Description & Help"
      SelectedItem(3) = "[3] View HighScore"
      SelectedItem(4) = "[4] Quit"

END SUB

