DECLARE SUB trans (flag!, c!)
DECLARE SUB keyboard (pacx!, pacy!, way!, newway!, key$)
DECLARE SUB checkmap (pacx!, pacy!, way!, ballx!(), bally!(), ballxold!(), ballyold!(), ballcount!(), ballway!(), points!, finish!, value!(), rotateindex!)
DECLARE SUB loadmap (map$(), marker!, letter!())
DECLARE SUB winner (letter!())
DECLARE SUB ending (letter!())
DECLARE SUB kbhit ()
DECLARE SUB printg (sentence$, x!, y!, scale!, c!, flagshade!, letter!())
DECLARE SUB drawpac (pacx!, pacy!, way!)
DECLARE SUB erasemonsters (ballx!(), bally!(), ballxold!(), ballyold!())
DECLARE SUB drawmonsters (ballx!(), bally!(), ballway!())
DECLARE SUB iniget (ballx!(), bally!())
DECLARE SUB opening (letter!(), value!())
DECLARE SUB movemonsters (ballx!(), bally!(), ballway!(), ballcount!())
DECLARE SUB iniball (ballx!(), bally!(), ballxold!(), ballyold!(), ballway!(), ballcount!())
DECLARE SUB rotatecolor (rotateindex!, value!())
DECLARE SUB erasepac (pacxold!, pacyold!)
DECLARE SUB movepac (pacx!, pacy!, way!)
DECLARE SUB makepalette (value!())
DECLARE SUB iniletter (letter!())
DECLARE SUB died (pacx!, pacy!, value!())
DECLARE SUB makecolors (value!())
DECLARE SUB lives ()
DECLARE SUB grabimage ()

REM John Hess
REM Game Project
REM UTIMATE PACGUY PLUS++

REM ***Variable Description***
REM value() = array that holds the colors
REM letter() = array that holds the fonts
REM map() = array that holds the objects in the map
REM ballx() = x coordinate in the tanks
REM bally() = y coordinate in the tanks
REM ballway() = direction of tanks
REM ballcount() = decision loop of tanks
REM ballxold() = holds old x coordinates for tanks
REM ballyold() = holds old y coordinates for tanks
REM pac1() = hold graphics for pacguy in direction 1
REM pac2() = hold graphics for pacguy in direction 2
REM pac3() = hold graphics for pacguy in direction 3
REM pac4() = hold graphics for pacguy in direction 4
REM tank1() = holds graphics for tanks in direction 1
REM tank2() = holds graphics for tanks in direction 2
REM tank3() = holds graphics for tanks in direction 3
REM tank4() = holds graphics for tanks in direction 4
REM background1() = holds the background under tank 1
REM background2() = holds the background under tank 2
REM background3() = holds the background under tank 3
REM background4() = holds the background under tank 4
REM wid = constant(don't ever change this) width of pacguy
REM backcolor = color of boundaries
REM markercolor = color of markers(dots)
REM way = direction of pacguy
REM pacx = x coordinate of pacguy
REM pacy = y coordinate of pacguy
REM pacxold = holds old x coordinate for pacguy
REM pacyold = holds old y coordinate for pacguy
REM finish = amount of lives
REM marker = amount of markers
REM lcv = loop control variable
REM points = amount of markers gotten already
REM rotateindex = index for rotate color sub
REM c = color
REM radius = radius of explosion
REM cirbackground() = background under explosion
REM **************************

REM ***Sub Description***
REM grabimage() = grabs images for the game
REM iniball() = sets the starting values for the tanks
REM makecolors() = makes the colors for the palette
REM makepalette() = makes the 60 shades of primary colors palette
REM iniletter() = gets the fonts
REM opening() = opening screen
REM loadmap() = loads the map
REM iniget() = gets the beginning backgrounds for tanks
REM lives() = puts the lives at the top of the screen
REM keyboard() = gets the keyboard strokes
REM winner() = winning screen
REM ending() = ending of game after 3 lives have been lost
REM kbhit() = waits for a keystroke
REM printg() = displays graphical letters
REM drawpac() = draws pacguy
REM erasemonsters() = erases the tanks
REM drawmonsters() = draws the tanks
REM checkmap() = checks for boundary colors
REM movemonsters() = moves the tanks
REM rotatecolor() rotates the colors
REM erasepac() = erases pacguy
REM died() = when the player dies
REM ***************************



REM COLOR number = 65536 * blue + 256 * green + red
REM equation used to create palette

main:

REM clear all variables

    CLEAR

REM screen mode 13
REM (320/200 with 256 colors)
   
    SCREEN 13

REM arrays

    DIM value(0 TO 300):         'colors

    DIM letter(0 TO 6080):       'fonts
   
    DIM map$(0 TO 50, 0 TO 50):   'map

    DIM ballx(0 TO 10):          'x coordinate of tanks
   
    DIM bally(0 TO 10):          'y coordinate of tanks
   
    DIM ballway(0 TO 10):        'way for tanks
   
    DIM ballcount(0 TO 10):      'decides when to check for a turn

    DIM ballxold(0 TO 10):       'holds old x coordinates
   
    DIM ballyold(0 TO 10):       'holds old y coordinates

REM pacguy graphics

    DIM SHARED pac1(0 TO 50)
    DIM SHARED pac2(0 TO 50)
    DIM SHARED pac3(0 TO 50)
    DIM SHARED pac4(0 TO 50)

REM tank graphics

    DIM SHARED tank1(0 TO 50)
    DIM SHARED tank2(0 TO 50)
    DIM SHARED tank3(0 TO 50)
    DIM SHARED tank4(0 TO 50)

REM array that hold the background of the tanks

    DIM SHARED background1(0 TO 50)
    DIM SHARED background2(0 TO 50)
    DIM SHARED background3(0 TO 50)
    DIM SHARED background4(0 TO 50)
    DIM SHARED background5(0 TO 50)


REM constants

    CONST wid = 5:             'width of pacguy(never change this)

    CONST backcolor = 150:     'only change this if you also change color of the block
   
    CONST markercolor = 240:   'color of the markers(dots)

REM ini variables

    way = 0:            'stops pacguy to begin with

    pacx = 165:         'starting point of pacx

    pacy = 125:         'starting point of pacy

    pacxold = pacx:     'puts variables into old ones

    pacyold = pacy
   
    finish = 2:         'gives you three lives

    points = 0:         'amount of points
   
REM grab all of the sprites
   
    CALL grabimage

REM ini the tanks

    CALL iniball(ballx(), bally(), ballxold(), ballyold(), ballway(), ballcount())

REM make the colors
   
    CALL makecolors(value())
           
REM put the colors into the registers

    CALL makepalette(value())

REM read in the fonts

    CALL iniletter(letter())

REM opening screen

    CALL opening(letter(), value())

REM load the map
   
    CLS

    RANDOMIZE (TIMER)

    CALL loadmap(map$(), marker, letter())

REM get the background under the tanks

    CALL iniget(ballx(), bally())

REM paste the lives at the top of the screen
   
    CALL lives

REM do until user wants to quit

    DO WHILE finish >= 0

REM grab keystrokes and store in newway if it is acceptable

        CALL keyboard(pacx, pacy, way, newway, key$)

REM move all of the tanks

        CALL movemonsters(ballx(), bally(), ballway(), ballcount())

REM move pacguy

        CALL movepac(pacx, pacy, way)

REM check map for boundaries

        CALL checkmap(pacx, pacy, way, ballx(), bally(), ballxold(), ballyold(), ballcount(), ballway(), points, finish, value(), rotateindex)

REM erase the tanks with old background

        CALL erasemonsters(ballx(), bally(), ballxold(), ballyold())

REM draw the tanks according to ballway

        CALL drawmonsters(ballx(), bally(), ballway())
   
REM erase pacguy

        CALL erasepac(pacxold, pacyold)

REM draw pacguy according to way

        CALL drawpac(pacx, pacy, way)

REM rotate colors (1-4 , 191-194)

        CALL rotatecolor(rotateindex, value())

REM store variables involved in animation into old variables
REM (produces flicker free or almost flicker free images)
REM (without using pages and only 16 colors)
   
        pacxold = pacx

        pacyold = pacy

        FOR lcv = 1 TO 5

            ballxold(lcv) = ballx(lcv)

            ballyold(lcv) = bally(lcv)

        NEXT lcv

REM check to see if they won

        IF points = marker THEN

            CALL winner(letter())

            GOTO main

        END IF

REM while running the game if the user hit q then it return to the main menu

        IF UCASE$(key$) = "Q" THEN

            CALL ending(letter())

            GOTO main

        END IF

    LOOP

REM display ending after 3 lives have been used

    CALL ending(letter())
                    
REM return to beginning of program

GOTO main

REM font data (and there is a lot of it)
REM (80 in each line)
REM (0's represent off pixels)
REM (1's represent on pixels)
REM (generated off of the system fonts)
REM * A good idea would be RLE(run length encoding) *

REM map data is beneath the font data

DATA 0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,1,1,1,1,1,1,1,0,0,1,1,0,1,1,0,0,1,1,1,1,1,1,1,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,0,0,0,0,0,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,1,0,0,1,1,1,0,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,0,1,1,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,1,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0
DATA 0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,0,1,1,0,1,1,1,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,0,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,1,0,1,1,0,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1
DATA 0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0
DATA 1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0
DATA 1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0
DATA 1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,0,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,1,0,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,1,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0
DATA 1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0




REM map data
REM (0 markers\dots)
REM (1 regular red block)
REM (2 walk thru block)
REM (3 flashing green block)
REM (4 rotating grey blocks)
REM (5 rotating green blocks)
REM (9 blank blocks)
REM anything else is sent to printg
REM and put on as a character in gfx mode

DATA 3,1,1,1,1,1,1,1,1,1,1,1,P,A,C,G,U,Y,1,1,1,1,1,1,1,1,1,1,1,3
DATA 5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,3,1,1,0,1,0,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,0,5
DATA 5,0,1,1,1,0,1,0,1,1,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1
DATA 1,0,0,0,1,0,0,0,0,0,1,0,0,3,s,e,c,r,e,t,0,1,1,1,0,1,0,1,0,5
DATA 5,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,0,0,0,1,2,0,0,0,1,0,0,1,0,1
DATA 1,0,1,0,1,1,0,0,1,0,1,1,1,3,0,1,0,1,0,1,3,0,1,0,1,1,0,1,0,5
DATA 5,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,1
DATA 1,0,1,1,1,0,1,1,1,1,1,1,0,3,3,0,4,0,4,0,4,0,1,0,1,1,0,1,0,5
DATA 5,0,1,1,0,0,2,2,2,2,0,0,0,3,3,0,1,0,1,0,1,0,1,0,2,0,0,1,0,1
DATA 1,0,0,1,1,0,1,1,1,1,1,1,0,3,3,0,1,1,2,2,1,0,0,0,1,1,0,1,0,5
DATA 5,0,1,1,1,0,1,1,1,1,1,1,0,3,3,9,3,1,2,1,1,1,1,1,1,0,0,1,0,1
DATA 1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,3,1,4,0,4,0,4,0,4,0,4,0,1,0,5
DATA 5,0,1,1,0,1,0,1,0,1,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,1,0,1
DATA 1,0,0,1,1,1,1,1,1,1,1,1,0,1,0,3,1,1,1,s,e,c,r,e,t,1,1,1,0,5
DATA 5,0,3,1,1,0,1,0,1,1,0,1,0,1,0,1,1,0,1,1,0,1,1,1,0,1,0,3,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5
DATA 3,1,1,1,1,1,1,1,1,1,1,1,R,E,T,U,R,N,S,1,1,1,1,1,1,1,1,1,1,3

SUB checkmap (pacx, pacy, way, ballx(), bally(), ballxold(), ballyold(), ballcount(), ballway(), points, finish, value(), rotateindex)
REM checks colors and map positions

    SELECT CASE way

        CASE 1

            SELECT CASE (POINT(pacx, pacy - wid))

                CASE markercolor

                    points = points + 1

                    LINE (pacx - 1, pacy - wid)-(pacx + 1, pacy - wid - 1), 0, BF

                    SOUND 200, .2

                CASE 150

            END SELECT


        CASE 2

            SELECT CASE (POINT(pacx + wid, pacy))
            
                CASE markercolor
                   
                    points = points + 1

                    LINE (pacx + wid, pacy - 1)-(pacx + wid + 1, pacy + 1), 0, BF

                    SOUND 200, .2
               
                CASE 150

            END SELECT

        CASE 3

            SELECT CASE (POINT(pacx, pacy + wid))
            
                CASE markercolor
               
                    points = points + 1

                    LINE (pacx - 1, pacy + wid - 1)-(pacx + 1, pacy + wid + 1), 0, BF

                    SOUND 200, .2
               
                CASE 150

            END SELECT

        CASE 4

            SELECT CASE (POINT(pacx - wid, pacy))
            
                CASE markercolor
                   
                    points = points + 1

                    LINE (pacx - wid, pacy - 1)-(pacx - wid - 1, pacy + 1), 0, BF

                    SOUND 200, .2
               
                CASE 150


            END SELECT

    END SELECT
    
    FOR lcv = 1 TO 5

        IF (ABS(ballx(lcv) - pacx) <= 9) AND (ABS(bally(lcv) - pacy) <= 9) THEN

            finish = finish - 1

            CALL died(pacx, pacy, value())

            SELECT CASE finish

                CASE 1

                    LINE (30, 0)-(40, 10), 0, BF

                CASE 0

                    LINE (15, 0)-(25, 10), 0, BF
            
            END SELECT

REM put pacguy back and make him stop

                pacx = 165
                pacy = 125
                way = 0
                rotateindex = 1

REM put all tanks back at their starting positions
REM or else you may end up dying a whole lot

            CALL erasemonsters(ballx(), bally(), ballxold(), ballyold())

            CALL iniball(ballx(), bally(), ballxold(), ballyold(), ballway(), ballcount())
       
            CALL iniget(ballx(), bally())

        END IF

    NEXT lcv

END SUB

SUB died (pacx, pacy, value())
REM when the player dies

DIM cirbackground(0 TO 200)

    PLAY "t255o0bb"

    c = 190
   
    GET (pacx - 10, pacy - 10)-(pacx + 10, pacy + 10), cirbackground

    FOR radius = 1 TO 5

        c = c + 1

        CIRCLE (pacx, pacy), radius, c

    NEXT radius

    FOR lcv = 1 TO 100

       CALL rotatecolor(rotateindex, value())

    NEXT lcv

    PUT (pacx - 10, pacy - 10), cirbackground, PSET

    LINE (pacxold - 4, pacyold - 4)-(pacxold + 4, pacyold + 4), 0, BF

END SUB

SUB drawmonsters (ballx(), bally(), ballway())


    GET (ballx(1) - 4, bally(1) - 4)-(ballx(1) + 4, bally(1) + 4), background1

    GET (ballx(2) - 4, bally(2) - 4)-(ballx(2) + 4, bally(2) + 4), background2

    GET (ballx(3) - 4, bally(3) - 4)-(ballx(3) + 4, bally(3) + 4), background3

    GET (ballx(4) - 4, bally(4) - 4)-(ballx(4) + 4, bally(4) + 4), background4

    GET (ballx(5) - 4, bally(5) - 4)-(ballx(5) + 4, bally(5) + 4), background5

FOR lcv = 1 TO 5

    SELECT CASE ballway(lcv)
        
        CASE 1

            PUT (ballx(lcv) - 4, bally(lcv) - 4), tank1, PSET

        CASE 2

            PUT (ballx(lcv) - 4, bally(lcv) - 4), tank2, PSET

        CASE 3

            PUT (ballx(lcv) - 4, bally(lcv) - 4), tank3, PSET

        CASE 4

            PUT (ballx(lcv) - 4, bally(lcv) - 4), tank4, PSET

    CASE ELSE

        CIRCLE (ballx(lcv), bally(lcv)), 4, 50

    END SELECT

NEXT lcv

END SUB

SUB drawpac (pacx, pacy, way)
REM draws pacguy

    SELECT CASE way

        CASE 1, 0

            PUT (pacx - 4, pacy - 4), pac1, PSET

        CASE 2

            PUT (pacx - 4, pacy - 4), pac2, PSET

        CASE 3

            PUT (pacx - 4, pacy - 4), pac3, PSET

        CASE 4

            PUT (pacx - 4, pacy - 4), pac4, PSET

    END SELECT

END SUB

SUB ending (letter())
REM ending

    CLS

    c = 20

    FOR y = 0 TO 199 STEP 5

        FOR x = 0 TO 319 STEP 5

            c = c + 3

            IF c > 50 THEN c = 20

            LINE (x, y)-(x + 4, y + 4), c, B

            LINE (x + 1, y + 1)-(x + 3, y + 3), c + 5, B

            PSET (x + 2, y + 2), c + 10

        NEXT x

    NEXT y

    CALL printg("Your", 160, 50, 3, 145, 1, letter())
   
    CALL printg("A", 160, 100, 3, 145, 1, letter())

    CALL printg("LOSER!", 163, 150, 5, 145, 1, letter())

REM grab it twice incase of accidental hitting of the keys

    i$ = INKEY$
       
    i$ = INKEY$

    DO WHILE i$ = ""
      
        i$ = INKEY$

    LOOP

    CLS


END SUB

SUB erasemonsters (ballx(), bally(), ballxold(), ballyold())

    PUT (ballxold(1) - 4, ballyold(1) - 4), background1, PSET

    PUT (ballxold(2) - 4, ballyold(2) - 4), background2, PSET

    PUT (ballxold(3) - 4, ballyold(3) - 4), background3, PSET

    PUT (ballxold(4) - 4, ballyold(4) - 4), background4, PSET

    PUT (ballxold(5) - 4, ballyold(5) - 4), background5, PSET

END SUB

SUB erasepac (pacxold, pacyold)
REM erases the pacguy
  
    LINE (pacxold - 4, pacyold - 4)-(pacxold + 4, pacyold + 4), 0, BF

END SUB

SUB grabimage
  
REM set the image to the upper left
   
    pacx = 4
    pacy = 4
    ballx = 4
    bally = 4

REM roller
      
            LINE (pacx - 2, pacy - 4)-(pacx + 2, pacy - 4), 191
            LINE (pacx - 2, pacy - 3)-(pacx + 2, pacy - 3), 192
            LINE (pacx - 2, pacy - 2)-(pacx + 2, pacy - 2), 194

REM hooks

            PSET (pacx - 3, pacy - 3), 50
            PSET (pacx + 3, pacy - 3), 50
            LINE (pacx - 4, pacy - 3)-(pacx - 4, pacy), 40
            LINE (pacx + 4, pacy - 3)-(pacx + 4, pacy), 40

REM body

            LINE (pacx - 4, pacy)-(pacx + 4, pacy), 50
            LINE (pacx - 4, pacy + 4)-(pacx + 4, pacy + 4), 50

            LINE (pacx - 2, pacy + 1)-(pacx + 2, pacy + 3), 60, B

REM wheels

            PSET (pacx - 3, pacy + 1), 191
            PSET (pacx - 3, pacy + 2), 192
            PSET (pacx - 3, pacy + 3), 194
         
            PSET (pacx - 4, pacy + 1), 191
            PSET (pacx - 4, pacy + 2), 192
            PSET (pacx - 4, pacy + 3), 194


            PSET (pacx + 3, pacy + 1), 191
            PSET (pacx + 3, pacy + 2), 192
            PSET (pacx + 3, pacy + 3), 194
                      
            PSET (pacx + 4, pacy + 1), 191
            PSET (pacx + 4, pacy + 2), 192
            PSET (pacx + 4, pacy + 3), 194

    GET (0, 0)-(8, 8), pac1

    CLS

REM roller

            LINE (pacx + 4, pacy - 2)-(pacx + 4, pacy + 2), 191
            LINE (pacx + 3, pacy - 2)-(pacx + 3, pacy + 2), 192
            LINE (pacx + 2, pacy - 2)-(pacx + 2, pacy + 2), 194

REM hooks

            PSET (pacx + 3, pacy - 3), 50
            PSET (pacx + 3, pacy + 3), 50
            LINE (pacx + 3, pacy - 4)-(pacx, pacy - 4), 40
            LINE (pacx + 3, pacy + 4)-(pacx, pacy + 4), 40


REM body

            LINE (pacx, pacy - 4)-(pacx, pacy + 4), 50
            LINE (pacx - 4, pacy - 4)-(pacx - 4, pacy + 4), 50

            LINE (pacx - 1, pacy - 2)-(pacx - 3, pacy + 2), 60, B

REM wheels

            PSET (pacx - 1, pacy - 3), 191
            PSET (pacx - 2, pacy - 3), 192
            PSET (pacx - 3, pacy - 3), 194
        
            PSET (pacx - 1, pacy - 4), 191
            PSET (pacx - 2, pacy - 4), 192
            PSET (pacx - 3, pacy - 4), 194


            PSET (pacx - 1, pacy + 3), 191
            PSET (pacx - 2, pacy + 3), 192
            PSET (pacx - 3, pacy + 3), 194
                     
            PSET (pacx - 1, pacy + 4), 191
            PSET (pacx - 2, pacy + 4), 192
            PSET (pacx - 3, pacy + 4), 194

    GET (0, 0)-(8, 8), pac2

    CLS
   

REM roller
     
            LINE (pacx - 2, pacy + 4)-(pacx + 2, pacy + 4), 191
            LINE (pacx - 2, pacy + 3)-(pacx + 2, pacy + 3), 192
            LINE (pacx - 2, pacy + 2)-(pacx + 2, pacy + 2), 194

REM hooks

            PSET (pacx - 3, pacy + 3), 50
            PSET (pacx + 3, pacy + 3), 50
            LINE (pacx - 4, pacy + 3)-(pacx - 4, pacy), 40
            LINE (pacx + 4, pacy + 3)-(pacx + 4, pacy), 40

REM body

            LINE (pacx - 4, pacy)-(pacx + 4, pacy), 50
            LINE (pacx - 4, pacy - 4)-(pacx + 4, pacy - 4), 50

            LINE (pacx - 2, pacy - 1)-(pacx + 2, pacy - 3), 60, B

REM wheels

            PSET (pacx - 3, pacy - 1), 191
            PSET (pacx - 3, pacy - 2), 192
            PSET (pacx - 3, pacy - 3), 194
          
            PSET (pacx - 4, pacy - 1), 191
            PSET (pacx - 4, pacy - 2), 192
            PSET (pacx - 4, pacy - 3), 194


            PSET (pacx + 3, pacy - 1), 191
            PSET (pacx + 3, pacy - 2), 192
            PSET (pacx + 3, pacy - 3), 194
                       
            PSET (pacx + 4, pacy - 1), 191
            PSET (pacx + 4, pacy - 2), 192
            PSET (pacx + 4, pacy - 3), 194

    GET (0, 0)-(8, 8), pac3

    CLS
   

REM roller
    
            LINE (pacx - 4, pacy - 2)-(pacx - 4, pacy + 2), 191
            LINE (pacx - 3, pacy - 2)-(pacx - 3, pacy + 2), 192
            LINE (pacx - 2, pacy - 2)-(pacx - 2, pacy + 2), 194

REM hooks

            PSET (pacx - 3, pacy - 3), 50
            PSET (pacx - 3, pacy + 3), 50
            LINE (pacx - 3, pacy - 4)-(pacx, pacy - 4), 40
            LINE (pacx - 3, pacy + 4)-(pacx, pacy + 4), 40

REM body

            LINE (pacx, pacy - 4)-(pacx, pacy + 4), 50
            LINE (pacx + 4, pacy - 4)-(pacx + 4, pacy + 4), 50

            LINE (pacx + 1, pacy - 2)-(pacx + 3, pacy + 2), 60, B

REM wheels

            PSET (pacx + 1, pacy - 3), 191
            PSET (pacx + 2, pacy - 3), 192
            PSET (pacx + 3, pacy - 3), 194
       
            PSET (pacx + 1, pacy - 4), 191
            PSET (pacx + 2, pacy - 4), 192
            PSET (pacx + 3, pacy - 4), 194


            PSET (pacx + 1, pacy + 3), 191
            PSET (pacx + 2, pacy + 3), 192
            PSET (pacx + 3, pacy + 3), 194
                    
            PSET (pacx + 1, pacy + 4), 191
            PSET (pacx + 2, pacy + 4), 192
            PSET (pacx + 3, pacy + 4), 194

    GET (0, 0)-(8, 8), pac4

    CLS

            PSET (ballx - 4, bally - 4), 1:  'upper left track
            PSET (ballx - 3, bally - 4), 1
            PSET (ballx - 4, bally - 3), 50
            PSET (ballx - 3, bally - 3), 50

            PSET (ballx - 4, bally - 2), 4:  'upper left track
            PSET (ballx - 3, bally - 2), 4
            PSET (ballx - 4, bally - 1), 50
            PSET (ballx - 3, bally - 1), 50

            PSET (ballx - 4, bally - 0), 1:  'upper left track
            PSET (ballx - 3, bally - 0), 1
            PSET (ballx - 4, bally + 1), 50
            PSET (ballx - 3, bally + 1), 50

            PSET (ballx - 4, bally + 2), 4:  'upper left track
            PSET (ballx - 3, bally + 2), 4
            PSET (ballx - 4, bally + 3), 50
            PSET (ballx - 3, bally + 3), 50

            PSET (ballx - 4, bally + 4), 1:  'upper left track
            PSET (ballx - 3, bally + 4), 1

REM right side

            PSET (ballx + 4, bally - 4), 1:  'upper left track
            PSET (ballx + 3, bally - 4), 1
            PSET (ballx + 4, bally - 3), 50
            PSET (ballx + 3, bally - 3), 50

            PSET (ballx + 4, bally - 2), 4:  'upper left track
            PSET (ballx + 3, bally - 2), 4
            PSET (ballx + 4, bally - 1), 50
            PSET (ballx + 3, bally - 1), 50

            PSET (ballx + 4, bally - 0), 1:  'upper left track
            PSET (ballx + 3, bally - 0), 1
            PSET (ballx + 4, bally + 1), 50
            PSET (ballx + 3, bally + 1), 50

            PSET (ballx + 4, bally + 2), 4:  'upper left track
            PSET (ballx + 3, bally + 2), 4
            PSET (ballx + 4, bally + 3), 50
            PSET (ballx + 3, bally + 3), 50

            PSET (ballx + 4, bally + 4), 1:  'upper left track
            PSET (ballx + 3, bally + 4), 1

REM cannon

            LINE (ballx - 2, bally + 4)-(ballx - 2, bally - 2), 200
            LINE (ballx - 1, bally + 4)-(ballx - 1, bally - 2), 215
            LINE (ballx, bally + 3)-(ballx, bally - 2), 230
            LINE (ballx + 1, bally + 4)-(ballx + 1, bally - 2), 215
            LINE (ballx + 2, bally + 4)-(ballx + 2, bally - 2), 200


    GET (0, 0)-(8, 8), tank1

    CLS
   

            PSET (ballx - 4, bally - 4), 1
            PSET (ballx - 4, bally - 3), 1
            PSET (ballx - 3, bally - 4), 50
            PSET (ballx - 3, bally - 3), 50
 
            PSET (ballx - 2, bally - 4), 4
            PSET (ballx - 2, bally - 3), 4
            PSET (ballx - 1, bally - 4), 50
            PSET (ballx - 1, bally - 3), 50
            
            PSET (ballx - 0, bally - 4), 1
            PSET (ballx - 0, bally - 3), 1
            PSET (ballx + 1, bally - 4), 50
            PSET (ballx + 1, bally - 3), 50

            PSET (ballx + 2, bally - 4), 4
            PSET (ballx + 2, bally - 3), 4
            PSET (ballx + 3, bally - 4), 50
            PSET (ballx + 3, bally - 3), 50

            PSET (ballx + 4, bally - 4), 1
            PSET (ballx + 4, bally - 3), 1
                            
REM bottom side

            PSET (ballx - 4, bally + 4), 1
            PSET (ballx - 4, bally + 3), 1
            PSET (ballx - 3, bally + 4), 50
            PSET (ballx - 3, bally + 3), 50

            PSET (ballx - 2, bally + 4), 4
            PSET (ballx - 2, bally + 3), 4
            PSET (ballx - 1, bally + 4), 50
            PSET (ballx - 1, bally + 3), 50
           
            PSET (ballx - 0, bally + 4), 1
            PSET (ballx - 0, bally + 3), 1
            PSET (ballx + 1, bally + 4), 50
            PSET (ballx + 1, bally + 3), 50

            PSET (ballx + 2, bally + 4), 4
            PSET (ballx + 2, bally + 3), 4
            PSET (ballx + 3, bally + 4), 50
            PSET (ballx + 3, bally + 3), 50

            PSET (ballx + 4, bally + 4), 1
            PSET (ballx + 4, bally + 3), 1

REM cannon
          
            LINE (ballx - 4, bally - 2)-(ballx + 2, bally - 2), 200
            LINE (ballx - 4, bally - 1)-(ballx + 2, bally - 1), 215
            LINE (ballx - 3, bally)-(ballx + 2, bally), 230
            LINE (ballx - 4, bally + 1)-(ballx + 2, bally + 1), 215
            LINE (ballx - 4, bally + 2)-(ballx + 2, bally + 2), 200

           


    GET (0, 0)-(8, 8), tank2

    CLS
   

            PSET (ballx - 4, bally - 4), 1:  'upper left track
            PSET (ballx - 3, bally - 4), 1
            PSET (ballx - 4, bally - 3), 50
            PSET (ballx - 3, bally - 3), 50

            PSET (ballx - 4, bally - 2), 4:  'upper left track
            PSET (ballx - 3, bally - 2), 4
            PSET (ballx - 4, bally - 1), 50
            PSET (ballx - 3, bally - 1), 50

            PSET (ballx - 4, bally - 0), 1:  'upper left track
            PSET (ballx - 3, bally - 0), 1
            PSET (ballx - 4, bally + 1), 50
            PSET (ballx - 3, bally + 1), 50

            PSET (ballx - 4, bally + 2), 4:  'upper left track
            PSET (ballx - 3, bally + 2), 4
            PSET (ballx - 4, bally + 3), 50
            PSET (ballx - 3, bally + 3), 50

            PSET (ballx - 4, bally + 4), 1:  'upper left track
            PSET (ballx - 3, bally + 4), 1

REM right side

            PSET (ballx + 4, bally - 4), 1:  'upper left track
            PSET (ballx + 3, bally - 4), 1
            PSET (ballx + 4, bally - 3), 50
            PSET (ballx + 3, bally - 3), 50

            PSET (ballx + 4, bally - 2), 4:  'upper left track
            PSET (ballx + 3, bally - 2), 4
            PSET (ballx + 4, bally - 1), 50
            PSET (ballx + 3, bally - 1), 50

            PSET (ballx + 4, bally - 0), 1:  'upper left track
            PSET (ballx + 3, bally - 0), 1
            PSET (ballx + 4, bally + 1), 50
            PSET (ballx + 3, bally + 1), 50

            PSET (ballx + 4, bally + 2), 4:  'upper left track
            PSET (ballx + 3, bally + 2), 4
            PSET (ballx + 4, bally + 3), 50
            PSET (ballx + 3, bally + 3), 50

            PSET (ballx + 4, bally + 4), 1:  'upper left track
            PSET (ballx + 3, bally + 4), 1

REM cannon

            LINE (ballx - 2, bally - 4)-(ballx - 2, bally + 2), 200
            LINE (ballx - 1, bally - 4)-(ballx - 1, bally + 2), 215
            LINE (ballx, bally - 3)-(ballx, bally + 2), 230
            LINE (ballx + 1, bally - 4)-(ballx + 1, bally + 2), 215
            LINE (ballx + 2, bally - 4)-(ballx + 2, bally + 2), 200

  
    GET (0, 0)-(8, 8), tank3

    CLS
   

            PSET (ballx - 4, bally - 4), 1
            PSET (ballx - 4, bally - 3), 1
          
            PSET (ballx - 3, bally - 4), 50
            PSET (ballx - 3, bally - 3), 50
  
            PSET (ballx - 2, bally - 4), 4
            PSET (ballx - 2, bally - 3), 4
            PSET (ballx - 1, bally - 4), 50
            PSET (ballx - 1, bally - 3), 50
             
            PSET (ballx - 0, bally - 4), 1
            PSET (ballx - 0, bally - 3), 1
            PSET (ballx + 1, bally - 4), 50
            PSET (ballx + 1, bally - 3), 50

            PSET (ballx + 2, bally - 4), 4
            PSET (ballx + 2, bally - 3), 4
            PSET (ballx + 3, bally - 4), 50
            PSET (ballx + 3, bally - 3), 50

            PSET (ballx + 4, bally - 4), 1
            PSET (ballx + 4, bally - 3), 1
                             
REM bottom side

            PSET (ballx - 4, bally + 4), 1
            PSET (ballx - 4, bally + 3), 1
            PSET (ballx - 3, bally + 4), 50
            PSET (ballx - 3, bally + 3), 50
 
            PSET (ballx - 2, bally + 4), 4
            PSET (ballx - 2, bally + 3), 4
            PSET (ballx - 1, bally + 4), 50
            PSET (ballx - 1, bally + 3), 50
            
            PSET (ballx - 0, bally + 4), 1
            PSET (ballx - 0, bally + 3), 1
            PSET (ballx + 1, bally + 4), 50
            PSET (ballx + 1, bally + 3), 50

            PSET (ballx + 2, bally + 4), 4
            PSET (ballx + 2, bally + 3), 4
            PSET (ballx + 3, bally + 4), 50
            PSET (ballx + 3, bally + 3), 50

            PSET (ballx + 4, bally + 4), 1
            PSET (ballx + 4, bally + 3), 1

REM cannon
         
            LINE (ballx + 4, bally - 2)-(ballx - 2, bally - 2), 200
            LINE (ballx + 4, bally - 1)-(ballx - 2, bally - 1), 215
            LINE (ballx + 3, bally)-(ballx - 2, bally), 230
            LINE (ballx + 4, bally + 1)-(ballx - 2, bally + 1), 215
            LINE (ballx + 4, bally + 2)-(ballx - 2, bally + 2), 200


    GET (0, 0)-(8, 8), tank4

    CLS


END SUB

SUB iniball (ballx(), bally(), ballxold(), ballyold(), ballway(), ballcount())
   
    FOR lcv = 1 TO 5

        ballway(lcv) = INT(RND * 4) + 1
       
        ballcount(lcv) = INT(RND * 10) + 5

    NEXT lcv

    ballx(1) = 25
    bally(1) = 25

    ballx(2) = 65
    bally(2) = 105

    ballx(3) = 115
    bally(3) = 105

    ballx(4) = 205
    bally(4) = 105

    ballx(5) = 245
    bally(5) = 95

    FOR lcv = 1 TO 5

        ballxold(lcv) = ballx(lcv)

        ballyold(lcv) = bally(lcv)

    NEXT lcv

END SUB

SUB iniget (ballx(), bally())
REM ini the gets for the background
   
    GET (ballx(1) - 4, bally(1) - 4)-(ballx(1) + 4, bally(1) + 4), background1

    GET (ballx(2) - 4, bally(2) - 4)-(ballx(2) + 4, bally(2) + 4), background2

    GET (ballx(3) - 4, bally(3) - 4)-(ballx(3) + 4, bally(3) + 4), background3

    GET (ballx(4) - 4, bally(4) - 4)-(ballx(4) + 4, bally(4) + 4), background4

    GET (ballx(5) - 4, bally(5) - 4)-(ballx(5) + 4, bally(5) + 4), background5

END SUB

SUB iniletter (letter())
REM ini letters

REM tell the user its loading the fonts

    COLOR 250
   
    LOCATE 2, 1
   
    PRINT "Loading Fonts"

    FOR index = 1 TO 6080  '6016
   
        READ letter(index)
   
    NEXT index

END SUB

SUB kbhit
REM waits for input

    DO WHILE INKEY$ = "": LOOP

    CLS

END SUB

SUB keyboard (pacx, pacy, way, newway, key$)
REM gets the keyboard strokes

REM get key and store it in newway
   
        key$ = INKEY$

        SELECT CASE key$

            CASE "4"

                newway = 4

            CASE "6"

                newway = 2

            CASE "8"

                newway = 1

            CASE "2"

                newway = 3

            CASE ELSE

                newway = newway

        END SELECT

REM wait for it to be a possible turn, then update way with newway

        SELECT CASE newway

        CASE 1

            IF POINT(pacx, pacy - wid - 1) <> backcolor AND POINT(pacx - wid + 1, pacy - wid - 1) <> backcolor AND POINT(pacx + wid - 1, pacy - wid - 1) <> backcolor THEN

                IF pacx / 5 = INT(pacx / 5) AND pacy / 5 = INT(pacy / 5) AND newway <> 0 THEN

                    way = newway

                END IF
               
          
            END IF

        CASE 2

            IF POINT(pacx + wid, pacy) <> backcolor AND POINT(pacx + wid, pacy - wid + 1) <> backcolor AND POINT(pacx + wid, pacy + wid - 1) <> backcolor THEN

                IF pacx / 5 = INT(pacx / 5) AND pacy / 5 = INT(pacy / 5) AND newway <> 0 THEN

                    way = newway

                END IF
               
              

            END IF

        CASE 3

            IF POINT(pacx, pacy + wid) <> backcolor AND POINT(pacx - wid + 1, pacy + wid) <> backcolor AND POINT(pacx + wid - 1, pacy + wid) <> backcolor THEN

                IF pacx / 5 = INT(pacx / 5) AND pacy / 5 = INT(pacy / 5) AND newway <> 0 THEN

                    way = newway

                END IF
               


            END IF

        CASE 4

            IF POINT(pacx - wid - 1, pacy) <> backcolor AND POINT(pacx - wid - 1, pacy - wid + 1) <> backcolor AND POINT(pacx - wid - 1, pacy + wid - 1) <> backcolor THEN

                IF pacx / 5 = INT(pacx / 5) AND pacy / 5 = INT(pacy / 5) AND newway <> 0 THEN

                    way = newway

                END IF
               


            END IF


    END SELECT

END SUB

SUB lives
REM places lives at the top

    PUT (15, 0), pac1, PSET

    PUT (30, 0), pac1, PSET

END SUB

SUB loadmap (map$(), marker, letter())
REM loads the map

REM marker is the amount of markers(flashing dots) in the games

    marker = 0

    FOR y = 1 TO 18
       
        FOR x = 1 TO 30

            READ map$(x, y)

            grafx = x * 10

            grafy = y * 10
       
            SELECT CASE map$(x, y)

                CASE "0"

REM markers
                   
                    marker = marker + 1

                    PSET (grafx + 4, grafy + 4), markercolor

                    PSET (grafx + 5, grafy + 5), markercolor

                    PSET (grafx + 4, grafy + 5), 1

                    PSET (grafx + 5, grafy + 4), 1

                CASE "1"

REM red box with green center

                    LINE (grafx, grafy)-(grafx + 9, grafy + 9), 150, B
                   
                    LINE (grafx + 1, grafy + 1)-(grafx + 8, grafy + 8), 160, B
                   
                    LINE (grafx + 2, grafy + 2)-(grafx + 7, grafy + 7), 170, B
                   
                    LINE (grafx + 3, grafy + 3)-(grafx + 6, grafy + 6), 180, B
                   
                    PAINT (grafx + 4, grafy + 4), 110, 170
               
                CASE "2"

REM fake red box
                    LINE (grafx, grafy)-(grafx + 9, grafy + 9), 140, B
                  
                    LINE (grafx + 1, grafy + 1)-(grafx + 8, grafy + 8), 145, B
                  
                    LINE (grafx + 2, grafy + 2)-(grafx + 7, grafy + 7), 155, B
                  
                    LINE (grafx + 3, grafy + 3)-(grafx + 6, grafy + 6), 160, B
                  
                    PAINT (grafx + 4, grafy + 4), 100, 160
               
                CASE "3"

REM flashing green block

                    LINE (grafx, grafy)-(grafx + 9, grafy + 9), 150, B

                    PAINT (grafx + 1, grafy + 1), INT(RND * 4) + 1, 150
                   
                CASE "4"

REM gray scaled square
                    
                    LINE (grafx, grafy)-(grafx + 9, grafy + 9), 150, B
                  
                    LINE (grafx + 1, grafy + 1)-(grafx + 8, grafy + 8), 191, B
                  
                    LINE (grafx + 2, grafy + 2)-(grafx + 7, grafy + 7), 192, B
                  
                    LINE (grafx + 3, grafy + 3)-(grafx + 6, grafy + 6), 193, B

                    LINE (grafx + 4, grafy + 4)-(grafx + 5, grafy + 5), 194, B

                CASE "5"

REM green scaled square(up and down)
               
                    LINE (grafx, grafy)-(grafx, grafy + 9), 150
                    LINE (grafx + 9, grafy)-(grafx + 9, grafy + 9), 150
                    LINE (grafx + 1, grafy)-(grafx + 8, grafy), 1
                    LINE (grafx + 1, grafy + 1)-(grafx + 8, grafy + 1), 1
                    LINE (grafx + 1, grafy + 2)-(grafx + 8, grafy + 2), 1
                    LINE (grafx + 1, grafy + 3)-(grafx + 8, grafy + 3), 2
                    LINE (grafx + 1, grafy + 4)-(grafx + 8, grafy + 4), 2
                    LINE (grafx + 1, grafy + 5)-(grafx + 8, grafy + 5), 3
                    LINE (grafx + 1, grafy + 6)-(grafx + 8, grafy + 6), 3
                    LINE (grafx + 1, grafy + 7)-(grafx + 8, grafy + 7), 4
                    LINE (grafx + 1, grafy + 8)-(grafx + 8, grafy + 8), 4
                    LINE (grafx + 1, grafy + 9)-(grafx + 8, grafy + 9), 4
           
              CASE "9"

REM blank one

                    LINE (grafx, grafy)-(grafx + 9, grafy + 9), 0, BF

              CASE ELSE

REM text in the map
               
                LINE (grafx, grafy)-(grafx + 9, grafy + 9), 150, B
                  
                LINE (grafx + 1, grafy + 1)-(grafx + 8, grafy + 8), 160, B
                  
                LINE (grafx + 2, grafy + 2)-(grafx + 7, grafy + 7), 170, B
                 
                LINE (grafx + 3, grafy + 3)-(grafx + 6, grafy + 6), 180, B

                PAINT (grafx + 4, grafy + 4), 110, 170

                CALL printg(map$(x, y), grafx + 5, grafy + 5, 1, 25, 1, letter())

           
            END SELECT

        NEXT x

    NEXT y


END SUB

SUB makecolors (value())
REM makes color values
   
REM tell the user its making color

COLOR 250

LOCATE 1, 1

PRINT "Loading Palette"

    index = 0
    blue = 0
    green = 0
    red = 0

REM rotating green colors(1 thru 5)

    FOR green = 35 TO 60 STEP 5
       
        index = index + 1

        value(index) = 65536 + 256 * green

    NEXT green

    green = 0

REM blue colors(6 thru 63)

    FOR blue = 6 TO 63
       
        index = index + 1
       
        value(index) = 65536 * blue + 256 * green + red
   
    NEXT blue

    blue = 0
    green = 0
    red = 0

REM green colors(64 thru 127)
  
    FOR green = 1 TO 63
       
        index = index + 1
       
        value(index) = 65536 * blue + 256 * green + red
   
    NEXT green

    blue = 0
    green = 0
    red = 0

REM red colors(128 thru 190)

    FOR red = 1 TO 63
       
        index = index + 1
       
        value(index) = 65536 * blue + 256 * green + red
   
    NEXT red

REM make 191 darker for the rotating wheel effect

    index = index + 1

    value(index) = 65536 * 20 + 256 * 20 + 20
   
REM rotating grey colors(191 thru 194)

    FOR black = 40 TO 60 STEP 5

        index = index + 1

        value(index) = 65536 * black + 256 * black + black

    NEXT black

    black = 0


REM grey colors(195 thru 255)

    FOR black = 6 TO 63
       
        index = index + 1
       
        value(index) = 65536 * black + 256 * black + black
   
    NEXT black

END SUB

SUB makepalette (value())
REM makes palette

    FOR index = 1 TO 256
       
        PALETTE (index), (value(index))

    NEXT index

END SUB

SUB movemonsters (ballx(), bally(), ballway(), ballcount())
REM moves the bouncing ball

    FOR lcv = 1 TO 5

        IF ballcount(lcv) <> 99 THEN ballcount(lcv) = ballcount(lcv) + 1

        IF ballcount(lcv) = 20 THEN

            ballway(lcv) = INT(RND * 4) + 1

            ballcount(lcv) = 0

        END IF


        SELECT CASE ballway(lcv)

            CASE 1

                IF POINT(ballx(lcv), bally(lcv) - 5) <> backcolor AND POINT(ballx(lcv) - 4, bally(lcv) - 5) <> backcolor AND POINT(ballx(lcv) + 4, bally(lcv) - 5) <> backcolor THEN

                    bally(lcv) = bally(lcv) - 1

                END IF

               
            CASE 2

                IF POINT(ballx(lcv) + 5, bally(lcv)) <> backcolor AND POINT(ballx(lcv) + 5, bally(lcv) - 4) <> backcolor AND POINT(ballx(lcv) + 5, bally(lcv) + 4) <> backcolor THEN

                    ballx(lcv) = ballx(lcv) + 1

                END IF

            CASE 3

                IF POINT(ballx(lcv), bally(lcv) + 5) <> backcolor AND POINT(ballx(lcv) - 4, bally(lcv) + 5) <> backcolor AND POINT(ballx(lcv) + 4, bally(lcv) + 5) <> backcolor THEN

                    bally(lcv) = bally(lcv) + 1

                END IF

            CASE 4

                IF POINT(ballx(lcv) - 5, bally(lcv)) <> backcolor AND POINT(ballx(lcv) - 5, bally(lcv) - 4) <> backcolor AND POINT(ballx(lcv) - 5, bally(lcv) + 4) <> backcolor THEN

                    ballx(lcv) = ballx(lcv) - 1

                END IF

        END SELECT

    NEXT lcv

END SUB

SUB movepac (pacx, pacy, way)
REM moves pacguy to new position if possible

REM move depending on way
REM 1 = forward
REM 2 = right
REM 3 = backword
REM 4 = left


    SELECT CASE way

        CASE 1

REM go forward if possible

            IF POINT(pacx, pacy - wid - 1) <> backcolor AND POINT(pacx - wid + 1, pacy - wid - 1) <> backcolor AND POINT(pacx + wid - 1, pacy - wid - 1) <> backcolor THEN

                pacy = pacy - 1
           
            END IF

        CASE 2

REM go right if possible

            IF POINT(pacx + wid, pacy) <> backcolor AND POINT(pacx + wid, pacy - wid + 1) <> backcolor AND POINT(pacx + wid, pacy + wid - 1) <> backcolor THEN

                pacx = pacx + 1
               

            END IF

        CASE 3

REM go backward if possible
           
            IF POINT(pacx, pacy + wid) <> backcolor AND POINT(pacx - wid + 1, pacy + wid) <> backcolor AND POINT(pacx + wid - 1, pacy + wid) <> backcolor THEN

                pacy = pacy + 1


            END IF

        CASE 4

REM go left if possible
           
            IF POINT(pacx - wid - 1, pacy) <> backcolor AND POINT(pacx - wid - 1, pacy - wid + 1) <> backcolor AND POINT(pacx - wid - 1, pacy + wid - 1) <> backcolor THEN

                pacx = pacx - 1


            END IF


    END SELECT

END SUB

SUB opening (letter(), value())

    CLS

REM grey scaled bar
REM CoOl EfFect

c = 200
bar = 0

    FOR y = 0 TO 199

        IF c = 250 THEN bar = 0

        IF c = 200 THEN bar = 1

        IF bar = 0 THEN c = c - 1

        IF bar = 1 THEN c = c + 1

        LINE (0, y)-(319, y), c

    NEXT y


REM From The Producer of.........

    CALL printg("From the", 160, 40, 2, 145, 1, letter())

    CALL printg("Producer of", 160, 60, 2, 145, 1, letter())

    CALL printg("Black Sun", 160, 80, 2, 85, 1, letter())

    CALL printg("And", 160, 100, 2, 145, 1, letter())

    CALL printg("Scroller", 160, 120, 2, 85, 1, letter())

    CALL printg("I Bring", 160, 140, 2, 145, 1, letter())

    CALL printg("to You...", 160, 160, 2, 145, 1, letter())
   
    DO WHILE INKEY$ = "": LOOP
   
    CALL trans(1, 0)

REM set the values for the square

    x1 = 0
    y1 = 0
    x2 = 319
    y2 = 199

REM set color to the brightest blue

    c = 60

REM make the background

    DO WHILE x1 <= 155

        x1 = x1 + 1
        y1 = y1 + 1
        x2 = x2 - 1
        y2 = y2 - 1
        c = c - .2

        LINE (x1, y1)-(x2, y2), c, B

    LOOP

REM pacguy and john hess in 3-d

    CALL printg("PACGUY", 157, 97, 4, 20, 0, letter())

    'CALL printg("PACGUY", 160, 100, 4, 145, 1, letter())

    CALL printg("P", 60, 100, 4, 145, 1, letter())

    CALL printg("A", 100, 100, 4, 85, 1, letter())

    CALL printg("C", 140, 100, 4, 145, 1, letter())
   
    CALL printg("G", 180, 100, 4, 85, 1, letter())
   
    CALL printg("U", 220, 100, 4, 145, 1, letter())
   
    CALL printg("Y", 260, 100, 4, 85, 1, letter())
   
    CALL printg("By: John Hess", 158, 168, 2, 20, 0, letter())

    CALL printg("By: John Hess", 160, 170, 2, 80, 1, letter())

REM waits for a keyboard hit
   
    DO WHILE INKEY$ = "": LOOP
   
    CALL trans(1, 0)

REM loop with the scrolling colors

    done = 0

DO WHILE done = 0

    done = 0

    x1 = 0
    y1 = 0
    x2 = 319
    y2 = 199

    c = 1

REM make the square(as in the above)

    DO WHILE x1 <= 155

        x1 = x1 + 1
        y1 = y1 + 1
        x2 = x2 - 1
        y2 = y2 - 1
        c = c + 1
        IF c > 4 THEN c = 1

        LINE (x1, y1)-(x2, y2), c, B

    LOOP

REM make the menu

    CALL printg("&Begin Game", 160, 40, 2, 145, 1, letter())

    CALL printg("&Story", 160, 70, 2, 145, 1, letter())

    CALL printg("&Instructions", 160, 100, 2, 145, 1, letter())

    CALL printg("&Payment", 160, 130, 2, 145, 1, letter())

    CALL printg("&Quit", 160, 160, 2, 145, 1, letter())

REM capture the keystroke
   
    i$ = INKEY$

    DO WHILE i$ = ""

        CALL rotatecolor(rotateindex, value())

        i$ = INKEY$

        IF UCASE$(i$) <> "B" AND UCASE$(i$) <> "S" AND UCASE$(i$) <> "I" AND UCASE$(i$) <> "P" AND UCASE$(i$) <> "Q" THEN

            i$ = ""

        END IF

    LOOP

REM check to see what it was

    SELECT CASE UCASE$(i$)

        CASE "B"

REM start the game

            CALL trans(3, 0)

            done = 1

        CASE "S"

REM show the story and then return back to main menu

            CLS

            PAINT (0, 0), 60, 60

            CALL printg("Story", 160, 30, 3, 80, 1, letter())

            CALL printg("After living a plain life as", 160, 50, 1, 145, 1, letter())

            CALL printg("a regular Pacguy, you were put", 160, 70, 1, 145, 1, letter())
           
            CALL printg("in control of the military,", 160, 90, 1, 145, 1, letter())

            CALL printg("destined to live the rest", 160, 110, 1, 145, 1, letter())

            CALL printg("of your life killing enemy", 160, 130, 1, 145, 1, letter())

            CALL printg("tanks as any good", 160, 150, 1, 145, 1, letter())

            CALL printg("steamroller should...", 160, 170, 1, 145, 1, letter())

            CALL printg("Email me at jameshes@seidata.com", 158, 194, 1, 145, 1, letter())

            CALL kbhit

            done = 0

        CASE "I"

REM show the instructions and return back to main menu

            CLS

            PAINT (0, 0), 60, 60

            CALL printg("Instructions", 160, 20, 2, 80, 1, letter())

            CALL printg("Just get all of", 160, 50, 1, 145, 1, letter())

            CALL printg("the flashing dots!!", 160, 70, 1, 145, 1, letter())

            CALL printg("8", 160, 100, 2, 145, 1, letter())

            CALL printg("4    6", 160, 120, 2, 145, 1, letter())

            CALL printg("2", 160, 140, 2, 145, 1, letter())

            CALL printg("Use the keypad", 160, 150, 1, 145, 1, letter())

            CALL printg("above to move", 160, 160, 1, 145, 1, letter())

            CALL printg("PACGUY!", 160, 180, 2, 145, 1, letter())

            CALL printg("Email me at jameshes@seidata.com", 158, 194, 1, 145, 1, letter())

            CALL kbhit

            done = 0

        CASE "P"

REM payment

        PAINT (0, 0), 60, 60

        CALL printg("In order to play", 160, 20, 1, 145, 1, letter())
  
        CALL printg("all of the other", 160, 40, 1, 145, 1, letter())

        CALL printg("levels, you must", 160, 60, 1, 145, 1, letter())

        CALL printg("send 100+ dollars", 160, 80, 1, 145, 1, letter())

        CALL printg("to John Hess at", 160, 100, 1, 145, 1, letter())

        CALL printg("5999 St. Rt. 48", 160, 120, 1, 145, 1, letter())

        CALL printg("Lawrenceburg, IN", 160, 140, 1, 145, 1, letter())

        CALL printg("&(or you could send", 160, 160, 1, 85, 1, letter())

        CALL printg("just two dollars&)", 160, 180, 1, 85, 1, letter())

        CALL printg("Email me at jameshes@seidata.com", 158, 194, 1, 145, 1, letter())

        CALL kbhit


        CASE "Q"

REM show quitting screen the end it
       
            CLS

            PAINT (0, 0), 60, 60

            CALL printg("Thanks", 160, 50, 4, 145, 1, letter())

            CALL printg("For", 160, 100, 4, 145, 1, letter())

            CALL printg("Playing!", 167, 150, 4, 145, 1, letter())

            CALL printg("Email me at jameshes@seidata.com", 158, 194, 1, 145, 1, letter())
           
            DO WHILE INKEY$ = "": LOOP

            CALL trans(3, 0)

            CLS

            END

    END SELECT

LOOP

END SUB

SUB printg (sentence$, x, y, scale, c, flagshade, letter())
REM prints graphical characters

REM finds the length of the sentence
   
    length = LEN(sentence$)

REM find the center x position
   
    x = x - (length * 10 * (scale / 2)) + (scale * 2)
   
REM find the center y position

    y = y - (scale * 5)

REM save time

    widthscale = 8 * scale

REM loop until you get to the end of the sentence

                          
    lcv = 0

    DO WHILE lcv < length

    lcv = lcv + 1

REM place the color into newc
   
        newc = c

REM find new letter
   
        letter$ = MID$(sentence$, lcv, 1)

        IF letter$ = "&" THEN

            lcv = lcv + 1

            letter$ = MID$(sentence$, lcv, 1)
  
            newc = 15

        END IF

REM find spot in array

        index = (ASC(letter$) - 33) * 64
   
        indexold = index

        IF letter$ <> " " THEN

REM loop for the y scale
       
            FOR ypos = 1 TO widthscale STEP scale
       
REM option for shading
               
                IF flagshade = 1 THEN
           
                    newc = newc + 5
       
                END IF

REM main scale loop
     
                FOR times = 1 TO scale
       
REM x scale loop
                   
                    FOR xpos = 1 TO widthscale STEP scale
                   
REM shading option 2      
                       
                        IF flagshade = 2 THEN
                       
                            newc = newc + 1
                       
                        END IF

REM find next pixel      
                       
                        index = index + 1
           
REM if pixel is lit then draw it     
                       
                        IF letter(index) <> 0 THEN
                           
                            LINE (xpos + x, ypos + y)-(xpos + x + scale, ypos + y), newc
                       
                        END IF
       
                    NEXT xpos
           
                    IF flagshade = 2 THEN
               
                        newc = c
           
                    END IF
     
REM reset index  
                   
                    index = index - 8
           
                    ypos = ypos + 1
     
                NEXT times
     
                index = index + 8
     
                ypos = ypos - scale
       
            NEXT ypos
   
        END IF
   
        x = x + (10 * scale)

        

    LOOP

END SUB

SUB rotatecolor (rotateindex, value())
REM rotates green
REM rotates the outside blocks


    rotateindex = rotateindex + 1
    IF rotateindex > 5 THEN rotateindex = 1

    SWAP value(rotateindex), value(rotateindex + 1)

    PALETTE (rotateindex), (value(rotateindex))

    SWAP value(rotateindex + 190), value(rotateindex + 191)

    PALETTE (rotateindex + 190), (value(rotateindex + 190))

     

END SUB

SUB trans (flag, c)
REM screen transitions

    SELECT CASE flag

        CASE 1

REM square

            x1 = 160
            
            x2 = 160

            y1 = 100

            y2 = 100

            DO WHILE x1 > 0

                x1 = x1 - 3

                x2 = x2 + 3

                y1 = y1 - 3

                y2 = y2 + 3

                LINE (x1, y1)-(x2, y2), c, BF

            LOOP

        CASE 2

REM circle

            FOR radius = 1 TO 200

                CIRCLE (160, 100), radius, c

            NEXT radius

        CASE 3

REM dots      
           
            FOR times = 1 TO 200

                x = INT(RND * 315)

                y = INT(RND * 195)

                LINE (x, y)-(x + 5, y + 5), c, BF

                FOR t = 1 TO 200: NEXT t

            NEXT times

            CLS
   
    END SELECT

                   

END SUB

SUB winner (letter())
REM winning screen

    CLS

    PAINT (0, 0), 60, 60

    CALL printg("Yes,", 160, 20, 3, 145, 1, letter())

    CALL printg("You", 160, 60, 3, 145, 1, letter())

    CALL printg("Did", 160, 100, 3, 145, 1, letter())

    CALL printg("Win", 160, 140, 3, 145, 1, letter())

    CALL printg("But...", 160, 180, 3, 145, 1, letter())

    i$ = INKEY$
   
    CALL kbhit

    PAINT (0, 0), 60, 60

    CALL printg("In order to play", 160, 20, 1, 145, 1, letter())
   
    CALL printg("all of the other", 160, 40, 1, 145, 1, letter())

    CALL printg("levels, you must", 160, 60, 1, 145, 1, letter())

    CALL printg("send 100+ dollars", 160, 80, 1, 145, 1, letter())

    CALL printg("to John Hess at", 160, 100, 1, 145, 1, letter())

    CALL printg("5999 St. Rt. 48", 160, 120, 1, 145, 1, letter())

    CALL printg("Lawrenceburg, IN", 160, 140, 1, 145, 1, letter())

    CALL printg("(or you could send", 160, 160, 1, 85, 1, letter())

    CALL printg("just two dollars)", 160, 180, 1, 85, 1, letter())

    CALL kbhit


END SUB

