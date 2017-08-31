SCREEN 13             'set the screen mode
RANDOMIZE TIMER       'set the random seed
DIM x(300) AS INTEGER 'create 300 varibles for the x and y of every point in
DIM y(300) AS INTEGER 'the trail
start:
CLS
score = 0
x(0) = INT(RND * 318) + 1  'set the starting location
y(0) = INT(RND * 191) + 9
px = INT(RND * 318) + 1    'set the Pi's starting point
py = INT(RND * 191) + 9
l = 1                      'set the starting length
LINE (0, 9)-(319, 199), 15, B   'draw a box around the screen, leaving space for the score
xs = 0                     'xs/ys=the amount the x/y will change by each cycle
ys = 0
dir = 1
DO
CIRCLE (px, py), 4, 10  'draw a circle for the Pi
'This next part is hard to understand.
'In order to clear the dot at the back of the snake, we must remember each
'point in between.
'Each cycle, before x(0) and y(0), which are the front of the snake, move, we
'must shift the x and y of each point to the next variable.
'In this way, the x and y of each point stay where they are while the snake
'moves forward away from them, until they are at the back and are cleared later
'in the code
FOR i = 300 TO 1 STEP -1  'if we dont go backwards all the numbers will be the same
x(i) = x(i - 1)           'shift the snake along
y(i) = y(i - 1)
NEXT i
IF dir = 1 THEN ys = -1: xs = 0
IF dir = 2 THEN xs = -1: ys = 0
IF dir = 3 THEN ys = 1: xs = 0
IF dir = 4 THEN xs = 1: ys = 0
IF POINT(x(0) + xs, y(0) + ys) > 0 THEN 'if the point the snake is moving to isnt black
        IF POINT(x(0) + xs, y(0) + ys) = 10 THEN 'if it's the color of the Pi
                score = score + 1             'increase the score
                l = l + 3                     'increase the length
                CIRCLE (px, py), 4, 0         'clear the Pi
                LINE (0, 9)-(319, 199), 15, B 'redraw the box incase part of it was erased
                px = INT(RND * 318) + 1       'set the Pi's new location
                py = INT(RND * 191) + 9
        ELSEIF POINT(x(0) + xs, y(0) + ys) <> 10 THEN 'if it's not the color of the Pi
                CLS
                LOCATE 1, 1
                PRINT "You lose"
                PRINT "Your score"; score    'print the score
                PRINT "Press any key"
                SLEEP                        'wait for any key
                GOTO start                          'exit the program
        END IF
END IF
x(0) = x(0) + xs
y(0) = y(0) + ys
PSET (x(0), y(0)), 15  'put a dot at the front of the snake
PSET (x(l), y(l)), 0   'clear the dot at the end of the snake
in$ = INKEY$            'note:user is to use the numpad
                '  dont left the user kill themselves by doubling back
IF in$ = "1" THEN         'up
        dir = dir + 1
        IF dir = 5 THEN dir = 1
ELSEIF in$ = "2" THEN   'left
        dir = dir - 1
        IF dir = 0 THEN dir = 4
END IF
IF INKEY$ = CHR$(27) THEN END
LOCATE 1, 1
PRINT "Score:"; score    'print the score
FOR i = 0 TO 10000       'this wastes time, slowing to program to a playable
NEXT i                   'speed
LOOP 'restart the loop

