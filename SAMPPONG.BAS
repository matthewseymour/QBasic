10 SCREEN 12
20 RANDOMIZE TIMER
30 DIM pad(2)   AS INTEGER      'remebers the location of the player's paddles
40 DIM paddir(2)   AS INTEGER   'remembers the direction of the player's paddles
50 DIM score(2)   AS INTEGER
60 endofgame% = 10
70 menu:
80 CLS
90 COLOR 10
100 DO
120 LOCATE 1, 1
130 PRINT "Menu"
140 PRINT "1. Start a new game"
150 PRINT "2. Set goal"; endofgame%
160 PRINT "3. Quit"
170 a$ = INKEY$
180 IF a$ = "1" THEN GOTO game
190 IF a$ = "2" THEN
200         LOCATE 5, 1
210         INPUT "First to ? wins: ", endofgame%
220         CLS
230         GOTO menu
240 END IF
250 IF a$ = "3" THEN END

260 LOOP

270 game:
280 CLS
290 serve = INT(RND * 2) + 1    'Who serves
300 FOR i = 1 TO 2
310 pad(i) = 260
320 paddir(i) = 0
330 score(i) = 0
340 NEXT i
350 DO
360 CIRCLE (bx, by), 10, 0      'clear the ball before we move it
370 IF serve > 0 THEN           'If there is a player serving
380         IF serve = 1 THEN bx = 20 ELSE bx = 619     'place the ball in front of
390         by = pad(serve)                             'the server's paddle
400 ELSE
410         bx = bx + bxs   'bxs = ball x-speed
420         by = by + bys   'bys = ball y-speed
430         IF by < 31 THEN bys = ABS(bys)    'if the ball hits the top of the screen make it move downwards
440         IF by > 468 THEN bys = -ABS(bys)    'the opposite for the bottom of the screen
450         IF bx < 20 AND ABS(by - pad(1)) < 35 THEN   'if the ball hits player 1
460                 bxs = ABS(bxs)                      's paddle, then go towards the
470                 bx = 20                             'left
480         END IF
490         IF bx > 619 AND ABS(by - pad(2)) < 35 THEN  'same for player 2
500                 bxs = -ABS(bxs)
510                 bx = 619
520         END IF
530         IF bx < 1 THEN  'if the ball scores on player 1
540                 score(2) = score(2) + 1
550                 serve = 2
560         END IF
570         IF bx > 638 THEN  'if the ball scores on player 2
580                 score(1) = score(1) + 1
590                 serve = 1
600         END IF
610 END IF
620 CIRCLE (bx, by), 10, 15    'now that it has been moved, we redraw the ball
630 FOR i = 1 TO 2
640 IF score(i) >= endofgame% THEN  'if the player wins
650         CLS
660         COLOR i + 8
670         PRINT "Player"; i; "wins!"
680         SLEEP
690         GOTO menu
700 END IF
710 pad(i) = pad(i) + paddir(i)    'move the paddle
720 IF pad(i) > 453 THEN pad(i) = 453     'don't let the paddle go beyond the
730 IF pad(i) < 46 THEN pad(i) = 46     'edge of the screen (paddle is 50 pixels tall)
740 NEXT i
750 LOCATE 1, 9
760 COLOR 9
770 PRINT score(1)
780 LOCATE 1, 70
790 COLOR 10
800 PRINT score(2)
810 LINE (0, 20)-(639, 479), 14, B       'draw a box around the screen
820 LINE (1, pad(1) - 25)-(10, pad(1) + 25), 9, BF     'draw player 1's paddle
830 LINE (1, pad(1) - 26)-(10, 21), 0, BF     'draw a black box above and below
840 LINE (1, pad(1) + 26)-(10, 478), 0, BF    'the paddle so when the paddle moves
                                              'it doesn't leave a trail
850 LINE (638, pad(2) - 25)-(629, pad(2) + 25), 10, BF   'The same for player 2
860 LINE (638, pad(2) - 26)-(629, 21), 0, BF
870 LINE (638, pad(2) + 26)-(629, 478), 0, BF
880 a$ = INKEY$
'Note: there is no need to hold the controls
'you press up and the paddle moves up until
'you press stop
890 IF a$ = "q" THEN paddir(1) = -3   'Player 1's controls: q = up
900 IF a$ = "a" THEN paddir(1) = 0    '                     a = stop                                
910 IF a$ = "z" THEN paddir(1) = 3    '                     z = down
920 IF a$ = "s" AND serve = 1 THEN    '                     s = serve
930         serve = 0          'no one is serving anymore
940         bxs = 3            'the ball moves to the right
950         bys = paddir(1)    'the ball moves with the paddle
960         IF bys = 0 THEN bys = 3   'make sure the ball doesnt move horzonital
970 END IF
980 IF a$ = "]" THEN paddir(2) = -3   'Player 2's controls: ] = up
990 IF a$ = "'" THEN paddir(2) = 0    '                     ' = stop                        
1000 IF a$ = "/" THEN paddir(2) = 3   '                     / = down
1010 IF a$ = ";" AND serve = 2 THEN   '                     ; = serve
1020         serve = 0         'no one is serving anymore
1030         bxs = -3          'the ball moves to the left
1040         bys = paddir(2)   'the ball moves with the paddle
1050         IF bys = 0 THEN bys = 3  'make sure the ball doesnt move horzonital
1060 END IF
1070 IF a$ = CHR$(27) THEN GOTO menu   'if the user presses esc, goto menu
1080 LOOP

