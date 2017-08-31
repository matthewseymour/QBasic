CLS
SCREEN 12
RANDOMIZE TIMER
WIDTH 80, 60
DIM itemx(1, 15) AS INTEGER
DIM itemy(1, 15) AS INTEGER
DIM tx(1 TO 100) AS INTEGER
DIM ty(1 TO 100) AS INTEGER
a& = 1
pad& = 1
scoret& = 4
max& = 100
dir& = 1
it1& = 1
it2& = 1
it3& = 1
it4& = 1
it5& = 1
it6& = 1
ballx& = 50
bally& = 10
pad1& = 50
pad2& = 50
pad3& = 50
pad4& = 50
p1& = 1
p2& = 1
p3& = 1
p4& = 1
co& = 1
items& = 1
it& = 5

GOTO start

new:
p1items& = 0
p2items& = 0
p3items& = 0
p4items& = 0
ly1& = 1
lx1& = 1
ly2& = 479
lx2& = 479
IF p1& = 0 THEN ly1& = 60
IF p2& = 0 THEN lx1& = 60
IF p3& = 0 THEN ly2& = 420
IF p4& = 0 THEN lx2& = 420
started& = 1
IF items& = 0 THEN it& = 0
IF p1& = 7 THEN p1& = INT(RND * 5) + 2
IF p2& = 7 THEN p2& = INT(RND * 5) + 2
IF p3& = 7 THEN p3& = INT(RND * 5) + 2
IF p4& = 7 THEN p4& = INT(RND * 5) + 2
IF p1& = 7 THEN p1r& = 1
IF p2& = 7 THEN p2r& = 1
IF p3& = 7 THEN p3r& = 1
IF p4& = 7 THEN p4r& = 1
                 
IF p1& = 1 THEN p1com& = 0
IF p2& = 1 THEN p2com& = 0
IF p3& = 1 THEN p3com& = 0
IF p4& = 1 THEN p4com& = 0
IF p1& > 1 THEN p1com& = 1
IF p2& > 1 THEN p2com& = 1
IF p3& > 1 THEN p3com& = 1
IF p4& > 1 THEN p4com& = 1
IF p1& = 6 THEN p1ac& = 15
IF p1& = 5 THEN p1ac& = 30
IF p1& = 4 THEN p1ac& = 45
IF p1& = 3 THEN p1ac& = 60
IF p1& = 2 THEN p1ac& = 70
IF p2& = 6 THEN p2ac& = 15
IF p2& = 5 THEN p2ac& = 30
IF p2& = 4 THEN p2ac& = 45
IF p2& = 3 THEN p2ac& = 60
IF p2& = 2 THEN p2ac& = 70
IF p3& = 6 THEN p3ac& = 15
IF p3& = 5 THEN p3ac& = 30
IF p3& = 4 THEN p3ac& = 45
IF p3& = 3 THEN p3ac& = 60
IF p3& = 2 THEN p3ac& = 70
IF p4& = 6 THEN p4ac& = 15
IF p4& = 5 THEN p4ac& = 30
IF p4& = 4 THEN p4ac& = 45
IF p4& = 3 THEN p4ac& = 60
IF p4& = 2 THEN p4ac& = 70
IF p1& = 6 THEN p1ai& = 400
IF p1& = 5 THEN p1ai& = 320
IF p1& = 4 THEN p1ai& = 240
IF p1& = 3 THEN p1ai& = 160
IF p1& = 2 THEN p1ai& = 80
IF p2& = 6 THEN p2ai& = 400
IF p2& = 5 THEN p2ai& = 320
IF p2& = 4 THEN p2ai& = 240
IF p2& = 3 THEN p2ai& = 160
IF p2& = 2 THEN p2ai& = 80
IF p3& = 6 THEN p3ai& = 400
IF p3& = 5 THEN p3ai& = 320
IF p3& = 4 THEN p3ai& = 240
IF p3& = 3 THEN p3ai& = 160
IF p3& = 2 THEN p3ai& = 80
IF p4& = 6 THEN p4ai& = 400
IF p4& = 5 THEN p4ai& = 320
IF p4& = 4 THEN p4ai& = 240
IF p4& = 3 THEN p4ai& = 160
IF p4& = 2 THEN p4ai& = 80
delay& = 0
p1score& = 0
p2score& = 0
p3score& = 0
p4score& = 0
p1descore& = 0
p2descore& = 0
p3descore& = 0
p4descore& = 0
IF scoret& = 3 OR scoret& = 6 THEN
        p1descore& = max&
        p2descore& = max&
        p3descore& = max&
        p4descore& = max&
END IF
p1des& = 240
p2des& = 240
p3des& = 240
p4des& = 240
p1pad& = 240
p2pad& = 240
p3pad& = 240
p4pad& = 240
p1size& = 0
p2size& = 0
p3size& = 0
p4size& = 0
p1desize& = 25
p2desize& = 25
p3desize& = 25
p4desize& = 25
p1dir& = 0
p2dir& = 0
p3dir& = 0
p4dir& = 0
p1scores& = 0
p2scores& = 0
p3scores& = 0
p4scores& = 0
p1speed& = 4 + pad& * 3
p2speed& = 4 + pad& * 3
p3speed& = 4 + pad& * 3
p4speed& = 4 + pad& * 3
lasthit& = serve&
combo& = 1
colo& = INT(RND * 5) + 1
FOR i = 1 TO it&
itemx(1, i) = INT(RND * 340) + 70
itemy(1, i) = INT(RND * 340) + 70
NEXT i
DO
        serve& = INT(RND * 4) + 1
        IF serve& = 1 AND p1& > 0 THEN EXIT DO
        IF serve& = 2 AND p2& > 0 THEN EXIT DO
        IF serve& = 3 AND p3& > 0 THEN EXIT DO
        IF serve& = 4 AND p4& > 0 THEN EXIT DO
LOOP
lasthit& = serve&
finished& = 0
subtract& = 0
startedd& = TIMER
xs& = 0
ys& = 0

restart:

ballspeed& = ballspeed& * 2
scombo& = 0
CLS
IF p1& > 0 THEN
        LOCATE 2, 62
        COLOR 1
        PRINT "Score:"; p1score&
        LOCATE 3, 62
        IF p1com& = 1 THEN
                IF p1& = 2 THEN PRINT "Computer Easiest"
                IF p1& = 3 THEN PRINT "Computer Easy"
                IF p1& = 4 THEN PRINT "Computer Medium"
                IF p1& = 5 THEN PRINT "Computer Hard"
                IF p1& = 6 THEN PRINT "Computer Hardest"
        END IF
        IF p1com& = 1 THEN GOTO ppp2
        PRINT "Human"
        LOCATE 4, 62
        PRINT "Keys:"
        LOCATE 5, 62
        PRINT "G-Serve"
        LOCATE 6, 62
        PRINT "V-Left"
        LOCATE 7, 62
        PRINT "B-Stop"
        LOCATE 8, 62
        PRINT "N - Right"
        LOCATE 9, 62
END IF
ppp2:
IF p2& > 0 THEN
        LOCATE 17, 62
        COLOR 4
        PRINT "Score:"; p2score&
        LOCATE 18, 62
        IF p2com& = 1 THEN
                IF p2& = 2 THEN PRINT "Computer Easiest"
                IF p2& = 3 THEN PRINT "Computer Easy"
                IF p2& = 4 THEN PRINT "Computer Medium"
                IF p2& = 5 THEN PRINT "Computer Hard"
                IF p2& = 6 THEN PRINT "Computer Hardest"
        END IF
        IF p2com& = 1 THEN GOTO ppp3
        PRINT "Human"
        LOCATE 19, 62
        PRINT "Keys:"
        LOCATE 20, 62
        PRINT "S-Serve"
        LOCATE 21, 62
        PRINT "Q-Up"
        LOCATE 22, 62
        PRINT "A-Stop"
        LOCATE 23, 62
        PRINT "Z-Down"
        LOCATE 24, 62
END IF
ppp3:
IF p3& > 0 THEN
        LOCATE 32, 62
        COLOR 2
        PRINT "Score:"; p3score&
        LOCATE 33, 62
        IF p3com& = 1 THEN
                IF p3& = 2 THEN PRINT "Computer Easiest"
                IF p3& = 3 THEN PRINT "Computer Easy"
                IF p3& = 4 THEN PRINT "Computer Medium"
                IF p3& = 5 THEN PRINT "Computer Hard"
                IF p3& = 6 THEN PRINT "Computer Hardest"
        END IF
       
        IF p3com& = 1 THEN GOTO ppp4
        PRINT "Human"
        LOCATE 34, 62
        PRINT "Keys:"
        LOCATE 35, 62
        PRINT "5-Serve"
        LOCATE 36, 62
        PRINT "1-Left"
        LOCATE 37, 62
        PRINT "2-Stop"
        LOCATE 38, 62
        PRINT "3-Right"
        LOCATE 39, 62
END IF
ppp4:
IF p4& > 0 THEN
        LOCATE 47, 62
        COLOR 14
        PRINT "Score:"; p4score&
        LOCATE 48, 62
        IF p4com& = 1 THEN
                IF p4& = 2 THEN PRINT "Computer Easiest"
                IF p4& = 3 THEN PRINT "Computer Easy"
                IF p4& = 4 THEN PRINT "Computer Medium"
                IF p4& = 5 THEN PRINT "Computer Hard"
                IF p4& = 6 THEN PRINT "Computer Hardest"
        END IF
        IF p4com& = 1 THEN GOTO fff
        PRINT "Human"
        LOCATE 49, 62
        PRINT "Keys:"
        LOCATE 50, 62
        PRINT "; -Serve"
        LOCATE 51, 62
        PRINT "] -Up"
        LOCATE 52, 62
        PRINT "' -Stop"
        LOCATE 53, 62
        PRINT "/ -Down"
        LOCATE 54, 62
END IF

fff:

IF p1& > 0 THEN LINE (481, 1)-(639, 119), 1, B
IF p1& > 0 THEN LINE (482, 2)-(638, 118), 9, B
IF p1& > 0 THEN LINE (483, 3)-(637, 117), 1, B
IF p2& > 0 THEN LINE (481, 121)-(639, 239), 4, B
IF p2& > 0 THEN LINE (482, 122)-(638, 238), 12, B
IF p2& > 0 THEN LINE (483, 123)-(637, 237), 4, B
IF p3& > 0 THEN LINE (481, 241)-(639, 359), 2, B
IF p3& > 0 THEN LINE (482, 242)-(638, 358), 10, B
IF p3& > 0 THEN LINE (483, 243)-(637, 357), 2, B
IF p4& > 0 THEN LINE (481, 361)-(639, 479), 6, B
IF p4& > 0 THEN LINE (482, 362)-(638, 478), 14, B
IF p4& > 0 THEN LINE (483, 363)-(637, 477), 6, B
VIEW SCREEN (lx1&, ly1&)-(lx2&, ly2&)

DO

IF delay& < 0 THEN delay& = 0

FOR i = 1 TO delay&
NEXT i

finished& = TIMER

IF scoret& = 1 OR scoret& = 4 THEN
        IF p1descore& > max& - 1 THEN
                IF max& > 0 THEN
                        GOTO theend
                END IF
        ELSEIF p2descore& > max& - 1 THEN
                IF max& > 0 THEN
                        GOTO theend
                END IF
        ELSEIF p3descore& > max& - 1 THEN
                IF max& > 0 THEN
                        GOTO theend
                END IF
        ELSEIF p4descore& > max& - 1 THEN
                IF max& > 0 THEN
                        GOTO theend
                END IF
        END IF
ELSEIF max& > 0 AND (scoret& = 3 OR scoret& = 6) THEN
        IF p1descore& < 1 THEN
                GOTO theend
        ELSEIF p2descore& < 1 THEN
                GOTO theend
        ELSEIF p3descore& < 1 THEN
                GOTO theend
        ELSEIF p4descore& < 1 THEN
                GOTO theend
        END IF
END IF

IF noserve& < 50 THEN noserve& = noserve& + 1

skip& = skip& + 1
IF skip& = 3 THEN
        skip& = 0
        IF p1score& < p1descore& AND p1& > 0 THEN
                IF p1descore& - p1score& > 10000 THEN p1score& = p1score& + 10000
                IF p1descore& - p1score& > 1000 THEN p1score& = p1score& + 1000
                IF p1descore& - p1score& > 100 THEN p1score& = p1score& + 100
                IF p1descore& - p1score& > 10 THEN p1score& = p1score& + 10
                IF p1descore& - p1score& > 0 THEN p1score& = p1score& + 1
                SOUND 1000, .1
        ELSEIF p2score& < p2descore& AND p2& > 0 THEN
                IF p2descore& - p2score& > 10000 THEN p2score& = p2score& + 10000
                IF p2descore& - p2score& > 1000 THEN p2score& = p2score& + 1000
                IF p2descore& - p2score& > 100 THEN p2score& = p2score& + 100
                IF p2descore& - p2score& > 10 THEN p2score& = p2score& + 10
                IF p2descore& - p2score& > 0 THEN p2score& = p2score& + 1
                SOUND 1000, .1
        ELSEIF p3score& < p3descore& AND p3& > 0 THEN
                IF p3descore& - p3score& > 10000 THEN p3score& = p3score& + 10000
                IF p3descore& - p3score& > 1000 THEN p3score& = p3score& + 1000
                IF p3descore& - p3score& > 100 THEN p3score& = p3score& + 100
                IF p3descore& - p3score& > 10 THEN p3score& = p3score& + 10
                IF p3descore& - p3score& > 0 THEN p3score& = p3score& + 1
                SOUND 1000, .1
        ELSEIF p4score& < p4descore& AND p4& > 0 THEN
                IF p4descore& - p4score& > 10000 THEN p4score& = p4score& + 10000
                IF p4descore& - p4score& > 1000 THEN p4score& = p4score& + 1000
                IF p4descore& - p4score& > 100 THEN p4score& = p4score& + 100
                IF p4descore& - p4score& > 10 THEN p4score& = p4score& + 10
                IF p4descore& - p4score& > 0 THEN p4score& = p4score& + 1
                SOUND 1000, .1
        ELSEIF p1score& > p1descore& AND p1& > 0 THEN
                p1score& = p1score& - 1
                p1change& = p1change& + 1
                IF p1change& = 10 THEN p1score& = p1descore&
                IF p1change& = 10 THEN p1change& = 0
                SOUND 1000, .1
        ELSEIF p2score& > p2descore& AND p2& > 0 THEN
                p2score& = p2score& - 1
                p2change& = p2change& + 1
                IF p2change& = 10 THEN p2score& = p2descore&
                IF p2change& = 10 THEN p2change& = 0
                SOUND 1000, .1
        ELSEIF p3score& > p3descore& AND p3& > 0 THEN
                p3score& = p3score& - 1
                p3change& = p3change& + 1
                IF p3change& = 10 THEN p3score& = p3descore&
                IF p3change& = 10 THEN p3change& = 0
                SOUND 1000, .1
        ELSEIF p4score& > p4descore& AND p4& > 0 THEN
                p4score& = p4score& - 1
                p4change& = p4change& + 1
                IF p4change& = 10 THEN p4score& = p4descore&
                IF p4change& = 10 THEN p4change& = 0
                SOUND 1000, .1
        END IF
END IF

IF p1size& > p1desize& THEN p1size& = p1size& - 1
IF p1size& < p1desize& THEN p1size& = p1size& + 1
IF p2size& > p2desize& THEN p2size& = p2size& - 1
IF p2size& < p2desize& THEN p2size& = p2size& + 1
IF p3size& > p3desize& THEN p3size& = p3size& - 1
IF p3size& < p3desize& THEN p3size& = p3size& + 1
IF p4size& > p4desize& THEN p4size& = p4size& - 1
IF p4size& < p4desize& THEN p4size& = p4size& + 1

IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)
IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)
IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)
IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)

IF scombo& > -1 THEN
        COLOR 0
        IF scombo& > 0 THEN COLOR 3
        LOCATE 30, 30
        PRINT combo&
        scombo& = scombo& - 1
END IF
VIEW
IF p1& > 0 THEN
        LOCATE 2, 62
        COLOR 1
        PRINT "Score:"; p1score&
END IF
IF p2& > 0 THEN
        LOCATE 17, 62
        COLOR 4
        PRINT "Score:"; p2score&
END IF
IF p3& > 0 THEN
        LOCATE 32, 62
        COLOR 2
        PRINT "Score:"; p3score&
END IF
IF p4& > 0 THEN
        LOCATE 47, 62
        COLOR 14
        PRINT "Score:"; p4score&
END IF

VIEW SCREEN (lx1& - 1, ly1& - 1)-(lx2&, ly2&)

FOR i = 1 TO 30
PSET (tx(i), ty(i)), 0
NEXT i
FOR i = 31 TO 60
CIRCLE (tx(i), ty(i)), 1, 0
NEXT i
FOR i = 61 TO 99
CIRCLE (tx(i), ty(i)), 1, 0
CIRCLE (tx(i), ty(i)), 2, 0
NEXT i
LINE (x& - 1, y& - 1)-(x& + 1, y& + 1), 0, BF
CIRCLE (x&, y&), 2, 0
CIRCLE (x&, y&), 3, 0
FOR i = 1 TO 98
tx(i) = tx(i + 1)
ty(i) = ty(i + 1)
NEXT i
tx(99) = x&
ty(99) = y&

FOR ba = 1 TO a&
IF serve& = 1 THEN
        x& = p1pad&
        y& = 13
ELSEIF serve& = 2 THEN
        x& = 13
        y& = p2pad&
ELSEIF serve& = 3 THEN
        x& = p3pad&
        y& = 467
ELSEIF serve& = 4 THEN
        x& = 467
        y& = p4pad&
END IF
IF x& < 65 AND xs& = 0 THEN xs& = -1
IF x& > 415 AND xs& = 0 THEN xs& = 1
IF y& < 65 AND ys& = 0 THEN ys& = -1
IF y& > 415 AND ys& = 0 THEN ys& = 1
x& = x& + xs&
y& = y& + ys&
IF x& < 60 AND y& < 60 THEN
        FOR i = 1 TO 60
                IF (x& - 3) < i AND y& - 3 < (60 - i) THEN
                        SWAP xs&, ys&
                        xs& = -xs&
                        ys& = -ys&
                        IF ABS(xs&) = ABS(ys&) THEN xs& = xs& + 1
                        x& = i + 3
                        y& = 63 - i
                        SOUND 750, .1
                END IF
        NEXT i
END IF
IF x& < 60 AND y& > 420 THEN
        FOR i = 1 TO 60
                IF (x& - 3) < i AND (y& + 3) > (420 + i) THEN
                        SWAP xs&, ys&
                        IF ABS(xs&) = ABS(ys&) THEN xs& = xs& + 1
                        x& = i + 3
                        y& = 417 + i
                        SOUND 750, .1
                END IF
        NEXT i
END IF
IF x& > 420 AND y& < 60 THEN
        FOR i = 1 TO 60
                IF (x& + 3) > (480 - i) AND (y& - 3) < (60 - i) THEN
                        SWAP xs&, ys&
                        IF ABS(xs&) = ABS(ys&) THEN xs& = xs& + 1
                        x& = 477 - i
                        y& = 63 - i
                        SOUND 750, .1
                END IF
        NEXT i
END IF
IF x& > 420 AND y& > 420 THEN
        FOR i = 1 TO 60
                IF (x& + 3) > (480 - i) AND (y& + 3) > (420 + i) THEN
                        SWAP xs&, ys&
                        xs& = -xs&
                        ys& = -ys&
                        IF ABS(xs&) = ABS(ys&) THEN xs& = xs& + 1
                        x& = 477 - i
                        y& = 417 + i
                        SOUND 750, .1
                END IF
        NEXT i
END IF
IF countdown& > 0 THEN countdown& = countdown& - 1
FOR i = 1 TO it&
IF it1& = 0 AND it2& = 0 AND it3& = 0 AND it4& = 0 AND it5& = 0 AND it6& = 0 THEN EXIT FOR
IF x& < itemx(1, i) + 9 THEN
        IF x& > itemx(1, i) - 9 THEN
                IF y& < itemy(1, i) + 9 THEN
                        IF y& > itemy(1, i) - 9 THEN
                                IF hit& = i AND countdown& > 0 THEN GOTO endder
                                hit& = i
                                countdown& = 20
                                GOSUB item
                        END IF
                END IF
        END IF
END IF
endder:
NEXT i
IF y& < 60 AND p1& = 0 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        ys& = -ys&
        IF ys& = 0 THEN ys& = 1
        y& = 60
        SOUND 500, .1
ELSEIF x& < 60 AND p2& = 0 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        xs& = -xs&
        IF xs& = 0 THEN xs& = 1
        x& = 60
        SOUND 500, .1
ELSEIF y& > 420 AND p3& = 0 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        ys& = -ys&
        IF ys& = 0 THEN ys& = -1
        y& = 420
        SOUND 500, .1
ELSEIF x& > 420 AND p4& = 0 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        xs& = -xs&
        IF xs& = 0 THEN xs& = -1
        x& = 420
        SOUND 500, .1
END IF
IF y& < 8 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF x& > (p1pad& - p1size&) THEN
                IF x& < (p1pad& + p1size&) THEN
                        ys& = INT(RND * 8) + 1
                        IF ys& = 0 THEN ys& = 1
                        IF xs& = 0 THEN xs& = INT(RND * 17) - 8
                        IF p1& > 1 THEN p1ac2& = INT(RND * (p1ac& * 2)) - p1ac&
                        IF p2& > 1 THEN p2ac2& = INT(RND * (p2ac& * 2)) - p2ac&
                        IF p3& > 1 THEN p3ac2& = INT(RND * (p3ac& * 2)) - p3ac&
                        IF p4& > 1 THEN p4ac2& = INT(RND * (p4ac& * 2)) - p4ac&
                        IF p1& > 0 THEN combo& = combo& + 1
                        IF p1& > 0 THEN scombo& = 50
                        IF p1& > 0 THEN lasthit& = 1
                        y& = 8
                        SOUND 700, .1
                END IF
        END IF
ELSEIF x& < 8 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF y& > (p2pad& - p2size&) THEN
                IF y& < (p2pad& + p2size&) THEN
                        xs& = INT(RND * 8) + 1
                        IF xs& = 0 THEN xs& = 1
                        IF ys& = 0 THEN ys& = INT(RND * 17) - 8
                        IF xs& > 8 THEN xs& = 8
                        IF p1& > 1 THEN p1ac2& = INT(RND * (p1ac& * 2)) - p1ac&
                        IF p2& > 1 THEN p2ac2& = INT(RND * (p2ac& * 2)) - p2ac&
                        IF p3& > 1 THEN p3ac2& = INT(RND * (p3ac& * 2)) - p3ac&
                        IF p4& > 1 THEN p4ac2& = INT(RND * (p4ac& * 2)) - p4ac&
                        IF p2& > 0 THEN combo& = combo& + 1
                        IF p2& > 0 THEN scombo& = 50
                        IF p2& > 0 THEN lasthit& = 2
                        x& = 8
                        SOUND 700, .1
                END IF
        END IF
ELSEIF y& > 472 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF x& > (p3pad& - p3size&) THEN
                IF x& < (p3pad& + p3size&) THEN
                        ys& = -(INT(RND * 8) + 1)
                        IF ys& = 0 THEN ys& = -1
                        IF xs& = 0 THEN xs& = INT(RND * 17) - 8
                        IF ys& < -8 THEN ys& = -8
                        IF p1& > 1 THEN p1ac2& = INT(RND * (p1ac& * 2)) - p1ac&
                        IF p2& > 1 THEN p2ac2& = INT(RND * (p2ac& * 2)) - p2ac&
                        IF p3& > 1 THEN p3ac2& = INT(RND * (p3ac& * 2)) - p3ac&
                        IF p4& > 1 THEN p4ac2& = INT(RND * (p4ac& * 2)) - p4ac&
                        IF p3& > 0 THEN combo& = combo& + 1
                        IF p3& > 0 THEN scombo& = 50
                        IF p3& > 0 THEN lasthit& = 3
                        y& = 472
                        SOUND 700, .1
                END IF
        END IF
ELSEIF x& > 472 THEN
        LINE (x& - 3, y& - 3)-(x& + 3, y& + 3), 0, BF
        IF y& > (p4pad& - p4size&) THEN
                IF y& < (p4pad& + p4size&) THEN
                        xs& = -(INT(RND * 8) + 1)
                        IF xs& = 0 THEN xs& = -1
                        IF ys& = 0 THEN ys& = INT(RND * 17) - 8
                        IF xs& < -8 THEN xs& = -8
                        IF p1& > 1 THEN p1ac2& = INT(RND * (p1ac& * 2)) - p1ac&
                        IF p2& > 1 THEN p2ac2& = INT(RND * (p2ac& * 2)) - p2ac&
                        IF p3& > 1 THEN p3ac2& = INT(RND * (p3ac& * 2)) - p3ac&
                        IF p4& > 1 THEN p4ac2& = INT(RND * (p4ac& * 2)) - p4ac&
                        IF p4& > 0 THEN combo& = combo& + 1
                        IF p4& > 0 THEN scombo& = 50
                        IF p4& > 0 THEN lasthit& = 4
                        x& = 472
                        SOUND 700, .1
                END IF
        END IF
END IF

IF y& < 0 AND p1& > 0 THEN GOSUB score1
IF x& < 0 AND p2& > 0 THEN GOSUB score2
IF y& > 480 AND p3& > 0 THEN GOSUB score3
IF x& > 480 AND p4& > 0 THEN GOSUB score4

NEXT ba

FOR i = 1 TO 15
PSET (tx(i), ty(i)), 8
NEXT i
FOR i = 16 TO 23
PSET (tx(i), ty(i)), 7
NEXT i
FOR i = 24 TO 30
PSET (tx(i), ty(i)), 4
NEXT i
FOR i = 31 TO 55
CIRCLE (tx(i), ty(i)), 1, 4
NEXT i
FOR i = 56 TO 80
CIRCLE (tx(i), ty(i)), 1, 12
NEXT i
FOR i = 81 TO 99
CIRCLE (tx(i), ty(i)), 1, 14
CIRCLE (tx(i), ty(i)), 2, 4
NEXT i

LINE (x& - 1, y& - 1)-(x& + 1, y& + 1), 14, BF
CIRCLE (x&, y&), 2, 12
CIRCLE (x&, y&), 3, 4

IF p1& > 0 AND p1dir& = 2 THEN p1pad& = p1pad& - p1speed&
IF p1& > 0 AND p1dir& = 1 THEN p1pad& = p1pad& + p1speed&

IF p2& > 0 AND p2dir& = 2 THEN p2pad& = p2pad& - p2speed&
IF p2& > 0 AND p2dir& = 1 THEN p2pad& = p2pad& + p2speed&

IF p3& > 0 AND p3dir& = 2 THEN p3pad& = p3pad& - p3speed&
IF p3& > 0 AND p3dir& = 1 THEN p3pad& = p3pad& + p3speed&

IF p4& > 0 AND p4dir& = 2 THEN p4pad& = p4pad& - p4speed&
IF p4& > 0 AND p4dir& = 1 THEN p4pad& = p4pad& + p4speed&

IF (p1pad& - p1size&) < 60 THEN p1pad& = (60 + p1size&)
IF (p1pad& + p1size&) > 420 THEN p1pad& = (420 - p1size&)

IF (p2pad& - p2size&) < 60 THEN p2pad& = (60 + p2size&)
IF (p2pad& + p2size&) > 420 THEN p2pad& = (420 - p2size&)

IF (p3pad& - p3size&) < 60 THEN p3pad& = (60 + p3size&)
IF (p3pad& + p3size&) > 420 THEN p3pad& = (420 - p3size&)

IF (p4pad& - p4size&) < 60 THEN p4pad& = (60 + p4size&)
IF (p4pad& + p4size&) > 420 THEN p4pad& = (420 - p4size&)

FOR i = 1 TO it&
IF it1& = 0 AND it2& = 0 AND it3& = 0 AND it4& = 0 AND it5& = 0 AND it6& = 0 THEN EXIT FOR
LINE (itemx(1, i) - 2, itemy(1, i) - 2)-(itemx(1, i) + 2, itemy(1, i) + 2), colo&, B
LINE (itemx(1, i) - 1, itemy(1, i) - 1)-(itemx(1, i) + 1, itemy(1, i) + 1), colo& + 8, B
PSET (itemx(1, i), itemy(1, i)), 15
NEXT i

p1co& = 1
p2co& = 4
p3co& = 2
p4co& = 14
IF decrease1& = 1 THEN
        p1co& = 0
        a1& = a1& + 1
        IF a1& = 50 THEN decrease1& = 0
ELSEIF decrease2& = 1 THEN
        p2co& = 0
        a2& = a2& + 1
        IF a2& = 50 THEN decrease2& = 0
ELSEIF decrease3& = 1 THEN
        p3co& = 0
        a3& = a3& + 1
        IF a3& = 50 THEN decrease3& = 0
ELSEIF decrease4& = 1 THEN
        p4co& = 0
        a4& = a4& + 1
        IF a4& = 50 THEN decrease4& = 0
END IF
IF increase1& = 1 THEN
        p1co& = 9
        a1& = a1& + 1
        IF a1& = 50 THEN increase1& = 0
ELSEIF increase2& = 1 THEN
        p2co& = 12
        a2& = a2& + 1
        IF a2& = 50 THEN increase2& = 0
ELSEIF increase3& = 1 THEN
        p3co& = 10
        a3& = a3& + 1
        IF a3& = 50 THEN increase3& = 0
ELSEIF increase4& = 1 THEN
        p4co& = 15
        a4& = a4& + 1
        IF a4& = 50 THEN increase4& = 0
END IF

IF lasthit& = 1 THEN c& = 1
IF lasthit& = 2 THEN c& = 4
IF lasthit& = 3 THEN c& = 2
IF lasthit& = 4 THEN c& = 14

LINE (lx1&, ly1&)-(lx2&, ly2&), c&, B

IF p1& = 0 OR p2& = 0 THEN GOTO skip1:

LINE (2, 2)-(2, 60), 1
LINE (2, 2)-(60, 2), 1
LINE (60, 2)-(2, 60), 1
LINE (3, 3)-(3, 59), 1
LINE (3, 3)-(59, 3), 1
LINE (59, 2)-(2, 59), 1

skip1:

IF p4& = 0 OR p1& = 0 THEN GOTO skip2:

LINE (478, 2)-(478, 60), 14
LINE (478, 2)-(420, 2), 14
LINE (420, 2)-(478, 60), 14
LINE (477, 3)-(477, 59), 14
LINE (477, 3)-(421, 3), 14
LINE (421, 2)-(478, 59), 14

skip2:

IF p2& = 0 OR p3& = 0 THEN GOTO skip3:

LINE (2, 478)-(2, 420), 2
LINE (2, 478)-(60, 478), 2
LINE (60, 478)-(2, 420), 2
LINE (3, 477)-(3, 421), 2
LINE (3, 477)-(59, 477), 2
LINE (59, 478)-(2, 421), 2

skip3:

IF p3& = 0 OR p4& = 0 THEN GOTO skip4:

LINE (478, 478)-(478, 420), 14
LINE (478, 478)-(420, 478), 14
LINE (420, 478)-(478, 420), 14
LINE (477, 477)-(477, 421), 14
LINE (477, 477)-(421, 477), 14
LINE (421, 478)-(478, 421), 14

skip4:

IF p1& > 0 THEN LINE (p1pad& - (p1size& - 1), 4)-(p1pad& + (p1size& - 1), 7), p1co&, BF
IF p1& > 0 THEN LINE (p1pad& - p1size&, 3)-(p1pad& + p1size&, 8), 1, B
IF p1& > 0 THEN LINE (p1pad& - (p1size& + 1), 2)-(61, 9), 0, BF
IF p1& > 0 THEN LINE (p1pad& + (p1size& + 1), 2)-(419, 9), 0, BF
IF p2& > 0 THEN LINE (4, p2pad& - (p2size& - 1))-(7, p2pad& + (p2size& - 1)), p2co&, BF
IF p2& > 0 THEN LINE (3, p2pad& - p2size&)-(8, p2pad& + p2size&), 4, B
IF p2& > 0 THEN LINE (2, p2pad& - (p2size& + 1))-(9, 61), 0, BF
IF p2& > 0 THEN LINE (2, p2pad& + (p2size& + 1))-(9, 419), 0, BF
IF p3& > 0 THEN LINE (p3pad& - (p3size& - 1), 476)-(p3pad& + (p3size& - 1), 473), p3co&, BF
IF p3& > 0 THEN LINE (p3pad& - p3size&, 477)-(p3pad& + p3size&, 472), 2, B
IF p3& > 0 THEN LINE (p3pad& - (p3size& + 1), 478)-(61, 471), 0, BF
IF p3& > 0 THEN LINE (p3pad& + (p3size& + 1), 478)-(419, 471), 0, BF
IF p4& > 0 THEN LINE (476, p4pad& - (p4size& - 1))-(473, p4pad& + (p4size& - 1)), p4co&, BF
IF p4& > 0 THEN LINE (477, p4pad& - p4size&)-(472, p4pad& + p4size&), 14, B
IF p4& > 0 THEN LINE (478, p4pad& - (p4size& + 1))-(471, 61), 0, BF
IF p4& > 0 THEN LINE (478, p4pad& + (p4size& + 1))-(471, 419), 0, BF

a$ = INKEY$

IF p1com& = 1 THEN GOSUB player1
IF p2com& = 1 THEN GOSUB player2
IF p3com& = 1 THEN GOSUB player3
IF p4com& = 1 THEN GOSUB player4

IF p1com& = 1 OR p1& = 0 THEN GOTO p2

IF a$ = "v" THEN p1dir& = 2
IF a$ = "b" THEN p1dir& = 0
IF a$ = "n" THEN p1dir& = 1
IF a$ = "g" AND serve& = 1 THEN GOSUB serve

p2:

IF p2com& = 1 OR p2& = 0 THEN GOTO p3

IF a$ = "q" THEN p2dir& = 2
IF a$ = "a" THEN p2dir& = 0
IF a$ = "z" THEN p2dir& = 1
IF a$ = "s" AND serve& = 2 THEN GOSUB serve

p3:

IF p3com& = 1 OR p3& = 0 THEN GOTO p4

IF a$ = "1" THEN p3dir& = 2
IF a$ = "2" THEN p3dir& = 0
IF a$ = "3" THEN p3dir& = 1
IF a$ = "5" AND serve& = 3 THEN GOSUB serve

p4:

IF p4com& = 1 OR p4& = 0 THEN GOTO finish

IF a$ = "]" THEN p4dir& = 2
IF a$ = "'" THEN p4dir& = 0
IF a$ = "/" THEN p4dir& = 1
IF a$ = ";" AND serve& = 4 THEN GOSUB serve

finish:

IF a$ = "=" THEN delay& = delay& - 100
IF a$ = "-" THEN delay& = delay& + 100
IF a$ = "c" THEN CLS
IF a$ = "m" THEN
        subtract& = TIMER
        GOTO start
ELSEIF a$ = "p" THEN
        COLOR 15
        LOCATE 30, 29
        PRINT "PAUSED"
        SLEEP
        DO
        c$ = INKEY$
        LOOP UNTIL (c$ <> "~")
        CLS
END IF

COLOR 0

LOOP

quit:
CLS
LOCATE 30, 27
PRINT "Press any key to continue"
COLOR 0
END

serve:
IF p1size& <> p1desize& THEN RETURN
IF p2size& <> p2desize& THEN RETURN
IF p3size& <> p3desize& THEN RETURN
IF p4size& <> p4desize& THEN RETURN
IF serve& = 1 THEN
        IF p1dir& = 1 THEN
                xs& = 8
                ys& = 8
        ELSEIF p1dir& = 2 THEN
                xs& = -8
                ys& = 8
        END IF
        IF p1dir& = 0 THEN ys& = 8
        p1serve& = p1serve& + 1
        p2serve& = 0
        p3serve& = 0
        p4serve& = 0
ELSEIF serve& = 2 THEN
        IF p2dir& = 1 THEN
                xs& = 8
                ys& = 8
        ELSEIF p2dir& = 2 THEN
                xs& = 8
                ys& = -8
        END IF
        IF p2dir& = 0 THEN xs& = 8
        p2serve& = p2serve& + 1
        p1serve& = 0
        p3serve& = 0
        p4serve& = 0
ELSEIF serve& = 3 THEN
        IF p3dir& = 1 THEN
                xs& = 8
                ys& = -8
        ELSEIF p3dir& = 2 THEN
                xs& = -8
                ys& = -8
        END IF
        IF p3dir& = 0 THEN ys& = -8
        p3serve& = p3serve& + 1
        p1serve& = 0
        p2serve& = 0
        p4serve& = 0
ELSEIF serve& = 4 THEN
        IF p4dir& = 1 THEN
                xs& = -8
                ys& = 8
        ELSEIF p4dir& = 2 THEN
                xs& = -8
                ys& = -8
        END IF
        IF p4dir& = 0 THEN xs& = -8
        p4serve& = p4serve& + 1
        p1serve& = 0
        p2serve& = 0
        p3serve& = 0
END IF
serve& = 0
scombo& = 50
IF p1& > 1 THEN p1ac2& = INT(RND * (p1ac& * 2)) - p1ac&
IF p2& > 1 THEN p2ac2& = INT(RND * (p2ac& * 2)) - p2ac&
IF p3& > 1 THEN p3ac2& = INT(RND * (p3ac& * 2)) - p3ac&
IF p4& > 1 THEN p4ac2& = INT(RND * (p4ac& * 2)) - p4ac&

RETURN

score1:
COLOR 0
LOCATE 30, 30
PRINT combo&
scombo& = scombo& - 1
COLOR 15
IF scoret& = 1 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& + 1
        IF lasthit& = 3 THEN p3descore& = p3descore& + 1
        IF lasthit& = 4 THEN p4descore& = p4descore& + 1
        IF lasthit& = 1 THEN p1descore& = p1descore& - 1
ELSEIF scoret& = 2 THEN
        p2descore& = p2descore& + 1
        p3descore& = p3descore& + 1
        p4descore& = p4descore& + 1
ELSEIF scoret& = 3 THEN
        p1descore& = p1descore& - 1
ELSEIF scoret& = 4 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& + combo&
        IF lasthit& = 3 THEN p3descore& = p3descore& + combo&
        IF lasthit& = 4 THEN p4descore& = p4descore& + combo&
        IF lasthit& = 1 THEN p1descore& = p1descore& - combo&
ELSEIF scoret& = 5 THEN
        p2descore& = p2descore& + combo&
        p3descore& = p3descore& + combo&
        p4descore& = p4descore& + combo&
ELSEIF scoret& = 6 THEN
        p1descore& = p1descore& - combo&
END IF
p4ch& = INT(RND * 2) + 1
p3ch& = INT(RND * 2) + 1
p2ch& = INT(RND * 2) + 1
p1ch& = INT(RND * 2) + 1
IF lasthit& = 2 THEN p2scores& = p2scores& + 1
IF lasthit& = 3 THEN p3scores& = p3scores& + 1
IF lasthit& = 4 THEN p4scores& = p4scores& + 1
xs& = 0
ys& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
IF lose& = 1 THEN
        p1desize& = 25
        p1speed& = 4 + pad& * 3
END IF
RETURN

score2:
COLOR 0
LOCATE 30, 30
PRINT combo&
scombo& = scombo& - 1
COLOR 15
IF scoret& = 1 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& - 1
        IF lasthit& = 3 THEN p3descore& = p3descore& + 1
        IF lasthit& = 4 THEN p4descore& = p4descore& + 1
        IF lasthit& = 1 THEN p1descore& = p1descore& + 1
ELSEIF scoret& = 2 THEN
        p1descore& = p1descore& + 1
        p3descore& = p3descore& + 1
        p4descore& = p4descore& + 1
ELSEIF scoret& = 3 THEN
        p2descore& = p2descore& - 1
ELSEIF scoret& = 4 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& - combo&
        IF lasthit& = 3 THEN p3descore& = p3descore& + combo&
        IF lasthit& = 4 THEN p4descore& = p4descore& + combo&
        IF lasthit& = 1 THEN p1descore& = p1descore& + combo&
ELSEIF scoret& = 5 THEN
        p1descore& = p1descore& + combo&
        p3descore& = p3descore& + combo&
        p4descore& = p4descore& + combo&
ELSEIF scoret& = 6 THEN
        p2descore& = p2descore& - combo&
END IF
p4ch& = INT(RND * 2) + 1
p3ch& = INT(RND * 2) + 1
p2ch& = INT(RND * 2) + 1
p1ch& = INT(RND * 2) + 1
IF lasthit& = 1 THEN p1scores& = p1scores& + 1
IF lasthit& = 3 THEN p3scores& = p3scores& + 1
IF lasthit& = 4 THEN p4scores& = p4scores& + 1
xs& = 0
ys& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
IF lose& = 1 THEN
        p2desize& = 25
        p2speed& = 4 + pad& * 3
END IF
RETURN

score3:
COLOR 0
LOCATE 30, 30
PRINT combo&
scombo& = scombo& - 1
COLOR 15
IF scoret& = 1 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& + 1
        IF lasthit& = 3 THEN p3descore& = p3descore& - 1
        IF lasthit& = 4 THEN p4descore& = p4descore& + 1
        IF lasthit& = 1 THEN p1descore& = p1descore& + 1
ELSEIF scoret& = 2 THEN
        p1descore& = p1descore& + 1
        p2descore& = p2descore& + 1
        p4descore& = p4descore& + 1
ELSEIF scoret& = 3 THEN
        p3descore& = p3descore& - 1
ELSEIF scoret& = 4 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& + combo&
        IF lasthit& = 3 THEN p3descore& = p3descore& - combo&
        IF lasthit& = 4 THEN p4descore& = p4descore& + combo&
        IF lasthit& = 1 THEN p1descore& = p1descore& + combo&
ELSEIF scoret& = 5 THEN
        p1descore& = p1descore& + combo&
        p2descore& = p2descore& + combo&
        p4descore& = p4descore& + combo&
ELSEIF scoret& = 6 THEN
        p3descore& = p3descore& - combo&
END IF
p4ch& = INT(RND * 2) + 1
p3ch& = INT(RND * 2) + 1
p2ch& = INT(RND * 2) + 1
p1ch& = INT(RND * 2) + 1
IF lasthit& = 1 THEN p1scores& = p1scores& + 1
IF lasthit& = 2 THEN p2scores& = p2scores& + 1
IF lasthit& = 4 THEN p4scores& = p4scores& + 1
xs& = 0
ys& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
IF lose& = 1 THEN
        p3desize& = 25
        p3speed& = 4 + pad& * 3
END IF
RETURN

score4:
COLOR 0
LOCATE 30, 30
PRINT combo&
scombo& = scombo& - 1
COLOR 15
IF scoret& = 1 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& + 1
        IF lasthit& = 3 THEN p3descore& = p3descore& + 1
        IF lasthit& = 4 THEN p4descore& = p4descore& - 1
        IF lasthit& = 1 THEN p1descore& = p1descore& + 1
ELSEIF scoret& = 2 THEN
        p1descore& = p1descore& + 1
        p2descore& = p2descore& + 1
        p3descore& = p3descore& + 1
ELSEIF scoret& = 3 THEN
        p4descore& = p4descore& - 1
ELSEIF scoret& = 4 THEN
        IF lasthit& = 2 THEN p2descore& = p2descore& + combo&
        IF lasthit& = 3 THEN p3descore& = p3descore& + combo&
        IF lasthit& = 4 THEN p4descore& = p4descore& - combo&
        IF lasthit& = 1 THEN p1descore& = p1descore& + combo&
ELSEIF scoret& = 5 THEN
        p1descore& = p1descore& + combo&
        p2descore& = p2descore& + combo&
        p3descore& = p3descore& + combo&
ELSEIF scoret& = 6 THEN
        p4descore& = p4descore& - combo&
END IF
p4ch& = INT(RND * 2) + 1
p3ch& = INT(RND * 2) + 1
p2ch& = INT(RND * 2) + 1
p1ch& = INT(RND * 2) + 1
IF lasthit& = 1 THEN p1scores& = p1scores& + 1
IF lasthit& = 2 THEN p2scores& = p2scores& + 1
IF lasthit& = 3 THEN p3scores& = p3scores& + 1
xs& = 0
ys& = 0
combo& = 1
scombo& = 0
serve& = lasthit&
waits& = 100
IF lose& = 1 THEN
        p4desize& = 25
        p4speed& = 4 + pad& * 3
END IF
RETURN

item:
startitem:
item& = INT(RND * 8) + 1
IF item& = 1 THEN
        IF it2& = 0 THEN GOTO startitem
        IF lasthit& = 1 THEN p1speed& = p1speed& - 1
        IF p1speed& < 1 THEN p1speed& = 1
        IF lasthit& = 2 THEN p2speed& = p2speed& - 1
        IF p2speed& < 1 THEN p2speed& = 1
        IF lasthit& = 3 THEN p3speed& = p3speed& - 1
        IF p3speed& < 1 THEN p3speed& = 1
        IF lasthit& = 4 THEN p4speed& = p4speed& - 1
        IF p4speed& < 1 THEN p4speed& = 1
        IF lasthit& = 1 THEN
                decrease1& = 1
                increase1& = 0
                a1& = 0
        ELSEIF lasthit& = 2 THEN
                decrease2& = 1
                increase2& = 0
                a2& = 0
        ELSEIF lasthit& = 3 THEN
                decrease3& = 1
                increase3& = 0
                a3& = 0
        ELSEIF lasthit& = 4 THEN
                decrease4& = 1
                increase4& = 0
                a4& = 0
        END IF
ELSEIF item& = 2 THEN
        IF it2& = 0 THEN GOTO startitem
        IF lasthit& = 1 THEN p1speed& = p1speed& + 1
        IF p1speed& > 10 THEN p1speed& = 10
        IF lasthit& = 2 THEN p2speed& = p2speed& + 1
        IF p2speed& > 10 THEN p2speed& = 10
        IF lasthit& = 3 THEN p3speed& = p3speed& + 1
        IF p3speed& > 10 THEN p3speed& = 10
        IF lasthit& = 4 THEN p4speed& = p4speed& + 1
        IF p4speed& > 10 THEN p4speed& = 10
        IF lasthit& = 1 THEN
                increase1& = 1
                decrease1& = 0
                a1& = 0
        ELSEIF lasthit& = 2 THEN
                increase2& = 1
                decrease2& = 0
                a2& = 0
        ELSEIF lasthit& = 3 THEN
                increase3& = 1
                decrease3& = 0
                a3& = 0
        ELSEIF lasthit& = 4 THEN
                increase4& = 1
                decrease4& = 0
                a4& = 0
        END IF
ELSEIF item& = 3 THEN
        IF it1& = 0 THEN GOTO startitem
        a& = a& - 1
        IF a& < 1 THEN a& = 1
ELSEIF item& = 4 THEN
        IF it1& = 0 THEN GOTO startitem
        a& = a& + 1
        IF a& > 4 THEN a& = 4
ELSEIF item& = 5 THEN
        IF it4& = 0 THEN GOTO startitem
rann:
        DO
        xs& = INT(RND * 17) - 8
        ys& = INT(RND * 17) - 8
        LOOP UNTIL (xs& <> 0 OR ys& <> 0)
        IF xs& = 0 AND p1& = 0 AND p3& = 0 THEN GOTO rann
        IF ys& = 0 AND p2& = 0 AND p4& = 0 THEN GOTO rann
ELSEIF item& = 6 THEN
        IF it3& = 0 THEN GOTO startitem
        rand& = INT(RND * 18) + 1
        IF rand& = 16 THEN padsize& = 100
        IF rand& = 15 THEN padsize& = 95
        IF rand& = 14 THEN padsize& = 90
        IF rand& = 13 THEN padsize& = 85
        IF rand& = 12 THEN padsize& = 80
        IF rand& = 11 THEN padsize& = 75
        IF rand& = 10 THEN padsize& = 70
        IF rand& = 9 THEN padsize& = 65
        IF rand& = 8 THEN padsize& = 60
        IF rand& = 7 THEN padsize& = 55
        IF rand& = 6 THEN padsize& = 50
        IF rand& = 5 THEN padsize& = 45
        IF rand& = 4 THEN padsize& = 40
        IF rand& = 3 THEN padsize& = 35
        IF rand& = 2 THEN padsize& = 30
        IF rand& = 1 THEN padsize& = 25
        IF rand& = 17 THEN padsize& = 20
        IF rand& = 18 THEN padsize& = 15
        IF lasthit& = 1 THEN p1desize& = padsize&
        IF lasthit& = 2 THEN p2desize& = padsize&
        IF lasthit& = 3 THEN p3desize& = padsize&
        IF lasthit& = 4 THEN p4desize& = padsize&
ELSEIF item& = 7 THEN
        IF it5& = 0 THEN GOTO startitem
        IF lasthit& = 1 THEN p1descore& = p1descore& + 5
        IF lasthit& = 2 THEN p2descore& = p2descore& + 5
        IF lasthit& = 3 THEN p3descore& = p3descore& + 5
        IF lasthit& = 4 THEN p4descore& = p4descore& + 5
ELSEIF item& = 8 THEN
        IF it6& = 0 THEN GOTO startitem
        IF combo& < 50000 THEN combo& = combo& * 2
        scombo& = 50
        LINE (0, 0)-(640, 480), 15, BF
        CLS
END IF
iitemx& = itemx(1, hit&)
iitemy& = itemy(1, hit&)
IF lasthit& = 1 THEN p1items& = p1items& + 1
IF lasthit& = 2 THEN p2items& = p2items& + 1
IF lasthit& = 3 THEN p3items& = p3items& + 1
IF lasthit& = 4 THEN p4items& = p4items& + 1
LINE (iitemx& - 2, iitemy& - 2)-(iitemx& + 2, iitemy& + 2), 0, BF
IF re& = 1 THEN RETURN
iitemx& = INT(RND * 340) + 70
iitemy& = INT(RND * 340) + 70
itemx(1, hit&) = iitemx&
itemy(1, hit&) = iitemy&
RETURN

player1:
IF y& < p1ai& AND ys& < 0 THEN
        p1des& = x& - xs& * ((10 + y&) / ys&)
        p1des& = p1des& + p1ac2&
ELSE
        IF serve& = 0 AND p1& > 3 THEN p1des& = 240 ELSE p1des& = p1pad&
END IF
IF serve& = 2 AND p1& > 4 THEN
        fx& = x&
        fy& = y&
        fx& = fx& + fy&
        p1des& = fx&
ELSEIF serve& = 3 AND p1& > 4 THEN
        p1des& = x&
ELSEIF serve& = 4 AND p1& > 4 THEN
        fx& = x&
        fy& = y&
        fx& = fx& - fy&
        p1des& = fx&
END IF
IF serve& = 1 THEN
        IF p1ch& = 1 THEN
                IF p2& > 0 THEN
                        IF p2descore& < p4descore& AND p2descore& < p3descore& THEN p1des& = 0
                END IF
                IF p3& > 0 THEN
                        IF p3descore& < p4descore& AND p3descore& < p2descore& THEN p1des& = p1pad&
                END IF
                IF p4& > 0 THEN
                        IF p4descore& < p3descore& AND p4descore& < p2descore& THEN p1des& = 480
                END IF
        ELSEIF p1ch& = 2 THEN
                IF p2& > 0 THEN
                        IF p2desize& < p4desize& AND p2desize& < p3desize& THEN p1des& = 0
                END IF
                IF p3& > 0 THEN
                        IF p3desize& < p4desize& AND p3desize& < p2desize& THEN p1des& = p1pad&
                END IF
                IF p4& > 0 THEN
                        IF p4desize& < p3desize& AND p4desize& < p2desize& THEN p1des& = 480
                END IF
        END IF
END IF
IF serve& = 5 THEN p1des& = 240
IF p1des& > p1pad& THEN p1dir& = 1
IF p1des& < p1pad& THEN p1dir& = 2
IF p1des& > (p1pad& - p1speed&) AND p1des& < (p1pad& + p1speed&) THEN p1dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 1 AND waits& = 0 THEN GOSUB serve

RETURN

player2:
IF x& < p2ai& AND xs& < 0 THEN
        p2des& = y& - ys& * ((10 + x&) / xs&)
        p2des& = p2des& + p2ac2&
ELSE
        IF serve& = 0 AND p2& > 3 THEN p2des& = 240 ELSE p2des& = p2pad&
END IF
IF serve& = 1 AND p2& > 4 THEN
        fx& = x&
        fy& = y&
        fy& = fy& + fx&
        p2des& = fy&
ELSEIF serve& = 4 AND p2& > 4 THEN
        p2des& = y&
ELSEIF serve& = 3 AND p2& > 4 THEN
        fx& = x&
        fy& = y&
        fy& = fy& - fx&
        p2des& = fy&
END IF
IF serve& = 2 THEN
        IF p2ch& = 1 THEN
                IF p1& > 0 THEN
                        IF p1descore& < p4descore& AND p1descore& < p3descore& THEN p2des& = 0
                END IF
                IF p3& > 0 THEN
                        IF p3descore& < p4descore& AND p3descore& < p1descore& THEN p2des& = p2pad&
                END IF
                IF p4& > 0 THEN
                        IF p4descore& < p3descore& AND p4descore& < p1descore& THEN p2des& = 480
                END IF
        ELSEIF p2ch& = 2 THEN
                IF p1& > 0 THEN
                        IF p1desize& < p4desize& AND p1desize& < p3desize& THEN p2des& = 0
                END IF
                IF p3& > 0 THEN
                        IF p3desize& < p4desize& AND p3desize& < p1desize& THEN p2des& = p2pad&
                END IF
                IF p4& > 0 THEN
                        IF p4desize& < p3desize& AND p4desize& < p1desize& THEN p2des& = 480
                END IF
        END IF
END IF
IF serve& = 5 THEN p2des& = 240
IF p2des& > p2pad& THEN p2dir& = 1
IF p2des& < p2pad& THEN p2dir& = 2
IF p2des& > (p2pad& - p2speed&) AND p2des& < (p2pad& + p2speed&) THEN p2dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 2 AND waits& = 0 THEN GOSUB serve

RETURN

player3:
IF y& > (480 - p3ai&) AND ys& > 0 THEN
        p3des& = x& - xs& * ((470 - y&) / -ys&)
        p3des& = p3des& + p3ac2&
ELSE
        IF serve& = 0 AND p3& > 3 THEN p3des& = 240 ELSE p3des& = p3pad&
END IF
IF serve& = 2 AND p3& > 4 THEN
        fx& = x&
        fy& = 480 - y&
        fx& = fx& + fy&
        p3des& = fx&
ELSEIF serve& = 1 AND p3& > 4 THEN
        p3des& = x&
ELSEIF serve& = 4 AND p3& > 4 THEN
        fx& = x&
        fy& = 480 - y&
        fx& = fx& - fy&
        p3des& = fx&
END IF
IF serve& = 3 THEN
        IF p3ch& = 1 THEN
                IF p1& > 0 THEN
                        IF p1descore& < p4descore& AND p1descore& < p2descore& THEN p3des& = 0
                END IF
                IF p2& > 0 THEN
                        IF p2descore& < p4descore& AND p2descore& < p1descore& THEN p3des& = p3pad&
                END IF
                IF p4& > 0 THEN
                        IF p4descore& < p2descore& AND p4descore& < p1descore& THEN p3des& = 480
                END IF
        ELSEIF p3ch& = 2 THEN
                IF p1& > 0 THEN
                        IF p1desize& < p4desize& AND p1desize& < p2desize& THEN p3des& = 0
                END IF
                IF p2& > 0 THEN
                        IF p2desize& < p4desize& AND p2desize& < p1desize& THEN p3des& = p3pad&
                END IF
                IF p4& > 0 THEN
                        IF p4desize& < p2desize& AND p4desize& < p1desize& THEN p3des& = 480
                END IF
        END IF
END IF
IF serve& = 5 THEN p3des& = 240
IF p3des& > p3pad& THEN p3dir& = 1
IF p3des& < p3pad& THEN p3dir& = 2
IF p3des& > (p3pad& - p3speed&) AND p3des& < (p3pad& + p3speed&) THEN p3dir& = 0
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 3 AND waits& = 0 THEN GOSUB serve
RETURN

player4:
IF x& > (480 - p4ai&) AND xs& > 0 THEN
        p4des& = y& - ys& * ((470 - x&) / -xs&)
        p4des& = p4des& + p4ac2&
ELSE
        IF serve& = 0 AND p4& > 3 THEN p4des& = 240 ELSE p4des& = p4pad&
END IF
IF serve& = 1 AND p4& > 4 THEN
        fx& = 480 - x&
        fy& = y&
        fy& = fy& + fx&
        p4des& = fy&
ELSEIF serve& = 2 AND p4& > 4 THEN
        p4des& = y&
ELSEIF serve& = 3 AND p4& > 4 THEN
        fx& = 480 - x&
        fy& = y&
        fy& = fy& - fx&
        p4des& = fy&
END IF
IF serve& = 4 THEN
        IF p4ch& = 1 THEN
                IF p1& > 0 THEN
                        IF p1descore& < p3descore& AND p1descore& < p2descore& THEN p4des& = 0
                END IF
                IF p2& > 0 THEN
                        IF p2descore& < p3descore& AND p2descore& < p1descore& THEN p4des& = p4pad&
                END IF
                IF p3& > 0 THEN
                        IF p3descore& < p2descore& AND p3descore& < p1descore& THEN p4des& = 480
                END IF
        ELSEIF p4ch& = 2 THEN
                IF p1& > 0 THEN
                        IF p1desize& < p3desize& AND p1desize& < p2desize& THEN p4des& = 0
                END IF
                IF p2& > 0 THEN
                        IF p2desize& < p3desize& AND p2desize& < p1desize& THEN p4des& = p4pad&
                END IF
                IF p3& > 0 THEN
                        IF p3desize& < p2desize& AND p3desize& < p1desize& THEN p4des& = 480
                END IF
        END IF
END IF
IF serve& = 5 THEN p4des& = 240
IF p4des& > p4pad& THEN p4dir& = 1
IF p4des& < p4pad& THEN p4dir& = 2
IF p4des& > (p4pad& - p4speed&) AND p4des& < (p4pad& + p4speed&) THEN p4dir& = 0
IF serve& = 4 THEN wwait& = wwait& + 1
IF waits& > 0 THEN waits& = waits& - 1
IF serve& = 4 AND waits& = 0 THEN GOSUB serve
RETURN

switch:
COLOR 5
a$ = INKEY$
IF a$ = "2" THEN
        arrow& = arrow& + 1
ELSEIF a$ = "8" THEN
        arrow& = arrow& - 1
ELSEIF a$ = " " THEN
        IF arrow& = 1 THEN it1& = it1& + 1
        IF arrow& = 2 THEN it2& = it2& + 1
        IF arrow& = 3 THEN it3& = it3& + 1
        IF arrow& = 4 THEN it4& = it4& + 1
        IF arrow& = 5 THEN it5& = it5& + 1
        IF arrow& = 6 THEN it6& = it6& + 1
END IF
IF it1& = 2 THEN it1& = 0
IF it2& = 2 THEN it2& = 0
IF it3& = 2 THEN it3& = 0
IF it4& = 2 THEN it4& = 0
IF it5& = 2 THEN it5& = 0
IF it6& = 2 THEN it6& = 0
IF arrow& = 7 THEN arrow& = 1
COLOR 4
IF arrow& = 1 THEN COLOR 12
LOCATE 17, 42
PRINT "Change Ball Speed -"
COLOR 4
IF arrow& = 2 THEN COLOR 12
LOCATE 18, 42
PRINT "Change Pad Speed -"
COLOR 4
IF arrow& = 3 THEN COLOR 12
LOCATE 19, 42
PRINT "Change Pad Size -"
COLOR 4
IF arrow& = 4 THEN COLOR 12
LOCATE 20, 42
PRINT "Change Ball Direction -"
COLOR 4
IF arrow& = 5 THEN COLOR 12
LOCATE 21, 42
PRINT "5 Point Bonus -"
COLOR 4
IF arrow& = 6 THEN COLOR 12
LOCATE 22, 42
PRINT "Combo Double -"
COLOR 4
IF it1& = 1 THEN COLOR 12
LOCATE 17, 66
PRINT "On "
COLOR 4
IF it1& = 0 THEN COLOR 12
LOCATE 17, 69
PRINT "Off"
COLOR 4
IF it2& = 1 THEN COLOR 12
LOCATE 18, 66
PRINT "On "
COLOR 4
IF it2& = 0 THEN COLOR 12
LOCATE 18, 69
PRINT "Off"
COLOR 4
IF it3& = 1 THEN COLOR 12
LOCATE 19, 66
PRINT "On "
COLOR 4
IF it3& = 0 THEN COLOR 12
LOCATE 19, 69
PRINT "Off"
COLOR 4
IF it4& = 1 THEN COLOR 12
LOCATE 20, 66
PRINT "On "
COLOR 4
IF it4& = 0 THEN COLOR 12
LOCATE 20, 69
PRINT "Off"
COLOR 4
IF it5& = 1 THEN COLOR 12
LOCATE 21, 66
PRINT "On "
COLOR 4
IF it5& = 0 THEN COLOR 12
LOCATE 21, 69
PRINT "Off"
COLOR 4
IF it6& = 1 THEN COLOR 12
LOCATE 22, 66
PRINT "On "
COLOR 4
IF it6& = 0 THEN COLOR 12
LOCATE 22, 69
PRINT "Off"
COLOR 4
IF a$ = "d" THEN
        CLS
        switch& = 0
END IF
RETURN

theend:
VIEW
started& = 0
CLS
IF scoret& = 1 OR scoret& = 4 THEN
        IF p1descore& > p2descore& AND p1descore& > p3descore& AND p1descore& > p4descore& THEN p1win& = 1 ELSE p1win& = 0
        IF p2descore& > p1descore& AND p2descore& > p3descore& AND p2descore& > p4descore& THEN p2win& = 1 ELSE p2win& = 0
        IF p3descore& > p2descore& AND p3descore& > p1descore& AND p3descore& > p4descore& THEN p3win& = 1 ELSE p3win& = 0
        IF p4descore& > p2descore& AND p4descore& > p3descore& AND p4descore& > p1descore& THEN p4win& = 1 ELSE p4win& = 0
ELSEIF scoret& = 3 OR scoret& = 6 THEN
        IF p1descore& < 1 THEN p1win& = 1
        IF p2descore& < 1 THEN p2win& = 1
        IF p3descore& < 1 THEN p3win& = 1
        IF p4descore& < 1 THEN p4win& = 1
END IF
IF p1win& = 1 THEN co1& = 1
IF p2win& = 1 THEN co1& = 4
IF p3win& = 1 THEN co1& = 2
IF p4win& = 1 THEN co1& = 14

IF p1win& = 1 THEN co2& = 9
IF p2win& = 1 THEN co2& = 12
IF p3win& = 1 THEN co2& = 10
IF p4win& = 1 THEN co2& = 6

co& = co1&

ch& = 0
FOR i = 1 TO 240
ch& = ch& + 1
IF ch& = 10 AND co& = co1& THEN
        co& = co2&
ELSEIF ch& = 10 AND co& = co2& THEN
        co& = co1&
END IF
IF ch& = 10 THEN ch& = 0
LINE (320 - i, 240 - i)-(320 + i, 240 + i), co&, B
NEXT i

FOR i = 1 TO 100
FOR d = 1 TO 500
NEXT d
LINE (320 - i, 240)-(320 + i, 240), 15
LINE (319 - i, 239)-(321 + i, 241), 7, B
NEXT i

FOR i = 1 TO 100
FOR d = 1 TO 200
NEXT d
LINE (220, 240 - i)-(420, 240 + i), 15, B
LINE (221, 241 - i)-(419, 239 + i), 7, B
LINE (219, 239 - i)-(421, 241 + i), 7, B
LINE (222, 242 - i)-(418, 238 + i), 0, BF
NEXT i

COLOR 3
LOCATE 20, 36
PRINT "Game Stats"
LINE (279, 160)-(359, 160), 3
IF scoret& = 1 OR scoret& = 4 THEN
        COLOR co1&
        LOCATE 22, 30
        IF p1win& = 1 THEN PRINT "Player 1 is the winner!"
        IF p2win& = 1 THEN PRINT "Player 2 is the winner!"
        IF p3win& = 1 THEN PRINT "Player 3 is the winner!"
        IF p4win& = 1 THEN PRINT "Player 4 is the winner!"
ELSEIF scoret& = 3 OR scoret& = 6 THEN
        COLOR co1&
        LOCATE 22, 30
        IF p1win& = 1 THEN PRINT "Player 1 is the Loser!"
        IF p2win& = 1 THEN PRINT "Player 2 is the Loser!"
        IF p3win& = 1 THEN PRINT "Player 3 is the Loser!"
        IF p4win& = 1 THEN PRINT "Player 4 is the Loser!"
END IF
IF p1& > 1 THEN p1t$ = "Com  "
IF p2& > 1 THEN p2t$ = "Com  "
IF p3& > 1 THEN p3t$ = "Com  "
IF p4& > 1 THEN p4t$ = "Com  "
IF p1& = 1 THEN p1t$ = "Human"
IF p2& = 1 THEN p2t$ = "Human"
IF p3& = 1 THEN p3t$ = "Human"
IF p4& = 1 THEN p4t$ = "Human"
IF p1& = 0 THEN p1t$ = "Off  "
IF p2& = 0 THEN p2t$ = "Off  "
IF p3& = 0 THEN p3t$ = "Off  "
IF p4& = 0 THEN p4t$ = "Off  "

COLOR 3
LOCATE 24, 30
PRINT "Player   Score  Goals"
COLOR 1
LOCATE 25, 31
PRINT "P1-"; p1t$
COLOR 4
LOCATE 26, 31
PRINT "P2-"; p2t$
COLOR 2
LOCATE 27, 31
PRINT "P3-"; p3t$
COLOR 14
LOCATE 28, 31
PRINT "P4-"; p4t$

COLOR 1
LOCATE 25, 39
PRINT p1descore&
COLOR 4
LOCATE 26, 39
PRINT p2descore&
COLOR 2
LOCATE 27, 39
PRINT p3descore&
COLOR 14
LOCATE 28, 39
PRINT p4descore&

COLOR 1
LOCATE 25, 46
PRINT p1scores&
COLOR 4
LOCATE 26, 46
PRINT p2scores&
COLOR 2
LOCATE 27, 46
PRINT p3scores&
COLOR 14
LOCATE 28, 46
PRINT p4scores&

gts& = finished& - startedd&
DO
IF gts& > 60 THEN
        gts& = gts& - 60
        gtm& = gtm& + 1
ELSE
        EXIT DO
END IF
LOOP
DO
IF gtm& > 60 THEN
        gtm& = gtm& - 60
        gth& = gth& + 1
ELSE
        EXIT DO
END IF
LOOP

COLOR 3
LOCATE 30, 30
PRINT "Game time:"
LOCATE 31, 30
PRINT gts&; "sec"
LOCATE 31, 30
IF gtm& > 0 THEN PRINT gtm&; "min"; gts&; "sec"
LOCATE 31, 30
IF gth& > 0 THEN PRINT gth&; "hr"; gtm&; "min"; gts&; "sec"
 
LOCATE 33, 31
PRINT "ontinue"
COLOR 11
LOCATE 33, 30
PRINT "C"
COLOR 15

DO WHILE a$ <> "c"
a$ = INKEY$
SWAP co1&, co2&
FOR i = 102 TO 240
ch& = ch& + 1
IF ch& = 10 AND co& = co1& THEN
        co& = co2&
ELSEIF ch& = 10 AND co& = co2& THEN
        co& = co1&
END IF
IF ch& = 10 THEN ch& = 0
LINE (320 - i, 240 - i)-(320 + i, 240 + i), co&, B
NEXT i
LOOP

gts& = 0
gtm& = 0
gth& = 0

GOTO start

start:
IF p1r& = 1 THEN p1& = 7
IF p2r& = 1 THEN p2& = 7
IF p3r& = 1 THEN p3& = 7
IF p4r& = 1 THEN p4& = 7
VIEW
CLS
DO
COLOR 14
LOCATE 1, 15
PRINT "Game Setup"
LINE (111, 8)-(191, 8)
COLOR 2
LOCATE 3, 15
PRINT "Players ("
LOCATE 3, 31
PRINT ")"
COLOR 10
LOCATE 3, 24
PRINT "1,2,3,4"
COLOR 1
LOCATE 4, 16
PRINT "P1 -"
COLOR 4
LOCATE 5, 16
PRINT "P2 -"
COLOR 2
LOCATE 6, 16
PRINT "P3 -"
COLOR 14
LOCATE 7, 16
PRINT "P4 -"
COLOR 1
LOCATE 4, 21
IF p1& = 0 THEN PRINT "Off               "
IF p1& = 1 THEN PRINT "Human             "
IF p1& = 2 THEN PRINT "Computer (Easiest)"
IF p1& = 3 THEN PRINT "Computer (Easy)   "
IF p1& = 4 THEN PRINT "Computer (Medium) "
IF p1& = 5 THEN PRINT "Computer (Hard)   "
IF p1& = 6 THEN PRINT "Computer (Hardest)"
IF p1& = 7 THEN PRINT "Computer (Random) "
COLOR 4
LOCATE 5, 21
IF p2& = 0 THEN PRINT "Off               "
IF p2& = 1 THEN PRINT "Human             "
IF p2& = 2 THEN PRINT "Computer (Easiest)"
IF p2& = 3 THEN PRINT "Computer (Easy)   "
IF p2& = 4 THEN PRINT "Computer (Medium) "
IF p2& = 5 THEN PRINT "Computer (Hard)   "
IF p2& = 6 THEN PRINT "Computer (Hardest)"
IF p2& = 7 THEN PRINT "Computer (Random) "
COLOR 2
LOCATE 6, 21
IF p3& = 0 THEN PRINT "Off               "
IF p3& = 1 THEN PRINT "Human             "
IF p3& = 2 THEN PRINT "Computer (Easiest)"
IF p3& = 3 THEN PRINT "Computer (Easy)   "
IF p3& = 4 THEN PRINT "Computer (Medium) "
IF p3& = 5 THEN PRINT "Computer (Hard)   "
IF p3& = 6 THEN PRINT "Computer (Hardest)"
IF p3& = 7 THEN PRINT "Computer (Random) "
COLOR 14
LOCATE 7, 21
IF p4& = 0 THEN PRINT "Off               "
IF p4& = 1 THEN PRINT "Human             "
IF p4& = 2 THEN PRINT "Computer (Easiest)"
IF p4& = 3 THEN PRINT "Computer (Easy)   "
IF p4& = 4 THEN PRINT "Computer (Medium) "
IF p4& = 5 THEN PRINT "Computer (Hard)   "
IF p4& = 6 THEN PRINT "Computer (Hardest)"
IF p4& = 7 THEN PRINT "Computer (Random) "
COLOR 4
IF items& = 1 THEN
        COLOR 4
        LOCATE 9, 40
        PRINT "Items Re"
        LOCATE 9, 49
        PRINT "ocate when Hit"
        COLOR 12
        LOCATE 9, 48
        PRINT "l"
        COLOR 4
        LOCATE 10, 41
        IF re& = 1 THEN PRINT "No "
        IF re& = 0 THEN PRINT "Yes"
        COLOR 4
        LOCATE 12, 40
        PRINT "Lose "
        LOCATE 12, 46
        PRINT "pgrades when Scored On"
        COLOR 12
        LOCATE 12, 45
        PRINT "U"
        COLOR 4
        LOCATE 13, 41
        IF lose& = 0 THEN PRINT "No "
        IF lose& = 1 THEN PRINT "Yes"
        LOCATE 15, 40
        PRINT "Item Switch"
        COLOR 12
        IF switch& = 1 THEN COLOR 4
        LOCATE 15, 46
        PRINT "w"
        IF switch& = 1 THEN
                COLOR 4
                LOCATE 15, 51
                PRINT "(Press  "
                LOCATE 15, 60
                PRINT "when done)"
                COLOR 12
                LOCATE 15, 58
                PRINT "D"
        END IF
END IF
COLOR 12
LOCATE 3, 40
PRINT "I"
COLOR 4
LOCATE 3, 41
PRINT "tems"
COLOR 4
LOCATE 4, 41
IF items& = 1 THEN PRINT "On "
IF items& = 0 THEN PRINT "Off"
IF items& = 1 THEN
        COLOR 12
        LOCATE 6, 40
        PRINT "N"
        COLOR 4
        LOCATE 6, 41
        PRINT "umber of Items"
        LOCATE 7, 40
        PRINT it&
END IF
COLOR 11
LOCATE 9, 15
PRINT "B"
COLOR 3
LOCATE 9, 16
PRINT "all Speed"
LOCATE 10, 16
IF a& = 1 THEN PRINT "Normal   "
IF a& = 2 THEN PRINT "Fast     "
IF a& = 3 THEN PRINT "Very Fast"
IF a& = 4 THEN PRINT "Too Fast "
COLOR 11
LOCATE 12, 15
PRINT "P"
COLOR 3
LOCATE 12, 16
PRINT "ad Speed"
LOCATE 13, 16
IF pad& = 1 THEN PRINT "Normal   "
IF pad& = 2 THEN PRINT "Fast     "
IF pad& = 3 THEN PRINT "Very Fast"
IF pad& = 4 THEN PRINT "Too Fast "
COLOR 1
LOCATE 15, 15
PRINT "Scoring"
LOCATE 15, 24
PRINT "ode"
COLOR 9
LOCATE 15, 23
PRINT "M"
COLOR 1
LOCATE 16, 16
IF scoret& = 1 THEN PRINT "Basic           "
IF scoret& = 2 THEN PRINT "All Gain        "
IF scoret& = 3 THEN PRINT "One Lose        "
IF scoret& = 4 THEN PRINT "Combo           "
IF scoret& = 5 THEN PRINT "All Gain (Combo)"
IF scoret& = 6 THEN PRINT "One Lose (Combo)"
IF scoret& = 1 OR scoret& = 4 THEN
        COLOR 1
        LOCATE 18, 15
        PRINT "Ma"
        LOCATE 18, 19
        PRINT "Score (0 = Unlimited)"
        COLOR 9
        LOCATE 18, 17
        PRINT "x"
        COLOR 1
        LOCATE 19, 16
        PRINT max&
ELSEIF scoret& = 3 OR scoret& = 6 THEN
        COLOR 1
        LOCATE 18, 15
        PRINT "Starting S"
        LOCATE 18, 26
        PRINT "ore (0 = No Game End)"
        COLOR 9
        LOCATE 18, 25
        PRINT "c"
        COLOR 1
        LOCATE 19, 16
        PRINT max&
END IF
COLOR 13
LOCATE 15, 2
PRINT "S"
COLOR 5
LOCATE 15, 3
PRINT "tart"
COLOR 13
LOCATE 17, 2
PRINT "R"
COLOR 5
LOCATE 17, 3
PRINT "eturn"
COLOR 13
LOCATE 19, 2
PRINT "Q"
COLOR 5
LOCATE 19, 3
PRINT "uit"
COLOR 15

o& = 0
IF p1& > 0 THEN o& = o& + 1
IF p2& > 0 THEN o& = o& + 1
IF p3& > 0 THEN o& = o& + 1
IF p4& > 0 THEN o& = o& + 1

IF switch& = 1 THEN GOSUB switch

IF switch& = 1 THEN GOTO skip
a$ = INKEY$
IF a$ = "r" AND started& = 1 THEN GOTO restart
IF a$ = "r" AND started& = 0 THEN GOTO new
IF a$ = "q" THEN GOTO quit
IF a$ = "s" THEN EXIT DO
IF a$ = "l" THEN
        re& = re& + 1
        IF re& = 2 THEN re& = 0
ELSEIF a$ = "u" THEN
        lose& = lose& + 1
        IF lose& = 2 THEN lose& = 0
ELSEIF a$ = "x" AND (scoret& = 1 OR scoret& = 4) THEN
        COLOR 0
        LOCATE 19, 16
        PRINT max&
        COLOR 1
        LOCATE 19, 17
        INPUT "", max&
        IF max& > 500000 THEN max& = 500000
        CLS
        COLOR 15
ELSEIF a$ = "c" AND (scoret& = 3 OR scoret& = 6) THEN
        COLOR 0
        LOCATE 16, 16
        PRINT max&
        COLOR 1
        LOCATE 18, 17
        INPUT "", max&
        IF max& > 500000 THEN max& = 500000
        CLS
        COLOR 15
ELSEIF a$ = "m" THEN
        scoret& = scoret& + 1
        IF scoret& > 6 THEN scoret& = 1
        CLS
ELSEIF a$ = "w" THEN
        arrow& = 1
        switch& = 1
ELSEIF a$ = "b" THEN
        a& = a& + 1
        CLS
ELSEIF a$ = "p" THEN
        pad& = pad& + 1
        IF pad& = 5 THEN pad& = 1
        CLS
ELSEIF a$ = "n" THEN
        it& = it& + 1
        IF it& > 15 THEN it& = 1
        CLS
ELSEIF a$ = "i" THEN
        items& = items& + 1
        CLS
ELSEIF a$ = "1" THEN
        p1& = p1& + 1
        IF p1& = 8 AND o& > 1 THEN p1& = 0
        IF p1& = 8 AND o& < 2 THEN p1& = 1
        CLS
ELSEIF a$ = "2" THEN
        p2& = p2& + 1
        IF p2& = 8 AND o& > 1 THEN p2& = 0
        IF p2& = 8 AND o& < 2 THEN p2& = 1
        CLS
ELSEIF a$ = "3" THEN
        p3& = p3& + 1
        IF p3& = 8 AND o& > 1 THEN p3& = 0
        IF p3& = 8 AND o& < 2 THEN p3& = 1
        CLS
ELSEIF a$ = "4" THEN
        p4& = p4& + 1
        IF p4& = 8 AND o& > 1 THEN p4& = 0
        IF p4& = 8 AND o& < 2 THEN p4& = 1
        CLS
END IF
skip:
IF items& = 2 THEN items& = 0
IF a& > 4 THEN a& = 1

LINE (ballx& - 1, bally& - 1)-(ballx&, bally&), 15, B
LINE (ballx& - 2, bally& - 2)-(ballx& + 1, bally& + 1), 0, B

LINE (0, 0)-(102, 102), co&, B
c1& = 1
c2& = 4
c3& = 2
c4& = 14

IF p1& = 0 THEN c1& = 7
IF p2& = 0 THEN c2& = 7
IF p3& = 0 THEN c3& = 7
IF p4& = 0 THEN c4& = 7

LINE (2, 2)-(2, 14), c1&
LINE (2, 2)-(14, 2), c1&
LINE (14, 2)-(2, 14), c1&
LINE (2, 100)-(2, 88), c2&
LINE (2, 100)-(14, 100), c2&
LINE (14, 100)-(2, 88), c2&
LINE (100, 100)-(100, 88), c3&
LINE (100, 100)-(88, 100), c3&
LINE (88, 100)-(100, 88), c3&
LINE (100, 2)-(100, 14), c4&
LINE (100, 2)-(88, 2), c4&
LINE (88, 2)-(100, 14), c4&

IF p1& > 0 THEN LINE (pad1& - 5, 2)-(pad1& + 5, 3), 1, BF
IF p2& > 0 THEN LINE (2, pad2& - 5)-(3, pad2& + 5), 4, BF
IF p3& > 0 THEN LINE (pad3& - 5, 100)-(pad3& + 5, 99), 2, BF
IF p4& > 0 THEN LINE (100, pad4& - 5)-(99, pad4& + 5), 14, BF

IF p1& > 0 THEN LINE (pad1& - 6, 2)-(15, 4), 0, BF
IF p1& > 0 THEN LINE (pad1& + 6, 2)-(87, 4), 0, BF
IF p2& > 0 THEN LINE (2, pad2& - 6)-(4, 15), 0, BF
IF p2& > 0 THEN LINE (2, pad2& + 6)-(4, 87), 0, BF
IF p3& > 0 THEN LINE (pad3& - 6, 100)-(15, 98), 0, BF
IF p3& > 0 THEN LINE (pad3& + 6, 100)-(87, 98), 0, BF
IF p4& > 0 THEN LINE (100, pad4& - 6)-(98, 15), 0, BF
IF p4& > 0 THEN LINE (100, pad4& + 6)-(98, 87), 0, BF

IF p1& = 0 THEN LINE (14, 2)-(88, 2), 7
IF p2& = 0 THEN LINE (2, 14)-(2, 88), 7
IF p3& = 0 THEN LINE (14, 100)-(88, 100), 7
IF p4& = 0 THEN LINE (100, 14)-(100, 88), 7

IF dir& = 1 THEN
        bally& = bally& - 1
ELSEIF dir& = 2 THEN
        bally& = bally& - 1
        ballx& = ballx& + 1
ELSEIF dir& = 3 THEN
        ballx& = ballx& + 1
ELSEIF dir& = 4 THEN
        bally& = bally& + 1
        ballx& = ballx& + 1
ELSEIF dir& = 5 THEN
        bally& = bally& + 1
ELSEIF dir& = 6 THEN
        bally& = bally& + 1
        ballx& = ballx& - 1
ELSEIF dir& = 7 THEN
        ballx& = ballx& - 1
ELSEIF dir& = 8 THEN
        bally& = bally& - 1
        ballx& = ballx& - 1
END IF
rand& = INT(RND * 3) - 1
IF bally& < 1 THEN
        IF p1& > 0 THEN co& = 1
        dir& = 5
        dir& = dir& + rand&
        SOUND 500, .1
ELSEIF ballx& < 1 THEN
        IF p2& > 0 THEN co& = 4
        dir& = 3
        dir& = dir& + rand&
        SOUND 500, .1
ELSEIF ballx& > 100 THEN
        IF p3& > 0 THEN co& = 14
        dir& = 7
        dir& = dir& + rand&
        SOUND 500, .1
ELSEIF bally& > 100 THEN
        IF p4& > 0 THEN co& = 2
        dir& = 1
        dir& = dir& + rand&
        SOUND 500, .1
END IF
IF dir& = 9 THEN dir& = 1
IF dir& = 0 THEN dir& = 8

IF dir& = 8 OR dir& = 1 OR dir& = 2 THEN
        IF ballx& > pad1& THEN pad1& = pad1& + 2
        IF ballx& < pad1& THEN pad1& = pad1& - 2
        IF pad1& > 82 THEN pad1& = 82
        IF pad1& < 20 THEN pad1& = 20
END IF
IF dir& = 6 OR dir& = 7 OR dir& = 8 THEN
        IF bally& > pad2& THEN pad2& = pad2& + 2
        IF bally& < pad2& THEN pad2& = pad2& - 2
        IF pad2& > 82 THEN pad2& = 82
        IF pad2& < 20 THEN pad2& = 20
END IF
IF dir& = 4 OR dir& = 5 OR dir& = 6 THEN
        IF ballx& > pad3& THEN pad3& = pad3& + 2
        IF ballx& < pad3& THEN pad3& = pad3& - 2
        IF pad3& > 82 THEN pad3& = 82
        IF pad3& < 20 THEN pad3& = 20
END IF
IF dir& = 2 OR dir& = 3 OR dir& = 4 THEN
        IF bally& > pad4& THEN pad4& = pad4& + 2
        IF bally& < pad4& THEN pad4& = pad4& - 2
        IF pad4& > 82 THEN pad4& = 82
        IF pad4& < 20 THEN pad4& = 20
END IF
LOOP
IF p1& = 7 THEN p1r& = 1 ELSE p1r& = 0
IF p2& = 7 THEN p2r& = 1 ELSE p2r& = 0
IF p3& = 7 THEN p3r& = 1 ELSE p3r& = 0
IF p4& = 7 THEN p4r& = 1 ELSE p4r& = 0
IF started& = 1 THEN finished& = finished& - subtract&
GOTO new

