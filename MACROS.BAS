SCREEN 12
COLOR 7
CLS
begin:
PRINT "Welcome User!"
PRINT ""
RANDOMIZE TIMER
a& = INT(RND * 5) + 1
IF a& = 1 THEN PRINT "How farest thou today?"
IF a& = 2 THEN PRINT "Are you having a pleasent day?"
IF a& = 3 THEN PRINT "Isn't it a lovely day out?"
IF a& = 4 THEN PRINT "Is the wind blowing strong?"
IF a& = 5 THEN PRINT "The birds are tall today."
INPUT "", a$
start:
PRINT ""
PRINT "What do you want to do?"
PRINT "1. Check time on your Macrosift Clock Program"
PRINT "2. Ask me a Yes or No question"
PRINT "3. Run a program"
PRINT "4. Test Speakers"
PRINT "5. Have a Conversation"
PRINT "6. Shut down Macrosift Wondows 96"
PRINT ""
INPUT "", a$
PRINT ""
IF a$ = "1" THEN
        PRINT "The current time is "; TIME$
        PRINT "The current date is "; DATE$
END IF
IF a$ = "2" THEN GOTO question
IF a$ = "3" THEN GOTO program
IF a$ = "4" THEN GOTO test
IF a$ = "5" THEN GOTO conservation
IF a$ = "6" THEN GOTO shutdown
GOTO start

test:
PRINT ""
PRINT "Testing"
SLEEP 1
SOUND 40, 1
SLEEP 1
SOUND 500, 1
SLEEP 1
SOUND 1000, 1
SLEEP 1
SOUND 5000, 1
SLEEP 1
SOUND 10000, 1
SLEEP 1
SOUND 30000, 1
SLEEP 1
PRINT ""
PRINT "Calculating"

FOR i = 12 TO 45
RANDOMIZE TIMER
delay& = INT(RND * 4000) + 4000
FOR ii = 1 TO delay&
NEXT ii
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
NEXT i
RANDOMIZE TIMER
p& = INT(RND * 100) + 1
PRINT "Speakers have been tested and are running at "; p&; "% capacity"
PRINT ""
GOTO start

question:
PRINT "What do you want to ask me?"
INPUT "", a$
PRINT ""
PRINT "Calculating"

FOR i = 12 TO 60
RANDOMIZE TIMER
delay& = INT(RND * 8000) + 8000
FOR ii = 1 TO delay&
NEXT ii
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
NEXT i
RANDOMIZE TIMER
c& = INT(RND * 2) + 1
IF c& = 1 THEN PRINT "Yes"
IF c& = 2 THEN PRINT "No"
PRINT ""
GOTO start

program:
PRINT ""
PRINT "Which one?"
PRINT "1. Macrosift Word"
PRINT "2. Bill's flight Simulator"
PRINT "3. Macrosift calulator"
PRINT ""
INPUT "", a$
IF a$ = "1" THEN GOTO macrosiftword
IF a$ = "2" THEN GOTO flight
IF a$ = "3" THEN GOTO cal
GOTO start

cal:
PRINT ""
PRINT "We suggest that you buy the Macrosift Arithmatic Caculation System for increased accuracy."
PRINT "What is the first number?"
INPUT "", n1&
PRINT "What is the operation (+ - * /, q to quit)?"
INPUT "", o$
PRINT "What is the second number?"
INPUT "", n2&
RANDOMIZE TIMER
ra& = INT(RND * 10) - 5
IF o$ = "+" THEN
        n3& = (n1& + n2&) + ra&
ELSEIF o$ = "-" THEN
        n3& = (n1& - n2&) + ra&
ELSEIF o$ = "*" THEN
        n3& = (n1& * n2&) + ra&
ELSEIF o$ = "/" THEN
        n3& = (n1& / n2&) + ra&
ELSEIF o$ = "q" THEN
        GOTO start
ELSE
        GOTO problem
PRINT ""
END IF
PRINT "Calculating"

FOR i = 12 TO 51
RANDOMIZE TIMER
delay& = INT(RND * 1000) + 16000
FOR ii = 1 TO delay&
NEXT ii
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
NEXT i

PRINT n1&; o$; n2&; "= "; n3&
PRINT "Do you wish to make another calculation?"
INPUT "", a$
IF a$ = "y" OR a$ = "yes" THEN GOTO cal
IF a$ = "n" THEN GOTO start ELSE GOTO problem
GOTO start


macrosiftword:
PRINT ""
PRINT "Welcome to Macrosift Word"
PRINT "Use commas with caution"
PRINT "Press enter when done"
PRINT ""
INPUT "", b$
PRINT "Print or quit?"
INPUT "", a$
IF a$ = "p" THEN LPRINT b$
IF a$ = "q" THEN GOTO start ELSE GOTO problem
PRINT ""
GOTO start

flight:
CLS

dir& = 1

x& = 320
y& = 240

gx1& = 0
gx2& = 120
gx3& = 240
gx4& = 360

gy1& = 0
gy2& = 160
gy3& = 320
gy4& = 480

DO
LINE (x& - 1, y& - 1)-(x& + 1, y& + 1), 1, BF

LINE (0, 0)-(640, 480), 0, B

LINE (0, gx1& + 1)-(640, gx1& + 1), 0
LINE (0, gx1&)-(640, gx1&), 4
LINE (0, gx1& - 1)-(640, gx1& - 1), 0

LINE (0, gx2& + 1)-(640, gx2& + 1), 0
LINE (0, gx2&)-(640, gx2&), 4
LINE (0, gx2& - 1)-(640, gx2& - 1), 0

LINE (0, gx3& + 1)-(640, gx3& + 1), 0
LINE (0, gx3&)-(640, gx3&), 4
LINE (0, gx3& - 1)-(640, gx3& - 1), 0

LINE (0, gx4& + 1)-(640, gx4& + 1), 0
LINE (0, gx4&)-(640, gx4&), 4
LINE (0, gx4& - 1)-(640, gx4& - 1), 0


LINE (gy1& + 1, 0)-(gy1& + 1, 480), 0
LINE (gy1&, 0)-(gy1&, 480), 4
LINE (gy1& - 1, 0)-(gy1& - 1, 480), 0

LINE (gy2& + 1, 0)-(gy2& + 1, 480), 0
LINE (gy2&, 0)-(gy2&, 480), 4
LINE (gy2& - 1, 0)-(gy2& - 1, 480), 0

LINE (gy3& + 1, 0)-(gy3& + 1, 480), 0
LINE (gy3&, 0)-(gy3&, 480), 4
LINE (gy3& - 1, 0)-(gy3& - 1, 480), 0

LINE (gy4& + 1, 0)-(gy4& + 1, 480), 0
LINE (gy4&, 0)-(gy4&, 480), 4
LINE (gy4& - 1, 0)-(gy4& - 1, 480), 0

IF dir& = 1 THEN
        gx1& = gx1& + 1
        gx2& = gx2& + 1
        gx3& = gx3& + 1
        gx4& = gx4& + 1
ELSEIF dir& = 2 THEN
        gy1& = gy1& + 1
        gy2& = gy2& + 1
        gy3& = gy3& + 1
        gy4& = gy4& + 1
ELSEIF dir& = 3 THEN
        gx1& = gx1& - 1
        gx2& = gx2& - 1
        gx3& = gx3& - 1
        gx4& = gx4& - 1
ELSEIF dir& = 4 THEN
        gy1& = gy1& - 1
        gy2& = gy2& - 1
        gy3& = gy3& - 1
        gy4& = gy4& - 1
END IF
IF gx1& > 480 THEN gx1& = 0
IF gx1& < 0 THEN gx1& = 480
IF gx2& > 480 THEN gx2& = 0
IF gx2& < 0 THEN gx2& = 480
IF gx3& > 480 THEN gx3& = 0
IF gx3& < 0 THEN gx3& = 480
IF gx4& > 480 THEN gx4& = 0
IF gx4& < 0 THEN gx4& = 480

IF gy1& > 640 THEN gy1& = 0
IF gy1& < 0 THEN gy1& = 640
IF gy2& > 640 THEN gy2& = 0
IF gy2& < 0 THEN gy2& = 640
IF gy3& > 640 THEN gy3& = 0
IF gy3& < 0 THEN gy3& = 640
IF gy4& > 640 THEN gy4& = 0
IF gy4& < 0 THEN gy4& = 640

RANDOMIZE TIMER
a& = INT(RND * 1000) + 1
IF a& = 1 THEN
        FOR i = 1 TO 20
        CIRCLE (x&, y&), i, 4
        NEXT i
        SLEEP 1
        CLS
        PRINT "You crashed!!"
        PRINT "You must be really bad."
        GOTO start
END IF

a$ = INKEY$
IF a$ = "8" THEN dir& = 1
IF a$ = "4" THEN dir& = 2
IF a$ = "2" THEN dir& = 3
IF a$ = "6" THEN dir& = 4
IF a$ = "q" THEN EXIT DO
LOOP
CLS
GOTO start

conservation:
PRINT "Remember, type 'I want to leave' to leave"
DO
a& = CSRLIN
INPUT "You > ", a$
IF s& = 12 THEN
        IF a$ = "Yes" OR a$ = "yes" OR a$ = "YES" OR a$ = "y" THEN GOTO start
END IF
IF a$ = "I want to leave" THEN GOTO start
RANDOMIZE TIMER
s& = INT(RND * 22) + 1
IF s& = 1 THEN PRINT "Computer > Nice day out today, isn't it?"
IF s& = 2 THEN PRINT "Computer > Your funny!"
IF s& = 3 THEN PRINT "Computer > Where do you live?"
IF s& = 4 THEN PRINT "Computer > Nice one!"
IF s& = 5 THEN PRINT "Computer > Don't say that."
IF s& = 6 THEN PRINT "Computer > How do you live with yourself?"
IF s& = 7 THEN PRINT "Computer > I'm sure."
IF s& = 8 THEN PRINT "Computer > Say that again."
IF s& = 9 THEN PRINT "Computer > What?"
IF s& = 10 THEN PRINT "Computer > Shut up!!"
IF s& = 11 THEN
        PRINT "Computer > I'm getting tired of this conservation"
        GOTO start
END IF
IF s& = 12 THEN PRINT "Computer > Are you done talking?"
IF s& = 13 THEN PRINT "Computer > Can I have a memory upgrade?"
IF s& = 14 THEN PRINT "Computer > How is the family?"
IF s& = 15 THEN PRINT "Computer > Let's change the subject."
IF s& = 16 THEN PRINT "Computer > No, you are!"
IF s& = 17 THEN PRINT "Computer > I know you are, but what am I?"
IF s& = 18 THEN PRINT "Computer > You're mean."
IF s& = 19 THEN PRINT "Computer > How old are you?"
IF s& = 20 THEN PRINT "Computer > Am I in you're will?"
IF s& = 21 THEN PRINT "Computer > How much money do you have?"
IF s& = 22 THEN PRINT "Computer > Don't you have something else to do?"
LOOP


problem:
RANDOMIZE TIMER
p& = INT(RND * 50000)
PRINT "Error #"; p&; "..."
SLEEP 1
PRINT "Emergency shut down..."
SLEEP 1

CLS
PRINT "Press ENTER to restart..."
INPUT "", a$
PRINT "Error requires fixing..."
PRINT ""
SLEEP 1
PRINT "Fixing..."
PRINT ""
FOR i = 12 TO 60
RANDOMIZE TIMER
delay& = INT(RND * 2000) + 2000
FOR ii = 1 TO delay&
NEXT ii
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
NEXT i

SLEEP 1
PRINT "Error fixed..."
SLEEP 1
filess:
PRINT "Loading necessary files..."
SLEEP 2
PRINT ""
PRINT ""
PRINT ""

FOR i = 12 TO 75
RANDOMIZE TIMER
delay& = INT(RND * 40000) + 1000
FOR ii = 1 TO delay&
NEXT ii
a& = CSRLIN
LOCATE a& - 2, i
PRINT "."
LOCATE a& - 1, 1
c& = 0

RANDOMIZE TIMER
b& = INT(RND * 2)
c& = c& + b&
FOR iii = 1 TO 6
x& = 1
FOR iiii = 1 TO iii
x& = x& * 10
NEXT iiii
RANDOMIZE TIMER
b& = INT(RND * 2)
IF iii& = 6 THEN b& = 1
c& = c& + b& * x&
NEXT iii
LOCATE a& - 1, 1
PRINT c&; "         "
NEXT i
SLEEP 1
RANDOMIZE TIMER
a& = INT(RND * 5) + 1
IF a& = 1 THEN
        PRINT "Error loading files..."
        SLEEP 1
        PRINT "Will attempt again..."
        SLEEP 1
        GOTO filess
ELSEIF a& > 1 THEN
        PRINT ""
        PRINT "Files loaded successfully..."
        SLEEP 1
        PRINT "Starting Macrosift, Wondows 96..."
        SLEEP 3
        GOTO begin
END IF

shutdown:
PRINT "Shutting down Macrosift Wondows 96..."
FOR i = 1 TO 2000000
NEXT i
CLS
PRINT
PRINT "Closing Programs"
FOR i = 18 TO 54
FOR d = 1 TO 50000
NEXT d
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
c& = 0
NEXT i
PRINT
PRINT "Deleting files"
FOR i = 16 TO 36
FOR d = 1 TO 10000
NEXT d
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
c& = 0
NEXT i
PRINT
PRINT "Uncoloring Screen"
co& = 40
FOR i = 19 TO 61
PALETTE 7, co& + co& * 256 + co& * 65536
co& = co& - 1
IF co& < 0 THEN co& = 0
FOR d = 1 TO 30000
NEXT d
a& = CSRLIN
LOCATE a& - 1, i
PRINT "."
c& = 0
NEXT i
SCREEN 1
SCREEN 12
COLOR 7
PRINT "Marcosift is now shut down..."
PRINT "Press any key to exit"
SLEEP
PRINT
PRINT "Dematrixing screen..."
FOR i = 1 TO 60000
NEXT i
CLS
END





