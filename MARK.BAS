CLS
SCREEN 12
start:
PRINT "Welcome to the game of questions on world records"
INPUT "", b$
CLS
PRINT "Q1 "
PRINT "The greatest number of volunteers to have collected litter in one place on one  day is"
PRINT "A.50,405"
PRINT "B.50,659"
PRINT "C.40,211"
INPUT "", a$
IF a$ = "a" THEN PRINT "You're right" ELSE GOTO lose
INPUT "", b$
CLS

PRINT "Q2"
PRINT "The fattest living cat in the world is _______ pounds"
PRINT "A.25 lbs"
PRINT "B.36 lbs"
PRINT "C.42 lbs"
INPUT "", a$
IF a$ = "b" THEN PRINT "Congradulatons you move on!" ELSE GOTO lose
INPUT "", b$
CLS















lose:

PRINT "sorry start again"
SLEEP
CLS
GOTO start

