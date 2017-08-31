SCREEN 12
CLS
PRINT "welcome to the house of horror"
INPUT "", a$
CLS
PRINT "Q1"
PRINT "what is the house of horror?"
PRINT "(A) A house where bug eyed insects eat people"
PRINT "(B) A house where a little old witch lives"
PRINT "(C) A place where they teach french class"
INPUT "", a$
IF a$ <> "c" THEN GOTO lose

CLS
PRINT "Q2"
PRINT "Do the house of horror have electricty?"
PRINT "(A)Yes but only for stupid french videos"
PRINT "(B)No"
PRINT "(C)Yes"
INPUT "", a$
IF a$ <> "a" THEN GOTO lose

CLS
PRINT "Q3"
PRINT "What is the evilest deamon of all?"
PRINT "(A)The Devil"
PRINT "(B)Mr. Jordine"
PRINT "(C)Headless Harry"
INPUT "", a$
IF a$ <> "b" THEN GOTO lose

CLS
PRINT "Q4"
PRINT "What is Mr. Jordine's altar ego"
PRINT "(A)The devil"
PRINT "(B)A witch"
PRINT "(C)All of the above"
INPUT "", a$
IF a$ <> "c" THEN GOTO lose

CLS
PRINT "Q4"
PRINT "What flag flies outside the house of horrors"
PRINT "(A)A Pentagram"
PRINT "(B) A flaming devil's skull"
PRINT "(C) A flaming devil's skull with a pentagram branded on to the cheek and the numbers 666 writen underneath!"
INPUT "", a$
IF a$ <> "c" THEN GOTO lose
CLS
END

































                  
CLS
lose:
PRINT "You lose HA HA HA now you must spend the rest of your life in the house of horror!"
END

