SCREEN 12
CLS
start:
LOCATE 12, 30
PRINT "Welcome to the apf6003."
LOCATE 13, 30
INPUT "number one?"; a
LOCATE 14, 30
INPUT "fuction? (+,-,*,/,%,sqr)"; f$
LOCATE 15, 30
INPUT "number two?"; b
LOCATE 16, 30
IF f$ = "+" THEN PRINT "Sum="; a + b
IF f$ = "-" THEN PRINT "Difference="; a - b
IF f$ = "*" THEN PRINT "Product="; a * b
IF f$ = "/" THEN
IF b = 0 THEN GOTO bastard
PRINT "Q="; a / b
END IF
IF f$ = "%" THEN
IF b = 0 THEN GOTO bastard
PRINT "Percentage="; (a / b) * 100
END IF
IF f$ = "sqr" THEN
IF a < 0 THEN GOTO bastard
IF b < 0 THEN GOTO bastard

PRINT "Square Root(a)="; SQR(a)
LOCATE 17, 30
PRINT "Square Root(b)="; SQR(b)
END IF

LOCATE 18, 30
INPUT "Q Quits, Enter for another", x$
IF x$ = "q" THEN END
CLS
GOTO start

bastard:
PRINT "ZERO POINT E!"

INPUT "", x$
CLS
GOTO start




