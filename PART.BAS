SCREEN 12
SLEEP
RANDOMIZE TIMER
DIM partx(500) AS SINGLE
DIM party(500) AS SINGLE
DIM energy(500) AS SINGLE
DIM dir(500) AS SINGLE
DIM spin(500) AS INTEGER
DIM xch(500) AS SINGLE
DIM ych(500) AS SINGLE
DIM charge(500) AS SINGLE


FOR i = 1 TO 500
partx(i) = INT(RND * 638) + 1
party(i) = INT(RND * 478) + 1
charge(i) = -1'INT(RND * 2) * 2 - 1
spin(i) = INT(RND * 2) * 2 - 1
energy(i) = INT(RND * 100) / 100
dir(i) = INT(RND * 628) / 100
NEXT i
FOR i = 1 TO 10
charge(i) = 1
'partx(i) = 320 + i
'party(i) = 240
NEXT i
num = 20
cons = 500
cons2 = 20
DO
FOR i = 1 TO num
'xch(i) = xch(i) * .9999
'ych(i) = ych(i) * .9999
FOR j = 1 TO num
IF j <> i THEN
        diffx = partx(j) - partx(i)
        diffy = party(j) - party(i)
        IF ABS(diffx - 639) < ABS(diffx) THEN diffx = diffx - 639
        IF ABS(diffx + 639) < ABS(diffx) THEN diffx = diffx + 639
        IF ABS(diffy - 479) < ABS(diffy) THEN diffy = diffy - 479
        IF ABS(diffy + 479) < ABS(diffy) THEN diffy = diffy + 479
        s = 0
        IF SQR(diffx ^ 2 + diffy ^ 2) <> 0 THEN s = (charge(i) * charge(j) * cons) / (diffx ^ 2 + diffy ^ 2)
        IF charge(i) = 1 AND charge(j) = 1 THEN
                IF SQR(diffx ^ 2 + diffy ^ 2) < cons2 THEN
                        s = s * -10
                        xch(i) = xch(i) * SQR(diffx ^ 2 + diffy ^ 2) / cons2
                        ych(i) = ych(i) * SQR(diffx ^ 2 + diffy ^ 2) / cons2
                END IF
        END IF
        IF s <> 0 AND ABS(s) > SQR(diffx ^ 2 + diffy ^ 2) / 2 THEN s = SQR(diffx ^ 2 + diffy ^ 2) / 2 * (s / ABS(s))
        IF diffx = 0 THEN diffx = .1
        a = ATN(ABS(diffy / diffx))
        IF diffy < 0 AND diffx > 0 THEN a = 6.283 - a
        IF diffy < 0 AND diffx < 0 THEN a = 3.1415 + a
        IF diffy > 0 AND diffx < 0 THEN a = 3.1415 - a
        xch(i) = xch(i) - COS(a) * s
        ych(i) = ych(i) - SIN(a) * s
        'LINE (partx(i), party(i))-(partx(i) - COS(a) * 30, party(i) - SIN(a) * 30), 5
END IF
NEXT j

NEXT i


FOR i = 1 TO num

CIRCLE (partx(i), party(i)), 1, 0
'LINE (partx(i), party(i))-(partx(i) + xch(i), party(i) + ych(i)), charge(i) + 6
partx(i) = partx(i) + xch(i)
party(i) = party(i) + ych(i)
IF partx(i) > 639 THEN partx(i) = partx(i) - 639
IF partx(i) < 0 THEN partx(i) = partx(i) + 639
IF party(i) > 479 THEN party(i) = party(i) - 479
IF party(i) < 0 THEN party(i) = party(i) + 479
CIRCLE (partx(i), party(i)), 1, charge(i) + 14
totalxch = totalxch + xch(i)
totalych = totalych + ych(i)
NEXT i
FOR i = 1 TO 10000
NEXT i
IF INKEY$ = CHR$(27) THEN END
LOOP

