SCREEN 13
RANDOMIZE TIMER
LINE (0, 0)-(320, 200), 0, BF
DIM x(1000) AS DOUBLE
DIM y(1000) AS DOUBLE
DIM sx(10)
DIM sy(10)
DIM sc(10)
DIM se(10)
PALETTE 0, 63 + 63 * 256 + 63 * 65536
FOR i = 1 TO 250
PALETTE i, i / 4
NEXT i
INPUT "Number of Spheres? ", num
FOR i = 1 TO num
se(i) = 1
sc(i) = 40
PRINT "Sphere"; i
INPUT "Charge? ", sc(i)
INPUT "X? ", sx(i)
INPUT "Y? ", sy(i)
IF sy(i) = 0 THEN sy(i) = INT(RND * 180) + 10
IF sx(i) = 0 THEN sx(i) = INT(RND * 300) + 10
IF sc(i) = 0 THEN sc(i) = INT(RND * 120) - 60
NEXT i
LINE (0, 0)-(320, 200), 0, BF
FOR i = 1 TO 1000
retry:
x(i) = INT(RND * 320)
y(i) = INT(RND * 200)
FOR j = 1 TO 10
        IF SQR((x(i) - sx(j)) ^ 2 + (y(i) - sy(j)) ^ 2) <= 5 AND se(j) = 1 THEN GOTO retry
NEXT j
NEXT i
SLEEP
FOR i = 1 TO num
CIRCLE (sx(i), sy(i)), 5, 1
NEXT i
FOR i = 250 TO 1 STEP -1
FOR j = 1 TO 1000
FOR k = 1 TO 10
IF se(k) = 1 THEN
        diffx = x(j) - sx(k)
        diffy = y(j) - sy(k)
        IF diffx = 0 THEN diffx = .0000001
        a = ATN(diffy / ABS(diffx))
        IF diffx > 0 THEN b = 1
        IF diffx < 0 THEN b = -1
        ox = x(j)
        oy = y(j)
        IF SQR(diffx ^ 2 + diffy ^ 2) > 5 THEN
                x(j) = x(j) + COS(a) * (sc(k) * b * (1 / SQR(diffx ^ 2 + diffy ^ 2)))
                y(j) = y(j) + SIN(a) * (sc(k) * (1 / SQR(diffx ^ 2 + diffy ^ 2)))
        END IF
END IF
LINE (ox, oy)-(x(j), y(j)), i
NEXT k
NEXT j
FOR j = 1 TO num
CIRCLE (sx(j), sy(j)), 5, 1
NEXT j
NEXT i
SLEEP

