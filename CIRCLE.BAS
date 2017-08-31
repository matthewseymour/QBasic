REM CIRC(x, y, radius, color, xstretch, ystretch)
DECLARE SUB CIRC (x!, y!, r!, c!, xs!, ys!)
SCREEN 13
FOR i = 0 TO 5
CALL CIRC(50, 50, 30, 15, 1, 1)
NEXT i
FOR i = 0 TO 5
CALL CIRC(150, 150, 40, 1, 1, i / 5)
NEXT i

SUB CIRC (x, y, r, c, xs, ys)
IF xs >= 1 THEN xs2 = xs ELSE xs2 = 1
IF ys >= 1 THEN ys2 = ys ELSE ys2 = 1
FOR i = 1 TO (r * 8 * xs2 * ys2)
PSET (x + (SIN(((360 / (r * 8 * xs2 * ys2)) * i) * (3.141592 / 180))) * (r * xs), y + (COS(((360 / (r * 8 * xs2 * ys2)) * i) * (3.141592 / 180))) * (r * ys)), c
NEXT i
END SUB

