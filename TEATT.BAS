SCREEN 13
RANDOMIZE TIMER



FOR i = 0 TO 20: PALETTE i, i * 3 + 256 * (i * 3) + 65536 * (i * 3): NEXT i
FOR i = 21 TO 40: PALETTE i, 65536 * ((i - 20) * 3): NEXT i
FOR i = 41 TO 60: PALETTE i, 256 * ((i - 40) * 3): NEXT i
FOR i = 61 TO 80: PALETTE i, 256 * ((i - 60) * 3) + 65536 * ((i - 60) * 3): NEXT i
FOR i = 81 TO 100: PALETTE i, ((i - 80) * 3): NEXT i
FOR i = 101 TO 120: PALETTE i, ((i - 100) * 3) + ((i - 100) * 3) * 65536: NEXT i
FOR i = 121 TO 140: PALETTE i, ((i - 120) * 3) + ((i - 120) * 3) * 256: NEXT i
FOR i = 141 TO 160: PALETTE i, ((i - 140) * 3) + INT((i - 140) * 1.5) * 256: NEXT i



DIM bg(784) AS INTEGER
DIM pi(701) AS INTEGER
DIM drop(49) AS INTEGER
FOR j = 0 TO 6
FOR i = 1 TO 12
a = i
b = i - 5
IF a > 6 THEN a = 6
IF b < 1 THEN b = 1
LINE (a, b)-(b, a), 15 - i + (j * 20)
NEXT i
LINE (7, 7)-(7, 0), 2 + (j * 20)
LINE (7, 7)-(0, 7), 2 + (j * 20)
LINE (0, 0)-(6, 0), 20 + (j * 20)
LINE (0, 0)-(0, 6), 20 + (j * 20)
PSET (0, 7), 11 + (j * 20)
PSET (7, 0), 11 + (j * 20)
GET (0, 0)-(7, 7), pi(49 * j + 1)
NEXT j

SLEEP
LINE (0, 0)-(31, 0), 160
LINE (0, 0)-(0, 31), 160
LINE (31, 31)-(31, 0), 142
LINE (31, 31)-(0, 31), 142
PSET (31, 0), 151
PSET (0, 31), 151
PSET (1, 1), 160
LINE (2, 1)-(1, 2), 160
FOR i = 3 TO 30
LINE (i, 1)-(1, i), 160 - INT(i / 3) + 1
NEXT i
FOR i = 1 TO 30
LINE (30, i)-(i, 30), 160 - INT((i + 30) / 3) + 1
NEXT i
PSET (30, 30), 142
SLEEP
LINE (0, 0)-(31, 0), 10
LINE (0, 0)-(0, 31), 10
LINE (31, 31)-(31, 0), 2
LINE (31, 31)-(0, 31), 2
PSET (31, 0), 6
PSET (0, 31), 6
PSET (1, 1), 10
LINE (2, 1)-(1, 2), 10
FOR i = 3 TO 30
LINE (i, 1)-(1, i), 10 - INT(i / 6) + 1
NEXT i
FOR i = 1 TO 30
LINE (30, i)-(i, 30), 10 - INT((i + 30) / 6) + 1
NEXT i
PSET (30, 30), 2

