SCREEN 12
size = 10
top = 1 'no greater then 48
FOR i = top + 14 TO top STEP -1
PALETTE i - (top - 1), i
FOR a = 1 TO size
CIRCLE (320, 240), (((top + 15) - i) * size) - a, i - (top - 1)
NEXT a
NEXT i

