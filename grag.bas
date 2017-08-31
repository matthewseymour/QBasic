$include: 'qb.bi'
DIM regs AS regType
SCREEN 13
DO
regs.AX = 3
CALL INTERRUPT(&H33, regs, regs)
leftB = regs.BX AND 1 'whether or not left button is pressed
rightB = regs.BX AND 2 'whether or not right button is pressed
IF leftB THEN 'if left button is pressed
PSET (regs.CX, regs.DX), 15 'draw a dot at mouse coordinates
END IF
LOOP UNTIL rightB 'press the right button to end
