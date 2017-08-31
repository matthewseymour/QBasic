DO
RANDOMIZE TIMER
opp = INT(RND * 4) + 1
show& = INT(RND * 3) + 1
nu1& = INT(RND * 9999) + 1
nu2& = INT(RND * 9999) + 1
use& = 0
IF opp = 1 THEN ans& = nu1& + nu2&
IF opp = 2 THEN ans& = nu1& - nu2&
IF opp = 3 THEN ans& = nu1& * nu2&
IF opp = 4 AND nu1& < nu2& THEN
        ans2! = nu1& / nu2&
        use& = 1
ELSEIF opp = 4 AND nu1& > nu2& THEN
        ans& = nu1& / nu2&
END IF
IF opp = 1 THEN opp = 43
IF opp = 2 THEN opp = 45
IF opp = 3 THEN opp = 120
IF opp = 4 THEN opp = 246

IF show& = 1 THEN
        IF use& = 0 THEN PRINT nu1&; CHR$(opp); nu2&; "="; ans&
        IF use& = 1 THEN PRINT nu1&; CHR$(opp); nu2&; "="; ans2!
ELSEIF show& = 2 THEN
        IF use& = 0 THEN PRINT HEX$(nu1&); CHR$(opp); HEX$(nu2&); "="; HEX$(ans&)
        IF use& = 1 THEN PRINT HEX$(nu1&); CHR$(opp); HEX$(nu2&); "="; HEX$(ans2!)
ELSEIF show& = 3 THEN
        IF use& = 0 THEN PRINT OCT$(nu1&); CHR$(opp); OCT$(nu2&); "="; OCT$(ans&)
        IF use& = 1 THEN PRINT OCT$(nu1&); CHR$(opp); OCT$(nu2&); "="; OCT$(ans2!)
END IF
FOR i = 1 TO 50000
NEXT i
LOOP

