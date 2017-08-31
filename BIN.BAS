CLS
DIM num(1 TO 26) AS STRING
aa& = TIMER
DO
a& = a& + 1
IF a& = 2 THEN
        a& = 0
        b& = b& + 1
END IF
IF b& = 2 THEN
        b& = 0
        c& = c& + 1
END IF
IF c& = 2 THEN
        c& = 0
        d& = d& + 1
END IF
IF d& = 2 THEN
        d& = 0
        e& = e& + 1
END IF
IF e& = 2 THEN
        e& = 0
        f& = f& + 1
END IF
IF f& = 2 THEN
        f& = 0
        g& = g& + 1
END IF
IF g& = 2 THEN
        g& = 0
        h& = h& + 1
END IF
IF h& = 2 THEN
        h& = 0
        i& = i& + 1
END IF
IF i& = 2 THEN
        i& = 0
        j& = j& + 1
END IF
IF j& = 2 THEN
        j& = 0
        k& = k& + 1
END IF
IF k& = 2 THEN
        k& = 0
        l& = l& + 1
END IF
IF l& = 2 THEN
        l& = 0
        m& = m& + 1
END IF
IF m& = 2 THEN
        m& = 0
        n& = n& + 1
END IF
IF n& = 2 THEN
        n& = 0
        o& = o& + 1
END IF
IF o& = 2 THEN
        o& = 0
        p& = p& + 1
END IF
IF p& = 2 THEN
        p& = 0
        q& = q& + 1
END IF
IF q& = 2 THEN
        q& = 0
        r& = r& + 1
END IF
IF r& = 2 THEN
        r& = 0
        s& = s& + 1
END IF
IF s& = 2 THEN
        s& = 0
        t& = t& + 1
END IF
IF t& = 2 THEN
        t& = 0
        u& = u& + 1
END IF
IF u& = 2 THEN
        u& = 0
        v& = v& + 1
END IF
IF v& = 2 THEN
        v& = 0
        w& = w& + 1
END IF
IF w& = 2 THEN
        w& = 0
        x& = x& + 1
END IF
IF x& = 2 THEN
        x& = 0
        y& = y& + 1
END IF
IF y& = 2 THEN
        y& = 0
        z& = z& + 1
END IF
IF z& = 2 THEN EXIT DO

IF a& = 1 THEN num(1) = "1" ELSE num(1) = "0"
IF b& = 1 THEN num(2) = "1" ELSE num(2) = "0"
IF c& = 1 THEN num(3) = "1" ELSE num(3) = "0"
IF d& = 1 THEN num(4) = "1" ELSE num(4) = "0"
IF e& = 1 THEN num(5) = "1" ELSE num(5) = "0"
IF f& = 1 THEN num(6) = "1" ELSE num(6) = "0"
IF g& = 1 THEN num(7) = "1" ELSE num(7) = "0"
IF h& = 1 THEN num(8) = "1" ELSE num(8) = "0"
IF i& = 1 THEN num(9) = "1" ELSE num(9) = "0"
IF j& = 1 THEN num(10) = "1" ELSE num(10) = "0"
IF k& = 1 THEN num(11) = "1" ELSE num(11) = "0"
IF l& = 1 THEN num(12) = "1" ELSE num(12) = "0"
IF m& = 1 THEN num(13) = "1" ELSE num(13) = "0"
IF n& = 1 THEN num(14) = "1" ELSE num(14) = "0"
IF o& = 1 THEN num(15) = "1" ELSE num(15) = "0"
IF p& = 1 THEN num(16) = "1" ELSE num(16) = "0"
IF q& = 1 THEN num(17) = "1" ELSE num(17) = "0"
IF r& = 1 THEN num(18) = "1" ELSE num(18) = "0"
IF s& = 1 THEN num(19) = "1" ELSE num(19) = "0"
IF t& = 1 THEN num(20) = "1" ELSE num(20) = "0"
IF u& = 1 THEN num(21) = "1" ELSE num(21) = "0"
IF v& = 1 THEN num(22) = "1" ELSE num(22) = "0"
IF w& = 1 THEN num(23) = "1" ELSE num(23) = "0"
IF x& = 1 THEN num(24) = "1" ELSE num(24) = "0"
IF y& = 1 THEN num(25) = "1" ELSE num(25) = "0"
IF z& = 1 THEN num(26) = "1" ELSE num(26) = "0"
bb& = bb& + 1
cc& = TIMER
LOCATE 5, 1
PRINT "Binary number       "; num(26) + num(25) + num(24) + num(23) + num(22) + num(21) + num(20) + num(19) + num(18) + num(17) + num(16) + num(15) + num(14) + num(13) + num(12) + num(11) + num(10) + num(9) + num(8) + num(7) + num(6) + num(5) + num( _
4) + num(3) + num(2) + num(1)
PRINT "Octal number        "; OCT$(bb&)
PRINT "Hexadecimal Number  "; HEX$(bb&)
PRINT "Decimal number     "; bb&
PRINT "Time               "; cc& - aa&; "sec"
LOOP UNTIL INKEY$ <> ""

