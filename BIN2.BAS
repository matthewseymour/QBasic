CLS
a& = 0
b& = 0
c& = 0
d& = 0
e& = 0
f& = 0
g& = 0
h& = 0
i& = 0
j& = 0
k& = 0
l& = 0
m& = 0
n& = 0
o& = 0
p& = 0
q& = 0
r& = 0
s& = 0
t& = 0
u& = 0
v& = 0
w& = 0
x& = 0
y& = 0
lin& = 1
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1

DO

a& = a& + 1
IF a& = 2 THEN
        IF lin& = 1 THEN lin& = lin& + 1
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        a& = 0
        b& = b& + 1
END IF
IF b& = 2 THEN
        IF lin& = 2 THEN lin& = lin& + 1
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        b& = 0
        c& = c& + 1
END IF
IF c& = 2 THEN
        IF lin& = 3 THEN lin& = lin& + 1
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        c& = 0
        d& = d& + 1
END IF
IF d& = 2 THEN
        IF lin& = 4 THEN lin& = lin& + 1
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        d& = 0
        e& = e& + 1
END IF
IF e& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 5 THEN lin& = lin& + 1
        e& = 0
        f& = f& + 1
END IF
IF f& = 2 THEN
        IF lin& = 6 THEN lin& = lin& + 1
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        f& = 0
        g& = g& + 1
END IF
IF g& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 7 THEN lin& = lin& + 1
        g& = 0
        h& = h& + 1
END IF
IF h& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 8 THEN lin& = lin& + 1
        h& = 0
        i& = i& + 1
END IF
IF i& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 9 THEN lin& = lin& + 1
        i& = 0
        j& = j& + 1
END IF
IF j& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 10 THEN lin& = lin& + 1
        j& = 0
        k& = k& + 1
END IF
IF k& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 11 THEN lin& = lin& + 1
        k& = 0
        l& = l& + 1
END IF
IF l& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 12 THEN lin& = lin& + 1
        l& = 0
        m& = m& + 1
END IF
IF m& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 13 THEN lin& = lin& + 1
        m& = 0
        n& = n& + 1
END IF
IF n& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 14 THEN lin& = lin& + 1
        n& = 0
        o& = o& + 1
END IF
IF o& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 15 THEN lin& = lin& + 1
        o& = 0
        p& = p& + 1
END IF
IF p& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 16 THEN lin& = lin& + 1
        p& = 0
        q& = q& + 1
END IF
IF q& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 17 THEN lin& = lin& + 1
        q& = 0
        r& = r& + 1
END IF
IF r& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 18 THEN lin& = lin& + 1
        r& = 0
        s& = s& + 1
END IF
IF s& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 19 THEN lin& = lin& + 1
        s& = 0
        t& = t& + 1
END IF
IF t& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 20 THEN lin& = lin& + 1
        t& = 0
        u& = u& + 1
END IF
IF u& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 21 THEN lin& = lin& + 1
        u& = 0
        v& = v& + 1
END IF
IF v& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 22 THEN lin& = lin& + 1
        v& = 0
        w& = w& + 1
END IF
IF w& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 23 THEN lin& = lin& + 1
        w& = 0
        x& = x& + 1
END IF
IF x& = 2 THEN
        RANDOMIZE TIMER
        ran& = INT(RND * 9) + 1
        IF lin& = 24 THEN lin& = lin& + 1
        x& = 0
        y& = y& + 1
END IF
IF y& = 2 THEN EXIT DO

LOCATE lin&, 1
PRINT y&; x&; w&; v&; u&; t&; s&; r&; q&; p&; o&; n&; m&; l&; k&; j&; i&; h&; g&; f&; e&; d&; c&; b&; a&; " "; ran&
LOOP

