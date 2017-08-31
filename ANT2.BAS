CLS
SCREEN 12
RANDOMIZE TIMER
DIM x(10, 200) AS INTEGER
DIM y(10, 200) AS INTEGER
DIM q(10, 200) AS INTEGER
DIM c(10, 200) AS INTEGER
DIM hun(10, 200) AS INTEGER
DIM age(10, 200) AS INTEGER
DIM exi(10, 200) AS INTEGER
DIM queen(10, 200) AS INTEGER
DIM egg(10, 200) AS INTEGER
DIM home(10, 200) AS INTEGER
DIM solider(10, 200) AS INTEGER
DIM worker(10, 200) AS INTEGER
DIM dx(10, 200) AS INTEGER
DIM dy(10, 200) AS INTEGER
DIM t(10, 200) AS INTEGER
DIM food(10, 200) AS INTEGER

DIM fx(1 TO 300) AS INTEGER
DIM fy(1 TO 300) AS INTEGER
DIM fho(1 TO 300) AS INTEGER
DIM fex(1 TO 300) AS INTEGER

foodx = INT(RND * 640) + 1
foody = INT(RND * 480) + 1
FOR i = 1 TO 50
DO
fex(i) = 1
fx(i) = INT(RND * 100) - 50 + foodx
fy(i) = INT(RND * 100) - 50 + foody
LOOP UNTIL (fx(i) < 640 AND fx(i) > 0 AND fy(i) < 480 AND fy(i) > 0)
NEXT i
foodx = INT(RND * 640) + 1
foody = INT(RND * 480) + 1
FOR i = 51 TO 100
DO
fex(i) = 1
fx(i) = INT(RND * 100) - 50 + foodx
fy(i) = INT(RND * 100) - 50 + foody
LOOP UNTIL (fx(i) < 640 AND fx(i) > 0 AND fy(i) < 480 AND fy(i) > 0)
NEXT i
foodx = INT(RND * 640) + 1
foody = INT(RND * 480) + 1
FOR i = 101 TO 150
DO
fex(i) = 1
fx(i) = INT(RND * 100) - 50 + foodx
fy(i) = INT(RND * 100) - 50 + foody
LOOP UNTIL (fx(i) < 640 AND fx(i) > 0 AND fy(i) < 480 AND fy(i) > 0)
NEXT i
foodx = INT(RND * 640) + 1
foody = INT(RND * 480) + 1
FOR i = 151 TO 200
DO
fex(i) = 1
fx(i) = INT(RND * 100) - 50 + foodx
fy(i) = INT(RND * 100) - 50 + foody
LOOP UNTIL (fx(i) < 640 AND fx(i) > 0 AND fy(i) < 480 AND fy(i) > 0)
NEXT i
foodx = INT(RND * 640) + 1
foody = INT(RND * 480) + 1
FOR i = 201 TO 250
DO
fex(i) = 1
fx(i) = INT(RND * 100) - 50 + foodx
fy(i) = INT(RND * 100) - 50 + foody
LOOP UNTIL (fx(i) < 640 AND fx(i) > 0 AND fy(i) < 480 AND fy(i) > 0)
NEXT i
foodx = INT(RND * 640) + 1
foody = INT(RND * 480) + 1
FOR i = 251 TO 300
DO
fex(i) = 1
fx(i) = INT(RND * 100) - 50 + foodx
fy(i) = INT(RND * 100) - 50 + foody
LOOP UNTIL (fx(i) < 640 AND fx(i) > 0 AND fy(i) < 480 AND fy(i) > 0)
NEXT i

queen(1, 1) = 1
x(1, 1) = INT(RND * 640) + 1
y(1, 1) = INT(RND * 480) + 1
dx(1, 1) = x(1, 1)
dy(1, 1) = y(1, 1)
hun(1, 1) = 500
age(1, 1) = 20
exi(1, 1) = 1
DO

LOCATE 1, 1
PRINT hun(1, 1)
FOR i = 1 TO 300
IF fex(i) = 1 AND fho(i) = 0 THEN
        CIRCLE (fx(i), fy(i)), 1, 2
ELSEIF fex(i) = 0 OR fho(i) = 1 THEN
        CIRCLE (fx(i), fy(i)), 1, 0
END IF
NEXT i
d = d + 1
FOR c = 1 TO 10
FOR a = 1 TO 200
IF exi(c, a) = 0 THEN GOTO skip
IF d = 1 THEN hun(c, a) = hun(c, a) - 1
IF d = 1 THEN age(c, a) = age(c, a) + 1
IF hun(c, a) = 0 THEN
        exi(c, a) = 0
        CIRCLE (x(c, a), y(c, a)), 1, 0
        CIRCLE (x(c, a), y(c, a)), 2, 0
END IF
IF egg(c, a) = 1 THEN
        CIRCLE (x(c, a), y(c, a)), 1, 15
        IF age(c, a) > 5 THEN
                egg(c, a) = 0
                CIRCLE (x(c, a), y(c, a)), 1, 0
        END IF
END IF
IF queen(c, a) = 1 THEN
        IF age(c, a) > 5000 THEN
                exi(c, a) = 0
                CIRCLE (x(c, a), y(c, a)), 1, 0
                CIRCLE (x(c, a), y(c, a)), 2, 0
        END IF
        IF hun(c, a) = 0 THEN
                exi(c, a) = 0
                CIRCLE (x(c, a), y(c, a)), 1, 0
                CIRCLE (x(c, a), y(c, a)), 2, 0
        END IF
        IF hun(c, a) > 200 AND INT(RND * 20) + 1 = 1 THEN
                FOR i = 1 TO 200
                IF exi(c, i) = 0 THEN
                        hun(c, a) = hun(c, a) - 20
                        x(c, i) = INT(RND * 50) - 25 + x(c, a)
                        y(c, i) = INT(RND * 50) - 25 + y(c, a)
                        IF x(c, i) < 0 THEN x(c, i) = 0
                        IF x(c, i) > 640 THEN x(c, i) = 640
                        IF y(c, i) < 0 THEN y(c, i) = 0
                        IF y(c, i) > 480 THEN y(c, i) = 480
                        dx(c, i) = x(c, i)
                        dy(c, i) = y(c, i)
                        q(c, i) = a
                        c(c, i) = c
                        egg(c, i) = 1
                        hun(c, i) = 100
                        atype = INT(RND * 2) + 1
                        ahome = INT(RND * 2)
                        IF atype = 1 THEN solider(c, i) = 1
                        IF atype = 2 THEN worker(c, i) = 1
                        home(c, i) = ahome
                        age(c, i) = 1
                        exi(c, i) = 1
                        GOTO done
                END IF
                NEXT i
done:
        END IF
        IF x(c, a) = dx(c, a) AND y(c, a) = dy(c, a) THEN
                dx(c, a) = INT(RND * 200) - 100 + x(c, a)
                dy(c, a) = INT(RND * 200) - 100 + y(c, a)
                IF dx(c, a) < 0 THEN dx(c, a) = 0
                IF dx(c, a) > 640 THEN dx(c, a) = 640
                IF dy(c, a) < 0 THEN dy(c, a) = 0
                IF dy(c, a) > 480 THEN dy(c, a) = 480
        END IF
        CIRCLE (x(c, a), y(c, a)), 1, 0
        CIRCLE (x(c, a), y(c, a)), 2, 0
        IF dx(c, a) > x(c, a) THEN x(c, a) = x(c, a) + 1
        IF dx(c, a) < x(c, a) THEN x(c, a) = x(c, a) - 1
        IF dy(c, a) > y(c, a) THEN y(c, a) = y(c, a) + 1
        IF dy(c, a) < y(c, a) THEN y(c, a) = y(c, a) - 1
        CIRCLE (x(c, a), y(c, a)), 1, c
        CIRCLE (x(c, a), y(c, a)), 2, c
END IF
IF egg(c, a) = 1 THEN GOTO skip
IF worker(c, a) = 1 THEN
        IF home(c, a) = 1 THEN
                IF x(c, a) = dx(c, a) AND y(c, a) = dy(c, a) THEN
                        dx(c, a) = INT(RND * 100) - 50 + x(c(c, a), q(c, a))
                        dy(c, a) = INT(RND * 100) - 50 + y(c(c, a), q(c, a))
                        IF dx(c, a) < 0 THEN dx(c, a) = 0
                        IF dx(c, a) > 640 THEN dx(c, a) = 640
                        IF dy(c, a) < 0 THEN dy(c, a) = 0
                        IF dy(c, a) > 480 THEN dy(c, a) = 480
                        IF food(c, a) > 0 THEN
                                fho(food(c, a)) = 0
                                CIRCLE (fx(food(c, a)), fy(food(c, a))), 1, 0
                                fx(food(c, a)) = x(c, a)
                                fy(food(c, a)) = y(c, a)
                                CIRCLE (fx(food(c, a)), fy(food(c, a))), 1, 0
                                food(c, a) = 0
                                GOTO done2
                        END IF
                        FOR i = 1 TO 300
                                IF fx(i) = x(c, a) AND fy(i) = y(c, a) AND fex(i) = 1 THEN
                                        food(c, a) = i
                                        fho(i) = 1
                                        CIRCLE (fx(i), fy(i)), 1, 0
                                        IF hun(c, a) < 50 THEN
                                                fex(i) = 0
                                                food(c, a) = 0
                                                hun(c, a) = hun(c, a) + 50
                                        END IF
                                END IF
                        NEXT i
done2:
                        FOR i = 1 TO 200
                        ch = INT(RND * 300) + 1
                        IF fex(ch) = 1 AND food(c, a) = 0 THEN
                                IF ABS(fx(ch) - x(c, a)) < 50 AND ABS(fy(ch) - y(c, a)) < 50 THEN
                                        IF ((ABS(fx(ch) - x(c(c, a), q(c, a))) > 50 AND ABS(fy(ch) - y(c(c, a), q(c, a))) > 50) OR hun(c(c, a), q(c, a)) < 250 OR INT(RND * 5) + 1 = 1) AND fho(ch) = 0 THEN
                                                dx(c, a) = fx(ch)
                                                dy(c, a) = fy(ch)
                                        END IF
                                END IF
                        END IF
                        NEXT i
                END IF
                IF food(c, a) > 0 AND (hun(c(c, a), q(c, a)) < 250 OR INT(RND * (CLNG(hun(c(c, a), q(c, a))) / 10)) + 1 = 1) THEN
                        dx(c, a) = x(c(c, a), q(c, a))
                        dy(c, a) = y(c(c, a), q(c, a))
                END IF
        END IF
        IF home(c, a) = 0 THEN
                IF x(c, a) = dx(c, a) AND y(c, a) = dy(c, a) THEN
                        dx(c, a) = INT(RND * 100) - 50 + x(c, a)
                        dy(c, a) = INT(RND * 100) - 50 + y(c, a)
                        IF dx(c, a) < 0 THEN dx(c, a) = 0
                        IF dx(c, a) > 640 THEN dx(c, a) = 640
                        IF dy(c, a) < 0 THEN dy(c, a) = 0
                        IF dy(c, a) > 480 THEN dy(c, a) = 480
                        IF food(c, a) > 0 THEN
                                fho(food(c, a)) = 0
                                CIRCLE (fx(food(c, a)), fy(food(c, a))), 1, 0
                                fx(food(c, a)) = x(c, a)
                                fy(food(c, a)) = y(c, a)
                                CIRCLE (fx(food(c, a)), fy(food(c, a))), 1, 0
                                food(c, a) = 0
                                GOTO done3
                        END IF
                        FOR i = 1 TO 300
                                IF fx(i) = x(c, a) AND fy(i) = y(c, a) AND fex(i) = 1 THEN
                                        dx(c, a) = INT(RND * 100) - 50 + x(c(c, a), q(c, a))
                                        dy(c, a) = INT(RND * 100) - 50 + y(c(c, a), q(c, a))
                                        IF dx(c, a) < 0 THEN dx(c, a) = 0
                                        IF dx(c, a) > 640 THEN dx(c, a) = 640
                                        IF dy(c, a) < 0 THEN dy(c, a) = 0
                                        IF dy(c, a) > 480 THEN dy(c, a) = 480
                                        FOR j = 1 TO 200
                                                check = INT(RND * 200) + 1
                                                IF worker(c, check) = 1 AND home(c, check) = 0 AND exi(c, check) = 1 AND food(c, check) = 0 THEN
                                                        dx(c, check) = x(c, a)
                                                        dy(c, check) = y(c, a)
                                                END IF
                                        NEXT j
                                        food(c, a) = i
                                        fho(i) = 1
                                        CIRCLE (fx(i), fy(i)), 1, 0
                                        IF hun(c, a) < 50 THEN
                                                fex(i) = 0
                                                food(c, a) = 0
                                                hun(c, a) = hun(c, a) + 50
                                        END IF
                                END IF
                        NEXT i
done3:
                        FOR i = 1 TO 200
                        ch = INT(RND * 300) + 1
                        IF fex(ch) = 1 AND food(c, a) = 0 THEN
                                IF ABS(fx(ch) - x(c, a)) < 50 AND ABS(fy(ch) - y(c, a)) < 50 THEN
                                        IF ((ABS(fx(ch) - x(c(c, a), q(c, a))) > 50 AND ABS(fy(ch) - y(c(c, a), q(c, a))) > 50) OR hun(c(c, a), q(c, a)) < 250) AND fho(ch) = 0 THEN
                                                dx(c, a) = fx(ch)
                                                dy(c, a) = fy(ch)
                                        END IF
                                END IF
                        END IF
                        NEXT i
                END IF
                IF food(c, a) > 0 AND (hun(c(c, a), q(c, a)) < 250 OR INT(RND * (CLNG(hun(c(c, a), q(c, a))) / 10)) + 1 = 1) THEN
                        dx(c, a) = x(c(c, a), q(c, a))
                        dy(c, a) = y(c(c, a), q(c, a))
                END IF
        END IF
        CIRCLE (x(c, a), y(c, a)), 1, 0
        FOR i = 1 TO 5
        IF dx(c, a) > x(c, a) THEN x(c, a) = x(c, a) + 1
        IF dx(c, a) < x(c, a) THEN x(c, a) = x(c, a) - 1
        IF dy(c, a) > y(c, a) THEN y(c, a) = y(c, a) + 1
        IF dy(c, a) < y(c, a) THEN y(c, a) = y(c, a) - 1
        NEXT i
        CIRCLE (x(c, a), y(c, a)), 1, c
        IF x(c, a) = x(c(c, a), q(c, a)) AND y(c, a) = y(c(c, a), q(c, a)) AND food(c, a) > 0 THEN
                fex(food(c, a)) = 0
                CIRCLE (fx(food(c, a)), fy(food(c, a))), 1, 0
                hun(c(c, a), q(c, a)) = hun(c(c, a), q(c, a)) + 50
        END IF
        
END IF

skip:
NEXT a
NEXT c
IF d = 30 THEN d = 0
LOOP


