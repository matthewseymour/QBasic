'11/20/03 4:41PM BLACK WINS!
CLS
SCREEN 12
LINE (0, 0)-(640, 480), 6, BF
DIM fx(1, 100) AS INTEGER
DIM fy(1, 100) AS INTEGER
DIM ax(1, 200) AS INTEGER
DIM ay(1, 200) AS INTEGER
DIM dx(1, 200) AS INTEGER
DIM dy(1, 200) AS INTEGER
DIM t(1, 200) AS INTEGER
DIM l(1, 200) AS INTEGER
food& = 5
DIM rfx(1, 100) AS INTEGER
DIM rfy(1, 100) AS INTEGER
DIM rax(1, 200) AS INTEGER
DIM ray(1, 200) AS INTEGER
DIM rdx(1, 200) AS INTEGER
DIM rdy(1, 200) AS INTEGER
DIM rt(1, 200) AS INTEGER
DIM rl(1, 200) AS INTEGER
rfood& = 5

RANDOMIZE TIMER
fx(1, 1) = INT(RND * 640) + 1
fy(1, 1) = INT(RND * 480) + 1
fx(1, 31) = INT(RND * 640) + 1
fy(1, 31) = INT(RND * 480) + 1
fx(1, 61) = INT(RND * 640) + 1
fy(1, 61) = INT(RND * 480) + 1

qx& = fx(1, 1) - 5
qy& = fy(1, 1) - 5
rqx& = fx(1, 61) - 5
rqy& = fy(1, 61) - 5

IF qx& > 640 THEN qx& = 640
IF qx& < 0 THEN qx& = 0
IF qy& > 480 THEN qy& = 480
IF qy& < 0 THEN qy& = 0
IF rqx& > 640 THEN rqx& = 640
IF rqx& < 0 THEN rqx& = 0
IF rqy& > 480 THEN rqy& = 480
IF rqy& < 0 THEN rqy& = 0


FOR i = 2 TO 30
RANDOMIZE TIMER
fx(1, i) = fx(1, 1) + INT(RND * 50) - 25
fy(1, i) = fy(1, 1) + INT(RND * 50) - 25
IF fx(1, i) > 640 THEN fx(1, i) = 640
IF fx(1, i) < 0 THEN fx(1, i) = 0
IF fy(1, i) > 480 THEN fy(1, i) = 480
IF fy(1, i) < 0 THEN fy(1, i) = 0
NEXT i

FOR i = 32 TO 60
RANDOMIZE TIMER
fx(1, i) = fx(1, 31) + INT(RND * 50) - 25
fy(1, i) = fy(1, 31) + INT(RND * 50) - 25
IF fx(1, i) > 640 THEN fx(1, i) = 640
IF fx(1, i) < 0 THEN fx(1, i) = 0
IF fy(1, i) > 480 THEN fy(1, i) = 480
IF fy(1, i) < 0 THEN fy(1, i) = 0
NEXT i


FOR i = 62 TO 100
RANDOMIZE TIMER
fx(1, i) = fx(1, 61) + INT(RND * 50) - 25
fy(1, i) = fy(1, 61) + INT(RND * 50) - 25
NEXT i

DO

fixbug& = fixbug& + 1
IF fixbug& = 2000 THEN LINE (0, 0)-(640, 480), 6, BF

a& = 0
FOR i = 1 TO 100
IF fx(1, i) = -100 THEN a& = a& + 1
NEXT i

IF a& > 29 THEN GOSUB replace
FOR i = 1 TO 100
CIRCLE (fx(1, i), fy(1, i)), 1, 2
NEXT i


CIRCLE (qx&, qy&), 1, 0
CIRCLE (qx&, qy&), 2, 0
IF food& > 3 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 10) + 1
        FOR i = 1 TO 200
                IF l(1, i) = 0 THEN
                        IF rand& = 1 THEN
                                food& = food& - 4
                                ax(1, i) = qx&
                                ay(1, i) = qy&
                                dx(1, i) = qx&
                                dy(1, i) = qy&
                                l(1, i) = 1
                                t(1, i) = 0
                                ants& = ants& + 1
                                EXIT FOR
                        END IF
                END IF
        NEXT i
END IF
FOR i = 1 TO 200
IF l(1, i) = 0 THEN GOTO finish
IF ax(1, i) = dx(1, i) AND ay(1, i) = dy(1, i) THEN
        IF ax(1, i) = qx& AND ay(1, i) = qy& THEN food& = food& + 1
        RANDOMIZE TIMER
        dx(1, i) = INT((RND * 50) - 25) + ax(1, i)
        dy(1, i) = INT((RND * 50) - 25) + ay(1, i)
        IF dx(1, i) = qx& AND dy(1, i) = qy& THEN dx(1, i) = qx& + 1
        FOR a = 1 TO 200
        IF t(1, i) = a THEN
                IF ABS(rax(1, t(1, i)) - ax(1, i)) < 3 AND ABS(ray(1, t(1, i)) - ay(1, i)) < 3 THEN
                        LINE (ax(1, i) - 20, ay(1, i) - 20)-(ax(1, i) + 20, ay(1, i) + 20), 6, BF
                        RANDOMIZE TIMER
                        ran& = INT(RND * 25) + 1
                        IF ran& <= 12 THEN
                                rl(1, t(1, i)) = 0
                        ELSEIF ran& >= 13 THEN
                                l(1, i) = 0
                        END IF
                        t(1, i) = 0
                        rt(1, t(1, i)) = 0
                END IF
        END IF
        NEXT a
        FOR a = 1 TO 200
        IF ABS(rax(1, a) - ax(1, i)) < 50 AND ABS(ray(1, a) - ay(1, i)) < 50 AND dx(1, i) <> qx& AND dy(1, i) <> qy& THEN
                t(1, i) = a
                c& = 0
                FOR b = 1 TO 200
                IF ABS(ax(1, b) - ax(1, i)) < 100 AND ABS(ay(1, b) - ay(1, i)) < 100 AND dx(1, b) <> qx& THEN
                        c& = c& + 1
                        IF c& = 20 THEN EXIT FOR
                        RANDOMIZE TIMER
                        d& = INT(RND * 2) + 1
                        IF d& = 1 THEN t(1, b) = a
                END IF
                NEXT b
        END IF
        NEXT a
        FOR a = 1 TO 100
        RANDOMIZE TIMER
        rand& = INT(RND * 100) + 1
        IF ABS(fx(1, rand&) - ax(1, i)) < 50 AND ABS(fy(1, rand&) - ay(1, i)) < 50 AND dx(1, i) <> qx& AND dy(1, i) <> qy& THEN
                dx(1, i) = fx(1, rand&)
                dy(1, i) = fy(1, rand&)
        END IF
        NEXT a
        FOR a = 1 TO 100
        IF ax(1, i) = fx(1, a) AND ay(1, i) = fy(1, a) THEN
                dx(1, i) = qx&
                dy(1, i) = qy&
                fx(1, a) = -100
                fy(1, a) = -100
                c& = 0
                FOR b = 1 TO 200
                IF ABS(ax(1, b) - ax(1, i)) < 300 AND ABS(ay(1, b) - ay(1, i)) < 300 AND ABS(ax(1, b) - ax(1, i)) > 50 AND ABS(ay(1, b) - ay(1, i)) > 50 AND dx(1, b) <> qx& THEN
                        dx(1, b) = ax(1, i)
                        dy(1, b) = ay(1, i)
                        c& = c& + 1
                        IF c& > 30 THEN EXIT FOR
                END IF
                NEXT b
        END IF
        NEXT a
END IF
IF rl(1, t(1, i)) = 0 THEN t(1, i) = 0
IF t(1, i) > 0 THEN
        dx(1, i) = rax(1, t(1, i))
        dy(1, i) = ray(1, t(1, i))
END IF
CIRCLE (ax(1, i), ay(1, i)), 1, 6
IF dx(1, i) > ax(1, i) THEN ax(1, i) = ax(1, i) + 1
IF dx(1, i) < ax(1, i) THEN ax(1, i) = ax(1, i) - 1
IF dy(1, i) > ay(1, i) THEN ay(1, i) = ay(1, i) + 1
IF dy(1, i) < ay(1, i) THEN ay(1, i) = ay(1, i) - 1

IF dx(1, i) > 640 THEN dx(1, i) = 640
IF dx(1, i) < 0 THEN dx(1, i) = 0
IF dy(1, i) > 480 THEN dy(1, i) = 480
IF dy(1, i) < 0 THEN dy(1, i) = 0
CIRCLE (ax(1, i), ay(1, i)), 1, 0
finish:
NEXT i

CIRCLE (rqx&, rqy&), 1, 4
CIRCLE (rqx&, rqy&), 2, 4
IF rfood& > 4 THEN
        RANDOMIZE TIMER
        rand& = INT(RND * 20) + 1
        FOR i = 1 TO 200
                IF rl(1, i) = 0 THEN
                        IF rand& = 1 THEN
                                rfood& = rfood& - 5
                                rax(1, i) = rqx&
                                ray(1, i) = rqy&
                                rdx(1, i) = rqx&
                                rdy(1, i) = rqy&
                                rl(1, i) = 1
                                rt(1, i) = 0
                                rants& = rants& + 1
                                EXIT FOR
                        END IF
                END IF
        NEXT i
END IF
FOR i = 1 TO 200
IF rl(1, i) = 0 THEN GOTO finish2:
IF rax(1, i) = rdx(1, i) AND ray(1, i) = rdy(1, i) THEN
        IF rax(1, i) = rqx& AND ray(1, i) = rqy& THEN rfood& = rfood& + 1
        FOR a = 1 TO 200
        IF rt(1, i) = a THEN
                IF ABS(ax(1, rt(1, i)) - rax(1, i)) < 3 AND ABS(ay(1, rt(1, i)) - ray(1, i)) < 3 THEN
                        LINE (rax(1, i) - 20, ray(1, i) - 20)-(rax(1, i) + 20, ray(1, i) + 20), 6, BF
                        RANDOMIZE TIMER
                        ran& = INT(RND * 7) + 1
                        IF ran& <= 3 THEN
                                rl(1, i) = 0
                        ELSEIF ran& >= 4 THEN
                                l(1, rt(1, i)) = 0
                        END IF
                        rt(1, i) = 0
                        t(1, rt(1, i)) = 0
                END IF
        END IF
        NEXT a
        RANDOMIZE TIMER
        rdx(1, i) = INT((RND * 50) - 25) + rax(1, i)
        rdy(1, i) = INT((RND * 50) - 25) + ray(1, i)
        IF rdx(1, i) = rqx& AND rdy(1, i) = rqy& THEN rdx(1, i) = rqx& + 1
        FOR a = 1 TO 100
        RANDOMIZE TIMER
        rand& = INT(RND * 100) + 1
        IF ABS(fx(1, rand&) - rax(1, i)) < 50 AND ABS(fy(1, rand&) - ray(1, i)) < 50 AND rdx(1, i) <> rqx& AND rdy(1, i) <> rqy& THEN
                rdx(1, i) = fx(1, rand&)
                rdy(1, i) = fy(1, rand&)
        END IF
        NEXT a
        FOR a = 1 TO 100
        IF rax(1, i) = fx(1, a) AND ray(1, i) = fy(1, a) THEN
                rdx(1, i) = rqx&
                rdy(1, i) = rqy&
                fx(1, a) = -100
                fy(1, a) = -100
                c& = 0
                FOR b = 1 TO 200
                IF ABS(rax(1, b) - rax(1, i)) < 300 AND ABS(ray(1, b) - ray(1, i)) < 300 AND ABS(rax(1, b) - rax(1, i)) > 50 AND ABS(ray(1, b) - ray(1, i)) > 50 AND rdx(1, b) <> rqx& THEN
                        rdx(1, b) = rax(1, i)
                        rdy(1, b) = ray(1, i)
                        c& = c& + 1
                        IF c& > 30 THEN EXIT FOR
                END IF
                NEXT b
        END IF
        NEXT a
        FOR a = 1 TO 200
        IF ABS(ax(1, a) - rax(1, i)) < 50 AND ABS(ay(1, a) - ray(1, i)) < 50 AND rdx(1, i) <> rqx& AND rdy(1, i) <> rqy& AND l(1, a) = 1 THEN
                rt(1, i) = a
                c& = 0
                FOR b = 1 TO 200
                IF ABS(rax(1, b) - rax(1, i)) < 200 AND ABS(ray(1, b) - ray(1, i)) < 200 AND rdx(1, b) <> rqx& THEN
                        c& = c& + 1
                        IF c& = 20 THEN EXIT FOR
                        RANDOMIZE TIMER
                        d& = INT(RND * 2) + 1
                        IF d& = 1 THEN rt(1, b) = a
                END IF
                NEXT b
        END IF
        NEXT a
END IF
IF l(1, rt(1, i)) = 0 THEN rt(1, i) = 0
IF rt(1, i) > 0 THEN
        rdx(1, i) = ax(1, rt(1, i))
        rdy(1, i) = ay(1, rt(1, i))
END IF
CIRCLE (rax(1, i), ray(1, i)), 1, 6
IF rdx(1, i) > rax(1, i) THEN rax(1, i) = rax(1, i) + 1
IF rdx(1, i) < rax(1, i) THEN rax(1, i) = rax(1, i) - 1
IF rdy(1, i) > ray(1, i) THEN ray(1, i) = ray(1, i) + 1
IF rdy(1, i) < ray(1, i) THEN ray(1, i) = ray(1, i) - 1

IF rdx(1, i) > 640 THEN rdx(1, i) = 640
IF rdx(1, i) < 0 THEN rdx(1, i) = 0
IF rdy(1, i) > 480 THEN rdy(1, i) = 480
IF rdy(1, i) < 0 THEN rdy(1, i) = 0
CIRCLE (rax(1, i), ray(1, i)), 1, 4
finish2:
NEXT i

LOOP

replace:
FOR i = 1 TO 100
IF fx(1, i) = -100 THEN
        RANDOMIZE TIMER
        fx(1, i) = INT(RND * 640) + 1
        fy(1, i) = INT(RND * 480) + 1
        b& = i
        EXIT FOR
END IF
NEXT i
FOR i = 1 TO 100
IF fx(1, i) = -100 THEN
        RANDOMIZE TIMER
        fx(1, i) = INT(RND * 50) - 25 + fx(1, b&)
        fy(1, i) = INT(RND * 50) - 25 + fy(1, b&)
END IF
IF fx(1, i) > 640 THEN fx(1, i) = 640
IF fx(1, i) < 0 THEN fx(1, i) = 0
IF fy(1, i) > 480 THEN fy(1, i) = 480
IF fy(1, i) < 0 THEN fy(1, i) = 0
NEXT i
RETURN

