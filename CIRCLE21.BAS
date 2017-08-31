SCREEN 12


Pi = 3.141592


x = 4!
y = 6!
z = 6.928203
d = 5

        
DO
theata = .2

        CIRCLE (320, 240), 100, 15
        PSET (320 + x * 10, 240 + y * 10), 15
        LINE (320 + COS(theata) * 200, 240 + SIN(theata) * 200)-(320 - COS(theata) * 200, 240 - SIN(theata) * 200), 15







ptheata = theata + Pi / 2
m = SIN(ptheata) / COS(ptheata)

cx = (x + m * y) / (1 + m ^ 2)
cy = m * cx

        LINE (320 + COS(ptheata) * 200, 240 + SIN(ptheata) * 200)-(320 - COS(ptheata) * 200, 240 - SIN(ptheata) * 200), 7
        PSET (320 + cx * 10, 240 + cy * 10), 4

r = SQR((x - cx) ^ 2 + (y - cy) ^ 2 + z ^ 2)
dtheata = d / 10     'radius of our sphere is always 10
i = SQR((x - cx) ^ 2 + (y - cy) ^ 2)
itheata = atan(z / i)
itheata = itheata + dtheata
i = COS(itheata) * r
z = SIN(itheata) * r
x = i * COS(theata) + cx
y = i * SIN(theata) + cy


        PSET (320 + x * 10, 240 + y * 10), 15
      
LOOP

