'1PSQB-A8.BAS by Dr. Jacques Mallah (jackmallah@hotmail.com)
'http://hammer.prohosting.com/~mathmind/qb.html
'start qb4.5 with /l
'$DYNAMIC   compile with qb4.5 for more frames/sec
DECLARE SUB endit () : DECLARE SUB onscreen () : DECLARE SUB paintsprites ()
DECLARE SUB medkit.etc () : DECLARE SUB showhealth () : DECLARE SUB badguys ()
DECLARE SUB yourmove () : DECLARE SUB time () : DECLARE SUB yourshot ()
DECLARE SUB crashtest (bx!, by!, vx!, vy!) : DECLARE FUNCTION atan2! (y!, x!)
DECLARE SUB showbadguy (b%) : DECLARE SUB showbadshot (x%) : DECLARE SUB showurshot (x%)
DECLARE SUB raycast () : DECLARE SUB btexture (xx%, dd%, bcc%, c%, bcc2%)
DECLARE SUB putcircle (x%, y%, R%, col%, circdis!) : DECLARE SUB showmed (b%)
DECLARE SUB putbox (x1!, y1%, x2!, y2%, col%, boxdis!)
DECLARE SUB memcopy (fromseg%, fromoffset%, toseg%, tooffset%, bytes%)
DECLARE SUB intro () : DECLARE SUB maketables () : DECLARE SUB makeworld ()
DECLARE SUB onkb () : DECLARE SUB readassembly () : DECLARE SUB offkb ()
DECLARE SUB hLINE (x1%, x2%, y%, c%) : DECLARE SUB vline (x%, yt%, yb%, c%)
CLEAR , , 1000: ntx% = 7: sizey% = 30: sizex% = 60: nmeds% = 3: nammo% = 2
maxshots% = 9: nbguys% = 25: nbguysm1% = nbguys% - 1: nbguyst2% = nbguys% * 2
shift = 49: nshots% = maxshots%: nspr% = maxshots% + nbguys% * 2 + nmeds% + nammo%
DIM kbcontrol%(128), kbmatrix%(128), sprtop%(319), sprbot%(319), odd%(319)
DIM fmap%(sizex% - 1, sizey% - 1), wdis(319), testin%(ntx%, 63, 63), dsfc(319)
DIM cmap%(sizex% - 1, sizey% - 1), sb1%(159, 199), st(1800), ct(1800), hicol%(255)
DIM map%(sizex% - 1, sizey% - 1), tant(1800), xb%(1800), yb%(1800)
DIM lowcol%(-128 TO 127), bicol%(255), atx%(319), ammo%(1), oammo%(1)
c% = nmeds% + nammo% - 1: DIM med%(c%), scmed(c%), mx(c%), my(c%)
DIM medis(c%), medx(c%), medy(c%), stt(1800), ctt(1800)
DIM sht(nshots%), shosht%(nshots%), shtx(nshots%), shty(nshots%), vshx(nshots%), vshy(nshots%)
DIM shtang%(nshots%), shtdis(nshots%), dela%(nshots%), shtht%(nshots%)
DIM bgh%(nbguysm1%), bgx(nbguysm1%), bgy(nbguysm1%)
DIM x(nbguysm1%), y(nbguysm1%), vbx(nbguysm1%), vby(nbguysm1%)
DIM scbg(nbguysm1%), bgang%(nbguysm1%), bgsht(nbguysm1%)
DIM bgshosht%(nbguysm1%), bgshtx(nbguysm1%), bgshty(nbguysm1%)
DIM bgvshx(nbguysm1%), bgvshy(nbguysm1%), bgshtdis(nbguysm1%)
DIM bgdela%(nbguysm1%), bgshtht%(nbguysm1%)
DIM dis(nspr%), spord%(nspr%), sptype%(nspr%), disi%(nspr%)

CALL intro: maketables: readassembly: onkb: makeworld
main: raycast: yourshot: time: yourmove: badguys: showhealth: medkit.etc
CALL paintsprites: onscreen: endit: GOTO main

kbisrdata: 'Keyboard interrupt data; routine from KEYB2.BAS by Angelo
DATA &HE9,&H1D,0,&HE9,&H3C,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,&H1E,&H31,&HC0,&H8E,&HD8,&HBE,&H24,0,&H0E,7
DATA &HBF,&H14,0,&HFC,&HA5,&HA5,&H8C,&HC3,&H8E,&HC0,&HBF,&H24,0,&HB8
DATA &H56,0,&HFA,&HAB,&H89,&HD8,&HAB,&HFB,&H1F,&HCB,&H1E,&H31,&HC0,&H8E
DATA &HC0,&HBF,&H24,0,&HBE,&H14,0,&H0E,&H1F,&HFC,&HFA,&HA5,&HA5,&HFB
DATA &H1F,&HCB,&HFB,&H9C,&H50,&H53,&H51,&H52,&H1E,&H56,&H06,&H57,&HE4,&H60
DATA &HB4,1,&HA8,&H80,&H74,4,&HB4,0,&H24,&H7F,&HD0,&HE0,&H88,&HC3
DATA &HB7,0,&HB0,0,&H2E,3,&H1E,&H12,0,&H2E,&H8E,&H1E,&H10,0
DATA &H86,&HE0,&H89,&H07,&HE4,&H61,&H0C,&H82,&HE6,&H61,&H24,&H7F,&HE6,&H61
DATA &HB0,&H20,&HE6,&H20,&H5F,7,&H5E,&H1F,&H5A,&H59,&H5B,&H58,&H9D,&HCF,-1

'screen routine from QUICKBMP.BAS by Dan Holmes
DATA 85,137,229,30,139,70,10,142,192,139,70,14,142,216,139,118
DATA 8,139,126,12,139,78,6,243,164,31,93,203

'spritedata:
DATA 0,6,2,1,0,4,""

'Map: each character (>"0") is a color or texture
'0 is empty space.  Outer walls must not contain any 0's, ?'s, or r's
'1, 2, 3, 4, 5, 6, 7, 8, 9, :, ;, <, +, >, @, A are wall textures
'4 is the map, A is the rainbow
'? is an ice block "door"
'r is random: 50% chance of ice, else texture @
DATA "666666667546C66666666666666666666666666666666666666666666666"
DATA "60000?00000006000006000000000000000000000?000000000000000006"
DATA "6000010000000A00000?0000000000000000000003000000000000000006"
DATA "6000010000000@000006662?266666662526666666666664?766666666?6"
DATA "6000010000000?0000060000000000000000000006000000000000000006"
DATA "6000010000000>000006000000000000000000000r000000000000000006"
DATA "6000010000000=0000060000000000000000000006000000000000000001"
DATA "60000?0000000<00000666666666444666666666?6000000000000000001"
DATA "6000020000000;0000060000000001000000000006000000000000000001"
DATA "6000020000000:000006000000000200000000000r000000000000000006"
DATA "600002000000090000060000000003000000000006000000000000000006"
DATA "600002000000080000060000000004000000000006000000000000000006"
DATA "60000?00000007000006000000000A777777777776666664?76666666666"
DATA "600003000000060000060000000000000000000000000000000000000006"
DATA "600003000000050000060000000000000000000000000000000000000006"
DATA "600003000000040000070000000000000000000000000000000000000006"
DATA "60000?000000030000060000000000000000000000000000000000000006"
DATA "600004000000020000060000000000000000000000000000000000000006"
DATA "600004000000010000612r45633r333333333333r3333600000000000006"
DATA "600004000000066576660000060000000000000000000600000000000006"
DATA "6000040000000?0000060000060000000000000000000600000000000006"
DATA "6000040000000?0000060000060000000000000000000?00000000000006"
DATA "6000055555555555555600000r0000000000000000000600000000000006"
DATA "6000000000?00000000600000600000000000000000006666666466666?6"
DATA "6000000000r000000006000006AAAAAAA?AAAAAAAAAA4600000000000003"
DATA "6000000000r00000000A0000000000000000000000000600000000000004"
DATA "6000000000r00000000A0000000000000000000000000r00000000000001"
DATA "6000000000r00000000A0000000000000000000000000600000000000002"
DATA "6000000000r00000000A0000000000000000000000000600000000000006"
DATA "6155555555555556AAA66666666666666AAA666656666666666656666666"

REM $STATIC
FUNCTION atan2 (y, x)
IF x = 0 THEN
IF y > 0 THEN atan2 = 90 ELSE IF y < 0 THEN atan2 = 270
ELSEIF x > 0 THEN
atan2 = (ATN(y / x) * 57.2958 + 360) MOD 360
ELSE
atan2 = (ATN(y / x) * 57.2958 + 180)
END IF
END FUNCTION

SUB badguys
SHARED nbguysm1%, testin%(), bgx(), bgy(), delta.t, bgh%(), dis()
SHARED px, py, bx, by, vx, vy, fdt, scbg(), bgang%(), x(), y(), fram%, ph%
SHARED bgsht(), bgshosht%(), bgvshx(), bgvshy(), ct(), st(), bgshtdis()
SHARED inx%, iny%, map%(), fmap%(), bsa%, bgshtx(), bgshty(), bgdela%()
SHARED bgshtht%(), nbguys%, vbx(), vby(), snd%

FOR x% = 0 TO nbguysm1%
testin%(4, INT(bgx(x%)) + 2, INT(bgy(x%)) + 19) = 0: NEXT

'bad guys: Note: I want to add some AI!
damp = .8 ^ fdt: sqrdt = SQR(delta.t) * 6
FOR x% = 0 TO nbguysm1%

IF bgh%(x%) > 0 THEN
bbgx = bx - bgx(x%): bbgy = by - bgy(x%)
dis(x%) = SQR(bbgx * bbgx + bbgy * bbgy) + .01
vbx(x%) = vbx(x%) * damp + (RND - .5) * sqrdt + bbgx / dis(x%) * 2 * delta.t * (1 - (x% = 8))
vby(x%) = vby(x%) * damp + (RND - .5) * sqrdt + bbgy / dis(x%) * 2 * delta.t * (1 - (x% = 8))
IF (px - bgx(x%)) ^ 2 + (py - bgy(x%)) ^ 2 < 1 THEN
vbx(x%) = vbx(x%) - bbgx / dis(x%) * fdt
vby(x%) = vby(x%) - bbgy / dis(x%) * fdt
vx = vx + bbgx / dis(x%) * fdt
vy = vy + bbgy / dis(x%) * fdt
END IF
svx% = SGN(vbx(x%)): svy% = SGN(vby(x%))
crashtest bgx(x%) + .15 * svx%, bgy(x%) + .15 * svy%, vbx(x%), vby(x%)
crashtest bgx(x%) - .15 * svx%, bgy(x%) + .15 * svy%, vbx(x%), vby(x%)
crashtest bgx(x%) + .15 * svx%, bgy(x%) - .15 * svy%, vbx(x%), vby(x%)
bgx(x%) = bgx(x%) + vbx(x%) * delta.t: bgy(x%) = bgy(x%) + vby(x%) * delta.t
scbg(x%) = 2 / (dis(x%) + .01)
bgang%(x%) = atan2(bgy(x%) - by, bgx(x%) - bx) * 5
delba% = (bgang%(x%) - bsa% + 1800) MOD 1800
x(x%) = delba% - scbg(x%) * 20: y(x%) = 100 - 25 * scbg(x%)

'bad guy's shot
IF bgsht(x%) <= 0 THEN
bgsht(x%) = 20 + RND: bgshosht%(x%) = 1: 'create shot
bgshtx(x%) = bgx(x%): bgshty(x%) = bgy(x%)
bgsta% = (bgang%(x%) + 900) MOD 1800
bgvshx(x%) = ct(bgsta%) * 7
bgvshy(x%) = st(bgsta%) * 7
END IF
END IF

'bad guy's shot
IF bgsht(x%) > 0 AND bgshosht%(x%) THEN
crashtest bgshtx(x%), bgshty(x%), bgvshx(x%), bgvshy(x%)
k% = map%(inx%, iny%)
IF k% THEN
bgshosht%(x%) = 0
IF k% = 15 AND bgsht(x%) > 0 THEN
map%(inx%, iny%) = 0
testin%(4, inx% + 2, iny% + 19) = 0
END IF
ELSE
bgshtx(x%) = bgshtx(x%) + bgvshx(x%) * delta.t
bgshty(x%) = bgshty(x%) + bgvshy(x%) * delta.t
bbx = bgshtx(x%) - bx: bby = bgshty(x%) - by
bgshtang% = atan2(bby, bbx) * 5
bgshtdis(x%) = SQR(bby * bby + bbx * bbx + .01)
dis(x% + nbguys%) = bgshtdis(x%)
'fix damage test
IF bgshtdis(x%) < .5 THEN
ph% = ph% - bgsht(x%) / 4 - 2.5 * (1 - (x% = 8)): bgshosht%(x%) = 0
IF snd% THEN SOUND 150, 1
vx = vx + bgvshx(x%) * .05: vy = vy + bgvshy(x%) * .05
END IF
'FOR y% = 0 TO nbguysm1%
'IF x% <> y% THEN
'bsdis = SQR((bgshty(x%) - bgy(y%)) ^ 2 + (bgshtx(x%) - bgx(y%)) ^ 2 + .01)
'IF bsdis < .5 THEN
'bgh%(y%) = bgh%(y%) - bgsht(x%) / 2 - 1: bgshosht%(x%) = 0
'vbx(y%) = vbx(y%) + bgvshx(x%) * .5: vby(y%) = vby(y%) + bgvshy(x%) * .5
'IF bgh%(y%) < 1 THEN fmap%(INT(bgx(y%)), INT(bgy(y%))) = 4
'END IF
'END IF: NEXT
bgdela%(x%) = (bgshtang% - bsa% + 1800) MOD 1800
bgshtht%(x%) = 30 / bgshtdis(x%)
END IF
END IF
IF bgsht(x%) > 0 THEN bgsht(x%) = bgsht(x%) - fdt

IF fram% / 2 = fram% \ 2 THEN
testin%(4, INT(px) + 2, INT(py) + 19) = 1
IF bgh%(x%) > 0 AND x% <> 8 THEN testin%(4, INT(bgx(x%)) + 2, INT(bgy(x%)) + 19) = 4
END IF
NEXT x%
'IF bgh%(8) > 0 THEN testin%(4, INT(bgx(8)) + 2, INT(bgy(8)) + 19) = 8

END SUB

SUB crashtest (bx, by, vx, vy) : 'note vx & vy args must be byref
SHARED map%(), delta.t, inx%, iny%
STATIC oinx%, oiny%, nallcl%, chn2%, xsign%, ysign%, k%, kx%, ky%

oinx% = INT(bx): oiny% = INT(by): nallcl% = 1
px = bx + vx * delta.t: py = by + vy * delta.t
inx% = INT(px): iny% = INT(py)
ysign% = SGN(vy): xsign% = SGN(vx)
chn2% = (inx% - oinx%) * xsign% + (iny% - oiny%) * ysign%
k% = map%(inx%, iny%)
IF inx% = oinx% THEN horz% = 1
IF iny% = oiny% THEN vert% = 1
IF chn2% = 2 THEN
ys% = (1 + ysign%) \ 2: xs% = (1 + xsign%) \ 2
kx% = map%(oinx%, iny%): ky% = map%(inx%, oiny%)
tstang% = SGN((px - bx) * (iny% + 1 - ys% - by) - (py - by) * (inx% + 1 - xs% - bx))
tst% = xsign% * ysign% * tstang%
IF tst% = 1 AND k% + ky% = 0 THEN nallcl% = 0
IF tst% = -1 AND k% + kx% = 0 THEN nallcl% = 0
IF ky% = 0 THEN
horz% = 1
ELSE
vert% = 1: k% = ky%: IF tst% = 1 THEN iny% = oiny%
END IF
IF kx% THEN
horz% = 1: k% = kx%: IF tst% = -1 THEN inx% = oinx%
ELSE
vert% = 1
END IF
END IF: IF k% = 0 THEN nallcl% = 0
IF nallcl% THEN
IF horz% AND vert% AND ky% = 0 AND kx% = 0 THEN
IF tst% = 1 THEN horz% = 0 ELSE vert% = 0
END IF
IF vert% THEN vx = 0
IF horz% THEN vy = 0
END IF
END SUB

SUB endit
SHARED kills%, nbguysm1%, nbguys%, kbmatrix%(), goon%, ph%, bgh%(), snd%

IF kbmatrix%(1) - 1 AND ph% > 0 AND kills% < nbguys% THEN
goon% = 2
ELSE
goon% = goon% - 1
END IF
IF goon% = 0 THEN
LOCATE 2, 1: offkb
IF kills% = nbguys% AND ph% > 0 THEN
PRINT "President Snore, you made it!": IF snd% THEN PLAY "mf gcfde"
ELSE
PRINT "You die"
FOR t% = 400 TO 200 STEP -20
IF snd% THEN SOUND t%, 1
tim = TIMER: DO: LOOP UNTIL TIMER > tim
NEXT
END IF
tim = TIMER + .5: DO: LOOP UNTIL TIMER > tim
SLEEP 1: END
END IF
END SUB

SUB hLINE (x1%, x2%, y%, c%)
SHARED sb1%(), hicol%(): ccc% = hicol%(c%) + c%
IF x1% < 0 THEN x1% = 0
IF x2% > 319 THEN x2% = 319
FOR x% = INT(x1% / 2) TO INT(x2% / 2)
sb1%(x%, y%) = ccc%
NEXT
END SUB

SUB intro : SHARED snd%
CLS : PRINT "3d Jack's Game", "By Dr. Jacques Mallah", "Assassins Edition.8"
PRINT : PRINT "In the year 3001 AD:"
PRINT "You, President Sal Snore of the United Snows of Antarctica,"
PRINT "are trapped in the Wight House with a bunch of guys trying to kill you. "
PRINT "They also reprogrammed your robot bodyguard.": PRINT
PRINT "Luckily, you have your trusty plasma gun (press 1) and machine gun (press 2)."
PRINT "Hiding's not your style.  You'll show them who's the boss!"
PRINT "Kill 'em all to win.  (24 guys and the robot)": PRINT
PRINT "use arrow forward, back to move"
PRINT "use arrow left, right to rotate"
PRINT "Alt to strafe with arrow left, right"
PRINT "Ctrl to shoot"
PRINT "To fight, try getting some distance and using strafe"
PRINT "Try shooting out some ice blocks"
PRINT "pick up "; : COLOR 0, 2: PRINT "-"; : COLOR 7, 0: PRINT " ammo, and ";
COLOR 4, 15: PRINT "+"; : COLOR 7, 0: PRINT " medical kits when needed"
PRINT "After starting, press Esc to take the easy way out - suicide!"
PRINT "press any key to start, SPACE for no sound": PRINT
PRINT "The # at the top left corner is frames per second - QB yeah!"
PRINT "The bar at the bottom is your health."
PRINT "j to toggle cheat mode";
i$ = INPUT$(1): IF i$ <> " " THEN snd% = 1
END SUB

SUB maketables
SHARED st(), ct(), dsfc(), hicol%(), lowcol%(), bicol%(), atx%(), tant()
SHARED xb%(), yb%(), spord%(), nspr%, stt(), ctt()
FOR tmp1% = 0 TO 1800
st(tmp1%) = SIN(tmp1% * ATN(1) / 225): stt(tmp1%) = st(tmp1%) * 256
ct(tmp1%) = COS(tmp1% * ATN(1) / 225): ctt(tmp1%) = ct(tmp1%) * 256
NEXT tmp1%
st(0) = 10 ^ -9: st(900) = 10 ^ -9: st(1800) = st(0)
stt(0) = 10 ^ -7: stt(900) = 10 ^ -7
ct(450) = 10 ^ -9: ct(1350) = 10 ^ -9
ctt(450) = 10 ^ -7: ctt(1350) = 10 ^ -7
FOR t% = 0 TO 1800
sqct = ABS(1 / ct(t%)): sqt = ABS(1 / st(t%))
IF sqt > sqct THEN nn = sqct * 255 ELSE nn = sqt * 255
xb%(t%) = ct(t%) * nn: yb%(t%) = st(t%) * nn
tant(t%) = st(t%) / ct(t%): NEXT
yb%(0) = 0: yb%(900) = 0
xb%(450) = 0: xb%(1350) = 0
FOR x% = 0 TO 319
atx%(x%) = ATN((x% - 160) * 3.14159 / 900) * 900 / 3.14159
dsfc(x%) = 100 / ABS(ct((atx%(x%) + 1800) MOD 1800))
NEXT
FOR c% = 0 TO 255
hicol%(c%) = &H100 * (c% + &H100 * (c% > &H7F))
lowcol%(c% - 128) = c% - 128 - &H100 * ((c% - 128) < 0)
bicol%(c%) = &H100 * (c% + &H100 * (c% > &H7F)) + c%
NEXT
FOR x% = 0 TO nspr%: spord%(x%) = x%: NEXT
END SUB

SUB makeworld
SHARED fmap%(), sizex%, sizey%, testin%(), hicol%(), cmap%(), map%(), ntx%
SHARED ph%, nbguysm1%, bgh%(), bgy(), bgx(), oldtim, nmeds%, medx(), medy()
SHARED nshots%, med%(), ammo%(), weap$, px, py, sa, nammo%
SHARED F0%, F1%, F2%, F3%, F4%, F5%, bg$
DEFINT T, X-Y
SCREEN 13: PLAY "mb": RANDOMIZE TIMER
nshots% = 1: weap$ = " plasma gun": ammo%(0) = 24: ammo%(1) = 200
px = 17.5: py = 27.5: sa = 1190
READ F0%, F1%, F2%, F3%, F4%, F5%, bg$

FOR y = 0 TO sizey% - 1: READ i$: FOR x = 0 TO sizex% - 1
map%(x, y) = ASC(MID$(i$, x + 1, 1)) - 48
IF map%(x, y) = 66 THEN map%(x, y) = 16 + (RND < .5)
IF map%(x, y) < 0 THEN map%(x, y) = map%(x, y) + 256
IF y = 0 OR x = 0 OR y = sizey% - 1 OR x = sizex% - 1 THEN
IF map%(x, y) = 0 THEN map%(x, y) = 18
IF map%(x, y) = 15 THEN map%(x, y) = 14
END IF
NEXT: NEXT

FOR t = 0 TO ntx%: FOR x = 0 TO 63: FOR y = 0 TO 63
testin%(t, x, y) = (t * 14 + SQR((x - 32) ^ 2 + (y - 32 - RND * t) ^ 2)) MOD 256
testin%(t, x, y) = testin%(t, x, y) + hicol%(t + 1 + (RND < .1))
NEXT: NEXT: NEXT

FOR x = 2 TO 61: FOR y = 19 TO 48
testin%(4, x, y) = map%(x - 2, y - 19): NEXT: NEXT
FOR x = 0 TO 59: FOR y = 0 TO 29: fmap%(x, y) = ((x + y) MOD 16) + 128
IF map%(x, y) = 15 THEN fmap%(x, y) = 15
NEXT: NEXT: FOR x = 16 TO 18: FOR y = 26 TO 28
fmap%(x, y) = 208: NEXT: NEXT
fmap%(39, 15) = -7: fmap%(24, 10) = -2: fmap%(17, 25) = 0
FOR x = 20 TO 35: fmap%(x, 25) = 20 - x: NEXT

FOR x = 0 TO 59: FOR y = 0 TO 29: cmap%(x, y) = 26
IF x / 2 = x \ 2 OR y / 2 = y \ 2 THEN cmap%(x, y) = 27
IF x / 2 = x \ 2 AND y / 2 = y \ 2 THEN cmap%(x, y) = 15
NEXT: NEXT: FOR x = 16 TO 18: FOR y = 26 TO 28
cmap%(x, y) = 208: NEXT: NEXT: cmap%(17, 27) = 15

COLOR 16: PRINT "Abandon": PRINT "all dope"
PRINT "Your ad": PRINT "  here:": PRINT " $100": PRINT " Call"
PRINT " 1-800-": PRINT " EATS": PRINT "  666": PRINT " QB 4.5"
PRINT " I  $": PRINT : PRINT " Wight": PRINT " House": PRINT " HIT"
PRINT : PRINT " Who's": PRINT "da man?": PRINT " Please": PRINT "recycle"
PRINT "   JM": FOR x = 0 TO 63: FOR y = 0 TO 15
IF POINT(x, y) THEN testin%(1, x, y + 1) = 15
IF POINT(x, y + 16) THEN testin%(5, x, y + 8) = 0
IF POINT(x, y + 32) THEN testin%(5, x, y + 24) = 0
IF POINT(x, y + 48) THEN testin%(5, x, y + 40) = 0
IF POINT(x, y + 64) THEN testin%(6, x, y + 32) = 7
IF POINT(x, y + 80) THEN testin%(2, x, y + 1) = 4
IF POINT(x, y + 96) THEN testin%(4, x, y + 1) = 15
IF POINT(x, y + 112) AND y < 8 THEN testin%(5, x, y + 56) = 0
IF POINT(x, y + 128) THEN testin%(3, x, y + 48) = 1
IF POINT(x, y + 144) THEN testin%(0, x, y + 48) = 6
IF POINT(x, y + 160) THEN testin%(7, x, y + 32) = 9
NEXT: NEXT: COLOR 15
FOR x = 0 TO 63: FOR y = 0 TO 63
t = 15: IF (RND * 60 > y) THEN t = 24 + RND * 6
testin%(7, x, y) = (testin%(7, x, y) AND &HFF) + hicol%(t)
NEXT: NEXT

ph% = 100: FOR x% = 0 TO nbguysm1%: bgh%(x%) = 100
randloc:
bgx(x%) = INT(RND * (sizex% - 1) + 1) + .5
bgy(x%) = INT(RND * (sizey% - 1) + 1) + .5
IF map%(INT(bgx(x%)), INT(bgy(x%))) GOTO randloc
NEXT: oldtim = TIMER: bgh%(8) = 1250
FOR x% = 0 TO nmeds% + nammo% - 1: med%(x%) = 1: NEXT
medx(0) = 16: medy(0) = 26: 'meds
medx(1) = 18: medy(1) = 21.5
medx(2) = 56: medy(2) = 26
medx(3) = 58: medy(3) = 2: 'ammo
medx(4) = 23: medy(4) = 22
END SUB

DEFSNG T, X-Y
SUB medkit.etc : 'medkits and ammo boxes
SHARED nmeds%, medis(), nbguyst2%, maxshots%, medx(), medy(), scmed(), dis()
SHARED mx(), my(), ph%, bx, by, bgx(), bgy(), bgh%(), med%(), nbguysm1%, bsa%
SHARED ammo%(), nammo%

FOR x% = 0 TO nmeds% + nammo% - 1
IF med%(x%) THEN
medis(x%) = SQR((bx - medx(x%)) ^ 2 + (by - medy(x%)) ^ 2)
dis(x% + nbguyst2% + maxshots% + 1) = medis(x%)
scmed(x%) = 3 / (dis(x% + nbguyst2% + maxshots% + 1) + .01)
bgang% = atan2(medy(x%) - by, medx(x%) - bx) * 5
delba% = (bgang% - bsa% + 1800) MOD 1800
mx(x%) = delba% - scmed(x%) * 10: my(x%) = 100 + 15 * scmed(x%)
IF medis(x%) < .36 THEN
IF x% < nmeds% AND ph% < 95 THEN
med%(x%) = 0: ph% = ph% + 35: IF ph% > 98 THEN ph% = 98
END IF
IF x% >= nmeds% THEN
med%(x%) = 0: ammo%(0) = ammo%(0) + 8: ammo%(1) = ammo%(1) + 50
END IF
END IF
FOR y% = 0 TO nbguysm1%
IF bgh%(y%) > 0 THEN
bsdis = (bgx(y%) - medx(x%)) * (bgx(y%) - medx(x%)) + (bgy(y%) - medy(x%)) * (bgy(y%) - medy(x%))
IF med%(x%) AND bsdis < .6 AND bgh%(y%) < 95 AND y% <> 8 AND x% < nmeds% THEN
med%(x%) = 0: bgh%(y%) = bgh%(y%) + 35: IF bgh%(y%) > 98 THEN bgh%(y%) = 98
END IF
END IF: NEXT
END IF: NEXT

END SUB

SUB memcopy (fromseg%, fromoffset%, toseg%, tooffset%, bytes%)
SHARED asm$
DEF SEG = VARSEG(asm$)
CALL Absolute(BYVAL fromseg%, BYVAL fromoffset%, BYVAL toseg%, BYVAL tooffset%, BYVAL bytes%, SADD(asm$))
DEF SEG
END SUB

SUB offkb
SHARED keyboardonflag%, kbcontrol%()
IF (keyboardonflag% = 0) THEN EXIT SUB
keyboardonflag% = 0
DEF SEG = VARSEG(kbcontrol%(0))
CALL Absolute(3)
DEF SEG
END SUB

SUB onkb
SHARED kbcontrol%(), keyboardonflag%
IF keyboardonflag% THEN EXIT SUB
keyboardonflag% = 1
DEF SEG = VARSEG(kbcontrol%(0))
CALL Absolute(0)
DEF SEG
END SUB

SUB onscreen
SHARED bitex%, fire, sb1%(), mg%, omg%, weap$, ammo%(), oammo%()
SHARED kills%, okills%, oofram%, ofram%

bitex% = 1: t% = (fire > 0) * 15: hLINE 155, 166, 100, -t%
vline 160, 96, 104, 15 + t%: bitex% = 0

'draw on screen
WAIT &H3DA, 8: 'wait for screen refresh
memcopy VARSEG(sb1%(0, 8)), VARPTR(sb1%(0, 8)), &HA000, &HA00, &HF000
IF mg% <> omg% OR kills% > okills% OR ammo%(mg%) <> oammo%(mg%) THEN
LOCATE 1, 10: PRINT weap$;
PRINT USING " ###"; ammo%(mg%);
PRINT USING " ammo ### "; kills%; : PRINT "kill";
IF kills% <> 1 THEN PRINT "s";  ELSE PRINT " ";
omg% = mg%: okills% = kills%: oammo%(mg%) = ammo%(mg%)
END IF
IF oofram% <> ofram% THEN
LOCATE 1, 1: PRINT USING "### fps"; ofram%; : oofram% = ofram%
END IF

END SUB

SUB paintsprites
SHARED nspr%, spord%(), dis(), nbguyst2%, nbguys%, maxshots%, disi%()

'This uses the painter's algorithm with an exchange sort to show sprites
FOR x% = 0 TO nspr%: disi%(spord%(x%)) = dis(spord%(x%)) * 512: NEXT
FOR x% = 0 TO nspr% - 1: FOR y% = x% + 1 TO nspr%
IF disi%(spord%(y%)) > disi%(spord%(x%)) THEN SWAP spord%(x%), spord%(y%)
NEXT: NEXT: FOR xx% = 0 TO nspr%
IF spord%(xx%) < nbguys% THEN
showbadguy spord%(xx%)
ELSEIF spord%(xx%) < nbguyst2% THEN
showbadshot spord%(xx%) - nbguys%
ELSEIF spord%(xx%) < nbguyst2% + maxshots% + 1 THEN
showurshot spord%(xx%) - nbguyst2%
ELSE
showmed spord%(xx%) - nbguyst2% - maxshots% - 1
END IF: NEXT xx%

END SUB

SUB putbox (x1, y1%, x2, y2%, col%, boxdis)
SHARED wdis()
FOR x% = x1 TO x2
IF x% >= 0 AND x% < 320 THEN
IF boxdis < wdis(x%) THEN vline x%, y1%, y2%, col%
END IF
NEXT
END SUB

SUB putcircle (x%, y%, R%, col%, circdis)
SHARED wdis()
xb% = x% - R% + 1: xt% = x% + R% - 1
IF xb% > -1 AND xb% < 320 THEN
IF circdis < wdis(xb%) THEN showc% = 1
END IF
IF xt% > -1 AND xt% < 320 THEN
IF circdis < wdis(xt%) THEN showc% = showc% + 1
END IF
IF showc% = 1 THEN
FOR xx% = xb% TO xt%
IF xx% > -1 AND xx% < 320 THEN
IF circdis < wdis(xx%) THEN
shthtx% = R% * SQR(1 - (xx% - x%) * (xx% - x%) / R% / R%) * .8
vline xx%, y% - shthtx%, y% + shthtx%, col%
END IF
END IF
NEXT
ELSEIF showc% = 2 THEN
FOR xx% = xb% TO xt%
shthtx% = R% * SQR(1 - (xx% - x%) * (xx% - x%) / R% / R%) * .8
vline xx%, y% - shthtx%, y% + shthtx%, col%
NEXT
END IF
END SUB

SUB raycast
SHARED wdis(), odd%(), st(), ct(), dsfc(), atx%(), hicol%(), testin%()
SHARED map%(), fmap%(), cmap%(), bicol%(), sb1%(), ntx%, gm%, xb%(), yb%()
SHARED sizex%, sizey%, lowcol%(), bx, by, efa%, px, py, bsa%, sa, stt(), ctt()
bx = px: by = py: efa% = (sa + 1960) MOD 1800: bsa% = sa
bxx% = bx * 256: byy% = by * 256: TIMR = TIMER * 10: nttx% = 2 * ntx% + 1
sizexf% = sizex% * 256: sizeyf% = sizey% * 256

FOR x% = 0 TO 319
t% = (efa% + atx%(x%) + 1800) MOD 1800: xx% = x% \ 2

IF xx% = x% \ 2 THEN
rxx% = bxx%: ryy% = byy%: oinx% = rxx% \ 256: oiny% = ryy% \ 256
inx% = oinx%: iny% = oiny%: ysign% = SGN(yb%(t%)): xsign% = SGN(xb%(t%))
ys% = (1 - ysign%) \ 2: xs% = (1 - xsign%) \ 2
yss& = ys% * 256 - byy%: xss& = xs% * 256 - bxx%

'find dis & col
oldi: DO: rxx% = rxx% + xb%(t%): ryy% = ryy% + yb%(t%)
oinx% = inx%: oiny% = iny%
inx% = rxx% \ &H100: iny% = ryy% \ &H100
k% = map%(inx%, iny%)
chn2% = (inx% - oinx%) * xsign% + (iny% - oiny%) * ysign%
LOOP UNTIL chn2% = 2 OR k%
IF chn2% = 2 THEN
kx% = map%(oinx%, iny%)
ky% = map%(inx%, oiny%)
IF k% + kx% + ky% = 0 GOTO oldi
tst% = xsign% * ysign% * SGN((rxx% - bxx%) * (iny% * 256 + yss&) - (ryy% - byy%) * (inx% * 256 + xss&))
IF (tst% = 1 AND k% + ky% = 0) OR (tst% <= 0 AND k% + kx% = 0) GOTO oldi
END IF
horz% = 0: IF inx% = (rxx% - xb%(t%)) \ &H100 THEN horz% = chn2% AND 1

IF chn2% = 2 THEN
IF tst% > 0 THEN
IF ky% THEN k% = ky%: iny% = oiny% ELSE horz% = 1
ELSE
IF kx% THEN horz% = 1: k% = kx%: inx% = oinx%
END IF
END IF
END IF

IF horz% THEN
wdis(x%) = (iny% * 256 + yss&) / stt(t%)
IF t% > 1780 OR t% < 20 OR (t% > 880 AND t% < 920) THEN
dis = (inx% * 256 + xss&) / ctt(t%): IF dis > wdis(x%) THEN wdis(x%) = dis
END IF
xfrac = bx + wdis(x%) * ct(t%)
bcc% = INT((xfrac - INT(xfrac)) * 63.9): IF ys% = 0 THEN bcc% = 63 - bcc%
ELSE
wdis(x%) = (inx% * 256 + xss&) / ctt(t%)
IF (t% > 1330 AND t% < 1370) OR (t% > 430 AND t% < 470) THEN
dis = (iny% * 256 + yss&) / stt(t%): IF dis > wdis(x%) THEN wdis(x%) = dis
END IF
xfrac = by + wdis(x%) * st(t%)
bcc% = INT((xfrac - INT(xfrac)) * 63.9): IF xs% THEN bcc% = 63 - bcc%
END IF

dd% = dsfc(x%) / wdis(x%): odd%(x%) = dd%

'load view to buffer
IF x% AND 1 THEN
afx% = ctt(t%) * dsfc(x%): afy% = stt(t%) * dsfc(x%): yt% = dd% + 1
fixfloor:
IF yt% < 92 THEN
fcxp% = (bxx% + afx% \ yt%): fcyp% = (byy% + afy% \ yt%)
IF fcxp% <= 0 OR fcyp% <= 0 OR fcxp% >= sizexf% OR fcyp% >= sizeyf% THEN
sb1%(xx%, yt% + 99) = 0: sb1%(xx%, 100 - yt%) = 0: yt% = yt% + 1: GOTO fixfloor
END IF
END IF
FOR y% = yt% TO 92
fcxp% = (bxx% + afx% \ y%): fcx% = fcxp% \ &H100
fcyp% = (byy% + afy% \ y%): fcy% = fcyp% \ &H100
flor% = fmap%(fcx%, fcy%)
IF flor% > 0 THEN
sb1%(xx%, y% + 99) = bicol%(flor%)
ELSEIF flor% >= -ntx% THEN
sb1%(xx%, y% + 99) = (testin%(-flor%, (fcxp% \ 4) AND &H3F, (fcyp% \ 4) AND &H3F) AND &HFF) + hicol%(testin%(-flor%, (fcxp% \ 4) AND &H3F, (fcyp% \ 4) AND &H3F) AND &HFF)
ELSE
flor% = -flor% - ntx% - 1
fcxp% = (fcxp% \ 4) AND &H3F: fcyp% = (fcyp% \ 4) AND &H3F
tst% = (testin%(flor%, fcxp%, fcyp%) AND &HFF00)
sb1%(xx%, y% + 99) = lowcol%((testin%(flor%, fcxp%, fcyp%) AND &HFF00) \ 256) + tst%
END IF
sb1%(xx%, 100 - y%) = bicol%(cmap%(fcx%, fcy%))
NEXT
END IF
IF k% = nttx% + 1 THEN k% = 0
IF k% > nttx% THEN
kx% = k%: IF k% = 17 THEN kx% = INT(TIMR + xfrac * 40) AND &HFF
yb% = 99 + dd%: IF yb% > 191 THEN yb% = 191
yt% = 100 - dd%: IF yt% < 8 THEN yt% = 8
IF x% AND 1 THEN
FOR y% = yt% TO yb%: sb1%(xx%, y%) = (sb1%(xx%, y%) AND &HFF) + hicol%(kx%): NEXT
ELSE
FOR y% = yt% TO yb%: sb1%(xx%, y%) = (sb1%(xx%, y%) AND &HFF00) + kx%: NEXT
END IF
ELSEIF x% AND 1 THEN
IF dd% > 31 THEN
hmd% = 100 - dd%: df% = (dd% + 4) \ 32: dof& = dd%: kx% = k% - ntx% - 1
FOR yfrac% = 0 TO 63: yt% = hmd% + (yfrac% * dof&) \ &H20: yb% = yt% + df%
IF yt% < 8 THEN yt% = 8
IF yb% > &HBF THEN yb% = &HBF
IF k% <= ntx% THEN
tst% = hicol%(testin%(k%, bcc%, yfrac%) AND &HFF) + (testin%(k%, obcc%, yfrac%) AND &HFF)
ELSE
tst% = (testin%(kx%, bcc%, yfrac%) AND &HFF00) + lowcol%((testin%(kx%, obcc%, yfrac%) AND &HFF00) \ 256)
END IF
FOR y% = yt% TO yb%: sb1%(xx%, y%) = tst%: NEXT: NEXT
ELSE
yb% = 2 * dd% - 1: hmd% = 100 - dd%
IF k% <= ntx% THEN
FOR y% = hmd% TO 99 + dd%: yfrac% = ((y% - hmd%) * 63) \ yb%
sb1%(xx%, y%) = hicol%(testin%(k%, bcc%, yfrac%) AND &HFF) + (testin%(k%, obcc%, yfrac%) AND &HFF)
NEXT
ELSE
kx% = k% - ntx% - 1
FOR y% = hmd% TO 99 + dd%: yfrac% = ((y% - hmd%) * 63) \ yb%
sb1%(xx%, y%) = (testin%(kx%, bcc%, yfrac%) AND &HFF00) + lowcol%((testin%(kx%, obcc%, yfrac%) AND &HFF00) \ 256)
NEXT
END IF
END IF
END IF
obcc% = bcc%: NEXT
END SUB

SUB readassembly
SHARED kbcontrol%(), kbmatrix%(), asm$
RESTORE kbisrdata: DEF SEG = VARSEG(kbcontrol%(0)): i& = 0: GOTO skip0
DO: POKE i&, q%: i& = i& + 1
skip0: READ q%: LOOP WHILE q% > -1: i& = 16
n& = VARSEG(kbmatrix%(0)): L& = n& AND 255: h& = ((n& AND &HFF00) \ 256)
POKE i&, L&: POKE i& + 1, h&: i& = i& + 2
n& = VARPTR(kbmatrix%(0)): L& = n& AND 255: h& = ((n& AND &HFF00) \ 256)
POKE i&, L&: POKE i& + 1, h&: i& = i& + 2
FOR t = 1 TO 28: READ a: asm$ = asm$ + CHR$(a): NEXT
DEF SEG
END SUB

SUB showbadguy (b%)
SHARED bgh%(), scbg(), x(), y(), dis(), F0%, F1%, F2%, F3%, F4%, F5%, wdis()
IF bgh%(b%) > 0 THEN
IF x(b%) >= 0 AND x(b%) <= 319 THEN
IF dis(b%) < wdis(x(b%)) THEN showb% = 1
END IF
xt% = x(b%) + scbg(b%) * 40
IF xt% >= 0 AND xt% < 320 THEN
IF dis(b%) < wdis(xt%) THEN showb% = 1
END IF
IF showb% THEN
IF b% = 8 THEN F1% = 7
putbox x(b%) + scbg(b%) * 16, y(b%) + 0, x(b%) + scbg(b%) * 24, y(b%) + scbg(b%) * 2, F0%, dis(b%)
putbox x(b%) + scbg(b%) * 15, y(b%) + scbg(b%) * 2, x(b%) + scbg(b%) * 25, y(b%) + scbg(b%) * 10, F1%, dis(b%)
putbox x(b%) + scbg(b%) * 10, y(b%) + scbg(b%) * 10, x(b%) + scbg(b%) * 30, y(b%) + scbg(b%) * 40, b%, dis(b%)
putbox x(b%), y(b%) + scbg(b%) * 11, x(b%) + scbg(b%) * 10, y(b%) + scbg(b%) * 20, b%, dis(b%)
putbox x(b%) + scbg(b%) * 30, y(b%) + scbg(b%) * 11, x(b%) + scbg(b%) * 40, y(b%) + scbg(b%) * 20, b%, dis(b%)
putbox x(b%), y(b%) + scbg(b%) * 20, x(b%) + scbg(b%) * 5, y(b%) + scbg(b%) * 40, b%, dis(b%)
putbox x(b%) + scbg(b%) * 35, y(b%) + scbg(b%) * 20, x(b%) + scbg(b%) * 40, y(b%) + scbg(b%) * 40, b%, dis(b%)
putbox x(b%) + scbg(b%) * 10, y(b%) + scbg(b%) * 40, x(b%) + scbg(b%) * 18, y(b%) + scbg(b%) * 70, F3%, dis(b%)
putbox x(b%) + scbg(b%) * 22, y(b%) + scbg(b%) * 40, x(b%) + scbg(b%) * 30, y(b%) + scbg(b%) * 70, F3%, dis(b%)
putbox x(b%) + scbg(b%) * 18, y(b%) + scbg(b%) * 40, x(b%) + scbg(b%) * 22, y(b%) + scbg(b%) * 50, F3%, dis(b%)
putbox x(b%) + scbg(b%) * 7, y(b%) + scbg(b%) * 70, x(b%) + scbg(b%) * 18, y(b%) + scbg(b%) * 75, F4%, dis(b%)
putbox x(b%) + scbg(b%) * 22, y(b%) + scbg(b%) * 70, x(b%) + scbg(b%) * 33, y(b%) + scbg(b%) * 75, F4%, dis(b%)
putbox x(b%) + scbg(b%) * 5, y(b%) + scbg(b%) * 35, x(b%) + scbg(b%) * 15, y(b%) + scbg(b%) * 40, F1%, dis(b%)
putbox x(b%) + scbg(b%) * 25, y(b%) + scbg(b%) * 35, x(b%) + scbg(b%) * 35, y(b%) + scbg(b%) * 40, F1%, dis(b%)
putbox x(b%) + scbg(b%) * 15, y(b%) + scbg(b%) * 25, x(b%) + scbg(b%) * 25, y(b%) + scbg(b%) * 35, F5%, dis(b%)
putbox x(b%) + scbg(b%) * 16, y(b%) + scbg(b%) * 3, x(b%) + scbg(b%) * 18, y(b%) + scbg(b%) * 4, 0, dis(b%)
putbox x(b%) + scbg(b%) * 22, y(b%) + scbg(b%) * 3, x(b%) + scbg(b%) * 24, y(b%) + scbg(b%) * 4, 0, dis(b%)
putbox x(b%) + scbg(b%) * 16, y(b%) + scbg(b%) * 4, x(b%) + scbg(b%) * 18, y(b%) + scbg(b%) * 4, 7, dis(b%)
putbox x(b%) + scbg(b%) * 22, y(b%) + scbg(b%) * 4, x(b%) + scbg(b%) * 24, y(b%) + scbg(b%) * 4, 7, dis(b%)
putbox x(b%) + scbg(b%) * 17, y(b%) + scbg(b%) * 4, x(b%) + scbg(b%) * 17, y(b%) + scbg(b%) * 4, 0, dis(b%)
putbox x(b%) + scbg(b%) * 23, y(b%) + scbg(b%) * 4, x(b%) + scbg(b%) * 23, y(b%) + scbg(b%) * 4, 0, dis(b%)
putbox x(b%) + scbg(b%) * 20, y(b%) + scbg(b%) * 5, x(b%) + scbg(b%) * 20, y(b%) + scbg(b%) * 6, 114, dis(b%)
putbox x(b%) + scbg(b%) * 18, y(b%) + scbg(b%) * 8, x(b%) + scbg(b%) * 22, y(b%) + scbg(b%) * 8, 4, dis(b%)
F1% = 6
END IF
END IF
END SUB

SUB showbadshot (x%)
SHARED bgsht(), bgshosht%(), bgdela%(), bgshtht%(), bgshtdis()
IF bgsht(x%) > 0 AND bgshosht%(x%) THEN
putcircle bgdela%(x%), 100, bgshtht%(x%), 4 - (x% = 8), bgshtdis(x%)
END IF
END SUB

SUB showhealth
SHARED gm%, ogm%, ph%, oph%
IF gm% THEN ph% = 100
IF ph% - oph% OR gm% - ogm% THEN
FOR y% = 194 TO 199
hLINE 0, 319 * ph% / 100, y%, 1 + 14 * gm%
hLINE 319 * ph% / 100 + 1, 319, y%, 4
NEXT: ogm% = gm%: oph% = ph%
END IF
END SUB

SUB showmed (b%)
SHARED med%(), scmed(), mx(), my(), medis(), nmeds%
IF med%(b%) THEN
c% = (b% < nmeds%)
putbox mx(b%) + 0, my(b%) + 0, mx(b%) + scmed(b%) * 20, my(b%) + scmed(b%) * 20, 2 - 13 * c%, medis(b%)
putbox mx(b%) + scmed(b%) * 8, my(b%) + scmed(b%) * 3, mx(b%) + scmed(b%) * 13, my(b%) + scmed(b%) * 17, 2 - 2 * c%, medis(b%)
putbox mx(b%) + scmed(b%) * 3, my(b%) + scmed(b%) * 8, mx(b%) + scmed(b%) * 17, my(b%) + scmed(b%) * 13, -4 * c%, medis(b%)
END IF
END SUB

SUB showurshot (x%)
SHARED mg%, fb%, sht(), shosht%(), dela%(), shtdis(), shtht%()
IF ((mg% AND fb% = 0) OR x%) AND sht(x%) > 0 AND shosht%(x%) THEN putcircle dela%(x%), 100 + 30 / shtdis(x%), shtht%(x%) / 3 + 1, 0, shtdis(x%)
IF ((mg% = 0 OR fb%) AND x% = 0) AND sht(x%) > 0 AND shosht%(x%) THEN putcircle dela%(x%), 100 + 10 / shtdis(x%), shtht%(x%) * 1.5, 13, shtdis(x%)
END SUB

SUB time
SHARED ofram%, delta.t, fdt, kbmatrix%(), gm%, fram%
STATIC oldtimer&, oldtim, afram%, godit

fram% = fram% + 1
IF INT(TIMER) - oldtimer& THEN
ofram% = fram%: fram% = 0: oldtimer& = INT(TIMER)
END IF

afram% = afram% + 1
IF oldtim <> TIMER THEN
delta.t = delta.t * .8 + (TIMER - oldtim) * .2 / afram%
oldtim = TIMER: afram% = 0
IF delta.t > .1 OR delta.t < 0 THEN delta.t = .1
fdt = 14 * delta.t
END IF

IF kbmatrix%(36) AND TIMER > godit THEN
IF gm% THEN gm% = 0 ELSE gm% = 1: 'cheat mode
godit = (TIMER + 1) MOD 86400
END IF

END SUB

SUB vline (x%, yt%, yb%, c%)
STATIC y%, xx%
SHARED sb1%(), hicol%(), odd%(), bicol%(), bitex%: xx% = x% \ 2
IF yt% < 8 THEN yt% = 8
IF yb% > 191 THEN yb% = 191
IF bitex% THEN
FOR y% = yt% TO yb%: sb1%(xx%, y%) = bicol%(c%): NEXT
ELSEIF x% AND 1 THEN
FOR y% = yt% TO yb%
sb1%(xx%, y%) = (sb1%(xx%, y%) AND &HFF) + hicol%(c%): NEXT
ELSE
FOR y% = yt% TO yb%: sb1%(xx%, y%) = (sb1%(xx%, y%) AND &HFF00) + c%: NEXT
END IF
END SUB

SUB yourmove
SHARED kbmatrix%(), ct(), st(), efa%, shift, delta.t, fdt
SHARED px, py, sa, va, vx, vy, testin%(), bx, by
IF kbmatrix%(56) THEN
IF kbmatrix%(77) THEN
vx = vx + ct((efa% + 450) MOD 1800) * shift * delta.t
vy = vy + st((efa% + 450) MOD 1800) * shift * delta.t
END IF
IF kbmatrix%(75) THEN
vx = vx + ct((efa% + 1350) MOD 1800) * shift * delta.t
vy = vy + st((efa% + 1350) MOD 1800) * shift * delta.t
END IF
ELSE
IF kbmatrix%(77) THEN va = va + shift * 90 * delta.t
IF kbmatrix%(75) THEN va = va - shift * 90 * delta.t
END IF
IF kbmatrix%(72) THEN
vx = vx + ct(efa%) * shift * delta.t
vy = vy + st(efa%) * shift * delta.t
END IF
IF kbmatrix%(80) THEN
vx = vx - ct(efa%) * shift * delta.t
vy = vy - st(efa%) * shift * delta.t
END IF
svx% = SGN(vx): svy% = SGN(vy)
crashtest px + .15 * svx%, py + .15 * svy%, vx, vy
crashtest px - .15 * svx%, py + .15 * svy%, vx, vy
crashtest px + .15 * svx%, py - .15 * svy%, vx, vy
px = px + vx * delta.t: py = py + vy * delta.t
sa = (sa + va * delta.t) MOD 1800
damp = 2 ^ -fdt
vx = vx * damp: vy = vy * damp: va = va * damp
testin%(4, INT(bx) + 2, INT(by) + 19) = 0
END SUB

SUB yourshot
SHARED kbmatrix%(), nshots%, weap$, sht(), ammo%(), shosht%(), bx, by, mg%
SHARED fdt, delta.t, snd%, fb%, ct(), st(), vshx(), vshy(), maxshots%
SHARED sizex%, sizey%, shtx(), shty(), map%(), inx%, iny%, testin%()
SHARED shtang%(), shtdis(), dis(), dela%(), shtht%(), fmap%(), efa%, sa
SHARED nbguys%, nbguysm1%, bgh%(), bgx(), bgy(), vbx(), vby(), fire, kills%
STATIC kk%

IF fire > 0 THEN fire = fire - fdt * nshots%

IF kbmatrix%(2) THEN mg% = 0: kk% = 0: nshots% = 1: weap$ = " plasma gun"
IF kbmatrix%(3) THEN mg% = 1: nshots% = 10: weap$ = "machine gun"

IF kbmatrix%(29) AND fire <= 0 AND sht(kk%) <= 0 AND ammo%(mg%) > 0 THEN
sht(kk%) = 20: shosht%(kk%) = 1: ammo%(mg%) = ammo%(mg%) - 1: 'create shot
shtx(kk%) = bx: shty(kk%) = by: fire = 18: IF snd% THEN SOUND 200, 1
vshx(kk%) = ct(efa%) * 10
vshy(kk%) = st(efa%) * 10
kk% = kk% + 1: IF kk% = nshots% THEN kk% = 0
IF mg% - 1 THEN fb% = 1
END IF

FOR x% = 0 TO maxshots%
IF shtx(x%) < 1 OR shtx(x%) > sizex% - 1 OR shty(x%) < 0 OR shty(x%) > sizey% - 1 THEN shosht%(x%) = 0
IF sht(x%) > 0 THEN sht(x%) = sht(x%) - fdt
IF sht(x%) > 0 AND shosht%(x%) THEN
crashtest shtx(x%), shty(x%), vshx(x%), vshy(x%)
k% = map%(inx%, iny%)
IF k% THEN shosht%(x%) = 0
shtx(x%) = shtx(x%) + vshx(x%) * delta.t: shty(x%) = shty(x%) + vshy(x%) * delta.t
IF k% = 15 AND sht(x%) > 0 THEN
map%(inx%, iny%) = 0
testin%(4, inx% + 2, iny% + 19) = 0
END IF

shtang%(x%) = atan2(shty(x%) - by, shtx(x%) - bx) * 5
shtdis(x%) = SQR((shty(x%) - by) ^ 2 + (shtx(x%) - bx) ^ 2 + .01)
dis(x% + nbguys% * 2) = shtdis(x%)
dela%(x%) = (shtang%(x%) - sa + 1800) MOD 1800
shtht%(x%) = 30 / shtdis(x%)

'fix damage test
FOR y% = 0 TO nbguysm1%
bsdis = (shty(x%) - bgy(y%)) * (shty(x%) - bgy(y%)) + (shtx(x%) - bgx(y%)) * (shtx(x%) - bgx(y%))
IF bsdis < .36 AND bgh%(y%) > 0 THEN
IF bsdis < .16 THEN bgh%(y%) = bgh%(y%) - sht(x%) / 2 - 5: shosht%(x%) = 0
vbx(y%) = vbx(y%) + vshx(x%) * .1: vby(y%) = vby(y%) + vshy(x%) * .1
IF fb% = 1 AND x% = 0 THEN
bgh%(y%) = bgh%(y%) - sht(x%) * 1.5 - 50: shosht%(x%) = 0
vbx(y%) = vbx(y%) + vshx(x%) * .5: vby(y%) = vby(y%) + vshy(x%) * .5
END IF
IF bgh%(y%) < 1 THEN
fmap%(INT(bgx(y%)), INT(bgy(y%))) = 4 - 4 * (y% = 8): kills% = kills% + 1
IF snd% THEN SOUND 180, 5
END IF
END IF: NEXT
END IF: NEXT
IF sht(0) < 0 THEN fb% = 0

END SUB

