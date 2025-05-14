CLS
x = 160
y = 160
speed = 4
'define colours
cf = 0 'foreground
cb = 1 'background
Dim colours(16)
colours(0) = RGB(white)
colours(1) = RGB(black)
colours(2) = RGB(blue)
colours(3) = RGB(green)
colours(4) = RGB(cyan)
colours(5) = RGB(red)
colours(6) = RGB(magenta)
colours(7) = RGB(yellow)
colours(8) = RGB(brown)
colours(9) = RGB(orange)
colours(10) = RGB(pink)
colours(11) = RGB(gold)
colours(12) = RGB(salmon)
colours(13) = RGB(beige)
colours(14) = RGB(lightgrey)
colours(15) = RGB(grey)

'Print help
Colour colours(cf)
Text 0,0,"(q)uit u/i:bg o/p:fg speed=4"

'Print colour-box
Box 309, 0, 10, 10,,,colours(cf)

'Framebuffer for foreground
'FRAMEBUFFER create
'FRAMEBUFFER write n 'screen
'Box 0, 15, 320, 310,,,colours(white)

'main loop
Do
  Pixel x, y
  a$ = Inkey$ 'get keypress
  'display number of key pressed
  'If Asc(a$) <> 0 Then Print Asc(a$)
  Select Case Asc(a$)
    Case 113 'q
      End
    Case 49 '1,
      speed = 1
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=1"
    Case 50 '2,
      speed = 2
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=2"
    Case 51 '3,
      speed = 3
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=3"
    Case 52 '4,
      speed = 4
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=4"
    Case 53 '5,
      speed = 5
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=5"
    Case 54 '6,
      speed = 6
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=6"
    Case 55 '7,
      speed = 7
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=7"
    Case 56 '8,
      speed = 8
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=8"
    Case 57 '9,
      speed = 9
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=9"
    Case 48 '0,
      speed = 10
      Text 0,0,"(q)uit u/i:bg o/p:fg speed=10"
    Case 128 'up
      y = y - speed
      If y > 12 Then
        Line x,y+speed,x,y
      Else
        Line x,y+speed,x,12
        y = 12
      End If
    Case 129 'down
      y = y + speed
      If y < 319 Then
        Line x,y-speed,x,y
      Else
        Line x,y-speed,x,319
        y = 319
      End If
    Case 130 'left
      x = x - speed
      If x > 0 Then
        Line x+speed,y,x,y
      Else
        Line x+speed,y,0,y
        x = 0
      End If
    Case 131 'right
      x = x + speed
      If x < 319 Then
        Line x-speed,y,x,y
      Else
        Line x-speed,y,319,y
        x = 319
      End If
    Case 112 'p, next foreground color
      cf = cf + 1
      If cf > 15 Then cf = 0
      Colour colours(cf)
      'fill colour-box
      Box 309, 0, 10, 10,,,colours(cf)
    Case 111 'o, prev. foreground color
      cf = cf - 1
      If cf < 0 Then cf = 15
      Colour colours(cf)
      'fill colour-box
      Box 309, 0, 10, 10,,,colours(cf)
    Case 117 'u, prev. background color
      cb = cb - 1
      If cb < 0 Then cb = 15
      Colour colours(cb)
      'FRAMEBUFFER copy f, n 'scrn-mem
      'FRAMEBUFFER write f 'screen
      Box 0, 15, 320, 310,,,colours(cb)
      Colour colours(cf)
      'FRAMEBUFFER copy n, f 'mem-scrn
    Case 105 'i, next background color
      cb = cb + 1
      If cb > 15 Then cb = 0
      Colour colours(cb)
      Box 0, 15, 320, 310,,,colours(cb)
      Colour colours(cf)
  End Select
Loop Until Asc(a$) = 13 'stop when 'q'
'FRAMEBUFFER write f 'screen
