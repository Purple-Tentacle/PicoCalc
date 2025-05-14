FRAMEBUFFER create
FRAMEBUFFER write f 'draw to memory
'create pattern of columns
For i = 0 To 320 Step 5
  r = Math(rand)*255
  g = Math(rand)*255
  b = Math(rand)*255
  Line i,0,i,320,,RGB(r,g,b)
  Line i+1,0,i+1,320,,RGB(r,g,b)
  Line i+2,0,i+2,320,,RGB(r,g,b)
  Line i+3,0,i+3,320,,RGB(r,g,b)
  Line i+4,0,i+4,320,,RGB(r,g,b)
Next i
FRAMEBUFFER copy F,N 'memory to screen
FRAMEBUFFER close 'free memory?????
FRAMEBUFFER write N 'draw to screen
'randomly draw columns
Do
  r = Math(rand)*255
  g = Math(rand)*255
  b = Math(rand)*255
  x = Math(rand)*320
  check = x Mod 5   'dividable by 5?
  If check = 0 Then 'dividable by 5!
    Line x,0,x,320,,RGB(r,g,b)
    Line x+1,0,x+1,320,,RGB(r,g,b)
    Line x+2,0,x+2,320,,RGB(r,g,b)
    Line x+3,0,x+3,320,,RGB(r,g,b)
    Line x+4,0,x+4,320,,RGB(r,g,b)
  EndIf
Loop
