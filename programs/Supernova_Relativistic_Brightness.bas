Width 80,20:Color 0,15:Cls:?
? " Created May 18, 2010 by Gabriel LaFreniere.":?:?
? " This FreeBasic program is compatible with the FreeBasic 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?

' Gosub commands are not supported.
' All variables must be declared.

Const pi = 4 * Atn(1)
Const black = 0, white = -1, purple = -65281, gray = -6908266, yellow = -256, blue = -16751361
Const red = -65536, green = -16726016, cyan = -16725816, dark = 1, bright = -257
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), dark_gray = Rgb(50,50,50)
Dim Shared As Integer margin, scale, y_origin
Dim Shared As Single x, y, beta, g_Lorentz, brightness, redshift

Screen 20,24,3
Windowtitle " The Supernova Relativistic Brightness"
Color black, background: Cls
margin = 100
scale = 16 * 30
y_origin = scale + margin
Line(0 + margin, 0 + margin)-(scale + margin, scale + margin), black, bf
For x = 0 To scale Step scale / 10
  Line(x + margin, 0 + margin)-(x + margin, scale + margin), gray
Next
For y = 0 To scale Step scale / 10
  Line(0 + margin, y + margin)-(scale + margin, y + margin), gray
Next
For x = 0 To scale Step .125
  beta = x / scale
  g_Lorentz = Sqr(1 - beta ^ 2)
  redshift = scale * (1 + beta) / 10
  Circle(x + margin, y_origin - redshift), 1, blue, 1
  Paint(x + margin, y_origin - redshift), blue
  If x < .980 * scale Then
    redshift = scale * (1 + beta) / g_Lorentz / 10
    Circle(x + margin, y_origin - redshift), 1, purple, 1
    Paint(x + margin, y_origin - redshift), purple
  End If
Next
For x = .275 * scale To scale Step .25
  beta = x / scale
  g_Lorentz = Sqr(1 - beta ^ 2)
  brightness = 1 / (1 + beta) / beta ^ 2
  brightness = scale / 10 * brightness
  If x > .28 * scale Then
    Circle(x + margin, y_origin - brightness), 1, green, 1
    Paint(x + margin, y_origin - brightness), green
  End If
  brightness = g_Lorentz / (1 + beta) / beta ^ 2
  brightness = scale / 10 * brightness
  Circle(x + margin, y_origin - brightness), 1, red, 1
  Paint(x + margin, y_origin - brightness), red
Next
x = 298: y = 56
Line(x-14, y-7)-(x+14, y), black
Line(x-14, y+7)-(x+14, y), black
Line(x-14, y-7)-(x-14, y+7), black
Paint(x, y), black
Line(x-95,y-1)-(x,y+1), black, bf
x = 448: y = 632
Line(x-14, y-7)-(x+14, y), black
Line(x-14, y+7)-(x+14, y), black
Line(x-14, y-7)-(x-14, y+7), black
Paint(x, y), black
Line(x-95,y-1)-(x,y+1), black, bf
x = 48: y = 440
Line(x-7, y+14)-(x, y-14), black
Line(x+7, y+14)-(x, y-14), black
Line(x-7, y+14)-(x+7, y+14), black
Paint(x, y), black
Line(x-1,y+95)-(x+1,y), black, bf

Locate 06, 13: Print "0    .1    .2    .3    .4    .5    .6    .7    .8    .9     c"
Locate 38, 13: Print "0    1.37  2.74  4.11  5.48  6.85  8.22  9.59  11.0  12.3  13.7"
Locate 04, 13: Print "Beta (v / c)"
Locate 04, 59: Print "glafreniere.com"
Locate 35, 02: Print " Redshift"
Locate 36, 02: Print "    or"
Locate 37, 02: Print "Brightness"
Locate 40, 13: Print "Billion light-years (uncertain)"
Locate 42, 02: Print "Regular redshift (blue curve):   1 + beta"
Locate 43, 02: Print "Relativistic redshift (purple): (1 + beta) / g      g = Sqr(1 - beta ^ 2)"
Locate 45, 02: Print "Supernova regular brightness (green curve):    1 / (1 + beta) / beta ^ 2"
Locate 46, 02: Print "Supernova relativistic brightness (red curve): g / (1 + beta) / beta ^ 2"
Locate 10, 12: Print "9"
Locate 13, 12: Print "8"
Locate 16, 12: Print "7"
Locate 19, 12: Print "6"
Locate 22, 12: Print "5"
Locate 25, 12: Print "4"
Locate 28, 12: Print "3"
Locate 31, 12: Print "2"
Locate 34, 12: Print "1"
Sleep: End
