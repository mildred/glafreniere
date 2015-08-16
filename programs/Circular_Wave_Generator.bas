' This is a FreeBasic program. Gabriel LaFreniere, Oct. 2, 2007. Updated Oct. 3, 2007. 
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
Dim As Single pi = 4 * Atn(1), x, t
Gosub Initialize

Do
  Swap page1, page2
  Screenset page1, page2
  screensync
  Pcopy 2, page1
  image = image + 1
  If image > images Then image = 1
  t = 2 * pi * image / images

'***************************************** ROTATION ******************************************
  
  For pixel = 0 To display.width
    x = Abs(2 * pi * (x.center - pixel) / lambda)
    If x < pi / 2 Then x = x + (pi / 4) * (1 - (2 * x / pi)) ^ 2
    amplitude = 100 * Sin(x + pi / 4 - t) / Sqr(x)
    Line(pixel,y.previous)-(pixel,y.center-amplitude),black
    y.previous = y.center - amplitude
  Next

  key$ = Right(Ucase(Inkey),1)
  If key$ = Chr(27) Or key$ = "K" Then End
  If key$ = "P" Then Sleep: key$ = Inkey
  Locate 7,16: ? image
Loop

Initialize:
Screen 19,24,3
page1 = 1
images = 150
lambda = 100
y.center = 450
display.width = 799
x.center = display.width / 2
red = Rgb(255,0,0)
green = Rgb(0,150,0)
gray = Rgb(175,175,175)
white = Rgb(255,255,255)
background = Rgb(225,225,225)
Screenset 2, 2: Color black, background: Cls
Windowtitle "The Circular Wave Generator"
Locate 5,3:  ? "t = 2 * pi * image / images"
Locate 7,3:  ? "t = 2 * pi *       /  150"
Locate 9,3:  ? "x = 2 * pi * distance / lambda"
Locate 12,3: ? "Correction while x < pi / 2:"
Locate 14,3: ? "x = x + (pi / 4) * (1 - (2 * x / pi)) ^ 2"
Locate 2,68: ? "y = Sin(x + pi / 4) / Sqr(x)"
Line(490, 24)-(525, 25), red, b
Locate 4,68: ? "y = Sin(x + pi / 4 - t) / Sqr(x)"
Line(490, 56)-(525, 57), black, b
Locate 18,61:? "Here, the wave medium is a 2-D space,"
Locate 19,61:? "for example the air layer between two"
Locate 20,61:? "parallel planes. The layer thickness"
Locate 21,61:? "should be smaller than lambda / 10."
Locate 18,3:? "The goal is to display a pi / 4 phase"
Locate 19,3:? "offset in the center in order to expand"
Locate 20,3:? "the concentric waves to an additional"
Locate 21,3:? "lambda / 8 position, because the 2-D"
Locate 22,3:? "central antinode width is 3/4 lambda."
Color black, white
Line(16,11)-(262,35), white, bf
Line(15,10)-(263,36), green, b
Line(16,11)-(262,35), green, b
Locate 2,5: ? "The Circular Wave Generator"
Color green, background
Locate 35,2: Print "Pause: press P."
Locate 36,2: Print "Press Esc to quit.      Oct. 3, 2007."
Locate 37,2: Print "Gabriel LaFreniere      glafreniere.com";
Locate 36,61:Print "The source code (freebasic.net) may be"
Locate 37,61:Print "freely distributed, copied or modified.";
Color black, background
'----------------------------------- AMPLITUDE TEMPLATE ---------------------------------------
For pixel = 0 To display.width
  x = Abs(2 * pi * (x.center - pixel) / lambda)
  If x < pi / 2 Then x = x + (pi / 4) * (1 - (2 * x / pi)) ^ 2
  y = 100 * Sqr((Sin(x)/Sqr(x))^2 + (Cos(x)/Sqr(x))^2)
  Line(pixel,y.center-y)-(pixel,y.center+y), white            'white template
  Line(pixel,y.center+y.previous+1)-(pixel,y.center+y+1),gray 'gray border
  Line(pixel,y.center-y.previous-1)-(pixel,y.center-y-1),gray
  Line(pixel,y.center+y.previous+2)-(pixel,y.center+y+2),gray
  Line(pixel,y.center-y.previous-2)-(pixel,y.center-y-2),gray
  y.previous = y
Next
y.previous = y.center
For pixel = 0 To display.width
  x = Abs(2 * pi * (x.center - pixel) / lambda)
  If x < pi / 2 Then x = x + (pi / 4) * (1 - (2 * x / pi)) ^ 2
  If x Then amplitude = 100 * Sin(x + (pi / 4)) / Sqr(x)  'Marcotte's formula is: sin(x)/x
  Line(pixel,y.previous)-(pixel,y.center-amplitude), red  'red curve
  y.previous = y.center - amplitude
Next
For pixel = 0 To display.width Step lambda / 4            'wavelength marks
  Line(pixel, y.center - 6)-(pixel, y.center + 6), gray
Next
For pixel = 0 To display.width Step lambda
  Line(pixel, y.center - 12)-(pixel, y.center + 12), black
Next
Line(0, y.center)-(display.width, y.center), black        'axis
Return
