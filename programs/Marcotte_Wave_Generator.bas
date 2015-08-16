' This is a FreeBasic program. Gabriel LaFreniere, Sept. 20, 2007. Updated Oct. 3, 2007. 
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
    If x < pi Then x = x + (pi / 2) * (1 - x / pi) ^ 2    'correction while x < pi / 2
    amplitude = 200 * Sin(x - t) / x
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
Windowtitle "The Marcotte Wave Generator"
Locate 5,3:  ? "t = 2 * pi * image / images"
Locate 7,3:  ? "t = 2 * pi *       /  150"
Locate 9,3:  ? "x = 2 * pi * distance / lambda"
Locate 12,3: ? "Correction while x < pi:"
Locate 14,3: ? "x = x + (pi / 2) * (1 - x / pi) ^ 2"
Locate 2,68: ? "y = sin(x) / x"
Line(490, 24)-(525, 25), red, b
Locate 4,68: ? "y = sin(x - t) / x"
Line(490, 56)-(525, 57), black, b
Locate 6,62: ? "N.B. If x = 0, y = 1 or y = sin(-t)"
Locate 8,62: ? "One may think that the black curve"
Locate 9,62: ? "should follow the red one. However,"
Locate 10,62:? "this is not a standing wave system"
Locate 11,62:? "but rather a progressive one. Its"
Locate 12,62:? "amplitude in the center is related"
Locate 13,62:? "to the emitter's structure, which"
Locate 14,62:? "for instance could be a sphere."
Locate 16,62:? "The goal was to display a pi / 2"
Locate 17,62:? "phase offset in the center in order"
Locate 18,62:? "to expand all the concentric waves"
Locate 19,62:? "to an additional lambda / 4 position."
Locate 20,62:? "This is how the electron behaves, and"
Locate 21,62:? "this will allow searchers to generate"
Locate 22,62:? "perfectly accurate fields of force,"
Locate 23,62:? "which are powerful standing waves set"
Locate 24,62:? "up between electrons or positrons."
Locate 17,3: ? "The wave curve is amazingly smooth."
Locate 18,3: ? "In addition, Mr. Jocelyn Marcotte's"
Locate 19,3: ? "formula is the simplest one."
Locate 21,3: ? "Because this formula works everywhere"
Locate 22,3: ? "from the center to infinity, this"
Locate 23,3: ? "spherical wave generator bears the"
Locate 24,3: ? "name of its discoverer."
Color black, white
Line(16,11)-(262,35), white, bf
Line(15,10)-(263,36), green, b
Line(16,11)-(262,35), green, b
Locate 2,5: ? "The Marcotte Wave Generator"
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
  If x < pi Then x = x + (pi / 2) * ((pi - x) / pi)^2     'correction while x < pi / 2
  y = 200 * sqr((Sin(x) / x)^2 + (Cos(x) / x)^2)
  Line(pixel,y.center-y)-(pixel,y.center+y), white        'white template
  Line(pixel,y.center+y.previous+1)-(pixel,y.center+y+1),gray 'gray border
  Line(pixel,y.center-y.previous-1)-(pixel,y.center-y-1),gray
  Line(pixel,y.center+y.previous+2)-(pixel,y.center+y+2),gray
  Line(pixel,y.center-y.previous-2)-(pixel,y.center-y-2),gray
  y.previous = y
Next
For pixel = 0 To display.width Step lambda / 4            'wavelength marks
  Line(pixel, y.center - 6)-(pixel, y.center + 6), gray
Next
For pixel = 0 To display.width Step lambda
  Line(pixel, y.center - 12)-(pixel, y.center + 12), black
Next
  Line(0, y.center)-(display.width, y.center), black      'axis
y.previous = y.center
For pixel = 0 To display.width                            'Marcotte's formula: sin(x)/x
  x = Abs(2 * pi * (x.center - pixel) / lambda)
' If x < pi Then x = x + (pi / 2) * (1 - x / pi) ^ 2      'this adjusts the red curve
  If x Then amplitude = 200 * Sin(x) / x
  Line(pixel,y.previous)-(pixel,y.center-amplitude), red  'red curve
  y.previous = y.center - amplitude
Next
Return
