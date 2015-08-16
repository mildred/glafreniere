' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
' This is a FreeBasic program. Gabriel LaFreniere, Sept. 10, 2007.
Dim As Single exterior.radius, interior.radius, amplitude, phase
Dim As Single x, y, t.time, pi, half.pi, double.pi, correction
Gosub Initialization

Do'------------------------------------- MAIN LOOP --------------------------------------------
  Swap work.page, visible.page                            'swap 1 <=> 0.
  Screenset work.page, visible.page                       'faster for graphics.
  Pcopy matrix.page, work.page
  screensync                                              'also useful as a speed regulator.
  Gosub Graphics
  If bitmap Then Gosub Bitmaps                            'bitmap sequence for animation.
  image = image + 1
  If image = images Then image = 0                        'reset.
Loop

Graphics:'------------------------------- GRAPHICS --------------------------------------------
Locate 6,15: Print Using "###"; image
t.time = double.pi * image / images                       'wave period according to the image.
previous.wave = scale * Sin(t.time - (pi / 2))
x.previous = x.center - scale * Cos(t.time - half.pi)     'the center is pi/2 shifted.
y.previous = y.center - scale * Sin(t.time - half.pi)

For x.pixel = 1 To half.width
  x = double.pi * x.pixel / lambda                        'distance (and phase) in radians.
  y = 1 / x                                               'amplitude, inverse of distance law.
  If x.pixel < half.lambda Then                           'correction: center pi/2 shifted.
    exterior.radius = (pi + x)^2.65                       'this is only an approximation
    interior.radius = (pi - x)^2.65                       'for the "v" volume.
    y = 41.5 / (exterior.radius + interior.radius)        'I NEED THE EXACT FORMULA.
    correction = half.pi * (1 - x / pi) ^ 2               'phase correction for the
    phase = t.time - x - correction                       'core, which is pi/2 shifted.
    sine.wave = scale * y * Sin(phase)
    dot.color = green                                     'green curve while x < pi.
  Else
    phase = t.time - x
    sine.wave = scale * y * Sin(phase)
    dot.color = violet                                    'violet curve while x > pi.
  End If
  
  Pset(x.center + x.pixel, y.center - scale * y), black   'black curve according to the volume.
  Pset(x.center - x.pixel, y.center - scale * y), black
  
  Line(x.center + x.pixel - 1, y.center - previous.wave)-(x.center + x.pixel, y.center - sine.wave),dot.color
  Line(x.center - x.pixel + 1, y.center - previous.wave)-(x.center - x.pixel, y.center - sine.wave),dot.color
  previous.wave = sine.wave
  x = x.center - scale * y * Cos(phase)
  y = y.center  - scale * y * Sin(phase)
  Line(x.previous, y.previous)-(x, y), dot.color
  x.previous = x
  y.previous = y
Next

a$ = Inkey                                                'keyboard.
If a$ = Chr(255)+ "k" Or a$ = Chr(27) Then End
a$ = Ucase(a$)
If a$ = "P" Then Sleep: a$ = "": b$ = Inkey               'pause.
If b$ = Chr(255)+ "k" Or b$ = Chr(27) Then End
If a$ = "S" Then sleep 100
do: loop while len(inkey)                                 'clear the keyboard buffer
Return

Bitmaps:'------------------------ CREATING A BITMAP SEQUENCE  ---------------------------------
Select Case capture
  Case Is < 10: number$ = "00"
  Case Is < 100: number$ = "0"
  Case Is < 1000: number$ = ""
End Select
Locate 34, 80: Print Using "Capture ## /"; capture;: Print captures
file$ = "capture" + number$ + Str(capture) + ".bmp"
Bsave file$,0
capture = capture + 1
If capture > captures Then End
Return

'------------------------------------- INITIALIZATION ----------------------------------------
Initialization:
Screen 19,24,3
work.page = 1
pi = 4 * Atn(1)
half.pi = 2 * Atn(1)
double.pi = 8 * Atn(1)
scale = 150                                               'scale or template.
lambda = 2 * scale / pi                                   'best equilibrium with amplitude.
half.lambda = lambda / 2
images = 256                                              'image number per period.
matrix.page = 2
display.width = 550
half.width = display.width / 2
x.center = half.width
y.center = scale
'bitmap = 1: capture = 1: captures = images               'for bitmap sequence.

white = Rgb(255,255,255)
gray = Rgb(150,150,150)
background = Rgb(225,225,225)
red = Rgb(255,0,0)
green = Rgb(0,175,0)
blue = Rgb(0,0,225)
violet = Rgb(200,0,255)
Color black, background
Screenset matrix.page, matrix.page: Cls
Color Rgb(0,150,0), background
Windowtitle "Spherical wave generator." 
If bitmap Then
  Locate 24, 15: Print "A bitmap sequence will be created."
  Locate 26, 15: Print "Press Enter to start."
End If
Locate 36,2: Print "Gabriel LaFreniere"
Locate 37,2: Print "Sept 10, 2007.    glafreniere.com";
Locate 36,58:Print "The source code (see freebasic.net) may be"
Locate 37,58:Print "freely distributed, copied or modified.";
Color black, background
Locate 2,2:  Print "x = 2 * pi * distance / lambda"
Locate 2,50: Print "y = 1 / x         Inverse of the distance law."
Line(355,24)-(385,25),blue,b
Locate 4,50: Print "y = 1 / v         Please find the correct v volume."
Line(355,56)-(385,57),black,b
Locate 6,50: Print "y = sin(x) / x    Jocelyn Marcotte's formula."
Line(355,88)-(385,89),red,b
Locate 8,68: Print "Sphere volume: (4/3)*pi*r^3"
Locate 4,2:  Print "t = 2 * pi * image / images"
Locate 6,2:  Print "t = 2 * pi *     /"; images - 1
Locate 12,6: Print "Rotation:"
Locate 14,6: Print "y = Sin(t - x) / x"
Line(4,217)-(34,218),violet,b
Locate 16,6: Print "y = Sin(t - x - corr) / v"
Line(4,249)-(34,250),green,b
Locate 14,42:Print "The spiral is the helicoidal transverse view."
Locate 16,42:Print "corr=(pi/2)*(1-x/pi)^2    Phase correction for x < pi."


Locate 22,2: Print "C.F. Quote from Mr. Milo Wolff's web site: http://members.tripod.com/mwolff/"
Locate 23,2: Print "   < Above is a photo of Milo comparing the waves of a particle with the layers of an onion.>"
Locate 25,2: Print "This program shows that the wave amplitude is really given by each layer volume."
Locate 26,2: Print "I succeeded in showing the curve to a good approximation. However, I could not find the"
Locate 27,2: Print "exact formula for the v volume because the energy distribution must be sinusoidal."
Locate 28,2: Print "This needs a differential calculus, albeit the energy may perhaps follow the gaussian"
Locate 29,2: Print "normal distribution:  y = pi ^ -(x ^ 2). The EXACT formula for v is missing!"
Locate 31,2: Print "P - Pause."
Locate 32,2: Print "S - Slow."
Locate 34,2: Print "Press Esc to quit."
If bitmap Then Sleep: If Inkey = Chr(13) Then Else bitmap = 0
y.previous = scale
y.blue.previous = 2000

For x.pixel = 0 To half.width
  x = double.pi * x.pixel / lambda                        'distance (or phase) in radians.
  If x.pixel > lambda / 4 Then                            'full lambda core below.
    y = scale / x                                         'inverse of the amplitude 1 / x.
    Line(x.center + x.pixel, y.center - y)-(x.center + x.pixel, y.center + y),white
    Line(x.center - x.pixel, y.center - y)-(x.center - x.pixel, y.center + y),white
    Line(x.center - x.pixel + 1, y.center + y.blue.previous)-(x.center - x.pixel, y.center + y), blue
    Line(x.center + x.pixel - 1, y.center + y.blue.previous)-(x.center + x.pixel, y.center + y), blue
    y.blue.previous = y
    y = scale * Sin(x) / x                                'Jocelyn Marcotte's electron curve.
    Line(x.center + x.pixel - 1, y.center - y.previous)-(x.center + x.pixel, y.center - y), red
    Line(x.center - x.pixel + 1, y.center - y.previous)-(x.center - x.pixel, y.center - y), red
  Else
    If x Then y = scale / x Else y = 2000: y.blue.previous = 2000  'amplitude curve.
    If x.pixel > 15 Then
    Line(x.center - x.pixel + 1, y.center + y.blue.previous)-(x.center - x.pixel, y.center + y), blue
    Line(x.center + x.pixel - 1, y.center + y.blue.previous)-(x.center + x.pixel, y.center + y), blue
    End If
    y.blue.previous = y
    If x.pixel Then y = scale * Sin(x) / x Else y = scale
    Line(x.center + x.pixel, y.center - y)-(x.center + x.pixel, y.center + y),white
    Line(x.center - x.pixel, y.center - y)-(x.center - x.pixel, y.center + y),white
    Line(x.center + x.pixel - 1, y.center - y.previous)-(x.center + x.pixel, y.center - y), red
    Line(x.center - x.pixel + 1, y.center - y.previous)-(x.center - x.pixel, y.center - y), red
    Line(x.center + x.pixel - 1, y.center + y.previous)-(x.center + x.pixel, y.center + y), red
    Line(x.center - x.pixel + 1, y.center + y.previous)-(x.center - x.pixel, y.center + y), red
  End If
  y.previous = y
Next

Line(0, y.center)-(display.width, y.center), gray         'horizontal central line.
Line(x.center, y.center-20)-(x.center, y.center+20), gray 'vertical central line.
For j = half.lambda To 2 * lambda Step half.lambda        'wavelength marks.
  Line(x.center + j, y.center - 10)-(x.center + j, y.center + 10), gray
  Line(x.center - j, y.center - 10)-(x.center - j, y.center + 10), gray
Next
t = Timer
Return
