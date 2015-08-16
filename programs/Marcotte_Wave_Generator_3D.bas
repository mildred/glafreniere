' This is a FreeBasic program. Gabriel LaFreniere, Sept. 20, 2007. Updated Oct. 3, 2007. 
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
Dim As Single amplitude, x.squared, correction, pi, double.pi, diagonal, contrast, brightness
Dim As Single period, factor, y.pixel, x.phase, interior.radius, exterior.radius
Dim As Single phase(0 To 400, 0 To 400), fade(0 To 400, 0 To 400)
Dim previous(0 To 800), pixel(0 To 800)
Gosub Initialization

Do
  Swap visible.page, work.page                            'swap 1 <-> 0
  Screenset work.page, visible.page
  Pcopy matrix.page, work.page
  Gosub Display
Loop

Display:'------------------------------- 3-D DISPLAY ------------------------------------------
period = image * 2 * pi / images                          'wave period according to image
For x = 0 To half.width  
  For y = 0 To display.height
    y.pixel = fade(x,y) * Cos(phase(x,y) - period)
    y.coord = y - factor * y.pixel + 10                   '10 lower to show upper waves.
    y.pixel = contrast * y.pixel                          'contrast = .7
    amplitude = y.pixel + brightness                      'brightness = .4
    luminance1 = 255 * amplitude                          '256 gray shades
    If luminance1 < 0 Then luminance1 = 0
    If luminance1 > 255 Then luminance1 = 255             'maximum.
    Pset(x.center + x, y.coord), Rgb(luminance1,luminance1,luminance1) 'no 3-D yet
    Pset(x.center - x, y.coord), Rgb(luminance1,luminance1,luminance1)
    If y > 0 Then gauge = y.coord - y.previous Else gauge = 1
    If gauge > 1 Then                                     'fill empty spaces
      y.pixel = fade(x,y-1) * Cos(phase(x,y-1) - period)  'previous pixel: y-1
      y.pixel = contrast * y.pixel                        'contrast = .7
      amplitude = y.pixel + brightness                    'brightness = .4
      luminance2 = 255 * amplitude                        'previous pixel
      shade.step = (luminance2 - luminance1) / gauge
      shade = luminance1                                  'start with actual pixel
      For j = 1 To gauge - 1                              'fill with progressive shades
        shade = shade + shade.step
        If shade > 255 Then shade = 255
        If shade < 0 Then shade = 0
        Pset(x.center + x, y.coord - j), Rgb(shade, shade, shade)      '3-D effect
        Pset(x.center - x, y.coord - j), Rgb(shade, shade, shade)
      Next
    End If
    y.previous = y.coord
  Next y
'------------------------------------- AXIAL GRAPHICS -----------------------------------------

  y.pixel = fade(x,half.height) * Cos(phase(x,half.height) - period)
  y = y.center - 75 * y.pixel
  If x > 0 Then Line(x.center + x - 1, y.previous2)-(x.center + x, y),black
  If x > 0 Then Line(x.center - x + 1, y.previous2)-(x.center - x, y),black
  y.previous2 = y
'----------------------------------------------------------------------------------------------
  a$ = Inkey                                              'keyboard
  If a$ = Chr(255)+ "k" Then End
  a$ = Ucase(a$)
  If a$ = "P" Then Sleep: b$ = Inkey: a$ = ""
  If Len(a$) Then End
  Getmouse x.mouse, y.mouse, reel, click                  'mouse
Next x
Locate 31,2: Print "t = 2 * pi *";image;" /"; images
If click = 1 Then image = image - 1                       'pause
If bitmap Then Gosub Bitmaps
image = image + 1
If image > images Then image = 1
Return

Calculus:'----------------------- PHASE AND AMPLITUDE DATA ------------------------------------
For x = 0 To half.width                                   'horizontal symmetry
  x.squared = x * x
  For y = 0 To display.height                             'no vertical symmetry
    y2 = 2 * (y - half.height)                            'ellipse 1:2 ratio for 3-D effect
    diagonal = Sqr(y2 * y2 + x.squared)                   'true distance (Pythagoras)
    x.phase = double.pi * diagonal / lambda               'phase delay according to distance

'********************** PHASE AND AMPLITUDE CORRECTION FOR THE CENTER *************************
    If Abs(x.phase) < pi Then
      x.phase = x.phase + (pi/2) * (1-x.phase/pi)^2       'phase and amplitude correction
    End If
'**********************************************************************************************
    phase(x,y) = x.phase
    fade(x,y) = Sqr(1 / x.phase)                          'omit Sqr for normal amplitude
  Next
Next
Return

Bitmaps:'------------------------- BITMAP SEQUENCE IF NEEDED ----------------------------------
Select Case capture
  Case Is < 10: number$ = "00"
  Case Is < 100: number$ = "0"
  Case Is < 1000: number$ = ""
End Select
'Locate 35, 85: Print Using "Capture ## /"; capture;: Print images
file$ = "capture" + number$ + Str(capture) + ".bmp"
Bsave file$,0
capture = capture + 1
If capture > images Then End
Return

'------------------------------------- INITIALIZATION -----------------------------------------
Initialization:
Screen 19,24,3
work.page = 1 
matrix.page = 2 
pi = 4 * Atn(1)
double.pi = 8 * Atn(1)
lambda = 100                                              'wavelength
images = 64                                               'number of images per period
image = 1
display.width = 800
half.width = display.width / 2
display.height = 400
half.height = display.height / 2
factor = 50                                               '3-D amplitude factor
contrast = .8
brightness = .35
x.center = half.width
y.center = 2 * half.height + 120
image1 = 1
'bitmap = 1: capture = 1                                  ' = 1 for bitmap sequence/animation.
Windowtitle "The Marcotte Wave Generator - 3-D effect"
red = Rgb(255,0,0)
gray = Rgb(150,150,150)
white = Rgb(255,255,255)
background = Rgb(225,225,225)
Color black, background
Screenset matrix.page, matrix.page
Cls: Color Rgb(0,150,0), background
If bitmap Then
  Locate 10, 15: Print "A bitmap sequence will be created."
  Locate 12, 15: Print "Press Enter to confirm."
  Sleep: If Not Inkey = Chr(13) Then bitmap = 0
End If
Locate 35,2: Print "Press Esc to quit."
Locate 36,2: Print "Pause: click or press P.   Oct. 3, 2007."
Locate 37,2: Print "Gabriel LaFreniere         glafreniere.com";
Locate 36,61:Print "The source code (freebasic.net) may be"
Locate 37,61:Print "freely distributed, copied or modified.";
Color black, background
Locate 28,2: Print "The goal was to obtain a pi / 2"
Locate 29,2: Print "phase offset at the center."
Locate 28,69:Print "y = sin(x - t) / x"
Locate 29,69:Print "x = 2 * pi * distance / lambda"
Locate 30,69:Print "Phase correction for x < pi:"
Locate 31,69:Print "x = x + (pi/2) * (1-x/pi)^2"
Gosub Calculus
'------------------------------------ AXIAL GRAPHICS ------------------------------------------
For x = 0 To half.width
  y = 75 * fade(x,half.height)
  Line(x.center + x, y.center - y)-(x.center + x, y.center + y), white'template.
  Line(x.center - x, y.center - y)-(x.center - x, y.center + y), white
Next
For j = lambda / 2 To 8 * lambda / 2 Step lambda / 2                  'wavelength scale bars
  Line(x.center+j, y.center - 5)-(x.center+j, y.center + 5), gray
  Line(x.center-j, y.center - 5)-(x.center-j, y.center + 5), gray
Next
Line(0, y.center)-(display.width, y.center), gray                     'horizontal central line
Line(x.center, y.center - 20)-(x.center, y.center + 20), gray         'vertical central line

' Mr. Jocelyn Marcotte's formula proves to be accurate. It produces a curve which reaches
' a higher point, indicating that this method underestimates the amplitude in the center.
' However, it is simple, and the curve is very smooth, making it much more attractive.
' In addition, the goal was to reproduce the electron pi/2 phase offset at the center
' in order to show how the interference pattern between two or more electrons should
' behave. So I will be very comfortable with this method in the future, until someone
' finally finds the perfect formula without this abnormal amplitude in the center. 
Return
