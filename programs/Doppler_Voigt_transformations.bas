' This is a FreeBasic program. Gabriel LaFreniere, Oct. 14, 2007. Updated Oct. 16, 2007. 
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56
#lang "fblite" 
Option Gosub

Dim As Single pi, double.pi, x.prime, y.prime, x.squared, y.squared, k.constant
Dim As Single beta, theta, g.Lorentz, amplitude, light.path, source.path
Dim As Single t.time, t.prime, center, x.coord, y.coord
beta = .5: k.constant = 1: work.page = 1: k.Voigt$ = "C"
Screen 19,24,3: Gosub Initialization

Do
  Swap visible.page, work.page                            'swap pages 0 <=> 1.
  screensync
  Screenset work.page, visible.page                       'set work page (faster).
  Pcopy matrix.page, work.page                            'copy on work page.
  Gosub Emitter                                           'wave display.
  key$ = Inkey                                            'keyboard.
  If Len(key$) Then
    If key$ = Chr(27) Or key$ = Chr(255)+"k" Then End     'quit.
    key$ = Ucase(key$)
    Select Case key$
      Case "A","B","C","D": k.Voigt$ = key$               'Voigt's constant.
    End Select
    speed = Val(key$)
    If speed > 0 And speed < 10 Then beta = speed / 10: speed = 0'beta speed.
    If key$ = "P" Then Sleep Else Gosub Initialization    'pause, or update variables.
    key$ = Inkey                                          'clear buffer for pause.
  End If
Loop

Emitter:                                                  'wave generator.
image = image + 1                                         'to be converted in wave period.
If x.prime * lambda > 800 - half.width Then image = 1     'reset.
t.time = image * double.pi / images                       'wave period according to image.
For x = 0 To display.width                                'horizontal scan.
  x.coord = (x - half.width) / lambda                     'x coordinate in wavelength units.
  x.squared = x.coord ^ 2

'**************************** THE WOLDEMAR VOIGT TRANSFORMATIONS ******************************

  x.prime = x.coord* g.Lorentz* k.constant+ t.time * beta ' x' coordinate in wavelength units.
  t.prime = t.time * g.Lorentz/ k.constant- x.coord* beta ' t' time, more exactly a wave phase.

'**********************************************************************************************
  
  If x = half.width Then center = x.prime * lambda        'for moving scale.
  For y = 0 To half.height                                'vertical scan, symmetry.
    y.coord = y / lambda                                  'y coordinate in wavelength units.
    light.path = Sqr(x.squared + y.coord^2)               'equals the delay in wave periods.
'If light.path < .5 Then light.path=light.path+.25*(1-light.path/.5)^2'correction for a smoother core. 
    amplitude = Sin(double.pi * (light.path - t.time)) / Sqr(light.path)'sinusoidal wave.
    If amplitude > 0 Then
      r = 0: g = 255 * amplitude: b = g / 2               'color distribution.
      If g > 255 Then r = g - 255: g = 255
      If r > 255 Then r = 255
      If b > 255 Then b = 255
    Else
      r = -255 * amplitude: g = 0: b = r / 2
      If r > 255 Then g = r - 255:r = 255
      If g > 255 Then g = 255
      If b > 255 Then b = 255
    End If
    Pset(x, half.height + y), Rgb(r,g,b)                  'emitter at rest (no Doppler).
    Pset(x, half.height - y), Rgb(r,g,b)

'**************** TRANSVERSE CONTRACTION IN ACCORDANCE WITH VOIGT'S K CONSTANT  ***************

    y.prime = y * k.constant                               'y' coordinate in pixel units.

'**********************************************************************************************
  
    amplitude = Sin(double.pi*(light.path-t.prime))/Sqr(light.path)'phase according to t.prime.
    If amplitude > 0 Then
      r = 0: g = 255 * amplitude: b = g / 2               'color distribution.
      If g > 255 Then r = g - 255: g = 255
      If r > 255 Then r = 255
      If b > 255 Then b = 255
    Else
      r = -255 * amplitude: g = 0: b = r / 2
      If r > 255 Then g = r - 255:r = 255
      If g > 255 Then g = 255
      If b > 255 Then b = 255
    End If
    Pset(x.prime*lambda+half.width,y.center+y.prime),Rgb(r,g,b)
    Pset(x.prime*lambda+half.width,y.center-y.prime),Rgb(r,g,b)
    If k.constant > 1 Then Circle (x.prime*lambda+half.width,y.center+y.prime),2,Rgb(r,g,b)
    If k.constant > 1 Then Circle (x.prime*lambda+half.width,y.center-y.prime),2,Rgb(r,g,b)
  Next
Next

'------------------------------------ WAVELENGTH SCALE ----------------------------------------
Line(half.width, 0)-(half.width, display.height), white
Line(center+half.width, display.height)-(center+half.width, 2 * display.height), white
For j = 0 To 8 * lambda Step lambda
  Line(half.width - 10, j)-(half.width + 10, j), white
  Line(center-10+half.width,j+display.height)-(center+10+half.width,j+display.height), white
Next
Return

'-------------------------------------- INITIALIZATION ----------------------------------------
Initialization:
matrix.page = 2
Windowtitle "The Doppler effect revisited.  The Woldemar Voigt Transformations.  Oct. 2007."
pi = 4 * Atn(1)
double.pi = 8 * Atn(1)
theta = Asin(beta)                                        'transverse wave angle.
g.Lorentz = Cos(theta)                                    'Lorentz contraction factor.
lambda = 36                                               'wavelength.
images = 128                                              'number of images per period.
display.width = 398
display.height = 8 * lambda
half.width = display.width / 2
half.height = display.height / 2
y.center = display.height + half.height
white = Rgb(255,255,255)
background = Rgb(225,225,225)
Screenset matrix.page
Color black, background: Cls: Locate 16,53: Color Rgb(0,0,255)
Select Case k.Voigt$                                      'Woldemar Voigt's constant.
  Case "A": k.constant = g.Lorentz ^ 2
            Print "k = g ^ 2  Faster frequency. Severe axial and"
            Locate, 53: Print "transverse contraction."
  Case "B": k.constant = g.Lorentz
            Print "The Michelson transformations: k = g. This is"
            Locate, 53: Print "the regular Doppler effect. No frequency shift."
            Locate, 53: Print "Contraction, axial: g^2; transverse: g."
  Case "C": k.constant = 1
            Print "Lorentz's transformations: k = 1 unnecessary."
            Locate, 53: Print "Slower frequency. No transverse contraction."
            Locate, 53: Print "Axial contraction according to g."
  Case "D": k.constant = 1 / g.Lorentz
            Print "k = 1/g. Slower frequency. Transverse dilation."
            Locate, 53: Print "                   No longitudinal contraction."
End Select
Color black
Locate 1, 53: Print "Press Esc. to quit.  Pause: Press P."
Locate 2, 53: Print "Press a key from A to D to change the constant."
Locate 3, 53: Print "Press a number to change the speed below:";
Locate 5, 53: Print "beta = v / c                     beta = ";
Print Using "#.#"; beta
Locate 6, 53: Print "g = sqr(1 - beta ^ 2)               g = ";
Print Using "#.###"; g.Lorentz
Locate 8, 53: Print "The Woldemar Voigt transformations produce a"
Locate 9, 53: Print "Doppler effect with a variable frequency shift."
Locate 11,53: Print "x'= x * g * k + t * beta"
Locate 12,53: Print "t'= t * g / k - x * beta"
Locate 13,53: Print "y'= y * k"
Locate 14,53: Print "z'= z * k                           k = ";
Print Using "#.###"; k.constant
Color Rgb(0,150,0)
Locate 37, 2: Print "This program may be freely distributed, copied or modified.  Gabriel LaFreniere   glafreniere.com";
Return

