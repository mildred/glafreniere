' This is a FreeBasic program. Gabriel LaFreniere, Oct. 14, 2007. Updated Oct. 24, 2007. 
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
Dim As Single beta, theta, g.Lorentz, amplitude, light.path, source.path
Dim As Single pi, double.pi, x, x.squared, y.squared, x.prime, y.prime
Dim As Single t.time, t.prime, center, x.coord, y.coord
lambda = 64: beta = .8: work.page = 1: choice$ = "C": Screen 19,24,3: Gosub Initialization

Do'-------------------------------------- MAIN LOOP -------------------------------------------
  Swap visible.page, work.page                            'swap pages 0 <=> 1.
  Screenset work.page, visible.page                       'set work page (faster).
  Pcopy matrix.page, work.page                            'copy on work page.
  Gosub Generator                                         'displays the moving electron.
  key$ = Inkey                                            'keyboard.
  If Len(key$) Then
    If key$ = Chr(27) Or key$ = Chr(255)+"k" Then End     'quit.
    key$ = Ucase(key$)
    speed = Val(key$)
    If speed > 0 And speed < 10 Then beta = speed / 10: speed = 0'beta speed.
    Select Case key$
      Case "A", "B", "C": choice$ = key$
      Case "0": beta = 0
      Case "+": lambda = lambda + 4: If lambda > 256 Then lambda = 256
      Case "-": lambda = lambda - 4: If lambda < 24 Then lambda = 24 
    End Select
    If key$ = "P" Then Sleep Else Gosub Initialization    'pause, or update variables.
    key$ = Inkey                                          'clear buffer (for pause).
  End If
Loop
'------------------------------------- WAVE GENERATOR -----------------------------------------
Generator:
image = image + 1                                         'to be converted in wave period.
If pixel > 800 Then image = 1                             'reset.
t.time = image  / images                                  'wave period according to image.

For x.pixel = -half.width To half.width                   'horizontal scan.
  x.coord = x.pixel / lambda                              'x coordinate in wavelength units.
  
  x.prime = x.coord * g.Lorentz + t.time  * beta          'correction added for accuracy only
  x.Prime = Int(x.prime * lambda) / lambda                'because the pixel x.prime * lambda
  If beta > 0 Then x.coord=(x.prime-t.time*beta)/g.Lorentz'is an integer.

'******************************** THE LORENTZ TRANSFORMATIONS *********************************

  x.prime = x.coord * g.Lorentz + t.time  * beta          ' x' coordinate in wavelength units.
  t.prime = t.time  * g.Lorentz - x.coord * beta          ' t' time, more exactly a wave phase.

'**********************************************************************************************

  x.squared = x.coord ^ 2
  t.prime = double.pi * t.prime                           'conversion in radians.
  pixel = x.prime * lambda + g.Lorentz * half.width
  If pixel = previous.pixel Then Goto bypass              'skip same vertical line.
  previous.pixel = pixel

  For y = 0 To half.height                                'vertical scan, symmetry.
    y.coord = y / lambda                                  'y coordinate in wavelength units.
    light.path = Sqr(x.squared + y.coord ^ 2)             'equals the delay in wave periods.
    x = double.pi * light.path                            'conversion in radians.
    If x < pi Then x = x + (pi / 2) * (1 - x / pi) ^ 2    'pi / 2 phase offset in the center.
    Select Case choice$
      Case "A"
        amplitude = brightness * Sin(x - t.prime) / x     'Marcotte's wave generator.
      Case "B"
        amplitude = brightness * Sin(x + t.prime) / x
      Case "C"                                            'adding in-waves to out-waves.
        amplitude = Sin(x - t.prime) / x + Sin(x + t.prime) / x
        amplitude = .5 * brightness * amplitude
'       amplitude = 2 * Sin(t.prime)*brightness*Sin(x)/x  'Marcotte's Sin(x)/x also works.
    End Select
    If amplitude > 0 Then                                 'color distribution.
      g =  amplitude: r = 0: b = g / 2
      If g > 255 Then r = g - 255
    Else
      r = -amplitude: g = 0: b = r / 2
      If r > 255 Then g = r - 255
    End If
    If r > 255 Then r = 255
    If g > 255 Then g = 255
    If b > 255 Then b = 255
    Pset(pixel, y.center + y), Rgb(r,g,b)                 'N.B. Lorentz's equation: y' = y.
    Pset(pixel, y.center - y), Rgb(r,g,b)
  Next

'------------------------------------ ON-AXIS GRAPHICS ----------------------------------------
  x = double.pi * Abs(x.coord)                            'conversion in radians.
  If x < pi Then x = x + (pi / 2) * (1 - x / pi) ^ 2      'pi / 2 phase offset in the center.
  Select Case choice$
    Case "A"
      amplitude = Sin(x - t.prime) / x                    'Marcotte's wave generator.
    Case "B"
      amplitude = Sin(x + t.prime) / x
    Case "C"                                              'adding in-waves to out-waves.
      amplitude = .5 * Sin(x - t.prime) / x + .5 * Sin(x + t.prime) / x
      amplitude2 = 70 * Cos(t.prime) * Sin(x) / x         'Marcotte's Sin(x)/x is accurate.
  End Select
  If x.pixel = -half.width Then y.previous = y.graph - 70 * amplitude
  Line(pixel, y.previous)-(pixel, y.graph - 70 * amplitude), black
  y.previous = y.graph - 70 * amplitude
' Pset(pixel, y.graph - amplitude2), green                'check Marcottes's formula above.
bypass:
Next

'-------------------------------------- INITIALIZATION ----------------------------------------
Initialization:
matrix.page = 2
Windowtitle "The Marcotte wave generator and the Lorentz transformations."
pi = 4 * Atn(1)
double.pi = 8 * Atn(1)
theta = Asin(beta)                                        'transverse wave angle.
g.Lorentz = Cos(theta)                                    'Lorentz contraction factor.
images = 32 + lambda / 4                                  'number of images per period.
display.width = 700
display.height = 336
half.width = display.width / 2
half.height = display.height / 2
y.center = half.height
y.graph = display.height + 46
brightness = 3000
green = Rgb(0,150,0)
white = Rgb(255,255,255)
background = Rgb(225,225,225)
Screenset matrix.page
Color black, background: Cls: Locate 16,53: Color Rgb(0,0,255)
Line(0,y.graph - 45)-(799,y.graph - 45), green
Line(0,y.graph     )-(799,y.graph     ), green
Line(0,y.graph + 45)-(799,y.graph + 45), green
Color black
Locate 28, 2: Print "Press A for outgoing waves."
Locate 29, 2: Print "Press B for incoming waves."
Locate 30, 2: Print "Press C for both incoming and outgoing waves."
Locate 32, 2: Print "The Lorentz transformations produce a Doppler"
Locate 33, 2: Print "effect with a slower frequency. They also"
Locate 34, 2: Print "produce a longitudinal contraction, but"
Locate 35, 2: Print "no contraction occurs transversally."
Locate 28,54: Print "Press Esc. to quit.  Pause: Press P."
Locate 29,54: Print "Press a number to change speed: ";
Print Using "#.#"; beta
Locate 30,54: Print "Press + or - to change wavelength:"; lambda
Locate 32,54: Print "beta = v / c              beta = ";
Print Using "#.#"; beta
Locate 33,54: Print "g = sqr(1 - beta ^ 2)        g = ";
Print Using "#.###"; g.Lorentz
Locate 35,54: Print "x'= x * g + t * beta    y'= y    z'= z"
Locate 36,54: Print "t'= t * g - x * beta"
Color Rgb(0,0,255)
Select Case choice$
  Case "A": Locate 28,2: Print "Displaying outgoing waves. "
  Case "B": Locate 29,2: Print "Displaying incoming waves. "
  Case "C": Locate 30,2: Print "Displaying both incoming and outgoing waves. "
End Select
Color green
Locate 36, 2: Print "This program may be freely distributed, copied"
Locate 37, 2: Print "or modified.  Gabriel LaFreniere, Oct. 24, 2007.";
Locate 37,83: Print "glafreniere.com";
Return

