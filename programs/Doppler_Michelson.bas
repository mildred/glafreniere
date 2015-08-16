' This is a FreeBasic program. Gabriel LaFreniere, Oct. 14, 2007. Updated Oct. 15, 2007. 
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
#lang "fblite" 
Option Gosub

Dim As Single pi, double.pi, x.prime, y.prime, x.squared, y.squared, k.constant
Dim As Single beta, theta, g.Lorentz, amplitude, light.path, source.path
Dim As Single t.time, t.prime, center, x.coord, y.coord
Screen 19,24,3: beta = .5: work.page = 1: Gosub Initialization

Do
  Swap visible.page, work.page                            'swap pages 0 <=> 1.
  screensync
  Screenset work.page, visible.page                       'set work page (faster).
  Pcopy matrix.page, work.page                            'copy on work page.
  Gosub Emitter                                           'displays the waves.
  key$ = Inkey                                            'keyboard.
  If key$ = chr(27) or key$ = chr(255)+"k" Then End       'exit
  key$ = ucase(key$)                                      'keyboard: exit.
  if key$ = "P" then sleep: a$ = inkey
  speed = val(key$): key$ = ""
  if speed > 0 and speed < 10 then
    beta = speed / 10
    speed = 0
    gosub Initialization
  end if
Loop

'-------------------------------------- WAVE GENERATOR ----------------------------------------
Emitter:
image = image + 1                                         'to be converted in wave period.
If x.prime * lambda > 800 - half.width Then image = 1     'reset.
t.time = image * double.pi / images                       'wave period according to image.
For x = 0 To display.width                                'horizontal scan.
  x.coord = (x - half.width) / lambda                     'x coordinate in wavelength units.
  x.squared = x.coord ^ 2
  x.prime = x.coord*g.Lorentz*k.constant+t.time*beta      'x' coordinate in wavelength units.
  t.prime = t.time*g.Lorentz/k.constant-x.coord*beta      't' time.
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
'------------------------------------ NO DOPPLER EFFECT ---------------------------------------
    Pset(x, half.height + y), Rgb(r,g,b)
    Pset(x, half.height - y), Rgb(r,g,b)

'---------------------------------- REGULAR DOPPLER EFFECT ------------------------------------
    y.prime = y * k.constant                               'y' coordinate in pixel units.
    amplitude = Sin(double.pi*(light.path-t.prime))/Sqr(light.path)'according to t.prime.
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
  Next
Next

'------------------------------------ WAVELENGTH SCALE ---------------------------------
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
Windowtitle "The Doppler effect revisited. - The Woldemar Voigt Transformations."
pi = 4 * Atn(1)
double.pi = 8 * Atn(1)
theta = Asin(beta)                                        'transverse wave angle.
g.Lorentz = Cos(theta)                                    'Lorentz contraction factor.
k.constant = g.Lorentz                                    'Woldemar Voigt's constant.
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
Color black, background: Cls
Color Rgb(0,150,0)
Locate 37, 2: Print "This program may be freely distributed, copied or modified.  Gabriel LaFreniere   glafreniere.com";
Color black
Locate 2, 53: Print "Press Esc. to exit.  Pause: Press P."
Locate 3, 53: Print "Press a number to change the speed: beta = ";
print using "#.#"; beta
Locate 5, 53: Print "beta = v / c"
Locate 8, 53: Print "This program uses this set of equations:"
Locate 10,53: Print "Michelson: x'= x * (1 - beta^2) + t * beta"
Locate 11,53: Print "           t'= t - x * beta"
Locate 12,53: Print "           y'= y * sqr(1 - beta^2)"
Locate 13,53: Print "           z'= z * sqr(1 - beta^2)"
Locate 15,53: Print "(Lorentz): x'= x * sqr(1 - beta^2) + t * beta"
Locate 16,53: Print "           t'= t * sqr(1 - beta^2) - x * beta"
Locate 17,53: Print "           y'= y"
Locate 18,53: Print "           z'= z"
Return
