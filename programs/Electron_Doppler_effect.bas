'This is a FreeBasic program. Gabriel LaFreniere, Sept. 12, 2007.
'Please download the free editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56
#lang "fblite" 
Option Gosub

Dim As Single xCoord, yCoord, xSquared, ySquared, lightPath, sourcePath
Dim As Single pi, doublePi, beta, theta, gLorentz, amplitude, tTime
Gosub Initialization

Do
  Swap workPage, visiblePage                              'swap 1 <-> 0.
  screensync
  Screenset workPage, visiblePage                         'work on hidden page (faster).
  Pcopy matrixPage, workPage                              'copy on work page.
  tTime = image * doublePi / images                       'wave period = image / images.
  Gosub staticPulsatingWaveCenter                         'wave generator. No Doppler effect.
  Gosub mobilePulsatingWaveCenter                         'two different Doppler effects.
  image = image + 1                                       'time running.
  Do
    If Len(Inkey) Then End                                'keyboard (exit).
    Getmouse xMouse, yMouse, , click                      'mouse (move/pause).
    Select Case click
      Case 1: image = (xMouse - halfWidth) * images / (doublePi * beta * lambda): Exit Do
      Case 2                                              'pause.
      Case Else: Exit Do
    End Select
  Loop
Loop

staticPulsatingWaveCenter:' SPHERICAL WAVES - NO DOPPLER EFFECT -------------------------------

For x = 0 To zoneWidth                                    'scan horizontally.
  xCoord = (x - halfWidth) / lambda                       'x coordinate in wavelength.
  xSquared = xCoord^2                                     'faster.
  For y = 0 To halfHeight                                 'scan vertically to 1/2 (symmetry).
    yCoord = y / lambda                                   'y coordinate in wavelength.
    lightPath = Sqr(xSquared + yCoord^2)                  'Pythagoras. Delay in wave period.
    amplitude = Sin(doublePi * (lightPath - tTime))       'delay and time in radians.
    luminance = 127.5 * (amplitude + 1)                   'gray scale, max = 2 * 127.5 = 255.
    Pset(x, halfHeight + y), Rgb(luminance,luminance,luminance)
    Pset(x, halfHeight - y), Rgb(luminance,luminance,luminance)'no Doppler.
  Next
Next
Return

mobilePulsatingWaveCenter:'GENERATING REGULAR AND LORENTZ DOPPLER EFFECT ----------------------

' The standard Doppler effect is well known and the calculus below is quite simple. However,
' it may be modified in order to match the Lorentz transformations. In other words, the
' emitter's frequency is slowed down according to Lorentz's g factor, producing a    
' general expansion in the all-azimuth wavelengths. The most stunning result of this is that
' the transverse wavelength is not contracted any more. It never changes: y'=y; z'=z (Lorentz).

' Each vertical scan begins with a 20 iteration precalculus. Then each new y value needs
' only two iterations (which is faster) because the lightPath variable is almost correct.
' One may use four or five more iterations below for very high beta speed (try beta = .9).
' The goal here is to show clearly why a Doppler effect occurs. 
' However, the Doppler effect may also be given directly by (L is for wave length):
' L'=L*(cos(asin(beta*sin(phi)))-beta*cos(phi))
' The phi variable is the (x,y) point angle by respect to the system center (both are moving).

'Step 1. The Doppler effect is a consequence of the variable light path from the source to a
'        mobile (x,y) point. The forward/backward asymmetry introduces a variable delay which
'        modifies the wave phase. Of course, the speed of light through the aether is constant. 

'Step 2. The Lorentz Doppler effect involves a slower electron pulsating frequency
'        according to the Lorentz g factor. This slower frequency causes a longer
'        wavelength, which is easily obtained below by making the light path shorter.

center = beta * tTime * lambda                            'in pixels (motion = speed * time).
If center > max - halfWidth Then image = 0                'reset.

For x = 0 To zoneWidth                                    'scanning x, y.
  xCoord = (x - halfWidth) / lambda
  For j = 1 To 20                                         '20 iterations precalculus.
    sourcePath = beta * lightPath                         'source path according to beta.
    lightPath = Sqr((xCoord + sourcePath)^2 + (halfHeight / lambda)^2)'Pythagoras (diagonal).
  Next                                                  '

  For y = 0 To halfHeight
    yCoord = (halfHeight - y) / lambda
    sourcePath = beta * lightPath
    lightPath = Sqr((xCoord + sourcePath)^2 + yCoord^2)
    sourcePath = beta * lightPath                         '2nd iteration.
    lightPath = Sqr((xCoord + sourcePath)^2 + yCoord^2)   'lightPath indicates the delay below.

'                                     1- ORIGINAL FREQUENCY.

    amplitude = Sin(doublePi * (lightPath - tTime))
    luminance = 127.5 * (amplitude + 1)
    Pset(1023 - zoneWidth + x, y), Rgb(luminance,luminance,luminance)      'regular Doppler.
    Pset(1023 - zoneWidth + x, height - y), Rgb(luminance,luminance,luminance)

'                                      2- SLOWER FREQUENCY.

    amplitude = Sin(doublePi * (gLorentz * lightPath - gLorentz * tTime))
    luminance = 127.5 * (amplitude + 1)
    Pset(center + x, height + y + 30), Rgb(luminance,luminance,luminance)  'Lorentz's Doppler.
    Pset(center + x, 2 * height - y + 30), Rgb(luminance,luminance,luminance)
  Next
Next


'------------------------------------ WAVELENGTH SCALE ----------------------------------------
Line(halfWidth, 0)-(halfWidth, height), black
Line(1023 - halfWidth, 0)-(1023 - halfWidth, height), black
Line(center + halfWidth, height + 30)-(center + halfWidth, 2 * height + 30), black
For j = 0 To 8 * lambda Step lambda
  Line(halfWidth - 10, j)-(halfWidth, j), black
  Line(halfWidth, j)-(halfWidth + 10, j), white
  Line(1023 - halfWidth - 10, j)-(1023 - halfWidth, j), black
  Line(1023 - halfWidth, j)-(1023 - halfWidth + 10, j), white
  Line(center + halfWidth - 10, j + height + 30)-(center + halfWidth, j + height + 30), black
  Line(center + halfWidth, j + height + 30)-(center + halfWidth + 10, j + height + 30), white
Next
Return

Initialization:'---------------------- INITIALIZATION -----------------------------------------
Screeninfo w, h                                           'detecting monitor resolution.
If w < 1024 Then                                          'filter 800 x 600 or less.
  Screen 12, 24, 1
  Locate 10,10:Print "Minimum 1024 x 768 screen resolution required."
  Locate 12,10:Print "Press any key to quit."
  Sleep: End
Elseif w = 1024 And h = 768 Then                          'filter 1024x768 and 1280x1024.
  Screen 20,24,3,1                                        '1 = full screen if possible.
Elseif w = 1280 And h = 1024 Then
  Screen 21,24,3,1                                        'full screen to avoid pixellisation.
Else
  Screen 20,24,3                                          'should fit into any other screen.
End If
max = w
workPage = 1
matrixPage = 2
pi = 4 * Atn(1)
doublePi = 8 * Atn(1)
beta = .5                                                 'normalized speed: beta = v/c.
theta = Asin(beta)                                        'transverse waves angle.
gLorentz = Cos(theta)                                     'Lorentz's g contraction factor.
lambda = 32                                               'static electron wavelength.
images = 128                                              'number of images per period.
zoneWidth = 400                                           'display zone.
height = 8 * lambda
halfWidth = zoneWidth / 2
halfHeight = height / 2
white = Rgb(255,255,255)
background = Rgb(225,225,225)
Screenset matrixPage, matrixPage
Color black, background: Cls
Color Rgb(0,150,0)
Locate 47,2: Print "The source code may be freely copied, distributed, or modified.";
Locate 48,2: Print "Gabriel LaFreniere     glafreniere.com     freebasic.net     Sept. 2007";
Color black, background
Locate 2,53: Print "Exit: press any key."
Locate 3,53: Print "Full screen or exit full"
Locate 4,53: Print "screen: Alt + Enter."
Locate 5,53: Print "Move source: click/drag."
Locate 6,53: Print "Pause: right click."
Locate 8,53: Print "Beta normalized speed:"
Locate 9,53: Print "beta = v / c = .5"
Locate 11,53:Print "Tranverse waves' angle:"
Locate 12,53:Print "theta = arc sin(v / c)"
Locate 13,53:Print "theta = 30"; Chr(248)
Locate 15,53:Print "Lorentz's g factor:"
Locate 16,53:Print "g = cos(theta) = .866"
Locate 17,4: Print "The unmoving wave source: no Doppler effect."
Locate 17,82:Print "Note the transverse wavelength  contraction."
Locate 35, 2:Print "The electron Doppler effect according to Lorentz: the slower frequency cancels the transverse contraction: y'= y; z'= z."
Locate 39,75:Print "LightPath = sqr((SourcePath + x)^2 + y^2)"
Locate 40,75:Print "SourcePath = beta * LightPath" 
Locate 37, 2:Print "Doppler Forward:  1 - beta = 0.5 * wavelength"
Locate 38, 2:Print "Doppler Backward: 1 + beta = 1.5 * wavelength (1:3 ratio)."
Locate 40, 2:Print "The electron forward/backward amazing symmetry:"
Locate 41, 2:Print "The frequency is slower: F' = g * F"
Locate 42, 2:Print "Forward:  (1 - beta) / g =  .577 * wavelength"
Locate 43, 2:Print "Backward: (1 + beta) / g = 1.732 * wavelength (same 1:3 ratio)."
Locate 44, 2:Print "Forward:  1 / 1.732 = .577"
Locate 45, 2:Print "Backward: 1 / .577 = 1.732"
Locate 48,117: Print "x";
Locate 43,126: Print "y"
Locate 43, 93: Print "Light path"
Locate 48, 89: Print "Source path       S";
Circle(690, 750), 3, black                                'source and light path diagram.
Paint(690, 750), black, black
Circle(860, 750), 3, black
Paint(860, 750), black, black
Circle(990, 600), 3, black
Paint(990, 600), black, black
Line(850, 664)-(870, 660), black                          'upper arrow.
Line(855, 673)-(870, 660), black
Line(850, 664)-(855, 673), black
Paint(858, 666), black, black
Line(804, 745)-(824, 750), black                          'lower arrow.
Line(804, 755)-(824, 750), black
Line(804, 745)-(804, 755), black
Paint(809, 750), black, black
Line(690, 750)-(990, 750), black                          'triangles.
Line(690, 750)-(990, 600), black
Line(990, 750)-(990, 600), black
Line(860, 750)-(990, 600), black
'w = Width
'Locate 46,118: Print "rows: " & HiWord(w)
'Locate 47,118: Print "cols: " & LoWord(w)
Return
