' This is a FreeBasic program. Created May 6, 2005 by Gabriel LaFreniere.
' Please download the IDE (editor) from: http://fbide.freebasic.net
' Sub routine procedures are listed in alphabetical order.
' This source code was adapted to the 0.20.0b Compiler (2008) for Windows available from:
' http://www.freebasic.net/index.php/download
' It should be still compatible with previous compilers. 

Declare Sub Choice_A()
Declare Sub Choice_B()
Declare Sub Choice_C()
Declare Sub Choice_D()
Declare Sub Choice_E()
Declare Sub Choice_F()
Declare Sub Frame()
Declare Sub Initialization()
Declare Sub Standing_waves()
Declare Sub Text()
Declare Sub Title()

Const white = Rgb(255,255,255), purple = Rgb(255,0,255), buff = Rgb(255,255,200), gray = Rgb(125,125,125)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,100,100), light_green = Rgb(175,255,175)
Const gold = Rgb(180,150,100), blue_sky = Rgb(210,230,255), dark_gray = Rgb(75,75,75)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0), black = Rgb(0,0,0)

Dim Shared As Integer x, x1, x2, x_pixel, x_left, x_right,  x_text, x_center, x_mouse
Dim Shared As Integer y, y1, y2, y_pixel, y_top,  y_bottom, y_text, y_center, y_mouse,  y_green, y_red, y_black
Dim Shared As Integer unidirectional, work_page, visible_page, matrix_page, granules, luminance, left_frame, wheel, click
Dim Shared As Integer r, g, b, j, k, margin, algorithm, line_number, amplitude, top

granules = 1023
Dim Shared As Single trend(granules), present(-1 To granules+1), past(granules)
Dim Shared As Single potential(granules), kinetic(granules), quadrature(granules)
Dim Shared As Single pi = 4 * Atn(1), lambda, phase, Gaussian, speed, Lagrangian(granules), k_Dewavrin
Dim Shared As Single x_phase, dark

Dim Shared As String choice, reflection, key, w_title
Dim Shared As String line20, line21, line22, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39
Dim Shared As String line43, line44, line47a, line47b, line47c, line48a, line48b, line48c

choice = "B": lambda = (granules+1) / 16: speed = 1: unidirectional = 0: reflection = "both": work_page = 1: algorithm = 1
Screen 20,24,3: Initialization()


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Sleep 1

'*********************************************************************
' REFLECTION MANAGEMENT.
'*********************************************************************

  Select Case reflection
    Case "hard"                                                       'hard reflection: no action (default).
    Case "soft"
      present(-1)         = trend(1)                                  'soft reflection.
      present(granules+1) = trend(granules-1)
    Case "both"
      present(-1)         = trend(1)                                  'soft reflection on the left hand side only.
    Case "none"
      present(-1)         = present(0)                                'no reflection (full damping on both sides).
'      trend(0) =  present(-1) * speed + present(0) * (1 - speed)     'correction for speed < 1.
      present(granules+1) = present(granules)
    Case "loop"                                                       'endless loop.
      present(-1)         = trend(granules)
      present(granules+1) = trend(0)
  End Select


'*********************************************************************
' JOCELYN MARCOTTE'S VIRTUAL WAVE ALGORITHM (CREATED JANUARY 2006).    "THE PAST IS A GUIDE TO THE FUTURE"
'*********************************************************************

  For x = 0 To granules                                               'updating amplitude states.
    past(x)  = present(x)                                             'previous amplitude.
    present(x) = trend(x)                                             'present amplitude (energy is amplitude squared).
  Next
  For x = 0 To granules
    trend(x) = present(x-1) + present(x+1) - past(x)                  'trend extrapolation.

'*********************************************************************
' END OF ALGORITHM - NEEDS THREE ELEMENTARY PROGRAM LINES ONLY!
'*********************************************************************

    If speed < 1 Then
      trend(x) = (2-2*speed^2)*present(x) + speed^2*(present(x-1)+present(x+1)) - past(x)'for slower speed: lenses, etc.
    End If
  Next


'*********************************************************************
' DISPLAYING PAST, PRESENT, AND TREND CURVES.
'*********************************************************************

  y_green = y_center - past(1)                                        'previous y position initialization.
  y_red   = y_center - trend(1)
  y_black = y_center - present(1)

  For x = 1 To granules                                               'line from 0 to 1, hence x = 0 omitted.
    Line(x-1, y_green)-(x, y_center - past(x) ), green
    Line(x-1, y_red) - (x, y_center - trend(x)), red
    Line(x-1, y_black)-(x, y_center - present(x) ), black             'wave curve.
    y_green = y_center - past(x)
    y_red   = y_center - trend(x)
    y_black = y_center - present(x)
    luminance = Sqr(Abs(1500 * trend(x)))
    If luminance > 255 Then luminance = 255    
    If trend(x) > 0 Then
      Line(x, y_center - 140)-(x, y_center - 125), Rgb(0, luminance, luminance / 2)
    Else
      Line(x, y_center - 140)-(x, y_center - 125), Rgb(luminance, 0, luminance / 2)
    End If


'*********************************************************************
' DISPLAYING MORE WAVE INFORMATION.
'*********************************************************************

'   Please note that the mechanism involving kinetic and potential
'   energy is not the same for longitudinal and transverse waves
'   and that the sound mechanism is even more complex.
'   It should be pointed out that this wave algorithm is not a
'   mechanism. But, surprisingly, one can easily deduce hypothetic
'   kinetic and potential energy from it. This is a very convincing 
'   proof that it finally works like true waves. Thus, it reveals
'   itself to be a powerful tool for studying material waves. For
'   instance, the Lagrangian = kinetic - potential is perfect for
'   detecting standing waves. This program displays a very convenient
'   yellow and blue stripe which also indicates their phase.  

    kinetic(x)   = (trend(x) - past(x)) / speed                       'kinetic energy; needs correction for slow speed.
'    Pset(x, y_center - kinetic(x)), green                            'below: quadrature = (lambda / 4 / pi) * kinetic.
    potential(x)  = (present(x+1)) - (present(x-1))                   'hypothetic potential energy, asymmetric leftward
                                                                      'and rightward with respect to kinetic.
    If x Mod 2 Then Pset(x, y_center - potential(x)), purple          'The purple dotted curve for potential energy may
                                                                      'also be seen as the slope for transverse waves.
    Lagrangian(x) = kinetic(x) ^ 2  - potential(x) ^ 2                'Lagrangian according to the square of amplitude.
    luminance = 10 * Lagrangian(x)                                    'should match the 256 RGB color shades.
    If choice = "A" Then luminance = 10 * luminance                   'the slope for the Gaussian curve is different.
    If luminance > 0 Then
      r = luminance / 2: g = luminance / 2                            'r + g: yellow is much brighter than blue.
      b = 0
      If r > 255 Then r = 255
      If g > 255 Then g = 255
    Else
      luminance = -luminance
      b = luminance
      r = luminance / 3                                               'adding fractional r and g to brighten blue.
      g = luminance / 3
      If r > 200 Then r = 200
      If g > 200 Then g = 200
    End If
    If b > 255 Then b = 255
    Line(x, y_center - 109)-(x, y_center - 95), Rgb(r,g,b)

'   Dewavrin's constant below works better than the well known:
'                 .5 * lambda / (2 * pi) 
'   because of the quantum effect (see WaveMechanics01 and 05).
    k_Dewavrin = .5 / sin(2 * pi / lambda)
    quadrature(x) = k_Dewavrin * kinetic(x)                           'quadrature (blue curve) according to kinetic.
    If x Mod 2 Then Pset(x, y_center - quadrature(x)), blue           'it works, but it is somewhat disputable.
    
    luminance = 5 * Sqr(present(x) ^ 2 + quadrature(x) ^ 2)           'phaseless energy (stripe in 256 gray tones).
    If luminance > 255 Then luminance = 255    
    Line(x, y_center - 125)-(x, y_center - 110), Rgb(luminance, luminance, luminance)
    Line(0, y_center - 140)-(granules, y_center -  94), black, b
    Line(-1, y_center - 125)-(granules+1, y_center - 110), gray, b
  Next


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

  key = Inkey
  If Len(key) Then
    If Len(key) = 2 Then key = Right(key, 1) + "+" Else key = Ucase(key)
    Select Case key
      Case Chr(27), "k+": End                                         'escape key or Windows' X quit button.
      Case "A": choice = "A": reflection = "both"
                speed = 1: unidirectional = 0
      Case "B": choice = "B": speed = 1
      Case "C": choice = "C"
                speed = .5: unidirectional = 1: reflection = "loop"
      Case "D": choice = "D"
                speed = .5: unidirectional = 1: reflection = "loop"
      Case "E": choice = "E": speed = .5: reflection = "none"
      Case "F": choice = "F": speed = 1: reflection = "hard"
      Case "G": unidirectional = 1                                    'one-way impulse.
      Case "H": unidirectional = 0                                    'bidirectional impulse.
      Case "I": choice = "B": speed = 1
                reflection = "both": unidirectional = 0               'initialization.
      Case "J": If speed < 1 Then speed = 1                           'full speed.
      Case "K": If speed = 1 Then speed = .5                          'slower speed.
      Case "M": Run "WaveMechanics00.exe"                             'main menu.
      Case "P": Screenset visible_page                                'pause.
                Color red, background: Locate 42, 89
                Print "P - Paused. Press any key to resume.   "
                key = "": Sleep
                Color black: Screenset work_page, visible_page
      Case "Q": key = "": Standing_waves()
      Case "R":                                                       'reset via initialization.
      Case "S": reflection = "hard"
      Case "T": reflection = "soft"
      Case "U": reflection = "both"
      Case "V": reflection = "none"
      Case "W": reflection = "loop": unidirectional = 1
      Case Else: key = ""                                             'avoid initialization.
    End Select
    If Len(key) Then Initialization()
    Do: Loop While Len(Inkey)                                         'clear buffer.
  End If


'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

  Getmouse x_mouse, y_mouse, wheel, click
  line_number = .5 + y_mouse / 16
  If line_number < 46 And x_mouse < 695 Then line_number = 0
  Color green_text, white
  Select Case line_number
    Case 21: Locate 21, 88
             If choice <> "A" Then
               Print line21
               If click Then
                 reflection = "both": unidirectional = 0
                 choice = "A": speed = 1: Initialization()
               End If
             End If
    Case 22: Locate 22, 88
             If choice <> "B" Then
               Print line22
               If click Then choice = "B": speed = 1: Initialization()
             End If
    Case 23: Locate 23, 88
             If choice <> "C" Then
               Print line23
               If click Then
                 speed = .5: unidirectional = 1: reflection = "loop"
                 choice = "C": Initialization()
               End If
             End If
    Case 24: Locate 24, 88
             If choice <> "D" Then
               Print line24
               If click Then
                 speed = .5: unidirectional = 1: reflection = "loop"
                 choice = "D": Initialization()
               End If
             End If
    Case 25: Locate 25, 88
             If choice <> "E" Then
               Print line25
               If click Then
                 choice = "E": speed = .5
                 reflection = "none": Initialization()
               End If
             End If
    Case 26: Locate 26, 88
             If choice <> "F" Then
               Print line26
               If click Then
                 choice = "F": speed = 1
                 reflection = "hard": Initialization()
               End If
             End If
    Case 28: Locate 28, 88
             If unidirectional = 0 Then
               Print line28
               If click Then unidirectional = 1: Initialization()
             End If
    Case 29: Locate 29, 88
             If unidirectional = 1 Then
               Print line29
               If click Then unidirectional = 0: Initialization()
             End If
    Case 31: Locate 31, 88
             If speed < 1 Then
               If Not choice = "E" Then
                 Print line31
                 If click Then speed = 1: Initialization()
               End If
             End If
    Case 32: Locate 32, 88
             If speed = 1 Then
               Print line32
               If click Then speed = .5: Initialization()
             End If
    Case 34: Locate 34, 88
             If Not reflection = "hard" Then
               Print line34
               If click Then reflection = "hard": Initialization()
             End If
    Case 35: Locate 35, 88
             If Not reflection = "soft" Then
               Print line35
               If click Then reflection = "soft": Initialization()
             End If
    Case 36: Locate 36, 88
             If Not reflection = "both" Then
               Print line36
               If click Then reflection = "both": Initialization()
             End If
    Case 37: Locate 37, 88
             If Not reflection = "none" Then
               Print line37
               If click Then reflection = "none": Initialization()
             End If
    Case 38: Locate 38, 88
             If Not reflection = "loop" Then
               Print line38
               If click > 0 Then reflection = "loop": unidirectional = 1: Initialization()
             End If
    Case 43: Locate 43, 88
             Print line43
             If click Then Standing_waves()
    Case 44: Locate 44, 88
             Print line44
             If click Then Initialization()
    Case 47: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 47, 73: Print line47c: Sleep 200 'slow.
                              If click Then Sleep 1000                'slower.
               Case Is > 472: Locate 47, 60: Print line47b
                              If click Then Run "WaveMechanics00.exe" 'main menu.
               Case Is > 318: Locate 47, 41: Print line47a
                              If click Then                           'initialization.
                                choice = "B": speed = 1
                                reflection = "both": unidirectional = 0
                                Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
                                Initialization()
                              End If
             End Select
    Case 48: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 48, 73: Print line48c;
                              If click Then Run "WaveMechanics05.exe" 'next program.
               Case Is > 472: Locate 48, 60: Print line48b;
                              If click Then End                       'quit.
               Case Is > 318: Locate 48, 41: Print line48a;
                              If click Then Run "WaveMechanics03.exe" 'previous program.
             End Select
  End Select
Loop


'*********************************************************************
' CHOICE A - GAUSSIAN IMPULSE AND RESULTING RICKER WAVELET.
'*********************************************************************

Sub Choice_A()
For j = -1 To granules+1
  x_phase = granules / 2 - j
  Gaussian = 1.0005 ^ (-x_phase ^ 2)
  present(j) = amplitude * Gaussian
  If unidirectional Then trend(j) = present(j-1) Else trend(j) = present(j)
Next

If speed < 1 And unidirectional Then                                  'for unidirectional slow speed only.
  For j = 0 To granules
    trend(j) =  present(j-1) * speed + present(j) * (1 - speed)
  Next
End If
End Sub


'*********************************************************************
' CHOICE B - GAUSSIAN-DAMPED SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Choice_B()

For j = 0 To granules
  x_phase = granules / 2 - j
  phase = 2 * pi * x_phase / lambda
  x_phase = Abs(x_phase)
  Gaussian = 1.00003 ^ (-x_phase ^ 2)                                 'overall Gaussian distribution.
  present(j) = amplitude * Gaussian * Cos(phase)
  If unidirectional Then trend(j) = present(j-1) Else trend(j) = present(j)
Next

If speed < 1 And unidirectional Then                                  'for unidirectional slow speed only.
  For j = 0 To granules
    trend(j) =  present(j-1) * speed + present(j) * (1 - speed)
  Next
End If
End Sub


'*********************************************************************
' CHOICE C - SAWTOOTH WAVES.
'*********************************************************************

Sub Choice_C()
If unidirectional Then amplitude = lambda / 4 Else amplitude = lambda / 2
x_phase = -amplitude
For j = granules / 2 - 2 * lambda To granules / 2 + 2 * lambda
  present(j) = x_phase
  x_phase += amplitude / (lambda / 2)
  If x_phase > amplitude Then x_phase = -amplitude
Next

For j = 0 To granules
    If unidirectional Then trend(j) = present(j-1) Else trend(j) = (present(j-1) + present(j+1)) / 2
Next

If speed < 1 And unidirectional Then                                  'for unidirectional slow speed only.                                
  For j = 0 To granules
    trend(j) =  present(j-1) * speed + present(j) * (1-speed)
  Next
End If
End Sub


'*********************************************************************
' CHOICE D - SQUARE WAVES.
'*********************************************************************

Sub Choice_D()
If unidirectional Then amplitude = lambda / 4 Else amplitude = lambda / 2
For j = granules / 2 - 2 * lambda To granules / 2 + lambda Step lambda
  For k = 1 To lambda / 2
    present(j+k) = amplitude
    present(j+k+lambda / 2) = -amplitude
  Next
Next

For j = 0 To granules
    If unidirectional Then trend(j) = present(j-1) Else trend(j) = (present(j-1) + present(j+1)) / 2
Next

If speed < 1 And unidirectional Then                                  'for unidirectional slow speed only.                                
  For j = 0 To granules
    trend(j) =  present(j-1) * speed + present(j) * (1-speed)
  Next
End If
End Sub


'*********************************************************************
' CHOICE E - HEAT.
'*********************************************************************

Sub Choice_E()
amplitude = 30: speed = .5
For j = 0 To granules
  x_phase = granules / 2 - j
  Gaussian = 1.00005 ^ (-x_phase ^ 2)
  present(j) =  Rnd * amplitude * Gaussian
  present(j) = -Rnd * amplitude * Gaussian + present(j) 
  trend(j) = present(j)
Next
End Sub


'*********************************************************************
' CHOICE F - STANDING WAVES.
'*********************************************************************

Sub Choice_F()
For j = 0 To granules
  phase = 2 * pi * j / lambda
  present(j) = speed * 6 * Sin(phase)
Next
End Sub


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
'*********************************************************************

Sub Frame()
margin = 10
x_left  = 8 * left_frame - margin - 4
x_right = 8 * left_frame + 8 * x_text + margin - 6
y_top = top * 16 - margin - 16
y_bottom = y_top + 16 * y_text + 2 * margin - 2

Line (x_left, y_top)-(x_right + 1, y_bottom + 1), gray, B
Line (x_left + 1, y_top + 1)-(x_right, y_bottom), black, B
Line (x_left + 1, y_bottom)-(x_right, y_bottom), white
Line (x_left, y_bottom + 1)-(x_right + 1, y_bottom + 1), white
Line (x_right, y_top + 1)-(x_right, y_bottom), white
Line (x_right+ 1, y_top)-(x_right + 1, y_bottom + 1), white
End Sub


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
Windowtitle "WaveMechanics04  -  Waves on a string (1-D)  -  Jocelyn Marcotte's algorithm."
matrix_page = 2
x_center = 512
y_center = 190
If unidirectional Then amplitude = 40 Else amplitude = 80

For j = 0 To granules                                                 'erasing previous data.
  past(j)    = 0
  present(j) = 0
  trend(j)   = 0
Next
present(-1) = 0
present(granules + 1) = 0

Select Case choice
  Case "A": Choice_A()
  Case "B": Choice_B()
  Case "C": Choice_C()
  Case "D": Choice_D()
  Case "E": Choice_E()
  Case "F": Choice_F()
End Select
Screenset matrix_page, matrix_page
Color black, background: Cls
Title()
Text()
Line(0, y_center)-(1023, y_center), gray
Screenset work_page, visible_page
End Sub


'*********************************************************************
' FORCE STANDING WAVES.
'*********************************************************************

Sub Standing_waves()
For j = 0 To granules
  If speed = 1 Then
    trend(j) = (present(j-1) + present(j+1)) / 2                      'simpler formula for speed = 1.
  Else
    trend(j) = (present(j-1) * speed + present(j+1) * speed + 2 * present(j) * (1 - speed)) / 2
End If
Next
End Sub


'*********************************************************************
' TEXT.
'*********************************************************************

Sub Text()
line21  = " A - Gaussian 1-D Ricker wavelet.      "
line22  = " B - Sine waves.                       "
line23  = " C - Sawtooth waves.                   "
line24  = " D - Square waves.                     "
line25  = " E - Heat.                             "
line26  = " F - Standing waves.                   "
line28  = " G - Unidirectional impulse.           "
line29  = " H - Bidirectional impulse.            "
line31  = " J - Full speed: perfect waves.        "
line32  = " K - Slower speed: quantum effect.     "
line34  = " S - Hard reflection.                  "
line35  = " T - Soft reflection.                  "
line36  = " U - Soft (left) and hard (right).     "
line37  = " V - No reflection.                    "
line38  = " W - Endless loop.                     "
line43  = " Q - Force standing waves.             "
line44  = " R - Reset.                            "

line47a = " I - Initialize.   "
line47b = " M - Menu.   "
line47c = " Slow.         "
line48a = " Previous Program. "
line48b = " Quit (Esc.)."
line48c = " Next Program. "

Locate 21
Locate, 3: Print "In January 2006, Mr. Jocelyn Marcotte created a wave algorithm different from Mr."
Locate, 3: Print "Philippe Delmotte's one. He followed a rather complex and different path, yet the"
Locate, 3: Print "final result looks much similar to the well known IIR (infinite impulse response)"
Locate, 3: Print "numeric filter. Mr. Delmotte's algorithm is rather related to Euler's method."
           Print
Locate, 3: Print "Mr. Marcotte's algorithm is amazingly simple:"
           Print
Locate,13: Print "past(x)  =  present(x)"
Locate,13: Print "present(x) =  trend(x)"
Locate,13: Print "trend(x)   =  present(x-1) + present(x+1) - past(x)"
           Print
Locate, 3: Print "The black curve above displays the 'present' energy, which is also converted into"
Locate, 3: Print "green (positive) and red (negative) stripe. The green curve for past energy and"
Locate, 3: Print "the red one for trend give a good indication about the waves' behavior. The blue "
Locate, 3: Print "dotted curve for quadrature is deducted from trend - past. It may also be seen "
Locate, 3: Print "as kinetic energy. Quadrature allows one to estimate the wave's overall energy,  "
Locate, 3: Print "which finally can be displayed as a phase-free black and white stripe."
           Print
Locate, 3: Print "The potential as a purple dotted curve is given by: present(x+1) - present(x-1). "
Locate, 3: Print "Standing waves including their phase are observable in the yellow and blue stripe"
Locate, 3: Print "above thanks to a hypothetic Lagrangian, which is given by: kinetic - potential."
           Print
Locate, 3: Print "This algorithm appears to work like Delmotte's one (June 2005). It is as easy to "
Locate, 3: Print "deal with, as this program clearly shows. Its basic principles, which are quite  "
Locate, 3: Print "different, should be well understood in order to be applied to 2-D and 3-D waves."


Locate 42,89: Print "P - Pause."

Color green_text
x_text = Len(line21) - 1                                              'text width (pixels = x * 8).
y_text = 18                                                           'number of lines (pixels = y * 16).
top = 21                                                              'upper limit.
left_frame= 88                                                        'limit on the left hand side: "Locate top, left".
Frame()
Locate 21,88: Print line21
Locate 22,88: Print line22
Locate 23,88: Print line23
Locate 24,88: Print line24
Locate 25,88: Print line25
Locate 26,88: Print line26
Locate 28,88: Print line28
Locate 29,88: Print line29
Locate 31,88: Print line31
Locate 32,88: Print line32
Locate 34,88: Print line34
Locate 35,88: Print line35
Locate 36,88: Print line36
Locate 37,88: Print line37
Locate 38,88: Print line38
Locate 43,88: Print line43
Locate 44,88: Print line44
Locate 47,42: Print "I - Initialize.    M - Menu.    Slow."
Locate 48,42: Print "Previous Program.  Quit (Esc.). Next Program.";

Color gray
Locate 47, 3: Print "Thanks to the creators of FreeBASIC."
Locate 48, 3: Print "Gabriel LaFreniere  glafreniere.com";
Locate 47,89: Print "May 12, 2009. This program may be"
Locate 48,89: Print "freely distributed, copied or modified.";

Color blue
Select Case choice
  Case "A": Locate 21, 88:  Print line21
  Case "B": Locate 22, 88:  Print line22
  Case "C": Locate 23, 88:  Print line23
  Case "D": Locate 24, 88:  Print line24
  Case "E": Locate 25, 88:  Print line25
  Case "F": Locate 26, 88:  Print line26
End Select

Select Case reflection
  Case "hard": Locate 34, 88:  Print line34
  Case "soft": Locate 35, 88:  Print line35
  Case "both": Locate 36, 88:  Print line36
  Case "none": Locate 37, 88:  Print line37
  Case "loop": Locate 38, 88:  Print line38
End Select

If speed = 1 Then
  Locate 31, 88: Print line31
Else
  Locate 32, 88: Print line32
End If
If unidirectional Then
  Locate 28, 88: Print line28
Else
  Locate 29, 88: Print line29
End If

Color black
End Sub


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Sub Title()
w_title = "Jocelyn Marcotte's algorithm"
Locate 1, 1: Print w_title
For x1 = 0 To 8 * Len(w_title)
  x2 = x_center + 2 * x1 - 8 * Len(w_title)
  For y1 = 1 To 14
    If Point(x1, y1) = black Then
      y2 = 2 * y1 + 2
      Line(x2, y2)-(x2, y2 + 1), gold
      If Point(x1 + 1, y1 - 1) = black Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), gold
      If Point(x1 + 1, y1 + 0) = black Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), gold
      If Point(x1 + 1, y1 + 1) = black Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), gold
    End If
  Next
  If (x1+1) Mod 8 Then Else Line(x2+1, 0)-(x2+1, 34), background      'separate invasive characters such as capital M.
Next
Line(0, 0)-(8 * Len(w_title), 14), background, bf                     'matrix title erased.
For x1 = x_center - 8 * Len(w_title) To x_center + 8 * Len(w_title)   'adding light and shades.
  For y1 = 0 To 34
    If Point(x1, y1) = gold And Point(x1-1, y1-1) = background Then Pset(x1-1, y1-1), buff
    If Point(x1, y1) = gold And Point(x1+1, y1+1) = background Then Pset(x1+1, y1+1), black
  Next
Next
For x = 512 - 8 * Len(w_title) To 512 + 8 * Len(w_title)              'adding luster.
  For y = 4 To 32
    If Point(x, y) = gold Then dark = 9 * Abs(18 - y): Pset(x, y), Rgb(240 - dark, 200 - dark, 120 - dark)
  Next
Next
End Sub
