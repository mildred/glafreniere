' This is a FreeBasic program. Created Nov. 2005 by Gabriel LaFreniere.
' Please download the IDE (editor) from: http://fbide.freebasic.net
' Sub routine procedures are listed in alphabetical order.
' This source code was adapted to the 2008 0.20.0b Compiler for Windows available from:
' http://www.freebasic.net/index.php/download
' It should still be compatible with previous compilers.

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

Const red   = Rgb(255,000,000), green       = Rgb(000,200,000)
Const blue  = Rgb(000,000,255), white       = Rgb(255,255,255)
Const gold  = Rgb(180,150,100), dark_gray   = Rgb(075,075,075)
Const gray  = Rgb(125,125,125), green_text  = Rgb(000,125,000)
Const buff  = Rgb(255,255,200), background  = Rgb(225,225,225)
Const cyan  = Rgb(000,100,100), light_green = Rgb(175,255,175)
Const black = Rgb(0,0,0)

Dim Shared As Integer x1, x2, x_pixel, x_left, x_right,  x_text, x_center, x_mouse
Dim Shared As Integer y1, y2, y_pixel, y_top,  y_bottom, y_text, y_center, y_mouse,  y_green, y_red, y_black
Dim Shared As Integer unidirectional, work_page, visible_page, matrix_page, granules, luminance, left_frame, wheel, click
Dim Shared As Integer r, g, b, j, k, margin, algorithm, line_number, amplitude, top

granules = 1024: work_page = 1: algorithm = 1
Dim Shared As Single trend(granules), present(-1 To granules+1), past(granules)
Dim Shared As Single potential(granules), kinetic(granules), quadrature(granules)
Dim Shared As Single pi = 4 * Atn(1), lambda, phase, Gaussian, speed, Lagrangian(granules), k_Dewavrin
Dim Shared As Single force(granules + 2), slope(granules + 2), influence(granules + 2)
Dim Shared As Single x, y, x_phase, dark, previous, ratio

Dim Shared As String choice, reflection, key, w_title
Dim Shared As String line20, line21, line22, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39
Dim Shared As String line43, line44, line47a, line47b, line47c, line48a, line48b, line48c


choice = "B": lambda = 1024 / 16: speed = 1: unidirectional = 1: reflection = "both"
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
' reflection.
'*********************************************************************

  Select Case reflection
    Case "hard"                                                       'hard reflection: no action (default).
    Case "soft"
      force(0) = force(1)                                             'soft reflection on both sides.
      force(granules) = force(granules-1)
    Case "both"                                                       'hard reflection (right, no action).
      force(0) = force(1)                                             'soft reflection (left).
    Case "none"
      force(0) = force(1) - slope(1) / Sqr(speed)                     'full damping on both sides.
      force(granules) = force(granules-1) - slope(granules-1) / Sqr(speed)
    Case "loop"                                                       'looping influence firstly, further action below.
      influence(granules) = force(granules-1) + force(0) - 2 * force(granules)
      influence(0) = force(granules) + force(1) - 2 * force(0)
  End Select


'*********************************************************************
' THIS IS MY TRANSPOSITION OF PHILIPPE DELMOTTE'S VIRTUAL WAVE
' ALGORITHM (JUNE 2005) USING NEWTON'S LAWS AND VERLET'S ALGORITHM.
'*********************************************************************

  For x = 1 To granules - 1
    influence(x) = force(x-1) + force(x+1) - 2 * force(x)             'influence based on force difference,
  Next                                                                'one iteration in advance.
  For x = 1 To granules - 1

    slope(x)  = slope(x) + influence(x) * speed ^ 2                   'force difference. Cosine curve for sine waves.
    force(x)  = force(x) + slope(x)                                   'wave curve. Sine curve for sine waves.

' END OF ALGORITHM - NEEDS THREE PROGRAM LINES ONLY !


'*********************************************************************
' DISPLAYING WAVE CURVES.
'*********************************************************************


    If x = 1 Then
      y_black = y_center - force(1)                                   'initializing previous state.
      y_green = y_center - slope(1) / ratio
    End If
    Line(x-1, y_green)-(x, y_center - slope(x) / ratio), green
    Line(x-1, y_black)-(x, y_center - force(x)), black
    y_black = y_center - force(x)
    y_green = y_center - slope(x) / ratio
    luminance = Sqr(Abs(1500 * force(x)))
    If luminance > 255 Then luminance = 255    
    If force(x) > 0 Then
      Line(x, y_center - 115)-(x, y_center - 100), Rgb(0, luminance, luminance / 2)
    Else
      Line(x, y_center - 115)-(x, y_center - 100), Rgb(luminance, 0, luminance / 2)
    End If
    luminance = Sqr(1000 * Sqr(force(x) ^ 2 + (slope(x) / ratio) ^ 2))'maximum amplitude (black and white stripe).
    If luminance > 255 Then luminance = 255    
    Line(x, y_center - 130)-(x, y_center - 115), Rgb(luminance, luminance, luminance)
    Line(0, y_center - 130)-(1023, y_center - 100), black, b
    Line(0, y_center - 115)-(1023, y_center - 115), black
  Next

  If reflection = "loop" Then                                         'joining both ends for endless loop.
    slope(granules)  = slope(granules) + influence(granules) * speed
    force(granules)  = force(granules) + slope(granules)
    slope(0)  = slope(0) + influence(0) * speed
    force(0)  = force(0) + slope(0)
  End If


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

  key = Inkey
  If Len(key) Then
    If Len(key) = 2 Then key = Right(key, 1) + "+" Else key = Ucase(key)
    Select Case key
      Case Chr(27), "k+": End                                         'Escape key or Windows' X quit button.
      Case "A": choice = "A": reflection = "both": unidirectional = 0
      Case "B": choice = "B"
      Case "C": choice = "C"
                speed = .5: unidirectional = 1: reflection = "loop"
      Case "D": choice = "D"
                speed = .5: unidirectional = 1: reflection = "loop"
      Case "E": choice = "E": speed = .5: reflection = "none"
      Case "F": choice = "F": reflection = "hard"
      Case "G": unidirectional = 1                                    'one-way impulse.
      Case "H": unidirectional = 0                                    'bidirectional impulse.
      Case "J": If speed < 1 Then speed = 1                           'full speed.
      Case "K": If speed = 1 Then speed = .5                          'slower speed.
      Case "I": choice = "B": speed = 1                               'initialization.
                reflection = "both": unidirectional = 0
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
                 choice = "A": reflection = "both"
                 unidirectional = 0: Initialization()
               End If
             End If
    Case 22: Locate 22, 88
             If choice <> "B" Then
               Print line22
               If click Then choice = "B": Initialization()
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
                 choice = "F"
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
               If Not choice = "E" Then                               '"heat" works with fractionary step only.
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
               If click Then reflection = "loop": unidirectional = 1: Initialization()
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
                                choice = "B": speed = 1: unidirectional = 0
                                Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
                                Initialization()
                              End If
             End Select
    Case 48: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 48, 73: Print line48c;
                              If click Then Run "WaveMechanics04.exe" 'next program.
               Case Is > 472: Locate 48, 60: Print line48b;
                              If click Then End                       'quit.
               Case Is > 318: Locate 48, 41: Print line48a;
                              If click Then Run "WaveMechanics02.exe" 'previous program.
             End Select
  End Select
Loop


'*********************************************************************
' CHOICE A - GAUSSIAN IMPULSE AND RESULTING RICKER WAVELET.
'*********************************************************************

Sub Choice_A()
  ratio = .05 * speed
  For j = 0 To granules + 1
    x = j - granules / 2
    Gaussian = 1.0005 ^ (-x ^ 2)
    force(j) = amplitude * Gaussian
  ' slope(j) = amplitude * Gaussian * x / 1000                        'check Jocelyn Marcotte's interesting formula
  Next                                                                'for unidirectional traveling Ricker wavelet.
  
  If unidirectional Then                                              'skip this for standing waves (slope = 0).
    For j = 1 To granules
      slope(j) = Sqr(speed) * (force(j) - force(j+1))                 'rightward.
  '   slope(j) = sqr(speed) * (force(j) - force(j-1))                 'leftward also possible.
    Next
  End If
End Sub

'*********************************************************************
' CHOICE B - GAUSSIAN-DAMPED SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Choice_B()
  ratio = (2 * pi / lambda) * speed
  For j = 0 To granules + 1
    x = granules / 2 - j
    phase = 2 * pi * x / lambda
    Gaussian = 1.00005 ^ (-x ^ 2)
    force(j) = amplitude * Gaussian * Sin(phase)
  Next
  
  If unidirectional Then                                              'same procedure as above.
    For j = 1 To granules
      slope(j) = Sqr(speed) * (force(j) - force(j+1))
    Next
  End If
End Sub


'*********************************************************************
' CHOICE C - SAWTOOTH WAVES.
'*********************************************************************

Sub Choice_C()
  ratio = .5  * speed
  If unidirectional Then amplitude = lambda / 4 Else amplitude = lambda / 2
  
  
  y = amplitude
  For j = granules / 2 - 2 * lambda To granules / 2 + 2 * lambda
    If y < -amplitude Then y = amplitude
    force(j) = y
    y -= amplitude / (lambda / 2)
  Next
  
  If unidirectional Then
    For j = 1 To granules
      slope(j) = Sqr(speed) * (force(j) - force(j+1))
    Next
    Else Standing_waves()                                             'this procedure may be skipped for sine waves,
  End If                                                              'but sawtooth waves need additional processing
End Sub                                                               '  for obtaining standing waves (bidirectional).


'*********************************************************************
' CHOICE D - SQUARE WAVES.
'*********************************************************************

Sub Choice_D()
  ratio = 1 * speed
  If unidirectional Then amplitude = lambda / 4 Else amplitude = lambda / 2
  
  For j = 0 To granules + 1
    force(j) = 0: slope(j) = 0
  Next
  
  For j = granules / 2 - 2 * lambda To granules / 2 + lambda Step lambda
    For k = 1 To lambda / 2
      force(j+k) = amplitude
      force(j+k+lambda / 2) = -amplitude
    Next
  Next
  
  If unidirectional Then
    For j = 1 To granules
      slope(j) = Sqr(speed) * (force(j) - force(j+1))
    Next
    Else Standing_waves()                                             'same additional procedure as above.
  End If
End Sub


'*********************************************************************
' CHOICE E - HEAT.
'*********************************************************************

Sub Choice_E()
  ratio = .3 * speed
  For j = 0 To granules + 1
    x = granules / 2 - j
    Gaussian = 1.00005 ^ (-x ^ 2)
    force(j) = Rnd * 20 * Gaussian
    force(j) = force(j) - Rnd * 20 * Gaussian
  Next
  Standing_waves()
End Sub


'*********************************************************************
' CHOICE F - STANDING WAVES.
'*********************************************************************

Sub Choice_F()
  amplitude = 100
  ratio = (2 * pi / lambda) * speed
  For j = 1 To granules
    phase = 2 * pi * j / lambda
    force(j) = .6 * amplitude * Sin(phase)
  Next
End Sub


'*********************************************************************
' CLEAR STRING DATA ON THE LEFT HAND SIDE.
'*********************************************************************

Sub Clear_left()
  For j = 0 To granules / 3
    force(j) = force(j) * j / (granules / 3)
    slope(j) = slope(j) * j / (granules / 3)
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
  Windowtitle "WaveMechanics03  -  Waves on a string (1-D)  -  Philippe Delmotte's algorithm."
  matrix_page = 2
  x_center = 512
  y_center = 170
  If unidirectional Then amplitude = 50 Else amplitude = 100
  For j = 0 To granules + 1
    force(j) = 0
    slope(j) = 0
  Next
  Select Case choice
    Case "A": Choice_A()
    Case "B": Choice_B()
    Case "C": Choice_C()
    Case "D": Choice_D()
    Case "E": Choice_E(): speed = .5                                  '"heat" works with fractionary step only.
    Case "F": Choice_F()
  End Select
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Title()
  Text()
  Line(0,y_center)-(1023, y_center), gray
  Screenset work_page, visible_page
End Sub


'*********************************************************************
' STANDING WAVES.
'*********************************************************************

Sub Standing_waves()
  For j = 1 To granules
    slope(j) = Sqr(speed) * (force(j) - force(j+1) / 2 - force(j-1) / 2)
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
  Locate, 3: Print "Mr. Philippe Delmotte's algorithm is capable of reproducing practically any wave "
  Locate, 3: Print "phenomenon. It is very easy to deal with, too. I managed to make the source code "
  Locate, 3: Print "the clearest and simplest possible. Please pay attention to this amazing and     "
  Locate, 3: Print "valuable invention. It will become famous and unavoidable in the future because  "
  Locate, 3: Print "matter and all forces are truly involving waves."
             Print
  Locate, 3: Print "This program explains how to launch a wave train on a string and how to control  "
  Locate, 3: Print "basic properties such as reflection. Further programs will add more elaborated   "
  Locate, 3: Print "procedures for 2-D and finally, 3-D virtual wave medium."
             Print
  Locate, 3: Print "It should be emphasized that the algorithm basically transfers wave data from one"
  Locate, 3: Print "granule to its nearest neighbor. On the contrary, fractional transfer introduces "
  Locate, 3: Print "quantum effects. This is clearly visible for sawtooth or square waves. In such a "
  Locate, 3: Print "case, higher harmonics become slower. So they are progressively left behind. The "
  Locate, 3: Print "resulting wave copied to a .wav file should be especially melodious on condition "
  Locate, 3: Print "that the slower noise is eliminated. Some energy is actually converted into heat."
             Print
  Locate, 3: Print "Delmotte's algorithm will also be useful for opticians and acousticians. All wave"
  Locate, 3: Print "phenomena such as the Airy Disk and Fresnel's diffraction may be experimented. As"
  Locate, 3: Print "a matter of fact, this virtual wave medium is a powerful and efficient laboratory."
             Print
  Locate, 3: Print "The Huygens Principle now appears to be true. It is no longer a convenient way to"
  Locate, 3: Print "predict how waves should behave, it IS how waves do behave. One must deal with a "
  Locate, 3: Print "new fact, though: because of its energy transfer mechanism, the medium granular  "
  Locate, 3: Print "structure reveals its quantum properties."
  
  Locate 42,89: Print "P - Pause."
  
  Color green_text
  x_text = Len(line21) - 1                                            'text width (pixels = x * 8).
  y_text = 18                                                         'number of lines (pixels = y * 16).
  top = 21                                                            'upper limit.
  left_frame= 88                                                      'limit on the left hand side: "Locate top, left".
  frame()
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
  Locate 47,89: Print "May 7, 2009. This program may be"
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
  w_title = "Philippe Delmotte's algorithm"
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
    If (x1+1) Mod 8 Then Else Line(x2+1, 0)-(x2+1, 34), background    'separate invasive characters such as capital M.
  Next
  Line(0, 0)-(8 * Len(w_title), 14), background, bf                   'matrix title erased.
  For x1 = x_center - 8 * Len(w_title) To x_center + 8 * Len(w_title) 'adding light and shades.
    For y1 = 0 To 34
      If Point(x1, y1) = gold And Point(x1-1, y1-1) = background Then Pset(x1-1, y1-1), buff
      If Point(x1, y1) = gold And Point(x1+1, y1+1) = background Then Pset(x1+1, y1+1), black
    Next
  Next
  For x = 512 - 8 * Len(w_title) To 512 + 8 * Len(w_title)            'adding luster.
    For y = 4 To 32
      If Point(x, y) = gold Then dark = 9 * Abs(18 - y): Pset(x, y), Rgb(240 - dark, 200 - dark, 120 - dark)
    Next
  Next
End Sub
