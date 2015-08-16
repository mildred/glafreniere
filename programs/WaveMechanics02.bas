' This is a FreeBasic program. Created Nov. 2005 by Gabriel LaFreniere. Updated April 30, 2008.
' Please download the editor/compiler from: http://fbide.freebasic.net
' Sub routine procedures are listed in alphabetical order.

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
Dim Shared As Integer y1, y2, y_pixel, y_top,  y_bottom, y_text, y_center, y_mouse, y_point, y_green, y_red, y_black
Dim Shared As Integer unidirectional, work_page, visible_page, matrix_page, granules, luminance, left_frame, wheel, click
Dim Shared As Integer r, g, b, j, k, margin, algorithm, line_number, amplitude, top

granules = 127: work_page = 1: algorithm = 1
Dim Shared As Single force(granules+1), slope(granules+1), influence(granules+1)
Dim Shared As Single trend(granules), present(-1 To granules+1), past(granules)
Dim Shared As Single potential(granules), kinetic(granules), quadrature(granules)
Dim Shared As Single pi = 4 * Atn(1), lambda, phase, Gaussian, speed, Lagrangian(granules), k_Dewavrin
Dim Shared As Single x, y, x_phase, dark, previous, ratio

Dim Shared As String choice, reflection, key, w_title
Dim Shared As String line20, line21, line22, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39
Dim Shared As String line43, line44, line47a, line47b, line47c, line48a, line48b, line48c

choice = "A": lambda = 16: speed = 1: unidirectional = 0: work_page = 1
Screen 20,24,3: Initialization()


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Sleep 20

  force(0) = force(1)                                                 'soft reflection (left).
                                                                      'automatic hard reflection (right).
  influence(1) = force(0) + force(2) - 2 * force(1)                   'influence on granule 1, one iteration in advance.


  For x = 1 To granules

'*********************************************************************
' THIS IS MY TRANSPOSITION OF PHILIPPE DELMOTTE'S VIRTUAL WAVE
' ALGORITHM (JUNE 2005) USING NEWTON'S LAWS AND VERLET'S ALGORITHM.
'*********************************************************************

    influence(x+1) = force(x) + force(x+2) - 2 * force(x+1)           'influence based on force difference.
    slope(x)  = slope(x) + influence(x)  * speed                      'force difference. Cosine curve for sine waves.
    force(x)  = force(x) + slope(x)                                   'wave curve. Sine curve for sine waves.

'*********************************************************************
' END OF ALGORITHM. - THREE PROGRAM LINES ONLY.
'*********************************************************************

    Circle(x * 8, y_center - slope(x) / ratio), 2.5,   green
    Paint (x * 8, y_center - slope(x) / ratio), green, green
    Circle(x * 8, y_center - force(x)), 2.5,   black
    Paint (x * 8, y_center - force(x)), black, black
  Next


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

  key = Inkey
  If Len(key) Then
    If Len(key) = 2 Then key = Right(key, 1) + "+" Else key = Ucase(key)
    Select Case key
      Case Chr(27), "k+": End                                         'Escape key or Windows' X quit button.
      Case "A":  choice = "A"                                         'four choices.
      Case "B":  choice = "B"
      Case "C":  choice = "C"
      Case "D":  choice = "D"
      Case "E":  unidirectional = 1                                   'one-way impulse.
      Case "F":  unidirectional = 0                                   'bidirectional impulse.
      Case "G":  If speed < 1 Then                                    'full speed.
                 key = ""  
                 speed = 1: Screenset matrix_page
                 Color blue, background: Locate 35, 88: Print line35
                 Color green_text:       Locate 36, 88: Print line36
                 End If
      Case "H":  If speed = 1 Then                                    'slower speed.
                 key = ""  
                 speed = .9: Screenset matrix_page
                 Color blue, background: Locate 36, 88: Print line36
                 Color green_text:       Locate 35, 88: Print line35
                 End If
      Case "I":  choice = "A": speed = 1: unidirectional = 0          'initialization.
      Case "M":  key = "": Run "WaveMechanics00.exe"                  'main menu.
      Case "P":  Screenset visible_page                               'pause.
                 Color red, background: Locate 45, 89
                 Print "P - Paused. Press any key to resume.   "
                 key = "": Sleep
                 Color black: Screenset work_page, visible_page
      Case "R":                                                       'reset via initialization.
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
    Case 27: Locate 27, 88
             If choice <> "A" Then
               Print line27
               If click Then choice = "A": Initialization()
             End If               
    Case 28: Locate 28, 88
             If choice <> "B" Then
               Print line28
               If click Then choice = "B": Initialization()
             End If               
    Case 29: Locate 29, 88
             If choice <> "C" Then
               Print line29
               If click Then choice = "C": Initialization()
             End If               
    Case 30: Locate 30, 88
             If choice <> "D" Then
               Print line30
               If click Then choice = "D": Initialization()
             End If               
    Case 32: Locate 32, 88
             If unidirectional = 0 Then
               Print line32
               If click Then unidirectional = 1: Initialization()
             End If               
    Case 33: Locate 33, 88
             If unidirectional = 1 Then
               Print line33
               If click Then unidirectional = 0: Initialization()
             End If               
    Case 35: Locate 35, 88
             If speed < 1 Then
               Print line35
               If click Then
                 speed = 1: Screenset matrix_page
                 Color blue, background: Locate 35, 88: Print line35
                 Color green_text:       Locate 36, 88: Print line36
               End If
             End If               
    Case 36: Locate 36, 88
             If speed = 1 Then
               Print line36
               If click Then
                 speed = .9: Screenset matrix_page
                 Color blue, background: Locate 36, 88: Print line36
                 Color green_text:       Locate 35, 88: Print line35
               End If
             End If               
    Case 47: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 47, 73: Print line47c: Sleep 200 'slow.
                              If click Then Sleep 1000                'slower.
               Case Is > 472: Locate 47, 60: Print line47b
                              If click Then Run "WaveMechanics00.exe" 'main menu.
               Case Is > 318: Locate 47, 41: Print line47a
                              If click Then                           'initialization.
                                choice = "A": speed = 1: unidirectional = 0
                                Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
                                Initialization()
                              End If
             End Select
    Case 48: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 48, 73: Print line48c;
                              If click Then Run "WaveMechanics03.exe" 'next program.
               Case Is > 472: Locate 48, 60: Print line48b;
                              If click Then End                       'quit.
               Case Is > 318: Locate 48, 41: Print line48a;
                              If click Then Run "WaveMechanics01.exe" 'previous program.
             End Select
  End Select
Loop


'*********************************************************************
' CHOICE A - GAUSSIAN IMPULSE AND RESULTING RICKER WAVELET.
'*********************************************************************

Sub Choice_A()
  If unidirectional Then amplitude = 3 * lambda Else amplitude = 6 * lambda
  For j = 0 To granules + 1
    x = j - granules / 2
    Gaussian = 1.008 ^ (-x ^ 2)
    force(j) = amplitude * Gaussian
    slope(j) = 0                                                      'no slope for standing waves (bidirectional).
  '  slope(j) = amplitude * Gaussian * x / 63                         'check Jocelyn Marcotte's interesting formula.
  Next
  
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
  If unidirectional Then amplitude = 2 * lambda Else amplitude = 4 * lambda
  For j = 0 To granules + 1
    x = granules / 2 - j
    phase = 2 * pi * x / lambda
    Gaussian = 1.002 ^ (-x ^ 2)
    force(j) = amplitude * Gaussian * Sin(phase)
    slope(j) = 0                                                      'no slope for standing waves (bidirectional).
  Next
  
  If unidirectional Then
    For j = 1 To granules
      slope(j) = Sqr(speed) * (force(j) - force(j+1))
    Next
  End If
End Sub


'*********************************************************************
' CHOICE C - SAWTOOTH WAVES.
'*********************************************************************

Sub Choice_C()
  For j = 0 To granules + 1
    force(j) = 0
  Next
  
  If unidirectional Then amplitude = lambda Else amplitude = 2 * lambda
  y_point = amplitude
  For j = granules / 2 - 2 * lambda To granules / 2 + 2 * lambda
    If y_point < -amplitude Then y_point = amplitude
    force(j) = y_point
    y_point -= amplitude / 8
  Next
  
  For j = 1 To granules                                               'sawtooth and square waves need more sophisticated
    If unidirectional Then                                            'treatment for standing waves (bidirectional)
         slope(j) = Sqr(speed) * (force(j) - force(j+1))              'probably because they contain harmonics.
    Else slope(j) = Sqr(speed) * (force(j) - force(j+1) / 2 - force(j-1) / 2)
    End If
  Next
End Sub


'*********************************************************************
' CHOICE D - SQUARE WAVES.
'*********************************************************************

Sub Choice_D()
  If unidirectional Then amplitude = lambda Else amplitude = 2 * lambda
  For j = 0 To granules + 1
    force(j) = 0: slope(j) = 0
  Next
  
  For j = granules / 2 - 2 * lambda To granules / 2 + lambda Step lambda
    For k = 1 To lambda / 2
      force(j+k) = amplitude
      force(j+k+lambda / 2) = -amplitude
    Next
  Next
  
  For j = 1 To granules
    If unidirectional Then
         slope(j) = Sqr(speed) * (force(j) - force(j+1))
    Else slope(j) = Sqr(speed) * (force(j) - force(j+1) / 2 - force(j-1) / 2)
    End If
  Next
End Sub


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
'*********************************************************************

Sub Frame()
  margin = 10
  x_left  = 8 * x_left - margin - 4
  x_right = 8 * x_left + 8 * x_text + margin - 6
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
  Windowtitle "WaveMechanics02 - Waves on a string (one dimension)."
  matrix_page = 2
  ratio = Sqr(speed) * 2 * pi / lambda
  x_center = 512
  y_center = 180
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Title()
  Text()
  Select Case choice
    Case "A": Choice_A()
    Case "B": Choice_B()
    Case "C": Choice_C()
    Case "D": Choice_D()
  End Select
  Circle(0, y_center), 2.5, black
  Paint(0, y_center), black, black
  Circle(1024, y_center), 2.5, black
  Paint(1023, y_center), black, black
  Line(0,y_center)-(1023, y_center), gray
End Sub


'*********************************************************************
' TEXT.
'*********************************************************************

Sub Text()
  Locate 1, 5 : Print "Left:  soft reflection."
  Locate 1,102: Print "Right: hard reflection."
  line27  = " A - Gaussian 1-D Ricker wavelet.      "
  line28  = " B - Sine waves.                       "
  line29  = " C - Sawtooth waves.                   "
  line30  = " D - Square waves.                     "
  line32  = " E - Unidirectional impulse.           "
  line33  = " F - Bidirectional impulse.            "
  line35  = " G - Full speed: perfect waves.        "
  line36  = " H - Slower speed: quantum effect.     "
  
  line47a = " I - Initialize.   "
  line47b = " M - Menu.   "
  line47c = " Slow.         "
  line48a = " Previous Program. "
  line48b = " Quit (Esc.)."
  line48c = " Next Program. "
  
  Locate 22
  Locate, 3: Print "Mr. Philippe Delmotte elaborated his new virtual medium on June 2005. His simple"
  Locate, 3: Print "and truly amazing algorithm is capable of simulating all kind of waves. Although"
  Locate, 3: Print "many algorithms had already been proposed before 2005, especially for video"
  Locate, 3: Print "games, none of them could reproduce waves with such a perfection."
             Print
  Locate, 3: Print "Because waves on a string are involving only one dimension, each so-called aether"
  Locate, 3: Print "granule is influenced by its two nearest neighbors according to their force (or"
  Locate, 3: Print "extension) difference. So the algorithm's first line goes like this:"
             Print
  Locate, 3: Print "           influence(x) = force(x-1) + force(x+1) - 2 * force(x)"
             Print
  Locate, 3: Print "Secondly, because this force or extension difference is actually the curve slope,"
  Locate, 3: Print "influence modifies the slope this way:"
             Print
  Locate, 3: Print "           slope(x)  = slope(x) + influence(x)"
             Print
  Locate, 3: Print "Finally, the new force is modified according to the new slope. It is that simple:"
             Print
  Locate, 3: Print "           force(x)  = force(x) + slope(x)"
             Print
  Locate, 3: Print "The black curve above indicates the force or extension, according to Hookes' law,"
  Locate, 3: Print "and the green one indicates the slope, which is equivalent to quadrature for sine"
  Locate, 3: Print "waves. This allows one to easily control Delmotte's algorithm, which was somewhat"
  Locate, 3: Print "different in its original form."
  
  Locate 45, 89: Print "P - Pause.  R - Reset."
  
  Color green_text
  x_text = Len(line27) - 1                                            'text width (pixels = x * 8).
  y_text = 10                                                         'number of lines (pixels = y * 16).
  top = 27                                                            'upper limit.
  x_left= 88                                                          'limit on the left hand side: "Locate top, left".
  frame()
  Locate 27,88: Print line27
  Locate 28,88: Print line28
  Locate 29,88: Print line29
  Locate 30,88: Print line30
  Locate 32,88: Print line32
  Locate 33,88: Print line33
  Locate 35,88: Print line35
  Locate 36,88: Print line36
  Locate 47,42: Print "I - Initialize.    M - Menu.    Slow."
  Locate 48,42: Print "Previous Program.  Quit (Esc.). Next Program.";
  
  Color gray
  Locate 47, 3: Print "Thanks to the creators of FreeBASIC."
  Locate 48, 3: Print "Gabriel LaFreniere  glafreniere.com";
  Locate 47,89: Print "May 12, 2009. This program may be"
  Locate 48,89: Print "freely distributed, copied or modified.";
  
  Color blue
  Select Case choice
    Case "A": Locate 27, 88: Print line27
    Case "B": Locate 28, 88: Print line28
    Case "C": Locate 29, 88: Print line29
    Case "D": Locate 30, 88: Print line30
  End Select
  If speed = 1 Then Locate 35, 88 Else Locate 36, 88
  If speed < 1 Then Print line36 Else Print line35
  If unidirectional Then Locate 32, 88 Else Locate 33, 88
  If unidirectional Then Print line32 Else Print line33
  Color black
End Sub


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Sub Title()
  w_title = "Waves on a string"
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