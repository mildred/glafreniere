' This is a FreeBasic program. Created Nov. 2005 by Gabriel LaFreniere. Updated April 20, 2008.
' Please download the editor/compiler from: http://fbide.freebasic.net
' Sub routine procedures are listed in alphabetical order.

granules = 1024: work.page = 1: algorithm = 1
Dim As Double force(granules + 2), slope(granules + 2), influence(granules + 2)
Dim As Double pi = 4 * Atn(1), lambda, phase, Gaussian, speed, ratio, x, y
choice$ = "C": lambda = 1024 / 8: speed = .9999: unidirectional = 1: reflection$ = "loop"
Screen 20,24,3: Gosub Initialization


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  Swap work.page, visible.page
  Screenset work.page, visible.page
  Pcopy matrix.page, work.page

  If speed < 1 And speed > .1 Then speed -= .0001                     'progressive transition.
  y.black = y.center - force(1)                                       'initialize previous state.
  y.green = y.center - slope(1) / ratio


'*********************************************************************
' reflection.
'*********************************************************************

  Select Case reflection$
    Case "hard"                                                       'hard reflection: no action (default).
    Case "soft"
      force(0) = force(1)                                             'soft reflection on both sides.
      force(granules) = force(granules-1)
    Case "both"                                                       'default hard reflection (right).
      force(0) = force(1)                                             'soft reflection (left).
    Case "none"
      force(0) = force(1) - slope(1)                                  'full damping on both sides.
      force(granules) = force(granules-1) - slope(granules-1)
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

    slope(x)  = slope(x) + influence(x) * speed                       'force difference. Cosine curve for sine waves.
    force(x)  = force(x) + slope(x)                                   'wave curve. Sine curve for sine waves.

'*********************************************************************
' END OF ALGORITHM - NEEDS THREE PROGRAM LINES ONLY !
'*********************************************************************

    Line(x-1, y.green)-(x, y.center - slope(x) / ratio), green
    Line(x-1, y.black)-(x, y.center - force(x)), black
    y.black = y.center - force(x)
    y.green = y.center - slope(x) / ratio
    luminance = Sqr(Abs(1500 * force(x)))
    If luminance > 255 Then luminance = 255    
    If force(x) > 0 Then
      Line(x, y.center - 120)-(x, y.center - 100), Rgb(0, luminance, luminance / 2)
    Else
      Line(x, y.center - 120)-(x, y.center - 100), Rgb(luminance, 0, luminance / 2)
    End If
  Next

  If reflection$ = "loop" Then                                        'joining end to beginning for endless loop.
    slope(granules)  = slope(granules) + influence(granules) * speed
    force(granules)  = force(granules) + slope(granules)
    slope(0)  = slope(0) + influence(0) * speed
    force(0)  = force(0) + slope(0)
  End If


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

  key$ = Inkey
  If Len(key$) Then
    If Len(key$) = 2 Then key$ = Right(key$, 1) + "+" Else key$ = Ucase(key$)
    Select Case key$
      Case Chr(27), "k+": End                                         'Escape key or Windows' X quit button.
      Case "A": choice$ = "A"                                         'four choices.
      Case "B": choice$ = "B"
      Case "C": choice$ = "C"
      Case "D": choice$ = "D"
      Case "E": choice$ = "E": speed = .1: reflection$ = "none"
      Case "G": unidirectional = 1                                    'one-way impulse.
      Case "H": unidirectional = 0                                    'bidirectional impulse.
      Case "J": If speed < 1 Then speed = 1                           'full speed.
      Case "K": If speed = 1 Then speed = .9999                       'slow speed.
      Case "I": lambda = 16: choice$ = "A"                            'initialization.
                speed = 1: unidirectional = 1
      Case "M": Run "WaveMechanics00.exe"                             'main menu.
      Case "P": Screenset visible.page                                'pause.
                Color red, background: Locate 45, 89
                Print "P - Paused. Press any key to resume.   "
                key$ = "": Sleep
                Color black: Screenset work.page, visible.page
      Case "Q": For j = 0 To granules + 1
                  slope(j) = 0
                Next
                key$ = ""
      Case "R":                                                       'reset via initialization.
      Case "S": reflection$ = "hard"
      Case "T": reflection$ = "soft"
      Case "U": reflection$ = "both"
      Case "V": reflection$ = "none"
      Case "W": reflection$ = "loop"
      Case Else: key$ = ""                                            'avoid initialization.
    End Select
    If Len(key$) Then Gosub Initialization
    Do: Loop While Len(Inkey)                                         'clear buffer.
  End If


'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

  Getmouse x.mouse, y.mouse, wheel, click
  line.number = .5 + y.mouse / 16
  If line.number < 46 And x.mouse < 695 Then line.number = 0
  Color green.text, white
  Select Case line.number
    Case 21: Locate 21, 88
             If choice$ <> "A" Then
               Print line21$
               If click Then choice$ = "A": Gosub Initialization
             End If
    Case 22: Locate 22, 88
             If choice$ <> "B" Then
               Print line22$
               If click Then choice$ = "B": Gosub Initialization
             End If
    Case 23: Locate 23, 88
             If choice$ <> "C" Then
               Print line23$
               If click Then choice$ = "C": Gosub Initialization
             End If
    Case 24: Locate 24, 88
             If choice$ <> "D" Then
               Print line24$
               If click Then choice$ = "D": Gosub Initialization
             End If
    Case 25: Locate 25, 88
             If choice$ <> "E" Then
               Print line25$
               If click Then
                 choice$ = "E": speed = .1
                 reflection$ = "none": Gosub Initialization
               End If
             End If
    Case 28: Locate 28, 88
             If unidirectional = 0 Then
               Print line28$
               If click Then unidirectional = 1: Gosub Initialization
             End If
    Case 29: Locate 29, 88
             If unidirectional = 1 Then
               Print line29$
               If click Then unidirectional = 0: Gosub Initialization
             End If
    Case 31: Locate 31, 88
             If speed < 1 Then
               If Not choice$ = "E" Then                              '"heat" works with fractionary step only.
                 Print line31$
                 If click Then speed = 1: Gosub Initialization
               End If
             End If
    Case 32: Locate 32, 88
             If speed = 1 Then
               Print line32$
               If click Then speed = .9999: Gosub Initialization
             End If
    Case 34: Locate 34, 88
             If Not reflection$ = "hard" Then
               Print line34$
               If click Then reflection$ = "hard": Gosub Initialization
             End If
    Case 35: Locate 35, 88
             If Not reflection$ = "soft" Then
               Print line35$
               If click Then reflection$ = "soft": Gosub Initialization
             End If
    Case 36: Locate 36, 88
             If Not reflection$ = "both" Then
               Print line36$
               If click Then reflection$ = "both": Gosub Initialization
             End If
    Case 37: Locate 37, 88
             If Not reflection$ = "none" Then
               Print line37$
               If click Then reflection$ = "none": Gosub Initialization
             End If
    Case 38: Locate 38, 88
             If Not reflection$ = "loop" Then
               Print line38$
               If click Then reflection$ = "loop": Gosub Initialization
             End If
    Case 47: Select Case x.mouse
               Case Is > 700
               Case Is > 576: Locate 47, 73: Print line47c$: Sleep 200'slow.
                              If click Then Sleep 1000                'slower.
               Case Is > 472: Locate 47, 60: Print line47b$
                              If click Then Run "WaveMechanics00.exe" 'main menu.
               Case Is > 318: Locate 47, 41: Print line47a$
                              If click Then                           'initialization.
                                lambda = 16: choice$ = "A": speed = 1: unidirectional = 0
                                Do: Getmouse a,b,c, click: Loop While click
                                Gosub Initialization
                              End If
             End Select
    Case 48: Select Case x.mouse
               Case Is > 700
               Case Is > 576: Locate 48, 73: Print line48c$;
                              If click Then Run "WaveMechanics03.exe" 'next program.
               Case Is > 472: Locate 48, 60: Print line48b$;
                              If click Then End                       'quit.
               Case Is > 318: Locate 48, 41: Print line48a$;
                              If click Then Run "WaveMechanics01.exe" 'previous program.
             End Select
  End Select
Loop


'*********************************************************************
' CHOICE A - GAUSSIAN IMPULSE AND RESULTING RICKER WAVELET.
'*********************************************************************

Choice.A:
ratio = .05
For j = 0 To granules + 1
  x = j - granules / 2
  Gaussian = 1.0005 ^ (-x ^ 2)
  force(j) = amplitude * Gaussian
  slope(j) = 0
' slope(j) = amplitude * Gaussian * x / 1000                          'check Jocelyn Marcotte's interesting formula.
Next

If unidirectional Then                                                'skip this for standing waves.
  For j = 1 To granules
    slope(j) = force(j) - force(j+1)                                  'rightward.
'   slope(j) = force(j) - force(j-1)                                  'leftward.
  Next
End If
Return


'*********************************************************************
' CHOICE B - GAUSSIAN-DAMPED SINUSOIDAL IMPULSE.
'*********************************************************************

Choice.B:
ratio = 2 * pi / lambda
For j = 0 To granules + 1
  x = granules / 2 - j
  phase = 2 * pi * x / lambda
  Gaussian = 1.00005 ^ (-x ^ 2)
  force(j) = amplitude * Gaussian * Sin(phase)
  slope(j) = 0
Next

If unidirectional Then
  For j = 1 To granules
    slope(j) = force(j) - force(j+1)
  Next
End If
Return


'*********************************************************************
' CHOICE C - SAWTOOTH WAVES.
'*********************************************************************

Choice.C:
ratio = 1

For j = 0 To granules + 1
  force(j) = 0: slope(j) = 0
Next

y = -lambda / 4
For j = granules / 2 - 2 * lambda To granules / 2 + 2 * lambda
  If y > lambda / 4 Then y = -lambda / 4
  force(j) = y: y += .5
Next

For j = 1 To granules
  If unidirectional Then
       slope(j) = force(j) - force(j+1)
  Else slope(j) = force(j) - force(j+1) / 2 - force(j-1) / 2
  End If
Next
Return


'*********************************************************************
' CHOICE D - SQUARE WAVES.
'*********************************************************************

Choice.D:
ratio = 1
If unidirectional Then extension = lambda / 4 Else extension = lambda / 2

For j = 0 To granules + 1
  force(j) = 0: slope(j) = 0
Next

For j = granules / 2 - 2 * lambda To granules / 2 + lambda Step lambda
  For k = 1 To lambda / 2
    force(j+k) = extension
    force(j+k+lambda / 2) = -extension
  Next
Next

For j = 1 To granules
  If unidirectional Then
       slope(j) = force(j) - force(j+1)
  Else slope(j) = force(j) - force(j+1) / 2 - force(j-1) / 2
  End If
Next
Return


'*********************************************************************
' CHOICE E - HEAT.
'*********************************************************************

Choice.E:
ratio = .3
For j = 0 To granules + 1
  x = granules / 2 - j
  Gaussian = 1.00005 ^ (-x ^ 2)
  force(j) = Rnd * 50 * Gaussian
  force(j) = force(j) - Rnd * 50 * Gaussian
  slope(j) = 0
Next
Return


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
'*********************************************************************

Frame:
margin = 10
x.left  = 8 * left. - margin - 4
x.right = 8 * left. + 8 * x.text + margin - 6
y.top = top * 16 - margin - 16
y.bottom = y.top + 16 * y.text + 2 * margin - 2

Line (x.left, y.top)-(x.right + 1, y.bottom + 1), gray, B
Line (x.left + 1, y.top + 1)-(x.right, y.bottom), black, B
Line (x.left + 1, y.bottom)-(x.right, y.bottom), white
Line (x.left, y.bottom + 1)-(x.right + 1, y.bottom + 1), white
Line (x.right, y.top + 1)-(x.right, y.bottom), white
Line (x.right+ 1, y.top)-(x.right + 1, y.bottom + 1), white
Return


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Initialization:
Windowtitle "WaveMechanics03 - Waves on a string (one dimension)."
matrix.page = 2
x.center = 512
y.center = 170
If unidirectional Then amplitude = 50 Else amplitude = 75
If speed < 1 Then speed = .9999
red  =  Rgb(255,0,0)
blue =  Rgb(0,0,255)
cyan =  Rgb(0,100,100)
gold =  Rgb(180,150,100)
gray =  Rgb(125,125,125)
buff =  Rgb(255,255,200)
green = Rgb(0,200,0)
white = Rgb(255,255,255)
dark.gray  =  Rgb(75,75,75)
green.text =  Rgb(0,125,0)
background =  Rgb(225,225,225)
light.green = Rgb(175,255,175)
Select Case choice$
  Case "A": Gosub Choice.A
  Case "B": Gosub Choice.B
  Case "C": Gosub Choice.C
  Case "D": Gosub Choice.D
  Case "E": Gosub Choice.E: speed = .1                                '"heat" works with fractionary step only.
End Select
Screenset matrix.page, matrix.page
Color black, background: Cls
Gosub Title
Gosub Text
Line(0,y.center)-(1023, y.center), gray
Screenset work.page, visible.page
Return


'*********************************************************************
' TEXT.
'*********************************************************************

Text:
line21$  = " A - Gaussian 1-D Ricker wavelet.      "
line22$  = " B - Damped sine waves.                "
line23$  = " C - Sawtooth waves.                   "
line24$  = " D - Square waves.                     "
line25$  = " E - Heat.                             "
line28$  = " G - Unidirectional impulse.           "
line29$  = " H - Bidirectional impulse.            "
line31$  = " J - Full speed. - Perfect waves.      "
line32$  = " K - Slow speed. - Quantum effect.     "
line34$  = " S - Hard reflection.                  "
line35$  = " T - Soft reflection.                  "
line36$  = " U - Soft (left) and hard (right).     "
line37$  = " V - No reflection.                    "
line38$  = " W - Endless loop.                     "

line47a$ = " I - Initialize.   "
line47b$ = " M - Menu.   "
line47c$ = " Slow.         "
line48a$ = " Previous Program. "
line48b$ = " Quit (Esc.)."
line48c$ = " Next Program. "

Locate 41, 89: Print "Green curve: slope or quadrature."
Locate 42, 89: Print "Black: force, extension (Hooke's law)."
Locate 45, 89: Print "P - Pause.  R - Reset."

Color green.text
x.text = Len(line21$) - 1                                             'text width (pixels = x * 8).
y.text = 18                                                           'number of lines (pixels = y * 16).
top = 21                                                              'upper limit.
left.= 88                                                             'limit on the left hand side: "Locate top, left".
Gosub frame
Locate 21,88: Print line21$
Locate 22,88: Print line22$
Locate 23,88: Print line23$
Locate 24,88: Print line24$
Locate 25,88: Print line25$
Locate 28,88: Print line28$
Locate 29,88: Print line29$
Locate 31,88: Print line31$
Locate 32,88: Print line32$
Locate 34,88: Print line34$
Locate 35,88: Print line35$
Locate 36,88: Print line36$
Locate 37,88: Print line37$
Locate 38,88: Print line38$
Locate 47,42: Print "I - Initialize.    M - Menu.    Slow."
Locate 48,42: Print "Previous Program.  Quit (Esc.). Next Program.";

Color gray
Locate 47, 3: Print "Thanks to the creators of FreeBASIC."
Locate 48, 3: Print "Gabriel LaFreniere  glafreniere.com";
Locate 47,89: Print "April 20, 2008. This program may be"
Locate 48,89: Print "freely distributed, copied or modified.";

Color blue
Select Case choice$
  Case "A": Locate 21, 88:  Print line21$
  Case "B": Locate 22, 88:  Print line22$
  Case "C": Locate 23, 88:  Print line23$
  Case "D": Locate 24, 88:  Print line24$
  Case "E": Locate 25, 88:  Print line25$
End Select

Select Case reflection$
  Case "hard": Locate 34, 88:  Print line34$
  Case "soft": Locate 35, 88:  Print line35$
  Case "both": Locate 36, 88:  Print line36$
  Case "none": Locate 37, 88:  Print line37$
  Case "loop": Locate 38, 88:  Print line38$
End Select

If speed = 1 Then
  Locate 31, 88: Print line31$
Else
  Locate 32, 88: Print line32$
End If
If unidirectional Then
  Locate 28, 88: Print line28$
Else
  Locate 29, 88: Print line29$
End If

Color black
Return


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Title:
title$ = "Waves on a string"
Locate 1,1: Print title$
For x1 = 0 To 8 * Len(title$)
  x2 = x.center + 2 * x1 - 8 * Len(title$)
  For y1 = 1 To 14
    If Point(x1, y1) = black Then
      y2 = 2 * y1 + 2
      Line(x2, y2)-(x2, y2 + 1), gold
      If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), gold
      If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), gold
      If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), gold
    End If
  Next
  If (x1+1) Mod 8 Then Else Line(x2+1, 0)-(x2+1, 34), background      'separate invasive characters such as capital M.
Next
Line(0, 0)-(8 * Len(title$), 14), background, bf                      'matrix title erased.
For x1 = x.center - 8 * Len(title$) To x.center + 8 * Len(title$)     'adding light and shades.
  For y1 = 0 To 34
    If Point(x1, y1) = gold And Point(x1-1, y1-1) = background Then Pset(x1-1, y1-1), buff
    If Point(x1, y1) = gold And Point(x1+1, y1+1) = background Then Pset(x1+1, y1+1), black
  Next
Next
For x = 512 - 8 * Len(title$) To 512 + 8 * Len(title$)                'adding luster.
  For y = 4 To 32
    If Point(x, y) = gold Then dark = 9 * Abs(18 - y): Pset(x, y), Rgb(240 - dark, 200 - dark, 120 - dark)
  Next
Next
Return
