' This is a FreeBasic program. Created March 2, 2008 by Gabriel LaFreniere. Updated March 10, 2008. 
' Please download the editor/compiler from: http://fbide.freebasic.net

Const background = Rgb(225,225,225), blue = Rgb (0, 0, 255), green = Rgb(0,150,0)
Const red = Rgb(255,0,0), white = Rgb(255,255,255), gold = Rgb(200,150,0), black = 0, gray = Rgb(150,150,150)
Const buff  = Rgb(255,255,200), green_text  = Rgb(000,125,000)
Dim As Integer x, y, x1, x2, y1, y2, x_mouse, y_mouse, wheel, click
Dim As Integer page1, page2 = 1, x_center = 512, previous_line, active
Dim As String key, title = "The Wave Mechanics", w_title = title
Dim As String line08, line09, line10, line11, line12, line13, line14, line15, line16, line17, line47
Dim As Single dark

Screen 20,24,3: Screenset 2, 2: : Color black, background: Cls
Windowtitle "WaveMechanics00  -  Main Menu  -  The Wave Mechanics"

' TITLE **************************************************************

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

' TEXT ***************************************************************

Locate 5, 15: ? "Choose a program from the list below."
Locate 47, 46: Print "Press Esc. to quit.";
Locate 48, 46: Print "Alt + Enter - Full screen (1024x768).";
Locate 47: Color green
Locate , 3:  Print "Thanks to the creators of FreeBASIC."
Locate , 3:  Print "Gabriel LaFreniere  glafreniere.com";
Locate 47
Locate , 90: Print "May 12, 2009. This program may be"
Locate , 90: Print "freely distributed, copied or modified.";

Color black
line08 = " 1 - Oscillations. Philippe Delmotte's invention. Euler's method and Anselme Dewavrin's algorithms.          "
line09 = " 2 - Waves on a string using a limited number of granules.                                                   "
line10 = " 3 - Philippe Delmotte's algorithm (June 2005) using more granules.                                          "
line11 = " 4 - Jocelyn Marcotte's algorithm (January 2006).                                                            "
line12 = " 5 - Waves in two dimensions. Optical or acoustic demonstrations using reflectors. Truly amazing.            "
line13 = " 6 - The Lorentz-Voigt Transformations. - The Doppler Effect Revisited.                                      "
'line14 = " 7 - 07.                                                                                                     "
'line15 = " 8 - 08.                                                                                                     "
'line16 = " 9 - 09.                                                                                                     "
line47  = " Press Esc. to quit.                   "
Color green_text, background
Locate 08, 10: ? line08
Locate 09, 10: ? line09
Locate 10, 10: ? line10
Locate 11, 10: ? line11
Locate 12, 10: ? line12
Locate 13, 10: ? line13
Locate 14, 10: ? line14
Locate 15, 10: ? line15
Locate 16, 10: ? line16
Locate 17, 10: ? line17
Line(0,  728)-(1023,728), white
Line(0,  729)-(1023,729), gray
Line(49,  99)-(974, 399), gray, b
Line(50, 100)-(974, 399), black, b
Line(973,101)-(973, 399), white
Line(974,100)-(974, 399), white
Line(51, 399)-(974, 399), white
Line(50, 400)-(974, 400), white

Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  Getmouse x_mouse, y_mouse, wheel, click: Locate 35, 3
  previous_line = active
  active = .5 + y_mouse / 16
  If x_mouse > 706 Or x_mouse < 86 Then active = 0

  If active Then
    Color green_text, white: Locate active, 10
    Select Case active
      Case 08: ? line08: If click Then Run "WaveMechanics01.exe"
      Case 09: ? line09: If click Then Run "WaveMechanics02.exe"
      Case 10: ? line10: If click Then Run "WaveMechanics03.exe"
      Case 11: ? line11: If click Then Run "WaveMechanics04.exe"
      Case 12: ? line12: If click Then Run "WaveMechanics05.exe"
      Case 13: ? line13: If click Then Run "WaveMechanics06.exe"
'      Case 14: ? line14: If click Then Run "WaveMechanics07.exe"
'      Case 15: ? line15: If click Then Run "WaveMechanics08.exe"
'      Case 16: ? line16: If click Then Run "WaveMechanics09.exe"
'      Case 17: ? line17: If click Then Run "WaveMechanics10.exe"
      Case 47: Locate 47, 45:? line47:  If click Then End
    End Select
  End If

  key = Inkey
  If Len(key) = 2 Then key = Right(key, 1) + "+" Else key = Ucase(key)
  Select Case key
    Case "A": Run "WaveMechanics01.exe"
    Case "B": Run "WaveMechanics02.exe"
    Case "C": Run "WaveMechanics03.exe"
    Case "D": Run "WaveMechanics04.exe"
    Case "E": Run "WaveMechanics05.exe"
    Case "F": Run "WaveMechanics06.exe"
    Case "G": Run "WaveMechanics07.exe"
    Case "H": Run "WaveMechanics08.exe"
    Case "I": Run "WaveMechanics09.exe"
    Case "J": Run "WaveMechanics10.exe"
    Case Chr(27), "k+": End                                           'Windows' window's X inkey is k...
  End Select    
  Sleep 30                                                            'additional time for CPU.
Loop
