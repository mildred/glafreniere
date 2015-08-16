Width 80,20:Color 0,15:Cls:?
? " Created November 3, 2008 by Gabriel LaFreniere.":?:?
? " This FreeBasic program was adapted to the 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " It should still be compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?

' Subs are in alphabetical order. Press F2 and double-click "Subs"
' to display the list. Then double-click the Sub name.

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(75,75,75)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Text()
Declare Sub Wave_Display()
Declare Sub Wave_Generator()

Dim Shared As Integer x_width = 640, y_height = 480, x_start, y_start, x_stop, y_stop
Dim Shared As Integer r, g, b, x, y, x_coord, y_coord, x_squared, x_emitter, x_center, y_center, lambda, half_lambda
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, iteration
Dim Shared As Integer frame, skipped_frames, line_number, click, x_mouse, y_mouse, wheel
Dim Shared As Single amplitude, phase, distance, radian, potential
Dim Shared As Single orthogonal, diagonal
Dim Shared As Single past(-3 To x_width+3, -3 To y_height+3)
Dim Shared As Single present(-3 To x_width+3, -3 To y_height+3)
Dim Shared As Single trend(-3 To x_width+3, -3 To y_height+3)
Dim Shared As String in_key, line47, line48

Screen 20,24,3
Initialization()


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  For frame = 0 To skipped_frames

' JOCELYN MARCOTTE'S OPTIMIZED 2-D WAVE ALGORITHM (CREATED IN 2006).  "THE PAST IS A GUIDE TO THE FUTURE"
  
    For x = x_start To x_stop: For y = y_start To y_stop
      past(x,y)  = present(x,y)                                       'updating previous states.
      present(x,y) = trend(x,y)
    Next: Next

    For x = x_start To x_stop: For y = y_start To y_stop
      orthogonal = present(x-1, y  ) + present(x,   y-1) + present(x,   y+1) + present(x+1, y  )'orthogonal influence.
      trend(x,y) = .5 * orthogonal - past(x,y)                        'trend extrapolation.
    Next: Next


'    For x = x_start To x_stop: For y = y_start To y_stop
'      orthogonal = present(x-1, y  ) + present(x,   y-1) + present(x,   y+1) + present(x+1, y  )'orthogonal influence.
'      diagonal   = present(x-1, y-1) + present(x-1, y+1) + present(x+1, y-1) + present(x+1, y+1)'diagonal influence.
'      trend(x,y) = .5 * orthogonal + .25 * diagonal - present(x,y) - past(x,y)                  'trend extrapolation.
'    Next: Next

' The more accurate diagonal correction shown above was elaborated by
' Mr. Jocelyn Marcotte himself in 2006. Surprisingly, it also yields
' faster results because the wave speed is accelerated from .707 to
' 1 pixel per loop exactly. This is possible because the sign for
' the "present" energy is negative (the positive sign leads to a
' slower wave speed). Additionally, the influence for diagonal
' transmission remains in accordance with the square of the distance
' law. This also works in 3-D using 26 neighbors and three levels
' of influence. Mr. Marcotte used this method in 2006 for reproducing
' my moving electron (he was the first one!) in full 3-D.
' The basic trend in 2-D may also be given by:

' trend(x,y) = (present(x-1,y) + present(x,y-1) + present(x,y+1) + present(x+1,y)) / 2 - past(x,y)

' This calculus is much simpler, yet the diagonal propagation is
' delayed to the next cycle. This is especially annoying for shorter
' wavelengths. It should also be emphasized that the "c" speed
' normalized to 1 is of the utmost importance for experiments
' involving motion. The Doppler effect becomes much easier to deal
' with, and this is especially true for the Lorentz transformations.
' Also, the phase can easily be related to the current iteration.

    If x_stop < x_width Then x_stop += 1
    If y_stop < y_height Then y_stop += 1
    iteration += 1
    Wave_Generator()
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If frame = 0 Then Wave_Display()                                  'skip other frames.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next
  If Len(in_key) Then Keyboard_Management()
  line_number = .5 + y_mouse / 16                                     'line number in text units.
  If line_number > 0 Then Mouse_Management()
Loop


'***********************************************************************************************************************
' END OF MAIN LOOP.
'***********************************************************************************************************************
' SUB PROCEDURES BELOW ARE IN ALPHABETICAL ORDER (Press F2 and double-click "Subs").
'***********************************************************************************************************************


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Windowtitle "Marcotte's 2-D Wave Algorithm."
  visible_page = 0
  work_page    = 1
  matrix_page  = 2
  x_center = .5 * x_width
  y_center = .5 * y_height
  lambda = 48                                                         'lambda in pixels, multiple of 4 advisable and
  half_lambda = .5 * lambda                                           '                  compatible with c = 1 pixel.
  skipped_frames = 4                                                  'select 0 for smoother but slower waves.
  x_emitter = 150
  iteration = 0
  x_start = 0
  y_start = 0
  x_stop = x_width
  y_stop = y_height

  For x = x_start To x_stop
    For y = y_start To y_stop
      past(x,y) = 0                                                   'erasing previous data.
      present(x,y) = 0
      trend(x,y) = 0
    Next
  Next

  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Color dark_gray
  Locate 47, 3: ? "Thanks to the creators of FreeBASIC."
  Locate 48, 3: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47,89: ? "November 2008. This program may be"
  Locate 48,89: ? "freely distributed, copied or modified.";
  Color green_text
  line47 = " Initialize.                        "
  line48 = " Press Esc. to quit.                "
  Locate 47, 46: ? line47
  Locate 48, 46: ? line48;
  Circle(x_emitter, y_center),.375 * lambda, green
  Circle(x_emitter, y_center),.375 * lambda + half_lambda, red
  Circle(x_emitter, y_center),.375 * lambda + lambda, green
End Sub


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

Sub Keyboard_Management()
  If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = Ucase(in_key)
  Select Case in_key
  Case Chr(27), "k+": End                                             'escape key or Windows' X quit button.
  Case "I": Initialization(): in_key = ""                             'initialization
  End Select
  Do: Loop While Len(Inkey)                                           'clear buffer.
End Sub


'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

Sub Mouse_Management()
  If x_mouse < 360 Or x_mouse > 648 Then Exit Sub
  Color green_text, white
  Locate line_number
  Select Case line_number
    Case 47:
      Locate 47, 46: ? line47
      If click > 0 Then Initialization()                              'Initialize.
    Case 48:
      Locate 48, 46: ? line48;
      If click > 0 Then End                                           'end program.
  End Select
  If x_mouse < x_width And y_mouse < y_height Then line_number = 1
End Sub


'*********************************************************************
' DISPLAYING POSITIVE AMPLITUDE IN GREEN AND NEGATIVE IN RED.
'*********************************************************************

Sub Wave_Display()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
' If line_number = 1 Then Exit Sub                                    'skip wave display when mouse cursor is on-screen.
  For x = 0 To x_width: For y = 0 To y_height
    luminance_1 = Abs(20 * present(x,y))
    b = luminance_1 / 2
    If b > 255 Then b = 255
    If luminance_1 > 255 Then
      luminance_2 = luminance_1 - 255
      If luminance_2 > 255 Then luminance_2 = 255
      luminance_1 = 255
    Else luminance_2 = 0
    End If
    If present(x,y) > 0 Then                                          'using complementary magenta and emerald green.
      r = luminance_2
      g = luminance_1
    Else
      r = luminance_1
      g = luminance_2
    End If
       Pset(x,y), Rgb(r,g,b)
  Next: Next  
End Sub


'*********************************************************************
' WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Wave_Generator()
  If iteration >= 4 * lambda Then                                     'stop pulsating.
    Return
  Elseif iteration < lambda Then
    amplitude = iteration * (20000 / lambda^2.5) / lambda             'tempering the first wave to avoid rebounds.
  Else amplitude = 20000 / lambda^2.5                                 'impulse zone is smaller for shorter wavelength. 
  End If

  phase = iteration * 2 * pi / lambda
  For x_coord = -lambda / 4 To lambda / 4
    x_squared = x_coord^2
    For y_coord = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y_coord^2)
      If distance <= .25 * lambda Then
        radian = 2 * pi * distance / lambda
        potential = Cos(radian)
        trend(x_emitter+x_coord,y_center+y_coord)=trend(x_emitter+x_coord,y_center+y_coord)+amplitude*potential*Cos(phase)'generator.
      End If
    Next
  Next
End Sub
 
