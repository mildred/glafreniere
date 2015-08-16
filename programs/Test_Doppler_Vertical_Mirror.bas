Width 80,20:Color 0,15:Cls:?
? " Created November 15, 2008 by Gabriel LaFreniere.":?:?
? " This FreeBasic program was adapted to the 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " It should still be compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?

' Gosub commands are not supported any more.
' All variables must be declared.
' Subs are in alphabetical order. Press F2 and double-click "Subs"
' to display the list. Then double-click the Sub name.

Declare Sub Damping_Management()
Declare Sub Drag_Emitters()
Declare Sub Flat_Mirrors()
Declare Sub Frame_of_Reference()
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Parabolic_Reflector()
Declare Sub Wave_Display()
Declare Sub Circular_Generator_Doppler()

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(75,75,75)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Dim Shared As Integer r, g, b, x, y, x_screen = 1024, y_screen = 768, x_width, y_height
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer x_coord, y_coord, x_squared, x_emitter, y_emitter, x_straight_emitter
Dim Shared As Integer y_parabola, y_beam_splitter, x_vertical_mirror, target
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulses, stop_pulsating, lambda, damping_zone = 401
Dim Shared As Integer frame, skipped_frames, line_number, axial, unit, parabola_radius
Dim Shared As Single amplitude, phase, previous_phase, distance, radian, wave_speed, ratio, brightness, move_frame, t_time, t_prime
Dim Shared As Single orthogonal, diagonal, influence, potential, k_Dewavrin, kinetic, Lagrangian, factor, previous
Dim Shared As Single energy(x_screen, y_screen), quadrature(x_screen, y_screen), beta, mirror_angle, g_Lorentz
Dim Shared As Single damping(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single past(   -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single present(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single trend(  -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As String line13, line14, line15, line16, line17, line18, line19, line20
Dim Shared As String line21a, line21b, line22a, line22b, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39
Dim Shared As String line40, line41, line42, line43, line44, line45, line46, line47, line48
Dim Shared As String in_key, display, file, bitmap_number
visible_page = 0: work_page = 1: matrix_page = 2: pulses = 4: axial = 0: x_width = 200: y_height = 200: bitmap = 1
stop_pulsating = 0: display = "amplitude": lambda = 40: brightness = 1: beta = .6: g_Lorentz = Sqr(1 - beta^2)
parabola_radius = 450: y_parabola = y_height - 2
y_beam_splitter = 100
x_emitter = 100
x_vertical_mirror = 198
y_emitter = 100
skipped_frames = 0                                                  'select 0 for smoother but slower waves.
Initialization()

'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  For frame = 0 To skipped_frames

' JOCELYN MARCOTTE'S 2-D WAVE ALGORITHM (CREATED IN 2006).            "THE PAST IS A GUIDE TO THE FUTURE"
  
    For x = x_start To x_stop: For y = y_start To y_stop
      past(x,y)  = present(x,y)                                       'updating previous states.
      present(x,y) = trend(x,y)
    Next: Next
    For x = x_start To x_stop: For y = y_start To y_stop
      orthogonal = present(x-1, y  ) + present(x,   y-1) + present(x,   y+1) + present(x+1, y  )'orthogonal influence.
      trend(x,y) = .5 * orthogonal - past(x,y)                        'trend extrapolation.
    Next: Next

    If iteration Mod lambda / 4 = 0 Then
      For x = 0 To x_width: For y = 0 To y_height
        energy(x,y) = quadrature(x,y)
      Next: Next      
      For x = 0 To x_width: For y = 0 To y_height
        quadrature(x,y) = trend(x,y)
      Next: Next      
      For x = 0 To x_width: For y = 0 To y_height
        energy(x,y) = Sqr(quadrature(x,y)^2 + energy(x,y)^2)
      Next: Next      
    End If

    move_frame += .707 * beta
    If move_frame > 1 Then move_frame -= 1: Frame_of_Reference()
    If frame = 0 Then Wave_Display()                                  'skip other frames.
'   Parabolic_Reflector()
    Flat_Mirrors()
    Damping_Management()                                              'processing damping zone.
    Circular_Generator_Doppler()                                      'circular impulses.
    iteration += 1
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next
  If y_mouse < 186 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If line_number = 0 And Abs(y_mouse - y_center) < 50 Then Drag_Emitters()'move emitters.
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
'  If bitmap > 0 Then                                                  'initialize bitmap = 1 for bitmap sequence.
'    Select Case bitmap
'      Case Is < 10: bitmap_number = "00"
'      Case Is < 100: bitmap_number = "0"
'      Case Is < 1000: bitmap_number = ""
'    End Select
'    file = "capture" + bitmap_number + Str(bitmap) + ".bmp"
'    Color red, background: Locate 44, 110: Print file
'    Bsave file, 0
'    bitmap += 1
'    If bitmap > 999 Then End
'  End If
Loop

'***********************************************************************************************************************
' END OF MAIN LOOP.
'***********************************************************************************************************************
' SUB PROCEDURES BELOW ARE IN ALPHABETICAL ORDER (Press F2 and double-click "Subs").
'***********************************************************************************************************************


'*********************************************************************
' DAMPING ZONE AND OTHER REFLECTION OR ANTI-REFLECTION DEVICES.
'*********************************************************************

Sub Damping_Management()

  For x = x_start To 0                                                'progressive damping, left side.
    For y = y_start To y_stop
      trend(x,y)   = damping(x,y) * trend(x,y)
      present(x,y) = damping(x,y) * present(x,y)
    Next
  Next
  For x = x_width To x_stop                                           'right side.
    For y = y_start To y_stop
      trend(x,y)   = damping(x,y) * trend(x,y)
      present(x,y) = damping(x,y) * present(x,y)
    Next
  Next
  For x = 0 To x_width
    For y = y_start To 0                                              'upper side.
      trend(x,y)   = damping(x,y) * trend(x,y)
      present(x,y) = damping(x,y) * present(x,y)
    Next
    For y = y_height To y_stop                                        'lower side.
      trend(x,y)   = damping(x,y) * trend(x,y)
      present(x,y) = damping(x,y) * present(x,y)
    Next
  Next

End Sub


'*********************************************************************
' DRAG EMITTERS.
'*********************************************************************

Sub Drag_Emitters()
End Sub


'*********************************************************************
' FLAT MIRRORS INCLUDING BEAM SPLITTER.
'*********************************************************************

Sub Flat_Mirrors()

' This mirror needs depixelation because of its irregular displacement.
' The move_frame variable indicates the correction value.

  For y = y_beam_splitter - y_center To y_beam_splitter + y_center    'vertical mirror.
    trend(x_vertical_mirror-1,y)   = Sqr(move_frame) * trend(x_vertical_mirror-1,y)
    trend(x_vertical_mirror,y) = 0
    Pset( x_vertical_mirror,  y), white                               'drawing the vertical mirror.
'   alternative:
'   trend(x_vertical_mirror-1, y) = .667 * trend(x_vertical_mirror-1,y) + .333 * trend(x_vertical_mirror,y)
'   trend(x_vertical_mirror,   y) = .5   * trend(x_vertical_mirror-1, y)
'   trend(x_vertical_mirror+1, y) =  0
  Next

End Sub


'*********************************************************************
' MOVING FRAME OF REFERENCE.
'*********************************************************************

Sub Frame_of_Reference()
  For x = x_start To x_stop                                           'moving all medium "granules" 1 pixel leftward.
    For y = y_start To y_stop
      present(x,y) = present(x+1,y)
      trend(x,y) = trend(x+1,y)
    Next
  Next
End Sub


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Screen 20,24,3
  Windowtitle " Test for vertical mirror - Doppler effect."
  g_Lorentz = Sqr(1 - beta^2)
  mirror_angle = Atn(g_Lorentz / 1)
  x_center = .5 * x_width
  y_center = .5 * y_height
  damping_zone = 30 * Sqr(lambda)                                     '100 for lambda = 25 to 200 for lambda = 100.
  iteration = 0
  x_start = -damping_zone
  y_start = -damping_zone
  x_stop = x_width  + damping_zone
  y_stop = y_height + damping_zone
  If stop_pulsating = 2 Then stop_pulsating = 0                       'restart.

  For x = x_start To x_stop                                           'erasing previous data.
    For y = y_start To y_stop
      past(x,y) = 0
      present(x,y) = 0
      trend(x,y) = 0
    Next
  Next

'*********************************************************************
' Progressive damping ratio precalculus.

  For x = x_start To 0                                                'left side damping ratio.
    ratio = ((damping_zone + x) / damping_zone) ^ (1/lambda)
    For y = y_start To y_stop
      damping(x,y) = ratio
    Next
  Next
  For x = x_width To x_stop                                           'right side.
    ratio = ((x_stop - x) / damping_zone) ^ (1/lambda)
    For y = y_start To y_stop
      damping(x,y) = ratio
    Next
  Next
  For x = x_start To x_stop                                           'upper side.
    For y = y_start To 0
      ratio = ((damping_zone + y) / damping_zone) ^ (1/lambda)
      If x < 0 Then                                                   'upper left corner.
        If x > y Then
          damping(x,y) = ratio        
        End If
      Elseif x > x_width Then                                         'upper right corner.
        If x_stop - x > y - y_start Then
          damping(x,y) = ratio                
        End If
      Else                                                            'remaining central upper side.
        damping(x,y) = ratio
      End If
    Next
  Next
  For x = x_start To x_stop                                           'lower side.
    For y = y_height To y_stop
      ratio = ((y_stop - y) / damping_zone) ^ (1/lambda)
      If x < 0 Then                                                   'lower left corner.
        If x + damping_zone > y_stop - y Then
          damping(x,y) = ratio        
        End If
      Elseif x > x_width Then                                         'lower right corner.
        If x_stop - x > y_stop - y Then
          damping(x,y) = ratio                
        End If
      Else                                                            'remaining central lower side.
        damping(x,y) = ratio
      End If
    Next
  Next

'*********************************************************************
' Display.

  Screenset matrix_page, matrix_page
  Color black, background: Cls: Locate 36
  Locate, 47: ? "Place the mouse cursor here or"
  Locate, 47: ? "press [ M ] to display the menu.":?
  Locate, 47: ? "Click and drag to move emitters.":?
  Locate, 47: ? "Right-click to resize the wave area.":?
  Locate, 47: ? "Select pulse duration here ";
  If stop_pulsating = 3 Then ? "(currently constant)." Else ? pulses
  Locate, 47: ? "Select wavelength here    "; lambda:?
  Color dark_gray
  Locate 47, 3: ? "Thanks to the creators of FreeBASIC."
  Locate 48, 3: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47,89: ? "February 11, 2009. This program may be"
  Locate 48,89: ? "freely distributed, copied or modified.";
  Color green_text
  line13 = "    Please Click Slowly!              "
  line14 = "                                      "
  line15 = " A- Show wave amplitude.              "
  line16 = " B- Show wave energy.                 "
  line17 = " C- Show standing waves.              "
  line18 = " D-                                   "
  line19 = " E- Reverse wave direction.           "
  line20 = " F- Force Standing Waves.             "
  line21a =" G- Skip 7 Frames (Faster).           "
  line21b =" G- Show All Frames (Slower).         "
  line22a =" H- Show Axial Graphics.              "
  line22b =" H- Hide Axial Graphics.              "
  line23 = " I- Initialize.                       "
  line24 = " J-                                   "
  line25 = " K-                                   "
  line26 = " L-                                   "
  line27 = " M- Press M to display this menu.     "
  line28 = " N-                                   "
  line29 = " O-                                   "
  line30 = " P- Pause.                            "
  line31 = " Q-                                   "
  line32 = " R- Restart.                          "
  line33 = " S- Stop pulsating.                   "
  line34 = " T- Constant Pulse.                   "
  line35 = " U-                                   "
  line36 = " V-                                   "
  line37 = " W-                                   "
  line38 = " X-                                   "
  line39 = " Y-                                   "
  line40 = " Z-                                   "
  line41 = "                                      "
  line42 = "                                      "
  line43 = " Pulses                               "
  line44 = " Lambda (Wavelength)                  "
  line45 = " Brightness Press [ + - = ]           "
  line46 = "                                      "
  line47 = " I- Initialize.                       "
  line48 = " Press Esc. to Quit.                  "
  Locate 47, 46: ? line47
  Locate 48, 46: ? line48;
  Locate 47, 72: ? "P- Pause."
  Locate 48, 72: ? "R- Restart.";
'  Line(0, 0)-(x_screen, y_height), background, bf
'  Line(0, 0)-(x_width, y_height), black, b
'  Line(0, y_center - 40)-(x_screen, y_center + 40), white, bf
'  Line(0, y_center - 40)-(x_screen, y_center + 40), black, b
'  Line(0, y_center)-(x_screen, y_center), gray
'  Circle(x_emitter, y_center), .5 * lambda, black
'  Line(x_straight_emitter - 1, y_center - 100)-(x_straight_emitter + 1, y_center + 100), black, b
'  For x = x_straight_emitter To x_screen Step lambda
'    Line(x, y_center - 40)-(x, y_center + 40), green
'  Next
'  For x = x_straight_emitter To 0 Step -lambda
'    Line(x, y_center - 40)-(x, y_center + 40), green
'  Next

' There is a lambda / 8 phase offset all around the 2-D circular emitter (lambda / 4 in 3-D).
'  Line(x_emitter +  .125 * lambda, y_center - 20)-(x_emitter +  .125 * lambda, y_center + 20), black
'  Line(x_emitter -  .125 * lambda, y_center - 20)-(x_emitter -  .125 * lambda, y_center + 20), black
'  Line(x_emitter + 1.125 * lambda, y_center - 20)-(x_emitter + 1.125 * lambda, y_center + 20), black
'  Line(x_emitter - 1.125 * lambda, y_center - 20)-(x_emitter - 1.125 * lambda, y_center + 20), black
End Sub


'*********************************************************************
' KEYBOARD MANAGEMENT.
'                        **** IMPORTANT ****
' Most of keybord commands are redirected to Mouse_Management Sub in
' order to simplify procedures and avoid occasional discrepancies.
'*********************************************************************

Sub Keyboard_Management()
  If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = Ucase(in_key)
' Screenset work_page, work_page
' cls: locate 10, 10: print in_key: sleep 1000                        'check Inkey value such as arrows, page up, etc.
  Select Case in_key
  Case Chr(27), "k+": End                                             'end program - escape key or Windows' X button.
  Case "A": line_number = 15: click = 1                               'show amplitude - execute via mouse management.
  Case "B": line_number = 16: click = 1                               'show energy.
  Case "C": line_number = 17: click = 1                               'show standing waves.
  Case "D": line_number = 18: click = 1                               'add instruction with or without initialization.
  Case "E": line_number = 19: click = 1                               'reverse wave direction.
  Case "F": line_number = 20: click = 1                               'force standing waves.
  Case "G": line_number = 21: click = 1                               'skip frames.
  Case "H": line_number = 22: click = 1                               'axial graphics.
  Case "I": line_number = 23: click = 1                               'initialization.
  Case "J": line_number = 24: click = 1                               '
  Case "K": line_number = 25: click = 1                               '
  Case "L": line_number = 26: click = 1                               '
  Case "M": line_number = 27: click = 1                               '
  Case "N": line_number = 28: click = 1                               '
  Case "O": line_number = 29: click = 1                               '
  Case "P": line_number = 30: click = 1                               'pause.
            Screenset work_page, work_page: Color red
            Locate 47, 46: ? " Paused. Press any key to resume.      "
            Sleep: in_key = ""
  Case "Q": line_number = 31: click = 1                               '
  Case "R": line_number = 32: click = 1                               'reset without initialization.
  Case "S": line_number = 33: click = 1                               '
  Case "T": line_number = 34: click = 1                               '
  Case "U": line_number = 35: click = 1                               '
  Case "V": line_number = 36: click = 1                               '
  Case "W": line_number = 37: click = 1                               '
  Case "X": line_number = 38: click = 1                               '
  Case "Y": line_number = 39: click = 1                               '
  Case "Z": line_number = 40: click = 1                               'constant pulse - restart or stop pulsating.

  Case "+": brightness = brightness / Sqr(.5)                         'brighter.
            If brightness > 4   Then brightness = 4
  Case "-": brightness = brightness * Sqr(.5)                         'darker.
            If brightness < .25 Then brightness = .25
  Case "=": brightness = 1                                            'normal brightness.
  End Select
  in_key = ""
  Do: Loop While Len(Inkey)                                           'clear buffer.
End Sub


'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

Sub Mouse_Management()
  Screenset work_page
  Color green_text, background
  If click = 2 Then                                                   'resize wave area.
    Color, background
    Do
      Swap work_page, visible_page
      Screenset work_page, visible_page: Cls
      Line(0,0)-(x_mouse, y_mouse), black, b
      Line(0,0)-(x_width, y_height), green, b
      Locate 2,2: ? "Width     "; x_mouse
      Locate 3,2: ? "Currently "; x_width
      Locate 5,2: ? "Height    "; y_mouse
      Locate 6,2: ? "Currently "; y_height
      Getmouse x_mouse, y_mouse, wheel, click
      If x_mouse < 400 Then x_mouse = 400 Else If x_mouse > x_screen - 50 Then x_mouse = x_screen
      If y_mouse < 300 Then y_mouse = 300 Else If y_mouse > y_screen - 50 Then y_mouse = y_screen
    Loop While click = 2
    x_width = x_mouse: y_height = y_mouse
    Initialization()
    Exit Sub
  End If
  Locate 13, 46: ? line13
  Locate 14, 46: ? line14
  Locate 15, 46: If display = "amplitude"      Then Color blue
                 ? line15: Color green_text
  Locate 16, 46: If display = "energy"         Then Color blue
                 ? line16: Color green_text
  Locate 17, 46: If display = "standing waves" Then Color blue
                 ? line17: Color green_text
  Locate 18, 46: ? line18
  Locate 19, 46: ? line19
  Locate 20, 46: ? line20
  Locate 21, 46: If skipped_frames = 0 Then ? line21a Else ? line21b
  Locate 22, 46: If axial = 0 Then ? line22a Else ? line22b
  Locate 23, 46: ? line23
  Locate 24, 46: ? line24
  Locate 25, 46: ? line25
  Locate 26, 46: ? line26
  Locate 27, 46: ? line27
  Locate 28, 46: ? line28
  Locate 29, 46: ? line29
  Locate 30, 46: ? line30
  Locate 31, 46: ? line31
  Locate 32, 46: ? line32
  Locate 33, 46: ? line33
  Locate 34, 46: If stop_pulsating = 3 Then  Color blue
                 ? line34: Color green_text
  Locate 35, 46: ? line35
  Locate 36, 46: ? line36
  Locate 37, 46: ? line37
  Locate 38, 46: ? line38
  Locate 39, 46: ? line39
  Locate 40, 46: ? line40
  Locate 41, 46: ? line41
  Locate 42, 46: ? line42
  Locate 43, 60: If stop_pulsating = 3 Then
                   Locate 43, 46:  ? line43
                 Else
                   Locate 43, 46:  ? line43
                   Locate 43, 53: Print pulses; " ="; pulses * lambda; " pixels."
                 End If
  Locate 44, 46: ? line44;: Locate, 66: Print lambda; " pixels."
  Locate 45, 46: ? line45
  Locate 46, 46: ? line46
  Locate 47, 46: ? line47
  Locate 48, 46: ? line48;
  Line(512 - 153, 186)-(512 + 152, 191),  background, bf
  Line(512 - 153, 186)-(512 + 152, 769), black, b
  Color green_text, white
  Locate line_number, 46

'*********************************************************************
' BEGIN MOUSE CLICK PROCESSING.

  Select Case line_number
    Case 14: ? line14
      If click > 0 Then
      End If
    Case 15: If Not display = "amplitude" Then ? line15
      If click > 0 Then display = "amplitude"                         'displaying amplitude in green and red.
    Case 16: If Not display = "energy" Then ? line16
      If click > 0 Then display = "energy"                            'energy in gray shades.
    Case 17: If Not display = "standing waves" Then ? line17
      If click > 0 Then display = "standing waves"                    'standing waves in blue and yellow tones.
    Case 18: ? line18
      If click > 0 Then
      End If
    Case 19: ? line19                                                 'reverse wave direction.
      If click > 0 Then
        For x = x_start + 1 To x_stop - 1: For y = y_start + 1 To y_stop - 1
          Swap present(x,y), trend(x,y)
        Next: Next
      End If
    Case 20: ? line20                                                 'force standing waves.
      If click > 0 Then
        iteration = pulses * lambda                           'stop pulsating.
        For x = x_start To x_stop: For y = y_start To y_stop          'trend(x,y) = present(x,y) works, but hardly.
           trend(x,y) = .5 * (past(x,y) + present(x,y))
           present(x,y) = trend(x,y)
        Next: Next
      End If
    Case 21                                                           'skipped frames.
      If skipped_frames = 0 Then ? line21a Else ? line21b
      If click > 0 Then
        If skipped_frames = 0 Then skipped_frames = 4 Else skipped_frames = 0
      End If
    Case 22: If axial = 0 Then ? line22a Else ? line22b               'show axial graphics.
      If click > 0 Then
        If axial = 0 Then axial = 1 Else axial = 0
      End If
    Case 23, 47                                                       'initialization (line 23 or 47).
      ? line23
      If click > 0 Then
        x_width = 480: y_height = 300: stop_pulsating = 0: display = "amplitude": lambda = 48: brightness = 1
        pulses = 8: axial = 1: x_emitter = x_width / 2: x_straight_emitter = x_emitter - 1.875 * lambda
        Initialization()
      End If
    Case 24: ? line24                                                 '
      If click > 0 Then
      End If
    Case 25: ? line25                                                 '
      If click > 0 Then
      End If
    Case 26: ? line26                                                 '
      If click > 0 Then
      End If
    Case 27: ? line27                                                 '
      If click > 0 Then
      End If
    Case 28: ? line28                                                 '
      If click > 0 Then
      End If
    Case 29: ? line29                                                 '
      If click > 0 Then
      End If
    Case 30: ? line30                                                 '
      If click > 0 Then
      End If
    Case 31: ? line31                                                 '
      If click > 0 Then
      End If
    Case 32                                                           'reset.
      ? line32
      If click > 0 Then
        iteration = 0
        If  stop_pulsating = 2 Then stop_pulsating = 0
        For x = x_start To x_stop
          For y = y_start To y_stop
            past(x,y) = 0                                             'erasing previous data.
            present(x,y) = 0
            trend(x,y) = 0
          Next
        Next
      End If
    Case 33: ? line33                                                 'stop pulsating.
      If click > 0 Then
        stop_pulsating = 1
        Screenset matrix_page: Color black, background
        Locate 43, 47: ? "Select pulse duration here.                     "
      End If
    Case 34                                                           'constant pulse.
      If stop_pulsating < 3 Then
        ? line34
        If click > 0 Then
          stop_pulsating = 3
          Screenset matrix_page: Color black, background
          Locate 43, 47: ? "Select pulse duration here (currently constant).";
          If iteration > pulses * lambda Then iteration = 0
        End If
      End If
    Case 35: ? line35                                                 '
      If click > 0 Then
      End If
    Case 36: ? line36                                                 '
      If click > 0 Then
      End If
    Case 37: ? line37                                                 '
      If click > 0 Then
      End If
    Case 38: ? line38                                                 '
      If click > 0 Then
      End If
    Case 39: ? line39                                                 '
      If click > 0 Then
      End If
    Case 40: ? line40                                                 '
      If click > 0 Then
      End If
    Case 41: ? line41                                                 '
      If click > 0 Then
      End If
    Case 42: ? line42                                                 '
      If click > 0 Then
      End If
    Case 43                                                           'select pulse duration.
      Locate,46: ? " Click to Select Pulse Duration       ";
      Locate,78: ? pulses
        If click > 0 Then
          Screenset visible_page
          Line(512 - 154, 639)-(512 + 153, 720), white, bf
          Line(512 - 154, 639)-(512 + 153, 720), black, b
          Locate 43, 46: ? "  2                               122"
          Locate 44, 46: ? " Currently"; pulses; " ="; pulses * lambda; " pixels."
          Line(512 - 124, 672)-(512 + 116, 688), buff, bf
          Line(512 - 124, 672)-(512 + 116, 688), black, b
          Do
            Getmouse x_mouse, y_mouse, wheel, click
            If x_mouse > 512 - 122 And x_mouse < 512 + 114 Then
              If x_coord <> x_mouse Then Line(x_coord, 673)-(x_coord, 687), buff
              Line(x_mouse, 673)-(x_mouse, 687), black
              Locate 42, 46
              ? " Selecting Pulses"; 2 * Int(x_mouse / 2) - 388; " ="; (2 * Int(x_mouse / 2) - 388) * lambda; " pixels.  "
              x = x_coord
              x_coord = x_mouse
            End If
          Loop While click
          pulses = 2 * Int(x / 2) - 388
          iteration = 0: stop_pulsating = 0
          Initialization()
        End If
    Case 44                                                           'select wavelength.
      Locate,46: ? " Click to Select Wavelength           ";
      Locate,78: ? lambda
        If click > 0 Then
          Screenset visible_page
          Line(512 - 154, 655)-(512 + 153, 736), white, bf
          Line(512 - 154, 655)-(512 + 153, 736), black, b
          Locate 44, 46: ? " 20                               100 "
          Locate 45, 46: ? " Currently"; lambda; " pixels."
          Line(512 - 124, 688)-(512 + 116, 704), buff, bf
          Line(512 - 124, 688)-(512 + 116, 704), black, b
          Do
            Getmouse x_mouse, y_mouse, wheel, click
            If x_mouse > 512 - 122 And x_mouse < 512 + 114 Then
              If x_coord <> x_mouse Then Line(x_coord, 689)-(x_coord, 703), buff
              Line(x_mouse, 689)-(x_mouse, 703), black
              Locate 43, 46: ? " Selecting Wavelength"; 4 * Int(x_mouse / 12) - 108; " pixels.  "
              x = x_coord
              x_coord = x_mouse
            End If
          Loop While click
          lambda = 4 * Int(x / 12) - 108
          iteration = 0: stop_pulsating = 0
          Initialization()
        End If
    Case 45: ? line45
      If click > 0 Then
      End If
    Case 46: ? line46                                                 '
      If click > 0 Then
      End If
    Case 47                                                           'initialization - see line 23.
    Case 48                                                           'ending program.
      ? line48;
      If click > 0 Then End
  End Select
  
  If click = 1 Then                                                   'avoid repetitive actions.
    Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
  End If
End Sub


'*********************************************************************
' HARD REFLECTION ON PARABOLA - DRAWING THE REFLECTOR.
'*********************************************************************

Sub Parabolic_Reflector()
'    For x = -4 * g_Lorentz * lambda To 4 * g_Lorentz * lambda
'    y = (x / g_Lorentz) ^ 2 / 2 / parabola_radius                     'classic parabola sagitta to lower side.
'    trend(x + x_emitter, y_parabola - y) = 0
'  Next
'    
'  If frame = 0 Then                                                   'drawing the reflector.
'    For x = -4 * lambda To 4 * lambda
'      y = x ^ 2 / 2 / parabola_radius                                 'true parabola.
'      Pset(x + x_emitter,   y_parabola - y), green
'    Next
'    For x = -4 * g_Lorentz * lambda To 4 * g_Lorentz * lambda
'      y = (x / g_Lorentz) ^ 2 / 2 / parabola_radius                   'squashed parabola.
'      Pset(x + x_emitter,   y_parabola - y), white
'    Next
'  End If
End Sub


'*********************************************************************
' DISPLAYING POSITIVE AMPLITUDE IN GREEN AND NEGATIVE IN RED.
'*********************************************************************

Sub Wave_Display()
  If line_number = 1 Then Return                                      'show background (hide waves).
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  
  If display = "amplitude" Then                                       'show wave amplitude in red and green colors.
    For x = 0 To x_width: For y = 0 To y_height
      luminance_1 = brightness * Abs(20 * trend(x,y))
      b = luminance_1 / 2
      If b > 255 Then b = 255
      If luminance_1 > 255 Then
        luminance_2 = luminance_1 - 255
        If luminance_2 > 255 Then luminance_2 = 255
        luminance_1 = 255
      Else luminance_2 = 0
      End If
      If present(x,y) > 0 Then                                        'using complementary magenta and emerald green.
        r = luminance_2
        g = luminance_1
      Else
        r = luminance_1
        g = luminance_2
      End If
         Pset(x,y), Rgb(r,g,b)
    Next: Next

  Elseif display = "energy" Then                                      'show energy in gray shades.

    For x = 0 To x_width: For y = 0 To y_height
      luminance_1 = brightness * 10 * energy(x,y)
      If luminance_1 > 255 Then luminance_1 = 255
      Pset(x,y), Rgb(luminance_1,luminance_1,luminance_1)
    Next: Next
    
  Elseif display = "standing waves" Then                              'show standing waves.

' Potential and kinetic energy as established below are hypothetic,
' as they should mechanically appear in a device such as a pendulum.
' The goal here is to make standing waves becoming visible by means
' of an also hypothetic Lagrangian. This method was elaborated by
' Mr. Jocelyn Marcotte and demonstrated in the previous program
' WaveMechanics04. In my picture, potential energy follows kinetic
' energy with a pi/2 offset. It is fully stored into a hypothetic
' field of force when kinetic energy is nil (when the pendulum stops).
' So it is equivalent to quadrature and the sum potential + kinetic
' should be constant in accordance with Pythagoras's theorem.

' However, the code below is rather consistent with Mr. Marcotte's
' point of view and it nevertheless yields amazingly nice results.
' Such an achievement deserves the warmest congratulations.

' Please note that standing waves appear twice per period with a zero
' amplitude phase in-between. Peaks occur twice per wavelength because 
' they may be positive or negative. Thus the wavelength may appear
' two times shorter. It is not an error, it is just a choice allowing
' the yellow color to indicate antinodes. As a consequence, the color
' alongside reflectors is inverted for soft and hard reflection.

    For x = 0 To x_width: For y = 0 To y_height
      kinetic = (trend(x,y) - past(x,y))^2
      potential = (present(x+1,y)-present(x-1,y))^2+(present(x,y+1)-present(x,y-1))^2
      Lagrangian = kinetic  - potential                               'classic Lagrangian.
      luminance_1 = brightness * .01 * lambda^2 * Lagrangian  'lambda^2 because of the wave generator circle.
      
      If luminance_1 > 0 Then
        r = luminance_1 / 2: g = luminance_1 / 2                      'r / 2 + g / 2 = 1 yellow, yet yellow is
        b = 0                                                         'biologically much brighter than blue.
      Else
        luminance_1 = -luminance_1
        b = luminance_1
        r = luminance_1 / 3                                           'adding fractional r and g to brighten blue.
        g = luminance_1 / 3
      End If
      If r > 255 Then r = 255
      If g > 255 Then g = 255
      If b > 255 Then b = 255
      Pset(x, y), Rgb(r,g,b)
    Next: Next
  End If

  If axial = 1 Then                                                   'axial graphics.
    Line(0, y_beam_splitter)-(3 * (lambda / g_Lorentz) * (1 + beta) - 1, y_beam_splitter), gray
    previous = y_beam_splitter
    For x = 0 To 3 * (lambda / g_Lorentz) * (1 + beta) - 1
      Line(x-1, previous)-(x, y_beam_splitter - brightness * present(x, y_beam_splitter)), white
      previous = y_beam_splitter - brightness * present(x, y_beam_splitter)
    Next
    For distance = 0 To 3 * (lambda / g_Lorentz) * (1 + beta) Step .5 * (lambda / g_Lorentz) * (1 + beta)'backward Doppler.
      Line(distance, y_beam_splitter - 10)-(distance, y_beam_splitter + 10), white
    Next
'    For x = 0 To x_stop                                               'axial amplitude graphics.
'      Pset(x,   y_beam_splitter - 2 * brightness * present(x, y_beam_splitter)), black
'      Pset(x+1, y_beam_splitter - 2 * brightness * present(x, y_beam_splitter)), white
'    Next
'    Line(0, y_beam_splitter)-(x_width, y_beam_splitter), gray
  End If

'  Line(x_emitter - 4 * lambda, 0)-(x_emitter - 4 * lambda, y_beam_splitter + 4 * lambda + 100), green
'  Line(x_emitter + 4 * lambda, 0)-(x_emitter + 4 * lambda, y_beam_splitter + 4 * lambda + 100), green
'  Line(x_emitter - 4 * lambda, 0)-(x_emitter + 4 * lambda, 0), green
'  Line(x_emitter - 4 * lambda - 100, y_beam_splitter - 4 * lambda)-(x_emitter + y_beam_splitter, y_beam_splitter - 4 * lambda), green
'  Line(x_emitter - 4 * lambda - 100, y_beam_splitter + 4 * lambda)-(x_emitter + y_beam_splitter, y_beam_splitter + 4 * lambda), green
'  Line(x_emitter + y_beam_splitter, y_beam_splitter -  4 * lambda)-(x_emitter + y_beam_splitter, y_beam_splitter + 4 * lambda), green
'  Line(x_emitter, 0)-(x_emitter, 2 * lambda), white
'  Line(x_emitter, y_parabola - 2 * lambda)-(x_emitter, y_parabola), white
'  For y = .5 * lambda To 2 * lambda Step lambda / 2
'    Line(x_emitter - 10, y)-(x_emitter + 10, y), white
'    Line(x_emitter - 10, y_parabola - y)-(x_emitter + 10, y_parabola - y), white
'  Next
  Circle(x_emitter, y_emitter), lambda / 4, gray
  Color background, black
'  Locate 48, 102: Print Using "###"; Int(iteration / (skipped_frames + 1));: ? "    "; iteration;
'  Locate 43, 1: ? " The Michelson Interferometer."
'  Locate 45, 1: ? " Beta (v/c):"; beta; "       "
'  Locate 46, 1: ? " Lambda:"; lambda; " pixels.    "
'  Locate 47, 1: ? " Contraction:";
'  ? Using "###.###   "; g_Lorentz;
'  Locate 48, 1: ? " Mirror angle:";
'  ? Using " ##.##"; mirror_angle * 180 / pi;: ? Chr(248); "  ";
'  Locate 48, 29: ? " glafreniere.com ";
'  If Int(iteration / (skipped_frames + 1)) = 330 Then Sleep
End Sub


'*********************************************************************
' CIRCULAR WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Circular_Generator_Doppler()
If stop_pulsating = 2 Then Exit Sub                                 'no pulsation.
  
  t_time = .5 * iteration * 2 * pi / lambda                           'Lorentz's c = 1; wave speed is .5 pixel per cycle.
  amplitude = 1500 / lambda ^ 2
  
  If stop_pulsating = 0 Then
    If iteration > 2 * (pulses - 2) * lambda / g_Lorentz Then         'wave speed is .5 pixel per cycle.
      stop_pulsating = 1                                              'prepare for stopping during last pulse.
      t_prime = g_Lorentz * t_time                                    'Lorentz's inverted: t' = g * t  (slower pulses).
      previous_phase =  Cos(t_prime)
      target = iteration + 2 * lambda
    End If
  Elseif stop_pulsating = 1 Then                                      'stop pulsating when amplitude reaches a minimum.
    t_prime = g_Lorentz * t_time
'    If previous_phase < 0 and Cos(t_prime) > 0 Then stop_pulsating = 2: Exit Sub
'    If previous_phase > 0 and Cos(t_prime) < 0 Then stop_pulsating = 2: Exit Sub
    previous_phase =  Cos(t_prime)
  End If

  If iteration < 4 * lambda Then                                      'start pulsating gradually.
    amplitude = amplitude * iteration / (4 * lambda)                  'useful for beam splitter, may be omitted.
  End If
  If stop_pulsating = 1 Then                                          'stop pulsating gradually.
    amplitude = amplitude * (target - iteration) / (2 * lambda)       'useful for beam splitter, may be omitted.
    If amplitude < 0 Then stop_pulsating = 2: Exit Sub
  End If


  For x = -(g_Lorentz * lambda) / 4 To (g_Lorentz * lambda) / 4
    x_squared = ((x + move_frame) / g_Lorentz) ^ 2
    t_prime = g_Lorentz * t_time - beta * ((x + move_frame) / lambda) 'Lorentz's inverted: t' = g * t - beta * x
    potential = amplitude * Cos(t_prime)
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
      If distance < lambda / 4 Then
        radian = 2 * pi * distance / lambda
        trend(x_emitter+x,y_emitter+y)=trend(x_emitter+x,y_emitter+y) + potential * Cos(radian)
      End If
    Next
  Next
End Sub
