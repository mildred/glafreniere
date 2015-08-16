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
Declare Sub Frame_of_Reference()
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Wave_Display()
Declare Sub Circular_Generator_Doppler()

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(75,75,75)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Dim Shared As Integer x_screen = 1024, y_screen = 768, x_width = 400, y_height = 400
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer r, g, b, x, y, x_scanner, x_previous, x_coord, y_coord, x_squared
Dim Shared As Integer x1_emitter, x2_emitter, y_emitter, x_straight_emitter
Dim Shared As Integer y_parabola, y_beam_splitter, x_vertical_mirror, x_scale
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulses, stop_pulsating, lambda, target, damping_zone = 401, parabola_radius
Dim Shared As Integer frame, skipped_frames, line_number, axial, unit, scan(-2 To x_width, y_height)
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Single t_time, t_prime, k_Dewavrin, ratio, move_frame, preferred_frame
Dim Shared As Single orthogonal, diagonal, influence, potential, kinetic, Lagrangian, factor, previous
Dim Shared As Single amplitude, phase, previous_phase, distance, radian, wave_speed, brightness, decimal
Dim Shared As Single energy(x_screen, y_screen), quadrature(x_screen, y_screen), beta, mirror_angle, g_Lorentz, c_speed
Dim Shared As Single damping(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single past(   -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single present(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single trend(  -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As String line13, line14, line15, line16, line17, line18, line19, line20
Dim Shared As String line21a, line21b, line22a, line22b, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39
Dim Shared As String line40, line41, line42, line43, line44, line45, line46, line47, line48
Dim Shared As String in_key, display, file, bitmap_number
visible_page = 0: work_page = 1: matrix_page = 2: lambda = 32
brightness = 1
beta = .5: g_Lorentz = Sqr(1 - beta^2)
pulses = 20
skipped_frames = 0                                                    'select 0 for smoother but slower display.
x_scale = -263
bitmap = 0
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
      orthogonal = present(x-1, y) + present(x, y-1) + present(x, y+1) + present(x+1, y)'orthogonal influence.
'     trend(x,y) = .5 * orthogonal - past(x,y)                                          'fastest trend extrapolation (constant pixel sum = 1).
'     trend(x,y) = .4 * (orthogonal + present(x,y)) - past(x,y)                         'slower wave speed: .4 * (4 + 1) - 1 = 1
      trend(x,y) = .25 * (2 - wave_speed) * orthogonal + wave_speed * present(x,y) - past(x,y)'slower wave speed.
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

    x_scanner -= 1
    If x_scanner < -2 Then Sleep: End
    move_frame += c_speed * preferred_frame
    If move_frame > 1 Then move_frame -= 1: Frame_of_Reference()
'   Parabolic_Reflector()
'   Flat_Mirrors()
    Damping_Management()                                              'processing damping zone.
    Circular_Generator_Doppler()                                      'circular impulses.
    iteration += 1
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next

  Wave_Display()                                                      'skip other frames.
  If y_mouse < 400 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
' If line_number = 0 And Abs(y_mouse - y_center) < 50 Then Drag_Emitters()'move emitters.
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And x_scanner < x_width + 100 Then                    'initialize bitmap = 1 for bitmap sequence.
    Select Case bitmap
      Case Is < 10: bitmap_number = "00"
      Case Is < 100: bitmap_number = "0"
      Case Is < 1000: bitmap_number = ""
    End Select
    file = "capture" + bitmap_number + Str(bitmap) + ".bmp"
    Color red, background: Locate 44, 110: Print file
    If bitmap Mod 5 Then Else Bsave file, 0
    bitmap += 1
    If bitmap > 999 Then End
  End If
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
' MOVING FRAME OF REFERENCE.
'*********************************************************************

Sub Frame_of_Reference()
  x1_emitter += 1
  x2_emitter += 1
  For x = x_stop To  x_start Step -1                                  'moving all medium "granules" 1 pixel rightward.
    For y = y_start To y_stop
      present(x,y) = present(x-1,y)
      trend(x,y) = trend(x-1,y)
    Next
  Next
End Sub


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Screen 20,24,3
  Windowtitle " The Time Scanner - Nov. 16, 2009"
  Screenset matrix_page, matrix_page
  Color black, background: Cls: Locate 35
  wave_speed = 1.04                                                   'set wave_speed = 0 for fastest c_speed (min. speed is 2).
' c_speed = Sin(pi / 4)                                               'fastest c_speed is .707 pixel per loop.
  c_speed = ((2 - wave_speed) / 2) ^ (1 / 4) * Sin(pi / 4)            '0.59 pixel per loop in order to match the scan speed.
' Locate 10, 10: Print c_speed: Sleep: End
  g_Lorentz = Sqr(1 - beta ^ 2)
  preferred_frame = (1 - g_Lorentz) / beta
  x1_emitter = .6 * x_width
  x2_emitter = .667 * x_width
  y_emitter = .5 * y_height
  mirror_angle = Atn(g_Lorentz / 1)
  x_center = .5 * x_width
  y_center = .5 * y_height
  damping_zone = 30 * Sqr(lambda)                                     '100 for lambda = 25 to 200 for lambda = 100.
  iteration = 0
  x_start = -damping_zone
  y_start = -damping_zone
  x_stop = x_width  + damping_zone
  y_stop = y_height + damping_zone
  x_scanner = x_width' + 200
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

  Locate 36
'  Locate, 47:   ? "Place the mouse cursor here or"
'  Locate, 47:   ? "press [ M ] to display the menu.":?
'  Locate, 47:   ? "Select wavelength here    "; lambda:?
  Color dark_gray
  Locate 47, 3: ? "Thanks to the creators of FreeBASIC."
  Locate 48, 3: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47,89: ? "November 16, 2009. This program may be"
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
  line32 = " R-                                   "
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
'  Locate 48, 72: ? "R- Restart.";
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
  Case "I": Initialization()
  Case "P": line_number = 30: click = 1                               'pause.
            Screenset work_page, work_page: Color red, background
            Locate 47, 46: ? " Paused. Press any key to resume.      "
            Sleep: in_key = ""

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
'  Locate 44, 46: ? line44;: Locate, 66: Print lambda; " pixels."
'  Line(512 - 153, 186)-(512 + 152, 191),  background, bf
'  Line(512 - 153, 410)-(512 + 152, 769), black, b
  Color green_text, white
  Locate line_number, 46

'*********************************************************************
' BEGIN MOUSE CLICK PROCESSING.

  Select Case line_number
    Case 47                                                           'initialization (line 23 or 47).
      ? line23
      If click > 0 Then
        Initialization()
      End If
    Case 48                                                           'ending program.
      ? line48;
      If click > 0 Then End
  End Select
  
  If click = 1 Then                                                   'avoid repetitive actions.
    Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
  End If
End Sub


'*********************************************************************
' DISPLAYING POSITIVE AMPLITUDE IN GREEN AND NEGATIVE IN RED.
'*********************************************************************

Sub Wave_Display()
' If line_number = 1 Then Return                                      'show background (hide waves).
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page

  t_time = c_speed * iteration * 2 * pi / lambda                      'Lorentz's c = 1; wave speed is .6 pixel per cycle.

  Line(34, 16)-(34 + 10 * lambda, 16), white                          'stationary scale.
  Line(34 + 10 * lambda, 16)-(34 + 10 * lambda, 24), white
  Line(34 + 8.66 * lambda, 16)-(34 + 8.66 * lambda, 38), white
  Line(x1_emitter + x_scale, 27)-(10 * lambda + x1_emitter + x_scale, 34), white, b 'moving scale.
  For x = 0 to 9 * lambda step 2 * lambda
    Line(x + 34, 16)-(x + 34, 24), white                               'stationary scale.
    Line(x + 34 + lambda, 16)-(x + 34 + lambda, 24), white
    Line(x + x1_emitter + x_scale, 28)-(x + x1_emitter + x_scale + lambda, 33), white, bf  'moving scale.
  Next
  Circle(x1_emitter + x_scale, 65), 25, white
  Paint (x1_emitter + x_scale + 24, 65), white, white
  Circle(x1_emitter + x_scale + 10 * lambda, 65), 25, white
  Paint (x1_emitter + x_scale + 10 * lambda, 65), white, white

  Color white, black
  Locate 1,  1: ? "    0   1   2   3   4   5   6   7   8   9  10"
  Locate 18, 1: ? " t = 0"
  Locate 19, 1: ? " x = 10"
  Locate 21, 1: ? " x' = g * x + b * t =  8.66"
  Locate 22, 1: ? " t' = g * t - b * x = -8.66 sec."
  Locate 24, 1: ? " glafreniere.com      Beta (b) = v / c = 0.5"
  Locate 25, 1: ? " Contraction factor: g = Sqr(1 - beta) = 0.866";

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
    If present(x,y) > 0 Then                                          'complementary magenta and emerald green.
      r = luminance_2
      g = luminance_1
    Else
      r = luminance_1
      g = luminance_2
    End If
    If x <= x_scanner And Point(x,y) < -1 Then Pset(x,y), Rgb(r,g,b)   'printing wave area.
    If x = x_scanner then                                             'scanning one row at a time.
      r = (r_previous(y) * move_frame) + (r * (1 - move_frame))
      g = (g_previous(y) * move_frame) + (g * (1 - move_frame))
      b = (b_previous(y) * move_frame) + (b * (1 - move_frame))
      If Point(x,y) < -1 Then scan(x,y) = Rgb(r,g,b) Else scan(x,y) = Point(x,y)
    Elseif x = x_scanner then                                         'scanning one row at a time.
    End If
    r_previous(y) = r
    g_previous(y) = g
    b_previous(y) = b
  Next: Next

  If x_scanner <= x_width Then
'    For y = 0 To y_height                                             'scanning one row at a time.
'      scan(x_scanner,y) = Point(x_scanner,y)
'    Next
    For x = x_scanner To x_width: For y = 0 To y_height               'printing scanned area.
      Pset(x,y), scan(x,y)
    Next: Next
  End If

  If x_scanner > 0 Then Line(x_scanner, 0)-(x_scanner, y_height), white
End Sub


'*********************************************************************
' CIRCULAR WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Circular_Generator_Doppler()
  If stop_pulsating = 2 Then Exit Sub                                 'no pulsation.
  
  t_time = c_speed * iteration * 2 * pi / lambda                      'Lorentz's c = 1; wave speed is .6 pixel per cycle.
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
    previous_phase =  Cos(t_prime)
  End If

  If iteration < 4 * lambda Then                                      'start pulsating gradually.
    amplitude = amplitude * iteration / (4 * lambda)                  'useful for beam splitter, may be omitted.
  End If
  If stop_pulsating = 1 Then                                          'stop pulsating gradually.
    amplitude = amplitude * (target - iteration) / (2 * lambda)       'useful for beam splitter, may be omitted.
    If amplitude < 0 Then stop_pulsating = 2: Exit Sub
  End If

  For x = -lambda / 4 To lambda / 4                                   'stationary emitter.
    x_squared = x ^ 2
    potential = amplitude * Cos(t_time)
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
      If distance < lambda / 4 Then
        radian = 2 * pi * distance / lambda
        trend(x1_emitter + x, y_emitter + y) = trend(x1_emitter + x, y_emitter + y) + potential * Cos(radian)
      End If
    Next
  Next


'  For x = -(g_Lorentz * lambda) / 4 To (g_Lorentz * lambda) / 4       'moving emitter.
'    x_squared = ((x + preferred_frame) / g_Lorentz) ^ 2
'    t_prime = g_Lorentz * t_time - beta * ((x + preferred_frame) / lambda) 'Lorentz's inverted: t' = g * t - beta * x
'    potential = amplitude * Cos(t_prime)
'    For y = -lambda / 4 To lambda / 4
'      distance = Sqr(x_squared + y^2)
'      If distance < lambda / 4 Then
'        radian = 2 * pi * distance / lambda
'        trend(x2_emitter+x,y_emitter+y)=trend(x2_emitter+x,y_emitter+y) + potential * Cos(radian)
'      End If
'    Next
'  Next
End Sub
