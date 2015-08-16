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

Dim Shared As Integer r, g, b, x, y, scale, x_screen, y_screen, x_width, y_height
scale = 4 'maximum 4.
x_screen = 1024 * scale
y_screen = 768 * scale
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer x_coord, y_coord, x_squared, x_emitter, y_emitter, x_straight_emitter
Dim Shared As Integer y_parabola, y_beam_splitter, x_vertical_mirror, h_height
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulses, stop_pulsating, lambda, path_width, contraction, damping_zone = 401
Dim Shared As Integer frame, skipped_frames, line_number, axial, unit, radius, sagitta, parabola_radius, target

Dim Shared As Single amplitude, phase, distance, radian, wave_speed, ratio, brightness, move_frame, t_time, t_prime
Dim Shared As Single orthogonal, diagonal, influence, potential, k_Dewavrin, kinetic, Lagrangian, factor, previous
Dim Shared As Single energy(x_screen, y_screen), quadrature(x_screen, y_screen), beta, mirror_angle, g_Lorentz
Dim Shared As Single damping(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single past(   -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single present(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single trend(  -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As String line13, line14, line15, line16, line17, line18, line19, line20
Dim Shared As String line21, line22a, line22b, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39
Dim Shared As String line40, line41, line42, line43, line44, line45, line46, line47, line48
Dim Shared As String in_key, display, file, bitmap_number
Screen 20,24,3
visible_page = 0: work_page = 1: matrix_page = 2
scale = 1
lambda = 48 * scale
beta = 0
bitmap = 0                                                            'set bitmap = 1 for bitmap image sequence.
pulses = 6
contraction = 1
x_width  = 400 * scale
y_height = 400 * scale
skipped_frames = 6
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
'      diagonal   = present(x-1, y-1) + present(x-1, y+1) + present(x+1, y-1) + present(x+1, y+1)'diagonal influence.
      trend(x,y) = .5 * orthogonal  - past(x,y)                  'trend extrapolation.
    Next: Next

    

    move_frame += .707 * beta
    If frame = 0 Then Wave_Display()                                  'skip other frames.
'    Parabolic_Reflector()
    If move_frame > 1 Then Frame_of_Reference()
    Flat_Mirrors()
    If move_frame > 1 Then move_frame -= 1
    Damping_Management()                                              'processing damping zone.
'    Circular_Generator_Doppler()                                      'circular impulses.
    iteration += 1
'    if iteration = 1260 then sleep:end
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next
  If y_mouse < 186 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 Then                                                  'initialize bitmap = 1 for bitmap sequence.
    Select Case bitmap
      Case Is < 10: bitmap_number = "00"
      Case Is < 100: bitmap_number = "0"
      Case Is < 1000: bitmap_number = ""
    End Select
    file = "capture" + bitmap_number + Str(bitmap) + ".bmp"
    Color red, background: Locate 42, 105: Print file
    Bsave file, 0
    bitmap += 1
    If bitmap > 699 Then End
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
' FLAT MIRRORS INCLUDING BEAM SPLITTER.
'*********************************************************************

Sub Flat_Mirrors()
'
'  For x = -g_Lorentz * path_width To g_Lorentz * path_width           'upper mirror.
'    trend(x + x_emitter, 0) = 0
'    If frame = 0 And x Mod scale = 0 Then Pset((x + x_emitter) / scale, 0), white
'  Next

'  If iteration < .7 * parabola_radius / g_Lorentz Then                'temporary corner reflector.
'    For y_coord = 0 To .2 * parabola_radius
'      x = x_emitter - y_coord * g_lorentz                             'left side.
'      y = y_emitter - lambda / 2 + y_coord
'      trend(x,   y) = .5  * trend(x,   y)
'      trend(x-1, y) = 0
'      trend(x-2, y) = 0
'      x = x_emitter + y_coord * g_lorentz                             'right side.
'      trend(x-1, y) = .667 * trend(x-1, y)
'      trend(x,   y) = .333  * trend(x,   y)
'      trend(x+1, y) = 0
'    Next
'
'    If frame = 0 And y_coord Mod scale = 0 Then                       'drawing the temporary corner reflector.
'      For y_coord = 0 To .2 * parabola_radius
'        y = y_emitter - lambda / 2 + y_coord
'        x = x_emitter + y_coord * g_lorentz
'        Pset(x / scale, y / scale), white
'        x = x_emitter - y_coord * g_lorentz
'        Pset(x / scale, y / scale), white
'      Next
'    End If
'  End If

'  For y = -path_width To path_width                                   'beam splitter; .76 transmission for scale = 1.
'    x = x_emitter - y * Tan(mirror_angle)                             '               .83 transmission for scale = 2.
'    trend(x-1, y + y_beam_splitter-1) = .94 * trend(x-1, y + y_beam_splitter-1)
'    trend(x  , y + y_beam_splitter  ) = .94 * trend(x  , y + y_beam_splitter  )
'    trend(x+1, y + y_beam_splitter+1) = .94 * trend(x+1, y + y_beam_splitter+1)
'    If frame = 0 And y Mod scale = 0 Then Pset(x / scale, (y + y_beam_splitter) / scale), white
'  Next


'  If move_frame > 1 Then                                              'right side vertical mirror.
'    For y = -2*path_width To 2*path_width
'    trend(x_vertical_mirror, y_beam_splitter + y) = 0
'    Next
'  Else                                                                'no action after medium displacement.
'    For y = -2*path_width To 2*path_width
'    trend(x_vertical_mirror, y_beam_splitter + y) = 0
'    Next
'  End If

  For y = -2*path_width To 2*path_width
'    trend(x_vertical_mirror-2, y_beam_splitter + y) = .8^(1/4) * trend(x_vertical_mirror-2, y_beam_splitter + y)
'    trend(x_vertical_mirror-1, y_beam_splitter + y) = .6^(1/4) * trend(x_vertical_mirror-1, y_beam_splitter + y)
'    trend(x_vertical_mirror+0, y_beam_splitter + y) = .4^(1/4) * trend(x_vertical_mirror+0, y_beam_splitter + y)
'    trend(x_vertical_mirror+1, y_beam_splitter + y) = .2^(1/4) * trend(x_vertical_mirror+1, y_beam_splitter + y)
    trend(x_vertical_mirror+2, y_beam_splitter + y) = 0
  Next

'If iteration = 600 Then Sleep: End

  If frame = 0 Then                                                   'drawing right side vertical mirror.
    For y = -2*path_width To 2*path_width
      Pset(x_vertical_mirror / scale, (y_beam_splitter + y) / scale), white
    Next
  End If
  
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

'  For y = -path_width To path_width'one third before and one third after for smoother reflection.
'    present(x_vertical_mirror, y_beam_splitter + y)   = .333 * present(x_vertical_mirror-1, y_beam_splitter + y)
'    present(x_vertical_mirror-1, y_beam_splitter + y) = .667 * present(x_vertical_mirror-1, y_beam_splitter + y)
'    trend(x_vertical_mirror, y_beam_splitter + y)     = .333 * trend(x_vertical_mirror  -1, y_beam_splitter + y)
'    trend(x_vertical_mirror-1, y_beam_splitter + y)   = .667 * trend(x_vertical_mirror  -1, y_beam_splitter + y)
'  Next

End Sub


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Windowtitle " Test Sinus Cardinalis Impulse"
  display = "amplitude"
  iteration = 0
  brightness = 1
  g_Lorentz = Sqr(1 - beta ^ 2)                                       'Lorentz's contraction factor.
  If contraction = 0 Then g_Lorentz = 1                               'do not apply Lorentz's contraction.
  If x_width < y_height Then x_width = y_height
  x_center = .5 * x_width
  y_center = .5 * y_height
  path_width = .25 * y_height
  parabola_radius = 4 * path_width
  y_parabola = y_height - 2
  x_emitter = x_center
  y_emitter = y_parabola - .5 * parabola_radius
  y_beam_splitter = y_center
  x_vertical_mirror = x_emitter + g_Lorentz * (y_beam_splitter)
  mirror_angle = Atn(g_Lorentz / 1)
  damping_zone = 10 * Sqr(lambda * (1 + beta) / g_Lorentz)            'backward Lorentz Doppler.
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

  For x = -1*lambda To 1*lambda                                       'impulse (sinus cardinalis).
    x_squared = x ^ 2
    For y = -1*lambda To 1*lambda
      distance = Sqr(x_squared + y^2)
      If distance < 1*lambda Then
'        amplitude = 50 * 1.02 ^ -distance^2
        radian = 2 * pi * distance / lambda
        if distance = 0 Then radian = 1 Else amplitude = 30 * Sin(radian) / radian
        If y = 0 Then
          Pset(x + x_emitter, y_emitter), gray
          Pset(x + x_emitter, y_emitter - 30 * amplitude), white
        End If
        trend(x_emitter + x, y_emitter + y) = trend(x_emitter + x, y_emitter + y) + amplitude
'        trend(x_emitter + x-1, y_emitter + y-1) = trend(x_emitter + x-1, y_emitter + y-1) + amplitude
'        trend(x_emitter + x+1, y_emitter + y+1) = trend(x_emitter + x+1, y_emitter + y+1) + amplitude
'        trend(x_emitter + x-1, y_emitter + y+1) = trend(x_emitter + x-1, y_emitter + y+1) + amplitude
'        trend(x_emitter + x+1, y_emitter + y-1) = trend(x_emitter + x+1, y_emitter + y-1) + amplitude
      End If
    Next
  Next
'Sleep: End

'*********************************************************************
' Progressive damping ratio precalculus.

  For x = x_start To 0                                                'left side damping ratio.
    ratio = ((damping_zone + x) / damping_zone) ^ (1/lambda * (1 + beta) / g_Lorentz)
    For y = y_start To y_stop
      damping(x,y) = ratio
    Next
  Next
  For x = x_width To x_stop                                           'right side.
    ratio = ((x_stop - x) / damping_zone) ^ (1/lambda * (1 - beta) / g_Lorentz)
    For y = y_start To y_stop
      damping(x,y) = ratio
    Next
  Next
  For x = x_start To x_stop                                           'upper side.
    For y = y_start To 0
      ratio = ((damping_zone + y) / damping_zone) ^ (1/lambda / g_Lorentz)
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
      ratio = ((y_stop - y) / damping_zone) ^ (1/lambda / g_Lorentz)
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
  Locate, 47: ? "Select wavelength here     "; lambda:?
  Color dark_gray
  Locate 47, 3: ? "Thanks to the creators of FreeBASIC."
  Locate 48, 3: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 46,99: ? "May 7, 2010. This program"
  Locate 47,99: ? "may be freely copied,"
  Locate 48,99: ? "distributed, or modified.";
  Color green_text
  line13 = "    Please Click Slowly!              "
  line14 = "                                      "
  line15 = " A- Show wave amplitude.              "
  line16 = " B- Show wave energy.                 "
  line17 = " C- Show standing waves.              "
  line18 = " D-                                   "
  line19 = " E- Reverse wave direction.           "
  line20 = " F- Force Standing Waves.             "
  line21 = " G-                                   "
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
' line34 = " T- Constant Pulse.                   "
  line34 = " T-                                   "
  line35 = " U- Skip 6 Frames (Faster).           "
  line36 = " V- Show All Frames (Slower).         "
  line37 = " W- True Scale (Faster).              "
  line38 = " X- Accurate Double Scale (Slower).   "
  line39 = " Y- Apply Lorentz's Contraction.      "
  line40 = " Z- No Contraction.                   "
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
      If x_mouse < 400 Then x_mouse = 400 Else If x_mouse > x_screen / scale - 50 Then x_mouse = x_screen / scale
      If y_mouse < 300 Then y_mouse = 300 Else If y_mouse > y_screen / scale - 50 Then y_mouse = y_screen / scale
    Loop While click = 2
    x_width = scale * x_mouse: y_height = scale * y_mouse
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
  Locate 21, 46: ? line21
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
  Locate 35, 46: If skipped_frames > 0 Then Color blue
                 ? line35: Color green_text
  Locate 36, 46: If skipped_frames = 0 Then Color blue
                 ? line36: Color green_text
  Locate 37, 46: If scale = 1 Then Color blue
                 ? line37: Color green_text
  Locate 38, 46: If scale = 2 Then Color blue
                 ? line38: Color green_text
  Locate 39, 46: If contraction = 1 Then Color blue
                 ? line39: Color green_text
  Locate 40, 46: If contraction = 0 Then Color blue
                 ? line40: Color green_text
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
        iteration = pulses * lambda                                   'stop pulsating.
        For x = x_start To x_stop: For y = y_start To y_stop          'trend(x,y) = present(x,y) works, but hardly.
           trend(x,y) = .5 * (past(x,y) + present(x,y))
           present(x,y) = trend(x,y)
        Next: Next
      End If
  
    Case 21: ? line21                                                 '
      If click > 0 Then
      End If
  
    Case 22: If axial = 0 Then ? line22a Else ? line22b               'show axial graphics.
      If click > 0 Then
        If axial = 0 Then axial = 1 Else axial = 0
      End If
  
    Case 23, 47                                                       'initialization (line 23 or 47).
      ? line23
      If click > 0 Then
        scale = 1
        lambda = 28 * scale
        beta = .5
        pulses = 6 
        bitmap = 0
        contraction = 1
        x_width  = 768 * scale
        y_height = 768 * scale
        skipped_frames = 6
        stop_pulsating = 0
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
  
    Case 33                                                           'stop pulsating.
      If stop_pulsating = 0 Or stop_pulsating > 2 Then
        ? line33
        If click > 0 Then
          stop_pulsating = 1                                          'stop gradually;
          target = iteration + 2 * lambda                             'otherwise, a medium disequilibrium occurs.
            Color blue, background
            Screenset work_page, work_page
            Locate 33, 46: ? line33
            Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
        End If
      End If
  
    Case 34                                                           'constant pulse.
      If stop_pulsating < 3 Then
        ? line34
        If click > 0 Then
          Color blue, background
          Screenset work_page, work_page
          Locate 34, 46: ? line34
          Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
          Initialization()
          stop_pulsating = 3
        End If
      End If
  
    Case 35                                                           'skip 6 frames.
      If skipped_frames = 0 Then
      ? line35
      If click > 0 Then
        skipped_frames = 6
        Color blue, background
        Screenset work_page, work_page
        Locate 35, 46: ? line35
        Color green_text, background
        Locate 36, 46: ? line36
        Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
      End If
    End If
  
    Case 36
      If skipped_frames = 6 Then                                      'show all frames.
        ? line36
        If click > 0 Then
          skipped_frames = 0
          Color blue, background
          Screenset work_page, work_page
          Locate 36, 46: ? line36
          Color green_text, background
          Locate 35, 46: ? line35
          Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
        End If
      End If
  
    Case 37                                                           'true scale.
      If scale = 2 Then
        ? line37
        If click > 0 Then
          scale = 1
          lambda  = lambda / 2
          x_width  = x_width / 2
          y_height = y_height / 2
          Color blue, background
          Screenset work_page, work_page
          Locate 37, 46: ? line37
          Color green_text, background
          Locate 38, 46: ? line38
          Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
          Initialization()
          Screenset work_page, work_page
        End If
      End If
  
    Case 38                                                           'double scale.
      If scale = 1 Then
        ? line38
        If click > 0 Then
          scale = 2
          lambda  = lambda * 2
          x_width  = x_width * 2
          y_height = y_height * 2
          Color blue, background
          Screenset work_page, work_page
          Locate 38, 46: ? line38
          Color green_text, background
          Locate 37, 46: ? line37
          Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
          Initialization()
          Screenset work_page, work_page
        End If
      End If
  
    Case 39                                                           'apply Lorentz's contraction.
      If contraction = 0 Then
        ? line39
        If click > 0 Then
          contraction = 1
          Color blue, background
          Screenset work_page, work_page
          Locate 39, 46: ? line39
          Color green_text, background
          Locate 40, 46: ? line40
          Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
          Initialization()
          Screenset work_page, work_page
        End If
      End If
  
    Case 40                                                           'don't apply Lorentz's contraction.
      If contraction = 1 Then
        ? line40
        If click > 0 Then
          contraction = 0
          Color blue, background
          Screenset work_page, work_page
          Locate 40, 46: ? line40
          Color green_text, background
          Locate 39, 46: ? line39
          Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
          Initialization()
          Screenset work_page, work_page
        End If
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
          Locate 43, 46: ? "  4                                12"
          Locate 44, 46: ? " Currently"; pulses; " ="; pulses * lambda; " pixels."
          Line(512 - 124, 672)-(512 + 116, 688), buff, bf
          Line(512 - 124, 672)-(512 + 116, 688), black, b
          Do
            Getmouse x_mouse, y_mouse, wheel, click
            If x_mouse > 512 - 122 And x_mouse < 512 + 114 Then
              If x_coord <> x_mouse Then Line(x_coord, 673)-(x_coord, 687), buff
              Line(x_mouse, 673)-(x_mouse, 687), black
              Locate 42, 46
              ? " Selecting Pulses"; Int(x_mouse / 29) - 9; " ="; (Int(x_mouse / 29) - 9) * lambda; " pixels.  "
              x = x_coord
              x_coord = x_mouse
            End If
          Loop While click
          pulses = Int(x / 29) - 9
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
' HARD REFLECTION ON PARABOLA.
'*********************************************************************

Sub Parabolic_Reflector()
  For x = -g_Lorentz * path_width To g_Lorentz * path_width
    y = (x / g_Lorentz) ^ 2 / 2 / parabola_radius                     'classic parabola sagitta.
    trend(x + x_emitter-1, y_parabola - y) = .667 * trend(x + x_emitter-1, y_parabola - y)
    trend(x + x_emitter,   y_parabola - y) = .333 * trend(x + x_emitter  , y_parabola - y)    
    trend(x + x_emitter+1, y_parabola - y) = 0
  Next
    
  If frame = 0 Then                                                   'drawing the reflector.
    For x = -path_width To path_width Step scale
      y = x ^ 2 / 2 / parabola_radius                                 'true parabola.
      Pset((x + x_emitter) / scale, (y_parabola - y) / scale), green
    Next
    For x = -g_Lorentz * path_width To g_Lorentz * path_width Step scale
      y = (x / g_Lorentz) ^ 2 / 2 / parabola_radius                   'squashed parabola.
      Pset((x + x_emitter) / scale, (y_parabola - y) / scale), white
    Next
  End If
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
    For x = 0 To x_width Step scale: For y = 0 To y_height Step scale
      luminance_1 = brightness * Abs(trend(x,y) + trend(x,y+1) + trend(x+1,y) + trend(x+1,y+1))
      b = luminance_1 / 2
      If b > 255 Then b = 255
      If luminance_1 > 255 Then
        luminance_2 = luminance_1 - 255
        If luminance_2 > 255 Then luminance_2 = 255
        luminance_1 = 255
      Else luminance_2 = 0
      End If
      If trend(x,y) > 0 Then                                          'using complementary magenta and emerald green.
        r = luminance_2
        g = luminance_1
      Else
        r = luminance_1
        g = luminance_2
      End If
      Pset(x / scale, y / scale), Rgb(r,g,b)
      If y = y_emitter Then
        Pset(x / scale-1, y / scale - .5*trend(x,y)), white
        Pset(x / scale-1, y / scale), black
      End If
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
  
                                                                      'axial graphics, left side.
'  Line(0, y_beam_splitter / scale)-((x_emitter - path_width) / scale, y_beam_splitter / scale), gray
'  For distance = 0 To (x_emitter - path_width) / scale Step (.5 / scale) * (lambda / g_Lorentz) * (1 + beta)'backward Doppler.
'    Line(distance, y_beam_splitter / scale - 10)-(distance, y_beam_splitter / scale + 10), white
'  Next
'  previous = y_beam_splitter / scale
'  For x = 0 To (x_emitter - path_width) / scale Step scale
'    luminance_1 = brightness * (.1 * (trend(x,y_beam_splitter) + trend(x,y_beam_splitter+1) + trend(x+1,y_beam_splitter) + trend(x+1,y_beam_splitter+1)))
'    Line(x / scale -1, previous)-(x / scale, y_beam_splitter / scale - luminance_1), white
'    previous = y_beam_splitter / scale - luminance_1
'  Next
'                                                                      'axial graphics, right side.
'  Line(x_vertical_mirror / scale - 2 * g_Lorentz * lambda / scale, y_beam_splitter / scale)-(x_vertical_mirror / scale, y_beam_splitter / scale), gray
'  For distance = x_vertical_mirror / scale To x_vertical_mirror / scale - 2 * g_Lorentz * lambda / scale Step -(.5 * g_Lorentz * lambda / scale)'standing wave axial contraction.
'    Line(distance, y_beam_splitter / scale - 10)-(distance, y_beam_splitter / scale + 10), white
'  Next
'  previous = y_beam_splitter / scale
'  For x = x_vertical_mirror - 2 * g_Lorentz * lambda + 1 To x_vertical_mirror Step scale
'    luminance_1 = brightness * (.1 * (trend(x,y_beam_splitter) + trend(x,y_beam_splitter+1) + trend(x+1,y_beam_splitter) + trend(x+1,y_beam_splitter+1)))
'    Line(x / scale -1, previous)-(x / scale, y_beam_splitter / scale - luminance_1), white
'    previous = y_beam_splitter / scale - luminance_1
'  Next
'  
'  If beta > 0 Then
'    Line((x_emitter - g_Lorentz * path_width) / scale, 0)-((x_emitter - g_Lorentz * path_width) / scale, y_height / scale), gray
'    Line((x_emitter + g_Lorentz * path_width) / scale+1, 0)-((x_emitter + g_Lorentz * path_width) / scale+1, y_height / scale), gray
'  End If
'
'  Line(x_center / scale - 5, y_beam_splitter / scale)-(x_center / scale + 5, y_beam_splitter / scale), white
'  Line(x_center / scale, y_beam_splitter / scale - 5)-(x_center / scale, y_beam_splitter / scale + 5), white
'  Line((x_emitter - path_width) / scale, 0)-((x_emitter - path_width) / scale, y_height / scale), green
'  Line((x_emitter + path_width) / scale, 0)-((x_emitter + path_width) / scale, y_height / scale), green
'  Line((x_emitter - path_width) / scale, 0)-((x_emitter + path_width) / scale, 0), green
'  Line(0, (y_beam_splitter - path_width) / scale)-(x_width / scale, (y_beam_splitter - path_width) / scale), green
'  Line(0, (y_beam_splitter + path_width) / scale)-(x_width / scale, (y_beam_splitter + path_width) / scale), green
'  Line(x_emitter / scale + y_beam_splitter / scale, (y_beam_splitter - path_width) / scale)-(x_emitter / scale + y_beam_splitter / scale, (y_beam_splitter + path_width) / scale), green
'  Line(x_emitter / scale, 0)-(x_emitter / scale, 2 * lambda / scale), white
'  Line(x_emitter / scale, (y_parabola - 2 * lambda) / scale)-(x_emitter / scale, y_parabola / scale), white
'  For y = .5 * lambda To 2 * lambda Step lambda / 2
'    Line(x_emitter / scale - 10, y / scale)-(x_emitter / scale + 10, y / scale), white
'    Line(x_emitter / scale - 10, (y_parabola - y) / scale)-(x_emitter / scale + 10, (y_parabola - y) / scale), white
'  Next
  Color white, black
'  Line(0,0)-(184,144), black, bf
'  Locate 48, 92: Print Using "#####"; Int(iteration / (skipped_frames + 1));
'  Locate 44,104: Print  " Iteration"; iteration; " ";
'  Locate 1, 5: ? "The  Michelson"
'  Locate 2, 5: ? "Interferometer"
'  Locate 4, 1: ? " Beta (v/c)  "; beta; "   "
'  Locate 5, 1: ? " Wavelength  "; lambda / scale; " pixels"
'  Locate 6, 1
'  If contraction = 0 or beta = 0 Then
'    Color red
'    ? " No Contraction.";
'    Color white
'  Else
'    ? " Contraction";
'    ? Using "###.###"; g_Lorentz
'  End If  
'  Locate 7, 1: ? " Mirror Angle";
'  ? Using " ##.##"; mirror_angle * 180 / pi;: ? Chr(248);
'  Locate 8, 1: ? " Transverse wave angle:";
'  Locate 9, 1: ? Using " arc sin beta ##.##"; asin(beta) * 180 / pi;: ? Chr(248);
'  Locate 48, 13: If g_Lorentz = 1 Then ? "  Regular Parabola "; Else ? " Squashed Parabola ";
'  Locate 48, 66: ? " glafreniere.com ";
End Sub


'*********************************************************************
' CIRCULAR WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Circular_Generator_Doppler()
  If stop_pulsating = 2 Then Exit Sub                                 'no pulsation.
  t_time = iteration * 2 * pi / lambda                                'Lorentz's c = 1; wave speed is 1 pixel per cycle.
  amplitude = 5000 / lambda ^ 2
  
  If stop_pulsating < 2 Then                                          'set stop_pulsating = 3 for constant pulse.
    If iteration < 2 * lambda Then                                    'start pulsating gradually.
      amplitude = amplitude * iteration / (2 * lambda)
    Elseif iteration = pulses * lambda - 2 * lambda Then              'set stop_pulsating = 1 to stop gradually;
      If stop_pulsating <> 1 Then
        stop_pulsating = 1                                            'otherwise, a medium disequilibrium occurs.
        target = iteration + 2 * lambda
      End If
    End If
  End If

  If stop_pulsating = 1 Then
    If iteration = target Then stop_pulsating = 2: Exit Sub           'stop pulsating totally.
    amplitude = amplitude * (target - iteration) / (2 * lambda)       'stop pulsating gradually.
  End If

  For x = -(g_Lorentz * lambda) / 4 To (g_Lorentz * lambda) / 4
    x_squared = (x / g_Lorentz) ^ 2
    t_prime = g_Lorentz * t_time - beta * (x / lambda)                'Lorentz's inverted: t' = g * t - beta * x
    potential = amplitude * Cos(t_prime)
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
      If distance < lambda / 4 Then
        radian = 2 * pi * distance / lambda
        trend(x_emitter + x, y_emitter + y) = trend(x_emitter + x, y_emitter + y) + potential * Cos(radian)
        trend(x_emitter + x-1, y_emitter + y-1) = trend(x_emitter + x-1, y_emitter + y-1) + .5 * potential * Cos(radian)
        trend(x_emitter + x+1, y_emitter + y+1) = trend(x_emitter + x+1, y_emitter + y+1) + .5 * potential * Cos(radian)
        trend(x_emitter + x-1, y_emitter + y+1) = trend(x_emitter + x-1, y_emitter + y+1) + .5 * potential * Cos(radian)
        trend(x_emitter + x+1, y_emitter + y-1) = trend(x_emitter + x+1, y_emitter + y-1) + .5 * potential * Cos(radian)
      End If
    Next
  Next
End Sub
