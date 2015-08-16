Width 80,20:Color 0,15:Cls:?
? " Updated in June 2010 by Gabriel LaFreniere.":?:?
? " This FreeBasic program was adapted to the 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " It should still be compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?

' Gosub commands are not supported any more.
' All variables must be declared.
' Subs are in alphabetical order. Press F2 and double-click "Subs"
' to display the list. Then double-click a Sub to display it.

Declare Sub Damping_Management()
Declare Sub Display()
Declare Sub Elliptic_Reflector()
Declare Sub Emitter_Single()
Declare Sub Emitter_Management()
Declare Sub Frame_of_Reference()
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Print_Galaxy_Single()

Const pi = 4 * Atn(1)
Const black = 0, white = -1, purple = -65281, gray = -5592406, yellow = -256, blue = -10184961'-16751361'gray -6908266
Const red = -65536, green = -16726016, cyan = -16725816, dark = 1, bright = -257
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), dark_gray = Rgb(75,75,75)

Dim Shared As Integer x_screen = 1280, y_screen = 1024, x_width = 1279, y_height = 719
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_alpha, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer r, g, b, j, x, y, scanner, x_scanner, x_previous, x_squared, x_text, colour
Dim Shared As Integer x_A_Int, x_B, x_C, x_D, x_E, y_D, y_E, x_origin, y_clock, x_emitter, y_emitter
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display, radius = 50, lambda = 48
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap, target
Dim Shared As Integer iteration, damping_zone = 401, x_y(-radius To radius, -radius To radius)
Dim Shared As Integer frame, skipped_frames, line_number, axial, unit, scan(-4 To x_width + 4, y_height)

Dim Shared As Single alpha, beta, beta_A, beta_B, beta_C, beta_D, beta_E, beta_F, beta_x, beta_y, beta_xy, xy
Dim Shared As Single g_Lorentz, g_alpha, g_Lorentz_A, g_Lorentz_B, g_Lorentz_C, g_Lorentz_D
Dim Shared As Single  g_Lorentz_E, g_Lorentz_F, g_Lorentz_x, g_Lorentz_y, g_Lorentz_xy
Dim Shared As Single x_A, x_coord, y_coord, x_prime, y_prime, t_time, t_prime, t_radian, t_prime_x, t_prime_y, t_prime_xy
Dim Shared As Single k_Dewavrin, move_frame, frame_speed, c_speed, beta_prime, half_chord
Dim Shared As Single orthogonal, diagonal, influence, potential, previous, ratio, factor, sagitta
Dim Shared As Single amplitude, phase, distance, radian, code, brightness, decimal, curve, soft
Dim Shared As Single synchro, start_A, start_B, start_C, start_D
Dim Shared As Single ellipse_radius, circle_radius, sphere_radius, twins_distance, wave_speed
Dim Shared As Single pulse(-lambda / 4 To lambda / 4, -lambda / 4 To lambda / 4)
Dim Shared As Single distance_xy(-lambda / 4 To lambda / 4, -lambda / 4 To lambda / 4)
Dim Shared As Single damping(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single past(   -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single present(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single trend(  -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)

Dim Shared As String line58, line59, line60, line61, line62, line63, line64
Dim Shared As String in_key, file, bitmap_number
visible_page = 0: work_page = 1: matrix_page = 2
wave_display = 0: skipped_frames = 9: scanner = 0: bitmap = 0         'set bitmap = 1 for BMP image sequence.
Initialization()

'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  For frame = 0 To skipped_frames

' JOCELYN MARCOTTE'S 2-D WAVE ALGORITHM (CREATED IN 2006).            "THE PAST IS A GUIDE TO THE FUTURE"

    If wave_display Then
      For x = x_start To x_stop: For y = y_start To y_stop
        past(x,y)  = present(x,y)                                     'updating previous states.
        present(x,y) = trend(x,y)
      Next: Next
      For x = x_start To x_stop: For y = y_start To y_stop
        orthogonal = present(x-1, y) + present(x, y-1) + present(x, y+1) + present(x+1, y) 'orthogonal influence.
'        trend(x,y) = .5 * orthogonal - past(x,y)                     'fastest trend extrapolation (constant pixel sum = 1).
'        trend(x,y) = .4 * (orthogonal + present(x,y)) - past(x,y)    'slower wave speed: .4 * (4 + 1) - 1 = 1
        trend(x,y) = .25 * (2 - wave_speed) * orthogonal + wave_speed * present(x,y) - past(x,y) 'using a slower wave speed
      Next: Next                                                      'because the scan speed must be 1 / beta - alpha = 1 pixel per loop.
    End If
    sphere_radius = iteration * c_speed
    t_time = iteration * c_speed                                      'absolute t time, equals the expanding cosmic sphere radius.
    twins_distance = beta_C * sphere_radius
    If scanner Then move_frame += c_speed * alpha
    If move_frame > 1 Then move_frame -= 1: Frame_of_Reference()
    x_A = x_A_Int - move_frame
    x_B = x_A + beta_B * sphere_radius                                'B is moving rightward (beta = 0.5 c).
    x_C = x_A + beta_C * sphere_radius                                'C is moving rightward (beta = 0.8 c).
    Damping_Management()                                              'processing damping zone.
    Emitter_Management()
    Elliptic_Reflector()
    iteration += 1
    If scanner Then
      x_scanner += 1
      If x_scanner = x_width + 2 Then Sleep: If Inkey = Chr(27) Then End
      If x_scanner > x_width + 2 Then x_scanner = x_width + 3
    End If
    If iteration = 2230 Then Sleep: If Inkey = Chr(27) Then End
    If iteration > 2300 Then Sleep: End
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
    Sleep 1                                                           'CPU access for multitask.
  Next

  Display()                                                           'skip other frames.
  If y_mouse < 768 Or y_mouse > 1024 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And iteration Mod 2 = 0 Then                          'set bitmap = 1 for bitmap sequence.
'  If bitmap > 0 Then
    Select Case bitmap
      Case Is < 10:    bitmap_number = "000"
      Case Is < 100:   bitmap_number = "00"
      Case Is < 1000:  bitmap_number = "0"
      Case Is < 10000: bitmap_number = ""
    End Select
    file = "capture_" + bitmap_number + Str(bitmap) + ".bmp"
    Color red, background
    Locate 52, 50: Print file
    Locate 54, 50: Print "Warning! A bitmap sequence is being created in the current directory."
    Bsave file, 0
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
' DISPLAYING THE GRAPHICS.
'*********************************************************************

Sub Display()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Color white, black                                                  'black is transparent to waves, white and dark are opaque.
  x_D = x_B                                                           'D, B and E are on the same vertical row (beta = 0.5 c).
  x_E = x_B
  y_D = y_center - beta_B * g_Lorentz_B * t_time
  y_E = y_center + beta_B * g_Lorentz_B * t_time
  Line(x_A - sphere_radius, y_center)-(x_A + sphere_radius, y_center), gray  'gray horizontal x axis.
  Line(x_A, y_center - sphere_radius)-(x_A, y_center + sphere_radius), gray  'gray vertical axis.

'  If scanner = 0 Then                                                'the scanner sometimes misses thin vertical lines.
    Circle(x_A, y_center), sphere_radius, gray,,, beta_B              'horizontal .5 c position (ellipses).
    Circle(x_A, y_center), sphere_radius, gray,,, beta_C              'horizontal .5 c position (ellipses).
    Circle(x_A, y_center), sphere_radius, gray,,, beta_D              'horizontal .8 c position.
    Circle(x_A, y_center), sphere_radius, gray,,, beta_E              'horizontal .92857 c position.
    Circle(x_A, y_center), sphere_radius, gray,,, beta_F              'horizontal .9756 c position.
    Line(x_A, y_center - sphere_radius)-(x_A, y_center + sphere_radius), gray 'vertical cenral position
    x_coord = beta_B * sphere_radius
    y_coord = g_Lorentz_B * sphere_radius
    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray 'gray vertical axes.
    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
    x_coord = beta_C * sphere_radius
    y_coord = g_Lorentz_C * sphere_radius
    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray
    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
    x_coord = beta_D * sphere_radius
    y_coord = g_Lorentz_D * sphere_radius
    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray
    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
    x_coord = beta_E * sphere_radius
    y_coord = g_Lorentz_E * sphere_radius
    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray
    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
    x_coord = beta_F * sphere_radius
    y_coord = g_Lorentz_F * sphere_radius
    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray
    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
'  End If

  beta = alpha * g_alpha                                              'transverse speed of D and E, a bit slower than that of B whose speed is alpha.
  g_Lorentz = Sqr(1 - beta ^ 2)                                       'mean contraction for expanding (A to C) and moving ellipse or ellipsoidal mirror. 
  x_coord = .5 * (x_A + x_C)                                          'center of the expanding ellipse or mirror.
  Circle(x_coord,y_center),.5*(x_C-x_A)/g_Lorentz,white,,, 1/g_Lorentz 'full expanding ellipse.
  For radian = 0 To 2 * pi Step pi / sphere_radius                    'expanding ellipsoid mirror - see "Elliptic_Reflector()".
    If radian < .05 Then                                              'small portions of the ellipsoidal mirror: .05 times 2 * pi each.
'      code = 1
    Elseif radian > 1 * .125 * 2 * pi - .05 And radian < 1 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 2 * .125 * 2 * pi - .05 And radian < 2 * .125 * 2 * pi + .05 Then code = 1
    Elseif radian > 3 * .125 * 2 * pi - .05 And radian < 3 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 4 * .125 * 2 * pi - .05 And radian < 4 * .125 * 2 * pi + .05 Then code = 1
    Elseif radian > 5 * .125 * 2 * pi - .05 And radian < 5 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 6 * .125 * 2 * pi - .05 And radian < 6 * .125 * 2 * pi + .05 Then code = 1
    Elseif radian > 7 * .125 * 2 * pi - .05 And radian < 7 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 8 * .125 * 2 * pi - .05                                     Then code = 1
    Else: code = 0
    End If
    If code Then
      x_coord = beta_B * sphere_radius * Cos(radian)                  'axial coordinate before transformation (proportional to speed).
      y_coord = beta_B * sphere_radius * Sin(radian)                  'transverse coordinate before transformation.
      beta = x_coord / t_time                                         'speed of the current spot before acceleration (leftward if beta is negative).
      g_Lorentz = Sqr(1 - beta^2)                                     'Lorentz contraction factor for the current spot.
      y_coord = y_coord / g_Lorentz                                   'transverse dilation in order to balance the final contraction below.
      beta = (beta + beta_B) / (1 + beta * beta_B)                    'accelerating the current spot according to beta_1.
      x_coord = beta * sphere_radius                                  'final coordinate for the current spot.
      beta = x_coord / t_time                                         'final speed of the current spot.
      g_Lorentz = Sqr(1 - beta^2)                                     'Lorentz contraction factor for the current spot.
      y_coord = g_Lorentz * y_coord                                   'transverse contraction according to my new three dimensional Lorentzian equations.
      Circle(x_A + x_coord, y_center + y_coord), 2, white,,,1
      Circle(x_A + x_coord, y_center + y_coord), 1, white,,,1
    End If
  Next

  Line(x_A, y_center)-(x_C, y_center), white                          'white horizontal axis (A to C).
  Line(x_B, y_D)-(x_B, y_E), white                                    'white vertical axis (D to E).
  
  For x = -radius To radius                                           'printing the galaxies.
    For y = -radius To radius                                         'scanning x and y, one pixel at a time.
      If x_y(x, y) Then                                               'copying the matrix color only when x_y(x, y) code = 1.
        
        beta_x = 0
        beta_y = 0
        Print_Galaxy_Single()
        beta_y = beta_C
        Print_Galaxy_Single()
        beta_y = -beta_C
        Print_Galaxy_Single()

        beta_x = -beta_B
        beta_y = beta_B
        Print_Galaxy_Single()
        beta_y = -beta_B
        Print_Galaxy_Single()

        beta_x = beta_B
        beta_y = beta_B
        Print_Galaxy_Single()
        beta_y = -beta_B
        Print_Galaxy_Single()

        beta_x = beta_C
        beta_y = 0
        Print_Galaxy_Single()
        beta_y = beta_C
        Print_Galaxy_Single()
        beta_y = -beta_C
        Print_Galaxy_Single()

        beta_x = beta_D
        beta_y = beta_B
        Print_Galaxy_Single()
        beta_y = -beta_B
        Print_Galaxy_Single()
        beta_y = beta_D
        Print_Galaxy_Single()
        beta_y = -beta_D
        Print_Galaxy_Single()

        beta_x = beta_E
        beta_y = 0
        Print_Galaxy_Single()
        beta_y = beta_C
        Print_Galaxy_Single()
        beta_y = -beta_C
        Print_Galaxy_Single()
        beta_y = beta_E
        Print_Galaxy_Single()
        beta_y = -beta_E
        Print_Galaxy_Single()

        beta_x = beta_F
        beta_y = beta_B
        Print_Galaxy_Single()
        beta_y = -beta_B
        Print_Galaxy_Single()
        beta_y = beta_D
        Print_Galaxy_Single()
        beta_y = -beta_D
        Print_Galaxy_Single()
        beta_y = beta_F
        Print_Galaxy_Single()
        beta_y = -beta_F
        Print_Galaxy_Single()
      End If
    Next
  Next

  Color white, dark                                                   'dark is opaque to waves.
  Locate 1, 1:  ? "D"                                                 'galaxy D identification.
  Circle(x_B, y_D), 8, dark,,,1
  Paint(x_B, y_D), dark, dark
  For x = 0 To 7: For y = 0 To 15
    If Point(x,y) = -1 Then Pset(x + x_B - 3, y + y_D - 7), white
  Next:Next

  Locate 1, 1:  ? "E"                                                 'galaxy E identification.
  Circle(x_B, y_E), 8, dark,,,1
  Paint(x_B, y_E), dark, dark
  For x = 0 To 7: For y = 0 To 15
    If Point(x,y) = -1 Then Pset(x + x_B - 3, y + y_E - 7), white
  Next:Next

  Locate 1, 1:  ? "B"                                                 'galaxy B identification.
  Circle(x_B, y_center), 8, dark,,,1
  Paint(x_B, y_center), dark, dark
  For x = 0 To 7: For y = 0 To 15
    If Point(x,y) = -1 Then Pset(x + x_B - 3, y + y_center - 7), white
  Next:Next

  Locate 1, 1:  ? "C"                                                 'galaxy C identification.
  Circle(x_C, y_center), 8, dark,,,1
  Paint(x_C, y_center), dark, dark
  For x = 0 To 7: For y = 0 To 15
    If Point(x,y) = -1 Then Pset(x + x_C - 3, y + y_center - 7), white
  Next:Next

  Locate 1, 1:  ? "A"                                                 'central unmoving galaxy A identification.
  Circle(x_A, y_center), 8, dark,,,1
  Paint(x_A, y_center), dark, dark
  For x = 0 To 7: For y = 0 To 15
    If Point(x,y) = -1 Then
      Pset(x + x_A - 3, y + y_center - 7), white
      Else Pset(x + x_A - 3, y + y_center - 7), dark
    End If
  Next:Next
  Locate 1, 1:  ? " "
  Circle(x_A, y_center),     sphere_radius, white,,,1                 'cosmic sphere growing at the speed of light.
  Color dark, background
  Line(0, 0)-(x_text, y_height), background, BF                       'opaque text box.
  Line(0, 0)-(x_text, y_height), white, B 
  Line(1, 1)-(x_text-1, y_height-1), white, B
  Line(2, 2)-(x_text-2, y_height-2), white, B
  Locate 02, 06: ? "   The Relativistic Big Bang"
  Locate 03, 06: ? "Observers Apparently Equidistant"
  Locate 05, 03: ? "Normalized speed beta:"
  Locate 06, 03: ?  Chr(225);"[x, y, z] = v[x, y, z] / c"
  Locate 07, 03: ? "Lorentz's contraction factor:"
  Locate 08, 03: ? "g[x, y, z] = Sqr(1 - ";Chr(225);"[x, y, z]^2)"
  Locate 10, 03: ? "The Lorentz modified and now all"
  Locate 11, 03: ? "azimuth transformations:"
  Locate 13, 03: ? "t[x]'   = g[x] * t      - ";Chr(225);"[x] * x"
  Locate 14, 03: ? "t[xy]'  = g[y] * t[x]'  - ";Chr(225);"[y] * y"
  Locate 15, 03: ? "t[xyz]' = g[z] * t[xy]' - ";Chr(225);"[z] * z"
  Locate 17, 03: ? "x' = g[x] * x + ";Chr(225);"[x] * t"
  Locate 18, 03: ? "y' = g[y] * y + ";Chr(225);"[y] * t[x]'"
  Locate 19, 03: ? "z' = g[z] * z + ";Chr(225);"[z] * t[xy]'"
  Locate 21, 03: ? Chr(225);"[xy]  = Sqr(";Chr(225);"[x]^2  + (g[x]  * ";Chr(225);"[y])^2)"
  Locate 22, 03: ?         "g[xy]  = Sqr(1 - ";Chr(225);"[xy]^2)"
  Locate 23, 03: ? Chr(225);"[xyz] = Sqr(";Chr(225);"[xy]^2 + (g[xy] * ";Chr(225);"[z])^2)"
  Locate 24, 03: ?         "g[xyz] = Sqr(1 - ";Chr(225);"[xyz]^2)"

' t_prime_x  = g_Lorentz_x * t_time    - beta_x * x                   'works perfectly in 2-D.
' t_prime_xy = g_Lorentz_y * t_prime_x - beta_y * y
' x_prime    = g_Lorentz_x * x         + beta_x * t_time
' y_prime    = g_Lorentz_y * y         + beta_y * t_prime_x

  Locate 26, 08: ? Chr(225); "        g"
  Locate 27, 03: ? "A";
  Print Using "###.#####"; beta_A;
  Print Using "###.#####"; g_Lorentz_A;: Print "     ";Chr(225);"[y] = 0"
  Locate 28, 03: ? "B";
  Print Using "###.#####"; beta_B;
  Print Using "###.#####"; g_Lorentz_B;: Print "     ";Chr(225);"[y] = 0"
  Locate 29, 03: ? "C";
  Print Using "###.#####"; beta_C;
  Print Using "###.#####"; g_Lorentz_C;: Print "     ";Chr(225);"[y] = 0"
  beta = Sqr(alpha^2+(g_alpha*alpha)^2)
  g_Lorentz = Sqr(1 - beta^2)
  Locate 30, 03: ? "D";
  Print Using "###.#####"; beta;
  Print Using "###.#####"; g_Lorentz;: Print "     ";Chr(225);"[x] = .26795"
  Locate 31, 03: ? "E";
  Print Using "###.#####"; beta;
  Print Using "###.#####"; g_Lorentz;: Print "     ";Chr(225);"[y] = .26795"
  Line(180, 466)-(193, 479), black
  Line(180, 492)-(193, 479), black
  Line(181, 466)-(194, 479), black
  Line(181, 492)-(194, 479), black
  Locate 33, 03: ? "Distance (pixel):"
  Locate 34, 03: ? "A to B "
  Locate 35, 03: ? "B to C "
  Locate 36, 03: ? "A to C "
  Locate 37, 03: ? "B to D "
  Locate 38, 03: ? "B to E "
  Locate 34, 10: ? x_B - x_A                                          'distance in pixels.
  Locate 35, 10: ? x_C - x_B
  Locate 36, 10: ? x_C - x_A
  Locate 37, 10: ? y_center - y_D
  Locate 38, 10: ? y_E - y_center

  Locate 40, 03: ? "Observer A is stationary. Observer C is"
  Locate 41, 03: ? "moving rightward at half of the speed  "
  Locate 42, 03: ? "of light. Observer B is moving at the  "
  Locate 43, 03: ? "intermediate speed, which is given by: "
  Locate 44, 03: ? "alpha = (1 - g) / ";Chr(225);

'*********************************************************************
' DISPLAYING THE WAVES - POSITIVE AMPLITUDE: GREEN; NEGATIVE: RED.
'*********************************************************************

  For x = x_text To x_width: For y = 0 To y_height
    If wave_display Then
      luminance_1 = brightness * Abs(trend(x,y))
      b = luminance_1 / 2
      If b > 255 Then b = 255
      If luminance_1 > 255 Then
        luminance_2 = luminance_1 - 255
        If luminance_2 > 255 Then luminance_2 = 255
        luminance_1 = 255
      Else luminance_2 = 0
      End If
      If present(x,y) > 0 Then                                        'complementary magenta and emerald green.
        r = luminance_2
        g = luminance_1
      Else
        r = luminance_1
        g = luminance_2
      End If
      If Point(x,y) = white Or Point(x,y) = red Or Point(x,y) = blue Or Point(x,y) = purple Or Point(x,y) = green Or Point(x,y) = gray Or Point(x,y) = dark Then
      Else
        If scanner = 0 Then
          Pset(x,y), Rgb(r,g,b)                                       'printing wave area.
        Else
          If x > x_scanner Then Pset(x,y), Rgb(r,g,b)
        End If
      End If
    End If
    If x = x_scanner And x > -1 And x < x_width Then                  'scanning one row at a time.
      r = (r_previous(y) * (1 - move_frame)) + (r * move_frame)
      g = (g_previous(y) * (1 - move_frame)) + (g * move_frame)
      b = (b_previous(y) * (1 - move_frame)) + (b * move_frame)
      If Point(x,y) = white Or Point(x,y) = red Or Point(x,y) = blue Or Point(x,y) = purple Or Point(x,y) = green Or Point(x,y) = gray Or Point(x,y) = dark Then
           scan(x,y) = Point(x,y)
      Else scan(x,y) = Rgb(r,g,b)
      End If
    End If
    r_previous(y) = r
    g_previous(y) = g
    b_previous(y) = b
  Next: Next
  
  Color black, background
  If scanner Then Line(x_scanner,0)-(x_scanner,y_height), white       'scanner line.
  Locate 53, 03: Print "Iteration... "; iteration
  Locate 54, 03: Print "Scanner..... "; x_scanner

  If scanner And x_scanner > -1 Then
    For x = 0 To x_scanner - 1: For y = 0 To y_height                 'printing scanned area.
      Pset(x,y), scan(x,y)
    Next: Next
  End If
End Sub


'*********************************************************************
' ELLIPTIC REFLECTOR MADE OUT OF 48 INDIVIDUAL REFLECTING SPOTS.
'*********************************************************************

Sub Elliptic_Reflector()                                              'see display() above for calculus details.
  For radian = 0 To 2 * pi Step pi / sphere_radius
    If radian < .05 Then
'      code = 1
    Elseif radian > 1 * .125 * 2 * pi - .05 And radian < 1 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 2 * .125 * 2 * pi - .05 And radian < 2 * .125 * 2 * pi + .05 Then code = 1
    Elseif radian > 3 * .125 * 2 * pi - .05 And radian < 3 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 4 * .125 * 2 * pi - .05 And radian < 4 * .125 * 2 * pi + .05 Then code = 1
    Elseif radian > 5 * .125 * 2 * pi - .05 And radian < 5 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 6 * .125 * 2 * pi - .05 And radian < 6 * .125 * 2 * pi + .05 Then code = 1
    Elseif radian > 7 * .125 * 2 * pi - .05 And radian < 7 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 8 * .125 * 2 * pi - .05                                     Then code = 1
    Else: code = 0
    End If

    If code Then
      x_coord = beta_B * sphere_radius * Cos(radian)
      y_coord = beta_B * sphere_radius * Sin(radian)
      beta = x_coord / t_time
      g_Lorentz = Sqr(1 - beta^2)
      y_coord = y_coord / g_Lorentz
      beta = (beta + beta_B) / (1 + beta * beta_B)
      x_coord = beta * sphere_radius
      beta = x_coord / t_time
      g_Lorentz = Sqr(1 - beta^2)
      y_coord = g_Lorentz * y_coord
      x_coord = x_coord + x_A                                         'pixel coordinates.
      y_coord = y_coord + y_center
      For x = -5 To 5
        x_squared = x^2
        For y = -5 To 5
          distance = Sqr(x_squared + y^2)
          If distance < 5 Then trend(x + x_coord, y + y_coord) = soft * trend(x + x_coord, y + y_coord)
        Next
      Next
    End If
  Next
End Sub


'*********************************************************************
' PREPARING DATA FOR MULTIPLE EMITTERS.  
'*********************************************************************

Sub Emitter_Management()
  If iteration < start_B + 1 Then Exit Sub
  If iteration = start_B + 1 Then wave_display = 1
  If iteration > start_B And iteration < start_B + lambda / g_Lorentz_B / c_speed Then '24 /.9634/.577: (1 * lambda / g / c) or multiples. 
    beta_x = beta_B
    beta_y = 0
    Emitter_Single()
  End If
  If iteration > start_A And iteration < start_A + lambda / g_Lorentz_A / c_speed Then
    beta_x = 0
    beta_y = 0
    Emitter_Single()
  End If
  If iteration > start_D And iteration < start_D + lambda / Sqr(1-(Sqr(alpha^2+(g_alpha*alpha)^2))^2) / c_speed Then
    beta_x = alpha
    beta_y = beta_B
    Emitter_Single()
    beta_x = alpha
    beta_y = -beta_B
    Emitter_Single()
  End If
  If iteration > start_C And iteration < start_C + lambda / g_Lorentz_C / c_speed Then
    beta_x = beta_C
    beta_y = 0
    Emitter_Single()
  End If
End Sub


' *********************** MOVING EMITTER *****************************  
' The Doppler emitters below are making use of my May 2006 modified
' and April 2010 three-dimensional Lorentzian equations. Now, any
' direction is allowed. The emitting sinusoidal lambda / 4 area is
' undergoing a contraction in the direction of motion. Additionally,
' the phase is in advance at the rear of the area in accordance with
' Lorentz's well-known "local time". The result is a slower pulsation
' frequency which was also predicted by Lorentz. This procedure 
' produces a perfect Doppler effect impossible to obtain otherwise.
' It is still possible to obtain the regular Doppler effect (no
' frequency change) by modifying Woldemar Voigt's 1887 equations.
'*********************************************************************


Sub Emitter_Single()
  g_Lorentz_x = Sqr(1 - beta_x^2)
  g_Lorentz_y = Sqr(1 - beta_y^2)
  beta_xy = Sqr(beta_x^2 + (g_Lorentz_x * beta_y)^2)
  g_Lorentz_xy = Sqr(1 - beta_xy^2)
  If iteration = start_B + 1 Then                                     'emitter B.
    t_prime_xy = g_Lorentz_xy * t_time                                'simplification for x = y = 0.
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi                                           'the goal of synchro is to start emitting when Cos(phase) = 1 exactly.
  Elseif iteration = start_A + 1 Then                                 'emitter A.
    t_prime_xy = g_Lorentz_xy * t_time
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi
  Elseif iteration = start_D + 1 Then                                 'emitters D and E.
    t_prime_xy = g_Lorentz_xy * t_time
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi
  Elseif iteration = start_C + 1 Then                                 'emitter C.
    t_prime_xy = g_Lorentz_xy * t_time
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi
  End If
  
' The formulas below represent a major breakthrough. They enable one
' to perform an unlimited number of all azimuth transformations ON
' THE SAME SCREEN. The variables x, x' both stand for real absolute
' distances. What's more, t, t' stand for the real wave period. Not  
' the time, which may however be established according to it. Thus,
' there is no space-time transformation.

'  g_Lorentz_x = Sqr(1 - beta_x^2)                                    'Lorentz's regular contraction factor for the "transformation axis" x.
'  g_Lorentz_y = Sqr(1 - beta_y^2)                                    'Lorentz's regular contraction factor for the transverse axis y.
'  beta_xy = Sqr(beta_x^2 + (g_Lorentz_x * beta_y)^2)                 'absolute speed according to Pythagoras (the transverse speed is finally slower).
'  g_Lorentz_xy = Sqr(1 - beta_xy^2)                                  'absolute final contraction.
'  t_prime_x  = g_Lorentz_x * t_time    - beta_x * x                  'wave period along the x axis.
'  x_prime    = g_Lorentz_x * x         + beta_x * t_time             'absolute x' coordinate.
'  y_prime    = g_Lorentz_y * y         + beta_y * t_prime_x          'absolute y' coordinate (the transverse distance is finally shorter).
'  t_prime_xy = g_Lorentz_y * t_prime_x - beta_y * y                  'absolute final wave period for the (x', y') coordinate.

  For x = -.25 * lambda To .25 * lambda                               'the emitting area radius is lambda / 4 in 2-D and 3-D.
    x_prime   = g_Lorentz_x * x      + beta_x * t_time                'the emitting area simultaneously contracts (g * x) and moves (beta * t) according to Lorentz.
    t_prime_x = g_Lorentz_x * t_time - beta_x * x                     'a slower wave period (g * t) and a phase shift (-beta * x) simultaneously occur according to Lorentz.
    x_coord = x_A + x_prime                                           'final absolute x' coordinate in pixel units.
    For y = -.25 * lambda To .25 * lambda
      If pulse(x, y) Then
        y_prime = g_Lorentz_y * y + beta_y * t_prime_x                'brand new tridimensional y' equation. z' = g[z] * z + beta[z] * t[xy]' should work as well.
        y_coord = y_center + y_prime                                  'final absolute y' coordinate in pixel units.
        t_prime_xy = g_Lorentz_y * t_prime_x - beta_y * y             'brand new tridimensional t'[xy] equation, to be converted into radians.
        t_radian = t_prime_xy * 2 * pi / lambda                       'the contracted emitting area also exhibits Louis de Broglie's "phase wave" because of the time shift.
        trend(x_coord, y_coord) = trend(x_coord, y_coord) + Cos(t_radian - synchro) * pulse(x, y)'finally performing the required impulse for each (x', y') "aether granule".
        If y = 0 Then                                                 'displaying the impulse curve (may be omitted).
          curve = 40 * Cos(t_radian - synchro) * pulse(x, y)
          If curve > 0 Then
               Line(x_coord, y_height + 45 - curve)-(x_coord, y_height + 45), green 
          Else Line(x_coord, y_height + 45 - curve)-(x_coord, y_height + 45), red
          End If
        End If
      End If
    Next
  Next
End Sub


'*********************************************************************
' MOVING THE FRAME OF REFERENCE LEFTWARD (in order to scan forward).
'*********************************************************************

Sub Frame_of_Reference()
  x_A_Int -= 1                                                        'one pixel steps, hence (x_A = x_A_Int - move_frame) is more accurate.
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
  Screen 21,24,3
  Windowtitle " The Relativistic Big Bang - Observers Apparently Equidistant"
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Line(0,0)-(x_width, y_height), black, bf
' Line(0,0)-(100,15), blue, bf: Locate 2: Print Point(1,1): Print blue: Sleep: End 'color test.

' Precalculus for the Lorentz Transformations.************************
  beta_A = 0                                                          'A is stationary.
  g_Lorentz_A = 1                                                     'no contraction.
  beta_C = .5                                                         'arbitrary normalized speed for B: 0.5 exactly (beta = v/c).
  g_Lorentz_C = Sqr(1 - beta_C^2)                                     'Lorentz's contraction factor: g = 0.866025403
  alpha = (1 - g_Lorentz_C) / beta_C                                  'alpha = 0.267949192 (in order to avoid transforming space!).
  g_alpha = Sqr(1 - alpha^2)                                          'Lorentz's contraction factor for alpha speed: g = 0.963433.
  beta_B = alpha                                                      'observer B is moving at the alpha intermediate speed.
  g_Lorentz_B = g_alpha                                               'his speed being reversed by the scanner, his contraction remains the same.
  beta_D = (alpha + beta_C) / (1 + alpha * beta_C )                   'according to Poincaré's law of relative speed addition: beta for D: 0.677
  g_Lorentz_D = Sqr(1 - beta_D^2)                                     'Lorentz's contraction factor for D: 0.7357815
  beta_E = (alpha + beta_D) / (1 + alpha * beta_D)                    '                        beta for E: 0.8 exactly.
  g_Lorentz_E = Sqr(1 - beta_E^2)                                     'Lorentz's contraction factor for E: 0.6 exactly.
  beta_F = (alpha + beta_E) / (1 + alpha * beta_E)                    '                        beta for F: 0.879
  g_Lorentz_F = Sqr(1 - beta_F^2)                                     'Lorentz's contraction factor for F: 0.476
'*********************************************************************
  iteration = 0
  iteration = 1000'***************************************************'normally 0.
  start_B = 1250
  start_A = 1586
  start_D = 1708
  start_C = 1826
  brightness = 10
  soft = .97                                                          'very faint reflection, yet on a large area.
  x_text = 342
  x_scale = -240
  c_speed = 1 / ((1 / beta_C) - alpha)                                '.577 pixel per loop in order to mach the scan speed (1 / beta) minus the alpha speed.
  wave_speed = .65                                                    'for algorithm (non linear, inverted: maximum 0, minimum 1.99), adjusted according to c_speed.
  If scanner Then'                                                                                      For example, .5 produces faster waves AND longer wavelength.
    x_alpha = .5 * x_width - 100
    skipped_frames = 0
    Else x_alpha = .5 * x_width
  End If
  x_origin = x_alpha - (1 - 2 * alpha) * 10 * lambda + 1
  x_scanner = x_alpha - 626
  y_center = .5 * y_height - 1
  y_clock = 86
  x_A_Int = x_alpha                                                   'for accuracy: x_A = x_A_Int - move_frame.
  x_A = x_A_Int
  damping_zone = 30 * Sqr(lambda)                                     '100 for lambda = 25 to 200 for lambda = 100.
  x_start = -damping_zone + x_text
  y_start = -damping_zone
  x_stop = x_width  + damping_zone
  y_stop = y_height + damping_zone
  If bitmap Then skipped_frames = 0

  For x = -.25 * lambda To .25 * lambda                               'precalculus for pulsating source with Doppler effect.
    x_squared = x^2                                                   'axial distance squared in pixel units.
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)                                 'distance in pixel units according to Pythagoras.
      If distance <= .25 * lambda Then
        pulse(x, y) = Cos(2 * pi * distance / lambda)                 'regular 2-D sinusoidal distribution.
        distance_xy(x, y) = distance
      End If
    Next
  Next

  Circle(x_A, y_center), radius, blue,,,1                             'central galaxy (blue matrix).
  Paint( x_A, y_center), blue, blue
  Circle(x_A, y_center), radius - 3, dark,,,1                    
  Paint( x_A, y_center), dark, dark
  For x = -radius To radius
    For y = -radius To radius
      If Point(x_A + x, y_center + y) = blue Then x_y(x, y) = 1 Else x_y(x, y) = 0'selecting blue pixels only.
    Next
  Next
  Circle(x_A, y_center), radius, black,,,1                            'erasing blue matrix.
  Paint( x_A, y_center), black, black
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

  Color dark_gray
  Locate 63, 2:  ? "Thanks to the creators of FreeBASIC."
  Locate 64, 2:  ? "Gabriel LaFreniere  glafreniere.com";
  Locate 64,101: ? "This program may be freely distributed, copied or modified.";
  Color green_text
  line58 = " A- Observer A preferred - Don't scan."
  line59 = " B- Observer B preferred - Scan.      "
  line60 = " W- Add Waves (very slow).            "
  line61 = " N- No Waves (faster).                "
  line62 = " P- Pause.                            "
  line63 = " I- Initialize.                       "
  line64 = " Press Esc. to Quit.                  "
  Locate 56, 50: ? "Please click slowly!"
  Locate 58, 46: ? line58
  Locate 59, 46: ? line59
  Locate 60, 46: ? line60
  Locate 61, 46: ? line61
  Locate 62, 46: ? line62
  Locate 63, 46: ? line63
  Locate 64, 46: ? line64;
  Color black, background
  Locate 57, 2: ? "Brightness: Press +/- (press = to reset)."
  If scanner Then
    Locate 59, 46: ? line59
  Else
    Locate 58, 46: ? line58
    Locate 59, 2:  ? "Skip Frames: Press a number from 0 to 9."
  End If
  If wave_display Then
    Locate 60, 46: ? line60
  Else
    Locate 61, 46: ? line61 
  End If
  Color white, black
  Locate 42,148: ? "June 7, 2010"
  Locate 43,145: ? "glafreniere.com"
  Locate 44,142: ? "Gabriel LaFreniere";
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
End Sub


'*********************************************************************
' KEYBOARD MANAGEMENT.
'                        **** IMPORTANT ****
' Most of the keybord commands are redirected to Mouse_Management Sub
' in order to simplify procedures and avoid occasional discrepancies.
'*********************************************************************

Sub Keyboard_Management()
  If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = Ucase(in_key)
' Screenset work_page, work_page
' cls: locate 10, 10: print in_key: sleep 1000                        'check Inkey value such as arrows, page up, etc.
  Select Case in_key
  Case Chr(27), "k+": End                                             'end program - escape key or Windows' X button.
  Case "A": line_number = 58: click = 1                               'don't scan.
  Case "B": line_number = 59: click = 1                               'don't scan.
  Case "W": line_number = 60: click = 1                               'add waves.
  Case "N": line_number = 61: click = 1                               'no waves.
  Case "I": Initialization()
  Case "P": line_number = 62: click = 1                               'pause.
  Case "+": brightness = brightness / Sqr(.5)                         'brighter.
            If brightness > 160   Then brightness = 160
  Case "-": brightness = brightness * Sqr(.5)                         'darker.
            If brightness < 10 Then brightness = 10
  Case "=": brightness = 20                                           'normal brightness.
  Case "0": skipped_frames = 0
  Case "1","2","3","4","5","6","7","8","9"                            'skip up to 9 frames.
            If scanner = 0 Then skipped_frames = 10 * Val(in_key) - 1
  End Select
  in_key = ""
  Do: Loop While Len(Inkey)                                           'clear buffer.
End Sub


'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

Sub Mouse_Management()
  Color green_text, white
  Locate line_number, 46

  Select Case line_number
    Case 58                                                           'don't scan.
      If scanner = 1 Then ? line58
      If click > 0 Then
        scanner = 0
        Initialization()
      End If
    Case 59                                                           'scan.
      If scanner = 0 Then ? line59
      If click > 0 Then
        scanner = 1
        Initialization()
      End If
    Case 60                                                           'wave display.
      If scanner = 1 Then ? line60
      If click > 0 Then
        wave_display = 1
        Color blue: Screenset matrix_page, visible_page
        Screenset matrix_page, visible_page
        Color black, background:      Locate 60, 46: Print line60
        Color green_text, background: Locate 61, 46: Print line61
      End If
    Case 61                                                           'no wave display.
      If scanner = 0 Then ? line61
      If click > 0 Then
        wave_display = 0
        Screenset matrix_page, visible_page
        Color black, background:      Locate 61, 46: Print line61
        Color green_text, background: Locate 60, 46: Print line60
      End If
    Case 62                                                           'Pause.
      ? line62
      If click > 0 Then
        Screenset work_page, work_page: Color red, background
        Locate 62, 46: ? " Paused. - Press Esc. to quit. - Press any other key to resume."
        Sleep: If(Inkey) = Chr(27) Then End Else in_key = ""
      End If
    Case 63                                                           'initialization (line 23 or 47).
      ? line63
      If click > 0 Then 
        Initialization()
      End If
    Case 64                                                           'ending program.
      ? line64;
      If click > 0 Then End
  End Select
  
  If click = 1 Then                                                   'avoid repetitive actions.
    Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
  End If
End Sub


'*********************************************************************
' PRINTING THE TRANSFORMED GALAXIES.
'*********************************************************************

Sub Print_Galaxy_Single()
  g_Lorentz_x = Sqr(1 - beta_x^2)
  g_Lorentz_y = Sqr(1 - beta_y^2)
  t_prime_x = g_Lorentz_x * t_time - beta_x * x
  x_prime   = g_Lorentz_x * x + beta_x * t_time
  y_prime   = g_Lorentz_y * y + beta_y * t_prime_x                    'brand new all-azimuth y' equation, amazingly efficient here.
  Pset(x_A + x_prime, y_center + y_prime), gray
End Sub

