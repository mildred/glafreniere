Width 80,20:Color 0,15:Cls:?
? " Created December 15, 2009 by Gabriel LaFreniere.":?:?
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
Declare Sub Display()
Declare Sub Elliptic_Reflector()
Declare Sub Emitter_Quad_A()
Declare Sub Emitter_Quad_B()
Declare Sub Emitter_Single()
Declare Sub Emitter_Management()
Declare Sub Frame_of_Reference()
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Print_Galaxy_Quad_A()
Declare Sub Print_Galaxy_Quad_B()
Declare Sub Print_Galaxy_Single()

Const pi = 4 * Atn(1)
Const black = 0, white = -1, purple = -65281, gray = -6908266, yellow = -256, blue = -10184961'-16751361
Const red = -65536, green = -16726016, cyan = -16725816, dark = 1, bright = -257
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), dark_gray = Rgb(75,75,75)

Dim Shared As Integer x_screen = 1280, y_screen = 1024, x_width = 1279, y_height = 767
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_alpha, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer r, g, b, j, x, y, scanner, x_scanner, x_previous, x_squared, colour
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_A_Int, x_B, x_C, x_D, x_E, y_D, y_E, x_origin, y_clock, x_emitter, y_emitter
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display, radius = 50, lambda = 24
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
Dim Shared As Single synchro, synchro_A, synchro_B, synchro_C, synchro_D, synchro_E
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
wave_display = 0: skipped_frames = 0: scanner = 0: bitmap = 0         'set bitmap = 1 for BMP image sequence.
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
    If iteration = 1948 Then Sleep: If Inkey = Chr(27) Then End
    If iteration > 2000 Then Sleep: End
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
  Locate 30, 09: ? x_B - x_A                                          'distance in pixels.
  Locate 31, 09: ? x_C - x_B
  Locate 32, 09: ? x_C - x_A
  Locate 33, 09: ? y_center - y_D
  Locate 34, 09: ? y_E - y_center

'  If scanner = 0 Then                                                'the scanner sometimes misses thin vertical lines.
    Circle(x_A, y_center), sphere_radius, gray,,, beta_B              'horizontal .5 c position (ellipses).
'    Circle(x_A, y_center), sphere_radius, gray,,, beta_C              'horizontal .5 c position (ellipses).
'    Circle(x_A, y_center), sphere_radius, gray,,, beta_D              'horizontal .8 c position.
'    Circle(x_A, y_center), sphere_radius, gray,,, beta_E              'horizontal .92857 c position.
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
'    x_coord = beta_E * sphere_radius
'    y_coord = g_Lorentz_E * sphere_radius
'    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray
'    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
'    x_coord = beta_F * sphere_radius
'    y_coord = g_Lorentz_F * sphere_radius
'    Line(x_A + x_coord, y_center - y_coord)-(x_A + x_coord, y_center + y_coord), gray
'    Line(x_A - x_coord, y_center - y_coord)-(x_A - x_coord, y_center + y_coord), gray
'  End If

  beta = alpha * g_alpha                                              'transverse speed of D and E, a bit slower than that of B whose speed is alpha.
  g_Lorentz = Sqr(1 - beta ^ 2)                                       'mean contraction for expanding (A to C) and moving ellipse or ellipsoidal mirror. 
  x_coord = .5 * (x_A + x_C)                                          'center of the expanding ellipse or mirror.
  Circle(x_coord,y_center),.5*(x_C-x_A)/g_Lorentz,white,,, 1/g_Lorentz 'full expanding ellipse.
'  For radian = 0 To 2 * pi Step pi / sphere_radius                    'expanding ellipsoid mirror - see "Elliptic_Reflector()".
'    If radian < .05 Then                                              'small portions of the ellipsoidal mirror: .05 times 2 * pi each.
''      code = 1
'    Elseif radian > 1 * .125 * 2 * pi - .05 And radian < 1 * .125 * 2 * pi + .05 Then code = 1
''    Elseif radian > 2 * .125 * 2 * pi - .05 And radian < 2 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 3 * .125 * 2 * pi - .05 And radian < 3 * .125 * 2 * pi + .05 Then code = 1
''    Elseif radian > 4 * .125 * 2 * pi - .05 And radian < 4 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 5 * .125 * 2 * pi - .05 And radian < 5 * .125 * 2 * pi + .05 Then code = 1
''    Elseif radian > 6 * .125 * 2 * pi - .05 And radian < 6 * .125 * 2 * pi + .05 Then code = 1
'    Elseif radian > 7 * .125 * 2 * pi - .05 And radian < 7 * .125 * 2 * pi + .05 Then code = 1
''    Elseif radian > 8 * .125 * 2 * pi - .05                                     Then code = 1
'    Else: code = 0
'    End If
'    If code Then
'      x_coord = beta_B * sphere_radius * Cos(radian)                  'axial coordinate before transformation (proportional to speed).
'      y_coord = beta_B * sphere_radius * Sin(radian)                  'transverse coordinate before transformation.
'      beta = x_coord / t_time                                         'speed of the current spot before acceleration (leftward if beta is negative).
'      g_Lorentz = Sqr(1 - beta^2)                                     'Lorentz contraction factor for the current spot.
'      y_coord = y_coord / g_Lorentz                                   'transverse dilation in order to balance the final contraction below.
'      beta = (beta + beta_B) / (1 + beta * beta_B)                    'accelerating the current spot according to beta_1.
'      x_coord = beta * sphere_radius                                  'final coordinate for the current spot.
'      beta = x_coord / t_time                                         'final speed of the current spot.
'      g_Lorentz = Sqr(1 - beta^2)                                     'Lorentz contraction factor for the current spot.
'      y_coord = g_Lorentz * y_coord                                   'transverse contraction according to my new three dimensional Lorentzian equations.
'      Circle(x_A + x_coord, y_center + y_coord), 1, white,,,1
'      Paint (x_A + x_coord, y_center + y_coord), white, white
'    End If
'  Next


  Line(x_A - sphere_radius, y_center)-(x_A + sphere_radius, y_center), gray  'gray horizontal x axis.
  Line(x_A, y_center - sphere_radius)-(x_A, y_center + sphere_radius), gray  'gray vertical axis.
  Line(x_A, y_center)-(x_C, y_center), white                          'white horizontal axis (A to C).
  Line(x_B, y_D)-(x_B, y_E), white                                    'white vertical axis (D to E).
  
  Color white, dark                                                   'dark is opaque to waves.

  For x = -radius To radius                                           'printing the galaxies.
    For y = -radius To radius                                         'scanning x and y, one pixel at a time.
      If x_y(x, y) Then                                               'copying the matrix color only when x_y(x, y) code = 1.
'        beta = beta_C
'        Print_Galaxy_Quad_A()
'        beta = beta_E
'        Print_Galaxy_Quad_A()
'        beta_x = beta_C
'        beta_y = beta_C * g_Lorentz_C                                 'the y coordinate is reduced according to the x' position.
'        Print_Galaxy_Quad_B()
'        beta_x = beta_C
'        beta_y = beta_E * g_Lorentz_C
'        Print_Galaxy_Quad_B()
'        beta_x = beta_E
'        beta_y = beta_C * g_Lorentz_E
'        Print_Galaxy_Quad_B()


        beta_x = beta_D
        beta_y = beta_F
        Print_Galaxy_Single()

        beta_x = Sqr(beta_F^2 +(g_Lorentz_F*beta_D)^2)
        beta_y = 0
        Print_Galaxy_Single()

        beta_x = 0
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = 0
        beta_y = beta_B
        Print_Galaxy_Single()
        
        beta_x = 0
        beta_y = beta_C
        Print_Galaxy_Single()
        
        beta_x = 0
        beta_y = -beta_B
        Print_Galaxy_Single()
        
        beta_x = 0
        beta_y = -beta_C
        Print_Galaxy_Single()
        
        beta_x = beta_B
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = beta_C
        beta_y = beta_C
        Print_Galaxy_Single()
        
        beta_x = -beta_B
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = beta_C
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = -beta_C
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = -beta_D
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = beta_E
        beta_y = 0
        Print_Galaxy_Single()
        
        beta_x = beta_B
        beta_y = beta_B
        Print_Galaxy_Single()
        
        beta_x = beta_B
        beta_y =-beta_B
        Print_Galaxy_Single()
        
        beta_x = beta_C
        beta_y = 0
        Print_Galaxy_Single()
      End If
    Next
  Next

'  Circle(x_A, y_center), radius, 12,,,1                              'central unmoving galaxy A.
'  Paint( x_A, y_center), white, 12
'  Circle(x_A, y_center), radius - 5, 12,,,1
'  Paint( x_A, y_center), 12, 12

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


'  Circle(x_A, y_center),     sphere_radius + 2, white,,,1             'cosmic sphere growing at the speed of light.
'  Circle(x_A-1, y_center),   sphere_radius + 2, white,,,1
'  Circle(x_A+1, y_center),   sphere_radius + 2, white,,,1
'  Circle(x_A, y_center-1),   sphere_radius + 2, white,,,1
'  Circle(x_A, y_center+1),   sphere_radius + 2, white,,,1
'  Circle(x_A-1, y_center-1), sphere_radius + 2, white,,,1
'  Circle(x_A+1, y_center+1), sphere_radius + 2, white,,,1
'  Circle(x_A-1, y_center+1), sphere_radius + 2, white,,,1
'  Circle(x_A+1, y_center-1), sphere_radius + 2, white,,,1

  Line(03, 169)-(332, 278), white, B
  Line(04, 170)-(331, 277), dark, BF
  Color white, dark

  Locate 12, 02: ? "x'= g[x] * x + ";Chr(225);"[x] * t "
  Locate 13, 02: ? "t[x]'= g[x] * t - ";Chr(225);"[x] * x "
  Locate 14, 02: ? "y'= g[y] * y + ";Chr(225);"[y] * t[x]'"
  Locate 15, 02: ? Chr(225);"[xy] = Sqr(";Chr(225);"[x]^2 + (g[x] * ";Chr(225);"[y])^2)"
  Locate 16, 02: ? "g[xy] = Sqr(1 - ";Chr(225);"[xy]^2)"
  Locate 17, 02: ? "t[xy]' = g[xy] * t - ";Chr(225);"[x] * x - ";Chr(225);"[y] * y"

'*********************************************************************
' DISPLAYING THE WAVES - POSITIVE AMPLITUDE: GREEN; NEGATIVE: RED.
'*********************************************************************

  For x = 0 To x_width: For y = 0 To y_height
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
  Locate 53, 02: Print "Iteration... "; iteration
  Locate 54, 02: Print "Scanner..... "; x_scanner

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

  If iteration < 1101 Then Exit Sub
  If iteration = 1101 Then wave_display = 1
  If iteration > 1100 And iteration < 1100 + lambda / g_Lorentz_B / c_speed Then '24 /.9634/.577: (1 * lambda / g / c).
    beta_x = beta_B
    beta_y = 0
    Emitter_Single()
  End If
  If iteration > 1401 And iteration < 1401 + lambda / g_Lorentz_A / c_speed Then
    beta_x = 0
    beta_y = 0
    Emitter_Single()
  End If
  If iteration > 1509 And iteration < 1509 + lambda / Sqr(1-(Sqr(alpha^2+(g_alpha*alpha)^2))^2) / c_speed Then
    beta_x = alpha
    beta_y = g_alpha * beta_x
    Emitter_Single()
    beta_x = alpha
    beta_y = -g_alpha * beta_x
    Emitter_Single()
  End If
  If iteration > 1615 And iteration < 1615 + lambda / g_Lorentz_C / c_speed Then
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
  If iteration = 1096 Then
    t_prime_xy = g_Lorentz_xy * t_time                                'simplification for x = y = 0.
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian
    Locate 29,100: Print t_radian; synchro
  Elseif iteration = 1402 Then
    t_prime_xy = g_Lorentz_xy * t_time
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi
  Elseif iteration = 1510 Then
    t_prime_xy = g_Lorentz_xy * t_time
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi
  Elseif iteration = 1616 Then
    t_prime_xy = g_Lorentz_xy * t_time
    t_radian = t_prime_xy * 2 * pi / lambda
    synchro = t_radian + pi
  End If
  
' The t' formula below represents a major breakthrough. It enables
' one to perform an unlimited number of all azimuths transformations.
    
  For x = -.25 * lambda To .25 * lambda                               'the emitting area radius is lambda / 4.
    x_prime = g_Lorentz_x * x + beta_x * t_time
    t_prime = g_Lorentz_x * t_time - beta_x * x
    x_coord = x_A + x_prime
    For y = -.25 * lambda To .25 * lambda
      If pulse(x, y) Then
        y_prime = g_Lorentz_y * y + beta_y * t_prime                  'new tridimensional y' equation: z' = g[z] * z + b[z] * t works as well.
        y_coord = y_center + y_prime                                  'y coordinate in pixel units.
        t_prime_xy = g_Lorentz_xy * t_time - beta_x * x - beta_y * y  'new tridimensional t'[xy] equation.
        t_radian = t_prime_xy * 2 * pi / lambda                       'conversion to radians.
        trend(x_coord, y_coord) = trend(x_coord, y_coord) + Cos(t_radian - synchro) * pulse(x, y)
        If y = 0 Then                                                 'displaying the impulse curve, may be omitted.
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
  Windowtitle " The Relativistic Big Bang - Louis de Broglie's Phase Wave"
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Line(0,0)-(1279,767), black, bf
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
  iteration = 650'****************************************************
  synchro_A = 0: synchro_B = 0: synchro_C = 0
  synchro_D = 0: synchro_E = 0
  brightness = 40
  soft = .97                                                          'very faint reflection, yet on a large area.
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
  x_start = -damping_zone
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

  Circle(x_A, y_center), radius, blue,,,1                             'central galaxy (matrix).
  Paint( x_A, y_center), blue, blue
'  Circle(x_A, y_center), radius - 5, dark,,,1                    
'  Paint( x_A, y_center), dark, dark
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

'  Line(x_origin, 0)-(x_origin, y_center - 158), white                'vertical wavelength scale.
'  For y = 0 To 12 * lambda Step lambda
'    Line(x_origin-4, y_center-y- 158)-(x_origin+4, y_center-y-158), white
'  Next

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
  Locate 02, 02: ? "The Relativistic Big Bang."
  Locate 03, 02: ? "Observers Apparently Equidistant"
  Locate 04, 02: ? "Normalized speed beta:"
  Locate 05, 02: ?  Chr(225);" = v / c"
  Locate 06, 02: ? "Lorentz's contraction factor:"
  Locate 07, 02: ? "g = Sqr(1 - ";Chr(225);"^2)"
  
  Locate 09, 02: ? "The Lorentz modified and now"
  Locate 10, 02: ? "all azimuths transformations:"

  Locate 21, 08: ? Chr(225); "        g"
  Locate 22, 02: ? "A";
  Print Using "###.#####"; beta_A;
  Print Using "###.#####"; g_Lorentz_A;
  Locate 23, 02: ? "B";
  Print Using "###.#####"; beta_B;
  Print Using "###.#####"; g_Lorentz_B;
  Locate 24, 02: ? "C";
  Print Using "###.#####"; beta_C;
  Print Using "###.#####"; g_Lorentz_C;
  beta = Sqr(alpha^2+(g_alpha*alpha)^2)
  g_Lorentz = Sqr(1 - beta^2)
  Locate 25, 02: ? "D";
  Print Using "###.#####"; beta;
  Print Using "###.#####"; g_Lorentz;
  Locate 26, 02: ? "E";
  Print Using "###.#####"; beta;
  Print Using "###.#####"; g_Lorentz;
  Locate 28, 02: ? "Distance (pixel):"
  Locate 30, 02: ? "A to B "
  Locate 31, 02: ? "B to C "
  Locate 32, 02: ? "A to C "
  Locate 33, 02: ? "B to D "
  Locate 34, 02: ? "B to E "
  
  Locate 36, 02: ? "The Galaxy A is truly stationary."
  Locate 37, 02: ? "Observer C is moving rightward"
  Locate 38, 02: ? "at half of the speed of light."
  Locate 39, 02: ? "Observer B is moving at the"
  Locate 40, 02: ? "intermediate speed, which is"
  Locate 41, 02: ? "given by: alpha = (1 - g) / ";Chr(225)
  Locate 42, 02: ? "This experiment proves that, in"
  Locate 43, 02: ? "a relativistic expanding world,"
  Locate 44, 02: ? "he is totally unable to detect"
  Locate 45, 02: ? "his true speed with respect to"
  Locate 46, 02: ? "the aether. All his attempts"
  Locate 47, 02: ? "lead to a null result."
  
  Locate 02, 118: ? "This is the de Broglie's phase wave. Its"
  Locate 03, 118: ? "speed is given by: 1 / beta, so that it is"
  Locate 04, 118: ? "always faster than the speed of light."
  
  Locate 06, 125: ? "It is the consequence of the t[xy]'"
  Locate 07, 125: ? "equation shown on the left. It is"
  Locate 08, 125: ? "useful in order to emit accurate"
  Locate 09, 125: ? "forward Doppler-contracted waves,"
  Locate 10, 125: ? "whose amplitude must be higher."
  
  Locate 34, 125: ? "There is a noticeable bias in the"
  Locate 35, 125: ? "lower right galaxy. It is easily"
  Locate 36, 125: ? "corrigeable by computing each"
  Locate 37, 125: ? "pixel separately. However, this"
  Locate 38, 125: ? "would imply that the galaxy is"
  Locate 39, 125: ? "also expanding at the same rate"
  Locate 40, 125: ? "as that of the universe. In this"
  Locate 41, 125: ? "case, the Time Scanner indicates"
  Locate 42, 125: ? "that the farthest galaxies should"
  Locate 43, 125: ? "also be smaller."

  Locate 45,148: ? "June 3, 2010"
  Locate 46,145: ? "glafreniere.com"
  Locate 47,142: ? "Gabriel LaFreniere";
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
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

Sub Print_Galaxy_Quad_A()                                             'four galaxies, orthogonal. Same distance, same speed.
  beta_x = 0
  beta_y = beta
  For j = 1 To 4
    Select Case j
      Case 2: beta_y = -beta
      Case 3: beta_y =  0: beta_x = beta
      Case 4: beta_x = -beta
    End Select
    g_Lorentz_x = Sqr(1 - beta_x^2)
    g_Lorentz_y = Sqr(1 - beta_y^2)
    x_prime = g_Lorentz_x * x + beta_x * t_time
    y_prime = g_Lorentz_y * y + beta_y * t_time
    Pset(x_A + x_prime, y_center + y_prime), gray
  Next
End Sub


Sub Print_Galaxy_Quad_B()                                             'four galaxies, diagonal. Same distance, same speed.
  For j = 1 To 4
    Select Case j
      Case 2: beta_y = -beta_y
      Case 3: beta_x = -beta_x
      Case 4: beta_y = -beta_y
    End Select
    g_Lorentz_x = Sqr(1 - Abs(beta_x)^2)
    g_Lorentz_y = Sqr(1 - Abs(beta_y)^2)
    x_prime = g_Lorentz_x * x + beta_x * t_time
    y_prime = g_Lorentz_y * y + beta_y * t_time
    Pset(x_A + x_prime, y_center + y_prime), gray
  Next
End Sub


Sub Print_Galaxy_Single()
  g_Lorentz_x = Sqr(1 - beta_x^2)
  g_Lorentz_y = Sqr(1 - beta_y^2)
  x_prime = g_Lorentz_x * x + beta_x * t_time
  t_prime = g_Lorentz_x * t_time - beta_x * x
  y_prime = g_Lorentz_y * y      + beta_y * t_prime                   'new tridimensional y' equation, z' = g[z] * z + b[z] * t' should work as well.
  Pset(x_A + x_prime, y_center + y_prime), white
  
' The test below was elaborated in order to check that the phase
' follows correctly the phase wave. Such a behavior allows the wave   'the de Broglie "phase wave" speed is given by: 1 / beta (always faster than the speed of light).
' generator to emit more energy forward than backward in accordance
' with my "active and reactive mass" calculus. The phase exhibits an
' irregular bias when the galaxy speed is about the same on both axes
' x and y. This is caused by the external parts of the emitting area,
' whose axes are not exactly aligned towards the center A. Such a
' problem would not occur if galaxies were also undergoing an
' expansion at the same rate as the Universe does.
' 1 - Set skipped_frames = 0
' 2 - Add galaxies moving diagonally in Sub Display (using this sub).
' 3 - The two lines below must be deactivated in Sub Initialization():
' Circle(x_A, y_center), radius - 5, dark,,,1                    
' Paint( x_A, y_center), dark, dark

  beta_xy = Sqr(beta_x^2 + (g_Lorentz_x * beta_y)^2)
  g_Lorentz_xy = Sqr(1 - beta_xy^2)
  If x = 35 and y = 35 Then
    Circle(x_A, y_center), Sqr(beta_F^2 +(g_Lorentz_F*beta_D)^2) * sphere_radius, white,,,1
    
  End If
  t_prime_xy = g_Lorentz_xy * t_time - beta_x * x - beta_y * y        'new tridimensional t' equation.
  t_radian = t_prime_xy * 2 * pi / lambda                             'conversion to radians.
  if Cos(t_radian) > 0 Then
    r = 255 * Cos(t_radian) * pulse(x/8, y/8)
    b = 128 * Cos(t_radian) * pulse(x/8, y/8)
    g = 0
  Else
    g = 255 * -Cos(t_radian) * pulse(x/8, y/8)
    b = 128 * -Cos(t_radian) * pulse(x/8, y/8)
    r = 0
  End If
  Pset(x_A + x_prime, y_center + y_prime), Rgb(r, g, b)
End Sub


