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
Declare Sub Emitter_A()
Declare Sub Emitter_B()
Declare Sub Frame_of_Reference()
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()

Const pi = 4 * Atn(1)
Const black = 0, white = -1, purple = -65281, gray = -6908266, yellow = -256, blue = -10184961'-16751361
Const red = -65536, green = -16726016, cyan = -16725816, dark = 1, bright = -257
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), dark_gray = Rgb(75,75,75)

Dim Shared As Integer x_screen = 1280, y_screen = 1024, x_width = 1279, y_height = 767
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_alpha, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer r, g, b, j, x, y, scanner, x_scanner, x_previous, x_squared
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_A, x_B, x_C, x_D, x_E, y_A, y_B, y_C, y_D, y_E, x_origin, y_clock, x_emitter, y_emitter
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display, lambda, radius = 30
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap, target
Dim Shared As Integer iteration, pulse_A, pulse_B, damping_zone = 401, x_y(-radius To radius, -radius To radius)
Dim Shared As Integer frame, skipped_frames, line_number, axial, unit, scan(-4 To x_width + 4, y_height)

Dim Shared As Single g_Lorentz_B, g_Lorentz_C, g_Lorentz_F, g_Lorentz_G, g_Lorentz_X, g_Lorentz_Y, g_Lorentz_Z
Dim Shared As Single x_coord, y_coord, x_prime, y_prime, t_time, t_prime, t_Lorentz, t_start
Dim Shared As Single g_alpha, beta_B, beta_C, beta_F, beta_G, beta_X, beta_Y, beta_Z, g_Lorentz
Dim Shared As Single k_Dewavrin, move_frame, frame_speed, c_speed, alpha, beta, beta_prime
Dim Shared As Single orthogonal, diagonal, influence, potential, previous, ratio, factor, sagitta
Dim Shared As Single amplitude, phase, distance, radian, brightness, decimal, curve, soft, half_chord
Dim Shared As Single ellipse_radius, circle_radius, sphere_radius, twins_distance, wave_speed
Dim Shared As Single energy(x_screen, y_screen), quadrature(x_screen, y_screen)
Dim Shared As Single damping(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single past(   -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single present(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single trend(  -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)

Dim Shared As String line58, line59, line60, line61, line62, line63, line64
Dim Shared As String in_key, file, bitmap_number
visible_page = 0: work_page = 1: matrix_page = 2
wave_display = 0: skipped_frames = 20: scanner = 0: bitmap = 0         'set bitmap = 1 for BMP image sequence.
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
    t_Lorentz = iteration * c_speed                                   'Lorentz's absolute t time based on current iteration.
    t_time = t_Lorentz / lambda                                       'absolute t time in wave period units.
    sphere_radius = t_Lorentz
    twins_distance = beta_B * t_Lorentz
    x_B = x_A - move_frame + beta_B * t_Lorentz                       'B is moving rightward (beta = 0.8 c).
    x_C = x_A - move_frame + beta_C * t_Lorentz                       'C is moving rightward (beta = 0.8 c).
    If scanner Then move_frame += c_speed * alpha
    If move_frame > 1 Then move_frame -= 1: Frame_of_Reference()
    Damping_Management()                                              'processing damping zone.
    Elliptic_Reflector()
    iteration += 1
    If scanner Then
      x_scanner += 1
      If x_scanner = x_width + 2 Then Sleep: If Inkey = Chr(27) Then End
      If x_scanner > x_width + 2 Then x_scanner = x_width + 3
    End If
    If iteration > 380 Then wave_display = 1: pulse_B = 1: Emitter_B()
    If iteration = 1702 Then Sleep: If Inkey = Chr(27) Then End
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
  If bitmap > 0  And iteration Mod 3 = 0 Then                         'set bitmap = 1 for bitmap sequence.
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
  t_prime = g_Lorentz_B * t_Lorentz                                   'simplified (x = 0).
  y_D = y_A - beta_B * t_prime
  y_E = y_A + beta_B * t_prime
  beta_X = .5 * (.5 * beta_C + beta_B)                                'the ellipse contracts according to .45 c: (.4 + .5) / 2.
  g_Lorentz_X = Sqr(1 - beta_X ^ 2)                                   'mean contraction for expanding (A to C) and moving ellipsoidal mirror. 
' x_coord = .5 * (x_A + x_C)                                          'ellipse.
' Circle(x_coord,y_A),.5*(x_C-x_A)/g_Lorentz_X,white,,, 1/g_Lorentz_X 'expanding ellipsoid mirror.
  For radian = 0 To 2 * pi Step pi / sphere_radius                    'expanding ellipsoid mirror - see "Elliptic_Reflector()".
      x_coord = beta_B * sphere_radius * Cos(radian)                  'axial coordinate before transformation (proportional to speed).
      y_coord = beta_B * sphere_radius * Sin(radian)                  'transverse coordinate before transformation.
      beta = x_coord / t_Lorentz                                      'speed of the current spot before acceleration (leftward if beta is negative).
      g_Lorentz = Sqr(1 - beta ^ 2)                                   'Lorentz contraction factor for the current spot.
      y_coord = y_coord / g_Lorentz                                   'transverse dilation in order to balance the final contraction below.
      beta_X = (beta + beta_B) / (1 + beta * beta_B)                  'accelerating the current spot according to beta_B.
      g_Lorentz_X = Sqr(1 - beta_X ^ 2)                               'Lorentz contraction factor for the current spot.
      x_coord = beta_X * sphere_radius                                'final coordinate for the current spot.
      beta = x_coord / t_Lorentz                                      'final speed of the current spot.
      g_Lorentz = Sqr(1 - beta ^ 2)                                   'Lorentz contraction factor for the current spot.
      y_coord = g_Lorentz * y_coord                                   'transverse contraction according to my new three dimensional equations.
    Circle(x_A + x_coord, y_center + y_coord), 1, white,,,1
    Paint (x_A + x_coord, y_center + y_coord), white, white
  Next
  beta = .5: x = 0: y = 0
  g_Lorentz = Sqr(1 - beta ^ 2)
  x_prime = g_Lorentz * x + beta * t_Lorentz
  t_prime = g_Lorentz * t_Lorentz - beta * x
  y_prime = g_Lorentz_B * y + beta_B * t_prime
  Circle(x_C, y_center), 24, 1,,,1
  Paint(x_C, y_center), black, 1
  Circle(x_B, y_center - y_prime), 24, 2,,,1
  Paint(x_B, y_center - y_prime), black, 2
  Circle(x_B, y_center + y_prime), 24, 3,,,1
  Paint(x_B, y_center + y_prime), black, 3
  Circle(x_A, y_center + y_prime), 24, 4,,,1
  Paint(x_A, y_center + y_prime), black, 4
  Circle(x_A, y_A), sphere_radius, gray,,, beta_B                     'horizontal .5 c position (ellipses).
  Circle(x_A, y_A), sphere_radius, gray,,, 1 / beta_B                 '  vertical .5 c position.
  Circle(x_A, y_A), sphere_radius, gray,,, beta_C                     'horizontal .8 c position.
  Circle(x_A, y_A), sphere_radius, gray,,, 1 / beta_C                 '  vertical .8 c position.
  Circle(x_A, y_A), sphere_radius, gray,,, beta_F                     'horizontal .92857 c position.
  Circle(x_A, y_A), sphere_radius, gray,,, 1 / beta_F                 '  vertical .92857 c position.
  Line(x_A - sphere_radius, y_center)-(x_A + sphere_radius, y_center),gray 'horizontal x axis.
  Line(x_A, y_center - sphere_radius)-(x_A, y_center + sphere_radius),gray 'vertical y axis.
  Line(x_B, y_center - y_prime)-(x_B, y_center + y_prime), white      'vertical axis for B.
  Color white, dark                                                   'dark is opaque to waves.

'*********************************************************************
' APPLYING THE LORENTZ TRANSFORMATIONS.
'*********************************************************************


  For x = -radius To radius                                           'x and y coordinates are given in pixel units.
    For y = -radius To radius                                         'scanning x and y, one pixel at a time.
      If x_y(x, y) Then                                               'copying the matrix blue color only when x_y(x, y) code = 1.

'*********************************************************************
' The y' formula below represents a major breakthrough. It enables
' one to perform an unlimited number of all azimut transformations.
' This means that the x axis is NOT necessarily the displacement
' axis. It is rather the TRANSFORMATION AXIS, where a local t' time
' takes place whatever the y or z (2-D or 3-D) coordinates may be.
''********************************************************************


        beta = 0
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_B * y + beta_B * t_prime
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        
        beta = .45                                                    'adjust x (according to beta) on the required ellipse.
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_B * y + beta_B * t_prime
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
        beta = .75
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_B * y + beta_B * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
        beta = .91
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_B * y + beta_B * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue

        beta = 0
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_C * y + beta_C * t_prime
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        
        beta = .33
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_C * y + beta_C * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
        beta = .625
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_C * y + beta_C * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
        beta = .83
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_C * y + beta_C * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue

        beta = 0
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_F * y + beta_F * t_prime
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        
        beta = .2
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_F * y + beta_F * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
        beta = .44
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_F * y + beta_F * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
        beta = .68
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_F * y + beta_F * t_prime
        Pset(x_A + x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A - x_prime, y_center - y_prime), blue
        
'       Pset(x_A + x, y_center + y), white                            'center A, no transformation.
        
        beta = beta_B
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = 1 * y + 0 * t_prime                                 'horizontal x axis. y' = y according to Lorentz.
        Pset(x_A - x_prime, y_center + y_prime), blue
        If iteration < 380 Then                                       'visible only before B starts emitting.
          Pset(x_A + x_prime, y_center - y_prime), white
        End If

        beta = beta_F
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = 1 * y + 0 * t_prime                                 'horizontal x axis. y' = y according to Lorentz.
        Pset(x_A + x_prime, y_center - y_prime), blue
        Pset(x_A - x_prime, y_center + y_prime), blue
        
        beta = .5                                                     'D and E are on the same plane as B (beta = .5).
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = g_Lorentz_B * y + beta_B * t_prime
        Pset(x_A + x_prime, y_center + y_prime), white
        Pset(x_A + x_prime, y_center - y_prime), white

        beta = beta_C
        g_Lorentz = Sqr(1 - beta ^ 2)
        x_prime = g_Lorentz * x + beta * t_Lorentz
        t_prime = g_Lorentz * t_Lorentz - beta * x
        y_prime = 1 * y + 0 * t_prime                                 'horizontal x axis. y' = y according to Lorentz.
        Pset(x_A - x_prime, y_center + y_prime), blue
        Pset(x_A + x_prime, y_center - y_prime), white                'white galaxy C moving forward 0.8 c.
      End If
    Next
  Next
  
  beta = .5: x = 0: y = 0
  g_Lorentz = Sqr(1 - beta ^ 2)
  x_prime = g_Lorentz * x + beta * t_Lorentz
  t_prime = g_Lorentz * t_Lorentz - beta * x
  y_prime = g_Lorentz_B * y + beta_B * t_prime
  Locate 1, 1:  ? "D"
  Circle(x_B, y_center - y_prime), 8, dark,,,1
  Paint(x_B, y_center - y_prime), dark, dark
  For x = 0 To 7: For y = 0 To 15                                     'galaxy moving upward (beta = 0.5 before transformation) identification D.
    If Point(x,y) = -1 Then Pset(x + x_B - 3, y + y_center - y_prime - 7), white
  Next:Next

  Locate 1, 1:  ? "E"
  Circle(x_B, y_center + y_prime), 8, dark,,,1
  Paint(x_B, y_center + y_prime), dark, dark
  For x = 0 To 7: For y = 0 To 15                                     'galaxy moving downward (beta = 0.5 before transformation) identification E.
    If Point(x,y) = -1 Then Pset(x + x_B - 3, y + y_center + y_prime - 7), white
  Next:Next

  Locate 1, 1:  ? "C"
  Circle(x_C, y_center), 8, dark,,,1
  Paint(x_C, y_center), dark, dark
  For x = 0 To 7: For y = 0 To 15                                     'galaxy moving forward (beta = 0.8) identification C.
    If Point(x,y) = -1 Then Pset(x + x_C - 3, y + y_center - 7), white
  Next:Next

  Locate 1, 1:  ? "B"
  Circle(x_B, y_center), 8, dark,,,1
  Paint(x_B, y_center), dark, dark
  For x = 0 To 7: For y = 0 To 15                                     'galaxy moving forward (beta = 0.5) identification B.
    If Point(x,y) = -1 Then Pset(x + x_B - 3, y + y_center - 7), white
  Next:Next

  Circle(x_A, y_center), radius, 12,,,1                               'central white galaxy A
  Paint( x_A, y_center), white, 12
  Circle(x_A, y_center), radius-7, 12,,,1
  Paint( x_A, y_center), 12, 12

  Locate 1, 1:  ? "A"                                                 'central galaxy identification A.
  For x = 0 To 7: For y = 0 To 15
    If Point(x,y) = -1 Then
      Pset(x + x_A - 3, y + y_center - 7), white
      Else Pset(x + x_A - 3, y + y_center - 7), dark
    End IF
  Next:Next
  Locate 1, 1:  ? " "

  Circle(x_A, y_center),     sphere_radius + 2, white,,,1             'cosmic sphere growing at the speed of light.
  Circle(x_A-1, y_center),   sphere_radius + 2, white,,,1
  Circle(x_A+1, y_center),   sphere_radius + 2, white,,,1
  Circle(x_A, y_center-1),   sphere_radius + 2, white,,,1
  Circle(x_A, y_center+1),   sphere_radius + 2, white,,,1
  Circle(x_A-1, y_center-1), sphere_radius + 2, white,,,1
  Circle(x_A+1, y_center+1), sphere_radius + 2, white,,,1
  Circle(x_A-1, y_center+1), sphere_radius + 2, white,,,1
  Circle(x_A+1, y_center-1), sphere_radius + 2, white,,,1
  Line(7, 170)-(210, 245), dark, BF
  Line(7, 170)-(210, 245), gray, B
  Color white, dark
  Locate 12, 03: ? "x'= g * x + b * t"
  Locate 13, 03: ? "t'= g * t - b * x"
  Locate 14, 03: ? "y'= g[y] * y + b[y] * t'"
  Locate 15, 03: ? "z'= g[z] * z + b[z] * t'"

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
  
  If scanner Then Line(x_scanner,0)-(x_scanner,y_height), white       'scanner line.
  Color black, background
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
    x_coord = beta_B * (sphere_radius+0) * Cos(radian)
    y_coord = beta_B * (sphere_radius+0) * Sin(radian)
    beta = x_coord / t_Lorentz
    g_Lorentz = Sqr(1 - beta ^ 2)
    y_coord = y_coord / g_Lorentz
    beta_X = (beta + beta_B) / (1 + beta * beta_B)
    g_Lorentz_X = Sqr(1 - beta_X ^ 2)
    x_coord = beta_X * sphere_radius
    beta = x_coord / t_Lorentz
    g_Lorentz = Sqr(1 - beta ^ 2)
    y_coord = g_Lorentz * y_coord
    x_coord = x_coord + x_A - move_frame                              'pixel coordinates.
    y_coord = y_coord + y_center
    For x = -5 To 5
      x_squared = x ^ 2
      For y = -5 To 5
        distance = Sqr(x_squared + y ^ 2)
        If distance < 5 Then trend(x + x_coord, y + y_coord) = soft * trend(x + x_coord, y + y_coord)
      Next
    Next
  Next
End Sub


'*********************************************************************
' UNMOVING WAVE GENERATOR A - NO DOPPLER EFFECT.
'*********************************************************************

Sub Emitter_A()                                                       'axial emitter B: beta = 0.5.
  If x_scanner - x_A > 7 * lambda Then Exit Sub 
  t_time = c_speed * iteration * 2 * pi / lambda                      'absolute time in radians.
  amplitude = (8 * lambda - (x_scanner - x_A)) / (8 * lambda)
  For x = -.25 * lambda To .25 * lambda
    x_squared = x^2                                                   'absolute axial distance squared in pixel units.
    potential = amplitude * Sin(t_time)
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)                                 'absolute distance in pixel units.
      If distance < .25 * lambda Then
        curve = potential * Cos(2 * pi * distance / lambda)
        trend(x_A + x, y_A + y) = trend(x_A + x, y_A + y) + curve
        If Int(y) = 0 Then                                            'displaying the impulse curve.
          If curve > 0 Then
               Line(x_A + x, y + y_height - 70 - 80 * curve)-(x_A + x, y_height - 70), green 
          Else Line(x_A + x, y + y_height - 70 - 80 * curve)-(x_A + x, y_height - 70), red
          End If
        End If
      End If
    Next
  Next

  Line(x_A - lambda / 2, y_height - 70)-(x_A + lambda / 2, y_height - 70), white 
  Line(x_A, y_height - 72)-(x_A, y_height - 68), white 
End Sub


'*********************************************************************
' MOVING WAVE GENERATOR B - THE DOPPLER EFFECT IS OBTAINED
' USING MY REVERSED VERSION OF THE LORENTZ TRANSFORMATIONS.
'*********************************************************************

Sub Emitter_B()                                                       'axial emitter B: beta = 0.5.
  If pulse_B = 0 Then Exit Sub
  If t_start = 0 Then t_start = g_Lorentz_B * t_time - .75            'start when amplitude is maximum where x = 0.
  t_prime = g_Lorentz_B * t_time                                      'Lorentz's t' time for x = 0.
  If t_prime - t_start > 3.73 Then pulse_B = 0: Exit Sub              'adding about .73 phase offset in 2-D in order to cancel some residual energy.
' Locate 20,20: Print t_prime - t_start
  
  For x = -.25 * g_Lorentz_B * lambda To .25 * g_Lorentz_B * lambda
    x_squared = (x / g_Lorentz_B)^2                                   'absolute axial distance squared in pixel units.
    x_coord = x / g_Lorentz_B / lambda                                'Lorentz's x coordinate in wavelength units.

'*********************************************************************
' This is my reversed version of the Lorentz transformations.
    x_prime = g_Lorentz_B * x_coord + beta_B * t_time                 'Lorentz's x' coordinate in lambda units.
    t_prime = g_Lorentz_B * t_time  - beta_B * x_coord                'Lorentz's t' time in wave period units.
'*********************************************************************
    
    x_prime = x_A - move_frame + x_prime * lambda                     'Lorentz's x' coordinate in pixels for twin B, relative to twin A.
    t_prime = 2 * pi * (t_prime - t_start)                            'Lorentz's t' time in radians, starting at 0 in the center.
    potential = Sin(t_prime)
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)                                 'absolute distance in pixel units.
      If distance < .25 * lambda Then
        curve = potential * Cos(2 * pi * distance / lambda)
        trend(x_prime, y_center + y) = trend(x_prime, y_center + y) + curve
        If Int(y) = 0 Then                                            'displaying the impulse curve.
          If curve > 0 Then
               Line(x_prime, y + y_height + 75 - 70 * curve)-(x_prime, y_height + 75), green 
          Else Line(x_prime, y + y_height + 75 - 70 * curve)-(x_prime, y_height + 75), red
          End If
        End If
      End If
    Next
  Next
  Line(x_B - lambda / 2, y_height + 75)-(x_B + lambda / 2, y_height + 75), white 
  Line(x_B, y_height + 70)-(x_B, y_height + 80), white 
End Sub

'*********************************************************************
' MOVING THE FRAME OF REFERENCE LEFTWARD (in order to scan forward).
'*********************************************************************

Sub Frame_of_Reference()
  x_A -= 1
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
  Windowtitle " The Relativistic Big Bang and the Cosmic Sphere"
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Line(0,0)-(1279,767), black, bf
' Line(0,0)-(100,15), blue, bf: Locate 2: Print Point(1,1): Print blue: Sleep: End 'color test.

' Precalculus for the Lorentz Transformations.************************
  beta_B = .5                                                         'normalized speed for B (green): beta_B = v / c = 0.5 exactly.
  g_Lorentz_B = Sqr(1 - beta_B ^ 2)                                   'Lorentz's contraction factor: g = 0.866025403
  beta_C = (beta_B + beta_B ) / (1 + beta_B * beta_B )                'beta_C = 0.8 exactly (red).
  g_Lorentz_C = Sqr(1 - beta_C ^ 2)                                   'Lorentz's contraction factor: g = 0.6
  beta_F = (beta_B + beta_C) / (1 + beta_B * beta_C)                  'beta_F = 0.928571428 (purple) according to Poincaré's law of relative
  g_Lorentz_F = Sqr(1 - beta_F ^ 2)                                   '   speed addition: beta" = (beta + beta') / (1 + beta * beta')
  beta_G = 2 * beta_Y / (1 + beta_Y ^ 2)                              'simplified speed addition: beta' = (beta + beta) / (1 + beta * beta)
  g_Lorentz_G = Sqr(1 - beta_G ^ 2)
  alpha = (1 - g_Lorentz_B) / beta_B                                  'alpha = 0.267949192 (in order to avoid transforming space!).
  g_alpha = Sqr(1 - alpha ^ 2)                                        'Lorentz's contraction factor for alpha speed: g = 0.963433.
  beta_X = .5 * (.5 * beta_C + beta_B)                                'the ellipse moves at .4 c (.8 / 2) but contracts according to .45 c: (.4 + .5) / 2.
  g_Lorentz_X = Sqr(1 - beta_X ^ 2)                                   'mean contraction for expanding ellipsoidal mirror (A to C). 
  beta_Y = .625
  g_Lorentz_Y = Sqr(1 - beta_Y ^ 2)
  beta_Z = .68
  g_Lorentz_Z = Sqr(1 - beta_Z ^ 2)
'*********************************************************************

  If bitmap Then skipped_frames = 0
  pulse_A = 1
  pulse_B = 0
  iteration = 0
'  iteration = 600'*******
  t_start = 0
  lambda = 24
  brightness = 64
  soft = .97                                                          'very faint reflection, yet on a large area.
  x_scale = -240
  wave_speed = 2 / 3                                                  'wave speed for algorithm only (non linear and inverted, maximum 0, minimum 1.99)
  c_speed = 1 / ((1 / beta_B) - alpha)                                '.577 pixel per loop in order to mach the scan speed (1 / beta) minus the alpha speed.
  If scanner Then x_alpha = 320 Else x_alpha = .5 * x_width
  x_origin = x_alpha - (1 - 2 * alpha) * 10 * lambda + 1
  x_scanner = x_alpha - 650
  y_center = .5 * y_height - 1
  y_clock = 86
  x_A = x_alpha
  y_A = y_center
  y_B = y_center
  y_C = y_center
  damping_zone = 30 * Sqr(lambda)                                     '100 for lambda = 25 to 200 for lambda = 100.
  x_start = -damping_zone
  y_start = -damping_zone
  x_stop = x_width  + damping_zone
  y_stop = y_height + damping_zone

  Circle(x_A, y_center), radius, blue,,,1                             'central galaxy (matrix).
  Paint( x_A, y_center), blue, blue
  Circle(x_A, y_center), radius - 7, dark,,,1                    
  Paint( x_A, y_center), dark, dark
  For x = -radius To radius
    For y = -radius To radius
      If Point(x_A + x, y_center + y) = blue Then x_y(x, y) = 1 Else x_y(x, y) = 0'selecting blue pixels only.
    Next
  Next
  Circle(x_A, y_center), radius, dark,,,1                             'erasing blue matrix.
  Paint( x_A, y_center), black, dark
  Circle(x_A, y_center), radius, black,,,1


  For x = x_start To x_stop                                           'erasing previous data.
    For y = y_start To y_stop
      past(x,y) = 0
      present(x,y) = 0
      trend(x,y) = 0
    Next
  Next

'  Line(x_origin, 0)-(x_origin, y_center - 158), white                 'vertical wavelength scale.
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
    Locate 60, 2:  ? " * Do not skip frames while scanning! *"
  End If
  If wave_display Then
    Locate 60, 46: ? line60
  Else
    Locate 61, 46: ? line61 
  End If
  Color white, black
  Locate 02, 02: ? "The Relativistic Big Bang and the Cosmic Sphere."
  Locate 04, 02: ? "Normalized speed beta:"
  Locate 05, 02: ? "b = v / c"
  Locate 06, 02: ? "Lorentz's contraction factor:"
  Locate 07, 02: ? "g = Sqr(1 - beta ^ 2)"
  
  Locate 09, 02: ? "The Lorentz modified and now"
  Locate 10, 02: ? "tridimensional transformations."
  Locate 33, 02: ? "Observer A...... Stationary."
  Locate 34, 02: ? "  Contraction... None."
  Locate 36, 02: ? "Observer B...... b = 0.5"
  Locate 37, 02: ? "  Contraction... g = 0.866"
  Locate 39, 02: ? "Observer C...... b = 0.8"
  Locate 40, 02: ? "  Contraction... g = 0.6"
  Locate 42, 02: ? "Observer D, axial speed.... b[x] = 0.5"
  Locate 43, 02: ? "  Transverse speed......... b[y] = 0.5"
  Locate 44, 02: ? "  Transverse contraction... g[y] = 0.866"
  Locate 45, 02: ? "  Actual transverse speed.. b[y] = 0.433"
  Locate 46, 02: ? "  True speed, Pythagoras. b[x,y] = 0.66"
  Locate 47, 02: ? "  Diagonal contraction... g[x,y] = 0.75"
  Locate 45,148: ? "May 19, 2010"
  Locate 46,145: ? "glafreniere.com"
  Locate 47,142: ? "Gabriel LaFreniere";
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
  Case "S": If pulse_B Then pulse_B = 0 Else pulse_B = 1              'start/stop pulsating (B).
  Case "+": brightness = brightness / Sqr(.5)                         'brighter.
            If brightness > 256   Then brightness = 256
  Case "-": brightness = brightness * Sqr(.5)                         'darker.
            If brightness < 8 Then brightness = 8
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