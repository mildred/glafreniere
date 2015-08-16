Width 80,20:Color 0,15:Cls:?
? " Created January 17, 2010 by Gabriel LaFreniere.":?:?
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
Declare Sub Frame_of_Reference()
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Wave_Emitter()
Declare Sub Wave_Emitter_Doppler()

Const pi = 4 * Atn(1)
Const black = 0, white = -1, purple = -65281, gray = -6908266, yellow = -256
Const red = -65536, blue = -16751361, green = -16726016, cyan = -16725816, dark = 1
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), dark_gray = Rgb(75,75,75)

Dim Shared As Integer x_screen = 620, y_screen = 310, x_width = 619, y_height = 309
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_first, x_last, x_alpha, x_center, y_center
Dim Shared As Integer r, g, b, x, y, scanner, x_scanner, x_previous, x_squared, x_mouse, y_mouse
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_twin_A, x_twin_B, x_twin_C, x_twin_B_prime, x_origin, y_clock, y_graph
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulses, pulse_A, pulse_B, pulse_C, lambda, threshold, damping_zone = 401
Dim Shared As Integer frame, skipped_frames, line_number, axial, OK, unit, scan(-4 To x_width + 8, y_height + 100)

Dim Shared As Single x_coord, y_coord, x_prime, y_prime
Dim Shared As Single alpha, beta, beta2, beta3, mirror_angle, ratio, factor, g_Lorentz, curve
Dim Shared As Single t_time, t_prime, k_Dewavrin, move_frame, frame_speed, c_speed, twins_distance
Dim Shared As Single orthogonal, diagonal, influence, potential, previous_potential, kinetic, Lagrangian, previous
Dim Shared As Single amplitude, phase, previous_phase, distance, radian, wave_speed, brightness, decimal
Dim Shared As Single energy(x_screen, y_screen), quadrature(x_screen, y_screen)
Dim Shared As Single damping(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single past(   -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single present(-damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)
Dim Shared As Single trend(  -damping_zone To x_screen + damping_zone, -damping_zone To y_screen + damping_zone)

Dim Shared As String line58, line59, line60, line61, line62, line63, line64
Dim Shared As String in_key, file, bitmap_number
visible_page = 0: work_page = 1: matrix_page = 2
skipped_frames = 0: scanner = 1: bitmap = 0                           'set bitmap = 1 for BMP image sequence.
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
'       trend(x,y) = .5 * orthogonal - past(x,y)                      'fastest trend extrapolation (constant pixel sum = 1).
'       trend(x,y) = .4 * (orthogonal + present(x,y)) - past(x,y)     'slower wave speed: .4 * (4 + 1) - 1 = 1
        trend(x,y) = .25 * (2 - wave_speed) * orthogonal + wave_speed * present(x,y) - past(x,y) 'scanner-compatible slower speed.
      Next: Next
    End If

    x_scanner += 1
    If x_scanner > x_width + 2 And scanner = 1 Then
     Sleep: If Inkey = Chr(27) Then End Else Initialization()
    End If
    move_frame += c_speed * alpha
    twins_distance += c_speed * beta
    If move_frame > 1 Then
      move_frame -= 1: Frame_of_Reference()
    Else OK = 0
    End If
    Damping_Management()                                              'processing damping zone.
    If wave_display Then Wave_Emitter(): Wave_Emitter_Doppler()       'sinusoidal impulses.
    iteration += 1
    If iteration = 300 Then pulse_C = 1
    If iteration = 2071 Then Exit For
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
    Sleep 1
  Next

  Display()                                                           'skip other frames.
  If y_mouse < 768 Or y_mouse > 1024 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And iteration Mod 10 = 0 Then
'  If bitmap > 0 And x_scanner > -15 And iteration Mod 10 = 0 Then
'  If bitmap > 0 Then                                                  'set bitmap = 1 for bitmap sequence.
    Select Case bitmap
      Case Is < 10:    bitmap_number = "000"
      Case Is < 100:   bitmap_number = "00"
      Case Is < 1000:  bitmap_number = "0"
      Case Is < 10000: bitmap_number = ""
    End Select
    file = "capture_" + bitmap_number + Str(bitmap) + ".bmp"
    Color red, background
    Locate 31, 50: Print file
    Locate 32, 50: Print "Warning! A bitmap sequence is being created in the current directory."
    Bsave file, 0
    bitmap += 1
    If bitmap > 2000 Then End
  End If
  If iteration = 2071 Then Sleep: If Inkey = Chr(27) Then End Else Initialization()
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
  x_twin_A -= 1
  x_twin_C -= 1
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
  Windowtitle " The Time Scanner and the Doppler Effect - Jan. 25, 2010"
  Screenset matrix_page, matrix_page
  Color black, background: Cls
' Line(0,0)-(1279,767), black, bf
' Line(0,0)-(1024,576), black, bf
' Line(0,0)-(100,15), blue, bf: Locate 2: Print Point(1,1): Print blue: Sleep: End 'color test.
  pulses = 15
  iteration = -8                                                      'exact phase in order to start emitting instantly.
  wave_display = 1
  If bitmap Then wave_display = 1
  lambda = 64
  brightness = 4
  beta = .5
  g_Lorentz = Sqr(1 - beta ^ 2)
  alpha = (1 - g_Lorentz) / beta                                      'intermediate speed alpha = 0,2679492
  beta2 = (beta + beta ) / (1 + beta * beta )
  beta3 = (beta + beta2) / (1 + beta * beta2)                         'Poincaré's formula: beta" = (beta + beta') / (1 + beta * beta').
  x_scale = -240
  wave_speed = 2 / 3                                                  'wave speed for algorithm only (non linear and inverted, maximum 0, minimum 1.99)
  c_speed = 1 / ((1 / beta) - alpha)                                  '.577 pixel per loop in order to mach the scan speed (1 / c) minus alpha speed.
  twins_distance = 0
  mirror_angle = Atn(g_Lorentz / 1)
  x_alpha = 155
  x_twin_A = x_alpha - 42
  x_scanner = x_twin_A - 286
  x_twin_B = x_twin_A
  x_twin_C = x_screen - 130
  pulse_A = 0
  pulse_B = 2
  pulse_C = 0
  If pulse_B = 2 Then threshold = - 38
  x_origin = x_alpha
  y_center = .5 * y_height
  y_clock = 86
  y_graph = 352
  damping_zone = 30 * Sqr(lambda)                                     '100 for lambda = 25 to 200 for lambda = 100.
  x_start = -damping_zone
  y_start = -damping_zone
  x_stop = x_width  + damping_zone
  y_stop = y_height + damping_zone

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

  Color black

  Locate 34, 50: ? "The reversed Lorentz transformations:"
  Locate 36, 50: ? "x' = g * x + bˆta * t   The Regular Doppler effect:  x'= g^2 * x + bˆta * t"
  Locate 37, 50: ? "t' = g * t - bˆta * x                                t'=       t - bˆta * x"
  Locate 39, 50: ? "In the future, those equations will become a must. They are unavoidable."
  Locate 40, 50: ? "Without them, it is impossible to obtain a perfect Doppler effect while"
  Locate 41, 50: ? "using a virtual wave medium."
  Locate 36,  2: ? "lambda... 64 pixels"
  Locate 37,  2: ? "beta..... 0.5   = v / c"
  Locate 38,  2: ? "g........ 0.866 = Sqr(1 - beta ^ 2)"
  Locate 39,  2: ? "alpha.... 0.268 = (1 - g) / beta"

  Locate 43, 50: ? "Press Esc. to quit."
  Locate 44, 50: ? "Press S to stop or resume emission."
  Locate 45, 50: ? "P - Pause."
  Locate 44, 2:  ? "Thanks to the creators of FreeBASIC."
  Locate 45, 02: ? "Special thanks to Philippe Delmotte"
  Locate 46, 02: ? "and Jocelyn Marcotte, the creators"
  Locate 47, 02: ? "of this amazing virtual wave medium."
  Locate 48, 02: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47, 88: ? "January 25, 2010. This program may be"
  Locate 48, 88: ? "freely distributed, copied or modified.";
  Swap work_page, visible_page
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
  Case "I": Initialization()
  Case "P": line_number = 62: click = 1                               'pause.
  Case "S": If pulse_B = 0 Then pulse_B = 1                           'start pulsating.
            If pulse_B = 3 Then pulse_B = 4                           'stop pulsating.
            If pulse_C = 0 Then pulse_C = 1                           'stop pulsating.
            If pulse_C = 2 Then pulse_C = 3                           'stop pulsating.
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
  Color green_text, white
  Locate line_number, 46

  Select Case line_number
    Case 62                                                           'pause.
      ? line62
      If click > 0 Then
        Screenset work_page, work_page: Color red, background
        Locate 45, 50: ? "Paused. Press any key to resume."
        Sleep: If Len(Inkey) Then in_key = ""
      End If
    Case 63                                                           'initialization (line 23 or 47).
      ? line63
      If click > 0 Then 
        Initialization()
      End If
  End Select
  
  If click = 1 Then                                                   'avoid repetitive actions.
    Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click = 1
  End If
End Sub


'*********************************************************************
' DISPLAYING THE GRAPHICS.
'*********************************************************************

Sub Display()
  t_time = c_speed * iteration                                        'absolute time according to wave speed vs. iteration.
  x_twin_B = x_twin_A - move_frame + beta * t_time                    'translation motion according to beta * t.

  Line(x_alpha,  y_center)-(x_alpha,  y_center - 2 * lambda - lambda / 8), white 'vertical wavelength scales.
  If pulse_C > 1 Then Line(x_twin_C,  y_center)-(x_twin_C,  y_center - 2 * lambda - lambda / 8), white
  For y = y_center - lambda / 2 - lambda / 8 To y_center - 2 * lambda - lambda / 8 Step -lambda / 2
    Line(x_twin_B - 4, y)-(x_twin_B + 4, y), white
    If pulse_C Then Line(x_twin_C - 4, y)-(x_twin_C + 4, y), white
  Next

  Line(x_alpha, y_center)-(x_alpha + 2 * lambda + lambda / 8, y_center), white 'horizontal wavelength scale.
  If x_scanner > 0 Then Circle(x_alpha, y_center), lambda / 2 + lambda / 8, white,,,1
  For x = x_alpha + lambda / 2 + lambda / 8 To x_alpha + 2 * lambda + lambda / 8 Step lambda / 2
    Line(x, y_center - 4)-(x, y_center + 4), white
  Next

  Line(0, y_graph - 1)-(x_width, y_graph + 1), black, bf              'wavelength scale for the curves.
  Line(x_twin_B + g_Lorentz * lambda, y_graph - 10)-(x_twin_B + g_Lorentz * lambda, y_graph + 10), black
  Line(x_twin_B - g_Lorentz * lambda, y_graph - 10)-(x_twin_B - g_Lorentz * lambda, y_graph + 10), black
  Line(x_twin_B, y_graph - 10)-(x_twin_B, y_graph + 10), black
  For x = lambda / 4 To lambda Step lambda / 4
    Line(x_twin_B + g_Lorentz * x, y_graph - 5)-(x_twin_B + g_Lorentz * x, y_graph + 5), black
    Line(x_twin_B - g_Lorentz * x, y_graph - 5)-(x_twin_B - g_Lorentz * x, y_graph + 5), black
  Next

  If Pulse_B = 1 Or Pulse_B = 2 Or Pulse_B = 4 Or Pulse_B = 5 Then    'highlighting the threshold.
    Line(g_Lorentz * threshold + x_twin_B, y_graph - 40)-(g_Lorentz * threshold + x_twin_B, y_graph + 40), Rgb(0, 255, 1)
  End If

  Color black, background
  Locate 21, 63: Print "Red: amplitude."
  Locate 24, 64: Print "Blue: impulse."
  Locate 41, 02: Print "Iteration"; iteration
  Locate 42, 02: Print "Scanner  "; x_scanner
  Color white, black                                                  'black is transparent, dark is opaque.
  Locate 1, 18: ? "Alpha"
  Locate 19, 02: Print "The Time Scanner"
  Locate 19, 62: Print "glafreniere.com"
  previous = y_graph

'*********************************************************************
' DISPLAYING THE WAVES - POSITIVE AMPLITUDE GREEN AND NEGATIVE RED.
'*********************************************************************

  For x = 0 To x_width: For y = 0 To y_height
    If wave_display Then
      If y = y_center Then
        potential = .036 * (trend(x-1,y) + trend(x+1,y) + trend(x,y-1) + trend(x,y+1))
        Line(x-1, previous)  -(x, y_graph - potential),   Rgb(255,0,1)
        Line(x-1, previous+1)-(x, y_graph - potential+1), Rgb(255,0,1)
        Line(x-1, previous-1)-(x, y_graph - potential-1), Rgb(255,0,1)
        previous = y_graph - potential
      End If
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
'     r = r / 8: r = r * 8: if r > 255 then r = 255                   'reducing the color palette for Gif animations.
'     g = g / 8: g = g * 8: if g > 255 then g = 255
'     b = b / 8: b = b * 8: if b > 255 then b = 255
      If Point(x,y) = white Or Point(x,y) = red Or Point(x,y) = blue Or Point(x,y) = purple Or Point(x,y) = green Or Point(x,y) = gray Or Point(x,y) = dark Then
      Else
        If scanner = 0 Then
          Pset(x,y), Rgb(r,g,b)                                       'printing the wave area.
        Else
          If x > x_scanner Then Pset(x,y), Rgb(r,g,b)
        End If
      End If
    End If

    If x = x_scanner And x > -1 And x < x_width + 1 Then              'scanning one row at a time.
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

  If x_scanner > -1 And x_scanner < x_width + 1 Then                  'adding the curve area.
    For y = y_height To y_height + 90
      scan(x_scanner, y) = Point(x_scanner, y)
    Next
  End If

  If scanner And x_scanner > -1 And x_scanner < x_width + 2 Then      'printing the scanned area.
    For x = 0 To x_scanner: For y = 0 To y_height + 90
      Pset(x,y), scan(x,y)
    Next: Next
  End If
  Line(.5 * x_width, y_height - 10)-(.5 * x_width, y_graph), black, bf'center.
  Line(x_scanner, 0)-(x_scanner, y_height + 90), white                'scanner line.
End Sub


'*********************************************************************
' UNMOVING WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Wave_Emitter()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  If pulse_C = 0 Then Exit Sub                                        'code 0: no pulsation.
  t_time = c_speed * (iteration + 20) * 2 * pi / lambda               'time, more exactly pulsation in radians.
  amplitude = Abs(Cos(t_time))

' Only one half of the amplitude must be applied in order to start or
' stop pulsating. Otherwise, a desequilibrium occurs and the average
' amplitude becomes positive or negative. The best way to achieve this
' is to start or stop pulsating only when amplitude reaches a maximum.

  If pulse_C = 1 Then                                                 'code 1: waiting for the 1/4 or 3/4 phase to start pulsating.
    If amplitude > .99 Then pulse_C = 2 Else Exit Sub                 'code 2: pulsating.
  Elseif pulse_C = 3 Then                                             'code 3: waiting for the 1/4 or 3/4 phase to stop pulsating.
    If amplitude > .99 Then
      pulse_C = 0: Exit Sub                                           'code 0: no pulsation.
    End If
  End If

  t_time = t_time - .25                                                'fine tuning in order to scan during maximum amplitude.
  previous = y_graph                                                  'first y coordinate of the impulse curve below.
  amplitude = 4200 / lambda ^ 2
  potential = amplitude * Cos(t_time)
  For x = -lambda / 4 To lambda / 4                                   'stationary emitter.
    x_squared = x^2
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
'     gaussian = pi ^ -((distance / lambda) ^ 2)                      'the normal distribution works for a stationary emitter only.
      If distance < lambda / 4 Then
        radian = 2 * pi * distance / lambda
        trend(x_twin_C + x, y_center + y) = trend(x_twin_C + x, y_center + y) + potential * Cos(radian)
        If y = 0 Then
          y_coord = y_graph - 40 * potential * Cos(radian)
          Line(x_twin_C + x, previous)-(x_twin_C + x + 1, y_coord), blue, bf 'impulse curve.
          previous = y_coord
        End If
      End If
    Next
  Next
  Line(x_twin_C + x - 1, previous)-(x_twin_C + x, y_graph), blue, bf  'last point of the impulse curve.
End Sub


'*********************************************************************
' MOVING WAVE GENERATOR - DOPPLER EFFECT USING MY
' REVERSED VERSION OF THE LORENTZ TRANSFORMATIONS.
'*********************************************************************

Sub Wave_Emitter_Doppler()                                            'moving emitter (twin B).
  If pulse_B = 0 Then Exit Sub                                        'code 0: no pulsation.
  x_coord = 0                                                         'Lorentz's x coordinate at the center of the emitting zone.
  t_time = c_speed * iteration * 2 * pi / lambda                      'absolute time in radians (according to 2 * pi).
  t_prime = g_Lorentz * t_time - beta * x_coord                       'more simply, at the origin: t_prime = g_Lorentz * t_time.
  amplitude = Abs(Sin(t_prime))                                       'checking the t' 1/4 or 3/4 phase.
  x_first = -.25 * lambda                                             'the emitting zone diameter is lambda / 2 before contraction.
  x_last  =  .25 * lambda
' Locate 34, 02: Print "Pulse code"; pulse_B; " "; threshold
' The emitting zone must respect Lorentz's local time so that a
' threshold follows the corresponding phase wave. In addition, the
' wave generator must start or stop emitting when amplitude reaches
' a maximum as shown above in Sub Wave_Emitter.

  Select Case Pulse_B                                                 'code 1: waiting zero amplitude to set the threshold.
    Case 1: If amplitude < .01 Then                                   'set .1 instead of .01 for shorter wavelength.
              pulse_B = 2
              threshold = -lambda / 2                                 'waiting for the 1/4 or 3/4 phase to start emitting.
            End If
            Exit Sub
    Case 2: threshold += 1                                            'code 2: emitting on the left side of the threshold.
            If threshold < x_first Then
              Exit Sub
            Elseif threshold > x_last Then
              pulse_B = 3: threshold = 0
            Else x_last = threshold
            End If                                                    'code 3: constant regular waves (no action).
    Case 4: If amplitude < .01 Then                                   'code 4: waiting for the 1/4 or 3/4 phase to stop emitting.
              pulse_B = 5: threshold = - lambda / 2
            End If
    Case 5: threshold += 1                                            'code 5: emitting on the right side of the threshold.
            If threshold > x_last Then
              pulse_B = 0                                             'code 0: no pulsation any more.
              threshold = 0
              Exit Sub
            Elseif threshold > x_first Then
              x_first = threshold
            End If
  End Select

  previous = y_graph                                                  'first y coordinate of the impulse curve below.
  t_time = c_speed * iteration / lambda                               'Lorentz's absolute t time, in wave period units.
  t_time = t_time + .011                                              'fine tuning for scanner.
  For x = x_first To x_last                                           'scanning the 2-D circular impulse zone, which will contract.
    x_squared = x^2                                                   'Lorentz's x coordinate for the emitting zone squared, in pixel units.
    x_coord = x / lambda                                              'Lorentz's x coordinate for the emitting zone, in wavelength units.

'*************************************************************
' Below is my reversed version of the Lorentz transformations.
'*************************************************************

    x_prime = g_Lorentz * x_coord + beta * t_time                     'Lorentz's x' coordinate for the emitting zone, in wavelength units.
    t_prime = g_Lorentz * t_time  - beta * x_coord                    'Lorentz's t' time, in wave period units.

' The Woldemar Voigt reversed transformations are featuring a variable
' frequency according to Voigt's constant:
'   x_prime = x_coord * g_Lorentz * Voigt_s_constant + beta * t_time  'Voigt's x' coordinate in wavelength units.
'   t_prime = t_time  * g_Lorentz / Voigt_s_constant - beta * x_coord 'Voigt's t' time (actually the emitter phase), in 2 pi units.
' In order to produce the regular Doppler effect, Voigt's equations
' may be simplified as below because Voigt's "k" constant equals
' Lorentz's g factor. In this case, the transverse wavelength
' contracts according to g and there is no frequency shift:
'   x_prime = x_coord * g_Lorentz^2 + beta * t_time                   'try this!
'   t_prime = t_time - beta * x_coord
    
    x_prime = x_twin_A - move_frame + x_prime * lambda                'Lorentz's x' coordinate in pixels for twin B, relative to twin A.
    t_prime = t_prime * 2 * pi                                        'Lorentz's t' time in radians.
    potential = Sin(t_prime)
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)                                 'absolute distance in pixel units.
      If distance < .25 * lambda Then
        curve = Cos(2 * pi * distance / lambda)
        trend(x_prime, y_center + y) = trend(x_prime, y_center + y) + potential * curve
        If y = 0 And x_scanner <= x_prime Then
          y_coord = y_graph - 40 * potential * curve
          Line(x_prime, previous)-(x_prime+1, y_coord), blue, bf     'impulse curve.
          previous = y_coord
        End If
      End If
    Next
  Next
  If x_scanner <= x_prime Then Line(x_prime, previous)-(x_prime+1, y_graph), blue, bf 'last point of the impulse curve.
End Sub
