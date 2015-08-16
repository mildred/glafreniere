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

Dim Shared As Integer x_screen = 1024, y_screen = 310, x_width = 1023, y_height = 335
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_first, x_last, x_alpha, x_center, y_center
Dim Shared As Integer r, g, b, x, y, scanner, x_scanner, x_previous, x_squared, x_mouse, y_mouse
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_origin, y_clock, y_graph
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulses, pulse_A, pulse_B, pulse_C, lambda, threshold, damping_zone = 401
Dim Shared As Integer frame, skipped_frames, line_number, axial, OK, unit, scan(-4 To x_width + 8, y_height + 100)

Dim Shared As Single x_coord, y_coord, x_prime, y_prime, x_twin_A, x_twin_B, x_twin_C
Dim Shared As Single alpha, beta, beta2, ratio, factor, g_Lorentz, g_alpha, curve, Doppler, field_contraction
Dim Shared As Single t_time, t_prime, k_Voigt, k_Dewavrin, move_frame, frame_speed, c_speed, twins_distance
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
skipped_frames = 0: scanner = 0: bitmap = 0                           'set bitmap = 1 for BMP image sequence.
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
        trend(x,y) = .5 * (present(x-1, y) + present(x, y-1) + present(x, y+1) + present(x+1, y)) - past(x,y)'simpler trend extrapolation.
'       orthogonal = present(x-1, y) + present(x, y-1) + present(x, y+1) + present(x+1, y) 'orthogonal influence.
'       diagonal   = present(x-1, y-1) + present(x-1, y+1) + present(x+1, y-1) + present(x+1, y+1)'diagonal influence.
'       trend(x,y) = .5 * orthogonal - past(x,y)                      'simpler trend extrapolation.
'       trend(x,y) = .5 * orthogonal + .25 * diagonal - present(x,y) - past(x,y)                  'trend extrapolation.
'       trend(x,y) = .4 * (orthogonal + present(x,y)) - past(x,y)     'slower wave speed: .4 * (4 + 1) - 1 = 1 (constant pixel sum = 1).
'       trend(x,y) = .25 * (2 - wave_speed) * orthogonal + wave_speed * present(x,y) - past(x,y) 'scanner-compatible slower speed.
      Next: Next
    End If

    x_scanner += 1
    If x_scanner > x_width + 2 And scanner = 1 Then
     Sleep: If Inkey = Chr(27) Then End Else Initialization()
    End If
    move_frame += c_speed * alpha
    If move_frame > 1 Then
      move_frame -= 1: Frame_of_Reference()
    Else OK = 0
    End If
    Damping_Management()                                              'processing damping zone.
    If wave_display Then Wave_Emitter(): Wave_Emitter_Doppler()       'sinusoidal impulses.
    iteration += 1
'    If iteration = 300 Then pulse_C = 1
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
'  If bitmap > 0 Then                                                 'set bitmap = 1 for bitmap sequence.
  If bitmap > 0 And iteration Mod 2 = 0 Then
'  If bitmap > 0 And iteration > 400 Then
    Select Case bitmap
      Case Is < 10:    bitmap_number = "000"
      Case Is < 100:   bitmap_number = "00"
      Case Is < 1000:  bitmap_number = "0"
      Case Is < 10000: bitmap_number = ""
    End Select
    file = "capture_" + bitmap_number + Str(bitmap) + ".bmp"
    Color red, background
    Locate 38, 50: Print file
    Locate 40, 50: Print "Warning! A bitmap sequence is being created in the current directory."
    Bsave file, 0
    bitmap += 1
    If bitmap > 1200 Then End
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
  x_origin -= 1
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
  Windowtitle " The Alpha Preferred Speed and the Fields of Force. - Jan. 29, 2010."
  Screenset matrix_page, matrix_page
  Color black, background: Cls
' Line(0,0)-(100,15), blue, bf: Locate 2: Print Point(1,1): Print blue: Sleep: End 'color test.
  wave_display = 1
  lambda = 64
  iteration = 20
  brightness = 4
  beta = .5
  g_Lorentz = Sqr(1 - beta ^ 2)
  alpha = (1 - g_Lorentz) / beta                                      'intermediate speed alpha = 0,2679492
  g_alpha = Sqr(1 - alpha ^ 2)                                        'electron contraction at this speed.
  Doppler = 1 + alpha                                                 'backward regular Doppler to be neutralized by faster frequency. 
  k_Voigt = g_alpha / Doppler                                         'a smaller Voigt's constant (see Lorentz transformations) produces a faster frequency.
  field_contraction = g_alpha * k_Voigt                               'standing wave contraction according to ratio 1 : backward Doppler.
' on-axis standing wave contraction is given by: g_Lorentz * k_Voigt. 'k_Voigt is Voigt's constant.
'                  faster frequency is given by: g_Lorentz / k_Voigt.
  field_contraction = 1 - alpha
  Doppler = (1 - beta) / g_Lorentz                                    'forward Lorentz Doppler for wavelength scale. 
' wave_speed = 2 / 3                                                  'wave speed for algorithm only (non linear and inverted, maximum 0, minimum 1.99)
' c_speed = 1 / ((1 / beta) - alpha)                                  '.577 pixel per loop in order to mach the scan speed (1 / c) minus alpha speed.
  c_speed = .714                                                      'regular speed.
  x_alpha = 411
  x_twin_A = x_width
  x_origin = 0
  x_scanner = -1100
  pulse_A = 1
  pulse_B = 1
  y_center = .5 * y_height
  y_graph = 456
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

  Color black
  Locate 22,  2: ? "Beta normalized speed..";: ? Using "##.### = v / c"; beta
  Locate 23,  2: ? "Contraction factor g...";: ? Using "##.### = Sqr(1 - beta^2)"; g_Lorentz
  Locate 24,  2: ? "Backward Doppler.......";: ? Using "##.###"; (1 + beta) / g_Lorentz;:?" = (1 + beta) / g"; 
  Locate 25,  2: ? "Forward Doppler........";: ? Using "##.### = (1 - beta) / g"; (1 - beta) / g_Lorentz
  Locate 34,  2: ? "Lambda (wavelength)...."; lambda; " pixels."
  Locate 35,  2: ? "Alpha preferred speed..";: ? Using "##.### = (1 - g) / beta"; alpha
  Locate 36,  2: ? "Field contraction......";: ? Using "##.### = 1 - alpha"; field_contraction
  Locate 22, 53: ? "This scene is displayed in the Alpha frame of reference. Hence, the two"
  Locate 23, 53: ? "electrons shown above apparently approach one another at the same speed."
  Locate 24, 53: ? "The frequency of the moving one is slower in accordance with Lorentz's"
  Locate 25, 53: ? "factor: F' = g * F so that the transverse wavelength remains constant."
  Locate 33, 53: ? "Blue curve: impulse. Red curve: amplitude."
  Locate 34, 53: ? "All fields of force are made out of standing waves whose energy is"
  Locate 35, 53: ? "equally radiated on both sides in spite of their motion. This justifies"
  Locate 36, 53: ? "the equal Action and Reaction Principle and Newton's laws still hold true."

  Locate 43, 50: ? "Press Esc. to quit."
  Locate 45, 50: ? "P - Pause."
  Locate 44, 2:  ? "Thanks to the creators of FreeBASIC."
  Locate 45, 02: ? "Special thanks to Philippe Delmotte"
  Locate 46, 02: ? "and Jocelyn Marcotte, the creators"
  Locate 47, 02: ? "of this amazing virtual wave medium."
  Locate 48, 02: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47, 88: ? "January 29, 2010. This program may be"
  Locate 48, 88: ? "freely distributed, copied or modified.";
  Line(0,576)-(x_width, 576), black
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
            If pulse_C = 0 Then pulse_C = 1                           'start pulsating.
            If pulse_C = 3 Then pulse_C = 4                           'stop pulsating.
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
        Sleep: If Inkey = Chr(27) Then End
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

  Line(x_twin_A, 0)-(x_twin_A, y_center), white                       'vertical wavelength scales.
  Line(x_twin_B, 0)-(x_twin_B, y_center), white
  For y = y_center - lambda / 2 - lambda / 8 To y_center - 3 * lambda Step -lambda / 2
    Line(x_twin_A - 4, y)-(x_twin_A + 4, y), white
    Line(x_twin_B - 4, y)-(x_twin_B + 4, y), white
  Next

  For x = x_twin_B To x_alpha - 20 Step (lambda / 2) * (1 - beta) / g_Lorentz'Doppler-modified wavelength scale (twin B).
    Line(x, y_graph - 4)-(x, y_graph + 4), black
  Next
  For x = x_twin_B To 0 Step -(lambda / 2) * (1 + beta) / g_Lorentz
    Line(x, y_graph - 4)-(x, y_graph + 4), black
  Next
  For x = x_twin_A To x_alpha + field_contraction * 3 * lambda + 20 Step -lambda / 2'regular wavelength scale (twin A).
    Line(x, y_graph - 4)-(x, y_graph + 4), black
  Next
  For x = x_twin_A To x_width Step lambda / 2
    Line(x, y_graph - 4)-(x, y_graph + 4), black
  Next
  
  previous = 0
  distance = Abs(x_alpha + 70 - x_twin_A)
  amplitude = 515 / Sqr(distance)
  For x = x_alpha To x_alpha + field_contraction * 3 * lambda - 1     'node and antinode envelope.
    radian = 2 * pi * (x - x_alpha) / (field_contraction * lambda)
    y_coord = amplitude * Abs(Sin(radian)) + 1
    Line(x, y_graph - y_coord)-(x, y_graph + y_coord), white
    Line(x-1, y_graph - previous)-(x, y_graph - y_coord), black
    Line(x-1, y_graph + previous)-(x, y_graph + y_coord), black
    previous = y_coord
  Next
  Line(x-1, y_graph - previous)-(x, y_graph), black
  Line(x-1, y_graph + previous)-(x, y_graph), black
  Line(x_alpha - 20, y_graph - 45)-(x_alpha + field_contraction * 3 * lambda + 20, y_graph + 45), black, B
  Line(x_alpha - 19, y_graph - 44)-(x_alpha + field_contraction * 3 * lambda + 19, y_graph + 44), black, B

  Line(0, y_graph)-(x_width, y_graph), black                          'wavelength scale for the curves.
  Color black, background
  Locate 41, 02: Print "Iteration"; iteration
  Locate 42, 02: Print "Scanner  "; x_scanner
  Color white, black                                                  'black is transparent, dark is opaque.
  Locate 21,113: Print "glafreniere.com"
  previous = y_graph

'*********************************************************************
' DISPLAYING THE WAVES - POSITIVE AMPLITUDE GREEN AND NEGATIVE RED.
'*********************************************************************

  For x = 0 To x_width: For y = 0 To y_height
    If wave_display Then
      If y = y_center Then
        potential = .1 * (trend(x-1,y) + trend(x+1,y) + trend(x,y-1) + trend(x,y+1))
        Line(x-1, previous)-(x, y_graph - potential), Rgb(255,0,1), b
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
End Sub


'*********************************************************************
' REGULAR WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Wave_Emitter()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  If pulse_A = 0 Then Exit Sub                                        'code 0: no pulsation.
  previous = 0
  For x = x_alpha + field_contraction * 3 * lambda + 20 To x_width   'white envelope according to distance.
    distance = Abs(x - x_twin_A)
    If distance < .2 * lambda Then
      radian = 1.35 * pi * distance / lambda                          'adjusting a sinusoidal curve to the 3 / 4 lambda central antinode.
      y_coord = 102 * Cos(radian)
    Else y_coord = 3.9 * lambda / Sqr(distance)
    End If
    Line(x, y_graph - y_coord)-(x, y_graph + y_coord), white
    Line(x-1, y_graph - previous - 1)-(x, y_graph - y_coord - 1), black
    Line(x-1, y_graph + previous + 1)-(x, y_graph + y_coord + 1), black
    previous = y_coord
  Next

  t_time = c_speed * iteration * 2 * pi / lambda                      'time, more exactly pulsation in radians.
  amplitude = Abs(Cos(t_time))
'  Locate 36,90:Print Using "###.####"; amplitude

' Only one half of the amplitude must be applied in order to start or
' stop pulsating. Otherwise, a desequilibrium occurs and the average
' amplitude becomes positive or negative. The best way to achieve this
' is to start or stop pulsating only when amplitude reaches a maximum.

  If pulse_A = 1 Then                                                 'code 1: waiting for the 1/4 or 3/4 phase to start pulsating.
    If amplitude > .99 Then pulse_A = 3 Else Exit Sub                 'code 3: pulsating.
  Elseif pulse_A = 4 Then                                             'code 4: waiting for the 1/4 or 3/4 phase to stop pulsating.
    If amplitude > .99 Then
      pulse_A = 0: Exit Sub                                           'code 0: no pulsation.
    End If
  End If

  previous = y_graph                                                  'first y coordinate of the impulse curve below.
  amplitude = 6700 / lambda ^ 2
  potential = amplitude * Cos(t_time)
  For x = -lambda / 4 To lambda / 4                                   'stationary emitter.
    x_squared = x^2
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
'     gaussian = pi ^ -((distance / lambda) ^ 2)                      'the normal distribution works for a stationary emitter only.
      If distance < lambda / 4 Then
        radian = 2 * pi * distance / lambda
        trend(x_twin_A + x, y_center + y) = trend(x_twin_A + x, y_center + y) + potential * Cos(radian)
        If y = 0 Then
          y_coord = y_graph - 62 * potential * Cos(radian)
          Line(x_twin_A + x - 1, previous)-(x_twin_A + x, y_coord), blue, b 'impulse curve.
          previous = y_coord
        End If
      End If
    Next
  Next
  Line(x_twin_A + x - 1, previous)-(x_twin_A + x, y_graph), blue, b   'last point of the impulse curve.
End Sub


'*********************************************************************
' TWIN B AND ITS MOVING WAVE GENERATOR - DOPPLER EFFECT
' USING MY REVERSED VERSION OF THE LORENTZ TRANSFORMATIONS.
'*********************************************************************

Sub Wave_Emitter_Doppler()                                            'twin B and its moving emitter.
  If pulse_B = 0 Then Exit Sub                                        'code 0: no pulsation.
  x_coord = 0                                                         'Lorentz's x coordinate in wavelength units (origin).
  t_time = c_speed * iteration / lambda                               'Lorentz's t absolute time in wave period units.

' Applying the Lorentz transformations for twin B.********************
  x_prime = g_Lorentz * x_coord + beta * t_time                       'Lorentz's x' coordinate in wavelength units.
  t_prime = g_Lorentz * t_time - beta * x_coord                       'Lorentz's t' local time in wave period units.

  x_twin_B = x_prime * lambda + x_origin - move_frame                 'Lorentz's x' coordinate for twin B in pixel units.
  phase = 2 * pi * t_prime                                            'Lorentz's t' local time, more exactly the phase in radians.
  previous = 0
  For x = 0 To x_alpha - 20                                          'white envelope according to distance.
    distance = Abs(x - x_twin_B)
    If distance < .2 * lambda * g_Lorentz Then
      radian = 1.5 * pi * distance / lambda                           'adjusting a sinusoidal curve to the 3 / 4 lambda central antinode.
      y_coord = 108 * Cos(radian)
    Else y_coord = 3.9 * lambda / Sqr(distance)
    End If
    Line(x, y_graph - y_coord)-(x, y_graph + y_coord), white
    Line(x-1, y_graph - previous - 1)-(x, y_graph - y_coord - 1), black
    Line(x-1, y_graph + previous + 1)-(x, y_graph + y_coord + 1), black
    previous = y_coord
  Next
  Line(x-1, y_graph - previous - 1)-(x, y_graph), black
  Line(x-1, y_graph + previous + 1)-(x, y_graph), black
  amplitude = Abs(Cos(phase))                                         'checking for the t' 1/4 or 3/4 phase.
'  Locate 36,30:Print Using "###.####"; amplitude;:? " ";Pulse_B
  x_first = -.25 * lambda                                             'the emitting zone radius is lambda / 4 before its contraction.
  x_last  =  .25 * lambda

' The emitting zone must respect Lorentz's local t' time. Hence, the
' threshold follows the corresponding phase wave. In addition, the
' wave generator must start or stop emitting when amplitude reaches
' a maximum as shown above (see Sub Wave_Emitter).

  Select Case Pulse_B                                                 'code 1: waiting ZERO amplitude to set the threshold.
    Case 1: If amplitude < .01 Then                                   'set .1 instead of .01 for shorter wavelength.
              pulse_B = 2
              threshold = -.5 * lambda * g_Lorentz^2                  'waiting for the 1/4 or 3/4 phase to start emitting.
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
              pulse_B = 5: threshold =  -.5 * lambda * g_Lorentz^2
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
  For x = x_first To x_last                                           'the impulse zone will contract.
    x_squared = x^2
    x_coord = x / lambda                                              'Lorentz's x  coordinate in wavelength units.
    x_prime = g_Lorentz * x_coord + beta * t_time                     'Lorentz's x' coordinate in wavelength units.
    t_prime = g_Lorentz * t_time - beta * x_coord                     'Lorentz's t' local time in wave period units.
    
    x_prime = x_prime * lambda + x_origin - move_frame                'Lorentz's x' coordinate in pixel units.
    radian = 2 * pi * t_prime                                         'Lorentz's t' local time for this coordinate.
    potential = 1.52 * Cos(radian)
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)                                 'absolute distance in pixel units.
      If distance < .25 * lambda Then
        curve = Cos(2 * pi * distance / lambda)
        trend(x_prime, y_center + y) = trend(x_prime, y_center + y) + potential * curve
        If y = 0 Then
          y_coord = y_graph - 72 * potential * curve
          Line(x_prime - 1, previous)-(x_prime, y_coord), blue, b     'impulse curve.
          previous = y_coord
        End If
      End If
    Next
  Next
  Line(x_prime - 1, previous)-(x_prime, y_graph), blue, b             'last point of the impulse curve.
End Sub
