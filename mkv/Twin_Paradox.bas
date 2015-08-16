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

Dim Shared As Integer x_screen = 1280, y_screen = 1024, x_width = 1279, y_height = 767
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_alpha, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer r, g, b, x, y, scanner, x_scanner, x_previous, x_squared
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_twin_A, x_twin_B, x_twin_B_prime, x_origin, y_clock
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulse_A, pulse_B, lambda, target, damping_zone = 401
Dim Shared As Integer frame, skipped_frames, line_number, axial, OK, unit, scan(-4 To x_width + 4, y_height)

Dim Shared As Single x_coord, x_prime, y_prime, y_coord
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
'        trend(x,y) = .5 * orthogonal - past(x,y)                      'fastest trend extrapolation (constant pixel sum = 1).
'        trend(x,y) = .4 * (orthogonal + present(x,y)) - past(x,y)     'slower wave speed: .4 * (4 + 1) - 1 = 1
        trend(x,y) = .25 * (2 - wave_speed) * orthogonal + wave_speed * present(x,y) - past(x,y) 'scanner compatible slower speed.
      Next: Next
    End If

    x_scanner += 1
    If x_scanner = x_width + 2 Then
      If scanner = 1 Then
        Sleep: If Inkey = Chr(27) Then End Else Initialization()
      End If
    Elseif x_scanner > x_width + 2 Then
      x_scanner = x_width + 3
    Elseif x_scanner-2 = x_origin Or x_scanner-2 = x_alpha Or x_scanner-2 = x_origin+10*lambda Or x_scanner-2 = Int(x_origin+g_Lorentz*10*lambda) Then
      Sleep 2000: If Len(Inkey) Then Sleep
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
'  If bitmap > 0 And x_scanner > -2 And iteration Mod 2 = 0 Then
  If bitmap > 0 Then                                                  'set bitmap = 1 for bitmap sequence.
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
  Windowtitle " The Time Scanner and the Twin Paradox - Jan. 10, 2010"
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Line(0,0)-(1279,767), black, bf
' Line(0,0)-(100,15), blue, bf: Locate 2: Print Point(1,1): Print blue: Sleep: End 'color test.
  iteration = 0
  pulse_A = 1
  pulse_B = 0
  wave_display = 1
  If bitmap Then wave_display = 1
  lambda = 32
  brightness = 20
  beta = .5
  g_Lorentz = Sqr(1 - beta ^ 2)
  alpha = (1 - g_Lorentz) / beta
  beta2 = (beta + beta ) / (1 + beta * beta )
  beta3 = (beta + beta2) / (1 + beta * beta2)                         'Poincaré's formula: beta" = (beta + beta') / (1 + beta * beta').
  x_scale = -240
  wave_speed = 2 / 3                                                  'wave speed for algorithm only (non linear and inverted, maximum 0, minimum 1.99)
  c_speed = 1 / ((1 / beta) - alpha)                                  '.577 pixel per loop in order to mach the scan speed (1 / c) minus alpha speed.
  twins_distance = 0
  mirror_angle = Atn(g_Lorentz / 1)
  x_alpha = 519
  x_origin = x_alpha - (1 - 2 * alpha) * 10 * lambda + 1
  x_scanner = x_alpha - 1111
  y_center = .5 * y_height
  y_clock = 86
  x_twin_A = x_alpha
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

  Line(x_origin, 0)-(x_origin, y_center - 158), white                 'vertical wavelength scale.
  For y = 0 To 12 * lambda Step lambda
    Line(x_origin-4, y_center-y- 158)-(x_origin+4, y_center-y-158), white
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
  Locate 63,122: ? "January 10, 2010. This program may be"
  Locate 64,122: ? "freely distributed, copied or modified.";
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
  Locate 58, 2: ? "Brightness: Press +/-. Press = to reset."
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
  Locate 02, 02: ? "Twins A and B have witnesses A' and B' in order to check their situation."
  Locate 03, 02: ? "A and B' synchronize their clocks when they meet."
  Locate 04, 02: ? "Wavelength: 32 pixels = 1 light-second."
  Locate 05, 02: ? "Frequency: 1 Hz."
  Locate 06, 02: ? "Distance in light-seconds:"
  Locate 08, 10: ? "A to A'.... 10.00"
  Locate 09, 10: ? "B to B'....  8.66"
  Locate 10, 10: ? "A to B'....  0.00"
  Locate 11, 10: ? "A to B .... 10.00"
  Locate 13, 02: ? "Time shift in slow seconds:"
  Locate 15, 12: ? "A vs. A':  0"
  Locate 16, 12: ? "B vs. B': -5"
  Locate 18, 02: If scanner = 1 then ? "Observer B preferred." Else ? "Observer A preferred."
  Locate 02, 93: ? "Half speed alpha......... 0.26795       alpha = (1 - g) / beta"
  Locate 03, 93: ? "Contraction.............. 0.963433      Sqr(1 - alpha ^ 2)"
  Locate 04, 93: ? "Normalized speed beta.... 0.5           beta (b) = v / c"
  Locate 05, 93: ? "Contraction factor g..... 0.866         g = Sqr(1 - beta ^ 2)"
  Locate 06, 93: ? "Double speed (green)..... 0.8           (.5 + .5) / (1 + .5 * .5)"
  Locate 07, 93: ? "Contraction.............. 0.6           Sqr(1 - .8 ^ 2)"
  Locate 08, 93: ? "Quadruple speed (purple). 0.92857       (.5 + .8) / (1 + .5 * .8)"
  Locate 09, 93: ? "Contraction.............. 0.37115       Sqr(1 - .92857 ^ 2)"
  Locate 11, 93: ? "The Time Scanner corrects the forward vs. backward time delay so "
  Locate 12, 93: ? "that it can switch the point of view from twin A to twin B much the"
  Locate 13, 93: ? "same way the Lorentz transformations do. The twin A is still the"
  Locate 14, 93: ? "preferred one because he is stationary with respect to the aether."
  Locate 14, 41: ? "x =  0    1   2   3   4   5   6   7   8   9  10"
  Locate 35, 41: ? "x =   0   1   2   3   4   5   6   7   8   9  10"
  Locate 36, 41: ? "t =   0.0 sec."
  Locate 39, 02: ? "Special thanks to Philippe Delmotte"
  Locate 40, 02: ? "and Jocelyn Marcotte, the creators"
  Locate 41, 02: ? "of this amazing virtual wave medium."
  Locate 42, 02: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 44, 02: ? "The Lorentz reversed transformations:"
  Locate 46, 02: ? "x'= g * x + b * t =  8.66 light-seconds for B.   x = 10; t = 0 sec."
  Locate 47, 02: ? "t'= g * t - b * x = -5 seconds for B.            x = 10; t = 0 sec."
  Locate 36,105: ? "The Time Scanner and the Twin Paradox."
  Locate 38, 93: ? "The problem with A and B is that they cannot reconcile their point "
  Locate 39, 93: ? "of view because of the perfect symmetry. Their motion with respect "
  Locate 40, 93: ? "to the aether cannot be measured for this reason. The medium which"
  Locate 41, 93: ? "is responsible for the propagation of waves transmitting forces is "
  Locate 42, 93: ? "still indispensable, though. Rejecting it was a huge error."
  Locate 44, 93: ? "The Alpha Observer is moving at the mean speed between A and B. He "
  Locate 45, 93: ? "proves to be very useful in order to reconcile their point of view."
  Locate 46, 93: ? "All of them should agree to use common alpha Cartesian coordinates"
  Locate 47, 93: ? "and time units. Transforming space and time is definitely absurd."
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
  Case "S": If pulse_B = 0 Then pulse_B = 1                           'start pulsating.
            If pulse_B = 2 Then pulse_B = 3                           'stop pulsating.
  Case "+": brightness = brightness / Sqr(.5)                         'brighter.
            If brightness > 80   Then brightness = 80
  Case "-": brightness = brightness * Sqr(.5)                         'darker.
            If brightness < 5 Then brightness = 5
  Case "=": brightness = 20                                           'normal brightness.
  Case "0": skipped_frames = 0
  Case "1","2","3","4","5","6","7","8","9"                            'skip up to 9 frames.
            If scanner = 0 Then skipped_frames = (Val(in_key)) ^ 2
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
        Color blue: Screenset matrix_page, visible_page
        Screenset matrix_page, visible_page
        Color black, background:      Locate 58, 46: Print line58
        Color green_text, background: Locate 59, 46: Print line59
      End If
    Case 59                                                           'don't scan.
      If scanner = 0 Then ? line59
      If click > 0 Then
        scanner = 1
        Screenset matrix_page, visible_page
        Color black, background:      Locate 59, 46: Print line59
        Color green_text, background: Locate 58, 46: Print line58
      End If
    Case 60                                                           'don't scan.
      If scanner = 1 Then ? line60
      If click > 0 Then
        wave_display = 1
        Color blue: Screenset matrix_page, visible_page
        Screenset matrix_page, visible_page
        Color black, background:      Locate 60, 46: Print line60
        Color green_text, background: Locate 61, 46: Print line61
      End If
    Case 61                                                           'don't scan.
      If scanner = 0 Then ? line61
      If click > 0 Then
        wave_display = 0
        Screenset matrix_page, visible_page
        Color black, background:      Locate 61, 46: Print line61
        Color green_text, background: Locate 60, 46: Print line60
      End If
    Case 62                                                           'scan.
      ? line62
      If click > 0 Then
        Screenset work_page, work_page: Color red, background
        Locate 62, 46: ? " Paused. Press any key to resume.      "
        Sleep: If Len(Inkey) Then in_key = ""
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
' DISPLAYING THE GRAPHICS.
'*********************************************************************

Sub Display()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Color white, black                                                  'black is transparent to waves, dark is opaque.

  x_twin_B = x_twin_A - move_frame + twins_distance
  x_twin_B_prime = x_twin_A  - move_frame + twins_distance - g_Lorentz * 10 * lambda
  t_time = (x_twin_B_prime - x_twin_A) / beta / lambda                'time based on beta speed and A to B' distance.

  If iteration > 556 Then                                             'the radio signal from A must reach B when distance is 10 light-seconds.
    Circle(x_twin_A, y_center), (iteration - 556) * c_speed + 4, gray,,,1
    Circle(x_twin_A, y_center), (iteration - 556) * c_speed - 4, gray,,,1
    radian = Atn(.5 * y_height / x_twin_A)
    x = x_twin_A + (iteration - 556)  * c_speed * Cos(pi + radian)
    y = y_center + (iteration - 556)  * c_speed * Sin(pi + radian)
    Paint(x,y), white, gray
'   circle(x,y), 3, red                                               'checking accurate position!
    x = x_twin_A + (iteration - 556)  * c_speed * Cos(pi - radian)
    y = y_center + (iteration - 556)  * c_speed * Sin(pi - radian)
    Paint(x,y), white, gray
    radian = Atn(.5 * y_height / (x_width - x_twin_A))
    x = x_twin_A + (iteration - 556)  * c_speed * Cos(radian)
    y = y_center + (iteration - 556)  * c_speed * Sin(radian)
    Paint(x,y), white, gray
    x = x_twin_A + (iteration - 556)  * c_speed * Cos(-radian)
    y = y_center + (iteration - 556)  * c_speed * Sin(-radian)
    Paint(x,y), white, gray
  End If

  x_coord = x_twin_A - move_frame + 10 * lambda                       'the Doppler signal from B must reach A when
  If iteration > 1115 Then                                            'distance to each other is 15 light-seconds.
    Circle(x_coord + 3, y_center), (iteration - 1110) * c_speed - 5, gray,,,1
    Circle(x_coord - 3, y_center), (iteration - 1110) * c_speed + 5, gray,,,1
    radian = Atn((.5 * y_height)/x_coord)
    x = x_coord + (iteration - 1110) * c_speed * Cos(pi + radian)
    y = y_center + (iteration - 1110) * c_speed * Sin(pi + radian)
    Paint(x,y), white, gray
    x = x_coord + (iteration - 1110)  * c_speed * Cos(pi - radian)
    y = y_center + (iteration - 1110)  * c_speed * Sin(pi - radian)
    Paint(x,y), white, gray
    radian = Atn((.5 * y_height)/(x_width - x_coord))
    x = x_coord + (iteration - 1110)  * c_speed * Cos(radian)
    y = y_center + (iteration - 1110)  * c_speed * Sin(radian)
    Paint(x,y), white, gray
    x = x_coord + (iteration - 1110)  * c_speed * Cos(-radian)
    y = y_center + (iteration - 1110)  * c_speed * Sin(-radian)
    Paint(x,y), white, gray
  End If

  For y = 0 To 12 * lambda Step lambda                                'vertical wavelength scale moving with A.
    Line(x_twin_A-4, y_center-y- 158)-(x_twin_A+4, y_center-y-158), white
  Next

  If iteration = 556 Then                                             'A stops emitting after a 5 second delay and the end
    pulse_A = 3: Sleep 2000: If Len(Inkey) Then Sleep                 'of his signal will reach B after 5 additional seconds.
  Elseif iteration = 1080 Then 
    pulse_B = 1                                                       'B starts emitting as a response to A.
    Sleep 2000: If Len(Inkey) Then Sleep
  Elseif iteration = 1661 Then
    pulse_B = 3: Sleep 2000: If Len(Inkey) Then Sleep                 'B stops emitting when his signal reaches A.
  End If
  
  Color white, dark                                                   'dark is opaque to waves.
  x_coord = x_twin_A - move_frame + twins_distance * (beta3 / beta)   'two purple references.
  Circle(x_coord, y_center), 40, purple,,,1 / Sqr(1-beta3^2)
  Paint( x_coord-13, y_center), purple, purple                        'contraction: 40 * 0.36115 = 14. 
  Circle(x_coord, y_center), 31, dark,,,1 / Sqr(1-beta3^2)
  Paint( x_coord-10, y_center), dark, dark
  
  x_coord = x_twin_A - move_frame - twins_distance * (beta3 / beta)
  Circle(x_coord, y_center), 40, purple,,,1 / Sqr(1-beta3^2)
  Paint( x_coord+13, y_center), purple, purple
  Circle(x_coord, y_center), 31, dark,,,1 / Sqr(1-beta3^2)
  Paint( x_coord+10, y_center), dark, dark

  x_coord = x_twin_A - move_frame + twins_distance * (beta2 / beta)   'two green references 0.8 c.
  Circle(x_coord, y_center), 40, green,,,1 / Sqr(1-beta2^2)
  Paint( x_coord-23, y_center), green, green                          'contraction: 40 * 0.6 = 24. 
  Circle(x_coord, y_center), 31, dark,,,1 / Sqr(1-beta2^2)' 0.6 contraction.
  Paint( x_coord-17, y_center), dark, dark
  
  x_coord = x_twin_A - move_frame - twins_distance * (beta2 / beta)
  Circle(x_coord, y_center), 40, green,,,1 / Sqr(1-beta2^2)
  Paint( x_coord+22, y_center), green, green
  Circle(x_coord, y_center), 31, dark,,,1 / Sqr(1-beta2^2)
  Paint( x_coord+17, y_center), dark, dark
  
  Circle(x_twin_A, y_center + twins_distance), 41, 5,,, g_Lorentz     'red C and D moving transversally 0.5 c.
  Paint( x_twin_A, y_center + twins_distance), red, 5
  Circle(x_twin_A, y_center + twins_distance), 31, dark,,, g_Lorentz
  Paint( x_twin_A, y_center + twins_distance), dark, dark

  Circle(x_twin_A, y_center - twins_distance), 41, 6,,, g_Lorentz
  Paint( x_twin_A, y_center - twins_distance+33), red, 6              'contraction: 40 * 0.866 = 34. 
  Circle(x_twin_A, y_center - twins_distance), 31, dark,,, g_Lorentz
  Paint( x_twin_A, y_center - twins_distance+25), dark, dark

  x_coord = x_twin_A - move_frame - (x_twin_B - x_twin_A)             'red reference moving leftward 0.5 c.
  Circle(x_coord, y_center), 41, 3,,,1 / g_Lorentz
  Paint( x_coord+33, y_center), red, 3
  Circle(x_coord, y_center), 31, dark,,,1 / g_Lorentz
  Paint( x_coord+25, y_center), dark, dark

  Circle(x_twin_A, y_center + y_clock), 40, blue,,,1                  'central reference blue clock A moving rightward 0.5 c.
  Paint( x_twin_A, y_center + y_clock), blue, blue
  Circle(x_twin_A, y_center + y_clock), 31, white,,,1                    
  Paint( x_twin_A, y_center + y_clock), white, white
  Circle(x_twin_A, y_center + y_clock), 5, dark,,,1                    
  Paint( x_twin_A, y_center + y_clock), dark, dark

  Line(x_twin_A - 4, y_center)-(x_twin_A + 4, y_center + y_clock - 40), blue, BF
  Circle(x_twin_A, y_center), 10, dark,,,1                            'blue emitter A moving rightward 0.5 c.
  Paint( x_twin_A, y_center), dark, dark

  x_coord = x_twin_A + 10 * lambda                                    'blue clock A' moving rightward 0.5 c.
  Circle(x_coord, y_center + y_clock), 40, blue,,,1
  Paint( x_coord, y_center + y_clock), blue, blue
  Circle(x_coord, y_center + y_clock), 31, white,,,1                    
  Paint( x_coord, y_center + y_clock), white, white
  Circle(x_coord, y_center + y_clock), 5, dark,,,1                    
  Paint( x_coord, y_center + y_clock), dark, dark

  Circle(x_twin_B, y_center - y_clock), 40, 2,,,1 / g_Lorentz         'red clock B moving leftward 0.5 c.
  Paint( x_twin_B, y_center - y_clock), red, 2
  Circle(x_twin_B, y_center - y_clock), 31, white,,,1 / g_Lorentz
  Paint( x_twin_B, y_center - y_clock), white, white
  Circle(x_twin_B, y_center - y_clock), 5, dark,,,1 / g_Lorentz
  Paint( x_twin_B, y_center - y_clock), dark, dark

  Line(x_twin_B - 4, y_center)-(x_twin_B + 4, y_center - y_clock + 40), red, BF
  Circle(x_twin_B, y_center), 10, dark,,,1                            'red emitter B moving leftward 0.5 c.
  Paint( x_twin_B, y_center), dark, dark

  Circle(x_twin_B_prime, y_center - y_clock), 40, red,,,1 / g_Lorentz 'red twin B' moving leftward 0.5 c.
  Paint( x_twin_B_prime, y_center - y_clock), red, red
  Circle(x_twin_B_prime, y_center - y_clock), 31, white,,,1 / g_Lorentz
  Paint( x_twin_B_prime, y_center - y_clock), white, white
  Circle(x_twin_B_prime, y_center - y_clock), 5, dark,,,1 / g_Lorentz
  Paint( x_twin_B_prime, y_center - y_clock), dark, dark

  For t_time =  - pi / 2 To 2 * pi - pi / 2 Step 2 * pi / 20          '20 clock marks.
    x = 27 * Cos(t_time) + x_twin_A
    y = 27 * Sin(t_time) + y_center + y_clock
    Line(x-2, y-2)-(x+2, y+2), dark, bf                               'blue clock A.
    Line(x + 10 * lambda-2, y-2)-(x + 10 * lambda+2, y+2), dark, bf   'blue clock A'.
    x = g_Lorentz * 27 * Cos(t_time) + x_twin_B
    y = 27 * Sin(t_time) + y_center - y_clock
    Line(x-2, y-2)-(x+1, y+2), dark, bf                               'red clock B.
    x = g_Lorentz * 27 * Cos(t_time) + x_twin_B_prime
    Line(x-2, y-2)-(x+1, y+2), dark, bf                               'red clock B'.
  Next

' IMPORTANT
' Twins A and B' proceed to the clock synchronization when they meet on the same x coordinate.
' The distance being negligible, both clocks are correctlyreset to 0 second in spite of B's slower clock.
' This is why the absolute time can be deduced from the beta speed and the distance to each other.
' Then A and A' proceed to a more complex clock synchronization in spite of the 10 light-second distance.
' The procedure works correctly because they are stationary. There is no Doppler effect. 
' However, the same procedure between B' and B ends up with a time shift because of the Doppler effect,
' and also because they are unaware that distance to each other is actually 8.66 light-seconds.

  t_time = (x_twin_B_prime - x_twin_A) / beta / lambda * 2 * pi / 20  '20 steps.
  x = 20 * Cos(t_time - pi / 2) + x_twin_A                            '20 is the needle length.
  y = 20 * Sin(t_time - pi / 2) + y_center + y_clock
  Line(x_twin_A, y_center + y_clock)-(x, y), dark                     'rotating the A blue clock needle.
  Line(x_twin_A+1, y_center + y_clock)-(x+1, y), dark
  Line(x_twin_A-1, y_center + y_clock)-(x-1, y), dark
  Line(x_twin_A, y_center + y_clock + 1)-(x, y + 1), dark
  Line(x_twin_A, y_center + y_clock - 1)-(x, y - 1), dark
  
  Line(x_twin_A + 10 * lambda, y_center + y_clock)-(x + 10 * lambda, y), dark 'rotating the A' blue clock needle.
  Line(x_twin_A + 10 * lambda+1, y_center + y_clock)-(x + 10 * lambda+1, y), dark
  Line(x_twin_A + 10 * lambda-1, y_center + y_clock)-(x + 10 * lambda-1, y), dark
  Line(x_twin_A + 10 * lambda, y_center + y_clock + 1)-(x + 10 * lambda, y + 1), dark
  Line(x_twin_A + 10 * lambda, y_center + y_clock - 1)-(x + 10 * lambda, y - 1), dark

  t_time = g_Lorentz * t_time - pi / 2                                'slower time for twins B and B'.
  x = g_Lorentz * 20 * Cos(t_time) + x_twin_B_prime                   'shorter x length for twins B and B'.
  y =             20 * Sin(t_time) + y_center - y_clock
  Line(x_twin_B_prime,   y_center - y_clock)-(  x,   y),   dark       'rotating the B' red clock needle.
  Line(x_twin_B_prime+1, y_center - y_clock)-(  x+1, y),   dark
  Line(x_twin_B_prime-1, y_center - y_clock)-(  x-1, y),   dark
  Line(x_twin_B_prime,   y_center - y_clock+1)-(x,   y+1), dark
  Line(x_twin_B_prime,   y_center - y_clock-1)-(x,   y-1), dark

  t_time =  t_time - .25 * (2 * pi)                                   'time shift = +5 sec for B.
  x = g_Lorentz * 20 * Cos(t_time) + x_twin_B
  y =             20 * Sin(t_time) + y_center - y_clock
  Line(x_twin_B,   y_center - y_clock)-(  x,   y),   dark             'rotating the B red clock needle.
  Line(x_twin_B+1, y_center - y_clock)-(  x+1, y),   dark
  Line(x_twin_B-1, y_center - y_clock)-(  x-1, y),   dark
  Line(x_twin_B,   y_center - y_clock+1)-(x,   y+1), dark
  Line(x_twin_B,   y_center - y_clock-1)-(x,   y-1), dark
  
  Line(x_twin_A, y_center + 132)-(10 * lambda + x_twin_A, y_center + 140), white, bf 'moving scales.
  For x = 0 To 9 * lambda Step 2 * lambda
    Line(x + x_twin_A, y_center + 132)-(x + x_twin_A + lambda, y_center + 140), blue, bf
  Next

  Line(x_twin_B - g_Lorentz * 10 * lambda, y_center - 132)-(x_twin_B, y_center - 140), white, bf
  For x = x_twin_B - g_Lorentz * 10 * lambda To x_twin_B - lambda Step g_Lorentz * 2 * lambda
    Line(x, y_center - 132)-(x + g_Lorentz * lambda, y_center - 140), red, bf
  Next
  
  Line(x_origin, y_center - 158)-(x_origin + 10 * lambda, y_center - 158), white 'upper stationary scale.
  Line(x_origin + 10 * g_Lorentz * lambda, y_center - 141)-(x_origin + 10 * g_Lorentz * lambda, y_center - 158), white
  For x = x_origin To x_origin + 10 * lambda Step lambda
    Line(x, y_center - 148)-(x, y_center - 158), white
  Next

  Line(x_origin, y_center + 158)-(x_origin + 10 * lambda, y_center + 158), white 'lower stationary scale.
  Line(x_origin + 10 * g_Lorentz * lambda, y_center + 141)-(x_origin + 10 * g_Lorentz * lambda, y_center + 158), white
  For x = x_origin To x_origin + 10 * lambda Step lambda
    Line(x, y_center + 148)-(x, y_center + 158), white
  Next

  Locate 1,  1:  ? "AA'"
  For x = 0 To 7: For y = 0 To 15                                     'blue twin identification A and A'
    If Point(x,y)    = -1 Then Pset(x + x_twin_A - 3, y + y_center - 7), white
    If Point(x,y)    = -1 Then Pset(x + x_twin_A - 3, y + y_center + y_clock + 7), dark
    If Point(x+8,y)  = -1 Then Pset(x + x_twin_A - 5 + 10 * lambda, y + y_center + y_clock + 7), dark
    If Point(x+16,y) = -1 Then Pset(x + x_twin_A + 3 + 10 * lambda, y + y_center + y_clock + 7), dark
  Next:Next

  Locate 1,  1:  ? "BB'"
  For x = 0 To 7: For y = 0 To 15                                     'red twin identification B and B'
    If Point(x,y)    = -1 Then Pset(x + x_twin_B - 3, y + y_center - 7), white
    If Point(x,y)    = -1 Then Pset(x + x_twin_B - 3, y + y_center - y_clock + 7), dark
    If Point(x+8,y)  = -1 Then Pset(x + x_twin_B_prime - 5, y + y_center - y_clock + 7), dark
    If Point(x+16,y) = -1 Then Pset(x + x_twin_B_prime + 3, y + y_center - y_clock + 7), dark
  Next:Next

  Circle(x_alpha, y_center), 39, 2,,,1 / Sqr(1 - alpha ^ 2)           'central alpha absolute reference.
  Paint(x_alpha, y_center),dark, 2
  Locate 25, x_alpha / 8 - 2: Print "Alpha"
  Circle(x_alpha, y_center), 39, white,,,1 / Sqr(1 - alpha ^ 2)
  Color white, black                                                  'black for transparency.
  Locate 1, 1: ? "   "
  If x_scanner < 168 Or x_scanner > 208 Then
    Locate 10, 22: Print Using "##.##"; (x_twin_B_prime - x_twin_A) / lambda
    Locate 11, 22: Print Using "##.##"; (x_twin_B - x_twin_A) / lambda
  Else
    Locate 08, 22: Print " 8.66"
    Locate 09, 22: Print "10.00"
    Locate 15, 22: ? "-5"
    Locate 16, 22: ? " 0"
  End If
  
  If scanner = 0 Then                                                 'preferred twin according to scanner choice.
    Locate 18, 11
    If x_scanner > 79 And x_scanner < 88 Then Print "B" Else Print "A"
  End If

  Locate 36, 45
  t_time = (x_twin_B_prime - x_twin_A) / beta / lambda                'time based on beta speed and A to B' distance.
  If x_scanner > x_origin - 21 And x_scanner < x_origin + 21 Then
       Print Using "###.#"; 0
  Else Print Using "###.#"; t_time
  End If


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
  
  Line(x_scanner,0)-(x_scanner,y_height), white                       'scanner.
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
' UNMOVING WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Wave_Emitter()

  If pulse_A = 0 Then Exit Sub                                        'no pulsation.
  t_time = c_speed * iteration * 2 * pi / lambda                      '.577 fine tuning to mach the real wave speed.
  amplitude = Abs(Sin(t_time))

' Only one half of the amplitude must be applied in order to start or
' stop pulsating. Otherwise, a desequilibrium occurs and the average
' amplitude becomes positive or negative. The best way to achieve this
' is to start or stop pulsating only when amplitude reaches a maximum.
' pulse_A = 1: waiting maximum amplitude in order to start pulsating.
' pulse_A = 3: waiting maximum amplitude in order to stop pulsating.

  If pulse_A = 1 Then
    If amplitude > .99 Then pulse_A = 2 Else Exit Sub
  Elseif pulse_A = 3 Then
    If amplitude > .99 Then
      pulse_A = 0: Exit Sub
    End If
  End If

  potential = Sin(t_time)
  For x = -lambda / 4 To lambda / 4                                   'stationary emitter.
    x_squared = x^2
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
      If distance < lambda / 4 Then
        radian = 2 * pi * distance / lambda
        trend(x_twin_A + x, y_center + y) = trend(x_twin_A + x, y_center + y) + potential * Cos(radian)
      End If
    Next
  Next
End Sub


'*********************************************************************
' MOVING WAVE GENERATOR - DOPPLER EFFECT USING MY
' REVERSED VERSION OF THE LORENTZ TRANSFORMATIONS.
'*********************************************************************

Sub Wave_Emitter_Doppler()                                            'moving emitter (twin B).

  If pulse_B = 0 Then Exit Sub                                        'no pulsation.
  t_time = c_speed * iteration * 2 * pi / lambda                      'absolute time in radians (according to 2 * pi).
  x_coord = 0                                                         'at the center in order to check the 1/4 or 3/4 phase.
  t_prime = g_Lorentz * t_time - beta * x_coord                       'more simply, at the center: t_prime = g_Lorentz * t_time
  amplitude = Abs(Sin(t_prime))

' Start or stop emitting when amplitude is maximum, hence when the phase reaches 1/4 or 3/4.
  If pulse_B = 1 Then
    If amplitude > .99 Then pulse_B = 2 Else Exit Sub                 'set .9 instead of .99 for shorter wavelength.
  Elseif pulse_B = 3 Then
    If amplitude > .99 Then
      pulse_B = 0: Exit Sub
    End If
  End If

  previous = 400
  t_time = c_speed * iteration / lambda                               'absolute time in wave period units.
  For x = -.25 * g_Lorentz * lambda To .25 * g_Lorentz * lambda
    x_squared = (x / g_Lorentz)^2                                     'absolute axial distance squared in pixel units.
    x_coord = x / g_Lorentz / lambda                                  'Lorentz's x coordinate in wavelength units.

'*************************************************************
' Below is my reversed version of the Lorentz transformations.
'*************************************************************

    x_prime = g_Lorentz * x_coord + beta * t_time                     'Lorentz's x' coordinate in lambda units.
    t_prime = g_Lorentz * t_time  - beta * x_coord                    'Lorentz's t' time in wave period units.
    
    x_prime = x_twin_A - move_frame + x_prime * lambda                'Lorentz's x' coordinate in pixels for twin B, relative to twin A.
    t_prime = t_prime * 2 * pi                                        'Lorentz's t' time in radians.
    potential = Sin(t_prime)
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)                                 'absolute distance in pixel units.
      If distance < .25 * lambda Then
        curve = Cos(2 * pi * distance / lambda)
        trend(x_prime, y_center + y) = trend(x_prime, y_center + y) + potential * curve
        If Int(y) = 0 Then                                            'displaying the impulse curve.
          Line(x_prime, y + 400 - 100 * potential * curve)-(x_prime, previous), black 
          previous = y + 400 - 100 * potential * curve
        End If
      End If
    Next
  Next
  Line(x_prime - 1, previous)-(x_prime - 1, 400), black 
End Sub
