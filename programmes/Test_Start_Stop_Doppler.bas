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

'Dim Shared As Integer x_screen = 1280, y_screen = 1024, x_width = 1279, y_height = 767
Dim Shared As Integer x_screen = 300, y_screen = 300, x_width = 299, y_height = 299
Dim Shared As Integer x_start, y_start, x_stop, y_stop, x_alpha, x_center, y_center, x_mouse, y_mouse
Dim Shared As Integer r, g, b, x, x_prime, y, scanner, x_scanner, x_previous, x_coord, y_coord, x_squared
Dim Shared As Integer r_previous(y_height), g_previous(y_height), b_previous(y_height)
Dim Shared As Integer x_twin_A, x_twin_B, x_twin_B_prime, y_prime, x_origin, y_clock
Dim Shared As Integer y_parabola, y_beam_splitter, x_emitter_1, x_scale, wave_display
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2, click, wheel, bitmap
Dim Shared As Integer iteration, pulses, pulse_A, pulse_B, lambda, target, damping_zone = 401
Dim Shared As Integer frame, skipped_frames, line_number, axial, OK, unit, scan(-4 To x_width + 4, y_height)

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
    If x_scanner = x_width + 2 And scanner = 1 Then
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
  If bitmap > 0 And x_scanner > -2 And iteration Mod 2 = 0 Then
'  If bitmap > 0 Then                                                  'set bitmap = 1 for bitmap sequence.
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
  Screen 20,24,3
  Windowtitle " The Time Scanner and the Doppler Effect - Jan. 09, 2010"
  Screenset matrix_page, matrix_page
  Color black, background: Cls
'  Line(0,0)-(1279,767), black, bf
' Line(0,0)-(100,15), blue, bf: Locate 2: Print Point(1,1): Print blue: Sleep: End 'color test.
  pulses = 15
  iteration = 28
  pulse_A = 0
  pulse_B = 1
  wave_display = 1
  If bitmap Then wave_display = 1
  lambda = 64
  brightness = .7
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
  x_alpha = 150
  x_twin_A = x_alpha - 43
  x_scanner = x_alpha - 322
  x_twin_B = x_twin_A
  x_origin = x_alpha
  y_center = .5 * y_height
  y_clock = 86
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
  Locate 2,  50: ? "Si la zone d'impulsion mesure un quart d'onde de rayon, il se produit"
  Locate 3,  50: ? "un retard de phase d'un quart d'onde … la suite d'une accumulation"
  Locate 4,  50: ? "d'‚nergie … l'int‚rieur de la zone. Ce programme montre qu'il faut"
  Locate 5,  50: ? "d'abord injecter de l'‚nergie pendant l'‚quivalent d'un quart de cycle"
  Locate 6,  50: ? "lors du d‚marrage pour ‚quilibrer l'‚nergie ‚mise. De la mˆme maniŠre,"
  Locate 7,  50: ? "au moment de cesser l'‚mission, il faut retrancher de l'‚nergie pendant"
  Locate 8,  50: ? "l'‚quivalent d'un quart de cycle pour ‚quilibrer l'‚nergie accumul‚e."
  Locate 9,  50: ? "Il suffit en pratique d'attendre que l'amplitude soit au maximum."
  Locate 11, 50: ? "Appuyer sur S pour arrˆter d'‚mettre ou recommencer … ‚mettre."
  Locate 13, 50: ? "Ce programme montre ‚galement que dans le cas o— il faut produire un"
  Locate 14, 50: ? "effet Doppler, on peut ‚quilibrer les ondes qui se propagent vers l'avant"
  Locate 15, 50: ? "avec celles qui se propagent vers l'arriŠre en accentuant le d‚calage"
  Locate 16, 50: ? "horaire des transformations de Lorentz. Il faut aussi ralentir la"
  Locate 17, 50: ? "fr‚quence de l'‚metteur et contracter la zone d'‚mission … l'aide du"
  Locate 18, 50: ? "facteur de contraction de Lorentz. C'est ce qui permet d'obtenir une"
  Locate 19, 50: ? "invariance de la longueur d'onde dans les directions transversales"
  Locate 20, 50: ? "selon les ‚quations de Lorentz: y'= y; z'= z."
  Locate 22, 50: ? "Appuyer sur Echap. pour quitter."
  Locate 44, 2:  ? "Thanks to the creators of FreeBASIC."
  Locate 45, 02: ? "Special thanks to Philippe Delmotte"
  Locate 46, 02: ? "and Jocelyn Marcotte, the creators"
  Locate 47, 02: ? "of this amazing virtual wave medium."
  Locate 48, 02: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47, 88: ? "January 9, 2010. This program may be"
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
  Case "A": line_number = 58: click = 1                               'don't scan.
  Case "B": line_number = 59: click = 1                               'don't scan.
  Case "W": line_number = 60: click = 1                               'add waves.
  Case "N": line_number = 61: click = 1                               'no waves.
  Case "I": Initialization()
  Case "P": line_number = 62: click = 1                               'pause.
  Case "S": If pulse_B = 0 Then pulse_B = 1                           'start pulsating.
            If pulse_B = 2 Then pulse_B = 3                           'stop pulsating.
  Case "+": brightness = brightness / Sqr(.5)                         'brighter.
            If brightness > 4   Then brightness = 4
  Case "-": brightness = brightness * Sqr(.5)                         'darker.
            If brightness < .25 Then brightness = .25
  Case "=": brightness = 1                                            'normal brightness.
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
  
  x_twin_B = x_twin_A  - move_frame + twins_distance
  Line(x_twin_B, y_center)-(x_twin_B, y_center - 2 * lambda - lambda / 8), white 'vertical wavelength scale.
  For y = y_center - lambda / 2 - lambda / 8 To y_center - 2 * lambda - lambda / 8 Step -lambda / 2
    Line(x_twin_B-4, y)-(x_twin_B+4, y), white
  Next

' Line(x_alpha, y_center)-(x_alpha, y_center - 2 * lambda - lambda / 8), white
  Line(x_alpha, y_center)-(x_alpha + 2 * lambda + lambda / 8, y_center), white'horizontal wavelength scale.
  For x = x_alpha + lambda / 2 + lambda / 8 To x_alpha + 2 * lambda + lambda / 8 Step lambda / 2
    Line(x, y_center - 4)-(x, y_center + 4), white
  Next

  Line(x_twin_B - lambda, 400)-(x_twin_B + lambda, 400), black        'graphics wavelength scale.
  For x = 0 To lambda Step lambda / 4
    Line(x_twin_B + x, 398)-(x_twin_B + x, 402), black
    Line(x_twin_B - x, 398)-(x_twin_B - x, 402), black
  Next

'*********************************************************************
' DISPLAYING THE WAVES - POSITIVE AMPLITUDE GREEN AND NEGATIVE RED.
'*********************************************************************

  For x = 0 To x_width: For y = 0 To y_height
    If wave_display Then
      luminance_1 = brightness * Abs(20 * trend(x,y))
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
  
  Line(x_scanner,0)-(x_scanner,y_height + 100), white                 'scanner.
  Color black, background
  Locate 40, 02: Print "Iteration... "; iteration
  Locate 41, 02: Print "Scanner..... "; x_scanner

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
  Exit Sub
  If pulse_A = 0 Then Exit Sub                                        'no pulsation.
  t_time = c_speed * iteration * 2 * pi / lambda                      '.577 fine tuning to mach the real wave speed.
  amplitude = Abs(Cos(t_time))

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

  amplitude = 1500 / lambda ^ 2
  potential = amplitude * Cos(t_time)
  For x = -lambda / 4 To lambda / 4                                   'stationary emitter.
    x_squared = x^2
    For y = -lambda / 4 To lambda / 4
      distance = Sqr(x_squared + y^2)
'     gaussian = pi ^ -((distance / lambda) ^ 2)                      'the normal distribution works for a stationary emitter only.
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

Sub Wave_Emitter_Doppler()

  If pulse_B = 0 Then Exit Sub                                        'no pulsation.
  t_time = c_speed * iteration * 2 * pi / lambda                      'standard time.
  t_prime = g_Lorentz * t_time' - beta * ((x_twin_B - x_twin_A) / lambda) 'Lorentz's reversed time equation: t' = g * t - beta * x
  amplitude = Abs(Sin(t_prime))

' Start or stop emitting when amplitude is maximum.
  If pulse_B = 1 Then
    If amplitude > .99 Then pulse_B = 2 Else Exit Sub                 'set .9 instead of .99 for shorter wavelength.
  Elseif pulse_B = 3 Then
    If amplitude > .99 Then
      pulse_B = 0: Exit Sub
    End If
  End If

  previous = 400
  amplitude = 1500 / lambda^2
  For x_prime = -.25 * g_Lorentz * lambda To .25 * g_Lorentz * lambda
    x_squared = (x_prime / g_Lorentz)^2
'   this is my reversed version of the Lorentz transformations: t' = g * t - b * x
'   please note that the time shift according to t' is 6 times exaggerated.
    t_prime = g_Lorentz * t_time - beta * (6 * x_prime / g_Lorentz / lambda)
    potential = amplitude * Sin(t_prime)
    For y = -.25 * lambda To .25 * lambda
      distance = Sqr(x_squared + y^2)
      If distance < .25 * lambda Then
        curve = Cos(2 * pi * distance / lambda)
        trend(x_twin_B + x_prime, y_center + y) = trend(x_twin_B + x_prime, y_center + y) + potential * curve
        If Int(y) = 0 Then                                            'displaying the impulse curve.
          Line(x_twin_B + x_prime, y + 400 - 100 * potential * curve)-(x_twin_B + x_prime, previous), black 
          previous = y + 400 - 100 * potential * curve
        End If
      End If
    Next
  Next
  Line(x_twin_B + x_prime - 1, previous)-(x_twin_B + x_prime - 1, 400), black 
End Sub
