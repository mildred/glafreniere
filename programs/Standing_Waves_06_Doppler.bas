Width 80,20:Color 0,15:Cls:?
? " by Gabriel LaFreniere.":?:?
? " This is a FreeBasic program.":?
? " The FreeBasic compiler ver. 0.20.0b (2008) for Windows is available here:":?
? " http://www.freebasic.net/index.php/download":?:?
? " This program is still compatible with previous compilers.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?

' Gosub commands are not supported any more.
' All variables must be declared.
' Subs are in alphabetical order. Press F2 and double-click "Subs"
' to display the list. Then double-click the Sub name.

Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Linear_Emitters()
Declare Sub Wave_Display()

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(120,120,120)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = -1966081
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Dim Shared As Integer r, g, b, j, x, y, x_width = 1280, y_height = 91
Dim Shared As Integer x_start, x_stop, y_start, y_start_2, y_stop, y_curve, y_curve_2
Dim Shared As Integer x_previous, x_lambda, x_squared, x_emitter_1, x_emitter_2
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2
Dim Shared As Integer iteration, lambda, previous, bitmap, x_screen, x_center, y_center
Dim Shared As Integer frame, skipped_frames, line_number, click, x_mouse, y_mouse, wheel
Dim Shared As Single radian, wave_speed, brightness, lambda_1a, lambda_2a, lambda_1b, lambda_2b
Dim Shared As Single alpha, g_alpha, beta, g_Lorentz, ratio, lambda_prime_a, lambda_prime_b
Dim Shared As Single orthogonal, diagonal, amplitude, phase, distance, Doppler
Dim Shared As Single x_coord, y_coord, t_time, x_prime, t_prime, image(0 To 260, 213 To 313)
Dim Shared As Single past(-11 To x_width+11), present(-11 To x_width+11), trend(-11 To x_width + 11)
Dim Shared As Single past2(-11 To x_width+11), present2(-11 To x_width+11), trend2(-11 To x_width + 11)
Dim Shared As String in_key, display, line13, line14, line15, line16, line17, line18, line19, line20
Dim Shared As String line44, line45, line46, line47, line48, bitmap_number, file
visible_page = 0: work_page = 1: matrix_page = 2: lambda = 100: bitmap = 0
brightness = 1: beta = .5
Screen 21,24,3
Initialization()


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  For frame = 0 To skipped_frames

' ******************************************************************** "THE PAST IS A GUIDE TO THE FUTURE"
' JOCELYN MARCOTTE'S 1-D VIRTUAL WAVE ALGORITHM (CREATED JANUARY 2006). IT IS THE SIMPLEST ALGORITHM EVER.
' ********************************************************************
    For x = x_start To x_stop                                         'updating amplitude states.
      past(x)  = present(x)                                           'previous amplitude.
      present(x) = trend(x)                                           'present amplitude (energy is amplitude squared).
      past2(x)  = present2(x)
      present2(x) = trend2(x)
    Next
    For x = x_start To x_stop 
      trend(x) = present(x-1) + present(x+1) - past(x)                'trend extrapolation.
      trend2(x) = present2(x-1) + present2(x+1) - past2(x)
    Next
' ********************************************************************
' END OF ALGORITHM.
' ********************************************************************

    trend(x_start) = present(x_start + 1): trend(x_start - 1) = 0     'full damping (left end).
    trend(x_stop)  = present(x_stop  - 1): trend(x_stop  + 1) = 0     'full damping (right end).
    trend2(x_start) = present2(x_start + 1): trend2(x_start - 1) = 0
    trend2(x_stop)  = present2(x_stop  - 1): trend2(x_stop  + 1) = 0
    Linear_Emitters()                                                 'impulses.
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    iteration += 1
    If iteration > 1400 Then Sleep: End
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
    Sleep 1
  Next
  Wave_Display()                                                      'skip other frames.
  If y_mouse < 186 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And iteration > 10 Then
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
    If bitmap > 1000 Then End
  End If
Loop

'***********************************************************************************************************************
' END OF MAIN LOOP.
'***********************************************************************************************************************


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Windowtitle " The Difference Between Regular and Relativistic Doppler Shifts"
  skipped_frames = 1                                                  'select 0 for smoother but slower waves.
  iteration = 0
  x_center = 1280 / 2
  y_center = 365
  x_start = -10
  x_stop = x_width + 10
  y_start = 120
  y_stop = y_start + y_height
  y_curve = y_stop + 54
  y_curve_2 = y_curve + 200
  y_start_2 = y_curve_2 + 54
  x_emitter_1 = 47
  g_Lorentz = Sqr(1 - beta^2)
  lambda_1a = lambda * (1 - beta)                                     'wavelength forward regular Doppler.
  lambda_2a = lambda * (1 + beta)                                     'wavelength backward regular Doppler.
  lambda_prime_a = lambda * g_Lorentz^2                               'wavelength compression, regular Doppler.
  lambda_1b = lambda * (1 - beta) / g_Lorentz                         'wavelength forward relativistic Doppler.
  lambda_2b = lambda * (1 + beta) / g_Lorentz                         'wavelength backward relativistic Doppler.
  lambda_prime_b = lambda * g_Lorentz                                 'wavelength compression, relativistic Doppler.
  For x = x_start - 1 To x_stop +1                                    'erasing previous data.
    past(x) = 0
    present(x) = 0
    trend(x) = 0
    past2(x) = 0
    present2(x) = 0
    trend2(x) = 0
  Next
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Bload "Standing_Waves_06_Doppler.bmp"
  Locate 50, 2: Print "               Emitter speed beta ";
  Print Using " #.######"; beta
  Locate 51, 2: Print "   Lorentz's compression factor g ";
  Print Using " #.######";  g_Lorentz
  Locate 53, 2: Print "          Forward regular Doppler "; lambda_1a
  Locate 54, 2: Print "         Backward regular Doppler "; lambda_2a
  Locate 55, 2: Print "     Lambda prime regular Doppler "; lambda_prime_a
  Locate 57, 2: Print "     Forward relativistic Doppler "; lambda_1b
  Locate 58, 2: Print "    Backward relativistic Doppler "; lambda_2b
  Locate 59, 2: Print "Lambda prime relativistic Doppler "; lambda_prime_b
  Locate 61, 2: Print "                 phase wave speed ";: Print Using " #.######";  1 / beta

  Color dark_gray
  Locate 63, 3:  ? "Thanks to the creators of FreeBASIC."
  Locate 64, 3:  ? "Gabriel LaFreniere  glafreniere.com";
  Locate 63,121: ? "September 3, 2010. This program may be"
  Locate 64,121: ? "freely distributed, copied or modified.";
  Color green_text
  line44 = " Lambda (Wavelength)                  "
  line45 = " Brightness Press [ + - = ]           "
  line46 = "                                      "
  line47 = " I- Initialize.                       "
  line48 = " Press Esc. to Quit.                  "
  Locate 47, 46: ? line47
  Locate 48, 46: ? line48;
  Locate 47, 72: ? "P- Pause."
  For x = 100 To 260                                                  'memorizing the moving emitters.
    For y = 217 To 309
      image(x, y) = Point(x, y)
    Next
  Next
  Line(100, 217)-(260, 309), background, bf                           'erasing the moving emitters.
  Line(20, 104)-(20 + 2 * lambda_2a, 105), black, b                   'upper left wavelength scale.
  For x = 20 To 2 * lambda_2a + 20 Step lambda_2a
    Line(x-1, 105)-(x+1, y_start - 2), black, bf
  Next
  For x = 20 + lambda_2a / 2 To 2 * lambda_2a + 20 Step lambda_2a
    Line(x-1, 105)-(x+1, y_start - 7), black, bf
  Next
  Line(956, 104)-(956 + 6 * lambda_1a, 105), black, b                 'upper right wavelength scale.
  For x = 956 To 956 + 6 * lambda_1a Step lambda_1a
    Line(x-1, 105)-(x+1, y_start - 2), black, bf
  Next
  For x = 956 + lambda_1a / 2 To 956 + 6 * lambda_1a Step lambda_1a
    Line(x-1, 105)-(x+1, y_start - 7), black, bf
  Next


  Line(20, y_start_2 + 108)-(20 + 2 * lambda_2b, y_start_2 + 109), black, b 'lower left wavelength scale.
  For x = 20 To 2 * lambda_2b + 20 Step lambda_2b
    Line(x-1, y_start_2 + 93)-(x+1, y_start_2 + 108), black, bf
  Next
  For x = 20 + .5 * lambda_2b To 2 * lambda_2b Step lambda_2b
    Line(x-1, y_start_2 + 101)-(x+1, y_start_2 + 108), black, bf
  Next
  
  Line(x_width - 22 - 5 * lambda_1b, y_start_2 + 108)-(x_width - 20, y_start_2 + 109), black, b 'lower right wavelength scale.
  For x = x_width - 22 - 5 * lambda_1b To x_width - 20 Step lambda_1b
    Line(x-1, y_start_2 + 93)-(x+1, y_start_2 + 108), black, bf
  Next
  For x = x_width - 22 - 4.5 * lambda_1b To x_width - 20 Step lambda_1b
    Line(x-1, y_start_2 + 101)-(x+1, y_start_2 + 108), black, bf
  Next

  Line(x_width - 107 - 4 * lambda_prime_a, y_center - 32)-(x_width - 107, y_center - 31), black, b'upper wavelength compression scale.
  For x = x_width - 107 - 4 * lambda_prime_a To x_width - 105 Step lambda_prime_a
    Line(x-1, y_center - 32)-(x+1, y_center - 46), black, bf
  Next
  For x = x_width - 107 - 3.5 * lambda_prime_a To x_width - 105 Step lambda_prime_a
    Line(x-1, y_center - 32)-(x+1, y_center - 39), black, bf
  Next
  Line(x_width - 102 - 4 * lambda_prime_b, y_center + 32)-(x_width - 100, y_center + 31), black, b'lower wavelength compression scale.
  For x = x_width - 102 - 4 * lambda_prime_b To x_width - 100 Step lambda_prime_b
    Line(x-1, y_center + 32)-(x+1, y_center + 46), black, bf
  Next
  For x = x_width - 102 - 3.5 * lambda_prime_b To x_width - 104 Step lambda_prime_b
    Line(x-1, y_center + 32)-(x+1, y_center + 39), black, bf
  Next

End Sub


'*********************************************************************
' KEYBOARD MANAGEMENT.
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
  Case "P": line_number = 30: click = 1                               'pause.
            Screenset work_page, work_page: Color red
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
' EMITTERS - STRAIGHT WAVES - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Linear_Emitters()
  x_emitter_1 = Int((iteration + 100) * beta)
  x_emitter_2 = Int(x_emitter_1 + 6.6 * lambda_prime_a)
  If iteration < 10 Then Exit Sub
  phase = (iteration - 9.5) * 2 * pi / lambda
  amplitude = 1900 * Cos(phase) / lambda ^ 2
  For x = -lambda / 8 To lambda / 8
    trend(x + x_emitter_1) = trend(x + x_emitter_1) + amplitude
  Next
  amplitude = 1300 * Cos(phase) / lambda ^ 2
  For x = -lambda / 8 To lambda / 8
    trend(x + x_emitter_2) = trend(x + x_emitter_2) + amplitude
  Next

' Below, the relativistic Doppler shift as well as the emitter
' motion are obtained solely using my reversed version of the
' Lorentz transformations. Please note that both the x coordinates
' and the t time refer to the regular non relativistic wavelength.

  t_time = iteration / lambda
  For x = -lambda / 4 To lambda / 4
    radian = 2 * pi * x / lambda
    x_coord = x / lambda
    x_prime = g_Lorentz * x_coord + beta * t_time
    t_prime = g_Lorentz * t_time  - beta * x_coord
    x_lambda = Int(x_prime * lambda)
    If x_lambda > x_previous Then 
      trend2(x_lambda + 54)  = trend2(x_lambda + 54)  + .125 * Cos(2 * pi * t_prime - .5) * Cos(radian)
      trend2(x_lambda + 548) = trend2(x_lambda + 548) + .125 * Cos(2 * pi * t_prime - .5) * Cos(radian)
      If frame = 0 Then Pset(x_lambda + 54, y_curve_2 - 15 * Cos(2 * pi * t_prime - .5) * Cos(radian)), red
    End If
    x_previous = x_lambda
  Next
End Sub


'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

Sub Mouse_Management()
  Screenset work_page
  Color green_text, background
  Locate 47, 46: ? line47
  Locate 48, 46: ? line48;
  Color green_text, white
  Locate line_number, 46

' MOUSE CLICK PROCESSING *********************************************
  Select Case line_number
  Case 47
      ? line47;
      If click > 0 Then Initialization()
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
  If line_number = 1 Then Return                                      'show background (hide waves).
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  For x = 0 To x_width - 1
    luminance_1 = brightness * Abs(7 * present(x))
    b = luminance_1 / 2
    If b > 255 Then b = 255
    If luminance_1 > 255 Then
      luminance_2 = luminance_1 - 255
      If luminance_2 > 255 Then luminance_2 = 255
      luminance_1 = 255
    Else luminance_2 = 0
    End If
    If present(x) > 0 Then                                            'using complementary magenta and emerald green.
      r = luminance_2
      g = luminance_1
    Else
      r = luminance_1
      g = luminance_2
    End If
    Line(x, y_start)-(x, y_start + y_height), Rgb(r,g,b)
  Next


  For x = 0 To x_width - 1
    luminance_1 = brightness * Abs(7 * present2(x))
    b = luminance_1 / 2
    If b > 255 Then b = 255
    If luminance_1 > 255 Then
      luminance_2 = luminance_1 - 255
      If luminance_2 > 255 Then luminance_2 = 255
      luminance_1 = 255
    Else luminance_2 = 0
    End If
    If present2(x) > 0 Then                                            'using complementary magenta and emerald green.
      r = luminance_2
      g = luminance_1
    Else
      r = luminance_1
      g = luminance_2
    End If
    Line(x, y_start_2)-(x, y_start_2 + y_height), Rgb(r,g,b)
  Next


  Line(x_emitter_1, y_curve - 52)-(x_emitter_2, y_curve + 52), dark_gray, bf'upper standing wave system.
  For x = x_emitter_1 + 25 To x_emitter_2 + 25
    radian = 2 * pi * (x - x_emitter_1) / lambda_prime_a              'standing wave envelope.
    amplitude = 49 * Sin(radian + pi / 2)
    Line(x - 25, y_curve - amplitude)-(x - 25, y_curve + amplitude), white
  Next
  Line(0, y_curve)-(x_width, y_curve), dark_gray
  previous = 0
  For x = 0 To x_width - 1                                            'axial amplitude graphics.
    amplitude = present(x)
    Line(x, y_curve - previous)-(x, y_curve - amplitude), black
    previous = amplitude
  Next

  Line(x_emitter_1, y_curve_2 - 52)-(x_emitter_2, y_curve_2 + 52), dark_gray, bf'lower standing wave system.
  For x = x_emitter_1 + 2 To x_emitter_2 + 2
    radian = 2 * pi * (x - x_emitter_1) / lambda_prime_b              'standing wave envelope.
    amplitude = 49 * Sin(radian + pi / 2)
    Line(x - 2, y_curve_2 - amplitude)-(x - 2, y_curve_2 + amplitude), white
  Next
  Line(0, y_curve_2)-(x_width, y_curve_2), dark_gray
  previous = 0
  For x = 0 To x_width - 1                                            'axial amplitude graphics.
    amplitude = present2(x)
    Line(x, y_curve_2 - previous)-(x, y_curve_2 - amplitude), black
    previous = amplitude
  Next

  For x = 102 To 175                                                  'printing A (left moving emitter).
    For y = 217 To 309
      If image(x, y) <> blue_sky Then Pset(x + x_emitter_1 - 139, y + 102), image(x, y)
    Next
  Next
  For x = 186 To 260                                                  'printing B (right moving emitter).
    For y = 217 To 309
      If image(x, y) <> blue_sky Then Pset(x + x_emitter_2 - 223, y + 102), image(x, y)
    Next
  Next
' Line(0, y_curve - 25)-(x_width - 1, y_curve + 25), black, b         'adjusting amplitude.
' Line(0, y_curve_2 - 25)-(x_width - 1, y_curve_2 + 25), black, b     'adjusting amplitude.
  Line(x_emitter_1-1, y_start)-(x_emitter_1+1, y_start + y_height), white, bf
  Line(x_emitter_1-1, y_curve - 52)-(x_emitter_1+1, y_curve + 52),  black, bf
  Line(x_emitter_2-1, y_start)-(x_emitter_2+1, y_start + y_height), white, bf
  Line(x_emitter_2-1, y_curve - 52)-(x_emitter_2+1, y_curve + 52),  black, bf
  Line(x_emitter_1-1, y_start_2)-(x_emitter_1+1, y_start_2 + y_height), white, bf
  Line(x_emitter_1-1, y_curve_2 - 52)-(x_emitter_1+1, y_curve_2 + 52),  black, bf
  Line(x_emitter_2-1, y_start_2)-(x_emitter_2+1, y_start_2 + y_height), white, bf
  Line(x_emitter_2-1, y_curve_2 - 52)-(x_emitter_2+1, y_curve_2 + 52),  black, bf

  Color black, background
  Locate 47, 2: Print "Iteration"; iteration
End Sub

