Width 80,20:Color 0,15:Cls:?
? " Created August 8, 2010 by Gabriel LaFreniere.":?:?
? " This is a FreeBasic program.":?
? " The FreeBasic compiler ver. 0.20.0b (2008) for Windows is available here:":?
? " http://www.freebasic.net/index.php/download":?:?
? " This program is still compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?

' Gosub commands are not supported any more.
' All variables must be declared.
' Subs are in alphabetical order. Press F2 and double-click "Subs"
' to display the list. Then double-click the Sub name.

Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Wave_Display()
Declare Sub Linear_Transmitters()

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(120,120,120)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Dim Shared As Integer r, g, b, j, x, y, x_width = 1280, y_height = 100
Dim Shared As Integer x_start, x_stop, y_start, y_stop, x_center, y_curve
Dim Shared As Integer x_coord, y_coord, x_squared, x_emitter_1, x_emitter_2
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2
Dim Shared As Integer iteration, previous, bitmap
Dim Shared As Integer frame, skipped_frames, line_number, click, x_mouse, y_mouse, wheel
Dim Shared As Single amplitude, phase, distance, radian, wave_speed, brightness
Dim Shared As Single orthogonal, diagonal, alpha, g_alpha, beta, g_beta, ratio
Dim Shared As Single lambda, lambda_prime, lambda_f, lambda_b
Dim Shared As Single past(-11 To x_width+11), present(-11 To x_width+11), trend(-11 To x_width + 11)
Dim Shared As String in_key, display, line13, line14, line15, line16, line17, line18, line19, line20
Dim Shared As String line44, line45, line46, line47, line48, bitmap_number, file
visible_page = 0: work_page = 1: matrix_page = 2: lambda_f = 100: beta = .1980198
brightness = 1: bitmap = 0
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
    Next
    For x = x_start To x_stop 
      trend(x) = present(x-1) + present(x+1) - past(x)                'trend extrapolation.
    Next
' ********************************************************************
' END OF ALGORITHM.
' ********************************************************************

    trend(x_start) = present(x_start + 1): trend(x_start - 1) = 0     'full damping (left end).
    trend(x_stop)  = present(x_stop  - 1): trend(x_stop  + 1) = 0     'full damping (right end).

    Linear_Transmitters()                                             'impulses.
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If frame = 0 Then Wave_Display()                                  'skip other frames.
    iteration += 1
    If iteration > 1800 Then Sleep: End
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next
  If y_mouse < 186 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And iteration > 400 Then
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
    If bitmap > 1500 Then End
  End If

Loop

'***********************************************************************************************************************
' END OF MAIN LOOP.
'***********************************************************************************************************************


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Windowtitle " Ivanov's Standing Waves"
  x_center = .5 * x_width
  skipped_frames = 1                                                  'select 0 for smoother but slower waves.
  iteration = 0
  x_start = -10
  x_stop = x_width + 10
  x_emitter_1 = 50
  x_emitter_2 = x_width - 50 
  g_beta = Sqr(1 - beta^2)
  lambda_b = lambda_f * (1 + beta) / g_beta
  ratio = lambda_b / lambda_f
  alpha = (ratio - 1) / (ratio + 1)
  g_alpha = Sqr(1 - alpha^2)
  lambda_prime = g_alpha * Sqr(lambda_f * lambda_b)
  y_start = 120
  y_stop = y_height + 120
  y_curve = y_stop + 75
  For x = 0 To x_width                                                'erasing previous data.
    past(x) = 0
    present(x) = 0
    trend(x) = 0
  Next
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Bload "Standing_Waves_01_Ivanov.bmp"

  Locate 50, 2: Print "            lambda forward ";  lambda_f
  Locate 51, 2: Print "           lambda backward ";: Print Using " ###.####"; lambda_b
  Locate 52, 2: Print "            geometric mean ";: Print Using " ###.####"; Sqr(lambda_f * lambda_b)
  Locate 53, 2: Print "           arithmetic mean ";: Print Using " ###.####"; (lambda_f + lambda_b) / 2
  Locate 54, 2: Print "          wavelength ratio ";  ratio
  Locate 55, 2: Print "       standing wave speed ";: Print Using "##.######"; alpha;
  Locate 56, 2: Print "   Lorentz's contraction g ";: Print Using "##.######";  g_alpha
  Locate 57, 2: Print " standing wave contraction ";: Print Using " ###.#####";  g_alpha * Sqr(lambda_f * lambda_b); g_alpha^2 * (lambda_f + lambda_b) / 2
  Locate 58, 2: Print "          phase wave speed ";: Print Using "###.#####";  1 / alpha

  Color dark_gray
  Locate 63, 3:  ? "Thanks to the creators of FreeBASIC."
  Locate 64, 3:  ? "Gabriel LaFreniere  glafreniere.com";
  Locate 63,121: ? "August 30, 2010. This program may be"
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
  Line(25, 103)-(25 + 2 * lambda_f, 104), black, b                    'wavelength scale.
  For x = 25 To 2 * lambda_f + 25 Step lambda_f
    Line(x-1, 104)-(x+1, y_start - 4), black, bf
  Next
  For x = 25 + lambda_f / 2 To 2 * lambda_f + 25 Step lambda_f
    Line(x-1, 104)-(x+1, y_start - 9), black, bf
  Next
  Line(x_center - 3 * lambda_prime, 103)-(x_center + 3 * lambda_prime, 104), black, b
  For x = x_center - 3 * lambda_prime To x_center + 3 * lambda_prime + 2 Step lambda_prime
    Line(x-1, 104)-(x+1, y_start - 4), black, bf
  Next
  For x = x_center - 3 * lambda_prime + lambda_prime / 2 To x_center + 3 * lambda_prime + 2 Step lambda_prime
    Line(x-1, 104)-(x+1, y_start - 9), black, bf
  Next
  Line(x_width - 25 - 2 * lambda_b, 103)-(x_width - 25, 104), black, b
  For x = x_width - 25 - 2 * lambda_b To x_width - 25 Step lambda_b
    Line(x-1, 104)-(x+1, y_start - 4), black, bf
  Next
  For x = x_width - 25 - 2 * lambda_b + lambda_b / 2 To x_width - 25 Step lambda_b
    Line(x-1, 104)-(x+1, y_start - 9), black, bf
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
' TRANSMITTERS - STRAIGHT WAVES - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Linear_Transmitters()
  phase = (iteration + .5) * 2 * pi / lambda_f
  amplitude = 1350 * Cos(phase) / lambda_f ^ 2                        'impulse zone is smaller for shorter wavelength.
  For x = -lambda_f / 8 To lambda_f / 8
    trend(x + x_emitter_1) = trend(x + x_emitter_1) + amplitude
  Next
  phase = (iteration + .5) * 2 * pi / lambda_b
  amplitude = 1350 * Cos(phase) / lambda_b ^ 2                        'impulse zone is smaller for shorter wavelength.
  For x = -lambda_b / 8 To lambda_b / 8
      trend(x + x_emitter_2) = trend(x + x_emitter_2) + amplitude
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

'*********************************************************************
' MOUSE CLICK PROCESSING.

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
    For y = y_start To y_start + 100
      Pset(x,y), Rgb(r,g,b)
    Next
  Next
  Line(x_center - 3 * lambda_prime - 1, y_curve - 52)-(x_center + 3 * lambda_prime + 1, y_curve + 52), dark_gray, bf
'  Line(0, y_curve - 25)-(x_width - 1, y_curve + 25), black, b        'adjusting amplitude.
  phase = 2 * pi * alpha * iteration / lambda_prime + 1.38
  For x = 0 To 6 * lambda_prime
    radian = 2 * pi * x / lambda_prime
    amplitude = 48 * Sin(phase - radian)
    Line(x + x_center - 3 * lambda_prime, y_curve - amplitude)-(x + x_center - 3 * lambda_prime, y_curve + amplitude), white
  Next
  Line(0, y_curve)-(x_width, y_curve), dark_gray
  previous = 0
  For x = 0 To x_width - 1                                            'axial amplitude graphics.
    amplitude = present(x)
    Line(x, y_curve - previous)-(x, y_curve - amplitude), black
    previous = amplitude
  Next
  Line(x_emitter_1-1, y_start)-(x_emitter_1+1, y_start + y_height), white, bf
  Line(x_emitter_1-1, y_curve - 26)-(x_emitter_1+1, y_curve + 26),  black, bf
  Line(x_emitter_2-1, y_start)-(x_emitter_2+1, y_start + y_height), white, bf
  Line(x_emitter_2-1, y_curve - 26)-(x_emitter_2+1, y_curve + 26),  black, bf
  Color black, background
  Locate 47, 2: Print "Iteration"; iteration
End Sub

