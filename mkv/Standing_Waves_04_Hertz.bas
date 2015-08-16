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
Declare Sub Linear_Transmitters()
Declare Sub Moving_Screen()
Declare Sub Wave_Display()

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(120,120,120)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Dim Shared As Integer r, g, b, j, x, y, x_width = 1280, y_height = 100
Dim Shared As Integer x_start, x_stop, y_start, y_stop, x_center, y_curve
Dim Shared As Integer x_coord, y_coord, x_squared, x_emitter_1, x_emitter_2
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2
Dim Shared As Integer iteration, lambda, previous, bitmap, x_screen, x_previous
Dim Shared As Integer frame, skipped_frames, line_number, click, x_mouse, y_mouse, wheel
Dim Shared As Single amplitude, phase, distance, radian, wave_speed, brightness, lambda_1, lambda_2
Dim Shared As Single orthogonal, diagonal, alpha, g_Lorentz, ratio, lambda_prime_1, lambda_prime_2
Dim Shared As Single past(-11 To x_width+11), present(-11 To x_width+11), trend(-11 To x_width + 11)
Dim Shared As String in_key, display, line13, line14, line15, line16, line17, line18, line19, line20
Dim Shared As String line44, line45, line46, line47, line48, bitmap_number, file
visible_page = 0: work_page = 1: matrix_page = 2: lambda = 100: bitmap = 0
x_emitter_1 = 0: x_emitter_2 = x_width: brightness = 1: alpha = .2679492
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
    For x = x_start To x_stop                                             'updating amplitude states.
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
    Moving_Screen()
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If frame = 0 Then Wave_Display()                                  'skip other frames.
    iteration += 1
    If iteration > 2500 Then Sleep: End
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next
  If y_mouse < 186 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And iteration > 900 Then
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
  Sleep 1
Loop

'***********************************************************************************************************************
' END OF MAIN LOOP.
'***********************************************************************************************************************


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Windowtitle " The Hertz Experiment using a Moving Screen"
  x_center = .5 * x_width
  skipped_frames = 1                                                  'select 0 for smoother but slower waves.
  iteration = 0
  x_start = -10
  x_stop = x_width + 10
  y_start = 120
  y_stop = y_height + 120
  y_curve = y_stop + 60
  g_Lorentz = Sqr(1 - alpha^2)
  lambda_1 =  lambda * (1 + alpha) / (1 - alpha)
  lambda_2 =  lambda * (1 - alpha) / (1 + alpha)
  lambda_prime_1 = g_Lorentz * Sqr(lambda * lambda_1)
  lambda_prime_2 = g_Lorentz * Sqr(lambda * lambda_2)
  lambda_prime_1 = lambda * (1 + alpha)
  For x = x_start - 1 To x_stop +1                                    'erasing previous data.
    past(x) = 0
    present(x) = 0
    trend(x) = 0
  Next
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Bload "Standing_Waves_04_Hertz.bmp"
  Locate 50, 2: Print "screen speed alpha ";: Print Using " #.######";  alpha
  Locate 51, 2: Print "  Lorentz's factor "; g_Lorentz
  Locate 52, 2: Print "            lambda "; lambda
  Locate 53, 2: Print "          lambda 1 "; lambda_1
  Locate 54, 2: Print "          lambda 2 "; lambda_2
  Locate 55, 2: Print "    lambda prime 1 "; lambda_prime_1;: ? lambda * (1 + alpha)
  Locate 56, 2: Print "    lambda prime 2 "; lambda_prime_2;: ? lambda * (1 - alpha)
  Locate 57, 2: Print "  phase wave speed ";: Print Using " #.######";  1 / alpha

  Color dark_gray
  Locate 63, 3:  ? "Thanks to the creators of FreeBASIC."
  Locate 64, 3:  ? "Gabriel LaFreniere  glafreniere.com";
  Locate 63,121: ? "August 23, 2010. This program may be"
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
  Line(20, 103)-(20 + 2 * lambda, 104), black, b                      'wavelength scale.
  For x = 20 To 2 * lambda + 20 Step lambda
    Line(x-1, 104)-(x+1, y_start - 4), black, bf
  Next
  For x = 20 + lambda / 2 To 2 * lambda + 20 Step lambda
    Line(x-1, 104)-(x+1, y_start - 9), black, bf
  Next
  Line(x_width - 20 - 2 * lambda, 103)-(x_width - 20, 104), black, b
  For x = x_width - 20 - 2 * lambda To x_width - 20 Step lambda
    Line(x-1, 104)-(x+1, y_start - 4), black, bf
  Next
  For x = x_width - 20 - 2 * lambda + lambda / 2 To x_width - 20 Step lambda
    Line(x-1, 104)-(x+1, y_start - 9), black, bf
  Next
'  Line(x_center - 3 * lambda_prime, 103)-(x_center + 3 * lambda_prime, 104), black, b
'  For x = x_center - 3 * lambda_prime To x_center + 3 * lambda_prime + 2 Step lambda_prime
'    Line(x-1, 104)-(x+1, y_start - 4), black, bf
'  Next
'  For x = x_center - 3 * lambda_prime + lambda_prime / 2 To x_center + 3 * lambda_prime + 2 Step lambda_prime
'    Line(x-1, 104)-(x+1, y_start - 9), black, bf
'  Next
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
  phase = iteration * 2 * pi / lambda
  amplitude = 1360 * Cos(phase) / lambda ^ 2
  If iteration > 1000 Then
    For x = 0 To lambda / 4
      trend(x + x_emitter_1) = trend(x + x_emitter_1) + amplitude
    Next
  End If
  For x = -lambda / 4 To 0
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
' MOVING SCREEN
'*********************************************************************
Sub Moving_Screen()
  x_screen = Int(iteration * alpha)                              'the 1-D moving reflector needs buffering.
  ratio = (iteration * alpha) - x_screen
' locate 40,40: print x_screen;: print using "  #.###"; ratio: sleep 1000
  trend(x_screen) = 0
  trend(x_screen - 1) =  (1 - ratio * .46) * trend(x_screen - 1)
  trend(x_screen - 2) =  (1 - ratio * .37) * trend(x_screen - 2)
  trend(x_screen - 3) =  (1 - ratio * .29) * trend(x_screen - 3)
  trend(x_screen - 4) =  (1 - ratio * .22) * trend(x_screen - 4)
  trend(x_screen - 5) =  (1 - ratio * .16) * trend(x_screen - 5)
  trend(x_screen - 6) =  (1 - ratio * .11) * trend(x_screen - 6)
  trend(x_screen - 7) =  (1 - ratio * .07) * trend(x_screen - 7)
  trend(x_screen - 8) =  (1 - ratio * .04) * trend(x_screen - 8)
  trend(x_screen - 9) =  (1 - ratio * .02) * trend(x_screen - 9)
  trend(x_screen -10) =  (1 - ratio * .01) * trend(x_screen -10)

  trend(x_screen + 1) =  (1 - ratio * .46) * trend(x_screen + 1)
  trend(x_screen + 2) =  (1 - ratio * .37) * trend(x_screen + 2)
  trend(x_screen + 3) =  (1 - ratio * .29) * trend(x_screen + 3)
  trend(x_screen + 4) =  (1 - ratio * .22) * trend(x_screen + 4)
  trend(x_screen + 5) =  (1 - ratio * .16) * trend(x_screen + 5)
  trend(x_screen + 6) =  (1 - ratio * .11) * trend(x_screen + 6)
  trend(x_screen + 7) =  (1 - ratio * .07) * trend(x_screen + 7)
  trend(x_screen + 8) =  (1 - ratio * .04) * trend(x_screen + 8)
  trend(x_screen + 9) =  (1 - ratio * .02) * trend(x_screen + 9)
  trend(x_screen +10) =  (1 - ratio * .01) * trend(x_screen +10)
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
'  Line(0, y_curve - 25)-(x_width - 1, y_curve + 25), black, b         'adjusting amplitude.
  
  Line(x_screen - 3 * lambda_prime_1 - 5, y_curve - 52)-(x_screen, y_curve + 52), dark_gray, bf
  For x = x_screen - 3 * lambda_prime_1 - 5 To x_screen
    radian = 2 * pi * (x_screen - x - 5) / lambda_prime_1             'standing wave envelope (left side).
    amplitude = 50 * Sin(radian)
    Line(x, y_curve - amplitude)-(x, y_curve + amplitude), white
  Next

  Line(x_screen + 5, y_curve - 52)-(x_screen + 5 * lambda_prime_2 + 5, y_curve + 52), dark_gray, bf
  For x = x_screen + 5 To x_screen + 5 * lambda_prime_2 + 5
    radian = 2 * pi * (x_screen - x + 5) / lambda_prime_2             'standing wave envelope (right side).
    amplitude = 50 * Sin(radian)
    Line(x, y_curve - amplitude)-(x, y_curve + amplitude), white
  Next

  Line(0, y_curve)-(x_width, y_curve), dark_gray
  previous = 0
  For x = 0 To x_width - 1                                            'axial amplitude graphics.
    amplitude = present(x)
    Line(x, y_curve - previous)-(x, y_curve - amplitude), black
    previous = amplitude
  Next
  Line(x_screen - 3, y_start)-(x_screen + 3, y_start + y_height), white, bf
  Line(x_screen - 5, y_curve - 52)-(x_screen + 5, y_curve + 52), white, bf
  Line(x_screen - 5, y_curve - 52)-(x_screen + 5, y_curve + 52), black, b
  Line(x_screen - 4, y_curve - 51)-(x_screen + 4, y_curve + 51), black, b

  Color black, background
  Locate 47, 2: Print "Iteration"; iteration
End Sub

