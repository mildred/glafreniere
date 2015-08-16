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
Declare Sub Linear_Transmitters()
Declare Sub Mouse_Management()
Declare Sub Theoretical_Display()
Declare Sub Wave_Display()

Const pi = 4 * Atn(1)
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(120,120,120)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)

Dim Shared As Integer r, g, b, j, x, y, x_width = 1280, y_height = 100
Dim Shared As Integer x_center, y_1a, y_1b, y_2a, y_2b, y_3a, y_3b
Dim Shared As Integer  x_squared, x_emitter_1, x_emitter_2
Dim Shared As Integer matrix_page, work_page, visible_page, luminance_1, luminance_2
Dim Shared As Integer iteration, previous, previous_1, previous_2, bitmap
Dim Shared As Integer frame, skipped_frames, line_number, click, x_mouse, y_mouse, wheel
Dim Shared As Single amplitude, phase, distance, radian, brightness
Dim Shared As Single wave_speed, amplitude_1, amplitude_2, lambda_1, lambda_2
Dim Shared As Single orthogonal, diagonal, alpha, g_alpha, ratio, lambda, lambda_prime
Dim Shared As Single x_coord, y_coord, x_prime, t_time, t_prime
Dim Shared As Single past(-1 To x_width+1), present(-1 To x_width+1), trend(-1 To x_width + 1)
Dim Shared As String in_key, display,line44, line45, line46, line47, line48, bitmap_number, file
ratio = Sqr(3): alpha = (ratio - 1) / (ratio + 1): g_alpha = Sqr(1 - alpha^2): lambda = 100
lambda_1 = lambda * (1 - alpha) / g_alpha: lambda_2 = lambda * (1 + alpha) / g_alpha
visible_page = 0: work_page = 1: matrix_page = 2: bitmap = 0
Screen 21,24,3: Initialization()


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  For frame = 0 To skipped_frames

' ******************************************************************** "THE PAST IS A GUIDE TO THE FUTURE"
' JOCELYN MARCOTTE'S VIRTUAL WAVE ALGORITHM (CREATED JANUARY 2006).     IT IS THE SIMPLEST ALGORITHM EVER.
' ********************************************************************
    For x = 0 To x_width                                              'updating amplitude states.
      past(x)  = present(x)                                           'previous amplitude.
      present(x) = trend(x)                                           'present amplitude (energy is amplitude squared).
    Next
    For x = 0 To x_width 
      trend(x) = present(x-1) + present(x+1) - past(x)                'trend extrapolation.
    Next
' ********************************************************************
' END OF ALGORITHM.
' ********************************************************************

    Linear_Transmitters()                                             'impulses.
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If frame = 0 Then Theoretical_Display(): Wave_Display()           'skip other frames.
    iteration += 1
    If iteration > 1400 Then
      Sleep: in_key = Inkey
      If in_key = Chr(27) Then End Else Initialization()
      End If
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next
  If y_mouse < 186 Or y_mouse > 768 Or x_mouse < 360 Or x_mouse > 664 Then
    line_number = 0
  Else line_number = .5 + y_mouse / 16                                'line number in text units.
  End If
  If Len(in_key) Then Keyboard_Management()
  If line_number > 1 Or click = 2 Then Mouse_Management()
  If bitmap > 0 And iteration > 500 Then
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
  Windowtitle " The Alpha Transformations "
  x_emitter_1 = 0
  x_emitter_2 = x_width
  brightness = 1
  skipped_frames = 0                                                  'select 0 for smoother but slower waves.
  iteration = 0
  y_1a = 160
  y_1b = y_1a + 103
  y_2a = y_1b + 80
  y_2b = y_2a + 103
  x_center = .5 * x_width
'  ratio = lambda_2 / lambda_1
'  alpha = (ratio - 1) / (ratio + 1)
'  g_alpha = Sqr(1 - alpha^2)
'  lambda = Sqr(lambda_2 * lambda_1)
  lambda_prime = g_alpha * Sqr(lambda_2 * lambda_1)
  For x = 0 To x_width                                                'erasing previous data.
    past(x) = 0
    present(x) = 0
    trend(x) = 0
  Next
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Bload "Standing_Waves_03_Transformations.bmp"
  Locate 49, 2: Print "        lambda 1 "; lambda_1
  Locate 50, 2: Print "        lambda 2 "; lambda_2
  Locate 51, 2: Print "wavelength ratio "; ratio
  Locate 52, 2: Print "     alpha speed";: Print Using "###.######"; alpha
  Locate 53, 2: Print "Lorentz's factor "; g_alpha
  Locate 54, 2: Print "          lambda ";: Print Using "####.####"; lambda
  Locate 55, 2: Print "    lambda prime ";: Print Using "###.#####"; lambda_prime
  Locate 56, 2: Print "phase wave speed ";: Print Using " #.######";  1 / alpha

  Color dark_gray
  Locate 63, 3:  ? "Thanks to the creators of FreeBASIC."
  Locate 64, 3:  ? "Gabriel LaFreniere  glafreniere.com";
  Locate 63,121: ? "August 16, 2010. This program may be"
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
  Line(39, y_1a - 20)-(40 + 3 * lambda_1 + 1, y_1a - 19), black, b    'wavelength scales.
  For x = 40 To 3 * lambda_1 + 40 Step lambda_1
    Line(x-1, y_1a - 18)-(x+1, y_1a - 4), black, bf
  Next
  For x = 40 + lambda_1 / 2 To 3 * lambda_1 + 40 Step lambda_1
    Line(x-1, y_1a - 18)-(x+1, y_1a - 9), black, bf
  Next
  Line(x_center - 3 * lambda_prime - 1, y_1a - 20)-(x_center + 3 * lambda_prime - 1, y_1a - 19), black, b
  For x = x_center - 3 * lambda_prime To x_center + 3 * lambda_prime Step lambda_prime
    Line(x-1, y_1a - 18)-(x+1, y_1a - 4), black, bf
  Next
  For x = x_center - 3 * lambda_prime + lambda_prime / 2 To x_center + 3 * lambda_prime + 2 Step lambda_prime
    Line(x-1, y_1a - 18)-(x+1, y_1a - 9), black, bf
  Next
  Line(x_width - 41 - 2 * lambda_2, y_1a - 20)-(x_width - 38, y_1a - 19), black, b
  For x = x_width - 40 - 2 * lambda_2 To x_width - 39 Step lambda_2
    Line(x-1, y_1a - 18)-(x+1, y_1a - 4), black, bf
  Next
  For x = x_width - 40 - 2 * lambda_2 + lambda_2 / 2 To x_width - 40 Step lambda_2
    Line(x-1, y_1a - 18)-(x+1, y_1a - 9), black, bf
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
  phase = iteration * 2 * pi / lambda_1
  amplitude = 870 * Cos(phase) / lambda_1 ^ 2                         'impulse zone is smaller for shorter wavelength.
    For x = 0 To lambda_1 / 4
      trend(x + x_emitter_1) = trend(x + x_emitter_1) + amplitude
    Next
  phase = iteration * 2 * pi / lambda_2
  amplitude = 915 * Cos(phase) / lambda_2 ^ 2                         'impulse zone is smaller for shorter wavelength.
    For x = -lambda_2 / 4 To 0
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
' DISPLAYING THEORETICAL WAVE PATTERN.
'*********************************************************************

Sub Theoretical_Display()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Line(x_center - 3 * lambda_prime - 1, y_1b - 52)-(x_center + 3 * lambda_prime + 1, y_1b + 52), dark_gray, bf
  Line(x_center - 3 * lambda_prime - 1, y_2b - 52)-(x_center + 3 * lambda_prime + 1, y_2b + 52), dark_gray, bf
'  Line(0, y_1b - 25)-(x_width - 1, y_1b + 25), black, b              'adjusting amplitude.
'  Line(0, y_2b - 25)-(x_width - 1, y_2b + 25), black, b              'adjusting amplitude.
  phase = 2 * pi * alpha * iteration / lambda_prime - .18
  For x = 0 To 6 * lambda_prime                                       'node and antinode template.
    radian = 2 * pi * x / lambda_prime
    amplitude = 50 * Sin(phase - radian)
    Line(x + x_center - 3 * lambda_prime, y_1b - amplitude)-(x + x_center - 3 * lambda_prime, y_1b + amplitude), white
    Line(x + x_center - 3 * lambda_prime, y_2b - amplitude)-(x + x_center - 3 * lambda_prime, y_2b + amplitude), white
  Next

  Line(0, y_2b)-(x_width, y_2b), dark_gray
  For x = -100 To x_width                                             'lower display.
    x_coord = x / lambda                                              ' x coord in lambda units.
    t_time = iteration / lambda                                       ' t time in wave period units (wave speed c = 1 pixel per iteration)
    phase = 2 * pi * x_coord                                          'phase in radians for this distance.

'************** APPLYING THE ALPHA TRANSFORMATIONS *******************

    x_prime = g_alpha * x_coord + alpha * t_time
    t_prime = g_alpha * t_time  - alpha * x_coord

'*********************************************************************

    x_prime = x_prime * lambda                                        ' x' in pixel units.
    t_prime = 2 * pi * t_prime                                        ' t' in radians.
    amplitude = Sin(phase - .7) * Sin(t_prime - .75)                   'synchronizing with virtual medium (adjust phase wave using t_prime).
    luminance_1 = 375 * Abs(amplitude)
    b = luminance_1 / 2
    If b > 255 Then b = 255
    If luminance_1 > 255 Then
      luminance_2 = luminance_1 - 255
      If luminance_2 > 255 Then luminance_2 = 255
      luminance_1 = 255
    Else luminance_2 = 0
    End If
    If amplitude > 0 Then
      r = luminance_2                                                 'using complementary magenta and emerald green.
      g = luminance_1
    Else
      r = luminance_1
      g = luminance_2
    End If
    For y = 0 To 50
       Pset(x_prime-1, y + y_2a), Rgb(r,g,b)
       Pset(x_prime  , y + y_2a), Rgb(r,g,b)
    Next
    If x > -100 Then Line(x_prime, y_2b - previous)-(x_prime, y_2b - 50 * amplitude), black
    previous = 50 * amplitude
  Next

End Sub


'*********************************************************************
' DISPLAYING POSITIVE AMPLITUDE IN GREEN AND NEGATIVE IN RED.
'*********************************************************************

Sub Wave_Display()
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
    For y = y_1a To y_1a + 50
      Pset(x,y), Rgb(r,g,b)
    Next
  Next
'  Line(0, y_1b - 25)-(x_width - 1, y_1b + 25), black, b              'adjusting amplitude.
  Line(0, y_1b)-(x_width, y_1b), dark_gray
  previous = 0
  For x = 0 To x_width - 1                                            'axial amplitude graphics.
    amplitude = present(x)
    Line(x, y_1b - previous)-(x, y_1b - amplitude), black
    previous = amplitude
  Next
  Color black, background
  Locate 47, 2: Print "Iteration"; iteration
End Sub

