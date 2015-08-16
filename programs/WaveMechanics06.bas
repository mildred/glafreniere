' This is a FreeBasic program.
' Please download the IDE (editor) from: http://fbide.freebasic.net
' The source code was adapted to the 0.20.0b Compiler (2008) for Windows available from:
' http://www.freebasic.net/index.php/download
' It should still be compatible with previous compilers and other platforms.

Declare Sub Emitter()
Declare Sub Initialization()
Declare Sub Title()

Dim Shared As String key, choice
Dim Shared As String line40, line41, line42, line43, line44, line45, line46
Dim Shared As String line47a, line47b, line47c, line48a, line48b, line48c

Dim Shared As Single beta, theta, g_Lorentz, amplitude, phase
Dim Shared As Single t_time, t_prime, center, x_coord, y_coord, variable
Dim Shared As Single x_prime, y_prime, x_squared, y_squared, Voigt_s_constant

Dim Shared As Integer x_center, y_center, x_scale, x, y, r, g, b, j
Dim Shared As Integer matrix_page, work_page, visible_page, line_number
Dim Shared As Integer half_width, display_width, half_height, display_height
Dim Shared As Integer x_mouse, y_mouse, wheel, click, image, images, lambda, speed

Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(75,75,75)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)
Const pi = 4 * Atn(1), double_pi = 8 * Atn(1)

beta = Sin(Pi / 4): work_page = 1: choice = "C"
Screen 20,24,3: Initialization()

Do
  Swap visible_page, work_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Emitter()                                                           'wave display.
  key = Inkey                                                         'keyboard management.
  If Len(key) Then
    If key = Chr(27) Or key = Chr(255)+"k" Then End                   'quit.
    key = Ucase(key)
    Select Case key
      Case "A","B","C","D": image = -100: choice = key: Initialization()'select Voigt's constant.
      Case "I"
        beta = Sin(Pi / 4): choice = "C": image = 0                            'initialization.
        Initialization()
      Case "M": Run "WaveMechanics00.exe"                             'main menu.
      Case "P"                                                        'pause.
        Screenset visible_page
        Color red, background
        Locate 38, 42: ? "Paused. Press any key to resume.           "
        Sleep
        key = Inkey                                                   'clear buffer.
      Case "R": image = 0                                             'reset.
    End Select
    If key = "0" Or Val(key) > 0 Then
      beta = Val(key) / 10                                            'select beta speed (keyboard).
      If key = "9" Then beta = Sin(Pi / 3)
      If key = "7" Then beta = Sin(Pi / 4)
      Initialization()
    End If
  End If

'*************************************************************************************************************************
' MOUSE MANAGEMENT.
'*************************************************************************************************************************

  Getmouse x_mouse, y_mouse, wheel, click
  line_number = .5 + y_mouse / 16
  If line_number < 40 Or x_mouse < 320 Or x_mouse > 695 Then line_number = 0
  Color green_text, white
  Locate line_number, 41

  Select Case line_number
    Case 40: If Not choice = "A" Then
               Print line40
               If click Then choice = "A": image = -100: Initialization()
             End If
    Case 41: If Not choice = "B" Then
               Print line41
               If click Then choice = "B": image = -100: Initialization()
             End If
    Case 42: If Not choice = "C" Then
               Print line42
               If click Then choice = "C": image = -100: Initialization()
             End If
    Case 43: If Not choice = "D" Then
               Print line43
               If click Then choice = "D": image = -100: Initialization()
             End If
    Case 45: Print "     Click to Select Beta Velocity.            "  'selecting Lorentz's normalized beta velocity.
             If click > 0 Then
               Screenset visible_page, visible_page                   'adjustable cursor.
               Locate 45, 41: Print "                              "
               Line(316, 687)-(698, 736), black, b
               Line(317, 688)-(697, 735), white, bf
               Locate 45, 42: ? "0                                        .866"
               Line(339, 704)-(648, 719), black, b
               Do
                 Getmouse x_mouse, y_mouse, wheel, click
                 variable = (x_mouse - 301) / 400
                 If variable > Sin(Pi / 3) Then variable = Sin(Pi / 3)
                 If variable < .1 Then variable = 0 
                 Line(340, 705)-(647, 718), buff, bf
                 If variable > 0 Then Line(301 + 400 * variable, 704)-(301 + 400 * variable, 719), black
                 Locate 44, 46: ? "Selecting Beta Velocity"; variable; "    "
                 Locate 46, 46: ? Using "Currently #.####"; beta
                 If Abs(y_mouse - 712) > 20 Then Exit Do              'off limits.
               Loop While click
             beta = variable: Initialization()
             End If

    Case 47
      Select Case x_mouse
        Case Is > 700
        Case Is > 576: Locate 47, 73: ? line47c: Sleep 500            'slow.
                       If click > 0 Then Sleep 2000                   'slower.
        Case Is > 472: Locate 47, 60: ? line47b
                       If click > 0 Then Run "WaveMechanics00.exe"    'main menu.
        Case Is > 318: Locate 47, 41: ? line47a
                       If click > 0 Then                              'initialization.
                         Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
                         beta = .7: choice = "C": image = 0
                         Initialization()
                       End If
      End Select

    Case 48  
      Select Case x_mouse
        Case Is > 700
        Case Is > 576: Locate 48, 73: ? line48c;
                       If click > 0 Then Run "WaveMechanics07.exe"    'next program.
        Case Is > 472: Locate 48, 60: ? line48b;
                       If click > 0 Then End                          'quit.
        Case Is > 318: Locate 48, 41: ? line48a;
                       If click > 0 Then Run "WaveMechanics05.exe"    'previous program.
      End Select
  End Select
Loop


'*************************************************************************************************************************
' EMITTER (WAVE GENERATOR) UNDERGOING NORMAL AND REVERSE DOPPLER TRANSFORMATIONS.
'*************************************************************************************************************************

Sub Emitter():
  
  For x = 0 To display_width                                          'full horizontal scan (no symmetry).
    x_coord = (x - half_width) / lambda                               'Lorentz's x coordinate in wavelength units.
    x_squared = x_coord ^ 2
    t_time = image * double_pi / images                               'Lorentz's t time in 2 pi units. ACTUALLY THE EMITTER
                                                                      'PULSATING PERIOD according to the current image.

' DISPLAYING THE STATIONARY WAVE EMITTER (NO DOPPLER EFFECT) *************************************************************

    For y = 0 To half_height                                          'wave generator; half vertical scan (axial symmetry).
      y_coord = y / lambda                                            'Lorentz's actual y coordinate in wavelength units.
      phase = Sqr(x_squared + y_coord ^ 2)                            'delay according to actual distance in wave period units.
      amplitude = Sin(double_pi * (phase - t_time)) / Sqr(phase)      'sinusoidal wave; dim according to distance/phase.
      If amplitude > 0 Then
        r = 0: g = 255 * amplitude: b = g / 2                         'color distribution.
        If g > 255 Then r = g - 255: g = 255
        If r > 255 Then r = 255
        If b > 255 Then b = 255
      Else
        r = -255 * amplitude: g = 0: b = r / 2
        If r > 255 Then g = r - 255:r = 255
        If g > 255 Then g = 255
        If b > 255 Then b = 255
      End If
      Pset(x, half_height + y), Rgb(r,g,b)                            'displaying the stationary emitter (no Doppler effect).
      Pset(x, half_height - y), Rgb(r,g,b)
    Next

' DISPLAYING THE TRANSFORMED WAVE EMITTER USING WOLDEMAR VOIGTS' VARIABLE DOPPLER EFFECT *********************************

' THE WOLDEMAR VOIGT REVERSED TRANSFORMATIONS ****************************************************************************
    x_prime = x_coord * g_Lorentz * Voigt_s_constant + t_time  * beta 'Lorentz's x' coordinate in wavelength units.
    t_prime = t_time  * g_Lorentz / Voigt_s_constant - x_coord * beta 'Lorentz's t' time (actually the emitter phase) in 2 pi units.
' ************************************************************************************************************************

    If x = half_width Then
      x_scale = x_prime * lambda                                      'the moving scale coordinate using constant x_coord = 0.
      If x_scale > 500 - half_width Then image = -100                 'reset.
    End If
    
    For y = 0 To half_height
      y_coord = y / lambda                                            'Lorentz's actual y coordinate in wavelength units.
      phase = Sqr(x_squared + y_coord ^ 2)                            'delay according to actual distance in wave period units.
      amplitude = Sin(double_pi * (phase - t_prime)) / Sqr(phase)     'amplitude according to t_prime.
      y_prime = y * Voigt_s_constant                                  'this is the y' coordinate in pixel units according to
                                                                      'Voigt's constant (Lorentz: K = 1, hence y' unchanged).
      If amplitude > 0 Then
        r = 0: g = 255 * amplitude: b = g / 2                         'color distribution.
        If g > 255 Then r = g - 255: g = 255
        If r > 255 Then r = 255
        If b > 255 Then b = 255
      Else
        r = -255 * amplitude: g = 0: b = r / 2
        If r > 255 Then g = r - 255:r = 255
        If g > 255 Then g = 255
        If b > 255 Then b = 255
      End If
      Pset(x_prime * lambda + half_width, y_center + y_prime), Rgb(r,g,b)'displaying the moving emitter (Doppler effect).
      Pset(x_prime * lambda + half_width, y_center - y_prime), Rgb(r,g,b)
      If Voigt_s_constant > 1 Then
        Pset(x_prime * lambda + half_width, y_center + y_prime + 1), Rgb(r,g,b)
        Pset(x_prime * lambda + half_width, y_center - y_prime - 1), Rgb(r,g,b)
      End If
    Next                                                              'adding a separator for clarity:
    If x = display_width Then Line(x_prime * lambda + half_width, y_center - y_prime - 1)-(x_prime * lambda + half_width, y_center + y_prime + 1), white


' DISPLAYING THE LORENTZ TRANSFORMATIONS USING MERELY THE SAME EQUATION SET AS ABOVE, EXCEPT FOR THE SIGN AND THE CONSTANT
    x_coord = x_prime * g_Lorentz - t_prime * beta                    'Lorentz's x coordinate in wavelength units.
    t_time  = t_prime * g_Lorentz + x_prime * beta                    'Lorentz's t time (actually the emitter phase) in 2 pi units.
' ************************************************************************************************************************

'   Please note that the reversed equation set below is capable of recovering
'   any of the four options A, B, C, or D because of Voigt's constant.
'   x_coord = x_prime * g_Lorentz / Voigt_s_constant - t_prime * beta
'   t_time  = t_prime * g_Lorentz * Voigt_s_constant + x_prime * beta

    For y = 0 To half_height                                          'vertical scan, axial symmetry.
      y_coord = y / lambda                                            'Lorentz's actual y coordinate in wavelength units.
      phase = Sqr(x_squared + y_coord ^ 2)                            'delay according to actual distance in wave period units.
      y_coord = y                                                     'Lorentz's actual y coordinate in pixel units.
      amplitude = Sin(double_pi * (phase - t_time)) / Sqr(phase)      'sinusoidal wave, dim according to distance/phase.
      If amplitude > 0 Then
        r = 0: g = 255 * amplitude: b = g / 2                         'color distribution.
        If g > 255 Then r = g - 255: g = 255
        If r > 255 Then r = 255
        If b > 255 Then b = 255
      Else
        r = -255 * amplitude: g = 0: b = r / 2
        If r > 255 Then g = r - 255:r = 255
        If g > 255 Then g = 255
        If b > 255 Then b = 255
      End If
      Pset(x_coord * lambda + 800, y_center + y_coord), Rgb(r,g,b)    'displaying the twice-transformed emitter..
      Pset(x_coord * lambda + 800, y_center - y_coord), Rgb(r,g,b)      
      Pset(x_coord * lambda + 800 + 1, y_center + y_coord), Rgb(r,g,b)'filling empty spaces (stretched area).
      Pset(x_coord * lambda + 800 + 1, y_center - y_coord), Rgb(r,g,b)      
    Next
  Next

  Line(half_width, 0)-(half_width, display_height), white             'wavelength scale.
  Line(x_scale + half_width, display_height + 2)-(x_scale + half_width, 2 * display_height + 2), white
  For j = 0 To 8 * lambda Step lambda
    Line(half_width - 10, j)-(half_width + 10, j), white
    Line(x_scale - 10 + half_width,j + display_height + 2)-(x_scale + 10 + half_width, j + display_height + 2), white
  Next
  image += 1                                                          'to be converted into wave period.
End Sub


'*************************************************************************************************************************
' INITIALIZATION.
'*************************************************************************************************************************

Sub Initialization()
  matrix_page = 2
  Windowtitle "WaveMechanics06 - The Lorentz-Voigt Transformations. - The Doppler Effect Revisited."
  theta = Asin(beta)                                                  'transverse wave angle.
  g_Lorentz = Cos(theta)                                              'Lorentz contraction factor.
  lambda = 36                                                         'wavelength.
  images = 128                                                        'number of images per period.
  display_width = 398
  display_height = 8 * lambda
  half_width = display_width / 2
  half_height = display_height / 2
  y_center = display_height + half_height + 2
  line40  = " A - Unusual Faster Frequency.       K = g ^ 2 "
  line41  = " B - Michelson: Regular Frequency.   K = g     "
  line42  = " C - Lorentz: Slower Frequency.      K = 1     "
  line43  = " D - Unusual Even Slower Frequency.  K = 1 / g "
  line45  = " Select Beta Velocity. Press a key from 0 to 9."
  line47a = " I - Initialize.   "
  line47b = " M - Menu.   "
  line47c = " Slow.         "
  line48a = " Previous Program. "
  line48b = " Quit (Esc.) "
  line48c = " Next Program. "

  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Color green_text
  Locate 40,41: ? line40
  Locate 41,41: ? line41
  Locate 42,41: ? line42
  Locate 43,41: ? line43
  Locate 45,41: ? line45
  Locate 47,42: ? "I - Initialize.    M - Menu.    Slow."
  Locate 48,42: ? "Previous Program.  Quit (Esc.)  Next Program.";
  Color Blue: Locate 13,53
  Select Case choice                                                  'Woldemar Voigt's constant.
    Case "A": Voigt_s_constant = g_Lorentz ^ 2
                          Print "The faster frequency leads to a severe axial and transverse contraction."
              Locate, 53: Print ""
              Locate 40,41: ? line40
    Case "B": Voigt_s_constant = g_Lorentz
              Locate, 72:  Print "THE MICHELSON TRANSFORMATION":?
              Locate, 53: Print "This may be seen as the regular Doppler effect. There is no frequency"
              Locate, 53: Print "shift. The system undergoes an axial contraction according to g ^ 2"
              Locate, 53: Print "and a transverse contraction according to g."
              Locate 41,41: ? line41
    Case "C": Voigt_s_constant = 1
              Locate, 73: Print "THE LORENTZ TRANSFORMATIONS":?
              Locate, 53: Print "The emitter frequency slows down and the system undergoes an axial"
              Locate, 53: Print "contraction according to Lorentz's g shrinking factor. There is no"
              Locate, 53: Print "transverse contraction. Here, Voigt's constant k = 1 becomes useless."
              Locate 42,41: ? line42
    Case "D": Voigt_s_constant = 1 / g_Lorentz
                          Print "This quite abnormal Doppler effect leads to an even slower frequency."
              Locate, 53: Print "It produces a transverse dilation but no longitudinal contraction."
              Locate 43,41: ? line43
  End Select
  Locate 11,53: Print "beta = v / c = ";
  Print Using "#.####"; beta;: Print "      g = sqr(1 - beta ^ 2) = ";
  Print Using "#.####"; g_Lorentz;: Print "      k = ";
  Print Using "#.####"; Voigt_s_constant
  Color black
  Locate  4,53: Print "The Lorentz transformations appear to be a special case of Woldemar Voigt's"
  Locate   ,53: Print "transformations featuring a slower frequency in accordance with Lorentz's"
  Locate   ,53: Print "contraction factor g shown below. In this case, Voigt's constant is no"
  Locate   ,53: Print "longer effective (k = 1) and it may be removed from the equation set. The"
  Locate   ,53: Print "absence of transverse contraction leads to Relativity. However, this also"
  Locate   ,53: Print "indicates that Lorentz's choice of variable was wrong."
  
  Locate 38,42: Print "Please click slowly!      Pause: Press P."
  Locate 38, 2: Print "This moving system is undergoing the"
  Locate 39, 2: Print "Doppler effect thanks to Woldemar"
  Locate 40, 2: Print "Voigt's reversed equation set, which"
  Locate 41, 2: Print "requires a special k constant."
  Locate 43, 6: Print "x'= x * g * k + t * beta"
  Locate 44, 6: Print "t'= t * g / k - x * beta"
  Locate 45, 6: Print "y'= y * k   z'= z * k"
  Locate 38,90: Print "This system was recovered from the"
  Locate 39,90: Print "moving one using Lorentz's reversed"
  Locate 40,90: Print "equations without the constant. It is"
  Locate 41,90: Print "all about the Lorentz transformations."
  Locate 43,93: Print "x = x' * g - t' * beta"
  Locate 44,93: Print "t = t' * g + x' * beta"
  Locate 45,93: Print "y = y'   z = z'"
  Title()
  Color dark_gray
  Locate 47, 2: ? "Thanks to the creators of FreeBASIC."
  Locate 48, 2: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47,90: ? "April 23, 2009. This program may be"
  Locate 48,90: ? "freely distributed, copied or modified.";
  Color black
  Line(317, 585)-(696, 800), gray, b
  Line(318, 586)-(696, 800), black, b
  Line(696, 586)-(696, 800), white
  Line(697, 585)-(697, 800), white
End Sub


'*************************************************************************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*************************************************************************************************************************

Sub Title()
  Dim As String display_title
  Dim As Integer y_title, x1, x2, y1, y2, dark
  display_title = "The Lorentz-Voigt Transformations"
  Locate 1,1: ? display_title
  center = 710
  y_title = 0
  For x1 = 0 To 8 * Len(display_title)
    x2 = center + 2 * x1 - 8 * Len(display_title)
    For y1 = 1 To 14
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), gold
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), gold
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), gold
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), gold
      End If
    Next
    If (x1+1) Mod 8 Then Else Line(x2+1, y_title)-(x2+1, y_title + 34), background'separate invasive characters such as capital M.
  Next
  Line(0, 0)-(8 * Len(display_title), 14), background, bf                     'matrix title erased.
  For x1 = center - 8 * Len(display_title) To center + 8 * Len(display_title) 'adding light and shades.
    For y1 = y_title To y_title + 34
      If Point(x1, y1) = gold And Point(x1-1, y1-1) = background Then Pset(x1-1, y1-1), buff
      If Point(x1, y1) = gold And Point(x1+1, y1+1) = background Then Pset(x1+1, y1+1), black
    Next
  Next
  For x1 = center - 8 * Len(display_title) To center + 8 * Len(display_title) 'adding luster.
    For y1 = y_title + 4 To y_title + 32
      If Point(x1, y1) = gold Then dark = 9 * Abs(18 + y_title - y1): Pset(x1, y1), Rgb(240 - dark, 200 - dark, 120 - dark)
    Next
  Next
End Sub
