Width 80,20:Color 0,15:Cls:?
? " March 22, 2010. By Gabriel LaFreniere.":?:?
? " This FreeBasic program was adapted to the 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " It should still be compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?
' Gosub commands are not supported any more.
' Variables must be declared.

Declare Sub Wave_Generator()
Declare Sub Initialization()
Dim Shared As String in_key
Dim Shared As Integer r, g, b, j, x, y, y_center, work_page, visible_page, matrix_page, speed
Dim Shared As Integer image, images, display_width, display_height, half_width, half_height
Dim Shared As Single x_coord, y_coord, x_prime, y_prime, x_squared, y_squared, center
Dim Shared As Single beta, lambda, theta, g_Lorentz, amplitude, distance, t_time, t_prime
Const pi = 4 * Atn(1), double_pi = 8 * Atn(1), white = Rgb(255,255,255)
Const background = Rgb(225,225,225), black = 0, green = Rgb(0,150,0)
beta = .6: work_page = 1: 
Screen 20,24,3: Initialization()

Do
  Swap visible_page, work_page                                        'swap pages 0 <=> 1.
  Screenset work_page, visible_page                                   'set work page (faster).
  Pcopy matrix_page, work_page                                        'copy on work page.
  Wave_Generator()                                                           'wave display.
  in_key = Inkey                                                      'keyboard only (no mouse management).
  If Len(in_key) Then
    If in_key = Chr(27) Or in_key = Chr(255)+"k" Then End             'quit.
    in_key = Ucase(in_key)
    speed = Val(in_key)                                               'selecting the beta normalized speed.
    If speed > 0 Then
      beta = speed / 10
      If in_key = "0" Then beta = 0
      If in_key = "7" Then beta = Sin(pi / 4)
      If in_key = "9" Then beta = Sin(pi / 3)
      speed = 0
      Initialization() 
    End If
    If in_key = "P" Then Sleep                                        'pause.
    If in_key = "R" Then image = 0: beta = .6: Initialization()       'reset.
    in_key = Inkey                                                    'clear buffer.
  End If
Loop


'*********************************************************************
' WAVE GENERATOR
'*********************************************************************

Sub Wave_Generator()
  image = image + 1                                                   'to be converted in wave period.
  If x_prime * lambda > 1080 - half_width Then image = 0              'reset.
  t_time = image * double_pi / images                                 'wave period according to image.

  For x = 0 To display_width                                          'horizontal scan.
    x_coord = (x - half_width) / lambda                               'x coordinate in wavelength units.
    x_squared = x_coord ^ 2
  
'*********************************************************************
' APPLYING THE LORENTZ TRANSFORMATIONS *******************************
'*********************************************************************

' THE EQUATION SET BELOW PRODUCES A DOPPLER EFFECT AND IT FORCES THE EMITTER TO MOVE
    x_prime = g_Lorentz * x_coord + beta * t_time                    ' x' coordinate in wavelength units.
    t_prime = g_Lorentz * t_time  - beta * x_coord                   ' t' time, more exactly the wave phase.

    For y_prime = 0 To half_height                                    'vertical scan (y' = y; axial symmetry).
      y_coord = y_prime / lambda                                      'y coordinate in wavelength units.
      distance = Sqr(x_squared + y_coord^2)                           'equals the delay in wave periods.
      amplitude = Sin(double_pi * (distance-t_prime)) / Sqr(distance) 'phase according to distance and t_prime.
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
      Pset(x_prime * lambda + half_width, y_center + y_prime), Rgb(r,g,b)
      Pset(x_prime * lambda + half_width, y_center - y_prime), Rgb(r,g,b)
    Next

' THIS EQUATION SET CORRECTS THE DOPPLER EFFECT AND IT STOPS THE EMITTER
    x_coord = g_Lorentz * x_prime - beta * t_prime                    'x coordinate in wavelength units.
    t_time  = g_Lorentz * t_prime + beta * x_prime                    't time, more exactly a wave phase.
    

    For y = 0 To half_height                                          'vertical scan (y' = y; axial symmetry).
      y_coord = y / lambda                                            'y coordinate in wavelength units.
      distance = Sqr(x_squared + y_coord^2)                           'equals the delay in wave periods.
      amplitude = Sin(double_pi * (distance - t_time)) / Sqr(distance)'phase according to distance and t.
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
      Pset(x_coord * lambda + half_width, half_height - y), Rgb(r,g,b)
      Pset(x_coord * lambda + half_width, half_height + y), Rgb(r,g,b)
    Next
    If x = half_width Then center = x_prime * lambda                  'for the moving wavelength scale below.
  Next
  
  Line(half_width, 0)-(half_width, display_height), white             'stationary wavelength scale.
  y_coord = y_center - half_height
  Line(center+half_width, y_coord)-(center+half_width, y_center + half_height), white 'moving wavelength scale.
  For j = 0 To 8 * lambda Step lambda
    Line(half_width - 10, j)-(half_width + 10, j), white
    Line(center-10+half_width,j + y_coord)-(center+10+half_width,j + y_coord), white
  Next
End Sub

'*********************************************************************
' INITIALIZATION
'*********************************************************************

Sub Initialization()
  matrix_page = 2
  Windowtitle "The Lorentz Transformations - The Doppler effect revisited."
  theta = Asin(beta)                                                  'transverse wave angle.
  g_Lorentz = Cos(theta)                                              'Lorentz contraction factor.
  lambda = 36                                                         'wavelength.
  images = 128                                                        'number of images per period.
  display_width = 398
  display_height = 8 * lambda
  half_width = display_width / 2
  half_height = display_height / 2
  y_center = display_height + half_height + 20
  Screenset matrix_page
  Color black, background: Cls: Locate 16,53: Color Rgb(0,0,255)
  Color black
  Locate 2, 54: Print "Press a number to change the speed of the emitter."
  Locate 4, 64: Print "Beta normalized speed.......... "; chr(225); " = ";
  Print Using "#.###    v / c"; beta
  Locate 5, 64: Print "Lorentz's contraction factor... g = ";
  Print Using "#.###    "; g_Lorentz;
  Print "Sqr(1 - "; chr(225); " ^ 2)"
  Locate 7, 54: Print "The Lorentz transformations produce a Doppler effect featuring a slower"
  Locate 8, 54: Print "frequency, so that the transverse wavelength remains unchanged. In"
  Locate 9, 54: Print "addition, a translation motion occurs. The result is the image below."
  Locate 11,74: Print "x'= g * x + "; chr(225); " * t      y'= y"
  Locate 12,74: Print "t'= g * t - "; chr(225); " * x      z'= z"
  Locate 14,54: Print "Surprisingly, the equation set below neutralizes this special Doppler"
  Locate 15,54: Print "effect. The stationary image on the left side is obtained this way."
  Locate 17,74: Print "x = g * x'- "; chr(225); " * t'     y = y'"
  Locate 18,74: Print "t = g * t'+ "; chr(225); " * x'     z = z'"
  Locate 40,29: Print "Lorentz's original equation was:  x'= (x - "; chr(225); " * t) / g"
  Locate 41,29: Print "                   Extracting x:  x = g * x'+ "; chr(225); " * t"
  Locate 42,29: Print "              Swapping x' and x:  x'= g * x + "; chr(225); " * t"
  Locate 44,29: Print "The important point is that x and t should refer to the stationary frame of reference."
  Locate 45,29: Print "Please note that x and x' stand for Cartesian coordinates (in light-second or wavelength units)."
  Locate 46,29: Print "Similarly, t and t' stand for the time (in seconds) or for the wave phase (in radian units)."
  Locate 43, 2: Print "Pause: Press P."
  Locate 44, 2: Print "Reset: Press R."
  Locate 46, 2: Print "Press Esc to Quit."
  Color green
  Locate 48,29: Print "This program may be freely distributed, copied or modified.  Gabriel LaFreniere   glafreniere.com";
End Sub
