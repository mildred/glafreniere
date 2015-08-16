Width 80,20:Color 0,15:Cls:?
? " Created June 28, 2010 by Gabriel LaFreniere.":?:?
? " This FreeBasic program is compatible with the 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " It is still compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?:?

Declare Sub Initialization()

Const green = -16726016, green_text = Rgb(0,125,0), red = -65536, gray = Rgb(150,150,150)
Const pi = 4 * Atn(1), black = 0, white = -1, background = Rgb(225,225,225)
Dim Shared As Integer x_width = 1279, y_height = 719, x_center, y_center
Dim Shared As Integer matrix_page, work_page, visible_page, luminance, click, wheel
Dim Shared As Integer r, g, b, j, x, y, x_text, bitmap, iteration, lambda = 120
Dim Shared As Single g_Lorentz_x, g_Lorentz_y, g_Lorentz_xy, beta_xy, beta_x = .5, beta_y = -.4 'upward, hence negative y velocity component.
Dim Shared As Single c_speed, x_coord, y_coord, t_time, t_prime_x, t_prime_xy
Dim Shared As Single radian, amplitude, distance, x_squared, x_prime, y_prime
Dim Shared As String in_key, file, bitmap_number

Screen 21,24,3
Windowtitle " The Moving Electron and the Lorentz Transformations"
visible_page = 0: work_page = 1: matrix_page = 2: bitmap = 0          'set bitmap = 1 for BMP image sequence.
Initialization()

Do
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  t_time = c_speed * iteration / lambda
  
'*********************************************************************
' DISPLAYING THE WAVES USING THE LORENTZ ALL AZIMUTH TRANSFORMATIONS.
' The x' and y' coordinates are firstly scanned in order to stabilize
' the on-screen display. This also eliminates pixelation effects.
' That is why the x and y coordinates must be recovered by inverting
' the equation: x_coord = (x_prime - beta_x * t_time) / g_Lorentz_x
' instead of:   x_prime = g_Lorentz_x * x_coord + beta_x * t_time
' and also:     y_coord = (y_prime - beta_y * t_prime_x) / g_Lorentz_y
' instead of:   y_prime = g_Lorentz_y * y_coord + beta_y * t_prime_x
'*********************************************************************

  For x = x_text To x_width                                           'starting with x' as integer in pixel units in order to avoid aliasing.
    x_prime = x / lambda                                              'the x coordinate in lambda units must then be recovered by inverting the x' equation.
    x_coord = (x_prime - beta_x * t_time) / g_Lorentz_x               'this is Lorentz's equation in its original form, except for x and x' which are being swapped.
    t_prime_x = g_Lorentz_x * t_time - beta_x * x_coord               'this t' equation was reversed as compared to Lorentz's original one by swapping t and t'.
    x_squared = (x_coord * lambda - 500)^2                            '         =>> It turns out that Lorentz definitely confused the variables t and t'.
    For y = 0 To y_height
      y_prime = y / lambda                                            'y' in lambda units.
      y_coord = (y_prime - beta_y * t_prime_x) / g_Lorentz_y          'y inverted equation as explained above.
      t_prime_xy = g_Lorentz_y * t_prime_x - beta_y * y_coord         'brand new t[xy]' tridimensional equation (t[xy]' in wave period units).
      distance = Sqr(x_squared + (y_coord * lambda - 575)^2)          'distance in pixel units.
      radian = 2 * pi * distance / lambda                             'distance converted into wavelength units and radians.
      If radian Then amplitude = Sin(radian)/radian Else amplitude=1  'Jocelyn Marcotte's electron formula: sinus cardinalis = Sin(x) / x). ("x" stands for distance in radians).
      luminance = 2500 * Sin(2 * pi * t_prime_xy) * amplitude         't_prime_xy must be converted into radian units.
      If luminance > 0 Then                                           'yields complementary emerald green (positive amplitude) and magenta (negative) using constant 50% blue.
        g = luminance                                                 '    =>> The RGB system is deficient because the green color is way too bright. It should be splitted into
        b = luminance / 2                                             '    =>> yellow and emerald green. A 4-color display (red, yellow, emerald green and blue) would perform better.
        If luminance > 255 Then r = luminance - 255 Else r = 0
      Else
        r = -luminance
        b = -luminance / 2
        If luminance < -255 Then g = -luminance - 255 Else g = 0
      End If
      If r > 255 Then r = 255
      If g > 255 Then g = 255
      If b > 255 Then b = 255
      Pset(x, y), Rgb(r,g,b)                                          'printing the wave area.
    Next
  Next
  
  Color black, background
  Locate 53, 03: Print "Iteration... "; iteration
  Locate 50, 145: Print "   July 4, 2010"
  Locate 51, 145: Print "glafreniere.com"
  For x = 1152 To 1278: For y = 784 To 816
      If Point (x, y) = black Then Pset (x, y - 100), white
  Next: Next
  Line(1152, 784)-(1278, 816), background, BF
  If bitmap > 0 Then
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
    If bitmap > 700 Then End
  End If
  in_key = Inkey                                                      'keyboard management.
  If in_key = "p" Or in_key = "P" Then
    Screenset visible_page
    Color red: Locate 60, 3: ? "Paused. - Press Esc. to quit. - Press any other key to resume."
    Sleep: If Inkey = Chr(27) Then End
  Elseif in_key = Chr(27) Then End
  End If
  iteration += 1
Loop


Sub Initialization()
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Line(0,0)-(x_width, y_height), black, bf
  g_Lorentz_x = Sqr(1 - beta_x^2)
  g_Lorentz_y = Sqr(1 - beta_y^2)
  g_Lorentz_xy = g_Lorentz_x * g_Lorentz_y
  beta_xy = Sqr(1 - g_Lorentz_xy^2)
  c_speed = 2                                                         'wave speed in pixel per loop units.
  x_text = 343
  x_center = x_width - .5 * (x_width - x_text)
  y_center = .5 * y_height
  iteration = 0
  Color black, background
  Line(0, 0)-(x_text, y_height), background, BF                       'opaque text box.
  Line(0, 0)-(x_text, y_height), white, B 
  Line(1, 1)-(x_text-2, y_height-1), white, B
  Line(2, 2)-(x_text-3, y_height-2), white, B

  Locate 02, 03: ? "     The Moving Electron and the      "
  Locate 03, 03: ? "  All Azimuth Lorentz Transformations "
  Locate 05, 03: ? "Normalized speed (idem for y and z): "
  Locate 06, 03: ?  Chr(225);"[x] = v[x] / c"
  Locate 07, 03: ? "Lorentz's contraction factor:"
  Locate 08, 03: ? "g[x] = Sqr(1 - ";Chr(225);"[x]^2)"
  Locate 10, 03: ? "The Lorentz modified and now"
  Locate 11, 03: ? "tridimensional transformations:"

  Color black, white
  Line(18, 184)-(318, 390), white, bf
  Line(18, 184)-(318, 390), black, b
  Locate 13, 05: ? "t[x]'   = g[x] * t      - ";Chr(225);"[x] * x"
  Locate 14, 05: ? "t[xy]'  = g[y] * t[x]'  - ";Chr(225);"[y] * y"
  Locate 15, 05: ? "t[xyz]' = g[z] * t[xy]' - ";Chr(225);"[z] * z"
  Locate 17, 05: ? "x' = g[x] * x + ";Chr(225);"[x] * t"
  Locate 18, 05: ? "y' = g[y] * y + ";Chr(225);"[y] * t[x]'"
  Locate 19, 05: ? "z' = g[z] * z + ";Chr(225);"[z] * t[xy]'"
  
  Locate 21, 05: ?         "g[xy]  = g[x] * g[y]"
  Locate 22, 05: ? Chr(225);"[xy]  = Sqr(1 - g[xy]^2)"
  Locate 23, 05: ?         "g[xyz] = g[x] * g[y] * g[z]"
  Locate 24, 05: ? Chr(225);"[xyz] = Sqr(1 - g[xyz]^2)"

  Color black, background
  Locate 26, 03: ? "Lorentz's equation set is a special case"
  Locate 27, 03: ? "of Woldemar Voigt's one. It was intended"
  Locate 28, 03: ? "to deal with an unusual Doppler effect"
  Locate 29, 03: ? "involving a slower frequency. Maxwell's"
  Locate 30, 03: ? "equations are then becoming invariant."
  
  Locate 32, 03: ? "Definitely, the Lorentz transformations"
  Locate 33, 03: ? "are all about the Doppler effect. They"
  Locate 34, 03: ? "more specifically show a contraction"
  Locate 35, 03: ? "in the direction of motion and a local"
  Locate 36, 03: ? "time, more exactly a phase shift. They"
  Locate 37, 03: ? "lead to a phase wave which is well"
  Locate 38, 03: ? "visible here. Amazingly, they perfectly"
  Locate 39, 03: ? "transform the spherical standing waves."

  Locate 41, 03: ? "Matter exhibits wave properties and it"
  Locate 42, 03: ? "should especially undergo the Lorentz"
  Locate 43, 03: ? "transformations as a Doppler effect."
  Locate 44, 03: ? "It is the very basis of its behavior."
  Locate 60, 3:  ? "Pause: press P."
  Locate 61, 3:  ? "Press Esc. to quit."
  Locate 63, 3:  ? "Thanks to the creators of FreeBASIC."
  Locate 64,101: ? "This program may be freely distributed, copied or modified.";
End Sub
