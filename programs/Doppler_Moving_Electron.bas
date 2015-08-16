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
Dim Shared As Integer x_width = 1279, y_height = 719, x_center, y_center, x_prime, y_prime
Dim Shared As Integer matrix_page, work_page, visible_page, luminance, click, wheel
Dim Shared As Integer r, g, b, j, x, y, x_text, bitmap, iteration, lambda = 120
Dim Shared As Single c_speed, x_coord, y_coord, g_Lorentz, x_squared, beta = .6
Dim Shared As Single t_time, t_prime, radian, amplitude, distance
Dim Shared As String in_key, file, bitmap_number

Screen 21,24,3
Windowtitle " The Moving Electron and the Lorentz Transformations"
visible_page = 0: work_page = 1: matrix_page = 2: bitmap = 0          'set bitmap = 1 for BMP image sequence.
Initialization()

Do
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  t_time =    c_speed * iteration / lambda
  
'*********************************************************************
' DISPLAYING THE WAVES USING THE LORENTZ TRANSFORMATIONS.
'*********************************************************************

  For x_prime = x_text To x_width                                     'the variable x' as integer is already known and x must rather be recovered in order to avoid pixelation (aliasing).
    x_coord = (x_prime - beta * t_time) / g_Lorentz                   'this is Lorentz's x' ORIGINAL equation except for x and x' which are being swapped: x' = (x - b * t) / g
    x_coord -= 500 + beta * c_speed * iteration / g_Lorentz                       'moving the original frame of reference. Additionally, x must be converted into wavelength units (x / lambda below).
    t_prime = g_Lorentz * t_time - beta * (x_coord / lambda)          'this is Lorentz's REVERSED t' equation except for t and t' which are being swapped: t' = (t - b * x) / g
    x_squared = x_coord^2                                             '    =>> Lorentz definitely confused the variables t and t' so that the sign should also be positive.
    For y = 0 To y_height                                             '    =>> In addition, x' and t' should stand for x coordinate and t time in the moving frame of reference.
      distance = Sqr(x_squared + (y - y_center)^2)
      radian = 2 * pi * distance / lambda
      If radian Then amplitude = Sin(radian)/radian Else amplitude=1  'Jocelyn Marcotte's electron formula ("x" as distance being in radian units, sinus cardinalis = Sin(x) / x).
      luminance = 2500 * Sin(2 * pi * t_prime) * amplitude            't_prime must be converted into radian units.
      If luminance > 0 Then                                           'yields complementary emerald green (positive amplitude) and magenta using constant 50% blue.
        g = luminance                                                 '    =>> The RGB green color alone is way too bright. It should be splitted into yellow and emerald green.
        b = luminance / 2                                             '    =>> The RGB system is deficient. A 4-color display (red, yellow, emerald green and blue) would perform better.
        If luminance > 255 Then r = luminance - 255 Else r = 0
      Else
        r = -luminance
        b = -luminance / 2
        If luminance < -255 Then g = -luminance - 255 Else g = 0
      End If
      If r > 255 Then r = 255
      If g > 255 Then g = 255
      If b > 255 Then b = 255
      y_prime = y                                                     'in accordance with Lorentz's original y' = y.
      Pset(x_prime, y_prime), Rgb(r,g,b)                              'printing the wave area.
    Next
  Next
  
  Color black, background
  Locate 53, 03: Print "Iteration... "; iteration
  Locate 50, 147: Print "May 1st, 2010"
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
    Sleep
    If Inkey = Chr(27) Then End
  Elseif in_key <> "" Then End                                        'execute now.
  End If
  iteration += 1
Loop


Sub Initialization()
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Line(0,0)-(x_width, y_height), black, bf
  g_Lorentz = Sqr(1 - beta^2)
  c_speed = 2                                                         'pixel per loop units.
  x_text = 342
  x_center = x_width - .5 * (x_width - x_text)
  y_center = .5 * y_height
  iteration = 0
  Color black, background
  Line(0, 0)-(x_text, y_height), background, BF                       'opaque text box.
  Line(0, 0)-(x_text, y_height), white, B 
  Line(1, 1)-(x_text-1, y_height-1), white, B
  Line(2, 2)-(x_text-2, y_height-2), white, B
  Locate 02, 03: ? "         The Moving Electron          "
  Locate 03, 03: ? "    and the Lorentz Transformations   "
  Locate 05, 03: ? "Normalized speed beta: "; Chr(225);" = v / c = 0.6"
  Locate 06, 03: ? "Lorentz's contraction factor:"
  Locate 08, 10: ? "g = Sqr(1 - ";Chr(225);Chr(253);") = 0.8"
  Locate 10, 03: ? "The Lorentz reversed transformations:"
  Line(59, 167)-(227, 214), black, B
  Line(60, 168)-(226, 213), gray, B
  Line(61, 169)-(226, 213), white, BF
  Color black, white
  Locate 12, 10: ? "x' = g * x + ";Chr(225);" * t"
  Locate 13, 10: ? "t' = g * t - ";Chr(225);" * x"
  Color black, background
  Locate 15, 03: ? "Inversely (Poincare's method):"
  Locate 17, 10: ? "x = g * x' - ";Chr(225);" * t'"
  Locate 18, 10: ? "t = g * t' + ";Chr(225);" * x'"
  Locate 20, 03: ? "However, x and t may also be given by:"
  Locate 22, 10: ? "x = (x'- ";Chr(225);" * t) / g"
  Locate 23, 10: ? "t = (t'+ ";Chr(225);" * x) / g"

  Locate 25, 03: ? "This equation set is almost identical to"
  Locate 26, 03: ? "Lorentz's original one. Because it works"
  Locate 27, 03: ? "perfectly, it turns out that Lorentz did"
  Locate 28, 03: ? "not notice that Voigt had confused the"
  Locate 29, 03: ? "variables t and t'. They must be swapped"
  Locate 30, 03: ? "and the plus sign is also required."
  Locate 32, 03: ? "Definitely, the Lorentz transformations"
  Locate 33, 03: ? "are all about the Doppler effect. They"
  Locate 34, 03: ? "more specifically show a contraction"
  Locate 35, 03: ? "in the direction of motion and a local"
  Locate 36, 03: ? "time, more exactly a phase wave. This"
  Locate 37, 03: ? "relativistic Doppler effect applies to"
  Locate 38, 03: ? "the electron, which is responsible for"
  Locate 39, 03: ? "the emission of light and radio waves ."

  Locate 41, 03: ? "Matter exhibits wave properties and it"
  Locate 42, 03: ? "should especially undergo the Lorentz"
  Locate 43, 03: ? "transformations as a Doppler effect."
  Locate 44, 03: ? "It is the very basis of its behavior."
  Locate 60, 3:  ? "Pause: press P."
  Locate 61, 3:  ? "Press Esc. to quit."
  Locate 63, 3:  ? "Thanks to the creators of FreeBASIC."
  Locate 64,101: ? "This program may be freely distributed, copied or modified.";
End Sub
