' This is a FreeBasic program.
' Please download the IDE (editor) from: http://fbide.freebasic.net
' The source code was adapted to the 0.20.0b Compiler (2008) for Windows available from:
' http://www.freebasic.net/index.php/download
' It should still be compatible with previous compilers and other platforms.

Declare Sub Graphics()
Declare Sub Initialization()
Declare Sub Enlarge()
Declare Sub Title()

Dim Shared As String key, choice, display_title, file, bitmap_number
Dim Shared As String line56, line57, line58, line59, line60, line61, line62
Dim Shared As String line63a, line63b, line63c, line64a, line64b, line64c

Dim Shared As Single t_time, t_prime, x_coord, y_coord, red_frame, green_frame, x2_emitter, x3_emitter, x_point
Dim Shared As Single x_prime, y_prime, x_squared, y_squared, g_Lorentz, radius_Lorentz, half_beta
Dim Shared As Single beta, theta, amplitude, phase,  scanner, printer, center, variable, flash, c_speed

Dim Shared As Integer x, y, r, g, b, j, y_title, x1, x2, y1, y2, dark, bitmap
Dim Shared As Integer x1_emitter, x_center, y1_center, y2_center, y3_center, y4_center
Dim Shared As Integer matrix_page, work_page, visible_page, tone(720)
Dim Shared As Integer display_width, radius, display_height, line_number, zero_point
Dim Shared As Integer x_mouse, y_mouse, wheel, click, image, images, lambda, speed
Dim Shared As Integer Ptr image_1, image_2, image_3, image_4, image_5, image_6
Dim Shared As Integer Ptr material_body, contracted_body, x_0, x_10, t_prime_BC

Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(75,75,75)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)
Const pi = 4 * Atn(1), double_pi = 8 * Atn(1)

bitmap = 0                                                            'bitmap = 1 creates an image sequence.
work_page = 1
Screen 21,24,3
Initialization()

Do
  Swap visible_page, work_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  Graphics()                                                          'wave display.
  key = Inkey                                                         'keyboard management.
  If Len(key) Then
    If key = Chr(27) Or key = Chr(255)+"k" Then End                   'quit.
    key = Ucase(key)
    Select Case key
      Case "I": Initialization()
      Case "M": Run "WaveMechanics00.exe"                             'main menu.
      Case "P"                                                        'pause.
        Screenset visible_page
        Color red, background
        Locate 61, 59: ? "Paused. Press any key to resume.           "
        Sleep
      Case "R"                                                        'reversing the process for experiments.
        If image > 10 Then image -= 3 Else image = 0
    End Select
    Do: Loop While Len(Inkey)                                         'clear buffer.
  End If

'***********************************************************************************************************************
' MOUSE MANAGEMENT.
'***********************************************************************************************************************

  Getmouse x_mouse, y_mouse, wheel, click
  line_number = .5 + y_mouse / 16
  If line_number < 56 Or x_mouse < 453 Or x_mouse > 836 Then line_number = 0
  Color green_text, white
  Locate line_number, 58

  Select Case line_number

    Case 63
      Select Case x_mouse
        Case Is > 835
        Case Is > 711: Locate 63, 90: ? line63c: Sleep 500            'slow.
                       If click > 0 Then Sleep 2000                   'slower.
        Case Is > 607: Locate 63, 77: ? line63b
                       If click > 0 Then Run "WaveMechanics00.exe"    'main menu.
        Case Is > 423: Locate 63, 58: ? line63a
                       If click > 0 Then                              'initialization.
                         Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
                         choice = "C"
                         Initialization()
                       End If
      End Select

    Case 64  
      Select Case x_mouse
        Case Is > 835
        Case Is > 711: Locate 64, 90: ? line64c;
                       If click > 0 Then Run "WaveMechanics08.exe"    'next program.
        Case Is > 607: Locate 64, 77: ? line64b;
                       If click > 0 Then End                          'quit.
        Case Is > 423: Locate 64, 58: ? line64a;
                       If click > 0 Then Run "WaveMechanics06.exe"    'previous program.
      End Select
  End Select

  If bitmap > 0 Then                                                  'initialize bitmap = 1 for bitmap sequence.
    Select Case bitmap
      Case Is < 10: bitmap_number = "000"
      Case Is < 100: bitmap_number = "00"
      Case Is < 1000: bitmap_number = "0"
      Case Is < 10000: bitmap_number = ""
    End Select
    file = "capture_" + bitmap_number + Str(bitmap) + ".bmp"
    Color red, background
    Locate 48, 2: Print file
    Locate 49, 2: Print "A bitmap sequence is being created in the current directory."
    Bsave file, 0
    bitmap += 1
    If bitmap > 1500 Then End
  End If

Loop


'***********************************************************************************************************************
'SUB GRAPHICS  -  DISPLAYING EMITTERS AND MATERIAL BODIES A, B, and C.
'***********************************************************************************************************************
Sub Graphics():

  If scanner < -1 Then
    Sleep(10)                                                         'allowing time for CPU.
    Exit Sub
  Else
    Color black, white
    t_time = image / images                                           'Lorentz's t time in 2 pi units.
    If image = 232 Then
      Sleep
      key = Inkey
      Initialization()
      If key = "r" Or key = "R" Then image = 190
      If key = Chr(27) Then End
      Exit Sub
    End If

' **********************************************************************************************************************
' MATERIAL BODIES.
' **********************************************************************************************************************

'   x_point is the center of A, which is stationary and untransformed (x = 0).
    x_point = zero_point

    If x_point + radius < scanner Then
      Get(0, y4_center - radius)-(2 * radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      Put (x_point - 30, y1_center - 85), image_1, Pset
    Elseif x_point - radius < scanner Then
      Get(0, y4_center - radius)-(scanner - x_point + radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      If scanner + 30 > x_point Then
        Put (x_point - 30, y1_center - 85), image_1, Pset
      End If
    End If

'   A' is also stationary and untransformed. A and A' are 10 wavelengths apart on the x axis.
    x_point = zero_point + 10 * lambda

    If x_point + radius < scanner Then
      Get(0, y4_center - radius)-(2 * radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      Put (x_point - 30, y1_center - 85), image_1, Pset
      Put (x_point - 15, y1_center - 85), image_4, Pset
    Elseif x_point - radius <= scanner Then
      Get(0, y4_center - radius)-(scanner - x_point + radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      If scanner + 30 > x_point Then Put (x_point - 30, y1_center - 85), image_1, Pset
      If scanner + 30 > x_point Then Put (x_point - 15, y1_center - 85), image_4, Pset
    End If


'   B is moving forward (v = .5 c) and its center is located at x = 0 before undergoing the Lorentz transformations.
    x_coord = 0
    x_prime = g_Lorentz * x_coord + -beta * t_time                    'according to Lorentz reversed transformations,
    x_point = zero_point + x_prime * lambda                           '  => using -beta because it is moving backward.

    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 85), image_2, Pset
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 85), image_2, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " x'=  ##.#      "; x_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y2_center - 32), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
      t_prime = g_Lorentz * t_time - -beta * x_coord                  'Lorentz's reversed time equation.
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " t'=  ##.#      "; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y2_center + 15), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

'   B' is moving forward along with B, APPARENTLY 10 wavelengths apart.
    x_coord += 10                                                     'absolute distance before contraction.
    x_prime = g_Lorentz * x_coord + -beta * t_time                    'according to Lorentz reversed transformations.
    x_point = zero_point + x_prime * lambda
    If image = 174 Then flash = x_point

    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 85), image_2, Pset
        Put (x_point - 15, y2_center - 85), image_4, Pset        
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 85), image_2, Pset
        Put (x_point - 15, y2_center - 85), image_4, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " x'=  ##.#      "; x_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y2_center - 32), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
      t_prime = g_Lorentz * t_time - -beta * x_coord                  'Lorentz's reversed time equation (backward: -beta).
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " t'=  ##.#      "; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y2_center + 15), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

'   C is moving backward (v = .5 c) and its center is located at x = 0 before undergoing the Lorentz transformations.
    x_coord = 0
    x_prime = g_Lorentz * x_coord + beta * t_time                     'according to Lorentz reversed transformations.
    x_point = zero_point + x_prime * lambda
    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body'***************************
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 85), image_3, Pset
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 85), image_3, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1
      If x_prime < -9.9 Then
        Print Using " x'= +##.#      "; x_prime                       'minus sign anomaly.
      Elseif x_prime <= 0 Then
        Print Using " x'=  +#.#      "; x_prime
      End If
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y3_center - 32), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
      t_prime = g_Lorentz * t_time - beta * x_coord                   'Lorentz's reversed time equation (backward: -beta).
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " t'=  ##.#      "; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y3_center + 15), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

'   C' is moving along with C, APPARENTLY 10 wavelengths apart.
    x_coord += 10
    x_prime = g_Lorentz * x_coord + beta * t_time
    x_point = zero_point + x_prime * lambda + 1
    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body'***************************
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 85), image_3, Pset
        Put (x_point - 15, y3_center - 85), image_4, Pset        
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 85), image_3, Pset
        Put (x_point - 15, y3_center - 85), image_4, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " x'=  ##.#      "; x_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y3_center - 32), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
      t_prime = g_Lorentz * t_time - beta * x_coord                   'Lorentz's reversed time equation (backward: -beta).
      Line(0, 0)-(128, 32), white, bf
      Locate 2, 1: Print Using " t'=  ##.#      "; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y3_center + 15), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

    Line(0, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), background, bf' erasing matrix for material bodies.
    For j = zero_point - 10 * lambda To zero_point + 20 * lambda Step lambda
      Line(j - 1, y1_center - 10)-(j + 1, y1_center + 10), black, bf  'axial scale.
    Next
    Line(zero_point - 10 * lambda, y1_center - 1)-(zero_point + 20 * lambda, y1_center + 1), black, bf
    Line(zero_point - 1 - 10 * lambda, y1_center - 34)-(zero_point + 1 - 10 * lambda, y3_center - radius), black, bf
    Line(zero_point - 1, y1_center - 34)-(zero_point + 1, y2_center - radius), black, bf
    Line(zero_point - 1 + 10 * lambda, y1_center - 34)-(zero_point + 1 + 10 * lambda, y2_center - radius), black, bf
    Line(zero_point - 1 + 20 * lambda, y1_center - 34)-(zero_point + 1 + 20 * lambda, y2_center - radius), black, bf
    
    Locate 8, 51: Print Using "t = ##.#  seconds."; t_time           'displaying absolute time.
    Line(392, 110)-(470, 128), black, b
    Line(393, 111)-(469, 127), black, b
    Locate 8, 81: Print Using "t = ##.#  seconds."; t_time
    Line(632, 110)-(710, 128), black, b
    Line(633, 111)-(711, 127), black, b
    
    Locate 5, Int(zero_point / 8) - 4: Print " x = 0 "
    Line(zero_point - 45, 62)-(zero_point + 13, 81), black, b
    Line(zero_point - 44, 63)-(zero_point + 12, 80), black, b
    Locate 5, Int((zero_point - 10 * lambda) / 8) - 6: Print " x = -10 light-seconds."
    Line(zero_point - 10 * lambda - 61, 62)-(zero_point - 10 * lambda + 125, 81), black, b
    Line(zero_point - 10 * lambda - 60, 63)-(zero_point - 10 * lambda + 124, 80), black, b
    Locate 5, Int((zero_point + 10 * lambda) / 8) - 5: Print " x = 10 "
    Line(zero_point + 10 * lambda - 53, 62)-(zero_point + 10 * lambda + 13, 81), black, b
    Line(zero_point + 10 * lambda - 52, 63)-(zero_point + 10 * lambda + 12, 80), black, b
    Locate 5, Int((zero_point + 20 * lambda) / 8) - 5: Print " x = 20 "
    Line(zero_point + 20 * lambda - 53, 62)-(zero_point + 20 * lambda + 13, 81), black, b
    Line(zero_point + 20 * lambda - 52, 63)-(zero_point + 20 * lambda + 12, 80), black, b
    Line(0, 0)-(160, 48), background, bf
    If image < 175 Then
      Circle(zero_point, y2_center), c_speed * image, red,,, 1        'signal from B expanding at the speed of light.
    Else
      Circle(flash, y2_center), c_speed * (image - 173), green,,, 1   'signal from B'.
    End If
    If image = 174 or image = 201 Then Sleep(2000)                    'small pause.
    image += 1                                                        'to be converted into wave period (2 * pi units).
    Sleep(1)                                                          'regulator.
  End If
End Sub


'***********************************************************************************************************************
' SUB ENLARGE  -  ENLARGING CHARACTERS.
'***********************************************************************************************************************
Sub Enlarge()

  center = 8
  y_title = 16
  Line(0, 0)-(15, 47), white, bf
  Color black, white
  Locate 1,1: Print "A"
  For x1 = 0 To 7
    x2 = center + 2 * x1 - 8
    For y1 = 0 To 15
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), black
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), black
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), black
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), black
      End If
    Next
  Next
  image_1 = ImageCreate(16, 32)
  Get (0, 16)-(15, 47), image_1
  Line(0, 0)-(15, 47), white, bf

  Locate 1,1: Print "B"
  For x1 = 0 To 7
    x2 = center + 2 * x1 - 8
    For y1 = 0 To 15
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), black
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), black
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), black
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), black
      End If
    Next
  Next
  image_2 = ImageCreate(16, 32)
  Get (0, 16)-(15, 47), image_2
  Line(0, 0)-(15, 47), white, bf

  Locate 1,1: Print "C"
  For x1 = 0 To 7
    x2 = center + 2 * x1 - 8
    For y1 = 0 To 15
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), black
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), black
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), black
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), black
      End If
    Next
  Next
  image_3 = ImageCreate(16, 32)
  Get (0, 16)-(15, 47), image_3
  Line(0, 0)-(15, 47), white, bf

  Locate 1,1: Print "'"
  For x1 = 0 To 7
    x2 = center + 2 * x1 - 8
    For y1 = 0 To 15
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), black
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), black
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), black
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), black
      End If
    Next
  Next
  image_4 = ImageCreate(16, 32)
  Get (0, 16)-(12, 47), image_4
  Line(0, 0)-(15, 47), white, bf

  Locate 1,1: Print "x= 0 "
  For x1 = 0 To 39
    x2 = center + 2 * x1
    For y1 = 0 To 15
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), red
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), red
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), red
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), red
      End If
    Next
  Next
  Line(4, 15)-(88, 48), red, b
  Line(5, 16)-(87, 47), red, b
  x_0 = ImageCreate(85, 38)
  Get (4, 15)-(88, 48), x_0
  Line(4, 15)-(88, 48), background, bf

  Locate 1,1: Print "x= 10"
  For x1 = 0 To 39
    x2 = center + 2 * x1
    For y1 = 0 To 15
      If Point(x1, y1) = black Then
        y2 = y_title + 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), red
        If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), red
        If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), red
        If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), red
      End If
    Next
  Next
  Line(4, 15)-(88, 48), red, b
  Line(5, 16)-(87, 47), red, b
  x_10 = ImageCreate(85, 38)
  Get (4, 15)-(88, 48), x_10
  Line(4, 15)-(88, 48), background, bf
  Locate 1,1: Print "     "
End Sub


'***********************************************************************************************************************
' SUB INITIALIZATION.
'***********************************************************************************************************************
Sub Initialization()
  matrix_page = 2
  Windowtitle "WaveMechanics07 - The Lorentz Transformations."
  beta = -.5
  theta = Asin(beta)                                                  'transverse wave angle.
  g_Lorentz = Cos(theta)                                              'Lorentz contraction factor.

' Poincaré's law of speed addition:                                    beta" = (beta + beta') / (1 + beta * beta')
' beta and beta' being the same, the formula simplifies to:            beta" = 2 * beta / (1 + beta ^ 2)
'                                                                      half_beta = beta / 2 = .25 would be incorrect.
  half_beta = .267949192
  lambda = 24                                                         'wavelength.
  radius = 4 * lambda
  material_body =   ImageCreate(2 * radius + 1, 2 * radius + 1)       'uncontracted material body (template).
  contracted_body = ImageCreate(2 * radius + 1, 2 * radius + 1)
  image = -1
  images = 10
  c_speed = lambda / images                                           'speed of light in pixels.
  display_width = 480
  display_height = 240
  radius_Lorentz = g_Lorentz * radius
  zero_point = 460
  x1_emitter = zero_point + 20 * lambda
  red_frame = zero_point
  green_frame = zero_point
  y1_center = 4 * lambda
  y2_center = y1_center + 8.5 * lambda
  y3_center = y2_center + 8.5 * lambda
  y4_center = 850
  scanner = 1430
  printer = 1415
'  scanner = 870
'  printer = 910
  
  line63a = " I - Initialize.   "
  line63b = " M - Menu.   "
  line63c = " Slow.         "
  line64a = " Previous Program. "
  line64b = " Quit (Esc.) "
  line64c = " Next Program. "

  Screenset matrix_page, matrix_page
  Color black, background: Cls
  Color green_text
  Locate 56,58: ? line56
  Locate 57,58: ? line57
  Locate 58,58: ? line58
  Locate 59,58: ? line59
  Locate 61,58: ? line61
  Locate 63,59: ? "I - Initialize.    M - Menu.    Slow."
  Locate 64,59: ? "Previous Program.  Quit (Esc.)  Next Program.";
  Line(640 - 190, 720)-(640 + 190, 1079), black, b
  Color Blue: Locate 13,53
  Select Case choice
    Case "A": 
    Case "B": 
    Case "C": 
    Case "D": 
  End Select
  Color black
  Locate 2, 110: ? "glafreniere.com";
  Locate 3, 110: ? "Sept. 21, 2009"

  Locate 15, 47 : Print"The Lorentz Transformations"
  Line(368, 240)-(582, 240), black
  Locate 17, 47 : Print"x' = g x + b t      y'= y"
  Locate 18, 47 : Print"t' = g t - b x      z'= z"
  Locate 20, 47 : Print"b (beta) = v / c = 0.5"
  Locate 21, 47 : Print"g = Sqr(1 - b^2) = 0.866"
  Locate 23, 47 : Print"g stands for Lorentz's"
  Locate 24, 47 : Print"contraction factor."
  Locate 27, 77 : Print"A and A' are stationary. No contraction occurs."
  Locate 29, 77 : Print"B and B' are moving forward. Contraction is .866"
  Locate 30, 77 : Print"The beta speed is positive: b = 0.5"
  Locate 31, 77 : Print"Starting point for B:  x = 0   x'= 0"
  Locate 32, 77 : Print"Starting point for B': x = 10  x'= 8.66"
  Locate 34, 77 : Print"C and C' are moving backward. Same .866 contraction."
  Locate 35, 77 : Print"The beta speed is negative: b = -0.5"
  Locate 36, 77 : Print"Starting point for C:  x = 0   x'= 0"
  Locate 37, 77 : Print"Starting point for C': x = 10  x'= 8.66"
  Locate 2,  132: Print "     THE TIME SHIFT"
  Locate 4,  132: Print "Observer B' adjusts his"
  Locate 5,  132: Print "clock upon reception of the"
  Locate 6,  132: Print "signal. However, he adds 10"
  Locate 7,  132: Print "seconds in order to compen-"
  Locate 8,  132: Print "sate the delay because he"
  Locate 9,  132: Print "erroneously checked that the"
  Locate 10, 132: Print "distance to B is 10 light-"
  Locate 11, 132: Print "seconds."
  Locate 13, 132: Print " VALIDATING THE TIME SHIFT"
  Locate 15, 132: Print "Because the signal duration"
  Locate 16, 132: Print "on a go and return trip was"
  Locate 17, 132: Print "20 seconds according to his"
  Locate 18, 132: Print "clock, observer B is now"
  Locate 19, 132: Print "sure that the distance to B'"
  Locate 20, 132: Print "is 10 light-seconds. What's"
  Locate 21, 132: Print "more, B' may include his 10"
  Locate 22, 132: Print "second time to the signal so"
  Locate 23, 132: Print "that B can check that their"
  Locate 24, 132: Print "clocks are synchronized."

  Locate 60,85: Print "Reverse: Press R."
  Locate 61,59: Print "Please click slowly!        Pause: Press P."
  Title()
  Enlarge()
  
  x_center = radius                                                   'the contraction is relative to the center.
  Line(0, y4_center - radius)-(2 * radius, y4_center + radius), black, bf
  Circle(radius, y4_center), radius - 5, white,,, 1
  Paint(radius, y4_center), white, white
  Line(x_center - 1, y4_center - radius)-(x_center + 1, y4_center + radius), black, bf
  Line(0, y4_center - 1)-(2 * radius, y4_center + 1), black, bf

  x_center = 3 * radius                                               'the contraction is relative to the center.
  Line(x_center - g_Lorentz * radius, y4_center - radius)-(x_center + g_Lorentz * radius, y4_center + radius), black, bf
  Circle(x_center, y4_center), radius - 5, white,,, 1 / g_Lorentz
  Paint(x_center, y4_center), white, white
  t_prime_BC = ImageCreate(22, 130)
  Line(x_center - 1, y4_center - radius)-(x_center + 1, y4_center + radius), black, bf
  Line(x_center - g_Lorentz * radius, y4_center - 1)-(x_center + g_Lorentz * radius, y4_center + 1), black, bf
  Line(0,   720)-(1280, 720), black

  Color dark_gray, background
  Locate 63,  2: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 64,  2: ? "Thanks to the creators of FreeBASIC.";
  Locate 63,121:? "Sept. 21, 2009. This program may be"
  Locate 64,121:? "freely distributed, copied or modified.";
  Color black
End Sub


'***********************************************************************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'***********************************************************************************************************************

Sub Title()
  display_title = "The Lorentz Transformations"
  Locate 1,1: ? display_title
  center = 640
  y_title = 660
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
    Next                                                              'separating invasive characters such as capital M.
    If (x1+1) Mod 8 Then Else Line(x2+1, y_title)-(x2+1, y_title + 34), background
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
