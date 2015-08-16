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
Dim Shared As Single beta, theta, amplitude, phase,  scanner, printer, center, variable, c

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
    If bitmap > 1300 Then End
  End If

Loop


'***********************************************************************************************************************
'SUB GRAPHICS  -  DISPLAYING EMITTERS AND MATERIAL BODIES A, B, and C.
'***********************************************************************************************************************
Sub Graphics():


  If scanner < -1 Then
    Sleep(10)                                                         'time for CPU.
    Exit Sub
  Else
    Color black, background
    For x = -radius To radius                                         'full horizontal scan (no symmetry).
      x_coord = x / lambda                                            'Lorentz's untransformed x coordinate in wavelength units.
      x_squared = x_coord ^ 2
      t_time = image / images                                         'Lorentz's t time in 2 pi units. It is actually the
                                                                      'emitter pulsating PERIOD according to the current image.

' DISPLAYING THE STATIONARY WAVE EMITTER (NO DOPPLER EFFECT) ***********************************************************
  
      x_point = x + x1_emitter
      If x_point < scanner + 1 Then    
        For y = 0 To radius                                           'wave generator; one half vertical scan (axial symmetry).
          y_coord = y / lambda                                        'Lorentz's actual y coordinate in wavelength units.
          phase = Sqr(x_squared + y_coord ^ 2)                        'delay according to actual distance in wave period units.
          amplitude = Sin(double_pi * (phase - t_time)) / Sqr(phase)  'sinusoidal wave; dimming according to distance/phase.
          amplitude = 1.5 * amplitude 
          If amplitude > 0 Then
            r = 0: g = 255 * amplitude: b = g / 2                     'color distribution.
            If g > 255 Then r = g - 255: g = 255
            If r > 255 Then r = 255
            If b > 255 Then b = 255
          Else
            r = -255 * amplitude: g = 0: b = r / 2
            If r > 255 Then g = r - 255:r = 255
            If g > 255 Then g = 255
            If b > 255 Then b = 255
          End If
          If phase < 4 Then
            Pset(x_point, y1_center + y), Rgb(r,g,b)                  'displaying the stationary emitter (no Doppler effect).
            Pset(x_point, y1_center - y), Rgb(r,g,b)
          End If
        Next
      End If
      If x = 10 And x_point < scanner + 20 Then                       'lambda scale.
        Line(x_point - 11, y1_center - radius)-(x_point - 9, y1_center + radius), white, bf
        For j = y1_center - radius To y1_center + radius Step lambda
          Line(x_point - 16, j - 1)-(x_point - 5, j + 2), white, bf
        Next
      End If
  
' DISPLAYING THE TRANSFORMED WAVE EMITTER (moving backward, hence -beta) USING THE LORENTZ TRANSFORMATIONS *************
      x_prime = g_Lorentz * x_coord + -beta * t_time                  'Lorentz's x' coordinate in wavelength units.
      t_prime = g_Lorentz * t_time  - -beta * x_coord                 'Lorentz's t' time (actually the emitter phase) in 2 pi units.
' **********************************************************************************************************************
  
      If x = 0 Then x2_emitter = x1_emitter + x_prime * lambda
      x_point = x1_emitter + x_prime * lambda
  
      If x_point < scanner + 1 Then    
        For y = 0 To radius
          y_coord = y / lambda                                        'Lorentz's actual y coordinate in wavelength units.
          phase = Sqr(x_squared + y_coord ^ 2)                        'delay according to actual distance in wave period units.
          amplitude = Sin(double_pi * (phase - t_prime)) / Sqr(phase) 'amplitude according to t_prime.
          amplitude = 1.5 * amplitude 
          y_prime = y                                                 'in accordance with Lorentz's y' = y.
  
          If amplitude > 0 Then
            r = 0: g = 255 * amplitude: b = g / 2                     'color distribution.
            If g > 255 Then r = g - 255: g = 255
            If r > 255 Then r = 255
            If b > 255 Then b = 255
          Else
            r = -255 * amplitude: g = 0: b = r / 2
            If r > 255 Then g = r - 255:r = 255
            If g > 255 Then g = 255
            If b > 255 Then b = 255
          End If
          If phase < 4 Then
            Pset(x_point, y2_center + y_prime), Rgb(r,g,b)            'displaying the emitter moving backward (Doppler effect).
            Pset(x_point, y2_center - y_prime), Rgb(r,g,b)
          End If
        Next
      End If
      If x = 10 And x_point < scanner + 20 Then                       'lambda scale.
        Line(x_point - 11, y2_center - radius)-(x_point - 9, y2_center + radius), white, bf
        For j = y2_center - radius To y2_center + radius Step lambda
          Line(x_point - 16, j - 1)-(x_point - 5, j + 2), white, bf
        Next
      End If
  
  
' DISPLAYING THE MOVING FORWARD TRANSFORMED WAVE EMITTER USING THE LORENTZ TRANSFORMATIONS *****************************
      x_prime = g_Lorentz * x_coord + beta * t_time                   'Lorentz's x' coordinate in wavelength units.
      t_prime = g_Lorentz * t_time  - beta * x_coord                  'Lorentz's t' time (actually the emitter phase) in 2 pi units.
' **********************************************************************************************************************
  
      If x = 0 Then x3_emitter = x1_emitter + x_prime * lambda
      x_point = x1_emitter + x_prime * lambda
  
      If x_point < scanner + 1 Then    
        For y = 0 To radius
          y_coord = y / lambda                                        'Lorentz's actual y coordinate in wavelength units.
          phase = Sqr(x_squared + y_coord ^ 2)                        'delay according to actual distance in wave period units.
          amplitude = Sin(double_pi * (phase - t_prime)) / Sqr(phase) 'amplitude according to t_prime.
          amplitude = 1.5 * amplitude 
          y_prime = y                                                 'in accordance with Lorentz's y' = y.
  
          If amplitude > 0 Then
            r = 0: g = 255 * amplitude: b = g / 2                     'color distribution.
            If g > 255 Then r = g - 255: g = 255
            If r > 255 Then r = 255
            If b > 255 Then b = 255
          Else
            r = -255 * amplitude: g = 0: b = r / 2
            If r > 255 Then g = r - 255:r = 255
            If g > 255 Then g = 255
            If b > 255 Then b = 255
          End If
          If phase < 4 Then
            Pset(x_point, y3_center + y_prime), Rgb(r,g,b)            'displaying the emitter moving forward (Doppler effect).
            Pset(x_point, y3_center - y_prime), Rgb(r,g,b)
          End If
        Next
      End If
      If x = 10 And x_point < scanner + 20 Then                       'lambda scale.
        Line(x_point - 11, y3_center - radius)-(x_point - 9, y3_center + radius), white, bf
        For j = y3_center - radius To y3_center + radius Step lambda
          Line(x_point - 16, j - 1)-(x_point - 5, j + 2), white, bf
        Next
      End If
    Next


' **********************************************************************************************************************
' MATERIAL BODIES ******************************************************************************************************
' **********************************************************************************************************************

'   x_point is the center of A, which is stationary and untransformed(x = 0).
    x_point = zero_point

    If x_point + radius < scanner Then
      Get(0, y4_center - radius)-(2 * radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      Put (x_point - 30, y1_center - 100), image_1, Pset
    Elseif x_point - radius < scanner Then
      Get(0, y4_center - radius)-(scanner - x_point + radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      If scanner + 30 > x_point Then
        Put (x_point - 30, y1_center - 100), image_1, Pset
      End If
    End If

'   A' is also stationary and untransformed. A and A' are 10 wavelengths apart on the x axis.
    x_point = zero_point + 10 * lambda

    If x_point + radius < scanner Then
      Get(0, y4_center - radius)-(2 * radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      Put (x_point - 30, y1_center - 100), image_1, Pset
      Put (x_point - 15, y1_center - 100), image_4, Pset
    Elseif x_point - radius <= scanner Then
      Get(0, y4_center - radius)-(scanner - x_point + radius, y4_center + radius), material_body
      Put(x_point - radius, y1_center - radius), material_body, Pset
      If scanner + 30 > x_point Then Put (x_point - 30, y1_center - 100), image_1, Pset
      If scanner + 30 > x_point Then Put (x_point - 15, y1_center - 100), image_4, Pset
    End If

    Locate 11, 15: Print Using "t'=  ##.#  sec."; t_time
    Line(144, 158)-(190, 176), black, b
    Line(145, 159)-(189, 175), black, b

'   B is moving backward (v = .5 c) but its center is located at x = 0 before undergoing the Lorentz transformations.
    x_coord = 0
    x_prime = g_Lorentz * x_coord + -beta * t_time                    'according to Lorentz reversed transformations,
    x_point = zero_point + x_prime * lambda                           '  => using -beta because it is moving backward.

    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 100), image_2, Pset
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 100), image_2, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      t_prime = g_Lorentz * t_time - -beta * x_coord                  'Lorentz's reversed time equation (backward: -beta).
      Locate 2, 1: Print Using " t =  ##.#  sec."; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y2_center + 40), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

'   B' is moving backward along with B, APPARENTLY 10 wavelengths apart.
    x_coord += 10                                                     'absolute distance before contraction.
    x_prime = g_Lorentz * x_coord + -beta * t_time                    'according to Lorentz reversed transformations.
    x_point = zero_point + x_prime * lambda
    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 100), image_2, Pset
        Put (x_point - 15, y2_center - 100), image_4, Pset        
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y2_center - radius), contracted_body, Pset
        Put (x_point - 30, y2_center - 100), image_2, Pset
        Put (x_point - 15, y2_center - 100), image_4, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      t_prime = g_Lorentz * t_time - -beta * x_coord                  'Lorentz's reversed time equation (backward: -beta).
      Locate 2, 1: Print Using " t =  ##.#  sec."; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y2_center + 40), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

'   C is moving forward (v = .5 c) but its center is located at x = 0 before undergoing the Lorentz transformations.
    x_coord = 0
    x_prime = g_Lorentz * x_coord + beta * t_time                     'according to Lorentz reversed transformations.
    x_point = zero_point + x_prime * lambda
    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body'***************************
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 100), image_3, Pset
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 100), image_3, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      t_prime = g_Lorentz * t_time - beta * x_coord                   'Lorentz's reversed time equation (forward: +beta).
      Locate 2, 1: Print Using " t'=  ##.#  sec."; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y3_center + 40), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

'   C' is moving along with C, APPARENTLY 10 wavelengths apart.
    x_coord -= 10
    x_prime = g_Lorentz * x_coord + beta * t_time
    x_point = zero_point + x_prime * lambda + 1
    If x_point - radius_Lorentz <= scanner Then
      If x_point < scanner - radius_Lorentz - 1 Then
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), contracted_body'***************************
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 100), image_3, Pset
        Put (x_point - 15, y3_center - 100), image_4, Pset        
      Else
        Get(x_center - radius_Lorentz, y4_center - radius)-(x_center + radius_Lorentz - (radius_Lorentz - (scanner - x_point)), y4_center + radius), contracted_body
        Put (x_point - radius_Lorentz, y3_center - radius), contracted_body, Pset
        Put (x_point - 30, y3_center - 100), image_3, Pset
        Put (x_point - 15, y3_center - 100), image_4, Pset
      End If  
    End If

    If scanner > x_point - 67 Then
      t_prime = g_Lorentz * t_time - beta * x_coord                   'Lorentz's reversed time equation (forward: +beta).
      Locate 2, 1: Print Using " t'=  ##.#  sec."; t_prime
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(x_point - 67, y3_center + 40), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If

    Line(0, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), background, bf' erasing matrix for material bodies.
    Locate 47, 3: Print "Hendrick A. Lorentz stated In 1904 that the Michelson"
    Locate 48, 3: Print "interferometer should undergo a contraction according"
    Locate 49, 3: Print "to a factor which is given by: Sqr(1 - beta ^ 2). He"
    Locate 50, 3: Print "also discovered that no contraction should occur on"
    Locate 51, 3: Print "transverse axes and that clocks should run slower. A"
    Locate 52, 3: Print "local time (time shift) should finally appear. "
    Locate 54, 3: Print "This program is a clear demonstration that Lorentz"
    Locate 55, 3: Print "was right. Introducing a space/time transformation"
    Locate 56, 3: Print "was an enormous error. It was totally useless, given"
    Locate 57, 3: Print "the fact that under those conditions, the observers"
    Locate 58, 3: Print "A, B and C shown above cannot detect which one is at"
    Locate 59, 3: Print "rest. In addition, Lorentz's version of Relativity"
    Locate 60, 3: Print "is capable of conciliating more than two results"
    Locate 61, 3: Print "without having to deal with so-called paradoxes"
    Locate 62, 3: Print "which are actually contradictions. It turns out"
    Locate 63, 3: Print "that Lorentz's Relativity is truly amazing."
'***********************************************************************************************************************
' SCANNING AND PRINTING.
'***********************************************************************************************************************

    For j = zero_point - 14 * lambda To zero_point + 15 * lambda Step lambda
      If j > scanner Then Exit For
      Line(j - 2, y1_center - 10)-(j + 2, y1_center + 10), blue, bf   'wavelength blue scale, will contract.
    Next
    If scanner > zero_point - 14 * lambda Then
      Line(zero_point - 14 * lambda, y1_center - 1)-(zero_point + 15 * lambda, y1_center + 1), blue, bf
    End If
    If scanner > zero_point - 10 * lambda - 3 Then
      Line(zero_point - 2 - 10 * lambda, y1_center - 40)-(zero_point + 2 - 10 * lambda, y1_center + 39), blue, bf
    End If
    If scanner > zero_point - 3 Then
      Line(zero_point - 2, y1_center - radius)-(zero_point + 2, y2_center), blue, bf
    End If
    If scanner > zero_point + 10 * lambda - 3 Then
      Line(zero_point - 2 + 10 * lambda, y1_center - radius)-(zero_point + 2 + 10 * lambda, y1_center + radius), blue, bf
    End If

    If scanner > zero_point - 75 Then                                 'displaying absolute time.
      Locate 11, 50: Print Using "t'=  ##.#  sec."; t_time            'will become t' after being scanned.
      Line(424, 158)-(470, 176), black, b
      Line(425, 159)-(469, 175), black, b
    End If

    If scanner > zero_point - 75 + 10 * lambda Then
      Locate 11, 85: Print Using "t'=  ##.#  sec."; t_time
      Line(704, 158)-(750, 176), black, b
      Line(705, 159)-(751, 175), black, b
    End If

    red_frame -= c * half_beta                                        'wavelength red scale, will not contract.
    If scanner > red_frame + 15 * lambda + 1 Then
      Line(red_frame - 14 * lambda, y2_center - 1)-(red_frame + 15 * lambda, y2_center + 1), red, bf
    Elseif scanner > red_frame - 14 * lambda Then
      Line(red_frame - 14 * lambda, y2_center - 1)-(scanner, y2_center + 1), red, bf
    End If
    If red_frame < scanner + 2 Then 
      Line(red_frame - 2, y2_center - 40)-(red_frame + 2, y2_center + 40), red, bf
    End If
    If red_frame < scanner + 43 Then Put(red_frame - 43, y2_center - 65), x_0, Pset
    If red_frame + 10 * lambda < scanner + 2 Then
      Line(red_frame + 10 * lambda - 2, y2_center - 40)-(red_frame + 10 * lambda + 2, y2_center + 40), red, bf
    End If
    If red_frame + 10 * lambda < scanner + 43 Then Put(red_frame + 10 * lambda - 43, y2_center - 65), x_10, Pset
    If red_frame - 10 * lambda < scanner + 2 Then
      Line(red_frame - 10 * lambda - 2, y2_center - 40)-(red_frame - 10 * lambda + 2, y2_center + 40), red, bf
    End If
    For j = red_frame - 14 * lambda To red_frame + 15 * lambda Step lambda
      If j > scanner Then Exit For
      Line(j - 2, y2_center - 10)-(j + 2, y2_center + 10), red, bf
    Next

'**************************************************************
'   The red scale origin is moving backward (half_beta = .267949192)
'   and its center is located at x = 0 before undergoing the Lorentz transformations.
'    x_coord = 0
'    x_prime = g_Lorentz * x_coord + -half_beta * t_time               'according to Lorentz reversed transformations,
'    x_point = zero_point + x_prime * lambda                           '  => using -beta because it is moving backward.
    
    If scanner > red_frame - 67 Then
'      t_prime = g_Lorentz * t_time - -half_beta * x_coord             'Lorentz's reversed time equation (backward: -beta).
      Locate 2, 1: Print Using " t =  ##.#"; t_time
      Line(39, 14)-(86, 32), black, b
      Line(40, 15)-(85, 31), black, b
      Get(8, 14)-(128, 32), t_prime_BC
      Put(red_frame - 67, y2_center - 30), t_prime_BC, Pset
      Line(0, 14)-(128, 32), background, bf
    End If


    green_frame -= c * -beta                                          'wavelength green scale, will contract to .866.
    If scanner > green_frame + 15 * lambda + 1 Then
      Line(green_frame - 14 * lambda, y3_center - 1)-(green_frame + 15 * lambda, y3_center + 1), green, bf
    Elseif scanner > green_frame - 14 * lambda Then
      Line(green_frame - 14 * lambda, y3_center - 1)-(scanner, y3_center + 1), green, bf
    End If
    If green_frame < scanner + 2 Then 
      Line(green_frame - 2, y3_center - 40)-(green_frame + 2, y3_center + 40), green, bf
    End If
    If green_frame + 10 * lambda < scanner + 2 Then
      Line(green_frame + 10 * lambda - 2, y3_center - 40)-(green_frame + 10 * lambda + 2, y3_center + 40), green, bf
    End If
    If green_frame - 10 * lambda < scanner + 2 Then
      Line(green_frame - 10 * lambda - 2, y3_center - 40)-(green_frame - 10 * lambda + 2, y3_center + 40), green, bf
    End If
    For j = green_frame - 14 * lambda To green_frame + 15 * lambda Step lambda
      If j > scanner Then Exit For
      Line(j - 2, y3_center - 10)-(j + 2, y3_center + 10), green, bf
    Next

    scanner -= c / beta
    printer -= g_Lorentz * c / beta
    Line(scanner - 1, -1)-(scanner + 1, 720), black, b
    Line(printer - 1, -1)-(printer + 1, 720), black, b
    Line(scanner + 2, 0)-(printer - 2, 719),  gray, bf

    For j = 0 To 719
      tone(j) = Point(scanner, j)                                     'scanning work page.
    Next
    Screenset matrix_page                                             'printing matrix page.
    For j = 0 To 719
      Pset(printer, j), tone(j)
    Next
    image += 1                                                        'to be converted into wave period (2 * pi units).
    Screenset work_page

'scanner = -1
    If scanner < 0 Then
      scanner = -2
      Screenset matrix_page
      Line(0, 0)-(176, 719), background, bf                           'erasing unscanned area.
      Line(0, y4_center - radius)-(x_center + radius_Lorentz, y4_center + radius), background, bf'erasing material bodies.
      Locate 2, 2: Print "After being scanned, A and A' exhibit a .866 contraction"
      Locate 3, 2: Print "and a -beta * x time shift according to Lorentz's reversed"
      Locate 4, 2: Print "equations shown below. Because A' is 10 light-seconds in"
      Locate 5, 2: Print "front of A, his clock displays a -5 seconds time shift."
      Locate 7, 2: Print "The Time Scanner is capable"
      Locate 8, 2: Print "of accelerating the whole"
      Locate 9, 2: Print "system to the right (.5 c"
      Locate 10,2: Print "faster here) according to"
      Locate 11,2: Print "Poincare's law of speed"
      Locate 12,2: Print "addition:"
      Locate 13,2: Print "                                                          "'erasing previous text.
      Locate 14,2: Print "   beta'' = (beta + beta') / (1 + beta * beta')"
      Locate 16,2: Print "A-A' are accelerated"      
      Locate 17,2: Print "from 0 to .5 c."
      Locate 18,2: Print "They contract from 1"
      Locate 19,2: Print "to .866 light-second."
      Locate 21,2: Print "B-B' are stopped"      
      Locate 22,2: Print "(from -.5 c to 0 c)."
      Locate 23,2: Print "The .866 contraction"
      Locate 24,2: Print "and the 5 sec. time"
      Locate 25,2: Print "shift are cancelled."
      Locate 27,2: Print "C-C' are accelerated"      
      Locate 28,2: Print "from .5 c to .8 c."
      Locate 29,2: Print "A severe contraction"
      Locate 30,2: Print "to .6 l-s occurs."
      Locate 32,2: Print "The reversed Lorentz transformations are given by:"
      Locate 33,2: Print "                                                  "'erasing.
      Locate 34,2: Print "  x'= g * x + beta * t     y'= y     beta = v / c"
      Locate 35,2: Print "  t'= g * t - beta * x     z'= z     g = Sqr(1 - beta ^ 2)"
      Locate 36,2: Print "  x = g * x'- beta * t'    y = y'"
      Locate 37,2: Print "  t = g * t'+ beta * x'    z = z'"
      Locate 39,2: Print "Instead of transforming space and time, those equations"
      Locate 40,2: Print "transform matter and hours. Then it becomes possible to"
      Locate 41,2: Print "include more than two material bodies inside the same"
      Locate 42,2: Print "Cartesian frame of reference and to transform them thanks"
      Locate 43,2: Print "to the Time Scanner. The important point is that the red"
      Locate 44,2: Print "scale shown above does not transform, so that the acceler-"
      Locate 45,2: Print "ated system still uses the same frame of reference."

      Locate 44,104: Print "Gabriel LaFreniere  glafreniere.com";
      Locate 45,104: Print "Thanks to the creators of FreeBASIC."

      Locate 47, 3: Print "Hendrick A. Lorentz stated In 1904 that the Michelson"
      Locate 48, 3: Print "interferometer should undergo a contraction according"
      Locate 49, 3: Print "to a factor which is given by: Sqr(1 - beta ^ 2). He"
      Locate 50, 3: Print "also discovered that no contraction should occur on"
      Locate 51, 3: Print "transverse axes and that clocks should run slower. A"
      Locate 52, 3: Print "local time (time shift) should finally appear. "
      Locate 54, 3: Print "This program is a clear demonstration that Lorentz"
      Locate 55, 3: Print "was right. Introducing a space/time transformation"
      Locate 56, 3: Print "was an enormous error. It was totally useless, given"
      Locate 57, 3: Print "the fact that under those conditions, the observers"
      Locate 58, 3: Print "A, B and C shown above cannot detect which one is at"
      Locate 59, 3: Print "rest. In addition, Lorentz's version of Relativity"
      Locate 60, 3: Print "is capable of conciliating more than two results"
      Locate 61, 3: Print "without having to deal with so-called paradoxes"
      Locate 62, 3: Print "which are actually contradictions. Indisputably,"
      Locate 63, 3: Print "it is the only correct one - and it is amazing!"
      Screenset work_page, visible_page
    End If
  End If
End Sub


'***********************************************************************************************************************
' SUB ENLARGE  -  ENLARGING CHARACTERS.
'***********************************************************************************************************************
Sub Enlarge()

  center = 8
  y_title = 16
  Color black, background
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
  Line(0, 0)-(15, 47), background, bf

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
  Line(0, 0)-(15, 47), background, bf

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
  Line(0, 0)-(15, 47), background, bf

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
  Line(0, 0)-(15, 47), background, bf

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
  Windowtitle "WaveMechanics07 - The Time Scanner and the reversed Lorentz Transformations."
  beta = .5
  theta = Asin(beta)                                                  'transverse wave angle.
  g_Lorentz = Cos(theta)                                              'Lorentz contraction factor.

' Poincaré's law of speed addition:                                    beta" = (beta + beta') / (1 + beta * beta')
' beta and beta' being the same, the formula simplifies to:            beta" = 2 * beta / (1 + beta ^ 2)
'                                                                      half_beta = beta / 2 = .25 would be incorrect.
  half_beta = .267949192
  lambda = 28                                                         'wavelength.
  radius = 4 * lambda
  material_body =   ImageCreate(2 * radius + 1, 2 * radius + 1)       'uncontracted material body (template).
  contracted_body = ImageCreate(2 * radius + 1, 2 * radius + 1)
  image = 0
  images = lambda / beta                                              'number of images per period.
  images = 50
  c = lambda / images                                                 'speed of light in pixels.
  display_width = 480
  display_height = 240
  radius_Lorentz = g_Lorentz * radius
  x1_emitter = 1280 - 200
  zero_point = 460
  red_frame = zero_point
  green_frame = zero_point
  y1_center = 720 / 6                                                 'graphics for AVI video 720 x 1280 pixels.
  y2_center = 720 / 2
  y3_center = 5 * 720 / 6
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
  Locate 13,10 : Print"This is the absolute time"
  Locate 14,11 : Print"before being scanned."
  Locate 17, 3 : Print"The x units are wavelengths"
  Locate 18, 3 : Print"(1 Hz), or light-seconds."
  Locate 19, 3 : Print"The t units are wave"
  Locate 20, 3 : Print"periods (2 pi), or seconds."
  Locate 26, 3 : Print"After being scanned, B-B' will"
  Locate 27, 3 : Print"display the absolute time. The"
  Locate 28, 3 : Print".866 contraction along with the"
  Locate 29, 3 : Print"time shift will be cancelled."
  Locate 32, 3 : Print"Transverse distances"
  Locate 33, 3 : Print"or wavelength never"
  Locate 34, 3 : Print"change: y'= y; z'= z."
  Locate 40, 3 : Print"C-C' will move faster (.5 to"
  Locate 41, 3 : Print".8 c.) after being scanned."
  Locate 42, 3 : Print"Additionally, their time shift"
  Locate 43, 3 : Print"will increase from 5 to 8 sec."
  Locate 44, 3 : Print"and they will contract to .6."
  Locate 47,107:Print "B and C normalized speed : beta = v / c  = 0.5 c."
  Locate 48,107:Print "B and C contraction :  g = Sqr(1-beta^2) ="; g_Lorentz
  Locate 49,107:Print "Scanning speed:     1 / beta =    1 / .5 = 2 c."
  Locate 50,107:Print "Printing speed:     g / beta = .866 / .5 = 1.732 c."
  Locate 52,107:Print "Faster speed for scanned and even more contracted"
  Locate 53,107:Print "C and C':   (beta+beta) / (1+beta*beta)  ="; (beta + beta) / (1 + beta * beta); " c."
  Locate 54,107:Print "Contraction for transformed C and C':  g ="; Sqr(1- ((beta + beta) / (1 + beta * beta))^2)
  Locate 56,107:Print "Intermediate speed for red scale:  beta' = 0.268 c."  
  Locate 57,107:Print " (beta'+beta') / (1+beta'*beta') = beta  = 0.5 c."  
  Locate 59,107:Print "NO CONTRACTION occurs for the slow moving red scale."
  Locate 60,107:Print "It becomes an absolute Cartesian frame of reference."
  Locate 61,59: Print "Please click slowly!      Pause: Press P."
  Title()
  Enlarge()
  
  Line(0, y4_center - radius)-(2 * radius, y4_center + radius), black, bf
  Circle(radius, y4_center), radius - 5, background,,, 1
  Paint(radius, y4_center), background, background

  x_center = 3 * radius                                                   'the contraction is relative to the center.
  Line(x_center - g_Lorentz * radius, y4_center - radius)-(x_center + g_Lorentz * radius, y4_center + radius), black, bf
  Circle(x_center, y4_center), radius - 5, background,,, 1 / g_Lorentz
  Paint(x_center, y4_center), background, background
  t_prime_BC = ImageCreate(22, 130)
  Line(x_center - 2, y4_center - radius)-(x_center + 2, y4_center + radius), black, bf
  Line(0,   720)-(1280, 720), black

  Locate 44,95: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 45,95: ? "Thanks to the creators of FreeBASIC."
  Color dark_gray
  Locate 63,121:? "August 22, 2009. This program may be"
  Locate 64,121:? "freely distributed, copied or modified.";
  Color black
End Sub


'***********************************************************************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'***********************************************************************************************************************

Sub Title()
  display_title = "The Time Scanner"
  Locate 1,1: ? display_title
  center = 640
  y_title = 730
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
