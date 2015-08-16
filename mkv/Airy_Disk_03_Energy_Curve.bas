' THE AIRY DISK ACCORDING TO HUYGENS' PRINCIPLE
Width 80,18:Color 0,15:Cls
?:? " Created by Gabriel LaFreniere."
?:? " This is a FreeBasic program."
?:? " The FreeBasic compiler ver. 0.20.0b (2008) for Windows is available here:"
?   " http://www.freebasic.net/index.php/download"
?:? " This program is still compatible with previous compilers."
?:? " In order to edit this program, you may download the IDE from:"
?   " http://fbide.freebasic.net"
?   "    ...or download the FbEdit software from:"
?   " http://radasm.110mb.com/fbedit/"
?   " http://sourceforge.net/projects/fbedit/"

Declare Sub Axial_Graphic():           Const pi = 4 * Atn(1)
Declare Sub Bitmaps():                 Const red =        RGB(255, 0, 0)
Declare Sub Color_Display():           Const cyan =       RGB(0,200,200)
Declare Sub Concave_Source():          Const gray =       RGB(128,128,128)
Declare Sub Cursor_Angle():            Const blue =       RGB(0, 0, 255)
Declare Sub Cursor_Brightness():       Const green =      RGB(0, 255, 0)
Declare Sub Cursor_Lambda():           Const black =      RGB(0,  0,  0)
Declare Sub Display():                 Const white =      RGB(255,255,255)
Declare Sub Energy_Display():          Const yellow =     RGB(255,255,0)
Declare Sub Black_and_White_Display(): Const background = RGB(225,225,225)
Declare Sub Hemispheric_Source()
Declare Sub Initialization()
Declare Sub Spherical_Source()
Declare Sub Update()

Dim Shared As Integer work_page = 1, visible_page, x, y, x_1, x_2, y_1, y_2, y_curve_1, y_curve_2
Dim Shared As Integer bitmap, y_height = 502, x_width = 776, x_center, y_center
Dim Shared As Integer image, images, x_focal_plane, calculus_done, x_mouse, y_mouse, mouse_line, click
Dim Shared As Integer y_previous, y_previous_phase_1, y_previous_phase_2, y_previous_1, y_previous_2
Dim Shared As Integer x_plane, x_coord, y_coord, y_phase_1, y_phase_2, y_wave_1, y_wave_2
Dim Shared As Integer capture=1, circle_number, cursor, minimum, maximum, previous_angle
Dim Shared As Integer previous_lambda

Dim Shared As Single axial_period(x_width), axial_amplitude(x_width), horizontal_zoom, normalize
Dim Shared As Single period(x_width, -y_height To y_height), gray_shade(x_width, -y_height To y_height)
Dim Shared As Single c_color(x_width, -y_height To y_height), luminance(x_width, -y_height To y_height)
Dim Shared As Single x_squared, y_squared, phase_1, phase_2, distance, x_Distance, y_Distance, y_Dist
Dim Shared As Single sine_amplitude, cosine_amplitude, path_difference, dimming, energy, previous_energy
Dim Shared As Single rotation, amplitude, phase, shade, angle_step, radius, arc_radius, lambda
Dim Shared As Single angle, aperture_angle, source_angle, pixel_angle, scan_angle, arc_angle, relative_aperture
Dim Shared As Single wavelet, factor, ratio, brightness, r_red, g_green, b_blue, small_radius, large_radius
Dim Shared As Single xA, yA, xB, yB, xC, yC, xD, yD, xE, yE, xF, yF, BCA, arc, start_angle, fine_angle
Dim Shared As Single AB, AC, AE, AF, BC, BD, BE, BG, CD, CF, CJ, CK, DP, GJ, GL, JK

Dim Shared As String number, deleting, file_name, in_key, mode
Dim Shared As String mouse_line_59, mouse_line_60, mouse_line_61, mouse_line_62, mouse_line_63
Screen 21,24,3: Initialization()

Do
	GetMouse x_mouse, y_mouse, , click
	Update()
	Swap work_page, visible_page
	ScreenSet work_page, visible_page
	PCopy 2, work_page

	Sleep 100
	Concave_Source()
	If bitmap Then Bitmaps()                                                      'set bitmap = 1 for screen capture.
	If aperture_angle = 180 Then Sleep: Initialization()
	aperture_angle += .25
   source_angle = aperture_angle / (180 / pi)
	lambda = x_center / 3 / (2 / Sin(source_angle)^2 - 1)
	If aperture_angle >= 90 Then lambda = 130
	If lambda > 200 Then lambda = 200 Else If lambda < 20 Then lambda = 20
	'Display()'
	in_key = Inkey'-------------------------------------------------------------- KEYBOARD MANAGEMENT
	If Len(in_key) Then
		If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = UCase(in_key)
		Select Case in_key
			Case "B": If mode <> "gray" Then mode = "gray": Update()
			Case "C": If mode <> "color"     Then mode = "color":     Update()
			Case "E": If mode <> "energy"    Then mode = "energy":    Update()
			Case "I": Initialization()
			Case "P"
				Color red: ScreenSet visible_page
				Locate 64, 39: Print " Paused. Press Esc. to Quit. Press any other key to resume.";
				Sleep: If Inkey = Chr(27) Then End Else in_key = ""
				Color black: Locate 64, 39: Print " P - Pause.";
			Case "k+", "X+", Chr(27): End
			Case "M+": If x_focal_plane < x_width Then x_focal_plane += 1
			Case "K+": If x_focal_plane > 0 Then x_focal_plane -= 1
			Case "=": brightness = 1: Update()
			Case "Z": calculus_done = 1: Update()
			Case Else: in_key = ""
		End Select
		Do: Loop While Len(Inkey) 
	End If
	If click = 1 Then Else GetMouse x_mouse, y_mouse, , click
	mouse_line = .5 + y_mouse / 16
	Locate 64, 2: Print x_mouse; y_mouse; click; mouse_line; "    ";
	Print Using "###.####"; Abs(x_mouse - x_center) / lambda;
	If mouse_line > 23 And mouse_line < 64 Then
		If x_mouse < 304 Or x_mouse > 496 Then mouse_line = 0
	Else mouse_line = 0
	End If
	Color blue, white'----------------------------------------------------------- MOUSE POSITION.
	Locate mouse_line, 39
	Select Case mouse_line
		Case 52: Print deleting: Locate 52, 39
			Print " Aperture Angle... ";
			Select Case aperture_angle
				Case Is < 10:   Print Using "#.##"; aperture_angle;: Print Chr(248); "  "
				Case Is < 100:  Print Using "##.##"; aperture_angle;: Print Chr(248); " "
				Case Is < 1000: Print Using "###.##"; aperture_angle;: Print Chr(248)
			End Select
		Case 53: Print deleting: Locate 53, 39
			Print " Wavelength....... ";
			Select Case lambda
				Case Is < 9.995: Print Using "#.##"; lambda
				Case Is < 99.95: Print Using "##.#"; lambda
				Case Else:       Print Using "### ";  lambda
			End Select
		Case 54: Print deleting: Locate 54, 39
			Print " Brightness.......";: Print Using " #.#"; brightness
		Case 59: If mode = "gray" Then Else Print mouse_line_59
		Case 60: If mode = "color"     Then Else Print mouse_line_60
		Case 61: If mode = "energy"    Then Else Print mouse_line_61
		Case 62: Print mouse_line_62
		Case 63: Print mouse_line_63
		Case 37: Print deleting;
	End Select
	Color black, background

	If click = 1 Then	'---------------------------------------------------------- MOUSE MANAGEMENT.
		If x_mouse > 0 And x_mouse < x_width And y_mouse < 720 Then x_focal_plane = x_mouse
		If x_mouse > x_width And y_mouse < 720 Then x_focal_plane = x_width
		bitmap = 0': image = 0
		Select Case mouse_line
			Case 52: Cursor_Angle()
			Case 53: Cursor_Lambda()
			Case 54: Cursor_Brightness()
			Case 59: If mode <> "gray" Then mode = "gray": Update()
			Case 60: If mode <> "color"     Then mode = "color":     Update()
			Case 61: If mode <> "energy"    Then mode = "energy":    Update()
			Case 62: Initialization()
			Case 63: End
		End Select
	End If
	If click = 2 And x_mouse > 0 And x_mouse < 1280 And y_mouse < 720 Then x_focal_plane = x_center
	Sleep 1
Loop


Sub Axial_Graphic()'********************************************** AXIAL GRAPHIC
	amplitude = .114 * luminance(0, 0)
	y_previous_phase_1 = y_curve_2 + amplitude * Sin(axial_period(0) - phase)
	y_previous_phase_2 = y_curve_2 + amplitude * Sin(axial_period(0) + phase)
	y_previous = amplitude
	y_previous_1 = amplitude * Sin(period(0, 0) - phase)
	y_previous_2 = amplitude * Sin(period(0, 0) + phase)
	For x = 0 To x_width / 2 - 1
		sine_amplitude = Sin(axial_period(x) + phase - pi / 2)
		cosine_amplitude = Cos(axial_period(x) - phase)
		y_phase_1 = y_curve_2 + amplitude * Sin(axial_period(x) - phase)
		y_phase_2 = y_curve_2 + amplitude * Sin(axial_period(x) + phase)
		If cosine_amplitude > 0 Then Line(x_center - x, y_previous_phase_1)-(x_center - x, y_phase_1), cyan
		If sine_amplitude > 0 Then Line(x_center + x, y_previous_phase_2)-(x_center + x, y_phase_2), cyan
		y_coord = .114 * luminance(x, 0)
		Line(x_center + x, y_curve_2 + y_coord)-(x_center + x, y_curve_2 - y_coord), white
		Line(x_center - x, y_curve_2 + y_coord)-(x_center - x, y_curve_2 - y_coord), white
		'PSet(x_center + x, y_curve_2 + y_coord + 1), gray
		'PSet(x_center + x, y_curve_2 - y_coord - 1), black
		'PSet(x_center - x, y_curve_2 + y_coord + 1), black
		'PSet(x_center - x, y_curve_2 - y_coord - 1), black
		Line(x_center + x, y_curve_2 + y_previous)-(x_center + x, y_curve_2 + y_coord), gray
		Line(x_center + x, y_curve_2 - y_previous)-(x_center + x, y_curve_2 - y_coord), gray
		Line(x_center - x, y_curve_2 + y_previous)-(x_center - x, y_curve_2 + y_coord), gray
		Line(x_center - x, y_curve_2 - y_previous)-(x_center - x, y_curve_2 - y_coord), gray
		y_wave_1 = .114 * luminance(x, 0) * Sin(period(x, 0) - phase)
		y_wave_2 = .114 * luminance(x, 0) * Sin(period(x, 0) + phase)  'symétrie.
		Line(x_center + x, y_curve_2 - y_previous_1)-(x_center + x, y_curve_2 - y_wave_1),black
		Line(x_center - x, y_curve_2 - y_previous_2)-(x_center - x, y_curve_2 - y_wave_2),black
		If cosine_amplitude < 0 Then Line(x_center - x, y_previous_phase_1)-(x_center - x, y_phase_1), cyan
		If sine_amplitude < 0 Then Line(x_center + x, y_previous_phase_2)-(x_center + x, y_phase_2), cyan
		y_previous_phase_1 = y_phase_1
		y_previous_phase_2 = y_phase_2
		y_previous = y_coord
		y_previous_1 = y_wave_1
		y_previous_2 = y_wave_2
	Next
	Line(0, y_curve_2)-(x_width, y_curve_2), black
	Line(x_focal_plane, 0)-(x_focal_plane, y_height), gray                        '2-D focal plane graphic.
	x_coord = Abs(x_center - x_focal_plane)                                       'focal plane amplitude curve.
   y_previous = .114 * luminance(x_coord, 0)
	For y = 0 To y_center
		y_coord = .114 * luminance(x_coord, y)
		Line(1279 - y_center + y, y_curve_2 - y_coord)-(1279 - y_center + y, y_curve_2 + y_coord), white
		Line(1279 - y_center - y, y_curve_2 - y_coord)-(1279 - y_center - y, y_curve_2 + y_coord), white
		Line(1279 - y_center + y, y_curve_2 - y_previous)-(1279 - y_center + y, y_curve_2 - y_coord), gray
		Line(1279 - y_center - y, y_curve_2 - y_previous)-(1279 - y_center - y, y_curve_2 - y_coord), gray
		Line(1279 - y_center + y, y_curve_2 + y_previous)-(1279 - y_center + y, y_curve_2 + y_coord), gray
		Line(1279 - y_center - y, y_curve_2 + y_previous)-(1279 - y_center - y, y_curve_2 + y_coord), gray
		y_previous = y_coord
	Next
	x_coord = Abs(x_center - x_focal_plane)                                       '2-D focal plane curve.
	If x_focal_plane < x_center Then phase_2 = -phase Else phase_2 = phase
	y_previous = y_curve_2 - .114 * luminance(x_coord, 0) * Sin(period(x_coord, 0) - phase_2)
	For y = 0 To y_center
		phase_1 = Sin(period(x_coord, y) - phase_2)
		y_coord = y_curve_2 - .114 * luminance(x_coord, y) * phase_1
		Line(1279 - y_center + y, y_previous)-(1279 - y_center + y, y_coord), black
		Line(1279 - y_center - y, y_previous)-(1279 - y_center - y, y_coord), black
		y_previous = y_coord
	Next
	Line(1279 - y_height, y_curve_2)-(1279, y_curve_2), black
End Sub


Sub Bitmaps()'*************************************** CREATING A BITMAP SEQUENCE
	Select Case capture
		Case Is < 10: number = "00"
		Case Is < 100: number = "0"
		Case Is < 1000: number = ""
	End Select
	file_name = "capture_" + number + Str(capture) + ".bmp"
   ScreenSet work_page, work_page: Color red, background
   Locate 56, 70: Print file_name
   Locate 58, 70: Print "Warning! A bitmap sequence is being created in the current directory."
	Color black, background
   BSave file_name,0
	capture += 1
	If capture > 1000 Then End
End Sub


Sub Black_and_White_Display()'************************** BLACK AND WHITE DISPLAY
	For x = 0 To x_width / 2
		For y = 0 To y_center
			shade = brightness * luminance(x, y) * Sin(period(x, y) + phase) + 100  'centering medium gray at 100.
			If shade > 255 Then shade = 255 Else If shade < 0 Then shade = 0
			PSet(x_center - x, y_center - y), RGB(shade, shade, shade)
			PSet(x_center - x, y_center + y), RGB(shade, shade, shade)
			shade = brightness * luminance(x, y) * Sin(period(x, y) - phase) + 100
			If shade > 255 Then shade = 255 Else If shade < 0 Then shade = 0
			PSet(x_center + x, y_center - y), RGB(shade, shade, shade)
			PSet(x_center + x, y_center + y), RGB(shade, shade, shade)
		Next
	Next
	'Line(x_focal_plane, 0)-(x_focal_plane, y_height), white
	Line(x_center,0)-(x_center,10), white                                         'landmarks.
	Line(x_center - lambda / 2,0)-(x_center - lambda / 2,5), white
	Line(x_center + lambda / 2,0)-(x_center + lambda / 2,5), white
	For x = 0 To y_center                                                         'regular Airy disk in color.
		x_squared = x ^ 2
	   x_plane = Abs(x_focal_plane - x_center)
		For y = 0 To y_center
			y_Dist = Sqr(y ^ 2 + x_squared)
			If y_Dist <= y_center Then
				If x_focal_plane > x_center Then
					shade = brightness * luminance(x_plane, y_Dist) * Sin(period(x_plane, y_Dist) - phase) + 100
				Else
					shade = brightness * luminance(x_plane, y_Dist) * Sin(period(x_plane, y_Dist) + phase) + 100
				End If
				If shade > 255 Then shade = 255 Else If shade < 0 Then shade = 0
				PSet(1279 - y_center + x, y_center - y), RGB(shade, shade, shade)
				PSet(1279 - y_center - x, y_center - y), RGB(shade, shade, shade)
				PSet(1279 - y_center + x, y_center + y), RGB(shade, shade, shade)
				PSet(1279 - y_center - x, y_center + y), RGB(shade, shade, shade)
			End If
		Next
	Next
	Axial_Graphic()
End Sub


Sub Color_Display()'********************************************** COLOR DISPLAY
	For x = 0 To x_width / 2
		For y = 0 To y_center
			If Sin(period(x, y) + phase) > 0 Then
				r_red = 0
				g_green = brightness * 255 * Sin(period(x, y) + phase) * .01 * luminance(x, y)
				b_blue = g_green / 2
				If g_green > 255 Then r_red = g_green - 255: g_green = 255
				If r_red > 255 Then r_red = 255
				If b_blue > 255 Then b_blue = 255
			Else
				r_red = brightness * 255 * -Sin(period(x, y) + phase) * .01 * luminance(x, y)
				g_green = 0
				b_blue = r_red / 2
				If r_red > 255 Then g_green = r_red - 255: r_red = 255
				If g_green > 255 Then g_green = 255
				If b_blue > 255 Then b_blue = 255
			End If
			PSet(x_center - x, y_center - y), RGB(r_red, g_green, b_blue)
			PSet(x_center - x, y_center + y), RGB(r_red, g_green, b_blue)
			If Sin(period(x, y) - phase) > 0 Then
				r_red = 0
				g_green = brightness * 255 * Sin(period(x, y) - phase) * .01 * luminance(x, y)
				b_blue = g_green / 2
				If g_green > 255 Then r_red = g_green - 255: g_green = 255
				If r_red > 255 Then r_red = 255
				If b_blue > 255 Then b_blue = 255
			Else
				r_red = brightness * 255 * -Sin(period(x, y) - phase) * .01 * luminance(x, y)
				g_green = 0
				b_blue = r_red / 2
				If r_red > 255 Then g_green = r_red - 255: r_red = 255
				If g_green > 255 Then g_green = 255
				If b_blue > 255 Then b_blue = 255
			End If
			PSet(x_center + x, y_center - y), RGB(r_red, g_green, b_blue)
			PSet(x_center + x, y_center + y), RGB(r_red, g_green, b_blue)
		Next
	Next

	For x = 0 To y_center                                                         'regular Airy disk in color.
		x_squared = x ^ 2
	   x_plane = Abs(x_focal_plane - x_center)
		For y = 0 To y_center
			y_Dist = Sqr(y ^ 2 + x_squared)
			If y_Dist <= y_center Then
				If x_focal_plane > x_center Then
					If Sin(period(x_plane, y_Dist) - phase) > 0 Then
						r_red = 0
						g_green = brightness * 255 * Sin(period(x_plane, y_Dist) - phase) * .01 * luminance(x_plane, y_Dist)
						b_blue = g_green / 2
						If g_green > 255 Then r_red = g_green - 255: g_green = 255
						If r_red > 255 Then r_red = 255
						If b_blue > 255 Then b_blue = 255
					Else
						r_red = brightness * 255 * -Sin(period(x_plane, y_Dist) - phase) * .01 * luminance(x_plane, y_Dist)
						g_green = 0
						b_blue = r_red / 2
						If r_red > 255 Then g_green = r_red - 255: r_red = 255
						If g_green > 255 Then g_green = 255
						If b_blue > 255 Then b_blue = 255
					End If		
				Else
					If Sin(period(x_plane, y_Dist) + phase) > 0 Then
						r_red = 0
						g_green = brightness * 255 * Sin(period(x_plane, y_Dist) + phase) * .01 * luminance(x_plane, y_Dist)
						b_blue = g_green / 2
						If g_green > 255 Then r_red = g_green - 255: g_green = 255
						If r_red > 255 Then r_red = 255
						If b_blue > 255 Then b_blue = 255
					Else
						r_red = brightness * 255 * -Sin(period(x_plane, y_Dist) + phase) * .01 * luminance(x_plane, y_Dist)
						g_green = 0
						b_blue = r_red / 2
						If r_red > 255 Then g_green = r_red - 255: r_red = 255
						If g_green > 255 Then g_green = 255
						If b_blue > 255 Then b_blue = 255
					End If
				End If
				PSet(1279 - y_center + x, y_center - y), RGB(r_red, g_green, b_blue)
				PSet(1279 - y_center - x, y_center - y), RGB(r_red, g_green, b_blue)
				PSet(1279 - y_center + x, y_center + y), RGB(r_red, g_green, b_blue)
				PSet(1279 - y_center - x, y_center + y), RGB(r_red, g_green, b_blue)
			End If
		Next
	Next
	Axial_Graphic()
End Sub


Sub Concave_Source()'******************** STANDARD CALCULUS FOR A CONCAVE SOURCE
	'the two simplified algorithms below were useful in order to check the accuracy of the standard one, but they are not used in this program:
	'if aperture_angle = 180 then Spherical_Source(): Exit Sub
	'if aperture_angle = 90 then Hemispheric_Source(): Exit Sub

	Line(0, 0)-(799, y_height), background, bf                                    'erasing text.
	Line(0, 0)-(1279, y_height), RGB(150,150,150), bf                             'darker area.
	Line(640 - 116, 10)-(640 + 116, 36), background, bf
	Line(640 - 116, 10)-(640 + 116, 36), black, b
	Locate 2, 68: Print "The Airy Disk Energy Curve"
	angle_step = source_angle / circle_number                                     'space between arcs of a circle.
	JK = Sin(source_angle)                                                        'first triangle.
	CJ = Cos(source_angle)                                                        'this algorithm was explained in Ether16_hemisphere.bas

'	----------------------------------------------------------------------------- SIMPLIFIED CALCULUS FOR THE OPTICAL AXIS ONLY
	For x = 0 To x_width / 2                                                      'x = 0  at the center of a sphere.
	   distance = x / lambda
	   path_difference = distance - Int(distance)                                 'maximum one wavelength.
	   phase_1 = 2 * pi * -path_difference                                        'on-axis period of reference.
	   sine_amplitude = 0: cosine_amplitude = 0
	   For angle = angle_step / 2 To source_angle Step angle_step
	      path_difference = distance * Cos(angle)
	      phase = 2 * pi * path_difference
	      sine_amplitude = sine_amplitude + Sin(angle) * Sin(phase)               'correcting sin(angle) using pi below, or: pi * sin(angle)).
	      cosine_amplitude = cosine_amplitude + Sin(angle) * Cos(phase)
	   Next
	   period(x, 0) = -Atn(cosine_amplitude / sine_amplitude) + pi                'absolute period.
	   If sine_amplitude < 0 Then period(x, 0) = period(x, 0) + pi                'validating all 4 quadrants.
	   If x = 0 Then normalize = 600 / Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)
	   luminance(x, 0) = normalize * Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)  'Pythagoras.
	   axial_period(x) = phase_1 - period(x, 0)                                   'period fluctuations.
	   axial_amplitude(x) = .195 * luminance(x, 0)
      If x = 0 Then y_previous = axial_amplitude(x)
	   Line(x_center - x, y_curve_1 - axial_amplitude(x))-(x_center - x, y_curve_1), white'on-axis energy curve.
	   Line(x_center + x, y_curve_1 - axial_amplitude(x))-(x_center + x, y_curve_1), white
	   Line(x_center + x, y_curve_1 - y_previous)-(x_center + x, y_curve_1 - axial_amplitude(x)), black
	   Line(x_center - x, y_curve_1 - y_previous)-(x_center - x, y_curve_1 - axial_amplitude(x)), black
		y_previous = axial_amplitude(x)
	Next

	'---------------------------------------------------------------------------- SIMPLIFIED CALCULUS FOR THE FOCAL PLANE ONLY
	If aperture_angle > 90 Then
		start_angle = angle_step / 2: fine_angle = pi
	Else
		start_angle = pi / 2 - source_angle + angle_step / 2: fine_angle = pi / 2 + source_angle
	End If
	previous_energy = 0
	For y = 0 To y_center
		distance = y / lambda
		sine_amplitude = 0: cosine_amplitude = 0
		For angle = start_angle To fine_angle Step angle_step
			AF = Sin(angle)                                                         'arc radius.
			AC = Cos(angle)                                                         'AC = BC = GJ
			GL = Sqr(JK ^ 2 - AC ^ 2)                                               'AC = GJ
			arc_angle = Atn(GL / CJ)                                                'CJ = BG; AB = 0
			If CJ < 0 Then arc_angle = arc_angle + pi
			If GL < 0 Then arc_angle = pi
			arc = AF * arc_angle
			path_difference = distance * Cos(angle)
			phase = 2 * pi * path_difference
			sine_amplitude =   sine_amplitude   + arc * Sin(phase)
			cosine_amplitude = cosine_amplitude + arc * Cos(phase)
		Next
		period(0, y) = Atn(cosine_amplitude / sine_amplitude)
		If sine_amplitude < 0 Then period(0, y) = period(0, y) + pi
		If y = 0 Then normalize = 600 / Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)
		luminance(0, y) = normalize * Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)'Pythagoras.
		Energy = .0035 * luminance(0,y) ^ 2
		If y = 0 Then previous_energy = Energy
		Line(8, y_center + y)-(8 + energy, y_center + y), white                    'regular Airy disk energy curve.
		Line(8, y_center - y)-(8 + energy, y_center - y), white
		Line(8 + previous_energy, y_center + y)-(8 + energy, y_center + y), black
		Line(8 + previous_energy, y_center - y)-(8 + energy, y_center - y), black
		previous_energy = energy
		shade = brightness * luminance(0, y)
		If shade > 255 Then shade = 255
		gray_shade(0, y) = RGB(shade, shade, shade)
		y_coord = y_curve_1 - normalize * .195 * cosine_amplitude
		If y = 0 Then
			y_previous = y_coord
			axial_period(0) = period(0, y) + pi
		End If
		Line(1279 - y_center + y, y_coord)-(1279 - y_center + y, y_curve_1), white 'focal plane amplitude curve.
		Line(1279 - y_center - y, y_coord)-(1279 - y_center - y, y_curve_1), white
		Line(1279 - y_center + y, y_previous)-(1279 - y_center + y, y_coord), black
		Line(1279 - y_center - y, y_previous)-(1279 - y_center - y, y_coord), black
		y_previous = y_coord
		'PSet(x_center, y_center - y), RGB(shade, shade, shade)                    'focal plane only.
		'PSet(x_center, y_center + y), RGB(shade, shade, shade)
	Next
	Line(7,0)-(7, y_height), black                                                'base for the focal plane energy curve.
	Line(0, y_curve_1)-(1279, y_curve_1), black                                   'base for amplitude curves.
	Line(x_width, y_height)-(x_width, 719), black                                 'separator.
	Line(0, y_height)-(1279, y_height), black
	If lambda >= 4 Then
		For x = 0 To x_center Step lambda                                           'ellipsoid wavelength scale.
			Line(x_center + x, y_curve_1)-(x_center + x, y_curve_1 + 10), black
			Line(x_center - x, y_curve_1)-(x_center - x, y_curve_1 + 10), black
		Next
		For x = 0 To x_center - lambda / 2 Step lambda
			Line(x_center + x + lambda / 2, y_curve_1)-(x_center + x + lambda / 2, y_curve_1 + 5), black
			Line(x_center - x - lambda / 2, y_curve_1)-(x_center - x - lambda / 2, y_curve_1 + 5), black
		Next
		For y = 0 To y_center Step lambda                                           'Airy disk wavelength scale.
			Line(1279 - y_center + y, y_curve_1)-(1279 - y_center + y, y_curve_1 + 10), black
			Line(1279 - y_center - y, y_curve_1)-(1279 - y_center - y, y_curve_1 + 10), black
			Line(8, y_center - y)-(18, y_center - y), black
			Line(8, y_center + y)-(18, y_center + y), black
		Next
		For y = 0 To y_center - lambda / 2 Step lambda
			Line(1279 - y_center + y + lambda / 2, y_curve_1)-(1279 - y_center + y + lambda / 2, y_curve_1 + 5), black
			Line(1279 - y_center - y - lambda / 2, y_curve_1)-(1279 - y_center - y - lambda / 2, y_curve_1 + 5), black
			Line(8, y_center - y - lambda / 2)-(13, y_center - y - lambda / 2), black
			Line(8, y_center + y + lambda / 2)-(13, y_center + y + lambda / 2), black
		Next
	EndIf

Exit Sub'*********************************


	Sleep 1000                                                                    'small pause for temporary Airy disk graphics.
	If Len(Inkey) Then Sleep                                                      'pause in order to observe the energy curves.
	in_key = Inkey
	Locate 51, 10: Print "Please wait..."
	'----------------------------------------------------------------------------- STANDARD CALCULUS

	For x = 1 To x_width / 2                                                      'x = 0 at the center of the sphere.
		x_Distance = x  / lambda                                                   'horizontal distance in wavelength units.
		x_squared = x_Distance ^ 2                                                 'horizontal distance squared.
		path_difference = x_Distance - Int(x_Distance)                             'never exceeds one wavelength.
		phase_1 = 2 * pi * path_difference                                         'on-axis reference period.
		For y = 0 To y_center                                                      'just one half is needed because of the symmetry.
			sine_amplitude = 0: cosine_amplitude = 0                                'initialization required for every pixel.
			y_Distance = y / lambda                                                 'vertical distance in wavelength units.
			distance = Sqr(y_Distance ^ 2 + x_squared)                              'distance to center according to Pythagoras.
			If x Then pixel_angle = Atn(y / x) Else pixel_angle = pi                '0° vertically, the source is on the left.
			angle = source_angle + pixel_angle - pi / 2
			AC = Sin(angle)
			start_angle = Acos(AC) + angle_step / 2
			If y = 0 Or aperture_angle > 90 Then start_angle = angle_step / 2
			For angle = start_angle To pi Step angle_step                           'distributing all arcs equally.
				AF = Sin(angle)                                                      'measuring all Huygens' wavelets path using trigonometry.
				AC = Cos(angle)                                                      'One may consider each wavelet, but this method
				BC = AC / Sin(pixel_angle)                                           'is rather slow. The goal here is to join all wavelets
				AB = BC * Cos(pixel_angle)                                           'whose path is equidistant by an arc of a circle.
				BG = CJ / Sin(pixel_angle)                                           'The arc length is a good indication of the number of
				GJ = BG * Cos(pixel_angle) + BC                                      'wavelets whose path difference is constant.
				If GJ > JK Then GJ = JK                                              'Then the final phase and amplitude is deduced
				If GJ < -JK Then GJ = -JK                                            'from the wavelet path difference summation.
				GL = Sqr(JK ^ 2 - GJ ^ 2)
				arc_angle = Atn(GL / (BG + AB))                                      'the angle allows one to obtain the arc length.
				If BG + AB < 0 Then arc_angle = arc_angle + pi                       'validating the second quadrant.
				arc = AF * arc_angle                                                 'arc length..
				If arc = 0 And y > 0 And aperture_angle < 91 Then Exit For           'limit exceeded: arc = 0 useless.
				If y = 0 Then arc = pi * Sin(angle): If angle > source_angle Then Exit For'the axial calculus is accurate.
				path_difference = distance * Cos(angle)
				phase = 2 * pi * path_difference
				sine_amplitude = sine_amplitude + arc * Sin(phase)                   'amplitude summation according to the arc length.
				cosine_amplitude = cosine_amplitude + arc * Cos(phase)               'the cosine is required to obtain the phase and the energy.
			Next
			luminance(x, y) = normalize * Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)'Pythagoras.
			period(x, y) = Atn(cosine_amplitude / sine_amplitude)                   'absolute wave period for this pixel.
			If sine_amplitude < 0 Then period(x, y) = period(x, y) + pi             'validating all 4 quadrants.
			If y = 0 Then
				period(x, y)= -period(x, y) + pi                                     'the axial calculus (y = 0) reverses the period.
				axial_amplitude(x) = luminance(x, y)
				axial_period(x) = phase_1 - period(x, y)                             'wave period fluctuations.
			End If
			shade = brightness * luminance(x, y)
			If shade > 255 Then shade = 255
			PSet(x_center + x, y_center - y), RGB(shade, shade, shade)              'temporary black and white diagram.
			PSet(x_center + x, y_center + y), RGB(shade, shade, shade)
			PSet(x_center - x, y_center - y), RGB(shade, shade, shade)
			PSet(x_center - x, y_center + y), RGB(shade, shade, shade)
		Next
		in_key = Inkey
		If Len(in_key) Then
			If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = UCase(in_key)
			Select Case in_key
				'Case "B": If mode <> "gray" Then mode = "gray"
				'Case "C": If mode <> "color"     Then mode = "color"
				'Case "E": If mode <> "energy"    Then mode = "energy"
				Case "P": Color red': ScreenSet work_page
					Locate 64, 39: Print " Paused. Press Esc. to Quit. Press any other key to resume.";
					Sleep: If Inkey = Chr(27) Then End
					Color black: Locate 64, 39: Print " P - Pause.                                                ";
				Case "X+", "k+", Chr(27): End
				Case "Z": calculus_done = 1: Exit Sub
			End Select
		End If
		GetMouse x_mouse, y_mouse, , click
		If click = 1 Then Exit Sub
	Next
	calculus_done = 1
End Sub


Sub Cursor_Angle() '******************************* ADJUSTING THE APERTURE ANGLE
	ScreenSet 2
	Color black, background
	previous_angle = aperture_angle
	minimum = 400 - 180
	maximum = 400 + 180
	Do
		Swap work_page, visible_page
		ScreenSet work_page, visible_page
		Cls
		If y_mouse > 786 And y_mouse < 866 Then aperture_angle = (x_mouse - minimum) / 2
		If aperture_angle < 1 Then aperture_angle = 1 Else If aperture_angle > 180 Then aperture_angle = 180
	   If aperture_angle <= 90 Then lambda = x_center / 3 / (2 / Sin(source_angle)^2 - 1) Else lambda = 130
		source_angle = aperture_angle / (180 / pi)
		cursor = minimum + 2 * aperture_angle
		Line(minimum, 815)-(maximum, 831), black, b
		Line(cursor - 1, 815)-(cursor + 1, 831), black, bf
		Locate 49, 33: Print " Aperture angle:"; aperture_angle; Chr(248); " "
		Print " (";
		Print Using "###"; 2 * aperture_angle;: Print Chr(248); " bilateral). "
		If aperture_angle > 89 Then
			relative_aperture = 0
		Else relative_aperture = .5 / Tan(source_angle)
		End If
		Locate 50, 39: Print " Relative aperture: Ÿ /";
		Print Using " #.##"; relative_aperture
		y = 594
		x_1 = 400 - 150 * Cos(source_angle)
		y_1 = y - 150 * Sin(source_angle)
		y_2 = y + 150 * Sin(source_angle)
		Circle(400, y), 150, black,,,1
		Paint(300, y), black
		Circle(400, y), 150, 1,,,1
		Line(x_1, y_1)-(400, y), 1
		Line(x_1, y_2)-(400, y), 1
		Paint(300, y), yellow, 1
		Line(400 - 150, y)-(400, y), 1
		GetMouse x_mouse, y_mouse, , click
	Loop While click = 1
	If previous_angle <> aperture_angle Then
		source_angle = aperture_angle / (180 / pi)
		lambda = x_center / 3 / (2 / Sin(source_angle)^2 - 1)
		If lambda < .2 Then lambda = .2
		If aperture_angle > 89 Then lambda = x_center / 3
		Update(): calculus_done = 0
	EndIf
End Sub


Sub Cursor_Brightness() '********************************** ADJUSTING BRIGHTNESS
	Locate 53, 39: Print deleting
	Color black, white
	If x_mouse > 400 Then
		brightness = 1 + (x_mouse - 400) / 10
	Else brightness = 1 / (1 + (400 - x_mouse) / 10)
	End If
	If brightness < .1 Then brightness = .1 Else If brightness > 9.99 Then brightness = 9.99
	Line(x_mouse - 1, 832)-(x_mouse + 1, 879), black, bf
	Line(400 - 1, 832)-(400 + 1, 879), black, bf
	Locate 54, 39: Print " Brightness....... ";: Print Using "#.# "; brightness
	Line(400 - 96, 847)-(400 + 97, 863), black, b
	ScreenSet 2, 2: Color black, background
	Locate 54, 39: Print " Brightness....... ";: Print Using "#.# "; brightness
	ScreenSet work_page, visible_page
End Sub


Sub Cursor_Lambda() '********************************** SELECTING THE WAVELENGTH
	previous_lambda = lambda                                                      'selecting lambda, minimum 10, maximum 200.
	minimum = 400 - 200 + 20                                                      'center 400, 2 pixels per wavelength.
	maximum = 400 + 200                                                           'center is 100, hence limits are 2 * 100 = 200.
	Do
		Swap work_page, visible_page
		ScreenSet work_page, visible_page
		Cls
		cursor = minimum + 2 * lambda - 20
		Line(minimum - 20, 831)-(maximum + 2, 847), black, b
		Line(cursor - 1, 831)-(cursor + 1, 847), black, bf
		Line(400 - lambda / 2, 806)-(400 + lambda / 2, 823), black, bf
		Locate 47, 18: Print "The Airy ellipsoid structure is only dependent on the aperture angle."
		Locate 48, 20: Print "Hence, modifying the wavelength is equivalent to zooming."
		Locate 50, 33: Print "Wavelength (lambda):"; lambda; " pixels."
		If y_mouse > 802 And y_mouse < 882 Then lambda = (x_mouse - 200) / 2
		If lambda > 200 Then lambda = 200 Else If lambda < 1 Then lambda = 1
		GetMouse x_mouse, y_mouse, , click
	Loop While click = 1
	Update(): calculus_done = 0
End Sub


Sub Display() '***************************************** DISPLAYING THE GRAPHICS
	Swap work_page, visible_page
	ScreenSet work_page, visible_page
	PCopy 2, work_page
	phase = 2 * pi * image / images
	Select Case mode
		Case "color":     Color_Display()
		Case "gray": Black_and_White_Display()
		Case "energy" :   Energy_Display()
	End Select
	distance = Abs((x_focal_plane - x_center)/ lambda)
	If distance Then
		Locate 35, 106: Print "Focal Plane Distance... ";
		Select Case distance
			Case Is < 9.995: Print Using "#.##"; distance;
			Case Is < 99.95: Print Using "##.#"; distance;
			Case Else:       Print Using "### "; distance;
		End Select
		Print " lambda (cross section)."; 		
	EndIf
	Locate 62, 2: Print "Image"; image
	If bitmap Then Bitmaps()                                                      'set bitmap = 1 for screen capture.
	image += 1
	If image = 1000 Then image = 0
End Sub


Sub Energy_Display()'**************************** BLACK AND WHITE ENERGY DISPLAY

	For x = 0 To x_width / 2
		For y = 0 To y_center
			shade = brightness * luminance(x, y)
			If shade > 255 Then shade = 255
			PSet(x_center + x, y_center - y), RGB(shade, shade, shade)
			PSet(x_center - x, y_center - y), RGB(shade, shade, shade)
			PSet(x_center + x, y_center + y), RGB(shade, shade, shade)
			PSet(x_center - x, y_center + y), RGB(shade, shade, shade)
		Next
	Next
	Line(x_focal_plane, 0)-(x_focal_plane, y_height), gray                        'cross-section cursor.
	'If x_focal_plane = x_center Then                                             'Airy ellipsoid contour.
	'	Line(x_center - 10, y_center - small_radius)-(x_center + 10, y_center - small_radius), green
	'	Line(x_center - 10, y_center + small_radius)-(x_center + 10, y_center + small_radius), green
	'	Circle(x_center, y_center), large_radius, green,,, small_radius / large_radius
	'End If

	x_plane = Abs(x_focal_plane - x_center)
	For x = 0 To y_center                                                         'regular Airy disk in gray shades.
		x_squared = x ^ 2
		For y = 0 To y_center
			y_Dist = Sqr(y ^ 2 + x_squared)
			If y_Dist <= y_center Then
				shade = brightness * luminance(x_plane, y_Dist)
				If shade > 255 Then shade = 255
				PSet(1279 - y_center + x, y_center - y), RGB(shade,shade,shade)
				PSet(1279 - y_center - x, y_center - y), RGB(shade,shade,shade)
				PSet(1279 - y_center + x, y_center + y), RGB(shade,shade,shade)
				PSet(1279 - y_center - x, y_center + y), RGB(shade,shade,shade)
			End If
		Next
	Next

   y_previous = y_curve_1 - .195 * luminance(0, 0)                               'optical axis amplitude graphic.
	For x = 0 To x_width / 2 - 1
		y_coord = y_curve_1 - .195 * luminance(x, 0)
		Line(x_center + x, y_coord)-(x_center + x, y_curve_1), white
		Line(x_center - x, y_coord)-(x_center - x, y_curve_1), white
		Line(x_center + x, y_previous)-(x_center + x, y_coord), black
		Line(x_center - x, y_previous)-(x_center - x, y_coord), black
		y_previous = y_coord
	Next
	Line(0, y_curve_1)-(x_width, y_curve_1), black
	Line(x_center, y_curve_1)-(x_center, y_curve_1 + 20), red
	Line(x_center - large_radius, y_curve_1 + 10)-(x_center - large_radius, y_curve_1 + 20), red
	Line(x_center + large_radius, y_curve_1 + 10)-(x_center + large_radius, y_curve_1 + 20), red

	x_coord = Abs(x_center - x_focal_plane)                                       'focal plane amplitude graphic.
   y_previous = y_curve_1 - .195 * luminance(x_coord, 0)
	For y = 0 To y_center
		y_coord = y_curve_1 - .195 * luminance(x_coord, y)
		Line(1279 - y_center + y, y_coord)-(1279 - y_center + y, y_curve_1), white
		Line(1279 - y_center - y, y_coord)-(1279 - y_center - y, y_curve_1), white
		Line(1279 - y_center + y, y_previous)-(1279 - y_center + y, y_coord), black
		Line(1279 - y_center - y, y_previous)-(1279 - y_center - y, y_coord), black
		y_previous = y_coord
	Next
	Line(1279 - y_height, y_curve_1)-(1279, y_curve_1), black
	Line(1279 - y_center, y_curve_1)-(1279 - y_center, y_curve_1 + 20), red
	Line(1279 - y_center - small_radius, y_curve_1 + 10)-(1279 - y_center - small_radius, y_curve_1 + 20), red
	Line(1279 - y_center + small_radius, y_curve_1 + 10)-(1279 - y_center + small_radius, y_curve_1 + 20), red
	If lambda >= 4 Then
		For x = 0 To x_center Step lambda                                           'ellipsoid wavelength scale.
			Line(x_center + x, y_curve_1)-(x_center + x, y_curve_1 + 10), black
			Line(x_center - x, y_curve_1)-(x_center - x, y_curve_1 + 10), black
			Line(x_center + x - lambda / 2, y_curve_1)-(x_center + x - lambda / 2, y_curve_1 + 5), black
			Line(x_center - x + lambda / 2, y_curve_1)-(x_center - x + lambda / 2, y_curve_1 + 5), black
		Next
		For y = 0 To y_center Step lambda                                           'Airy disk wavelength scale.
			Line(1279 - y_center + y, y_curve_1)-(1279 - y_center + y, y_curve_1 + 10), black
			Line(1279 - y_center - y, y_curve_1)-(1279 - y_center - y, y_curve_1 + 10), black
			Line(1279 - y_center + y + lambda / 2, y_curve_1)-(1279 - y_center + y + lambda / 2, y_curve_1 + 5), black
			Line(1279 - y_center - y - lambda / 2, y_curve_1)-(1279 - y_center - y - lambda / 2, y_curve_1 + 5), black
		Next
	EndIf
End Sub


Sub Hemispheric_Source() '********* SIMPLIFIED CALCULUS FOR A HEMISPHERIC SOURCE
	PCopy 2, work_page
	ScreenSet work_page, work_page
	source_angle = aperture_angle / (180 / pi)                                    'see Concave_Source() for some more details.
	angle_step = source_angle / circle_number                                     'this algorithm was explained in Ether16_hemisphere.bas
	For x = 0 To x_width / 2
		x_Distance = x  / lambda
		x_squared = x_Distance ^ 2
		path_difference = x_Distance - Int(x_Distance)
		phase_1 = 2 * pi * path_difference
		For y = 0 To y_center
			sine_amplitude = 0: cosine_amplitude = 0
			y_Distance = y / lambda
			distance = Sqr(y_Distance ^ 2 + x_squared)
			If y Then pixel_angle = Atn(x / y) Else pixel_angle = 0
			pixel_angle = pixel_angle + pi / 2
			For angle = angle_step / 2 To pi Step angle_step
				AC = Cos(angle)
				If y = 0 Then
					If angle > pi / 2 Then BC = -1 Else BC = 1
				Else
					BC = AC / Sin(pixel_angle)
				End If
				If BC > 1 Then BC = 1 Else If BC < -1 Then BC = -1
				AB = BC * Cos(pixel_angle)
				BE = Sqr(1 - BC ^ 2)
				If AB Then arc_angle = Atn(BE / AB) Else arc_angle = pi / 2
				If AB < 0 Then arc_angle = arc_angle + pi
				arc_radius = Sin(angle)
				If arc_radius < 0 Then arc_radius = 0
				arc = arc_radius * arc_angle
				path_difference = distance * Cos(angle)
				phase = 2 * pi * path_difference
				sine_amplitude = sine_amplitude + arc * Sin(phase)
				cosine_amplitude = cosine_amplitude + arc * Cos(phase)
			Next
			period(x, y) = Atn(sine_amplitude / cosine_amplitude)
			If cosine_amplitude < 0 Then period(x, y) = period(x, y) + pi
			'if x = 0 and y = 0 then normalize = Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude) / 30
			luminance(x, y) = normalize * Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)
			shade = luminance(x, y)
			If shade > 255 Then shade = 255
			gray_shade(x, y) = RGB(shade, shade, shade)
			r_red = 100 * (Cos(period(x, y)) + 1)
			g_green = 100 * (Cos(period(x, y) + pi) + 1)
			r_red = r_red + (luminance(x, y) - 155)
			g_green = g_green + (luminance(x, y) - 155)
			If r_red > 255 Then r_red = 255 Else If r_red < 0 Then r_red = 0
			If g_green > 255 Then g_green = 255 Else If g_green < 0 Then g_green = 0
			b_blue = (r_red + g_green) / 2
			If lambda < 6 Then c_color(x, y) = gray_shade(x, y) Else c_color(x, y) = RGB(r_red, g_green, b_blue)
			If mode = "color" Then
				PSet(x_center + x, y_center - y), c_color(x, y)                      'printing temporary color diagram.
				PSet(x_center + x, y_center + y), c_color(x, y)
				PSet(x_center - x, y_center - y), c_color(x, y)
				PSet(x_center - x, y_center + y), c_color(x, y)
			Else
				'Pset(x_center + x, y_center - y), gray_shade(x, y)                  'printing temporary black and white diagram.
				'Pset(x_center + x, y_center + y), gray_shade(x, y)
				'Pset(x_center - x, y_center - y), gray_shade(x, y)
				'Pset(x_center - x, y_center + y), gray_shade(x, y)
			End If
			If y = 0 Then                                                           'optical axis.
				axial_period(x) = phase_1 - period(x, y)                             'memorizing abnormal period fluctuations.
				axial_amplitude(x) = .1 * luminance(x, y)
			End If
		Next
		in_key = Inkey
		If Len(in_key) Then
			If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = UCase(in_key)
			Select Case in_key
				Case "B": If mode <> "gray" Then mode = "gray": Update()
				Case "C": If mode <> "color"     Then mode = "color":     Update()
				Case "E": If mode <> "energy"    Then mode = "energy":    Update()
				Case "I": Initialization(): Exit Sub
				Case "P"
					Color red: ScreenSet visible_page
					Locate 64, 39: Print " Paused. Press Esc. to Quit. Press any other key to resume.";
					Sleep: If Inkey = Chr(27) Then End Else in_key = ""
					Color black: Locate 64, 39: Print " P - Pause.";
				Case "k+", "X+", Chr(27): End
				Case "Z": calculus_done = 1: Update(): Exit Sub
				Case Else: in_key = ""
			End Select
		End If
		GetMouse x_mouse, y_mouse, , click
		If click = 1 Then Exit Sub
	Next
	calculus_done = 1
End Sub


Sub Initialization()'******************************************** INITIALIZATION
	WindowTitle "The Airy Disk Pattern According to the Huygens Principle."
	mode = "energy"
	calculus_done = 0                                                             'set calculus_done = 1 if the calculus is no longer required.
	brightness = 1
	x_center = x_width / 2
	x_focal_plane = x_center
	y_center = y_height / 2
	aperture_angle = 7                                                          'cone angle as referred to the optical axis.
   source_angle = aperture_angle / (180 / pi)
   lambda = 20
	images = 64
	image = 0
	circle_number = 100                                                           'number of concentric circles (less than 100 is inaccurate).
	y_curve_1 = 680                                                               'energy curves.
	y_curve_2 = 632                                                               'axial curves.
	ScreenSet 2, 2                                                                'matrix page.
	Color black, background: Cls
	mouse_line_59 = " B - Black and White. ": Locate 59, 39: Print mouse_line_59
	mouse_line_60 = " C - Color.           ": Locate 60, 39: Print mouse_line_60
	mouse_line_61 = " E - Energy.          ": Locate 61, 39: Print mouse_line_61
	mouse_line_62 = " I - Initialize.      ": Locate 62, 39: Print mouse_line_62
	mouse_line_63 = " Q - Quit (Esc.)      ": Locate 63, 39: Print mouse_line_63
	deleting =      "                      "
	Line(295, 777)-(514, 867), black, b
	Locate 50, 70: Print "Click on the image to move the focal plane cursor."
	Locate 51, 70: Print "You may also use the left/right arrows."
	Locate 52, 70: Print "Right click to reset."
	Locate 54, 70: Print "Please click slowly!"
	Locate 50, 40: Print "Click to select:"
	Locate 64, 39: Print " P - Pause.";
	Locate 45,100: Print "Gabriel LaFreniere     glafreniere.com         Oct. 8, 2010."
	Line(0, 720)-(1280, 720), black
	Color RGB(0,150,0), background
	Locate 62, 121: Print "October 8, 2010. This program may be"
	Locate 63, 121: Print "freely distributed, copied or modified."
	Locate 64, 121: Print "Gabriel LaFreniere  glafreniere.com";
	Color black
	PCopy 2, 0
	PCopy 2, 1
	Update()
End Sub


Sub Spherical_Source()'SIMPLIFIED CALCULUS FOR A FULL SPHERICAL SOURCE (ELECTRON)
	PCopy 2, work_page
	ScreenSet work_page, work_page
	'Line(0, 0)-(x_width, y_height),background, bf                                 'erasing.
	source_angle = aperture_angle / (180 / pi)
	angle_step = source_angle / circle_number                                     'this algorithm was explained in Ether16_hemisphere.bas
	For x = 0 To x_width / 2
		x_Distance = x  / lambda
		x_squared = x_Distance ^ 2
		path_difference = x_Distance - Int(x_Distance)
		phase_1 = 2 * pi * path_difference
		For y = 0 To y_center
			sine_amplitude = 0: cosine_amplitude = 0
			y_Distance = y / lambda
			distance = Sqr(y_Distance ^ 2 + x_squared)
			For angle = angle_step / 2  To pi Step angle_step
				path_difference = distance * Cos(angle)
				phase = 2 * pi * path_difference
				sine_amplitude = sine_amplitude + Sin(angle) * Sin(phase)
				cosine_amplitude = cosine_amplitude + Sin(angle) * Cos(phase)
			Next
			period(x, y) = Atn(sine_amplitude / cosine_amplitude)
			If cosine_amplitude < 0 Then period(x, y) = period(x, y) + pi
			luminance(x, y) = 2 * normalize * Sqr(sine_amplitude * sine_amplitude + cosine_amplitude * cosine_amplitude)
			shade = luminance(x, y)
			If shade > 255 Then shade = 255
			gray_shade(x, y) = RGB(shade, shade, shade)
			r_red = 100 * (Cos(period(x, y)) + 1)
			g_green = 100 * (Cos(period(x, y) + pi) + 1)
			r_red = r_red + (luminance(x, y) - 155)
			g_green = g_green + (luminance(x, y) - 155)
			If r_red > 255 Then r_red = 255 Else If r_red < 0 Then r_red = 0
			If g_green > 255 Then g_green = 255 Else If g_green < 0 Then g_green = 0
			b_blue = (r_red + g_green) / 2
			If lambda < 6 Then c_color(x, y) = gray_shade(x, y) Else c_color(x, y) = RGB(r_red, g_green, b_blue)
			If mode = "energy" Or mode = "axial" Then
				PSet(x_center + x, y_center - y), gray_shade(x, y)
				PSet(x_center + x, y_center + y), gray_shade(x, y)
				PSet(x_center - x, y_center - y), gray_shade(x, y)
				PSet(x_center - x, y_center + y), gray_shade(x, y)
			ElseIf mode = "color" Then
				PSet(x_center + x, y_center - y), c_color(x, y)
				PSet(x_center + x, y_center + y), c_color(x, y)
				PSet(x_center - x, y_center - y), c_color(x, y)
				PSet(x_center - x, y_center + y), c_color(x, y)
			End If
			If y = 0 Then
				axial_period(x) = phase_1 - period(x, y)
				axial_amplitude(x) = .15 * luminance(x, y)
			End If
		Next
		in_key = Inkey: If Len(in_key) Then Exit Sub
		GetMouse x_mouse, y_mouse, , click
		If click = 1 Then Exit Sub
	Next
	calculus_done = 1
End Sub


Sub Update()'****************************************** UPDATING CURRENT CHOICES
	ScreenSet 2, 2                                                                'hidden matrix page = 2.
	in_key = ""
	source_angle = aperture_angle / (180 / pi)
	If aperture_angle > 89 Then
		small_radius = lambda / 2
		large_radius = lambda * (1 + .5 * Cos(source_angle))
	Else
		small_radius = lambda * (1 + .226 * (Cos(source_angle))) / (2 * Sin(source_angle))'1.226 seems more accurate than the well known 1.22 Airy number.
		large_radius = 2 * lambda * (1 / Sin(source_angle)) ^ 2 - lambda * Sin(source_angle)
		large_radius =  lambda * (2 / Sin(source_angle) ^ 2 - 1 + .64 * Cos(source_angle))
		
	End If
	Locate 33, 106: Print "Airy Disk Radius....... ";
	Select Case small_radius / lambda
		Case Is < 9.995: Print Using "#.##"; small_radius / lambda;
		Case Is < 99.95: Print Using "##.#"; small_radius / lambda;
		Case Else:       Print Using "### "; small_radius / lambda;
	End Select
	Print " lambda (small radius)."
	Locate 33, 29: Print "Airy Ellipsoid...... ";
	Select Case large_radius / lambda
		Case Is < 9.995: Print Using "#.##"; large_radius / lambda;
		Case Is < 99.95: Print Using "##.#"; large_radius / lambda;
		Case Is < 999.9: Print Using "### "; large_radius / lambda;
		Case Else:       Print Using "####"; large_radius / lambda;
	End Select
	Print " lambda (large radius on the optical axis)."
	Locate 33, 2: Print "Aperture Angle.... ";
	Select Case aperture_angle
		Case Is < 10:   Print Using "#.##"; aperture_angle;: Print Chr(248); "  "
		Case Is < 100:  Print Using "##.##"; aperture_angle;: Print Chr(248); " "
		Case Is < 1000: Print Using "###.##"; aperture_angle;: Print Chr(248)
	End Select
	Locate 34, 2: Print "Bilateral Angle... ";
			Select Case 2 * aperture_angle
				Case Is < 10:   Print Using "#.#"; 2 * aperture_angle;: Print Chr(248); "  "
				Case Is < 100:  Print Using "##.#"; 2 * aperture_angle;: Print Chr(248); " "
				Case Is < 1000: Print Using "###.#"; 2 * aperture_angle;: Print Chr(248)
			End Select
	If aperture_angle > 89 Then
		relative_aperture = 0
	Else relative_aperture = .5 / Tan(source_angle)
	End If
	Locate 34, 29:  Print "Relative Aperture... Ÿ /";
	If relative_aperture < 9.995 Then
	   Print Using " #.##"; relative_aperture
	Else
	   Print Using " ##.#"; relative_aperture
	EndIf
	If aperture_angle = 180 Then
		Print " (full spherical source).      "
	ElseIf aperture_angle > 90 Then
		Print " (incomplete spherical source)."
	ElseIf aperture_angle = 90 Then
		Print " (hemispheric source).         "
	Else
		Print " (concave source).             "
	EndIf
	Locate 34,106: Print "Wavelength (lambda).... ";
	Select Case lambda
		Case Is < 9.995: Print Using "#.##"; lambda;
		Case Is < 99.95: Print Using "##.#"; lambda;
		Case Else:       Print Using "###"; lambda;
	End Select
	If lambda < 1.995 Then Print " pixel.  " Else Print " pixels. "
	Locate 52, 39: Print " Aperture Angle... ";
		Select Case aperture_angle
			Case Is < 10:   Print Using "#.##"; aperture_angle;: Print Chr(248); "  "
			Case Is < 100:  Print Using "##.##"; aperture_angle;: Print Chr(248); " "
			Case Is < 1000: Print Using "###.##"; aperture_angle;: Print Chr(248)
		End Select
	Locate 53, 39: Print " Wavelength....... ";
	Select Case lambda
		Case Is < 9.995: Print Using "#.##"; lambda
		Case Is < 99.95: Print Using "##.#"; lambda
		Case Else:       Print Using "### ";  lambda
	End Select
	Locate 54, 39: Print " Brightness.......";: Print Using " #.#"; brightness
	Locate 59, 39: Print mouse_line_59
	Locate 60, 39: Print mouse_line_60
	Locate 61, 39: Print mouse_line_61
	Locate 62, 39: Print mouse_line_62
	Locate 63, 39: Print mouse_line_63
   Locate 35, 2: Print "These graphics were generated by the summation of a large number of Huygens' wavelets."
	Locate 45, 2
	Select Case mode
		Case "gray"
			Print "The cyan curve indicates the departure from the axial wavelet normal phase."
		Case "color"
			Print "The cyan curve indicates the departure from the axial wavelet normal phase."
		Case "energy"
			Print "Amplitude distribution on the optical axis.                                "
	End Select
	Color RGB(0,0,255)
	Select Case mode
		Case "gray":   Locate 59, 39: Print mouse_line_59
		Case "color":  Locate 60, 39: Print mouse_line_60
		Case "energy": Locate 61, 39: Print mouse_line_61                          'current mode displayed in blue.
	End Select
	Color black, background
	'ScreenSet work_page, visible_page
End Sub
