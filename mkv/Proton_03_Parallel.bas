' Interference pattern from multiple emitters.
Width 80,18:Color 0,15:Cls
?:? " Created by Gabriel LaFreniere."
?:? " This is a FreeBasic program."
?   " http://fbide.freebasic.net"
?   "    ...or download the FbEdit editor/compiler from:"
?   " http://radasm.110mb.com/fbedit/"
?   " http://sourceforge.net/projects/fbedit/"

Const pi = 4 * Atn(1)
Const red =  RGB(255,0,0)
Const cyan = RGB(0,200,200)
Const gray = RGB(128,128,128)
Const blue = RGB(0,0,255)
Const green = RGB(0,255,0)
Const black = RGB(0,0,0)
Const white = RGB(255,255,255)
Const yellow = RGB(255,255,0)
Const purple = RGB(255, 0, 255)
Const background = RGB(225,225,225)

Declare Sub Calculus()          
Declare Sub Display()          
Declare Sub Initialization()
Declare Sub Screen_Capture()

Dim Shared As Integer visible_page, work_page = 1, matrix_page = 2, previous_amplitude
Dim Shared As Integer r, g, b, x, y, z, y_curve, colour, gray_shade
Dim Shared As Integer x_width = 960, y_height = 720, x_center, y_center, bitmap = 0
Dim Shared As Integer image, images, x_mouse, y_mouse, mouse_line, click

Dim Shared As Single x_1, x_2, x_3, x_4, y_1, y_2, y_3, y_4
Dim Shared As Single x_coord, y_coord, x_squared, y_squared, z_squared
Dim Shared As Single x_distance, y_distance, z_distance
Dim Shared As Single shade(x_width, y_height)
Dim Shared As Single brightness, amplitude, phase, radian, angle, lambda, distance, side, half_side

Dim Shared As String in_key, mode, deleting, file, bitmap_number
Dim Shared As String mouse_line_59, mouse_line_60, mouse_line_61, mouse_line_62, mouse_line_63
Screen 21,24,3: Initialization()

Do
	Swap work_page, visible_page
	Screenset work_page, visible_page
	Pcopy matrix_page, work_page
	Calculus()
	Display()
	If bitmap Then Screen_Capture()
	GetMouse x_mouse, y_mouse, , click
	in_key = Inkey'--------------------------------------------------------- KEYBOARD MANAGEMENT.
	If Len(in_key) Then
		If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = UCase(in_key)
		Select Case in_key
			Case "I": Initialization()
			Case "P"
				Color red: ScreenSet visible_page
				Locate 64, 39: Print " Paused. Press Esc. to Quit. Press any other key to resume.";
				Sleep: If Inkey = Chr(27) Then End Else in_key = ""
				Color black: Locate 64, 39: Print " P - Pause.";
			Case "k+", "X+", Chr(27): End
			Case "+": z_distance += lambda 
			Case "-": z_distance -= lambda 
			Case "=": brightness = 1
			Case Else: in_key = ""
		End Select
		Do: Loop While Len(Inkey) 
	End If
	If click = 1 Then Else GetMouse x_mouse, y_mouse, , click
	mouse_line = .5 + y_mouse / 16
	Locate 62, 2: Print "Mouse Data"; x_mouse; y_mouse; click; mouse_line; "    "
	If mouse_line > 23 And mouse_line < 64 Then
		If x_mouse < 296 Or x_mouse > 514 Then mouse_line = 0
	Else mouse_line = 0
	End If
	Color blue, white'----------------------------------------------------------- MOUSE POSITION.
	Locate mouse_line, 39
	Select Case mouse_line
		Case 53: Print deleting: Locate 53, 39
			Print " Wavelength....... ";
			Select Case lambda
				Case Is < 9.995: Print Using "#.##   "; lambda
				Case Is < 99.95: Print Using "##.#   "; lambda
				Case Else:       Print Using "###    ";  lambda
			End Select
		Case 54: Print deleting: Locate 54, 39
			Print " Brightness.......";: Print Using " #.#    "; brightness
		Case 59: If mode = "gray"   Then Else Print mouse_line_59
		Case 60: If mode = "color"  Then Else Print mouse_line_60
		Case 61: If mode = "energy" Then Else Print mouse_line_61
		Case 62: Print mouse_line_62
		Case 63: Print mouse_line_63
		Case 37: Print deleting;
	End Select
	Color black, background

	If click = 1 Then	'-------------------------------------------------------- MOUSE MANAGEMENT.
		bitmap = 0: image = 0
		Select Case mouse_line
			Case 62: Initialization()
			Case 63: End
		End Select
	End If
	Sleep 1
Loop


Sub Calculus() '************************************************* WAVE SUMMATION
   For x = 0 To x_width
   	For y = 0 To y_height
   		shade(x, y) = 0
   	Next
   Next
   Line(0, 0)-(x_width, y_height), black, bf
   Color black, background
   For x = 0 To x_width
	   x_distance = x - x_1
	   x_squared = x_distance^2
   	For y = 0 To y_height
	   	y_coord = y
   	   y_distance = y_coord - y_1
   		distance = Sqr(x_squared + y_distance * y_distance + z_squared)
   		radian = 2 * pi * distance / lambda
   		amplitude = Sin(radian) / radian                                          'electron formula.
   		shade(x, y) = shade(x, y) + amplitude
   	Next
   Next
   For x = 0 To x_width
	   x_distance = x - x_2
	   x_squared = x_distance^2
   	For y = 0 To y_height
	   	y_coord = y
   	   y_distance = y_coord - y_2
   		distance = Sqr(x_squared + y_distance * y_distance + z_squared)
   		radian = 2 * pi * distance / lambda
   		amplitude = Sin(radian + pi) / radian                                          'electron formula.
   		shade(x, y) = shade(x, y) + amplitude
   	Next
   Next
   For x = 0 To x_width
	   x_distance = x - x_3
	   x_squared = x_distance^2
   	For y = 0 To y_height
	   	y_coord = y
   	   y_distance = y_coord - y_3
   		distance = Sqr(x_squared + y_distance * y_distance + z_squared)
   		radian = 2 * pi * distance / lambda
   		amplitude = Sin(radian) / radian                                          'electron formula.
   		shade(x, y) = shade(x, y) + amplitude
   	Next
   Next
   For x = 0 To x_width
	   x_distance = x - x_4
	   x_squared = x_distance^2
   	For y = 0 To y_height
	   	y_coord = y
   	   y_distance = y_coord - y_4
   		distance = Sqr(x_squared + y_distance * y_distance + z_squared)
   		radian = 2 * pi * distance / lambda
   		amplitude = Sin(radian + pi) / radian                                          'electron formula.
   		shade(x, y) = shade(x, y) + amplitude
   	Next
   Next
   Select Case image
   	Case Is > 400: z_distance += .01 * lambda
   	Case Is > 100: z_distance += .05 * lambda
   	Case Is >  70: z_distance += .1  * lambda
   	Case Is >   0: z_distance +=  1  * lambda
   End Select
	 
	z_squared = z_distance^2
End Sub


Sub Display() '***************************************** DISPLAYING THE GRAPHICS
	Select Case mode
		Case "energy"
		   For x = 0 To x_width
		   	For y = 0 To y_height
		   		gray_shade = 4000 * Abs(shade(x, y))^2 / lambda^(1/4)
		   	   If gray_shade > 255 Then gray_shade = 255
		 	      PSet(x, y), RGB(gray_shade, gray_shade, gray_shade)
		   	Next
		   Next
		Case "color"
		   For x = 0 To x_width
		   	For y = 0 To y_height
		   		amplitude = Abs(shade(x, y)) / lambda
		   		colour = z_distance * 700 * amplitude
		   		r = 0: g = 0
		   		b = colour / 2
		   		If shade(x, y) > 0 Then
		   			r = colour
		   			If colour > 255 Then g = colour - 255 
		   		Else
		   			g = colour
		   			If colour > 255 Then r = colour - 255 
		   		EndIf
		   		If r > 255 Then r = 255
		   		If g > 255 Then g = 255
		   		If b > 255 Then b = 255
		 	      If Point(x, y) = black Then PSet(x, y), RGB(r,g,b)
		   	Next
		   Next
	End Select
	Circle(x_1, y_1), lambda / 2, white,,,1
	'Paint (x_1, y_1), white, white
	Circle(x_1, y_1), lambda, white,,,1
	Circle(x_2, y_2), lambda / 2, white,,,1
	'Paint (x_2, y_2), white, white
	Circle(x_2, y_2), lambda, white,,,1
	Circle(x_3, y_3), lambda / 2, white,,,1
	'Paint (x_3, y_3), white, white
	Circle(x_3, y_3), lambda, white,,,1
	Circle(x_4, y_4), lambda / 2, white,,,1
	'Paint (x_4, y_4), white, white
	Circle(x_4, y_4), lambda, white,,,1
	Locate 39, 124: Print "Lambda.....";: Print Using "###.##"; lambda;: Print " pixels."
	Locate 40, 124: Print "Side.......";: Print Using "###.##"; side / lambda;: Print " x lambda."
	Locate 41, 124: Print "Diagonal...";: Print Using "###.##"; Sqr(2) * side / lambda;: Print " x lambda."
	Locate 42, 124: Print "Offset.....";: Print Using "###.##"; z_distance / lambda;: Print " x lambda."
	Locate 43, 124: Print "Offset.....";: Print Using "###.##"; z_distance / side;:   Print " x side."
	image += 1
	Locate 45, 123: Print image
	If image = 1000 Then image = 0
End Sub


Sub Initialization()'******************************************** INITIALIZATION
	WindowTitle "Four-Electron Hypothesis - Interferences for Progressive Distances on a Parallel Plane."
	mode = "color"
	brightness = 1
	x_center = x_width / 2
	y_center = y_height / 2
	y_curve = y_height + 50
   lambda = 16
	images = 32
	image = 0
	ScreenSet matrix_page, matrix_page
	Color black, background: Cls
	Color black, white
	side = 13.25 * lambda' 13.25; 42.25; 71.25  -   13.3 vs. 18.8
	half_side = .5 * side
	z_distance = 1 * lambda' 82.55
	z_squared = z_distance^2
	x_1 = x_center - half_side
	y_1 = y_center - half_side
	x_2 = x_1 + side
	y_2 = y_1
	x_3 = x_2
	y_3 = y_1 + side
	x_4 = x_1
	y_4 = y_3
	Color black, background
	mouse_line_59 = " B - Black and White.     ": Locate 59, 39: Print mouse_line_59
	mouse_line_60 = " C - Color.               ": Locate 60, 39: Print mouse_line_60
	mouse_line_61 = " E - Energy.              ": Locate 61, 39: Print mouse_line_61
	mouse_line_62 = " I - Initialize.          ": Locate 62, 39: Print mouse_line_62
	mouse_line_63 = " Q - Quit (Esc.)          ": Locate 63, 39: Print mouse_line_63
	deleting =      "                          "
	'Line(295, 777)-(514, 867), black, b
   Locate 02,124: Print "   THE WAVE STRUCTURE OF MATTER"
   Locate 04,124: Print "Here, the proton is considered to be"
   Locate 05,124: Print "made out of four electrons placed on"
   Locate 06,124: Print "the vertices of a square. Their"
   Locate 07,124: Print "neighbors are in antiphase so that"
   Locate 08,124: Print "opposite electrons are in phase."
   Locate 10,124: Print "This is the resulting interference"
   Locate 11,124: Print "pattern on a parallel plane whose"
   Locate 12,124: Print "distance increases rapidly, then"
   Locate 13,124: Print "more slowly."
   Locate 15,124: Print "The important point is that any"
   Locate 16,124: Print "low-energy area (shown in black) is"
   Locate 17,124: Print "capable of capturing an electron on"
   Locate 18,124: Print "condition that there is no isolated"
   Locate 19,124: Print "electron in the vicinity."
   Locate 21,124: Print "Please note: those are spherical"
   Locate 22,124: Print "standing waves. Interferences from"
   Locate 23,124: Print "regular traveling waves would not"
   Locate 24,124: Print "produce the same effect."
    
	Locate 56, 40: Print "Click to select."
	Locate 57, 40: Print "Please click slowly!"
	Locate 64, 39: Print " P - Pause.";
	Color RGB(0,150,0), background
	Locate 62, 121: Print "November 29, 2010. This program may be"
	Locate 63, 121: Print "freely distributed, copied or modified."
	Locate 64, 121: Print "Gabriel LaFreniere  glafreniere.com";
	Color black, background
End Sub


Sub Screen_Capture()
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
	Color black, background
	BSave file, 0
	bitmap += 1
	If bitmap > 1000 Then End

End Sub
