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
Declare Sub Update()          

Dim Shared As Integer work_page = 1, visible_page, previous_amplitude, calculus_done
Dim Shared As Integer r, g, b, x, y, z, y_curve, colour, gray_shade
Dim Shared As Integer bitmap, x_width = 1000, y_height = 108, x_center, y_center
Dim Shared As Integer image, images, x_mouse, y_mouse, mouse_line, click

Dim Shared As Single x_1, x_2, x_3, x_4, y_1, y_2, y_3, y_4
Dim Shared As Single x_coord, y_coord, x_squared, y_squared, x_distance, y_distance
Dim Shared As Single shade(x_width, y_height)
Dim Shared As Single brightness, amplitude, phase, radian, angle, lambda, distance, side, half_side

Dim Shared As String in_key, mode, deleting, hypothesis
Dim Shared As String mouse_line_59, mouse_line_60, mouse_line_61, mouse_line_62, mouse_line_63
Screen 21,24,3: Initialization()

Do
	Display()
	GetMouse x_mouse, y_mouse, , click
	in_key = Inkey'--------------------------------------------------------- KEYBOARD MANAGEMENT.
	If Len(in_key) Then
		If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = UCase(in_key)
		Select Case in_key
			Case "B": If mode <> "gray"   Then mode = "gray":   Update()
			Case "C": If mode <> "color"  Then mode = "color":  Update()
			Case "E": If mode <> "energy" Then mode = "energy": Update()
			Case "I": Initialization()
			Case "P"
				Color red: ScreenSet visible_page
				Locate 64, 39: Print " Paused. Press Esc. to Quit. Press any other key to resume.";
				Sleep: If Inkey = Chr(27) Then End Else in_key = ""
				Color black: Locate 64, 39: Print " P - Pause.";
			Case "k+", "X+", Chr(27): End
			Case "+": lambda = lambda * Sqr(2): Update()
			Case "-": lambda = lambda / Sqr(2): Update()
			Case "=": brightness = 1: Update()
			Case "Z": calculus_done = 1: Update()
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
			Case 59: If mode <> "gray"   Then mode = "gray":   Update()
			Case 60: If mode <> "color"  Then mode = "color":  Update()
			Case 61: If mode <> "energy" Then mode = "energy": Update()
			Case 62: Initialization()
			Case 63: End
		End Select
	End If
	Sleep 1
Loop


Sub Calculus() '************************************************* WAVE SUMMATION
   ScreenSet work_page, work_page
   PCopy 2, work_page
   For x = 0 To x_width
   	For y = 0 To y_height
   		shade(x, y) = 0
   	Next
   Next
   'Line(0, 0)-(x_width, y_height), black, bf
   Color black, background
   'Locate 60, 01: Print " First electron...."
   'For x = 0 To x_width
   '	Locate 60, 19: Print Using "##### /"; x;: print x_width
   '	For y = 0 To y_height
	'   	x_coord = x
	'   	y_coord = y
	'   	x_distance = x_coord - x_1
   '	   y_distance = y_coord - y_1
   '		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   '		radian = 2 * pi * distance / lambda
   '		If distance Then amplitude = Sin(radian) / radian Else amplitude = 1      'electron formula.
   '		shade(x, y) = shade(x, y) + amplitude
   '	Next
   'Next
   'Locate 61, 01: Print " Second electron..."
   'For x = 0 To x_width
   '	Locate 61, 19: Print Using "##### /"; x;: print x_width
   '	For y = 0 To y_height
	'   	x_coord = x
	'   	y_coord = y
	'   	x_distance = x_coord - x_2
   '	   y_distance = y_coord - y_2
   '		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   '		radian = 2 * pi * distance / lambda
   '		If distance Then amplitude = Sin(radian + pi) / radian Else amplitude = -1'opposite spin.
   '		shade(x, y) = shade(x, y) + amplitude
   '	Next
   'Next
   'Locate 62, 01: Print " Third electron...."
   'For x = 0 To x_width
   '	Locate 62, 19: Print Using "##### /"; x;: print x_width
   '	For y = 0 To y_height
	'   	x_coord = x
	'   	y_coord = y
	'   	x_distance = x_coord - x_3
   '	   y_distance = y_coord - y_3
   '		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   '		radian = 2 * pi * distance / lambda
   '		If distance Then amplitude = Sin(radian) / radian Else amplitude = 1      'electron formula.
   '		shade(x, y) = shade(x, y) + amplitude
   '	Next
   'Next
   'Locate 63, 01: Print " Last electron....."
   'For x = 0 To x_width
   '	Locate 63, 19: Print Using "##### /"; x;: print x_width
   '	For y = 0 To y_height
	'   	x_coord = x
	'   	y_coord = y
	'   	x_distance = x_coord - x_4
   '	   y_distance = y_coord - y_4
   '		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   '		radian = 2 * pi * distance / lambda
   '		If distance Then amplitude = Sin(radian + pi) / radian Else amplitude = -1'opposite spin.
   '		shade(x, y) = shade(x, y) + amplitude
   '	Next
   'Next
   calculus_done = 1
   Display()
   Circle(1015, 10 * lambda + half_side), 25, white,,,1
   Circle(1015, 10 * lambda + half_side), lambda/2, white,,,1
   Paint (1015, 10 * lambda + half_side), white, white
End Sub


Sub Display() '***************************************** DISPLAYING THE GRAPHICS
	'If calculus_done = 0 Then Calculus()
   'Locate 13, 2: Print "A to B distance.......";: Print Using " ##.## lambda    "; side / lambda
   'Locate 14, 2: Print "Diagonal, not shown...";: Print Using " ##.## lambda    "; side * Sqr(2) / lambda
	phase = 2 * pi * image / images
   previous_amplitude = 0
   'Line(x_1, y_curve - 100)-(x_1, y_curve), black
   'Line(x_2, y_curve - 100)-(x_2, y_curve), black
  	previous_amplitude = 0
   For x = 0 To x_width
   	x_distance = x - x_1
		radian = 2 * pi * x_distance / lambda
		If x_distance Then amplitude = 2000 * Sin(radian) / radian Else amplitude = 2000     'electron formula (sinus cardinalis).
   	'If amplitude < -73 Then amplitude = -73 Else If amplitude > 49 Then amplitude = 49
   	Line(x-1, y_curve - previous_amplitude)-(x, y_curve - amplitude), cyan
   	previous_amplitude = amplitude
   Next
   previous_amplitude = 0
   For x = 0 To x_width
   	x_distance = x - x_2
		radian = 2 * pi * x_distance / lambda
		If x_distance Then amplitude = 2000 * Sin(radian) / radian Else amplitude = 2000
   	'If amplitude < -73 Then amplitude = -73 Else If amplitude > 49 Then amplitude = 49
   	Line(x-1, y_curve - previous_amplitude)-(x, y_curve - amplitude), purple
   	previous_amplitude = amplitude
   Next
   previous_amplitude = 0

   For x = 0 To x_width
   	x_distance = x - x_1
		radian = 2 * pi * x_distance / lambda
		If x_distance Then amplitude = 2000 * Sin(radian) / radian Else amplitude = 2000
   	x_distance = x - x_2
		radian = 2 * pi * x_distance / lambda
		If x_distance Then amplitude += 2000 * Sin(radian) / radian Else amplitude += 2000
   	'If amplitude < -73 Then amplitude = -73 Else If amplitude > 49 Then amplitude = 49
   	Line(x-1, y_curve - previous_amplitude)-(x, y_curve - amplitude), black
   	previous_amplitude = amplitude
   Next

	'Select Case mode
	'	Case "energy"
	'	   For x = 0 To x_width
	'	   	For y = 0 To y_height
	'	   		gray_shade = 6000 * Abs(shade(x, y))^2 / lambda^(1/4)
	'	   	   If gray_shade > 255 Then gray_shade = 255
	'	 	      PSet(x, y), RGB(gray_shade, gray_shade, gray_shade)
	'	   	Next
	'	   Next
	'	Case "color"
	'	   For x = 0 To x_width
	'	   	For y = 0 To y_height
	'	   		amplitude = Abs(shade(x, y)) / lambda
	'	   		colour = 600000 * amplitude^(1.2)
	'	   		r = 0: g = 0
	'	   		b = colour / 2
	'	   		If shade(x, y) > 0 Then
	'	   			r = colour
	'	   			If colour > 255 Then g = colour - 255 
	'	   		Else
	'	   			g = colour
	'	   			If colour > 255 Then r = colour - 255 
	'	   		EndIf
	'	   		If r > 255 Then r = 255
	'	   		If g > 255 Then g = 255
	'	   		If b > 255 Then b = 255
	'	 	      If Point(x, y) = black Then PSet(x, y), RGB(r,g,b)
	'	   	Next
	'	   Next
	'End Select
   'Line(x_center - 94, y_height + 81)-(x_center + 94, y_height + 100), white, bf
   'Line(x_center - 94, y_height + 81)-(x_center + 94, y_height + 100), black, b
   Color black, white
	'Select Case hypothesis
	'	Case "1a"
   '      Locate 13, 52: Print "    REPULSIVE FORCE    "
   '      Locate 04, 44: Print"A"
   '      Locate 04, 83: Print"B"
	'	Case "1b"
	'	   Locate 13, 52: Print "   ATTRACTIVE  FORCE   "
	'	   Locate 04, 43: Print"A"
	'	   Locate 04, 83: Print"B"
	'	Case "1c"
	'	   Locate 13, 52: Print " UNSTABLE  EQUILIBRIUM "
	'	   Locate 04, 42: Print"A"
	'	   Locate 04, 84: Print"B"
	'	Case "1d"
	'	   Locate 04, 43: Print"A"
	'	   Locate 04, 83: Print"B"
	'	   Line(x_center - 96, y_height + 79)-(x_center + 96, y_height + 102), black, bf
	'	   Line(x_center - 93, y_height + 82)-(x_center + 93, y_height + 099), yellow, bf
	'	   Color black, yellow
	'	   Locate 13, 52: Print "  STABLE  EQUILIBRIUM  "
	'End Select
   Color black, background
	'Select Case hypothesis
	'	Case "1a"
	'	   Locate 08, 02: Print "Destructive interferences."
	'	   Locate 08, 50: Print "Constructive interferences."
	'	   Locate 08, 99: Print "Destructive interferences."
   '      Locate 12, 2: Print "Lambda (wavelength)..."; lambda; " pixels."
	'	   Locate 13, 88: Print "Please note: those are standing waves."
	'	   Locate 14, 87: Print "Stronger in-between radiation pressure."
	'	Case "1b"
	'	   Locate 08, 02: Print "Constructive interferences."
	'	   Locate 08, 50: Print "Destructive interferences."
	'	   Locate 08, 99: Print "Constructive interferences."
	'	   Locate 14, 89: Print "Stronger external radiation pressure."
	'	Case "1c"
	'	   Locate 08, 52: Print "1/8 lambda phase shift."
	'	   Locate 14, 92: Print "Equilibrated radiation pressure."
	'	Case "1d"
	'	   Locate 08, 52: Print "1/8 lambda phase shift."
	'	   Locate 12, 92: Print "The phase shift neutralizes the"
	'	   Locate 13, 92: Print "negative charge of the electrons."
	'	   Locate 14, 92: Print "Equilibrated radiation pressure."
	'End Select

   Line(0, y_curve)-(x_width, y_curve), black, b
   Line(0, 2*y_curve)-(x_width, 2*y_curve), black, b
   Line(0, y_curve+39)-(x_width, y_curve+39), black, b
   Line(0, y_curve-39)-(x_width, y_curve-39), black, b
   Line(x_1+half_side, 0)-(x_1+half_side, 2*y_curve), black
   Line(x_1-half_side, 0)-(x_1-half_side, 2*y_curve), black
   Line(x_2+half_side, 0)-(x_2+half_side, 2*y_curve), black
   For x = x_1 - 14 * lambda To x_2 + 14 * lambda Step lambda
   	Line(x, y_curve - 8)-(x, y_curve), black
   Next
   For x = x_2 - 30 * lambda To x_2 + 14 * lambda Step lambda
   	Line(x, y_curve)-(x, y_curve + 8), black
   Next
   Locate 18, 2: Print "Diagonal..";: Print Using "##.### x lambda."; side / lambda
   Locate 19, 1: Print "(Side......";: Print Using "##.### x lambda)"; (side / lambda) / sqr(2)
Sleep: End
End Sub


Sub Initialization()'******************************************** INITIALIZATION
	WindowTitle "Four-Electron Hypothesis - Two electrons in phase on the diagonal."
	mode = "color"
	calculus_done = 0                                                             'set calculus_done = 1 if the calculus is no longer required.
	brightness = 1
	x_center = x_width / 2
	y_center = y_height / 2
	y_curve = y_height + 50
   lambda = 24
	images = 64
	image = 0
	ScreenSet 2, 2                                                                'matrix page.
	Color black, background: Cls
	Color black, white
   Line(0, 525)-(x_width - 1, 562), white, bf
   Line(0, 525)-(x_width - 1, 562), black, b
   Locate 34, 02: Print "Four electrons on the"
   Locate 35, 02: Print "vertices of a square."
   Locate 34, 40: Print "ANALYSIS 2       - Electrons A and C are on the diagonal."
   Locate 35, 40: Print "                 - A and C are in phase (same spin)."
   Line(217, 525)-(253, 562), black, b
   Locate 34, 29: Print"A B"
   Locate 35, 29: Print"D C"
   side = 18.8 * lambda
	half_side = .5 * side
	Color black, background
	mouse_line_59 = " B - Black and White.     ": Locate 59, 39: Print mouse_line_59
	mouse_line_60 = " C - Color.               ": Locate 60, 39: Print mouse_line_60
	mouse_line_61 = " E - Energy.              ": Locate 61, 39: Print mouse_line_61
	mouse_line_62 = " I - Initialize.          ": Locate 62, 39: Print mouse_line_62
	mouse_line_63 = " Q - Quit (Esc.)          ": Locate 63, 39: Print mouse_line_63
	deleting =      "                          "
	Line(295, 777)-(514, 867), black, b
	Locate 56, 70: Print "Please click slowly!"
	Locate 50, 40: Print "Click to select:"
	Locate 64, 39: Print " P - Pause.";
	Color RGB(0,150,0), background
	Locate 62, 121: Print "November 30, 2010. This program may be"
	Locate 63, 121: Print "freely distributed, copied or modified."
	Locate 64, 121: Print "Gabriel LaFreniere  glafreniere.com";
	Color black, background
	Update()
End Sub



Sub Update() '**************************************************** UPDATING DATA
	x_1 = x_center - half_side
	y_1 = y_center
	x_2 = x_1 + side
	y_2 = y_1
	x_3 = x_2
	y_3 = y_1 + side
	x_4 = x_1
	y_4 = y_3
   Calculus()                                                                    'electrons on the vertices of a square.
End Sub
