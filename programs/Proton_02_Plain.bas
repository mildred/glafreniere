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
Const background = RGB(225,225,225)

Declare Sub Calculus()          
Declare Sub Display()          
Declare Sub Initialization()

Dim Shared As Integer work_page = 1, visible_page
Dim Shared As Integer r, g, b, x, y, z, previous
Dim Shared As Integer bitmap, x_width = 1280, y_height = 720, x_center, y_center
Dim Shared As Integer image, images, x_mouse, y_mouse, mouse_line, click

Dim Shared As Single x_1, x_2, x_3, x_4, y_1, y_2, y_3, y_4
Dim Shared As Single x_coord, y_coord, x_squared, y_squared, x_distance, y_distance
Dim Shared As Single shade(-1 To x_width + 1, -1 To y_height + 1)
Dim Shared As Single lambda, length, phase, radian, angle, diagonal_1, diagonal_2
Dim Shared As Single distance, side, half_side, colour, amplitude, brightness, gray_shade

Dim Shared As String in_key, mode, deleting
Dim Shared As String mouse_line_59, mouse_line_60, mouse_line_61, mouse_line_62, mouse_line_63
Screen 21,24,3: Initialization()

Do
	GetMouse x_mouse, y_mouse, , click
	Calculus()
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
	Swap work_page, visible_page
   ScreenSet work_page, visible_page
	PCopy 2, work_page
   For x = 0 To x_width
   	For y = 0 To y_height
   		shade(x, y) = 0
   	Next
   Next
   Locate 10, 12: Print " First electron...."
   For x = 0 To x_width
   	Locate 10, 31: Print Using "##### /"; x;: print x_width
   	For y = 0 To y_height
	   	x_coord = x
	   	y_coord = y
	   	x_distance = x_coord - x_1
   	   y_distance = y_coord - y_1
   		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   		radian = 2 * pi * distance / lambda
   		If distance Then shade(x, y) = shade(x, y) + Sin(radian) / (distance) Else shade(x, y) = shade(x, y) + 1
   	Next
   Next
   Locate 11, 12: Print " Second electron..."
   For x = 0 To x_width
   	Locate 11, 31: Print Using "##### /"; x;: print x_width
   	For y = 0 To y_height
	   	x_coord = x
	   	y_coord = y
	   	x_distance = x_coord - x_2
   	   y_distance = y_coord - y_2
   		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   		radian = 2 * pi * distance / lambda + pi                                 'opposite spin.
   		If distance Then shade(x, y) = shade(x, y) + Sin(radian) / (distance) Else shade(x, y) = shade(x, y) - 1
   	Next
   Next
   Locate 12, 12: Print " Third electron...."
   For x = 0 To x_width
   	Locate 12, 31: Print Using "##### /"; x;: print x_width
   	For y = 0 To y_height
	   	x_coord = x
	   	y_coord = y
	   	x_distance = x_coord - x_3
   	   y_distance = y_coord - y_3
   		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   		radian = 2 * pi * distance / lambda
   		If distance Then shade(x, y) = shade(x, y) + Sin(radian) / (distance) Else shade(x, y) = shade(x, y) + 1
   	Next
   Next
   Locate 13, 12: Print " Last electron....."
   For x = 0 To x_width
   	Locate 13, 31: Print Using "##### /"; x;: print x_width
   	For y = 0 To y_height
	   	x_coord = x
	   	y_coord = y
	   	x_distance = x_coord - x_4
   	   y_distance = y_coord - y_4
   		distance = Sqr(x_distance * x_distance + y_distance * y_distance)
   	   radian = 2 * pi * distance / lambda + pi                                 'opposite spin.
   		If distance Then shade(x, y) = shade(x, y) + Sin(radian) / (distance) Else shade(x, y) = shade(x, y) - 1
   	Next
   Next
   Display()
	side += 1 * lambda
	half_side = .5 * side
	x_1 = 10 * lambda
	y_1 = 10 * lambda
	x_2 = x_1 + 2 * half_side
	y_2 = y_1
	x_3 = x_2
	y_3 = y_1 + 2 * half_side
	x_4 = x_1
	y_4 = y_3
End Sub


Sub Display() '***************************************** DISPLAYING THE GRAPHICS
	phase = 2 * pi * image / images
	Select Case mode
		Case "black and white"
		   For x = 0 To x_width
		   	For y = 0 To y_height
		   		gray_shade = Abs(shade(x, y))
		   		gray_shade += Abs(shade(x, y-1)) + Abs(shade(x+1, y)) + Abs(shade(x, y+1)) + Abs(shade(x-1, y))
		   		gray_shade = 2000 * gray_shade
		   	   If gray_shade > 255 Then gray_shade = 255
		 	      PSet(x, y), RGB(gray_shade, gray_shade, gray_shade)
		   	Next
		   Next
		Case "color"
		   For x = 0 To x_width
		   	For y = 0 To y_height
		   		colour = 40000 * shade(x, y)^2 / lambda^(1/8)
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
		 	      PSet(x, y), RGB(r,g,b)
		   	Next
		   Next
			
	End Select
	For distance = x_width - x_2 To half_side Step -1 
		diagonal_1 = Sqr(half_side^2 + distance^2)
		diagonal_2 = Sqr(half_side^2 + (distance + side)^2)
		length = diagonal_2 - diagonal_1
		'? length / lambda - Int(length / lambda), (length Mod lambda)
		If length / lambda - Int(length / lambda) < .1 Then
			Line(x_2 + distance, y_1 + half_side - 2)-(x_2 + distance, y_1 + half_side + 2), green, bf
			previous += 1
		ElseIf length / lambda - Int(length / lambda) > .46 And length / lambda - Int(length / lambda) < .54 Then
			Line(x_2 + distance, y_1 + half_side - 2)-(x_2 + distance, y_1 + half_side + 2), red, bf
		Else
			If previous Then
				circle(x_2 + distance + previous / 2, y_1 + half_side), .03 * distance, white,,,1
				previous = 0
			EndIf
			'PSet(x_2 + distance, y_1 + half_side), blue
		EndIf
	Next
	Locate 47, 2: Print "Image.........."; image
	Locate 48, 2: Print "Lambda........."; lambda; " pixels."
	Locate 49, 2: Print "Side..........."; side / lambda; " x lambda"
	distance = Sqr(2) * side / lambda
	Locate 50, 2: Print "Diagonal......."; distance; " x lambda"
	Locate 51, 2: Print "Decimal........"; distance - Int(distance)
	'If Abs(.8 - (distance - Int(distance))) < .04 Then Sleep 1000
	If image = 1 Or image = 13 Or image = 30 Or image = 50 Then Sleep
	image += 1
	If image = 1000 Then image = 0
End Sub


Sub Initialization()'******************************************** INITIALIZATION
	WindowTitle "Four-Electron Interference Pattern."
	mode = "black and white"
	brightness = 1
	x_center = x_width / 2
	y_center = y_height / 2
   lambda = 10
	images = 64
	image = 0
	side = 25.3 * lambda'13.25; 42.25; 71.25 --- 13.3; 18.8 --- 25.3; 35.779 --- 42.3; 59.82
	half_side = .5 * side
	x_1 = 10 * lambda
	y_1 = 10 * lambda'y_center - half_side
	x_2 = x_1 + 2 * half_side
	y_2 = y_1
	x_3 = x_2
	y_3 = y_1 + 2 * half_side
	x_4 = x_1
	y_4 = y_3
	ScreenSet 2, 2                                                                'matrix page.
	Color black, background: Cls
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
	Line(0, 720)-(1280, 720), black
	Color RGB(0,150,0), background
	Locate 62, 121: Print "November 30, 2010. This program may be"
	Locate 63, 121: Print "freely distributed, copied or modified."
	Locate 64, 121: Print "Gabriel LaFreniere  glafreniere.com";
	Color black, background
   Calculus()                                                                    'electrons on the vertices of a square.
End Sub

