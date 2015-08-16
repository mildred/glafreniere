Width 80,20:Color 0,15:Cls:?
? " Created July 20, 2008 by Gabriel LaFreniere.":?:?
? " This FreeBasic program was adapted to the 0.20.0b Compiler (2008) for Windows:":?
? " http://www.freebasic.net/index.php/download":?:?
? " It should still be compatible with previous versions.":?
? " Please download the IDE (editor) from:":?
? " http://fbide.freebasic.net":?
' Subs are in alphabetical order.
Declare Sub Frame()                                                   'Press F2. Click the + on the left of "S Subs"
Declare Sub Graphics()                                                'to display the list. Double-click the Sub name.
Declare Sub Initialization()
Declare Sub Keyboard_Management()
Declare Sub Mouse_Management()
Declare Sub Reflection_Management()
Declare Sub Text()
Declare Sub Title()
Declare Sub Update()
Declare Sub Wave_Display()
Declare Sub Wave_Generator()

Dim Shared As Integer x_Fresnel, x_width, y_height, x_left, x_text, x_squared, y_text, top_start, left_start
Dim Shared As Integer lambda, quarter_lambda, half_lambda, x_emitter, x_receiver, x_pixel, y_pixel
Dim Shared As Integer x_transverse_axis, upper_emitter, lower_emitter, upper_receiver, lower_receiver, max_receiver
Dim Shared As Integer first_pixel_emitter, last_pixel_emitter, first_pixel_receiver, last_pixel_receiver
Dim Shared As Integer skipped_frames, iteration, pulse, pulses, x_coord, y_coord, luminance, x, y, r, g, b, n
Dim Shared As Integer previous, horizontally, vertically, pixel, luminance1, luminance2, center, x_center, y_center
Dim Shared As Integer click, x_mouse, y_mouse, wheel, previous_wheel, int_number, emitter_height, receiver_height
Dim Shared As Integer e_upper, r_upper, previous_upper, previous_lower, previous_emitter, previous_receiver, selecting
Dim Shared As Integer variable, skip, radius, x_start, y_start, axial_distance, number, upper_vertical, lower_vertical
x_width = 1280: y_height = 1024: n = 2 * x_width + y_height
Dim Shared As Integer xe_0(n), xe_0a(n), xe_0b(n), xe_1(n), xe_1a(n), xe_1b(n), xe_2(n), xe_3(n) 'e: emitter reflector.
Dim Shared As Integer ye_0(n), ye_0a(n), ye_0b(n), ye_1(n), ye_1a(n), ye_1b(n), ye_2(n), ye_3(n)
Dim Shared As Integer xr_0(n), xr_0a(n), xr_0b(n), xr_1(n), xr_1a(n), xr_1b(n), xr_2(n), xr_3(n) 'r: receiver reflector.
Dim Shared As Integer yr_0(n), yr_0a(n), yr_0b(n), yr_1(n), yr_1a(n), yr_1b(n), yr_2(n), yr_3(n)
Dim Shared As Single e_mix(n), r_mix(n)                               'emitter and receiver reflector trend to be mixed.
Dim Shared As Single present(-3 To x_width+3, -3 To y_height+3)
Dim Shared As Single past(-3 To x_width+3, -3 To y_height+3), trend(-3 To x_width+3, -3 To y_height+3)
Dim Shared As Single difference, amplitude, orthogonal, diagonal, kinetic, potential, factor, r_Schwarzschild
Dim Shared As Single small_radius, beta, g_Lorentz, h_sphere, gain, emitter_gain, reference, ratio, Lagrangian, t
Dim Shared As Single phase, angle, radian, k_Dewavrin, distance1, distance2, quadrature, brightness, sagitta, h_parabola
Dim Shared As String e_reflector, r_reflector, previous_reflector, reflection, in_key, display
Dim Shared As String line38a, line38b, line38c, line38d, line39a, line39b, line39c, line39d
Dim Shared As String line40a, line40b, line40c, line40d, line41a, line41b, line41c, line41d
Dim Shared As String line42a, line42b, line42c, line42d, line43a, line43b, line43c, line43d
Dim Shared As String line44a, line44b, line44c, line44d, line45a, line45b, line45c, line45d
Dim Shared As String line47a, line47b, line47c, line48a, line48b, line48c
Dim Shared As Ubyte matrix_page, work_page, visible_page, margin = 20, curve, line_number, frames
Const black = 0, white = Rgb(255,255,255), purple = Rgb(255,0,255), dark_gray = Rgb(75,75,75)
Const red = Rgb(255,0,0), blue = Rgb(0,0,255), cyan = Rgb(0,200,200), gray = Rgb(150,150,150)
Const gold = Rgb(180,150,100), buff = Rgb(255,255,200), blue_sky = Rgb(210,230,255)
Const green_text = Rgb(0,125,0), background = Rgb(225,225,225), green = Rgb(0,200,0)
Const pi = 4 * Atn(1), log_conversion = 4.3429448                     'conversion to common logarithm * 10

Screen 20,24,3
Initialization()
Update()


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  For frames = 0 To skipped_frames

' JOCELYN MARCOTTE'S OPTIMIZED 2-D WAVE ALGORITHM (CREATED IN 2006).   "THE PAST IS A GUIDE TO THE FUTURE"
  
    For x = 0 To horizontally: For y = y_height - vertically To vertically
      past(x,y)  = present(x,y)                                       'updating previous states.
      present(x,y) = trend(x,y)
    Next: Next
    For x = 0 To horizontally: For y = y_height - vertically To vertically
      orthogonal = present(x-1, y  ) + present(x,   y-1) + present(x,   y+1) + present(x+1, y  )'orthogonal influence.
      diagonal   = present(x-1, y-1) + present(x-1, y+1) + present(x+1, y-1) + present(x+1, y+1)'diagonal influence.
      trend(x,y) = .5 * orthogonal + .25 * diagonal - present(x,y) - past(x,y)                  'trend extrapolation.
    Next: Next
  
    If horizontally < x_width Then horizontally += 1
    If vertically < y_height Then vertically += 1
    iteration += 1
    Wave_Generator()
    Reflection_Management()
    If frames = 0 Then Wave_Display()
    in_key = Inkey                                                    'keyboard management.
    Getmouse x_mouse, y_mouse, wheel, click                           'mouse management.
    If Len(in_key) Or click > 0 Then Exit For                         'execute now.
  Next

'                            IMPORTANT

' The more accurate diagonal correction shown above was elaborated by
' Mr. Jocelyn Marcotte himself in 2006. Surprisingly, it also yields
' faster results because the wave speed is accelerated from .707 to
' 1 pixel per cycle exactly. This is possible because the sign for
' the "present" energy is negative (the positive sign leads to a
' slower wave speed). Additionally, the influence for diagonal
' transmission remains in accordance with the square of the distance
' law. This also works in 3-D using 26 neighbors and three levels
' of influence. Mr. Marcotte used this method in 2006 for reproducing
' my moving electron (he was the first one!) in full 3-D.
' The basic trend in 2-D may also be given by:

' trend(x,y) = (present(x-1,y) + present(x,y-1) + present(x,y+1) + present(x+1,y)) / 2 - past(x,y)

' This calculus is much simpler, yet the diagonal propagation is
' delayed to the next cycle. This is especially annoying for shorter
' wavelengths. It should also be emphasized that the "c" speed
' normalized to 1 is of the utmost importance for experiments
' involving motion. The Doppler effect becomes much easier to deal
' with, and this is especially true for the Lorentz transformations.
' Also, the phase can easily be related to the current iteration.

  If Len(in_key) Then Keyboard_Management()
  line_number = .5 + y_mouse / 16                                     'line number in text units.
  If line_number < 38 Then line_number = 0
  If y_mouse > -1 And y_mouse < y_height And x_mouse < x_width Then   'mouse on wave area.
    Pcopy matrix_page, work_page                                      'show graphics.
    If Abs(y_mouse - y_center) < 51 Then line_number = 16             'drag emitter or receiver.
    If x_mouse < 30 Then
      center = .5 * (upper_emitter + lower_emitter)
      If y_mouse < center Then line_number = 17 Else line_number = 18 'drag reflector edges (emitter).
    Elseif x_mouse > x_width - 30 Then
      center = .5 * (upper_receiver + lower_receiver)
      If y_mouse < center Then line_number = 19 Else line_number = 20 'drag reflector edges (receiver).
    End If
  End If
  If wheel <> previous_wheel And wheel <> -1 Then                     'select reflector height using mouse wheel.
    int_number = (y_center - upper_emitter) / quarter_lambda          'integer variable stands for lambda / 4 steps.
    factor = previous_wheel - wheel                                   'move to nearest lambda / 4 integer.
    upper_emitter = y_center - int_number * quarter_lambda + factor * quarter_lambda
    If upper_emitter > y_center-quarter_lambda Then upper_emitter = y_center-quarter_lambda Else If upper_emitter < 0 Then upper_emitter = 0
    lower_emitter = y_height - upper_emitter
    upper_receiver = upper_emitter
    lower_receiver = lower_emitter
    Update()
  End If
  If wheel <> -1 Then previous_wheel = wheel
  If line_number > 0 Or click = 2 Then Mouse_Management()
Loop


'***********************************************************************************************************************
' END OF MAIN LOOP.
'***********************************************************************************************************************
' SUB PROCEDURES BELOW ARE IN ALPHABETICAL ORDER.
'***********************************************************************************************************************


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
'*********************************************************************

Sub Frame()
  Dim As Integer y_top, y_bottom, x_right

  x_left  = 8 * left_start - margin - 10
  x_right = 8 * left_start + 8 * x_text + margin
  y_top = top_start * 16 - margin - 6
  y_bottom = y_top + 16 * y_text + margin - 4
  Line (x_left, y_top)-(x_right + 1, y_bottom + 1), gray, B
  Line (x_left + 1, y_top + 1)-(x_right, y_bottom), dark_gray, B
  Line (x_left + 1, y_bottom)-(x_right, y_bottom), white
  Line (x_left, y_bottom + 1)-(x_right + 1, y_bottom + 1), white
  Line (x_right, y_top + 1)-(x_right, y_bottom), white
  Line (x_right+ 1, y_top)-(x_right + 1, y_bottom + 1), white
End Sub

'*********************************************************************
' GRAPHICS ON WAVE ZONE.
'*********************************************************************

Sub Graphics()
  Dim As Integer maximum
  Line(0,x_width)-(0,y_height), background, bf                        'erase all.
  Line(-1,-1)-(x_width, y_height), black, b
  Line(0, y_center)-(x_width, y_center), black
  For x_pixel = x_emitter To x_width Step lambda
    Line(x_pixel,y_center-30)-(x_pixel,y_center), green               'axial distance to emitter, phase.
    Line(2*x_emitter-x_pixel,y_center-30)-(2*x_emitter-x_pixel,y_center), green'leftward.
  Next
  For y_pixel = lambda To y_center Step lambda
    Line(0,y_center+y_pixel)-(30, y_center+y_pixel), green            'off-axis distance (vertically), phase only.
    Line(0,y_center-y_pixel)-(30, y_center-y_pixel), green            'upward.
    Line(x_width-30,y_center+y_pixel)-(x_width, y_center+y_pixel), green'duplicate on the receiver side.
    Line(x_width-30,y_center-y_pixel)-(x_width, y_center-y_pixel), green
  Next
  For x_pixel = x_emitter + .5 * lambda To x_width Step lambda
    Line(x_pixel,y_center-20)-(x_pixel,y_center), red                 'axial distance to reflector, phase opposition.
    Line(2*x_emitter-x_pixel,y_center-20)-(2*x_emitter-x_pixel,y_center), red'leftward.
  Next
  For x_pixel = x_emitter + quarter_lambda To x_width Step half_lambda
    Line(x_pixel,y_center-10)-(x_pixel,y_center), gray                'axial distance to reflector, quadrature.
    Line(2*x_emitter-x_pixel+half_lambda,y_center-10)-(2*x_emitter-x_pixel+half_lambda,y_center), gray'leftward.
  Next
  
  If e_reflector <> "no reflector" Then
    For pixel = first_pixel_emitter To last_pixel_emitter
      Pset(xe_0(pixel), ye_0(pixel)), black                           'drawing the emitter reflector.
    Next
  End If
  If r_reflector <> "no reflector" Then
    For pixel = first_pixel_receiver To last_pixel_receiver
      Pset(xr_0(pixel), yr_0(pixel)), black                           'drawing the receiver reflector.
    Next
  End If
  
  If max_receiver > x_width - 30 Then maximum = x_width - 30 Else maximum = max_receiver
  If x_transverse_axis > x_center Then center = x_center Else center = x_transverse_axis
  Line(30, y_center)-(center, y_center+20), buff, bf                  'emitter zone.
  Line(center, y_center)-(maximum, y_center+20), blue_sky, bf         'receiver zone.
  Line(30, y_center)-(maximum+1, y_center+20), black, b
  Line(max_receiver, y_center)-(x_width, y_center), black
  Line(center, y_center)-(center, y_center+20), black
  Line(0,upper_emitter)-(30,upper_emitter+2), red, bf                 'reflector upper limit.
  Line(0,lower_emitter-2)-(30,lower_emitter), red, bf                 'lower limit.
  Line(x_width-30,upper_receiver)-(x_width,upper_receiver+2), red, bf 'receiver upper limit.
  Line(x_width-30,lower_receiver-2)-(x_width,lower_receiver), red, bf 'lower limit.
  Screenset work_page, visible_page
End Sub

'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Sub Initialization()
  Windowtitle "WaveMechanics05  -  Waves in 2 Dimensions."
  visible_page = 0
  work_page    = 1
  matrix_page  = 2
  x_width  = 770
  y_height = 536
  y_center = Int(y_height / 2)
  lambda = 36                                                         'lambda in pixels, compatible with c = 1 pixel.
  pulse = 220                                                         'pulse length in pixels whatever the wavelength.
  e_reflector = "ellipse"
  r_reflector = "ellipse"
  reflection = "hard"
'  reflection = "soft"
  display = "amplitude"
'  display = "energy"
'  display = "standing waves"
  curve = 0                                                           'optional superimposed amplitude curve.
  brightness = 1
  skipped_frames = 8                                                  'for faster results.
  upper_emitter = 0
  lower_emitter = y_height
  upper_receiver = 0
  lower_receiver = y_height
  x_emitter = 3 * lambda
  x_receiver = x_width - 3 * lambda
  For x = 0 To x_width: For y = 0 To y_height
    past(x,y) = 0                                                     'erasing previous data.
    present(x,y) = 0
    trend(x,y) = 0
  Next: Next
End Sub

'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

Sub Keyboard_Management()
If Len(in_key) = 2 Then in_key = Right(in_key, 1) + "+" Else in_key = Ucase(in_key)
Select Case in_key
  Case Chr(27), "k+": End                                             'escape key or Windows' X quit button.
  Case "A": If e_reflector <> "no reflector" Then e_reflector = "no reflector" Else in_key = ""
  Case "B": e_reflector = "straight"
            upper_emitter = y_center - lambda
            lower_emitter = y_center + lambda
            If reflection = "hard" Then x_emitter = quarter_lambda Else x_emitter = half_lambda
  Case "C": e_reflector = "parabola"
            upper_emitter = 0
            lower_emitter = y_height
            x_emitter = .25 * x_width
  Case "D": e_reflector = "ellipse"
            upper_emitter = 0
            lower_emitter = y_height
            x_emitter = .25 * x_width
            If r_reflector = "ellipse" Then
              x_receiver = x_width - x_emitter
              upper_receiver = 0
              lower_receiver = y_height
            End If
  Case "E": e_reflector = "corner"
            If reflection = "hard" Then
              x_emitter = .5 * lambda
              upper_emitter = y_center - lambda
              lower_emitter = y_center + lambda
            Else
              x_emitter = lambda
              upper_emitter = y_center - 1.5 * lambda
              lower_emitter = y_center + 1.5 * lambda
            End If
Case "F":   e_reflector = "three-sided"
            If reflection = "hard" Then
              x_emitter = quarter_lambda
              upper_emitter = y_center - lambda
              lower_emitter = y_center + lambda
            Else
              x_emitter = half_lambda
              upper_emitter = y_center - 1.5 * lambda
              lower_emitter = y_center + 1.5 * lambda
            End If
  Case "I": x_width = 1023: Initialization(): Update(): in_key = ""   'initialization
  Case "M": Run "WaveMechanics00.exe"                                 'main menu.
  Case "P": Screenset visible_page                                    'pause.
            Color red, background: 
            If y_height < 544 Then Locate 35, 3 Else Locate 48, 2
            ? " P - Paused. Press any key to resume. ";
            in_key = "": Sleep
            Screenset work_page, visible_page
  Case "R":                                                           'reset via update.
  Case "S": If r_reflector <> "no reflector" Then r_reflector = "no reflector" Else in_key = ""
  Case "T": r_reflector = "straight"
            upper_receiver = y_center - lambda
            lower_receiver = y_center + lambda
            If reflection = "hard" Then x_receiver = x_width - quarter_lambda Else x_receiver = x_width - half_lambda
  Case "U": r_reflector = "parabola"
            upper_receiver = 0
            lower_receiver = y_height
            x_receiver = .75 * x_width
  Case "V": r_reflector = "ellipse"
            upper_receiver = 0
            lower_receiver = y_height
            x_receiver = .75 * x_width
  Case "W": r_reflector = "corner"
            If reflection = "hard" Then
              x_receiver = x_width - .5 * lambda
              upper_receiver = y_center - lambda
              lower_receiver = y_center + lambda
            Else
              x_receiver = x_width - lambda
              upper_receiver = y_center - 1.5 * lambda
              lower_receiver = y_center + 1.5 * lambda
            End If
  Case "X": r_reflector = "three-sided"
            If reflection = "hard" Then
              x_receiver = x_width - quarter_lambda
              upper_receiver = y_center - lambda
              lower_receiver = y_center + lambda
            Else
              x_receiver = x_width - half_lambda
              upper_receiver = y_center - 1.5 * lambda
              lower_receiver = y_center + 1.5 * lambda
            End If
  Case "Y": r_reflector = "aperture"
            e_reflector  = "parabola"
            pulse = 600:  lambda = 32
            x_width = 1023: y_height = 399
            y_center = Int(y_height / 2)
            upper_emitter  = y_center - 199
            lower_emitter  = y_center + 199
            upper_receiver = y_center - 2 * lambda
            lower_receiver = y_center + 2 * lambda
            x_emitter = 300: x_receiver = 500
  Case "Z": e_reflector  = "straight": r_reflector = "two slits"
            x_width = 536: y_height = 536: reflection = "hard"
            lambda = 16: pulse = x_width: curve = 0: display = "energy"
            upper_emitter = 0:        upper_receiver = .5 * y_height - 4 * lambda
            lower_emitter = y_height: lower_receiver = .5 * y_height + 4 * lambda
            x_emitter = .25 * lambda: x_receiver = .25 * x_width
            skipped_frames = 4
  Case "1": e_reflector = "parabola": r_reflector = "parabola"        'exchanges between two parabolas.
            x_width = 500: y_height = 400: reflection = "hard"
            lambda = 24: pulse = 300: curve = 0: display = "amplitude"
            upper_emitter = 0:        upper_receiver = 0
            lower_emitter = y_height: lower_receiver = y_height
            x_emitter = .5 * x_width: x_receiver = .5 * x_width
            skipped_frames = 4
  Case "2": e_reflector = "no reflector": r_reflector = "ellipse"     'ellipse stunning properties.
            x_width =400: y_height = 400: reflection = "hard"
            lambda = 24: pulse = 250: curve = 0: display = "amplitude"
            upper_emitter = 0:        upper_receiver = 0
            lower_emitter = y_height: lower_receiver = y_height
            x_emitter = 4.5 * lambda: x_receiver = x_width - 4.5 * lambda
            skipped_frames = 2
  Case "3": e_reflector = "ellipse": r_reflector = "ellipse"          'circular reflector.
            x_width = 550: y_height = 550: reflection = "hard"
            lambda = 100: pulse = 400: curve = 1: display = "amplitude"
            upper_emitter = 0:        upper_receiver = 0
            lower_emitter = y_height: lower_receiver = y_height
            x_emitter = .5 * x_width: x_receiver = .5 * x_width
            skipped_frames = 4
  Case "4": e_reflector = "corner": r_reflector = "corner"            'corner reflector and soft reflection.
            x_width = 600: y_height = 400: reflection = "soft"
            lambda = 48: pulse = 500: curve = 0: display = "amplitude"
            upper_emitter = .5 * y_height - 2 * lambda: upper_receiver = .5 * y_height - 2 * lambda
            lower_emitter = .5 * y_height + 2 * lambda: lower_receiver = .5 * y_height + 2 * lambda
            x_emitter = lambda: x_receiver = x_width - .5 * lambda
            skipped_frames = 4
  Case "5": e_reflector = "ellipse": r_reflector = "ellipse"          'maximum elongated ellipses.
            x_width = 764: y_height = 200: reflection = "hard"
            lambda = 36: pulse = 500: curve = 0: display = "amplitude"
            upper_emitter = 0:        upper_receiver = .5 * y_height - 4 * lambda
            lower_emitter = y_height: lower_receiver = .5 * y_height + 4 * lambda
            x_emitter = .25 * lambda: x_receiver = x_width - .25 * lambda
            skipped_frames = 2
  Case "6": e_reflector = "parabola": r_reflector = "no reflector"    'emitter in Fresnel's diffraction pattern
            x_width = 470: y_height = 536: reflection = "hard"        'where radiation (hence reactance) is minimum.
            lambda = 16: pulse = x_width: curve = 0: display = "energy"
            upper_emitter = .5 * y_height - 5 * lambda 
            lower_emitter = .5 * y_height + 5 * lambda
            x_emitter = .5 * x_width: x_receiver = x_width
            skipped_frames = 8
  Case "7": e_reflector = "straight": r_reflector = "aperture"        'off-axis aperture.
            x_width = 536: y_height = 536: reflection = "hard"
            lambda = 16: pulse = x_width: curve = 0: display = "energy"
            upper_emitter = 0:        upper_receiver = .5 * y_height - 6 * lambda
            lower_emitter = y_height: lower_receiver = .5 * y_height - 1 * lambda
            x_emitter = .25 * lambda: x_receiver = .25 * x_width
            skipped_frames = 4
  Case "8": e_reflector = "no reflector": r_reflector = "straight"    'anti-noise screen.
            x_width = 536: y_height = 536: reflection = "soft"
            lambda = 16: pulse = .66 * x_width: curve = 0: display = "energy"
            upper_emitter = 0:        upper_receiver = .5 * y_height - 2 * lambda
            lower_emitter = y_height: lower_receiver = y_height
            x_emitter = .5 * lambda: x_receiver = .5 * x_width
            skipped_frames = 8
  Case "9": e_reflector = "parabola": r_reflector = "corner"          'plane waves reflected by corner reflector.
            x_width = 770: y_height = 350: reflection = "hard"
            lambda = 24: pulse = 300: curve = 0: display = "amplitude"
            upper_emitter = 0:        upper_receiver = 0
            lower_emitter = y_height: lower_receiver = y_height
            x_emitter = 8 * lambda: x_receiver = x_width - .25 * lambda
            skipped_frames = 8
  Case "0": e_reflector = "straight": r_reflector = "aperture"        'Fresnel's diffraction pattern.
            x_width = 1022: y_height = 538: reflection = "hard"
            lambda = 16: pulse = 1000: curve = 0: display = "energy"
            upper_emitter = 0:        upper_receiver = .5 * y_height - 4 * lambda
            lower_emitter = y_height: lower_receiver = .5 * y_height + 4 * lambda
            x_emitter = .25 * lambda: x_receiver = .5 * x_width
            skipped_frames = 8
  Case "+": brightness = 1.2 * brightness: in_key = "": If brightness > 5  Then brightness = 5
  Case "-": brightness = brightness / 1.2: in_key = "": If brightness < .2 Then brightness = .2
  Case "=": brightness =  1:               in_key = ""
  Case Else: in_key = ""                                              'avoid initialization.
End Select
If Len(in_key) Then Update()
Do: Loop While Len(Inkey)                                             'clear buffer.
End Sub

'*********************************************************************
' MOUSE MANAGEMENT.
'*********************************************************************

Sub Mouse_Management()
  Dim As Integer y_previous
  Color green_text, white
  If click = 2 Then                                                   'change resolution.
    Color, background
    Do
      Swap work_page, visible_page
      Screenset work_page, visible_page: Cls
      Line(0,0)-(x_mouse, y_mouse), black, b
      Line(0,0)-(x_width, y_height), green, b
      Circle(x_emitter, y_center), quarter_lambda, black
      Circle(x_receiver,y_center), quarter_lambda, black
      If e_reflector = "ellipse" Or r_reflector = "ellipse" Then
        Circle(x_transverse_axis,small_radius), x_transverse_axis, black,,, g_Lorentz
      End If
      Locate 2,2: ? "Width     "; x_mouse
      Locate 3,2: ? "Currently "; x_width
      Locate 5,2: ? "Height    "; y_mouse
      Locate 6,2: ? "Currently "; y_height
      Getmouse x_mouse, y_mouse, wheel, click
      If x_mouse < 400 Then x_mouse = 400 Else If x_mouse > 1000 Then x_mouse = 1022
      If y_mouse < 200 Then y_mouse = 200 Else If y_mouse > 745 Then y_mouse = 767
    Loop While click = 2
    If click > -1 Then 
      If e_reflector = "ellipse" And r_reflector = "ellipse" Then     'preserve proportions.
        x_emitter = x_emitter * x_mouse / x_width                     'previous width.
        x_receiver = x_mouse - x_emitter                              'symmetric.
        lower_emitter = y_mouse: lower_receiver = y_mouse             'maximum, to be corrected in sub Update.
        upper_emitter = 0: upper_receiver = 0
        pulse = pulse * x_mouse / x_width
      End If
      x_width = x_mouse: y_height = y_mouse
      y_previous = y_center
      y_center = Int(y_height / 2)
      emitter_height  = lower_emitter  - upper_emitter                'preserve reflectors relative position.
      receiver_height = lower_receiver - upper_receiver
      e_upper = y_previous - upper_emitter                            'using previous center.
      r_upper = y_previous - upper_receiver
      upper_emitter  = y_center - e_upper
      If upper_emitter < 0 Then upper_emitter = 0
      upper_receiver = y_center - r_upper
      If upper_receiver < 0 Then upper_receiver = 0
      lower_emitter  = upper_emitter  + emitter_height
      lower_receiver = upper_receiver + receiver_height
      skipped_frames = x_width * y_height / 79401 - 1                 'usually preferable, otherwise still adjustable.
      click = 0
      Update()
    End If
    Return
  End If
  
  Locate line_number
  Select Case line_number
  
    Case 16                                                           'select emitter axial distance.
      If click > 0 Then
        previous = x_mouse
        Screenset visible_page,visible_page: Color black, background
        If x_mouse < center Then
          previous_emitter = x_emitter
          Line(0, y_center)-(x_center, y_center + 20), buff, bf
          Line(x_center+1, y_center)-(max_receiver, y_center + 20), blue_sky, bf
          Line(0, y_center)-(max_receiver, y_center + 20), black,b
          Line(x_center+1, y_center)-(x_center+1, y_center + 20), black
          Do
            If x_mouse < quarter_lambda Then x_mouse = quarter_lambda
            If x_mouse > x_center Then x_mouse = x_center
            If previous <> x_mouse Then Line(previous, y_center + 1)-(previous, y_center + 19), buff
            Line(x_mouse, y_center + 1)-(x_mouse, y_center + 19), black
            x_emitter = x_mouse
            Locate 21, 6: ? "Selecting";
            ? Using " ##.###"; x_emitter / lambda;
            ? " lambda ("; x_emitter; " pixels). "
            previous = x_mouse
            Getmouse x_mouse, y_mouse, wheel, click
            If Abs(y_mouse - y_center) > 50 Then x_emitter = previous_emitter: Return
          Loop While click
          If e_reflector =  "ellipse" Then upper_emitter  = 0: lower_emitter  = y_height
          Update()
        Else                                                          'select receiver distance.
          previous_receiver = x_receiver
          Line(0, y_center)-(x_emitter-1, y_center + 20), buff, bf
          Line(x_emitter-1, y_center)-(max_receiver, y_center + 20), blue_sky, bf
          Line(0, y_center)-(max_receiver, y_center + 20), black,b
          Line(x_emitter-1, y_center)-(x_emitter-1, y_center + 20), black
          Do
            If x_mouse > max_receiver Then x_mouse = max_receiver
            If x_mouse < x_emitter Then x_mouse = x_emitter
            If previous <> x_mouse Then Line(previous, y_center + 1)-(previous, y_center + 19), blue_sky
            Line(x_mouse, y_center + 1)-(x_mouse, y_center + 19), black
            selecting = x_mouse
            Locate 21,60: ? "Selecting";
            ? Using " ##.### lambda from emitter"; (selecting - x_emitter) / lambda;
            Locate 22,60: ? "("; x_width - selecting; " pixels to right edge). "
            previous = x_mouse
            Getmouse x_mouse, y_mouse, wheel, click
            If Abs(y_mouse - y_center) > 50 Then x_receiver = previous_receiver: Return
          Loop While click
          x_receiver = selecting
          Update()
        End If
      End If
  
    Case 17                                                           'adjusting the reflector upper edge (emitter).
      If click > 0 Then
        previous = y_mouse
        Screenset visible_page,visible_page: Color black, background
        previous_upper = upper_emitter
        Line(0, 0)-(20, y_height - half_lambda), buff, bf
        Line(0, 0)-(20, y_height - half_lambda), black, b
        Do
          If y_mouse > y_height - half_lambda Then y_mouse = y_height - half_lambda
          If y_mouse < 0 Then y_mouse = 0
          If previous <> y_mouse Then Line(1, previous)-(19, previous), buff
          Line(0, y_mouse)-(20, y_mouse), black
          selecting = y_mouse
          Locate 28, 7: ? Using "Currently ##.### lambda"; Abs(upper_emitter - y_center) / lambda
          Locate 29, 7: ? Using "Selecting ##.### lambda"; Abs(selecting - y_center) / lambda
          previous = y_mouse
          Getmouse x_mouse, y_mouse, wheel, click
          If x_mouse > 30 Or x_mouse < 0 Then upper_emitter = previous_upper: Return
        Loop While click
        upper_emitter = selecting
        If upper_emitter > lower_emitter Then Swap upper_emitter, lower_emitter
        Update()
      End If
  
    Case 18                                                           'adjusting the reflector lower edge (emitter).
      If click > 0 Then
        previous = y_mouse
        Screenset visible_page,visible_page: Color black, background
        previous_lower = lower_emitter
        Line(0, half_lambda)-(20, y_height), buff, bf
        Line(0, half_lambda)-(20, y_height), black, b
        Do
          If y_mouse > y_height Then y_mouse = y_height
          If y_mouse < half_lambda Then y_mouse = half_lambda
          If previous <> y_mouse Then Line(1, previous)-(19, previous), buff
          Line(0, y_mouse)-(20, y_mouse), black
          selecting = y_mouse
          Locate 28, 7: ? Using "Currently ##.### lambda"; Abs(lower_emitter - y_center) / lambda
          Locate 29, 7: ? "Selecting";
          ? Using " ##.### lambda"; Abs(selecting - y_center) / lambda
          previous = y_mouse
          Getmouse x_mouse, y_mouse, wheel, click
          If x_mouse > 30 Or x_mouse < 0 Then lower_emitter = previous_lower: Return
        Loop While click
        lower_emitter = selecting
        If upper_emitter > lower_emitter Then Swap upper_emitter, lower_emitter
        Update()
      End If
  
    Case 19                                                           'adjusting the reflector upper edge (receiver).
      If click > 0 Then
        previous = y_mouse
        Screenset visible_page,visible_page: Color black, background
        previous_upper = upper_receiver
        Line(x_width-20, 0)-(x_width, y_height - half_lambda), blue_sky, bf
        Line(x_width-20, 0)-(x_width, y_height - half_lambda), black, b
        Do
          If y_mouse > y_height - half_lambda Then y_mouse = y_height - half_lambda
          If y_mouse < 0 Then y_mouse = 0
          If previous <> y_mouse Then Line(x_width-19, previous)-(x_width-1, previous), blue_sky
          Line(x_width-20, y_mouse)-(x_width, y_mouse), black
          selecting = y_mouse
          Locate 27, 101: ? Using "Currently ##.### lambda"; Abs(upper_receiver - y_center) / lambda
          Locate 28, 101: ? Using "Selecting ##.### lambda"; Abs(selecting - y_center) / lambda
          previous = y_mouse
          Getmouse x_mouse, y_mouse, wheel, click
          If x_mouse < x_width-30 Or x_mouse > x_width  Then upper_emitter = previous_upper: Return
        Loop While click
        upper_receiver = selecting
        If upper_receiver > lower_receiver Then Swap upper_receiver, lower_receiver
        Update()
      End If
  
    Case 20                                                           'adjusting the reflector lower edge (receiver).
      If click > 0 Then
        previous = y_mouse
        Screenset visible_page,visible_page: Color black, background
        previous_lower = lower_receiver
        Line(x_width-20, half_lambda)-(x_width, y_height), blue_sky, bf
        Line(x_width-20, half_lambda)-(x_width, y_height), black, b
        Do
          If y_mouse > y_height Then y_mouse = y_height
          If y_mouse < half_lambda Then y_mouse = half_lambda
          If previous <> y_mouse Then Line(x_width-19, previous)-(x_width-1, previous), blue_sky
          Line(x_width-20, y_mouse)-(x_width, y_mouse), black
          selecting = y_mouse
          Locate 27, 101: ? Using "Currently ##.### lambda"; Abs(lower_receiver - y_center) / lambda
          Locate 28, 101: ? Using "Selecting ##.### lambda"; Abs(selecting - y_center) / lambda
          previous = y_mouse
          Getmouse x_mouse, y_mouse, wheel, click
          If x_mouse < x_width-30 Or x_mouse > x_width Then lower_receiver = previous_lower: Return
        Loop While click
        lower_receiver = selecting
        If upper_receiver > lower_receiver Then Swap upper_receiver, lower_receiver
        Update()
      End If
  
    Case 38
      Select Case x_mouse                                             'no reflector.
        Case Is < 257
          If e_reflector <> "no reflector" Then
            Locate,3: ? line38a
            If click > 0 Then e_reflector = "no reflector": Update()
          End If
        Case Is < 512: Locate,35: ? line38b;                          'select wavelength.
          Locate,46: ? " Click to select."
          If click > 0 Then
            Screenset visible_page
            Line(272, 575)-(496, 624), black, b
            Line(273, 576)-(495, 623), white, bf
            Locate 39, 39: ? "16              100"
            Line(330, 592)-(420, 607), black, b
            Do
              Getmouse x_mouse, y_mouse, wheel, click
              variable = (x_mouse - 320) / 4
              If variable < 4 Then variable = 4 Else If variable > 25 Then variable = 25
              Line(331, 593)-(419, 606), buff, bf
              Line(320 + variable * 4, 593)-(320 + variable * 4, 606), black
              Locate 37, 36: ? "Wavelength "; variable * 4; " pixels. "
              Locate 39, 36: ? "Currently "; lambda
              If Abs(y_mouse - 600) > 26 Or Abs(x_mouse - 385) > 112 Then Return'rectangle limits.
            Loop While click
            lambda = variable * 4: reference = 0: Update()
          End If
        Case Is < 768
          If display <> "amplitude" Then
            Locate,67: ? line38c
            If click > 0 Then
              display = "amplitude"
              Screenset matrix_page
              Color green_text, background
              Locate 39,67: ? line39c
              Locate 40,67: ? line40c
              Color blue
              Locate 38,67: ? line38c
            End If
          End If
        Case Else
          If r_reflector <> "no reflector" Then
            Locate,99: ? line38d
            If click > 0 Then r_reflector = "no reflector": Update()
          End If
      End Select
  
    Case 39
      Select Case x_mouse
        Case Is < 257                                                 'emitter straight reflector.
          If e_reflector <> "straight" Then
            Locate,3: ? line39a
            If click > 0 Then
              e_reflector = "straight"
              upper_emitter = y_center - lambda
              lower_emitter = y_center + lambda
              If reflection = "hard" Then x_emitter = quarter_lambda Else x_emitter = half_lambda
              Update()
            End If
          End If
        Case Is < 512: Locate,35: ? line39b;                          'select pulse length in pixels.
          Locate,42: ? " Click to select."
          If click > 0 Then
            Screenset visible_page
            Line(272, 591)-(496, 640), black, b
            Line(273, 592)-(495, 639), white, bf
            Locate 39, 36: ? "50                     200"
            Line(310, 608)-(448, 623), black, b
            Do
              Getmouse x_mouse, y_mouse, wheel, click
              variable = (x_mouse - 306) / 1
              If variable < 5 Then variable = 5 Else If variable > 140 Then variable = 140
              Line(311, 609)-(447, 622), buff, bf
              Line(306 + variable, 609)-(306 + variable, 622), black
              Locate 38, 36: ? "Pulse length "; variable * 10; " pixels. "
              Locate 40, 36: ? "Currently "; pulse
              If Abs(y_mouse - 601) > 27 Or Abs(x_mouse - 385) > 112 Then Return'rectangle limits.
            Loop While click
            pulse = variable * 10: Update()
          End If
        Case Is < 768
          If display <> "energy" Then
            Locate,67: ? line39c
            If click > 0 Then
              display = "energy"
              Screenset matrix_page
              Color green_text, background
              Locate 38,67: ? line38c
              Locate 40,67: ? line40c
              Color blue
              Locate 39,67: ? line39c
            End If
          End If
        Case Else                                                     'receiver straight reflector.
          Locate,99: ? line39d
          If click > 0 Then
            r_reflector = "straight"
            upper_receiver = y_center - lambda
            lower_receiver = y_center + lambda
            If reflection = "hard" Then x_receiver = x_width - quarter_lambda Else x_receiver = x_width - half_lambda
            Update()
          End If
      End Select
  
    Case 40
      Select Case x_mouse
        Case Is < 257                                                 'emitter parabolic reflector.
          Locate,3: ? line40a
          If click > 0 Then
            e_reflector = "parabola"
            upper_emitter = 0
            lower_emitter = y_height
            x_emitter = .25 * x_width
            Update()
          End If
        Case Is < 512                                                 'hard reflection.
          If reflection <> "hard" Then
            Locate,35: ? line40b
            If click > 0 Then reflection = "hard": Update()
          End If
        Case Is < 768
          If display <> "standing waves" Then
            Locate,67: ? line40c
            If click > 0 Then
              display = "standing waves"
              Screenset matrix_page
              Color green_text, background
              Locate 38,67: ? line38c
              Locate 39,67: ? line39c
              Color blue
              Locate 40,67: ? line40c
            End If
          End If
        Case Else                                                     'receiver parabolic reflector.
          Locate,99: ? line40d
          If click > 0 Then
            r_reflector = "parabola"
            upper_receiver = 0
            lower_receiver = y_height
            Update()
          End If
      End Select
  
    Case 41
      Select Case x_mouse
        Case Is < 257                                                 'emitter elliptic reflector.
          Locate, 3: ? line41a
          If click > 0 Then
            e_reflector = "ellipse"
            upper_emitter = 0
            lower_emitter = y_height
            x_emitter = .25 * x_width
            If r_reflector = "ellipse" Then
              x_receiver = x_width - x_emitter
              upper_receiver = 0
              lower_receiver = y_height
            End If
            Update()
          End If
        Case Is < 512                                                 'soft reflection.
          If reflection <> "soft" Then
            Locate,35: ? line41b
            If click > 0 Then reflection = "soft": Update()
          End If
        Case Is < 768
          If Timer - t < 2 Then Return 
          Locate,67                                                   'show or hide amplitude curve.
          If curve = 1 Then
            ? " Hide axial amplitude curve."
          Else
            ? " Show axial amplitude curve."
          End If
          If click > 0 Then                                           'this option needs no interruption.
            Screenset matrix_page: Color green_text,background
            Locate 41, 67: t = Timer                                  'timer to avoid two consecutive actions.
            If curve = 1 Then
              curve = 0
              ? " Show axial amplitude curve."
            Else
              curve = 1
              ? " Hide axial amplitude curve."
            End If
          End If
        Case Else                                                     'receiver elliptic reflector.
          Locate,99: ? line41d
          If click > 0 Then
            r_reflector = "ellipse"
            upper_receiver = 0
            lower_receiver = y_height
            Update()
          End If
          If r_reflector <> "ellipse" Then
            If click > 0 Then r_reflector = "ellipse": Update()
          End If
      End Select
  
    Case 42
      Select Case x_mouse
        Case Is < 257                                                 'emitter corner reflector.
          Locate,3: ? line42a
          If click > 0 Then
            e_reflector = "corner"
            If reflection = "hard" Then
              x_emitter = .5 * lambda
              upper_emitter = y_center - lambda
              lower_emitter = y_center + lambda
            Else
              x_emitter = lambda
              upper_emitter = y_center - 1.5 * lambda
              lower_emitter = y_center + 1.5 * lambda
            End If
            Update()
          End If
        Case Is < 512                                                 'check emitter gain.
          If emitter_gain = 0 Then
            Locate, 35
            If e_reflector = "no reflector" Then
              ? " Please select a reflector. "
            Else
              ? line42b
              If click > 0 Then
                previous_reflector = e_reflector
                e_reflector = "no reflector"
                r_reflector = "no reflector"
                iteration = 0
                reference = 0
                Update()
                emitter_gain = 1                                      'follow-up in sub Wave_Display()
              End If
            End If
          End If
        Case Is < 768                                                 'instruction for screen resolution.
          Locate, 67: ? " Right click to select.     "
        Case Else                                                     'receiver corner reflector.
          Locate,99: ? line42d
          If click > 0 Then
            r_reflector = "corner"
            If reflection = "hard" Then
              x_receiver = x_width - .5 * lambda
              upper_receiver = y_center - lambda
              lower_receiver = y_center + lambda
            Else
              x_receiver = x_width - lambda
              upper_receiver = y_center - 1.5 * lambda
              lower_receiver = y_center + 1.5 * lambda
            End If
            Update()
          End If
      End Select
  
    Case 43
      Select Case x_mouse
        Case Is < 257                                                 'emitter three-sided reflector.
          Locate,3: ? line43a
          If click > 0 Then
            e_reflector = "three-sided"
            If reflection = "hard" Then
              x_emitter = quarter_lambda
              upper_emitter = y_center - lambda
              lower_emitter = y_center + lambda
            Else
              x_emitter = half_lambda
              upper_emitter = y_center - 1.5 * lambda
              lower_emitter = y_center + 1.5 * lambda
            End If
            Update()
          End If
        Case Is < 512                                                 'reverse wave direction.
          Locate, 35: ? line43b
          If click > 0 Then
            For x = 1 To x_width - 1: For y = 1 To y_height - 1
              Swap present(x,y), trend(x,y)
            Next: Next
            Screenset work_page, work_page
            Color blue: Locate 43, 35: ? line43b
            If iteration < pulse - lambda Then                        'stop emitting.
              Do
                iteration += lambda
              Loop While iteration < pulse - lambda                   'allow generator to stop emitting when phase = 0.
            End If
            Do                                                        'avoid repetitive actions.
              Getmouse x_mouse, y_mouse, wheel, click
            Loop While click > 0
          End If
        Case Is < 768
        Case Else                                                     'receiver three-sided reflector.
          Locate,99: ? line43d
          If click > 0 Then
            r_reflector = "three-sided"
            If reflection = "hard" Then
              x_receiver = x_width - quarter_lambda
              upper_receiver = y_center - lambda
              lower_receiver = y_center + lambda
            Else
              x_receiver = x_width - half_lambda
              upper_receiver = y_center - 1.5 * lambda
              lower_receiver = y_center + 1.5 * lambda
            End If
            Update()
          End If
      End Select
  
    Case 44
      Select Case x_mouse
        Case Is < 257
        Case Is < 512                                                 'force standing waves.
          Locate, 35: ? line44b
          If click > 0 Then
            For x = 0 To x_width: For y = 0 To y_height               'trend(x,y) = present(x,y) works, but hardly.
               trend(x,y) = .5 * (past(x,y) + present(x,y))
               present(x,y) = trend(x,y)
            Next: Next
            Screenset work_page, work_page
            Color blue: Locate 44, 35: ? line44b
            If iteration < pulse - lambda Then                        'stop emitting.
              Do
                iteration += lambda
              Loop While iteration < pulse - lambda                   'allow generator to stop emitting when phase = 0.
            End If
            Do                                                        'avoid repetitive actions.
              Getmouse x_mouse, y_mouse, wheel, click
            Loop While click > 0
          End If
        Case Is < 768
        Case Else                                                     'aperture instead of receiver.
          If r_reflector <> "aperture" Then
            Locate,99: ? line44d
            If click > 0 Then r_reflector = "aperture": Update()
          End If
      End Select
  
    Case 45
      previous = skipped_frames
      If click > 0 And x_mouse > 663 And x_mouse < 748 Then           'updating for skipped frames only.
        Screenset matrix_page
        Color green_text, background
        Locate, 67: ? line45c
        Color blue
      End If
      Select Case x_mouse
        Case Is < 257
        Case Is < 512                                                 'stop or restart emitting.
          Locate, 35: ? line45b
          If click > 0 Then
            If iteration > pulse Then
              iteration = 0                                           'restart.
            Elseif iteration >= pulse - lambda Then                   'generator will soon stop anyway when phase = 0.
            Elseif iteration < pulse - lambda Then                    'stop emitting.
              Do
                iteration += lambda
              Loop While iteration < pulse - lambda                   'allow generator to stop emitting when phase = 0.
            End If
            Do                                                        'avoid repetitive actions.
              Getmouse x_mouse, y_mouse, wheel, click
            Loop While click > 0
          End If
        Case Is < 664: Locate, 67: ? " Select a number "              'skipped frames below.
        Case Is < 684: If skipped_frames <> 0 Then    Locate 45, 84: ? " 0 "
                       If click > 0 Then skipped_frames = 0: Locate 45, 84: ? " 0 "
        Case Is < 700: If skipped_frames <> 1 Then    Locate 45, 86: ? " 1 "
                       If click > 0 Then skipped_frames = 1: Locate 45, 86: ? " 1 "
        Case Is < 716: If skipped_frames <> 2 Then    Locate 45, 88: ? " 2 "
                       If click > 0 Then skipped_frames = 2: Locate 45, 88: ? " 2 "
        Case Is < 732: If skipped_frames <> 4 Then    Locate 45, 90: ? " 4 "
                       If click > 0 Then skipped_frames = 4: Locate 45, 90: ? " 4 "
        Case Is < 748: If skipped_frames <> 8 Then    Locate 45, 92: ? " 8 "
                       If click > 0 Then skipped_frames = 8: Locate 45, 92: ? " 8 "
  
        Case Else                                                     'two slits screen instead of reflector.
          If r_reflector <> "two slits" Then
            Locate,99: ? line45d
            If click > 0 Then
              e_reflector  = "straight": r_reflector = "two slits"
              x_width = 536: y_height = 536: reflection = "hard"
              lambda = 16: pulse = x_width: curve = 0: display = "energy"
              upper_emitter = 0:        upper_receiver = .5 * y_height - 4 * lambda
              lower_emitter = y_height: lower_receiver = .5 * y_height + 4 * lambda
              x_emitter = .25 * lambda: x_receiver = .25 * x_width
              skipped_frames = 4
              Update()
            End If
          End If
      End Select
      
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
                         Initialization()
                         Update()
                       End If
      End Select
    
    Case 48  
      Select Case x_mouse
        Case Is > 700
        Case Is > 576: Locate 48, 73: ? line48c;
                       If click > 0 Then Run "WaveMechanics06.exe"    'next program.
        Case Is > 472: Locate 48, 60: ? line48b;
                       If click > 0 Then End                          'quit.
        Case Is > 318: Locate 48, 41: ? line48a;
                       If click > 0 Then Run "WaveMechanics04.exe"    'previous program.
      End Select
  End Select
  Color black, background
End Sub

'*********************************************************************
' REFLECTION MANAGEMENT.
'*********************************************************************

Sub Reflection_Management()

' **************** ANTIREFLECTIVE EDGES.

  For x = 0 To x_width                                                'NULL REFLECTION on condition that the incidence
    If x - x_emitter Then                                             'angle is known. The middle point for equal trend
      angle = Atn(y_center / ((x - x_emitter) / 1.732))               'and present values appears to be about 30°, not
    Else angle = pi / 2                                               '45°. Thus, the angle must be "compressed"
    End If                                                            'according to a 1.732 factor or Sqr(3) approx.
    trend(x,0)=present(x,1)*Sin(angle)^2 + trend(x,1)*Cos(angle)^2    'top, adjusting trend and present ratio to angle.
    trend(x,y_height)=present(x,y_height-1)*Sin(angle)^2 + trend(x,y_height-1)*Cos(angle)^2'bottom.
  Next
  
  For y = 0 To y_height
    If y - y_center Then
      angle = Atn(x_emitter / ((y - y_center) / 1.732))
    Else angle = pi / 2
    End If
    trend(0,y)=present(1,y)*Sin(angle)^2 + trend(1,y)*Cos(angle)^2    'left side.
    present(x_width+1, y) = present(x_width, y)                       'antireflective screen for 90° incidence (right).
  Next
  
  If r_reflector = "aperture" Then
    For y_pixel = 0 To y_height                                       'here, upper_receiver and lower_receiver stand
      If y_pixel < upper_receiver Or y_pixel > lower_receiver Then    '                         ...for aperture limits.
        trend(x_receiver, y_pixel) = present(x_receiver-1, y_pixel)   'antireflective screen outside aperture.
        trend(x,y) = present(x+1,y)
        trend(x_receiver+1, y_pixel) = 0
      End If
    Next
  Elseif r_reflector = "two slits" Then
    For y_pixel = 0 To y_height
      If Abs(y_pixel - upper_receiver) > quarter_lambda And Abs(y_pixel - lower_receiver) > quarter_lambda  Then
        trend(x_receiver, y_pixel) = present(x_receiver-1, y_pixel)   'antireflective screen outside slits.
        trend(x,y) = present(x+1,y)
        trend(x_receiver+1, y_pixel) = 0
      End If
    Next
  End If

' **************** REFLECTORS.

' Radio waves and light undergo a hard reflection on reflectors or
' mirrors, and so hard reflection is the rule in optics and
' radioelectricity. However, I was aware that acousticians would
' prefer soft reflection as it applies (but not necessarily) to
' sound. So I had to make this option available too.

' Hard reflection produces a pi phase shift well visible because
' the first reflected half-wave turns from red to green. It is quite
' easy to obtain by canceling trend. See below: trend(x, y) = 0.

' However, it is not that simple for soft reflection because one
' must take the angle into account. For 0 or 90° reflectors, it can
' be performed rather easily by duplicating forward and canceling
' backward trend: trend(x, y) = trend(x+1, y); trend(x-1, y) = 0.
' Unfortunately, other angles need a 2-layer duplication and
' a 2-layer cancellation, so that even the 0° or 90° sections need
' also a 2-layer procedure in order to preserve symmetry. And
' finally, the correct trend mixture must be balanced carefully
' according to the angle. In the future, an even more sophisticated
' depixellation method similar to bicubic for images will surely
' prevail even for hard reflection, but this is beyond my goal.

  If e_reflector <> "no reflector" Then                               'reflector on the EMITTER side.
    If reflection = "hard" Then
      For pixel = first_pixel_emitter To last_pixel_emitter
        trend(xe_0(pixel), ye_0(pixel)) = 0
      Next
    Else
      For pixel = first_pixel_emitter To last_pixel_emitter
        trend(xe_1(pixel),ye_1(pixel))=e_mix(pixel)*trend(xe_1a(pixel),ye_1a(pixel))+(1-e_mix(pixel))*trend(xe_1b(pixel),ye_1b(pixel))
        trend(xe_0(pixel),ye_0(pixel))=e_mix(pixel)*trend(xe_0a(pixel),ye_0a(pixel))+(1-e_mix(pixel))*trend(xe_0b(pixel),ye_0b(pixel))
        trend(xe_2(pixel),ye_2(pixel))=0
        trend(xe_3(pixel),ye_3(pixel))=0
      Next
    End If
  End If

  If r_reflector = "no reflector" Or r_reflector = "aperture" Or r_reflector = "two slits" Then skip = 1 Else skip = 0
  
  If skip = 0 Then                                                    'reflector on the RECEIVER side.
    If reflection = "hard" Then
      For pixel = first_pixel_receiver To last_pixel_receiver
        trend(xr_0(pixel), yr_0(pixel)) = 0
      Next
    Else
      For pixel = first_pixel_receiver To last_pixel_receiver
        trend(xr_1(pixel),yr_1(pixel))=r_mix(pixel)*trend(xr_1a(pixel),yr_1a(pixel))+(1-r_mix(pixel))*trend(xr_1b(pixel),yr_1b(pixel))
        trend(xr_0(pixel),yr_0(pixel))=r_mix(pixel)*trend(xr_0a(pixel),yr_0a(pixel))+(1-r_mix(pixel))*trend(xr_0b(pixel),yr_0b(pixel))
        trend(xr_2(pixel),yr_2(pixel))=0
        trend(xr_3(pixel),yr_3(pixel))=0
      Next
    End If
  End If
  Circle(x_emitter, y_center), .375 * lambda, gray                    'emitter position.
  Circle(x_receiver,y_center), .375 * lambda, gray                    'receiver position.
End Sub

'*********************************************************************
' TEXT.
'*********************************************************************

Sub Text()
  line38a  = " A - No reflector.          "
  line39a  = " B - Straight reflector.    "
  line40a  = " C - Parabolic reflector.   "
  line41a  = " D - Elliptic reflector.    "
  line42a  = " E - Corner reflector.      "
  line43a  = " F - Three-sided reflector. "
  line44a  = "                            "
  line45a  = "                            "
  
  line38b  = " Wavelength                 "
  line39b  = " Pulse length               "
  line40b  = " Hard reflection.           "
  line41b  = " Soft reflection.           "
  line42b  = " Check emitter gain.        "
  line43b  = " Reverse wave direction.    "
  line44b  = " Force standing waves.      "
  line45b  = " Stop/Restart emitting.     "
  
  line38c  = " Show amplitude.            "
  line39c  = " Show energy.               "
  line40c  = " Show standing waves.       "
  line41c  = " Show axial amplitude curve."
  line42c  = " Resolution      x"
  line43c  = "                            "
  line44c  = " Brightness: press  + - =   "
  line45c  = " Skipped frames   0 1 2 4 8 "
  
  line38d  = " S - No reflector.          "
  line39d  = " T - Straight reflector.    "
  line40d  = " U - Parabolic reflector.   "
  line41d  = " V - Elliptic reflector.    "
  line42d  = " W - Corner reflector.      "
  line43d  = " X - Three-sided reflector. "
  line44d  = " Y - Linear aperture.       "
  line45d  = " Z - Two slits experiment.  "
  
  line47a = " I - Initialize.   "
  line47b = " M - Menu.   "
  line47c = " Slow.         "
  line48a = " Previous Program. "
  line48b = " Quit (Esc.)."
  line48c = " Next Program. "
  
  Color green_text
  Locate 38,3: ? line38a;: Locate,35: ? line38b;: Locate,67: ? line38c;: Locate,99: ? line38d
  Locate 39,3: ? line39a;: Locate,35: ? line39b;: Locate,67: ? line39c;: Locate,99: ? line39d
  Locate 40,3: ? line40a;: Locate,35: ? line40b;: Locate,67: ? line40c;: Locate,99: ? line40d
  Locate 41,3: ? line41a;: Locate,35: ? line41b;: Locate,67: ? line41c;: Locate,99: ? line41d
  Locate 42,3: ? line42a;: Locate,35: ? line42b;: Locate,67: ? line42c;: Locate,99: ? line42d
  Locate 43,3: ? line43a;: Locate,35: ? line43b;: Locate,67: ? line43c;: Locate,99: ? line43d
  Locate 44,3: ? line44a;: Locate,35: ? line44b;: Locate,67: ? line44c;: Locate,99: ? line44d
  Locate 45,3: ? line45a;: Locate,35: ? line45b;: Locate,67: ? line45c;: Locate,99: ? line45d
  Locate 38,47: ? lambda; " pixels."
  Locate 39,49: ? pulse;  " pixels."
  Locate 42,79: ? Using "####"; x_width
  Locate 42,85: ? y_height
  Color blue
  If curve = 1 Then Locate 41,67: ? " Hide axial amplitude curve."
  Select Case e_reflector
    Case "no reflector": Locate 38, 3: ? line38a
    Case "straight":     Locate 39, 3: ? line39a
    Case "parabola":     Locate 40, 3: ? line40a
    Case "ellipse":      Locate 41, 3: ? line41a
    Case "corner":       Locate 42, 3: ? line42a
    Case "three-sided":  Locate 43, 3: ? line43a
  End Select
  Select Case r_reflector
    Case "no reflector": Locate 38,99: ? line38d
    Case "straight":     Locate 39,99: ? line39d
    Case "parabola":     Locate 40,99: ? line40d
    Case "ellipse":      Locate 41,99: ? line41d
    Case "corner":       Locate 42,99: ? line42d
    Case "three-sided":  Locate 43,99: ? line43d
    Case "aperture":     Locate 44,99: ? line44d
    Case "two slits":    Locate 45,99: ? line45d
  End Select
  Select Case skipped_frames
    Case Is < 1: skipped_frames = 0: Locate 45,85: ? "0"
    Case Is < 2: skipped_frames = 1: Locate 45,87: ? "1"
    Case Is < 4: skipped_frames = 2: Locate 45,89: ? "2"
    Case Is < 8: skipped_frames = 4: Locate 45,91: ? "4"
    Case Else  : skipped_frames = 8: Locate 45,93: ? "8"
  End Select
  Select Case reflection
    Case "hard": Locate 40,35: ? line40b
    Case "soft": Locate 41,35: ? line41b
  End Select
  Select Case display
    Case "amplitude":      Locate 38,67: ? line38c
    Case "energy":         Locate 39,67: ? line39c
    Case "standing waves": Locate 40,67: ? line40c
  End Select
  
  Color black
  x_text = 23                                                         'text width (pixels = x * 8).
  y_text = 8                                                          'number of lines.
  top_start = 38
  left_start = 5                                                      'limit on the left hand side.
  Frame()
  Locate top_start - 1
  Locate,left_start + 1: ? "  EMITTER  "
  
  left_start= 37
  Frame()
  Locate top_start - 1
  Locate,left_start + 1: ? "  OPTIONS  "
  
  left_start= 69
  Frame()
  Locate top_start - 1
  Locate,left_start + 1: ? "  DISPLAY  "
  
  left_start= 101
  Frame()
  Locate top_start - 1
  Locate,left_start + 1: ? "  RECEIVER  "
  
  Locate 19, 6: ? "Click the yellow area to move emitter."
  Locate 20, 6: ? "Currently";
  ? Using " ##.###"; x_emitter / lambda;
  ? " lambda to reflector ("; x_emitter; " pixels)."
  Locate 19,60: ? "Click the blue area to move receiver."
  Locate 20,60: ? "Currently";
  ? Using " ##.###"; (x_receiver - x_emitter) / lambda;: ? " lambda from emitter"
  Locate 21,60: ? "("; x_width - x_receiver; " pixels to right edge). "
  x_text = 23                                                         'text width (x * 8 pixels).
  y_text = 3                                                          'number of lines.
  left_start= 5                                                       'limit on the left hand side (x * 8 pixels).
  top_start = 31                                                      'line number for upper limit
  Frame()
  Locate top_start
  Locate,left_start:  ? "  Click on the left to"
  Locate,left_start:  ? "   adjust the emitter"
  Locate,left_start:  ? "    reflector edges."
  left_start= 37: Frame()
  Locate top_start
  Locate,left_start:  ? " Use the mouse wheel to"
  Locate,left_start:  ? "  adjust all reflector"
  Locate,left_start:  ? "    edges equally."
  left_start= 69: Frame()
  Locate top_start
  Locate,left_start-1:? "Move the emitter to adjust"
  Locate,left_start-1:? "   the two reflector"
  Locate,left_start-1:? "   distances equally."
  left_start= 101
  top_start = 2: Frame()
  Locate top_start
  Locate,left_start:  ? "Move the mouse pointer off"
  Locate,left_start:  ? "of the wave area to allow "
  Locate,left_start:  ? "  waves to be displayed."
  top_start = 8: Frame()
  Locate top_start
  Locate,left_start:  ? " Right click and drag"
  Locate,left_start:  ? " to enlarge or reduce"
  Locate,left_start:  ? "    the wave area."
  top_start = 14: Frame()
  Locate top_start
  Locate,left_start:  ? " You may have to click"
  Locate,left_start:  ? " slowly, depending on"
  Locate,left_start:  ? " wave area resolution."
  top_start = 26: Frame()
  Locate top_start
  Locate,left_start-1:? "Press a number from 0 to 9"
  Locate,left_start-1:? "to observe more interesting"
  Locate,left_start-1:? " reflector configurations."
  top_start = 31: Frame()
  Locate top_start
  Locate,left_start:  ? "Click on the right of the"
  Locate,left_start:  ? "wave area to adjust the"
  Locate,left_start:  ? "receiver reflector edges."

  Locate 35,4:   ? "P - Pause."
  Locate 35,118: ? "R - Reset."

  Color green_text
  Locate 47,42: ? "I - Initialize.    M - Menu.    Slow."
  Locate 48,42: ? "Previous Program.  Quit (Esc.). Next Program.";
  
  Color dark_gray
  Locate 47, 3: ? "Thanks to the creators of FreeBASIC."
  Locate 48, 3: ? "Gabriel LaFreniere  glafreniere.com";
  Locate 47,89: ? "September 21, 2008. This program may be"
  Locate 48,89: ? "freely distributed, copied or modified.";
  Color black
End Sub

'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Sub Title()
  Dim As String display_title
  Dim As Integer y_title, x1, x2, y1, y2, dark
  display_title = "Waves in Two Dimensions"
  Locate 1,1: ? display_title
  center = 1024 / 2
  y_title = 538
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

'*********************************************************************
' DATA UPDATE.
'*********************************************************************

Sub Update()
  Dim As Integer x_reflector
  k_Dewavrin = Sin(2 * pi / lambda)                                   'Dewavrin's constant for each angular step.
  half_lambda  = lambda / 2
  quarter_lambda = lambda / 4                                         'hence lambda is a multiple of 4.
  pulses = pulse / lambda                                             'conversion to pulses in 2 * pi units (c = 1).
  pulse = pulses * lambda                                             'integer multiple of lambda.
  If x_width < 400 Then x_width = 400
  If y_height < 200 Then y_height = 200
  If x_width Mod 2 > 0 Then
    If e_reflector = "ellipse" And r_reflector = "ellipse" Then x_width -= 1 'symmetry is occasionally advisable.
  End If
  x_center = Int(x_width / 2)
  y_center = Int(y_height / 2)
  If x_emitter > x_center Then x_emitter = x_center                   'avoid emitter and receiver invalid positions.
  If reflection = "soft" Then
    If x_emitter < half_lambda Then x_emitter = half_lambda
    max_receiver = x_width - half_lambda
  Else
    If x_emitter < quarter_lambda Then x_emitter = quarter_lambda
    max_receiver = x_width - quarter_lambda
  End If
  If x_receiver > max_receiver Then x_receiver = max_receiver
  If x_receiver < x_emitter Then x_receiver = x_emitter
  'If (x_receiver + x_emitter) Mod 2 Then x_receiver -= 1             'the transverse central axis MUST be in the center.
  If Abs(lower_emitter - upper_emitter) < quarter_lambda Then upper_emitter = lower_emitter - quarter_lambda
  If lower_emitter >  y_height Then lower_emitter  = y_height
  If lower_receiver > y_height Then lower_receiver = y_height
  If pulses < 3 Then pulses = 3: pulse = pulses * lambda              'lower limit. - fade in and out to consider.
  horizontally = x_emitter + quarter_lambda                           'skip unused area for faster results.
  vertically = y_center + quarter_lambda
  x_transverse_axis = Int((x_receiver + x_emitter) / 2)               'x coordinate for central transverse y axis.
  If x_transverse_axis > x_center Then center = x_center Else center = x_transverse_axis
  iteration = 0
  gain = 0
  emitter_gain = 0
  previous = y_center
  t = Timer
  Screenset matrix_page, matrix_page
  Color black, background: Cls
  
  Title()
  Text()
  
  For x = -1 To x_width+1: For y = -1 To y_height+1                   'erasing previous data.
    past(x,y)    = 0
    present(x,y) = 0
    trend(x,y)   = 0
  Next: Next
  
  Select Case e_reflector
  
  ' A standard reflection procedure for this program has been established as follows:
  
  'If reflection = "hard" Then
  '  For pixel = first_pixel_emitter To last_pixel_emitter
  '    trend(xe_0(pixel), ye_0(pixel)) = 0
  '  Next
  'Else
  '  For pixel = first_pixel_emitter To last_pixel_emitter
  '    trend(xe_1(pixel),ye_1(pixel))=e_mix(pixel)*trend(xe_1a(pixel),ye_1a(pixel))+(1-e_mix(pixel))*trend(xe_1b(pixel),ye_1b(pixel))
  '    trend(xe_0(pixel),ye_0(pixel))=e_mix(pixel)*trend(xe_0a(pixel),ye_0a(pixel))+(1-e_mix(pixel))*trend(xe_0b(pixel),ye_0b(pixel))
  '    trend(xe_2(pixel),ye_2(pixel))=0
  '    trend(xe_3(pixel),ye_3(pixel))=0
  '  Next
  'End If
  
  ' The goal was to obtain the simplest, hence fastest results
  ' for SOFT REFLECTION, with appears a bit complicated to handle
  ' here because of Mr. Marcotte's diagonal calculus. The variable
  ' ratio allows one to obtain better results for curved reflectors
  ' such as parabolic or elliptic. As a matter of fact, soft reflection
  ' produces a different node and antinode pattern where energy
  ' is mainly concentrated alongside the reflector. This particular
  ' behavior is indeed especially problematic for curved reflectors.
  ' Thus, some residual artifacts cannot be avoided, albeit a better 
  ' depixelation method such a bicubic remains possible.
  
  ' HARD REFLECTION remains extremely simple to obtain, though:
  ' trend(xe_0(pixel), ye_0(pixel)) = 0
  
    Case "straight"
  
      first_pixel_emitter = upper_emitter
      last_pixel_emitter = lower_emitter
      For pixel = first_pixel_emitter To last_pixel_emitter
        xe_0(pixel)  = 0                                              'main pixel coordinates on reflector.
        ye_0(pixel)  = pixel
        xe_0a(pixel) = 1                                              'pixel trend to be transferred to xe_0, ye_0.
        ye_0a(pixel) = pixel
        xe_0b(pixel) = 1                                              'same pixel (useless here).
        ye_0b(pixel) = pixel
        xe_1(pixel)  = 1                                              'next pixel to the right to receive 1a,1b below.
        ye_1(pixel)  = pixel
        xe_1a(pixel) = 1                                              'pixel trend to be transferred to xe_1, ye_1. 
        ye_1a(pixel) = pixel
        xe_1b(pixel) = 1                                              'same pixel (useless here). 
        ye_1b(pixel) = pixel
        xe_2(pixel)  = -1                                             'next pixel to the left for canceling trend.
        ye_2(pixel)  = pixel
        xe_3(pixel)  = -1                                             'same pixel (useless here).
        ye_3(pixel)  = pixel
        e_mix(pixel) = 1                                              'no mixing for vertical or horizontal reflectors.
      Next
  
    Case "parabola"
  
  '   Processing external emitter upper parabola section.
  
      pixel = 0
      radius = 2 * x_emitter                                          'circle of reference radius for parabola.
      first_pixel_emitter  = -1
      last_pixel_emitter   = -1
  
      For x_pixel = x_width To 0 Step -1                              'x_pixel stands for parabola sagitta.
        h_parabola = Sqr(2 * x_pixel * radius)                        'classic parabola formula; h stands for height.
        y_pixel = y_center - h_parabola                               'y_pixel stands for h referred to window center.
        If y_pixel = 0 Then x_start = x_pixel: Exit For               'looking for the first on-screen pixel.
        If y_pixel > 0 Then                                           'parabola may be narrower than window height.
          x_start = x_width
          If upper_emitter < y_pixel Then upper_emitter = y_pixel
          If lower_emitter > 2 * y_center - y_pixel Then lower_emitter = 2 * y_center - y_pixel
          Exit For
        End If
      Next
  
      For x_pixel = x_start To 0 Step -1
        h_parabola = Sqr(2 * x_pixel * radius)
        y_pixel = y_center - h_parabola
        If first_pixel_emitter  = -1 And y_pixel = upper_emitter      Then first_pixel_emitter  = pixel
        If last_pixel_emitter   = -1 And y_pixel = lower_emitter + 1  Then last_pixel_emitter   = pixel - 1
        axial_distance = radius + x_pixel                             'orthogonal to curve for this coordinate.
        angle = Atn((axial_distance - x_pixel) / h_parabola)          'angle to VERTICAL axis.
        If angle > pi / 4 Then y_start = y_pixel: Exit For            'switch to vertical scanning to avoid gaps.
        angle = 2 * angle                                             'the goal is to obtain cos(angle) = 0 for 45°.
        e_mix(pixel) = (Cos(angle))^2                                 'trend mixing works for both 0a,0b and 1a,1b below.
        xe_0(pixel)  = x_pixel                                        'main reflector pixel. Receives xe_0a / 0b below.
        ye_0(pixel)  = y_pixel
        xe_0a(pixel) = x_pixel                                        'always next pixel down. To be transferred xe_0.
        ye_0a(pixel) = y_pixel + 1
        xe_0b(pixel) = x_pixel + 1                                    '   ...pixel and mixed with this one on its right.
        ye_0b(pixel) = y_pixel + 1
        xe_1(pixel)  = x_pixel                                        'always next pixel down. Receives 1a and 1b below.
        ye_1(pixel)  = y_pixel + 1
        xe_1a(pixel) = x_pixel                                        'always two pixels down. To be transferred to xe_1
        ye_1a(pixel) = y_pixel + 2
        xe_1b(pixel) = x_pixel + 1                                    '         ...and mixed with this one on its right.
        ye_1b(pixel) = y_pixel + 2
        xe_2(pixel)  = x_pixel                                        'one pixel up for canceling trend.
        ye_2(pixel)  = y_pixel - 1
        xe_3(pixel)  = x_pixel - 1                                    'one pixel up and to the left for canceling trend.
        ye_3(pixel)  = y_pixel - 1
        pixel += 1
      Next

  '   Processing parabola emitter section nearer to axis.
  
      For y_pixel = y_start To y_center
        If first_pixel_emitter  = -1 And y_pixel = upper_emitter      Then first_pixel_emitter  = pixel
        If last_pixel_emitter   = -1 And y_pixel = lower_emitter + 1  Then last_pixel_emitter   = pixel - 1
        h_parabola = y_center - y_pixel
        x_pixel = h_parabola ^ 2 / 2 / radius                         'classic parabola sagitta.
        axial_distance = radius + x_pixel                             'orthogonal to curve for this coordinate.
        angle = Atn(h_parabola / (axial_distance - x_pixel))          'angle to HORIZONTAL axis.
        angle = 2 * angle                                             'the goal is to obtain cos(angle) = 0 for 45°.
        e_mix(pixel) = (Cos(angle))^2                                 'trend mixing works for both 0a,0b and 1a,1b below.
        xe_0(pixel)  = x_pixel                                        'main reflector pixel. Receives xe_0a / 0b below.
        ye_0(pixel)  = y_pixel
        xe_0a(pixel) = x_pixel + 1                                    'next pixel to the right. To be transferred to main
        ye_0a(pixel) = y_pixel
        xe_0b(pixel) = x_pixel + 1                                    '...pixel and mixed with this one next pixel down.
        ye_0b(pixel) = y_pixel + 1
        xe_1(pixel)  = x_pixel + 1                                    'next pixel to the right. Receives 1a and 1b below.
        ye_1(pixel)  = y_pixel
        xe_1a(pixel) = x_pixel + 2                                    'two pixels to the right. To be transferred to
        ye_1a(pixel) = y_pixel
        xe_1b(pixel) = x_pixel + 2                                    ' ...xe_1 and mixed with this one next pixel down.
        ye_1b(pixel) = y_pixel + 1
        xe_2(pixel)  = x_pixel - 1                                    'next pixel to the left for canceling trend.
        ye_2(pixel)  = y_pixel
        xe_3(pixel)  = x_pixel - 1                                    'one pixel up and to the left for canceling trend.
        ye_3(pixel)  = y_pixel - 1
        pixel += 1
      Next
  
      number = pixel - 1
  
      For pixel = number To 2 * number                                'processing ellipse symmetric lower section.
        e_mix(pixel) =  e_mix(2 * number - pixel)
        xe_0( pixel) =                xe_0( 2 * number - pixel)
        xe_0a(pixel) =                xe_0a(2 * number - pixel)
        xe_0b(pixel) =                xe_0b(2 * number - pixel)
        xe_1( pixel) =                xe_1( 2 * number - pixel)
        xe_1a(pixel) =                xe_1a(2 * number - pixel)
        xe_1b(pixel) =                xe_1b(2 * number - pixel)
        xe_2( pixel) =                xe_2( 2 * number - pixel)
        xe_3( pixel) =                xe_3( 2 * number - pixel)
        ye_0( pixel) = 2 * y_center - ye_0( 2 * number - pixel)
        ye_0a(pixel) = 2 * y_center - ye_0a(2 * number - pixel)
        ye_0b(pixel) = 2 * y_center - ye_0b(2 * number - pixel)
        ye_1( pixel) = 2 * y_center - ye_1( 2 * number - pixel)
        ye_1a(pixel) = 2 * y_center - ye_1a(2 * number - pixel)
        ye_1b(pixel) = 2 * y_center - ye_1b(2 * number - pixel)
        ye_2( pixel) = 2 * y_center - ye_2( 2 * number - pixel)
        ye_3( pixel) = 2 * y_center - ye_3( 2 * number - pixel)
        If first_pixel_emitter  = -1 And ye_0( pixel) = upper_emitter      Then first_pixel_emitter  = pixel
        If last_pixel_emitter   = -1 And ye_0( pixel) = lower_emitter  + 1 Then last_pixel_emitter   = pixel - 1
      Next
      
      If last_pixel_emitter = -1 Then last_pixel_emitter = pixel - 1
  
  
    Case "ellipse"                                                    'emitter elliptic reflector.
  
  ' The ellipse precalculus below is based on the fact that a circle
  ' undergoing the Lorentz transformations becomes a squashed ellipse.
  ' It is simpler than the classic calculus based on Schwarzschild's
  ' constant K for conics: squashed ellipse: K > 0, sphere: K = 0,
  ' prolate ellipse: K < 0 > -1, parabola: K = -1, hyperbola: K < -1.
  ' According to Schwarzschild, sagittae for all conics are given by:
  ' sagitta = h^2 / (r * (1 + Sqr(1 - (h^2 / r^2) * (K + 1)))) where
  ' the circle of reference radius r is not the same for prolate and
  ' oblate ellipses. h (height) stands for off-axis distance.
  ' Schwarzschild's circle of reference radius r and Schwarzschild's
  ' constant K should match both the emitter to receiver ratio and
  ' the emitter to its reflector distance. Below, beta is equivalent
  ' to Schwarzschild's constant (K = - beta) which is useless here.
  ' It should be emphasized that the ALL of the ellipse functions can
  ' be derived from the Lorentz transformations. As far as I know,
  ' this had never been pointed out.
  
      radius = x_transverse_axis                                      'large radius as reference for circle sagitta.
      factor = x_receiver / x_emitter                                 'Descartes' optical magnification factor X.
      beta = (factor - 1) / (factor + 1)                              'inversely: factor = (1 + beta)/(1 - beta)
      g_Lorentz = Sqr(1 - beta^2)                                     'Lorentz's contraction factor g (vertically here).
      small_radius = g_Lorentz * radius                               'ellipse small radius contracted according to g.
      diagonal = Sqr((radius - x_emitter)^2 + small_radius^2)         'proof and/or reference: diagonal = large radius.
  '   r_Schwarzschild = radius * g_Lorentz^2                          'Schwarzschild's circle of reference radius r.
      r_Schwarzschild = 2 * (x_emitter*x_receiver) / (x_emitter+x_receiver)'same result using Descartes' optical formula.
  
  '   Processing emitter squashed (oblate) ellipse upper section.
  
      pixel = 0
      first_pixel_emitter  = -1
      last_pixel_emitter   = -1
      If y_center - upper_emitter  > small_radius Then upper_emitter  = y_center - small_radius
      If lower_emitter - y_center  > small_radius Then lower_emitter  = y_center + small_radius
  
  
      For x_pixel = radius To 0 Step -1                               'x_pixel stands for circle or ellipse sagitta.
        h_sphere = Sqr(radius^2 - (radius - x_pixel)^2)               'classic circle formula where h stands for height.
        y_pixel = y_center - g_Lorentz * h_sphere                     'y_pixel stands for ellipse contracted height.
        If first_pixel_emitter  = -1 And y_pixel = upper_emitter      Then first_pixel_emitter  = pixel
        If last_pixel_emitter   = -1 And y_pixel = lower_emitter + 1  Then last_pixel_emitter   = pixel - 1
        axial_distance=r_Schwarzschild+x_pixel*(radius-r_Schwarzschild)/radius'orthogonal to curve for this coordinate.
        angle = Atn((axial_distance - x_pixel) / (y_center - y_pixel))'angle to VERTICAL axis.
        If angle > pi / 4 Then y_start = y_pixel: Exit For            'switch to vertical scanning to avoid gaps.
        angle = 2 * angle                                             'the goal is to obtain cos(angle) = 0 for 45°.
        e_mix(pixel) = (Cos(angle))^2                                 'trend mixing works for both 0a,0b and 1a,1b below.
  
        xe_0(pixel)  = x_pixel                                        'main reflector pixel. Receives xe_0a / 0b below.
        ye_0(pixel)  = y_pixel
        xe_0a(pixel) = x_pixel                                        'always next pixel down. To be transferred to main
        ye_0a(pixel) = y_pixel + 1
        xe_0b(pixel) = x_pixel + 1                                    '   ...pixel and mixed with this one on its right.
        ye_0b(pixel) = y_pixel + 1
        xe_1(pixel)  = x_pixel                                        'always next pixel down. Receives 1a and 1b below.
        ye_1(pixel)  = y_pixel + 1
        xe_1a(pixel) = x_pixel                                        'always two pixels down. To be transferred to xe_1
        ye_1a(pixel) = y_pixel + 2
        xe_1b(pixel) = x_pixel + 1                                    '         ...and mixed with this one on its right.
        ye_1b(pixel) = y_pixel + 2
        xe_2(pixel)  = x_pixel                                        'one pixel up for canceling trend.
        ye_2(pixel)  = y_pixel - 1
        xe_3(pixel)  = x_pixel - 1                                    'one pixel up and to the left for canceling trend.
        ye_3(pixel)  = y_pixel - 1
  
        pixel += 1
      Next
      
  '   Processing emitter elongated (prolate) ellipse section near to axis.
  
      For y_pixel = y_start To y_center
        If first_pixel_emitter  = -1 And y_pixel = upper_emitter      Then first_pixel_emitter  = pixel
        If last_pixel_emitter   = -1 And y_pixel = lower_emitter + 1  Then last_pixel_emitter   = pixel - 1
        h_sphere = (y_center - y_pixel) / g_Lorentz                   'sphere uncontracted height.
        x_pixel = radius - Sqr(radius^2 - h_sphere^2)                 'sphere or ellipse sag.
        axial_distance=r_Schwarzschild+x_pixel*(radius-r_Schwarzschild)/radius'orthogonal to curve for this coordinate.
        angle = Atn((y_center - y_pixel) / (axial_distance - x_pixel))'angle to HORIZONTAL axis.
        angle = 2 * angle                                             'the goal is to obtain cos(angle) = 0 for 45°.
        e_mix(pixel) = (Cos(angle))^2                                 'trend mixing works for both 0a,0b and 1a,1b below.
        
        xe_0(pixel)  = x_pixel                                        'main reflector pixel. Receives xe_0a / 0b below.
        ye_0(pixel)  = y_pixel
        xe_0a(pixel) = x_pixel + 1                                    'next pixel to the right. To be transferred to main
        ye_0a(pixel) = y_pixel
        xe_0b(pixel) = x_pixel + 1                                    '...pixel and mixed with this one next pixel down.
        ye_0b(pixel) = y_pixel + 1
        xe_1(pixel)  = x_pixel + 1                                    'next pixel to the right. Receives 1a and 1b below.
        ye_1(pixel)  = y_pixel
        xe_1a(pixel) = x_pixel + 2                                    'two pixels to the right. To be transferred to
        ye_1a(pixel) = y_pixel
        xe_1b(pixel) = x_pixel + 2                                    ' ...xe_1 and mixed with this one next pixel down.
        ye_1b(pixel) = y_pixel + 1
        xe_2(pixel)  = x_pixel - 1                                    'next pixel to the left for canceling trend.
        ye_2(pixel)  = y_pixel
        xe_3(pixel)  = x_pixel - 1                                    'one pixel up and to the left for canceling trend.
        ye_3(pixel)  = y_pixel - 1
  
        pixel += 1
      Next
  
      number = pixel - 1
  
      For pixel = number To 2 * number                                'processing ellipse symmetric lower section.
        e_mix(pixel) =  e_mix(2 * number - pixel)
        xe_0( pixel) =                xe_0( 2 * number - pixel)
        xe_0a(pixel) =                xe_0a(2 * number - pixel)
        xe_0b(pixel) =                xe_0b(2 * number - pixel)
        xe_1( pixel) =                xe_1( 2 * number - pixel)
        xe_1a(pixel) =                xe_1a(2 * number - pixel)
        xe_1b(pixel) =                xe_1b(2 * number - pixel)
        xe_2( pixel) =                xe_2( 2 * number - pixel)
        xe_3( pixel) =                xe_3( 2 * number - pixel)
        ye_0( pixel) = 2 * y_center - ye_0( 2 * number - pixel)
        ye_0a(pixel) = 2 * y_center - ye_0a(2 * number - pixel)
        ye_0b(pixel) = 2 * y_center - ye_0b(2 * number - pixel)
        ye_1( pixel) = 2 * y_center - ye_1( 2 * number - pixel)
        ye_1a(pixel) = 2 * y_center - ye_1a(2 * number - pixel)
        ye_1b(pixel) = 2 * y_center - ye_1b(2 * number - pixel)
        ye_2( pixel) = 2 * y_center - ye_2( 2 * number - pixel)
        ye_3( pixel) = 2 * y_center - ye_3( 2 * number - pixel)
  
        If first_pixel_emitter  = -1 And ye_0( pixel) = upper_emitter      Then first_pixel_emitter  = pixel
        If last_pixel_emitter   = -1 And ye_0( pixel) = lower_emitter  + 1 Then last_pixel_emitter   = pixel - 1
      Next
      
      If last_pixel_emitter = -1 Then last_pixel_emitter = pixel - 1
      
    Case "corner"
  
      first_pixel_emitter = upper_emitter
      last_pixel_emitter = lower_emitter
      x_pixel = y_center - upper_emitter
      For pixel = first_pixel_emitter To last_pixel_emitter
        e_mix(pixel) = 1
        If pixel < y_center Then
          xe_0(pixel)  = x_pixel                                      'main pixel coordinates on reflector.
          ye_0(pixel)  = pixel
          xe_0a(pixel) = x_pixel + 1                                  'pixel trend to be transferred to xe_0, ye_0.
          ye_0a(pixel) = pixel   + 1
          xe_0b(pixel) = x_pixel + 1                                  'same pixel (useless here).
          ye_0b(pixel) = pixel   + 1
          xe_1(pixel)  = x_pixel + 1                                  'next pixel to the right to receive 1a,1b below.
          ye_1(pixel)  = pixel
          xe_1a(pixel) = x_pixel + 2                                  'pixel trend to be transferred to xe_1, ye_1. 
          ye_1a(pixel) = pixel   + 1
          xe_1b(pixel) = x_pixel + 2                                  'same pixel (useless here). 
          ye_1b(pixel) = pixel   + 1
          xe_2(pixel)  = x_pixel                                      'next pixel up for canceling trend.
          ye_2(pixel)  = pixel   - 1
          xe_3(pixel)  = x_pixel - 1                                  'next pixel up and to the left for canceling trend.
          ye_3(pixel)  = pixel   - 1
          x_pixel -= 1
        Elseif pixel = y_center Then
          xe_0(pixel)  = x_pixel
          ye_0(pixel)  = pixel
          xe_0a(pixel) = x_pixel + 1
          ye_0a(pixel) = pixel
          xe_0b(pixel) = x_pixel + 1
          ye_0b(pixel) = pixel
          xe_1(pixel)  = x_pixel + 1
          ye_1(pixel)  = pixel
          xe_1a(pixel) = x_pixel + 2
          ye_1a(pixel) = pixel
          xe_1b(pixel) = x_pixel + 2
          ye_1b(pixel) = pixel
          xe_2(pixel)  = x_pixel
          ye_2(pixel)  = pixel - 1
          xe_3(pixel)  = x_pixel
          ye_3(pixel)  = pixel + 1
          x_pixel += 1
        Else
          xe_0(pixel)  = x_pixel
          ye_0(pixel)  = pixel
          xe_0a(pixel) = x_pixel + 1
          ye_0a(pixel) = pixel   - 1
          xe_0b(pixel) = x_pixel + 1
          ye_0b(pixel) = pixel   - 1
          xe_1(pixel)  = x_pixel + 1
          ye_1(pixel)  = pixel
          xe_1a(pixel) = x_pixel + 2
          ye_1a(pixel) = pixel   - 1
          xe_1b(pixel) = x_pixel + 2
          ye_1b(pixel) = pixel   - 1
          xe_2(pixel)  = x_pixel
          ye_2(pixel)  = pixel   + 1
          xe_3(pixel)  = x_pixel - 1
          ye_3(pixel)  = pixel   + 1
          x_pixel += 1
      End If
      Next
  
    Case "three-sided"
  
      first_pixel_emitter = upper_emitter
      last_pixel_emitter = lower_emitter
      upper_vertical = y_center - x_emitter                           'corners 45° to emitter upward and downward.
      lower_vertical = y_center + x_emitter
      If upper_vertical <= upper_emitter Then
        upper_vertical = upper_emitter
        x_pixel = 0
      Else
        x_pixel = upper_vertical - upper_emitter
      End If
  
      For pixel = first_pixel_emitter To last_pixel_emitter
        e_mix(pixel) = 1
        If pixel < upper_vertical Then
          xe_0(pixel)  = x_pixel                                      'main pixel coordinates on reflector.
          ye_0(pixel)  = pixel
          xe_0a(pixel) = x_pixel + 1                                  'pixel trend to be transferred to xe_0, ye_0.
          ye_0a(pixel) = pixel   + 1
          xe_0b(pixel) = x_pixel + 1                                  'same pixel (useless here).
          ye_0b(pixel) = pixel   + 1
          xe_1(pixel)  = x_pixel + 1                                  'next pixel to the right to receive 1a,1b below.
          ye_1(pixel)  = pixel
          xe_1a(pixel) = x_pixel + 2                                  'pixel trend to be transferred to xe_1, ye_1. 
          ye_1a(pixel) = pixel   + 1
          xe_1b(pixel) = x_pixel + 2                                  'same pixel (useless here). 
          ye_1b(pixel) = pixel   + 1
          xe_2(pixel)  = x_pixel                                      'next pixel up for canceling trend.
          ye_2(pixel)  = pixel   - 1
          xe_3(pixel)  = x_pixel - 1                                  'next pixel up and to the left for canceling trend.
          ye_3(pixel)  = pixel   - 1
          x_pixel -= 1
        Elseif pixel <= lower_vertical Then
          xe_0(pixel)  = x_pixel
          ye_0(pixel)  = pixel
          xe_0a(pixel) = x_pixel + 1
          ye_0a(pixel) = pixel
          xe_0b(pixel) = x_pixel + 1
          ye_0b(pixel) = pixel
          xe_1(pixel)  = x_pixel + 1
          ye_1(pixel)  = pixel
          xe_1a(pixel) = x_pixel + 2
          ye_1a(pixel) = pixel
          xe_1b(pixel) = x_pixel + 2
          ye_1b(pixel) = pixel
          xe_2(pixel)  = x_pixel
          ye_2(pixel)  = pixel - 1
          xe_3(pixel)  = x_pixel
          ye_3(pixel)  = pixel + 1
        Else
          x_pixel += 1
          xe_0(pixel)  = x_pixel
          ye_0(pixel)  = pixel
          xe_0a(pixel) = x_pixel + 1
          ye_0a(pixel) = pixel   - 1
          xe_0b(pixel) = x_pixel + 1
          ye_0b(pixel) = pixel   - 1
          xe_1(pixel)  = x_pixel + 1
          ye_1(pixel)  = pixel
          xe_1a(pixel) = x_pixel + 2
          ye_1a(pixel) = pixel   - 1
          xe_1b(pixel) = x_pixel + 2
          ye_1b(pixel) = pixel   - 1
          xe_2(pixel)  = x_pixel
          ye_2(pixel)  = pixel   + 1
          xe_3(pixel)  = x_pixel - 1
          ye_3(pixel)  = pixel   + 1
      End If
    Next
  
  End Select
  
  Select Case r_reflector                                             'See emitter above for 'REM comments.
  
    Case "straight"
  
      If reflection = "hard" Then
           x_reflector = x_receiver + quarter_lambda                  'here, receiver reflector distance is adjustable.
      Else x_reflector = x_receiver + half_lambda
      End If
      first_pixel_receiver = upper_receiver
      last_pixel_receiver = lower_receiver
      For pixel = first_pixel_receiver To last_pixel_receiver
        xr_0(pixel)  = x_reflector
        yr_0(pixel)  = pixel
        xr_0a(pixel) = x_reflector - 1
        yr_0a(pixel) = pixel
        xr_0b(pixel) = x_reflector - 1
        yr_0b(pixel) = pixel
        xr_1(pixel)  = x_reflector - 1
        yr_1(pixel)  = pixel
        xr_1a(pixel) = x_reflector - 1
        yr_1a(pixel) = pixel
        xr_1b(pixel) = x_reflector - 1
        yr_1b(pixel) = pixel
        xr_2(pixel)  = x_reflector + 1
        yr_2(pixel)  = pixel
        xr_3(pixel)  = x_reflector + 1
        yr_3(pixel)  = pixel
        r_mix(pixel) = 1
      Next
  
    Case "parabola"                                                   'receiver parabolic reflector.
  
  '   Processing receiver external upper parabola section.
  
      pixel = 0
      radius = 2 * (x_width - x_receiver)
      first_pixel_receiver = -1
      last_pixel_receiver  = -1
      For x_pixel = x_width To 0 Step -1
        h_parabola = Sqr(2 * x_pixel * radius)
        y_pixel = y_center - h_parabola
        If y_pixel = 0 Then x_start = x_pixel: Exit For
        If y_pixel > 0 Then
          x_start = x_width
          If upper_receiver < y_pixel Then upper_receiver = y_pixel
          If lower_receiver > 2 * y_center - y_pixel Then lower_receiver = 2 * y_center - y_pixel
          Exit For
        End If
      Next
  
      For x_pixel = x_start To 0 Step -1
        h_parabola = Sqr(2 * x_pixel * radius)
        y_pixel = y_center - h_parabola
        If first_pixel_receiver = -1 And y_pixel = upper_receiver     Then first_pixel_receiver = pixel
        If last_pixel_receiver  = -1 And y_pixel = lower_receiver + 1 Then last_pixel_receiver  = pixel - 1
        axial_distance = radius + x_pixel
        angle = Atn((axial_distance - x_pixel) / h_parabola)
        If angle > pi / 4 Then y_start = y_pixel: Exit For
        angle = 2 * angle
        r_mix(pixel) = (Cos(angle))^2
        xr_0(pixel)  = x_width - x_pixel
        yr_0(pixel)  = y_pixel
        xr_0a(pixel) = x_width - x_pixel
        yr_0a(pixel) = y_pixel + 1
        xr_0b(pixel) = x_width - x_pixel - 1
        yr_0b(pixel) = y_pixel + 1
        xr_1(pixel)  = x_width - x_pixel
        yr_1(pixel)  = y_pixel + 1
        xr_1a(pixel) = x_width - x_pixel
        yr_1a(pixel) = y_pixel + 2
        xr_1b(pixel) = x_width - x_pixel - 1
        yr_1b(pixel) = y_pixel + 2
        xr_2(pixel)  = x_width - x_pixel
        yr_2(pixel)  = y_pixel - 1
        xr_3(pixel)  = x_width - x_pixel + 1
        yr_3(pixel)  = y_pixel - 1
        pixel += 1
      Next
  
  '   Processing receiver parabola section nearer to axis.
  
      For y_pixel = y_start To y_center
        If first_pixel_receiver = -1 And y_pixel = upper_receiver     Then first_pixel_receiver = pixel
        If last_pixel_receiver  = -1 And y_pixel = lower_receiver + 1 Then last_pixel_receiver  = pixel - 1
        h_parabola = y_center - y_pixel
        x_pixel = h_parabola ^ 2 / 2 / radius
        axial_distance = radius + x_pixel
        angle = Atn(h_parabola / (axial_distance - x_pixel))
        angle = 2 * angle
        r_mix(pixel) = (Cos(angle))^2
        xr_0(pixel)  = x_width - x_pixel
        yr_0(pixel)  = y_pixel
        xr_0a(pixel) = x_width - x_pixel - 1
        yr_0a(pixel) = y_pixel
        xr_0b(pixel) = x_width - x_pixel - 1
        yr_0b(pixel) = y_pixel + 1
        xr_1(pixel)  = x_width - x_pixel - 1
        yr_1(pixel)  = y_pixel
        xr_1a(pixel) = x_width - x_pixel - 2
        yr_1a(pixel) = y_pixel
        xr_1b(pixel) = x_width - x_pixel - 2
        yr_1b(pixel) = y_pixel + 1
        xr_2(pixel)  = x_width - x_pixel + 1
        yr_2(pixel)  = y_pixel
        xr_3(pixel)  = x_width - x_pixel + 1
        yr_3(pixel)  = y_pixel - 1
        pixel += 1
      Next
  
  '   Processing receiver parabola symmetric lower section.
  
      number = pixel - 1
  
      For pixel = number To 2 * number
        r_mix(pixel) =  r_mix(2 * number - pixel)
        xr_0( pixel) =                xr_0( 2 * number - pixel)
        xr_0a(pixel) =                xr_0a(2 * number - pixel)
        xr_0b(pixel) =                xr_0b(2 * number - pixel)
        xr_1( pixel) =                xr_1( 2 * number - pixel)
        xr_1a(pixel) =                xr_1a(2 * number - pixel)
        xr_1b(pixel) =                xr_1b(2 * number - pixel)
        xr_2( pixel) =                xr_2( 2 * number - pixel)
        xr_3( pixel) =                xr_3( 2 * number - pixel)
        yr_0( pixel) = 2 * y_center - yr_0( 2 * number - pixel)
        yr_0a(pixel) = 2 * y_center - yr_0a(2 * number - pixel)
        yr_0b(pixel) = 2 * y_center - yr_0b(2 * number - pixel)
        yr_1( pixel) = 2 * y_center - yr_1( 2 * number - pixel)
        yr_1a(pixel) = 2 * y_center - yr_1a(2 * number - pixel)
        yr_1b(pixel) = 2 * y_center - yr_1b(2 * number - pixel)
        yr_2( pixel) = 2 * y_center - yr_2( 2 * number - pixel)
        yr_3( pixel) = 2 * y_center - yr_3( 2 * number - pixel)
        If first_pixel_receiver = -1 And yr_0( pixel) = upper_receiver     Then first_pixel_receiver = pixel
        If last_pixel_receiver  = -1 And yr_0( pixel) = lower_receiver + 1 Then last_pixel_receiver  = pixel - 1
      Next
      
      If last_pixel_receiver = -1 Then last_pixel_receiver = pixel - 1
  
    Case "ellipse"                                                    'receiver elliptic reflector.
  
      radius = x_width - .5 * (x_receiver + x_emitter)
      factor = (x_width - x_emitter) / (x_width - x_receiver)
      beta = (factor - 1) / (factor + 1)
      g_Lorentz = Sqr(1 - beta^2)
      small_radius = g_Lorentz * radius
      r_Schwarzschild = radius * g_Lorentz^2
      
  '   Processing receiver squashed (oblate) ellipse upper section.
  
      pixel = 0
      first_pixel_receiver = -1
      last_pixel_receiver  = -1
      If y_center - upper_receiver  > small_radius Then upper_receiver  = y_center - small_radius
      If lower_receiver - y_center  > small_radius Then lower_receiver  = y_center + small_radius
  
      For x_pixel = radius To 0 Step -1
        h_sphere = Sqr(radius^2 - (radius - x_pixel)^2)
        y_pixel = y_center - g_Lorentz * h_sphere
        If first_pixel_receiver = -1 And y_pixel = upper_receiver     Then first_pixel_receiver = pixel
        If last_pixel_receiver  = -1 And y_pixel = lower_receiver + 1 Then last_pixel_receiver  = pixel - 1
        axial_distance = r_Schwarzschild + x_pixel * (radius - r_Schwarzschild) / radius
        angle = Atn((axial_distance - x_pixel) / (y_center - y_pixel))
        If angle > pi / 4 Then y_start = y_pixel: Exit For
        angle = 2 * angle
        r_mix(pixel) = (Cos(angle))^2
  
        xr_0(pixel)  = x_width - x_pixel
        yr_0(pixel)  = y_pixel
        xr_0a(pixel) = x_width - x_pixel
        yr_0a(pixel) = y_pixel + 1
        xr_0b(pixel) = x_width - x_pixel - 1
        yr_0b(pixel) = y_pixel + 1
        xr_1(pixel)  = x_width - x_pixel
        yr_1(pixel)  = y_pixel + 1
        xr_1a(pixel) = x_width - x_pixel
        yr_1a(pixel) = y_pixel + 2
        xr_1b(pixel) = x_width - x_pixel - 1
        yr_1b(pixel) = y_pixel + 2
        xr_2(pixel)  = x_width - x_pixel
        yr_2(pixel)  = y_pixel - 1
        xr_3(pixel)  = x_width - x_pixel + 1
        yr_3(pixel)  = y_pixel - 1
  
        pixel += 1
      Next
      
'   Processing receiver elongated (prolate) ellipse section near to axis.
  
      For y_pixel = y_start To y_center
        If first_pixel_receiver = -1 And y_pixel = upper_receiver     Then first_pixel_receiver = pixel
        If last_pixel_receiver  = -1 And y_pixel = lower_receiver + 1 Then last_pixel_receiver  = pixel - 1
        h_sphere = (y_center - y_pixel) / g_Lorentz
        x_pixel = radius - Sqr(radius^2 - h_sphere^2)
        axial_distance=r_Schwarzschild+x_pixel*(radius-r_Schwarzschild)/radius'orthogonal to curve for this coordinate.
        angle = Atn((y_center - y_pixel) / (axial_distance - x_pixel))
        angle = 2 * angle
        r_mix(pixel) = (Cos(angle))^2
        
        xr_0(pixel)  = x_width - x_pixel
        yr_0(pixel)  = y_pixel
        xr_0a(pixel) = x_width - x_pixel - 1
        yr_0a(pixel) = y_pixel
        xr_0b(pixel) = x_width - x_pixel - 1
        yr_0b(pixel) = y_pixel + 1
        xr_1(pixel)  = x_width - x_pixel - 1
        yr_1(pixel)  = y_pixel
        xr_1a(pixel) = x_width - x_pixel - 2
        yr_1a(pixel) = y_pixel
        xr_1b(pixel) = x_width - x_pixel - 2
        yr_1b(pixel) = y_pixel + 1
        xr_2(pixel)  = x_width - x_pixel + 1
        yr_2(pixel)  = y_pixel
        xr_3(pixel)  = x_width - x_pixel + 1
        yr_3(pixel)  = y_pixel - 1
  
        pixel += 1
      Next
  
  '   Processing ellipse symmetric lower section.
  
      number = pixel - 1
  
      For pixel = number To 2 * number
        r_mix(pixel) =  r_mix(2 * number - pixel)
        xr_0( pixel) =                xr_0( 2 * number - pixel)
        xr_0a(pixel) =                xr_0a(2 * number - pixel)
        xr_0b(pixel) =                xr_0b(2 * number - pixel)
        xr_1( pixel) =                xr_1( 2 * number - pixel)
        xr_1a(pixel) =                xr_1a(2 * number - pixel)
        xr_1b(pixel) =                xr_1b(2 * number - pixel)
        xr_2( pixel) =                xr_2( 2 * number - pixel)
        xr_3( pixel) =                xr_3( 2 * number - pixel)
        yr_0( pixel) = 2 * y_center - yr_0( 2 * number - pixel)
        yr_0a(pixel) = 2 * y_center - yr_0a(2 * number - pixel)
        yr_0b(pixel) = 2 * y_center - yr_0b(2 * number - pixel)
        yr_1( pixel) = 2 * y_center - yr_1( 2 * number - pixel)
        yr_1a(pixel) = 2 * y_center - yr_1a(2 * number - pixel)
        yr_1b(pixel) = 2 * y_center - yr_1b(2 * number - pixel)
        yr_2( pixel) = 2 * y_center - yr_2( 2 * number - pixel)
        yr_3( pixel) = 2 * y_center - yr_3( 2 * number - pixel)
  
        If first_pixel_receiver = -1 And yr_0( pixel) = upper_receiver     Then first_pixel_receiver = pixel
        If last_pixel_receiver  = -1 And yr_0( pixel) = lower_receiver + 1 Then last_pixel_receiver  = pixel - 1
      Next
      
      If last_pixel_receiver = -1 Then last_pixel_receiver = pixel - 1
      
    Case "corner"
  
      first_pixel_receiver = upper_receiver
      last_pixel_receiver = lower_receiver
      x_pixel = x_width - (y_center - upper_receiver)
      For pixel = first_pixel_receiver To last_pixel_receiver
        r_mix(pixel) = 1
        If pixel < y_center Then
          xr_0(pixel)  = x_pixel
          yr_0(pixel)  = pixel
          xr_0a(pixel) = x_pixel - 1
          yr_0a(pixel) = pixel   + 1
          xr_0b(pixel) = x_pixel - 1
          yr_0b(pixel) = pixel   + 1
          xr_1(pixel)  = x_pixel - 1
          yr_1(pixel)  = pixel
          xr_1a(pixel) = x_pixel - 2
          yr_1a(pixel) = pixel   + 1
          xr_1b(pixel) = x_pixel - 2
          yr_1b(pixel) = pixel   + 1
          xr_2(pixel)  = x_pixel
          yr_2(pixel)  = pixel   - 1
          xr_3(pixel)  = x_pixel + 1
          yr_3(pixel)  = pixel   - 1
          x_pixel += 1
        Elseif pixel = y_center Then
          xr_0(pixel)  = x_pixel
          yr_0(pixel)  = pixel
          xr_0a(pixel) = x_pixel - 1
          yr_0a(pixel) = pixel
          xr_0b(pixel) = x_pixel - 1
          yr_0b(pixel) = pixel
          xr_1(pixel)  = x_pixel - 1
          yr_1(pixel)  = pixel
          xr_1a(pixel) = x_pixel - 2
          yr_1a(pixel) = pixel
          xr_1b(pixel) = x_pixel - 2
          yr_1b(pixel) = pixel
          xr_2(pixel)  = x_pixel
          yr_2(pixel)  = pixel - 1
          xr_3(pixel)  = x_pixel
          yr_3(pixel)  = pixel + 1
          x_pixel -= 1
        Else
          xr_0(pixel)  = x_pixel
          yr_0(pixel)  = pixel
          xr_0a(pixel) = x_pixel - 1
          yr_0a(pixel) = pixel   - 1
          xr_0b(pixel) = x_pixel - 1
          yr_0b(pixel) = pixel   - 1
          xr_1(pixel)  = x_pixel - 1
          yr_1(pixel)  = pixel
          xr_1a(pixel) = x_pixel - 2
          yr_1a(pixel) = pixel   - 1
          xr_1b(pixel) = x_pixel - 2
          yr_1b(pixel) = pixel   - 1
          xr_2(pixel)  = x_pixel
          yr_2(pixel)  = pixel   + 1
          xr_3(pixel)  = x_pixel + 1
          yr_3(pixel)  = pixel   + 1
          x_pixel -= 1
        End If
      Next
  
    Case "three-sided"
      If reflection = "hard" Then
           x_reflector = x_receiver + quarter_lambda                  'receiver reflector distance is adjustable.
      Else x_reflector = x_receiver + half_lambda
      End If
      If x_reflector < y_center - upper_receiver Then upper_receiver = y_center - x_reflector 'stop before left edge.
      If x_reflector < lower_receiver - y_center Then lower_receiver = y_center + x_reflector
      upper_vertical = y_center - (x_reflector - x_receiver)
      lower_vertical = y_center + (x_reflector - x_receiver)
      x_pixel = x_reflector - (upper_vertical - upper_receiver)
      first_pixel_receiver = upper_receiver
      last_pixel_receiver = lower_receiver
      For pixel = first_pixel_receiver To last_pixel_receiver
        r_mix(pixel) = 1
        If pixel < upper_vertical Then
          xr_0(pixel)  = x_pixel
          yr_0(pixel)  = pixel
          xr_0a(pixel) = x_pixel - 1
          yr_0a(pixel) = pixel   + 1
          xr_0b(pixel) = x_pixel - 1
          yr_0b(pixel) = pixel   + 1
          xr_1(pixel)  = x_pixel - 1
          yr_1(pixel)  = pixel
          xr_1a(pixel) = x_pixel - 2
          yr_1a(pixel) = pixel   + 1
          xr_1b(pixel) = x_pixel - 2
          yr_1b(pixel) = pixel   + 1
          xr_2(pixel)  = x_pixel
          yr_2(pixel)  = pixel   - 1
          xr_3(pixel)  = x_pixel + 1
          yr_3(pixel)  = pixel   - 1
          x_pixel += 1
        Elseif pixel <= lower_vertical Then
          xr_0(pixel)  = x_pixel
          yr_0(pixel)  = pixel
          xr_0a(pixel) = x_pixel - 1
          yr_0a(pixel) = pixel
          xr_0b(pixel) = x_pixel - 1
          yr_0b(pixel) = pixel
          xr_1(pixel)  = x_pixel - 1
          yr_1(pixel)  = pixel
          xr_1a(pixel) = x_pixel - 2
          yr_1a(pixel) = pixel
          xr_1b(pixel) = x_pixel - 2
          yr_1b(pixel) = pixel
          xr_2(pixel)  = x_pixel
          yr_2(pixel)  = pixel - 1
          xr_3(pixel)  = x_pixel
          yr_3(pixel)  = pixel + 1
        Else
          x_pixel -= 1
          xr_0(pixel)  = x_pixel
          yr_0(pixel)  = pixel
          xr_0a(pixel) = x_pixel - 1
          yr_0a(pixel) = pixel   - 1
          xr_0b(pixel) = x_pixel - 1
          yr_0b(pixel) = pixel   - 1
          xr_1(pixel)  = x_pixel - 1
          yr_1(pixel)  = pixel
          xr_1a(pixel) = x_pixel - 2
          yr_1a(pixel) = pixel   - 1
          xr_1b(pixel) = x_pixel - 2
          yr_1b(pixel) = pixel   - 1
          xr_2(pixel)  = x_pixel
          yr_2(pixel)  = pixel   + 1
          xr_3(pixel)  = x_pixel + 1
          yr_3(pixel)  = pixel   + 1
        End If
      Next
  
    Case "aperture"                                                   'not a reflector. See Sub Reflection_Management().
      first_pixel_receiver = 0
      last_pixel_receiver  = y_height
      For pixel = 0 To y_height                                       'for aperture drawing purpose.
        If pixel < upper_receiver Or pixel > lower_receiver Then      'stand for aperture limits.
          xr_0(pixel) = x_receiver
          yr_0(pixel) =   pixel
        Else
          xr_0(pixel) = x_receiver
          yr_0(pixel) = 0
        End If
      Next
  
    Case "two slits"                                                  'not a reflector. See Sub Reflection_Management().
      first_pixel_receiver = 0
      last_pixel_receiver  = y_height
      For pixel = 0 To y_height                                       'for slits drawing purpose.
        If Abs(pixel - upper_receiver) > quarter_lambda And Abs(pixel - lower_receiver) > quarter_lambda  Then
          xr_0(pixel) = x_receiver
          yr_0(pixel) =   pixel
        Else
          xr_0(pixel) = x_receiver
          yr_0(pixel) = 0
        End If
      Next
  
  End Select

  y = y_center - upper_emitter                                        'checking Fresnel's number = 1 
  sagitta = xe_0(first_pixel_emitter)
  diagonal = Sqr(y^2+(x_emitter-sagitta)^2)
  x_Fresnel = -10                                                     'focus point invisible if beyond wave area.
  For x = sagitta To x_width
    distance1 = x + x_emitter
    distance2 = diagonal + Sqr(y^2+(x-sagitta)^2)                     'looking for Rayleigh's lambda / 2 Criterion.
    If distance2 - distance1 < .5 * lambda Then x_Fresnel = x: Exit For'Fresnel's diffraction aperture focus point.
  Next                                                                'the lower radiation point occurs for .707 lambda.
  
  Graphics()
End Sub

'*********************************************************************
' WAVES IN COLOR, GRAPHICS AND GAIN CALCULUS
'*********************************************************************

Sub Wave_Display()
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy matrix_page, work_page
  
  
  If display = "amplitude" Then
    For x = 0 To x_width: For y = 0 To y_height
      luminance1 = brightness * Abs(20 * present(x,y))
      b = luminance1 / 2
      If b > 255 Then b = 255
      If luminance1 > 255 Then
        luminance2 = luminance1 - 255
        If luminance2 > 255 Then luminance2 = 255
        luminance1 = 255
      Else luminance2 = 0
      End If
      If present(x,y) > 0 Then                                        'using complementary magenta and emerald green.
        r = luminance2
        g = luminance1
      Else
        r = luminance1
        g = luminance2
      End If
         Pset(x,y), Rgb(r,g,b)
    Next: Next
  
  Elseif display = "energy" Then

' Mr. Anselme Dewavrin informed me in Oct. 2006 that the phase step
' for oscillations involving a fixed number of iterations had to be
' corrected according to the wavelength (see WaveMechanics01 program).
' A more accurate quadrature can be deduced using this correction.
' Instead of:

' phase angular step = 2 * pi / lambda                                'inaccurate.

' one should prefer "Dewavrin's constant", which is given by:

' k_Dewavrin = Sin(2 * pi / lambda)                                   'accurate value for each angular step.
    
' The shorter the wavelength, the greater the difference. This
' indicates that a genuine QUANTUM EFFECT takes place here. It has
' already been demonstrated that shorter waves travel slower for this
' reason. On the one hand, any quantum effect needs a constant in
' order to be predicted correctly. On the other hand, despite the
' fact that the correction Sin(2 * pi / lambda) is well known in the
' math literature, quantum waves are still almost totally ignored.
' Thus, Dewavrin's constant proves to be highly relevant and useful.

' This correction may be compared to the number of cuts needed to
' obtain a given number of sections. For instance, a long straw must
' be cut two times in order to obtain three smaller straws, hence
' the x - 1 formula transposed into lambda - 1 below.

' k_Dewavrin = 2 * pi / (lambda + (2 * pi / (lambda - 1)))            'almost accurate.

' Finally, the factor for quadrature (pi/2 offset) is given by:

    factor = .5 / k_Dewavrin                                          'matches a two-step difference, which can't be
                                                                      'avoided. The middle point below is: present(x,y). 
    For x = 0 To x_width: For y = 0 To y_height
      quadrature = factor * (trend(x,y) - past(x,y))                  'actually (trend - present) + (present - past), so
      luminance1 = brightness * 10 * Sqr(present(x,y)^2+quadrature^2) 'there are two of Dewavrins's steps involved.
      If luminance1 > 255 Then luminance1 = 255
      Pset(x,y), Rgb(luminance1,luminance1,luminance1)
    Next: Next
    Circle(x_Fresnel,y_center),3, gray                                'Fresnel's diffraction aperture focus point.
    Circle(x_Fresnel,y_center),2, black
    
  Elseif display = "standing waves" Then

' Potential and kinetic energy as established below are hypothetic,
' as they should mechanically appear in a device such as a pendulum.
' The goal here is to make standing waves becoming visible by means
' of an also hypothetic Lagrangian. This method was elaborated by
' Mr. Jocelyn Marcotte and demonstrated in the previous program
' WaveMechanics04. In my picture, potential energy follows kinetic
' energy with a pi/2 offset. It is fully stored into a hypothetic
' field of force when kinetic energy is nil (when the pendulum stops).
' So it is equivalent to quadrature and the sum potential + kinetic
' should be constant in accordance with Pythagoras's theorem.

' However, the code below is rather consistent with Mr. Marcotte's
' point of view and it nevertheless yields amazingly nice results.
' Such an achievement deserves the warmest congratulations.

' Please note that standing waves appear twice per period with a zero
' amplitude phase in-between. Peaks occur twice per wavelength because 
' they may be positive or negative. Thus the wavelength may appear
' two times shorter. It is not an error, it is just a choice allowing
' the yellow color to indicate antinodes. As a consequence, the color
' alongside the reflectors is inverted for soft and hard reflection.

  For x = 0 To x_width: For y = 0 To y_height
    kinetic = (trend(x,y) - past(x,y))^2
    potential = (present(x+1,y)-present(x-1,y))^2+(present(x,y+1)-present(x,y-1))^2
    Lagrangian = kinetic  - potential                                 'classic Lagrangian.
    luminance = brightness * .01 * lambda^2 * Lagrangian              'lambda^2 because of the wave generator circle.
    
    If luminance > 0 Then
      r = luminance / 2: g = luminance / 2                            'r / 2 + g / 2 = 1 yellow, yet yellow is
      b = 0                                                           'biologically much brighter than blue.
    Else
      luminance = -luminance
      b = luminance
      r = luminance / 3                                               'adding fractional r and g to brighten blue.
      g = luminance / 3
    End If
    If r > 255 Then r = 255
    If g > 255 Then g = 255
    If b > 255 Then b = 255
    Pset(x, y), Rgb(r,g,b)
  Next: Next
    
  End If

  If e_reflector <> "no reflector" Then
    For pixel = first_pixel_emitter To last_pixel_emitter             'drawing the emitter reflector.
      Pset(xe_0(pixel), ye_0(pixel)), white
    Next
  End If
  If r_reflector <> "no reflector" Then
    For pixel = first_pixel_receiver To last_pixel_receiver           'drawing the receiver reflector.
      Pset(xr_0(pixel), yr_0(pixel)), white
    Next
  End If
  
  If curve = 1 Then                                                   'amplitude curve.
    previous = y_center
    For x = 2 To x_width - 2
      amplitude = y_center - brightness * present(x,y_center)
      Line(x,  previous)-(x+1,amplitude), white                       'graphics.
      Line(x-1,previous)-(x,  amplitude), black
      previous = amplitude
    Next
  ' Locate 35, 15: ? "Iteration"; iteration; " "; Int(Timer - t); " sec."
    If horizontally < x_width Then Line(2*x_emitter-horizontally,y_height-vertically)-(horizontally,vertically), gray, b
  End If
  
  If emitter_gain > 0 Then                                            'checking emitter gain.
    Locate 42, 35
    If emitter_gain = 1 Then
      If iteration < x_receiver - x_emitter Then
        skipped_frames = 8
        ? " Waiting for reference.     "
      Elseif iteration < x_receiver - x_emitter + 2 * lambda Then
        skipped_frames = 0
        If Abs(present(x_receiver, y_center)) > reference Then reference = Abs(present(x_receiver, y_center))
        If reference < .1 Then reference = 0
        ? " Amplitude reference ";
        ? Using "##.##"; reference
      Elseif iteration = x_receiver - x_emitter + 2 * lambda Then
        e_reflector = previous_reflector
        Update()
        skipped_frames = 8
        iteration = 0
        emitter_gain = 2
      End If
    Elseif emitter_gain = 2 Then
      If iteration > x_receiver - x_emitter Then skipped_frames = 0    
      If Abs(present(x_receiver,y_center)) > gain Then gain = Abs(present(x_receiver,y_center))
      If gain / reference < .00001 Then gain = .00001 * reference
      ratio = gain^2 / reference^2                                    'energy = amplitude squared.
      Locate 42, 35: ? " Reflector gain ";
      If ratio > .01 Then
        ? Using " ##.# dB"; Log(ratio) * log_conversion               'natural logarithm to common logarithm * 10.
        Locate 43, 35: ? " Ratio  1 : ";
        ? Using "##.##       "; gain^2 / reference^2
      Else
        ? "(waiting)."
      End If
      
    End If
  End If
End Sub

'*********************************************************************
' WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Sub Wave_Generator()
  If iteration >= pulse Then
    Return
  Elseif iteration < lambda Then
    amplitude = iteration * (20000 / lambda^2.5) / lambda             'tempering first wave to avoid rebounds.
  Else amplitude = 20000 / lambda^2.5                                 're: impulse zone smaller for shorter wavelength. 
  End If

  phase = iteration * 2 * pi / lambda                                 'Dewavrin's constant may correct the wavelength,
  For x_coord = -lambda / 4 To lambda / 4                             '...but not the wave speed which is slightly < 1.
    x_squared = x_coord^2
    For y_coord = -lambda / 4 To lambda / 4
      distance1 = Sqr(x_squared + y_coord^2)
      If distance1 <= .25 * lambda Then
        radian = 2 * pi * distance1 / lambda
        potential = Cos(radian)
        trend(x_emitter+x_coord,y_center+y_coord)=trend(x_emitter+x_coord,y_center+y_coord)+amplitude*potential*Cos(phase)'generator.
      End If
    Next
  Next
End Sub
 