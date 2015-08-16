' This is a FreeBasic program. Created Nov. 2005 by Gabriel LaFreniere.
' Please download the IDE (editor) from: http://fbide.freebasic.net
' Sub routine procedures are listed in alphabetical order.
' This source code was adapted to the 2008 0.20.0b Compiler for Windows available from:
' http://www.freebasic.net/index.php/download
' It should still be compatible with previous compilers.

Declare Sub Frame()
Declare Sub Initialization()
Declare Sub Standing_waves()
Declare Sub Text()
Declare Sub Title()

Const red   = Rgb(255,000,000), green       = Rgb(000,200,000)
Const blue  = Rgb(000,000,255), white       = Rgb(255,255,255)
Const gold  = Rgb(180,150,100), dark_gray   = Rgb(075,075,075)
Const gray  = Rgb(125,125,125), green_text  = Rgb(000,125,000)
Const buff  = Rgb(255,255,200), background  = Rgb(225,225,225)
Const cyan  = Rgb(000,100,100), light_green = Rgb(175,255,175)
Const black = Rgb(0,0,0)

Dim Shared As Integer x1, x2, x_pixel, x_left, x_right,  x_text, x_center, x_mouse
Dim Shared As Integer y1, y2, y_pixel, y_top,  y_bottom, y_text, y_center, y_mouse, y_point, y_green, y_red, y_black, y_previous
Dim Shared As Integer unidirectional, work_page, visible_page, matrix_page, granules, luminance, left_frame
Dim Shared As Integer r, g, b, j, k, radius, margin, algorithm, line_number, amplitude, top, correction
Dim Shared As Integer distance, wheel, previous_wheel, click, skipped_frames, new_position, shade
Dim Shared As Ulongint iteration                                      'huge number required.

algorithm = 1: work_page = 1: algorithm = 1: correction = 1: radius = 30
Dim Shared As Integer sphere(-radius To radius, -radius To radius)
Dim Shared As Single pi = 4 * Atn(1), lambda = 360, abs_lambda, phase, Gaussian, speed, Lagrangian(granules)
Dim Shared As Single x, y, y_sphere, x_phase, x1_squared, x2_squared, potential_squared, memory_2_squared
Dim Shared As Single sine, cosine, multiple, motion, energy, potential, memory_1, memory_2, diagonal
Dim Shared As Single dark, previous, ratio, omega, step_1, step_2, k_Dewavrin

Dim Shared As String a, choice, reflection, key, w_title
Dim Shared As String line20, line21, line22, line23, line24, line25, line26, line27, line28, line29
Dim Shared As String line30, line31, line32, line33, line34, line35, line36, line37, line38, line39a, line39b
Dim Shared As String line43, line44, line45, line47a, line47b, line47c, line48a, line48b, line48c

Screen 20,24,3: Initialization()


' ********************************************************************
' MAIN LOOP.
' ********************************************************************

Do
  Swap work_page, visible_page
  Screenset work_page, visible_page
  Pcopy 2, work_page
  Sleep 1
  For j = 0 To skipped_frames

    
' ********************************************************************
' BELOW IS MY OWN SIMPLIFICATION OF PHILIPPE DELMOTTE'S VIRTUAL
' WAVE ALGORITHM USING NEWTON'S LAWS AND VERLET'S ALGORITHM.
' ********************************************************************

    motion = motion - energy                                          'I was unaware of the link to wavelength in 2005.
    energy = energy + motion * step_1 ^ 2                             'step squared just once, finally equivalent.
                                          
    iteration += 1                                                    'to be converted in time for accurate sine curve.
  Next
  
  If algorithm = 1 Then
    For j = 0 To skipped_frames


' ********************************************************************
' ANSELME DEWAVRIN'S OCT. 2006 SIMPLIFIED ALGORITHM USING EULER'S METHOD.
' ********************************************************************

      sine  =  sine - cosine * step_1                                 'two simple program lines only. AMAZING!
      cosine = cosine + sine * step_1

    Next
  
  Else
    For j = 0 To skipped_frames


' ********************************************************************
' ANSELME DEWAVRIN'S DEC. 2006 SIMPLIFICATION OF JOCELYN MARCOTTE'S VIRTUAL WAVE ALGORITHM.
' ACCORDING TO DEWAVRIN, THIS ALGORITHM IS RELATED TO IIR (infinite impulse response) NUMERIC FILTER. 
' ********************************************************************

      potential = memory_1 * step_2 - memory_2                        'potential as sine, kinetic energy recoverable.
      memory_2  = memory_1                                            'memorizing potential's two previous states.
      memory_1  = potential

    Next
  End If


' ********************************************************************
' DISPLAYING OSCILLATING SPHERE.
' ********************************************************************

  If algorithm = 1 Then
    y_sphere = y_center - amplitude * sine                            'sphere oscillation according to Dewavrin's sine.
    Circle(x_center + lambda / 4,y_center - amplitude * cosine),10,red'red circle for cosine, Euler's method only.
  Else
    y_sphere = y_center - amplitude * potential / abs_lambda          'sphere oscillation using Marcotte's "potential".
  End If

  For x = -radius To radius                                           'printing memorized sphere's gray shades.
    For y = -radius To radius
      If sphere(x,y) Then Pset(x + x_center, y + y_sphere), sphere(x,y)
    Next
  Next

  Circle(x_center, y_sphere), radius, gray                            'sphere contour.
  Circle(x_center, y_sphere),      2, black                           'small black circle to check accuracy.
  Circle(x_center + lambda / 4, y_center - amplitude * motion), 15, 0 'black circle for motion (indicates quadrature).
  Circle(x_center + lambda / 4, y_center - amplitude * motion),  2, 0 'small black circle to check accuracy.


' ********************************************************************
' DISPLAYING ACCURATE PROGRESSIVE SINE CURVE.
' ********************************************************************

  For x = 0 To 1023                                                   'horizontal scan (screen res. 1024 x 768 pixels).
    distance = (x_center - x)
    omega =         2 * pi * distance  /     lambda                   'pulsation in radians according to distance.
    omega = omega + 2 * pi * iteration / abs_lambda                   'adding the time in radians according to iteration.
    y_previous = y: y = y_center + amplitude * Sin(omega)             'vertical scan according to amplitude.
    Line(x - 1, y_previous)-(x, y), black                             'compare Euler's method to sine curve.
  Next


' ********************************************************************
' KEYBOARD MANAGEMENT.
' ********************************************************************

  key = Inkey
  If Len(key) Then
    If Len(key) = 2 Then key = Right(key, 1) + "+" Else key = Ucase(key)
    Select Case key
      Case Chr(27), "k+": End                                         'chr(27) Escape key or Windows' X quit button.
      Case "A":  algorithm = 1
      Case "B":  algorithm = 2
      Case "C":  If correction Then correction = 0 Else correction = 1
      Case "I":  lambda = 200: multiple = 0: algorithm = 1            'initialization.
                 correction = 0: skipped_frames = 0: correction = 1
      Case "M":  key = "": Run "WaveMechanics00.exe"                  'main menu.
      Case "P":  Screenset visible_page 
                 Color red: Locate 42, 89: Print "P - Paused."
                 key = "": Sleep                                      'pause.
                 Color black: Screenset work_page, visible_page
      Case "+":  multiple = multiple * 2                              'increase skipped frames.
                 If multiple = 0 Then multiple = 1
                 If multiple > 4096 Then multiple = 4096
                 skipped_frames = multiple * abs_lambda - 1
      Case "-":  multiple = multiple / 2                              'reduce skipped frames.
                 skipped_frames = multiple * abs_lambda - 1
                 If multiple < 1 Then multiple = 0: skipped_frames = 0
      Case "=":  If skipped_frames = 0 Then                           'add or remove skipped frames.
                   skipped_frames = 1: multiple = 4096
                   skipped_frames = multiple * abs_lambda - 1
                 Else
                   multiple = 0: skipped_frames = 0
                 End If
      Case "M+": skipped_frames += 1                                  'fine tuning (righ/left arrow keys).
                 key = "": Screenset 2, 2: Color green_text
                 Locate 43, 89: Print "Press +/-/= to skip frames:"; skipped_frames
                 Screenset work_page, visible_page
      Case "K+": skipped_frames -= 1
                 If skipped_frames < 0 Then skipped_frames = 0
                 key = "": Screenset 2, 2: Color green_text
                 Locate 43, 89: Print "Press +/-/= to skip frames:"; skipped_frames; "      "
                 Screenset work_page, visible_page
      Case "R":                                                       'reset.
      Case Else: key = ""                                             'avoid initialization.
    End Select
    If Len(key) Then Initialization()
    Do: Loop While Len(Inkey)                                         'clear buffer.
  End If


' ********************************************************************
' MOUSE MANAGEMENT.
' ********************************************************************

  Getmouse x_mouse, y_mouse, wheel, click
  line_number = .5 + y_mouse / 16
  If line_number < 46 And x_mouse < 695 Then line_number = 0
  Color green_text, white
  Select Case line_number
    Case 37: Locate 37, 88                                            'Delmotte's algorithm.
             If algorithm = 2 Then
               Print line37
               If click Then algorithm = 1: Initialization()
             End If               
    Case 38: Locate 38, 88                                            'Delmotte's algorithm.
             If algorithm = 1 Then
               Print line38
               If click Then algorithm = 2: Initialization()
             End If               
    Case 39: Locate 39, 88                                            'add correction.
             If correction Then
               Print line39b
             Else
               Print line39a
             End If
             If click Then
               If correction Then correction = 0 Else correction = 1
               Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click 'avoid unnecessary initialization. 
               Initialization()
             End If
    Case 43: Locate 43, 88                                            'add or remove skipped frames.
             Print line43;
             Locate 43,116: Print skipped_frames
             If click Then
               If skipped_frames = 0 Then
                 skipped_frames = 1: multiple = 4096
                 skipped_frames = multiple * abs_lambda - 1
               Else
                 multiple = 0: skipped_frames = 0
               End If
               Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
               Initialization()
             End If
    Case 45: Locate 45, 88                                            'reset.
             Print line45
             If click Then
               Do: Getmouse x_mouse, y_mouse, wheel, click: Loop While click
               Initialization()
             End If
    Case 47: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 47, 73: Print line47c: Sleep 200 'slow.
                              If click Then Sleep 1000                'slower.
               Case Is > 472: Locate 47, 60: Print line47b
                              If click Then Run "WaveMechanics00.exe" 'main menu.
               Case Is > 318: Locate 47, 41: Print line47a
                              If click Then
                                correction = 0: skipped_frames = 0    'initialization.
                                lambda = 200: algorithm = 1: correction = 1: Initialization()
                              End If
             End Select
    Case 48: Select Case x_mouse
               Case Is > 700
               Case Is > 576: Locate 48, 73: Print line48c;
                              If click Then Run "WaveMechanics02.exe" 'next program.
               Case Is > 472: Locate 48, 60: Print line48b;
                              If click Then End                       'quit.
               Case Is > 318: Locate 48, 41: Print line48a;
                              If click Then Run "WaveMechanics00.exe" 'previous program.
             End Select
  End Select


' ********************************************************************
' WAVELENGTH SELECTION.
' ********************************************************************

  If y_mouse < y_center + 18 And y_mouse > y_center - 18 Then
    Line(0, y_center - 16)-(1023, y_center + 16), black, b
    Line(0, y_center - 15)-(1023, y_center + 15), white, bf
    Line(x_center, y_center - 15)-(x_center + lambda / 4, y_center + 15), gray, bf
    Line(x_center, y_center - 16)-(x_center + lambda, y_center + 16), black, b
    Color black, white
    If lambda > 0 Then Locate 18, 44 Else Locate 18, 67
    Print "Lambda: ";: Print lambda; " pixels."
    new_position = (x_mouse - x_center) * 4: If Abs(new_position) < 24 Then new_position = 24
    If click Then                                                     'click.
      lambda = new_position
      multiple = 0: skipped_frames = 0
      Initialization()
    End If
    If wheel = -1 Then previous_wheel = wheel
    If wheel - previous_wheel Then                                    'mouse wheel.
      lambda = lambda + wheel - previous_wheel
      If lambda > 0 And lambda < 24 Then lambda = 24
      If lambda < 0 And lambda >-24 Then lambda =-24
      multiple = 0: skipped_frames = 0
      Initialization()
    End If
  End If

  Color black, background
  Locate 42, 113: Print iteration                                     'iteration number up to billions.
  If algorithm = 1 Then
    Locate 32, 96: Print Using "##.##########"; -Abs(sine)            'always negative to avoid formatting malfunction.
    If sine > 0 Then Locate 32, 96: Print " "                         'removing negative sign when positive.
  End If
  If wheel = -1 Then Else previous_wheel = wheel
Loop


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
' ********************************************************************

Sub Frame()
  margin = 14                                                         'internal margin.
  x_left  = 8 * x_left - 8 - margin                                   'final dimensions. Lateral margins are larger.
  x_right = x_left + 8 + 8 * x_text + margin
  y_top = top * 16 - 16 - .6 * margin
  y_bottom = y_top + 16 * y_text + 2 * .5 * margin
  
  Line (x_left, y_top)-(x_right + 1, y_bottom + 1), gray, B
  Line (x_left + 1, y_top + 1)-(x_right, y_bottom), black, B
  Line (x_left + 1, y_bottom)-(x_right, y_bottom), white
  Line (x_left, y_bottom + 1)-(x_right + 1, y_bottom + 1), white
  Line (x_right, y_top + 1)-(x_right, y_bottom), white
  Line (x_right+ 1, y_top)-(x_right + 1, y_bottom + 1), white
End Sub


'*********************************************************************
' GRAPHICS.
' ********************************************************************

Sub Graphics()
  For x = -radius To radius                                           'memorizing a matrix sphere.
    x1_squared = x ^ 2
    x2_squared = (x - 1000 + radius) ^ 2
    For y = -radius To radius
      potential_squared = y ^ 2
      memory_2_squared = (y - 1000 + radius) ^ 2
      If Sqr(x1_squared + potential_squared) <= radius Then
        diagonal = Sqr(x2_squared + memory_2_squared)
        shade = 4.5 * (diagonal - 1414 + 2 * radius + 10)             'gray shades for the sphere.
        If shade < 0 Then shade = 0 Else If shade > 255 Then shade = 255
        sphere(x,y) = Rgb(shade,shade,shade)
        If sphere(x,y) = 0 Then sphere(x,y) = 1                       '1 as printable black.
      End If
    Next
  Next
  
  For x = 0 To 512 Step abs_lambda / 4
    Line(x_center + x, y_center - 10)-(x_center + x, y_center), gray  'lambda / 4 marks.
    Line(x_center - x, y_center - 10)-(x_center - x, y_center), gray
  Next
  
  For x = 0 To 512 Step abs_lambda
    Line(x_center + x, y_center - 16)-(x_center + x, y_center), black 'lambda marks.
    Line(x_center - x, y_center - 16)-(x_center - x, y_center), black
  Next
  
  Line(0, y_center)-(1023, y_center), gray
  Line(x_center + lambda / 4, y_center - amplitude)-(x_center + lambda / 4, y_center + amplitude), gray
  Line(-1, y_center - amplitude)-(1024, y_center + amplitude), white, b
  Line(x_center, y_center - amplitude)-(x_center + lambda, y_center + amplitude), gray, b
End Sub


'*********************************************************************
' INITIALIZATION.
' ********************************************************************

Sub Initialization()
  Windowtitle "WaveMechanics01. Oscillations. Anselme Dewavrin's algorithms. Philippe Delmotte and Jocelyn Marcotte's virtual wave medium."
  abs_lambda = Abs(lambda)
  If correction Then
       step_1 = 2 * Sin(pi / lambda)                                  'Devavrin's step was simplified in April 2008.
  Else step_1 = 2 * pi / abs_lambda                                   'basic step in radians for Delmotte's algorithm.
  End If
  step_2 = 2 - step_1 ^ 2                                             'conversion for IIR algorithm.
  memory_1 = 0
  memory_2 = 2 * pi
  amplitude = 120
  y_center = 279
  x_center = 512
  iteration = 0
  sine   = 0
  cosine = Cos(Sqr(2 - (Sin(4 * pi / lambda) / Sin(2 * pi / lambda))) / 2)'Jocelyn Marcotte's initialization (april 2008).
  energy = 0
  motion = 1
  energy = energy + step_1 ^ 2 * motion                               'needs initial synchronization.
  Screenset 2, visible_page: Color black, background: Cls
  Title()
  Graphics()
  Text()
End Sub


'*********************************************************************
' TEXT SECTION.
' ********************************************************************

Sub Text()
  x_text = Len("energy = energy + motion / step ^ 2 ")                'text width (pixels = x * 8).
  y_text = 2                                                          'number of lines (pixels = y * 16).
  top = 29                                                            'upper limit.
  x_left= 89                                                          'limit on the left hand side: "Locate top, left".
  Locate top, x_left: ? "motion = motion - energy"
  Locate    , x_left: ? "energy = energy + motion * step ^ 2"
  Frame()
  
  If algorithm = 1 Then
    top = 34
    x_left= 89
    Locate top, x_left: ? "sine  =  sine - cosine * step"             'Dewavrin's algorithm derived from Delmotte's.
    Locate    , x_left: ? "cosine = cosine + sine * step"
    Locate 40,114: Print Using "###.#########"; step_1
    a = "step = 2 * sin(pi / lambda)"
    Locate 32, 89: Print "sine ="
  
    If correction Then
      Locate 40, 89: Print "Step using formula below: ";
    Else
      Locate 40, 89: Print "step =  2 * pi / lambda = ";
    End If
    Else
    y_text = 3: top = 33: x_left= 89
        potential = step_2 * memory_1 - memory_2                      'potential energy as sine, so kinetic recoverable.
        memory_2 = memory_1                                           'memorizing potential's two previous states.
        memory_1 = potential
    Locate top, x_left: ? "potential = memory1 * step - memory2"      'infinite impulse filter (Marcotte).
    Locate    , x_left: ? "memory2 = memory1"
    Locate    , x_left: ? "memory1 = potential"
    Locate 40,114: Print Using "###.#########"; step_2
    a = "step = 2 - (2 * sin(pi / lambda)) ^ 2"
    If correction Then
      Locate 40, 89: Print "Step using formula below: ";
    Else
      Locate 40, 89: Print "step = 2-(2*pi/lambda)^2 =";
    End If
  End If  
  Frame()
  x_text = Len(a): top = 45: x_left= 25: y_text = 1
  Locate top, x_left: ? a
  Frame()
  Locate 4, 3
  ?"Robert Hooke wrote in 1678: ® Ut tensio sic vis ¯ (As the extension, so the force). Any mechanical oscillating system works ":Locate,3
  ?"like a pendulum. Firstly, motion as kinetic energy is stored as energy inside a field of force. Then this stored energy,    ":Locate,3
  ?"which exerts a force like a spring, is converted again into motion. Because energy cannot be created or destroyed, this     ":Locate,3
  ?"process may go on endlessly on condition that force and extension are proportional according to Hooke's law. Otherwise,     ":Locate,3
  ?"energy is progressively transferred into heat or waves such as sound. The important point is that energy transmission in    ":Locate,3
  ?"a step-by-step process introduces quantum properties."
  Locate 28, 3
  ?"Mr. Philippe Delmotte from Lille, France, invented a computerized virtual medium   ": Locate, 3
  ?"for waves in June 2005. I simplified Mr. Delmotte's algorithm in November 2005 in  ": Locate, 3
  ?"order to obtain pure oscillations without waves. The recent algorithm is:"
  ?: Locate, 3
  If algorithm = 1 Then
    ?"Mr. Delmotte deduced his algorithm from Verlet's one. He also took Newton's laws ": Locate, 3
    ?"into account. However, Mr. Anselme Dewavrin also from Lille, France, informed me ": Locate, 3
    ?"in October 2006 that my iterative calculus was clearly similar to Euler's method ": Locate, 3
    ?"using sine and cosine. As far as I know, he truly invented this simplification:  "
  Else
    ?"Mr. Jocelyn Marcotte invented his own algorithm for computerized virtual waves in": Locate, 3
    ?"Jan 2006. In Dec 2006, Mr. Dewavrin also discovered that Mr. Marcotte's algorithm": Locate, 3
    ?"was similar to the IIR (infinite impulse response) electronic filter. Once again,": Locate, 3
    ?"he succeeded in simplifying the algorithm for oscillations and he obtained this: "
  End If                              
  Locate 37, 3
  ?"The sphere above follows Mr. Delmotte's sine or Mr. Marcotte's potential. Amazingly,":Locate, 3
  ?"all three algorithms introduce exactly the same quantum anomaly. It becomes clearly": Locate, 3
  ?"visible after millions of iterations, especially for shorter wavelengths. In order ": Locate, 3
  ?"to accelerate the process, you may activate the oscilloscope by pressing the + key.": Locate, 3
  ?"It turns out that mechanical waves truly behave this way when the medium transmits ": Locate, 3
  ?"energy step by step. Sine values are accurate using Mr. Marcotte's initialization  ": Locate, 3
  ?"(April 2008) and Mr. Dewavrin's revisited step (October 2006), which is given by:"
  
  line37  = " A - Philippe Delmotte's algorithm.     "
  line38  = " B - Jocelyn Marcotte's algorithm.      "
  line39a = " C - Add step correction.               "
  line39b = " C - Remove step correction.            "
  line43  = " Press +/-/= to skip frames:            "
  line45  = " R - Reset.                             "
  line47a = " I - Initialize    "
  line47b = " M - Menu    "
  line47c = " Slow          "
  line48a = " Previous Program  "
  line48b = " Quit (Esc.) "
  line48c = " Next Program  "
  
  Locate 19, 82: Print "Click here to select wavelength: ";: Print lambda; " pixels."
  Locate 42, 89: Print "P - Pause.   Iteration:"
  
  Locate 44, 89: Print "Fine tuning: use left/right arrow keys."
  
  Color green_text
  Locate 37, 88: Print line37
  Locate 38, 88: Print line38
  Locate 39, 88: If correction Then Print line39b Else Print line39a
  Locate 43, 88: Print line43;
  Locate 43,116: Print skipped_frames
  Locate 45, 88: Print line45
  Locate 47, 42: Print "I - Initialize     M - Menu     Slow"
  Locate 48, 42: Print "Previous Program   Quit (Esc.)  Next Program";
  Color gray
  Locate 47, 3:  Print "Thanks to the creators of FreeBASIC."
  Locate 48, 3:  Print "Gabriel LaFreniere  glafreniere.com";
  Locate 47, 89: Print "May 12, 2009. This program may be"
  Locate 48, 89: Print "freely distributed, copied or modified.";
  Color blue
  If algorithm = 1 Then
    Locate 37, 88
    Print line37
  Else
    Locate 38, 88
    Print line38
  End If
  Color black
  Screenset work_page, visible_page
End Sub


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Sub Title()
w_title = "Oscillations"
  Locate 1, 1: Print w_title
  For x1 = 0 To 8 * Len(w_title)
    x2 = x_center + 2 * x1 - 8 * Len(w_title)
    For y1 = 1 To 14
      If Point(x1, y1) = black Then
        y2 = 2 * y1 + 2
        Line(x2, y2)-(x2, y2 + 1), gold
        If Point(x1 + 1, y1 - 1) = black Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), gold
        If Point(x1 + 1, y1 + 0) = black Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), gold
        If Point(x1 + 1, y1 + 1) = black Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), gold
      End If
    Next
    If (x1+1) Mod 8 Then Else Line(x2+1, 0)-(x2+1, 34), background    'separate invasive characters such as capital M.
  Next
  Line(0, 0)-(8 * Len(w_title), 14), background, bf                   'matrix title erased.
  For x1 = x_center - 8 * Len(w_title) To x_center + 8 * Len(w_title) 'adding light and shades.
    For y1 = 0 To 34
      If Point(x1, y1) = gold And Point(x1-1, y1-1) = background Then Pset(x1-1, y1-1), buff
      If Point(x1, y1) = gold And Point(x1+1, y1+1) = background Then Pset(x1+1, y1+1), black
    Next
  Next
  For x = 512 - 8 * Len(w_title) To 512 + 8 * Len(w_title)            'adding luster.
    For y = 4 To 32
      If Point(x, y) = gold Then dark = 9 * Abs(18 - y): Pset(x, y), Rgb(240 - dark, 200 - dark, 120 - dark)
    Next
  Next
End Sub

