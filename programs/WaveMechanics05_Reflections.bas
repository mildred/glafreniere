' This is a FreeBasic program. Created June 2, 2008 by Gabriel LaFreniere. Updated August 30, 2008.
' Please download the editor/compiler from: http://fbide.freebasic.net
' Subs are listed in alphabetical order.
x.width = 1024: y.height = 400: lambda = 30: thickness = 3 * lambda
Dim As Double damping(thickness), distance, orthogonal, diagonal, total, difference
Dim As Double trend(x.width, y.height), present(-1 To x.width+1, -1 To y.height+1), past(x.width, y.height)
Dim As Double pi = 4 * Atn(1), phase, angle, radian, factor, Gaussian, x, y, t, speed, h.diff, v.diff
work.page = 1: choice$ = "B"
Screen 20,24,3: Gosub Initialization


'*********************************************************************
' MAIN LOOP.
'*********************************************************************

Do
  If display = 0 Then
    Swap work.page, visible.page
    Screenset work.page, visible.page
    Pcopy matrix.page, work.page
  End If


'*********************************************************************
' ANTI-REFLECTIVE EDGES.
'*********************************************************************

'  present(-1,-1) = (present(-1,0) + present(0,-1)) / 2               'special procedure for the 4 vertices.
'  present(x.width+1,-1) = (present(x.width,-1) + present(x.width+1,0)) / 2
'  present(-1,y.height+1) = (present(-1,y.height) + present(0,y.height+1)) / 2
'  present(x.width+1,y.height+1) = (present(x.width+1,y.height) + present(x.width,y.height+1)) / 2
  

  For x = 0 To x.width                                                'upper edge alternative options.
    
'    present(x,-1) = 0                                                'default hard reflection because present = 0.
'    present(x,-1) = trend(x,0)                                       'soft reflection.
'    present(x,-1) = present(x,0)                                     'full damping for 90°, hard reflection for 0°.
'    present(x,-1) = (present(x,0) + trend(x,0)) / 2                  'full damping for 30°, soft reflection for 90°.
'    present(x,-1) = (present(x,0) + present(x,1)) / 2                'full damping for 40°, soft reflection for 90°.

'    total = present(x,0) + present(x,1) + present(x,2) + present(x,3) + present(x,4) + present(x,5) + present(x,6) + present(x,7)
'    present(x,-1) = total / 8                                        'soft reflection because:
                                                                      'present(x,0) = present(x,7) for waves moving
  Next                                                                'horizontally. This is equivalent to no reflection.

  For x = 0 To x.width
    For y = 0 To thickness                                            'progressive damping, upper edge.
      present(x,y)          = present(x,y)          * damping(y)
      trend(x,y)            = trend(x,y)            * damping(y)
      present(x,y.height-y) = present(x,y.height-y) * damping(y)      'lower edge.
      trend(x,y.height-y)   = trend(x,y.height-y)   * damping(y)
    Next

'   present(x,-1) = present(x,0)                                      'upper edge damping (works well for 90° only).
'   present(x,y.height+1) = present(x,y.height)                       'lower edge (90°).
  Next

  For y = 0 To y.height
    For x = 0 To thickness                                            'progressive damping, left.
      present(x,y) = present(x,y) * damping(x)
      trend(x,y)   = trend(x,y)   * damping(x)
    Next

'   present(-1, y) = present(0, y)                                    'left edge damping (works well for 90° only).
    present(x.width+1, y) = present(x.width, y)                       'right (90°) 
  Next


'*********************************************************************
'SCREEN AND FILTER PROCEDURES COMPATIBLE WITH THE 8-GRANULE ALGORITHM.
'*********************************************************************

  x = 90: min = 150: max = 300
  Line(x, min)-(x, max), white                                        'vertical white line.
  For y = min To max                                                  'FULL DAMPING for 90° incidence only.
     trend(x,y) = present(x+1,y)                                      'also works for orthogonal standing waves.
     trend(x-1,y) = 0
  Next
  
  x = 900: min = 0: max = y.height / 2
  Line(x, min)-(x, max), blue                                         'vertical blue line.
  For y = min To max                                                  'TRANSPARENT MIRROR with hard reflection.
    trend(x, y) = sqr(.5) * trend(x, y)                               'the square root of the required transparency.

'    present(x,y) = .5 * present(x,y)                                 'this alternative needs 2 actions, but the
'    trend(x,y) =   .5 * trend(x,y)                                   'transparency factor is correct here.
  Next                                                                '(works on both sides).
  
  x = 900: min = y.height / 2: max = y.height
  Line(x, min)-(x, max), purple                                       'vertical violet line.
  For y = y.height / 2 To y.height                                    'adjustable AMPLIFICATION from .9 to 1 approx.
    trend(x+1, y) = 1 * trend(x, y)                                   'may also produce adjustable TRANSPARENCE.
  Next                                                                'variable soft/hard reflection on the other side.

  y = 40: min = 100: max = 350
  Line(min, y+50)-(min+50, y), red                                    'upward 45° RED line.
  Line(min+50, y)-(max-100, y), red                                   'horizontal RED line.
  Line(max-100,y)-(max, y+50), red                                    'downward 22.5° RED line.
  y = 90
  For x = min To max                                                  'SOFT REFLECTION.
    if x < min+50 then
      trend(x,  y) = trend(x+1,y+1)
      trend(x+1,y) = trend(x+2,y+1)                                   '45° mirror.
      trend(x,y-1) = 0
      trend(x,y-2) = 0
      y -= 1
    elseif x < max - 100 then                                         'horizontal mirror.
      trend(x,y)   = trend(x, y+1)                                    'one row for horizontal or vertical reflectors.
      trend(x,y-1) = 0                                                'hard reflection on the other side.
    else
      trend(x, y)  = (trend(x,y+1) + trend(x-1,y+1))/2                '22.5° mirror.
      trend(x,y+1) = (trend(x,y+2) + trend(x-1,y+2))/2      
      trend(x,y-1) = 0
      trend(x,y-2) = 0
      if not x mod 2 then y += 1
    end if

'  For x = min To max
'    trend(x, y) = .9 * trend(x, y+1)                                 'VARIABLE REFLECTION . Reflection is soft with
'    trend(x, y-1) = 0                                                'a factor of 1, but it becomes hard around .9.
'  Next
                                                                      'This actually adjusts the period and this
                                                                      'property could be usable for zonal mirrors
                                                                      'working like diffractive lenses.
  Next

  y = 310: min = 150: max = 350
  Line(min, y)-(max, y), green                                        'horizontal green line.
  For x = min To max                                                  'HARD REFLECTION on both faces.
    trend(x,y) = 0                                                    'needs one simple action.
'   trend(x,y) = present(x,y)                                         '(alternative).

'    present(x,y)   = 0                                               'unstable antireflection to examine.
'      trend(x,y+1) = 0
  Next

  y = 310: min = 500: max = 800
  Line(min,y)-(max,y), cyan                                           'horizontal cyan line.
  For x = min To max                                                  'NO REFLECTION. The incidence angle is needed.
    If x - x.source Then                                              'apparently, the middle point between the null
      angle = Atn((y - y.source) / ((x - x.Source) / 1.732))          'reflection and the soft reflection is 30°,
     Else angle = pi / 2                                              'not 45°. This means that the true angle must
    End If                                                            'be compressed according to a 1,732 factor, which
   trend(x, y) = present(x, y-1) * Sin(angle) ^ 2 + trend(x, y-1) * Cos(angle) ^ 2' is the square root of 3.
   trend(x, y+1) = 0                                                  'the angle modifies the present vs. trend ratio.
  Next

  Line(x.source-5,200)-(x.source+5, 200), gray
  Line(x.source,195)-(x.source, 205), gray

'*********************************************************************
' JOCELYN MARCOTTE'S VIRTUAL WAVE ALGORITHM (CREATED JANUARY 2006).    "THE PAST IS A GUIDE TO THE FUTURE"
'*********************************************************************

  For x = 0 To maximum: For y = 0 To y.height                         'updating previous states.
    past(x,y)  = present(x,y)
    present(x,y) = trend(x,y)
  Next: Next
  For x = 0 To maximum: For y = 0 To y.height
    orthogonal = (present(x-1, y  ) + present(x,   y-1) + present(x,   y+1) + present(x+1, y  )) / 2
    diagonal   = (present(x-1, y-1) + present(x-1, y+1) + present(x+1, y-1) + present(x+1, y+1)) / 4
    trend(x,y) = orthogonal + diagonal - present(x,y) - past(x,y)
'   trend(x,y) = orthogonal/3 + diagonal/3 + present(x,y) - past(x,y) 'PLUS present produces a c = .6 speed instead of 1.
  Next: Next


'*********************************************************************
'                            IMPORTANT
'*********************************************************************
' The more accurate diagonal correction shown above was elaborated by
' Mr. Jocelyn Marcotte himself in 2006. Surprizingly, it also yields
' faster results because the wave speed is accelerated from .707 to
' 1 pixel per cycle exactly. The basic trend may also be given by:

' trend(x,y) = (present(x-1,y) + present(x,y-1) + present(x,y+1) + present(x+1,y)) / 2 - past(x,y)

' This calculus is much simpler, yet the diagonal propagation is
' delayed to the next cycle. This is especially annoying for shorter
' wavelenghts. It should also be emphasized that the "c" speed
' normalized to 1 is of the utmost importance for experiments
' involving motion. The Doppler effect becomes much easier to deal
' with, and this is especially true for the Lorentz transformations.


'*********************************************************************
' WAVES IN COLOR.
'*********************************************************************
    
  If display = 0 Then
    For x = 0 To x.width: For y = 0 To y.height
      luminance1 = Abs(20 * present(x,y))
      b = luminance1 / 2
      If b > 255 Then b = 255
      If luminance1 > 255 Then
        luminance2 = luminance1 - 255
        If luminance2 > 255 Then luminance2 = 255
        luminance1 = 255
      Else luminance2 = 0
      End If
      If present(x,y) > 0 Then
        r = luminance2
        g = luminance1
      Else
        r = luminance1
        g = luminance2
      End If
         Pset(x,y), Rgb(r,g,b)
    Next: Next
  End If

  If display = 0 Then                                                 '2 skipped frames.

    For x = 0 To x.width Step 2                                       'graphics.
      Line(x-2, previous)-(x, y.graphics - 2 * present(x,y.source)), black
      previous = y.graphics - 2 * present(x,y.source)
      distance = Abs(x - x.source)
      y = 285 / Sqr(distance)
      If distance > .375 * lambda Then Pset(x,y.graphics - y), black: Pset(x,y.graphics + y), black
    Next


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

    key$ = Inkey
    If Len(key$) Then
      If Len(key$) = 2 Then key$ = Right(key$, 1) + "+" Else key$ = Ucase(key$)
      Select Case key$
        Case Chr(27), "k+": End                                       'escape key or Windows' X quit button.
        Case "A": choice$ = "A"
        Case "B": choice$ = "B"
        Case "M": Run "WaveMechanics00.exe"                           'main menu.
        Case "P": Screenset visible.page                              'pause.
                  Color red, background: Locate 42, 89
                  Print "P - Paused. Press any key to resume.   "
                  key$ = "": Sleep
                  Color black: Screenset work.page, visible.page
        Case "R":                                                     'reset via initialization.
        Case "S"
          For x = 0 To maximum: For y = 0 To y.height                 'force standing waves.
           trend(x,y) = 0
           present(x,y) = .1 * present(x,y)
          Next: Next
          key$ = ""
        Case Else: key$ = ""                                          'avoid initialization.
      End Select
      If Len(key$) Then Gosub Initialization
      Do: Loop While Len(Inkey)                                       'clear buffer.
    End If
  
  
  '*********************************************************************
  ' MOUSE MANAGEMENT.
  '*********************************************************************
  
    Getmouse x.mouse, y.mouse, wheel, click
    line.number = .5 + y.mouse / 16
    If line.number < 46 And x.mouse < 695 Then line.number = 0
    Color green.text, white
    Select Case line.number

      Case 44: Locate 44, left.
               Print line44$
               If click Then Gosub Initialization
      Case 47: Select Case x.mouse
                 Case Is > 700
                 Case Is > 576: Locate 47, 73: Print line47c$: Sleep 200'slow.
                                If click Then Sleep 1000              'slower.
                 Case Is > 472: Locate 47, 60: Print line47b$
                                If click Then Run "WaveMechanics00.exe"'main menu.
                 Case Is > 318: Locate 47, 41: Print line47a$
                                If click Then                         'initialization.
                                  choice$ = "B"
                                  Do: Getmouse a,b,c, click: Loop While click
                                  Gosub Initialization
                                End If
               End Select
      Case 48: Select Case x.mouse
                 Case Is > 700
                 Case Is > 576: Locate 48, 73: Print line48c$;
                                If click Then Run "WaveMechanics06.exe"'next program.
                 Case Is > 472: Locate 48, 60: Print line48b$;
                                If click Then End                     'quit.
                 Case Is > 318: Locate 48, 41: Print line48a$;
                                If click Then Run "WaveMechanics04.exe"'previous program.
               End Select
    End Select

    If maximum < x.width Then Line(maximum,180)-(maximum,220), gray
  End If

  If maximum < x.width Then maximum += 1
  iteration += 1
  display += 1
  If display = 3 Then display = 0                                     '2 skipped frames.
  If choice$ = "B" Then Gosub Choice.B
Loop


'*********************************************************************
' CHOICE A - GAUSSIAN IMPULSE AND RESULTING RICKER WAVELET.
'*********************************************************************

Choice.A:
For i = 0 To x.width: For j = 0 To y.height
  x = Sqr((i - x.source) ^ 2 + (j - y.height / 2) ^ 2)
  Gaussian = 1.005 ^ (-x ^ 2)
  present(i,j) = 120 * Gaussian
  trend(i,j) = present(i,j)
Next: Next
Return


'*********************************************************************
' CHOICE B - COSINE-DAMPED SINUSOIDAL IMPULSE.
'*********************************************************************

Choice.B:
If iteration > 4 * lambda -1 Then Return
phase = 2 * pi * iteration / lambda

For x.coord = -lambda / 2 To lambda / 2
  x.squared = x.coord^2
  For y.coord = -lambda / 2 To lambda / 2
    distance = Sqr(x.squared + y.coord^2)
    If distance < lambda / 2 Then
      x = 2 * pi * distance / lambda
      If x Then y = Sin(x) / x Else y = 1
      trend(x.source+x.coord, y.source+y.coord) = trend(x.source+x.coord, y.source+y.coord) + (13000 / lambda^2.5) * y * Cos(phase)
    End If
  Next
Next
Return


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
'*********************************************************************

Frame:
margin = 10
x.left  = 8 * left. - margin - 4
x.right = 8 * left. + 8 * x.text + margin - 6
y.top = top * 16 - margin - 16
y.bottom = y.top + 16 * y.text + 2 * margin - 2

Line (x.left, y.top)-(x.right + 1, y.bottom + 1), gray, B
Line (x.left + 1, y.top + 1)-(x.right, y.bottom), black, B
Line (x.left + 1, y.bottom)-(x.right, y.bottom), white
Line (x.left, y.bottom + 1)-(x.right + 1, y.bottom + 1), white
Line (x.right, y.top + 1)-(x.right, y.bottom), white
Line (x.right+ 1, y.top)-(x.right + 1, y.bottom + 1), white
Return


'*********************************************************************
' INITIALIZATION.
'*********************************************************************

Initialization:
Windowtitle "WaveMechanics05 - Reflection Procedures - Waves in 2 Dimensions"
matrix.page = 2
x.center = x.width / 2
y.center = y.height / 2
x.source = 250
y.source = y.center
maximum = x.source + lambda / 2
y.graphics = y.height + 100
iteration = 0
t = Timer
If unidirectional Then amplitude = 40 Else amplitude = 80
red  =        Rgb(255,0,0)
blue =        Rgb(0,0,255)
cyan =        Rgb(0,150,150)
gold =        Rgb(180,150,100)
gray =        Rgb(125,125,125)
buff =        Rgb(255,255,200)
green =       Rgb(0,200,0)
white =       Rgb(255,255,255)
purple =      Rgb(255,0,255)
dark.gray  =  Rgb(75,75,75)
green.text =  Rgb(0,125,0)
background =  Rgb(225,225,225)
light.green = Rgb(175,255,175)
Screenset matrix.page, matrix.page
Color black, background: Cls
For x = 0 To x.width: For y = 0 To y.height                           'erasing previous data.
  past(x,y)    = 0
  present(x,y) = 0
  trend(x,y)   = 0
Next: Next
For x = -1 To x.width + 1
present(x, -1) = 0
present(x, y.height + 1) = 0
Next
For y = -1 To x.height + 1
present(-1, y) = 0
present(x.width + 1, y) = 0
Next
damping(0) = 0
For x = 1 To thickness                                                'progressive damping.
' damping(x) = x ^.02 / thickness ^ .02                               'pure exponential, seems a bit less efficient.
  damping(x) = (Sqr(thickness^2-(thickness-x)^2)/thickness)^.1        '10th root of a circle sagitta.
Next                                                                  'this method is the most perfect, but it needs a
                                                                      'wide damping zone which becomes unusable. In
Select Case choice$                                                   'addition, the wavelength is critical.
  Case "A": Gosub Choice.A: maximum = 200
End Select
Gosub Title
Gosub Text
Line(0,0)-(x.width, y.height), black, bf
Line(0,y.graphics)-(x.width, y.graphics), gray
Line(x.source,y.graphics-20)-(x.source, y.graphics+20), gray
Line(x.source-.5*lambda,y.graphics-10)-(x.source-.5*lambda, y.graphics+10), gray
Line(x.source+.5*lambda,y.graphics-10)-(x.source+.5*lambda, y.graphics+10), gray
Pcopy matrix.page, work.page
Return


'*********************************************************************
' TEXT.
'*********************************************************************

Text:
line47a$ = " I - Initialize.   "
line47b$ = " M - Menu.   "
line47c$ = " Slow.         "
line48a$ = " Previous Program. "
line48b$ = " Quit (Esc.)."
line48c$ = " Next Program. "

Locate 37
Locate, 3: Print "WHITE line: full damping. Works well for 90";CHR$(248);" incidence only."
Locate, 3: Print "RED line: soft reflection. A bit more complicated for 45";CHR$(248);" reflectors"
Locate, 3: Print "GREEN line: hard reflection on both sides. Very easy to obtain."
Locate, 3: Print "CYAN line: adjustable damping. The incidence angle must be known, though."
Locate, 3: Print "BLUE line: adjustable transparent mirror. Produces a hard reflection."
Locate, 3: Print "VIOLET line: adjustable transparence or amplification up to 2x. Produces a hard"
Locate, 3: Print "             or a soft reflection according to the amplification rate."
Locate, 3: Print "EDGES (except on the right hand side): progressive all azimut damping."
Locate, 3: Print "RIGHT edge: full damping for a 90";CHR$(248);" angle only."

Locate 43,89: Print "P - Pause."
Locate 44,89: Print "R - Reset."

Color green.text
x.text = Len(line21$) - 1                                             'text width (pixels = x * 8).
y.text = 25                                                           'number of lines (pixels = y * 16).
top = 5                                                               'upper limit.
left.= 84                                                             'limit on the left hand side: "Locate top, left".
'Gosub frame
Locate 21,left.: Print line21$
Locate 22,left.: Print line22$
Locate 47,42: Print "I - Initialize.    M - Menu.    Slow."
Locate 48,42: Print "Previous Program.  Quit (Esc.). Next Program.";

Color gray
Locate 47, 3: Print "Thanks to the creators of FreeBASIC."
Locate 48, 3: Print "Gabriel LaFreniere  glafreniere.com";
Locate 47,89: Print "August 30, 2008. This program may be"
Locate 48,89: Print "freely distributed, copied or modified.";

Color blue
Select Case choice$
  Case "A": Locate 21, left.:  Print line21$
  Case "B": Locate 22, left.:  Print line22$
End Select

Color black
Return


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Title:
title$ = "Waves in 2 Dimensions"
Locate 1,1: Print title$
center = 1024 / 2
y.title = 400
For x1 = 0 To 8 * Len(title$)
  x2 = center + 2 * x1 - 8 * Len(title$)
  For y1 = 1 To 14
    If Point(x1, y1) = black Then
      y2 = y.title + 2 * y1 + 2
      Line(x2, y2)-(x2, y2 + 1), gold
      If Point(x1 + 1, y1 - 1) = 0 Then Line(x2 + 1, y2 - 1)-(x2 + 1, y2 + 0), gold
      If Point(x1 + 1, y1 + 0) = 0 Then Line(x2 + 1, y2 + 0)-(x2 + 1, y2 + 1), gold
      If Point(x1 + 1, y1 + 1) = 0 Then Line(x2 + 1, y2 + 1)-(x2 + 1, y2 + 2), gold
    End If
  Next
  If (x1+1) Mod 8 Then Else Line(x2+1, y.title)-(x2+1, y.title + 34), background'separate invasive characters such as capital M.
Next
Line(0, 0)-(8 * Len(title$), 14), background, bf                      'matrix title erased.
For x1 = center - 8 * Len(title$) To center + 8 * Len(title$)         'adding light and shades.
  For y1 = y.title To y.title + 34
    If Point(x1, y1) = gold And Point(x1-1, y1-1) = background Then Pset(x1-1, y1-1), buff
    If Point(x1, y1) = gold And Point(x1+1, y1+1) = background Then Pset(x1+1, y1+1), black
  Next
Next
For x1 = center - 8 * Len(title$) To center + 8 * Len(title$)         'adding luster.
  For y1 = y.title + 4 To y.title + 32
    If Point(x1, y1) = gold Then dark = 9 * Abs(18 + y.title - y1): Pset(x1, y1), Rgb(240 - dark, 200 - dark, 120 - dark)
  Next
Next
Return
