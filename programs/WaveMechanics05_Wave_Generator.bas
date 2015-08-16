' This is a FreeBasic program. Created June 2, 2008 by Gabriel LaFreniere. Updated July 19, 2008.
' Please download the editor/compiler from: http://fbide.freebasic.net
' Subs are listed in alphabetical order.
x.width = 490: y.height = 490
Dim As Double trend(x.width, y.height), present(-1 To x.width+1, -1 To y.height+1), past(x.width, y.height)
Dim As Double distance, orthogonal, diagonal, total, difference, amplitude
Dim As Double pi = 4 * Atn(1), phase, angle, radian, factor, Gaussian, x, y, t, speed
lambda = 100: work.page = 1
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
' ANTIREFLECTIVE EDGES.
'*********************************************************************

' For x = -1 To x.width + 1                                           'simpler, but works best for 90° incidence only.
'   present(x,-1) = present(x,0)                                      'top edge.
'   present(x,y.height+1) = present(x,y.height)                       'bottom edge.
' Next

  For x = 0 To x.width                                                'NULL REFLECTION on condition that the incidence
    If x - x.source Then                                              'angle is known. The middle point for equal trend
      angle = Atn(y.source / ((x - x.Source) / 1.732))                'and present values appears to be about 30°, not
    Else angle = pi / 2                                               '45°. Thus, the angle must be "compressed"
    End If                                                            'according to a 1.732 factor or Sqr(3) approx.
   trend(x,0)=present(x,1)*Sin(angle)^2+trend(x,1)*Cos(angle)^2       'adjusting trend and present ratio to angle.
   trend(x,y.height)=present(x,y.height-1)*Sin(angle)^2+trend(x,y.height-1)*Cos(angle)^2'bottom.
   trend(0,x)=present(1,x)*Sin(angle)^2+trend(1,x)*Cos(angle)^2       'left, transposition vertically (same angle).
   trend(x.width,x)=present(x.width-1,x)*Sin(angle)^2+trend(x.width-1,x)*Cos(angle)^2'right.
  Next


'*********************************************************************
' JOCELYN MARCOTTE'S OPTIMIZED 2-D WAVE ALGORITHM (CREATED IN 2006).   "THE PAST IS A GUIDE TO THE FUTURE"
'*********************************************************************

    For x = x.center - maximum To x.center + maximum                  'updating previous states.
      For y = 0 To y.height
        past(x,y)  = present(x,y)
        present(x,y) = trend(x,y)
    Next: Next
    For x = x.center - maximum To x.center + maximum                  'trend extrapolation.
      For y = 0 To y.height
      orthogonal = present(x-1, y  ) + present(x,   y-1) + present(x,   y+1) + present(x+1, y  )
      diagonal   = present(x-1, y-1) + present(x-1, y+1) + present(x+1, y-1) + present(x+1, y+1)
      trend(x,y) = .5 * orthogonal + .25 * diagonal - present(x,y) - past(x,y)
    Next: Next

  If maximum < x.center Then maximum += 1
  iteration += 1
  Gosub Wave.generator

'*********************************************************************
'                            IMPORTANT
'*********************************************************************
' The more accurate diagonal correction shown above was elaborated by
' Mr. Jocelyn Marcotte himself in 2006. Surprizingly, it also yields
' faster results because the wave speed is accelerated from .707 to
' 1 pixel per cycle exactly. This is possible because the sign for
' the "present" energy is negative (the positive sign leads to a
' slower wave speed). Additionnally, the influence for diagonal
' transmission remains in accordance with the square of the distance
' law. This also works in 3-D using 26 neighbors and three levels
' of influence. Mr. Marcotte used this method in 2006 for reproducing
' my moving electron (he was the first one!) in full 3-D.
' The basic trend in 2-D may also be given by:

' trend(x,y) = (present(x-1,y) + present(x,y-1) + present(x,y+1) + present(x+1,y)) / 2 - past(x,y)

' This calculus is much simpler, yet the diagonal propagation is
' delayed to the next cycle. This is especially annoying for shorter
' wavelenghts. It should also be emphasized that the "c" speed
' normalized to 1 is of the utmost importance for experiments
' involving motion. The Doppler effect becomes much easier to deal
' with, and this is especially true for the Lorentz transformations.


'*********************************************************************
' WAVES IN COLOR
'*********************************************************************
    
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
    If present(x,y) > 0 Then                                          'complementary magenta and emerald green.
      r = luminance2
      g = luminance1
    Else
      r = luminance1
      g = luminance2
    End If
       Pset(x,y), Rgb(r,g,b)
  Next: Next

  For x = 0 To x.width Step 2                                         'graphics.
    Line(x-2, previous)-(x, y.graphics - 2 * present(x,y.source)), black
    previous = y.graphics - 2 * present(x,y.source)
  Next


'*********************************************************************
' KEYBOARD MANAGEMENT.
'*********************************************************************

  key$ = Inkey
  If Len(key$) Then
    If Len(key$) = 2 Then key$ = Right(key$, 1) + "+" Else key$ = Ucase(key$)
    Select Case key$
      Case Chr(27), "k+": End                                         'escape key or Windows' X quit button.
      Case "I": Gosub Initialization
      Case "M": Run "WaveMechanics00.exe"                             'main menu.
      Case "P": Screenset visible.page                                'pause.
                Color red, background: Locate 44, 89
                Print "P - Paused. Press any key to resume.   "
                key$ = "": Sleep
                Screenset work.page, visible.page
      Case "R":                                                       'reset via initialization.
      Case Else: key$ = ""                                            'avoid initialization.
    End Select
    If Len(key$) Then Gosub Initialization
    Do: Loop While Len(Inkey)                                         'clear buffer.
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
                              If click Then Sleep 1000                'slower.
               Case Is > 472: Locate 47, 60: Print line47b$
                              If click Then Run "WaveMechanics00.exe" 'main menu.
               Case Is > 318: Locate 47, 41: Print line47a$
                              If click Then                           'initialization.
                                Do: Getmouse a,b,c, click: Loop While click
                                Gosub Initialization
                              End If
             End Select
    Case 48: Select Case x.mouse
               Case Is > 700
               Case Is > 576: Locate 48, 73: Print line48c$;
                              If click Then Run "WaveMechanics06.exe" 'next program.
               Case Is > 472: Locate 48, 60: Print line48b$;
                              If click Then End                       'quit.
               Case Is > 318: Locate 48, 41: Print line48a$;
                              If click Then Run "WaveMechanics04.exe" 'previous program.
             End Select
  End Select

'  Color black, background: Locate 27,91: Print " Iteration"; iteration;
'  Print Using "    Time ###.###"; Timer - t
  Circle(x.source, y.source),  .375 * lambda, white                   '3/4 lambda emitter.
  If maximum < x.width Then Line(x.center + maximum+1,y.center-20)-(x.center + maximum+1,y.center+20), background
Loop


'*********************************************************************
' FRIENDLY ADJUSTABLE FRAMES.
'*********************************************************************

Frame:
margin = 20
x.left  = 8 * left. - margin - 10
x.right = 8 * left. + 8 * x.text + margin
y.top = top * 16 - margin - 6
y.bottom = y.top + 16 * y.text + 2 * margin

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
Windowtitle "WaveMechanics05 - Wave generator"
matrix.page = 2
x.center = x.width / 2
y.center = y.height / 2
x.source = x.center
y.source = y.center
maximum = 10
y.graphics = y.height + 110
iteration = 0
t = Timer
If unidirectional Then amplitude = 40 Else amplitude = 80
red  =        Rgb(255,0,0)
blue =        Rgb(0,0,255)
cyan =        Rgb(0,100,100)
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
Gosub Title
Gosub Text

For pixel = 0 To x.width                                              'graphics.
  distance = Abs(pixel - x.source)
  If distance > lambda / (2 * pi) Then                                'curve junction point: .159 * lambda, surprizingly.
    y = 295 / Sqr(distance)
  Else
    x = 2 * pi * distance / (.75 * lambda)                            'x = 2 * pi * dist / lambda for 3-D wave core.
   If x Then y = sin(x) / x Else y = 1
    y = y * 1000 / lambda^.5                                          'approx.
  End If
  Line(pixel,y.graphics - y)-(pixel,y.graphics + y), white
Next

Line(0,y.graphics)-(x.width, y.graphics), gray
Line(x.source,y.graphics-20)-(x.source, y.graphics+20), gray
Line(x.source-.375*lambda, y.graphics-10)-(x.source-.375*lambda,  y.graphics+100), black
Line(x.source+.375*lambda, y.graphics-10)-(x.source+.375*lambda,  y.graphics+100), black
Line(x.source-1.375*lambda,y.graphics-10)-(x.source-1.375*lambda, y.graphics+10), gray
Line(x.source+1.375*lambda,y.graphics-10)-(x.source+1.375*lambda, y.graphics+10), gray


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

x.text = 52                                                           'text width (pixels = x * 8).
y.text = 6                                                            'number of lines (pixels = y * 16).
top = 7                                                               'upper limit.
left.= 70                                                             'limit on the left hand side: "Locate top, left".
Gosub frame
Locate top - 1
Locate,left.: Print "  THE 2-D WAVE GENERATOR  ":?
Locate,left.: print "The red curve below indicates the sinusoidal input"
Locate,left.: print "energy distributed on a .5 lambda circular surface."
Locate,left.: print "The 2-D virtual medium is nevertheless reacting in"
Locate,left.: print "accordance with its own .75 lambda circular core"
Locate,left.: print "diameter. The larger 3-D spherical core diameter is"
Locate,left.: print "rather one wavelength, especially for standing waves."
y.text = 9
top = 19
Gosub frame
Locate top - 1
Locate,left.: Print "  THE 2-D OPTIMIZED WAVE ALGORITHM  ":?
Locate,left.: print "In 2006, Mr. Jocelyn Marcotte elaborated a faster and"
Locate,left.: print "more accurate version of his 2-D wave algorithm. The"
Locate,left.: print "resulting wave speed is one pixel per loop exactly"
Locate,left.: print "instead of .707. This is important because a c = 1"
Locate,left.: print "speed allows the use of Lorentz's simplified formulas"
Locate,left.: print "in order to produce the Doppler effect. Moreover,"
Locate,left.: print "one half of the diagonal transmission of energy is"
Locate,left.: print "added to the full orthogonal one. This improved all"
Locate,left.: print "azimut energy transmission is also possible in 3-D."
y.text = 5
top = 34
Gosub frame
Locate top - 1
Locate,left.: Print "  THE DAMPING SCREEN  ":?
Locate,left.: print "I found in June 2008 a more efficient damping screen"
Locate,left.: print "which works well here whatever the incidence angle is,"
Locate,left.: print "on condition that it is known. However, for multiple"
Locate,left.: print "or moving sources, Mr. Delmotte's method using a wide"
Locate,left.: print "off-screen damping zone still remains preferable."

Locate 44,89: Print "P - Pause."
Locate 45,89: Print "R - Reset."
Locate 45,27: print "3/4 lambda"

Color green.text
Locate 47,42: Print "I - Initialize.    M - Menu.    Slow."
Locate 48,42: Print "Previous Program.  Quit (Esc.). Next Program.";

Color gray
Locate 47, 3: Print "Thanks to the creators of FreeBASIC."
Locate 48, 3: Print "Gabriel LaFreniere  glafreniere.com";
Locate 47,89: Print "July 19, 2008. This program may be"
Locate 48,89: Print "freely distributed, copied or modified.";
Color black
Return


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Title:
title$ = "Waves in 2 Dimensions"
Locate 1,1: Print title$
center = 769
y.title = 20
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


'*********************************************************************
' WAVE GENERATOR - SINUSOIDAL IMPULSE.
'*********************************************************************

Wave.generator:
If iteration > 100 * lambda Then
  if iteration > 101 * lambda Then gosub Initialization
  Return
  Elseif iteration > 99 * lambda Then                                 'tempering both ends to avoid anomalies. 
  amplitude = (100 * lambda - iteration) * (26500 / lambda^2.5) / lambda'impulse zone smaller for shorter wavelength. 
  Elseif iteration < lambda Then
  amplitude = iteration * (26500 / lambda^2.5) / lambda
  else amplitude = 26500 / lambda^2.5
End If

phase = 2 * pi * iteration / lambda                                   'thanks to Mr. Marcotte's wave speed c = 1 pixel.
y.point2 = y.graphics

For x.coord = -lambda / 4 To lambda / 4
  x.squared = x.coord^2
  For y.coord = -lambda / 4 To lambda / 4
    distance = Sqr(x.squared + y.coord^2)
    If distance <= .25 * lambda Then
      x = 2 * pi * distance / lambda
      y = cos(x)
      if y.coord = 0 then                                             'show generator (red curve).
        y.point1 = y.graphics - .0038 * lambda^2.5 * y * amplitude * Cos(phase)
        line(x.source+x.coord-1, y.point2)-(x.source+x.coord, y.point1), red
        y.point2 = y.point1
      end if
      trend(x.source+x.coord, y.source+y.coord) = trend(x.source+x.coord, y.source+y.coord) + amplitude * y * Cos(phase)'generator.
    End If
  Next
Next
Return