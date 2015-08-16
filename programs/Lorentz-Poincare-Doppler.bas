' This is a FreeBasic program. Created Nov. 19, 2007. Gabriel LaFreniere.
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
Dim As Single beta, new.data, g.Lorentz, x.reversed, t.reversed
Dim As Single x.coord, x.prime, t.time, t.prime
Gosub Initialization

Do
  Swap work.page, visible.page                            'swap 0 <=> 1.
  Screenset work.page, visible.page
  Pcopy 2, work.page                                      'matrix page is 2.
  Getmouse x.mouse, y.mouse, , click                      'get mouse.
  If y.mouse > 195 And y.mouse < 216 Then                 'modify beta.
    new.data = .01 * x.mouse / 8
    If new.data < .01 Then new.data = 0
    Locate 12,33: Print Using "beta =##.### "; new.data;
    Print "light second per second. "
    Line(1, 196)-(798, 215), white, bf
    Line(x.mouse, 196)-(x.mouse, 215), black
    For j = 80 To 799 Step 80
      Line(j,195)-(j,216), black
    Next
    Line(beta * 800 - 1, 196)-(beta * 800 + 1, 215), red, bf
    If click = 1 Then
      If beta = new.data Then Else beta = new.data: Gosub Update
      End If
  Elseif y.mouse > 260 And y.mouse < 281 Then             'modify x coordinate.
    new.data = .1 * (x.mouse - 400) / 8
    If Abs(new.data) < .05 Then new.data = 0
    Locate 16, 40: Print Using "x = ##.### "; new.data;
    If Abs(new.data) > 1 Then Print "light seconds. " Else Print "light second.  "
    Line(1, 261)-(798, 280), white, bf
    Line(x.mouse, 261)-(x.mouse, 280), black
    For j = 80 To 799 Step 80
      Line(j, 260)-(j, 281), black
    Next
    Line(400 + x.coord * 80 - 1, 261)-(400 + x.coord * 80 + 1, 280), red, bf
    If click = 1 Then
      If x.coord = new.data Then Else x.coord = new.data: Gosub Update
    End If
  Elseif y.mouse > 323 And y.mouse < 344 Then             'modify t time.
    new.data = .1 * (x.mouse - 400) / 8
    If Abs(new.data) < .05 Then new.data = 0
    Locate 20,43: Print Using "t = ##.### "; new.data;
    If Abs(new.data) > 1 Then Print "seconds. " Else Print "second.  "
    Line(1, 324)-(798, 343), white, bf
    Line(x.mouse, 323)-(x.mouse, 344), black
    For j = 80 To 799 Step 80
      Line(j, 323)-(j, 344), black
    Next
    Line(400 + t.time * 80 - 1, 323)-(400 + t.time * 80 + 1, 344), red, bf
    If click = 1 Then
      If t.time = new.data Then Else t.time = new.data: Gosub Update
    End If
  End If
  in.key$ = Inkey                                         'check inkey.
  If Len(in.key$) Then
    If Len(in.key$) = 2 Then in.key$ = Right(in.key$, 1) + "+" Else in.key$ = Ucase(in.key$)
    Select Case in.key$
      Case "I": beta = .866025403: x.coord = 1: t.time = 0: Gosub Update'Initialize.
      Case "k+",Chr$(27):End
    End Select
  End If
Loop

Update:
Screenset 2, 2
If beta > .865 And beta < .867 Then beta = .866025403     'for accurate .5 contraction. 
g.Lorentz = Sqr(1 - beta ^ 2)
'---------------------------------- DISPLAYING ACTIVE DATA ------------------------------------ 

Locate 8, 84: Print Using "beta = ##.###     "; beta
Locate 9, 84: Print Using "   g = ##.###     "; g.Lorentz
Locate 10,84: Print Using "   x = ##.###     "; x.coord
Locate 11,84: Print Using "   t = ##.###     "; t.time

'-------------------------- DISPLAYING LORENTZ'S ORIGINAL EQUATIONS --------------------------- 

x.prime = (x.coord - beta * t.time) / g.Lorentz           'Lorentz's original.
t.prime = (t.time - beta * x.coord) / g.Lorentz
x.reversed = (x.prime + beta * t.prime) / g.Lorentz       'Poincaré's reversed.
t.reversed = (t.prime + beta * x.prime) / g.Lorentz
If Abs(x.prime) < .01 Then x.prime = 0                    'display problems if less than .01
If Abs(t.prime) < .01 Then t.prime = 0
If Abs(x.reversed) < .01 Then x.reversed = 0
If Abs(t.reversed) < .01 Then t.reversed = 0
Locate 3, 57: Print Using "##.### : 1"; g.Lorentz
Locate 5, 57: Print Using "##.###  "; x.prime;            'Lorentz's original.
If Abs(x.prime) > 1 Then Print "light seconds. " Else Print "light second. "
Locate 6, 57: Print Using "##.###  "; t.prime;
If Abs(t.prime) > 1 Then Print "seconds. " Else Print "second. "
Locate 8, 57: Print Using "##.###  "; x.reversed          'Poincaré's reversed.
Locate 9, 57: Print Using "##.###  "; t.reversed

' IMPORTANT: The Lorentz transformations were intended to correct the Doppler effect in order
' to make Maxwell's equations invariant in any moving frame or reference. However, Lorentz
' was strongly convinced that matter should contract. He was well aware that a time shift
' should occur, and also that clocks should tick slower.

' Unfortunately, Lorentz's equations do not yield correct results from this point of view.
' They can be reversed, though, by swapping the x and x' variables. The formulas below
' indeed produce the correct results. So the graphics (see below) use them, and one can
' easily see that all happens the way Lorentz predicted.

' Surprisingly, they also produce a special Doppler effect involving a slower frequency.
' My program Doppler_Voigt_transformations.bas explains this.
' I am of an opinion that Lorentz's Doppler equations should always be present together
' with his original ones, in order to justify the contraction and the time effects.

' It should be emphasized that the Lorentz transformations are a mechanical law of nature,
' the greatest of all, making Relativity MUCH LESS important. After all, Relativity is
' just the result of it. It is just the result of any moving observer's errors.

'---------------------- DISPLAYING MODIFIED LORENTZ'S DOPPLER EQUATIONS ----------------------- 

x.prime = g.Lorentz * x.coord + beta * t.time             'Lorentz's Doppler.
t.prime = g.Lorentz * t.time  - beta * x.coord
x.reversed = g.Lorentz * x.prime - beta * t.prime         'Retrieving x, t. Poincaré's method.
t.reversed = g.Lorentz * t.prime + beta * x.prime
If Abs(x.prime) < .01 Then x.prime = 0                    'display problems if less than .01
If Abs(t.prime) < .01 Then t.prime = 0
If Abs(x.reversed) < .01 Then x.reversed = 0
If Abs(t.reversed) < .01 Then t.reversed = 0
Locate 25, 56: Print Using "##.###  "; x.prime;
If Abs(x.prime) > 1 Then Print "light seconds (or wavelengths).  " Else Print "light second (or wavelength).  "
Locate 26, 56: Print Using "##.###  "; t.prime;
If Abs(t.prime) > 1 Then Print "seconds (or wave periods).  " Else Print "second (or wave period).  "
Locate 28, 56: Print Using "##.###  "; x.reversed
Locate 29, 56: Print Using "##.###  "; t.reversed

x.reversed = (x.prime - beta * t.time) / g.Lorentz        'retrieving Lorentz's equations.
t.reversed = (t.prime + beta * x.coord) / g.Lorentz       'the plus sign indicates an anomaly.
If Abs(x.reversed) < .01 Then x.reversed = 0
If Abs(t.reversed) < .01 Then t.reversed = 0
Locate 31, 56: Print Using "##.###  "; x.reversed
Locate 32, 56: Print Using "##.###  "; t.reversed

Line(0, 196)-(798, 215), gray, bf                         'cursor boxes. 
Line(0, 195)-(799, 216), black, b
Line(beta * 800-1, 196)-(beta * 800+1, 215), red, bf
Line(0, 261)-(798, 280), gray, bf
Line(0, 260)-(799, 281), black, b
Line(400 + x.coord * 80-1, 261)-(400 + x.coord * 80+1, 280), red, bf
Line(0, 324)-(798, 343), gray, bf
Line(0, 323)-(799, 344), black, b
Line(400 + t.time * 80-1, 324)-(400 + t.time * 80+1, 343), red, bf

'--------------------------------- DISPLAYING THE GRAPHICS ------------------------------------ 

Line(0, 512)-(799, 572), background, bf
For j = 80 To 799 Step 80
  Line(j, 515)-(j, 555), black
Next
Locate 35,2:  Print " x'=    -4        -3        -2        -1         0         1         2         3         4"
circle(400 + 80 * x.prime, 542), 26, black,,, 1 / g.Lorentz
line(400 + 80 * x.prime, 515)-(400 + 80 * x.prime, 543),black
line(400 + 80 * beta * t.time, 515)-(400 + 80 * beta * t.time, 569), black
line(400 + 80 * x.prime, 515)-(400 + 80 * beta * t.time, 515), black
line(400 + 80 * x.prime, 569)-(400 + 80 * beta * t.time, 569), black
Return

Initialization:
Screen 19,24,3
Windowtitle "The Lorentz transformations - Poincare's symmetric equations and the Doppler effect."
work.page = 1
beta = .866025403
g.Lorentz = Sqr(1 - beta ^ 2)
x.coord = 1
t.time = 0
white = Rgb(255,255,255)
background = Rgb(235,235,235): 
gray = Rgb(215,215,215)
dark.gray = Rgb(100,100,100)
green = Rgb(0,150,0)
blue = Rgb(0,0,255)
red = Rgb(225,0,0)
Screenset 2, 2
Color green, background: Cls
Locate 37, 3: Print "Gabriel LaFreniere  glafreniere.com";
Locate 37,78: Print "Updated Nov. 22, 2007";
Color black
Locate 1, 38: Print "THE LORENTZ TRANSFORMATIONS"
Locate 2, 82: Print "Press Esc to quit."
Locate 3, 82: Print "I - Initialize."
Locate 3, 3:  Print "Lorentz's contraction factor: g = sqr(1 - beta ^ 2) ="
Locate 5, 3:  Print "Lorentz's equations: x'= (x - beta * t) / g      x' ="
Locate 6, 3:  Print "                     t'= (t - beta * x) / g      t' ="
Locate 8, 3:  Print "Poincare's reversed: x = (x'+ beta * t') / g     x  ="
Locate 9, 3:  Print "                     t = (t'+ beta * x') / g     t  ="
Locate 5, 84: Print "y'= y   z'= z"
Locate 11,31: Print "Move the cursor below and click to select. "
Locate 15,2:  Print " beta = .1        .2        .3        .4        .5        .6        .7        .8        .9"
Locate 19,2:  Print " x =    -4        -3        -2        -1         0         1         2         3         4"
Locate 23,2:  Print " t =    -4        -3        -2        -1         0         1         2         3         4"
Locate 25, 3: Print "Lorentz's Doppler:  x'=  g * x + beta * t       x' ="
Locate 26, 3: Print "                    t'=  g * t - beta * x       t' ="
Locate 28, 3: Print "         Reversed:  x =  g * x'- beta * t'      x  ="
Locate 29, 3: Print "                    t =  g * t'+ beta * x'      t  ="
Locate 31, 3: Print " Or, surprisingly:  x = (x'- beta * t) / g      x  =         Compare with Lorentz's equations."
Locate 32, 3: Print "                    t = (t'+ beta * x) / g      t  =         Plus sign instead of minus."

line(6, 374)-(772, 422), white, b                         'emphazise preferred Doppler.
line(7, 375)-(771, 421), white, b
line(8, 376)-(770, 420), white, b
line(9, 377)-(769, 419), dark.gray, b
line(769, 377)-(769, 419), white
line(770, 376)-(770, 420), dark.gray
line(771, 375)-(771, 421), dark.gray
line(772, 374)-(772, 422), dark.gray
line(9, 419)-(769, 419), white
line(8, 420)-(770, 420), dark.gray
line(7, 421)-(771, 421), dark.gray
line(6, 422)-(772, 422), dark.gray
For j = 80 To 799 Step 80
  Line(j-1, 217)-(j+1, 221), black, bf
  Line(j-1, 282)-(j+1, 286), black, bf
  Line(j-1, 345)-(j+1, 349), black, bf
Next
Gosub Update
Return