' This is a FreeBasic program. Gabriel LaFreniere, Oct. 22, 2007. Updated Oct. 23, 2007. 
' Please download the editor/compiler from: http://fbide.freebasic.net/index.php?menuID=56 
Dim As Single pi = 4 * Atn(1), sine, cosine
work.page = 1: lambda = 100: Screen 19,24,3: Gosub Initialization

Do
  Swap work.page, visible.page                            'swap 1 <--> 0.
  Screenset work.page, visible.page                       'pset is faster on work page.
  Pcopy 2, work.page                                      'copy matrix page on work page.
  For x = 0 To 799                                        'window 800 pixels.

'******************* MR. ANSELME DEWAVRIN'S ALGORITHM AFTER EULER'S METHOD ********************

    sine =   sine + cosine * 2 * pi / lambda
    cosine = cosine - sine * 2 * pi / lambda

'**********************************************************************************************
    Pset(x, y.center - sine), green
    Pset(x, y.center - cosine), red
  Next

  key$ = Ucase(Inkey)                                     'keyboard.
  Select Case key$
    Case Chr(27), Chr(255)+"K": End                       'quit.
    Case "+": lambda = lambda * 2                         'sub multiples of 800 only.
              If lambda > 800 Then lambda = 800 else Gosub Initialization
    Case "-": lambda = lambda / 2
              If lambda < 25  Then lambda = 25  else Gosub Initialization              
  End Select
Loop

Initialization:
red = Rgb(255,0,0)
green = Rgb(0,150,0)
white = Rgb(255,255,255)
background = Rgb(225,225,225)
template = sqr(10 * lambda)                               'wave amplitude in pixel units.
sine = 0
cosine = template                                         'cosine initialization for 0°.
y.center = 223
Color black, background
Screenset 2, 2: Cls
Locate 4, 3: Print "In June 2005, Mr. Philippe Delmotte applied Newton's laws to Verlet's algorithm and obtained"
Locate 5, 3: Print "a perfect computerized wave medium, the Virtual Aether. In November 2005 I derived from it an"
Locate 6, 3: Print "algorithm for the pendulum, which also produces a sine curve (replace sine and cosine below with"
Locate 7, 3: Print "energy and inertia). Mr. Anselme Dewavrin derived in Oct. 2006 a similar algorithm from Euler's"
Locate 8, 3: Print "method, which introduces discrete steps. So the curve is unstable if the wavelength is too short."
Locate 21,3: Print "The point is that, if the medium is made of granules, the so-called error is rather a true fact."
Locate 22,3: Print "This strongly indicates that genuine waves should be slower as soon as the wavelength becomes"
Locate 23,3: Print "too short with respect to the number of granules involved. Each granule transmits energy with"
Locate 24,3: Print "discrete steps, the way Euler's method does, and a quantum effect appears."
Locate 26,3: Print "The quantum properties explain the lens effect and the electron amplification. They also explain"
Locate 27,3: Print "why all electrons oscillate on the same frequency, which is the highest possible."
Locate 30,3: Print "Green curve : sine."
Locate 31,3: Print "Red curve: cosine."
Locate 33,3: Print "Press Esc to quit."
Color black, white
Line(300,7)-(500,38),black,bf
Line(302,9)-(498,36),white,bf
Locate 2,41: Print "DEWAVRIN'S ALGORITHM"
Line(210,456)-(590,502),black,bf
Line(212,458)-(588,500),white,bf
Locate 30,31: Print "sine  =  sine + cosine * 2 * pi / lambda"
Locate 31,31: Print "cosine = cosine - sine * 2 * pi / lambda"
Locate 36: Color red, background
Locate 34,3: Print "Press + or - to change lambda:";: Print lambda; " pixels."
Locate 36: Color green
Locate, 3: Print "Thanks to FreeBASIC creators."
Locate, 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate 36,57: Print "Oct. 23, 2007. This FreeBASIC program may"         'Créé le 24 octobre 2006.
Locate 37,57: Print "be freely distributed, copied or modified.";
Color black, background
Line(0, y.center - template)-(799, y.center + template), white, bf
Line(0, y.center - template - 1)-(799, y.center - template - 1), black
Line(0, y.center + template + 1)-(799, y.center + template + 1), black
Line(0, y.center)-(799, y.center), black                   'axe
For x = 0 To 799                                          'véritable sineoïde pour comparer.
  Line(x, y.center - y)-(x, y.center - template * Sin(2 * pi * x / lambda)), Rgb(150,150,150)
  y = template * Sin(2 * pi * x / lambda)
Next
Return
