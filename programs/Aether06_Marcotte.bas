Dim amplitudeMax(800)
Dim As Single pi = 4 * Atn(1), leftwardAmplitude, rightwardAmplitude, x, t
Gosub Initialize

Do
  Swap page1, page2
  Screenset page1, page2
  screensync
  Pcopy 2, page1
  t = 2 * pi * image / images                             'time, in radians.

'***************************************** ROTATION ******************************************
  For pixel = 0 To length
    precedentLeft = yCentre - leftwardAmplitude
    precedentRight = yCentre - rightwardAmplitude
    precedentTotal  = yCentre - totalAmplitude
    x = 2 * pi * (xCentre - pixel + .001) / lambda        'avoid x = 0, .001 negligible.
    leftwardAmplitude  = template * (Cos(t) * Sin(x) + Sin(t) * (1 - Cos(x))) / x
    rightwardAmplitude = template * (Cos(t) * Sin(x) - Sin(t) * (1 - Cos(x))) / x
    totalAmplitude = leftwardAmplitude + rightwardAmplitude
    If amplitudeMax(pixel) < totalAmplitude Then amplitudeMax(pixel) = totalAmplitude
    Line(pixel, yCentre - amplitudeMax(pixel))-(pixel, yCentre + amplitudeMax(pixel)),grey
    Line(pixel, yCentre)-(pixel, yCentre - rightwardAmplitude), white 'enhance rightwards waves.
    Line(pixel, precedentLeft)-(pixel, yCentre - leftwardAmplitude), black   'leftward waves.
    Line(pixel, precedentRight)-(pixel, yCentre - rightwardAmplitude), black 'rightward waves.
    Line(pixel, precedentTotal )-(pixel, yCentre - totalAmplitude), black    'standing waves.
  Next

  For pixel = 0 To length
    precedentGreen  = yCentre - greenAmplitude
    precedentRed = yCentre - redAmplitude
    x = 2 * pi * (xCentre - pixel + .001) / lambda
    
'****************************** Mr. JOCELYN MARCOTTE'S EQUATIONS *****************************

    greenAmplitude  = template * Sin(x) / x               'phase, display in green.
    redAmplitude = template * (1 - Cos(x)) / x            'quadrature, display in red.

'*********************************************************************************************

    Line(pixel, precedentGreen )-(pixel, yCentre - greenAmplitude ), green 'PHASE.
    Line(pixel, precedentRed)-(pixel, yCentre - redAmplitude),       red   'QUADRATURE.
  Next

'******************************* LOCATE STANDING WAVES NODES *********************************

  For pixel = xCentre - 4 * lambda To length Step lambda / 2
    Line(pixel, yCentre - 10)-(pixel, yCentre + 10),black
  Next

  Line(0, yCentre)-(length, yCentre),black                'horizontal axis.
  keyInput$ = Inkey
  If Len(keyInput$) = 2 Then keyInput$ = Right(keyInput$, 1) + "+" Else keyInput$ = Ucase(keyInput$)
  If keyInput$ = Chr(27) Or keyInput$ = "k+" Then End
  If keyInput$ = "P" Then Sleep: Do: Loop While Len(Inkey)'pause. Slow: clear buffer.
'  if bitmap = 1 then gosub Bitmaps                       '150 bitmaps sequence if needed.
  image = image + 1
  If image = images Then image = 0: bitmap = 1
Loop

Bitmaps:'***************************** BITMAP SEQUENCE ***************************************
Select Case capture
  Case Is < 10: number$ = "00"
  Case Is < 100: number$ = "0"
  Case Is < 1000: number$ = ""
End Select
fileName$ = "capture" + number$ + Str(capture) + ".bmp"
'Locate 24, 3: Print fileName$
Bsave fileName$,0
capture = capture + 1
If image > images - 1 Then End 
Return

'*************************************** INITIALIZE *****************************************
Initialize:
Screen 19,24,3
background = Rgb(225,225,225)                             'frequently used colors.
white = Rgb(255,255,255)
red = Rgb(255,0,0)
green = Rgb(0,255,150)
grey = Rgb(185,185,185)
lambda = 150
length = 799
xCentre = 399 : yCentre = 299
template = 149                                            'amplitude template.
precedentGreen = yCentre
precedentRed = yCentre
page1 = 1: images = 150
Screenset 2, 2: Color black, background: Cls
Color black, white
Line(184 - 78, 152)-(184 + 78, 183), green, bf            'phase equation.
Line(184 - 75, 155)-(184 + 75, 180), white, bf
Locate 11, 17:? "y = sin(x) / x"
Line(616 - 98, 152)-(616 + 98, 183), red, bf              'quadrature equation.
Line(616 - 95, 155)-(616 + 95, 180), white, bf
Locate 11, 68:? "y = (1 - cos(x)) / x"
Color Rgb(0,150,0), background
Locate 36,3:? "October 14, 2006."
Locate 37,3:? "Gabriel LaFreniere   glafreniere.com";
Locate 36,60:?"This freeBASIC program can be freely";
Locate 37,60:?"copied, modified or distributed.";
Color black, background
Locate 2, 5 :?"THE ELECTRON'S TRAVELLING WAVES"
Locate 13,74:?"Quadrature"
Locate 13,21:?"Phase"
Locate 4,3:?  "By March 2006, Mr. Jocelyn Marcotte"
Locate 5,3:?  "informed me that the electron's standing"
Locate 6,3:?  "waves evolved according to the formula"
Locate 7,3:?  "below, which is a solution of Bessel's"
Locate 8,3:?  "spherical function (green line):"
Locate 4,60:? "Mr. Marcotte succeeded in resolving"
Locate 5,60:? "the electron's progressive waves"
Locate 6,60:? "equations on July 27, 2006. He"
Locate 7,60:? "discovered the correct formula for"
Locate 8,60:? "quadrature or pi / 2 (red line):"
Locate 27,3:? "The x distance in radians:"
Locate 29,3:? "x = 2 * pi * distance / lambda"
Locate 24,74:?"Rotation."
Locate 26,60:?"Now that the formula for quadrature is"
Locate 27,60:?"known, a full rotation becomes possible."
Locate 28,60:?"In order to show the waves travelling"
Locate 29,60:?"in opposite directions, one must join"
Locate 30,60:?"those equations together and add a t"
Locate 31,60:?"variable for time (from 0 to 2 * pi):"
Locate 33,60:?"y = (Cos(t)*Sin(x)-Sin(t)*(1-Cos(x)))/x"
Locate 34,60:?"y = (Cos(t)*Sin(x)+Sin(t)*(1-Cos(x)))/x"
Locate 35,60:?"y = (Sin(t+x)-Sin(t))/x"
Locate 33,3:? "Quit: press [ Esc ]"
Locate 34,3:? "Pause or Slow: press [ P ]"
Return