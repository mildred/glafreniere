Dim amplitudeMax(800) 
Dim As Single pi = 4 * Atn(1), backwardAmplitude, rightwardAmplitude
Dim As Single beta, theta, frontwardLambda, backwardLambda, x, t, pixel, ratio
Gosub Initialize

Do
  Swap page1, page2
  Screenset page1, page2
  screensync
  Pcopy 2, page1
  t = 2 * pi * image / images                             'time in radians.

'**************************************** ROTATION *******************************************
  For pixel = 0 To length
    precedentBackward = backwardAmplitude
    precedentFrontward = rightwardAmplitude
    precedentTotal = totalAmplitude
    x = 2 * pi * (xCentre - pixel + .001) / frontwardLambda  'avoid x = 0, .001 negligible.
    rightwardAmplitude =   template * (Cos(t) * Sin(x) - Sin(t) * (1 - Cos(x))) / x / (1 - beta)
    x = 2 * pi * (xCentre - pixel + .001) / backwardLambda
    backwardAmplitude = template * (Cos(t) * Sin(x) + Sin(t) * (1 - Cos(x))) / x / (1 + beta)
    totalAmplitude = backwardAmplitude + rightwardAmplitude
    If amplitudeMax(pixel) < Abs(totalAmplitude) Then amplitudeMax(pixel) = Abs(totalAmplitude)
    Line(pixel, yCentre - amplitudeMax(pixel))-(pixel, yCentre + amplitudeMax(pixel)),grey
    Line(pixel, yCentre)-(pixel, yCentre - totalAmplitude), white 'enhance total amplitude.
    Line(pixel, yCentre - precedentBackward)-(pixel, yCentre - backwardAmplitude), red
    Line(pixel, yCentre - precedentFrontward)-(pixel, yCentre - rightwardAmplitude), blue
    Line(pixel, yCentre - precedentTotal)-(pixel, yCentre - totalAmplitude), black
  Next

'******************************* LOCATE STANDING WAVES NODES *********************************

For pixel = xCentre - 6 * lambda * Cos(theta) To length Step lambda * Cos(theta) / 2
    Line(pixel, yCentre - 10)-(pixel, yCentre + 10),black
  Next

  Line(0, yCentre)-(length, yCentre),black                'horizontal axis.
  keyInput$ = Inkey
  If Len(keyInput$) = 2 Then keyInput$ = Right(keyInput$, 1) + "+" Else keyInput$ = Ucase(keyInput$)
  Select Case keyInput$
    Case chr(27),"k+": End
    Case "P": Sleep: Do: Loop While Len(inkey)            'pause. Slow: clear buffer.
    Case "0","1","2","3","4","5","6","7","8","9"
      beta = Val(keyInput$) / 10
      Gosub Update
  End Select
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
fileName$ = "capture" + number$ + str(capture) + ".bmp"
'Locate 24, 3: Print fileName$
bsave fileName$,0
capture = capture + 1
If capture = images Then End 
Return

'************************************ UPDATE SPEED DATA **************************************
Update:
ratio = (1 + beta) / (1 - beta)
theta = Asin(beta)
frontwardLambda =   lambda * (1 - beta) / Cos(theta)
backwardLambda = lambda * (1 + beta) / Cos(theta)
Screenset 2
Locate 5, 93: Print using "#.#"; beta
Locate 15,25: Print using "### pixels"; lambda / Cos(theta);: ?"."
Locate 29,28: Print using " ##.#"; ratio
Locate 31,27: Print using "##.#"; theta * 180 / pi;: Print chr(248)
Locate 32,30: Print using "#.###"; Cos(theta)
Locate 35,25: Print using "#.###"; Cos(theta)
For pixel = 0 To length
  amplitudeMax(pixel) = 0
Next
image = 0
Return

'*************************************** INITIALIZE *****************************************
Initialize:
Screen 19,24,3
background = Rgb(225,225,225)                             'frequently used colors.
white = Rgb(255,255,255)
red = Rgb(255,0,0)
blue = Rgb(0,100,255)
grey = Rgb(185,185,185)
lambda = 150
length = 799
xCentre = 399: yCentre = 299
page1 = 1: images = 150
beta = .5
template = 60                                             'amplitude template.
Screenset 2, 2: Color black, background: Cls
Color black, white
Line(6,10)-(370,34),black,bf
Line(8,12)-(368,32),white,bf
Locate 2,3:?  "THE ELECTRON'S WAVES AND THE DOPPLER EFFECT"
Line(78,345)-(242,372),black,bf
Line(80,347)-(240,370),white,bf
Locate 23,12:?"THE DOPPLER EFFECT"
Line(498,12)-(738,51),black,bf
Line(500,14)-(736,49),white,bf
Locate 2,65:? "Press a number from 0 to 9"
Locate 3,65:? "to change the system speed."
Color Rgb(0,150,0), background
Locate 35,60:?"Gabriel LaFreniere   glafreniere.com"
Locate 36,60:?"This FreeBASIC program can be freely";
Locate 37,60:?"copied, modified or distributed.";
Color black, background
Locate 4,3:?  "Mr. Jocelyn Marcotte succeeded in resolving"
Locate 5,3:?  "the electron's progressive waves equations"
Locate 6,3:?  "on July 27, 2006. He discovered the correct"
Locate 7,3:?  "formula for quadrature (pi / 2):"
Locate 9,3:?  "Phase:       y = sin(x) / x"
Locate 10,3:? "Quadrature:  y = (1 - cos(x)) / x"
Locate 12,3:? "x = 2 * pi * distance / lambda"
Locate 13,3:? "Lambda increases according to Lorentz"
Locate 14,3:? "in addition to the Doppler effect:"
Locate 15,3:? "L' = L / cos(theta) = "
Locate 16,3:? "L  = 150 pixels (at rest)."
Locate 25,3:? "Frontward: L' * (1 - beta)"
Locate 26,3:? "Backward:  L' * (1 + beta)"
Locate 28,3:? "Amplitude ratio:"
Locate 29,3:? "(1 + beta) / (1 - beta) ="
Locate 30,3:? "The Lorentz Transformations angle:"
Locate 31,3:? "theta = arc sin(beta) = 30";:? chr(248)
Locate 32,3:? "Contraction = cos(theta) ="
Locate 33,3:? "Slower frequency according to Lorentz,"
Locate 34,3:? "which increases the wavelength:"
Locate 35,3:? "F' = F * cos(theta) =       F"
Locate 36,3:? "Pause or Slow: press [ P ]"
Locate 37,3:? "Press [ Esc ] to Quit. August 8, 2006.";
Locate 5,60:? "Normalized speed: beta = v / c ="
Locate 7,60:? "While the electron moves through the"
Locate 8,60:? "aether because of the Doppler effect,"
Locate 9,60:? "its nodes and antinodes are still pre-"
Locate 10,60:?"sent. This is very surprising. Note that"
Locate 11,60:?"here, the system is moving rightward. So"
Locate 12,60:?"the waves moving in that direction seem"
Locate 13,60:?"to move slower. The waves absolute speed"
Locate 14,60:?"is relative to the aether, though: it is"
Locate 15,60:?"the speed of light (the constant c)."
Locate 23,64:?"THE MOBILE ENVELOPPE UNDERGOES A"
Locate 24,61:?"CONTRACTION BUT ITS AMPLITUDE INCREASES"
Locate 26,60:?"The nodes and antinodes are contracting"
Locate 27,60:?"themselves according to the Lorentz"
Locate 28,60:?"transformations. However, the system's"
Locate 29,60:?"amplitude, hence its energy, increases."
Locate 30,60:?"This proves that the matter mass must"
Locate 31,60:?"grow up according to the Doppler. It"
Locate 32,60:?"strongly indicates that matter is made"
Locate 33,60:?"of MOVING standing waves."
Gosub Update
Return