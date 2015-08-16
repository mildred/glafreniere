Dim As Single pi = 4 * Atn(1), x, rotation, distance, amplitude, xFlottante, tPrime
Dim As Single beta, theta, sinTheta, cosTheta
Screen 19,24,3: page1 = 1: Gosub Initialiser              'créé le le 26 juin 2006.

'******************** L'ÉLECTRON SELON LES TRANSFORMATIONS DE LORENTZ ************************
Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  image = image + 1: If image > images Then image = 1
  rotation = 2 * pi * image / images                      'rotation de phase selon l'image.
  line(0,yCentre2)-(799,yCentre2), rgb(180,180,180)

  For xCoord = -xCentre To xCentre                        'pas de symétrie horizontale.
    xCarre = xCoord ^ 2
    xPrime = xCoord / cosTheta                            'contraction selon Lorentz, entier.
    xPrimeCarre = xPrime ^ 2
    xFlottante = xPrime / cosTheta                        'retrouver l'abscisse x exacte.
    tPrime = 2 * pi * xFlottante * sinTheta / (lambda / cosTheta)  'décalage horaire selon Lorentz.
    For yCoord = 0 To yCentre                             'symétrie verticale.
      Gosub Stationnaire                                  'ondes stationnaires sphériques.
      Gosub Mobile                                        'électron mobile.
    Next
  Next
  
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  If saisie$ = "P" Then Sleep
  If saisie$ = chr(27) Or saisie$ = "k+" Then End
Loop

Stationnaire:
distance = Sqr(yCoord * yCoord + xCarre)                  'distance selon Pythagore.
x = 2 * pi * distance / lambda                            'distance en radians.
If x Then amplitude  = Sin(x) / x * Sin(rotation) Else amplitude = Sin(rotation)
If amplitude > 0 Then
  rouge = amplitude * 1000
  bleu = rouge / 2
  If rouge > 255 Then vert = rouge - 255 Else vert = 0
Else
  vert = -amplitude * 1000
  bleu = vert / 2
  If vert > 255 Then rouge = vert - 255 Else rouge = 0
End If
If rouge > 255 Then rouge = 255
If vert  > 255 Then vert  = 255
If bleu  > 255 Then bleu  = 255
Pset(xCoord + xCentre, yCentre - yCoord), Rgb(rouge,vert,bleu)
Pset(xCoord + xCentre, yCentre + yCoord), Rgb(rouge,vert,bleu)
If yCoord = 0 Then Pset(xCoord + xCentre, yCentre2 + 75 * amplitude)
Return

Mobile:
distance = Sqr(yCoord * yCoord + xPrimeCarre)             'distance selon Pythagore.
x = 2 * pi * distance / lambda                            'distance en radians.
If x Then amplitude  = Sin(x) / x * Sin(rotation - tPrime) Else amplitude = Sin(rotation)
If amplitude > 0 Then
  rouge = amplitude * 1000
  bleu = rouge / 2
  If rouge > 255 Then vert = rouge - 255 Else vert = 0
Else
  vert = -amplitude * 1000
  bleu = vert / 2
  If vert > 255 Then rouge = vert - 255 Else rouge = 0
End If
If rouge > 255 Then rouge = 255
If vert  > 255 Then vert  = 255
If bleu  > 255 Then bleu  = 255
Pset(xCoord + xCentre2, yCentre - yCoord), Rgb(rouge,vert,bleu)
Pset(xCoord + xCentre2, yCentre + yCoord), Rgb(rouge,vert,bleu)
If yCoord = 0 Then Pset(xCoord + xCentre2, yCentre2 + 75 * amplitude)
Return

Initialiser:
rouge = Rgb(255,100,100)
vert  = Rgb(0,200,0)
fond  = Rgb(225,225,225)
blanc = Rgb(255,255,255)
beta = sin(pi / 4)
beta = .866
theta = Asin(beta)
sinTheta = Sin(theta)
cosTheta = Cos(theta)
lambda = 200'0
hauteur = 300
largeur = 400
xCentre = largeur / 2 - 1
xCentre2 = 800 - largeur / 2
yCentre = hauteur / 2
yCentre2 = hauteur + 78
Screenset 2, 2                                            'créer une page matrice.
Color noir, fond: Cls
images = 48

Locate 36,38: ?"Gabriel LaFreniŠre, le 15 octobre 2006.  glafreniere.com"
Locate 37,38: ?"Ce programme peut ˆtre copi‚, modifi‚ ou distribu‚ librement.";
Pcopy 2, page1
Return

