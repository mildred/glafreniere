Dim As Single max1(-400 To 400), max2(-400 To 400)
Dim As Single pi = 4 * Atn(1), x, xCarre, beta, rotation, distance, lambdaLorentz
Dim As Single amplitude, phi, lambdaAvant, lambdaArriere, rapport, contraste
Screen 19,24,3: page1 = 1: Gosub Initialiser

'******************* L'ÉLECTRON SELON LES FORMULES DE M. JOCELYN MARCOTTE ********************
Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  image = image + 1: If image > images Then image = 1
  rotation = 2 * pi * image / images                      'rotation de phase selon l'image.

  Gosub Stationnaire                                      'ondes stationnaires sphériques.
  Gosub ElectronMobile                                    'électron mobile.

  saisie$ = Inkey
  If Len(saisie$) = 2 And Right(saisie$, 1) = "k" Then End' « X » de la fenêtre.
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
  Case Chr(27), "k+": End
  Case "I": Gosub Initialiser
  Case "P": Sleep
  Case "+": lambda = lambda + 10: If lambda > 200 Then lambda = 200
    lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
    For xCoord = -xCentre To xCentre
      max1(xCoord) = 0: max2(xCoord) = 0                  'effacer l'enveloppe.
    Next
    Screenset 2,2: Locate 33, 38: ? lambda; " pixels. ": Screenset page1, page2
  Case "-": lambda = lambda - 10: If lambda < 20 Then lambda = 20
    lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
    For xCoord = -xCentre To xCentre
      max1(xCoord) = 0: max2(xCoord) = 0
    Next
    Screenset 2,2: Locate 33, 38: ? lambda; " pixels. ": Screenset page1, page2
  Case "0","1","2","3","4","5","6","7","8","9"
    beta = Val(saisie$) / 10
    lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
    For xCoord = -xCentre To xCentre
      max1(xCoord) = 0: max2(xCoord) = 0
    Next
    Screenset 2,2: Locate 34,62: Print Using "#.#"; beta;
    Screenset page1, page2
  End Select
  Do: Loop While Len(Inkey)
Loop

ElectronMobile:'********************** ELECTRON MOBILE ***************************************

For xCoord = -xCentre To xCentre                          'pas de symétrie horizontale.

'*********************************************************************************************
  xCarre = (xCoord + .5) ^ 2
  For yCoord = 0 To yCentre                               'symétrie verticale.
    distance = Sqr(yCoord * yCoord + xCarre)              'distance selon Pythagore.
    If xCoord >= 0 Then
      phi = Atn(yCoord / xCoord)                          'calcul de l'angle phi.
      lambdaAvant =   lambdaLorentz*(Cos(Asin(beta*Sin(phi)))-beta*Cos(phi))'effet Doppler « relatif ».
      lambdaArriere = lambdaLorentz*(Cos(Asin(beta*Sin(phi+pi)))-beta*Cos(phi+pi))
      If yCoord = 0 Then lambdaAvant = lambdaLorentz * (1 - beta): lambdaArriere = lambdaLorentz * (1 + beta)
      rapport = Sqr(lambdaArriere / lambdaAvant)
      x = 2 * pi * distance / lambdaAvant                 'conversion en radians.
      
'************************** FORMULE UNIFIÉE DE M. JOCELYN MARCOTTE ***************************
      amplitude = rapport * (Cos(rotation) * Sin(x) + Sin(rotation) * (1 - Cos(x))) / x
      x = 2 * pi * distance / lambdaArriere
      amplitude = amplitude + (Cos(rotation) * Sin(x) - Sin(rotation) * (1 - Cos(x))) / x / rapport
'*********************************************************************************************
    Elseif xCoord < 0 Then
      phi = Atn(yCoord / xCoord) + pi
      lambdaAvant = lambdaLorentz*(Cos(Asin(beta*Sin(phi+pi)))-beta*Cos(phi+pi))
      lambdaArriere = lambdaLorentz*(Cos(Asin(beta*Sin(phi)))-beta*Cos(phi))
      If yCoord = 0 Then lambdaAvant = lambdaLorentz * (1 - beta): lambdaArriere = lambdaLorentz * (1 + beta)
      rapport = Sqr(lambdaArriere / lambdaAvant)
      x = 2 * pi * distance / lambdaAvant
      amplitude = rapport * (Cos(rotation) * Sin(x) - Sin(rotation) * (1 - Cos(x))) / x
      x = 2 * pi * distance / lambdaArriere
      amplitude = amplitude + (Cos(rotation) * Sin(x) + Sin(rotation) * (1 - Cos(x))) / x / rapport
    End If
    If amplitude > 0 Then
      rouge = amplitude * 1000
      bleu = rouge / 3
      If rouge > 255 Then vert = (rouge - 255) / 2 Else vert = 0
    Else
      vert = -amplitude * 1000
      bleu = vert / 3
      If vert > 255 Then rouge = (vert - 255) / 2 Else rouge = 0
    End If
    If rouge > 255 Then rouge = 255
    If vert  > 255 Then vert  = 255
    If bleu  > 255 Then bleu  = 255
    Pset(xCoord + xCentre2, yCentre - yCoord), Rgb(rouge,vert,bleu)
    Pset(xCoord + xCentre2, yCentre + yCoord), Rgb(rouge,vert,bleu)
    
'************************************** GRAPHIQUE 1-D ****************************************
    If yCoord = 0 Then
      amplitude = 40 * amplitude
      If Abs(amplitude) > max2(xCoord) Then max2(xCoord) = Abs(amplitude)
      Line(xCoord + xCentre2, yCentre2 - max2(xCoord))-(xCoord + xCentre2, yCentre2 + max2(xCoord)), Rgb(150,150,150)
      Line(xCoord + xCentre2, yCentre2)-(xCoord + xCentre2, yCentre2 + amplitude), Rgb(255,255,255)
      Line(xCoord + xCentre2, yPrecedent)-(xCoord + xCentre2, yCentre2 + amplitude)
      yPrecedent = yCentre2 + amplitude
    End If
  Next
Next
Return

Stationnaire:'******************** ELECTRON STATIONNAIRE *************************************

Line(0,yCentre2)-(799,yCentre2), Rgb(180,180,180)
Line(399, 0)-(400,yCentre2), Rgb(128,128,128), bf
For xCoord = -xCentre To xCentre
  xCarre = xCoord ^ 2
  For yCoord = 0 To yCentre                               'symétrie verticale.
    distance = Sqr(yCoord * yCoord + xCarre)              'distance selon Pythagore.
    x = 2 * pi * distance / lambda                        'distance en radians.
    If x Then amplitude  = Sin(x) / x * Sin(rotation+pi/2) Else amplitude = Sin(rotation+pi/2)'sin(x)/x correspond à la quadrature.
    If amplitude > 0 Then
      rouge = amplitude * 2000
      bleu = rouge / 3
      If rouge > 255 Then vert = (rouge - 255) / 2 Else vert = 0
    Else
      vert = -amplitude * 2000
      bleu = vert / 3
      If vert > 255 Then rouge = (vert - 255) / 2 Else rouge = 0
    End If
    If rouge > 255 Then rouge = 255
    If vert  > 255 Then vert  = 255
    If bleu  > 255 Then bleu  = 255
    Pset(xCoord + xCentre, yCentre - yCoord), Rgb(rouge,vert,bleu)
    Pset(xCoord + xCentre, yCentre + yCoord), Rgb(rouge,vert,bleu)

'************************************** GRAPHIQUE 1-D ****************************************
    If yCoord = 0 Then
      amplitude = 80 * amplitude
      If Abs(amplitude) > max1(xCoord) Then max1(xCoord) = Abs(amplitude)
      Line(xCoord + xCentre, yCentre2 - max1(xCoord))-(xCoord + xCentre, yCentre2 + max1(xCoord)), Rgb(150,150,150)
      Line(xCoord + xCentre, yCentre2)-(xCoord + xCentre, yCentre2 + amplitude), Rgb(255,255,255)
      Line(xCoord + xCentre, yPrecedent)-(xCoord + xCentre, yCentre2 + amplitude)
      yPrecedent = yCentre2 + amplitude
    End If
  Next
Next
Return

Initialiser:'************************* INITIALISATION ****************************************
fond  = Rgb(225,225,225)
beta = .7
lambda = 50
lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
hauteur = 300
largeur = 400
xCentre = largeur / 2 - 1
xCentre2 = 800 - largeur / 2
yCentre = hauteur / 2
yCentre2 = hauteur + 82
image = 0
images = 48
contraste = 3 / 4
Screenset 2, 2                                            'créer une page matrice.
Color noir, fond: Cls
For xCoord = -xCentre To xCentre                          'effacer l'enveloppe.
  max1(xCoord) = 0: max2(xCoord) = 0
Next

Locate 26,32:  ?"Phase       y = sin(x) / x"
Locate 27,32:  ?"Quadrature  y = (1 - cos(x)) / x"
Locate 28,32:  ?"Rotation    y = (sin(t + x) - sin(t)) / x"
Locate 30,2:  ?"Les formules de M. Jocelyn Marcotte peuvent reproduire l'‚lectron dans"
Locate 31,2:  ?"toutes ses parties. Elles ne sont pas limit‚es … l'axe du d‚placement."
Locate 32,2:  ?"La formule de rotation a ‚t‚ simplifi‚e par M. Philippe Delmotte."
Locate 33,2:  ?"Zoom: appuyez sur + ou - . Lambda ="; lambda; " pixels."
Locate 34,2:  ?"Vitesse normalis‚e: appuyez sur un chiffre de 1 … 9. Bˆta = ";: Print Using "#.#"; beta;: Print " = v / c"
Locate 35,2:  ?"Initialiser: appuyez sur I."
Locate 36,2:  ?"Pause: appuyez sur P."
Locate 37,2:  ?"QUITTER : Appuyez sur Echap.";
Locate 36,35: ?"Ce programme FreeBASIC peut ˆtre copi‚, modifi‚ ou distribu‚"
Locate 37,35: ?"librement. Gabriel LaFreniŠre, le 5 nov. 2006.  glafreniere.com";
Pcopy 2, page1
Return
