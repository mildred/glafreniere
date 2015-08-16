images = 48: largeur = 400: hauteur = 300
Dim max1(largeur), max2(largeur)
Dim graph1(images, largeur), graph2(images, largeur)
Dim gris1(images, largeur, hauteur), gris2(images, largeur, hauteur)
Dim amplit1(images, largeur, hauteur), amplit2(images, largeur, hauteur)
Dim As Single pi = 4 * Atn(1), x, xCarre, beta, rotation, distance, lambdaLorentz
Dim As Single amplitude, phi, lambdaAvant, lambdaArriere, rapport, facteur, pas, ton
Screen 19,24,3: page1 = 1: Gosub Initialiser

'******************* L'ÉLECTRON SELON LES FORMULES DE M. JOCELYN MARCOTTE ********************
Do
  Swap page1, page2
  screensync
  Screenset page1, page2
  Pcopy 2, page1
  rotation = 2 * pi * image / images                      'rotation de phase selon l'image.
  If calculFait Then
    For x = 1 To largeur
      For y = 0 To hauteur
        Pset(x, y), gris1(image,x,y)
        Pset(x + largeur + 2, y), gris2(image,x,y)
      Next
      Line(x, yCentreGraph - max1(x)) - (x, yCentreGraph + max1(x)), Rgb(150,150,150)
      Line(x, yCentreGraph)-(x, graph1(image, x)), Rgb(255,255,255)
      Line(x, graph1(image, x - 1))-(x, graph1(image, x))
      Line(x + largeur + 2, yCentreGraph - max2(x)) - (x + largeur + 2, yCentreGraph + max2(x)), Rgb(150,150,150)
      Line(x + largeur + 2, yCentreGraph)-(x + largeur + 2, graph2(image, x)), Rgb(255,255,255)
      Line(x + largeur + 2, graph2(image, x - 1))-(x + largeur + 2, graph2(image, x))
    Next
    Line(largeur + 1, hauteur)-(largeur + 2, yCentreGraph), Rgb(150,150,150), bf
  Else    
    Gosub Stationnaire                                    'ondes stationnaires sphériques.
    Gosub ElectronMobile                                  'électron mobile.
  End If
  a$ = Right(Ucase(Inkey),1)
  Select Case a$
  Case Chr(27), "X", "K": End
  Case "I": Gosub Initialiser
  Case "P": Sleep
  Case "+": lambda = lambda + 10: If lambda > 200 Then lambda = 200
    lambdaLorentz = lambda / Sqr(1 - beta ^ 2)            'dilatation de la longueur d'onde.
    facteur = .02 * Sqr(lambda)                           'facteur d'amplitude du relief.
    image = 0: calculFait = 0
    For xCoord = 0 To largeur
      max1(xCoord) = 0: max2(xCoord) = 0                  'effacer l'enveloppe.
    Next
    Screenset 2,2: Locate 33, 60: ? lambda; " pixels. ": Screenset page1, page2
  Case "-": lambda = lambda - 10: If lambda < 20 Then lambda = 20
    lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
    facteur = .02 * Sqr(lambda)
    image = 0: calculFait = 0
    For xCoord = 0 To largeur
      max1(xCoord) = 0: max2(xCoord) = 0
    Next
    Screenset 2,2: Locate 33, 60: ? lambda; " pixels. ": Screenset page1, page2
  Case "0","1","2","3","4","5","6","7","8","9"
    beta = Val(a$) / 10
    lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
    image = 0: calculFait = 0
    For xCoord = 0 To largeur
      max1(xCoord) = 0: max2(xCoord) = 0
    Next
    Screenset 2,2: Locate 34,59: Print Using "#.#"; beta;
    Screenset page1, page2
  End Select
  Do: Loop While Len(Inkey)
  image = image + 1: If image = images + 1 Then image = 1
Loop

ElectronMobile:'********************** ELECTRON MOBILE ***************************************

For xCoord = 0 To largeur
  xDistance = xCoord - xCentre1
  xCarre = (xDistance + .5) ^ 2
  For yCoord = -yCentre To yCentre                        'pas de symétrie verticale.
    distance = 2 * yCoord                                 'distance vert. doublée (plan incliné de 60°).
    distance = Sqr(distance * distance + xCarre)          'distance selon Pythagore.
    If xDistance >= 0 Then
      phi = Atn(2 * yCoord / xDistance)                      'calcul de l'angle phi.
      lambdaAvant =   lambdaLorentz*(Cos(Asin(beta*Sin(phi)))-beta*Cos(phi))'effet Doppler « relatif ».
      lambdaArriere = lambdaLorentz*(Cos(Asin(beta*Sin(phi+pi)))-beta*Cos(phi+pi))
      If yCoord = 0 Then lambdaAvant = lambdaLorentz * (1 - beta): lambdaArriere = lambdaLorentz * (1 + beta)
      rapport = Sqr(lambdaArriere / lambdaAvant)          'racine: utilisé dans les deux sens.
      
'************** FORMULE UNIFIÉE ORIGINALE DE M. JOCELYN MARCOTTE (JUILLET 2006) ***************
'     x = 2 * pi * distance / lambdaAvant                 'conversion en radians.
'     amplitude = rapport * (Cos(rotation) * Sin(x) + Sin(rotation) * (1 - Cos(x))) / x
'     x = 2 * pi * distance / lambdaArriere
'     amplitude = amplitude + (Cos(rotation) * Sin(x) - Sin(rotation) * (1 - Cos(x))) / x / rapport
'**********************************************************************************************

'****************** FORMULE SIMPLIFIÉE DE M. PHILIPPE DELMOTTE (SEPTEMBRE 2006) ***************
      x = 2 * pi * distance / lambdaAvant                 'conversion en radians.
      amplitude = rapport * (Sin(-rotation + x) - Sin(-rotation)) / x
      x = 2 * pi * distance / lambdaArriere
      amplitude = amplitude + (Sin(rotation + x) - Sin(rotation)) / x / rapport
'**********************************************************************************************

Elseif xDistance < 0 Then
      phi = Atn(2 * yCoord / xDistance) + pi
      lambdaAvant = lambdaLorentz*(Cos(Asin(beta*Sin(phi+pi)))-beta*Cos(phi+pi))
      lambdaArriere = lambdaLorentz*(Cos(Asin(beta*Sin(phi)))-beta*Cos(phi))
      If yCoord = 0 Then lambdaAvant = lambdaLorentz * (1 - beta): lambdaArriere = lambdaLorentz * (1 + beta)
      rapport = Sqr(lambdaArriere / lambdaAvant)
      x = 2 * pi * distance / lambdaAvant
      amplitude = rapport * (Sin(rotation + x) - Sin(rotation)) / x
      x = 2 * pi * distance / lambdaArriere
      amplitude = amplitude + (Sin(-rotation + x) - Sin(-rotation)) / x / rapport
    End If
    amplitude = 1.5 * amplitude                           'ajuster le contraste (deux fois moins que stationnaire car il y a deux ondes).
    echelle = 127 + 127 * amplitude                       'valeur sur l'échelle de gris.

    '*************** EFFET DE RELIEF SELON LA PROCÉDURE DE M. PHILIPPE DELMOTTE ******************    

    y = yCoord - facteur * echelle                        '1- Décaler vers le haut selon la luminance.
    y = y + 120 * facteur                                 '2- Déplacer toute l'image vers le bas pour compenser.
    ton = echelle
    If ton > 255 Then ton = 255 Else If ton < 0 Then ton = 0' écrêter.
    If y - j < hauteur Then
      Pset (largeur + 2 + xCoord, y + yCentre), Rgb(ton, ton, ton)
      gris2(image, xCoord, y + yCentre) = Rgb(ton, ton, ton)
    End If
    ecart = y - yPrec                                     'combler l'écart si le pixel
    yPrec = y                                             '      précédent n'était pas voisin.
    If ecart <> 0 Then                                    'estomper avec le pixel précédent.
      pas = (tonPrec - ton) / ecart
      tonPrec = ton
      For j = 1 To ecart - 1                              'combler les espaces vides.
        ton = ton + pas
        If ton > 255 Then ton = 255 Else If ton < 0 Then ton = 0
        If y - j < hauteur Then
          Pset(largeur + 2 + xCoord, y + yCentre - j), Rgb(ton, ton, ton)
          gris2(image, xCoord, y + yCentre - j) = Rgb(ton, ton, ton)
        End If
      Next
    End If      
    
'************************************** GRAPHIQUE 1-D ****************************************
    If yCoord = 0 Then
      amplitude = -20 * amplitude
      graph2(image, xCoord) = yCentreGraph + amplitude
      If Abs(amplitude) > max2(xCoord) Then max2(xCoord) = Abs(amplitude)
      Line(largeur + 2 + xCoord, yCentreGraph - max2(xCoord))-(largeur + 2 + xCoord, yCentreGraph + max2(xCoord)), Rgb(150,150,150)
      Line(largeur + 2 + xCoord, yCentreGraph)-(largeur + 2 + xCoord, yCentreGraph + amplitude), Rgb(255,255,255)
      Line(largeur + 2 + xCoord, yPrecedent)-(largeur + 2 + xCoord, yCentreGraph + amplitude)
      yPrecedent = yCentreGraph + amplitude
    End If
  Next
Next
If image = images Then calculFait = 1
Return

Stationnaire:'******************** ELECTRON STATIONNAIRE *************************************

Line(largeur + 1, hauteur)-(largeur + 2, yCentreGraph), Rgb(150,150,150), bf
For xCoord = 0 To largeur
  xCarre = (xCoord - xCentre1 + .5) ^ 2
  For yCoord = 0 To hauteur
    distance = 2 * (yCentre - yCoord)                     'distance vert. doublée (plan incliné de 60°).
    distance = Sqr(distance * distance + xCarre)          'distance selon Pythagore.
    x = 2 * pi * distance / lambda                        'distance en radians.
    If x Then amplitude  = Sin(x) / x * Sin(rotation+pi/2) Else amplitude = Sin(rotation+pi/2)
    amplitude = 3 * amplitude                             'ajuster le contraste
    echelle = 127 + 127 * amplitude                       'valeur sur l'échelle de gris.
    
'*************** EFFET DE RELIEF SELON LA PROCÉDURE DE M. PHILIPPE DELMOTTE ******************    

    y = yCoord - facteur * echelle                        '1- Décaler vers le haut selon la luminance.
    y = y + 120 * facteur                                 '2- Déplacer toute l'image vers le bas pour compenser.
    ton = echelle
    If ton > 255 Then ton = 255 Else If ton < 0 Then ton = 0' écrêter.
    If y - j < hauteur Then
      Pset (xCoord, y), Rgb(ton, ton, ton)                'imprimer sur le pixel décalé.
      gris1(image, xCoord, y) = Rgb(ton, ton, ton)
    End If
    ecart = y - yPrec                                     'combler l'écart si le pixel
    yPrec = y                                             '      précédent n'était pas voisin.
    If ecart <> 0 Then                                    'estomper avec le pixel précédent.
      pas = (tonPrec - ton) / ecart
      tonPrec = ton
      For j = 1 To ecart - 1                              'combler les espaces vides.
        ton = ton + pas
        If ton > 255 Then ton = 255 Else If ton < 0 Then ton = 0
        If y - j < hauteur Then
          Pset(xCoord, y - j), Rgb(ton, ton, ton)
          gris1(image, xCoord, y - j) = Rgb(ton, ton, ton)
        End If
      Next
    End If        

'************************************** GRAPHIQUE 1-D ****************************************
    If yCoord = yCentre Then
      amplitude = -20 * amplitude
      graph1(image, xCoord) = yCentreGraph + amplitude
      If Abs(amplitude) > max1(xCoord) Then max1(xCoord) = Abs(amplitude)
      Line(xCoord, yCentreGraph - max1(xCoord))-(xCoord, yCentreGraph + max1(xCoord)), Rgb(150,150,150)
      Line(xCoord, yCentreGraph)-(xCoord, yCentreGraph + amplitude), Rgb(255,255,255)
      Line(xCoord, yPrecGraph)-(xCoord, yCentreGraph + amplitude)
      yPrecGraph = yCentreGraph + amplitude
    End If
  Next
Next
Return

Initialiser:'************************* INITIALISATION ****************************************
fond  = Rgb(225,225,225)
beta = .7
lambda = 50
lambdaLorentz = lambda / Sqr(1 - beta ^ 2)
xCentre1 = largeur / 2
xCentre2 = 800 - largeur / 2
yCentre = hauteur / 2
yCentreGraph = hauteur + 86
Screenset 2, 2                                            'créer une page matrice.
Color noir, fond: Cls
calculFait = 0
image = 0
facteur = .02 * Sqr(lambda)                               'facteur d'amplitude du relief.
For xCoord = 0 To largeur                                 'effacer l'enveloppe.
  max1(xCoord) = 0: max2(xCoord) = 0
Next

Locate 26,32:  ?"Phase       y = sin(x) / x"
Locate 27,32:  ?"Quadrature  y = (1 - cos(x)) / x"
Locate 28,32:  ?"Rotation    y = (sin(t + x) - sin(t)) / x"
Locate 30,2:  ?"Ce programme d‚montre que les formules de M. Jocelyn Marcotte"
Locate 31,2:  ?"peuvent reproduire l'‚lectron (l'onde de LaFreniŠre) dans toutes ses"
Locate 32,2:  ?"parties. La formule de rotation simplifi‚e est de M. Philippe Delmotte."
Locate 33,6:  ?"- Zoom: appuyez sur + ou - .................. lambda ="; lambda; " pixels."
Locate 34,6:  ?"- Vitesse: appuyez sur un chiffre de 0 … 9... bˆta = ";: Print Using "#.#"; beta;: Print " = v / c."
Locate 35,2:  ?"Initialiser: appuyez sur I."
Locate 36,2:  ?"Pause: appuyez sur P."
Locate 37,2:  ?"QUITTER : Appuyez sur Echap.";
Locate 36,34: ?"Ce programme FreeBASIC peut ˆtre copi‚, modifi‚ ou distribu‚"
Locate 37,34: ?"librement. Gabriel LaFreniŠre, le 29 sept. 2006.  glafreniere.com";
Pcopy 2, page1
Return
