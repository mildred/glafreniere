cote = 440  'nombre de particules sur un coté du champ.
Dim precedent(-1 To cote)
Dim As Single M(-1 To cote, -1 To cote)
Dim As Single I(-1 To cote, -1 To cote) 
Dim As Single P(-1 To cote+1, -1 To cote)
Dim As Single potentiel(0 To 10 * cote)'                  'principe de Huygens, sous-multiples..
Dim As Single potentiel2(-1 To cote, -1 To cote)
Dim As Single pi, ondelette, angle, lambda, lambda2, xCarre, yCarre, arcSinus, xDistance
Dim As Single xCoord, distNormale, periode, distance, rotation, amplitude, phi, xPoint
Dim As Single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance, vitesse
Dim As Single betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, luminance
Screen 19,24,3: Gosub Initialisation: Gosub Huygens

Do'                 MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS, 2 jan 2006.

  For y = 0 To cote - 1
    I(0,y) = (P(-1,y) + P(1,y) + P(0,y-1) + P(0,y+1)) / 4 - P(0,y)' un cran à l'avance.
  Next

  For x = 0 To cote - 1
    For y = 0 To cote - 1
'------------------------------ CALCUL SELON LA LOI DE HOOKE ------------------------
      I(x+1,y) = (P(x,y) + P(x+2,y) + P(x+1,y-1) + P(x+1,y+1)) / 4 - P(x+1,y)' I => Influence
      P(x, y) = P(x, y) + I(x, y) + M(x, y)                                  ' P => Potentiel
      M(x, y) = M(x, y) + I(x, y)                                            ' M => Mémoire
'------------------------------------- FIN DU CALCUL --------------------------------
    Next
  Next
  
  If reflexion = 2 Then                                   'réflexion "molle".
    For x = 0 To cote - 1
      P(x, -1) = P(x, 0): P(x, cote) = P(x, cote - 1)
    Next
    For y = 0 To cote - 1
      P(-1, y) = P(0, y): P(cote, y) = P(cote - 1, y)
    Next
  Elseif reflexion = 0 Then                               'pas de réflexion.
    For x = 0 To cote - 1
      P(x, -1) = P(x, 0) - 2 * M(x, 0): P(x, cote) = P(x, cote - 1) - 2 * M(x, cote - 1)
    Next
    For y = 0 To cote - 1
      P(-1, y) = P(0, y) - 2 * M(0, y): P(cote, y) = P(cote - 1, y) - 2 * M(cote - 1, y)
    Next
  End If                                                  'réflexion dure par défaut.
  
  
  xPoint = xPoint + vitesse: If xPoint > cote Then xPoint = xPoint - cote
  If afficher = 0 Then                                    'afficher une fois sur deux.
    Swap page1, page2
    Screenset page1, page2
    Pcopy 2, page1
    Line(xPoint, graphique - 10)-(xPoint, graphique + 10),noir
    Line(cote - xPoint, graphique - 10)-(cote - xPoint, graphique + 10),noir
    Color bleu
    Select Case reflexion                                 'identifier le type de réflexion.
      Case 0: Locate 29, 58: Print ligne29$
      Case 1: Locate 30, 58: Print ligne30$
      Case 2: Locate 31, 58: Print ligne31$
    End Select
    Color noir  
    
    For x = 0 To cote - 1                                 'diagramme principal.
      For y = 0 To cote - 1
        luminance = 100 * (P(x, y) + 1)
        If luminance > 255 Then luminance = 255
        If luminance < 0 Then luminance = 0
        Pset (x, y), Rgb(luminance,luminance,luminance)
      Next
    Next
    
    For x = 0 To cote - 1                                 'graphique 1D.
      courbe% = graphique - 5 * P(x, cote / 2)
      Pset (x, precedent(x)), &hE1E1E1                    'rgb 225, ou 14803425.
      Pset (x, courbe%), 0
      precedent(x) = courbe%
    Next
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
    If Len(saisie$) Then
      Select Case saisie$
        Case "A": Gosub Inverser: Do: Loop While Len(inkey)'éviter les actions répétées.
        Case "B": Gosub Perturbation
        Case "C": Gosub EffacerMemoire
        Case "D": Gosub Doppler
        Case "E": Gosub ToutEffacer
        Case "I": Gosub Huygens2: reflexion = 2
        Case "M": Run "Ether00.exe"
        Case "K+":Run "Ether04.exe"                       'flèche gauche.
        Case "M+":Run "Ether06.exe"                       'flèche droite.        
        Case "k+",CHR$(27): End
        Case "X": reflexion = 0
        Case "Y": reflexion = 1
        Case "Z": reflexion = 2
      End Select
    End If
    
    If clic = 0 Then Getmouse xSouris, ySouris, , clic    'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
    'locate 30, 12: print xSouris; ySouris; clic; ligne; "    ";
    If ligne < 31 Then
      If ligne < 24 Or xSouris > 736 Or xSouris < 458 Then ligne = 0
    Elseif ligne > 34 Then
      If ligne > 37 Or xSouris < 304 Or xSouris > 496 Then ligne = 0
    End If
    If clic = 2 Then Gosub ToutEffacer
    If clic = 1 Then
      If xSouris < cote And ySouris < cote Then ligne = 1 'clic sur l'écran.
      Select Case ligne
        Case 1 : Gosub RondsDansLeau
        Case 24: Gosub Inverser
        Case 25: Gosub Perturbation
        Case 26: Gosub EffacerMemoire
        Case 27: Gosub Doppler
        Case 28: Gosub ToutEffacer
        Case 29: reflexion = 0
        Case 30: reflexion = 1
        Case 31: reflexion = 2
      End Select
    End If

    Color , turquoise: Locate ligne, 58
    Select Case ligne                                     'rehausser l'affichage.
      Case 24: Print ligne24$
      Case 25: Print ligne25$
      Case 26: Print ligne26$
      Case 27: Print ligne27$
      Case 28: Print ligne28$
      Case 29: If reflexion = 0 Then Else Print ligne29$
      Case 30: If reflexion = 1 Then Else Print ligne30$
      Case 31: If reflexion = 2 Then Else Print ligne31$
        
      Case 35: Locate, 39: Print ligne35$: If clic = 1 Then Gosub Huygens2: reflexion = 2
      Case 36: Locate, 39: Print ligne36$: If clic = 1 Then End
      Case 37: Locate, 39: Print ligne37$;
               If xSouris < xCentre Then
                 Gosub flecheGauche
                 If clic = 1 Then Run "Ether04.exe"
                Else
                  Gosub flecheDroite
                 If clic = 1 Then  Run "Ether06.exe"
                End If      
    End Select
    Color , fond
'    locate 33, 3: print using "#.## sec"; timer - temps
'    temps = timer
  End If
  Getmouse xSouris, ySouris, , clic                       'vérifier à chaque tour.
  afficher = afficher + 1
  If clic Or afficher = 2 Then afficher = 0               'afficher une fois sur deux.
Loop

Doppler:'----------------------------- EFFET DOPPLER -----------------------------------------
reflexion = 0
xPoint = cote / 2 + lambda / 8
For x = -1 To cote
  xDistance = x - cote / 4
  xCarre = xDistance * xDistance
  For y = -1 To cote
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    distance = Sqr(xCarre + yCarre)
    If xDistance > 0 Then                                 'distinguer les quadrants.
      phi = Atn(yDistance / xDistance) + pi               'phi est l'angle de propagation.
    Elseif xDistance < 0 Then phi = Atn(yDistance / xDistance)
    Else phi = pi / 2                                     'éviter la division par zéro.
    End If
    lambda2=lambda*(Cos(Asin(beta*Sin(phi)))-beta*Cos(phi))'effet Doppler « relatif ».
    deuxPiDistanceSurLambda = 2 * pi * distance / lambda2 'différence de marche selon l'angle.
    amplitudeSinus = Sin(deuxPiDistanceSurLambda)
    amplitudeCosinus = .12 *  Cos(deuxPiDistanceSurLambda)'.12 varie selon la longueur d'onde.
    If distance > lambda2 * 2 Then amplitude = lambda2 / distance Else amplitude = .5
    If distance > 10 * lambda2 Then amplitude = 0
    P(x, y) = amplitude * amplitudeSinus
    M(x, y) = amplitude * amplitudeCosinus
  Next
Next
Do: Loop While Len(inkey)
Return

EffacerMemoire:'----- SUPPRIMER LES DONNÉES RELATIVES A LA MÉMOIRE--------------------------
If clic > 0 Then
  Do
    Getmouse xSouris, ySouris, , clic'                    'éviter des actions à répétition.
  Loop While clic > 0
End If

For x = -1 To cote
  For y = -1 To cote
    M(x, y) = 0
  Next
Next
Do: Loop While Len(inkey)
Return

ToutEffacer:'---------------------- INITIALISER LES VARIABLES --------------------------------
For x = -1 To cote
  For y = -1 To cote
    M(x, y) = 0
    P(x, y) = 0
  Next
Next
Return

Inverser:'------------------------ INVERSER LE SENS DES ONDES --------------------------------
If clic > 0 Then
  Do
    Getmouse xSouris, ySouris, , clic'                    'éviter des actions à répétition.
  Loop While clic > 0
End If

For x = -1 To cote
  For y = -1 To cote
    M(x, y) = -M(x, y)
   'P(x, y)= -P(x, y)                                     'alternative.
  Next
Next
Do: Loop While Len(inkey)
Return

Huygens:'-------------- DISTRIBUTION DU POTENTIEL SELON LE PRINCIPE DE HUYGENS ---------------
xPoint = cote / 2 + lambda / 8

For x = 0 To 8 * cote                                     'surmultiplier: précision accrue.
  amplitudeSinus = 0
  amplitudeCosinus = 0
  deuxPiDistanceSurLambda = (2 * pi * x / lambda) / 8
  For angle = 0 To pi Step ondelette
      periode = deuxPiDistanceSurLambda * Cos(angle)
      amplitudeSinus = amplitudeSinus + Sin(periode)
      amplitudeCosinus = amplitudeCosinus + Cos(periode)
  Next
  amplitude = Sgn(amplitudeCosinus) * Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
  'if amplitudeCosinus < 0 then amplitude = -amplitude contourné par SGN.
  xCoord = x / 1000
  distNormale = pi ^ (-xCoord ^ 2)                        'distribution normale approchée.
  potentiel(x) = pi * ondelette * amplitude * distNormale 'potentiel selon 2 * distance.
Next

For x = 0 To cote
  xDistance = x - cote / 2
  xCarre = xDistance * xDistance
  For y = 0 To cote
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    dist = 8 * Sqr(xCarre + yCarre)
    P(x, y) = potentiel(dist)                             'distribution du potentiel.
    potentiel2(x, y) = P(x, y)
  Next
Next

temps = Timer
Return

Huygens2:'------------------ REDISTRIBUTION DU POTENTIEL SELON HUYGENS -----------------------
xPoint = cote / 2 + lambda / 8
For x = -1 To cote
  For y = -1 To cote
    M(x, y) = 0
    P(x, y) = potentiel2(x, y)                            'le potentiel est déjà calculé.
  Next
Next
Do: Loop While Len(inkey)
Return

RondsDansLeau:
For x = -1 To cote
  x2 = xSouris - x + cote / 2
  If x2 < 0 Or x2 > cote Then x2 = -1
  For y = -1 To cote
    y2 = ySouris - y + cote / 2
    If y2 < 0 Or y2 > cote Then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2) / 4            'ajout d'un nouveau potentiel.
  Next
Next
reflexion = 0
Return

Perturbation:'---------- PERTURBATION CENTRALE SELON LA DISTRIBUTION NORMALE -----------------
xPoint = cote / 2 + lambda / 8
For x = -1 To cote
  xDistance = x - cote / 2
  xCarre = xDistance * xDistance
  For y = -1 To cote
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    distance = Sqr(xCarre + yCarre)
    xCoord = distance / 15
    distNormale = pi ^ (-xCoord ^ 2)                      'distribution normale approx.
    P(x, y) = 10 * distNormale +.00001
    M(x, y) = 0: I(x, y) = 0
  Next
Next
Do: Loop While Len(inkey)
reflexion = 2                                             'réflexion molle.
Return

flecheGauche:
Line (xgg+6,yFleche-4)-(xgg+8,yFleche-4),noir
Line (xgg+4,yFleche-3)-(xgg+8,yFleche-3),noir
Line (xgg+2,yFleche-2)-(xgg+8,yFleche-2),noir
Line (xgg,yFleche-1)-(xgd,yFleche-1),noir
Line (xgg-2,yFleche)-(xgd,yFleche),noir
Line (xgg,yFleche+1)-(xgd,yFleche+1),noir
Line (xgg+2,yFleche+2)-(xgg+8,yFleche+2),noir
Line (xgg+4,yFleche+3)-(xgg+8,yFleche+3),noir
Line (xgg+6,yFleche+4)-(xgg+8,yFleche+4),noir
Return

flecheDroite:
Line (xdd-8,yFleche-4)-(xdd-6,yFleche-4),noir
Line (xdd-8,yFleche-3)-(xdd-4,yFleche-3),noir
Line (xdd-8,yFleche-2)-(xdd-2,yFleche-2),noir
Line (xdg,yFleche-1)-(xdd,yFleche-1),noir
Line (xdg,yFleche)-(xdd+2,yFleche),noir
Line (xdg,yFleche+1)-(xdd,yFleche+1),noir
Line (xdd-8,yFleche+2)-(xdd-2,yFleche+2),noir
Line (xdd-8,yFleche+3)-(xdd-4,yFleche+3),noir
Line (xdd-8,yFleche+4)-(xdd-6,yFleche+4),noir
Return

Initialisation:'------------------------ INITIALISATION --------------------------------------
fond = Rgb(225,225,225)
bleu = Rgb(0,0,255)
vert = Rgb(0,150,0)
turquoise = Rgb (230, 255, 255)
pi = 4 * Atn(1)
beta = .5                                                 'moitié de la vitesse de la lumière.
page1 = 1
lambda = cote / 20
vitesse = .5                                              'vitesse de l'onde.
reflexion = 2                                             'réflexion molle.
ondelette = pi / 100                                      'cent ondelettes est un minimum.
xCentre = 400
yCentre = cote / 2
graphique = cote + 50
xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yFleche = 584

Screenset 2,2: Color noir, fond: Cls
titre$ = "L'ETHER VIRTUEL": Print titre$
xOrig = 800 - (800 - cote) / 2 - Len(titre$) * 8 : 
yOrig = 2: accent = xOrig + 37
For x = 0 To 8 * Len(titre$)                              'agrandir l'en-tête.
  For y = 0 To 16
    If Point(x,y) = 0 Then
      Pset(2 * x + xOrig, 2 * y + yOrig), Rgb(0,0,255)
      Pset(2 * x + xOrig + 1, 2 * y + yOrig), Rgb(0,0,255)
      Pset(2 * x + xOrig, 2 * y + yOrig + 1), Rgb(0,0,255)
      Pset(2 * x + xOrig + 1, 2 * y + yOrig + 1), Rgb(0,0,255)
    End If    
  Next
Next
Line(accent,4)-(accent + 4,4), Rgb(0,0,255):Line(accent + 1,3)-(accent + 5,3), Rgb(0,0,255):Line(accent + 2,2)-(accent + 6,2), Rgb(0,0,255)'accent du titre.
Color fond, fond: Locate 1, 1: Print titre$' effacer.
Color 0, fond: Locate 5
Locate, 59: Print "Ce programme montre des ondes circulai-  "
Locate, 59: Print "res dans un espace … deux dimensions.    "
Print
Locate, 59: Print "Si vous cliquez sur ® Inverser le sens   "
Locate, 59: Print "des ondes ¯, ci-dessous, celles-ci re-   "
Locate, 59: Print "tourneront vers leur origine. Il s'agit  "
Locate, 59: Print "du fameux retournement temporel dont on  "
Locate, 59: Print "a beaucoup parl‚ en acoustique.          "
Print
Locate, 59: Print "L'‚ther virtuel confirme que le noyau du "
Locate, 59: Print "systŠme, calcul‚ ici selon le principe   "
Locate, 59: Print "de Huygens, mesure trois-quarts d'onde.  "
Print
Locate, 59: Print "Cette invention ne se limite pas … l'‚-  "
Locate, 59: Print "ther. Elle s'applique … toutes les ondes,"
Locate, 59: Print "ce qui signifie que les chercheurs sp‚-  "
Locate, 59: Print "cialis‚s en optique, en acoustique et en "
Locate, 59: Print "‚lectronique pourront aussi l'utiliser."

ligne24$ = " A - Inverser le sens des ondes.        ": Locate 24, 58: Print ligne24$
ligne25$ = " B - Impulsion gaussienne simple.       ": Locate 25, 58: Print ligne25$
ligne26$ = " C - Initialiser la m‚moire.            ": Locate 26, 58: Print ligne26$
ligne27$ = " D - Convergentes avec effet Doppler.   ": Locate 27, 58: Print ligne27$
ligne28$ = " E - Tout effacer. Souris: bouton droit.": Locate 28, 58: Print ligne28$
ligne29$ = " X - Pas de r‚flexion.                  ": Locate 29, 58: Print ligne29$
ligne30$ = " Y - R‚flexion dure.                    ": Locate 30, 58: Print ligne30$
ligne31$ = " Z - R‚flexion molle.                   ": Locate 31, 58: Print ligne31$
ligne35$ = "    I - Initialiser.    ": Locate 35, 39: Print ligne35$
ligne36$ = "    Quitter (Echap).    ": Locate 36, 39: Print ligne36$
ligne37$ = "                        "
Gosub flecheGauche: Gosub flecheDroite
Locate 20, 5: Print "Calcul selon Huygens en cours..."
Locate 29, 2: Print "Cliquez sur l'‚cran.            Bouton droit: effacer."
Color vert
Locate 3, 60: Print "Une invention de M. Philippe Delmotte."
Locate 35
Locate , 2:  Print "Mes remerciements chaleureux …"
Locate , 2:  Print "M. Anselme Dewavrin pour son aide."
Locate , 2:  Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate , 68: Print "Le 14 oct. 2006. Ce programme "
Locate , 68: Print "FreeBASIC peut ˆtre distribu‚,"
Locate , 68: Print "copi‚ ou modifi‚ librement.   ";
Color noir
Line(cote / 2 - .375 * lambda, graphique - 20)-(cote / 2 - .375 * lambda, graphique + 20),Rgb(175,175,175)' repère 3/4 d'onde.
Line(cote / 2 + .375 * lambda, graphique - 20)-(cote / 2 + .375 * lambda, graphique + 20),Rgb(175,175,175)
Line(0, graphique)-(cote - 1, graphique),Rgb(175,175,175)
Pcopy 2, page1
Return
