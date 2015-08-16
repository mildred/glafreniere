page1 = 1: images = 96: longueur = 800
Dim As Single amplitude2DStat(longueur), amplitude3DStat(longueur)
Dim As Single max2DProg(longueur), max3DProg(longueur), courbe2DStat(longueur)
Dim As Single amplitude2DProg(images, longueur)
Dim As Single amplitudeSinus, amplitudeCosinus, normaliser, phase, amplitude
Dim As Single amplitudeSinus3D(longueur), amplitudeCosinus3D(longueur)
Dim As Single amplitudeSinus2D(longueur), amplitudeCosinus2D(longueur)
Dim As Single pi, distance, pasProg, pasStat, angle, differenceDeMarche
Dim As Single xCoord, yCoord, periode, cosPeriode, sinPeriode, interieur, exterieur, piSurDeux
Screen 19,24,3: Gosub Initialisation

Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  screensync                                              'régulariser l'animation.
  
  If progressives Then'----------------- DIAGRAMME -----------------------------------------
    If dimensions = 3 Then Gosub Progressives3D Else Gosub Progressives2D
  Else
    If dimensions = 3 Then Gosub Stationnaires3D Else Gosub Stationnaires2D
  End If

'--------------------------------------- SAISIE CLAVIER --------------------------------------
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  If Len(saisie$) Then
    Do: Loop While Len(Inkey)
    bitmap = 0
    Select Case saisie$
      Case "A": If dimensions = 3 Then dimensions = 2 Else dimensions = 3
                image = -1: Gosub MiseAJour
      Case "B": If progressives Then progressives = 0 Else progressives = 1
                image = -1: Gosub MiseAJour
      Case "I": Gosub Initialisation
      Case "k+",Chr$(27): End
      Case "M": Run "Ether00.exe"
      Case "K+":Run "Ether05.exe"                         'flèche gauche.
      Case "M+":Run "Ether07.exe"                         'flèche droite
    End Select
  End If
  
'---------------------------------------- SAISIE SOURIS --------------------------------------
  Getmouse xSouris, ySouris, , clic
  Do
    If clic = 1 Then Getmouse xSouris, ySouris, , clac    'attendre le relâchement.
  Loop Until clac <> 1
  ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
  If ligne > 32 And ligne < 38 Then
    If xSouris < 304 Or xSouris > 512 Then ligne = 0
  Else ligne = 0  
  End If
'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  Color noir, turquoise
  Locate ligne, 39
  Select Case ligne
    Case 33: If dimensions = 3 Then Print ligne33a$ Else Print ligne33b$
    Case 34: If progressives Then Print ligne34a$ Else Print ligne34b$
    Case 35: Print ligne35$
    Case 36: Print ligne36$
    Case 37: Print ligne37$;: If xSouris < 400 Then Gosub flecheGauche Else Gosub flecheDroite
  End Select
  Color noir, fond
'------------------------------------ ACTIONS SUITE À UN CLIC --------------------------------
  If clic = 1 Then
    bitmap = 0
    Select Case ligne
      Case 33: If dimensions = 3 Then dimensions = 2 Else dimensions = 3
                image = -1: Gosub MiseAJour
      Case 34: If progressives Then progressives = 0 Else progressives = 1
                image = -1: Gosub MiseAJour
      Case 35: Gosub Initialisation                 
      Case 36: End
      Case 37: If xSouris < 400 Then Run "Ether05.exe" Else Run "Ether07.exe"
    End Select
  End If
  image = image + 1
  If image = images Then image = 0
Loop
'********************************* FIN DE LA BOUCLE PRINCIPALE *******************************

'-------------------------------------- DESSIN DES FLÈCHES -----------------------------------
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

Progressives2D:'----- ONDES PROGRESSIVES SELON HUYGENS, DEUX DIMENSIONS ----------------------

If OKprog2D Then Goto prog2D                              'calcul selon Huygens complété.

'Effectuer le calcul selon le principle de Huygens
'-------------------------------------------------
For pixel = 0 To longueur - 1
  amplitudeSinus = 0                                      'initialiser à chaque pixel
  amplitudeCosinus = 0
  xCoord = xCentre - pixel

  For angle = 0 To piSurDeux Step pasProg                 'balayer la sphère sur 90° seulement.
    phase = xCoord * Cos(angle) * 2*pi/lambda
    amplitudeSinus   += Sin(phase)
    amplitudeCosinus += Cos(phase)
  Next

  amplitudeSinus2D(pixel)   = AmplitudeSinus
  amplitudeCosinus2D(pixel) = AmplitudeCosinus
Next

'Générer les courbes normalisée à afficher
'-----------------------------------------
normaliser = 120 / amplitudeCosinus2D(xCentre)            'amplitude maximum en pixels.
For pixel = 0 To longueur - 1
  amplitudeSinus2D(pixel)   *= normaliser
  amplitudeCosinus2D(pixel) *= normaliser
  max2DProg(pixel)           = Sqr(amplitudeSinus2D(pixel)^2 + amplitudeCosinus2D(pixel)^2)
Next

OKprog2D = 1                                              'indique si le calcul est terminé.
prog2D:                                                   'raccourci si la courbe est connue.

'Dessiner les courbes
'--------------------                                     'préparation des paramètres
periode = 2 * pi * image / images
cosPeriode = Cos(periode)
sinPeriode = Sin(periode)

ondePrecedent = amplitudeSinus3D(0) * sinPeriode + amplitudeCosinus3D(0) * cosPeriode 'formule de l'onde progressive

For pixel = 0 To longueur - 1

  xCoord = Abs(pixel - xCentre) * 2 * pi / lambda         'x = 2 * pi * distance / lambda
  If xCoord < 0.0001 Then xCoord = 0.0001                 'pour éviter la division par 0

  yCoord = 240 / Sqr(xCoord * pi / 2)                   'y = 1 / sqr(x * pi / 2)
  If xCoord > pi / 3 Then
    Line(pixel, yCentre + yPrecedent)-(pixel, yCentre + yCoord), rouge
    Line(pixel, yCentre - yPrecedent)-(pixel, yCentre - yCoord), rouge
  End If
  yPrecedent = yCoord

  onde  = amplitudeCosinus2D(pixel) * cosPeriode - amplitudeSinus2D(pixel) * sinPeriode 'formule de l'onde progressive
  onde2 = amplitudeCosinus2D(longueur-1-pixel) * cosPeriode - amplitudeSinus2D(longueur-1-pixel) * sinPeriode 'formule de l'onde progressive
  Line(pixel,yCentre)-(pixel,yCentre-onde),blanc          'zone blanche.
  Pset(pixel, yCentre), gris                              'ligne grise centrale.
  Line(pixel, yCentre - ondePrecedent)-(pixel, yCentre - onde), noir
  Pset(pixel, yCentre - onde2), noir
  Pset(pixel, yCentre - onde - onde2), noir                'somme des deux courbes: stationnaire.
  ondePrecedent = onde

  Pset (pixel, yCentre - max2DProg(pixel)), noir 'courbe des maxima.
  Pset (pixel, yCentre + max2DProg(pixel)), noir

Next

For x = -.125 * lambda To 400 Step lambdaSurDeux
  Line (400 + x, yCentre - 10)-(400 + x, yCentre + 10), gris
  Line (400 - x, yCentre - 10)-(400 - x, yCentre + 10), gris
Next
Return

Progressives3D:'----- ONDES PROGRESSIVES SELON HUYGENS, TROIS DIMENSIONS ---------------------

If OKprog3D Then Goto prog3D                              'calcul selon Huygens complété.

'Effectuer le calcul selon le principle de Huygens
'-------------------------------------------------
For pixel = 0 To longueur - 1
  amplitudeSinus = 0                                      'initialiser à chaque pixel
  amplitudeCosinus = 0
  xCoord = xCentre - pixel

  For angle = 0 To piSurDeux Step pasProg                 'balayer la sphère sur 90° seulement.
    phase = xCoord * Cos(angle) * 2*pi/lambda
    amplitudeSinus   += Sin(angle) * Sin(phase)           'sin(angle) indique un rayon.
    amplitudeCosinus += Sin(angle) * Cos(phase)
  Next

  amplitudeSinus3D(pixel)   = AmplitudeSinus
  amplitudeCosinus3D(pixel) = AmplitudeCosinus
Next

'Générer les courbes à afficher
'------------------------------
normaliser = 120 / amplitudeCosinus3D(xCentre)            'amplitude maximum en pixels.
For pixel = 0 To longueur - 1
  amplitudeSinus3D(pixel)   *= normaliser
  amplitudeCosinus3D(pixel) *= normaliser
  max3DProg(pixel)           = Sqr(amplitudeSinus3D(pixel)^2 + amplitudeCosinus3D(pixel)^2)
Next

OKprog3D = 1                                              'indique si le calcul est terminé.
prog3D:                                                   'raccourci si la courbe est connue.

'Dessiner les courbes
'--------------------                                     'préparation des paramètres
periode = 2 * pi * image / images
cosPeriode = Cos(periode)
sinPeriode = Sin(periode)

yPrecedent = 240 / (xCentre * 2 * pi / lambda)            'formule: y = 1 / x
ondePrecedent = amplitudeSinus3D(0) * sinPeriode + amplitudeCosinus3D(0) * cosPeriode 'formule de l'onde progressive

For pixel = 0 To longueur - 1

  xCoord = Abs(pixel - xCentre) * 2 * pi / lambda         'x = 2 * pi * distance / lambda
  If xCoord < 0.0001 Then xCoord = 0.0001                 'pour éviter la division par 0

  If xCoord < pi / 2 Then
    yCoord = 240 * Sin(xCoord) / xCoord                   'formule: y = sin(x) / x
    Line(pixel, yCentre + yPrecedent)-(pixel, yCentre + yCoord), Rgb(0,200,100)
    Line(pixel, yCentre - yPrecedent)-(pixel, yCentre - yCoord), Rgb(0,200,100)
  Else
    yCoord = 240 / xCoord                                 'formule: y = 1 / x
    Line(pixel, yCentre + yPrecedent)-(pixel, yCentre + yCoord), rouge
    Line(pixel, yCentre - yPrecedent)-(pixel, yCentre - yCoord), rouge
  End If
  yPrecedent = yCoord

  onde  = amplitudeCosinus3D(pixel) * cosPeriode - amplitudeSinus3D(pixel) * sinPeriode 'formule de l'onde progressive
  onde2 = amplitudeCosinus3D(longueur-1-pixel) * cosPeriode - amplitudeSinus3D(longueur-1-pixel) * sinPeriode 'formule de l'onde progressive
  Line(pixel,yCentre)-(pixel,yCentre-onde),blanc          'zone blanche.
  Pset(pixel, yCentre), gris                              'ligne grise centrale.
  Line(pixel, yCentre - ondePrecedent)-(pixel, yCentre - onde), noir
  Pset(pixel, yCentre - onde2), noir
  Pset(pixel, yCentre - onde - onde2), noir                'somme des deux courbes: stationnaire.
  ondePrecedent = onde

  Pset (pixel, yCentre - max3DProg(pixel)), noir 'courbe des maxima.
  Pset (pixel, yCentre + max3DProg(pixel)), noir

Next

For x = 0 To 400 Step lambdaSurDeux                       'repères des longueurs d'onde.
  Line (400 + x, yCentre - 10)-(400 + x, yCentre + 10), gris
  Line (400 - x, yCentre - 10)-(400 - x, yCentre + 10), gris
Next
Return

Stationnaires2D:'---- ONDES STATIONNAIRES SELON HUYGENS, DEUX DIMENSIONS ---------------------

If OKstat2D Then Goto stat2D                              'calcul selon Huygens complété.
For pixel = 0 To 799 - xCentre                            'distribution de l'amplitude.
  amplitudeSinus = 0
  amplitudeCosinus = 0
  For angle = 0 To pi Step pasStat                        'balayer l'arc de cercle sur 180°.
    differenceDeMarche = pixel * Cos(angle) / lambda
    phase = 2 * pi * differenceDeMarche
    amplitudeSinus = amplitudeSinus + Sin(phase)
    amplitudeCosinus = amplitudeCosinus + Cos(phase)
  Next
  If pixel = 0 Then normaliser = 2 * pixels / amplitudeCosinus' amplitude en pixels.
  amplitude2DStat(pixel) = normaliser * amplitudeCosinus  'quadrature (sinus -> phase).
  courbe2DStat(pixel) = normaliser * Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
Next pixel                                                'vaut la racine carrée de l'énergie.
OKstat2D = 1
stat2D:                                                   'raccourci si la courbe est connue.
periode = 2 * pi * image / images
For pixel = 0 To 799 - xCentre                            'afficher la courbe.
  yCoord = amplitude2DStat(pixel) * Cos(periode)          'rotation selon l'image en cours.
  Line (xCentre + pixel, yCentre)-(xCentre + pixel, yCentre - yCoord), blanc 'zone blanche.
  Line (xCentre - pixel, yCentre)-(xCentre - pixel, yCentre - yCoord), blanc
  Pset(xCentre + pixel, yCentre), gris                    'ligne grise centrale.
  Pset(xCentre - pixel, yCentre), gris
  Pset (xCentre + pixel, yCentre - yCoord), noir          'courbe de l'amplitude.
  Pset (xCentre - pixel, yCentre - yCoord), noir
  xCoord = 2 * pi * pixel / lambda                        'x = 2 * pi * distance / lambda
'  yCoord = 350 * sin(xCoord + pi / 4) / (1 + xCoord)     'imprécis, peut-être à développer. 
  If xCoord > pi / 3 Then
    yCoord = 240 * Sin(xCoord+pi/4) / Sqr(xCoord * pi/2)  'y = sin(x + pi / 4) / sqr(x * pi / 2)
    Line(xCentre + pixel, yCentre + yVertPrecedent)-(xCentre + pixel, yCentre + yCoord), Rgb(0,200,100)
    Line(xCentre + pixel, yCentre - yVertPrecedent)-(xCentre + pixel, yCentre - yCoord), Rgb(0,200,100)
    Line(xCentre - pixel, yCentre + yVertPrecedent)-(xCentre - pixel, yCentre + yCoord), Rgb(0,200,100)
    Line(xCentre - pixel, yCentre - yVertPrecedent)-(xCentre - pixel, yCentre - yCoord), Rgb(0,200,100)
    yVertPrecedent = yCoord
    yCoord = 240 / Sqr(xCoord * pi / 2)                   'y = 1 / sqr(x * pi / 2)
    Line(xCentre + pixel, yCentre + yRougePrecedent)-(xCentre + pixel, yCentre + yCoord), rouge
    Line(xCentre + pixel, yCentre - yRougePrecedent)-(xCentre + pixel, yCentre - yCoord), rouge
    Line(xCentre - pixel, yCentre + yRougePrecedent)-(xCentre - pixel, yCentre + yCoord), rouge
    Line(xCentre - pixel, yCentre - yRougePrecedent)-(xCentre - pixel, yCentre - yCoord), rouge
    yRougePrecedent = yCoord
  Else
    yCoord = 240 * Sin(xCoord+pi/4) / Sqr(xCoord * pi/2)  'y = sin(x + pi/4) / sqr(x * pi/2)
    yVertPrecedent = yCoord
    yRougePrecedent = yCoord
    yCoord = courbe2DStat(pixel)
    Pset(xCentre + pixel, yCentre + yCoord), noir
    Pset(xCentre + pixel, yCentre - yCoord), noir
    Pset(xCentre - pixel, yCentre + yCoord), noir
    Pset(xCentre - pixel, yCentre - yCoord), noir
  End If
Next
For x = -.125 * lambda To 400 Step lambdaSurDeux
  Line (400 + x, yCentre - 10)-(400 + x, yCentre + 10), gris
  Line (400 - x, yCentre - 10)-(400 - x, yCentre + 10), gris
Next
Return

Stationnaires3D:'---- ONDES STATIONNAIRES SELON HUYGENS, TROIS DIMENSIONS --------------------

If OKstat3D Then Goto stat3D                              'calcul selon Huygens complété.
For pixel = 0 To 799 - xCentre                            'distribution de l'amplitude.
  amplitudeSinus = 0
  amplitudeCosinus = 0
  For angle = 0 To pi Step pasStat                        'balayer la sphère sur 180°.
    differenceDeMarche = pixel * Cos(angle) / lambda
    phase = 2 * pi * differenceDeMarche
    amplitudeSinus = amplitudeSinus + Sin(angle) * Sin(phase)   ' sin(angle) simule le rayon.
    amplitudeCosinus = amplitudeCosinus + Sin(angle) * Cos(phase)
  Next
  If pixel = 0 Then normaliser = 2 * pixels / amplitudeCosinus  ' amplitude maximum en pixels.
  amplitude3DStat(pixel) = normaliser * amplitudeCosinus        ' quadrature (sinus -> phase).
Next pixel
OKstat3D = 1                                              'calcul selon Huygens terminé.
stat3D:                                                   'raccourci si la courbe est connue.
periode = 2 * pi * image / images
For pixel = 0 To 799 - xCentre                            'afficher les courbes.
  yCoord = amplitude3DStat(pixel) * Cos(periode)          'rotation selon l'image en cours.
  Line (xCentre + pixel, yCentre)-(xCentre + pixel, yCentre - yCoord), blanc 'zone blanche.
  Line (xCentre - pixel, yCentre)-(xCentre - pixel, yCentre - yCoord), blanc
  Pset(xCentre + pixel, yCentre), gris                    'ligne grise centrale.
  Pset(xCentre - pixel, yCentre), gris
  Pset (xCentre + pixel, yCentre - yCoord), noir          'courbe de l'amplitude.
  Pset (xCentre - pixel, yCentre - yCoord), noir
  xCoord = 2 * pi * pixel / lambda                        'x = 2 * pi * distance / lambda
  If pixel Then
    yCoord = 240 * Sin(xCoord) / xCoord                   'formule: y = sin(x) / x
    Line(xCentre + pixel, yCentre + yVertPrecedent)-(xCentre + pixel, yCentre + yCoord), Rgb(0,200,100)
    Line(xCentre + pixel, yCentre - yVertPrecedent)-(xCentre + pixel, yCentre - yCoord), Rgb(0,200,100)
    Line(xCentre - pixel, yCentre + yVertPrecedent)-(xCentre - pixel, yCentre + yCoord), Rgb(0,200,100)
    Line(xCentre - pixel, yCentre - yVertPrecedent)-(xCentre - pixel, yCentre - yCoord), Rgb(0,200,100)
    yVertPrecedent = yCoord
  Else yVertPrecedent = 240
  End If
  If pixel / (lambda / 2) > .5 Then
    yCoord = 240 / xCoord                                 'formule: y = 1 / x
    Line(xCentre + pixel, yCentre + yRougePrecedent)-(xCentre + pixel, yCentre + yCoord), rouge
    Line(xCentre + pixel, yCentre - yRougePrecedent)-(xCentre + pixel, yCentre - yCoord), rouge
    Line(xCentre - pixel, yCentre + yRougePrecedent)-(xCentre - pixel, yCentre + yCoord), rouge
    Line(xCentre - pixel, yCentre - yRougePrecedent)-(xCentre - pixel, yCentre - yCoord), rouge
    yRougePrecedent = yCoord
  Else
    yRougePrecedent = yVertPrecedent
  End If
Next
For x = 0 To 400 Step lambdaSurDeux                       'repères des longueurs d'onde.
  Line (400 + x, yCentre - 10)-(400 + x, yCentre + 10), gris
  Line (400 - x, yCentre - 10)-(400 - x, yCentre + 10), gris
Next
Return

Initialisation:'----------------------- INITIALISATION ---------------------------------------
pi = 4 * Atn(1)
piSurDeux = 2 * Atn(1)
vert = Rgb(0,150,0)
rouge = Rgb(255,0,0)
bleu = Rgb(0,0,255)
gris = Rgb(150,150,150)
blanc = Rgb(255,255,255)
fond = Rgb(225,225,225)
turquoise = Rgb (230, 255, 255)
OK = 0
image = 0
xCentre = 400
yCentre = 250
lambda = 160                                              'longueur d'onde, nombre pair.
lambdaSurDeux = lambda / 2
pasProg = pi / 2 / 1000                                   'Huygens: 1000 ondelettes sur 90°.
pasStat = pi / 1000                                       'Huygens: 1000 ondelettes sur 180°.
pixels = 120                                              'amplitude des courbes sur l'écran.
dimensions = 3
progressives = 1
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
Screenset 2,2: Color noir, fond: Cls
Line(20,6)-(226,39), rouge, b
Line(21,7)-(225,38), rouge, b
Line(22,8)-(224,37), rouge, b
Line(23,9)-(223,36), blanc, bf
Color noir, blanc
Locate 2, 5: Print "LES ONDES CONCENTRIQUES"
Color noir, fond
ligne33a$= "  A - Deux dimensions.    "
ligne33b$= "  A - Trois dimensions.   "
ligne34a$= "  B - Ondes stationnaires."
ligne34b$= "  B - Ondes progressives. "
ligne35$ = "  I - Initialiser.        ": Locate 35, 39: Print ligne35$
ligne36$ = "      Quitter (Echap).    ": Locate 36, 39: Print ligne36$
ligne37$ = "                          "
Locate 33: Color vert, fond
Locate , 2: Print "M. Jocelyn Marcotte a confirm‚ ces"
Locate , 2: Print "r‚sultats grƒce … l'Ether Virtuel."
Locate , 2: Print "Il a aussi particip‚ … la r‚daction"
Locate , 2: Print "de ce programme en acc‚l‚rant le"
Locate , 2: Print "calcul des ondes progressives.";
Locate 34
Locate , 66: Print "Gabriel LaFreniŠre glafreniere.com"
Locate , 66: Print "Ce programme peut ˆtre distribu‚,"
Locate , 66: Print "copi‚ ou modifi‚ librement."
Locate , 66: Print "Le 16 ao–t 2006.";
Color noir
Gosub flecheGauche: Gosub flecheDroite
eg = 12: ed = 242: eh = 68: eb = 120
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré 1, en haut à gauche.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc
eg = 12: ed = 292: eh = 342: eb = 492
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré 2, en bas à gauche.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc
eg = 516: ed = 786: eh = 22: eb = 150
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré 3, en haut à droite.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc
eg = 508: ed = 790: eh = 358: eb = 502
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré 4, en bas à droite.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc
Locate 23
Locate, 4: Print "Les ondes stationnaires sph‚ri-"
Locate, 4: Print "ques ‚voluent dans un milieu ‚-"
Locate, 4: Print "lastique … trois dimensions. Le"
Locate, 4: Print "noyau central pr‚sente un diamŠ-"
Locate, 4: Print "tre d'une onde entiŠre. Mais le"
Locate, 4: Print "disque central mesure seulement"
Locate, 4: Print "trois-quarts d'onde dans le cas"
Locate, 4: Print "d'un milieu … deux dimensions."
Locate 3
Locate, 67: Print "Ces diagrammes sont ‚tablis …"
Locate, 67: Print "l'aide d'un algorithme bas‚ sur"
Locate, 67: Print "le principe de Huygens. Le cal-"
Locate, 67: Print "cul se r‚sume … faire la somme "
Locate, 67: Print "de mille ondelettes de Huygens "
Locate, 67: Print "r‚parties uniform‚ment sur une "
Locate, 67: Print "circonf‚rence ou une sphŠre.   "
Pcopy 2, page1
Gosub MiseAJour
Return

MiseAJour:'----------------------------- MISE À JOUR -----------------------------------------
Screenset 2

If dimensions = 3 Then
  Locate 6, 4: Print "Milieu … trois dimensions."
  Locate 33, 39: Print ligne33a$
  Locate 24, 66: Print "                                 "
  Locate 25, 66: Print "x = 2 * pi * distance / lambda   "
  Locate 27, 66: Print "Courbe rouge: y = 1 / x          "
  Locate 28, 66: Print "              x > pi / 2         "
  Locate 30, 66: Print "Courbe verte: y = sin(x) / x     "
  Locate 31, 66: Print "              x > 0              "
  Locate 9, 3: Print "M. Jocelyn Marcotte a d‚couvert le"
  Locate 10,3: Print "27 juillet 2006 que la quadrature"
  Locate 11,3: Print "correspond … la formule suivante:"
  Locate 13,3: Print "y = (1 - cos(x)) / x"
Else
  Locate 6, 4: Print "Milieu … deux dimensions. "
  Locate 33, 39: Print ligne33b$
  Locate 24, 66: Print "x = 2 * pi * distance / lambda   "
  Locate 25, 66: Print "x > pi / 3                       "
  Locate 27, 66: Print "Courbe rouge:                    "
  Locate 28, 66: Print "y = 1 / sqr(x * pi / 2)          "
  Locate 30, 66: Print "Courbe verte (approximation):    "
  Locate 31, 66: Print "y = sin(x + pi/4) / sqr(x * pi/2)"
  Locate 9, 3: Print "                                  " 'effacer.
  Locate 10,3: Print "                                 "
  Locate 11,3: Print "                                 "
  Locate 13,3: Print "                    "
End If


If progressives Then
  Locate 7, 4: Print "Ondes progressives. "
  Locate 34,39:Print ligne34a$
Else
  Locate 7, 4: Print "Ondes stationnaires."
  Locate 34,39:Print ligne34b$
End If
Screenset page1, page2
Return
