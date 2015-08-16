nombre = 200
Dim As Single x1(0 To nombre), x2(0 To nombre)                        'position, axe x.
Dim As Single vitesse1(0 To nombre), vitesse2(0 To nombre)            'vitesse (ou mémoire).
Dim As Single influence1(0 To nombre), influence2(0 To nombre)        'influence (ou force).
Dim As Single constante, phase, xCoord, yCoord, gauche, droite
Dim As Single espace1, espace2
Screen 19,24,3: page1 = 1: Gosub Initialisation

'            MODÉLISATION DE L'ÉTHER AVEC INFLUENCE EN RAISON DE LA DISTANCE.
'            Créé le 29 nov. 2005. Mise à jour le 11 janvier 2006.
Do
  Swap page1, page2
  Screenset page1, page2                                  'travailler sur la page cachée.
  Pcopy 2, page1                                          'copier la troisième page (texte).
  screensync
'--------------------- CALCUL SELON LA DISTANCE, DEUX HYPOTHESES -----------------------------
  x1(0) = x1(1) - espace1                                 'réflexion molle.
  x1(nombre1) = x1(nombre1 - 1) + espace1
  For granule = 1 To nombre1 - 1                          'calculer la première onde.
    gauche = x1(granule) - x1(granule - 1)
    droite = x1(granule + 1) - x1(granule)
    influence1(granule) = constante * (droite - gauche)
  Next
  x2(0) = x2(1) - espace2                                 'réflexion molle.
  x2(nombre2) = x2(nombre2 - 1) + espace2
  For granule = 1 To nombre2 - 1                          'calculer la deuxième onde.
    gauche = x2(granule) - x2(granule - 1)
    droite = x2(granule + 1) - x2(granule)
    influence2(granule) = constante * (droite - gauche)
  Next

For granule = 1 To nombre1 - 1                            'afficher la première onde.
    Line (x1(granule), yCentre1 - 4)-(x1(granule) + 2, yCentre1 + 4), grisClair, BF
    Circle (granule * espace1, yCentre1 - 40 + 10 * vitesse1(granule)), rayon, grisClair
    vitesse1(granule) = vitesse1(granule) + influence1(granule)
    x1(granule) = x1(granule) + vitesse1(granule)
    Line (x1(granule), yCentre1 - 4)-(x1(granule) + 2, yCentre1 + 4), 0, BF
    Circle (granule * espace1, yCentre1 - 40 + 10 * vitesse1(granule)), rayon, 0
  Next

  For granule = 1 To nombre2 - 1                          'afficher la deuxième onde.
    Line (x2(granule), yCentre2% - 4)-(x2(granule) + 2, yCentre2 + 4), grisClair, BF
    Circle (granule * espace2, yCentre2 - 40 + 10 * vitesse2(granule)), rayon, grisClair
    vitesse2(granule) = vitesse2(granule) + influence2(granule)
    x2(granule) = x2(granule) + vitesse2(granule)
    Line (x2(granule), yCentre2 - 4)-(x2(granule) + 2, yCentre2 + 4), 0, BF
    Circle (granule * espace2, yCentre2 - 40 + 10 * vitesse2(granule)), rayon, 0
    Next  

  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
    Case Chr$(27), "k+": End
    Case "I": Gosub Initialisation: saisie$ = Inkey$
    Case "M":  Run "Ether00.exe"                          'menu.
    Case "K+": Run "Ether06.exe"                          'flèche gauche.
    Case "M+": Run "Ether08.exe"                          'flèche droite.
  End Select

  Getmouse xSouris, ySouris, , clic                       'saisie souris.
  lignePrecedente = ligne
  ligne = .5 + ySouris / 16
'  locate 33, 43: print xSouris; ySouris; clic; "    "; ligne; "    "
  If ligne < 35 Or xSouris < 328 Or xSouris > 488 Then ligne = 0
  
  Color noir, turquoise: Locate ligne, 42
  Select Case ligne                                       'rehausser l'affichage au besoin.
    Case 35: Print ligne35$
    Case 36: Print ligne36$
    Case 37: Print ligne37$;: If xSouris < xCentre Then Gosub flecheGauche Else Gosub flecheDroite
  End Select
  Color noir, grisClair

  If clic = 1 Then                                        'aiguillage selon le clic.
    Select Case ligne
      Case 35: Sleep 100: Gosub Initialisation
      Case 36: End
      Case 37: If xSouris < 400 Then Run "Ether06.exe" Else Run "Ether08.exe"
    End Select
  End If

Loop
'----------------------------------- DESSIN DES FLÈCHES --------------------------------------
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

Initialisation:'----------------------- INITIALISATION ---------------------------------------
pi = 4 * Atn(1)
constante = .1
lambda = 18
rayon = 3
espace1 = 7: espace2 = 14
nombre1 = 800 / espace1: nombre2 = 800 / espace2
yCentre1 = 70
yCentre2 = 170
diametre = 2 * rayon
xCentre = 400
gabarit$ = "Programme Ether07. Propagation d'une onde longitudinale le long d'une corde.  "
lignes = 17                                               'nombre de lignes du texte.
th = 16                                                   'haut du texte.
tg = 50 - Len(gabarit$) / 2 + 1                           'gauche du texte.
eh = th * 16 - 28                                         'haut de l'encadré.
eb = eh + 16 * lignes + 24                                'bas de l'encadré.
eg = tg * 8 - 32                                          'gauche de l'encadré.
ed = 800 - eg                                             'droite de l'encadré.
xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yFleche = 584

For granule = 0 To nombre1                                     'repartition, onde 1.
  vitesse1(granule) = 0
  influence1(granule) = 0
  x1(granule) = granule * espace1
  phase = 2 * pi * granule / lambda
  xCoord = granule
  yCoord = pi ^ (.005 * -xCoord ^ 2)                      'distribution normale approx.
  x1(granule) = x1(granule) + 20 * yCoord * Cos(phase)              'impulsion initiale.
Next

For granule = 0 To nombre2                                     'repartition, onde 2.
  vitesse2(granule) = 0
  influence2(granule) = 0
  x2(granule) = granule * espace2
  phase = 2 * pi * granule / lambda
  xCoord = granule
  yCoord = pi ^ (.005 * -xCoord ^ 2)
  x2(granule) = x2(granule) + 20 * yCoord * Cos(phase)
Next

blanc = Rgb(255,255,255)                                  'couleurs usuelles.
gris = Rgb(150,150,150)
grisClair = Rgb(225,225,225)
vert = Rgb(0,150,0)
turquoise = Rgb (230,255,255)

Color noir, grisClair
Screenset 2, 0: Cls
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc

Locate 14, 41: Print "L'EFFET DE LENTILLE"
Locate th
Locate , 12: Print "Programme Ether07. Propagation d'une onde longitudinale le long d'une corde.  "
Locate , 12: Print "Dans ce programme, la notion de POTENTIEL est remplac‚e par une position."
Locate , 12: Print "La MEMOIRE devient la vitesse de chaque granule. L'INFLUENCE des deux granules"
Locate , 12: Print "voisins sur chacun d'eux est une force de r‚pulsion qui varie en fonction de"
Locate , 12: Print "leur distance relative, de sorte que tous tendent … se situer … ‚gale distance."
Print
Locate , 12: Print "L'onde inf‚rieure se propage le long d'une corde dont les granules sont deux"
Locate , 12: Print "fois plus espac‚s. Sa vitesse de propagation est ici deux fois plus rapide,"
Locate , 12: Print "mais il existe d'autres facons d'‚valuer l'influence, par exemple en raison"
Locate , 12: Print "inverse de la distance, ou en raison inverse du carr‚ de la distance."
Print
Locate , 12: Print "Il est clair que la vitesse des ondes ne peut pas ˆtre la mˆme si les granules"
Locate , 12: Print "sont concentr‚s ou rar‚fi‚s, ce qui se produit … l'int‚rieur des noeuds des"
Locate , 12: Print "ondes stationnaires. J'en avais conclu que les ondes planes qui circulent dans"
Locate , 12: Print "l'‚ther devaient ˆtre dispers‚es par les ondes stationnaires de l'‚lectron, ce"
Locate , 12: Print "qui devrait se traduire par un effet d'amplification. L'‚lectron utilise cette"
Locate , 12: Print "‚nergie pour subsister et rayonner des ondes sph‚riques en permanence."
ligne35$ = "  I - Initialiser.  ": Locate 35, 42: Print ligne35$
ligne36$ = "  Quitter (Echap).  ": Locate 36, 42: Print ligne36$
ligne37$ = "                    "
Locate 35: Color vert
Locate , 3: Print "Un grand merci … M. Anselme Dewavrin"
Locate , 3: Print "ainsi qu'aux cr‚ateurs de FreeBASIC."
Locate , 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate , 70: Print "Le 11 jan. 2006. Ce programme "
Locate , 70: Print "FreeBASIC peut ˆtre distribu‚,"
Locate , 70: Print "copi‚ ou modifi‚ librement.   ";
Color noir
Gosub flecheGauche
Gosub flecheDroite
Screenset page1, page2
Pcopy 2, page1
Return
