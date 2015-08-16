nombre = 99
Dim As Integer xCoord(0 To nombre + 1)
Dim As Single energie(0 To nombre + 2), influence(0 To nombre + 1)
Dim As Single inertie(0 To nombre + 1), energiePrec(0 To nombre + 1)
Dim As Single vitesse, pi, lambda, phase, amplitude, yCoord, pas
Screen 19,24,3: page1 = 1: Gosub Initialisation

'    MODÉLISATION DE L'ÉTHER, ÉTAPE 2. 
'    par Gabriel LaFrenière. Programme révisé le 15 nov. 2005 et le 8 oct. 2006.
'    Apparemment, chaque granule de l'éther possède trois attributs:
'1 - Une ÉNERGIE, qui d'un point de vue mécanique peut être reliée à une vitesse (E = mv^2 / 2).
'2 - Une INERTIE, qui s'oppose à la variation de cette vitesse. Cette propriété peut
'    aussi être considérée comme une mémoire.
'3 - Une INFLUENCE réciproque entre granules voisins, qui tend à uniformiser leur énergie.

Do
  Screensync
  Swap page1, page2
  Screenset page1, page2                                  'travailler sur la page cachée.
  Pcopy 2, page1                                          'copier la troisième page (texte).

  energie(0) = energie(1)                                 'réflexion "molle", facultatif.
  influence(1) = energie(0) + energie(2) - 2 * energie(1) 'un cran d'avance.

'----------------------------- CALCUL SELON LA LOI DE HOOKE ---------------------------------
  For x = 1 To nombre
    influence(x+1) =  energie(x) + energie(x+2) - 2 * energie(x+1) 'un cran d'avance.
    energie(x) = energie(x) + inertie(x) + pas * influence(x)
    inertie(x) =  inertie(x) + pas * influence(x)         'le pas ne sert qu'à ralentir
  Next                                                    'les ondes. Il peut être éliminé.
'------------------------------------ FIN DU CALCUL -----------------------------------------

  For granule = 1 To nombre                               'afficher l'onde.
    rayon2 = energiePrec(granule) / 4 + 3
    If rayon2 > 19 Then rayon2 = 20
    If rayon2 < 0 Then rayon2 = 0
    Circle (xCoord(granule), yCentre - energiePrec(granule)), rayon, fond
    Circle (xCoord(granule), yCentre - energie(granule)), rayon, noir
    energiePrec(granule) = energie(granule)
  Next

'-------------------------------- SAISIE CLAVIER ET SOURIS------------------------------------
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
    Case Chr$(27), "k+", "X+": End
    Case "I": Gosub Initialisation: saisie$ = Inkey$
    Case "K+": Run "Ether01.exe"                          'flèche gauche.
    Case "M+": Run "Ether03.exe"                          'flèche droite.
    Case "M": Run "Ether00.exe"                           'menu principal.
  End Select

  Getmouse xSouris, ySouris, , clic                       'saisie de la souris.
  lignePrecedente = ligne
  ligne = .5 + ySouris / 16
  'locate 33, 43: print xSouris; ySouris; clic; "    "; ligne; "    "
  If ligne < 35 Or xSouris < 328 Or xSouris > 488 Then ligne = 0
  
  Color noir, turquoise: Locate ligne, 42
  Select Case ligne                                       'rehausser l'affichage au besoin.
    Case 35: Print ligne35$
    Case 36: Print ligne36$
    Case 37: Print ligne37$;
             If xSouris < xCentre Then Gosub flecheGauche Else Gosub flecheDroite
  End Select
  Color noir, fond

  If clic = 1 Then                                        'aiguillage selon le clic.
    Select Case ligne
      Case 35: Sleep 100: Gosub Initialisation
      Case 36: End
      Case 37: If xSouris < 400 Then Run "Ether01.exe" Else Run "Ether03.exe"
    End Select
  End If

Loop
'---------------------------------- DESSIN DES FLÈCHES ---------------------------------------
flecheGauche:
Line (xgg+6,yF-4)-(xgg+8,yF-4),noir
Line (xgg+4,yF-3)-(xgg+8,yF-3),noir
Line (xgg+2,yF-2)-(xgg+8,yF-2),noir
Line (xgg,yF-1)-(xgd,yF-1),noir
Line (xgg-2,yF)-(xgd,yF),noir
Line (xgg,yF+1)-(xgd,yF+1),noir
Line (xgg+2,yF+2)-(xgg+8,yF+2),noir
Line (xgg+4,yF+3)-(xgg+8,yF+3),noir
Line (xgg+6,yF+4)-(xgg+8,yF+4),noir
Return

flecheDroite:
Line (xdd-8,yF-4)-(xdd-6,yF-4),noir
Line (xdd-8,yF-3)-(xdd-4,yF-3),noir
Line (xdd-8,yF-2)-(xdd-2,yF-2),noir
Line (xdg,yF-1)-(xdd,yF-1),noir
Line (xdg,yF)-(xdd+2,yF),noir
Line (xdg,yF+1)-(xdd,yF+1),noir
Line (xdd-8,yF+2)-(xdd-2,yF+2),noir
Line (xdd-8,yF+3)-(xdd-4,yF+3),noir
Line (xdd-8,yF+4)-(xdd-6,yF+4),noir
Return

Initialisation:'----------------------- INITIALISATION ---------------------------------------
pi = 4 * Atn(1)
rayon = 3
diametre = 2 * rayon
xCentre = 400
lambda = 20
pas = .3                                                  'le pas ralentit les ondes.
gabarit$ = "Programme Ether03. Le programme pr‚c‚dent ne montrait que des billes qui se"
lignes = 15                                               'nombre de lignes du texte.
tb = 33                                                   'bas du texte.
th = tb - lignes                                          'haut du texte.
tg = 50 - Len(gabarit$) / 2 + 1                           'gauche du texte.
eh = th * 16 - 32                                         'haut de l'encadré.
yCentre = eh / 2                                          'emplacement de l'onde.
eb = eh + 16 * lignes + 32                                'bas de l'encadré.
eg = tg * 8 - 32                                          'gauche de l'encadré.
ed = 800 - eg                                             'droite de l'encadré.
xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yF = 584

For j = 0 To nombre + 1
  xCoord(j) = j * (diametre + 2)                          'distribution des points.
  phase = 2 * pi * j / lambda
  yCoord = pi ^ (.0001 * -xCoord(j) ^ 2)                  'impulsion initiale selon
  amplitude = 50 * yCoord                                 'une distribution normale approx:
  energie(j) = amplitude * Cos(phase)                     '   .0001 modifie la longueur.
  inertie(j) = 0                                          '   50 modifie l'amplitude.
Next

blanc = Rgb(255,255,255)                                  'couleurs usuelles.
gris = Rgb(150,150,150)
fond = Rgb(225,225,225)
vert = Rgb(0,150,0)
turquoise = Rgb (230,255,255)

Color noir, fond
Screenset 2, 0: Cls
Circle (xCoord(0), yCentre), rayon, noir
Circle (xCoord(nombre + 1), yCentre), rayon, noir
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc

Color noir, fond: Locate th
Locate , tg: Print "Programme ETHER02. Le programme pr‚c‚dent montrait des billes isol‚es qui "
Locate , tg: Print "oscillent selon la loi de Hooke. Mais si les billes sont dispos‚es sur une"
Locate , tg: Print "droite et si elles ont une INFLUENCE sur leurs voisines, elles provoquent "
Locate , tg: Print "l'apparition d'une onde. L'ENERGIE des billes (ou des granules) est mo-"
Locate , tg: Print "difi‚e selon celle de leurs voisines. Leur influence varie selon la diff‚- "
Locate , tg: Print "rence d'‚nergie, qui peut ˆtre compar‚e … une tension ‚lectrique."
Print
Locate , tg: Print "De plus, chaque bille ou granule semble avoir une m‚moire de ses variations"
Locate , tg: Print "d'‚nergie ant‚rieures, ce qui peut ˆtre consid‚r‚ comme une INERTIE."
Print
Locate , tg: Print "IMPORTANT: ce programme montre des billes qui oscillent transversalement  "
Locate , tg: Print "parce que c'est la repr‚sentation la plus intuitive d'une onde. Toutefois "
Locate , tg: Print "le calcul correspond plut“t … une onde longitudinale."
Print
Locate , tg: Print "A gauche, la r‚flexion est dite ® molle ¯. A droite, elle est ® dure ¯."
Color noir, fond
ligne35$ = "  I - Initialiser.  ": Locate 35, 42: Print ligne35$
ligne36$ = "  Quitter (Echap).  ": Locate 36, 42: Print ligne36$
ligne37$ = "                    "
Locate 35: Color vert
Locate , 3: Print "Merci … M. Philippe Delmotte"
Locate , 3: Print "ainsi qu'aux cr‚ateurs de FreeBASIC."
Locate , 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate , 70: Print "Le 8 oct. 2006. Ce programme "
Locate , 70: Print "FreeBASIC peut ˆtre distribu‚,"
Locate , 70: Print "copi‚ ou modifi‚ librement.   ";
Color noir
Gosub flecheGauche
Gosub flecheDroite
Return
