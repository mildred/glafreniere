nombre = 99
DIM as integer xCoord(nombre + 1)
DIM as single energie(nombre + 2), influence(nombre + 1)
dim as single inertie(nombre + 1), energiePrec(nombre + 1)
dim as single vitesse, pi, lambda, phase, amplitude, yCoord, pas
screen 19,24,3: page1 = 1: GOSUB Initialisation

'    MODÉLISATION DE L'ÉTHER, ÉTAPE 2. 
'    par Gabriel LaFrenière. Programme révisé le 15 nov. 2005 et le 9 oct. 2006.
'    Apparemment, chaque point de l'éther possède trois attributs:
'1 - Une ÉNERGIE, qui d'un point de vue mécanique peut être reliée à une vitesse (E = mv^2/2).
'2 - Une INERTIE qui s'oppose à la variation de cette vitesse. Cette propriété peut
'    aussi être considérée comme une mémoire.
'3 - Une INFLUENCE réciproque entre granules voisins, qui tend à uniformiser leur énergie.

DO
  Screensync
  swap page1, page2
  screenset page1, page2                                  'travailler sur la page cachée.
  pcopy 2, page1                                          'copier la troisième page (texte).

  energie(0) = energie(1)                                           'réflexion "molle".
  influence(1) = pas * ((energie(0) + energie(2)) - 2 * energie(1)) 'un tour d'avance.

'----------------------------- CALCUL SELON LA LOI DE HOOKE ---------------------------------
  FOR x = 1 TO nombre
    influence(x + 1) = pas * ((energie(x) + energie(x + 2)) - 2 * energie(x + 1))
    energie(x) = energie(x) + inertie(x) + influence(x)
    inertie(x) = influence(x) + inertie(x)
  NEXT
'------------------------------------ FIN DU CALCUL -----------------------------------------

  FOR granule = 1 TO nombre                               'afficher quatre types d'ondes.
    rayon2 = energiePrec(granule) / 4 + 3
    IF rayon2 > 19 THEN rayon2 = 20
    IF rayon2 < 0 THEN rayon2 = 0
    CIRCLE (xCoord(granule), yCentre - energiePrec(granule)), rayon, grisClair
    CIRCLE (xCoord(granule), yCentre - energie(granule)), rayon, noir

    CIRCLE (xCoord(granule), yCentre - 50), rayon2, grisClair  'effacer.
    rayon2 = energie(granule) / 4 + 3
    IF rayon2 > 19 THEN rayon2 = 20
    IF rayon2 < 0 THEN rayon2 = 0
    CIRCLE (xCoord(granule), yCentre - 50), rayon2, noir

    erg = energiePrec(granule) / 2
    LINE (xCoord(granule) + erg, yCentre + 50)-(xCoord(granule) + erg + 2, yCentre + 55), grisClair, BF
    erg = energie(granule) / 2
    LINE (xCoord(granule) + erg, yCentre + 50)-(xCoord(granule) + erg + 2, yCentre + 55), noir, BF

    CIRCLE (xCoord(granule), yCentre + 100), rayon + 1, 1 'couleur 1: identifier le contour.
    luminance = 10 * (energie(granule) + 20)
    IF luminance > 255 THEN luminance = 255
    IF luminance < 0 THEN luminance = 0
    PAINT (xCoord(granule), yCentre + 100), rgb(luminance,luminance,luminance), 1
    energiePrec(granule) = energie(granule)
  NEXT

  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  SELECT CASE saisie$
    CASE CHR$(27), "k+", "X+": END
    CASE "I": GOSUB Initialisation: a$ = inkey$
    CASE "K+": run "Ether02.exe"                          'flèche gauche.
    CASE "M+": run "Ether04.exe"                          'flèche droite.
    CASE "M": run "Ether00.exe"                           'menu principal.
  END SELECT

  GETMOUSE xSouris, ySouris, , clic                       'saisie de la souris.
  lignePrecedente = ligne
  ligne = .5 + ySouris / 16
  if ligne < 35 or xSouris < 328 or xSouris > 488 then ligne = 0
  
  color noir, turquoise: locate ligne, 42
  select case ligne                                       'rehausser l'affichage au besoin.
    case 35: print ligne35$
    case 36: print ligne36$
    case 37: PRINT ligne37$;: if xSouris < xCentre then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, grisClair

  if clic = 1 then                                        'aiguillage selon le clic.
    select case ligne
      case 35: sleep 100: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether02.exe" else run "Ether04.exe"
    end select
  end if

LOOP

flecheGauche:
line (xgg+6,yFleche-4)-(xgg+8,yFleche-4),noir
line (xgg+4,yFleche-3)-(xgg+8,yFleche-3),noir
line (xgg+2,yFleche-2)-(xgg+8,yFleche-2),noir
line (xgg,yFleche-1)-(xgd,yFleche-1),noir
line (xgg-2,yFleche)-(xgd,yFleche),noir
line (xgg,yFleche+1)-(xgd,yFleche+1),noir
line (xgg+2,yFleche+2)-(xgg+8,yFleche+2),noir
line (xgg+4,yFleche+3)-(xgg+8,yFleche+3),noir
line (xgg+6,yFleche+4)-(xgg+8,yFleche+4),noir
return

flecheDroite:
line (xdd-8,yFleche-4)-(xdd-6,yFleche-4),noir
line (xdd-8,yFleche-3)-(xdd-4,yFleche-3),noir
line (xdd-8,yFleche-2)-(xdd-2,yFleche-2),noir
line (xdg,yFleche-1)-(xdd,yFleche-1),noir
line (xdg,yFleche)-(xdd+2,yFleche),noir
line (xdg,yFleche+1)-(xdd,yFleche+1),noir
line (xdd-8,yFleche+2)-(xdd-2,yFleche+2),noir
line (xdd-8,yFleche+3)-(xdd-4,yFleche+3),noir
line (xdd-8,yFleche+4)-(xdd-6,yFleche+4),noir
return

Initialisation:'----------------------- INITIALISATION ---------------------------------------
pi = 4 * ATN(1)
rayon = 3
diametre = 2 * rayon
xCentre = 407
yCentre = 100
lambda = 20
pas = .2
gabarit$ = "Programme Ether03. Le programme pr‚c‚dent ne montrait que des billes qui se"
lignes = 15                                               'nombre de lignes du texte.
tb = 33                                                   'bas du texte.
th = tb - lignes                                          'haut du texte.
tg = 50 - len(gabarit$) / 2 + 1                           'gauche du texte.
eh = th * 16 - 32                                         'haut de l'encadré.
eb = eh + 16 * lignes + 32                                'bas de l'encadré.
eg = tg * 8 - 32                                          'gauche de l'encadré.
ed = 800 - eg                                             'droite de l'encadré.
xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yFleche = 584

FOR j = 1 TO nombre + 1
  xCoord(j) = j * (diametre + 2)                          'distribution des points.
  phase = 2 * pi * j / lambda
  yCoord = pi ^ (.0001 * -xCoord(j) ^ 2)                  'impulsion initiale selon
  amplitude = 50 * yCoord                                 'une distribution normale approx:
  energie(j) = amplitude * COS(phase)                     '      .0001 modifie la longueur.
  inertie(j) = 0                                          '      50 modifie l'amplitude.
NEXT

blanc = rgb(255,255,255)                                  'couleurs usuelles.
gris = rgb(150,150,150)
grisClair = rgb(225,225,225)
vert = rgb(0,150,0)
turquoise = rgb (230,255,255)

color noir, grisClair
screenset 2, 0: cls
CIRCLE (xCoord(0), yCentre), rayon, noir
CIRCLE (xCoord(nombre + 1), yCentre), rayon, noir
line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré.
line (eg + 1,eh + 1)-(ed, eb), noir, B
line (eg + 1,eb)-(ed, eb), blanc
line (eg,eb + 1)-(ed + 1, eb + 1), blanc
line (ed,eh + 1)-(ed, eb), blanc
line (ed + 1,eh)-(ed + 1, eb + 1), blanc

LOCATE th
LOCATE , tg: PRINT "Programme Ether03. Le programme pr‚c‚dent ne montrait que des billes qui se "
LOCATE , tg: PRINT "d‚placent de haut en bas, comme on peut le voir encore ci-dessus."
PRINT
LOCATE , tg: PRINT "Cela suggŠre … tort qu'il s'agit de vibrations transversales. En fait ces"
LOCATE , tg: PRINT "vibrations correspondent plus justement … des variations ® d'‚nergie ¯ et"
LOCATE , tg: PRINT "les ‚changes se font longitudinalement. Il s'agit donc plut“t de vibrations"
LOCATE , tg: PRINT "longitudinales, et c'est le cas mˆme dans un milieu en trois dimensions."
PRINT
LOCATE , tg: PRINT "On peut aussi consid‚rer que les granules sont immobiles et que les ‚changes"
LOCATE , tg: PRINT "d'‚nergie se font … distance. Mais selon l'hypothŠse la plus raisonnable,"
LOCATE , tg: PRINT "il semble bien qu'on devra consid‚rer que les granules se d‚placent les uns"
LOCATE , tg: PRINT "vers les autres et que leur influence d‚pend de la distance qui les s‚pare."
PRINT
LOCATE , tg: PRINT "Etrangement, les quatre repr‚sentations montr‚es ci-dessus sont bas‚es sur"
LOCATE , tg: PRINT "le mˆme algorithme; mais il existe d'autres m‚thodes de calcul."
ligne35$ = "  I - Initialiser.  ": LOCATE 35, 42: PRINT ligne35$
ligne36$ = "  Quitter (Echap).  ": LOCATE 36, 42: PRINT ligne36$
ligne37$ = "                    "
Locate 35: Color vert
Locate , 3: Print "F‚licitations … M. Philippe Delmotte"
Locate , 3: Print "ainsi qu'aux cr‚ateurs de FreeBASIC."
Locate , 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate , 70: Print "Le 8 oct. 2006. Ce programme "
Locate , 70: Print "FreeBASIC peut ˆtre distribu‚,"
Locate , 70: Print "copi‚ ou modifi‚ librement.   ";
color noir
gosub flecheGauche
gosub flecheDroite
screenset page1, page2
pcopy 2, page1
RETURN
