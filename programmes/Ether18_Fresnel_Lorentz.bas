largeur = 800: hauteur = 480                              'nombre de particules par côté.
dim as single M(-1 to largeur+1, -1 to hauteur+1)
dim as single I(-1 to largeur+1, -1 to hauteur+1)
dim as single P(-1 to largeur+1, -1 to hauteur+1)
dim as single facteur, rapport, longueur, luminosite, contraste, phi
dim as single amplitudeSinus, amplitudeCosinus, c, ton, xOrig, yOrig, depart
dim as single xPrime, tPrime, temps, beta, theta, pas, ixe, igrec, xSource
dim as single xCoord, yCoord, periode, rotation, amplitude, rayon, gLorentz
dim as single pi, angle, lambda, xCarre, yCarre, distance, xDistance, yDistance
screen 19,24,3: page2 = 1: gosub Initialisation

'      PULSATION DES ONDES: LIGNE 40.

do'    MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.
  for y = 0 to hauteur - 1
    I(0,y) = (P(-1,y) + P(1,y) + P(0,y-1) + P(0,y+1)) / 4 - P(0,y)' un cran à l'avance.
  next
  for x = 0 to largeur - 1
    for y = 0 to hauteur - 1
'---------------------------- ALGORITHME DE M. PHILIPPE DELMOTTE ------------------------------
      I(x+1,y) = (P(x,y) + P(x+2,y) + P(x+1,y-1) + P(x+1,y+1)) / 4 - P(x+1,y)' I => Influence
      P(x, y) = P(x, y) + I(x, y) + M(x, y)                                  ' P => Potentiel
      M(x, y) = M(x, y) + I(x, y)                                            ' M => Mémoire
'-------------------------------------- FIN DU CALCUL -----------------------------------------
    next
  next
  for y = 0 to hauteur - 1                                'pas de réflexion, gauche et droite.
    P(-1, y) = P(0, y) - 2 * M(0, y): P(largeur, y) = P(largeur - 1, y) - 2 * M(largeur - 1, y)
  next
  for x = 0 to largeur - 1                                'pas de réflexion, haut et bas.
    P(x, hauteur) = P(x, hauteur - 1) - 2 * M(x, hauteur - 1): P(x, -1) = P(x, 0) - 2 * M(x, 0)
  next
  for x = -1 to largeur + 1                               'amortir le résidu de réflexion (haut).
    for y = 1 to 40
      P(x, y) = (1 - (40 - y) * .001) * P(x, y)
      M(x, y) = (1 - (40 - y) * .001) * M(x, y)
    next
  next
'************ PULSATION D'UNE SOURCE LINÉAIRE SELON LES TRANSFORMATIONS DE LORENTZ ************

  temps = temps + c / lambda                              'c est la vitesse des ondes.
  for xCoord = depart to 10 * lambda
    xOrig = xCoord / lambda                               'xOrig et xPrime en longueurs d'onde.
    xPrime = xOrig * cos(theta) + temps * sin(theta)      '--- TRANSFORMATIONS DE LORENTZ. ---
    tPrime = temps * cos(theta) - xOrig * sin(theta)      'temps et tPrime en périodes d'onde.
    if xPrime * lambda > 450 then sleep: b$ = inkey: gosub Initialisation: exit for
    gosub Pulsation
'*************************************** SOURCE FIXE ******************************************
    xPrime = xOrig + 39                                   ' x' = x   et positionner à droite.
    tPrime = temps                                        ' t' = t   pour source fixe.
    gosub Pulsation
  next

  if clic = 0 then getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  gosub afficherCouleurs'------------ DIAGRAMME PRINCIPAL -------------------------------------

'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = inkey
  if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
  if saisie$ = "k+" then end 
  if len(saisie$) then
    bitmap = 0
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+"': locate 34,2: print saisie$
    select case saisie$
      case "k+",chr$(27): end
      case "I": gosub Initialisation
      case "P": sleep
    end select
  end if
'----------------------------------------- SAISIE SOURIS --------------------------------------

  if clic = 0 then getmouse xSouris, ySouris, , clic    'vérifier une dernière fois.
  ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
  if ligne > 26 and ligne < 38 then
    if xSouris < 304 or xSouris > 496 then ligne = 0
  else ligne = 0  
  end if
  
'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 39
  select case ligne
    case 35: print ligne35$      
    case 36: print ligne36$
    case 37: print ligne37$;: if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, fond
'-------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
  if clic = 1 then
    bitmap = 0
    select case ligne
      case 35: gosub Initialisation
      case 36: end
    end select
  end if
  if bitmap then gosub Bitmaps                           'capture d'images si désiré.
  clic = 0
loop

afficherCouleurs:'------------------ AFFICHER EN COULEURS -------------------------------------
for x = 0 to largeur
  for y = 0 to hauteur
    luminance = luminosite * P(x, y)
    if P(x, y) < 0 then
      vert = luminance * luminance
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    else
      rouge = luminance * luminance
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    end if
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    if bleu > 255 then bleu = 255 else if bleu < 0 then bleu = 0
    pset (x, y), rgb(rouge,vert,bleu)
  next
next
return

Pulsation:
M(xPrime * lambda-1, yOrig-1) = M(xPrime * lambda-1, yOrig-1) +.25 * cos(2 * pi * tPrime)
M(xPrime * lambda-1, yOrig-0) = M(xPrime * lambda-1, yOrig-0) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda-1, yOrig+1) = M(xPrime * lambda-1, yOrig+1) +.25 * cos(2 * pi * tPrime)
M(xPrime * lambda+0, yOrig-1) = M(xPrime * lambda+0, yOrig-1) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda+0, yOrig+0) = M(xPrime * lambda+0, yOrig+0) + 1  * cos(2 * pi * tPrime)
M(xPrime * lambda+0, yOrig+1) = M(xPrime * lambda+0, yOrig+1) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda+1, yOrig-1) = M(xPrime * lambda+1, yOrig-1) +.25 * cos(2 * pi * tPrime)
M(xPrime * lambda+1, yOrig-0) = M(xPrime * lambda+1, yOrig-0) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda+1, yOrig+1) = M(xPrime * lambda+1, yOrig+1) +.25 * cos(2 * pi * tPrime)
return

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
select case capture
  case is < 10: numero$ = "00"
  case is < 100: numero$ = "0"
  case is < 1000: numero$ = ""
end select
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
color rgb(255,255,255), rgb(255,0,0)                      'signaler la capture d'images.
locate 34, 43: print fichier$
bsave fichier$,0
color noir, fond
capture = capture + 1
if capture > 500 then end 
return

'------------------------------------ DESSIN DES FLÈCHES -------------------------------------
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

'#############################################################################################
Initialisation:'------------------------ INITIALISATION --------------------------------------
'#############################################################################################
fond = rgb(225,225,225)
blanc= rgb(255,255,255)
gris = rgb(75,75,75)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
vert = rgb(0,150,0)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               'séquence bitmap si désiré.
beta = .5
theta = asin(beta)
gLorentz = sqr(1 - beta ^ 2)                              'facteur de contraction g de Lorentz.
c = .5                                                    'vitesse de l'onde en pixels par passage.
temps = 0
lambda = 16
depart = 3.5
luminosite = 3                     
contraste = 2
yOrig = 4 * lambda'****************
yOrig = hauteur
xSource = 400
relief = lambda / 5
demiOnde = 1
xCentre = largeur / 2
yCentre = hauteur / 2
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
line(0,0)-(largeur, hauteur),noir, bf
ligne35$ = "    I - Initialiser.     ": locate 35, 39: print ligne35$
ligne36$ = "        Quitter (Echap). ": locate 36, 39: print ligne36$
ligne37$ = "                         "
locate 32
locate, 2: print "La structure g‚n‚rale de la diffraction de Fresnel est invariante si la source lin‚aire se d‚place.";
locate, 2: print "Mais elle se contracte et la p‚riode des ondes ‚volue sur l'axe du d‚placement en plus de ralentir,";
locate, 2: print "selon les transformations de Lorentz."
locate 36: color vert
locate, 2: print "Mille mercis … M. Philippe Delmotte."
locate, 2: print "Gabriel LaFreniŠre  glafreniere.com";
locate 35
locate, 69:print "Le 17 nov. 2006. Ce programme"
locate, 69:print "FreeBASIC peut ˆtre copi‚,"
locate, 69:print "distribu‚ ou modifi‚ librement.";

for x = -1 to largeur + 1                                 'effacer.
  for y = -1 to hauteur + 1
    P(x, y) = .000001                                     'accélère le calcul (!!?).
    M(x, y) = .000001
  next
next
pcopy 2, page1
pcopy 2, page2
screenset page1, page2
return
