largeur = 800: hauteur = 460                              'nombre de particules par côté.
dim as single M(-1 to largeur+1, -1 to hauteur+1)
dim as single I(-1 to largeur+1, -1 to hauteur+1)
dim as single P(-1 to largeur+1, -1 to hauteur+1)
dim as single facteur, rapport, longueur, luminosite, contraste, phi
dim as single amplitudeSinus, amplitudeCosinus, c, ton, xOrig, yOrig, depart
dim as single xPrime, tPrime, temps, beta, theta, pas, ixe, igrec, xSource
dim as single xCoord, yCoord, periode, rotation, amplitude, rayon, gLorentz
dim as single pi, angle, lambda, xCarre, yCarre, distance, xDistance, yDistance
screen 19,24,3: page2 = 1: beta = .5: gosub Initialisation

'      IMPORTANT: ALLER À LA LIGNE 45 POUR LA PULSATION D'UNE ONDE (11 novembre 2006).

do'    MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.
  for y = 0 to hauteur - 1
    I(0,y) = (P(-1,y) + P(1,y) + P(0,y-1) + P(0,y+1)) / 4 - P(0,y)' un cran à l'avance.
  next

  for x = 0 to largeur - 1
    for y = 0 to hauteur - 1
'------------------------------ CALCUL SELON LA LOI DE HOOKE ------------------------
      I(x+1,y) = (P(x,y) + P(x+2,y) + P(x+1,y-1) + P(x+1,y+1)) / 4 - P(x+1,y)' I => Influence
      P(x, y) = P(x, y) + I(x, y) + M(x, y)                                  ' P => Potentiel
      M(x, y) = M(x, y) + I(x, y)                                            ' M => Mémoire
'------------------------------------- FIN DU CALCUL --------------------------------
    next
  next

  for x = 0 to largeur - 1                                'pas de réflexion, haut et bas.
    P(x, -1) = P(x, 0) - 2 * M(x, 0): P(x, hauteur) = P(x, hauteur - 1) - 2 * M(x, hauteur - 1)
  next
  for y = 0 to hauteur - 1                                'pas de réflexion, gauche et droite.
    P(-1, y) = P(0, y) - 2 * M(0, y): P(largeur, y) = P(largeur - 1, y) - 2 * M(largeur - 1, y)
  next
  
'****************** PULSATION D'UNE ONDE SELON LES TRANSFORMATIONS DE LORENTZ *****************

  temps = temps + c / lambda                              'c est la vitesse des ondes.
  rayon = 2.1875                                          'il faut émettre au centre de l'un
  pas = 2 * pi / 31                                       'des ventres des ondes stationnaires, 
  for angle = pas / 2 to 2 * pi step pas                  'soit 0,1875 plus n * 0,5.
    xCoord = depart * lambda + rayon * lambda * cos(angle)
    yCoord = rayon * lambda * sin(angle)
    xOrig = xCoord / lambda                               'xOrig et xPrime en longueurs d'onde.
    xPrime = xOrig * cos(theta) + temps * sin(theta)      '--- TRANSFORMATIONS DE LORENTZ. ---
    tPrime = temps * cos(theta) - xOrig * sin(theta)      'temps et tPrime en périodes d'onde.
    if xPrime * lambda > 799 then gosub Initialisation: exit for
    gosub Pulsation    
    pset(xPrime * lambda, yOrig * lambda + yCoord), gris
  next
'**********************************************************************************************
  xOrig = depart                                          'position de départ, constante.
  xPrime = xOrig * cos(theta) + temps * sin(theta)        'x' selon Lorentz.
  circle(xPrime * lambda, yCentre), rayon * lambda, noir,,, 1 / gLorentz 'ellipse mobile.

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
      case "0": beta = 0 : gosub Initialisation
      case "1": beta = .1: gosub Initialisation
      case "2": beta = .2: gosub Initialisation
      case "3": beta = .3: gosub Initialisation
      case "4": beta = .4: gosub Initialisation
      case "5": beta = .5: gosub Initialisation
      case "6": beta = .6: gosub Initialisation
      case "7": beta = .7: gosub Initialisation
      case "8": beta = .8: gosub Initialisation
      case "9": beta = .9: gosub Initialisation
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
    if P(x, y) < 0 then
      luminance = luminosite * -P(x, y)
      vert = luminance
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    else
      luminance = luminosite * P(x, y)
      rouge = luminance
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
'-------------- PULSATION SELON UN CARRÉ DE 9 GRANULES POUR RÉDUIRE LES ARTÉFACTS -------------
Pulsation:
' Le programme Ether04 montre que la mémoire (M ci-dessous) correspond à la quadrature.
' Il vaut donc mieux utiliser le cosinus de la période si l'on modifie la mémoire.
' Autrement il en résulte une anomalie si bêta = 0, car dans ce cas la période n'évolue pas.
M(xPrime * lambda-1, yOrig * lambda + yCoord-1) = M(xPrime * lambda-1, yOrig * lambda + yCoord-1) +.25 * cos(2 * pi * tPrime)
M(xPrime * lambda-1, yOrig * lambda + yCoord-0) = M(xPrime * lambda-1, yOrig * lambda + yCoord-0) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda-1, yOrig * lambda + yCoord+1) = M(xPrime * lambda-1, yOrig * lambda + yCoord+1) +.25 * cos(2 * pi * tPrime)
M(xPrime * lambda+0, yOrig * lambda + yCoord-1) = M(xPrime * lambda+0, yOrig * lambda + yCoord-1) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda+0, yOrig * lambda + yCoord+0) = M(xPrime * lambda+0, yOrig * lambda + yCoord+0) + 1  * cos(2 * pi * tPrime)
M(xPrime * lambda+0, yOrig * lambda + yCoord+1) = M(xPrime * lambda+0, yOrig * lambda + yCoord+1) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda+1, yOrig * lambda + yCoord-1) = M(xPrime * lambda+1, yOrig * lambda + yCoord-1) +.25 * cos(2 * pi * tPrime)
M(xPrime * lambda+1, yOrig * lambda + yCoord-0) = M(xPrime * lambda+1, yOrig * lambda + yCoord-0) +.5  * cos(2 * pi * tPrime)
M(xPrime * lambda+1, yOrig * lambda + yCoord+1) = M(xPrime * lambda+1, yOrig * lambda + yCoord+1) +.25 * cos(2 * pi * tPrime)
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
temps = 0
theta = asin(beta)
gLorentz = sqr(1 - beta ^ 2)                              'facteur de contraction g de Lorentz.
c = .5                                                    'vitesse de l'onde en pixels par passage.
lambda = 50
depart = 3
luminosite = 40                       
contraste = 2
yOrig = hauteur / 2 / lambda
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
locate 30
locate, 2: print "Appuyez sur un chiffre de 0 … 9 pour modifier la vitesse. Actuellement: bˆta = ";
           print using "#.# c"; beta
locate, 2: print "L'ellipse ‚met des ondelettes de Huygens en respectant les transformations de Lorentz. De cette"
locate, 2: print "maniŠre, l'effet Doppler des ondes ‚mises vers l'int‚rieur concorde avec celui des ondes ‚mises"
locate, 2: print "vers l'ext‚rieur. La partie centrale ‚quivaut … mon ‚lectron mobile, mais en deux dimensions."
locate 35: color vert
locate, 2: print "Mes salutations … Leo."
locate, 2: print "Mille mercis … M. Philippe Delmotte."
locate, 2: print "Gabriel LaFreniŠre  glafreniere.com";
locate 35
locate, 69:print "Le 18 nov. 2006. Ce programme"
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
