cote = 800                                                'nombre de particules par côté.
dim precedent(-1 to cote+1)
dim as single M(-1 to cote+1, -1 to cote+1)
dim as single I(-1 to cote+1, -1 to cote+1) 
dim as single P(-1 to cote+1, -1 to cote+1), P2(-1 to cote+1, -1 to cote+1)
dim as single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance
dim as single betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, pas, ton, phi
dim as single facteur, rapport, longueur, luminosite, contraste, phase
dim as single pi, angle, lambda, xCarre, yCarre, arcSinus, xDistance, affaiblissement
dim as single xCoord, distNormale, periode, distance1, distance2, rotation, amplitude
screen 19,24,3: page1 = 1: gosub Initialisation

do  '                         LE RAYONNEMENT DES CHAMPS DE FORCE.

  for y = 0 to cote - 1                                   'amorçage, un cran à l'avance.
    I(0,y) = (P(-1,y) + P(1,y) + P(0,y-1) + P(0,y+1)) / 4 - P(0,y)
  next

  for x = 0 to cote - 1
    for y = 0 to cote - 1
'------------------------------ CALCUL SELON LA LOI DE HOOKE ------------------------
      I(x+1,y) = (P(x,y) + P(x+2,y) + P(x+1,y-1) + P(x+1,y+1)) / 4 - P(x+1,y)' I => Influence
      P(x, y) = P(x, y) + I(x, y) + M(x, y)                                  ' P => Potentiel
      M(x, y) = M(x, y) + I(x, y)                                            ' M => Mémoire
'------------------------------------- FIN DU CALCUL --------------------------------
    next
  next

  for x = 0 to cote - 1                                   'pas de réflexion, haut et bas.
    P(x, -1) = P(x, 0) - 2 * M(x, 0): P(x, cote) = P(x, cote - 1) - 2 * M(x, cote - 1)
  next
  
  if reflexion then
    for y = 0 to cote - 1'                                'réflexion gauche et droite.
      P(-1, y) = P(0, y): P(cote, y) = P(cote - 1, y)
    next
  else
    for y = 0 to cote - 1                                 'pas de réflexion.
      P(-1, y) = P(0, y) - 2 * M(0, y): P(cote, y) = P(cote - 1, y) - 2 * M(cote - 1, y)
    next
  end if
  
  if clic < 1 then getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  if afficher = 0 or clic > 0 then                        'afficher une fois sur deux.
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    gosub afficherRelief'-------- PRODUIT LE DIAGRAMME PRINCIPAL -----------------------------
    circle(xElectron, 200), 40, blanc
    circle(xElectron, 200), 41, noir
'    Circle(difference, 200), 40, blanc
'    Circle(difference, 200), 41, noir
'--------------------------------------- SAISIE CLAVIER --------------------------------------
    saisie$ = inkey
    if len(saisie$) then
      bitmap = 0
      if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
      select case saisie$
        case "C": mode$ = "convergent":   gosub PlanoConvexe
        case "D": mode$ = "divergent" :   gosub PlanoConvexe
        case "S": mode$ = "stationnaire": gosub PlanoConvexe
        case "I": gosub Initialisation
        case "R": gosub Inverser
        case "E": gosub EffacerMemoire
        case "=+": relief = relief - 1: if relief < 0 then relief = 0   'F3.
        case ">+": relief = relief + 1: if relief > 12 then relief = 12 'F4.
        case "?+":                                                      'F5.
        case "@+":                                                      'F6.
        case "A+": reflexion = 0                                        'F7.
        case "B+": reflexion = 1                                        'F8.
        case "k+", "X+", chr$(27): end
        case "M": run "Ether00.exe"
        case "K+":run "Ether12.exe"                       'flèche gauche.
        case "M+":run "Ether14.exe"                       'flèche droite.
        case "+": luminosite = luminosite + 40: if luminosite > 120 then luminosite = 120
                  if luminosite = 120 then contraste = 1.05 else contraste = 1.58
        case "-": luminosite = luminosite - 40: if luminosite < 40 then luminosite = 40
                  if luminosite = 80 then contraste = 1.58 else contraste = 3.1
        case "1": lambda = 10: relief = lambda / 10: gosub PlanoConvexe
        case "2": lambda = 20: relief = lambda / 10: gosub PlanoConvexe
        case "3": lambda = 30: relief = lambda / 10: gosub PlanoConvexe
        case "4": lambda = 40: relief = lambda / 10: gosub PlanoConvexe
        case "5": lambda = 50: relief = lambda / 10: gosub PlanoConvexe
        case "6": lambda = 60: relief = lambda / 10: gosub PlanoConvexe
        case "7": lambda = 70: relief = lambda / 10: gosub PlanoConvexe
        case "8": lambda = 80: relief = lambda / 10: gosub PlanoConvexe
        case "9": lambda = 90: relief = lambda / 10: gosub PlanoConvexe
      end select
      gosub MiseAjour
    end if
'---------------------------------------- SAISIE SOURIS --------------------------------------

    if clic = 0 then getmouse xSouris, ySouris, , clic    'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
'    locate 33, 80: print xSouris; ySouris; clic; ligne; "    ";
    if ligne > 26 and ligne < 35 then
      if xSouris < 224 or xSouris > 504 then ligne = 0
    elseif ligne > 34 then
      if xSouris < 304 or xSouris > 504 then ligne = 0
    else ligne = 0  
    end if
    
    if clic = 2 and ySouris < cote / 2 then gosub ToutEffacer

'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
    color noir, turquoise
    locate ligne, 29
    select case ligne
      case 27: print ligne27$
      case 28: print ligne28$
      case 29: print ligne29$
      case 30: print ligne30$
      case 31: print ligne31$
      case 35: locate 35, 39: print ligne35$
      case 36: locate 36, 39: print ligne36$
      case 37: locate 37, 39: print ligne37$;
               if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
    end select
    color bleu, fond
    if mode$ = "convergent"   then locate 27, 29: print ligne27$
    if mode$ = "divergent"    then locate 28, 29: print ligne28$
    if mode$ = "stationnaire" then locate 29, 29: print ligne29$
    color noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
    if clic = 1 then
      clic = 0: bitmap = 0
      select case ligne
        case 27: mode$ = "convergent":   gosub PlanoConvexe
        case 28: mode$ = "divergent":    gosub PlanoConvexe
        case 29: mode$ = "stationnaire": gosub PlanoConvexe
        case 30: gosub Inverser
        case 31: gosub EffacerMemoire
        case 35: gosub Initialisation
        case 36: end
        case 37: if xSouris < 400 then run "Ether12.exe" else run "Ether14.exe"
      end select
    end if
    if bitmap then gosub Bitmaps                          'capture d'images si désiré.
'    locate 34, 2: print using "#.## sec"; timer - temps
    temps = timer
  end if
  afficher = afficher + 1
  if afficher = 2 then afficher = 0                       'afficher une fois sur deux
  if OK = 0 then
    image = image + 1
    if image = seuil then gosub Superposer
  end if
loop

afficherRelief:'---------------------- AFFICHER EN RELIEF ------------------------------------
haut = 10                                                 'doit être pair re: espaces vides.
bas = cote
for x = gauche to droite
  for y = haut to bas step 2                              'step 2 pour aplatir en ellipses.
    luminance = luminosite * (P(x, y) + contraste)
    if luminance > 255 then luminance = 255
    if luminance < 0 then luminance = 0
    yCoord = y / 2 - relief * P(x, y)                     'décaler selon l'amplitude.
    pset (x - gauche, yCoord), rgb(luminance,luminance,luminance)
    if y > haut then ecart = yCoord - yCoordPrec else ecart = 1
    if ecart > 1  then
      luminance2 = luminosite * (P(x, y - 2) + contraste) 'estomper avec le pixel précédent.
      pas = (luminance2 - luminance) / ecart
      ton = luminance                                     'ton de gris au départ.
      for j = 1 to ecart - 1                              'combler les espaces vides.
        ton = ton + pas
        if ton > 255 then ton = 255
        if ton < 0 then ton = 0
        pset(x - gauche, yCoord - j), rgb(ton, ton, ton)
      next
    end if        
    yCoordPrec = yCoord
  next
next
'if bitmap then else line(0, 398)-(cote,405), fond, BF    'niveler le bas(vérifier).
return

Bitmaps:'------------------------- CRÉER UNE SÉQUENCE BITMAP ---------------------------------
select case capture
  case is < 10: numero$ = "00"
  case is < 100: numero$ = "0"
  case is < 1000: numero$ = ""
end select
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
'Locate 3, 18:  Print " La pression de radiation exerc‚e par le champ biconvexe cr‚‚ par deux ‚lectrons.  "
'Locate 23, 18: Print "       The radiation pressure exerted by the biconvex field. glafreniere.com       "
locate 24, 18: print "                                                                                   "
locate 34, 43: print fichier$
bsave fichier$,0
color noir, fond
capture = capture + 1
if capture > 600 then end 
return

PlanoConvexe:'----- CHAMP PLANO-CONVEXE (FAIT D'ONDES PLANES ET D'ONDES DIVERGENTES) ---------
gosub ToutEffacer
ok = 0
image = 0
entier = distance / lambda                                'multiple entier de lambda.
xElectron = 700

for x = 0 to 600
  distance1 = x                                           'l'onde 1 est plane.
  xDistance2 = x - xElectron
  xCarre2 = xDistance2 * xDistance2
  for y = 0 to cote
    yDistance = y - yCentre
    distance2 = sqr(xCarre2 + yDistance * yDistance)      'l'onde 2 est sphérique.
    if xDistance2 > 0 then
      angle = atn(abs(yDistance) / xDistance2)
    elseif xDistance2 = 0 then
      angle = pi / 2
    else
      angle = pi - atn(abs(yDistance) / xDistance2)
    end if
    phi = angle / 2                                       'angle des transformations Lorentz.
    beta = sin(phi)                                       'vitesse purement théorique.
    affaiblissement = 1.5 * beta ^ 2                      'selon le carré de l'amplitude.
    phase = 2 * pi * distance1 / lambda
    amplitudeSinus = affaiblissement * sin(phase)         'amplitude normale, onde 1.
    amplitudeCosinus = affaiblissement * cos(phase)       'amplitude à la quadrature, onde 1.
    phase = 2 * pi * distance2 / lambda
    P(x,y) = amplitudeCosinus + affaiblissement * cos(phase)'potentiel à la quadrature.
    P2(x,y) = amplitudeSinus + affaiblissement * sin(phase)' ce potentiel sera superposé
  next                                                    '  après un délai d'un quart d'onde.
next

for x = 0 to 3 * lambda
  affaiblissement = x / (3 * lambda)
  for y = 0 to cote
    P(x,y) = affaiblissement * P(x,y)
    P2(x,y) = affaiblissement * P2(x,y)
  next    
next
for x = 600 - 3 * lambda to 600
  affaiblissement = (600 - x) / (3 * lambda)
  for y = 0 to cote
    P(x,y) = affaiblissement * P(x,y)
    P2(x,y) = affaiblissement * P2(x,y)
  next    
next
for y = 0 to 3 * lambda
  affaiblissement = y / (3 * lambda)
  for x = 0 to 800
    P(x,y) = affaiblissement * P(x,y)
    P2(x,y) = affaiblissement * P2(x,y)
  next
next
for y = cote - 3 * lambda to cote
  affaiblissement = (cote - y) / (3 * lambda)
  for x = 0 to 800
    P(x,y) = affaiblissement * P(x,y)
    P2(x,y) = affaiblissement * P2(x,y)
  next
next
temps = timer
return


EffacerMemoire:'--------- EFFACER LES VARIABLES MÉMOIRE: MODE STATIONNAIRE -------------------
for x = -1 to cote
  for y = -1 to cote
    M(x, y) = 0
  next
next
gosub MiseAjour
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
gris = rgb(150,150,150)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
vert = rgb(0,150,0)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               'séquence bitmap si désiré.
gauche = 0
droite = cote
lambda = 30
relief = lambda / 10
lambdaSurDeux = lambda / 2
vitesse = 1 / sqr(2)                                      'vitesse de l'onde en pixels.
seuil = lambdaSurDeux / vitesse                           'quart d'onde atteint pour ondes progressives.
mode$ = "stationnaire"
fraction = 8
luminosite = 40
contraste = 3.1
reflexion = 0
xCentre = cote / 2
yCentre = cote / 2                                        'bas de la fenêtre visible.
largeur = 600                                             'largeur de la partie visible.
hauteur = 400                                             'hauteur de la partie visible.
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer la page matrice.
color noir, fond: cls
locate 15, 10: print "R‚partition de l'amplitude en cours..."
locate 27
locate, 2:   print "Longueur d'onde : chiffre"
locate, 2:   print "de 1 … 9 (x10):"; lambda; " pixels."
locate, 2:   print "Contraste.. [+] ou [-]:"
locate, 2:   print "Relief....... F3 ou F4:"; relief; " "
'Locate 31, 2: If reflexion Then 
'             Print "Pas de r‚lexion.... F7."
'           Else
'             Print "R‚lexion........... F8."
'           End If

ligne27$ = " C- Fictif: ondes convergentes.    ": locate 27, 29: print ligne27$
ligne28$ = " D- Normal: ondes divergentes.     ": locate 28, 29: print ligne28$
ligne29$ = " S- Possible: ondes stationnaires. ": locate 29, 29: print ligne29$
ligne30$ = " R- Inverser le sens des ondes.    ": locate 30, 29: print ligne30$
ligne31$ = " E- Effacer la m‚moire.            "
ligne35$ = "  I- Initialiser.       ": locate 35, 39: print ligne35$
ligne36$ = "     Quitter (Echap).   ": locate 36, 39: print ligne36$
ligne37$ = "                        "

locate 29, 65: print "Le champ de force ne rayonne norma-"
locate 30, 65: print "lement que des ondes divergentes."
color noir, blanc
line (0,490)-(799,533), blanc, bf
locate 32, 2:  print "S'il est immobilis‚, le champ de force pr‚sente une structure semblable … celle de la lentille"
locate 33, 2:  print "diffractive. L'‚ther virtuel confirme que dans ce cas, il rayonne aussi des ondes convergentes."
line (0,490)-(799,533), noir, b

gosub flecheGauche: gosub flecheDroite
if bitmap = 1 then gosub PlanoConvexe: return

color blanc, rouge
line(511,411)-(781,431),blanc,B
line(512,412)-(782,431),blanc,B
line(513,413)-(783,431),rouge,BF
locate 27, 66: print "LE RAYONNEMENT DU CHAMP DE FORCE"
color vert, fond
locate 36, 2:  print "F‚licitations … M. Philippe Delmotte!"
locate 37, 2:  print "Gabriel LaFreniŠre  glafreniere.com";
locate 36, 63: print "29 nov. 2006. Ce programme peut ˆtre"
locate 37, 63: print "distribu‚, copi‚ ou modifi‚ librement.";

color noir
pcopy 2, 0
pcopy 2, 1
gosub PlanoConvexe
return'                              FIN DE L'INITIALISATION
'#############################################################################################


Inverser:'------------------------ INVERSER LE SENS DES ONDES --------------------------------
for x = -1 to cote
  for y = -1 to cote
    M(x, y) = -M(x, y)                                    'inverser la mémoire.
   'P(x, y)= -P(x, y)                                     'alternative: inverser le potentiel.
  next
next
gosub MiseAjour
return

MiseAjour:'------------------------------- MISE A JOUR ---------------------------------------
do
  getmouse xSouris, ySouris, , clic                       'attendre le relâchement du bouton.
loop while clic > 0                                       'éviter le piège: clic = -1 !
do: vider$ = inkey: loop while len(vider$)                'vider le tampon.
screenset 2                                               'page cachée servant de matrice.
locate 15, 10: print "Calcul en cours...                    "
locate 29, 26
if luminosite = 40 then
  print "1"
elseif luminosite = 80 then print "2"
else print "3"
end if
locate 30, 25: print relief; "  "
locate 31, 2
'If reflexion Then 
'  Print "Pas de r‚lexion.... F7."
'  Else
'  Print "R‚lexion........... F8."
'End If
locate 28, 2:   print "de 1 … 9 (x10):"; lambda; " pixels."
screenset page1
return

Superposer:'------------------- AJOUTER LES ONDES À LA QUADRATURE ----------------------------
OK = 1: image = 0
if mode$ = "stationnaire" then                            'la lentille diffractive a deux
  if alterner then                                        'configurations possibles:
    alterner = 0                                          'centre obstrué ou centre ouvert.
    return                                                'le potentiel précédent seulement.
  else
    alterner = 1
    gosub ToutEffacer                                     'le potentiel ci-dessous seulement.
  end if
end if
for x = 0 to 600
  for y = 0 to cote
    P(x,y) = P(x,y) + P2(x,y)                             'ajout du potentiel à la quadrature.
  next
next
if mode$ = "convergent" then gosub Inverser               'convergence artificielle.
return

ToutEffacer:'---------------------- INITIALISER LES VARIABLES --------------------------------
for x = -1 to cote
  for y = -1 to cote
    M(x, y) = .000000001                                  'éviter de mettre la variable à zéro.
    P(x, y) = .000000001                                  '0 exactement ralentit le calcul(?).
  next
next
contraste = 3.1
relief = lambda / 10
lambdaSurDeux = lambda / 2
vitesse = 1 / sqr(2)                                      'vitesse de l'onde en pixels.
seuil = lambdaSurDeux / vitesse                           '1/4 d'onde atteint, ondes progressives.
gosub MiseAjour
return
