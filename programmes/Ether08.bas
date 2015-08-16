cote = 800                                                'nombre de particules par côté.
dim as single potentiel(0 to 10 * cote)'                  'principe de Huygens, sous-multiples..
dim as single M(-1 to cote+1, -1 to cote+1)
dim as single I(-1 to cote+1, -1 to cote+1): dim precedent(-1 to cote+1)
dim as single P(-1 to cote+1, -1 to cote+1), potentiel2(-1 to cote+1, -1 to cote+1)
dim as single betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, pas, ton
dim as single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance
dim as single xCoord, distNormale, periode, distance, rotation, amplitude, phi
dim as single ondelette, facteur, rapport, longueur, luminosite, contraste, relief
dim as single pi, angle, lambda, xCarre, yCarre, arcSinus, xDistance, affaiblissement
screen 19,24,3: gosub Initialisation

do'    MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.

  for y = 0 to cote - 1
    I(0,y) = (P(-1,y) + P(1,y) + P(0,y-1) + P(0,y+1)) / 4 - P(0,y)' un cran à l'avance.
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
  
  if clic = 0 then getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  if afficher = 0 or clic > 0 then                        'afficher une fois sur deux.
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    gosub afficherRelief'------------- DIAGRAMME PRINCIPAL -----------------------------------

'--------------------------------------- SAISIE CLAVIER --------------------------------------
    saisie$ = inkey
    if len(saisie$) then
      bitmap = 0
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
      select case saisie$
        case "I": luminosite = 40
                  contraste = 3.1
                  demiOnde = 1
                  if lambda <> 40  then lambda = 40: gosub Huygens
                  relief = lambda / 10
                  gosub HuitiemeDeCercle
        case "R": gosub Inverser
        case "S": gosub EffacerMemoire
        case "C": gosub Cercle
        case "D": gosub DemiCercle
        case "E": gosub ToutEffacer
        case "H": gosub HuitiemeDeCercle
        case "L": gosub OndePlane
        case "P": gosub Rotation1
        case "=+": relief = relief - 1: if relief < 0 then relief = 0   'F3.
        case ">+": relief = relief + 1: if relief > 10 then relief = 10 'F4.
        case "?+": demiOnde = 0                                         'F5.
        case "@+": demiOnde = 1                                         'F6.
        case "A+": reflexion = 0                                        'F7.
        case "B+": reflexion = 1                                        'F8.
        case "k+", "X+", chr$(27): end
        case "M": run "Ether00.exe"
        case "K+":run "Ether07.exe"                       'flèche gauche.
        case "M+":run "Ether09.exe"                       'flèche droite.
        case "+": luminosite = luminosite + 40: if luminosite > 120 then luminosite = 120
                  if luminosite = 120 then contraste = 1.05 else contraste = 1.58
        case "-": luminosite = luminosite - 40: if luminosite < 40 then luminosite = 40
                  if luminosite = 80 then contraste = 1.58 else contraste = 3.1
        case "1": lambda = 10: gosub Huygens
        case "2": lambda = 20: gosub Huygens
        case "3": lambda = 30: gosub Huygens
        case "4": lambda = 40: gosub Huygens
        case "5": lambda = 50: gosub Huygens
        case "6": lambda = 60: gosub Huygens
        case "7": lambda = 70: gosub Huygens
        case "8": lambda = 80: gosub Huygens
        case "9": lambda = 90: gosub Huygens
      end select
      gosub MiseAjour
    end if
'---------------------------------------- SAISIE SOURIS --------------------------------------

    if clic = 0 then getmouse xSouris, ySouris, , clic    'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
    if ligne > 26 and ligne < 38 then
      if xSouris < 304 or xSouris > 496 then ligne = 0
    else ligne = 0  
    end if
    
    if clic = 1 and ySouris < cote / 2 then gosub RondsDansLeau' clic sur l'écran.
    if clic = 2 and ySouris < cote / 2 then gosub ToutEffacer

'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
    color noir, turquoise
    locate ligne, 39
    select case ligne
      case 27: print ligne27$
      case 28: print ligne28$
      case 29: print ligne29$
      case 30: print ligne30$
      case 31: print ligne31$
      case 32: print ligne32$
      case 33: print ligne33$
      case 34: print ligne34$
      case 35: print ligne35$
      case 36: print ligne36$
      case 37: print ligne37$;: if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
    end select
    color noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
    if clic = 1 then
      bitmap = 0
      select case ligne
        case 27: gosub ToutEffacer
        case 28: gosub Inverser
        case 29: gosub EffacerMemoire
        case 30: gosub Cercle
        case 31: gosub DemiCercle
        case 32: gosub HuitiemeDeCercle
        case 33: gosub OndePlane
        case 34: gosub Rotation1
        case 35: if lambda <> 40  then
                    lambda = 40: gosub Huygens
                 else gosub Cercle
                 end if 
        case 36: end
        case 37: if xSouris < 400 then run "Ether07.exe" else run "Ether09.exe"
      end select
    end if
    if bitmap then gosub Bitmaps                          'capture d'images si désiré.
'    locate 34, 65: print using "#.## sec"; timer - temps
'    temps = timer
  end if
  clic = 0
  afficher = afficher + 1
  if afficher = 2 then afficher = 0                       'afficher une fois sur deux
  if passage then                                         'rotation, ajouter deux positrons.
    passage = passage - 1
    if passage = 0 then gosub Rotation2
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

'---------------------------- ONDES RÉPARTIES SUR UNE CIRCONFÉRENCE --------------------------
Cercle:
pcopy 2, page1
pcopy 2, page2
gosub ToutEffacer
reflexion = 0
rayon = .667 * (cote / 2 )                                'appriximatif, à préciser.
nombre = 2 * pi * rayon / lambda                          'une ondelette par longueur d'onde.
circonference = nombre * lambda                           'nombre entier multiple de lambda.
if demiOnde then nombre = nombre * 2'                     'deux ondelettes par longueur d'onde.
rayon = circonference / (2 * pi)                          'rayon exact.
facteur = 3 / sqr(nombre)'                                'ajuste l'amplitude selon le nombre.

for j = 1 to nombre
  angle = 2 * pi * j / nombre
  xPixel = cote / 2 + rayon * sin(angle)
  yPixel = cote / 2 + rayon * cos(angle)
  for x = -1 to cote
    x2 = xPixel - x + cote / 2
    if x2 < 0 or x2 > cote then x2 = -1
    for y = -1 to cote
      y2 = yPixel - y + cote / 2
      if y2 < 0 or y2 > cote then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    next
  next
next
gosub MiseAjour
return

'---------------------------- ONDES RÉPARTIES SUR UN DEMI-CERCLE -----------------------------
DemiCercle:
pcopy 2, page1
pcopy 2, page2
gosub ToutEffacer
reflexion = 0
rayon = .667 * (cote / 2 )                                'appriximatif, à préciser.
nombre = 2 * pi * rayon / lambda                          'nombre d'ondelettes, circonférence.
circonference = nombre * lambda                           'nombre entier multiple de lambda.
if demiOnde = 0 then nombre = nombre / 2                  'une ondelette par longueur d'onde.
rayon = circonference / (2 * pi)                          'rayon exact.
facteur = 3 / sqr(nombre)'                                'ajuste l'amplitude selon le nombre.

for j = 1 to nombre                                       'même nombre que pour un cercle.
  angle = pi + pi * j / nombre                            'pi et non 2 * pi pour demi-cercle.
  xPixel = cote / 2 + rayon * sin(angle)
  yPixel = cote / 2 + rayon * cos(angle)
  for x = -1 to cote
    x2 = xPixel - x + cote / 2
    if x2 < 0 or x2 > cote then x2 = -1
    for y = -1 to cote
      y2 = yPixel - y + cote / 2
      if y2 < 0 or y2 > cote then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    next
  next
next
gosub MiseAjour
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

'------------------------ ONDES RÉPARTIES SUR UN ARC DE CERCLE -------------------------------
HuitiemeDeCercle:
pcopy 2, page1
pcopy 2, page2
gosub ToutEffacer
reflexion = 0
rayon = .75 * cote                                        'rayon approximatif provisoire.
nombre = 2 * pi * rayon / lambda                          'une ondelette par longueur d'onde.
circonference = nombre * lambda                           'nombre entier multiple de lambda.
rayon = circonference / (2 * pi)                          'rayon exact.
longueur = circonference / 8                              'longueur de l'arc de cercle.
if demiOnde then nombre = nombre / 4 else nombre = nombre / 8'nombre sur un 8e de cercle.
pas = pi / 4 / nombre
angle = pi + .375 * pi
facteur = 3 / sqr(nombre)'                                'ajuste l'amplitude selon le nombre.

for j = 1 to nombre
  xPixel = .9 * cote + rayon * sin(angle)
  yPixel = .5 * cote + rayon * cos(angle)
  for x = -1 to cote
    x2 = xPixel - x + cote / 2
    if x2 < 0 or x2 > cote then x2 = -1
    for y = -1 to cote
      y2 = yPixel - y + cote / 2
      if y2 < 0 or y2 > cote then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    next
  next
  angle = angle + pas
next
gosub MiseAjour
return

Huygens:'-------------- DISTRIBUTION DU POTENTIEL SELON LE PRINCIPE DE HUYGENS ---------------
gosub ToutEffacer
nombre = 1
relief = lambda / 10                                      'facteur d'amplitude du relief.
gosub MiseAjour
pcopy 2, 0
screenset 0, 0
locate 17, 10: print "Longueur d'onde (lambda) actuelle: "; lambda; " pixels." 
locate 19, 10: print "Appuyez sur un chiffre pour la modifier au besoin: lambda = 10.n" 

for x = 0 to 8 * cote                                     'surmultiplier: précision accrue.
  amplitudeSinus = 0
  amplitudeCosinus = 0
  deuxPiDistanceSurLambda = (2 * pi * x / lambda) / 8
  for angle = 0 to pi step ondelette
      periode = deuxPiDistanceSurLambda * cos(angle)
      amplitudeSinus = amplitudeSinus + sin(periode)
      amplitudeCosinus = amplitudeCosinus + cos(periode)
  next
  amplitude = sgn(amplitudeCosinus) * sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
  'if amplitudeCosinus < 0 then amplitude = -amplitude contourné par SGN.
  xCoord = x / 1000
  distNormale = pi ^ (-xCoord ^ 2)                        'distribution normale approchée.
  potentiel(x) = pi * ondelette * amplitude * distNormale 'potentiel selon 2 * distance.
  pset (xCentre - x / 8 , yCentre / 2 - 4 * potentiel(x)), 0
  pset (xCentre + x / 8 , yCentre / 2 - 4 * potentiel(x)), 0
next

for x = 0 to cote
  xDistance = x - cote / 2
  xCarre = xDistance * xDistance
  for y = 0 to cote
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    dist = 8 * sqr(xCarre + yCarre)
    P(x, y) = potentiel(dist)                             'distribution du potentiel.
    potentiel2(x, y) = P(x, y)
  next
next
temps = timer
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
ondelette = pi / 100                                      'cent ondelettes est un minimum.
'bitmap = 1                                               'séquence bitmap si désiré.
gauche = 0
droite = cote
page2 = 1                                                 'page1 correspond à 0 au départ.
lambda = 40
luminosite = 40
contraste = 3.1
demiOnde = 1
xCentre = cote / 2
yCentre = cote / 2
largeur = 600                                             'largeur de la partie visible.
hauteur = 400                                             'hauteur de la partie visible.
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
windowtitle "Ether08  -  Le principe de Huygens appliqué à l'Éther Virtuel"
locate 6, 10:  print "L'‚ther virtuel est une invention de M. Philippe Delmotte."
locate 15, 10: print "R‚partition de l'amplitude en cours..."
locate 27
locate, 2:   print "Cliquez sur l'‚cran pour d‚marrer une"
locate, 2:   print "onde. Bouton droit: tout effacer.    "

ligne27$ = "  E - Tout effacer.      ": locate 27, 39: print ligne27$
ligne28$ = "  R - Inverser le sens.  ": locate 28, 39: print ligne28$
ligne29$ = "  S - Mode stationnaire. ": locate 29, 39: print ligne29$
ligne30$ = "  C - Cercle complet.    ": locate 30, 39: print ligne30$
ligne31$ = "  D - Demi-cercle.       ": locate 31, 39: print ligne31$
ligne32$ = "  H - HuitiŠme de cercle.": locate 32, 39: print ligne32$
ligne33$ = "  L - Ondes planes.      ": locate 33, 39: print ligne33$
ligne34$ = "  P - SystŠme rotatif.   ": locate 34, 39: print ligne34$
ligne35$ = "  I - Initialiser.       ": locate 35, 39: print ligne35$
ligne36$ = "      Quitter (Echap).   ": locate 36, 39: print ligne36$
ligne37$ = "                         "

locate 28
locate, 65: print "L'‚ther virtuel permet d'observer  "
locate, 65: print "tous les ph‚nomŠnes ondulatoires, y"
locate, 65: print "compris la figure d'Airy et la dif-"
locate, 65: print "fraction de Fresnel. Quelques onde-"
locate, 65: print "lettes de Huygens suffisent parfois."

gosub flecheGauche: gosub flecheDroite
color blanc, rouge
line(548,411)-(744,430),blanc,B
line(549,412)-(745,431),blanc,B
line(550,413)-(745,431),rouge,BF
locate 27, 71: print "LE PRINCIPE DE HUYGENS"
locate 35: color vert, fond
locate , 2: print "Un grand merci … M. Anselme Dewavrin"
locate , 2: print "ainsi qu'aux cr‚ateurs de FreeBASIC."
locate , 2: print "Gabriel LaFreniŠre  glafreniere.com";
locate 35
locate , 65: print "Le 7 d‚cembre 2006. Ce programme"
locate , 65: print "FreeBASIC peut ˆtre distribu‚, "
locate , 65: print "copi‚ ou modifi‚ librement.";
color noir
pcopy 2, 0
pcopy 2, 1
gosub Huygens
gosub HuitiemeDeCercle
return
'#############################################################################################


Inverser:'------------------------ INVERSER LE SENS DES ONDES --------------------------------
for x = -1 to cote
  for y = -1 to cote
    M(x, y) = -M(x, y)
   'P(x, y)= -P(x, y)                                     'alternative.
  next
next
gosub MiseAjour
return

MiseAjour:'------------------------------- MISE A JOUR ---------------------------------------
do
  getmouse xSouris, ySouris, , clic                       'attendre le relâchement du bouton.
loop while clic > 0                                       'éviter le piège: clic = -1 !
do: vider$ = inkey: loop while len(vider$)                'vider le tampon.
clic = 0                                                  'éviter les actions à répétition.
screenset 2                                               'page cachée servant de matrice.
locate 30, 2
  print "Contraste: appuyez sur [+] ou [-]: ";
if luminosite = 40 then
  print "1"
elseif luminosite = 80 then print "2"
else print "3"
end if
locate 29, 2
print "Lambda:"; lambda; " pixels (chiffre de 1 … 9)."
locate 31, 2
  print "Amplitude du relief..... F3 ou F4:"; relief;" "
locate 32, 2
if demiOnde then 
  print "Espacement onde entiŠre....... F5    "
  else
  print "Espacement demi-onde.......... F6    "
end if
locate 33, 2
if reflexion then 
  print "Pas de r‚lexion............... F7    "
  else
  print "R‚lexion gauche et droite..... F8    "
end if
locate 33, 70: if nombre then print "Nombre d'ondelettes:"; nombre; "  " else print "                       "
screenset page1
return

'---------------------------- ONDES RÉPARTIES SUR UNE LIGNE DROITE ---------------------------
OndePlane:
pcopy 2, page1
pcopy 2, page2
gosub ToutEffacer
longueur = .5 * cote                                      'longueur approximative.
nombre = longueur / lambda + 1                            'une ondelette par longueur d'onde.
longueur = lambda * (nombre - 1)                          'entier, longueur exacte.
if demiOnde then nombre = nombre * 2'                     'deux ondelettes par longueur d'onde.
facteur = 3 / sqr(nombre)'                                'ajuste l'amplitude selon le nombre.
xPixel = 0
if demiOnde then
haut = (cote - ((nombre + 1) * (lambda / 2))) / 2
else
haut = (cote - ((nombre + 1) * lambda)) / 2
end if


for j = 1 to nombre
  if demiOnde then
  yPixel = haut + j * (lambda / 2)
  else
  yPixel = haut + j * lambda
  end if

  for x = -1 to cote
    x2 = xPixel - x + cote / 2
    if x2 < 0 or x2 > cote then x2 = -1
    for y = -1 to cote
      y2 = yPixel - y + cote / 2
      if y2 < 0 or y2 > cote then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    next
  next
next
reflexion = 1                                             'sur les côtés seulement.
gosub MiseAjour
return

'-------------------------------- ONDE STATIONNAIRE CIRCULAIRE -------------------------------
RondsDansLeau:
if xSouris < 50 then xSouris = 50
if ySouris < 50 then ySouris = 50
if xSouris > cote - 50 then xSouris = cote - 50
if ySouris > cote / 2 - 50 then ySouris = cote / 2 - 50
ySouris = 2 * ySouris                                     'l'écran réel vaut le double.    
for x = -1 to cote
  x2 = xSouris - x + cote / 2
  if x2 < 0 or x2 > cote then x2 = -1
  for y = -1 to cote
    y2 = ySouris - y + cote / 2
    if y2 < 0 or y2 > cote then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2)                'ajout d'un nouveau potentiel.
  next
next
nombre = nombre + 1
gosub MiseAjour
return

Rotation1:'----------- ROTATION DE PHASE, MISE EN PLACE DE 2 ÉLECTRONS -----------------------
gosub ToutEffacer
if demiOnde then fraction = 8 else fraction = 1
reflexion = 0
passage = lambda / 2
xSouris = cote / 2 - lambda / fraction                    'électron, spin -1/2
ySouris = cote / 2 - lambda / fraction
for x = -1 to cote
  x2 = xSouris - x + cote / 2
  if x2 < 0 or x2 > cote then x2 = -1
  for y = -1 to cote
    y2 = ySouris - y + cote / 2
    if y2 < 0 or y2 > cote then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2)                'ajout d'un nouveau potentiel.
  next
next
xSouris = cote / 2 + lambda / fraction                    'électron, spin +1/2
ySouris = cote / 2 + lambda / fraction
for x = -1 to cote
  x2 = xSouris - x + cote / 2
  if x2 < 0 or x2 > cote then x2 = -1
  for y = -1 to cote
    y2 = ySouris - y + cote / 2
    if y2 < 0 or y2 > cote then y2 = -1
    P(x, y) = P(x, y) - potentiel2(x2, y2)                'potentiel en opposition de phase.
  next
next
nombre = 4
gosub MiseAjour
return

Rotation2:'-------------- ROTATION DE PHASE, AJOUT DE 2 POSITRONS ----------------------------
xSouris = cote / 2 + lambda / fraction                    'positron, spin pi.
ySouris = cote / 2 - lambda / fraction
for x = -1 to cote
  x2 = xSouris - x + cote / 2
  if x2 < 0 or x2 > cote then x2 = -1
  for y = -1 to cote
    y2 = ySouris - y + cote / 2
    if y2 < 0 or y2 > cote then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2)                'ajout d'un nouveau potentiel.
  next
next
xSouris = cote / 2 - lambda / fraction                    'positron, spin 2 pi.
ySouris = cote / 2 + lambda / fraction
for x = -1 to cote
  x2 = xSouris - x + cote / 2
  if x2 < 0 or x2 > cote then x2 = -1
  for y = -1 to cote
    y2 = ySouris - y + cote / 2
    if y2 < 0 or y2 > cote then y2 = -1
    P(x, y) = P(x, y) - potentiel2(x2, y2)                'potentiel en opposition de phase.
  next
next
return

ToutEffacer:'---------------------- INITIALISER LES VARIABLES --------------------------------
for x = -1 to cote
  for y = -1 to cote
    M(x, y) = .000000001                                  'éviter de mettre la variable à zéro.
    P(x, y) = .000000001                                  ' 0 exactement ralentit le calcul(?).
  next
next
nombre = 0
gosub MiseAJour
return
