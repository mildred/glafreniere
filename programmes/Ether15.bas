page1 = 1: hauteur = 300: longueur = 799
dim as single periodeAxe(longueur), amplitudeAxe(longueur)
dim as single periode(longueur, -hauteur to hauteur), gris(longueur, -hauteur to hauteur)
dim as single couleur(longueur, -hauteur to hauteur), luminance(longueur, -hauteur to hauteur)
dim as single AB, BC, AC, CD, BCA, arc, angle
dim as single xCoord, yCoord, rotation, amplitude, phase, ton, pas
dim as single ondelette, facteur, rapport, luminosite, rouge, vert, bleu
dim as single xCarre, yCarre, phase1, phase2, distance, xDistance, yDistance
dim as single pi, zoomHorizontal, lambda, lambdaPrec, lambdaSurDeux, normaliser
dim as single amplitudeSinus, amplitudeCosinus, differenceDeMarche, affaiblissement
screen 19,24,3: gosub Initialisation

do'<<<<<<<<<<<<<<<<<<< LA DIFFRACTION DE FRESNEL - CALCUL SELON HUYGENS >>>>>>>>>>>>>>>>>>>>>>
  
  getmouse xSouris, ySouris, , clic
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  gosub Afficher'------------------------- DIAGRAMME -----------------------------------------

'--------------------------------------- SAISIE CLAVIER --------------------------------------
  if Saisie$ = "" then Saisie$ = inkey
  if len(Saisie$) then
    bitmap = 0
    if len(Saisie$) = 2 then Saisie$ = right(Saisie$, 1) + "+" else Saisie$ = ucase(Saisie$)
    select case Saisie$
      case "A": mode$ = "axial": images = 255: Saisie$ = "nul"
                screenset 2: color rgb(0,0,255)
                locate 34, 39: print ligne34$: color noir
                locate 32, 39: print ligne32$
                locate 33, 39: print ligne33$
                pcopy 2, page1
      case "C": if source$ <> "circulaire"   then
                  source$ = "circulaire"
                  gosub MiseAjour: sourceLin = 0: sourceCirc = 0
                end if
      case "E": mode$ = "gris": images = 32: Saisie$ = "nul"
                screenset 2: color rgb(0,0,255)
                locate 32, 39: print ligne32$: color noir
                locate 33, 39: print ligne33$
                locate 34, 39: print ligne34$
                pcopy 2, page1
      case "I": gosub Initialisation
      case "L": if source$ <> "lineaire"   then
                  source$ = "lineaire"
                  gosub MiseAjour: sourceLin = 0: sourceCirc = 0
                end if
      case "M": run "Ether00.exe"
      case "O": mode$ = "couleurs": images = 32: Saisie$ = "nul"
                screenset 2: color rgb(0,0,255)
                locate 33, 39: print ligne33$: color noir
                locate 32, 39: print ligne32$
                locate 34, 39: print ligne34$
                pcopy 2, page1
      case "k+", "X+", chr$(27): end
      case "K+":run "Ether14.exe"                         'flèche gauche.
      case "M+":run "Ether16.exe"                         'flèche droite.
    end select
    if Saisie$ = "nul" then Saisie$ = "" else gosub MiseAjour
  end if
'---------------------------------------- SAISIE SOURIS --------------------------------------

  if clic = 1 then else getmouse xSouris, ySouris, , clic
  ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
  if ligne > 25 and ligne < 38 then
    if xSouris < 304 or xSouris > 498 then ligne = 0
  else ligne = 0  
  end if
'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 39
  select case ligne
    case 26: print ligne37$: locate 26, 39                'effacer.
      print " Longueur d'onde:"; lambda
    case 28: print ligne37$: locate 28, 39                'effacer.
      if source$ = "lineaire" then
        print " Longueur (source):"; longueurSource
      else print " Rayon (source):"; rayon
      end if
    case 29: print ligne29$
    case 30: if source$ = "lineaire" then else print ligne30$
    case 31: if source$ = "circulaire" then else print ligne31$
    case 32: if mode$ = "gris" then else print ligne32$
    case 33: if mode$ = "couleurs" then else print ligne33$
    case 34: if mode$ = "axial" then else print ligne34$
    case 35: print ligne35$
    case 36: print ligne36$
    case 37: print ligne37$;: if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
  if clic = 1 then
    if ySouris < 300 and xSouris > 0 then
      distanceCoupe = xSouris
      facteur = rayon ^ 2 / (distanceCoupe * lambda / zoomHorizontal)
      locate 20,6: print using "#.##   "; facteur        'nombre de Fresnel, non entier.
      end if
    bitmap = 0
    select case ligne
      case 30: if source$ <> "lineaire" then
                  source$ = "lineaire"
                  gosub MiseAjour: sourceLin = 0: sourceCirc = 0
               end if
      case 31: if source$ <> "circulaire" then
                  source$ = "circulaire"
                  gosub MiseAjour: sourceLin = 0: sourceCirc = 0
               end if
      case 32: mode$ = "gris": images = 32:     gosub MiseAJour
      case 33: mode$ = "couleurs": images = 32: gosub MiseAJour
      case 34: mode$ = "axial": images = 255:   gosub MiseAJour
      case 26: gosub CurseurLambda
      case 28: gosub CurseurSource
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether14.exe" else run "Ether16.exe"
    end select
  end if
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
loop

Afficher:'------------------------- AFFICHER LES GRAPHIQUES ----------------------------------

if source$ = "lineaire" then
  if sourceLin = 0 then gosub SourceLineaire
else
  if sourceCirc = 0 then  gosub SourceCirculaire
end if
image = image + 1
if image = images then image = 0
phase = 2 * pi * image / images

if mode$ = "couleurs" then
  for x = 0 to longueur
    for y = 0 to hauteur / 2
      rouge = 100 * (sin(periode(x, y) - phase) + 1)
      vert = 100 * (sin(periode(x, y) - phase + pi) + 1)
      rouge = rouge + (luminance(x, y) - 155)
      vert = vert + (luminance(x, y) - 155)
      if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
      if vert > 255 then vert = 255 else if vert < 0 then vert = 0
      bleu = (rouge + vert) / 2
      if lambda * zoomHorizontal < 6 then rouge = gris(x, y): vert = gris(x, y): bleu = gris(x, y)
      pset(x, yCentre - y), rgb(rouge, vert, bleu)
      pset(x, yCentre + y), rgb(rouge, vert, bleu)
    next
  next
  line(distanceCoupe, 0)-(distanceCoupe, hauteur), blanc
elseif mode$ = "gris" then
  for x = 0 to longueur
    for y = 0 to hauteur / 2
      pset(x, yCentre - y), gris(x, y)
      pset(x, yCentre + y), gris(x, y)
    next
  next
  line(distanceCoupe, 0)-(distanceCoupe, hauteur), blanc
elseif mode$ = "axial" then
  yPrecedent = 0                                          'amplitude précédente.
  yPhasePrec = yCentre
  for x = 0 to longueur
    amplitudeSinus = sin(periodeAxe(x) - phase + pi / 4)
    yPhase = yCentre + 60 * sin(periodeAxe(x) - phase)
    if amplitudeSinus > 0 then line(x, yPhasePrec)-(x, yPhase), noir
    yCoord = .5 * amplitudeAxe(x)
    line(x, yCentre + yCoord)-(x, yCentre - yCoord), rgb(gris(x,0), gris(x,0), gris(x,0))
    line(x, yCentre - yPrecedent)-(x, yCentre - yCoord), noir   'courbe d'amplitude.
    line(x, yCentre + yPrecedent)-(x, yCentre + yCoord), noir
    if amplitudeSinus < 0 then line(x, yPhasePrec)-(x, yPhase), noir
    yPrecedent = yCoord
    yPhasePrec = yPhase
  next
  line(0,hauteur)-(799,hauteur), noir
  line(distanceCoupe, 70)-(distanceCoupe, hauteur), noir
end if
if mode$ = "axial" then ton = noir else ton = blanc
for n = 1 to 6                                            'repères selon le nombre de Fresnel.
  distance = zoomHorizontal * rayon ^ 2 / (n * lambda)
  line(distance, hauteur - 10)-(distance, hauteur), ton
  if distance > 50 and distance < 790 then locate 20, distance / 8: print n
next
if source$ = "lineaire" then                              'vue en coupe, 2D.
  for x = 0 to hauteur / 2
    yCoord = 450 - .3 * luminance(distanceCoupe, x)
    line(hauteur / 2 + x, yCoord)-(hauteur / 2 + x, 599), gris(distanceCoupe, x)
    line(hauteur / 2 - x, yCoord)-(hauteur / 2 - x, 599), gris(distanceCoupe, x)
    pset(hauteur / 2 + x, yCoord), noir
    pset(hauteur / 2 - x, yCoord), noir
  next
else
  for x = 0 to hauteur / 2                                'vue en coupe, 3D.
    xCarre = x ^ 2
    for y = 0 to hauteur / 2 step 2                       'graphique elliptique.
      yDist = sqr(y ^ 2 + xCarre)
      if yDist <= hauteur / 2 then
        pset(hauteur / 2 + x, 520 - y / 2), gris(distanceCoupe,yDist)
        pset(hauteur / 2 - x, 520 - y / 2), gris(distanceCoupe,yDist)
        pset(hauteur / 2 + x, 520 + y / 2), gris(distanceCoupe,yDist)
        pset(hauteur / 2 - x, 520 + y / 2), gris(distanceCoupe,yDist)
      end if
    next
  next
  yPrec = 436 - .3 * luminance(distanceCoupe, 0)
  for xPoint = 0 to hauteur / 2
    yCoord = 436 - .3 * luminance(distanceCoupe, xPoint)
    line(hauteur / 2 + xPoint, yCoord)-(hauteur / 2 + xPoint, 436), gris(distanceCoupe,xPoint)
    line(hauteur / 2 - xPoint, yCoord)-(hauteur / 2 - xPoint, 436), gris(distanceCoupe,xPoint)
    line(hauteur / 2 + xPoint - 1, yPrec)-(hauteur / 2 + xPoint, yCoord), noir
    line(hauteur / 2 - xPoint + 1, yPrec)-(hauteur / 2 - xPoint, yCoord), noir
    yPrec = yCoord
  next
  line(hauteur / 2 - xPoint, 436)-(hauteur / 2 + xPoint, 436), noir
end if
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
if capture > 100 then end 
return

CurseurLambda:'--------------- MODIFICATION DE LA LONGUEUR D'ONDE ----------------------------
screenset 2
color noir, fond
lambdaPrec = lambda
minimum = 200 + 5
maximum = 200 + 400
do
  swap page1, page2
  screenset page1, page2
  cls
  curseur = 200 + 4 * lambda
  if curseur < minimum then curseur = minimum else if curseur > maximum then curseur = maximum
  line(minimum - 2,399)-(maximum + 2, 415), noir, b
  line(curseur - 1,399)-(curseur + 1, 415), noir, bf
  DistanceCoupe = longueurSource ^ 2 / (5 * lambda)
  rayon = int(longueurSource / 2)
  zoomHorizontal = 600 / DistanceCoupe
  if zoomHorizontal > 1 then zoomHorizontal = 1
  locate 22, 27: if lambda * zoomHorizontal < 6 then print "Les ondes sont trop courtes pour ˆtre montr‚es en couleurs." 
  locate 24, 27: print "Longueur d'onde (lambda):"; lambda
  if ySouris > 399 and ySouris < 416 then 
    lambda = int((xSouris - 200) / 4)
  else lambda = lambdaPrec
  end if
  if lambda > 100 then lambda = 100 else if lambda < 1 then lambda = 1
  getmouse xSouris, ySouris, , clic
loop while clic = 1
if lambdaPrec = lambda then else gosub MiseAJour: sourceLin = 0: sourceCirc = 0
return

CurseurSource:'------------ MODIFICATION DE LA LONGUEUR DE LA SOURCE -------------------------
screenset 2
color noir, fond
longueurPrecedente = longueurSource
minimum = 100
maximum = 700
do
  swap page1, page2
  screenset page1, page2
  cls
  curseur = longueurSource
  if curseur < minimum then curseur = minimum else if curseur > maximum then curseur = maximum
  line(minimum - 2,431)-(maximum + 2, 447), noir, b
  line(curseur - 1,431)-(curseur + 1, 447), noir, bf
  DistanceCoupe = longueurSource ^ 2 / (5 * lambda)
  rayon = int(longueurSource / 2)
  zoomHorizontal = 600 / DistanceCoupe
  if zoomHorizontal > 1 then zoomHorizontal = 1
  locate 22, 27: if lambda * zoomHorizontal < 6 then print "Les ondes sont trop courtes pour ˆtre montr‚es en couleurs." 
  locate 26, 27
  if source$ = "lineaire" then print " Longueur (source):"; longueurSource else print " Rayon (source):"; int(longueurSource / 2)
  if ySouris > 430 and ySouris < 448 then
    longueurSource = xSouris: rayon = int(longueurSource / 2)
    else longueurSource = longueurPrecedente: rayon = int(longueurSource / 2)
  end if
  if longueurSource > 700 then longueurSource = 700
  if longueurSource < 100 then longueurSource = 100
  getmouse xSouris, ySouris, , clic
loop while clic = 1
if longueurPrecedente = longueurSource then else gosub MiseAJour: sourceLin = 0: sourceCirc = 0
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
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               'séquence bitmap si désiré.
'mode$ = "couleurs"
mode$ = "gris"
source$ = "lineaire"
source$ = "circulaire"
sourceLin = 0: sourceCirc = 0                             'calcul à faire ou à refaire.
longueurSource = 240
rayon = int(longueurSource / 2)
lambda = 20                                               'longueur d'onde.
images = 32
xCentre = longueur / 2
yCentre = hauteur / 2
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
ligne30$ = " L - Source lin‚aire.   ": locate 30, 39: print ligne30$
ligne31$ = " C - Source circulaire. ": locate 31, 39: print ligne31$
ligne32$ = " E - Echelle de gris.   ": locate 32, 39: print ligne32$
ligne33$ = " O - Ondes en couleurs. ": locate 33, 39: print ligne33$
ligne34$ = " A - Rayonnement axial. ": locate 34, 39: print ligne34$
ligne35$ = " I - Initialiser.       ": locate 35, 39: print ligne35$
ligne36$ = "     Quitter (Echap).   ": locate 36, 39: print ligne36$
ligne37$ = "                        "
locate 1
locate, 2: print "Le graphique axial montre que les phases n'‚voluent pas r‚guliŠrement le long de l'axe."
locate, 2: print "Etonnamment, les ondes circulent plus vite que la lumiŠre au centre des lobes … cause d'un effet"
locate, 2: print "de ciseau. Elles subissent ensuite un retard de phase d'une demi-p‚riode entre chaque lobe si la"
locate, 2: print "source est circulaire. Mais si la source est lin‚aire, cet effet est beaucoup plus faible."
locate 20, 2: print "n ="
locate 21, 2: print "Nombre de Fresnel: L = R ^ 2 / (n * lambda)"
'Locate, 64: Print "R: rayon ou demi-longueur."
locate 21
locate, 64: print "Ce programme fait appel au principe"
locate, 64: print "de Huygens. Cette m‚thode a produit"
locate, 64: print "jusqu'ici des r‚sultats identiques "
locate, 64: print "… ceux que montre l'‚ther virtuel. "
print
locate, 64: print "Remarquez les variations du rayon- "
locate, 64: print "nement le long de l'axe central. Ce"
locate, 64: print "ph‚nomŠne se produit aussi dans le "
locate, 64: print "rayonnement des champs de force."
print
locate, 64: print "Cela permet d'expliquer la structure"
locate, 64: print "de la matiŠre et sa m‚canique, par "
locate, 64: print "exemple la force de Coulomb."

gosub flecheGauche: gosub flecheDroite
color blanc, rgb(0,0,200)
line(264,349)-(479,368),rgb(0,0,200),BF
line(264,349)-(479,368),blanc,B
line(263,348)-(480,369),noir,B
locate 23, 35: print "LA DIFFRACTION DE FRESNEL"
color rgb(0,150,0), fond
locate 35, 63: print "29 nov. 2006. Ce programme peut ˆtre"
locate 36, 63: print "distribu‚, copi‚ ou modifi‚ librement.";
locate 37, 63: print "Gabriel LaFreniŠre   glafreniere.com";
color noir
gosub MiseAJour
pcopy 2, 0
pcopy 2, 1
return
'#############################################################################################

MiseAjour:'------------------------------- MISE A JOUR ---------------------------------------
screenset 2, 2                                            'page cachée servant de matrice.
lambdaSurDeux = lambda / 2
DistanceCoupe = longueurSource ^ 2 / (5 * lambda)
rayon = int(longueurSource / 2)
zoomHorizontal = 600 / DistanceCoupe
if zoomHorizontal > 1 then zoomHorizontal = 1
distanceCoupe = 600
if mode$ = "axial" then images = 255 else images = 24
locate 26, 39: print ligne37$
locate 26, 39: if lambda < 0 then print using " Longueur d'onde: #.##"; lambda; else print " Longueur d'onde:"; lambda 
locate 27, 39: print " Zoom horizontal:";
if zoomHorizontal < 9.995 then print using " #.##"; zoomHorizontal else print using "##.#"; zoomHorizontal
locate 28, 39: print ligne37$: locate 28, 39              'effacer.
if source$ = "lineaire" then
     print " Longueur (source):"; longueurSource
else print " Rayon (source):"; rayon
end if
if lambda * zoomHorizontal < 6 then ligne33$= "     Couleur impossible." else ligne33$ = " O - Ondes en couleurs. "
locate 30, 39: print ligne30$
locate 31, 39: print ligne31$
locate 32, 39: print ligne32$
locate 33, 39: print ligne33$
locate 34, 39: print ligne34$
color rgb(0,0,255)                                        'choix en cours affichés en bleu.
if source$ = "circulaire" then
  locate 31, 39: print ligne31$
else locate 30, 39: print ligne30$
end if
select case mode$
  case "gris":     locate 32, 39: print ligne32$
  case "couleurs": locate 33, 39: print ligne33$
  case "axial":    locate 34, 39: print ligne34$
end select
color noir, fond
return

SourceCirculaire:'-------------- CALCUL D'UNE SOURCE CIRCULAIRE ------------------------------
pcopy 2, page1
screenset page1, page1
line(0, 0)-(longueur, hauteur),fond, bf                   'effacer.
locate 10, 43: print "Ce graphique reproduit le faisceau lumineux d'un laser ou"
locate 12, 43: print "d'un st‚nop‚. Veuillez attendre qu'il soit achev‚."
locate 14, 43: print "Utilisez ensuite la souris pour d‚placer le curseur."
locate 16, 43: print "Le graphique inf‚rieur montre une section de ce faisceau."
nombre = 200                                              'nombre de cercles concentriques.
pas = 2 * rayon / nombre                                  'espace entre les cercles.
AB = rayon                                                'côtés d'un triangle non rectangle.
BC = pas / 2                                              'rayon d'un petit cercle initial.
normaliser = 2.5 * rayon / lambda ^ 2
for x = 0 to longueur                                     'éviter zéro.
  xDistance = x / lambda / zoomHorizontal                 'distance x, en longueurs d'onde.
  xCarre = xDistance ^ 2                                  'distance au carré, horizontalement.
  differenceDeMarche = xDistance - int(xDistance)         'maximum: une longueur d'onde.
  phase1 = 2 * pi * differenceDeMarche                    'période de référence sur l'axe.
  for y = 0 to hauteur / 2                                'symétrie: une moitié suffit.
    AC = y
    amplitudeSinus = 0: amplitudeCosinus = 0              'initialiser à chaque pixel.
    BC = pas / 2                                          'petit cercle initial.
    do
      angle = acos((AC ^ 2 + BC ^ 2 - AB ^ 2) / (2 * AC * BC))'loi des cosinus (théorème d'AL Kashi).
      if AC > AB + BC then
        arc = 0                                           'cercle entièrement à l'extérieur.
        goto Raccourci
      elseif AC < AB - BC then
        arc = BC * pi                                     'cercle entièrement à l'intérieur.
      else
        arc = BC * angle                                  'arc de cercle dans la source.
      end if
      yDistance = BC / lambda                             'dist. transv. en longueurs d'onde.
      distance = sqr(xCarre + yDistance ^ 2)              'distance de l'arc au pixel.
      differenceDeMarche = distance - int(Distance)
      phase = 2 * pi * differenceDeMarche                 'convertir la distance en période.
      if distance < .5 then distance = .5                 'éviter la division par zéro.
      affaiblissement = arc * (1 / distance)              'la référence 1 lambda est arbitraire.
      amplitudeSinus = amplitudeSinus + affaiblissement * sin(phase)'sommation des ondelettes.
      amplitudeCosinus = amplitudeCosinus + affaiblissement * cos(phase)'Cos pour quadrature.
Raccourci:
      BC = BC + pas                                       'rayon de cercles successifs.
    loop while BC < AC + AB
    periode(x, y) = atn(amplitudeSinus / amplitudeCosinus)          'période absolue.
    if amplitudeCosinus < 0 then periode(x, y) = periode(x, y) + pi 'valider les 4 quadrants.
    luminance(x, y) = normaliser * sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
    ton = luminance(x, y)
    if ton > 255 then ton = 255
    gris(x, y) = rgb(ton, ton, ton)                       'échelle de gris: énergie.
    rouge = 100 * (sin(periode(x, y)) + 1)
    vert = 100 * (sin(periode(x, y) + pi) + 1)
    rouge = rouge + (luminance(x, y) - 155)
    vert = vert + (luminance(x, y) - 155)
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    bleu = (rouge + vert) / 2
    if lambda * zoomHorizontal < 6 then couleur(x, y) = gris(x, y) else couleur(x, y) = rgb(rouge, vert, bleu)
    if mode$ = "gris" or mode$ = "axial" then
      pset(x, yCentre - y), gris(x, y)                    'diagramme en échelle de gris.
      pset(x, yCentre + y), gris(x, y)
    elseif mode$ = "couleurs" then
      pset(x, yCentre - y), couleur(x, y)
      pset(x, yCentre + y), couleur(x, y)
    end if
    if y = 0 then                                         'rayonnement axial.
      periodeAxe(x) = phase1 - periode(x, y)              'fluctuations dans la période.
      amplitudeAxe(x) = .15 * luminance(x, y)
    end if
  next
  Saisie$ = inkey: if len(Saisie$) then return
  getmouse xSouris, ySouris, , clic
  if clic = 1 then return
next
sourceCirc = 1                                            'indicateur: calcul circulaire fait.
return

SourceLineaire:
pcopy 2, page1
screenset page1, page1
line(0, 0)-(longueur, hauteur),fond, bf                   'effacer.
locate 10, 65: print "Ce graphique montre le rayonnement"
locate 11, 65: print "radial et omnidirectionnel d'une"
locate 12, 65: print "source lin‚aire, par exemple un"
locate 13, 65: print "alignement d'‚metteurs synchronis‚s."
nombre = sqr(400 * longueurSource / lambda)               'nombre d'ondelettes.
pas = longueurSource / nombre                             'espace entre les ondelettes.
normaliser = 130 * (pas / lambda) ^ 2                     'uniformiser l'amplitude.
for x = 0 to longueur                                     'éviter zéro.
  xDistance = x / lambda / zoomHorizontal                 'distance x, en longueurs d'onde.
  xCarre = xDistance ^ 2                                  'distance au carré, horizontalement.
  differenceDeMarche = xDistance - int(xDistance)         'maximum: une longueur d'onde.
  phase1 = 2 * pi * differenceDeMarche                    'période de référence sur l'axe.
  for y = 0 to hauteur / 2                                'symétrie: une moitié suffit.
    amplitudeSinus = 0: amplitudeCosinus = 0              'initialiser à chaque pixel.
    yCoord = longueurSource / 2 - y                       'repère pour situer les ondelettes.
    for ondelette = pas / 2 to longueurSource step pas    'selon le nombre d'ondelettes.
      yDistance = (ondelette - yCoord) / lambda           'dist. verticale en longueurs d'onde.
      distance = sqr(xCarre + yDistance ^ 2)              'distance de la source au pixel.
      differenceDeMarche = distance - int(distance)       'une longueur d'onde au maximum.
      phase = 2 * pi * differenceDeMarche                 'convertir la distance en période.
      if distance < .5 then distance = .5                 'éviter la division par zéro.
      affaiblissement = sqr(1 / distance)                 'la référence 1 est arbitraire.
      amplitudeSinus = amplitudeSinus + affaiblissement * sin(phase)'sommation des ondelettes.
      amplitudeCosinus = amplitudeCosinus + affaiblissement * cos(phase)'Cos pour quadrature.      
    next
    periode(x, y) = atn(amplitudeSinus / amplitudeCosinus)          'période absolue.
    if amplitudeCosinus < 0 then periode(x, y) = periode(x, y) + pi 'valider les 4 quadrants.
    luminance(x, y) = normaliser * (amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
    ton = luminance(x, y)
    if ton > 255 then ton = 255
    gris(x, y) = rgb(ton, ton, ton)                       'échelle de gris: énergie.
    rouge = 100 * (sin(periode(x, y)) + 1)
    vert = 100 * (sin(periode(x, y) + pi) + 1)
    rouge = rouge + (luminance(x, y) - 155)
    vert = vert + (luminance(x, y) - 155)
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    bleu = (rouge + vert) / 2
    if lambda * zoomHorizontal < 6 then couleur(x, y) = gris(x, y) else couleur(x, y) = rgb(rouge, vert, bleu)
    if mode$ = "gris" or mode$ = "axial" then
      pset(x, yCentre - y), gris(x, y)                    'diagramme en échelle de gris.
      pset(x, yCentre + y), gris(x, y)
    elseif mode$ = "couleurs" then
      pset(x, yCentre - y), couleur(x, y)
      pset(x, yCentre + y), couleur(x, y)
    end if
    if y = 0 then                                         'rayonnement axial.
      periodeAxe(x) = phase1 - periode(x, y)              'fluctuations dans la période.
      amplitudeAxe(x) = .3 * luminance(x, y)
    end if
  next
  Saisie$ = inkey: if len(Saisie$) then return
  getmouse xSouris, ySouris, , clic
  if clic = 1 then return
next
sourceLin = 1                                             'indicateur: calcul linéaire fait.
return
