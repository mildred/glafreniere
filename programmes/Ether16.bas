page1 = 1: hauteur = 300: longueur = 799
dim as single periodeAxe(longueur), amplitudeAxe(longueur)
dim as single periode(longueur, -hauteur to hauteur), gris(longueur, -hauteur to hauteur)
dim as single couleur(longueur, -hauteur to hauteur), luminance(longueur, -hauteur to hauteur)
dim as single xCarre, yCarre, phase1, phase2, distance, xDistance, yDistance
dim as single pi, zoomHorizontal, lambda, lambdaPrec, lambdaSurDeux, normaliser
dim as single amplitudeSinus, amplitudeCosinus, differenceDeMarche, affaiblissement
dim as single xCoord, yCoord, rotation, amplitude, phase, ton, pas, rayon, rayonArc
dim as single angle, angleSource, anglePixel, angleBalayage, angleArc, ouvertureRelative
dim as single ondelette, facteur, rapport, luminosite, rouge, vert, bleu, petitRayon
dim as single AB, AC, AE, AF, BC, BD, BE, BG, CD, CF, CJ, CK, DP, GJ, GL, JK, grandRayon
dim as single xA, yA, xB, yB, xC, yC, xD, yD, xE, yE, xF, yF, BCA, arc, angleDebut, angleFin
screen 19,24,3: gosub Initialisation

do'<<<<<<<<<<<<<<<<<<<<<< L'ELLIPSOÏDE D'AIRY - CALCUL SELON HUYGENS >>>>>>>>>>>>>>>>>>>>>>>>>
  
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
      case "A": mode$ = "axial"
      case "B": mode$ = "composite"
      case "C": mode$ = "couleurs"
      case "D": mode$ = "courbe"
      case "E": mode$ = "energie"
      case "G": mode$ = "amplitude"
      case "I": gosub Initialisation
      case "M": run "Ether00.exe"
      case "k+", "X+", chr$(27): end
      case "K+":run "Ether15.exe"                         'flèche gauche.
      case "M+":run "Ether17.exe"                         'flèche droite.
    end select
    gosub MiseAjour
  end if
'---------------------------------------- SAISIE SOURIS --------------------------------------

  if clic = 1 then else getmouse xSouris, ySouris, , clic
  ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
  if ligne > 23 and ligne < 38 then
    if xSouris < 304 or xSouris > 496 then ligne = 0
  else ligne = 0  
  end if
'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 39
  select case ligne
    case 26: print effacer$: locate 26, 39
      print " Longueur d'onde..."; lambda
    case 27: print effacer$: locate 27, 39
      print " Angle d'ouverture."; angleOuverture; chr(248)
    case 24: print effacer$: locate 24, 39
      print " Luminosit‚....... ";: print using "#.##"; luminosite
    case 29: if mode$ = "courbe"    then else print ligne29$
    case 30: if mode$ = "energie"   then else print ligne30$
    case 31: if mode$ = "amplitude" then else print ligne31$
    case 32: if mode$ = "composite" then else print ligne32$
    case 33: if mode$ = "couleurs"  then else print ligne33$
    case 34: if mode$ = "axial"     then else print ligne34$
    case 35: print ligne35$
    case 36: print ligne36$
    case 37: print effacer$;: if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
  if clic = 1 then
    if ySouris < 300 and xSouris > 0 then planDeCoupe = xSouris
    bitmap = 0: image = 0
    select case ligne
      case 24: gosub CurseurLuminosite
      case 29: mode$ = "courbe":    gosub MiseAJour
      case 30: mode$ = "energie":   gosub MiseAJour
      case 31: mode$ = "amplitude": gosub MiseAJour
      case 32: mode$ = "composite": gosub MiseAJour
      case 33: mode$ = "couleurs":  gosub MiseAJour
      case 34: mode$ = "axial":     gosub MiseAJour
      case 26: gosub CurseurLambda
      case 27: gosub CurseurAngle
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether15.exe" else run "Ether17.exe"
    end select
  end if
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
loop

Afficher:'------------------------- AFFICHER LES GRAPHIQUES ----------------------------------
if calculFait = 0 then gosub SourceConcave
plan = abs(planDeCoupe - 400)
for x = 0 to hauteur / 2                                  'graphiques du plan de coupe.
  xCarre = x ^ 2
  for y = 0 to hauteur / 2 step 2                         'graphique elliptique.
    yDist = sqr(y ^ 2 + xCarre)
    if yDist <= hauteur / 2 then
      ton = luminosite * luminance(plan,yDist)
      if ton > 255 then ton = 255
      pset(hauteur / 2 + x, 520 - y / 2), rgb(ton,ton,ton)
      pset(hauteur / 2 - x, 520 - y / 2), rgb(ton,ton,ton)
      pset(hauteur / 2 + x, 520 + y / 2), rgb(ton,ton,ton)
      pset(hauteur / 2 - x, 520 + y / 2), rgb(ton,ton,ton)
    end if
  next
next
yPrec = 436 - .22 * luminance(plan, 0)                    'graphique 2-D.
for xPoint = 0 to hauteur / 2
  ton = luminosite * luminance(plan, xPoint)
  if ton > 255 then ton = 255
  yCoord = 436 - .22 * luminance(plan, xPoint)
  line(hauteur / 2 + xPoint, yCoord)-(hauteur / 2 + xPoint, 436), blanc
  line(hauteur / 2 - xPoint, yCoord)-(hauteur / 2 - xPoint, 436), blanc
  line(hauteur / 2 + xPoint, yPrec)-(hauteur / 2 + xPoint, yCoord), noir
  line(hauteur / 2 - xPoint, yPrec)-(hauteur / 2 - xPoint, yCoord), noir
  yPrec = yCoord
next
line(0,437)-(hauteur, 437), noir

'********************************** GRAPHIQUE PRINCIPAL **************************************
image = image + 1
if image = images then image = 0
phase = 2 * pi * image / images
select case mode$
  case "courbe":    gosub Courbe                          'courbe de l'énergie.
  case "couleurs":  gosub Couleurs                        'ondes en couleurs.
  case "composite": gosub Composite                       'ondes plus énergie.
  case "amplitude": gosub Gris                            'ondes en échelle de gris.
  case "energie" :  gosub Energie                         'graphique d'énergie en gris.
  case "axial"   :  gosub Axial                           'graphique axial.
end select
return

Axial:'------------------------------- GRAPHIQUE AXIAL ---------------------------------------
locate 1, 2: print "Le graphique axial montre que les phases (repr‚sent‚es en rouge) n'‚voluent pas r‚guliŠrement"
locate 2, 2: print "le long de l'axe. Les ondes circulent plus vite que la lumiŠre au centre de chaque lobe … cause"
locate 3, 2: print "d'un effet de ciseau."
locate 17,2: print "Pour cette raison, leur longueur d'onde est agrandie de maniŠre … ce que leur fr‚quence demeure"
locate 18,2: print "constante. Il en r‚sulte une inversion de phase (un saut d'une demi-p‚riode) entre chaque lobe."
amplitude = .15 * luminance(0, 0)
yPhasePrec1 = yCentre + amplitude * sin(periodeAxe(0) - phase)
yPhasePrec2 = yCentre + amplitude * sin(periodeAxe(0) + phase)
yPrecedent = amplitude
yPrecedent1 = amplitude * sin(periode(0, 0) - phase)
yPrecedent2 = amplitude * sin(periode(0, 0) + phase)
for x = 0 to longueur / 2
  amplitudeSinus = sin(periodeAxe(x) + phase - pi / 2)
  amplitudeCosinus = cos(periodeAxe(x) - phase)
  yPhase1 = yCentre + amplitude * sin(periodeAxe(x) - phase)
  yPhase2 = yCentre + amplitude * sin(periodeAxe(x) + phase)
  if amplitudeCosinus > 0 then line(xCentre - x, yPhasePrec1)-(xCentre - x, yPhase1), rgb(255,100,100)
  if amplitudeSinus > 0 then line(xCentre + x, yPhasePrec2)-(xCentre + x, yPhase2), rgb(255,100,100)
  yCoord = .15 * luminance(x, 0)
  line(xCentre + x, yCentre + yCoord)-(xCentre + x, yCentre - yCoord), blanc
  line(xCentre - x, yCentre + yCoord)-(xCentre - x, yCentre - yCoord), blanc
  line(xCentre + x, yCentre + yPrecedent)-(xCentre + x, yCentre + yCoord), noir'courbe d'amplitude.
  line(xCentre + x, yCentre - yPrecedent)-(xCentre + x, yCentre - yCoord), noir
  line(xCentre - x, yCentre + yPrecedent)-(xCentre - x, yCentre + yCoord), noir
  line(xCentre - x, yCentre - yPrecedent)-(xCentre - x, yCentre - yCoord), noir
  yOndeAxiale1 = .15 * luminance(x, 0) * sin(periode(x, 0) - phase)
  yOndeAxiale2 = .15 * luminance(x, 0) * sin(periode(x, 0) + phase)  'symétrie.
  line(xCentre + x, yCentre + yPrecedent1)-(xCentre + x, yCentre + yOndeAxiale1),noir
  line(xCentre - x, yCentre + yPrecedent2)-(xCentre - x, yCentre + yOndeAxiale2),noir
  if amplitudeCosinus < 0 then line(xCentre - x, yPhasePrec1)-(xCentre - x, yPhase1), rgb(255,100,100)
  if amplitudeSinus < 0 then line(xCentre + x, yPhasePrec2)-(xCentre + x, yPhase2), rgb(255,100,100)
  yPhasePrec1 = yPhase1
  yPhasePrec2 = yPhase2
  yPrecedent = yCoord
  yPrecedent1 = yOndeAxiale1
  yPrecedent2 = yOndeAxiale2
next
  line(planDeCoupe, 50)-(planDeCoupe, hauteur - 50), noir
return

Bitmaps:'------------------------ CRÉER UNE SÉQUENCE BITMAP ----------------------------------
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

Composite:'---------------- ONDES QUI TIENNENT COMPTE DE L'ÉNERGIE ---------------------------
for x = 0 to longueur / 2
  for y = 0 to hauteur / 2
    ton = 2 * luminosite * luminance(x, y) * sin(periode(x, y) + phase)
    if ton > 255 then ton = 255 else if ton < 0 then ton = 0
    pset(xCentre - x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre - x, yCentre + y), rgb(ton, ton, ton)
    ton = 2 * luminosite * luminance(x, y) * sin(periode(x, y) - phase)
    if ton > 255 then ton = 255 else if ton < 0 then ton = 0
    pset(xCentre + x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre + x, yCentre + y), rgb(ton, ton, ton)
  next
next
line(planDeCoupe, 0)-(planDeCoupe, hauteur), blanc
line(xCentre,0)-(xCentre,10), blanc                       'repères, centre et longueur d'onde.
line(xCentre - lambda / 2,0)-(xCentre - lambda / 2,5), blanc
line(xCentre + lambda / 2,0)-(xCentre + lambda / 2,5), blanc
return

Couleurs:'-------------------------- ONDES EN COULEURS ---------------------------------------
for x = 0 to longueur / 2
  for y = 0 to hauteur / 2
    rouge = luminosite * 25 * (sin(periode(x, y) + phase) + 1)
    vert  = luminosite * 25 * (sin(periode(x, y) + phase + pi) + 1)
    rouge = rouge + rouge * .03 * luminance(x, y)
    vert = vert + vert * .03 * luminance(x, y)
    if rouge > 255 then vert = vert + (rouge - 255)
    if vert > 255 then rouge = rouge + (vert - 255)
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    bleu = (rouge + vert) / 2
    pset(xCentre - x, yCentre - y), rgb(rouge, vert, bleu)
    pset(xCentre - x, yCentre + y), rgb(rouge, vert, bleu)
    rouge = luminosite * 25 * (sin(periode(x, y) - phase) + 1)
    vert  = luminosite * 25 * (sin(periode(x, y) - phase + pi) + 1)
    rouge = rouge + rouge * .03 * luminance(x, y)
    vert = vert + vert * .03 * luminance(x, y)
    if rouge > 255 then vert = vert + (rouge - 255)
    if vert > 255 then rouge = rouge + (vert - 255)
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    bleu = (rouge + vert) / 2
    pset(xCentre + x, yCentre - y), rgb(rouge, vert, bleu)
    pset(xCentre + x, yCentre + y), rgb(rouge, vert, bleu)
  next
next
line(planDeCoupe, 0)-(planDeCoupe, hauteur), blanc
line(xCentre,0)-(xCentre,10), blanc                       'repères, centre et longueur d'onde.
line(xCentre - lambda / 2,0)-(xCentre - lambda / 2,5), blanc
line(xCentre + lambda / 2,0)-(xCentre + lambda / 2,5), blanc
return

Courbe:
line(planDeCoupe, 0)-(planDeCoupe, 50), noir
line(planDeCoupe, hauteur - 50)-(planDeCoupe, hauteur), noir
x = abs(planDeCoupe - 400)
for y = 0 to hauteur / 2
  energie = .0022 * luminance(x,y) ^ 2
  if y = 0 then energiePrecedente = energie
  line(4, yCentre + y)-(4 + energie, yCentre + y), blanc  'graphique de l'énergie.
  line(4, yCentre - y)-(4 + energie, yCentre - y), blanc
  line(4 + energiePrecedente, yCentre + y)-(4 + energie, yCentre + y), noir
  line(4 + energiePrecedente, yCentre - y)-(4 + energie, yCentre - y), noir
  energiePrecedente = energie
next
line(3,0)-(3, hauteur), noir                              'souligner le graphique.
line(0,437)-(hauteur, 437), noir
return

CurseurAngle:'-------------- MODIFICATION DE L'ANGLE D'OUVERTURE -----------------------------
screenset 2
color noir, fond
anglePrecedent = angleOuverture
minimum = 400 - 180
maximum = 400 + 180 
do
  swap page1, page2
  screenset page1, page2
  cls
  if ySouris > 386 and ySouris < 466 then 
    angleOuverture = (xSouris - minimum) / 2
    else angleOuverture = anglePrecedent
  end if
  if angleOuverture < 10 then angleOuverture = 10 else if angleOuverture > 180 then angleOuverture = 180
  angleSource = angleOuverture / (180 / pi)
  curseur = minimum + 2 * angleOuverture
  line(minimum + 19, 415)-(maximum, 431), noir, b
  line(curseur - 1,415)-(curseur + 1, 431), noir, bf
  locate 24, 33: print " Angle d'ouverture:"; angleOuverture; chr(248);
  print " (";
  print 2 * angleOuverture;: print chr(248); " bilat‚ral)."
  if angleOuverture > 89 then
    ouvertureRelative = 0
  else ouvertureRelative = .5 / tan(angleSource)
  end if
  locate 25, 39: print " Ouverture relative: Ÿ /";
  print using "###.##"; ouvertureRelative                 'surveiller anomalie du compilateur.
  y = 194
  x1 = 400 - 150 * cos(angleSource)
  y1 = y - 150 * sin(angleSource)
  y2 = y + 150 * sin(angleSource)
  circle(400, y), 150, noir
  paint(300, y), noir
  circle(400, y), 150, 1
  line(x1, y1)-(400, y), 1
  line(x1, y2)-(400, y), 1
  paint(300, y), jaune, 1
  line(400 - 150, y)-(400, y), 1
  getmouse xSouris, ySouris, , clic
loop while clic = 1
if anglePrecedent = angleOuverture then else gosub MiseAJour: calculFait = 0
return

CurseurLambda:'-------------- MODIFICATION DE LA LONGUEUR D'ONDE -----------------------------
lambdaPrec = lambda                                       'lambda: minimum 10, maximum 100.
minimum = 400 - 200 + 40                                  'centre à 400, 4 pixels par longueur d'onde.
maximum = 400 + 200                                       'centre à 50 pixels, extrêmes selon 4 * 50.
do
  swap page1, page2
  screenset page1, page2
  cls
  curseur = minimum + 4 * lambda - 40
'  If curseur < minimum Then curseur = minimum Else If curseur > maximum Then curseur = maximum
  line(minimum - 2,399)-(maximum + 2, 415), noir, b
  line(curseur - 1,399)-(curseur + 1, 415), noir, bf
  line(400 - lambda / 2,374)-(400 + lambda / 2, 391), noir, bf
  rayon = int(longueurSource / 2)
  locate 20, 18: print "La structure de l'ellipso‹de d'Airy ne d‚pend que de l'angle d'ouverture."
  locate 21, 20: print "En cons‚quence, agrandir la longueur d'onde ‚quivaut … faire un zoom."
  locate 23, 33: print "Longueur d'onde (lambda):"; lambda; " pixels."
  if ySouris > 370 and ySouris < 450 then 
    lambda = int((xSouris - 200) / 4)
  else lambda = lambdaPrec
  end if
  if lambda > 100 then lambda = 100 else if lambda < 10 then lambda = 10
  getmouse xSouris, ySouris, , clic
loop while clic = 1
if lambdaPrec = lambda then else gosub MiseAJour: calculFait = 0
return

CurseurLuminosite:'------------ MODIFICATION DE LA LUMINOSITÉ --------------------------------
locate 23, 39: print effacer$
locate 25, 39: print effacer$
color noir, turquoise
if xSouris > 400 then
  luminosite = 1 + (xSouris - 400) / 10
else luminosite = 1 / (1 + (400 - xSouris) / 10)
end if
if luminosite < .1 then luminosite = .1 else if luminosite > 9.99 then luminosite = 9.99
line(xSouris - 1, 352)-(xSouris + 1, 399), noir, bf
line(400 - 1, 352)-(400 + 1, 399), noir, bf
locate 24, 39: print " Luminosit‚....... ";: print using "#.## "; luminosite
line(400 - 96, 367)-(400 + 97, 383), noir, b
screenset 2, 2: color noir, fond
locate 24, 39: print " Luminosit‚....... ";: print using "#.## "; luminosite
return

'------------------------- GRAPHIQUE D'ÉNERGIE EN ÉCHELLE DE GRIS ----------------------------
Energie:
for x = 0 to longueur / 2
  for y = 0 to hauteur / 2
    ton = luminosite * luminance(x, y)
    if ton > 255 then ton = 255
    pset(xCentre + x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre + x, yCentre + y), rgb(ton, ton, ton)
    pset(xCentre - x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre - x, yCentre + y), rgb(ton, ton, ton)
  next
next
line(planDeCoupe, 0)-(planDeCoupe, hauteur), blanc        'curseur du plan de coupe.
if planDeCoupe = 400 then                                 'souligner les dimensions de l'ellipsoïde.
  line(400 - 10, yCentre - petitRayon)-(400 + 10, yCentre - petitRayon), rgb(0,255,0)
  line(400 - 10, yCentre + petitRayon)-(400 + 10, yCentre + petitRayon), rgb(0,255,0)
  circle(400e, yCentre), grandRayon, rgb(0,255,0),,, petitRayon / grandRayon
  line(400 - 10, yCentre - lambda / 2)-(400 + 10, yCentre - lambda / 2), rgb(255,100,100)
  line(400 - 10, yCentre + lambda / 2)-(400 + 10, yCentre + lambda / 2), rgb(255,100,100)
end if
line(xCentre,0)-(xCentre, 10), blanc                      'repères, centre et longueur d'onde.
line(xCentre,hauteur - 10)-(xCentre, hauteur), blanc
line(xCentre - lambda / 2,0)-(xCentre - lambda / 2,5), rgb(255,100,100)
line(xCentre + lambda / 2,0)-(xCentre + lambda / 2,5), rgb(255,100,100)
line(xCentre - lambda / 2,hauteur - 5)-(xCentre - lambda / 2, hauteur), rgb(255,100,100)
line(xCentre + lambda / 2,hauteur - 5)-(xCentre + lambda / 2, hauteur), rgb(255,100,100)
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

Gris:'---------------------------- ONDES EN ÉCHELLE DE GRIS ----------------------------------
for x = 0 to longueur / 2
  for y = 0 to hauteur / 2
    ton = luminosite * luminance(x, y) * sin(periode(x, y) + phase) + 128'gris moyen à 128.
    if ton > 255 then ton = 255 else if ton < 0 then ton = 0
    pset(xCentre - x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre - x, yCentre + y), rgb(ton, ton, ton)
    ton = luminosite * luminance(x, y) * sin(periode(x, y) - phase) + 128
    if ton > 255 then ton = 255 else if ton < 0 then ton = 0
    pset(xCentre + x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre + x, yCentre + y), rgb(ton, ton, ton)
  next
next
line(planDeCoupe, 0)-(planDeCoupe, hauteur), blanc
line(xCentre,0)-(xCentre,10), blanc                       'repères, centre et longueur d'onde.
line(xCentre - lambda / 2,0)-(xCentre - lambda / 2,5), blanc
line(xCentre + lambda / 2,0)-(xCentre + lambda / 2,5), blanc
return

'#############################################################################################
Initialisation:'------------------------ INITIALISATION --------------------------------------
'#############################################################################################
fond = rgb(225,225,225)
blanc = rgb(255,255,255)
jaune = rgb(255,255,200)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               'séquence bitmap si désiré.
'mode$ = "energie"
mode$ = "couleurs"
calculFait = 0                                            'calcul à faire ou à refaire.
luminosite = 1
angleOuverture = 45                                       'angle du cône lumineux.
lambda = 30                                               'longueur d'onde.
images = 32
nombre = 140                                              'nombre de cercles concentriques.
xCentre = 400                                             'symétrie au centre de l'écran.
yCentre = hauteur / 2
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
ligne29$ = " D - Courbe d'‚nergie.  ": locate 29, 39: print ligne29$
ligne30$ = " E - Energie seulement. ": locate 30, 39: print ligne30$
ligne31$ = " G - Ondes (amplitude). ": locate 31, 39: print ligne31$
ligne32$ = " B - Ondes (composite). ": locate 32, 39: print ligne32$
ligne33$ = " C - Ondes en couleurs. ": locate 33, 39: print ligne33$
ligne34$ = " A - Rayonnement axial. ": locate 34, 39: print ligne34$
ligne35$ = " I - Initialiser.       ": locate 35, 39: print ligne35$
ligne36$ = "     Quitter (Echap).   ": locate 36, 39: print ligne36$
effacer$ = "                        "
locate 21
locate, 64: print "Ce programme fait appel au principe"
locate, 64: print "de Huygens, comme le pr‚c‚dent."
print
locate, 64: print "Il montre que la tache d'Airy n'est"
locate, 64: print "pas qu'un disque entour‚ d'anneaux."
locate, 64: print "Le foyer est en fait un ellipsoide"
locate, 64: print "de r‚volution allong‚ dont le grand"
locate, 64: print "rayon d‚pend de l'angle d'ouverture,"
locate, 64: print "tout comme le petit."
print
locate, 64: print "On constate que le chiffre d'Airy,"
locate, 64: print "soit 1,22, devient vite inexact si"
locate, 64: print "l'angle d'ouverture augmente."
locate 31, 6: print "Calcul en cours..."
locate 33, 6: print "Veuillez attendre que le"
locate 34, 6: print "diagramme soit achev‚."

gosub flecheGauche: gosub flecheDroite
dim titre(178 * 22)
color blanc, rgb(255,0,0)
line(311, 300)-(488, 321), noir,B
line(312, 301)-(487, 320), rgb(255,0,0),BF
line(312, 301)-(487, 320), blanc,B
locate 20, 41: print "L'ELLIPSOIDE  D'AIRY"
get(311,300)-(488,321), titre
line(311, 300)-(488, 321), fond,BF
put(311,307), titre, pset
color rgb(0,150,0), fond
locate 35, 63: print "Le 12 mai 2006. Ce programme peut ˆtre";
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
Saisie$ = ""
lambdaSurDeux = lambda / 2
planDeCoupe = 400
angleSource = angleOuverture / (180 / pi)
if mode$ = "axial" then images = 100 else images = 24
if angleOuverture > 90 then
  petitRayon = lambda / 2
  grandRayon = lambda * (1 + .5 * cos(angleSource))
else
  petitRayon = .5 * lambda * (1.22 - .22 * sin(angleSource)) / sin(angleSource)
  grandRayon = 2 * lambda * (1 / sin(angleSource)) ^ 2 - lambda * sin(angleSource)
  
end if
locate 22, 40: print "Petit rayon......";
print using" #.##"; petitRayon / lambda
locate 23, 40: print "Grand rayon......";
if grandRayon / lambda > 9.995 then
  print using" ##.#"; grandRayon / lambda
else
  print using" #.##"; grandRayon / lambda
end if
locate 26, 39: print effacer$
locate 26, 39: print " Longueur d'onde..."; lambda
locate 27, 39: print effacer$
locate 27, 39: print " Angle d'ouverture."; angleOuverture; chr(248)
locate 24, 39: print " Luminosit‚....... ";: print using "#.##"; luminosite
locate 29, 39: print ligne29$
locate 30, 39: print ligne30$
locate 31, 39: print ligne31$
locate 32, 39: print ligne32$
locate 33, 39: print ligne33$
locate 34, 39: print ligne34$
color rgb(0,0,255)
select case mode$
  case "courbe":    locate 29, 39: print ligne29$         'choix en cours affichés en bleu.
  case "energie":   locate 30, 39: print ligne30$
  case "amplitude": locate 31, 39: print ligne31$
  case "composite": locate 32, 39: print ligne32$
  case "couleurs":  locate 33, 39: print ligne33$
  case "axial":     locate 34, 39: print ligne34$
end select
color noir, fond
return

SourceConcave:'--------------- CALCUL STANDARD D'UNE SOURCE CONCAVE --------------------------
pcopy 2, page1
screenset page1, page1
'les calculs simplifiés ci-dessous permettent de confirmer le calcul standard:
'if angleOuverture = 180 then gosub SourceSpherique: return
'if angleOuverture = 90 then gosub SourceHemispherique: return

line(0,0)-(799,hauteur), fond, bf                         'effacer le texte.
pas = angleSource / nombre                                'espace entre les arcs de cercle.
JK = sin(angleSource)                                     'triangle de départ.
CJ = cos(angleSource)
normaliser = 3                                            'valable avec nombre = 100 seulement.
'********************** Calcul simplifié et précis de l'axe seulement ************************
'For x = 0 To longueur / 2                                 'x = 0  au centre d'une sphère.
'  distance = x / lambda
'  differenceDeMarche = distance - Int(distance)           'maximum: une longueur d'onde.
'  phase1 = 2 * pi * -differenceDeMarche                   'période de référence sur l'axe.
'  amplitudeSinus = 0: amplitudeCosinus = 0
'  For angle = pas / 2 To angleSource Step pas
'    differenceDeMarche = distance * cos(angle)
'    phase = 2 * pi * differenceDeMarche
'    amplitudeSinus = amplitudeSinus + Sin(angle) * Sin(phase)' sin(angle) à corriger par pi ci-dessous ( ou faire: pi * sin(angle)).
'    amplitudeCosinus = amplitudeCosinus + Sin(angle) * Cos(phase)
'  next
'  periode(x, 0) = -Atn(amplitudeCosinus / amplitudeSinus) + pi  'période absolue.
'  If amplitudeSinus < 0 Then periode(x, 0) = periode(x, 0) + pi 'valider les 4 quadrants.
'  if x = 0 then normaliser = 600 / Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
'  luminance(x, 0) = pi * normaliser * Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore. Correction par pi.
'  periodeAxe(x) = phase1 - periode(x, 0)                  'fluctuations dans la période.
'  amplitudeAxe(x) = .1 * luminance(x, 0)
'  ton = luminance(x, 0)
'  If ton > 255 Then ton = 255
'  gris(x, 0) = Rgb(ton, ton, ton)
'  line(xCentre - x, yCentre - .1 * luminance(x, 0))-(xCentre - x, yCentre + .1 * luminance(x, 0)), gris(x, 0)
'  line(xCentre + x, yCentre - .1 * luminance(x, 0))-(xCentre + x, yCentre + .1 * luminance(x, 0)), gris(x, 0)
'next

'********************** Calcul simplifié et précis du plan focal seulement ************************
if angleOuverture > 90 then
  angleDebut = pas / 2: angleFin = pi
else angleDebut = pi / 2 - angleSource + pas / 2: angleFin = pi / 2 + angleSource
end if
energiePrecedente = 0
for y = 0 to hauteur / 2
  distance = y / lambda
  amplitudeSinus = 0: amplitudeCosinus = 0
  for angle = angleDebut to angleFin step pas
    AF = sin(angle)                                       'rayon de l'arc de balayage.
    AC = cos(angle)                                       'AC = BC = GJ
    GL = sqr(JK ^ 2 - AC ^ 2)                             'AC = GJ
    angleArc = atn(GL / CJ)                               'CJ = BG; AB = 0
    if CJ < 0 then angleArc = angleArc + pi
    if GL < 0 then angleArc = pi
    arc = AF * angleArc
    differenceDeMarche = distance * cos(angle)
    phase = 2 * pi * differenceDeMarche
    amplitudeSinus =   amplitudeSinus   + arc * sin(phase)
    amplitudeCosinus = amplitudeCosinus + arc * cos(phase)
  next
  periode(0, y) = atn(amplitudeCosinus / amplitudeSinus)
  if amplitudeSinus < 0 then periode(0, y) = periode(0, y) + pi
  if y = 0 then normaliser = 600 / sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
  luminance(0, y) = normaliser * sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
  energie = .0022 * luminance(0,y) ^ 2
  if y = 0 then energiePrecedente = energie
  line(4, yCentre + y)-(4 + energie, yCentre + y), blanc  'graphique de l'énergie.
  line(4, yCentre - y)-(4 + energie, yCentre - y), blanc
  line(4 + energiePrecedente, yCentre + y)-(4 + energie, yCentre + y), noir
  line(4 + energiePrecedente, yCentre - y)-(4 + energie, yCentre - y), noir
  energiePrecedente = energie
  ton = luminosite * luminance(0, y)
  if ton > 255 then ton = 255
  gris(0, y) = rgb(ton, ton, ton)
  pset(xCentre, yCentre - y), gris(0, y)                  'plan focal.
  pset(xCentre, yCentre + y), gris(0, y)
  yCoord = 436 - .22 * luminance(0, y)
  if y = 0 then
    yCoordPrecedent = yCoord
    periodeAxe(0) = periode(0, y) + pi
  end if
  line(hauteur / 2 + y, yCoord)-(hauteur / 2 + y, 436), blanc' graphique du plan focal.
  line(hauteur / 2 - y, yCoord)-(hauteur / 2 - y, 436), blanc
  line(hauteur / 2 + y, yCoordPrecedent)-(hauteur / 2 + y, yCoord), noir
  line(hauteur / 2 - y, yCoordPrecedent)-(hauteur / 2 - y, yCoord), noir
  yCoordPrecedent = yCoord
next
line(3,0)-(3, hauteur), noir                              'souligner les graphiques.
line(0,437)-(hauteur, 437), noir

'********************************* CALCUL ÉLABORÉ STANDARD ***********************************
for x = 1 to longueur / 2                                 'x = 0  au centre d'une sphère.
  xDistance = x  / lambda                                 'horizontale, en longueurs d'onde.
  xCarre = xDistance ^ 2                                  'distance horizontale au carré.
  differenceDeMarche = xDistance - int(xDistance)         'maximum: une longueur d'onde.
  phase1 = 2 * pi * differenceDeMarche                    'période de référence sur l'axe.
  for y = 0 to hauteur / 2                                'symétrie: une moitié suffit.
    amplitudeSinus = 0: amplitudeCosinus = 0              'initialiser à chaque pixel.
    yDistance = y / lambda                                'verticale, en longueurs d'onde.
    distance = sqr(yDistance ^ 2 + xCarre)                'distance du centre selon Pythagore.
    if x then anglePixel = atn(y / x) else anglePixel = pi'vertical: 0°, source à gauche.
    angle = angleSource + anglePixel - pi / 2
    AC = sin(angle)
    angleDebut = acos(AC) + pas / 2
    if y = 0 or angleOuverture > 90 then angleDebut = pas / 2
    for angle = angleDebut to pi step pas                 'distribuer les arcs sur la source.
      AF = sin(angle)                                     'Ce calcul est expliqué dans le
      AC = cos(angle)                                     'programme Ether16_concave.bas.
      BC = AC / sin(anglePixel)                           'Il s'agit de repérer des arcs de
      AB = BC * cos(anglePixel)                           'cercles dont tous les points sont
      BG = CJ / sin(anglePixel)                           'équidistants du pixel considéré,
      GJ = BG * cos(anglePixel) + BC                      'et de mesurer leur longueur dans
      if GJ > JK then GJ = JK                             'le but d'en faire la sommation. 
      if GJ < -JK then GJ = -JK
      GL = sqr(JK ^ 2 - GJ ^ 2)
      angleArc = atn(GL / (BG + AB))                      'détermine la longueur de l'arc.
      if BG + AB < 0 then angleArc = angleArc + pi        'valider le deuxième quadrant.
      arc = AF * angleArc                                 'longueur de l'arc, but du calcul.
      if arc = 0 and y > 0 and angleOuverture < 91 then exit for'limite atteinte: arc = 0 inutile.
      if y = 0 then arc = pi * sin(angle): if angle > angleSource then exit for'calcul axial précis.
      differenceDeMarche = distance * cos(angle)
      phase = 2 * pi * differenceDeMarche
      amplitudeSinus = amplitudeSinus + arc * sin(phase)  'amplitude proportionnelle à l'arc.
      amplitudeCosinus = amplitudeCosinus + arc * cos(phase)
    next
    periode(x, y) = atn(amplitudeCosinus / amplitudeSinus)'période absolue.
    luminance(x, y) = normaliser * sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
    if amplitudeSinus < 0 then periode(x, y) = periode(x, y) + pi 'valider les 4 quadrants.
    if y = 0 then
      periode(x, y)= -periode(x, y) + pi                  'le calcul axial inverse la période.
      amplitudeAxe(x) = luminance(x, y)
      periodeAxe(x) = phase1 - periode(x, y)              'fluctuations dans la période.
    end if
    ton = luminosite * luminance(x, y)
    if ton > 255 then ton = 255
    pset(xCentre + x, yCentre - y), rgb(ton, ton, ton)    'diagramme en échelle de gris.
    pset(xCentre + x, yCentre + y), rgb(ton, ton, ton)
    pset(xCentre - x, yCentre - y), rgb(ton, ton, ton)
    pset(xCentre - x, yCentre + y), rgb(ton, ton, ton)
  next
  Saisie$ = inkey
  if len(Saisie$) then return
  getmouse xSouris, ySouris, , clic
  if clic = 1 then return
next
calculFait = 1                                            'indicateur: calcul terminé.
return

SourceHemispherique:'----- CALCUL SIMPLIFIÉ D'UNE SOURCE HÉMISPHÉRIQUE -----------------------
pcopy 2, page1
screenset page1, page1
angleSource = angleOuverture / (180 / pi)
pas = angleSource / nombre                                'espace entre les arcs de cercle.
for x = 0 to longueur / 2                                 'x = 0  au centre d'une sphère.
  xDistance = x  / lambda                                 'horizontale, en longueurs d'onde.
  xCarre = xDistance ^ 2                                  'distance horizontale au carré.
  differenceDeMarche = xDistance - int(xDistance)         'maximum: une longueur d'onde.
  phase1 = 2 * pi * differenceDeMarche                    'période de référence sur l'axe.
  for y = 0 to hauteur / 2                                'symétrie: une moitié suffit.
    amplitudeSinus = 0: amplitudeCosinus = 0              'initialiser à chaque pixel.
    yDistance = y / lambda                                'verticale, en longueurs d'onde.
    distance = sqr(yDistance ^ 2 + xCarre)                'distance du centre selon Pythagore.
    if y then anglePixel = atn(x / y) else anglePixel = 0 'vertical: 0°, source à gauche.
    anglePixel = anglePixel + pi / 2                      'adapter l'angle au shéma d'origine.
    for angle = pas / 2 to pi step pas                    '200 arcs distribués sur 180°.
      AC = cos(angle)                                     'Explications dans Ether16_hemisphere.bas
      if y = 0 then
        if angle > pi / 2 then BC = -1 else BC = 1
      else
        BC = AC / sin(anglePixel)
      end if
      if BC > 1 then BC = 1 else if BC < -1 then BC = -1 
      AB = BC * cos(anglePixel)
      BE = sqr(1 - BC ^ 2)
      if AB then angleArc = atn(BE / AB) else angleArc = pi / 2
      if AB < 0 then angleArc = angleArc + pi             'valider le deuxième quadrant.
      rayonArc = sin(angle)
      if rayonArc < 0 then rayonArc = 0
      arc = rayonArc * angleArc
      differenceDeMarche = distance * cos(angle)
      phase = 2 * pi * differenceDeMarche
      amplitudeSinus = amplitudeSinus + arc * sin(phase)  'amplitude proportionnelle à l'arc.
      amplitudeCosinus = amplitudeCosinus + arc * cos(phase)
    next
    periode(x, y) = atn(amplitudeSinus / amplitudeCosinus)          'période absolue.
    if amplitudeCosinus < 0 then periode(x, y) = periode(x, y) + pi 'valider les 4 quadrants.
'    if x = 0 and y = 0 then normaliser = Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus) / 30
    luminance(x, y) = normaliser * sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
    ton = luminance(x, y)
    if ton > 255 then ton = 255
    gris(x, y) = rgb(ton, ton, ton)                       'échelle de gris: énergie.
    rouge = 100 * (cos(periode(x, y)) + 1)                'quadrature: éviter l'amplitude à zéro.
    vert = 100 * (cos(periode(x, y) + pi) + 1)
    rouge = rouge + (luminance(x, y) - 155)
    vert = vert + (luminance(x, y) - 155)
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    bleu = (rouge + vert) / 2
    if lambda < 6 then couleur(x, y) = gris(x, y) else couleur(x, y) = rgb(rouge, vert, bleu)
    if mode$ = "energie" or mode$ = "axial" then
      pset(xCentre + x, yCentre - y), gris(x, y)                    'diagramme en échelle de gris.
      pset(xCentre + x, yCentre + y), gris(x, y)
      pset(xCentre - x, yCentre - y), gris(x, y)
      pset(xCentre - x, yCentre + y), gris(x, y)
    elseif mode$ = "couleurs" then
      pset(xCentre + x, yCentre - y), couleur(x, y)
      pset(xCentre + x, yCentre + y), couleur(x, y)
      pset(xCentre - x, yCentre - y), couleur(x, y)
      pset(xCentre - x, yCentre + y), couleur(x, y)
    end if
    if y = 0 then                                         'rayonnement axial.
      periodeAxe(x) = phase1 - periode(x, y)              'fluctuations dans la période.
      amplitudeAxe(x) = .1 * luminance(x, y)
    end if
  next
  Saisie$ = inkey: if len(Saisie$) then return
  getmouse xSouris, ySouris, , clic
  if clic = 1 then return
next
calculFait = 1                                            'indicateur: calcul terminé.
return

SourceSpherique:'----- CALCUL SIMPLIFIÉ D'UNE SOURCE SPHERIQUE COMPLÈTE ----------------------
pcopy 2, page1
screenset page1, page1
line(0, 0)-(longueur, hauteur),fond, bf                   'effacer.
locate 10, 43: print "Si l'angle d'ouverture fait 360° et couvre donc"
locate 12, 43: print "une sphère complète, les ondes deviennent"
locate 14, 43: print "stationnaires et l'on obtient le même diagramme"
locate 16, 43: print "que s'il s'agissait d'un électron."
angleSource = angleOuverture / (180 / pi)
pas = angleSource / nombre                                'espace entre les arcs de cercle.
for x = 0 to longueur / 2                                 'x = 0  au centre d'une sphère.
  xDistance = x  / lambda                                 'horizontale, en longueurs d'onde.
  xCarre = xDistance ^ 2                                  'distance horizontale au carré.
  differenceDeMarche = xDistance - int(xDistance)         'maximum: une longueur d'onde.
  phase1 = 2 * pi * differenceDeMarche                    'période de référence sur l'axe.
  for y = 0 to hauteur / 2                                'symétrie: une moitié suffit.
    amplitudeSinus = 0: amplitudeCosinus = 0              'initialiser à chaque pixel.
    yDistance = y / lambda                                'verticale, en longueurs d'onde.
    distance = sqr(yDistance ^ 2 + xCarre)                'distance selon Pythagore.
    for angle = pas / 2  to pi step pas                   'balayer la sphère sur 180°.
      differenceDeMarche = distance * cos(angle)
      phase = 2 * pi * differenceDeMarche
      amplitudeSinus = amplitudeSinus + sin(angle) * sin(phase)'sin(angle) simule le rayon.
      amplitudeCosinus = amplitudeCosinus + sin(angle) * cos(phase)
    next
    periode(x, y) = atn(amplitudeSinus / amplitudeCosinus)'période absolue.
    if amplitudeCosinus < 0 then periode(x, y) = periode(x, y) + pi'valider les 4 quadrants.
    luminance(x, y) = 2 * normaliser * sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
    ton = luminance(x, y)
    if ton > 255 then ton = 255
    gris(x, y) = rgb(ton, ton, ton)                       'échelle de gris: énergie.
    rouge = 100 * (cos(periode(x, y)) + 1)                'quadrature: éviter l'amplitude à zéro.
    vert = 100 * (cos(periode(x, y) + pi) + 1)
    rouge = rouge + (luminance(x, y) - 155)
    vert = vert + (luminance(x, y) - 155)
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    bleu = (rouge + vert) / 2
    if lambda < 6 then couleur(x, y) = gris(x, y) else couleur(x, y) = rgb(rouge, vert, bleu)
    if mode$ = "energie" or mode$ = "axial" then
      pset(xCentre + x, yCentre - y), gris(x, y)          'diagramme en échelle de gris.
      pset(xCentre + x, yCentre + y), gris(x, y)
      pset(xCentre - x, yCentre - y), gris(x, y)
      pset(xCentre - x, yCentre + y), gris(x, y)
    elseif mode$ = "couleurs" then
      pset(xCentre + x, yCentre - y), couleur(x, y)
      pset(xCentre + x, yCentre + y), couleur(x, y)
      pset(xCentre - x, yCentre - y), couleur(x, y)
      pset(xCentre - x, yCentre + y), couleur(x, y)
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
calculFait = 1                                            'indicateur: calcul terminé.
return
