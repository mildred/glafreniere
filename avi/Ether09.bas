cote = 1600                                               'nombre de particules par côté.
dim as single M(-1 to cote+1, -1 to cote+1)
dim as single I(-1 to cote+1, -1 to cote+1): dim precedent(-1 to cote+1)
dim as single P(-1 to cote+1, -1 to cote+1), potentiel2(-1 to cote+1, -1 to cote+1)
dim as single pi, angle, lambda, lambda1, lambda2, xCarre, yCarre, arcSinus, xDistance
dim as single xCoord, distNormale, periode, distance, rotation, amplitude, phi, rapport
dim as single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance, facteur
dim as single betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, pas, ton, facteurLorentz
SCREEN 19,24,3: gosub Initialisation: gosub Doppler

do'    MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS AVEC EFFET DOPPLER, 2 déc. 2005.

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
  
  for x = 0 to cote - 1                                   'pas de réflexion.
    P(x, -1) = P(x, 0) - 2 * M(x, 0): P(x, cote) = P(x, cote - 1) - 2 * M(x, cote - 1)
  next
  for y = 0 to cote - 1
    P(-1, y) = P(0, y) - 2 * M(0, y): P(cote, y) = P(cote - 1, y) - 2 * M(cote - 1, y)
  next
  
  getmouse xSouris, ySouris, , clic                       'vérifier entre les affichages.
  if afficher = 0 or clic = 1 then                        'afficher une fois sur deux.
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    if relief then gosub afficherRelief else gosub afficher'DIAGRAMME PRINCIPAL --------------

    line(xCentre - gauche, 0)-(xCentre - gauche,40),noir  'repères de l'origine.
    line(xCentre - gauche, 360)-(xCentre - gauche,397),noir
    line(xCentre - gauche + 1, 0)-(xCentre - gauche + 1,40),blanc
    line(xCentre - gauche + 1, 360)-(xCentre - gauche + 1,397),blanc
    
    if bitmap then
      locate 26, 2:  print "L'onde de LaFreniŠre           glafreniere.com          LaFreniere's Wave"
      locate 30, 16: print "Dec. 2005"
    end if
    line(0, graphique)-(600, graphique), gris             'graphique 1D.
    prec = graphique
    for x = gauche to droite
      courbe = graphique - 3 * P(x, cote / 2)
      line(x - gauche - 1, prec)-(x - gauche, courbe), 0
      prec = courbe
    next
    
    saisie$ = ucase(inkey)                                'saisie clavier.
    if len(saisie$) then
      capture = 0
      if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+"
      select case saisie$
        case "I": beta = .5: gosub Doppler
        case "A": gosub Inverser
        case "B": if relief then relief = 0 else relief = 1
        case "X+",CHR$(27): end
        case "M": run "Ether00.exe"
        case "K+":run "Ether08.exe"                       'flèche gauche.
        case "M+":run "Ether00.exe"                       'flèche droite.  
        case "1": beta = .1: gosub Doppler
        case "2": beta = .2: gosub Doppler
        case "3": beta = .3: gosub Doppler
        case "4": beta = .4: gosub Doppler
        case "5": beta = .5: gosub Doppler
        case "6": beta = .6: gosub Doppler
        case "7": beta = .7: gosub Doppler
        case "8": beta = .8: gosub Doppler
        case "9": beta = .9: gosub Doppler
        case "0": beta =  0: gosub Doppler
      end select
    end if
    
    locate 22, 77: if relief then print ligne22a$ else print ligne22b$
    locate 30, 2:  print using "Beta: .# c"; beta
    if clic <> 1 then getmouse xSouris, ySouris, , clic   'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
    if ligne < 20 then
      if ligne < 10 or xSouris > 794 or xSouris < 616 then ligne = 0
    elseif ligne > 34 then
      if ligne > 37 or xSouris < 304 or xSouris > 496 then ligne = 0
    end if
    if clic = 1 then
      select case ligne
        case 10 : beta = 0 : gosub Doppler
        case 11 : beta = .1: gosub Doppler
        case 12 : beta = .2: gosub Doppler
        case 13 : beta = .3: gosub Doppler
        case 14 : beta = .4: gosub Doppler
        case 15 : beta = .5: gosub Doppler
        case 16 : beta = .6: gosub Doppler
        case 17 : beta = .7: gosub Doppler
        case 18 : beta = .8: gosub Doppler
        case 19 : beta = .9: gosub Doppler
        case 21 : gosub Inverser
        case 22 : if relief then relief = 0 else relief = 1
      end select
    end if

    color , turquoise: locate ligne, 77
    select case ligne                                     'rehausser l'affichage.
      case 10: print ligne10$
      case 11: print ligne11$
      case 12: print ligne12$
      case 13: print ligne13$
      case 14: print ligne14$
      case 15: print ligne15$
      case 16: print ligne16$
      case 17: print ligne17$
      case 18: print ligne18$
      case 19: print ligne19$
      case 21: print ligne21$
      case 22: if relief then print ligne22a$ else print ligne22b$
      case 35: LOCATE, 39: PRINT ligne35$: if clic = 1 then beta = .5: gosub Doppler
      case 36: LOCATE, 39: PRINT ligne36$: if clic = 1 then end
      case 37: LOCATE, 39: PRINT ligne37$;
               if xSouris < 400 then
                 gosub flecheGauche
                 if clic = 1 then run "Ether08.exe"
                else
                  gosub flecheDroite
                 if clic = 1 then  run "Ether00.exe"
                end if      
    end select
    color , fond
    if bitmap then gosub Bitmaps                          'capture d'images si désiré.
    locate 30, 80: print using "#.## sec"; timer - temps;
    temps = timer
    capture = capture + 1: if capture > 999 then capture = 0: gosub Doppler
    print ". Image"; capture
  end if
  afficher = afficher + 1
  if afficher = 2 then afficher = 0                       'afficher une fois sur deux
loop

Doppler:'----------------------------- EFFET DOPPLER -----------------------------------------
color noir, fond: cls
locate 15, 10: print "Calcul en cours..."
swap page1, page2
screenset page1, page2
pcopy 2, page1
reflexion = 0
facteurLorentz = sqr(1 - beta ^ 2)                        'facteur de contraction de Lorentz.
lambda1 = lambda / facteurLorentz                         'fréquence ralentie selon Lorentz.
rapport = (1 + beta) / (1 - beta)                         'rapport des longueurs d'onde.
xCentre = cote / (rapport + 1)                            'nombre égal d'ondes de chaque côté.
ondes = xCentre / (lambda1 * (1 - beta))                  'ne pas dépasser la fenêtre.
difference = (cote - largeur) / 2
gauche = difference - difference * beta                   'pixels visibles sur le diagramme.
droite = gauche + largeur

for x = -1 to cote
  xDistance = x - xCentre
  xCarre = xDistance * xDistance
  for y = -1 to cote
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    distance = sqr(xCarre + yCarre)                       'distance selon Pythagore.
    if xDistance > 0 then                                 'distinguer les quadrants.
      phi = atn(yDistance / xDistance) + pi               'phi est l'angle de propagation.
    elseif xDistance < 0 then phi = atn(yDistance / xDistance)
    else phi = pi / 2                                     'éviter la division par zéro.
    end if
    lambda2=lambda1*(cos(asin(beta*sin(phi)))-beta*cos(phi))'effet Doppler « relatif ».
    deuxPiDistanceSurLambda = 2 * pi * distance / lambda2 'différence de marche selon l'angle.
    amplitudeSinus = sin(deuxPiDistanceSurLambda)
    amplitudeCosinus = .08 *  cos(deuxPiDistanceSurLambda)
    if distance > 4 * lambda2 then amplitude = 4 * lambda2 / distance else amplitude = distance / (4 * lambda2)' affaiblir au centre.
    if distance / lambda2 > ondes then amplitude = 0      'limite extérieure.
    amplitude = 1.5 * amplitude * facteurLorentz          'compenser l'augmentation de masse.
    P(x, y) = amplitude * amplitudeSinus
    M(x, y) = amplitude * amplitudeCosinus                'inclut une correction à expliquer.
  next
next
do: loop while len(inkey)
return

EffacerMemoire:'------- EFFACER LES DONNÉES RELATIVES A LA MÉMOIRE------------------------
if clic > 0 then
  do
    getmouse xSouris, ySouris, , clic'                    'éviter des actions à répétition.
  loop while clic > 0
end if

for x = -1 to cote
  for y = -1 to cote
    M(x, y) = 0
  next
next
do: loop while len(inkey)
return

Inverser:'------------------------ INVERSER LE SENS DES ONDES --------------------------------
if clic > 0 then
  do
    getmouse xSouris, ySouris, , clic'                    'éviter des actions à répétition.
  loop while clic > 0
end if

for x = -1 to cote
  for y = -1 to cote
    M(x, y) = -M(x, y)
   'P(x, y)= -P(x, y)                                     'alternative.
  next
next
do: loop while len(inkey)
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
if capture > 500 then end 
color noir, fond
return

afficher:'--------------------------- AFFICHER NORMALEMENT -----------------------------------
haut = cote / 2 - (hauteur / 2)
bas = haut + hauteur - 2
for x = gauche to droite
  for y = haut to bas
    luminance = 30 * (P(x, y) + 4)
    if luminance > 255 then luminance = 255
    if luminance < 0 then luminance = 0
    PSET (x - gauche, y - haut), rgb(luminance,luminance,luminance)
  next
next
return

afficherRelief:'---------------------- AFFICHER EN RELIEF ------------------------------------
haut = 200                                                'doit être pair re: espaces vides.
bas = (haut + hauteur) * 2 - 2
for x = gauche to droite
  for y = haut to bas step 2                              'step 2 pour aplatir en ellipses.
    luminance = 30 * (P(x, y) + 4)
    if luminance > 255 then luminance = 255
    if luminance < 0 then luminance = 0
    yCoord = y / 2 - haut - facteur * P(x, y)             'décaler selon l'amplitude.
    PSET (x - gauche, yCoord), rgb(luminance,luminance,luminance)
    if y > haut then ecart = yCoord - yCoordPrec else ecart = 1
    if ecart > 1  then
      luminance2 = 30 * (P(x, y - 2) + 4)                 'estomper avec le pixel précédent.
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
line(0, 398)-(600,398), fond                              'niveler le bas.
line(0, 399)-(600,399), fond
line(0, 400)-(600,400), fond
line(0, 401)-(600,401), fond
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

Initialisation:'------------------------ INITIALISATION --------------------------------------
fond = rgb(225,225,225)
blanc= rgb(255,255,255)
gris = rgb(150,150,150)
bleu = rgb(0,0,255)
vert = rgb(0,150,0)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               'séquence bitmap si désiré.
relief = 1                                                'affiche les ondes en relief.
beta = .5                                                 'moitié de la vitesse de la lumière.
page1 = 1
lambda = 40
yCentre = 200
largeur = 600                                             'largeur de la partie visible.
hauteur = 400                                             'hauteur de la partie visible.
facteur = 1.5                                             'facteur d'amplitude du relief.
reflexion = 2                                             'réflexion molle.
graphique = 2 * yCentre + 45                              'emplacement du graphique 2-D.
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
color noir, fond: cls
line(611,7)-(787,42), blanc, B 
line(612,8)-(787,42), rgb(255,0,0),BF 
titre$ = "L'ELECTRON": print titre$
xOrig = 800 - 200 / 2 - len(titre$) * 8 : 
yOrig = 12
for x = 0 to 8 * len(titre$)                              'agrandir l'en-tête.
  for y = 0 to 16
    if point(x,y) = 0 then
      pset(2 * x + xOrig, 2 * y + yOrig), blanc
      pset(2 * x + xOrig + 1, 2 * y + yOrig), blanc
      pset(2 * x + xOrig, 2 * y + yOrig + 1), blanc
      pset(2 * x + xOrig + 1, 2 * y + yOrig + 1), blanc
    end if    
  next
next
line(656,13)-(662,11), blanc
line(656,14)-(662,12), blanc
locate 1,1: print "          "
locate 15, 10: print "Calcul en cours..."
locate 30, 63: print "2 dimensions."
'locate 31, 2: print "L'‚ther virtuel de Philippe Delmotte.  Philippe Delmotte's Virtual Aether."
locate 4,78: print "dans l'‚ther virtuel"
locate 5,78: print "de Philippe Delmotte."
locate 32, 2: print "Des ondes convergentes avec effet Doppler produisent des interf‚rences trŠs particuliŠres."
locate 33, 2: print "L'‚lectron est fait d'ondes stationnaires sph‚riques qui pr‚sentent le mˆme aspect."
locate 7, 78: print "Choisissez la vitesse" 
locate 8, 78: print "normalis‚e bˆta (v/c):"
ligne10$ = " 0 - 0,0 c  au repos.  ": locate 10, 77: print ligne10$
ligne11$ = " 1 - 0,1 c             ": locate 11, 77: print ligne11$
ligne12$ = " 2 - 0,2 c             ": locate 12, 77: print ligne12$
ligne13$ = " 3 - 0,3 c             ": locate 13, 77: print ligne13$
ligne14$ = " 4 - 0,4 c             ": locate 14, 77: print ligne14$
ligne15$ = " 5 - 0,5 c  50 % de c. ": locate 15, 77: print ligne15$
ligne16$ = " 6 - 0,6 c             ": locate 16, 77: print ligne16$
ligne17$ = " 7 - 0,7 c             ": locate 17, 77: print ligne17$
ligne18$ = " 8 - 0,8 c             ": locate 18, 77: print ligne18$
ligne19$ = " 9 - 0,9 c  90 % de c. ": locate 19, 77: print ligne19$
ligne21$ = " A - Inverser le sens. ": locate 21, 77: print ligne21$
ligne22a$ =" B - Ondes sans relief."
ligne22b$ =" B - Ondes avec relief."
locate 22, 77: if relief then print ligne22a$ else print ligne22b$
ligne35$ = "    I - Initialiser.    ": LOCATE 35, 39: PRINT ligne35$
ligne36$ = "    Quitter (Echap).    ": LOCATE 36, 39: PRINT ligne36$
ligne37$ = "                        "
gosub flecheGauche: gosub flecheDroite
locate 24
locate , 78: print "S.V.P veuillez patien-"
locate , 78: print "ter. Ce programme doit"
locate , 78: print "‚valuer plus de cinq"
locate , 78: print "millions de granules"
locate , 78: print "d'‚ther … chaque image."
color vert
locate 35
locate , 2:  print "Mes remerciements chaleureux …"
locate , 2:  print "M. Anselme Dewavrin pour son aide."
locate , 2:  print "Gabriel LaFreniŠre  glafreniere.com";
locate 35
locate , 70: print "D‚c. 2005. Ce programme"
locate , 70: print "FreeBASIC peut ˆtre distribu‚,"
locate , 70: print "copi‚ ou modifi‚ librement.";
color noir
pcopy 0, 2
pcopy 0, page1
return
