largeur = 800: hauteur = 600
dim couleur(96, largeur, hauteur)                         'grand tableau de variables 3D.
dim graph(96, largeur)
dim as single phase1(largeur, hauteur)
dim as single phase2(largeur, hauteur)
dim as single phase3(largeur, hauteur)
dim as single energie(largeur, hauteur)
dim as single amplitude1(largeur, hauteur)
dim as single amplitude2(largeur, hauteur)
dim as single affaib, rayon, max, angle, angle1, angle2, phi, phase, facteur
dim as single amplitude, amplitudeSin, amplitudeCos, flottante, nombre, pas, spin
dim as single diagonale1, diagonale2, periode, temps, differenceDeMarche, espacement
dim as single pi, deuxPi, rotation, sag, prec, beta, demiEspace, brillance, luminance
screen 19,24,3: page1 = 1: gosub Initialisation

do'             LES CHAMPS DE FORCE. AJOUT D'UNE ONDE PLANE À DEUX ONDES SPHÉRIQUES.
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  image = image + 1
  if bitmap then cls
  if OK then
    for x = 0 to largeur
      getmouse xS, yS, , clac                             'saisie Souris: vérifier souvent.
      if clac > 0 then clic = clac: xSouris = xS: ySouris = yS 'les valeurs persistent.
      for y = 0 to hauteur
        pset(x,y), couleur(image,x,y)'                    'graphique 2-D.
      next
    next
    if pleinEcran = 0 then
      prec = graph(image,0)
      for x = 1 to largeur
        line(x-1, prec)-(x, graph(image,x)), noir         'graphique 1-D.
        prec = graph(image,x)
      next
    end if    
  else gosub Aiguiller                                    'mémoriser les pixels.
  end if

  saisie$ = inkey
  if len(saisie$) then
    capture = 0
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    select case saisie$
      case "I": gosub Initialisation
      case "E": if choix$ <> "ellipses" then   choix$ = "ellipses"
      case "H": if choix$ <> "hyperboles" then choix$ = "hyperboles"
      case "J": if pleinEcran then pleinEcran = 0 else pleinEcran = 1
      case "M": run "Ether00.exe"                         'menu principal, souvent utile.
      case "O": if choix$ <> "amplitude" then  choix$ = "amplitude"
      case "R": t = timer: do: loop until timer - t > 1
      case "S","T","U","V","W","X","Y","Z": combo$ = saisie$: biconvexe = 1
      case chr$(27): if pleinEcran then
                       pleinEcran = 0: combo$ = ""
                     else end
                     end if
      case "1": lambda = 12                               'divisible par 4.
      case "2": lambda = 20
      case "3": lambda = 32
      case "4": lambda = 40
      case "5": lambda = 52
      case "6": lambda = 60
      case "7": lambda = 72
      case "8": lambda = 80
      case "9": lambda = 92
      case "0": lambda = 8
      case "+": xElectron2 = xElectron2 + lambda / 4: combo$ = ""
      case "-": xElectron2 = xElectron2 -  lambda / 4: combo$ = ""
      case "I+": if brillance < 4 then brillance = brillance + .2 else saisie$ = "nul"' page haut.
      case "Q+": if brillance > .39 then brillance = brillance - .2 else saisie$ = "nul"' page bas.
      case ";+": images = 12                              'F1.
      case "<+": images = 24                              'F2.
      case "=+": images = 48                              'F3.
      case ">+": images = 96                              'F4.
      case "?+": accord = 0: combo$ = ""                  'F5.
      case "@+": accord = 1: combo$ = ""                  'F6.
      case "A+": accord = 2: combo$ = ""                  'F7.
      case "B+": accord = 3: combo$ = ""                  'F8.
      case "K+": run "Ether11.exe"                        'flèche gauche.
      case "M+": run "Ether13.exe"                        'flèche droite.  
      case "k+", "X+", chr(27): end
      case else: saisie$ = "nul"
    end select
    if saisie$ = "R" or saisie$ = "nul" then
    else
      if clic < 1 then gosub MiseAJour
    end if    
    do: loop while len(inkey)
  end if

  getmouse xSouris, ySouris, , clic
  ligne = .5 + ySouris / 16
'  If pleinEcran Then ligne = 0
'  locate 34, 84: print xSouris; ySouris; clic; ligne; "    ";
  if ligne < 35 then
    if ligne < 25 or xSouris > 384 then ligne = 0
  else
    if xSouris < 304 or xSouris > 496 then ligne = 0
  end if
    
  color , turquoise: locate ligne, 2
  select case ligne'************************************** rehausser l'affichage.
    case 25: if pleinEcran then print ligne25a$ else print ligne25$
    case 26: print ligne26$
    case 28: if choix$ = "amplitude" then else print ligne28$
    case 29: if choix$ = "hyperboles" then else print ligne29$
    case 30: if choix$ = "ellipses" then else print ligne30$
    case 35: locate ligne, 39: print ligne35$
    case 36: locate ligne, 39: print ligne36$
    case 37: locate ligne, 39: print ligne37$;
             if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, fond

  if clic = 1 then'*************************************** action suite à un clic.
    select case ligne
      case 25: if pleinEcran then pleinEcran = 0 else pleinEcran = 1
               gosub MiseAJour
      case 26:                                            'sleep ci-dessous.
      case 28: if choix$ <> "amplitude"  then choix$ = "amplitude":  gosub MiseAJour
      case 29: if choix$ <> "hyperboles" then choix$ = "hyperboles": gosub MiseAJour
      case 30: if choix$ <> "ellipses"   then choix$ = "ellipses":   gosub MiseAJour
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether11.exe" else run "Ether13.exe"
      case else: ligne = 0
    end select
  end if
  if ligne = 0 and clic > 0 then gosub MiseEnPlace
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
'  locate 35, 2: print using "#.### sec"; timer - temps;: print "."
'  temps = timer
  if image = images then image = 0
  do
    if clic = 1 and ligne = 26 then sleep 1000: ySouris = 0: exit do
    getmouse xSouris, ySouris, , clic                     'éviter les actions à répétition.
  loop while clic > 0
loop

Aiguiller:'-------------------- GRAPHIQUE SELON LES CHOIX EN COURS ---------------------------
if image = images then OK = 1
rotation = image * 2 * pi / images                        'rotation de phase selon l'image.
prec = graphique

for x = 0 to largeur
  getmouse xS, yS, , clac                                 'vérifier souvent.
  if clac > 0 then clic = clac: xSouris = xS: ySouris = yS'les valeurs persistent.
  for y = 0 to hauteur
    select case choix$
'******************************** AFFICHER LA PÉRIODE EN COULEURS ****************************
      case "amplitude"
        amplitudeSin = amplitude1(x, y) * sin(phase1(x, y) - rotation) + amplitude2(x, y) * sin(phase2(x, y) - rotation)
        amplitudeSin = amplitudeSin + .008 * lambda * sin(phase3(x, y) - rotation) * energie(x,y)
        if espacement < 400 then facteur = brillance * 400 else facteur = brillance * espacement
        if amplitudeSin > 0 then
          r = 100 * facteur * amplitudeSin / lambda
          b = r / 2
          if r > 255 then g = r - 255 else g = 0
        else
          g = -100 * facteur * amplitudeSin / lambda
          b = g / 2
          if g > 255 then r = g - 255 else r = 0
        end if
        if y = yCentre then graph(image, x) = graphique - .3 * facteur * amplitudeSin / sqr(lambda)'pour graphique 1-D.
      case "hyperboles"                                   'faire apparaître les hyperboles.  
        if champ then
          
        else
            amplitude = 15000000 * energie(x,y) * amplitude1(x, y) * amplitude2(x, y) / lambda ^ 2'l'énergie est le carré de l'amplitude.
            if espacement > 300 then amplitude = amplitude * espacement ^ 2 / 90000
             amplitude = amplitude * sin(phase3(x, y) - rotation)

          if y = yCentre then graph(image, x) = graphique - 10000 * amplitude1(x, y) * amplitude2(x, y) * energie(x,y) / lambda + 15'pour graphique 1-D.
        end if
        luminance = amplitude1(x, y) * sin(phase1(x, y) - rotation) + amplitude2(x, y) * sin(phase2(x, y) - rotation)
        if luminance > 0 then
          r = brillance * amplitude
          b = r / 2
          if r > 255 then g = r - 255 else g = 0
        else
          g = brillance * amplitude
          b = g / 2
          if g > 255 then r = g - 255 else r = 0
        end if
      case "ellipses"                                     'faire apparaître les ellipses.
        if champ then
          
        else
          luminance = sin(phase1(x, y) / 2 + phase2(x, y) / 2 - rotation)
          if y = yCentre then graph(image, x) = graphique - 30 * abs(sin(phase1(x, y) / 2 + phase2(x, y) / 2 - rotation)) + 15
        end if
        r = 255 * abs(luminance)
        g = r
        b = r
        luminance = amplitude1(x, y) * sin(phase1(x, y) - rotation) + amplitude2(x, y) * sin(phase2(x, y) - rotation)
        if luminance > 0 then
          g = g / 2: b = b / 1.7
        else
          r = r / 2: b = b / 1.7
        end if        
    end select
    if bitmap then                                        '16 nuances pour animations GIF.
      r = r / 16: r = r * 16
      g = g / 16: g = g * 16
      b = b / 16: b = b * 16
    end if
    if r < 0 then r = 0'                                  'écrêter.
    if g < 0 then g = 0
    if b < 0 then b = 0
    if r > 255 then r = 255
    if g > 255 then g = 255
    if b > 255 then b = 255
    pset(x,y), rgb(r,g,b)                                 'graphique 2-D.
    couleur(image,x,y) = rgb(r,g,b)                       'grand tableau pour accélérer.
    if y = yCentre and pleinEcran = 0 then
      if graph(image, x) < hauteur then graph(image, x) = hauteur + 1' écrêter.
      line(x - 1, prec)-(x, graph(image, x)), noir        'graphique 1-D.
      prec = graph(image, x)
    end if
  next
next
return

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
select case capture
  case is < 10: numero$ = "00"
  case is < 100: numero$ = "0"
  case is < 1000: numero$ = ""
end select
locate 19, 1: print "Champs de force - Fields of Force  glafreniere.com "
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
locate 25, 2
print fichier$;: print using "  Image ##    "; image;
'bsave fichier$, 0
capture = capture + 1
'If capture > images - 1 Then End 
color noir, fond
return

MiseAJour:'------------------------ AFFICHER LES CHANGEMENTS ---------------------------------
OK = 0
clic = 0
image = 0
lambdaSurDeux = lambda / 2
screenset 2
locate 25, 2: print ligne25$                              'rafraîchir les choix.
locate 26, 2: print ligne26$
locate 28, 2: print ligne28$
locate 29, 2: print ligne29$
locate 30, 2: print ligne30$

color bleu, fond                                          'identifier le choix en cours.
select case choix$
  case "amplitude":  locate 28, 2: print ligne28$
  case "hyperboles": locate 29, 2: print ligne29$
  case "ellipses":   locate 30, 2: print ligne30$
end select
color noir, fond
if pleinEcran then
  largeur = 800: hauteur = 600
else
  largeur = 400: hauteur = 300
end if
xCentre = largeur / 2
yCentre = hauteur / 2

select case accord
  case 0: spin = 0:          accord$ = "0 pi    "
  case 1: spin = pi / 2:     accord$ = "pi / 2  "
  case 2: spin = pi:         accord$ = "pi      "
  case 3: spin = 3 * pi / 2: accord$ = "3 pi / 2"
end select
locate 32, 2: print "Page haut / Page bas - Luminosit‚:";: print using " #.#"; brillance
locate 33, 2: print "F1 … F4 - Nombre d'images par cycle:"; images
locate 34, 2: print "F5 … F8 - Spin (d‚phasage): "; accord$
locate 35, 2: print "Chiffre de 0 … 9 - Lambda:"; lambda; " pixels. "
locate 36, 2: print "Lettres de S … Z - Configurations: "; combo$; " "
locate 37, 2: print "D‚placez les sources avec la souris.";

espacement = abs(xElectron2 - xElectron1)
demiEspace = espacement / 2
yElectron1 = yCentre
yElectron2 = yCentre
screenset page1, page2
gosub Tableau
return

MiseEnPlace:'--------------- Positionner les électrons avec la souris ------------------------
screenset page1, page1
do
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  line(0, yCentre - lambdaSurDeux)-(799, yCentre + lambdaSurDeux), fond, bf
  line(0, yCentre - lambdaSurDeux)-(799, yCentre + lambdaSurDeux), noir, b
  depart1 = (799 - xElectron2) / lambda                   'entier, en longueurs d'ondes.
  depart1 = xElectron2 + depart1 * lambda                 'entier, en pixels.
  depart2 = xElectron1 / lambda
  depart2 = xElectron1 - depart2 * lambda
  Saisie$ = ""
  
  if clic = 1 then                                        'électron1.
    if ySouris > hauteur / 2 + lambdaSurDeux or ySouris < hauteur / 2 - lambdaSurDeux then
      if biconvexe = 0 then xSouris = xElectron2 else xSouris = xElectron1 
      Saisie$ = "nul"
    end if
    circle(xElectron2, yCentre), lambdaSurDeux, noir
    line(xElectron2, yCentre - lambdaSurDeux)-(xElectron2, yCentre + lambdaSurDeux), noir
    for pas = depart1 to 0 step -lambda
      line(pas, yCentre - lambdaSurDeux)-(pas, yCentre + lambdaSurDeux), rouge
    next
    for pas = depart1 + lambdaSurDeux to 0 step -lambda
      line(pas, yCentre - lambdaSurDeux)-(pas, yCentre + lambdaSurDeux), vert
    next
    circle(xSouris, yCentre), lambdaSurDeux, noir
    line(xSouris, yCentre - lambdaSurDeux)-(xSouris, yCentre + lambdaSurDeux), noir
  elseif clic = 2 then                                                    'électron2.
    if ySouris > hauteur / 2 + lambdaSurDeux or ySouris < hauteur / 2 - lambdaSurDeux then xSouris = xElectron2: Saisie$ = "nul"
    if biconvexe then circle(xElectron1, yCentre), lambdaSurDeux, noir
    line(xElectron1, yCentre - lambdaSurDeux)-(xElectron1, yCentre + lambdaSurDeux), noir
    for pas = depart2 to 800 step lambda
      line(pas, yCentre - lambdaSurDeux)-(pas, yCentre + lambdaSurDeux), rouge
    next
    for pas = depart2 - lambdaSurDeux to 800 step lambda
      line(pas, yCentre - lambdaSurDeux)-(pas, yCentre + lambdaSurDeux), vert
    next
    circle(xSouris, yCentre), lambdaSurDeux, noir
    line(xSouris, yCentre - lambdaSurDeux)-(xSouris, yCentre + lambdaSurDeux), noir
  else
    Saisie$ = "nul"
    if biconvexe then circle(xElectron1, yCentre), lambdaSurDeux, noir
    circle(xElectron2, yCentre), lambdaSurDeux, noir
    for pas = depart1 to 0 step -lambda
      line(pas, yCentre - lambdaSurDeux)-(pas, yCentre), rouge
    next
    for pas = depart1 + lambdaSurDeux to 0 step -lambda
      line(pas, yCentre - lambdaSurDeux)-(pas, yCentre), vert
    next
    for pas = depart2 to 800 step lambda
      line(pas, yCentre)-(pas, yCentre + lambdaSurDeux), rouge
    next
    for pas = depart2 + lambdaSurDeux to 800 step lambda
      line(pas, yCentre)-(pas, yCentre + lambdaSurDeux), vert
    next
  end if

  getmouse xS, yS, , clac
  if clac > 0 then clic = clac: xSouris = xS: ySouris = yS
loop while clac > 0

if Saisie$ <> "nul" then
  if clic = 1 then
    if biconvexe = 0 then xElectron2 = xSouris else xElectron1 = xSouris
  end if
  if clic = 2 then xElectron2 = xSouris
  combo$ = ""
  gosub MiseAJour
end if
return

Tableau:'--------------------- METTRE LES DONNÉES EN MÉMOIRE ---------------------------------
for x = 0 to largeur
  xCoord1 = x - xElectron1
  xCarre1 = xCoord1 ^ 2
  xCoord2 = x - xElectron2
  xCarre2 = xCoord2 ^ 2
  for y = 0 to hauteur
    yCoord1 = y - yElectron1
    yCoord2 = y - yElectron2

'*************************************   PREMIER SYSTÈME
    diagonale1 = sqr(yCoord1 * yCoord1 + xCarre1)         'distance réelle selon Pythagore.
    flottante = diagonale1 / lambda                       'distance en longueurs d'onde.
    entier = flottante                                    'distance, nombre entier.
    differenceDeMarche = flottante - entier               'difference en longueurs d'onde.
    amplitude1(x,y) = lambdaSurDeux / diagonale1
    if diagonale1 < lambdaSurDeux then
      sag = rayon - sqr(rayon ^ 2 - differenceDeMarche ^ 2) 'flèche d'un cercle fictif.
      differenceDeMarche = 1.25 * (.2 + sag)              'arrondir l'angle central.
      amplitude1(x,y) = 1
    end if
    phase1(x, y) = deuxPi * differenceDeMarche            'phase en radians, système 1.

'*************************************   DEUXIÈME SYSTÈME
    diagonale2 = sqr(yCoord2 * yCoord2 + xCarre2)         'comme ci-dessus.
    amplitude2(x,y) = lambdaSurDeux / diagonale2
    flottante = diagonale2 / lambda
    entier = flottante
    differenceDeMarche = flottante - entier
    if diagonale2 < lambdaSurDeux then
      sag = rayon - sqr(rayon ^ 2 - differenceDeMarche ^ 2)
      differenceDeMarche = 1.25 * (.2 + sag)
      amplitude2(x,y) = 1
    end if
    if diagonale1 > diagonale2 then distance = diagonale1 else distance = diagonale2
    affaib = demiEspace / distance                 'la partie stationnaire n'est jamais plus
    phase2(x, y) = spin + deuxPi * differenceDeMarche     'phase de la deuxième onde.
    

    amplitudeSin = (sin(phase1(x, y)) + sin(phase2(x, y))) / 2
    amplitudeCos = (cos(phase1(x, y)) + cos(phase2(x, y))) / 2
    energie(x, y) = amplitudeSin * amplitudeSin + amplitudeCos * amplitudeCos' max = 1

'*****************************  TROISIÈME SYSTÈME: ONDES PLANES *****************************
    flottante = x / lambda                                'distance en longueurs d'onde.
    entier = flottante                                    'distance, nombre entier.
    differenceDeMarche = flottante - entier               'difference en longueurs d'onde.
    phase3(x, y) = deuxPi * differenceDeMarche            'phase en radians, système 1.
    
  next y
next x
return

'----------------------------------- DESSIN DES FLÈCHES --------------------------------------
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
turquoise = rgb (230, 255, 255)
fond = rgb(225,225,225)
blanc= rgb(255,255,255)
rouge = rgb(255,0,0)
bleu = rgb(0,0,255)
vert = rgb(0,150,0)
pi = 4 * atn(1)
deuxPi = 8 * atn(1)
images = 24
largeur = 400: hauteur = 300
'bitmap = 1                                               'séquence bitmap si désiré.
rayon = sin(pi / 4)'                                      'rayon fictif pour le noyau central.
champ = 0                                                 '1 -> afficher le champ. 0 -> ondes.
biconvexe = 1                                             'champ biconvexe.
combo$ = ""
choix$ = "hyperboles"
choix$ = "ellipses"
choix$ = "amplitude"
accord = 2                                                'couple électron-positron par défaut.
'spin = pi                                                'spin, 2e électron ou positron.
lambda = 12
lambdaSurDeux = lambda / 2
pleinEcran = 0
xCentre = largeur / 2
yCentre = hauteur / 2
espacement = .75 * largeur
entier = espacement / lambda
espacement = entier * lambda                              'multiple exact.
xElectron1 = largeur - 6 * lambda
xElectron2 = largeur
yElectron1 = yCentre
yElectron2 = yCentre
brillance = 1
graphique = hauteur + 28
xdd = 400 + 50
xgg = 400 - 50                                            'coordonnées des flèches.
xdg = 400 + 20
xgd = 400 - 20
yFleche = 584
screenset 2
color noir, fond: cls
if bitmap then gosub MiseAJour: return
titre$ = " LES CHAMPS DE FORCE HYPERBOLIQUES "                        'titre.
centre = 800 - (800 - largeur) / 2
xg = centre - 4 * len(titre$) + 3
xd = centre + 4 * len(titre$) + 3
yh = 10
yb = 34
line(xg-3,yh-3)-(xd-3,yb-3), blanc, B 
line(xg-2,yh-2)-(xd-2,yb-2), blanc, B 
line(xg-1 ,yh-1)-(xd-1,yb-1), blanc, B 
line(xg,yh)-(xd,yb), rouge,BF

line(4,431)-(4,480), noir                                 'crochets pour choix multiples.
line(4,431)-(7,432), noir, b
line(4,479)-(7,480), noir, b
color blanc, rouge
locate 2,59: print titre$
color noir, fond
locate 2
locate , 2:  print "Utilisez le bouton gauche ou droit de la souris"
locate , 2:  print "pour d‚placer les ‚lectrons. Les lignes vertes "
locate , 2:  print "indiquent les demi-longueurs d'onde. Ajustez la"
locate , 2:  print "luminosit‚ … l'aide des touches [page haut] et "
locate , 2:  print "[page bas]. Vous pouvez aussi d‚placer le deu- "
locate , 2:  print "xiŠme ‚lectron, par pas d'un quart d'onde, avec"
locate , 2:  print "les touches [ + ] et [ - ]."
locate 4
locate , 53:  print "Le programme pr‚c‚dent montrait comment les on-"
locate , 53:  print "des provenant de deux ‚lectrons se composent. "
locate , 53:  print "Ici, on a ajout‚ des ondes planes provenant de "
locate , 53:  print "la gauche; le diagramme montre donc comment les"
locate , 53:  print "trois trains d'onde se composent. Et puisque la"
locate , 53:  print "superposition des ondes sph‚riques produit des "
locate , 53:  print "hyperboles, l'ensemble demeure hyperbolique."
print
locate , 53:  print "Par ailleurs le programme suivant montre que si"
locate , 53:  print "des ondes sont stationnaires, elles doivent ra-"
locate , 53:  print "yonner des ondes convergentes pourvu qu'elles  "
locate , 53:  print "soient amplifi‚es par les ondes de l'‚ther. Le "
locate , 53:  print "principe de Huygens s'applique, en ce sens que "
locate , 53:  print "les ondelettes devraient provoquer la formation"
locate , 53:  print "de nouveaux fronts d'onde."
locate 22
locate , 53:  print "Ce programme est en cours de r‚daction. Il ne  "
locate , 53:  print "sera pas termin‚ prochainement parce qu'il me  "
locate , 53:  print "servira … analyser les propri‚t‚s de ces champs"
locate , 53:  print "de force, qui me sont pour l'instant inconnues."
print
locate , 53:  print "Il m'apparaŒt clairement que les effets magn‚- "
locate , 53:  print "tiques sont produits par de tels systŠmes. Les "
locate , 53:  print "ondes convergentes devraient provoquer une syn-"
locate , 53:  print "chronisation des ‚lectrons. Elles devraient les"
locate , 53:  print "alimenter aussi en ‚nergie suppl‚mentaire, mˆme"
locate , 53:  print "si l'effet de lentille y contribue d‚j…."

ligne25$ = " J - Graphique pleine fenˆtre.                 ": locate 25, 2: print ligne25$ 
ligne25a$ =" J - Retour au graphique normal.               "
ligne26$ = " R - Ralenti.                                  ": locate 26, 2: print ligne26$ 
ligne28$ = " O - Afficher l'amplitude (les ondes).         ": locate 28, 2: print ligne28$
ligne29$ = " H - L'‚nergie seulement (les hyperboles).     ": locate 29, 2: print ligne29$
ligne30$ = " E - Les phases relatives (les ellipses).      ": locate 30, 2: print ligne30$
ligne35$ = "    I - Initialiser.    ": locate 35, 39: print ligne35$
ligne36$ = "    Quitter (Echap).    ": locate 36, 39: print ligne36$
ligne37$ = "                        "
gosub flecheGauche: gosub flecheDroite

color vert:
locate 13
locate , 2:  print "Merci aux collaborateurs du projet Ether Virtuel."
locate , 2:  print "- Philippe Delmotte"
locate , 2:  print "- Anselme Dewavrin"
locate , 2:  print "- Jocelyn Marcotte   ...et quelques autres."
locate 35, 63: print "Gabriel LaFreniŠre. glafreniere.com"
locate 36, 63: print "20 mars 2006. Ce programme peut ˆtre";
locate 37, 63: print "distribu‚, copi‚ ou modifi‚ librement.";
pcopy 2, 0
pcopy 2, 1
color noir, fond
gosub MiseAJour
return
' e accent aigu:        ‚
' e accent grave:       Š
' e accent circonflexe: ˆ
' e tr‚ma:              ‰
' a accent grave:       …
' u accent grave:       —
' u accent circonflexe: –
' o accent circonflexe  “
' i accent circonflexe  Œ
' c cédille:            ‡
' guillemets:           ®  ¯