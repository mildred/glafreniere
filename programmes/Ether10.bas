images = 36: largeur = 800: hauteur = 650
dim gris(images,largeur,hauteur)                          'grand tableau 3D.
dim graph(images,largeur)                                 'graphique 1D.
dim as single amplitude(largeur,hauteur)
dim as single pi, deuxPi, angle, phase, flottante, differenceDeMarche, facteurDeLorentz
dim as single diagonale, sag, periode, rotation, rapport, temps, beta, pas, deltaTemps
dim as single facteur, x, xCoord, xCarre, contraste, affaiblissement, rayon, amplit, phase2
screen 19,24,3: page1 = 1: gosub Initialisation

do'    « L'ONDE DE LAFRENIÈRE »
'      CALCULÉE À L'AIDE DES TRANSFORMATIONS DE LORENTZ (OU DU SCANNER DU TEMPS).
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1

  if OK then                                              'afficher le tableau de variables.
    for x = 0 to largeur
      for y = 0 to maximum
        pset(x,y),gris(image,x,y)
      next
    next    
  else gosub Memoriser
  end if

  saisie$ = inkey
  if len(saisie$) then
    capture = 0
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    select case saisie$
      case "I": gosub Initialisation
      case "A": screenset 2: locate 21, 77
                if acceleration then
                  acceleration = 0: print ligne21$
                else
                  acceleration = 1: print ligne21a$
                end if
                screenset page1, page2
      case "B": screenset 2: locate 22, 77
                if relief then
                  relief = 0: print ligne22$
                else
                  relief = 1: print ligne22a$
                end if                
                screenset page1, page2
      case "C": if pleinEcran then
                  pleinEcran = 0
                  screenset 2: locate 23, 77: print ligne23$
                  screenset page1, page2
                  else pleinEcran = 1
                end if
      case chr(27)
                if pleinEcran then
                  pleinEcran = 0
                  screenset 2: locate 23, 77: print ligne23$
                  screenset page1, page2
                  else end
                end if
      case "X+", "k+": end
      case "M": run "Ether00.exe"
      case "K+":run "Ether09.exe"                         'flèche gauche.
      case "M+":run "Ether11.exe"                         'flèche droite.  
      case "1": beta = .1
      case "2": beta = .2
      case "3": beta = .3
      case "4": beta = .4
      case "5": beta = .5
      case "6": beta = .6
      case "7": beta = .7
      case "8": beta = .8
      case "9": beta = .9
      case "0": beta =  0
      case "+": lambda = lambda + 10
                if lambda > 200 then lambda = 200
      case "-": lambda = lambda - 10
                if lambda < 20 then lambda = 20
      case else: saisie$ = "fait"
    end select
    do: loop while len(inkey)                             'vider le tampon.
    if saisie$ = "fait" then else image = 0: gosub Electron
  end if
  
  getmouse xSouris, ySouris, , clic                       'saisie Souris.
  ligne = .5 + ySouris / 16
  if ligne < 23 then
    if ligne < 10 or xSouris > 794 or xSouris < 616 then ligne = 0
  elseif ligne > 34 then
    if ligne > 37 or xSouris < 304 or xSouris > 496 then ligne = 0
  end if
  if clic = 1 then
    do: getmouse xSouris, ySouris, , clic: loop while clic > 0
    select case ligne
      case 10: beta = 0
      case 11: beta = .1
      case 12: beta = .2
      case 13: beta = .3
      case 14: beta = .4
      case 15: beta = .5
      case 16: beta = .6
      case 17: beta = .7
      case 18: beta = .8
      case 19: beta = .9
      case 21: screenset 2: locate 21, 77
               if acceleration then
                 acceleration = 0: print ligne21$
               else
                 acceleration = 1: print ligne21a$
               end if
               screenset page1, page2
      case 22: screenset 2: locate 22, 77
               if relief then
                 relief = 0: print ligne22$
               else
                 relief = 1: print ligne22a$
               end if                
               screenset page1, page2
      case 23: if pleinEcran then pleinEcran = 0 else pleinEcran = 1
      case 35: gosub Initialisation: ligne = 0
      case 36: end
      case 37: if xSouris < 400 then run "Ether09.exe" else  run "Ether11.exe"
      case else: saisie$ = "fait"
    end select
    if saisie$ = "fait" then else image = 0: gosub Electron
  end if

  color , turquoise: locate ligne, 77
  select case ligne                                       'rehausser l'affichage.
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
    case 21: if acceleration then print ligne21a$ else print ligne21$
    case 22: if relief then print ligne22a$ else print ligne22$
    case 23: if pleinEcran then print ligne23a$ else print ligne23$
    case 35: locate, 39: print ligne35$
    case 36: locate, 39: print ligne36$
    case 37: locate, 39: print ligne37$;
             if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color , fond
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
  image = image + 1: if image > images then image = 1
  if acceleration then
    beta = beta + .001: if beta > .99 then beta = 0
    gosub Electron
  end if
loop

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
select case capture
  case is < 10: numero$ = "00"
  case is < 100: numero$ = "0"
  case is < 1000: numero$ = ""
end select
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
locate 34, 43: print fichier$
'bsave fichier$,0
capture = capture + 1
if capture > images - 1 then end 
color noir, fond
return

Electron:'------------------ METTRE EN MÉMOIRE LES DONNÉES DE L'ÉLECTRON ----------------------

'############################ APPLICATION DES TRANSFORMATIONS DE LORENTZ #####################

facteurDeLorentz = sqr(1 - beta ^ 2)                      'facteur de contraction de Lorentz.
deltaTemps = beta / facteurDeLorentz                      'temps local selon Henri Poincaré.

'#############################################################################################
screenset 2                                               'mise à jour de l'image-mère.
locate 28, 78: print "lambda ="; lambda; " pixels.   "
locate 28, 2:  print using "Beta: #.###"; beta
locate 28, 46: print using "Contraction: #.###"; facteurDeLorentz
screenset page1, page2
if OK then image = 0                                      'sinon accélération compromise.
OK = 0                                                    'tableau de variables à refaire.
lambdaSurDeux = lambda / 2
facteur = .002 * lambda                                   'facteur d'amplitude du relief.
if pleinEcran then
  largeur = 800
  if relief then hauteur = 650 else hauteur = 600
  maximum = 600: graphique = 550
  else
  largeur = 600: hauteur = 415
  maximum = 415: graphique = 402
end if
if pleinEcran = 0 or acceleration = 0 then
  for j = 0 to images
    for x = 0 to largeur
      for y = hauteur - 50 to hauteur                     'effacer la partie inférieure. 
        gris(j,x,y) = fond
      next
    next
  next
end if
if pleinEcran then graphique = 550 else graphique = 402   'graphique 1D.
xCentre = largeur / 2
yCentre = hauteur / 2
                 
for pixel = 0 to largeur / 2                              'symétrie gauche-droite.
  xCoord = pixel / facteurDeLorentz + .001                'contraction seulement, deltaTemps plus bas.
  xCarre = xCoord ^ 2                                     '  (.001 négligeable, éviter x = 0)  
  for y = 0 to hauteur                                    'pleine hauteur pour relief.
    yCoord = (y - yCentre)                                'centre à la demi-hauteur.
    if relief then yCoord = 2 * yCoord                    'relief vu selon un angle de 60°.
    diagonale = sqr(yCoord * yCoord + xCarre)             'distance réelle selon Pythagore.
    x = 2 * pi * diagonale / lambda
    amplit = sin(x) / x                                   'formule de M. Jocelyn Marcotte.
    amplitude(xCentre + pixel, y) = 4 * amplit
    amplitude(xCentre - pixel, y) = 4 * amplit
  next y
next x
return

Memoriser:'----------------------- MÉMORISER TOUTES LES IMAGES -------------------------------
if image = images then OK = 1
phase2 = image * 2 * pi / images

for x = 0 to largeur
  periode =  deltaTemps * (xCentre - x) * 2 * pi / lambda 'appliquer le temps local (Poincaré).
  for y = 0 to hauteur
    ton = 127 + 127 * amplitude(x,y) * sin(phase2 + periode)
    yCoord = y - facteur * ton + 70 * facteur             'décaler selon la luminance.
    if ton > 255 then ton = 255
    if ton < 0 then ton = 0
    if relief then                                        'effet de relief si désiré.
      if yCoord < 0 then yCoord = 0
      if yCoord < 403  or pleinEcran then pset (x, yCoord), rgb(ton, ton, ton)
      gris(image,x,yCoord) = rgb(ton, ton, ton)           'grand tableau de variables.
      ecart = yCoord - yCoordPrec
      if ecart > 1  then                                  'estomper avec le pixel précédent.
        luminance2 = 127 + 127 * amplitude(x, y - 1) * sin(phase2 + periode)
        ton = 127 + 127 * amplitude(x,y) * sin(phase2 + periode)'ton de départ.
        pas = (luminance2 - ton) / ecart
        for j = 1 to ecart - 1                            'combler les espaces vides.
          ton = ton + pas
          if ton > 255 then ton = 255
          if ton < 0 then ton = 0
          if yCoord - j < 403  or pleinEcran then pset(x - gauche, yCoord - j), rgb(ton, ton, ton)
          gris(image,x,yCoord - j) = rgb(ton, ton, ton)
        next
      end if        
      yCoordPrec = yCoord
    else
      pset(x,y), rgb(ton, ton, ton)
      gris(image,x,y) = rgb(ton, ton, ton)                'tableau de variables sans relief.
    end if
  next
next
periode =  deltaTemps * (xCentre) * 2 * pi / lambda       'graphique 1D.
prec = 22 * amplitude(0,yCentre) * sin(phase2 + periode)
for x = 1 to largeur
  periode =  deltaTemps * (xCentre - x) * 2 * pi / lambda
  yCoord = 22 * amplitude(x,yCentre) * sin(phase2 + periode)
  line(x, graphique - 22 * amplitude(x,yCentre))-(x, graphique + 22 * amplitude(x,yCentre)), blanc
  pset(x, graphique), grisMoyen
  line(x - 1, graphique - prec)-(x, graphique - yCoord), noir
  graph(image,x) = graphique - yCoord
  prec = yCoord
next
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
vert = rgb(0,150,0)
bleu = rgb(0,0,255)
fond = rgb(225,225,225)
blanc= rgb(255,255,255)
grisMoyen = rgb(128, 128,128)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
deuxPi = 2 * pi
rayon = sin(pi / 4)'                                      'rayon fictif pour le noyau central.
'bitmap = 1                                               'séquence bitmap si désiré.
relief = 1                                                'afficher les ondes en relief.
pleinEcran = 0
acceleration = 0
'largeur = 600: hauteur = 398
beta = sin(pi / 4)                                        'vitesse normalisée: c = 1.
lambda = 80
xCentre = largeur / 2
yCentre = hauteur / 2
contraste = 2
luminosite = 255 / contraste
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2
color noir, fond: cls
locate 27, 2: print "L'onde de LaFreniŠre"
locate 27, 58:print "LaFreniere's Wave"
locate 28, 16:print "glafreniere.com"
locate 28, 66:print "Jan. 2006"
if bitmap then gosub Electron: return
line(610,6)-(785,40), blanc, B 
line(611,7)-(786,41), blanc, B 
line(612,8)-(787,42), rgb(255,0,0),BF
locate 1,1
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
locate 30, 2: print "Les ondes stationnaires sph‚riques et concentriques prennent un tout autre aspect si on les soumet"
locate 31, 2: print "aux transformations de Lorentz. Etonnamment, un effet Doppler apparaŒt. Le programme pr‚c‚dent a"
locate 32, 2: print "confirm‚ ces r‚sultats grƒce … l'Ether Virtuel. Mon ® Scanner du Temps ¯ les confirme ‚galement."
locate 33, 2: print "Ces trois d‚monstrations cumul‚es prouvent que cette onde peut exister. Il s'agit d'un ‚lectron."
locate 4, 78: print "selon les transforma-"
locate 5, 78: print "tions de Lorentz."
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
ligne21$ = " A - Avec acc‚l‚ration.": locate 21, 77: print ligne21$
ligne21a$ =" A - Sans acc‚l‚ration."
ligne22a$ =" B - Ondes sans relief."
ligne22$ = " B - Ondes avec relief."
ligne23$ = " C - Pleine fenˆtre.   ": locate 23, 77: print ligne23$
ligne23a$= " C - Fenˆtre normale.  "
locate 22, 77: if relief then print ligne22a$ else print ligne22$
ligne35$ = "    I - Initialiser.    ": locate 35, 39: print ligne35$
ligne36$ = "    Quitter (Echap).    ": locate 36, 39: print ligne36$
ligne37$ = "                        "
gosub flecheGauche: gosub flecheDroite
locate 25, 78: print "Pour modifier la lon-"
locate 26, 78: print "gueur d'onde, appuyez"
locate 27, 78: print "sur [ + ] ou [ - ] :"
color vert
locate 35
locate , 2: print "Mille mercis … M. Anselme Dewavrin"
locate , 2: print "ainsi qu'aux cr‚ateurs de FreeBASIC."
locate , 2: print "Gabriel LaFreniŠre.  glafreniere.com";
locate 35
locate , 66: print "Mise … jour le 29 nov. 2006."
locate , 66: print "Ce programme peut ˆtre distribu‚,"
locate , 66: print "copi‚ ou modifi‚ librement.";
color noir
pcopy 2, 0
pcopy 2, 1
gosub Electron
return
