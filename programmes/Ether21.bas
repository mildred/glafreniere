screen 19,24,3: images = 24: largeur = 799: hauteur = 213 'créé le 19 mars 2007.
dim as single pi, amplitude, amplitude1, amplitude2
dim as single t, temps, xOrign, xPrime, tPrime, distance
dim as single xCarre, yCarre, Doppler, beta, phase, gLorentz
dim image(1000), horloge(2000), decalage(2000)
beta = .5: page2 = 1: choix$ = "A": lambda = 100          'longueur d'onde en pixels.
gosub Initialisation

do'--------------------------------- BOUCLE PRINCIPALE ----------------------------------------
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  select case choix$
    case "A": if choixA = 0 then gosub Choix_A
    case "B": if choixB = 0 then gosub Choix_B
  end select
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.

'------------------------------------- SAISIE CLAVIER -----------------------------------------
  saisie$ = inkey
  if len(saisie$) then
'   if saisie$ = Chr(255)+"k" then end                    'à savoir: bouton de fermeture.
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    select case saisie$
      case "A": choix$ = "A"                              'formules de Lorentz originales.
      case "B": choix$ = "B"                              'formules de Lorentz inversées.
      case "X+", "k+",chr$(27): end                       'varie selon le compilateur (!?)
      case "I": lambda = 100: beta = .5: choix$ = "A"     'tout initialiser.
      case "P": sleep: saisie$ = ""                       'pause.
      case "R": if choix$ = "A" then                      'reculer.
                  xImpression -= 2
                  if xImpression < -gLorentz * xCentre then xImpression = gLorentz * xCentre - 2
                else
                  xImpression += 2
                  if xImpression > gLorentz * xCentre then xImpression = -gLorentz * xCentre + 2
                end if
                saisie$ = ""
      case "T": if choix$ = "A" then choixA = 1 else choixB = 1
                screenset 2: locate 33, 2
                print "                      "            'effacer « pause » et « reculer ».
                saisie$ = "": temps = 0: xScanner = 0: gosub CalculRapide
      case "M": run "Ether00.exe"                         'menu principal.
      case "K+":run "Ether20.exe"                         'flèche gauche.
      case "M+":run "Ether00.exe"                         'flèche droite.
      case "+": lambda = lambda + 10: if lambda > 200 then lambda = 200
      case "-": lambda = lambda - 10: if lambda < 60 then lambda = 60
      case "0": beta =  0
      case "1": beta = .1
      case "2": beta = .2
      case "3": beta = .3
      case "4": beta = .4
      case "5": beta = .5
      case "6": beta = .6
      case "7": beta = sin(pi / 4)                        '.707: angle thêta de 45° exactement.
      case "8": beta = .8
      case "9": beta = .866                               '.866: contraction 1/2 exactement.
      case else:saisie$ = ""
    end select
    do: loop while len(inkey)                             'vider le tampon.
    if len(saisie$) then gosub Initialisation
  end if
'-------------------------------------- SAISIE SOURIS -----------------------------------------

  getmouse xSouris, ySouris, , clic
  if ySouris > 0 and ySouris < 509 then gosub AfficherTemps
  ligne = .5 + ySouris / 16
  if ligne > 32 and ligne < 38 then
    if xSouris < 320 then ligne = 0
    if ligne > 35 and xSouris > 480 then ligne = 0
  else ligne = 0
  end if

'--------------------------------- REHAUSSER L'AFFICHAGE --------------------------------------
  color noir, turquoise
  locate ligne, 41
  select case ligne
    case 33: if not choix$ = "A" then print ligne33$      'choix en cours déjà affiché en bleu.
    case 34: if not choix$ = "B" then print ligne34$
    case 35: print ligne35$      
    case 36: print ligne36$
    case 37: print ligne37$;: if xSouris < 400 then gosub FlecheGauche else gosub FlecheDroite
  end select
  color noir, fond
'--------------------------------- ACTIONS SUITE À UN CLIC ------------------------------------
  if clic = 1 then
    clic = 0: bitmap = 0: afficher = 0
    select case ligne
      case 33: choix$ = "A": gosub Initialisation
      case 34: choix$ = "B": gosub Initialisation
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether19.exe" else run "Ether00.exe"
    end select
  end if
loop
'------------------------------- FIN DE LA BOUCLE PRINCIPALE ----------------------------------

'--------------------------------- POINTEUR SUR LES IMAGES -------------------------------------
AfficherTemps:
if choix$ = "A" and choixA = 0 then
  if clic = 1 then
    xImpression = gLorentz * (xSouris - xCentre)
    if xImpression < -400 * gLorentz then xImpression = -400 * gLorentz
    if xImpression > gLorentz * 400 then xImpression = gLorentz * 400
  end if
  return
end if
if choix$ = "B" and choixB = 0 then
  if clic = 1 then
    xImpression = gLorentz * (xSouris - xCentre)
    if xImpression < -400 * gLorentz then xImpression = -400 * gLorentz
    if xImpression > gLorentz * 400 then xImpression = gLorentz * 400
  end if
  return
end if

if ySouris > hauteur and ySouris < 296 then gosub AfficherTransformations: return' pointeur sur l'échelle.
line(xSouris, hauteur)-(xSouris, hauteur + 28), noir
if xSouris < 55 then
  xC = 55
elseif xSouris > 744 then xC = 744
else xC = xSouris
end if
color , blanc
line(xC-55,hauteur-75)-(xC+55, hauteur-1), blanc, bf      'boîte de texte.
locate 10,(xC-39) / 8  : print "Cliquez pour"
locate 11,(xC-39) / 8  : print "fixer le"
locate 12,(xC-39) / 8  : print "temps t."
locate 13,(xC-39) / 8+2: print using " ###.###"; (xSouris - xCentre) / lambda
locate 13,(xC-39) / 8   : print "t =";
line(xC-55,hauteur-75)-(xC+55, hauteur-1), noir, b        'encadré.
color , fond
if clic = 1 then
' t = x car le temps peut être exprimé aussi bien en secondes qu'en périodes d'ondes:
  temps = (xSouris - xCentre) / lambda                    'xSouris - xCentre = x en pixels.
  if abs(temps) < .001 then temps = 0                     'éviter un problème d'affichage.
  if xSouris = xPrecedent then return                     'éviter de répéter les mêmes calculs.
  xPrecedent = xSouris
  if choix$ = "A" and choixA = 0 then choixA = 1
  if choix$ = "B" and choixB = 0 then choixB = 1
  gosub CalculRapide
end if
t = xSouris - xCentre                                     'afficher position et contraction.
circle(xCentre + t * beta, yCentre + yHaut), lambda-1, blanc,,, 1 / gLorentz
circle(xCentre + t * beta, yCentre + yHaut), lambda+1, blanc,,, 1 / gLorentz
circle(xCentre + t * beta, yCentre + yHaut), lambda,   noir,,,  1 / gLorentz
line(xCentre+t*beta, yCentre+yHaut-lambda)-(xCentre+t*beta, yCentre+yHaut+lambda),noir
line(xCentre+t*beta-gLorentz*lambda, yCentre+yHaut)-(xCentre+t*beta+gLorentz*lambda, yCentre+yHaut),noir
return

'----------------------------- POINTEUR SUR L'ÉCHELLE GRADUÉE ----------------------------------
AfficherTransformations:
line(0,0)-(799, hauteur), fond, bf                        'effacer la partie supérieure.
line(0,yHaut)-(799, yBas), fond, bf                       'effacer la partie inférieure.
line(-1,hauteur)-(800, hauteur+83), noir, b               'délimiter la zone de l'échelle.
line(xCentre - 175, 6)-(xCentre + 175, 7), noir, b
locate 1, 35:print " LES TRANSFORMATIONS DE LORENTZ "
color noir, fond
locate 11,2: print "v ="; v;
if beta > 0 then print " kilomŠtres par seconde."; else print " kilomŠtre par seconde.";
locate,89: print using "t = #.###"; temps
locate, 2: print using "Vitesse normalis‚e bˆta = v / c = #.###"; beta;
locate,86: print using "bˆta = #.###"; beta
locate, 2: print "Facteur g = sqr(1 - bˆta ^ 2) = ";: print using "#.###"; gLorentz;
locate,89: print using "g = #.###"; gLorentz
locate 2
locate,2: print "Pour rendre compte de la contraction selon le facteur g de Lorentz, de la translation selon v * t,"
locate,2: print "du ralentissement des horloges selon le facteur g et enfin du d‚calage horaire selon -beta * x, il"
locate,2: print "faut permuter les variables x, x' et t, t' des ‚quations de Lorentz de la maniŠre suivante:":?
locate,2: print "Contraction/translation: x'=(x-v*t)/sqr(1-(v/c)^2)    "
locate,2: print "       Heure locale: t'=(t-v*x/c^2)/sqr(1-(v/c)^2)    "
locate,2: print "   Choix B, avec permutation des variables x et x':"
locate,2: print "                                       ... t et t':"
x1 = 438: x2 = 788                                        'souligner les équations en cours.
if choix$ = "A" then
  locate 8,57: print "x' = g * x + bˆta * t    secondes lumiŠre."
  locate 9,57: print "t' = g * t - bˆta * x    secondes lentes."
  color, blanc
  y1 = 78: y2 = 112
  line(x1,y1)-(x2,y2), noir, b
  line(x1+1,y1+1)-(x2-1,y2-2), blanc, bf
  locate 6,57: print "x' = (x - bˆta * t) / g  secondes lumiŠre."
  locate 7,57: print "t' = (t - bˆta * x) / g  secondes lentes."
else
  locate 6,57: print "x' = (x - bˆta * t) / g  secondes lumiŠre."
  locate 7,57: print "t' = (t - bˆta * x) / g  secondes lentes."
  color, blanc
  y1 = 111: y2 = 145
  line(x1,y1)-(x2,y2), noir, b
  line(x1+1,y1+1)-(x2-1,y2-2), blanc, bf
  locate 8,57: print "x' = g * x + bˆta * t    secondes lumiŠre."
  locate 9,57: print "t' = g * t - bˆta * x    secondes lentes."
end if
if choix$ = "B" then color, blanc else color, fond
color, fond
locate 22
locate,2: print "Le temps t' concerne les secondes lentes que les horloges mobiles affichent r‚ellement, ou que"
locate,2: print "les horloges au repos semblent afficher vues du r‚f‚rentiel mobile. Le choix B est pr‚f‚rable car"
locate,2: print "toutes les variables sont exprim‚es en grandeurs r‚elles, mais pas n‚cessairement exactes.":?
locate,2: print "Ce programme montre qu'on peut avantageusement exprimer les variables x et x' en longueurs"
locate,2: print "d'onde, et les variables t et t' en p‚riodes d'onde. De cette maniŠre, toutes les variables"
locate,2: print "repr‚sentent des grandeurs exactes, et alors la Relativit‚ de Lorentz prend tout son sens. Il"
locate,2: print "faut rejeter l'id‚e absurde d'un ® espace-temps ¯ qui se transforme. Il est bien plus simple de"
locate,2: print "consid‚rer que c'est plut“t la longueur d'onde et la p‚riode des ‚lectrons qui se transforme."
locate,2: print "C'est pourquoi la matiŠre, dont la structure d‚pend des ‚lectrons, doit elle aussi se transformer."
line(xSouris, hauteur)-(xSouris, hauteur + 28), noir      'curseur mobile du haut.
if xSouris < 55 then
  xC = 55
elseif xSouris > 744 then xC = 744
else xC = xSouris                                         'afficher selon la souris.
end if
color , blanc
if choix$ = "A" then
  xOrign = (xSouris - xCentre) / lambda                   'x selon la souris.
  xPrime = (xOrign - beta * temps) / gLorentz             'équations originales de Lorentz.
  tPrime = (temps - beta * xOrign) / gLorentz             'équations originales de Lorentz.
  if abs(xPrime) < .001 then xPrime = 0'                  'régler un problème d'affichage.
  if abs(tPrime) < .001 then tPrime = 0
  xC = xCentre + xPrime * lambda
  line(xC, hauteur)-(xC, hauteur + 28), noir              'curseur mobile du haut.
  if xC < 55 then xC = 55 else if xC > 744 then xC = 744
  line(xC - 55,hauteur - 45)-(xC + 55, hauteur), blanc, bf'boîte de texte.
  locate 12,(xC - 31) / 8 + 2: print using " ###.###"; xPrime
  locate 12,(xC - 31) / 8    : print "x'=";
  locate 13,(xC - 31) / 8 + 2: print using " ###.###"; tPrime
  locate 13,(xC - 31) / 8    : print "t'=";
  line(xC - 55,hauteur - 45)-(xC + 55, hauteur), noir, b  'encadré.
  xC = xSouris
  line(xC, hauteur+2)-(xC, hauteur+100), noir             'curseur mobile du bas.
  if xC < 55 then xC = 55
  if xC > 744 then xC = 744
  line(xC-55, yHaut)-(xC+55, yHaut + 42), blanc, bf       'boîte de texte.
  locate 20, (xC - 31) / 8 + 2: print using " ###.###"; xOrign
  locate 20, (xC - 31) / 8:     print "x =";
  locate 21, (xC - 31) / 8 + 2: print using " ###.###"; temps
  locate 21, (xC - 31) / 8:     print "t =";
  line(xC-55, yHaut)-(xC+55, yHaut + 42), noir, b         'encadré.
else
  xOrign = (xSouris - xCentre) / lambda                   ' x  selon la souris.
  xPrime = gLorentz * xOrign + beta * temps               ' x' selon les équations inversées.
  tPrime = gLorentz * temps - beta * xOrign               ' t' selon les équations inversées.
  line(xC - 55,hauteur - 45)-(xC + 55, hauteur), blanc, bf' boîte de texte.
  locate 12,(xC - 31) / 8 + 2: print using " ###.###"; xOrign
  locate 12,(xC - 31) / 8    : print "x =";
  locate 13,(xC - 31) / 8 + 2: print using " ###.###"; temps
  locate 13,(xC - 31) / 8    : print "t =";
  line(xC - 55,hauteur - 45)-(xC + 55, hauteur), noir, b  'encadré.
  if abs(xPrime) < .001 then xPrime = 0'                  'régler un problème d'affichage.
  if abs(tPrime) < .001 then tPrime = 0
  xC = xCentre + xPrime * lambda
  line(xC, hauteur+2)-(xC, hauteur+100), noir             'curseur mobile du bas.
  if xC < 55 then xC = 55
  if xC > 744 then xC = 744
  line(xC-55, yHaut)-(xC+55, yHaut + 42), blanc, bf       'boîte de texte.
  locate 20, (xC - 31) / 8 + 2: print using " ###.###"; xPrime
  locate 20, (xC - 31) / 8:     print "x'=";
  locate 21, (xC - 31) / 8 + 2: print using " ###.###"; tPrime
  locate 21, (xC - 31) / 8:     print "t'=";
  line(xC-55, yHaut)-(xC+55, yHaut + 42), noir, b         'encadré.
end if
color , fond
return

Bitmaps:'-------------------------- Créer une séquence bitmap ---------------------------------
select case capture
  case is < 10: numero$ = "00"
  case is < 100: numero$ = "0"
  case is < 1000: numero$ = ""
end select
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
color rgb(255,255,255), rgb(255,0,0)                      'signaler la capture d'images.
locate 34, 79: print fichier$; " "; bitmap
bsave fichier$,0
color noir, fond
capture = capture + 1
if capture > 600 then end
return

CalculRapide:
'------------------------------- DIAGRAMME DU HAUT, NORMAL ------------------------------------   
screenset 2, page2                                        'afficher sur la page matrice.
for x = 0 to xCentre                                      'côtés symétriques.
  xCarre = x * x                                          ' x' au carré, Pythagore ci-dessous.
  for y = 0 to yCentre                                    'côtés symétriques.
    yCarre = y * y
    distance = sqr(xCarre + yCarre)
    phase = 2 * pi * distance / lambda
    amplitude  = sin(phase - temps * 2 * pi)
    if distance > lambda then amplitude = amplitude / (distance / lambda)
    if amplitude > 0 then
      rouge = amplitude * 600
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    else
      vert = -amplitude * 600
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    end if
    if rouge > 255 then rouge = 255
    if vert  > 255 then vert  = 255
    if bleu  > 255 then bleu  = 255
    pset(xCentre + x, yCentre + y), rgb(rouge,vert,bleu)
    pset(xCentre - x, yCentre + y), rgb(rouge,vert,bleu)
    pset(xCentre + x, yCentre - y), rgb(rouge,vert,bleu)
    pset(xCentre - x, yCentre - y), rgb(rouge,vert,bleu)
  next
next
'---------------------------- DIAGRAMME DU BAS, EFFET DOPPLER ---------------------------------   
line(0,295)-(799,509), fond, bf                           'effacer l'image précédente.
for x = -xCentre to xCentre                               'pas de symétrie horizontale.
  xOrign = x / lambda                                     ' ÉQUATIONS INVERSÉES:
  xPrime = gLorentz * xOrign + beta * temps               ' x et x' en longueurs d'onde.
  tPrime = gLorentz * temps - beta * xOrign               ' t et t' en périodes d'onde.
  xCarre = xOrign * xOrign                                'x au carré, Pythagore ci-dessous.
  for y = 0 to yCentre                                    'symétrie verticale.
    yCarre = (y / lambda) * (y / lambda)
    distance = sqr(xCarre + yCarre)
    phase = 2 * pi * distance                             'phase selon la distance.
    amplitude  = sin(phase - tPrime * 2 * pi)             'conversion de t' en radians.
    if distance > 1 then amplitude = amplitude / distance
    if amplitude > 0 then                                 'traduire l'amplitude en couleurs.
      rouge = amplitude * 600
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    else
      vert = -amplitude * 600
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    end if
    if rouge > 255 then rouge = 255
    if vert  > 255 then vert  = 255
    if bleu  > 255 then bleu  = 255                       ' y' = y selon Lorentz:
    pset(xCentre + xPrime * lambda, yCentre + y + yHaut), rgb(rouge,vert,bleu)
    pset(xCentre + xPrime * lambda, yCentre - y + yHaut), rgb(rouge,vert,bleu)
  next
next

locate 15, 2: print using "t (2 pi) = ##.### sec"; temps;: print "."
line(0,hauteur+44)-(799, hauteur+81), fond, bf            'effacer l'échelle précédente.
locate 17, 2:  print "t' ="
for x = -2 * lambda to 2 * lambda step lambda / 10        'nouvelle échelle graduée.
  xOrign = x / lambda                                     '  *** ÉQUATIONS INVERSÉES: ***
  xPrime = gLorentz * xOrign + beta * temps               ' x et x' en longueurs d'onde.
  tPrime = gLorentz * temps - beta * xOrign               ' t et t' en périodes d'onde.
  xP = xPrime * lambda                                    ' x' en pixels.
  if x mod lambda then                                    'barres des décimales:
    line(xCentre + xP, hauteur + 66)-(xCentre + xP, hauteur + 81), noir
  else                                                    'barres des entiers:
    line(xCentre + xP-1,hauteur+61)-(xCentre + xP+1, hauteur+81), noir, bf
    locate 15, 12: print using "###.###"; tPrime          'afficher le temps t'.
    if tPrime > 0 then locate 15, 13: print "+"
    get(97,225)-(143,238), horloge
    if choix$ = "A" then
      put(xCentre + x - 26, 256), horloge,  pset
    else put(xCentre + xP - 26, 260), horloge,  pset
    end if
  end if
next
locate 15, 2: print using "t (2 pi) = ##.### sec"; temps;: print "."
if temps > 0 then locate 15, 13: print "+"

pcopy 2, page1
screenset page1, page2
return

Choix_A:'------------------------ BALAYAGE VERS LA DROITE -------------------------------------

' Le principe du Scanner du Temps est simple: il s'agit de balayer une scène mobile en suivant
' exactement l'onde de phase, dont la vitesse exprimée en secondes lumière par seconde, ou
' mieux en longueurs d'ondes par période, est égale à 1 / bêta. Il en ressort qu'à l'endroit
' où se trouve le Scanner, le temps t' DEMEURE À ZÉRO. C'est pourquoi le temps t dépend de
' la position x du Scanner (xScanner = xImpression / gLorentz) exprimée en longueurs d'onde
' comparativement à bêta:

xScanner = xImpression / gLorentz                         'position du Scanner, nombre entier.
temps = (xImpression / gLorentz) * beta / lambda          'temps selon la position du Scanner.

' xImpression est l'endroit où il faut imprimer la portion d'image balayée par le Scanner. 
' Les variables xOrign ci-dessous représentent les abscisses du système AU REPOS, les
' variables xPrime étant celles du système MOBILE. Rappelons que les distances peuvent être
' exprimées aussi bien en secondes lumière qu'en longueurs d'onde, alors que le temps peut
' être exprimé aussi bien en secondes qu'en périodes d'ondes. Dans ce dernier cas, les
' unités de temps doivent être converties en radians : période de l'onde = t * 2 * pi.
' Toutefois, les ÉQUATIONS ORIGINALES de Lorentz avaient pour but de corriger l'effet
' Doppler d'un système mobile, aux fins de rendre les équations de Maxwell invariantes.
' Pour obtenir l'effet contraire, il faut permuter les variables x et x':

' x' = (x - bêta * t) / g   en longueurs d'onde ou en secondes lumière.
' t' = (t - bêta * x) / g   en secondes lentes.

' x' = g * x + bêta * t     en longueurs d'onde ou en secondes lumière.
' t' = g * t - bêta * x     en secondes lentes.

' Étonnamment, la variable x' des équations originales de Lorentz concerne donc le
' référentiel AU REPOS DANS L'ÉTHER. C'est pourquoi le choix "A" montre, mais seulement à
' l'étape qui suit le balayage, les transformations SELON LES FORMULES ORIGINALES de Lorentz.

xOrign = 0                                                ' x en longueurs d'onde = 0.
xPrime = gLorentz * xOrign + beta * temps                 ' x' en longueurs d'onde.
tPrime = gLorentz * temps  - beta * xOrign                ' t' en périodes d'onde.
centre = xCentre + xPrime * lambda                        ' centre du système mobile.

for pixel = gLorentz * -xCentre to gLorentz * xCentre     ' x en pixels, dans un espace limité.
  xOrign = pixel / lambda                                 ' x en longueurs d'onde.
  xCarre = xOrign * xOrign                                ' x au carré, Pythagore ci-dessous.
  xPrime = gLorentz * xOrign + beta * temps               ' x' en longueurs d'onde.
  tPrime = gLorentz * temps  - beta * xOrign              ' t' en périodes d'onde.
  xPrimePixel = xCentre + xPrime * lambda                 ' x' en pixels (entier).
  if pixel = xImpression then
    xCopier = xPrimePixel: xColler = xCentre + pixel
  else
    if pixel = xImpression + 1 then goto CourtCircuit     'sauvegarder les pixels d'impression.
  end if
  
  for y = 0 to yCentre                                    'symétrie verticale.
    yCarre = (y / lambda) * (y / lambda)
    distance = sqr(xCarre + yCarre)
    phase = 2 * pi * distance                             'phase selon la distance.
    amplitude  = sin(phase - tPrime * 2 * pi)             'conversion de t' en radians.
    if distance > 1 then amplitude = amplitude / distance
    if amplitude > 0 then                                 'traduire l'amplitude en couleurs.
      rouge = amplitude * 600
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    else
      vert = -amplitude * 600
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    end if
    if rouge > 255 then rouge = 255
    if vert  > 255 then vert  = 255
    if bleu  > 255 then bleu  = 255
    pset(xPrimePixel, yCentre + y + yHaut), rgb(rouge,vert,bleu) 'selon Lorentz: y' = y
    pset(xPrimePixel, yCentre - y + yHaut), rgb(rouge,vert,bleu)
  next
CourtCircuit:
next

circle(centre, yCentre + yHaut), 100 + 3, 1,,, 1 / gLorentz' cercle bleu central.
circle(centre, yCentre + yHaut), 100 - 3, 1,,, 1 / gLorentz
paint(centre,  yCentre + yHaut + 100), rgb(50,50,255), 1
circle(centre, yCentre + yHaut), 100 + 3, rgb(50,50,255),,, 1 / gLorentz
circle(centre, yCentre + yHaut), 100 - 3, rgb(50,50,255),,, 1 / gLorentz

for pixel = -2 * gLorentz * lambda to 2 * gLorentz * lambda
  phase = 2 * pi * pixel / lambda / gLorentz              'enveloppe des ondes stationnaires.
  amplitude = 21 * sin(phase)
  line(centre+pixel,yHaut+21-amplitude)-(centre+pixel,yHaut+21+amplitude),blanc
  pset(centre+pixel,yHaut+21-amplitude), noir
  pset(centre+pixel,yHaut+21+amplitude), noir
  xOrign =  pixel / lambda / gLorentz                     ' x en longueurs d'onde.
  tPrime = gLorentz * temps  - beta * xOrign              ' t' en périodes d'onde.
  phase = 2 * pi * pixel / lambda / gLorentz
  amplitude1 = 10 * sin(phase + tPrime * 2 * pi)
  line(centre+pixel-1,yHaut+21-amplitude1)-(centre+pixel,yHaut+22-amplitude1),noir,b
  Doppler = lambda * (1 - beta) / gLorentz
  phase = 2 * pi * pixel / lambda / gLorentz
  amplitude2 = 10 * sin(phase - tPrime * 2 * pi)
  line(centre+pixel-1,yHaut+21-amplitude2)-(centre+pixel,yHaut+22-amplitude2),noir,b
next

circle(xCentre-centre, yCentre + yHaut), 50,   1,,, 1 / gLorentz 'cercle de référence mobile.
paint(xCentre-centre+gLorentz*40, yCentre + yHaut), blanc, 1
circle(xCentre-centre, yCentre + yHaut), 50, blanc,,, 1 / gLorentz
circle(xCentre-centre, yCentre + yHaut), 45, noir,,, 1 / gLorentz
paint(xCentre-centre+gLorentz*40, yCentre + yHaut), noir, noir

circle(largeur-lambda, yCentre + yHaut), 50, 1            'cercle de référence fixe.
paint(largeur-lambda, yCentre + yHaut), blanc,1
circle(largeur-lambda, yCentre + yHaut), 50, blanc
circle(largeur-lambda, yCentre + yHaut), 45, noir
paint(largeur-lambda, yCentre + yHaut), noir,noir

get(xCopier, yHaut)-(xCopier, yBas), image                'une ligne de balayage à la fois.
screenset 2
put(xColler, 0), image, pset
screenset page1

for x = -2 * lambda to 2 * lambda step lambda / 10        'échelle contractée selon Lorentz.
  xC = xCentre + x * gLorentz + temps * beta * lambda
  if x mod lambda then                                    'barres des décimales.
    line(xC, hauteur+66)-(xC,hauteur+81), noir
  else                                                    'barres des entiers.
    line(xC-1,hauteur+61)-(xC+1, hauteur+81), noir, bf
  end if
next

xC = xCentre + temps * beta * lambda                      'centre du référentiel mobile.
t = gLorentz * temps'                                     't' au centre, en secondes lentes.
if abs(t) < .001 then t = 0

locate 15, 12: print using "###.###"; t                   'afficher le temps t' du centre.
if temps > 0 then locate 15, 13: print "+"
get(97,225)-(143,238), horloge
put(xC - 26, 257), horloge, pset

tPrime = t + 2 * beta                                     'décalage horaire pour -2 * x
if abs(tPrime) < .001 then tPrime = 0                     'régler un problème d'affichage.
locate 15, 12: print using "###.###"; tPrime
if tPrime > 0 then locate 15, 13: print "+"
get(97,225)-(143,238), horloge
put(xC - gLorentz * 2 * lambda - 26, 257), horloge, pset

tPrime = t + beta                                         'décalage horaire pour -1 * x
if abs(tPrime) < .001 then tPrime = 0
locate 15, 12: print using "###.###"; tPrime
if tPrime > 0 then locate 15, 13: print "+"
get(97,225)-(143,238), horloge
put(xC - gLorentz * lambda - 26, 257), horloge, pset

tPrime = t - beta                                         'décalage horaire pour +1 * x
if abs(tPrime) < .001 then tPrime = 0
locate 15, 12: print using "###.###"; tPrime
if tPrime > 0 then locate 15, 13: print "+"
get(97,225)-(143,238), horloge
put(xC + gLorentz * lambda - 26, 257), horloge, pset

tPrime = t - 2 * beta                                     'décalage horaire pour +2 * x
if abs(tPrime) < .001 then tPrime = 0
locate 15, 12: print using "###.###"; tPrime
if tPrime > 0 then locate 15, 13: print "+"
get(97,225)-(143,238), horloge
put(xC + gLorentz * 2 * lambda - 26, 257), horloge, pset

if abs(temps) < .001 then temps = 0
locate 15, 12: print using "###.###"; temps               'afficher le temps t en cours.
if tPrime > 0 then locate 15, 13: print "+"
locate 17, 2:  print "t' ="
line(xCentre + xImpression, 0)-(xCentre + xImpression, hauteur), blanc
line(xCentre + xImpression+1, 0)-(xCentre + xImpression+1, hauteur), noir
line(xCentre + xImpression, hauteur+1)-(xCentre + xImpression, hauteur+40), noir
line(xCentre + xScanner, hauteur+42)-(xCentre + xScanner, hauteur+82), noir
line(xCentre + xScanner, yHaut)-(xCentre + xScanner, yBas), blanc
line(xCentre + xScanner+1, yHaut)-(xCentre + xScanner+1, yBas), noir
xImpression += 1                                          'un pixel à la fois.
if xImpression > gLorentz * xCentre then                  'fin du balayage.
  screenset 2
  line(xImpression + xCentre, 0)-(799, hauteur), fond, bf 'effacer le reste du texte.
  screenset page1
  xImpression = -gLorentz * xCentre
end if
return

Choix_B:'------------------------- BALAYAGE VERS LA GAUCHE ------------------------------------
xScanner = xImpression / gLorentz                         'corriger la contraction selon g.
xCarre = (xImpression / gLorentz)*(xImpression / gLorentz)
temps = xImpression * -beta / lambda / gLorentz
centre = temps * lambda * beta                            'centre des systèmes mobiles.

'--------------------------- ÉMETTEUR SANS EFFET DOPPLER (HAUT) -------------------------------
for pixel = xCentre to -xCentre step -1
  if pixel = xScanner then
    xPrime = xImpression / gLorentz                       'xImpression est un entier.
    xCarre = xPrime * xPrime
    else xCarre = pixel * pixel
  end if

  for y = 0 to yCentre                                    'symétrie verticale.
    yCarre = y * y
    distance = sqr(xCarre + yCarre) / lambda              'distance en longueurs d'onde.
    phase = 2 * pi * distance
    if distance > 1 then
      amplitude1 = sin(phase - temps * 2 * pi) / distance
      else amplitude1 = sin(phase - temps * 2 * pi)
    end if
    if amplitude1 > 0 then
      rouge = amplitude1 * 600
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    else
      vert = -amplitude1 * 600
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    end if
    if rouge > 255 then rouge = 255
    if vert  > 255 then vert  = 255
    if bleu  > 255 then bleu  = 255
    pset(xCentre + pixel, yCentre + y), rgb(rouge,vert,bleu)
    pset(xCentre + pixel, yCentre - y), rgb(rouge,vert,bleu)
  next

next

  
circle(xCentre, yCentre), 100 + 3, 1                      'cercle bleu central.
circle(xCentre, yCentre), 100 - 3, 1
paint(xCentre,  yCentre + 100), rgb(50,50,255), 1
circle(xCentre, yCentre), 100 + 3, rgb(50,50,255)
circle(xCentre, yCentre), 100 - 3, rgb(50,50,255)

for pixel = -2 * lambda to 2 * lambda
  phase = 2 * pi * pixel / lambda                         'enveloppe des ondes stationnaires.
  amplitude = 21 * sin(phase)
  line(xCentre + pixel, 20-amplitude)-(xCentre + pixel, 20+amplitude), blanc
  pset(xCentre + pixel, 20-amplitude), noir
  pset(xCentre + pixel, 20+amplitude), noir
  distance = pixel / lambda                               'distance en longueurs d'onde.
  phase = 2 * pi * distance
  amplitude1 = 10 * sin(phase - temps * 2 * pi)
  pset(xCentre + pixel, 20-amplitude1), noir              'ondes progressives, gauche.
  amplitude2 = 10 * sin(phase + temps * 2 * pi)
  pset(xCentre + pixel, 20-amplitude2), noir              'ondes progressives, droite.
  amplitude = amplitude1 + amplitude2                     'ondes stationnaires:
  line(xCentre + pixel-1, 20-amplitude)-(xCentre + pixel, 20-amplitude+1), noir, b
next

circle(largeur-30-centre-150/gLorentz, yCentre), 50, 1,,, 1 / gLorentz'cercle de référence mobile.
circle(largeur-30-centre-150/gLorentz, yCentre), 40, 1,,, 1 / gLorentz
paint (largeur-30-centre-150/gLorentz, yCentre-45), blanc, 1
paint (largeur-30-centre-150/gLorentz, yCentre), noir, 1
circle(largeur-30-centre-150/gLorentz, yCentre), 50, blanc,,, 1 / gLorentz

circle(200+centre-100/gLorentz, yCentre), 50, 1,,, 1 / gLorentz 'cercle de référence mobile.
circle(200+centre-100/gLorentz, yCentre), 40, 1,,, 1 / gLorentz
paint (200+centre-100/gLorentz+gLorentz*45, yCentre), blanc, 1
paint (200+centre-100/gLorentz+gLorentz*35, yCentre), noir,1
circle(200+centre-100/gLorentz, yCentre), 50, blanc,,, 1 / gLorentz
                                                          
get(xCentre+xScanner,0)-(xCentre+xScanner,hauteur),image  'une ligne de balayage à la fois.
screenset 2                                               'imprimer sur la page matrice.
put(xCentre+xImpression,yHaut),image,pset
screenset page1

temps = xImpression * -beta / lambda / gLorentz
if abs(temps) < .001 then temps = 0                       'régler un problème d'affichage.

locate 15, 12: print using "###.###"; temps               'afficher le temps t.
if temps > 0 then locate 15, 13: print "+"
get(97,225)-(143,238), horloge                            'reporter sur horloges du temps t'.
if xScanner > 2 * lambda then
  put(xCentre + gLorentz * 2 * lambda - 26, 257), horloge, pset
else
  locate 15, 12: print using "###.###"; 2 * beta          'afficher le décalage horaire.
  locate 15, 13: print "-"
  get(97,225)-(143,238), decalage
  put(xCentre + gLorentz * 2 * lambda - 26, 257), decalage, pset  
end if
if xScanner > lambda then
  put(xCentre + gLorentz * lambda - 26, 257), horloge, pset
else
  locate 15, 12: print using "###.###"; beta
  locate 15, 13: print "-"
  get(97,225)-(143,238), decalage
  put(xCentre + gLorentz * lambda - 26, 257), decalage, pset  
end if
if xScanner > 0 then
  put(xCentre - 26, 257), horloge, pset
else
  locate 15, 12: print using "###.###"; 0
  locate 15, 13: print " "
  get(97,225)-(143,238), decalage
  put(xCentre - 26, 257), decalage, pset  
end if
if xScanner > -lambda then
  put(xCentre - gLorentz * lambda - 26, 257), horloge, pset
else
  locate 15, 12: print using "###.###"; beta
  locate 15, 13: print "+"
  get(97,225)-(143,238), decalage
  put(xCentre - gLorentz * lambda - 26, 257), decalage, pset  
end if
if xScanner > -2 * lambda then
  put(xCentre - gLorentz * 2 * lambda - 26, 257), horloge, pset
else
  locate 15, 12: print using "###.###"; 2 * beta
  locate 15, 13: print "+"
  get(97,225)-(143,238), decalage
  put(xCentre - gLorentz * 2 * lambda - 26, 257), decalage, pset  
end if

locate 15, 12: print using "###.###"; temps               'afficher finalement le temps t.
if temps > 0 then locate 15, 13: print "+"
xImpression -= 1
if xImpression / gLorentz < -xCentre then                 'balayage achevé.
  screenset 2
  line(0,295)-(xImpression + xCentre,509), fond, bf       'effacer le reste du texte.
  screenset page1
  xImpression = gLorentz * xCentre
end if

line(xCentre + xScanner, hauteur+1)-(xCentre + xScanner, hauteur+40), noir
line(xCentre + xScanner, 0)-(xCentre + xScanner, hauteur), blanc
line(xCentre + xImpression, hauteur+42)-(xCentre + xImpression, hauteur+82), noir
line(xCentre + xImpression, yHaut)-(xCentre + xImpression, yBas), blanc
return

'----------------------------------- DESSIN DES FLÈCHES ---------------------------------------
FlecheDroite:
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
FlecheGauche:
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

'----------------------------------------------------------------------------------------------
Initialisation:'---------------------- INITIALISATION -----------------------------------------
'----------------------------------------------------------------------------------------------
fond = rgb(225,225,225)                                   'définir les couleurs.
blanc= rgb(255,255,255)
turquoise = rgb (230, 255, 255)
c = 299792458                                             'c en mètres par seconde.
v = beta * c / 1000                                       'v en kilomètres par seconde.
pi = 4 * atn(1)
gLorentz = sqr(1 - beta^2)                                'contraction selon Lorentz.
xCentre = largeur / 2
yCentre = hauteur / 2
yHaut = hauteur + 83
yBas = yHaut + hauteur
choixA = 0: choixB = 0                                    'code 0: balayage à effectuer.
'bitmap = 1                                               'séquence bitmap si désiré.
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
windowtitle "Ether21  -  Le Scanner du Temps et les transformations de Lorentz."
ligne33$ = "  A- Corriger l'effet Doppler.  ": locate 33, 41: print ligne33$
ligne34$ = "  B- Provoquer l'effet Doppler. ": locate 34, 41: print ligne34$
ligne35$ = "  I- Initialiser.  ": locate 35, 41: print ligne35$
ligne36$ = "  Quitter (Echap).  ": locate 36, 41: print ligne36$
ligne37$ = "                    "
for x = -2 to 2
  locate 1,1: print x                                     'afficher les abscisses.
  if x > 0 then locate 1,1: print "+"
  get(0,0)-(15,15), image
  put(xCentre + lambda * x - 11, 240), image, pset
next
locate 1,1: print "  ": get(0,0)-(0,0), image             'effacer.
locate 15, 2:  print "t (2 pi) =        sec."
locate 15, 78: print "t' en secondes lentes.";
locate 16, 2:  print "x (ou lambda) ="
locate 16, 78: print "x en secondes lumiŠre.";
locate 17, 2:  print "t' ="
for x = -2 * lambda to 2 * lambda step lambda / 10        'échelle graduée.
  if x mod lambda then                                    'barres des décimales.
    line(xCentre + x, hauteur + 2)-(xCentre + x, hauteur + 17), noir
    if choix$ = "B" then line(xCentre + x * gLorentz, hauteur + 66)-(xCentre + x * gLorentz, hauteur + 81), noir
  else                                                    'barres des entiers.
    line(xCentre+x-1,hauteur+2)-(xCentre+x+1, hauteur+25), noir, bf
    if choix$ = "B" then line(xCentre+x*gLorentz-1,hauteur+61)-(xCentre+x*gLorentz+1, hauteur+81), noir, bf
  end if
next
if choix$ = "A" then
  x = 57: locate 2
  xImpression = -gLorentz * xCentre                       'balayage vers la droite.
else
  x = 2: locate 20
  xImpression = gLorentz * xCentre                        'balayage vers la gauche.
end if
Locate , x:print "Cliquez sur les images pour d‚placer le"
Locate , x:print "plan de balayage. Si vous d‚sirez sauter"
Locate , x:print "cette ‚tape, appuyez sur le ® T ¯ et :":?
Locate , x:print "Promenez le pointeur de la souris sur l'‚-"
Locate , x:print "chelle gradu‚e pour connaŒtre les grandeurs"
Locate , x:print "donn‚es par les ‚quations de Lorentz.":?
Locate , x:print "Promenez le pointeur de la souris sur les"
Locate , x:print "images puis cliquez pour modifier le temps"
Locate , x:print "® t ¯ tel qu'indiqu‚."
locate 33
locate, 2: print "P- Pause.  R- Reculer."
locate, 2: print "T- Tableau suivant."
locate, 2: print "Lambda ="; lambda; " (appuyez sur + ou -)."
locate, 2: print using "Vitesse bˆta = v/c = #.###"; beta
locate, 2: print "(appuyez sur un chiffre de 0 … 9).";
color rgb(0,0,255)                                        'choix en cours affiché en bleu.
if choix$ = "A" then
  locate 33, 41: print ligne33$
else locate 34, 41: print ligne34$ 
end if
locate 33: color rgb(0,150,0)
locate,82: print "   glafreniere.com"
locate,82: print "  Le 18 avril 2007"
locate,82: print "Gabriel LaFreniŠre"
locate,69: print "  Ce programme peut ˆtre copi‚,"
locate,69: print "modifi‚ ou distribu‚ librement.";
gosub FlecheGauche
gosub FlecheDroite
pcopy 2, page1
pcopy 2, page2
screenset page1, page2
color noir
'Print " CHR$(130)  ‚   e accent aigu."
'Print " CHR$(133)  …   a accent grave."
'Print " CHR$(136)  ˆ   e accent circonflexe."
'Print " CHR$(138)  Š   e accent grave."
'Print " CHR$(140)  Œ   i accent circonflexe."
'Print " CHR$(174)  ®  ¯ guillemets. "
'Print " CHR$(147)  “   o accent circonflexe."
return
