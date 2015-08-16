largeur = 400: hauteur = 400                              'nombre de particules par côté.
dim as single sinus(-1 to largeur+2, -1 to hauteur+2)
dim as single cosinus(-1 to largeur+2, -1 to hauteur+2)
dim as single influence(-1 to largeur+2, -1 to hauteur+2)
dim as single amortissement(-1 to largeur+2, -1 to hauteur+2)
dim as single facteur, rapport, longueur, luminosite, contraste
dim as single amplitudeSinus, amplitudeCosinus, c, ton, xOrig, yOrig, depart
dim as single xPrime, tPrime, temps, beta, theta, ixe, igrec, xSource, etape
dim as single xCoord, yCoord, periode, rotation, amplitude, rayon, gLorentz, phi
dim as single pi, angle, lambda, xCarre, yCarre, distance, xDistance, yDistance
marge = 100: beta = 0: lambda = 40: choix$ = "A"
screen 19,24,3: page2 = 1: : gosub Initialisation

do'                       MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.
  for y = 0 to hauteur - 1
    influence(0,y) = (sinus(-1,y) + sinus(1,y) + sinus(0,y-1) + sinus(0,y+1)) / 2 - 2 * sinus(0,y)' un cran à l'avance.
  next
' IMPORTANT: l'algorithme qui suit demeure fondamentalement celui qui a été inventé par M.
' Philippe Delmotte en juin 2005 en faisant intervenir les lois de Newton sur l'inertie. Mais
' M. Dewavrin a remarqué (voir Ether04) que le principe de base était identique à la méthode
' d'Euler, qui est fondée sur la trigonométrie, et qui permet de construire une sinusoïde.
' J'ai transposé moi-même cette méthode dans l'algorithme de M Delmotte. La vitesse de l'onde,
' soit « c », est ici de 0,707 (cos 45°) pixel par passage, alors qu'elle était seulement
' de 0,5 pixel par passage avec mon algorithme précédent. On peut obtenir un effet de lentille
' en ajoutant un indice (> 1) come ceci: cosinus(x, y) = cosinus(x, y) + influence(x, y) / indice

  for x = 0 to largeur: for y = 0 to hauteur
    
'------------- CALCUL SELON LA MÉTHODE D'EULER SIMPLIFIÉE PAR M. ANSELME DEWAVRIN -------------
    influence(x+1,y) = (sinus(x,y) + sinus(x+2,y) + sinus(x+1,y-1) + sinus(x+1,y+1)) / 2 - 2 * sinus(x+1,y)
    cosinus(x,y) = cosinus(x,y) + influence(x,y)
    sinus(x,y) = (sinus(x,y) + cosinus(x,y)) * amortissement(x,y)
'--------------------------------------- FIN DU CALCUL ----------------------------------------
  next: next

  nombre += 1
  temps = temps + c / lambda                              'c est la vitesse des ondes.
  select case choix$
    case "A": gosub choixA
    case "B": gosub choixB
    case "C": gosub choixC
  end select

  if clic = 0 then getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  if afficher = 1 then                                    'afficher une fois sur deux.
    afficher = 0
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    gosub afficherCouleurs'---------- DIAGRAMME PRINCIPAL -------------------------------------
  else afficher += 1
  end if
'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = inkey
  if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
  if len(saisie$) then
    bitmap = 0
    select case saisie$
      case "X+",chr$(27): end
      case "I": beta = 0: lambda = 40: choix$ = "A"
      case "P": sleep: saisie$ = ""
      case "A": marge = 50
      case "B": marge = 100
      case "C": marge = 150
      case "0": beta = 0
      case "1": beta = .1
      case "2": beta = .2
      case "3": beta = .3
      case "4": beta = .4
      case "5": beta = .5
      case "6": beta = .6
      case "7": beta = .7
      case "8": beta = .8
      case "9": beta = .9
      case else: saisie$ = ""
    end select
    if len(saisie$) then gosub Initialisation
  end if
'----------------------------------------- SAISIE SOURIS --------------------------------------

  if clic = 0 then getmouse xSouris, ySouris, , clic      'vérifier une dernière fois.
  ligne = .5 + ySouris / 16
  if ligne > 26 and ligne < 38 then
    if xSouris < 304 then ligne = 0
  else ligne = 0  
  end if
  
'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 39
  select case ligne
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
'-------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
  if clic = 1 then
    bitmap = 0
    select case ligne
      case 30: choix$ = "A": luminosite = 10: gosub Initialisation
      case 31: choix$ = "B": luminosite = 40: gosub Initialisation
      case 32: choix$ = "C": luminosite = 40: gosub Initialisation
      case 35: beta = 0: lambda = 40: choix$ = "A": gosub Initialisation
      case 36: end
      Case 37: If xSouris < 400 Then Run "Ether17.exe" Else Run "Ether00.exe"
    end select
  end if
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
  clic = 0
loop

afficherCouleurs:'------------------ AFFICHER EN COULEURS -------------------------------------
for x = 0 to largeur
  for y = 0 to hauteur
    if sinus(x, y) < 0 then
      luminance = luminosite * -sinus(x, y)
      vert = luminance
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    else
      luminance = luminosite * sinus(x, y)
      rouge = luminance
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    end if
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    if bleu > 255 then bleu = 255 else if bleu < 0 then bleu = 0
    pset (x, y), rgb(rouge,vert,bleu)
    if x = largeur / 2 then
      circle(largeur+100+20 * (lambda / (2 * pi)) * cosinus(x,y),y), 1, rgb(0,0,255)
      circle(699+1000 * sinus(x,y), y), 1, noir
      circle(largeur+100+20 * sinus(x,y),y),1, noir
'      pset(largeur+30+1600 * influence(x,y),y), rgb(0,0,225)
    end if
  next
next
line(699-20, 0)-(699+20, hauteur), noir, b
line(699-20, marge)-(699+20, hauteur-marge), noir, b
return

choixA:'****** PULSATION D'UNE ONDE SIMPLE SELON LES TRANSFORMATIONS DE LORENTZ ***************
yCoord = 0
xOrig = xCoord / lambda                                   'xOrig et xPrime en longueurs d'onde.
xPrime = xOrig * cos(theta) + temps * sin(theta)          '--- TRANSFORMATIONS DE LORENTZ. ---
tPrime = temps * cos(theta) - xOrig * sin(theta)          'temps et tPrime en périodes d'onde.
if xPrime * lambda > 799 then gosub Initialisation
gosub Pulsation    
return

choixB:'*********** PULSATION D'UN CERCLE SELON LES TRANSFORMATIONS DE LORENTZ ****************

  rayon = 3.1875                                          'il faut émettre au centre de l'un
  etape = 2 * pi / 49                                     'des ventres des ondes stationnaires, 
  for angle = etape / 2 to 2 * pi step etape              'soit 0,1875 plus n * 0,5.
    xCoord = depart * lambda + rayon * lambda * cos(angle)'en pixels.
    yCoord = rayon * lambda * sin(angle)                  'en pixels.
    xOrig = xCoord / lambda                               'xOrig et xPrime en longueurs d'onde.
    xPrime = xOrig * cos(theta) + temps * sin(theta)      '--- TRANSFORMATIONS DE LORENTZ. ---
    tPrime = temps * cos(theta) - xOrig * sin(theta)      'temps et tPrime en périodes d'onde.
    if xPrime * lambda > 799 then gosub Initialisation: exit for
    gosub Pulsation    
  next
'**********************************************************************************************
  xOrig = depart                                          'position de départ, constante.
  xPrime = xOrig * cos(theta) + temps * sin(theta)        'x' selon Lorentz.
  circle(xPrime * lambda, yCentre), rayon * lambda, noir,,, 1 / gLorentz 'ellipse mobile.
return

choixC:'******* PULSATION D'UN ARC DE CERCLE SELON LES TRANSFORMATIONS DE LORENTZ *************

  rayon = 8
  etape = 2 * pi / 199
  for angle = pi / 3 to pi - pi / 3 step etape
    xCoord = depart * lambda + rayon * lambda * cos(angle)
    yCoord = rayon * lambda * sin(angle) - 100
    xOrig = xCoord / lambda                               'xOrig et xPrime en longueurs d'onde.
    xPrime = xOrig * cos(theta) + temps * sin(theta)      '--- TRANSFORMATIONS DE LORENTZ. ---
    tPrime = temps * cos(theta) - xOrig * sin(theta)      'temps et tPrime en périodes d'onde.
    if xPrime * lambda > 799 then gosub Initialisation: exit for
    gosub Pulsation    
  next
return


Pulsation:'---- PULSATION SELON UN CARRÉ DE 9 GRANULES POUR RÉDUIRE LES ARTÉFACTS -------------
if nombre > 2 * lambda / c then return
' Le programme Ether04 montre que le cosinus correspond vraiment à la quadrature.
' Il vaut donc mieux utiliser le cosinus de la période si l'on modifie la variable cosinus.
' Autrement il en résulte une anomalie quand bêta = 0, car dans ce cas la période n'évolue pas.
cosinus(xPrime * lambda-1, yOrig * lambda + yCoord-1) = cosinus(xPrime * lambda-1, yOrig * lambda + yCoord-1) +.25 * cos(2 * pi * tPrime)
cosinus(xPrime * lambda-1, yOrig * lambda + yCoord-0) = cosinus(xPrime * lambda-1, yOrig * lambda + yCoord-0) +.5  * cos(2 * pi * tPrime)
cosinus(xPrime * lambda-1, yOrig * lambda + yCoord+1) = cosinus(xPrime * lambda-1, yOrig * lambda + yCoord+1) +.25 * cos(2 * pi * tPrime)
cosinus(xPrime * lambda+0, yOrig * lambda + yCoord-1) = cosinus(xPrime * lambda+0, yOrig * lambda + yCoord-1) +.5  * cos(2 * pi * tPrime)
cosinus(xPrime * lambda+0, yOrig * lambda + yCoord+0) = cosinus(xPrime * lambda+0, yOrig * lambda + yCoord+0) + 1  * cos(2 * pi * tPrime)
cosinus(xPrime * lambda+0, yOrig * lambda + yCoord+1) = cosinus(xPrime * lambda+0, yOrig * lambda + yCoord+1) +.5  * cos(2 * pi * tPrime)
cosinus(xPrime * lambda+1, yOrig * lambda + yCoord-1) = cosinus(xPrime * lambda+1, yOrig * lambda + yCoord-1) +.25 * cos(2 * pi * tPrime)
cosinus(xPrime * lambda+1, yOrig * lambda + yCoord-0) = cosinus(xPrime * lambda+1, yOrig * lambda + yCoord-0) +.5  * cos(2 * pi * tPrime)
cosinus(xPrime * lambda+1, yOrig * lambda + yCoord+1) = cosinus(xPrime * lambda+1, yOrig * lambda + yCoord+1) +.25 * cos(2 * pi * tPrime)
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
temps = 0: nombre = 0
theta = asin(beta)
gLorentz = sqr(1 - beta ^ 2)                              'facteur de contraction g de Lorentz.
c = sin(pi / 4)                                           'vitesse de l'onde en pixels par passage.
contraste = 2
yOrig = hauteur / 2 / lambda
xSource = 300
relief = lambda / 5
racine = marge / 5
demiOnde = 1
xCentre = largeur / 2
yCentre = hauteur / 2
select case choix$
  case "A": xCoord = largeur/2: luminosite = 1000
  case "B": depart = 5: luminosite = 40
  case "C": depart = 5: luminosite = 40
  case ""
  case ""
end select
ixe = lambda/2'***********************

xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
line(0,0)-(largeur, hauteur),noir, bf
ligne35$ = "        I - Initialiser. ": locate 35, 39: print ligne35$
ligne36$ = "        Quitter (Echap). ": locate 36, 39: print ligne36$
ligne37$ = "                         "
locate 26, 62: print "1 x                      50 x"
locate 28, 47: print "Le sinus est affich‚ en noir; le cosinus, en bleu."
locate 30
locate, 2: print "A- Marge de 50 pixels.    Il faut une marge d'au moins 100 pixels pour amortir convenablement les"
locate, 2: print "B- Marge de 100 pixels.   r‚flexions sur les bords du tableau. Dans ce cas, la distribution sinu-"
locate, 2: print "C- Marge de 150 pixels.   so‹dale doit ˆtre r‚duite … la racine vingtiŠme, comme ceci:"
locate,28: print "potentiel(x, y) = potentiel(x, y) * sin(angle) ^ (1 / (marge / 5))"
locate, 2: print ""
locate, 2: print ""
locate, 2: print ""
locate 36: color vert
locate, 2: print "Mille mercis … M. Philippe Delmotte."
locate, 2: print "Gabriel LaFreniŠre  glafreniere.com";
locate 35
locate, 69:print "Le 25 nov. 2006. Ce programme"
locate, 69:print "FreeBASIC peut ˆtre copi‚,"
locate, 69:print "distribu‚ ou modifi‚ librement.";

for x = -1 to largeur + 1                                 'effacer.
  for y = -1 to hauteur + 1
    sinus(x, y) = .000001                                 'accélère le calcul (!!?).
    cosinus(x, y) = .000001
    amortissement(x,y) = 1
  next
next
for x = -1 to marge                                       'mémoriser l'amortissement.
  angle = (pi / 2) * x / marge
  for y = -1 to hauteur+1
    amortissement(x,y) = sin(angle) ^ (1 / racine)
    amortissement(largeur-x,y) = amortissement(x,y)
    if y < x then
      amortissement(x,y) = sin((pi / 2) * y / marge) ^ (1 / racine)
      amortissement(largeur-x,y) = amortissement(x,y)
    end if
    if x > hauteur-y then
      amortissement(x,y) = sin((pi / 2) * (hauteur-y) / marge) ^ (1 / racine)
      amortissement(largeur-x,y) = amortissement(x,y)
    end if
  next
next
for x = marge+1 to largeur - marge                        'mémoriser le centre, haut et bas.
  for y = -1 to marge
    amortissement(x,y) = sin((pi / 2) * y / marge) ^ (1 / racine)
    amortissement(x,hauteur-y) = amortissement(x,y)
  next
next
for x = 0 to largeur                                     'visualiser l'amortissement.
  for y = 0 to hauteur
    ton = 200 * amortissement(x,y) ^ 18
    pset(x,y), rgb (ton,ton,ton)
  next
next
sleep 1000
pcopy 2, page1
pcopy 2, page2
screenset page1, page2
return
