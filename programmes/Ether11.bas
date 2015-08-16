largeur = 800: hauteur = 600
dim couleur(96, largeur, hauteur)                         'grand tableau de variables 3D.
dim graph(96, largeur)
dim as single phase1(largeur, hauteur)
dim as single phase2(largeur, hauteur)
dim as single energie(largeur, hauteur)
dim as single amplitude1(largeur, hauteur)
dim as single amplitude2(largeur, hauteur)
dim as single affaiblissement(largeur, hauteur)
dim as single affaib, rayon, max, angle, angle1, angle2, phi, phase, facteur
dim as single diagonale1, diagonale2, periode, temps, differenceDeMarche, espacement
dim as single pi, deuxPi, rotation, sag, prec, beta, demiEspace, brillance, luminance
dim as single amplitude, amplitudeSin, amplitudeCos, flottante, nombre, ajout, pas, spin
screen 19,24,3: page1 = 1: gosub Initialisation

do'                                LES CHAMPS DE FORCE.
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
      case "A": if biconvexe then biconvexe = 0: combo$ = ""
      case "B": if biconvexe = 0 then biconvexe = 1
      case "C": if champ = 0 then champ = 1
      case "D": if champ then champ = 0
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
      case "0": lambda = 100
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
      case "K+": run "Ether10.exe"                        'flèche gauche.
      case "M+": run "Ether12.exe"                        'flèche droite.  
      case "X+", "k+": end                                'fenêtre Windows.
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
  if ligne < 35 then
    if ligne < 23 or xSouris > 384 then ligne = 0
  else
    if xSouris < 304 or xSouris > 496 then ligne = 0
  end if
    
  color , turquoise: locate ligne, 2
  select case ligne'************************************** rehausser l'affichage.
    case 23: if biconvexe then print ligne23$
    case 24: if biconvexe then else print ligne24$
    case 25: if pleinEcran then print ligne25a$ else print ligne25$
    case 26: if champ then else print ligne26$
    case 27: if champ then print ligne27$
    case 28: print ligne28$
    case 29: if choix$ = "amplitude" then else print ligne29$
    case 30: if choix$ = "hyperboles" then else print ligne30$
    case 31: if choix$ = "ellipses" then else print ligne31$
    case 35: locate ligne, 39: print ligne35$
    case 36: locate ligne, 39: print ligne36$
    case 37: locate ligne, 39: print ligne37$;
             if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, fond

  if clic = 1 then'*************************************** action suite à un clic.
    select case ligne
      case 23: if biconvexe then biconvexe = 0: gosub MiseAJour
      case 24: if biconvexe = 0 then biconvexe = 1: gosub MiseAJour
      case 25: if pleinEcran then pleinEcran = 0 else pleinEcran = 1
               gosub MiseAJour
      case 26: if champ = 0 then champ = 1: gosub MiseAJour
      case 27: if champ then champ = 0:     gosub MiseAJour
      case 28                                             'sleep ci-dessous.
      case 29: if choix$ <> "amplitude" then  choix$ = "amplitude":  gosub MiseAJour
      case 30: if choix$ <> "hyperboles" then choix$ = "hyperboles": gosub MiseAJour
      case 31: if choix$ <> "ellipses" then   choix$ = "ellipses":   gosub MiseAJour
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether10.exe" else run "Ether12.exe"
      case else: ligne = 0
    end select
  end if
  if ligne = 0 and clic > 0 then gosub MiseEnPlace
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
'  locate 35, 2: print using "#.### sec"; timer - temps;: print "."
'  temps = timer
  if image = images then image = 0
  do
    if clic = 1 and ligne = 28 then sleep 1000: ySouris = 0: exit do
    getmouse xSouris, ySouris, , clic                     'éviter les actions à répétition.
  loop while clic > 0
loop

Aiguiller:'-------------------- GRAPHIQUE SELON LES CHOIX EN COURS ---------------------------
if image = images then OK = 1
rotation = image * 2 * pi / images                        'rotation de phase selon l'image.
prec = graphique
                                                          '          NOTA BENE :
                                                          'amplitude1 et 2 selon la distance.
                                                          'Affaiblissement() selon que l'onde
for x = 0 to largeur                                      'est stationnaire ou non.
  getmouse xS, yS, , clac                                 'vérifier souvent.
  if clac > 0 then clic = clac: xSouris = xS: ySouris = yS'les valeurs persistent.
  for y = 0 to hauteur
    select case choix$
'******************************** AFFICHER LA PÉRIODE EN COULEURS ****************************
      case "amplitude"
        if champ then                                     'l'amplitude varie entre -2 et +2.
          amplitudeSin = sin(phase1(x, y) - rotation) + sin(phase2(x, y) - rotation)
          if amplitudeSin > 0 then
            r = 300 * brillance * amplitudeSin * affaiblissement(x, y)
            b = r / 2
            if r > 255 then g = r - 255 else g = 0        'le rouge atteint, virer au blanc.
          else
            g = -300 * brillance * amplitudeSin * affaiblissement(x, y)
            b = g / 2
            if g > 255 then r = g - 255 else r = 0
          end if
          if y = yCentre then graph(image, x) = graphique - 11 * amplitudeSin  * affaiblissement(x, y)'pour graphique 1-D.
        else
          amplitudeSin = amplitude1(x, y) * sin(phase1(x, y) - rotation) + amplitude2(x, y) * sin(phase2(x, y) - rotation)
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
        end if
      case "hyperboles"                                   'faire apparaître les hyperboles.  
        if champ then
          if biconvexe then
            amplitude = 1000 * affaiblissement(x, y) * energie(x,y)
            if espacement > 200 then amplitude = amplitude *  200 / espacement
          else
            amplitude = 500 * affaiblissement(x, y) * energie(x,y)
          end if
          if y = yCentre then graph(image, x) = graphique - 40 * affaiblissement(x, y) * energie(x,y) + 15'pour graphique 1-D.
        else
          if biconvexe then
            amplitude = 15000000 * energie(x,y) * amplitude1(x, y) * amplitude2(x, y) / lambda ^ 2'l'énergie est le carré de l'amplitude.
            if espacement > 300 then amplitude = amplitude * espacement ^ 2 / 90000
          else
            amplitude = 1500000 * energie(x,y) * amplitude1(x, y) * amplitude2(x, y) / lambda
          end if          
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
          luminance = 2 * affaiblissement(x, y) * abs(sin(phase1(x, y) / 2 + phase2(x, y) / 2 - rotation))
          if y = yCentre then graph(image, x) = graphique - 35 * affaiblissement(x, y) * abs(sin(phase1(x, y) / 2 + phase2(x, y) / 2 - rotation)) + 12
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
locate 23, 2: print ligne23$                              'rafraîchir les choix.
locate 24, 2: print ligne24$
locate 25, 2: print ligne25$ 
locate 26, 2: print ligne26$
locate 27, 2: print ligne27$
locate 29, 2: print ligne29$
locate 30, 2: print ligne30$
locate 31, 2: print ligne31$

color bleu, fond                                          'identifier le choix en cours.
if biconvexe then
       locate 24, 2:  print ligne24$
  else locate 23, 2:  print ligne23$
end if
if champ then
       locate 26, 2:  print ligne26$
  else locate 27, 2:  print ligne27$
end if
select case choix$
  case "amplitude":  locate 29, 2: print ligne29$
  case "hyperboles": locate 30, 2: print ligne30$
  case "ellipses":   locate 31, 2: print ligne31$
end select
color noir, fond
if pleinEcran then
  largeur = 800: hauteur = 600
else
  largeur = 400: hauteur = 300
end if
xCentre = largeur / 2
yCentre = hauteur / 2
if combo$ <> "" then
  biconvexe = 1
  select case combo$
    case "S": accord = 0
      xElectron1 = -1000
      xElectron2 = 1000 + largeur
    case "T": accord = 2
      xElectron1 = largeur - lambda
      xElectron2 = 1000
    case "U": accord = 1
      xElectron1 = xCentre - 1.5 * lambda
      xElectron2 = xElectron1 + 3 * lambda
    case "V": accord = 0
      xElectron1 = xCentre - lambdaSurDeux
      xElectron2 = xElectron1 + lambda
    case "W": accord = 1
      xElectron1 = xCentre - lambda / 8
      xElectron2 = xElectron1 + lambda / 4
    case "X": accord = 1
      xElectron1 = xCentre - 3 * lambda / 8
      xElectron2 = xElectron1 + 3 * lambda / 4
    case "Y": accord = 2
      xElectron1 = xCentre - lambda / 4
      xElectron2 = xElectron1 + lambdaSurDeux
    case "Z": accord = 0
      xElectron1 = xCentre - lambda / 4
      xElectron2 = xElectron1 + lambdaSurDeux
  end select
end if
select case accord
  case 0: spin = 0:          accord$ = "0 pi    "
  case 1: spin = pi / 2:     accord$ = "pi / 2  "
  case 2: spin = pi:         accord$ = "pi      "
  case 3: spin = 3 * pi / 2: accord$ = "3 pi / 2"
end select
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
    if biconvexe then
      diagonale1 = sqr(yCoord1 * yCoord1 + xCarre1)       'distance réelle selon Pythagore.
      flottante = diagonale1 / lambda                     'distance en longueurs d'onde.
      entier = flottante                                  'distance, nombre entier.
      differenceDeMarche = flottante - entier             'difference en longueurs d'onde.
      amplitude1(x,y) = lambdaSurDeux / diagonale1
      if diagonale1 < lambdaSurDeux then
        sag = rayon - sqr(rayon ^ 2 - differenceDeMarche ^ 2) 'flèche d'un cercle fictif.
        differenceDeMarche = 1.25 * (.2 + sag)            'arrondir l'angle central.
        amplitude1(x,y) = 1
      end if
    else                                                  'ondes planes, origine lointaine.
      diagonale1 = x                                      'arbitraire, varie avec x uniquement.
      flottante = diagonale1 / lambda                     'distance en longueurs d'onde.
      entier = flottante                                  'distance, nombre entier.
      differenceDeMarche = flottante - entier             'difference en longueurs d'onde.
      amplitude1(x,y) = .05                               'ondes planes: amplitude constante.
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
    if biconvexe then
      if diagonale1 > diagonale2 then distance = diagonale1 else distance = diagonale2
      affaib = demiEspace / distance                 'la partie stationnaire n'est jamais plus
    else                                            'forte que la plus faible des deux ondes.
      affaib = .8                                   'ondes planes, amplitude constante.
    end if
    phase2(x, y) = spin + deuxPi * differenceDeMarche     'phase de la deuxième onde.
    
'   IMPORTANT: le champ de force ne rayonne de l'énergie que dans la mesure
'   où ses ondes sont stationnaires. Selon les transformations de Lorentz, la
'   vitesse d'un système mobile correspond à la vitesse normalisée bêta,
'   qui s'évalue selon le sinus de l'angle phi. Cet angle correspond au demi-angle
'   de « l'effet de ciseau », qui détermine la vitesse de l'onde de phase (1 / bêta),
'   cette vitesse étant toujours supérieure à celle de la lumière. On peut donc
'   déterminer un bêta fictif et ensuite l'énergie du champ de force à l'aide de cet angle.

'   L'angle peut être déterminé à l'aide de la loi des cosinus (le théorème d'Al Kashi):
    angle = acos((diagonale1 ^ 2 + diagonale2 ^ 2 - espacement ^ 2) / (2 * diagonale1 * diagonale2))

'   Le champ plano-convexe est plus puissant parce que l'angle sur un axe orthogonal
'   n'est jamais inférieur à 90°, et aussi parce que le volume des ondes stationnaires
'   axiales se situe à l'intérieur d'un paraboloïde et non d'un ellipsoïde : 
    if biconvexe = 0 then angle = acos(xCoord2 / diagonale2)
    if angle < 0 then angle = 0
      
'   Le demi-angle de croisement indique dans quelle mesure les ondes sont stationnaires:
    phi = angle / 2                                       'angle des transformations Lorentz.
    beta = sin(phi)                                       'vitesse purement théorique.
    affaiblissement(x, y) = (affaib * beta) ^ 2           'selon le carré de l'amplitude.
    amplitudeSin = (sin(phase1(x, y)) + sin(phase2(x, y))) / 2
    amplitudeCos = (cos(phase1(x, y)) + cos(phase2(x, y))) / 2
    energie(x, y) = amplitudeSin * amplitudeSin + amplitudeCos * amplitudeCos' max = 1
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
images = 48
largeur = 400: hauteur = 300
'bitmap = 1                                               'séquence bitmap si désiré.
rayon = sin(pi / 4)'                                      'rayon fictif pour le noyau central.
champ = 0                                                 '1 -> afficher le champ. 0 -> ondes.
biconvexe = 1                                             'champ biconvexe.
combo$ = ""
choix$ = "hyperboles"
choix$ = "ellipses"
choix$ = "amplitude"
accord = 1                                                'couple électron-positron par défaut.
spin = pi / 2                                             'spin, 2e électron ou positron.
lambda = 32
lambdaSurDeux = lambda / 2
pleinEcran = 0
xCentre = largeur / 2
yCentre = hauteur / 2
espacement = .75 * largeur
entier = espacement / lambda
espacement = entier * lambda                              'multiple exact.
xElectron1 = (largeur - espacement) / 2
xElectron2 = xElectron1 + espacement
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
titre$ = "  LES CHAMPS DE FORCE  "                        'titre.
centre = 800 - (800 - largeur) / 2
xg = centre - 4 * len(titre$) + 3
xd = centre + 4 * len(titre$) + 3
yh = 10
yb = 34
line(xg-3,yh-3)-(xd+3,yb+3), noir, B 
line(xg-2,yh-2)-(xd+2,yb+2), blanc, B 
line(xg-1,yh-1)-(xd+1,yb+1), blanc, B 
line(xg,yh)-(xd,yb), rouge,BF
line(4,351)-(4,384), noir                                 'crochets pour choix multiples.
line(4,351)-(7,352), noir, b
line(4,383)-(7,384), noir, b
line(4,399)-(4,432), noir
line(4,399)-(7,400), noir, b
line(4,431)-(7,432), noir, b
line(4,447)-(4,496), noir
line(4,447)-(7,448), noir, b
line(4,495)-(7,496), noir, b
color blanc, rouge
locate 2,65: print titre$
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
locate , 53:  print "Toutes les forces de la nature agissent par les"
locate , 53:  print "champs de force. Il s'agit des ondes partielle-"
locate , 53:  print "ment stationnaires qui  apparaissent lors de la"
locate , 53:  print "rencontre des ondes que les atomes et les ‚lec-"
locate , 53:  print "trons ‚mettent. Ces champs sont alors amplifi‚s"
locate , 53:  print "par les ondes de l'‚ther, et c'est pourquoi ils"
locate , 53:  print "retournent des ondes puissantes vers les atomes"
locate , 53:  print "et les ‚lectrons qui leur ont donn‚ naissance. "
print
locate , 53:  print "C'est ce qui permet d'expliquer la m‚canique de"
locate , 53:  print "la matiŠre, et donc la gravit‚,  les forces nu-"
locate , 53:  print "cl‚aires,  les champs magn‚tiques,  ‚lectriques"
locate , 53:  print "et ‚lectrostatiques, et mˆme l'action de la lu-"
locate , 53:  print "miŠre et des ondes radio. Le m‚canisme ‚tant le"
locate , 53:  print "mˆme dans tous les cas,  toutes les forces sont"
locate , 53:  print "d‚sormais unifi‚es  en invoquant la pression de"
locate , 53:  print "radiation exerc‚e par les champs de force."
locate , 53:  print ""
locate , 53:  print "Le champ de force plano-convexe se forme … par-"
locate , 53:  print "tir des ondes ‚mises par un systŠme ‚loign‚, de"
locate , 53:  print "sorte qu'elles sont planes ou presque. On cons-"
locate , 53:  print "tate alors que le volume de la section centrale"
locate , 53:  print "atteint un maximum, d'o— un maximum d'efficaci-"
locate , 53:  print "t‚. Le champ biconvexe se forme entre n'importe"
locate , 53:  print "quels ‚metteurs d'ondes rapproch‚s, le meilleur"
locate , 53:  print "exemple ‚tant le champ ‚lectrostatique. Les ef-"
locate , 53:  print "fets d'attraction ou  de r‚pulsion d‚pendent de"
locate , 53:  print "la p‚riode relative des deux ‚metteurs,  seules"
locate , 53:  print "les ondes en phase exer‡ant une pression de ra-"
locate , 53:  print "diation sur ces ‚metteurs."

ligne23$ = " A - Le champ de force plano-convexe.          ": locate 23, 2: print ligne23$
ligne24$ = " B - Le champ de force biconvexe.              ": locate 24, 2: print ligne24$
ligne25$ = "     J - Graphique pleine fenˆtre.             ": locate 25, 2: print ligne25$ 
ligne25a$ ="     J - Retour au graphique normal.           "
ligne26$ = " C - Afficher le champ de force seulement.     ": locate 26, 2: print ligne26$
ligne27$ = " D - Afficher les ondes qui forment le champ.  ": locate 27, 2: print ligne27$
ligne28$ = "     R - Ralenti.                              ": locate 28, 2: print ligne28$ 
ligne29$ = " O - Afficher l'amplitude (les ondes).         ": locate 29, 2: print ligne29$
ligne30$ = " H - L'‚nergie seulement (les hyperboles).     ": locate 30, 2: print ligne30$
ligne31$ = " E - Les phases relatives (les ellipses).      ": locate 31, 2: print ligne31$
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
locate 36, 63: print "Le 4 mars 2006. Ce programme peut ˆtre";
locate 37, 63: print "distribu‚, copi‚ ou modifi‚ librement.";
pcopy 2, 0
pcopy 2, 1
color noir, fond
gosub MiseAJour
return
