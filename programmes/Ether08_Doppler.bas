cote = 800                                                'nombre de particules par côté.
dim as single potentiel(0 to 10 * cote)'                  'principe de Huygens, sous-multiples..
dim as single M(-1 to cote+1, -1 to cote+1)
dim as single I(-1 to cote+1, -1 to cote+1): dim precedent(-1 to cote+1)
dim as single P(-1 to cote+1, -1 to cote+1), potentiel2(-1 to cote+1, -1 to cote+1)
dim as single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance, c
dim as single xCoord, distNormale, periode, distance, rotation, amplitude, phi
dim as single ondelette, facteur, rapport, longueur, luminosite, contraste, relief
dim as single betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, pas, ton, xSource
dim as single pi, angle, lambda, xCarre, yCarre, arcSinus, xDistance, affaiblissement
screen 19,24,3: gosub Initialisation

'      IMPORTANT: ALLER À LA LIGNE 45 POUR LA PULSATION D'UNE ONDE (11 novembre 2006).

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

'*********************************** PULSATION D'UNE ONDE *************************************
  xSource = xSource + (beta * c)                          'point mobile à déplacer.
  if xSource > cote - lambda then xSource = 0
  periode = 2 * pi * xSource / lambda                     'la période évolue selon ce point.
  for x = -5 to 5
    carre = x * x                                         'pour distance selon Pythagore.
    for y = 396 to 404
      distance = sqr(carre + (400 - y) ^ 2)               'simuler un bosse semblable au noyau
      phi = 2 * pi * distance / 10                        'de l'électron (rayon 10 pixels).
      if phi then amplitude = sin(phi) / phi else amplitude = 1'formule de Marcotte: sin(x)/x
      if distance > 5 then amplitude = 0
      P(xSource+x, y) = P(xSource+x, y) + 4 * amplitude * sin(periode)'générer une onde.
      if y = 400 then line(xSource+x, y/2 - 20*amplitude* sin(periode))-(xSource+x, y/2), noir
    next
  next

  if clic = 0 then getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  if afficher = 0 or clic > 0 then                        'afficher une fois sur deux.
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    gosub afficherRelief'------------- DIAGRAMME PRINCIPAL -----------------------------------

'--------------------------------------- SAISIE CLAVIER --------------------------------------
    saisie$ = inkey
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    if saisie$ = "k+" then end 
    if len(saisie$) then
      bitmap = 0
      if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+"': locate 34,2: print saisie$
      select case saisie$
        case "k+",chr$(27): end
      end select
    end if
'---------------------------------------- SAISIE SOURIS --------------------------------------

    if clic = 0 then getmouse xSouris, ySouris, , clic    'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
    if ligne > 26 and ligne < 38 then
      if xSouris < 304 or xSouris > 496 then ligne = 0
    else ligne = 0  
    end if
    
'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
    color noir, turquoise
    locate ligne, 39
    select case ligne
      
      case 36: print ligne36$
      case 37: print ligne37$;: if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
    end select
    color noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
    if clic = 1 then
      bitmap = 0
      select case ligne
        case 36: end
      end select
    end if
    if bitmap then gosub Bitmaps                          'capture d'images si désiré.
  end if
  clic = 0
  afficher = afficher + 1
  if afficher = 2 then afficher = 0                       'afficher une fois sur deux
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
ondelette = pi / 100                                      'cent ondelettes est un minimum.
'bitmap = 1                                               'séquence bitmap si désiré.
beta = .5
c = .5                                                    'vitesse de l'onde en pixels par passage.
gauche = 0
droite = cote
page2 = 1                                                 'page1 correspond à 0 au départ.
lambda = 30
luminosite = 40
contraste = 3.1
xSource = 200
relief = lambda / 5
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

ligne36$ = "      Quitter (Echap).   ": locate 36, 39: print ligne36$
ligne37$ = "                         "
return
