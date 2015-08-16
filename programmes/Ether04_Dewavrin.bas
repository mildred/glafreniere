page1 = 1: nombre = 799                                   'nombre de points.
dim as single sinus(-1 to nombre + 2)                     'le sinus correspond au potentiel.
dim as single cosinus(-1 to nombre + 1)                   'le cosinus correspond à la mémoire.
dim as single pas(-1 to nombre + 1)
dim as single pi, periode, xPoint, xCoord, yCoord, temps, constante, vitesse, affaiblissement
screen 19,24,3: gosub Initialisation

do
  swap page1, page2                                       'permuter les pages (plus rapide).
'  screensync
  screenset page1, page2                                  'travailler sur la page cachée.
  pcopy 2, page1                                          'copier la troisième page (texte).
  
  if reflexion = 0 then                                   'réflexion nulle (0).
    sinus(-1) = sinus(0) - cosinus(0) / vitesse
    sinus(nombre + 1) = sinus(nombre) - cosinus(nombre) / vitesse
  elseif reflexion = 2 then                               'réflexion molle (2).
   sinus(-1) = sinus(0)
   sinus(nombre + 1) = sinus(nombre)
  end if                                                  'réflexion dure (1) par défaut.
  
  pas(0) = (sinus(-1) + sinus(1)) - 2* sinus(0)           'un cran d'avance.

for x = 0 to nombre
  
' Cet algorithme est semblable à celui que M. Philippe Delmotte a mis au point en juin 2005
' en faisant intervenir la mécanique de Newton et non la méthode d'Euler. C'est donc M.
' Delmotte qui en est l'inventeur. M. Dewavrin a remarqué la similitude avec la méthode
' d'Euler, comme le montre le programme oscillations_Delmotte_Dewavrin.bas. Cette similitude
' suggère fortement que les lois de Newton s'expliquent à cause des ondes dont la matière est
' faite. Le lien avec la méthode d'Euler est évident car la courbe rouge correspond vraiment à
' la quadrature, et son amplitude au « pas » de cette méthode, qui vaut: 2 * pi / lambda.
' Ici, on a porté la vitesse « c » de l'onde de 0,707 (cos 45°) à 1 pixel exactement par
' passage dans la boucle en doublant la valeur de l'influence. Mais c'est au prix d'un
' comportement différent, en particulier en ce qui concerne la « chaleur », et aussi
' les ondes carrées ou en dents de scie, qui deviennent impraticables.

'******* CALCUL SELON L'ALGORITHME DE M. ANSELME DEWAVRIN, BASÉ SUR LA MÉTHODE D'EULER ********

    pas(x+1)   = sinus(x)   + sinus(x+2) - 2 * sinus(x+1) 'influence des 2 granules voisins.
    cosinus(x) = cosinus(x) + pas(x)
    sinus(x)   = sinus(x)   + cosinus(x)
    
'************************************** FIN DU CALCUL ****************************************

    pset (x, yCentre - sinus(x)), vert                    'courbe sinus.
    pset (x, yCentre + cosinus(x) / constante), rouge     'courbe cosinus.
    luminance = 3.5 * (sinus(x) + 40)
    if luminance > 255 then luminance = 255
    if luminance < 0 then luminance = 0
    line (x, yCentre + 60)-(x, yCentre + 90), rgb(luminance,luminance,luminance)
  next                                                    'échelle de gris.

  line(xPoint, yCentre - 10)-(xPoint, yCentre + 10),noir  'repère mobile: vitesse des ondes.
  xPoint = xPoint + vitesse: if xPoint > 800 then xPoint = xPoint - 800
  color bleu
  select case reflexion
    case 0: locate 32, 12: print ligne32$                 'afficher l'option en cours.
    case 1: locate 33, 12: print ligne33$
    case 2: locate 34, 12: print ligne34$
    end select
  color noir
  saisie$ = inkey
  if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
  if len(saisie$) then
    select case saisie$
      case chr$(27), "k+", "X+": end                      'exécution, commandes clavier.
      case "A": gosub Impulsion
      case "B": gosub Progressives
      case "C": gosub Stationnaires
      case "I": gosub Sinusoidales
      case "M": run "Ether00.exe"                         'menu principal, souvent utile.
      case "K+":run "Ether03.exe"                         'flèche gauche.
      case "M+":run "Ether05.exe"                         'flèche droite.        
      case "X": reflexion = 0
      case "Y": reflexion = 1
      case "Z": reflexion = 2
    end select
  end if

  getmouse xSouris, ySouris, , clic                       'saisie souris.
  ligne = .5 + ySouris / 16
  if xSouris < 86 or xSouris > 712 then ligne = 0
  if ligne < 25 or ligne > 37 then ligne = 0
  if ligne > 34 then
    if xSouris < 304 or xSouris > 496 then ligne = 0
  end if  
  
  if ligne then
    color noir, turquoise: locate ligne, 12               'rehausser l'affichage.
    select case ligne                                     'action suite à un clic.
      case 25: print ligne25$: if clic = 1 then gosub Impulsion
      case 26: print ligne26$: if clic = 1 then gosub Progressives
      case 27: print ligne27$: if clic = 1 then gosub Stationnaires
      case 28: print ligne28$: if clic = 1 then gosub Sinusoidales
      case 32: if reflexion = 0 then else print ligne32$
               if clic = 1 then reflexion = 0
      case 33: if reflexion = 1 then else print ligne33$
               if clic = 1 then reflexion = 1
      case 34: if reflexion = 2 then else print ligne34$
               if clic = 1 then reflexion = 2
      case 35: locate, 39: print ligne35$: if clic = 1 then gosub Initialisation: sleep 100
      case 36: locate, 39: print ligne36$: if clic = 1 then end
      case 37: locate, 39: print ligne37$;
               if xSouris < xCentre then
                 gosub flecheGauche
                 if clic = 1 then run "Ether03.exe"
               else
                  gosub flecheDroite
                 if clic = 1 then  run "Ether05.exe"
               end if
    end select    
    color noir, grisClair
  else
    do: getmouse xSouris, ySouris, , clic: loop while clic > 0 
  end if
loop

flecheGauche:'----------------------- DESSIN DES FLÈCHES -------------------------------------
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

'#############################################################################################
Initialisation:'---------------------- INITIALISATION ----------------------------------------
'#############################################################################################
rouge = rgb(255,0,0)                                      'couleurs usuelles.
vert = rgb(0,150,0)
bleu = rgb(0,0,255)
gris = rgb(150,150,150)
blanc = rgb(255,255,255)
grisClair = rgb(225,225,225)
turquoise = rgb (230,255,255)
color noir, grisClair
screenset 2, 2: cls

reflexion = 2
pi = 4 * atn(1)
xCentre = 400
yCentre = 80
lambda = (nombre + 1) / 10                                'pour ondes stationnaires stables.
lambda = 2 * lambda
vitesse = 1                                               'vitesse « c » en pixels par passage.
constante = 2 * pi / lambda / vitesse                     'selon la vitesse de l'onde.
lambdaSurDeux = lambda / 2
lambdaSurQuatre = lambda / 4
gabarit$ = "                                                                          "
lignes = 11                                               'nombre de lignes du texte.
th = 13                                                   'haut du texte.
tg = 50 - len(gabarit$) / 2 + 1                           'gauche du texte.
eh = th * 16 - 32                                         'haut de l'encadré.
eb = eh + 16 * lignes + 26                                'bas de l'encadré.
eg = tg * 8 - 32                                          'gauche de l'encadré.
ed = 800 - eg                                             'droite de l'encadré.
line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré.
line (eg + 1,eh + 1)-(ed, eb), noir, B
line (eg + 1,eb)-(ed, eb), blanc
line (eg,eb + 1)-(ed + 1, eb + 1), blanc
line (ed,eh + 1)-(ed, eb), blanc
line (ed + 1,eh)-(ed + 1, eb + 1), blanc

xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yFleche = 584

windowtitle "Ether04_Dewavrin  -  La propagation des ondes dans un milieu à une seule dimension."
locate 1,16:  print "Courbe verte et ‚chelle de gris: le sinus.  Courbe rouge: le cosinus." 
locate th
locate , 13: print "M. Philippe Delmotte a cr‚‚ en juin 2005 un algorithme capable de reproduire"
locate , 13: print "des ondes en s'inspirant des lois de Newton sur l'inertie. En octobre 2006,"
locate , 13: print "M. Anselme Dewavrin a montr‚ que son approche ‚tait apparent‚e … la m‚thode "
locate , 13: print "d'Euler. Vous pouvez consulter le programme oscillations_Delmotte_Dewavrin"
locate , 13: print "pour construire une sinuso‹de grƒce … cette m‚thode, qui en BASIC se r‚sume"
locate , 13: print "… ceci:":?
locate , 13: print "cosinus = 1"
locate , 13: print "pas = lambda / (2 * pi)"
locate , 13: print "DO:   sinus = sinus + cosinus / pas"
locate , 13: print "    cosinus = cosinus - sinus / pas: LOOP"
ligne25$ = " A - Impulsion simple selon la distribution normale (impulsion gaussienne).   "
locate 25, 12: print ligne25$
ligne26$ = " B - Ondes progressives. Le cosinus d‚termine leur sens.                      "
locate 26, 12: print ligne26$
ligne27$ = " C - Ondes stationnaires. Le cosinus est nul au d‚part.                       "
locate 27, 12: print ligne27$
ligne28$ = " I - Impulsion sinuso‹dale stationnaire.                                      "
locate 28, 12: print ligne28$
ligne32$ = " X - Pas de r‚flexion.                                                        "
locate 32, 12: print ligne32$
ligne33$ = " Y - R‚flexion dure.                                                          "
locate 33, 12: print ligne33$
ligne34$ = " Z - R‚flexion molle.                                                         "
locate 34, 12: print ligne34$
ligne35$ = "    I - Initialiser.    ": locate 35, 39: print ligne35$
ligne36$ = "    Quitter (Echap).    ": locate 36, 39: print ligne36$
ligne37$ = "                        "
locate 36: color vert
locate , 3: print "Merci aux cr‚ateurs de FreeBASIC."
locate , 3: print "Gabriel LaFreniŠre. glafreniere.com";
locate 35
locate , 65: print "Le 14 d‚cembre 2006."
locate , 65: print "Ce programme peut ˆtre distribu‚,"
locate , 65: print "copi‚ ou modifi‚ librement.";
color noir
gosub flecheGauche
gosub flecheDroite
gosub Sinusoidales
return
'#############################################################################################

Impulsion:'---------- IMPULSION GAUSSIENNE SELON LA DISTRIBUTION NORMALE ----------------------
xPoint = 400                                              'ramener le repère au centre.
for x = -1 to nombre + 1
  cosinus(x) = 0
  xCoord = abs(x - 400)                                   'impulsion initiale selon la
  yCoord = pi ^ (.002 * -xCoord ^ 2)                      'distribution normale (approx).
  sinus(x) = 70 * yCoord
next
return

Progressives:'-------- Alternative pour ondes progressives: agir sur le cosinus.---------------
xPoint = 400                                              'ramener le repère au centre.
amplitude = 30                                            'amplitude partielle en pixels.
for granule = -1 to nombre + 1                            'exprimée en pixels par passage
  sinus(granule) = 0                                      'dans la boucle.
  cosinus(granule) = 0
next
for granule = 0 to nombre
  periode = 2 * pi * ((granule - nombre / 2) / lambda)
  sinus(granule) = sin(periode)                           'distribution sinusoidale.
  cosinus(granule) = cos(periode)                         'le véritable cosinus.
  xCoord = abs(granule - nombre / 2)                      'impulsion initiale selon la
  yCoord = pi ^ (.000025 * -xCoord ^ 2)                   '...distribution normale approx.
  sinus(granule) = sinus(granule) * amplitude * yCoord
  cosinus(granule) = cosinus(granule) * -constante * amplitude * yCoord
next                                                      'le cosinus dépend d'une constante
                                                          'aparentée au « pas » de l'algorithme
return                                                    'de M. Dewavrin: pas=lambda/(2*pi)

Sinusoidales:'------------------ ONDES SINUSOïDALES INITIALES --------------------------------
xPoint = 400                                              'ramener le repère au centre.
for x = -1 to nombre + 1
  cosinus(x) = 0
  periode = 2 * pi * ((x - 400) / lambda)
  sinus(x) = cos(periode)                                 'distribution sinusoidale.
  affaiblissement = (xCentre - abs(x - xCentre)) / xCentre
  xCoord = abs(x - 400)                                   'impulsion initiale selon la
  yCoord = pi ^ (.0001 * -xCoord ^ 2)                     'distribution normale (approx).
  sinus(x) = 60 * yCoord * sinus(x)
next
return

Stationnaires:'--------------- ONDES STATIONNAIRES SINUSOïDALES ------------------------------
xPoint = 400                                              'ramener le repère au centre.
reflexion = 1
for x = -1 to nombre + 1
  cosinus(x) = 0
  periode = 2 * pi * x / lambda
  sinus(x) = 30 * sin(periode)
next
return

