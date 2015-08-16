page1 = 1: nombre = 799                                   'nombre de points.
dim as single potentiel(-1 to nombre + 2)
dim as single memoire(-1 to nombre + 1)
dim as single influence(-1 to nombre + 1)
dim as single pi, periode, xPoint, xCoord, yCoord, temps, constante, vitesse, affaiblissement
screen 19,24,3: gosub Initialisation

do
  swap page1, page2                                       'permuter les pages (plus rapide).
'  screensync
  screenset page1, page2                                  'travailler sur la page cachée.
  pcopy 2, page1                                          'copier la troisième page (texte).
  
  if reflexion = 0 then                                   'réflexion nulle (0).
    potentiel(-1) = potentiel(0) - memoire(0) / vitesse
    potentiel(nombre + 1) = potentiel(nombre) - memoire(nombre) / vitesse
  elseif reflexion = 2 then                               'réflexion molle (2).
   potentiel(-1) = potentiel(0)
   potentiel(nombre + 1) = potentiel(nombre)
  end if                                                  'réflexion dure (1) par défaut.
  
  influence(0) = (potentiel(-1) + potentiel(1)) / 2 - potentiel(0)  'un cran d'avance.

for granule = 0 to nombre
  
' Cet algorithme est semblable à celui que M. Philippe Delmotte a mis au
' point en juin 2005 en faisant intervenir la mécanique de Newton.
' Ici, la vitesse « c » de l'onde est de 0,707 (cos 45°) pixel par passage dans la boucle.
' On peut porter cette vitesse à 1 pixel exactement en doublant la valeur de l'influence:
' influence(granule+1)=(potentiel(granule)+potentiel(granule+2))-2*potentiel(granule + 1)
' Mais c'est au prix d'un comportement différent, en particulier en ce qui concerne les
' réflexions, la « chaleur », et les ondes carrées ou en dents de scie.

'****** CALCUL SELON L'ALGORITHME DE M. PHILIPPE DELMOTTE, BASÉ SUR LES LOIS DE NEWTON ********

    influence(granule + 1) = (potentiel(granule) + potentiel(granule + 2)) / 2 - potentiel(granule + 1)
    memoire(granule) = memoire(granule) + (influence(granule) / 1)
    potentiel(granule) = potentiel(granule) + memoire(granule)
    
'************************************** FIN DU CALCUL ****************************************

    pset (granule, yCentre-potentiel(granule)), vert                         'courbe potentiel.
    pset (granule, yCentre+(lambda/(2*pi)/vitesse) * memoire(granule)), rouge'courbe memoire.
    luminance = 3.5 * (potentiel(granule) + 40)
    if luminance > 255 then luminance = 255
    if luminance < 0 then luminance = 0
    line (granule, yCentre + 60)-(granule, yCentre + 90), rgb(luminance,luminance,luminance)
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
    select case saisie$                                   'exécution des commandes clavier.
      case chr$(27), "k+", "X+": end
      case "A": gosub Impulsion
      case "B": gosub Progressives
      case "C": gosub Stationnaires
      case "D": gosub Supprimermemoire
      case "E": gosub Carrees
      case "F": gosub DentsDeScie
      case "G": gosub Chaleur
      case "I": gosub Initialisation
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
      case 28: print ligne28$: if clic = 1 then gosub Supprimermemoire
      case 29: print ligne29$: if clic = 1 then gosub Carrees
      case 30: print ligne30$: if clic = 1 then gosub DentsDeScie
      case 31: print ligne31$: if clic = 1 then gosub Chaleur
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

Carrees:'------------------------------ Ondes carrées. ---------------------------------------
xPoint = 400
for granule = -1 to nombre + 1
  memoire(granule) = 0
  potentiel(granule) = 0
next
compteur = 0
for granule = nombre / 2 - 3 * lambda to nombre / 2 + 3 * lambda
  if compteur < lambda then potentiel(granule) = 20 else potentiel(granule) = -20
  compteur = compteur + 1: if compteur > 2 * lambda then compteur = 0
next
return

Chaleur:'------------------------ Chaleur: distribution au hasard.----------------------------
xPoint = 400
for granule = -1 to nombre + 1
  memoire(granule) = 0
  xCoord = abs(granule - 400)                             'impulsion initiale selon
  yCoord = pi ^ (.0001 * -xCoord ^ 2)                     'la distribution normale approx.
  potentiel(granule) = 10 * yCoord * (rnd - .5)           'potentiel au hasard.
next
reflexion = 0                                             'éliminer les ondes progressives.
return

DentsDeScie:'---------------------- Ondes en dents de scie. ----------------------------------
xPoint = 400
for granule = -1 to nombre + 1
  memoire(granule) = 0
  potentiel(granule) = 0
next
amplit = 0
for granule = nombre / 2 - 3 * lambda to nombre / 2 + 3 * lambda
  amplit = amplit + 1: if amplit > 20 then amplit = -20
  potentiel(granule) = amplit
next  
return

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

reflexion = 1
pi = 4 * atn(1)
xCentre = 400
yCentre = 80
vitesse = 1 / sqr(2)                                      'vitesse de l'onde en pixels.
lambda = (nombre + 1) / 10                                'pour ondes stationnaires stables.
lambdaSurDeux = lambda / 2
lambdaSurQuatre = lambda / 4
gabarit$ = "                                                                          "
lignes = 10                                               'nombre de lignes du texte.
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

windowtitle "Ether04  -  La propagation des ondes dans un milieu à une seule dimension selon M. Philippe Delmotte."
locate 1,16:  print "Courbe verte et ‚chelle de gris: le potentiel.  Courbe rouge: la m‚moire." 
locate th
locate , 13: print "Comme dans le programme pr‚c‚dent, l'‚nergie est simplement transmise de gra-"
locate , 13: print "nule en granule. Elle peut ˆtre positive ou n‚gative, d'o— les tons clairs ou"
locate , 13: print "sombres montr‚s ci-dessus. Ce programme utilise l'algorithme de M. Philippe  "
locate , 13: print "Delmotte, ‚labor‚ en juin 2005 selon les lois de Newton. Il n'exige que trois"
locate , 13: print "lignes de code. La ® m‚moire ¯ peut ˆtre apparent‚e au cosinus.":?
locate , 13: print "L'addition des ondes progressives produit des ondes stationnaires. Les ondes "
locate , 13: print "en dents de scie montrent que les harmoniques sont peu compatibles avec une  "
locate , 13: print "structure granuleuse de l'‚ther. Leur ‚nergie serait convertie en chaleur.   "
locate , 13: print "On obtient des ondes progressives en introduisant une m‚moire."
ligne25$ = " A - Impulsion simple selon la distribution normale (impulsion gaussienne).   "
locate 25, 12: print ligne25$
ligne26$ = " B - Ondes progressives. La memoire d‚termine leur sens.                      "
locate 26, 12: print ligne26$
ligne27$ = " C - Ondes stationnaires. La memoire est nul au d‚part.                       "
locate 27, 12: print ligne27$
ligne28$ = " D - Effacer la memoire. Les ondes progressives deviennent stationnaires.     "
locate 28, 12: print ligne28$
ligne29$ = " E - Ondes carr‚es: harmoniques impairs seulement.                            "
locate 29, 12: print ligne29$
ligne30$ = " F - Ondes en dents de scie: les harmoniques ‚lev‚s se d‚gradent.             "
locate 30, 12: print ligne30$
ligne31$ = " G - Chaleur: vibrations qui se dispersent plus lentement que les ondes.      "
locate 31, 12: print ligne31$
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
locate , 65: print "Le 9 d‚cembre 2006."
locate , 65: print "Ce programme peut ˆtre distribu‚,"
locate , 65: print "copi‚ ou modifi‚ librement.";
color noir
gosub flecheGauche
gosub flecheDroite
gosub sinusoidales
return
'#############################################################################################

Impulsion:'----------------- IMPULSION SELON LA DISTRIBUTION NORMALE -------------------------
xPoint = 400
reflexion = 1
for x = -1 to nombre + 1
  memoire(x) = 0
  xCoord = abs(x - 400)                                   'impulsion initiale selon la
  yCoord = pi ^ (.002 * -xCoord ^ 2)                      'distribution normale (approx).
  potentiel(x) = 70 * yCoord
next
return

Progressives:'-------- Méthode pour ondes progressives: agir sur la memoire.-------------
xPoint = 400
reflexion = 2
amplitude = 30                                            'amplitude partielle en pixels.
constante = pi / lambda / vitesse                         'selon la vitesse de l'onde (1/sqr(2)).
for granule = -1 to nombre + 1                            '   exprimée en pixels par passage
  potentiel(granule) = 0                                  '   dans la boucle.
  memoire(granule) = 0
next
for granule = 0 to nombre
  periode = 2 * pi * ((granule - nombre / 2) / lambda)
  potentiel(granule) = sin(periode)                       'distribution sinusoïdale.
  memoire(granule) = cos(periode)
  xCoord = abs(granule - nombre / 2)                      'impulsion initiale selon la
  yCoord = pi ^ (.000025 * -xCoord ^ 2)                   '   distribution normale approx.
  potentiel(granule) = potentiel(granule) * amplitude * yCoord
  memoire(granule) = memoire(granule) * -constante * amplitude * yCoord
next
return

sinusoidales:'------------------ ONDES SINUSOÏDALES INITIALES --------------------------------
xPoint = 400
reflexion = 2
for x = -1 to nombre + 1
  memoire(x) = 0
  periode = 2 * pi * ((x - 400) / lambda)
  potentiel(x) = cos(periode)                             'distribution sinusoïdale.
  affaiblissement = (xCentre - abs(x - xCentre)) / xCentre
  xCoord = abs(x - 400)                                   'impulsion initiale selon la
  yCoord = pi ^ (.0001 * -xCoord ^ 2)                     'distribution normale (approx).
  potentiel(x) = 60 * yCoord * potentiel(x)
next
return

Stationnaires:'--------------- ONDES STATIONNAIRES SINUSOïDALES ------------------------------
xPoint = 400                                              'ramener le repère au centre.
reflexion = 1
for x = -1 to nombre + 1
  memoire(x) = 0
  periode = 2 * pi * x / lambda
  potentiel(x) = 30 * sin(periode)
next
return

Supprimermemoire:'--------- Supprimer les données relatives au memoire (la mémoire).---------------------
for granule = -1 to nombre + 1
  memoire(granule) = 0
next
return
