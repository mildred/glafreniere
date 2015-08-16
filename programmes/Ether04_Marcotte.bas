page1 = 1: nombre = 799                                   'nombre de points.
dim as single potentiel1(-1 to nombre + 1)
dim as single potentiel2(-1 to nombre + 1)
dim as single potentiel3(-1 to nombre + 1)
dim as single pi, periode, xCoord, yCoord
screen 19,24,3: gosub Initialisation                      'créé le 19 février 2006.

do
  swap page1, page2
  screensync
  screenset page1, page2                                  'travailler sur la page cachée.
  pcopy 2, page1                                          'copier la troisième page (texte).
  
'**************************** CALCUL SELON M. JOCELYN MARCOTTE *******************************

  for granule = -1 to nombre + 1                          'mémoriser les deux derniers états
    potentiel3(granule) = potentiel2(granule)             'du potentiel
    potentiel2(granule) = potentiel1(granule)
  next
  for granule = 0 to nombre
    potentiel1(granule) = potentiel2(granule-1) + potentiel2(granule+1) - potentiel3(granule)

'************************************** FIN DU CALCUL ****************************************

    pset (granule, yCentre - potentiel1(granule)), noir   'graphique principal : potentiel.
'    Pset (granule, yCentre - potentiel2(granule)), rouge 'observer le délai entre les courbes
'    Pset (granule, yCentre - potentiel3(granule)), vert  'noires, rouges et vertes.
    luminance = 3.5 * (potentiel1(granule) + 40)
    if luminance > 255 then luminance = 255
    if luminance < 0 then luminance = 0'                  'graphique de luminance.
    line (granule, yCentre - 54)-(granule, yCentre - 64), rgb(luminance,luminance,luminance)
    line (granule, yCentre + 54)-(granule, yCentre + 64), rgb(luminance,luminance,luminance)
  next
  
  if reflexion = 0 then                                   'réflexion nulle (0).
   potentiel1(-1)         = potentiel2(0)
   potentiel1(nombre + 1) = potentiel2(nombre)
  elseif reflexion = 2 then                               'réflexion molle (2).
   potentiel1(-1)         = potentiel1(0)
   potentiel1(nombre + 1) = potentiel1(nombre)
  end if                                                  'réflexion dure (1) par défaut.

  line(xPoint, yCentre - 10)-(xPoint, yCentre + 10),noir  'repères mobiles, vitesse des ondes.
  line(799 - xPoint, yCentre - 10)-(799 - xPoint, yCentre + 10),noir
  xPoint = xPoint + 1: if xPoint > 800 then xPoint = xPoint - 800

  color bleu
  select case reflexion
    case 0: locate 32, 12: print ligne32$
    case 1: locate 33, 12: print ligne33$
    case 2: locate 34, 12: print ligne34$
    end select
  color noir
'------------------------------------- SAISIE CLAVIER ----------------------------------------
  saisie$ = inkey
  if len(saisie$) then
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    select case saisie$
      case chr$(27), "k+": end
      case "A": gosub Impulsion
      case "B": gosub Progressives: image = 0
      case "C": gosub Stationnaires
      case "D": gosub SupprimerMemoire
      case "E": gosub Carrees
      case "F": gosub DentsDeScie
      case "G": gosub Chaleur
      case "I": gosub Initialisation
      case "M": run "Ether00.exe"                         'menu caché; y recourir au besoin.
      case "K+":run "Ether03.exe"                         'flèche gauche.
      case "M+":run "Ether05.exe"                         'flèche droite.        
      case "X": reflexion = 0
      case "Y": reflexion = 1
      case "Z": reflexion = 2
    end select
  end if
'-------------------------------------- SAISIE SOURIS ----------------------------------------
  getmouse xSouris, ySouris, , clic
  ligne = .5 + ySouris / 16
  if xSouris < 86 or xSouris > 712 then ligne = 0
  if ligne < 25 or ligne > 37 then ligne = 0
  if ligne > 34 then
    if xSouris < 304 or xSouris > 496 then ligne = 0
  end if  
  
  if ligne then
    color noir, turquoise: locate ligne, 12               'rehausser l'affichage.
    select case ligne
      case 25: print ligne25$: if clic = 1 then gosub Impulsion
      case 26: print ligne26$: if clic = 1 then gosub Progressives
      case 27: print ligne27$: if clic = 1 then gosub Stationnaires
      case 28: print ligne28$: if clic = 1 then gosub SupprimerMemoire
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
  potentiel1(granule)  = 0
  potentiel2(granule) = 0
next
compteur = 0
for granule = nombre / 2 - 3 * lambda to nombre / 2 + 3 * lambda
  if compteur < lambda then potentiel1(granule) = 20 else potentiel1(granule) = -20
  compteur = compteur + 1: if compteur > 2 * lambda then compteur = 0
next
for granule = 0 to nombre
  potentiel2(granule) = (potentiel1(granule-1) + potentiel1(granule+1)) / 2
next
reflexion = 1
return

Chaleur:'------------------------ Chaleur: distribution au hasard.----------------------------
xPoint = 400
for granule = -1 to nombre + 1
  potentiel1(granule)  = 0
  potentiel2(granule) = 0
next
for granule = -1 to nombre + 1
  xCoord = abs(granule - 400)                             'impulsion initiale selon
  yCoord = pi ^ (.0001 * -xCoord ^ 2)                     'la distribution normale simplifiée.
  potentiel1(granule) = 30 * yCoord * (rnd - .5)           'potentiel au hasard.
next
for granule = 0 to nombre
  potentiel2(granule) = (potentiel1(granule-1) + potentiel1(granule+1)) / 2
next
reflexion = 0                                             'éliminer les ondes progressives.
return

DentsDeScie:'---------------------- Ondes en dents de scie. ----------------------------------
xPoint = 400
for granule = -1 to nombre + 1
  potentiel1(granule)  = 0
  potentiel2(granule) = 0
next
amplit = 0
for granule = nombre / 2 - 3 * lambda to nombre / 2 + 3 * lambda
  amplit = amplit + 1: if amplit > 20 then amplit = -20
  potentiel1(granule) = amplit
next  
for granule = 0 to nombre
  potentiel2(granule) = (potentiel1(granule-1) + potentiel1(granule+1)) / 2
next
reflexion = 1
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
yCentre = 100
lambda = (nombre + 1) / 10
lambdaSurDeux = lambda / 2
gabarit$ = "Comme dans le programme pr‚c‚dent, le potentiel est simplement transmis de"
lignes = 9                                                'nombre de lignes du texte.
th = 14                                                   'haut du texte.
tg = 50 - len(gabarit$) / 2 + 1                           'gauche du texte.
eh = th * 16 - 32                                         'haut de l'encadré.
eb = eh + 16 * lignes + 32                                'bas de l'encadré.
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

locate th
locate , 13: print "Ce programme utilise une variante invent‚e en janvier 2006 par un Qu‚b‚cois," 
locate , 13: print "M. Jocelyn Marcotte. Cette variante produit des ondes dont la vitesse est"
locate , 13: print "plus rapide, soit exactement un pixel par tour de boucle. De plus, les ondes"
locate , 13: print "carr‚es ou en dents de scie demeurent intactes, ce qui est surprenant. Au"
locate , 13: print "contraire, l'algorithme original de M. Philippe Delmotte les disperse peu …"
locate , 13: print "peu en provoquant des vibrations locales qu'on peut comparer … de la chaleur."
print
locate , 13: print "Cette version permet de mieux contr“ler les ph‚nomŠnes de r‚flexion et aussi"
locate , 13: print "de produire des ondes progressives plus facilement."

ligne25$ = " A - Impulsion simple selon la distribution normale (impulsion gaussienne).   "
locate 25, 12: print ligne25$
ligne26$ = " B - Ondes progressives. Leur mise en place est trŠs pr‚cise.                 "
locate 26, 12: print ligne26$
ligne27$ = " C - Ondes stationnaires. Il n'y a pas de diff‚rence de potentiel.            "
locate 27, 12: print ligne27$
ligne28$ = " D - Annuler la diff‚rence de potentiel. Les ondes deviennent stationnaires.  "
locate 28, 12: print ligne28$
ligne29$ = " E - Ondes carr‚es: harmoniques impairs seulement.                            "
locate 29, 12: print ligne29$
ligne30$ = " F - Ondes en dents de scie: tous les harmoniques.                            "
locate 30, 12: print ligne30$
ligne31$ = " G - Cet algorithme ne permet pas de reproduire la chaleur.                   "
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
color vert: locate 36
locate , 2: print "Merci aux cr‚ateurs de FreeBASIC."
locate , 2: print "Gabriel LaFreniŠre. glafreniere.com";
locate 35
locate , 67: print "Le 7 d‚c. 2006. Ce programme"
locate , 67: print "FreeBASIC peut ˆtre distribu‚,"
locate , 67: print "copi‚ ou modifi‚ librement.";
color noir
gosub flecheGauche
gosub flecheDroite
gosub Sinusoidales
return
'#############################################################################################

Impulsion:'------------------- IMPULSION SELON LA DISTRIBUTION NORMALE -----------------------
xPoint = 400
reflexion = 1
potentiel1(-1) = 0                                        'effacer les extrémités.
potentiel1(nombre+1) = 0
potentiel2(nombre) = 0
for granule = 0 to nombre
  xCoord = abs(granule - 400)                             'impulsion initiale selon la
  yCoord = pi ^ (.002 * -xCoord ^ 2)                      '  distribution normale  simplifiée.
  potentiel1(granule) = 50 * yCoord
  potentiel3(granule) = 0
next
for granule = 0 to nombre
  potentiel2(granule) = (potentiel1(granule-1) + potentiel1(granule+1)) / 2
next
return

Progressives:'---- Ondes progressives, une seule étape (pas de quart d'onde ajouté)-----------
xPoint = 400
reflexion = 2
potentiel1(-1) = 0                                        'effacer les extrémités.
potentiel1(nombre+1) = 0
potentiel2(nombre) = 0

for granule = 0 to nombre
  periode = 2 * pi * ((granule - nombre / 2) / lambda)
  potentiel1(granule) = sin(periode)                      'distribution sinusoidale.
  xCoord = abs(granule - nombre / 2)                      'atténuer les extrémités selon la
  yCoord = pi ^ (.000025 * -xCoord ^ 2)                   '           distribution normale.
  amplitude = 25                                          'amplitude maximum en pixels.
  potentiel1(granule) = potentiel1(granule) * amplitude * yCoord
  potentiel2(granule-1) = potentiel1(granule)
next
return

Sinusoidales:'------------------ ONDES SINUSOïDALES INITIALES --------------------------------
xPoint = 400
reflexion = 2
potentiel1(-1) = 0                                        'effacer les extrémités.
potentiel1(nombre+1) = 0

for granule = 0 to nombre
  periode = 2 * pi * ((granule - 400) / lambda)
  potentiel1(granule) = cos(periode)                      'distribution sinusoidale.
  xCoord = abs(granule - 400)                             'atténuer les extrémités selon la
  yCoord = pi ^ (.0001 * -xCoord ^ 2)                     'distribution normale simplifiée.
  potentiel1(granule) = 50 * yCoord * potentiel1(granule)
next
for granule = 0 to nombre
  potentiel2(granule) = (potentiel1(granule-1) + potentiel1(granule+1)) / 2
next
return

Stationnaires:'--------------- ONDES STATIONNAIRES SINUSOïDALES ------------------------------
xPoint = 400
reflexion = 1
for granule = -1 to nombre + 1
  periode = 2 * pi * granule / lambda
  potentiel1(granule) = 2 * sin(periode)
  potentiel2(granule) = 0
next
return

SupprimerMemoire:'------ Supprimer la mémoire produit des ondes stationnaires.----------------
'----------------------- Ici, il s'agit plutôt d'annuler la différence de potentiel: 0 volt!
for granule = 0 to nombre
  potentiel2(granule) = (potentiel1(granule-1) + potentiel1(granule+1)) / 2
next
return
