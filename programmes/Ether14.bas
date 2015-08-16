dim image1(10000)
dim as single amplitude(largeur,hauteur)
dim as single facteur, xCoord, contraste, gamma, rayon, amplit, xPrime, tPrime
dim as single pi, deuxPi, angle, phase, flottante, theta, distance, temps, deltaTemps
dim as single DopplerAvant, DopplerArriere, rapport, xCentre, xGauche, xDroit, xRetour
dim as single beta, facteurG, pas, phase2, chrono, tempsLocal, x, rayonSignal, rayonEcho
screen 19,24,3: page1 = 1: gosub Initialisation

do'                        CALCUL DES TRANSFORMATIONS DE LORENTZ.
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  Saisie$ = inkey
  if len(Saisie$) then
    capture = 0
    if len(Saisie$) = 2 then Saisie$ = right(Saisie$, 1) + "+" else Saisie$ = ucase(Saisie$)
    select case Saisie$
      case "A": mode$ = "accélération"
      case "D": mode$ = "départ"
                xCentre = origine
      case "I": gosub Initialisation
      case "P": mode$ = "pause"
                xCentre = origine + beta * secondeLumiere
      case "T": mode$ = "translation"
      case "R": mode$ = "renseignements"
                chrono = 1'                               'confirmer les équations du tableau.
                if vitesse$ = "0" then vitesse$ = ".866"
      case "k+", "X+", chr(27): end
      
      case "M": run "Ether00.exe"
      case "K+":run "Ether13.exe"                         'flèche gauche.
      case "M+":run "Ether15.exe"                         'flèche droite.  
      case "1": vitesse$ = ".1"
      case "2": vitesse$ = ".2"
      case "3": vitesse$ = ".3"
      case "4": vitesse$ = ".4"
      case "5": vitesse$ = ".5"
      case "6": vitesse$ = ".6"
      case "7": vitesse$ = ".707"
      case "8": vitesse$ = ".8"
      case "9": vitesse$ = ".866"
      case "0": vitesse$ =  "0"
      case "+": if mode$ <> "déplacement" then mode$ = "déplacement": gosub MiseAJour
      case "-": if mode$ <> "déplacement" then mode$ = "déplacement": gosub MiseAJour
      case chr$(13): texte = texte + 1
                if texte > 5 then texte = 0
      case else: Saisie$ = "nul"
    end select
    do: loop while len(inkey)                             'vider le tampon.
    if Saisie$ = "nul" then else gosub MiseAJour
  end if

  getmouse xSouris, ySouris, , clic                       'saisie Souris.
  ligne = .5 + ySouris / 16
  if ligne > 22 and ligne < 35 then ligne = 0
  if ligne < 23 then
    if ligne < 7 or xSouris < 552 then ligne = 0
  elseif ligne > 34 then
    if ligne > 37 or xSouris < 304 or xSouris > 496 then ligne = 0
  end if

  if clic = 1 then
    select case ligne
      case 7:  vitesse$ =  "0"
      case 8:  vitesse$ = ".1"
      case 9:  vitesse$ = ".2"
      case 10: vitesse$ = ".3"
      case 11: vitesse$ = ".4"
      case 12: vitesse$ = ".5"
      case 13: vitesse$ = ".6"
      case 14: vitesse$ = ".707"
      case 15: vitesse$ = ".8"
      case 16: vitesse$ = ".866"
      case 17: clic = 0
      case 18: mode$ = "départ"
      case 19: mode$ = "pause"
      case 20: mode$ = "translation"
      case 21: mode$ = "accélération"
      case 22: mode$ = "renseignements"
               chrono = 1'                                'confirmer les équations du tableau.
               if vitesse$ = "0" then vitesse$ = ".866"
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether13.exe" else run "Ether15.exe"
      case else:
                 ligne = 0
    end select
    if ligne then clic = 0: gosub MiseAJour
    if clic = 1 and mode$ <> "déplacement" then mode$ = "déplacement": gosub MiseAJour
  end if
  
  select case mode$
    case "pause":          gosub Pause
    case "départ" :        gosub Depart
    case "déplacement":    gosub Deplacement
    case "translation":    gosub Translation
    case "accélération":   gosub Acceleration
    case "renseignements": gosub Renseignements
  end select
  color , turquoise: locate ligne, 70
  select case ligne                                       'rehausser l'affichage.
    case 7:  if vitesse$ <> "0"    then print ligne07$
    case 8:  if vitesse$ <> ".1"   then print ligne08$
    case 9:  if vitesse$ <> ".2"   then print ligne09$
    case 10: if vitesse$ <> ".3"   then print ligne10$
    case 11: if vitesse$ <> ".4"   then print ligne11$
    case 12: if vitesse$ <> ".5"   then print ligne12$
    case 13: if vitesse$ <> ".6"   then print ligne13$
    case 14: if vitesse$ <> ".707" then print ligne14$
    case 15: if vitesse$ <> ".8"   then print ligne15$
    case 16: if vitesse$ <> ".866" then print ligne16$
    case 18: if mode$ <> "départ"         then print ligne18$
    case 19: if mode$ <> "pause"          then print ligne19$
    case 20: if mode$ <> "translation"    then print ligne20$
    case 21: if mode$ <> "accélération"   then print ligne21$
    case 22: if mode$ <> "renseignements" then print ligne22$
    case 35: locate, 39: print ligne35$
    case 36: locate, 39: print ligne36$
    case 37: locate, 39: print ligne37$;
             if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select

  color noir, fond
  if bitmap then gosub Bitmaps                            'capture d'images si désiré.
  image = image + 1: if image > images then image = 1
loop

'--------------------------------------- ACCÉLÉRATION ----------------------------------------
Acceleration:
beta = beta + .01                                         'pas d'un centième.
if beta > .866 then beta = 0
facteurG = sqr(1 - beta ^ 2)                              'facteur de contraction de Lorentz.
deltaTemps = -x * beta                                    'décalage horaire, temps apparent.
temps = (x - deltaTemps) / facteurG                       'temps absolu à écouler.
xCentre = origine + diametre * beta * temps               'point avec t' = x.
'xCentre = origine + beta * secondeLumiere                'point avec t = 1.
'xCentre = origine + rayon * beta / facteurG              'point d'égalité des deux ondes.
gosub Diagramme
color rouge, blanc
line(138,300)-(237,321),blanc, bf
line(138,300)-(237,321),noir, b
locate 20, 19: print "Bˆta = ";: print using "#.##"; beta
sleep 100
if clic = 1 then mode$ = "déplacement": gosub MiseAJour: return
color noir
return

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

'------------------------------------- POINT DE DÉPART ---------------------------------------
Depart:
xCentre = origine
gosub Diagramme
return

'--------------------------------- DÉPLACEMENT AVEC LA SOURIS --------------------------------
Deplacement:
gosub Diagramme
if beta = 0 then return
if clic = 1 then
  do
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    if xSouris > -1 then xCentre = xSouris
    gosub Diagramme
    getmouse xSouris, ySouris, , clic                     'saisie Souris continuelle.
  loop while clic = 1
end if

if Saisie$ = "+" or Saisie$ = "-" then
xCentre = int(xCentre)                                    'pas d'un pixel exactement.
  do
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    if Saisie$ = "+" then xCentre = xCentre + 1
    if Saisie$ = "-" then xCentre = xCentre - 1
    gosub Diagramme
    Saisie$ = inkey                                       'saisie clavier continuelle.
  loop until Saisie$ = "" 
end if  
return

'---------------------------------- AFFICHER LE DIAGRAMME ------------------------------------
Diagramme:
color noir, fond
circle(origine, yCentre), rayon, grisClair                'sphère au repos.
circle(xCentre, yCentre), rayon, noir,,,1 / facteurG      'mobile contracté.
line(xCentre, yCentre - rayon)-(xCentre, yCentre + rayon), noir   'repère, centre du mobile.
line(origine - rayon, yCentre)-(origine + rayon, yCentre), grisClair
line(xCentre - facteurG * rayon, yCentre)-(xCentre + facteurG * rayon, yCentre), grisClair
distance = (xCentre - origine) / secondeLumiere           'distance en secondes lumière.
if beta then chrono = distance / beta else chrono = 0     'chronomètre en temps absolu.
rayonSignal = (xCentre - origine) / beta                  'rayon de l'onde émise à x=0, t=0.
xRetour = origine + diametre * beta / facteurG            'point de réception de l'écho radar.
rayonEcho = (xRetour - xCentre) / beta                    'rayon de l'onde écho radar.
if rayonSignal < 0 then rayonSignal = 0
if rayonEcho < 0 then rayonEcho = 0
if rayonSignal > 1000 then rayonSignal = 0
if rayonEcho > 1000 then rayonEcho = 0
circle(origine, yCentre), rayonSignal, bleuClair          'onde émise par O à x=0, t=0.
circle(xRetour, yCentre), rayonEcho, bleuClair            'onde reçue par O au point xRetour.
line(origine - 60, yCentre - 87)-(origine + 60, yCentre - 35), blanc, bf' système au repos
line(origine - 60, yCentre - 87)-(origine + 60, yCentre - 35), noir, b  ' (sert de gabarit).

if vitesse$ = ".707" then                                 'préciser les points hors-série.
  line(origine + .707 * diametre, diametre - 40)-(origine + .707 * diametre, 20 + diametre), noir
  locate 20,50: print vitesse$
elseif vitesse$ = ".866" then
  line(origine + .866 * diametre, diametre - 40)-(origine + .866 * diametre, 20 + diametre), noir
  locate 20,57: print vitesse$
end if
color noir, blanc

xCoord = origine + rayon * beta / facteurG                'point d'égalité des deux ondes.
xCoord = xCoord - rayon / (beta * facteurG)               'point de départ de l'onde de phase.
if beta then xPhase = xCoord + diametre * chrono / beta else xPhase = origine' onde de phase.
locate 7, 18: print "t'= x = 0,5"                         'à coller vis-à-vis le repère.
locate 8, 18: print "Onde de phase"
locate 9, 18: print "v = 1 / beta"
get(128,93)-(248,145), image1                             'copier et coller.
line(xPhase, yCentre - rayon)-(xPhase, yCentre + rayon), bleuClair' repère de l'onde de phase.
if beta then put(xPhase - 60, yCentre - 140), image1, pset 'boîte sur l'onde de phase.
line(origine - 59, yCentre - 86)-(origine + 59, yCentre - 36), blanc, bf 'effacer
tempsLocal = facteurG * chrono
locate 7, 18: print using "x'= #.###"; distance           'à coller au centre du mobile.
locate 8, 18: print "Temps local O"
locate 9, 18: print using "t'= #.### sec"; tempsLocal     'temps apparent au centre du mobile.
get(128,93)-(248,145), image1                             'copier et coller au centre du mobile.
put(xCentre - 60,yCentre + 37), image1, pset              'boîte au centre du mobile.
line(origine - 59, yCentre - 86)-(origine + 59, yCentre - 36), blanc, bf 'effacer

color marron, blanc                                        'repère fixe, ou référence.
line(origine + rayon - 50, yCentre - 72)-(origine  + rayon + 50, yCentre - 35), blanc, bf 'effacer
line(origine + rayon - 50, yCentre - 72)-(origine  + rayon + 50, yCentre - 35), noir, b
line(origine + rayon - 6, yCentre - 35)-(origine  + rayon + 6, yCentre - 35), 1
line(origine + rayon - 6, yCentre - 35)-(origine  + rayon, yCentre - 10), 1
line(origine + rayon + 6, yCentre - 35)-(origine  + rayon, yCentre - 10), 1
paint(origine + rayon, yCentre - 34),noir, 1
locate 8,39: print "RepŠre fixe"
locate 9,39: print "  x = 0,5  "
xGauche = xCentre - facteurG * rayon                      'x gauche en pixels.
line(xGauche, yCentre)-(xGauche, yCentre + rayon), noir   'repère, x gauche.
xDroit = xCentre + facteurG * rayon                       'x droit en pixels.
line(xDroit, yCentre)-(xDroit, yCentre + rayon), noir     'repère, x droit.
xPrime = (xCentre - origine + facteurG * rayon) / secondeLumiere 'x droit en secondes lumière.
xPrime = facteurG * x + beta * chrono                     'même valeur, calcul plus correct.
tPrime = facteurG * chrono - x * beta                     'temps local à droite du mobile.
locate 7, 18: print using "x'= #.###"; xPrime             'à coller à droite du mobile.
locate 8, 18: print "Temps local B"
locate 9, 18: print using "t'= #.### sec"; tPrime             'temps apparent à droite du mobile.
get(128,93)-(248,145), image1                             'copier et coller à droite du mobile.
put(xDroit - 60, yCentre - 25), image1, pset              'boîte à droite du mobile.
line(origine - 59, yCentre - 86)-(origine + 59, yCentre - 36), blanc, bf' effacer

gosub Equations
color noir, blanc
xPrime = (xCentre - origine - facteurG * rayon) / secondeLumiere 'x gauche en secondes lumière.
tPrime = facteurG * chrono + x * beta                     'temps local à gauche du mobile.
locate 7, 18: print using "x'= #.###"; xPrime             'à transférer à gauche du mobile.
locate 8, 18: print "Temps local A"
locate 9, 18: print using "t'= #.### sec"; tPrime         'temps apparent à gauche du mobile.
get(128,93)-(248,145), image1                             'copier et coller à gauche du mobile.
put(xGauche - 60, yCentre - 25), image1, pset             'boîte à gauche du mobile.
line(origine - 59, yCentre - 86)-(origine + 59, yCentre - 36), blanc, bf' effacer
locate 7, 22: print "x = 0"
locate 8, 19: print "Temps absolu"
locate 9, 18: print using "t = #.### sec"; chrono
color noir, fond
return

'------------------------------------ TABLEAU DES ÉQUATIONS ----------------------------------
Equations:
line(525,362)-(792,437), blanc, bf
line(525,362)-(792,437), noir, b
xPrime = facteurG * x + beta * chrono                     'même valeur, calcul plus correct.
tPrime = facteurG * chrono - x * beta                     'temps local à droite du mobile.
color marron, blanc                                       'afficher les équations en marron.
locate 24, 68: print "x'= +";                             'utiliser les équations véritables
if xPrime > 0 then locate , 73 else locate , 72           'pour les valider.
print using "#.### "; facteurG * x + beta * chrono;: print "= g * x + beta * t"
locate 25, 68: print "t'= +";
if tPrime > 0 or beta = 0 then locate , 73 else locate , 72 
print using "#.###"; facteurG * chrono - beta * x;:  print " = g * t - bˆta * x"
locate 26, 68: print "x = +";                             'valeur fixe en guise d'exemple.
locate , 73: print using "#.###"; (xPrime - beta * chrono) / facteurG;: print " = (x'- beta * t) / g"
locate 27, 68: print "t = +";
if chrono >= 0 then locate , 73 else locate , 72 
print using "#.###"; (tPrime + beta * x) / facteurG ;: print " = (t'+ bˆta * x) / g"
color noir, fond
return

'-------------------------------- MISE À JOUR DES DONNÉES ------------------------------------
MiseAJour:
screenset 2
color noir, fond
locate 7, 70:  print ligne07$
locate 8, 70:  print ligne08$
locate 9, 70:  print ligne09$
locate 10, 70: print ligne10$
locate 11, 70: print ligne11$
locate 12, 70: print ligne12$
locate 13, 70: print ligne13$
locate 14, 70: print ligne14$
locate 15, 70: print ligne15$
locate 16, 70: print ligne16$
locate 18, 70: print ligne18$
locate 19, 70: print ligne19$
locate 20, 70: print ligne20$
locate 21, 70: print ligne21$
locate 22, 70: print ligne22$

color bleu, fond
beta = val(vitesse$)
select case vitesse$                                      'indiquer la vitesse en cours.
  case "0":    locate 7, 70:  print ligne07$
               mode$ = "départ"                           'translation impossible
  case ".1":   locate 8, 70:  print ligne08$
  case ".2":   locate 9, 70:  print ligne09$
  case ".3":   locate 10, 70: print ligne10$
  case ".4":   locate 11, 70: print ligne11$
  case ".5":   locate 12, 70: print ligne12$
  case ".6":   locate 13, 70: print ligne13$
  case ".707": locate 14, 70: print ligne14$
               beta = sin(pi / 4)
  case ".8":   locate 15, 70: print ligne15$
  case ".866": locate 16, 70: print ligne16$
               beta = sin(pi / 3)
end select
select case mode$                                         'indiquer le mode en cours.
  case "départ":         locate 18, 70: 
                         if vitesse$ = "0" then print ligne18a$ else print ligne18$
                         xCentre = origine
  case "pause":          locate 19, 70: print ligne19$
                         xCentre = origine + beta * secondeLumiere
  case "translation":    locate 20, 70: print ligne20$
                         xCentre = 0
  case "accélération":   locate 21, 70: print ligne21$
  case "renseignements": locate 22, 70: print ligne22$
end select
'                               CALCUL DES VARIABLES DE BASE
color noir, fond
theta = asin(beta)                                        'inclinaison des ondes transversales.
facteurG = sqr(1 - beta ^ 2)                              'facteur de contraction de Lorentz.
gamma = 1 / facteurG                                      'facteur gamma de la Relativité.
deltaTemps = -x * beta
locate 30, 64: print "Bˆta: ";: print using "#.####"; beta;: print " = sin(thˆta) = v / c"
locate 31, 64: print "Thˆta: ";: print using "##.###"; theta * 180 / pi;: print chr(248);: print " = arc sin(bˆta)"
locate 32, 64: print "Facteur g: ";: print using "#.####"; facteurG;: print " = sqr(1 - beta ^ 2)"
locate 33, 64: print "Facteur gamma: ";: print using "#.####"; gamma;: print " = 1 / g"
locate 34, 64: print "D‚calage horaire: ";: print using "#.###"; deltaTemps;: print " = -x * beta"
screenset page1, page2
return

'--------------------------------- PAUSE APRÈS UNE SECONDE -----------------------------------
Pause:
xCentre = origine + beta * secondeLumiere
gosub Diagramme
return

'-------------------------------------- RENSEIGNEMENTS ---------------------------------------
Renseignements:
line(0,0)-(548,371), fond, bf
line(0,6)-(548,371), noir, b
line(1,7)-(547,370), noir, b
color noir, fond
select case texte
  case 0: gosub texte0
  case 1: gosub texte1
  case 2: gosub texte2
  case 3: gosub texte3
  case 4: gosub texte4
  case 5: gosub texte5
end select
locate 23, 25: print "Appuyez sur [ Entr‚e ]."
gosub Equations
return

'-------------------------------------- RENSEIGNEMENTS ---------------------------------------
texte0:
locate 1, 27:print " RENSEIGNEMENTS "
print
locate ,2: print "Utilisez la souris ou les touches [+ -] pour d‚placer le mobile."
locate ,2: print "Pour modifier la vitesse bˆta, appuyez sur un chiffre de 1 … 9."
print
locate ,2: print "Pour ne pas compliquer inutilement les calculs, on ne considŠre"
locate ,2: print "ici qu'une seule distance en abscisses, soit x = 0,5 de maniŠre …"
locate ,2: print "obtenir une sphŠre dont le diamŠtre mesure une seconde lumiŠre.  "
print
locate ,2: print "On pr‚sume que cette sphŠre est occup‚e par trois observateurs. B "
locate ,2: print "est … l'avant, l'amiral O est au centre et A … est l'arriŠre. Ils "
locate ,2: print "subiront une contraction selon le facteur g de Lorentz. Les horlo-"
locate ,2: print "ges indiqueront des secondes plus lentes qui pr‚senteront un d‚ca-"
locate ,2: print "lage horaire de";: print using " #.### SECONDE LOCALE"; -x * beta;: print " selon: -x * beta."
print
locate ,2: print "Un observateur fixe post‚ … l'origine (x = 0) demande … l'observa- "
locate ,2: print "teur central de remettre ses horloges … z‚ro au moment o— ils se   "
locate ,2: print "croisent. Ce dernier ‚met un signal pour que les deux autres obser-"
locate ,2: print "vateurs synchronisent leurs horloges. Sachant qu'ils sont distants "
locate ,2: print "d'une demi-seconde lumiŠre de l'amiral O, tous les observateurs si-"
locate ,2: print "tu‚s sur la surface de la sphŠre r‚gleront leurs horloges … 0,5 sec"
locate ,2: print "dŠs la r‚ception du signal... L'effet Doppler est diabolique!"
return

'-------------------------------- TEXTE SUR L'EFFET DOPPLER ----------------------------------
texte1:
locate 1, 20:print " L'EFFET DOPPLER DE LA MATIERE "
print
locate ,2: print "Parce que sa fr‚quence ralentit avec sa vitesse selon le facteur g "
locate ,2: print "de Lorentz, la matiŠre est sujette … un effet Doppler trŠs particu-"
locate ,2: print "lier. Les champs de force et la lumiŠre qu'elle ‚met sont eux aussi"
locate ,2: print "soumis … cet effet Doppler. Ainsi son fonctionnement demeure stable"
locate ,2: print "et un observateur entraŒn‚ avec elle se croit toujours au repos."
print
locate ,2: print "Effet Doppler en fr‚quence:"
DopplerAvant = facteurG / (1 - beta)
locate ,2: print "avant:   F'= ";: print using "#.### F "; DopplerAvant;
print " = g / (1 - beta) =  sqr(2 / (1 - beta) - 1)"
DopplerArriere = facteurG / (1 + beta)
locate ,2: print "arriŠre: F'= ";: print using "#.### F "; DopplerArriere;
print " = g / (1 + beta) =  sqr(2 / (1 + beta) - 1)"
print
locate ,2: print "Effet Doppler en longueur d'onde: lambda' ="
DopplerAvant = (1 - beta) / facteurG
locate ,2: print "avant:   ";: print using "#.### lambda"; DopplerAvant;
print " = (1 - beta) / g  =  sqr(2 / (1 + beta) - 1)"
DopplerArriere = (1 + beta) / facteurG
locate ,2: print "arriŠre: ";: print using "#.### lambda"; DopplerArriere;
print " = (1 + beta) / g  =  sqr(2 / (1 - beta) - 1)"
print
locate ,2: print "R‚ciprocit‚:"
locate ,2: print "              fr‚quence avant = 1 / fr‚quence arriŠre."
locate ,2: print "              fr‚quence avant = longueur d'onde arriŠre."
print
locate ,2: print "La masse de la matiŠre augmente … cause de cet effet Doppler:"
locate ,2: print "m' = ";: print using "#.#### kg"; (DopplerAvant + DopplerArriere) / 2;
           print "  =  (avant + arriŠre) / 2  =  facteur gamma."
return

'------------------------------- TEXTE SUR LE SCANNER DU TEMPS -----------------------------------
texte2:
locate 1, 25: print " LE SCANNER DU TEMPS "
print
locate ,2: print "En mars 2004, j'inventais un dispositif capable de reproduire les  "
locate ,2: print "effets des transformations de Lorentz. Je m'‚tais inspir‚ des cam‚-"
locate ,2: print "ras … scanner, qui produisent des images selon un balayage prolon- "
locate ,2: print "g‚ dans le temps. Il est clair que deux horloges espac‚es photogra-"
locate ,2: print "phi‚es de cette maniŠre n'indiqueront plus la mˆme heure. De plus, "
locate ,2: print "la vitesse d'impression peut ˆtre plus lente que la vitesse de ba- "
locate ,2: print "layage, ce qui permet d'obtenir des images contract‚es conform‚ment"
locate ,2: print "au facteur de contraction g de Lorentz."
print
locate ,2: print "J'ai alors d‚couvert avec stup‚faction que ce dispositif produisait"
locate ,2: print "un effet Doppler parfait. Or cela n'a rien de surprenant car c'est "
locate ,2: print "Woldemar Voigt qui a mis au point les ‚quations dont s'est inspir‚ "
locate ,2: print "Lorentz, pr‚cis‚ment dans le but de corriger l'effet Doppler."
print
locate ,2: print "C'est pour cette raison que les ‚quations de Lorentz ont la propri-"
locate ,2: print "‚t‚ de corriger l'effet Doppler. En permutant les variables x et x'"
locate ,2: print "on peut donc le provoquer. Cela d‚montre qu'il faut aussi permuter "
locate ,2: print "ces variables pour obtenir la contraction de la matiŠre, ce qui est"
locate ,2: print "parfaitement conforme … la pens‚e de Lorentz."
return

'-------------------------------- TEXTE SUR LA RELATIVITÉ ------------------------------------
texte3:
locate 1, 27: print " LA RELATIVITE "
print
locate ,2: print "C'est en 1904 que Henri Poincar‚ a trouv‚ les valeurs correctes des"
locate ,2: print "‚quations de Lorentz et qu'il a ‚nonc‚ un postulat de relativit‚. "
locate ,2: print "Cela fait de lui le d‚couvreur incontestable de la Relativit‚."
print
locate ,2: print "Pourtant la Relativit‚ n'est qu'une illusion. Les ‚quations donn‚es"
locate ,2: print "par Poincar‚ sont effectivement SYMETRIQUES et r‚versibles. Avec"
locate ,2: print "x = 0,5 et t = 1, on obtient cependant une dilatation de x' et t':"
print
t = chrono
xPrime = (x + beta * t) / facteurG                        'chrono = 1
tPrime = (t + x * beta) / facteurG
locate ,2: print "x'= ";: print using "#.###"; xPrime;
           print " = (x + beta * t ) / g    ";
           print "t'= ";: print using "#.###"; tPrime;
           print " = (t + beta * x ) / g"
locate ,2: print "x = ";: print using "#.###"; (xPrime - beta * tPrime) / facteurG;
           print " = (x'- beta * t') / g    ";
           print "t = ";: print using "#.###"; (tPrime - beta * xPrime) / facteurG;
           print " = (t'- beta * x') / g"
print
locate ,2: print "Il en est ainsi parce que ces ‚quations admettent des valeurs auxi-"
locate ,2: print "liaires d'espace et de temps. Or Lorentz a bien pr‚cis‚ que c'‚tait"
locate ,2: print "un artifice math‚matique. Il est pr‚f‚rable de permuter les varia- "
locate ,2: print "bles, car alors les ‚quations respectent mieux sa pens‚e. Mais du  "
locate ,2: print "coup ";
color rouge: print "elles perdent leur belle et trompeuse sym‚trie:": color noir
print
xPrime = facteurG * x + beta * t                          't = chrono = 1
tPrime = facteurG * t - beta * x                          'temps local à droite du mobile.
locate , 2: print "x'= +";                                'utiliser les équations véritables
if xPrime > 0 then locate , 7 else locate , 8             'pour les valider.
print using "#.### "; facteurG * x + beta * 1;: print "= g * x + beta * t";
print "     t'= +";
if tPrime > 0 then locate , 41 else locate , 42
print using "#.###"; facteurG * t - beta * x;:  print " = g * t - bˆta * x"
locate , 2: print "x = +";                                'valeur fixe en guise d'exemple.
print using "#.###"; (xPrime - beta * t) / facteurG;: print " = (x'- beta * t) / g";
print "   t = +";
print using "#.###"; (tPrime + beta * x) / facteurG ;: print " = (t'+ bˆta * x) / g"
return
'------------------------- TEXTE: LA SYNCHRONISATION DES HORLOGES ----------------------------
texte4:
locate 1, 19: print " LA SYNCHRONISATION DES HORLOGES "
print
locate ,2: print "Les physiciens devront r‚aliser t“t ou tard que les ‚quations pr‚- "
locate ,2: print "sent‚es par Lorentz prˆtent … confusion en invoquant une dilatation"
locate ,2: print "de l'espace. Ce sont plut“t les objets qui se contractent s'ils se "
locate ,2: print "d‚placent comparativement … l'‚ther. Les horloges fonctionnent plus"
locate ,2: print "lentement et elles n'indiquent donc pas toujours l'heure convenue."
print
locate ,2: print "Jusqu'… preuve du contraire, la vitesse de la lumiŠre est constante"
locate ,2: print "et cette vitesse est relative … l'‚ther. Il faut revenir au systŠme"
locate ,2: print "de Descartes, qui doit ˆtre pr‚sum‚ au repos dans l'‚ther, avec des"
locate ,2: print "grandeurs x, y, z et t absolues. Seul le temps t' est une grandeur "
locate ,2: print "auxiliaire et inexacte, qui ne servira qu'… expliquer pourquoi la  "
locate ,2: print "Relativit‚ se v‚rifie. Par ailleurs on a toujours y' = y; z' = z."
print
locate ,2: print "N'importe quel ‚tudiant peut calculer le temps que mettra un signal"
locate ,2: print "radio ‚mis par l'observateur central pour parvenir … chacun des ob-"
locate ,2: print "servateurs avant et arriŠre. Tous conviendront qu'il atteindra plus"
locate ,2: print "vite celui qui se situe … l'arriŠre, … cause de l'effet Doppler. Le"
locate ,2: print "d‚calage horaire pr‚vu par Henri Poincar‚ et confirm‚ par les ‚qua-"
locate ,2: print "tions de Lorentz n'est que le r‚sultat d'une erreur commise par les"
locate ,2: print "observateurs et n'a rien … voir avec une ® dilatation du temps ¯."
return

'------------------------------------ TEXTE: CONCLUSION --------------------------------------
texte5:
locate 1, 29: print " CONCLUSION "
print
locate ,2: print "Il est clair que Lorentz avait postul‚, comme FitzGerald, que l'in-"
locate ,2: print "terf‚romŠtre de Michelson devait se contracter puisqu'il ne pouvait"
locate ,2: print "pas r‚v‚ler le vent d'‚ther. Or ses ‚quations pr‚voient plut“t une "
locate ,2: print "dilatation de l'espace, ce qui a induit tout le monde en erreur.   "
print
locate ,2: print "On peut l'expliquer en rappelant que ses ‚quations s'inspirent de"
locate ,2: print "celles de Woldemar Voigt, qui travaillait sur les ‚quations de Max-"
locate ,2: print "well. Il obtenait de cette maniŠre une invariance parfaite advenant"
locate ,2: print "un mouvement de translation de l'‚metteur et du r‚cepteur."
print
locate ,2: print "C'est ‚vident, l'interf‚romŠtre de Michelson se contracte vraiment."
locate ,2: print "C'est la matiŠre qui se contracte, et non pas l'espace qui se dila-"
locate ,2: print "te. Les horloges qui n'affichent pas l'heure donn‚e par l'observa- "
locate ,2: print "toire de Greenwich, peu importe sa vitesse … travers l'‚ther, sont "
locate ,2: print "tout simplement inexactes. Il suffit de s'en tenir … la convention."
print
locate ,2: print "On ne voit pas bien pourquoi il faudrait aller jusqu'… triturer le "
locate ,2: print "temps et l'espace quand les transformations de Lorentz et la g‚om‚-"
locate ,2: print "trie d'Euclide expliquent facilement la Relativit‚. Pourquoi alors "
locate ,2: print "compliquer les choses? Pourquoi renoncer … l'espace euclidien?"
return

'---------------------------------------- TRANSLATION ----------------------------------------
Translation:
if beta = 0 then mode$ = "départ" : gosub MiseAJour: return
xCentre = xCentre + 1                                     'translation par pas d'un pixel.
if xCentre > 800 then xCentre = 0
screensync'                                               'uniformiser la vitesse.
gosub Diagramme
if clic = 1 then mode$ = "déplacement": gosub MiseAJour: return
if xCentre = origine + 1 then sleep 1000
'entier = origine + beta * diametre
'If xCentre = entier + 1 Then Sleep 1000
return

'------------------------------------- DESSIN DES FLÈCHES ------------------------------------
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
rouge = rgb(255,0,0)
marron = rgb(200,50,0)
grisClair = rgb(175, 175,175)
bleuClair = rgb(100,150,255)
turquoise = rgb (230, 255, 255)
chamois = rgb(245,235,220): 

pi = 4 * atn(1)
deuxPi = 2 * pi
'bitmap = 1                                               'séquence bitmap si désiré.
acceleration = 0
vitesse$ = ".866"                                         'vitesse$ normalisée selon c = 1.
beta = sin(pi / 3)
x = .5                                                    'x = 0,5 (fixe) en secondes lumière.
mode$ = "translation"
secondeLumiere = 320                                      'une seconde lumière en pixels.
diametre = secondeLumiere                                 'diamètre du système au repos
pas = beta / secondeLumiere                               'vitesse en pixels par pas.
rayon = diametre / 2                                      'rayon du système.
origine = secondeLumiere / 2 + 28                         'origine du plan cartésien.
xCentre = origine + beta * secondeLumiere
yCentre = rayon + 20
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2
color noir, fond: cls
if bitmap then return
color noir, blanc
line(577,8)-(757,53), rouge, BF
line(581,12)-(753,49), blanc, BF
locate 2,74: print " LES TRANSFORMATIONS "
locate 3,74: print "     DE LORENTZ      "
color noir, fond
line(4,378)-(487,532),grisClair, b                        'boîte de texte.
line(5,379)-(486,531),noir, b
line(5,531)-(487,531),blanc
line(4,532)-(487,532),blanc
line(486,380)-(486,532),blanc
line(487,379)-(487,532),blanc
locate 25, 3: print "Les transformations de Lorentz  rendent compte d'un  d‚ca-"
locate 26, 3: print "lage horaire, d'un ralentissement des horloges, d'une con-"
locate 27, 3: print "traction et d'une translation. Avec bˆta = 0,866, le point"
locate 28, 3: print "en x = 0,5 passe … 0,25 … cause de la contraction et alors"
locate 29, 3: print "la translation selon : x' = g * x + bˆta * t  devient plus"
locate 30, 3: print "‚vidente. Les ‚quations de Lorentz produisent l'effet con-"
locate 31, 3: print "traire car il a eu recours de son propre aveu … un artifi-"
locate 32, 3: print "ce math‚matique qui avait pour effet de dilater plut“t le "
locate 33, 3: print "temps et l'espace. L'heure locale t' retarde … l'avant."
color noir, fond
locate 1, 35: print "Une seconde lumiŠre"

line(origine, 8)-(origine + 10, 4), noir                  'flèches, une seconde lumière.
line(origine, 8)-(origine + 10, 12), noir
line(origine + 10, 4)-(origine + 10, 12),noir
paint(origine + 2, 8),noir,noir
line(origine + diametre, 8)-(origine + diametre - 10, 4), noir
line(origine + diametre, 8)-(origine + diametre - 10, 12), noir
line(origine + diametre - 10, 4)-(origine + diametre - 10, 12),noir
paint(origine + diametre - 2, 8),noir,noir
line(origine+2,7)-(origine + 70, 7), noir
line(origine, 8)-(origine + 70, 8), noir
line(origine+2, 9)-(origine + 70, 9), noir
line(origine + diametre - 69, 7)-(origine + diametre-2, 7), noir
line(origine + diametre - 69, 8)-(origine + diametre, 8), noir
line(origine + diametre - 69, 9)-(origine + diametre-2, 9), noir

line(origine - rayon, 20)-(origine + diametre, 20), grisClair       'repères.
line(origine - rayon, 20 + diametre)-(origine + diametre, 20 + diametre), grisClair

for j = origine - rayon to origine + .9 * secondeLumiere step secondeLumiere / 10
  line(j, 20)-(j, 30 + secondeLumiere), grisClair
next
line(origine, 12)-(origine, 30 + diametre), noir
line(origine + diametre, 12)-(origine + diametre, 30 + diametre), noir

locate 23,3:  print "-.5"
locate 23,7:  print "-.4"
locate 23,11: print "-.3"
locate 23,15: print "-.2"
locate 23,19: print "-.1"
locate 23,24: print "0"
locate 23,28: print ".1"
locate 23,32: print ".2"
locate 23,36: print ".3"
locate 23,40: print ".4"
locate 23,44: print ".5"
locate 23,48: print ".6"
locate 23,52: print ".7"
locate 23,56: print ".8"
locate 23,60: print ".9"
locate 23,64: print "1"

locate 5, 74: print "bˆta    thˆta  contraction"
locate 6, 91: print "selon g:"
ligne07$ = " 0 - 0,0     0"      + chr(248) + "        0,0     ": locate 7, 70:  print ligne07$
ligne08$ = " 1 - 0,1     5,739"  + chr(248) + "    0,995   ":     locate 8, 70:  print ligne08$
ligne09$ = " 2 - 0,2     11,537" + chr(248) + "   0,98    ":      locate 9, 70:  print ligne09$
ligne10$ = " 3 - 0,3     17,458" + chr(248) + "   0,954   ":      locate 10, 70: print ligne10$
ligne11$ = " 4 - 0,4     23,578" + chr(248) + "   0,917   ":      locate 11, 70: print ligne11$
ligne12$ = " 5 - 0,5     30"     + chr(248) + "       0,866   ":  locate 12, 70: print ligne12$
ligne13$ = " 6 - 0,6     36,87"  + chr(248) + "    0,8     ":     locate 13, 70: print ligne13$
ligne14$ = " 7 - 0,707   45"     + chr(248) + "       0,7071  ":  locate 14, 70: print ligne14$
ligne15$ = " 8 - 0,8     53,13"  + chr(248) + "    0,6     ":     locate 15, 70: print ligne15$
ligne16$ = " 9 - 0,866   60"     + chr(248) + "       0,5     ":  locate 16, 70: print ligne16$
ligne18$ =  " D - D‚part aprŠs contraction. ": locate 18, 70: print ligne18$
ligne18a$ = " D - D‚part impossible.        "
ligne19$ =  " P - Pause aprŠs une seconde.  ": locate 19, 70: print ligne19$
ligne20$ =  " T - Translation continue.     ": locate 20, 70: print ligne20$
ligne21$ =  " A - Acc‚l‚ration t'= x; x'= t.": locate 21, 70: print ligne21$
ligne22$ =  " R - Renseignements.           ": locate 22, 70: print ligne22$
ligne35$ =  "    I - Initialiser.    ": locate 35, 39: print ligne35$
ligne36$ =  "    Quitter (Echap).    ": locate 36, 39: print ligne36$
ligne37$ =  "                        "
gosub flecheGauche: gosub flecheDroite

color vert, fond
locate 35
locate , 2:    print "Merci aux cr‚ateurs de FreeBASIC."
locate , 2:    print "A Serge Cabala, qui a cru … l'‚ther."
locate , 2:    print "Gabriel LaFreniŠre.  glafreniere.com";
locate 36, 63: print "29 nov. 2006. Ce programme peut ˆtre"
locate 37, 63: print "distribu‚, copi‚ ou modifi‚ librement.";
color noir
pcopy 2, 0
pcopy 2, 1
gosub MiseAJour
return
