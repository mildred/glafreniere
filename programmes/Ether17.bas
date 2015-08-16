dim as single pi = 4 * atn(1), differenceDeMarche, periode, phase, temps
dim as single theta, xCoord, yCoord, xCarre, xPrime, tPrime, diagonale, beta
screen 19,24,3: page1 = 1: gosub Initialisation
do'            L'EFFET DOPPLER CALCULÉ À L'AIDE DES TRANSFORMATIONS DE LORENTZ.
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  image = image + 1                                       'espace: mesuré en longueurs d'onde.
  temps = image / images                                  'temps:  mesuré en période d'onde.
  for x = 0 to largeur                                    'coordonnée x du pixel.
    xCoord = (x - xCentre) / lambda                       'coordonnée x en longueurs d'onde.
    xCarre = xCoord * xCoord                              'distance non transformée au carré.
    xPrecedent = xPixel

'------------------------ APPLIQUER LES TRANSFORMATIONS DE LORENTZ ---------------------------
'   Grâce à l'angle thêta, qui vaut: arc sin (v / c),
'   les transformations de Lorentz peuvent être exprimées ainsi:
'   x' = x * Cos(theta) + t * Sin(theta)
'   t' = t * Cos(theta) - x * Sin(theta)
'   Pour retrouver les variables d'origine, il faut faire bien évidemment:
'   x = (x' - t * Sin(theta)) / Cos(theta)
'   t = (t' + x * Sin(theta)) / Cos(theta)
'   Toutefois, il faut permuter les variables pour retrouver les équations de Lorentz:
'   x' = (x - t * Sin(theta)) / Cos(theta)
'   t' = (t + x * Sin(theta)) / Cos(theta)
'   Cela s'explique parce que Woldemar Voigt avait tenté d'élaborer en 1887 des équations
'   dans le but de corriger l'effet Doppler en les appliquant aux équations de Maxwell.
'   Sachant qu'elles indiquaient une contraction, Lorentz les a récupérées et améliorées en
'   1895 dans le but d'expliquer pourquoi l'interféromètre de Michelson ne fonctionnait pas.
'   Mais c'est Henri Poincaré qui a finalement donné les valeurs définitives en 1904.
'   Dans leur forme d'origine, les transformations de Lorentz ont donc pour effet de corriger
'   l'effet Doppler. C'est pourquoi le fait de permuter les variables a plutôt pour 
'   conséquence de provoquer  l'effet Doppler. En plus de la contraction, il se produit un
'   ralentissement de la fréquence. Mais l'effet le plus spectaculaire, c'est l'apparition
'   d'heures locales, qui font en sorte en pratique que la période des ondes évolue le long
'   de l'axe du déplacement. Cela se traduit par une « onde de phase » qui est très visible
'   dans le cas des ondes stationnaires à des vitesses faibles, par exemple: v / c = 0,1.

    xPrime = xCoord * cos(theta) + temps * sin(theta)     'distance x' en longueurs d'onde.
    xPixel = xPrime * lambda                              'nombre entier pour le pixel.
    xCoord = (xPixel / lambda - temps * sin(theta)) / cos(theta)'retrouver le chiffre exact.
    tPrime = temps * cos(theta) - xCoord * sin(theta)     'temps t' en période d'onde.

    if xPixel + largeur / 2 > 800 then image = 0          'image hors-écran: recycler.
    for y = 0 to hauteur / 2                              'coordonnee y du pixel, symétrie axiale.
      yCoord = (y - yCentre) / lambda                     'coordonnée y en longueurs d'onde.
      diagonale = sqr(xCarre + yCoord * yCoord)           'selon Pythagore, en longueurs d'onde.

'------------------------------ AFFICHER LES ONDES NORMALES ----------------------------------
      select case mode$
        case "convergentes" : phase = 2 * pi * (-diagonale - temps)'en radians, temps absolu.
                              ton = 128 * (sin(phase) + 1)
        case "divergentes"  : phase = 2 * pi * (diagonale - temps)
                              ton = 128 * (sin(phase) + 1)
        case "stationnaires"
          if diagonale < .25 then diagonale = .25         'noyau onde entière de l'électron.
          phase = 2 * pi * (diagonale + temps)            'onde convergente.
          ton = 64 * (sin(phase) + 1)
          phase = 2 * pi * (diagonale - temps)            'onde divergente.
          ton = ton + 64 * (sin(phase) + 1)
      end select
      if ton < 0 then ton = 0 else if ton > 255 then ton = 255
      pset(x, hauteur + y + 6), rgb(ton,ton,ton)          'afficher l'onde normale.
      pset(x, 2 * hauteur - y + 6), rgb(ton,ton,ton)      'symétrie axiale.

'---------------------- AFFICHER LES ONDES TRANSFORMÉES SELON LORENTZ ------------------------
      if xPixel > xPrecedent then                         'passer si le pixel est déjà traité.
        diagonale = sqr(xCoord * xCoord + yCoord * yCoord)'
        select case mode$
          case "convergentes" : phase = 2 * pi * (-diagonale - tPrime)
                                ton = 128 * (sin(phase) + 1)
          case "divergentes"  : phase = 2 * pi * (diagonale - tPrime)
                                ton = 128 * (sin(phase) + 1)
          case "stationnaires"
            if diagonale < .25 then diagonale = .25       'noyau onde entière de l'électron.
            phase = 2 * pi * (diagonale + tPrime)         'onde convergente.
            ton = 64 * (sin(phase) + 1)
            phase = 2 * pi * (diagonale - tPrime)         'onde divergente.
            ton = ton + 64 * (sin(phase) + 1)
        end select
        if ton < 0 then ton = 0 else if ton > 255 then ton = 255
        pset(xPixel + largeur / 2, y), rgb(ton,ton,ton)   'afficher l'onde avec effet Doppler.
        pset(xPixel + largeur / 2, hauteur - y), rgb(ton,ton,ton)
      end if
    next y
  next x

  Saisie$ = inkey
  if len(Saisie$) then
    capture = 0
    if len(Saisie$) = 2 then Saisie$ = right(Saisie$, 1) + "+" else Saisie$ = ucase(Saisie$)
    select case Saisie$
      case "I": gosub Initialisation
      case "X+", "k+", chr$(27): end
      case "C": mode$ = "convergentes"
      case "D": mode$ = "divergentes"
      case "S": mode$ = "stationnaires"
      case "M": run "Ether00.exe"
      case "K+":run "Ether16.exe"                         'flèche gauche.
      case "M+":run "Ether18.exe"                         'flèche droite.  
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
      case else: Saisie$ = "nul"
    end select
    do: loop while len(inkey)                             'vider le tampon.
    if Saisie$ = "nul" then else gosub MiseAJour
  end if

  getmouse xSouris, ySouris, , clic                       'saisie Souris.
  ligne = .5 + ySouris / 16
  select case ligne
    case is > 34: if ligne > 37 or xSouris < 304 or xSouris > 496 then ligne = 0
    case is > 29: ligne = 0
    case is > 14: if xSouris < 552 then ligne = 0
    case else: ligne = 0
  end select
  if ligne = 0 then clic = 0
  if clic = 1 then
    select case ligne
      case 15: vitesse$ =  "0"
      case 16: vitesse$ = ".1"
      case 17: vitesse$ = ".2"
      case 18: vitesse$ = ".3"
      case 19: vitesse$ = ".4"
      case 20: vitesse$ = ".5"
      case 21: vitesse$ = ".6"
      case 22: vitesse$ = ".707"
      case 23: vitesse$ = ".8"
      case 24: vitesse$ = ".866"
      case 26: if mode$ <> "convergentes" then mode$ = "convergentes" else ligne = 0
      case 27: if mode$ <> "divergentes" then mode$ = "divergentes" else ligne = 0
      case 28: if mode$ <> "stationnaires" then mode$ = "stationnaires" else ligne = 0
      case 29: gosub CurseurLambda
      case 35: gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether16.exe" else run "Ether18.exe"
      case else: ligne = 0
    end select
    if ligne then gosub MiseAJour
  end if

  color , turquoise: locate ligne, 70
  select case ligne                                       'rehausser l'affichage.
    case 15: if vitesse$ <> "0"    then print ligne15$
    case 16: if vitesse$ <> ".1"   then print ligne16$
    case 17: if vitesse$ <> ".2"   then print ligne17$
    case 18: if vitesse$ <> ".3"   then print ligne18$
    case 19: if vitesse$ <> ".4"   then print ligne19$
    case 20: if vitesse$ <> ".5"   then print ligne20$
    case 21: if vitesse$ <> ".6"   then print ligne21$
    case 22: if vitesse$ <> ".707" then print ligne22$
    case 23: if vitesse$ <> ".8"   then print ligne23$
    case 24: if vitesse$ <> ".866" then print ligne24$
    case 26: if mode$ <> "convergentes"  then print ligne26$
    case 27: if mode$ <> "divergentes"   then print ligne27$
    case 28: if mode$ <> "stationnaires" then print ligne28$
    case 29: print ligne29$
    case 35: locate, 39: print ligne35$
    case 36: locate, 39: print ligne36$
    case 37: locate, 39: print ligne37$;
             if xSouris < 400 then gosub flecheGauche else gosub flecheDroite
  end select
  color noir, fond
loop

CurseurLambda:'-------------- MODIFICATION DE LA LONGUEUR D'ONDE -----------------------------
lambdaPrec = lambda                                       'lambda: minimum 10, maximum 100.
do
  swap page1, page2
  screenset page1, page2
  cls
  curseur = 551 + 2 * lambda
  line(570,447)-(752, 464), noir, b
  line(curseur - 1,448)-(curseur + 1, 464), noir, bf
  line(658 - lambda / 2,431)-(658 + lambda / 2, 447), noir, bf
  rayon = int(longueurSource / 2)
  locate 23, 45: print "Agrandir la longueur d'onde ‚quivaut … faire un zoom."
  locate 25, 45: print "Longueur d'onde (lambda):"; lambda; " pixels."
  if ySouris > 447 and ySouris < 465 then 
    lambda = int((xSouris - 550) / 2)
  else lambda = lambdaPrec
  end if
  if lambda > 100 then lambda = 100 else if lambda < 10 then lambda = 10
  getmouse xSouris, ySouris, , clic
loop while clic = 1
if lambdaPrec = lambda then else gosub MiseAJour: calculFait = 0
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
fond = rgb(225,225,225)
vert = rgb(0,150,0)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
blanc = rgb(255,255,255)
turquoise = rgb (230, 255, 255)
vitesse$ = ".866"                                         'vitesse$ normalisée selon c = 1.
images = 24: lambda = 20
largeur = 540: hauteur = 200
xCentre = largeur / 2: yCentre = hauteur / 2
mode$ = "divergentes"
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2
color noir, fond: cls
locate 14, 74: print "Bˆta    Thˆta  Contraction"
ligne15$ = " 0 - 0,0     0"      + chr(248) + "        0,0     ": locate 15, 70: print ligne15$
ligne16$ = " 1 - 0,1     5,739"  + chr(248) + "    0,995   ":     locate 16, 70: print ligne16$
ligne17$ = " 2 - 0,2     11,537" + chr(248) + "   0,98    ":      locate 17, 70: print ligne17$
ligne18$ = " 3 - 0,3     17,458" + chr(248) + "   0,954   ":      locate 18, 70: print ligne18$
ligne19$ = " 4 - 0,4     23,578" + chr(248) + "   0,917   ":      locate 19, 70: print ligne19$
ligne20$ = " 5 - 0,5     30"     + chr(248) + "       0,866   ":  locate 20, 70: print ligne20$
ligne21$ = " 6 - 0,6     36,87"  + chr(248) + "    0,8     ":     locate 21, 70: print ligne21$
ligne22$ = " 7 - 0,707   45"     + chr(248) + "       0,7071  ":  locate 22, 70: print ligne22$
ligne23$ = " 8 - 0,8     53,13"  + chr(248) + "    0,6     ":     locate 23, 70: print ligne23$
ligne24$ = " 9 - 0,866   60"     + chr(248) + "       0,5     ":  locate 24, 70: print ligne24$
ligne26$ =  " C - Ondes convergentes.       ": locate 26, 70: print ligne26$
ligne27$ =  " D - Ondes divergentes.        ": locate 27, 70: print ligne27$
ligne28$ =  " S - Ondes stationnaires.      ": locate 28, 70: print ligne28$
ligne29$ =  " Longueur d'onde: cliquer.     "
ligne35$ =  "    I - Initialiser.    ": locate 35, 39: print ligne35$
ligne36$ =  "    Quitter (Echap).    ": locate 36, 39: print ligne36$
ligne37$ =  "                        "
gosub flecheGauche: gosub flecheDroite
locate 31, 71: print "Bˆta....... "; chr(225); " = sin "; chr(233); " = v / c"
locate 32, 71: print "Thˆta...... "; chr(233); " = arc sin "; chr(225)
locate 33, 71: print "Contraction g = cos "; chr(233)
color blanc, rouge
locate 27, 9: print " LES TRANSFORMATIONS DE LORENTZ ET L'EFFET DOPPLER."
line(63, 415)-(471, 415), rouge, b
line(63, 413)-(473, 432), noir, b
line(64, 414)-(472, 431), blanc, b
color vert, fond
locate 35, 63: print "29 nov. 2006. Ce programme peut ˆtre"
locate 36, 63: print "distribu‚, copi‚ ou modifi‚ librement.";
locate 37, 63: print "Gabriel LaFreniŠre.  glafreniere.com";
color noir, fond
locate 29, 2: print "Ce programme d‚montre que les transformations de Lorentz ne sont"
locate 30, 2: print "rien d'autre et rien de plus que le reflet de l'effet Doppler que"
locate 31, 2: print "subit la matiŠre. S'il est vrai que les ‚lectrons assemblent les"
locate 32, 2: print "mol‚cules en fonction de leur longueur d'onde, la matiŠre en mou-"
locate 33, 2: print "vement doit ‚voluer … une cadence plus lente et se contracter"
locate 34, 2: print "comme le pr‚voyait Lorentz, soit selon:"
color noir, blanc
line(50,555)-(300,595), blanc, bf
line(50,555)-(300,595), noir, b
locate 36, 10: print "x' = x * Cos "; chr(233); " + t * Sin "; chr(233)
locate 37, 10: print "t' = t * Cos "; chr(233); " - x * Sin "; chr(233);
color noir, fond
gosub MiseAJour
return

'-------------------------------- MISE À JOUR DES DONNÉES ------------------------------------
MiseAJour:
screenset 2
color noir, fond
locate 15, 70: print ligne15$
locate 16, 70: print ligne16$
locate 17, 70: print ligne17$
locate 18, 70: print ligne18$
locate 19, 70: print ligne19$
locate 20, 70: print ligne20$
locate 21, 70: print ligne21$
locate 22, 70: print ligne22$
locate 23, 70: print ligne23$
locate 24, 70: print ligne24$
locate 26, 70: print ligne26$
locate 27, 70: print ligne27$
locate 28, 70: print ligne28$
locate 29, 70: print " Longueur d'onde:"; lambda; " pixels. "
color bleu, fond
beta = val(vitesse$)                                      'exceptions pour 0,707 et 0,866.
select case vitesse$                                      'indiquer la vitesse en cours.
  case "0":    locate 15, 70: print ligne15$
  case ".1":   locate 16, 70: print ligne16$
  case ".2":   locate 17, 70: print ligne17$
  case ".3":   locate 18, 70: print ligne18$
  case ".4":   locate 19, 70: print ligne19$
  case ".5":   locate 20, 70: print ligne20$
  case ".6":   locate 21, 70: print ligne21$
  case ".707": locate 22, 70: print ligne22$
               beta = sin(pi / 4)
  case ".8":   locate 23, 70: print ligne23$
  case ".866": locate 24, 70: print ligne24$
               beta = sin(pi / 3)
end select
select case mode$
  case "convergentes":  locate 26, 70: print ligne26$
  case "divergentes":   locate 27, 70: print ligne27$
  case "stationnaires": locate 28, 70: print ligne28$
end select
color noir, fond
theta = asin(beta)                                        'inclinaison des ondes transversales.
image = 0
screenset page1, page2
return
