largeur = 900: hauteur = 560                              'nombre de particules par c�t�.
dim as single influence(-1 to largeur+1, -1 to hauteur+1) 'cr�� le 20 nov. 2006.
dim as single sinus(-1 to largeur+1, -1 to hauteur+1)
dim as single cosinus(-1 to largeur+1, -1 to hauteur+1)
dim as single amortissement(-1 to largeur+1, -1 to hauteur+1)
dim as single facteur, rapport, luminosite, contraste, pas, petitPas, gLorentz, xScan
dim as single amplitudeSinus, amplitudeCosinus, c, xOrig, yOrig, xDepart, yDepart
dim as single xPrime, yPrime, tPrime, temps, beta, theta, ixe, igrec, longueur
dim as single xCoord, yCoord, periode, rotation, amplitude, rayon, petitRayon, radian
dim as single pi, angle, lambda, xCarre, yCarre, distance, xDistance, yDistance, phi
beta = -.5: choix$ = "B"
screen 19,24,3: page2 = 1: : gosub Initialisation

do'                       MOD�LISATION DE L'�THER EN DEUX DIMENSIONS.

  for y = 0 to hauteur - 1' un cran � l'avance pour avoir acc�s aux valeurs pr�c�dentes.
    influence(0,y) = sinus(-1,y) + sinus(1,y) + sinus(0,y-1) + sinus(0,y+1) - 4 * sinus(0,y)
  next

' IMPORTANT: l'algorithme qui suit demeure fondamentalement celui qui a �t� invent� par M.
' Philippe Delmotte en juin 2005 en faisant intervenir les lois de Newton sur l'inertie. Mais
' M. Dewavrin a remarqu� (voir Ether04) que le principe de base �tait identique � la m�thode
' d'Euler, qui est fond�e sur la trigonom�trie, et qui permet de construire une sinuso�de.
' J'ai transpos� moi-m�me cette m�thode dans l'algorithme de M Delmotte. Si on fait la moyenne
' des 4 pixels voisins, l'influence n'en vaut que le quart. Mais on peut doubler sa valeur �
' 0,5, ce qui permet de faire passer la vitesse des ondes de 0,5 � 0.707 (sin 45�) pixels par
' passage. C'est la limite extr�me: toute valeur sup�rieure � 0,5 provoque un d�s�quilibre. On
' peut n�anmoins obtenir un effet de lentille en introduisant un indice de r�fraction toujours
' sup�rieur � 1, comme ceci: cosinus(x,y) = cosinus(x,y) + .5 *influence(x,y) / indice

  for x = 0 to largeur - 1: for y = 0 to hauteur - 1

'------------- CALCUL SELON LA M�THODE D'EULER SIMPLIFI�E PAR M. ANSELME DEWAVRIN -------------
'l'amortissement est facultatif et permet d'�liminer les r�flexions sur les bords.

    influence(x+1,y) = sinus(x,y) + sinus(x+2,y) + sinus(x+1,y-1) + sinus(x+1,y+1) - 4 * sinus(x+1,y)'un cran � l'avance.
    cosinus(x,y) = cosinus(x,y) + .5 * influence(x,y)
    sinus(x,y) = (sinus(x,y) + cosinus(x,y)) * amortissement(x,y)
  next: next

  select case choix$'                                     'r�partition des t�ches.
    case "A": gosub ChoixA
    case "B": gosub ChoixB
    case "C": gosub ChoixC
    case "D": gosub ChoixD
    case "E": gosub ChoixE
  end select

  getmouse xSouris, ySouris, , clic                       'v�rifier entre les affichages.
  if clic > 0 then afficher = 0: espacer = 0: scanner = 0 'agir au plus t�t.
  if scanner then afficher = espacer + 1                  'toujours afficher avec le scanner.
  if afficher > espacer then                              'afficher une image sur deux.
    afficher = 0
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    gosub AfficherCouleurs'---------- DIAGRAMME PRINCIPAL -------------------------------------
  else afficher +=1
  end if

'------------------------------------ �COULEMENT DU TEMPS -------------------------------------
  temps = temps + c / lambda          'le temps t de Lorentz exprim� en p�riode d'onde.
                                      'c = .707 est la vitesse des ondes en pixels par passage.

'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = inkey
  if len(saisie$) then
    bitmap = 0
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    if beta <> 0 and saisie$ = "S" then saisie$ = "T"     'texte diff�rent si beta = 0.
    if scanner then
      select case saisie$
      case "P"                                            'pause accept�e.
      case "+","-","H+","P+": saisie$ = ""                '�viter si le scanner est actif.
      case else                                 'toute autre intervention d�sactive le scanner.
        scanner = 0: screenset 2, page2: locate 37, 2 
        print "S - Activer le Scanner du Temps.      ";
        if saisie$ = "S" or saisie$ = "T" then saisie$ = ""  
      end select
    end if
    select case saisie$
      case "A","B","C","D","E": choix$ = saisie$
      case "X+", "k+",chr$(27): end                       'le � X � varie selon le FBIde(!?)
      case chr(13): saisie$ = ""
                if beta > 0 then gosub ReculerGauche
                if beta < 0 then gosub ReculerDroite
      case "I": beta = -.5: choix$ = "B"                   'initialiser.
      case "V": beta = -beta                              'vitesse n�gative ou positive.
      case "P": sleep: saisie$ = ""                       'pause.
      case "F": saisie$ = "":                             'afficher peu d'images.
                if espacer then
                  espacer = 0
                  screenset 2, page2: locate 36, 14: print "F - Acc�l�rer le calcul. "
                else
                  espacer = 20
                  screenset 2, page2: locate 36, 14: print "F - Afficher normalement."
                end if
      case "S": saisie$ = ""                              'Scanner du Temps; beta = 0.
                scanner = 1: espacer = 0: xScan = gauche + marge / 2
                screenset 2, page2: locate 37, 2: print "S - D�sactiver le Scanner du Temps.   ";
                line(0,0)-(largeur-100, hauteur-100),noir, bf
                color fond, noir: locate 3
                locate,57:?"Appuyez sur le P (pause)"
                locate,57:?"pour lire ce texte.":?
                locate,57:?"Ici, la vitesse du syst�me ondulatoire est"
                locate,57:?"nulle. Dans ce cas, le Scanner du Temps"
                locate,57:?"le reproduit tel qu'il apparara�trait"
                locate,57:?"s'il se d�pla�ait � la moiti� de la"
                locate,57:?"vitesse de la lumi�re, soit "; chr(225); " = 0,5.":?
                locate,57:?"Lorsque ce texte aura disparu, vous"
                locate,57:?"pourrez comparer en appuyant sur le 5.":?
                locate,57:?"Si la vitesse n'est pas nulle, vous pouvez"
                locate,57:?"�galement appuyer sur le S pour v�rifier"
                locate,57:?"la loi de l'addition des vitesse selon"
                locate,57:?"Henri Poincar�. Le Scanner a alors pour"
                locate,57:?"effet d'acc�l�rer le syst�me d'une valeur"
                locate,57:?"RELATIVE de 0,5 c, soit selon cette loi.":?
                locate,57:?"Si la vitesse est n�gative, le Scanner a"
                locate,57:?"pour effet de ralentir le syst�me: "; chr(225); " < -5;"
                locate,57:?"de l'IMMOBILISER: "; chr(225); " = -5; ou m�me de"
                locate,57:?"l'acc�l�rer dans l'autre sens: "; chr(225); " > -5."
                color noir, fond
      case "T": saisie$ = ""                              'Scanner du Temps, vitesse non nulle.
                scanner = 1: espacer = 0: xScan = gauche + marge / 2
                screenset 2, page2: locate 37, 2: print "S - D�sactiver le Scanner du Temps.   ";
                line(0,0)-(largeur-100, hauteur-100),noir, bf
                color fond, noir: locate 1
                locate,57:?"Appuyez sur le P (pause)"
                locate,57:?"pour lire ce texte.":?
                locate,57:?"Le Scanner ne reproduit que la partie"
                locate,57:?"gauche de cette fen�tre. Le syst�me doit"
                locate,57:?"donc s'y trouver (appuyez sur Entr�e au"
                locate,57:?"besoin, ou attendez qu'il y soit parvenu).":?
                locate,57:?"Ici, la vitesse du syst�me n'est pas nulle."
                locate,57:?"Le Scanner du Temps peut montrer l'aspect"
                locate,57:?"qu'il aurait s'il �tait acc�l�r� d'une"
                locate,57:?"valeur de 0,5 c selon la loi de l'addition"
                locate,57:?"des vitesses de Henri Poincar�. La vitesse"
                locate,57:?"RELATIVE finale n'atteint jamais celle de"
                locate,57:?"la lumi�re quelle que soit la situation.":?
                locate,57:?"Vitesse actuelle: beta_1 = ";: print using "#.# c"; beta
                locate,57:?"    Acc�l�ration: beta_2 = ";: print "0.5 c"
                locate,57:?"        Vitesse relative = ";: print using "#.### c"; (beta + .5) / (1 + beta * .5):? 
                locate,57:?"L'�quation de Poincar� se lit comme suit:"
                locate,57:?"Vitesse relative ="
                locate,57:?"(beta_1 + beta_2) / (1 + beta_1 * beta_2)":?
                locate,57:?"Si la vitesse est n�gative, le Scanner a"
                locate,57:?"pour effet de ralentir le syst�me: "; chr(225); " < -0,5"
                locate,57:?"ou de l'IMMOBILISER: "; chr(225); " = -0,5. Il peut m�me"
                locate,57:?"l'acc�l�rer dans l'autre sens: "; chr(225); " > -0,5."
                color noir, fond
      case "M": run "Ether00.exe"
      case "K+":run "Ether17.exe"                         'fl�che gauche.
      case "M+":run "Ether19.exe"                         'fl�che droite.
      case "0": beta = 0
      case "1": beta = .1
      case "2": beta = .2
      case "3": beta = .3
      case "4": beta = .4
      case "5": beta = .5
      case "6": beta = .6
      case "7": beta = .7
      case "8": beta = .8
'     case "9": beta = .9'                                'conditions difficiles, � �viter.
      case "+": luminosite = luminosite * 1.1: saisie$ = ""'non modifiable si le scanner est actif.
      case "-": luminosite = luminosite / 1.1: saisie$ = ""
      case "H+":contraste = contraste * 1.02
                luminosite = luminosite / 1.15
                saisie$ = ""
      case "P+":contraste = contraste / 1.02
                luminosite = luminosite * 1.15
                saisie$ = ""
      case else:saisie$ = ""
    end select
    do: loop while len(inkey)                             'vider le tampon.
    if len(saisie$) then gosub Initialisation
  end if
'----------------------------------------- SAISIE SOURIS --------------------------------------

  if clic = 0 then getmouse xSouris, ySouris, , clic      'v�rifier une 2e fois au besoin.
  ligne = .5 + ySouris / 16
  if ligne > 26 and ligne < 38 then
    if xSouris < 320 then ligne = 0
    if ligne > 34 and xSouris > 480 then ligne = 0
  else ligne = 0
  end if

'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 41
  select case ligne
    case 30: if not choix$ = "A" then print ligne30$      'choix en cours d�j� affich� en bleu.
    case 31: if not choix$ = "B" then print ligne31$
    case 32: if not choix$ = "C" then print ligne32$
    case 33: if not choix$ = "D" then print ligne33$
    case 34: if not choix$ = "E" then print ligne34$      
    case 35: print ligne35$      
    case 36: print ligne36$
    case 37: print ligne37$;: if xSouris < 400 then gosub FlecheGauche else gosub FlecheDroite
  end select
  color noir, fond
'-------------------------------------- ACTIONS SUITE � UN CLIC -------------------------------
  if clic = 1 then
    clic = 0: bitmap = 0: afficher = 0: espacer = 0
    scanner = 0: screenset 2, page2: locate 37, 2: print "S - Activer le Scanner du Temps.      ";
    select case ligne
      case 30: choix$ = "A": gosub Initialisation
      case 31: choix$ = "B": gosub Initialisation
      case 32: choix$ = "C": gosub Initialisation
      case 33: choix$ = "D": gosub Initialisation
      case 34: choix$ = "E": gosub Initialisation
      case 35: beta = -.5: choix$ = "B": gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether17.exe" else run "Ether19.exe"
    end select
  end if
  if bitmap then gosub Bitmaps                            'capture d'images si d�sir�.
loop

AfficherCouleurs:'****************** AFFICHER EN COULEURS *************************************

for x = marge/2 to largeur - marge/2                      'masquer 50 pixels (100 amortis).
  if abs(x - xScan) < 1 then scan = 1 else scan = 0       'activer le scanner si x = xScan
  for y = marge/2 to hauteur - marge/2
    luminance = luminosite * abs(sinus(x, y)) ^ contraste 'afficher en rouge et vert. Ajouter
    if sinus(x, y) < 0 then                               'du bleu car les couleurs doivent
      vert = .67 * luminance ' 0,67 + 0,33 = 1            '�tre compl�mentaires le plus possi-
      bleu = .33 * luminance ' 50% du vert.               'ble pour �viter une dominante jaune.
      if vert > 255 then rouge = .67 * luminance - 255 else rouge = 0
    else
      rouge = .75 * luminance ' 0,75 + 0,25 = 1           'proportion de bleu moindre
      bleu  = .25 * luminance ' 33% du rouge.             'pour �viter le fushia.
      if rouge > 255 then vert = .75 * luminance - 255 else vert = 0
      if rouge > 255 then bleu = bleu + .33 * (.75 * luminance - 255)'ajouter 33% du vert.
    end if
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    if bleu > 255 then bleu = 255 else if bleu < 0 then bleu = 0
    
'*********************************** LE SCANNER DU TEMPS **************************************

' 1- Ce Scanner peut montrer comment un syst�me au repos devrait appara�tre
'    s'il se d�pla�ait vers la droite � la moiti� de la vitesse de la lumi�re.
' 2- Il peut convertir un syst�me mobile en syst�me au repos. Il faut alors r�gler la
'    vitesse � -0,5 pour que le balayage s'effectue dans le sens contraire du d�placement.
' 3- Il peut acc�l�rer ou ralentir un syst�me mobile selon la loi de l'addition
'    des vitesses de Poincar�, peu importe sa direction initiale.
' 4- On peut modifier le programme pour qu'il traite une autre vitesse que celle qui
'    a �t� choisie ici, soit la moiti� de celle de la lumi�re. Il suffit en pratique
'    de remplacer les valeurs .866 et .5 ci-dessous par le sinus et le cosinus de
'    l'angle th�ta correspondant � la vitesse d�sir�e, par exemple .8 et .6 si beta = 0,6.

    if scanner then                                       '428 pixels � gauche, 370 � droite.
      if x-marge/2 < gauche then pset (x-marge/2, y-marge/2), rgb(rouge,vert,bleu)'moiti� non transform�e � gauche.
      if scan then                                        'affiicher la moiti� transform�e � droite.
        screenset 2,page2                                 'afficher sur la page matrice.
        pset ((x-marge/2)*.866+gauche, y-marge/2), rgb(rouge,vert,bleu)'scan avec contraction 0,866.
        screenset page1, page2
      end if
    else pset (x-marge/2, y-marge/2), rgb(rouge,vert,bleu)'image compl�te normale sans scanner.
    end if
  next
next
if scanner then                                 'd�placer le scanner s'il est actif.
  xScan = xScan - c /.5                         'vitesse de balayage = c / beta avec beta = .5
  if xScan < marge / 2 then xScan = marge / 2   'stopper � gauche de l'�cran. 
  line(xScan-marge/2, 0)-(xScan-marge/2, hauteur - marge), fond'marquer l'emplacement du scanner.
  line(gauche, 0)-(gauche, hauteur - marge), fond        's�parer la fen�tre en deux parties.
  line((xScan-marge/2)*.866+gauche+1, 0)-((xScan-marge/2)*.866+gauche+1, hauteur - marge), fond'� imprimante �.
end if
return

Bitmaps:'-------------------------- Cr�er une s�quence bitmap ---------------------------------
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

ChoixA:'*********************** DEUX SOURCES SUR DES PARALL�LES *******************************

'  Dans le cas d'une source unique, il est avantageux de g�n�rer
'  les ondelettes sur un petit cercle pour �viter les art�facts.
'  Le point de d�part (x, y) est fixe. Les transformations de Lorentz ne sont
'  utilis�es que lors des impulsions dans la proc�dure � Impulsion � plus bas.

for radian = pas / 2 to 2 * pi step pas
  xCoord = xDepart + .1875 * lambda * sin(radian)                  'source 1, en pixels.
  yCoord = yDepart - 3 * lambda + .1875 * lambda * cos(radian) 
  gosub Impulsion 
next
for radian = pas / 2 to 2 * pi step pas
  xCoord = xDepart + .1875 * lambda * sin(radian)                  'source 2, en pixels.
  yCoord = yDepart + 3 * lambda + .1875 * lambda * cos(radian) 
  gosub Impulsion 
next
return

ChoixB:'************* NOMBREUSES SOURCES R�PARTIES SUR UNE CIRCONF�RENCE **********************

for radian = pas / 2 to 2 * pi step pas                   'cercle parfait.
  xCoord = xDepart + rayon * lambda * cos(radian)         'en pixels.
  yCoord = yDepart + rayon * lambda * sin(radian)         'en pixels.
  gosub Impulsion
next
return

ChoixC:'******* PULSATION D'UN ARC DE CERCLE SELON LES TRANSFORMATIONS DE LORENTZ *************

for radian = pi / 2.7 to pi - pi / 2.7 step pas           'arc de cercle.
  xCoord = xDepart + rayon * lambda * cos(radian)         'x en pixels.
  yCoord = yDepart + rayon * lambda * sin(radian)         'y en pixels.
  gosub Impulsion   
next
return
xPrime = xDepart / lambda * cos(theta) + temps * sin(theta)'xPrime selon Lorentz.
'if afficher = 0 then circle(xPrime * lambda-marge/2, yDepart-marge/2), rayon * lambda, blanc,,, 1 / gLorentz 'ellipse mobile.

ChoixD:'******************* SOURCES ALIGN�ES SUR UNE DROITE HORIZONTALE ***********************

for xCoord = xDepart to xDepart + longueur step pas       'x en pixels.
  gosub Impulsion
next
return

ChoixE:'******************** SOURCES ALIGN�ES SUR UNE DROITE VERTICALE ************************

xCoord = xDepart                                          'en longueurs d'onde.
for yCoord = yDepart to yDepart + longueur step pas       'y en pixels.
  gosub Impulsion
next
return

'------------------------------------- DESSIN DES FL�CHES -------------------------------------
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

Impulsion:
'-------------------------- APPLIQUER LES TRANSFORMATIONS DE LORENTZ --------------------------

'    Admirez ici toute la � magie � des �quations de Lorentz, qui � elles seules, et tr�s
'    simplement, permettent d'effectuer quatre transformations bien distinctes, c'est � dire:

' 1- Une contraction en longueurs d'onde sur l'axe du d�placement selon: x * cos(theta).
' 2- Un d�calage horaire en p�riodes d'onde selon: -x * sin(theta), donc �gal � -beta si x = 1.
'    Ce d�calage se traduit par une � onde de phase � tr�s visible avec le choix � B �.
' 3- Un ralentissement de la p�riode selon: t * cos(theta). Ainsi, malgr� l'effet Doppler,
'    la longueur d'onde demeure inchang�e sur un axe transversal: y' = y; z' = z.
' 4- Un mouvement de translation en longueurs d'onde correspondant �: t * sin(theta).

'    Je tiens � pr�ciser que les �quations pr�sent�es par Lorentz �taient invers�es:
'    xPrime = (xOrig - temps * sin(theta)) / cos(theta)   'avec permutation de x, x' et t, t'.
'    tPrime = temps( + xOrig * sin(theta)) / cos(theta)   'et donc:
'    x' = (x - t * beta) / g
'    t' = (t + x * beta) / g                              'en effet:
'    Le sinus de l'angle th�ta correspond � la vitesse normalis�e: beta = v / c. 
'    Le cosinus de l'angle th�ta correspond au facteur de contraction: g = sqr(1 - beta ^ 2).

'    Il s'agissait pour Lorentz de corriger l'effet Doppler qui s'�tait d�j� produit. Mais
'    cela les a rendues incompr�hensibles. Minkowski, Einstein et m�me Poincar� ont alors os�
'    envisager, contre toute logique, une transformation de l'espace et du temps! Ils auraient
'    mieux fait d'envisager une transformation de la longueur et de la fr�quence des ondes,
'    car tous (y compris Lorentz) appliquaient ces transformations aux �quations de Maxwell...

xOrig =  xCoord / lambda
xPrime = xOrig * cos(theta) + temps * sin(theta)          ' x et x' en longueurs d'onde.
tPrime = temps * cos(theta) - xOrig * sin(theta)          ' t et t' en p�riodes d'onde.
yPrime = yCoord                                           ' selon Lorentz, on a y'= y
                                                          ' (ici en pixels pour le raccourci).

'---------------------------- MAINTENIR L'ENSEMBLE DANS L'�CRAN -------------------------------
if tPrime < 0 then return                                 'tPrime respecte l'onde de phase.
if xPrime * lambda > largeur - marge / 2 then
  gosub ReculerGauche
elseif xPrime * lambda < marge / 2 then'                  'Le Scanner doit continuer son
  if scanner then                                         'travail co�te que co�te. Il faut
    if xPrime * lambda < marge/4 then gosub ReculerDroite 'donc lui accorder un sursis malgr�
  else gosub ReculerDroite'                               'l'affaiblissement sur les bords.
  end if
end if
'-------------------------- IMPULSION SELON UN CARR� DE NEUF PIXELS ---------------------------

cosinus(xPrime*lambda,  yPrime)   = cosinus(xPrime*lambda,  yPrime)  +4*cos(2*pi*tPrime)
cosinus(xPrime*lambda+1,yPrime)   = cosinus(xPrime*lambda+1,yPrime)  +2*cos(2*pi*tPrime)
cosinus(xPrime*lambda-1,yPrime)   = cosinus(xPrime*lambda-1,yPrime)  +2*cos(2*pi*tPrime)
cosinus(xPrime*lambda,  yPrime+1) = cosinus(xPrime*lambda,  yPrime+1)+2*cos(2*pi*tPrime)
cosinus(xPrime*lambda,  yPrime-1) = cosinus(xPrime*lambda,  yPrime-1)+2*cos(2*pi*tPrime)
cosinus(xPrime*lambda-1,yPrime-1) = cosinus(xPrime*lambda-1,yPrime-1) + cos(2*pi*tPrime)
cosinus(xPrime*lambda+1,yPrime-1) = cosinus(xPrime*lambda+1,yPrime-1) + cos(2*pi*tPrime)
cosinus(xPrime*lambda+1,yPrime+1) = cosinus(xPrime*lambda+1,yPrime+1) + cos(2*pi*tPrime)
cosinus(xPrime*lambda-1,yPrime+1) = cosinus(xPrime*lambda-1,yPrime+1) + cos(2*pi*tPrime)
if afficher = 0 then pset(xPrime*lambda-marge/2,  yPrime-marge/2), blanc
return

ReculerDroite:'-------------- D�PORTER L'ENSEMBLE VERS LA DROITE ------------------------------

' xPrime = xOrig * cos(theta) + temps * sin(theta)        '�quations de Lorentz invers�es.
' tPrime = temps * cos(theta) - xOrig * sin(theta)
' xOrig = (xPrime - temps * sin(theta)) / cos(theta)      'avec permutation de x, x' et t, t'.
' temps = (tPrime + xOrig * sin(theta)) / cos(theta)

xCible = largeur - 2 * marge - rayon * lambda * gLorentz  'cible, � droite de l'�cran.
if choix$ = "C" then xCible = largeur - marge - .4 * rayon * lambda * gLorentz'arc de cercle incomplet.
xOrig = xDepart / lambda                                  'ancienne origine en longueurs d'onde.
xPrime = xOrig * cos(theta) + temps * sin(theta)          'emplacement actuel du syst�me.
deportation = xCible - xPrime * lambda                    'distance de la cible.
if deportation < 1 then return                            'le syst�me est encore � droite.
tPrime = temps * cos(theta) - xOrig * sin(theta)          'temps t' actuel � conserver.
xDepart = xDepart + deportation * gLorentz                'reculer le point de d�part.
xOrig = xDepart / lambda                                  'origine x en longueurs d'onde.
temps = (tPrime + xOrig * sin(theta)) / cos(theta)        'temps en cours selon x et t'.

for x = largeur - deportation to -1 step -1: for y = -1 to hauteur+1'd�porter � droite.
      sinus(x + deportation,y) = sinus(x, y)
    cosinus(x + deportation,y) = cosinus(x, y)
next:next
for x = -1 to deportation: for y = -1 to hauteur + 1      'nouveau m�dium sans �nergie.
      sinus(x,y) = .00000001                              'non nul, acc�l�re le calcul (!?).
    cosinus(x,y) = .00000001
next:next
return

ReculerGauche:'-------------- D�PORTER L'ENSEMBLE VERS LA GAUCHE ------------------------------

xCible = marge + rayon * lambda * gLorentz                'cible, � gauche de l'�cran.
if choix$ = "C" then xCible = marge + .4 * rayon * lambda * gLorentz'arc de cercle incomplet.
xOrig = xDepart / lambda                                  'ancienne origine en longueurs d'onde.
xPrime = xOrig * cos(theta) + temps * sin(theta)          'emplacement actuel du syst�me.
deportation = xPrime * lambda - xCible                    'distance de la cible.
if deportation < 1 then return                            'le syst�me est encore � gauche.
tPrime = temps * cos(theta) - xOrig * sin(theta)          'temps t' actuel � conserver.
xDepart = xDepart - deportation * gLorentz                'reculer le point de d�part.
xOrig = xDepart / lambda                                  'origine x en longueurs d'onde.
temps = (tPrime + xOrig * sin(theta)) / cos(theta)        'temps en cours selon x et t'.
for x = -1 to largeur-deportation: for y = -1 to hauteur+1'd�porter le m�dium.
      sinus(x, y) =   sinus(x+deportation, y)
    cosinus(x, y) = cosinus(x+deportation, y)
next:next
for x = largeur - deportation to largeur + 1: for y = -1 to hauteur + 1'nouveau m�dium sans �nergie.
      sinus(x, y) = .00000001                             'non nul, acc�l�re le calcul (!?).
    cosinus(x, y) = .00000001
next:next
return

'----------------------------------------------------------------------------------------------
Initialisation:'------------------------ INITIALISATION ---------------------------------------
'----------------------------------------------------------------------------------------------
fond = rgb(225,225,225)                                   'd�finir les couleurs.
blanc= rgb(255,255,255)
gris = rgb(75,75,75)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
vert = rgb(0,150,0)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               's�quence bitmap si d�sir�.
theta = asin(beta)
gLorentz = sqr(1 - beta ^ 2) ' (ou gLorentz = cos(theta)  'facteur de contraction g de Lorentz.
c = cos(pi / 4)                                           'vitesse de l'onde en pixels/passage.
contraste = 2
espacer = 0                                               'afficher peu d'images (plus rapide).
scanner = 0
rayon = 0
marge = 100                                               '50 pixels ne sont pas affich�s.
gauche = (largeur - marge) / (.866 + 1)                   'selon: gauche*gLorentz = largeurEcran-gauche = droite
petitPas = 2 * pi / 11                                    'nombre impair.
xCentre = largeur / 2
yCentre = hauteur / 2
xDepart = 250                                             'en pixels, favoriser la gauche.
yDepart = yCentre                                         'en pixels.
' xOrig: abscisse x selon Lorentz pour calcul du temps t et t' (voir plus bas).
xgg = 400 - 50                                            'coordonn�es des fl�ches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'cr�er une page matrice.
color noir, fond: cls
line(0,0)-(largeur-100, hauteur-100),noir, bf
windowtitle "Ether18  -  L'effet Doppler selon Lorentz  -  Le Scanner du Temps  -  26 d�c. 2006"
ligne30$ = "  A - Deux sources sur des parall�les (interf�rences).     ": locate 30, 41: print ligne30$
ligne31$ = "  B - Sources align�es sur un cercle (ondes stationnaires).": locate 31, 41: print ligne31$
ligne32$ = "  C - ... sur un arc de cercle (tache d'Airy en 2-D).      ": locate 32, 41: print ligne32$
ligne33$ = "  D - sur une droite horizontale (diffraction de Fresnel). ": locate 33, 41: print ligne33$
ligne34$ = "  E - sur une droite verticale (diffraction de Fresnel).   ": locate 34, 41: print ligne34$
ligne35$ = "  I - Initialiser.  ": locate 35, 41: print ligne35$
ligne36$ = "  Quitter (Echap).  ": locate 36, 41: print ligne36$
ligne37$ = "                    "
locate 30
locate, 2: print "Vitesse: chiffre de 0 � 8. B�ta = ";      'directives.
           print using "#.#"; beta
locate, 2: print "Inverser le sens: appuyez sur le V.":?
locate, 2: print "Luminosit�: appuyez sur + ou -."
locate, 2: print "Contraste: fl�ches haut ou bas."
locate, 2: print "Reculer le syst�me: appuyez sur Entr�e."
locate, 2: print "P - Pause.  F - Acc�l�rer le calcul."
locate, 2: print "S - Activer le Scanner du Temps.      ";
color bleu
'--------------------------- DISTRIBUTION (N.B. xDepart en pixels)-----------------------------
select case choix$
  case "A": luminosite = 6: contraste = 1                 '---> DEUX SOURCES.
            lambda = 40: pas = 2 * pi / 49
            xOrig = xDepart / lambda                      'en longueurs d'onde.
            locate 30, 41: print ligne30$
  case "B": luminosite = 2: contraste = 1                 '---> CERCLE.  
            rayon = 3.6875: lambda = 50'                   rayon: �mettre au centre de l'un
            pas = 2 * pi / 999'                            des ventres des ondes stationnaires,
            locate 31, 41: print ligne31$'                 soit 0,1875 plus n * 0,5.
            if beta < 0 then xOrig = xDepart / lambda + rayon else xOrig = xDepart / lambda - rayon
  case "C": luminosite = .05: contraste = 2
            yDepart = 100: lambda = 24                    '---> ARC DE CERCLE.
            rayon = 16: pas = 2 * pi / 2000               'grand rayon (en longueurs d'onde).
            if beta < 0 then xOrig = xDepart / lambda + .4 * rayon else xOrig = xDepart / lambda - .4 * rayon
            locate 32, 41: print ligne32$
  case "D": luminosite = .004: contraste = 3: lambda = 16 '---> DROITE HORIZONTALE.
            yCoord = hauteur - 70
            longueur = 10 * lambda: pas = lambda / 20
            if beta = 0 then xDepart = xDepart - longueur / 2
            if beta < 0 then xOrig = (xDepart + longueur) / lambda else xOrig = xDepart / lambda
            locate 33, 41: print ligne33$
  case "E": luminosite = .02: contraste = 3: lambda = 20  '---> DROITE VERTICALE.
            longueur = 10 * lambda: pas = lambda / 10
            yDepart = yCentre - longueur / 2
            xOrig = xDepart / lambda
            locate 34, 41: print ligne34$
end select

' TRANSFORMATIONS DE LORENTZ, POUR R�F�RENCE:
' xPrime = xOrig * cos(theta) + temps * sin(theta)        '�quations de Lorentz invers�es.
' tPrime = temps * cos(theta) - xOrig * sin(theta)
' xOrig = (xPrime - temps * sin(theta)) / cos(theta)      '�quations normales avec
' temps = (tPrime + xOrig * sin(theta)) / cos(theta)      'permutation de x, x' et t, t'.

tPrime = 0                                                'initialiser le temps � t' � pour
temps = (tPrime + xOrig * sin(theta)) / cos(theta)        '�viter le ressac des ondes.
luminosite = luminosite * gLorentz                        'compenser �l'augmentation de masse�.

locate 35: color vert
locate,62: print "Ce programme FreeBASIC peut �tre"
locate,62: print "distribu�, copi� ou modifi� librement."
locate,62: print "Gabriel LaFreni�re  glafreniere.com";

for x = -1 to largeur + 1                                 'effacer.
  for y = -1 to hauteur + 1
    sinus(x, y) = .00000001                               'acc�l�re le calcul (!!?).
    cosinus(x, y) = .00000001
    amortissement(x,y) = 1
  next
next
'--------------------------------- M�MORISER L'AMORTISSEMENT ----------------------------------
racine = marge / 5
for x = -1 to marge
  angle = (pi / 2) * x / marge
  for y = -1 to hauteur+1
    amortissement(x,y) = sin(angle) ^ (1 / racine)
    amortissement(largeur-x,y) = amortissement(x,y)
    if y < x then                                         'les coins.
      amortissement(x,y) = sin((pi / 2) * y / marge) ^ (1 / racine)
      amortissement(largeur-x,y) = amortissement(x,y)
    end if
    if x > hauteur-y then
      amortissement(x,y) = sin((pi / 2) * (hauteur-y) / marge) ^ (1 / racine)
      amortissement(largeur-x,y) = amortissement(x,y)
    end if
  next
next
for x = marge to largeur - marge                          'le centre, haut et bas.
  for y = -1 to 100
    amortissement(x,y) = sin((pi / 2) * y / marge) ^ (1 / racine)
    amortissement(x,hauteur-y) = amortissement(x,y)
  next
next
gosub FlecheGauche
gosub FlecheDroite
pcopy 2, page1
pcopy 2, page2
screenset page1, page2
return
