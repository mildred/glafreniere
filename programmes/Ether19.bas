largeur = 900: hauteur = 560                              'nombre de particules par c�t�.
dim as single P3(-1 to largeur+1, -1 to hauteur+1)        'cr�� le 20 d�c. 2006.
dim as single P2(-1 to largeur+1, -1 to hauteur+1)
dim as single P1(-1 to largeur+1, -1 to hauteur+1)
dim as single amortissement(-1 to largeur+1, -1 to hauteur+1)
dim as single facteur, rapport, luminosite, contraste, pas, petitPas, gLorentz, xScan
dim as single amplitudeSinus, amplitudeCosinus, c, xOrig, yOrig, xDepart, yDepart
dim as single xPrime, yPrime, tPrime, temps, beta, theta, ixe, igrec, longueur, angleMiroir
dim as single xCoord, yCoord, periode, rotation, amplitude, rayon, petitRayon, radian
dim as single pi, angle, lambda, xCarre, yCarre, distance, xDistance, yDistance, P1Precedent
contraction = 1: beta = -.7: choix$ = "B"
screen 19,24,3: page2 = 1: gosub Initialisation

do'                       MOD�LISATION DE L'�THER EN DEUX DIMENSIONS.

' L'algorithme de M. Jocelyn Marcotte ci-dessous a �t� cr�� en janvier 2006. Il diff�re de
' celui qui a �t� invent� par M. Philippe Delmotte en juin 2005 selon les lois de Newton sur
' l'inertie. Le principe de base est cependant le m�me: il s'agit de transmettre � un granule
' central le � potentiel � moyen des quatre granules voisins de mani�re � ce que les �changes
' continuels de granule en granule permettent la transmission d'ondes � virtuelles �. Ici, les
' r�flexions son �limin�es en att�nuant progressivement les ondes sur les bords. On obtient
' une lame semi-transparente en r�duisant la valeur du potentiel l� o� se situe le miroir.

' MM. Delmotte et Marcotte ont le m�rite d'avoir r�ussi non seulement � faire en sorte que
' le potentiel se transmette de proche en proche, mais � obtenir des ondes parfaites. Pour
' autant que je sache, ce n'�tait pas le cas des autres algorithmes mis au point jusque l�.
' Toutefois je n'ai pas pu v�rifier la date exacte o� M. Paul Falstad a produit sa version,
' qui est manifestement correcte (http://www.falstad.com/). Son site existe depuis longtemps,
' mais ses applets ne pr�sentaient pas de telles ondes autrefois.

' Je tiens � souligner que c'est M. Anselme Dewavrin qui a r�ussi � retracer en octobre 2006
' les raisons m�caniques profondes qui font en sorte que ces deux algorithmes fonctionnent
' correctement. M. Dewavrin a d'abord remarqu�  que le principe de base de l'algorithme de
' M. Delmotte �tait apparent� � la m�thode d'Euler, qui permet de construire une sinuso�de.
' Il ne s'agit pas seulement de transmettre l'�nergie; il s'agit de faire en sorte qu'il se
' produise des oscillations, comme le montre le programme que j'avais mis au point � cet
' effet: oscillations_Delmotte_Dewavrin.bas (o� j'ai renomm� les variables selon Euler).
' Je lui ai alors sugg�r� d'analyser l'algorithme de M. Marcotte. � ma grande surprise, il m'a 
' imm�diatement transmis un algorithme inspir� d'un filtre num�rique, et qui �tait tout aussi
' capable de produire une sinuso�de: oscillations_Marcotte_Dewavrin.bas. Tous deux montrent 
' que la vitesse des ondes n'est pas exactement proportionnelle � leur longueur, d'o� un effet
' � quantique � li� au nombre de granules. Cela explique l'effet de lentille, par exemple.
' Ces programmes sont disponibles � cette adresse: http://www.glafreniere.com/programmes/

' Ainsi donc, MM. Delmotte, Marcotte, Dewavrin et moi avons r�ussi � mettre au point une
' v�ritable th�orie de l'�ther bas�e sur une structure granuleuse. Je suis tr�s fier d'avoir 
' particip� � l'�laboration de cette th�orie, qui sera un jour fondamentale en physique.
' D'ailleurs, Augustin Fresnel parlait de � points mat�riels s�par�s par des intervalles �.
' L'algorithme de M. Delmotte est parfois avantageux, mais il faut admirer la simplicit�
' �tonnante de l'algorithme de M. Marcotte, qui pourrait �tre qualifi� de � parfait �.

'----------------------------- CALCUL SELON M. JOCELYN MARCOTTE -------------------------------
  for x = -1 to largeur + 1: for y = -1 to hauteur + 1
    P3(x,y) = P2(x,y)                                     'm�moriser les deux derniers �tats
    P2(x,y) = P1(x,y)                                     'du potentiel, de mani�re � produi- 
  next: next                                              're des oscillations sinuso�dales.
  for x = 0 to largeur: for y = 0 to hauteur
    P2(x,y) = P2(x,y) * amortissement(x,y)                'antireflet, facultatif.
    P1(x,y) = (P2(x-1,y) + P2(x,y-1) + P2(x+1,y) + P2(x,y+1)) / 2 - P3(x,y)'4/2 = 2; 2-1 = 1,
  next: next                                              '  d'o� une transmission sans pertes.
'-------------------------------------- FIN DU CALCUL -----------------------------------------

' Voici la version � Euler � de M. Philippe Delmotte (voir Ether18) pour fins de comparaison:
' I repr�sente l'influence. S est le sinus et C, le cosinus (la quadrature v�ritable).

' I(x+1,y) = S(x,y) + S(x+2,y) + S(x+1,y-1) + S(x+1,y+1) - 4 * S(x+1,y)'un cran � l'avance.
' C(x,y) = C(x,y) + .5 * I(x,y)
' S(x,y) = (S(x,y) + C(x,y))

  getmouse xSouris, ySouris, , clic                       'v�rifier entre les affichages.
' if scanner or bitmap then afficher = espacer + 1        'afficher tous les cycles au besoin.
  if clic > 0 then afficher = 0: espacer = 0: scanner = 0 'agir au plus t�t.
  if afficher > espacer then                              'afficher une image sur deux.
    afficher = 0
    swap page1, page2
    screenset page1, page2
    pcopy 2, page1
    gosub AfficherCouleurs'---------- DIAGRAMME PRINCIPAL -------------------------------------
  else afficher +=1
  end if

  select case choix$'                                     'r�partition des t�ches.
    case "A": gosub ChoixA
    case "B": gosub ChoixB
    case "C": gosub ChoixC
    case "D": gosub ChoixD
  end select
  if bitmap and afficher = 0 then gosub Bitmaps           'capture d'images si d�sir�.

'------------------------------------ �COULEMENT DU TEMPS -------------------------------------
  temps = temps + c / lambda  'le temps t de Lorentz est exprim� en p�riode d'onde. Lambda est
' exprim� en pixels et la vitesse des ondes, soit c, en pixels par cycle de calcul.

'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = inkey
  if len(saisie$) then
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    if beta <> 0 and saisie$ = "S" then saisie$ = "T"     'texte diff�rent si beta = 0.
    if bitmap and saisie$ = "Z" then bitmap = 0: capture = 0: saisie$ = ""
    if scanner then                                       'prioritaire avec le Scanner.
      select case saisie$
      case "P"                                            'pause accept�e, trait�e plus bas.
      case "+","-","H+","P+": saisie$ = ""                '�viter si le scanner est actif.
      case else                                 'toute autre intervention d�sactive le scanner.
        scanner = 0: screenset 2, page2: locate 37, 2 
        print "S - Activer le Scanner du Temps.      ";
        if saisie$ = "S" or saisie$ = "T" then saisie$ = ""  
      end select
    end if
    select case saisie$
      case "A","B","C","D": choix$ = saisie$
      case "X+", "k+",chr$(27): end                       'le � X � varie selon le FBIde(!?)
      case chr(13): reculer = 1: saisie$ = ""
      case "I": contraction = 1: beta = -.7: choix$ = "B" 'initialiser.
      case "V": beta = -beta                              'vitesse n�gative ou positive.
      case "Z": bitmap = 1: saisie$ = ""                  'cr�er une s�quence bitmap.
      case "P": sleep: saisie$ = ""                       'pause.
      case "F": saisie$ = "":                             'afficher peu d'images.
                if espacer = 20 then
                  espacer = 1
                  screenset 2, page2: locate 36, 14: print "F - Acc�l�rer le calcul. "
                else
                  espacer = 20
                  screenset 2, page2: locate 36, 14: print "F - Afficher normalement."
                end if
      case "L": if contraction then contraction = 0 else contraction = 1'lame � 45� si d�sir�.
      case "S": saisie$ = ""                              'Scanner du Temps; beta = 0.
                scanner = 1: espacer = 0: xScan = gauche + marge / 2
                screenset 2, page2: locate 37, 2: print "S - D�sactiver le Scanner du Temps.   ";
                line(0,0)-(largeur-marge, hauteur-marge),noir, bf
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
                line(0,0)-(largeur-marge, hauteur-marge),noir, bf
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
      case "K+":run "Ether18.exe"                         'fl�che gauche.
      case "M+":run "Ether20.exe"                         'fl�che droite.
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
  if ligne > 30 and ligne < 38 then
    if xSouris < 320 then ligne = 0
    if ligne > 34 and xSouris > 480 then ligne = 0
  else ligne = 0
  end if

'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 41
  select case ligne
    case 31: if not choix$ = "A" then print ligne31$      'choix en cours d�j� affich� en bleu.
    case 32: if not choix$ = "B" then print ligne32$
    case 33: if not choix$ = "C" then print ligne33$
    case 34: if not choix$ = "D" then print ligne34$      
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
      case 31: choix$ = "A": gosub Initialisation
      case 32: choix$ = "B": gosub Initialisation
      case 33: choix$ = "C": gosub Initialisation
      case 34: choix$ = "D": gosub Initialisation
      case 35: contraction = 1: beta = -.7: choix$ = "B": gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether18.exe" else run "Ether20.exe"
    end select
  end if
loop

AfficherCouleurs:'****************** AFFICHER EN COULEURS *************************************

for x = marge/2 to largeur - marge/2                      'masquer 50 pixels (100 amortis).
  if abs(x - xScan) < 1 then scan = 1 else scan = 0       'activer le scanner si x = xScan
  for y = marge/2 to hauteur - marge/2
    luminance = luminosite * abs(P2(x, y)) ^ contraste    'afficher en rouge et vert. Ajouter
    if P2(x, y) < 0 then                                  'du bleu car les couleurs doivent
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
      if scan then                                        'afficher la moiti� transform�e � droite.
        screenset 2                                       'afficher sur la page matrice.
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
  line(gauche, 0)-(gauche, hauteur - marge), fond         's�parer la fen�tre en deux parties.
  line((xScan-marge/2)*.866+gauche+1, 0)-((xScan-marge/2)*.866+gauche+1, hauteur - marge), fond'� imprimante �.
end if
return

Antireflet:'---------------------- M�MORISER L'AMORTISSEMENT ----------------------------------
racine = marge / 5
for x = marge to largeur - marge                          'r�tablir l'amortissement normal.
  for y = marge to hauteur - marge
    amortissement(x,y) = 1
  next
next

for x = -1 to marge'                                      'voir le programme: reflexions.bas.
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
  for y = -1 to marge
    amortissement(x,y) = sin((pi / 2) * y / marge) ^ (1 / racine)
    amortissement(x,hauteur-y) = amortissement(x,y)
  next
next
return

Bitmaps:'-------------------------- Cr�er une s�quence bitmap ---------------------------------
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

ChoixA:'* PULSATION D'UN ARC DE CERCLE HORIZONTAL SELON LES TRANSFORMATIONS DE LORENTZ ********

for radian = pi / 2.3 to pi - pi / 2.3 step pas           'arc de cercle.
  xCoord = xDepart + rayon * lambda * cos(radian)         'x en pixels.
  yCoord = yDepart + rayon * lambda * sin(radian)         'y en pixels.
  gosub Impulsion   
next
'-------------------------------- D�PLACER LE MIROIR INCLIN� ----------------------------------

xCoord = (xDepart/lambda) * cos(theta) + temps * sin(theta)'coordonn�e de d�part du miroir.
xMiroir = xCoord * lambda - longueur * gLorentz / 2
if xMiroir<marge and beta<0 then gosub ReculerDroite: return'maintenir l'ensemble dans l'�cran.
if xMiroir+longueur > largeur-marge and beta > 0 then gosub ReculerGauche: return
if reculer then                                           'ex�cuter la commande de reculer.
  if beta > 0 then gosub ReculerGauche
  if beta < 0 then gosub ReculerDroite
  reculer = 0
end if
if afficher = 0 then                                      'afficher l'espace du miroir.
  line(xMiroir-marge/2, yMiroir-marge/2)-(xMiroir-marge/2+longueur, yMiroir-marge/2-longueur), gris
  line(xMiroir-marge/2, yMiroir-marge/2)-(xMiroir-marge/2+longueur, yMiroir-marge/2-longueur), gris, b
  line(xMiroir-marge/2+longueur*gLorentz, yMiroir-marge/2)-(xMiroir-marge/2+longueur*gLorentz, yMiroir-marge/2-longueur), gris
end if
for x = xMiroir to xMiroir + longueur * gLorentz          'miroir.
  y = yMiroir - (x - xMiroir) / tan(angleMiroir)          'coordonn�e y du miroir.
  P1(x,y) = .75 * P1(x,y)                                 '.75 -> ajuster la transparence.
  if afficher = 0 and scanner = 0 then pset(x-marge/2,  y-marge/2), blanc: pset(x-marge/2+1, y-marge/2), blanc'afficher le miroir.
next
return

ChoixB:'* PULSATION D'UN ARC DE CERCLE VERTICAL SELON LES TRANSFORMATIONS DE LORENTZ **********
for radian = -.25 to .25 step pas                         'arc de cercle � droite du centre.
  xCoord = xDepart + rayon * lambda * cos(radian)         'en pixels.
  yCoord = yDepart + rayon * lambda * sin(radian)         'en pixels.
  gosub Impulsion
next
'-------------------------------- D�PLACER LE MIROIR INCLIN� ----------------------------------

xCoord = ((xDepart+175)/lambda) * cos(theta) + temps * sin(theta)'coordonn�e de d�part du miroir.
xMiroir = xCoord * lambda                                 ' maintenir l'ensemble dans l'�cran:
if xMiroir < marge + 50 and beta < 0 then gosub ReculerDroite: return
if xMiroir+(longueur+40)*gLorentz>largeur-marge and beta>0 then gosub ReculerGauche:return
if reculer then                                           'ex�cuter la commande de reculer.
  if beta > 0 then gosub ReculerGauche
  if beta < 0 then gosub ReculerDroite
  reculer = 0
end if
if afficher = 0 then                                      'afficher l'espace du miroir.
  x1 = xMiroir - longueur / 2 - marge / 2
  y1 = yDepart + longueur / 2 - marge / 2
  line(x1, y1)-(x1 + longueur, y1 - longueur), gris, b    'carr� non contract�.
end if
for y = yDepart - longueur / 2 to yDepart + longueur / 2  'miroir plus petit que la source.
  x = xMiroir - (yDepart - y) * tan(angleMiroir)          'coordonn�e x du miroir.
  P1(x,y) = .85 * P1(x,y)                                 '.85 -> ajuster la transparence.
  if afficher = 0 then 
    pset(x-marge/2, y-marge/2), blanc                     'afficher le miroir.
    pset(x-marge/2+1, y-marge/2), blanc
  end if
next

return

ChoixC:'******************* SOURCES ALIGN�ES SUR UNE DROITE HORIZONTALE ***********************

for xCoord = xDepart to xDepart + longueur step pas       'x en pixels.
  gosub Impulsion                                         'g�n�rer les ondes.
next
'-------------------------------- D�PLACER LE MIROIR INCLIN� ----------------------------------

xCoord = (xDepart/lambda) * cos(theta) + temps * sin(theta)'coordonn�e de d�part du miroir.
xMiroir = xCoord * lambda
if xMiroir<marge and beta<0 then gosub ReculerDroite: return'maintenir l'ensemble dans l'�cran.
if xMiroir+longueur > largeur-marge and beta > 0 then gosub ReculerGauche:return
if reculer then                                           'ex�cuter la commande de reculer.
  if beta > 0 then gosub ReculerGauche
  if beta < 0 then gosub ReculerDroite
  reculer = 0
end if
if afficher = 0 then                                      'afficher l'espace du miroir.
  line(xMiroir-marge/2, yMiroir-marge/2)-(xMiroir-marge/2+longueur,  yMiroir-marge/2-longueur), gris
  line(xMiroir-marge/2, yMiroir-marge/2)-(xMiroir-marge/2+longueur,  yMiroir-marge/2-longueur), gris, b
  line(xMiroir-marge/2+longueur*gLorentz, yMiroir-marge/2)-(xMiroir-marge/2+longueur*gLorentz,  yMiroir-marge/2-longueur), gris
end if
for x = xMiroir - 20 to xMiroir + longueur * gLorentz + 20'miroir plus grand que la source.
  y = yMiroir - (x - xMiroir) / tan(angleMiroir)          'coordonn�e y du miroir.
  P1(x,y) = .75 * P1(x,y)                                 '.75 -> ajuster la transparence.
  if afficher = 0 and scanner = 0 then pset(x-marge/2,  y-marge/2), blanc: pset(x-marge/2+1, y-marge/2), blanc'afficher le miroir.
next
return

ChoixD:'******************* SOURCES ALIGN�ES SUR UNE DROITE VERTICALE ************************
if reculer then
  if beta > 0 then gosub ReculerGauche
  if beta < 0 then gosub ReculerDroite
  reculer = 0
end if
xCoord = xDepart
for yCoord = yDepart - longueur to yDepart step pas       'y en pixels.
  gosub Impulsion
next
'-------------------------------- D�PLACER LE MIROIR INCLIN� ----------------------------------

xCoord = ((xDepart+espace)/lambda) * cos(theta) + temps * sin(theta)'coordonn�e de d�part du miroir.
xMiroir = xCoord * lambda
' maintenir l'ensemble dans l'�cran:
if xMiroir-espace<marge and beta<0 then gosub ReculerDroite:return'maintenir l'ensemble dans l'�cran.
if xMiroir+(longueur+40)*gLorentz>largeur-marge and beta>0 then gosub ReculerGauche:return
if reculer then                                           'ex�cuter la commande de reculer.
  if beta > 0 then gosub ReculerGauche
  if beta < 0 then gosub ReculerDroite
  reculer = 0
end if
if afficher = 0 then                                      'afficher l'espace du miroir.
  x1 = xMiroir-marge/2-20*gLorentz
  y1 = yDepart-marge/2+20
  line(x1, y1)-(x1+longueur+40,  y1-longueur-40), gris, b
  line(x1, y1)-(x1+longueur+40,  y1-longueur-40), gris
  line(x1+(longueur+40)*gLorentz, y1-longueur-40)-(x1+(longueur+40)*gLorentz, y1), gris
end if
for y = yDepart - longueur - 20 to yDepart + 20           'miroir plus grand que la source.
  x = xMiroir + (yDepart - y) * tan(angleMiroir)          'coordonn�e x du miroir.
  P1(x,y) = .85 * P1(x,y)                                 '.85 -> ajuster la transparence.
  if afficher = 0 and scanner = 0 then pset(x-marge/2, y-marge/2), blanc: pset(x-marge/2+1, y-marge/2), blanc'afficher le miroir.
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
x = xPrime * lambda                                       ' coordonn�e du pixel, nombre entier.
if tPrime < 0 then return                                 ' tPrime respecte l'onde de phase.

'-------------------------- IMPULSION SELON UN CARR� DE NEUF PIXELS ---------------------------
P1(x,  yPrime)   = P1(x,  yPrime)  +4*cos(2*pi*tPrime)
P1(x+1,yPrime)   = P1(x+1,yPrime)  +2*cos(2*pi*tPrime)
P1(x-1,yPrime)   = P1(x-1,yPrime)  +2*cos(2*pi*tPrime)
P1(x,  yPrime+1) = P1(x,  yPrime+1)+2*cos(2*pi*tPrime)
P1(x,  yPrime-1) = P1(x,  yPrime-1)+2*cos(2*pi*tPrime)
P1(x-1,yPrime-1) = P1(x-1,yPrime-1) + cos(2*pi*tPrime)
P1(x+1,yPrime-1) = P1(x+1,yPrime-1) + cos(2*pi*tPrime)
P1(x+1,yPrime+1) = P1(x+1,yPrime+1) + cos(2*pi*tPrime)
P1(x-1,yPrime+1) = P1(x-1,yPrime+1) + cos(2*pi*tPrime)
if afficher = 0 and scanner = 0 then pset(x-marge/2,  yPrime-marge/2), blanc
return

ReculerDroite:'-------------- D�PORTER L'ENSEMBLE VERS LA DROITE ------------------------------

xCoord = (xDepart/lambda) * cos(theta) + temps * sin(theta)'coordonn�e de d�part du miroir.
' xPrime = xOrig * cos(theta) + temps * sin(theta)        '�quations de Lorentz invers�es.
' tPrime = temps * cos(theta) - xOrig * sin(theta)
' xOrig = (xPrime - temps * sin(theta)) / cos(theta)      'avec permutation de x, x' et t, t'.
' temps = (tPrime + xOrig * sin(theta)) / cos(theta)
select case choix$
  case "A": xCible = largeur - marge-100-.2*rayon*lambda  'arc de cercle incomplet.
  case "B": xCible = .4 * largeur / gLorentz
  case "C": xCible = largeur - marge-longueur*gLorentz
  case "D": xCible = largeur - marge-300                  'moins distance du miroir.
end select
xOrig = xDepart / lambda                                  'ancienne origine en longueurs d'onde.
xPrime = xOrig * cos(theta) + temps * sin(theta)          'emplacement actuel du syst�me.
deportation = xCible - xPrime * lambda                    'distance de la cible.
if deportation < 1 then return                            'le syst�me est encore � droite.
tPrime = temps * cos(theta) - xOrig * sin(theta)          'temps t' actuel � conserver.
xDepart = xDepart + deportation * gLorentz                'reculer le point de d�part.
xOrig = xDepart / lambda                                  'origine x en longueurs d'onde.
temps = (tPrime + xOrig * sin(theta)) / cos(theta)        'temps en cours selon x et t'.

for x = largeur - deportation to -1 step -1: for y = -1 to hauteur+1'd�porter � droite.
      P2(x + deportation,y) = P2(x, y)
    P1(x + deportation,y) = P1(x, y)
next:next
for x = -1 to deportation: for y = -1 to hauteur + 1      'm�dium neuf sans �nergie.
    P2(x,y) = .00000001                                   'non nul, acc�l�re le calcul (!?).
    P1(x,y) = .00000001
next:next
return

ReculerGauche:'-------------- D�PORTER L'ENSEMBLE VERS LA GAUCHE ------------------------------

xCoord = (xDepart/lambda) * cos(theta) + temps*sin(theta) 'coordonn�e de d�part du miroir.
select case choix$
  case "A": xCible = marge + .2*rayon*lambda*gLorentz     'arc de cercle incomplet.
  case "B": xCible = marge
  case "C": xCible = marge
  case "D": xCible = marge + espace                       'xCible: � gauche de l'�cran.
end select
xOrig = xDepart / lambda                                  'ancienne origine en longueurs d'onde.
xPrime = xOrig * cos(theta) + temps * sin(theta)          'emplacement actuel du syst�me.
deportation = xPrime * lambda - xCible                    'distance de la cible.
if deportation < marge then return                        'le syst�me est encore trop � gauche.
tPrime = temps * cos(theta) - xOrig * sin(theta)          'temps t' actuel � conserver.
xDepart = xDepart - deportation * gLorentz                'reculer le point de d�part.
xOrig = xDepart / lambda                                  'origine x en longueurs d'onde.
temps = (tPrime + xOrig * sin(theta)) / cos(theta)        'temps en cours selon x et t'.
for x = -1 to largeur-deportation: for y = -1 to hauteur+1'd�porter le m�dium.
      P2(x, y) =   P2(x+deportation, y)
    P1(x, y) = P1(x+deportation, y)
next:next
for x = largeur - deportation to largeur + 1: for y = -1 to hauteur + 1'nouveau m�dium sans �nergie.
    P2(x, y) = .00000001                                  'non nul: acc�l�re le calcul (!?).
    P1(x, y) = .00000001
next:next
return

'----------------------------------------------------------------------------------------------
Initialisation:'------------------------ INITIALISATION ---------------------------------------
'----------------------------------------------------------------------------------------------
fond = rgb(225,225,225)                                   'd�finir les couleurs.
blanc= rgb(255,255,255)
gris = rgb(150,150,150)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
vert = rgb(0,150,0)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               's�quence bitmap si d�sir�.
theta = asin(beta)
gLorentz = sqr(1 - beta ^ 2) ' ou gLorentz = cos(theta)   'facteur de contraction g de Lorentz.
c = cos(pi / 4)                                           'vitesse de l'onde en pixels/cycle.
espacer = 1                                               'afficher un cycle sur deux.
scanner = 0
rayon = 0
marge = 100                                               '50 pixels ne sont pas affich�s.
espace = 100                                              'entre la source et la miroir.
gauche = (largeur - marge) / (.866 + 1)                   'selon: gauche*gLorentz = largeurEcran-gauche = droite
petitPas = 2 * pi / 11                                    'nombre impair.
xCentre = largeur / 2
yCentre = hauteur / 2
yDepart = yCentre                                         'en pixels.
angleMiroir = pi / 4                                      'si la lame demeure � 45�.
if contraction then angleMiroir = atn(gLorentz / 1)       'l'angle varie selon la contraction.
xgg = 400 - 50                                            'coordonn�es des fl�ches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'cr�er une page matrice.
color noir, fond: cls
line(0,0)-(largeur-marge, hauteur-marge),noir, bf
windowtitle "Ether19  -  La lame s�paratrice de l'interf�rom�tre de Michelson."
locate 30,47: print "Sources align�es sur :"
ligne31$ = "  A - un arc de cercle horizontal. ": locate 31, 41: print ligne31$
ligne32$ = "  B - un arc de cercle vertical.   ": locate 32, 41: print ligne32$
ligne33$ = "  C - une droite horizontale.      ": locate 33, 41: print ligne33$
ligne34$ = "  D - une droite verticale.        ": locate 34, 41: print ligne34$
ligne35$ = "  I - Initialiser.  ": locate 35, 41: print ligne35$
ligne36$ = "  Quitter (Echap).  ": locate 36, 41: print ligne36$
ligne37$ = "                    "
locate 30
locate, 2: print "Vitesse: chiffre de 0 � 8. B�ta = ";      'directives.
           print using "#.#"; beta
locate, 2: print "V - Inverser le sens du d�placement."
locate, 2: print "L - Angle de la lame: ";
           print using "##.##"; angleMiroir * 180 / pi;: print chr(248)
locate, 2: print "Luminosit�: appuyez sur + ou -."
locate, 2: print "Contraste: fl�ches haut ou bas."
locate, 2: print "Reculer le syst�me: appuyez sur Entr�e."
locate, 2: print "P - Pause.  F - Acc�l�rer le calcul."
locate, 2: print "S - Activer le Scanner du Temps.      ";
color bleu
'--------------------------- DISTRIBUTION (N.B. xDepart en pixels)-----------------------------
select case choix$
  case "A": luminosite = .7: contraste = 1.5              '---> ARC DE CERCLE HORIZONTAL.
            yDepart = marge: lambda = 24
            rayon = 16: pas = 2 * pi / 2000               'grand rayon (en longueurs d'onde).
            longueur = .4 * rayon * lambda                'longueur du miroir selon l'arc.
            yMiroir = .6 * hauteur
            if beta < 0 then
              xDepart = 600 * gLorentz
              xOrig = xDepart / lambda + .2 * rayon
            else
              xDepart = 180: c = .695                     'les ondes courtes sont plus lentes.
              xOrig = xDepart / lambda - .2 * rayon
            end if
            locate 31, 41: print ligne31$
  case "B": luminosite = 4: contraste = 1                 '---> ARC DE CERCLE VERTICAL.
            rayon = 18: lambda = 24: yDepart = .65 * hauteur
            longueur = .45 * rayon * lambda                'longueur du miroir selon l'arc.
            pas = 2 * pi / 3000
            if beta < 0 then
              xDepart = 150 * gLorentz ^ 2: c = .66
              xOrig = xDepart / lambda + rayon
            else
              xDepart = 100
              xOrig = (xDepart + longueur) / lambda + rayon - 200 / lambda
            end if
            locate 32, 41: print ligne32$'                 soit 0,1875 plus n * 0,5.
  case "C": luminosite = 1: contraste = 1.5: lambda = 24  '---> DROITE HORIZONTALE.
            yCoord = hauteur - 70: yMiroir = 380          'point de d�part du miroir, axe y.
            longueur = 10 * lambda: pas = lambda / 20
            if beta < 0 then
              xDepart = 500 * gLorentz ^ 3
              xOrig = (xDepart + longueur) / lambda
            else
              xDepart = marge: c = .695
              xOrig = xDepart / lambda
            end if
            locate 33, 41: print ligne33$
  case "D": luminosite = 1: contraste = 1.5: lambda = 24  '---> DROITE VERTICALE.
            longueur = 8 * lambda: pas = lambda / 20
            yDepart = hauteur - marge - 22                '22: miroir plus large que la source.
            if beta<0 then xDepart = largeur-marge-longueur-espace-100 else xDepart = marge*gLorentz: c = .695
            xOrig = xDepart / lambda
            locate 34, 41: print ligne34$
end select

' TRANSFORMATIONS DE LORENTZ, POUR R�F�RENCE:
' xPrime = xOrig * cos(theta) + temps * sin(theta)        '�quations de Lorentz invers�es.
' tPrime = temps * cos(theta) - xOrig * sin(theta)
' xOrig = (xPrime - temps * sin(theta)) / cos(theta)      '�quations originales de Lorentz
' temps = (tPrime + xOrig * sin(theta)) / cos(theta)      'avec permutation de x, x' et t, t'.

' Il faut initialiser le temps t' en se r�f�rant au point x (ici xOrig) selon l'�quation
' originale de Lorentz pour le r�trocalcul du temps t, ce qui �vite le ressac des ondes.
' Cette proc�dure permet aussi de d�marrer les impulsions � partir de l'onde de phase,
' l� o� t' vaut constamment z�ro. De cette mani�re, la premi�re onde est toujours compl�te.
tPrime = 0
temps = (tPrime + xOrig * sin(theta)) / cos(theta)        
luminosite = luminosite * gLorentz                        'compenser �l'augmentation de masse�.

locate 30: color vert
locate,79: print "Le 23 janvier 2007."
locate,79: print "Code source FreeBASIC"
locate,79: print "Gabriel LaFreni�re"
locate,79: print "glafreniere.com":?
locate,70: print "Ondes selon Jocelyn Marcotte."
locate,70: print "Ce programme peut �tre copi�,"
locate,70: print "modifi� ou distribu� librement.";
for x = -1 to largeur + 1                                 'effacer.
  for y = -1 to hauteur + 1
    P1(x, y) = .00000001                                  'acc�l�re le calcul (!!?).
    P2(x, y) = .00000001
    P3(x, y) = .00000001
  next
next
gosub Antireflet
gosub FlecheGauche
gosub FlecheDroite
pcopy 2, page1
pcopy 2, page2
screenset page1, page2
return
