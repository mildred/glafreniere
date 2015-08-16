largeur = 1000: hauteur = 795                             'nombre de particules par c�t�.
dim as single max(-1 to hauteur+1)                        'cr�� le 19 janvier 2006.
dim as single amplit(-1 to hauteur+1)
dim as single P3(-1 to largeur+1, -1 to hauteur+1)
dim as single P2(-1 to largeur+1, -1 to hauteur+1)
dim as single P1(-1 to largeur+1, -1 to hauteur+1)
dim as single amortissement(-1 to largeur+1, -1 to hauteur+1)
dim as single facteur, rapport, luminosite, contraste, pas, xMax
dim as single xCoord, yCoord, periode, rotation, amplitude, ampl
dim as single amplitudeSinus, amplitudeCosinus, c, temps, phase
dim as single pi, angle, lambda, xCarre, yCarre, distance, xDistance, yDistance
choix$ = "B": screen 19,24,3: page2 = 1: gosub Initialisation

do'                       MOD�LISATION DE L'�THER EN DEUX DIMENSIONS.

' L'algorithme de M. Jocelyn Marcotte ci-dessous a �t� cr�� en janvier 2006. Il diff�re de
' celui qui a �t� invent� par M. Philippe Delmotte en juin 2005 selon les lois de Newton sur
' l'inertie. Le principe de base est cependant le m�me: il s'agit de transmettre � un granule
' central le � potentiel � moyen des quatre granules voisins de mani�re � ce que les �changes
' continuels de granule en granule permettent la transmission d'ondes � virtuelles �. Ici, les
' r�flexions son �limin�es en att�nuant progressivement les ondes sur les bords. On obtient
' une r�flexion en n'effectuant pas le calcul l� o� se situe l'obstruction.

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
  for x = -1 to xMaxim + 1: for y = -1 to hauteur + 1
    P3(x,y) = P2(x,y)                                     'm�moriser les deux derniers �tats
    P2(x,y) = P1(x,y)                                     'du potentiel, de mani�re � produi- 
  next: next                                              're des oscillations sinuso�dales.
  for x = 0 to xMaxim
    if choix$ = "C" and x = source then obstruction = 1 else obstruction = 0
    for y = 0 to hauteur
      P2(x,y) = P2(x,y) * amortissement(x,y)              'antireflet.
      P1(x,y) = (P2(x-1,y) + P2(x,y-1) + P2(x+1,y) + P2(x,y+1)) / 2 - P3(x,y)
      if obstruction then                                 'obstruction du choix "C".
        if y < haut or y > bas then else P1(x,y) = P2(x,y)
      end if
    next
  next
'-------------------------------------- FIN DU CALCUL -----------------------------------------

' Voici la version � Euler � de M. Philippe Delmotte (voir Ether18) pour fins de comparaison:
' I repr�sente l'influence. S est le sinus et C, le cosinus (la quadrature v�ritable).

' I(x+1,y) = S(x,y) + S(x+2,y) + S(x+1,y-1) + S(x+1,y+1) - 4 * S(x+1,y)'un cran � l'avance.
' C(x,y) = C(x,y) + .5 * I(x,y)
' S(x,y) = (S(x,y) + C(x,y))

  getmouse xSouris, ySouris, , clic                       'v�rifier entre les affichages.
  if clic > 0 then afficher = 0: espacer = 0              'agir au plus t�t.
  if afficher > espacer then                              'afficher une image sur deux.
    afficher = 1
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
  end select
  if bitmap and afficher = 0 then gosub Bitmaps           'capture d'images si d�sir�.
  if xMaxim < largeur then xMax = xMax + c: xMaxim = xMax 'omettre la zone sans ondes.

'------------------------------------ �COULEMENT DU TEMPS -------------------------------------
  temps = temps + pas: if temps > 2 * pi then temps = temps - 2 * pi

'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = inkey
  if len(saisie$) then
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
    if bitmap and saisie$ = "Z" then bitmap = 0: capture = 0: saisie$ = ""
    select case saisie$
      case "A","B","C": choix$ = saisie$
      case "X+", "k+",chr$(27): end                       'le � X � varie selon le FBIde(!?)
      case "I": choix$ = "B"                              'initialiser.
'     case "Z": bitmap = 1: saisie$ = ""                  'cr�er une s�quence bitmap.
      case "P": sleep: saisie$ = ""                       'pause.
      case "F": saisie$ = "":                             'afficher peu d'images.
                if espacer = 20 then
                  espacer = 1
                  screenset 2, page2: locate 37, 13: print "F- Acc�l�rer le calcul. ";
                else
                  espacer = 20
                  screenset 2, page2: locate 37, 13: print "F- Afficher normalement.";
                end if
      case "M": run "Ether00.exe"
      case "K+":run "Ether19.exe"                         'fl�che gauche.
      case "M+":run "Ether21.exe"                         'fl�che droite.
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
  if clic > 0 and ySouris < hauteur - 2 * deuxMarges then curseur = xSouris + marge: gosub PrincipeDeHuygens
  if ligne > 31 and ligne < 38 then
    if xSouris < 320 then ligne = 0
    if ligne > 35 and xSouris > 480 then ligne = 0
  else ligne = 0
  end if

'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 41
  select case ligne
    case 32: if not choix$ = "A" then print ligne32$      'choix en cours d�j� affich� en bleu.
    case 33: if not choix$ = "B" then print ligne33$
    case 34: if not choix$ = "C" then print ligne34$
    case 35: print ligne35$      
    case 36: print ligne36$
    case 37: print ligne37$;: if xSouris < 400 then gosub FlecheGauche else gosub FlecheDroite
  end select
  color noir, fond
'-------------------------------------- ACTIONS SUITE � UN CLIC -------------------------------
  if clic = 1 then
    clic = 0: bitmap = 0: afficher = 0
    select case ligne
      case 32: choix$ = "A": gosub Initialisation
      case 33: choix$ = "B": gosub Initialisation
      case 34: choix$ = "C": gosub Initialisation
      case 35: choix$ = "B": gosub Initialisation
      case 36: end
      case 37: if xSouris < 400 then run "Ether19.exe" else run "Ether21.exe"
    end select
  end if
loop

AfficherCouleurs:'****************** AFFICHER EN COULEURS *************************************
for x = marge to largeur - marge                          'masquer les 100 pixels amortis.
  if abs(x - xScan) < 1 then scan = 1 else scan = 0       'activer le scanner si x = xScan
  for y = deuxMarges to hauteur - deuxMarges              'masquer les pixels peu utiles.
    luminance = luminosite * abs(P1(x, y)) ^ contraste    'afficher en rouge et vert. Ajouter
    if P1(x, y) < 0 then                                  'du bleu car les couleurs doivent
      vert = .67 * luminance ' 0,67 + 0,33 = 1            '�tre compl�mentaires le plus possi-
      bleu = .33 * luminance ' 50% du vert.               'ble pour �viter une dominante jaune.
      if vert > 255 then rouge = .67 * luminance - 255 else rouge = 0
    else
      rouge = .75 * luminance ' 0,75 + 0,25 = 1           'proportion de bleu moindre pour
      bleu  = .25 * luminance ' 33% du rouge.             '�viter le fushia.
      if rouge > 255 then vert = .75 * luminance - 255 else vert = 0
      if rouge > 255 then bleu = bleu + .33 * (.75 * luminance - 255)'ajouter 33% du vert.
    end if
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    if bleu > 255 then bleu = 255 else if bleu < 0 then bleu = 0
    pset (x - marge, y - deuxMarges), rgb(rouge,vert,bleu)
  next
next
gosub Graphique
return

Antireflet:'---------------------- M�MORISER L'AMORTISSEMENT ----------------------------------
racine = marge / 5
for x = -1 to largeur + 1                                 'initialiser l'amortissement (1=nul).
  for y = -1 to hauteur + 1
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

ChoixA:'************************* �METTEUR LIN�AIRE SIMPLE ************************************
for y = haut to bas
  P1(source,y) = P1(source,y) + cos(temps)   
next
return

ChoixB:'****************** �METTEUR LIN�AIRE EN OPPOSITION DE PHASE ***************************
for y = 0 to hauteur
  P1(marge,y) = P1(marge,y) + cos(temps)   
next
for y = haut to bas
  P1(source,y) = P1(source,y) + cos(temps + pi)           'opposition de phase.
next
return

ChoixC:'************************** OBSTRUCTION �QUIVALENTE ************************************
for y = 0 to hauteur
  P1(marge,y) = P1(marge,y) + cos(temps)   
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

Graphique:'************************ GRAPHIQUE ET GABARIT **************************************

line(source-marge,haut-deuxMarges)-(source-marge,bas-deuxMarges), blanc'�metteur ou obstruction.
line(curseur - marge, 0)-(curseur - marge, hauteur - 2 * deuxMarges), blanc'zone du graphique.
for y = deuxMarges to hauteur - deuxMarges
  ampl = sqr(P1(curseur-la8, y) * P1(curseur-la8, y) + P1(curseur+la8, y) * P1(curseur+la8, y))
  ampl = ampl+sqr(P1(curseur, y-la8) * P1(curseur, y-la8) + P1(curseur, y+la8) * P1(curseur, y+la8))/2
  amplit(y)= (3 * amplit(y) + ampl) / 4'                  'moyenne pour stabiliser.
  ampl = 43 * P1(curseur, y)
  if choix$ = "A" then
    line(curseur-marge-30*amplit(y),y-deuxMarges)-(curseur-marge-amplitudePrecedente,y-deuxMarges-1), blanc
    line(curseur-marge+max(y-1),y-deuxMarges-1)-(curseur-marge+max(y),y-deuxMarges),bleuClair
  else
    line(curseur-14-max(y),y-deuxMarges)-(curseur-14+max(y),y-deuxMarges),noir
    line(curseur-marge+87-max(y-1),y-deuxMarges-1)-(curseur-marge+87-max(y),y-deuxMarges),bleuClair 'gabarit.
    line(curseur-marge+87+max(y-1),y-deuxMarges-1)-(curseur-marge+87+max(y),y-deuxMarges),bleuClair
    line(curseur-marge+30*amplit(y),y-deuxMarges)-(curseur-marge+amplitudePrecedente,y-deuxMarges-1), blanc
    line(curseur-marge-30*amplit(y),y-deuxMarges)-(curseur-marge-amplitudePrecedente,y-deuxMarges-1), blanc
  end if
  line(curseur-marge+amplPrec,y-deuxMarges-1)-(curseur-marge+ampl,y-deuxMarges), rgb(255,255,0)
  amplitudePrecedente = 30*amplit(y)
  amplPrec = ampl
next
return

'*************************** SOMMATION DES ONDELETTES DE HUYGENS ******************************
PrincipeDeHuygens:
if curseur < source + 50 then curseur = source + 50
distance = curseur - source
xCarre = distance ^ 2
for y = deuxMarges to hauteur - deuxMarges
  amplitudeSinus = 0
  amplitudeCosinus = 0
  for yDistance = haut to bas step .1
    yCarre = (yDistance - y) ^ 2
    distance = sqr(xCarre + yCarre)                       'Pythagore.
    phase = 2 * pi * distance / lambda
    amplitude = sqr(300) / sqr(distance)                  'amplitude: racine carr�e en 2-D.
    amplitudeSinus = amplitudeSinus + amplitude * sin(phase)
    amplitudeCosinus = amplitudeCosinus + amplitude * cos(phase)
  next
  max(y)=.15*sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
next
return
'----------------------------------------------------------------------------------------------
Initialisation:'------------------------ INITIALISATION ---------------------------------------
'----------------------------------------------------------------------------------------------
fond = rgb(225,225,225)                                   'd�finir les couleurs.
blanc= rgb(255,255,255)
gris = rgb(150,150,150)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
bleuClair = rgb(150,200,255)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               's�quence bitmap si d�sir�.
lambda = 12                                               'divisible par 8 pour quart d'onde.
la8 = lambda / 8
c = 1 / sqr(2)                                            'vitesse de l'onde en pixels/cycle.
c = .702                                                  'vitesse r�elle un peu plus lente.
pas = c * 2 * pi / lambda
temps = .15                                               '�vite un ressac au d�marrage.
espacer = 0                                               'cycles � ne pas afficher sur x+1.
luminosite = 100
contraste = 2
marge = 100                                               'pixels affaiblis.
deuxMarges = 2 * marge                                    'pixels non affich�s.
source = marge + 6 * lambda                               'source � 6 longueurs d'ondes.
xMax = source + 2 * lambda
xMaxim = xMax
xCentre = largeur / 2
yCentre = hauteur / 2
longueur = 10 * lambda                                    'longueur de l'�metteur.
haut = yCentre - longueur / 2
bas  = yCentre + longueur / 2
curseur = xCentre
xgg = 400 - 50                                            'coordonn�es des fl�ches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
screenset 2, 2                                            'cr�er une page matrice.
color noir, fond: cls
windowtitle "Ether20  -  La lumi�re traverse les objets."
ligne32$ = "  A- Source lin�aire seule.        ": locate 32, 41: print ligne32$
ligne33$ = "  B- Source en opposition de phase.": locate 33, 41: print ligne33$
ligne34$ = "  C- Obstruction lin�aire.         ": locate 34, 41: print ligne34$
ligne35$ = "  I- Initialiser.  ": locate 35, 41: print ligne35$
ligne36$ = "  Quitter (Echap).  ": locate 36, 41: print ligne36$
ligne37$ = "                    "
locate 26
locate, 2: print "Les ondes radio, les rayons X et les rayons gamma peuvent tr�s bien traverser certains objets. Il"
locate, 2: print "est donc permis de penser que m�me la lumi�re visible pourrait �galement les traverser. Ce pro-"
locate, 2: print "gramme montre qu'il suffit de supposer � priori qu'aucun objet ne peut intercepter ces ondes, peu"
locate, 2: print "importe leur fr�quence. C'est plut�t la mati�re qui �met de nouvelles ondes en opposition de"
locate, 2: print "phase, mais seulement dans la mesure o� ses �lectrons peuvent vibrer sur cette fr�quence.":?
locate, 2: print "Cliquez sur l'image pour d�placer le"
locate, 2: print "curseur. Le gabarit (en bleu) est"
locate, 2: print "calcul� selon le principe de Huygens."
locate, 2: print "Luminosit�: appuyez sur + ou -."
locate, 2: print "Contraste: fl�ches haut ou bas."
locate, 2: print "P- Pause.  F- Acc�l�rer le calcul.";
color bleu
'--------------------------- DISTRIBUTION (N.B. xDepart en pixels)-----------------------------
select case choix$
  case "A": locate 32, 41: print ligne32$                 '�metteur lin�aire simple.
  case "B": locate 33, 41: print ligne33$                 '�metteur en opposition de phase.
  case "C": locate 34, 41: print ligne34$                 'obstruction �quivalente.
end select

locate 32: color rgb(0,150,0)
locate,79: print "Le 18 avril 2007."
locate,79: print "Gabriel LaFreni�re"
locate,79: print "glafreniere.com"
locate,70: print "Ondes selon Jocelyn Marcotte."
locate,70: print "Ce programme peut �tre copi�,"
locate,70: print "modifi� ou distribu� librement.";
for x = -1 to largeur + 1                                 'effacer.
  for y = -1 to hauteur + 1
    P1(x, y) = .00000001                                  'acc�l�re le calcul (!?). Il semble
    P2(x, y) = .00000001                                  'que � option explicit � �limine
    P3(x, y) = .00000001                                  'ce probl�me d'initialisation.
  next
next
gosub Antireflet
gosub PrincipeDeHuygens
gosub FlecheGauche
gosub FlecheDroite
pcopy 2, page1
pcopy 2, page2
screenset page1, page2
return
