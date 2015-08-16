
#lang "fblite" 
Option Gosub 
largeur = 1380: hauteur = 560                              'nombre de particules par c�t�.
Const fond = Rgb(225,225,225)                                   'd�finir les couleurs.
Const blanc= Rgb(255,255,255)
Const gris = Rgb(75,75,75)
Const turquoise = Rgb (230, 255, 255)
Const pi = 4 * Atn(1)
Dim As Single influence(-1 To largeur+1, -1 To hauteur+1) 'cr�� le 20 nov. 2006.  MODIFI� 16 oct. 2009.
Dim As Single sinus(-1 To largeur+1, -1 To hauteur+1)
Dim As Single cosinus(-1 To largeur+1, -1 To hauteur+1)
Dim As Single amortissement(-1 To largeur+1, -1 To hauteur+1)
Dim As Single facteur, rapport, luminosite, contraste, pas, petitPas, gLorentz, xScan
Dim As Single amplitudeSinus, amplitudeCosinus, c, xOrig, yOrig, xDepart, yDepart
Dim As Single xPrime, yPrime, tPrime, temps, beta, theta, ixe, igrec, longueur
Dim As Single xCoord, yCoord, periode, rotation, amplitude, rayon, petitRayon, radian
Dim As Single angle, lambda, xCarre, yCarre, distance, xDistance, yDistance, phi
beta = .5: choix$ = "E"
Screen 21,24,3: page2 = 1: : Gosub Initialisation

Do'                       MOD�LISATION DE L'�THER EN DEUX DIMENSIONS.

  For y = 0 To hauteur - 1' un cran � l'avance pour avoir acc�s aux valeurs pr�c�dentes.
    influence(0,y) = sinus(-1,y) + sinus(1,y) + sinus(0,y-1) + sinus(0,y+1) - 4 * sinus(0,y)
  Next

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

  For x = 0 To largeur - 1: For y = 0 To hauteur - 1

'------------- CALCUL SELON LA M�THODE D'EULER SIMPLIFI�E PAR M. ANSELME DEWAVRIN -------------
'l'amortissement est facultatif et permet d'�liminer les r�flexions sur les bords.

    influence(x+1,y) = sinus(x,y) + sinus(x+2,y) + sinus(x+1,y-1) + sinus(x+1,y+1) - 4 * sinus(x+1,y)'un cran � l'avance.
    cosinus(x,y) = cosinus(x,y) + .5 * influence(x,y)
    sinus(x,y) = (sinus(x,y) + cosinus(x,y)) * amortissement(x,y)
  Next: Next

  Select Case choix$'                                     'r�partition des t�ches.
    Case "A": Gosub ChoixA
    Case "B": Gosub ChoixB
    Case "C": Gosub ChoixC
    Case "D": Gosub ChoixD
    Case "E": Gosub ChoixE
  End Select

  Getmouse xSouris, ySouris, , clic                       'v�rifier entre les affichages.
  If afficher Then
    Gosub AfficherCouleurs'---------- DIAGRAMME PRINCIPAL -------------------------------------
    afficher = 0                                          'afficher une fois sur deux.
    If bitmap Then Gosub Bitmaps                          'capture d'images si d�sir�.
    Swap page1, page2
    Screenset page1, page2
    Pcopy 2, page1
    Else afficher = 1
  End If

'------------------------------------ �COULEMENT DU TEMPS -------------------------------------
  temps = temps + c / lambda          'le temps t de Lorentz exprim� en p�riode d'onde.
                                      'c = .707 est la vitesse des ondes en pixels par passage.

'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = Inkey
  If Len(saisie$) Then
    bitmap = 0
    If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
    If beta <> 0 And saisie$ = "S" Then saisie$ = "T"     'texte diff�rent si beta = 0.
    If scanner Then
      Select Case saisie$
      Case "P"                                            'pause accept�e.
      Case "+","-","H+","P+": saisie$ = ""                '�viter si le scanner est actif.
      Case Else                                 'toute autre intervention d�sactive le scanner.
        scanner = 0: Screenset 2, page2: Locate 37, 2 
        Print "S - Activer le Scanner du Temps.      ";
        If saisie$ = "S" Or saisie$ = "T" Then saisie$ = ""  
      End Select
    End If
    Select Case saisie$
      Case "A","B","C","D","E": choix$ = saisie$
      Case "X+", "k+",Chr$(27): End                       'le � X � varie selon le FBIde(!?)
      Case Chr(13): saisie$ = ""
                If beta > 0 Then Gosub ReculerGauche
                If beta < 0 Then Gosub ReculerDroite
      Case "I": beta = -.5: choix$ = "B"                   'initialiser.
      Case "V": beta = -beta                              'vitesse n�gative ou positive.
      Case "P": Sleep: saisie$ = ""                       'pause.
      Case "F": saisie$ = "":                             'afficher peu d'images.
                If espacer Then
                  espacer = 0
                  Screenset 2, page2: Locate 36, 14: Print "F - Acc�l�rer le calcul. "
                Else
                  espacer = 20
                  Screenset 2, page2: Locate 36, 14: Print "F - Afficher normalement."
                End If
      Case "S": saisie$ = ""                              'Scanner du Temps; beta = 0.
                scanner = 1: espacer = 0: xScan = gauche + marge / 2
                Screenset 2, page2: Locate 37, 2: Print "S - D�sactiver le Scanner du Temps.   ";
                Line(0,0)-(largeur-100, hauteur-100),noir, bf
                Color fond, noir: Locate 3
                Locate,57:?"Appuyez sur le P (pause)"
                Locate,57:?"pour lire ce texte.":?
                Locate,57:?"Ici, la vitesse du syst�me ondulatoire est"
                Locate,57:?"nulle. Dans ce cas, le Scanner du Temps"
                Locate,57:?"le reproduit tel qu'il apparara�trait"
                Locate,57:?"s'il se d�pla�ait � la moiti� de la"
                Locate,57:?"vitesse de la lumi�re, soit "; Chr(225); " = 0,5.":?
                Locate,57:?"Lorsque ce texte aura disparu, vous"
                Locate,57:?"pourrez comparer en appuyant sur le 5.":?
                Locate,57:?"Si la vitesse n'est pas nulle, vous pouvez"
                Locate,57:?"�galement appuyer sur le S pour v�rifier"
                Locate,57:?"la loi de l'addition des vitesse selon"
                Locate,57:?"Henri Poincar�. Le Scanner a alors pour"
                Locate,57:?"effet d'acc�l�rer le syst�me d'une valeur"
                Locate,57:?"RELATIVE de 0,5 c, soit selon cette loi.":?
                Locate,57:?"Si la vitesse est n�gative, le Scanner a"
                Locate,57:?"pour effet de ralentir le syst�me: "; Chr(225); " < -5;"
                Locate,57:?"de l'IMMOBILISER: "; Chr(225); " = -5; ou m�me de"
                Locate,57:?"l'acc�l�rer dans l'autre sens: "; Chr(225); " > -5."
                Color noir, fond
      Case "T": saisie$ = ""                              'Scanner du Temps, vitesse non nulle.
                scanner = 1: espacer = 0: xScan = gauche + marge / 2
                Screenset 2, page2: Locate 37, 2: Print "S - D�sactiver le Scanner du Temps.   ";
                Line(0,0)-(largeur-100, hauteur-100),noir, bf
                Color fond, noir: Locate 1
                Locate,57:?"Appuyez sur le P (pause)"
                Locate,57:?"pour lire ce texte.":?
                Locate,57:?"Le Scanner ne reproduit que la partie"
                Locate,57:?"gauche de cette fen�tre. Le syst�me doit"
                Locate,57:?"donc s'y trouver (appuyez sur Entr�e au"
                Locate,57:?"besoin, ou attendez qu'il y soit parvenu).":?
                Locate,57:?"Ici, la vitesse du syst�me n'est pas nulle."
                Locate,57:?"Le Scanner du Temps peut montrer l'aspect"
                Locate,57:?"qu'il aurait s'il �tait acc�l�r� d'une"
                Locate,57:?"valeur de 0,5 c selon la loi de l'addition"
                Locate,57:?"des vitesses de Henri Poincar�. La vitesse"
                Locate,57:?"RELATIVE finale n'atteint jamais celle de"
                Locate,57:?"la lumi�re quelle que soit la situation.":?
                Locate,57:?"Vitesse actuelle: beta_1 = ";: Print Using "#.# c"; beta
                Locate,57:?"    Acc�l�ration: beta_2 = ";: Print "0.5 c"
                Locate,57:?"        Vitesse relative = ";: Print Using "#.### c"; (beta + .5) / (1 + beta * .5):? 
                Locate,57:?"L'�quation de Poincar� se lit comme suit:"
                Locate,57:?"Vitesse relative ="
                Locate,57:?"(beta_1 + beta_2) / (1 + beta_1 * beta_2)":?
                Locate,57:?"Si la vitesse est n�gative, le Scanner a"
                Locate,57:?"pour effet de ralentir le syst�me: "; Chr(225); " < -0,5"
                Locate,57:?"ou de l'IMMOBILISER: "; Chr(225); " = -0,5. Il peut m�me"
                Locate,57:?"l'acc�l�rer dans l'autre sens: "; Chr(225); " > -0,5."
                Color noir, fond
      Case "M": Run "Ether00.exe"
      Case "K+":Run "Ether17.exe"                         'fl�che gauche.
      Case "M+":Run "Ether19.exe"                         'fl�che droite.
      Case "0": beta = 0
      Case "1": beta = .1
      Case "2": beta = .2
      Case "3": beta = .3
      Case "4": beta = .4
      Case "5": beta = .5
      Case "6": beta = .6
      Case "7": beta = .7
      Case "8": beta = .8
'     case "9": beta = .9'                                'conditions difficiles, � �viter.
      Case "+": luminosite = luminosite * 1.1: saisie$ = ""'non modifiable si le scanner est actif.
      Case "-": luminosite = luminosite / 1.1: saisie$ = ""
      Case "H+":contraste = contraste * 1.02
                luminosite = luminosite / 1.15
                saisie$ = ""
      Case "P+":contraste = contraste / 1.02
                luminosite = luminosite * 1.15
                saisie$ = ""
      Case "Y": bitmap = 1: saisie$ = ""
      Case "Z": bitmap = 0: saisie$ = ""
      Case Else:saisie$ = ""
    End Select
    Do: Loop While Len(Inkey)                             'vider le tampon.
    If Len(saisie$) Then Gosub Initialisation
  End If
'----------------------------------------- SAISIE SOURIS --------------------------------------

  If clic = 0 Then Getmouse xSouris, ySouris, , clic      'v�rifier une 2e fois au besoin.
  ligne = .5 + ySouris / 16
  If ligne > 26 And ligne < 38 Then
    If xSouris < 320 Then ligne = 0
    If ligne > 34 And xSouris > 480 Then ligne = 0
  Else ligne = 0
  End If

'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  Color noir, turquoise
  Locate ligne, 41
  Select Case ligne
    Case 30: If Not choix$ = "A" Then Print ligne30$      'choix en cours d�j� affich� en bleu.
    Case 31: If Not choix$ = "B" Then Print ligne31$
    Case 32: If Not choix$ = "C" Then Print ligne32$
    Case 33: If Not choix$ = "D" Then Print ligne33$
    Case 34: If Not choix$ = "E" Then Print ligne34$      
    Case 35: Print ligne35$      
    Case 36: Print ligne36$
    Case 37: Print ligne37$;: If xSouris < 400 Then Gosub FlecheGauche Else Gosub FlecheDroite
  End Select
  Color noir, fond
'-------------------------------------- ACTIONS SUITE � UN CLIC -------------------------------
  If clic = 1 Then
    clic = 0: bitmap = 0: afficher = 0: espacer = 0
    scanner = 0: Screenset 2, page2: Locate 37, 2: Print "S - Activer le Scanner du Temps.      ";
    Select Case ligne
      Case 30: choix$ = "A": Gosub Initialisation
      Case 31: choix$ = "B": Gosub Initialisation
      Case 32: choix$ = "C": Gosub Initialisation
      Case 33: choix$ = "D": Gosub Initialisation
      Case 34: choix$ = "E": Gosub Initialisation
      Case 35: beta = -.5: choix$ = "B": Gosub Initialisation
      Case 36: End
      Case 37: If xSouris < 400 Then Run "Ether17.exe" Else Run "Ether19.exe"
    End Select
  End If
Loop

AfficherCouleurs:'****************** AFFICHER EN COULEURS *************************************

For x = marge/2 To largeur                                'masquer 50 pixels (100 amortis).
  If Abs(x - xScan) < 1 Then scan = 1 Else scan = 0       'activer le scanner si x = xScan
  For y = marge/2 To hauteur - marge/2
    luminance = luminosite * Abs(sinus(x, y)) ^ contraste 'afficher en rouge et vert. Ajouter
    If sinus(x, y) < 0 Then                               'du bleu car les couleurs doivent
      vert = .67 * luminance ' 0,67 + 0,33 = 1            '�tre compl�mentaires le plus possi-
      bleu = .33 * luminance ' 50% du vert.               'ble pour �viter une dominante jaune.
      If vert > 255 Then rouge = .67 * luminance - 255 Else rouge = 0
    Else
      rouge = .75 * luminance ' 0,75 + 0,25 = 1           'proportion de bleu moindre
      bleu  = .25 * luminance ' 33% du rouge.             'pour �viter le fushia.
      If rouge > 255 Then vert = .75 * luminance - 255 Else vert = 0
      If rouge > 255 Then bleu = bleu + .33 * (.75 * luminance - 255)'ajouter 33% du vert.
    End If
    If rouge > 255 Then rouge = 255 Else If rouge < 0 Then rouge = 0
    If vert > 255 Then vert = 255 Else If vert < 0 Then vert = 0
    If bleu > 255 Then bleu = 255 Else If bleu < 0 Then bleu = 0
    
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

    If scanner Then                                       '428 pixels � gauche, 370 � droite.
      If x-marge/2 < gauche Then Pset (x-marge/2, y-marge/2), Rgb(rouge,vert,bleu)'moiti� non transform�e � gauche.
      If scan Then                                        'afficher la moiti� transform�e � droite.
        Screenset 2,page2                                 'afficher sur la page matrice.
        Pset ((x-marge/2)*.866+gauche, y-marge/2), Rgb(rouge,vert,bleu)'scan avec contraction 0,866.
        Screenset page1, page2
      End If
    Else If Not Point(x-marge/2, y-marge/2) = blanc Then Pset (x-marge/2, y-marge/2), Rgb(rouge,vert,bleu)'image compl�te normale sans scanner.
    End If
  Next
Next
If scanner Then                                 'd�placer le scanner s'il est actif.
  xScan = xScan - c /.5                         'vitesse de balayage = c / beta avec beta = .5
  If xScan < marge / 2 Then xScan = marge / 2   'stopper � gauche de l'�cran. 
  Line(xScan-marge/2, 0)-(xScan-marge/2, hauteur - marge), fond'marquer l'emplacement du scanner.
  Line(gauche, 0)-(gauche, hauteur - marge), fond        's�parer la fen�tre en deux parties.
  Line((xScan-marge/2)*.866+gauche+1, 0)-((xScan-marge/2)*.866+gauche+1, hauteur - marge), fond'� imprimante �.
End If
Return

Bitmaps:'-------------------------- Cr�er une s�quence bitmap ---------------------------------
Select Case capture
  Case Is < 10: numero$ = "00"
  Case Is < 100: numero$ = "0"
  Case Is < 1000: numero$ = ""
End Select
fichier$ = "capture" + numero$ + Str(capture) + ".bmp"
Color Rgb(255,255,255), Rgb(255,0,0)                      'signaler la capture d'images.
Locate 32, 2: Print fichier$
Bsave fichier$,0
Color noir, fond
capture = capture + 1
If capture > 5000 Then End                                'pr�caution.
Return

ChoixA:'*********************** DEUX SOURCES SUR DES PARALL�LES *******************************

'  Dans le cas d'une source unique, il est avantageux de g�n�rer
'  les ondelettes sur un petit cercle pour �viter les art�facts.
'  Le point de d�part (x, y) est fixe. Les transformations de Lorentz ne sont
'  utilis�es que lors des impulsions dans la proc�dure � Impulsion � plus bas.

For radian = pas / 2 To 2 * pi Step pas
  xCoord = xDepart + .1875 * lambda * Sin(radian)                  'source 1, en pixels.
  yCoord = yDepart - 3 * lambda + .1875 * lambda * Cos(radian) 
  Gosub Impulsion 
Next
For radian = pas / 2 To 2 * pi Step pas
  xCoord = xDepart + .1875 * lambda * Sin(radian)                  'source 2, en pixels.
  yCoord = yDepart + 3 * lambda + .1875 * lambda * Cos(radian) 
  Gosub Impulsion 
Next
Return

ChoixB:'************* NOMBREUSES SOURCES R�PARTIES SUR UNE CIRCONF�RENCE **********************

For radian = pas / 2 To 2 * pi Step pas                   'cercle parfait.
  xCoord = xDepart + rayon * lambda * Cos(radian)         'en pixels.
  yCoord = yDepart + rayon * lambda * Sin(radian)         'en pixels.
  Gosub Impulsion
Next
Return

ChoixC:'******* PULSATION D'UN ARC DE CERCLE SELON LES TRANSFORMATIONS DE LORENTZ *************

For radian = pi / 2.7 To pi - pi / 2.7 Step pas           'arc de cercle.
  xCoord = xDepart + rayon * lambda * Cos(radian)         'x en pixels.
  yCoord = yDepart + rayon * lambda * Sin(radian)         'y en pixels.
  Gosub Impulsion   
Next
Return
xPrime = xDepart / lambda * Cos(theta) + temps * Sin(theta)'xPrime selon Lorentz.
'if afficher = 0 then circle(xPrime * lambda-marge/2, yDepart-marge/2), rayon * lambda, blanc,,, 1 / gLorentz 'ellipse mobile.

ChoixD:'******************* SOURCES ALIGN�ES SUR UNE DROITE HORIZONTALE ***********************

For xCoord = xDepart To xDepart + longueur Step pas       'x en pixels.
  Gosub Impulsion
Next
Return

ChoixE:'******************** SOURCES ALIGN�ES SUR UNE DROITE VERTICALE ************************

xCoord = xDepart                                          'en longueurs d'onde.
For yCoord = yDepart To yDepart + longueur Step pas       'y en pixels.
  Gosub Impulsion
Next
Return

'------------------------------------- DESSIN DES FL�CHES -------------------------------------
FlecheDroite:
Line (xdd-8,yFleche-4)-(xdd-6,yFleche-4),noir
Line (xdd-8,yFleche-3)-(xdd-4,yFleche-3),noir
Line (xdd-8,yFleche-2)-(xdd-2,yFleche-2),noir
Line (xdg,yFleche-1)-(xdd,yFleche-1),noir
Line (xdg,yFleche)-(xdd+2,yFleche),noir
Line (xdg,yFleche+1)-(xdd,yFleche+1),noir
Line (xdd-8,yFleche+2)-(xdd-2,yFleche+2),noir
Line (xdd-8,yFleche+3)-(xdd-4,yFleche+3),noir
Line (xdd-8,yFleche+4)-(xdd-6,yFleche+4),noir
Return
FlecheGauche:
Line (xgg+6,yFleche-4)-(xgg+8,yFleche-4),noir
Line (xgg+4,yFleche-3)-(xgg+8,yFleche-3),noir
Line (xgg+2,yFleche-2)-(xgg+8,yFleche-2),noir
Line (xgg,yFleche-1)-(xgd,yFleche-1),noir
Line (xgg-2,yFleche)-(xgd,yFleche),noir
Line (xgg,yFleche+1)-(xgd,yFleche+1),noir
Line (xgg+2,yFleche+2)-(xgg+8,yFleche+2),noir
Line (xgg+4,yFleche+3)-(xgg+8,yFleche+3),noir
Line (xgg+6,yFleche+4)-(xgg+8,yFleche+4),noir
Return

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
xPrime = xOrig * Cos(theta) + temps * Sin(theta)          ' x et x' en longueurs d'onde.
tPrime = temps * Cos(theta) - xOrig * Sin(theta)          ' t et t' en p�riodes d'onde.
yPrime = yCoord                                           ' selon Lorentz, on a y'= y
                                                          ' (ici en pixels pour le raccourci).

'---------------------------- MAINTENIR L'ENSEMBLE DANS L'�CRAN -------------------------------
If tPrime < 0 Then Return                                 'tPrime respecte l'onde de phase.
If xPrime * lambda > largeur - marge / 2 Then
  Gosub ReculerGauche
Elseif xPrime * lambda < marge / 2 Then'                  'Le Scanner doit continuer son
  If scanner Then                                         'travail co�te que co�te. Il faut
    If xPrime * lambda < marge/4 Then Gosub ReculerDroite 'donc lui accorder un sursis malgr�
  Else Gosub ReculerDroite'                               'l'affaiblissement sur les bords.
  End If
End If
'-------------------------- IMPULSION SELON UN CARR� DE NEUF PIXELS ---------------------------

cosinus(xPrime*lambda,  yPrime)   = cosinus(xPrime*lambda,  yPrime)  +4*Cos(2*pi*tPrime)
cosinus(xPrime*lambda+1,yPrime)   = cosinus(xPrime*lambda+1,yPrime)  +2*Cos(2*pi*tPrime)
cosinus(xPrime*lambda-1,yPrime)   = cosinus(xPrime*lambda-1,yPrime)  +2*Cos(2*pi*tPrime)
cosinus(xPrime*lambda,  yPrime+1) = cosinus(xPrime*lambda,  yPrime+1)+2*Cos(2*pi*tPrime)
cosinus(xPrime*lambda,  yPrime-1) = cosinus(xPrime*lambda,  yPrime-1)+2*Cos(2*pi*tPrime)
cosinus(xPrime*lambda-1,yPrime-1) = cosinus(xPrime*lambda-1,yPrime-1) + Cos(2*pi*tPrime)
cosinus(xPrime*lambda+1,yPrime-1) = cosinus(xPrime*lambda+1,yPrime-1) + Cos(2*pi*tPrime)
cosinus(xPrime*lambda+1,yPrime+1) = cosinus(xPrime*lambda+1,yPrime+1) + Cos(2*pi*tPrime)
cosinus(xPrime*lambda-1,yPrime+1) = cosinus(xPrime*lambda-1,yPrime+1) + Cos(2*pi*tPrime)
Pset(xPrime*lambda-marge/2,  yPrime-marge/2), blanc
Return

ReculerDroite:'-------------- D�PORTER L'ENSEMBLE VERS LA DROITE ------------------------------

' xPrime = xOrig * cos(theta) + temps * sin(theta)        '�quations de Lorentz invers�es.
' tPrime = temps * cos(theta) - xOrig * sin(theta)
' xOrig = (xPrime - temps * sin(theta)) / cos(theta)      'avec permutation de x, x' et t, t'.
' temps = (tPrime + xOrig * sin(theta)) / cos(theta)

xCible = largeur - 2 * marge - rayon * lambda * gLorentz  'cible, � droite de l'�cran.
If choix$ = "C" Then xCible = largeur - marge - .4 * rayon * lambda * gLorentz'arc de cercle incomplet.
xOrig = xDepart / lambda                                  'ancienne origine en longueurs d'onde.
xPrime = xOrig * Cos(theta) + temps * Sin(theta)          'emplacement actuel du syst�me.
deportation = xCible - xPrime * lambda                    'distance de la cible.
If deportation < 1 Then Return                            'le syst�me est encore � droite.
tPrime = temps * Cos(theta) - xOrig * Sin(theta)          'temps t' actuel � conserver.
xDepart = xDepart + deportation * gLorentz                'reculer le point de d�part.
xOrig = xDepart / lambda                                  'origine x en longueurs d'onde.
temps = (tPrime + xOrig * Sin(theta)) / Cos(theta)        'temps en cours selon x et t'.

For x = largeur - deportation To -1 Step -1: For y = -1 To hauteur+1'd�porter � droite.
      sinus(x + deportation,y) = sinus(x, y)
    cosinus(x + deportation,y) = cosinus(x, y)
Next:Next
For x = -1 To deportation: For y = -1 To hauteur + 1      'nouveau m�dium sans �nergie.
      sinus(x,y) = .00000001                              'non nul, acc�l�re le calcul (!?).
    cosinus(x,y) = .00000001
Next:Next
Return

ReculerGauche:'-------------- D�PORTER L'ENSEMBLE VERS LA GAUCHE ------------------------------

xCible = marge + rayon * lambda * gLorentz                'cible, � gauche de l'�cran.
If choix$ = "C" Then xCible = marge + .4 * rayon * lambda * gLorentz'arc de cercle incomplet.
xOrig = xDepart / lambda                                  'ancienne origine en longueurs d'onde.
xPrime = xOrig * Cos(theta) + temps * Sin(theta)          'emplacement actuel du syst�me.
deportation = xPrime * lambda - xCible                    'distance de la cible.
If deportation < 1 Then Return                            'le syst�me est encore � gauche.
tPrime = temps * Cos(theta) - xOrig * Sin(theta)          'temps t' actuel � conserver.
xDepart = xDepart - deportation * gLorentz                'reculer le point de d�part.
xOrig = xDepart / lambda                                  'origine x en longueurs d'onde.
temps = (tPrime + xOrig * Sin(theta)) / Cos(theta)        'temps en cours selon x et t'.
For x = -1 To largeur-deportation: For y = -1 To hauteur+1'd�porter le m�dium.
      sinus(x, y) =   sinus(x+deportation, y)
    cosinus(x, y) = cosinus(x+deportation, y)
Next:Next
For x = largeur - deportation To largeur + 1: For y = -1 To hauteur + 1'nouveau m�dium sans �nergie.
      sinus(x, y) = .00000001                             'non nul, acc�l�re le calcul (!?).
    cosinus(x, y) = .00000001
Next:Next
Return

'----------------------------------------------------------------------------------------------
Initialisation:'------------------------ INITIALISATION ---------------------------------------
'----------------------------------------------------------------------------------------------
'bitmap = 1                                               's�quence bitmap si d�sir�.
theta = Asin(beta)
gLorentz = Sqr(1 - beta ^ 2) ' (ou gLorentz = cos(theta)  'facteur de contraction g de Lorentz.
c = Cos(pi / 4)                                           'vitesse de l'onde en pixels/passage.
contraste = 2
espacer = 0                                               'afficher peu d'images (plus rapide).
scanner = 0
rayon = 0
marge = 100                                               '50 pixels ne sont pas affich�s.
gauche = (largeur - marge) / (.866 + 1)                   'selon: gauche*gLorentz = largeurEcran-gauche = droite
petitPas = 2 * pi / 11                                    'nombre impair.
xCentre = largeur / 2
yCentre = hauteur / 2
xDepart = 50                                              'en pixels, favoriser la gauche.
'xDepart = xCentre
yDepart = yCentre                                         'en pixels.
' xOrig: abscisse x selon Lorentz pour calcul du temps t et t' (voir plus bas).
xgg = 400 - 50                                            'coordonn�es des fl�ches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
Screenset 2, 2                                            'cr�er une page matrice.
Color noir, fond: Cls
Line(0,0)-(largeur-100, hauteur-100),noir, bf
Windowtitle "L'optique du mouvement - 17 oct. 2009"
ligne30$ = "  A - Deux sources sur des parall�les (interf�rences).     ": Locate 30, 41: Print ligne30$
ligne31$ = "  B - Sources align�es sur un cercle (ondes stationnaires).": Locate 31, 41: Print ligne31$
ligne32$ = "  C - ... sur un arc de cercle (tache d'Airy en 2-D).      ": Locate 32, 41: Print ligne32$
ligne33$ = "  D - sur une droite horizontale (diffraction de Fresnel). ": Locate 33, 41: Print ligne33$
ligne34$ = "  E - sur une droite verticale (diffraction de Fresnel).   ": Locate 34, 41: Print ligne34$
ligne35$ = "  I - Initialiser.  ": Locate 35, 41: Print ligne35$
ligne36$ = "  Quitter (Echap).  ": Locate 36, 41: Print ligne36$
ligne37$ = "                    "
Locate 30
Locate, 2: Print "Vitesse: chiffre de 0 � 8. B�ta = ";      'directives.
           Print Using "#.#"; beta
Locate, 2: Print "Inverser le sens: appuyez sur le V.":?
Locate, 2: Print "Luminosit�: appuyez sur + ou -."
Locate, 2: Print "Contraste: fl�ches haut ou bas."
Locate, 2: Print "Reculer le syst�me: appuyez sur Entr�e."
Locate, 2: Print "P - Pause.  F - Acc�l�rer le calcul."
Locate, 2: Print "S - Activer le Scanner du Temps.      ";
Color blanc, noir
Locate 1, 2 : Print Using "Speed #.# c"; beta
Locate 29,2 : Print Using "Contraction .###"; gLorentz
Locate 1,85 : Print "glafreniere.com"
Locate 29,82: Print "Gabriel LaFreniere"
Color bleu, fond
'--------------------------- DISTRIBUTION (N.B. xDepart en pixels)-----------------------------
Select Case choix$
  Case "A": luminosite = 6: contraste = 1                 '---> DEUX SOURCES.
            lambda = 40: pas = 2 * pi / 49
            xOrig = xDepart / lambda                      'en longueurs d'onde.
            Locate 30, 41: Print ligne30$
  Case "B": luminosite = 2: contraste = 1                 '---> CERCLE.  
            rayon = 3.6875: lambda = 50'                   rayon: �mettre au centre de l'un
            pas = 2 * pi / 999'                            des ventres des ondes stationnaires,
            Locate 31, 41: Print ligne31$'                 soit 0,1875 plus n * 0,5.
            If beta < 0 Then xOrig = xDepart / lambda + rayon Else xOrig = xDepart / lambda - rayon
  Case "C": luminosite = .05: contraste = 2
            yDepart = 100: lambda = 24                    '---> ARC DE CERCLE.
            rayon = 16: pas = 2 * pi / 2000               'grand rayon (en longueurs d'onde).
            If beta < 0 Then xOrig = xDepart / lambda + .4 * rayon Else xOrig = xDepart / lambda - .4 * rayon
            Locate 32, 41: Print ligne32$
  Case "D": luminosite = .004: contraste = 3: lambda = 16 '---> DROITE HORIZONTALE.
            yCoord = hauteur - 70
            longueur = 10 * lambda: pas = lambda / 20
            If beta = 0 Then xDepart = xDepart - longueur / 2
            If beta < 0 Then xOrig = (xDepart + longueur) / lambda Else xOrig = xDepart / lambda
            Locate 33, 41: Print ligne33$
  Case "E": luminosite = .02: contraste = 3: lambda = 20  '---> DROITE VERTICALE.
            longueur = 10 * lambda: pas = lambda / 10
            yDepart = yCentre - longueur / 2
            xOrig = xDepart / lambda
            Locate 34, 41: Print ligne34$
End Select

' TRANSFORMATIONS DE LORENTZ, POUR R�F�RENCE:
' xPrime = xOrig * cos(theta) + temps * sin(theta)        '�quations de Lorentz invers�es.
' tPrime = temps * cos(theta) - xOrig * sin(theta)
' xOrig = (xPrime - temps * sin(theta)) / cos(theta)      '�quations normales avec
' temps = (tPrime + xOrig * sin(theta)) / cos(theta)      'permutation de x, x' et t, t'.

tPrime = 0                                                'initialiser le temps � t' � pour
temps = (tPrime + xOrig * Sin(theta)) / Cos(theta)        '�viter le ressac des ondes.
luminosite = luminosite * gLorentz                        'compenser �l'augmentation de masse�.

Locate 35: Color vert
Locate,62: Print "Ce programme FreeBASIC peut �tre"
Locate,62: Print "distribu�, copi� ou modifi� librement."
Locate,62: Print "Gabriel LaFreni�re  glafreniere.com";

For x = -1 To largeur + 1                                 'effacer.
  For y = -1 To hauteur + 1
    sinus(x, y) = .00000001                               'acc�l�re le calcul (!!?).
    cosinus(x, y) = .00000001
    amortissement(x,y) = 1
  Next
Next
'--------------------------------- M�MORISER L'AMORTISSEMENT ----------------------------------
racine = marge / 5
For x = -1 To marge
  angle = (pi / 2) * x / marge
  For y = -1 To hauteur+1
    amortissement(x,y) = Sin(angle) ^ (1 / racine)
    amortissement(largeur-x,y) = amortissement(x,y)
    If y < x Then                                         'les coins.
      amortissement(x,y) = Sin((pi / 2) * y / marge) ^ (1 / racine)
      amortissement(largeur-x,y) = amortissement(x,y)
    End If
    If x > hauteur-y Then
      amortissement(x,y) = Sin((pi / 2) * (hauteur-y) / marge) ^ (1 / racine)
      amortissement(largeur-x,y) = amortissement(x,y)
    End If
  Next
Next
For x = marge To largeur - marge                          'le centre, haut et bas.
  For y = -1 To 100
    amortissement(x,y) = Sin((pi / 2) * y / marge) ^ (1 / racine)
    amortissement(x,hauteur-y) = amortissement(x,y)
  Next
Next
Gosub FlecheGauche
Gosub FlecheDroite
Pcopy 2, page1
Pcopy 2, page2
Screenset page1, page2
Return
