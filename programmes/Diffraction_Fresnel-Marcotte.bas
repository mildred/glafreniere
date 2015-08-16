cote = 800                                                'nombre de particules par côté.
DIM precedent(-1 TO cote+1)
DIM AS SINGLE potentiel1(-1 TO cote+1, -1 TO cote+1)
DIM AS SINGLE potentiel2(-1 TO cote+1, -1 TO cote+1)
DIM AS SINGLE potentiel3(-1 TO cote+1, -1 TO cote+1)
DIM AS SINGLE potHuygens(0 to 10 * cote)                  'principe de Huygens, sous-multiples..
DIM AS SINGLE potAdditionnel(-1 TO cote+1, -1 TO cote+1)  'pour ajout après un quart d'onde.
Dim As Single potAdditionnel2(-1 To cote+1, -1 To cote+1)
DIM AS SINGLE betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, pas, ton
DIM AS SINGLE amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance
DIM AS SINGLE xCoord, distNormale, periode, distance, rotation, amplitude, phi
DIM AS SINGLE ondelette, facteur, rapport, longueur, luminosite, contraste
DIM AS SINGLE pi, angle, lambda, xCarre, yCarre, arcSinus, xDistance, affaiblissement, inverser
DIM AS SINGLE Selection, GainA, GainB, GainC, SommeA, SommeB, SommeC

DIM AS SINGLE Generateur, GenValeur


'                      MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.
'______________________________________________________________________________________________
'                             CALCUL SELON M. JOCELYN MARCOTTE                               
'
'
'  Sélectionner l'équation à utiliser en fonction de la valeurs de la variable Selection :
'
'    - Le programme est stable seulement pour des valeurs de Selection comprisent
'      entre 0.25 et 0.5 inclusivement.
'
'
'  Valeur intéressante pour Selection :
'
'         Selection = 1/4          -> GainA = -1      (granule courrant)
'                                     GainB =  1 / 2  (granules sur les axes)
'                                     GainC =  1 / 4  (granules sur les diagonales)
'                                     Les gains sont tous des puissances de 2 !
'                                     Il y a donc moins d'erreur d'arrondis dans les calculs
'
'         Selection = 1/(2+sqr(2)) -> GainA = -0.8284 (granule courrant)
'                                     GainB =  0.4142 (granules sur les axes)
'                                     GainC =  0.2929 (granules sur les diagonales)
'                                     Le ratio GainB / GainC = 0.4142 / 0.2929 = 1.4142 = sqr(2) !
'                                     Le gain est donc inversement proportionnel à la distance qui
'                                     sépare les granules du granule central
'
'         Selection = 1/3          -> GainA = -2 / 3  (granule courrant)
'                                     GainB =  1 / 3  (granules sur les axes)
'                                     GainC =  1 / 3  (granules sur les diagonales)
'                                     Les GainA et GainB sont égaux !
'                                     Les 8 granules voisins ont un gain identique
'
'         Selection = 1/2          -> GainA = 0      (granule courrant)
'                                     GainB = 0      (granules sur les axes)
'                                     GainC = 1/2  (granules sur les diagonales)
'                                     Seule les diagonales ont un effet dans les calculs !
'

Selection = 0.25
'Selection = 1/(2+sqr(2))
'Selection = 1/3
'Selection = 1/2

GainA = 4*Selection-2    ' Gain appliqué à la valeur précédente du granule courant.
GainB = 1-2*Selection    ' Gain appliqué à la valeur précédente des 4 granules voisins adjacent (sur les axes).
GainC = (Selection)      ' Gain appliqué à la valeur précédente des 4 granules voisins sur les diagonales.


' Les gains suivants de A, B et C correspondent à l'algorithme de Ether08x
'GainA = 0
'GainB = 1/2
'GainC = 0


SCREEN 19,24,3: GOSUB Initialisation


Largeur = 150
Mur1 = cote / 2 - Largeur / 2
Mur2 = cote / 2 + Largeur / 2

Relief = 1


DO'                      MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.
'______________________________________________________________________________________________
'                             CALCUL SELON M. JOCELYN MARCOTTE                               


  GenValeur = 2 * sin(Generateur)
  FOR y = Mur1+1  TO Mur2-1
    potentiel1( -1, y) = GenValeur
  NEXT
  Generateur = Generateur + 2 * pi / (lambda / 2)         'phase selon 2 pi et lambda / 2.


  FOR x = -1 TO cote + 1                                  'mémoriser les deux derniers états
    FOR y = -1 to cote  + 1                               'du potentiel.
      potentiel3(x,y) = potentiel2(x,y)
      potentiel2(x,y) = potentiel1(x,y)
    NEXT
  NEXT


' Faire une réflexion mole sur les 2 murs
'  FOR x = 0 TO cote*2 / 3
'    potentiel2(x, Mur1  ) = potentiel2(x, Mur1-2)
'    potentiel2(x, Mur1+1) = potentiel2(x, Mur1+3)
'    potentiel2(x, Mur2  ) = potentiel2(x, Mur2-2)
'    potentiel2(x, Mur2+1) = potentiel2(x, Mur2+3)
'  NEXT
'  potentiel2(cote*2 / 3 - 1, Mur1) = potentiel2(cote*2 / 3 + 1, Mur1)
'  potentiel2(cote*2 / 3 - 1, Mur2) = potentiel2(cote*2 / 3 + 1, Mur2)
  
  
  FOR x = 0 TO cote                                       ' Calculer le nouvel état du potentiel
    FOR y = 0 TO cote
          SommeA = potentiel2(x,  y  )
          SommeB = potentiel2(x,  y-1) + potentiel2(x,  y+1) + potentiel2(x+1,y  ) + potentiel2(x-1,y  )
          SommeC = potentiel2(x-1,y-1) + potentiel2(x+1,y+1) + potentiel2(x+1,y-1) + potentiel2(x-1,y+1)
          Potentiel1(x,y) = SommeA*GainA + SommeB*GainB + SommeC*GainC - potentiel3(x,y)
    NEXT
  NEXT



' Faire une réflexion mole tout le tour des 2 murs
  FOR x = 0 TO cote*1/12
    potentiel1(x, Mur1-1) = Potentiel2(x, Mur1-1)*GainA + (2*Potentiel2(x, Mur1-2) + Potentiel2(x-1, Mur1-1) + Potentiel2(x+1, Mur1-1))*GainB + (Potentiel2(x-1, Mur1-2) + Potentiel2(x+1, Mur1-2))*GainC*2 - Potentiel3(x, Mur1-1)
    potentiel1(x, Mur1  ) = 5
    potentiel1(x, Mur1+1) = Potentiel2(x, Mur1+1)*GainA + (2*Potentiel2(x, Mur1+2) + Potentiel2(x-1, Mur1+1) + Potentiel2(x+1, Mur1+1))*GainB + (Potentiel2(x-1, Mur1+2) + Potentiel2(x+1, Mur1+2))*GainC*2 - Potentiel3(x, Mur1+1)
    potentiel1(x, Mur2-1) = Potentiel2(x, Mur2-1)*GainA + (2*Potentiel2(x, Mur2-2) + Potentiel2(x-1, Mur2-1) + Potentiel2(x+1, Mur2-1))*GainB + (Potentiel2(x-1, Mur2-2) + Potentiel2(x+1, Mur2-2))*GainC*2 - Potentiel3(x, Mur2-1)
    potentiel1(x, Mur2  ) = 5
    potentiel1(x, Mur2+1) = Potentiel2(x, Mur2+1)*GainA + (2*Potentiel2(x, Mur2+2) + Potentiel2(x-1, Mur2+1) + Potentiel2(x+1, Mur2+1))*GainB + (Potentiel2(x-1, Mur2+2) + Potentiel2(x+1, Mur2+2))*GainC*2 - Potentiel3(x, Mur2+1)
  NEXT
  potentiel1(cote*1/12,Mur1) = 0
  potentiel1(cote*1/12,Mur2) = 0
'  x = cote*2 / 3 + 1
'  potentiel1(x, Mur1-1) = Potentiel2(x, Mur1-1)*GainA + (  Potentiel2(x+1, Mur1-1) + Potentiel2(x-1, Mur1-1) + Potentiel2(x, Mur1-2) + Potentiel2(x, Mur1  ))*GainB + (Potentiel2(x-1, Mur1-2) + 2*Potentiel2(x+1, Mur1-2) + Potentiel2(x+1, Mur1  )                          )*GainC - Potentiel3(x, Mur1-1)
'  potentiel1(x, Mur1  ) = Potentiel2(x, Mur1  )*GainA + (2*Potentiel2(x+1, Mur1  ) +                           Potentiel2(x, Mur1-1) + Potentiel2(x, Mur1+1))*GainB + (Potentiel2(x-1, Mur1-1) +   Potentiel2(x-1, Mur1+1) + Potentiel2(x+1, Mur1-1) + Potentiel2(x+1, Mur1+1))*GainC - Potentiel3(x, Mur1  )
'  potentiel1(x, Mur1+1) = Potentiel2(x, Mur1+1)*GainA + (  Potentiel2(x+1, Mur1+1) + Potentiel2(x-1, Mur1+1) + Potentiel2(x, Mur1+2) + Potentiel2(x, Mur1  ))*GainB + (Potentiel2(x-1, Mur1+2) + 2*Potentiel2(x+1, Mur1+2) + Potentiel2(x+1, Mur1  )                          )*GainC - Potentiel3(x, Mur1+1)
'  potentiel1(x, Mur2-1) = Potentiel2(x, Mur2-1)*GainA + (  Potentiel2(x+1, Mur2-1) + Potentiel2(x-1, Mur2-1) + Potentiel2(x, Mur2-2) + Potentiel2(x, Mur2  ))*GainB + (Potentiel2(x-1, Mur2-2) + 2*Potentiel2(x+1, Mur2-2) + Potentiel2(x+1, Mur2  )                          )*GainC - Potentiel3(x, Mur2-1)
'  potentiel1(x, Mur2  ) = Potentiel2(x, Mur2  )*GainA + (2*Potentiel2(x+1, Mur2  ) +                           Potentiel2(x, Mur2-1) + Potentiel2(x, Mur2+1))*GainB + (Potentiel2(x-1, Mur2-1) +   Potentiel2(x-1, Mur2+1) + Potentiel2(x+1, Mur2-1) + Potentiel2(x+1, Mur2+1))*GainC - Potentiel3(x, Mur2  )
'  potentiel1(x, Mur2+1) = Potentiel2(x, Mur2+1)*GainA + (  Potentiel2(x+1, Mur2+1) + Potentiel2(x-1, Mur2+1) + Potentiel2(x, Mur2+2) + Potentiel2(x, Mur2  ))*GainB + (Potentiel2(x-1, Mur2+2) + 2*Potentiel2(x+1, Mur2+2) + Potentiel2(x+1, Mur2  )                          )*GainC - Potentiel3(x, Mur2+1)


  reflexion = 0                                           'sélection du mode : pas de réflexion
  If reflexion = 2 Then                                   'réflexion "molle".
    For x = 0 To cote
      Potentiel1(x, -1)     = Potentiel1(x, 0)*GainB    + (Potentiel1(x-1, 0)    + Potentiel1(x+1, 0))*GainC
      Potentiel1(x, cote+1) = Potentiel1(x, cote)*GainB + (Potentiel1(x-1, cote) + Potentiel1(x+1, cote))*GainC
    Next
    For y = -1 To cote +1
      Potentiel1(-1, y)     = Potentiel1(0, y)*GainB    + (Potentiel1(0, y-1)    + Potentiel1(0, y+1))*GainC
      Potentiel1(cote+1, y) = Potentiel1(cote, y)*GainB + (Potentiel1(cote, y-1) + Potentiel1(cote, y+1))*GainC
    Next
  Elseif reflexion = 0 Then                               'pas de réflexion.
    For x = 0 To cote
      Potentiel1(x, -1)     = Potentiel2(x, 0)*GainB    + (Potentiel2(x-1, 0)    + Potentiel2(x+1, 0))*GainC
      Potentiel1(x, cote+1) = Potentiel2(x, cote)*GainB + (Potentiel2(x-1, cote) + Potentiel2(x+1, cote))*GainC
    Next
    For y = 0 To cote
      Potentiel1(-1, y)     = Potentiel2(0, y)*GainB    + (Potentiel2(0, y-1)    + Potentiel2(0, y+1))*GainC
      Potentiel1(cote+1, y) = Potentiel2(cote, y)*GainB + (Potentiel2(cote, y-1) + Potentiel2(cote, y+1))*GainC
    Next
  End If                                                  'réflexion dure par défaut.

'______________________________________________________________________________________________


  IF clic = 0 THEN GETMOUSE xSouris, ySouris, , clic      'vérifier entre les affichages.
  IF afficher = 0 OR clic > 0 THEN                        'afficher une fois sur deux.
    SWAP page1, page2
    SCREENSET page1, page2
    PCOPY 2, page1
    GOSUB afficherRelief'------------- DIAGRAMME PRINCIPAL -----------------------------------

'--------------------------------------- SAISIE CLAVIER --------------------------------------
    saisie$ = Inkey
    IF LEN(Saisie$) THEN
      If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
      bitmap = 0
      SELECT CASE Saisie$
        CASE "I": luminosite = 40
                  contraste = 3.1
                  demiOnde = 1
                  IF lambda <> 20  THEN lambda = 20: GOSUB Huygens
                  relief = lambda / 10
        CASE "R": GOSUB Inverser
        CASE "S": GOSUB EffacerMemoire
        CASE "=+": relief = relief - 1: IF relief < 0 THEN relief = 0   'F3.
        CASE ">+": relief = relief + 1: IF relief > 10 THEN relief = 10 'F4.
        CASE "?+": demiOnde = 0                                         'F5.
        CASE "@+": demiOnde = 1                                         'F6.
        CASE "A+": reflexion = 0                                        'F7.
        CASE "B+": reflexion = 1                                        'F8.
        CASE "k+",CHR$(27): END
        CASE "M": RUN "Ether00.exe"
        CASE "K+":RUN "Ether07.exe"                       'flèche gauche.
        CASE "M+":RUN "Ether09.exe"                       'flèche droite.
        CASE "+": luminosite = luminosite + 40: IF luminosite > 120 THEN luminosite = 120
                  IF luminosite = 120 THEN contraste = 1.05 ELSE contraste = 1.58
        CASE "-": luminosite = luminosite - 40: IF luminosite < 40 THEN luminosite = 40
                  IF luminosite = 80 THEN contraste = 1.58 ELSE contraste = 3.1
        CASE "1": lambda = 10: GOSUB Huygens
        CASE "2": lambda = 20: GOSUB Huygens
        CASE "3": lambda = 30: GOSUB Huygens
        CASE "4": lambda = 40: GOSUB Huygens
        CASE "5": lambda = 50: GOSUB Huygens
        CASE "6": lambda = 60: GOSUB Huygens
        CASE "7": lambda = 70: GOSUB Huygens
        CASE "8": lambda = 80: GOSUB Huygens
        CASE "9": lambda = 90: GOSUB Huygens
      END SELECT
      GOSUB MiseAjour
    END IF
'---------------------------------------- SAISIE SOURIS --------------------------------------

    IF clic = 0 THEN GETMOUSE xSouris, ySouris, , clic    'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
    IF ligne > 26 AND ligne < 38 THEN
      IF xSouris < 304 OR xSouris > 496 THEN ligne = 0
    ELSE ligne = 0  
    END IF
    
    IF clic = 2 AND ySouris < cote / 2 THEN GOSUB ToutEffacer

'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
    COLOR noir, turquoise
    LOCATE ligne, 39
    SELECT CASE ligne
      CASE 27: PRINT ligne27$
      CASE 28: PRINT ligne28$
      CASE 29: PRINT ligne29$
      CASE 30: PRINT ligne30$
      CASE 31: PRINT ligne31$
      CASE 32: PRINT ligne32$
      CASE 33: PRINT ligne33$
      CASE 34: PRINT ligne34$
      CASE 35: PRINT ligne35$
      CASE 36: PRINT ligne36$
      CASE 37: PRINT ligne37$;: IF xSouris < 400 THEN GOSUB flecheGauche ELSE GOSUB flecheDroite
    END SELECT
    COLOR noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
    IF clic = 1 THEN
      bitmap = 0
      SELECT CASE ligne
        CASE 27: GOSUB ToutEffacer
        CASE 28: GOSUB Inverser
        CASE 29: GOSUB EffacerMemoire
        CASE 35: IF lambda <> 40  THEN lambda = 40: GOSUB Huygens
        CASE 36: END
        CASE 37: IF xSouris < 400 THEN RUN "Ether07.exe" ELSE RUN "Ether09.exe"
      END SELECT
    END IF
    IF bitmap THEN GOSUB Bitmaps                          'capture d'images si désiré.
'    locate 34, 65: print using "#.## sec"; timer - temps
'    temps = timer
  END IF
  clic = 0
  afficher = afficher + 1
  IF afficher = 2 THEN afficher = 0                       'afficher une fois sur deux
LOOP

afficherRelief:'---------------------- AFFICHER EN RELIEF ------------------------------------
haut = 10                                                 'doit être pair re: espaces vides.
bas = cote
FOR x = gauche TO droite
  FOR y = haut TO bas                                     'step 2 pour aplatir en ellipses.
    luminance = luminosite * (potentiel1(x, y) + contraste)
    IF luminance > 255 THEN luminance = 255
    IF luminance < 0 THEN luminance = 0
    yCoord = y / 2 - relief * potentiel1(x, y)                     'décaler selon l'amplitude.

 '   IF x < cote / 3 AND y > cote / 2 - Largeur - 2 AND y < cote / 2 - Largeur + 2 THEN
 '     PSET (x - gauche, yCoord), RGB(0, 200, 0)
 '   ELSEIF x < cote / 3 AND y > cote / 2 + Largeur - 2 AND y < cote / 2 + Largeur + 2 THEN
 '     PSET (x - gauche, yCoord), RGB(0, 200, 0)
 '   ELSE
      PSET (x - gauche, yCoord), RGB(luminance,luminance,luminance)
 '   ENDIF

    IF y > haut THEN ecart = yCoord - yCoordPrec ELSE ecart = 1
    IF ecart > 1  THEN
      luminance2 = luminosite * (potentiel1(x, y - 1) + contraste) 'estomper avec le pixel précédent.
      pas = (luminance2 - luminance) / ecart
      ton = luminance                                     'ton de gris au départ.
      FOR j = 1 TO ecart - 1                              'combler les espaces vides.
        ton = ton + pas
        IF ton > 255 THEN ton = 255
        IF ton < 0 THEN ton = 0

'        IF x < cote / 3 AND y > cote / 2 - Largeur - 2 AND y < cote / 2 - Largeur + 2 THEN
'          PSET (x - gauche, yCoord - j), RGB(0, 200, 0)
'        ELSEIF x < cote / 3 AND y > cote / 2 + Largeur - 2 AND y < cote / 2 + Largeur + 2 THEN
'          PSET (x - gauche, yCoord - j), RGB(0, 200, 0)
'        ELSE
          PSET(x - gauche, yCoord - j), RGB(ton, ton, ton)
'        ENDIF

      NEXT
    END IF        
    yCoordPrec = yCoord
  NEXT
NEXT
'if bitmap then else line(0, 398)-(cote,405), fond, BF    'niveler le bas(vérifier).
RETURN

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
SELECT CASE capture
  CASE IS < 10: numero$ = "00"
  CASE IS < 100: numero$ = "0"
  CASE IS < 1000: numero$ = ""
END SELECT
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
COLOR RGB(255,255,255), RGB(255,0,0)                      'signaler la capture d'images.
LOCATE 34, 43: PRINT fichier$
bsave fichier$,0
COLOR noir, fond
capture = capture + 1
IF capture > 500 THEN END 
RETURN

EffacerMemoire:'--------- EFFACER LES VARIABLES MÉMOIRE: MODE STATIONNAIRE -------------------
FOR x = 0 TO cote
  FOR y = 0 TO cote
    SommeA = potentiel1(x,y)
    SommeB = potentiel1(x+1,y)   + potentiel1(x-1,y)   + potentiel1(x,y+1)   + potentiel1(x,y-1)
    SommeC = potentiel1(x+1,y+1) + potentiel1(x-1,y+1) + potentiel1(x+1,y-1) + potentiel1(x-1,y-1)
    potentiel2(x,y) = (SommeA*GainA + SommeB*GainB + SommeC*GainC)/(GainA + 4*GainB + 4*GainC)
  NEXT
NEXT
GOSUB MiseAjour
RETURN

'------------------------------------ DESSIN DES FLÈCHES -------------------------------------
flecheDroite:
LINE (xdd-8,yFleche-4)-(xdd-6,yFleche-4),noir
LINE (xdd-8,yFleche-3)-(xdd-4,yFleche-3),noir
LINE (xdd-8,yFleche-2)-(xdd-2,yFleche-2),noir
LINE (xdg,yFleche-1)-(xdd,yFleche-1),noir
LINE (xdg,yFleche)-(xdd+2,yFleche),noir
LINE (xdg,yFleche+1)-(xdd,yFleche+1),noir
LINE (xdd-8,yFleche+2)-(xdd-2,yFleche+2),noir
LINE (xdd-8,yFleche+3)-(xdd-4,yFleche+3),noir
LINE (xdd-8,yFleche+4)-(xdd-6,yFleche+4),noir
RETURN
flecheGauche:
LINE (xgg+6,yFleche-4)-(xgg+8,yFleche-4),noir
LINE (xgg+4,yFleche-3)-(xgg+8,yFleche-3),noir
LINE (xgg+2,yFleche-2)-(xgg+8,yFleche-2),noir
LINE (xgg,yFleche-1)-(xgd,yFleche-1),noir
LINE (xgg-2,yFleche)-(xgd,yFleche),noir
LINE (xgg,yFleche+1)-(xgd,yFleche+1),noir
LINE (xgg+2,yFleche+2)-(xgg+8,yFleche+2),noir
LINE (xgg+4,yFleche+3)-(xgg+8,yFleche+3),noir
LINE (xgg+6,yFleche+4)-(xgg+8,yFleche+4),noir
RETURN


Huygens:'-------------- DISTRIBUTION DU POTENTIEL SELON LE PRINCIPE DE HUYGENS ---------------
GOSUB ToutEffacer
GOSUB MiseAjour
RETURN

'#############################################################################################
Initialisation:'------------------------ INITIALISATION --------------------------------------
'#############################################################################################
fond = RGB(225,225,225)
blanc= RGB(255,255,255)
gris = RGB(150,150,150)
bleu = RGB(0,0,255)
rouge = RGB(255,0,0)
vert = RGB(0,150,0)
turquoise = RGB (230, 255, 255)
pi = 4 * ATN(1)
ondelette = pi / 100                                      'cent ondelettes est un minimum.
'bitmap = 1                                               'séquence bitmap si désiré.
gauche = 0
droite = cote
page2 = 1                                                 'page1 correspond à 0 au départ.
lambda = 20
relief = 1'lambda / 10
luminosite = 40
contraste = 3.1
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
SCREENSET 2, 2                                            'créer une page matrice.
COLOR noir, fond: CLS
LOCATE 6, 10:  PRINT "L'‚ther virtuel est une invention de M. Philippe Delmotte."
LOCATE 15, 10: PRINT "R‚partition de l'amplitude en cours..."
LOCATE 21,5: PRINT "Bonjour Jocelyn."
LOCATE , 5:  PRINT "Croyez-vous qu'il serait possible d'‚liminer totalement les r‚flexions ?"
LOCATE , 5:  PRINT "Si oui, votre algorithme me serait trŠs utile."
LOCATE , 54:  PRINT "Gabriel."

LOCATE 27


ligne27$ = "  E - Tout effacer.      ": LOCATE 27, 39: PRINT ligne27$
ligne28$ = "  R - Inverser le sens.  ": LOCATE 28, 39: PRINT ligne28$
ligne29$ = "  S - Mode stationnaire. ": LOCATE 29, 39: PRINT ligne29$
'ligne30$ = "  C - Cercle complet.    ": LOCATE 30, 39: PRINT ligne30$
'ligne31$ = "  D - Demi-cercle.       ": LOCATE 31, 39: PRINT ligne31$
'ligne32$ = "  H - HuitiŠme de cercle.": LOCATE 32, 39: PRINT ligne32$
'ligne33$ = "  L - Ondes planes.      ": LOCATE 33, 39: PRINT ligne33$
'ligne34$ = "  P - SystŠme rotatif.   ": LOCATE 34, 39: PRINT ligne34$
ligne35$ = "  I - Initialiser.       ": LOCATE 35, 39: PRINT ligne35$
ligne36$ = "      Quitter (Echap).   ": LOCATE 36, 39: PRINT ligne36$
ligne37$ = "                         "

LOCATE 28
LOCATE, 65: PRINT "L'‚ther virtuel permet d'observer  "
LOCATE, 65: PRINT "tous les ph‚nomŠnes ondulatoires, y"
LOCATE, 65: PRINT "compris la figure d'Airy et la dif-"
LOCATE, 65: PRINT "fraction de Fresnel."

GOSUB flecheGauche: GOSUB flecheDroite
COLOR blanc, rouge
LINE(548,411)-(744,430),blanc,B
LINE(549,412)-(745,431),blanc,B
LINE(550,413)-(745,431),rouge,BF
LOCATE 27, 71: PRINT "LE PRINCIPE DE HUYGENS"
LOCATE 33: COLOR vert, fond
LOCATE , 2: PRINT "Ce programme est l'oeuvre de M. Jocelyn Marcotte. Il a ‚t‚ ‚labor‚ … partir du programme Ether08,"
LOCATE , 2: PRINT "dont il ne conserve que la structure. "
print
LOCATE , 2: PRINT "Je le remercie chaleureusement."
LOCATE , 2: PRINT "Gabriel LaFreniŠre  glafreniere.com";
LOCATE 35
LOCATE , 65: PRINT "Le 24 mars 2006. Ce programme"
LOCATE , 65: PRINT "FreeBASIC peut ˆtre distribu‚, "
LOCATE , 65: PRINT "copi‚ ou modifi‚ librement.";
COLOR noir
GOSUB MiseAjour
PCOPY 2, 0
PCOPY 2, 1
RETURN
'#############################################################################################


Inverser:'------------------------ INVERSER LE SENS DES ONDES --------------------------------
FOR x = -1 TO cote + 1
  FOR y = -1 TO cote + 1
    inverser = potentiel1(x,y)
    potentiel1(x,y) = potentiel2(x,y)
    potentiel2(x,y) = inverser
  NEXT
NEXT
GOSUB MiseAjour
RETURN

MiseAjour:'------------------------------- MISE A JOUR ---------------------------------------
DO
  GETMOUSE xSouris, ySouris, , clic                       'attendre le relâchement du bouton.
LOOP WHILE clic > 0                                       'éviter le piège: clic = -1 !
DO: vider$ = inkey: LOOP WHILE LEN(vider$)                'vider le tampon.
clic = 0                                                  'éviter les actions à répétition.
SCREENSET 2                                               'page cachée servant de matrice.
LOCATE 30, 2
  PRINT "Contraste: appuyez sur [+] ou [-]: ";
IF luminosite = 40 THEN
  PRINT "1"
ELSEIF luminosite = 80 THEN PRINT "2"
ELSE PRINT "3"
END IF
LOCATE 29, 2
PRINT "Lambda:"; lambda; " pixels (chiffre de 1 … 9)."
LOCATE 31, 2
  PRINT "Amplitude du relief..... F3 ou F4:"; relief;" "
SCREENSET page1
RETURN

'---------------------------- ONDES RÉPARTIES SUR UNE LIGNE DROITE ---------------------------
OndePlane:
PCOPY 2, page1
PCOPY 2, page2
GOSUB ToutEffacer
longueur = .5 * cote                                      'longueur approximative.
nombre = longueur / lambda + 1                            'une ondelette par longueur d'onde.
longueur = lambda * (nombre - 1)                          'entier, longueur exacte.
IF demiOnde THEN nombre = nombre * 2'                     'deux ondelettes par longueur d'onde.
facteur = 3 / SQR(nombre)'                                'ajuste l'amplitude selon le nombre.
xPixel = 0
IF demiOnde THEN
haut = (cote - ((nombre + 1) * (lambda / 2))) / 2
ELSE
haut = (cote - ((nombre + 1) * lambda)) / 2
END IF


FOR j = 1 TO nombre
  IF demiOnde THEN
  yPixel = haut + j * (lambda / 2)
  ELSE
  yPixel = haut + j * lambda
  END IF

  FOR x = -1 TO cote
    x2 = xPixel - x + cote / 2
    IF x2 < 0 OR x2 > cote THEN x2 = -1
    FOR y = -1 TO cote
      y2 = yPixel - y + cote / 2
      IF y2 < 0 OR y2 > cote THEN y2 = -1
      potentiel1(x, y) = potentiel1(x, y) + facteur * potAdditionnel(x2, y2)    'ajout d'un nouveau potentiel.
      potentiel2(x, y) = potentiel2(x, y) + facteur * potAdditionnel2(x2, y2)   'ajout d'un nouveau potentiel.
    NEXT
  NEXT
NEXT
reflexion = 1
GOSUB MiseAjour
RETURN

ToutEffacer:'---------------------- INITIALISER LES VARIABLES --------------------------------
FOR x = -1 TO cote + 1
  FOR y = -1 TO cote + 1
    potentiel1(x, y) = .000000001                         '0 exactement ralentit le calcul(?).
    potentiel2(x, y) = .000000001
    potentiel3(x, y) = .000000001
  NEXT
NEXT
nombre = 0
GOSUB MiseAJour
RETURN
