cote = 800                                               'nombre de particules par côté.
Dim As Single M(-1 To cote+1, -1 To cote+1)
Dim As Single I(-1 To cote+1, -1 To cote+1): Dim precedent(-1 To cote+1)
Dim As Single P(-1 To cote+1, -1 To cote+1), potentiel2(-1 To cote+1, -1 To cote+1)
Dim As Single pi, angle, lambda, xCarre, yCarre, arcSinus, xDistance, affaiblissement
Dim As Single xCoord, distNormale, periode, distance, rotation, amplitude, phi
Dim As Single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance
Dim As Single betaSinusAngle, deuxPiDistanceSurLambda, temps, beta, pas, ton
Dim As Single ondelette, facteur, rapport, longueur, luminosite, contraste, relief
Screen 19,24,3: Gosub Initialisation

Do'    MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS, 14 Jan. 2005.

  For y = 0 To cote - 1
    I(0,y) = (P(-1,y) + P(1,y) + P(0,y-1) + P(0,y+1)) / 4 - P(0,y)' un cran à l'avance.
  Next

  For x = 0 To cote - 1
    For y = 0 To cote - 1
'------------------------------ CALCUL SELON LA LOI DE HOOKE ------------------------
      I(x+1,y) = (P(x,y) + P(x+2,y) + P(x+1,y-1) + P(x+1,y+1)) / 4 - P(x+1,y)' I => Influence
      P(x, y) = P(x, y) + I(x, y) + M(x, y)                                  ' P => Potentiel
      M(x, y) = M(x, y) + I(x, y)                                            ' M => Mémoire
'------------------------------------- FIN DU CALCUL --------------------------------
    Next
  Next

  For x = 0 To cote - 1                                   'pas de réflexion, haut et bas.
    P(x, -1) = P(x, 0) - 2 * M(x, 0): P(x, cote) = P(x, cote - 1) - 2 * M(x, cote - 1)
  Next
  
  If reflexion Then
    For y = 0 To cote - 1'                                'réflexion gauche et droite.
      P(-1, y) = P(0, y): P(cote, y) = P(cote - 1, y)
    Next
  Else
    For y = 0 To cote - 1                                 'pas de réflexion.
      P(-1, y) = P(0, y) - 2 * M(0, y): P(cote, y) = P(cote - 1, y) - 2 * M(cote - 1, y)
    Next
  End If
  
  If clic = 0 Then Getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  If afficher = 0 Or clic > 0 Then                        'afficher une fois sur deux.
    Swap page1, page2
    Screenset page1, page2
    Pcopy 2, page1
    Gosub afficherRelief'------------- DIAGRAMME PRINCIPAL -----------------------------------

'--------------------------------------- SAISIE CLAVIER --------------------------------------
  saisie$ = Inkey
  If Len(saisie$) = 2 and Right(saisie$, 1) = "k" then end
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
    If Len(saisie$) Then
      bitmap = 0
      If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+"': locate 34,2: print saisie$
      Select Case saisie$
        Case "I": luminosite = 40
                  contraste = 3.1
                  demiOnde = 1
                  If lambda <> 40  Then lambda = 40: Gosub Huygens
                  relief = lambda / 10
                  Gosub HuitiemeDeCercle
        Case "R": Gosub Inverser
        Case "S": Gosub EffacerMemoire
        Case "C": Gosub Cercle
        Case "D": Gosub DemiCercle
        Case "E": Gosub ToutEffacer
        Case "H": Gosub HuitiemeDeCercle
        Case "L": Gosub OndePlane
        Case "P": Gosub Rotation1
        Case "=+": relief = relief - 1: If relief < 0 Then relief = 0   'F3.
        Case ">+": relief = relief + 1: If relief > 10 Then relief = 10 'F4.
        Case "?+": demiOnde = 0                                         'F5.
        Case "@+": demiOnde = 1                                         'F6.
        Case "A+": reflexion = 0                                        'F7.
        Case "B+": reflexion = 1                                        'F8.
        Case "k+",Chr$(27): End
        Case "M": Run "Ether00.exe"
        Case "K+":Run "Ether07.exe"                       'flèche gauche.
        Case "M+":Run "Ether09.exe"                       'flèche droite.
        Case "+": luminosite = luminosite + 40: If luminosite > 120 Then luminosite = 120
                  If luminosite = 120 Then contraste = 1.05 Else contraste = 1.58
        Case "-": luminosite = luminosite - 40: If luminosite < 40 Then luminosite = 40
                  If luminosite = 80 Then contraste = 1.58 Else contraste = 3.1
        Case "1": lambda = 10: Gosub Huygens
        Case "2": lambda = 20: Gosub Huygens
        Case "3": lambda = 30: Gosub Huygens
        Case "4": lambda = 40: Gosub Huygens
        Case "5": lambda = 50: Gosub Huygens
        Case "6": lambda = 60: Gosub Huygens
        Case "7": lambda = 70: Gosub Huygens
        Case "8": lambda = 80: Gosub Huygens
        Case "9": lambda = 90: Gosub Huygens
      End Select
      Gosub MiseAjour
    End If
'---------------------------------------- SAISIE SOURIS --------------------------------------

    If clic = 0 Then Getmouse xSouris, ySouris, , clic    'vérifier une dernière fois.
    ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
    If ligne > 26 And ligne < 38 Then
      If xSouris < 304 Or xSouris > 496 Then ligne = 0
    Else ligne = 0  
    End If
    
    If clic = 1 And ySouris < cote / 2 Then Gosub RondsDansLeau' clic sur l'écran.
    If clic = 2 And ySouris < cote / 2 Then Gosub ToutEffacer

'------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
    Color noir, turquoise
    Locate ligne, 39
    Select Case ligne
      Case 27: Print ligne27$
      Case 28: Print ligne28$
      Case 29: Print ligne29$
      Case 30: Print ligne30$
      Case 31: Print ligne31$
      Case 32: Print ligne32$
      Case 33: Print ligne33$
      Case 34: Print ligne34$
      Case 35: Print ligne35$
      Case 36: Print ligne36$
      Case 37: Print ligne37$;: If xSouris < 400 Then Gosub flecheGauche Else Gosub flecheDroite
    End Select
    Color noir, fond
'------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
    If clic = 1 Then
      bitmap = 0
      Select Case ligne
        Case 27: Gosub ToutEffacer
        Case 28: Gosub Inverser
        Case 29: Gosub EffacerMemoire
        Case 30: Gosub Cercle
        Case 31: Gosub DemiCercle
        Case 32: Gosub HuitiemeDeCercle
        Case 33: Gosub OndePlane
        Case 34: Gosub Rotation1
        Case 35: If lambda <> 40  Then
                    lambda = 40: Gosub Huygens
                 Else Gosub Cercle
                 End If 
        Case 36: End
        Case 37: If xSouris < 400 Then Run "Ether07.exe" Else Run "Ether09.exe"
      End Select
    End If
    If bitmap Then Gosub Bitmaps                          'capture d'images si désiré.
    Locate 34, 65: Print Using "#.## sec"; Timer - temps
    temps = Timer
  End If
  clic = 0
  afficher = afficher + 1
  If afficher = 2 Then afficher = 0                       'afficher une fois sur deux
  If passage Then                                         'rotation, ajouter deux positrons.
    passage = passage - 1
    If passage = 0 Then Gosub Rotation2
  End If
Loop

afficherRelief:'---------------------- AFFICHER EN RELIEF ------------------------------------
haut = 10                                                 'doit être pair re: espaces vides.
bas = cote
For x = gauche To droite
  For y = haut To bas Step 2                              'step 2 pour aplatir en ellipses.
    luminance = luminosite * (P(x, y) + contraste)
    If luminance > 255 Then luminance = 255
    If luminance < 0 Then luminance = 0
    yCoord = y / 2 - relief * P(x, y)                     'décaler selon l'amplitude.
    Pset (x - gauche, yCoord), Rgb(luminance,luminance,luminance)
    If y > haut Then ecart = yCoord - yCoordPrec Else ecart = 1
    If ecart > 1  Then
      luminance2 = luminosite * (P(x, y - 2) + contraste) 'estomper avec le pixel précédent.
      pas = (luminance2 - luminance) / ecart
      ton = luminance                                     'ton de gris au départ.
      For j = 1 To ecart - 1                              'combler les espaces vides.
        ton = ton + pas
        If ton > 255 Then ton = 255
        If ton < 0 Then ton = 0
        Pset(x - gauche, yCoord - j), Rgb(ton, ton, ton)
      Next
    End If        
    yCoordPrec = yCoord
  Next
Next
'if bitmap then else line(0, 398)-(cote,405), fond, BF    'niveler le bas(vérifier).
Return

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
Select Case capture
  Case Is < 10: numero$ = "00"
  Case Is < 100: numero$ = "0"
  Case Is < 1000: numero$ = ""
End Select
fichier$ = "capture" + numero$ + Str(capture) + ".bmp"
Color Rgb(255,255,255), Rgb(255,0,0)                      'signaler la capture d'images.
Locate 34, 43: Print fichier$
Bsave fichier$,0
Color noir, fond
capture = capture + 1
If capture > 500 Then End 
Return

'---------------------------- ONDES RÉPARTIES SUR UNE CIRCONFÉRENCE --------------------------
Cercle:
Pcopy 2, page1
Pcopy 2, page2
Gosub ToutEffacer
reflexion = 0
rayon = .667 * (cote / 2 )                                'appriximatif, à préciser.
nombre = 2 * pi * rayon / lambda                          'une ondelette par longueur d'onde.
circonference = nombre * lambda                           'nombre entier multiple de lambda.
If demiOnde Then nombre = nombre * 2'                     'deux ondelettes par longueur d'onde.
rayon = circonference / (2 * pi)                          'rayon exact.
facteur = 3 / Sqr(nombre)'                                'ajuste l'amplitude selon le nombre.

For j = 1 To nombre
  angle = 2 * pi * j / nombre
  xPixel = cote / 2 + rayon * Sin(angle)
  yPixel = cote / 2 + rayon * Cos(angle)
  For x = -1 To cote
    x2 = xPixel - x + cote / 2
    If x2 < 0 Or x2 > cote Then x2 = -1
    For y = -1 To cote
      y2 = yPixel - y + cote / 2
      If y2 < 0 Or y2 > cote Then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    Next
  Next
Next
Gosub MiseAjour
Return

'---------------------------- ONDES RÉPARTIES SUR UN DEMI-CERCLE -----------------------------
DemiCercle:
Pcopy 2, page1
Pcopy 2, page2
Gosub ToutEffacer
reflexion = 0
rayon = .667 * (cote / 2 )                                'appriximatif, à préciser.
nombre = 2 * pi * rayon / lambda                          'nombre d'ondelettes, circonférence.
circonference = nombre * lambda                           'nombre entier multiple de lambda.
If demiOnde = 0 Then nombre = nombre / 2                  'une ondelette par longueur d'onde.
rayon = circonference / (2 * pi)                          'rayon exact.
facteur = 3 / Sqr(nombre)'                                'ajuste l'amplitude selon le nombre.

For j = 1 To nombre                                       'même nombre que pour un cercle.
  angle = pi + pi * j / nombre                            'pi et non 2 * pi pour demi-cercle.
  xPixel = cote / 2 + rayon * Sin(angle)
  yPixel = cote / 2 + rayon * Cos(angle)
  For x = -1 To cote
    x2 = xPixel - x + cote / 2
    If x2 < 0 Or x2 > cote Then x2 = -1
    For y = -1 To cote
      y2 = yPixel - y + cote / 2
      If y2 < 0 Or y2 > cote Then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    Next
  Next
Next
Gosub MiseAjour
Return

EffacerMemoire:'--------- EFFACER LES VARIABLES MÉMOIRE: MODE STATIONNAIRE -------------------
For x = -1 To cote
  For y = -1 To cote
    M(x, y) = 0
  Next
Next
Gosub MiseAjour
Return

'------------------------------------ DESSIN DES FLÈCHES -------------------------------------
flecheDroite:
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
flecheGauche:
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

'------------------------ ONDES RÉPARTIES SUR UN ARC DE CERCLE -------------------------------
HuitiemeDeCercle:
Pcopy 2, page1
Pcopy 2, page2
Gosub ToutEffacer
reflexion = 0
rayon = .75 * cote                                        'rayon approximatif provisoire.
nombre = 2 * pi * rayon / lambda                          'une ondelette par longueur d'onde.
circonference = nombre * lambda                           'nombre entier multiple de lambda.
rayon = circonference / (2 * pi)                          'rayon exact.
longueur = circonference / 8                              'longueur de l'arc de cercle.
If demiOnde Then nombre = nombre / 4 Else nombre = nombre / 8'nombre sur un 8e de cercle.
pas = pi / 4 / nombre
angle = pi + .375 * pi
facteur = 3 / Sqr(nombre)'                                'ajuste l'amplitude selon le nombre.

For j = 1 To nombre
  xPixel = .9 * cote + rayon * Sin(angle)
  yPixel = .5 * cote + rayon * Cos(angle)
  For x = -1 To cote
    x2 = xPixel - x + cote / 2
    If x2 < 0 Or x2 > cote Then x2 = -1
    For y = -1 To cote
      y2 = yPixel - y + cote / 2
      If y2 < 0 Or y2 > cote Then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    Next
  Next
  angle = angle + pas
Next
Gosub MiseAjour
Return

Huygens:'-------------- DISTRIBUTION DU POTENTIEL SELON LE PRINCIPE DE HUYGENS ---------------
Gosub ToutEffacer
nombre = 1
relief = lambda / 10                                      'facteur d'amplitude du relief.
Gosub MiseAjour
Pcopy 2, 0
Screenset 0, 0
Locate 15, 10: Print "Performing calculus according to Huygens' Principle..."
Locate 17, 10: Print "Wavelength (lambda) now: "; lambda; " pixels." 
Locate 19, 10: Print "To select wavelength, press any number from 1 to 9 : lambda = 10.n" 

For x = .25 * cote To .75 * cote
  xDistance = x - cote / 2
  xCarre = xDistance * xDistance
  For y = .25 * cote To .75 * cote
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    distance = Sqr(xCarre + yCarre)
    amplitudeSinus = 0
    amplitudeCosinus = 0
    deuxPiDistanceSurLambda = 2 * pi * distance / lambda
    For angle = 0 To pi Step ondelette
        periode = deuxPiDistanceSurLambda * Cos(angle)
        amplitudeSinus = amplitudeSinus + Sin(periode)
        amplitudeCosinus = amplitudeCosinus + Cos(periode)
    Next
    amplitude = Sgn(amplitudeCosinus) * Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
    'if amplitudeCosinus < 0 then amplitude = -amplitude contourné par SGN.
    xCoord = distance / 100
    distNormale = pi ^ (-xCoord ^ 2)                      'distribution normale approx.
    P(x, y) = pi * ondelette * amplitude * distNormale    'distribution du potentiel.
    potentiel2(x, y) = P(x, y)
    If y = yCentre Then Pset (x-1 , yCentre / 2 - 4 * P(x, y)), 0
  Next

  saisie$ = Inkey
  If Len(saisie$) = 2 and Right(saisie$, 1) = "k" then end
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  If Len(saisie$) Then
    If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+"
    Select Case saisie$
      Case "k+", Chr$(27): End
      Case "M": Run "Ether00.exe"
      Case "K+": Run "Ether07.exe"                        'flèche gauche.
      Case "M+": Run "Ether09.exe"                        'flèche droite.        
    End Select
  End If
  Getmouse xSouris, ySouris, , clic
  ligne = .5 + ySouris / 16
  If clic = 1 And ligne = 37 Then
    If xSouris < 400 Then
      Run "Ether07.exe"
    Else
      Run "Ether09.exe"
    End If          
  End If
Next
temps = Timer
Return

'#############################################################################################
Initialisation:'------------------------ INITIALISATION --------------------------------------
'#############################################################################################
fond = Rgb(225,225,225)
blanc= Rgb(255,255,255)
gris = Rgb(150,150,150)
bleu = Rgb(0,0,255)
rouge = Rgb(255,0,0)
vert = Rgb(0,150,0)
turquoise = Rgb (230, 255, 255)
pi = 4 * Atn(1)
ondelette = pi / 100                                      'cent ondelettes est un minimum.
'bitmap = 1                                               'séquence bitmap si désiré.
gauche = 0
droite = cote
page2 = 1                                                 'page1 correspond à 0 au départ.
lambda = 40
fraction = 8
luminosite = 40
contraste = 3.1
demiOnde = 1
yCentre = cote / 2
largeur = 600                                             'largeur de la partie visible.
hauteur = 400                                             'hauteur de la partie visible.
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
Screenset 2, 2                                            'créer une page matrice.
Color noir, fond: Cls


Locate 6, 10:  Print "The Virtual Aether was created by Mr. Philippe Delmotte, from France."
Locate 15, 10: Print "Performing amplitude distribution..."
Locate 34, 75: Print "Lambda ="; lambda; " pixels."
Locate 27
Locate, 2:   Print "Click on screen to start a new wave. "
Locate, 2:   Print "Right click to clear the screen.     "
Locate, 2:   Print "Select lambda: press a number (x10). "
Locate, 2:   Print "Contrast: press [+] or [-] keys:   1 "
Locate, 2:   Print "3-D effect range: F3 or F4:   "; relief;"     "
Locate, 2: If demiOnde Then 
             Print "Full wavelength between wavelets: F5."
           Else
             Print "Half wavelength between wavelets: F6."
           End If
Locate, 2: If reflexion Then 
             Print "Cancel left and right reflection: F7."
           Else
             Print "Left and right reflection: F8.        "
           End If

ligne27$ = "  E - Clear all.         ": Locate 27, 39: Print ligne27$
ligne28$ = "  R - Reverse direction. ": Locate 28, 39: Print ligne28$
ligne29$ = "  S - Standing waves.    ": Locate 29, 39: Print ligne29$
ligne30$ = "  C - Full circle.       ": Locate 30, 39: Print ligne30$
ligne31$ = "  D - Half circle.       ": Locate 31, 39: Print ligne31$
ligne32$ = "  H - 1/8 of a circle.   ": Locate 32, 39: Print ligne32$
ligne33$ = "  L - Plane wave.        ": Locate 33, 39: Print ligne33$
ligne34$ = "  P - Rotating system.   ": Locate 34, 39: Print ligne34$
ligne35$ = "  I - Initialize all.    ": Locate 35, 39: Print ligne35$
ligne36$ = "      Press Esc to quit. ": Locate 36, 39: Print ligne36$
ligne37$ = "                         "

Locate 28
Locate, 65: Print "Very few Huygens' wavelets along a "
Locate, 65: Print "curve or straight line and each of "
Locate, 65: Print "them a half-wavelength spaced will "
Locate, 65: Print "produce a new wavefront, and even  "
Locate, 65: Print "a figure related to the Airy disk. "

Gosub flecheGauche: Gosub flecheDroite
Color blanc, rouge
Line(549,412)-(745,431),blanc,B
Line(550,413)-(745,431),rouge,BF
Locate 27, 73: Print "HUYGENS' PRINCIPLE"
Color vert, fond
Locate 35
Locate , 2:  Print "Special thanks to FreeBASIC creators"
Locate , 2:  Print "and to Mr. Anselme Dewavrin."
Locate , 2:  Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate , 65: Print "Jan 2006. This FreeBASIC program"
Locate , 65: Print "can be freely distributed, copied"
Locate , 65: Print "or modified.";



Color noir
Pcopy 2, 0
Pcopy 2, 1
Gosub Huygens
Gosub HuitiemeDeCercle
Return
'#############################################################################################


Inverser:'------------------------ INVERSER LE SENS DES ONDES --------------------------------
For x = -1 To cote
  For y = -1 To cote
    M(x, y) = -M(x, y)
   'P(x, y)= -P(x, y)                                     'alternative.
  Next
Next
Gosub MiseAjour
Return

MiseAjour:'------------------------------- MISE A JOUR ---------------------------------------
Do
  Getmouse xSouris, ySouris, , clic                       'attendre le relâchement du bouton.
Loop While clic > 0                                       'éviter le piège: clic = -1 !
Do: vider$ = Inkey: Loop While Len(vider$)                'vider le tampon.
clic = 0                                                  'éviter les actions à répétition.
Screenset 2                                               'page cachée servant de matrice.
Locate 30,37
If luminosite = 40 Then
  Print "1"
Elseif luminosite = 80 Then Print "2"
Else Print "3"
End If
Locate 31,32: Print relief; "  "
Locate 32, 2                                              'distinguer le choix possible.
If demiOnde Then  
  Print "Full wavelength between wavelets: F5."
Else
  Print "Half wavelength between wavelets: F6."
End If
Locate 33, 2
If reflexion Then 
  Print "Cancel left and right reflection: F7."
Else
  Print "Left and right reflection: F8.       "
End If
Locate 33, 65: If nombre Then Print "Number of wavelets:"; nombre; "  " Else Print "                      "
Locate 34, 75: Print "Lambda ="; lambda; " pixels."
Screenset page1
Return

'---------------------------- ONDES RÉPARTIES SUR UNE LIGNE DROITE ---------------------------
OndePlane:
Pcopy 2, page1
Pcopy 2, page2
Gosub ToutEffacer
longueur = .5 * cote                                      'longueur approximative.
nombre = longueur / lambda + 1                            'une ondelette par longueur d'onde.
longueur = lambda * (nombre - 1)                          'entier, longueur exacte.
If demiOnde Then nombre = nombre * 2'                     'deux ondelettes par longueur d'onde.
facteur = 3 / Sqr(nombre)'                                'ajuste l'amplitude selon le nombre.
xPixel = 0
If demiOnde Then
haut = (cote - ((nombre + 1) * (lambda / 2))) / 2
Else
haut = (cote - ((nombre + 1) * lambda)) / 2
End If


For j = 1 To nombre
  If demiOnde Then
  yPixel = haut + j * (lambda / 2)
  Else
  yPixel = haut + j * lambda
  End If

  For x = -1 To cote
    x2 = xPixel - x + cote / 2
    If x2 < 0 Or x2 > cote Then x2 = -1
    For y = -1 To cote
      y2 = yPixel - y + cote / 2
      If y2 < 0 Or y2 > cote Then y2 = -1
      P(x, y) = P(x, y) + facteur * potentiel2(x2, y2)    'ajout d'un nouveau potentiel.
    Next
  Next
Next
reflexion = 1                                             'sur les côtés seulement.
Gosub MiseAjour
Return

'-------------------------------- ONDE STATIONNAIRE CIRCULAIRE -------------------------------
RondsDansLeau:
If xSouris < 50 Then xSouris = 50
If ySouris < 50 Then ySouris = 50
If xSouris > cote - 50 Then xSouris = cote - 50
If ySouris > cote / 2 - 50 Then ySouris = cote / 2 - 50
ySouris = 2 * ySouris                                     'l'écran réel vaut le double.    
For x = -1 To cote
  x2 = xSouris - x + cote / 2
  If x2 < 0 Or x2 > cote Then x2 = -1
  For y = -1 To cote
    y2 = ySouris - y + cote / 2
    If y2 < 0 Or y2 > cote Then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2)                'ajout d'un nouveau potentiel.
  Next
Next
nombre = nombre + 1
Gosub MiseAjour
Return

Rotation1:'----------- ROTATION DE PHASE, MISE EN PLACE DE 2 ÉLECTRONS -----------------------
Gosub ToutEffacer
reflexion = 0
passage = lambda / 2
xSouris = cote / 2 - lambda / fraction                    'électron, spin -1/2
ySouris = cote / 2 - lambda / fraction
For x = -1 To cote
  x2 = xSouris - x + cote / 2
  If x2 < 0 Or x2 > cote Then x2 = -1
  For y = -1 To cote
    y2 = ySouris - y + cote / 2
    If y2 < 0 Or y2 > cote Then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2)                'ajout d'un nouveau potentiel.
  Next
Next
xSouris = cote / 2 + lambda / fraction                    'électron, spin +1/2
ySouris = cote / 2 + lambda / fraction
For x = -1 To cote
  x2 = xSouris - x + cote / 2
  If x2 < 0 Or x2 > cote Then x2 = -1
  For y = -1 To cote
    y2 = ySouris - y + cote / 2
    If y2 < 0 Or y2 > cote Then y2 = -1
    P(x, y) = P(x, y) - potentiel2(x2, y2)                'potentiel en opposition de phase.
  Next
Next
nombre = 4
Gosub MiseAjour
Return

Rotation2:'-------------- ROTATION DE PHASE, AJOUT DE 2 POSITRONS ----------------------------
xSouris = cote / 2 + lambda / fraction                    'positron, spin pi.
ySouris = cote / 2 - lambda / fraction
For x = -1 To cote
  x2 = xSouris - x + cote / 2
  If x2 < 0 Or x2 > cote Then x2 = -1
  For y = -1 To cote
    y2 = ySouris - y + cote / 2
    If y2 < 0 Or y2 > cote Then y2 = -1
    P(x, y) = P(x, y) + potentiel2(x2, y2)                'ajout d'un nouveau potentiel.
  Next
Next
xSouris = cote / 2 - lambda / fraction                    'positron, spin 2 pi.
ySouris = cote / 2 + lambda / fraction
For x = -1 To cote
  x2 = xSouris - x + cote / 2
  If x2 < 0 Or x2 > cote Then x2 = -1
  For y = -1 To cote
    y2 = ySouris - y + cote / 2
    If y2 < 0 Or y2 > cote Then y2 = -1
    P(x, y) = P(x, y) - potentiel2(x2, y2)                'potentiel en opposition de phase.
  Next
Next
Return

ToutEffacer:'---------------------- INITIALISER LES VARIABLES --------------------------------
For x = -1 To cote
  For y = -1 To cote
    M(x, y) = .000000001                                  'éviter de mettre la variable à zéro.
    P(x, y) = .000000001                                  ' 0 exactement ralentit le calcul(?).
  Next
Next
nombre = 0
Screenset 2: Locate 33, 65: Print "                       "
Return
