images = 36: largeur = 800: hauteur = 650
Dim gris(images,largeur,hauteur)                          'grand tableau 3D.
Dim graph(images,largeur)                                 'graphique 1D.
Dim As Single amplitude(largeur,hauteur)
Dim As Single pi, deuxPi, angle, phase, flottante, differenceDeMarche, facteurDeLorentz
Dim As Single diagonale, sag, periode, rotation, rapport, temps, beta, pas, deltaTemps
Dim As Single facteur, xCoord, contraste, affaiblissement, rayon, amplit, phase2
Screen 19,24,3: page1 = 1: Gosub Initialisation

Do'    « L'ONDE DE LAFRENIÈRE »
'      CALCULÉE À L'AIDE DES TRANSFORMATIONS DE LORENTZ (OU DU SCANNER DU TEMPS).
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1

  If OK Then                                              'afficher le tableau de variables.
    For x = 0 To largeur
      For y = 0 To maximum
        Pset(x,y),gris(image,x,y)
      Next
'    if pleinEcran = 0 then pset(x, graphique), grisMoyen: pset(x, graph(image,x)), noir
    Next    
  Else Gosub Memoriser
  End If

  saisie$ = Inkey
  If Len(saisie$) = 2 and Right(saisie$, 1) = "k" then end
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  If Len(saisie$) Then
    Do: Loop While Len(Inkey)                             'vider le tampon.
    capture = 0
    If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+"
    Select Case saisie$
      Case "I": Gosub Initialisation
      Case "A": Screenset 2: Locate 21, 77
                If acceleration Then
                  acceleration = 0: Print ligne21$
                Else
                  acceleration = 1: Print ligne21a$
                End If
                Screenset page1, page2
      Case "B": Screenset 2: Locate 22, 77
                If relief Then
                  relief = 0: Print ligne22$
                Else
                  relief = 1: Print ligne22a$
                End If                
                Screenset page1, page2
      Case "C": If pleinEcran Then
                  pleinEcran = 0
                  Screenset 2: Locate 23, 77: Print ligne23$
                  Screenset page1, page2
                  Else pleinEcran = 1
                End If
      Case Chr$(27)
                If pleinEcran Then
                  pleinEcran = 0
                  Screenset 2: Locate 23, 77: Print ligne23$
                  Screenset page1, page2
                  Else End
                End If
      Case "M": Run "Ether00.exe"
      Case "K+":Run "Ether09.exe"                         'flèche gauche.
      Case "M+":Run "Ether11.exe"                         'flèche droite.  
      Case "1": beta = .1
      Case "2": beta = .2
      Case "3": beta = .3
      Case "4": beta = .4
      Case "5": beta = .5
      Case "6": beta = .6
      Case "7": beta = .7
      Case "8": beta = .8
      Case "9": beta = .9
      Case "0": beta =  0
      Case "+": lambda = lambda + 10
                If lambda > 200 Then lambda = 200
      Case "-": lambda = lambda - 10
                If lambda < 20 Then lambda = 20
      Case Else: saisie$ = "fait"
    End Select
    If saisie$ = "fait" Then Else image = 0: Gosub Electron
  End If
  
  Getmouse xSouris, ySouris, , clic                       'saisie Souris.
  ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
  If ligne < 23 Then
    If ligne < 10 Or xSouris > 794 Or xSouris < 616 Then ligne = 0
  Elseif ligne > 34 Then
    If ligne > 37 Or xSouris < 304 Or xSouris > 496 Then ligne = 0
  End If
  If clic = 1 Then
    Do: Getmouse xSouris, ySouris, , clic: Loop While clic > 0
    Select Case ligne
      Case 10: beta = 0
      Case 11: beta = .1
      Case 12: beta = .2
      Case 13: beta = .3
      Case 14: beta = .4
      Case 15: beta = .5
      Case 16: beta = .6
      Case 17: beta = .7
      Case 18: beta = .8
      Case 19: beta = .9
      Case 21: Screenset 2: Locate 21, 77
               If acceleration Then
                 acceleration = 0: Print ligne21$
               Else
                 acceleration = 1: Print ligne21a$
               End If
               Screenset page1, page2
      Case 22: Screenset 2: Locate 22, 77
               If relief Then
                 relief = 0: Print ligne22$
               Else
                 relief = 1: Print ligne22a$
               End If                
               Screenset page1, page2
      Case 23: If pleinEcran Then pleinEcran = 0 Else pleinEcran = 1
      Case 35: Gosub Initialisation: ligne = 0
      Case 36: End
      Case 37: If xSouris < 400 Then Run "Ether09.exe" Else  Run "Ether11.exe"
      Case Else: saisie$ = "fait"
    End Select
    If saisie$ = "fait" Then Else image = 0: Gosub Electron
  End If

  Color , turquoise: Locate ligne, 77
  Select Case ligne                                       'rehausser l'affichage.
    Case 10: Print ligne10$
    Case 11: Print ligne11$
    Case 12: Print ligne12$
    Case 13: Print ligne13$
    Case 14: Print ligne14$
    Case 15: Print ligne15$
    Case 16: Print ligne16$
    Case 17: Print ligne17$
    Case 18: Print ligne18$
    Case 19: Print ligne19$
    Case 21: If acceleration Then Print ligne21a$ Else Print ligne21$
    Case 22: If relief Then Print ligne22a$ Else Print ligne22$
    Case 23: If pleinEcran Then Print ligne23a$ Else Print ligne23$
    Case 35: Locate, 39: Print ligne35$
    Case 36: Locate, 39: Print ligne36$
    Case 37: Locate, 39: Print ligne37$;
             If xSouris < 400 Then Gosub flecheGauche Else Gosub flecheDroite
  End Select
  Color , fond
'  locate 29, 78: print using "Image ##    "; image;
'  print using "#.## sec"; timer - temps;: print "."
  If bitmap Then Gosub Bitmaps                            'capture d'images si désiré.
'  temps = timer
  image = image + 1: If image > images Then image = 1
  If acceleration Then
    beta = beta + .001: If beta > .99 Then beta = 0
    Gosub Electron
  End If
Loop

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
Select Case capture
  Case Is < 10: numero$ = "00"
  Case Is < 100: numero$ = "0"
  Case Is < 1000: numero$ = ""
End Select
fichier$ = "capture" + numero$ + Str(capture) + ".bmp"
Locate 34, 43: Print fichier$
'bsave fichier$,0
capture = capture + 1
If capture > images - 1 Then End 
Color noir, fond
Return

Electron:'------------------ METTRE EN MÉMOIRE LES DONNÉES DE L'ÉLECTRON ----------------------

'############################ APPLICATION DES TRANSFORMATIONS DE LORENTZ #####################

facteurDeLorentz = Sqr(1 - beta ^ 2)                      'facteur de contraction de Lorentz.
deltaTemps = beta / facteurDeLorentz                      'temps local selon Henri Poincaré.

'#############################################################################################
Screenset 2                                               'mise à jour de l'image-mère.
Locate 28, 78: Print "lambda ="; lambda; " pixels.   "
Locate 28, 2:  Print Using "Beta: #.###"; beta
Locate 28, 46: Print Using "Contraction: #.###"; facteurDeLorentz
Screenset page1, page2
If OK Then image = 0                                      'sinon accélération compromise.
OK = 0                                                    'tableau de variables à refaire.
lambdaSurDeux = lambda / 2
facteur = .002 * lambda                                   'facteur d'amplitude du relief.
If pleinEcran Then
  largeur = 800
  If relief Then hauteur = 650 Else hauteur = 600
  maximum = 600: graphique = 550
  Else
  largeur = 600: hauteur = 415
  maximum = 415: graphique = 402
End If
If pleinEcran = 0 Or acceleration = 0 Then
  For j = 0 To images
    For x = 0 To largeur
      For y = hauteur - 50 To hauteur                     'effacer la partie inférieure. 
        gris(j,x,y) = fond
      Next
    Next
  Next
End If
If pleinEcran Then graphique = 550 Else graphique = 402   'graphique 1D.
xCentre = largeur / 2
yCentre = hauteur / 2
                 
For x = 0 To largeur / 2                                  'symétrie gauche-droite.
  xCoord = x / facteurDeLorentz                           'contraction des distances.
  xCarre = xCoord ^ 2 
  For y = 0 To hauteur                                    'pleine hauteur pour relief.
    yCoord = (y - yCentre)                                'centre à la demi-hauteur.
    If relief Then yCoord = 2 * yCoord                    'relief vu selon un angle de 60°.
    diagonale = Sqr(yCoord * yCoord + xCarre)             'distance réelle selon Pythagore.
    flottante = diagonale / lambda                        'distance en longueurs d'onde.
    entier = flottante                                    'distance, nombre entier.
    differenceDeMarche = flottante - entier               'difference en longueurs d'onde.
    If diagonale > lambdaSurDeux Then
      affaiblissement = Sqr(lambdaSurDeux / diagonale)    'affaiblissement atténué.
    Else'                                                 'noyau central onde entière.
      sag = rayon - Sqr(rayon ^ 2 - differenceDeMarche ^ 2) 'flèche d'un cercle fictif.
      differenceDeMarche = 1.25 * (.2 + sag)              'arrondir l'angle central.
      xCoord = 2 * flottante
      affaiblissement = 2 / (xCoord ^ 2 + 1)              'formule du noyau de l'électron.
    End If
    phase = deuxPi * differenceDeMarche                   'phase en radians.
    amplit = -Cos(phase + pi / 2)                         'quadrature pour amplitude max.
    amplitude(xCentre + x, y) = amplit * affaiblissement
    amplitude(xCentre - x, y) = amplit * affaiblissement
  Next y
Next x
Return

Memoriser:'----------------------- MÉMORISER TOUTES LES IMAGES -------------------------------
If image = images Then OK = 1
phase2 = image * 2 * pi / images

For x = 0 To largeur
  periode =  deltaTemps * (xCentre - x) * 2 * pi / lambda
  For y = 0 To hauteur
    ton = 127 + 127 * amplitude(x,y) * Sin(phase2 + periode)
    yCoord = y - facteur * ton + 70 * facteur             'décaler selon la luminance.
    If ton > 255 Then ton = 255
    If ton < 0 Then ton = 0
    If relief Then                                        'effet de relief si désiré.
      If yCoord < 0 Then yCoord = 0
      If yCoord < 403  Or pleinEcran Then Pset (x, yCoord), Rgb(ton, ton, ton)
      gris(image,x,yCoord) = Rgb(ton, ton, ton)           'grand tableau de variables.
      ecart = yCoord - yCoordPrec
      If ecart > 1  Then                                  'estomper avec le pixel précédent.
        luminance2 = 127 + 127 * amplitude(x, y - 1) * Sin(phase2 + periode)
        ton = 127 + 127 * amplitude(x,y) * Sin(phase2 + periode)'ton de départ.
        pas = (luminance2 - ton) / ecart
        For j = 1 To ecart - 1                            'combler les espaces vides.
          ton = ton + pas
          If ton > 255 Then ton = 255
          If ton < 0 Then ton = 0
          If yCoord - j < 403  Or pleinEcran Then Pset(x - gauche, yCoord - j), Rgb(ton, ton, ton)
          gris(image,x,yCoord - j) = Rgb(ton, ton, ton)
        Next
      End If        
      yCoordPrec = yCoord
    Else
      Pset(x,y), Rgb(ton, ton, ton)
      gris(image,x,y) = Rgb(ton, ton, ton)                'tableau de variables sans relief.
    End If
  Next
Next
periode =  deltaTemps * (xCentre) * 2 * pi / lambda       'graphique 1D.
prec = 22 * amplitude(0,yCentre) * Sin(phase2 + periode)
For x = 1 To largeur
  periode =  deltaTemps * (xCentre - x) * 2 * pi / lambda
  yCoord = 22 * amplitude(x,yCentre) * Sin(phase2 + periode)
  Line(x, graphique - 22 * amplitude(x,yCentre))-(x, graphique + 22 * amplitude(x,yCentre)), blanc
  Pset(x, graphique), grisMoyen
  Line(x - 1, graphique - prec)-(x, graphique - yCoord), noir
  graph(image,x) = graphique - yCoord
  prec = yCoord
Next
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

Initialisation:'------------------------ INITIALISATION --------------------------------------
vert = Rgb(0,150,0)
bleu = Rgb(0,0,255)
rouge = Rgb(255,0,0)
fond = Rgb(225,225,225)
blanc= Rgb(255,255,255)
grisMoyen = Rgb(128, 128,128)
turquoise = Rgb (230, 255, 255)
pi = 4 * Atn(1)
deuxPi = 2 * pi
rayon = Sin(pi / 4)'                                      'rayon fictif pour le noyau central.
'bitmap = 1                                               'séquence bitmap si désiré.
relief = 1                                                'afficher les ondes en relief.
pleinEcran = 0
acceleration = 0
'largeur = 600: hauteur = 398
beta = Sin(pi / 4)                                        'vitesse normalisée: c = 1.
lambda = 80
xCentre = largeur / 2
yCentre = hauteur / 2
contraste = 2
luminosite = 255 / contraste
xgg = 400 - 50                                            'coordonnées des flèches.
xgd = 400 - 20
xdg = 400 + 20
xdd = 400 + 50
yFleche = 584
Screenset 2,2
Color noir, fond: Cls
Locate 27, 2: Print "L'onde de LaFreniŠre"
Locate 27, 58:Print "LaFreniere's Wave"
Locate 28, 16:Print "glafreniere.com"
Locate 28, 66:Print "Jan. 2006"
If bitmap Then Gosub Electron: Return
Line(610,6)-(785,36), blanc, B 
Line(611,7)-(786,37), blanc, B 
Line(612,8)-(787,38), rouge, BF
Color blanc, rouge: Locate 2, 82
Print "THE ELECTRON"
Color noir, fond
Locate 4, 77: Print "    and Lorentz's "
Locate 5, 77: Print "   Transformations."
Locate 30, 2: Print "The spherical and concentric standing wave systems look quite different while they are undergoing"
Locate 31, 2: Print "Lorentz's transformations. A true Doppler effect stunningly transforms both incoming and outgoing"
Locate 32, 2: Print "waves. Those results are confirmed thanks to the Virtual Aether. My Time Scanner also confirms   "
Locate 33, 2: Print "them. Those three demonstrations prove that such a wave system is possible. It is an electron."
Locate 7, 78: Print "Select the normalized" 
Locate 8, 78: Print "speed (beta = v/c):"
ligne10$ = " 0 - 0,0 c  at rest.   ": Locate 10, 77: Print ligne10$
ligne11$ = " 1 - 0,1 c             ": Locate 11, 77: Print ligne11$
ligne12$ = " 2 - 0,2 c             ": Locate 12, 77: Print ligne12$
ligne13$ = " 3 - 0,3 c             ": Locate 13, 77: Print ligne13$
ligne14$ = " 4 - 0,4 c             ": Locate 14, 77: Print ligne14$
ligne15$ = " 5 - 0,5 c  half of c. ": Locate 15, 77: Print ligne15$
ligne16$ = " 6 - 0,6 c             ": Locate 16, 77: Print ligne16$
ligne17$ = " 7 - 0,7 c             ": Locate 17, 77: Print ligne17$
ligne18$ = " 8 - 0,8 c             ": Locate 18, 77: Print ligne18$
ligne19$ = " 9 - 0,9 c  90 % of c. ": Locate 19, 77: Print ligne19$
ligne21$ = " A - Accelarating.     ": Locate 21, 77: Print ligne21$
ligne21a$ =" A - Stop accelerating."
ligne22a$ =" B - No 3-D Graphics.  "
ligne22$ = " B - 3-D Graphics.     "
ligne23$ = " C - Full Window.      ": Locate 23, 77: Print ligne23$
ligne23a$= " C - Normal Window.    "
Locate 22, 77: If relief Then Print ligne22a$ Else Print ligne22$
ligne35$ = "    I - Initialise.     ": Locate 35, 39: Print ligne35$
ligne36$ = "    Quit (Press Esc).   ": Locate 36, 39: Print ligne36$
ligne37$ = "                        "
Gosub flecheGauche: Gosub flecheDroite
Locate 25, 78: Print "Press [ + ] or [ - ] "
Locate 26, 78: Print "to increase or reduce"
Locate 27, 78: Print "wavelength:          "
Color vert
Locate 35
Locate , 2:  Print "Special thanks to FreeBASIC creators"
Locate , 2:  Print "and to Mr. Anselme Dewavrin."
Locate , 2:  Print "Gabriel LaFreniere  glafreniere.com";
Locate 35
Locate , 65: Print "Jan. 2006. This freeBasic program"
Locate , 65: Print "can be freely distributed, copied"
Locate , 65: Print "or modified.";
Color noir
Pcopy 2, 0
Pcopy 2, 1
Gosub Electron
Return
