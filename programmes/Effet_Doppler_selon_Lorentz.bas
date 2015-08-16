Dim As Single pi = 4 * Atn(1)
Dim As Single amplitude, distance, xPrime, xCarre, xMarcotte, beta, theta, tPrime, x, y, t
Gosub Initialiser

Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  t = 2 * pi * image / images                             'temps selon les images, en radians.

  For xCoord = -demiLargeur To demiLargeur
    x = 2 * pi * xCoord / lambda
  
'************************ APPLICATION DES TRANSFORMATIONS DE LORENTZ **************************

    xPrime =  x / Cos(theta)
    tPrime = -x * Sin(theta)

'**********************************************************************************************
'    xPrime = 2 * pi * xCoord / (cos(theta) * lambda)     'au choix: essayez pour voir!

'   important: selon Lorentz, il y a contraction de la longueur
'   d'onde selon: xPrime = 2 * pi * xCoord / (cos(theta) * lambda)
'   mais ses équations originales ont plutôt pour effet de
'   dilater l'espace selon: x / cos(theta), ce qui est ici plus simple, et revient au même.

    xCarre = xPrime ^ 2
    For yCoord = 0 To demiHauteur
      y = 2 * pi * yCoord / lambda                        'N.B. y'= y selon Lorentz.
      xMarcotte = Sqr(y * y + xCarre): If xMarcotte = 0 Then xMarcotte = .001  'éviter x = 0
      amplitude = Sin(xMarcotte - t - tPrime) / xMarcotte 'selon M. Marcotte: y = sin(x) / x
      ton = contraste * (amplitude + 128 / contraste)
      If ton > 255 Then ton = 255 Else If ton < 0 Then ton = 0
      Pset(xCoord + demiLargeur, demiHauteur - yCoord), Rgb(ton,ton,ton)
      Pset(xCoord + demiLargeur, demiHauteur + yCoord), Rgb(ton,ton,ton)
    Next
  Next

  saisie$ = Right(Ucase(Inkey),1)
  If saisie$ = Chr(27) Or saisie$ = "X" Or saisie$ = "K" Then End
  If saisie$ = "P" Then Sleep: Do: Loop While Len(Inkey)  'pause. Ralenti: vider le tampon.
  image = image + 1
  If image = images Then image = 0
Loop

'*************************************** INITIALISER *****************************************
Initialiser:
Screen 19,24,3
fond = Rgb(225,225,225)                                   'couleurs usuelles.
lambda = 32
largeur = 500
demiLargeur = largeur / 2
hauteur = 350
demiHauteur = hauteur / 2
xCentre = 399 : yCentre = 299
page1 = 1
images = 64
contraste = 1000
beta = .5
theta = Asin(beta)
Screenset 2, 2: Color noir, fond: Cls
Locate 5,65:? "L'effet Doppler est obtenu grƒce …"
Locate 6,65:? "ces ‚quations ‚l‚mentaires d‚riv‚es"
Locate 7,65:? "des transformations de Lorentz:"
Locate 10,68:?"x' =  x / cos(thˆta)"
Locate 12,68:?"t' = -x * sin(thˆta)"
Locate 15,68:?"x et t en radians."
Locate 17,68:?"x = 2 * pi: une longueur d'onde."
Locate 19,68:?"t = 2 * pi: une p‚riode d'onde."
Locate 21,68:?"thˆta = asin(v / c)"
Locate 25,26:?"LES TRANSFORMATIONS DE LORENTZ ET L'EFFET DOPPLER"
Locate 27,3:? "T“t ou tard, les physiciens devront reconnaŒtre que les transformations de Lorentz n'ont pas la"
Locate 28,3:? "vertu de transformer l'espace et le temps. Cette id‚e est absurde. Lorentz parlait plut“t d'une"
Locate 29,3:? "contraction des objets, d'un d‚calage horaire et d'un ralentissement des horloges. Ce programme"
Locate 30,3:? "montre qu'il s'agit tout simplement d'une cons‚quence de l'effet Doppler que subit la matiŠre,"
Locate 31,3:? "ce que les transformations de Voigt laissaient d'ailleurs entrevoir..."
Locate 33,3:? "Pause ou ralenti: appuyez sur [ P ]"
Locate 34,3:? "Quitter: appuyez sur [ Echap ]"
Color Rgb(0,150,0), fond
Locate 37,3:? "Gabriel LaFreniŠre   glafreniere.com";
Locate 36,61:?"Le 6 sept. 2006. Ce programme peut ˆtre";
Locate 37,61:?"copi‚, modifi‚ ou distribu‚ librement.";
Return