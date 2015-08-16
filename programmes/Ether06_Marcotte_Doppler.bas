Dim amplitudeMax(800) 
Dim As Single pi = 4 * Atn(1), amplitudeArriere, amplitudeAvant, x, t, pixel, rapport
Dim As Single beta, theta, lambdaAvant, lambdaArriere, xPrime, tPrime
Gosub Initialiser

Do
  Swap page1, page2
  Screenset page1, page2
  screensync
  Pcopy 2, page1
  t = 2 * pi * image / images                             'temps, en radians.

'**************************************** ROTATION *******************************************
  For pixel = 0 To largeur
    precedenteArriere = amplitudeArriere
    precedenteAvant = amplitudeAvant
    precedenteTotale = amplitudeTotale
    x = 2 * pi * (xCentre - pixel + .001) / lambdaAvant   'ajout de .001 pour éviter x = 0
    amplitudeAvant =   gabarit * (Cos(t) * Sin(x) - Sin(t) * (1 - Cos(x))) / x / (1 - beta)
    x = 2 * pi * (xCentre - pixel + .001) / lambdaArriere
    amplitudeArriere = gabarit * (Cos(t) * Sin(x) + Sin(t) * (1 - Cos(x))) / x / (1 + beta)
    amplitudeTotale = amplitudeArriere + amplitudeAvant
    If amplitudeMax(pixel) < abs(amplitudeTotale) Then amplitudeMax(pixel) = abs(amplitudeTotale)
    Line(pixel, yCentre - amplitudeMax(pixel))-(pixel, yCentre + amplitudeMax(pixel)),gris
    Line(pixel, yCentre)-(pixel, yCentre - amplitudeTotale), blanc'souligner l'amplitude totale.
    Line(pixel, yCentre - precedenteArriere)-(pixel, yCentre - amplitudeArriere), rouge
    Line(pixel, yCentre - precedenteAvant)-(pixel, yCentre - amplitudeAvant), bleu
    Line(pixel, yCentre - precedenteTotale)-(pixel, yCentre - amplitudeTotale), noir
  Next

'****************************** REPÈRES DES LONGUEURS D'ONDE *********************************
  For pixel = xCentre - 5 * lambda * Cos(theta) To largeur Step lambda * Cos(theta)
    Line(pixel, yCentre - 10)-(pixel, yCentre + 10),noir
  Next

  Line(0, yCentre)-(largeur, yCentre),noir                'axe horizontal.
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
    Case chr(27),"k+": End
    Case "P": Sleep: Do: Loop While Len(inkey)            'pause. Ralenti: vider le tampon.
    Case "0","1","2","3","4","5","6","7","8","9"
      beta = Val(saisie$) / 10
      Gosub miseAJour
  End Select
'  if bitmap = 1 then gosub Bitmaps                       '150 images bitmap au besoin.
  image = image + 1
  If image = images Then image = 0: bitmap = 1
Loop

Bitmaps:'***************************** SÉQUENCE BITMAP ***************************************
Select Case capture
  Case Is < 10: number$ = "00"
  Case Is < 100: number$ = "0"
  Case Is < 1000: number$ = ""
End Select
fileName$ = "capture" + number$ + str(capture) + ".bmp"
'Locate 24, 3: Print fileName$
bsave fileName$,0
capture = capture + 1
If capture = images Then End 
Return

'*********************** MISE A JOUR DES DONNÉES RELATIVES À LA VITESSE **********************
miseAJour:
rapport = (1 + beta) / (1 - beta)
theta = Asin(beta)
lambdaAvant =   lambda * (1 - beta) / Cos(theta)
lambdaArriere = lambda * (1 + beta) / Cos(theta)
screenset 2
locate 5, 95: print using "#.#"; beta
Locate 15,25: print using "### pixels"; lambda / Cos(theta);: ?"."
locate 29,28: print using " ##.#"; rapport
locate 31,27: print using "##.#"; theta * 180 / pi;: print chr(248)
locate 32,30: print using "#.###"; cos(theta)
locate 35,25: print using "#.###"; cos(theta)
For pixel = 0 To largeur
  amplitudeMax(pixel) = 0
next
image = 0
Return

'************************************* INITIALISATION ****************************************
Initialiser:
Screen 19,24,3
fond = Rgb(225,225,225)                                   'couleurs usuelles.
blanc = Rgb(255,255,255)
rouge = Rgb(255,0,0)
bleu = Rgb(0,100,255)
gris = Rgb(185,185,185)
lambda = 150
hauteur = 599: largeur = 799
xCentre = largeur / 2 : yCentre = hauteur / 2
page1 = 1: images = 150
beta = .5
gabarit = 60                                              'gabarit d'amplitude.
Screenset 2, 2: Color noir, fond: Cls
Color noir, blanc
Line(12,10)-(371,34),noir,bf
Line(14,12)-(369,32),blanc,bf
Locate 2,4:?  "LES ONDES DE L'ELECTRON ET L'EFFET DOPPLER"
Line(78,345)-(234,372),noir,bf
Line(80,347)-(232,370),blanc,bf
Locate 23,13:?"L'EFFET DOPPLER"
Line(468,12)-(778,51),noir,bf
Line(470,14)-(776,49),blanc,bf
Locate 2,61:? "Appuyez sur un chiffre de 0 … 9 pour"
Locate 3,61:? "modifier la vitesse d'entraŒnement. "
Color Rgb(0,150,0), fond
Locate 35, 60:? "Gabriel LaFreniŠre   glafreniere.com";
Locate 36,60:?"Le 8 ao–t 2006. Ce programme peut ˆtre";
Locate 37,60:?"copi‚, modifi‚ ou distribu‚ librement.";
Color noir, fond
Locate 4,3:?  "Le 27 juillet 2006, M. Marcotte a r‚ussi …"
Locate 5,3:?  "r‚soudre le calcul des ondes progressives"
Locate 6,3:?  "de l'‚lectron en d‚terminant leur amplitude"
Locate 7,3:?  "pr‚cise lorsqu'elles sont … la quadrature:"
Locate 9,3:?  "Phase:       y = sin(x) / x"
Locate 10,3:? "Quadrature:  y = (1 - cos(x)) / x"
Locate 12,3:? "x = 2 * pi * distance / lambda"
Locate 13,3:? "En plus de l'effet Doppler, lambda"
Locate 14,3:? "subit une dilatation selon Lorentz:"
Locate 15,3:? "L' = L / cos(thˆta) = "
Locate 16,3:? "L  = 150 pixels (au repos)."
Locate 25,3:? "Vers l'avant:   L' * (1 - bˆta)"
Locate 26,3:? "Vers l'arriŠre: L' * (1 + bˆta)"
Locate 28,3:? "Rapport des amplitudes:"
Locate 29,3:? "(1 + bˆta) / (1 - bˆta) ="
Locate 30,3:? "Angle des transformations de Lorentz:"
Locate 31,3:? "thˆta = arc sin(bˆta) = 30";:? chr(248)
Locate 32,3:? "Contraction = cos(thˆta) ="
Locate 33,3:? "Fr‚quence ralentie selon Lorentz,"
Locate 34,3:? "ce qui allonge la longueur d'onde:"
Locate 35,3:? "F' = F * cos(thˆta) =       F"
Locate 36,3:? "Pause ou ralenti: appuyez sur [ P ]"
Locate 37,3:? "Quitter: appuyez sur [ Echap ]";
Locate 5,60:? "Vitesse normalis‚e: bˆta = v / c = 0,5"
Locate 7,60:?"L'‚lectron qui se d‚place … travers l'‚-"
Locate 8,60:? "ther … cause de l'effet Doppler continue"
Locate 9,60:? "de pr‚senter les mˆmes ventres et les"
Locate 10,60:?"mˆmes noeuds que s'il ‚tait au repos, ce"
Locate 11,60:?"qui est tout … fait surprenant. Ce sys-"
Locate 12,60:?"tŠme se d‚place vers la droite, et c'est"
Locate 13,60:?"pourquoi les ondes semblent se propager"
Locate 14,60:?"plus lentement dans ce sens. Leur vites-"
Locate 15,60:?"se est constante relativement … l'‚ther."
Locate 23,62:?"UNE ENVELOPPE MOBILE QUI SE CONTRACTE"
Locate 24,67:?"ET QUI GAGNE EN AMPLITUDE"
Locate 26,60:?"Les ventres et les noeuds se contractent"
Locate 27,60:?"selon les transformations de Lorentz. De"
Locate 28,60:?"plus l'amplitude du systŠme, et donc son"
Locate 29,60:?"‚nergie, augmente avec sa vitesse. Cela"
Locate 30,60:?"confirme que la masse de la matiŠre aug-"
Locate 31,60:?"mente selon l'effet Doppler. Tout ceci"
Locate 32,60:?"suggŠre fortement que la matiŠre est"
Locate 33,60:?"faite d'ondes stationnaires mobiles."
Gosub miseAJour
Return