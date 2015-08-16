Dim amplitudeMax(800) 
Dim As Single pi = 4 * Atn(1), amplitudeGauche, amplitudeDroite, x, t
Gosub Initialiser

Do
  Swap page1, page2
  Screenset page1, page2
  screensync
  Pcopy 2, page1
  t = 2 * pi * image / images                             'temps, en radians.

'***************************************** ROTATION ******************************************
  For pixel = 0 To largeur
    precedentGauche = yCentre - amplitudeGauche
    precedentDroite = yCentre - amplitudeDroite
    precedentTotal  = yCentre - amplitudeTotale
    x = 2 * pi * (xCentre - pixel + .001) / lambda        '.001 négligeable pour éviter x = 0.
    amplitudeGauche = gabarit * (Cos(t) * Sin(x) + Sin(t) * (1 - Cos(x))) / x
    amplitudeDroite = gabarit * (Cos(t) * Sin(x) - Sin(t) * (1 - Cos(x))) / x
    amplitudeTotale = amplitudeGauche + amplitudeDroite
    If amplitudeMax(pixel) < amplitudeTotale Then amplitudeMax(pixel) = amplitudeTotale
    Line(pixel, yCentre - amplitudeMax(pixel))-(pixel, yCentre + amplitudeMax(pixel)),gris
    Line(pixel, yCentre)        -(pixel, yCentre - amplitudeDroite), blanc'souligner les ondes vers la droite.
    Line(pixel, precedentGauche)-(pixel, yCentre - amplitudeGauche), noir 'ondes vers la gauche.
    Line(pixel, precedentDroite)-(pixel, yCentre - amplitudeDroite), noir 'ondes vers la droite.
    Line(pixel, precedentTotal )-(pixel, yCentre - amplitudeTotale), noir 'ondes stationnaires.
  Next

  For pixel = 0 To largeur
    precedentVert  = yCentre - amplitudeVert
    precedentRouge = yCentre - amplitudeRouge
    x = 2 * pi * (xCentre - pixel + .001) / lambda
    
'****************************** FORMULES DE M. JOCELYN MARCOTTE  *****************************

    amplitudeVert  = gabarit * Sin(x) / x                 'phase, sera affichée en vert.
    amplitudeRouge = gabarit * (1 - Cos(x)) / x           'quadrature, sera affichée en rouge.

'*********************************************************************************************

    Line(pixel, precedentVert )-(pixel, yCentre - amplitudeVert ), vert  'PHASE, EN VERT.
    Line(pixel, precedentRouge)-(pixel, yCentre - amplitudeRouge), rouge 'QUADRATURE, EN ROUGE.
  Next

'*********************************** REPÈRES DU GRAPHIQUE ************************************
  For pixel = xCentre -4  * lambda To largeur Step lambda 'repères des longueurs d'onde.
    Line(pixel, yCentre - 10)-(pixel, yCentre + 10),noir
  Next

  Line(0, yCentre)-(largeur, yCentre),noir                'axe horizontal.
  saisie$ = Right(Ucase(Inkey),1)
  If saisie$ = Chr(27) Or saisie$ = "X" Or saisie$ = "K" Then End
  If saisie$ = "P" Then Sleep: Do: Loop While Len(Inkey)  'pause. Ralenti: vider le tampon.
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
fileName$ = "capture" + number$ + Str(capture) + ".bmp"
'Locate 24, 3: Print fileName$
Bsave fileName$,0
capture = capture + 1
If capture > images - 1 Then End 
Return

'*************************************** INITIALISER *****************************************
Initialiser:
Screen 19,24,3
fond = Rgb(225,225,225)                                   'couleurs usuelles.
blanc = Rgb(255,255,255)
rouge = Rgb(255,0,0)
vert = Rgb(0,255,150)
gris = Rgb(185,185,185)
lambda = 155
largeur = 799
xCentre = 399 : yCentre = 299
gabarit = 149                                             'gabarit d'amplitude.
precedentVert = yCentre
precedentRouge = yCentre
page1 = 1: images = 150
Screenset 2, 2: Color noir, fond: Cls
Color noir, blanc
Line(184 - 78, 152)-(184 + 78, 183), vert, bf             'formule de phase.
Line(184 - 75, 155)-(184 + 75, 180), blanc, bf
Locate 11, 17:? "y = sin(x) / x"
Line(616 - 98, 152)-(616 + 98, 183), rouge, bf            'formule de quadrature.
Line(616 - 95, 155)-(616 + 95, 180), blanc, bf
Locate 11, 68:? "y = (1 - cos(x)) / x"
Color noir, fond
Locate 2, 4:?"LES ONDES PROGRESSIVES DE L'ELECTRON"
Locate 13,74:?"Quadrature"
Locate 13,21:?"Phase"
Locate 4,3:?  "M. Jocelyn Marcotte m'avait signal‚ dŠs"
Locate 5,3:?  "mars 2006 que les ondes stationnaires de"
Locate 6,3:?  "l'‚lectron correspondaient … la formule"
Locate 7,3:?  "suivante, qui est une solution de la fonc-"
Locate 8,3:?  "tion de Bessel sph‚rique (courbe verte):"
Locate 4,60:? "Le 27 juillet 2006, M. Marcotte a r‚ussi"
Locate 5,60:? "… r‚soudre le calcul des ondes progres-"
Locate 6,60:? "sives de l'‚lectron en d‚terminant leur"
Locate 7,60:? "amplitude pr‚cise lorsqu'elles sont …"
Locate 8,60:? "la quadrature (courbe rouge):"
Locate 27,3:? "La distance x est exprim‚e en radians:"
Locate 29,3:? "x = 2 * pi * distance / lambda"
Locate 24,73:?"La rotation."
Locate 26,61:?"L'amplitude des ondes progressives au"
Locate 27,61:?"moment de la quadrature ‚tant connue,"
Locate 28,61:?"il devient possible d'effectuer une ro-"
Locate 29,61:?"tation dans un sens ou dans l'autre en"
Locate 30,61:?"introduisant un temps t (de 0 … 2 * pi)"
Locate 31,61:?"et en fusionnant les deux formules:"
Locate 33,61:?"y = (Cos(t)*Sin(x)-Sin(t)*(1-Cos(x)))/x"
Locate 34,61:?"y = (Cos(t)*Sin(x)+Sin(t)*(1-Cos(x)))/x"
Locate 36,61:?"Simplification selon Philippe Delmotte:"
Locate 37,61:?"y = (Sin(t+x)-Sin(t))/x";
Locate 32,3:? "Pause ou ralenti: appuyez sur [ P ]"
Locate 33,3:? "Quitter: appuyez sur [ Echap ]"
Color Rgb(0,150,0), fond
Locate 35,3:?"Le 15 oct. 2006. Ce programme peut ˆtre";
Locate 36,3:?"copi‚, modifi‚ ou distribu‚ librement.";
Locate 37,3:? "Gabriel LaFreniŠre   glafreniere.com";
Return