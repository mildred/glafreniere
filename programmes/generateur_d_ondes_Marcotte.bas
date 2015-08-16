Dim As Single pi = 4 * Atn(1), x, temps
Gosub Initialiser

Do
  Swap page1, page2
  Screenset page1, page2
  screensync
  Pcopy 2, page1
  image = image + 1
  If image > images Then image = 1
  temps = 2 * pi * image / images

'***************************************** ROTATION ******************************************
  For pixel = 0 To largeur
    x = Abs(2 * pi * (xCentre - pixel) / lambda)
    If x < pi Then x = x + (pi/2) * ((pi-x) / pi)^2       'correction de phase de pi / 2.
'   If x < pi Then x = x - (pi/2) * ((pi-x) / pi)^(1.45)  'ajuste au détriment de la période.
    amplitude = 200 * Sin(x - temps) / x
    Line(pixel, precedent)-(pixel, yCentre - amplitude), noir 'ondes.
    precedent = yCentre - amplitude
  Next

  For pixel = 0 To largeur Step lambda                    'repères des longueurs d'onde.
    Line(pixel, yCentre - 8)-(pixel, yCentre + 8),noir
  Next

  Line(0, yCentre)-(largeur, yCentre),noir                'axe.
  saisie$ = Right(Ucase(Inkey),1)
  If saisie$ = Chr(27) Or saisie$ = "K" Then End
  If saisie$ = "P" Then Sleep: saisie$ = Inkey
  Locate 4,16: ? image
Loop

Initialiser:
Screen 19,24,3
fond = Rgb(225,225,225)
blanc = Rgb(255,255,255)
rouge = Rgb(255,0,0)
vert = Rgb(0,150,0)
gris = Rgb(150,150,150)
lambda = 100
largeur = 799
xCentre = largeur / 2 : yCentre = 450
precedentVert = yCentre
precedentRouge = yCentre
page1 = 1: images = 150
Screenset 2, 2: Color noir, fond: Cls
windowtitle "Le générateur d'ondes Marcotte"
Locate 2,3:  ? "t = 2 * pi * image / images"
Locate 4,3:  ? "t = 2 * pi *       /  150"
Locate 6,3:  ? "x = 2 * pi * distance / lambda"
Locate 9,3: ?  "Correction lorsque x < pi:"
Locate 11,3: ? "x = x + (pi / 2) * (1 - x / pi) ^ 2"
Locate 2,68: ? "y = sin(x) / x"
Line(490, 24)-(525, 25), rouge, b
Locate 4,68: ? "y = sin(x - t) / x"
Line(490, 56)-(525, 57), noir, b
Locate 6,62: ? "On pourrait croire que la courbe noire"
Locate 7,62: ? "devrait respecter la courbe rouge,"
Locate 8,62: ? "mais il faut remarquer qu'il s'agit"
Locate 9,62: ? "ici d'ondes progressives et non pas"
Locate 10,62:? "d'ondes stationnaires. L'amplitude"
Locate 11,62:? "des ondes au centre d‚pend de la"
Locate 12,62:? "structure de l'‚metteur, qui pourrait"
Locate 13,62:? "par exemple ˆtre une petite sphŠre."
Locate 15,62:? "Ce qui importe avant tout, c'est que"
Locate 16,62:? "le programme affiche correctement un"
Locate 17,62:? "retard de phase de pi / 2 au centre."
Locate 18,62:? "Les ondes en sont d‚cal‚es d'un quart"
Locate 19,62:? "d'onde, ce qui permettra d'afficher"
Locate 20,62:? "trŠs pr‚cis‚ment la p‚riode des champs"
Locate 21,62:? "de force que produisent les ‚lectrons."
Locate 14,3: ? "La courbe ‚volue d'une maniŠre trŠs"
Locate 15,3: ? "fluide et la formule de M. Jocelyn"
Locate 16,3: ? "Marcotte est la plus simple qui soit."
Locate 18,3: ? "Puisque cette formule intervient par-"
Locate 19,3: ? "tout, du centre … l'infini, je propose"
Locate 20,3: ? "d'appeler ce g‚n‚rateur d'ondes:"
Locate 22,3: ? "® Le g‚n‚rateur d'ondes Marcotte ¯"
Color vert, fond
Locate 33,3: ? "Pause: appuyez sur le P.";
Locate 34,3: ? "Quitter: appuyez sur Echap.";
Locate 36,3: ? "Le 27 septembre 2007.";
Locate 37,3: ? "Gabriel LaFreniŠre   glafreniere.com";
Locate 36,58:? "Ce programme FreeBasic peut ˆtre"
Locate 37,58:? "distribu‚, copi‚ ou modifi‚ … volont‚.";
Color noir, fond
For pixel = 0 To largeur                                  'formule sin(x)/x.
  x = Abs(2 * pi * (xCentre - pixel) / lambda)
  If x Then amplitude = 200 * Sin(x) / x
  Line(pixel, precedent)-(pixel, yCentre - amplitude), rouge
  precedent = yCentre - amplitude
Next
Return
