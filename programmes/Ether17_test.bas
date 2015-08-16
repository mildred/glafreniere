Dim As Single pi = 4 * Atn(1), differenceDeMarche, periode, phase, temps
Dim As Single theta, xCoord, yCoord, xCarre, xPrime, tPrime, diagonale, beta 
beta = Sin(pi / 3): images = 24: largeur = 400: hauteur = 200: lambda = 20: page1 = 1
theta = Asin(beta): xCentre = largeur / 2: yCentre = hauteur / 2: image = 135
Screen 19,24,3: Color noir, Rgb(225,225,225): Screenset 2: Cls: Gosub Texte
Do'          L'EFFET DOPPLER CALCULÉ À L'AIDE DES TRANSFORMATIONS DE LORENTZ.
  Swap page1, page2
  screensync: Screenset page1, page2
  Pcopy 2, page1
  If xPrime * lambda > 800 Then image = 135
  image = image + 1                                       'espace: mesuré en longueurs d'onde.
  temps = image / images                                  'temps:  mesuré en période d'onde.
'------------------------------ AFFICHER DES ONDES NORMALES ----------------------------------
  For x = 0 To largeur                                    'coordonnée x du pixel.
    xCoord = (x - xCentre) / lambda                       'coordonnée x en longueurs d'onde.
    xCarre = xCoord * xCoord                              'distance au carré.
    For y = 0 To hauteur / 2                              'coordonnee y du pixel, symétrie axiale.
      yCoord = (y - yCentre) / lambda                     'coordonnée y en longueurs d'onde.
      diagonale = Sqr(yCoord * yCoord + xCarre)           'selon Pythagore, en longueurs d'onde, .
      differenceDeMarche = diagonale - Int(diagonale)     'une longueur d'onde au maximum.
      phase = 2 * pi * (differenceDeMarche - temps)       'phase en radians selon le temps absolu. 
      ton = 128 * (Sin(phase) + 1)
      If ton < 0 Then ton = 0 Else If ton > 255 Then ton = 255
      Pset(x, hauteur + y + 10), Rgb(ton,ton,ton)         'afficher l'onde normale.
      Pset(x, 2 * hauteur - y + 10), Rgb(ton,ton,ton)     'symétrie axiale.
      If x Mod 2 Then Gosub Transformation                'il y a contraction de moitié.
    Next y
    If Len(inkey) Then End
  Next x
Loop
'---------------------- AFFICHER LES ONDES TRANSFORMÉES SELON LORENTZ ------------------------
Transformation:
xPrime = xCoord * Cos(theta) + temps  * Sin(theta)        'distance x' en longueurs d'onde.
tPrime = temps  * Cos(theta) - xCoord * Sin(theta)        'temps t' en période d'onde.
phase = 2 * pi * (differenceDeMarche - tPrime)            'phase selon t'.
ton = 128 * (Sin(phase) + 1)
If ton < 0 Then ton = 0 Else If ton > 255 Then ton = 255
Pset(xPrime * lambda, y), Rgb(ton,ton,ton)                'afficher l'onde avec effet Doppler.
Pset(xPrime * lambda, hauteur - y), Rgb(ton,ton,ton)
Return

Texte:
Screenset 2
Locate 15, 60: Print "LES TRANSFORMATIONS DE LORENTZ"
Locate 16, 60: Print "     et l'effet Doppler."
Locate 18, 52: Print "Ce programme d‚montre que les transformations de"
Locate 19, 52: Print "Lorentz ne sont rien d'autre et rien de plus que"
Locate 20, 52: Print "l'effet Doppler de la matiŠre. Dans la forme pro-"
Locate 21, 52: Print "pos‚e par Lorentz, les ‚quations ont plut“t pour"
Locate 22, 52: Print "effet de le corriger. Les variables x et x' ont "
Locate 23, 52: Print "donc ‚t‚ permut‚es dans les ‚quations que ce pro-"
Locate 24, 52: Print "gramme utilise. De cette maniŠre, les ondes pr‚-"
Locate 25, 52: Print "sentent cet effet Doppler particulier qui se ca-"
Locate 26, 52: Print "ract‚rise par un ralentissement de la fr‚quence."
Locate 27, 52: Print "La vitesse v vaut 86,6% de celle de la lumiŠre."
Locate 29, 3: Print "Voici les ‚quations que je propose:"
Locate 30, 3: Print "Theta: ";; chr(233); " = 60"; chr(248); " = arc sin (v / c)."
Locate 29, 42: Print "x' = x * Cos "; chr(233); " + t * Sin "; chr(233); "     x' = x * 0,5 + t * 0,866"
Locate 30, 42: Print "t' = t * Cos "; chr(233); " - x * Sin "; chr(233); "     t' = t * 0,5 - x * 0,866": Print
Locate, 3: Print "Lorentz avait clairement dans l'id‚e que la matiŠre qui se d‚place devait se contracter. Il a"
Locate, 3: Print "aussi pr‚cis‚ que si ses formules indiquaient une dilatation de l'espace et du temps, il ne"
Locate, 3: Print "s'agissait l… que d'un artifice math‚matique. Dans ce contexte, la version de la Relativit‚"
Locate, 3: Print "pr‚sent‚e par Albert Einstein apparaŒt ridicule. Pourquoi envisager une hypothŠse aussi d‚liran-"
Locate, 3: Print "te que la dilatation de l'espace et du temps alors qu'il est tellement plus simple et logique"
Locate, 3: Print "de parler de la contraction de la matiŠre ?"
Locate, 52: Print "Gabriel LaFreniŠre  15 mai 2006  glafreniere.com";

Return