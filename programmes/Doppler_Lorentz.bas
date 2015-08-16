Dim As Single beta, theta, gLorentz, amplitude, tTemps, centre
Dim As Single pi, deuxPi, xCoord, yCoord, xCarre, yCarre, trajetLumiere, trajetSource
Screen 19,24,3: Gosub Initialisation  'par Gabriel LaFrenière, le 25 août 2007.

Do
  Swap pageVisible, pageCachee                            'échanger les pages 0 et 1.
  screensync
  Screenset pageCachee, pageVisible                       'travailler sur la page cachée.
  Pcopy pageMere, pageCachee                              'copier page-mère sur page cachée.
  tTemps = image * deuxPi / images                        'période de l'onde selon l'image.
  Gosub EmetteurStationnaire                              'ondes sans effet Doppler.
  Gosub EmetteurMobile                                    'ondes avec effet Doppler.
  image = image + 1                                       'écoulement du temps.
  If Len(Inkey) Then End                                  'saisie clavier: sortie.
  Do
    Getmouse xSouris, ySouris, , clic                     'saisie sourie: pause.
    If clic = 1 Then Else Exit Do
  Loop
Loop

EmetteurStationnaire:'------------ ONDES SANS EFFET DOPPLER -----------------------------------

For x = 0 To largeur                                      'balayage horizontal.
  xCoord = (x - demiLargeur) / lambda                     'coordonnées x en longueurs d'onde.
  xCarre = xCoord^2                                       'accélère le calcul dans la boucle.
  For y = 0 To demiHauteur                                'symétrie verticale.
    yCoord = y / lambda                                   'coordonnées y en longueurs d'onde.
    trajetLumiere = Sqr(xCarre + yCoord^2)                       'côté AC = délai en périodes d'onde.
    amplitude = Sin(deuxPi * (trajetLumiere - tTemps))           'dépend du temps t et du délai AC.
    luminance = 127 * (amplitude + 1)                     'luminance en 255 tons de gris.
    Pset(x, demiHauteur + y), Rgb(luminance,luminance,luminance)
    Pset(x, demiHauteur - y), Rgb(luminance,luminance,luminance)
  Next
Next
Return

EmetteurMobile:'------------------ ONDES AVEC EFFET DOPPLER -----------------------------------

' Pour obtenir l'effet Doppler normal, on utilise ici un calcul élémentaire. Il s'agit de
' montrer que les transformations de Lorentz sont simples. En effet, pour modifier ensuite
' cet effet Doppler selon les prévisions de Lorentz, il suffit de ralentir la fréquence de
' l'émetteur selon le facteur « g » et d'allonger la longueur d'onde selon: lambda / g.

centre = beta * tTemps * lambda                           'centre du système après un temps t
If centre > 600 Then image = 0                            ' (simple translation selon v * t).

For x = 0 To largeur
  xCoord = (x - demiLargeur) / lambda
  For y = 0 To demiHauteur
    yCoord = (demiHauteur - y) / lambda
    yCarre = yCoord^2
    For j = 1 To 5                                        'calcul itératif, préciser le trajet.
      trajetSource = beta * trajetLumiere
      trajetLumiere = Sqr((xCoord + trajetSource)^2 + yCarre)'triangle rectangle: Pythagore.
    Next                                                  'trajetLumiere indique le délai.

' Étape 1. L'effet Doppler normal dépend du trajet effectué par la lumière pour atteindre le
'          point mobile situé en x,y. La durée de ce trajet varie selon que le rayon provient
'          de l'avant ou de l'arrière, la vitesse absolue des ondes étant constante.

    amplitude = Sin(deuxPi * (trajetLumiere - tTemps))    'FRÉQUENCE ORIGINALE.
    luminance = 127 * (amplitude + 1)                     'luminance en 255 tons de gris.
    Pset(largeur + x + 2, y), Rgb(luminance,luminance,luminance)
    Pset(largeur + x + 2, hauteur - y), Rgb(luminance,luminance,luminance)

' Étape 2. L'effet Doppler selon Lorentz tient compte du ralentissement de la fréquence de
'          l'électron selon le facteur g de Lorentz. Il en résulte une dilatation de sa
'          longueur d'onde, qu'on obtient ici en raccoucissant la longueur du trajet.

    amplitude = Sin(deuxPi * (gLorentz * trajetLumiere - gLorentz * tTemps))
    luminance = 127 * (amplitude + 1)
    Pset(centre + x, hauteur + y + 20), Rgb(luminance,luminance,luminance)
    Pset(centre + x, 2 * hauteur - y + 20), Rgb(luminance,luminance,luminance)
  Next
Next

'------------------------------- REPÈRES DES LONGUEURS D'ONDE ---------------------------------
Line(demiLargeur, 0)-(demiLargeur, hauteur), blanc
Line(largeur + demiLargeur + 2, 0)-(largeur + demiLargeur + 2, hauteur), blanc
Line(centre + demiLargeur, hauteur + 20)-(centre + demiLargeur, 2 * hauteur + 20), blanc
For j = 0 To 8 * lambda Step lambda
  Line(demiLargeur - 10, j)-(demiLargeur + 10, j), blanc
  Line(largeur + demiLargeur - 8, j)-(largeur + demiLargeur + 12, j), blanc
  Line(centre + demiLargeur - 10, j + hauteur + 20)-(centre + demiLargeur + 10, j + hauteur + 20), blanc
Next
Return

'-------------------------------------- INITIALISATION ----------------------------------------
Initialisation:
pageMere = 2
pageCachee = 1
Windowtitle "Programme Doppler_Lorentz -  L'effet Doppler selon Lorentz."
pi = 4 * Atn(1)
deuxPi = 8 * Atn(1)
beta = .5                                                 'vitesse normalisée: bêta = v/c.
theta = Asin(beta)                                        'angle des ondes transversales.
gLorentz = Cos(theta)                                     'facteur de contraction g de Lorentz.
lambda = 32                                               'longueur d'onde, système au repos.
images = 128                                              'nombre d'images par période.
largeur = 398                                             'dimensions de la fenêtre.
hauteur = 8 * lambda
demiLargeur = largeur / 2
demiHauteur = hauteur / 2
blanc = Rgb(255,255,255)
fond = Rgb(225,225,225)
Screenset pageMere
Color noir, fond: Cls
Color Rgb(0,150,0)
Locate 36,56: Print "Gabriel LaFreniŠre    glafreniere.com"
Locate 37, 2: Print "Le code source (voir freebasic.net) peut ˆtre distribu‚, copi‚ ou modifi‚ en toute libert‚.";
Color noir
Locate 17,18: Print "Emetteur au repos.                L'effet Doppler normal. Contraction transversale."
Locate 35, 2: Print "L'effet Doppler selon Lorentz. La fr‚quence ralentit et il n'y a plus de contraction transversale."
Locate 36, 2: Print "Pause: cliquez sur la fenˆtre."
Return
' Il existe une formule qui permet d'obtenir plus directement l'effet Doppler « relatif »:
' lambda2=lambda1*(cos(asin(beta*sin(phi)))-beta*cos(phi))
' phi est l'angle du point (x,y) comparativement au centre du système.
