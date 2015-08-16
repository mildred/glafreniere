Dim As Single h, t, x, y, pi, deuxPi, phase, distance, amplitude
Dim As Single rayon, rayonExterieur, rayonInterieur
Screen 19,24,3: page1 = 1: Gosub Initialisation

'     par Gabriel LaFrenière le 30 mai 2007. Ondes avec noyau central onde entière.
Do
  Swap page1, page2                                       'les valeurs sont échangées.
  Screenset page1, page2                                  'travailler sur la page cachée.
  Pcopy 2, page1
  screensync
  Gosub Graphique
  If bitmap Then Gosub Bitmaps
  image = image + 1
  If image > images Then image = 1
Loop

Graphique:'----------------------------- GRAPHIQUE --------------------------------------------
Locate 6,15: Print Using "###"; image
t = 2 * pi * image / images                               'période de l'onde selon l'image.
ondePrec = echelle * Sin(t - (pi / 2))
xPrecedent = xCentre - echelle * Cos(t - (pi / 2))
yPrecedent = yGraph  - echelle * Sin(t - (pi / 2))

For xPixel = 1 To demiLargeur
  x = 2 * pi * xPixel / lambda                            'distance en radians.
  y = 1 / x                                               'amplitude, inverse de la distance.
  If xPixel < lambda / 2 Then                             'correction pour le noyau central.

' tout indique que l'amplitude de l'onde dépend d'un certain volume dans lequel l'énergie
' peut se déployer à un instant donné. Le calcul simplifié ci-dessous montre qu'on peut
' obtenir une assez bonne approximation en calculant le volume de la « couche d'oignon »
' dans laquelle l'onde se déploie. Mais pour obtenir des résultats plus précis, il faudrait
' considérer une répartition sinusoïdale de la pression à l'intérieur de cette couche, ou
' peut-être une répartion selon la distribution normale simplifiée:  y = pi ^ -(x ^ 2)

    rayonExterieur = (pi + x)^2.65
    rayonInterieur = (pi - x)^2.65
    y = 41.5 / (rayonExterieur + rayonInterieur)
'    If xPixel < lambda / 4 Then y = sin(x)/x
'    phase = t - x - (pi / 2) * (( lambda / 2 - xPixel) / (lambda / 2))^2
'    phase = t - x - (pi / 2) * ((pi - x) / pi) ^ 2
    phase = t - x - (pi / 2) * (1 - x / pi) ^ 2           'ajouter une correction à la phase.
    onde = echelle * y * Sin(phase)
    couleur = vert                                        'courbe en vert si x < pi.
  Else
    phase = t - x
    onde = echelle * y * Sin(phase)
    couleur = violet                                      'courbe en violet si x > pi.
  End If
  
  Pset(xCentre + xPixel, yGraph - echelle * y), noir      'courbe noire selon le volume.
  Pset(xCentre - xPixel, yGraph - echelle * y), noir
  
  Line(xCentre + xPixel - 1, yGraph - ondePrec)-(xCentre + xPixel, yGraph - onde),couleur
  Line(xCentre - xPixel + 1, yGraph - ondePrec)-(xCentre - xPixel, yGraph - onde),couleur
  ondePrec = onde
  x = xCentre - echelle * y * Cos(phase)
  y = yGraph  - echelle * y * Sin(phase)
  Line(xPrecedent,yPrecedent)-(x,y),couleur
  xPrecedent = x
  yPrecedent = y
Next

'If image = images / 2 + 1 Then Sleep 1000
a$ = Inkey                                                'vérifier le clavier.
If a$ = Chr(255)+ "k" Or a$ = Chr(27) Then End
a$ = Ucase(a$)
If a$ = "P" Then Sleep: a$ = "": b$ = Inkey: If b$ = Chr(255)+ "k" Or b$ = Chr(27) Then End
Do: Loop While Len(Inkey)
If a$ = "R" Then Sleep 100
If Len(Inkey) Then Sleep 100
Getmouse xSouris, ySouris, roulette, clic                 'vérifier la souris.
If clic = 1 Then Sleep 200
Return

Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
Select Case capture
  Case Is < 10: numero$ = "00"
  Case Is < 100: numero$ = "0"
  Case Is < 1000: numero$ = ""
End Select
Locate 34, 80: Print Using "Capture ## /"; capture;: Print captures
fichier$ = "capture" + numero$ + Str(capture) + ".bmp"
Bsave fichier$,0
capture = capture + 1
If capture > captures Then End
Return

'------------------------------------- INITIALISATION ----------------------------------------
Initialisation:
pi = 4 * Atn(1)
deuxPi = 8 * Atn(1)
echelle = 150                                             'échelle ou gabarit du graphique.
lambda = 2 * echelle / pi                                 'équilibrer avec l'amplitude.
lambdaSurDeux = lambda / 2
images = 256                                              'nombre d'images par période.
image = 1
t = 0
captures = images                                         'pour animations.
capture = 1
largeur = 550                                             'dimensions de la fenêtre.
demiLargeur = largeur / 2
facteur = 24                                              'facteur d'amplitude du relief.
xCentre = demiLargeur
yCentre = demiHauteur
yGraph = echelle
'bitmap = 1                                               'crée une séquence bitmap.

blanc = Rgb(255,255,255)
gris = Rgb(150,150,150)
fond = Rgb(225,225,225)
rouge = Rgb(255,0,0)
vert = Rgb(0,175,0)
bleu = Rgb(0,0,225)
violet = Rgb(200,0,255)
Color noir, fond
Screenset 2,2: Cls
Color Rgb(0,150,0), fond
Windowtitle "Générateur d'ondes sphériques." 
If bitmap Then
  Locate 24, 15: Print "Une s‚quence bitmap sera cr‚‚e."
  Locate 26, 15: Print "Appuyez sur ® Entr‚e ¯ pour confirmer."
End If
Locate 34,58:Print "Le code source (voir freebasic.net) peut"
Locate 35,58:Print "ˆtre distribu‚, copi‚ ou modifi‚ librement."
Locate 36,58:Print "The source code (see freebasic.net) may be"
Locate 37,58:Print "freely distributed, copied or modified.";
Color noir, fond
Locate 2,2: Print "x = 2 * pi * distance / lambda"
Locate 2,50:Print "y = 1 / x"
Line(355,24)-(385,25),bleu,b
Locate 4,50:Print "y = 1 / v"
Line(355,56)-(385,57),noir,b
Locate 6,50:Print "y = sin(x) / x"
Line(355,88)-(385,89),rouge,b
Locate 14,6:Print "Rotation:"
Locate 4,2: Print "t = 2 * pi * image / images"
Locate 6,2: Print "t = 2 * pi *     /"; images
Locate 16,6:Print "y = Sin(t - x) / x"
Line(4,249)-(34,250),violet,b
Locate 18,6:Print "y = Sin(t - x - corr) / v"
Line(4,281)-(34,282),vert,b
Locate 14,42:Print "Volume sphere: (4/3)*pi*r^3"
Locate 16,42:Print "Correction, x < pi:"
Locate 18,42:Print "corr = (pi/2) * (1-x/pi)^2"
Locate 22,58:Print "Ce programme montre que l'amplitude d'une"
Locate 23,58:Print "onde sph‚rique d‚pend du volume de la"
Locate 24,58:Print "® couche d'oignon ¯ dans laquelle elle"
Locate 25,58:Print "peut se d‚ployer. La courbe noire n'est"
Locate 26,58:Print "pas tout … fait exacte ici parce que le"
Locate 27,58:Print "calcul devrait tenir compte d'une r‚par-"
Locate 28,58:Print "tition sinuso‹dale de l'‚nergie. Il faut"
Locate 29,58:Print "normaliser le volume v … 1 lorque x = 0."
Locate 30,58:Print "On calcule ici ce volume v d'une maniŠre"
Locate 31,58:Print "discutable, mais efficace."
Locate 31,2: Print "P - Pause."
Locate 32,2: Print "R - Ralenti - Slow."
Locate 33,2: Print "Pour quitter, appuyez sur ® Echap ¯. "
Locate 34,2: Print "Press Esc. to quit."
Locate 36,2: Print "Gabriel LaFreniŠre    glafreniere.com"
Locate 37,2: Print "Le 30 mai 2007.       May 30, 2007.";
If bitmap Then Sleep: If Inkey = Chr(13) Then Else bitmap = 0
yPrec = echelle
yBleuPrec = 2000

For xPixel = 0 To demiLargeur
  x = 2 * pi * xPixel / lambda                            'distance en radians.
  If xPixel > lambda / 4 Then                             'noyau onde entière à concilier.
    y = echelle / x                                       'courbe d'amplitude
    Line(xCentre + xPixel, yGraph - y)-(xCentre + xPixel, yGraph + y),blanc
    Line(xCentre - xPixel, yGraph - y)-(xCentre - xPixel, yGraph + y),blanc
    Line(xCentre - xPixel + 1, yGraph + yBleuPrec)-(xCentre - xPixel, yGraph + y), bleu
    Line(xCentre + xPixel - 1, yGraph + yBleuPrec)-(xCentre + xPixel, yGraph + y), bleu
    yBleuPrec = y
    y = echelle * Sin(x) / x                              'courbe de l'électron en rouge.
    Line(xCentre + xPixel - 1, yGraph - yPrec)-(xCentre + xPixel, yGraph - y), rouge
    Line(xCentre - xPixel + 1, yGraph - yPrec)-(xCentre - xPixel, yGraph - y), rouge
  Else
    If x Then y = echelle / x Else y = 2000: yBleuPrec = 2000  'courbe d'amplitude
    If xPixel > 8 Then
    Line(xCentre - xPixel + 1, yGraph + yBleuPrec)-(xCentre - xPixel, yGraph + y), bleu
    Line(xCentre + xPixel - 1, yGraph + yBleuPrec)-(xCentre + xPixel, yGraph + y), bleu
    End If
    yBleuPrec = y
    If xPixel Then y = echelle * Sin(x) / x Else y = echelle
    Line(xCentre + xPixel, yGraph - y)-(xCentre + xPixel, yGraph + y),blanc
    Line(xCentre - xPixel, yGraph - y)-(xCentre - xPixel, yGraph + y),blanc
    Line(xCentre + xPixel - 1, yGraph - yPrec)-(xCentre + xPixel, yGraph - y), rouge
    Line(xCentre - xPixel + 1, yGraph - yPrec)-(xCentre - xPixel, yGraph - y), rouge
    Line(xCentre + xPixel - 1, yGraph + yPrec)-(xCentre + xPixel, yGraph + y), rouge
    Line(xCentre - xPixel + 1, yGraph + yPrec)-(xCentre - xPixel, yGraph + y), rouge
  End If
  yPrec = y
Next

Line(0, yGraph)-(largeur, yGraph), gris                   'ligne centrale horiz.
Line(xCentre, yGraph-20)-(xCentre, yGraph+20), gris       'ligne centrale verticale.
For j = lambdaSurDeux To 4 * lambda / 2 Step lambda / 2   'repères.
  Line(xCentre+j, yGraph - 10)-(xCentre+j, yGraph + 10), gris
  Line(xCentre-j, yGraph - 10)-(xCentre-j, yGraph + 10), gris
Next
t = Timer
Return
