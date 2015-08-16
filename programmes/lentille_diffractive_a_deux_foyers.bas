Dim As Double distance
Dim As Single rayon, pi, deuxPi, yCarre, zCarre, conversion
Dim As Single lambda, differenceDeMarche, phase1, phase2
Screen 19,24,1: Gosub Initialisation

'     par Gabriel LaFrenière, le 20 juin 2007.

For y = 0 To rayon                                        'coordonnée y en pixels.
  yCarre = (y * conversion)^2                             'y au carré, en millimètres.
  For z = 0 To rayon                                      'coordonnée z en pixels.
    zCarre = (z * conversion)^2                           'z au carré, en millimètres.
    If Sqr(yCarre + zCarre) > rayon * conversion Then Exit For'pixel hors-source.
    distance = Sqr(x1Carre + yCarre + zCarre)             'distance absolue en millimètres.
    differenceDeMarche = distance - x1
    phase1 = deuxPi * differenceDeMarche / lambda
    distance = Sqr(x2Carre + yCarre + zCarre)
    differenceDeMarche = distance - x2
    phase2 = deuxPi * differenceDeMarche / lambda
    If Sin(phase1) < 0 Then
         couleur = noir                                   'interférences destructives en noir.
    Else couleur = blanc                                  'interfér. constructives en blanc.
    End If
    If Sin(phase2) < 0 Then couleur = noir
    Pset(yCentre + y, zCentre + z), couleur               'afficher les quatre quadrants.
    Pset(yCentre - y, zCentre + z), couleur
    Pset(yCentre + y, zCentre - z), couleur
    Pset(yCentre - y, zCentre - z), couleur
  Next
Next

Sleep: End

'------------------------------------- INITIALISATION ----------------------------------------
Initialisation:
deuxPi = 8 * Atn(1)                                       'équivaut à 8 fois 45° en radians.
lambda = .00055                                           'vert, couleur la plus visible.
lambdaSurDeux = lambda / 2
rayon = 250                                               'rayon de la source en pixels.
rayonSource = 5                                           'rayon en millimètres: Diam. = 1 cm.
conversion = rayonSource / rayon                          'convertir y et z en millimètres.
x1 = 2000                                                 'premier foyer à un mètre (1000 mm).
x2 = 4000                                                 'deuxième foyer à deux mètres.
x1Carre = x1^2
x2Carre = x2^2
fond = Rgb(225,225,225)
blanc = Rgb(255,255,255)
yCentre = 540                                             'centre de l'axe horizontal.
zCentre = rayon + 10                                      'centre de l'axe vertical. 
Windowtitle "La lentille diffractive à deux foyers."
Color noir, fond
Cls: Color Rgb(0,150,0)
Locate 36, 2: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 37, 2: Print "Le code source (voir freebasic.net) peut ˆtre distribu‚, copi‚ ou modifi‚ en toute libert‚.";
Color noir
Locate 34, 2: Print "Pour quitter, appuyez sur une touche."
Return
