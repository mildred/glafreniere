Dim As Double distance
Dim As Single differenceDeMarche, conversion, phase, lambda, deuxPi, yCarre, zCarre
Screen 19,24,1: Gosub Initialisation

'     par Gabriel LaFreni�re, le 20 juin 2007.



For y = 0 To rayon                                        'coordonn�e y en pixels.
  yCarre = (y * conversion)^2                             'y au carr�, en millim�tres.
  For z = 0 To rayon                                      'coordonn�e z en pixels.
    zCarre = (z * conversion)^2                           'z au carr�, en millim�tres.
    If Sqr(yCarre + zCarre) > rayon * conversion Then Exit For'pixel hors-source.
    distance = sqr(xCarre + yCarre + zCarre)              'distance absolue en millim�tres.
    differenceDeMarche = distance - x
    phase = deuxPi * differenceDeMarche / lambda          'p�riode comparativement au centre.
    If Sin(phase) < 0 Then
         couleur = noir                                   'interf�rences destructives en noir.
    Else couleur = blanc                                  'interf�r. constructives en blanc.
    End If
    Pset(yCentre + y, zCentre + z), couleur               'afficher les quatre quadrants.
    Pset(yCentre - y, zCentre + z), couleur
    Pset(yCentre + y, zCentre - z), couleur
    Pset(yCentre - y, zCentre - z), couleur
  Next
Next
Sleep: End

'------------------------------------- INITIALISATION ----------------------------------------
Initialisation:
deuxPi = 8 * Atn(1)                                       '�quivaut � 8 fois 45� en radians.
lambda = .00055                                           'vert, couleur la plus visible.
rayon = 250                                               'rayon de la source en pixels.
rayonSource = 5                                           'rayon en millim�tres: Diam. = 1 cm.
conversion = rayonSource / rayon                          'convertir y et z en millim�tres.
x = 2000                                                  'distance au foyer (focale).
xCarre = x^2
fond = Rgb(225,225,225)
blanc = Rgb(255,255,255)
yCentre = 540                                             'centre de l'axe horizontal.
zCentre = rayon + 10                                      'centre de l'axe vertical. 
Windowtitle "La lentille diffractive (ou r�seau de Soret)."
Color noir, fond
Cls: Color Rgb(0,150,0)
Locate 36, 2: Print "Gabriel LaFreni�re  glafreniere.com";
Locate 37, 2: Print "Le code source (voir freebasic.net) peut �tre distribu�, copi� ou modifi� en toute libert�.";
Color noir
Locate 34, 2: Print "Pour quitter, appuyez sur une touche."
Return
