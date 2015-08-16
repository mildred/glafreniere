Dim As Single pi = 4 * Atn(1)
Dim As Single hauteur, longueur, AB, BC, AC, BE, xA, yA, xB, yB, xC, yC
Dim As Single angle, anglePixel, angleSource, angleBalayage, arc, angleArc, rayonArc
page1 = 1: rouge = Rgb(255,0,0): bleu = Rgb(0,0,255)
Screen 19,24,3: Color noir, Rgb(225,225,225): Screenset 2,2: Cls

' LE DISQUE D'AIRY - MÉTHODE PAR SOMMATION D'ARCS DE CERCLE CONCENTRIQUES.

rayonSource = 150                                         'ce rayon détermine le foyer.
angleBalayage = 70 / (180 / pi)                           'angle de l'arc balayant la source.
xC = 210: yC = 200                                        'centre de la sphère.
xC2 = 590: yC2 = 200                                      'centre du plan orthogonal 2.
xC3 = 590: yC3 = 448                                      'centre du plan orthogonal 3.
xP = xC + 90: yP = yC - 70                                'coordonnées initiales du pixel.
Line(xC - rayonSource, yC)-(xC + rayonSource, yC), noir   'axe optique.
Circle(xC, yC), 3, noir                                   'centre de la sphère et foyer.
Circle(xC2, yC2), 3, noir                                 'centre du plan orthogonal 2.
Circle(xC2, yC2), rayonSource, noir, 0, pi                'sphère du plan orthogonal 2.
Circle(xC3, yC3), 3, noir                                 'centre du plan orthogonal 3.
Circle(xC, yC), rayonSource, noir                         'sphère où se situe la source.
Circle(xC, yC), rayonSource+1, rouge, pi / 2, pi + pi / 2 'source: un hémisphère (à gauche).
Circle(xC, yC), rayonSource, rouge, pi / 2, pi + pi / 2   'accentuer la source.
Line(xC, yC - rayonSource)-(xC, yC + rayonSource), noir   'axe vertical transversal.
Locate 1, 2: 
Locate, 30: Print "CALCUL DU DISQUE D'AIRY - Source h‚misph‚rique."
Locate, 27: Print "M‚thode par sommation d'arcs de cercles concentriques."
Locate 37, 72: Print "Mai 2006 - glafreniere.com";
Locate 14, 26: Print "C"
Locate 12, 73: Print "C"
Locate 28, 75: Print "A"
Locate 23, 6 : Print "Plan montrant la calotte sph‚rique en rouge."
Locate 24, 8 : Print "Modifiez l'angle du pixel ci-dessus:"
Locate 14, 60: Print "Plan orthogonal comprenant AC."
Locate 15, 56: Print "Modifiez l'angle de balayage ci-dessus."
Locate 30, 60: Print "Plan orthogonal comprenant AF."
Locate 30, 2 : Print "Rayon de la sphŠre ="; rayonSource; " pixels."

Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1

  Getmouse xSouris, ySouris, , clic                       'saisie sourie.
  If ySouris < 1 Then xP = xC + 90: yP = yC - 70 : angleBalayage = 70 / (180 / pi): clic = 0 'initialiser.
  If clic = 1 Then
    If xSouris < 400 Then                                 'modifier l'emplacement du pixel.
      xPrecedent = xP: yPrecedent = yP
      xP = xSouris : yP = ySouris                         'coordonnées du pixel.
      If yP > yC Then yP = yC
      distance = Sqr((xP - xC) ^ 2 + (yC - yP) ^ 2)
      If distance > rayonSource Then xP = xPrecedent: yP = yPrecedent
    Else                                                  'modifier l'angle de l'arc de balayage.
      x = xSouris - xC2: y = yC2 - ySouris
      If y < 0 Then y = 0
      If x = 0 Then
        angleBalayage = pi / 2
      Else angleBalayage = Atn(y / x)                     'angle du pixel sur l'axe optique.
      End If
      If x < 0 Then angleBalayage = angleBalayage + pi    'valider le deuxième quadrant.    
    End If
  End If

  Circle(xP, yP), 3, noir                                 'emplacement du pixel.
  xLettreP = xP / 8: yLettreP = yP / 16
  Locate yLettreP, xLettreP: Print "P";
  If xP = xC Then
    anglePixel = pi / 2
  Else anglePixel = Atn((yC - yP) / (xP - xC))            'angle du pixel sur l'axe optique.
  End If
  If xP - xC < 0 Then anglePixel = anglePixel + pi        'valider le deuxième quadrant.
  rayonArc = rayonSource * Sin(angleBalayage)
  If rayonArc < 0 Then rayonArc = 0

  xCercle = xC + rayonSource * Cos(anglePixel)'           'point de la sphère sur l'axe du pixel.
  yCercle = yC - rayonSource * Sin(anglePixel)
  Circle(xCercle, yCercle), 3, noir
  Line(xCercle, yCercle)-(xC, yC), bleu                   'axe du pixel, du centre à la sphère.

  AC = rayonSource * Cos(angleBalayage)                   'côté du triangle sur l'axe du pixel.
  xA = xC + AC * Cos(anglePixel)                          'point A au centre du plan 1.
  yA = yC - AC * Sin(anglePixel)
  Circle(xA, yA), 3, noir
  xLettreA = xA / 8: yLettreA = yA / 16
  Locate yLettreA, xLettreA: Print "A"

  If anglePixel = 0 Or anglePixel = pi Then
    If angleBalayage > pi / 2 Then BC = -rayonSource Else BC = rayonSource
  Else
    BC = AC / Sin(anglePixel)
  End If
  If BC > rayonSource Then BC = rayonSource Else If BC < -rayonSource Then BC = -rayonSource 

  xB = xC: yB = yC - BC                                   'point B sur l'axe transversal.
  Circle(xB, yB), 3, noir
  xLettreB = xB / 8: yLettreB = yB / 16
  If yLettreB > 0 And yLettreB < 26 Then Locate yLettreB, xLettreB: Print "B";

  xF = xA + rayonArc * Cos(anglePixel + pi / 2)           'point sur l'arc de balayage.
  yF = yA - rayonArc * Sin(anglePixel + pi / 2)
  Circle(xF, yF), 3, noir
  xLettreF = xF / 8: yLettreF = yF / 16
  Locate yLettreF, xLettreF: Print "F"
  xE = xA + rayonArc * Cos(anglePixel + 3 * pi / 2)       'point E de l'arc de balayage.
  yE = yA - rayonArc * Sin(anglePixel + 3 * pi / 2)
  Circle(xE, yE), 3, noir
  Line (xF, yF)-(xE, yE), bleu                            'arc de balayage (à mesurer).

  Line (xB, yB)-(xC, yC), rouge                           'traçé du triangle ABC.
  Line (xC, yC)-(xA, yA), rouge
  If BC > rayonSource Then Else Line (xA, yA)-(xB, yB), rouge

  xD = xC2 + rayonSource * Cos(angleBalayage)             'point D du plan 2.
  yD = yC2 - rayonSource * Sin(angleBalayage)
  Circle(xD, yD), 3, noir
  Line (xD, yD)-(xC2, yC2), bleu                          'indique l'angle de balayage.
  xLettreD = xD / 8 + 1: yLettreD = yD / 16
  Locate yLettreD, xLettreD: Print "D"

  xA = xD                                                 'point A du plan 2.
  yA = yC2
  Line(xC2 - rayonSource, yC2)-(xC2 + rayonSource, yC2), noir 'axe horizontal du plan 2.
  Line(xC2, yC2 - rayonSource)-(xC2, yC2), noir           'axe vertical du plan 2.
  Line (xA, yA)-(xD, yD), bleu                            'côté vertical du triangle ACD.
  Circle(xA, yA), 3, noir
  xLettreA = xA / 8: yLettreA = 13
  Locate yLettreA, xLettreA: Print "A"

  Circle(xC3, yC3), rayonArc, noir, 0, pi                 'arc de balayage, plan 3.
  Circle(xC3, yC3), rayonSource, noir, 0, pi              'superposer la sphère.
  Line(xC3 - rayonArc, yC3)-(xC3 + rayonArc, yC3), noir   'axe horizontal du plan 3.
  
  BE = Sqr(rayonSource ^ 2 - BC ^ 2)
  AB = BC * Cos(anglePixel)                               'côté du triangle, plan 2.
  xB = xC3 - AB: yB = yC3                                 'point B
  Circle(xB, yB), 3, noir
  xLettreB = xB / 8: yLettreB = yB / 16
  Locate yLettreB, xLettreB: Print "B";

  xE = xB: yE = yC3 - BE                                  'point E
  Circle(xE, yE), 3, noir
  Line(xE, yE)-(xB, yB), rouge
  Line(xE, yE)-(xC3, yC3), rouge
  xLettreE = xE / 8: yLettreE = yE / 16
  Locate yLettreE, xLettreE: Print "E";

  If AB Then angleArc = Atn(BE / AB) Else angleArc = pi / 2
  If AB < 0 Then angleArc = angleArc + pi                 'valider le deuxième quadrant.
  angleArcdegres = angleArc * 180 / pi
  Locate 31, 66: Print "Angle de l'arc:"; angleArcdegres; chr(248)
  arc = rayonArc * angleArc
  Locate 32, 63: Print "Longueur de l'arc:"; arc
  Circle(xC3, yC3), rayonArc, rouge, pi - angleArc, pi    'arc effectif, plan 3.
  Circle(xC3, yC3), rayonArc + 1, rouge, pi - angleArc, pi
  angleBalayageDegres = angleBalayage * 180 / pi
  Locate 16, 64: Print "Angle de balayage:"; angleBalayageDegres; chr(248)
  anglePixelDegres = anglePixel * 180 / pi
  Locate 24, 44: Print anglePixelDegres; chr(248)

'-------------------------------------- RÉSUMÉ DU CALCUL -------------------------------------
  AC = rayonSource * Cos(angleBalayage)
  If anglePixel = 0 Or anglePixel = pi Then
    If angleBalayage > pi / 2 Then BC = -rayonSource Else BC = rayonSource
  Else
    BC = AC / Sin(anglePixel)
  End If
  If BC > rayonSource Then BC = rayonSource Else If BC < -rayonSource Then BC = -rayonSource 
  AB = BC * Cos(anglePixel)
  BE = Sqr(rayonSource ^ 2 - BC ^ 2)
  If AB Then angleArc = Atn(BE / AB) Else angleArc = pi / 2
  If AB < 0 Then angleArc = angleArc + pi                 'valider le deuxième quadrant.
  rayonArc = rayonSource * Sin(angleBalayage)
  If rayonArc < 0 Then rayonArc = 0
  arc = rayonArc * angleArc

  Locate 31,2: Print "AC ="; AC
  Locate 32,2: Print "BC ="; BC
  Locate 33,2: Print "AB ="; AB
  Locate 34,2: Print "BE ="; BE 
  Locate 35,2: Print "Angle de l'arc"; angleArc * 180 / pi 
  Locate 36,2: Print "Rayon de l'arc:"; rayonArc 
  Locate 37,2: Print "Longueur de l'arc:"; arc;
  If arc < 2 Then Print " pixel."; Else Print " pixels.";
Loop Until Len(inkey)
