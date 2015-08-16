Dim As Single pi = 4 * Atn(1)
Dim As Single hauteur, longueur, AB, BC, AC, AF, BE, BG, CG, CJ, GJ, GL, JK, rayonSource
Dim As Single angle, anglePixel, angleSource, angleBalayage, arc, angleArc
page1 = 1: rouge = Rgb(255,0,0): bleu = Rgb(0,0,255)
Screen 19,24,3: Color noir, Rgb(225,225,225): Screenset 2,2: Cls

'          LE DISQUE D'AIRY - MÉTHODE PAR SOMMATION D'ARCS DE CERCLE ÉQUIDISTANTS.

rayonSphere = 150                                         'ce rayon détermine le foyer.
angleSource = 60 / (180 / pi)                             'angle de la calotte sphérique.
angleBalayage = 100 / (180 / pi)                          'angle de l'arc balayant la sphère.
xC = 210: yC = 200                                        'centre de la sphère.
xC2 = 590: yC2 = 200                                      'centre du plan orthogonal 2.
xC3 = xC2: yC3 = 394                                      'centre du plan orthogonal 3.
xC4 = xC2: yC4 = 570                                      'centre du plan orthogonal 4.
xP = xC + 90: yP = yC - 70                                'coordonnées initiales du pixel.
Line(xC - rayonSphere, yC)-(xC + rayonSphere, yC), noir   'axe optique.
Circle(xC, yC), rayonSphere, noir                         'sphère où se situe la source.
Circle(xC, yC), 3, noir                                   'centre de la sphère et foyer.
Circle(xC2, yC2), rayonSphere, noir, 0, pi                'demi-sphère du plan orthogonal 2.
Circle(xC2, yC2), 3, noir                                 'centre du plan orthogonal 2.
Circle(xC3, yC3), rayonSphere, noir, 0, pi                'demi-sphère du plan orthogonal 3.
Circle(xC3, yC3), 3, noir                                 'centre du plan orthogonal 3.
Circle(xC4, yC4), rayonSphere, noir, 0, pi                'demi-sphère du plan orthogonal 4.
Circle(xC4, yC4), 3, noir                                 'centre du plan orthogonal 4.
Line(xC, yC - rayonSphere)-(xC, yC + rayonSphere), noir   'axe vertical transversal.
Locate 1, 2: Print "Gabriel LaFreniŠre"
Locate 2, 2: Print "glafreniere.com"
Locate 3, 2: Print "Mai 2006": Locate 1
Locate, 27: Print "CALCUL DU DISQUE D'AIRY - Source circulaire sph‚rique."
Locate, 28: Print "M‚thode par sommation d'arcs de cercle ‚quidistants."
Locate, 35: Print "Rayon de la sphŠre ="; rayonSphere; " pixels."
Locate 13, 73: Print "C"
Locate 13, 28: Print "C"
Locate 25, 76: Print "J"
Locate 36, 76: Print "A"
Locate 23, 6 : Print "Plan montrant la calotte sph‚rique en rouge."
Locate 24, 7 : Print "Modifiez l'angle du pixel P ci-dessus:"
Locate 25, 11: Print "Angle de la source (bouton droit):"
Locate 14, 60: Print "Plan redress‚ comprenant AFC."
Locate 15, 58: Print "Modifiez l'angle de balayage:"
Locate 26, 60: Print "Plan orthogonal comprenant GJK."
Locate 37, 60: Print "Plan orthogonal comprenant ABGF.";

Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
'--------------------------------- SAISIE DES MODIFICATIONS ----------------------------------

  Getmouse xSouris, ySouris, , clic                       'saisie sourie.
  If ySouris < 0 Then                                     'initialiser (pointeur hors-fenêtre).
    angleSource = 60 / (180 / pi)
    angleBalayage = 100 / (180 / pi)
    xP = xC + 90: yP = yC - 70
  End If

  If clic = 1 Then
    If xSouris < 400 Then                                 'modifier l'emplacement du pixel.
      xPrecedent = xP: yPrecedent = yP
      xP = xSouris : yP = ySouris                         'coordonnées du pixel.
      If yP > yC Then yP = yC
      distance = Sqr((xP - xC) ^ 2 + (yC - yP) ^ 2)
      If distance > rayonSphere Then xP = xPrecedent: yP = yPrecedent
    Else                                                  'modifier l'angle de balayage.
      If ySouris <= yC2 Then x = xSouris - xC2
      If x > rayonSphere Then x = rayonSphere Else If x < -rayonSphere Then x = -rayonSphere
      angleBalayage = Acos(x / rayonSphere)               'angle balayant la source.
    End If
  Elseif clic = 2 Then
    x = xC - xSouris
    If Abs(x) < rayonSphere Then
      angleSource = Acos(x / rayonSphere)
    End If
  End If
  If xP = xC Then                                         'angle du pixel sur l'axe optique.
    anglePixel = pi / 2
  Elseif yP = yC Then
    If xP > xC Then anglePixel = 0 Else anglePixel = pi
  Else
    anglePixel = Atn((yC - yP) / (xP - xC))
  If xP - xC < 0 Then anglePixel = anglePixel + pi        'valider le deuxième quadrant.
  End If
If anglePixel = 0 Then anglePixel = .0000001              'éviter l'infini.
'----------------------------------- CALCUL DES DISTANCES ------------------------------------

  AF = rayonSphere * Sin(angleBalayage)                   'rayon de l'arc de balayage.
  If AF < 0 Then AF = 0'                                  'pour graphiques seulement.
  AC = rayonSphere * Cos(angleBalayage)                   'côté AC du triangle sur l'axe du pixel.
  BC = AC / Sin(anglePixel)                               'distance BC.
  AB = BC * Cos(anglePixel)                               'distance AB, plan 2.
  JK = rayonSphere * Sin(angleSource)                     'distance JK, plan 1.
  CJ = rayonSphere * Cos(angleSource)                     'distance CJ: du centre à la corde.
  BG = CJ / Sin(anglePixel)                               'distance BG.
  GJ = BG * Cos(anglePixel) + BC                          'distance GJ.
  If GJ > JK Then GJ = JK
  If GJ < -JK Then GJ = -JK
  GL = Sqr(JK ^ 2 - GJ ^ 2)
  angleArc = Atn(GL / (BG + AB))                          'détermine la longueur de l'arc.
  If BG + AB < 0 Then angleArc = angleArc + pi            'valider le deuxième quadrant.
  arc = AF * angleArc                                     'longueur de l'arc, but du programme.

'--------------------------------------- GRAPHIQUES ------------------------------------------

  Circle(xC, yC), rayonSphere,   rouge, pi - angleSource, pi + angleSource 'calotte sphérique.
  Circle(xC, yC), rayonSphere+1, rouge, pi - angleSource, pi + angleSource 'selon l'angle.
  
  Circle(xP, yP), 3, noir                                 'point P - Emplacement du pixel.
  xLettreP = xP / 8: yLettreP = yP / 16
  Locate yLettreP, xLettreP: Print "P";

  xCercle = xC + rayonSphere * Cos(anglePixel)'           'point de la sphère sur l'axe du pixel.
  yCercle = yC - rayonSphere * Sin(anglePixel)
  Circle(xCercle, yCercle), 3, noir
  Line(xCercle, yCercle)-(xC, yC), bleu                   'axe du pixel, du centre à la sphère.

  xA = xC + AC * Cos(anglePixel)                          'point A au centre du plan 1.
  yA = yC - AC * Sin(anglePixel)
  Circle(xA, yA), 3, noir
  xLettreA = xA / 8 - 1: yLettreA = yA / 16 + 1
  Locate yLettreA, xLettreA: Print "A"

  xK = xC - CJ                                            'extrémités de la calotte sphérique.
  yK = yC - JK
  Circle(xK, yK), 3, noir
  xLettreK = xK / 8: yLettreK = yK / 16
  Locate yLettreK, xLettreK: Print "K";
  yH = yC + rayonSphere * Sin(angleSource)
  xH = xK
  Line(xK, yK)-(xH, yH), rouge                            'KH - Diamètre de la calotte 
  
  xJ = xC - CJ                                            'centre de la corde de la calotte.
  yJ = yC
  xLettreJ = xJ / 8 - 1: yLettreJ = yJ / 16 + .5
  Locate yLettreJ, xLettreJ: Print " J ";

  xG = xC - rayonSphere * Cos(angleSource)                'point G du plan 1.
  yG = yC - GJ
  xLettreG = xG / 8: yLettreG = yG / 16
  Locate yLettreG, xLettreG - 1: Print "G";
  Circle(xG, yG), 3, noir
  Line(xG, yG)-(xC, yC), rouge
  
  xB = xC: yB = yC - BC                                   'point B sur l'axe transversal.
  Circle(xB, yB), 3, noir
  xLettreB = xB / 8: yLettreB = yB / 16
  If yLettreB > 0 And yLettreB < 26 Then Locate yLettreB, xLettreB: Print "B";

  Line (xB, yB)-(xC, yC), rouge                           'traçé du triangle ABC.
  Line (xC, yC)-(xA, yA), rouge
  If BC > rayonSphere Then Else Line (xA, yA)-(xB, yB), rouge

  xF = xA + AF * Cos(anglePixel + pi / 2)                 'point F, rayon de l'arc de balayage.
  yF = yA - AF * Sin(anglePixel + pi / 2)
  Circle(xF, yF), 3, noir
  xLettreF = xF / 8: yLettreF = yF / 16
  Locate yLettreF, xLettreF: Print "F"
  xE = xA + AF * Cos(anglePixel + 3 * pi / 2)             'point E, rayon de l'arc de balayage.
  yE = yA - AF * Sin(anglePixel + 3 * pi / 2)
  Circle(xE, yE), 3, noir
  Line (xF, yF)-(xE, yE), bleu                            'rayon de l'arc de balayage.

  xF = xC4 - AF: yF = yC4                                 'point F du plan 4.
  Circle(xF, yF), 3, noir
  xLettreF = xF / 8: yLettreF = yF / 16
  Locate yLettreF, xLettreF: Print "F"

  xF = xC2 + rayonSphere * Cos(angleBalayage)             'point F du plan 2.
  yF = yC2 - rayonSphere * Sin(angleBalayage)
  Circle(xF, yF), 3, noir
  Line (xF, yF)-(xC2, yC2), bleu                          'indique l'angle de balayage.
  xLettreF = xF / 8 + 1: yLettreF = yF / 16
  Locate yLettreF, xLettreF: Print "F"

  xA = xF                                                 'point A du plan 2.
  yA = yC2
  Line(xC2 - rayonSphere, yC2)-(xC2 + rayonSphere, yC2), noir 'axe horizontal du plan 2.
  Line(xC2, yC2 - rayonSphere)-(xC2, yC2), noir           'axe vertical du plan 2.
  Line (xA, yA)-(xF, yF), bleu                            'côté vertical du triangle ACD.
  Circle(xA, yA), 3, noir
  xLettreA = xA / 8: yLettreA = 13
  Locate yLettreA, xLettreA: Print "A"
  
  Circle(xC3, yC3), JK, noir, 0, pi                       'arc de la source, plan 3.
  Line(xC3 - JK, yC3)-(xC3 + JK, yC3), noir               'axe horizontal du plan 3.
  
  xK = xC3 - JK                                           'point K du plan 3.
  yK = yC3
  Circle(xK, yK), 3, noir
  xLettreK = xK / 8: yLettreK = yK / 16
  Locate yLettreK, xLettreK + 2: Print "K"
  
  xL = xC3 - GJ                                           'point L du plan 3.
  yL = yC3 - GL
  Circle(xL, yL), 3, noir
  xLettreL = xL / 8: yLettreL = yL / 16
  Locate yLettreL, xLettreL: Print "L"
  
  xG = xL                                                 'point G du plan 3.
  yG = yC3
  Circle(xG, yG), 3, noir
  xLettreG = xG / 8: yLettreG = yG / 16
  Line(xG, yG)-(xL, yL), noir                             'GL - Croisement des deux cercles.
  Locate yLettreG, xLettreG+1: Print "G"
  
  Circle(xC4, yC4), AF, noir, 0, pi                       'arc de balayage, plan 4.
  Circle(xC4, yC4), rayonSphere, noir, 0, pi              'superposer la sphère.
  Line(xC4 - AF, yC4)-(xC4 + AF, yC4), noir               'axe horizontal du plan 4.
  
  Circle(xC4, yC4), AF, rouge, pi - angleArc, pi          'arc effectif, plan 4.
  Circle(xC4, yC4), AF + 1, rouge, pi - angleArc, pi
  angleBalayageDegres = angleBalayage * 180 / pi
  Locate 15, 87: Print angleBalayageDegres; chr(248)
  anglePixelDegres = anglePixel * 180 / pi
  Locate 24, 45: Print anglePixelDegres; chr(248)
  angleSourceDegres = angleSource * 180 / pi
  Locate 25, 45: Print angleSourceDegres; chr(248)

'----------------------------------- AFFICHER LES CALCULS ------------------------------------

  Locate 27,2: Print "AF ="; AF;: Locate, 20: Print "rayonSphere * Sin(angleBalayage)"
  Locate 28,2: Print "AC ="; AC;: Locate, 20: Print "rayonSphere * Cos(angleBalayage)"
  Locate 29,2: Print "BC ="; BC;: Locate, 20: Print "AC / Sin(anglePixel)"
  Locate 30,2: Print "AB ="; AB;: Locate, 20: Print "BC * Cos(anglePixel)"
  Locate 31,2: Print "JK ="; JK;: Locate, 20: Print "rayonSphere * Sin(angleSource)"
  Locate 32,2: Print "CJ ="; CJ;: Locate, 20: Print "rayonSphere * Cos(angleSource)"
  Locate 33,2: Print "BG ="; BG;: Locate, 20: Print "CJ / Sin(anglePixel)"
  Locate 34,2: Print "GJ ="; GJ;: Locate, 20: Print "BG * Cos(anglePixel) + BC"
  Locate 35,2: Print "GL ="; GL;: Locate, 20: Print "Sqr(JK ^ 2 - GJ ^ 2)"
  angleArcdegres = angleArc * 180 / pi
  Locate 36, 2: Print "Angle (arc):"; angleArcdegres; chr(248);
  Locate, 20: Print "atn(GL / (BG + AB))"
  arcEntier = arc
  Locate 37, 2: Print "Arc:  "; arcEntier;: Locate, 20: Print "AF * angleArc"
Loop Until Len(inkey)
