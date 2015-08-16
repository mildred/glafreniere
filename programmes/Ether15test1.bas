page1 = 1
rayon1 = 200
rayon2 = 300
yCentre = 303
xCentre1 = 250
xCentre2 = 500
color 0, 11: cls: locate 2
print " Ce programme explique la proc‚dure … suivre pour distribuer des ondelettes"
print " de Huygens sur la source, sous forme de cercles concentriques. Cette"
print " m‚thode trŠs rapide est utilis‚e dans le programme Ether15."
Screen 19,24,3
fond = Rgb(225,225,225)
Dim As Single pi = 4 * Atn(1)
Dim as Single AB, BC, AC, CD, BCA
Color noir, fond: Screenset 2,2: Cls
locate 33
locate, 2: print "En vertu du principe de Huygens, on peut r‚partir uniform‚ment un grand nombre d'ondelettes sur"
locate, 2: print "la surface de la source circulaire. Mais cette m‚thode est trŠs lente parce qu'il faut calculer"
locate, 2: print "chacune des ondelettes. On peut avantageusement balayer la surface de la source avec des arcs de"
locate, 2: print "cercle successifs, dont le rayon croŒt progressivement. La puissance de chaque ondelette est"
locate, 2: print "proportionnelle … la longueur de chaque arc de cercle, qu'il s'agit donc d'‚tablir d'abord.";
Do'    LA DIFFRACTION DE FRESNEL - CALCUL D'UN ARC DE CERCLE SELON LE THÉORÈME D'AL KASHI.
  
  Getmouse xSouris, ySouris, , clic
  if xSouris > xCentre1 + rayon1 + rayon2 then xSouris = xCentre1 + rayon1 + rayon2
  if xSouris < xCentre1 + (rayon2 - rayon1) then xSouris = xCentre1 + (rayon2 - rayon1)
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  Circle(xCentre1, yCentre), 2, noir
  Circle(xCentre2, yCentre), 2, noir
  Circle(xCentre1, yCentre), rayon1, noir
  Circle(xCentre2, yCentre), rayon2, noir
  AB = rayon1: BC = rayon2: AC = xCentre2 - xCentre1
  BCA = Acos((AC ^ 2 + BC ^ 2 - AB ^ 2) / (2 * AC * BC))
  angle = BCA * 180 / pi
  CD = BC * Cos(BCA)
  xD = xCentre2 - CD
  BD = rayon2 * Sin(BCA)
  yB = yCentre - BD
  Line(xCentre2, yCentre)-(xCentre2 - CD, yCentre), rgb(255,0,0)
  Line(xD, yB)-(xD, yCentre), rgb(0,0,255)
  Line(xD, yB)-(xCentre2, yCentre), rgb(0,175,0)
  Line(xCentre1, yCentre)-(xD, yB), rgb(200, 0, 200)
  Locate 20, 31: Print "A"
  xAngleB = (xCentre2 - CD) / 8
  yAngleB = (yB) / 16
  Locate yAngleB,  xAngleB: Print "B"
  xAngleC = xCentre2 / 8
  Locate 20, xAngleC: Print "C"
  xAngleE = (xCentre2 - rayon2) / 8 - 1
  Locate 19, xAngleE: Print "E"
  Line(0, yCentre)-(799, yCentre), noir
  locate 1
  locate, 2: print "Vous pouvez d‚placer le centre du grand cercle avec la souris."
  locate, 2: print "Rayon du grand cercle:"; rayon2; " pixels. Rayon de la source:"; rayon1
  locate, 2: print "Angle BCA = Acos((AC ^ 2 + BC ^ 2 - AB ^ 2) / (2 * AC * BC)) = "; angle;chr(248); " (loi des cosinus)."
  locate, 2: print "Longueur de l'arc de cercle EB:"; rayon2 * BCA; " pixels: L = rayon * angle BCA en radians."
  print
  locate, 70: print "glafreniere.com"
  if clic = 1 then  xCentre2 = xSouris else if clic = -1 then xCentre2 = 500
Loop Until Len(inkey)
