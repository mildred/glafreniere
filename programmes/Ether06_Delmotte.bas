Screen 19,24,1
Dim As Single pi = 4 * Atn(1), x, amplitude
fond = Rgb(225,225,225)
gabarit = 250
graphique = 300
precedentVert = graphique
precedentRouge = graphique
lambda = 80
Color noir, fond: Cls
Line(0,300)-(799,300),noir

For pixel = 0 To 799
  x = 2 * pi * (400 - pixel) / lambda
  
  If x Then amplitude = Sin(x) / x else amplitude = 1                'PHASE.
  
  Line(pixel, precedentVert)-(pixel, graphique - gabarit * amplitude), Rgb(0,200,0)
  precedentVert = graphique - gabarit * amplitude
  
  If x Then amplitude = 2 * (Sin(x / 2)) ^ 2 / x else amplitude = 0  'QUADRATURE
' If x Then amplitude = (1 - cos(x)) / x else amplitude = 0   'formule de M. Jocelyn Marcotte.
  
  Line(pixel, precedentRouge)-(pixel, graphique - gabarit * amplitude), Rgb(255,0,0)
  precedentRouge = graphique - gabarit * amplitude
Next

Locate 2, 3:  print "Formules de l'‚lectron selon M. Jocelyn Marcotte:"
Locate 4, 3:  print "Phase:       y = sin(x) / x"
Locate 5, 3:  print "Quadrature:  y = (1 - cos(x)) / x"
Locate 7, 3:  print "Avec:  x = 2 * pi * distance / lambda"
Locate 27, 3: print "M. Philippe Delmotte m'a avis‚ le 18 septembre"
Locate 28, 3: print "2006 que l'amplitude des ondes au moment de la"
Locate 29, 3: print "quadrature pouvait aussi ˆtre exprim‚e de la"
Locate 30, 3: print "maniŠre suivante:"
Locate 32, 3: print "  y = 2 * (Sin(x / 2)) ^ 2 / x"
Locate 37,3:? "Gabriel LaFreniŠre   glafreniere.com";
Locate 36,59:?"Le 19 sept. 2006. Ce programme peut ˆtre";
Locate 37,59:?"copi‚, modifi‚ ou distribu‚ librement.";
Sleep
