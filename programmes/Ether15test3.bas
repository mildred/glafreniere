Dim As Single pi = 4 * Atn(1), AB, BC, AC, arc, angle, phase, distance, xDistance, yDistance
Dim As Single affaiblissement, differenceDeMarche, amplitudeSinus, amplitudeCosinus
hauteur = 300: longueur = 799: rayon = 120: AB = rayon    'le rayon est le côté AB du triangle.
yCentre = hauteur / 2: pas = 2 * AB / 100: zoom = 5
lambda = 4: fond = Rgb(225,225,225): blanc = rgb(255,255,255)
Screen 19,24,1: Color noir, fond: Cls 'Le 14 avril 2006.  Gabriel LaFrenière glafreniere.com
' LA DIFFRACTION DE FRESNEL - MÉTHODE PAR SOMMATION D'ARCS DE CERCLE CONCENTRIQUES.
For x = 0 To longueur
  xDistance = (zoom * x) / lambda                         'zoom horizontal 4x.
  xCarre = xDistance ^ 2                                  'distance en longueurs d'onde,
  For y = 0 To yCentre                                    '                         au carré.
    amplitudeSinus = 0: amplitudeCosinus = 0              'initialiser à chaque pixel.
    AC = y: BC = pas / 2                                  'côtés d'un triangle non rectangle.
    Do
      angle = Acos((AC ^ 2 + BC ^ 2 - AB ^ 2) / (2 * AC * BC))'loi des cosinus, ou théorème
      If AC > AB + BC Then                                '                       d'Al Kashi.
        arc = 0: angle = 0                                'cercle entièrement à l'extérieur.
      Elseif AC < AB - BC Then
        arc = pi * BC                                     'cercle entièrement à l'intérieur.
      Else
        arc = BC * angle                                  'arc de cercle à l'intérieur.
      End If
      yDistance = BC / lambda                             'dist. transv. en longueurs d'onde.
      distance = Sqr(xCarre + yDistance ^ 2)              'distance de l'arc au pixel.
      differenceDeMarche = distance - Int(distance)       'une longueur d'onde au maximum.
      phase = 2 * pi * differenceDeMarche                 'convertir la distance en période.
      If distance < .5 Then distance = .5                 'éviter la division par zéro.
      affaiblissement = arc * (40 / distance)             'luminosité selon terme correcteur.
      amplitudeSinus = amplitudeSinus + affaiblissement * Sin(phase)'sommation des ondelettes.
      amplitudeCosinus = amplitudeCosinus + affaiblissement * Cos(phase)'Cos pour quadrature.      
      BC = BC + pas                                       'cercle concentrique suivant.
    Loop While BC < AC + AB                               'le cercle maximum est atteint.
    amplitude = Sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)'Pythagore.
    ton = amplitude: If ton > 255 Then ton = 255
    Pset(x,yCentre - y), Rgb(ton, ton, ton)               'échelle de gris selon l'amplitude.
    Pset(x,yCentre + y), Rgb(ton, ton, ton)               'l'énergie vaut le carré de l'amplitude.
    If x = longueur Then
      Line(560 + y, 556 - .38 * amplitude)-(560 + y, 556), blanc
      Line(560 - y, 556 - .38 * amplitude)-(560 - y, 556), blanc
      Pset(560 + y, 556 - .38 * amplitude), noir           'disque d'Airy en voie de formation.
      Pset(560 - y, 556 - .38 * amplitude), noir
    End If
  Next
  If Len(inkey) Then Exit For
Next
'------------ EMPLACEMENT DES ZONES CLAIRES ET SOMBRES SELON LE NOMBRE DE FRESNEL ------------
for n = 1 to 8
  distance = rayon ^ 2 / (n * lambda)
  line(distance / zoom, hauteur - 140)-(distance / zoom, hauteur + 18), noir
next
Line(560 - hauteur / 2, 556)-(560 + hauteur / 2, 556), noir 'base du graphique 2D.
Locate 21, 8:  Print "n = 8 7 6  5   4       3              2          L = R ^ 2 / (n * lambda)          1"
print
Print " LA DIFFRACTION DE FRESNEL. Source lumineuse circulaire plane et ‚quiphas‚e (laser, st‚nop‚)."
Print " M‚thode des arcs de cercle concentriques, selon le principe de Huygens. La distance des zones de"
Print " rayonnement maximum et minimum, sur l'axe, correspond … un entier n: c'est le nombre de Fresnel."
Print " J'affirme que ces zones ont un lien avec les raies spectrales et la structure des atomes. Le"
Print " rayonnement des champs de force pr‚sente en effet, sur l'axe central, des zones trŠs semblables"
Print " … celles qui sont montr‚es ci-dessus."
print
Print " Il faut souligner que la p‚riode des"
Print " ondes qui circulent le long de cet axe"
Print " ‚volue de maniŠre … justifier aussi la"
Print " force de Coulomb. Quelle que soit la "
Print " distance qui les s‚pare, deux ‚lectrons"
Print " ou positrons sont toujours en pr‚sence"
Print " d'ondes en provenance du champ de force,"
print " et dont la p‚riode ne varie pas.";
Locate 36, 44: Print " A droite, la tache d'Airy est en voie de formation."
Locate 37, 44: Print "Le 15 avril 2006.  Gabriel LaFreniŠre   glafreniere.com";
Sleep