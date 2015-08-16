Screen 19,24,1
Dim as Single amplitude, amplitudeSinus, amplitudeCosinus, angle
Dim As Single differenceDeMarche, lambda, affaiblissement, luminance
Dim As Single pi, xDistance, yDistance, distance, phase, phase1, phase2
pi = 4 * atn(1): lambda = 14: hauteur = 274

' <<<<<<<<<< LA DIFFRACTION DE FRESNEL >>>>>>>>>>

for x = 0 to 800                                          'balayage horizontal sur 800 pixels.
  xDistance = (x + 1) / lambda 'éviter zéro               'distance x en longueurs d'onde.
  differenceDeMarche = xdistance - int(xdistance)
  phase1 = 2 * pi * differenceDeMarche
  for y = 0 to hauteur                                    'balayage vertical sur 280 pixels.
    amplitudeSinus = 0                                    'initialiser à chaque pixel.
    amplitudeCosinus = 0                                  'utile pour le calcul de l'énergie.

'*************************** CALCUL D'APRÈS LE PRINCIPE DE HUYGENS ***************************

    for ySource = .2 * hauteur to .8 * hauteur            'on produit une ondelette par pixel.
      yDistance = (ySource - y) / lambda'                 'distance y en longueurs d'onde.
      distance = sqr(xDistance ^ 2 + yDistance ^ 2)       'distance de la source au pixel.
      affaiblissement = sqr(lambda / distance)                 'l'amplitude faiblit avec la distance
      differenceDeMarche = distance - int(distance)'      'une longueur d'onde au maximum.
      phase = 2 * pi * differenceDeMarche                 'convertir la distance en période.
      amplitudeSinus = amplitudeSinus + affaiblissement * Sin(phase)    'somme des ondelettes.
      amplitudeCosinus = amplitudeCosinus + affaiblissement * Cos(phase)'quadrature.
    next
'************************************** FIN DU CALCUL ****************************************

    phase2 = atn(amplitudeSinus / amplitudeCosinus)       'retrouver la période des ondes.
    if amplitudeCosinus < 0 then phase2 = phase2 + pi     'valider les quatre quadrants.
    ' L'énergie s'évalue d'après le sinus et le cosinus selon le théorème de Pythagore,
    ' mais il faut éviter la racine carrée pour obtenir l'amplitude. 
    luminance = .1 * (amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
    gris = .5 * luminance
    if gris > 255 then gris = 255
    pset(x, y), rgb(gris, gris, gris)                     'diagramme en échelle de gris.
    phase = phase1 - phase2                               'varie sur 1/4 d'onde semble-t-il...
    amplitude = luminance * sin(phase2)
    if amplitude > 0 then                                 'diagramme en couleurs.
      rouge = amplitude
      bleu = .5 * amplitude
      if amplitude > 255 then vert = amplitude - 255 else vert = 0
    else
      vert = -amplitude
      bleu = .5 * -amplitude
      if amplitude < -255 then rouge = -amplitude - 255 else rouge = 0
    end if
    if rouge > 255 then rouge = 255
    if vert > 255 then vert = 255
    if bleu > 255 then bleu = 255
    pset(x, y + hauteur + 60), rgb(rouge, vert, bleu)
    if y = hauteur / 2 then                               'graphique central.
      if cos(phase) > 0 then rouge = 255 * cos(phase) else rouge = 0
      if sin(phase) < 0 then vert = 255 * -sin(phase) else vert = 0
      line(x, hauteur + 30 - .06 * luminance)-(x, hauteur + 30 + .06 * luminance), rgb(rouge, vert, 0)
    end if
    if len(inkey) then end
  next
next
color rgb(200,200,200)
locate 1, 1: print " LA DIFFRACTION DE FRESNEL. Source lin‚aire rectiligne ‚quiphas‚e. Rayonnement radial."
locate 2, 1: print " Le diagramme en ‚chelle de gris ne montre que l'‚nergie des ondes."
locate 17,1: print " Ce graphique montre l'‚nergie pr‚sente sur l'axe central et la p‚riode relative, qui fluctue:"
locate 23,1: print " Ce diagramme montre les ondes, leur ‚nergie et leur p‚riode:"
Locate 36,3: Print "Merci aux cr‚ateurs de FreeBASIC."
Locate 37,3: Print "Gabriel LaFreniŠre.  glafreniere.com";
Locate 36,60:Print "Le 2 avril 2006. Ce programme peut ˆtre"
Locate 37,60:Print "distribu‚, copi‚ ou modifi‚ librement.";
sleep
end
