images = 48: largeur = 400: hauteur = 300
dim max1(largeur), max2(largeur)
dim graph1(images, largeur), graph2(images, largeur)
dim couleur1(images, largeur, hauteur), couleur2(images, largeur, hauteur)
dim amplit1(images, largeur, hauteur), amplit2(images, largeur, hauteur)
dim as single pi = 4 * atn(1), x, xCarre, beta, rotation, distance, lambdaLorentz
dim as single amplitude, phi, lambdaAvant, lambdaArriere, rapport, facteur, pas, ton
screen 19,24,3: page1 = 1: gosub Initialiser

'******************* L'ÉLECTRON SELON LES FORMULES DE M. JOCELYN MARCOTTE ********************
do
  swap page1, page2
  screensync
  screenset page1, page2
  pcopy 2, page1
  rotation = 2 * pi * image / images                      'rotation de phase selon l'image.
  if calculFait then
    for x = 1 to largeur
      for y = 0 to hauteur
        pset(x, y), couleur1(image,x,y)
        pset(x + largeur + 2, y), couleur2(image,x,y)
      next
      line(x, yCentreGraph - max1(x)) - (x, yCentreGraph + max1(x)), rgb(150,150,150)
      line(x, yCentreGraph)-(x, graph1(image, x)), rgb(255,255,255)
      line(x, graph1(image, x - 1))-(x, graph1(image, x))
      line(x + largeur + 2, yCentreGraph - max2(x)) - (x + largeur + 2, yCentreGraph + max2(x)), rgb(150,150,150)
      line(x + largeur + 2, yCentreGraph)-(x + largeur + 2, graph2(image, x)), rgb(255,255,255)
      line(x + largeur + 2, graph2(image, x - 1))-(x + largeur + 2, graph2(image, x))
    next
    line(largeur + 1, 0)-(largeur + 2, yCentreGraph), rgb(150,150,150), bf
  else    
    gosub Stationnaire                                    'ondes stationnaires sphériques.
    Gosub ElectronMobile                                  'électron mobile.
  end if
  a$ = right(ucase(inkey),1)
  select case a$
  case chr(27), "X", "K": end
  case "I": gosub Initialiser
  case "P": sleep
  case "+": lambda = lambda + 10: if lambda > 200 then lambda = 200
    lambdaLorentz = lambda / sqr(1 - beta ^ 2)            'dilatation de la longueur d'onde.
    facteur = .01 * sqr(lambda)                           'facteur d'amplitude du relief.
    image = 0: calculFait = 0
    for xCoord = 0 to largeur
      max1(xCoord) = 0: max2(xCoord) = 0                  'effacer l'enveloppe.
    next
    screenset 2,2: locate 33, 60: ? lambda; " pixels. ": screenset page1, page2
  case "-": lambda = lambda - 10: if lambda < 20 then lambda = 20
    lambdaLorentz = lambda / sqr(1 - beta ^ 2)
    facteur = .01 * sqr(lambda)
    image = 0: calculFait = 0
    for xCoord = 0 to largeur
      max1(xCoord) = 0: max2(xCoord) = 0
    next
    screenset 2,2: locate 33, 60: ? lambda; " pixels. ": screenset page1, page2
  case "0","1","2","3","4","5","6","7","8","9"
    beta = val(a$) / 10
    lambdaLorentz = lambda / sqr(1 - beta ^ 2)
    image = 0: calculFait = 0
    for xCoord = 0 to largeur
      max1(xCoord) = 0: max2(xCoord) = 0
    next
    screenset 2,2: locate 34,59: print using "#.#"; beta;
    screenset page1, page2
  end select
  do: loop while len(inkey)
  image = image + 1: if image = images + 1 then image = 1
loop

Couleurs:
If amplitude > 0 Then
  vert = amplitude
  bleu = vert / 3
  If vert > 255 Then rouge = (vert - 255) / 2 Else rouge = 0
Else
  rouge = -amplitude
  bleu = rouge / 3
  If rouge > 255 Then vert = (rouge - 255) / 2 Else vert = 0
End If

if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
if vert > 255 then vert = 255 else if vert < 0 then vert = 0
if bleu > 255 then bleu = 255 else if bleu < 0 then bleu = 0
return

ElectronMobile:'********************** ELECTRON MOBILE ***************************************

for xCoord = 0 to largeur
  xDistance = xCoord - xCentre1
  xCarre = (xDistance + .5) ^ 2
  for yCoord = 0 to hauteur                               'pas de symétrie verticale.
    distance = 2 * (yCentre - yCoord)                     'distance vert. doublée (plan incliné de 60°).
    distance = sqr(distance * distance + xCarre)          'distance selon Pythagore.
    if xDistance >= 0 then
      if xDistance then phi = atn(2 * (yCentre - yCoord) / xDistance) else phi = pi / 2                      'calcul de l'angle phi.
      lambdaAvant =   lambdaLorentz*(cos(asin(beta*sin(phi)))-beta*cos(phi))'effet Doppler « relatif ».
      lambdaArriere = lambdaLorentz*(cos(asin(beta*sin(phi+pi)))-beta*cos(phi+pi))
      if (yCentre - yCoord) = 0 then lambdaAvant = lambdaLorentz * (1 - beta): lambdaArriere = lambdaLorentz * (1 + beta)
      rapport = sqr(lambdaArriere / lambdaAvant)          'racine: utilisé dans les deux sens.
      
'************** FORMULE UNIFIÉE ORIGINALE DE M. JOCELYN MARCOTTE (JUILLET 2006) ***************
'     x = 2 * pi * distance / lambdaAvant                 'conversion en radians.
'     amplitude = rapport * (Cos(rotation) * Sin(x) + Sin(rotation) * (1 - Cos(x))) / x
'     x = 2 * pi * distance / lambdaArriere
'     amplitude = amplitude + (Cos(rotation) * Sin(x) - Sin(rotation) * (1 - Cos(x))) / x / rapport
'**********************************************************************************************

'****************** FORMULE SIMPLIFIÉE DE M. PHILIPPE DELMOTTE (SEPTEMBRE 2006) ***************
      x = 2 * pi * distance / lambdaAvant                 'conversion en radians.
      amplitude = rapport * (sin(-rotation + x) - sin(-rotation)) / x
      x = 2 * pi * distance / lambdaArriere
      amplitude = amplitude + (sin(rotation + x) - sin(rotation)) / x / rapport
'**********************************************************************************************

    elseif xDistance < 0 then
      phi = atn(2 * (yCentre - yCoord) / xDistance) + pi
      lambdaAvant = lambdaLorentz*(cos(asin(beta*sin(phi+pi)))-beta*cos(phi+pi))
      lambdaArriere = lambdaLorentz*(cos(asin(beta*sin(phi)))-beta*cos(phi))
      if yCoord = 0 then lambdaAvant = lambdaLorentz * (1 - beta): lambdaArriere = lambdaLorentz * (1 + beta)
      rapport = sqr(lambdaArriere / lambdaAvant)
      x = 2 * pi * distance / lambdaAvant
      amplitude = rapport * (sin(rotation + x) - sin(rotation)) / x
      x = 2 * pi * distance / lambdaArriere
      amplitude = amplitude + (sin(-rotation + x) - sin(-rotation)) / x / rapport
    end if
    graphique = -60 * amplitude / 2                       'il y a deux ondes. 
    amplitude = gabarit * amplitude / 2

'************** EFFET DE RELIEF INSPIRÉ DE LA PROCÉDURE DE M. PHILIPPE DELMOTTE ***************

    y = yCoord - facteur * amplitude                      'décaler le pixel selon l'amplitude.
    ecart = y - yPrec                                     'selon le pixel précédent.
    yPrec = y

    if yCoord = -yCentre or ecart < 2 then                'écrase/côtoie les pixels précédents.
      gosub Couleurs
      pset(xCoord + largeur + 2, y), rgb(rouge, vert, bleu)
      if y >= 0 and y <= hauteur then couleur2(image, xCoord, y) = rgb(rouge, vert, bleu)
      amplitudePrec = amplitude
    else                             'comble le vide si le pixel précédent n'était pas voisin.
      pas = (amplitudePrec - amplitude) / ecart           'estomper avec le pixel précédent.
      amplitudePrec = amplitude
      for j = 0 to ecart                                  'combler les espaces vides.
        gosub Couleurs
        pset(xCoord + largeur + 2, y - j), rgb(rouge, vert, bleu)
        if y - j >= 0 and y - j <= hauteur then couleur2(image, xCoord, y - j) = rgb(rouge, vert, bleu)
        amplitude = amplitude + pas
      next
    end if
    
'************************************** GRAPHIQUE 1-D ****************************************
    if yCoord = yCentre then
      graph2(image, xCoord) = yCentreGraph + graphique
      if abs(graphique) > max2(xCoord) then max2(xCoord) = abs(graphique)
      line(largeur + 2 + xCoord, yCentreGraph - max2(xCoord))-(largeur + 2 + xCoord, yCentreGraph + max2(xCoord)), rgb(150,150,150)
      line(largeur + 2 + xCoord, yCentreGraph)-(largeur + 2 + xCoord, yCentreGraph + graphique), rgb(255,255,255)
      line(largeur + 2 + xCoord, yPrecedent)-(largeur + 2 + xCoord, yCentreGraph + graphique)
      yPrecedent = yCentreGraph + graphique
    end if
  next
next
if image = images then calculFait = 1
return

Stationnaire:'******************** ELECTRON STATIONNAIRE *************************************

line(largeur + 1, hauteur)-(largeur + 2, yCentreGraph), rgb(150,150,150), bf
for xCoord = 0 to largeur
  xCarre = (xCoord - xCentre1 + .5) ^ 2
  for yCoord = 0 to hauteur
    distance = 2 * (yCentre - yCoord)                     'distance vert. doublée (plan incliné de 60°).
    distance = sqr(distance * distance + xCarre)          'distance selon Pythagore.
    x = 2 * pi * distance / lambda                        'distance en radians.
    if x then amplitude = sin(x)/x*cos(rotation) else amplitude = cos(rotation)

'************** EFFET DE RELIEF INSPIRÉ DE LA PROCÉDURE DE M. PHILIPPE DELMOTTE ***************

    graphique = -60 * amplitude
    amplitude = gabarit * amplitude
    y = yCoord - facteur * amplitude                      'décaler le pixel selon l'amplitude.
    ecart = y - yPrec                                     'selon le pixel précédent.
    yPrec = y
    if yCoord = 0 or ecart < 2 then                       'écrase/côtoie les pixels précédents.
      gosub Couleurs
      pset(xCoord, y), rgb(rouge, vert, bleu)
      if y >= 0 and y <= hauteur then couleur1(image, xCoord, y) = rgb(rouge, vert, bleu)
      amplitudePrec = amplitude
    else                             'comble le vide si le pixel précédent n'était pas voisin.
      pas = (amplitudePrec - amplitude) / ecart           'estomper avec le pixel précédent.
      amplitudePrec = amplitude
      for j = 0 to ecart                                  'combler les espaces vides.
        gosub Couleurs
        pset(xCoord, y - j), rgb(rouge, vert, bleu)
        if y - j >= 0 and y - j <= hauteur then couleur1(image, xCoord, y - j) = rgb(rouge, vert, bleu)
        amplitude = amplitude + pas
      next
    end if
    
'************************************** GRAPHIQUE 1-D ****************************************
    if yCoord = yCentre then
      graph1(image, xCoord) = yCentreGraph + graphique
      if abs(graphique) > max1(xCoord) then max1(xCoord) = abs(graphique)
      line(xCoord, yCentreGraph - max1(xCoord))-(xCoord, yCentreGraph + max1(xCoord)), rgb(150,150,150)
      line(xCoord, yCentreGraph)-(xCoord, yCentreGraph + graphique), rgb(255,255,255)
      line(xCoord, yPrecGraph)-(xCoord, yCentreGraph + graphique)
      yPrecGraph = yCentreGraph + graphique
    end if
  next
next
return

Initialiser:'************************* INITIALISATION ****************************************
fond  = rgb(225,225,225)
beta = .5
lambda = 70
lambdaLorentz = lambda / sqr(1 - beta ^ 2)
xCentre1 = largeur / 2
xCentre2 = 800 - largeur / 2
yCentre = hauteur / 2
yCentreGraph = hauteur + 82
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
calculFait = 0
image = 0
facteur = .009 * sqr(lambda)                              'facteur d'amplitude du relief.
gabarit = 1500                                            'écrêter l'excédent de 256 tons * 3.
for xCoord = 0 to largeur                                 'effacer l'enveloppe.
  max1(xCoord) = 0: max2(xCoord) = 0
next

locate 26,32:  ?"Phase      y = sin(x) / x"
locate 27,32:  ?"Quadrature y = (1 - cos(x)) / x"
locate 28,32:  ?"Rotation   y = (sin(t + x) - sin(t)) / x"
locate 30,2:  ?"Ce programme d‚montre que les formules de M. Jocelyn Marcotte peuvent reproduire l'‚lectron"
locate 31,2:  ?"dans toutes ses parties. La formule de rotation a ‚t‚ simplifi‚e par M. Philippe Delmotte."
locate 33,6:  ?"- Zoom: appuyez sur + ou - .................. lambda ="; lambda; " pixels."
locate 34,6:  ?"- Vitesse: appuyez sur un chiffre de 0 … 9... bˆta = ";: print using "#.#"; beta;: print " = v / c."
locate 35,2:  ?"Initialiser: appuyez sur I."
locate 36,2:  ?"Pause: appuyez sur P."
locate 37,2:  ?"QUITTER : Appuyez sur Echap.";
locate 36,34: ?"Ce programme FreeBASIC peut ˆtre copi‚, modifi‚ ou distribu‚"
locate 37,34: ?"librement. Gabriel LaFreniŠre, le 5 nov. 2006.  glafreniere.com";
pcopy 2, page1
return
