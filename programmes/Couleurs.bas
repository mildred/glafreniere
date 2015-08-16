SCREEN 18,24,2' par Gabriel LaFrenière, révisé le 27 octobre 2005.
dim as single gris, Lx, Ly, pas, distance, pi, angle
pi = 4 * atn(1)
pas = pi / 300
rayon% = 218
r% = 20
ref% = 1000                         'cercles de référence avec rayon de 1000 pixels à déduire.
xCentre% = 320: yCentre%= 240
page1 = 1

DO
  SWAP page1, page2                                         'permuter les pages.
  SCREENSET page1, page2                                    'afficher l'autre page.
  color 1, rgb(225,225,225): cls
  
  xBleu% = xCentre% + rayon% * cos(angle)
  yBleu% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xCiel% = xCentre% + rayon% * cos(angle)
  yCiel% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xTurquoise% = xCentre% + rayon% * cos(angle)
  yTurquoise% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xVert% = xCentre% + rayon% * cos(angle)
  yVert% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xVertTendre% = xCentre% + rayon% * cos(angle)
  yVertTendre% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xLime% = xCentre% + rayon% * cos(angle)
  yLime% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xJaune% = xCentre% + rayon% * cos(angle)
  yJaune% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xOrange% = xCentre% + rayon% * cos(angle)
  yOrange% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xRouge% = xCentre% + rayon% * cos(angle)
  yRouge% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xMagenta% = xCentre% + rayon% * cos(angle)
  yMagenta% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xViolet% = xCentre% + rayon% * cos(angle)
  yViolet% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
  xIndigo% = xCentre% + rayon% * cos(angle)
  yIndigo% = yCentre% + rayon% * sin(angle)
  angle = angle + pi / 6
                                    'cercles de référence avec rayon de 1000 pixels à déduire.
  xB% = xCentre% + (ref% - 128) * cos(angle + pi)           '+ pi: le point est à l'opposé.
  yB% = yCentre% + (ref% - 128) * sin(angle + pi)': circle (xB%, yB%), ref%, 0
  angle = angle + 2 * pi / 3
  xV% = xCentre% + (ref% - 128) * cos(angle + pi)
  yV% = yCentre% + (ref% - 128) * sin(angle + pi)': circle (xV%, yV%), ref%, 0
  angle = angle + 2 * pi / 3
  xR% = xCentre% + (ref% - 128) * cos(angle + pi)
  yR% = yCentre% + (ref% - 128) * sin(angle + pi)': circle (xR%, yR%), ref%, 0
  angle = angle + 2 * pi / 3

  for x% = xCentre% - rayon% to xCentre% + rayon%
    for y% = yCentre% - rayon% to yCentre% + rayon%
      Lx = xCentre% - x%
      Ly = yCentre% - y%
      distance = sqr(Lx * Lx + Ly * Ly)
      if distance <= rayon% then
        Lx = xR% - x%
        Ly = yR% - y%
        distance = sqr(Lx * Lx + Ly * Ly)
        if distance > ref% then rouge% = 255 else rouge% = 255 - (ref% - distance)
        Lx = xV% - x%
        Ly = yV% - y%
        distance = sqr(Lx * Lx + Ly * Ly)
        if distance > ref% then vert% = 255 else vert% = 255 - (ref% - distance)
        Lx = xB% - x%
        Ly = yB% - y%
        distance = sqr(Lx * Lx + Ly * Ly)
        if distance > ref% then bleu% = 255 else bleu% = 255 - (ref% - distance)
        if rouge% < 0 then rouge% = 0
        if rouge% > 255 then rouge% = 255
        if vert% < 0 then vert% = 0
        if vert% > 255 then vert% = 255
        if bleu% < 0 then bleu% = 0
        if bleu% > 255 then bleu% = 255
        pset (x%,y%), rgb (rouge%,vert%,bleu%)
      end if
    next
  next

  line (xBleu%, yBleu%)-(xRouge%, yRouge%), 0
  line (xVertTendre%, yVertTendre%)-(xRouge%, yRouge%), 0
  line (xVertTendre%, yVertTendre%)-(xBleu%, yBleu%), 0
  line (xMagenta%, yMagenta%)-(xJaune%, yJaune%), 0
  line (xJaune%, yJaune%)-(xVert%, yVert%), 0
  line (xVert%, yVert%)-(xBleu%, yBleu%), 0
  line (xBleu%, yBleu%)-(xMagenta%, yMagenta%), 0
  
  circle (xCentre%,yCentre%),rayon%, 0
  circle (xRouge%, yRouge%), r%, 1: paint (xRouge%, yRouge%), rgb(255,0,0), 1
  circle (xOrange%, yOrange%), r%, 1: paint (xOrange%, yOrange%), rgb(255,128,0), 1
  circle (xJaune%, yJaune%), r%, 1: paint (xJaune%, yJaune%), rgb(255,255,0), 1
  circle (xLime%, yLime%), r%, 1: paint (xLime%, yLime%), rgb(128,255,0), 1
  circle (xVertTendre%, yVertTendre%), r%, 1: paint (xVertTendre%, yVertTendre%), rgb(0,255,0), 1
  circle (xVert%, yVert%), r%, 1: paint (xVert%, yVert%), rgb(0,255,128), 1
  circle (xBleu%, yBleu%), r%, 1: paint (xBleu%, yBleu%), rgb(0,0,255), 1
  circle (xTurquoise%, yTurquoise%), r%, 1: paint (xTurquoise%, yTurquoise%), rgb(0,255,255), 1
  circle (xCiel%, yCiel%), r%, 1: paint (xCiel%, yCiel%), rgb(0,128,255), 1
  circle (xIndigo%, yIndigo%), r%, 1: paint (xIndigo%, yIndigo%), rgb(128,0,255), 1
  circle (xViolet%, yViolet%), r%, 1: paint (xViolet%, yViolet%), rgb(255,0,255), 1
  circle (xMagenta%, yMagenta%), r%, 1: paint (xMagenta%, yMagenta%), rgb(255,0,128), 1
  
  angle = angle + pas: if angle > 2 * pi then angle = angle - 2 * pi
  if len(inkey$) then end
LOOP
