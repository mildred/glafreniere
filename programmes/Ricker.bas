screen 19,24,2                                            'créé le 10 février 2007.
windowtitle "L'ondelette de Ricker"
dim as single pi = 4 * atn(1), x, y, distance
color NOIR, rgb(225,225,225): cls
ROUGE = rgb(255,0,0)
VERT  = Rgb(0,200,0)
BLEU  = rgb(100,100,255)
GRIS  = rgb(150,150,150)
ORANGE= rgb(255,150,0)
VIOLET= Rgb(255,0,255)
TURQUOISE= rgb(0,150,150)
xCentre = 400
yCentre = 327
gabarit = 160
line(0,yCentre-gabarit)-(799,yCentre-gabarit), GRIS               'y = 1.
line(0,yCentre-2*gabarit)-(799,yCentre-2*gabarit), GRIS           'y = 2.
line(0,yCentre-(2/pi)*gabarit)-(500,yCentre-(2/pi)*gabarit), GRIS 'y = 2 / pi.
line(0,yCentre-gabarit/pi)-(528,yCentre-gabarit/pi), GRIS         'y = 1 / pi.
for pixel = xCentre - 300 to xCentre + 300 step 100               'abscisses.
  line(pixel,yCentre-2*gabarit)-(pixel,yCentre),GRIS
next
locate 10,16: ?"x =      -2          -1           0            1           2 "

for pixel = 0 to 799                                      'distribution normale doublée.
  x = (xCentre - pixel) / 100
  y = 2 * gabarit * pi^(-x^2)
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), ROUGE
  yPrec = y
next
for pixel = 0 to 799                                      'distribution normale, haut.
  x = (xCentre - pixel) / 100                             'x = 1: 100 pixels.
  y = gabarit * pi^(-x^2)
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), VIOLET
  yPrec = y
next
for pixel = 0 to 799                                      'distribution normale, bas.
  x = (xCentre - pixel) / 100                             'x = 1: 100 pixels.
  y = gabarit * pi^(-x^2)
  line(pixel, yCentre + yPrec)-(pixel, yCentre + y), VIOLET
  yPrec = y
next
'---------------------------------- ONDELETTES DE RICKER --------------------------------------
yPrec = 0
for pixel = 0 to 799                                      'ondelette de Ricker uniforme.
  x = (xCentre - pixel) / 100
  y = x * gabarit * pi^(-x^2)
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), VERT
  yPrec = y
next

distance = 4/3                                            'C.F. volume (sphère): (4/3)*pi*R^3
for pixel = 450 to 799                                    '                    (point minimum).
  x = (pixel - xCentre) / 100 - distance
  y = gabarit * pi^(-x^2) / (distance / x + 1)
  if pixel = 450 then yPrec = y
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), ORANGE'ondelette de Ricker, partie droite.
  yPrec = y
next
yPrec = 0
distance = -4/3
for pixel = 0 to 350
  x = (pixel - xCentre) / 100 - distance
  y = gabarit * pi^(-x^2) / (distance / x + 1)
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), ORANGE'partie gauche.
  yPrec = y
next
distance = 2
for pixel = 450 to 799                                    '2e ondelette de Ricker affaiblie.
  x = (pixel - xCentre) / 100 - distance
  y = gabarit * pi^(-x^2) / (distance / x + 1)
  if pixel = 450 then yPrec = y
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), BLEU 'partie droite.
  yPrec = y
next
distance = -2
for pixel = 0 to 350                                      'partie gauche.
  x = (pixel - xCentre) / 100 - distance
  y = gabarit * pi^(-x^2) / (distance / x + 1)
  if pixel = 0 then yPrec = y
  line(pixel, yCentre - yPrec)-(pixel, yCentre - y), BLEU 'partie droite.
  yPrec = y
next
for pixel = 0 to 799                                      'affaiblissement selon la distance.
  x = (xCentre - pixel) / 100
  y = (2 / pi) ^ 2 * gabarit / x                          'constante selon (2 / pi)^2.
  pset(pixel, yCentre - y), NOIR
  pset(pixel, yCentre + y), NOIR
next
line(0,yCentre)-(799,yCentre), rgb(150,150,150)           'axe.
line(30,424)-(60,424), VIOLET
line(30,425)-(60,425), VIOLET
line(30,488)-(60,488), ROUGE
line(30,489)-(60,489), ROUGE
line(30,552)-(60,552), VERT
line(30,553)-(60,553), VERT
line(480,440)-(510,440), NOIR
line(480,441)-(510,441), NOIR
line(480,520)-(510,520), ORANGE
line(480,521)-(510,521), ORANGE
line(480,536)-(510,536), BLEU
line(480,537)-(510,537), BLEU
locate  1,1: ?" y = 2 "
locate 11,1: ?" y = 1 "
locate 15,1: ?" y = 2 / pi "
locate 18,1: ?" y = 1 / pi "
locate 21,1: ?" y = 0 "
locate 25
locate,2:  ?"La distribution normale:":?
locate,10: ?"y = pi^(-x^2)":?
locate,2:  ?"L'impulsion gaussienne initiale doubl‚e:":?
locate,10: ?"y = 2 * pi^(-x^2)":?
locate,2:  ?"L'ondelette de Ricker uniforme:":?
locate,10: ?"y = x * pi^(-x^2)":locate 26
locate,66: ?"L'affaiblissement:":?
locate,66: ?"y = (2 / pi) ^ 2 / x":?
locate,60: ?"L'ondelette de Ricker affaiblie:":?
locate,66: ?"y = pi^(-x^2) / (distance / x + 1)"
locate,66: ?"y = pi^(-x^2) / ((4/3) / x + 1)"
locate,66: ?"y = pi^(-x^2) / ( 2 / x + 1)"
Color Rgb(0,150,0): locate 37
Locate,2:Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 36,56: Print "Le 14 f‚v. 2007. Ce programme FreeBASIC peut"
Locate  , 56: Print "ˆtre distribu‚, copi‚ ou modifi‚ librement. ";
sleep
