SCREEN 19,24,1              'Gabriel LaFrenière, le 16 nov. 2005.    glafreniere.com
fond = rgb(225,225,225)
color 0, fond: cls
locate 1, 1 : print " CE TEXTE SERA AGRANDI"
locate 3, 21 : print "minuscules"
decaler = 282
bleu = rgb(150,150,255)
rouge = rgb(255,0,0)
line(65,15)-(90,40), noir
line(155,15)-(130,40), noir
line(10,15)-(20,35), noir
line(33,28)-(53,28),noir
line(43,18)-(43,38),noir
circle(205,16),15,noir
circle(109,28),14,1
paint(100,24),0,1

for x = 0 to 250'                                         'doubler les dimensions.
  x2 = 2 * x + decaler: if x2 > 799 then x2 = 800'                
  for y = 0 to 45
    y2 = 2 * y: if y2 > 599 then y2 = 600
    if point(x, y) = noir then                            'code anti-crénelage original.
      pset(x2, y2), noir: pset(x2, y2+1)                  'deux pixels sur quatre seulement.
      if point(x-1,y-1) = noir then pset(x2-1,y2-1), noir: pset(x2-1,y2), noir
      if point(x-1,y+1) = noir then pset(x2-1,y2+1), noir: pset(x2-1,y2+2), noir
      if point(x-1,y) = noir then pset(x2-1,y2), noir: pset(x2-1,y2+1), noir
    end if  
  next
next

for x = 280 to 800'                                         'doubler les dimensions.
  for y = 0 to 100
    if point(x, y) = noir and point(x-1, y) = fond then pset(x-1, y), noir
  next
next

for x = 0 to 250'                                         'doubler les dimensions, carrés.
  x2 = 2 * x + decaler: if x2 > 799 then x2 = 800'                
  for y = 0 to 45
    y2 = 2 * y + 150: if y2 > 599 then y2 = 600
    if point(x, y) = noir then
      pset(x2, y2), noir: pset(x2+1, y2), noir: pset(x2+1, y2+1), noir: pset(x2, y2+1), noir
    end if  
  next
next

locate 7,4:  print "Image originale.                  Agrandissement am‚lior‚ sans cr‚nelage."
locate 17,48: print "Agrandissement avec cr‚nelage."
locate 24
locate, 4: print "Le fait de doubler les dimensions d'une image en utilisant des carr‚s produit une pix‚lisation,"
locate, 4: print "c'est … dire un ® effet d'escalier ¯ ou cr‚nelage (anglais: aliasing) d‚sagr‚able."
?
locate, 4: print "Ci-dessus, le cr‚nelage a ‚t‚ ‚limin‚ en recourant … un algorithme sp‚cial. Cet algorithme est"
locate, 4: print "sup‚rieur … ceux qui font appel … une interpolation de type bicubique parce qu'il ne provoque"
locate, 4: print "pas de flou. Il devrait ˆtre plus complexe pour traiter efficacement une image en couleurs"
locate, 4: print "normale, mais tout indique que c'est possible."
?
locate, 4: print "Code source FreeBASIC, 16 ao–t 2006."
locate, 4: print "Ce programme peut ˆtre distribu‚, copi‚ ou modifi‚ librement."
print
locate, 4: print "glafreniere.com";

do
  getmouse xPixel, yPixel, roulette, clic
  locate 5, 5: print xPixel, yPixel; "    " 
loop until len(inkey)
