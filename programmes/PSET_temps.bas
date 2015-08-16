SCREEN 19,24,2' écran 19: 800 x 600 pixels; code 24: 256 tons par couleur; deux écrans.
titre$ = "L'ETHER VIRTUEL"'                  Gabriel LaFrenière  glafreniere.com
longueur = len(titre$) * 16: hauteur = 32
dim pixel(-1 to 800, -1 to 600)
dim gris(-1 to 800, -1 to 600)
dim ton(0 to 799, 0 to 599)
dim image((longueur * hauteur) * 2 + 4)
dim luminance(0 to longueur, 0 to hauteur)
dim as Single temps1, temps2
pset(0,0),rgb(225,225,225): grisClair = point(0,0)
debut:
screenset 0, 0
color noir, grisClair: cls
locate 1,1 : print titre$
sleep 500
for x = 0 to longueur - 1                                 'mettre le titre en mémoire.
  for y = 0 to hauteur - 1
    if point(x,y) = noir then luminance(x,y) = 0 else luminance(x,y) = grisClair
  next
next

for x = 0 to longueur - 1                                 'agrandir et afficher le titre.
  for y = 0 to hauteur - 1
    for z = 0 to 1
      line(2 * x, 2 * y + z)-(2 * x + 1, 2 * y + z), rgb(luminance(x, y), luminance(x, y), luminance(x, y))
    next  
  next
next
line(39,1)-(42,1), noir
line(38,2)-(41,2), noir
line (63,4)-(63,9), grisClair
line (191,4)-(191,9), grisClair
get (0,0)-(longueur, hauteur), image
put (270, 250), image, pset
sleep 500

temps1 = timer                                            'test sur l'écran affiché.
gosub Estomper
temps1 = timer - temps1

screenset 1,0                                             'la page n'est pas affichée.
cls: put (270, 250), image, pset

temps2 = timer                                            'test sur l'écran caché.
gosub Estomper
temps2 = timer - temps2

put (270, 150), image, pset
locate 14,42: print titre$
locate 3 ,4: print "Ce programme d‚montre que FreeBASIC travaille plus"
locate 4 ,4: print "rapidement sur un ‚cran cach‚."
locate 30,4: print "PSET sur ‚cran affich‚..";temps1; " sec"
locate 31,4: print "PSET sur ‚cran cach‚....";temps2; " sec"
locate 33,4: print "Gain:";temps1 - temps2;
gain = 100 * (temps1 - temps2) / temps2
print " sec. si l'instruction PSET est effectu‚e sur l'‚cran cach‚, soit"; gain;" %."
locate 24, 4: print "             Agrandissement simple suivi d'une interpolation … 5 pixels anti-cr‚nelage."
locate 35, 4: print "Pour recommencer, appuyez sur Entr‚e."
locate 36, 4: print "Pour quitter, appuyez sur une autre touche.       glafreniere.com  Le 15 ao–t 2006."
screenset 0,1

sleep: if inkey = chr(13) then goto debut else end

Estomper:

locate 23, 4:if temps2 then print"Ecran cach‚. Agrandissement simple suivi d'une interpolation … 5 pixels anti-cr‚nelage." else print "Ecran affich‚."

for x = 1 to 798                                          'bicubique sur l'écran au complet.
  for y = 1 to 598
'   if point(x-1,y-1) = noir then z1 = 0 else z1 = 225
   if point(x,y-1) = noir then z2 = 0 else z2 = 225
'   if point(x+1,y-1) = noir then z3 = 0 else z3 = 225
   if point(x-1,y) = noir then z4 = 0 else z4 = 225
   if point(x,y) = noir then z5 = 0 else z5 = 225
   if point(x+1,y) = noir then z6 = 0 else z6 = 225
'   if point(x-1,y+1) = noir then z7 = 0 else z7 = 225
   if point(x,y+1) = noir then z8 = 0 else z8 = 225
'   if point(x+1,y+1) = noir then z9 = 0 else z9 = 225
   gris(x,y) = (z1+z2+z3+z4+z5+z6+z7+z8+z9)/5'/9
  next
next

for x = 0 to 800                                          'balayer 4 fois l'écran complet.
  for y = 0 to 600                                        'total: 1920000 instructions PSET.
    pset(x,y), rgb(225, gris(x,y), gris(x,y))
  next
next

for x = 0 to 800
  for y = 0 to 600
    pset(x,y), rgb(gris(x,y), 225, gris(x,y))
  next
next

for x = 0 to 800
  for y = 0 to 600
    pset(x,y), rgb(gris(x,y), gris(x,y), 225)
  next
next

for x = 0 to 800
  for y = 0 to 600
    pset(x,y), rgb(gris(x,y), gris(x,y), gris(x,y))
  next
next

return
