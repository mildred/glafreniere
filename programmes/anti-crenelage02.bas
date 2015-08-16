SCREEN 19,24,1              'Gabriel LaFrenière, le 16 nov. 2005.    glafreniere.com
fond = rgb(225,225,225)
dore = rgb(200,150,0)
color dore, fond: cls
texte$ = "AGRANDISSEMENT - minuscules"
print texte$

for x = 0 to 800'                                         'doubler les dimensions.
  x2 = 2 * x: if x2 > 799 then x2 = 800'                
  for y = 0 to 600
    y2 = 2 * y + 10: if y2 > 599 then y2 = 600
    if point(x, y) = dore then                            'code anti-crénelage original.
      pset(x2, y2), dore: pset(x2+1, y2)                  'deux pixels sur quatre seulement.
      if point(x-1,y-1) = dore then pset(x2-1,y2-1), dore: pset(x2,y2-1),dore'dessus gauche.
      if point(x-1,y+1) = dore then pset(x2-1,y2+1), dore: pset(x2,y2+1),dore'dessous gauche.
      if point(x,y-1) = dore then pset(x2,y2-1), dore: pset(x2+1,y2-1),dore  'dessus
      if point(x,y-1) = fond and point(x,y+1) = fond then pset(x2,y2+1),dore: pset(x2+1,y2+1),dore' décoratif pour lettrages.
    end if  
  next
next

sleep:end