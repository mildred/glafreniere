Screen 19,24,2' par Gabriel LaFrenière. Créé le 9 février 2007.
Windowtitle "Création d'une sinusoïde par superposition de trois ondelettes de Ricker"
Color noir, Rgb(225,225,225): Cls
Dim As Single pi = 4 * Atn(1), x, y, Ricker, distance
xCentre = 400
yCentre = 300
gabarit = 200
Line(0,yCentre-gabarit)-(799,yCentre-gabarit), Rgb(150,150,150)      'gabarit.
Line(0,yCentre-gabarit/pi)-(500,yCentre-gabarit/pi), Rgb(150,150,150)'point de croisement.
For pixel = xCentre - 300 To xCentre + 300 Step 100                  'repères des abscisses.
Line(pixel,yCentre-gabarit)-(pixel,yCentre),Rgb(150,150,150)
Next

For pixel = 0 To 799                                      'distribution normale simplifiée.
  x = (xCentre - pixel) / 100                             'x = 1 équivaut à 100 pixels.
  y = gabarit * pi^(-x^2)
  Line(pixel, yCentre - yPrec)-(pixel, yCentre - y), Rgb(0,175,0)
  yPrec = y
Next

For pixel = 0 To 799                                      'somme de 3 ondelettes de Ricker.
  x = (xCentre - pixel) / 100
  
  x = x - 2
  Ricker = gabarit * pi^(-x^2) * x
  Pset(pixel, yCentre - Ricker), Rgb(255,0,0)             'ondelette de Ricker en rouge.
  y = Ricker
  
  x = x + 2
  Ricker = gabarit * pi^(-x^2) * x
  Pset(pixel, yCentre - Ricker), Rgb(255,0,0)
  y = y + Ricker
  
  x = x + 2
  Ricker = gabarit * pi^(-x^2) * x
  Pset(pixel, yCentre - Ricker), Rgb(255,0,0)
  y = y + Ricker
  
  If pixel > 200 And pixel < 600 Then
    Line(pixel, yCentre - yPrec)-(pixel, yCentre - y), Rgb(100,100,255) 'sinusoïde en bleu.
  End If
  yPrec = y
Next

Line(0,yCentre)-(799,yCentre), Rgb(150,150,150)           'axe.
Line(30,392)-(60,392),Rgb(0,175,0)                        'vert
Line(30,393)-(60,393),Rgb(0,175,0)
Line(30,408)-(60,408),Rgb(255,0,0)                        'rouge.
Line(30,409)-(60,409),Rgb(255,0,0)
Line(250,408)-(280,408),Rgb(100,100,255)                  'bleu.
Line(250,409)-(280,409),Rgb(100,100,255)
Locate 2
Locate,3:  ?"Ce programme montre qu'on peut tracer une ondelette de Ricker (en rouge) … partir de la formule"
Locate,3:  ?"de la distribution normale simplifi‚e (en vert). On obtient une sinuso‹de en faisant la somme"
Locate,3:  ?"de trois ondelettes de Ricker d‚cal‚es de 2 x. Cette m‚thode est comparable … celle d'Euler."
Locate 6,16: ?"x =      -2          -1           0            1           2"
Locate 7,1:  ?" y = 1 "
Locate 15,1: ?" y = 1 / pi "
Locate 19,1: ?" y = 0 "
Locate 25
Locate,9: ?" y = pi^(-x^2)"
Locate,9: ?" y = pi^(-x^2) * x";
Locate,37:?"Somme de trois ondelettes de Ricker d‚cal‚es de 2 x.":?:?
Locate,37:?"La sinuso‹de bleue est relativement pr‚cise entre -1 et +1."
Locate,37:?"Il subsiste sans doute une l‚gŠre impr‚cision qu'on pourrait"
Locate,37:?"r‚duire en ajoutant deux autres ondelettes de part et d'autre."
Color Rgb(0,150,0)
Locate 34
Locate,56:Print "Gabriel LaFreniŠre  glafreniere.com":?
Locate,56:Print "Le 14 f‚v. 2007. Ce programme FreeBASIC peut"
Locate,56:Print "ˆtre distribu‚, copi‚ ou modifi‚ librement. ";
Sleep
