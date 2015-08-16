'       CRÉÉ LE 13 OCTOBRE 2006. - LES OSCILLATIONS D'UN PENDULE PRODUISENT UNE SINUSOÏDE.
Dim As Single energie, inertie, sinus, cosinus, phase, pas, pi = 4 * Atn(1)
Screen 19,24,1
blanc = Rgb(255,255,255)
vert = Rgb(0,150,0)
fond = Rgb(225,225,225)
lambda = 200                                              'Noter que la longueur d'onde est
pas = (lambda / (2 * pi)) ^ 2                             'exprimée en nombre de fois où la
gabarit = 100                                             'l'algorithme est traité, ce qui 
inertie = gabarit                                         'correspond ici à des pixels.
yCentre = 236
Color noir, fond: Cls: Locate 2
Locate, 3: Print "Robert Hooke a ‚crit en 1678: ® Ut tensio sic vis ¯  (telle extension, telle force). La loi de"
Locate, 3: Print "Hooke est la devise de l'Ecole Polytechnique de Montr‚al. Elle s'applique entre autres au pendule"
Locate, 3: Print "de Huygens. L'augmentation de la tension sur son ressort ‚tant lin‚aire, c'est … dire proportion-"
Locate, 3: Print "nelle … l'extension, la p‚riode d'oscillation demeure constante quelle que soit l'amplitude. Ci-"
Locate, 3: Print "dessous, la v‚ritable sinuso‹de en noir confirme le trac‚ blanc donn‚ par l'algorithme. La courbe"
Locate, 3: Print "rouge selon l'inertie indique la quadrature. M. Anselme Dewavrin a mis au point un algorithme si-"
Locate, 3: Print "milaire qui montre que ce calcul est apparent‚ … la m‚thode d'Euler (courbe verte)."
Locate 26
Locate, 3: Print "L'algorithme qui produit cette courbe est vraiment ‚l‚mentaire. L'‚nergie qui d‚termine les"
Locate, 3: Print "oscillations du pendule est trait‚e r‚p‚titivement selon les ‚quations ci-dessous. Le pas"
Locate, 3: Print "variable permet de modifier ou de pr‚ciser les oscillations, qui sont sinuso‹dales."
Locate 34, 3: Print "pas = (lambda / (2 * pi)) ^ 2"
Color noir, blanc
Line(206, 361)-(594, 389),noir, bf
Line(208, 363)-(592, 387),blanc, bf
Locate 24,28: Print "Les variations de l'‚nergie sont sinuso‹dales."
Line(250,456)-(550,502),noir,bf
Line(252,458)-(548,500),blanc,bf
Locate 30,35: Print "inertie = inertie - energie"
Locate 31,35: Print "energie = energie + inertie / pas"
Color noir, fond
For x = 0 To 799
'********************* CALCUL SELON LA LOI DE HOOKE ET LES LOIS DE NEWTON *********************
' équations de Gabriel La Frenière.
  inertie = inertie - energie
  energie = energie + inertie / pas
'************************************** FIN DU CALCUL *****************************************
  Line(x, yCentre - Sqr(pas) * energie)-(x, yCentre),blanc'sinusoïde selon l'algorithme.
  Pset(x, yCentre - inertie),Rgb(255,0,0)                 'l'inertie indique le cosinus.
  y = gabarit * Sin(2 * pi * x / lambda)
  Pset(x, yCentre - y), noir                              'véritable sinusoïde pour comparer.
Next
cosinus = gabarit
pas = lambda / (2 * pi)                                   'la racine carrée du pas ci-dessus.
For x = 0 To 799
'****************************** CALCUL SELON LA MÉTHODE D'EULER *******************************
'  équations de M. Anselme Dewavrin.
   sinus = sinus + cosinus / pas
   cosinus = cosinus - sinus / pas
'************************************** FIN DU CALCUL *****************************************
  Pset(x, yCentre + sinus), Rgb(0,175,0)
Next
Line(0, yCentre)-(799, yCentre), noir                     'axe
Locate 36: Color vert
Locate, 3: Print "Merci aux cr‚ateurs de FreeBASIC."
Locate, 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate,70: Print "Le 21 oct. 2006. Ce programme "
Locate,70: Print "FreeBASIC peut ˆtre distribu‚,"
Locate,70: Print "copi‚ ou modifi‚ librement.   ";
Do: Loop Until Len(Inkey)
