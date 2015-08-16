Screen 18,24,3' écran 18: 640 x 480 pixels; code 24: 256 tons par couleur; code 3: trois écrans.
x = 100: y = 250: page1 = 1: pas = 1
Color 0, Rgb(225,225,225): Screenset 2: Cls 'la page No. 2 est la page-mère, la TROISIÈME page...
Locate 2, 26: Print "LA PROGRAMMATION A TROIS ECRANS"
Locate 23, 3: Print "Le programme affiche deux ‚crans en alternance.";
Locate 24, 3: Print "Cette m‚thode permet de r‚aliser des animations plus fluides."
Locate 25, 3: Print "De plus, FreeBASIC travaille plus rapidement sur une page cach‚e."
Locate 26, 3: Print "Une page-mŠre additionnelle permet d'afficher un arriŠre-plan"
Locate 27, 3: Print "permanent sans devoir le recr‚er constamment."
Locate 29, 3: Print "Pour quitter, appuyez sur une touche.  Gabriel LaFreniŠre  glafreniere.com";

Do                         'modifier la page cachée pendant que l'autre est affichée.
  Swap page1, page2        'les valeurs 1 et 0 sont échangées.
  screensync               'attendre le cycle (généralement 85 Hz) pour régulariser l'animation.
  Screenset page1, page2   'travailler sur la page cachée, afficher la page visible.
'  Screenlock              'parfois utile en programmation avancée.
  Pcopy 2, page1           'copier la page-mère sur la page de travail (cachée).
  x = x + pas
  If x > 538 Or x < 100 Then pas = - pas
  Circle (x,y), 30, 0                                     'cercle mobile, pour démonstration.
  Paint (x,y), Rgb(255 - (x - 100) / 2.12, x / 2.12, 0), 0'passer de rouge à vert.
  
  If page1 Then'                                          'une fois sur deux.
    Line(270,50)-(370,150),Rgb(0,0,255), bf               'carré bleu.
    Locate 5,38: Print "Page 1"
  Else
    Line(270,50)-(370,150),Rgb(255,0,0), bf               'carré rouge.
    Locate 6,38: Print "Page 2"
  End If
  If Len(inkey) Then End
'  Screenunlock                                           'déverrouiller après Screenlock.
Loop
