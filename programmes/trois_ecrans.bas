Screen 18,24,3' �cran 18: 640 x 480 pixels; code 24: 256 tons par couleur; code 3: trois �crans.
x = 100: y = 250: page1 = 1: pas = 1
Color 0, Rgb(225,225,225): Screenset 2: Cls 'la page No. 2 est la page-m�re, la TROISI�ME page...
Locate 2, 26: Print "LA PROGRAMMATION A TROIS ECRANS"
Locate 23, 3: Print "Le programme affiche deux �crans en alternance.";
Locate 24, 3: Print "Cette m�thode permet de r�aliser des animations plus fluides."
Locate 25, 3: Print "De plus, FreeBASIC travaille plus rapidement sur une page cach�e."
Locate 26, 3: Print "Une page-m�re additionnelle permet d'afficher un arri�re-plan"
Locate 27, 3: Print "permanent sans devoir le recr�er constamment."
Locate 29, 3: Print "Pour quitter, appuyez sur une touche.  Gabriel LaFreni�re  glafreniere.com";

Do                         'modifier la page cach�e pendant que l'autre est affich�e.
  Swap page1, page2        'les valeurs 1 et 0 sont �chang�es.
  screensync               'attendre le cycle (g�n�ralement 85 Hz) pour r�gulariser l'animation.
  Screenset page1, page2   'travailler sur la page cach�e, afficher la page visible.
'  Screenlock              'parfois utile en programmation avanc�e.
  Pcopy 2, page1           'copier la page-m�re sur la page de travail (cach�e).
  x = x + pas
  If x > 538 Or x < 100 Then pas = - pas
  Circle (x,y), 30, 0                                     'cercle mobile, pour d�monstration.
  Paint (x,y), Rgb(255 - (x - 100) / 2.12, x / 2.12, 0), 0'passer de rouge � vert.
  
  If page1 Then'                                          'une fois sur deux.
    Line(270,50)-(370,150),Rgb(0,0,255), bf               'carr� bleu.
    Locate 5,38: Print "Page 1"
  Else
    Line(270,50)-(370,150),Rgb(255,0,0), bf               'carr� rouge.
    Locate 6,38: Print "Page 2"
  End If
  If Len(inkey) Then End
'  Screenunlock                                           'd�verrouiller apr�s Screenlock.
Loop
