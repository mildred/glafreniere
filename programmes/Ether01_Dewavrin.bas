Dim As Single sinus, cosinus, sinus2, cosinus2, pas, pas2, phase, lambda, pi = 4 * Atn(1)
Gosub Initialiser

Do
  Swap page1, page2
  Screenset page1, page2
  Screensync
  Pcopy 2, page1
  For x = 0 To 799                                        'multiple de la longueur d'onde - 1.
    Pset(x, yCentre - gabarit * sinus), Rgb(0,150,0)      'sin = 0, cos = 1 avec phase = 0 pi.
    Pset(x, yCentre - gabarit * cosinus), Rgb(225,0,0)
'   Pset(x,yCentre+gabarit*sinus2/pas),Rgb(0,150,255)     'correction selon le pas. Courbes
'   Pset(x,yCentre+gabarit*cosinus2),Rgb(225,0,255)       'affichées en opposition de phase.

'*************** ALGORITHME DE M. ANSELME DEWAVRIN, D'APRÈS LA MÉTHODE D'EULER ****************
    sinus = sinus + cosinus / pas
    cosinus = cosinus - sinus / pas

'    le résultat est le même si le pas (mis au carré) n'intervient qu'une fois:
'    sinus2 = sinus2 + cosinus2                           'sinus inexact, mais cohérent
'    cosinus2 = cosinus2 - sinus2 / pas2                  '(voir correction ci-dessus).
  Next
  nombre += 1: Locate 34,23: Print 800 * nombre
  a$ = Inkey
  If a$ = "p" Or a$ = "P" Then a$ = "": Sleep: Do: Loop While Len(Inkey)
  If a$ = Chr(13) Then a$ = "": sinus = 0: sinus2 = 0: cosinus = 1: cosinus2 = 1: nombre = 0
  If Len(a$) Then End 
Loop

Initialiser:
Screen 19,24,3
page1 = 1
blanc = Rgb(255,255,255)
vert = Rgb(0,150,0)
fond = Rgb(225,225,225)
lambda = 800 / 4                                          'sous-multiple de 800 obligatoire.
gabarit = 150                                             'amplitude en pixels.
cosinus = 1: cosinus2 = 1                                 'cosinus pour 0°.
pas = lambda / (2 * pi)
pas2 =(lambda / (2 * pi)) ^ 2                             'le carré du pas normal.
yCentre = gabarit + 10
Color noir, fond
Screenset 2, 2: Cls: Locate 1
Locate 21
Locate, 3: Print "Leonhard Euler (1707-1783) a cr‚‚ une m‚thode permettant de calculer les fonctions sinuso‹dales."
Locate, 3: Print "En octobre 2006, M. Anselme Dewavrin a mis au point l'algorithme indiqu‚ ci-dessous, qui d‚rive"
Locate, 3: Print "des ‚quations d'Euler. Sachant que la m‚thode d'Euler est approximative, on observe que les"
Locate, 3: Print "sinuso‹des pr‚sentent un d‚calage progressif comparativement … celle dont la valeur est exacte."
Locate, 3: Print "La marge d'erreur diminue … mesure que le pas augmente. On peut l'‚valuer en relevant le nombre"
Locate, 3: Print "d'it‚rations au moment o— le d‚calage atteint une longueur d'onde exactement. En novembre 2005,"
Locate, 3: Print "j'avais moi-mˆme ‚labor‚ un algorithme semblable … partir de l'algorithme cr‚‚ en juin 2005 par"
Locate, 3: Print "M. Philippe Delmotte bas‚ plut“t sur les lois de Newton, et qui produit des ® ondes virtuelles ¯."
Locate 33, 3: Print "pas = lambda / (2 * pi)"
Locate 34, 3: Print "Nombre d'it‚rations:"
Locate 30,3: Print "Courbe verte: le sinus."
Locate 31,3: Print "Courbe rouge: le cosinus."
Locate 30,73: Print "Lambda: 200 pixels, repr‚-"
Locate 31,73: Print "sentant 200 it‚rations."
Locate 32,73: Print "Pause: appuyez sur [ P ]."
Locate 33,73: Print "Initialiser: [ Entr‚e ]."
Color noir, blanc
Line(250,456)-(550,502),noir,bf
Line(252,458)-(548,500),blanc,bf
Locate 30,35: Print "sinus = sinus + cosinus / pas"
Locate 31,35: Print "cosinus = cosinus - sinus / pas"
Locate 36: Color vert, fond
Locate, 3: Print "Merci aux cr‚ateurs de FreeBASIC."
Locate, 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate,70: Print "Le 25 oct. 2006. Ce programme "         'Créé le 24 octobre 2006.
Locate,70: Print "FreeBASIC peut ˆtre distribu‚,"
Locate,70: Print "copi‚ ou modifi‚ librement.   ";
Color noir, fond
Line(0, yCentre)-(799, yCentre), noir                     'axe
For x = 0 To 799                                          'véritable sinusoïde pour comparer.
  Line(x, yCentre - y)-(x, yCentre - gabarit * Sin(2 * pi * x / lambda)), Rgb(150,150,150)
  y = gabarit * Sin(2 * pi * x / lambda)
Next
Return
