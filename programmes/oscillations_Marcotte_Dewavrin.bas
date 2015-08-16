Screen 19,24,3: Screenset 2,2: Color 0, Rgb(225,225,225)  'par Gabriel LaFreni�re.
Dim As Double pi = 4 * Atn(1), pas, y1, y2, y3            'le 14 d�cembre 2006. glafreniere.com
page1 = 1: yCentre = 600 / 2: gabarit = 50: lambda = 100  'lambda: sous-multiple de 800 requis.
Cls: Line(0, yCentre)-(799, yCentre), noir

y3 = 2 * pi / lambda                                      'd�termine l'amplitude et le pas.
pas = 2 - y3 ^ 2                                          'formule ci-dessous simplifi�e.
Print Using "##.##############"; y3                       'l'impr�cision diminue selon lambda.
Print Using "##.##############"; Sin(2 * pi / lambda)     'c'est la formule exacte.
Print pas                                                 'la diff�rence permet de mesurer avec
Print Sin(4 * pi / lambda) / Sin(2 * pi / lambda)         'pr�cision l'anomalie � quantique �.
'  Ces formules m'ont �t� soumises par M. Anselme Dewavrin. Il s'agit d'une adaptation d'un
'  algorithme de filtre num�rique de type � IIR �. Le pas ainsi calcul� produit une sinuso�de
'  fixe et pr�cise parce qu'il correspond au rapport exact des amplitudes entre deux points
'  distants de 2 * pi. Vous pouvez le v�rifier en supprimant l'apostrophe � REM � ci-apr�s:
'pas = Sin(4 * pi / lambda) / Sin(2 * pi / lambda)
'  L'amplitude y normalis�e � 1 s'obtient par: Sin(2 * pi / lambda), mais elle peut varier:
'y3 = .1                                                  'essayez y3 = .1 pour voir.
Do                                                        'continu pour observer le glissement,
  Swap page1, page2                                       'qui est plus s�v�re avec une
  Screenset page1, page2                                  'longueur d'onde plus courte 
  Pcopy 2, page1                                          '(v�rifiez avec lambda = 40).
  
  For pixel = 0 To 799
    Pset(pixel, yCentre - gabarit * y1), 0                           'courbe de l'algorithme.
    Pset(pixel,yCentre-gabarit*Sin((2*pi*pixel)/lambda)),Rgb(255,0,0)'sinuso�de de r�f�rence.
  
    y1 = pas * y3 - y2
    y2 = y3                                               'm�moriser les deux derniers
    y3 = y1                                               '�tats du � potentiel �.
                                                          'remarquer la similitude avec
  Next                                                    'l'algorithme de M. Jocelyn Marcotte.
Loop Until Len(Inkey)                                     'voir le programme Ether04_Marcotte.
