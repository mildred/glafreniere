Dim As Single pi = 4 * Atn(1), x, rotation, distance, amplitude
Dim As Single beta, theta, sinTheta, cosTheta, tPrime, xCoord, xPrime
Screen 19,24,3: page1 = 1: Gosub Initialiser              'cr�� le 26 juillet 2006.
'******************** L'�LECTRON SELON LES TRANSFORMATIONS DE LORENTZ ************************
Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  image = image + 1: If image > images Then image = 1
  rotation = 2 * pi * image / images                      'rotation de phase selon l'image.

  Gosub Stationnaire                                      'ondes stationnaires sph�riques.
  Gosub ElectronMobile                                    '�lectron mobile.
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
  Case Chr(27), "k+": End
  Case "I": Gosub Initialiser
  Case "P": Sleep: sleep 100: Do: Loop While Len(Inkey)
  Case "+": lambda = lambda + 10: If lambda > 200 Then lambda = 200
    Screenset 2,2: Locate 33, 38: ? lambda; " pixels. ": Screenset page1, page2
  Case "-": lambda = lambda - 10: If lambda < 10 Then lambda = 10
    Screenset 2,2: Locate 33, 38: ? lambda; " pixels. ": Screenset page1, page2
  Case "0","1","2","3","4","5","6","7","8","9"
    beta = Val(saisie$) / 10
    theta = Asin(beta)
    sinTheta = Sin(theta)
    cosTheta = Cos(theta)
    Screenset 2,2: Locate 34,63: Print Using "#.#"; beta;
    Print Using " = v / c  Theta = ##.#"; Asin(beta) * 180 / pi;: Print Chr(248): Screenset page1, page2
  End Select
  Do: Loop While Len(Inkey)
Loop

ElectronMobile:'********************** ELECTRON MOBILE ***************************************

For xCoord = -xCentre / cosTheta To xCentre / cosTheta Step 1 / cosTheta
'   remplir l'espace additionnel disponible apr�s contraction du syst�me (facultatif).

'*********************** APPLICATION DES TRANSFORMATIONS DE LORENTZ **************************

'   Ether17 montre un �lectron qui se d�place: alors les formules de Lorentz s'appliquent
'   int�gralement. Toutefois, l'�lectron mobile occupe ici une position fixe sur l'�cran. Le
'   temps t des �quations ne concernant que le mouvement de translation, il n'est pas utile.
'   On a constamment t = 0 et la partie des �quations avec � t � peut �tre �limin�e:
'   x' = x * Cos(theta) + t * Sin(theta)
'   t' = t * Cos(theta) - x * Sin(theta)
'   avec theta = arc sin (v / c). Mais avec t = 0, on peut simplifier:
'   x' = x * Cos(theta)
'   t' = -x * Sin(theta)
'   La variable x' indique que les ondes de la mati�re se contractent � grande vitesse.
'   Cette variable x' exprim�e en secondes lumi�re doit normalement �tre convertie
'   en longueurs d'onde. Mais ici, les distances sont exprim�es en pixels.
'   La variable t' indique que ces ondes subissent un d�calage horaire.
'   Cette variable t' exprim�e en secondes doit ici �tre convertie en p�riode d'onde.
'   Malheureusement, la disparition du temps � t � annule le ralentissement de la p�riode
'   de l'�lectron mobile, qu'on peut toutefois observer avec le programme Ether17.
'   Il se produit des variations dans la p�riode le long de l'axe du d�placement, qui
'   ont pour effet de provoquer l'effet Doppler et l'apparition d'une � onde de phase �.
'   Il suffit donc finalement d'appliquer ces deux formules d'une grande simplicit�:

  xPrime =  xCoord * cosTheta
  tPrime = -xCoord * sinTheta

'*********************************************************************************************
  tPrime = 2 * pi * tPrime / lambda                       'conversion en p�riode d'onde
  xCarre = xCoord ^ 2
  For yCoord = 0 To yCentre                               'sym�trie verticale.
    distance = Sqr(yCoord * yCoord + xCarre)              'distance selon Pythagore.
    x = 2 * pi * distance / lambda                        'conversion en longueurs d'onde.
    If x Then amplitude  = Sin(x) / x * Sin(rotation + tPrime) Else amplitude = Sin(rotation)' t' = 0
    If amplitude > 0 Then
      rouge = amplitude * 2000
      bleu = rouge / 3
      If rouge > 255 Then vert = (rouge - 255) / 2 Else vert = 0
    Else
      vert = -amplitude * 2000
      bleu = vert / 3
      If vert > 255 Then rouge = (vert - 255) / 2 Else rouge = 0
    End If
    If rouge > 255 Then rouge = 255
    If vert  > 255 Then vert  = 255
    If bleu  > 255 Then bleu  = 255
    Pset(xPrime + xCentre2, yCentre - yCoord), Rgb(rouge,vert,bleu)
    Pset(xPrime + xCentre2, yCentre + yCoord), Rgb(rouge,vert,bleu)
    If yCoord = 0 Then
      Line(xPrime + xCentre2, yPrecedent)-(xPrime + xCentre2, yCentre2 + 65 * amplitude)
      yPrecedent = yCentre2 + 65 * amplitude
    End If
  Next
Next
Return

Stationnaire:'******************** ELECTRON STATIONNAIRE *************************************

Line(0,yCentre2)-(799,yCentre2), Rgb(180,180,180)
Line(399, 0)-(400,yCentre2), Rgb(128,128,128), bf
For xCoord = -xCentre To xCentre
  xCarre = xCoord ^ 2
  For yCoord = 0 To yCentre                               'sym�trie verticale.
    distance = Sqr(yCoord * yCoord + xCarre)              'distance selon Pythagore.
    x = 2 * pi * distance / lambda                        'distance en radians.
    If x Then amplitude  = Sin(x) / x * Sin(rotation) Else amplitude = Sin(rotation)
    If amplitude > 0 Then
      rouge = amplitude * 2000
      bleu = rouge / 3
      If rouge > 255 Then vert = (rouge - 255) / 2 Else vert = 0
    Else
      vert = -amplitude * 2000
      bleu = vert / 3
      If vert > 255 Then rouge = (vert - 255) / 2 Else rouge = 0
    End If
    If rouge > 255 Then rouge = 255
    If vert  > 255 Then vert  = 255
    If bleu  > 255 Then bleu  = 255
    Pset(xCoord + xCentre, yCentre - yCoord), Rgb(rouge,vert,bleu)
    Pset(xCoord + xCentre, yCentre + yCoord), Rgb(rouge,vert,bleu)
    If yCoord = 0 Then
      Line(xCoord + xCentre, yPrecedent)-(xCoord + xCentre, yCentre2 + 65 * amplitude)
      yPrecedent = yCentre2 + 65 * amplitude
    End If
  Next
Next
Return

Initialiser:'************************* INITIALISATION ****************************************
fond  = Rgb(225,225,225)
beta = .8
theta = Asin(beta)
sinTheta = Sin(theta)
cosTheta = Cos(theta)
lambda = 140
hauteur = 300
largeur = 400
xCentre = largeur / 2 - 1
xCentre2 = 800 - largeur / 2
yCentre = hauteur / 2
yCentre2 = hauteur + 82
Screenset 2, 2                                            'cr�er une page matrice.
Color noir, fond: Cls
images = 48

Locate 20,80: ?"theta = Asin(v / c)"
Locate 20,52: ?"x'=  x * Cos(theta)"
Locate 21,52: ?"t'= -x * Sin(theta)"
Locate 29,3:  ?"L'ELECTRON selon les transformations de Lorentz (ce n'est rien d'autre qu'un effet Doppler !)"
Locate 30,3:  ?"Pour montrer la structure des ondes stationnaires, il vaut mieux afficher les ondes en couleurs."
Locate 31,3:  ?"Le vert et le rouge indiquent la phase et l'opposition de phase."
Locate 32,3:  ?"Le blanc indique les zones o� l'amplitude est tr�s grande; le noir, celles o� elle est nulle."
Locate 33,3:  ?"Zoom: appuyez sur + ou - . Lambda ="; lambda; " pixels."
Locate 34,3:  ?"Vitesse normalis�e: appuyez sur un chiffre de 0 � 9. B�ta = ";: Print Using "#.#"; beta;
Print Using " = v / c  theta = ##.#"; Asin(beta) * 180 / pi;: Print Chr(248)
Locate 35,3:  ?"Initialiser: appuyez sur I."
Locate 36,3:  ?"Pause ou ralenti: appuyez sur P."
Locate 37,3:  ?"QUITTER : Appuyez sur Echap.";
Locate 36,38: ?"Gabriel LaFreni�re, le 17 octobre.  glafreniere.com"
Locate 37,38: ?"Ce programme peut �tre copi�, modifi� ou distribu� librement.";
Pcopy 2, page1
Return
