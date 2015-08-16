diametre = 66
Dim xC(1 To 3): Dim image((diametre * diametre * 2) + 4)
Dim As Single inertie(1 To 3), energie(1 To 3)
Dim As Single influence
Screen 19,24,3: page1 = 1: Gosub Initialisation

'                  Cr�� le 23 nov. 2005. MOD�LISATION DE L'�THER, �TAPE 1.
'     J'ai simplifi� l'algorithme ci-dessous le 11 octobre 2006: deux lignes de programme
'     suffisent au lieu de trois (la variable � potentielPrecedent � a �t� supprim�e).
'     Ici, les granules voisins n'ont pas d'influence: il n'y a pas d'�changes.
'     Dans ce cas, chaque � granule � de l'�ther ne poss�de que deux attributs:
' 1 - Une �NERGIE, ou un potentiel, une pression, une tension, une extension, un �cart, etc.
'     D'un point de vue math�matique, on constate que cette �nergie produit une sinuso�de
'     parfaite si elle est affich�e en ordonn�es sur en graphique, les pas �tant en abscisses.
' 2 - Une INERTIE, qu'on peut consid�rer comme de l'�nergie mise en M�MOIRE comme l'avait
'     fait remarquer M. Philippe Delmotte d�s l'invention de l'�ther Virtuel en juin 2005. 
'     Il s'agit de deux forces distinctes, qui peuvent donc aussi �tre nomm�es Potentiel1 et
'     Potentiel2 selon l'algorithme de M. Jocelyn Marcotte. Par exemple, le son qui se pro-
'     page dans l'air cause alternativement une compression des mol�cules de l'air, puis leur
'     d�placement, ce mouvement se traduisant par de l'�nergie cin�tique selon E=mv^2/2.
'     Ces deux forces �voluent d'une mani�re complexe dans le cas des ondes progressives,
'     mais elles alternent tout simplement dans le cas des ondes stationnaires.

'     IMPORTANT. - Il faut en conclure que les ondes stationnaires ob�issent strictement �
'     la loi de Hooke. On peut les reproduire � l'aide de l'algorithme �l�mentaire ci-dessous,
'     qui ne comporte que deux lignes de programme. Il est donc faux de pr�tendre que les
'     ondes stationnaires sont faites de deux trains d'ondes qui se propagent dans des
'     directions oppos�es. M�me si cette assertion se v�rifie par le calcul math�matique,
'     le processus m�canique indique au contraire que tout est bien plus simple. Au lieu de
'     se transmettre par � INFLUENCE � d'un granule � l'autre, l'�nergie RESTE SUR PLACE.
Do
  Screensync
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  
  For granule = 1 To 3
'******************************* CALCUL SELON LA LOI DE HOOKE *********************************

    inertie(granule) = inertie(granule) - energie(granule)       'un cycle exige 6 �tapes au
    energie(granule) = energie(granule) + inertie(granule) / pas 'minimum, qu'on peut multi-
                                                                 'plier en introduisant un pas.
'************************************** FIN DU CALCUL *****************************************
 
    Put(xC(granule), yCentre - energie(granule)), image, Pset
  Next

  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
    Case Chr(27), "k+", "X+": End                         'le � X � de la fen�tre produit k ou parfois X.
    Case "I":  Sleep 10: Gosub Initialisation
    Case "K+": Run "Ether00.exe"                          'fl�che gauche.
    Case "M+": Run "Ether02.exe"                          'fl�che droite.
    Case "M":  Run "Ether00.exe"                          'menu principal.
  End Select

  Getmouse xSouris, ySouris, , clic                       'saisie sourie.
  ligne = .5 + ySouris / 16
  If ligne < 35 Or ligne > 37 Or xSouris < 328 Or xSouris > 488 Then ligne = 0
  
  If ligne Then
    Color noir, turquoise
    Locate ligne, 42
    Select Case ligne
      Case 35: Print ligne35$: If clic = 1 Then Sleep 10: Gosub Initialisation
      Case 36: Print ligne36$: If clic = 1 Then End
      Case 37: Print ligne37$;
               If xSouris < xCentre Then Gosub flecheGauche Else Gosub flecheDroite
               If clic = 1 Then
                 If xSouris < xCentre Then Run "Ether00.exe" Else Run "Ether02.exe"
               End If
    End Select
    Color noir, fond
  End If
Loop

'---------------------------------- DESSIN DES FL�CHES ---------------------------------------
flecheGauche:
Line (xgg+6,yF-4)-(xgg+8,yF-4),noir
Line (xgg+4,yF-3)-(xgg+8,yF-3),noir
Line (xgg+2,yF-2)-(xgg+8,yF-2),noir
Line (xgg,yF-1)-(xgd,yF-1),noir
Line (xgg-2,yF)-(xgd,yF),noir
Line (xgg,yF+1)-(xgd,yF+1),noir
Line (xgg+2,yF+2)-(xgg+8,yF+2),noir
Line (xgg+4,yF+3)-(xgg+8,yF+3),noir
Line (xgg+6,yF+4)-(xgg+8,yF+4),noir
Return

flecheDroite:
Line (xdd-8,yF-4)-(xdd-6,yF-4),noir
Line (xdd-8,yF-3)-(xdd-4,yF-3),noir
Line (xdd-8,yF-2)-(xdd-2,yF-2),noir
Line (xdg,yF-1)-(xdd,yF-1),noir
Line (xdg,yF)-(xdd+2,yF),noir
Line (xdg,yF+1)-(xdd,yF+1),noir
Line (xdd-8,yF+2)-(xdd-2,yF+2),noir
Line (xdd-8,yF+3)-(xdd-4,yF+3),noir
Line (xdd-8,yF+4)-(xdd-6,yF+4),noir
Return

Initialisation:
Screenset 2, 2
blanc = Rgb(255,255,255)                                  'couleurs usuelles.
gris = Rgb(150,150,150)
fond = Rgb(225,225,225)
vert = Rgb(0,150,0)
turquoise = Rgb (230,255,255)
Color noir, fond: Cls
pas = 200                                                 '6 �tapes au minimum lorsque pas = 1.
xCentre = 408
yCentre = 284
rayon = diametre / 2                                      'rayon des sph�res ou granules.
xC(1) = 150 - rayon: xC(2) = 400 - rayon: xC(3) = 650 - rayon
energie(1) = 30: energie(2) = 60: energie(3) = 120        'distribution de l'�nergie initiale.
inertie(1) = 0: inertie(2) = 0: inertie(3) = 0

For x = 0 To diametre                                     'dessiner une sph�re matrice.
  xCa1 = (x - rayon) * (x - rayon)
  xCa2 = (x - 1000) * (x - 1000)
  For y = 0 To diametre
    yCa1 = (y - rayon) * (y - rayon)
    yCa2 = (y - 1000) * (y - 1000)
    diag = Sqr(xCa2 + yCa2)
    lum = 4.5 * (diag - 1414 + diametre + 10)             'd�grad� en �chelle de gris.
    If lum < 0 Then lum = 0 Else If lum > 255 Then lum = 255
    If Sqr(xCa1 + yCa1) < rayon Then Pset(x,y), Rgb(lum,lum,lum) 
  Next
Next
Circle(rayon,rayon), rayon, Rgb(125,125,125)
Get(0,0)-(diametre,diametre), image
Line(0,0)-(diametre,diametre), fond, BF                   'effacer la sph�re matrice.
Line(10, yCentre + rayon)-(xC(1), yCentre + rayon), noir
Line(xC(1) + diametre - 1, yCentre + rayon)-(xC(2) - 1, yCentre + rayon), noir
Line(xC(2) + diametre + 1, yCentre + rayon)-(xC(3) - 1, yCentre + rayon), noir
Line(xC(3) + diametre + 1, yCentre + rayon)-(790, yCentre + rayon), noir

gabarit$ = "Programme Ether01. La loi de Hooke ne fait intervenir"
lignes = 6                                                'nombre de lignes du texte.
th = 7                                                    'haut du texte.
tg = 10                                                   'gauche du texte.
eh = th * 16 - 32                                         'haut de l'encadr�.
eb = eh + 16 * lignes + 32                                'bas de l'encadr�.
eg = tg * 8 - 24                                          'gauche de l'encadr�.
ed = eg + 8 * Len(gabarit$) + 32                          'droite de l'encadr�.
xgg = xCentre - 50                                        'coordonn�es des fl�ches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yF = 584
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadr�.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc
Locate, 2: Print "Robert Hooke a �crit en 1678: � Ut tensio sic vis �  (telle extension, telle force). La loi de"
Locate, 2: Print "Hooke est la devise de l'Ecole Polytechnique de Montr�al. Elle s'applique entre autres au pendule"
Locate, 2: Print "de Huygens. L'augmentation de la tension sur son ressort �tant lin�aire, c'est � dire proportion-"
Locate, 2: Print "nelle � l'extension, la p�riode d'oscillation demeure constante quelle que soit l'amplitude."
Locate th
Locate , tg: Print "Programme Ether01. Le pendule ne fait intervenir que"
Locate , tg: Print "deux variables: une �nergie et une inertie. Remarquer"
Locate , tg: Print "le synchronisme. De m�me, fondamentalement, toutes"
Locate , tg: Print "les ondes ne font que convertir alternativement de"
Locate , tg: Print "l'�nergie cin�tique en �nergie de tension."
Locate , tg: Print "- Programme suivant: cliquez sur la fl�che droite."
locate 28
Locate, 3: Print "L'algorithme qui produit ces mouvements est vraiment �l�mentaire."
Locate, 3: Print "L'�nergie qui d�termine les oscillations du pendule est trait�e"
Locate, 3: Print "r�p�titivement selon les �quations ci-dessous. Le pas variable"
Locate, 3: Print "permet de pr�ciser les oscillations, qui sont SINUSOIDALES.": Print
Line(27, 505)-(315, 550), noir, bf
Line(29, 507)-(313, 548), blanc, bf
color noir, blanc
Locate, 6: Print "inertie = inertie - energie"
Locate, 6: Print "energie = energie + inertie / pas"
color noir, fond
ligne35$ = "  I - Initialiser.  ": Locate 35, 42: Print ligne35$
ligne36$ = "  Quitter (Echap).  ": Locate 36, 42: Print ligne36$
ligne37$ = "                    "
Locate 36: Color vert
Locate , 3: Print "Merci aux cr�ateurs de FreeBASIC."
Locate , 3: Print "Gabriel LaFreni�re  glafreniere.com";
Locate 35
Locate , 70: Print "Le 19 oct. 2006. Ce programme "
Locate , 70: Print "FreeBASIC peut �tre distribu�,"
Locate , 70: Print "copi� ou modifi� librement.   ";
Gosub flecheGauche
Gosub flecheDroite
Return

