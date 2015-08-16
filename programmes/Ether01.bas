diametre = 66
Dim xC(1 To 3): Dim image((diametre * diametre * 2) + 4)
Dim As Single inertie(1 To 3), energie(1 To 3)
Dim As Single influence
Screen 19,24,3: page1 = 1: Gosub Initialisation

'                  Créé le 23 nov. 2005. MODÉLISATION DE L'ÉTHER, ÉTAPE 1.
'     J'ai simplifié l'algorithme ci-dessous le 11 octobre 2006: deux lignes de programme
'     suffisent au lieu de trois (la variable « potentielPrecedent » a été supprimée).
'     Ici, les granules voisins n'ont pas d'influence: il n'y a pas d'échanges.
'     Dans ce cas, chaque « granule » de l'éther ne possède que deux attributs:
' 1 - Une ÉNERGIE, ou un potentiel, une pression, une tension, une extension, un écart, etc.
'     D'un point de vue mathématique, on constate que cette énergie produit une sinusoïde
'     parfaite si elle est affichée en ordonnées sur en graphique, les pas étant en abscisses.
' 2 - Une INERTIE, qu'on peut considérer comme de l'énergie mise en MÉMOIRE comme l'avait
'     fait remarquer M. Philippe Delmotte dès l'invention de l'Éther Virtuel en juin 2005. 
'     Il s'agit de deux forces distinctes, qui peuvent donc aussi être nommées Potentiel1 et
'     Potentiel2 selon l'algorithme de M. Jocelyn Marcotte. Par exemple, le son qui se pro-
'     page dans l'air cause alternativement une compression des molécules de l'air, puis leur
'     déplacement, ce mouvement se traduisant par de l'énergie cinétique selon E=mv^2/2.
'     Ces deux forces évoluent d'une manière complexe dans le cas des ondes progressives,
'     mais elles alternent tout simplement dans le cas des ondes stationnaires.

'     IMPORTANT. - Il faut en conclure que les ondes stationnaires obéissent strictement à
'     la loi de Hooke. On peut les reproduire à l'aide de l'algorithme élémentaire ci-dessous,
'     qui ne comporte que deux lignes de programme. Il est donc faux de prétendre que les
'     ondes stationnaires sont faites de deux trains d'ondes qui se propagent dans des
'     directions opposées. Même si cette assertion se vérifie par le calcul mathématique,
'     le processus mécanique indique au contraire que tout est bien plus simple. Au lieu de
'     se transmettre par « INFLUENCE » d'un granule à l'autre, l'énergie RESTE SUR PLACE.
Do
  Screensync
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  
  For granule = 1 To 3
'******************************* CALCUL SELON LA LOI DE HOOKE *********************************

    inertie(granule) = inertie(granule) - energie(granule)       'un cycle exige 6 étapes au
    energie(granule) = energie(granule) + inertie(granule) / pas 'minimum, qu'on peut multi-
                                                                 'plier en introduisant un pas.
'************************************** FIN DU CALCUL *****************************************
 
    Put(xC(granule), yCentre - energie(granule)), image, Pset
  Next

  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
    Case Chr(27), "k+", "X+": End                         'le « X » de la fenêtre produit k ou parfois X.
    Case "I":  Sleep 10: Gosub Initialisation
    Case "K+": Run "Ether00.exe"                          'flèche gauche.
    Case "M+": Run "Ether02.exe"                          'flèche droite.
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

'---------------------------------- DESSIN DES FLÈCHES ---------------------------------------
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
pas = 200                                                 '6 étapes au minimum lorsque pas = 1.
xCentre = 408
yCentre = 284
rayon = diametre / 2                                      'rayon des sphères ou granules.
xC(1) = 150 - rayon: xC(2) = 400 - rayon: xC(3) = 650 - rayon
energie(1) = 30: energie(2) = 60: energie(3) = 120        'distribution de l'énergie initiale.
inertie(1) = 0: inertie(2) = 0: inertie(3) = 0

For x = 0 To diametre                                     'dessiner une sphère matrice.
  xCa1 = (x - rayon) * (x - rayon)
  xCa2 = (x - 1000) * (x - 1000)
  For y = 0 To diametre
    yCa1 = (y - rayon) * (y - rayon)
    yCa2 = (y - 1000) * (y - 1000)
    diag = Sqr(xCa2 + yCa2)
    lum = 4.5 * (diag - 1414 + diametre + 10)             'dégradé en échelle de gris.
    If lum < 0 Then lum = 0 Else If lum > 255 Then lum = 255
    If Sqr(xCa1 + yCa1) < rayon Then Pset(x,y), Rgb(lum,lum,lum) 
  Next
Next
Circle(rayon,rayon), rayon, Rgb(125,125,125)
Get(0,0)-(diametre,diametre), image
Line(0,0)-(diametre,diametre), fond, BF                   'effacer la sphère matrice.
Line(10, yCentre + rayon)-(xC(1), yCentre + rayon), noir
Line(xC(1) + diametre - 1, yCentre + rayon)-(xC(2) - 1, yCentre + rayon), noir
Line(xC(2) + diametre + 1, yCentre + rayon)-(xC(3) - 1, yCentre + rayon), noir
Line(xC(3) + diametre + 1, yCentre + rayon)-(790, yCentre + rayon), noir

gabarit$ = "Programme Ether01. La loi de Hooke ne fait intervenir"
lignes = 6                                                'nombre de lignes du texte.
th = 7                                                    'haut du texte.
tg = 10                                                   'gauche du texte.
eh = th * 16 - 32                                         'haut de l'encadré.
eb = eh + 16 * lignes + 32                                'bas de l'encadré.
eg = tg * 8 - 24                                          'gauche de l'encadré.
ed = eg + 8 * Len(gabarit$) + 32                          'droite de l'encadré.
xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yF = 584
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc
Locate, 2: Print "Robert Hooke a ‚crit en 1678: ® Ut tensio sic vis ¯  (telle extension, telle force). La loi de"
Locate, 2: Print "Hooke est la devise de l'Ecole Polytechnique de Montr‚al. Elle s'applique entre autres au pendule"
Locate, 2: Print "de Huygens. L'augmentation de la tension sur son ressort ‚tant lin‚aire, c'est … dire proportion-"
Locate, 2: Print "nelle … l'extension, la p‚riode d'oscillation demeure constante quelle que soit l'amplitude."
Locate th
Locate , tg: Print "Programme Ether01. Le pendule ne fait intervenir que"
Locate , tg: Print "deux variables: une ‚nergie et une inertie. Remarquer"
Locate , tg: Print "le synchronisme. De mˆme, fondamentalement, toutes"
Locate , tg: Print "les ondes ne font que convertir alternativement de"
Locate , tg: Print "l'‚nergie cin‚tique en ‚nergie de tension."
Locate , tg: Print "- Programme suivant: cliquez sur la flŠche droite."
locate 28
Locate, 3: Print "L'algorithme qui produit ces mouvements est vraiment ‚l‚mentaire."
Locate, 3: Print "L'‚nergie qui d‚termine les oscillations du pendule est trait‚e"
Locate, 3: Print "r‚p‚titivement selon les ‚quations ci-dessous. Le pas variable"
Locate, 3: Print "permet de pr‚ciser les oscillations, qui sont SINUSOIDALES.": Print
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
Locate , 3: Print "Merci aux cr‚ateurs de FreeBASIC."
Locate , 3: Print "Gabriel LaFreniŠre  glafreniere.com";
Locate 35
Locate , 70: Print "Le 19 oct. 2006. Ce programme "
Locate , 70: Print "FreeBASIC peut ˆtre distribu‚,"
Locate , 70: Print "copi‚ ou modifi‚ librement.   ";
Gosub flecheGauche
Gosub flecheDroite
Return

