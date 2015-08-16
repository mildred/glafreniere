dim as single pi, AB, AC, AD, CB, BD, CD, ACB, ACD, ADB, ADC, BAD, CAB, CAD, cosinus
blanc = rgb(255,255,255)
fond = rgb(245,235,220): 
gris = rgb(180,180,180)
vert = rgb(0,255,0)
ordonnees = 300
abscisses = 200
pi = 4 * atn(1)
page1 = 1
Ax = 400: Ay = 100
Bx = Ax:  By = ordonnees
Cx = abscisses: Cy = ordonnees
Dx = 700: Dy = ordonnees
CD = Dx - Cx

SCREEN 19,24,3
screenset 2, 2'********************** SECTION PERMANENTE ************************************
color noir, fond: cls

for j = 100 to ordonnees - 100 step 100                   'quadrillage.
  line(0,j)-(800,j),gris
next
for j = 0 to 700 step 100
  line(j,100)-(j,ordonnees),gris
next
line(0,ordonnees)-(800,ordonnees),noir                    'axe des abscisses en ordonnees.
line(abscisses,100)-(abscisses,ordonnees),noir            'axe des ordonnees en abscisses.
locate 2,74: print "Souris:"
locate 2,87: print "Sommet A :"

'************************************* SECTION MODIFIABLE ************************************
do
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  saisie$ = Inkey
  If Len(saisie$) Then
    If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
    select case saisie$
      case "H+": Ay = Ay - 1: if Ay < 100 then Ay = 100
      case "P+": Ay = Ay + 1: if Ay > ordonnees - 2 then Ay = ordonnees - 2
      case "K+": Ax = Ax - 1: if Ax < 0 then Ax = 0
                 Bx = Ax
      case "M+": Ax = Ax + 1: if Ax > 799 then Ax = 799
                 Bx = Ax
      case "k+",CHR$(27):end
    end select
  end if
  
  getmouse xSouris, ySouris, , clic                       'saisie Souris.
  if ySouris < 100 or ySouris > ordonnees - 2 then clic = 0
  xSouris = xSouris - abscisses
  ySouris = ordonnees - ySouris
  if clic = 1 then
    Ax = xSouris + abscisses
    Bx = Ax
    Ay = ordonnees - ySouris
  end if
'*********************************** CALCUL DES DONNÉES **************************************
  AB = ordonnees - Ay                                     'côtés.
  CB = abs(Bx - abscisses)
  BD = abs(Dx - Bx)
  AC = sqr(AB * AB + CB * CB)
  AD = sqr(AB * AB + BD * BD)
  ACB = atn(AB / CB)                                      'division par zéro acceptée !!!
  ADB = atn(AB / BD)
  BAD = atn(BD / AB)
  CAB = atn(CB / AB)

'************************ LOI DES COSINUS, OU THÉORÈME D'AL KASHI ****************************
' l'effet Doppler « relatif » est aussi fondé sur le théorème d'Al Kashi:
' Doppler = lambda * (cos(asin(beta * sin(phi))) - beta * cos(phi))

  ACD = acos((CD ^ 2 + AC ^ 2 - AD ^ 2) / (2 * CD * AC))
  ADC = acos((CD ^ 2 + AD ^ 2 - AC ^ 2) / (2 * CD * AD))
  CAD = acos((AD ^ 2 + AC ^ 2 - CD ^ 2) / (2 * AD * AC))


  locate 1
  locate , 1: print "                   TRIGONOMETRIE                   "
  locate , 1: print "  Vous pouvez d‚placer le sommet du triangle avec  " 
  locate , 1: print "  la souris ou … l'aide des flŠches du clavier.    "
  locate , 1: print "  F‚v. 2006. Pour quitter, appuyez sur [ Echap ].  " 
  locate 3, 87: print "x = "; Ax - abscisses              'coordonnées du sommet.
  locate 4, 87: print "y = "; ordonnees - Ay
  locate 3, 74: if xSouris <> -1 - abscisses then print "x = "; xSouris
  locate 4, 74: if ySouris <> 1 + ordonnees then print "y = "; ySouris
  ver = (Ay - 5) / 16: if ver < 1 then ver = 1 
  hor = (Ax + 3) / 8:  if hor < 1 then hor = 1 
  locate ver, hor: print "A";
  locate 20, hor:  print "B";
  locate 20, 25:   print "C";
  locate, 88:      print "D";
  locate 2, 60: print "AB = "; AB
  locate 3, 60: print "BD = "; BD
  locate 4, 60: print "CB = "; CB
  locate 5, 60: print "CD = "; CD
  
  locate 22
  locate, 2:  print "AC = "; AC;
  locate, 24: print "AC = SQR(AB * AB + CB * CB) ...th‚orŠme de Pythagore, ou comme ci-dessous:"
  locate, 2:  print "AD = "; AD;
  locate, 24: print "AD = AB / SIN(ADB)      AD = AB / COS(BAD)      AD = BD / SIN(BAD)"
  locate, 2:  print "AB = "; CB * TAN(ACB);
  locate, 24: print "AB = CB * TAN(ACB)      AB = BD * TAN(ADC)      AB = AD * SIN(ADB)"
  locate, 2:  print "BD = "; AB * TAN(BAD);
  locate, 24: print "BD = AB * TAN(BAD)      BD = AD * SIN(BAD)      BD = AD * COS(ADB)"
  print
  locate, 2:  print "ADC ="; ADC * 180 / pi; chr(248);
  locate, 24: print "ADC = ACOS((CD ^ 2 + AD ^ 2 - AC ^ 2) / (2 * CD * AD)) ...loi des cosinus."
  locate, 2:  print "CAD ="; CAD * 180 / pi; chr(248);
  locate, 24: print "CAD = ACOS((AD ^ 2 + AC ^ 2 - CD ^ 2) / (2 * AD * AC))"
  locate, 2:  print "ACD ="; ACD * 180 / pi; chr(248);
  locate, 24: print "ACD = ACOS((CD ^ 2 + AC ^ 2 - AD ^ 2) / (2 * CD * AC))"
  
  cosinus = (CD ^ 2 + AC ^ 2 - AD ^ 2) / (2 * CD * AC)    'calcul via le cosinus.
  ACD = atn(sqr(1 - cosinus ^ 2) / cosinus)
  print
  locate, 2:  print "ACB ="; ACB * 180 / pi; chr(248);
  locate, 24: print "ACB = ATN(AB / CB)      ACB = ASIN(AB / AC)     ACB = ACOS(CB / AC)"
  locate, 2:  print "ADB ="; ADB * 180 / pi; chr(248);
  locate, 24: print "ADB = ATN(AB / BD)      ADB = ASIN(AB / AD)     ADB = ACOS(BD / AD)"
  locate, 2:  print "BAD ="; BAD * 180 / pi; chr(248);
  locate, 24: print "BAD = ATN(BD / AB)      BAD = ASIN(BD / AD)     BAD = ACOS(AB / AD)"
  locate, 2:  print "CAB ="; CAB * 180 / pi; chr(248);
  locate, 24: print "CAB = ATN(CB / AB)      CAB = ASIN(CB / AC)     CAB = ACOS(AB / AC)"
  print
  locate, 2: print "Loi des cosinus (th‚orŠme d'Al-Kashi) : c ^ 2 = a ^ 2 + b ^ 2 - 2 * a * b * cos(angle)"
  color rgb(0,150,0)
  locate 37, 2: print "Ce programme peut ˆtre distribu‚, copi‚ ou modifi‚ librement. Gabriel LaFreniŠre. glafreniere.com";
  color noir
  line(Ax,Ay)-(Cx,Cy), 1
  line(Ax,Ay)-(Dx,Dy), 1
  line(Cx,Cy)-(Dx,Dy), 1
  paint((Cx+Dx)/2, ordonnees -1), blanc, 1
  line(Ax,Ay)-(Bx,By), 1
loop
