screen 19,24,1
dim as Double amplitudeSinus, amplitudeCosinus, diagonale, differenceDeMarche, lambda
dim as Double valeur, flottante, pi, affaiblissement, xCarre, xCoord, yCoord, sequence
dim as Double periode, rotation, phase, phaseLumiere, energie, xCoord1, yCoord1
GOSUB Initialisation     'Étrangement, le calcul est plus rapide avec des variables Double.

DO  'par Gabriel LaFrenière le 12 novembre 2005. La lumière vibre sur une fréquence secondaire.
  
  LOCATE 32, 87: PRINT using "Image ## /"; image;: print images; "  "
  image = image + 1
  sequence = sequence + 1 / images: if sequence > 99 then sequence = 1
  LOCATE 33, 85: PRINT using "S‚quence ## "; sequence
  IF image > images THEN
    image = 1                                             'utile pour les animations.
    locate 34, 87: print using "Temps###.###"; timer - temps' 1,75 sec à 1,7 GHz.
    temps = timer
  end if
  periode = image * 2 * pi / images                       'période de l'électron selon l'image. 
  rotation = sequence * 2 * pi / sequences                'période de l'onde composite.

  FOR x = 0 TO largeur                                    'coordonnée x du pixel.
    xCoord = x - xOnde                                    'distance sur l'axe horizontal.
    xCarre = xCoord * xCoord                              'distance au carré, onde 1 et 2.
    FOR y = 0 TO 200'hauteur                              'coordonnee y du pixel.
'-------------------------- 1. L'électron effectue une rotation.----------------------------- 
      yCoord1 = y - yOnde1                                'distance fixe onde 1, axe vertical.
      diagonale = SQR(xCarre + yCoord1 * yCoord1)         'distance réelle fixe, Pythagore.
      IF diagonale THEN affaiblissement = lambda / 2 / diagonale else affaiblissement = 1
      phaseLumiere = 2 * pi * diagonale / lambdaLumiere   'phase des ondulations.  
      xCoord1 = xCoord + rayon * sin(rotation - phaseLumiere) 'dist. électron axe horizontal.
      yCoord1 = yCoord1 + rayon * cos(rotation - phaseLumiere)'dist. électron axe vertical.
      diagonale = SQR(xCoord1 * xCoord1 + yCoord1 * yCoord1)  'distance réelle variable.
      flottante = diagonale / lambda                      'dist. précise en longueurs d'onde.
      entier = flottante                                  'distance, nombre entier.
      differenceDeMarche = flottante - entier             'difference en longueurs d'onde.
      phase = 2 * pi * differenceDeMarche - periode       'phase en radians.
      amplitudeSinus = affaiblissement * SIN(phase)
      amplitudeCosinus = affaiblissement * COS(phase)     'quadrature, inutilisée ici.
'-------------------- 2. Le proton est fixe et à la quadrature.------------------------------- 
      yCoord = y - yOnde2                                 'distance onde 2, axe vertical.
      diagonale = SQR(yCoord * yCoord + xCarre)           'distance réelle selon Pythagore.
      flottante = diagonale / lambda                      'distance en longueurs d'onde.
      entier = flottante                                  'distance, nombre entier.
      differenceDeMarche = flottante - entier             'difference en longueurs d'onde.
      IF diagonale THEN affaiblissement = lambda / 2 / diagonale else affaiblissement = 1
      phase = 2 * pi * differenceDeMarche - periode + (lambda / 4)'proton à la quadrature.
      amplitudeSinus = amplitudeSinus + affaiblissement * SIN(phase)
      amplitudeCosinus = amplitudeCosinus + affaiblissement * COS(phase)
      energie = sqr(amplitudeSinus * amplitudeSinus + amplitudeCosinus * amplitudeCosinus)
      select case afficher
        case 0: valeur = 5 * amplitudeSinus + .5          'déporter côté positif, en partie.
        case 1: valeur = 5 * energie                            'affiche la racine
        case 2: valeur = 5 * amplitudeSinus + 5 * energie + .2  'carrée de l'énergie.
      end select
      luminance = 255 * valeur                            'luminance en 256 tons de gris.
      if luminance < 0 then luminance = 0
      if luminance > 255 then luminance = 255             'écrêter.
      pset(x,y), rgb(luminance,luminance,luminance)       'afficher le pixel.
    NEXT y
    a$ = ucase(Inkey)                                     'vérifier le clavier.
    Getmouse xSouris, ySouris, roulette, clic             'vérifier la souris.
    lignePrecedente = ligne
    ligne = .5 + ySouris / 16
    if ligne < 31 or ligne > 34 or xSouris < 232 or xSouris > 528 then ligne = 0
    'locate 20, 10: print xSouris, ySouris, clic, ligne; "  "
    if ligne and clic = 1 then
      GOSUB Saisie
    elseif len(a$) or ligne <> lignePrecedente then GOSUB Saisie
    end if
  NEXT x
  
  circle(xOnde - rayon * sin(rotation),yOnde1 - rayon * cos(rotation)),5,1
  paint (xOnde - rayon * sin(rotation),yOnde1 - rayon * cos(rotation)), 1
  circle(xOnde,yOnde1),rayon,0
  circle(xOnde,yOnde2),5,1
  paint (xOnde,yOnde2), 1
  line(xOnde - rayon * sin(rotation),yOnde1 - rayon * cos(rotation))-(xOnde,yOnde2),0:sleep 100
  if sin(rotation+1.2) > 0 then gosub FlecheGauche else gosub FlecheDroite
  locate 1, 30: print " La lumiŠre - The Light  glafreniere.com "
  if bitmap then'                                         '= 1: crée une séquence bitmap.
    select case total
      case is < 10: numero$ = "00" + str(total)
      case is < 100: numero$ = "0" + str(total)
      case is < 1000: numero$ = str(total)
    end select
    fichier$ = "image" + numero$ +".bmp": bsave fichier$,0
    total = total + 1
    color rgb(255,0,0), rgb(225,225,225)
    locate 30,60: print "Captures bitmap en cours:"; total; " /";images * lambdaLumiere / lambda - 5
    if total > images * lambdaLumiere / lambda - 5 then end
    color 0, rgb(225,225,225)
  end if
LOOP

FlecheGauche:'------------------------------------------- électron récepteur, attraction.
circle (xElectron, yElectron), 17, 1
paint (xElectron, yElectron), rgb(225,225,225), 1
line (xElectron-14, yElectron)-(xElectron+14, yElectron), 0
line (xElectron-12, yElectron-1)-(xElectron+14, yElectron-1), 0
line (xElectron-12, yElectron+1)-(xElectron+14, yElectron+1), 0
line (xElectron-10, yElectron+2)-(xElectron-2, yElectron+2), 0
line (xElectron-10, yElectron-2)-(xElectron-2, yElectron-2), 0
line (xElectron-8, yElectron+3)-(xElectron-2, yElectron+3), 0
line (xElectron-8, yElectron-3)-(xElectron-2, yElectron-3), 0
line (xElectron-6, yElectron+4)-(xElectron-2, yElectron+4), 0
line (xElectron-6, yElectron-4)-(xElectron-2, yElectron-4), 0
line (xElectron-4, yElectron+5)-(xElectron-2, yElectron+5), 0
line (xElectron-4, yElectron-5)-(xElectron-2, yElectron-5), 0
return

FlecheDroite:'------------------------------------------- électron récepteur, répulsion.
circle (xElectron, yElectron), 17, rgb(255,254,255)       'blanc avec 254 distinctif.
paint (xElectron, yElectron), rgb(255,254,255), rgb(255,254,255)
circle (xElectron, yElectron), 16, rgb(255,255,255)
paint (xElectron, yElectron), 0, rgb(255,255,255)
line (xElectron-14, yElectron)-(xElectron+14, yElectron), rgb(255,255,255)
line (xElectron-14, yElectron-1)-(xElectron+12, yElectron-1), rgb(255,255,255)
line (xElectron-14, yElectron+1)-(xElectron+12, yElectron+1), rgb(255,255,255)
line (xElectron+2, yElectron+2)-(xElectron+10, yElectron+2), rgb(255,255,255)
line (xElectron+2, yElectron-2)-(xElectron+10, yElectron-2), rgb(255,255,255)
line (xElectron+2, yElectron+3)-(xElectron+8, yElectron+3), rgb(255,255,255)
line (xElectron+2, yElectron-3)-(xElectron+8, yElectron-3), rgb(255,255,255)
line (xElectron+2, yElectron+4)-(xElectron+6, yElectron+4), rgb(255,255,255)
line (xElectron+2, yElectron-4)-(xElectron+6, yElectron-4), rgb(255,255,255)
line (xElectron+2, yElectron+5)-(xElectron+4, yElectron+5), rgb(255,255,255)
line (xElectron+2, yElectron-5)-(xElectron+4, yElectron-5), rgb(255,255,255)
return

Saisie:

if len(a$) = 2 then a$ = right(a$, 1) + "+"
  select case a$
    case "A": afficher = 0
    case "B": afficher = 1
    case "C": afficher = 2
    case "I": gosub initialisation
    case chr(27), "X+": end
  end select

if clic then
  select case ligne
    case 31: afficher = 0
    case 32: afficher = 1
    case 33: afficher = 2
    case 34: end
  end select
end if    

color 1, rgb(225,225,225)
locate 34, 30: print ligne34$
if afficher = 0 then color rgb(0, 0, 255), rgb(225,225,225) else color 1, rgb(225,225,225)
locate 31, 30: if ligne <> 31 then print ligne31$
if afficher = 1 then color rgb(0, 0, 255), rgb(225,225,225) else color 1, rgb(225,225,225)
locate 32, 30: if ligne <> 32 then print ligne32$
if afficher = 2 then color rgb(0, 0, 255), rgb(225,225,225) else color 1, rgb(225,225,225)
locate 33, 30: if ligne <> 33 then print ligne33$

locate ligne, 30: color 1, rgb(230, 255, 255)
select case ligne
  case 31: if afficher > 0 then print ligne31$
  case 32: if afficher <> 1 then print ligne32$
  case 33: if afficher < 2 then print ligne33$
  case 34: print ligne34$
end select
color 1, rgb(225,225,225)
return

'***************************** INITIALISATION ********************************
Initialisation:
color 0, rgb(225,225,225):cls
pi = 4 * atn(1)
lambdaLumiere = 850                                       'des émetteurs au récepteur.
images = 8                                                'nombre d'images par période.
image = 1
lambda = 20                                               'longueur d'onde de l'électron.
rayon = lambda / 2                                        'cercle décrit par l'émetteur.
sequences = lambdaLumiere / lambda                        'nombre de séquences par oscillation.
sequence = 1
largeur = 800                                             'dimensions de la fenêtre.
hauteur = 414
ecart = 6 * lambda - lambda / 4
xOnde= 50                                                 'coordonnées de l'onde.
xElectron = 750                                           'coordonnées de l'électron récepteur.
yElectron = 136
yCentre = 100
yOnde1 = yCentre - ecart / 2
yOnde2 = yCentre + ecart / 2
afficher = 2
'bitmap = 1                                               'pour captures d'écran.
temps = timer
locate 14, 45: print "LA LUMIERE"
locate , 2: print "Dans le coin inf‚rieur gauche, le proton du noyau de l'atome ‚met un rayonnement … la quadrature"
locate , 2: print "comparativement … celui de l'‚lectron qui est montr‚ au-dessus de lui. Lorsqu'il est excit‚ par"
locate , 2: print "un rayonnement inhabituel, cet ‚lectron oscille trŠs l‚gŠrement dans un mouvement de va-et-vient."
locate , 2: print "Il peut aussi d‚crire des cercles comme on le montre ci-dessus. Le rayon du cercle peut ˆtre trŠs"
locate , 2: print "faible, de l'ordre d'une demi-longueur d'onde de l'‚lectron, mais cela suffit pour provoquer des"
locate , 2: print "ondulations dans le rayonnement COMPOSITE qui en r‚sulte. Il s'agit d'ondulations transversales,"
locate , 2: print "ce qui explique la polarisation de la lumiŠre."
print
locate , 2: print "De plus tout ‚lectron r‚cepteur comme celui qui est montr‚ … droite re‡oit des ondes dont la phase"
locate , 2: print "alterne. Il ne s'agit pas d'une modulation d'amplitude ni de fr‚quence: on peut dire qu'il s'agit"
locate , 2: print "plut“t d'une modulation de phase. Cet ‚lectron est donc alternativement attir‚, puis repouss‚, et"
locate , 2: print "c'est pourquoi il ‚mettra … son tour de la nouvelle lumiŠre s'il est captif d'un proton."
print
locate , 2: print "L'‚nergie des deux ‚metteurs se r‚partit sur des hyperboles. Le troisiŠme choix ci-dessous permet"
locate , 2: print "d'afficher … la fois les ondes et leur ‚nergie, d'o— un meilleur contraste."
color rgb(0,150,0), rgb(225,225,225)
locate 35, 2: print "glafreniere.com  "
locate 36, 2: print "Code source FreeBASIC (freebasic.net) par Gabriel LaFreniŠre, le 12 nov. 2005."
locate 37, 2: print "Ce code peut ˆtre distribu‚, copi‚ ou modifi‚ en toute libert‚.";
ligne31$ = " A - Afficher les ondes.             "
ligne32$ = " B - Afficher l'‚nergie.             "
ligne33$ = " C - Afficher les ondes et l'‚nergie."
ligne34$ = "   - Quitter: appuyez sur ® Echap ¯. "
color rgb(0, 0, 255), rgb(225,225,225)
locate 33, 30: print ligne33$
color 1, rgb(225,225,225)
locate 31, 30: print ligne31$
locate 32, 30: print ligne32$
locate 34, 30: print ligne34$
RETURN
