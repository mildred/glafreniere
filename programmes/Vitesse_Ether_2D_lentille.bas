largeur = 800: hauteur = 460                              'nombre de particules par côté.
DIM As Single P1(-1 TO largeur+1, -1 TO hauteur+1)
DIM As Single P2(-1 TO largeur+1, -1 TO hauteur+1)
DIM As Single P3(-1 TO largeur+1, -1 TO hauteur+1)
DIM As single Selection, GainA, GainB, GainC, SommeA, SommeB, SommeC, Vitesse, GA, GB, GC

DIM as single pi, lambda, t, Rayon

SCREEN 19,24,3: page2 = 1: GOSUB Initialisation


Selection = 0.25         ' Valeur qui crée le moins de distortion lorsque Lambda est petit.
GainA = 4*Selection-2    ' Gain appliqué à la valeur précédente du granule courant.
GainB = 1-2*Selection    ' Gain appliqué à la valeur précédente des 4 granules voisins (sur les axes).
GainC = Selection        ' Gain appliqué à la valeur précédente des 4 granules voisins (sur les diagonales).


Rayon = 288
RayonCarre = Rayon^2

DO'                      MODÉLISATION DE L'ÉTHER EN DEUX DIMENSIONS.
'______________________________________________________________________________________________
'                             CALCUL SELON M. JOCELYN MARCOTTE                               

  t = t + 2*pi/Lambda : if t > 2*pi then t -= 2*pi        'générer une onde plane venant de la gauche
  FOR y = -1 TO hauteur+1
    P1(0,y) = 10 * SIN(t)
  NEXT


  FOR x = -1 TO largeur+1 : FOR y = -1 TO hauteur+1       'mémoriser les deux derniers états
      P3(x,y) = P2(x,y)                                   'du potentiel.
      P2(x,y) = P1(x,y)
  NEXT : NEXT


  FOR y = -1 TO hauteur+1                                 'pas de réflexion
    P2(-1, y)     = P3(0,y)                               '  à gauche
    P2(largeur+1, y) = P3(largeur,y)                      '  à droit
  NEXT
  FOR x = -1 TO largeur+1                                 'réflexion molle
    P2(x, -1)     = P2(x, 1)                              '  en haut
    P2(x, hauteur+1) = P2(x, hauteur-1)                   '  en bas
  NEXT

                                                          'Calcul du nouveau potentiel
  FOR x = 0 TO largeur : FOR y = 0 TO hauteur             '---------------------------

    'Les ondes vont plus lentement à l'intérieur du cercle
    Vitesse = 1
    IF ((x-xCentre)^2 + (y-yCentre)^2) < RayonCarre and x < 200 THEN Vitesse = 0.66
    GC = GainC * Vitesse^2
    GB = GainB * Vitesse^2
    GA = 2 - 4*GB - 4*GC

    SommeA  = P2(x,  y  )
    SommeB  = P2(x,  y-1) + P2(x,  y+1) + P2(x+1,y  ) + P2(x-1,y  )
    SommeC  = P2(x-1,y-1) + P2(x+1,y+1) + P2(x+1,y-1) + P2(x-1,y+1)
    P1(x,y) = GA*SommeA + GB*SommeB + GC*SommeC - P3(x,y)

  NEXT : NEXT

  
  circle(xCentre, yCentre), rayon, blanc, pi - .69, pi + .69
  line(200,yCentre - 184)-(200, yCentre + 184), blanc     'diamètre: 368 pixels / 2 = 184.
  line(692,yCentre - 30)-(692, yCentre + 30), blanc
  line(682,yCentre - 30)-(702, yCentre - 30), blanc
  line(682,yCentre + 30)-(702, yCentre + 30), blanc

  if clic = 0 then getmouse xSouris, ySouris, , clic      'vérifier entre les affichages.
  swap page1, page2
  screenset page1, page2
  pcopy 2, page1
  gosub afficherCouleurs'------------ DIAGRAMME PRINCIPAL -------------------------------------


'--------------------------------------- SAISIE CLAVIER ---------------------------------------
  saisie$ = inkey
  if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+" else saisie$ = ucase(saisie$)
  if saisie$ = "k+" then end 
  if len(saisie$) then
    bitmap = 0
    if len(saisie$) = 2 then saisie$ = right(saisie$, 1) + "+"': locate 34,2: print saisie$
    select case saisie$
      case "k+",chr$(27): end
      case "I": gosub Initialisation
      case "P": sleep
    end select
  end if
'----------------------------------------- SAISIE SOURIS --------------------------------------

  if clic = 0 then getmouse xSouris, ySouris, , clic
  ligne = .5 + ySouris / 16
'    locate 30, 80: print xSouris; ySouris; clic; ligne; "    ";
  if ligne > 26 and ligne < 38 then
    if xSouris < 304 or xSouris > 496 then ligne = 0
  else ligne = 0  
  end if
  
'-------------------------------------- REHAUSSER L'AFFICHAGE ---------------------------------
  color noir, turquoise
  locate ligne, 39
  select case ligne
    case 35: print ligne35$      
    case 36: print ligne36$
 end select
  color noir, fond
'-------------------------------------- ACTIONS SUITE À UN CLIC -------------------------------
  if clic = 1 then
    bitmap = 0
    select case ligne
      case 35: gosub Initialisation
      case 36: end
    end select
  end if
  if bitmap then gosub Bitmaps                           'capture d'images si désiré.
  clic = 0
loop


afficherCouleurs:'------------------ AFFICHER EN COULEURS -------------------------------------
for x = 0 to largeur : for y = 0 to hauteur
    if P1(x, y) < 0 then
      luminance = luminosite * -P1(x, y)
      vert = luminance
      bleu = vert / 3
      if vert > 255 then rouge = (vert - 255) / 2 else rouge = 0
    else
      luminance = luminosite * P1(x, y)
      rouge = luminance
      bleu = rouge / 3
      if rouge > 255 then vert = (rouge - 255) / 2 else vert = 0
    end if
    if rouge > 255 then rouge = 255 else if rouge < 0 then rouge = 0
    if vert > 255 then vert = 255 else if vert < 0 then vert = 0
    if bleu > 255 then bleu = 255 else if bleu < 0 then bleu = 0
    pset (x, y), rgb(rouge,vert,bleu)
  next
next
return


Bitmaps:'------------------------- Créer une séquence bitmap ---------------------------------
select case capture
  case is < 10: numero$ = "00"
  case is < 100: numero$ = "0"
  case is < 1000: numero$ = ""
end select
fichier$ = "capture" + numero$ + str(capture) + ".bmp"
color rgb(255,255,255), rgb(255,0,0)                      'signaler la capture d'images.
locate 34, 43: print fichier$
bsave fichier$,0
color noir, fond
capture = capture + 1
if capture > 500 then end 
return


'#############################################################################################
Initialisation:'------------------------ INITIALISATION --------------------------------------
'#############################################################################################
fond = rgb(225,225,225)
blanc= rgb(255,255,255)
gris = rgb(75,75,75)
bleu = rgb(0,0,255)
rouge = rgb(255,0,0)
vert = rgb(0,150,0)
turquoise = rgb (230, 255, 255)
pi = 4 * atn(1)
'bitmap = 1                                               'séquence bitmap si désiré.
lambda = 20
luminosite = 20                       
contraste = 2
relief = lambda / 5
demiOnde = 1
xCentre = 422
yCentre = hauteur / 2
screenset 2, 2                                            'créer une page matrice.
color noir, fond: cls
line(0,0)-(largeur, hauteur),noir, bf
ligne35$ = "    I - Initialiser.     ": locate 35, 39: print ligne35$
ligne36$ = "        Quitter (Echap). ": locate 36, 39: print ligne36$
ligne37$ = "                         "
locate 30
locate, 2: print "Indice du quartz........ I = 1,515."
locate, 2: print "Vitesse de l'onde: 1/1.515 = 0,66 c."
locate, 2: print "Focale de la lentille....F = 558 pixels."
locate, 2: print "DiamŠtre de la lentille..D = 368 pixels."
locate, 2: print "Ouverture relative...F / D = Ÿ/ 1,516."
locate, 2: print "                    Lambda = 20 pixels."
locate, 2: print "Largeur de la tache d'Airy:"
locate, 2: print "        2 * lambda * F / D = 60 pixels.";
locate 30
locate, 47: print "Les ondes voyagent … 1 / 1,515 = 66% de leur vitesse"
locate, 47: print "normale … l'int‚rieur de la lentille. Les ondes sont"
locate, 47: print "focalis‚es … droite et produisent l'‚quivalent de la"
locate, 47: print "tache d'Airy, mais en deux dimensions seulement. La"
locate, 47: print "focale vaut environ le double du rayon: 288 * 2 = 576."
locate 35: color vert
locate, 69:print "Le 16 nov. 2006. Ce programme"
locate, 69:print "FreeBASIC peut ˆtre copi‚,"
locate, 69:print "distribu‚ ou modifi‚ librement.";

for x = -1 to largeur + 1                                 'effacer.
  for y = -1 to hauteur + 1
    P1(x, y) = .000001                                    'accélère le calcul (!!?).
    P2(x, y) = .000001
    P3(x, y) = .000001
  next
next

pcopy 2, page1
pcopy 2, page2
screenset page1, page2
return
