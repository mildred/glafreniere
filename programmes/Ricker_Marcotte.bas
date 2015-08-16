cote = 200   'nombre de particules sur un coté du champ.
Dim precedent(0 To cote), precedent2(0 To cote)
Dim As Single potentiel1(-1 To cote+1, -1 To cote+1, -1 To cote+1)
Dim As Single potentiel2(-1 To cote+1, -1 To cote+1, -1 To cote+1)
Dim As Single potentiel3(-1 To cote+1, -1 To cote+1, -1 To cote+1)
Dim As Single potentiel(0 To 10 * cote)'                  'principe de Huygens, sous-multiples..
Dim As Single potentielAdd(-1 To cote+1, -1 To cote+1, -1 To cote+1)
Dim As Single potentiel2Add(-1 To cote+1, -1 To cote+1, -1 To cote+1)
Dim As Single pi, ondelette, angle, lambda, lambda2, xCarre, yCarre, arcSinus, xDistance
Dim As Single xCoord, distNormale, periode, distance, rotation, amplitude, phi, xPoint
Dim As Single amplitudeSinus, amplitudeCosinus, differenceDeMarche, yDistance, vitesse
Dim As Single betaSinusAngle, deuxPiDistanceSurLambda, temps, luminance, inverser
Dim As Single GainA, GainB, GainC, GainD, SommeA, SommeB, SommeC, SommeD, maximum, minimum, pot


'                      MODÉLISATION DE L'ÉTHER EN TROIS DIMENSIONS.
'______________________________________________________________________________________________
'                             CALCUL SELON M. JOCELYN MARCOTTE                               
'
'
'  Sélectionner l'équation à utiliser en ajustant les valeurs de GainC et GainD :
'
'    - Le programme est stable seulement lorsque la somme GainC + GainD est près de 0.25
'
'
'  Valeurs intéressantes à essayer :
'
'
'       GainD = sqr(3) / (1 + sqr(32) + sqr(48) )
'       GainC = sqr(2) / (1 + sqr(32) + sqr(48) )
'             ->  GainD =  0,0941 (granules sur les sommets)
'                 GainC =  0,1152 (granules sur les diagonales)
'                 GainB =  0,1629 (granules sur les axes)
'                 GainA = -1,1125 (granule courrant)
'             ->  Le ratio GainB / GainC = 0.1629 / 0.1152 = 1.4142 = sqr(2) !
'                 Le ratio GainB / GainD = 0.1629 / 0.0941 = 1.7321 = sqr(3) !
'                 Le gain est donc inversement proportionnel à la distance qui
'                 sépare les granules du granule central
'
'
'       GainD = 1 / 9
'       GainC = 1 / 9
'             ->  GainD =  1 / 9 (granules sur les sommets)
'                 GainC =  1 / 9 (granules sur les diagonales)
'                 GainB =  1 / 9 (granules sur les axes)
'                 GainA = -8 / 9 (granule courrant)
'             ->  Les GainB, GainC et GainD sont égaux !
'                 Les 26 granules voisins ont un gain identique
'
'
'       GainD = 1 / 8
'       GainC = 1 / 8
'             ->  GainD =  1 / 8 (granules sur les sommets)
'                 GainC =  1 / 8 (granules sur les diagonales)
'                 GainB =  0     (granules sur les axes)
'                 GainA = -1 / 2 (granule courrant)
'             ->  Les gains sont tous des puissances de 2 !
'                 Il y a donc moins d'erreur d'arrondis dans les calculs
'
'


' Valeurs à ajuster :
GainD = 1 / (4 + Sqr(3) + Sqr(24) )
GainC = 1 / (4 + Sqr(2) + Sqr(32/3) )


' Ne pas toucher à ces formules :
GainB = -4*GainC -  4*GainD + 1
GainA = 12*GainC + 16*GainD - 4


' Une autre possibilité :
'GainA = 0
'GainB = 1 / 3
'GainC = 0
'GainD = 0



Screen 19,24,3: Gosub Initialisation: Gosub Perturbation


'                      MODÉLISATION DE L'ÉTHER EN TROIS DIMENSIONS.
'______________________________________________________________________________________________
'                             CALCUL SELON M. JOCELYN MARCOTTE                               

Do


'Le code est répété 3 fois pour éviter cette triple boucle de copie

' Mémoriser les deux derniers états du potentiel.
' FOR x = -1 TO cote + 1 : FOR y = -1 to cote  + 1 : FOR z = -1 TO cote + 1
'       potentiel3(x,y,z) = potentiel2(x,y,z)
'       potentiel2(x,y,z) = potentiel1(x,y,z)
' NEXT : NEXT : NEXT




'______________________________________________________________________________________________
' PREMIÈRE ÉTAPE : CALCUL DE POTENTIEL 3

  For x = 0 To cote : For y = 0 To cote : For z = 0 To cote
    SommeA = potentiel1(x,y,z)
    SommeB = potentiel1(x+1,y,z) + potentiel1(x-1,y,z) + potentiel1(x,y+1,z) + potentiel1(x,y-1,z) + potentiel1(x,y,z+1) + potentiel1(x,y,z-1)
    SommeC = potentiel1(x+1,y+1,z) + potentiel1(x-1,y+1,z) + potentiel1(x+1,y-1,z) + potentiel1(x-1,y-1,z) + potentiel1(x+1,y,z+1) + potentiel1(x-1,y,z+1) + potentiel1(x+1,y,z-1) + potentiel1(x-1,y,z-1) + potentiel1(x,y+1,z+1) + potentiel1(x,y-1,z+1) + potentiel1(x,y+1,z-1) + potentiel1(x,y-1,z-1)
    SommeD = potentiel1(x+1,y+1,z+1) + potentiel1(x-1,y+1,z+1) + potentiel1(x+1,y-1,z+1) + potentiel1(x-1,y-1,z+1) + potentiel1(x+1,y+1,z-1) + potentiel1(x-1,y+1,z-1) + potentiel1(x+1,y-1,z-1) + potentiel1(x-1,y-1,z-1)
    potentiel3(x,y,z) = GainA*SommeA + GainB*SommeB + GainC*SommeC + GainD*SommeD - potentiel2(x,y,z)
  Next : Next : Next
  
  If reflexion = 2 Then                                   'réflexion "molle".
    For x = 0 To cote : For y = 0 To cote
      potentiel3(x, y, -1)     = potentiel3(x, y, 0)
      potentiel3(x, y, cote+1) = potentiel3(x, y, cote)
    Next : Next
    For y = 0 To cote : For z = 0 To cote
      potentiel3(-1, y, z)     = potentiel3(0, y, z)
      potentiel3(cote+1, y, z) = potentiel3(cote, y, z)
    Next : Next
    For x = 0 To cote : For z = 0 To cote
      potentiel3(x, -1, z)     = potentiel3(x, 0, z)
      potentiel3(x, cote+1, z) = potentiel3(x, cote, z)
    Next : Next

  Elseif reflexion = 0 Then                               'pas de réflexion.
    For x = 0 To cote : For y = 0 To cote
      SommeB = potentiel1(x,y,0)
      SommeC = potentiel1(x+1,y,  0) + potentiel1(x-1,y,  0) + potentiel1(x,y+1,  0) + potentiel1(x,y-1,  0)
      SommeD = potentiel1(x+1,y+1,0) + potentiel1(x-1,y+1,0) + potentiel1(x+1,y-1,0) + potentiel1(x-1,y-1,0)
      potentiel3(x, y, -1) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel1(x,y,cote)
      SommeC = potentiel1(x+1,y,  cote) + potentiel1(x-1,y,  cote) + potentiel1(x,y+1,  cote) + potentiel1(x,y-1,  cote)
      SommeD = potentiel1(x+1,y+1,cote) + potentiel1(x-1,y+1,cote) + potentiel1(x+1,y-1,cote) + potentiel1(x-1,y-1,cote)
      potentiel3(x, y, cote+1) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
    For y = 0 To cote :For z = 0 To cote
      SommeB = potentiel1(0,y,z)
      SommeC = potentiel1(0, y+1,  z) + potentiel1(0, y-1,  z) + potentiel1(0, y,  z+1) + potentiel1(0, y,  z-1)
      SommeD = potentiel1(0, y+1,z+1) + potentiel1(0, y-1,z+1) + potentiel1(0, y+1,z-1) + potentiel1(0, y-1,z-1)
      potentiel3(-1, y, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel1(cote,y,z)
      SommeC = potentiel1(cote, y+1,  z) + potentiel1(cote, y-1,  z) + potentiel1(cote, y, z+1)  + potentiel1(cote, y,  z-1)
      SommeD = potentiel1(cote, y+1,z+1) + potentiel1(cote, y-1,z+1) + potentiel1(cote, y+1,z-1) + potentiel1(cote, y-1,z-1)
      potentiel3(cote+1, y, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
    For x = 0 To cote :For z = 0 To cote
      SommeB = potentiel1(x,0,z)
      SommeC = potentiel1(x+1, 0,  z) + potentiel1(x-1, 0,  z) + potentiel1(x,  0,z+1) + potentiel1(x,  0, z-1)
      SommeD = potentiel1(x+1, 0,z+1) + potentiel1(x-1, 0,z+1) + potentiel1(x+1,0,z-1) + potentiel1(x-1, 0,z-1)
      potentiel3(x, -1, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel1(x,cote,z)
      SommeC = potentiel1(x+1, cote,  z) + potentiel1(x-1, cote,  z) + potentiel1(x,  cote,z+1) + potentiel1(x,  cote, z-1)
      SommeD = potentiel1(x+1, cote,z+1) + potentiel1(x-1, cote,z+1) + potentiel1(x+1,cote,z-1) + potentiel1(x-1, cote,z-1)
      potentiel3(x, cote+1, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
  End If                                                  'réflexion dure par défaut.


'______________________________________________________________________________________________
' DEUXIÈME ÉTAPE : CALCUL DE POTENTIEL 2

  For x = 0 To cote : For y = 0 To cote : For z = 0 To cote
    SommeA = potentiel3(x,y,z)
    SommeB = potentiel3(x+1,y,z) + potentiel3(x-1,y,z) + potentiel3(x,y+1,z) + potentiel3(x,y-1,z) + potentiel3(x,y,z+1) + potentiel3(x,y,z-1)
    SommeC = potentiel3(x+1,y+1,z) + potentiel3(x-1,y+1,z) + potentiel3(x+1,y-1,z) + potentiel3(x-1,y-1,z) + potentiel3(x+1,y,z+1) + potentiel3(x-1,y,z+1) + potentiel3(x+1,y,z-1) + potentiel3(x-1,y,z-1) + potentiel3(x,y+1,z+1) + potentiel3(x,y-1,z+1) + potentiel3(x,y+1,z-1) + potentiel3(x,y-1,z-1)
    SommeD = potentiel3(x+1,y+1,z+1) + potentiel3(x-1,y+1,z+1) + potentiel3(x+1,y-1,z+1) + potentiel3(x-1,y-1,z+1) + potentiel3(x+1,y+1,z-1) + potentiel3(x-1,y+1,z-1) + potentiel3(x+1,y-1,z-1) + potentiel3(x-1,y-1,z-1)
    potentiel2(x,y,z) = GainA*SommeA + GainB*SommeB + GainC*SommeC + GainD*SommeD - potentiel1(x,y,z)
  Next : Next : Next
  
  If reflexion = 2 Then                                   'réflexion "molle".
    For x = 0 To cote : For y = 0 To cote
      potentiel2(x, y, -1)     = potentiel2(x, y, 0)
      potentiel2(x, y, cote+1) = potentiel2(x, y, cote)
    Next : Next
    For y = 0 To cote : For z = 0 To cote
      potentiel2(-1, y, z)     = potentiel2(0, y, z)
      potentiel2(cote+1, y, z) = potentiel2(cote, y, z)
    Next : Next
    For x = 0 To cote : For z = 0 To cote
      potentiel2(x, -1, z)     = potentiel2(x, 0, z)
      potentiel2(x, cote+1, z) = potentiel2(x, cote, z)
    Next : Next

  Elseif reflexion = 0 Then                               'pas de réflexion.
    For x = 0 To cote : For y = 0 To cote
      SommeB = potentiel3(x,y,0)
      SommeC = potentiel3(x+1,y,  0) + potentiel3(x-1,y,  0) + potentiel3(x,y+1,  0) + potentiel3(x,y-1,  0)
      SommeD = potentiel3(x+1,y+1,0) + potentiel3(x-1,y+1,0) + potentiel3(x+1,y-1,0) + potentiel3(x-1,y-1,0)
      potentiel2(x, y, -1) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel3(x,y,cote)
      SommeC = potentiel3(x+1,y,  cote) + potentiel3(x-1,y,  cote) + potentiel3(x,y+1,  cote) + potentiel3(x,y-1,  cote)
      SommeD = potentiel3(x+1,y+1,cote) + potentiel3(x-1,y+1,cote) + potentiel3(x+1,y-1,cote) + potentiel3(x-1,y-1,cote)
      potentiel2(x, y, cote+1) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
    For y = 0 To cote :For z = 0 To cote
      SommeB = potentiel3(0,y,z)
      SommeC = potentiel3(0, y+1,  z) + potentiel3(0, y-1,  z) + potentiel3(0, y,  z+1) + potentiel3(0, y,  z-1)
      SommeD = potentiel3(0, y+1,z+1) + potentiel3(0, y-1,z+1) + potentiel3(0, y+1,z-1) + potentiel3(0, y-1,z-1)
      potentiel2(-1, y, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel3(cote,y,z)
      SommeC = potentiel3(cote, y+1,  z) + potentiel3(cote, y-1,  z) + potentiel3(cote, y, z+1)  + potentiel3(cote, y,  z-1)
      SommeD = potentiel3(cote, y+1,z+1) + potentiel3(cote, y-1,z+1) + potentiel3(cote, y+1,z-1) + potentiel3(cote, y-1,z-1)
      potentiel2(cote+1, y, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
    For x = 0 To cote :For z = 0 To cote
      SommeB = potentiel3(x,0,z)
      SommeC = potentiel3(x+1, 0,  z) + potentiel3(x-1, 0,  z) + potentiel3(x,  0,z+1) + potentiel3(x,  0, z-1)
      SommeD = potentiel3(x+1, 0,z+1) + potentiel3(x-1, 0,z+1) + potentiel3(x+1,0,z-1) + potentiel3(x-1, 0,z-1)
      potentiel2(x, -1, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel3(x,cote,z)
      SommeC = potentiel3(x+1, cote,  z) + potentiel3(x-1, cote,  z) + potentiel3(x,  cote,z+1) + potentiel3(x,  cote, z-1)
      SommeD = potentiel3(x+1, cote,z+1) + potentiel3(x-1, cote,z+1) + potentiel3(x+1,cote,z-1) + potentiel3(x-1, cote,z-1)
      potentiel2(x, cote+1, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
  End If                                                  'réflexion dure par défaut.


'______________________________________________________________________________________________
' TROISIÈME ÉTAPE : CALCUL DE POTENTIEL 1

  For x = 0 To cote : For y = 0 To cote : For z = 0 To cote
    SommeA = potentiel2(x,y,z)
    SommeB = potentiel2(x+1,y,z) + potentiel2(x-1,y,z) + potentiel2(x,y+1,z) + potentiel2(x,y-1,z) + potentiel2(x,y,z+1) + potentiel2(x,y,z-1)
    SommeC = potentiel2(x+1,y+1,z) + potentiel2(x-1,y+1,z) + potentiel2(x+1,y-1,z) + potentiel2(x-1,y-1,z) + potentiel2(x+1,y,z+1) + potentiel2(x-1,y,z+1) + potentiel2(x+1,y,z-1) + potentiel2(x-1,y,z-1) + potentiel2(x,y+1,z+1) + potentiel2(x,y-1,z+1) + potentiel2(x,y+1,z-1) + potentiel2(x,y-1,z-1)
    SommeD = potentiel2(x+1,y+1,z+1) + potentiel2(x-1,y+1,z+1) + potentiel2(x+1,y-1,z+1) + potentiel2(x-1,y-1,z+1) + potentiel2(x+1,y+1,z-1) + potentiel2(x-1,y+1,z-1) + potentiel2(x+1,y-1,z-1) + potentiel2(x-1,y-1,z-1)
    potentiel1(x,y,z) = GainA*SommeA + GainB*SommeB + GainC*SommeC + GainD*SommeD - potentiel3(x,y,z)
  Next : Next : Next
  
  If reflexion = 2 Then                                   'réflexion "molle".
    For x = 0 To cote : For y = 0 To cote
      potentiel1(x, y, -1)     = potentiel1(x, y, 0)
      potentiel1(x, y, cote+1) = potentiel1(x, y, cote)
    Next : Next
    For y = 0 To cote : For z = 0 To cote
      potentiel1(-1, y, z)     = potentiel1(0, y, z)
      potentiel1(cote+1, y, z) = potentiel1(cote, y, z)
    Next : Next
    For x = 0 To cote : For z = 0 To cote
      potentiel1(x, -1, z)     = potentiel1(x, 0, z)
      potentiel1(x, cote+1, z) = potentiel1(x, cote, z)
    Next : Next

  Elseif reflexion = 0 Then                               'pas de réflexion.
    For x = 0 To cote : For y = 0 To cote
      SommeB = potentiel2(x,y,0)
      SommeC = potentiel2(x+1,y,  0) + potentiel2(x-1,y,  0) + potentiel2(x,y+1,  0) + potentiel2(x,y-1,  0)
      SommeD = potentiel2(x+1,y+1,0) + potentiel2(x-1,y+1,0) + potentiel2(x+1,y-1,0) + potentiel2(x-1,y-1,0)
      potentiel1(x, y, -1) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel2(x,y,cote)
      SommeC = potentiel2(x+1,y,  cote) + potentiel2(x-1,y,  cote) + potentiel2(x,y+1,  cote) + potentiel2(x,y-1,  cote)
      SommeD = potentiel2(x+1,y+1,cote) + potentiel2(x-1,y+1,cote) + potentiel2(x+1,y-1,cote) + potentiel2(x-1,y-1,cote)
      potentiel1(x, y, cote+1) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
    For y = 0 To cote :For z = 0 To cote
      SommeB = potentiel2(0,y,z)
      SommeC = potentiel2(0, y+1,  z) + potentiel2(0, y-1,  z) + potentiel2(0, y,  z+1) + potentiel2(0, y,  z-1)
      SommeD = potentiel2(0, y+1,z+1) + potentiel2(0, y-1,z+1) + potentiel2(0, y+1,z-1) + potentiel2(0, y-1,z-1)
      potentiel1(-1, y, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel2(cote,y,z)
      SommeC = potentiel2(cote, y+1,  z) + potentiel2(cote, y-1,  z) + potentiel2(cote, y, z+1)  + potentiel2(cote, y,  z-1)
      SommeD = potentiel2(cote, y+1,z+1) + potentiel2(cote, y-1,z+1) + potentiel2(cote, y+1,z-1) + potentiel2(cote, y-1,z-1)
      potentiel1(cote+1, y, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
    For x = 0 To cote :For z = 0 To cote
      SommeB = potentiel2(x,0,z)
      SommeC = potentiel2(x+1, 0,  z) + potentiel2(x-1, 0,  z) + potentiel2(x,  0,z+1) + potentiel2(x,  0, z-1)
      SommeD = potentiel2(x+1, 0,z+1) + potentiel2(x-1, 0,z+1) + potentiel2(x+1,0,z-1) + potentiel2(x-1, 0,z-1)
      potentiel1(x, -1, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
      SommeB = potentiel2(x,cote,z)
      SommeC = potentiel2(x+1, cote,  z) + potentiel2(x-1, cote,  z) + potentiel2(x,  cote,z+1) + potentiel2(x,  cote, z-1)
      SommeD = potentiel2(x+1, cote,z+1) + potentiel2(x-1, cote,z+1) + potentiel2(x+1,cote,z-1) + potentiel2(x-1, cote,z-1)
      potentiel1(x, cote+1, z) = SommeB*GainB + SommeC*GainC + SommeD*GainD
    Next : Next
  End If                                                  'réflexion dure par défaut.


'______________________________________________________________________________________________
' AFFICHAGE DE POTENTIEL 1 

  xPoint = xPoint + vitesse: If xPoint > cote Then xPoint = xPoint - cote
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  
  For x = 0 To cote                                       'diagramme principal (agrandi).
    For y = cote/4 To 3*cote/4
      luminance = 100 * potentiel1(x, y, cote / 2) + 128
      If luminance > 255 Then luminance = 255
      If luminance < 0 Then luminance = 0
      Pset (2*x,2*y-cote/2),    Rgb(luminance,luminance,luminance)
      Pset (2*x+1,2*y-cote/2),  Rgb(luminance,luminance,luminance)
      Pset (2*x+1,2*y-cote/2+1),Rgb(luminance,luminance,luminance)
      Pset (2*x,2*y-cote/2+1),  Rgb(luminance,luminance,luminance)
    Next
  Next
  
  maximum = 0 : minimum = 0
  For x = 0 To cote                                       'graphique 1D (agrandi).
    pot = potentiel1(x, cote / 2, cote / 2)
    If pot > maximum Then maximum = pot
    If pot < minimum Then minimum = pot
    courbe% = graphique - 16 * pot
    Line(2*x-2, precedent(x-1))-(2*x, courbe%), 0
    precedent(x) = courbe%
  Next
  
  If clic = 0 Then Getmouse xSouris, ySouris, , clic      'vérifier une dernière fois.
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  If Len(saisie$) Then
    Select Case saisie$
      Case "P": Sleep: vider$ = Inkey
      Case "X+","k+",Chr$(27): End
    End Select
  End If
  
  ligne = .5 + ySouris / 16
  'locate 30, 12: print xSouris; ySouris; clic; ligne; "    ";
  If ligne < 31 Then
    If ligne < 24 Or xSouris > 736 Or xSouris < 458 Then ligne = 0
  Elseif ligne > 34 Then
    If ligne > 37 Or xSouris < 304 Or xSouris > 496 Then ligne = 0
  End If
  If clic = 1 Then
    If xSouris < cote And ySouris < cote Then ligne = 1   'clic sur l'écran.
  End If
  Getmouse xSouris, ySouris, , clic
Loop

Perturbation:'---------- PERTURBATION CENTRALE SELON LA DISTRIBUTION NORMALE -----------------
For x = -1 To cote + 1
  xDistance = x - cote / 2
  xCarre = xDistance * xDistance
  For y = -1 To cote + 1
    yDistance = y - cote / 2
    yCarre = yDistance * yDistance
    For z = -1 To cote + 1
      zDistance = z - cote / 2
      zCarre = zDistance * zDistance
      distance = Sqr(xCarre + yCarre + zCarre)
      xCoord = distance / (((3/8)*lambda) / 2)
      distNormale = pi ^ (-xCoord ^ 2)                    'distribution normale (approx?).
      potentiel1(x, y, z) = 20 * distNormale +.00001
      potentielAdd(x, y, z) = potentiel1(x, y, z)
    Next
  Next
Next

For x = 0 To cote : For y = 0 To cote : For z = 0 To cote
  SommeA = potentiel1(x,y,z)
  SommeB = potentiel1(x+1,y,z) + potentiel1(x-1,y,z) + potentiel1(x,y+1,z) + potentiel1(x,y-1,z) + potentiel1(x,y,z+1) + potentiel1(x,y,z-1)
  SommeC = potentiel1(x+1,y+1,z) + potentiel1(x-1,y+1,z) + potentiel1(x+1,y-1,z) + potentiel1(x-1,y-1,z) + potentiel1(x+1,y,z+1) + potentiel1(x-1,y,z+1) + potentiel1(x+1,y,z-1) + potentiel1(x-1,y,z-1) + potentiel1(x,y+1,z+1) + potentiel1(x,y-1,z+1) + potentiel1(x,y+1,z-1) + potentiel1(x,y-1,z-1)
  SommeD = potentiel1(x+1,y+1,z+1) + potentiel1(x-1,y+1,z+1) + potentiel1(x+1,y-1,z+1) + potentiel1(x-1,y-1,z+1) + potentiel1(x+1,y+1,z-1) + potentiel1(x-1,y+1,z-1) + potentiel1(x+1,y-1,z-1) + potentiel1(x-1,y-1,z-1)
  potentiel2(x,y,z) = (SommeA*GainA + SommeB*GainB + SommeC*GainC + SommeD*GainD)/(GainA + 6*GainB + 12*GainC + 8*GainD)
  potentiel2Add(x, y, z) = potentiel2(x, y, z)
Next : Next : Next

Swap page1, page2                                         'premier diagramme.
Screenset page1, page2
Pcopy 2, page1

For x = 0 To cote
  For y = cote/4 To 3*cote/4
    luminance = 100 * potentiel1(x, y, cote / 2) + 128
    If luminance > 255 Then luminance = 255
    If luminance < 0 Then luminance = 0
    Pset (2*x,2*y-cote/2),    Rgb(luminance,luminance,luminance)
    Pset (2*x+1,2*y-cote/2),  Rgb(luminance,luminance,luminance)
    Pset (2*x+1,2*y-cote/2+1),Rgb(luminance,luminance,luminance)
    Pset (2*x,2*y-cote/2+1),  Rgb(luminance,luminance,luminance)
  Next
Next
maximum = 0 : minimum = 0
For x = 0 To cote                                         'graphique 1D (agrandi).
  pot = potentiel1(x, cote / 2, cote / 2)
  If pot > maximum Then maximum = pot
  If pot < minimum Then minimum = pot
  courbe% = graphique - 16 * pot
  Line(2*x-2, precedent(x-1))-(2*x, courbe%), 0
  precedent(x) = courbe%
Next
Do: Loop While Len(Inkey)

Return

Initialisation:'------------------------ INITIALISATION --------------------------------------
fond  = Rgb(225,225,225)
rouge = Rgb(255,0,0)
bleu  = Rgb(0,0,255)
vert  = Rgb(0,150,0)
dore  = Rgb(175,150,0)
violet= Rgb(255,0,255)
orange= Rgb(255,150,0)
turquoise = Rgb(0,150,150)
pi = 4 * Atn(1)
page1 = 1
lambda = 40*(4/3)                                         'C.F. Volume (sphère) = (4/3)*pi*R^3
gabarit = 162                                             'amplitude selon l'espace disponible.
reflexion = 0                                             'réflexion molle.
ondelette = pi / 100                                      'cent ondelettes est un minimum.
xCentre = 400
yCentre = cote / 2
graphique = 450
xgg = xCentre - 50                                        'coordonnées des flèches.
xgd = xCentre - 20
xdg = xCentre + 20
xdd = xCentre + 50
yFleche = 584

Screenset 2,2: Color noir, fond: Cls
Windowtitle "L'ondelette de Ricker"
titre$ = "L'ETHER VIRTUEL": Print titre$
xOrig = 800 - (800 - cote) / 2 - Len(titre$) * 8
xOrig = 480
yOrig = 2: accent = xOrig + 37
For x = 0 To 8 * Len(titre$)                              'agrandir l'en-tête.
  For y = 0 To 16
    If Point(x,y) = 0 Then
      Pset(2 * x + xOrig, 2 * y + yOrig), Rgb(0,0,255)
      Pset(2 * x + xOrig + 1, 2 * y + yOrig), Rgb(0,0,255)
      Pset(2 * x + xOrig, 2 * y + yOrig + 1), Rgb(0,0,255)
      Pset(2 * x + xOrig + 1, 2 * y + yOrig + 1), Rgb(0,0,255)
    End If    
  Next
Next
Line(accent,4)-(accent + 4,4), Rgb(0,0,255):Line(accent + 1,3)-(accent + 5,3), Rgb(0,0,255):Line(accent + 2,2)-(accent + 6,2), Rgb(0,0,255)'accent du titre.
Color fond, fond: Locate 1, 1: Print titre$               'effacer.
Color 0, fond: Locate 5
Locate,5: Print "Veuillez patienter."
Locate,5: Print "Calcul en cours..."
Locate 4
Locate,56:Print "Ce programme reproduit des ondes sph‚riques "
Locate,56:Print "dans un m‚dium … trois dimensions. Le volume"
Locate,56:Print "d'une sphŠre vaut: V = (4/3) * pi * R^3. Les"
Locate,56:Print "unit‚s ® x ¯ de la distribution normale se-"
Locate,56:Print "lon y = pi^(-x^2) d‚terminent la demi-onde"
Locate,56:Print "d'une ondelette de Ricker cr‚‚e par une im-"
Locate,56:Print "pulsion gaussienne mesur‚e dans ces unit‚s:"
Locate,56:Print "lambda = 2*(4/3), d'o—: (3/4)*lambda/2 = 1."
Locate,56:Print "L'amplitude initiale doit ˆtre doubl‚e:":?
Locate,65:Print "y = 2 * pi^(-x^2)"
Line(465,216)-(500,216), rouge
Line(465,217)-(500,217), rouge
Locate,65:Print "y = sqr(3/4) * pi^(-x^2)":?
Line(465,232)-(500,232), violet
Line(465,233)-(500,233), violet
Locate,56:Print "L'ondelette de Ricker uniforme:":?
Locate,65:Print "y = x * pi^(-x^2)":?
yPrec = 280
For pixel = -lambda To lambda
  xPoint = pixel / ((3/8)*lambda)
  y = 30 * xPoint * 2*pi^(-xPoint^2)                      'ondelette sans affaiblissement.
  Pset(pixel+720,280), Rgb(100,100,100)
  Line(pixel+720, yPrec)-(pixel+720, 280-y), noir
  yPrec = 280-y
Next
Locate,56:Print "L'ondelette de Ricker avec affaiblissement:":?
Locate,65:Print "y = pi^(-x^2) / (distance/x+1)":?
Locate,65:Print "distance = 1,2  en unit‚s x"
Line(465,392)-(500,392), orange
Line(465,393)-(500,393), orange
Locate,65:Print "distance =   3"
Line(465,408)-(500,408), bleu
Line(465,409)-(500,409), bleu
Locate,65:Print "distance = 5,1":?
Line(465,424)-(500,424), vert
Line(465,425)-(500,425), vert
Locate,56:Print "L'affaiblissement:":?
Locate,65:Print "y = (2 / pi) ^ 2 / x":?:?
Line(465,488)-(500,488), dore
Line(465,489)-(500,489), dore
Locate 33,32:Print "Demi-onde."
Color vert
Locate 33
Locate,56:Print "Ondes selon M. Jocelyn Marcotte."
Locate,56:Print "Gabriel LaFreniŠre  glafreniere.com":?
Locate,56:Print "Le 14 f‚v. 2007. Ce programme FreeBASIC peut"
Locate,56:Print "ˆtre distribu‚, copi‚ ou modifi‚ librement. ";
Color noir
For x = -cote To cote                                     'gabarit d'affaiblissement (doré).
  xPoint = x / ((3/8)*lambda)
  y = (2 / pi) ^ 2 * gabarit / xPoint                     'constante selon (2 / pi)^2.
  If Abs(x) > 10 Then Pset(x+cote, graphique+y), dore
  If Abs(x) > 10 Then Pset(x+cote, graphique-y), dore
Next
yPrec = graphique
For pixel = -100 To 100                                   'distribution normale(haut, rouge).
  xPoint = pixel / ((3/8)*lambda)
  y = gabarit * 2*pi^(-xPoint^2)                          'haut: gabarit doublé.
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), rouge
  yPrec = graphique-y
Next
yPrec = graphique
For pixel = -100 To 100                                   'distribution normale(bas, violet).
  xPoint = pixel / ((3/8)*lambda)
  y = gabarit * Sqr(3/4) * pi^(-xPoint^2)                 'bas: gabarit * 0,866.
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique+y), violet
  yPrec = graphique+y
Next

'---------------------------------- ONDELETTES DE RICKER --------------------------------------
yPrec = graphique + 80
distance = 1.2
For pixel = 15 To 100                                     'ondelette rapprochée droite (orange).
  xPoint = pixel / ((3/8)*lambda) - distance
  y = gabarit * pi^(-xPoint^2) / (distance / xPoint + 1)
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), orange
  yPrec = graphique-y
Next
yPrec = graphique
distance = -1.2
For pixel = -100 To -15                                   'ondelette rapprochée gauche (orange).
  xPoint = pixel / ((3/8)*lambda) - distance
  y = gabarit * pi^(-xPoint^2) / (distance / xPoint + 1)
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), orange
  yPrec = graphique-y
Next
yPrec = graphique
distance = -3
For pixel = -120 To -1                                    'ondelette moyenne gauche (bleu).
  xPoint = pixel / ((3/8)*lambda) - distance
  y = gabarit * pi^(-xPoint^2) / (distance / xPoint + 1)
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), bleu
  yPrec = graphique-y
Next
yPrec = graphique
distance = 3
For pixel = 1 To 120                                      'ondelette moyenne droite (bleu).
  xPoint = pixel / ((3/8)*lambda) - distance
  y = gabarit * pi^(-xPoint^2) / (distance / xPoint + 1)
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), bleu
  yPrec = graphique-y
Next
yPrec = graphique
distance = -5.1
For pixel = -cote To -50                                  'ondelette éloignée gauche (vert).
  xPoint = pixel / ((3/8)*lambda) - distance
  y = gabarit * pi^(-xPoint^2) / (distance / xPoint + 1)
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), vert
  yPrec = graphique-y
Next
yPrec = graphique
distance = 5.1
For pixel = 50 To cote                                    'ondelette éloignée droite (vert).
  xPoint = pixel / ((3/8)*lambda) - distance
  y = gabarit * pi^(-xPoint^2) / (distance / xPoint + 1)
  Line(pixel+cote-1, yPrec)-(pixel+cote, graphique-y), vert
  yPrec = graphique-y
Next
Line(0, graphique)-(2 * cote, graphique), Rgb(100,100,100)'axe
Line(245,graphique+28)-(245,graphique+60), Rgb(100,100,100)
Line(245+lambda/2,graphique-17)-(245+lambda/2,graphique+60), Rgb(100,100,100)
'Sleep:End
Pcopy 2, page1
Return

