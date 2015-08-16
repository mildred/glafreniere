Dim As Single pi, distance, phi, lambdaPrime, amplitude, beta, differenceDeMarche, xPrime
Dim As Single rotation, sag, brillance, luminance, flottante, rayon, phase, facteurG, xCarre
pi = 4 * Atn(1): demiLargeur = 200: demiHauteur = 150: images = 48: lambda = 20
beta = .707: facteurG = Sqr(1 - beta ^ 2): rayon = Sin(pi / 4)
Screen 19,24,3: page1 = 1: Screenset 2: Color noir, Rgb(225,225,225): Cls
Locate 2, 64: Print "L'EFFET DOPPLER RELATIF"
Locate 4, 53: Print "Cet effet Doppler est dit ® relatif ¯ parce que"
Locate 5, 53: Print "l'‚metteur occupe une position fixe par rapport"
Locate 6, 53: Print "… celle de l'observateur. C'est donc une appli-"
Locate 7, 53: Print "cation du principe de Relativit‚ de Galil‚e, ce"
Locate 8, 53: Print "qui signifie que l'observateur aussi se d‚place"
Locate 9, 53: Print "… travers le m‚dium qui v‚hicule les ondes. Cet"
Locate 10,53: Print "observateur voit alors les ondes se d‚placer … "
Locate 11,53: Print "des vitesse diff‚rentes selon leur direction et"
Locate 12,53: Print "il a l'impression que les ondes qui circulent  "
Locate 13,53: Print "transversalement proviennent du mˆme endroit.  "
Locate 15,53: Print "Les repŠres blancs montrent que les ondes sont "
Locate 16,53: Print "comprim‚es si elles circulent le long des axes "
Locate 17,53: Print "orthogonaux y et z. Elles ne le sont pas si la "
Locate 18,53: Print "fr‚quence du systŠme ralentit selon les trans- "
Locate 19,53: Print "formations de Lorentz, d'o—: y' = y; z' = z."
Locate 21, 4: Print "Ce programme utilise la formule suivante :"
Locate 23, 4: Print "lambdaPrime = lambda * (cos(asin(beta * sin(phi))) - beta * cos(phi))"
Locate 25, 4: Print "Cette formule est une application de la loi des cosinus, aussi connue sous le nom de"
Locate 26, 4: Print "th‚orŠme d'Al Kashi. Elle peut sembler complexe … premiŠre vue. Toutefois, on conviendra"
Locate 27, 4: Print "qu'elle permet d'‚liminer d'un coup tous les pr‚paratifs qui autrement seraient requis"
Locate 28, 4: Print "avant d'utiliser la formule de l'effet Doppler normal, qui est plus simple:"
Locate 30,20: Print "lambdaPrime = lambda * (1 - beta * cos(phi))"
Locate 32, 4: Print "Cette formule n'est pas trŠs pratique car pour d‚terminer la valeur de l'angle phi, il faut"
Locate 33, 4: Print "d'abord rep‚rer l'endroit o— se situait la source ‚mettrice au moment o— elle a ‚mis l'onde."

Locate 36,38: Print "Gabriel LaFreniŠre, le 6 juin 2006.  glafreniere.com"
Locate 37,38: Print "Ce programme peut ˆtre copi‚, modifi‚ ou distribu‚ librement.";

Do'                             L'EFFET DOPPLER DE LA MATIÈRE.
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  image = image + 1
  If image > images Then image = 1
  rotation = image * 2 * pi / images                      'rotation de phase selon l'image.
  For xPixel = -demiLargeur To demiLargeur
    xCarre = xPixel * xPixel
    For yPixel = 0 To demiHauteur
      If xPixel < 0 Then                                  'angle phi pour l'effet Doppler.
        phi = pi + Atn(yPixel / xPixel)
      Elseif xPixel = 0 Then
        If yPixel = 0 Then phi = 0 Else phi = pi / 2
      Else phi = Atn(yPixel / xPixel)
      End If

'********************************* EFFET DOPPLER « RELATIF » *********************************
      lambdaPrime = lambda * (cos(asin(beta * sin(phi))) - beta * cos(phi))
      
      distance = Sqr(yPixel * yPixel + xCarre)            'la distance xCarré est dilatée.
      flottante = distance / lambdaPrime                  'distance en longueurs d'onde.
      differenceDeMarche = flottante - Int(flottante)
      If flottante > .5 Then                              'hors du noyau central.
        phase = 2 * pi * differenceDeMarche               'phase en radians.
      Else                                                'intérieur du noyau central.
        flottante = .5                                    '0,5 à moduler arbitrairement.
        sag = rayon - Sqr(rayon^2 - differenceDeMarche^2) 'flèche d'un cercle fictif.
        differenceDeMarche = 1.25 * (.2 + sag)            'arrondir la courbe centrale.
        phase = 2 * pi * differenceDeMarche
      End If
      amplitude = Sin(phase - rotation) / flottante
      ton = 128 * (amplitude + 1)
      If ton > 255 Then ton = 255 Else If ton < 0 Then ton = 0
      Pset(demiLargeur + xPixel, demiHauteur - yPixel), Rgb(ton,ton,ton)
      Pset(demiLargeur + xPixel, demiHauteur + yPixel), Rgb(ton,ton,ton)
    Next
  Next
  line(demiLargeur, 0)-(demiLargeur, 2 * demiHauteur), noir  
  For y = demiHauteur - 7 * lambda To 2 * demiHauteur Step lambda
    Line(demiLargeur - 2, y)-(demiLargeur + 2, y), Rgb(255,255,255)
  Next
Loop Until Len(inkey)
