screen 19,24,1: color noir, rgb(225,225,225): cls
dim as single P1,P2,P3, pas
P1 = 1                                                    'amplitude nominale.
gabarit = 100                                             'amplitude en pixels.
yCentre = 200 + gabarit
line(0, yCentre)-(799, yCentre), noir
pas = 1.1                            'instable si < 1, amortissement proportionnel au pas si > 1

do
  P3 = P2                                                 'mémoriser les deux derniers
  P2 = P1                                                 'états du potentiel « P ».
  P1 = P2 - P3 / pas                                      'évolution selon le pas.
  
  line(pixel-3, yCentre - gabarit * P2)-(pixel, yCentre - gabarit * P1), noir
  locate 30,5: print P1 * gabarit; "                "     'afficher l'amplitude.
  pixel += 4                                              'pas de 4 pixels.
  if pixel > 400 then pixel = 0: P1=1: P2=0: P3=0: cls: line(0, yCentre)-(799, yCentre), noir
  a$ = inkey
  if a$ = "p" or a$ = "P" then a$ = "": sleep: do: loop while len(inkey)'pause au besoin.
  sleep 200
loop until len(a$)
