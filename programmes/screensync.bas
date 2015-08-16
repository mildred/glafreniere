SCREEN 18,24,1: depart = timer
' L'instruction « screensync » permet de réaliser des animations plus fluides en n'affichant
' une page que lorsqu'elle est complète. Elle permet aussi de régulariser le tempo quel que 
' soit la vitesse du processeur, mais il sera un peu plus lent si l'écran fonctionne sur
' 60 Hz plutôt que sur 85 Hz ou plus.
dim as Single angle, pas
color 0, rgb(225,225,225): cls
pi = 4 * atn(1): pas = 2 * pi / 85: rayon = 100

do

  line(320, 340)-(x, y), rgb(225,225,225)
  angle = angle - pas
  x = 320 + rayon * sin(angle)
  y = 340 + rayon * cos(angle)
  line(320, 340)-(x, y), 0
  compteur = compteur + 1'                                  'compter les balayages.
  if timer - depart >= 1 then                               'période d'une seconde.
    locate 10,5
    print "Nombre de balayages par seconde relev‚s sur cet ‚cran:"; compteur; " Hz. "
    locate 12,5
    print "D'autres programmes peuvent r‚duire ce compte."
    locate 14,5
    print "Appuyez sur une touche."
    compteur = 0: depart = timer                            'initialiser.
  end if
  
  screensync                                                'attendre le balayage suivant.

loop until len(inkey)
