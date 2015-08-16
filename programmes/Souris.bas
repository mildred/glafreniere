' par Gabriel LaFrenière, mise à jour le 1er novembre 2005.
screen 18,24,2: color 0, rgb(225,225,225): cls
locate 3, 11: print "Prise en charge de la souris en FreeBASIC."
locate 4, 11: print "Cliquez sur l'un des choix ci-dessous."
Choix1$ = " Choix No. 1. ": locate 10, 10: print Choix1$    'afficher les choix.
Choix2$ = " Choix No. 2. ": locate 11, 10: print Choix2$
Choix3$ = " Choix No. 3. ": locate 12, 10: print Choix3$
Choix4$ = " Quitter ici. ": locate 13, 10: print Choix4$

do
  getmouse xSouris, ySouris, roulette, clic                 'relever l'état de la souris.
  color 0, rgb(225,225,225): locate 16
  locate , 10 : print "     Coordonn‚e x du pixel : "; xSouris; "  "
  locate , 10 : print "     Coordonn‚e y du pixel : "; ySouris; "  "
  locate , 10 : print "                  Roulette : "; roulette; "  "
  locate , 10 : print "                    Bouton : "; clic; "  "
  lignePrecedente = ligne
  ligne = .5 + ySouris / 16
  if ligne < 10 or ligne > 13 or xSouris < 72 or xSouris > 184 then ligne = 0
  
  if ligne <> lignePrecedente then'                         'seulement s'il y a modification.
    color 0, rgb(225,225,225): locate lignePrecedente, 10
    select case lignePrecedente                             'rétablir l'affichage.
      case 10: print Choix1$
      case 11: print Choix2$
      case 12: print Choix3$
      case 13: print Choix4$
    end select
    color 0, rgb (230, 255, 255): locate ligne, 10
    select case ligne                                       'rehausser l'affichage.
      case 10: print Choix1$
      case 11: print Choix2$
      case 12: print Choix3$
      case 13: print Choix4$
    end select
  end if

  if clic = 1 then                                          'agir s'il y a eu clic.
    locate 7, 10
    select case ligne
      case 10: print Choix1$
      case 11: print Choix2$
      case 12: print Choix3$
      case 13: end
    end select
  end if
loop until len(inkey)
