Screen 19,24,3
fond = Rgb(225,225,225): gris = Rgb(150,150,150): rouge = Rgb(255,0,0)
blanc = Rgb(255,255,255): turquoise = Rgb (230, 255, 255): vert = Rgb(1,150,0)
Screenset 2,0: page2 = 1: Color noir, fond: Cls
Gosub Titre
gabarit$ = "                                                                            "
lignes = 3                                                'nombre de lignes du texte.
th = 4                                                    'haut du texte.
tb = th + lignes                                          'bas du texte.
tg = 50 - Len(gabarit$) / 2 + 1                           'gauche du texte.
eh = th * 16 - 24                                         'haut de l'encadré.
eb = eh + 16 * lignes + 16                                'bas de l'encadré.
eg = tg * 8 - 32                                          'gauche de l'encadré.
ed = 800 - eg                                             'droite de l'encadré.
windowtitle "Ether00  -  L'Éther Virtuel de Philippe Delmotte."
Locate 4
Locate, 13: ? "       Cliquez sur l'un des programmes dont la liste figure ci-dessous, ou  "
Locate, 13: ? "appuyez sur la lettre indiqu‚e. Pour revenir … ce menu, appuyez sur le M.   "
Color vert
Locate, 13: ? "glafreniere.com   freebasic.net";
Locate, 49:   ? "Gabriel LaFreniŠre, le 23 janvier 2006.";
Color 1
ligne08$ = " A - Ether01. Oscillations selon la loi de Hooke.                              "
ligne09$ = " B - Ether02. Propagation d'une onde longitudinale selon la loi de Hooke.      "
ligne10$ = " C - Ether03. Diff‚rentes repr‚sentations de cette onde … une dimension.       "
ligne11$ = " D - Ether04. Le mˆme calcul d'une onde sur un corde, en plus pr‚cis.          "
ligne12$ = " E - Ether05. Les ondes circulaires dans un espace … deux dimensions.          "
ligne13$ = " F - Ether06. Calcul des ondes concentriques selon le principe de Huygens.     "
ligne14$ = " G - Ether07. Influence en raison de la distance; propagation sur une corde.   "
ligne15$ = " H - Ether08. A VOIR ABSOLUMENT - L'‚ther en 2-D avec diff‚rents exemples.     "
ligne16$ = " I - Ether09. Les ondes convergentes avec effet Doppler: l'‚lectron.           "
ligne17$ = " J - Ether10. L'‚lectron selon les transformations de Lorentz.                 "
ligne18$ = " K - Ether11. Les champs de force … deux ondes et leur structure.              "
ligne19$ = " L - Ether12. Les champs de force hyperboliques … trois ondes.                 "
ligne20$ = " M - Ether13. Le rayonnemment du champ de force.                               "
ligne21$ = " N - Ether14. Les transformations de Lorentz.                                  "
ligne22$ = " O - Ether15. La diffraction de Fresnel: laser, st‚nop‚, source lin‚aire, etc. "
ligne23$ = " P - Ether16. La tache d'Airy, renomm‚e l'ellipsoide d'Airy.                   "
ligne24$ = " Q - Ether17. Les transformations de Lorentz et l'effet Doppler.               "
ligne25$ = " R - Ether18. L'effet Doppler, Lorentz et l'Ether Virtuel. Le Scanner du Temps."
ligne26$ = " S - Ether19. La lame s‚paratarice de l'interf‚romŠtre de Michelson.           "
ligne27$ = " T - Ether20. La lumiŠre traverse les objets.                                  "
ligne28$ = " U - Ether21. Le Scanner du Temps et les transformations de Lorentz.           "
ligne36$ = " Pour mettre fin … ces programmes, appuyez sur [ Echap ] ou [ Alt + F4 ].      "
ligne37$ = "                                                                               "
Locate 08, 12: ? ligne08$
Locate 09, 12: ? ligne09$
Locate 10, 12: ? ligne10$
Locate 11, 12: ? ligne11$
Locate 12, 12: ? ligne12$
Locate 13, 12: ? ligne13$
Locate 14, 12: ? ligne14$
Locate 15, 12: ? ligne15$
Locate 16, 12: ? ligne16$
Locate 17, 12: ? ligne17$
Locate 18, 12: ? ligne18$
Locate 19, 12: ? ligne19$
Locate 20, 12: ? ligne20$
Locate 21, 12: ? ligne21$
Locate 22, 12: ? ligne22$
Locate 23, 12: ? ligne23$
Locate 24, 12: ? ligne24$
Locate 25, 12: ? ligne25$
Locate 26, 12: ? ligne26$
Locate 27, 12: ? ligne27$
Locate 28, 12: ? ligne28$
Locate 36, 12: ? ligne36$
Gosub flecheGauche
Gosub flecheDroite
Line (eg,eh)-(ed + 1, eb + 1), gris, B                    'encadré.
Line (eg + 1,eh + 1)-(ed, eb), noir, B
Line (eg + 1,eb)-(ed, eb), blanc
Line (eg,eb + 1)-(ed + 1, eb + 1), blanc
Line (ed,eh + 1)-(ed, eb), blanc
Line (ed + 1,eh)-(ed + 1, eb + 1), blanc

Do
  Swap page1, page2
  Screenset page1, page2
  Pcopy 2, page1
  Getmouse xSouris, ySouris,, clic: Locate 35, 3
  lignePrecedente = ligne
  ligne = .5 + ySouris / 16
  If xSouris > 706 Or xSouris < 86 Then ligne = 0
  
  If ligne Then
    Color noir, turquoise: Locate ligne, 12
    Select Case ligne
      Case 08: ? ligne08$
      Case 09: ? ligne09$
      Case 10: ? ligne10$
      Case 11: ? ligne11$
      Case 12: ? ligne12$
      Case 13: ? ligne13$
      Case 14: ? ligne14$
      Case 15: ? ligne15$
      Case 16: ? ligne16$
      Case 17: ? ligne17$
      Case 18: ? ligne18$
      Case 19: ? ligne19$
      Case 20: ? ligne20$
      Case 21: ? ligne21$
      Case 22: ? ligne22$
      Case 23: ? ligne23$
      Case 24: ? ligne24$
      Case 25: ? ligne25$
      Case 26: ? ligne26$
      Case 27: ? ligne27$
      Case 28: ? ligne28$
      Case 36: ? ligne36$;
      Case 37: ? ligne37$;
               If xSouris < 400 Then Gosub flecheGauche Else Gosub flecheDroite
    End Select
  End If

  If clic = 1 Then
    Select Case ligne
      Case 08: Run "Ether01.exe"
      Case 09: Run "Ether02.exe"
      Case 10: Run "Ether03.exe"
      Case 11: Run "Ether04.exe" 
      Case 12: Run "Ether05.exe" 
      Case 13: Run "Ether06.exe"
      Case 14: Run "Ether07.exe"
      Case 15: Run "Ether08.exe"
      Case 16: Run "Ether09.exe"
      Case 17: Run "Ether10.exe"
      Case 18: Run "Ether11.exe"
      Case 19: Run "Ether12.exe"
      Case 20: Run "Ether13.exe"
      Case 21: Run "Ether14.exe"
      Case 22: Run "Ether15.exe"
      Case 23: Run "Ether16.exe"
      Case 24: Run "Ether17.exe"
      Case 25: Run "Ether18.exe"
      Case 26: Run "Ether19.exe"
      Case 27: Run "Ether20.exe"
      Case 28: Run "Ether21.exe"
      Case 36: End
      Case 37: If xSouris < 400 Then Run "Ether21.exe" Else Run "Ether01.exe"
    End Select
  End If
  
  saisie$ = Inkey
  If Len(saisie$) = 2 Then saisie$ = Right(saisie$, 1) + "+" Else saisie$ = Ucase(saisie$)
  Select Case saisie$
    Case "A":  Run "Ether01.exe"
    Case "B":  Run "Ether02.exe"
    Case "C":  Run "Ether03.exe"
    Case "D":  Run "Ether04.exe"
    Case "E":  Run "Ether05.exe"
    Case "F":  Run "Ether06.exe"
    Case "G":  Run "Ether07.exe"
    Case "H":  Run "Ether08.exe"
    Case "I":  Run "Ether09.exe"
    Case "J":  Run "Ether10.exe"
    Case "K":  Run "Ether11.exe"
    Case "L":  Run "Ether12.exe"
    Case "M":  Run "Ether13.exe"
    Case "N":  Run "Ether14.exe"
    Case "O":  Run "Ether15.exe"
    Case "P":  Run "Ether16.exe"
    Case "Q":  Run "Ether17.exe"
    Case "R":  Run "Ether18.exe"
    Case "S":  Run "Ether19.exe"
    Case "T":  Run "Ether20.exe"
    Case "U":  Run "Ether21.exe"
    Case "K+": Run "Ether21.exe"                          'flèche gauche.
    Case "M+": Run "Ether01.exe"                          'flèche droite.
    Case Chr(27), "k+", "X+": End                         'le « X » de la fenêtre produit k ou parfois X.
  End Select    
Loop

Titre:'---------------------- EN-TÊTE - Reconstruire l'image numérisée.-----------------------
Read longueur
Read largeur
Read codePrec
Read nombre
Line(238,1)-(552,31), blanc, B
Line(239,2)-(553,32), blanc, B
Line(240,3)-(554,33), blanc, B
Line(241,4)-(555,34), rouge, BF
decaler = 400 - longueur / 2
For x = 0 To longueur
  For y = 0 To largeur
    If nombre = 0 Then
      Read nombre
      If codePrec Then codePrec = 0 Else codePrec = 1
    End If
    If codePrec Then Else Pset (x + decaler, y + 5), blanc
    nombre = nombre - 1
  Next
Next
Return

flecheGauche:'---------------------- DESSIN DES FLÈCHES --------------------------------------
xf = 400
xgg = xf - 50                                             'coordonnées.
xgd = xf - 20
xdg = xf + 20
xdd = xf + 50
yf = 584
Line (xgg+6,yf-4)-(xgg+8,yf-4),noir
Line (xgg+4,yf-3)-(xgg+8,yf-3),noir
Line (xgg+2,yf-2)-(xgg+8,yf-2),noir
Line (xgg,yf-1)-(xgd,yf-1),noir
Line (xgg-2,yf)-(xgd,yf),noir
Line (xgg,yf+1)-(xgd,yf+1),noir
Line (xgg+2,yf+2)-(xgg+8,yf+2),noir
Line (xgg+4,yf+3)-(xgg+8,yf+3),noir
Line (xgg+6,yf+4)-(xgg+8,yf+4),noir
Return

flecheDroite:
Line (xdd-8,yf-4)-(xdd-6,yf-4),noir
Line (xdd-8,yf-3)-(xdd-4,yf-3),noir
Line (xdd-8,yf-2)-(xdd-2,yf-2),noir
Line (xdg,yf-1)-(xdd,yf-1),noir
Line (xdg,yf)-(xdd+2,yf),noir
Line (xdg,yf+1)-(xdd,yf+1),noir
Line (xdd-8,yf+2)-(xdd-2,yf+2),noir
Line (xdd-8,yf+3)-(xdd-4,yf+3),noir
Line (xdd-8,yf+4)-(xdd-6,yf+4),noir
Return

' Cette image a été numérisée et compressée à l'aide d'un petit programme personnel.
Data 246,23,1,6,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,16,1,6,1,16,1,6,1,16,1,23,1,23,1,23,1,22,2,21,3,19,5,18,3,82,2,21,4,3,1,16,4,2,1,17,7,18,4,67,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18
Data 6,18,6,18,6,1,7,1,8,1,6,1,7,1,8,1,2,2,2,1,6,3,7,5,2,1,5,5,6,4,3,1,3,9,4,3,4,1,16,2,5,2,14,2,6,3,13,2,6,5,10,3,19,3,56,5,19,3,21,2,22,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,16,1,6
Data 1,16,1,6,1,16,1,6,2,22,3,21,5,19,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,7,1,8,1,6,1,7,1,8,1,6,1,7,1,8,1,14,1,15,1,7,1,8,1,6,1,7,1,8,1,6,1,7,1,8,1,6,18,6,18,6,18,6,18,6,1,16,1,6,1
Data 16,1,6,1,16,1,6,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,7,1,8,1,6,1,7,1,8,1,6,1,6,3,7,1,6,1,5,5,6,1,6,1,3,9,4,1,6,1,16,1,6,2,14,2,6,3,13,2,6,5,10,3,19,3,8,1,16,1,6,1,16,1,6,1,16,1
Data 6,18,6,18,6,18,6,18,6,1,8,1,7,1,6,1,8,1,7,1,6,1,8,3,5,1,6,1,8,4,11,2,6,6,10,3,4,9,8,9,2,6,8,8,3,6,8,6,5,5,9,4,8,3,22,2,23,1,23,1,174,1,23,1,23,2,22,4,20,7,17,9,15,11,13,1,3,10,10,1,5,10,16
Data 10,17,7,19,2,19,3,11,1,7,2,14,1,4,3,16,2,1,2,19,3,21,2,22,1,23,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,16,1,6,1,16,1,6,1,16,1,6,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,8
Data 1,7,1,6,1,8,1,7,1,6,1,8,3,5,1,6,1,8,4,11,2,6,6,10,3,4,9,8,9,2,6,8,8,3,6,8,6,5,5,9,4,8,3,22,2,23,1,23,1,6,5,19,3,21,2,22,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,16,1,6,1,16,1,6,1,16
Data 1,6,2,22,3,21,5,19,1,23,1,23,1,23,15,9,16,8,17,7,17,7,1,14,3,6,1,15,2,6,1,16,1,23,1,23,1,23,1,6,1,15,1,7,1,15,1,7,2,12,2,8,14,10,2,22,1,23,1,23,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18
Data 6,1,7,1,8,1,6,1,7,1,8,1,6,1,6,3,7,1,6,1,5,5,6,1,6,1,3,9,4,1,6,1,16,1,6,2,14,2,6,3,13,2,6,5,10,3,19,3,32,1,16,1,6,1,16,1,6,1,16,1,6,18,6,18,6,18,6,18,6,1,16,1,6,1,16,1,6,1,16,1,23,1,23,1,23
Data 1,22,2,21,3,19,5,18,3, 3