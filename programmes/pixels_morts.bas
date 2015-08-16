Screeninfo w, h
Screenres w, h, 24, 1, 1
Do
  Color 0, Rgb(255,255,255): Cls
  Locate 30,20: Print "Pour quitter, appuyez sur Echap."
  Locate 35,20: Print "Fenˆtre ou plein-‚cran: appuyez sur Alt + Entr‚e."
  Locate 40,20: Print "Appuyez sur une autre touche pour changer la couleur."
  Sleep: a$ = inkey: If a$ = chr(27) Or right (a$,1) = "k" Then End
  Color 0, Rgb(255,255,255): Gosub Routine
  Color 0, Rgb(0,0,0):       Gosub Routine
  Color 0, Rgb(255,0,0):     Gosub Routine
  Color 0, Rgb(0,255,0):     Gosub Routine
  Color 0, Rgb(0,0,255):     Gosub Routine
  Color 0, Rgb(255,255,0):   Gosub Routine
  Color 0, Rgb(0,255,255):   Gosub Routine
  Color 0, Rgb(255,0,255):   Gosub Routine
Loop

Routine:
Cls: Sleep: a$ = inkey: If a$ = chr(27) Or right (a$,1) = "k" Then End
Return