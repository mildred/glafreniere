Screen 19,24,1: Color 0, Rgb(225,225,225): Cls
Locate 3, 4: Print "Renseignements sur la vitesse d'ex‚cution des variables sous FreeBASIC."
Const max = 1000000
Dim As Single temps, temps1, temps2
Dim As Integer a(0 To 4000, 0 To 4000)
Dim As Integer b(0 To 4000, 0 To 4000)
Locate 4

temps = Timer
For y=0 To 4000
  For x=0 To 4000
   a(y,x)=10
   nombre = nombre+1
  Next
Next
Print: Print
Locate , 4: Print "Temps n‚cessaire si les variables tableau d'une boucle sont dans l'ordre: ";
temps1 = Timer - temps
Print Using "##.## sec."; temps1


nombre = 0
temps = Timer
For y=0 To 4000
  For x=0 To 4000
   b(x,y)=10
   nombre = nombre+1
  Next
Next
Locate , 4: Print "Temps n‚cessaire si les variables sont invers‚es:";
temps2 = Timer - temps
Print Using "##.## "; temps2;
Gosub calcul

Print
a1 = 1:a2 = 2
temps = Timer
For x=0 To max
  a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2
  a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2
  a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2
  a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2
  a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2:a1 = a2
Next
Locate , 4: Print "Temps n‚cessaire pour affecter des valeurs … des variables Integer:";
temps1 = Timer - temps
Print Using "##.## sec"; temps1 

Dim As Byte e1, e2
e1 = 1:e2 = 2
temps = Timer
For x=0 To max
  e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2
  e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2
  e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2
  e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2
  e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2:e1 = e2
Next
Locate , 4: Print "Mˆme chose avec des variables Byte:";
temps2 = Timer - temps
Print Using "##.## "; temps2;
Gosub calcul

Dim As Short d1, d2
d1 = 1:d2 = 2
temps = Timer
For x=0 To max
  d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2
  d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2
  d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2
  d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2
  d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2:d1 = d2
Next
Locate , 4: Print "Mˆme chose avec des variables Short:";
temps2 = Timer - temps
Print Using "##.## "; temps2;
Gosub calcul

Dim As Single b1, b2
b1 = 1: b2 = 2
temps = Timer
For x=0 To max
  b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2
  b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2
  b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2
  b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2
  b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2:b1 = b2
Next
Locate , 4: Print "Mˆme chose avec des variables Single:";
temps2 = Timer - temps
Print Using "##.## "; temps2;
Gosub calcul

Dim As Double c1, c2
c1 = 1: c2 = 2
temps = Timer
For x=0 To max
  c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2
  c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2
  c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2
  c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2
  c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2:c1 = c2
Next
Locate , 4: Print "Mˆme chose avec des variables Double:";
temps2 = Timer - temps
Print Using "##.## "; temps2;
Gosub calcul
Print

Print
Dim As Single f, g
temps = Timer
For x=0 To max
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
  g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2:g = f ^ 2
Next
Locate , 4: Print "Temps n‚cessaire pour ‚lever des variables au carr‚:";
temps1 = Timer - temps
Print Using "##.## sec"; temps1 

temps = Timer
For x=0 To max
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
  g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f:g = f * f
Next
Locate , 4: Print "Pour multiplier des variable par elles-mˆmes:";
temps2 = Timer - temps
Print Using "##.## "; temps2;: Print "sec."
Locate , 4
Print "Le d‚lai est le mˆme. Sous QuickBASIC, il valait mieux ‚viter d'‚lever les variables au carr‚."

Print: Print
Locate , 4: Print "Pour quitter, appuyez sur une touche.";
Print
Color Rgb(0,150,0)
Locate 37, 2: Print "Ce programme peut ˆtre distribu‚, copi‚ ou modifi‚ librement. Gabriel LaFreniŠre. glafreniere.com";

Sleep: End

calcul:
Print "sec, soit "; 
Print Using "##.# "; temps2 / temps1;
Print " fois plus."
If Len(Inkey) Then End
Return