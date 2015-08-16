Dim As Single pi = 4 * Atn(1)
Dim As Single AB, BC, AC, BCA, arc, somme, surface
Screen 19,24,1: Color noir, Rgb(225,225,225): Cls

' LA DIFFRACTION DE FRESNEL - MÉTHODE PAR SOMMATION D'ARCS DE CERCLE CONCENTRIQUES.

yCentre = 250: rayon = 100                                'rayon de la source.
pas = 2 * rayon / 32'                                     'balayer par 32 arcs de cercles.
AB = rayon: AC = 200: BC = pas / 2
Line(0,yCentre)-(799,yCentre), noir
Circle(200, yCentre), rayon, noir

Do
  BCA = Acos((AC ^ 2 + BC ^ 2 - AB ^ 2) / (2 * AC * BC))  'loi des cosinus, théorème d'Al Kashi.
  If AC > AB + BC Then
    arc = 0: BCA = 0                                      'cercle entièrement à l'extérieur.
  Elseif AC < AB - BC Then
    arc = 2 * pi * BC                                     'cercle entièrement à l'intérieur.
  Else
    arc = BC * BCA                                        'arc de cercle dans la source.
  End If
  Circle(AC + 200, yCentre), BC, noir
  Circle(AC + 200, yCentre), BC, Rgb(255,0,0), pi- BCA, pi + BCA
  somme = somme + arc
  BC = BC + pas
Loop While BC < AC + rayon

surface = pi * rayon ^ 2
Locate 36, 4: Print "Surface du cercle selon pi * R ^ 2:      "; surface; " (en pixels)."
Locate 37, 4: Print "Surface approximative selon la sommation:"; 2 * somme * pas; "                      glafreniere.com"
'l'arc de cercle ne couvre qu'une moitié de la source, d'où la multiplication par 2.
Sleep
