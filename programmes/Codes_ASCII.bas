Dim As String numero
Screen 18,24,2:Color 0, Rgb(225,225,225): Cls
caractere = 14:x = 2: y = 4
Locate 2, 31: Print "LISTE DES CODES ASCII"
Do
  If caractere < 100 Then
    numero = " " + Right$(Str$(caractere), 2)
  Else numero = Right$(Str$(caractere), 3)
  End If
  Locate y, x: Print numero; " "; Chr$(caractere);
  caractere = caractere + 1
  y = y + 1: If y = 28 Then y = 4: x = x + 8
Loop Until caractere = 254
Locate 29,2: Print "Appuyez sur une touche. Page suivante: la liste des codes cach�s pour obtenir"
Locate 30,2: Print "des caract�res accentu�s. Ils ne sont visibles que dans le code source.";
Sleep: Cls: If Asc(Inkey$) = 255 Then End         'correspond au "X" de la fen�tre.

Locate 1, 1: Print "               "
Locate 2, 26: Print "LISTE DES CARACTERES ACCENTU�S"
Print
Print " Les caract�res sp�ciaux qui figurent ci-dessous ont �t� obtenus � l'aide"
Print " d'un caract�re particulier, une sorte � d'hi�roglyphe � qui n'est visible"
Print " que dans le code source."
Print
Print " CHR$(128)  �   c c�dille majuscule."
Print " CHR$(129)  �   u tr�ma."
Print " CHR$(130)  �   e accent aigu."
Print " CHR$(131)  �   a accent circonflexe."
Print " CHR$(133)  �   a accent grave."
Print " CHR$(135)  �   c c�dille."
Print " CHR$(136)  �   e accent circonflexe."
Print " CHR$(137)  �   e tr�ma."
Print " CHR$(138)  �   e accent grave."
Print " CHR$(139)  �   i tr�ma."
Print " CHR$(140)  �   i accent circonflexe."
Print " CHR$(144)  �   e accent aigu majuscule(?)."
Print " CHR$(147)  �   o accent circonflexe."
Print " CHR$(148)  �   o tr�ma."
Print " CHR$(150)  �   u accent circonflexe."
Print " CHR$( ? )  �   u accent grave."
Print " CHR$(174)  �   guillemet gauche. �  �"
Print " CHR$(175)  �   guillemet droit."
Print " CHR$(159)  �   f ancienne fonte."
Print " � � � � � � � � � � � � � � � � � � � � � �"
Print " � � � � � � � � � � � � � � � � � � � � � � � � �"
Color Rgb(0,150,0)
Locate 29, 2: Print "Le 31 janvier 2006.  Gabriel LaFreni�re.  glafreniere.com"
Locate 30, 2: Print "Ce programme peut �tre distribu�, copi� ou modifi� librement.";
Sleep
