Screen 19,24,1: Color 0, Rgb(225,225,225): Cls
Locate 2, 31: Print "Renseignements sur les variables de FreeBASIC."
Locate 4
Dim b As Single
Dim c As Double
a = 4 * Atn(1)
Locate, 2:  Print "  a = "; a; "                     Les variables non d‚clar‚es sont du type Integer par d‚faut, 4 bytes."
a = -2147483648
Locate, 2:  Print "  a = "; a; "            Variable non d‚clar‚e Integer, minimum: -2147483648."
a = 2147483647
Locate, 2:  Print "  a = "; a; "            Variable non d‚clar‚e Integer, maximum:  2147483647"
Dim As Single d, e, f
Print
g! = (4 * Atn(1)) ^ 77
h% = 4 * Atn(1) * 10 ^ 8
i& = 4 * Atn(1) * 10 ^ 8

k# = 4 * Atn(1)
Locate, 2: Print " g! = "; g!; "         D‚claration automatique Single par: !"
Locate, 2: Print " h% = "; h%; "             D‚claration automatique Integer inutile, sauf pour identification."
Locate, 2: Print " i& = "; i&; "             D‚claration automatique Longint impossible."
Locate, 2: Print " k# = "; k#; "      D‚claration automatique Double par: #."
r$ = "Gabriel LaFreniŠre"
Locate, 2: Print " r$ =  "; r$; "    D‚claration automatique String par: $ (1 byte par caractŠre)."
Print

b = 4 * Atn(1)
c = 4 * Atn(1)
Locate, 2:  Print "  b = "; b; "              Variable d‚clar‚e Single:  8 chiffres … virgule flottante (4 bytes)."
Locate, 2:  Print "  c = "; c; "      Variable d‚clar‚e Double: 16 chiffres … virgule flottante (8 bytes)."
Dim l As Byte
l = 4 * Atn(1) * 10 ^ 1
Locate, 2: Print "  l = "; l; "                    Variable d‚clar‚e Byte:   entier de -128 … 127 (1 byte)."
Dim la As Ubyte
Ub = 255
Locate, 2: Print " Ub = "; Ub; "                   Variable d‚clar‚e Ubyte:  entier de 0 … 255 (1 byte)."
Dim m As Short
m = 4 * Atn(1) * 10 ^ 4
Locate, 2: Print "  m = "; m; "                 Variable d‚clar‚e Short:  entier de -32768 … 32767 (2 bytes)."
Dim Us As Ushort
Us = 65535
Locate, 2: Print " Us =  "; Us; "                 Variable d‚clar‚e Ushort: entier de 0 … 65535 (2 bytes)."
Dim j As Longint
j = 1234567890123456789
Dim As Uinteger Ui
Ui = 4294967295
Locate, 2:  Print " Ui =  "; Ui; "            Variable d‚clar‚e Uinteger, de 0 … 4294967295 (4 bytes)."
Dim Ul As Ulongint
Ul = 18446744073709551615
Locate, 2: Print "  j = "; j; "   Variable d‚clar‚e Longint: -9223372036854775808 … 9223372036854775807"
Locate, 2: Print " Ul =  "; Ul; "  Variable d‚clar‚e Ulongint: 0 … 18446744073709551615 (8 bytes)."
Print
Const pi = 4 * Atn(1)                                       'Const Double par défaut.
Const pi2 As Single = 4 * Atn(1)                            'échec: Const demeure Double.
Locate, 2: Print " pi = "; pi;  "      Constante Const: Double par d‚faut. A savoir: Const pi = 4 * atn(1)"
Locate, 2: Print "pi2 = "; pi2; "      Impossible de d‚clarer Const pi2 en tant que Single."
d = 4 * Atn(1)
e = 4 * Atn(1)
Locate, 2: Print "  d = "; d; "              Deux variables (ou plus) de type Single d‚clar‚es simultan‚ment."
Locate, 2: Print "  e = "; e; "              ... cette proc‚dure fonctionne avec tous les types de variables."


Print
k# = d
Locate, 2: Print " k# = "; k#;  "      Une variable Single vers‚e dans une variable Double laisse un"
Locate , 3: Print "      (valeur inexacte)     r‚sidu de 8 chiffres inexacts. En cons‚quence, tout calcul"
Locate , 31: Print "impliquant des variables Double qui fait intervenir une seule"
Locate , 31: Print "variable Single perd toute sa pr‚cision."
Print
a = 3
k# = 4 * Atn(1) / a * a
Locate, 2: Print " k# = "; k#;  "      Une variable Double et une variable … nombre entier font bon m‚nage."
Locate 36, 2: Print "Janvier 2006. Pour quitter, appuyez sur une touche.";
Color Rgb(0,150,0)
Locate 37, 2: Print "Ce programme peut ˆtre distribu‚, copi‚ ou modifi‚ librement. Gabriel LaFreniŠre. glafreniere.com";
Sleep: End
