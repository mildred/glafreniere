' This is a FreeBasic program. Created April 9, 2008 by Gabriel LaFreniere. Updated April 18, 2008.
' Please download the editor/compiler from: http://fbide.freebasic.net
#lang "fblite" 
Option Gosub 
Dim As Double pi = 4 * Atn(1), accurate.step, sine, cosine
accurate.step = 2 * sin(pi / 360)                                     ' Mr. Dewavrin's original (Oct. 2006) step was:
Screen 20,24,1: Color black, Rgb(225,225,225): Cls                    ' Sin(4 * pi / lambda) / Sin(2 * pi / lambda)
Windowtitle "Is Euler's method accurate?"
cosine = Cos(accurate.step / 2)                                       ' Jocelyn Marcotte's initialization, April 5, 2008.

For angle = 0 To 90
  x = 3: y = angle + 2
  If angle > 45 Then x = 53: y = angle - 43
  Locate y, x: Print "Sin";
  If angle < 10 Then Print " ";
  Print angle;: Print Chr(248);                                       ' The cosine is accurate too but it indicates
  Print Using " = ##.#########"; sine                                 ' magnitudes for intermediate angles: 45.5°, etc.
  
  sine  =  sine + cosine * accurate.step                              ' Anselme Dewavrin's amazing algorithm
  cosine = cosine - sine * accurate.step                              ' for Euler's method (Oct. 2006).

Next

Locate  3, 82: Print "Euler's method is (almost) accurate."
Locate  5, 82: Print "Sine and cosine are given by Mr. Anselme"
Locate  6, 82: Print "Dewavrin's amazingly simple algorithm found"
Locate  7, 82: Print "in October 2006: "
Locate  9, 82: Print "sine  =  sine + cosine * step"
Locate 10, 82: Print "cosine = cosine - sine * step"
Locate 12, 82: Print "The step was formerly given by:"
Locate 14, 82: Print "2 * pi / 360 = 0.017453292"
Locate 16, 82: Print "This step was inaccurate, though."
Locate 18, 82: Print "The correct step is given by:"
Locate 20, 82: Print "2 * sin(pi / 360) = 0.017453071"
Locate 24, 82: Print "Finally, Mr. Jocelyn Marcotte found in"
Locate 25, 82: Print "April 2008 that the cosine had to be"
Locate 26, 82: Print "initialized this way (instead of 1):"
Locate 28, 82: Print "cosine = Cos(step / 2) = 0.99996192"
Locate 30, 82: Print "Sine is now accurate up to 9 digits. The"
Locate 31, 82: Print "cosine offset remains as a result of its"
Locate 32, 82: Print "initialization but magnitudes are still "
Locate 33, 82: Print "correct for half degrees: 0.5, 1.5, etc."
Locate 34, 82: Print "This is a minor problem because cosine may"
Locate 35, 82: Print "be recovered using Pythagoras' theorem:"
Locate 37, 82: Print "Sqr(1 - sine ^ 2)"
Locate 40, 82: Print "Press any key to quit."

 
Locate 42, 82: Print Using "pi (system)  ##.####################"; pi
Locate 43, 82:       Print "pi (accurate) 3.14159265358979323846..."
Locate 47, 28: Print Using "System #.############"; Sin(pi / 4)
Locate 46, 82: Print "April 13, 2008. Updated April 18, 2008."
Locate 47, 82: Print "Gabriel LaFreniere    glafreniere.com"
Sleep
