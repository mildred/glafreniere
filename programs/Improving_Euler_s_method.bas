' This is a FreeBasic program. Created April 6, 2008 by Gabriel LaFreniere. Updated April 8, 2008.
' Please download the editor/compiler from: http://fbide.freebasic.net
#lang "fblite" 
Option Gosub 
Dim As Double pi = 4 * Atn(1), inaccurate.step, accurate.step
Dim As Double sine, cosine, angle, true.sine, true.cosine, ratio
work.page = 1: Screen 20,24,3: Gosub Initialization


' ********************************************************************
' MAIN LOOP.
' ********************************************************************

Do
  Screenset work.page, visible.page
  Pcopy 2, work.page
  iteration = 0
  sine   = 0
  cosine = 1
  y.previous.1 = y.upper
  y.previous.2 = y.upper - amplitude
  y.previous.3 = y.upper
  y.previous.4 = y.upper

' ********************************************************************
' UPPER GRAPHICS - ANSELME DEWAVRIN'S OCT. 2006 SIMPLIFIED ALGORITHM USING *INACCURATE* STEP.
' ********************************************************************

  For x = 0 To lambda
    
    angle = 2 * pi * x / lambda
    true.sine   = Sin(angle)
    true.cosine = Cos(angle)

    y.point = y.upper - amplitude * sine
    Line(x.left+x, y.previous.1)-(x.left+x, y.point), black           'inaccurate sine, black curve.
    y.previous.1 = y.point

    y.point = y.upper - amplitude * cosine
    Line(x.left+x, y.previous.2)-(x.left+x, y.point), green           'inaccurate cosine, green curve.
    y.previous.2 = y.point

    y.point = y.upper - lambda ^ 2 * (true.sine - sine)
    Line(x.left+x, y.previous.3)-(x.left+x, y.point), red             'sine error, red curve.
    y.previous.3 = y.point

    y.point = y.upper - .01 * lambda ^ 2 * (true.cosine - cosine)
    Line(x.left+x, y.previous.4)-(x.left+x, y.point), blue            'cosine error, blue curve enlarged 100x.
    y.previous.4 = y.point

    If x = 45 Then
      Locate 5, 106: Print Using "##.########"; sine
      Locate 6, 106: Print Using "##.########"; -Abs(true.sine - sine)'negative to avoid digit display malfunction.
      Locate 11,106: Print Using "##.########"; cosine
      Locate 12,106: Print Using "##.########"; -Abs(true.cosine - cosine)
    Elseif x = 90 Then
      Locate 7, 106: Print Using "##.########"; sine
      Locate 8, 106: Print Using "##.########"; -Abs(true.sine - sine)
      Locate 13,106: Print Using "##.########"; cosine
      Locate 14,106: Print Using "##.########"; -Abs(true.cosine - cosine)
    Elseif x = 360 Then
      Locate 9, 106: Print Using "##.########"; -Abs(sine)
      Locate 10,106: Print Using "##.########"; -Abs(true.sine - sine)
      Locate 15,106: Print Using "##.########"; cosine
      Locate 16,106: Print Using "##.########"; -Abs(true.cosine - cosine)
    End If

    sine  =  sine + cosine * inaccurate.step                          'upper graphics: inaccurate step.
    cosine = cosine - sine * inaccurate.step

    iteration += 1
    
  Next


' ********************************************************************
' CENTER GRAPHICS - ANSELME DEWAVRIN'S OCT. 2006 SIMPLIFIED ALGORITHM USING *ACCURATE* STEP.
' ********************************************************************

  iteration = 0
  sine   = 0
  cosine = 1
  y.previous.1 = y.upper
  y.previous.2 = y.upper - amplitude
  y.previous.3 = y.upper
  y.previous.4 = y.upper

  For x = 0 To lambda
    
    angle = 2 * pi * x / lambda
    true.sine   = Sin(angle)
    true.cosine = Cos(angle)

    y.point = y.center - amplitude * sine
    Line(x.left+x, y.previous.1)-(x.left+x, y.point), black           'still inaccurate sine, black curve.
    y.previous.1 = y.point

    y.point = y.center - amplitude * cosine
    Line(x.left+x, y.previous.2)-(x.left+x, y.point), green           'still inaccurate cosine, green curve.
    y.previous.2 = y.point

    y.point = y.center - lambda ^ 2 * (true.sine - sine)
    Line(x.left+x, y.previous.3)-(x.left+x, y.point), red             'sine error red curve.
    y.previous.3 = y.point

    y.point = y.center - .01 * lambda ^ 2 * (true.cosine - cosine)
    Line(x.left+x, y.previous.4)-(x.left+x, y.point), blue            'cosine error blue curve enlarged 100x.
    y.previous.4 = y.point

    If x = 45 Then
      Locate 19,106: Print Using "##.########"; sine
      Locate 20,106: Print Using "##.########"; -Abs(true.sine - sine)
      Locate 25,106: Print Using "##.########"; cosine
      Locate 26,106: Print Using "##.########"; -Abs(true.cosine - cosine)
    Elseif x = 90 Then
      Locate 21,106: Print Using "##.########"; sine
      Locate 22,106: Print Using "##.########"; -Abs(true.sine - sine)
      Locate 27,106: Print Using "##.########"; cosine
      Locate 28,106: Print Using "##.########"; -Abs(true.cosine - cosine)
    Elseif x = 360 Then
      Locate 23,106: Print Using "##.########"; -Abs(sine)
      Locate 24,106: Print Using "##.########"; -Abs(true.sine - sine)
      Locate 29,106: Print Using "##.########"; cosine
      Locate 30,106: Print Using "##.########"; -Abs(true.cosine - cosine)
    End If

    sine  =  sine + cosine * accurate.step                            'Anselme Dewavrin's accurate step.
    cosine = cosine - sine * accurate.step

    iteration += 1
    
  Next


' ********************************************************************
' LOWER GRAPHICS - CORRECTION USING *ACCURATE* STEP.
' ********************************************************************

  iteration = 0
  sine   = 0
  cosine = Cos(accurate.step/2)                                       'Mr. Jocelyn Marcotte's cosine initialization.
  y.previous.1 = y.upper
  y.previous.2 = y.upper - amplitude
  y.previous.3 = y.upper
  y.previous.4 = y.upper

  For x = 0 To lambda
    
    angle = 2 * pi * x / lambda
    true.sine   = Sin(angle)
    true.cosine = Cos(angle)

    y.point = y.lower - amplitude * sine
    Line(x.left+x, y.previous.1)-(x.left+x, y.point), black           'sine, black curve.
    y.previous.1 = y.point

    y.point = y.lower - amplitude * cosine
    Line(x.left+x, y.previous.2)-(x.left+x, y.point), green           'cosine, green curve.
    y.previous.2 = y.point

    y.point = y.lower - lambda ^ 2 * (true.sine - sine)
    Line(x.left+x, y.previous.3)-(x.left+x, y.point), red             'sine error red curve.
    y.previous.3 = y.point

    y.point = y.lower - .01 * lambda ^ 2 * (true.cosine - cosine)
    Line(x.left+x, y.previous.4)-(x.left+x, y.point), blue            'cosine error blue curve enlarged 100x.
    y.previous.4 = y.point

    If x = 45 Then
      Locate 33,106: Print Using "##.########"; sine
      Locate 34,106: Print Using "##.########"; -Abs(true.sine - sine)
    Elseif x = 90 Then
      Locate 35,106: Print Using "##.########"; sine
      Locate 36,106: Print Using "##.########"; -Abs(true.sine - sine)
    Elseif x = 360 Then
      Locate 37,106: Print Using "##.########"; -Abs(sine)
      Locate 38,106: Print Using "##.########"; -Abs(true.sine - sine)
    End If

    sine  =  sine + cosine * accurate.step                            'Anselme Dewavrin's accurate step.
    cosine = cosine - sine * accurate.step

    iteration += 1
    
  Next

  Screenset, work.page
  Sleep:End
Loop


'*********************************************************************
' INITIALIZATION.
' ********************************************************************

Initialization:
Windowtitle "Improving Euler's method"
red   = Rgb(255,0,0)
blue  = Rgb(75,125,255)
cyan  = Rgb(0,100,100)
gold  = Rgb(250,200,130)
gray  = Rgb(125,125,125)
green = Rgb(0,200,0)
white = Rgb(255,255,255)
yellow      = Rgb(255,240,180)
dark.gray   = Rgb(75,75,75)
green.text  = Rgb(0,100,0)
background  = Rgb(225,225,225)
light.green = Rgb(175,255,175)

lambda = 360
inaccurate.step = 2 * pi / lambda                                     'basic step in radians for Delmotte's algorithm.
'accurate.step=Sqr(2 - (Sin(4 * pi / lambda) / Sin(2 * pi / lambda))) 'first version of Dewarin's step (Oct. 2006).
accurate.step = 2 * sin(pi / lambda)                                  'April 2008 version, waiting for the author's
amplitude = 100                                                       'permission to reveal his name.
y.upper  = 10 * 16
y.center = 24 * 16
y.lower  = 38 * 16
x.left = 512 - lambda / 2
iteration = 0
sine   = 0
cosine = 1
Screenset 2, 2: Color black, background: Cls
Line(x.left, y.upper - amplitude)-(x.left + lambda, y.upper + amplitude), white, b
Line(x.left, y.center - amplitude)-(x.left + lambda, y.center + amplitude), white, b
Line(x.left, y.lower - amplitude)-(x.left + lambda, y.lower + amplitude), white, b
For x = x.left To x.left + lambda Step lambda / 4
  Line(x, y.upper - amplitude +1)-(x, y.upper + amplitude -1), gray   'lambda / 4 marks.
  Line(x, y.center -amplitude +1)-(x, y.center +amplitude -1), gray
  Line(x, y.lower - amplitude +1)-(x, y.lower + amplitude -1), gray
Next
Line(x.left, y.upper)-( x.left + lambda, y.upper),  gray
Line(x.left, y.center)-(x.left + lambda, y.center), gray
Line(x.left, y.lower)-( x.left + lambda, y.lower),  gray
Gosub Title
Color gray, background
Locate 47, 3: Print "Thanks to the creators of FreeBASIC."
Locate 48, 3: Print "Gabriel LaFreniere  glafreniere.com";
Locate 47,89: Print "April 17, 2008. This program may be"
Locate 48,89: Print "freely distributed, copied or modified.";
Color black
Locate 2, 3:  Print "Black: sine curve."
Locate 3, 3:  Print "Green: cosine curve."
Locate 4, 3:  Print "Red:   sine error."
Locate 5, 3:  Print "Blue:  cosine error / 100."
Locate 2, 99: Print "Lambda = 360 pixels in order"
Locate 3, 99: Print "to reproduce the 360";Chr(248);" cycle."

Locate 7, 3:  Print "          DIAGRAM 1"
Locate 9, 3:  Print "Below is Mr. Anselme Dewavrin's "
Locate 10,3:  Print "simplified algorithm for Euler's"
Locate 11,3:  Print "method. The sine curve is asymmetric  "
Locate 12,3:  Print "and the cosine error is severe. "
Locate 14,3:  Print "step = 2 * pi / lambda"
Locate 15,3:  Print "sine  =  sine + cosine * step"
Locate 16,3:  Print "cosine = cosine - sine * step"

Locate 19,3:  Print "          DIAGRAM 2"
Locate 21,3:  Print "Here, the algorithm is the same but"
Locate 22,3:  Print "the step is Mr. Dewavrin's improved"
Locate 23,3:  Print "one. This option produces very small"
Locate 24,3:  Print "error for 180";Chr(248);" angle or multiples."
Locate 25,3:  Print "In addition, the sine error curve"
Locate 26,3:  Print "becomes perfectly symmetrical. The"
Locate 27,3:  Print "cosine error is still severe, though. "
Locate 28,3:  Print "The accurate step is given by:        "
Locate 30,3:  Print "       2 * sin(pi / lambda)           "

Locate 33,3:  Print "          DIAGRAM 3"
Locate 35,3:  Print "The goal is to correct the sine error."
Locate 36,3:  Print "Mr. Jocelyn Marcotte discovered in    "
Locate 37,3:  Print "April 2008 that this can be achieved  "
Locate 38,3:  Print "by initializing the cosine magnitude  "
Locate 39,3:  Print "to: "
Locate 41,3:  Print "     cosine = Cos(step / 2)           "
Locate 43,3:  Print "Finally, the cosine magnitude may be  "
Locate 44,3:  Print "deduced from sine using Pythagoras'   "
Locate 45,3:  Print "theorem: Sqr(1 - sine ^ 2)."


Locate 5, 91: Print "Sin 45";Chr(248);".......              Wrong"
Locate 6, 91: Print "Sine error...."
Locate 7, 91: Print "Sin 90";Chr(248);".......              Wrong"
Locate 8, 91: Print "Sine error...."
Locate 9, 91: Print "Sin 360";Chr(248);"......              Wrong"
Locate 10,91: Print "Sine error...."
Locate 11,91: Print "Cos 45";Chr(248);"......."
Locate 12,91: Print "Cosine error..              Severe"
Locate 13,91: Print "Cos 90";Chr(248);"......."
Locate 14,91: Print "Cosine error..              Severe"
Locate 15,91: Print "Cos 360";Chr(248);"......"
Locate 16,91: Print "Cosine error.."

Locate 19,91: Print "Sin 45";Chr(248);".......              Wrong"
Locate 20,91: Print "Sine error...."
Locate 21,91: Print "Sin 90";Chr(248);".......              Wrong"
Locate 22,91: Print "Sine error...."
Locate 23,91: Print "Sin 360";Chr(248);"......              Accurate"
Locate 24,91: Print "Sine error....              Accurate"
Locate 25,91: Print "Cos 45";Chr(248);"......."
Locate 26,91: Print "Cosine error..              Severe"
Locate 27,91: Print "Cos 90";Chr(248);"......."
Locate 28,91: Print "Cosine error..              Severe"
Locate 29,91: Print "Cos 360";Chr(248);"......              Accurate"
Locate 30,91: Print "Cosine error..              Accurate"

Locate 33,91: Print "Sin 45";Chr(248);".......              Accurate"
Locate 34,91: Print "Sine error....              Accurate"
Locate 35,91: Print "Sin 90";Chr(248);".......              Accurate"
Locate 36,91: Print "Sine error....              Accurate"
Locate 37,91: Print "Sin 360";Chr(248);"......              Accurate"
Locate 38,91: Print "Sine error....              Accurate"
Return


'*********************************************************************
' ENLARGED TITLE USING MY ORIGINAL DEPIXELATION METHOD. 
'*********************************************************************

Title:
title$ = "Improving Euler's method"
Locate 1,1: Print title$
For x1 = 0 To 8 * Len(title$)
  x2 = 512 + 2 * x1 - 8 * Len(title$)
  For potential = 1 To 14
    If Point(x1, potential) = black Then
      memory.1 = 2 * potential + 2
      Line(x2, memory.1)-(x2, memory.1 + 1), gold                     'provisionary color.
      If Point(x1 + 1, potential - 1) = 0 Then Line(x2 + 1, memory.1 - 1)-(x2 + 1, memory.1 + 0), gold
      If Point(x1 + 1, potential + 0) = 0 Then Line(x2 + 1, memory.1 + 0)-(x2 + 1, memory.1 + 1), gold
      If Point(x1 + 1, potential + 1) = 0 Then Line(x2 + 1, memory.1 + 1)-(x2 + 1, memory.1 + 2), gold
    End If
  Next
  If (x1 + 1) Mod 8 Then Else Line(x2 + 1, 0)-(x2 + 1, 34), background'separate invasive characters such as capital M.
Next
Line(0, 0)-(8 * Len(title$), 14), background, bf                      'matrix title erased.
For x = 512 - 8 * Len(title$) To 512 + 8 * Len(title$)
  For y = 0 To 34
    If Point(x, y) = gold And Point(x-1, y-1) = background Then Pset(x-1, y-1),yellow 'shedding light on top.
    If Point(x, y) = gold And Point(x+1, y+1) = background Then Pset(x+1, y+1), black 'throwing shade on bottom.
  Next
Next
For x = 512 - 8 * Len(title$) To 512 + 8 * Len(title$)                'adding luster.
  For y = 4 To 32
    ratio = 1 / ((Abs(18 - y))^1.5 / 40 + 1)
    If Point(x, y) = gold Then Pset(x, y), Rgb(255 * ratio, 210 * ratio, 150 * ratio)
  Next
Next
Return

