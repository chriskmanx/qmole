Start Maxima with the command "maxima".  Maxima will display version
information and a prompt.  End each Maxima command with a semicolon.
End the session with the command "quit();".  Here's a sample session:

@example
[wfs@@chromium]$ maxima
Maxima 5.9.1 http://maxima.sourceforge.net
Using Lisp CMU Common Lisp 19a
Distributed under the GNU Public License. See the file COPYING.
Dedicated to the memory of William Schelter.
This is a development version of Maxima. The function bug_report()
provides bug reporting information.
(%i1) factor(10!);
                            8  4  2
(%o1)                      2  3  5  7
(%i2) expand ((x + y)^6);
       6        5       2  4       3  3       4  2      5      6
(%o2) y  + 6 x y  + 15 x  y  + 20 x  y  + 15 x  y  + 6 x  y + x
(%i3) factor (x^6 - 1);
                              2            2
(%o3)       (x - 1) (x + 1) (x  - x + 1) (x  + x + 1)
(%i4) quit();
[wfs@@chromium]$
@end example

Maxima can search the info pages.  Use the @mref{describe} command to show
information about the command or all the commands and variables containing 
a string.
The question mark @mref{?} (exact search) and double question mark @mref{??}@w{}
(inexact search) are abbreviations for @code{describe}:

@example
(%i1) ?? integ
 0: Functions and Variables for Elliptic Integrals
 1: Functions and Variables for Integration
 2: Introduction to Elliptic Functions and Integrals
 3: Introduction to Integration
 4: askinteger  (Functions and Variables for Simplification)
 5: integerp  (Functions and Variables for Miscellaneous Options)
 6: integer_partitions  (Functions and Variables for Sets)
 7: integrate  (Functions and Variables for Integration)
 8: integrate_use_rootsof  (Functions and Variables for Integration)
 9: integration_constant_counter  (Functions and Variables for
    Integration)
 10: nonnegintegerp  (Functions and Variables for linearalgebra)
Enter space-separated numbers, `all' or `none': 5 4

 -- Function: integerp (<expr>)
     Returns `true' if <expr> is a literal numeric integer, otherwise
     `false'.

     `integerp' returns false if its argument is a symbol, even if the
     argument is declared integer.

     Examples:

          (%i1) integerp (0);
          (%o1)                         true
          (%i2) integerp (1);
          (%o2)                         true
          (%i3) integerp (-17);
          (%o3)                         true
          (%i4) integerp (0.0);
          (%o4)                         false
          (%i5) integerp (1.0);
          (%o5)                         false
          (%i6) integerp (%pi);
          (%o6)                         false
          (%i7) integerp (n);
          (%o7)                         false
          (%i8) declare (n, integer);
          (%o8)                         done
          (%i9) integerp (n);
          (%o9)                         false

 -- Function: askinteger (<expr>, integer)
 -- Function: askinteger (<expr>)
 -- Function: askinteger (<expr>, even)
 -- Function: askinteger (<expr>, odd)
     `askinteger (<expr>, integer)' attempts to determine from the
     `assume' database whether <expr> is an integer.  `askinteger'
     prompts the user if it cannot tell otherwise, and attempt to
     install the information in the database if possible.  `askinteger
     (<expr>)' is equivalent to `askinteger (<expr>, integer)'.

     `askinteger (<expr>, even)' and `askinteger (<expr>, odd)'
     likewise attempt to determine if <expr> is an even integer or odd
     integer, respectively.

(%o1)                                true
@end example

To use a result in later calculations, you can assign it to a variable or
refer to it by its automatically supplied label.  In addition, @mref{%}@w{}
refers to the most recent calculated result:

@example
(%i1) u: expand ((x + y)^6);
       6        5       2  4       3  3       4  2      5      6
(%o1) y  + 6 x y  + 15 x  y  + 20 x  y  + 15 x  y  + 6 x  y + x
(%i2) diff (u, x);
         5         4       2  3       3  2       4        5
(%o2) 6 y  + 30 x y  + 60 x  y  + 60 x  y  + 30 x  y + 6 x
(%i3) factor (%o2);
                                    5
(%o3)                      6 (y + x)
@end example

Maxima knows about complex numbers and numerical constants:

@example
(%i1) cos(%pi);
(%o1)                          - 1
(%i2) exp(%i*%pi);
(%o2)                          - 1
@end example

Maxima can do differential and integral calculus:

@example
(%i1) u: expand ((x + y)^6);
       6        5       2  4       3  3       4  2      5      6
(%o1) y  + 6 x y  + 15 x  y  + 20 x  y  + 15 x  y  + 6 x  y + x
(%i2) diff (%, x);
         5         4       2  3       3  2       4        5
(%o2) 6 y  + 30 x y  + 60 x  y  + 60 x  y  + 30 x  y + 6 x
(%i3) integrate (1/(1 + x^3), x);
                                  2 x - 1
                2            atan(-------)
           log(x  - x + 1)        sqrt(3)    log(x + 1)
(%o3)    - --------------- + ------------- + ----------
                  6             sqrt(3)          3
@end example

Maxima can solve linear systems and cubic equations:

@example
(%i1) linsolve ([3*x + 4*y = 7, 2*x + a*y = 13], [x, y]);
                        7 a - 52        25
(%o1)              [x = --------, y = -------]
                        3 a - 8       3 a - 8
(%i2) solve (x^3 - 3*x^2 + 5*x = 15, x);
(%o2)       [x = - sqrt(5) %i, x = sqrt(5) %i, x = 3]
@end example

Maxima can solve nonlinear sets of equations.  Note that if you don't
want a result printed, you can finish your command with @kbd{$} instead
of @kbd{;}.

@example
(%i1) eq_1: x^2 + 3*x*y + y^2 = 0$
(%i2) eq_2: 3*x + y = 1$
(%i3) solve ([eq_1, eq_2]);
              3 sqrt(5) + 7      sqrt(5) + 3
(%o3) [[y = - -------------, x = -----------], 
                    2                 2

                               3 sqrt(5) - 7        sqrt(5) - 3
                          [y = -------------, x = - -----------]]
                                     2                   2
@end example

Maxima can generate plots of one or more functions:

@example
(%i1) plot2d (sin(x)/x, [x, -20, 20])$
@end example
@ifnotinfo
@image{figures/introduction1, 10cm}
@end ifnotinfo
@example
(%i2) plot2d ([atan(x), erf(x), tanh(x)], [x, -5, 5], [y, -1.5, 2])$
@end example
@ifnotinfo
@image{figures/introduction2, 10cm}
@end ifnotinfo
@example
@group
(%i3) plot3d (sin(sqrt(x^2 + y^2))/sqrt(x^2 + y^2), 
         [x, -12, 12], [y, -12, 12])$
@end group
@end example
@ifnotinfo
@image{figures/introduction3, 12cm}
@end ifnotinfo 

@c FOLLOWING TEXT DESCRIBES THE TCL/TK PLOT WINDOW WHICH IS NO LONGER THE DEFAULT
@c Moving the cursor to the top left corner of the plot window will pop up
@c a menu that will, among other things, let you generate a PostScript file
@c of the plot.  (By default, the file is placed in your home directory.)
@c You can rotate a 3D plot.

@opencatbox
@category{Help}
@closecatbox

