@c -----------------------------------------------------------------------------
@page
@node Constants, Lists, Strings, Data Types and Structures
@section Constants
@c -----------------------------------------------------------------------------

@menu
* Functions and Variables for Constants::
@end menu

@c -----------------------------------------------------------------------------
@node Functions and Variables for Constants,  , Constants, Constants
@subsection Functions and Variables for Constants
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{%e}
@defvr {Constant} %e
@ifinfo
@vrindex e
@vrindex Euler's number
@vrindex Base of natural logarithm
@end ifinfo

@code{%e} represents the base of the natural logarithm, also known as Euler's
number.  The numeric value of @code{%e} is the double-precision floating-point
value 2.718281828459045d0.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%i}
@defvr {Constant} %i
@ifinfo
@vrindex i
@vrindex Imaginary unit
@end ifinfo

@code{%i} represents the imaginary unit, @math{sqrt(- 1)}.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{false}
@defvr {Constant} false

@code{false} represents the Boolean constant of the same name.
Maxima implements @code{false} by the value @code{NIL} in Lisp.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%gamma}
@defvr {Constant} %gamma
@ifinfo
@vrindex Euler-Mascheroni constant
@end ifinfo

The Euler-Mascheroni constant, 0.5772156649015329 ....
@c DOUBTLESS THERE IS MORE TO SAY HERE.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{ind}
@defvr {Constant} ind
@ifinfo
@vrindex Indeterminate
@end ifinfo

@code{ind} represents a bounded, indefinite result.

See also @mrefdot{limit}

Example:

@c ===beg===
@c limit (sin(1/x), x, 0);
@c ===end===
@example
(%i1) limit (sin(1/x), x, 0);
(%o1)                          ind
@end example

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{inf}
@defvr {Constant} inf
@ifinfo
@vrindex Real infinity
@end ifinfo

@code{inf} represents real positive infinity.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{infinity}
@defvr {Constant}  infinity
@ifinfo
@vrindex Complex infinity
@end ifinfo

@code{infinity} represents complex infinity.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{minf}
@defvr {Constant} minf
@ifinfo
@vrindex Minus infinity
@vrindex Negative infinity
@end ifinfo

@code{minf} represents real minus (i.e., negative) infinity.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%phi}
@defvr {Constant} %phi
@ifinfo
@vrindex phi
@vrindex Golden mean
@end ifinfo

@code{%phi} represents the so-called @i{golden mean}, @math{(1 + sqrt(5))/2}.
The numeric value of @code{%phi} is the double-precision floating-point value
1.618033988749895d0.

@mref{fibtophi} expresses Fibonacci numbers @code{fib(n)} in terms of
@code{%phi}.

By default, Maxima does not know the algebraic properties of @code{%phi}.
After evaluating @code{tellrat(%phi^2 - %phi - 1)} and @code{algebraic: true},
@mref{ratsimp} can simplify some expressions containing @code{%phi}.

Examples:

@code{fibtophi} expresses Fibonacci numbers @code{fib(n)} in terms of @code{%phi}.

@c ===beg===
@c fibtophi (fib (n));
@c fib (n-1) + fib (n) - fib (n+1);
@c fibtophi (%);
@c ratsimp (%);
@c ===end===
@example
(%i1) fibtophi (fib (n));
                           n             n
                       %phi  - (1 - %phi)
(%o1)                  -------------------
                           2 %phi - 1
(%i2) fib (n-1) + fib (n) - fib (n+1);
(%o2)          - fib(n + 1) + fib(n) + fib(n - 1)
(%i3) fibtophi (%);
            n + 1             n + 1       n             n
        %phi      - (1 - %phi)        %phi  - (1 - %phi)
(%o3) - --------------------------- + -------------------
                2 %phi - 1                2 %phi - 1
                                          n - 1             n - 1
                                      %phi      - (1 - %phi)
                                    + ---------------------------
                                              2 %phi - 1
(%i4) ratsimp (%);
(%o4)                           0
@end example

By default, Maxima does not know the algebraic properties of @code{%phi}.
After evaluating @code{tellrat (%phi^2 - %phi - 1)} and @code{algebraic: true},
@code{ratsimp} can simplify some expressions containing @code{%phi}.

@c ===beg===
@c e : expand ((%phi^2 - %phi - 1) * (A + 1));
@c ratsimp (e);
@c tellrat (%phi^2 - %phi - 1);
@c algebraic : true;
@c ratsimp (e);
@c ===end===
@example
(%i1) e : expand ((%phi^2 - %phi - 1) * (A + 1));
                 2                      2
(%o1)        %phi  A - %phi A - A + %phi  - %phi - 1
(%i2) ratsimp (e);
                  2                     2
(%o2)        (%phi  - %phi - 1) A + %phi  - %phi - 1
(%i3) tellrat (%phi^2 - %phi - 1);
                            2
(%o3)                  [%phi  - %phi - 1]
(%i4) algebraic : true;
(%o4)                         true
(%i5) ratsimp (e);
(%o5)                           0
@end example

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%pi}
@defvr {Constant} %pi
@ifinfo
@vrindex pi
@end ifinfo

@code{%pi} represents the ratio of the perimeter of a circle to its diameter.
The numeric value of @code{%pi} is the double-precision floating-point value
3.141592653589793d0.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{true}
@defvr {Constant} true

@code{true} represents the Boolean constant of the same name.
Maxima implements @code{true} by the value @code{T} in Lisp.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{und}
@defvr {Constant} und
@ifinfo
@vrindex Undefined
@end ifinfo

@code{und} represents an undefined result.

See also @mrefdot{limit}

Example:

@c ===beg===
@c limit (x*sin(x), x, inf);
@c ===end===
@example
(%i1) limit (x*sin(x), x, inf);
(%o1)                          und
@end example

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{zeroa}
@defvr {Constant} zeroa

@code{zeroa} represents an infinitesimal above zero.  @code{zeroa} can be used
in expressions.  @code{limit} simplifies expressions which contain
infinitesimals.

See also @mref{zerob} and @mrefdot{limit}

Example:

@code{limit} simplifies expressions which contain infinitesimals:

@c ===beg===
@c limit(zeroa);
@c limit(zeroa+x);
@c ===end===
@example
(%i1) limit(zeroa);
(%o1)                           0
(%i2) limit(x+zeroa);
(%o2)                           x
@end example

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{zerob}
@defvr {Constant} zerob

@code{zerob} represents an infinitesimal below zero.  @code{zerob} can be used
in expressions.  @code{limit} simplifies expressions which contain
infinitesimals.

See also @mref{zeroa} and @mrefdot{limit}

@opencatbox
@category{Constants}
@closecatbox
@end defvr

