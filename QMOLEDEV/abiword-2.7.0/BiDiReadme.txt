This file contains instructions on building, using and developing
AbiWord with bidi-rectional support and is maintained by Tomas Frydrych
<tomas@frydrych.uklinux.net> to whom comments, suggestions, and bug 
fixes of the BiDi features should be directed.

Enabling BiDi during build
--------------------------
Bidi is now enabled by default.

Using BiDi
----------
The BiDi algorithm is Unicode-based, so that text is correctly
arranged as it is typed in. It uses the FriBidi library to calculate
the layout, which in turn follows the Unicode bidirectional algorithm.
When the user requires the text to be handled differently than the
Unicode algorithm prescribes, he or she can force any segment of the 
text into a particular direction using two toolbar buttons (the three
bidi buttons are now located on the Extra toolbar).

The dominant direction of the paragraph can be selected either
using a third toolbar button, or from the Format->Paragraph
dialogue.

The 'Other' tab of the Preferences dialogue allows to change 
the default direction; however, this change will only take place 
when a new document is created or AbiWord restarted.
The other two bidi checkboxes in the preferences allow to turn
the shaping engine on and off, and to specify whether when
the shaping engine is used, it should only change the visual
output, or the actual document.

It should be noted that while the shaping engine supports Arabic,
in the Unix version this will only work under utf-8 locale with
Unicode fonts (see UnixLocale.txt and UnixFonts.txt); those wishing 
to use Arabic with AW under an 8-bit locale will have to switch the 
shaping engine off (as mentioned in the previous paragraph) and put 
up with the basic letter forms (the shaping engine is very basic, and
will one day be replaced by something better, probably Pango).


TECHNICAL NOTES
===============

Brief Introduction to the BiDi Problem
--------------------------------------

Let's say that the user inputs a string 'abcd XYZ UVW klm' where
small letters represent text that is from left to right (ltr) while
capitals represent text that is right to left (rtl), and that the
overall direction of the document is ltr. The visual representation
of the string on the screen will need to be

  'abcd WVU ZYX klm'

that is, the visual order is different than the order in which the
text was input, the logical order. The algorithm that is used to
work out the visual ordering is straight forward, but cannot be
applied to an arbitrary segment of the text. Rather it has to take into
account always an entire line, and this causes problem for software
that does not draw text in whole lines, such as AbiWord.

A chunk of text which has uniform attributes, such as font or colour,
is in AbiWord terminology called run. In BiDi mode, text in a run
has to be also of consistent direction, with three possible options:
ltr, rtl, and neutral. The last of these applies basically to whitespace,
which derives its actual direction from the context. The direction of
the run is worked out automatically from the Unicode value of a
character, but can be overridden by the user.

The heart of BiDi in AbiWord is built into fp_Line class (guess why?).
Each run of text stores its direction, and this is used by the function
fp_Line::_createMapOfRuns() to calculate the order of the runs belonging
to the particular line. The order is stored in an array that is used
to translate logical coordinates to visual ones and vice versa. Any
operations on the actual text buffer have to use logical order, while
any operations on the screen have to use visual order.

Each paragraph has also a new property, in AW called dom-dir (dominant 
direction). This can be either ltr or rtl. The string of text we used 
in the illustration earlier ('abcd XYZ UVW klm') will look differently,
if the overall direction is ltr, say an English document with a Hebrew 
quote in it ('abcd WVU ZYX klm') or rtl, such as Hebrew document with 
an English quote ('klm WVU ZYX abcd'). This paragraph property is in AW
always explicitly specified by the user, we do not use any heuristic 
algorithm that would try to guess.

This really is all there is to BiDi, just sometimes it makes life more
complicated than this introduction might lead you to believe. For instance
the insertion point can appear on two places on the screen at the same time,
because the visual position of the next character to be input is the
function of the direction of this character, which we do not know, since
it has not been typed in yet :-). Or, things are more complicated if you
want to display say rtl quote in an ltr paragraph, but the quote itself
contains another ltr quote. Fortunately, the bulk of this work is done for
us by the well tested FriBidi library.


Developing BiDi
---------------
<This section was mostly obsolete, now that all builds are bidi builds>

Any new code should be 'littered' with asserts, because these
are great help in tracing bugs. In particular, put an assert in
before dereferencing any pointers; virtually all SIGSEGVs I have
experienced with BiDi in AW had to do with dereferencing 0 (we 
often need to refer to things earlier than the non-BiDi version,
so sometimes these things do not exist yet).
