/* 
$Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test11.c,v 1.3 2002/04/01 13:36:36 amai Exp $

Summary: pasting in XmText truncates; pasting in XmTextField crashes

It's possible that there's something screwed up about my machine,
since I see this problem with OSF Motif as well as Lesstif --
but the Lesstif behavior is both different and worse, which is
probably not a good thing in any event.

...........................

Date: Wed, 17 Feb 1999 16:47:50 -0800
From: Jamie Zawinski <jwz@mozilla.org>
Newsgroups: comp.windows.x.motif,comp.os.linux.x
Followup-To: comp.windows.x.motif
Subject: inconsistent XmText and XmTextField paste behavior
Message-ID: <36CB6336.6E722CC@jwz.org>

I'm running OSF Motif 2.1.0 on Linux 2.0.36 with X11R6 (Red Hat 5.2,
but not the version of Motif that Red Hat sells.)

I find that I cannot paste into XmTextField widgets if the string I
am trying to paste contains high-bit characters (like the copyright
symbol, for example.)  If the characters are plain ASCII, it works
fine.  If not, then either: only the part of the string before the
first high-bit character is pasted; or I get a SEGV in free().

I've narrowed it down somewhat: this is a problem that XmTextField
has but that XmText does not!

Here's a simple test program.  This creates a window with two
single-line text areas in it, one of which is XmText and the other of
which is XmTextField.  If I select some text containing high-bit
characters in some other window, I can paste it into the top text field
but not into the bottom text field.

Does this happen for you, or is my system broken in some special way?

As far as I can tell, this problem only exhibits itself with this
version of Linux.  I was using this same version of Motif on older 
Linux systems without losing in this way (that was back in the 1.2.13
days.)

Interestingly, when I try this same test program with Lesstif 0.87.1,
pasting into the top text field stops pasting at the first high-bit
character; and pasting into the bottom text field dumps core in free().

Any suggestions appreciated...

(Upgrading to Motif 2.0 is not an option, unfortunately.)
*/

#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

int main (int argc, char **argv)
{
  XtAppContext app;
  Arg av[100];
  int ac;
  Widget toplevel, form, text, textf;
  toplevel = XtAppInitialize (&app, "xmtest", 0, 0,
                              &argc, argv, 0, 0, 0);
  ac = 0;
  form = XmCreateForm (toplevel, "form", av, ac);
  ac = 0;
  text = XmCreateText (form, "text", av, ac);
  ac = 0;
  XtSetArg(av[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(av[ac], XmNtopWidget, text); ac++;
  textf = XmCreateTextField (form, "text", av, ac);
  XtManageChild (text);
  XtManageChild (textf);
  XtManageChild (form);
  XtRealizeWidget (toplevel);
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */
  exit (0);
}
