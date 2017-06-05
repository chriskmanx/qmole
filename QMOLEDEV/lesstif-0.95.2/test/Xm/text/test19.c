/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test19.c,v 1.4 2002/07/01 17:28:12 dannybackx Exp $

I am using lesstif-0.92.32-6 on a RedHat 7.2 machine and I am having a
display problem with scrolled text widgets. This is going to sound weird
but I can make this happen every time by using these key strokes:

    Hit <enter> 3 times
    type at least one character
    <backspace> to the previous line
    start typing a string of text

At this point I start seeing two lines of typing, the one I am actually
typing on and one two lines down mirroring the current one. The second
line is not editable and does not get saved to the db when the data from
the field is read but I will get no end of complaints from my users about
this. Please help.




Enter
	a
	b
	c
	111111
then place the cursor after the b and hit backspace 4 times.
This displays 
	(empty)
	c
	11
	1111
instead of
	(empty)
	c
	111111
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/XmAll.h>
 
int
main (int argc, char **argv)
{
	Arg	al[20];
	int	n;
	Widget	Top_Level, mb, Main_ul, Editor;

	Top_Level = XtInitialize (argv[0], "test", NULL, 0, &argc, argv);

	n = 0;
	XtSetArg(al[n], XmNheight, 200); n++;
	XtSetArg(al[n], XmNwidth, 200); n++;
	Main_ul = XmCreateMainWindow (Top_Level, "Main_ul", al, n);
	XtManageChild (Main_ul);

	n = 0;
	mb = XmCreateMenuBar(Main_ul, "mb", NULL, 0);
	XtManageChild(mb);
	XtVaCreateManagedWidget("Menu", xmCascadeButtonWidgetClass, mb, NULL);

	n = 0;
	XtSetArg(al[n], XmNeditMode, XmMULTI_LINE_EDIT);  n++;
	Editor = XmCreateScrolledText (Main_ul, "Editor", al, n);
	XtManageChild (Editor);

	XmMainWindowSetAreas (Main_ul, mb, NULL, NULL, NULL, XtParent(Editor));
	XtRealizeWidget (Top_Level);

	LessTifTestMainLoop(Top_Level);

	exit(0);
}
