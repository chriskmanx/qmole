/* test for multi font labels */

#include <Xm/LabelGP.h>
#include <Xm/BulletinB.h>

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreate("Here is a ", "MY_FONT1");
  XmString xmstr2 = XmStringCreate("different font", "MY_FONT");

  XmString xmstr = XmStringConcat(xmstr1, xmstr2);

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "LabelG", NULL, 0, &argc, argv, FallBack, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  fontlist = XmFontListAppendEntry(fontlist,
			   XmFontListEntryCreate("MY_FONT1",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1")));

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("One", xmLabelGadgetClass, two, XmNfontList, fontlist, 
				XmNlabelString, xmstr, NULL);

  XtRealizeWidget(toplevel);
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  209,   43, 0,0,0, /* Two */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  188,   22, 0,0,0, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  exit(0);
}
