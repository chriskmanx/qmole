/* test for showAsDefault resource */

#include <Xm/Xm.h>
#include <Xm/PushB.h>

static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nDefault\nButton", "MY_FONT");

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, 
				XmNfontList, fontlist, 
				XmNlabelString, xmstr1, 
				XmNshowAsDefault, 5,
				NULL);

  XtRealizeWidget(toplevel);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  267,  282,  100,  130, 0,0,0, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
