
#include <Xm/LabelGP.h>
#include <Xm/BulletinB.h>

static String fallback[] = {
	"*XmBulletinBoard.background:	green",
	"*XmLabelGadget.background:	red",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;
  XmFontList fontlist;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, fallback, NULL);

  fontlist = XmFontListAppendEntry(NULL,
				   XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
							 XmFONT_IS_FONT,
							 XLoadQueryFont(XtDisplay(toplevel), 
									"-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("OneLabelWidget", xmLabelGadgetClass, two, NULL); /*XmNfontList, fontlist, NULL);*/

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	109,	38,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	10,	10,	88,	17,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
