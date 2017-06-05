/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test6.c,v 1.7 2001/06/18 08:33:07 amai Exp $
  test for multi font labels */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Label.h>
#include <Xm/LabelP.h>


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
  XmString xmstr1 = XmStringCreateLtoR("Here is a\n", "MY_FONT1");
  XmString xmstr2 = XmStringCreate("different font", "MY_FONT");
  XmString xmstr3 = XmStringCreate("accelerator", "MY_FONT");
  XmStringContext context;
  char *text;
  XmStringCharSet tag;
  XmStringDirection dir;
  Boolean sep;

  XmString xmstr = XmStringConcat(xmstr1, xmstr2);

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);

  XmStringInitContext(&context, xmstr);
  while (XmStringGetNextSegment(context, &text, &tag, &dir, &sep)) {
	printf("Text: %s Tag: %s Dir: %d Sep: %d\n", text, tag, dir, sep);
	fflush(stdout);
  }

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  fontlist = XmFontListAppendEntry(fontlist,
			   XmFontListEntryCreate("MY_FONT1",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--30-*-*-*-*-*-iso8859-1")));

  one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, toplevel, XmNfontList, fontlist, 
				XmNlabelString, xmstr,
				XmNacceleratorText, xmstr3, NULL);

  printf("xmstr: %p %p\n", xmstr, ((XmLabelRec*)one)->label._label);
  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	128,	55,	0,0,0,	/* Form */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
