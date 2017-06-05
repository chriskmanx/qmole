/* $Id: test23.c,v 1.1 2000/12/12 21:14:34 amai Exp $ */
/*
   SF [ Bug #125495 ] programmatic selection fails in List widgets

 */

#include <stdio.h>
#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>

#include <Xm/List.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>

void buttonCB (Widget, XtPointer, XtPointer);
void populateList (Widget);



char *labels[7] =
      {"LessTif", "Rocks", "But", "Has a", "Programmatic",
       "Selection", "Bug"};
XmString xmStrings[7];


void
populateList (Widget list)
{
  int i;

  for (i = 0; i < 7; i++)
    {
      xmStrings[i] = XmStringCreateSimple (labels[i]);
    }
  XtVaSetValues (list,
		 XmNitemCount, 7,
		 XmNitems, xmStrings,
		 NULL);

  XtVaSetValues (list,
		 XmNselectedItemCount, 1,
		 XmNselectedItems, &xmStrings[0],
		 NULL);
}


void
buttonCB (Widget w, XtPointer data, XtPointer cbs)
{
  Widget list = (Widget) data;
  XmStringTable strTab;
  int numSel;
  int i;
  char *text;
  static int count = 0;


  if (count == 7)
    exit (0);

  XtVaGetValues (list,
		 XmNselectedItems, &strTab,
		 XmNselectedItemCount, &numSel,
		 NULL);
  fprintf (stderr, "Before: num selected = %d", numSel);
  XmStringGetLtoR (strTab[0], XmSTRING_DEFAULT_CHARSET, &text);
  fprintf (stderr, ", text=%s", text);

  for (i = 0; i < 7; i++)
    {
      if (XmStringCompare (strTab[0], xmStrings[i]))
	fprintf (stderr, ", item index=%d", i);
    }
  fprintf (stderr, "\n");

  XtVaSetValues (list,
		 XmNselectedItemCount, 1,
		 XmNselectedItems, &xmStrings[count],
		 NULL);
  count++;

  XtVaGetValues (list,
		 XmNselectedItemCount, &numSel,
		 NULL);

  fprintf (stderr, "...after: num selected: %d\n", numSel);
}


int
main (int argc, char *argv[])
{
  Widget tmpDialogW;
  Widget tmpFormW;
  Widget labelTmp;

  XtAppContext appContext;
  Widget rootWidget;
  Widget panedW;
  Widget list;
  Widget button;
  XmString tmpStr;

  rootWidget = XtVaAppInitialize (&appContext, "lesstifBug", NULL, 0,
				  &argc, argv, NULL, NULL);

  panedW = XtVaCreateManagedWidget ("pane",
				    xmPanedWindowWidgetClass, rootWidget,
				    XmNsashWidth, 0,
				    XmNsashHeight, 0,
				    NULL);

  list = XtVaCreateManagedWidget ("list",
				  xmListWidgetClass, panedW,
				  XmNvisibleItemCount, 10,
				  NULL);
  populateList (list);

  tmpStr = XmStringCreateSimple ("select next");
  button = XtVaCreateManagedWidget ("button",
				    xmPushButtonWidgetClass, panedW,
				    XmNlabelString, tmpStr,
				    NULL);
  XmStringFree (tmpStr);
  XtAddCallback (button, XmNactivateCallback, buttonCB, (XtPointer) list);

  XtRealizeWidget (rootWidget);

#if 0
  XtAppMainLoop (appContext);
#else
  LessTifTestMainLoop(rootWidget);
#endif
  exit(0);
}
