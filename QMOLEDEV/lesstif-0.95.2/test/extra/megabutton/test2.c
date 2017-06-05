#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/ArrowB.h>
#include <X11/Xmu/Editres.h>
#include <X11/Shell.h>
#include <X11/Core.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeBG.h>

#include "MegaB.h"

#include <stdio.h>

#define APPNAME "MegaTest"
#define APPCLASS "MegaTest"

#define ICOUNT 200

static void dyn_menuCB(arrow, data, cbs)
Widget arrow;
XtPointer data;
XmArrowButtonCallbackStruct *cbs;
{
  int i;
  char buf[100];
  Widget menu, megatb;
#define NITEMS 40
  XmString xmstrings[NITEMS];
  Arg args[10];


printf("dyn_menuCB\n");
  
  for (i = 0; i < NITEMS; i++)
    {
      sprintf(buf, "item %d", i);
      xmstrings[i]  = XmStringCreateSimple(buf);
    }


  /* Create popup menu */
  menu = XmCreatePopupMenu(arrow, "popup", NULL, 0);


  /* Create MegaButton widget */
  i = 0;
  XtSetArg(args[i], XmNitems, xmstrings); i++;
  XtSetArg(args[i], XmNitemCount, NITEMS); i++;
  megatb = XtCreateManagedWidget("_megaButton", 
				 xmMegaButtonWidgetClass, menu, args, i);


  /* Free the XmStrings */
  for (i = 0; i < NITEMS; i++)
    XmStringFree(xmstrings[i]);
    
  
  XtAddCallback(menu, XmNunmapCallback,
		(XtCallbackProc) XtDestroyWidget, NULL);


  /* Display the popup menu */
  XmMenuPosition(menu, (XButtonPressedEvent *)cbs->event);
  XtManageChild(menu);
}

int
main(argc, argv)
     int argc; char **argv;
{
  Widget toplevel, arrow;
  XtAppContext app;
  Display *theDisplay;

  XtToolkitInitialize();
  app = XtCreateApplicationContext();
	
  theDisplay = XtOpenDisplay (app, NULL, APPNAME, APPCLASS, 
			      NULL, 0, &argc, argv);

  if (!theDisplay)
    {
      printf("%s: can't open display, exiting...", APPNAME);
      exit (0);
    }

  toplevel = XtAppCreateShell (APPNAME, APPCLASS,
			       applicationShellWidgetClass, theDisplay, NULL, 0);

  arrow = XtVaCreateWidget("down_arrow",
			   xmArrowButtonWidgetClass, toplevel,
			   XmNarrowDirection, XmARROW_DOWN,
			   NULL);

  XtManageChild(arrow);
  XtAddCallback(arrow, XmNactivateCallback,
		(XtCallbackProc) dyn_menuCB, NULL);




  /* XtAddEventHandler(toplevel, 0, True, _XEditResCheckMessages, NULL); */
  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   23,   23, 0,0,0, /* down_arrow */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);
  exit(0);
}


