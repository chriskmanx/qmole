/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test21.c,v 1.3 2001/06/15 09:30:51 amai Exp $ */
/* "Hello world" application used for size tests ... */
/* Compile with -DNOTEST to get the "original" code */


#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/PushBG.h>

#define APP_CLASS "Foo"

Widget CreateMainWindowWithMenuBar(Widget toplevel);

static Widget toplevel ;  /* used in OKCB for set its size */


int main(int argc, char *argv[])
{
     XtAppContext app_context;
     int i ;
     Widget mainw, nb ;


     XtSetLanguageProc(NULL, NULL, NULL);
     toplevel = XtVaAppInitialize(&app_context, APP_CLASS, NULL, 0,
                                  &argc, argv, NULL, NULL);

     /*** create utility widgets */
     mainw = CreateMainWindowWithMenuBar(toplevel);
     XtManageChild(mainw);

     XtRealizeWidget(toplevel);

#ifdef NOTEST
     XtAppMainLoop(app_context);
#else
     LessTifTestMainLoop(toplevel);
#endif

     return (0);
}


static void
QuitCB (Widget w, XtPointer client_data, XtPointer call_data)
{
     exit (0);
}


Widget
CreateMainWindowWithMenuBar(Widget toplevel)
{
     Widget      main_window, pb;

     Arg         args[20];
     Cardinal    n;



     /*  Create MainWindow.
      */
     n = 0;
     main_window = XmCreateMainWindow (toplevel, "main_window", args, n);
     XtManageChild (main_window);


     /*  Create MenuBar in MainWindow.
      */
     n = 0;
     pb = XmCreatePushButtonGadget (main_window, "Hello Motif", args, n);
     XtManageChild (pb);
     XtAddCallback (pb, XmNactivateCallback, QuitCB, NULL);

     return (main_window);
}
