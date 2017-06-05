/* mainw.c -- demonstrate a simple MainWindow by showing
 * how it can manage a label.
 */
#include <Xm/ScrollBar.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>


int
main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, mw, hello;
    XtAppContext app;
    XmString label;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Mainw7", NULL, 0, 
        &argc, argv, NULL, NULL);

    /* Create a 500x300 scrolled window.  This value is arbitrary,
     * but happens to look good initially.  It is resizable by the user.
     */
    mw = XtVaCreateManagedWidget ("mainw",
          xmMainWindowWidgetClass, toplevel, 
      /*  xmScrolledWindowWidgetClass, toplevel, */
        NULL);

    /*    hs = XtVaCreateManagedWidget ("hs",
        xmScrollBarWidgetClass, mw,
        NULL);

    vs = XtVaCreateManagedWidget ("vs",
        xmScrollBarWidgetClass, mw,
        XmNprocessingDirection,XmMAX_ON_TOP,
        NULL); */

    label = XmStringCreateLocalized("Hello World!");

    hello = XtVaCreateManagedWidget(
	   "hellohellohello",            /* arbitrary widget name */
	   xmLabelWidgetClass,    /* widget class from Label.h */
	   mw,            /* parent widget */
	   NULL);              /* terminate varargs list */

    /*   XmMainWindowSetAreas(mw,NULL,NULL,NULL,NULL,hello); */
    XtRealizeWidget (toplevel);


{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,   94,   17, 0,0,0, /* mainw */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   94,   17, 0,0,0, /* hello */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

    exit(0);
}
