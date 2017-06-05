/* A very simple test - used to test different settings of e.g alignment,
 * shadowThickness and highlightThickness
 */

#include <Xm/LabelGP.h>
#include <Xm/BulletinB.h>

static String fallback[] = {
    "*lb.labelString: OneLabelWidget",
    NULL
};

int
main(int argc, char **argv)
{
    Widget toplevel, one,two;
    XtAppContext app;
    XmFontList fontlist;
    
    XtSetLanguageProc(NULL, NULL, NULL);
    
    toplevel = XtVaAppInitialize(&app, "Test5", NULL, 0, &argc, argv, fallback, NULL);
    
  one = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

    two = XtVaCreateManagedWidget("lb", xmLabelGadgetClass, one, NULL);
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
