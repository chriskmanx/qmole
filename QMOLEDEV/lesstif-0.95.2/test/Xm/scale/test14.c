/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scale/test14.c,v 1.5 2001/06/18 14:16:07 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


Widget  scale, text;

void Doit(Widget w, XtPointer client, XtPointer call)
{
        char    *s;
        int     i;

        s = NULL;
        XtVaGetValues(text, XmNvalue, &s, NULL);
        if (s == NULL)
                return;

        i = atoi(s);
        if (i < 0 || i > 100)
                return;

        if (client)
                XmScaleSetValue(scale, i);
        else
                XtVaSetValues(scale, XmNvalue, i, NULL);
}


int
main(int argc, char **argv)
{
  Widget toplevel, but1, but2, form;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
                XmNx,   400,
                XmNy,   300,
        NULL);

  scale = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, form,
                XmNorientation,         XmHORIZONTAL,
                XmNscaleWidth,          100,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNtopAttachment,       XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_NONE,
        NULL);

  text = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, form,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           scale,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_NONE,
        NULL);

  but1 = XtVaCreateManagedWidget("XtSetValues", xmPushButtonWidgetClass, form,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           text,
                XmNrightAttachment,     XmATTACH_POSITION,
                XmNrightPosition,       50,
                XmNbottomAttachment,    XmATTACH_FORM,
        NULL);
  XtAddCallback(but1, XmNactivateCallback, Doit, 0);

  but2 = XtVaCreateManagedWidget("XmScaleSetValue", xmPushButtonWidgetClass, form,
                XmNleftAttachment,      XmATTACH_WIDGET,
                XmNleftWidget,          but1,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           text,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_FORM,
        NULL);

  XtAddCallback(but2, XmNactivateCallback, Doit, (XtPointer)1);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  180,   75, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  180,   19, 0,0,0, /* sb */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,   19, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,   19,  180,   31, 0,0,0, /* text */
   CWWidth | CWHeight | CWX | CWY,    0,   50,   90,   25, 0,0,0, /* XtSetValues */
   CWWidth | CWHeight | CWX | CWY,   90,   50,   90,   25, 0,0,0, /* XmScaleSetValue */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
