/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vasimple/test3.c,v 1.4 2002/03/22 00:00:07 amai Exp $ */
#include <stdlib.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>


void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget form, rc1, rc2;
  XtAppContext app;
  XmString s1, s2;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  form = XmCreateForm(toplevel, "form", NULL, 0);
  XtManageChild(form);

  s1 = XmStringCreateSimple("check1");
  s2 = XmStringCreateSimple("check2");
  rc1 = XmVaCreateSimpleCheckBox(form, "checkBox1", cb,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmVaCHECKBUTTON, s1, 0, NULL, NULL,
				    XmVaCHECKBUTTON, s2, 0, NULL, NULL,
				    XmNleftAttachment, XmATTACH_FORM,
				    NULL);

  rc2 = XmVaCreateSimpleCheckBox(form, "checkBox2", cb,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmVaCHECKBUTTON, s1, 0, NULL, NULL,
				    XmVaCHECKBUTTON, s2, 0, NULL, NULL,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, rc1,
				    NULL);
  XtManageChild(rc1);
  XtManageChild(rc2);

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  134,   60, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   67,   60, 0,0,0, /* checkBox1 */
   CWWidth | CWHeight | CWX | CWY,    3,    4,   61,   25, 0,0,0, /* button_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,   61,   25, 0,0,0, /* button_1 */
   CWWidth | CWHeight | CWX | CWY,   67,    0,   67,   60, 0,0,0, /* checkBox2 */
   CWWidth | CWHeight | CWX | CWY,    3,    4,   61,   25, 0,0,0, /* button_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,   61,   25, 0,0,0, /* button_1 */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
