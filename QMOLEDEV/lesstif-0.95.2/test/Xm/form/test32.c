/**
 *
 * test32.c
 *
 **/
#undef FIX

#include <stdio.h>
#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/FormP.h>
#include <Xm/Separator.h>


char *fallback[] = {
	"*borderWidth: 1",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  Widget form;
  Widget label1, label2,label3,label4;
  Widget rc1, rc2, rc3, rc4, rc5, rc6, rc7;
  Widget sep1, sep2, sep3;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "test32", NULL, 0, &argc, argv, fallback, NULL);

  form = XmCreateForm(toplevel, "form", NULL, 0);

  label1 = XmCreateLabel(form, "label1", NULL, 0);
  XtVaSetValues(label1,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNtopOffset, 5,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNleftOffset, 5,
  	XmNwidth, 61,
  	XmNheight, 63,
  	NULL);
  XtManageChild(label1);

  rc1 = XmCreateLabel(form, "rc1", NULL, 0);
  XtVaSetValues(rc1,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, label1,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftOffset, 123,
#ifdef FIX
  	XmNleftWidget, label1,
#else
  	XmNleftWidget, NULL,
#endif
  	XmNwidth, 47,
  	XmNheight, 66,
  	NULL);
  XtManageChild(rc1);

  rc2 = XmCreateLabel(form, "rc2", NULL, 0);
  XtVaSetValues(rc2,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, rc1,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, rc1,
  	XmNleftOffset, 73,
  	XmNwidth, 50,
  	XmNheight, 66,
  	NULL);
  XtManageChild(rc2);

  label2 = XmCreateLabel(form, "label2", NULL, 0);
  XtVaSetValues(label2,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, label1,
  	XmNtopOffset, 5,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNleftOffset, 5,
  	XmNwidth, 68,
  	XmNheight, 63,
  	NULL);
  XtManageChild(label2);

  rc3 = XmCreateLabel(form, "rc3", NULL, 0);
  XtVaSetValues(rc3,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, label2,
  	XmNleftAttachment, XmATTACH_WIDGET,
#ifdef FIX
  	XmNleftWidget, label2,
#else
  	XmNleftWidget, NULL,
#endif
  	XmNleftOffset, 95, /* This kills it!!!! */
  	XmNwidth, 113,
  	XmNheight, 66,
  	NULL);
  XtManageChild(rc3);

  sep1 = XmCreateSeparator(form, "sep1", NULL, 0);
  XtVaSetValues(sep1,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, rc3,
  	XmNleftOffset, 5,
  	XmNorientation, XmVERTICAL,
  	XmNborderWidth, 0,
  	NULL);
  XtManageChild(sep1);

  rc4 = XmCreateLabel(form, "rc4", NULL, 0);
  XtVaSetValues(rc4,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, rc3,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, sep1,
  	XmNleftOffset, 8,
  	XmNwidth, 113,
  	XmNheight, 66,
  	NULL);
  XtManageChild(rc4);

  sep2 = XmCreateSeparator(form, "sep2", NULL, 0);
  XtVaSetValues(sep2,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, rc4,
  	XmNleftOffset, 5,
  	XmNorientation, XmVERTICAL,
  	XmNborderWidth, 0,
  	NULL);
  XtManageChild(sep2);

  label3 = XmCreateLabel(form, "label3", NULL, 0);
  XtVaSetValues(label3,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, label2,
  	XmNtopOffset, 5,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNbottomOffset, 5,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNleftOffset, 5,
  	XmNwidth, 61,
  	XmNheight, 63,
  	NULL);
  XtManageChild(label3);

  rc5 = XmCreateLabel(form, "rc5", NULL, 0);
  XtVaSetValues(rc5,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, label3,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftOffset, 123,
#ifdef FIX
  	XmNleftWidget, label3,
#else
  	XmNleftWidget, NULL,
#endif
  	XmNwidth, 70,
  	XmNheight, 66,
  	NULL);
  XtManageChild(rc5);

  rc6 = XmCreateLabel(form, "rc6", NULL, 0);
  XtVaSetValues(rc6,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, rc5,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, rc5,
  	XmNleftOffset, 73,
  	XmNwidth, 73,
  	XmNheight, 66,
  	NULL);
  XtManageChild(rc6);

  rc7 = XmCreateLabel(form, "rc7", NULL, 0);
  XtVaSetValues(rc7,
  	XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNtopWidget, rc6,
  	XmNtopOffset, 2,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, sep2,
  	XmNleftOffset, 8,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNrightOffset, 5,
  	XmNwidth, 107 /*73*/,
  	XmNheight, 37 /*66*/,
  	NULL);
  XtManageChild(rc7);

  label4 = XmCreateLabel(form, "XFEED P", NULL, 0);
  XtVaSetValues(label4,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, rc7,
  	XmNbottomOffset, 5,
  	XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNleftWidget, rc7,
  	XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNrightWidget, rc7,
  	XmNwidth, 61,
  	XmNheight, 16,
  	NULL);
  XtManageChild(label4);

  XtManageChild(form);
  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  469,  215, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   61,   63, 0,0,0, /* label1 */
   CWWidth | CWHeight | CWX | CWY,  123,    5,   47,   66, 0,0,0, /* rc1 */
   CWWidth | CWHeight | CWX | CWY,  245,    5,   50,   66, 0,0,0, /* rc2 */
   CWWidth | CWHeight | CWX | CWY,    5,   75,   68,   63, 0,0,0, /* label2 */
   CWWidth | CWHeight | CWX | CWY,   95,   75,  113,   66, 0,0,0, /* rc3 */
   CWWidth | CWHeight | CWX | CWY,  215,    0,    2,  215, 0,0,0, /* sep1 */
   CWWidth | CWHeight | CWX | CWY,  225,   75,  113,   66, 0,0,0, /* rc4 */
   CWWidth | CWHeight | CWX | CWY,  345,    0,    2,  215, 0,0,0, /* sep2 */
   CWWidth | CWHeight | CWX | CWY,    5,  145,   61,   63, 0,0,0, /* label3 */
   CWWidth | CWHeight | CWX | CWY,  123,  145,   70,   66, 0,0,0, /* rc5 */
   CWWidth | CWHeight | CWX | CWY,  268,  145,   73,   66, 0,0,0, /* rc6 */
   CWWidth | CWHeight | CWX | CWY,  355,  147,  107,   37, 0,0,0, /* rc7 */
   CWWidth | CWHeight | CWX | CWY,  355,  124,  107,   16, 0,0,0, /* XFEED P */ 
    };
    PrintDetails(toplevel,Expected);
};
      LessTifTestMainLoop(toplevel);

  exit(0);
}
