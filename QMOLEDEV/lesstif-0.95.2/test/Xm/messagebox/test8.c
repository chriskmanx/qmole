/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test8.c,v 1.7 2001/06/18 08:50:43 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/CascadeB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>

#include "../../common/Test.h"

#define LOCAL_CLIP_FRONT 0
#define LOCAL_CLIP_REAR 1
#define GLOBAL_CLIP_FRONT 2
#define GLOBAL_CLIP_REAR 3

void GXMakeClipBox(void);

Widget GXTopWidget, GXClipBox;

int main(int argc, char **argv)
{
  XtAppContext theApp;

  GXTopWidget= XtVaAppInitialize(&theApp, "test1", NULL, 0, &argc, argv,
				 NULL, NULL); 
  XtManageChild(XmCreateLabel(GXTopWidget, "Label", NULL, 0));
  GXMakeClipBox();
  XtRealizeWidget(GXTopWidget);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,  392,  328,  157, 0,0,0, /* GXClipBox */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  328,   31, 0,0,0, /* Menu Bar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   52,   21, 0,0,0, /* Button */
   CWWidth | CWHeight | CWX | CWY,   11,   42,  306,  104, 0,0,0, /* ClipBoxForm */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   21,  104, 0,0,0, /* FrontGlobalClipScale */
   CWWidth | CWHeight | CWX | CWY,    2,    0,   19,  104, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,   24,    0,   19,  104, 0,0,0, /* FrontLocalClipScale */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   19,  104, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,  263,    0,   22,  104, 0,0,0, /* RearLocalClipScale */
   CWWidth | CWHeight | CWX | CWY,    3,    0,   19,  104, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,  275,    0,   31,  104, 0,0,0, /* RearGlobalClipScale */
   CWWidth | CWHeight | CWX | CWY,   12,    0,   19,  104, 0,0,0, /* Scrollbar */
};

/* toplevel should be replaced with to correct applicationShell */
PrintDetails(GXClipBox, Expected);
}
  LessTifTestMainLoop(GXClipBox);

  exit(0);
}

static void scaleCB(Widget w, int client, XtPointer call_data)
/*-
 *  callback for clipping scales
-*/
{
    float depth;

    XmScaleCallbackStruct *call = (XmScaleCallbackStruct *) call_data;
    int value = call->value;
    printf("Got client: %d value %d\n",client,value);

}


Pixel
GXGetColor(char *colorstr)
/*-
  return pixel for a given color
-*/
{
    XrmValue from, to;

    from.size = strlen(colorstr) + 1;
    if (from.size < sizeof(String))
	from.size = sizeof(String);
    from.addr = colorstr;
    to.addr = NULL;
    XtConvert(GXTopWidget, XtRString, &from, XtRPixel, &to);

    if (to.addr != NULL)
	return ((Pixel) * ((Pixel *) to.addr));
    else
	return ((XtArgVal) NULL);

}

void GXSetTroughColor(Widget w, Pixel selectColor)
/*-
  provide an API for setting the trough color of an XmScale Widget
-*/
{
    WidgetList *kids;
    int nkids;
    Arg argList[1], tmpargs[2];
    int i, s, t;

    i = 0;
    XtSetArg(argList[i], XmNtroughColor, selectColor);
    i++;
    /* Unfortunately, scale does not have a direct way to get its scrollbar
     * widget, so use Composite resources */
    s = 0;
    XtSetArg(tmpargs[s], XmNnumChildren, &nkids);
    s++;
    XtSetArg(tmpargs[s], XmNchildren, &kids);
    s++;
    XtGetValues(w, tmpargs, s);
    for (t = 0; t < nkids; t++)
	if (XmIsScrollBar((Widget) kids[t]))
	    XtSetValues((Widget) kids[t], argList, i);
}

void GXMakeClipBox(void)
/*-
  make a dialog box for clipping control
-*/
{
    Widget form, scale, menu;
    Arg args[10];
    int n;

    XmString xmstr;

    GXClipBox = XmCreateTemplateDialog(GXTopWidget, "GXClipBox", NULL, 0);
    xmstr = XmStringCreateSimple("Clip Box--Top View");
    XtVaSetValues(GXClipBox,
		  XmNdefaultPosition, FALSE,
		  XmNx, 50,
		  XmNy, 370,
		  XmNtransient, FALSE,
		  XmNdialogTitle, xmstr,
		  XmNrubberPositioning, TRUE,
		  NULL);
    XtUnmanageChild(XmMessageBoxGetChild(GXClipBox, XmDIALOG_SEPARATOR));
    XmStringFree(xmstr);
    XtManageChild(menu=XmCreateMenuBar(GXClipBox, "Menu Bar",NULL,0));
    XtManageChild(XmCreateCascadeButton(menu,"Button",NULL,0));
    form = (Widget) XmCreateForm(GXClipBox, "ClipBoxForm", NULL, 0);
    XtManageChild(form);

    /* front global scale */
    scale = (Widget) XmCreateScale(form, "FrontGlobalClipScale", NULL, 0);
    XtVaSetValues(scale,
		  XmNdecimalPoints, 0,
		  XmNmaximum, 100,
		  XmNminimum, 0,
		  XmNshowValue, FALSE,
		  XmNvalue, 0,
		  XmNprocessingDirection, XmMAX_ON_TOP,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, 0,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, 7,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    GXSetTroughColor(scale, GXGetColor("white"));

    XtManageChild(scale);
    XtAddCallback(scale, XmNvalueChangedCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) GLOBAL_CLIP_FRONT);
    XtAddCallback(scale, XmNdragCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) GLOBAL_CLIP_FRONT);

    /* front local scale */
    scale = (Widget) XmCreateScale(form, "FrontLocalClipScale", NULL, 0);
    XtVaSetValues(scale,
		  XmNdecimalPoints, 0,
		  XmNmaximum, 100,
		  XmNminimum, 0,
		  XmNshowValue, FALSE,
		  XmNvalue, 0,
		  XmNprocessingDirection, XmMAX_ON_TOP,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, 8,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, 14,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    GXSetTroughColor(scale, GXGetColor("red"));

    XtManageChild(scale);
    XtAddCallback(scale, XmNvalueChangedCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) LOCAL_CLIP_FRONT);
    XtAddCallback(scale, XmNdragCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) LOCAL_CLIP_FRONT);

#ifdef USEGL
    n = 0;
    XtSetArg(args[n], GLwNdoublebuffer, TRUE);
    n++;
    XtSetArg(args[n], GLwNrgba, TRUE);
    n++;
    XtSetArg(args[n], GLwNallocateBackground, TRUE);
    n++;
    ClipBoxGL = GLwCreateMDrawingArea(form, "GLWidget", args, n);
    XtVaSetValues(ClipBoxGL,
		  XmNheight, 200,
		  XmNwidth, 200,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, 15,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, 85,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);

    XtManageChild(ClipBoxGL);
    XtAddCallback(ClipBoxGL, GLwNexposeCallback, (XtPointer) ClipRedrawCB, 0);
    XtAddCallback(ClipBoxGL, GLwNresizeCallback, (XtPointer) ClipResizeCB, 0);
    XtAddCallback(ClipBoxGL, GLwNginitCallback, (XtPointer) ClipInitCB, 0);
#endif

    /* rear scale */
    scale = (Widget) XmCreateScale(form, "RearLocalClipScale", NULL, 0);
    XtVaSetValues(scale,
		  XmNdecimalPoints, 0,
		  XmNmaximum, 100,
		  XmNminimum, 0,
		  XmNshowValue, FALSE,
		  XmNvalue, 100,
		  XmNprocessingDirection, XmMAX_ON_TOP,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, 86,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, 93,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    GXSetTroughColor(scale, GXGetColor("green"));

    XtManageChild(scale);
    XtAddCallback(scale, XmNvalueChangedCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) LOCAL_CLIP_REAR);
    XtAddCallback(scale, XmNdragCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) LOCAL_CLIP_REAR);

    /* rear scale */
    scale = (Widget) XmCreateScale(form, "RearGlobalClipScale", NULL, 0);
    XtVaSetValues(scale,
		  XmNdecimalPoints, 0,
		  XmNmaximum, 100,
		  XmNminimum, 0,
		  XmNshowValue, FALSE,
		  XmNvalue, 100,
		  XmNprocessingDirection, XmMAX_ON_TOP,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, 90,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, 100,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    GXSetTroughColor(scale, GXGetColor("blue"));
    XtManageChild(scale);
    XtAddCallback(scale, XmNvalueChangedCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) GLOBAL_CLIP_REAR);
    XtAddCallback(scale, XmNdragCallback, (XtCallbackProc) scaleCB,
		  (XtPointer) GLOBAL_CLIP_REAR);

    /* install_icon(GXClipBox); */
    XtManageChild(GXClipBox);
    /* GXIgnoreExit(GXClipBox);*/
}
