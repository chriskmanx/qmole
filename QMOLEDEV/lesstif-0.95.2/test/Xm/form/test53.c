/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test53.c,v 1.5 2002/05/01 15:39:21 amai Exp $ */

/***********************************************************************/
/* Open Visualization Data Explorer                                    */
/* (C) Copyright IBM Corp. 1989,1999                                   */
/* ALL RIGHTS RESERVED                                                 */
/* This code licensed under the                                        */
/*    "IBM PUBLIC LICENSE - Open Visualization Data Explorer"          */
/***********************************************************************/

#include <stdlib.h>
#include <limits.h>

#include <X11/StringDefs.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>
#include <Xm/Label.h>
#include <Xm/BulletinB.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


typedef struct {
    	Widget cancel;
    	Widget ok;
    	Widget separator1;
    	Widget align_form;
    	Widget llbtn;
    	Widget lrbtn;
    	Widget ctbtn;
    	Widget upperbtn;
    	Widget lowerbtn;
    	Widget leftbtn;
    	Widget rightbtn;
    	Widget ulbtn;
    	Widget urbtn;
    	Widget alignLabel;
    	Widget separator2;
    	Widget hLabel;
    	Widget hspacing;
    	Widget vLabel;
    	Widget vspacing;
    	Widget spaceLabel;
    	Widget separator3;
    	Widget gc_rc;
    	Widget noneTButton;
    	Widget oneDhTButton;
    	Widget oneDvTButton;
    	Widget twoDTButton;
    } GridDialog;

static GridDialog gd;
GridDialog *this = &gd;

static char *FallBack[] = {
		NULL
};

void GridDialog_DimensionCB(Widget    widget,
                             XtPointer clientData,
                             XtPointer callData);

Widget createDialog(Widget parent)
{
    Arg arg[20];
    int n = 0;
    Widget form, label;

    XtSetArg(arg[n], XmNheight,              400);  n++;
    XtSetArg(arg[n], XmNdialogStyle,         XmDIALOG_APPLICATION_MODAL);  n++;

    form = XmCreateForm(parent, "form",NULL, 0);

    this->cancel = XtVaCreateManagedWidget("cancelButton",
	xmPushButtonWidgetClass, form,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNbottomAttachment,  XmATTACH_FORM,
        XmNbottomOffset,      10,
        NULL);

    this->ok = XtVaCreateManagedWidget("okButton",
	xmPushButtonWidgetClass, form,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNbottomAttachment,  XmATTACH_FORM,
        XmNbottomOffset,      10,
        NULL);

    this->separator1 = XtVaCreateManagedWidget("separator1",
	xmSeparatorGadgetClass, form,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNbottomAttachment,  XmATTACH_WIDGET,
        XmNbottomWidget,      this->ok,
        XmNbottomOffset,      10,
        NULL);


    this->align_form = XtVaCreateManagedWidget("alignForm",
	xmFormWidgetClass,	form,
	XmNbottomAttachment,	XmATTACH_WIDGET,
	XmNbottomWidget,	this->separator1,
	XmNbottomOffset,	10,
	XmNrightAttachment,	XmATTACH_FORM,
	XmNleftAttachment,	XmATTACH_FORM,
	XmNleftOffset,		5,
	XmNrightOffset,		5,
	XmNtopAttachment,	XmATTACH_FORM,
	XmNtopOffset,		252,
	NULL);

    this->llbtn = XtVaCreateManagedWidget("llButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_FORM,
        XmNbottomOffset,      10,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
	NULL);

    this->lrbtn = XtVaCreateManagedWidget("lrButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_FORM,
        XmNbottomOffset,      10,
	XmNleftAttachment,    XmATTACH_WIDGET,
	XmNleftWidget,	      this->llbtn,
	XmNleftOffset,	      30,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
	NULL);

    this->ctbtn = XtVaCreateManagedWidget("ctButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_WIDGET,
        XmNbottomOffset,      10,
	XmNbottomWidget,      this->llbtn,
        XmNleftAttachment,    XmATTACH_POSITION,
        XmNleftPosition,      50,
        XmNleftOffset,        -35,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
        NULL);

    this->upperbtn = XtVaCreateWidget("upperButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->ctbtn,
        XmNbottomOffset,      10,
        XmNleftAttachment,    XmATTACH_POSITION,
        XmNleftPosition,      50,
        XmNleftOffset,        -35,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
        NULL);

    this->lowerbtn = XtVaCreateWidget("lowerButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_FORM,
        XmNbottomOffset,      10,
        XmNleftAttachment,    XmATTACH_POSITION,
        XmNleftPosition,      50,
        XmNleftOffset,        -35,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
        NULL);

    this->leftbtn = XtVaCreateWidget("leftButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,      0,
	XmNbottomWidget,      this->ctbtn,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
        NULL);

    this->rightbtn = XtVaCreateWidget("rightButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,      0,
	XmNbottomWidget,      this->ctbtn,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
        NULL);

    this->ulbtn = XtVaCreateManagedWidget("ulButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->ctbtn,
        XmNbottomOffset,      10,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
	NULL);


    this->urbtn = XtVaCreateManagedWidget("urButton",
	xmToggleButtonWidgetClass, this->align_form,
        XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->ctbtn,
        XmNbottomOffset,      10,
	XmNleftAttachment,    XmATTACH_OPPOSITE_WIDGET,
	XmNleftWidget,	      this->lrbtn,
	XmNleftOffset,	      0,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNindicatorType,     XmONE_OF_MANY,
	XmNshadowThickness,   0,
	XmNset,               False,
	NULL);

    this->alignLabel = XtVaCreateManagedWidget("alignLabel",
	xmLabelWidgetClass,this->align_form,
	XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->urbtn,
	XmNbottomOffset,      10,
	XmNleftAttachment,    XmATTACH_FORM,
	XmNleftOffset,        5,
	NULL);

    this->separator2 = XtVaCreateManagedWidget("separator2",
	xmSeparatorGadgetClass, form,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNbottomAttachment,  XmATTACH_WIDGET,
        XmNbottomWidget,      this->align_form,
        XmNbottomOffset,      10,
        NULL);

    this->hLabel = XtVaCreateManagedWidget("hLabel",
	xmLabelWidgetClass,form,
	XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->separator2,
	XmNbottomOffset,      10,
	XmNleftAttachment,    XmATTACH_FORM,
	XmNrightAttachment,   XmATTACH_FORM,
	XmNrightOffset,       100,
	XmNleftOffset,        5,
	XmNshadowThickness,   0,
	XmNset,		      True,
	NULL);

    this->hspacing = XtVaCreateManagedWidget("hNumber",
	xmTextFieldWidgetClass,form,
        XmNbottomAttachment, XmATTACH_WIDGET,
	XmNbottomWidget,     this->separator2, 
	XmNbottomOffset,     10,
	XmNrightAttachment,  XmATTACH_FORM,
	XmNrightOffset,      5,
	XmNcolumns,	     3,
	NULL);

    this->vLabel = XtVaCreateManagedWidget("vLabel",
	xmLabelWidgetClass,form,
	XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->hLabel,
	XmNbottomOffset,      10,
	XmNleftAttachment,    XmATTACH_FORM,
	XmNleftOffset,        5,
	XmNrightAttachment,   XmATTACH_FORM,
	XmNrightOffset,       100,
	XmNshadowThickness,   0,
	XmNset,		      True,
	NULL);

    this->vspacing = XtVaCreateManagedWidget("vNumber",
	xmTextFieldWidgetClass,form,
        XmNbottomAttachment, XmATTACH_WIDGET,
	XmNbottomWidget,     this->hLabel, 
	XmNbottomOffset,     10,
	XmNrightAttachment,  XmATTACH_FORM,
	XmNrightOffset,      5,
	XmNcolumns,	     3,

	NULL);

    this->spaceLabel = XtVaCreateManagedWidget("spaceLabel",
	xmLabelWidgetClass,   form,
	XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->vLabel,
	XmNbottomOffset,      5,
	XmNleftAttachment,    XmATTACH_FORM,
	XmNleftOffset,        5,
	XmNrightAttachment,   XmATTACH_FORM,
	XmNrightOffset,       5,
	XmNalignment,	      XmALIGNMENT_CENTER,
	NULL);

    this->separator3 = XtVaCreateManagedWidget("separator3",
	xmSeparatorGadgetClass, form,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        5,
        XmNrightAttachment,   XmATTACH_FORM,
        XmNrightOffset,       5,
        XmNbottomAttachment,  XmATTACH_WIDGET,
        XmNbottomWidget,      this->spaceLabel,
        XmNbottomOffset,      10,
        NULL);

    n = 0;
    XtSetArg(arg[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(arg[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(arg[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(arg[n], XmNbottomWidget, this->separator3); n++;
    XtSetArg(arg[n], XmNleftOffset, 3); n++;
    XtSetArg(arg[n], XmNrightOffset, 3); n++;
    XtSetArg(arg[n], XmNbottomOffset, 10); n++;
    this->gc_rc = XmCreateRadioBox(form,
	"gridType", arg, n);

    XtManageChild(this->gc_rc);

    this->noneTButton = XtVaCreateManagedWidget("noneTButton",
	xmToggleButtonWidgetClass, this->gc_rc,
	XmNshadowThickness,		0,
	NULL);

    this->oneDhTButton = XtVaCreateManagedWidget("oneDhTButton",
	xmToggleButtonWidgetClass, this->gc_rc,
	XmNshadowThickness,		0,
	NULL);

    this->oneDvTButton = XtVaCreateManagedWidget("oneDvTButton",
	xmToggleButtonWidgetClass, this->gc_rc,
	XmNshadowThickness,		0,
	NULL);

    this->twoDTButton = XtVaCreateManagedWidget("twoDTButton",
	xmToggleButtonWidgetClass, this->gc_rc,
	XmNshadowThickness,		0,
	NULL);

    label = XtVaCreateManagedWidget("typeLabel",
	xmLabelWidgetClass,   form,
	XmNbottomAttachment,  XmATTACH_WIDGET,
	XmNbottomWidget,      this->gc_rc,
	XmNbottomOffset,      5,
	XmNleftAttachment,    XmATTACH_FORM,
	XmNleftOffset,        5,
	XmNrightAttachment,   XmATTACH_FORM,
	XmNrightOffset,       5,
	XmNtopAttachment,     XmATTACH_FORM,
	XmNtopOffset,         5,
	XmNalignment,	      XmALIGNMENT_CENTER,
	NULL);

    XtAddCallback(this->noneTButton,
		      XmNvalueChangedCallback,
		      (XtCallbackProc)GridDialog_DimensionCB,
		      (XtPointer)this);

    XtAddCallback(this->oneDvTButton,
		      XmNvalueChangedCallback,
		      (XtCallbackProc)GridDialog_DimensionCB,
		      (XtPointer)this);

    XtAddCallback(this->oneDhTButton,
		      XmNvalueChangedCallback,
		      (XtCallbackProc)GridDialog_DimensionCB,
		      (XtPointer)this);

    XtAddCallback(this->twoDTButton,
		      XmNvalueChangedCallback,
		      (XtCallbackProc)GridDialog_DimensionCB,
		      (XtPointer)this);


    XtManageChild(form);
    return form;
}


void GridDialog_DimensionCB(Widget    widget,
                             XtPointer clientData,
                             XtPointer callData)
{
    Boolean right, lower, center;
    GridDialog *dialog = (GridDialog*) clientData;

    if(!XmToggleButtonGetState(widget)) return;

    lower = right = center = False;
    if(XmToggleButtonGetState(dialog->lowerbtn)) {lower  = True;};
    if(XmToggleButtonGetState(dialog->rightbtn)) {right  = True;};
    if(XmToggleButtonGetState(dialog->lrbtn))    {lower  = True; right = True;}
    if(XmToggleButtonGetState(dialog->llbtn))    {lower  = True;}
    if(XmToggleButtonGetState(dialog->urbtn))    {right  = True;};
    if(XmToggleButtonGetState(dialog->ctbtn))    {center = True;};

    if(widget == dialog->noneTButton)
    {
	XtSetSensitive(dialog->hspacing, False);
	XtSetSensitive(dialog->vspacing, False);

	XtVaSetValues(dialog->separator2, XmNbottomWidget, dialog->ok, NULL);
	XtUnmanageChild(dialog->align_form);
    }
    else if(widget == dialog->oneDvTButton)
    {

	XtSetSensitive(dialog->hspacing, False);
	XtSetSensitive(dialog->vspacing, True);

	XtManageChild(dialog->upperbtn);
	XtManageChild(dialog->lowerbtn);


	XtVaSetValues(dialog->alignLabel, 
		XmNbottomWidget,	dialog->upperbtn,
		NULL);
	XtVaSetValues(dialog->ctbtn, 
		XmNbottomAttachment,	XmATTACH_WIDGET,
		XmNbottomWidget,	dialog->lowerbtn,
		NULL);

	XtUnmanageChild(dialog->ulbtn);
	XtUnmanageChild(dialog->urbtn);
	XtUnmanageChild(dialog->llbtn);
	XtUnmanageChild(dialog->lrbtn);
	XtUnmanageChild(dialog->leftbtn);
	XtUnmanageChild(dialog->rightbtn);

	XtManageChild(dialog->align_form);
	XtVaSetValues(dialog->separator2, 
	    XmNbottomWidget, dialog->align_form,NULL);
	if(lower)
	    XmToggleButtonSetState(dialog->lowerbtn, True, False);
	else if(center)
	    XmToggleButtonSetState(dialog->ctbtn, True, False);
	else
	    XmToggleButtonSetState(dialog->upperbtn, True, False);
	XtVaSetValues (dialog->align_form, XmNheight, 0, NULL);
    }
    else if(widget == dialog->oneDhTButton)
    {

	XtSetSensitive(dialog->hspacing, True);
	XtSetSensitive(dialog->vspacing, False);

	XtManageChild(dialog->leftbtn);
	XtManageChild(dialog->rightbtn);

	XtVaSetValues(dialog->alignLabel,
		XmNbottomWidget,	dialog->ctbtn,
		NULL);
	XtVaSetValues(dialog->ctbtn, 
		XmNbottomAttachment,	XmATTACH_FORM,
		NULL);

	XtUnmanageChild(dialog->ulbtn);
	XtUnmanageChild(dialog->urbtn);
	XtUnmanageChild(dialog->llbtn);
	XtUnmanageChild(dialog->lrbtn);
	XtUnmanageChild(dialog->upperbtn);
	XtUnmanageChild(dialog->lowerbtn);

	XtManageChild(dialog->align_form);
	XtVaSetValues(dialog->separator2, 
	    XmNbottomWidget, dialog->align_form,NULL);

	if(right)
	    XmToggleButtonSetState(dialog->rightbtn, True, False);
	else if(center)
	    XmToggleButtonSetState(dialog->ctbtn, True, False);
	else
	    XmToggleButtonSetState(dialog->leftbtn, True, False);

	/* make the form widget recalc its height requirement.  In the 1dH case,
	   the align_form widget is getting too tall.  Changing from 1dH to either
	   2d or 1dV, then makes a mess on the screen.*/
	XtVaSetValues (dialog->align_form, XmNheight, 0, NULL);
    }
    else if(widget == dialog->twoDTButton)
    {

	XtSetSensitive(dialog->hspacing, True);
	XtSetSensitive(dialog->vspacing, True);

	XtManageChild(dialog->ulbtn);
	XtManageChild(dialog->urbtn);
	XtManageChild(dialog->llbtn);
	XtManageChild(dialog->lrbtn);

	XtVaSetValues(dialog->alignLabel,
		XmNbottomWidget,	dialog->ulbtn,
		NULL);
	XtVaSetValues(dialog->ctbtn, 
		XmNbottomAttachment,	XmATTACH_WIDGET,
		XmNbottomWidget,	dialog->llbtn,
		NULL);

	XtUnmanageChild(dialog->leftbtn);
	XtUnmanageChild(dialog->rightbtn);
	XtUnmanageChild(dialog->upperbtn);
	XtUnmanageChild(dialog->lowerbtn);

	XtManageChild(dialog->align_form);
	XtVaSetValues(dialog->separator2, 
	    XmNbottomWidget, dialog->align_form,NULL);
	if(lower)
	    if(right)
		XmToggleButtonSetState(dialog->lrbtn, True, False);
	    else
		XmToggleButtonSetState(dialog->llbtn, True, False);
	else if(center)
	    XmToggleButtonSetState(dialog->ctbtn, True, False);
	else
	    if(right)
		XmToggleButtonSetState(dialog->urbtn, True, False);
	    else
		XmToggleButtonSetState(dialog->ulbtn, True, False);
	XtVaSetValues (dialog->align_form, XmNheight, 0, NULL);
    }
}


#if 0
boolean GridDialog::okCallback(Dialog *d)
{
    int   width, height;
    Boolean  upper_left, upper_right, center, lower_left, lower_right, snap;
    Boolean  upper, lower, right, left;
    unsigned int x_align; 
    unsigned int y_align;
    WorkSpaceInfo *wsinfo = this->workSpace->getInfo();
    Boolean align_horizontal;
    Boolean align_vertical;

    align_horizontal = align_vertical = False;
    if(XmToggleButtonGetState(this->twoDTButton))
	align_horizontal = align_vertical = True;
    else if(XmToggleButtonGetState(this->oneDvTButton))
	align_vertical = True;
    else if(XmToggleButtonGetState(this->oneDhTButton))
	align_horizontal = True;

    XtVaGetValues(this->ulbtn,    XmNset, &upper_left, NULL);
    XtVaGetValues(this->urbtn,    XmNset, &upper_right, NULL);
    XtVaGetValues(this->ctbtn,    XmNset, &center, NULL);
    XtVaGetValues(this->llbtn,    XmNset, &lower_left, NULL);
    XtVaGetValues(this->lrbtn,    XmNset, &lower_right, NULL);
    XtVaGetValues(this->rightbtn, XmNset, &right, NULL);
    XtVaGetValues(this->leftbtn,  XmNset, &left, NULL);
    XtVaGetValues(this->upperbtn, XmNset, &upper, NULL);
    XtVaGetValues(this->lowerbtn, XmNset, &lower, NULL);

    if(align_horizontal)
    {
	if (upper_left || lower_left || left) 
	    x_align = XmALIGNMENT_BEGINNING;
	else if (center)
	    x_align = XmALIGNMENT_CENTER;
	else
	    x_align = XmALIGNMENT_END;
    }
    else
	x_align = XmALIGNMENT_NONE;

    if(align_vertical)
    {
	if (upper_left || upper_right || upper) 
	    y_align = XmALIGNMENT_BEGINNING;
	else if (center)
	    y_align = XmALIGNMENT_CENTER;
	else
	    y_align = XmALIGNMENT_END;
    }
    else
	y_align = XmALIGNMENT_NONE;


    wsinfo->setGridAlignment(x_align, y_align);
    
    wsinfo->setGridActive(align_horizontal || align_vertical);
    
    XtVaGetValues(this->hspacing, XmNiValue, &width, NULL);
    XtVaGetValues(this->vspacing, XmNiValue, &height, NULL);
    wsinfo->setGridSpacing(width, height);

    this->workSpace->installInfo(NULL);

    return TRUE;
}

void GridDialog::resetToggleBtn()
{
     XtVaSetValues(this->ulbtn,    XmNset, False, NULL);
     XtVaSetValues(this->urbtn,    XmNset, False, NULL);
     XtVaSetValues(this->llbtn,    XmNset, False, NULL);
     XtVaSetValues(this->lrbtn,    XmNset, False, NULL);
     XtVaSetValues(this->ctbtn,    XmNset, False, NULL);
     XtVaSetValues(this->leftbtn,  XmNset, False, NULL);
     XtVaSetValues(this->rightbtn, XmNset, False, NULL);
     XtVaSetValues(this->upperbtn, XmNset, False, NULL);
     XtVaSetValues(this->lowerbtn, XmNset, False, NULL);
}
#endif


int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);
  XtVaSetValues(Shell,
  	XmNallowShellResize, True,
  	NULL);

  Form = createDialog(Shell);
  XtManageChild(Form);
  XmToggleButtonSetState(this->noneTButton, True, True);
  XtRealizeWidget(Shell);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  357,  593,  145,  287, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   56,  252,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,   97,   73,   25, 0,0,0, /* llButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   97,   73,   25, 0,0,0, /* lrButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   62,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   73,   25, 0,0,0, /* ulButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   27,   73,   25, 0,0,0, /* urButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   40,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   40,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  135,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  135,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  139,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  133,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  133,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  133,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  133,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  135,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  196,  371, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,  107,  336,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  336,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  324,  186,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,  186,   62, 0,0,0, /* alignForm */},
   {CWWidth | CWHeight | CWX | CWY,   58,   27,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   85,   25, 0,0,0, /* leftButton */},
   {CWWidth | CWHeight | CWX | CWY,   90,   27,   91,   25, 0,0,0, /* rightButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  186,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   91,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   91,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  186,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  186,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  190,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  184,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  184,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  184,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  184,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  186,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  145,  287, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   56,  252,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,   58,   27,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   85,   25, 0,0,0, /* leftButton */},
   {CWWidth | CWHeight | CWX | CWY,   90,   27,   91,   25, 0,0,0, /* rightButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   40,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   40,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  135,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  135,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  139,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  133,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  133,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  133,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  133,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  135,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  196,  441, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,  107,  406,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  406,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  394,  186,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,  186,  132, 0,0,0, /* alignForm */},
   {CWWidth | CWHeight | CWX | CWY,   58,   62,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   27,   91,   25, 0,0,0, /* upperButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   97,   91,   25, 0,0,0, /* lowerButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  186,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   91,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   91,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  186,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  186,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  190,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  184,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  184,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  184,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  184,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  186,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  145,  287, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   56,  252,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,   58,   62,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   27,   91,   25, 0,0,0, /* upperButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   97,   91,   25, 0,0,0, /* lowerButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   40,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   40,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  135,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  135,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  139,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  133,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  133,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  133,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  133,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  135,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  196,  441, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,  107,  406,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  406,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  394,  186,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,  186,  132, 0,0,0, /* alignForm */},
   {CWWidth | CWHeight | CWX | CWY,    5,   97,   73,   25, 0,0,0, /* llButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   97,   73,   25, 0,0,0, /* lrButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   62,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   73,   25, 0,0,0, /* ulButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   27,   73,   25, 0,0,0, /* urButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  186,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   91,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   91,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  186,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  186,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  190,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  184,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  184,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  184,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  184,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  186,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  145,  287, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   56,  252,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,   97,   73,   25, 0,0,0, /* llButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   97,   73,   25, 0,0,0, /* lrButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   62,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   73,   25, 0,0,0, /* ulButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   27,   73,   25, 0,0,0, /* urButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   40,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   40,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  135,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  135,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  139,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  133,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  133,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  133,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  133,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  135,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  196,  441, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,  107,  406,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  406,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  394,  186,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,  186,  132, 0,0,0, /* alignForm */},
   {CWWidth | CWHeight | CWX | CWY,    5,   97,   73,   25, 0,0,0, /* llButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   97,   73,   25, 0,0,0, /* lrButton */},
   {CWWidth | CWHeight | CWX | CWY,   58,   62,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   73,   25, 0,0,0, /* ulButton */},
   {CWWidth | CWHeight | CWX | CWY,  108,   27,   73,   25, 0,0,0, /* urButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  186,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   91,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   91,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  155,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  186,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  186,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  190,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  184,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  184,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  184,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  184,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  186,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},

   {CWWidth | CWHeight            ,  357,  593,  145,  371, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   56,  336,   84,   25, 0,0,0, /* cancelButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  336,   60,   25, 0,0,0, /* okButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,  324,  135,    2, 0,0,0, /* separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  252,  135,   62, 0,0,0, /* alignForm */},
   {CWWidth | CWHeight | CWX | CWY,   33,   27,   73,   25, 0,0,0, /* ctButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,   27,   85,   25, 0,0,0, /* leftButton */},
   {CWWidth | CWHeight | CWX | CWY,   39,   27,   91,   25, 0,0,0, /* rightButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    0,   64,   17, 0,0,0, /* alignLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  240,  135,    2, 0,0,0, /* separator2 */},
   {CWWidth | CWHeight | CWX | CWY,    5,  213,   40,   17, 0,0,0, /* hLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  199,   36,   31, 0,0,0, /* hNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  186,   40,   17, 0,0,0, /* vLabel */},
   {CWWidth | CWHeight | CWX | CWY,  104,  172,   36,   31, 0,0,0, /* vNumber */},
   {CWWidth | CWHeight | CWX | CWY,    5,  164,  135,   17, 0,0,0, /* spaceLabel */},
   {CWWidth | CWHeight | CWX | CWY,    5,  152,  135,    2, 0,0,0, /* separator3 */},
   {CWWidth | CWHeight | CWX | CWY,    3,   27,  139,  115, 0,0,0, /* gridType */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  133,   25, 0,0,0, /* noneTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   31,  133,   25, 0,0,0, /* oneDhTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   59,  133,   25, 0,0,0, /* oneDvTButton */},
   {CWWidth | CWHeight | CWX | CWY,    3,   87,  133,   25, 0,0,0, /* twoDTButton */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,  135,   17, 0,0,0, /* typeLabel */},
   {CWWidth | CWHeight            ,    0,    0,   52,   17, 0,0,0, /* TipLabel */},
};

  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->oneDhTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

#if 1
  XmToggleButtonSetState(this->noneTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->oneDvTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->noneTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->twoDTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->noneTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->twoDTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);

  XmToggleButtonSetState(this->oneDhTButton, True, True);
  LessTifTestWaitForIt(Shell);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);
#endif
}

  LessTifTestMainLoop(Shell);
  exit(0);
}
