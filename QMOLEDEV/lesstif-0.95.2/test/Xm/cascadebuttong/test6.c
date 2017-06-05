/* $Header: /cvsroot/lesstif/lesstif/test/Xm/cascadebuttong/test6.c,v 1.2 2001/05/23 15:00:02 amai Exp $ */
#include <stdlib.h>

#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumnP.h>
#include <Xm/RepType.h>


char *fallback[] =
{
    "*mb*tearOffModel:					tear_off_enabled",
    "*mc1.labelString:					Shadow=1",
    "*mc1.shadowThickness:				1",
    "*mp1*shadowThickness:				1",
    "*mi1.labelString:					Foo",
    "*mc2.labelString:					Shadow=3",
    "*mc2.shadowThickness:				3",
    "*mp2*shadowThickness:				3",
    "*mi2.labelString:					Bar",
    "*cc1.labelString:					Shadow=1",
    "*cc1.shadowThickness:				1",
    "*cp1*shadowThickness:				1",
    "*ci1.labelString:					Foo",
    "*cc2.labelString:					Shadow=3",
    "*cc2.shadowThickness:				3",
    "*cp2*shadowThickness:				3",
    "*ci2.labelString:					Bar",
    "*oi1.labelString:					Shadow=1 Highlight=1",
    "*om1.XmCascadeButtonGadget.shadowThickness:	1",
    "*om1.XmCascadeButtonGadget.highlightThickness:	1",
    "*op1.shadowThickness:				1",
    "*oi2.labelString:					Shadow=1 Highlight=3",
    "*om2.XmCascadeButtonGadget.shadowThickness:	1",
    "*om2.XmCascadeButtonGadget.highlightThickness:	3",
    "*op2.shadowThickness:				1",
    "*oi3.labelString:					Shadow=3 Highlight=1",
    "*om3.XmCascadeButtonGadget.shadowThickness:	3",
    "*om3.XmCascadeButtonGadget.highlightThickness:	1",
    "*op3.shadowThickness:				3",
    "*oi4.labelString:					Shadow=3 Highlight=3",
    "*om4.XmCascadeButtonGadget.shadowThickness:	3",
    "*om4.XmCascadeButtonGadget.highlightThickness:	3",
    "*op4.shadowThickness:				3",
    NULL
};

int
main(int argc, char **argv)
{
    XtAppContext ac;
    Widget as, form, mb;
    Widget mp1, mc1, mi1;
    Widget mp2, mc2, mi2;
    Widget cr1, cp1, ci1;
    Widget cr2, cp2, ci2;
    Widget op1, om1;
    Widget op2, om2;
    Widget op3, om3;
    Widget op4, om4;
    Arg args[6];

    XmRepTypeInstallTearOffModelConverter();
    as = XtAppInitialize(&ac, "test6", NULL, 0, &argc, argv, fallback, NULL, 0);
    form = XmCreateForm(as, "form", NULL, 0);

    XtSetArg(args[0], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[1], XmNtopAttachment, XmATTACH_FORM);
    mb = XmCreateMenuBar(form, "mb", args, 2);

    mp1 = XmCreatePulldownMenu(mb, "mp1", NULL, 0);
    XtSetArg(args[0], XmNsubMenuId, mp1);
    mc1 = XmCreateCascadeButtonGadget(mb, "mc1", args, 1);
    XtManageChild(XmCreatePushButtonGadget(mp1, "mi1", NULL, 0));

    mp2 = XmCreatePulldownMenu(mb, "mp2", NULL, 0);
    XtSetArg(args[0], XmNsubMenuId, mp2);
    mc2 = XmCreateCascadeButtonGadget(mb, "mc2", args, 1);
    XtManageChild(XmCreatePushButtonGadget(mp2, "mi2", NULL, 0));

    XtSetArg(args[0], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[1], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[2], XmNtopWidget, mb);
    cr1 = XmCreateRowColumn(form, "cr1", args, 3);
    ((XmRowColumnWidget)cr1)->row_column.type = XmMENU_PULLDOWN;
    cp1 = XmCreatePulldownMenu(cr1, "cp1", NULL, 0);
    XtSetArg(args[0], XmNsubMenuId, cp1);
    XtManageChild(XmCreateCascadeButtonGadget(cr1, "cc1", args, 1));
    XtManageChild(XmCreatePushButtonGadget(cp1, "ci1", NULL, 0));

    XtSetArg(args[0], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[1], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[2], XmNtopWidget, cr1);
    cr2 = XmCreateRowColumn(form, "cr2", args, 3);
    ((XmRowColumnWidget)cr2)->row_column.type = XmMENU_PULLDOWN;
    cp2 = XmCreatePulldownMenu(cr2, "cp2", NULL, 0);
    XtSetArg(args[0], XmNsubMenuId, cp2);
    XtManageChild(XmCreateCascadeButtonGadget(cr2, "cc2", args, 1));
    XtManageChild(XmCreatePushButtonGadget(cp2, "ci2", NULL, 0));

    op1 = XmCreatePulldownMenu(form, "op1", NULL, 0);
    XtManageChild(XmCreatePushButtonGadget(op1, "oi1", NULL, 0));
    XtSetArg(args[0], XmNsubMenuId, op1);
    XtSetArg(args[1], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[2], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[3], XmNtopWidget, cr2);
    om1 = XmCreateOptionMenu(form, "om1", args, 4);

    op2 = XmCreatePulldownMenu(form, "op2", NULL, 0);
    XtManageChild(XmCreatePushButtonGadget(op2, "oi2", NULL, 0));
    XtSetArg(args[0], XmNsubMenuId, op2);
    XtSetArg(args[1], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[2], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[3], XmNtopWidget, om1);
    om2 = XmCreateOptionMenu(form, "om2", args, 4);

    op3 = XmCreatePulldownMenu(form, "op3", NULL, 0);
    XtManageChild(XmCreatePushButtonGadget(op3, "oi3", NULL, 0));
    XtSetArg(args[0], XmNsubMenuId, op3);
    XtSetArg(args[1], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[2], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[3], XmNtopWidget, om2);
    om3 = XmCreateOptionMenu(form, "om3", args, 4);

    op4 = XmCreatePulldownMenu(form, "op4", NULL, 0);
    XtManageChild(XmCreatePushButtonGadget(op4, "oi4", NULL, 0));
    XtSetArg(args[0], XmNsubMenuId, op4);
    XtSetArg(args[1], XmNbottomAttachment, XmATTACH_FORM);
    XtSetArg(args[2], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[3], XmNrightAttachment, XmATTACH_FORM);
    XtSetArg(args[4], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[5], XmNtopWidget, om3);
    om4 = XmCreateOptionMenu(form, "om4", args, 6);

    XtManageChild(om4);
    XtManageChild(om3);
    XtManageChild(om2);
    XtManageChild(om1);
    XtManageChild(cr2);
    XtManageChild(cr1);
    XtManageChild(mc2);
    XtManageChild(mc1);
    XtManageChild(mb);
    XtManageChild(form);
    XtRealizeWidget(as);
    
    {
	static XtWidgetGeometry Expected[] =
	{
	    CWWidth | CWHeight,              0,   0, 177, 228,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0,   0, 138,  33,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  5,   5,  62,  23,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 67,   5,  66,  23,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0,  33,  74,  25,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  3,   3,  68,  19,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0,  58,  82,  30,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  3,   3,  76,  24,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0,  88, 163,  31,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  3,   3,   4,  25,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 10,   3, 150,  25,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0, 119, 167,  35,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  3,   3,   4,  29,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 10,   3, 154,  29,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0, 154, 173,  35,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  3,   3,   4,  29,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 10,   3, 160,  29,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  0, 189, 177,  39,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY,  3,   3,   4,  33,  0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 10,   3, 164,  33,  0, 0, 0,
	};
	PrintDetails(as, Expected);
    }

    LessTifTestMainLoop(as);
    return 0;
}
