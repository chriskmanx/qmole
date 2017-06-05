/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test53.c,v 1.4 2001/05/15 14:46:10 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/FormP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RepType.h>
#include <Xm/SeparatoGP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MainWP.h>


/* This test demonstrates a problem with NEWMENUS and attaching submenus
   after creation with tearoff enabled.  Run once normally, and once with
   -xrm "*tearOffModel: tear_off_enabled".  Notice that the menu order is
   different(!?) and that there's no submenu for the one of the
   cascadebuttons 
 */

#define RC_Armed(w)	(((XmRowColumnWidget)(w))->row_column.armed)


Widget toplevel, mainw, menubar, filepane;
Widget button, button1, button2, sep;
Widget saveAsPane, button3, button4, button5;
Widget button6, button7, button8, button9, editpane;
Widget button10, button11, sub_popup_pane;
Widget inclPane, button12, button13, button14;
Widget one, two, three;
Widget optpane;
Widget opt1, opt2;
Widget ms1, ms2;
Widget wa, md, mp, mo, option;
Widget popup;
#if 0
Widget	mb;
#endif

void
PrintPostedFrom(Widget w)
{
    Widget pf = XmGetPostedFromWidget(w);

    printf("XmGetPostedFromWidget(%s)=%s\n",
	   XtName(w),
	   pf ? XtName(pf) : "(null)");
}

void
PopdownCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf("popdown %s\n", XtName(w));
}

void
PopupCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf("popup %s\n", XtName(w));
}

void
MapCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf("RC_CascadeBtn(%s)=%p, armed=%x\n", XtName(w), RC_CascadeBtn(w), (int)RC_Armed(w));
}

void
DisarmCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf("disarm\n");
}

void
PushCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    Widget mp = (Widget)client_data;

    if (XmIsTraversable(wa))
	printf("(wa) IS TRAVERSABLE\n");
    else
	printf("(wa) IS NOT TRAVERSABLE\n");
    if (_XmIsNavigable(wa))
	printf("(wa) IS NAVIGABLE\n");
    else
	printf("(wa) IS NOT NAVIGABLE\n");
#if 0
    if (XmIsTraversable(mb))
	printf("(mb) IS TRAVERSABLE\n");
    else
	printf("(mb) IS NOT TRAVERSABLE\n");
    if (_XmIsNavigable(mb))
	printf("(mb) IS NAVIGABLE\n");
    else
	printf("(mb) IS NOT NAVIGABLE\n");
#endif
    if (XmIsTraversable(md))
	printf("(md) IS TRAVERSABLE\n");
    else
	printf("(md) IS NOT TRAVERSABLE\n");
    if (_XmIsNavigable(md))
	printf("(md) IS NAVIGABLE\n");
    else
	printf("(md) IS NOT NAVIGABLE\n");
    if (XmIsTraversable(mp))
	printf("(mp) IS TRAVERSABLE\n");
    else
	printf("(mp) IS NOT TRAVERSABLE\n");
    if (_XmIsNavigable(mp))
	printf("(mp) IS NAVIGABLE\n");
    else
	printf("(mp) IS NOT NAVIGABLE\n");
    if (XmIsTraversable(option))
	printf("(option) IS TRAVERSABLE\n");
    else
	printf("(option) IS NOT TRAVERSABLE\n");
    if (_XmIsNavigable(option))
	printf("(option) IS NAVIGABLE\n");
    else
	printf("(option) IS NOT NAVIGABLE\n");

    printf("\n");

    PrintPostedFrom(mp);
}

void
buttonPressEventHandler(Widget w,
			XtPointer client_data,
			XEvent *e,
			Boolean *cont)
{
    XButtonEvent *be = (XButtonEvent*)e;

    if (1/*be->button == 3*/)
    {
	XmMenuPosition(popup, be);

	XtManageChild(popup);
    }
}

void
buttonPopup(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XButtonEvent *be = (XButtonEvent*)event;

    printf("Widget: %s : BUTTON3!\n", XtName(w));

    XmMenuPosition(popup, be);

    XtManageChild(popup);
}


static char but_trans[] = "\
#override <Btn3Down> : buttonPopup()\n";

static XtActionsRec app_actions[] = {
	{ "buttonPopup", buttonPopup }
};


void GXAttachMenu(Widget submenu, Widget parent, int position)
/*-
   attach SUBMENU to PARENT menu's POSITIONth cascade button (starts at 0)
-*/
{
    Cardinal kidcnt;
    WidgetList *kidlist;
    int i, cnt;
    WidgetClass class;

    /* get list of children */
    XtVaGetValues(parent,
		  XtNchildren, &kidlist,
		  XtNnumChildren, &kidcnt,
		  NULL);
    if ((position > kidcnt) || (position < 0))
	return;			/* verify range */

    /* search for POSITIONth cascade button */
    for (i = 0, cnt = -1; i < kidcnt; i++) {
	class = XtClass((Widget) kidlist[i]);
	if (class == xmCascadeButtonWidgetClass)
	    cnt++;
	if (cnt == position) {	/* set submenuid & return */
	    XtVaSetValues((Widget) kidlist[i],
			  XmNsubMenuId, submenu,
			  NULL);
	    return;
	}
    }
    return;			/* failed to find match */
}


int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Arg args[6];
    int ac;
    Dimension mmt, mmb, mml, mmr, mmw, mmh, st, ht;

    XmRepTypeInstallTearOffModelConverter();

    toplevel = XtVaAppInitialize(&theApp, "mainW", NULL, 0,
				 &argc, argv, NULL, NULL);

    XtAppAddActions(theApp, app_actions, XtNumber(app_actions));
    XtSetArg(args[0], XtNtranslations, XtParseTranslationTable(but_trans));
    mainw = XmCreateMainWindow(toplevel, "MainWindow", args, 1);
    XtManageChild(mainw);

    printf("after creation:\n");

    wa = XtVaCreateWidget("work_area", xmRowColumnWidgetClass, mainw,
			  XmNrowColumnType, XmWORK_AREA,
			  NULL);
#if 0
/* this causes core dumps with Motif-1.2 on Solaris */
    PrintPostedFrom(wa);
#endif

#if 0
    mb = XtVaCreateWidget("menu_bar", xmRowColumnWidgetClass, mainw,
			  XmNrowColumnType, XmMENU_BAR,
			  NULL);

    PrintPostedFrom(mb);
#endif

    ms1 = XtVaCreatePopupShell("ms1", xmMenuShellWidgetClass, mainw,
			       XmNwidth, 1,
			       XmNheight, 1,
			       NULL);


    md = XtVaCreateWidget("menu_pulldown", xmRowColumnWidgetClass, ms1,
			  XmNrowColumnType, XmMENU_PULLDOWN,
			  NULL);

    PrintPostedFrom(md);

    ms2 = XtVaCreatePopupShell("ms2", xmMenuShellWidgetClass, mainw,
			       XmNwidth, 1,
			       XmNheight, 1,
			       NULL);

    mp = XtVaCreateWidget("menu_popup", xmRowColumnWidgetClass, ms2,
			  XmNrowColumnType, XmMENU_POPUP,
			  NULL);

    PrintPostedFrom(mp);

    mo = XtVaCreateWidget("menu_option", xmRowColumnWidgetClass, mainw,
			  XmNrowColumnType, XmMENU_OPTION,
			  NULL);

    PrintPostedFrom(mo);

    printf("\n");

    menubar = XmCreateMenuBar(mainw,
			      "menuBar",
			      NULL,0);

    filepane = XmCreatePulldownMenu(menubar,
				    "file_pane",
				    NULL,0);

    button = XtVaCreateManagedWidget("File",
				     xmCascadeButtonWidgetClass,
				     menubar,
				     XmNsubMenuId, filepane,
				     NULL);
  XtVaGetValues(button,
		XmNmarginTop, &mmt, XmNmarginBottom, &mmb,
		XmNmarginLeft, &mml, XmNmarginRight, &mmr,
		XmNmarginWidth, &mmw, XmNmarginHeight, &mmh,
		XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		NULL);
printf("%d %d %d %d %d %d %d %d\n",
	mmt, mmb, mml, mmr, mmw, mmh, st, ht);


    editpane = XmCreatePulldownMenu(menubar,
				    "pane2",
				    NULL, 0);

    button6 = XtVaCreateManagedWidget("Edit",
				      xmCascadeButtonWidgetClass,
				      menubar,
				      XmNsubMenuId, editpane,
				      NULL);

    button7 = XtVaCreateManagedWidget("Cut",
				      xmPushButtonWidgetClass,
				      editpane,
				      NULL);

    button12 = XtVaCreateManagedWidget("Include",
				       xmCascadeButtonWidgetClass,
				       filepane,
				       XmNsubMenuId, NULL,
				       NULL);
    inclPane = XmCreatePulldownMenu(filepane,
				    "incl_pane",
				    NULL, 0);



    button3 = XtVaCreateManagedWidget("Save As",
				      xmCascadeButtonWidgetClass,
				      filepane,
				      XmNsubMenuId, NULL,
				      NULL);

    saveAsPane = XmCreatePulldownMenu(filepane,
				      "save_as_pane",
				      NULL, 0);


    button1 = XtVaCreateManagedWidget("Open",
				      xmPushButtonWidgetClass,
				      filepane,
				      NULL);

    XtAddCallback(XtParent(filepane), XmNpopdownCallback, PopdownCB, NULL);

    XtAddCallback(XtParent(filepane), XmNpopupCallback, PopupCB, NULL);

    XtAddCallback(filepane, XmNmapCallback, MapCB, NULL);

    XtAddCallback(button1, XmNactivateCallback, PushCB, (Widget)filepane);

    XtAddCallback(button1, XmNdisarmCallback, DisarmCB, NULL);

    sep = XtVaCreateManagedWidget("sep",
				  xmSeparatorGadgetClass,
                                  filepane,
                                  NULL);

    button2 = XtVaCreateManagedWidget("Close",
				      xmPushButtonWidgetClass,
				      filepane,
				      NULL);



    button13 = XtVaCreateManagedWidget("fig",
				       xmPushButtonWidgetClass,
				       inclPane,
				       NULL);

    button14 = XtVaCreateManagedWidget("pic",
				       xmPushButtonWidgetClass,
				       inclPane,
				       NULL);


    XtAddCallback(XtParent(saveAsPane), XmNpopupCallback, PopupCB, NULL);

    XtAddCallback(XtParent(saveAsPane), XmNpopdownCallback, PopdownCB, NULL);

    XtAddCallback(saveAsPane, XmNmapCallback, MapCB, NULL);


    button4 = XtVaCreateManagedWidget("MS Word",
				      xmPushButtonWidgetClass,
				      saveAsPane,
				      NULL);

    button5 = XtVaCreateManagedWidget("LaTeX",
				      xmPushButtonWidgetClass,
				      saveAsPane,
				      NULL);

    XtAddCallback(button5, XmNactivateCallback, PushCB, (Widget)saveAsPane);

    XtManageChild(menubar);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, mainw,
				  NULL);

    optpane = XmCreatePulldownMenu(mainw, "optpane", NULL, 0);

    opt1 = XtVaCreateManagedWidget("true",
				   xmPushButtonWidgetClass,
				   optpane,
				   NULL);

    opt2 = XtVaCreateManagedWidget("false",
				   xmPushButtonWidgetClass,
				   optpane,
				   NULL);

    ac = 0;
    XtSetArg(args[ac], XmNsubMenuId, optpane); ac++;
    XtSetArg(args[ac], XmNlabelString, XmStringCreateLocalized("Guess:")); ac++;
    XtSetArg(args[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(args[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(args[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;

    option = XmCreateOptionMenu(one, "option", args, ac);

    XtManageChild(option);

  XtVaGetValues(XmOptionButtonGadget(option),
		XmNmarginTop, &mmt, XmNmarginBottom, &mmb,
		XmNmarginLeft, &mml, XmNmarginRight, &mmr,
		XmNmarginWidth, &mmw, XmNmarginHeight, &mmh,
		XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		NULL);
printf("%d %d %d %d %d %d %d %d %d %d\n",
	mmt, mmb, mml, mmr, mmw, mmh, st, ht, XtWidth(option), XtHeight(option));

    XtAddCallback(opt1, XmNactivateCallback, PushCB, (Widget)option);

    two = XtVaCreateManagedWidget("option", xmPushButtonWidgetClass, one,
				  XmNtopAttachment, XmATTACH_WIDGET,
				  XmNtopWidget, option,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_NONE,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);

    XtAddCallback(two, XmNactivateCallback, PushCB, (Widget)option);
    XtAddCallback(two, XmNactivateCallback, PushCB, (Widget)optpane);

    three = XtVaCreateManagedWidget("menu", xmPushButtonWidgetClass, one,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, option,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, two,
				    NULL);

    XtAddEventHandler(two, KeyPressMask, False,
		      buttonPressEventHandler, NULL);

#if 0
    XtSetArg(args[0], XmNmenuPost, /* "<Btn3Down>"*/"<Button3>");
#endif
    popup = XmCreatePopupMenu(one, "popup", args, 1);

#if 1
#endif

    button8 = XtVaCreateManagedWidget("button8",
				      xmPushButtonWidgetClass,
				      popup,
				      NULL);

    XtAddCallback(button8, XmNactivateCallback, PushCB, (Widget)popup);

    sub_popup_pane = XmCreatePulldownMenu(two,
					 "save_as_pane",
					 NULL, 0);

    button10 = XtVaCreateManagedWidget("button10", xmPushButtonWidgetClass, sub_popup_pane,
				       NULL);

    XtAddCallback(button10, XmNactivateCallback, PushCB, (Widget)sub_popup_pane);

    button11 = XtVaCreateManagedWidget("button11", xmPushButtonWidgetClass, sub_popup_pane,
				       NULL);

    button9 = XtVaCreateManagedWidget("button9", xmCascadeButtonWidgetClass, popup,
				      XmNsubMenuId, sub_popup_pane,
				      NULL);

    XtAddCallback(three, XmNactivateCallback, PushCB, (Widget)menubar);
    XtAddCallback(three, XmNactivateCallback, PushCB, (Widget)filepane);
    XtAddCallback(three, XmNactivateCallback, PushCB, (Widget)saveAsPane);

    GXAttachMenu(inclPane,filepane,0);
    GXAttachMenu(saveAsPane,filepane,1);

    XmMainWindowSetAreas(mainw, menubar, NULL, NULL, NULL, one);

    XtRealizeWidget(toplevel);

  XtVaGetValues(XmOptionButtonGadget(option),
		XmNmarginTop, &mmt, XmNmarginBottom, &mmb,
		XmNmarginLeft, &mml, XmNmarginRight, &mmr,
		XmNmarginWidth, &mmw, XmNmarginHeight, &mmh,
		XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		NULL);
printf("%d %d %d %d %d %d %d %d %d %d\n",
	mmt, mmb, mml, mmr, mmw, mmh, st, ht, XtWidth(option), XtHeight(option));


  
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  116,   91, 0,0,0, /* MainWindow */
   CWWidth | CWHeight | CWX | CWY,    3,    3,    4,   25, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   10,    3,   84,   25, 0,0,0, /* OptionButton */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  116,   31, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,   45,    5,   40,   21, 0,0,0, /* Edit */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  116,   60, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  116,   35, 0,0,0, /* option */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   40,   29, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   46,    3,   67,   29, 0,0,0, /* OptionButton */
   CWWidth | CWHeight | CWX | CWY,   68,   35,   48,   25, 0,0,0, /* option */
   CWWidth | CWHeight | CWX | CWY,    0,   35,   68,   25, 0,0,0, /* menu */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);
    */

    exit(0);
}

