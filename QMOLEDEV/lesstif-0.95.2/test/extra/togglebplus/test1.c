/*
 * demo.c - Demonstration der neuen ToggleButton-Klasse
 * Letzte Modifikation: 31.10.1994
 * Harald Albrecht ( albrecht@igpm.rwth-aachen.de )
 */

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include "ToggleBplus.h"
#include <X11/keysym.h>
#include <stdio.h>

void DummyCallback(Widget w, XtPointer clientdata, XtPointer calldata)
{
    exit(0);
} /* DummyCallback */


static char *Fallbacks[] = {
    "*.background:			gray75",
    "*.XmToggleButton.selectColor:	red",
    NULL
};

int main(int argc, char **argv)
{
    XtAppContext AppCtx;
    Widget       TopLevel, Main, Menu, Form, Text, PullDownMenu;
    Widget       Cascade, Butt1, Butt3, Butt4, Butt5;
    Widget       Frame, Label, RowCol, w;
    Arg          args[10];
    
    TopLevel = XtVaAppInitialize(&AppCtx, "Demo", NULL, 0, 
                                 &argc, argv, Fallbacks, NULL);
    Main = XmCreateMainWindow(TopLevel, "main", NULL, 0);
    XtManageChild(Main);

    Menu = XmCreateMenuBar(Main, "menu", NULL, 0);
    XtManageChild(Menu);
    XtSetArg(args[0], XmNmnemonic, XK_F);
    Cascade = XmCreateCascadeButton(Menu, "File", args, 1);
    XtManageChild(Cascade);
    PullDownMenu = XmCreatePulldownMenu(Cascade, "filemenu", NULL, 0);

    w = XmCreateToggleButtonPlus(PullDownMenu, "Enjoy it!", NULL, 0);
    XtVaSetValues(w, XmNmnemonic, XK_j, NULL);
    XtManageChild(w);
    XtVaCreateManagedWidget(
        "sepp1", xmSeparatorWidgetClass, PullDownMenu,
        NULL);
    w = XmCreateToggleButtonPlus(PullDownMenu, "One", NULL, 0);
    XtVaSetValues(w, XmNmnemonic, XK_O, XmNindicatorType, XmONE_OF_MANY, NULL);
    XtManageChild(w);
    w = XmCreateToggleButtonPlus(PullDownMenu, "Two", NULL, 0);
    XtVaSetValues(w, XmNmnemonic, XK_T, XmNindicatorType, XmONE_OF_MANY, NULL);
    XtManageChild(w);
    w = XmCreateToggleButtonPlus(PullDownMenu, "Fourtytwo", NULL, 0);
    XtVaSetValues(w, XmNmnemonic, XK_F, XmNindicatorType, XmONE_OF_MANY, NULL);
    XtManageChild(w);
    XtVaCreateManagedWidget(
        "sepp2", xmSeparatorWidgetClass, PullDownMenu,
        NULL);
    Butt1 = XtVaCreateManagedWidget(
        "Exit", xmPushButtonWidgetClass, PullDownMenu,
        XmNmnemonic, XK_E,
        NULL);
    XtAddCallback(Butt1, XmNactivateCallback, 
                  (XtCallbackProc) DummyCallback, NULL);
    
    XtVaSetValues(Cascade, XmNsubMenuId, PullDownMenu, NULL);
    
    Form = XtVaCreateManagedWidget("form", xmFormWidgetClass, Main, NULL);
    Label = XtVaCreateManagedWidget(
        "XmN_OF_MANY", xmLabelWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_FORM, 
	XmNleftOffset,      24,
	XmNtopAttachment,   XmATTACH_FORM, 
	XmNtopOffset,       8, 
	NULL);
    Frame = XtVaCreateManagedWidget(
        "frame1", xmFrameWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_FORM, 
	XmNleftOffset,      20, 
	XmNtopAttachment,   XmATTACH_WIDGET, 
	XmNtopWidget,       Label, 
	XmNrightAttachment, XmATTACH_POSITION, 
	XmNrightPosition,   50,
	XmNrightOffset,     4,  
	NULL);
    RowCol = XtVaCreateManagedWidget(
	"RowCol", xmRowColumnWidgetClass, Frame, 
        NULL);

    Butt3 = XmCreateToggleButtonPlus(RowCol, "I like the OSF", NULL, 0);
    XtVaSetValues(Butt3, XmNsensitive, False, NULL);
    XtManageChild(Butt3);
    Butt4 = XmCreateToggleButtonPlus(RowCol, "Support Freeware!", NULL, 0);
    XtVaSetValues(Butt4, XmNset, True, NULL);
    XtManageChild(Butt4);
    Butt5 = XmCreateToggleButtonPlus(RowCol, "New Widgets", NULL, 0);
    XtManageChild(Butt5);

    Label = XtVaCreateManagedWidget(
        "XmONE_OF_MANY", xmLabelWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_POSITION, 
	XmNleftPosition,    50,
	XmNleftOffset,      4,
	XmNtopAttachment,   XmATTACH_FORM, 
	XmNtopOffset,       8, 
	NULL);
    Frame = XtVaCreateManagedWidget(
        "frame2", xmFrameWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_POSITION, 
	XmNleftPosition,    50,
	XmNleftOffset,      4, 
	XmNtopAttachment,   XmATTACH_WIDGET, 
	XmNtopWidget,       Label, 
	XmNrightAttachment, XmATTACH_FORM, 
	XmNrightOffset,     20,  
	NULL);
    RowCol = XtVaCreateManagedWidget(
	"RowCol", xmRowColumnWidgetClass, Frame, 
	XmNradioBehavior, True,
        XmNradioAlwaysOne,  True,
        NULL);

    Butt3 = XmCreateToggleButtonPlus(RowCol, "Athena", NULL, 0);
    XtVaSetValues(Butt3, XmNindicatorType, XmONE_OF_MANY, NULL);
    XtManageChild(Butt3);
    Butt4 = XmCreateToggleButtonPlus(RowCol, "OSF/Motif(=Hades)", NULL, 0);
    XtVaSetValues(Butt4, XmNindicatorType, XmONE_OF_MANY, NULL);
    XtManageChild(Butt4);
    Butt5 = XmCreateToggleButtonPlus(RowCol, "Harry's Motif Widgets", NULL, 0);
    XtVaSetValues(Butt5, XmNindicatorType, XmONE_OF_MANY, 
                         XmNset, True, NULL);
    XtManageChild(Butt5);

/*    Label = XtVaCreateManagedWidget(
	"XmNtriState = True", 
	xmLabelWidgetClass, Form, 
	XmNleftAttachment,   XmATTACH_FORM, 
	XmNleftOffset,	     20, 
	XmNtopAttachment,    XmATTACH_WIDGET, 
	XmNtopWidget,        Frame, 
	XmNtopOffset,        8, 
	NULL);

    Frame = XtVaCreateManagedWidget(
        "frame3", xmFrameWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_FORM, 
	XmNleftOffset,      20, 
	XmNtopAttachment,   XmATTACH_WIDGET, 
	XmNtopWidget,       Label, 
	XmNrightAttachment, XmATTACH_FORM, 
	XmNrightOffset,     20,  
	NULL);

    RowCol = XtVaCreateManagedWidget(
	"RowCol", xmRowColumnWidgetClass, Frame, 
        NULL);
    Butt3 = XtVaCreateManagedWidget(
	"Click on me several times.", xmToggleButtonWidgetClass, RowCol,
        XmNtriState, True,
        XmNset, XmTOGGLE_DONTKNOW, 
	NULL);
*/	
    Text = XtVaCreateManagedWidget(
	"The new xmToggleButtonWidgetClass", 
	xmLabelWidgetClass, Form, 
	XmNleftAttachment,   XmATTACH_FORM, 
	XmNleftOffset,	     20, 
	XmNrightAttachment,  XmATTACH_FORM, 
	XmNrightOffset,	     20, 
	XmNtopAttachment,    XmATTACH_WIDGET, 
	XmNtopWidget,        Frame, 
	XmNtopOffset,        20, 
	XmNbottomAttachment, XmATTACH_FORM, 
	XmNbottomOffset,     20, 
	NULL);

    XmMainWindowSetAreas(Main, Menu, NULL, NULL, NULL, Form);
    XtRealizeWidget(TopLevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  370,  204, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  370,   31, 0,0,0, /* menu */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  370,  173, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,   24,    8,   70,   17, 0,0,0, /* XmN_OF_MANY */
   CWWidth | CWHeight | CWX | CWY,   20,   25,  161,   91, 0,0,0, /* frame1 */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  157,   87, 0,0,0, /* RowCol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  151,   25, 0,0,0, /* I like the OSF */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  151,   25, 0,0,0, /* Support Freeware! */
   CWWidth | CWHeight | CWX | CWY,    3,   59,  151,   25, 0,0,0, /* New Widgets */
   CWWidth | CWHeight | CWX | CWY,  189,    8,   82,   17, 0,0,0, /* XmONE_OF_MANY */
   CWWidth | CWHeight | CWX | CWY,  189,   25,  161,   91, 0,0,0, /* frame2 */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  157,   87, 0,0,0, /* RowCol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  151,   25, 0,0,0, /* Athena */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  151,   25, 0,0,0, /* OSF/Motif(=Hades) */
   CWWidth | CWHeight | CWX | CWY,    3,   59,  151,   25, 0,0,0, /* Harry's Motif Widgets */
   CWWidth | CWHeight | CWX | CWY,   20,  136,  330,   17, 0,0,0, /* The new xmToggleButtonWidgetClass */
};
    PrintDetails(    TopLevel ,Expected);
};
   LessTifTestMainLoop(    TopLevel );
    return 0;   
} /* main */
