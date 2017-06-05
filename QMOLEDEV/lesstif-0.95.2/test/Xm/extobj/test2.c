/* $Header: /cvsroot/lesstif/lesstif/test/Xm/extobj/test2.c,v 1.3 2002/05/03 12:03:41 amai Exp $ */
/*
 * exttest.c -- This is for playing around with LessTif's or M*tif's
 *              extension objects and the grab mechanism. Just start it
 *              and use the File menu and watch your terminal window as
 *              you let your mouse select different menu entries.
 * Written by Harald Albrecht, albrecht@igpm.rwth-aachen.de
 */
 

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/DesktopP.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>
#include <Xm/ShellEP.h>
#include <Xm/VendorSEP.h>
#include <Xm/DialogSP.h>
#include <Xm/DialogSEP.h>
#include <Xm/VendorSP.h>

#include "../../common/Test.h"


char TreeBuffer[512];

void DumpExtObj(Widget w)
{
    String name, type, cname;
    Widget lp;
    char   xbuff[64];

    name = XtName(((XmExtRec *) w)->ext.logicalParent);
    if ( name == NULL ) {
        sprintf(xbuff, "<%08X>", (int)((XmExtRec *) w)->ext.logicalParent);
	name = xbuff;      
    }
    lp = ((XmExtRec *) w)->ext.logicalParent;
    printf("%s[logical parent = %s (%sclass = %s)]\n",
	   TreeBuffer, name,
	   XmIsExtObject(lp) ? "ExtObj " : "",
	   lp->core.widget_class->core_class.class_name);
    switch ( ((XmExtRec *) w)->ext.extensionType ) {
    case XmCACHE_EXTENSION:
        type = "cache extension"; break;
    case XmDESKTOP_EXTENSION:
        type = "desktop extension"; break;
    case XmSHELL_EXTENSION:
        type = "shell extension"; break;
    case XmPROTOCOL_EXTENSION:
        type = "protocol extension"; break;
    case XmDEFAULT_EXTENSION:
        type = "default extension"; break;
    default:
        type = "unknown";
    }
    printf("%s[extension type = %s]\n", TreeBuffer, type);
    if ( XtParent(w) ) {
        name = XtName(XtParent(w));
	if ( name == NULL ) {
	    sprintf(xbuff, "<%08X>", (int) XtParent(w));
	    name = xbuff;      
	}
	cname = w->core.widget_class->core_class.class_name;
    } else {
        name = "(NULL parent)"; cname = "";
    }
    printf("%s[physical parent = %s (class = %s)]\n", TreeBuffer, name, cname);
    switch ( ((XmExtRec *) w)->ext.extensionType ) {
    case XmSHELL_EXTENSION:
        printf("%s[useAsyncGeometry: %s, lastConfigureRequest: %i]\n",
	       TreeBuffer,
	       ((XmShellExtRec *) w)->shell.useAsyncGeometry ? "Yes" : "No",
	       (int)((XmShellExtRec *) w)->shell.lastConfigureRequest);
        break;
    }
    if ( XmIsDesktopObject(w) ) {
        if ( ((XmDesktopRec *) w)->desktop.parent ) {
            name = XtName(((XmDesktopRec *) w)->desktop.parent);
	    if ( name == NULL ) {
	        sprintf(xbuff, "<%08X>", (int)((XmDesktopRec *) w)->desktop.parent);
		name = xbuff;
	    }
	} else {
	    name = "(NULL parent)";
	}
        printf("%s[(desktop) parent: %s]\n",
	       TreeBuffer, name);
    }
    if ( XmIsVendorShellExt(w) ) {
        printf("%s[(vendorshellext)]\n",
	       TreeBuffer);
    }
    if ( XmIsDialogShellExt(w) ) {
        printf("%s[(dialogshellext)]\n",
	       TreeBuffer);
    }
} /* DumpExtObj */

void DumpW(Widget w, String Prefix, String SubPrefix, String EveryPrefix)
{
    String name;
    char   *p;
    char   xbuff[64];

    p = TreeBuffer + strlen(TreeBuffer);
    strcpy(p, Prefix);
    if ( w ) {
        name = XtName(w);
	if ( name == NULL ) {
	    sprintf(xbuff, "<%08X>", (int)(char *)w);
	    name = xbuff;      
	}
	printf("%s%s <%08X> (%sclass %s)\n", TreeBuffer,
	       name ? name : "(no name)",
	       (int) w,
	       XmIsExtObject(w) ? "ExtObj " : "",
	       w->core.widget_class->core_class.class_name);
	strcpy(p, SubPrefix);
	strcat(p, EveryPrefix);
	if ( XmIsExtObject(w) ) {
	    DumpExtObj(w);
	}
    } else {
        printf("%s(NULL widget)\n", TreeBuffer);
    }
    *p = 0;
} /* DumpW */

void DumpWidgetLevel(Widget w, String Prefix, String SubPrefix)
{
    char   *p;
    Widget *child, *children, lp;
    int     i, NumPopupChildren, NumObjChildren, NumChildren;
    String  name;
    
    if ( XtIsWidget(w) ) {
        NumPopupChildren = w->core.num_popups;
    } else {
        NumPopupChildren = 0;
    }
    if ( _XmIsFastSubclass(XtClass(w), XmSCREEN_BIT ) ) {
        children       = ((XmScreenRec *) w)->desktop.children;
        NumObjChildren = ((XmScreenRec *) w)->desktop.num_children;
    } else if ( XmIsDesktopObject(w) ) {
        children       = ((XmDesktopRec *) w)->desktop.children;
        NumObjChildren = ((XmDesktopRec *) w)->desktop.num_children;
    } else {
        NumObjChildren = 0;
    }
    if ( XtIsComposite(w) ) {
        NumChildren = ((CompositeRec *) w)->composite.num_children;
    } else {
        NumChildren = 0;
    }

    p = TreeBuffer + strlen(TreeBuffer);
    DumpW(w, Prefix, SubPrefix, 
	  (NumChildren || NumObjChildren || NumPopupChildren) ? "| " : "  ");
    strcpy(p, SubPrefix);

    /*
     * First dump all ordinary children...
     */
    if ( XtIsComposite(w) ) {
        child = ((CompositeRec *) w)->composite.children;
	for ( i = NumChildren; i > 0; i-- ) {
	    if ( (i != 1) || NumPopupChildren || NumObjChildren ) {
		DumpWidgetLevel(*child, "+-- ", "|   ");
	    } else {
		DumpWidgetLevel(*child, "`-- ", "    ");
	    }
	    child++;
	}
    }
    /*
     * ...then dump all popup children...
     */
    child = w->core.popup_list;
    for ( i = NumPopupChildren; i > 0; i-- ) {
        if ( (i != 1) ) {
	    DumpWidgetLevel(*child, "+-- (POPUP CHILD) ", "|   ");
	} else {
	    DumpWidgetLevel(*child, "`-- (POPUP CHILD) ", "    ");
	}
	child++;
    }
    /*
     * ...and finally the illegimate children...
     */
    child = children;
    for ( i = NumObjChildren; i > 0; i-- ) {
        if ( (i != 1) ) {
	    DumpWidgetLevel(*child, "+-- ", "|   ");
	} else {
	    DumpWidgetLevel(*child, "`-- ", "    ");
	}
	child++;
    }
    *p = 0;
} /* DumpWidgetLevel */

void DumpWidgetTree(Widget w)
{
    printf("\n*** Widget Instance Hierarchy ***\n");
    DumpWidgetLevel(w, "", "");
} /* DumpWidgetTree */

void DumpDisplayTree(Widget w)
{
    w = XmGetXmDisplay(XtDisplayOfObject(w));
    printf("\n*** Display Widget Instance Hierarchy ***\n");
    DumpWidgetLevel(w, "", "");
} /* DumpDisplayTree */


void DumpGrabList(Widget w)
{
    XmDisplay    wd;
    XmModalData  modal;
    int          i;

    wd = (XmDisplay) XmGetXmDisplay(XtDisplayOfObject(w));
    printf("\n*** Grab List Dump ***\n");
    modal = wd->display.modals;
    printf("%i entries in grab list\n", wd->display.numModals);
    for ( i = 0; i < wd->display.numModals; i++ ) {
        printf("Entry #%i ----------\n", (int)(i + 1));
	DumpW((Widget) modal->wid,     "  wid     = ", "            ", "  ");
	DumpW((Widget) modal->ve,      "  ve      = ", "            ", "  ");
	DumpW((Widget) modal->grabber, "  grabber = ", "            ", "  ");
	printf("  exclusive = %s, spring loaded = %s\n",
	       modal->exclusive ? "Yes" : "No",
	       modal->springLoaded ? "Yes" : "No");
	modal++;
    }
} /* DumpGrabList */



void DumpCB(Widget w, XtPointer ClientData, XtPointer CBData)
{
    printf("\n****** Dump Callback Triggered ******");
    DumpDisplayTree(w);
    DumpGrabList(w);
} /* DumpCB */

void DestroyDialogCB(Widget w, XtPointer ClientData, XtPointer CBData)
{
    XtDestroyWidget((Widget) ClientData);
} /* DestroyDialogCB */

void DoDialogCB(Widget w, XtPointer ClientData, XtPointer CBData)
{
    Widget  sh, dsh, Dlg, ok, top;
    String  name;
    int     counter;
    Boolean DoCascade;

    counter = 1; DoCascade = False;
    switch ( (unsigned char)(unsigned int) ClientData ) {
    case 44:
	counter = 2;
	ClientData = (XtPointer) XmDIALOG_APPLICATION_MODAL;
    case XmDIALOG_APPLICATION_MODAL:
        name = "application_modal";
	break;
    case XmDIALOG_FULL_APPLICATION_MODAL:
        name = "full_application_modal";
	break;
    case 43:
        DoCascade = True;
    case 42:
	counter = 2;
	ClientData = (XtPointer) XmDIALOG_MODELESS;
    case XmDIALOG_MODELESS:
        name = "modeless";
	break;
    }
    printf("\n****** Creating %s Dialog ******", name);

    sh = w;
    while ( XtParent(sh) ) {
        sh = XtParent(sh);
    }
    for ( ; counter ; counter-- ) {
#if 0
        Dlg = XmCreateFormDialog(sh, name, NULL, 0);
#else
        dsh = XtVaCreateWidget(name, xmDialogShellWidgetClass, sh, NULL);
        Dlg = XtVaCreateWidget(name, xmFormWidgetClass, dsh, NULL);
#endif
	ok = XmCreatePushButton(Dlg, "Close", NULL, 0);
	XtVaSetValues(ok,
		      XmNleftAttachment, XmATTACH_FORM,
		      XmNrightAttachment, XmATTACH_FORM,
		      XmNtopAttachment, XmATTACH_FORM,
		      XmNbottomAttachment, XmATTACH_FORM,
		      NULL);
	XtAddCallback(ok, XmNactivateCallback, DestroyDialogCB, 
		      (XtPointer) XtParent(Dlg));
	XtManageChild(ok);
	XtVaSetValues(Dlg,
		      XmNdialogStyle, (unsigned char)(unsigned int) ClientData,
		      XmNdefaultButton, ok,
		      NULL);
	XtManageChild(Dlg);
	if ( DoCascade ) {
	    sh = Dlg;
	}
    }

/*
    top = w;
    while ( XtParent(top) != NULL ) {
        top = XtParent(top);
    }
    DumpWidgetTree(top); */
    DumpDisplayTree(w);
    DumpGrabList(w);
} /* DoDialogCB */

void CreateMainW(Widget w, String name)
{
    Widget Form, MenuBar, Label, FileCascade, FileMenu;
    Widget Cascade, CascadeMenu, wch;

    Form = XmCreateForm(w, name, NULL, 0);
    MenuBar = XmCreateMenuBar(Form, "menubar", NULL, 0);
    XtVaSetValues(MenuBar, 
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    Label = XmCreateLabel(Form, "ExtTest Test Application", NULL, 0);
    XtVaSetValues(Label,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, MenuBar,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    FileCascade = XmCreateCascadeButton(MenuBar, "File", NULL, 0);
    FileMenu    = XmCreatePulldownMenu(MenuBar, "filemenu", NULL, 0);
    XtVaSetValues(FileCascade, XmNsubMenuId, FileMenu, NULL);

    Cascade = XmCreateCascadeButton(FileMenu, "Cascade", NULL, 0);
    XtManageChild(Cascade);

    XtManageChild(wch = XmCreatePushButton(FileMenu, "Modeless Dialog", NULL, 0));
    XtAddCallback(wch, XmNactivateCallback, DoDialogCB, (XtPointer) XmDIALOG_MODELESS);
    XtManageChild(wch = XmCreatePushButton(FileMenu, "Two Modeless Dialogs", NULL, 0));
    XtAddCallback(wch, XmNactivateCallback, DoDialogCB, (XtPointer) 42);
    XtManageChild(wch = XmCreatePushButton(FileMenu, "Two Modeless Dialogs Cascaded", NULL, 0));
    XtAddCallback(wch, XmNactivateCallback, DoDialogCB, (XtPointer) 43);
    XtManageChild(wch = XmCreatePushButton(FileMenu, "Application Modal Dialog", NULL, 0));
    XtAddCallback(wch, XmNactivateCallback, DoDialogCB, (XtPointer) XmDIALOG_APPLICATION_MODAL);
    XtManageChild(wch = XmCreatePushButton(FileMenu, "Two Application Modal Dialogs", NULL, 0));
    XtAddCallback(wch, XmNactivateCallback, DoDialogCB, (XtPointer) 44);
    XtManageChild(wch = XmCreatePushButton(FileMenu, "Full Application Modal Dialog", NULL, 0));
    XtAddCallback(wch, XmNactivateCallback, DoDialogCB, (XtPointer) XmDIALOG_FULL_APPLICATION_MODAL);

    XtManageChild(XmCreatePushButton(FileMenu, "Quit", NULL, 0));

    CascadeMenu = XmCreatePulldownMenu(FileMenu, "cascademenu", NULL, 0);
    XtVaSetValues(Cascade, XmNsubMenuId, CascadeMenu, NULL);
    XtManageChild(XmCreatePushButton(CascadeMenu, "Test1", NULL, 0));
    XtManageChild(wch = XmCreatePushButton(CascadeMenu, "Test2", NULL, 0));
    XtAddCallback(wch, XmNarmCallback, DumpCB, (XtPointer) NULL);

    XtManageChild(FileCascade);
    XtManageChild(MenuBar);
    XtManageChild(Label);
    XtManageChild(Form);
} /* CreateMainW */


int main(int argc, char **argv)
{
    XtAppContext    AppCtx;
    Widget          TopLevel, TopLevel2;
    XmBaseClassExt *bce;

    TopLevel = XtAppInitialize(&AppCtx, "Exttest", NULL, 0,
                               &argc, argv, NULL, NULL, 0);
    CreateMainW(TopLevel, "main_first");
    
    bce = _XmGetBaseClassExtPtr(XtClass(TopLevel), XmQmotif);
    printf("bce: %08X (sec obj create = %08X)\n\n",
           (int) (*bce),
           (int) ((*bce)->secondaryObjectCreate)
           );

    TopLevel2 = XtAppCreateShell("exttest2", "Exttest", 
				 applicationShellWidgetClass, 
				 XtDisplay(TopLevel),
				 NULL, 0);
    XtVaSetValues(TopLevel, XmNx, 500, NULL);
    CreateMainW(TopLevel2, "main_second");

    XtRealizeWidget(TopLevel); XtRealizeWidget(TopLevel2);
    DumpWidgetTree(TopLevel);
    DumpDisplayTree(TopLevel);
    DumpGrabList(TopLevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  148,   48, 0,0,0, /* main_first */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  148,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  148,   17, 0,0,0, /* ExtTest Test Application */ 
    };
    PrintDetails(    TopLevel ,Expected);
};
   LessTifTestMainLoop(    TopLevel );
   
   exit(0);
} /* main */

/* End of exttest.c */
