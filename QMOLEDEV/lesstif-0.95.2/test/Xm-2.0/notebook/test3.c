/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/notebook/test3.c,v 1.2 2001/06/18 14:49:16 amai Exp $ */
/* from
 *  http://techpubs.sgi.com:80/library/dynaweb_bin/ebt-bin/0650/nph-infosrch.cgi/infosrchtpl/SGI_Developer/Motif_Port_G/@InfoSearch__BookTextView/1094;he=0?DwebQuery=motif
 */   

#include <stdlib.h>
#include <stdio.h>

/* include init */
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MenuShell.h>
#include <Xm/Notebook.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"

 
#define NUM_PAGES 8
#define APPNAME "Notebook"
 
 
static void pageChangeCB(Widget w, XtPointer client, XtPointer call);
 
/* end init */
 
 
int
main(int argc, char ** argv)
{
    XtAppContext AppContext;    
    Widget     TopLevel = XtVaOpenApplication(&AppContext, 
                               APPNAME, 
                               NULL, 0, 
                               &argc, argv, 
                               NULL,
                               sessionShellWidgetClass,
                               NULL);
 
/* include notebook */    
    Widget notebook = XtVaCreateWidget("notebook",
                               xmNotebookWidgetClass,
                               TopLevel,
                               NULL);
    int i;
    XmString xmstr;
    char    labelString[32];
 
    for ( i=0; i< NUM_PAGES; i++ ) {
 
        sprintf(labelString,"Page %d\n",i);
        xmstr = XmStringCreateLocalized(labelString);
 
        (void) XtVaCreateManagedWidget("label", xmLabelWidgetClass,
                        notebook,
                        XmNpageNumber, i+1,
                        XmNlabelString, xmstr,
                        NULL);
 
        XmStringFree(xmstr);
    }
 
    for ( i=0; i< NUM_PAGES; i+=4 ) {
        int j;
 
        sprintf(labelString,"Major\nTab %d\n",i);
        xmstr = XmStringCreateLocalized(labelString);
 
        (void) XtVaCreateManagedWidget("button",
                        xmPushButtonWidgetClass,
                        notebook,
                        XmNpageNumber, i+1,
                        XmNlabelString, xmstr,
                        XmNnotebookChildType,
                        XmMAJOR_TAB,
                        NULL);
        XmStringFree(xmstr);
 
        for ( j=i; j<i+4; j++ ) {
        
            sprintf(labelString,"Minor\nTab %d\n",j);
            xmstr = XmStringCreateLocalized(labelString);
 
            (void) XtVaCreateManagedWidget("button",
                            xmPushButtonWidgetClass,
                            notebook,
                            XmNpageNumber, j+1,
                            XmNlabelString, xmstr,
                            XmNnotebookChildType,
                            XmMINOR_TAB,
                            NULL);
            XmStringFree(xmstr);
        }
    }
 
    XtAddCallback(notebook, XmNpageChangedCallback,
                  (XtCallbackProc) pageChangeCB, NULL);
 
    XtManageChild(notebook);
/* end notebook */
 
    XtRealizeWidget(TopLevel);
 
    LessTifTestMainLoop(TopLevel);
 
}
 
 
/* pageChangeCB - comment */
static void
pageChangeCB(Widget w, XtPointer client, XtPointer call)
{
 
}
