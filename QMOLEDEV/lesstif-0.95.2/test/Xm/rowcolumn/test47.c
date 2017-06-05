/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test47.c,v 1.4 2001/05/15 14:46:10 amai Exp $
From:        Dr Keith Distin <kdistin@msxi.co.uk>
To:          lesstif@hungry.com
Subject:     bug/differeces with Motif
Date:        Fri, 20 Nov 1998 11:31:47 +0000
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>

         
void cancelCB(Widget w, XtPointer clientData, XtPointer callData)
{
  
    XtUnmanageChild(XtParent(XtParent(w)));
}

void pushCB(Widget w, XtPointer clientData, XtPointer callData)
{
    int i;
    char tmp[10];
    static Widget dialog = NULL, rc;
    Widget label[10], pb;
    static int nColumns = 2;
    
    if (!dialog) {
        dialog = (Widget) XmCreateFormDialog(w, "rc_test", NULL, 0);
        
        rc = XtVaCreateManagedWidget("rc1", xmRowColumnWidgetClass, dialog, 
                XmNorientation, XmVERTICAL,
                XmNpacking, XmPACK_COLUMN,
                NULL, 0);
        
        for (i = 0; i < 9; i++) {
            sprintf(tmp, "Label %d", i);
            label[i] = XtCreateManagedWidget(tmp, xmLabelWidgetClass, rc, NULL, 0); 
        }
        
        pb = XtCreateManagedWidget("Close", xmPushButtonWidgetClass, rc, NULL, 0); 
        XtAddCallback(pb, XmNactivateCallback, cancelCB, NULL);
    }     
    
    nColumns = (nColumns == 2) ? 1 : 2;
    XtVaSetValues ( rc,
            XmNnumColumns, nColumns,
            NULL);
    
    XtManageChild (dialog);
    
    return;
}        

int
main(int argc, char** argv)
{    
    Widget toplevel, pushb;    
    XtAppContext app_context;    
            
    toplevel = XtAppInitialize(&app_context, "RowColumnTest", NULL,
             0, &argc, argv, NULL, NULL, 0);
    
    pushb = XmCreatePushButton(toplevel, "Push Me", NULL, 0);
    XtAddCallback(pushb, XmNactivateCallback, pushCB, NULL);
    XtManageChild(pushb);
    
    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   54,   25, 0,0,0, /* Push Me */ 
    };
    PrintDetails(toplevel,Expected);
};
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(app_context);
    */
    exit(0);
}
