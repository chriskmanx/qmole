/*
$Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test10.c,v 1.4 2002/04/01 13:36:36 amai Exp $
From:        Dr Keith Distin <kdistin@msxi.co.uk>
To:          lesstif@hungry.com
Subject:     bug/differeces with Motif
Date:        Fri, 20 Nov 1998 11:31:47 +0000
*/

#include <Xm/Xm.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

         
void pushCB(Widget w, XtPointer clientData, XtPointer callData)
{
     Widget x = (Widget) clientData;
     Arg args[5];    
     int nargs=0;
     
     XtSetArg(args[0], XmNeditable, True); nargs++;
     XtSetValues(x, args, nargs);
     
     /* the following works */
     /* XmTextFieldSetEditable(x, True); */
     
     return;
}        
  
int main(int argc, char** argv)
{    
    Widget toplevel, textf, rc, pushb;    
    XtAppContext app_context;    
    Arg args[5];    
    int nargs=0;
    
    
            
    toplevel = XtAppInitialize(&app_context, "TextFieldTest", NULL,
             0, &argc, argv, NULL, NULL, 0);
    
    
    rc = XmCreateRowColumn(toplevel, "rc", args, nargs);
    pushb = XmCreatePushButton(rc, "Push Me", args, nargs);
    nargs = 0;
    XtSetArg(args[0], XmNeditable, False); nargs++;
    textf = XmCreateTextField(rc, "textf", args, nargs);
    XtAddCallback(pushb, XmNactivateCallback, pushCB, (XtPointer) textf);
    XtManageChild(rc);
    XtManageChild(pushb);
    XtManageChild(textf);
    
    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,   65, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   25, 0,0,0, /* Push Me */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  138,   31, 0,0,0, /* textf */ 
    };
    PrintDetails(toplevel,Expected);
};
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(app_context);
    */
    exit(0);
}
