/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test9.c,v 1.6 2002/04/17 16:32:01 amai Exp $ */
/*
     To: lesstif@hungry.com
     From: M.Mochol@elka.pw.edu.pl (Codematic)

     Hi,

     I have the lesstif-current (almost new - from a few days ago). What result 
     should be after this:

     #include <Xm.h>

     main()
     {
      XmString s1, s2; 
      s1 = XmStringCreateLtoR("ala ma pieska", "");
      s2 = XmStringCreateLocalized("ala ma pieska"), 
      printf("%d\n", XmStringCompare(s1, s2));
     }


     In Lesstif this program prints 0, original Motif returns 1.
     I think this is because in Lesstif XmStringCompare() uses
     XmStringByteCompare(). So I wrote my own version XmStringCompare().
     I found this bug while having problems with XmSelectionDialog. Below
     are: test program and patch to XmString.c. Also I changed 
     _XmStringGetNextSegment(). When compound string does not contain direction
     component, or component == XmSTRING_DIRECTION_DEFAULTS (0xff)- it returns
     XmSTRING_DIRECTION_L_TO_R (0x0, not 0xff).

     I hope you understand me, my English sucks :(

     greetings
     Codematic
*/


#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/SelectioB.h>

#include "../../common/Test.h"


Widget toplevel;

void AnyCB(Widget w, XtPointer client_data, XtPointer call_data)
{
 /* Lesstif doesn't call this callback */
 printf("AnyCB was called\n");
}

void CreateMySelectionDialog(XmStringTable item_names, int item_num,  
  XtCallbackProc OkCallback)
{
 Widget		dialog;
 int		i;

 dialog = XmCreateSelectionDialog(toplevel, "selection", NULL, 0);
 XtVaSetValues(dialog, XmNmustMatch, True, NULL);  /* !!! */
/* when unmanaged - AnyCB also was not called 
 XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_APPLY_BUTTON));
 XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_SELECTION_LABEL));
 XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_TEXT));
*/

 XtVaSetValues(dialog,
		XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		XmNlistItemCount, item_num,
		XmNlistItems, item_names,
		NULL);

 XtAddCallback(dialog, XmNokCallback, OkCallback, NULL); 
/* XtAddCallback(dialog, XmNcancelCallback, XtDestroyWidget, (XtPointer)0);*/
 for (i = 0 ; i < item_num ; i++) XmStringFree(item_names[i]);
 XtFree((char*)item_names);

 XtManageChild(dialog);
 XtPopup(XtParent(dialog), XtGrabNone);
}


void ChangeWindefTitleParameterCB(Widget w, XtPointer client_data, XtPointer
call_data)
{
 XmStringTable		names;
 int			i;

#define NUMBER 5

 names = (XmString*)XtCalloc(NUMBER, sizeof(XmString));
 for (i = 0 ; i < NUMBER ; i++)
   names[i] = XmStringCreateLocalized("ala ma pieska");
 CreateMySelectionDialog(names, NUMBER, AnyCB);
}

int main(int argc, char **argv)
{
 XtAppContext app;
 Widget one;
 XmString s1, s2;
 int i;
 

 
 toplevel = XtVaAppInitialize(&app, "test", NULL, 0, &argc, argv, NULL, NULL);

 one = XtVaCreateManagedWidget("Push me",
        xmPushButtonWidgetClass,
        toplevel, NULL);
 
 XtAddCallback(one, XmNactivateCallback, ChangeWindefTitleParameterCB, NULL);
 XtRealizeWidget(toplevel);

 s1 = XmStringCreateLtoR("ala ma pieska", "");
 s2 = XmStringCreateLocalized("ala ma pieska");
 i = XmStringCompare(s1, s2);
 printf("%d\n", i); /* on original Motif should be 1 */

 
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   54,   25, 0,0,0, /* Push me */ 
    };
    PrintDetails( toplevel ,Expected);
};
   LessTifTestMainLoop( toplevel );
   
   exit(0);
}
