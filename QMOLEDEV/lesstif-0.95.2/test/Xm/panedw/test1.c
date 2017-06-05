/* $Header: /cvsroot/lesstif/lesstif/test/Xm/panedw/test1.c,v 1.4 2002/04/17 20:18:35 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PanedWP.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 
#ifdef LESSTIF_REVISION
#include <XmI/XmI.h>
#endif

Widget toplevel, field, pane, button, button2, button3;


#define PANEBOUND 1

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  int i;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass,
                                 toplevel, NULL);

#ifdef LESSTIF_REVISION
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
#endif
  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  pane, 
                                  NULL);  

#if 1
#ifdef LESSTIF_REVISION
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
#endif
  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 40,
#endif
                                   NULL);

#ifdef LESSTIF_REVISION
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
#endif
  button2 = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass,
                                   pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 20,
#endif
                                   NULL);

  XtAddCallback(button2, XmNactivateCallback, activate_callback, NULL);

#if 1
#ifdef LESSTIF_REVISION
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
#endif
  button3 = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass,
                                   pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 40,
#endif
                                   NULL);
  XtAddCallback(button3, XmNactivateCallback, activate_callback, NULL);
#endif

#endif

  XtRealizeWidget(toplevel);

#if 0
  printf("DUMP\n");
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  for (i = 0; i < PW_NumManagedChildren(pane); i++) {
    printf("child: %-8p %-10s ", PW_ManagedChildren(pane)[i],
	   XtName(PW_ManagedChildren(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(PW_ManagedChildren(pane)[i]),
	   PWC_Sash(PW_ManagedChildren(pane)[i]),
	   PWC_PositionIndex(PW_ManagedChildren(pane)[i]),
	   PWC_Position(PW_ManagedChildren(pane)[i]));
  }
  printf("ALL CHILDREN\n");
  for (i = 0; i < MGR_NumChildren(pane); i++) {
    printf("child: %-8p %-10s ", MGR_Children(pane)[i],
	   XtName(MGR_Children(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(MGR_Children(pane)[i]),
	   PWC_Sash(MGR_Children(pane)[i]),
	   PWC_PositionIndex(MGR_Children(pane)[i]),
	   PWC_Position(MGR_Children(pane)[i]));
  }
  printf("\n");

  printf("UNMANAGE\n");
  XtUnmanageChild(button);
  XtUnmanageChild(button2);
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  for (i = 0; i < PW_NumManagedChildren(pane); i++) {
    printf("child: %-8p %-10s ", PW_ManagedChildren(pane)[i],
	   XtName(PW_ManagedChildren(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(PW_ManagedChildren(pane)[i]),
	   PWC_Sash(PW_ManagedChildren(pane)[i]),
	   PWC_PositionIndex(PW_ManagedChildren(pane)[i]),
	   PWC_Position(PW_ManagedChildren(pane)[i]));
  }
  printf("ALL CHILDREN\n");
  for (i = 0; i < MGR_NumChildren(pane); i++) {
    printf("child: %-8p %-10s ", MGR_Children(pane)[i],
	   XtName(MGR_Children(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(MGR_Children(pane)[i]),
	   PWC_Sash(MGR_Children(pane)[i]),
	   PWC_PositionIndex(MGR_Children(pane)[i]),
	   PWC_Position(MGR_Children(pane)[i]));
  }
  printf("\n");

  printf("MANAGE\n");
  XtManageChild(button2);
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  for (i = 0; i < PW_NumManagedChildren(pane); i++) {
    printf("child: %-8p %-10s ", PW_ManagedChildren(pane)[i],
	   XtName(PW_ManagedChildren(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(PW_ManagedChildren(pane)[i]),
	   PWC_Sash(PW_ManagedChildren(pane)[i]),
	   PWC_PositionIndex(PW_ManagedChildren(pane)[i]),
	   PWC_Position(PW_ManagedChildren(pane)[i]));
  }
  printf("ALL CHILDREN\n");
  for (i = 0; i < MGR_NumChildren(pane); i++) {
    printf("child: %-8p %-10s ", MGR_Children(pane)[i],
	   XtName(MGR_Children(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(MGR_Children(pane)[i]),
	   PWC_Sash(MGR_Children(pane)[i]),
	   PWC_PositionIndex(MGR_Children(pane)[i]),
	   PWC_Position(MGR_Children(pane)[i]));
  }
  printf("\n");
#endif

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,  131, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   31, 0,0,0, /* field */
   CWWidth | CWHeight | CWX | CWY,    3,   42,  138,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,   75,  138,   20, 0,0,0, /* button2 */
   CWWidth | CWHeight | CWX | CWY,    3,  103,  138,   25, 0,0,0, /* button3 */
   CWWidth | CWHeight | CWX | CWY,    0,   98,  144,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,  124,   66,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   70,  144,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,  124,   33,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   37,  144,    2, 0,0,0, /* separator */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

