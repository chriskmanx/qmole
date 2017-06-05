/* $Header: /cvsroot/lesstif/lesstif/test/Xm/panedw/test8.c,v 1.7 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PanedWP.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 
#ifdef LESSTIF_REVISION
#include <XmI/XmI.h>
#endif

#include "../../common/Test.h"


Widget toplevel, field, pane, button, button2, button3;

#if XmVERSION < 2
int main()
{
  printf("This test only works well with a Motif 2.x compatible library\n");
  exit(0);
}
#else

#define PANEBOUND 1

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
}

void reorient(Widget w, XtPointer client, XtPointer call)
{
	static int o = XmVERTICAL;

	if (o == XmVERTICAL)
		o = XmHORIZONTAL;
	else
		o = XmVERTICAL;

	XtVaSetValues(pane, XmNorientation, o, NULL);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  int i;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0,
                               &argc, argv, NULL,
			       NULL);

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass,
                                 toplevel, NULL);

#ifdef LESSTIF_REVISION
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
#endif
  field = XtVaCreateManagedWidget("field",xmPushButtonWidgetClass,
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

  XtAddCallback(button, XmNactivateCallback, reorient, NULL);

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
  
{
    static XtWidgetGeometry Expected[] = {
      NULL
    };
    PrintDetails(toplevel,Expected);
};

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
#endif /* #if 0 */

  LessTifTestMainLoop(toplevel);

  exit(0);
}

#endif
