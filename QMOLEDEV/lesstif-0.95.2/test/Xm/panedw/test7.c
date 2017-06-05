/* $Id: test7.c,v 1.7 2001/05/15 14:22:52 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 
#include <Xm/Form.h> 
#include <Xm/DialogS.h> 

#include "../../common/Test.h"


static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

Widget toplevel, field, pane, button;
Widget dialog;
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  492,  361,  102,   25, 0,0,0, /* TopForm */
   CWWidth | CWHeight | CWX | CWY,   10,    0,   36,   25, 0,0,0, /* Bad */
   CWWidth | CWHeight | CWX | CWY,   56,    0,   36,   25, 0,0,0, /* Good */

   CWWidth | CWHeight            ,    8,   24,  366,   98, 0,0,0, /* FD */
   CWWidth | CWHeight            ,    8,   24,  366,   98, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  360,   59, 0,0,0, /* DetailCheckBox */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  354,   25, 0,0,0, /* button_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  354,   25, 0,0,0, /* button_1 */
   CWWidth | CWHeight | CWX | CWY,    3,   70,  360,   25, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   72,   25, 0,0,0, /* bigbutton1 */
   CWWidth | CWHeight | CWX | CWY,  288,    0,   72,   25, 0,0,0, /* bigbutton2 */
   CWWidth | CWHeight | CWX | CWY,  346,   51,   10,   30, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   65,  366,    2, 0,0,0, /* separator */
   CWWidth | CWHeight            ,  360,  325,  366,   98, 0,0,0, /* FD */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  366,   98, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  360,   25, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   72,   25, 0,0,0, /* bigbutton1 */
   CWWidth | CWHeight | CWX | CWY,  288,    0,   72,   25, 0,0,0, /* bigbutton2 */
   CWWidth | CWHeight | CWX | CWY,    3,   36,  360,   59, 0,0,0, /* DetailCheckBox */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  354,   25, 0,0,0, /* button_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  354,   25, 0,0,0, /* button_1 */
   CWWidth | CWHeight | CWX | CWY,  346,   17,   10,   30, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  366,    2, 0,0,0, /* separator */
};

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
}

void makeform(Widget pane)
  {
  Widget Form,b1,b2;

  	Form = XmCreateForm(pane, "Form", NULL, 0);
  	b1 = XmCreatePushButton(Form,"bigbutton1",NULL,0);
  	XtVaSetValues(b1,
  		XmNtopAttachment, XmATTACH_FORM,
  		XmNbottomAttachment, XmATTACH_FORM,
  		XmNleftAttachment, XmATTACH_POSITION,
  		XmNleftPosition, 0,
  		XmNrightAttachment, XmATTACH_POSITION,
  		XmNrightPosition, 20,
  		NULL);
  	XtManageChild(b1);
  	b2 = XmCreatePushButton(Form,"bigbutton2",NULL,0);
  	XtVaSetValues(b2,
  		XmNtopAttachment, XmATTACH_FORM,
  		XmNbottomAttachment, XmATTACH_FORM,
  		XmNleftAttachment, XmATTACH_POSITION,
  		XmNleftPosition, 80,
  		XmNrightAttachment, XmATTACH_POSITION,
  		XmNrightPosition, 100,
  		NULL);
  	XtManageChild(b2);
  	XtManageChild(Form);
  }

void makebox(Widget pane)
  {
  Widget checkbox;

  checkbox = XmVaCreateSimpleCheckBox ( pane,"DetailCheckBox", NULL,
                              XmVaCHECKBUTTON, NULL, NULL, NULL, NULL,
                              XmVaCHECKBUTTON, NULL, NULL, NULL, NULL,
                              NULL );
  XtManageChild (checkbox);
  }


void callback(Widget W, int type)
{
Widget pane;
static int position = 0;

  switch (type)
  {
  case 1:
	  dialog = XmCreateDialogShell(toplevel,"FD",NULL,0);
	  break;
  case 2:
	  dialog = XmCreateFormDialog(toplevel,"FD",NULL,0);
	  /*
	  dialog = XmCreateBulletinBoardDialog(toplevel,"FD",NULL,0);
	  */
	  break;
  }

  pane = XtVaCreateWidget("pane", xmPanedWindowWidgetClass, dialog,
				 XmNsashHeight, 30,
                                 NULL);


  if (position / 2 * 2 == position)
  {
	  makebox(pane);
	  makeform(pane);
	  position = 1;
  }
  else
  {
	  makeform(pane);
	  makebox(pane);
	  position = 0;
  }

  XtManageChild(pane);
  XtManageChild(dialog);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget pb, pb1;
  Widget Form;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,FallBack,NULL);

  Form = XmCreateForm(toplevel,"TopForm",NULL,0);

  pb = XmCreatePushButton(Form,"Bad",NULL,0);
  	XtVaSetValues(pb,
  		XmNtopAttachment, XmATTACH_FORM,
  		XmNbottomAttachment, XmATTACH_FORM,
  		XmNleftAttachment, XmATTACH_POSITION,
  		XmNleftPosition, 10,
  		XmNrightAttachment, XmATTACH_POSITION,
  		XmNrightPosition, 45,
  		NULL);
  XtAddCallback(pb,XmNactivateCallback,(XtCallbackProc)callback,(XtPointer)1);
  XtManageChild(pb);
  pb1 = pb;

  pb = XmCreatePushButton(Form,"Good",NULL,0);
  	XtVaSetValues(pb,
  		XmNtopAttachment, XmATTACH_FORM,
  		XmNbottomAttachment, XmATTACH_FORM,
  		XmNleftAttachment, XmATTACH_POSITION,
  		XmNleftPosition, 55,
  		XmNrightAttachment, XmATTACH_POSITION,
  		XmNrightPosition, 90,
  		NULL);
  XtAddCallback(pb,XmNactivateCallback,(XtCallbackProc)callback,(XtPointer)2);
  XtManageChild(pb);

  XtManageChild(Form);
  XtRealizeWidget(toplevel);
  LessTifTestWaitForIt(toplevel);
  
{
    PrintDetails(toplevel,Expected);
};
  LessTifTestPushButton(pb1);
  
{
    PrintDetails(dialog,Expected);
};
  LessTifTestPushButton(pb);
  
{
    PrintDetails(dialog,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

