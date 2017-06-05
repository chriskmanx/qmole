/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test32.c,v 1.8 2008/04/01 17:52:42 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/DrawingA.h>

void layoutApplication(Widget topLevel);
Widget topLevel, topForm, mainMenu, fileMenu, editMenu;
Widget helpMenu, optionMenu, popMenu, drawA, optionMenu2;


/* callback for option menu */
void drawCB(Widget wid, XtPointer cld, XtPointer cad)
{
}

void createOptionMenu(Widget parent)
{
  XmString lineStr, squareStr, circleStr;
  XmString optLab;

  optLab = XmStringCreateLocalized("Shape : ");
  lineStr = XmStringCreateLocalized("Line");
  squareStr = XmStringCreateLocalized("Square");
  circleStr = XmStringCreateLocalized("Circle");
  optionMenu = XmVaCreateSimpleOptionMenu(parent,
					  "optmenu", optLab, 'D', 0, drawCB,
					  XmVaPUSHBUTTON, lineStr, (KeySym)'L', 0, 0,
					  XmVaPUSHBUTTON, squareStr, (KeySym)'S', 0, 0,
					  XmVaPUSHBUTTON, circleStr, (KeySym)'C', 0, 0,
					  0);
  XmStringFree(optLab);
  XmStringFree(lineStr);
  XmStringFree(squareStr);
  XmStringFree(circleStr);
  XtManageChild(optionMenu);

  XtVaSetValues(optionMenu,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, mainMenu,
		NULL);
}

int main(int argc, char *argv[])
{
  XtAppContext appContext;

  topLevel = XtAppInitialize(&appContext, "Menus", 0, 0,
					  &argc, argv, 0, 0, 0);

  layoutApplication(topLevel);
  XtRealizeWidget(topLevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	226,	35,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	134,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	52,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	58,	3,	73,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	134,	0,	92,	35,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	4,	29,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	10,	3,	79,	29,	0,0,0,
};

  PrintDetails(topLevel, Expected);
  }
    LessTifTestMainLoop(topLevel);
    /*
  XtAppMainLoop(appContext);
  */
  return 0;
}


Widget GXCreateOptionMenu(Widget parent, char **entries)
{
  Arg args[10];
  XmString *buttonArray;
  char **cptr;
  int cnt,i,n;
  Widget opt,pulldown;


  cptr=entries;
  cnt=0;
  do {
    cnt++;
    cptr++;    
  } while (*cptr);

  if (!(buttonArray=(XmString*)malloc(cnt*sizeof(XmString)))) {
    return NULL;
  }

  /* create the menu */
  for (i=0;i<cnt;i++)
    buttonArray[i]=XmStringCreateSimple(entries[i]);
  n=0;
  XtSetArg(args[n], XmNbuttonCount, cnt);  n++;
  XtSetArg(args[n], XmNbuttons, buttonArray); n++;
  pulldown = XmCreateSimplePulldownMenu(parent,"OptionMenuPulldown",args,n);
  for (i=0;i<cnt;i++)
    XmStringFree(buttonArray[i]);
  free(buttonArray);

  n=0;
  XtSetArg(args[n], XmNsubMenuId, pulldown);  n++;
  opt=XmCreateOptionMenu(parent,"OptionMenu",args,n);
  XtManageChild(opt);
  return opt;
}



void layoutApplication(Widget topLevel)
{
  char *entries[]={"Entry 1", "Entry 2", "Entry 3", NULL};
  /* note, later this will be a list of queries */
  topForm = XtVaCreateManagedWidget("topForm",
				    xmFormWidgetClass, topLevel,
				    NULL);
  
  createOptionMenu(topForm);

  optionMenu2=GXCreateOptionMenu(topForm,entries);
  /* handle it */
  XtManageChild(optionMenu2);
  XtVaSetValues(optionMenu2,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, mainMenu,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, optionMenu,
		NULL);

}
