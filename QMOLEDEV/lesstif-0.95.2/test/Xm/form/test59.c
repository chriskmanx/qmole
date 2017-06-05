/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test59.c,v 1.3 2002/05/01 15:39:21 amai Exp $ */

/*
  SF [ #224084 ] Managing/Unmanaging children in form:

  The accompanying program shows a problem in LessTif 0.91.8 
  (Motif 1.2 variant) which I don't have with OSF/Motif (1.2.4, 2.1.30). 
  In brief, if you click in the menubar, the number of textfields in the form 
  changes (it starts with four fields, and then cycles through 1,2,3,4,1,...).
  In LessTif the textfields aren't displayed properly. The first textfield
  appears OK after the first click, the second one appears truncated after the
  second click. After the third click the second textfield then finally appears
  complete, but the third doesn't.
  At the 4th click, suddenly all fields are there. Etc. 

*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


#define MAX_FIELDS 4

void manage_fields(int number);

XtAppContext appcontext;
Widget toplevel, fields[MAX_FIELDS], labels[MAX_FIELDS], form;
static int current_number_of_fields;

int main(int argc, char *argv[])
{
	Widget mainwindow, menubar, workarea;
	XmString str;

	void callb_menu(Widget widget, XtPointer client_data, XtPointer call_data);
	void create_fields(int number);

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&appcontext, "Test", NULL, 0, &argc, argv,
				     NULL, NULL);

/* Create main window */
	mainwindow = XtVaCreateManagedWidget("mainwindow", xmMainWindowWidgetClass, toplevel,
					     NULL);

/* Create menu */
	str = XmStringCreateLocalized ("Click here");
	menubar = XmVaCreateSimpleMenuBar(mainwindow, "menubar",
					  XmVaCASCADEBUTTON, str, 'C',
					  NULL);
	str = XmStringCreateLocalized ("Change form");
	XmVaCreateSimplePulldownMenu(menubar, "menu", 0, callb_menu,
				     XmVaPUSHBUTTON, str, 'C', NULL, NULL,
				     NULL);
	XtManageChild(menubar);

/* Create work area within main window, which is a form */
	workarea = XtVaCreateWidget("workarea", xmFormWidgetClass, mainwindow,
                                    XmNfractionBase, 100,
                                    NULL);

/* Right side of work area contains another form */
	form = XtVaCreateWidget("form", xmFormWidgetClass, workarea,
				XmNfractionBase,    MAX_FIELDS,
				XmNtopAttachment,   XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				XmNheight,          33*MAX_FIELDS,
				NULL);

/* create several text fields in `form' */
	current_number_of_fields = MAX_FIELDS;
	create_fields(current_number_of_fields);
	
	XtManageChild(form);

	XtManageChild(workarea);

	XtRealizeWidget(toplevel);

    {
/* Note: the following values are the result of
 * querying the current geometry.
 */
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  430,  196,   86,  163, 0,0,0, /* mainwindow */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   86,   31, 0,0,0, /* menubar */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,   76,   21, 0,0,0, /* button_0 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   31,   86,  132, 0,0,0, /* workarea */},
   {CWWidth | CWHeight | CWX | CWY,   40,    0,   46,  132, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   46,   17, 0,0,0, /* field-1 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   33,   46,   17, 0,0,0, /* field-2 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   66,   46,   17, 0,0,0, /* field-3 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   99,   46,   17, 0,0,0, /* field-4 */},

   {CWWidth | CWHeight            ,  430,  196,   86,  163, 0,0,0, /* mainwindow */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   86,   31, 0,0,0, /* menubar */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,   76,   21, 0,0,0, /* button_0 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   31,   86,  132, 0,0,0, /* workarea */},
   {CWWidth | CWHeight | CWX | CWY,   40,    0,   46,  132, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   46,   17, 0,0,0, /* field-1 */},

   {CWWidth | CWHeight            ,  430,  196,   86,  163, 0,0,0, /* mainwindow */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   86,   31, 0,0,0, /* menubar */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,   76,   21, 0,0,0, /* button_0 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   31,   86,  132, 0,0,0, /* workarea */},
   {CWWidth | CWHeight | CWX | CWY,   40,    0,   46,  132, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   46,   17, 0,0,0, /* field-1 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   33,   46,   17, 0,0,0, /* field-2 */},

   {CWWidth | CWHeight            ,  430,  196,   86,  163, 0,0,0, /* mainwindow */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   86,   31, 0,0,0, /* menubar */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,   76,   21, 0,0,0, /* button_0 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   31,   86,  132, 0,0,0, /* workarea */},
   {CWWidth | CWHeight | CWX | CWY,   40,    0,   46,  132, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   46,   17, 0,0,0, /* field-1 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   33,   46,   17, 0,0,0, /* field-2 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   66,   46,   17, 0,0,0, /* field-3 */},

   {CWWidth | CWHeight            ,  430,  196,   86,  163, 0,0,0, /* mainwindow */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   86,   31, 0,0,0, /* menubar */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,   76,   21, 0,0,0, /* button_0 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   31,   86,  132, 0,0,0, /* workarea */},
   {CWWidth | CWHeight | CWX | CWY,   40,    0,   46,  132, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   46,   17, 0,0,0, /* field-1 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   33,   46,   17, 0,0,0, /* field-2 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   66,   46,   17, 0,0,0, /* field-3 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   99,   46,   17, 0,0,0, /* field-4 */},
};
/* toplevel should be replaced with to correct applicationShell */

	PrintDetails(toplevel, Expected);
	LessTifTestWaitForIt(toplevel);
	manage_fields(1);
	PrintDetails(toplevel, Expected);
	LessTifTestWaitForIt(toplevel);
	manage_fields(2);
	PrintDetails(toplevel, Expected);
	LessTifTestWaitForIt(toplevel);
	manage_fields(3);
	PrintDetails(toplevel, Expected);
	LessTifTestWaitForIt(toplevel);
	manage_fields(4);
	PrintDetails(toplevel, Expected);
    }

	/*
	XtAppMainLoop(appcontext);
	*/
	LessTifTestMainLoop(toplevel);

	return(0);
}

void create_fields(int number)
{
	int i;
	char string[10];

	XtUnmanageChild(form);

	sprintf(string, "field-%d", 1);
	fields[0] = XtVaCreateManagedWidget(string, xmLabelWidgetClass, form,
						    XmNtopAttachment,   XmATTACH_POSITION,
						    XmNtopPosition,     0,
					    XmNrightAttachment, XmATTACH_FORM,
					    NULL);
	for(i=1; i<number; i++)
	{
		sprintf(string, "field-%d", i+1);
		fields[i] = XtVaCreateManagedWidget(string, xmLabelWidgetClass, form,
						    XmNtopAttachment,   XmATTACH_POSITION,
						    XmNtopPosition,     i,
						    XmNrightAttachment, XmATTACH_FORM,
						    NULL);
	}
	
	XtManageChild(form);
}

void manage_fields(int number)
{
	int i;

/* unmanage the form */
	XtUnmanageChild(form);

/* unmanage the fields */
	if (number < current_number_of_fields)
	{
		for(i=number; i<current_number_of_fields; i++)
		{
			XtUnmanageChild(fields[i]);
		}
	}
	else
	{
		for(i=current_number_of_fields; i<number; i++)
		{
			XtManageChild(fields[i]);
		}
	}

	current_number_of_fields = number;

	XtManageChild(form);
}

void callb_menu(Widget widget, XtPointer client_data, XtPointer call_data)
{
	static int calls = -1;
	void manage_fields(int number);

	calls++;
	manage_fields((calls%MAX_FIELDS)+1);
}
