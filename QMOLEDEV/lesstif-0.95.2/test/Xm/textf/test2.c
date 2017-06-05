#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 

Widget toplevel, field, form, button;

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmFontList	fl = NULL;
	XFontStruct	*f = NULL;
	Arg		al[1];

	f = XLoadQueryFont(XtDisplay(w), "-*-courier-*-o-*--24-*");
	if (f)
		fl = XmFontListCreate(f, XmSTRING_DEFAULT_CHARSET);
	else
		fprintf(stderr, "XLoadQueryFont => NULL\n");

	if (fl) {
		XtSetArg(al[0], XmNfontList, fl);
		XtSetValues(field, al, 1);
		XmFontListFree(fl);
	} else
		fprintf(stderr, "XmFontList => NULL\n");

	XmTextFieldSetString(field, "Hello");
}

void LoseFocus(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "LoseFocus\n");
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass,
                                 toplevel, NULL);

  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  form, 
                                  XmNleftAttachment, XmATTACH_FORM,
                                  XmNrightAttachment, XmATTACH_FORM,
                                  XmNtopAttachment, XmATTACH_FORM,
                                  XmNbottomAttachment, XmATTACH_NONE,
                                  NULL);  
  XtAddCallback(field, XmNlosingFocusCallback, LoseFocus, NULL);

  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   form, 
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget, field,
                                   XmNbottomAttachment, XmATTACH_FORM,
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, activate_callback, NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  138,   56, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  138,   31, 0,0,0, /* field */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  138,   25, 0,0,0, /* button */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
