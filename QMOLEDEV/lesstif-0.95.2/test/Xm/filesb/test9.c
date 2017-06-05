/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test9.c,v 1.14 2002/05/01 15:39:21 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/FileSBP.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>

#include "../../common/Test.h"

#include "mkdirtree.h"

XmQualifyProc (*defaultQualifyProc)();
Widget In;
Widget Out;

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

static void
StuffInt(Widget w, int value)
{
char buf[1024];

	sprintf(buf, "%i", value);
	XmTextFieldSetString(w, buf);
}

static void
StuffXmString(Widget w, XmString value)
{
char *buf;

	if (XmStringGetLtoR(value, XmFONTLIST_DEFAULT_TAG, &buf))
	{
	    XmTextFieldSetString(w, buf);
	    XmTextFieldSetInsertionPosition(w, XmTextFieldGetLastPosition(w));
	    printf(" >%s<\n", buf);
	    XtFree(buf);
	}
	else
	{
	    if (value)
	    {
		XmTextFieldSetString(w, "");
	        printf(" ><\n");
	    }
	    else
	    {
		XmTextFieldSetString(w, "NULL");
	        printf(" NULL\n");
	    }
	}
}

static void
StuffValues(Widget w, Widget In, XmFileSelectionBoxCallbackStruct *in)
{
	printf("reason %i\n", in->reason);
	StuffInt(XtNameToWidget(In, "*reason_Text"), in->reason);

	printf("event %i\n", (int)in->event);
	StuffInt(XtNameToWidget(In, "*event_Text"), (int)in->event);

	printf("value %i", in->length);
	StuffXmString(XtNameToWidget(In, "*value_Text"), in->value);
	StuffInt(XtNameToWidget(In, "*length_Text"), in->length);

	printf("mask %i", in->mask_length);
	StuffXmString(XtNameToWidget(In, "*mask_Text"), in->mask);
	StuffInt(XtNameToWidget(In, "*mask_length_Text"), in->mask_length);

	printf("dir %i", in->dir_length);
	StuffXmString(XtNameToWidget(In, "*dir_Text"), in->dir);
	StuffInt(XtNameToWidget(In, "*dir_length_Text"), in->dir_length);

	printf("pattern %i", in->pattern_length);
	StuffInt(XtNameToWidget(In, "*pattern_length_Text"), in->pattern_length);
	StuffXmString(XtNameToWidget(In, "*pattern_Text"), in->pattern);
}

static void
myQualifyProc(Widget w, XmFileSelectionBoxCallbackStruct *in, XmFileSelectionBoxCallbackStruct *out)
{
	printf("------ IN -----\n");
	StuffValues(w, In, in);
	(*defaultQualifyProc)(w, in, out);
	printf("\n------ OUT -----\n");
	StuffValues(w, Out, out);
	printf("\n");
}

static Widget
CreateField(Widget parent, String name, ArgList arglist, Cardinal argcount)
{
Widget Form;
Widget Label;
Widget TextField;
String tmpName;

	tmpName = XtMalloc(strlen(name) + 100);
	sprintf(tmpName, "%s_Form", name);
	Form = XmCreateForm(parent, tmpName, NULL, 0);
	Label = XmCreateLabel(Form, name, NULL, 0);
	sprintf(tmpName, "%s_Text", name);
	TextField = XmCreateTextField(Form, tmpName, arglist, argcount);
	XtVaSetValues(Label,
		XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget, TextField,
		XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNbottomWidget, TextField,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, TextField,
		NULL);
	XtVaSetValues(TextField,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild(Label);
	XtManageChild(TextField);
	XtFree(tmpName);
	return(Form);
}

static Widget
CreateFSBcallbackStruct(Widget parent, String name, ArgList arglist, Cardinal argcount)
{
String *tmp;
String tmpName;
Widget Frame;
Widget Title;
Widget RC;
Widget Field;
String fields[] = {
	"reason",
	"event",
	"value",
	"length",
	"mask",
	"mask_length",
	"dir",
	"dir_length",
	"pattern",
	"pattern_length",
	NULL
};

	tmpName = XtMalloc(strlen(name) + 37);
	Frame = XmCreateFrame(parent, name, arglist, argcount);
	XtVaSetValues(Frame,
		/*XmNshadowType, XmSHADOW_IN,*//* blows up */
		/*XmNshadowType, XmSHADOW_OUT,*//* blows up */
		XmNshadowType, XmSHADOW_ETCHED_IN,
		/*XmNshadowType, XmSHADOW_ETCHED_OUT,*//* blows up */
		XmNshadowThickness, 5,
		NULL);
	sprintf(tmpName, "%sput CallbackStruct", name);
	Title = XmCreateLabel(Frame, tmpName, arglist, argcount);
	XtFree(tmpName);
	XtVaSetValues(Title,
		XmNchildType, XmFRAME_TITLE_CHILD,
		XmNchildHorizontalAlignment, XmALIGNMENT_CENTER,
		NULL);
	RC = XmCreateRowColumn(Frame, name, arglist, argcount);

	tmp = fields;
	while (*tmp)
	{
	    Field = CreateField(RC, *tmp, arglist, argcount);
	    XtManageChild(Field);
	    tmp++;
	};
	XtManageChild(Title);
	XtManageChild(RC);
	return(Frame);
}

static void
QualifyCallback(Widget w)
{
XmFileSelectionBoxCallbackStruct In1;
XmFileSelectionBoxCallbackStruct Out1;
String Text;

	printf("qualify\n");
	Text = XmTextFieldGetString(XtNameToWidget(In, "*reason_Text"));
	In1.reason = atoi(Text); XtFree(Text);
	Text = XmTextFieldGetString(XtNameToWidget(In, "*event_Text"));
	In1.event = (XEvent *)atoi(Text); XtFree(Text);

	Text = XmTextFieldGetString(XtNameToWidget(In, "*value_Text"));
	if (strcmp("NULL", Text) == 0)
	{
	    In1.value = (XmString)NULL;
	}
	else
	{
	    In1.value = XmStringCreateSimple(Text); XtFree(Text);
	}
	Text = XmTextFieldGetString(XtNameToWidget(In, "*length_Text"));
	In1.length = atoi(Text); XtFree(Text);

	Text = XmTextFieldGetString(XtNameToWidget(In, "*mask_Text"));
	if (strcmp("NULL", Text) == 0)
	{
	    In1.mask = (XmString)NULL;
	}
	else
	{
	    In1.mask = XmStringCreateSimple(Text); XtFree(Text);
	}
	Text = XmTextFieldGetString(XtNameToWidget(In, "*mask_length_Text"));
	In1.mask_length = atoi(Text); XtFree(Text);

	Text = XmTextFieldGetString(XtNameToWidget(In, "*dir_Text"));
	if (strcmp("NULL", Text) == 0)
	{
	    In1.dir = (XmString)NULL;
	}
	else
	{
	    In1.dir = XmStringCreateSimple(Text); XtFree(Text);
	}
	Text = XmTextFieldGetString(XtNameToWidget(In, "*dir_length_Text"));
	In1.dir_length = atoi(Text); XtFree(Text);

	Text = XmTextFieldGetString(XtNameToWidget(In, "*pattern_Text"));
	if (strcmp("NULL", Text) == 0)
	{
	    In1.pattern = (XmString)NULL;
	}
	else
	{
	    In1.pattern = XmStringCreateSimple(Text); XtFree(Text);
	}
	Text = XmTextFieldGetString(XtNameToWidget(In, "*pattern_length_Text"));
	In1.pattern_length = atoi(Text); XtFree(Text);

	myQualifyProc(w, &In1, &Out1);
	XmStringFree(In1.value);
	XmStringFree(In1.mask);
	XmStringFree(In1.dir);
	XmStringFree(In1.pattern);
	XmStringFree(Out1.value);
	XmStringFree(Out1.mask);
	XmStringFree(Out1.dir);
	XmStringFree(Out1.pattern);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel;
  Widget Form;
  Widget FSB;
  Widget Qualify;
  Arg args[1];
  Cardinal n;

  make_tmp_dir_tree();
  toplevel = XtVaAppInitialize(&app, "FSBQualifyTest", NULL, 0,
 		               &argc, argv, FallBack, NULL);
  XtVaSetValues(toplevel,
  	XmNallowShellResize, True,
  	NULL);

  Form = XmCreateForm(toplevel, "MainForm", NULL, 0);
  XtVaSetValues(Form,
  	XmNverticalSpacing, 10,
  	XmNhorizontalSpacing, 10,
  	NULL);
  In = CreateFSBcallbackStruct(Form, "In", NULL, 0);
  FSB = XmCreateFileSelectionBox(Form, "FSB", NULL, 0);
  set_path(FSB);
  n = 0;
  XtSetArg(args[n], XmNeditable, False); n++;
  Out = CreateFSBcallbackStruct(Form, "Out", args, n);
  Qualify = XmCreatePushButton(Form, "Qualify", NULL, 0);
  XtAddCallback(Qualify, XmNactivateCallback, (XtCallbackProc)QualifyCallback, NULL);

  XtVaSetValues(Qualify,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_FORM,
  	/*
  	XmNrightAttachment, XmATTACH_WIDGET,
  	XmNrightWidget, FSB,
  	*/
  	NULL);
  XtVaSetValues(In,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, Qualify,
  	NULL);
  XtVaSetValues(Out,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, Qualify,
  	NULL);
  XtVaSetValues(FSB,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, In,
  	XmNrightAttachment, XmATTACH_WIDGET,
  	XmNrightWidget, Out,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);

  XtManageChild(In);
  XtManageChild(Out);
  XtManageChild(FSB);
  XtManageChild(Qualify);
  XtManageChild(Form);

  XtVaGetValues(FSB,
  	XmNqualifySearchDataProc, &defaultQualifyProc,
  	NULL);
  XtVaSetValues(FSB,
  	XmNqualifySearchDataProc, myQualifyProc,
  	NULL);
  XtRealizeWidget(toplevel);
  {
  Dimension width;

  XtVaGetValues(In,
  	XmNwidth, &width,
  	NULL);
  XtVaSetValues(Qualify,
  	XmNwidth, width,
  	NULL);
  }

  {
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  800,  420, 0,0,0, /* MainForm */},
   {CWWidth | CWHeight | CWX | CWY,   10,   10,  242,  365, 0,0,0, /* In */},
   {CWWidth | CWHeight | CWX | CWY,   59,    0,  124,   17, 0,0,0, /* Input CallbackStruct */},
   {CWWidth | CWHeight | CWX | CWY,    5,   17,  232,  343, 0,0,0, /* In */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  226,   31, 0,0,0, /* reason_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* reason */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* reason_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,   37,  226,   31, 0,0,0, /* event_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* event */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* event_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,   71,  226,   31, 0,0,0, /* value_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* value */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* value_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  105,  226,   31, 0,0,0, /* length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* length_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  139,  226,   31, 0,0,0, /* mask_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  173,  226,   31, 0,0,0, /* mask_length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask_length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_length_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  207,  226,   31, 0,0,0, /* dir_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  241,  226,   31, 0,0,0, /* dir_length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir_length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_length_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  275,  226,   31, 0,0,0, /* pattern_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  309,  226,   31, 0,0,0, /* pattern_length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern_length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_length_Text */},
   {CWWidth | CWHeight | CWX | CWY,  262,   10,  276,  400, 0,0,0, /* FSB */},
   {CWWidth | CWHeight | CWX | CWY,  182,   68,   83,   17, 0,0,0, /* Items */},
   {CWWidth | CWHeight | CWX | CWY,  182,   85,   83,  184, 0,0,0, /* ItemsListSW */},
   {CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  165, 0,0,0, /* VertScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,  169,   64,   15, 0,0,0, /* HorScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  165, 0,0,0, /* ItemsList */},
   {CWWidth | CWHeight | CWX | CWY,   10,  279,  256,   17, 0,0,0, /* Selection */},
   {CWWidth | CWHeight | CWX | CWY,   10,  296,  256,   31, 0,0,0, /* Text */},
   {CWWidth | CWHeight | CWX | CWY,    0,  337,  276,    2, 0,0,0, /* Separator */},
   {CWWidth | CWHeight | CWX | CWY,   10,  349,   64,   41, 0,0,0, /* OK */},
   {CWWidth | CWHeight | CWX | CWY,   74,  349,   64,   41, 0,0,0, /* Apply */},
   {CWWidth | CWHeight | CWX | CWY,  138,  349,   64,   41, 0,0,0, /* Cancel */},
   {CWWidth | CWHeight | CWX | CWY,  202,  349,   64,   41, 0,0,0, /* Help */},
   {CWWidth | CWHeight | CWX | CWY,   10,   10,  256,   17, 0,0,0, /* FilterLabel */},
   {CWWidth | CWHeight | CWX | CWY,   10,   68,  162,   17, 0,0,0, /* Dir */},
   {CWWidth | CWHeight | CWX | CWY,   10,   27,  256,   31, 0,0,0, /* FilterText */},
   {CWWidth | CWHeight | CWX | CWY,   10,   85,  162,  184, 0,0,0, /* DirListSW */},
   {CWWidth | CWHeight | CWX | CWY,  147,    0,   15,  165, 0,0,0, /* VertScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,  169,  143,   15, 0,0,0, /* HorScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  143,  165, 0,0,0, /* DirList */},
   {CWWidth | CWHeight | CWX | CWY,  548,   10,  242,  365, 0,0,0, /* Out */},
   {CWWidth | CWHeight | CWX | CWY,   56,    0,  130,   17, 0,0,0, /* Output CallbackStruct */},
   {CWWidth | CWHeight | CWX | CWY,    5,   17,  232,  343, 0,0,0, /* Out */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  226,   31, 0,0,0, /* reason_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* reason */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* reason_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,   37,  226,   31, 0,0,0, /* event_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* event */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* event_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,   71,  226,   31, 0,0,0, /* value_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* value */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* value_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  105,  226,   31, 0,0,0, /* length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* length_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  139,  226,   31, 0,0,0, /* mask_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  173,  226,   31, 0,0,0, /* mask_length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask_length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_length_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  207,  226,   31, 0,0,0, /* dir_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  241,  226,   31, 0,0,0, /* dir_length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir_length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_length_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  275,  226,   31, 0,0,0, /* pattern_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_Text */},
   {CWWidth | CWHeight | CWX | CWY,    3,  309,  226,   31, 0,0,0, /* pattern_length_Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern_length */},
   {CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_length_Text */},
   {CWWidth | CWHeight | CWX | CWY,   10,  385,  242,   25, 0,0,0, /* Qualify */},
};
#else
    static XtWidgetGeometry Expected[] = {
      CWWidth | CWHeight            ,    6,   22,  808,  420, 0,0,0, /* MainForm */
      CWWidth | CWHeight | CWX | CWY,   10,   10,  242,  365, 0,0,0, /* In */
      CWWidth | CWHeight | CWX | CWY,   59,    0,  124,   17, 0,0,0, /* Input CallbackStruct */
      CWWidth | CWHeight | CWX | CWY,    5,   17,  232,  343, 0,0,0, /* In */
      CWWidth | CWHeight | CWX | CWY,    3,    3,  226,   31, 0,0,0, /* reason_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* reason */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* reason_Text */
      CWWidth | CWHeight | CWX | CWY,    3,   37,  226,   31, 0,0,0, /* event_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* event */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* event_Text */
      CWWidth | CWHeight | CWX | CWY,    3,   71,  226,   31, 0,0,0, /* value_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* value */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* value_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  105,  226,   31, 0,0,0, /* length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* length_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  139,  226,   31, 0,0,0, /* mask_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  173,  226,   31, 0,0,0, /* mask_length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask_length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_length_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  207,  226,   31, 0,0,0, /* dir_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  241,  226,   31, 0,0,0, /* dir_length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir_length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_length_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  275,  226,   31, 0,0,0, /* pattern_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  309,  226,   31, 0,0,0, /* pattern_length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern_length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_length_Text */
      CWWidth | CWHeight | CWX | CWY,  262,   10,  284,  400, 0,0,0, /* FSB */
      CWWidth | CWHeight | CWX | CWY,  190,   68,   83,   17, 0,0,0, /* Items */
      CWWidth | CWHeight | CWX | CWY,  190,   85,   83,  182, 0,0,0, /* ItemsListSW */
      CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  163, 0,0,0, /* VertScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,  167,   64,   15, 0,0,0, /* HorScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  163, 0,0,0, /* ItemsList */
      CWWidth | CWHeight | CWX | CWY,   10,  277,  264,   17, 0,0,0, /* Selection */
      CWWidth | CWHeight | CWX | CWY,   10,  294,  264,   31, 0,0,0, /* Text */
      CWWidth | CWHeight | CWX | CWY,    0,  335,  284,    2, 0,0,0, /* Separator */
      CWWidth | CWHeight | CWX | CWY,   10,  347,   66,   43, 0,0,0, /* OK */
      CWWidth | CWHeight | CWX | CWY,   76,  347,   66,   43, 0,0,0, /* Apply */
      CWWidth | CWHeight | CWX | CWY,  142,  347,   66,   43, 0,0,0, /* Cancel */
      CWWidth | CWHeight | CWX | CWY,  208,  347,   66,   43, 0,0,0, /* Help */
      CWWidth | CWHeight | CWX | CWY,   10,   10,  264,   17, 0,0,0, /* FilterLabel */
      CWWidth | CWHeight | CWX | CWY,   10,   68,  170,   17, 0,0,0, /* Dir */
      CWWidth | CWHeight | CWX | CWY,   10,   27,  264,   31, 0,0,0, /* FilterText */
      CWWidth | CWHeight | CWX | CWY,   10,   85,  170,  182, 0,0,0, /* DirListSW */
      CWWidth | CWHeight | CWX | CWY,  155,    0,   15,  163, 0,0,0, /* VertScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,  167,  151,   15, 0,0,0, /* HorScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,    0,  151,  163, 0,0,0, /* DirList */
      CWWidth | CWHeight | CWX | CWY,  556,   10,  242,  365, 0,0,0, /* Out */
      CWWidth | CWHeight | CWX | CWY,   56,    0,  130,   17, 0,0,0, /* Output CallbackStruct */
      CWWidth | CWHeight | CWX | CWY,    5,   17,  232,  343, 0,0,0, /* Out */
      CWWidth | CWHeight | CWX | CWY,    3,    3,  226,   31, 0,0,0, /* reason_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* reason */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* reason_Text */
      CWWidth | CWHeight | CWX | CWY,    3,   37,  226,   31, 0,0,0, /* event_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* event */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* event_Text */
      CWWidth | CWHeight | CWX | CWY,    3,   71,  226,   31, 0,0,0, /* value_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* value */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* value_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  105,  226,   31, 0,0,0, /* length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* length_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  139,  226,   31, 0,0,0, /* mask_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  173,  226,   31, 0,0,0, /* mask_length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* mask_length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* mask_length_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  207,  226,   31, 0,0,0, /* dir_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  241,  226,   31, 0,0,0, /* dir_length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* dir_length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* dir_length_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  275,  226,   31, 0,0,0, /* pattern_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_Text */
      CWWidth | CWHeight | CWX | CWY,    3,  309,  226,   31, 0,0,0, /* pattern_length_Form */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   88,   31, 0,0,0, /* pattern_length */
      CWWidth | CWHeight | CWX | CWY,   88,    0,  138,   31, 0,0,0, /* pattern_length_Text */
      CWWidth | CWHeight | CWX | CWY,   10,  385,  242,   25, 0,0,0, /* Qualify */
};
#endif
    /* toplevel should be replaced with to correct applicationShell */
    PrintDetails(toplevel, Expected);
  }
LessTifTestMainLoop(toplevel);
  exit(0);
}
