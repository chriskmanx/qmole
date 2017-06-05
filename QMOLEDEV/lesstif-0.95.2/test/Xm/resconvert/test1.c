/* $Header: /cvsroot/lesstif/lesstif/test/Xm/resconvert/test1.c,v 1.3 2002/04/13 12:05:29 amai Exp $ */

#include <stdlib.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Label.h>


static          XmString
string_append(s1, s2)
	XmString        s1;
	XmString        s2;
{
	XmString        s3;
	if (s1 == NULL)
		return NULL;
	s3 = XmStringConcat(s1, s2);
	XmStringFree(s1);
	XmStringFree(s2);
	return s3;
}

static          Boolean
string_to_xmstring(display, args, num_args, from_value,
		   to_value, converter_data)
	Display        *display;
	XrmValue       *args;
	Cardinal       *num_args;
	XrmValue       *from_value;
	XrmValue       *to_value;
	XtPointer      *converter_data;
{
	static XmString string;
	char           *instring;
	char           *p;
	XmStringDirection dir = XmSTRING_DIRECTION_L_TO_R;
	/* Complain if nothing to convert */
	if (from_value->addr == NULL) {
		XtDisplayStringConversionWarning(display, from_value->addr,
						 XmRXmString);
		return False;
	}
	/*
	 * If the caller has allocated storage for the converted value, check
	 * that it is big enough
	 */
	if ((to_value->addr != NULL) && (to_value->size < sizeof(XmString))) {
		to_value->size = sizeof(XmString);
		return False;
	}
	/*
	 * Take a copy of the from_value, parse it into segments and create
	 * the compound string
	 */
	string = XmStringCreateLtoR("", XmFONTLIST_DEFAULT_TAG);
	instring = XtNewString(from_value->addr);
	if (instring != NULL) {
		for (p = strtok(instring, "#"); p != NULL; p = strtok(NULL, "#")) {
			string = string_append(string,
					       XmStringCreateLocalized(p));
			if (dir == XmSTRING_DIRECTION_L_TO_R)
				dir = XmSTRING_DIRECTION_R_TO_L;
			else
				dir = XmSTRING_DIRECTION_L_TO_R;
			string = string_append(string, XmStringDirectionCreate(dir));
		}
		XtFree(instring);
	}
	/* Failed to create compound string... */
	if (string == NULL) {
		XtDisplayStringConversionWarning(display, from_value->addr,
						 XmRXmString);
		return False;
	}
	/* Successful conversion, return value to caller */
	if (to_value->addr)
		*(XmString *) to_value->addr = string;
	else
		to_value->addr = (XPointer) & string;
	to_value->size = sizeof(XmString);
	return True;
}

static void
string_to_xmstring_destroy(app, to_value, converter_data, args,
			   num_args)
	XtAppContext    app;
	XrmValue       *to_value;
	XtPointer       converter_data;
	XrmValue       *args;
	Cardinal       *num_args;
{
	XmStringFree(*(XmString *) to_value->addr);
}

int
main(argc, argv)
	int             argc;
	char          **argv;
{
	XtAppContext    app_context;
	Display        *display;
	Widget          appshell;
	Widget          label;

	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();

	/* XtOpenDisplay() builds the resource database */
	display = XtOpenDisplay(app_context, NULL, "name", "Class", NULL, 0,
				&argc, argv);

	/*
	 * Create our first VendorShell before registering the type
	 * converter, otherwise Motif will overwrite it
	 */
	appshell = XtAppCreateShell("name", "Class",
			     applicationShellWidgetClass, display, NULL, 0);
	XtSetTypeConverter(XmRString, XmRXmString, string_to_xmstring,
			   NULL, 0, XtCacheNone | XtCacheRefCount,
			   string_to_xmstring_destroy);

	/* Create a label (with an XmString labelString resource) */
	label=XmCreateLabel(appshell, "label", NULL, 0);

	XtManageChild(label);
	XtRealizeWidget(appshell);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   34,   17, 0,0,0, /* label */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);

	exit(0);
}
