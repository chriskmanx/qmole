/* code to duplicate the preferences menu in xephem.
 */

#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeBP.h>

Widget	toplevel;
#define	XtD	XtDisplay(toplevel)

static void pref_simplepair (Widget pd, int prefname, char *pdname,
    char *tip, char *cblabel, int cbmne, XtCallbackProc callback, int op1pref,
    char *op1name, int op1mne, int op2pref, char *op2name, int op2mne);

static void
set_xmstring(Widget w, String res, char *text)
{
    XmString str;

    str = XmStringCreateLtoR(text, XmFONTLIST_DEFAULT_TAG);

    XtVaSetValues(w, res, str, NULL);
}

/* Create "Preferences" PulldownMenu.
 * use the given menu_bar widget as a base.
 * this is called early when the main menu bar is being built..
 * initialize the prefs[] array from the initial state of the toggle buttons.
 */
void
pref_create_pulldown (menu_bar)
Widget menu_bar;
{
	Widget cascade, menu_pane, pull_right;
	Widget tb1, tb2, tb3;
	Arg args[20];
	int n;

	n = 0;
	menu_pane = XmCreatePulldownMenu (menu_bar, "Preferences", args, n);

	    pref_simplepair (menu_pane, 0, "Equatorial",
		    "Whether RA/Dec values are topocentric or geocentric",
		    "Equatorial", 'E', NULL,
		    0, "Topocentric", 'T',
		    0, "Geocentric", 'G');

#if 1
	    pref_simplepair (menu_pane, 0, "Precision",
       "Whether numeric values are shown with more or fewer significant digits",
		    "Precision", 'P', NULL,
		    0, "Higggggggggg", 'H',
		    0, "Low", 'L');

	    pref_simplepair (menu_pane, 0, "MessageBell",
		"Whether to beep when a message is added to the Message dialog",
		    "Message Bell", 'M', NULL,
		    0, "Off", 'f',
		    0, "On", 'O');

	    pref_simplepair (menu_pane, 0, "PromptPreFill",
		"Whether prompt dialogs are prefilled with their current value",
		    "Prompt Prefill", 'f', NULL,
		    0, "No", 'N',
		    0, "Yes", 'Y');

	    pref_simplepair (menu_pane, 0, "Units",
		    "Whether xephem uses english or metric units",
		    "Units", 'U', NULL,
		    0, "English", 'E',
		    0, "Metric", 'M');

	    pref_simplepair (menu_pane, 0, "TZone",
		"Whether time stamps and the calendar are in local time or UTC",
		    "Zone Display", 'Z', NULL,
		    0, "Local", 'L',
		    0, "UTC", 'U');

	    pref_simplepair (menu_pane, 0, "Tips",
		    "Whether to display these little tip boxes!",
			"Show help tips", 't', NULL,
			0, "No", 'N',
			0, "Yes", 'Y');

	    /* create the date formats pullright menu -- it has 3 options */

	    n = 0;
	    XtSetArg (args[n], XmNradioBehavior, True); n++;
	    pull_right = XmCreatePulldownMenu (menu_pane, "DateFormat",args,n);

		n = 0;
		XtSetArg (args[n], XmNmnemonic, 'M'); n++;
		XtSetArg (args[n], XmNvisibleWhenOff, True); n++;
		tb1 = XmCreateToggleButton (pull_right, "MDY", args, n);
		XtManageChild (tb1);
		set_xmstring (tb1, XmNlabelString, "M/D/Y");

		n = 0;
		XtSetArg (args[n], XmNmnemonic, 'Y'); n++;
		XtSetArg (args[n], XmNvisibleWhenOff, True); n++;
		tb2 = XmCreateToggleButton (pull_right, "YMD", args, n);
		XtManageChild (tb2);
		set_xmstring (tb2, XmNlabelString, "Y/M/D");

		n = 0;
		XtSetArg (args[n], XmNmnemonic, 'D'); n++;
		XtSetArg (args[n], XmNvisibleWhenOff, True); n++;
		tb3 = XmCreateToggleButton (pull_right, "DMY", args, n);
		XtManageChild (tb3);
		set_xmstring (tb3, XmNlabelString, "D/M/Y");

		n = 0;
		XtSetArg (args[n], XmNsubMenuId, pull_right);  n++;
		XtSetArg (args[n], XmNmnemonic, 'D'); n++;
		cascade= XmCreateCascadeButton(menu_pane,"DateFormatCB",args,n);
		XtManageChild (cascade);
		set_xmstring (cascade, XmNlabelString, "Date Formats");
#endif

	n = 0;
	XtSetArg (args[n], XmNsubMenuId, menu_pane);  n++;
	XtSetArg (args[n], XmNmnemonic, 'P'); n++;
	cascade = XmCreateCascadeButton (menu_bar, "PreferencesCB", args, n);
	set_xmstring (cascade, XmNlabelString, "Preferences");
	XtManageChild (cascade);
}

/* make one option pair.
 * the state of op1 determines the initial settings. to put it another way,
 * if neither option is set the *second* becomes the default.
 */
static void
pref_simplepair (pd, prefname, pdname, tip, cblabel, cbmne, callback,
		 op1pref, op1name, op1mne, op2pref, op2name, op2mne)
Widget pd;	/* parent pulldown menu */
int prefname;	/* one of Preferences enum */
char *pdname;	/* pulldown name */
char *tip;	/* tip text for the main cascade pair */
char *cblabel;	/* cascade button label */
int cbmne;	/* cascade button mnemonic character */
XtCallbackProc callback;	/* callback function */
int op1pref;	/* option 1 PREF code */
char *op1name;	/* option 1 TB name */
int op1mne;	/* option 1 TB mnemonic character */
int op2pref;	/* option 2 PREF code */
char *op2name;	/* option 2 TB name */
int op2mne;	/* option 2 TB mnemonic character */
{
	Widget pull_right, cascade;
	Widget tb1, tb2;
	Arg args[20];
	int t;
	int n;
	Dimension mw, mr, ml, mh, ht, wd;

	n = 0;
	XtSetArg (args[n], XmNradioBehavior, True); n++;
	pull_right = XmCreatePulldownMenu (pd, pdname, args,n);

	    n = 0;
	    XtSetArg (args[n], XmNmnemonic, op1mne); n++;
	    XtSetArg (args[n], XmNvisibleWhenOff, True); n++;
	    tb1 = XmCreateToggleButton (pull_right, op1name, args, n);
	    XtManageChild (tb1);

	    n = 0;
	    XtSetArg (args[n], XmNmnemonic, op2mne); n++;
	    XtSetArg (args[n], XmNvisibleWhenOff, True); n++;
	    tb2 = XmCreateToggleButton (pull_right, op2name, args, n);
	    XtManageChild (tb2);

	    t = XmToggleButtonGetState(tb1);
	    XmToggleButtonSetState (tb2, !t, False);

	    n = 0;
	    XtSetArg (args[n], XmNsubMenuId, pull_right);  n++;
	    XtSetArg (args[n], XmNmnemonic, cbmne); n++;
	    cascade = XmCreateCascadeButton (pd, "PrefCB", args, n);
	    XtManageChild (cascade);
	    set_xmstring (cascade, XmNlabelString, cblabel);

#if 0
XtVaGetValues(cascade,
	      XmNmarginWidth, &mw,
	      XmNmarginRight, &mr,
	      XmNmarginLeft, &ml,
	      XmNmarginHeight, &mh,
	      XmNwidth, &wd, XmNheight, &ht,
	      NULL);
printf("%s:MW: %d MR: %d ML: %d MH: %d WD: %d HT: %d\n", XtName(cascade), mw, mr, ml, mh, wd, ht);
printf("%s:2:TEST WIDTH: %d\n", XtName(cascade), XtWidth(cascade));
fflush(stdout);
#endif
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget rc;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test1", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XmCreateMenuBar(toplevel, "MB", NULL, 0);

    pref_create_pulldown(rc);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	92,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	5,	5,	82,	21,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


