/*
  $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test8.c,v 1.4 2002/04/17 16:22:01 amai Exp $
** accelerators
*/

#include <stdlib.h>
#include <stdio.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/keysym.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>
#include <Xm/ToggleBG.h>
#include <stdio.h>

Widget shell = (Widget) NULL;
Widget quitb = (Widget) NULL;

/* Structure used to pass details of the accelerator to the event handler */
typedef struct {
	KeyCode         keycode;
	Modifiers       modifiers;
	Widget          pushbutton;
}               accel_key_t, *accel_key_p;

void            handle_accel();

void
install_accelerator(shell, keysym, modifiers, pushbutton)
	Widget          shell;
	KeySym          keysym;
	Modifiers       modifiers;
	Widget          pushbutton;
{
	KeyCode         keycode;
	accel_key_p     accel;

	/* Convert keysym to keycode and set up passive grab on shell */
	keycode = XKeysymToKeycode(XtDisplay(shell), keysym);
	if (keycode == 0)
		return;		/* with error message */
	XtGrabKey(shell, keycode, modifiers, False, GrabModeAsync,
		  GrabModeAsync);

	/* Set up event handler on shell */
	accel = (accel_key_p) XtMalloc(sizeof(accel_key_t));
	accel->keycode = keycode;
	accel->modifiers = modifiers;
	accel->pushbutton = pushbutton;
	XtAddEventHandler(shell, KeyPressMask, False, handle_accel,
			  (XtPointer) accel);
}

void 
handle_accel(widget, client_data, event, cont)
	Widget          widget;
	XtPointer       client_data;
	XEvent         *event;
	Boolean        *cont;
{
	/* Check that we have the right key combination */
	accel_key_p     accel = (accel_key_p) client_data;
	if ((event->xkey.state == accel->modifiers)
	    && (event->xkey.keycode == accel->keycode))
		XtCallActionProc(accel->pushbutton, "ArmAndActivate", event,
				 NULL, 0);
}

void create_shell (display, app_name, app_argc, app_argv)
Display *display;
char *app_name;
int app_argc;
char **app_argv;
{
	Widget template = (Widget) NULL;
	Widget sep = (Widget) NULL;
	Widget form = (Widget) NULL;
	Widget radiob = (Widget) NULL;
	Widget radio_1 = (Widget) NULL;
	Widget radio_2 = (Widget) NULL;
	Widget radio_3 = (Widget) NULL;
	Widget label_1 = (Widget) NULL;
	Widget text = (Widget) NULL;
	Widget label_2 = (Widget) NULL;
	Widget okb = (Widget) NULL;
	Widget children[4];      /* Children to manage */
	Arg al[64];                    /* Arg List */
	register int ac = 0;           /* Arg Count */
	XmString xmstring;

	/* Create a pretty standard sort of dialog */
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Keyboard Accelerators"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	shell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	XtSetArg(al[ac], XmNdialogType, XmDIALOG_TEMPLATE); ac++;
	template = XmCreateMessageBox ( shell, "template", al, ac );
	ac = 0;
	sep = XmMessageBoxGetChild ( template, XmDIALOG_SEPARATOR );
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	XtSetArg(al[ac], XmNhorizontalSpacing, 5); ac++;
	XtSetArg(al[ac], XmNverticalSpacing, 5); ac++;
	form = XmCreateForm ( template, "form", al, ac );
	ac = 0;
	radiob = XmCreateRadioBox ( form, "radiob", al, ac );
	radio_1 = XmCreateToggleButtonGadget ( radiob, "radio_1", al, ac );
	radio_2 = XmCreateToggleButtonGadget ( radiob, "radio_2", al, ac );
	radio_3 = XmCreateToggleButtonGadget ( radiob, "radio_3", al, ac );
	label_1 = XmCreateLabel ( form, "label_1", al, ac );
	text = XmCreateTextField ( form, "text", al, ac );
	xmstring = XmStringCreateLtoR("Quit button can be activated by Ctrl-Shift-Q\nanywhere in the dialog", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label_2 = XmCreateLabel ( form, "label_2", al, ac );
	ac = 0;
	XmStringFree ( xmstring);
	xmstring = XmStringCreateLtoR("OK", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	okb = XmCreatePushButton ( template, "okb", al, ac );
	ac = 0;
	XmStringFree ( xmstring);
	xmstring = XmStringCreateLtoR("Quit", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	quitb = XmCreatePushButton ( template, "quitb", al, ac );
	ac = 0;
	XmStringFree ( xmstring);

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetValues ( radiob,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftWidget, radiob); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
	XtSetValues ( label_1,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopWidget, label_1); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, label_1); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
	XtSetValues ( text,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopWidget, radiob); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetValues ( label_2,al, ac );
	ac = 0;
	children[ac++] = radio_1;
	children[ac++] = radio_2;
	children[ac++] = radio_3;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = radiob;
	children[ac++] = label_1;
	children[ac++] = text;
	children[ac++] = label_2;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = form;
	children[ac++] = okb;
	children[ac++] = quitb;
	XtManageChildren(children, ac);
	ac = 0;
	XtManageChild ( template);
}



XtAppContext app_context;
Display *display;       /*  Display             */

int main (argc,argv)
int    argc;
char            **argv;
{
  XtSetLanguageProc ( (XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL );
  XtToolkitInitialize ();
  app_context = XtCreateApplicationContext ();
  display = XtOpenDisplay (app_context, NULL, argv[0], "XApplication",
			   NULL, 0, &argc, argv);
  if (!display)
    {
      printf("%s: can't open display, exiting...\n", argv[0]);
      exit (-1);
    }
  /* Create the dialog */
  create_shell ( display, argv[0], argc, argv );

  /* Install the accelerator */
  install_accelerator (shell, XK_q, ControlMask | ShiftMask, quitb);
  XtRealizeWidget (shell);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  428,  398,  295,  196, 0,0,0, /* template */
   CWWidth | CWHeight | CWX | CWY,    0,  148,  295,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  273,  127, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   73,   87, 0,0,0, /* radiob */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   67,   25, 0,0,0, /* radio_1 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,   67,   25, 0,0,0, /* radio_2 */
   CWWidth | CWHeight | CWX | CWY,    3,   59,   67,   25, 0,0,0, /* radio_3 */
   CWWidth | CWHeight | CWX | CWY,   83,    5,   46,   17, 0,0,0, /* label_1 */
   CWWidth | CWHeight | CWX | CWY,   83,   27,  138,   31, 0,0,0, /* text */
   CWWidth | CWHeight | CWX | CWY,    5,   97,  268,   30, 0,0,0, /* label_2 */
   CWWidth | CWHeight | CWX | CWY,   11,  160,   36,   25, 0,0,0, /* okb */
   CWWidth | CWHeight | CWX | CWY,  248,  160,   36,   25, 0,0,0, /* quitb */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(shell, Expected);
}
LessTifTestMainLoop(shell);
  exit (0);
}

