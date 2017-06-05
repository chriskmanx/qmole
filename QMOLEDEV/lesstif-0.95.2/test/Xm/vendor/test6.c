/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vendor/test6.c,v 1.1 2001/03/16 14:28:01 amai Exp $ */
/*

   see http://www.mail-archive.com/lesstif@hungry.com/msg00535.html

      Hello Lesstif,

      Scenario:

      lesstif-0.91.4
      Motif-2.0 widgets
      Redhat 6.2

      I get the warning message:

      Warning: XtRemoveGrab asked to remove a widget not on the list

      twice every time I destroy a toplevel shell.

      I've included a simple example, press "Create Window 2" to make
      a second window, and when you press "Quit 2" in this window, you'll
      get the error message...

      I don't get this message with openmotif :-)

      Am I doing something wrong in my code, or is this a bug in Lesstif?

      Mogens
      -- 
      Mogens Kjaer, Carlsberg Laboratory, Dept. of Chemistry
      Gamle Carlsberg Vej 10, DK-2500 Valby, Denmark
      Phone: +45 33 27 53 25, Fax: +45 33 27 47 08
      Email: mk@crc.dk Homepage: http://www.crc.dk

*/


#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>

void quit_callback ();
void window2_callback ();

static int n;
static Arg wargs[1000];
static Widget toplevel, form, quit, make_w2;
static Widget toplevel2, form2, quit2;

void
quit_callback (Widget w, caddr_t client_data, caddr_t user_data)
{
  exit (0);
}

void
quit2_callback (Widget w, caddr_t client_data, caddr_t user_data)
{
  XtDestroyWidget (toplevel2);
}

void
make_w2_callback (Widget w, caddr_t client_data, caddr_t user_data)
{
  n = 0;
  XtSetArg (wargs[n], XtNtitle, "Window 2");
  n++;
  toplevel2 =
    XtCreateApplicationShell ("Window2", topLevelShellWidgetClass, wargs, n);

  n = 0;
  form2 =
    XtCreateManagedWidget ("form2", xmFormWidgetClass, toplevel2, wargs, n);

  n = 0;
  XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_FORM);
  n++;
  XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM);
  n++;
  XtSetArg (wargs[n], XmNlabelString, XmStringCreateSimple ("Quit 2"));
  n++;
  quit2 =
    XtCreateManagedWidget ("quit2", xmPushButtonWidgetClass, form2, wargs, n);
  XtAddCallback (quit2, XmNactivateCallback, (XtCallbackProc) quit2_callback,
		 NULL);

  XtRealizeWidget (toplevel2);
}

int
main (int argc, char *argv[])
{
  Widget xref;

  n = 0;
  toplevel =
    XtInitialize ("example", "Example", (XrmOptionDescRec *) wargs, n, &argc,
		  argv);

  n = 0;
  form =
    XtCreateManagedWidget ("form", xmFormWidgetClass, toplevel, wargs, n);

  n = 0;
  XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_FORM);
  n++;
  XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM);
  n++;
  XtSetArg (wargs[n], XmNlabelString, XmStringCreateSimple ("Quit"));
  n++;
  xref = quit =
    XtCreateManagedWidget ("quit", xmPushButtonWidgetClass, form, wargs, n);
  XtAddCallback (quit, XmNactivateCallback, (XtCallbackProc) quit_callback,
		 NULL);

  n = 0;
  XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_FORM);
  n++;
  XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_WIDGET);
  n++;
  XtSetArg (wargs[n], XmNleftWidget, xref);
  n++;
  XtSetArg (wargs[n], XmNlabelString,
	    XmStringCreateSimple ("Create Window 2")); n++;
  xref = make_w2 =
    XtCreateManagedWidget ("make_w2", xmPushButtonWidgetClass, form, wargs,
			   n);
  XtAddCallback (make_w2, XmNactivateCallback,
		 (XtCallbackProc) make_w2_callback, NULL);

  XtRealizeWidget (toplevel);
#if 0
  XtMainLoop ();
#else
  LessTifTestMainLoop(toplevel);
#endif
  
  exit(0);
}
