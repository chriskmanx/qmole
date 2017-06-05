/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test26.c,v 1.1 2001/09/19 16:32:47 amai Exp $
   from  [ lesstif-Bugs-225495 ] programmatic selection fails in List widgets
*/
#include <stdio.h>
#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/PushB.h>

void
deselect (Widget w, XtPointer client_data, XtPointer call_data)
{
  Widget list = (Widget)client_data;
  Arg wargs[10];
  int m;

  m = 0;
  XtSetArg (wargs[m], XmNselectedItemCount, 0);
  m++;

/*
 * BUG:
 * With lesstif the prog works only when the following line is commented out,
 * with motif the prog works in both cases
 */
  XtSetArg (wargs[m], XmNselectedItems, NULL);
  m++;

  XtSetValues (list, wargs, m);
}

char *items[] = {
  "hello", "world!", "what's", "up"
};

#define N_ITEMS (sizeof items/sizeof *items)

int
main (int argc, char **argv)
{

  Arg wargs[20];
  int m, i;

  Widget toplevel, form, button, list;
  XmString xm_items[N_ITEMS];

  /*
   * initializing
   */


  toplevel = XtInitialize (argv[0], "TestList", NULL, 0, &argc, argv);

  /*
   * creating the form
   */

  m = 0;
  form = XmCreateForm (toplevel, "XmForm", wargs, m);
  XtManageChild (form);

  /*
   * creating the button
   */

  m = 0;
  XtSetArg (wargs[m], XmNtopAttachment, XmATTACH_FORM);
  m++;
  XtSetArg (wargs[m], XmNleftAttachment, XmATTACH_FORM);
  m++;
  XtSetArg (wargs[m], XmNrightAttachment, XmATTACH_FORM);
  m++;
  XtSetArg (wargs[m], XmNbottomAttachment, XmATTACH_NONE);
  m++;
  button = XmCreatePushButton (form, "Deselect", wargs, m);
  XtManageChild (button);

  /*
   * creating the list
   */

  for (i = 0; i < N_ITEMS; i++)
    {

      xm_items[i] = XmStringCreate (items[i], XmSTRING_DEFAULT_CHARSET);
    }

  m = 0;
  XtSetArg (wargs[m], XmNitemCount, N_ITEMS);
  m++;
  XtSetArg (wargs[m], XmNitems, xm_items);
  m++;
  XtSetArg (wargs[m], XmNselectedItemCount, 0);
  m++;
  XtSetArg (wargs[m], XmNtopAttachment, XmATTACH_WIDGET);
  m++;
  XtSetArg (wargs[m], XmNtopWidget, button);
  m++;
  XtSetArg (wargs[m], XmNtopAttachment, XmATTACH_WIDGET);
  m++;
  XtSetArg (wargs[m], XmNleftAttachment, XmATTACH_FORM);
  m++;
  XtSetArg (wargs[m], XmNrightAttachment, XmATTACH_FORM);
  m++;
  XtSetArg (wargs[m], XmNbottomAttachment, XmATTACH_FORM);
  m++;

  XtSetArg (wargs[m], XmNselectionPolicy, XmMULTIPLE_SELECT);
  m++;
  list = XmCreateList (form, "XmList", wargs, m);
  XtManageChild (list);

	/*
         * setting the callback
         */


  XtAddCallback (button, XmNactivateCallback, deselect, (XtPointer) list);

	/*
         * realizing
         */

  XtRealizeWidget (toplevel);
#if 0
  XtMainLoop ();
#else
  LessTifTestMainLoop(toplevel);
#endif

  return (EXIT_SUCCESS);
}
