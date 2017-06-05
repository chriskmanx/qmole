/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test64.c,v 1.2 2002/05/03 12:03:41 amai Exp $ */


/*
   In real Motif, this program will make a menubar with one item, a 
   separator, and another "pull-right" menu that contains one item. 

   In LessTif, the second pullright menu does not work. See the 
   "BUG HERE" comment in the code. 

   Info: 
   uname -a: Linux lace 2.2.13 #2 Mon Jan 24 19:04:07 EST 2000 i686 unknown 
   LessTif version: lesstif-0.91.4 
   gcc version: gcc version egcs-2.91.66 19990314/Linux (egcs-1.1.2 release) 

   Keep up the great work, I think LessTif is a great project. 

   -Paul 
   pwilkins@wilkins.ne.mediaone.net 
*/


#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>

#include "../../common/Test.h"

void
file_cb (Widget w, XtPointer client_data, XtPointer call_data)
{
  printf ("Menu Item %d\n", (int) client_data);
}


void
right_cb (Widget w, XtPointer client_data, XtPointer call_data)
{
  printf ("Right Menu Item %d\n", (int) client_data);
}


void
create_menu (Widget parent)
{
  Widget menubar, file_w, right_w;
  XmString str1, str2;

  str1 = XmStringCreateSimple ("File");
  menubar = XmVaCreateSimpleMenuBar (parent, "main_w",
				     XmVaCASCADEBUTTON, str1, 'F', NULL);
  XmStringFree (str1);

  str1 = XmStringCreateSimple ("Item1");
  str2 = XmStringCreateSimple ("Item2");
  file_w = XmVaCreateSimplePulldownMenu (menubar, "file_menu", 0, file_cb,
					 XmVaPUSHBUTTON, str1, '1', NULL,
					 NULL, XmVaSEPARATOR,
					 XmVaCASCADEBUTTON, str2, NULL, NULL);
  XmStringFree (str1);
  XmStringFree (str2);

  str1 = XmStringCreateSimple ("Item2");
  right_w = XmVaCreateSimplePulldownMenu (file_w, "spcl_menu", 2,	/* BUG HERE: real motif needs 2, lesstif needs 1. 
									   This probably has something to do with the 
									   XmVaSEPARATOR above */
					  right_cb,
					  XmVaPUSHBUTTON, str1, '2', NULL,
					  NULL, NULL);
  XmStringFree (str1);

  XtManageChild (menubar);

}


int
main (int argc, char *argv[])
{
  XtAppContext app;
  Widget toplevel, main_w;

  toplevel = XtVaAppInitialize (&app, "menu_bug", NULL, 0,
				&argc, argv, NULL, XtNinput, TRUE, NULL);

  main_w = XtVaCreateWidget ("main_w",
			     xmMainWindowWidgetClass, toplevel,
			     XtNminWidth, 200, XtNminHeight, 200, NULL);

  /* set up the menu bar */
  create_menu (main_w);

  XtManageChild (main_w);

  XtRealizeWidget (toplevel);

  /* XtAppMainLoop (app); */
  LessTifTestMainLoop(  toplevel );
  
  exit(0);
}
