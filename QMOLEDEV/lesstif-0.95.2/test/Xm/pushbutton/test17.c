/* $Header: /cvsroot/lesstif/lesstif/test/Xm/pushbutton/test17.c,v 1.4 2001/05/15 14:26:56 amai Exp $
To:          lesstif@hungry.com
From:        Silverman Bill <bill@wisdom.weizmann.ac.il>
Subject:     Bug Report

I first encountered this bug in lesstif 0.83.  All of
my tests and developed applications worked ok on lesstif
0.81 .  When I tried to upgrade to 0.83, on my SPARC
station running SunOS 4.1.4 3 with X11R5 (IntrinsicP.h
revision and date 1.57 91/06/26), it crashed every
program and test, immediately.  I thought that I had
a bad source, and having better things to do, decided to
pick up a later version of lesstif after a while.

I picked up lesstif-current (creation date 980816) a few
days ago, rebuilt and installed it, and the problem was
still there.  According to your instructions, I am
supplying a test program and a stack trace.

I really need lesstif for the SunOS system, since
several of our public machines are old SPARCs, still
running SunOS.

Thank you for your attention,

     William Silverman

------------------ stack trace -----------------------

Starting program: /tmp_mnt/home/bill/bill/TOOLS/sunos-motif-egs/ch02/hello 

Breakpoint 1, main (argc=1, argv=0xeffff85c) at hello.c:26
26          XtSetLanguageProc (NULL, NULL, NULL);
(gdb) continue
Continuing.

Program received signal SIGSEGV, Segmentation fault.
0xef70ea88 in XmSetColorCalculation (proc=0) at Visual.c:74
74
(gdb) where
#0  0xef70ea88 in XmSetColorCalculation (proc=0) at Visual.c:74
#1  0xef736c0c in class_initialize () at Vendor.c:632
#2  0xef5a6388 in XtInitializeWidgetClass ()
#3  0xef5a636c in XtInitializeWidgetClass ()
#4  0xef5a636c in XtInitializeWidgetClass ()
#5  0xef5a64cc in _XtCreate ()
#6  0xef5a6d60 in _XtAppCreateShell ()
#7  0xef5c8e88 in XtVaAppCreateShell ()
#8  0xef5c9154 in _XtVaAppInitialize ()
#9  0x24a0 in XtVaAppInitialize ()
#10 0x230c in main (argc=1, argv=0xeffff85c) at hello.c:28
(gdb) 

------------------- test program ------------------------
*/

/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* hello.c -- initialize the toolkit using an application context and a 
 * toplevel shell widget, then create a pushbutton that says Hello using
 * the varargs interface.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/PushB.h>

int main(int argc, char *argv[])
{
    Widget        toplevel, button;
    XtAppContext  app;
    void          button_pushed();
    XmString 	  label;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Hello", NULL, 0,
        &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized ("Push here to say hello"); 
    button = XtVaCreateManagedWidget ("pushme",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString, label,
        NULL);
    XmStringFree (label);
    XtAddCallback (button, XmNactivateCallback, button_pushed, NULL);

    XtRealizeWidget (toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	144,	25,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	45,	30,	30,	25,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
    XtAppMainLoop (app);
    */
  exit(0);
}

void
button_pushed(Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    printf ("Hello Yourself!\n");
}


