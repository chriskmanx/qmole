/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test10.c,v 1.1 2005/04/13 19:54:56 dannybackx Exp $
 * 
 * Investigate bug #1037816 - XmStringCreateLtoR and XmStringGenerate NL broken
 *
 * I'm running 0.93.94-8 on debian (installed with
 * lesstif2 devel. headers)
 *
 * When generating a string with XmStringCreateLtoR that
 * contains various \n and they are put as string in an
 * information dialog box, the text is split in new lines,
 * but each line following has a funny character at the
 * beginning (here NL).
 *
 * Since this subroutine is obsolete, I tried
 * XmStringGenerate, the result are even worse. the funny
 * chars remain and I get no line splitting (and new funny
 * characters, VT)
 *
 * The code is nebula irc (http://nebula-irc.sf.net) and
 * the two versions are switchable with a macro in main.h
 *
 * This code works fine on OpenMotif (both subroutine call
 * versions), on IRIX Motif by sgi (2.1 style) and on AIX
 * Motif (old style function) the text is fine.
 */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/XmAll.h>


int main(int argc, char **argv)
{
    Widget		toplevel, label, rc;
    XtAppContext	app;
    int			i;
    XmString		xms;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel,
		    NULL);

    /* 1 - XmStringCreate */
    xms = XmStringCreate("Hello World", "bleh");
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    /* 2 - XmStringCreateSimple */
    XtManageChild(XmCreateSeparator(rc, " ", NULL, 0));
    xms = XmStringCreateSimple("Hello World");
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    /* 3 - XmStringCreate */
    XtManageChild(XmCreateSeparator(rc, " ", NULL, 0));
    xms = XmStringCreate("XmStringCreate - Hello\tWorld\nThis is boring", "bleh");
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    /* 4 - XmStringCreateSimple */
    XtManageChild(XmCreateSeparator(rc, " ", NULL, 0));
    xms = XmStringCreateSimple("XmStringCreateSimple - Hello\tWorld\nThis is boring");
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    /* 5 - XmStringGenerate */
    XtManageChild(XmCreateSeparator(rc, " ", NULL, 0));
    xms = XmStringGenerate("XmStringGenerate - Hello\tWorld\nThis is boring", NULL, XmCHARSET_TEXT, NULL);
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    /* 6 - XmStringCreateLocalized */
    XtManageChild(XmCreateSeparator(rc, " ", NULL, 0));
    xms = XmStringCreateLocalized("XmStringCreateLocalized - Hello\tWorld\nThis is boring");
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    /* 7 - XmStringCreateLtoR */
    XtManageChild(XmCreateSeparator(rc, " ", NULL, 0));
    xms = XmStringCreateLtoR("XmStringCreateLtoR - Hello\tWorld\nThis is boring", XmFONTLIST_DEFAULT_TAG);
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc, XmNlabelString, xms, NULL);
    XmStringFree(xms);

    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   70,   17, 0,0,0, /* label */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
    exit(0);
}
