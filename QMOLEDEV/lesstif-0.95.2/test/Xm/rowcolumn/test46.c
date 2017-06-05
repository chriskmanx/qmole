/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test46.c,v 1.4 2001/05/23 14:21:12 amai Exp $
From:        Dr Keith Distin <kdistin@msxi.co.uk>
To:          lesstif@hungry.com
Subject:     bug/differeces with Motif
Date:        Fri, 20 Nov 1998 11:31:47 +0000
*/

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>


static char * labelDesc[] = { /* values based on approximate new cost */
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "END"
};

static void CreateMenuChildren(Widget parent, char **label, int display,
                               XtPointer defaultClientData)
{
    Widget w;
    int i = 0;

    /* 
     * Create an entry for each item in the menu.
     */

    for (i = 0; strcmp(label[i], "END"); i++) {
        w = XtCreateManagedWidget(label[i],
                                  xmPushButtonWidgetClass,
                                  parent, NULL, 0);

        if (display == i) {
            XtVaSetValues(parent,
                          XmNmenuHistory, w,
                          NULL, NULL);
        }
    }
}

Widget CreateMenu(char *name,
                  Widget parent,
                  char **label,
                  int display,
                  XtPointer defaultClientData)
{
    Widget w;

    Widget option = XmCreateOptionMenu(parent, name, NULL, 0);

    w = XmCreatePulldownMenu(parent, "opt_pulldown", NULL, 0);

    XtVaSetValues(option, XmNsubMenuId, w, NULL);

    CreateMenuChildren(w, label, display, defaultClientData);

    XtManageChild(option);

    return (option);
}

int
main (int argc, char **argv)
{
    Widget shell1, opt;
    XtAppContext app;
    
    shell1 = XtVaAppInitialize(&app, "Xrecords", NULL, 0,
                               &argc, argv, NULL,
                               XmNtitle, "Xtest",
                               XmNiconName, "Xtest",
                               XmNallowShellResize, TRUE,
                               NULL);
    
    /* create an option menu with the fourth item in the list displayed */
    opt = CreateMenu("opt", shell1, labelDesc, 3, NULL);

    XtRealizeWidget(shell1);

    LessTifTestMainLoop(shell1);
    /*
    XtAppMainLoop(app);
    */

    exit(0);
}
