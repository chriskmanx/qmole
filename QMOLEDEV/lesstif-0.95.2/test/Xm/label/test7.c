/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test7.c,v 1.6 2002/04/15 18:17:21 amai Exp $
   From:        Yasushi Yamasaki <yamapu@osk3.3web.ne.jp>
   To:          lesstif@hungry.com
   Subject:     bug report
   Date:        Sun, 30 Aug 1998 23:55:47 +0900
   Cc:          yamapu@osk3.3web.ne.jp
 */

/* test1.c */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>

int
main(int argc, char *argv[])
{
    XtAppContext app;
    Widget shell, w;

    shell = XtAppInitialize(&app, "Test", NULL, 0,
			    &argc, argv, NULL, NULL, 0);
			    
fprintf(stderr, "**********fontfontfont***********\n");

    w = XtCreateWidget("name", xmLabelWidgetClass, shell, NULL, 0);
    XtDestroyWidget(w);

fprintf(stderr, "**********fontfontfont***********\n");

    w = XtCreateWidget("name", xmLabelWidgetClass, shell, NULL, 0);
    XtDestroyWidget(w);
    
    exit(0);
}
