/* $Header: /cvsroot/lesstif/lesstif/test/Xm/pushbutton/test18.c,v 1.6 2004/10/03 13:26:50 dannybackx Exp $
   From:        Yasushi Yamasaki <yamapu@osk3.3web.ne.jp>
   To:          lesstif@hungry.com
   Subject:     bug report
   Date:        Sun, 30 Aug 1998 23:55:47 +0900
   Cc:          yamapu@osk3.3web.ne.jp
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>

int
main(int argc, char *argv[])
{
    XtAppContext app;
    Widget shell, w;
    void *before;
    void *after;
    int iter = 0;
    int diff = 0;
    int total = 0;

    shell = XtAppInitialize(&app, "Test", NULL, 0,
			    &argc, argv, NULL, NULL, 0);

    /* Part 1 - create/destroy cycle */

    while (iter < 20)
    {
	before = sbrk((ptrdiff_t) 0);
	w = XtCreateWidget("name", xmPushButtonWidgetClass, shell, NULL, 0);
	XtDestroyWidget(w);
	after = sbrk((ptrdiff_t) 0);
	if ((int)((char *)after - (char *)before) > 0)
	{
	    if (iter != 0)
	    {
		/*
		printf("%i %i %p %i\n", iter, iter - diff, after - before, (after - before) / (iter - diff));
		 */
		total += (int)((char *)after - (char *)before);
	    }
	    diff = iter;
	}
	iter++;
    }
    printf("leaking %i bytes per Create/Destroy\n", total / iter);

    /* Part 2 - Display text */
	w = XtCreateManagedWidget("name", xmPushButtonWidgetClass, shell, NULL, 0);
	for (iter = 0; iter < 200; iter++) {
		char	s[20];
		XmString	xms;
		Arg	a;

		sprintf(s, "%d", iter);
		xms = XmStringCreateSimple(s);
		XtSetArg(a, XmNlabelString, xms);
		XtSetValues(w, &a, 1);
		XmStringFree(xms);
	}
	XtRealizeWidget(shell);

    /* Part 3 - Close down */
    XtDestroyWidget(shell);
    XtDestroyApplicationContext(app);

    /* End */
    fprintf(stderr, "Finishing up\n");

    sleep(300);
    exit(0);
}
