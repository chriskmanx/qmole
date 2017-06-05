/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test15.c,v 1.5 2001/06/18 14:22:06 amai Exp $
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
#include <Xm/ScrolledW.h>

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
    while (iter < 10000)
    {
	before = sbrk((ptrdiff_t) 0);
	w = XtCreateWidget("name", xmScrolledWindowWidgetClass, shell, NULL, 0);
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
    exit((int)(total / iter));
}
