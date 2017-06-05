/* $Header: /cvsroot/lesstif/lesstif/test/Xm/bulletinboard/test10.c,v 1.9 2001/05/19 21:44:18 amai Exp $ */
/*
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
#include <Xm/BulletinB.h>

#include "../../common/Test.h"


int
main(int argc, char *argv[])
{
    XtAppContext app;
    Widget shell, w;
    void *before;
    void *after;
    int iter = 0;
    size_t diff = 0,  total = 0;

    shell = XtAppInitialize(&app, "Test", NULL, 0,
			    &argc, argv, NULL, NULL, 0);
    while (iter < 10000)
    {
	before = sbrk((ptrdiff_t) 0);
	w = XtCreateWidget("name", xmBulletinBoardWidgetClass, shell, NULL, 0);
	XtDestroyWidget(w);
	after = sbrk((ptrdiff_t) 0);
	diff = (size_t)((char *)after-(char *)before);
	if (diff > 0)
	{
	    if (iter != 0)
	    {
	      /*		printf("%i %i %p %i\n", iter, iter - diff,
				after - before, (after - before) / (iter -
				diff)); */
		total += diff;
	    }
	}
	iter++;
    }
    printf("leaking %li bytes per Create/Destroy\n", total / iter);
    exit((int)(total / iter));
}
