/*
   $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/icong/test1.c,v 1.1 2002/04/18 19:19:03 amai Exp $
   This test case looks for memory leakage.
   Based on the test38.c from Xm/form.
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/Container.h>
#include <Xm/IconG.h>

int
main(int argc, char *argv[])
{
    XtAppContext app;
    Widget shell, w, bb, cw;
    void *before;
    void *after;
    int iter = 0;
    int diff = 0;
    int total = 0;

    shell = XtAppInitialize(&app, "Test", NULL, 0,
			    &argc, argv, NULL, NULL, 0);
    cw = XmCreateContainer(shell, "bb", NULL, 0);

    while (iter < 1000)
    {
	before = sbrk((ptrdiff_t) 0);
	w = XtCreateWidget("name", xmIconGadgetClass, cw, NULL, 0);
	XtDestroyWidget(w);
	after = sbrk((ptrdiff_t) 0);
	if ((int)((char *)after - (char *)before) > 0)
	{
	    if (iter != 0)
	    {
	      /* printf("%i %i %p %i\n", iter, iter - diff, after -
		 before, (after - before) / (iter - diff)); */
		total += (int)((char *)after - (char *)before);
	    }
	    diff = iter;
	}
	iter++;
    }
    printf("leaking %i bytes per Create/Destroy\n", total / iter);
    exit((int)(total / iter));
}
