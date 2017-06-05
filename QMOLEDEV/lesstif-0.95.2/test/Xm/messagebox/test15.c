/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test15.c,v 1.7 2008/01/02 19:50:21 dannybackx Exp $
  
   From:        Yasushi Yamasaki <yamapu@osk3.3web.ne.jp>
   To:          lesstif@hungry.com
   Subject:     bug report
   Date:        Sun, 30 Aug 1998 23:55:47 +0900
   Cc:          yamapu@osk3.3web.ne.jp
 */

#include <stdio.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/MessageB.h>

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
    while (iter < 100)
    {
	before = sbrk((ptrdiff_t) 0);
	w = XtCreateWidget("name", xmMessageBoxWidgetClass, shell, NULL, 0);
	XtDestroyWidget(w);
	after = sbrk((ptrdiff_t) 0);
	if ((int)((char*) after - (char*) before) > 0)
	{
	    if (iter != 0)
	    {
		total += (int)((char*) after - (char*) before);
	    }
	    diff = iter;
	}
	iter++;
    }
    printf("leaking %i bytes per Create/Destroy\n", total / iter);
    exit((int)(total / iter));
}
