/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/xmos/test6.c,v 1.3 2001/08/15 08:04:37 amai Exp $
 * test for _XmOSFindPatternPart()
 *
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/XmosP.h>

int
main(int argc,
     char **argv)
{
	char buf[256];
	String ret;

#if NOT_AUTOMATIC
	for (;;) {
		gets(buf);
		if (feof(stdin))
			break;
		ret = _XmOSFindPatternPart(buf);
		printf("buf: %p FindPatternPart: %p: string '%s'\n", buf, ret, (ret) ? ret : "");
	}
#endif
	exit(0);
}
