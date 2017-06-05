/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/xmos/test3.c,v 1.3 2001/08/15 08:04:37 amai Exp $
 * test for _XmOSQualifyFileSpec()
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
	char dir[256], pat[256];
	String quald, qualpat;

#if NOT_AUTOMATIC
	for (;;) {
		printf("Enter dir: ");
		gets(dir);
		if (feof(stdin))
			break;
		printf("Enter pat: ");
		gets(pat);
		if (feof(stdin))
			break;
		_XmOSQualifyFileSpec(dir, pat, &quald, &qualpat);
		printf("\n  Dir: '%s' Pat: '%s' quald: '%s' qualpat: '%s'\n",
			dir, pat, quald, qualpat);
	}
#endif
	exit(0);
}
