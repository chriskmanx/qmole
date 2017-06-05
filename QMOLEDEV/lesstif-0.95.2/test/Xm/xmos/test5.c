/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/xmos/test5.c,v 1.4 2001/08/15 08:04:37 amai Exp $
 * test for _XmOSBuildFileList()
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
	String quald, qualpat, *entries;
	unsigned int numents, numalloc, i;

#if NOT_AUTOMATIC
	entries = NULL;
	numents = numalloc = 0;
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
		numents = 0;
		_XmOSBuildFileList(quald, qualpat, XmFILE_ANY_TYPE,
				   &entries, &numents, &numalloc);
		for (i = 0; i < numents; i++)
			printf("Entry %d: '%s'\n", i, entries[i]);
	}
#endif
	exit(0);
}
