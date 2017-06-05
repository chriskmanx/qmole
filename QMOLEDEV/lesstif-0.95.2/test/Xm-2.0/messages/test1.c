/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/messages/test1.c,v 1.4 2001/05/08 23:05:04 rwscott Exp $
 * Produce a C source file that contains the Motif private error
 * messages.
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#if XmREVISION > 0
#include "tif20.h"
#endif

int 
main(int argc, char *argv[])
{
	int	i;
	char	*p;

#if XmREVISION > 0
        printf(copyright);
	printf("\n");
	printf("#include <Xm/XmP.h>\n");
	printf("#include <XmI/MessagesI.h>\n");
	printf("\n");
	for (i=0; list[i].n; i++) {
		printf("_XmConst char *%s =\n    %c", list[i].n, '"');
		for (p = *(list[i].t); *p; p++)
			if (*p == '\n') {
				putchar('\\');
				putchar('n');
				putchar('"');
				putchar('\n');
				putchar(' ');
				putchar(' ');
				putchar(' ');
				putchar(' ');
				putchar('"');
			} else if (*p == '"') {
				putchar('\\');
				putchar('"');
			} else if (!isprint(*p)) {
				printf("\\%o", (unsigned char)*p);
			} else
				putchar(*p);
		putchar('"');
		putchar(';');
		putchar('\n');
		putchar('\n');
	}
#endif
	exit(0);
}
