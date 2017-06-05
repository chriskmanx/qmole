/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/messages/test1.c,v 1.1 2001/01/03 13:39:03 amai Exp $
 * Produce a C source file that contains the Motif private error
 * messages.
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "tif21.h"

int 
main(int argc, char *argv[])
{
	int	i;
	char	*p;

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
	exit(0);
}
