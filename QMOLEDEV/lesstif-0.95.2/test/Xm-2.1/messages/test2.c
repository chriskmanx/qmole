/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/messages/test2.c,v 1.1 2001/01/03 13:39:03 amai Exp $
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
	printf("\n");
        printf("#ifndef XM_MESSAGES_I_H\n");
        printf("#define XM_MESSAGES_I_H\n");
	printf("\n");
	printf("#ifdef __cplusplus\n");
	printf("extern \"C\" {\n");
	printf("#endif\n");
	printf("\n");
	for (i=0; list[i].n; i++) {
		printf("extern _XmConst char * %s;\n", list[i].n);
	}
	printf("\n");
	printf("#ifdef __cplusplus\n");
	printf("}\n");
	printf("#endif\n");
	printf("\n");
	printf("#endif /* XM_MESSAGES_I_H */\n");
	printf("\n");
	exit(0);
}
