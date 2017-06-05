/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/messages/test2.c,v 1.4 2001/05/08 23:05:04 rwscott Exp $
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
#endif
	exit(0);
}
