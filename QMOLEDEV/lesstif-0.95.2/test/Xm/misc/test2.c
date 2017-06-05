/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test2.c,v 1.2 2002/05/13 12:39:01 amai Exp $ */

/* generate XmStrDefs.c from the Motif libXm it is linked to */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <Xm/XmP.h>

/*
 * order is C, N, R, V, S
 */
int
main() {
    const char *ptr;
    int CNRVS = 0;

    ptr = _XmStrings;
    do {
	/*
	 * this isn't perfect.  The first and the last entries have to
	 * be fixed by hand (XmS and XmRTopItemPosition)
	 */
	if (*ptr) {
	    if (CNRVS == 0 && islower(*ptr))
		CNRVS = 1;
            if (CNRVS == 1 && isupper(*ptr))
		CNRVS = 2;
            if (CNRVS == 2 && islower(*ptr))
		CNRVS = 3;
            if (CNRVS == 3 && isupper(*ptr))
		CNRVS = 4;
	}

	switch (CNRVS) {
	case 0:
	    printf("#ifndef XmC%s\n", ptr);
	    printf("#define XmC%s ((char *)&_XmStrings[%d])\n",
		   ptr, ptr - _XmStrings);
	    printf("#endif\n");
	    break;
	case 1:
	    printf("#ifndef XmN%s\n", ptr);
	    printf("#define XmN%s ((char *)&_XmStrings[%d])\n",
		   ptr, ptr - _XmStrings);
	    printf("#endif\n");
	    break;
	case 2:
	    printf("#ifndef XmR%s\n", ptr);
	    printf("#define XmR%s ((char *)&_XmStrings[%d])\n",
		   ptr, ptr - _XmStrings);
	    printf("#endif\n");
	    break;
	case 3:
	    printf("#ifndef XmV%s\n", ptr);
	    printf("#define XmV%s ((char *)&_XmStrings[%d])\n",
		   ptr, ptr - _XmStrings);
	    printf("#endif\n");
	    break;
	case 4:
	    printf("#ifndef XmS%s\n", ptr);
	    printf("#define XmS%s ((char *)&_XmStrings[%d])\n",
		   ptr, ptr - _XmStrings);
	    printf("#endif\n");
	}
	ptr += strlen(ptr) + 1;
    } while (*ptr);

    printf("\n\n");
    printf("/*\n");
    printf(" * The missing commas are intentional.  Let the compiler\n");
    printf(" * concatenate the strings\n");
    printf(" */\n");
    ptr = _XmStrings;
    printf("const char _XmStrings[] =\n");
    do {
	printf("/* %-5d */ \"%s\\0\"\n", (int)(ptr - _XmStrings), ptr);
	ptr += strlen(ptr) + 1;
    } while (*ptr);
    printf("\"\\0\";\n");
    
    exit(0);
}
