/*
 * test7.c -- test XmGetSecondaryResourceData().  Taken from the OSF/Motif
 * programmers manual (page 1-539).
 */
#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>


int
main(int argc, char **argv)
{
    XtAppContext AppContext;
    Widget       TopLevel, w;
    XmSecondaryResourceData *block_array;
    Cardinal num_blocks, i, j;
    
    TopLevel = XtAppInitialize(&AppContext, "secres",
                               NULL, 0,
                               &argc, argv,
                               NULL,
                               NULL, 0);


    w = XtCreateWidget("vs", vendorShellWidgetClass, TopLevel, NULL, 0);

    if ((num_blocks = XmGetSecondaryResourceData(vendorShellWidgetClass,
						&block_array)) != 0) {
	for (i = 0; i < num_blocks; i++) {
	    for (j = 0; j < block_array[i]->num_resources; j++) {
		printf("%s\n", block_array[i]->resources[j].resource_name);
	    }
	    XtFree((char *)block_array[i]->resources);
	    XtFree((char *)block_array[i]);
	}
	XtFree((char *)block_array);
    }
    exit(0);
}
