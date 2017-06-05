/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/baseclass/test6.c,v 1.2 2001/06/11 08:26:30 amai Exp $
 * test XmGetSecondaryResourceData().  Taken from the OSF/Motif
 * programmers manual (page 1-539).
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/SeparatoG.h>
#include <Xm/BulletinB.h>

int
main(int argc, char **argv)
{
    XtAppContext AppContext;
    Widget       TopLevel, w, bb;
    XmSecondaryResourceData *block_array;
    Cardinal num_blocks, i, j;
    
    TopLevel = XtAppInitialize(&AppContext, "secres",
                               NULL, 0,
                               &argc, argv,
                               NULL,
                               NULL, 0);

    bb = XtCreateWidget("bb", xmBulletinBoardWidgetClass, TopLevel, NULL, 0);
    w = XtCreateWidget("sep", xmSeparatorGadgetClass, bb, NULL, 0);

    if ((num_blocks = XmGetSecondaryResourceData(xmSeparatorGadgetClass,
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
