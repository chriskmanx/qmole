/* $Id: test8.c,v 1.1 2000/07/07 07:56:17 amai Exp $ */
/* This is the 'short' version of the tests as in test[5-7];
   This is really just the test as in the OSF manual - I didn't attempt
   to make any explicit calls to initialize something ... */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Text.h>

int main() {

       XmSecondaryResourceData * block_array ;
       Cardinal num_blocks, i, j ;
       if (num_blocks = XmGetSecondaryResourceData (xmTextWidgetClass,
                                                    &block_array)) {
         for (i = 0; i < num_blocks; i++) {
           for (j = 0 ; j < block_array[i]->num_resources; j++) {
             printf("%s\n", block_array[i]->resources[j].resource_name);
           }
           XtFree((char*)block_array[i]->resources);
           XtFree((char*)block_array[i]);
         }
         XtFree((char*)block_array);
       }
 exit(0);
}
