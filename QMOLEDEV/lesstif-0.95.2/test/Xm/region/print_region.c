/*
 * just a simple function to print out the contents of
 * of a region.
 *
 */

#include <Xm/XmP.h>
#include <stdio.h>

void 
print_region(XmRegion region)
{
  int i;

  printf ("Region ----------------\n");
  printf ("   size     - %ld\n", region->size);
  printf ("   numRects - %ld\n", region->numRects);
  printf ("   extents  - (%d,%d)-(%d,%d)\n", 
	  region->extents.x1, region->extents.y1,
	  region->extents.x2, region->extents.y2);
  printf ("rects:\n");

  for (i=0; i < region->numRects; i++)
    printf ("  [%d] - (%d,%d)-(%d,%d)\n", i, 
	   region->rects[i].x1, region->rects[i].y1,
	   region->rects[i].x2, region->rects[i].y2);

  printf ("-----------------------\n");
}
