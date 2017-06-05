/* test for _XmRegionGetRectangles */

#include <Xm/XmP.h>
#include <stdio.h>

extern void print_region();

int
main(int argc,
     char **argv)
{
  XmRegion region1 = _XmRegionCreate();
  XRectangle rect;
  XRectangle *rects;
  long numRects;
  int i;

  printf ("\tRegion 1 -- _XmRegionCreate()\n");
  print_region(region1);

  rect.x = rect.y = 5;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  rect.x = 5; rect.y = 105;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  printf ("\tRegion 1 -- after two _XmUnionRectWithRegion\n");
  print_region(region1);

  _XmRegionGetRectangles(region1, &rects, &numRects);

  for (i=0 ; i < numRects; i ++) 
    printf ("Rectangle [%d] - (%d,%d)x(%d,%d)\n", i,
	    rects[i].x, rects[i].y, rects[i].width, rects[i].height);
  exit(0);
}
