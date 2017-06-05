/* test for _XmRegionIntersectionRectWithRegion */

#include <Xm/XmP.h>
#include <stdio.h>

extern void print_region();

int
main(int argc,
     char **argv)
{
  XmRegion region1 = _XmRegionCreate();
  XRectangle rect;

  printf ("\tRegion 1 -- _XmRegionCreate()\n");
  print_region(region1);

  rect.x = rect.y = 5;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  printf ("\tRegion 1 -- after _XmUnionRectWithRegion\n");
  print_region(region1);

  rect.x = 10; rect.y = 20;
  rect.width = rect.height = 100;

  _XmRegionIntersectRectWithRegion(&rect,
				   region1, region1);

  printf ("\tRegion 1 -- after _XmUnionRectWithRegion & _XmIntersectRectWithRegion\n");
  print_region(region1);
  exit(0);
}
