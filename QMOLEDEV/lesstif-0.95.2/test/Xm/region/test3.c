/* test for behavior of _XmRegionUnionRectWithRegion with
   rectangles that overlap */

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

  rect.x = 5; rect.y = 105;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  printf ("\tRegion 1 -- after two _XmUnionRectWithRegion\n");
  print_region(region1);
  exit(0);
}
