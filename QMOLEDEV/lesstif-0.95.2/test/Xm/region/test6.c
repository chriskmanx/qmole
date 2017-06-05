/* test for _XmRegionClear */

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

  rect.x = rect.y = 500;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  printf ("\tRegion 1 -- after two _XmUnionRectWithRegion\n");
  print_region(region1);

  _XmRegionClear(region1);

  printf ("\tRegion 1 -- after _XmRegionClear\n");
  print_region(region1);
  exit(0);
}
