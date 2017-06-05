/* test for _XmRegionOffset */

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

  _XmRegionOffset(region1, 10, 10);

  printf ("\tRegion 1 -- after _XmRegionOffset\n");
  print_region(region1);
  exit(0);
}
