/* test for _XmRegionCreate and _XmRegionCreateSize */

#include <Xm/XmP.h>
#include <stdio.h>

extern void print_region();

int
main(int argc,
     char **argv)
{
  XmRegion region1 = _XmRegionCreate();
  XmRegion region2 = _XmRegionCreateSize(100);

  printf ("\tRegion 1 -- _XmRegionCreate()\n");
  print_region(region1);
  printf ("\tRegion 2 -- _XmRegionCreateSize(100)\n");
  print_region(region2);
  exit(0);
}
