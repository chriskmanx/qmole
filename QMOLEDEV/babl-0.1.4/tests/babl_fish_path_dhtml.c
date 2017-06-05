#include "config.h"
#include <stdlib.h>
#include "babl-internal.h"

int main (void)
{
  babl_init ();

  babl_set_extender (babl_extension_quiet_log ());
  babl_fish_stats (stdout);

  babl_exit ();

  return 0;
}
