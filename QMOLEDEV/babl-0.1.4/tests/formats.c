/* perform a symmetricality of conversion test on a set of randomized
 * RGBA data */

#include "config.h"
#include <stdlib.h>
#include <math.h>
#include "babl-internal.h"


static int format_check (Babl *babl,
                         void *userdata)
{
  babl_log ("%s\tloss: %f", babl->instance.name, babl->format.loss);
  return 0;
}

int main (void)
{
  babl_init ();

  babl_set_extender (babl_extension_quiet_log ());
  babl_format_class_for_each (format_check, (void *) 1);

  babl_exit ();

  return 0;
}
