/* perform a symmetricality of conversion test on a set of randomized
 * RGBA data */

#include "config.h"
#include <stdlib.h>
#include <math.h>
#include "babl-internal.h"

#define ERROR_TOLERANCE    0.5

static int OK = 1;

static int
each_conversion (Babl *babl,
                 void *userdata)
{
  double error = babl->conversion.error;

  if (error >= ERROR_TOLERANCE)
    {
      babl_log ("%s\terror:%f", babl->instance.name, error);
      OK = 0;
    }
  return 0;
}

int main (void)
{
  babl_init ();

  babl_set_extender (babl_extension_quiet_log ());
  babl_conversion_class_for_each (each_conversion, NULL);

  babl_exit ();

  return !OK;
}
