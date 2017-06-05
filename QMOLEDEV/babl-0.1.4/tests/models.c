/* perform a symmetricality of conversion test on a set of randomized
 * RGBA data */

#include "config.h"
#include <stdlib.h>
#include <math.h>
#include "babl-internal.h"

int OK = 1;


static int model_check (Babl *babl,
                        void *userdata)
{
  if (!babl_model_is_symmetric (babl))
    {
      babl_log ("%s is not symmetric", babl->instance.name);
      OK = 0;
    }
  return 0;
}


int main (void)
{
  babl_init ();

  babl_set_extender (babl_extension_quiet_log ());
  babl_model_class_for_each (model_check, NULL);

  babl_exit ();

  return !OK;
}
