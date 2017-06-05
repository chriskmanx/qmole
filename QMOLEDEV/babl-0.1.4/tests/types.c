#include "config.h"
#include <math.h>
#include "babl-internal.h"

int OK = 1;

static int type_check (Babl *babl,
                       void *userdata)
{
  if (!babl_type_is_symmetric (babl))
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
  babl_type_class_for_each (type_check, NULL);

  babl_exit ();

  return !OK;
}
