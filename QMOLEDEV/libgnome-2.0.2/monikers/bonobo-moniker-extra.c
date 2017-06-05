#include "config.h"
#include <string.h>

#include <bonobo/bonobo-shlib-factory.h>
#include "bonobo-moniker-extra.h"

static BonoboObject *
bonobo_extra_moniker_factory (BonoboGenericFactory *this,
			      const char           *object_id,
			      void                 *data)
{
        g_return_val_if_fail (object_id != NULL, NULL);

	if (!strcmp (object_id, "OAFIID:GNOME_Moniker_Config")) {
		return BONOBO_OBJECT (bonobo_moniker_simple_new (
			"config:", bonobo_moniker_config_resolve));

	} else if (!strcmp (object_id, "OAFIID:GNOME_Moniker_ConfIndirect")) {
                return BONOBO_OBJECT (bonobo_moniker_simple_new (
                        "conf_indirect:", bonobo_moniker_conf_indirect_resolve));

	} else
                g_warning ("Failing to manufacture a '%s'", object_id);

	return NULL;
}

BONOBO_ACTIVATION_SHLIB_FACTORY ("OAFIID:GNOME_Moniker_std_Factory",
                                 "Extra bonobo moniker",
                                 bonobo_extra_moniker_factory, NULL);
