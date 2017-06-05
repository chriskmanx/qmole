#ifndef BONOBO_ACTIVATION_CLIENT_H
#define BONOBO_ACTIVATION_CLIENT_H

#include <glib/gutils.h>
#include <bonobo-activation-private.h>

void bonobo_activation_release_corba_client (void);
void bonobo_activation_add_reset_notify     (GVoidFunc fn);
void bonobo_activation_register_client      (Bonobo_ActivationContext context,
					     CORBA_Environment       *ev);

#endif /* BONOBO_ACTIVATION_CLIENT_H */
