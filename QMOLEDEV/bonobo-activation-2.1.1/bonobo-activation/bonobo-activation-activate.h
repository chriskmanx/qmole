/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  bonobo-activation: A library for accessing bonobo-activation-server.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Author: Elliot Lee <sopwith@redhat.com>
 */
#ifndef BONOBO_ACTIVATION_ACTIVATE_H
#define BONOBO_ACTIVATION_ACTIVATE_H

#include <bonobo-activation/Bonobo_Activation_types.h>

G_BEGIN_DECLS

CORBA_Object bonobo_activation_name_service_get (CORBA_Environment * ev);


Bonobo_ServerInfoList *bonobo_activation_query   (const char *requirements,
                                                  char *const *selection_order,
                                                  CORBA_Environment * ev);
CORBA_Object bonobo_activation_activate          (const char *requirements,
                                                  char *const *selection_order,
                                                  Bonobo_ActivationFlags flags,
                                                  Bonobo_ActivationID * ret_aid,
                                                  CORBA_Environment * ev);
CORBA_Object bonobo_activation_activate_from_id  (const Bonobo_ActivationID aid,
                                                  Bonobo_ActivationFlags flags,
                                                  Bonobo_ActivationID * ret_aid,
                                                  CORBA_Environment * ev);

void         bonobo_activation_set_activation_env_value (const char *name,
							 const char *value);

/* debugging functions. */
void         bonobo_activation_set_test_components_enabled (gboolean val);
gboolean     bonobo_activation_get_test_components_enabled (void);

G_END_DECLS

#endif /* BONOBO_ACTIVATION_ACTIVATE_H */
