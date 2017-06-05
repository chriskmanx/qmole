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
#ifndef BONOBO_ACTIVATION_REGISTER_H
#define BONOBO_ACTIVATION_REGISTER_H

#include <orbit/orbit.h>
#include <bonobo-activation/Bonobo_Activation_types.h>

G_BEGIN_DECLS

Bonobo_RegistrationResult
	bonobo_activation_register_active_server      (const char   *iid,
						       CORBA_Object  obj,
						       GSList       *reg_env);
void    bonobo_activation_unregister_active_server    (const char   *iid, 
						       CORBA_Object  obj);

GSList *bonobo_activation_registration_env_set        (GSList       *reg_env,
						       const char   *name,
						       const char   *value);
void    bonobo_activation_registration_env_free       (GSList       *reg_env);

void    bonobo_activation_registration_env_set_global (GSList       *reg_env,
						       gboolean      append_if_existing);
GSList *bonobo_activation_registration_env_get_global (void);


#ifndef BONOBO_DISABLE_DEPRECATED

Bonobo_RegistrationResult
	bonobo_activation_active_server_register   (const char   *registration_id,
						    CORBA_Object  obj);

void    bonobo_activation_active_server_unregister (const char   *iid, 
						    CORBA_Object  obj);

char       *bonobo_activation_make_registration_id (const char *iid, 
						    const char *display);
#endif /* BONOBO_DISABLE_DEPRECATED */


const char *bonobo_activation_iid_get       (void);

G_END_DECLS

#endif /* BONOBO_ACTIVATION_REGISTER_H */
