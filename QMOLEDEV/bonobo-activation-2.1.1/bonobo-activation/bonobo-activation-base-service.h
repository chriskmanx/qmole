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
 *
 */
/* The  folowing API is not intended for application use.
 * It is intended only for people who want to extend OAF bootstraping system.
 * I have no idea why we have all this tralala but Eliot knows and he _tried_
 * to explain it in docs/bonobo-activation-base-service.txt
 */

/*
 * DO NOT USE this API, it is deprecated and crufty.
 */

#ifndef BONOBO_ACTIVATION_BASE_SERVICE_H
#define BONOBO_ACTIVATION_BASE_SERVICE_H

#ifndef BONOBO_DISABLE_DEPRECATED

#include <glib.h>
#include <orbit/orbit.h>

G_BEGIN_DECLS

typedef struct {
	const char *name;
	const char *session_name;
	const char *username;
        const char *hostname;
        const char *domain; /* FIXME: unused - remove ? */
} BonoboActivationBaseService;

typedef struct _BonoboActivationBaseServiceRegistry BonoboActivationBaseServiceRegistry;

struct _BonoboActivationBaseServiceRegistry {
	void   (*lock)         (const BonoboActivationBaseServiceRegistry *registry,
                                gpointer                                   user_data);
	void   (*unlock)       (const BonoboActivationBaseServiceRegistry *registry,
                                gpointer                      user_data);
	char * (*check)        (const BonoboActivationBaseServiceRegistry *registry,
                                const BonoboActivationBaseService         *base_service,
                                int                          *ret_distance, 
                                gpointer                      user_data);
	void   (*register_new) (const BonoboActivationBaseServiceRegistry *registry,
                                const char                   *ior,
                                const BonoboActivationBaseService         *base_service, 
                                gpointer                      user_data);
	void   (*unregister)   (const BonoboActivationBaseServiceRegistry *registry,
                                const char                   *ior,
                                const BonoboActivationBaseService         *base_service,
                                gpointer                      user_data);
};

typedef CORBA_Object (*BonoboActivationBaseServiceActivator) (
                                const BonoboActivationBaseService *base_service,
                                const char                       **command,
                                int                                ior_fd,
                                CORBA_Environment                 *ev);


void         bonobo_activation_base_service_registry_add
                               (const BonoboActivationBaseServiceRegistry *registry,
                                int                                        priority, 
                                gpointer                                   user_data);

CORBA_Object bonobo_activation_base_service_check
                               (const BonoboActivationBaseService *base_service,
                                CORBA_Environment                 *ev);

void         bonobo_activation_base_service_set
                               (const BonoboActivationBaseService         *base_service,
                                CORBA_Object                               obj, 
                                CORBA_Environment                         *ev);
void         bonobo_activation_base_service_unset
                               (const BonoboActivationBaseService         *base_service,
                                CORBA_Object                               obj, 
                                CORBA_Environment                         *ev);

/* Do not release() the returned value */
CORBA_Object bonobo_activation_service_get
                               (const BonoboActivationBaseService         *base_service);

void         bonobo_activation_base_service_activator_add
                               (BonoboActivationBaseServiceActivator       activator,
                                int                                        priority);
void         bonobo_activation_base_service_debug_shutdown
                               (CORBA_Environment                         *ev);

G_END_DECLS

#endif /* BONOBO_DISABLE_DEPRECATED */

#endif /* BONOBO_ACTIVATION_BASE_SERVICE_H */
