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
#ifndef BONOBO_ACTIVATION_PRIVATE_H
#define BONOBO_ACTIVATION_PRIVATE_H

#include <config.h>
#include <string.h>
#include <bonobo-activation/bonobo-activation-base-service.h>
#include <bonobo-activation/Bonobo_ActivationContext.h>

#define BONOBO_ACTIVATION_FACTORY_TIMEOUT 1000

void         bonobo_activation_timeout_reg_check_set  (gboolean           on);
gboolean     bonobo_activation_timeout_reg_check      (gpointer           data);

typedef CORBA_Object (*BonoboForkReCheckFn)      (const Bonobo_ActivationEnvironment *environemnt,
                                                  const char                         *act_iid,
                                                  gpointer                            user_data,
                                                  CORBA_Environment                  *ev);
CORBA_Object bonobo_activation_server_by_forking (const char                        **cmd, 
						  gboolean                            set_process_group,
						  int                                 fd_arg,
						  const Bonobo_ActivationEnvironment *environemnt,
						  const char                         *od_iorstr,
						  const char                         *act_iid,
						  BonoboForkReCheckFn                 re_check,
						  gpointer                            user_data,
						  CORBA_Environment                  *ev);

void         bonobo_activation_base_service_init      (void);
int          bonobo_activation_ior_fd_get             (void);
CORBA_Object bonobo_activation_activation_context_get (void);
CORBA_Object bonobo_activation_object_directory_get   (const char        *username,
                                                       const char        *hostname);
void         bonobo_activation_init_activation_env    (void);

extern gboolean bonobo_activation_private;

CORBA_Object bonobo_activation_internal_activation_context_get_extended (
                                                       gboolean           existing_only,
                                                       CORBA_Environment *ev);

CORBA_Object bonobo_activation_internal_service_get_extended (
                                                       const BonoboActivationBaseService *base_service,
                                                       gboolean           existing_only,
                                                       CORBA_Environment *ev);

gboolean Bonobo_ActivationEnvironment_match (const Bonobo_ActivationEnvironment *a,
					     const Bonobo_ActivationEnvironment *b);

void Bonobo_ActivationEnvValue_set  (Bonobo_ActivationEnvValue *env,
				     const char                *name,
				     const char                *value);
void Bonobo_ActivationEnvValue_copy (Bonobo_ActivationEnvValue *dest,
				     Bonobo_ActivationEnvValue *src);


#endif /* BONOBO_ACTIVATION_PRIVATE_H */

