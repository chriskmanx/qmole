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
#ifndef BONOBO_ACTIVATION_SHLIB_H
#define BONOBO_ACTIVATION_SHLIB_H

#include <bonobo-activation/Bonobo_Activation_types.h>

G_BEGIN_DECLS

typedef struct {
	const char *iid;

	/* This routine should call bonobo_activation_plugin_use(servant, impl_ptr), 
         * as should all routines which activate CORBA objects
	 * implemented by this shared library. This needs to be done 
         * before making any CORBA calls on the object, or
	 * passing that object around. First thing after servant creation 
         * always works. :) 
         */

        CORBA_Object (*activate) (PortableServer_POA poa,
                                  const char *iid, 
                                  gpointer impl_ptr,	/* This pointer should be stored by the implementation
                                                         * to be passed to bonobo_activation_plugin_unuse() in the 
                                                         * implementation's destruction routine. */
				  CORBA_Environment *ev);
        gpointer dummy[4];
} BonoboActivationPluginObject;

typedef struct {
	const BonoboActivationPluginObject *plugin_object_list;
	const char *description;
        gpointer dummy[8];
} BonoboActivationPlugin;

void  bonobo_activation_plugin_use    (PortableServer_Servant servant, 
                                       gpointer impl_ptr);

void  bonobo_activation_plugin_unuse  (gpointer impl_ptr);


CORBA_Object bonobo_activation_activate_shlib_server (Bonobo_ActivationResult *sh, 
                                                      CORBA_Environment    *ev);

G_END_DECLS

#endif /* BONOBO_ACTIVATION_SHLIB_H */
