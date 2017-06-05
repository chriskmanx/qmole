/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  oaf-async: A library for accessing oafd in a nice way.
 *
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
 *  Author: Mathieu Lacage <mathieu@eazel.com>
 *
 */


#ifndef BONOBO_ACTIVATION_ASYNC_H
#define BONOBO_ACTIVATION_ASYNC_H

#include <bonobo-activation/Bonobo_Activation_types.h>

G_BEGIN_DECLS

/* activated_object is CORBA_OBJECT_NIL if the activation 
   failed somehow. If this is the case, error_reason contains
   a valid string which describes the pb encountered.
   If this is not the case, error_reason is not defined.
   activated_object should be CORBA_Object_release'd by the user
*/
typedef void (*BonoboActivationCallback) (CORBA_Object   activated_object, 
                                          const char    *error_reason, 
                                          gpointer       user_data);


void bonobo_activation_activate_async (const char *requirements,
                                       char *const *selection_order,
                                       Bonobo_ActivationFlags flags,
                                       BonoboActivationCallback callback,
                                       gpointer user_data,
                                       CORBA_Environment * ev);

void bonobo_activation_activate_from_id_async (const Bonobo_ActivationID aid,
                                               Bonobo_ActivationFlags flags,
                                               BonoboActivationCallback callback,
                                               gpointer user_data,
                                               CORBA_Environment * ev);

G_END_DECLS

#endif /* BONOBO_ACTIVATION_ASYNC_H */

