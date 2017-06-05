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
#ifndef BONOBO_ACTIVATION_INIT_H
#define BONOBO_ACTIVATION_INIT_H

#include <orbit/orbit.h>
#include <popt.h>

G_BEGIN_DECLS

CORBA_ORB      bonobo_activation_orb_init   (int   *argc, 
                                             char **argv);
CORBA_ORB      bonobo_activation_orb_get    (void);

gboolean       bonobo_activation_is_initialized   (void);
CORBA_ORB      bonobo_activation_init       (int      argc, 
                                             char   **argv);
void           bonobo_activation_preinit    (gpointer app, 
                                             gpointer mod_info);
void           bonobo_activation_postinit   (gpointer app, 
                                             gpointer mod_info);

CORBA_Context  bonobo_activation_context_get      (void);

const char    *bonobo_activation_hostname_get     (void);
const char    *bonobo_activation_session_name_get (void);
/* FIXME: unused - remove ? */
const char    *bonobo_activation_domain_get       (void);
#define bonobo_activation_username_get() g_get_user_name()

char          *bonobo_activation_get_popt_table_name (void);
gboolean       bonobo_activation_debug_shutdown      (void);

extern struct poptOption bonobo_activation_popt_options[];

G_END_DECLS

#endif /* BONOBO_ACTIVATION_INIT_H */

