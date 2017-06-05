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
#ifndef BONOBO_ACTIVATION_ID_H
#define BONOBO_ACTIVATION_ID_H

#ifndef BONOBO_DISABLE_DEPRECATED

#include <bonobo-activation/Bonobo_Activation_types.h>

G_BEGIN_DECLS

/* If you wish to manipulate the internals of this structure, please
   use g_malloc/g_free to allocate memory. */
typedef struct
{
	char *iid;		/* Implementation ID */
	char *user;		/* user name */
	char *host;		/* DNS name or IP address */
	char *domain;		/* FIXME: unused - remove ? */
}
BonoboActivationInfo;


Bonobo_ActivationID    bonobo_activation_info_stringify      (const BonoboActivationInfo *actinfo);
BonoboActivationInfo  *bonobo_activation_servinfo_to_actinfo (const Bonobo_ServerInfo    *servinfo);
BonoboActivationInfo  *bonobo_activation_id_parse            (const CORBA_char           *actid);
BonoboActivationInfo  *bonobo_activation_info_new            (void);
void                   bonobo_activation_info_free           (BonoboActivationInfo       *actinfo);

G_END_DECLS

#endif /* BONOBO_DISABLE_DEPRECATED */

#endif /* BONOBO_ACTIVATION_ID_H */
