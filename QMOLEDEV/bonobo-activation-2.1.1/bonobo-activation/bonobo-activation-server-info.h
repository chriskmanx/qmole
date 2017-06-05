/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  liboaf: A library for accessing oafd in a nice way.
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

#ifndef BONOBO_ACTIVATION_SERVER_INFO_H
#define BONOBO_ACTIVATION_SERVER_INFO_H

#include <bonobo-activation/Bonobo_Activation_types.h>

G_BEGIN_DECLS

Bonobo_ActivationProperty *bonobo_server_info_prop_find        (Bonobo_ServerInfo                      *server,
                                                                const char                             *prop_name);
const char                *bonobo_server_info_prop_lookup      (Bonobo_ServerInfo                      *server,
                                                                const char                             *prop_name,
                                                                GSList                                 *i18n_languages);
void                       Bonobo_ActivationPropertyValue_copy           (Bonobo_ActivationPropertyValue                   *copy,
                                                                          const Bonobo_ActivationPropertyValue             *original);
void                       Bonobo_ActivationProperty_copy                (Bonobo_ActivationProperty                        *copy,
                                                                          const Bonobo_ActivationProperty                  *original);
void                       CORBA_sequence_Bonobo_ActivationProperty_copy (CORBA_sequence_Bonobo_ActivationProperty         *copy,
                                                                          const CORBA_sequence_Bonobo_ActivationProperty   *original);
void                       Bonobo_ServerInfo_copy              (Bonobo_ServerInfo                      *copy, 
                                                                const Bonobo_ServerInfo                *original);
Bonobo_ServerInfo         *Bonobo_ServerInfo_duplicate         (const Bonobo_ServerInfo                *original);
Bonobo_ServerInfoList     *Bonobo_ServerInfoList_duplicate     (const Bonobo_ServerInfoList            *original);

G_END_DECLS

#endif /* BONOBO_ACTIVATION_SERVER_INFO_H */


