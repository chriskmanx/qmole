/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-ssh-agent.c - handles SSH i/o from the clients

   Copyright (C) 2007 Stefan Walter

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef GKDSSHAGENT_H_
#define GKDSSHAGENT_H_

#include <glib.h>

#include "pkcs11/pkcs11.h"

int               gkd_ssh_agent_startup                 (const gchar *prefix);

void              gkd_ssh_agent_accept                  (void);

void              gkd_ssh_agent_shutdown                (void);

gboolean          gkd_ssh_agent_initialize              (CK_FUNCTION_LIST_PTR funcs);

void              gkd_ssh_agent_uninitialize            (void);

#endif /* GKDSSHAGENT_H_ */
