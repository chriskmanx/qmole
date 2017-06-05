/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef GKDGPGPRIVATE_H_
#define GKDGPGPRIVATE_H_

#include "egg/egg-buffer.h"

#include "pkcs11/pkcs11.h"

#include <gck/gck.h>

#include <glib.h>

typedef struct _GkdGpgAgentCall {
	int sock;
	GckModule *module;
	GIOChannel *channel;
	gboolean terminal_ok;
} GkdGpgAgentCall;

/* -----------------------------------------------------------------------------
 * GPG CONSTANTS
 */


/* Commands */
#define GPG_AGENT_ID      "AGENT_ID"
#define GPG_AGENT_NOP     "NOP"
#define GPG_AGENT_BYE     "BYE"
#define GPG_AGENT_RESET   "RESET"
#define GPG_AGENT_OPTION  "OPTION"
#define GPG_AGENT_GETPASS "GET_PASSPHRASE"
#define GPG_AGENT_CLRPASS "CLEAR_PASSPHRASE"
#define GPG_AGENT_GETINFO "GETINFO"

#define GPG_AGENT_OPT_DISPLAY "display="

/* Options */
#define GPG_AGENT_FLAG_DATA   "data"
#define GPG_AGENT_FLAG_CHECK  "check"
#define GPG_AGENT_FLAG_REPEAT "repeat"

/* Responses */
#define GPG_AGENT_OK      "OK "
#define GPG_AGENT_ERR     "ERR "
#define GPG_AGENT_DATA    "D "

/* -----------------------------------------------------------------------------
 * gkd-gpg-agent-ops.c
 */

/* -----------------------------------------------------------------------------
 * gkd-gpg-agent.c
 */

gboolean              gkd_gpg_agent_initialize_with_module          (GckModule *module);

GckSession*          gkd_gpg_agent_checkout_main_session           (void);

void                  gkd_gpg_agent_checkin_main_session            (GckSession* session);

gboolean              gkd_gpg_agent_send_reply                      (GkdGpgAgentCall *call,
                                                                     gboolean ok,
                                                                     const gchar *response);

gboolean              gkd_gpg_agent_send_data                       (GkdGpgAgentCall *call,
                                                                     const gchar *data);

GSettings*            gkd_gpg_agent_settings                        (void);

/* -----------------------------------------------------------------------------
 * gkd-gpg-agent-ops
 */


gboolean              gkd_gpg_agent_ops_options                     (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_getpass                     (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_clrpass                     (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_getinfo                     (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_nop                         (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_bye                         (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_reset                       (GkdGpgAgentCall *call,
                                                                     gchar *args);

gboolean              gkd_gpg_agent_ops_id                          (GkdGpgAgentCall *call,
                                                                     gchar *args);

#endif /*GKDGPGPRIVATE_H_*/
