/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-pam.h - Common PAM definitions

   Copyright (C) 2007 Stef Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef GKRPAM_H_
#define GKRPAM_H_

#include <pwd.h>

#ifndef LOG_AUTHPRIV
#define LOG_AUTHPRIV    LOG_AUTH
#endif

#define GKR_LOG_ERR     (LOG_ERR | LOG_AUTHPRIV)
#define GKR_LOG_WARN    (LOG_WARNING | LOG_AUTHPRIV)
#define GKR_LOG_NOTICE  (LOG_NOTICE | LOG_AUTHPRIV)
#define GKR_LOG_INFO    (LOG_INFO | LOG_AUTHPRIV)

int       gkr_pam_client_run_operation (struct passwd *pwd, const char *socket,
                                        int op, int argc, const char* argv[]);

#endif /*GKRPAM_H_*/
