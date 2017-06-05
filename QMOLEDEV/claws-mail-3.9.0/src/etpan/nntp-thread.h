/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2012 DINH Viet Hoa and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef NNTP_THREAD_H

#define NNTP_THREAD_H

#include <libetpan/libetpan.h>
#include "folder.h"

void nntp_main_set_timeout(int sec);
void nntp_main_init(gboolean skip_ssl_cert_check);
void nntp_main_done(gboolean have_connectivity);

void nntp_init(Folder * folder);
void nntp_done(Folder * folder);

int nntp_threaded_connect(Folder * folder, const char * server, int port);
int nntp_threaded_connect_ssl(Folder * folder, const char * server, int port);

void nntp_threaded_disconnect(Folder * folder);

void nntp_threaded_cancel(Folder * folder);

int nntp_threaded_login(Folder * folder, const char * login, const char * password);
int nntp_threaded_date(Folder * folder, struct tm *lt);
int nntp_threaded_list(Folder * folder, clist **grouplist);
int nntp_threaded_post(Folder * folder, char *contents, size_t len);
int nntp_threaded_article(Folder * folder, guint32 num, char **contents, size_t *len);
int nntp_threaded_group(Folder * folder, const char *group, struct newsnntp_group_info **info);
int nntp_threaded_mode_reader(Folder * folder);
int nntp_threaded_xover(Folder * folder, guint32 beg, guint32 end, struct newsnntp_xover_resp_item **single_result, clist **multiple_result);
int nntp_threaded_xhdr(Folder * folder, const char *header, guint32 beg, guint32 end, clist **hdrlist);

#endif
