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

#ifndef ETPAN_ERRORS_H

#define ETPAN_ERRORS_H

enum {
  NO_ERROR = 0,
  ERROR_CACHE,
  ERROR_MEMORY,
  ERROR_CONFIG,
  ERROR_INVAL,
  ERROR_PARSE,
  ERROR_MOVE,
  ERROR_COPY,
  ERROR_MSG_LIST,
  ERROR_ENV_LIST,
  ERROR_CONNECT, /* 10 */
  ERROR_FETCH,
  ERROR_APPEND,
  ERROR_SEND,
  ERROR_NOT_IMPLEMENTED,
  ERROR_FILE,
  ERROR_TOO_MUCH_RECIPIENT,
  ERROR_COMMAND,
  ERROR_STREAM,
  ERROR_CANCELLED,
  ERROR_CREATE, /* 20 */
  ERROR_UNKNOWN_PROTOCOL,
  ERROR_COULD_NOT_ENCRYPT,
  ERROR_CHECK,
  ERROR_STATUS,
  ERROR_EXPUNGE,
  ERROR_MARK_AS_SPAM,
  ERROR_BUSY,
  ERROR_NEWSGROUPS_LIST,
  ERROR_IMAP_MAILBOX_LIST,
  ERROR_IMAP_SELECT, /* 30 */
  ERROR_IMAP_CREATE,
  ERROR_PRIVACY,
  ERROR_DIRECTORY_NOT_FOUND,
  ERROR_LDAP_SEARCH,
  ERROR_NOT_SUPPORTED,
  ERROR_COULD_NOT_POSTPONE,
  ERROR_NO_FROM,
  ERROR_AUTH,
};

#endif
