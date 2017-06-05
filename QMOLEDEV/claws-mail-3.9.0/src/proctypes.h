/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2012 the Claws Mail team
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

#ifndef PROCTYPES_H
#define PROCTYPES_H

#include <glib.h>

struct _MsgInfo;
typedef struct _MsgInfo			MsgInfo;

struct _MsgFlags;
typedef struct _MsgFlags		MsgFlags;

typedef guint32 MsgPermFlags;
typedef guint32 MsgTmpFlags;

struct _MsgFileInfo;
typedef struct _MsgFileInfo     	MsgFileInfo;

struct _MsgInfoUpdate;
typedef struct _MsgInfoUpdate 		MsgInfoUpdate;

struct _MailFilteringData;
typedef struct _MailFilteringData	MailFilteringData;

struct _MsgInfoExtraData;
typedef struct _MsgInfoExtraData	MsgInfoExtraData;

typedef GSList MsgInfoList;
typedef GSList MsgNumberList;



struct _HeaderEntry;
typedef struct _HeaderEntry	HeaderEntry;

struct _Header;
typedef struct _Header		Header;



struct _MimeType;
typedef struct _MimeType	MimeType;

struct _MimeInfo;
typedef struct _MimeInfo	MimeInfo;

struct _MimeParser;
typedef struct _MimeParser	MimeParser;



#endif
