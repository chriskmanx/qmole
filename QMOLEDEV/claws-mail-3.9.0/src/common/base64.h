/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail teams
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

#ifndef __BASE64_H__
#define __BASE64_H__

#include <glib.h>

#define B64LEN(len)	((len) / 3 * 4 + ((len) % 3 ? 4 : 0))

typedef struct _Base64Decoder	Base64Decoder;

struct _Base64Decoder
{
	gint buf_len;
	gchar buf[4];
};

void base64_encode	(gchar		*out,
			 const guchar	*in,
			 gint		 inlen);
gint base64_decode	(guchar		*out,
			 const gchar	*in,
			 gint		 inlen);

Base64Decoder *base64_decoder_new	(void);
void	       base64_decoder_free	(Base64Decoder	*decoder);
gint	       base64_decoder_decode	(Base64Decoder	*decoder,
					 const gchar	*in,
					 guchar		*out,
					 gint           inlen);

#endif /* __BASE64_H__ */
