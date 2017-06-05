/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#include <glib.h>
#include <ctype.h>
#include <string.h>

#include "base64.h"
#include "utils.h"

static const gchar base64char[64] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static const signed char base64val[128] = {
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
	-1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
	15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
	-1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
	41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1
};

#define BASE64VAL(c)	(IS_ASCII(c) ? base64val[(gint) (c)] : -1)

void base64_encode(gchar *out, const guchar *in, gint inlen)
{
	const guchar *inp = in;
	gchar *outp = out;

	while (inlen >= 3) {
		*outp++ = base64char[(inp[0] >> 2) & 0x3f];
		*outp++ = base64char[((inp[0] & 0x03) << 4) |
				     ((inp[1] >> 4) & 0x0f)];
		*outp++ = base64char[((inp[1] & 0x0f) << 2) |
				     ((inp[2] >> 6) & 0x03)];
		*outp++ = base64char[inp[2] & 0x3f];

		inp += 3;
		inlen -= 3;
	}

	if (inlen > 0) {
		*outp++ = base64char[(inp[0] >> 2) & 0x3f];
		if (inlen == 1) {
			*outp++ = base64char[(inp[0] & 0x03) << 4];
			*outp++ = '=';
		} else {
			*outp++ = base64char[((inp[0] & 0x03) << 4) |
					     ((inp[1] >> 4) & 0x0f)];
			*outp++ = base64char[((inp[1] & 0x0f) << 2)];
		}
		*outp++ = '=';
	}

	*outp = '\0';
}

gint base64_decode(guchar *out, const gchar *in, gint inlen)
{
	const gchar *inp = in;
	guchar *outp = out;
	gchar buf[4];

	if (inlen < 0)
		inlen = G_MAXINT;

	while (inlen >= 4 && *inp != '\0') {
		buf[0] = *inp++;
		inlen--;
		if (BASE64VAL(buf[0]) == -1) break;

		buf[1] = *inp++;
		inlen--;
		if (BASE64VAL(buf[1]) == -1) break;

		buf[2] = *inp++;
		inlen--;
		if (buf[2] != '=' && BASE64VAL(buf[2]) == -1) break;

		buf[3] = *inp++;
		inlen--;
		if (buf[3] != '=' && BASE64VAL(buf[3]) == -1) break;

		*outp++ = ((BASE64VAL(buf[0]) << 2) & 0xfc) |
			  ((BASE64VAL(buf[1]) >> 4) & 0x03);
		if (buf[2] != '=') {
			*outp++ = ((BASE64VAL(buf[1]) & 0x0f) << 4) |
				  ((BASE64VAL(buf[2]) >> 2) & 0x0f);
			if (buf[3] != '=') {
				*outp++ = ((BASE64VAL(buf[2]) & 0x03) << 6) |
					   (BASE64VAL(buf[3]) & 0x3f);
			}
		}
	}

	return outp - out;
}

Base64Decoder *base64_decoder_new(void)
{
	Base64Decoder *decoder;

	decoder = g_new0(Base64Decoder, 1);
	return decoder;
}

void base64_decoder_free(Base64Decoder *decoder)
{
	g_free(decoder);
}

gint base64_decoder_decode(Base64Decoder *decoder,
			   const gchar *in, guchar *out, gint inlen)
{
	const gchar *in_end = in + inlen;
	gint len, total_len = 0;
	gboolean in_more = inlen > 0;
	gboolean got_eq = FALSE;
	gint buf_len;
	gchar buf[4];

	cm_return_val_if_fail(decoder != NULL, -1);
	cm_return_val_if_fail(in != NULL, -1);
	cm_return_val_if_fail(out != NULL, -1);

	/* Start with previous saved tail */	
	buf_len = decoder->buf_len;
	memcpy(buf, decoder->buf, sizeof(buf));

	while (in_more) {
		while (buf_len < 4 && in_more) {
			gchar c = *in;

			in++;
			got_eq = (c == '=');
			if (got_eq || BASE64VAL(c) >= 0)
				buf[buf_len++] = c;
			in_more = (in < in_end) && !(got_eq && (buf_len == 4));
		}
		if (buf_len == 4) {
			len = base64_decode(out, buf, 4);
			out += len;
			total_len += len;
			buf_len = 0;
		}
	}
	if (buf_len < 4) { 
		/* Save tail for next iteration call. It wll be ignored if ends here. */
		decoder->buf_len = buf_len;
		memcpy(decoder->buf, buf, buf_len);
	}
	return total_len;
}
