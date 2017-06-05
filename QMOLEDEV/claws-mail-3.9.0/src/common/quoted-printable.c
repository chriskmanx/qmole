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

#include "utils.h"

#define MAX_LINELEN	76

#define IS_LBREAK(p) \
	(*(p) == '\0' || *(p) == '\n' || (*(p) == '\r' && *((p) + 1) == '\n'))

#define SOFT_LBREAK_IF_REQUIRED(n)					\
	if (len + (n) > MAX_LINELEN ||					\
	    (len + (n) == MAX_LINELEN && (!IS_LBREAK(inp + 1)))) {	\
		*outp++ = '=';						\
		*outp++ = '\n';						\
		len = 0;						\
	}

void qp_encode_line(gchar *out, const guchar *in)
{
	const guchar *inp = in;
	gchar *outp = out;
	guchar ch;
	gint len = 0;

	while (*inp != '\0') {
		ch = *inp;

		if (IS_LBREAK(inp)) {
			*outp++ = '\n';
			len = 0;
			if (*inp == '\r')
				inp++;
			inp++;
		} else if (ch == '\t' || ch == ' ') {
			if (IS_LBREAK(inp + 1)) {
				SOFT_LBREAK_IF_REQUIRED(3);
				*outp++ = '=';
				get_hex_str(outp, ch);
				outp += 2;
				len += 3;
				inp++;
			} else {
				SOFT_LBREAK_IF_REQUIRED(1);
				*outp++ = *inp++;
				len++;
			}
		} else if ((ch >= 33 && ch <= 60) || (ch >= 62 && ch <= 126)) {
			SOFT_LBREAK_IF_REQUIRED(1);
			*outp++ = *inp++;
			len++;
		} else {
			SOFT_LBREAK_IF_REQUIRED(3);
			*outp++ = '=';
			get_hex_str(outp, ch);
			outp += 2;
			len += 3;
			inp++;
		}
	}

	if (len > 0)
		*outp++ = '\n';

	*outp = '\0';
}

gint qp_decode_line(gchar *str)
{
	gchar *inp = str, *outp = str;

	while (*inp != '\0') {
		if (*inp == '=') {
			if (inp[1] && inp[2] &&
			    get_hex_value((guchar *)outp, inp[1], inp[2])
			    == TRUE) {
				inp += 3;
			} else if (inp[1] == '\0' || g_ascii_isspace(inp[1])) {
				/* soft line break */
				break;
			} else {
				/* broken QP string */
				*outp = *inp++;
			}
		} else {
			*outp = *inp++;
		}
		outp++;
	}

	*outp = '\0';

	return outp - str;
}

gint qp_decode_const(gchar *out, gint avail, const gchar *str)
{
	const gchar *inp = str;
	gchar *outp = out;

	while (*inp != '\0' && avail > 0) {
		if (*inp == '=') {
			if (inp[1] && inp[2] &&
			    get_hex_value((guchar *)outp, inp[1], inp[2])
			    == TRUE) {
				inp += 3;
			} else if (inp[1] == '\0' || g_ascii_isspace(inp[1])) {
				/* soft line break */
				break;
			} else {
				/* broken QP string */
				*outp = *inp++;
			}
		} else {
			*outp = *inp++;
		}
		outp++;
		avail--;
	}

	*outp = '\0';

	return outp - out;
}

gint qp_decode_q_encoding(guchar *out, const gchar *in, gint inlen)
{
	const gchar *inp = in;
	guchar *outp = out;

	if (inlen < 0)
		inlen = G_MAXINT;

	while (inp - in < inlen && *inp != '\0') {
		if (*inp == '=' && inp + 3 - in <= inlen) {
			if (get_hex_value(outp, inp[1], inp[2]) == TRUE) {
				inp += 3;
			} else {
				*outp = *inp++;
			}
		} else if (*inp == '_') {
			*outp = ' ';
			inp++;
		} else {
			*outp = *inp++;
		}
		outp++;
	}

	*outp = '\0';

	return outp - out;
}

gint qp_get_q_encoding_len(const guchar *str)
{
	const guchar *inp = str;
	gint len = 0;

	while (*inp != '\0') {
		if (*inp == 0x20)
			len++;
		else if (*inp == '=' || *inp == '?' || *inp == '_' ||
			 *inp < 32 || *inp > 127 || g_ascii_isspace(*inp))
			len += 3;
		else
			len++;

		inp++;
	}

	return len;
}

void qp_q_encode(gchar *out, const guchar *in)
{
	const guchar *inp = in;
	gchar *outp = out;

	while (*inp != '\0') {
		if (*inp == 0x20)
			*outp++ = '_';
		else if (*inp == '=' || *inp == '?' || *inp == '_' ||
			 *inp < 32 || *inp > 127 || g_ascii_isspace(*inp)) {
			*outp++ = '=';
			get_hex_str(outp, *inp);
			outp += 2;
		} else
			*outp++ = *inp;

		inp++;
	}

	*outp = '\0';
}
