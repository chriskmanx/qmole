/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999,2000 Hiroyuki Yamamoto
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
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "enriched.h"
#include "utils.h"

#define ERTFBUFSIZE	8192


static ERTFState ertf_read_line		(ERTFParser	*parser);
static void ertf_append_char		(ERTFParser	*parser,
					 gchar		 ch);

static ERTFState ertf_parse_tag		(ERTFParser	*parser);
static void ertf_get_parenthesis	(ERTFParser	*parser, 
					 gchar		*buf, 
					 gint 		 len);

ERTFParser *ertf_parser_new(FILE *fp, CodeConverter *conv)
{
	ERTFParser *parser;

	cm_return_val_if_fail(fp   != NULL, NULL);
	cm_return_val_if_fail(conv != NULL, NULL);

	parser             = g_new0(ERTFParser, 1);
	parser->fp         = fp;
	parser->conv       = conv;
	parser->str        = g_string_new(NULL);
	parser->buf        = g_string_new(NULL);
	parser->bufp       = parser->buf->str;
	parser->newline    = TRUE;
	parser->empty_line = TRUE;
	parser->space      = FALSE;
	parser->pre        = FALSE;

	return parser;
}

void ertf_parser_destroy(ERTFParser *parser)
{
	g_string_free(parser->str, TRUE);
	g_string_free(parser->buf, TRUE);
	g_free(parser);
}

gchar *ertf_parse(ERTFParser *parser)
{
	parser->state = ERTF_NORMAL;
	g_string_truncate(parser->str, 0);

	if (*parser->bufp == '\0') {
		g_string_truncate(parser->buf, 0);
		parser->bufp = parser->buf->str;
		if (ertf_read_line(parser) == ERTF_EOF)
			return NULL;
	}
	
	while (*parser->bufp != '\0') {
		switch (*parser->bufp) {
			case '<':
				if (parser->str->len == 0)
					ertf_parse_tag(parser);
				else
					return parser->str->str;
				break;
			case '\n':
			case '\r':	
				if (parser->bufp[0] == '\r' && parser->bufp[1] == '\n') 
						parser->bufp++;

				if (!parser->pre) {
					/* When not pre (not <nofill>), 1 CRLF = SPACE, N>1 CRLF = N-1 CRLF*/
					if (!parser->newline) {
						parser->newline = TRUE;
						parser->space = TRUE;
					parser->bufp++;
					break;
					}

				}
			default:
				ertf_append_char(parser, *parser->bufp++);
		}
	}
	
	return parser->str->str;
}

static ERTFState ertf_read_line(ERTFParser *parser)
{
	gchar buf[ERTFBUFSIZE];
	gchar buf2[ERTFBUFSIZE];
	gint index;

	if (fgets(buf, sizeof(buf), parser->fp) == NULL) {
		parser->state = ERTF_EOF;
		return ERTF_EOF;
	}

	if (conv_convert(parser->conv, buf2, sizeof(buf2), buf) < 0) {
		g_warning("ertf_read_line(): code conversion failed\n");

		index = parser->bufp - parser->buf->str;

		g_string_append(parser->buf, buf);

		parser->bufp = parser->buf->str + index;

		return ERTF_ERR;
	}

	index = parser->bufp - parser->buf->str;

	g_string_append(parser->buf, buf2);

	parser->bufp = parser->buf->str + index;

	return ERTF_NORMAL;
}

static void ertf_append_char(ERTFParser *parser, gchar ch)
{
	GString *str = parser->str;

	if (!parser->pre && parser->space) {
		if (ch != '\n')
			g_string_append_c(str, ' ');
		parser->space = FALSE;
	}

	g_string_append_c(str, ch);

	parser->empty_line = FALSE;
	if (ch == '\n') {
		if (parser->newline)
			parser->empty_line = TRUE;
		else
			parser->newline = TRUE;
	} else
		parser->newline = FALSE;
}

static ERTFState ertf_parse_tag(ERTFParser *parser)
{
	gchar buf[ERTFBUFSIZE];
	gchar *p;
	gchar *down;
	
	ertf_get_parenthesis (parser, buf, sizeof(buf));
	
	for (p = buf; *p != '\0'; p++) {
		if (isspace (*(guchar *)p)) {
			*p = '\0';
			break;
		}
	}

	parser->state = ERTF_UNKNOWN;
	if (buf[0] == '\0') return parser->state;

	down = g_utf8_strdown (buf, -1);

	if (!strcmp(down, "nofill")) {
		parser->pre   = TRUE;
		parser->state = ERTF_NOFILL;
	}
	else if (!strcmp(down, "/nofill")) {
		parser->pre   = FALSE;
		parser->state = ERTF_NORMAL;
	}
	g_free(down);
	return parser->state;
}
		
static void ertf_get_parenthesis(ERTFParser *parser, gchar *buf, gint len)
{
	gchar *p;

	buf[0] = '\0';
	cm_return_if_fail(*parser->bufp == '<');

	/* ignore params */
	if (!g_ascii_strncasecmp(parser->bufp, "<param>", 4)) {
		parser->bufp += 7;
		while ((p = strstr(parser->bufp, "</param>")) == NULL)
			if (ertf_read_line(parser) == ERTF_EOF) return;
		parser->bufp = p + 8;
		return;
	}
	parser->bufp++;
	while ((p = strchr(parser->bufp, '>')) == NULL)
		if (ertf_read_line(parser) == ERTF_EOF) return;

	strncpy2(buf, parser->bufp, MIN(p - parser->bufp + 1, len));
	parser->bufp = p + 1;
}

