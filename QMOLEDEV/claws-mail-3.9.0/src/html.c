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
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "html.h"
#include "codeconv.h"
#include "utils.h"

#define SC_HTMLBUFSIZE	8192
#define HR_STR		"------------------------------------------------"

typedef struct _SC_HTMLSymbol	SC_HTMLSymbol;

struct _SC_HTMLSymbol
{
	gchar *const key;
	gchar *const val;
};

static SC_HTMLSymbol symbol_list[] = {
 {"&#34;", "\42"},
 {"&#38;", "\46"},
 {"&#39;", "\47"},
 {"&#60;", "\74"},
 {"&#62;", "\76"},
 {"&#034;", "\42"},
 {"&#038;", "\46"},
 {"&#039;", "\47"},
 {"&#060;", "\74"},
 {"&#062;", "\76"},
 {"&#146;", "\47"},
 {"&#153;", "\342\204\242"},
 {"&#160;", "\40"},
 {"&#161;", "\302\241"},
 {"&#162;", "\302\242"},
 {"&#163;", "\302\243"},
 {"&#164;", "\302\244"},
 {"&#165;", "\302\245"},
 {"&#166;", "\302\246"},
 {"&#167;", "\302\247"},
 {"&#168;", "\302\250"},
 {"&#169;", "\302\251"},
 {"&#170;", "\302\252"},
 {"&#171;", "\302\253"},
 {"&#172;", "\302\254"},
 {"&#173;", "\302\255"},
 {"&#174;", "\302\256"},
 {"&#175;", "\302\257"},
 {"&#176;", "\302\260"},
 {"&#177;", "\302\261"},
 {"&#178;", "\302\262"},
 {"&#179;", "\302\263"},
 {"&#180;", "\302\264"},
 {"&#181;", "\302\265"},
 {"&#182;", "\302\266"},
 {"&#183;", "\302\267"},
 {"&#184;", "\302\270"},
 {"&#185;", "\302\271"},
 {"&#186;", "\302\272"},
 {"&#187;", "\302\273"},
 {"&#188;", "\302\274"},
 {"&#189;", "\302\275"},
 {"&#190;", "\302\276"},
 {"&#191;", "\302\277"},
 {"&#192;", "\303\200"},
 {"&#193;", "\303\201"},
 {"&#194;", "\303\202"},
 {"&#195;", "\303\203"},
 {"&#196;", "\303\204"},
 {"&#197;", "\303\205"},
 {"&#198;", "\303\206"},
 {"&#199;", "\303\207"},
 {"&#200;", "\303\210"},
 {"&#201;", "\303\211"},
 {"&#202;", "\303\212"},
 {"&#203;", "\303\213"},
 {"&#204;", "\303\214"},
 {"&#205;", "\303\215"},
 {"&#206;", "\303\216"},
 {"&#207;", "\303\217"},
 {"&#208;", "\303\220"},
 {"&#209;", "\303\221"},
 {"&#210;", "\303\222"},
 {"&#211;", "\303\223"},
 {"&#212;", "\303\224"},
 {"&#213;", "\303\225"},
 {"&#214;", "\303\226"},
 {"&#215;", "\303\227"},
 {"&#216;", "\303\230"},
 {"&#217;", "\303\231"},
 {"&#218;", "\303\232"},
 {"&#219;", "\303\233"},
 {"&#220;", "\303\234"},
 {"&#221;", "\303\235"},
 {"&#222;", "\303\236"},
 {"&#223;", "\303\237"},
 {"&#224;", "\303\240"},
 {"&#225;", "\303\241"},
 {"&#226;", "\303\242"},
 {"&#227;", "\303\243"},
 {"&#228;", "\303\244"},
 {"&#229;", "\303\245"},
 {"&#230;", "\303\246"},
 {"&#231;", "\303\247"},
 {"&#232;", "\303\250"},
 {"&#233;", "\303\251"},
 {"&#234;", "\303\252"},
 {"&#235;", "\303\253"},
 {"&#236;", "\303\254"},
 {"&#237;", "\303\255"},
 {"&#238;", "\303\256"},
 {"&#239;", "\303\257"},
 {"&#240;", "\303\260"},
 {"&#241;", "\303\261"},
 {"&#242;", "\303\262"},
 {"&#243;", "\303\263"},
 {"&#244;", "\303\264"},
 {"&#245;", "\303\265"},
 {"&#246;", "\303\266"},
 {"&#247;", "\303\267"},
 {"&#248;", "\303\270"},
 {"&#249;", "\303\271"},
 {"&#250;", "\303\272"},
 {"&#251;", "\303\273"},
 {"&#252;", "\303\274"},
 {"&#253;", "\303\275"},
 {"&#254;", "\303\276"},
 {"&#255;", "\303\277"},
 {"&#338;", "\305\222"},
 {"&#339;", "\305\223"},
 {"&#352;", "\305\240"},
 {"&#353;", "\305\241"},
 {"&#376;", "\305\270"},
 {"&#710;", "\313\206"},
 {"&#732;", "\313\234"},
 {"&#8194;", "\342\200\202"},
 {"&#8195;", "\342\200\203"},
 {"&#8201;", "\342\200\211"},
 {"&#8211;", "\342\200\223"},
 {"&#8212;", "\342\200\224"},
 {"&#8216;", "\342\200\230"},
 {"&#8217;", "\342\200\231"},
 {"&#8218;", "\342\200\232"},
 {"&#8220;", "\342\200\234"},
 {"&#8221;", "\342\200\235"},
 {"&#8222;", "\342\200\236"},
 {"&#8224;", "\342\200\240"},
 {"&#8225;", "\342\200\241"},
 {"&#8226;", "\342\200\242"},
 {"&#8230;", "\342\200\246"},
 {"&#8240;", "\342\200\260"},
 {"&#8249;", "\342\200\271"},
 {"&#8250;", "\342\200\272"},
 {"&#8364;", "\342\202\254"},
 {"&#8482;", "\342\204\242"},
 {"&quot;", "\42"},
 {"&amp;", "\46"},
 {"&apos;", "\47"},
 {"&lt;", "\74"},
 {"&gt;", "\76"},
 {"&squot;", "\47"},
 {"&nbsp;", "\40"},
 {"&iexcl;", "\302\241"},
 {"&cent;", "\302\242"},
 {"&pound;", "\302\243"},
 {"&curren;", "\302\244"},
 {"&yen;", "\302\245"},
 {"&brvbar;", "\302\246"},
 {"&sect;", "\302\247"},
 {"&uml;", "\302\250"},
 {"&copy;", "\302\251"},
 {"&ordf;", "\302\252"},
 {"&laquo;", "\302\253"},
 {"&not;", "\302\254"},
 {"&shy;", "\302\255"},
 {"&reg;", "\302\256"},
 {"&macr;", "\302\257"},
 {"&deg;", "\302\260"},
 {"&plusmn;", "\302\261"},
 {"&sup2;", "\302\262"},
 {"&sup3;", "\302\263"},
 {"&acute;", "\302\264"},
 {"&micro;", "\302\265"},
 {"&para;", "\302\266"},
 {"&middot;", "\302\267"},
 {"&cedil;", "\302\270"},
 {"&sup1;", "\302\271"},
 {"&ordm;", "\302\272"},
 {"&raquo;", "\302\273"},
 {"&frac14;", "\302\274"},
 {"&frac12;", "\302\275"},
 {"&frac34;", "\302\276"},
 {"&iquest;", "\302\277"},
 {"&Agrave;", "\303\200"},
 {"&Aacute;", "\303\201"},
 {"&Acirc;", "\303\202"},
 {"&Atilde;", "\303\203"},
 {"&Auml;", "\303\204"},
 {"&Aring;", "\303\205"},
 {"&AElig;", "\303\206"},
 {"&Ccedil;", "\303\207"},
 {"&Egrave;", "\303\210"},
 {"&Eacute;", "\303\211"},
 {"&Ecirc;", "\303\212"},
 {"&Euml;", "\303\213"},
 {"&Igrave;", "\303\214"},
 {"&Iacute;", "\303\215"},
 {"&Icirc;", "\303\216"},
 {"&Iuml;", "\303\217"},
 {"&ETH;", "\303\220"},
 {"&Ntilde;", "\303\221"},
 {"&Ograve;", "\303\222"},
 {"&Oacute;", "\303\223"},
 {"&Ocirc;", "\303\224"},
 {"&Otilde;", "\303\225"},
 {"&Ouml;", "\303\226"},
 {"&times;", "\303\227"},
 {"&Oslash;", "\303\230"},
 {"&Ugrave;", "\303\231"},
 {"&Uacute;", "\303\232"},
 {"&Ucirc;", "\303\233"},
 {"&Uuml;", "\303\234"},
 {"&Yacute;", "\303\235"},
 {"&THORN;", "\303\236"},
 {"&szlig;", "\303\237"},
 {"&agrave;", "\303\240"},
 {"&aacute;", "\303\241"},
 {"&acirc;", "\303\242"},
 {"&atilde;", "\303\243"},
 {"&auml;", "\303\244"},
 {"&aring;", "\303\245"},
 {"&aelig;", "\303\246"},
 {"&ccedil;", "\303\247"},
 {"&egrave;", "\303\250"},
 {"&eacute;", "\303\251"},
 {"&ecirc;", "\303\252"},
 {"&euml;", "\303\253"},
 {"&igrave;", "\303\254"},
 {"&iacute;", "\303\255"},
 {"&icirc;", "\303\256"},
 {"&iuml;", "\303\257"},
 {"&eth;", "\303\260"},
 {"&ntilde;", "\303\261"},
 {"&ograve;", "\303\262"},
 {"&oacute;", "\303\263"},
 {"&ocirc;", "\303\264"},
 {"&otilde;", "\303\265"},
 {"&ouml;", "\303\266"},
 {"&divide;", "\303\267"},
 {"&oslash;", "\303\270"},
 {"&ugrave;", "\303\271"},
 {"&uacute;", "\303\272"},
 {"&ucirc;", "\303\273"},
 {"&uuml;", "\303\274"},
 {"&yacute;", "\303\275"},
 {"&thorn;", "\303\276"},
 {"&yuml;", "\303\277"},
 {"&OElig;", "\305\222"},
 {"&oelig;", "\305\223"},
 {"&Scaron;", "\305\240"},
 {"&scaron;", "\305\241"},
 {"&Yuml;", "\305\270"},
 {"&circ;", "\313\206"},
 {"&tilde;", "\313\234"},
 {"&ensp;", "\342\200\202"},
 {"&emsp;", "\342\200\203"},
 {"&thinsp;", "\342\200\211"},
 {"&ndash;", "\342\200\223"},
 {"&mdash;", "\342\200\224"},
 {"&lsquo;", "\342\200\230"},
 {"&rsquo;", "\342\200\231"},
 {"&sbquo;", "\342\200\232"},
 {"&ldquo;", "\342\200\234"},
 {"&rdquo;", "\342\200\235"},
 {"&bdquo;", "\342\200\236"},
 {"&dagger;", "\342\200\240"},
 {"&Dagger;", "\342\200\241"},
 {"&bull;", "\342\200\242"},
 {"&hellip;", "\342\200\246"},
 {"&permil;", "\342\200\260"},
 {"&lsaquo;", "\342\200\271"},
 {"&rsaquo;", "\342\200\272"},
 {"&euro;", "\342\202\254"},
 {"&trade;", "\342\204\242"}
};

typedef struct _SC_HTMLAltSymbol	SC_HTMLAltSymbol;

struct _SC_HTMLAltSymbol
{
	gint key;
	gchar *const val;
};

static GHashTable *default_symbol_table;

static SC_HTMLState sc_html_read_line	(SC_HTMLParser	*parser);
static void sc_html_append_char			(SC_HTMLParser	*parser,
					 gchar		 ch);
static void sc_html_append_str			(SC_HTMLParser	*parser,
					 const gchar	*str,
					 gint		 len);
static SC_HTMLState sc_html_parse_tag	(SC_HTMLParser	*parser);
static void sc_html_parse_special		(SC_HTMLParser	*parser);
static void sc_html_get_parenthesis		(SC_HTMLParser	*parser,
					 gchar		*buf,
					 gint		 len);


SC_HTMLParser *sc_html_parser_new(FILE *fp, CodeConverter *conv)
{
	SC_HTMLParser *parser;

	cm_return_val_if_fail(fp != NULL, NULL);
	cm_return_val_if_fail(conv != NULL, NULL);

	parser = g_new0(SC_HTMLParser, 1);
	parser->fp = fp;
	parser->conv = conv;
	parser->str = g_string_new(NULL);
	parser->buf = g_string_new(NULL);
	parser->bufp = parser->buf->str;
	parser->state = SC_HTML_NORMAL;
	parser->href = NULL;
	parser->newline = TRUE;
	parser->empty_line = TRUE;
	parser->space = FALSE;
	parser->pre = FALSE;

#define SYMBOL_TABLE_ADD(table, list) \
{ \
	gint i; \
 \
	for (i = 0; i < sizeof(list) / sizeof(list[0]); i++) \
		g_hash_table_insert(table, list[i].key, list[i].val); \
}
#define SYMBOL_TABLE_REF_ADD(table, list) \
{ \
	gint i; \
 \
	for (i = 0; i < sizeof(list) / sizeof(list[0]); i++) \
		g_hash_table_insert(table, &list[i].key, list[i].val); \
}

	if (!default_symbol_table) {
		default_symbol_table =
			g_hash_table_new(g_str_hash, g_str_equal);
		SYMBOL_TABLE_ADD(default_symbol_table, symbol_list);
	}

#undef SYMBOL_TABLE_ADD
#undef SYMBOL_TABLE_REF_ADD

	parser->symbol_table = default_symbol_table;

	return parser;
}

void sc_html_parser_destroy(SC_HTMLParser *parser)
{
	g_string_free(parser->str, TRUE);
	g_string_free(parser->buf, TRUE);
	g_free(parser->href);
	g_free(parser);
}

gchar *sc_html_parse(SC_HTMLParser *parser)
{
	parser->state = SC_HTML_NORMAL;
	g_string_truncate(parser->str, 0);

	if (*parser->bufp == '\0') {
		g_string_truncate(parser->buf, 0);
		parser->bufp = parser->buf->str;
		if (sc_html_read_line(parser) == SC_HTML_EOF)
			return NULL;
	}

	while (*parser->bufp != '\0') {
		switch (*parser->bufp) {
		case '<': {
			SC_HTMLState st;
			st = sc_html_parse_tag(parser);
			/* when we see an href, we need to flush the str
			 * buffer.  Then collect all the chars until we
			 * see the end anchor tag
			 */
			if (SC_HTML_HREF_BEG == st || SC_HTML_HREF == st)
				return parser->str->str;
			} 
			break;
		case '&':
			sc_html_parse_special(parser);
			break;
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			if (parser->bufp[0] == '\r' && parser->bufp[1] == '\n')
				parser->bufp++;

			if (!parser->pre) {
				if (!parser->newline)
					parser->space = TRUE;

				parser->bufp++;
				break;
			}
			/* fallthrough */
		default:
			sc_html_append_char(parser, *parser->bufp++);
		}
	}

	return parser->str->str;
}

static SC_HTMLState sc_html_read_line(SC_HTMLParser *parser)
{
	gchar buf[SC_HTMLBUFSIZE];
	gchar buf2[SC_HTMLBUFSIZE];
	gint index;

	if (parser->fp == NULL)
		return SC_HTML_EOF;

	if (fgets(buf, sizeof(buf), parser->fp) == NULL) {
		parser->state = SC_HTML_EOF;
		return SC_HTML_EOF;
	}

	if (conv_convert(parser->conv, buf2, sizeof(buf2), buf) < 0) {
		index = parser->bufp - parser->buf->str;

		conv_utf8todisp(buf2, sizeof(buf2), buf);
		g_string_append(parser->buf, buf2);

		parser->bufp = parser->buf->str + index;

		return SC_HTML_CONV_FAILED;
	}

	index = parser->bufp - parser->buf->str;

	g_string_append(parser->buf, buf2);

	parser->bufp = parser->buf->str + index;

	return SC_HTML_NORMAL;
}

static void sc_html_append_char(SC_HTMLParser *parser, gchar ch)
{
	GString *str = parser->str;

	if (!parser->pre && parser->space) {
		g_string_append_c(str, ' ');
		parser->space = FALSE;
	}

	g_string_append_c(str, ch);

	parser->empty_line = FALSE;
	if (ch == '\n') {
		parser->newline = TRUE;
		if (str->len > 1 && str->str[str->len - 2] == '\n')
			parser->empty_line = TRUE;
	} else
		parser->newline = FALSE;
}

static void sc_html_append_str(SC_HTMLParser *parser, const gchar *str, gint len)
{
	GString *string = parser->str;

	if (!parser->pre && parser->space) {
		g_string_append_c(string, ' ');
		parser->space = FALSE;
	}

	if (len == 0) return;
	if (len < 0)
		g_string_append(string, str);
	else {
		gchar *s;
		Xstrndup_a(s, str, len, return);
		g_string_append(string, s);
	}

	parser->empty_line = FALSE;
	if (string->len > 0 && string->str[string->len - 1] == '\n') {
		parser->newline = TRUE;
		if (string->len > 1 && string->str[string->len - 2] == '\n')
			parser->empty_line = TRUE;
	} else
		parser->newline = FALSE;
}

static SC_HTMLTag *sc_html_get_tag(const gchar *str)
{
	SC_HTMLTag *tag;
	gchar *tmp;
	guchar *tmpp;

	cm_return_val_if_fail(str != NULL, NULL);

	if (*str == '\0' || *str == '!') return NULL;

	Xstrdup_a(tmp, str, return NULL);

	tag = g_new0(SC_HTMLTag, 1);

	for (tmpp = tmp; *tmpp != '\0' && !g_ascii_isspace(*tmpp); tmpp++)
		;

	if (*tmpp == '\0') {
		tag->name = g_utf8_strdown(tmp, -1);
		return tag;
	} else {
		*tmpp++ = '\0';
		tag->name = g_utf8_strdown(tmp, -1);
	}

	while (*tmpp != '\0') {
		SC_HTMLAttr *attr;
		gchar *attr_name;
		gchar *attr_value;
		gchar *p;
		gchar quote;

		while (g_ascii_isspace(*tmpp)) tmpp++;
		attr_name = tmpp;

		while (*tmpp != '\0' && !g_ascii_isspace(*tmpp) &&
		       *tmpp != '=')
			tmpp++;
		if (*tmpp != '\0' && *tmpp != '=') {
			*tmpp++ = '\0';
			while (g_ascii_isspace(*tmpp)) tmpp++;
		}

		if (*tmpp == '=') {
			*tmpp++ = '\0';
			while (g_ascii_isspace(*tmpp)) tmpp++;

			if (*tmpp == '"' || *tmpp == '\'') {
				/* name="value" */
				quote = *tmpp;
				tmpp++;
				attr_value = tmpp;
				if ((p = strchr(attr_value, quote)) == NULL) {
					g_warning("sc_html_get_tag(): syntax error in tag: '%s'\n", str);
					return tag;
				}
				tmpp = p;
				*tmpp++ = '\0';
				while (g_ascii_isspace(*tmpp)) tmpp++;
			} else {
				/* name=value */
				attr_value = tmpp;
				while (*tmpp != '\0' && !g_ascii_isspace(*tmpp)) tmpp++;
				if (*tmpp != '\0')
					*tmpp++ = '\0';
			}
		} else
			attr_value = "";

		g_strchomp(attr_name);
		attr = g_new(SC_HTMLAttr, 1);
		attr->name = g_utf8_strdown(attr_name, -1);
		attr->value = g_strdup(attr_value);
		tag->attr = g_list_append(tag->attr, attr);
	}

	return tag;
}

static void sc_html_free_tag(SC_HTMLTag *tag)
{
	if (!tag) return;

	g_free(tag->name);
	while (tag->attr != NULL) {
		SC_HTMLAttr *attr = (SC_HTMLAttr *)tag->attr->data;
		g_free(attr->name);
		g_free(attr->value);
		g_free(attr);
		tag->attr = g_list_remove(tag->attr, tag->attr->data);
	}
	g_free(tag);
}

static void decode_href(SC_HTMLParser *parser)
{
	gchar *tmp;
	SC_HTMLParser *tparser = g_new0(SC_HTMLParser, 1);

	tparser->str = g_string_new(NULL);
	tparser->buf = g_string_new(parser->href);
	tparser->bufp = tparser->buf->str;
	tparser->symbol_table = default_symbol_table;
	
	tmp = sc_html_parse(tparser);
	
	g_free(parser->href);
	parser->href = g_strdup(tmp);

	sc_html_parser_destroy(tparser);
}

static SC_HTMLState sc_html_parse_tag(SC_HTMLParser *parser)
{
	gchar buf[SC_HTMLBUFSIZE];
	SC_HTMLTag *tag;

	sc_html_get_parenthesis(parser, buf, sizeof(buf));

	tag = sc_html_get_tag(buf);

	parser->state = SC_HTML_UNKNOWN;
	if (!tag) return SC_HTML_UNKNOWN;

	if (!strcmp(tag->name, "br")) {
		parser->space = FALSE;
		sc_html_append_char(parser, '\n');
		parser->state = SC_HTML_BR;
	} else if (!strcmp(tag->name, "a")) {
		GList *cur;
		for (cur = tag->attr; cur != NULL; cur = cur->next) {
			if (cur->data && !strcmp(((SC_HTMLAttr *)cur->data)->name, "href")) {
				g_free(parser->href);
				parser->href = g_strdup(((SC_HTMLAttr *)cur->data)->value);
				decode_href(parser);
				parser->state = SC_HTML_HREF_BEG;
				break;
			}
		}
	} else if (!strcmp(tag->name, "/a")) {
		parser->state = SC_HTML_HREF;
	} else if (!strcmp(tag->name, "p")) {
		parser->space = FALSE;
		if (!parser->empty_line) {
			parser->space = FALSE;
			if (!parser->newline) sc_html_append_char(parser, '\n');
			sc_html_append_char(parser, '\n');
		}
		parser->state = SC_HTML_PAR;
	} else if (!strcmp(tag->name, "pre")) {
		parser->pre = TRUE;
		parser->state = SC_HTML_PRE;
	} else if (!strcmp(tag->name, "/pre")) {
		parser->pre = FALSE;
		parser->state = SC_HTML_NORMAL;
	} else if (!strcmp(tag->name, "hr")) {
		if (!parser->newline) {
			parser->space = FALSE;
			sc_html_append_char(parser, '\n');
		}
		sc_html_append_str(parser, HR_STR "\n", -1);
		parser->state = SC_HTML_HR;
	} else if (!strcmp(tag->name, "div")    ||
		   !strcmp(tag->name, "ul")     ||
		   !strcmp(tag->name, "li")     ||
		   !strcmp(tag->name, "table")  ||
		   !strcmp(tag->name, "tr")     ||
		   (tag->name[0] == 'h' && g_ascii_isdigit(tag->name[1]))) {
		if (!parser->newline) {
			parser->space = FALSE;
			sc_html_append_char(parser, '\n');
		}
		parser->state = SC_HTML_NORMAL;
	} else if (!strcmp(tag->name, "/table") ||
		   (tag->name[0] == '/' &&
		    tag->name[1] == 'h' &&
		    g_ascii_isdigit(tag->name[1]))) {
		if (!parser->empty_line) {
			parser->space = FALSE;
			if (!parser->newline) sc_html_append_char(parser, '\n');
			sc_html_append_char(parser, '\n');
		}
		parser->state = SC_HTML_NORMAL;
	} else if (!strcmp(tag->name, "/div")   ||
		   !strcmp(tag->name, "/ul")    ||
		   !strcmp(tag->name, "/li")) {
		if (!parser->newline) {
			parser->space = FALSE;
			sc_html_append_char(parser, '\n');
		}
		parser->state = SC_HTML_NORMAL;
			}

	sc_html_free_tag(tag);

	return parser->state;
}

static void sc_html_parse_special(SC_HTMLParser *parser)
{
	gchar symbol_name[9];
	gint n;
	const gchar *val;

	parser->state = SC_HTML_UNKNOWN;
	cm_return_if_fail(*parser->bufp == '&');

	/* &foo; */
	for (n = 0; parser->bufp[n] != '\0' && parser->bufp[n] != ';'; n++)
		;
	if (n > 7 || parser->bufp[n] != ';') {
		/* output literal `&' */
		sc_html_append_char(parser, *parser->bufp++);
		parser->state = SC_HTML_NORMAL;
		return;
	}
	strncpy2(symbol_name, parser->bufp, n + 2);
	parser->bufp += n + 1;

	if ((val = g_hash_table_lookup(parser->symbol_table, symbol_name))
	    != NULL) {
		sc_html_append_str(parser, val, -1);
		parser->state = SC_HTML_NORMAL;
		return;
	} 

	sc_html_append_str(parser, symbol_name, -1);
}

static void sc_html_get_parenthesis(SC_HTMLParser *parser, gchar *buf, gint len)
{
	gchar *p;

	buf[0] = '\0';
	cm_return_if_fail(*parser->bufp == '<');

	/* ignore comment / CSS / script stuff */
	if (!strncmp(parser->bufp, "<!--", 4)) {
		parser->bufp += 4;
		while ((p = strstr(parser->bufp, "-->")) == NULL)
			if (sc_html_read_line(parser) == SC_HTML_EOF) return;
		parser->bufp = p + 3;
		return;
	}
	if (!g_ascii_strncasecmp(parser->bufp, "<style", 6)) {
		parser->bufp += 6;
		while ((p = strcasestr(parser->bufp, "</style>")) == NULL)
			if (sc_html_read_line(parser) == SC_HTML_EOF) return;
		parser->bufp = p + 8;
		return;
	}
	if (!g_ascii_strncasecmp(parser->bufp, "<script", 7)) {
		parser->bufp += 7;
		while ((p = strcasestr(parser->bufp, "</script>")) == NULL)
			if (sc_html_read_line(parser) == SC_HTML_EOF) return;
		parser->bufp = p + 9;
		return;
	}

	parser->bufp++;
	while ((p = strchr(parser->bufp, '>')) == NULL)
		if (sc_html_read_line(parser) == SC_HTML_EOF) return;

	strncpy2(buf, parser->bufp, MIN(p - parser->bufp + 1, len));
	g_strstrip(buf);
	parser->bufp = p + 1;
}
