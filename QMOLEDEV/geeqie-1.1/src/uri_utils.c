/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Authors: John Ellis, Vladimir Nadvornik, Laurent Monin
 *
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include "main.h"
#include "uri_utils.h"

#include "filedata.h"
#include "ui_fileops.h"

/*
 *-----------------------------------------------------------------------------
 * drag and drop uri utils
 *-----------------------------------------------------------------------------
 */

/* the following characters are allowed to be unencoded for pathnames:
 *     $ & + , / : = @
 */
static gint escape_char_list[] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/*   0 */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/*  10 */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/*  20 */
/*	     spc !  "  #  $  %  &  '	       */
	1, 1, 0, 0, 1, 1, 0, 1, 0, 0,	/*  30 */
/*	(  )  *  +  ,  -  .  /  0  1	       */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/*  40 */
/*	2  3  4  5  6  7  8  9  :  ;	       */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 1,	/*  50 */
/*	<  =  >  ?  @  A  B  C  D  E	       */
	1, 0, 1, 1, 0, 0, 0, 0, 0, 0,	/*  60 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/*  70 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/*  80 */
/*	Z  [  \  ]  ^  _  `  a  b  c	       */
	0, 1, 1, 1, 1, 0, 1, 0, 0, 0,	/*  90 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 100 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 110 */
/*	x  y  z  {  |  }  ~ del      	       */
	0, 0, 0, 1, 1, 1, 0, 0		/* 120, 127 is end */
};

static gchar *hex_char = "0123456789ABCDEF";

static gboolean escape_test(guchar c)
{
	if (c < 32 || c > 127) return TRUE;
	return (escape_char_list[c] != 0);
}

static const gchar *escape_code(guchar c)
{
	static gchar text[4];

	text[0] = '%';
	text[1] = hex_char[c>>4];
	text[2] = hex_char[c%16];
	text[3] = '\0';

	return text;
}

gchar *uri_text_escape(const gchar *text)
{
	GString *string;
	gchar *result;
	const gchar *p;

	if (!text) return NULL;

	string = g_string_new("");

	p = text;
	while (*p != '\0')
		{
		if (escape_test(*p))
			{
			g_string_append(string, escape_code(*p));
			}
		else
			{
			g_string_append_c(string, *p);
			}
		p++;
		}

	result = string->str;
	g_string_free(string, FALSE);

	/* dropped filenames are expected to be utf-8 compatible */
	if (!g_utf8_validate(result, -1, NULL))
		{
		gchar *tmp;

		tmp = g_locale_to_utf8(result, -1, NULL, NULL, NULL);
		if (tmp)
			{
			g_free(result);
			result = tmp;
			}
		}

	return result;
}

/* this operates on the passed string, decoding escaped characters */
void uri_text_decode(gchar *text)
{
	if (strchr(text, '%'))
		{
		gchar *w;
		gchar *r;

		w = r = text;

		while (*r != '\0')
			{
			if (*r == '%' && *(r + 1) != '\0' && *(r + 2) != '\0')
				{
				gchar t[3];
				gint n;

				r++;
				t[0] = *r;
				r++;
				t[1] = *r;
				t[2] = '\0';
				n = (gint)strtol(t, NULL, 16);
				if (n > 0 && n < 256)
					{
					*w = (gchar)n;
					}
				else
					{
					/* invalid number, rewind and ignore this escape */
					r -= 2;
					*w = *r;
					}
				}
			else if (w != r)
				{
				*w = *r;
				}
			r++;
			w++;
			}
		if (*w != '\0') *w = '\0';
		}
}

static void uri_list_parse_encoded_chars(GList *list)
{
	GList *work = list;

	while (work)
		{
		gchar *text = work->data;

		uri_text_decode(text);

		work = work->next;
		}
}

GList *uri_list_from_text(gchar *data, gboolean files_only)
{
	GList *list = NULL;
	gint b, e;

	b = e = 0;

	while (data[b] != '\0')
		{
		while (data[e] != '\r' && data[e] != '\n' && data[e] != '\0') e++;
		if (strncmp(data + b, "file:", 5) == 0)
			{
			gchar *path;
			b += 5;
			while (data[b] == '/' && data[b+1] == '/') b++;
			path = g_strndup(data + b, e - b);
			list = g_list_append(list, path_to_utf8(path));
			g_free(path);
			}
		else if (!files_only && strncmp(data + b, "http:", 5) == 0)
			{
			list = g_list_append(list, g_strndup(data + b, e - b));
			}
		else if (!files_only && strncmp(data + b, "ftp:", 3) == 0)
			{
			list = g_list_append(list, g_strndup(data + b, e - b));
			}
		while (data[e] == '\r' || data[e] == '\n') e++;
		b = e;
		}

	uri_list_parse_encoded_chars(list);

	return list;
}

GList *uri_filelist_from_text(gchar *data, gboolean files_only)
{
	GList *path_list = uri_list_from_text(data, files_only);
	GList *filelist = filelist_from_path_list(path_list);
	string_list_free(path_list);
	return filelist;
}

gchar *uri_text_from_list(GList *list, gint *len, gboolean plain_text)
{
	gchar *uri_text = NULL;
	GString *string;
	GList *work;

	if (!list)
		{
		if (len) *len = 0;
		return NULL;
		}

	string = g_string_new("");

	work = list;
	while (work)
		{
		const gchar *name8;	/* dnd filenames are in utf-8 */

		name8 = work->data;

		if (!plain_text)
			{
			gchar *escaped;

			escaped = uri_text_escape(name8);
			g_string_append(string, "file:");
			g_string_append(string, escaped);
			g_free(escaped);

			g_string_append(string, "\r\n");
			}
		else
			{
			g_string_append(string, name8);
			if (work->next) g_string_append(string, "\n");
			}

		work = work->next;
		}

	uri_text = string->str;
	if (len) *len = string->len;
	g_string_free(string, FALSE);

	return uri_text;
}

gchar *uri_text_from_filelist(GList *list, gint *len, gboolean plain_text)
{
	GList *path_list = filelist_to_path_list(list);
	gchar *ret = uri_text_from_list(path_list, len, plain_text);
	string_list_free(path_list);
	return ret;
}
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
