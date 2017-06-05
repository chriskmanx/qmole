%option nounput never-interactive

%{
/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (c) 2001-2007 by Hiroyuki Yamamoto & The Claws Mail Team
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

#include <string.h>
#include <glib.h>

#include "codeconv.h"
#include "matcher.h"
#include "matcher_parser_lex.h"

#ifndef YYSTYPE
#include "matcher_parser_parse.h"
#endif

#define MAX_STR_CONST 8192

static char string_buf[MAX_STR_CONST];
static char *string_buf_ptr;

static void add_char(char ch)
{
	if (string_buf_ptr - string_buf < sizeof(string_buf))
		*string_buf_ptr++ = ch;
}


/* this function will reinitializes the parser */

void matcher_parser_init(void)
{
        BEGIN(0);
}
%}

%option prefix="matcher_parser"
%option outfile="lex.yy.c"
%option yylineno

%x string
%x section

%%
			
"in"	return MATCHER_IN; 

			/*
			 * a keyword consists of alpha and underscore 
			 * characters, possibly preceded by a tilde (~)
			 */

(~|[a-z])[a-z_]*	{
				gint id;

				if (-1 == (id = get_matchparser_tab_id(yytext))) { 
					REJECT;
				} else					
					return id;
			}
[ \t]+
"\n"		return MATCHER_EOL;
"&"		return MATCHER_AND;
"|"		return MATCHER_OR;
\"		{
		BEGIN(string);
		string_buf_ptr = string_buf;
		}
<string>\"	{
		/* get out of the state: string ends. */
		BEGIN(0);
		*string_buf_ptr = '\0';
		if (!g_utf8_validate(string_buf, -1, NULL)) {
			gchar *tmp = conv_codeset_strdup(string_buf, conv_get_locale_charset_str(), CS_INTERNAL);
			if (tmp) {
				strcpy(string_buf, tmp);
				g_free(tmp);
			}
		}
		yylval.str = string_buf;
		return MATCHER_STRING;
		}
<string>\\.	{
                /* take care of quoted characters */
		add_char(yytext[1]);
		}
<string>.	{
		add_char(yytext[0]);
		}
^\[.*\]$	{
                /* for section name in configuration file */
		BEGIN(0);
		yylval.str = yytext + 1;
		yytext[strlen(yytext) - 1] = '\0';
		return MATCHER_SECTION;
		}
[-+]?[0-9]+	{
		yylval.str = yytext;
		return MATCHER_INTEGER;
		}
rulename	{
		return MATCHER_RULENAME;
		}
disabled	{
		return MATCHER_DISABLED;
		}
account	{
		return MATCHER_ACCOUNT;
		}
enabled	{
		return MATCHER_ENABLED;
		}

%%
