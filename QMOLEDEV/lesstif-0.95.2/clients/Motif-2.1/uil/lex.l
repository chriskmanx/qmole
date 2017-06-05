%{
/**
 *
 * $Id: lex.l,v 1.1 2004/08/28 19:28:18 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 

#include <LTconfig.h>

#include <string.h>
#include <stdio.h>

#include "uil.h"
#include "misc.h"
#include "yacc.h"
#include "glue.h"
#include "Include.h"

#define MAX_INCLUDE_DEPTH 10

#ifdef FLEX_SCANNER
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
#endif

FileData Files[MAX_INCLUDE_DEPTH];
extern YYSTYPE yylval;

char *FileName = Files[0].Name;
int LineNumber = 1;

int include_stack_ptr = 0;

%}

%x comment

inclfile	include[ \t]+file[ \t]+('|\(?\")[^'"]*('|\"\)?)\;
ident		[a-zA-Z][a-zA-Z_0-9]*
string		\"[^\"]*\"
tstring		'[^']*'
float		-?[0-9]+\.[0-9]*
integer		-?[0-9]+
background	background[ \t]+color
foreground	foreground[ \t]+color

%%
{inclfile}	{
		    char *s;

		    if (include_stack_ptr >= MAX_INCLUDE_DEPTH)
		    {
			fprintf(stderr,"Includes nested too deeply\n");
			exit(1);
		    }

		    s = strpbrk(yytext,"'\"")+1;

		    s[strcspn(s,"\"'")] = 0;

#ifdef FLEX_SCANNER
		    include_stack[include_stack_ptr++] = YY_CURRENT_BUFFER;
#endif

		    strcpy(Files[include_stack_ptr].Name,s);

		    Files[include_stack_ptr].lineno = LineNumber;

		    FileName = Files[include_stack_ptr].Name;

		    LineNumber = 1;

		    yyin = IncludeOpenFile(s);

		    if (!yyin)
		    {
			perror(s);
		    }

#ifdef FLEX_SCANNER
		    yy_switch_to_buffer(yy_create_buffer(yyin,YY_BUF_SIZE));
#endif
		}

{string}	{
		    yylval.string = __MrmStore((char *)yytext);

		    return STRING;
		}

{tstring}	{
		    yytext[strlen(yytext)-1] = yytext[0] = '"';
		    yylval.string = __MrmStore(yytext);

		    return STRING;
		}

{float}		{
		    yylval.string = __MrmStore(yytext);

		    return FLOAT;
		}

{integer}	{
		    yylval.string = __MrmStore(yytext);

		    return INTEGER;
		}

character_set	{
		    return CHAR_SET;
		}

right_to_left	{
		    return RIGHT_TO_LEFT;
		}

sixteen_bit	{
		    return SIXTEEN_BIT;
		}

{background}	{
		    return BACKGROUND_COLOR;
		}

{foreground}	{
		    return FOREGROUND_COLOR;
		}

font_unit	{
		    return FONT_UNIT;
		}

font_table	{
		    return FONT_TABLE;
		}

font		{
		    return FONT;
		}

xbitmapfile	{
		    return XBITMAPFILE;
		}

xpixmapfile	{
		    return XPIXMAPFILE;
		}

color_table	{
		    return COLOR_TABLE;
		}

rgb		{
		    return RGB;
		}

color		{
		    return COLOR;
		}

user_defined	{
		    return USER_DEFINED;
		}

boolean		{
		    return BOOLEAN_TYPE;
		}

reason		{
		    return REASON;
		}

private		{
		    return PRIVATE;
		}

gadget		{
		    return GADGET;
		}

icon 		{
		    return ICON;
		}

keysym 		{
		    return KEYSYM;
		}

unmanaged	{
		    return UNMANAGED;
		}

widget		{
		    return WIDGET;
		}

list		{
		    return LIST;
		}

names		{
		    return NAMES;
		}

version		{
		    return VERSION_t;
		}

module		{
		    return MODULE;
		}

value		{
		    return VALUE;
		}

string_table	{
		    return STRING_TABLE;
		}

imported	{
		    return IMPORTED;
		}

exported	{
		    return EXPORTED;
		}

controls	{
		    return CONTROLS;
		}

arguments	{
		    return ARGUMENTS;
		}

argument	{
		    return ARGUMENT;
		}

objects		{
		    return OBJECTS;
		}

object		{
		    return OBJECT;
		}

callbacks	{
		    return CALLBACK;
		}

end 		{
		    return END;
		}

any		{
		    return ANY_TYPE;
		}

string		{
		    return STRING_TYPE;
		}

float		{
		    return FLOAT_TYPE;
		}

integer		{
		    return INTEGER_TYPE;
		}

false		{
		    yylval.string = 0;
		    return BOOL;
		}

true		{
		    yylval.string = (char *)1;
		    return BOOL;
		}

separate	{
		    return SEPARATE;
		}

compound_string	{
		    return COMPOUND_STRING;
		}

procedures	{
		    return PROCEDURES;
		}

procedure	{
		    return PROCEDURE;
		}

{ident}		{
		    yylval.string = (char *)__MrmStore(yytext);
		    return ID;
		}

"/*"		{
#ifdef FLEX_SCANNER
		    BEGIN(comment);
#else
		    BEGIN comment;
#endif
		}
<comment>"*/"	{
#ifdef FLEX_SCANNER
		    BEGIN(INITIAL);
#else
		    BEGIN INITIAL;
#endif
		}
<comment>\n	{
		    LineNumber++;
		}
<comment>"*"	{
			/* Eat */
		}
<comment>.	{
			/* Eat */
		}

"!"[^\n]*	{
		    /* Eat */
		}

[ \t]+	{
		    /* Eat */
		}

\n		{
		    LineNumber++;
		}

.		{
		    return yytext[0];
		}

%%

int yywrap()
{
    if (--include_stack_ptr < 0)
    {
	return 1;
    }
#ifdef FLEX_SCANNER
    else
    {
	LineNumber = Files[include_stack_ptr].lineno;

	yy_switch_to_buffer(include_stack[include_stack_ptr]);
    }
#endif

    FileName = Files[include_stack_ptr].Name;

    return 0;
}
