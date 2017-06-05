%{
/**
 *
 * $Id: yacc.y,v 1.1 2004/08/28 19:28:18 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "uil.h"	
#include "glue.h"

extern int LineNumber;
extern char *FileName;

#ifdef	YYTEXT_POINTER
extern char *yytext;
#else
extern unsigned char yytext[];	/* Unsigned needed on HP/UX */
#endif
extern int yylex(void);

/* For the prototype police.
   GNU bison up to 1.28 doesn't allow us to use the
   const qualifier here. Shame on it ...
 */ 
void yyerror(char *s);
void yyExit(int line, char *file, char *fmt, ...);


static int False = 0;
static int True = 1;


#define YYDEBUG	1
int yydebug = 1;
%}

%token	STRING
	ID
	VERSION_t
	NAMES
	MODULE
	VALUE
	INTEGER
	FLOAT
	STRING_TABLE
	INTEGER_TYPE
	FLOAT_TYPE
	STRING_TYPE
	ANY_TYPE
	BOOLEAN_TYPE
	PROCEDURE
	PROCEDURES
	IMPORTED
	CONTROLS
	ARGUMENT
	ARGUMENTS
	OBJECT
	CALLBACK
	END
	EXPORTED
	OBJECTS
	CHAR_SET
	WIDGET
	INC_FILE
	LIST
	UNMANAGED
	KEYSYM
	ICON
	COMPOUND_STRING
	SEPARATE
	BOOL
	GADGET
	PRIVATE
	REASON
	USER_DEFINED
	RGB
	COLOR
	COLOR_TABLE
	XBITMAPFILE
	XPIXMAPFILE
	FONT
	FONT_TABLE
	FONT_UNIT
	BACKGROUND_COLOR
	FOREGROUND_COLOR
	RIGHT_TO_LEFT
	SIXTEEN_BIT

%type <string>
	STRING
	ID
	INTEGER
	FLOAT
	VERSION_t
	NAMES
	MODULE
	VALUE
	STRING_TABLE
	INTEGER_TYPE
	FLOAT_TYPE
	STRING_TYPE
	ANY_TYPE
	BOOLEAN_TYPE
	PROCEDURE
	PROCEDURES
	IMPORTED
	CONTROLS
	ARGUMENT
	ARGUMENTS
	OBJECT
	CALLBACK
	END
	EXPORTED
	OBJECTS
	CHAR_SET
	WIDGET
	INC_FILE
	LIST
	UNMANAGED
	KEYSYM
	ICON
	COMPOUND_STRING
	SEPARATE
	BOOL
	GADGET
	PRIVATE
	REASON
	USER_DEFINED
	RGB
	COLOR
	COLOR_TABLE
	XBITMAPFILE
	XPIXMAPFILE
	FONT
	FONT_TABLE
	FONT_UNIT
	BACKGROUND_COLOR
	FOREGROUND_COLOR
	RIGHT_TO_LEFT
	SIXTEEN_BIT

%type <string>
	body
	object
	module
	add_expr
	mult_expr
	prim_expr
	font_list
	string_list
	color_list
	procedure_list
	control_list
	callback_list
	argument_list
	proc_argument_list
	initializers
	character_set
	extra_cs_parms
	list
	list_arg
	features
	controls
	arguments
	callbacks
	compound_string

%union {
    char	*string;
};

%%

input:		MODULE ID module body END MODULE ';'
		{
		}
	|
		{
		    __MrmWarn(LOC,"Empty input file\n");
		}
;

body:		body VALUE initializers
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = $1;
		}
	|	body PROCEDURE procedure_list
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = $1;
		}
	|	body LIST list
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	body OBJECT object
		{
		    $$ = body_OBJECT_object($1,$3);
		}
	|
		{
		    $$ = NULL;
		}

;

module:		VERSION_t '=' STRING module
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	NAMES '=' ID module
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = $1;
		}
	|	CHAR_SET '=' ID module
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	OBJECTS '=' '{' object_list '}'
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = NULL;
		}
;

object_list:	ID '=' WIDGET ';' object_list
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	ID '=' GADGET ';' object_list
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
;

initializers:	ID ':' add_expr ';' initializers 
		{
		    MakeTable($1,$3, 0);
		}
	|	ID ':'  FONT_TABLE '(' font_list ')' ';'
		{
		    MakeTable($1, $5, 0);
		}
	|	ID ':'  STRING_TABLE '(' string_list ')' ';' initializers 
		{
		    MakeTable($1,$5,0);
		}
	|	ID ':'  COLOR_TABLE '(' color_list ')' ';' initializers 
		{
		    MakeTable($1, $5, 0);
		}
	|	ID ':' PRIVATE ARGUMENT '(' STRING ',' type ')' ';' initializers
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	ID ':'  REASON '(' STRING ')' ';' initializers
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	ID ':' EXPORTED add_expr ';' initializers
		{
		    MakeTable($1, $4, 1);
		}
	|	ID ':' IMPORTED type ';' initializers
		/* {
		    __MrmWarn(LOC,"NO OP\n");
		} */
	|
		{
			$$ = NULL;
	       	}
;

font_list:	character_set '=' ID ',' font_list
		{
		    $$ = AddFont($1, $3, $5);
		}
	|	character_set '=' ID
		{
		    $$ = AddFont($1, $3, NULL);
		}		
;

character_set:	ID
		{
		    $$ = CharSetName(NULL, $1);
		}
	|	CHAR_SET '(' STRING extra_cs_parms ')'
		{
		    $$ = CharSetName($4, $3);
		}
;

extra_cs_parms:	',' RIGHT_TO_LEFT '=' BOOL extra_cs_parms
		{
		    $$ = CharSetRToL($5, (int)(long)$4);
		}
	|	',' SIXTEEN_BIT '=' BOOL extra_cs_parms
		{
		    $$ = CharSet16Bit($5, (int)(long)$4);
		}
	|
		{
		    $$ = MakeNewCharSet();
		}
;

color_list:	ID '=' STRING ',' color_list
		{
		    $$ = AddColor($1,$3, $5, 1);
		}
	|	ID '=' STRING 
		{
		    $$ = AddColor($1,$3, NULL, 1);
		}			
	|	BACKGROUND_COLOR '=' STRING 
		{
		    $$ = AddColor(__MrmStore("\"_bg\""),$3, NULL, 0);
		}
	|	BACKGROUND_COLOR '=' STRING ',' color_list
		{
		    $$ = AddColor(__MrmStore("\"_bg\""), $3, $5, 0);
		}
	|	FOREGROUND_COLOR '=' STRING 
		{
		    $$ = AddColor(__MrmStore("\"_fg\""), $3, NULL, 0);
		}
	|	FOREGROUND_COLOR '=' STRING ',' color_list
		{
		    $$ = AddColor(__MrmStore("\"_fg\""), $3, $5, 0);
		}
	|	COLOR '(' STRING ')' '=' STRING ',' color_list
		{
		    $$ = AddColor($3, $6, $8, 0);
		}
	|	COLOR '(' STRING ')' '=' STRING
		{
		    $$ = AddColor($3, $6, NULL, 0);
		}
;

string_list:	string_list ',' STRING
		{
		    $$ = InsertString($1, $3);
		}
	|	string_list ',' ID
		{
		    $$ = InsertString($1, $3);
		}
	|	STRING
		{
		    $$ = InsertString(NULL,$1);
		}
	|	ID
		{
		    $$ = InsertString(NULL, $1);
		}
;

procedure_list:	procedure_list ID '(' type_list ')' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	procedure_list ID ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = $1;
		}
	|	procedure_list ID '(' ')' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = $1;
		}
	|	ID '(' type_list ')' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	ID ';'
		{
		    __MrmWarn(LOC, "NO OP\n");
		    $$ = $1;
		}
	|	ID '(' ')' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = $1;
		}
;

type_list:	type_list ',' type
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	type
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
;

type:		INTEGER_TYPE 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	FLOAT_TYPE 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	ANY_TYPE 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	STRING_TYPE 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	BOOLEAN_TYPE 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	COMPOUND_STRING 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	COLOR 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	FONT_TABLE
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
;

list:		list ID ':' ARGUMENTS '{' list_arg '}' ';' 
		{
		    AddAttributeList($2, $6);
		    $$ = NULL;
		}
	|	list ID ':' CONTROLS '{' control_list '}' ';' 
		{
		    AddControlList($2, $6);
		    $$ = NULL;
		}
	|	list ID ':' CALLBACK '{' callback_list '}' ';' 
		{
		    AddCallbackList($2, $6);
		    $$ = NULL;
		}
	|
		{
		    $$ = NULL;
		}
;

list_arg:	list_arg ID '=' add_expr ';' 
		{
		    $$ = (char *)arglist_arglist_ID_addexpr($1, $2, $4);
		}
	|
		{
		    $$ = NULL;
		}
;

object:		object ID ':' IMPORTED ID ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	object ID ':' ID '{' features '}' ';'
		{
		    ID_ID_features($2,$4,$6);
		    $$ = $6;
		}
	|	object ID ':' ID WIDGET '{' features '}' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	object ID ':' EXPORTED ID '{' features '}' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		    ID_ID_features($2, $5, $7);
		    $$ = $7;
		}
	|	object ID ':' USER_DEFINED PROCEDURE ID '{' features '}' ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|
		{
		    /* __MrmWarn(LOC,"Empty object\n"); */
		}
;

features:	features controls
		{
		    features_controls($1,$2);
		    $$ = $1;
		}
	|	features arguments
		{
		    features_arguments($1,$2);
		    $$ = $1;
		}
	|	features callbacks 
		{
		    features_callbacks($1,$2);
		    $$ = $1;
		}
	|
		{
		    $$ = (char *)Features_NULL();
		}
;

controls:	CONTROLS '{' control_list '}' ';'
		{
		    $$ = $3;
		}
	|	CONTROLS ID ';'
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = NULL;
		}
;

control_list:	control_list ID ID ';'  
		{
		    $$ = (char *)controllist_controllist_ID_ID($1,$2,$3,1);
		}
	|	control_list ID '{' features '}' ';'
		{
		    $$ = (char *)control_list_ID_features($1, $2, $4, 1);
		}
	|	control_list ID UNMANAGED '{' features '}' ';'
		{   /* $2 may be wrong for this type of call */
		    /* I confused this rule for the next one */
		    $$ = (char *)control_list_ID_features($1, $2, $5, 0);
		}					
	|	control_list UNMANAGED ID '{' features '}' ';' 
		{
		    $$ = (char *)control_list_ID_features($1,$3, $5, 0);
		}
	|	control_list ID ':' ID '{' features '}' ';' 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|	control_list UNMANAGED ID ID ';' 
		{
		    $$ = (char *)controllist_controllist_ID_ID($1,$3,$4,0);
		}
	|	control_list CONTROLS ID ';' 
		{
		    $$ = InheritControls($1, $3);
		}
	|	control_list USER_DEFINED ID ';' 
		{
		    __MrmWarn(LOC,"NO OP\n");
		}
	|
		{
			$$ = NULL;
		}
;

arguments:	ARGUMENTS '{' argument_list '}' ';'
		{
		    $$ = $3;
		}
;

argument_list:	argument_list ID '=' add_expr ';' 
		{
		    $$ = arglist_arglist_ID_addexpr($1,$2,$4);
		}
	|	argument_list ID '=' ID ID ';'  /* another widget */
		{
		    $$ = WidgetArgument($1, $2, $5);
	     	}
	|	argument_list ARGUMENTS ID ';'
		{
		    $$ = InheritArgument($1, $3);
		}
	|
		{
		    $$ = NULL;
		}
;

add_expr:	mult_expr
		{
		    $$ = $1;
		}
	|	add_expr '-' mult_expr
		{
		    $$ = Subtract($1,$3);
		}
	|	add_expr '+' mult_expr
		{
		    $$ = Add($1,$3);
		}
	|	add_expr '&' prim_expr
		{
		    $$ = AppendStrings($1, $3);
		}
;

mult_expr:	prim_expr
		{
		    $$ = $1;
		}
	|	mult_expr '*' prim_expr
		{
		    $$ = Multiply($1,$3);
		}
	|	mult_expr '/' prim_expr
		{
		    $$ = Divide($1,$3);
		}
;

prim_expr:	ID
		{
		    $$ = (char *) expr_ID($1);
		}
	|	INTEGER FONT_UNIT
		{
		    __MrmWarn(LOC, "Font Unit not implemented yet\n");
		    $$ = (char *) prim_exp($1);
		}
	|	INTEGER
		{
		    $$ = (char *)prim_exp($1);
		}
	|	'#' character_set STRING
		{
		    fprintf(stderr,"BLAB BLAB ID = %s\n", $3);
		    $$ = (char *) expr_STRING($3, $2,/*Compound?*/ False);
		}
	|	STRING
		{
		    $$ = (char *) expr_STRING($1, NULL, /*Compound?*/ False);
		}
	|	BOOL
		{
		    $$ = (char *) expr_BOOL($1);
		}
	|	KEYSYM '(' ID ')'
		{
		    $$ = (char *) keysym($3);
		}
	|	FONT '(' STRING ')'
		{
		    $$ = (char *) font($3);
		}
	|	KEYSYM '(' STRING ')'
		{
		    $$ = (char *) keysym($3);
		}
	|	XPIXMAPFILE '(' STRING ')'
		{
		    __MrmWarn(LOC,"NO OP\n");
		    $$ = NULL;
		}
	|	XBITMAPFILE '(' STRING ')'
		{	
		    $$ = (char *) bitmap($3);
		}
	|	ICON '(' COLOR_TABLE '=' ID ',' string_list ')'
		{
		    $$ = (char *) pixmap($5, $7);
		}
	|	ICON '(' string_list ')'
		{
		    $$ = pixmap(NULL,$3);
		}
	|	RGB '(' INTEGER ',' INTEGER ',' INTEGER ')'
		{
		    $$ = color(NULL, $3, $5, $7);
		}
	|	COLOR '(' STRING ')'
		{
		    $$ = color($3, 0, 0, 0);
		}
	|	COLOR '(' STRING ',' ID ')'
		{
		    $$ = color($3, 0, 0, 0); /* For Now  FIX ME */
		}
	|	compound_string
		{
		    $$ = $1;
		}
	|	'(' add_expr ')'
		{	
		    $$ = $2;
		}
;

compound_string: COMPOUND_STRING '(' STRING ',' SEPARATE '=' BOOL ')'
		{
		    /* string, separate, IsAddress */
		    $$ = (char *) expr_STRING_Compound($3,(int)(long)$7,False);
		}
	|	COMPOUND_STRING '(' ID ',' SEPARATE '=' BOOL ')'
		{
		    $$ = (char *) expr_STRING_Compound($3,(int)(long)$7,True);
		}
	|	COMPOUND_STRING '(' ID ')'
		{
		    $$ = (char *) expr_STRING_Compound($3, False, True);
		}
	|	COMPOUND_STRING '(' STRING ')'
		{
		    $$ = (char *) expr_STRING_Compound($3, False, False);
		}
;

callbacks:	CALLBACK '{' callback_list '}' ';'
		{
		    $$ = $3;
		}
	|	CALLBACK ID ';'
                {
		    $$ = InheritCallback($2);
		}
;

callback_list:	callback_list ID '=' PROCEDURE ID '(' proc_argument_list ')' ';'
		{
		    $$ = (char *)callbacklist_callbacklist_PROCID_arglist($1,
									  $2,
									  $5,
									  $7);
		}
	|	callback_list ID '=' PROCEDURES '{' procedure_call_list '}' ';'
		{
		    __MrmWarn(LOC,"NO OP************\n");
		}
	|
		{
		    $$ = NULL;
		}
;

procedure_call_list: procedure_call_list ID '(' proc_argument_list ')' ';'
		{
		    __MrmWarn(LOC, "NO OP\n");
		}
	|
		{
		}
;

proc_argument_list: proc_argument_list ',' INTEGER
		{
		    $$ = Parameter($1, prim_exp($3));
		}
	|	proc_argument_list ',' ID
		{
		    $$ = Parameter($1, expr_ID($3));
		}
	|	proc_argument_list ',' STRING
		{
		    $$ = Parameter($1, expr_STRING($3, NULL, 0));
		}
	|	INTEGER
		{
		    $$ = Parameter(NULL, prim_exp($1));
		}
	|	ID
		{
		    $$ = Parameter(NULL, expr_ID($1));
		}
	|	STRING
		{
		    $$ = Parameter(NULL, expr_STRING($1, NULL, 0));
		}
	|	compound_string
		{
		    $$ = Parameter(NULL, expr_STRING($1, NULL, 1));
		}
	|
		{
		    $$ = Parameter(NULL, NULL);
		}
;


%%


void
yyerror(char *s)
{
    fprintf(stderr,"%s:%d: %s\n", FileName, LineNumber, s);
    fprintf(stderr," current token \'%s\'\n", yytext);
}


void
yyExit(int line, char *file, char *fmt, ...)
{
    va_list ap;

    yyerror("");

    va_start (ap, fmt);

    fprintf(stderr,"%s:%d:", file, line);
    vfprintf(stderr, fmt, ap);

    va_end(ap);

    exit(1);
}
