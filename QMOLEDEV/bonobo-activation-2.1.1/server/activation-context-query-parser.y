/*
 *  oafd: OAF CORBA dameon.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 1999, 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Elliot Lee <sopwith@redhat.com>,
 *
 */

%{
#include "activation-context-query.h"

#include <glib.h>
#include <stdlib.h>

void yyerror(char *s);
int yylex ();
int yyparse (void);
void initFlex (const char *s);

static QueryExpr *parsed_expression;
%}

%union
{
  char *val_string;
  char **val_stringv;
  gdouble val_number;
  gboolean val_boolean;
  QueryExpr *qexp;
  GSList *elist;
  int val_enum;
}

%token P_CONST_STRING P_CONST_NUMBER P_CONST_BOOLEAN P_CONST_ID
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%right P_NOT P_NEGATE
%token P_DOLLAR
%left P_MULTIPLY P_DIVIDE
%left P_ADD P_SUBTRACT
%token P_EQ P_NEQ P_LEQ P_GEQ P_LT P_GT
%left P_OR P_AND P_XOR
%token COMMA PERIOD
%token PARSE_ERROR

%type <val_string> P_CONST_STRING
%type <val_number> P_CONST_NUMBER
%type <val_boolean> P_CONST_BOOLEAN
%type <val_string> P_CONST_ID
%type <qexp> expr
%type <qexp> expr_binop
%type <qexp> expr_unop
%type <qexp> expr_constant
%type <qexp> expr_variable
%type <qexp> expr_function
%type <qexp> expr_id
%type <elist> exprlist
%type <qexp> expr_sub
%type <val_stringv> expr_stringv
%type <elist> stringlist
%type <qexp> expr_obvious
%type <val_enum> binop

%%

whole_expression: expr { parsed_expression = $1; };

expr: expr_binop
      | expr_obvious;

exprlist: expr { $$ = g_slist_prepend(NULL, $1); }
      | expr COMMA exprlist { $$ = g_slist_prepend($3, $1); };

expr_obvious: expr_unop | expr_sub | expr_constant | expr_variable | expr_function | expr_id;

expr_sub: LPAREN expr RPAREN { $$ = $2; };

binop: P_MULTIPLY { $$ = P_MULTIPLY; }
| P_DIVIDE { $$ = P_DIVIDE; }
| P_SUBTRACT { $$ = P_SUBTRACT; }
| P_ADD { $$ = P_ADD; }
| P_EQ { $$ = P_EQ; }
| P_NEQ { $$ = P_NEQ; }
| P_LEQ { $$ = P_LEQ; }
| P_GEQ { $$ = P_GEQ; }
| P_GT { $$ = P_GT; }
| P_LT { $$ = P_LT; }
| P_OR { $$ = P_OR; }
| P_AND { $$ = P_AND; }
| P_XOR { $$ = P_XOR; };

expr_binop: expr_obvious binop expr { $$ = qexp_binop_new ($1, $2, $3); };

expr_unop: P_NOT expr_obvious { $$ = qexp_unop_new (P_NOT, $2); }
| P_SUBTRACT expr_obvious %prec P_NEGATE { $$ = qexp_unop_new (P_NEGATE, $2); };

expr_constant: P_CONST_STRING {
	  QueryExprConst qctmp;
	  qctmp.type = CONST_STRING;
	  qctmp.u.v_string = $1;
	  $$ = qexp_constant_new (qctmp);
        }
        | P_CONST_NUMBER {
	  QueryExprConst qctmp;
	  qctmp.type = CONST_NUMBER;
	  qctmp.u.v_number = $1;
	  $$ = qexp_constant_new (qctmp);
	}
        | P_CONST_BOOLEAN {
	  QueryExprConst qctmp;
	  qctmp.type = CONST_BOOLEAN;
	  qctmp.u.v_boolean = $1;
	  $$ = qexp_constant_new (qctmp);
	}
	| expr_stringv {
	  QueryExprConst qctmp;
	  qctmp.type = CONST_STRINGV;
	  qctmp.u.v_stringv = $1;
	  $$ = qexp_constant_new (qctmp);
	};

expr_stringv: LBRACKET stringlist RBRACKET {
  char **new_stringv;
  int i, n;
  GSList *cur;

  n = g_slist_length($2);
  new_stringv = g_new (char *, n + 1);
  for (cur = $2, i = 0; i < n; i++, cur = cur->next) {
    new_stringv[i] = cur->data;
  }
  new_stringv[i] = NULL;

  g_slist_free ($2);

  $$ = new_stringv;
};

stringlist: P_CONST_STRING { $$ = g_slist_prepend (NULL, $1); }
	| stringlist COMMA P_CONST_STRING { $$ = g_slist_append ($1, $3); };

expr_variable: P_DOLLAR P_CONST_ID { $$ = qexp_variable_new ($2); };

expr_function: P_CONST_ID LPAREN exprlist RPAREN { $$ = qexp_function_new ($1, $3); }
	| P_CONST_ID LPAREN RPAREN { $$ = qexp_function_new ($1, NULL); }
	| P_CONST_ID PERIOD P_CONST_ID LPAREN exprlist RPAREN {
	$$ = qexp_function_new($3, g_slist_prepend ($5, qexp_id_new ($1))); }
	| P_CONST_ID PERIOD P_CONST_ID LPAREN RPAREN {
	$$ = qexp_function_new($3, g_slist_prepend (NULL, qexp_id_new ($1))); };

expr_id: P_CONST_ID { $$ = qexp_id_new ($1); };

%%

static GString *parse_errors = NULL;

void yyerror (char *s)
{
  g_string_append (parse_errors, s);
  g_string_append_c (parse_errors, '\n');
}

const char *qexp_parse (const char *_code, 
			QueryExpr **retme)
{
  parsed_expression = NULL;

  g_assert (retme);

  if (!parse_errors)
    parse_errors = g_string_new (NULL);
  else
    g_string_truncate (parse_errors, 0);

  initFlex (_code);
  yyparse();

  *retme = parsed_expression;

  if (parse_errors->len)
    return parse_errors->str;
  else
    return NULL;
}
