%{
/*

A good bit of this file is taken from kdelibs/corba/kded/lex.l, which
solved almost the same problem in a nice way. It was written by either
David Faure or Steffan Hansen (unclear which). Don Comeau also
deserves credit for teaching me how to use the whole lex/yacc thing in
the beginning.

  - ECL
 */

#include "activation-context-query.h"
#include "activation-context-query-parser.h"

#include <string.h>
#include <stdlib.h>
#define YY_NO_UNPUT

static char* putSymbol (char *_name);
static char* putString (char *_name);
static int yywrap (void);
int yylex (void);
void initFlex (const char *_code);

%}

DIGIT	[0-9]

%%

"=="  { return P_EQ; }
"!="  { return P_NEQ; }
"<"   { return P_LT; }
">"   { return P_GT; }
"<="  { return P_LEQ; }
">="  { return P_GEQ; }
"&&"  { return P_AND; }
"AND" { return P_AND; }
"||"  { return P_OR; }
"OR"  { return P_OR; }
"~"   { return P_NOT; }
"NOT" { return P_NOT; }
"^^"  { return P_XOR; }
"XOR" { return P_XOR; }

"/"   { return P_DIVIDE; }
"+"   { return P_ADD; }
"-"   { return P_SUBTRACT; }
"*"   { return P_MULTIPLY; }
","   { return COMMA; }
"."   { return PERIOD; }

"("   { return LPAREN; }
")"   { return RPAREN; }
"["   { return LBRACKET; }
"]"   { return RBRACKET; }

"$"   { return P_DOLLAR; }

(TRUE|true|True|YES|yes|Yes) { yylval.val_boolean = TRUE; return P_CONST_BOOLEAN; }
(FALSE|false|False|NO|no|No) { yylval.val_boolean = FALSE; return P_CONST_BOOLEAN; }

"'"(\\'|[^'])+"'" { yylval.val_string = putString (yytext); return P_CONST_STRING; }

{DIGIT}+"."{DIGIT}+ { yylval.val_number = atof (yytext); return P_CONST_NUMBER; }
{DIGIT}+ { yylval.val_number = atof (yytext); return P_CONST_NUMBER; }

[a-zA-Z_][a-zA-Z0-9_:]* { yylval.val_string = putSymbol (yytext); return P_CONST_ID; }

[ \t\n\r]+          /* eat up whitespace */

. { return PARSE_ERROR; }

%%

static char *
putSymbol (char *_name)
{
  return g_strdup (_name);
}

static char *
putString (char *_str)
{
  int l = strlen (_str);
  char *p = (char*) g_malloc (l + 1);
  char *s = _str + 1;
  char *d = p;
  while (s < _str + l - 1)
  {
     if (*s != '\\') {
        *d++ = *s++;
     } else {
        s++;
        if (*s == '\\')
          *d++ = '\\';
        else if (*s == 'n')
          *d++ = '\n';
        else if (*s == 'r')
          *d++ = '\r';
        else if (*s == 't')
          *d++ = '\t';
        s++;
     }
  }

  *d = 0;
  return p;
}

void
initFlex (const char *_code)
{
  yy_switch_to_buffer (yy_scan_string (_code));
}

static int
yywrap (void)
{
  yy_delete_buffer (YY_CURRENT_BUFFER);
  return 1;
}
