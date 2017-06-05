%{
/*
 * comb files, looking for typedefs
 * [UNDER CONSTRUCTION]
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static int intypedef = 0, inparens = 0;
static char otherbuf[4096] = { 0 };
static int nested = 0, sawname = 0, parend = 0, skipone = 0;

%}
comment		\/\*([^\*][^\/])*\*\/
ws		[ \t\n\r\f]+
punct		[\~\`\!\@\#\$\%\^\&\*\-\+\=\|\<\>\.\?\/\"\'\[\]\:]+
ident		[a-zA-z_][a-zA-Z_0-9]*
number		[0-9]+
%%
{ws}		{ /* skip it */ }
{comment}	{ /* skip it */ }
{punct}		{ /* skip it */ }
"typedef"	{ intypedef = 1; inparens = 0; nested = 0; parend = 0; skipone = 0; }
"{"		{ if (intypedef) nested++; }
"}"		{ if (intypedef) nested--; }
"("		{
			if (!intypedef || nested)
				break;
			inparens = 1;
			parend++;
		}
"\)"		{
			if (!intypedef || nested)
				break;
			parend--;
			if (parend == 0 && !skipone) {
				skipone = 1;
				printf("%s\n", otherbuf);
			}
			else if (parend == 0) {
				skipone = 0;
				inparens = 0;
				otherbuf[0] = 0;
			}
		}
","		{
			if (intypedef && !nested && !parend)
				printf("%s\n", otherbuf);
		}
";"		{
			if (intypedef && !nested && !inparens) {
				intypedef = 0;
				if (strlen(otherbuf) != 0)
					printf("%s\n", otherbuf);
			}
		}
{ident}		{ strcpy(otherbuf, yytext); }
{number}	{ /* skip it */ }
%%
#ifdef yywrap
#undef yywrap
#endif
int
yywrap()
{
    return 1;
}

int
main(int argc, char *argv[])
{
    while (yylex())
	;
    exit(0);
}
