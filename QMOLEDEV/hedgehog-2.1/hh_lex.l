%{
/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* This file implements a lexer for HogLisp.
 */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_ast.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* This is like normal atoi, but returns unsigned value and checks for
   overflow. */

static hh_signed_word_t hh_atoi(char *s)
{
  hh_signed_word_t r = 0;
  int is_negative = 0;

  if (*s == '-') {
    is_negative = 1;
    s++;
  }

  while (*s >= '0' && *s <= '9') {
    if (is_negative) {
      if (r <= -214748365 || (r == -214748364 && *s == '9'))
	hh_fatal(NULL, "Too large integer constant");
      r = 10 * r - (*s++ - '0');
    } else {
      if (r >= 214748365 || (r == 214748364 && *s >= '8'))
	hh_fatal(NULL, "Too large integer constant");
      r = 10 * r + (*s++ - '0');
    }
  }
  return r;
}


/* Checked conversion from hex value to unsigned long. */

static hh_word_t hh_htoi(char *s)
{
  hh_word_t r = 0;
  unsigned int i;

  while (1) {
    if (*s >= '0' && *s <= '9')
      i = *s - '0';
    else if (*s >= 'a' && *s <= 'f')
      i = 10 + *s - 'a';
    else if (*s >= 'A' && *s <= 'F')
      i = 10 + *s - 'A';
    else
      return r;
    if (r >= 0x10000000UL)
      hh_fatal(NULL, "Too large hexadecimal constant");
    r = (r << 4) | i;
    s++;
  }
  HH_NOTREACHED;
}


/* Checked conversion from octal value to unsigned long. */

static hh_word_t hh_otoi(char *s)
{
  hh_word_t r = 0;

  while (*s >= '0' && *s <= '7') {
    if (r >= 0x20000000UL)
      hh_fatal(NULL, "Too large octal constant");
    r = (r << 3) | (*s++ - '0');
  }
  return r;
}


/* Mangle away escape characters in strings, return the new length. */

static unsigned int hh_mangle(char *s)
{
  char *to = s, *from = s;
  int temp;

  while (1) {
    if (*from == '\"')
      return to - s;
    if (*from == '\\') {
      switch (*++from) {
      case '\\': *to++ = '\\'; ++from; break;
      case '\'': *to++ = '\''; ++from; break;
      case '"': *to++ = '"'; ++from; break;
      case 'a': *to++ = '\a'; ++from; break;
      case 'b': *to++ = '\b'; ++from; break;
      case 't': *to++ = '\t'; ++from; break;
      case 'n': *to++ = '\n'; ++from; break;
      case 'v': *to++ = '\v'; ++from; break;
      case 'f': *to++ = '\f'; ++from; break;
      case 'r': *to++ = '\r'; ++from; break;

      case '0': case '1': case '2': case '3':
      case '4': case '5': case '6': case '7':
      	/* Octal. Up to three digits. */
	temp = *from++ - '0';
	if (*from >= '0' && *from <= '7') {
	  temp = temp * 8 + *from++ - '0';
	  if (*from >= '0' && *from <= '7')
	    temp = temp * 8 + *from++ - '0';
	}
	*to++ = temp & 0xff;
	break;

      case 'x':
      case 'X':
      	/* Hexadecimal. Any number of digits. Value is limited to 0-255. */
	temp = 0;
    	for (++from; ; ++from) {
	  if (*from >= '0' && *from <= '9')
	    temp = temp * 16 + *from - '0';
	  else if (*from >= 'a' && *from <= 'f')
	    temp = temp * 16 + *from - 'a' + 10;
	  else if (*from >= 'A' && *from <= 'F')
	    temp = temp * 16 + *from - 'A' + 10;
	  else
	    break;
	}
	*to++ = temp & 0xff;
	break;
      	
      default:
	hh_fatal(NULL, "Unknown escape sequence: \\%c (0x%02x)", *from, *from);
      }
    } else
      *to++ = *from++;
  }
  HH_NOTREACHED;
}


static enum {
  HH_DEFINE, HH_IFDEF, HH_IFNDEF
} hh_directive_kind;

typedef struct hh_directive_t {
  struct hh_directive_t *next;
  char *name;
} hh_directive_t;

static hh_directive_t *hh_directives = NULL;

static int hh_directive_is_defined(const char *name)
{
  hh_directive_t *d;

  for (d = hh_directives; d != NULL; d = d->next)
    if (strcmp(d->name, name) == 0)
      return 1;
  return 0;
}

void hh_directive_define(const char *name)
{
  hh_directive_t *d;

  if (hh_directive_is_defined(name))
    return;
  d = malloc(sizeof(hh_directive_t));
  if (d == NULL) {
    fprintf(stderr, "Out of memory when defining macro directives.\n");
    exit(1);
  }
  d->name = malloc(strlen(name) + 1);
  if (d->name == NULL) {
    fprintf(stderr, "Out of memory when defining macro directives.\n");
    exit(1);
  }
  strcpy(d->name, name);
  d->next = hh_directives;
  hh_directives = d;
}


typedef enum {
  /* The current input is parsed. */
  HH_INPUT_ACTIVE,
  /* The current input might be parsed in an #else branch. */
  HH_INPUT_SUPPRESSED,
  /* The current input won't be parsed within the current #ifdef .. #endif. */
  HH_INPUT_INACTIVE,
} hh_ifdef_input_t;

#define HH_IFDEF_STACK_DEPTH  32
static hh_ifdef_input_t hh_ifdef_stack[HH_IFDEF_STACK_DEPTH];

static int hh_ifdef_level = 0;


static int hh_ifdef_push(const char *name, int inverted)
{
  hh_ifdef_input_t input;

  if (hh_ifdef_level == HH_IFDEF_STACK_DEPTH)
    hh_fatal(NULL, "Too many nested #ifdef's");

  if (hh_ifdef_level == 0
      || hh_ifdef_stack[hh_ifdef_level - 1] == HH_INPUT_ACTIVE)
    input = HH_INPUT_ACTIVE;
  else
    input = HH_INPUT_INACTIVE;

  if (input == HH_INPUT_ACTIVE)
    if ((inverted && hh_directive_is_defined(name))
	|| (!inverted && !hh_directive_is_defined(name)))
      input = HH_INPUT_SUPPRESSED;

  hh_ifdef_stack[hh_ifdef_level] = input;
  hh_ifdef_level++;

  return input == HH_INPUT_ACTIVE;
}

static int hh_ifdef_toggle(void)
{
  if (hh_ifdef_level == 0)
    hh_fatal(NULL, "Stray #else");
  
  switch (hh_ifdef_stack[hh_ifdef_level - 1]) {
  case HH_INPUT_ACTIVE:
    hh_ifdef_stack[hh_ifdef_level - 1] = HH_INPUT_SUPPRESSED;
    return 0;
  case HH_INPUT_SUPPRESSED:
    hh_ifdef_stack[hh_ifdef_level - 1] = HH_INPUT_ACTIVE;
    return 1;
  case HH_INPUT_INACTIVE:
    return 0;
  }
  HH_NOTREACHED;
  return 0;
}

static int hh_ifdef_pop(void)
{
  if (hh_ifdef_level == 0)
    hh_fatal(NULL, "Stray #endif");

  hh_ifdef_level--;
  return hh_ifdef_level == 0
    || hh_ifdef_stack[hh_ifdef_level - 1] == HH_INPUT_ACTIVE;
}


%}

%x MACRO_TOKEN MACRO_OMIT

%%

^[ \t]*#define[ \t]+	{ hh_directive_kind = HH_DEFINE; BEGIN(MACRO_TOKEN); }
<*>^[ \t]*#ifdef[ \t]+	{ hh_directive_kind = HH_IFDEF; BEGIN(MACRO_TOKEN); }
<*>^[ \t]*#ifndef[ \t]+	{ hh_directive_kind = HH_IFNDEF; BEGIN(MACRO_TOKEN); }

<MACRO_TOKEN>[0-9a-zA-Z_-]+	{
			  switch (hh_directive_kind) {
			  case HH_DEFINE:
  			    hh_directive_define(yytext);
			    BEGIN(0);
			    break;
			  case HH_IFDEF:
			    if (hh_ifdef_push(yytext, 0))
			      BEGIN(0);
			    else
			      BEGIN(MACRO_OMIT);
			    break;
			  case HH_IFNDEF:
			    if (hh_ifdef_push(yytext, 1))
			      BEGIN(0);
			    else
			      BEGIN(MACRO_OMIT);
			    break;
			  }
			}
<MACRO_TOKEN>.		{ hh_fatal(NULL, "Expected directive"); }

<*>^[ \t]*#else		{ if (hh_ifdef_toggle())
			    BEGIN(0);
			  else
			    BEGIN(MACRO_OMIT);
			}
<*>^[ \t]*#endif	{ if (hh_ifdef_pop())
			    BEGIN(0);
			  else
			    BEGIN(MACRO_OMIT);
			}
<MACRO_OMIT>.		{ /* Skip until `#endif or #else'. */ }

<*>\n			{ hh_current_line++; }
;[^\n]*			{ /* Skip comments. */ }
[ \t\r]			{ ; /* Ignore whitespaces and MS-DOSisms. */ }
\.\.\.			{ return HH_ELLIPSIS; }
-?[1-9][0-9]*		{ hh_lval = hh_alloc_node(HH_AST_INTEGER);
			  hh_lval->u.integer = hh_atoi(hh_text);
			  return HH_ATOM; }
0[xX][0-9a-fA-F]+	{ hh_lval = hh_alloc_node(HH_AST_UNSIGNED_INTEGER);
			  hh_lval->u.unsigned_integer = hh_htoi(hh_text + 2);
			  return HH_ATOM; }
0[0-7]+			{ hh_lval = hh_alloc_node(HH_AST_UNSIGNED_INTEGER);
			  hh_lval->u.unsigned_integer = hh_otoi(hh_text + 1);
			  return HH_ATOM; }
0			{ hh_lval = hh_alloc_node(HH_AST_INTEGER);
			  hh_lval->u.integer = 0;
			  return HH_ATOM; }

\'[^'\\]\'		{ hh_lval = hh_alloc_node(HH_AST_INTEGER);
                          hh_lval->u.integer = yytext[1];
                          return HH_ATOM; }
\'\\.\'			{ hh_lval = hh_alloc_node(HH_AST_INTEGER);
                          switch (yytext[2]) {
                          case '0': hh_lval->u.integer = '\0'; break;
                          case 'a': hh_lval->u.integer = '\a'; break;
                          case 'b': hh_lval->u.integer = '\b'; break;
                          case 't': hh_lval->u.integer = '\t'; break;
                          case 'n': hh_lval->u.integer = '\n'; break;
                          case 'v': hh_lval->u.integer = '\v'; break;
                          case 'f': hh_lval->u.integer = '\f'; break;
                          case 'r': hh_lval->u.integer = '\r'; break;
                          case '\\': hh_lval->u.integer = '\\'; break;
                          default:
	                    hh_fatal(NULL, "Unknown escape sequence");
                          }
                          return HH_ATOM; }

\"(\\.|[^\"\\])*\"	{ hh_lval = hh_alloc_node(HH_AST_STRING);
			  hh_lval->u.string =
			    hh_ast_string(hh_text + 1, hh_mangle(hh_text + 1));
			  return HH_ATOM; }
##			{ hh_lval = hh_alloc_node(HH_AST_SYMBOL);
			  hh_lval->u.symbol = hh_symbol_macroconcat;
			  return HH_ATOM; }
#\'			{ hh_lval = hh_alloc_node(HH_AST_SYMBOL);
			  hh_lval->u.symbol = hh_symbol_macroquote;
			  return HH_ATOM; }
[^() \t\n\r\'\"0-9$]+[^() \t\n\r]* {
			  hh_lval = hh_alloc_node(HH_AST_SYMBOL);
			  hh_lval->u.symbol = hh_ast_symbol(hh_text);
			  return HH_ATOM; }

<<EOF>>			{ hh_lval = NULL; return EOF; }
.			{ hh_lval = NULL; return *hh_text; }

%%

int hh_wrap(void)
{
  if (hh_ifdef_level != 0)
    hh_fatal(NULL, "No matching #endif");
  return 1;
}
