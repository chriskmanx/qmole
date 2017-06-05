/* A byte code interpreter for Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Authors: Lars Wirzenius <liw@iki.fi>
 *          Kenneth Oksanen <cessu@iki.fi> (minor tweaking)
 */

/* This file, to be executed on the host, takes the definitions and
   documentation of builtins in `hh_builtins.def' and produces an SGML
   document of them. */

#include "hh_common.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define EMPTY_CELL "any"
#define STRING_CELL "string"
#define INTEGER_CELL "integer"
#define CONS_CELL "cons"
#define CONS3_CELL "cons3"
#define SYMBOL_CELL "symbol"
#define AVL_CELL "AVL-tree node"

#define MAX_ARGS 128
#define MAX_FUNCS 128

struct arg {
  int single;
  const char *name;
  const char *type;
  const char *doc;
};

struct func {
  const char *name;
  const char *doc;
  struct arg args[MAX_ARGS + 1];
};

struct module {
  const char *doc;
  struct func funcs[MAX_FUNCS + 1];
};


struct module modules[] = {
#define MODULE(doc)				\
  {						\
    doc,					\
      {

#define MODULE_END				\
      }						\
  },
    	
#define ARG(name, type, doc)			\
  {						\
    1,						\
    #name,					\
    type,					\
    doc,					\
  },

#define REMAINING_ARGS(name, type, doc)		\
  {						\
    0,						\
    #name,					\
    type,					\
    doc,					\
  },

#define BUILTIN(lisp_name, c_name, doc_string, args, code_gen)	\
  {								\
    lisp_name,							\
    doc_string,							\
    { { -1, "XXX" }, args },					\
  },
    
#include "hh_builtins.def"
    
};

const int num_modules = sizeof(modules) / sizeof(modules[0]);

void safe(const char *tag, const char *value)
{
  if (tag)
    printf("<%s>", tag);
  for (; *value != '\0'; ++value) {
    switch (*value) {
    case '<': printf("&lt;"); break;
    case '>': printf("&gt;"); break;
    case '&': printf("&amp;"); break;
    default:  printf("%c", *value); break;
    }
  }
  if (tag)
    printf("</%s>", tag);
}

void title2id(const char *title)
{
    for (; *title != '\0'; ++title) {
	if (isalnum(*title))
	    printf("%c", tolower(*title));
    }
}

int main(void)
{
  int i;
  int j;
  int has_args;
  struct module *m;
  struct func *f;
  struct arg *a;
  
  for (i = 0; i < num_modules; ++i) {
    m = &modules[i];
    if (m->doc == NULL)
      continue;
    if (strstr(m->doc, "Internal") != NULL)
    	continue;
    
    printf("<sect2 id='");
    title2id(m->doc);
    printf("'>\n");
    safe("title", m->doc);
    printf("<glosslist>\n");
    
    for (j = 0; m->funcs[j].name != NULL; ++j) {
      f = &m->funcs[j];
      
      printf("<glossentry>\n");
      printf("<glossterm>(");
      safe("literal", f->name);
      has_args = 0;
      for (a = f->args; a->name != NULL; ++a) {
	if (a->single == -1)
	  continue;
	printf(" ");
	safe("replaceable", a->name);
	has_args = 1;
      }
      printf(")</glossterm>\n");
      
      printf("<glossdef>\n");
      safe("para", f->doc);
      
      if (has_args) {
	printf("<itemizedlist>\n");
	for (a = f->args; a->name != NULL; ++a) {
	  if (a->single == -1)
	    continue;
	  printf("<listitem><para>");
	  safe("replaceable", a->name);
	  if (a->single)
	    printf(" (%s): ", a->type);
	  else
	    printf(" (%s, optional): ", a->type);
	  safe(NULL, a->doc);
	  printf("</para></listitem>");
	}
	printf("</itemizedlist>\n");
      }
      
      printf("</glossdef>\n");
      printf("</glossentry>\n");
    }
    
    printf("</glosslist>\n");
    printf("</sect2>\n");
  }
  
  return 0;
}
