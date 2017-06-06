/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _SCRIPT_H_
#define _SCRIPT_H_

#include <stdio.h>
#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

/************ Macros and Definitions ************/

/************ Structures ************/
typedef void (*eterm_script_handler_function_t)(spif_charptr_t *);
typedef struct {
  char *name;
  eterm_script_handler_function_t handler;
} eterm_script_handler_t;

/************ Variables ************/

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

/* Handlers */
extern void script_handler_copy(spif_charptr_t *);
extern void script_handler_echo(spif_charptr_t *);
extern void script_handler_exec_dialog(spif_charptr_t *);
extern void script_handler_exit(spif_charptr_t *);
extern void script_handler_kill(spif_charptr_t *);
extern void script_handler_msgbox(spif_charptr_t *);
extern void script_handler_paste(spif_charptr_t *);
extern void script_handler_save(spif_charptr_t *);
extern void script_handler_save_buff(spif_charptr_t *);
extern void script_handler_scroll(spif_charptr_t *);
extern void script_handler_search(spif_charptr_t *);
extern void script_handler_spawn(spif_charptr_t *);
extern void script_handler_string(spif_charptr_t *);
extern void script_handler_nop(spif_charptr_t *);

#ifdef ESCREEN
extern void script_handler_es_display(spif_charptr_t *);
extern void script_handler_es_region(spif_charptr_t *);
extern void script_handler_es_statement(spif_charptr_t *);
extern void script_handler_es_reset(spif_charptr_t *);
#endif

/* Engine */
extern eterm_script_handler_t *script_find_handler(const char *);
extern void script_parse(char *);

_XFUNCPROTOEND

#endif	/* _SCRIPT_H_ */
