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

#ifndef _E_H_
#define _E_H_
/* includes */
#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

/************ Macros and Definitions ************/
#define IPC_TIMEOUT    ((char *) 1)

#define enl_ipc_sync()  do {if (check_image_ipc(0)) {char *reply = enl_send_and_wait("nop"); FREE(reply);}} while (0)

/************ Variables ************/
extern Window ipc_win;

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern unsigned char check_for_enlightenment(void);
extern Window enl_ipc_get_win(void);
extern void enl_ipc_send(char *);
extern char *enl_wait_for_reply(void);
extern char *enl_ipc_get(const char *);
extern void enl_query_for_image(unsigned char);
extern char *enl_send_and_wait(char *);

_XFUNCPROTOEND

#endif	/* _E_H_ */
