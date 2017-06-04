/*
 * Copyright (C) 2002 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <signal.h>

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#define NULL ((void*) 0)
#endif

#ifdef HAVE_BACKTRACE
#include <execinfo.h>
static void
print_backtrace (void)
{
  void *bt[500];
  int bt_size;
  int i;
  char **syms;
  
  bt_size = backtrace (bt, 500);

  syms = backtrace_symbols (bt, bt_size);
  
  i = 0;
  while (i < bt_size)
    {
      fprintf (stderr, "  %s\n", syms[i]);
      ++i;
    }

  free (syms);
}
#else
static void
print_backtrace (void)
{
  fprintf (stderr, "Not compiled with backtrace support\n");
}
#endif

static int error_trap_depth = 0;

static int
x_error_handler (Display     *xdisplay,
                 XErrorEvent *error)
{
  char buf[64];
  
  XGetErrorText (xdisplay, error->error_code, buf, 63);

  if (error_trap_depth == 0)
    {
      print_backtrace ();
      
      fprintf (stderr, "Unexpected X error: %s serial %ld error_code %d request_code %d minor_code %d)\n",
                buf,
                error->serial, 
                error->error_code, 
                error->request_code,
                error->minor_code);

      exit (1);
    }

  return 1; /* return value is meaningless */
}

static void
error_trap_push (SnDisplay *display,
                 Display   *xdisplay)
{
  ++error_trap_depth;
}

static void
error_trap_pop (SnDisplay *display,
                Display   *xdisplay)
{
  if (error_trap_depth == 0)
    {
      fprintf (stderr, "Error trap underflow!\n");
      exit (1);
    }
  
  XSync (xdisplay, False); /* get all errors out of the queue */
  --error_trap_depth;
}
