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

#include <config.h>
#include <libsn/sn.h>
#include <libsn/sn-xmessages.h>
#include <libsn/sn-internals.h>

#include "test-boilerplate.h"

static void
message_func (SnDisplay       *display,
              const char      *message_type,
              const char      *message,
              void            *user_data)
{
  char *prefix;
  char **names;
  char **values;
  int i;

#if 0
  printf ("raw %s: %s\n",
          message_type, message);
#endif
  
  prefix = NULL;
  names = NULL;
  values = NULL;

  if (sn_internal_unserialize_message (message,
                                       &prefix, &names, &values))
    {
      printf (" %s:\n", prefix);

      i = 0;
      while (names && names[i])
        {
          printf ("   '%s' = '%s'\n", names[i], values[i]);
          
          ++i;
        }

      sn_internal_strfreev (names);
      sn_internal_strfreev (values);
    }
}

int
main (int argc, char **argv)
{
  Display *xdisplay;
  SnDisplay *display;

  if (argc != 3)
    {
      fprintf (stderr, "arguments must be type and begin type of events to watch\n");
      return 1;
    }
  
  xdisplay = XOpenDisplay (NULL);
  if (xdisplay == NULL)
    {
      fprintf (stderr, "Could not open display\n");
      return 1;
    }

  if (getenv ("LIBSN_SYNC") != NULL)
    XSynchronize (xdisplay, True);
  
  XSetErrorHandler (x_error_handler);

  /* We have to select for property events on one root window
   */
  XSelectInput (xdisplay, DefaultRootWindow (xdisplay),
                PropertyChangeMask);
  
  display = sn_display_new (xdisplay,
                            error_trap_push,
                            error_trap_pop);

  sn_internal_add_xmessage_func (display, DefaultScreen (xdisplay),
                                 argv[1], argv[2],
                                 message_func,
                                 NULL, NULL);
  
  while (TRUE)
    {
      XEvent xevent;

      XNextEvent (xdisplay, &xevent);

      sn_display_process_event (display, &xevent);
    }
  
  return 0;
}
