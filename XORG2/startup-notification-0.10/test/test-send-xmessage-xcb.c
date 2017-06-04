/*
 * Copyright (C) 2002 Red Hat, Inc.
 * Copyright (C) 2009 Julien Danjou <julien@danjou.info>
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

#include "test-boilerplate.h"

int
main (int argc, char **argv)
{
  xcb_connection_t *xconnection;
  SnDisplay *display;

  if (argc != 4)
    {
      fprintf (stderr, "Must specify message type, message begin type, and message content as first, second, and third args\n");
      return 1;
    }

  int screen;
  xconnection = xcb_connect (NULL, &screen);
  if (xconnection == NULL)
    {
      fprintf (stderr, "Could not open display\n");
      return 1;
    }

  display = sn_xcb_display_new (xconnection, NULL, NULL);

  sn_internal_broadcast_xmessage (display, screen,
                                  argv[1],
                                  argv[2],
                                  argv[3]);

  return 0;
}
