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

#include "test-boilerplate.h"

int
main (int argc, char **argv)
{
  xcb_connection_t *xconnection;
  SnDisplay *display;
  SnLauncheeContext *context;
  int screen;

  xconnection = xcb_connect (NULL, &screen);
  if (xconnection == NULL)
    {
      fprintf (stderr, "Could not open display\n");
      return 1;
    }

  display = sn_xcb_display_new (xconnection,
                                NULL, NULL);

  context = sn_launchee_context_new_from_environment (display, screen);

  if (context == NULL)
    {
      fprintf (stderr, "Failed to get launch feedback info from DESKTOP_LAUNCH_ID/DESKTOP_LAUNCH_WINDOW\n");
      exit (1);
    }

  printf ("Launchee started with window ID \"%s\"\n",
          sn_launchee_context_get_startup_id (context));

  /* simulate startup time */
  sleep (4);

  printf ("Launchee startup complete\n");
  sn_launchee_context_complete (context);

  while (TRUE)
    {
       xcb_generic_event_t *xevent = xcb_wait_for_event(xconnection);

      sn_xcb_display_process_event (display, xevent);

      free(xevent);
    }

  return 0;
}
