/*
 * Copyright (C) 2002 Red Hat, Inc.
 * Copyright (C) 2002 Julien Danjou <julien@danjou.info>
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
#include <assert.h>

#include <xcb/xcb_atom.h>
#include <xcb/xcb_aux.h>

#include "test-boilerplate.h"

static pid_t child_pid = 0;

/* This is a poor way to obtain a timestamp normally (one should be available
 * to the app from the user clicking on a button or something), but such a
 * method is not available for this simple test application.
 */
xcb_timestamp_t
slowly_obtain_timestamp (SnDisplay *display)
{
  xcb_window_t xwindow;
  xcb_connection_t *xconnection;
  xcb_generic_event_t *event;
  xcb_screen_t *s;

  xconnection = sn_display_get_x_connection (display);

  s = xcb_aux_get_screen(xconnection, 0);

  {
    uint32_t attrs[] = { 1, XCB_EVENT_MASK_PROPERTY_CHANGE | XCB_EVENT_MASK_STRUCTURE_NOTIFY };
    char* name;

    xwindow = xcb_generate_id (xconnection);

    xcb_create_window (xconnection, XCB_COPY_FROM_PARENT, xwindow,
                       s->root, -100, -100, 1, 1, 0, XCB_COPY_FROM_PARENT,
                       XCB_COPY_FROM_PARENT, XCB_CW_OVERRIDE_REDIRECT | XCB_CW_EVENT_MASK,
                       attrs);

    name = "Fake Window";

    xcb_change_property (xconnection, XCB_PROP_MODE_REPLACE,
                         xwindow, WM_NAME, STRING, 8,
                         strlen (name), name);
  }

  xcb_flush (xconnection);
  event = xcb_wait_for_event (xconnection);
  xcb_property_notify_event_t *ev = (xcb_property_notify_event_t *) event;
  xcb_timestamp_t timestamp = ev->time;
  free (ev);

  xcb_destroy_window (xconnection, xwindow);

  return timestamp;
}

int
main (int argc, char **argv)
{
  xcb_connection_t *xconnection;
  SnDisplay *display;
  SnLauncherContext *context;
  xcb_timestamp_t timestamp;
  int screen;

  if (argc < 2)
    {
      fprintf (stderr, "must specify command line to launch\n");
      exit (1);
    }

  xconnection = xcb_connect (NULL, &screen);
  if (xconnection == NULL)
    {
      fprintf (stderr, "Could not open display\n");
      return 1;
    }

  display = sn_xcb_display_new (xconnection,
                                NULL, NULL);

  context = sn_launcher_context_new (display, screen);

  sn_launcher_context_set_name (context, "Test Launch");
  sn_launcher_context_set_description (context, "Launching a test program for libsn");
  sn_launcher_context_set_binary_name (context, argv[1]);

  timestamp = slowly_obtain_timestamp (display);
  sn_launcher_context_initiate (context,
                                "test-launcher",
                                argv[1],
                                timestamp);

  switch ((child_pid = fork ()))
    {
    case -1:
      fprintf (stderr, "Fork failed: %s\n", strerror (errno));
      break;
    case 0:
      sn_launcher_context_setup_child_process (context);
      execv (argv[1], argv + 1);
      fprintf (stderr, "Failed to exec %s: %s\n", argv[1], strerror (errno));
      _exit (1);
      break;
    }

  while (TRUE)
    {
      xcb_generic_event_t *xevent = xcb_wait_for_event (xconnection);

      sn_xcb_display_process_event (display, xevent);

      free (xevent);
    }

  sn_launcher_context_unref (context);

  return 0;
}
