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
#include <assert.h>

#include "test-boilerplate.h"

static pid_t child_pid = 0;

/* This is a poor way to obtain a timestamp normally (one should be available
 * to the app from the user clicking on a button or something), but such a
 * method is not available for this simple test application.
 */
Time
slowly_obtain_timestamp (SnDisplay *display)
{
  Window xwindow;
  Display *xdisplay;
  XEvent event;

  xdisplay = sn_display_get_x_display (display);

  {
    XSetWindowAttributes attrs;
    Atom atom_name;
    Atom atom_type;
    char* name;

    attrs.override_redirect = True;
    attrs.event_mask = PropertyChangeMask | StructureNotifyMask;

    xwindow =
      XCreateWindow (xdisplay,
                     RootWindow (xdisplay, 0),
                     -100, -100, 1, 1,
                     0,
                     CopyFromParent,
                     CopyFromParent,
                     CopyFromParent,
                     CWOverrideRedirect | CWEventMask,
                     &attrs);

    atom_name = XInternAtom (xdisplay, "WM_NAME", TRUE);
    assert (atom_name != None);
    atom_type = XInternAtom (xdisplay, "STRING", TRUE);
    assert (atom_type != None);

    name = "Fake Window";
    XChangeProperty (xdisplay, 
                     xwindow, atom_name,
                     atom_type,
                     8, PropModeReplace, name, strlen (name));
  }
  
  XWindowEvent (xdisplay,
                xwindow,
                PropertyChangeMask,
                &event);

  XDestroyWindow (xdisplay, xwindow);

  return event.xproperty.time;
}

int
main (int argc, char **argv)
{
  Display *xdisplay;
  SnDisplay *display;
  SnLauncherContext *context;
  Time timestamp;

  if (argc < 2)
    {
      fprintf (stderr, "must specify command line to launch\n");
      exit (1);
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
  
  display = sn_display_new (xdisplay,
                            error_trap_push,
                            error_trap_pop);

  context = sn_launcher_context_new (display, DefaultScreen (xdisplay));

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
      XEvent xevent;

      XNextEvent (xdisplay, &xevent);

      sn_display_process_event (display, &xevent);
    }

  sn_launcher_context_unref (context);
  
  return 0;
}
