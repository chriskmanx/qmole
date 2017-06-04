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

#include <xcb/xcb_aux.h>

#include "test-boilerplate.h"

static void
monitor_event_func (SnMonitorEvent *event,
                    void            *user_data)
{
  SnMonitorContext *context;
  SnStartupSequence *sequence;

  context = sn_monitor_event_get_context (event);
  sequence = sn_monitor_event_get_startup_sequence (event);

  switch (sn_monitor_event_get_type (event))
    {
    case SN_MONITOR_EVENT_INITIATED:
    case SN_MONITOR_EVENT_CHANGED:
      {
        const char *s;

        if (sn_monitor_event_get_type (event) == SN_MONITOR_EVENT_INITIATED)
          {
            printf ("Initiated sequence %s\n",
                    sn_startup_sequence_get_id (sequence));
          }
        else
          {
            printf ("Changed sequence %s\n",
                    sn_startup_sequence_get_id (sequence));
          }

        s = sn_startup_sequence_get_id (sequence);
        printf (" id %s\n", s ? s : "(unset)");

        s = sn_startup_sequence_get_name (sequence);
        printf (" name %s\n", s ? s : "(unset)");

        s = sn_startup_sequence_get_description (sequence);
        printf (" description %s\n", s ? s : "(unset)");

        printf (" workspace %d\n",
                sn_startup_sequence_get_workspace (sequence));

        s = sn_startup_sequence_get_binary_name (sequence);
        printf (" binary name %s\n", s ? s : "(unset)");
        s = sn_startup_sequence_get_icon_name (sequence);
        printf (" icon name %s\n", s ? s : "(unset)");

        s = sn_startup_sequence_get_wmclass (sequence);
        printf (" wm class %s\n", s ? s : "(unset)");
      }
      break;

    case SN_MONITOR_EVENT_COMPLETED:
      printf ("Completed sequence %s\n",
              sn_startup_sequence_get_id (sequence));
      break;

    case SN_MONITOR_EVENT_CANCELED:
      printf ("Canceled sequence %s\n",
              sn_startup_sequence_get_id (sequence));
      break;
    }
}

int
main (int argc, char **argv)
{
  xcb_connection_t *xconnection;
  SnDisplay *display;
  SnMonitorContext *context;
  int screen;

  xconnection = xcb_connect (NULL, &screen);
  if (xconnection == NULL)
    {
      fprintf (stderr, "Could not open display\n");
      return 1;
    }

  /* We have to select for property events on at least one
   * root window (but not all as INITIATE messages go to
   * all root windows)
   */
  xcb_screen_t *s = xcb_aux_get_screen (xconnection, screen);
  const uint32_t select_input_val[] = { XCB_EVENT_MASK_PROPERTY_CHANGE };
  xcb_change_window_attributes (xconnection, s->root, XCB_CW_EVENT_MASK,
                                select_input_val);

  display = sn_xcb_display_new (xconnection, NULL, NULL);

  context = sn_monitor_context_new (display, screen,
                                    monitor_event_func,
                                    NULL, NULL);

  while (TRUE)
    {
      xcb_generic_event_t *xevent = xcb_wait_for_event (xconnection);

      sn_xcb_display_process_event (display, xevent);

      free (xevent);
    }

  sn_monitor_context_unref (context);

  return 0;
}
