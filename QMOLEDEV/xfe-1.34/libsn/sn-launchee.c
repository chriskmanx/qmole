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
#include "sn-launchee.h"
#include "sn-internals.h"
#include "sn-xmessages.h"
#include <errno.h>

struct SnLauncheeContext
{
  int refcount;
  SnDisplay *display;
  int screen;
  char *startup_id;
};

/**
 * sn_launchee_context_new:
 * @display: an #SnDisplay
 * @screen: an X screen number
 * @startup_id: launch ID as in DESKTOP_STARTUP_ID env variable
 * 
 * Creates a new launchee-side context for the startup notification
 * protocol.
 * 
 * Return value: a new launchee context
 **/
SnLauncheeContext*
sn_launchee_context_new (SnDisplay         *display,
                         int                screen,
                         const char        *startup_id)
{
  SnLauncheeContext *context;

  context = sn_new0 (SnLauncheeContext, 1);
  
  context->refcount = 1;
  
  context->display = display;
  sn_display_ref (context->display);
  context->screen = screen;
  
  context->startup_id = sn_internal_strdup (startup_id);

  return context;
}

/**
 * sn_launchee_context_new_from_environment:
 * @display: an #SnDisplay
 * @screen: an X screen number
 * 
 * Tries to create an #SnLauncheeContext given information in the
 * program's environment (DESKTOP_STARTUP_ID environment
 * variable). Returns %NULL if the env variables are not available or
 * can't be parsed.
 * 
 * Return value: a new #SnLauncheeContext or %NULL
 **/
SnLauncheeContext*
sn_launchee_context_new_from_environment (SnDisplay *display,
                                          int        screen)
{
  const char *id_str;
  
  id_str = getenv ("DESKTOP_STARTUP_ID");
  
  if (id_str == NULL)
    return NULL;
  
  return sn_launchee_context_new (display, screen, id_str);
}

void
sn_launchee_context_ref (SnLauncheeContext *context)
{
  context->refcount += 1;
}

void
sn_launchee_context_unref (SnLauncheeContext *context)
{
  context->refcount -= 1;
  if (context->refcount == 0)
    {
      sn_display_unref (context->display);
      sn_free (context->startup_id);
      
      sn_free (context);
    }
}

/**
 * sn_launchee_context_get_startup_id:
 * @context: an #SnLauncheeContext
 * 
 * Get the startup ID for the context.
 * 
 * Return value: the startup ID for the context.
 **/
const char*
sn_launchee_context_get_startup_id (SnLauncheeContext *context)
{
  return context->startup_id;
}

/**
 * sn_launchee_context_get_id_has_timestamp:
 * @context: an #SnLauncheeContext
 * 
 * Return whether the startup ID for the context contains a timestamp
 * 
 * Return value: whether the startup ID has an embedded timestamp
 **/
int
sn_launchee_context_get_id_has_timestamp (SnLauncheeContext *context)
{
  char * time_str;

  time_str = sn_internal_find_last_occurrence(context->startup_id, "_TIME");

  return time_str != NULL;
}

/**
 * sn_launchee_context_get_timestamp:
 * @context: an #SnLauncheeContext
 * 
 * Return the timestamp embedded in the startup ID
 * 
 * Return value: timestamp embedded in the startup ID
 **/
Time
sn_launchee_context_get_timestamp (SnLauncheeContext *context)
{
  char * time_str;

  time_str = sn_internal_find_last_occurrence(context->startup_id, "_TIME");
  if (time_str != NULL)
    {
      /* Skip past the "_TIME" part */
      time_str += 5;

      return sn_internal_string_to_ulong (time_str);
    }

  fprintf (stderr,
           "libsn: No timestamp contained in the startup ID!\n");
  /* Unfortunately, all values are valid; let's just return -1 */
  return -1;
}

/**
 * sn_launchee_context_complete:
 * @context: an #SnLauncheeContext
 *
 * Called by the launchee when it is fully started up and the startup
 * sequence should end.
 * 
 **/
void
sn_launchee_context_complete (SnLauncheeContext *context)
{
  char *keys[2];
  char *vals[2];
  char *message;
  
  keys[0] = "ID";
  keys[1] = NULL;
  vals[0] = context->startup_id;
  vals[1] = NULL; 

  message = sn_internal_serialize_message ("remove",
                                           (const char**) keys,
                                           (const char**) vals);

  sn_internal_broadcast_xmessage (context->display,
                                  context->screen,
                                  "_NET_STARTUP_INFO",
                                  "_NET_STARTUP_INFO_BEGIN",
                                  message);

  sn_free (message);
}

/**
 * sn_launchee_context_setup_window:
 * @context: a #SnLauncheeContext
 * @xwindow: window to be set up
 * 
 * Sets up @xwindow, marking it as launched by the startup sequence
 * represented by @context. For example sets the _NET_STARTUP_ID
 * property. Only the group leader windows of an application MUST be
 * set up with this function, though if any other windows come from a
 * separate startup sequence, they can be set up separately.
 * 
 **/
void
sn_launchee_context_setup_window (SnLauncheeContext *context,
                                  Window             xwindow)
{
  sn_internal_set_utf8_string (context->display,
                               xwindow,
                               "_NET_STARTUP_ID",
                               context->startup_id);
}
