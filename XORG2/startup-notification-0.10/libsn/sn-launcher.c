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
#include "sn-launcher.h"
#include "sn-internals.h"
#include "sn-xmessages.h"

#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <assert.h>

static SnList* context_list = NULL;

struct SnLauncherContext
{
  int                 refcount;
  SnDisplay          *display;
  int                 screen;
  char               *startup_id;
  char               *name;
  char               *description;
  int                 workspace;
  char               *wmclass;
  char               *binary_name;
  char               *icon_name;
  struct timeval      initiation_time;
  unsigned int        completed : 1;
  unsigned int        canceled : 1;
};

/**
 * sn_launcher_context_new:
 * @display: an #SnDisplay
 * @screen: X screen number
 * @event_func: function to be called when a notable event occurs
 * @event_func_data: data to pass to @event_func
 * @free_data_func: function to be called on @event_func_data when freeing the context
 *
 * Creates a new launcher context, to be used by the program that is
 * starting a startup sequence. For example a file manager might
 * create a launcher context when the user double-clicks on
 * an application icon.
 *
 * Return value: a new #SnLauncherContext
 **/
SnLauncherContext*
sn_launcher_context_new (SnDisplay           *display,
                         int                  screen)
{
  SnLauncherContext *context;

  if (context_list == NULL)
    context_list = sn_list_new ();

  context = sn_new0 (SnLauncherContext, 1);

  context->refcount = 1;
  context->display = display;
  sn_display_ref (context->display);

  context->workspace = -1;
  
  sn_list_prepend (context_list, context);

  return context;
}

/**
 * sn_launcher_context_ref:
 * @context: a #SnLauncherContext
 *
 * Increments the reference count of @context
 **/
void
sn_launcher_context_ref (SnLauncherContext *context)
{
  context->refcount += 1;
}

/**
 * sn_launcher_context_unref:
 * @context: a #SnLauncherContext
 *
 * Decrements the reference count of @context and frees the
 * context if the count reaches zero.
 **/
void
sn_launcher_context_unref (SnLauncherContext *context)
{
  context->refcount -= 1;

  if (context->refcount == 0)
    {
      sn_list_remove (context_list, context);

      sn_free (context->startup_id);      

      sn_free (context->name);
      sn_free (context->description);
      sn_free (context->wmclass);
      sn_free (context->binary_name);
      sn_free (context->icon_name);

      sn_display_unref (context->display);
      sn_free (context);
    }
}


static char*
strip_slashes (const char *src)
{
  char *canonicalized_name;
  char *s;
  
  canonicalized_name = sn_internal_strdup (src);

  s = canonicalized_name;
  while (*s)
    {
      if (*s == '/')
        *s = '|';
      ++s;
    }

  return canonicalized_name;
}

/**
 * sn_launcher_context_initiate:
 * @context: an #SnLaunchContext
 * @launcher_name: name of the launcher app, suitable for debug output
 * @launchee_name: name of the launchee app, suitable for debug output
 * @timestamp: X timestamp of event causing the launch
 *
 * Initiates a startup sequence. All the properties of the launch (such
 * as type, geometry, description) should be set up prior to
 * initiating the sequence.
 **/
void
sn_launcher_context_initiate (SnLauncherContext *context,
                              const char        *launcher_name,
                              const char        *launchee_name,
                              Time               timestamp)
{
  static int sequence_number = 0;
  static sn_bool_t have_hostname = FALSE;
  static char hostbuf[257];
  char *s;
  int len;
  char *canonicalized_launcher;
  char *canonicalized_launchee;
  int i;
#define MAX_PROPS 12
  char *names[MAX_PROPS];
  char *values[MAX_PROPS];  
  char *message;
  char workspacebuf[257];
  char screenbuf[257];
  
  if (context->startup_id != NULL)
    {
      fprintf (stderr, "%s called twice for the same SnLaunchContext\n",
               __func__);
      return;
    }

  if (!have_hostname)
    {
      if (gethostname (hostbuf, sizeof (hostbuf)-1) == 0)
        have_hostname = TRUE;
      else
        hostbuf[0] = '\0';
    }

  canonicalized_launcher = strip_slashes (launcher_name);
  canonicalized_launchee = strip_slashes (launchee_name);
  
  /* man I wish we could use g_strdup_printf */
  len = strlen (launcher_name) + strlen (launchee_name) +
    256; /* 256 is longer than a couple %d and some slashes */
  
  s = sn_malloc (len + 3);
  snprintf (s, len, "%s/%s/%d-%d-%s_TIME%lu",
            canonicalized_launcher, canonicalized_launchee,
            (int) getpid (), (int) sequence_number, hostbuf,
            (unsigned long) timestamp);
  ++sequence_number;
  
  sn_free (canonicalized_launcher);
  sn_free (canonicalized_launchee);

  context->startup_id = s;
  
  i = 0;

  names[i] = "ID";
  values[i] = context->startup_id;
  ++i;  

  names[i] = "SCREEN";
  sprintf (screenbuf, "%d", context->screen);
  values[i] = screenbuf;
  ++i;
  
  if (context->name != NULL)
    {
      names[i] = "NAME";
      values[i] = context->name;
      ++i;
    }

  if (context->description != NULL)
    {
      names[i] = "DESCRIPTION";
      values[i] = context->description;
      ++i;
    }

  if (context->workspace >= 0)
    {
      names[i] = "DESKTOP";
      sprintf (workspacebuf, "%d", context->workspace);
      values[i] = workspacebuf;
      ++i;
    }

  if (context->wmclass != NULL)
    {
      names[i] = "WMCLASS";
      values[i] = context->wmclass;
      ++i;
    }

  if (context->binary_name != NULL)
    {
      names[i] = "BIN";
      values[i] = context->binary_name;
      ++i;
    }

  if (context->icon_name != NULL)
    {
      names[i] = "ICON";
      values[i] = context->icon_name;
      ++i;
    }
  
  assert (i < MAX_PROPS);
  
  names[i] = NULL;
  values[i] = NULL;

  gettimeofday (&context->initiation_time, NULL);
  
  message = sn_internal_serialize_message ("new",
                                           (const char**) names,
                                           (const char**) values);
  
  sn_internal_broadcast_xmessage (context->display,
                                  context->screen,
                                  "_NET_STARTUP_INFO",
                                  "_NET_STARTUP_INFO_BEGIN",
                                  message);

  sn_free (message);
}

void
sn_launcher_context_complete (SnLauncherContext *context)
{
  char *keys[2];
  char *vals[2];
  char *message;

  if (context->startup_id == NULL)
    {
      fprintf (stderr, "%s called for an SnLauncherContext that hasn't been initiated\n",
               "sn_launcher_context_complete");
      return;
    }
  
  keys[0] = "ID";
  keys[1] = NULL;
  vals[0] = context->startup_id;
  vals[1] = NULL; 

  message = sn_internal_serialize_message ("remove",
                                           (const char**) keys,
                                           (const char **) vals);

  sn_internal_broadcast_xmessage (context->display,
                                  context->screen,
                                  "_NET_STARTUP_INFO",
                                  "_NET_STARTUP_INFO_BEGIN",
                                  message);

  sn_free (message);
}

const char*
sn_launcher_context_get_startup_id (SnLauncherContext *context)
{

  return context->startup_id;
}

sn_bool_t
sn_launcher_context_get_initiated (SnLauncherContext *context)
{
  return context->startup_id != NULL;
}

/**
 * sn_launcher_context_setup_child_process:
 * @context: an #SnLauncherContext
 *
 * This function should be called after forking, but before exec(), in
 * the child process being launched. It sets up the environment variables
 * telling the child process about the launch ID.
 * This function will leak the strings passed to putenv() so should
 * only be used prior to an exec().
 *
 **/
void
sn_launcher_context_setup_child_process (SnLauncherContext *context)
{
  char *startup_id;

  if (context->startup_id == NULL)
    {
      fprintf (stderr, "%s called for an SnLauncherContext that hasn't been initiated\n",
               "sn_launcher_context_setup_child_process");
      return;
    }

  /* Man we need glib here */

  startup_id = sn_malloc (strlen (context->startup_id) + strlen ("DESKTOP_STARTUP_ID") + 3);
  strcpy (startup_id, "DESKTOP_STARTUP_ID=");
  strcat (startup_id, context->startup_id);

  putenv (startup_id);

  /* Can't free strings passed to putenv */
}

/* FIXME use something pluggable, not fprintf */
#define WARN_ALREADY_INITIATED(context) do { if ((context)->startup_id != NULL) {               \
      fprintf (stderr, "%s called for an SnLauncherContext that has already been initiated\n", \
               __func__);                                                                  \
      return;                                                                                  \
} } while (0)

void
sn_launcher_context_set_name (SnLauncherContext *context,
                              const char        *name)
{
  WARN_ALREADY_INITIATED (context);

  sn_free (context->name);
  context->name = sn_internal_strdup (name);
}

void
sn_launcher_context_set_description (SnLauncherContext *context,
                                     const char        *description)  
{
  WARN_ALREADY_INITIATED (context);

  sn_free (context->description);
  context->description = sn_internal_strdup (description);
}

void
sn_launcher_context_set_workspace (SnLauncherContext *context,
                                   int                workspace)
{
  WARN_ALREADY_INITIATED (context);

  context->workspace = workspace;
}

void
sn_launcher_context_set_wmclass (SnLauncherContext *context,
                                 const char        *klass)
{
  WARN_ALREADY_INITIATED (context);

  sn_free (context->wmclass);
  context->wmclass = sn_internal_strdup (klass);
}

void
sn_launcher_context_set_binary_name (SnLauncherContext *context,
                                     const char        *name)
{
  WARN_ALREADY_INITIATED (context);

  sn_free (context->binary_name);
  context->binary_name = sn_internal_strdup (name);
}

void
sn_launcher_context_set_icon_name (SnLauncherContext *context,
                                   const char        *name)
{
  WARN_ALREADY_INITIATED (context);

  sn_free (context->icon_name);
  context->icon_name = sn_internal_strdup (name);
}

void
sn_launcher_context_set_extra_property (SnLauncherContext *context,
                                        const char        *name,
                                        const char        *value)
{
  WARN_ALREADY_INITIATED (context);

  /* FIXME implement this */
}

void
sn_launcher_context_get_initiated_time (SnLauncherContext *context,
                                        long              *tv_sec,
                                        long              *tv_usec)
{
  if (context->startup_id == NULL)
    {
      fprintf (stderr, "%s called for an SnLauncherContext that hasn't been initiated\n",
               "sn_launcher_context_get_initiated_time");
      return;
    }

  if (tv_sec)
    *tv_sec = context->initiation_time.tv_sec;
  if (tv_usec)
    *tv_usec = context->initiation_time.tv_usec;
}

void
sn_launcher_context_get_last_active_time (SnLauncherContext *context,
                                          long              *tv_sec,
                                          long              *tv_usec)
{
  if (context->startup_id == NULL)
    {
      fprintf (stderr, "%s called for an SnLauncherContext that hasn't been initiated\n",
               "sn_launcher_context_get_initiated_time");
      return;
    }

  /* for now, maybe forever, the same as initiated time. */
  
  if (tv_sec)
    *tv_sec = context->initiation_time.tv_sec;
  if (tv_usec)
    *tv_usec = context->initiation_time.tv_usec;
}
