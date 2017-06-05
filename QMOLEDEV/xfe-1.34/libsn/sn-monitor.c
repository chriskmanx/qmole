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
#include "sn-monitor.h"
#include "sn-internals.h"
#include "sn-xmessages.h"
#include <sys/time.h>

struct SnMonitorContext
{
  int refcount;
  SnDisplay *display;
  int screen;
  SnMonitorEventFunc event_func;
  void *event_func_data;
  SnFreeFunc free_data_func;
  /* a context doesn't get events for sequences
   * started prior to context creation
   */
  int creation_serial;  
};

struct SnMonitorEvent
{
  int refcount;
  SnMonitorEventType type;
  SnMonitorContext *context;
  SnStartupSequence *sequence;
};

struct SnStartupSequence
{
  int refcount;
  
  SnDisplay *display;
  int screen;

  char *id;
  
  char *name;
  char *description;

  char *wmclass;

  int workspace;
  Time timestamp;

  char *binary_name;
  char *icon_name;  

  unsigned int completed : 1;
  unsigned int canceled : 1;
  unsigned int timestamp_set : 1;
  
  int creation_serial;

  struct timeval initiation_time;
};

static SnList *context_list = NULL;
static SnList *sequence_list = NULL;
static int next_sequence_serial = 0;

static void xmessage_func (SnDisplay       *display,
                           const char      *message_type,
                           const char      *message,
                           void            *user_data);

/**
 * sn_monitor_context_new:
 * @display: an #SnDisplay
 * @screen: an X screen number
 * @event_func: function to call when an event is received
 * @event_func_data: extra data to pass to @event_func
 * @free_data_func: function to free @event_func_data when the context is freed
 *
 * Creates a new context for monitoring startup sequences.  Normally
 * only launch feedback indicator applications such as the window
 * manager or task manager would use #SnMonitorContext.
 * #SnLauncherContext and #SnLauncheeContext are more often used by
 * applications.
 *
 * To detect startup sequence initiations, PropertyChangeMask must be
 * selected on all root windows for a display.  libsn does not do this
 * for you because it's pretty likely to mess something up. So you
 * have to do it yourself in programs that use #SnMonitorContext.
 * 
 * Return value: a new #SnMonitorContext
 **/
SnMonitorContext*
sn_monitor_context_new (SnDisplay           *display,
                        int                  screen,
                        SnMonitorEventFunc   event_func,
                        void                *event_func_data,
                        SnFreeFunc           free_data_func)
{
  SnMonitorContext *context;
  
  context = sn_new0 (SnMonitorContext, 1);

  context->refcount = 1;
  context->event_func = event_func;
  context->event_func_data = event_func_data;
  context->free_data_func = free_data_func;

  context->display = display;
  sn_display_ref (context->display);
  context->screen = screen;
  
  if (context_list == NULL)
    context_list = sn_list_new ();

  if (sn_list_empty (context_list))
    sn_internal_add_xmessage_func (display,
                                   screen,
                                   "_NET_STARTUP_INFO",
                                   "_NET_STARTUP_INFO_BEGIN",
                                   xmessage_func,
                                   NULL, NULL);
    
  sn_list_prepend (context_list, context);

  /* We get events for serials >= creation_serial */
  context->creation_serial = next_sequence_serial;
  
  return context;
}

/**
 * sn_monitor_context_ref:
 * @context: an #SnMonitorContext
 *
 * Increments the reference count on @context.
 * 
 **/
void
sn_monitor_context_ref (SnMonitorContext *context)
{
  context->refcount += 1;
}

/**
 * sn_monitor_context_unref:
 * @context: an #SnMonitorContext
 * 
 * Decrements the reference count on @context and frees the
 * context if the count reaches 0.
 **/
void
sn_monitor_context_unref (SnMonitorContext *context)
{
  context->refcount -= 1;

  if (context->refcount == 0)
    {
      sn_list_remove (context_list, context);

      if (sn_list_empty (context_list))
        sn_internal_remove_xmessage_func (context->display,
                                          context->screen,
                                          "_NET_STARTUP_INFO",
                                          xmessage_func,
                                          NULL);
      
      if (context->free_data_func)
        (* context->free_data_func) (context->event_func_data);
      
      sn_display_unref (context->display);
      sn_free (context);
    }
}

void
sn_monitor_event_ref (SnMonitorEvent *event)
{
  event->refcount += 1;
}

void
sn_monitor_event_unref (SnMonitorEvent *event)
{
  event->refcount -= 1;

  if (event->refcount == 0)
    {
      if (event->context)
        sn_monitor_context_unref (event->context);
      if (event->sequence)
        sn_startup_sequence_unref (event->sequence);
      sn_free (event);
    }
}

SnMonitorEvent*
sn_monitor_event_copy (SnMonitorEvent *event)
{
  SnMonitorEvent *copy;

  copy = sn_new0 (SnMonitorEvent, 1);

  copy->refcount = 1;

  copy->type = event->type;
  copy->context = event->context;
  if (copy->context)
    sn_monitor_context_ref (copy->context);
  copy->sequence = event->sequence;
  if (copy->sequence)
    sn_startup_sequence_ref (copy->sequence);
  
  return copy;
}

SnMonitorEventType
sn_monitor_event_get_type (SnMonitorEvent *event)
{
  return event->type;
}

SnStartupSequence*
sn_monitor_event_get_startup_sequence (SnMonitorEvent *event)
{
  return event->sequence;
}

SnMonitorContext*
sn_monitor_event_get_context (SnMonitorEvent *event)
{
  return event->context;
}

void
sn_startup_sequence_ref (SnStartupSequence *sequence)
{
  sequence->refcount += 1;
}

void
sn_startup_sequence_unref (SnStartupSequence *sequence)
{
  sequence->refcount -= 1;

  if (sequence->refcount == 0)
    {      
      sn_free (sequence->id);

      sn_free (sequence->name);
      sn_free (sequence->description);
      sn_free (sequence->wmclass);
      sn_free (sequence->binary_name);
      sn_free (sequence->icon_name);
      
      sn_display_unref (sequence->display);
      sn_free (sequence);
    }
}

const char*
sn_startup_sequence_get_id (SnStartupSequence *sequence)
{
  return sequence->id;
}

sn_bool_t
sn_startup_sequence_get_completed (SnStartupSequence *sequence)
{
  return sequence->completed;
}

const char*
sn_startup_sequence_get_name (SnStartupSequence *sequence)
{
  return sequence->name;
}

const char*
sn_startup_sequence_get_description (SnStartupSequence *sequence)
{
  return sequence->description;
}

int
sn_startup_sequence_get_workspace (SnStartupSequence *sequence)
{
  return sequence->workspace;
}

Time
sn_startup_sequence_get_timestamp (SnStartupSequence *sequence)
{
  if (!sequence->timestamp_set)
    {
      fprintf (stderr,
               "libsn: Buggy startup-notification launcher!  No timestamp!\n");
      /* Unfortunately, all values are valid; let's just return -1 */
      return -1;
    }
  else
    return sequence->timestamp;
}

const char*
sn_startup_sequence_get_wmclass (SnStartupSequence *sequence)
{
  return sequence->wmclass;
}

const char*
sn_startup_sequence_get_binary_name (SnStartupSequence *sequence)
{
  return sequence->binary_name;
}

const char*
sn_startup_sequence_get_icon_name (SnStartupSequence *sequence)
{
  return sequence->icon_name;
}

int
sn_startup_sequence_get_screen (SnStartupSequence *sequence)
{
  return sequence->screen;
}

/**
 * sn_startup_sequence_get_initiated_time:
 * @sequence: an #SnStartupSequence
 * @tv_sec: seconds as in struct timeval
 * @tv_usec: microseconds as struct timeval
 *
 * When a startup sequence is first monitored, libstartup-notification
 * calls gettimeofday() and records the time, this function
 * returns that recorded time.
 * 
 **/
void
sn_startup_sequence_get_initiated_time (SnStartupSequence *sequence,
                                        long              *tv_sec,
                                        long              *tv_usec)
{
  if (tv_sec)
    *tv_sec = sequence->initiation_time.tv_sec;
  if (tv_usec)
    *tv_usec = sequence->initiation_time.tv_usec;
}

/**
 * sn_startup_sequence_get_last_active_time:
 * @sequence: an #SnStartupSequence
 * @tv_sec: seconds as in struct timeval
 * @tv_usec: microseconds as in struct timeval
 *
 * Returns the last time we had evidence the startup was active.
 * This function should be used to decide whether a sequence
 * has timed out.
 * 
 **/
void
sn_startup_sequence_get_last_active_time (SnStartupSequence *sequence,
                                          long              *tv_sec,
                                          long              *tv_usec)
{
  /* for now the same as get_initiated_time */
  if (tv_sec)
    *tv_sec = sequence->initiation_time.tv_sec;
  if (tv_usec)
    *tv_usec = sequence->initiation_time.tv_usec;
}

void
sn_startup_sequence_complete (SnStartupSequence *sequence)
{
  char *keys[2];
  char *vals[2];
  char *message;

  if (sequence->id == NULL)
    return;

  if (sequence->screen < 0)
    return;
  
  keys[0] = "ID";
  keys[1] = NULL;
  vals[0] = sequence->id;
  vals[1] = NULL; 

  message = sn_internal_serialize_message ("remove",
                                           (const char**) keys,
                                           (const char **) vals);

  sn_internal_broadcast_xmessage (sequence->display,
                                  sequence->screen,
                                  "_NET_STARTUP_INFO",
                                  "_NET_STARTUP_INFO_BEGIN",
                                  message);

  sn_free (message);

}

static SnStartupSequence*
sn_startup_sequence_new (SnDisplay *display)
{
  SnStartupSequence *sequence;
  
  sequence = sn_new0 (SnStartupSequence, 1);

  sequence->refcount = 1;

  sequence->creation_serial = next_sequence_serial;
  ++next_sequence_serial;
  
  sequence->id = NULL;
  sequence->display = display;
  sn_display_ref (display);

  sequence->screen = -1; /* not set */
  sequence->workspace = -1; /* not set */
  sequence->timestamp = 0;
  sequence->timestamp_set = FALSE;

  sequence->initiation_time.tv_sec = 0;
  sequence->initiation_time.tv_usec = 0;
  gettimeofday (&sequence->initiation_time, NULL);
  
  return sequence;
}

typedef struct
{
  SnMonitorEvent *base_event;
  SnList *events;
} CreateContextEventsData;

static sn_bool_t
create_context_events_foreach (void *value,
                               void *data)
{
  /* Make a list of events holding a ref to the context they'll go to,
   * for reentrancy robustness
   */
  SnMonitorContext *context = value;
  CreateContextEventsData *ced = data;

  /* Don't send events for startup sequences initiated before the
   * context was created
   */
  if (ced->base_event->sequence->creation_serial >=
      context->creation_serial)
    {
      SnMonitorEvent *copy;
      
      copy = sn_monitor_event_copy (ced->base_event);
      copy->context = context;
      sn_monitor_context_ref (copy->context);
      
      sn_list_prepend (ced->events, copy);
    }
  
  return TRUE;
}

static sn_bool_t
dispatch_event_foreach (void *value,
                        void *data)
{
  SnMonitorEvent *event = value;

  /* Dispatch and free events */
  
  if (event->context->event_func)
    (* event->context->event_func) (event,
                                    event->context->event_func_data);

  sn_monitor_event_unref (event);

  return TRUE;
}

static sn_bool_t
filter_event (SnMonitorEvent *event)
{
  sn_bool_t retval;

  retval = FALSE;

  /* Filter out duplicate events and update flags */
  switch (event->type)
    {
    case SN_MONITOR_EVENT_CANCELED:
      if (event->sequence->canceled)
        {
          retval = TRUE;
        }
      else
        {
          event->sequence->canceled = TRUE;
        }
      break;
    case SN_MONITOR_EVENT_COMPLETED:
      if (event->sequence->completed)
        {
          retval = TRUE;
        }
      else
        {
          event->sequence->completed = TRUE;
        }
      break;

    default:
      break;
    }
  
  return retval;
}

static SnStartupSequence*
add_sequence (SnDisplay *display)
{
  SnStartupSequence *sequence;
  
  sequence =
    sn_startup_sequence_new (display);
  
  if (sequence)
    {
      sn_startup_sequence_ref (sequence); /* ref held by sequence list */
      if (sequence_list == NULL)
        sequence_list = sn_list_new ();
      sn_list_prepend (sequence_list, sequence);
    }

  return sequence;
}

static void
remove_sequence (SnStartupSequence *sequence)
{
  sn_list_remove (sequence_list, sequence);
  sn_startup_sequence_unref (sequence);
}

static void
dispatch_monitor_event (SnDisplay      *display,
                        SnMonitorEvent *event)
{
  if (event->type == SN_MONITOR_EVENT_INITIATED)
    {
      if (event->sequence == NULL)
        event->sequence = add_sequence (display);
    }  
      
  if (event->sequence != NULL &&
      !filter_event (event))
    {
      CreateContextEventsData cced;
          
      cced.base_event = event;
      cced.events = sn_list_new ();
          
      sn_list_foreach (context_list, create_context_events_foreach,
                       &cced);
          
      sn_list_foreach (cced.events, dispatch_event_foreach, NULL);

      /* values in the events list freed on dispatch */
      sn_list_free (cced.events);

      /* remove from sequence list */
      if (event->type == SN_MONITOR_EVENT_COMPLETED)
        remove_sequence (event->sequence);
    }
}

sn_bool_t
sn_internal_monitor_process_event (SnDisplay *display)
{
  sn_bool_t retval;

  if (context_list == NULL ||
      sn_list_empty (context_list))
    return FALSE; /* no one cares */

  retval = FALSE;
  
  return retval;
}

typedef struct
{
  SnDisplay *display;
  const char *id;
  SnStartupSequence *found;
} FindSequenceByIdData;

static sn_bool_t
find_sequence_by_id_foreach (void *value,
                             void *data)
{
  SnStartupSequence *sequence = value;
  FindSequenceByIdData *fsd = data;
  
  if (strcmp (sequence->id, fsd->id) == 0 &&
      sn_internal_display_get_id (sequence->display) ==
      sn_internal_display_get_id (fsd->display))
    {
      fsd->found = sequence;
      return FALSE;
    }

  return TRUE;
}

static SnStartupSequence*
find_sequence_for_id (SnDisplay      *display,
                      const char     *id)
{
  FindSequenceByIdData fsd;
  
  if (sequence_list == NULL)
    return NULL;
  
  fsd.display = display;
  fsd.id = id;
  fsd.found = NULL;
  
  sn_list_foreach (sequence_list, find_sequence_by_id_foreach, &fsd);

  return fsd.found;
}

static sn_bool_t
do_xmessage_event_foreach (void *value,
                           void *data)
{
  SnMonitorEvent *event = value;
  SnDisplay *display = data;
  
  dispatch_monitor_event (display, event);

  return TRUE;
}

static sn_bool_t
unref_event_foreach (void *value,
                     void *data)
{
  sn_monitor_event_unref (value);
  return TRUE;
}

static void
xmessage_func (SnDisplay  *display,
               const char *message_type,
               const char *message,
               void       *user_data)
{
  /* assert (strcmp (message_type, KDE_STARTUP_INFO_ATOM) == 0); */
  char *prefix;
  char **names;
  char **values;
  int i;
  const char *launch_id;
  SnStartupSequence *sequence;
  SnList *events;
  
  prefix = NULL;
  names = NULL;
  values = NULL;
  if (!sn_internal_unserialize_message (message, &prefix, &names, &values))
    return;
  
  launch_id = NULL;
  i = 0;
  while (names[i])
    {
      if (strcmp (names[i], "ID") == 0)
        {
          launch_id = values[i];
          break;
        }
      ++i;
    }

  events = sn_list_new ();
  
  if (launch_id == NULL)
    goto out;
  
  sequence = find_sequence_for_id (display, launch_id);

  if (strcmp (prefix, "new") == 0)
    {
      if (sequence == NULL)
        {
          SnMonitorEvent *event;
          char *time_str;

          sequence = add_sequence (display);
          if (sequence == NULL)
            goto out;
          
          sequence->id = sn_internal_strdup (launch_id);

          /* Current spec says timestamp is part of the startup id; so we need
           * to get the timestamp here if the launcher is using the current spec
           */
          time_str = sn_internal_find_last_occurrence (sequence->id, "_TIME");
          if (time_str != NULL)
            {
              /* Skip past the "_TIME" part */
              time_str += 5;

              sequence->timestamp = sn_internal_string_to_ulong (time_str);
              sequence->timestamp_set = TRUE;
            }
          
          event = sn_new (SnMonitorEvent, 1);
          
          event->refcount = 1;
          event->type = SN_MONITOR_EVENT_INITIATED;
          event->context = NULL;
          event->sequence = sequence; /* ref from add_sequence goes here */
          
          sn_list_append (events, event);
        }
    }

  if (sequence == NULL)
    goto out;
  
  if (strcmp (prefix, "change") == 0 ||
      strcmp (prefix, "new") == 0)
    {
      sn_bool_t changed = FALSE;

      i = 0;
      while (names[i])
        {
          if (strcmp (names[i], "BIN") == 0)
            {
              if (sequence->binary_name == NULL)
                {
                  sequence->binary_name = sn_internal_strdup (values[i]);
                  changed = TRUE;
                }              
            }
          else if (strcmp (names[i], "NAME") == 0)
            {
              if (sequence->name == NULL)
                {
                  sequence->name = sn_internal_strdup (values[i]);
                  changed = TRUE;
                }
            }
          else if (strcmp (names[i], "SCREEN") == 0)
            {
              if (sequence->screen < 0)
                {
                  int n;
                  n = atoi (values[i]);
                  if (n >= 0 && n < sn_internal_display_get_screen_number (sequence->display))
                    {
                      sequence->screen = n;
                      changed = TRUE;
                    }
                }
            }
          else if (strcmp (names[i], "DESCRIPTION") == 0)
            {
              if (sequence->description == NULL)
                {
                  sequence->description = sn_internal_strdup (values[i]);
                  changed = TRUE;
                }
            }          
          else if (strcmp (names[i], "ICON") == 0)
            {
              if (sequence->icon_name == NULL)
                {
                  sequence->icon_name = sn_internal_strdup (values[i]);
                  changed = TRUE;
                }
            }
          else if (strcmp (names[i], "DESKTOP") == 0)
            {
              int workspace;
              
              workspace = sn_internal_string_to_ulong (values[i]);

              sequence->workspace = workspace;
              changed = TRUE;
            }
          else if (strcmp (names[i], "TIMESTAMP") == 0 && 
                   !sequence->timestamp_set)
            {
              /* Old version of the spec says that the timestamp was
               * sent as part of a TIMESTAMP message.  We try to
               * handle that to enable backwards compatibility with
               * older launchers.
               */
              Time timestamp;

              timestamp = sn_internal_string_to_ulong (values[i]);

              sequence->timestamp = timestamp;
              sequence->timestamp_set = TRUE;
              changed = TRUE;
            }
          else if (strcmp (names[i], "WMCLASS") == 0)
            {
              if (sequence->wmclass == NULL)
                {
                  sequence->wmclass = sn_internal_strdup (values[i]);
                  changed = TRUE;
                }
            }
          
          ++i;
        }

      if (strcmp (prefix, "new") == 0)
        {
          if (sequence->screen < 0)
            {
              SnMonitorEvent *event;
              
              event = sn_new (SnMonitorEvent, 1);
              
              event->refcount = 1;
              event->type = SN_MONITOR_EVENT_COMPLETED;
              event->context = NULL;
              event->sequence = sequence;
              sn_startup_sequence_ref (sequence);
              
              sn_list_append (events, event);

              fprintf (stderr,
                       "Ending startup notification for %s (%s) because SCREEN "
                       "field was not provided; this is a bug in the launcher "
                       "application\n",
                       sequence->name ? sequence->name : "???",
                       sequence->binary_name ? sequence->binary_name : "???");
            }
        }
      else if (changed)
        {
          SnMonitorEvent *event;
          
          event = sn_new (SnMonitorEvent, 1);
          
          event->refcount = 1;
          event->type = SN_MONITOR_EVENT_CHANGED;
          event->context = NULL;
          event->sequence = sequence;
          sn_startup_sequence_ref (sequence);
          
          sn_list_append (events, event);
        }
    }
  else if (strcmp (prefix, "remove") == 0)
    {
      SnMonitorEvent *event;
      
      event = sn_new (SnMonitorEvent, 1);
      
      event->refcount = 1;
      event->type = SN_MONITOR_EVENT_COMPLETED;
      event->context = NULL;
      event->sequence = sequence;
      sn_startup_sequence_ref (sequence);
      
      sn_list_append (events, event);
    }

  sn_list_foreach (events,
                   do_xmessage_event_foreach,
                   display); 
  
 out:
  if (events != NULL)
    {
      sn_list_foreach (events, unref_event_foreach, NULL);
      sn_list_free (events);
    }
  
  sn_free (prefix);
  sn_internal_strfreev (names);
  sn_internal_strfreev (values);
}
