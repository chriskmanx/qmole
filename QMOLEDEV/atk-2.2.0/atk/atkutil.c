/* ATK -  Accessibility Toolkit
 * Copyright 2001 Sun Microsystems Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "atkutil.h"
#include "atkmarshal.c"
#include "config.h"

static void atk_util_class_init (AtkUtilClass *klass);

static AtkObject *previous_focus_object = NULL;

GType
atk_util_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo typeInfo =
      {
        sizeof (AtkUtilClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) atk_util_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        sizeof (AtkUtil),
        0,
        (GInstanceInitFunc) NULL,
      } ;
      type = g_type_register_static (G_TYPE_OBJECT, "AtkUtil", &typeInfo, 0) ;
    }
  return type;
}

static void
atk_util_class_init (AtkUtilClass *klass)
{
  klass->add_global_event_listener = NULL;
  klass->remove_global_event_listener = NULL;
  klass->get_root = NULL;
  klass->get_toolkit_name = NULL;
  klass->get_toolkit_version = NULL;
}

/*
 * This file supports the addition and removal of multiple focus handlers
 * as long as they are all called in the same thread.
 */
static AtkEventListenerInit  focus_tracker_init = (AtkEventListenerInit) NULL;

static gboolean init_done = FALSE;

/*
 * Array of FocusTracker structs
 */
static GArray *trackers = NULL;
static guint  index = 0;

typedef struct _FocusTracker FocusTracker;

struct _FocusTracker {
  guint index;
  AtkEventListener func;
};

/**
 * atk_focus_tracker_init:
 * @init: Function to be called for focus tracker initialization
 *
 * Specifies the function to be called for focus tracker initialization.
 * This function should be called by an implementation of the
 * ATK interface if any specific work needs to be done to enable
 * focus tracking.
 **/
void
atk_focus_tracker_init (AtkEventListenerInit    init)
{
  if (!focus_tracker_init)
    focus_tracker_init = init;
}

/**
 * atk_add_focus_tracker:
 * @focus_tracker: Function to be added to the list of functions to be called
 * when an object receives focus.
 *
 * Adds the specified function to the list of functions to be called
 * when an object receives focus.
 *
 * Returns: added focus tracker id, or 0 on failure.
 **/
guint
atk_add_focus_tracker (AtkEventListener   focus_tracker)
{
  g_return_val_if_fail (focus_tracker, 0);

  if (!init_done)
  {
    if (focus_tracker_init)
    {
      focus_tracker_init ();
    }
    trackers = g_array_sized_new (FALSE, TRUE, sizeof (FocusTracker), 0);
    init_done = TRUE;
  }
  if (init_done)
  {
    FocusTracker item;

    item.index = ++index;
    item.func = focus_tracker;
    trackers = g_array_append_val (trackers, item);
    return index;
  }
  else
  {
    return 0;
  }
}

/**
 * atk_remove_focus_tracker:
 * @tracker_id: the id of the focus tracker to remove
 *
 * Removes the specified focus tracker from the list of functions
 * to be called when any object receives focus.
 **/
void
atk_remove_focus_tracker (guint            tracker_id)
{
  FocusTracker *item;
  guint i;

  if (trackers == NULL)
    return;

  if (tracker_id == 0)
    return;

  for (i = 0; i < trackers->len; i++)
  {
    item = &g_array_index (trackers, FocusTracker, i);
    if (item->index == tracker_id)
    {
      trackers = g_array_remove_index (trackers, i);
      break;
    }
  }
}

/**
 * atk_focus_tracker_notify:
 * @object: an #AtkObject
 *
 * Cause the focus tracker functions which have been specified to be
 * executed for the object.
 **/
void
atk_focus_tracker_notify (AtkObject       *object)
{
  FocusTracker *item;
  guint i;

  if (trackers == NULL)
    return;

  if (object == previous_focus_object)
    return;
  else
    {
      if (previous_focus_object)
        g_object_unref (previous_focus_object);

      previous_focus_object = object;
      if (object)
        {
          g_object_ref (object);

          for (i = 0; i < trackers->len; i++)
            {
              item = &g_array_index (trackers, FocusTracker, i);
              g_return_if_fail (item != NULL);
              item->func (object);
            }
        }
    
    }
}

/**
 * atk_add_global_event_listener:
 * @listener: the listener to notify
 * @event_type: the type of event for which notification is requested
 *
 * Adds the specified function to the list of functions to be called
 * when an ATK event of type event_type occurs.
 *
 * The format of event_type is the following:
 *  "ATK:<atk_type>:<atk_event>
 *
 * Where "ATK" works as the namespace, <atk_interface> is the name of
 * the ATK type (interface or object) and <atk_event> is the name of
 * the signal defined on that interface.
 *
 * For example:
 *   ATK:AtkObject:state-change
 *   ATK:AtkText:text-selection-changed
 *
 * Returns: added event listener id, or 0 on failure.
 **/
guint
atk_add_global_event_listener (GSignalEmissionHook listener,
			       const gchar        *event_type)
{
  guint retval;
  AtkUtilClass *klass = g_type_class_ref (ATK_TYPE_UTIL);

  if (klass->add_global_event_listener)
    {
      retval = klass->add_global_event_listener (listener, event_type);
    }
  else
    {
      retval = 0;
    }
  g_type_class_unref (klass);

  return retval;
}

/**
 * atk_remove_global_event_listener:
 * @listener_id: the id of the event listener to remove
 *
 * Removes the specified event listener
 **/
void
atk_remove_global_event_listener (guint listener_id)
{
  AtkUtilClass *klass = g_type_class_peek (ATK_TYPE_UTIL);

  if (klass && klass->remove_global_event_listener)
    klass->remove_global_event_listener (listener_id);
}

/**
 * atk_add_key_event_listener:
 * @listener: the listener to notify
 * @data: a #gpointer that points to a block of data that should be sent to the registered listeners,
 *        along with the event notification, when it occurs.  
 *
 * Adds the specified function to the list of functions to be called
 *        when a key event occurs.  The @data element will be passed to the
 *        #AtkKeySnoopFunc (@listener) as the @func_data param, on notification.
 *
 * Returns: added event listener id, or 0 on failure.
 **/
guint
atk_add_key_event_listener (AtkKeySnoopFunc listener, gpointer data)
{
  guint retval;
  AtkUtilClass *klass = g_type_class_peek (ATK_TYPE_UTIL);
  if (klass && klass->add_key_event_listener)
    {
      retval = klass->add_key_event_listener (listener, data);
    }
  else
    {
      retval = 0;
    }

  return retval;
}

/**
 * atk_remove_key_event_listener:
 * @listener_id: the id of the event listener to remove
 *
 * Removes the specified event listener
 **/
void
atk_remove_key_event_listener (guint listener_id)
{
  AtkUtilClass *klass = g_type_class_peek (ATK_TYPE_UTIL);

  if (klass->remove_key_event_listener)
    klass->remove_key_event_listener (listener_id);
}

/**
 * atk_get_root:
 *
 * Gets the root accessible container for the current application.
 *
 * Returns: (transfer none): the root accessible container for the current
 * application
 **/
AtkObject*
atk_get_root (void)
{
  AtkUtilClass *klass = g_type_class_ref (ATK_TYPE_UTIL);
  AtkObject    *retval;
  if (klass->get_root)
    {
      retval = klass->get_root ();
    }
  else
    {
      retval = NULL;
    }
  g_type_class_unref (klass);

  return retval;
}

/**
 * atk_get_focus_object:
 *
 * Gets the currently focused object.
 * 
 * Since: 1.6
 *
 * Returns: (transfer none): the currently focused object for the current
 * application
 **/
AtkObject*
atk_get_focus_object (void)
{
  return previous_focus_object;
}

/**
 * atk_get_toolkit_name:
 *
 * Gets name string for the GUI toolkit implementing ATK for this application.
 *
 * Returns: name string for the GUI toolkit implementing ATK for this application
 **/
const gchar*
atk_get_toolkit_name (void)
{
  const gchar *retval;
  AtkUtilClass *klass = g_type_class_ref (ATK_TYPE_UTIL);
  if (klass->get_toolkit_name)
    {
      retval = klass->get_toolkit_name ();
    }
  else
    {
      retval = NULL;
    }
  g_type_class_unref (klass);

  return retval;
}

/**
 * atk_get_toolkit_version:
 *
 * Gets version string for the GUI toolkit implementing ATK for this application.
 *
 * Returns: version string for the GUI toolkit implementing ATK for this application
 **/
const gchar*
atk_get_toolkit_version (void)
{
  const gchar *retval;
  AtkUtilClass *klass = g_type_class_ref (ATK_TYPE_UTIL);
  if (klass->get_toolkit_version)
    {
      retval = klass->get_toolkit_version ();
    }
  else
    {
      retval = NULL;
    }
  g_type_class_unref (klass);

  return retval;
}

/**
 * atk_get_version:
 *
 * Gets the current version for ATK.
 *
 * Returns: version string for ATK
 *
 * Since: 1.20
 */
const gchar *
atk_get_version (void)
{
  return VERSION;
}

