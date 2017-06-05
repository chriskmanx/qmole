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
#include "sn-common.h"
#include "sn-internals.h"


struct SnDisplay
{
  int refcount;
  enum SnDisplayType type;
  union
  {
      struct
      {
          Display *xdisplay;
          Screen **screens;
          SnDisplayErrorTrapPush push_trap_func;
          SnDisplayErrorTrapPop  pop_trap_func;
      } xlib;
  } x;
  int n_screens;
  SnList *xmessage_funcs;
  SnList *pending_messages;
};

/**
 * sn_display_new:
 * @xdisplay: an X window system display
 * @push_trap_func: function to push an X error trap
 * @pop_trap_func: function to pop an X error trap
 * 
 * Creates a new #SnDisplay object, containing
 * data that libsn associates with an X display.
 *
 * @push_trap_func should be a function that causes X errors to be
 * ignored until @pop_trap_func is called as many times as
 * @push_trap_func has been called. (Nested push/pop pairs must be
 * supported.) The outermost @pop_trap_func in a set of nested pairs
 * must call XSync() to ensure that all errors that will occur have in
 * fact occurred. These functions are used to avoid X errors due to
 * BadWindow and such.
 * 
 * Return value: the new #SnDisplay
 **/
SnDisplay*
sn_display_new (Display                *xdisplay,
                SnDisplayErrorTrapPush  push_trap_func,
                SnDisplayErrorTrapPop   pop_trap_func)
{
  SnDisplay *display;
  int i;
  
  display = sn_new0 (SnDisplay, 1);

  display->type = SN_DISPLAY_TYPE_XLIB;
  display->x.xlib.xdisplay = xdisplay;
  display->n_screens = ScreenCount (xdisplay);
  display->x.xlib.screens = sn_new (Screen*, display->n_screens);
  display->refcount = 1;

  display->x.xlib.push_trap_func = push_trap_func;
  display->x.xlib.pop_trap_func = pop_trap_func;

  for (i = 0; i < display->n_screens; ++i)
    display->x.xlib.screens[i] = ScreenOfDisplay (display->x.xlib.xdisplay, i);

  return display;
}


/**
 * sn_display_ref:
 * @display: an #SnDisplay
 * 
 * Increment the reference count for @display
 **/
void
sn_display_ref (SnDisplay *display)
{
  display->refcount += 1;
}

/**
 * sn_display_unref:
 * @display: an #SnDisplay
 * 
 * Decrement the reference count for @display, freeing
 * display if the reference count reaches zero.
 **/
void
sn_display_unref (SnDisplay *display)
{
  display->refcount -= 1;
  if (display->refcount == 0)
    {
      if (display->xmessage_funcs)
        sn_list_free (display->xmessage_funcs);
      if (display->pending_messages)
        sn_list_free (display->pending_messages);
      switch (display->type)
      {
       case SN_DISPLAY_TYPE_XLIB:
        sn_free (display->x.xlib.screens);
        break;
      }
      sn_free (display);
    }
}

/**
 * sn_display_get_x_display:
 * @display: an #SnDisplay
 * 
 * 
 * 
 * Return value: X display for this #SnDisplay
 **/
Display*
sn_display_get_x_display (SnDisplay *display)
{
  if(display->type == SN_DISPLAY_TYPE_XLIB)
      return display->x.xlib.xdisplay;
  return NULL;
}


/**
 * sn_internal_display_get_id:
 * @display: an #SnDisplay
 *
 *
 *
 * Return value: X display id.
 **/
void *
sn_internal_display_get_id (SnDisplay *display)
{
  switch (display->type)
  {
   case SN_DISPLAY_TYPE_XLIB:
    return display->x.xlib.xdisplay;
  }
  return NULL;
}

/**
 * sn_internal_display_get_x_screen:
 * @display: an #SnDisplay
 * @number: screen number to get
 *
 * Gets a screen by number; if the screen number
 * does not exist, returns %NULL.
 *
 * Return value: X screen or %NULL
 **/
Screen*
sn_internal_display_get_x_screen (SnDisplay *display,
                                  int        number)
{
  if (display->type != SN_DISPLAY_TYPE_XLIB || number < 0 || number >= display->n_screens)
    return NULL;
  else
    return display->x.xlib.screens[number];
}

/**
 * sn_internal_display_get_root_window:
 * @display: an #SnDisplay
 * @number: screen number to get root window from
 *
 * Gets a root window; if the screen number
 * does not exist, returns %NULL.
 *
 * Return value: X root window or %NULL
 **/
Window
sn_internal_display_get_root_window (SnDisplay *display,
                                     int       number)
{
    if (number >= 0 && number < display->n_screens)
      switch (display->type)
      {
       case SN_DISPLAY_TYPE_XLIB:
        return RootWindow (display->x.xlib.xdisplay, number);
      }
    return None;
}

/**
 * sn_internal_display_get_type
 * @display: an #SnDisplay
 *
 *
 *
 * Return value: X connection type
 */
enum SnDisplayType
sn_internal_display_get_type (SnDisplay *display)
{
    return display->type;
}


/**
 * sn_internal_display_get_screen_number:
 * @display an #SnDisplay
 *
 *
 *
 * Return value: The number of screen for this #SnDisplay
 **/
int
sn_internal_display_get_screen_number (SnDisplay *display)
{
    return display->n_screens;
}

/**
 * sn_display_process_event:
 * @display: a display
 * @xevent: X event
 * 
 * libsn should be given a chance to see all X events by passing them
 * to this function. If the event was a property notify or client
 * message related to the launch feedback protocol, the
 * sn_display_process_event() returns true. Calling
 * sn_display_process_event() is not currently required for launchees,
 * only launchers and launch feedback displayers. The function returns
 * false for mapping, unmapping, window destruction, and selection
 * events even if they were involved in launch feedback.
 * 
 * Return value: true if the event was a property notify or client message involved in launch feedback
 **/
sn_bool_t
sn_display_process_event (SnDisplay *display,
                          XEvent    *xevent)
{
  sn_bool_t retval;

  retval = FALSE;

  if (sn_internal_monitor_process_event (display))
    retval = TRUE;

  if (sn_internal_xmessage_process_event (display, xevent))
    retval = TRUE;
  
  return retval;
}


/**
 * sn_display_error_trap_push:
 * @display: a display
 *
 *  Calls the push_trap_func from sn_display_new() if non-NULL.
 **/
void
sn_display_error_trap_push (SnDisplay *display)
{
  switch (display->type)
  {
   case SN_DISPLAY_TYPE_XLIB:
    if (display->x.xlib.push_trap_func)
      (* display->x.xlib.push_trap_func) (display, display->x.xlib.xdisplay);
    break;
  }
}

/**
 * sn_display_error_trap_pop:
 * @display: a display
 *
 *  Calls the pop_trap_func from sn_display_new() if non-NULL.
 **/
void
sn_display_error_trap_pop  (SnDisplay *display)
{
  switch (display->type)
  {
   case SN_DISPLAY_TYPE_XLIB:
    if (display->x.xlib.pop_trap_func)
      (* display->x.xlib.pop_trap_func) (display, display->x.xlib.xdisplay);
    break;
  }
}

void
sn_internal_display_get_xmessage_data (SnDisplay              *display,
                                       SnList                **funcs,
                                       SnList                **pending)
{
  if (display->xmessage_funcs == NULL)
    display->xmessage_funcs = sn_list_new ();

  if (display->pending_messages == NULL)
    display->pending_messages = sn_list_new ();
  
  if (funcs)
    *funcs = display->xmessage_funcs;
  if (pending)
    *pending = display->pending_messages;
}

