/*
 * WindowsWM extension is based on AppleWM extension
 * Authors:	Kensuke Matsuzaki
 */
/**************************************************************************

Copyright (c) 2002 Apple Computer, Inc.
All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sub license, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice (including the
next paragraph) shall be included in all copies or substantial portions
of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
IN NO EVENT SHALL PRECISION INSIGHT AND/OR ITS SUPPLIERS BE LIABLE FOR
ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

**************************************************************************/

/* THIS IS NOT AN X CONSORTIUM STANDARD */

#define NEED_EVENTS
#define NEED_REPLIES
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <X11/Xlibint.h>
#include <X11/extensions/windowswmstr.h>
#include <X11/extensions/Xext.h>
#include <X11/extensions/extutil.h>
#include <stdio.h>

static XExtensionInfo _windowswm_info_data;
static XExtensionInfo *windowswm_info = &_windowswm_info_data;
static char *windowswm_extension_name = WINDOWSWMNAME;

#define WindowsWMCheckExtension(dpy,i,val) \
  XextCheckExtension (dpy, i, windowswm_extension_name, val)

/*****************************************************************************
 *                                                                           *
 *			   private utility routines                          *
 *                                                                           *
 *****************************************************************************/

static int close_display (Display *dpy, XExtCodes *extCodes);
static Bool wire_to_event ();
static Status event_to_wire ();

static /* const */ XExtensionHooks windowswm_extension_hooks = {
  NULL,				/* create_gc */
  NULL,				/* copy_gc */
  NULL,				/* flush_gc */
  NULL,				/* free_gc */
  NULL,				/* create_font */
  NULL,				/* free_font */
  close_display,		/* close_display */
  wire_to_event,		/* wire_to_event */
  event_to_wire,		/* event_to_wire */
  NULL,				/* error */
  NULL,				/* error_string */
};

static XEXT_GENERATE_FIND_DISPLAY (find_display, windowswm_info,
                                   windowswm_extension_name,
                                   &windowswm_extension_hooks,
                                   WindowsWMNumberEvents, NULL);
     
static XEXT_GENERATE_CLOSE_DISPLAY (close_display, windowswm_info);

static Bool
wire_to_event (Display *dpy, XEvent  *re, xEvent  *event)
{
  XExtDisplayInfo *info = find_display (dpy);
  XWindowsWMNotifyEvent *se;
  xWindowsWMNotifyEvent *sevent;
  
  WindowsWMCheckExtension (dpy, info, False);
  
  switch ((event->u.u.type & 0x7f) - info->codes->first_event)
    {
    case WindowsWMControllerNotify:
    case WindowsWMActivationNotify:
      se = (XWindowsWMNotifyEvent *) re;
      sevent = (xWindowsWMNotifyEvent *) event;
      se->type = sevent->type & 0x7f;
      se->serial = _XSetLastRequestRead(dpy,(xGenericReply *) event);
      se->send_event = (sevent->type & 0x80) != 0;
      se->display = dpy;
      se->window = sevent->window;
      se->time = sevent->time;
      se->kind = sevent->kind;
      se->arg = sevent->arg;
      se->x = sevent->x;
      se->y = sevent->y;
      se->w = sevent->w;
      se->h = sevent->h;
      return True;
    }
  return False;
}

static Status
event_to_wire (Display *dpy, XEvent  *re, xEvent  *event)
{
  XExtDisplayInfo *info = find_display (dpy);
  XWindowsWMNotifyEvent *se;
  xWindowsWMNotifyEvent *sevent;
  
  WindowsWMCheckExtension (dpy, info, False);
  
  switch ((re->type & 0x7f) - info->codes->first_event)
    {
    case WindowsWMControllerNotify:
    case WindowsWMActivationNotify:
      se = (XWindowsWMNotifyEvent *) re;
      sevent = (xWindowsWMNotifyEvent *) event;
      sevent->type = se->type | (se->send_event ? 0x80 : 0);
      sevent->sequenceNumber = se->serial & 0xffff;
      sevent->window = se->window;
      sevent->kind = se->kind;
      sevent->arg = se->arg;
      sevent->time = se->time;
      sevent->x = se->x;
      sevent->y = se->y;
      sevent->w = se->w;
      sevent->h = se->h;
      return 1;
  }
  return 0;
}

/*****************************************************************************
 *                                                                           *
 *		    public Windows-WM Extension routines                     *
 *                                                                           *
 *****************************************************************************/

#if 0
#include <stdio.h>
#define TRACE(msg)  fprintf(stderr, "WindowsWM%s\n", msg);
#else
#define TRACE(msg)
#endif


Bool
XWindowsWMQueryExtension (Display *dpy,
			  int *event_basep, int *error_basep)
{
  XExtDisplayInfo *info = find_display (dpy);
  
  TRACE("QueryExtension...");
  if (XextHasExtension(info))
    {
      *event_basep = info->codes->first_event;
      *error_basep = info->codes->first_error;
      TRACE("QueryExtension... return True");
      return True;
    }
  else
    {
      TRACE("QueryExtension... return False");
      return False;
    }
}

Bool
XWindowsWMQueryVersion (Display* dpy, int* majorVersion,
			int* minorVersion, int* patchVersion)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMQueryVersionReply rep;
  xWindowsWMQueryVersionReq *req;
  
  TRACE("QueryVersion...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMQueryVersion, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMQueryVersion;
  if (!_XReply(dpy, (xReply *)&rep, 0, xFalse))
    {
      UnlockDisplay(dpy);
      SyncHandle();
      TRACE("QueryVersion... return False");
      return False;
    }
  *majorVersion = rep.majorVersion;
  *minorVersion = rep.minorVersion;
  *patchVersion = rep.patchVersion;
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("QueryVersion... return True");
  return True;
}

Bool
XWindowsWMDisableUpdate (Display* dpy, int screen)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMDisableUpdateReq *req;

  TRACE("DisableUpdate...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMDisableUpdate, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMDisableUpdate;
  req->screen = screen;
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("DisableUpdate... return True");
  return True;
}

Bool
XWindowsWMReenableUpdate (Display* dpy, int screen)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMReenableUpdateReq *req;
  
  TRACE("ReenableUpdate...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMReenableUpdate, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMReenableUpdate;
  req->screen = screen;
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("ReenableUpdate... return True");
    return True;
}

Bool
XWindowsWMSelectInput (Display* dpy, unsigned long mask)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMSelectInputReq *req;
  
  TRACE("SelectInput...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMSelectInput, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMSelectInput;
  req->mask = mask;
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("SetlectInput... return True");
  return True;
}


Bool
XWindowsWMSetFrontProcess (Display* dpy)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMSetFrontProcessReq *req;
  
  TRACE("SetFrontProcess...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMSetFrontProcess, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMSetFrontProcess;
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("SetFrontProcess... return True");
  return True;
}

Bool
XWindowsWMFrameGetRect (Display* dpy, unsigned int frame_style,
			unsigned int frame_style_ex, unsigned int frame_rect,
			short ix, short iy, short iw, short ih,
			short *rx, short *ry, short *rw, short *rh)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMFrameGetRectReply rep;
  xWindowsWMFrameGetRectReq *req;
  
  TRACE("FrameGetRect...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMFrameGetRect, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMFrameGetRect;
  req->frame_style = frame_style;
  req->frame_style_ex = frame_style_ex;
  req->frame_rect = frame_rect;
  req->ix = ix;
  req->iy = iy;
  req->iw = iw;
  req->ih = ih;
  rep.x = rep.y = rep.w = rep.h = 0;
  if (!_XReply(dpy, (xReply *)&rep, 0, xFalse))
    {
      UnlockDisplay(dpy);
      SyncHandle();
      TRACE("FrameGetRect... return False");
      return False;
    }
  *rx = rep.x; *ry = rep.y;
  *rw = rep.w; *rh = rep.h;
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("FrameGetRect... return True");
  return True;
}

Bool
XWindowsWMFrameDraw (Display* dpy, int screen, Window window,
		     unsigned int frame_style, unsigned int frame_style_ex,
		     short ix, short iy, short iw, short ih)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMFrameDrawReq *req;
  
  TRACE("FrameDraw...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMFrameDraw, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMFrameDraw;
  req->screen = screen;
  req->window = window;
  req->frame_style = frame_style;
  req->frame_style_ex = frame_style_ex;
  req->ix = ix;
  req->iy = iy;
  req->iw = iw;
  req->ih = ih;
  
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("FrameDraw... return True");
  return True;
}

Bool
XWindowsWMFrameSetTitle (Display* dpy, int screen, Window window,
			 unsigned int title_length, const char *title_bytes)
{
  XExtDisplayInfo *info = find_display (dpy);
  xWindowsWMFrameSetTitleReq *req;
  
  TRACE("FrameSetTitle...");
  WindowsWMCheckExtension (dpy, info, False);
  
  LockDisplay(dpy);
  GetReq(WindowsWMFrameSetTitle, req);
  req->reqType = info->codes->major_opcode;
  req->wmReqType = X_WindowsWMFrameSetTitle;
  req->screen = screen;
  req->window = window;
  req->title_length = title_length;
  
  req->length += (title_length + 3)>>2;
  Data (dpy, title_bytes, title_length);
  
  UnlockDisplay(dpy);
  SyncHandle();
  TRACE("FrameSetTitle... return True");
  return True;
}
