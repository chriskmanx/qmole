/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/GrabShell.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/GrabShell.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";


#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/GrabShellP.h>
#include <Xm/MwmUtil.h>
#include <Xm/TransltnsP.h>
#include <Xm/MenuUtilP.h>
#include <XmI/PixConvI.h>
#include <XmI/MessagesI.h>
#include <XmI/PopupUtil.h>

#include <XmI/DebugUtil.h>


/*
  MODULE: GrabShell.c
  DESCRIPTION:
  Contains the routines that compose the GrabShell widget.

  A grab shell can have multiple children.  The grab shell does not
  perform any special layouts on its children.  It leaves child placement
  up to its children.  The grab shell will remain visible until focus is lost.
  At which time it will pop itself down.

  The grab shell does not have any window manager decorations.  

  The grab shell determines its width and height from the first child only.

  The grab shell moves the first child to x,y = 2,2.  This is probably due to
  the shadow thickness which has a default value of 2.

  Warning, this widget does not do anything with the extension record yet.
  END:
*/

#define VALID(w) (w && XtIsManaged(w))
#define Unused(x) (void)(x)
#define superclass (&vendorShellClassRec)
#define Offset(field) XtOffsetOf(XmGrabShellRec, grab_shell.field)

/* Forward declarations */
static void BtnDown(Widget,XEvent *,String *,Cardinal *);
static void BtnUp(Widget,XEvent *,String *,Cardinal *);
static void ChangeManaged(Widget w);
static void ClassPartInitialize(WidgetClass widget_class);
static void CreateBottomShadowGC(Widget w);
static void CreateTopShadowGC(Widget w);
static void Destroy(Widget w);
static void DoLayout(Widget w);
static void DrawBorder(Widget w, XEvent *xev, Region region);
static void ForceExpose(Widget w);
static Widget GetFirstManagedChild(Widget w);
static XtGeometryResult GeometryManager(Widget w, 
   XtWidgetGeometry *request, 
   XtWidgetGeometry *reply);
static int IgnoreError(Display *d, XErrorEvent *e);
static void Initialize(Widget request, 
   Widget c_new,
   ArgList args,
   Cardinal *num_args);
static void MapNotifyHandler(Widget w, 
   XtPointer cdata, 
   XEvent *xev, 
   Boolean *pb);
static void Popdown(Widget,XEvent *,String *,Cardinal *);
static void Resize(Widget w);
static Boolean SetValues(Widget current, 
   Widget request, 
   Widget c_new, 
   ArgList args,
   Cardinal *num_args);
static void _XmAllowEvents(Widget w, int event_mode, Time _time);
static void _XmGrabShellFocus(Widget w, int operation, Time _time);
static void _XmGrabShellGrab(Widget w, Time _time);
static void _XmGrabShellUngrab(Widget w, Time _time);

#if 0

typedef int (*XErrorHandler) (      /* WARNING, this type not in Xlib spec */
#if NeedFunctionPrototypes
    Display*            /* display */,
    XErrorEvent*        /* error_event */
#endif
);

#endif

enum {
  XmGRAB_SHELL_FOCUS_SAVE=0,
  XmGRAB_SHELL_FOCUS_RESTORE,
  XmGRAB_SHELL_FOCUS_SET
};

/* Resources for the GrabShell class */
static XtResource resources[] = {

   {
     XmNbottomShadowColor,
     XmCBottomShadowColor,
     XmRPixel,
     sizeof(Pixel),
     Offset(bottom_shadow_color),
     XmRCallProc,
     (XtPointer)_XmBottomShadowColorDefault
   },

   {
     XmNbottomShadowPixmap,
     XmCBottomShadowPixmap,
     XmRBottomShadowPixmap,
     sizeof(Pixmap),
     Offset(bottom_shadow_pixmap),
     XmRImmediate,
     (XtPointer)XmUNSPECIFIED_PIXMAP
   },

   {
     XmNgrabStyle,
     XmCGrabStyle,
     XmRInt,
     sizeof(int),
     Offset(grab_style),
     XmRImmediate,
     (XtPointer)GrabModeAsync 
   },

   { 
     XmNoverrideRedirect,
     XmCOverrideRedirect,
     XmRBoolean,
     sizeof(Boolean),
     XtOffsetOf(XmGrabShellRec, shell.override_redirect),
     XmRImmediate,
     (XtPointer)True
   },

   {
     XmNownerEvents,
     XmCOwnerEvents,
     XmRBoolean,
     sizeof(Boolean),
     Offset(owner_events),
     XmRImmediate,
     (XtPointer)False
   },

   {
     XtNsaveUnder,
     XtCSaveUnder,
     XtRBoolean,
     sizeof(Boolean),
     XtOffsetOf(XmGrabShellRec, shell.save_under),
     XmRImmediate,
     (XtPointer)True
   },

   {
     XmNshadowThickness,
     XmCShadowThickness,
     XmRHorizontalDimension,
     sizeof(Dimension),
     Offset(shadow_thickness),
     XmRImmediate,
     (XtPointer)2
   },

   {
     XmNtopShadowColor,
     XmCTopShadowColor,
     XmRPixel,
     sizeof(Pixel),
     Offset(top_shadow_color),
     XmRCallProc,
     (XtPointer)_XmTopShadowColorDefault
   },

   {
     XmNtopShadowPixmap,
     XmCTopShadowPixmap,
     XmRTopShadowPixmap,
     sizeof(Pixmap),
     Offset(top_shadow_pixmap),
     XmRCallProc,
     (XtPointer)_XmTopShadowPixmapDefault
   }

};

/* Action table */
static XtActionsRec actionsList[] = {
   {"GrabShellBtnDown", (XtActionProc) BtnDown},
   {"GrabShellBtnUp", (XtActionProc) BtnUp},
   {"GrabShellPopdown", (XtActionProc) Popdown} 
};

/* ========================================================================== */
/* Class record */

XmGrabShellClassRec xmGrabShellClassRec = {
   /* Core class part */
   {
      (WidgetClass) &vendorShellClassRec,     /* superclass */
      "XmGrabShell",                          /* class_name */
      sizeof(XmGrabShellRec),                 /* widget_size */
      NULL,                                   /* class_initialize */
      ClassPartInitialize,                    /* class_part_initialize */
      FALSE,                                  /* class_inited */
      Initialize,                             /* initialize */
      NULL,                                   /* initialize_hook */
      XtInheritRealize,                       /* realize */
      actionsList,                            /* actions */
      XtNumber(actionsList),                  /* num_actions */
      resources,                              /* resources */
      XtNumber(resources),                    /* num_resources */
      NULLQUARK,                              /* xrm_class */
      TRUE,                                   /* compress_motion */
      XtExposeCompressMaximal,                /* compress_exposure */
      TRUE,                                   /* compress_enterleave */
      FALSE,                                  /* visible_interest */
      Destroy,                                /* destroy */
      Resize, /* XtInheritResize,  */         /* resize */
      DrawBorder, /* XtInheritExpose  */      /* expose */
      SetValues,                              /* set_values */
      NULL,                                   /* set_values_hook */
      XtInheritSetValuesAlmost,               /* set_values_almost */
      NULL,                                   /* get_values_hook */
      NULL,                                   /* accept_focus */
      XtVersion,                              /* version */
      NULL,                                   /* callback offsets */
      _XmGrabShell_translations,              /* tm_table */
      NULL,                                   /* query_geometry */
      NULL,                                   /* display_accelerator */
      NULL                                    /* extension */ 
   },
   /* Composite class part */
   {
#if 0
      XtInheritGeometryManager, 	/* geometry manager */
#else
      GeometryManager,        /* geometry manager */
#endif
      ChangeManaged,          /* ChangeManaged */
      XtInheritInsertChild,   /* insert_child */ 
      XtInheritDeleteChild,   /* delete_child */ 
      NULL                    /* extension */ 
   },
   /* Shell class part */
   {
      NULL                    /* extension */ 
   },
   /* WMShell class part */
   {
      NULL                    /* extension */ 
   },
   /* VendorShell class part */
   {
      NULL                    /* extension */ 
   },
   /* XmGrabShell class part */
   {
      NULL                    /* extension */ 
   }
};

WidgetClass xmGrabShellWidgetClass = (WidgetClass) &xmGrabShellClassRec;

/*
  FUNCTION: BtnDown
  SYNOPSIS: static void BtnDown(Widget, XEvent *, String *, Cardinal *)
  DESCRIPTION:
  This function handles button down actions. 
  END:
*/
static void 
BtnDown(
   Widget w,
   XEvent * xev,
   String * params,
   Cardinal * num_params)
{
   int event_mode;
   XButtonEvent *bev;
   Position rootx_return, rooty_return;

   DEBUGOUT(_LtDebug(__FILE__, w, "%s:BtnDown\n",
   	__FILE__));

   /* Check to see if mapped yet */
   if(GS_Mapped(w) == False)
   {
      return;
   }

   bev = (XButtonEvent *) xev;

   XtTranslateCoords(w, 0, 0, &rootx_return, &rooty_return);

   /* if within grab shell window then return */
   if ((bev->x_root >= rootx_return 
        && bev->x_root <= rootx_return + XtWidth(w))
       && (bev->y_root >= rooty_return 
           && bev->y_root <= rooty_return + XtHeight(w)))
   {
      DEBUGOUT(_LtDebug(__FILE__, w, "BtnDown: In grab shell window.\n"));

      event_mode = (GS_GrabStyle(w) == GrabModeSync) ? SyncBoth : AsyncBoth;
      _XmAllowEvents(w, event_mode, CurrentTime);

      return;
   }

   Popdown(w,xev,params,num_params);

   Unused(xev);
   Unused(params);
   Unused(num_params);
}

/*
  FUNCTION: BtnUp
  SYNOPSIS: static void BtnUp(Widget, XEvent *, String *, Cardinal *)
  DESCRIPTION:
  This function handles button up actions. 
  END:
*/
static void 
BtnUp(
   Widget w,
   XEvent * xev,
   String * params,
   Cardinal * num_params)
{
   int event_mode;

   DEBUGOUT(_LtDebug(__FILE__, w, "BtnUp\n"));

   /* Check to see if mapped yet */
   if(GS_Mapped(w) == False)
   {
      return;
   }

   event_mode = (GS_GrabStyle(w) == GrabModeSync) ? SyncBoth : AsyncBoth;
   _XmAllowEvents(w, event_mode, CurrentTime);

   Unused(xev);
   Unused(params);
   Unused(num_params);
}

/*
  FUNCTION: ChangeManaged
  SYNOPSIS: static void ChangeManaged(Widget w)
  DESCRIPTION:
  This function calls for a layout regardless if the grabshell is unmanaged.
  END:
*/
static void 
ChangeManaged(Widget w)
{
   Widget child;
   XtWidgetGeometry pref;
   XtGeometryResult res;
   Dimension adjustment;

   DEBUGOUT(_LtDebug(__FILE__, w,
                     "ChangeManaged: dimensions are %d %d\n",
                      XtWidth(w), XtHeight(w)));

   /* Shells only support one child. */
   child = GetFirstManagedChild(w); 

   if(!child)
   {
      /* This will happen if being realized and there are no managed child */
      DEBUGOUT(_LtDebug(__FILE__, w, 
                        "ChangeManaged: No managed child bye.\n"));
      return;
   } 
  
   /* Get the preferred size of the child */
   XtQueryGeometry(child, NULL, &pref);
   DEBUGOUT(_LtDebug(__FILE__, child,
                     "get preferred size of parent from child %s\n",
                     _LtDebugWidgetGeometry2String(&pref)));

   /* adjust for shadow thickness */
   adjustment = GS_ShadowThickness(w); 
   pref.width = pref.width == 0 ? 1 : pref.width + (2*adjustment);
   pref.height = pref.height == 0 ? 1 : pref.height + (2*adjustment);
   /* change the width and height */
   pref.request_mode = CWWidth | CWHeight;

   /* Make the geometry request */
   res = _XmMakeGeometryRequest(w, &pref);
   if (res == XtGeometryNo)
   {
       DEBUGOUT(_LtDebug(__FILE__, w,
                        "XtGeometryNo returned... THIS SHOULD NOT HAPPEN\n"));
   }

   /* Lay me out */
   DoLayout(w);
}

/*
  FUNCTION: ClassPartInitialize
  SYNOPSIS: static void ClassPartInitialize(WidgetClass widget_class)
  DESCRIPTION:
  This function sets the XmGRAB_SHELL_BIT in the extension record.
  END:
*/
static void
ClassPartInitialize(WidgetClass widget_class)
{
#if 0
   /* FIXTHIS: Need to do something here with the extension pointer */
   XmGrabShellWidgetClass nbclass = (XmGrabShellWidgetClass)widget_class;
   XmVendorShellExtObjectClass ext, *extptr;
   extptr = (XmVendorShellExtObjectClass *)_XmGetClassExtensionPtr(
                    (XmGenericClassExt *)&(nbclass->vendor_class.extension),
                                                                   NULLQUARK);

#endif

   _XmFastSubclassInit(widget_class, XmGRAB_SHELL_BIT);

}

/*
  FUNCTION: CreateBottomShadowGC
  SYNOPSIS: static void CreateBottomShadowGC(Widget w)
  DESCRIPTION:
  This function creates the bottom shadow GC for the widget.
  This is performed by calling _XmGetPixmapBasedGC.
  END:
*/
static void
CreateBottomShadowGC(Widget w)
{
   XGCValues values;
   XtGCMask mask;

   mask = GCLineWidth;
   values.line_width = 1;
   GS_BottomShadowGC(w) = _XmGetPixmapBasedGC(w,
      GS_BottomShadowPixmap(w),
      GS_BottomShadowColor(w),
      mask,
      &values);
}

/*
  FUNCTION: CreateTopShadowGC
  SYNOPSIS: static void CreateTopShadowGC(Widget w)
  DESCRIPTION:
  This function creates the top shadow GC for the widget.
  This is performed by calling _XmGetPixmapBasedGC.
  END:
*/
static void
CreateTopShadowGC(Widget w)
{
   XGCValues values;
   XtGCMask mask;

   /* get top shadow GC values */
   mask = GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
   values.line_width = 1;
   values.line_style = LineSolid;
   values.join_style = JoinMiter;
   values.cap_style = CapButt;

   GS_TopShadowGC(w) = _XmGetPixmapBasedGC(w,
      GS_TopShadowPixmap(w),
      GS_TopShadowColor(w),
      mask,
      &values);
}

/*
  FUNCTION: Destroy
  SYNOPSIS: static void Destroy(Widget w)
  DESCRIPTION:
  This function releases allocated memory done by the widgets instance. 
  END:
*/
static void
Destroy(Widget w)
{
   Unused(w);

   /* release GC's */
   XtReleaseGC(w,GS_TopShadowGC(w));
   XtReleaseGC(w,GS_BottomShadowGC(w));
}

/*
  FUNCTION: DoLayout
  SYNOPSIS: static void DoLayout(Widget w)
  DESCRIPTION:
  This function resizes the window according to its childs geometry.
  END:
*/
static void
DoLayout(Widget w)
{
   Widget child = NULL;
   Position child_x, child_y;
   Dimension child_width, child_height;
   Dimension adjustment;

   DEBUGOUT(_LtDebug(__FILE__, w, "DoLayout ...\n"));

   child = GetFirstManagedChild(w);

   if(child != NULL)
   {
      DEBUGOUT(_LtDebug(__FILE__, w,
                        "DoLayout: dimensions before change are %u %u\n",
                        XtWidth(w), XtHeight(w)));

      adjustment = GS_ShadowThickness(w); 

      /* change childs x,y */
      child_x = adjustment;
      child_y = adjustment;

      /* change child width,height */
      child_width = XtWidth(w) - 2*adjustment; 
      child_height = XtHeight(w) - 2*adjustment;

      /* configure child object */
      XmeConfigureObject(child,
                         child_x,
                         child_y,
                         child_width,
                         child_height,
                         XtBorderWidth(child));
   }
}

/*
  FUNCTION: DrawBorder
  SYNOPSIS: static void DrawBorder(Widget w)
  DESCRIPTION:
  This function draws the widget border.
  END:
*/
static void
DrawBorder(Widget w, XEvent *xev, Region region)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "DrawBorder ...\n"));

   XmeDrawShadows(XtDisplayOfObject(w),       /* display */
                  XtWindowOfObject(w),        /* window */
                  GS_TopShadowGC(w),          /* top gc */
                  GS_BottomShadowGC(w),       /* bottom gc */
                  0,                          /* position x */
                  0,                          /* position y */
                  XtWidth(w),                 /* width */
                  XtHeight(w),                /* height */
                  GS_ShadowThickness(w),      /* shadow thickness */
                  (unsigned int) XmSHADOW_OUT);       /* shadow type */

   Unused(w);
   Unused(xev);
   Unused(region);
}

/*
  FUNCTION: ForceExpose
  SYNOPSIS: static void ForceExpose(Widget w)
  DESCRIPTION:
  This function forces an expose on the widget.
  END:
*/
static void
ForceExpose(Widget w)
{
   Display *dpy;

   dpy = XtDisplayOfObject(w);

   XFlush(dpy);

   DrawBorder(w, NULL, NULL);
}

/*
  FUNCTION: GetFirstManagedChild
  SYNOPSIS: statis Widget GetFirstManageChild(Widget composite)
  DESCRIPTION:
  This function returns the first managed child widget or NULL.
  END:
*/ 
static Widget
GetFirstManagedChild(Widget w)
{
   Cardinal i;
   Widget child = NULL;

   /* 
      REMARK: 
      Using MGR_* macros does a cast to a structure we are not using.  
      However, the pointer ends up pointing to what we want since we
      are accessing the composite structure.  See the macros MGR_NumChildren
      and MGR_Children in MacrosI.h if you don't understand this.  
      Does anyone see a problem with this? 
      END:
   */

   for(i = 0; i < MGR_NumChildren(w); i++) 
   {
      if(XtIsManaged(MGR_Children(w)[i])) 
      {
         /*
            REMARK:
            Any subclass of the shell that manages more then one
            child is broken.
            END:
         */
         child = MGR_Children(w)[i]; /* first managed child */
         break;          
      }
   }
   return child;
}

/*
  FUNCTION: GeometryManager
  SYNOPSIS: static XtGeometryResult(Widget, XtWidgetGeometry *, XtWidgetGeometry *)
  DESCRIPTION:
  This function handles geometry requests by children.  It does this by calling
  _XmMakeGeometryRequest and then resetting the width and height.  It always
  returns XtGeometryYes.
  END:
*/
static XtGeometryResult 
GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
   XtGeometryResult res;
   XtWidgetGeometry wants;
   Widget me = XtParent(w);

   DEBUGOUT(_LtDebug(__FILE__, me,
                     "geometry_manager: request %s, allow_shell_resize %s\n",
                     _LtDebugWidgetGeometry2String(request),
                     ((XmGrabShellWidget)me)->shell.allow_shell_resize
                     ? "True"
                     : "False"));

   if (Shell_AllowResize(me) == FALSE && XtIsRealized(w))
   {
       /* No changes allowed after being realized. */
       return XtGeometryNo;
   }

   if (request->request_mode & (CWX | CWY))
   {
       /* Can not change my x or y */
       return XtGeometryNo;
   }

   DEBUGOUT(_LtDebug(__FILE__, me,
                     "geometry_manager: %s\n",
                     _LtDebugWidgetGeometry2String(request)));

   wants = *request;

   res = _XmMakeGeometryRequest(me, &wants);

   if (res == XtGeometryNo)
   {
       DEBUGOUT(_LtDebug(__FILE__, w,
                        "XtGeometryNo returned... THIS SHOULD NOT HAPPEN\n"));
   }
   *reply = wants;

   if (!(request->request_mode & XtCWQueryOnly))
   {
       /* if not a query then set the values */
       XtWidth(w) = wants.width;
       XtHeight(w) = wants.height;
       if (request->request_mode & CWBorderWidth)
       {
          XtX(w) = XtY(w) = -request->border_width;
       }
   }

   DEBUGOUT(_LtDebug(__FILE__, w,
                     "geometry_manager: size %dx%d => Yes\n",
                     reply->width, reply->height));

   return XtGeometryYes;
}

/*
  FUNCTION: IgnoreError
  SYNOPSIS: 
  DESCRIPTION: static int IgnoreError(Display *d, XErrorEvent *e)
  This function is used to ignore X errors.  Typically, this will occur
  when doing grabs.  I wonder if ignoring errors is bad idea.  Any one
  have any thoughts on the issue.
  END:
*/
static int 
IgnoreError(Display *d, XErrorEvent *e)
{

   DEBUGOUT(_LtDebug(__FILE__, NULL, "In IgnoreError\n"));
   DEBUGOUT(_LtDebug(__FILE__, NULL, 
                     "type: %d\n"
                     "display: %p\n"
                     "resourceid: %p\n"
                     "serial: %ul\n"
                     "error_code: %u\n"
                     "request_code: %u\n"
                     "minor_code: %u\n",
                     e->type,
                     e->display,
                     e->resourceid,
                     e->serial,
                     (unsigned int) e->error_code,
                     (unsigned int) e->request_code,
                     (unsigned int) e->minor_code));

   Unused(d);
   Unused(e);

   return 0;
}

/*
  FUNCTION: Initialize
  SYNOPSIS: static void Initialize(Widget,Widget,ArgList,Cardinal *)
  DESCRIPTION:
  This function initializes the instance data.  It calls routines to
  setup the top and bottom shadow GC.  Sets up an event handler by calling
  XtAddEventHandler for MapNotify events with the parameter mask 
  StructureNotifyMask.  This routine also sets the widgets width and height 
  to 1 if 0 (see macro FAKE_NONZERO in code).
  END:
*/
static void
Initialize(Widget request,
           Widget c_new,
           ArgList args,
           Cardinal *num_args)
{
   Unused(request);
   Unused(c_new);
   Unused(args);
   Unused(num_args);

   if (!XtIsSubclass(c_new, shellWidgetClass))
   {
      XmeWarning(c_new, _XmMsgGrabS_0000);
   }

   CreateBottomShadowGC(c_new);
   CreateTopShadowGC(c_new);

   /* setup map notify handler */
   XtAddEventHandler(c_new, 
                     /* MapNotify Events */
                     StructureNotifyMask, 
#if 0
                     SubstructureNotifyMask | StructureNotifyMask, 
#endif
                     False, 
                     (XtEventHandler) MapNotifyHandler, 
                     (XtPointer) NULL);

#if 0
#define FAKE_NONZERO
#ifdef  FAKE_NONZERO
   if (XtWidth(c_new) == 0 || XtHeight(c_new) == 0)
   {
      DEBUGOUT(_LtDebug(__FILE__, c_new,
                        "Initialize: dimensions %d %d changed to 1x1\n",
                        XtWidth(c_new), XtHeight(c_new)));

      XtWidth(c_new) = XtHeight(c_new) = 1;
   }
#endif
#endif
}

/*
  FUNCTION: MapNotifyHandler
  SYNOPSIS: static void MapNotifyHandler(Widget w, XtPointer cdata, XEvent *xev, Boolean *pb);
  DESCRIPTION:
  This function handles MapNotify events.  If the grab shell is in the process
  of popping up onto the screen then this routine will perform a grab.  It also
  will save the current focus state and then set the focus on the grab shell.
  END:
*/
static void 
MapNotifyHandler(Widget w, XtPointer cdata, XEvent *xev, Boolean *pb)
{
   int event_mode;
   XErrorHandler old_handler;

   *pb = TRUE;

   /* Do we have a map notify event */
   if (xev->type != MapNotify)
   {
      return;
   }

   /* Have we already done what we wanted to do */
   if (GS_Mapped(w) != False)
   {
      return;
   }

   DEBUGOUT(_LtDebug(__FILE__, w,
                     "MapNotifyHandler ...\n"));

   /* Set internal flag to on.  Shut it off in popdown */
   GS_Mapped(w) = True;

   /* Save post time. */
   GS_PostTime(w) = XtLastTimestampProcessed(XtDisplay(w));

   ForceExpose(w);

#if 1
#if 1
   _XmGrabShellGrab(w, GS_PostTime(w));
#else
   _XmGrabShellGrab(w, CurrentTime);
#endif
#endif

   /* Only needed if using GrabModeSync..I think */
   event_mode = (GS_GrabStyle(w) == GrabModeSync) ? SyncBoth : AsyncBoth;

#if 1
   _XmAllowEvents(w, event_mode, GS_PostTime(w));
#else
   _XmAllowEvents(w, event_mode, CurrentTime);
#endif

   /* The time parameter is ignored on a XmGRAB_SHELL_FOCUS_SAVE */
   _XmGrabShellFocus(w, XmGRAB_SHELL_FOCUS_SAVE, GS_PostTime(w));

   /* Setup ignore X error routine */
   old_handler = XSetErrorHandler(IgnoreError);

   _XmGrabShellFocus(w, XmGRAB_SHELL_FOCUS_SET, CurrentTime);

   /* Should I sync here? */
   XSync(XtDisplay(w),False);

   /* Set error handler back to the old error handler */
   XSetErrorHandler(old_handler);
}

/*
  FUNCTION: Popdown
  SYNOPSIS: static void Popdown(Widget, XEvent *, String *, Cardinal *)
  DESCRIPTION:
  This function handles popdown actions.  It resets the focus and then
  replays the pointer event.  May need to put a check in here to not
  replay the last pointer event if it wasn't a pointer event.  
  END:
*/
static void 
Popdown(
   Widget w,
   XEvent * xev,
   String * params,
   Cardinal * num_params)
{
   XErrorHandler old_handler;

   DEBUGOUT(_LtDebug(__FILE__, w, "Popdown\n"));

   /* Check to see if mapped yet */
   if(GS_Mapped(w) == False)
   {
      return;
   }

   /* Get unpost time stamp */
   GS_UnpostTime(w) = XtLastTimestampProcessed(XtDisplay(w));

   _XmAllowEvents(w, ReplayPointer, GS_UnpostTime(w));

   _XmGrabShellUngrab(w, GS_UnpostTime(w));

   /* popdown the shell */
   _XmPopdown(w);

   /* Unlock map notify handler */
   GS_Mapped(w) = False;

   /* Setup ignore X error routine */
   old_handler = XSetErrorHandler(IgnoreError);

   /* restore focus */
   _XmGrabShellFocus(w, XmGRAB_SHELL_FOCUS_RESTORE, GS_UnpostTime(w));

   /* Should I sync here? */
   XSync(XtDisplay(w),False);

   /* Set error handler back to the old error handler */
   XSetErrorHandler(old_handler);

   Unused(xev);
   Unused(params);
   Unused(num_params);
}

/* 
  FUNCTION: Resize
  SYNOPSIS: static void Resize(Widget w)
  DESCRIPTION:
  This routine handles resizing by calling DoLayout and then the core classes
  resize method.
  END:
*/
static void 
Resize(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "%s:Resize %ix%i\n",
        __FILE__,
        XtWidth(w), XtHeight(w)));

   /* FIX ME: Should resize regardless if realized.  However,
              this sort of fixes a geometry problem with the scrolled
              window.  Remove the if realized check when scrolled
              window geometry is working correctly.  */ 
   if(XtIsRealized(w))
   {  /* widgets must be resized */
      DoLayout(w);
   }
}

/* 
  FUNCTION: SetValues
  SYNOPSIS: static Boolean SetValues(Widget,Widget,Widget,ArgList,Cardinal *)
  DESCRIPTION:
  This routine handles the setting of widget resources.
  END:
*/
static Boolean 
SetValues(Widget old, 
          Widget request, 
          Widget c_new, 
          ArgList args,
          Cardinal *num_args)
{
   XmGrabShellWidget ow = (XmGrabShellWidget) old;
   XmGrabShellWidget nw = (XmGrabShellWidget) c_new;
   Boolean need_refresh = False;

   /* FIXTHIS: SetValues is not implemented yet. */

   DEBUGOUT(_LtDebug(__FILE__, old, "SetValues ...\n"));

   Unused(request);
   Unused(args);
   Unused(num_args);
   Unused(ow);
   Unused(nw);

   return need_refresh;
}

/*
  FUNCTION: _XmAllowEvents
  SYNOPSIS: static void _XmAllowEvents(Widget w, int event_mode, Time _time)
  DESCRIPTION:
  This function releases any queued events since the last grab by calling
  XAllowEvents.
  END:
*/
static void
_XmAllowEvents(Widget w, int event_mode, Time _time)
{
   XAllowEvents(XtDisplay(w), event_mode, _time);
}

/*
  FUNCTION: _XmGrabShellFocus
  SYNOPSIS: static void _XmGrabShellFocus(Widget w, int operation, Time _time);
  DESCRIPTION:
  This function handles focus operations for the grab shell.
  This is similar to _XmMenuFocus routine.  Only difference is the grab shell
  does not use the state structure (at least for now).  
  END:
*/
static void
_XmGrabShellFocus(Widget w, int operation, Time _time)
{
   Window window_return;
   int revert_to_return;

   switch (operation)
   {
   case XmGRAB_SHELL_FOCUS_SAVE:
      XGetInputFocus(XtDisplay(w),
                     &GS_OldFocus(w),
                     &GS_OldRevertTo(w));
      break;


   case XmGRAB_SHELL_FOCUS_RESTORE:
      XSetInputFocus(XtDisplay(w),
                     GS_OldFocus(w),
                     GS_OldRevertTo(w),
                     _time);

      XGetInputFocus(XtDisplay(w),
                     &window_return,
                     &revert_to_return);

      if (GS_OldFocus(w) != window_return
          || GS_OldRevertTo(w) != revert_to_return)
      {
         DEBUGOUT(_LtDebug(__FILE__, w, "  SetInputFocus call failed.\n"));
      }
      break;

   case XmGRAB_SHELL_FOCUS_SET:
      XSetInputFocus(XtDisplay(w),
                     XtWindow(w),
                     RevertToPointerRoot,
                     _time);

      XGetInputFocus(XtDisplay(w),
                     &window_return,
                     &revert_to_return);

      if (window_return != XtWindow(w)
          || revert_to_return != RevertToPointerRoot)
      {
          DEBUGOUT(_LtDebug(__FILE__, w, "  setting input focus failed\n"));
          _XmUngrabKeyboard(w, _time);
          return;
      }

      break;
   }
}

/*
  FUNCTION: _XmGrabShellGrab
  SYNOPSIS: static void _XmGrabShellGrab(Widget w)
  DESCRIPTION:
  This function handles grab operations for the grab shell.  This is performed
  by calling _XmGrabKeyboard and _XmGrabPointer.
  END:
*/
static void
_XmGrabShellGrab(Widget w, Time _time)
{
   Display *dpy;
   unsigned int mask;

   dpy = XtDisplayOfObject(w);

   _XmGrabKeyboard(w, 
                   GS_OwnerEvents(w),   /* Owner events */
                   GrabModeAsync,       /* Pointer mode */ 
#if 0
                   GS_GrabStyle(w),     /* Pointer mode */
#endif
                   GS_GrabStyle(w),     /* Keyboard mode */
                   _time);        /* Time */


   mask = ButtonPressMask | ButtonReleaseMask; 
#if 0
          | EnterWindowMask | LeaveWindowMask;  
#endif

   _XmGrabPointer(w, 
                  GS_OwnerEvents(w),    /* Owner events */
                  mask,                 /* Event mask */
                  GS_GrabStyle(w),      /* Pointer mode */
                  GrabModeAsync,        /* Keyboard mode */ 
#if 0
                  GS_GrabStyle(w),      /* Keyboard mode */
#endif
                  None,                 /* Confine to */
                  XmGetMenuCursor(dpy), /* Cursor */
                  _time);         /* Time */
}

/*
  FUNCTION: _XmGrabShellUngrab
  SYNOPSIS: static void _XmGrabShellUngrab(Widget w)
  DESCRIPTION:
  This function handles ungrab operations for the grab shell.  This is performed
  by calling _XmUngrabKeyboard and _XmUngrabPointer.
  END:
*/
static void
_XmGrabShellUngrab(Widget w, Time _time)
{
   _XmUngrabPointer(w, _time);
   _XmUngrabKeyboard(w, _time);
}

/* Public routines */
/*
  FUNCTION: XmCreateGrabShell
  SYNOPSIS: Widget XmCreateGrabShell(Widget,char *,ArgList,Cardinal)
  DESCRIPTION:
  Creates an instance of a XmGrabShellWidget.
  END:
*/
Widget
XmCreateGrabShell(Widget parent, char *name, ArgList arglist,
                 Cardinal cnt)
{

   return XtCreatePopupShell(name,
                             xmGrabShellWidgetClass,
                             parent,
                             arglist,
                             cnt);

}


