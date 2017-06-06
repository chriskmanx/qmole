/*
  ImageMagick X11 compatibility methods.
*/
#ifndef _PRER5ICCCM_H
#define _PRER5ICCCM_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#if defined(PRE_R6_ICCCM)
/*
  Compatability defines for pre X11R6 ICCCM.
*/
#define XK_KP_Home              0xFF95
#define XK_KP_Left              0xFF96
#define XK_KP_Up                0xFF97
#define XK_KP_Right             0xFF98
#define XK_KP_Down              0xFF99
#define XK_KP_Prior             0xFF9A
#define XK_KP_Page_Up           0xFF9A
#define XK_KP_Next              0xFF9B
#define XK_KP_Page_Down         0xFF9B
#define XK_KP_End               0xFF9C
#define XK_KP_Delete            0xFF9F

extern Export Status
  XInitImage(XImage *ximage);
#endif

#if defined(PRE_R5_ICCCM)
/*
  Compatability defines for pre X11R5 ICCCM.
*/
extern Export XrmDatabase
  XrmGetDatabase();
#endif

#if defined(PRE_R4_ICCCM)
/*
  Compatability defines for pre X11R4 ICCCM.
*/
#if defined(vms)
#define XMaxRequestSize(display)  16384
#endif

#define WithdrawnState  0
#define XInductColormap(display,colormap)  XInstallColormap(display,colormap)
#define XUninductColormap(display,colormap)  XUninstallColormap(display,colormap)

typedef struct _XTextProperty
{
  unsigned char
    *value;

  Atom
    encoding;

  int
    format;

  unsigned long
    nitems;
} XTextProperty;

/*
  Pre R4 ICCCM compatibility routines.
*/
char
  *XResourceManagerString();

extern Export int
  XWMGeometry();

extern Export Status
  XGetRGBColormaps(),
  XGetWMName(),
  XReconfigureWMWindow(),
  XSetWMProtocols(),
  XWithdrawWindow();

extern Export XClassHint
  *XAllocClassHint();

extern Export XIconSize
  *XAllocIconSize();

extern Export XSizeHints
  *XAllocSizeHints();

extern Export XStandardColormap
  *XAllocStandardColormap();

extern Export XWMHints
  *XAllocWMHints();

extern Export VisualID
  XVisualIDFromVisual();

extern Export void
  XrmDestroyDatabase(),
  XSetWMIconName(),
  XSetWMName(),
  XSetWMProperties();
#else
#define XInductColormap(display,colormap)
#define XUninductColormap(display,colormap)
#endif

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
