/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PixConv.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PixConv.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <XmI/PixConvI.h>  /* <--- just to see if we get any warnings */

#include <XmI/DebugUtil.h>

/*
  MODULE: PixConv.c
  DESCRIPTION:
  Contains routines to do pixmap manipulations. 
  END:
*/
/*
  FUNCTION: _XmTopShadowPixmapDefault 
  SYNOPSIS: void _XmTopShadowPixmapDefault(Widget w, int offset, XrmValue *val)
  DESCRIPTION:
  This function is an internal Motif 2.0 function.  It sets the default 
  top shadow pixmap.  This routine is not implemented yet.  It just returns
  XmUNSPECIFIED_PIXMAP for the time being. 
  END:
*/
void
_XmTopShadowPixmapDefault(Widget w, int offset, XrmValue *val)
{
   static Pixmap pmap;

   /* FIXTHIS: This is not implemented yet. */
   pmap = XmUNSPECIFIED_PIXMAP;
   val->addr = (XPointer) &pmap;
}

/*
  FUNCTION: _XmHighlightPixmapDefault 
  SYNOPSIS: void _XmHighlightPixmapDefault(Widget w, int offset, XrmValue *val)
  DESCRIPTION:
  This function is an internal Motif 2.0 function.  It sets the default 
  highlight shadow pixmap.  This routine is not implemented yet.  
  It just returns XmUNSPECIFIED_PIXMAP for the time being. 
  END:
*/
void
_XmHighlightPixmapDefault(Widget w, int offset, XrmValue *val)
{
   static Pixmap pmap;

   /* FIXTHIS: This is not implemented yet. */
   pmap = XmUNSPECIFIED_PIXMAP;
   val->addr = (XPointer) &pmap;
}

/*
  FUNCTION: _XmGetPixmapBasedGC
  SYNOPSIS: GC _XmGetPixmapBasedGC(Widget w, Pixmap pmap, Pixel pix, XtGCMask mask, XGCValues *values)
  DESCRIPTION:
  This function is an internal Motif 2.0 function.  This is a guess at what
  it does since there is no documentation on it that I know of. 
  END:
*/
GC
_XmGetPixmapBasedGC(
   Widget w, 
   Pixmap pmap, 
   Pixel pix, 
   XtGCMask mask,
   XGCValues *values) 
{
   if(pmap != None
      && pmap != XmUNSPECIFIED_PIXMAP)
   {
        /* we're dealing with a pixmap'ed bottom shadow*/
      mask |= GCTile | GCFillStyle;

      values->tile = pmap;
      values->fill_style = FillTiled;
   }
   else
   {
      mask |= GCForeground | GCBackground;

      values->foreground = pix;
      values->background = XtBackground(w);
   }

   return XtGetGC(w, mask, values);
}
