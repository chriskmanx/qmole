/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Direction.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Direction.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/TraitP.h>
#include <Xm/LayoutT.h>
#include <XmI/DirectionI.h>

#include <XmI/DebugUtil.h>


/* ================================================================ */
/* Internal toolkit routines (but external scope) */

void
_XmDirectionDefault(Widget w, int offset, XrmValue *value)
{
   /* FIX ME: I'm a stub */

   static XmDirection dir = XmDEFAULT_DIRECTION;

   DEBUGOUT(_LtDebug(__FILE__, w, "_XmDirectionDefault\n"));

   value->addr = (char *) &dir;
   value->size = sizeof(XmDirection);
}

XmDirection
_XmGetLayoutDirection(Widget w)
{
   /* FIX ME: this is a guess */
   
   XmSpecifyLayoutDirectionTrait ptrait;
   XmDirection dir = XmDEFAULT_DIRECTION;

   if ((ptrait = (XmSpecifyLayoutDirectionTrait)
                  XmeTraitGet((XtPointer)XtClass(w),
		  XmQTspecifyLayoutDirection))!= NULL)
   {
      if(ptrait->get_direction != NULL)
      {
         dir =  ptrait->get_direction(w);
      }
   }

   return dir;
}

void
_XmFromLayoutDirection(Widget widget, int offset, XtArgVal *value)
{
    /* FIX ME: this is a guess */
    XmDirection dir;
    XmStringDirection sdir;

    dir = *((XmDirection *) (((char *)widget) + offset));
    
    sdir = XmDirectionToStringDirection(dir);

    *value = (XtArgVal)sdir;
}


XmImportOperator
_XmToLayoutDirection(Widget widget, int offset, XtArgVal *value)
{
    /* FIX ME: this is a guess */
    XmDirection dir;
    XmStringDirection sdir;

    sdir = (XmStringDirection) *value;
    dir = XmStringDirectionToDirection(sdir);

    *value = (XtArgVal)dir;

    return XmSYNTHETIC_LOAD;
}

/* ================================================================ */
/* Public routines */

Boolean
XmDirectionMatch(XmDirection dir1, XmDirection dir2)
{
   /* A nice exercise in Boolean algebra... 
      Somehow I think one could do better ;-) */
   if ( (dir1&dir2&XmHORIZONTAL_MASK) && 
        ((dir1&XmHORIZONTAL_MASK) != (dir2&XmHORIZONTAL_MASK)) )
      return False;
   if ((dir1&dir2&XmVERTICAL_MASK) && 
        ((dir1&XmVERTICAL_MASK) != (dir2&XmVERTICAL_MASK)) )
      return False;
   if ((dir1&dir2&XmPRECEDENCE_MASK) && 
        ((dir1&XmPRECEDENCE_MASK) != (dir2&XmPRECEDENCE_MASK)) )
      return False;

   return True;
}

Boolean
XmDirectionMatchPartial(XmDirection dir1, XmDirection dir2, XmDirection dir_mask)
{

   if ( (dir1 & dir_mask) == (dir2 & dir_mask) )
      return True;
   else
      return False;
}

/*
   FUNCTION: XmDirectionToStringDirection
   SYNOPSIS: XmStringDirection XmDirectionToStringDirection(XmDirection dir)
   DESCRIPTION:
   XmDirectionToStringDirection converts a XmDirection value into
   its equivalent XmStringDirection value.
   END:
*/
XmStringDirection
XmDirectionToStringDirection(XmDirection dir)
{
   XmStringDirection sdir = XmSTRING_DIRECTION_DEFAULT;

   /* check for horizontal element */
   if (dir & XmHORIZONTAL_MASK)
   {
      /* horizontal flag found */
      if (dir & XmLEFT_TO_RIGHT_MASK)
      {
         sdir = XmSTRING_DIRECTION_L_TO_R; 
      }
      else if (dir & XmRIGHT_TO_LEFT_MASK)
      {
         sdir = XmSTRING_DIRECTION_R_TO_L; 
      }
   }
   return sdir;
}

/*
   FUNCTION: XmStringDirectionToDirection
   SYNOPSIS: XmDirection XmStringDirectionToDirection(XmStringDirection sdir)
   DESCRIPTION:
   XmStringDirectionToDirection converts a XmStringDirection value into
   its equivalent XmDirection value.
   END:
*/
XmDirection
XmStringDirectionToDirection(XmStringDirection sdir)
{

   XmDirection dir = XmDEFAULT_DIRECTION;

   if (sdir == XmSTRING_DIRECTION_L_TO_R)
   {
      dir = XmLEFT_TO_RIGHT;
   }
   else if (sdir == XmSTRING_DIRECTION_R_TO_L)
   {
      dir = XmRIGHT_TO_LEFT;
   } 

   return dir;
}
