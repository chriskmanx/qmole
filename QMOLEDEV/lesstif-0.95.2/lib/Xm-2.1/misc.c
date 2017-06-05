/**
 *
 * $Id: misc.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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

/*
 * This file contains routines that just don't belong anywhere else:
   internal stuff and some fallbacks for non-ANSI stuff
 */

#include <LTconfig.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <X11/Intrinsic.h>
#if XtSpecificationRelease < 6
#include <X11/IntrinsicP.h>
#endif

#include <XmI/XmI.h>
#include <Xm/XmP.h>

#include <XmI/LTmisc.h>

#include <XmI/DebugUtil.h>


/* ******************* LessTif internal stuff ******************* */


const char _XmVersionString[] = XmVERSION_STRING;


void
XmUpdateDisplay(Widget w)
{
    XEvent ev;
    Display *dsp;

    dsp = XtDisplay(w);

    /* First process all available events ... */
    while (XCheckMaskEvent(dsp, ExposureMask, &ev))
    {
	XtDispatchEvent(&ev);
    }

    /* Flush all buffers */
    XSync(dsp, False);

    /* Process remaining events ... */
    while (XCheckMaskEvent(dsp, ExposureMask, &ev))
    {
	XtDispatchEvent(&ev);
    }
}

#define even_stipple_width 8
#define even_stipple_height 8
static unsigned char even_stipple_bits[] =
{
    0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55
 };
#define odd_stipple_width 8
#define odd_stipple_height 8
static unsigned char odd_stipple_bits[] =
{
    0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa
 };


void
_XmInstallStippleImages(Widget w)
{
    XImage *even_stipple_image;
    XImage *odd_stipple_image;

    _XmCreateImage(even_stipple_image, XtDisplay(w), (char *)even_stipple_bits,
		   even_stipple_width, even_stipple_height, LSBFirst);
    XmInstallImage(even_stipple_image, XmEVEN_STIPPLE_IMAGE);

    _XmCreateImage(odd_stipple_image, XtDisplay(w), (char *)odd_stipple_bits,
		   odd_stipple_width, odd_stipple_height, LSBFirst);
    XmInstallImage(odd_stipple_image, XmODD_STIPPLE_IMAGE);
}


/*
 * Below are some functions to deal with a multi-threaded environment.
 * Use these instead of the Xt stuff, to ensure that we can still cope
 * with X11r5 where this stuff didn't exist.
 */
#ifdef	XtSpecificationRelease
#if	XtSpecificationRelease > 5
#define	R6plus
#endif
#endif

void
_XmAppLock(XtAppContext appc)
{
#ifdef	R6plus
	XtAppLock(appc);
#endif
}


void
_XmAppUnlock(XtAppContext appc)
{
#ifdef	R6plus
	XtAppUnlock(appc);
#endif
}


void
_XmProcessLock(void)
{
#ifdef	R6plus
	XtProcessLock();
#endif
}


void
_XmProcessUnlock(void)
{
#ifdef	R6plus
	XtProcessUnlock();
#endif
}


/*
 * These are utility functions so you don't need to type so much.
 * Just pass a widget or gadget.
 */
void
_XmObjectLock(Widget w)
{
#ifdef	R6plus
	if (XmIsGadget(w))
		XtAppLock(XtWidgetToApplicationContext(
			XtParent(w)));
	else
		XtAppLock(XtWidgetToApplicationContext(w));
#endif
}


void
_XmObjectUnlock(Widget w)
{
#ifdef	R6plus
	if (XmIsGadget(w))
		XtAppUnlock(XtWidgetToApplicationContext(
			XtParent(w)));
	else
		XtAppUnlock(XtWidgetToApplicationContext(w));
#endif
}


/* ***** A couple of Fallbacks for non-ANSI stuff ***** */
/*       
       This might a crucial thing: if upon build-time the decision
       is made to build these implementations they might hurt 
       later on another system or after a system update.
       While the implementations may not be perfect we ensure
       that their signatures are conforming to the relevant
       standards, i.e. Single UNIX Specification V2 and the
       upcoming joint version of "Posix.1 & SUSV3".
	 
       The following hacks _should_ therefore conform to the docs
	 e.g. from
         http://www.opengroup.org/onlinepubs/007908799
     
       Any deviation is not by intention, but a lack of time and
       proper testing ...
       amai, 20010326: move them "out the way" by using
                       a prefix
*/

#ifndef HAVE_STRCASECMP
extern int
_Lt_strcasecmp(const char *s1, const char *s2)
{
    int c1, c2;

    while (*s1 && *s2)
    {
	c1 = tolower(*s1);
	c2 = tolower(*s2);
	if (c1 != c2)
	{
	    return (c1 - c2);
	}
	s1++;
	s2++;
    }
    return (int)(*s1 - *s2);
}
#endif

#ifndef HAVE_STRNCASECMP
extern int
_Lt_strncasecmp(const char *s1, const char *s2, size_t count)
{
    int c1, c2;
    int i = 0;

    while (*s1 && *s2 && i < (int)count)
    {
	c1 = tolower(*s1);
	c2 = tolower(*s2);
	if (c1 != c2)
	{
	    return (c1 - c2);
	}
	s1++;
	s2++;
	i++;
    }
    return (int)(*s1 - *s2);
}
#endif


/* amai:
   The following hacks for basename() and dirname() _should_
   be conforming to Single UNIX Specification (SUS) V2 as of 
      http://www.opengroup.org/onlinepubs/007908799/xsh/basename.html
   and
      http://www.opengroup.org/onlinepubs/007908799/xsh/dirname.html
     
   Any deviation is not by intention, but due a lack of time and
   proper testing ...
*/

#define cDIRSEP '/'

#ifndef HAVE_BASENAME
extern char *
_Lt_basename(char *inpath) {

  char *ptr;
  static char *path=NULL; /* limit memory leak to one entry */

  if (path)
     free(path);
  if (!inpath || !*inpath) {
     path=(char*)malloc(2);
     strcpy(path, ".");
     return path;
  }

  /* Make a local copy */  
  path=(char *)malloc(strlen(inpath)+1);
  strcpy(path, inpath);
  
  /* limit trailing '/' chars */
  ptr=path+strlen(path)-1;
  while( (ptr>path) && (*ptr==cDIRSEP) ) {
     *ptr='\0';
     ptr--;
  }

  /* look up what's left */
  ptr = strrchr(path, cDIRSEP);
  if (!ptr)
     return path;

  if (*(ptr+1))
  {
    return ptr+1;
  }
  else
  {
    /* "/" only */
    return ptr;
  }
} /* basename() */
#endif /* !HAVE_BASENAME */


#ifndef HAVE_DIRNAME
extern char *
_Lt_dirname(char *inpath) {

  char *ptr;
  static char *path=NULL; /* limit memory leak to one entry */


  if (path)
     free(path);  
  if (!inpath || !*inpath) {
     path=(char*)malloc(2);
     strcpy(path, ".");
     return path;
  }

  /* Make a local copy */  
  path=(char *)malloc(strlen(inpath)+1);
  strcpy(path, inpath);

  ptr = strrchr(path, cDIRSEP);
  if (!ptr) {
     strcpy(path, ".");
     return path;
  }

  /* 'eat' trailing '/' characters */
  while( (ptr>path) && (*ptr==cDIRSEP) ) {
     *ptr='\0';
     ptr--;
  }
  ptr=strrchr(path, cDIRSEP);
  if (ptr)
    return ptr;
  else
    return path;

} /* dirname() */ 
#endif /* !HAVE_DIRNAME */
