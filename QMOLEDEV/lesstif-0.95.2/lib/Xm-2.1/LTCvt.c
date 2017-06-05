/**
 *
 * $Id: LTCvt.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright © 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Id: LTCvt.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#ifdef NONSTANDARD_CONVERTERS

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <XmI/HashI.h>
#include <XmI/ImageCacheI.h>
#include <Xm/XmP.h>
#include <Xm/ScreenP.h>
#if XmVERSION >= 2
#include <Xm/XpmP.h>
#else
#include <XmI/XmXpm.h>
#endif

#include <XmI/DebugUtil.h>


static char *_search_path = NULL;

/* Prototypes to ensure proper type for the definitions below: */
static Boolean
_XmNSECvtStringToBitmap (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtStringToPixmap (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtShapeStyleToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtBooleanToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtBoolToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtAttachmentToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtPositionToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtCardinalToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtPixelToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtColorToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtUnsignedCharToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtDimensionToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtXmStringToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);
static Boolean
_XmNSECvtWidgetToString (Display*, XrmValue*, Cardinal*, XrmValue*, XrmValue*, XtPointer*);


/*
 * _XmNSEGetPixmap is not part of the OSF/Motif API.
 * This routine is used with the string to pixmap converter
 * routine.  It does a lookup into the cache based on the name
 * of the pixmap and also where the foreground, background
 * and depth are equal to zero.
 */
static Pixmap
_XmNSEGetPixmap(Screen *screen, char *fname)
{
    static Colormap _cmap;

#if XmVERSION >= 2
    XpmAttributes xpm_attrib;
#else
    _LtXpmAttributes xpm_attrib;
#endif
    Pixmap pmap;
    Pixmap mask;

    Display *dpy;
    Window w;

    char *pathname_to_pixmap = NULL;

    dpy = DisplayOfScreen(screen);
    w = RootWindowOfScreen(screen);

    /* initialize colormap if it hasn't been done */
    if (_cmap == (Colormap)NULL)
    {
	XWindowAttributes w_attrib;
	XGetWindowAttributes(dpy, w, &w_attrib);
	_cmap = w_attrib.colormap;
    }

    /*
     * Set the foreground, background and depth all to zero 
     * for now.
     */
    pmap = XmGetPixmapByDepth(screen, fname, 0, 0, 0);
    if (pmap != XmUNSPECIFIED_PIXMAP)
    {
	return pmap;
    }

    if (_search_path == NULL)
    {
	_LTCreateSearchPath();
    }

    /*
     * Attempt to find pixmap in search_path
     * if an absolute path was not given.
     */
    if (fname != NULL && fname[0] == '/')
    {
	pathname_to_pixmap = XtNewString(fname);
    }
    else
    {
	SubstitutionRec sub;

	sub.match = 'B';
	sub.substitution = fname;

	pathname_to_pixmap = XtResolvePathname(dpy,
					       "bitmaps",
					       NULL,
					       NULL,
					       _search_path,
					       &sub,
					       1,
					       NULL);
    }

    if (pathname_to_pixmap == NULL || strlen(pathname_to_pixmap) == 0)
    {
	return XmUNSPECIFIED_PIXMAP;
    }

#if XmVERSION >= 2
    xpm_attrib.colormap = _cmap;
    xpm_attrib.closeness = 40000;
    xpm_attrib.valuemask = XpmSize | XpmReturnPixels
	| XpmColormap | XpmCloseness;

    if (XpmReadFileToPixmap(dpy, w, pathname_to_pixmap,
			       &pmap, &mask,
			       &xpm_attrib) == XpmSuccess)
#else
    xpm_attrib.colormap = _cmap;
    xpm_attrib.closeness = 40000;
    xpm_attrib.valuemask = _LtXpmSize | _LtXpmReturnPixels
	| _LtXpmColormap | _LtXpmCloseness;

    if (_LtXpmReadFileToPixmap(dpy, w, pathname_to_pixmap,
			       &pmap, &mask,
			       &xpm_attrib) == _LtXpmSuccess)
#endif
    {
	_LTAddPixmapToCache(fname, pmap, screen, 0, 0, 0, 0, 0, 0, 0);
	/* FIX ME! */
    }
    else
    {
	/* could not find it so lets return it as unspecified */
	pmap = XmUNSPECIFIED_PIXMAP;
    }

    XtFree(pathname_to_pixmap);

    return pmap;
}

/*
 * _XmNSECvtStringToPixmap is not part of the OSF/Motif API.
 * This routine is used to convert a string to a pixmap.
 * This is done by calling the _XmNSEGetPixmap routine with the
 * supplied string.  
 */
static Boolean
_XmNSECvtStringToPixmap(Display *dpy,
		        XrmValue *args,
		        Cardinal *num_args,
		        XrmValue *from,
		        XrmValue *to,
		        XtPointer *converter_data)
{
    static Pixmap _pmap;
    Screen *screen;
    char *name;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmNSECvtStringToPixmap()\n"));

    if (_XmGetDefaultDisplay() == NULL)
    {
	return False;
    }

    if (*num_args != 1)
    {
	XtWarningMsg("wrongParameters",
		     "cvtStringToPixmap",
		     "XtToolkitError",
		     "String to Pixmap conversion needs screen argument",
		     (String *)NULL,
		     (Cardinal *)NULL);
    }

    /* get arguments */
    screen = *((Screen **)args[0].addr);
    name = (char *)from->addr;

    /* over kill check */
    if (name == NULL
	|| strcmp(name, "None") == 0
	|| strcmp(name, "XmUNSPECIFIED_PIXMAP") == 0)
    {
	_pmap = XmUNSPECIFIED_PIXMAP;
    }
    else
    {
	_pmap = _XmNSEGetPixmap(screen, name);
    }

    if (to->addr == NULL)
    {
	to->addr = (XPointer)&_pmap;
	to->size = sizeof(Pixmap);
    }
    else
    {
	if (to->size >= sizeof(Pixmap))
	{
	    *((Pixmap *)to->addr) = _pmap;
	    to->size = sizeof(Pixmap);
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRPixmap);
	}
    }

    return True;
}

/*
 * _XmNSECvtStringToPixmap is not part of the OSF/Motif API.
 * This routine is used to convert a string to a pixmap.
 * This is done by calling the _XmNSEGetPixmap routine with the
 * supplied string.  
 */
static Boolean
_XmNSECvtStringToBitmap(Display *dpy,
		        XrmValue *args,
		        Cardinal *num_args,
		        XrmValue *from,
		        XrmValue *to,
		        XtPointer *converter_data)
{
	static Pixmap _pmap;
	Screen *screen;
	char *name;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmNSECvtStringToBitmap(from=%s) called\n",
		(char *)from->addr));

	if (_XmGetDefaultDisplay() == NULL) {
		return False;
	}

	if (*num_args != 1) {
		XtWarningMsg("wrongParameters", "cvtStringToBitmap", "XtToolkitError",
			"String to Bitmap conversion needs screen argument",
			(String *)NULL,
			(Cardinal *)NULL);
	}

	/* get arguments */
	screen = *((Screen **)args[0].addr);
	name = (char *)from->addr;

	/* over kill check */
	if (name == NULL || strcmp(name, "None") == 0
			|| strcmp(name, "XmUNSPECIFIED_PIXMAP") == 0) {
		_pmap = XmUNSPECIFIED_PIXMAP;
	} else {
		fprintf(stderr, "_XmNSECvtStringToBitmap(from=%s) called\n",
			(char *)from->addr);
		_pmap = XmUNSPECIFIED_PIXMAP;		/* FIX ME */

		return False;
/*		_pmap = _XmNSEGetPixmap(screen, name);	*/
	}

	if (to->addr == NULL) {
		to->addr = (XPointer)&_pmap;
		to->size = sizeof(Pixmap);
	} else {
		if (to->size >= sizeof(Pixmap)) {
			*((Pixmap *)to->addr) = _pmap;
			to->size = sizeof(Pixmap);
		} else {
			XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRPixmap);
		}
	}

	return True;
}

/* Begin stuff copied from X11/Xmu/Converters.h */
#define XtRShapeStyle "ShapeStyle"
#define XtERectangle "Rectangle"
#define XtEOval "Oval"
#define XtEEllipse "Ellipse"
#define XtERoundedRectangle "RoundedRectangle"

#define XmuShapeRectangle 1
#define XmuShapeOval 2
#define XmuShapeEllipse 3
#define XmuShapeRoundedRectangle 4
/* End stuff copied from X11/Xmu/Converters.h */

#ifndef	XtCXtToolkitError
#define	XtCXtToolkitError	"XtToolkitError"
#endif

static String XtNwrongParameters = "wrongParameters";

typedef struct
{
    int value;
    char *name;
    int size;
} ConversionMap;


static ConversionMap boolean_map[] =
{
    {True, XtEtrue, sizeof(XtEtrue)},
    {False, XtEfalse, sizeof(XtEfalse)},
    {True, XtEon, sizeof(XtEon)},
    {False, XtEoff, sizeof(XtEoff)},
    {True, XtEyes, sizeof(XtEyes)},
    {False, XtEno, sizeof(XtEno)},
};


static ConversionMap shape_style_map[] =
{
    {XmuShapeRectangle, XtERectangle, sizeof(XtERectangle)},
    {XmuShapeRectangle, "ShapeRectangle", sizeof("ShapeRectangle")},
    {XmuShapeOval, XtEOval, sizeof(XtEOval)},
    {XmuShapeOval, "ShapeOval", sizeof("ShapeOval")},
    {XmuShapeEllipse, "ShapeEllipse", sizeof("ShapeEllipse")},
    {XmuShapeEllipse, XtEEllipse, sizeof(XtEEllipse)},
    {XmuShapeRoundedRectangle, "ShapeRoundedRectangle",
     sizeof("ShapeRoundedRectangle")},
    {XmuShapeRoundedRectangle, XtERoundedRectangle,
     sizeof(XtERoundedRectangle)},
};


#define	string_convert_done(value) \
	{							\
	    if (toVal->addr != NULL) {				\
		if (toVal->size < size) {		        \
		    toVal->size = size;			        \
		    return False;				\
		}						\
		strcpy((char *) toVal->addr, value);	\
	    }							\
	    else {						\
		toVal->addr = (XPointer)value;			\
	    }							\
	    toVal->size = size; 				\
	    return True;					\
	}

#define map_convert_done(value) \
	{							\
	    if (toVal->addr != NULL) {				\
		if (toVal->size < value.size) {		        \
		    toVal->size = value.size;		        \
		    return False;				\
		}						\
		strcpy((char *) toVal->addr, value.name);	\
	    }							\
	    else {						\
		toVal->addr = value.name;       		\
	    }							\
	    toVal->size = value.size; 				\
	    return True;					\
	}


static void
XtDisplayConversionWarning(String type, XtPointer from, String to)
{
    String params[2];
    Cardinal num_params = 2;

    params[0] = (String)from;
    params[1] = to;
    XtWarningMsg("conversionError", type, XtCXtToolkitError,
		 "Cannot convert value %d to type %s",
		 params, &num_params);
}


static Boolean
_XmNSECvtShapeStyleToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			    XrmValue *fromVal, XrmValue *toVal,
			    XtPointer *data)
{
    int *i = (int *)(fromVal->addr);
    int index;


    for (index = 0; XtNumber(shape_style_map); index++)
    {
	if (*i == shape_style_map[index].value)
	{
	    map_convert_done(shape_style_map[index]);
	}
    }

    XtDisplayConversionWarning(XtRShapeStyle, (XtPointer)(long)*i, XtRString);
    return False;
}


static Boolean
_XmNSECvtBooleanToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			 XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    Boolean *i = (Boolean *)(fromVal->addr);
    int index;


    for (index = 0; XtNumber(boolean_map); index++)
    {
	if (*i == boolean_map[index].value)
	{
	    map_convert_done(boolean_map[index]);
	}
    }

    XtDisplayConversionWarning(XtRBoolean, (XtPointer)(long)*i, XtRString);
    return False;
}


static Boolean
_XmNSECvtBoolToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
		      XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    Boolean *i = (Boolean *)(fromVal->addr);
    int index;


    for (index = 0; XtNumber(boolean_map); index++)
    {
	if (*i == boolean_map[index].value)
	{
	    map_convert_done(boolean_map[index]);
	}
    }

    XtDisplayConversionWarning(XtRBool, (XtPointer)(long)*i, XtRString);
    return False;
}


static Boolean
_XmNSECvtAttachmentToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			  XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    const char *buffer;

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		  XtNwrongParameters, "cvtattachmentToString", XtCXtToolkitError,
		      "Attachment to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    /* buffer must not be freed! */
    buffer = _LtDebugAttachment2String(*(Position *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtPositionToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			  XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		  XtNwrongParameters, "cvtPositionToString", XtCXtToolkitError,
		      "Position to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "%d", *(Position *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtIntToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
		     XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			XtNwrongParameters, "cvtIntToString", XtCXtToolkitError,
			"Int to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "%d", *(int *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtCardinalToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			  XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		  XtNwrongParameters, "cvtCardinalToString", XtCXtToolkitError,
		      "Cardinal to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "%d", *(Cardinal *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtPixelToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
		       XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		     XtNwrongParameters, "cvtPixelToString", XtCXtToolkitError,
			"Pixel to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "%ld", *(Pixel *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtColorToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
		       XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		     XtNwrongParameters, "cvtColorToString", XtCXtToolkitError,
			"Color to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "rgb:%04hx/%04hx/%04hx", ((XColor *)fromVal->addr)->red,
	    ((XColor *)fromVal->addr)->green, ((XColor *)fromVal->addr)->blue);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtUnsignedCharToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			      XrmValue *fromVal, XrmValue *toVal,
			      XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			XtNwrongParameters, "cvtUnsignedCharToString",
			XtCXtToolkitError,
		  "UnsignedChar to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "%d", *(unsigned char *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtDimensionToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			   XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    static char buffer[32];

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		 XtNwrongParameters, "cvtDimensionToString", XtCXtToolkitError,
		     "Dimension to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    sprintf(buffer, "%d", *(Dimension *)fromVal->addr);
    size = strlen(buffer);
    string_convert_done(buffer);
}


static Boolean
_XmNSECvtXmStringToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			  XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    char *s;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmNSECvtXmStringToString\n"));

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		 XtNwrongParameters, "cvtDimensionToString", XtCXtToolkitError,
		      "XmString to String conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    if (XmStringGetLtoR(*(XmString *)fromVal->addr, XmFONTLIST_DEFAULT_TAG, &s))
    {
	/* There's a memory leak here : nobody does free(s) */
	size = strlen(s);
	string_convert_done(s);
    } 
    else
       return False;		/* Conversion failed */
}


static Boolean
_XmNSECvtWidgetToString(Display *dpy, XrmValue *args, Cardinal *numArgs,
			  XrmValue *fromVal, XrmValue *toVal, XtPointer *data)
{
    int size;
    Widget w;
    static char *s = NULL; /* amai: a small(?) memory leak - see below */

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmNSECvtWidgetToString\n"));

    if (*numArgs != 0)
    {
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		 XtNwrongParameters, "cvtWidgetToString", XtCXtToolkitError,
		      "Widget to String conversion needs extra arguments",
			(String *)NULL, (Cardinal *)NULL);
    }

    if (s)
      XtFree(s);
    w = *(Widget *)fromVal->addr;
    s = XtNewString(XtName(w));
    size = strlen(s);
    string_convert_done(s);
}


/* this is needed for the string to pixmap converters */
static XtConvertArgRec args[] =
{
    {
	XtBaseOffset,
	(XtPointer)XtOffsetOf(WidgetRec, core.screen),
	sizeof(Screen *)
    }
};


/*
 * register nonstandard extensions, if any
 */
extern void
_XmRegisterNSEConverters(void)
{
    XtSetTypeConverter(XmRString,	/* source type */
		       XmRPixmap,	/* target type */
		       _XmNSECvtStringToPixmap,	/* converter routine */
		       args,		/* args for converter routine */
		       XtNumber(args),	/* number of args for converter */
		       XtCacheNone,	/* caching instructions */
		       NULL);		/* destructor function */

    XtSetTypeConverter(XmRString,	/* source type */
		       XmRBitmap,	/* target type */
		       _XmNSECvtStringToBitmap,	/* converter routine */
		       args,		/* args for converter routine */
		       XtNumber(args),	/* number of args for converter */
		       XtCacheNone,	/* caching instructions */
		       NULL);		/* destructor function */

#if 0
/* amai: see thread about DragIcon/Ishmail */
    XtSetTypeConverter(XmRString,
		       XmRBitmap,
		       _XmNSECvtStringToBitmap,
		       args,
		       XtNumber(args),
		       XtCacheNone,
		       NULL);
#endif
    XtSetTypeConverter(XmRString,
		       XmRXmBackgroundPixmap,
		       _XmNSECvtStringToPixmap,
		       args,
		       XtNumber(args),
		       XtCacheNone,
		       NULL);

    /* Converters for reverse-direction Editres below */
    XtSetTypeConverter(XtRShapeStyle, XtRString, _XmNSECvtShapeStyleToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRBoolean, XtRString, _XmNSECvtBooleanToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRBool, XtRString, _XmNSECvtBoolToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRShapeStyle, XtRString, _XmNSECvtShapeStyleToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRBoolean, XtRString, _XmNSECvtBooleanToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRBool, XtRString, _XmNSECvtBoolToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XmRAttachment, XtRString, _XmNSECvtAttachmentToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRPosition, XtRString, _XmNSECvtPositionToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRInt, XtRString, _XmNSECvtIntToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRCardinal, XtRString, _XmNSECvtCardinalToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRPixel, XtRString, _XmNSECvtPixelToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRColor, XtRString, _XmNSECvtColorToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRUnsignedChar, XtRString,
		       _XmNSECvtUnsignedCharToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRDimension, XtRString, _XmNSECvtDimensionToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XmRXmString, XtRString, _XmNSECvtXmStringToString,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XmRWidget, XtRString, _XmNSECvtWidgetToString,
		       NULL, 0, XtCacheNone, NULL);
}

#endif /* NONSTANDARD_CONVERTERS */
