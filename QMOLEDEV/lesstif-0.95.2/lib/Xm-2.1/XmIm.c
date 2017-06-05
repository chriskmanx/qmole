/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/XmIm.c,v 1.3 2007/09/12 20:38:02 jwrdegoede Exp $
 *
 * Copyright © 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: XmIm.c,v 1.3 2007/09/12 20:38:02 jwrdegoede Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/Xos.h>
#include <X11/Xlocale.h> /* don't #include <locale.h> here! */

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextFP.h>
#include <Xm/VendorSEP.h>
#include <Xm/XmIm.h>
#include <XmI/LTmisc.h>
#include <XmI/DebugUtil.h>


/*
 * This file contains I18N (Internationalisation) Input Method handling.
 *
 * Comments are deduced from the Motif 2.0 manual pages.
 *
 * The current implementation is *Quick and Dirty* and follows the motif
 * specs incompletely. - Danny 4 Feb 96.
 */

#undef TextF_FontTextWidth
#define TextF_FontTextWidth(w,s,l)     (int)_XmTextF_FontTextWidth(w, s, l)

/*
 * This is a private data structure. A pointer to it is in the
 * VendorShell Extension object.
 */
typedef struct XmICStuff
{
    XIC			xic;
    XIM			xim;
    Widget		ve, text;		/* Don't know if we need this */
    struct XmICStuff	*next;
    struct XmICStuff	*orig_xim;
    int			count;
}
XmICStuff;

static void _XmSetPosition(Widget w, XPoint *pos);

static void _XmImReconfigXIC(Widget w, XmICStuff *stuff, XFontSet fontset, XPoint *sp);

extern int _XmTextF_FontTextWidth(Widget w, char *s, int l);

/*
 * Find the private data structure and cast it correctly.
 * This is close to the implementation of XmImGetXIM.
 */
static struct XmICStuff *
_XmFindICStuff(Widget w)
{
    XmVendorShellExtObject v = (XmVendorShellExtObject)_LtFindVendorExt(w);
    XmICStuff *stuff;

    if (v == NULL)
    {
	return NULL;
    }

    stuff = (XmICStuff *)v->vendor.im_info;

    while (stuff)
    {
	if (stuff->text == w)
	{
	    return stuff;
	}
	stuff = stuff->next;
    }

    return NULL;
}


static void
_XmFreeICStuff(Widget w, XmICStuff *stuff)
{
	XmICStuff		*p, *q;
	XmVendorShellExtObject	v = (XmVendorShellExtObject)_LtFindVendorExt(w);

	if (v == NULL || stuff == NULL)
		return;

	p = (XmICStuff *)v->vendor.im_info;
	if (p == stuff) {
		/* It's the first entry - simple. */
		v->vendor.im_info = (XtPointer)stuff->next;
	} else {
		while (p) {
			q = p->next;
			if (q == stuff)
				break;
			p = q;
		}
		if (!p)
			return;		/* Should this happen ? */
		p->next = q->next;
	}

	/* if count!=0 then someone uses the stuff as orig_xim
	   so unlink it but not free it */
	if (!stuff->count)
		XtFree((char *)stuff);
}

/*
 * XmImGetXIC creates and registers an XIC with the specified arguments for the
 * widget. If XmINHERIT_POLICY is specified for input_policy, then a new XIC
 * will be created only if required by the arguments or by the VendorShell
 * input policy. Any existing XIC registered with the widget is unregistered.
 *
 * If input_policy is IP_PER_WIDGET, then a new XIC for this widget is created.
 * If it is IP_PER_SHELL, then a new XIC for the shell is created if needed.
 *
 * (Danny's comment :)
 * Reusing an XIC is - in my opinion - not possible between widgets, because
 * the XNClientWindow of an XIC can not be changed after creation.
 *
 * Reusing the XIM is possible though.
 */

#define XmIMInputPolicy XmInputPolicy

/* this is a 2.x call, but we need it in 1.2 as well */
extern XIC
XmImGetXIC(Widget w, XmIMInputPolicy input_policy, ArgList args, Cardinal num_args)
{
    XmVendorShellExtObject v = (XmVendorShellExtObject)_LtFindVendorExt(w);

    XIM xim;			/* the input method */
    XIMStyles *xim_styles;
    XIMStyle input_style = 0;
    XmICStuff *stuff = NULL, *s;
    char *p, *buf, *b, *k;

    /*
     * Note : the two structures below should be handled together.
     * Don't edit/comment out stuff in one without dealing with the other.
     */
    static char *styles[] =
    {
	"OverTheSpot",
	"OffTheSpot",
	"Rxvt",		/* This isn't a legal input style ! */
	"Root",
	"Root",
	NULL};

    static XIMStyle style_bits[] =
    {
    /* OverTheSpot */		XIMPreeditPosition | XIMStatusArea,
    /* OffTheSpot */		XIMPreeditArea | XIMStatusArea,
    /* Like rxvt */		XIMPreeditPosition | XIMStatusNothing,
    /* Root */			XIMPreeditNothing | XIMStatusNothing,
    /* Not really root */	XIMPreeditNone | XIMStatusNone
    };

    Boolean found;
    int i, j;

    if (v == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmImRegister: no vendor shell extension found\n"));
	return 0;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmImRegister: InputMethodString '%s'\n",
		      v->vendor.input_method_string
		      ? v->vendor.input_method_string
		      : "(null)"));

    if ((stuff = _XmFindICStuff(w)) == NULL)
    {
	stuff = (XmICStuff *)XtMalloc(sizeof(XmICStuff));
	stuff->ve = (Widget)v;
	stuff->text = w;
	stuff->xim = NULL;
	stuff->xic = NULL;
	stuff->count = 1;
	stuff->orig_xim = NULL;

	stuff->next = (XmICStuff *)v->vendor.im_info;
	v->vendor.im_info = (XtPointer)stuff;
    }

    p = v->vendor.input_method_string;
    xim = NULL;

    /* Check locale to avoid SEGV when XOpenIM. */
    if (p || XSetLocaleModifiers("") != ""){	/* Input Method is defined? */
	char *locale;

	locale = setlocale(LC_ALL, NULL); /* Query */
	if (locale == NULL || !strcmp(locale, "C") || !strcmp(locale, "POSIX"))
	  return 0;
    }
    else	/* Not open IM */
	return 0;

    /*
     * Try to reuse an XIM
     */
    for (s = (XmICStuff *)v->vendor.im_info; s != NULL; s = s->next)
    {
	if (s->ve == (Widget)v && s != stuff
		&& s->orig_xim == NULL)		/* Only accept an 'original' one */
	{
	    xim = s->xim;

	    stuff->orig_xim = s;
	    stuff->count = 0;	/* Don't count, we're a reference ! */
	    s->count++;

	    DEBUGOUT(_LtDebug(__FILE__, w, "XmImGetXIC: reuse XIM %p (count %d)\n", xim, s->count));
	    break;
	}
    }

    if (!xim)
    {
	if (p)
	{
	    /* Loop over the contents of input_method_string */
	    b = p;
	    while (*b)
	    {
		k = strchr(b, ',');
		if (k)
		{
		    *k = '\0';
		}
		buf = XtMalloc(10 + strlen(b));
		strcpy(buf, "@im=");
		strcat(buf, b);

		if ((p = XSetLocaleModifiers(buf)) != NULL)
		{
		    xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);

		    if (xim) {
			DEBUGOUT(_LtDebug(__FILE__, w,
			    "XOpenIM succeeded with XSetLocaleModifiers(%s) -> xim %p\n",
			    buf, xim));
		    }
		}
		XtFree(buf);
		if (k)
		{
		    *k = ',';
		    b = k + 1;
		}
		else
		{
		    break;
		}

		if (xim)
		{
		    break;
		}
	    }
	}
	else
	{
	    /* Use default to open IM. */
	    if (XSetLocaleModifiers ("") != NULL){
	    	xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);
	    }
	    /*if ((p = XSetLocaleModifiers("@im=none")) != NULL) {
		xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);
		if (xim) {
		    DEBUGOUT(_LtDebug(__FILE__, w,
			"XOpenIM succeeded with XSetLocaleModifiers(%s)\n",
			"@im=none"));
		}
	    }*/
	}

	if (xim == NULL && (p = XSetLocaleModifiers("")) != NULL)
	{
	    xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);
	    if (xim) {
		DEBUGOUT(_LtDebug(__FILE__, w,
			"XOpenIM succeeded with XSetLocaleModifiers('')\n"));
	    }
	}
    }

    if (!xim)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Failed to open input method\n"));
	return 0;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XOpenIM -> %p\n", xim));
    if (XGetIMValues(xim, XNQueryInputStyle, &xim_styles, NULL) || xim_styles == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Input method doesn't support any style\n"));
	XCloseIM(xim);
	return 0;
    }

#if 0
    if (_LtDebugInDebug(__FILE__, w)) {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmImRegister - Supported styles :\n"));
	for (j = 0; j < xim_styles->count_styles; j++)
	{
	    DEBUGOUT(_LtDebug0(__FILE__, w, " (%d) %X %s", j,
			       xim_styles->supported_styles[j],
			   (xim_styles->supported_styles[j] & XIMStatusNothing)
			       ? "XIMStatusNothing" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			       (xim_styles->supported_styles[j] & XIMStatusArea)
			       ? "XIMStatusArea" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			 (xim_styles->supported_styles[j] & XIMStatusCallbacks)
			       ? "XIMStatusCallbacks" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			       (xim_styles->supported_styles[j] & XIMStatusNone)
			       ? "XIMStatusNone" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			  (xim_styles->supported_styles[j] & XIMPreeditNothing)
			       ? "XIMPreeditNothing" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			 (xim_styles->supported_styles[j] & XIMPreeditPosition)
			       ? "XIMPreeditPosition" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMPreeditCallbacks)
			       ? "XIMPreeditCallbacks" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			     (xim_styles->supported_styles[j] & XIMPreeditNone)
			       ? "XIMPreeditNone" : ""));
	    DEBUGOUT(_LtDebug0(__FILE__, w, " %s",
			     (xim_styles->supported_styles[j] & XIMPreeditArea)
			       ? "XIMPreeditArea" : ""));

	    DEBUGOUT(_LtDebug0(__FILE__, w, "\n"));
	}
    }
#endif

    /* Check whether we have input styles that match the resource */
    /*
     * FIX ME
     *
     * This should really use a "find the best option" algorithm, instead 
     * of just looking for the first match.
     */
    for (i = 0, found = False; styles[i] && !found; i++)
    {
#if 0
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Trying %s (0x%X)\n", styles[i], style_bits[i]));
#endif

	if (strstr(v->vendor.preedit_type_string, styles[i]) == 0)
	{
	    continue;
	}
	for (j = 0; j < xim_styles->count_styles; j++)
	{
	    if (style_bits[i] == xim_styles->supported_styles[j])
	    {
		found = True;
		input_style = style_bits[i];
		break;
	    }
	}
    }
    XFree(xim_styles);

    if (!found)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
	     "XmImRegister: input method doesn't support our preedit type\n"));
	XCloseIM(xim);
	return 0;
    }

    stuff->xim = xim;

    if (!XtIsRealized(XtParent(v)))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "XmImRegister: not realized yet\n"));
    }
    else
    {
	XRectangle rect;
	XRectangle status_rect;
	XPoint spot;
	XVaNestedList preedit_attr = NULL;
	XVaNestedList status_attr = NULL;
	OutputData o;
	unsigned long fg, bg;
	XFontSet	fontset = NULL;
	int		fontheight = 0;

	if (XmIsTextField(w))
	{
	    if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_FONTSET)
	    {
		fontset = (XFontSet)TextF_FontList(w)->renditions[0]->font;
		fontheight = TextF_FontHeight(w);
	    }
        }
	else if (XmIsText(w))
	{
	    o = Text_OutputData(w);
	    if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONTSET)
	    {
	        fontset = (XFontSet)Out_FontList(o)->renditions[0]->font;
		fontheight = Out_FontHeight(o);
	    }
	}

	if ((input_style & XIMPreeditPosition) && fontset)
	{

            rect.x = XtX(w);
            rect.y = XtY(w);
            rect.width  = XtWidth(w);
            rect.height = XtHeight(w);

            _XmSetPosition(w, &spot);

	/*
	 * FIX ME
	 * The lines below used to say 0 and 65535 which was clearly
	 * hardware dependent. Now it's not hardware dependent but still
	 * hardcoded to black and white. What should these be ?
	 *
	 * This occurs in several places in this file.
	 */
	    fg = BlackPixelOfScreen(XtScreen(w));
	    bg = WhitePixelOfScreen(XtScreen(w));

            preedit_attr = XVaCreateNestedList(0,
                                        XNArea, &rect,
                                        XNSpotLocation, &spot,
					XNForeground, fg,
					XNBackground, bg,
					XNFontSet, fontset,
                                        NULL);
	}

	stuff->xic = XCreateIC(xim,
			       XNInputStyle, input_style,
			       XNClientWindow, XtWindow(w),
			       XNFocusWindow, XtWindow(w),
                        preedit_attr ? XNPreeditAttributes : NULL, preedit_attr,
                        status_attr ? XNStatusAttributes : NULL, status_attr,
			       NULL);

	/* Try to use XIMPreeditArea */
	if (!stuff->xic && fontset)
	{

	    input_style = XIMPreeditArea | XIMStatusArea;

            rect.x = XtX(w);
            rect.y = XtY(w) + XtHeight(w) - fontheight * 2;
            rect.width  = XtWidth(w);
            rect.height = fontheight;

            status_rect.x = XtX(w);
            status_rect.y = XtY(w) + XtHeight(w) - fontheight;
            status_rect.width  = XtWidth(w);
            status_rect.height = fontheight;

	/*
	 * FIX ME
	 * The lines below used to say 0 and 65535 which was clearly
	 * hardware dependent. Now it's not hardware dependent but still
	 * hardcoded to black and white. What should these be ?
	 *
	 * This occurs in several places in this file.
	 */
	    fg = BlackPixelOfScreen(XtScreen(w));
	    bg = WhitePixelOfScreen(XtScreen(w));

            preedit_attr = XVaCreateNestedList(0,
                                        XNArea, &rect,
					XNForeground, fg,
					XNBackground, bg,
					XNFontSet, fontset,
                                        NULL);

            status_attr = XVaCreateNestedList(0,
				XNArea, &status_rect,
				XNForeground, fg,
				XNBackground, bg,
				XNFontSet, fontset,
			NULL);
	}

	if (! stuff->xic)
		stuff->xic = XCreateIC(xim,
				XNInputStyle, input_style,
				XNClientWindow, XtWindow(w),
				XNFocusWindow, XtWindow(w),
				preedit_attr ? XNPreeditAttributes : NULL, preedit_attr,
				status_attr ? XNStatusAttributes : NULL, status_attr,
			NULL);

	if (preedit_attr) XFree(preedit_attr);
	if (status_attr)  XFree(status_attr);

	if (!stuff->xic)
	{
	    /* Force input_style to root-window */
	    input_style = XIMPreeditNothing | XIMStatusNothing;
	    stuff->xic = XCreateIC(xim,
			XNInputStyle, input_style,
			XNClientWindow, XtWindow(w),
			XNFocusWindow, XtWindow(w),
		NULL);
	}
	else if (input_style & XIMPreeditArea)
	{
		XRectangle rect;
		XVaNestedList preedit_attr = NULL;

		rect.x = 0; rect.y = 0; rect.width = 0; rect.height = 0;
    		preedit_attr = XVaCreateNestedList(0, XNAreaNeeded, &rect, NULL);
    		XSetICValues(stuff->xic, XNPreeditAttributes, preedit_attr, NULL);

		XFree(preedit_attr);
	}

	if (stuff->xic)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "We have an IC\n"));
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "IC Creation failed\n"));
	    XCloseIM(xim);
	}
    }

    return stuff->xic;
}


#if 0
extern void
XmSetStatusPosition(void)
{
    return;
}
#endif


static void
_XmSetPosition(Widget w, XPoint *pos)
{

    if (XmIsTextField(w))
    {
        pos->x = TextF_FontTextWidth(w, TextF_Value(w), TextF_CursorPos(w))
		+ TextF_XDraw(w) + TextF_XOffset(w);
        pos->y = TextF_YDraw(w) + TextF_FontAscent(w);
    }
    else if (XmIsText(w))
    {
        OutputData o = Text_OutputData(w);
        pos->x = Out_CursorX(o);
        pos->y = Out_CursorY(o);
    }

}


extern void
_XmImSendSpot(Widget w) /* Code from rxvt */
{
    XPoint          spot;
    XVaNestedList   preedit_attr;
    XIMStyle        input_style;
    XmICStuff       *stuff = NULL;

    stuff = _XmFindICStuff(w);
    if (stuff == NULL)
	return;

    if (stuff->xic == NULL)
        return;
    else {
        XGetICValues(stuff->xic, XNInputStyle, &input_style, NULL);
        if (!(input_style & XIMPreeditPosition))
            return;
    }
    _XmSetPosition(w, &spot);

    preedit_attr = XVaCreateNestedList(0, XNSpotLocation, &spot, NULL);
    XSetICValues(stuff->xic, XNPreeditAttributes, preedit_attr, NULL);
    XFree(preedit_attr);
}


/*
 * XmImRegister registers a widget with its input manager. This adds the specified
 * widget to a list of widgets that are supported by the input manager for an input
 * method. If an input method has not been opened by a previous call to XmImRegister,
 * the first time this routine is called it opens an input method using the XmNinputMethod
 * resource for the VendorShell. If the XmNinputMethod is NULL, an input method is
 * opened using the current locale.
 *
 * If an input method cannot be opened in the current locale, XLookupString provides
 * input processing. The application is responsible for unregistering a widget by
 * calling XmImUnregister.
 */
extern void
XmImRegister(Widget w,
             unsigned int reserved)
{
    XIC xic;

    xic = XmImGetXIC(w, IP_PER_SHELL, NULL, 0);
}


/*
 * XmImUnregister removes the specified widget from the list of widgets
 * registered for input by the input manager.
 */
extern void
XmImUnregister(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "XmImUnregister is not implemented yet.\n"));
}


/*
 * XmImSetFocusValues notifies the input manager that the specified widget
 * has received input focus. This function also updates the attributes of
 * the input context associated with the widget.
 * If the previous parameters for the widget's IC do not allow the previously
 * registered IC to be reused, that IC will be unregistered, and a new one will
 * be created and registered with the widget.
 */
extern void
XmImSetFocusValues(Widget w, ArgList args, Cardinal num_args)
{
    XmICStuff *stuff = _XmFindICStuff(w);


    if (stuff == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "XmImSetFocusValues (no IC found)\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmImSetFocusValues\n"));

    XmImSetValues(w, args, num_args);

    if (stuff->xic) {
	XSetICFocus(stuff->xic);
    } else {
	XmImRegister(w, 0);
        XmImSetValues(w, args, num_args);

	if (stuff->xic)
	    XSetICFocus(stuff->xic);
    }
}


/* Varargs interface to the above. */
extern void
XmImVaSetFocusValues(Widget w, ...)
{
    va_list ap;
    Arg *a;
    int n, i;

    /* determine the number of argument pairs */
    va_start(ap, w);
    n = 0;
    while (va_arg(ap, char *) != NULL)
    {
	va_arg(ap, XtArgVal);
	n++;
    }
    va_end(ap);

    if (n>0) {
       a = (Arg *)XtCalloc(n, sizeof(Arg));
       va_start(ap, w);
       for (i = 0; i < n; i++)
       {
   	  a[i].name = va_arg(ap, char *);
    	  a[i].value = va_arg(ap, XtArgVal);
       }
       va_end(ap);
       XmImSetFocusValues(w, a, n);
       XtFree((char *)a);
    }
    else {
       XmImSetFocusValues(w, NULL, 0);
    }
}


/*
 * XmImSetValues updates attributes of the input context associated with the
 * specified widget.
 * If the previous parameters for the widget's IC do not allow the previously
 * registered IC to be reused, that IC will be unregistered, and a new one will
 * be created and registered with the widget.
 */
extern void
XmImSetValues(Widget w,
              ArgList args,
              Cardinal num_args)
{

    /* But only support XmNforeground, XmNbackground, XmNfontList, XmNspotLocation */
    Cardinal i;
    XmICStuff *stuff = _XmFindICStuff(w);
    Boolean font_changed = False;
    Boolean spot_changed = False;
    XFontSet fontset = NULL;
    XPoint spot;

    XVaNestedList preedit_attr = NULL;

    if (stuff == NULL || stuff->xic == NULL) {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmImSetValues(NULL)\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmImSetValues()\n"));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, NULL, args, num_args, False));

    for (i = 0; i < num_args; i++)
    {
	if (strcmp(args[i].name, XmNforeground) == 0)
	{
    	    preedit_attr = XVaCreateNestedList(0, XNForeground, args[i].value, NULL);
	}
	else if (strcmp(args[i].name, XmNbackground) == 0)
	{
    	    preedit_attr = XVaCreateNestedList(0, XNBackground, args[i].value, NULL);
	}
	else if (strcmp(args[i].name, XmNspotLocation) == 0)
	{
    	    preedit_attr = XVaCreateNestedList(0, XNSpotLocation, args[i].value, NULL);
	    spot.x = ((XPoint *)args[i].value)->x;
	    spot.y = ((XPoint *)args[i].value)->y;
	    spot_changed = True;
	}
	else if (strcmp(args[i].name, XmNfontList) == 0)
	{
	    XmFontList fontlist = (XmFontList)args[i].value;
	    if (fontlist->renditions[0]->type == XmFONT_IS_FONTSET)
	    {
    	        preedit_attr = XVaCreateNestedList(0,
			XNFontSet, fontlist->renditions[0]->font, NULL);
		font_changed = True;
		fontset = (XFontSet)fontlist->renditions[0]->font;
	    }
	    else
	        continue;
	}
	else
	    continue;

    	XSetICValues(stuff->xic, XNPreeditAttributes, preedit_attr, NULL);
	/* FIX ME - XNLineSpacing, XNColormap, XNStdColormap,
	   XNAreaNeeded, XNSpotLocation, XNArea, ... */
    }

    if (font_changed)	/* We try to use better input_style */
    {
        if (spot_changed)
            _XmImReconfigXIC(w, stuff, fontset, &spot);
	else
            _XmImReconfigXIC(w, stuff, fontset, NULL);
    }

    /*
     * FIX ME -
     *      should check whether the vendor shell is realized ??
     *      how do you cope with XNVaNestedList ??
     */

    if (preedit_attr) XFree(preedit_attr);
}


/* Varargs interface to the above. */
extern void
XmImVaSetValues(Widget w, ...)
{
    va_list ap;
    Arg *a;
    int n, i;

    /* determine the number of argument pairs */
    va_start(ap, w);
    n = 0;
    while (va_arg(ap, char *) != NULL)
    {
	va_arg(ap, XtArgVal);
	n++;
    }
    va_end(ap);

    if (n>0) {
       a = (Arg *)XtCalloc(n, sizeof(Arg));
       va_start(ap, w);
       for (i = 0; i < n; i++)
       {
   	   a[i].name  = va_arg(ap, char *);
	   a[i].value = va_arg(ap, XtArgVal);
       }
       va_end(ap);
       XmImSetValues(w, a, n);
       XtFree((char *)a);
    }
    else {
       XmImSetValues(w, NULL, 0);
    }
}


/*
 * XmImUnsetFocus unsets a specified widget's focus, then notifies the input
 * manager that the specified widget has lost its input focus.
 */
extern void
XmImUnsetFocus(Widget w)
{
    XmICStuff *stuff;

    if (w == NULL) {
#ifdef	LESSTIF_VERBOSE
	_XmWarning(w, "XmImUnsetFocus(NULL)\n");
#endif
	return;
    }

    stuff = _XmFindICStuff(w);
    if (stuff == NULL) {
#ifdef	LESSTIF_VERBOSE
	_XmWarning(w, "XmImUnsetFocus: _XmFindICStuff returned NULL\n");
#endif
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmImUnsetFocus\n"));

    if (stuff->text == w && stuff->xic)
    {
	XUnsetICFocus(stuff->xic);
    }
}


/*
 * XmImGetXIM retrieves the XIM data structure representing the input method
 * that the input manager has opened for the specified widget.
 * If an input method has not been opened by a previous call to XmImRegister,
 * the first time this routine is called it opens an input method using the
 * XmNinputMethod resource for the VendorShell.
 */
extern XIM
XmImGetXIM(Widget w)
{
    XmICStuff *stuff = _XmFindICStuff(w);

    if (stuff)
    {
	return stuff->xim;
    }

    /* FIX ME (see comment above) */
    return NULL;
}


/*
 * XmImMbLookupString returns a string composed in the locale associated with
 * the widget's input method and a KeySym that is currently mapped to the
 * keycode in the KeyPressedEvent.
 */
extern int
XmImMbLookupString(Widget w,
                   XKeyPressedEvent *evp,
                   char *buf,
                   int nbytes,
                   KeySym *keysym,
                   int *status)
{
    XmICStuff *stuff = _XmFindICStuff(w);
    XIC xic = NULL;

    if (stuff)
    {
	xic = stuff->xic;
    }

    if (xic)
    {
	int	r;
	char	*st;
	r = XmbLookupString(xic, evp, buf, nbytes, NULL, status);
	switch (*status) {
	case XLookupNone: st = "XLookupNone";
		break;
	case XLookupBoth: st = "XLookupBoth";
		break;
	case XLookupKeySym: st = "XLookupKeySym";
		break;
	case XLookupChars: st = "XLookupChars";
		break;
	case XBufferOverflow: st = "XBufferOverflow";
		break;
	default:
		st = "??";
	}
	DEBUGOUT(_LtDebug(__FILE__, w,
		"XmImMbLookupString -> %d (status %s) buf %c\n",
		r, st, buf[0]));
	return r;
    }

    if (status)
    {
	*status = XLookupBoth;
    }

    return XLookupString(evp, buf, nbytes, keysym, NULL);
}


/*
 * XmImSetXIC registers the X Input Context (XIC) with the specified widget.
 * Any existing XIC registered for the widget is unregistered. The new XIC
 * registered for the widget is returned.
 */
extern XIC
XmImSetXIC(Widget w,
           XIC xic)
{
    XmVendorShellExtObject v = (XmVendorShellExtObject)_LtFindVendorExt(w);
    XmICStuff *stuff;

    if (v == NULL || xic == NULL)
    {
	return NULL;		/* FIX ME - is this right ? */
    }

    XmImUnregister(w);

    if (_XmFindICStuff(w) == NULL)
    {
	stuff = (XmICStuff *)XtMalloc(sizeof(XmICStuff));
	stuff->ve = (Widget)v;
	stuff->text = w;
	stuff->xim = XIMOfIC(xic);
	stuff->xic = xic;
	stuff->count = 1;
	stuff->orig_xim = NULL;

	stuff->next = (XmICStuff *)v->vendor.im_info;
	v->vendor.im_info = (XtPointer)stuff;
    }

    return xic;
}

/*
 * XmImCloseXIM closes all input contexts associated with the input method of
 * the specified widget. The widget is used to identify the Display that specifies
 * the Input Method opened for the widget. Upon closure, all widgets registered
 * with the input contexts are unregistered. Also the Input Method specified by
 * Display is closed.
 */


/* FIX ME
 * Currently we only tackle stuff under our own shell widget.
 * This is not enough, as the above text (which reflects the Motif 2.0 spec) indicates
 * that we should deal with the "Display".
 * If more than one window is open on the same Display, then we need to find them.
 * This requires the XmDisplay to keep track of top level widgets, or of (Vendor)Shells.
 * FIX ME
 */

extern void
XmImCloseXIM(Widget w)
{
    XmICStuff *stuff;

    if (!w)
    {
	return;
    }
    if ((stuff = _XmFindICStuff(w)) == NULL)
    {
	return;
    }

    XmImUnregister(w);

    if (stuff->xic) {
	XDestroyIC(stuff->xic);
	stuff->xic = NULL;
    }

    /* Make sure to only close the XIM when this is the last reference to it */
    /* amai: I think one optimize that if clauses, but for now
             just try to avoid some segfaults :-)
	     We have too many "if (stuff->xim)" probably ...
	     FIX ME */
    if (stuff->orig_xim) {
	stuff->orig_xim->count--;
	if (stuff->orig_xim->count == 0) {
		if (stuff->xim)
		   XCloseIM(stuff->xim);
		DEBUGOUT(_LtDebug(__FILE__, w, "XCloseIM(%p)\n", stuff->xim));
		stuff->orig_xim->xim = NULL;
		/* stuff->orig_xim is now useless */
		XtFree(stuff->orig_xim);
	} else {
		DEBUGOUT(_LtDebug(__FILE__, w, "XmImCloseXIM(%p), count -> %d\n",
			stuff->xim, stuff->orig_xim->count));
	}
	stuff->xim = NULL;
	stuff->orig_xim = NULL;
    } else if (stuff->count != 0) {
	/* Someone else refers to us, just decrement count */
	stuff->count--;
	if (stuff->count == 0) {
		if (stuff->xim)
		   /* that if () avoids the crash upon closing File Open
		      or Goto Line dialogs within NEdit */
		   XCloseIM(stuff->xim);
		DEBUGOUT(_LtDebug(__FILE__, w, "XCloseIM(%p)\n", stuff->xim));
		stuff->xim = NULL;
	} else {
		DEBUGOUT(_LtDebug(__FILE__, w, "XmImCloseXIM(%p), count -> %d\n",
			stuff->xim, stuff->count));
	}
	stuff->xim = NULL;
    } else if (stuff->xim) {
	XCloseIM(stuff->xim);
	DEBUGOUT(_LtDebug(__FILE__, w, "XCloseIM(%p)\n", stuff->xim));
		stuff->orig_xim->xim = NULL;
	stuff->xim = NULL;
    }

    _XmFreeICStuff(w, stuff);
}


/*
 * XmImFreeXIC unregisters all widgets associated with the specified XIC. The
 * specified widget must be specified with the specified XIC.
 */
extern void
XmImFreeXIC(Widget w, XIC xic)
{
}

/* amai: Those _XmIm* are for MBC (=Motif Binary Compatibility?! */
extern void
_XmImChangeManaged(Widget w)
{
}


extern void
_XmImRealize(Widget w)
{
}


extern void
_XmImResize(Widget w)
{
}


extern void
_XmImRedisplay(Widget w)
{
}


extern void
XmImMbResetIC(Widget widget,
              char **mb)
{
/*
    XmICStuff *stuff;
    XIM xim;

    if (!widget)
    {
	return;
    }
    if ((stuff = _XmFindICStuff(widget)) == NULL)
    {
	return;
    }

    xim = stuff->xim;
*/
  _XmWarning(NULL, "XmImMbResetIC() is not yet implemented!");
}


/* Reconfig XIC for better use of input_method */
/* Only called if FontSet is changed and available */
static void
_XmImReconfigXIC(Widget w, XmICStuff *stuff, XFontSet fontset, XPoint *sp)
{
    /* Best style */
    XIMStyle input_style = XIMPreeditPosition | XIMStatusNothing;
    XIMStyle curr_style;
    XIC	old_xic = stuff->xic;
    XIC new_xic = NULL;

    XGetICValues(old_xic, XNInputStyle, &curr_style, NULL);
    if (curr_style & XIMPreeditPosition || curr_style & XIMPreeditArea)
    	return;	/* We already have a better input_style */

    {
	XRectangle rect;
	XRectangle status_rect;
	XVaNestedList preedit_attr = NULL;
	XVaNestedList status_attr = NULL;
	XPoint spot;
	unsigned long fg, bg;
	int		fontheight = 0;

	if (!sp)
	{
	    XFontStruct **foo;
	    char **bar;
	    int i, num;

	    num = XFontsOfFontSet(fontset, &foo, &bar);
	    for (i=0; i<num; i++){
		if (foo[i]->max_bounds.ascent + foo[i]->max_bounds.descent  > fontheight)
		    fontheight = foo[i]->max_bounds.ascent + foo[i]->max_bounds.descent;
	    }
	    spot.x = 0;
	    spot.y = fontheight;
	}
	else
	{
	    spot.x = sp->x;
	    spot.y = sp->y;
	}

        preedit_attr = XVaCreateNestedList(0, XNForeground, &fg,
					      XNBackground, &bg, NULL);
        XGetICValues(old_xic, XNPreeditAttributes, preedit_attr, NULL);

	{

            rect.x = XtX(w);
            rect.y = XtY(w);
            rect.width  = XtWidth(w);
            rect.height = XtHeight(w);

            preedit_attr = XVaCreateNestedList(0,
                                        XNArea, &rect,
                                        XNSpotLocation, &spot,
					XNForeground, fg,
					XNBackground, bg,
					XNFontSet, fontset,
                                        NULL);
	}

	new_xic = XCreateIC(stuff->xim,
			       XNInputStyle, input_style,
			       XNClientWindow, XtWindow(w),
			       XNFocusWindow, XtWindow(w),
                        preedit_attr ? XNPreeditAttributes : NULL, preedit_attr,
			       NULL);

	/* Try to use XIMPreeditArea */
	if (!new_xic)
	{

	    input_style = XIMPreeditArea | XIMStatusArea;

            rect.y = XtY(w) + XtHeight(w) - fontheight * 2;
            rect.height = fontheight;

            status_rect.x = XtX(w);
            status_rect.y = XtY(w) + XtHeight(w) - fontheight;
            status_rect.width  = XtWidth(w);
            status_rect.height = fontheight;

            preedit_attr = XVaCreateNestedList(0,
                                        XNArea, &rect,
					XNForeground, fg,
					XNBackground, bg,
					XNFontSet, fontset,
                                        NULL);

            status_attr = XVaCreateNestedList(0,
                                        XNArea, &status_rect,
					XNForeground, fg,
					XNBackground, bg,
					XNFontSet, fontset,
                                        NULL);
	}

	new_xic = XCreateIC(stuff->xim,
			       XNInputStyle, input_style,
			       XNClientWindow, XtWindow(w),
			       XNFocusWindow, XtWindow(w),
                        preedit_attr ? XNPreeditAttributes : NULL, preedit_attr,
                        status_attr ? XNStatusAttributes : NULL, status_attr,
			       NULL);

	XFree(preedit_attr);
	XFree(status_attr);

	if (new_xic)
	{
	    stuff->xic = new_xic;
	    XDestroyIC(old_xic);
	}
	else
	    return;

	if (input_style & XIMPreeditArea)
	{
		XRectangle rect;
		XVaNestedList preedit_attr = NULL;

		rect.x = 0; rect.y = 0; rect.width = 0; rect.height = 0;
    		preedit_attr = XVaCreateNestedList(0, XNAreaNeeded, &rect, NULL);
    		XSetICValues(stuff->xic, XNPreeditAttributes, preedit_attr, NULL);

		XFree(preedit_attr);
	}

    }

}
