/**
 *
 * $Id: BaseClass.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: BaseClass.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/DisplayP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/ScreenP.h>
#include <Xm/MenuShellP.h>
#include <Xm/DragIconP.h>
#include <Xm/DropTransP.h>
#include <Xm/VendorSEP.h>
#include <Xm/DropSMgrP.h>
#include <Xm/DialogS.h>

#include <X11/RectObjP.h>
#include <X11/Xutil.h>

#include <XmI/DebugUtil.h>


/* For the prototype police */

extern XmWrapperData	_XmGetWrapperData(WidgetClass wc);
extern void		_XmFreeWrapperData(XmWrapperData data);

/* Some global data */
XrmQuark XmQmotif = 0;
int _XmInheritClass = 0;
XmBaseClassExt *_Xm_fastPtr=0;

/*
 * credit where credit is due.  A good portion of the changes made today come
 * from a patch submitted by Harald Albrecht.  Don't blame him if you don't
 * like the coding style, though -- that's my fault (MLM).
 * Danny gets the credit for the XContext hint, though.
 */
extern XmGenericClassExt *
_XmGetClassExtensionPtr(XmGenericClassExt *listHeadPtr,
			XrmQuark owner)
{
    XmGenericClassExt *ext = listHeadPtr;

    while (ext && *ext && ((*ext)->record_type != owner))
    {
	ext = (XmGenericClassExt *)&((*ext)->next_extension);
    }

    return ext;
}


extern Boolean
_XmIsSlowSubclass(WidgetClass wc,
		  unsigned int bit)
{
    XmBaseClassExt *bce;

    bce = (XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif);

    if (!bce || !*bce)
    {
	return False;
    }

    if (_XmGetFlagsBit((*bce)->flags, bit))
    {
	return True;
    }

    return False;
}


/*
 * Check a widget class for being a real (core) LessTif widget class. We
 * consider application defined classes as not being standard (beep) widget
 * classes.
 */
extern Boolean
_XmIsStandardMotifWidgetClass(WidgetClass wc)
{
    XmBaseClassExt *bclass, *super;
    unsigned char *flags, *sflags;
    int i;

    /*
     * If the widget class in question has no extension record it is
     * apparently no real LessTif widget class.
     */
    bclass = (XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif);

    if ((bclass == NULL) || (*bclass == NULL))
    {
	return False;
    }

    super = (XmBaseClassExt *)_XmGetBaseClassExtPtr(wc->core_class.superclass,
						    XmQmotif);

    /*
     * If our superclass has no extension record then we may be either
     * a primitive or manager widget. But we're nice and just check for
     * one of the fast subclass bits set. If we find a bit set, we assume
     * for a standard motif class. But we consider only standard motif
     * flags and not the application defined bits from bit 129 upwards.
     */
    if ((super == NULL) || (*super == NULL))
    {
	flags = (*bclass)->flags;

	for (i = XmLAST_FAST_SUBCLASS_BIT << 3; i >= 0; i--)
	{
	    if (flags[i] != 0)
	    {
		return True;
	    }
	}

	return False;
    }

    /*
     * Now make sure that the fast subclass flags differ at least in one bit.
     * We only consider the standard motif flags, not the possible application
     * defined bits from bit 129 on.
     */
    flags = (*bclass)->flags;
    sflags = (*super)->flags;

    for (i = XmLAST_FAST_SUBCLASS_BIT << 3; i >= 0; i--)
    {
	if (flags[i] != sflags[i])
	{
	    return True;
	}
    }

    return False;
}


/*
 * according to Harald, this is for backwards compatibility only.  A local
 * function, BaseClassPartInitialize(), provides the functionality that
 * used to be here.
 */
extern void
_XmBaseClassPartInitialize(WidgetClass wc)
{
}

/* -----------------------------------------------
 * Widget extension stuff
 *
 * why do I always forget about XContexts?  Thanks, guys.
 * 022296 - Harald pointed out to me today that some of these contexts will
 * be used when the widget is unitialized, therefore XtWindow(widget) will
 * return NULL.  Not really a problem with the way they are currently used
 * (prehook push, posthook pop), but it will be a problem with, say,
 * VendorShellExt if we set the extension when the widget is unrealized, and
 * do a get when the widget is realized (the XID part of the context call will
 * be wrong, thus we won't get the right [if any] context data).  Also, since
 * they're unitialized, all will use the XID null value, and no longer be
 * unique.  He suggested that I might get around this problem by using the
 * widget as the XID, which should take care of the uniqueness problem.  Even
 * though the methods for, e.g., get_values/set_values have other Widget values,
 * we know that the program keeps the same value for the life of the widget,
 * so we should be okay.  Better not use this in class_part_initialize, though.
 * But then again, why would you...
 */
typedef struct _WidgetStack
{
    struct _WidgetStack *next;
    XmWidgetExtData data;
}
XmWidgetExtDataStackRec, *XmWidgetExtDataStackPtr;

/*
 * associate a context with an extension type.
 */
static XContext extContexts[XmDEFAULT_EXTENSION + 1] = { 0 };


static XContext
FindAssociatedContext(unsigned int extType)
{
    if (extType > XmDEFAULT_EXTENSION)
    {
	_XmError(NULL, "Bad extension type for WidgetExtData.");
    }

    if (extContexts[extType] != 0)
    {
	return extContexts[extType];
    }

    extContexts[extType] = XUniqueContext();

    return extContexts[extType];
}


/*
 * nondestructively fetch an Extension data record.  Hey Danny, this
 * looks like a good way to find the VendorShellExt without looping
 * through Composite children!  Whadaya think?
 */
extern XmWidgetExtData
_XmGetWidgetExtData(Widget widget, unsigned char extType)
{
    XContext c = FindAssociatedContext(extType);
    XmWidgetExtDataStackPtr node;

    if (XFindContext(XtDisplay(widget), (XID)widget, c, (XPointer *)&node) != 0)
    {
	return NULL;
    }

    return node->data;
}


extern void
_XmFreeWidgetExtData(Widget widget)
{
}


/*
 * apparently, these can be chained...
 */
extern void
_XmPushWidgetExtData(Widget widget,
		     XmWidgetExtData data,
		     unsigned char extType)
{
    XContext c = FindAssociatedContext(extType);
    XmWidgetExtDataStackPtr head = NULL, node;

    node = (XmWidgetExtDataStackPtr)XtCalloc(1,
					     sizeof(XmWidgetExtDataStackRec));
    node->data = data;

    XFindContext(XtDisplay(widget), (XID)widget, c, (XPointer *)&head);

    node->next = head;

    XSaveContext(XtDisplay(widget), (XID)widget, c, (XPointer)node);
}


extern void
_XmPopWidgetExtData(Widget widget,
		    XmWidgetExtData *dataRtn,
		    unsigned char extType)
{
    XContext c = FindAssociatedContext(extType);
    XmWidgetExtDataStackPtr node;

    if (XFindContext(XtDisplay(widget), (XID)widget, c, (XPointer *)&node) != 0)
    {
	/*
	 * Only print this message if the widget is not in the process
	 * of being destroyed.
	 * This is a case in which the message doesn't make sense...
	 */
	if (! widget->core.being_destroyed)
		_XmError(widget, "No ExtNode to pop!");

	*dataRtn = NULL;

	return;
    }

    if (node->next)
    {
	XSaveContext(XtDisplay(widget), (XID)widget, c,
		     (XPointer)node->next);
    }
    else
    {
	XDeleteContext(XtDisplay(widget), (XID)widget, c);
    }

    *dataRtn = node->data;

    XtFree((char *)node);
}


/* -------------------------------------------------------------
 * Wrapper functions.
 */
/*
 * Returns the top-most entry on the per-widget-class stack without
 * removing it from the stack. If the stack is empty or doesn't exists
 * yet, the entry and the stacks are created on the fly.
 */
XmWrapperData
_XmGetWrapperData(WidgetClass wc)
{
    XmBaseClassExt *ext, bce;

    ext = (XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif);

    if (*ext)
    {
	bce = *ext;

	if (bce->version < XmBaseClassExtVersion)
	{
	    return (XmWrapperData)NULL;
	}
    }
    else
    {
	bce = (XmBaseClassExt)XtCalloc(1, sizeof(XmBaseClassExtRec));

	bce->next_extension = NULL;
	bce->record_type = XmQmotif;
	bce->version = XmBaseClassExtVersion;
	bce->record_size = sizeof(XmBaseClassExtRec);

	*ext = bce;
    }

    /*
     * If the wrapper data is missing, create a clean wrapper on-the-fly.
     */
    if (bce->wrapperData == NULL)
    {
	bce->wrapperData = (XmWrapperData)XtCalloc(1, sizeof(XmWrapperDataRec));
    }

    return bce->wrapperData;
}


/*
 * This isn't really necessary! Better fold this into the source at the
 * appropiate places. Or make it a simple #define --aldi
 *
 * MLM: True, but I understand the sentiment.  The four functions taken
 * together "black box" the interface.  Good information hiding...
 */
extern void
_XmFreeWrapperData(XmWrapperData data)
{
    XtFree((char *)data);
}


/*
 * Push information about the hooked function pointer on a per-widget-class
 * stack.
 *
 * BTW - The _XmXXXWrapperData() functions should really be static, as
 * no-one outside this module should be allowed to call them. They are
 * considered private and no-one outside this module should ever need
 * them. --aldi
 *
 * I made Push and Pop static, but left Get and Free global, after testing
 * linkage with M*tif.
 */
static XmWrapperData
_XmPushWrapperData(WidgetClass wc)
{
    XmBaseClassExt *ext, bce;
    XmWrapperData wrapper = NULL;

    /*
     * Try to find the class extension record and if you can't find one,
     * create one. If we found one, then make sure, it has the right
     * version number. This way, a widget class with no base class ext.
     * record will get a fresh one just on-the-fly.
     */
    ext = (XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif);

    /*
     * Remark: As we do not hand over a NULL pointer to
     * _XmGetBaseClassExtPtr(), we never get back a NULL pointer. So de-
     * referencing the pointer returned by _XmGetBaseClassExtPtr() is
     * just fine here.
     */
    if (!*ext)
    {
	bce = (XmBaseClassExt)XtMalloc(sizeof(XmBaseClassExtRec));

	bce->next_extension = NULL;
	bce->record_type = XmQmotif;
	bce->version = XmBaseClassExtVersion;
	bce->record_size = sizeof(XmBaseClassExtRec);
	bce->wrapperData = NULL;

	*ext = bce;
    }

    bce = *ext;

    if (bce->version < XmBaseClassExtVersion)
    {
	return wrapper;
    }

    /*
     * If there is already some wrapper data in the list, then clone the
     * head wrapper entry of the list. Otherwise allocate a fresh one
     * filled with zeros.
     */
    if (bce->wrapperData != NULL)
    {
	wrapper = (XmWrapperData)XtMalloc(sizeof(XmWrapperDataRec));

	memcpy(wrapper, bce->wrapperData, sizeof(XmWrapperDataRec));

	wrapper->widgetClass = wc;
	wrapper->next = bce->wrapperData;
    }
    else
    {
	wrapper = (XmWrapperData)XtMalloc(sizeof(XmWrapperDataRec));

	memset(wrapper, 0, sizeof(XmWrapperDataRec));
    }

    bce->wrapperData = wrapper;

    return wrapper;
}


/*
 * Pop data from the per-widget-class stack. The wrapper data is *NOT*
 * freed but instead a pointer to it is returned to the caller. Thus, it's
 * the caller's responsiblity to free the data later on. And to say it
 * loud and clear: the wrapper is removen from the list but the memory
 * isn't freed.
 */
static XmWrapperData
_XmPopWrapperData(WidgetClass wc)
{
    XmBaseClassExt *ext;
    XmWrapperData wrapper;

    ext = (XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif);
    wrapper = (*ext)->wrapperData;

    if (wrapper)
    {
	(*ext)->wrapperData = wrapper->next;
    }

    return wrapper;
}


/* ---------------------------------------------------------------------
 * Following is the realize wrapper stuff. Whereas the wrapper system
 * for hooking the class_part_init, initialize, set_values and get_values
 * methods is somewhat... clean, the realize wrapper stuff is going
 * real dirty! The main reason why this had becoming so clumpsy is to
 * cope with the way realize methods may be chained up to a class'
 * superclass:
 * In principle, there are two ways of chaining. The first one needs
 * some more work but is also a clean way for chaining (`clean' in the
 * presence of the wrapper mechanism):
 *   - DON'T let core_class.realize initially point to your own realize
 *     method! Use XtInheritRealize instead for a start.
 *   - During your class_part_init method, core_class.realize has been
 *     resolved to point to your superclass' realize method. So save
 *     this pointer in a safe place (probably within your class record).
 *     Place a pointer to your own realize method into core_class.realize.
 *   - DON'T use your superclass' core_class.realize pointer but use
 *     the pointer saved in the step before.
 * Then in the ClassPartInitLeafWrapper we could have saved the realize
 * pointer to a wrapper data block and put a pointer to a wrapper into
 * core_class.realize. The wrapper then in turn had only to ask the widget
 * for its class and use the realize proc as specified in the wrapper for
 * that widget class.
 * FORGET IT! (sigh)
 * Almost 100% of the widget code available uses a second chaining method:
 *   - Put a pointer to your realize method into core_class.realize.
 *   - When chaining, just use core_class.super_class->core_class.realize.
 * While this is appearently easier to code it renders all attempts useless
 * to cleanly hook the realize proc. Sigh (once again, I'm repeating myself).
 * The only solution for this misery that don't break existing code is:
 * Each wrapper entry gets a distinct level number. That number identifies
 * on what level below the vendorShell widget class a widget class resides.
 * In order to assign that number to a wrapper we need a separate wrapper
 * for each level (that is what the RealizeWrapper<#> procs are good for).
 * This level number indicates which core_class.realize was intended to
 * be called.
 *
 * All these explanations do apply to the geometry_handler and resize
 * wrappers, too (sigh, once more...).
 */

/*
 * Allow for 7 levels of descendants below the vendor shell widget
 * class. This is an arbitrary setting, but compatible with M*tif.
 */
static void RealizeWrapper(Widget w, XtValueMask *ValueMask,
			   XSetWindowAttributes *Attribs,
			   int IntentedWrapperDepth);
/*
 * Almost the same as above, but now for the resize stuff.
 */
static void ResizeWrapper(Widget w, int IntentedWrapperDepth);

/*
 * Just as above, but now for the geometry handler stuff.
 */
static XtGeometryResult GeometryHandlerWrapper(Widget w,
					       XtWidgetGeometry *request,
					       XtWidgetGeometry *reply,
					       int IntentedWrapperDepth);

/*
 * max depths for the wrappers
 */
#define MAX_REALIZE_WRAPPER_DEPTH	7
#define MAX_RESIZE_WRAPPER_DEPTH	10
#define MAX_GEOM_WRAPPER_DEPTH		9

/*
 * shorthand
 */
#define MAKE_REALIZE_WRAPPER(Level) \
    static void RealizeWrapper##Level(Widget w, XtValueMask *ValueMask, \
                                      XSetWindowAttributes *Attribs) \
    { RealizeWrapper(w, ValueMask, Attribs, Level); }

#define MAKE_RESIZE_WRAPPER(Level) \
    static void ResizeWrapper##Level(Widget w) \
    { ResizeWrapper(w, Level); }


#define MAKE_GEOM_WRAPPER(Level) \
    static XtGeometryResult GeometryHandlerWrapper##Level(Widget w, \
                                              XtWidgetGeometry *request, \
                                              XtWidgetGeometry *reply) \
    { return GeometryHandlerWrapper(w, request, reply, Level); }

/*
 * definitions
 */
MAKE_REALIZE_WRAPPER(0)
MAKE_REALIZE_WRAPPER(1)
MAKE_REALIZE_WRAPPER(2)
MAKE_REALIZE_WRAPPER(3)
MAKE_REALIZE_WRAPPER(4)
MAKE_REALIZE_WRAPPER(5)
MAKE_REALIZE_WRAPPER(6)
MAKE_REALIZE_WRAPPER(7)

MAKE_RESIZE_WRAPPER(0)
MAKE_RESIZE_WRAPPER(1)
MAKE_RESIZE_WRAPPER(2)
MAKE_RESIZE_WRAPPER(3)
MAKE_RESIZE_WRAPPER(4)
MAKE_RESIZE_WRAPPER(5)
MAKE_RESIZE_WRAPPER(6)
MAKE_RESIZE_WRAPPER(7)
MAKE_RESIZE_WRAPPER(8)
MAKE_RESIZE_WRAPPER(9)
MAKE_RESIZE_WRAPPER(10)

MAKE_GEOM_WRAPPER(0)
MAKE_GEOM_WRAPPER(1)
MAKE_GEOM_WRAPPER(2)
MAKE_GEOM_WRAPPER(3)
MAKE_GEOM_WRAPPER(4)
MAKE_GEOM_WRAPPER(5)
MAKE_GEOM_WRAPPER(6)
MAKE_GEOM_WRAPPER(7)
MAKE_GEOM_WRAPPER(8)
MAKE_GEOM_WRAPPER(9)

static XtRealizeProc RealizeWrappers[] =
{
    RealizeWrapper0,
    RealizeWrapper1,
    RealizeWrapper2,
    RealizeWrapper3,
    RealizeWrapper4,
    RealizeWrapper5,
    RealizeWrapper6,
    RealizeWrapper7
};

static XtWidgetProc ResizeWrappers[] =
{
    ResizeWrapper0,
    ResizeWrapper1,
    ResizeWrapper2,
    ResizeWrapper3,
    ResizeWrapper4,
    ResizeWrapper5,
    ResizeWrapper6,
    ResizeWrapper7,
    ResizeWrapper8,
    ResizeWrapper9,
    ResizeWrapper10
};

static XtGeometryHandler GeometryWrappers[] =
{
    GeometryHandlerWrapper0,
    GeometryHandlerWrapper1,
    GeometryHandlerWrapper2,
    GeometryHandlerWrapper3,
    GeometryHandlerWrapper4,
    GeometryHandlerWrapper5,
    GeometryHandlerWrapper6,
    GeometryHandlerWrapper7,
    GeometryHandlerWrapper8,
    GeometryHandlerWrapper9
};


/*
 * Returns a widget class' depth in relation to the vendor shell widget
 * class. Any direct subclass of the vendor shell widget class is
 * considered to be of depth #1. Any widget class not derived from the
 * vendor shell class will result in an error message. This is much
 * securer than bombing later with a NULL pointer or even worse getting
 * strange results and no-one knows why...
 */
static int
RealizeDepth(WidgetClass MyWc)
{
    int depth = 0;
    WidgetClass wc = MyWc;

    while ((wc != NULL) && (wc != (WidgetClass)vendorShellWidgetClass))
    {
	depth++;
	wc = wc->core_class.superclass;
    }

    if (wc == NULL)
    {
	_XmError(NULL,
		 "PANIC: widget class \"%s\" tried to take part in the\n"
		 "realize posthook mechanism but is not a descendant class of\n"
		 "vendorShellWidgetClass.",
		 MyWc->core_class.class_name);
    }

    if (depth > MAX_REALIZE_WRAPPER_DEPTH)
    {
	_XmError(NULL,
		 "SORRY: widget class \"%s\" is subclassed too deep from\n"
		 "the vendorShellWidgetClass. Current depth is %i whereas the\n"
		 "allowed maximum depth is %d.",
		 MyWc->core_class.class_name,
		 depth, MAX_REALIZE_WRAPPER_DEPTH);
    }

    return depth;
}


/*
 * Almost the same as the previous function, but this time the levels
 * are the distance from the rectObject widget class.
 */
static int
ResizeDepth(WidgetClass MyWc)
{
    int depth = 0;
    WidgetClass wc = MyWc;

    while ((wc != NULL) && (wc != (WidgetClass)rectObjClass))
    {
	depth++;
	wc = wc->core_class.superclass;
    }

    if (wc == NULL)
    {
	/* yes, there are commas missing in the following lines.  No, it's
	 * not a mistake.  Let the compiler join the lines.
	 */
	_XmError(NULL,
		 "PANIC: widget class \"%s\" tried to take part in the\n"
		 "resize posthook mechanism but is not a descendant class of\n"
		 "rectObject. Something strange is happening!",
		 MyWc->core_class.class_name);
    }

    if (depth >MAX_RESIZE_WRAPPER_DEPTH)
    {
	_XmError(NULL,
		 "SORRY: widget class \"%s\" is subclassed too deep from\n"
		 "the rectObject widget class. Current depth is %i whereas\n"
		 "the allowed maximum depth is %d.",
		 MyWc->core_class.class_name,
		 depth, MAX_RESIZE_WRAPPER_DEPTH);
    }

    return depth;
}


/*
 * Same as above, but this time for the geometry handler stuff.
 */
static int
GeomDepth(WidgetClass MyWc)
{
    int depth = 0;
    WidgetClass wc = MyWc;

    while ((wc != NULL) && (wc != (WidgetClass)rectObjClass))
    {
	depth++;
	wc = wc->core_class.superclass;
    }

    if (wc == NULL)
    {
	_XmError(NULL,
		 "PANIC: widget class \"%s\" tried to take part in the\n"
		 "geometry manager posthook mechanism but is not a descendant\n"
		 "class of rectObject. Something strange is happening!",
		 MyWc->core_class.class_name);
    }

    if (depth >MAX_GEOM_WRAPPER_DEPTH)
    {
	_XmError(NULL,
		 "SORRY: widget class \"%s\" is subclassed too deep from\n"
		 "the rectObject widget class. Current depth is %i whereas\n"
		 "the allowed maximum depth is %d.",
		 MyWc->core_class.class_name,
		 depth, MAX_GEOM_WRAPPER_DEPTH);
    }

    return depth;
}


/*
 * Initialize the base class extension record of a widget class. The base
 * class extension record specifies various hooks and other nasty things.
 * And a widget class can inherit these nasties from its ancestors. This
 * what we implement here. You know: the failures of the parents will be
 * punished 'til the fifth generation...
 *
 * This function is private to this module! No need to export it... or
 * someone else may really call it --aldi
 *
 * The next static extension record just serves as a reservoir for zeros
 * (respective NULLs). It is used whenever a widget's superclass has no
 * extension record.
 */
static XmBaseClassExtRec DummySuperClassExtension = { 0 };

static void (*ObjectClassPartInit)(WidgetClass c) = NULL;
static Boolean (*ObjectSetValues)(Widget old, Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args) = NULL;


/*
 * Much the same as XtIsSubclass(). But this time it checks whether a
 * widget *class* is a subclass of some other class. This function is
 * useful not only to us, but also to others. That may be the reason why
 * M*tif 2.0 now exports it.
 */
Boolean
_XmIsSubclassOf(WidgetClass wc, WidgetClass sclass)
{
    WidgetClass parent = wc;

    while (parent && parent != sclass)
    {
	parent = parent->core_class.superclass;
    }

    if (parent == sclass)
    {
	return True;
    }

    return False;
}


/*
 * figure out if this widget class is one of those that needs inheritance
 * wrapper help.
 */
static void
ResolveWrappers(WidgetClass wc, WidgetClass sclass)
{
    XmWrapperData Wrapper, SuperWrapper;

    Wrapper = _XmGetWrapperData(wc);
    SuperWrapper = _XmGetWrapperData(sclass);

    /*
     * Only the vendor shell class and its subclasses will take part in
     * the realize wrapper game...
     */
    if (_XmIsSubclassOf(wc, vendorShellWidgetClass))
    {

	/*
	 * If the widget class wants to inherit the realize method from
	 * its superclass then use the function pointer stored with the
	 * superclass' extension record. Otherwise use the pointer as
	 * specified in the class record and store it in the extension
	 * record. The extension record will always hold the real pointer
	 * whereas the core_class.realize field points to a wrapper.
	 * NOTE: we assert here vendorShellWidgetClass->core_class.realize
	 *       is NOT (to repeat: NOT) set to XtInheritRealize!!!
	 */
	if (wc->core_class.realize == XtInheritRealize)
	{
	    if (SuperWrapper->realize)
	    {
		Wrapper->realize = SuperWrapper->realize;
	    }
	    else
	    {
		Wrapper->realize = sclass->core_class.realize;
	    }
	}
	else
	{
	    Wrapper->realize = wc->core_class.realize;
	}

	wc->core_class.realize = RealizeWrappers[RealizeDepth(wc)];
    }

    if (_XmIsSubclassOf(wc, rectObjClass))
    {

	if (wc->core_class.resize == XtInheritResize)
	{
	    if (SuperWrapper->resize)
	    {
		Wrapper->resize = SuperWrapper->resize;
	    }
	    else
	    {
		Wrapper->resize = sclass->core_class.resize;
	    }
	}
	else
	{
	    Wrapper->resize = wc->core_class.resize;
	}

	wc->core_class.resize = ResizeWrappers[ResizeDepth(wc)];
    }

#define C_C(wc) ((CompositeClassRec *) wc)

    if (_XmIsSubclassOf(wc, compositeWidgetClass))
    {
	if (C_C(wc)->composite_class.geometry_manager ==
	    XtInheritGeometryManager)
	{
	    if (SuperWrapper->geometry_manager)
	    {
		Wrapper->geometry_manager = SuperWrapper->geometry_manager;
	    }
	    else
	    {
		Wrapper->geometry_manager =
		    C_C(sclass)->composite_class.geometry_manager;
	    }
	}
	else
	{
	    Wrapper->geometry_manager =
		C_C(wc)->composite_class.geometry_manager;
	}

	C_C(wc)->composite_class.geometry_manager =
	    GeometryWrappers[GeomDepth(wc)];
    }
#undef C_C
}


static XmBaseClassExt *
BaseClassPartInitialize(WidgetClass wc)
{
    XmBaseClassExt *ext = NULL, *sext = NULL;
    XmBaseClassExt new_ext, super_ext;
    Boolean need_ext = False;
    WidgetClass sclass;
    Boolean need_wrappers = False;

    /*
     * Some widget classes desperatly need a base class extension record
     * despite the fact that their superclass has no extension record.
     * The widget classes concerned of that problem are those with
     * superclasses which are  Xt 'kernel' classes. And the Xt intrinsics
     * have no knowledge of the dreaded things we're doing here... Sigh,
     * in former times all things were better!
     *
     * --aldi: Testing M*tif's bce records using the wctest programs shows
     * that the list of widget classes below is more than sufficient. Running
     * wctest indicates that all these widget classes have their own bce
     * record anyway. So the test below is overcautious -- at least when
     * LessTif has reached a certain quality and all those widget classes
     * have their own bce records attached to their class records.
     */
    if (wc == (WidgetClass)&vendorShellClassRec ||	/* wmShell:: */
	wc == (WidgetClass)&xmPrimitiveClassRec ||	/* core:: */
	wc == (WidgetClass)&xmGadgetClassRec ||		/* rectObj:: */
	wc == (WidgetClass)&xmManagerClassRec ||	/* constraint:: */
	wc == (WidgetClass)&xmDisplayClassRec ||	/* applicationShell:: */
	wc == (WidgetClass)&xmScreenClassRec ||		/* core:: */
	wc == (WidgetClass)&xmMenuShellClassRec ||	/* overrideShell:: */
	wc == (WidgetClass)&xmExtClassRec)		/* object:: */
    {
	need_ext = True;
    }
    else if (wc == (WidgetClass)&rectObjClassRec ||
	     wc == (WidgetClass)&compositeClassRec)
    {
	need_wrappers = True;
    }

    /*
     * Ask for a base class extension record of the widget class to be
     * initialized. Also ask the superclass for its base class extension
     * record.
     */
    sclass = wc->core_class.superclass;

    ext = _XmGetBaseClassExtPtr(wc, XmQmotif);

    /*
     * If someone really should instanciate an Object...
     */
    if (sclass)
    {
	sext = _XmGetBaseClassExtPtr(sclass, XmQmotif);
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "BaseClassPartInit on %s with %s base ext rec.\n",
		      wc->core_class.class_name,
		      (ext && *ext) ? "a" : "NO!"));

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "BaseClassPartInit: sclass %s with %s base ext rec.\n",
		      wc->core_class.superclass->core_class.class_name,
		      (sext && *sext) ? "a" : "NO!"));

    /*
     * If the superclass doesn't have a base class extension record and
     * our widget class hasn't a base class extension record either and
     * it doesn't need one at all, say goodby to it all! We do hereby
     * sort out all those crazy widget classes which have an extension
     * record but are not subclassed of any LessTif widget class.
     */
    if (!need_ext)
    {

	if (need_wrappers)
	{
	    ResolveWrappers(wc, sclass);
	    return ext;
	}

	if (!sext || !*sext)
	{

	    /*
	     * Unfortunatly, the set_values, get_values and initialize
	     * hooks may trigger nevertheless.
	     */
	    if (ext && *ext)
	    {
		_XmWarning(NULL,
			   "Widget class %s has a base extension record, but\n\
			its superclass %s has not. Using the hook mechanism\n\
			will cause trouble.\n",
			   wc->core_class.class_name,
			   wc->core_class.superclass->core_class.class_name);

		return (XmBaseClassExt *)NULL;
	    }
	}
    }

    /*
     * The next one should never happen! Or something very bad is going on.
     */
    if (!ext)
    {
	return (XmBaseClassExt *)NULL;
    }

    if (!*ext)
    {
	/*
	 * In case this widget class has no class extension record
	 * but needs one for LessTif's sake, set up a base class
	 * extension record and therefore force the use of default
	 * hook procedures.
	 * The whole bunch here is necessary in order to support application
	 * defined widget classes which are inherited from widget classes
	 * of the LessTif 'core'. So every user contributed class gets
	 * a base class extension record if it does not already have one.
	 *
	 * MLM: Is this really true?  Run-time testing of TearOffButton seemed
	 * to indicate that this class had no base extension...
	 */
	XmBaseClassExt p;
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "BaseClassPartInit(): %s got bce record on the fly\n",
			  wc->core_class.class_name));

	*ext = p = (XmBaseClassExt)XtCalloc(sizeof(XmBaseClassExtRec), 1);
	p->record_type = XmQmotif;
	p->version = XmBaseClassExtVersion;
	p->record_size = sizeof(XmBaseClassExtRec);
	p->classPartInitPrehook = XmInheritClassPartInitPrehook;
	p->classPartInitPosthook = XmInheritClassPartInitPosthook;
	p->initializePrehook = XmInheritInitializePrehook;
	p->initializePosthook = XmInheritInitializePosthook;
	p->setValuesPrehook = XmInheritSetValuesPrehook;
	p->setValuesPosthook = XmInheritSetValuesPosthook;
	p->getValuesPrehook = XmInheritGetValuesPrehook;
	p->getValuesPosthook = XmInheritGetValuesPosthook;
	p->secondaryObjectClass = XmInheritClass;
	p->secondaryObjectCreate = XmInheritSecObjectCreate;
	p->getSecResData = XmInheritGetSecResData;
	p->widgetNavigable = XmInheritWidgetNavigable;
	p->focusChange = XmInheritFocusChange;
    }

    /*
     * Let us be notorious and always clean the flags. The flags have
     * to be set during the class_part_init method of a widget class.
     */
    memset((*ext)->flags, 0, 32);

    /*
     * If the superclass hasn't a base class extension record, use
     * a dummy record filled with NULLs. This way we never end up with
     * unintentionally unresolved XtInherits. On the other side the
     * message XtInherit spits out when it gets accidently called may
     * help more than a silent ignore.
     */
    new_ext = *ext;

    if (!*sext)
    {
	super_ext = &DummySuperClassExtension;
    }
    else
    {
	super_ext = *sext;

	if (super_ext == NULL)
	{
	    super_ext = &DummySuperClassExtension;
	}
    }

    if (new_ext->classPartInitPrehook == XmInheritClassPartInitPrehook)
    {
	new_ext->classPartInitPrehook = super_ext->classPartInitPrehook;
    }

    if (new_ext->classPartInitPosthook == XmInheritClassPartInitPosthook)
    {
	new_ext->classPartInitPosthook = super_ext->classPartInitPosthook;
    }

    if (new_ext->initializePrehook == XmInheritInitializePrehook)
    {
	new_ext->initializePrehook = super_ext->initializePrehook;
    }

    if (new_ext->initializePosthook == XmInheritInitializePosthook)
    {
	new_ext->initializePosthook = super_ext->initializePosthook;
    }

    if (new_ext->setValuesPrehook == XmInheritSetValuesPrehook)
    {
	new_ext->setValuesPrehook = super_ext->setValuesPrehook;
    }

    if (new_ext->setValuesPosthook == XmInheritSetValuesPosthook)
    {
	new_ext->setValuesPosthook = super_ext->setValuesPosthook;
    }

    if (new_ext->secondaryObjectClass == XmInheritClass)
    {
	new_ext->secondaryObjectClass = super_ext->secondaryObjectClass;
    }

    if (new_ext->secondaryObjectCreate == XmInheritSecObjectCreate)
    {
	new_ext->secondaryObjectCreate = super_ext->secondaryObjectCreate;
    }

    if (new_ext->getValuesPrehook == XmInheritGetValuesPrehook)
    {
	new_ext->getValuesPrehook = super_ext->getValuesPrehook;
    }

    if (new_ext->getValuesPosthook == XmInheritGetValuesPosthook)
    {
	new_ext->getValuesPosthook = super_ext->getValuesPosthook;
    }

    if (new_ext->widgetNavigable == XmInheritWidgetNavigable)
    {
	new_ext->widgetNavigable = super_ext->widgetNavigable;
    }

    if (new_ext->focusChange == XmInheritFocusChange)
    {
	new_ext->focusChange = super_ext->focusChange;
    }

    if (new_ext->getSecResData == XmInheritGetSecResData)
    {
	new_ext->getSecResData = super_ext->getSecResData;
    }

    ResolveWrappers(wc, sclass);

    return ext;
}


/*
 * A wrapper shrinked over a widget class' realize procedure. This is
 * called from the RealizeWrapper<#> funcions with the last param
 * "WrapperDepth" specifying the wrapper number the cpu was taking.
 */
static void
RealizeWrapper(Widget w, XtValueMask *ValueMask,
	       XSetWindowAttributes *Attribs, int IntentedWrapperDepth)
{
    int WindUp;
    WidgetClass wc = XtClass(w);
    XmWidgetExtData ext;
    XmWrapperData wrap;

    /*
     * If someone tries to fool us, return immediatly. This is just
     * defensive programming... and will effectively hide the real
     * cause of the core dump suddenly appearing lateron...
     */
    if (!XtIsSubclass(w, vendorShellWidgetClass))
    {
	return;
    }

    /*
     * Call the real realize procedure. We can't never stumple over
     * a XtInherit pointer because the vendor shell widget class has
     * its own realize procedure. NULL pointers aren't allowed either.
     * Well, in theory _XmGetWrapperData may fail to locate a valid
     * wrapper data block and create one on the fly which contains
     * only NULL pointers. Thus, I'm defensive once again and catch
     * such catastrophies.
     */
    WindUp = RealizeDepth(wc) - IntentedWrapperDepth;

    if (WindUp > 0)
    {
	do
	{
	    wc = wc->core_class.superclass;
	}
	while (--WindUp > 0);
    }

    wrap = _XmGetWrapperData(wc);
    if (wrap == NULL || wrap->realize == NULL)
    {
	_XmError(w, "PANIC: no realize procedure specified for this widget.");
    }

    wrap->realize(w, ValueMask, Attribs);

    ext = _XmGetWidgetExtData(w, XmSHELL_EXTENSION);

/*    if (ext != NULL && ext->widget != NULL && !XmIsDialogShell(w)) */
    if (ext != NULL && ext->widget != NULL && wc != xmDialogShellWidgetClass)
    {
	/*
	 * Check for a callback list to be executed (the only exception
	 * is the XmDialogShell class which has no such callback list).
	 */
	XtCallCallbackList(ext->widget,
			   VSEP_RealizeCallback(ext->widget),
			   NULL);
    }
}


static void
ResizeWrapper(Widget w, int IntentedWrapperDepth)
{
    int WindUp;
    WidgetClass wc = XtClass(w);
    XmWrapperData WrapperData;
    Boolean need_navig = False;
    static Boolean recurse = False;

    WindUp = ResizeDepth(wc) - IntentedWrapperDepth;

    if (WindUp > 0)
    {
	do
	{
	    wc = wc->core_class.superclass;
	}
	while (--WindUp > 0);
    }

    /* _XmNavigResize is only used for shell widgets */
    if (XtParent(w) != NULL && XtIsShell(XtParent(w)))
    {
	need_navig = True;
    }

    /*
     * For the resize wrapper it is just fine to have no resize method.
     */
    WrapperData = _XmGetWrapperData(wc);

    if (WrapperData != NULL && WrapperData->resize != NULL)
    {
	if (!recurse && _XmDropSiteWrapperCandidate(w))
	{
	    recurse = True;

	    XmDropSiteStartUpdate(w);
	    WrapperData->resize(w);
	    XmDropSiteEndUpdate(w);

	    recurse = False;
	}
	else
	{
	    WrapperData->resize(w);
	}
    }

    if (need_navig)
    {
	_XmNavigResize(w);
    }
}


/*
 * fun with GeoUtils showed me why this was broken.  Remember how a 
 * geometry manager is called with the child requesting the change?
 * Well, guess which Widget class we should use....  The parent's!
 */
static XtGeometryResult
GeometryHandlerWrapper(Widget w, XtWidgetGeometry *request,
		       XtWidgetGeometry *reply, int IntentedWrapperDepth)
{
    int WindUp;
    WidgetClass wc = XtClass(XtParent(w));
    XmWrapperData WrapperData;
    static Boolean recurse = False;
    XtGeometryResult ret;

    DEBUGOUT(_LtDebug(__FILE__, w, "GeometryHandlerWrapper(%d) : request %s\n",
		      IntentedWrapperDepth, _LtDebugWidgetGeometry2String(request)));

    WindUp = GeomDepth(wc) - IntentedWrapperDepth;

    if (WindUp > 0)
    {
	do
	{
	    wc = wc->core_class.superclass;
	}
	while (--WindUp > 0);
    }

    WrapperData = _XmGetWrapperData(wc);

    if (WrapperData == NULL || WrapperData->geometry_manager == NULL)
    {
	_XmError(XtParent(w),
		 "PANIC: no geometry_manager procedure "
		 "specified for this widget: %s:%s.",
		 XtClass(XtParent(w))->core_class.class_name,
		 XtName(XtParent(w)));
    }

    if (!recurse && _XmDropSiteWrapperCandidate(w))
    {
	recurse = True;

	XmDropSiteStartUpdate(w);
	ret = WrapperData->geometry_manager(w, request, reply);
	XmDropSiteEndUpdate(w);

	recurse = False;
    }
    else
    {
	ret = WrapperData->geometry_manager(w, request, reply);
    }

    return ret;
}


/* ---------------------------------------------------------------------
 * Following is the implementation of the hook wrapper mechanism. In
 * order to do pre- and post-processing before/after a widget's class
 * methods have been called a wrapper mechanism is used: every wrapper
 * consists of a root and a leaf function. The root function replaces
 * whatever method installed in the object class and is setup by
 * _XmInitializeExtensions(). The pointers to the old methods are
 * stored in some global variables in order to call them later in the
 * root wrapper level. That root wrapper then checks for a pre-hook and
 * call it if one is available. The root wrapper also replaces the
 * function pointer to a class method that would have been called as
 * the last method by a pointer to the leaf wrapper. In turn, the leaf
 * wrapper then calls that last method, restores the original function
 * pointer and calls the post-hook function. Any questions remaining...?
 */


/*
 * This is the ClassPartInit method's post-hook. It calls the last
 * ClassPartInit method in the chain and then restores the original
 * function pointer to that particular last method.
 */
static void
ClassPartInitLeafWrapper(WidgetClass widget_class)
{
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(widget_class, XmQmotif));

    if (!bce || !*bce)
    {
	return;
    }

    wrap = (XmWrapperData)(*bce)->wrapperData;

    if (wrap->classPartInitLeaf)
    {
	(wrap->classPartInitLeaf)(widget_class);
    }

    if ((*bce)->classPartInitPosthook)
    {
	((*bce)->classPartInitPosthook)(widget_class);
    }

    widget_class->core_class.class_part_initialize = wrap->classPartInitLeaf;

    wrap->classPartInitLeaf = NULL;
}


/*
 * This ClassPartInit pre-hook is called as the first method whenever a
 * new widget class gets initialized.
 *
 * HEY! What about recursion?! I hope that no-one ever initializes
 * another widget class from within his class part init method (and that
 * widget class is a subclass of the current widget class).
 */
static void
ClassPartInitRootWrapper(WidgetClass widget_class)
{
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;

    bce = BaseClassPartInitialize(widget_class);

    if (!bce || !*bce)
    {
	if (ObjectClassPartInit)
	{
	    (ObjectClassPartInit)(widget_class);
	}

	return;
    }

    if ((*bce)->classPartInitPrehook)
    {
	((*bce)->classPartInitPrehook)(widget_class);
    }

    if (ObjectClassPartInit)
    {
	(ObjectClassPartInit)(widget_class);
    }

    if ((*bce)->classPartInitPosthook)
    {
	wrap = _XmGetWrapperData(widget_class);

	if (widget_class->core_class.class_part_initialize !=
	    ClassPartInitLeafWrapper)
	{

	    wrap->classPartInitLeaf =
		widget_class->core_class.class_part_initialize;

	    widget_class->core_class.class_part_initialize =
		ClassPartInitLeafWrapper;
	}
    }
}


/*
 * Initialize posthook stuff.
 * it is called just before the last init has been called and then redirects
 * the control to the last one before activating the post-hook.
 */
static void
InitializeLeafWrapper(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    WidgetClass wc = XtClass(new_w);
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif));

    if (!bce || !*bce)
    {
	return;
    }

    if ((*bce)->initializePosthook)
    {

	/*
	 * If we're not a shell and our parent is constraint, then
	 * the hook was installed in the class record's constraint part of
	 * our parent.
	 * MLM:  Ahhh.  Somebody said something about this, but I missed the
	 * real meaning.
	 */
	if (!XtIsShell(new_w) && XtParent(new_w) &&
	    XtIsConstraint(XtParent(new_w)))
	{
	    wc = XtClass(XtParent(new_w));

	    wrap = _XmPopWrapperData(wc);

	    ((ConstraintClassRec *)wc)->constraint_class.initialize =
		wrap->initializeLeaf;
	}
	else
	{
	    wrap = _XmPopWrapperData(wc);

	    wc->core_class.initialize = wrap->initializeLeaf;
	}

	if (wrap->initializeLeaf)
	{
	    (wrap->initializeLeaf)(request, new_w, args, num_args);
	}

	((*bce)->initializePosthook)(request, new_w, args, num_args);

	_XmFreeWrapperData(wrap);
    }
}


/*
 * The initialze pre-hook is called on any fresh widget instance before
 * any other initialize method has ever seen this new_w widget.
 */
static void
InitializeRootWrapper(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    WidgetClass wc = XtClass(new_w);
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif));

    if (!bce || !*bce)
    {
	return;
    }

    /*
     * If there is a pre-hook... call it! And if there's a post-hook
     * hook it up! But don't forget to check for a constraint parent.
     * In this case the parent's constraint initialize method will
     * be called after the widget's initialize method (+ superclasses)
     * has been called.
     */
    if ((*bce)->initializePrehook)
    {
	((*bce)->initializePrehook)(request, new_w, args, num_args);
    }

    if ((*bce)->initializePosthook)
    {
	if (!XtIsShell(new_w) && XtParent(new_w) &&
	    XtIsConstraint(XtParent(new_w)))
	{
	    wc = XtClass(XtParent(new_w));
	    wrap = _XmPushWrapperData(wc);

	    wrap->initializeLeaf =
		((ConstraintClassRec *)wc)->constraint_class.initialize;

	    ((ConstraintClassRec *)wc)->constraint_class.initialize
		= InitializeLeafWrapper;
	}
	else
	{
	    wrap = _XmPushWrapperData(wc);

	    wrap->initializeLeaf = wc->core_class.initialize;

	    wc->core_class.initialize = InitializeLeafWrapper;
	}
    }
}


/*
 * This is the set_values post-hook. It is called as the last set_values
 * method in the chain and in turn calls the real last set_values method
 * and the post-hook. 'nuff said.
 */
static Boolean
SetValuesLeafWrapper(Widget old, Widget request, Widget new_w,
		     ArgList args, Cardinal *num_args)
{
    WidgetClass wc = XtClass(new_w);
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;
    Boolean refresh = False;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif));

    if (!bce || !*bce)
	return False;

    if ((*bce)->setValuesPosthook)
    {
	if (!XtIsShell(new_w) && XtParent(new_w) &&
	    XtIsConstraint(XtParent(new_w)))
	{
	    wrap = _XmPopWrapperData(XtClass(XtParent(new_w)));
	    wc = XtClass(XtParent(new_w));
	    (((ConstraintClassRec *)wc)->constraint_class.set_values)
		= wrap->setValuesLeaf;
	}
	else
	{
	    wrap = _XmPopWrapperData(wc);

	    wc->core_class.set_values = wrap->setValuesLeaf;
	}

	if (wrap->setValuesLeaf != NULL)
	{
	    refresh = wrap->setValuesLeaf(old, request, new_w, args, num_args);
	}

	refresh |= (*bce)->setValuesPosthook(old, request, new_w,
					     args, num_args);

	_XmFreeWrapperData(wrap);
    }

    return refresh;
}


/*
 * This SetValues pre-hook is called before any set_values methods whenever
 * a XtSetValues() call occures. We then call the registered pre-hook for
 * this widget class -- if there is one. And if there is a registered
 * post-hook we'll install a special post-hook proc. This proc is then
 * called as the last set_values method and calls then the original
 * set_values handler as well as the post-hook.
 */
static Boolean
SetValuesRootWrapper(Widget old, Widget request, Widget new_w,
		     ArgList args, Cardinal *num_args)
{
    WidgetClass wc = XtClass(request);
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;
    Boolean refresh = False;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif));

    if (!bce || !*bce)
    {
	if (ObjectSetValues)
	{
	    return (ObjectSetValues) (old, request, new_w, args, num_args);
	}
    }

    if ((*bce)->setValuesPrehook)
    {
	refresh = ((*bce)->setValuesPrehook)(old, request, new_w,
					     args, num_args);
    }

    /*
     * If this widget has a constraint resources, then the
     * setValues method of our parent will be called after all the
     * setValues methods of our class and superclasses. In that
     * case we have to hook in the setValues method of the parent.
     */
    if ((*bce)->setValuesPosthook)
    {
	if (!XtIsShell(new_w) && XtParent(new_w) &&
	    XtIsConstraint(XtParent(new_w)))
	{
	    wrap = _XmPushWrapperData(XtClass(XtParent(new_w)));

	    wc = XtClass(XtParent(new_w));

	    wrap->setValuesLeaf
		= ((ConstraintClassRec *)wc)->constraint_class.set_values;

	    (((ConstraintClassRec *)wc)->constraint_class.set_values)
		= SetValuesLeafWrapper;
	}
	else
	{
	    wrap = _XmPushWrapperData(wc);

	    wrap->setValuesLeaf = wc->core_class.set_values;

	    wc->core_class.set_values = SetValuesLeafWrapper;
	}
    }

    if (ObjectSetValues)
    {
	refresh |= (ObjectSetValues)(old, request, new_w, args, num_args);
    }

    return refresh;
}


/*
 * The GetValues post-hook is called after any get_values_hook methods of
 * the current widget's class and superclasses. But it is called BEFORE any
 * potential parent's constraint get_values_hook method. Yeah, what a
 * design!
 */
static void
GetValuesLeafWrapper(Widget w, ArgList args, Cardinal *num_args)
{
    WidgetClass wc = XtClass(w);
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif));

    if (!bce || !*bce)
    {
	return;
    }

    if ((*bce)->getValuesPosthook)
    {
	wrap = _XmPopWrapperData(wc);

	wc->core_class.get_values_hook = wrap->getValuesLeaf;

	if (wrap->getValuesLeaf)
	{
	    (wrap->getValuesLeaf)(w, args, num_args);
	}

	((*bce)->getValuesPosthook)(w, args, num_args);

	_XmFreeWrapperData(wrap);
    }
}


/*
 * The GetValues pre-hook is called before any other get_values_hook method.
 * But there's a problem with the post-hook function. It isn't called as the
 * last function in the chain in SOME circumstances. If a widget uses the
 * constraint get_values_hook by defining the appropiate extensions record
 * then the hook will be called BEFORE all constraint get_values_hook(s).
 * The csf did a clean design, really. Clean of operability. So let us stick
 * at 100% compatibility for two (IMHO maybe silly) reasons:
 * 1. The whole pre- & post-hook stuff is undocumented, so don't you dare
 *    to use it with your self-written widgets. And even if the stuff would
 *    have been documented, you surely wouldn't dare to use it either...
 * 2. The LessTif widgets don't need the post-hook to be called AFTER all
 *    constraint get_values_hooks. So we simply don't worry about it.
 * BTW - shall we consider recursive operations? Argh, I'm going crazy
 * on this!! --aldi
 * MLM: Yes, you have to consider recursion.  If you don't, people who do
 * a GetValues, for example, in initialize methods will be in extreme pain.
 * I had to change _XmGetUnitType because of this (it used to do a GetValues,
 * but is called from ImportArgs in ResInd).
 * MLM 2:  I looked, and the Constraint class adds a separate GetValues
 * hook.  What is done with that?
 */
static void
GetValuesRootWrapper(Widget w, ArgList args, Cardinal *num_args)
{
    WidgetClass wc = XtClass(w);
    XmBaseClassExt *bce = NULL;
    XmWrapperData wrap;

    bce = ((XmBaseClassExt *)_XmGetBaseClassExtPtr(wc, XmQmotif));

    if (!bce || !*bce)
    {
	return;
    }

    if ((*bce)->getValuesPrehook)
    {
	((*bce)->getValuesPrehook)(w, args, num_args);
    }

    if ((*bce)->getValuesPosthook)
    {
	wrap = _XmPushWrapperData(wc);

	wrap->getValuesLeaf = wc->core_class.get_values_hook;

	wc->core_class.get_values_hook = GetValuesLeafWrapper;
    }
}


/*
 * Initialize the dreaded extension mechanism. This new extension is needed
 * in LessTif in order to pre- or postprocess some toolkit operations. This
 * initialization is done only once during the creation of the first
 * vendorShell.
 * MLM: Possibly, but I'm gonna make sure it's called by putting in the
 * lesstif core widgets -- Gadget, Primitive, Manager, VendorShell.
 */
extern void
_XmInitializeExtensions(void)
{
    /* We want to use the same as Motif, so our
     * widgets can be used with theirs. -- Chris */
    if (XmQmotif == 0)
    {
	XmQmotif = XrmStringToQuark("OSF_MOTIF");
    }

    /* for why this happens, see testXm/baseclass/test3.c */
    if (ObjectClassPartInit == NULL)
    {

	ObjectClassPartInit = objectClassRec.object_class.class_part_initialize;
	ObjectSetValues = objectClassRec.object_class.set_values;

	objectClassRec.object_class.class_part_initialize
	    = ClassPartInitRootWrapper;
	objectClassRec.object_class.initialize = InitializeRootWrapper;
	objectClassRec.object_class.set_values = SetValuesRootWrapper;
	objectClassRec.object_class.get_values_hook = GetValuesRootWrapper;
    }
}


/*
 * Assumption: widgets that have subresources use this as a helper
 * function when their getSecResData BaseClassExt function is called.
 * Testing shows that the resources are uncompiled -- thus the call
 * to _XmTransformSubResources...  Note the two allocations, and their
 * relationship to testXm/baseclass/test5.c.  Now to figure out who uses
 * it.
 */
extern Cardinal
_XmSecondaryResourceData(XmBaseClassExt bce,
			 XmSecondaryResourceData **secResDataRtn,
			 XtPointer client_data,
			 String name,
			 String class_name,
			 XmResourceBaseProc basefunctionpointer)
{
    XmSecondaryResourceData sec, *sd;
    WidgetClass wc;

    if (!bce)
    {
	return 0;
    }

    if (!bce->secondaryObjectClass)
    {
	return 0;
    }

    sec = (XmSecondaryResourceData)XtCalloc(1,
					    sizeof(XmSecondaryResourceDataRec));

    wc = bce->secondaryObjectClass;

    sec->name = name;
    sec->res_class = class_name;
    sec->client_data = client_data;
    sec->base_proc = basefunctionpointer;

    _XmTransformSubResources(wc->core_class.resources,
			     wc->core_class.num_resources,
			     &sec->resources, &sec->num_resources);

    sd = (XmSecondaryResourceData *)XtMalloc(sizeof(XmSecondaryResourceData));

    *sd = sec;
    *secResDataRtn = sd;

    return 1;
}


/*
 * Testing shows that calling this function before and after instantiation
 * of a widget results in uncompiled resources.  So I assume that this
 * guy can "decompile" resources (test by instantiating a subpart).
 * Unfortunately, a call to GetSubresources will compile the list (broken,
 * broken, broken), and this call won't work if you haven't instantiated
 * an object (the "owner" field in the BaseClassExt isn't set until
 * class_intialize), thus the following crap.
 */
extern void
_XmTransformSubResources(XtResourceList comp_resources,
			 Cardinal num_comp_resources,
			 XtResourceList *resources,
			 Cardinal *num_resources)
{
    XtResourceList res;
    Cardinal i;

    if (num_comp_resources == 0)
    {
	*resources = NULL;
	*num_resources = 0;
	return;
    }

    res = (XtResourceList)XtCalloc(num_comp_resources, sizeof(XtResource));

    if ((int)(comp_resources[0].resource_offset) >= 0)
    {
	memcpy(res, comp_resources, sizeof(XtResource) * num_comp_resources);
    }
    else
    {
	/*
	 * invert the compilation: ripped off from GetResList.c (X11R6).
	 */
	for (i = 0; i < num_comp_resources; i++)
	{
	    res[i].resource_name
		= XrmQuarkToString((XrmQuark)(long)comp_resources[i].resource_name);
	    res[i].resource_class
		= XrmQuarkToString((XrmQuark)(long)comp_resources[i].resource_class);
	    res[i].resource_type
		= XrmQuarkToString((XrmQuark)(long)comp_resources[i].resource_type);
	    /* MLM: sigh.  For want of a nail...  I coulda saved myself days if
	     * I'da done this in the first place */
	    res[i].resource_size
		= comp_resources[i].resource_size;
	    res[i].resource_offset
		= -((int)comp_resources[i].resource_offset + 1);
	    res[i].default_type
		= XrmQuarkToString((XrmQuark)(long)comp_resources[i].default_type);
	    res[i].default_addr
		= comp_resources[i].default_addr;
	}
    }

    *resources = res;
    *num_resources = num_comp_resources;
}


/*
 * lo and behold, there is a man page for this one.
 * According to OSF/Motif Programmer's Reference, page 1-539, there are
 * three things to free: the resources, the block, and the block array
 * (see testXm/baseclass/test5.c).
 */
extern Cardinal
XmGetSecondaryResourceData(WidgetClass w_class,
			   XmSecondaryResourceData **secondaryDataRtn)

{
    XmBaseClassExt *bce = NULL;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "XmGetSecondaryResourceData() called on %s.\n",
		      w_class->core_class.class_name));

    bce = _XmGetBaseClassExtPtr(w_class, XmQmotif);

    if (!bce || !*bce)
    {
	return 0;
    }

    if ((*bce)->getSecResData == NULL)
    {
	return 0;
    }

    return ((*bce)->getSecResData)(w_class, secondaryDataRtn);
}
