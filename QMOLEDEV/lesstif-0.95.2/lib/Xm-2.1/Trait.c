/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Trait.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Trait.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

/*
 * This file contains a poor man's implementation of traits.
 *
 * Traits are a simple mechanism to build multiple inheritance into Motif.
 *
 * The poor man's implementation is probably quite good enough because :
 *	- trait names are quark-ified strings which you can compare quickly
 *	- there aren't very many Motif classes.
 *
 * This implementation is a simple sequential search of the widgetclass.
 * From the widgetclass we find an array of entries which have the quark
 * representing the trait name, and a pointer to the trait record.
 *
 * As you'll understand we do two simple searches on short lists, which is
 * not all that slow.
 *
 * The inheritance mechanism is simple : it doesn't exist.
 * Instead the XmeTraitGet() function looks up the superclass and does the
 * lookup again. This inefficiency is simple to fix.
 */

#include <LTconfig.h>

#include <stdio.h>    /* for _LtDebugTraitReport() only */

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#include <Xm/XmP.h>
#include <Xm/TraitP.h>

/* All *T headers we have. Change this for the 2.1 tree! */
#include <Xm/AccTextT.h>
#include <Xm/ActivatableT.h>
#include <Xm/CareVisualT.h>
#include <Xm/ContainerT.h>
#include <Xm/ContItemT.h>
#include <Xm/DialogSavvyT.h>
#include <Xm/JoinSideT.h>
#include <Xm/LayoutT.h>
#include <Xm/MenuT.h>
#include <Xm/NavigatorT.h>
#include <Xm/ScrollFrameT.h>
#include <Xm/SpecRenderT.h>
#include <Xm/TakesDefT.h>
#include <Xm/TransferT.h>

#include <XmI/XmI.h>

#include <XmI/DebugUtil.h>

/* avoid a compiler warning. This interface is just a debugging feature
   and may disappear in future releases */
extern void _LtDebugTraitReport(void);


/* The numbers below are really just increments */
#define	NUM_WIDGET_CLASSES	32
#define	NUM_TRAITS		10

typedef struct _XmTraitListStruct {
	XrmQuark	trait;
	XtPointer	traitrec;
} _XmTraitRec;

static struct _XmTraitWidgetList {
	WidgetClass		wc;
	_XmTraitRec		*traits;
	int			num;
	int			pad;	/* so size becomes 16, no padding */
} *_traits = NULL;

static int	_num_traits = 0,
		_max_traits = 0;

/* This is the list of traits defined in Motif 2.0 */
XrmQuark  XmQTaccessTextual          = NULLQUARK;
XrmQuark  XmQTactivatable            = NULLQUARK;
XrmQuark  XmQTcareParentVisual       = NULLQUARK;
XrmQuark  XmQTcontainer              = NULLQUARK;
XrmQuark  XmQTcontainerItem          = NULLQUARK;
XrmQuark  XmQTdialogShellSavvy       = NULLQUARK;
XrmQuark  XmQTjoinSide               = NULLQUARK;
XrmQuark  XmQTmenuSavvy              = NULLQUARK;
XrmQuark  XmQTmenuSystem             = NULLQUARK;
XrmQuark  XmQTnavigator              = NULLQUARK;
XrmQuark  XmQTscrollFrame            = NULLQUARK;
XrmQuark  XmQTspecifyLayoutDirection = NULLQUARK;
XrmQuark  XmQTspecifyRenderTable     = NULLQUARK;
XrmQuark  XmQTtakesDefault           = NULLQUARK;
XrmQuark  XmQTtransfer               = NULLQUARK;

/*
 * XmeTraitGet finds out whether or not a given object has installed a given
 * trait. If it has installed, a pointer to the associated trait record is
 * returned, otherwise the result is NULL.
 * 
 * object	the object (a widget class) that you are inquiring about.
 * trait	the trait name.
 */
extern XtPointer
XmeTraitGet(XtPointer obj, XrmQuark trait)
{
	int		i, j;
	WidgetClass	super;

	if (obj == NULL || trait == 0) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmeTraitGet(NULL)\n"));
		_XmInitTraits();
		return NULL;
	}

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmeTraitGet(%s,%s)\n",
			((WidgetClass)obj)->core_class.class_name,
			XrmQuarkToString(trait)));

	for (i=0; i<_num_traits; i++)
	{
	    if (_traits[i].wc == (WidgetClass)obj)
	    {
		for (j=0; _traits[i].traits[j].trait && j<_traits[i].num; j++)
		    if (_traits[i].traits[j].trait == trait)
		    {
			/* We may have a removed trait here,
			 * search superclass for it */
			if (_traits[i].traits[j].traitrec == NULL)
			{
				super = XtSuperclass(obj);
				return XmeTraitGet((XtPointer)super, trait);
			}

			return _traits[i].traits[j].traitrec;
		    }

		    /* Not found, look in the superclass */
#if 1
		/* Sometimes XtSuperclass doesn't work. */
		/* Use its expansion instead */
		    super = ((WidgetClass)obj)->core_class.superclass;
#else
		    super = XtSuperclass(obj);
#endif
		    return XmeTraitGet((XtPointer)super, trait);

		    /* Never reached */
	    }
	}

	/* Widget Class not found */
	return NULL;
}

/*
 * XmeTraitSet installs trait on object. It is typically called from a widget's
 * class_part_initialize method. Subclasses of this object inherit the trait.
 *
 * object	Specifies the object.
 * trait	The trait name (an XrmQuark value).
 *
 * Returns True on success.
 */
extern Boolean
XmeTraitSet(XtPointer obj, XrmQuark trait, XtPointer rec)
{
	int	i, j;

	if (obj == NULL || trait == 0) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmeTraitSet(NULL)\n"));
		_XmInitTraits();
		return False;
	}

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmeTraitSet(%s,%s)\n",
			((WidgetClass)obj)->core_class.class_name,
			XrmQuarkToString(trait)));

	for (i=0; i<_num_traits; i++)
	{
	    if (_traits[i].wc == (WidgetClass)obj)
	    {
		/* Expand array */
		if (_traits[i].num == 0)
		{
		    _traits[i].num += NUM_TRAITS;
		    if (_traits[i].traits) {
		    	_traits[i].traits = (_XmTraitRec *) XtRealloc(
				(char *)_traits[i].traits,
				sizeof(_XmTraitRec) * _traits[i].num);
			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"XmeTraitSet: _traits[%d].traits = %p,"
				" size %d = %d * %d\n",
				i, _traits[i].traits,
				_traits[i].num * sizeof(_XmTraitRec),
				_traits[i].num,
				sizeof(_XmTraitRec)));
		    } else {
		    	_traits[i].traits = (_XmTraitRec *) XtCalloc(
				_traits[i].num, sizeof(_XmTraitRec));
			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"XmeTraitSet: _traits[%d].traits = %p,"
				" size %d = %d * %d\n",
				i, _traits[i].traits,
				_traits[i].num * sizeof(_XmTraitRec),
				_traits[i].num,
				sizeof(_XmTraitRec)));
		    }
		}

		for (j=0; _traits[i].traits[j].trait && j<_traits[i].num; j++)
		    if (_traits[i].traits[j].trait == trait)
		    {

			/* Trait was already installed !! */
			/* If rec == NULL this removes the trait.
			 * Code for supporting this is in XmeTraitGet */
			_traits[i].traits[j].traitrec = rec;

			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"XmeTraitSet: overwrite _traits[%d]."
				"traits[%d].traitrec\n",
				i, j));

			return True;
		    }
		if (j == _traits[i].num)
		{
		    /* Expand array first */
		    _traits[i].num += NUM_TRAITS;
		    if (_traits[i].traits) {
			_traits[i].traits = (_XmTraitRec *) XtRealloc(
			(char *)_traits[i].traits,
			sizeof(_XmTraitRec) * _traits[i].num);
			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"XmeTraitSet: _traits[%d].traits = %p,"
				" size %d = %d * %d\n",
				i, _traits[i].traits,
				_traits[i].num * sizeof(_XmTraitRec),
				_traits[i].num,
				sizeof(_XmTraitRec)));
		    } else {
			_traits[i].traits = (_XmTraitRec *) XtCalloc(
				_traits[i].num, sizeof(_XmTraitRec));
			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"XmeTraitSet: _traits[%d].traits = %p,"
				" size %d = %d * %d\n",
				i, _traits[i].traits,
				_traits[i].num * sizeof(_XmTraitRec),
				_traits[i].num,
				sizeof(_XmTraitRec)));
		    }
		}

		/* Add the trait to the list */
		_traits[i].traits[j].trait = trait;
		_traits[i].traits[j].traitrec = rec;
#if 0
		_traits[i].traits[j+1].trait = NULLQUARK;
#endif

		DEBUGOUT(_LtDebug(__FILE__, NULL,
			"XmeTraitSet: write _traits[%d]."
			"traits[%d].traitrec\n",
			i, j));
		DEBUGOUT(_LtDebug(__FILE__, NULL,
			"XmeTraitSet: write _traits[%d]."
			"traits[%d].traitrec\n",
			i, j+1));

		return True;
	    }
	}

	/* No traits known yet for this widget class */

	if (_max_traits == _num_traits) {
		_max_traits += NUM_WIDGET_CLASSES;
		if (_traits) {
		    _traits = (struct _XmTraitWidgetList *)
			XtRealloc((char *)_traits,
			_max_traits * sizeof(struct _XmTraitWidgetList));
		} else {
		    _traits = (struct _XmTraitWidgetList *) XtCalloc(
			_max_traits, sizeof(struct _XmTraitWidgetList));
		    DEBUGOUT(_LtDebug(__FILE__, NULL,
			"XmeTraitSet: _traits = %p, size %d = %d * %d\n",
			_traits,
			_max_traits * sizeof(struct _XmTraitWidgetList),
			_max_traits,
			sizeof(struct _XmTraitWidgetList)));
		}
	}

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmeTraitSet: set _traits[%d]\n", i));

	_traits[i].wc = (WidgetClass)obj;
	_traits[i].num = 0;
	_traits[i].traits = NULL;
	_num_traits++;

	DEBUGOUT(_LtDebug0(__FILE__, NULL, "Recursive Call : ")); /* no \n */

	return XmeTraitSet(obj, trait, rec);	/* Recursive call */
}

extern void
_XmInitTraits(void)
{
	if (XmQTaccessTextual) {
	    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmInitTraits() 2nd time\n"));
	    return;
	}

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmInitTraits()\n"));

	XmQTaccessTextual = XrmStringToQuark("XmQTaccessTextual");
	XmQTactivatable = XrmStringToQuark("XmQTactivatable");
	XmQTcareParentVisual = XrmStringToQuark("XmQTcareParentVisual");
	XmQTcontainer = XrmStringToQuark("XmQTcontainer");
	XmQTcontainerItem = XrmStringToQuark("XmQTcontainerItem");
	XmQTdialogShellSavvy = XrmStringToQuark("XmQTdialogShellSavvy");
	XmQTjoinSide = XrmStringToQuark("XmQTjoinSide");
	XmQTmenuSavvy = XrmStringToQuark("XmQTmenuSavvy");
	XmQTmenuSystem = XrmStringToQuark("XmQTmenuSystem");
	XmQTnavigator = XrmStringToQuark("XmQTnavigator");
	XmQTscrollFrame = XrmStringToQuark("XmQTscrollFrame");
	XmQTspecifyLayoutDirection = XrmStringToQuark("XmQTspecifyLayoutDirection");
	XmQTspecifyRenderTable = XrmStringToQuark("XmQTspecifyRenderTable");
	XmQTtakesDefault = XrmStringToQuark("XmQTtakesDefault");
	XmQTtransfer = XrmStringToQuark("XmQTtransfer");
}


/* for debugging purposes only - as the name indicates */
extern void
_LtDebugTraitReport(void)
{
	int		i, j;
	XrmQuark	q;

	for (i=0; i<_num_traits; i++) {
		fprintf(stderr, "Widget class '%s'\n", _traits[i].wc->core_class.class_name);
		for (j=0; _traits[i].traits[j].trait && j<_traits[i].num; j++) {
			q = _traits[i].traits[j].trait;
			fprintf(stderr, "\t\t%s\n", XrmQuarkToString(q));
		}
	}
}
