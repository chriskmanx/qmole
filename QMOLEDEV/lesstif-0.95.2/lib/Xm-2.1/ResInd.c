/**
 *
 * $Id: ResInd.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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
/**
 *
 * This file contains functions relating to resolution independent
 * dimension control in Motif/LessTif
 *
 **/

static const char rcsid[] = "$Id: ResInd.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/VendorSEP.h>

#include <XmI/DebugUtil.h>

/*
 * ruthlessly stripped from Xt/Resources.c
 */
static void
CopyToArg(char *src, XtArgVal *dst, unsigned int size)
{
    union
    {
	long longval;
#if SIZEOF_LONG == 8
	int intval;
#endif
	short shortval;
	char charval;
	char *charptr;
	XtPointer ptr;
    }
    u;

    if (size <= sizeof(XtArgVal))
    {

	memcpy(&u, src, size);

	if (size == sizeof(long))
	{
	    *dst = (XtArgVal)u.longval;
	}
#if SIZEOF_LONG == 8
	else if (size == sizeof(int))
	{
	    *dst = (XtArgVal)u.intval;
	}
#endif
	else if (size == sizeof(short))
	{
	    *dst = (XtArgVal)u.shortval;
	}
	else if (size == sizeof(char))
	{
	    *dst = (XtArgVal)u.charval;
	}
	else if (size == sizeof(char *))
	{
	    *dst = (XtArgVal)u.charptr;
	}
	else if (size == sizeof(XtPointer))
	{
	    *dst = (XtArgVal)u.ptr;
	}
	else
	{
	    memcpy(dst, src, size);
	}
    }
    else
    {
	memcpy(dst, src, size);
    }
}

static void
CopyFromArg(XtArgVal src, char *dst, unsigned int size)
{
    if (size > sizeof(XtArgVal))
    {
	memcpy(dst, (void *)src, size);
    }
    else
    {
	union
	{
	    long longval;
#if SIZEOF_LONG == 8
	    int intval;
#endif
	    short shortval;
	    char charval;
	    char *charptr;
	    XtPointer ptr;
	}
	u;
	char *p = (char *)&u;

	if (size == sizeof(long))
	{
	    u.longval = (long)src;
	}
#if SIZEOF_LONG == 8
	else if (size == sizeof(int))
	{
	    u.intval = (int)src;
	}
#endif
	else if (size == sizeof(short))
	{
	    u.shortval = (short)src;
	}
	else if (size == sizeof(char))
	{
	    u.charval = (char)src;
	}
	else if (size == sizeof(XtPointer))
	{
	    u.ptr = (XtPointer)src;
	}
	else if (size == sizeof(char *))
	{
	    u.charptr = (char *)src;
	}
	else
	{
	    p = (char *)&src;
	}

	memcpy(dst, p, size);
    }
}

void
_XmFromHorizontalPixels(Widget widget,
			int offset,
			XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmHORIZONTAL,
				     XmPIXELS,
				     *value,
				     unitType);

    *value = converted_value;
}

void
_XmFromVerticalPixels(Widget widget,
		      int offset,
		      XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmVERTICAL,
				     XmPIXELS,
				     *value,
				     unitType);

    *value = converted_value;
}

XmImportOperator
_XmToHorizontalPixels(Widget widget,
		      int offset,
		      XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmHORIZONTAL,
				     unitType,
				     *value,
				     XmPIXELS);

    *value = converted_value;
    return XmSYNTHETIC_LOAD;
}

XmImportOperator
_XmToVerticalPixels(Widget widget,
		    int offset,
		    XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmVERTICAL,
				     unitType,
				     *value,
				     XmPIXELS);

    *value = converted_value;
    return XmSYNTHETIC_LOAD;
}

/*
 * this function _does_ appear to combine all the synthetic resources an
 * object has with the synthetic resources the parent classes have.  Check
 * out testXm/misc/test6.c and testXm/misc/mot.compsyn.  Closer examinination
 * of mot.compsyn will show that duplicate synthetics are not found, even
 * when a subclass has the same synthetic as a superclass (e.g., Form
 * and BulletinBoard.  My assumption is that the subclass synthetic takes
 * precedence over the superclass, as that's the class initialization order,
 * and that's what Xt does.  An interesting experiment is to subclass from
 * a widget that has synthetic resources, and add no new ones.  You'll
 * discover that after class part initialization (check in initialization;
 * you can call _XmBuildResources once), you'll have some.
 */
void
_XmBuildResources(XmSyntheticResource **wc_resources_ptr,
		  int *wc_num_resources_ptr,
		  XmSyntheticResource *sc_resources,
		  int sc_num_resources)
{
    XmSyntheticResource *cmb;
    int ncmb, i, j, wb;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "BuildResources\n"));

    if (*wc_num_resources_ptr == 0)
    {
	*wc_resources_ptr = sc_resources;
	*wc_num_resources_ptr = sc_num_resources;
	return;
    }

    ncmb = *wc_num_resources_ptr + sc_num_resources;
    cmb = (XmSyntheticResource *)XtMalloc(ncmb * sizeof(XmSyntheticResource));
    memcpy((void *)cmb,
           (void *)sc_resources,
	   sc_num_resources * sizeof(XmSyntheticResource));
    memcpy((void *)&cmb[sc_num_resources],
           (void *)*wc_resources_ptr,
	   *wc_num_resources_ptr * sizeof(XmSyntheticResource));

    for (i = 0; i < sc_num_resources; i++)
    {
	wb = sc_num_resources;
	for (j = 0; j < (ncmb - wb);)
	{
	    if (cmb[i].resource_name == cmb[wb + j].resource_name &&
		cmb[i].resource_size == cmb[wb + j].resource_size &&
		cmb[i].resource_offset == cmb[wb + j].resource_offset)
	    {
		cmb[i] = cmb[wb + j];
		if ((ncmb - (wb + j + 1) > 0))
		{
		    memcpy((void *)&cmb[wb + j],
		           (void *)&cmb[wb + j + 1],
			   (ncmb - (wb + j + 1)) * sizeof(XmSyntheticResource));
		}
		ncmb--;
	    }
	    else
	    {
		j++;
	    }
	}
    }
    *wc_resources_ptr = cmb;
    *wc_num_resources_ptr = ncmb;
}

/*
 * the only thing I know that Motif does is to "quarkify" the strings.
 */
void
_XmInitializeSyntheticResources(XmSyntheticResource *resources,
				int num_resources)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "InitializeSyntheticResources\n"));

    if (!resources || !num_resources)
    {
	return;
    }
    for (i = 0; i < num_resources; i++)
    {
	resources[i].resource_name =
	    (char *)(long)XrmStringToQuark(resources[i].resource_name);
    }
}

/*
 * _Xm*GetValueHook:
 *
 * Ugh.  Another doubly nested loop.  There has GOT to be a better way to
 * do this.
 * There is another bit of obscene trickery here.  After I dumped the
 * synthetics table (textXm/misc/test3.c, mot.syn), I could not for the
 * life of me figure out why Gadget were specifying their object.parent
 * fields for some of the synthetics.  Then it occurred to me that we
 * rarely pay attention to our offset values in DefaultProcs.c, so ExportProcs
 * were free to do the same, or to use them in nefarious ways.  Look in
 * Gadget.c for more details.
 * If you don't do GetValues right, forget it.  What works in R5 won't in R6,
 * by default.  See lib/Xt/Resources.c (GETVALUES_BUG).  Using CopyToArg
 * and CopyFromArg will work with R5, and give a toolkit error (by default)
 * in R6.
 */
void
_XmPrimitiveGetValuesHook(Widget w,
			  ArgList args,
			  Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));

    DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveGetValuesHook\n"));

    if (!XmIsPrimitive(w))
    {
	return;
    }

    for (i = 0; i < *num_args; i++)
    {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->primitive_class.num_syn_resources; j++)
	{
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &pwc->primitive_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);
		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}

	/*
	 * check for constraint synthetics
	 */
	if (!XmIsManager(XtParent(w)))
	{
	    continue;
	}
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	{
	    continue;
	}
	if (!w->core.constraints)
	{
	    continue;
	}
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++)
	{
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w->core.constraints + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);
		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}
    }
}

void
_XmGadgetGetValuesHook(Widget w,
		       ArgList args,
		       Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmGadgetClass gwc = (XmGadgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));

    DEBUGOUT(_LtDebug(__FILE__, w, "GadgetGetValuesHook\n"));

    if (!XmIsGadget(w))
    {
	return;
    }

    for (i = 0; i < *num_args; i++)
    {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < gwc->gadget_class.num_syn_resources; j++)
	{
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &gwc->gadget_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);
		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}

	/*
	 * check for constraint synthetics
	 */
	if (!XmIsManager(XtParent(w)))
	{
	    continue;
	}
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	{
	    continue;
	}
	if (!w->core.constraints)
	{
	    continue;
	}
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++)
	{
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w->core.constraints + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);
		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}
    }
}

void
_XmManagerGetValuesHook(Widget w,
			ArgList args,
			Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmManagerWidgetClass pwc = (XmManagerWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));

    DEBUGOUT(_LtDebug(__FILE__, w, "ManagerGetValuesHook\n"));

    if (!XmIsManager(w))
    {
	return;
    }

    for (i = 0; i < *num_args; i++)
    {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->manager_class.num_syn_resources; j++)
	{
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &pwc->manager_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);
		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}

	/*
	 * check for constraint synthetics
	 */
	if (!XmIsManager(XtParent(w)))
	{
	    continue;
	}
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	{
	    continue;
	}
	if (!w->core.constraints)
	{
	    continue;
	}
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++)
	{
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w->core.constraints + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);
		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}
    }
}

void
_XmExtGetValuesHook(Widget w,
		    ArgList args,
		    Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmExtObjectClass ewc = (XmExtObjectClass)XtClass(w);
    XmSyntheticResource *res;
    XrmQuark rq;


    DEBUGOUT(_LtDebug(__FILE__, w, "ExtObjectGetValuesHook\n"));

    if (!XmIsExtObject(w))
    {
	return;
    }

    for (i = 0; i < *num_args; i++)
    {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < ewc->ext_class.num_syn_resources; j++)
	{
	    res = &ewc->ext_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (rq == tq && res->export_proc != NULL)
	    {
		XtArgVal val = 0;

		CopyToArg((char *)w + res->resource_offset,
			  &val,
			  res->resource_size);

		(res->export_proc) (w,
				    res->resource_offset,
				    &val);

		CopyFromArg(val,
			    (char *)args[i].value,
			    res->resource_size);
	    }
	}
    }
}

void
_XmExtImportArgs(Widget w,
		 ArgList args,
		 Cardinal *num_args)
{
    XmExtObjectClass ewc = (XmExtObjectClass)XtClass(w);
    int i;
    XrmQuark tq;

    DEBUGOUT(_LtDebug(__FILE__, w, "ExtImportArgs\n"));

    if (!XmIsExtObject(w))
    {
	return;
    }
    /*
     * An ExtObject will never have a real parent, and will never have
     * constraints.
     */
    for (i = 0; i < *num_args; i++)
    {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < ewc->ext_class.num_syn_resources; j++)
	{
	    res = &ewc->ext_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc != NULL)
	    {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w + res->resource_offset,
				res->resource_size);
		}
		else
		{
		    args[i].value = value;
		}
	    }
	}
    }
}

void
_XmPrimitiveImportArgs(Widget w,
		       ArgList args,
		       Cardinal *num_args)
{
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));
    int i;
    XrmQuark tq;

    DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveImportArgs\n"));

    if (!XmIsPrimitive(w))
    {
	return;
    }

    /*
     * This was commented, 'cause there should be a better way than a doubly
     * nested loop.
     * 012396 - MLM - As bad as this is, it seems to be what Motif does.  This
     * will be what I do for now, but this should be revisited.
     */
    for (i = 0; i < *num_args; i++)
    {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->primitive_class.num_syn_resources; j++)
	{
	    res = &pwc->primitive_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;

	    if (tq == rq && res->import_proc != NULL)
	    {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w + res->resource_offset,
				res->resource_size);
		}
		else
		{
		    args[i].value = value;
		}
	    }
	}
	/*
	 * Form (at least) has synthetic constraint resources.  Do them, too.
	 */
	if (!XmIsManager(XtParent(w)))
	{
	    continue;
	}
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	{
	    continue;
	}
	if (!w->core.constraints)
	{
	    continue;
	}
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++)
	{
	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc)
	    {
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w->core.constraints +
				res->resource_offset,
				res->resource_size);
		}
		else
		{
		    args[i].value = value;
		}
	    }
	}
    }
}

void
_XmGadgetImportArgs(Widget w,
		    ArgList args,
		    Cardinal *num_args)
{
    XmGadgetClass gwc = (XmGadgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));
    int i;
    XrmQuark tq;

    DEBUGOUT(_LtDebug(__FILE__, w, "GadgetImportArgs\n"));

    if (!XmIsGadget(w))
    {
	return;
    }

    /*
     * see comments above.
     */
    for (i = 0; i < *num_args; i++)
    {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < gwc->gadget_class.num_syn_resources; j++)
	{
	    res = &gwc->gadget_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc != NULL)
	    {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w + res->resource_offset,
				res->resource_size);
		}
		else
		{
		    args[i].value = value;
		}
	    }
	}
	/*
	 * Form (at least) has synthetic constraint resources.  Do them, too.
	 */
	if (!XmIsManager(XtParent(w)))
	{
	    continue;
	}
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	{
	    continue;
	}
	if (!w->core.constraints)
	{
	    continue;
	}
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++)
	{
	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc)
	    {
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w->core.constraints +
				res->resource_offset,
				res->resource_size);
		}
		else
		{
		    args[i].value = value;
		}
	    }
	}
    }
}

/*
 * Got the same problems here that I have in BuildGadgetResources -- how to
 * get the GCache part of a gadget, and gadgets with no cache part.
 * 012796 -- Solved by the BaseClass extension
 */
void
_XmGadgetImportSecondaryArgs(Widget w,
			     ArgList args,
			     Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmExtClassRec *subpclass;
    int i;
    XrmQuark tq;

    DEBUGOUT(_LtDebug(__FILE__, w, "GadgetImportSecondaryArgs\n"));

    bce = (XmBaseClassExt *)_XmGetBaseClassExtPtr(XtClass(w), XmQmotif);

    if (!XmIsGadget(w) || !*bce ||
	!(subpclass = (XmExtObjectClass)(*bce)->secondaryObjectClass))
    {
	return;
    }

    /*
     * see comments above.
     */
    for (i = 0; i < *num_args; i++)
    {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < subpclass->ext_class.num_syn_resources; j++)
	{
	    res = &subpclass->ext_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc != NULL)
	    {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		(res->import_proc) (w, res->resource_offset, &value);
		args[i].value = value;
	    }
	}
    }
}

void
_XmManagerImportArgs(Widget w,
		     ArgList args,
		     Cardinal *num_args)
{
    /*XmManagerWidget mw = (XmManagerWidget)w;*/
    XmManagerWidgetClass pwc = (XmManagerWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));
    int i;
    XrmQuark tq;

    DEBUGOUT(_LtDebug(__FILE__, w, "ManagerImportArgs\n"));

    if (!XmIsManager(w))
    {
	return;
    }
    /*
     * A little trickier this time.  When you read this, keep in mind that
     * managers often have Manager parents, and thus have constraint resources.
     */
    for (i = 0; i < *num_args; i++)
    {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

#if 0
	/* Hack
	 * Don't assign 0 values to width/height
	 *
	 * Danny june 24, 1999
	 */
	if (tq == XrmStringToQuark(XmNwidth)) {
		if (mw->manager.user_data == 0xdead) {
			_XmWarning(w, "_XmManagerImportArgs (in init): set width of widget to 0\n");
		} else {
			_XmWarning(w, "_XmManagerImportArgs: won't set width of widget to 0\n");
			continue;
		}
	}
	if (tq == XrmStringToQuark(XmNheight)) {
		if (mw->manager.user_data == 0xdead) {
			_XmWarning(w, "_XmManagerImportArgs (in init): set height of widget to 0\n");
		} else {
			_XmWarning(w, "_XmManagerImportArgs: won't set height of widget to 0\n");
			continue;
		}
	}
#endif

	for (j = 0; j < pwc->manager_class.num_syn_resources; j++)
	{
	    res = &pwc->manager_class.syn_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc != NULL)
	    {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w + res->resource_offset,
				res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
	/*
	 * Form (at least) has synthetic constraint resources.  Do them, too.
	 */
	if (!XmIsManager(XtParent(w)))
	{
	    continue;
	}
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	{
	    continue;
	}
	if (!w->core.constraints)
	{
	    continue;
	}

	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++)
	{
	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)(long)res->resource_name;
	    if (tq == rq && res->import_proc)
	    {
		value = args[i].value;
		if ((res->import_proc) (w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD)
		{
		    CopyFromArg(value,
				(char *)w->core.constraints +
				res->resource_offset,
				res->resource_size);
		}
		else
		{
		    args[i].value = value;
		}
	    }
	}
    }
}

#define	FU		0x1	/* (Pixels per) Font Unit		*/
#define	CMMPP		0x2	/* 100ths of millimeters per pixel	*/

int
_XmConvertUnits(Screen *screen,
		int orientation,
		int from_type,
		int from_val,
		int to_type)
{
    int		fu = 0, cmmpp = 0;
    struct conv
    {
	int	num, denom;	/* Constant conversion factor		*/
	int	numv, denomv;	/* Variable conversion factor		*/
    }		*cp;
    static struct conv conversions[5][5] =
    {{				/* from PIXELS to ...			*/
	{1, 1, 0, 0},		/* PIXELS				*/
	{1, 1, CMMPP, 0},	/* 100TH_MILLIMETERS = CMMPP		*/
	{50, 127, CMMPP, 0},	/* 1000TH_INCHES = CMMPP * 10/25.4	*/
	{360, 127, CMMPP, 0},	/* 100TH_POINTS = CMMPP * 72/25.4	*/
	{100, 1, 0, FU}		/* 100TH_FONT_UNITS = 100/FU		*/
    },
    {				/* from 100TH_MILLIMETERS to ...	*/
	{1, 1, 0, CMMPP},	/* PIXELS = 1/CMMPP			*/
	{1, 1, 0, 0},		/* 100TH_MILLIMETERS			*/
	{50, 127, 0, 0},	/* 1000TH_INCHES = 10/25.4		*/
	{360, 127, 0, 0},	/* 100TH_POINTS = 72/25.4		*/
	{100, 1, 0, CMMPP | FU}	/* 100TH_FONT_UNITS = 1/CMMPP * 100/FU	*/
    },
    {				/* from 1000TH_INCHES to ...		*/
	{127, 50, 0, CMMPP},	/* PIXELS = 1/CMMPP * 25.4/10		*/
	{127, 50, 0, 0},	/* 100TH_MILLIMETERS = 25.4/10		*/
	{1, 1, 0, 0},		/* 1000TH_INCHES			*/
	{36, 5, 0, 0},		/* 100TH_POINTS = 72/10			*/
	{254, 1, 0, CMMPP | FU}	/* 100TH_FONT_UNITS = 1/CMMPP * 25.4/10	*/
    },				/*		      * 100/FU		*/
    {				/* from 100TH_POINTS to ...		*/
	{127, 360, 0, CMMPP},	/* PIXELS = 1/CMMPP * 25.4/72		*/
	{127, 360, 0, 0},	/* 100TH_MILLIMETERS = 25.4/72		*/
	{5, 36, 0, 0},		/* 1000TH_INCHES = 10/72		*/
	{1, 1, 0, 0},		/* 100TH_POINTS				*/
	{635, 18, 0, CMMPP | FU}/* 100TH_FONT_UNITS = 1/CMMPP * 25.4/72	*/
    },				/*		      * 100/FU		*/
    {				/* from 100TH_FONT_UNITS to ...		*/
	{1, 100, FU, 0},	/* PIXELS = FU/100			*/
	{1, 100, FU | CMMPP, 0},/* 100TH_MILLIMETERS = CMPP * FU/100	*/
	{1, 254, FU | CMMPP, 0},/* 1000TH_INCHES = CMPP * 10/25.4	*/
				/*		   * FU/100		*/
	{18, 635, FU | CMMPP,0},/* 100TH_POINTS = CMPP * 72/25.4	*/
				/*		  * FU/100		*/
	{1, 1, 0, 0}		/* 100TH_FONT_UNITS			*/
    }};

    /* The trivial case - converting a value to itself */

    if (from_type == to_type)
	return from_val;

    /* Fetch the font units and/or screen dimensions if we need them */

    cp = &conversions[from_type][to_type];
    if ((cp->numv | cp->denomv) & FU)
	fu = _XmGetFontUnit(screen, orientation);
    if ((cp->numv | cp->denomv) & CMMPP)
	cmmpp = orientation == XmHORIZONTAL
	    ? (100 * WidthMMOfScreen(screen)) / WidthOfScreen(screen)
	    : (100 * HeightMMOfScreen(screen)) / HeightOfScreen(screen);

    /* Do the actual conversion, first all multiplies, then all divides	*/

    from_val *= cp->num;
    if (cp->numv & FU)
	from_val *= fu;
    if (cp->numv & CMMPP)
	from_val *= cmmpp;
    from_val /= cp->denom;
    if (cp->denomv & FU)
	from_val /= fu;
    if (cp->denomv & CMMPP)
	from_val /= cmmpp;
    return from_val;
}

/*
 * MLM: In Motif, if this function is called with an uncompiled resource list,
 * it explodes messily.  However, if you look at the output of misc/test5.c
 * you will notice an interesting thing;  the XmNunitType resource floats
 * to the top (check testXm/misc/test5.c + testXm/misc/mot.compres).
 * I can't find any other candidate for doing this other than
 * _XmSortResourceList; after passing in a LessTif widget resource list into
 * this function, I can verify that that does indeed happen.
 * Note the XrmResource argument:  it must be a compiled resource list.
 * Addendum: this actually makes a weird sort of sense:  the Synthetic resource
 * ResInd stuff wants the UnitType to be correct when called.  That doesn't
 * mean it's pretty, though.
 */
void
_XmSortResourceList(XrmResource *list[],
		    Cardinal len)
{
    int i;
    XrmQuark unit;
    XrmResource *tmp;

    unit = XrmStringToQuark(XmNunitType);

    for (i = 0; i < len; i++)
    {
	if (list[i]->xrm_name == unit)
	{
	    break;
	}
    }

    if (i == len || i == 0)	/* Nobody home, or already done */
    {
	return;
    }

    tmp = list[i];
/*    memcpy((void *)&list[1], (void *)&list[0], i * sizeof(XrmResource *)); */
    list[i] = list[0];
    list[0] = tmp;
}

unsigned char
_XmGetUnitType(Widget widget)
{
    Widget ve;

    /*
     * There used to be a call to XtGetValues here.  Unfortunately, with the
     * BaseClass stuff, that won't work, as that looks like a recursive call
     * to GetValues (i.e., the call to here from _XmGadgetImportArgs).  That
     * implied that the GetValuesRootWrapper was called twice, with the
     * result that GetValuesLeafWrapper was called infinitely recursive.
     * Bad thing, that.
     */
    if (XmIsVendorShell(widget))
    {
	if ((ve = _LtFindVendorExt(widget)) != NULL)
	{
	    return VSEP_UnitType(ve);
	}
	else
	{
	    return XmPIXELS;
	}
    }
    if (XmIsManager(widget))
    {
	return MGR_UnitType(widget);
    }

    if (XmIsPrimitive(widget))
    {
	return Prim_UnitType(widget);
    }

    if (XmIsGadget(widget))
    {
	return G_UnitType(widget);
    }

    if (XmIsExtObject(widget))
    {
	return G_UnitType(ExtObj_LogicalParent(widget));
    }

    return XmPIXELS;
}

/*
 * XmP.h says this is here
 */
void
_XmUnitTypeDefault(Widget w,
		   int offset,
		   XrmValue *val)
{
    static unsigned char unit_type;
    Widget ve;

    unit_type = XmPIXELS;

    if ((XmIsPrimitive(w) || XmIsGadget(w)))
    {
	if (XmIsManager(XtParent(w)))
	{
	    unit_type = MGR_UnitType(XtParent(w));
	}
    }
    else if (XmIsManager(w))
    {
	if (XmIsManager(XtParent(w)))
	{
	    unit_type = MGR_UnitType(XtParent(w));
	}
	else if (XmIsVendorShell(XtParent(w)))
	{
	    if ((ve = _LtFindVendorExt(XtParent(w))) != NULL)
	    {
		unit_type = VSEP_UnitType(ve);
	    }
	}
    }

    val->addr = (XPointer)&unit_type;
}

void
_XmExportXmString(Widget w, int offset, XtArgVal *value)
{
    XmString str;

    str = *(XmString *)(((char *)w) + offset);
    if (str)
    {
	str = XmStringCopy(str);
    }

    *value = (XtArgVal)str;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmExportXmString(%p, %d, _) %p -> '%s'\n",
		w, offset,
		str,
		_LtDebugXmString2String(str)));
}

void
_XmExportString(Widget w, int offset, XtArgVal *value)
{
    String str;

    str = *(String *)(((char *)w) + offset);
    if (str)
    {
	str = XtNewString(str);
    }

    *value = (XtArgVal)str;
}

int
XmConvertUnits(Widget widget,
	       int orientation,
	       int from_unit_type,
	       int from_value,
	       int to_unit_type)
{
    if (widget == NULL)
    {
	return 0;
    }
    if (orientation != XmHORIZONTAL && orientation != XmVERTICAL)
    {
	return 0;
    }
    if (from_unit_type < 0 || from_unit_type > Xm100TH_FONT_UNITS)
    {
	return 0;
    }
    if (to_unit_type < 0 || to_unit_type > Xm100TH_FONT_UNITS)
    {
	return 0;
    }
    return _XmConvertUnits(XtScreenOfObject(widget), orientation,
			   from_unit_type, from_value, to_unit_type);
}

int
XmCvtToHorizontalPixels(Screen *screen, int from_val, int from_type)
{
    if (!screen)
    {
	return 0;
    }
    if (from_type < 0 || from_type > Xm100TH_FONT_UNITS)
    {
	return 0;
    }
    return _XmConvertUnits(screen, XmHORIZONTAL, from_type, from_val, XmPIXELS);
}

int
XmCvtToVerticalPixels(Screen *screen, int from_val, int from_type)
{
    if (!screen)
    {
	return 0;
    }
    if (from_type < 0 || from_type > Xm100TH_FONT_UNITS)
    {
	return 0;
    }
    return _XmConvertUnits(screen, XmVERTICAL, from_type, from_val, XmPIXELS);
}

int
XmCvtFromHorizontalPixels(Screen *screen, int from_val, int to_type)
{
    if (!screen)
    {
	return 0;
    }
    if (to_type < 0 || to_type > Xm100TH_FONT_UNITS)
    {
	return 0;
    }
    return _XmConvertUnits(screen, XmHORIZONTAL, XmPIXELS, from_val, to_type);
}

int
XmCvtFromVerticalPixels(Screen *screen, int from_val, int to_type)
{
    if (!screen)
    {
	return 0;
    }
    if (to_type < 0 || to_type > Xm100TH_FONT_UNITS)
    {
	return 0;
    }
    return _XmConvertUnits(screen, XmVERTICAL, XmPIXELS, from_val, to_type);
}

void
XmSetFontUnits(Display *display, int h_value, int v_value)
{
    int		i;
    XmScreen	sw;

    /* Set the horizontalFontUnit and verticalFontUnit values in the
     * screen object for each screen in the display.  But don't bother
     * with that pesky resource interface.
     */

    if (display)
	for (i = ScreenCount(display) - 1; i >= 0; i--)
	{
	    sw = (XmScreen)XmGetXmScreen(ScreenOfDisplay(display, i));
	    if (h_value > 0)
		Screen_HorizUnit(sw) = h_value;
	    if (v_value > 0)
		Screen_VertUnit(sw) = v_value;
	}
}

void
XmSetFontUnit(Display *display, int value)
{
    XmSetFontUnits( display, value, value);
}
