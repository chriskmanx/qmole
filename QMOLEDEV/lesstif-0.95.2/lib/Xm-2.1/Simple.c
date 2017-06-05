/**
 *
 * $Id: Simple.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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

static const char rcsid[] = "$Id: Simple.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CascadeBG.h>
#include <Xm/LabelG.h>
#include <Xm/ManagerP.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/SeparatoG.h>

#include <XmI/DebugUtil.h>

/* amai: what's this for?:
#ifdef	SIMPLE_WALKUP */

/* For the prototype police */
extern Widget _XmCreateSimpleGadget(char *name, Widget parent, int def_type,
				    XmSimpleMenu data, int which,
				    int call_data, ArgList args, int argc);

/*
 * I should have done this in the first place.  The old implementation wouldn't
 * catch resources specified outside of the create call.
 */
#undef Offset
#define Offset(field) XtOffsetOf(XmSimpleMenuRec, field)
static XtResource simple_resources[] =
{
    {
	XmNbuttonAccelerators, XmCButtonAccelerators, XmRStringTable,
	sizeof(String *), Offset(accelerator),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonAcceleratorText, XmCButtonAcceleratorText, XmRXmStringTable,
	sizeof(XmStringTable), Offset(accelerator_text),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonCount, XmCButtonCount, XmRInt,
	sizeof(int), Offset(count),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNbuttonMnemonicCharSets, XmCButtonMnemonicCharSets, XmRStringTable,
	sizeof(String *), Offset(mnemonic_charset),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonMnemonics, XmCButtonMnemonics, XmRKeySymTable,
	sizeof(XmKeySymTable), Offset(mnemonic),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttons, XmCButtons, XmRXmStringTable,
	sizeof(XmStringTable), Offset(label_string),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonSet, XmCButtonSet, XmRInt,
	sizeof(int), Offset(button_set),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNbuttonType, XmCButtonType, XmRButtonType,
	sizeof(XtPointer), Offset(button_type),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNoptionLabel, XmCOptionLabel, XmRXmString,
	sizeof(XmString), Offset(option_label),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNoptionMnemonic, XmCOptionMnemonic, XmRKeySym,
	sizeof(KeySym), Offset(option_mnemonic),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNpostFromButton, XmCPostFromButton, XmRInt,
	sizeof(int), Offset(post_from_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNsimpleCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(callback),
	XmRImmediate, (XtPointer)NULL
    }
};

Widget
_XmCreateSimpleGadget(char *name, Widget parent, int def_type,
	   XmSimpleMenu data, int which, int call_data, ArgList args, int argc)
{
    WidgetClass bclass = NULL;
    Arg alist[20];
    int cnt;
    Widget ret;
    ArgList combined;

    cnt = 0;

    if (data->button_type && data->button_type[which])
    {
	def_type = data->button_type[which];
    }

    switch (def_type)
    {
    case XmCASCADEBUTTON:
	bclass = xmCascadeButtonGadgetClass;
	break;

    case XmTOGGLEBUTTON:
	bclass = xmToggleButtonGadgetClass;
	XtSetArg(alist[cnt], XmNindicatorType, XmN_OF_MANY);
	cnt++;
	XtSetArg(alist[cnt], XmNvisibleWhenOff, True);
	cnt++;
	break;

    case XmDOUBLE_SEPARATOR:
	bclass = xmSeparatorGadgetClass;
	XtSetArg(alist[cnt], XmNseparatorType, XmDOUBLE_LINE);
	cnt++;
	break;

    case XmPUSHBUTTON:
	bclass = xmPushButtonGadgetClass;
	break;

    case XmRADIOBUTTON:
	bclass = xmToggleButtonGadgetClass;
	XtSetArg(alist[cnt], XmNindicatorType, XmONE_OF_MANY);
	cnt++;
	XtSetArg(alist[cnt], XmNvisibleWhenOff, True);
	cnt++;
	break;

    case XmSEPARATOR:
	bclass = xmSeparatorGadgetClass;
	break;

    case XmTITLE:
	bclass = xmLabelGadgetClass;
	break;

    default:
	_XmError(parent, "Simple* Gadget type not defined! %d", def_type);
	break;
    }

    if (data->accelerator && data->accelerator[which])
    {
	XtSetArg(alist[cnt], XmNaccelerator,
		 data->accelerator[which]);
	cnt++;
    }

    if (data->accelerator_text && data->accelerator_text[which])
    {
	XtSetArg(alist[cnt], XmNacceleratorText,
		 data->accelerator_text[which]);
	cnt++;
    }

    if (data->mnemonic_charset && data->mnemonic_charset[which])
    {
	XtSetArg(alist[cnt], XmNmnemonicCharSet,
		 data->mnemonic_charset[which]);
	cnt++;
    }

    if (data->mnemonic && data->mnemonic[which])
    {
	XtSetArg(alist[cnt], XmNmnemonic,
		 data->mnemonic[which]);
	cnt++;
    }

    if (data->label_string && data->label_string[which])
    {
	XtSetArg(alist[cnt], XmNlabelString,
		 data->label_string[which]);
	cnt++;
    }

    combined = XtMergeArgLists(alist, cnt, args, argc);
    ret = XtCreateManagedWidget(name, bclass, parent, combined, cnt + argc);
    XtFree((char *)combined);

    if (data->callback)
    {
	if (bclass == xmToggleButtonGadgetClass)
	{
	    XtAddCallback(ret, XmNvalueChangedCallback,
			  data->callback, (XtPointer)(long)call_data);
	}
	else if (bclass == xmPushButtonGadgetClass ||
		 bclass == xmCascadeButtonGadgetClass)
	{
	    XtAddCallback(ret, XmNactivateCallback,
			  data->callback, (XtPointer)(long)call_data);
	}
    }

    return ret;
}


Widget
XmCreateSimpleCheckBox(Widget parent,
		       char *name,
		       Arg *arglist,
		       Cardinal argcount)
{
    Widget rc;
    Arg myArgList[5];
    char buf[32];
    int i, n;
    ArgList combined;
    XmSimpleMenuRec data;

#ifdef	SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    memset((void *)&data, 0, sizeof(XmSimpleMenuRec));

    n = 0;
    XtSetArg(myArgList[n], XmNrowColumnType, XmWORK_AREA);
    n++;
    XtSetArg(myArgList[n], XmNradioAlwaysOne, False);
    n++;
    XtSetArg(myArgList[n], XmNisHomogeneous, True);
    n++;
    XtSetArg(myArgList[n], XmNentryClass, xmToggleButtonGadgetClass);
    n++;
    XtSetArg(myArgList[n], XmNpacking, XmPACK_COLUMN);
    n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);
    n += argcount;

    rc = XtCreateWidget(name, xmRowColumnWidgetClass, parent,
			combined, n);

    XtGetApplicationResources(rc, (XtPointer)&data,
			      simple_resources, XtNumber(simple_resources),
			      combined, n);

    for (i = 0; i < data.count; i++)
    {
	sprintf(buf, "button_%d", i);
	_XmCreateSimpleGadget(buf, rc, XmTOGGLEBUTTON,
			      &data, i, i, combined, n);
    }

    XtFree((char *)combined);

    return rc;
}


Widget
XmCreateSimpleMenuBar(Widget parent,
		      char *name,
		      Arg *arglist,
		      Cardinal argcount)
{
    Widget rc;
    Arg myArgList[5];
    char buf[32];
    int i, n;
    ArgList combined;
    XmSimpleMenuRec data;

#ifdef	SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    memset((void *)&data, 0, sizeof(XmSimpleMenuRec));

    n = 0;
    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_BAR);
    n++;
    XtSetArg(myArgList[n], XmNorientation, XmHORIZONTAL);
    n++;
    XtSetArg(myArgList[n], XmNpacking, XmPACK_TIGHT);
    n++;
    XtSetArg(myArgList[n], XmNentryClass, xmCascadeButtonGadgetClass);
    n++;
    XtSetArg(myArgList[n], XmNisHomogeneous, True);
    n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);
    n += argcount;

    rc = XtCreateWidget(name, xmRowColumnWidgetClass, parent,
			combined, n);

    XtGetApplicationResources(rc, (XtPointer)&data,
			      simple_resources, XtNumber(simple_resources),
			      combined, n);

    for (i = 0; i < data.count; i++)
    {
	sprintf(buf, "button_%d", i);
	_XmCreateSimpleGadget(buf, rc, XmCASCADEBUTTON,
			      &data, i, i, combined, n);
    }

    XtFree((char *)combined);

    return rc;
}


Widget
XmCreateSimpleOptionMenu(Widget parent,
			 char *name,
			 Arg *arglist,
			 Cardinal argcount)
{
    Widget rc, label, button;
    char buf[32];
    int i;
    XmSimpleMenuRec data;
    int scnt = 0;
    int bcnt = 0;
    int lcnt = 0;


#ifdef	SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    memset((void *)&data, 0, sizeof(XmSimpleMenuRec));

    XtGetSubresources(parent, (XtPointer)&data, name, XmCSimpleOptionMenu,
		      simple_resources, XtNumber(simple_resources),
		      arglist, argcount);

    rc = XmCreateOptionMenu(parent, name, arglist, argcount);

    RC_OptionSubMenu(rc) = XmCreatePulldownMenu(rc, name, arglist, argcount);

    button = XmOptionButtonGadget(rc);
    if (button)
    {
	XtVaSetValues(button, XmNsubMenuId, RC_OptionSubMenu(rc), NULL);

	if (data.option_mnemonic)
	{
	    XtVaSetValues(button, XmNmnemonic, data.option_mnemonic, NULL);
	}
    }

    if (data.option_label)
    {
	label = XmOptionLabelGadget(rc);
	if (label)
	{
	    XtVaSetValues(label, XmNlabelString, data.option_label, NULL);
	}
    }

    for (i = 0; i < data.count; i++)
    {
	if (data.button_type)
	{
	    switch (data.button_type[i])
	    {
	    case XmDOUBLE_SEPARATOR:
	    case XmSEPARATOR:
		sprintf(buf, "separator_%d", scnt);
		scnt++;
		break;

	    case XmTITLE:
		sprintf(buf, "label_%d", lcnt);
		lcnt++;
		break;

	    case XmCASCADEBUTTON:
	    case XmPUSHBUTTON:
	    default:
		sprintf(buf, "button_%d", bcnt);
		bcnt++;
		break;
	    }
	    {
	    Widget tmp;

		tmp = _XmCreateSimpleGadget(buf, RC_OptionSubMenu(rc),
				      data.button_type[i],
				      &data, i, bcnt - 1, arglist, argcount);
		if (data.button_type[i] == XmPUSHBUTTON && data.button_set == bcnt - 1)
		{
		    XtVaSetValues(rc,
			    XmNmenuHistory, tmp,
			    NULL);
		}
	    }
	}
	else
	{
	    sprintf(buf, "button_%d", bcnt);
	    bcnt++;
	    if (RC_Type(rc) == XmMENU_BAR)
	    {
		_XmCreateSimpleGadget(buf, RC_OptionSubMenu(rc),
				      XmCASCADEBUTTON,
				      &data, i, bcnt - 1, arglist, argcount);
	    }
	    else
	    {
		_XmCreateSimpleGadget(buf, RC_OptionSubMenu(rc),
				      XmPUSHBUTTON,
				      &data, i, bcnt - 1, arglist, argcount);
	    }
	}
    }

    /* set the default button */
    if (data.button_set >= 0 && data.button_set < data.count)
    {
	/* rws 12 Aug 1999
	   Don't set the label if the value is NULL, just pick it up from
	   the resource database by default.
	   Xquote Options->Updates...
	 */
	/*
	if (data.label_string[data.button_set])
	{
	    XtVaSetValues(button, XmNlabelString,
		      data.label_string[data.button_set], NULL);
	}
	*/
    }

    return rc;
}


Widget
XmCreateSimplePopupMenu(Widget parent,
			char *name,
			Arg *arglist,
			Cardinal argcount)
{
    Widget rc;
    char buf[32];
    int i;
    XmSimpleMenuRec data;
    int scnt = 0;
    int tcnt = 0;
    int bcnt = 0;

#ifdef	SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    memset((void *)&data, 0, sizeof(XmSimpleMenuRec));

    rc = XmCreatePopupMenu(parent, name, arglist, argcount);

    XtGetApplicationResources(rc, (XtPointer)&data,
			      simple_resources, XtNumber(simple_resources),
			      arglist, argcount);

    for (i = 0; i < data.count; i++)
    {
	if (data.button_type)
	{
	    switch (data.button_type[i])
	    {
	    case XmDOUBLE_SEPARATOR:
	    case XmSEPARATOR:
		sprintf(buf, "separator_%d", scnt);
		scnt++;
		break;

	    case XmTITLE:
		sprintf(buf, "label_%d", tcnt);
		tcnt++;
		break;

	    case XmCHECKBUTTON:
	    case XmRADIOBUTTON:
	    case XmPUSHBUTTON:
	    case XmCASCADEBUTTON:
	    default:
		sprintf(buf, "button_%d", bcnt);
		bcnt++;
		break;
	    }

	    _XmCreateSimpleGadget(buf, rc, data.button_type[i],
				  &data, i, bcnt - 1, arglist, argcount);
	}
	else
	{
	    sprintf(buf, "button_%d", bcnt);
	    bcnt++;
	    _XmCreateSimpleGadget(buf, rc, XmPUSHBUTTON,
				  &data, i, bcnt - 1, arglist, argcount);
	}
    }

    return rc;
}


Widget
XmCreateSimplePulldownMenu(Widget parent,
			   char *name,
			   Arg *arglist,
			   Cardinal argcount)
{
    Widget rc;
    char buf[64];
    int i;
    XmSimpleMenuRec data;
    int num_children;
    Widget *wlist;
    int scnt = 0;
    int tcnt = 0;
    int bcnt = 0;

#ifdef	SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    memset((void *)&data, 0, sizeof(XmSimpleMenuRec));

    rc = XmCreatePulldownMenu(parent, name, arglist, argcount);

    XtGetApplicationResources(XtParent(rc), (XtPointer)&data,
			      simple_resources, XtNumber(simple_resources),
			      arglist, argcount);

    /* find the button that the new menu is supposed to posted from */
    XtVaGetValues(parent,
		  XmNnumChildren, &num_children,
		  XmNchildren, &wlist,
		  NULL);

#if 0
    /* rws 25 Jan 1999
       vasimple/test5 shows that this is not correct.  This would only work
       if the menu that this menu is being posted from is also a simple type
       menu.  In this test it is not, and therefore will not find a button
       of the correct name.
     */
    sprintf(buf, "button_%d", data.post_from_button);
    for (i = 0; i < num_children; i++)
    {
	if (strcmp(XtName(wlist[i]), buf) == 0)
	{
	    break;
	}
    }

    if (i == num_children)
    {
	/* fixme: what do we do here? */
	/* rws 5 Dec 1997
	   printf("something bad happened! %d\n",i);
	   return rc;
	 */
    }
    else
    {

	/* add the new pulldown menu to the cascade button */
	XtVaSetValues(wlist[i], XmNsubMenuId, rc, NULL);
    }
#else
    {
    int count = 0;

	for (i = 0; i < num_children; i++)
	{
	    if (!XmIsTearOffButton(wlist[i]) && 
	         (XmIsPushButton(wlist[i]) ||
	          XmIsPushButtonGadget(wlist[i]) ||
	          XmIsToggleButton(wlist[i]) ||
	          XmIsToggleButtonGadget(wlist[i]) ||
	          XmIsCascadeButton(wlist[i]) ||
	          XmIsCascadeButtonGadget(wlist[i])))
	    {
		if (count == data.post_from_button)
		{
		    break;
		}
		count++;
	    }
	}
	if (i != num_children)
	{
	    XtVaSetValues(wlist[i], XmNsubMenuId, rc, NULL);
	}
	else
	{
	}
    }
#endif

    /* create the buttons in the menu */

    for (i = 0; i < data.count; i++)
    {
	if (data.button_type)
	{
	    switch (data.button_type[i])
	    {
	    case XmDOUBLE_SEPARATOR:
	    case XmSEPARATOR:
		sprintf(buf, "separator_%d", scnt);
		scnt++;
		break;

	    case XmTITLE:
		sprintf(buf, "label_%d", tcnt);
		tcnt++;
		break;

	    case XmCHECKBUTTON:
	    case XmRADIOBUTTON:
	    case XmPUSHBUTTON:
	    case XmCASCADEBUTTON:
	    default:
		sprintf(buf, "button_%d", bcnt);
		bcnt++;
		break;
	    }
	    _XmCreateSimpleGadget(buf, rc, data.button_type[i],
				  &data, i, bcnt - 1, arglist, argcount);
	}
	else
	{
	    sprintf(buf, "button_%d", bcnt);
	    bcnt++;
	    _XmCreateSimpleGadget(buf, rc, XmPUSHBUTTON,
				  &data, i, bcnt - 1, arglist, argcount);
	}
    }

    return rc;
}


Widget
XmCreateSimpleRadioBox(Widget parent,
		       char *name,
		       Arg *arglist,
		       Cardinal argcount)
{
    Widget rc;
    Arg myArgList[6];
    char buf[32];
    int i, n;
    ArgList combined;
    XmSimpleMenuRec data;
    Widget tgl;

#ifdef	SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    memset((void *)&data, 0, sizeof(XmSimpleMenuRec));

    n = 0;
    XtSetArg(myArgList[n], XmNrowColumnType, XmWORK_AREA); n++;
    XtSetArg(myArgList[n], XmNradioAlwaysOne, True); n++;
    XtSetArg(myArgList[n], XmNradioBehavior, True); n++;
    XtSetArg(myArgList[n], XmNisHomogeneous, True); n++;
    XtSetArg(myArgList[n], XmNentryClass, xmToggleButtonGadgetClass); n++;
    XtSetArg(myArgList[n], XmNpacking, XmPACK_COLUMN); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);
    n += argcount;

    rc = XtCreateWidget(name, xmRowColumnWidgetClass, parent,
			combined, n);

    XtGetApplicationResources(rc, (XtPointer)&data,
			      simple_resources, XtNumber(simple_resources),
			      combined, n);

    for (i = 0; i < data.count; i++)
    {
	sprintf(buf, "button_%d", i);
	tgl = _XmCreateSimpleGadget(buf, rc, XmRADIOBUTTON, &data, i, i,
				    combined, n);
	if ((i == data.button_set) &&
	    XmIsToggleButton(tgl))
	{
	    XmToggleButtonSetState(tgl, True, False);
	}
	else if ((i == data.button_set) &&
	         XmIsToggleButtonGadget(tgl))
	{
	    XmToggleButtonGadgetSetState(tgl, True, False);
	}
    }

    XtFree((char *)combined);

    return rc;
}
