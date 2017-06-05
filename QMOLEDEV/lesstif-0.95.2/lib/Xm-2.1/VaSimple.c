/**
 *
 * $Id: VaSimple.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
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
 *
 * Portions of this work are derived from X11R6/xc/lib/Xt/Varargs.c, which
 * is distributed under the following copyright:
 *
 * Copyright (c) 1985, 1986, 1987, 1988, 1989, 1994  X Consortium
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Except as contained in this notice, the name of the X Consortium shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from the X Consortium.
 *
 **/

static const char rcsid[] = "$Id: VaSimple.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/PushBGP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/VaSimpleP.h>

#include <XmI/DebugUtil.h>


/* this seems realistic to me */
#define MAX_EMBEDDED_BUTTONS	512
static XmButtonType button_types[MAX_EMBEDDED_BUTTONS];
static XmString button_strings[MAX_EMBEDDED_BUTTONS];
static KeySym button_mnemonics[MAX_EMBEDDED_BUTTONS];
static String button_accel[MAX_EMBEDDED_BUTTONS];
static XmString button_acc_text[MAX_EMBEDDED_BUTTONS];


extern Widget _XtCreateWidget(String,
			      WidgetClass,
			      Widget,
			      ArgList, Cardinal,
			      XtTypedArgList, Cardinal);


#if XmVERSION > 1
void _XmCountVaList(va_list var,
 		   int *button_count,
		   int *args_count,
		   int *typed_count,
		   int *total_count);
			   
void _XmVaToTypedArgList(va_list var,
			int max_count,
			XtTypedArgList *args_return,
			Cardinal *num_args_return);
#endif

/*
 *    Given a nested list, _XmCountNestedList() returns counts of the
 *    total number of attribute-value pairs and the count of those
 *    attributes that are typed. The list is counted recursively.
 */
static void
_XmCountNestedList(XtTypedArgList avlist, int *total_count, int *typed_count)
{
    for (; avlist->name != NULL; avlist++)
    {
	if (strcmp(avlist->name, XtVaNestedList) == 0)
	{
	    _XmCountNestedList((XtTypedArgList)avlist->value, total_count,
			       typed_count);
	}
	else
	{
	    if (avlist->type != NULL)
	    {
		++(*typed_count);
	    }
	    ++(*total_count);
	}
    }
}


/*
 *    Given a variable length attribute-value list, _XmCountVaList()
 *    returns counts of the total number of attribute-value pairs,
 *    and the count of the number of those attributes that are typed.
 *    The list is counted recursively.
 */
void
_XmCountVaList(va_list var, int *button_count, int *args_count,
	       int *typed_count, int *total_count)
{
    String attr;

    *total_count = 0;
    *typed_count = 0;
    *button_count = 0;
    *args_count = 0;
    memset((void *)button_types, 0, MAX_EMBEDDED_BUTTONS * sizeof(unsigned char));
    memset((void *)button_strings, 0, MAX_EMBEDDED_BUTTONS * sizeof(XmString));
    memset((void *)button_mnemonics, 0, MAX_EMBEDDED_BUTTONS * sizeof(KeySym));
    memset((void *)button_accel, 0, MAX_EMBEDDED_BUTTONS * sizeof(String));
    memset((void *)button_acc_text, 0, MAX_EMBEDDED_BUTTONS * sizeof(XmString));

    for (attr = va_arg(var, String); attr != NULL;
	 attr = va_arg(var, String))
    {
	if (strcmp(attr, XtVaTypedArg) == 0)
	{
	    va_arg(var, String);
	    va_arg(var, String);
	    va_arg(var, XtArgVal);
	    va_arg(var, int);
	    ++(*total_count);
	    ++(*typed_count);
	}
	else if (strcmp(attr, XtVaNestedList) == 0)
	{
	    _XmCountNestedList(va_arg(var, XtTypedArgList),
			       total_count,
			       typed_count);
	}
	else if (strcmp(attr, XmVaCASCADEBUTTON) == 0)
	{
	    /* label - XmString, mnemonic - KeySym */
	    button_types[*button_count] = XmCASCADEBUTTON;
	    button_strings[*button_count] = va_arg(var, XmString);
	    button_mnemonics[*button_count] = va_arg(var, KeySym);
	    ++(*button_count);
	}
	else if (strcmp(attr, XmVaCHECKBUTTON) == 0 ||
		 strcmp(attr, XmVaPUSHBUTTON) == 0 ||
		 strcmp(attr, XmVaRADIOBUTTON) == 0 ||
		 strcmp(attr, XmVaTOGGLEBUTTON) == 0)
	{
	    if (strcmp(attr, XmVaCHECKBUTTON) == 0)
	    {
		button_types[*button_count] = XmCHECKBUTTON;
	    }

	    else if (strcmp(attr, XmVaPUSHBUTTON) == 0)
	    {
		button_types[*button_count] = XmPUSHBUTTON;
	    }

	    else if (strcmp(attr, XmVaRADIOBUTTON) == 0)
	    {
		button_types[*button_count] = XmRADIOBUTTON;
	    }

	    else if (strcmp(attr, XmVaTOGGLEBUTTON) == 0)
	    {
		button_types[*button_count] = XmTOGGLEBUTTON;
	    }

	    /* label - XmString, mnemonic - KeySym, accelerator - String,
	       accelerator_text - XmString */
	    button_strings[*button_count] = va_arg(var, XmString);
	    button_mnemonics[*button_count] = va_arg(var, KeySym);
	    button_accel[*button_count] = va_arg(var, String);
	    button_acc_text[*button_count] = va_arg(var, XmString);
	    ++(*button_count);
	}
	else if (strcmp(attr, XmVaTITLE) == 0)
	{
	    /* title - XmString */
	    if (strcmp(attr, XmVaTITLE) == 0)
	    {
		button_types[*button_count] = XmTITLE;
	    }
	    button_strings[*button_count] = va_arg(var, XmString);
	    ++(*button_count);
	}
	else if (strcmp(attr, XmVaDOUBLE_SEPARATOR) == 0 ||
		 strcmp(attr, XmVaSEPARATOR) == 0 ||
		 strcmp(attr, XmVaSINGLE_SEPARATOR) == 0)
	{
	    /* just eat it, and up the button count */
	    if (strcmp(attr, XmVaDOUBLE_SEPARATOR) == 0)
	    {
		button_types[*button_count] = XmDOUBLE_SEPARATOR;
	    }
	    else if (strcmp(attr, XmVaSEPARATOR) == 0)
	    {
		button_types[*button_count] = XmSEPARATOR;
	    }
	    else if (strcmp(attr, XmVaSINGLE_SEPARATOR) == 0)
	    {
		button_types[*button_count] = XmSEPARATOR;
	    }
	    ++(*button_count);
	}
	else
	{
	    va_arg(var, XtArgVal);
	    ++(*total_count);
	    ++(*args_count);
	}
    }
}


/*
 *    _XmTypedArgToArg() invokes a resource converter to convert the
 *    passed typed arg into a name/value pair and stores the name/value
 *    pair in the passed Arg structure.  If memory is allocated for the
 *    converted value, the address is returned in the value field of 
 *    memory_return; otherwise that field is NULL.  The function returns
 *    1 if the conversion succeeded and 0 if the conversion failed.
 */
static int
_XmTypedArgToArg(Widget widget, XtTypedArgList typed_arg, ArgList arg_return,
		 XtResourceList resources, Cardinal num_resources,
		 ArgList memory_return)
{
    String to_type = NULL;
    XrmValue from_val, to_val;


    if (widget == NULL)
    {
	_XmWarning(NULL, "Attempt to convert TypedArg for NULL Widget.");
	return (0);
    }

    /* again we assume that the XtResourceList is un-compiled */

    for (; num_resources--; resources++)
    {
	if (strcmp(typed_arg->name, resources->resource_name) == 0)
	{
	    to_type = resources->resource_type;
	    break;
	}
    }

    if (to_type == NULL)
    {
	_XmWarning(widget, "Unable to find type of resource for conversion");
	return (0);
    }

    to_val.addr = NULL;
    from_val.size = typed_arg->size;
    if ((strcmp(typed_arg->type, XtRString) == 0) ||
	(typed_arg->size > (int)sizeof(XtArgVal)))
    {
	from_val.addr = (XPointer)typed_arg->value;
    }
    else
    {
	from_val.addr = (XPointer)&typed_arg->value;
    }

    XtConvertAndStore(widget, typed_arg->type, &from_val, to_type, &to_val);

    if (to_val.addr == NULL)
    {
	_XmWarning(widget, "Type conversion failed");
	return (0);
    }

    arg_return->name = typed_arg->name;
    memory_return->value = (XtArgVal)NULL;

    if (strcmp(to_type, XtRString) == 0)
    {
	arg_return->value = (XtArgVal)to_val.addr;
    }
    else
    {
	if (to_val.size == sizeof(long))
	{
	    arg_return->value = (XtArgVal)*(long *)to_val.addr;
	}
	else if (to_val.size == sizeof(short))
	{
	    arg_return->value = (XtArgVal)*(short *)to_val.addr;
	}
	else if (to_val.size == sizeof(char))
	{
	    arg_return->value = (XtArgVal)*(char *)to_val.addr;
	}
	else if (to_val.size == sizeof(XtArgVal))
	{
	    arg_return->value = *(XtArgVal *)to_val.addr;
	}
	else if (to_val.size > sizeof(XtArgVal))
	{
	    arg_return->value = (XtArgVal)XtMalloc(to_val.size);
	    memory_return->value = (XtArgVal)arg_return->value;
	    memcpy((void *)arg_return->value, to_val.addr, to_val.size);
	}
    }

    return (1);
}


/*
 *    _XmNestedArgtoArg() converts the passed nested list into
 *    an ArgList/count.
 */
static int
_XmNestedArgtoArg(Widget widget, XtTypedArgList avlist, ArgList args,
		  XtResourceList resources, Cardinal num_resources,
		  ArgList memory_return)
{
    int count = 0;

    for (; avlist->name != NULL; avlist++)
    {
	if (avlist->type != NULL)
	{
	    /* If widget is NULL, the typed arg is ignored */
	    if (widget != NULL)
	    {
		/* this is a typed arg */
		count += _XmTypedArgToArg(widget, avlist, (args + count),
					  resources, num_resources,
					  (memory_return + count));
	    }
	}
	else if (strcmp(avlist->name, XtVaNestedList) == 0)
	{
	    count += _XmNestedArgtoArg(widget, (XtTypedArgList)avlist->value,
				       (args + count), resources, num_resources,
				       (memory_return + count));
	}
	else
	{
	    (args + count)->name = avlist->name;
	    (args + count)->value = avlist->value;
	    ++count;
	}
    }

    return (count);
}


/*
 * Free memory allocated through _XmVaToArgList.  The actual args array
 * size is expected to be total_count * 2, where total_count is the number
 * of elements needed for resource representations.  The lower half of the
 * array contains pairs of resource names and values as usual.  For each
 * element [n] in the lower half of the array, the value field of the
 * corresponding element [n + total_count] in the upper half of the array
 * has been pressed into service in order to note whether the resource value
 * is a pointer to memory that was allocated in _XmTypedArgToArg.  In the
 * upper half, if the value field is not NULL, it contains the address of
 * memory which should now be freed.  That memory could have been allocated
 * only as a result of the conversion of typed arguments.  Therefore, if
 * there were no typed arguments in the original varargs, there is no need
 * to examine the upper half of the array.  In the choice of data structure
 * to make this representation, priority was given to the wish to retrofit
 * the release of memory around the existing signature of _XmVaToArgList.
 */
static void
_XmFreeArgList(ArgList args, int total_count, int typed_count)
{
    ArgList p;

    if (args)
    {
	if (typed_count)
	{
	    for (p = args + total_count; total_count--; ++p)
	    {
		if (p->value)
		    XtFree((char *)p->value);
	    }
	}
	XtFree((char *)args);
    }
}


/*
 * Description: Retreives the normal and constraint resources
 *              for this widget.
 * Arguments: widget - the widget.
 * RETURNED        res_list - the list of resource for this widget
 * RETURNED        number - the number of resources in the above list.
 * Returns: none
 */
static void
_XmGetResources(Widget widget, XtResourceList *res_list, Cardinal *number)
{
    Widget parent = XtParent(widget);

    XtInitializeWidgetClass(XtClass(widget));
    XtGetResourceList(XtClass(widget), res_list, number);

    if (!XtIsShell(widget) && parent && XtIsConstraint(parent))
    {
	XtResourceList res, constraint, cons_top;
	Cardinal num_constraint, temp;

	XtGetConstraintResourceList(XtClass(parent), &constraint,
				    &num_constraint);

	cons_top = constraint;
	*res_list = (XtResourceList)XtRealloc((char *)*res_list,
					      ((*number + num_constraint) *
					       sizeof(XtResource)));

	for (temp = num_constraint, res = *res_list + *number;
	     temp != 0; temp--)
	{
	    *res++ = *constraint++;
	}

	*number += num_constraint;
	XtFree((char *)cons_top);
    }
}


/* 
 *    Given a variable argument list, _XmVaToArgList() returns the 
 *    equivalent ArgList and count. _XmVaToArgList() handles nested 
 *    lists and typed arguments.  If typed arguments are present, the
 *    ArgList should be freed with _XmFreeArgList.
 */
static void
_XmVaToArgList(Widget widget, va_list var, int max_count,
	       ArgList *args_return, Cardinal *num_args_return)
{
    String attr;
    int count;
    ArgList args = (ArgList)NULL;
    XtTypedArg typed_arg;
    XtResourceList resources = (XtResourceList)NULL;
    Cardinal num_resources;
    Boolean fetched_resource_list = False;

    if (max_count == 0)
    {
	*num_args_return = 0;
	*args_return = (ArgList)NULL;
	return;
    }

    max_count *= 2;
    args = (ArgList)XtMalloc((unsigned)(max_count * sizeof(Arg)));
    for (count = max_count; --count >= 0;)
    {
	args[count].value = (XtArgVal)0;
    }
    max_count /= 2;
    count = 0;

    for (attr = va_arg(var, String); attr != NULL;
	 attr = va_arg(var, String))
    {
	if (strcmp(attr, XtVaTypedArg) == 0)
	{
	    typed_arg.name = va_arg(var, String);
	    typed_arg.type = va_arg(var, String);
	    typed_arg.value = va_arg(var, XtArgVal);
	    typed_arg.size = va_arg(var, int);

	    /* if widget is NULL, typed args are ignored */
	    if (widget != NULL)
	    {
		if (!fetched_resource_list)
		{
		    _XmGetResources(widget, &resources, &num_resources);
		    fetched_resource_list = True;
		}
		count += _XmTypedArgToArg(widget, &typed_arg, &args[count],
					  resources, num_resources,
					  &args[max_count + count]);
	    }
	}
	else if (strcmp(attr, XtVaNestedList) == 0)
	{
	    if (widget != NULL || !fetched_resource_list)
	    {
		_XmGetResources(widget, &resources, &num_resources);
		fetched_resource_list = True;
	    }

	    count += _XmNestedArgtoArg(widget, va_arg(var, XtTypedArgList),
				       &args[count], resources, num_resources,
				       &args[max_count + count]);
	}
	else if (strcmp(attr, XmVaCASCADEBUTTON) == 0)
	{
	    /* label - XmString, mnemonic - KeySym */
	    va_arg(var, XmString);
	    va_arg(var, KeySym);
	}
	else if (strcmp(attr, XmVaCHECKBUTTON) == 0 ||
		 strcmp(attr, XmVaPUSHBUTTON) == 0 ||
		 strcmp(attr, XmVaRADIOBUTTON) == 0 ||
		 strcmp(attr, XmVaTOGGLEBUTTON) == 0)
	{
	    /* label - XmString, mnemonic - KeySym, accelerator - String,
	       accelerator_text - XmString */
	    va_arg(var, XmString);
	    va_arg(var, KeySym);
	    va_arg(var, String);
	    va_arg(var, XmString);
	}
	else if (strcmp(attr, XmVaTITLE) == 0)
	{
	    /* title - XmString */
	    va_arg(var, XmString);
	}
	else if (strcmp(attr, XmVaDOUBLE_SEPARATOR) == 0 ||
		 strcmp(attr, XmVaSEPARATOR) == 0 ||
		 strcmp(attr, XmVaSINGLE_SEPARATOR) == 0)
	{
	    /* just eat it */
	}
	else
	{
	    args[count].name = attr;
	    args[count].value = va_arg(var, XtArgVal);
	    count++;
	}
    }

    XtFree((char *)resources);

    *num_args_return = (Cardinal)count;
    *args_return = (ArgList)args;
}


static int
_XmNestedArgtoTypedArg(XtTypedArgList args,
		       XtTypedArgList avlist)
{
    int count = 0;

    for (; avlist->name != NULL; avlist++)
    {
	if (avlist->type != NULL)
	{
	    (args + count)->name = avlist->name;
	    (args + count)->type = avlist->type;
	    (args + count)->size = avlist->size;
	    (args + count)->value = avlist->value;
	    ++count;
	}
	else if (strcmp(avlist->name, XtVaNestedList) == 0)
	{
	    count += _XmNestedArgtoTypedArg((args + count),
					    (XtTypedArgList)avlist->value);
	}
	else
	{
	    (args + count)->name = avlist->name;
	    (args + count)->type = NULL;
	    (args + count)->value = avlist->value;
	    ++count;
	}
    }
    return (count);
}


/*
 *    Given a variable argument list, _XmVaToTypedArgList() returns 
 *    the equivalent TypedArgList. _XmVaToTypedArgList() handles nested
 *    lists.
 *    Note: _XmVaToTypedArgList() does not do type conversions.
 */
void
_XmVaToTypedArgList(va_list var, int max_count,
		    XtTypedArgList *args_return, Cardinal *num_args_return)
{
    XtTypedArgList args = NULL;
    String attr;
    int count;

    args = (XtTypedArgList)
	XtMalloc((unsigned)(max_count * sizeof(XtTypedArg)));

    for (attr = va_arg(var, String), count = 0; attr != NULL;
	 attr = va_arg(var, String))
    {
	if (strcmp(attr, XtVaTypedArg) == 0)
	{
	    args[count].name = va_arg(var, String);
	    args[count].type = va_arg(var, String);
	    args[count].value = va_arg(var, XtArgVal);
	    args[count].size = va_arg(var, int);
	    ++count;
	}
	else if (strcmp(attr, XtVaNestedList) == 0)
	{
	    count += _XmNestedArgtoTypedArg(&args[count],
					    va_arg(var, XtTypedArgList));
	}
	else if (strcmp(attr, XmVaCASCADEBUTTON) == 0)
	{
	    /* label - XmString, mnemonic - KeySym */
	    va_arg(var, XmString);
	    va_arg(var, KeySym);
	}
	else if (strcmp(attr, XmVaCHECKBUTTON) == 0 ||
		 strcmp(attr, XmVaPUSHBUTTON) == 0 ||
		 strcmp(attr, XmVaRADIOBUTTON) == 0 ||
		 strcmp(attr, XmVaTOGGLEBUTTON) == 0)
	{
	    /* label - XmString, mnemonic - KeySym, accelerator - String,
	       accelerator_text - XmString */
	    va_arg(var, XmString);
	    va_arg(var, KeySym);
	    va_arg(var, String);
	    va_arg(var, XmString);
	}
	else if (strcmp(attr, XmVaTITLE) == 0)
	{
	    /* title - XmString */
	    va_arg(var, XmString);
	}
	else if (strcmp(attr, XmVaDOUBLE_SEPARATOR) == 0 ||
		 strcmp(attr, XmVaSEPARATOR) == 0 ||
		 strcmp(attr, XmVaSINGLE_SEPARATOR) == 0)
	{
	    /* just eat it */
	}
	else
	{
	    args[count].name = attr;
	    args[count].type = NULL;
	    args[count].value = va_arg(var, XtArgVal);
	    ++count;
	}
    }

    *args_return = args;
    *num_args_return = count;
}


/*
 * note: The Va version of the "Simple" routines wipe out any
 * "Simple" creation resources the caller specified in the va_list.
 * This is poorly documented, but is Motif 1.2 behavior.
 */

Widget
XmVaCreateSimpleCheckBox(Widget parent,
			 String name,
			 XtCallbackProc callback,
			 ...)
{
    ArgList arglist;
    Arg myarglist[8];
    Cardinal n = 0;
    int total_count = 0;
    int args_count = 0;
    int button_count = 0;
    int typed_count = 0;
    va_list var;
    Widget rc;

    DEBUGOUT(_LtDebug(__FILE__, parent, "XmVaCreateSimpleCheckBox();\n"));

#ifdef SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    /* find out how many buttons, how many args, etc... we have 
     * (this also sets the button_strings, button_mnemonics, etc
     * arrays)
     */
    va_start(var, callback);
    _XmCountVaList(var, &button_count, &args_count, &typed_count, &total_count);
    va_end(var);

    /* set Simple resources */
    XtSetArg(myarglist[n], XmNsimpleCallback, callback); n++;
    XtSetArg(myarglist[n], XmNbuttonCount, button_count); n++;
    XtSetArg(myarglist[n], XmNbuttons, button_strings); n++;

#if 0
    /* the XmVaCreateSimpleCheckBox man page says these Simple
     * resources aren't used in the 1.2 release of Motif 
     */
    XtSetArg(myarglist[n], XmNbuttonMnemonics, button_mnemonics); n++;
    XtSetArg(myarglist[n], XmNbuttonAccelerators, button_accel); n++;
    XtSetArg(myarglist[n], XmNbuttonAcceleratorText, button_acc_text); n++;
#endif

    rc = XmCreateSimpleCheckBox(parent, name, myarglist, n);

    /* I'm not sure if this (the callers valist) list should be merged
     * into the list passed to the above XmCreateSimpleCheckBox() or
     * if it can be set afterward, but it is easier to set it
     * afterward
     */
    va_start(var, callback);
    _XmVaToArgList(rc, var, total_count, &arglist, &n);
    va_end(var);

    XtSetValues(rc, arglist, n);

    _XmFreeArgList(arglist, total_count, typed_count);

    return rc;
}


/*
 * I'm not 100% sure the XmVaCreateSimple*Menu* calls are correct,
 * since I can't really test them.
 * 
 * note: The Va version of the "Simple" routines wipe out any
 * "Simple" creation resources the caller specified in the va_list.
 * This is poorly documented, but is Motif 1.2 behavior.
 */
Widget
XmVaCreateSimpleMenuBar(Widget parent,
			String name,
			...)
{
    ArgList arglist;
    Arg myarglist[8];
    Cardinal n = 0;
    int total_count = 0;
    int args_count = 0;
    int button_count = 0;
    int typed_count = 0;
    va_list var;
    Widget rc;

    DEBUGOUT(_LtDebug(__FILE__, parent, "XmVaCreateSimpleMenuBar();\n"));

#ifdef SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    /* find out how many buttons, how many args, etc... we have 
     * (this also sets the button_strings, button_mnemonics, etc
     * arrays)
     */
    va_start(var, name);
    _XmCountVaList(var, &button_count, &args_count, &typed_count, &total_count);
    va_end(var);

    /* set Simple resources */
    XtSetArg(myarglist[n], XmNbuttonCount, button_count); n++;
    XtSetArg(myarglist[n], XmNbuttons, button_strings); n++;
    XtSetArg(myarglist[n], XmNbuttonMnemonics, button_mnemonics); n++;

    rc = XmCreateSimpleMenuBar(parent, name, myarglist, n);

    /* I'm not sure if this (the callers valist) list should be merged
     * into the list passed to the above XmCreateSimpleMenuBar() or
     * if it can be set afterward, but it is easier to set it
     * afterward
     */
    va_start(var, name);
    _XmVaToArgList(rc, var, total_count, &arglist, &n);
    va_end(var);

    XtSetValues(rc, arglist, n);

    _XmFreeArgList(arglist, total_count, typed_count);

    return rc;
}


/*
 * This sucker can be (and has been) tested with Xinvest, also with
 * testXm/vasimple/test4 .
 *
 * note: The Va version of the "Simple" routines wipe out any
 * "Simple" creation resources the caller specified in the va_list.
 * This is poorly documented, but is Motif 1.2 behavior.
 */
Widget
XmVaCreateSimpleOptionMenu(Widget parent,
			   String name,
			   XmString option_label,
			   KeySym option_mnemonic,
			   int button_set,
			   XtCallbackProc callback,
			   ...)
{
    ArgList arglist, combined;
    Arg myarglist[11];
    Cardinal n = 0;
    int total_count = 0, args_count = 0, button_count = 0, typed_count = 0,
      mine;
    va_list var;
    Widget rc = NULL;

    DEBUGOUT(_LtDebug(__FILE__, parent, "XmVaCreateSimpleOptionMenu();\n"));

#ifdef SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    /* find out how many buttons, how many args, etc... we have 
     * (this also sets the button_strings, button_mnemonics, etc
     * arrays)
     */
    va_start(var, callback);
    _XmCountVaList(var, &button_count, &args_count, &typed_count, &total_count);
    va_end(var);

    va_start(var, callback);
    _XmVaToArgList(rc, var, total_count, &arglist, &n);
    va_end(var);

    /* set Simple resources */
    mine = 0;
    XtSetArg(myarglist[mine], XmNbuttonSet, button_set); mine++;
    XtSetArg(myarglist[mine], XmNoptionLabel, option_label); mine++;
    XtSetArg(myarglist[mine], XmNoptionMnemonic, option_mnemonic); mine++;
    XtSetArg(myarglist[mine], XmNsimpleCallback, callback); mine++;
    XtSetArg(myarglist[mine], XmNbuttonCount, button_count); mine++;
    XtSetArg(myarglist[mine], XmNbuttonType, button_types); mine++;
    XtSetArg(myarglist[mine], XmNbuttons, button_strings); mine++;
    XtSetArg(myarglist[mine], XmNbuttonMnemonics, button_mnemonics); mine++;
    XtSetArg(myarglist[mine], XmNbuttonAccelerators, button_accel); mine++;
    XtSetArg(myarglist[mine], XmNbuttonAcceleratorText, button_acc_text);
    mine++;

    combined = XtMergeArgLists(myarglist, mine, arglist, n);
    rc = XmCreateSimpleOptionMenu(parent, name, combined, n + mine);
    XtFree((char *)combined);

    return rc;
}


/*
 * I'm not 100% sure the XmVaCreateSimple*Menu* calls are correct,
 * since I can't really test them.
 * 
 * note: The Va version of the "Simple" routines wipe out any
 * "Simple" creation resources the caller specified in the va_list.
 * This is poorly documented, but is Motif 1.2 behavior.
 */
Widget
XmVaCreateSimplePopupMenu(Widget parent,
			  String name,
			  XtCallbackProc callback,
			  ...)
{
    ArgList arglist;
    Arg myarglist[11];
    Cardinal n = 0;
    int total_count = 0;
    int args_count = 0;
    int button_count = 0;
    int typed_count = 0;
    va_list var;
    Widget rc;

    DEBUGOUT(_LtDebug(__FILE__, parent, "XmVaCreateSimplePopupMenu();\n"));

#ifdef SIMPLE_WALKUP
	/*
	 * Jex demonstrates that this shouldn't happen.
	 * TFM also says
	 *	"parent - Specifies the widget ID of the parent of the MenuShell"
	 * which seems to confirm that.
	 */
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    /* find out how many buttons, how many args, etc... we have 
     * (this also sets the button_strings, button_mnemonics, etc
     * arrays)
     */
    va_start(var, callback);
    _XmCountVaList(var, &button_count, &args_count, &typed_count, &total_count);
    va_end(var);

    /* set Simple resources */
    XtSetArg(myarglist[n], XmNsimpleCallback, callback); n++;
    XtSetArg(myarglist[n], XmNbuttonCount, button_count); n++;
    XtSetArg(myarglist[n], XmNbuttonType, button_types); n++;
    XtSetArg(myarglist[n], XmNbuttons, button_strings); n++;
    XtSetArg(myarglist[n], XmNbuttonMnemonics, button_mnemonics); n++;
    XtSetArg(myarglist[n], XmNbuttonAccelerators, button_accel); n++;
    XtSetArg(myarglist[n], XmNbuttonAcceleratorText, button_acc_text); n++;

    rc = XmCreateSimplePopupMenu(parent, name, myarglist, n);

    /* I'm not sure if this (the callers valist) list should be merged
     * into the list passed to the above XmCreateSimplePopupMenu() or
     * if it can be set afterward, but it is easier to set it
     * afterward
     */
    va_start(var, callback);
    _XmVaToArgList(rc, var, total_count, &arglist, &n);
    va_end(var);

    XtSetValues(rc, arglist, n);

    _XmFreeArgList(arglist, total_count, typed_count);

    return rc;
}


/*
 * I'm not 100% sure the XmVaCreateSimple*Menu* calls are correct,
 * since I can't really test them.
 * 
 * note: The Va version of the "Simple" routines wipe out any
 * "Simple" creation resources the caller specified in the va_list.
 * This is poorly documented, but is Motif 1.2 behavior.
 */
Widget
XmVaCreateSimplePulldownMenu(Widget parent,
			     String name,
			     int post_from_button,
			     XtCallbackProc callback,
			     ...)
{
    ArgList arglist;
    Arg myarglist[11];
    Cardinal n = 0;
    int total_count = 0;
    int args_count = 0;
    int button_count = 0;
    int typed_count = 0;
    va_list var;
    Widget rc;


    DEBUGOUT(_LtDebug(__FILE__, parent, "XmVaCreateSimplePulldownMenu();\n"));

#ifdef SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif


    /* find out how many buttons, how many args, etc... we have 
     * (this also sets the button_strings, button_mnemonics, etc
     * arrays)
     */
    va_start(var, callback);
    _XmCountVaList(var, &button_count, &args_count, &typed_count, &total_count);
    va_end(var);

    /* set Simple resources */
    XtSetArg(myarglist[n], XmNpostFromButton, post_from_button); n++;
    XtSetArg(myarglist[n], XmNsimpleCallback, callback); n++;
    XtSetArg(myarglist[n], XmNbuttonCount, button_count); n++;
    XtSetArg(myarglist[n], XmNbuttonType, button_types); n++;
    XtSetArg(myarglist[n], XmNbuttons, button_strings); n++;
    XtSetArg(myarglist[n], XmNbuttonMnemonics, button_mnemonics); n++;
    XtSetArg(myarglist[n], XmNbuttonAccelerators, button_accel); n++;
    XtSetArg(myarglist[n], XmNbuttonAcceleratorText, button_acc_text); n++;

    rc = XmCreateSimplePulldownMenu(parent, name, myarglist, n);

    /* I'm not sure if this (the callers valist) list should be merged
     * into the list passed to the above XmCreateSimplePulldownMenu() or
     * if it can be set afterward, but it is easier to set it
     * afterward
     */
    va_start(var, callback);
    _XmVaToArgList(rc, var, total_count, &arglist, &n);
    va_end(var);

    XtSetValues(rc, arglist, n);

    _XmFreeArgList(arglist, total_count, typed_count);

    return rc;
}


/*
 * note: The Va version of the "Simple" routines wipe out any
 * "Simple" creation resources the caller specified in the va_list.
 * This is poorly documented, but is Motif 1.2 behavior.
 */
Widget
XmVaCreateSimpleRadioBox(Widget parent,
			 String name,
			 int button_set,
			 XtCallbackProc callback,
			 ...)
{
    ArgList arglist;
    Arg myarglist[8];
    Cardinal n = 0;
    int total_count = 0;
    int args_count = 0;
    int button_count = 0;
    int typed_count = 0;
    va_list var;
    Widget rc;

    DEBUGOUT(_LtDebug(__FILE__, parent, "XmVaCreateSimpleRadioBox();\n"));

#ifdef SIMPLE_WALKUP
    while (parent && !XtIsComposite(parent))
    {
	parent = XtParent(parent);
    }
#endif

    /* find out how many buttons, how many args, etc... we have 
     * (this also sets the button_strings, button_mnemonics, etc
     * arrays)
     */
    va_start(var, callback);
    _XmCountVaList(var, &button_count, &args_count, &typed_count, &total_count);
    va_end(var);

    /* set Simple resources */
    XtSetArg(myarglist[n], XmNbuttonSet, button_set); n++;
    XtSetArg(myarglist[n], XmNsimpleCallback, callback); n++;
    XtSetArg(myarglist[n], XmNbuttonCount, button_count); n++;
    XtSetArg(myarglist[n], XmNbuttons, button_strings); n++;

#if 0
    /* the XmVaCreateSimpleRadioButton man page says these Simple
     * resources aren't used in the 1.2 release of Motif 
     */
    XtSetArg(myarglist[n], XmNbuttonMnemonics, button_mnemonics); n++;
    XtSetArg(myarglist[n], XmNbuttonAccelerators, button_accel); n++;
    XtSetArg(myarglist[n], XmNbuttonAcceleratorText, button_acc_text); n++;
#endif

    rc = XmCreateSimpleRadioBox(parent, name, myarglist, n);

    /* I'm not sure if this (the callers valist) list should be merged
     * into the list passed to the above XmCreateSimpleRadioBox() or
     * if it can be set afterward, but it is easier to set it
     * afterward
     */
    va_start(var, callback);
    _XmVaToArgList(rc, var, total_count, &arglist, &n);
    va_end(var);

    XtSetValues(rc, arglist, n);

    _XmFreeArgList(arglist, total_count, typed_count);

    return rc;
}
