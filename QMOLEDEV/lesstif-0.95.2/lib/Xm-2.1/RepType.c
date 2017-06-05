/**
 *
 * $Id: RepType.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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
static const char rcsid[] = "$Id: RepType.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/RepType.h>

#include <XmI/LTmisc.h>

#include <XmI/DebugUtil.h>


static XmRepTypeList rep_types = NULL;
static int number_of_types = 0;
static int max_number_of_types = 0;


/* Make a copy of a RepType entry.
 * The space must already be allocated.
 * Return the amount of character data copied.
 */

static int
__XmRepTypeCopyRecord(XmRepTypeEntry to, XmRepTypeEntry from, String *strs,
		      char *chars)
{
    int i;
    char *p, *cp;

    cp = chars;
    to->num_values = from->num_values;
    to->reverse_installed = from->reverse_installed;
    to->rep_type_id = from->rep_type_id;
    to->value_names = strs;
    to->rep_type_name = cp;
    p = from->rep_type_name;
    while ((*cp++ = *p++));
    if (from->values)
    {
	to->values = (unsigned char *)cp;
	p = (char *)from->values;
	for (i = 0; i < to->num_values; i++)
	    *cp++ = *p++;
    }
    else
	to->values = NULL;
    for (i = 0; i < to->num_values; i++)
    {
	strs[i] = cp;
	p = from->value_names[i];
	while ((*cp++ = *p++));
    }
    return cp - chars;
}

static int
__XmRepTypeStringToValue(XmRepTypeEntry entry,
			 String value_name)
{
    int length = strlen(value_name);
    char *pStr;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "__XmRepTypeStringToValue(%s,%s)\n",
		      entry->rep_type_name, value_name));

    pStr = value_name;
    if ((length > 2) &&
	(tolower(pStr[0]) == 'x') && (tolower(pStr[1]) == 'm'))
    {
	pStr += 2;
    }

    for (i = entry->num_values - 1; i >= 0; --i)
    {
	if (strcasecmp(pStr, entry->value_names[i]) == 0)
	{
	    break;
	}
    }

    if (i >= 0)
    {
	if (entry->values)
	{
	    i = entry->values[i];
	}
	return i;
    }
    else
	return -1;
}

static String
__XmRepTypeValueToString(XmRepTypeEntry entry,
			 unsigned char value)
{
    int i;
    Boolean error;

    if (entry->values == NULL)
    {
	i = value;
	error = value >= entry->num_values;
    }
    else
    {
	error = True;
	for (i = 0; i < entry->num_values; i++)
	{
	    if (entry->values[i] == value)
	    {
		error = False;
		break;
	    }
	}
    }

    if (error)
    {
	return NULL;
    }
    else
    {
	return entry->value_names[i];
    }
}

static Boolean
__XmCvtStringToRep(Display *display,
		   XrmValue *args,
		   Cardinal *num_args,
		   XrmValue *from,
		   XrmValue *to,
		   XtPointer *converter_data)
{
    XmRepTypeEntry entry = rep_types + *(int *)args[0].addr;
    int result;
    static unsigned char value;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "__XmCvtStringToRep()\n"));

    if (from->addr == NULL)
    {
	XtDisplayStringConversionWarning(display, from->addr,
					 entry->rep_type_name);
	return False;
    }

    result = __XmRepTypeStringToValue(entry, (String)from->addr);

    if (result < 0)
    {
	XtDisplayStringConversionWarning(display, from->addr,
					 entry->rep_type_name);

	return False;
    }

    value = (unsigned char)result;

    if (to->addr == NULL)
    {
	to->addr = (XPointer)&value;
    }
    else
    {
	if (to->size >= sizeof value)
	{
	    *(unsigned char *)to->addr = value;
	}
	else
	{
	    XtDisplayStringConversionWarning(display, from->addr,
					     entry->rep_type_name);

	    return False;
	}
    }
    to->size = sizeof value;

    return True;
}


static Boolean
__XmCvtRepToString(Display *display,
		   XrmValue *args,
		   Cardinal *num_args,
		   XrmValue *from,
		   XrmValue *to,
		   XtPointer *converter_data)
{
    XmRepTypeEntry entry = rep_types + *(int *)args[0].addr;
    static String value;

    if (from->addr == NULL)
    {
	String params[1];
	Cardinal numParams = 1;

	params[0] = (String)from->addr;
	XtAppWarningMsg(XtDisplayToApplicationContext(display),
			"conversionError", entry->rep_type_name,
			"XtToolkitError",
			"Cannot convert value NULL of type %s to type string",
			params, &numParams);

	return False;
    }

    value = __XmRepTypeValueToString(entry, *(unsigned char *)from->addr);

    if (value == NULL)
    {
	String params[2];
	Cardinal numParams = 2;
	char ASCIIValue[4];

    error:
	value = entry->value_names[0];
	sprintf(ASCIIValue, "%i", *(unsigned char *)from->addr);
	params[0] = ASCIIValue;
	params[1] = entry->rep_type_name;
	XtAppWarningMsg(XtDisplayToApplicationContext(display),
			"conversionError", entry->rep_type_name,
			"XtToolkitError",
			"Cannot convert value %s of type %s to type string",
			params, &numParams);

	return False;
    }

    if (to->addr == NULL)
    {
	to->addr = (XPointer)&value;
    }
    else
    {
	if (to->size >= sizeof value)
	{
	    *(String *)to->addr = value;
	}
	else
	{
	    goto error;
	}
    }
    to->size = sizeof value;

    return True;
}

void
XmCvtStringToUnitType(XrmValuePtr args, Cardinal *num_args,
		      XrmValue *from_val, XrmValue *to_val)
{
    int result;
    static unsigned char value;

    /* Much like __XmCvtStringToRep, but there's no display
     * so it can't be called directly.
     */

    if (from_val->addr == NULL)
    {
	XtStringConversionWarning(from_val->addr, XmRUnitType);
	to_val->addr = NULL;
	to_val->size = 0;
	return;
    }

    result = __XmRepTypeStringToValue(rep_types + XmRepTypeGetId(XmRUnitType),
				      (String)from_val->addr);

    if (result < 0)
    {
	XtStringConversionWarning(from_val->addr, XmRUnitType);
	to_val->addr = NULL;
	to_val->size = 0;
	return;
    }

    value = (unsigned char)result;

    if (to_val->addr == NULL)
    {
	to_val->addr = (XPointer)&value;
	to_val->size = sizeof value;
    }
    else
    {
	if (to_val->size >= sizeof value)
	{
	    *(unsigned char *)to_val->addr = value;
	    to_val->size = sizeof value;
	}
	else
	{
	    XtStringConversionWarning(from_val->addr, XmRUnitType);
	    to_val->addr = NULL;
	    to_val->size = 0;
	}
    }
}

void
XmRepTypeAddReverse(XmRepTypeId rep_type_id)
{
    XmRepTypeEntry entry;
    XtConvertArgRec Args[1];

    if ((unsigned short)rep_type_id >= number_of_types)
	return;
    entry = rep_types + rep_type_id;

    if (!entry->reverse_installed)
    {
	entry->reverse_installed = True;

	Args[0].address_mode = XtImmediate;
	Args[0].address_id = (XtPointer)(long)entry->rep_type_id;
	Args[0].size = sizeof(XmRepTypeId);

	XtSetTypeConverter(entry->rep_type_name, XmRString,
			   (XtTypeConverter)__XmCvtRepToString,
			   Args, 1, XtCacheNone, NULL);
    }
}

XmRepTypeId
XmRepTypeGetId(String rep_type)
{
    int i;

    for (i = 0; i < number_of_types; i++)
    {
	if (!strcmp(rep_type, rep_types[i].rep_type_name))
	{
	    return i;
	}
    }

    return XmREP_TYPE_INVALID;
}

String *
XmRepTypeGetNameList(XmRepTypeId rep_type_id,
		     Boolean use_uppercase_format)
{
    int i, len;
    char *cp, *p;
    String *strs;
    XmRepTypeEntry entry;

    if ((unsigned short)rep_type_id >= number_of_types)
	return NULL;
    entry = rep_types + rep_type_id;

    /* Find the total size of the list and strings, and allocate */

    len = 0;
    for (i = 0; i < entry->num_values; i++)
	len += strlen(entry->value_names[i]);
    strs = (String *)XtMalloc(entry->num_values * (use_uppercase_format ?
	sizeof(String) + 3 : sizeof(String) + 1) + sizeof(String) + len);

    /* Copy the strings, adding "Xm" and uppercasing if needed */

    cp = (char *)(strs + entry->num_values + 1);
    for (i = 0; i < entry->num_values; i++)
    {
	strs[i] = cp;
	p = entry->value_names[i];
	if (use_uppercase_format)
	{
	    *cp++ = 'X';
	    *cp++ = 'm';
	    while ((*cp++ = toupper(*p++)));
	}
	else
	    while ((*cp++ = *p++));
    }

    /* NULL-terminate the list */

    strs[i] = NULL;
    return strs;
}

XmRepTypeEntry
XmRepTypeGetRecord(XmRepTypeId rep_type_id)
{
    int i, len;
    XmRepTypeEntry entry, copy;

    if ((unsigned short)rep_type_id >= number_of_types)
	return NULL;
    entry = rep_types + rep_type_id;

    /* Find the total size of the record, lists, and strings, and allocate */

    len = 0;
    for (i = 0; i < entry->num_values; i++)
	len += strlen(entry->value_names[i]);
    copy = (XmRepTypeEntry)XtMalloc(sizeof(XmRepTypeEntryRec) +
	strlen(entry->rep_type_name) + 1 + entry->num_values *
	(entry->values ? sizeof(String) + 2 : sizeof(String) + 1) + len);

    __XmRepTypeCopyRecord(copy, entry, (String *)(copy + 1),
			  (char *)((String *)(copy + 1) + entry->num_values));
    return copy;
}

XmRepTypeList
XmRepTypeGetRegistered(void)
{
    int i, strlens, num_values;
    char *cp;
    String *sp;
    XmRepTypeList list, lp;

    /* 14 Oct 1998: There used to be a test to only call XmRegisterConverters
     * if number_of_types was 0.  But this meant if an application installed
     * some converters, then called XmRepTypeGetRegistered, the "base"
     * converters wouldn't be registered.  XmRegisterConverters can handle
     * redundant calls, so let it worry about it.		- Jamie
     */

    XmRegisterConverters();

    /* Find the sizes of various parts of the records */

    strlens = num_values = 0;
    for (lp = rep_types; lp < rep_types + number_of_types; lp++)
    {
	strlens += strlen(lp->rep_type_name);
	for (i = 0; i < lp->num_values; i++)
	    strlens += strlen(lp->value_names[i]);
	if (lp->values)
	    strlens += lp->num_values;
	num_values += lp->num_values;
    }

    /* Allocate the memory to hold everything */

    list = (XmRepTypeList)
	XtMalloc(number_of_types * (sizeof(XmRepTypeListRec) + 1) +
		 num_values * (sizeof(String) + 1) + strlens +
		 sizeof(XmRepTypeListRec));

    /* Copy each entry */

    lp = list;
    sp = (String *)(list + number_of_types + 1);
    cp = (char *)(sp + num_values);
    for (i = 0; i < number_of_types; i++)
    {
	cp += __XmRepTypeCopyRecord(lp, rep_types + i, sp, cp);
	sp += rep_types[i].num_values;
	lp++;
    }
    memset(lp, 0, sizeof(XmRepTypeListRec));
    return list;
}

void
XmRepTypeInstallTearOffModelConverter(void)
{
    static char *tear_off_models[] =
    {
	"tear_off_enabled",
	"tear_off_disabled"
    };

    XmRepTypeRegister(XmRTearOffModel,
		      tear_off_models,
		      NULL, 2);
}

XmRepTypeId
XmRepTypeRegister(String rep_type,
		  String *value_names,
		  unsigned char *values,
		  unsigned char num_values)
{
    int i, len;
    char *cp;
    XmRepTypeId id = XmRepTypeGetId(rep_type);
    XmRepTypeEntryRec ne;
    XtConvertArgRec Args[1];

    if (id != XmREP_TYPE_INVALID)
    {
	return id;
    }

    if (number_of_types + 1 > max_number_of_types)
    {
	max_number_of_types = (max_number_of_types + 1) * 2;

	rep_types = (XmRepTypeList)XtRealloc((char *)rep_types,
					     sizeof(XmRepTypeListRec) *
					     max_number_of_types);
    }

    /* Find the total size of the lists and strings, and allocate it */

    len = 0;
    for (i = 0; i < num_values; i++)
	len += strlen(value_names[i]);
    cp = XtMalloc(strlen(rep_type) + 1 + num_values *
		  (values ? sizeof(String) + 2 : sizeof(String) + 1) + len);

    /* Copy the data into the new record */

    ne.rep_type_name = rep_type;
    ne.value_names = value_names;
    ne.values = values;
    ne.num_values = num_values;
    ne.reverse_installed = False;
    ne.rep_type_id = number_of_types;
    __XmRepTypeCopyRecord(rep_types + number_of_types, &ne, (String *)cp,
			  (char *)((String *)cp + num_values));

    /* now install the forward converter */

    Args[0].address_mode = XtImmediate;
    Args[0].address_id = (XtPointer)(long)number_of_types;
    Args[0].size = sizeof(XmRepTypeId);

    XtSetTypeConverter(XmRString, rep_type,
		       __XmCvtStringToRep,
		       Args, 1, XtCacheNone, NULL);
    return number_of_types++;
}

Boolean
XmRepTypeValidValue(XmRepTypeId rep_type_id,
		    unsigned char test_value,
		    Widget enable_default_warning)
{
    int i;
    XmRepTypeEntry entry;

    if ((unsigned short)rep_type_id >= number_of_types)
    {
	if (enable_default_warning)
	    _XmWarning(enable_default_warning,
		       "XmRepTypeValidValue: missing type.\n");
	return False;
    }
    entry = rep_types + rep_type_id;

    if (entry->values)
    {
	for (i = 0; i < entry->num_values; i++)
	{
	    if (test_value == entry->values[i])
	    {
		return True;
	    }
	}
    }
    else
    {
	if (test_value < entry->num_values)
	{
	    return True;
	}
    }

    if (enable_default_warning)
	_XmWarning(enable_default_warning,
		   "illegal value (%d) for rep type %s",
		   test_value, entry->rep_type_name);

    return False;
}
