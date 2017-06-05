/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/XmTabList.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
 * 
 * Copyright © 2000, 2001, 2002 LessTif Development Team
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


#include <LTconfig.h>
#include <Xm/XmP.h>
#include <XmI/XmI.h>

#include <XmI/DebugUtil.h>


static XmTab _XmTabCopy(XmTab s)
{
	XmTab	r;

	r = (XmTab)XtMalloc(sizeof(struct _XmTabRec));
	r->value = s->value;
	r->units = s->units;
	r->offset_model = s->offset_model;
	r->alignment = s->alignment;
	r->decimal = XtNewString(s->decimal);

	return r;
}

extern XmTabList XmTabListCopy(XmTabList tablist, int offset, Cardinal count)
{
	if (!tablist)
		return NULL;
	if (tablist == (XmTabList)XmAS_IS)
		return tablist;

	if (count == 0)
		count = tablist->count;

	return XmTabListInsertTabs(NULL, tablist->tabs, count, 0);
}

extern void XmTabListFree(XmTabList tablist)
{
	int	i;

	if (!tablist)
		return;
	if (tablist->tabs) {
		for (i=0; i<tablist->count; i++)
			XmTabFree(tablist->tabs[i]);
		XtFree((char *)tablist->tabs);
	}
	XtFree((char *)tablist);
}

extern XmTab XmTabListGetTab(XmTabList tablist, Cardinal position)
{
	if (position < 0 || position > tablist->count)
		return NULL;
	return tablist->tabs[position];
}

extern XmTabList
XmTabListInsertTabs(XmTabList oldlist,
                    XmTab *tabs,
                    Cardinal tab_count,
                    int position)
{
	XmTabList	r;
	int		i, j, d;

	if (!tabs)  
		return oldlist;
	if (tab_count==0)
		return oldlist;

	r = (XmTabList)XtMalloc(sizeof(struct _XmTabRec));
	r->count = tab_count;
	if (oldlist)
		r->count += oldlist->count;

	r->tabs = (XmTab *)XtCalloc(r->count, sizeof(XmTab *));

	/* Prepare for all the copy steps */
	d = 0;

	/* Copy the part before the position */
	for (i=0; i<position; i++, d++) {
		r->tabs[d] = _XmTabCopy(oldlist->tabs[i]);
	}

	/* Copy in the new list */
	for (j=0; j<tab_count; d++, j++) {
		r->tabs[d] = _XmTabCopy(tabs[j]);
	}

	/* Copy the part after the position */
	for (; i<position; i++, d++) {
		r->tabs[d] = _XmTabCopy(oldlist->tabs[i]);
	}

	return r;
}


extern XmTabList
XmTabListRemoveTabs(XmTabList oldlist,
                    Cardinal *position_list,
                    Cardinal position_count)
{
	if (!position_list)
		return oldlist;
	if (position_count==0)
		return oldlist;

	_XmWarning(NULL, "XmTabListRemoveTabs(): not yet implemented!");
	return (XmTabList)NULL;
}

extern XmTabList
XmTabListReplacePositions(XmTabList oldlist,
                          Cardinal *position_list,
                          XmTab *tabs,
                          Cardinal tab_count)
{
	if (!tabs || !oldlist || !position_list)
		return NULL;
	if (tab_count==0)
		return NULL;

	_XmWarning(NULL, "XmTabListReplacePositions(): not yet implemented!");
	return (XmTabList)NULL;
}


extern Cardinal
XmTabListTabCount(XmTabList tablist)
{
	return tablist ? tablist->count : 0;
}


extern void
XmTabSetValue(XmTab tab,
              float value)
{
	if (tab)
		tab->value = value;
}


extern float
XmTabGetValues(XmTab tab,
               unsigned char *units,
               XmOffsetModel *offset,
               unsigned char *alignment,
               char **decimal)
{
	if (tab == NULL)
		return 0.;
	if (units)
		*units = tab->units;
	if (offset)
		*offset = tab->offset_model;
	if (alignment)
		*alignment = tab->alignment;
	if (decimal)
		*decimal = XtNewString(tab->decimal);	/* FIX ME ??? */
	return tab->value;
}


extern XmTabList
XmStringTableProposeTablist(XmStringTable strings,
                            Cardinal num_strings,
                            Widget widget,
                            float pad_value,
                            XmOffsetModel offset_model)
{
	_XmWarning(NULL, "XmStringTableProposeTablist(): not yet implemented!");
	return (XmTabList)NULL;
}           


extern XmTab
XmTabCreate(float value,
            unsigned char units,
            XmOffsetModel offset_model,
            unsigned char alignment,
            char *decimal)
{
	XmTab	r;

	r = (XmTab)XtMalloc(sizeof(struct _XmTabRec));
	r->value = value;
	r->units = units;
	r->offset_model = offset_model;
	r->alignment = alignment;
	r->decimal = XtNewString(decimal);

	return r;
}
		     

extern void
XmTabFree(XmTab tab)
{
	if (tab)
		XtFree(tab->decimal);
	XtFree((char *)tab);
}
