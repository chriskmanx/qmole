/**
 *
 * $Id: Cache.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Id: Cache.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/ExtObjectP.h>

#include <XmI/DebugUtil.h>


void
_XmCacheDelete(XtPointer data)
{
    XmGadgetCacheRefPtr node;

    node = (XmGadgetCacheRefPtr)DataToGadgetCache(data);
    node->cache.ref_count--;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "Deleting %p : %08x: refcount: %d\n",
		      data, node, node->cache.ref_count));

    if (node->cache.ref_count == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "Ref count 0: deleting %08x\n", node));

	node->cache.prev->next = node->cache.next;

	if (node->cache.next != NULL)	/* rws 16 Jun 1997
					   This is not the correct solution
					   since the node never gets freed, but
					   it does stop a core dump in one of
					   my apps
					 */
	{
	    node->cache.next->prev = node->cache.prev;
	    XtFree((char *)node);
	}
	else
	{
	    _XmWarning(NULL,
		       "Cache.c:_XmCacheDelete - node->cache.next is NULL");
	}
    }
}

void
_XmCacheCopy(XtPointer src, XtPointer dest, size_t size)
{
    memcpy(dest, src, size);
}

XtPointer
_XmCachePart(XmCacheClassPartPtr cp, XtPointer cpart, size_t size)
{
    XmGadgetCachePtr list;
    XtPointer newpart;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "Attempting to cache a part %p head: %p.\n", cpart,
		      &ClassCacheHead(cp)));

    /*
     * Guess what.  Motif doesn't save memory and self link the initial node.
     * Big surprise.
     */
    if (ClassCacheHead(cp).next == NULL)
    {
	ClassCacheHead(cp).prev = &ClassCacheHead(cp);
	ClassCacheHead(cp).next = &ClassCacheHead(cp);
	ClassCacheHead(cp).ref_count = -1;
    }

    /* search cache */
    list = ClassCacheHead(cp).next;

    while (list != &ClassCacheHead(cp))
    {
	XmGadgetCachePtr tmp;

	if (ClassCacheCompare(cp) (cpart, CacheDataPtr(list)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL,
			      "Cache hit: %08x\n", CacheDataPtr(list)));

	    if (cpart != CacheDataPtr(list))
	    {
		list->ref_count++;
	    }

	    return CacheDataPtr(list);
	}

	tmp = list->next;

	if (cpart == CacheDataPtr(list))
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL,
			      "In cache, but invalid.  Deleting old entry.\n"));

	    _XmCacheDelete(cpart);
	}

	list = tmp;
    }

    /* not in cache, add new entry */
    list = (XmGadgetCachePtr)XtCalloc(1, sizeof(XmGadgetCache) + size);

    DEBUGOUT(_LtDebug(__FILE__, NULL,
	     "Not in cache.  Adding new entry of size %d: %p.\n", size, list));

    newpart = (XtPointer)((char *)list + sizeof(XmGadgetCache));

    list->prev = ClassCacheHead(cp).prev;
    ClassCacheHead(cp).prev->next = list;

    list->next = &ClassCacheHead(cp);
    ClassCacheHead(cp).prev = list;

    list->ref_count = 1;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "Next: %p Prev: %p\n", list->prev, list->next));

    ClassCacheCopy(cp) (cpart, (XtPointer)newpart, size);

    return newpart;
}
