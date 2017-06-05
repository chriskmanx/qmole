/**
 *
 * $Id: Traversal.c,v 1.4 2009/04/29 09:45:14 paulgevers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2001, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Id: Traversal.c,v 1.4 2009/04/29 09:45:14 paulgevers Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/XmI.h>
#include <XmI/TraversalI.h>

#include <Xm/XmP.h>
#include <Xm/VendorS.h>
#include <Xm/VendorSEP.h>
#include <Xm/BaseClassP.h>
#include <Xm/GadgetP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/ManagerP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/DrawingAP.h>
#include <Xm/MenuShellP.h>

#include <XmI/DebugUtil.h>


#define ALLOC_INCR		8

/*
 * forwards
 */
Widget _XmTraverseAway(XmTravTree tree, Widget w, Boolean control);
XmNavigability _XmGetNavigability(Widget w);
XmNavigationType _XmGetNavigationType(Widget w);
Boolean _XmNewTravGraph(XmTravTree tree, Widget shell, Widget first);
Boolean _XmIsViewable(Widget w);
Widget _XmIsScrollableClipWidget(Widget w, XRectangle *rect);
Boolean _XmIntersectionOf(XRectangle *a, XRectangle *b, XRectangle *dest);
void _XmTravGraphRemove(XmTravTree tree, Widget w);
void _XmTravGraphAdd(XmTravTree tree, Widget w);
void _XmTravGraphUpdate(XmTravTree tree, Widget w);
Boolean _XmGetEffectiveView(Widget w, XRectangle *rect);
Boolean _XmSetInitialOfTabGraph(XmTravTree tree, Widget tab, Widget first);
void _XmTabListAdd(XmTravTree tree, Widget w);
void _XmTabListDelete(XmTravTree tree, Widget w);
Widget _XmGetClippingAncestor(Widget w, XRectangle *rect);
Widget _XmTraverse(XmTravTree tree, XmTraversalDirection dir, Widget w);


/************************** Tree Manip functions ***************************/

/*
 * MLM: be careful defining this.  This turns on _LtDebug *and* dumping of the
 * traversal graph.  If you do turn it on, NOTE that performance will really
 * suck.  You can get visual effects if you turn it on (slooooowwww exposes).
 */
#define DUMP_NODES 0

#if DUMP_NODES
extern void
DumpNode(int which, XmTravTreeNode node)
{
    if (node == NULL)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL,
			   " Attempt to dump NULL node!\n"));

	return;
    }
    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "  %d: addr: %p type: %d nav_type: %d tab_parent: %p\n",
		       which, node, node->type, node->nav_type,
		       node->tab_parent.link));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "  %d: widget: %s class: %s rect: %d %d %d %d\n",
		       which, XtName(node->widget),
		       XtClass(node->widget)->core_class.class_name,
		       node->rect.x, node->rect.y,
		       node->rect.width, node->rect.height));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "  %d: next: %p prev: %p up: %p down: %p\n",
		       which, node->next, node->prev, node->up, node->down));

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n"));
}


extern void
DumpTree(XmTravTree tree)
{
    int i;

    if (tree == NULL)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL,
			   "DumpTree: Tree is NULL\n"));
	return;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "Tree: Widget: %s current: %p num_entries: %d\n",
		       XtName(tree->shell), tree->current, tree->num_entries));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "      num_alloc: %d next_alloc: %d num_excls: %d\n",
		       tree->num_alloc, tree->next_alloc, tree->num_excls));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "      num_tab_alloc: %d num_tab_entries: %d\n",
		       tree->num_tab_alloc, tree->num_tab_entries));

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "Exclusive/tabs\n"));
    for (i = 0; i < tree->num_tab_entries; i++)
	DEBUGOUT(_LtDebug0(__FILE__, NULL,
			   "  %d: %s\n", i, XtName(tree->excl_tabs[i])));

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "Nodes:\n"));
    for (i = 0; i < tree->num_entries; i++)
	DumpNode(i, &tree->head[i]);
}


extern void
DumpFocusData(XmFocusData fd)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "FocusData: active_tab %p focus_item %p old_focus %p\n",
		       fd->active_tab_group, fd->focus_item,
		       fd->old_focus_item));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "           pointer_item: %p old_pointer: %p\n",
		       fd->pointer_item, fd->old_pointer_item));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "           flush: %d focal_point: %d first_focus: %p\n",
		       fd->flush, fd->focal_point, fd->first_focus));

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "           focus_policy: %d\n", fd->focus_policy));

    DumpTree(&fd->tree);
}

#else

/*
#undef DEBUGOUT
#define DEBUGOUT(x)
*/
#define DumpNode(w,n)
#define DumpTree(t)
#define DumpFocusData(f)

#endif

static XmTravTreeNode
GetNodeOfWidget(XmTravTree tree, Widget w)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "GetNodeOfWidget\n"));

    if (!w)
    {
	return NULL;
    }

    for (i = 0; i < tree->num_entries; i++)
    {
	if (tree->head[i].widget == w)
	{
	    return &tree->head[i];
	}
    }
    return NULL;
}

static XmTravTreeNode
GetNodeFromGraph(XmTravTreeNode node, Widget first)
{
    XmTravTreeNode tmp;

    DEBUGOUT(_LtDebug(__FILE__, first, "GetNodeFromGraph\n"));

    if (!first)
    {
	return NULL;
    }

    if (node->up == NULL)
    {
	return NULL;
    }

    tmp = node->up;
    while (1)
    {
	if (tmp->widget == first)
	{
	    return tmp;
	}

	if (tmp == node->down)
	{
	    return NULL;
	}

	tmp = tmp->next;

	if (tmp == NULL)
	{
	    return NULL;
	}
    }
}


static XmTravTreeNode
AllocListEntry(XmTravTree tree)
{
    XmTravTreeNode node;

    if (tree->num_alloc == 0)
    {
	if (tree->next_alloc != 0)
	{
	    tree->num_alloc = tree->next_alloc;
	}
	else
	{
	    tree->num_alloc = ALLOC_INCR * 2;
	}
	tree->head = (XmTravTreeNode)XtCalloc(tree->num_alloc,
					      sizeof(XmTravTreeNodeRec));
    }
    else if (tree->num_entries == tree->num_alloc)
    {
	tree->num_alloc += ALLOC_INCR * 2;
	tree->head = (XmTravTreeNode)XtRealloc((char *)tree->head,
					       sizeof(XmTravTreeNodeRec) *
					       tree->num_alloc);
    }

    node = &tree->head[tree->num_entries];
    tree->num_entries++;
    return node;
}


static Boolean
GetChildList(Widget w, WidgetList *children, Cardinal *nchildren)
{
    XmManagerClassExt *mce;

    if (XmIsManager(w))
    {
	mce = _XmGetManagerClassExtPtr(XtClass(w), NULLQUARK);

	if (mce && *mce && (*mce)->traversal_children)
	{
	    return ((*mce)->traversal_children) (w, children, nchildren);
	}
    }
    return False;
}


static void
GetNodeList(Widget w, XRectangle *rect, XmTravTree tree,
	    int toffset, int coffset)
{
    XmNavigability nav;
    XmNavigationType nt;
    XmTravTreeNode tmp;
    int num_nodes;
    XRectangle trect;
    WidgetList children;
    Cardinal nchildren, i;
    Boolean needfree;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "GetNodeList: toffset: %d coffset: %d\n",
		      toffset, coffset));

    if (CoreBeingDestroyed(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: Widget is being destroyed; return\n"));

	return;
    }

    nav = _XmGetNavigability(w);

    if (nav == XmNOT_NAVIGABLE && !XtIsShell(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: Widget is not navigable; return\n"));

	return;
    }

    num_nodes = tree->num_entries;

    tmp = AllocListEntry(tree);

    tmp->widget = w;
    tmp->rect.x = rect->x + XtX(w) + XtBorderWidth(w);
    tmp->rect.y = rect->y + XtY(w) + XtBorderWidth(w);
    tmp->rect.width = XtWidth(w);
    tmp->rect.height = XtHeight(w);

    if (num_nodes)
    {
	nt = _XmGetNavigationType(w);
    }
    else
    {
	nt = XmSTICKY_TAB_GROUP;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "GetNodeList: Got NavigationType %d Navigability: %d\n",
		      nt, nav));

    tmp->nav_type = nt;
    if (nav == XmCONTROL_NAVIGABLE)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: Widget is CONTROL NODE; coffset %d\n",
			  coffset));

	tmp->type = XmCONTROL_NODE;
	tmp->tab_parent.offset = coffset;
	return;
    }
    if (nav == XmTAB_NAVIGABLE)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: Widget is TAB NODE; toffset %d\n",
			  toffset));

	tmp->type = XmTAB_NODE;
	tmp->tab_parent.offset = toffset;
	return;
    }
    if ((nav != XmNOT_NAVIGABLE || num_nodes == 0) && !XtIsComposite(w))
    {
	tree->num_entries--;
	return;
    }

    trect = tmp->rect;
    rect = &trect;
    if (nav == XmDESCENDANTS_NAVIGABLE)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: DESCENDENTS NAVIGABLE; return\n"));

	num_nodes = toffset;
	tree->num_entries--;
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: create tab and control trees\n"));

	tmp->type = XmTAB_TREE_NODE;
	tmp->tab_parent.offset = toffset;
	tmp->up = NULL;
	tmp->down = NULL;

	toffset = num_nodes;

	tmp = AllocListEntry(tree);
	*tmp = tree->head[num_nodes];
	num_nodes++;

	tmp->tab_parent.offset = toffset;
	tmp->type = XmCONTROL_TREE_NODE;

	coffset = num_nodes;
    }

    if (!(needfree = GetChildList(w, &children, &nchildren)))
    {
	children = MGR_Children(w);
	nchildren = MGR_NumChildren(w);
    }

    for (i = 0; i < nchildren; i++)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "GetNodeList: recurse: toffset %d coffset %d\n",
			  toffset, coffset));

	GetNodeList(children[i], rect, tree, toffset, coffset);
    }

    if (needfree)
    {
	XtFree((char *)children);
    }
}


static void
GetRectRelativeToShell(Widget w, XRectangle *rect)
{
    Position cx, cy;

    DEBUGOUT(_LtDebug(__FILE__, w, "GetRectRelativeToShell\n"));

    rect->width = XtWidth(w);
    rect->height = XtHeight(w);

    do
    {
	cx = XtX(w) + XtBorderWidth(w);
	cy = XtY(w) + XtBorderWidth(w);

	w = XtParent(w);
    }
    while (w && !XtIsShell(w));

    rect->x = cx;
    rect->y = cy;
}


static int
CompareExcls(XmConst void *a, XmConst void *b)
{
    return 0;
}


static int
CompareNodesVert(XmConst void *a, XmConst void *b)
{
    XmTravTreeNode nodea = *(XmTravTreeNode *)a, nodeb = *(XmTravTreeNode *)b;
    Position centera = nodea->rect.x + nodea->rect.width / 2;
    Position centerb = nodeb->rect.x + nodeb->rect.width / 2;

    if (centera > nodeb->rect.x + nodeb->rect.width &&
	centerb < nodea->rect.x)
    {
	return 1;
    }
    else if (centerb > nodea->rect.x + nodea->rect.width &&
	centera < nodeb->rect.x)
    {
	return -1;
    }
    else if (nodea->rect.y < nodeb->rect.y)
    {
	return -1;
    }
    else if (nodea->rect.y > nodeb->rect.y)
    {
	return 1;
    }
    else
    {
	return 0;
    }
}


static int
CompareNodesHoriz(XmConst void *a, XmConst void *b)
{
    XmTravTreeNode nodea = *(XmTravTreeNode *)a, nodeb = *(XmTravTreeNode *)b;
    Position centera = nodea->rect.y + nodea->rect.height / 2;
    Position centerb = nodeb->rect.y + nodeb->rect.height / 2;

    if (centera > nodeb->rect.y + nodeb->rect.height &&
	centerb < nodea->rect.y)
    {
	return 1;
    }
    else if (centerb > nodea->rect.y + nodea->rect.height &&
	centera < nodeb->rect.y)
    {
	return -1;
    }
    else if (nodea->rect.x < nodeb->rect.x)
    {
	return -1;
    }
    else if (nodea->rect.x > nodeb->rect.x)
    {
	return 1;
    }
    else
    {
	return 0;
    }
}


static void
SortGraph(XmTravTreeNode node, Boolean have_excls)
{
    XmTravTreeNode *block, tmp, *ptr;
    int cnt, i;

    if (node->up == NULL)
    {
	return;
    }

    cnt = 1;
    tmp = node->up;
    while (tmp->next != NULL)
    {
	cnt++;
	tmp = tmp->next;
    }

    block = (XmTravTreeNode *)XtMalloc(cnt * sizeof(XmTravTreeNode));

    i = 0;
    tmp = node->up;
    while (tmp != NULL)
    {
	block[i] = tmp;
	i++;
	tmp = tmp->next;
    }

    if (node->type == XmTAB_TREE_NODE && cnt > 1)
    {
	if (have_excls)
	{
	    qsort(block, cnt, sizeof(XmTravTreeNode), CompareExcls);
	}
	else
	{
	    qsort(&block[1], cnt - 1, sizeof(XmTravTreeNode), CompareNodesHoriz);
	}
    }
    else if (cnt > 1 && (have_excls || node->nav_type == XmSTICKY_TAB_GROUP))
    {
	qsort(block, cnt, sizeof(XmTravTreeNode), CompareNodesHoriz);
    }

    node->up = block[0];
    ptr = block;
    block[0]->prev = NULL;

    for (i = 1; i < cnt; i++)
    {
	(*ptr)->next = *(ptr + 1);
	ptr++;
	(*ptr)->prev = *(ptr - 1);
    }
    (*ptr)->next = NULL;
    node->down = *ptr;

    if (node->type == XmCONTROL_TREE_NODE)
    {

	node->up->prev = *ptr;
	node->down->next = node->up;

	if (!have_excls || node->type == XmSTICKY_TAB_GROUP)
	{
	    qsort(block, cnt, sizeof(XmTravTreeNode), CompareNodesVert);
	}

	ptr = block;
	block[0]->up = block[cnt - 1];
	for (i = 1; i < cnt; i++)
	{
	    (*ptr)->down = *(ptr + 1);
	    ptr++;
	    (*ptr)->up = *(ptr - 1);
	}
	(*ptr)->down = block[0];
    }

    XtFree((char *)block);
}


static XmTravTreeNode
GetNextNearestNode(XmTravTreeNode node, XRectangle *rect)
{
    XmTravTreeNode tmp, *block;
    int cnt, i;
    XmTravTreeNodeRec input;

    DEBUGOUT(_LtDebug(__FILE__, node->widget, "GetNextNearestNode\n"));

    if (!node->up)
    {
	return NULL;
    }

    cnt = 1;
    tmp = node->up;
    do
    {
	cnt++;
	if (tmp == node->down)
	{
	    break;
	}
	tmp = tmp->next;
    }
    while (tmp != NULL);

    block = (XmTravTreeNode *)XtMalloc(cnt * sizeof(XmTravTreeNode));

    input.widget = NULL;
    input.rect = *rect;
    block[0] = &input;

    tmp = node->up;
    for (i = 1; i < cnt; i++)
    {
	block[i] = tmp;
	tmp = tmp->next;
    }

    qsort(block, cnt, sizeof(XmTravTreeNode), CompareNodesHoriz);

    i = 0, tmp = NULL;
    while (i != cnt)
    {
	if (block[i] != &input)
	{
	    i++;
	    continue;
	}
	else if (i + 1 == cnt)
	{
	    tmp = NULL;
	    break;
	}
	else
	{
	    tmp = block[i + 1];
	    break;
	}
    }

    XtFree((char *)block);

    return tmp;
}


static void
LinkNodeList(XmTravTree tree)
{
    unsigned short i;
    XmTravTreeNode tmp, link;

    DEBUGOUT(_LtDebug(__FILE__, tree->shell, "LinkNodeList\n"));
    DEBUGOUT(DumpTree(tree));

    tmp = tree->head;

    for (i = 0; i < tree->num_entries; i++)
    {
	if (tmp->tab_parent.offset >= 0)
	{
	    link = &tree->head[tmp->tab_parent.offset];
	}
	else
	{
	    link = NULL;
	}

	tmp->tab_parent.link = link;
	if (link)
	{
	    if (link->down)
	    {
		link->down->next = tmp;
	    }
	    else
	    {
		link->up = tmp;
	    }
	    tmp->next = NULL;
	    tmp->prev = link->down;
	    link->down = tmp;
	}
	else
	{
	    tmp->next = NULL;
	    tmp->prev = NULL;
	}

	tmp++;
    }

    DEBUGOUT(DumpTree(tree));
}


static void
SortNodeList(XmTravTree tree)
{
    int i;
    XmTravTreeNode tmp;

    DEBUGOUT(_LtDebug(__FILE__, tree->shell, "SortNodeList\n"));

    tmp = tree->head;

    for (i = 0; i < tree->num_entries; i++)
    {
	if (tmp->type == XmTAB_TREE_NODE || tmp->type == XmCONTROL_TREE_NODE)
	{
	    if (tree->num_excls != 0)
	    {
		SortGraph(tmp, True);
	    }
	    else
	    {
		SortGraph(tmp, False);
	    }
	}

	tmp++;
    }

    DEBUGOUT(DumpTree(tree));
}


static Boolean
NodeIsTraversable(XmTravTreeNode node)
{
    if (node == NULL)
	return False;

    DEBUGOUT(_LtDebug(__FILE__, node->widget, "NodeIsTraversable\n"));

    if (node->widget == NULL)
    {
	return False;
    }

    if (node->type == XmTAB_TREE_NODE || node->type == XmCONTROL_TREE_NODE)
    {
	return False;
    }

    return XmIsTraversable(node->widget);
}


static XmTravTreeNode
NextControl(XmTravTreeNode node)
{
    XmTravTreeNode last, next, ptr, start;

    start = node;
    last = node;
    ptr = node;
    next = NULL;

    do
    {

	if (ptr > start && ptr < next && next == NULL)
	{
	    next = ptr;
	}

	if (ptr <= last)
	{
	    last = ptr;
	}

	ptr = ptr->next;

    }
    while (ptr != node);

    if (next == NULL)
    {
	next = last;
    }

    return next;
}


static XmTravTreeNode
PrevControl(XmTravTreeNode node)
{
    XmTravTreeNode last, prev, ptr, start;

    start = node;
    last = node;
    ptr = node;
    prev = NULL;

    do
    {

	if (ptr < start && ptr > prev && prev == NULL)
	{
	    prev = ptr;
	}

	if (ptr > last)
	{
	    last = ptr;
	}

	ptr = ptr->prev;

    }
    while (ptr != node);

    if (prev == NULL)
    {
	prev = last;
    }

    return prev;
}


static XmTravTreeNode
TraverseControl(XmTravTreeNode node, XmTraversalDirection dir)
{
    XmTravTreeNode tmp, last;

    if (!node)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "TraverseControl> given is NULL\n"));

	return NULL;
    }

    DEBUGOUT(_LtDebug(__FILE__, node->widget, "TraverseControl\n"));

    if (node->type == XmCONTROL_TREE_NODE)
    {
	DEBUGOUT(_LtDebug(__FILE__, node->widget,
			  "TraverseControl: CONTROL_TREE_NODE\n"));

	node = node->up;
	if (node == NULL)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL,
			      "TraverseControl: node->up is NULL\n"));
	    return NULL;
	}

	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "TraverseControl: go HOME in CONTROL_TREE\n"));
	dir = XmTRAVERSE_HOME;
    }
    else if (node->type != XmCONTROL_NODE)
    {
	DEBUGOUT(_LtDebug(__FILE__, node->widget,
			  "TraverseControl: not CONTROL node\n"));
	return NULL;
    }

    tmp = node;
    last = NULL;
    do
    {
	switch (dir)
	{
	case XmTRAVERSE_CURRENT:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_CURRENT\n"));
	    break;

	case XmTRAVERSE_NEXT:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_NEXT\n"));
	    tmp = NextControl(tmp);
	    break;

	case XmTRAVERSE_PREV:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_PREV\n"));
	    tmp = PrevControl(tmp);
	    break;

	case XmTRAVERSE_HOME:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_HOME\n"));
	    DEBUGOUT(DumpNode(0, tmp));
	    tmp = tmp->tab_parent.link;

	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: parent_link:\n"));
	    DEBUGOUT(DumpNode(0, tmp));
	    tmp = tmp->up;

	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: parent up:\n"));
	    DEBUGOUT(DumpNode(0, tmp));
	    node = tmp->tab_parent.link;

	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseControl: parent up parent_link:\n"));
	    DEBUGOUT(DumpNode(0, node));
	    node = node->down;

	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseControl: parent up parent_link down:\n"));
	    DEBUGOUT(DumpNode(0, node));
	    dir = XmTRAVERSE_RIGHT;

	    break;

	case XmTRAVERSE_UP:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_UP\n"));
	    tmp = tmp->up;
	    break;

	case XmTRAVERSE_DOWN:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_DOWN\n"));
	    tmp = tmp->down;
	    break;

	case XmTRAVERSE_LEFT:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_LEFT\n"));
	    tmp = tmp->prev;
	    break;

	case XmTRAVERSE_RIGHT:
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: TRAVERSE_RIGHT\n"));
	    DEBUGOUT(DumpNode(0, tmp));
	    tmp = tmp->next;
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: after TRAVERSE_RIGHT\n"));
	    DEBUGOUT(DumpNode(0, tmp));
	    break;

	case XmTRAVERSE_NEXT_TAB_GROUP:
	case XmTRAVERSE_PREV_TAB_GROUP:
	default:
	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseControl: TRAVERSE_NEXT(or PREV)_TAB\n"));
	    tmp = last;
	}

	if (tmp == NULL)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL,
			      "TraverseControl: traverse to NULL\n"));
	    return NULL;
	}

	if (NodeIsTraversable(tmp))
	{
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: node is traversable;return\n"));

	    DEBUGOUT(DumpNode(0, tmp));

	    return tmp;
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseControl: node is not traversable\n"));

	    DEBUGOUT(DumpNode(0, tmp));
	}
    }
    while (tmp != node);

    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
		      "TraverseControl: No luck; returning\n"));
    DEBUGOUT(DumpNode(0, tmp));

    return tmp;
}


static XmTravTreeNode
TraverseTab(XmTravTreeNode node, XmTraversalDirection dir)
{
    XmTravTreeNode tmp, last;
    Boolean dir_is_cur, dir_is_not_cur;

    if (!node)
    {
	DEBUGOUT(_LtDebug(__FILE__, node->widget,
		 "TraverseTab called with null\n"));
	return NULL;
    }

    DEBUGOUT(_LtDebug(__FILE__, node->widget, "TraverseTab\n"));

    if (node->type == XmCONTROL_NODE)
    {
	if (! node->tab_parent.link) {
	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseTab -> NULL\n"));

	    return NULL;
	}

	node = node->tab_parent.link;
    }

    tmp = node;

    dir_is_cur = (dir == XmTRAVERSE_CURRENT);
    dir_is_not_cur = !dir_is_cur;

    DEBUGOUT(_LtDebug(__FILE__, node->widget,
		      "TraverseTab -> entering loop\n"));
    do
    {
	if (dir != XmTRAVERSE_PREV_TAB_GROUP)
	{
	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseTab -> TRAVERSE_NEXT_TAB_GROUP\n"));

	    if (tmp->type != XmTAB_TREE_NODE || tmp->up == NULL)
	    {

		DEBUGOUT(_LtDebug(__FILE__, node->widget,
				  "TraverseTab -> Not TAB_TREE or up NULL\n"));

		if (tmp->next == NULL)
		{

		    DEBUGOUT(_LtDebug(__FILE__, node->widget,
				      "TraverseTab -> next node NULL\n"));
		    last = tmp;

		    do
		    {
			tmp = tmp->tab_parent.link;

			if (tmp == NULL)
			{
			    break;
			}

			if (dir_is_not_cur)
			{
			    last = tmp;
			    continue;
			}

			if (tmp == node)
			{
			    break;
			}

			last = tmp;

		    }
		    while (tmp->next == NULL);

		    if (dir_is_cur && tmp == node)
		    {
			DEBUGOUT(_LtDebug(__FILE__, node->widget,
					  "TraverseTab -> CURRENT and home\n"));
			return NULL;
		    }

		    if (tmp == NULL)
		    {
			tmp = last;
		    }
		    else
		    {
			tmp = tmp->next;
		    }
		}
		else
		{
		    tmp = tmp->next;
		}
	    }
	    else
	    {
		tmp = tmp->up;
	    }

	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseTab -> TRAVERSE_NEXT_TAB_GROUP\n"));
	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseTab -> Compute to\n"));
	    DEBUGOUT(DumpNode(0, tmp));
	}
	else
	{

	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseTab -> TRAVERSE_PREV_TAB_GROUP\n"));

	    if (tmp->type != XmTAB_TREE_NODE || tmp->down == NULL)
	    {

		DEBUGOUT(_LtDebug(__FILE__, node->widget,
				  "TraverseTab -> Not TAB_TREE or down NULL\n"));

		if (tmp->prev == NULL)
		{

		    last = tmp;

		    do
		    {
			tmp = tmp->tab_parent.link;

			if (tmp == NULL)
			{
			    break;
			}

			last = tmp;

		    }
		    while (tmp->prev == NULL);

		    if (tmp == NULL)
		    {
			tmp = last;
		    }
		    else
		    {
			tmp = tmp->prev;
		    }
		}
		else
		{
		    tmp = tmp->prev;
		}
	    }
	    else
	    {
		tmp = tmp->down;
	    }
	}

	if (tmp == node)
	{
	    DEBUGOUT(_LtDebug(__FILE__, node->widget,
			      "TraverseTab -> home\n"));

	    return NULL;
	}

	if (tmp->type == XmCONTROL_TREE_NODE)
	{
	    DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
			      "TraverseTab -> goto ControlNode\n"));

	    DEBUGOUT(DumpNode(0, tmp));

	    if ((last = TraverseControl(tmp, dir)) != NULL)
	    {

		DEBUGOUT(_LtDebug(__FILE__, tmp->widget,
				  "TraverseTab -> return Control\n"));

		DEBUGOUT(DumpNode(0, last));

		return last;
	    }
	}

    }
    while (!NodeIsTraversable(tmp));

    return tmp;
}


static Boolean
EffectiveView(Widget w, XRectangle *src, XRectangle *rect)
{
    Boolean no_scr_par = True;
    XRectangle irect, orect;

    DEBUGOUT(_LtDebug(__FILE__, w, "EffectiveView\n"));

    if (!_XmIsViewable(w))
    {
	_XmClearRect(rect);
	return False;
    }

    _XmSetRect(rect, w);

    while ((w = XtParent(w)) && !XtIsShell(w))
    {

	if (!_XmIsViewable(w))
	{
	    _XmClearRect(rect);
	    return False;
	}

	if (_XmIsScrollableClipWidget(w, rect))
	{
	    no_scr_par = False;
	    continue;
	}

	if (no_scr_par)
	{
	    if (_XmIntersectRect(rect, w, rect))
	    {
		continue;
	    }
	    else
	    {
		return False;
	    }
	}

	if (!_XmIntersectRect(rect, w, &irect))
	{
	    _XmClearRect(rect);
	    return False;
	}

	if (rect->width != irect.width || rect->height != irect.height)
	{
	    _XmClearRect(rect);
	    return False;
	}
    }

    if (!src)
    {
	return True;
    }

    if (no_scr_par)
    {
	return _XmIntersectionOf(rect, src, rect);
    }

    if (!_XmIntersectionOf(rect, src, &orect))
    {
	_XmClearRect(rect);
	return False;
    }

    if (rect->width != irect.width || rect->height != irect.height)
    {
	_XmClearRect(rect);
	return False;
    }

    return True;
}


static Widget
FindFirstManaged(Widget w)
{
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w, "FindFirstManaged\n"));

    if (!XtIsShell(w))
    {
	return NULL;
    }

    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	if (XtIsManaged(MGR_Children(w)[i]))
	    return MGR_Children(w)[i];
    }
    return NULL;
}


static Boolean
SetInitialNode(XmTravTreeNode root, XmTravTreeNode leaf)
{
    DEBUGOUT(_LtDebug(__FILE__, root->widget, "SetInitialNode\n"));

    if (!leaf)
    {
	return False;
    }

    if (root->up == leaf)
    {
	return True;
    }

    DEBUGOUT(_LtDebug2(__FILE__, root->widget, leaf->widget,
		       "SetInitialNode leaf\n"));

    if (root->type == XmTAB_TREE_NODE)
    {
	root->down->next = root->up;
	root->up->prev = root->down;
	root->up = leaf;
	root->down = leaf->prev;
	leaf->prev->next = NULL;
	leaf->prev = NULL;
    }
    else
    {
	root->up = leaf;
	root->down = leaf->prev;
    }

    return True;
}


static int
SearchTabList(XmTravTree tree, Widget w)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "SearchTabList\n"));

    for (i = 0; i < tree->num_tab_entries; i++)
    {
	if (tree->excl_tabs[i] == w)
	{
	    return i;
	}
    }
    return -1;
}


static void
DeleteFromTabList(XmTravTree tree, int node)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "DeleteFromTabList\n"));

    if (node < 0 || tree->num_tab_entries == 0)
    {
	return;
    }

    memcpy(&tree->excl_tabs[node], &tree->excl_tabs[node + 1],
	  (tree->num_tab_entries - node - 1) * sizeof(Widget));

    tree->num_tab_entries--;
    tree->excl_tabs[tree->num_tab_entries] = NULL;
}


static void
SetInitialWidgets(XmTravTree tree)
{
    XmTravTreeNode tmp, child;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, tree->shell, "SetInitialWidgets\n"));

    tmp = tree->head;
    for (i = 0; i < tree->num_entries; i++)
    {
	if (tmp->type != XmTAB_TREE_NODE && tmp->type != XmCONTROL_TREE_NODE)
	{
	    tmp++;
	    continue;
	}

	if (tmp->up == NULL)
	{
	    tmp++;
	    continue;
	}

	if (tmp->widget != NULL && XmIsManager(tmp->widget) &&
	    MGR_InitialFocus(tmp->widget) &&
	    (child = GetNodeFromGraph(tmp, MGR_InitialFocus(tmp->widget))))
	{
	    SetInitialNode(tmp, child);
	}
	else if (tmp->type == XmTAB_TREE_NODE)
	{
	    SetInitialNode(tmp, tmp + 1);
	}

	tmp++;
    }

    DEBUGOUT(DumpTree(tree));
}


static Boolean
InitializeCurrent(XmTravTree tree, Widget w, Boolean check)
{
    XmTravTreeNode node;
    XmNavigability nav;

    DEBUGOUT(_LtDebug(__FILE__, w, "InitializeCurrent\n"));

    DEBUGOUT(DumpTree(tree));

    if (tree->current != NULL)
    {
	if (w == NULL)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL,
			      "InitializeCurrent: Widget is null\n"));

	    return True;
	}
	if (tree->current->widget == w)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "InitializeCurrent: Widget matches\n"));

	    return True;
	}
    }

    node = GetNodeOfWidget(tree, w);
    if (node == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "InitializeCurrent: Node is NULL\n"));

	if (check && (nav = _XmGetNavigability(w)) != XmNOT_NAVIGABLE)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "InitializeCurrent: Node is NULL: return new "
                              "trav graph\n"));

	    return _XmNewTravGraph(tree, tree->shell, w);
	}
	do
	{
	    if (XtIsShell(w))
	    {
		break;
	    }

	    if ((node = GetNodeOfWidget(tree, w)) != NULL)
	    {
		break;
	    }

	    w = XtParent(w);
	}
	while (w != NULL);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "InitializeCurrent: Climbed to node %08x\n",
			  node));
    }

    if (node != NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "InitializeCurrent: set current and return node\n"));
	DEBUGOUT(DumpNode(0, node));

	tree->current = node;
	return True;
    }
    else if (tree->current != NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "InitializeCurrent: current has value and return\n"));

	return True;
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "InitializeCurrent: set current to head; return\n"));

	DEBUGOUT(DumpNode(0, tree->head));

	tree->current = tree->head;
    }
    return True;
}


static Widget
FindFirstFocus(Widget w)
{
    Widget shell = _XmFindTopMostShell(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "FindFirstFocus\n"));

    return _XmNavigate(shell, XmTRAVERSE_CURRENT);
}

/************************ implementation functions *************************/

extern Widget
_XmFindTopMostShell(Widget w)
{
    Widget shell;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFindTopMostShell\n"));

    shell = w;

    while (shell && !XtIsShell(shell))
    {
	shell = XtParent(shell);
    }

    return shell;
}


extern void
_XmFreeTravGraph(XmTravTree tree)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmFreeTravGraph\n"));

    if (tree->num_alloc == 0)
    {
	return;
    }

    XtFree((char *)tree->head);

    tree->num_entries = 0;
    tree->head = NULL;
    tree->current = NULL;
    tree->next_alloc = tree->num_alloc;
    tree->num_alloc = 0;
    tree->shell = NULL;
}


extern Boolean
_XmNewTravGraph(XmTravTree tree, Widget shell, Widget first)
{
    XRectangle rect;

    DEBUGOUT(_LtDebug(__FILE__, shell, "_XmNewTravGraph\n"));

    if (shell == NULL)
    {
	if (tree->shell == NULL)
	{
	    shell = first;
	    if (shell != NULL)
	    {
		while (!XtIsShell(shell))
		{
		    shell = XtParent(shell);
		}
		tree->shell = shell;
	    }
	}
    }

    if (tree->shell == NULL || CoreBeingDestroyed(tree->shell))
    {
	_XmFreeTravGraph(tree);
	return False;
    }

    tree->num_entries = 0;
    rect.x = -(XtBorderWidth(shell) + XtX(shell));
    rect.y = -(XtBorderWidth(shell) + XtY(shell));
    rect.width = XtWidth(shell);
    rect.height = XtHeight(shell);

    GetNodeList(shell, &rect, tree, -1, -1);

    if (tree->num_entries > tree->num_alloc)
    {
	tree->num_alloc *= 2;
	tree->head = (XmTravTreeNode)XtRealloc((char *)tree->head,
					       sizeof(XmTravTreeNodeRec) *
					       tree->num_alloc);
    }

    LinkNodeList(tree);

    SortNodeList(tree);

    SetInitialWidgets(tree);

    InitializeCurrent(tree, first, False);

    return True;
}


extern void
_XmResetTravGraph(Widget wid)
{
    XmFocusData fd = _XmGetFocusData(wid);

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmResetTravGraph\n"));

    if (!fd)
    {
	return;
    }

    if (fd->tree.num_entries == 0)
    {
	return;
    }

    _XmFreeTravGraph(&fd->tree);
}


extern void
_XmTravGraphRemove(XmTravTree tree, Widget w)
{
    XmTravTreeNode nd;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTravGraphRemove\n"));

    if (tree->num_entries == 0)
    {
	return;
    }

    while ((nd = GetNodeOfWidget(tree, w)) != NULL)
    {
	nd->widget = NULL;
    }
}


extern void
_XmTravGraphAdd(XmTravTree tree, Widget w)
{
    XmTravTreeNode nd;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTravGraphAdd\n"));

    if (tree->num_entries == 0)
    {
	return;
    }

    if ((nd = GetNodeOfWidget(tree, w)) != NULL)
    {
	return;
    }

    _XmFreeTravGraph(tree);
}


extern void
_XmTravGraphUpdate(XmTravTree tree, Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTravGraphUpdate\n"));

    _XmFreeTravGraph(tree);
}


extern unsigned char
_XmGetFocusPolicy(Widget w)
{
    Widget shell = _XmFindTopMostShell(w);
    unsigned char policy = XmEXPLICIT;
    Widget ext = NULL;

    if (shell)
    {
	ext = _LtFindVendorExt(shell);
    }

    if (ext)
    {
	policy = VSEP_FocusPolicy(ext);
    }

    if (shell && ext)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetFocusPolicy\n"));
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetFocusPolicy shell %p ext %p\n",
			  shell, ext));
    }

    return policy;
}


extern XmFocusData
_XmCreateFocusData(void)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCreateFocusData\n"));

    return (XmFocusData)XtCalloc(1, sizeof(XmFocusDataRec));
}


extern void
_XmDestroyFocusData(XmFocusData focusData)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmDestroyFocusData\n"));

    _XmFreeTravGraph(&focusData->tree);
    XtFree((char *)focusData->tree.excl_tabs);
    XtFree((char *)focusData);
}


extern XmFocusData
_XmGetFocusData(Widget wid)
{
    Widget orig = wid;
    Widget ve;
    XmFocusData fd;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGetFocusData\n"));

    if (!wid)
    {
	return NULL;
    }

    while (!XtIsShell(wid))
    {
	wid = XtParent(wid);
    }

    DEBUGOUT(_LtDebug2(__FILE__, orig, wid, "_XmGetFocusData: Found shell\n"));

    if (!wid)
    {
	return NULL;
    }

    if (CoreBeingDestroyed(wid))
    {
	return NULL;
    }

    if (XtIsSubclass(wid, vendorShellWidgetClass))
    {
	ve = _LtFindVendorExt(wid);

	if (!ve)
	{
	    DEBUGOUT(_LtDebug(__FILE__, orig,
			      "_XmGetFocusData: no vendor ext\n"));

	    return NULL;
	}

	fd = VSEP_FocusData(ve);

	if (!fd)
	{
	    DEBUGOUT(_LtDebug(__FILE__, orig,
			      "_XmGetFocusData: no focus data\n"));

	    return NULL;
	}

	fd->focus_policy = VSEP_FocusPolicy(ve);

	return fd;
    }

    DEBUGOUT(_LtDebug(__FILE__, orig,
		      "_XmGetFocusData: not LessTif vendorshell subclass\n"));

    return NULL;
}


extern XmNavigability
_XmGetNavigability(Widget w)
{
    XmBaseClassExt *bce;
    XmNavigability r;

    if (!XtIsRectObj(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
		    "_XmGetNavigability : !XtIsRectObj => XmNOT_NAVIGABLE\n"));

	return XmNOT_NAVIGABLE;
    }

    if (CoreBeingDestroyed(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
	      "_XmGetNavigability : CoreBeingDestroyed => XmNOT_NAVIGABLE\n"));

	return XmNOT_NAVIGABLE;
    }

    bce = _XmGetBaseClassExtPtr(XtClass(w), XmQmotif);

    if (!bce || !*bce)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGetNavigability : no bce => XmNOT_NAVIGABLE\n"));

	return XmNOT_NAVIGABLE;
    }

    if (!(*bce)->widgetNavigable)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
	"_XmGetNavigability : NULL bce->widgetNavigable => XmNOT_NAVIGABLE\n"));

	return XmNOT_NAVIGABLE;
    }

    r = ((*bce)->widgetNavigable) (w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmGetNavigability : bce->widgetNavigable => %s\n",
		      _LtDebugNavigability2String(r)));

    return r;
}


extern XmNavigationType
_XmGetNavigationType(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetNavigationType\n"));

    if (XmIsPrimitive(w))
    {
	return Prim_NavigationType(w);
    }

    if (XmIsManager(w))
    {
	return MGR_NavigationType(w);
    }

    if (XmIsGadget(w))
    {
	return G_NavigationType(w);
    }

    return XmNONE;
}


extern Boolean
_XmGetEffectiveView(Widget w, XRectangle *rect)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetEffectiveView\n"));

    return EffectiveView(w, NULL, rect);
}


extern Boolean
_XmIsTraversable(Widget w, Boolean vischeck)
{
    XRectangle rect;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmIsTraversable\n"));
    DEBUGOUT(_LtDebug("MENU", w, "_XmIsTraversable\n"));

    if (w == NULL || !XtIsManaged(w))
    {
	DEBUGOUT(_LtDebug0("MENU", w, "\tNot-managed\n"));
	return False;
    }

    if (!_XmIsNavigable(w))
    {
	DEBUGOUT(_LtDebug0("MENU", w, "\tNot-navigable\n"));
	return False;
    }

    if (vischeck)
    {
	if (XmGetVisibility(w) == XmVISIBILITY_FULLY_OBSCURED)
	{
	    return False;
	}

	return True;
    }

    return _XmGetEffectiveView(w, &rect);
}


extern Boolean
_XmIsNavigable(Widget w)
{
    XmNavigability nav;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmIsNavigable()\n"));
    DEBUGOUT(_LtDebug("MENU", w, "_XmIsNavigable()\n"));

    if (!w)
    {
	return False;
    }

    nav = _XmGetNavigability(w);
    if (nav != XmTAB_NAVIGABLE && nav != XmCONTROL_NAVIGABLE)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, " returns False 1: %s\n", _LtDebugNavigability2String(nav)));
	DEBUGOUT(_LtDebug0("MENU", w, " returns False 1: %s\n", _LtDebugNavigability2String(nav)));

	return False;
    }

    w = XtParent(w);

    while (w)
    {
	if (XtIsShell(w))
	{
	    break;
	}

	nav = _XmGetNavigability(w);
	if (nav == XmNOT_NAVIGABLE)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, " returns False 2: %s\n", _LtDebugNavigability2String(nav)));
	    DEBUGOUT(_LtDebug("MENU", w, " returns False 2: %s\n", _LtDebugNavigability2String(nav)));
	    return False;
	}

	w = XtParent(w);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, " returns True\n"));
    DEBUGOUT(_LtDebug("MENU", w, " returns True\n"));

    return True;
}


extern Boolean
_XmIsViewable(Widget w)
{
    XWindowAttributes wa;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmIsViewable\n"));

    if (CoreBeingDestroyed(w))
    {
	return False;
    }

    if (!XtIsRealized(w))
    {
	return False;
    }

    if (!XtIsManaged(w))
    {
	return False;
    }

    if (XmIsGadget(w))
    {
	return True;
    }

    if (CoreMappedWhenManaged(w))
    {
	return True;
    }

    XGetWindowAttributes(XtDisplay(w), XtWindow(w), &wa);
    if (wa.map_state != IsViewable)
    {
	return False;
    }

    return True;
}


extern void
_XmValidateFocus(Widget wid)
{
    XmFocusData fd = _XmGetFocusData(wid);
    Widget w;

    if (fd == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmValidateFocus(fd NULL)\n"));
	return;
    }

    if (fd->focus_policy != XmEXPLICIT)
    {
	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmValidateFocus (fp != XmEXPLICIT)\n"));
	return;
    }

    if (fd->focus_item == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmValidateFocus (fi != NULL)\n"));
	return;
    }

    if (_XmIsTraversable(fd->focus_item, True))
    {
	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmValidateFocus (fi traversable)\n"));
	return;
    }

    if (fd->focus_item == fd->active_tab_group)
    {
	w = _XmTraverseAway(&fd->tree, fd->focus_item, False);
    }
    else
    {
	w = _XmTraverseAway(&fd->tree, fd->focus_item, True);
    }

    if (w == NULL)
    {
	w = wid;
    }

    _XmMgrTraversal(w, XmTRAVERSE_CURRENT);
}


extern void
_XmSetActiveTabGroup(XmFocusData focusData,
		     Widget tabGroup)
{
    DEBUGOUT(_LtDebug(__FILE__, tabGroup, "_XmSetActiveTabGroup\n"));

    focusData->active_tab_group = tabGroup;
}


extern Widget
_XmGetActiveTabGroup(Widget widget)
{
    XmFocusData fd = _XmGetFocusData(widget);

    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmGetActiveTabGroup\n"));

    if (!fd)
    {
	return NULL;
    }

    return fd->active_tab_group;
}


extern Widget
_XmFindNextTabGroup(Widget wid)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmFindNextTabGroup\n"));

    return _XmNavigate(wid, XmTRAVERSE_NEXT_TAB_GROUP);
}


extern Widget
_XmFindPrevTabGroup(Widget wid)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmFindPrevTabGroup\n"));

    return _XmNavigate(wid, XmTRAVERSE_PREV_TAB_GROUP);
}


extern Boolean
_XmSetInitialOfTabGraph(XmTravTree tree, Widget tab, Widget first)
{
    XmTravTreeNode node, tnode;

    DEBUGOUT(_LtDebug(__FILE__, tab, "_XmSetInitialOfTabGraph\n"));

    if ((node = GetNodeOfWidget(tree, tab)) == NULL)
    {
	return False;
    }

    if (node->type != XmTAB_TREE_NODE)
    {
	if (node->type != XmCONTROL_TREE_NODE)
	    return False;
    }

    if (SetInitialNode(node, GetNodeFromGraph(node, first)))
    {
	return True;
    }

    if ((tnode = GetNodeFromGraph(node, tab)) == NULL)
    {
	return False;
    }

    if (!SetInitialNode(tnode, GetNodeFromGraph(tnode, first)))
    {
	return False;
    }

    if (!SetInitialNode(node, tnode))
    {
	return False;
    }

    return True;
}


extern void
_XmSetInitialOfTabGroup(Widget tab_group,
			Widget init_focus)
{
    XmFocusData fd;

    DEBUGOUT(_LtDebug(__FILE__, tab_group, "_XmSetInitialOfTabGroup\n"));

    if (XmIsManager(tab_group))
    {
	MGR_InitialFocus(tab_group) = init_focus;
    }

    if ((fd = _XmGetFocusData(tab_group)) == NULL)
    {
	return;
    }

    if (fd->tree.num_entries == 0)
    {
	return;
    }

    _XmSetInitialOfTabGraph(&fd->tree, tab_group, init_focus);
}


extern Widget
_XmGetActiveItem(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetActiveItem\n"));

    return XmGetFocusWidget(w);
}


extern void
_XmTabListAdd(XmTravTree tree, Widget w)
{
    int node;
    Widget shell;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTabListAdd\n"));

    if ((node = SearchTabList(tree, w)) >= 0)
    {
	return;
    }

    if (tree->num_tab_alloc == 0)
    {
	shell = _XmFindTopMostShell(w);

	tree->num_tab_alloc = ALLOC_INCR;

	tree->excl_tabs = (Widget *)XtCalloc(tree->num_tab_alloc,
					     sizeof(Widget));

	tree->excl_tabs[0] = shell;
	tree->num_tab_entries = 1;
    }

    if (tree->num_tab_entries == tree->num_tab_alloc)
    {
	tree->num_tab_alloc += ALLOC_INCR;

	tree->excl_tabs = (Widget *)XtRealloc((char *)tree->excl_tabs,
					      tree->num_tab_alloc *
					      sizeof(Widget));
    }

    tree->excl_tabs[tree->num_tab_entries] = w;
    tree->num_tab_entries++;
}


extern void
_XmTabListDelete(XmTravTree tree, Widget w)
{
    int node;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTabListDelete\n"));

    node = SearchTabList(tree, w);

    DeleteFromTabList(tree, node);

    if (tree->num_tab_entries + ALLOC_INCR < tree->num_tab_alloc)
    {
	tree->num_tab_alloc -= ALLOC_INCR;
	tree->excl_tabs = (Widget *)XtRealloc((char *)tree->excl_tabs,
					      tree->num_tab_alloc *
					      sizeof(Widget));
    }
}


extern void
_XmClearFocusPath(Widget wid)
{
    XmFocusData fd;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmClearFocusPath\n"));

    while (wid != NULL)
    {
	if (XtIsShell(wid))
	{
	    if ((fd = _XmGetFocusData(wid)) == NULL)
	    {
		return;
	    }

	    fd->active_tab_group = NULL;
	    fd->focus_item = NULL;
	    fd->old_focus_item = NULL;

	    return;
	}

	if (XmIsManager(wid))
	{
	    MGR_ActiveChild(wid) = NULL;
	}

	wid = XtParent(wid);
    }
}


extern Boolean
_XmFocusIsHere(Widget w)
{
    XmFocusData fd = _XmGetFocusData(w);
    Widget par;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFocusIsHere\n"));

    if (!fd)
    {
	return False;
    }

    if (fd->focus_item == NULL)
    {
	return False;
    }

    par = fd->focus_item;
    while (!XtIsShell(par))
    {
	if (par == w)
	{
	    return True;
	}

	par = XtParent(par);
    }

    return False;
}


extern void
_XmFocusModelChanged(Widget wid,
		     XtPointer client_data,
		     XtPointer call_data)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmFocusModelChanged\n"));
}


extern void
_XmSetRect(XRectangle *rect,
	   Widget w)
{
    Position rx, ry;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmSetRect\n"));

    XtTranslateCoords(XtParent(w), XtX(w), XtY(w), &rx, &ry);

    rect->x = rx + XtBorderWidth(w);
    rect->y = ry + XtBorderWidth(w);
    rect->width = XtWidth(w);
    rect->height = XtHeight(w);
}


extern Boolean
_XmIntersectionOf(XRectangle *a, XRectangle *b, XRectangle *dest)
{
    Position ax, ay, bx, by, tmp;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmIntersectionOf\n"));

    ax = a->x + a->width;
    bx = b->x + b->width;
    ay = a->y + a->height;
    by = b->y + b->height;

    ax--;
    bx--;
    ay--;
    by--;

    if (a->x >= b->x)
    {
	dest->x = a->x;
    }
    else
    {
	dest->x = b->x;
    }

    if (a->y >= b->y)
    {
	dest->y = a->y;
    }
    else
    {
	dest->y = b->y;
    }

    if (ax < bx)
    {
	tmp = ax - dest->x + 1;
    }
    else
    {
	tmp = bx - dest->x + 1;
    }

    if (tmp < 0)
    {
	tmp = 0;
    }

    dest->width = tmp;

    if (ay < by)
    {
	tmp = ay - dest->y + 1;
    }
    else
    {
	tmp = by - dest->y + 1;
    }

    if (tmp < 0)
    {
	tmp = 0;
    }

    dest->height = tmp;

    if (dest->width == 0 || dest->height == 0)
    {
	return False;
    }

    return True;
}


extern int
_XmIntersectRect(XRectangle *srcRectA,
		 Widget widget,
		 XRectangle *dstRect)
{
    Position rx, ry;
    XRectangle rect;

    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmIntersectRect\n"));

    XtTranslateCoords(XtParent(widget), XtX(widget), XtY(widget), &rx, &ry);

    rx += XtBorderWidth(widget);
    ry += XtBorderWidth(widget);

    rect.x = rx;
    rect.y = ry;
    rect.width = XtWidth(widget);
    rect.height = XtHeight(widget);

    return _XmIntersectionOf(srcRectA, &rect, dstRect);
}


extern int
_XmEmptyRect(XRectangle *r)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmEmptyRect\n"));

    if (r->x == 0 && r->height == 0)
    {
	return True;
    }

    return False;
}


extern void
_XmClearRect(XRectangle *r)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmClearRect\n"));

    r->x = 0;
    r->y = 0;
    r->width = 0;
    r->height = 0;
}


extern Widget
_XmGetClippingAncestor(Widget w, XRectangle *rect)
{
    XRectangle rect_in, rect_out;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetClippingAncestor\n"));

    if (w == NULL)
    {
	return NULL;
    }

    w = XtParent(w);

    while (w && !XtIsShell(w))
    {
	_XmSetRect(&rect_in, w);
	if (!_XmIntersectionOf(rect, &rect_in, &rect_out))
	{
	    return w;
	}
	if (rect->width != rect_out.width || rect->height != rect_out.height)
	{
	    return w;
	}

	w = XtParent(w);
    }

    return NULL;
}


extern Widget
_XmIsScrollableClipWidget(Widget w, XRectangle *rect)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmIsScrollableClipWidget\n"));

    if (!XmIsDrawingArea(w))
    {
	return NULL;
    }

    if (DA_ResizePolicy(w) == XmRESIZE_NONE)
    {
	return NULL;
    }

    if (!XmIsScrolledWindow(XtParent(w)) ||
	((Widget)SW_ClipWindow(XtParent(w)) != w &&
	 SW_WorkWindow(XtParent(w)) != w))
    {
	return NULL;
    }

    _XmSetRect(rect, w);

    return XtParent(w);
}

/*
 * Function Name: _XmCreateVisibilityRect
 *
 * If the widget isn´t visible the function returns False, else it
 * creates the intersection between the widget and all it´s parents execpt
 * the shell and returns True.
 */

extern Boolean
_XmCreateVisibilityRect(Widget w,
			XRectangle *rectPtr)
{
    Widget cw;
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmCreateVisibilityRect\n"));

    if (!_XmIsViewable(w))
    {
	_XmClearRect(rectPtr);
	return False;
    }
    if (w && XtParent(w) &&
	(cw =_XmIsScrollableClipWidget(XtParent(w), rectPtr)))
    {
	w = cw;
	if (!_XmIsViewable(w))
	{
	    _XmClearRect(rectPtr);
	    return False;
	}
    }

    _XmSetRect(rectPtr, w);

    while (w && !XtIsShell(w))
    {
	if (_XmIsViewable(w) && _XmIntersectRect(rectPtr, w, rectPtr))
	{
	    w = XtParent(w);
	}
	else
	{
	    _XmClearRect(rectPtr);
	    return False;
	}
    }
    return True;
}


extern Boolean
_XmCallFocusMoved(Widget old,
		  Widget new_wid,
		  XEvent *event)
{
    Widget tsh;
    Widget ve;
    XmFocusMovedCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCallFocusMoved(): %s %s\n",
		      old ? XtName(old) : "(null)",
		      new_wid ? XtName(new_wid) : "(null)"));

    if (old)
    {
	tsh = _XmFindTopMostShell(old);
    }
    else
    {
	tsh = _XmFindTopMostShell(new_wid);
    }
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCallFocusMoved(): tsh %s\n",
		      tsh ? XtName(tsh) : "(null)"
		      ));

    if (!XtIsSubclass(tsh, vendorShellWidgetClass))
    {
	return True;
    }

    ve = _LtFindVendorExt(tsh);
    if (!ve)
    {
	return True;
    }

    if (!VSEP_FocusMovedCallback(ve))
    {
	return True;
    }

    cbs.reason = XmCR_FOCUS;
    cbs.event = event;
    cbs.cont = True;
    cbs.old_focus = old;
    cbs.new_focus = new_wid;
    cbs.focus_policy = VSEP_FocusPolicy(ve);

    XtCallCallbackList(tsh, VSEP_FocusMovedCallback(ve), (XtPointer)&cbs);

    return cbs.cont;
}


static Boolean
_XmCallTraverseObscured(Widget w, XmTraversalDirection dir)
{
    XRectangle rect;
    XmTraverseObscuredCallbackStruct cbs;
    Widget anc, scr;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmCallTraverseObscured\n"));

    cbs.reason = XmCR_OBSCURED_TRAVERSAL;
    cbs.event = NULL;
    cbs.traversal_destination = w;
    cbs.direction = dir;

    _XmSetRect(&rect, w);
    anc = w;

    while ((anc = _XmGetClippingAncestor(anc, &rect)) != NULL)
    {
	if ((scr = _XmIsScrollableClipWidget(anc, &rect)) != NULL)
	{
	    XtCallCallbackList(scr, SW_TraverseObscuredCallback(scr),
			       (XtPointer)&cbs);
	    anc = scr;
	}
	else
	{
	    _XmIntersectRect(&rect, anc, &rect);
	}
    }

    return _XmIsTraversable(w, True);
}


extern void
_XmWidgetFocusChange(Widget wid,
		     XmFocusChange change)
{
    XmBaseClassExt *bce;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmWidgetFocusChange: %s\n",
		_LtDebugFocusChange2String(change)));

    if (!XtIsRectObj(wid))
	return;

    if (CoreBeingDestroyed(wid))
    {
	return;
    }

    bce = _XmGetBaseClassExtPtr(XtClass(wid), XmQmotif);

    if (bce && *bce && (*bce)->focusChange)
    {
		((*bce)->focusChange) (wid, change);
    }
	else
	{
		DEBUGOUT(_LtDebug(__FILE__, wid,
			"_XmWidgetFocusChange: couldn't call (*bce)->focusChange\n"));
	}
}


extern Boolean
_XmFocusIsInShell(Widget wid)
{
    XmFocusData fd;
    Window fw;
    int revert;
    Widget w;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmFocusIsInShell\n"));

    wid = _XmFindTopMostShell(wid);

    if (XtIsSubclass(wid, vendorShellWidgetClass))
    {
	fd = _XmGetFocusData(wid);
	if (fd)
	{
	    if (fd->focal_point == XmNO_RELATION)
	    {
		return False;
	    }
	    else
	    {
		return True;
	    }
	}
    }

    XGetInputFocus(XtDisplay(wid), &fw, &revert);
    if (fw == PointerRoot)
	return False;

    if (fw == None)
    {
	return False;
    }

    w = XtWindowToWidget(XtDisplay(wid), fw);
    if (w == NULL)
    {
	return False;
    }

    w = _XmFindTopMostShell(w);

    if (wid == w)
    {
	return True;
    }

    return False;
}


extern Boolean
_XmShellIsExclusive(Widget wid)
{
    XmFocusData fd = _XmGetFocusData(wid);

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmShellIsExclusive\n"));

    if (!fd)
    {
	return False;
    }

    DEBUGOUT(_LtDebug(__FILE__, wid,
		      "XmShell is eclusive: %d\n", fd->tree.num_excls));

    if (fd->tree.num_excls != 0)
    {
	return True;
    }

    return False;
}


extern Boolean
_XmGrabTheFocus(Widget w,
		XEvent *event)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGrabTheFocus\n"));

    return _XmMgrTraversal(w, XmTRAVERSE_CURRENT);
}


extern Widget
_XmTraverse(XmTravTree tree, XmTraversalDirection dir, Widget w)
{
    XmNavigability nv;
    XmTravTreeNode nd;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTraverse\n"));

    if (dir == XmTRAVERSE_CURRENT && w != NULL)
    {
	nv = _XmGetNavigability(w);
	if ((nv == XmTAB_NAVIGABLE || nv == XmCONTROL_NAVIGABLE)
	    && XmIsTraversable(w))
	{
	    return w;
	}

	return NULL;
    }

    if (tree->num_entries == 0)
    {
	if (!_XmNewTravGraph(tree, tree->shell, w))
	{
	    return NULL;
	}
    }
    else
    {
	if (!InitializeCurrent(tree, w, True))
	{
	    return NULL;
	}
    }

    if (dir == XmTRAVERSE_CURRENT)
    {
	if (tree->current->widget != w)
	{
	    return NULL;
	}

	if (tree->current->type == XmTAB_NODE ||
	    tree->current->type == XmCONTROL_NODE)
	{
	    if (NodeIsTraversable(tree->current))
	    {
		return w;
	    }
	    else
	    {
		return NULL;
	    }
	}
    }

    if (dir == XmTRAVERSE_NEXT_TAB_GROUP || dir == XmTRAVERSE_PREV_TAB_GROUP)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmTraverseAway: call TraverseTab(%d GROUP)\n",
			  dir));

	nd = TraverseTab(tree->current, dir);
    }
    else
    {
	if (dir != XmTRAVERSE_CURRENT ||
	    tree->current->type == XmCONTROL_TREE_NODE)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmTraverseAway: call TraverseControl(%d)\n",
			      dir));

	    nd = TraverseControl(tree->current, dir);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmTraverseAway: call TraverseTab(%d)\n",
			      dir));

	    nd = TraverseTab(tree->current, dir);
	}
    }

    if (nd)
    {
	tree->current = nd;
	return nd->widget;
    }

    return NULL;
}


extern Widget
_XmTraverseAway(XmTravTree tree, Widget w, Boolean control)
{
    XmTravTreeNode node = NULL;
    XRectangle rect;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTraverseAway\n"));

    if (tree->num_entries == 0)
    {
	if (!_XmNewTravGraph(tree, tree->shell, w))
	{
	    return NULL;
	}

	if (!InitializeCurrent(tree, w, True))
	{
	    return NULL;
	}
    }

    if (tree->current->widget != w && tree->current->type == XmTAB_TREE_NODE)
    {
	if (control)
	{
	    tree->current++;
	}

	GetRectRelativeToShell(w, &rect);
	node = GetNextNearestNode(tree->current, &rect);

	if (node)
	{
	    tree->current = node;
	}
    }

    if (tree->current->widget == w || NodeIsTraversable(tree->current))
    {

	node = NULL;

	if (tree->current->type == XmCONTROL_NODE ||
	    tree->current->type == XmCONTROL_TREE_NODE)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmTraverse: call TraverseControl(RIGHT)\n"));

	    node = TraverseControl(tree->current, XmTRAVERSE_RIGHT);
	}

	if (!node)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmTraverseAway: call TraverseTab\n"));

	    node = TraverseTab(tree->current, XmTRAVERSE_NEXT_TAB_GROUP);
	}

	tree->current = node;
    }

    if (tree->current && tree->current->widget == w)
    {
	return tree->current->widget;
    }

    return NULL;
}


extern Boolean
_XmMgrTraversal(Widget widget, XmTraversalDirection direction)
{
    Widget shell = _XmFindTopMostShell(widget);
    Widget tg, cur_focus;
    XmFocusData fd;
    static Boolean in_traversal = False;
    Boolean ret = False;

    shell = _XmFindTopMostShell(widget);
    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmMgrTraversal\n"));

    if (in_traversal)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "_XmMgrTraversal: InTraversal already\n"));
	return False;
    }

    if (shell == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget, "_XmMgrTraversal: NoShell\n"));
	return False;
    }

    if (CoreBeingDestroyed(shell))
    {
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "_XmMgrTraversal: BeingDestroyed\n"));
	return False;
    }

    if ((fd = _XmGetFocusData(shell)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget, "_XmMgrTraversal: NoFocusData\n"));
	return False;
    }

    if (fd->focus_policy != XmEXPLICIT)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget, "_XmMgrTraversal: NotExplicit\n"));
	return False;
    }

    in_traversal = True;

    cur_focus = fd->focus_item;

    if (cur_focus == NULL && widget == shell && fd->first_focus != NULL
	&& _XmIsTraversable(fd->first_focus, True))
    {
	DEBUGOUT(_LtDebug(__FILE__, widget, "_XmMgrTraversal: first_focus\n"));
	widget = fd->first_focus;
    }
    else
    {
	Widget	ow = widget;
	widget = _XmTraverse(&fd->tree, direction, widget);
	DEBUGOUT(_LtDebug2(__FILE__, ow, widget, "_XmMgrTraversal: _XmTraverse\n"));
	if (widget && shell != _XmFindTopMostShell(widget))
	{
		/* FIX ME */
		_XmWarning(shell, "%s:_XmMgrTraversal(%d) - I just Traversed into a different shell!!!", __FILE__, __LINE__);
		widget = NULL;
	}
    }

    if (widget == NULL || widget != cur_focus || fd->old_focus_item == NULL)
    {

	if (widget != NULL && ((tg = XmGetTabGroup(widget)) != NULL) &&
	    _XmCallFocusMoved(cur_focus, widget, NULL) &&
	    _XmCallTraverseObscured(widget, direction))
	{

	    _XmSetFocusResetFlag(shell, True);

	    XtSetKeyboardFocus(shell, NULL);

	    _XmSetFocusResetFlag(shell, False);

	    _XmClearFocusPath(cur_focus);

	    fd->active_tab_group = tg;
	    if (widget != tg && XmIsManager(tg))
	    {
		MGR_ActiveChild(tg) = widget;
	    }

	    if (XtParent(widget) != tg)
	    {
		if (XmIsManager(XtParent(widget)))
		    MGR_ActiveChild(XtParent(widget)) = widget;
	    }

	    if (cur_focus == NULL)
	    {
		cur_focus = widget;
	    }

	    fd->focus_item = widget;
	    fd->old_focus_item = cur_focus;

#if defined(DEBUG)
if (strcmp(XtName(shell), "mgdiff") == 0 &&
    strcmp(XtName(widget), "Text") == 0)
{
Widget tmp = widget;

    printf("%s %d\n", __FILE__, __LINE__);
    while (tmp != NULL)
    {
	    printf("%s %s %s\n", 
		    XtName(tmp),
		    XtClass(tmp)->core_class.class_name,
		    _XmFindTopMostShell(tmp) ? XtName(_XmFindTopMostShell(tmp)) : "NULL"
		    );
	    tmp = XtParent(tmp);
    }
}
#endif /* DEBUG */
	    DEBUGOUT(_LtDebug("RWS1", shell,
			      "KeyboardFocus set to %s %s %p\n",
			      shell ? XtName(shell) : "(null)",
			      widget ? XtName(widget) : "(null)",
			      widget));
	    DEBUGOUT(_LtDebug(__FILE__, shell,
			      "KeyboardFocus set to %s %s\n",
			      shell ? XtName(shell) : "(null)",
			      widget ? XtName(widget) : "(null)"));

	    XtSetKeyboardFocus(shell, widget);

	    ret = True;
	}
	else if (!_XmIsTraversable(cur_focus, True))
	{

	    widget = FindFirstManaged(shell);

	    _XmSetFocusResetFlag(shell, True);

	    DEBUGOUT(_LtDebug2(__FILE__, shell, widget,
			"XtSetKeyboardFocus -> child\n"));

	    XtSetKeyboardFocus(shell, widget);

	    _XmSetFocusResetFlag(shell, False);

	    _XmClearFocusPath(cur_focus);

	    _XmFreeTravGraph(&fd->tree);
	}
    }
    else
    {
	ret = True;
    }

    if (fd->tree.num_entries != 0 && fd->focal_point == XmNO_RELATION)
    {
	if (XtIsSubclass(shell, vendorShellWidgetClass)
	    && _XmFocusIsInShell(shell))
	{
	    _XmFreeTravGraph(&fd->tree);
	}
    }

    in_traversal = False;

    return ret;
}


extern void
_XmProcessTraversal(Widget widget,
		    XmTraversalDirection direction,
		    Boolean check)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmProcessTraversal\n"));

    _XmMgrTraversal(widget, direction);
}


extern Widget
_XmNavigate(Widget wid,
	    XmTraversalDirection direction)
{
    Widget shell = _XmFindTopMostShell(wid), ret;
    XmFocusData fd = _XmGetFocusData(shell);

    if (fd == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmNavigate (fd NULL) => NULL\n"));
	return NULL;
    }

    if (fd->focus_policy != XmEXPLICIT)
    {
	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmNavigate (fp != explicit) => NULL\n"));
	return NULL;
    }

    ret = _XmTraverse(&fd->tree, direction, wid);

    if (fd->tree.num_entries == 0)
    {
	DEBUGOUT(_LtDebug2(__FILE__, wid, ret, "_XmNavigate (#entries == 0)\n"));
	return ret;
    }

    if (fd->focal_point != XmNO_RELATION)
    {
	DEBUGOUT(_LtDebug2(__FILE__, wid, ret, "_XmNavigate (NO_RELATION)\n"));
	return ret;
    }

    if (XtIsSubclass(shell, vendorShellWidgetClass) &&
	_XmFocusIsInShell(shell))
    {
	_XmFreeTravGraph(&fd->tree);
    }

    return ret;
}


extern Widget
_XmGetFirstFocus(Widget wid)
{
    XmFocusData fd;
    Widget hierarchy;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGetFirstFocus\n"));

    fd = _XmGetFocusData(wid);

    if (fd == NULL)
    {
	return NULL;
    }

    if (fd->focus_item)
    {
	return fd->focus_item;
    }

    if (fd->first_focus)
    {
	return fd->first_focus;
    }

    hierarchy = _XmFindTopMostShell(wid);

    fd->first_focus = _XmNavigate(hierarchy, XmTRAVERSE_CURRENT);

    return fd->first_focus;
}

/************************* method functions *****************************/


extern void
_XmNavigInitialize(Widget request,
		   Widget new_wid,
		   ArgList args,
		   Cardinal *num_args)
{
    XmFocusData fd;
    XmNavigationType nt = _XmGetNavigationType(new_wid);

    DEBUGOUT(_LtDebug("RWS", new_wid, "_XmNavigInitialize\n"));
    DEBUGOUT(_LtDebug(__FILE__, new_wid, "_XmNavigInitialize\n"));

    fd = _XmGetFocusData(new_wid);
    if (fd == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, new_wid,
		"_XmNavigInitialize : NULL FocusData\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, new_wid,
		"_XmNavigInitialize, navigation type %s\n",
		_LtDebugNavigationType2String(nt)));

    if (nt == XmEXCLUSIVE_TAB_GROUP)
    {
	fd->tree.num_excls++;
    }

    if (nt == XmEXCLUSIVE_TAB_GROUP || nt == XmSTICKY_TAB_GROUP)
    {
	_XmTabListAdd(&fd->tree, new_wid);
    }

    if (fd->tree.num_entries != 0 &&
	_XmGetNavigability(new_wid) != XmNOT_NAVIGABLE)
    {
	_XmTravGraphAdd(&fd->tree, new_wid);
    }
}


extern Boolean
_XmNavigSetValues(Widget current,
		  Widget request,
		  Widget new_wid,
		  ArgList args,
		  Cardinal *num_args)
{
    XmFocusData fd = _XmGetFocusData(new_wid);
    XmNavigationType cur_nt, new_nt;
    XmNavigability cur_nav, new_nav;
    Boolean tchange = False;
    Widget tmp;

    DEBUGOUT(_LtDebug(__FILE__, new_wid, "_XmNavigSetValues\n"));

    if (!fd)
    {
	return False;
    }

    new_nt = _XmGetNavigationType(new_wid);
    cur_nt = _XmGetNavigationType(current);

    if (cur_nt != new_nt)
    {
	if (cur_nt == XmEXCLUSIVE_TAB_GROUP &&
	    new_nt != XmEXCLUSIVE_TAB_GROUP)
	{
	    tchange = True;
	    fd->tree.num_excls--;
	}
	else if (cur_nt != XmEXCLUSIVE_TAB_GROUP &&
		 new_nt == XmEXCLUSIVE_TAB_GROUP)
	{
	    tchange = True;
	    fd->tree.num_excls++;
	}
    }

    if (new_nt == XmEXCLUSIVE_TAB_GROUP || new_nt == XmSTICKY_TAB_GROUP)
    {
	if (cur_nt != XmEXCLUSIVE_TAB_GROUP && cur_nt != XmSTICKY_TAB_GROUP)
	{
	    _XmTabListAdd(&fd->tree, new_wid);
	}
    }
    else
    {
	if (cur_nt == XmEXCLUSIVE_TAB_GROUP && cur_nt == XmSTICKY_TAB_GROUP)
	{
	    _XmTabListDelete(&fd->tree, new_wid);
	}
    }

    if (!XtIsRealized(new_wid))
    {
	return False;
    }

    if (fd->focus_policy != XmEXPLICIT)
    {
	return False;
    }

    if (fd->tree.num_entries != 0)
    {
	new_nav = _XmGetNavigability(new_wid);
	cur_nav = _XmGetNavigability(current);
	if (!tchange)
	{
	    if (cur_nav == XmNOT_NAVIGABLE && new_nav != XmNOT_NAVIGABLE)
	    {
		_XmTravGraphAdd(&fd->tree, new_wid);
	    }
	}
	if (tchange || cur_nav != new_nav)
	{
	    _XmFreeTravGraph(&fd->tree);
	}
    }

    if (fd->focus_item == NULL)
    {
	if (!XmIsTraversable(new_wid))
	{
	    return False;
	}

	tmp = _XmFindTopMostShell(new_wid);
	if (!tmp)
	{
	    return False;
	}

	if (!_XmFocusIsInShell(tmp))
	{
	    return False;
	}

	if (fd->focal_point == XmME)
	{
	    return False;
	}

	_XmMgrTraversal(new_wid, XmTRAVERSE_CURRENT);

	return False;
    }

    if (fd->focus_item != new_wid)
    {
	return False;
    }

    if (_XmIsTraversable(new_wid, True))
    {
	return False;
    }

    if (fd->active_tab_group == new_wid)
    {
	tchange = False;
    }
    else
    {
	tchange = True;
    }

    tmp = _XmTraverseAway(&fd->tree, new_wid, tchange);

    if (tmp == NULL)
    {
	tmp = new_wid;
    }

    _XmMgrTraversal(tmp, XmTRAVERSE_CURRENT);

    if (!XtSensitive(new_wid))
    {
	_XmWidgetFocusChange(new_wid, XmFOCUS_OUT);
    }

    return True;
}


extern void
_XmNavigChangeManaged(Widget w)
{
    XmFocusData fd;
    Widget tmp;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmNavigChangeManaged\n"));

    if (!XtIsRealized(w))
    {
	return;
    }

    if ((fd = _XmGetFocusData(w)) == NULL)
    {
	return;
    }

    if (fd->focus_policy != XmEXPLICIT)
    {
	return;
    }

    if (fd->focus_item == NULL)
    {
	if (!XtIsRectObj(w))
	{
	    return;
	}

	if (fd->first_focus == NULL)
	{
	    fd->first_focus = FindFirstFocus(w);
	}

	if ((tmp = FindFirstManaged(w)) == NULL)
	{
	    return;
	}

	XtSetKeyboardFocus(w, tmp);

	return;
    }

    if (CoreBeingDestroyed(fd->focus_item))
    {
	return;
    }

    if (_XmIsTraversable(fd->focus_item, True))
    {
	return;
    }

    if (fd->focus_item == fd->active_tab_group)
    {
	tmp = _XmTraverseAway(&fd->tree, fd->focus_item, False);
    }
    else
    {
	tmp = _XmTraverseAway(&fd->tree, fd->focus_item, True);
    }

    if (tmp == NULL)
    {
	tmp = fd->focus_item;
    }

    _XmMgrTraversal(tmp, XmTRAVERSE_CURRENT);
}


extern void
_XmNavigResize(Widget wid)
{
    XmFocusData fd;
    Widget tmp;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmNavigResize\n"));

    if (!XtIsRealized(wid))
    {
	return;
    }

    if (XtIsRectObj(wid))
    {
	return;
    }

    if ((fd = _XmGetFocusData(wid)) == NULL)
    {
	return;
    }

    if (fd->focus_policy != XmEXPLICIT)
    {
	return;
    }

    if (fd->focus_item != NULL)
    {
	if (CoreBeingDestroyed(fd->focus_item))
	{
	    return;
	}
    }

    if (fd->focus_item == NULL)
    {
	if (XtParent(wid) == NULL)
	{
	    return;
	}

	if ((tmp = FindFirstManaged(wid)) == NULL)
	{
	    return;
	}

	XtSetKeyboardFocus(wid, tmp);

	return;
    }

    if (_XmIsTraversable(fd->focus_item, True))
    {
	return;
    }

    if (_XmIsTraversable(fd->focus_item, False))
    {
	if (_XmMgrTraversal(fd->focus_item, XmTRAVERSE_CURRENT))
	{
	    return;
	}
    }

    if (fd->focus_item == fd->active_tab_group)
    {
	tmp = _XmTraverseAway(&fd->tree, fd->focus_item, False);
    }
    else
    {
	tmp = _XmTraverseAway(&fd->tree, fd->focus_item, True);
    }

    if (tmp == NULL)
    {
	tmp = fd->focus_item;
    }

    _XmMgrTraversal(tmp, XmTRAVERSE_CURRENT);
}


extern void
_XmNavigDestroy(Widget wid)
{
    XmFocusData fd = _XmGetFocusData(wid);
    XmNavigationType nt;
    Widget tmp, shell;

    DEBUGOUT(_LtDebug("RWS", wid, "_XmNavigDestroy\n"));
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmNavigDestroy\n"));

    if (!fd)
    {
	return;
    }
    DEBUGOUT(_LtDebug("RWS", wid, "_XmNavigDestroy %s\n",
    	fd->focus_item ? XtName(fd->focus_item) : "NULL"
    	));

    if (fd->first_focus == wid)
    {
	fd->first_focus = NULL;
    }

    nt = _XmGetNavigationType(wid);

    if (nt == XmEXCLUSIVE_TAB_GROUP || nt == XmSTICKY_TAB_GROUP)
    {
	if (nt == XmEXCLUSIVE_TAB_GROUP)
	{
	    fd->tree.num_excls--;
	}
	_XmTabListDelete(&fd->tree, wid);
    }

    if (fd->focus_item == wid)
    {
	if (fd->focus_policy != XmEXPLICIT)
	{
	    fd->focus_item = NULL;
	}
	else
	{
	    if (wid == fd->active_tab_group)
	    {
		tmp = _XmTraverseAway(&fd->tree, fd->focus_item, False);
	    }
	    else
	    {
		tmp = _XmTraverseAway(&fd->tree, fd->focus_item, True);
	    }

	    if (tmp && (shell = _XmFindTopMostShell(wid)))
	    {
		if (!_XmMgrTraversal(shell, XmTRAVERSE_CURRENT))
		{
		    fd->focus_item = NULL;
		}
	    }
	    else
	    {
		fd->focus_item = NULL;
	    }
	}
    DEBUGOUT(_LtDebug("RWS", wid, "_XmNavigDestroy %s\n",
    	fd->focus_item ? XtName(fd->focus_item) : "NULL"
    	));
    }

    if (fd->tree.num_entries != 0)
    {
	_XmTravGraphRemove(&fd->tree, wid);
    }

    if (fd->active_tab_group == wid)
    {
	fd->active_tab_group = NULL;
    }

    if (fd->old_focus_item == wid)
    {
	fd->old_focus_item = NULL;
    }

    if (fd->pointer_item == wid)
    {
	fd->pointer_item = NULL;
    }
}

/************************ EXPORTED FUNCTIONS ************************/
extern void
XmAddTabGroup(Widget tab_group)
{
    Arg args[1];

    DEBUGOUT(_LtDebug(__FILE__, tab_group, "XmAddTabGroup\n"));

    XtSetArg(args[0], XmNnavigationType, XmEXCLUSIVE_TAB_GROUP);
    XtSetValues(tab_group, args, 1);
}


extern void
XmRemoveTabGroup(Widget tab_group)
{
    Arg args[1];

    DEBUGOUT(_LtDebug(__FILE__, tab_group, "XmRemoveTabGroup\n"));

    XtSetArg(args[0], XmNnavigationType, XmNONE);
    XtSetValues(tab_group, args, 1);
}


extern Widget
XmGetTabGroup(Widget widget)
{
    XmFocusData fd;
    XmNavigationType nt;
    Boolean loop;

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmGetTabGroup\n"));

    if (!widget)
    {
	return NULL;
    }

    if (_XmGetFocusPolicy(widget) != XmEXPLICIT)
    {
	return NULL;
    }

    fd = _XmGetFocusData(widget);

    if (!fd)
    {
	return NULL;
    }

    if (fd->tree.num_excls == 0)
    {
	loop = False;
    }
    else
    {
	loop = True;
    }

    for (;;)
    {
	nt = _XmGetNavigationType(widget);

	if (nt == XmSTICKY_TAB_GROUP || nt == XmEXCLUSIVE_TAB_GROUP)
	{
	    return widget;
	}

	if (nt == XmTAB_GROUP && !loop)
	{
	    return widget;
	}
	else
	{
	    widget = XtParent(widget);
	}

	if (XtIsShell(widget))
	{
	    break;
	}
    }
    return widget;
}


extern Boolean
XmProcessTraversal(Widget widget, XmTraversalDirection direction)
{
	Widget shell = _XmFindTopMostShell(widget);
	Widget ve;

	DEBUGOUT(_LtDebug(__FILE__, widget, "XmProcessTraversal\n"));

	ve = _LtFindVendorExt(shell);

	if (!ve) {
		return False;
	}

	if (VSEP_FocusPolicy(ve) != XmEXPLICIT) {
		return False;
	}

	return _XmMgrTraversal(widget, direction);
}


extern Widget
XmGetFocusWidget(Widget widget)
{
    XmFocusData fd = _XmGetFocusData(widget);
    Widget w;

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmGetFocusWidget\n"));

    if (!fd)
    {
	return NULL;
    }

    if (fd->focus_policy == XmEXPLICIT)
    {
	w = fd->focus_item;
    }
    else
    {
	w = fd->pointer_item;
    }

    if (w == NULL)
    {
	return NULL;
    }

    if (XmIsManager(w) && MGR_HighlightedWidget(w))
    {
	return MGR_HighlightedWidget(w);
    }

    return w;
}


extern Boolean
XmIsTraversable(Widget widget)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "XmIsTraversable\n"));

    return _XmIsTraversable(widget, False);
}


extern XmVisibility
XmGetVisibility(Widget widget)
{
    XRectangle rect;

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmGetVisibility\n"));

    if (!widget)
    {
	return XmVISIBILITY_FULLY_OBSCURED;
    }

    if (!_XmCreateVisibilityRect(widget, &rect))
    {
	return XmVISIBILITY_FULLY_OBSCURED;
    }

    if (XtWidth(widget) != rect.width || XtHeight(widget) != rect.height)
    {
	return XmVISIBILITY_PARTIALLY_OBSCURED;
    }

    return XmVISIBILITY_UNOBSCURED;
}
