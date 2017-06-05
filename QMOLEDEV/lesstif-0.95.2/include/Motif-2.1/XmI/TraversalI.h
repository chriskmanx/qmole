/**
 *
 * $Id: TraversalI.h,v 1.1 2004/08/28 19:23:36 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1997-2000 LessTif Development Team
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

#ifndef _XMI_TRAVERSALI_H
#define _XMI_TRAVERSALI_H

#include <Xm/XmP.h>

#define XmFOCUS_RESET   	1
#define XmFOCUS_IGNORE		2

typedef enum {
    XmNO_RELATION,
    XmUNCLE,
    XmNEPHEW,
    XmCOUSIN,
    XmME
} XmRelations;

enum {
    XmTAB_TREE_NODE,
    XmTAB_NODE,
    XmCONTROL_TREE_NODE,
    XmCONTROL_NODE
};
 
/*
 * because we use a dynamically allocated array for the tree nodes (which
 * can shrink and grow as things change), we need to use array indices while
 * building the tree.  When the tree is finished, we switch over to pointers
 * for speed.
 */
typedef union {
    struct _XmTravTreeNodeRec	*link;
    int				offset;
} XmTravTreeNodeLink;

/*
 * the tree node structure.  At least one node per widget
 */
typedef struct _XmTravTreeNodeRec {
    unsigned char type;
    XmNavigationType nav_type;
    XmTravTreeNodeLink tab_parent;
    Widget widget;
    XRectangle rect;
    struct _XmTravTreeNodeRec *next;
    struct _XmTravTreeNodeRec *prev;
    struct _XmTravTreeNodeRec *up;
    struct _XmTravTreeNodeRec *down;
} XmTravTreeNodeRec, *XmTravTreeNode;

/*
 * structure for managing the tree
 */
typedef struct _XmTravTreeRec {
    XmTravTreeNode head;
    Widget shell;
    XmTravTreeNode current;
    unsigned short num_entries;
    unsigned short num_alloc;
    unsigned short next_alloc;
    unsigned short num_excls;
    unsigned short num_tab_alloc;
    unsigned short num_tab_entries;
    Widget *excl_tabs;
} XmTravTreeRec, *XmTravTree;

/*
 * ye old focus data for VendorSEP.
 */
typedef struct _XmFocusDataRec {
    Widget active_tab_group;
    Widget focus_item;
    Widget old_focus_item;
    Widget pointer_item;
    Widget old_pointer_item;
    int    dead[4];		/* these are apparently unused. */
    Boolean flush;
    XCrossingEvent last_enter_leave;
    XmRelations focal_point;
    unsigned char focus_policy;
    XmTravTreeRec tree;
    Widget first_focus;
} XmFocusDataRec;

extern void _XmFreeTravGraph(XmTravTree tree);
extern Boolean _XmIsTraversable(Widget w, Boolean vischeck);
extern Boolean _XmIsViewable(Widget w);
extern XmNavigationType _XmGetNavigationType(Widget w);

#endif /* _XMI_TRAVERSALI_H */
