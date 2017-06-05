/* -*- Mode: C; c-basic-offset: 4 -*-
 * libglade - a library for building interfaces from XML files at runtime
 * Copyright (C) 1998-2002  James Henstridge <james@daa.com.au>
 *
 * glade-private.h: private datastructures for the GladeXML object.
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
 * License along with this library; if not, write to the 
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */
#ifndef GLADE_PRIVATE_H
#define GLADE_PRIVATE_H
#include <stdio.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <glade/glade-xml.h>
#include <glade/glade-parser.h>

struct _GladeXMLPrivate {
    GladeInterface *tree; /* the tree for this GladeXML */

    GtkTooltips *tooltips; /* if not NULL, holds all tooltip info */

    /* hash tables of widgets.  The keys are stored as widget data,
     * and get freed with those widgets. */
    GHashTable *name_hash;
	
    /* hash table of signals.  The Data is a GList of GladeSignalData
     * structures which get freed when the GladeXML object is
     * destroyed */
    GHashTable *signals;

    /* the current toplevel being built */
    GtkWindow *toplevel;

    /* the accel group to add accelerators to (not mnemonics) */
    GtkAccelGroup *accel_group;

    /* these hold the focus and default widgets for a window until they
     * get packed into the window -- we can't call gtk_widget_grab_focus
     * or grab_default until this occurs */
    GtkWidget *focus_widget;
    GtkWidget *default_widget;

    /* list of GtkWidget properties waiting to be set.  (they couldn't
     * be set earlier because the value widget hadn't been created
     * yet). */
    GList *deferred_props;
};

typedef struct _GladeSignalData GladeSignalData;
struct _GladeSignalData {
    GObject *signal_object;
    char *signal_name;
    char *connect_object; /* or NULL if there is none */
    gboolean signal_after;
};

typedef struct _GladeDeferredProperty GladeDeferredProperty;
struct _GladeDeferredProperty {
    const gchar *target_name;

    enum { DEFERRED_PROP, DEFERRED_REL } type;
    union {
	struct {
	    GObject *object;
	    const gchar *prop_name;
	} prop;
	struct {
	    AtkRelationSet *relation_set;
	    AtkRelationType relation_type;
	} rel;
    } d;
};

typedef enum {
    GLADE_DEBUG_PARSER = 1 << 0,
    GLADE_DEBUG_BUILD  = 1 << 1
} GladeDebugFlag;

extern guint _glade_debug_flags;
#ifdef DEBUG
#  define GLADE_NOTE(type, action)  G_STMT_START { \
    if (_glade_debug_flags & GLADE_DEBUG_##type) \
        { action; };                } G_STMT_END
#else
#  define GLADE_NOTE(type, action)
#endif

#endif

