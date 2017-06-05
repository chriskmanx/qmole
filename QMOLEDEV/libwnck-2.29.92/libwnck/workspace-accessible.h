/* vim: set sw=2 et: */
/*
 * Copyright 2002 Sun Microsystems Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __WNCK_WORKSPACE_ACCESSIBLE_H__
#define __WNCK_WORKSPACE_ACCESSIBLE_H__

#include <gtk/gtk.h>
#include <atk/atk.h>
#include "workspace.h"
#include "pager-accessible.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define WNCK_WORKSPACE_TYPE_ACCESSIBLE                     (wnck_workspace_accessible_get_type ())
#define WNCK_WORKSPACE_ACCESSIBLE(obj)                     (G_TYPE_CHECK_INSTANCE_CAST ((obj), WNCK_WORKSPACE_TYPE_ACCESSIBLE, WnckWorkspaceAccessible))
#define WNCK_WORKSPACE_ACCESSIBLE_CLASS(klass)             (G_TYPE_CHECK_CLASS_CAST ((klass), WNCK_WORKSPACE_TYPE_ACCESSIBLE, WnckWorkspaceAccessibleClass))
#define WNCK_IS_WORKSPACE_ACCESSIBLE(obj)                  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), WNCK_WORKSPACE_TYPE_ACCESSIBLE))
#define WNCK_IS_WORKSPACE_ACCESSIBLE_CLASS(klass)          (G_TYPE_CHECK_CLASS_TYPE ((klass), WnckWorkspaceAccessible))
#define WNCK_WORKSPACE_ACCESSIBLE_GET_CLASS(obj)           (G_TYPE_INSTANCE_GET_CLASS ((obj), WNCK_WORKSPACE_TYPE_ACCESSIBLE, WnckWorkspaceAccessibleClass)) 

typedef struct _WnckWorkspaceAccessible WnckWorkspaceAccessible;
typedef struct _WnckWorkspaceAccessibleClass WnckWorkspaceAccessibleClass;

struct _WnckWorkspaceAccessible
{
  AtkGObjectAccessible parent;

  int index;
};

struct _WnckWorkspaceAccessibleClass 
{
  AtkGObjectAccessibleClass parent_class;
};

GType wnck_workspace_accessible_get_type (void) G_GNUC_CONST;

AtkObject* wnck_workspace_accessible_new (GObject *obj); 

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __WNCK_WORKSPACE_ACCESSIBLE_H__ */
