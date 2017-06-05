/*
 * peas-extension-set.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Steve Frécinaux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __PEAS_EXTENSION_SET_H__
#define __PEAS_EXTENSION_SET_H__

#include <glib-object.h>
#include "peas-engine.h"

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_TYPE_EXTENSION_SET            (peas_extension_set_get_type())
#define PEAS_EXTENSION_SET(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_TYPE_EXTENSION_SET, PeasExtensionSet))
#define PEAS_EXTENSION_SET_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_TYPE_EXTENSION_SET, PeasExtensionSetClass))
#define PEAS_IS_EXTENSION_SET(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_TYPE_EXTENSION_SET))
#define PEAS_IS_EXTENSION_SET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_TYPE_EXTENSION_SET))
#define PEAS_EXTENSION_SET_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_TYPE_EXTENSION_SET, PeasExtensionSetClass))

typedef struct _PeasExtensionSet         PeasExtensionSet;
typedef struct _PeasExtensionSetClass    PeasExtensionSetClass;
typedef struct _PeasExtensionSetPrivate  PeasExtensionSetPrivate;

/**
 * PeasExtensionSet:
 *
 * The #PeasExtensionSet structure contains only private data and should only
 * be accessed using the provided API.
 */
struct _PeasExtensionSet {
  GObject parent;

  PeasExtensionSetPrivate *priv;
};

/**
 * PeasExtensionSetClass:
 * @parent_class: The parent class.
 * @call: The VFunc for peas_extension_set_call().
 * @extension_added: Signal class handler for the
 *                   #PeasExtensionSet::extension-added signal.
 * @extension_removed: Signal class handler for the
 *                   #PeasExtensionSet::extension-removed signal.
 *
 * The class structure for #PeasExtensionSet.
 */
struct _PeasExtensionSetClass {
  GObjectClass parent_class;

  /* Virtual public methods */
#ifndef PEAS_DISABLE_DEPRECATED
  gboolean   (*call)                      (PeasExtensionSet *set,
                                           const gchar      *method_name,
                                           GIArgument       *args);
#else
  /*< private >*/
  gpointer __DEPRECATED_call;
#endif

  /*< public >*/
  /* Signals */
  void       (*extension_added)           (PeasExtensionSet *set,
                                           PeasPluginInfo   *info,
                                           PeasExtension    *exten);
  void       (*extension_removed)         (PeasExtensionSet *set,
                                           PeasPluginInfo   *info,
                                           PeasExtension    *exten);

  /*< private >*/
  gpointer padding[8];
};

/**
 * PeasExtensionSetForeachFunc:
 * @set: A #PeasExtensionSet.
 * @info: A #PeasPluginInfo.
 * @exten: A #PeasExtension.
 * @data: Optional data passed to the function.
 *
 * This function is passed to peas_extension_set_foreach() and
 * will be called for each extension in @set.
 *
 * Since: 1.2
 */
typedef void (*PeasExtensionSetForeachFunc) (PeasExtensionSet *set,
                                             PeasPluginInfo   *info,
                                             PeasExtension    *exten,
                                             gpointer          data);

/*
 * Public methods
 */
GType              peas_extension_set_get_type    (void)  G_GNUC_CONST;

#if !defined(PEAS_DISABLE_DEPRECATED) && !defined(__GI_SCANNER__)
gboolean           peas_extension_set_call        (PeasExtensionSet *set,
                                                   const gchar      *method_name,
                                                   ...);
gboolean           peas_extension_set_call_valist (PeasExtensionSet *set,
                                                   const gchar      *method_name,
                                                   va_list           va_args);
gboolean           peas_extension_set_callv       (PeasExtensionSet *set,
                                                   const gchar      *method_name,
                                                   GIArgument       *args);
#endif

void               peas_extension_set_foreach     (PeasExtensionSet *set,
                                                   PeasExtensionSetForeachFunc func,
                                                   gpointer          data);

PeasExtension     *peas_extension_set_get_extension (PeasExtensionSet *set,
                                                     PeasPluginInfo   *info);

PeasExtensionSet  *peas_extension_set_newv        (PeasEngine       *engine,
                                                   GType             exten_type,
                                                   guint             n_parameters,
                                                   GParameter       *parameters);
PeasExtensionSet  *peas_extension_set_new_valist  (PeasEngine       *engine,
                                                   GType             exten_type,
                                                   const gchar      *first_property,
                                                   va_list           var_args);
PeasExtensionSet  *peas_extension_set_new         (PeasEngine       *engine,
                                                   GType             exten_type,
                                                   const gchar      *first_property,
                                                   ...);

G_END_DECLS

#endif /* __PEAS_EXTENSION_SET_H__ */
