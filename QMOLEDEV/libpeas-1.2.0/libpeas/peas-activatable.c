/*
 * peas-activatable.c
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Steve Frécinaux
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "peas-activatable.h"

/**
 * SECTION:peas-activatable
 * @short_description: Interface for activatable plugins.
 * @see_also: #PeasExtensionSet
 *
 * #PeasActivatable is an interface which should be implemented by plugins
 * that should be activated on an object of a certain type (depending on the
 * application). For instance, in a typical windowed application,
 * #PeasActivatable plugin instances could be bound to individual toplevel
 * windows.
 *
 * It is typical to use #PeasActivatable along with #PeasExtensionSet in order
 * to activate and deactivate extensions automatically when plugins are loaded
 * or unloaded.
 *
 * You can also use the code of this interface as a base for your own
 * extension types, as illustrated by gedit's %GeditWindowActivatable and
 * %GeditDocumentActivatable interfaces.
 **/

G_DEFINE_INTERFACE(PeasActivatable, peas_activatable, G_TYPE_OBJECT)

void
peas_activatable_default_init (PeasActivatableInterface *iface)
{
  static gboolean initialized = FALSE;

  if (!initialized)
    {
      /**
       * PeasActivatable:object:
       *
       * The object property contains the targetted object for this
       * #PeasActivatable instance, for example a toplevel window in a typical
       * windowed application. It is set at construction time and won't change.
       */
      g_object_interface_install_property (iface,
                                           g_param_spec_object ("object",
                                                                "Object",
                                                                "Object",
                                                                G_TYPE_OBJECT,
                                                                G_PARAM_READWRITE |
                                                                G_PARAM_CONSTRUCT_ONLY |
                                                                G_PARAM_STATIC_STRINGS));

      initialized = TRUE;
    }
}

/**
 * peas_activatable_activate:
 * @activatable: A #PeasActivatable.
 *
 * Activates the extension on the targetted object.
 *
 * On activation, the extension should hook itself to the object
 * where it makes sense.
 */
void
peas_activatable_activate (PeasActivatable *activatable)
{
  PeasActivatableInterface *iface;

  g_return_if_fail (PEAS_IS_ACTIVATABLE (activatable));

  iface = PEAS_ACTIVATABLE_GET_IFACE (activatable);
  if (iface->activate != NULL)
    iface->activate (activatable);
}

/**
 * peas_activatable_deactivate:
 * @activatable: A #PeasActivatable.
 *
 * Deactivates the extension on the targetted object.
 *
 * On deactivation, an extension should remove itself from all the hooks it
 * used and should perform any cleanup required, so it can be unreffed safely
 * and without any more effect on the host application.
 */
void
peas_activatable_deactivate (PeasActivatable *activatable)
{
  PeasActivatableInterface *iface;

  g_return_if_fail (PEAS_IS_ACTIVATABLE (activatable));

  iface = PEAS_ACTIVATABLE_GET_IFACE (activatable);
  if (iface->deactivate != NULL)
    iface->deactivate (activatable);
}

/**
 * peas_activatable_update_state:
 * @activatable: A #PeasActivatable.
 *
 * Triggers an update of the extension internal state to take into account
 * state changes in the targetted object, due to some event or user action.
 */
void
peas_activatable_update_state (PeasActivatable *activatable)
{
  PeasActivatableInterface *iface;

  g_return_if_fail (PEAS_IS_ACTIVATABLE (activatable));

  iface = PEAS_ACTIVATABLE_GET_IFACE (activatable);
  if (iface->update_state != NULL)
    iface->update_state (activatable);
}

