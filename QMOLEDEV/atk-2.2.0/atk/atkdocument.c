/* ATK -  Accessibility Toolkit
 * Copyright 2001 Sun Microsystems Inc.
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

#include "atkdocument.h"

enum {
  LOAD_COMPLETE,
  RELOAD,
  LOAD_STOPPED,
  LAST_SIGNAL
};

static void atk_document_base_init (AtkDocumentIface *class);

static guint atk_document_signals[LAST_SIGNAL] = {0};

GType
atk_document_get_type (void)
{
  static GType type = 0;

  if (!type) {
    static const GTypeInfo tinfo =
    {
      sizeof (AtkDocumentIface),
      (GBaseInitFunc) atk_document_base_init,
      (GBaseFinalizeFunc) NULL,

    };

    type = g_type_register_static (G_TYPE_INTERFACE, "AtkDocument", &tinfo, 0);
  }

  return type;
}

static void
atk_document_base_init (AtkDocumentIface *class)
{
  static gboolean initialized = FALSE;
  if (!initialized)
    {
      atk_document_signals[LOAD_COMPLETE] =
        g_signal_new ("load_complete",
                      ATK_TYPE_DOCUMENT,
                      G_SIGNAL_RUN_LAST,
                      0,
                      (GSignalAccumulator) NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
      atk_document_signals[RELOAD] =
        g_signal_new ("reload",
                      ATK_TYPE_DOCUMENT,
                      G_SIGNAL_RUN_LAST,
                      0,
                      (GSignalAccumulator) NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
      atk_document_signals[LOAD_STOPPED] =
        g_signal_new ("load_stopped",
                      ATK_TYPE_DOCUMENT,
                      G_SIGNAL_RUN_LAST,
                      0,
                      (GSignalAccumulator) NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);

      initialized = TRUE;
    }
}

/**
 * atk_document_get_document_type:
 * @document: a #GObject instance that implements AtkDocumentIface
 *
 * Gets a string indicating the document type.
 *
 * Returns: a string indicating the document type
 **/
const gchar*
atk_document_get_document_type (AtkDocument *document)
{
  AtkDocumentIface *iface;

  g_return_val_if_fail (ATK_IS_DOCUMENT (document), NULL);

  iface = ATK_DOCUMENT_GET_IFACE (document);

  if (iface->get_document_type)
    {
      return (iface->get_document_type) (document);
    }
  else
    {
      return NULL;
    }
}

/**
 * atk_document_get_document:
 * @document: a #GObject instance that implements AtkDocumentIface
 *
 * Gets a %gpointer that points to an instance of the DOM.  It is
 * up to the caller to check atk_document_get_type to determine
 * how to cast this pointer.
 *
 * Returns: (transfer none): a %gpointer that points to an instance of the DOM.
 **/
gpointer 
atk_document_get_document (AtkDocument *document)
{
  AtkDocumentIface *iface;

  g_return_val_if_fail (ATK_IS_DOCUMENT (document), NULL);

  iface = ATK_DOCUMENT_GET_IFACE (document);

  if (iface->get_document)
    {
      return (iface->get_document) (document);
    }
  else
    {
      return NULL;
    }
}

/**
 * atk_document_get_locale:
 * @document: a #GObject instance that implements AtkDocumentIface
 *
 * Gets a UTF-8 string indicating the POSIX-style LC_MESSAGES locale
 *          of the content of this document instance.  Individual
 *          text substrings or images within this document may have
 *          a different locale, see atk_text_get_attributes and
 *          atk_image_get_image_locale.
 *
 * Returns: a UTF-8 string indicating the POSIX-style LC_MESSAGES
 *          locale of the document content as a whole, or NULL if
 *          the document content does not specify a locale.
 **/
const gchar *
atk_document_get_locale (AtkDocument *document)
{
  AtkDocumentIface *iface;

  g_return_val_if_fail (ATK_IS_DOCUMENT (document), NULL);

  iface = ATK_DOCUMENT_GET_IFACE (document);

  if (iface->get_document_locale)
    {
      return (iface->get_document_locale) (document);
    }
  else
    {
      return NULL;
    }
}


/**
 * atk_document_get_attributes:
 * @document: a #GObject instance that implements AtkDocumentIface
 *
 * Gets an AtkAttributeSet which describes document-wide
 *          attributes as name-value pairs.
 *
 * Since: 1.12
 *
 * Returns: (transfer none): An AtkAttributeSet containing the explicitly
 *          set name-value-pair attributes associated with this document
 *          as a whole.
 **/
AtkAttributeSet *
atk_document_get_attributes (AtkDocument *document)
{
  AtkDocumentIface *iface;

  g_return_val_if_fail (ATK_IS_DOCUMENT (document), NULL);

  iface = ATK_DOCUMENT_GET_IFACE (document);

  if (iface->get_document_attributes)
    {
      return (iface->get_document_attributes) (document);
    }
  else
    {
      return NULL;
    }
}

/**
 * atk_document_get_attribute_value:
 * @document: a #GObject instance that implements AtkDocumentIface
 * @attribute_name: a character string representing the name of the attribute
 *            whose value is being queried.
 *
 * Since: 1.12
 *
 * Returns: a string value associated with the named attribute for this
 *    document, or NULL if a value for #attribute_name has not been specified
 *    for this document.
 */
const gchar *
atk_document_get_attribute_value (AtkDocument *document, 
				  const gchar *attribute_name)
{
  AtkDocumentIface *iface;

  g_return_val_if_fail (ATK_IS_DOCUMENT (document), NULL);

  iface = ATK_DOCUMENT_GET_IFACE (document);

  if (iface->get_document_attribute_value)
    {
      return (iface->get_document_attribute_value) (document, attribute_name);
    }
  else
    {
      return NULL;
    }
}

/**
 * atk_document_set_attribute_value:
 * @document: a #GObject instance that implements AtkDocumentIface
 * @attribute_name: a character string representing the name of the attribute
 *            whose value is being set.
 * @attribute_value: a string value to be associated with #attribute_name.
 *
 * Since: 1.12
 *
 * Returns: TRUE if #value is successfully associated with #attribute_name
 *          for this document, FALSE otherwise (e.g. if the document does not
 *          allow the attribute to be modified).
 */
gboolean
atk_document_set_attribute_value (AtkDocument *document, 
				  const gchar *attribute_name,
				  const gchar *attribute_value)
{
  AtkDocumentIface *iface;

  g_return_val_if_fail (ATK_IS_DOCUMENT (document), FALSE);

  iface = ATK_DOCUMENT_GET_IFACE (document);

  if (iface->set_document_attribute)
    {
      return (iface->set_document_attribute) (document, attribute_name, attribute_value);
    }
  else
    {
      return FALSE;
    }
}
