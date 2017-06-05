/* GAIL - The GNOME Accessibility Implementation Library
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

#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>
#include "gailcanvasitem.h"
#include "gailcanvastext.h"
#include <libgail-util/gail-util.h>

struct _GailCanvasText
{
  GailCanvasItem parent;
  GailTextUtil *textutil;
};

static void           gail_canvas_text_class_init          (GailCanvasTextClass *klass);
static void           gail_canvas_text_text_interface_init (AtkTextIface        *iface);
static gchar*         gail_canvas_text_get_text            (AtkText             *text,
                                                            gint                start_offset,
                                                            gint                end_offset);
static gchar*         gail_canvas_text_get_text_after_offset 
                                                           (AtkText             *text,
                                                            gint                offset,
                                                            AtkTextBoundary     boundary_type,
                                                            gint                *start_offset,
                                                            gint                *end_offset);
static gchar*         gail_canvas_text_get_text_at_offset  (AtkText             *text,
                                                            gint                offset,
                                                            AtkTextBoundary     boundary_type,
                                                            gint                *start_offset,
                                                            gint                *end_offset);
static gchar*         gail_canvas_text_get_text_before_offset 
                                                           (AtkText             *text,
                                                            gint                offset,
                                                            AtkTextBoundary     boundary_type,
                                                            gint                *start_offset,
                                                            gint                *end_offset);
static gunichar       gail_canvas_text_get_character_at_offset 
                                                            (AtkText            *text,
                                                             gint               offset);
static gint           gail_canvas_text_get_character_count  (AtkText            *text);
static gint           gail_canvas_text_get_caret_offset     (AtkText            *text);
static gboolean       gail_canvas_text_set_caret_offset     (AtkText            *text,
                                                             gint               offset);
static gint           gail_canvas_text_get_offset_at_point  (AtkText            *text,
                                                             gint               x,
                                                             gint               y,
				                             AtkCoordType       coords);
static void           gail_canvas_text_get_character_extents (AtkText           *text,
                                                              gint              offset,
                                                              gint              *x,
                                                              gint              *y,
                                                              gint              *width,
                                                              gint              *height,
                                                              AtkCoordType      coords);
static AtkAttributeSet* 
                      gail_canvas_text_get_run_attributes    (AtkText           *text,
                                                              gint              offset,
                                                              gint              *start_offset,
                                                              gint              *end_offset);
static AtkAttributeSet* 
                      gail_canvas_text_get_default_attributes (AtkText          *text);
static gint           gail_canvas_text_get_n_selections      (AtkText           *text);
static gchar*         gail_canvas_text_get_selection         (AtkText           *text,
                                                              gint              selection_num,
                                                              gint              *start_pos,
                                                              gint              *end_pos);
static gboolean       gail_canvas_text_add_selection         (AtkText           *text,
                                                              gint              start_pos,
                                                              gint              end_pos);
static gboolean       gail_canvas_text_remove_selection      (AtkText           *text,
                                                              gint              selection_num);
static gboolean       gail_canvas_text_set_selection         (AtkText           *text,
                                                              gint              selection_num,
                                                              gint              start_pos,
                                                              gint              end_pos);
static gchar*         get_text_near_offset                   (AtkText           *text,
                                                              GailOffsetType    function,
                                                              AtkTextBoundary   boundary_type,
                                                              gint              offset,
                                                              gint              *start_offset,
                                                              gint              *end_offset);

G_DEFINE_TYPE_WITH_CODE(GailCanvasText, 
			gail_canvas_text, 
			GAIL_TYPE_CANVAS_ITEM, 
			G_IMPLEMENT_INTERFACE (ATK_TYPE_TEXT,
					       gail_canvas_text_text_interface_init);)

static void
gail_canvas_text_init (GailCanvasText *foo)
{
  ;
}

AtkObject*
gail_canvas_text_new (GObject *obj)
{
  gpointer object;
  AtkObject *atk_object;
  GailCanvasText *gail_text;

  g_return_val_if_fail (GNOME_IS_CANVAS_ITEM (obj), NULL);
  object = g_object_new (GAIL_TYPE_CANVAS_TEXT, NULL);
  atk_object = ATK_OBJECT (object);
  gail_text = GAIL_CANVAS_TEXT (object);

  atk_object_initialize (atk_object, obj);
  gail_text->textutil = gail_text_util_new ();

  if (GNOME_IS_CANVAS_RICH_TEXT (obj))
    {
      gail_text_util_buffer_setup (gail_text->textutil,
				     gnome_canvas_rich_text_get_buffer (GNOME_CANVAS_RICH_TEXT (obj)));
    }
  else if (GNOME_IS_CANVAS_TEXT (obj))
    {
      gail_text_util_text_setup (gail_text->textutil,
				   GNOME_CANVAS_TEXT (obj)->text);
    }

  atk_object->role =  ATK_ROLE_TEXT;
  return atk_object;
}

static void
gail_canvas_text_class_init (GailCanvasTextClass *klass)
{
}

static void
gail_canvas_text_text_interface_init (AtkTextIface *iface)
{
  g_return_if_fail (iface != NULL);

  iface->get_text = gail_canvas_text_get_text;
  iface->get_text_after_offset = gail_canvas_text_get_text_after_offset;
  iface->get_text_at_offset = gail_canvas_text_get_text_at_offset;
  iface->get_text_before_offset = gail_canvas_text_get_text_before_offset;
  iface->get_character_at_offset = gail_canvas_text_get_character_at_offset;
  iface->get_character_count = gail_canvas_text_get_character_count;
  iface->get_caret_offset = gail_canvas_text_get_caret_offset;
  iface->set_caret_offset = gail_canvas_text_set_caret_offset;
  iface->get_offset_at_point = gail_canvas_text_get_offset_at_point;
  iface->get_character_extents = gail_canvas_text_get_character_extents;
  iface->get_n_selections = gail_canvas_text_get_n_selections;
  iface->get_selection = gail_canvas_text_get_selection;
  iface->add_selection = gail_canvas_text_add_selection;
  iface->remove_selection = gail_canvas_text_remove_selection;
  iface->set_selection = gail_canvas_text_set_selection;
  iface->get_run_attributes = gail_canvas_text_get_run_attributes;
  iface->get_default_attributes = gail_canvas_text_get_default_attributes;
}

static gchar*
gail_canvas_text_get_text (AtkText *text,
                           gint    start_offset,
                           gint    end_offset)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextIter start, end;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), NULL);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, NULL);

  buffer = gail_text->textutil->buffer;
  gtk_text_buffer_get_iter_at_offset (buffer, &start, start_offset);
  gtk_text_buffer_get_iter_at_offset (buffer, &end, end_offset);

  return gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
}

static gchar*
gail_canvas_text_get_text_after_offset (AtkText         *text,
                                        gint            offset,
                                        AtkTextBoundary boundary_type,
                                        gint            *start_offset,
                                        gint            *end_offset)
{
  return get_text_near_offset (text, GAIL_AFTER_OFFSET,
                               boundary_type, offset, 
                               start_offset, end_offset);
}

static gchar*
gail_canvas_text_get_text_at_offset (AtkText         *text,
                                     gint            offset,
                                     AtkTextBoundary boundary_type,
                                     gint            *start_offset,
                                     gint            *end_offset)
{
  return get_text_near_offset (text, GAIL_AT_OFFSET,
                               boundary_type, offset, 
                               start_offset, end_offset);
}

static gchar*
gail_canvas_text_get_text_before_offset (AtkText         *text,
                                         gint            offset,
                                         AtkTextBoundary boundary_type,
                                         gint            *start_offset,
                                         gint            *end_offset)
{
  return get_text_near_offset (text, GAIL_BEFORE_OFFSET,
                               boundary_type, offset, 
                               start_offset, end_offset);
}

static gunichar
gail_canvas_text_get_character_at_offset (AtkText *text,
                                          gint    offset)
{
  GailCanvasText *gail_item;
  GtkTextIter start, end;
  GtkTextBuffer *buffer;
  gchar *string;
  gchar *index;
  gunichar unichar;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), '\0');
  gail_item = GAIL_CANVAS_TEXT (text);
  buffer = gail_item->textutil->buffer;
  if (offset >= gtk_text_buffer_get_char_count (buffer))
    return '\0';

  gtk_text_buffer_get_start_iter (buffer, &start);
  gtk_text_buffer_get_end_iter (buffer, &end);
  string = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
  index = g_utf8_offset_to_pointer (string, offset);

  unichar = g_utf8_get_char (index);
  g_free (string);
  return unichar;
}

static gint
gail_canvas_text_get_character_count (AtkText *text)
{
  GtkTextBuffer *buffer;
  GailCanvasText *gail_text;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), 0);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, 0);
  buffer = gail_text->textutil->buffer;
  return gtk_text_buffer_get_char_count (buffer);
}

static gint
gail_canvas_text_get_caret_offset (AtkText *text)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextMark *cursor_mark;
  GtkTextIter cursor_itr;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), 0);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, 0);
  buffer = gail_text->textutil->buffer;
  cursor_mark = gtk_text_buffer_get_insert (buffer);
  gtk_text_buffer_get_iter_at_mark (buffer, &cursor_itr, cursor_mark);
  return gtk_text_iter_get_offset (&cursor_itr);
}

static gboolean
gail_canvas_text_set_caret_offset (AtkText *text,
                                   gint    offset)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextIter pos_itr;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), FALSE);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, FALSE);
  buffer = gail_text->textutil->buffer;
  gtk_text_buffer_get_iter_at_offset (buffer,  &pos_itr, offset);
  gtk_text_buffer_move_mark_by_name (buffer, "insert", &pos_itr);
  return TRUE;
}

static gint
gail_canvas_text_get_offset_at_point (AtkText      *text,
                                      gint         x,
                                      gint         y,
                                      AtkCoordType coords)
{
  return -1;
}

static void
gail_canvas_text_get_character_extents (AtkText      *text,
                                        gint         offset,
                                        gint         *x,
                                        gint         *y,
                                        gint         *width,
                                        gint         *height,
                                        AtkCoordType coords)
{
  return;
}

static AtkAttributeSet*
gail_canvas_text_get_run_attributes (AtkText *text,
                                     gint    offset,
                                     gint    *start_offset,
                                     gint    *end_offset)
{
  GailCanvasText *gail_text;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), NULL);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, NULL);

  return gail_misc_buffer_get_run_attributes (gail_text->textutil->buffer,
                                              offset, start_offset, end_offset);
}

static AtkAttributeSet*
gail_canvas_text_get_default_attributes (AtkText *text)
{
  return NULL;
}

static gint
gail_canvas_text_get_n_selections (AtkText *text)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  gint select_start, select_end;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), -1);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, -1);
  buffer = gail_text->textutil->buffer;
  
  gtk_text_buffer_get_selection_bounds (buffer, &start, &end);
  select_start = gtk_text_iter_get_offset (&start);
  select_end = gtk_text_iter_get_offset (&end);

  if (select_start != select_end)
     return 1;
  else
     return 0;
}

static gchar*
gail_canvas_text_get_selection (AtkText *text,
                                gint    selection_num,
                                gint    *start_pos,
                                gint    *end_pos)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextIter start, end;

 /* Only let the user get the selection if one is set, and if the
  * selection_num is 0.
  */
  if (selection_num != 0)
     return NULL;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), NULL);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, NULL);
  buffer = gail_text->textutil->buffer;

  gtk_text_buffer_get_selection_bounds (buffer, &start, &end);
  *start_pos = gtk_text_iter_get_offset (&start);
  *end_pos = gtk_text_iter_get_offset (&end);

  if (*start_pos != *end_pos)
    return gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
  else
    return NULL;
}

static gboolean
gail_canvas_text_add_selection (AtkText *text,
                                gint    start_pos,
                                gint    end_pos)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextIter pos_itr;
  GtkTextIter start, end;
  gint select_start, select_end;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), FALSE);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, FALSE);
  buffer = gail_text->textutil->buffer;

  gtk_text_buffer_get_selection_bounds (buffer, &start, &end);
  select_start = gtk_text_iter_get_offset (&start);
  select_end = gtk_text_iter_get_offset (&end);

 /* If there is already a selection, then don't allow another to be added,
  * since GtkTextView only supports one selected region.
  */
  if (select_start == select_end)
    {
      gtk_text_buffer_get_iter_at_offset (buffer,  &pos_itr, start_pos);
      gtk_text_buffer_move_mark_by_name (buffer, "insert", &pos_itr);
      gtk_text_buffer_get_iter_at_offset (buffer,  &pos_itr, end_pos);
      gtk_text_buffer_move_mark_by_name (buffer, "selection_bound", &pos_itr);
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
gail_canvas_text_remove_selection (AtkText *text,
                                 gint    selection_num)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextMark *cursor_mark;
  GtkTextIter cursor_itr;
  GtkTextIter start, end;
  gint select_start, select_end;

  if (selection_num != 0)
     return FALSE;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), FALSE);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, FALSE);
  buffer = gail_text->textutil->buffer;

  gtk_text_buffer_get_selection_bounds(buffer, &start, &end);
  select_start = gtk_text_iter_get_offset(&start);
  select_end = gtk_text_iter_get_offset(&end);

  if (select_start != select_end)
    {
     /* Setting the start & end of the selected region to the caret position
      * turns off the selection.
      */
      cursor_mark = gtk_text_buffer_get_insert (buffer);
      gtk_text_buffer_get_iter_at_mark (buffer, &cursor_itr, cursor_mark);
      gtk_text_buffer_move_mark_by_name (buffer, "insert", &cursor_itr);
      gtk_text_buffer_move_mark_by_name (buffer, "selection_bound", &cursor_itr);
      return TRUE;
    }
  else
    return FALSE;
}



static gboolean
gail_canvas_text_set_selection (AtkText *text,
                              gint    selection_num,
                              gint    start_pos,
                              gint    end_pos)
{
  GailCanvasText *gail_text;
  GtkTextBuffer *buffer;
  GtkTextIter pos_itr;
  GtkTextIter start, end;
  gint select_start, select_end;

 /* Only let the user move the selection if one is set, and if the
  * selection_num is 0
  */
  if (selection_num != 0)
     return FALSE;

  g_return_val_if_fail (GAIL_IS_CANVAS_TEXT (text), FALSE);
  gail_text = GAIL_CANVAS_TEXT (text);
  g_return_val_if_fail (gail_text->textutil, FALSE);
  buffer = gail_text->textutil->buffer;

  gtk_text_buffer_get_selection_bounds(buffer, &start, &end);
  select_start = gtk_text_iter_get_offset(&start);
  select_end = gtk_text_iter_get_offset(&end);

  if (select_start != select_end)
    {
      gtk_text_buffer_get_iter_at_offset (buffer,  &pos_itr, start_pos);
      gtk_text_buffer_move_mark_by_name (buffer, "insert", &pos_itr);
      gtk_text_buffer_get_iter_at_offset (buffer,  &pos_itr, end_pos);
      gtk_text_buffer_move_mark_by_name (buffer, "selection_bound", &pos_itr);
      return TRUE;
    }
  else
    return FALSE;
}

static gchar*
get_text_near_offset (AtkText          *text,
                      GailOffsetType   function,
                      AtkTextBoundary  boundary_type,
                      gint             offset,
                      gint             *start_offset,
                      gint             *end_offset)
{
  return gail_text_util_get_text (GAIL_CANVAS_TEXT (text)->textutil, NULL,
                                  function, boundary_type, offset, 
                                  start_offset, end_offset);
}
