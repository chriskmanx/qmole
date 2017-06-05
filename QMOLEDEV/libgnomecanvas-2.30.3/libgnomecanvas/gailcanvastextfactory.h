/* GAIL - The GNOME Accessibility Implementation Library
 * Copyright 2001 Sun Microsystems Inc.
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
 * Boston, MA 02111-1307, USA.
 */

#ifndef __GAIL_CANVAS_TEXT_FACTORY_H__
#define __GAIL_CANVAS_TEXT_FACTORY_H__

#include <atk/atkobjectfactory.h>

G_BEGIN_DECLS

#define GAIL_TYPE_CANVAS_TEXT_FACTORY                 (gail_canvas_text_factory_get_type ())
#define GAIL_CANVAS_TEXT_FACTORY(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAIL_TYPE_CANVAS_TEXT_FACTORY, GailCanvasTextFactory))
#define GAIL_CANVAS_TEXT_FACTORY_CLASS(klass)         (G_TYPE_CHECK_CLASS_CAST ((klass), GAIL_TYPE_CANVAS_TEXT_FACTORY, GailCanvasTextFactoryClass))
#define GAIL_IS_CANVAS_TEXT_FACTORY(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAIL_TYPE_CANVAS_TEXT_FACTORY))
#define GAIL_IS_CANVAS_TEXT_FACTORY_CLASS(klass)      (G_TYPE_CHECK_CLASS_TYPE ((klass), GAIL_TYPE_CANVAS_TEXT_FACTORY))
#define GAIL_CANVAS_TEXT_FACTORY_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj), GAIL_TYPE_CANVAS_TEXT_FACTORY, GailCanvasTextFactoryClass))


typedef struct _GailCanvasTextFactory                GailCanvasTextFactory;
typedef struct _GailCanvasTextFactoryClass           GailCanvasTextFactoryClass;

struct _GailCanvasTextFactory
{
  AtkObjectFactory parent;
};

struct _GailCanvasTextFactoryClass
{
  AtkObjectFactoryClass parent_class;
};

GType gail_canvas_text_factory_get_type(void);

G_END_DECLS

#endif /* __GAIL_CANVAS_TEXT_FACTORY_H__ */

