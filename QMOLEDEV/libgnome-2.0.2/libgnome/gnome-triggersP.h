/*
 * Copyright (C) 1997, 1998 Elliot Lee
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

#ifndef GNOME_TRIGGERSP_H
#define GNOME_TRIGGERSP_H 1

/* Yes, this mechanism is lame, that's why it's hidden :) */
typedef struct _GnomeTriggerList GnomeTriggerList;

struct _GnomeTriggerList {
  char *nodename;
  GnomeTriggerList **subtrees;
  GnomeTrigger **actions;
  gint numsubtrees;
  gint numactions;
};

#endif
