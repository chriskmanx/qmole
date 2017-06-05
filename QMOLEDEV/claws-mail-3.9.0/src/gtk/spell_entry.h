/*
 * @file libsexy/sexy-icon-entry.h Entry widget
 *
 * @Copyright (C) 2004-2006 Christian Hammond.
 * Some of this code is from gtkspell, Copyright (C) 2002 Evan Martin.
 * Adapted for Claws Mail (c) 2009-2012 Pawel Pekala and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __SPELL_ENTRY_H__
#define __SPELL_ENTRY_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#ifdef USE_ENCHANT

G_BEGIN_DECLS

typedef struct _ClawsSpellEntry ClawsSpellEntry;
typedef struct _ClawsSpellEntryClass ClawsSpellEntryClass;
typedef struct _ClawsSpellEntryPriv ClawsSpellEntryPriv;

#include <gtk/gtk.h>
#include "gtkaspell.h"

#define CLAWS_TYPE_SPELL_ENTRY            (claws_spell_entry_get_type())
#define CLAWS_SPELL_ENTRY(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), CLAWS_TYPE_SPELL_ENTRY, ClawsSpellEntry))
#define CLAWS_SPELL_ENTRY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), CLAWS_TYPE_SPELL_ENTRY, ClawsSpellEntryClass))
#define CLAWS_IS_SPELL_ENTRY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), CLAWS_TYPE_SPELL_ENTRY))

struct _ClawsSpellEntry
{
	GtkEntry parent_object;
	
	ClawsSpellEntryPriv *priv;
	GtkAspell *gtkaspell;
       
	void (*gtk_reserved1)(void);
	void (*gtk_reserved2)(void);
	void (*gtk_reserved3)(void);
	void (*gtk_reserved4)(void);
};

struct _ClawsSpellEntryClass
{
	GtkEntryClass parent_class;

	void (*gtk_reserved1)(void);
	void (*gtk_reserved2)(void);
	void (*gtk_reserved3)(void);
	void (*gtk_reserved4)(void);
};

GType		claws_spell_entry_get_type		(void);
GtkWidget *	claws_spell_entry_new			(void);
void		claws_spell_entry_set_gtkaspell		(ClawsSpellEntry *entry,
							 GtkAspell *gtkaspell);
void		claws_spell_entry_recheck_all		(ClawsSpellEntry *entry);
void 		claws_spell_entry_check_all		(ClawsSpellEntry *entry);
void 		claws_spell_entry_context_set		(ClawsSpellEntry *entry);
void 		claws_spell_entry_check_backwards	(ClawsSpellEntry *entry);
void 		claws_spell_entry_check_forwards_go	(ClawsSpellEntry *entry);


G_END_DECLS

#endif  /* USE_ENCHANT */
#endif	/* __SPELL_ENTRY_H__ */
