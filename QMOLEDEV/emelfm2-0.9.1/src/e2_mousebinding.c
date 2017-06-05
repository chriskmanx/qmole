/* $Id: e2_mousebinding.c 3054 2014-02-14 22:16:24Z tpgww $

Copyright (C) 2008-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/e2_mousebinding.c
@brief functions for handling pointer (mouse) button bindings
*/

/**
\page bindings mouse bindings

The primary data for button-bindings are stored in a treestore established
at session-start. Its hierarchy currently provides for 3 levels of "category"
(more levels can be defined at compile-time). One or more widgets can be bound
to any category. One or more categories can be bound to any widget. Any level
can be bound to the widget. Any button can be assigned to any level. Buttons
assigned to a category apply to all widgets in that category, and to all widgets
in any descendant category, unless and until pre-empted by another assignment of
the same button in a descendant category.

After the last level of categories in any tree branch, the next level of the
hierarchy has string-form button descriptions and related data.

Categories can be added to the buttons treestore at any time and in any order,
from strings like "1.2.3", with or without any buttons. A category must, and its
contents may, be added to the treestore before the category is bound to any
widget. When a category is first bound to any widget, the buttons in the category
(if any) are prepared for use - see below. Buttons can be added to a category
later, provided they are specifically converted to runtime format.

Treestore data are converted to a tree (btnbindings) for runtime access. Each
member of btnbindings corresponds to a binding category. Among other things,
the member has a list of bound widgets, and a list of button-data structs. In
the latter, each member has specific button data and the bound action/command.
A category can be "registered" (added to runtime tree) at any time, and any
prior rt data for the category will then be destroyed.

Category data for "core" widgets are destroyed and rebuilt as appropriate
during a config dialog. As of now, there are no "transient" bindings. They
probably don't need to be rebuilt, anyhow.

Category names must be set at compile time, as the relevant widgets need to know
the names to which to bind. In theory at least, category names could be made
configurable. If so, it would be reasonable to also allow creation and deletion
of categories in a config dialog.

Binding a category to a widget means that a button-event on the widget will
callback to scan the buttons in the bound group and all its ancestor bindings in
turn, and act on the first-found matched binding. Double- and triple-events can
be recorded. The existence of such will cause a short delay in performing "lesser"
events, as we must wait to see if a "greater" happens.

Look for code tagged with #ifdef E2_TRANSIENTBINDINGS, and in particular a
dummy set of bindings and their initialization and cancellation, in
e2_edit_dialog.c

One or more button-presses may be 'aliased' by binding them to the action
'button.fake'. The argument for that action must be a string containing one or
more button-names in a form gtk together with e2 can parse, like <Ctrl><Alt>1+2,
or just a button-number (up to 32). If more than one button is to be faked, they
must be separated by a ' ' char.
*/
/**
\page bindings_ui button bindings interface

ToDo - describe how this works
*/

#include "e2_mousebinding.h"

#ifdef E2_MOUSECUSTOM

#define BUTTON_BINDINGS_KEY "__bound-buttoncats__"

#include <string.h>

typedef enum
{
	EVENT_ANY = 0,
	EVENT_NONE,
	EVENT_BEFORE,
	EVENT_AFTER,
	EVENT_SAME,
	EVENT_DIFFERENT, //before or after, not same
} E2_BtnChainType;

typedef struct _E2_BtnRuntime
{
	guint bnum;			//gdk button no. 1 ... (i.e. no multi-button mask)
	GdkEventType type;	//gdk enum for single/double/triple press or release
	GdkModifierType state;	//applicable modifier key(s)
	gchar *action;		//name of action or command, maybe ""
	gchar *action_data;	//action data, maybe ""
} E2_BtnRuntime;

typedef struct _E2_BtnChain
{
	GtkWidget *from;	//the widget where the event was initiated
	GdkDevice *device;	//the device which originated the event
	E2_BtnRuntime *binding;	//data for the most-recent button-event
	guint timeout_id;	//id of multi-click wait-timer
#ifdef MCOL_REL
	guint32 relcount;	//counter for managing 1-3 multi-releases
#endif
} E2_BtnChain;

typedef struct _E2_BtnCatRuntime
{
	GQuark name_quark;	//quark for the category-name (which is structured like level1[.level2.level3 ...]
	GNode *node;		//pointer to treenode for which this struct is data
	guint level;		//level in nodes-tree and bindings-tree, effectively the
						//no. of '.'-separated parts of the category name - 1
	GtkTreeRowReference *ref;	//reference to config data model iter that's the root for this category
//#ifdef E2_IDLE_BTNSYNC
//	gboolean synced;	//TRUE when config treestore data have been converted to runtime format
//#endif
	GSList *buttons;	//E2_BtnRuntime's for each button in the category
	GSList *instances;	//list of widgets bound to this binding group NEEDED ?
} E2_BtnCatRuntime;

//tree of E2_BtnbindingRuntime structs, each for a 'category' of bindings
static GNode *btnbindings = NULL;
//#ifdef E2_IDLE_BTNSYNC
//pointer for use with btnbindings list
//static GNode *binding_cur = NULL;
//#endif
//list of E2_BtnChain structs, each for a 'watched' event
static GSList *chainevents = NULL;

extern guint click_interval;

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief cleanup watch-n-wait data @a data
@param data pointer to data struct

@return
*/
static void _e2_mousebinding_clear_chainmember (E2_BtnChain *data)
{
	if (data->timeout_id > 0)
		g_source_remove (data->timeout_id);
	chainevents = g_slist_remove (chainevents, data);
	DEALLOCATE (E2_BtnChain, data);
}
/**
@brief cleanup bound-buttons data in list @a usedbuttons

@param usedbuttons pointer to a bound-buttons list

@return
*/
static void _e2_mousebinding_free_buttonslist (GSList *usedbuttons)
{
	GSList *member;
	for (member = usedbuttons; member != NULL; member = member->next)
	{
		E2_BtnRuntime *brt = member->data;
		if (brt != NULL)
		{
			//make sure this binding is not in use
			if (chainevents != NULL)
			{
				GSList *chained;
				for (chained = chainevents; chained != NULL; chained = chained->next)
				{
					E2_BtnChain *data = (E2_BtnChain *)chained->data;
					if (data->binding == brt)
					{
						if (data->timeout_id != 0)
							g_source_remove (data->timeout_id);
						chained->data = NULL;
						DEALLOCATE (E2_BtnChain, data);
					}
				}
				chainevents = g_slist_remove_all (chainevents, NULL);
			}

			g_free (brt->action);
			g_free (brt->action_data);
			DEALLOCATE (E2_BtnRuntime, brt);
		}
	}
	g_slist_free (usedbuttons);
}
/**
@brief cleanup data for the bound buttons for the category associated with @a rt
@param rt pointer to button binding category data struct

@return
*/
static void _e2_mousebinding_free_buttons_category (E2_BtnCatRuntime *rt)
{
	if (rt->buttons != NULL)
	{
		_e2_mousebinding_free_buttonslist (rt->buttons);
		rt->buttons = NULL;
	}
}
/**
@brief recursively translate a buttonbinding treestore branch into a listed runtme data struct
This is for "core" widgets with a standard E2_BtnWidget value.
Stored data are validated before listing
@param model model for the bindings treestore
@param iter pointer to iter to be used to interrogage @a model
@param btns pointer to initialised list to which the created struct(s) will be appended

@return
*/
static void _e2_mousebinding_sync_one (GtkTreeModel *model, GtkTreeIter *iter,
	GSList **btns)
{
	gchar *cat, *button, *action, *action_data;
	gboolean valid, dclick, tclick;
#ifdef MCOL_REL
	gboolean release;
#endif
	guint buttonnum = 0;
	GdkModifierType state = 0;
	E2_BtnRuntime *b;

	gtk_tree_model_get (model, iter,
		MCOL_CAT, &cat, MCOL_BTN, &button,				//strings
		MCOL_DBL, &dclick, MCOL_TRP, &tclick,			//bools
#ifdef MCOL_REL
		MCOL_REL, &release,
#endif
		MCOL_ACT, &action, MCOL_ARG, &action_data, -1);	//strings

	valid = (*cat == '\0'//for a button row, the category should be empty
			 && *button != '\0'
			 && *action != '\0');	//data may be empty, but not this

	if (valid)
	{
		valid = e2_mousebinding_parse_name (button, &buttonnum, &state, NULL, FALSE);
	}
	if (valid)
	{
		E2_Action *act = e2_action_check (action);
		valid = (act == NULL || !(act->exclude & E2_ACTION_EXCLUDE_POINTER));
	}
	if (valid)
	{
		b = ALLOCATE (E2_BtnRuntime);
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (b, valid = FALSE;)
#else
		if (b == NULL)
			valid = FALSE;
#endif
	}
	else
		b = NULL; //warning prevention

	if (valid)
	{
#ifdef MCOL_REL
		if (release)
		{
			if (dclick)
				b->type = E2_2BUTTON_RELEASE;
			else if (tclick)
				b->type = E2_3BUTTON_RELEASE;
			else
				b->type = GDK_BUTTON_RELEASE;
		}
		else
#endif
		if (dclick)
			b->type = GDK_2BUTTON_PRESS;
		else if (tclick)
			b->type = GDK_3BUTTON_PRESS;
		else
			b->type = GDK_BUTTON_PRESS;

		b->state = state;
		b->bnum = buttonnum;
		b->action = action;
		b->action_data = action_data;

		*btns = g_slist_append (*btns, b);
	}
	else
	{
		g_free (action);
		g_free (action_data);
	}
	g_free (cat);
	g_free (button);
}
/**
@brief translate, from primary treestore to runtime list, the data for a "core"
widget button-binding category
This can be called at any time, including when there are no buttons for the
category in the treestore. Pre-existing data for the category are cleared,
so this cannot be used to _add_ buttons to the category
@param rt pointer to data struct for the button-binding category to be processed

@return if E2_IDLE_BTNSYNC applies, TRUE if the process completed properly
*/
static
#ifdef E2_IDLE_BTNSYNC
gboolean
#else
void
#endif
 _e2_mousebinding_sync_category (E2_BtnCatRuntime *rt)
{
	if (!gtk_tree_row_reference_valid (rt->ref))
	{
		printd (WARN, "internal buttoninding error, row reference not valid anymore");
#ifdef E2_IDLE_BTNSYNC
		return FALSE;
#else
		return;
#endif
	}
	GtkTreePath *path = gtk_tree_row_reference_get_path (rt->ref);
	if (path == NULL)
	{
		printd (WARN, "internal button binding error, path is NULL");
#ifdef E2_IDLE_BTNSYNC
		return FALSE;
#else
		return;
#endif
	}

	printd (DEBUG, "%s e2_mousebinding_sync", g_quark_to_string (rt->name_quark));
	_e2_mousebinding_free_buttons_category (rt);	//get rid of any old btn data for this category

	E2_OptionSet *set = e2_option_get ("mousebuttons");
	GtkTreeIter iter;
	gtk_tree_path_down (path);	//down to the buttons level
	while (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		//convert current button to runtime form
		_e2_mousebinding_sync_one (set->ex.tree.model, &iter, &rt->buttons);
		gtk_tree_path_next (path);
	}
	gtk_tree_path_free (path);
#ifdef E2_IDLE_BTNSYNC
//	rt->synced = TRUE;
	return TRUE;
#else
	return;
#endif
}
/**
@brief find the first if any category whose name-quark matches the one in @a data
@param tree node data
@param data pointer to walker data struct
@return TRUE when a match is found
*/
static gboolean _e2_mousebinding_walk_names (GNode *node, E2_NodeMatch *data)
{
	if (data->qname == ((E2_BtnCatRuntime *) node->data)->name_quark)
	{
		data->match = node;
		return TRUE;
	}
	return FALSE;
}
/**
@brief get a button-binding category treenode for @a name
@param name the name of the binding category to search for, non-NULL and non-empty
Any missing ancestor nodes are created and partially-populated
@return the node for @a name, or NULL upon error
*/
static GNode *_e2_mousebinding_get_category (const gchar *name)
{
	const gchar *e;
	guint level;
	GNode *retval = NULL;

	for (e = name, level = 0; e != NULL; e++, level++)
	{
		e = strchr (e, '.');
		gchar *levelname = (e == NULL) ? (gchar *)name : g_strndup (name, e - name);
		GQuark qname = g_quark_from_string (levelname);
		if (levelname != name)
			g_free (levelname);
		E2_NodeMatch data = { qname, NULL };
		if (btnbindings != NULL)
			g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
				(GNodeTraverseFunc) _e2_mousebinding_walk_names, &data);
		if (data.match == NULL)
		{
			//init runtime data
			E2_BtnCatRuntime *rt = ALLOCATE0 (E2_BtnCatRuntime);
			CHECKALLOCATEDWARN (rt, return NULL;)
			rt->name_quark = qname;
			rt->node = g_node_new (rt);
			if (btnbindings == NULL)
			{
//				rt->node = g_node_new (rt);
				btnbindings = rt->node;
			}
			else
			{
				//pity about doing this scan again ...
				g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
					(GNodeTraverseFunc) e2_keybinding_walk_toparent, &data);
/*				if (data.match == NULL) can't fail
				{
					DEALLOCATE (E2_BtnCatRuntime, rt);
					return NULL;
				}
*/
//				rt->node = g_node_new (rt);
				g_node_append (data.match, rt->node);
			}
			retval = rt->node; //in case we're finished now
			rt->level = level;
		}
		else
			retval = data.match;
		if (e == NULL)
			break;
	}
	return retval;
}
/**
@brief find first binding for @a widget which conforms to all args
@param event gdk data struct for event being processed
//@param level 0-based config-treestore level of a category to which @a widget is bound
@param how enum indicating the type of match wanted

@return the matched data struct or NULL if no match
*/
static E2_BtnRuntime *_e2_mousebinding_find_event (E2_BtnCatRuntime *rt,
	GdkEventButton *event, E2_BtnChainType how)
{
	GSList *node;
	for (node = rt->buttons; node != NULL; node = node->next)
	{
		E2_BtnRuntime *this = (E2_BtnRuntime *)node->data;
		if  (event->button == this->bnum
		 && (event->state & E2_MODIFIER_MASK) == this->state)
		{
			gboolean proceed;
			GdkEventType thistype = this->type;
			if (how == EVENT_ANY)
				proceed = TRUE;
			else
			switch (event->type)
			{
#ifdef MCOL_REL
				case GDK_BUTTON_RELEASE:
					//FIXME this is crap without a counter
					proceed = (
					(how == EVENT_SAME &&
					  ( thistype == GDK_BUTTON_RELEASE
					 || thistype == E2_2BUTTON_RELEASE
					 || thistype == E2_3BUTTON_RELEASE )
					)
					|| (how == EVENT_BEFORE &&
					  ( thistype == E2_2BUTTON_RELEASE
					 || thistype == E2_3BUTTON_RELEASE )
					)
					|| (how == EVENT_AFTER &&
					  ( thistype == GDK_BUTTON_RELEASE
					 || thistype == E2_2BUTTON_RELEASE )
					)
						);
					break;
#endif
				case GDK_3BUTTON_PRESS:
					//triple-press can have pre-cursor or but no later one
					proceed = ((how == EVENT_SAME && thistype == GDK_3BUTTON_PRESS)
							|| (how == EVENT_BEFORE && (thistype == GDK_BUTTON_PRESS || thistype == GDK_2BUTTON_PRESS))
								);
					break;
				case GDK_2BUTTON_PRESS:
					//double-press can have pre-cursor or maybe a repeat of a later one
					proceed = ((how == EVENT_SAME && thistype == GDK_2BUTTON_PRESS)
							|| (how == EVENT_BEFORE && thistype == GDK_BUTTON_PRESS)
							|| (how == EVENT_AFTER && thistype == GDK_3BUTTON_PRESS)
								);
					break;
				default:
					//single-press can't have pre-cursor, but maybe a repeat of a later one
					proceed = ((how == EVENT_SAME && thistype == GDK_BUTTON_PRESS)
							|| (how == EVENT_AFTER && (thistype == GDK_2BUTTON_PRESS || thistype == GDK_3BUTTON_PRESS))
								);
					break;
			}
			if (proceed) //all tests passed
			{
				return this;
			}
		}
	}
	return NULL;
}
/**
@brief find any chained binding which conforms to all args
Relative comparisons (BEFORE/AFTER etc) mean @a binding data is BEFORE/AFTER
@a event data in sequence of events e.g. press BEFORE double BEFORE triple
@param from widget where event was initiated
@param event gdk data struct for event being processed
@param binding data for binding applicable to @a event
@param how enum indicating the type of match wanted

@return the matched data struct or NULL if no match
*/
static E2_BtnChain *_e2_mousebinding_match_chain (GtkWidget *from,
	GdkEventButton *event, E2_BtnRuntime *binding, E2_BtnChainType how)
{
	//CHECKME for multi-clicks, press/release events affecting nested widgets
	//can arrive in _strange_ order !!
	E2_BtnChain *match = NULL;
	GSList *member;
	for (member = chainevents; member != NULL; member = member->next)
	{
		E2_BtnChain *this = (E2_BtnChain *)member->data;
		//easiest and most-failable tests first
		if (from == this->from
			&& event->device == this->device
			&& (binding == this->binding || how != EVENT_SAME)
			&& event->button == this->binding->bnum
			&& (event->state & E2_MODIFIER_MASK) == this->binding->state
			)
		{
			gboolean proceed;
#ifdef MCOL_REL
			guint releases;
#endif
			GdkEventType thistype = this->binding->type;
			if (how == EVENT_ANY)
				proceed = TRUE;
			else
			switch (event->type)
			{
#ifdef MCOL_REL
				case GDK_BUTTON_RELEASE:
					releases = this->relcount + 1;
					proceed = (
					(how == EVENT_SAME &&
					  ( (releases == 1 && thistype == GDK_BUTTON_RELEASE)
					 || (releases == 2 && thistype == E2_2BUTTON_RELEASE)
					 || (releases == 3 && thistype == E2_3BUTTON_RELEASE) )
				    )
					|| (how == EVENT_BEFORE &&
					  ( (releases == 1 && thistype == E2_2BUTTON_RELEASE)
					 || ((releases == 1 || releases == 2) && thistype == E2_3BUTTON_RELEASE) )
					)
					|| (how == EVENT_AFTER &&
					  ( ((releases == 2 || releases == 3) && thistype == GDK_BUTTON_RELEASE)
					 || (releases == 3 && thistype == E2_2BUTTON_RELEASE) )
					)
						);
					break;
#endif
				case GDK_3BUTTON_PRESS:
					//triple-press can have pre-cursor or but no later one
					proceed = ((how == EVENT_SAME && thistype == GDK_3BUTTON_PRESS)
							|| (how == EVENT_BEFORE && (thistype == GDK_BUTTON_PRESS || thistype == GDK_2BUTTON_PRESS))
								);
					break;
				case GDK_2BUTTON_PRESS:
					//double-press can have pre-cursor or maybe a repeat of a later one
					proceed = ((how == EVENT_SAME && thistype == GDK_2BUTTON_PRESS)
							|| (how == EVENT_BEFORE && thistype == GDK_BUTTON_PRESS)
							|| (how == EVENT_AFTER && thistype == GDK_3BUTTON_PRESS)
								);
					break;
				default:
					//single-press can't have pre-cursor, but maybe a repeat of a later one
					proceed = ((how == EVENT_SAME && thistype == GDK_BUTTON_PRESS)
							|| (how == EVENT_AFTER && (thistype == GDK_2BUTTON_PRESS || thistype == GDK_3BUTTON_PRESS))
								);
					break;
			}
			if (proceed) //all tests passed
			{
				match = this;
				break;
			}
		}
	}
	return match;
}

/**
@brief parse button-name string
@a count is relevant only if E2_KEYFAKE is defined
@param button the button name string, optionally with leading <mod>'s and/or
 '+'-separated button-numbers
@param number store for button number or buttons-mask corresponding to @a button
@param state store for modifier key flags
@param count whether @a button is single, double or triple-event, or NULL
@param multi TRUE to look for > 1 button and create a mask instead of a number

@return TRUE if the parse was successful
*/
gboolean e2_mousebinding_parse_name (gchar *button, guint32 *number,
	GdkModifierType *state, guint *count, gboolean multi)
{
	//detect and process any modifiers or number > 9 in the string
	gchar *s = strrchr (button, '>');
	if (s != NULL)	//modifier(s) present
	{
		gchar c;
#ifdef WITH_BUTTONFAKE
		//detect and process any <NPress> in button string
		guint times = 1;
		gchar *p1, *p2, *tail, *m = button;
		while ((m = strstr (m, "Press")) != NULL) //not translated
		{
			//go forward to >, else abort
			tail = strchr (m, '>');
			if (tail == NULL)
				break;
			//go back to <, else abort
			p1 = g_strndup (button, m - button);
			p2 = strrchr (p1, '<');
			if (p2 == NULL)
			{
				g_free (p1);
				break;
			}
			*p2 = '\0';
			//record first count item
			if (count != NULL && times == 1)
			{
				c = *(p2 + sizeof (gchar));
				if (c == '2' || c == '3')
					times = c - '0';
			}
			//join parts before and after
			m = button;
			button = g_strconcat (p1, ++tail, NULL);
			g_free (m);
			g_free (p1);
			m = button + (p2 - p1);
		}
		if (m != NULL)
			return FALSE;
# ifdef MCOL_REL
		m = button;
		while ((m = strstr (m, "Release")) != NULL)	//not translated
		{
			//go forward to >, else abort
			tail = strchr (m, '>');
			if (tail == NULL)
				break;
			//go back to <, else abort
			p1 = g_strndup (button, m - button);
			p2 = strrchr (p1, '<');
			if (p2 == NULL)
			{
				g_free (p1);
				break;
			}
			*p2 = '\0';
			//record first count item
			if (count != NULL && times == 1)
			{
				c = *(p2 + sizeof (gchar));
				if (c == '2' || c == '3')
					times = c - '0';
			}
			//join parts before and after
			m = button;
			button = g_strconcat (p1, ++tail, NULL);
			g_free (m);
			g_free (p1);
			m = button + (p2 - p1);
		}
		if (m != NULL)
			return FALSE;
# endif
		if (count != NULL)
			*count = times;
		s = strrchr (button, '>');
		if (s != NULL)	//modifier(s) still present
		{
#endif //def WITH_BUTTONFAKE
			//fake processing of button-string, as if keycode == button num
			//for gtk parsing, limit to a single digit, assume no whitespace
			s += 2 * sizeof (gchar);
			c = *s;
			*s = '\0';
			gtk_accelerator_parse (button, number, state);
			if (*number != 0 || *state != 0) //valid parse
			{
				*s = c;
				s--;
			}
			else
				return FALSE;
#ifdef WITH_BUTTONFAKE
		}
#endif
	}
	else	//no modifiers
	{
		*state = 0;
		if (count != NULL)
			*count = 0;
		s = button;
	}

	if (multi)
	{
		guint32 mask = 0;
		while (*s != '\0')
		{
			gulong store = strtoul (s, &s, 10);
			if (*s != '\0' && *s != '+')
				return FALSE;
			if (store == 0 || store > 32)
				return FALSE;
			mask |= 1 << (store - 1);
//			if (*s == '+')
//				s++;
		}
		if (mask > 0)
			*number = mask;
		else
			return FALSE;
	}
	else
	{
		*number = strtoul (s, &s, 10); //any number will do
		if (*s != '\0')
			return FALSE;
	}

	return TRUE;
}

  /*********************/
 /***** callbacks *****/
/*********************/

#ifdef E2_IDLE_BTNSYNC
static gboolean _e2_mousebinding_walk_sync (GNode *node, gpointer data)
{
	_e2_mousebinding_sync_category ((E2_BtnCatRuntime *) node->data);
	return FALSE;
}
/**
@brief convert data for one or all binding category(ies) from treestore to
runtime format (if any category remains to be processed)
This func is mainloop idle callback, to shave some time from session startup
@param data pointer to binding to be processed, or NULL to do all in list

@return FALSE (so the idle is cancelled)
*/
static gboolean _e2_mousebinding_idle_sync_cb (E2_BtnCatRuntime *data)
{
	if (data != NULL)
		//sync a single BtnCatRuntime
		_e2_mousebinding_sync_category (data);
	else
	{
/*		if (binding_cur == NULL)	//after all bindings (or none) have been synced ...
			binding_cur = btnbindings;	//start syncing from beginning of list
		if (binding_cur != NULL)
		{
			//sync a single BtnCatRuntime
			_e2_mousebinding_sync_category ((E2_BtnCatRuntime *) binding_cur->data);
			//setup pointer for next idle callback
			binding_cur = binding_cur->next;
		}
*/
		g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousebinding_walk_sync, NULL);
	}
	return FALSE;	//remove this fn from event sources
}
/* *
@brief setup for converting binding data, after the bindindgs treestore is altered
@param model UNUSED
@param arg1  UNUSED
@param arg2  UNUSED
@param data  UNUSED
@return
*/
/*static void _e2_mousebinding_model_changed_cb (
		GtkTreeModel *model,
		GtkTreePath *arg1,
		GtkTreeIter *arg2,
		gpointer data)
{
	binding_cur = NULL;
FIXME new approach timer does one category only
	g_idle_add ((GSourceFunc) _e2_mousebinding_idle_sync_cb, NULL);
}*/
#endif  //def E2_IDLE_BTNSYNC

/**
@brief (de)sensitize option tree buttons for selected option tree row
Config dialog page buttons are de-sensitized if the row is a category.
@param selection pointer to selection
@param model config data tree model
@param path treepath of row to be checked
@param path_currently_selected
@param set data struct for the button bindings option

@return TRUE always (the row is always selectable)
*/
gboolean e2_mousebinding_tree_selection_check_cb (
	GtkTreeSelection *selection, GtkTreeModel *model, GtkTreePath *path,
	gboolean path_currently_selected, E2_OptionSet *set)
{
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		gchar *cat;
		gtk_tree_model_get (set->ex.tree.model, &iter, MCOL_CAT, &cat, -1);
		gboolean retval = (cat[0] == '\0');
		g_free (cat);
		GtkTreeView *view = gtk_tree_selection_get_tree_view (selection);
		NEEDCLOSEBGL
		e2_option_tree_adjust_buttons (view, retval);
		NEEDOPENBGL
	}
	return TRUE;
}
/**
@brief ensure that cells in the same row as any category name are hidden

Checks whether @a iter in @a model has an empty string in column 0.
If not (i.e. it is a category row), the cell content is hidden,

@param model treemodel for the button-bindings option tree
@param iter pointer to iter with data for the current model row
@param cell cell renderer UNUSED
@param data pointer to data UNUSED

@return TRUE (cell is visible) if row is not a category name
*/
gboolean e2_mousebinding_visible_check_cb (GtkTreeModel *model,
	GtkTreeIter *iter, GtkCellRenderer *cell, gpointer data)
{
	gchar *cat;
	gtk_tree_model_get (model, iter, MCOL_CAT, &cat, -1);
	gboolean retval = (cat[0] == '\0');
	g_free (cat);
	return retval;
}
/**
@brief decide whether tree row is draggable
Checks whether column 0 of the current row has a null string i.e. the row is not
a category. If so, the row is draggable
@param drag_source GtkTreeDragSource data struct
@param path tree path to a row on which user is initiating a drag

@return TRUE if the row can be dragged
*/
gboolean e2_mousebinding_tree_draggable_check_cb (
	GtkTreeDragSource *drag_source, GtkTreePath *path)
{
	if (!GTK_IS_TREE_MODEL (drag_source))
		return TRUE;
	GtkTreeModel *model = GTK_TREE_MODEL (drag_source);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (model, &iter, path))
	{
		gchar *cat;
		gtk_tree_model_get (model, &iter, MCOL_CAT, &cat, -1);
		gboolean retval = (cat[0] == '\0');
		g_free (cat);
		return retval;
	}
	return TRUE;
}
/**
@brief perform the action specified in @a data
This func is a mainloop timer callbacK
@param data pointer to mouse chain data struct

@return FALSE always to cancel the timer
*/
static gboolean _e2_mousebinding_do_action_cb (E2_BtnChain *data)
{
	//in case the action takes a while ...
	g_source_remove (data->timeout_id);

	E2_BtnRuntime *binding = data->binding;
	e2_action_run_simple_from (binding->action, binding->action_data, data->from);
	data->timeout_id = 0;	//no need to explicitly remove source when cleaning
	_e2_mousebinding_clear_chainmember (data);
	return FALSE;
}
/**
@brief find and run any action that's assigned to combination of @a widget and @a event
This is a button event (press and depending on MCOL_REL, maybe also release) signal callback.
Sequences of events are 'marshalled' for multi-clicks with distinct bindings for
different numbers of clicks
@param widget the widget where @a event was initiated
@param event data struct for the event
@param user_data UNUSED pointer specified when cb was connected

@return TRUE (i.e. prevent other handlers) if @a event has been acted upon
*/
static gboolean _e2_mousebinding_button_event_cb (GtkWidget *widget,
	GdkEventButton *event, gpointer user_data)
{
	printd (DEBUG,"custom-button-event cb begins: event-type: %d", event->type);
	gboolean retval = FALSE;
	GNode *topnode = NULL;
	GPtrArray *boundto = (GPtrArray *)
		g_object_get_data (G_OBJECT (widget), BUTTON_BINDINGS_KEY);
	guint indx;
	for (indx = 0; indx < boundto->len; indx++)
	{
		GNode *node = ((E2_BtnCatRuntime *) g_ptr_array_index (boundto, indx))->node;
		while (1)
		{
			GSList *member;
			for (member = ((E2_BtnCatRuntime *)node->data)->buttons; member != NULL;
				 member = member->next)
			{
//=============
				E2_BtnRuntime *binding = _e2_mousebinding_find_event (
					(E2_BtnCatRuntime *)node->data, event, EVENT_SAME);
				if (binding != NULL) //exact match for this event
				{
					E2_BtnChain *pausedata = _e2_mousebinding_match_chain (widget,
						event, binding, EVENT_SAME);
					if (pausedata != NULL)	//there's already a watch in progress for this event
					{
						printd (DEBUG,"extend existing button binding watch");
						//wait some more before acting
						if (pausedata->timeout_id != 0)	//should always succeed
							g_source_remove (pausedata->timeout_id);
						pausedata->timeout_id = g_timeout_add (click_interval + 50,
							(GSourceFunc)_e2_mousebinding_do_action_cb, pausedata);
					}
					else
					{
						printd (DEBUG,"no existing button binding watch");
#ifdef MCOL_REL
						guint32 releases = 0;
#endif
						pausedata = _e2_mousebinding_match_chain (widget, event, binding, EVENT_BEFORE);
						if (pausedata != NULL)	//there's already a 'preceding' watch in progress for this event
						{
#ifdef MCOL_REL
							if (event->type == GDK_BUTTON_RELEASE)
								releases = pausedata->relcount + 1;	//CHECKME
#endif
							//we're now forward in the sequence, don't need old data
							_e2_mousebinding_clear_chainmember (pausedata);
							printd (DEBUG,"killed prior button binding watch");
						}
						else
							printd (DEBUG,"no prior button binding watch");

						if (_e2_mousebinding_find_event (
							(E2_BtnCatRuntime *)node->data, event, EVENT_AFTER) != NULL)
						{	//there's a 'chained' binding which may be triggered by another event
							//We don't now know which one to perform, so wait to see if that other
							//event happens
							pausedata = ALLOCATE (E2_BtnChain);
							CHECKALLOCATEDWARN (pausedata, return TRUE;)
							pausedata->from = widget;
							pausedata->device = event->device;
							pausedata->binding = binding;
#ifdef MCOL_REL
							pausedata->relcount = releases;
#endif
							pausedata->timeout_id = 0;	//race management
							chainevents = g_slist_prepend (chainevents, pausedata);
							pausedata->timeout_id = g_timeout_add (click_interval + 50,
								(GSourceFunc)_e2_mousebinding_do_action_cb, pausedata);
						}
						else	//just run it CHECKME in another thread or idle cb?
						{
//							NEEDCLOSEBGLX
							OPENBGL
							e2_action_run_simple_from (binding->action, binding->action_data, widget);
							CLOSEBGL
							//remember how far up this branch we've gone, to prevent repeats for other categories
//CHECKME					if (topnode == NULL || g_node_depth (node) < g_node_depth (topnode))
								topnode = node;
							retval = TRUE; //prevent any other handler
							break;
						}
					}
				}
//=============
			}
			if (member != NULL || G_NODE_IS_ROOT (node))
				break;
			else
			{
				node = node->parent;
				if (node == topnode)
					break;
			}
		}
	}
//	printd (DEBUG,"custom-button-event cb returns %s", (retval) ? "TRUE":"FALSE");
	return retval;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief re-register all "core" widgets' button bindings
Binding to the custom-button handler should occur before binding to a 'normal'
callback for the respective widgets

@return
*/
void e2_mousebinding_register_all (void)
{
	e2_mousebinding_enrol (NULL, _C(17), (void(*)(E2_OptionSet*))NULL);
#ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (NULL, _C(17), (void(*)(E2_OptionSet*))NULL);
#endif

	gchar *category = g_strconcat(_C(17),".",_C(23),NULL);  //_("general.main
	e2_mousebinding_enrol (NULL, category, (void(*)(E2_OptionSet*))NULL);
#ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (NULL, category, (void(*)(E2_OptionSet*))NULL);
#endif
	g_free (category);

	e2_fileview_register_pointerbindings (app.pane1.view.treeview);
	e2_fileview_register_pointerbindings (app.pane2.view.treeview);

	GList *member;
	for (member = app.command_lines; member != NULL ; member = g_list_next (member))
	{
		E2_CommandLineRuntime *line = (E2_CommandLineRuntime *) member->data;
		e2_command_line_register_pointerbindings (line);
	}
	//tab bindings are also set when the tab is created - so that new ones are covered
	for (member = app.tabslist; member != NULL ; member = g_list_next (member))
	{
		E2_OutputTabRuntime *tab = (E2_OutputTabRuntime *)member->data;
		e2_output_register_pointerbindings (GTK_WIDGET(tab->text));
	}
}
/**
@brief apply button-binding category @a category to "core" widget @a widget
If E2_TREEINCREMENT applies and @a category doesn't exist in the bindings treestore,
it (and any missing ancestor(s)) is added to that store.
If @a category hasn't been used before, its data are setup for runtime usage, or
if E2_IDLE_BTNSYNC applies, such setup is arranged (to be done at idle time)
If non-NULL, the function named @a defaults_func must have the same form as
_e2_keybinding_tree_defaults (). But there is no enrolment if @a category is
already in the keybindings treestore (from config file or a prior instance of a
transient item that uses the category)
@param widget the widget to which the binding will be attached, or NULL
@param category name of the binding, structured like level1[.level2.level3 ...]
@param defaults_func pointer to function which creates or adds to a tree-option array, or NULL

@return
*/
void e2_mousebinding_enrol (GtkWidget *widget, const gchar *category,
	void (*defaults_func)(E2_OptionSet*))
{
	GNode *member = _e2_mousebinding_get_category (category);
	if (member == NULL)
		return;
	E2_BtnCatRuntime *rt = (E2_BtnCatRuntime *)member->data;
	gboolean first = (rt->ref == NULL);	//first-time usage of category
	if (first)
	{
		//add iter to bindings config treestore if not already present
		E2_OptionSet *set = e2_option_get ("mousebuttons");
		GtkTreeIter iter;
#ifdef E2_TRANSIENTBINDINGS
		gboolean old =
#endif
		e2_tree_get_lowest_iter_for_str (set->ex.tree.model, MCOL_CAT, &iter, category);
		//the model may change between registrations, so remember where this cat starts
		GtkTreePath *path = gtk_tree_model_get_path (set->ex.tree.model, &iter);
		rt->ref = gtk_tree_row_reference_new (set->ex.tree.model, path);
		gtk_tree_path_free (path);
#ifdef E2_TRANSIENTBINDINGS
		if (defaults_func != (void(*)(E2_OptionSet*))NULL && !old)
		{
			//run the defaults-install function, which in turn calls
			//e2_option_tree_setup_defaults() which sets up a pointer array at
			//dummy_set.ex.tree.def.tree_strings
			E2_OptionSet dummyset;
			(*defaults_func) (&dummyset);
			gchar **split = dummyset.ex.tree.def.tree_strings;

			//adjust start-index of parsed array
			const gchar *p = strrchr (category, '.');
			p = (p == NULL) ? category : p + sizeof (gchar);
			gint i;
			for (i = 0; split[i] != NULL && strstr (split[i], p) == NULL; i++);
			if (G_UNLIKELY (split[i] == NULL))
				i = 0;
			//parse the data & add it to the bindings store
			e2_option_tree_set_from_array (set->name, &split[i], NULL, &iter);

			g_strfreev (split);
		}
#endif
#ifdef E2_IDLE_BTNSYNC
		g_idle_add ((GSourceFunc) _e2_mousebinding_idle_sync_cb, rt);
		printd (DEBUG, "arranged button bindings idle-sync for %s", category);
#else
		//translate the option tree data to rt form, now
		_e2_mousebinding_sync_category (rt);
#endif
	}

	if (G_LIKELY (widget != NULL))
	{
		printd (DEBUG, "register widget %x in btnbinding category '%s'", widget, category);
		GPtrArray *boundto = (GPtrArray *)
				g_object_get_data (G_OBJECT (widget), BUTTON_BINDINGS_KEY);
		if (boundto == NULL)	//this is the first time widget is enrolled
		{
			//the model is: just one cb for all bindings
			g_signal_connect (G_OBJECT (widget), "button-press-event",
				G_CALLBACK (_e2_mousebinding_button_event_cb), NULL);
#ifdef MCOL_REL
			g_signal_connect (G_OBJECT (widget), "button-release-event",
				G_CALLBACK (_e2_mousebinding_button_event_cb), NULL);
#endif

			boundto = g_ptr_array_new ();
			g_object_set_data_full (G_OBJECT (widget), BUTTON_BINDINGS_KEY, boundto,
				(GDestroyNotify) e2_keybinding_clear_widget_bindings);
			//widget remembers it's in the category
			g_ptr_array_add (boundto, rt);
			//and vice versa
			rt->instances = g_slist_append (rt->instances, widget);
		}
		else	//already in category(ies)
		{
			//ensure widget is in only 1 category per categories-branch
			gboolean descendant = FALSE;
			guint indx;
			for (indx = 0; indx < boundto->len; indx++)
			{
				E2_BtnCatRuntime *this = (E2_BtnCatRuntime *)g_ptr_array_index (boundto, indx);
				if (g_node_is_ancestor (this->node, rt->node))
				{
					g_ptr_array_remove (boundto, rt);
					this->instances = g_slist_remove_all (this->instances, widget);
					break;
				}
				else if (this->node == rt->node
						|| g_node_is_ancestor (rt->node, this->node))
				{
					descendant = TRUE;
					break;
				}
			}

			if (!descendant)	//no same- or lower-level binding yet for this widget
			{
				//widget remembers it's in the category
				g_ptr_array_add (boundto, rt);
				//and vice versa
				rt->instances = g_slist_append (rt->instances, widget);
			}
		}
	}
}
/**
@brief unregister all button binding callbacks for a widget
@param node node in categories tree
@param data pointer to data for widget to be unblocked
@return FALSE
*/
static gboolean _e2_mousebinding_walk_unregister (GNode *node, E2_NodeData *data)
{
	E2_BtnCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				if (GTK_IS_WIDGET (widgets->data))
				{
					//in case widget isn't to be destroyed
					g_signal_handlers_disconnect_matched (widgets->data,
						G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_mousebinding_button_event_cb, NULL);
					printd (DEBUG, "UNregister buttonbinding for widget");
					gpointer boundto = g_object_get_data (G_OBJECT (widgets->data),
							BUTTON_BINDINGS_KEY);
					if (boundto != NULL)
					{
						while (g_ptr_array_remove_fast ((GPtrArray *)boundto, rt)); //probably only one
					}
				}
				else
					printd (DEBUG, "No UNregister button-binding for destroyed widget");
				widgets->data = NULL;	//don't remove, list pointer cannot change yet
			}
		}
		rt->instances = g_slist_remove_all (rt->instances, NULL);
	}
	return FALSE;
}
/**
@brief cancel binding of @a category to @a widget
@param widget the widget which is to be unbound, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_mousebinding_disrol (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_mousebinding_disrol, category: %s, widget: _",
		(category) ? category : "ALL");
	GQuark qname;
	if (category != NULL)
	{
		qname = g_quark_try_string (category);
		if (qname == 0)
			return;	//nothing will match, so nothing to do
	}
	else
		qname = 0;

	E2_NodeData data = { qname, widget };
	g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousebinding_walk_unregister, &data);
}
/**
@brief block all button binding callbacks for a widget
@param node node in categories tree
@param data pointer to data for widget to be blocked
@return FALSE
*/
static gboolean _e2_mousebinding_walk_block (GNode *node, E2_NodeData *data)
{
	E2_BtnCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				g_signal_handlers_block_matched (widgets->data,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_mousebinding_button_event_cb, NULL);
			}
		}
	}
	return FALSE;
}
/**
@brief block operation of binding @a category to @a widget
@param widget the widget which is to be blocked, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_mousebinding_block (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_mousebinding_block, category: %s, widget: _",
		(category) ? category : "ALL");
	GQuark qname;
	if (category != NULL)
	{
		qname = g_quark_try_string (category);
		if (qname == 0)
			return;	//nothing will match, so nothing to do
	}
	else
		qname = 0;

	E2_NodeData data = { qname, widget };
	g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousebinding_walk_block, &data);
}
/**
@brief unblock all button binding callbacks for a widget
@param node node in categories tree
@param data pointer to data for widget to be unblocked
@return FALSE
*/
static gboolean _e2_mousebinding_walk_unblock (GNode *node, E2_NodeData *data)
{
	E2_BtnCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				g_signal_handlers_unblock_matched (widgets->data,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_mousebinding_button_event_cb, NULL);
			}
		}
	}
	return FALSE;
}
/**
@brief unblock operation of binding @a category to @a widget
@param widget the widget which is to be unblocked, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_mousebinding_unblock (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_mousebinding_block, category: %s, widget: _",
		(category) ? category : "ALL");
	GQuark qname;
	if (category != NULL)
	{
		qname = g_quark_try_string (category);
		if (qname == 0)
			return;	//nothing will match, so nothing to do
	}
	else
		qname = 0;

	E2_NodeData data = { qname, widget };
	g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousebinding_walk_unblock, &data);
}

/**
@brief iterate over all tree categories to cleanup data
@param node a node in the categories tree
@param pointerised TRUE/FALSE, TRUE to completely clean the data (session-end)
@return FALSE
*/
static gboolean _e2_mousebinding_walk_categories (GNode *node, gpointer data)
{
	E2_BtnCatRuntime *rt = node->data;
	gtk_tree_row_reference_free (rt->ref);
	rt->ref = NULL;
	if (rt->buttons != NULL)
	{
		_e2_mousebinding_free_buttonslist (rt->buttons);
		rt->buttons = NULL;
	}
	if (rt->instances != NULL)
	{
/*		if (data != NULL)
		{
			//if bound widgets are not being destroyed, disconnect signals with data = rt
			GSList *widgets;
			for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
			{
				if (GTK_IS_WIDGET (widgets->data))	//the bound widget may have been destroyed
				{
					g_signal_handlers_disconnect_matched (widgets->data,
						G_SIGNAL_MATCH_FUNC, //| G_SIGNAL_MATCH_DATA,
						0, 0, NULL, _e2_mousebinding_button_event_cb, NULL);
				}
			}
		}
*/
		g_slist_free (rt->instances);
		rt->instances = NULL;
	}
	if (data != NULL)
	{
		DEALLOCATE (E2_BtnCatRuntime, rt);
		g_node_destroy (node);
	}
	return FALSE;
}
/**
@brief cleanup some btn bindings' runtime data
The bindings data list is not affected
Callbacks are checked before re-connection

@return
*/
void e2_mousebinding_clean (void)
{
	g_node_traverse (btnbindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
		(GNodeTraverseFunc) _e2_mousebinding_walk_categories, NULL);
}
/* *
@brief cleanup all bindings data
Presently unused
@return
*/
/*void e2_mousebinding_free_all (void)
{
	g_node_traverse (btnbindings, G_POST_ORDER, G_TRAVERSE_ALL, -1,
		(GNodeTraverseFunc) _e2_mousebinding_walk_categories, GINT_TO_POINTER (1));
*/
/**
@brief display current button bindings, all or a nominated category
Expects BGL on/closed
@param parameters argument(s) to help command, starting with _(buttons)

@return
*/
void e2_mousebinding_output_help (gchar *parameters)
{
	//decide which section of the bindings to report
	//NULL reports all (general)
	gchar *section = e2_utils_find_whitespace (parameters);
	if (section != NULL)
	{
		section =  g_strstrip (section);
		if (*section == '\0')
			section = NULL;
	}
	// get treestore iter for start of bindings to report
	GtkTreeIter iter;
	E2_OptionSet *set = e2_option_get ("mousebuttons");
	GtkTreeModel *mdl = set->ex.tree.model;
	if (section == NULL)
		gtk_tree_model_get_iter_first (mdl, &iter);
	else if (!e2_tree_find_iter_from_str (mdl, MCOL_CAT, section, &iter, FALSE))
	{
		gchar *msg = g_strdup_printf (_("Cannot find a button binding named %s"),
			section);
		e2_output_print_error (msg, TRUE);
		return;
		//revert to showing all
//		gtk_tree_model_get_iter_first (mdl, &iter);
	}

	GString *str = g_string_sized_new (64);
	g_string_printf (str,
		"\t  "PROGNAME" %s\n"
		"\t+---------------------------+\n",
		_("button bindings"));
	e2_output_print (&app.tab, str->str, NULL, FALSE, NULL);
	str = g_string_truncate (str, 0);

	gchar detail[16];
	const gchar *fmts[] = { "<%s>", "<2%s>", "<3%s>" }; //0 used only for single-release
	//in accord with gtk parsing of key-modifiers, these are not translated
	const gchar *extras[] = { "Press"
#ifdef MCOL_REL
		, "Release"
#endif
		};

	do
	{
		GtkTreeIter iter2;
		if (gtk_tree_model_iter_children (mdl, &iter2, &iter))
		{
			do
			{
				gchar *cat, *btn, *act, *arg, *numptr, *temp;
				const gchar *fmt;
				gboolean dbl, tripl;
#ifdef MCOL_REL
				gboolean rel;
#endif
				// get data from treestore
				gtk_tree_model_get (mdl, &iter2, MCOL_CAT, &cat, MCOL_BTN, &btn,
					MCOL_DBL, &dbl, MCOL_TRP, &tripl,
#ifdef MCOL_REL
					MCOL_REL, &rel,
#endif
					MCOL_ACT, &act, MCOL_ARG, &arg, -1);
				if (*cat != '\0')
				{
					GtkTreeIter iter3;
					g_string_append_printf (str, "\t%s\n", cat);
					g_free (cat);
					if (gtk_tree_model_iter_children (mdl, &iter3, &iter2))
					{
						do
						{
							gtk_tree_model_get (mdl, &iter3, MCOL_CAT, &cat, MCOL_BTN, &btn,
								MCOL_DBL, &dbl, MCOL_TRP, &tripl,
#ifdef MCOL_REL
								MCOL_REL, &rel,
#endif
								MCOL_ACT, &act, MCOL_ARG, &arg, -1);
							if (*cat != '\0')
							{
								GtkTreeIter iter4;
								g_string_append_printf (str, "\t\t%s\n", cat);
								g_free (cat);
								if (gtk_tree_model_iter_children (mdl, &iter4, &iter3))
								{
									do
									{
										gtk_tree_model_get (mdl, &iter4, MCOL_BTN, &btn,
											MCOL_DBL, &dbl, MCOL_TRP, &tripl,
#ifdef MCOL_REL
											MCOL_REL, &rel,
#endif
											MCOL_ACT, &act, MCOL_ARG, &arg, -1);
#ifdef MCOL_REL
										if (rel)
										{
											if (dbl)
												fmt = fmts[1];
											else if (tripl)
												fmt = fmts[2];
											else
												fmt = fmts[0];
											snprintf (detail, sizeof (detail), fmt, extras[1]);
										}
										else //press
										{
#endif
											if (dbl)
											{
												fmt = fmts[1];
												snprintf (detail, sizeof (detail), fmt, extras[0]);
											}
											else if (tripl)
											{
												fmt = fmts[2];
												snprintf (detail, sizeof (detail), fmt, extras[0]);
											}
											else
												*detail = '\0';
#ifdef MCOL_REL
										}
#endif
										numptr = strrchr (btn, '>');
										if (numptr != NULL)
										{
											//split into parts
											temp = g_strdup (++numptr);
											*numptr = '\0';
											numptr = temp;
										}
										else
										{
											numptr = "";
										}

										g_string_append_printf (str, "\t\t\t%s%s%s\t%s\t%s\n", btn, detail, numptr, act, arg);
										g_free (btn);
										g_free (act);
										g_free (arg);
										if (*numptr != '\0')
											g_free (numptr);
									} while (gtk_tree_model_iter_next (mdl, &iter4));
								}
							}
							else
							{
								//FIXME do this more efficiently
#ifdef MCOL_REL
								if (rel)
								{
									if (dbl)
										fmt = fmts[1];
									else if (tripl)
										fmt = fmts[2];
									else
										fmt = fmts[0];
									snprintf (detail, sizeof (detail), fmt, extras[1]);
								}
								else //press
								{
#endif
									if (dbl)
									{
										fmt = fmts[1];
										snprintf (detail, sizeof (detail), fmt, extras[0]);
									}
									else if (tripl)
									{
										fmt = fmts[2];
										snprintf (detail, sizeof (detail), fmt, extras[0]);
									}
									else
										*detail = '\0';
#ifdef MCOL_REL
								}
#endif
								numptr = strrchr (btn, '>');
								if (numptr != NULL)
								{
									//split into parts
									temp = g_strdup (++numptr);
									*numptr = '\0';
									numptr = temp;
								}
								else
								{
									numptr = "";
								}
								g_string_append_printf (str, "\t\t%s%s%s\t%s\t%s\n", btn, detail, numptr, act, arg);
								g_free (cat);
								g_free (btn);
								g_free (act);
								g_free (arg);
								if (*numptr != '\0')
									g_free (numptr);
							}
						} while (gtk_tree_model_iter_next (mdl, &iter3));
					}
				}
				else
				{
#ifdef MCOL_REL
					if (rel)
					{
						if (dbl)
							fmt = fmts[1];
						else if (tripl)
							fmt = fmts[2];
						else
							fmt = fmts[0];
						snprintf (detail, sizeof (detail), fmt, extras[1]);
					}
					else //press
					{
#endif
						if (dbl)
						{
							fmt = fmts[1];
							snprintf (detail, sizeof (detail), fmt, extras[0]);
						}
						else if (tripl)
						{
							fmt = fmts[2];
							snprintf (detail, sizeof (detail), fmt, extras[0]);
						}
						else
							*detail = '\0';
#ifdef MCOL_REL
					}
#endif
					numptr = strrchr (btn, '>');
					if (numptr != NULL)
					{
						//split into parts
						temp = g_strdup (++numptr);
						*numptr = '\0';
						numptr = temp;
					}
					else
					{
						numptr = "";
					}
					g_string_append_printf (str, "\t%s%s%s\t%s\t%s\n", btn, detail, numptr, act, arg);
					g_free (cat);
					g_free (btn);
					g_free (act);
					g_free (arg);
					if (*numptr != '\0')
						g_free (numptr);
				}
			} while (gtk_tree_model_iter_next (mdl, &iter2));
		}
	} while (gtk_tree_model_iter_next (mdl, &iter) && section == NULL);

	e2_output_print (&app.tab, str->str, NULL, FALSE, NULL);

	g_string_free (str, TRUE);
}
#ifdef WITH_BUTTONFAKE
typedef struct _E2_BtnFake
{
	gpointer from;
	GdkWindow *window;
	const gchar *buttons_string;
} E2_BtnFake;
/**
@brief issue fake gdk button-event(s) in accord with data
@param data pointer to data for the events
@return
*/
static void _e2_mousebinding_fake_events (E2_BtnFake *data)
{
//	NOTE THIS IS NOT YET TESTED
	//CHECKME separate button-modifier events too ?
	//what about multi-click events ?
	//and corresponding release-event(s) ?
	if (GTK_IS_WIDGET (data->from))
	{
		GdkEvent *event2 = gdk_event_new (GDK_BUTTON_PRESS);
		event2->any.window = data->window;
//		event2->any.send_event = '\1';	//TRUE;
		//multi-btn separator is space char
		gchar **split = g_strsplit (data->buttons_string, " ", -1);
		guint fakebtn, /*times, */i, j = g_strv_length (split);
		GdkModifierType fakestate;
		for (i = 0; i < j; i++)
		{
			if (split[i] == NULL || *split[i] == '\0')
				continue;
/*			//handle instances of "\," which are not a separator
			s = split[i] + strlen (split[i]) - 1;
			if (*s == '\\' && s > split[i] && *(s-1) != '\\')
				*s = ',';
*/
			if (!e2_mousebinding_parse_name (split[i], &fakebtn, &fakestate, NULL, FALSE)) //&times))
				continue;
/*			if (times > 1)
			{
				handle multi event ?
			}
*/
			event2->button.button = fakebtn;
			event2->button.state = fakestate;
//			event2->key.time = event->key.time + i;
			gtk_propagate_event ((GtkWidget *)data->from, event2);
		}
		event2->any.window = NULL; //prevent destruction of the 'real' event window during cleanup
		gdk_event_free (event2);
		g_strfreev (split);
	}
	printd (DEBUG, "_e2_mousebinding_fake_events ends");
}
/**
@brief issue fake gdk button press event(s) in accord with action data
This expects as action-data a string indicating which buttons(s) are to be issued
Each btn name in the string must be in a form parsable by gtk e.g. <Ctrl><Alt>4.
Sequential buttons must be separated by a space char
@param from the widget where the action was initiated - must not be NULL
@param art action runtime data

@return TRUE if timer is set up to issue fake event(s)
*/
static gboolean _e2_mousebinding_issue (gpointer from, E2_ActionRuntime *art)
{
	printd (DEBUG, "_e2_mousebinding_issue from:_ art->data: %s", art->data);
	const gchar *fakes = (const gchar *) art->data;
	if (fakes == NULL || *fakes == '\0')
		return FALSE;	//user goofed the config data
	GdkEvent *event = gtk_get_current_event ();
	if (event == NULL)
		return FALSE;	//should never happen
/*	if (event->type != GDK_BUTTON_PRESS)
	{
		gdk_event_free (event);
		return FALSE;
	}
*/
	E2_BtnFake fake_data;
	fake_data.from = (from != NULL) ? from : app.main_window;	//insurance
	fake_data.window = event->any.window;	//survives event cleanup
	//CHECKME any other event data to re-use ?
	fake_data.buttons_string = fakes;

	gdk_event_free (event);
	_e2_mousebinding_fake_events (&fake_data);
	WAIT_FOR_EVENTS;
	return TRUE;
}
#endif	//def WITH_BUTTONFAKE
/**
@brief register actions related to button bindings

@return
*/
void e2_mousebinding_actions_register (void)
{
#ifdef WITH_BUTTONFAKE
	E2_Action action =
	{g_strconcat(_A(129),".",_A(128),NULL),_e2_mousebinding_issue,TRUE,E2_ACTION_TYPE_ITEM,0,NULL,NULL};
	e2_action_register (&action);
#endif
}
/**
@brief install default tree options for button bindings
This function is called only if set data is missing from the config file
@param set pointer to set data

@return
*/
static void _e2_mousebinding_tree_defaults (E2_OptionSet *set)
{
	//the button name strings are parsed by gtk, no translation is possible
	//button codes may be 1 ... whatever
	//columns: 0 cat, 1 button, 2 dblclick, 3 trplclick, [4 release,] 5 action, 6 action_data
#ifdef MCOL_REL
	ensure lines here, and for all transients, have 6 separators
#endif
	e2_option_tree_setup_defaults (set,
	g_strdup("mousebuttons=<"),  //internal name
	g_strconcat(_C(17),"|||||",NULL),  //_("general"
	g_strconcat("\t",_C(23),"|||||",NULL),  //_("main"
	g_strconcat("\t\t|9|||",_A(14),".",_A(76),"|",NULL), //"refresh"
	g_strconcat("\t\t",_C(33),"|||||",NULL),  //_("panes"
	g_strconcat("\t\t\t|<Control>2|||",_A(13),".",_A(54),"|",NULL), //"go up
	g_strconcat("\t\t\t|4|||",_A(13),".",_A(52),"|",NULL), //"go back
	g_strconcat("\t\t\t|5|||",_A(13),".",_A(53),"|",NULL), //"go forward
	g_strconcat("\t\t",_C(12),"|||||",NULL),  //_"directory line"
	g_strconcat("\t\t\t|1|true||",_A(5),".",_A(30),"|",NULL), //"dirline.activate
	g_strconcat("\t\t\t|<Control>2|||",_A(13),".",_A(67),"|",NULL), //"pane.open
	g_strconcat("\t\t\t|<Alt>2|||",_A(13),".",_A(54),"|",NULL), //"pane.go up
	g_strconcat("\t\t",_C(5),"|||||",NULL),  //_("command line"
	g_strconcat("\t\t\t|1|true||",_A(1),".",_A(30),"|",NULL), //"command.activate
	g_strconcat("\t\t\t|<Alt>2|||",_A(1),".",_A(84),"|",NULL), //"command.send"
	g_strconcat("\t\t",_C(28),"|||||",NULL),  //_("output"
	g_strconcat("\t\t\t|1|true||",_A(10),".",_A(30),"|",NULL), //"activate
	g_strconcat("\t\t\t|<Control>2|||",_A(10),".",_A(33),"|1",NULL), //"adjust ratio
	g_strconcat("\t\t\t|<Shift><Alt>4|true||",_A(10),".",_A(72),"|",NULL), //"page up"
	g_strconcat("\t",_C(11),"|||||",NULL),  //_("dialogs"
	g_strdup(">"),
	NULL);
}
/**
@brief initialize buttons tree-option

@return
*/
void e2_mousebinding_options_register (void)
{
	//no screen rebuilds needed after any change to these options
	gchar *group_name = g_strconcat(_C(20),".",_C(35),NULL);
	E2_OptionSet *set = e2_option_tree_register ("mousebuttons", group_name, _C(35),
		NULL, e2_mousebinding_tree_selection_check_cb, e2_mousebinding_tree_draggable_check_cb,
		E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDBUTTONS);
	e2_option_tree_add_column (set, _("Category"), E2_OPTION_TREE_TYPE_STR, 0, "",
		E2_OPTION_TREE_COL_NOT_EDITABLE, NULL, NULL);
	e2_option_tree_add_column (set, _("Button"), E2_OPTION_TREE_TYPE_BUTTON, 0, "",
		0, e2_mousebinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Double"), E2_OPTION_TREE_TYPE_BOOL, FALSE, "false",
		0, e2_mousebinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Triple"), E2_OPTION_TREE_TYPE_BOOL, FALSE, "false",
		0, e2_mousebinding_visible_check_cb, NULL);
#ifdef MCOL_REL
	e2_option_tree_add_column (set, _("Release"), E2_OPTION_TREE_TYPE_BOOL, FALSE, "false",
		0, e2_mousebinding_visible_check_cb, NULL);
#endif
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, e2_mousebinding_visible_check_cb,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_LAYOUT));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL , 0, "",
		0, e2_mousebinding_visible_check_cb,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_ACCEL //CHECKME EXCLUDE_POINTER ?
		| E2_ACTION_EXCLUDE_LAYOUT | E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_mousebinding_tree_defaults);

}

#endif //def E2_MOUSECUSTOM
