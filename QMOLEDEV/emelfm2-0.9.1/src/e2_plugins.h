/* $Id: e2_plugins.h 2980 2013-11-30 06:09:20Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_PLUGINS_H__
#define __E2_PLUGINS_H__

#include "emelfm2.h"

//plugins config-data treestore columns
enum
{
	LOAD_COL,
	MENU_COL,
	LABEL_COL,
	ICON_COL,
	TIP_COL,
	FILE_COL,
	PATH_COL,
	SIG_COL
};

//PluginAction controls
typedef enum _E2PActStatus
{
	E2PA_NONE		= 0,	 //!< default value when PluginAction is allocated
	E2PA_LOAD		= 1,	 //!< this action is [to be] loaded at runtime
	E2PA_SHOW		= 1 << 1,//!< this action is to be shown in plugins context menu
	E2PA_NONACT		= 1 << 2,//!< this data is not in ->actsarray

	E2PA_CLEANLABEL	= 1 << 5,//!< g_free the label string when done with it
	E2PA_CLEANTIP	= 1 << 6,//!< g_free the tip string when done with it
	E2PA_CLEANICON	= 1 << 7,//!< g_free the icon name string when done with it
} E2PActStatus;
//ensure PluginAction :: flags is big enough for the above enum

//for cleaning all strings
#define E2PA_CLEANALL E2PA_CLEANLABEL | E2PA_CLEANTIP | E2PA_CLEANICON
//for manipulating status
#define IS_LOADED(a) ((a)->flags & E2PA_LOAD)
#define SET_LOADED(a) ((a)->flags |= E2PA_LOAD)
#define SET_UNLOADED(a) ((a)->flags &= ~E2PA_LOAD)
#define IS_SHOWN(a) ((a)->flags & E2PA_SHOW)
#define SET_SHOWN(a) ((a)->flags |= E2PA_SHOW)
#define SET_UNSHOWN(a) ((a)->flags &= ~E2PA_SHOW)

//plugin initialisation controls
typedef enum _E2PInit
{
	E2P_UIDATA = 1,
	E2P_SETUP  = 1 << 1
} E2PInit;
//for initialising everything
#define E2P_INITALL 0x3

typedef enum _E2PStatus
{
	E2P_LOCKIN	= 1,	//!< user may not trigger an unload via a config dialog
	E2P_NOCFG	= 1 << 1,//!< don't show this plugin or its actions if any in any config dialog
	E2P_CANCFG	= 1 << 2,//!< may include this no-action plugin in any config dialog (& its label/tip should be suitably informative)
	E2P_ONMENU	= 1 << 3,//!< always show this plugin's actions in relevant menu
	E2P_OFFMENU	= 1 << 4,//!< never show this plugin's actions in relevant menu
	E2P_CLEANL	= 1 << 5,//!< g_free plugin title string before unloading
	E2P_CLEANT	= 1 << 6,//!< g_free plugin tip string before unloading
} E2PStatus;
//ensure Plugin :: flags is big enough for the above enum

typedef struct _PluginAction
{
	const gchar *signature; //!< unique identifier, same as p->signature
	gchar *label; //!< translated menu text, usually const
	gchar *description; //!< translated tooltip, usually const
	gchar *icon;  //!< icon file : [path/]basename[.ext], usually const
	gchar *aname; //!< action name, allocated, for cleanups, owned by the actions hash-table
	E2_Action *action; //!< local cache used in menu construction
	gpointer action_data; //!< data for action, g_free to clean it if necessary
	E2PActStatus flags : 8;
} PluginAction;

typedef struct _E2_Plugin
{
	const gchar *signature; //!< ID string, never alters after it's initialised
	gchar *title;	//!< for display in config dialogs when plugin has no action
	gchar *tip;		//!< ditto
	GModule *module;	//!< NULL when plugin isn't loaded
//ONLY WORKS for static func gboolean (*cleaner)(struct _E2_Plugin*); //!< pre-unload cleanup func, returns TRUE upon success
	PluginAction *actsarray; //!< array of PluginAction's, data shared with app.plugacts
	guint8 actscount; //!< count of items in actsarray
	guint8 refcount;
	E2PStatus flags : 8;
} Plugin;

//any plugin may have its own custom PluginIface, castable to/from a Plugin,
//and with extra stuff at the end, constituting the plugin's API
#define PluginIface Plugin

//for retrieving data when loading a plugin file
typedef struct _E2P_InitData
{
	GModule *module;
	Plugin *(*init) (E2PInit);
} E2P_InitData;

typedef struct _E2P_DirEnt
{
	gchar *path;
	mode_t mode;
} E2P_DirEnt;

gboolean e2_plugins_open_module (gchar *filepath, E2P_InitData *data);
gboolean e2_plugins_unload1 (Plugin *p, gboolean force);
void e2_plugins_load_configured (void);
void e2_plugins_unload_all (gboolean force);
void e2_plugins_actiondata_clear (PluginAction *pa);
void e2_plugins_store_config_data (GtkTreeModel *model, GtkTreeIter *sibling,
	Plugin *p, gboolean inmenu, gchar *localpath);
gint e2_plugins_count_actions (GtkTreeModel *model, GtkTreeIter *iter);
void e2_plugins_freshen_data (void);
void e2_plugins_update_configured (void);
void e2_plugins_clean (void);

//Plugin *get_plugin_by_name (gchar *name);
Plugin *e2_plugins_get_installed (const gchar *sigvers);
Plugin *e2_plugins_get_plugin (const gchar *sigvers, gboolean with_ref);
gboolean e2_plugins_find_function (const gchar *sigvers, const gchar *func_name,
	gpointer *address);

gboolean e2_plugins_configure (gpointer from, E2_ActionRuntime *art);

E2_Action *e2_plugins_action_register (const E2_Action *newaction);
gboolean e2_plugins_action_unregister (const gchar *name);
E2_OptionSet *e2_plugins_option_register (E2_OptionType type, gchar *name,
	gchar *group, gchar *desc, gchar *tip, gchar *depends,
	E2_OptionSetupExtra *ex, E2_OptionFlags flags);
gboolean e2_plugins_option_unregister (gchar *name);
void e2_plugins_options_register (void);

//declare common in-plugin funcs
Plugin *init_plugin (E2PInit mode);
gboolean clean_plugin (Plugin *p);

//boilerplate code

#define PLUGINIT_INTRO \
/* macros work with castable pointer */ \
PluginIface *ifptr = (PluginIface*)&iface; \
ifptr->signature = ANAME VERSION;

//ifptr->cleaner = clean_plugin;

#ifdef USE_GLIB2_10
# define ALLOCATEPLUGACTS(num) \
ifptr->actsarray = (PluginAction *) g_slice_alloc (sizeof(PluginAction) * num);
#else
# define ALLOCATEPLUGACTS(num) \
ifptr->actsarray = (PluginAction *) g_try_malloc (sizeof(PluginAction) * num);
#endif

#define PLUGINIT_NUMBERED_ALLOCATE(num) \
ALLOCATEPLUGACTS(num) \
if (ifptr->actsarray != NULL) \
{ \
	memset (ifptr->actsarray, 0, sizeof (PluginAction) * num); \
	ifptr->actscount = num;

// OTHER INIT STUFF CAN GO HERE, IF THAT FAILS, ifptr->refcount MUST FINISH AT 0

#define PLUGINIT_NUMBERED_END \
} \
return (Plugin*)ifptr;

#define PLUGINIT_NUMBERED_ACTION(num,ns,ne,func,lbl,tip,icn) \
PluginAction *pa##num = ifptr->actsarray + num - 1; \
if (mode & E2P_SETUP) \
{ \
	E2_Action plugact = \
{g_strconcat (ns,".",ne,NULL),func,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL}; \
	if ((pa##num->action = e2_plugins_action_register (&plugact)) != NULL) \
	{ \
		pa##num->aname = plugact.name; \
		ifptr->refcount = 1; /* signal success */ \
	} \
	else \
		g_free (plugact.name); \
} \
/* data for the menu item */ \
if ((mode & E2P_UIDATA) && ((mode & E2P_SETUP) == 0 || pa##num->aname != NULL)) \
{ \
	pa##num->label = lbl; \
	pa##num->description = tip; \
	pa##num->icon = icn; \
} \
if ((mode & E2P_UIDATA) || pa##num->aname != NULL) \
{ \
	pa##num->signature = #num "@" ANAME; \
} \
else \
{ \
	pa##num = NULL; \
}

#define PLUGINIT_ONE_START(ns,ne,func,lbl,tip,icn) \
PLUGINIT_INTRO \
PluginAction *pa = ALLOCATE0 (PluginAction); \
if (pa != NULL) \
{ \
	if (mode & E2P_SETUP) \
	{ \
		E2_Action plugact = \
	{g_strconcat (ns,".",ne,NULL),func,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL}; \
		if ((pa->action = e2_plugins_action_register (&plugact)) != NULL) \
		{ \
			pa->aname = plugact.name; \
			ifptr->refcount = 1; /* signal success */ \
		} \
		else \
			g_free (plugact.name); \
	} \
	/* data for the menu item */ \
	if ((mode & E2P_UIDATA) && ((mode & E2P_SETUP) == 0 || pa->aname != NULL)) \
	{ \
		pa->label = lbl; \
		pa->description = tip; \
		pa->icon = icn; \
	} \
	if ((mode & E2P_UIDATA) || pa->aname != NULL) \
	{ \
		pa->signature = ANAME; \
	} \
	else \
	{ \
		DEALLOCATE (PluginAction, pa); \
		pa = NULL; \
	} \
} \
if (pa != NULL) \
{ \
	ifptr->actsarray = pa; \
	ifptr->actscount = 1;

// OTHER SUCCESSFUL-INIT STUFF CAN GO HERE, IF THAT FAILS, RESET ifptr->refcount TO 0

#define PLUGINIT_ONE_MIDDLE \
} \
if ((mode & E2P_SETUP) && ifptr->refcount == 0) \
{ \
	if (pa != NULL) \
	{ \
		if (pa->aname != NULL) \
		{ \
			if (!e2_plugins_action_unregister (pa->aname)) \
				g_free (pa->aname); \
		} \
		DEALLOCATE (PluginAction, pa); \
		pa = NULL; \
		ifptr->actsarray = NULL; \
		ifptr->actscount = 0; \
	}

// OTHER FAILED-INIT STUFF CAN GO HERE

#define PLUGINIT_ONE_END \
} \
return (Plugin*)ifptr;

#define PLUGINIT_ONEACTION_SIMPLE(ns,ne,func,lbl,tip,icn) \
PLUGINIT_ONE_START(ns,ne,func,lbl,tip,icn) \
PLUGINIT_ONE_END

/*
#define PLUGINIT_TWO_START(ns1,ne1,func1,lbl1,tip1,icn1,ns2,ne2,func2,lbl2,tip2,icn2) \
PLUGINIT_INTRO \
PLUGINIT_NUMBERED_ACTION(1,ns1,ne1,func1,lbl1,tip1,icn1) \
PLUGINIT_NUMBERED_ACTION(2,ns2,ne2,func2,lbl2,tip2,icn2) \
if (ifptr->actsarray != NULL) \
{ \
	ifptr->actscount = g_slist_length (ifptr->actsarray); \

// OTHER SUCCESSFUL-INIT STUFF CAN GO HERE, IF THAT FAILS, RESET ifptr->refcount TO 0

#define PLUGINIT_TWO_END \
} \
return (Plugin *)ifptr;

#define PLUGINIT_TWOACTION_SIMPLE(ns1,ne1,func1,lbl1,tip1,icn1,ns2,ne2,func2,lbl2,tip2,icn2) \
PLUGINIT_TWO_START(ns1,ne1,func1,lbl1,tip1,icn1,ns2,ne2,func2,lbl2,tip2,icn2) \
PLUGINIT_TWO_END
*/


#ifdef USE_GLIB2_10
# define FREEPLUGACTS g_slice_free1 (p->actscount * sizeof(PluginAction), p->actsarray);
#else
# define FREEPLUGACTS g_free (p->actsarray);
#endif

#define PLUGIN_CLEAR_ACTIONS(p) \
gboolean ret = TRUE; \
if (p->actsarray != NULL) \
{ \
	guint8 i; \
	for (i = 0; i < p->actscount; i++) \
		e2_plugins_actiondata_clear (p->actsarray + i); \
	FREEPLUGACTS \
	p->actsarray = NULL; \
}

#endif //ndef __E2_PLUGINS_H__
