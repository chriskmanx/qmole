
/*
 * Copyright (C) 1999, 2000 Red Hat, Inc.
 *               2001 SuSE Linux AG.
 * All rights reserved.
 *
 * This file is part of GNOME 2.0.
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

#undef TIME_INIT

#define GNOME_ACCESSIBILITY_ENV "GNOME_ACCESSIBILITY"
#define GNOME_ACCESSIBILITY_KEY "/desktop/gnome/interface/accessibility"

/* This module takes care of handling application and library
   initialization and command line parsing */

#include <config.h>
#include "gnome-macros.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <gmodule.h>
#include <gconf/gconf.h>
#include <gconf/gconf-value.h>
#include <gconf/gconf-client.h>

#include "gnome-i18nP.h"

#include "gnome-program.h"
#include "gnome-util.h"
#include "gnome-init.h"
#include "gnome-i18n.h"
#include "gnome-url.h"

struct _GnomeProgramPrivate {
    enum {
	APP_UNINIT=0,
	APP_CREATE_DONE=1,
	APP_PREINIT_DONE=2,
	APP_POSTINIT_DONE=3
    } state;

    /* Construction properties */
    int prop_popt_flags;
    struct poptOptions *prop_popt_table;
    gchar *prop_human_readable_name;
    gchar *prop_gnome_prefix;
    gchar *prop_gnome_libdir;
    gchar *prop_gnome_sysconfdir;
    gchar *prop_gnome_datadir;
    gchar *prop_app_prefix;
    gchar *prop_app_libdir;
    gchar *prop_app_sysconfdir;
    gchar *prop_app_datadir;
    gboolean prop_create_directories;
    gboolean prop_enable_sound;
    gchar *prop_espeaker;

    gchar **gnome_path;

    /* valid-while: state > APP_CREATE_DONE */
    char *app_id;
    char *app_version;
    char **argv;
    int argc;

    /* valid-while: state == APP_PREINIT_DONE */
    poptContext arg_context;

    /* valid-while: state == APP_PREINIT_DONE */
    GArray *top_options_table;
    GSList *accessibility_modules;
};

enum {
    PROP_0,
    PROP_APP_ID,
    PROP_APP_VERSION,
    PROP_HUMAN_READABLE_NAME,
    PROP_GNOME_PATH,
    PROP_GNOME_PREFIX,
    PROP_GNOME_LIBDIR,
    PROP_GNOME_DATADIR,
    PROP_GNOME_SYSCONFDIR,
    PROP_APP_PREFIX,
    PROP_APP_LIBDIR,
    PROP_APP_DATADIR,
    PROP_APP_SYSCONFDIR,
    PROP_CREATE_DIRECTORIES,
    PROP_ENABLE_SOUND,
    PROP_ESPEAKER,
    PROP_POPT_TABLE,
    PROP_POPT_FLAGS,
    PROP_POPT_CONTEXT,
    PROP_LAST
};

static void gnome_program_finalize      (GObject           *object);

static GQuark quark_get_prop = 0;
static GQuark quark_set_prop = 0;

static GPtrArray *program_modules = NULL;
static GPtrArray *program_module_list = NULL;
static gboolean program_initialized = FALSE;
static GnomeProgram *global_program = NULL;

static guint last_property_id = PROP_LAST;

#define	PREALLOC_CPARAMS (8)
#define	PREALLOC_MODINFOS (8)

GNOME_CLASS_BOILERPLATE (GnomeProgram, gnome_program,
			 GObject, G_TYPE_OBJECT)

static void
gnome_program_set_property (GObject *object, guint param_id,
			    const GValue *value, GParamSpec *pspec)
{
    GnomeProgram *program;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNOME_IS_PROGRAM (object));

    program = GNOME_PROGRAM (object);

    switch (param_id) {
    case PROP_POPT_TABLE:
	program->_priv->prop_popt_table = g_value_peek_pointer (value);
	break;
    case PROP_POPT_FLAGS:
	program->_priv->prop_popt_flags = g_value_get_int (value);
	break;
    case PROP_HUMAN_READABLE_NAME:
	g_free (program->_priv->prop_human_readable_name);
	program->_priv->prop_human_readable_name = g_value_dup_string (value);
	break;
    case PROP_GNOME_PATH:
	if (program->_priv->gnome_path) {
	    g_strfreev (program->_priv->gnome_path);
	    program->_priv->gnome_path = NULL;
	}
	if (g_value_get_string (value))
	    program->_priv->gnome_path = g_strsplit
		(g_value_get_string (value), ":", -1);
	break;
    case PROP_GNOME_PREFIX:
	g_free (program->_priv->prop_gnome_prefix);
	program->_priv->prop_gnome_prefix = g_value_dup_string (value);
	break;
    case PROP_GNOME_SYSCONFDIR:
	g_free (program->_priv->prop_gnome_sysconfdir);
	program->_priv->prop_gnome_sysconfdir = g_value_dup_string (value);
	break;
    case PROP_GNOME_DATADIR:
	g_free (program->_priv->prop_gnome_datadir);
	program->_priv->prop_gnome_datadir = g_value_dup_string (value);
	break;
    case PROP_GNOME_LIBDIR:
	g_free (program->_priv->prop_gnome_libdir);
	program->_priv->prop_gnome_libdir = g_value_dup_string (value);
	break;
    case PROP_APP_PREFIX:
	g_free (program->_priv->prop_app_prefix);
	program->_priv->prop_app_prefix = g_value_dup_string (value);
	break;
    case PROP_APP_SYSCONFDIR:
	g_free (program->_priv->prop_app_sysconfdir);
	program->_priv->prop_app_sysconfdir = g_value_dup_string (value);
	break;
    case PROP_APP_DATADIR:
	g_free (program->_priv->prop_app_datadir);
	program->_priv->prop_app_datadir = g_value_dup_string (value);
	break;
    case PROP_APP_LIBDIR:
	g_free (program->_priv->prop_app_libdir);
	program->_priv->prop_app_libdir = g_value_dup_string (value);
	break;
    case PROP_CREATE_DIRECTORIES:
	program->_priv->prop_create_directories = g_value_get_boolean (value);
	break;
    case PROP_ENABLE_SOUND:
	program->_priv->prop_enable_sound = g_value_get_boolean (value);
	break;
    case PROP_ESPEAKER:
	g_free (program->_priv->prop_espeaker);
	program->_priv->prop_espeaker = g_value_dup_string (value);
	break;
    default: {
	    GObjectSetPropertyFunc set_func;

	    set_func = g_param_spec_get_qdata (pspec, quark_set_prop);
	    if (set_func)
		set_func (object, param_id, value, pspec);
	    else
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);

	    break;
	}
    }
}

static void
gnome_program_get_property (GObject *object, guint param_id, GValue *value,
			    GParamSpec *pspec)
{
    GnomeProgram *program;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNOME_IS_PROGRAM (object));

    program = GNOME_PROGRAM (object);

    switch (param_id) {
    case PROP_APP_ID:
	g_value_set_string (value, program->_priv->app_id);
	break;
    case PROP_APP_VERSION:
	g_value_set_string (value, program->_priv->app_version);
	break;
    case PROP_HUMAN_READABLE_NAME:
	g_value_set_string (value, program->_priv->prop_human_readable_name);
	break;
    case PROP_POPT_CONTEXT:
	g_value_set_pointer (value, program->_priv->arg_context);
	break;
    case PROP_GNOME_PATH:
	if (program->_priv->gnome_path)
	    g_value_set_string (value, g_strjoinv (":", program->_priv->gnome_path));
	else
	    g_value_set_string (value, NULL);
	break;
    case PROP_GNOME_PREFIX:
	g_value_set_string (value, program->_priv->prop_gnome_prefix);
	break;
    case PROP_GNOME_SYSCONFDIR:
	g_value_set_string (value, program->_priv->prop_gnome_sysconfdir);
	break;
    case PROP_GNOME_DATADIR:
	g_value_set_string (value, program->_priv->prop_gnome_datadir);
	break;
    case PROP_GNOME_LIBDIR:
	g_value_set_string (value, program->_priv->prop_gnome_libdir);
	break;
    case PROP_APP_PREFIX:
	g_value_set_string (value, program->_priv->prop_app_prefix);
	break;
    case PROP_APP_SYSCONFDIR:
	g_value_set_string (value, program->_priv->prop_app_sysconfdir);
	break;
    case PROP_APP_DATADIR:
	g_value_set_string (value, program->_priv->prop_app_datadir);
	break;
    case PROP_APP_LIBDIR:
	g_value_set_string (value, program->_priv->prop_app_libdir);
	break;
    case PROP_CREATE_DIRECTORIES:
	g_value_set_boolean (value, program->_priv->prop_create_directories);
	break;
    case PROP_ENABLE_SOUND:
	g_value_set_boolean (value, program->_priv->prop_enable_sound);
	break;
    case PROP_ESPEAKER:
	g_value_set_string (value, program->_priv->prop_espeaker);
	break;
    default: {
	    GObjectSetPropertyFunc get_func;

	    get_func = g_param_spec_get_qdata (pspec, quark_get_prop);
	    if (get_func)
		get_func (object, param_id, value, pspec);
	    else
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);

	    break;
	}
    }
}

static void
add_to_module_list (GPtrArray *module_list, const gchar *module_name)
{
    char **modnames;
    int i, j;

    if (!module_name)
	return;

    modnames = g_strsplit (module_name, ",", -1);
    for (i = 0; modnames && modnames[i]; i++) {
	for (j = 0; j < module_list->len; j++)
	    if (strcmp (modnames[i], (char *) g_ptr_array_index (module_list, j)) == 0)
		return;

	g_ptr_array_add (module_list, g_strdup (modnames[i]));
    }
    g_strfreev (modnames);
}

static int
find_module_in_array (const GnomeModuleInfo *ptr, GnomeModuleInfo **array)
{
    int i;

    for (i = 0; array[i] && array[i] != ptr; i++) {
	if (array[i] == ptr)
	    break;
    }

    if (array[i])
	return i;
    else
	return -1;
}

static void /* recursive */
gnome_program_module_addtolist (GnomeModuleInfo **new_list,
				int *times_visited,
				int *num_items_used,
				int new_item_idx)
{
    GnomeModuleInfo *new_item;
    int i;

    g_assert (new_item_idx >= 0);

    new_item = g_ptr_array_index (program_modules, new_item_idx);
    if(!new_item)
	return;

    if (find_module_in_array (new_item, new_list) >= 0)
	return; /* already cared for */

    /* Does this item have any dependencies? */
    if (times_visited[new_item_idx] > 0) {
	/* We already tried to satisfy all the dependencies for this module,
	 *  and we've come back to it again. There's obviously a loop going on.
	 */
	g_error ("Module '%s' version '%s' has a requirements loop.",
		 new_item->name, new_item->version);
    }
    times_visited[new_item_idx]++;

    if (new_item->requirements) {
	for (i = 0; new_item->requirements[i].required_version; i++) {
	    int n;

	    n = find_module_in_array (new_item->requirements[i].module_info,
				      (GnomeModuleInfo **)program_modules->pdata);
	    gnome_program_module_addtolist
		(new_list, times_visited, num_items_used, n);
	}
    }

    /* now add this module on */
    new_list[*num_items_used] = new_item;
    (*num_items_used)++;
    new_list[*num_items_used] = NULL;
}

static void
gnome_program_module_list_order (void)
{
    int i;
    GnomeModuleInfo **new_list;
    int *times_visited; /* Detects dependency loops */
    int num_items_used;

    new_list = g_alloca (program_modules->len * sizeof(gpointer));
    new_list[0] = NULL;
    num_items_used = 0;
  
    times_visited = g_alloca (program_modules->len * sizeof(int));
    memset(times_visited, '\0', program_modules->len * sizeof(int));

    /* Create the new list with proper ordering */
    for(i = 0; i < (program_modules->len - 1); i++) {
	gnome_program_module_addtolist (new_list, times_visited,
					&num_items_used, i);
    }

    /* Now stick the new, ordered list in place */
    memcpy (program_modules->pdata, new_list,
	    program_modules->len * sizeof(gpointer));
}

static void
gnome_program_class_init (GnomeProgramClass *class)
{
    GObjectClass *object_class;

    object_class = (GObjectClass*) class;
    parent_class = g_type_class_peek_parent (class);

    quark_set_prop = g_quark_from_static_string ("gnome-program-set-property");
    quark_get_prop = g_quark_from_static_string ("gnome-program-get-property");

    object_class->set_property = gnome_program_set_property;
    object_class->get_property = gnome_program_get_property;

    g_object_class_install_property
	(object_class,
	 PROP_POPT_TABLE,
	 g_param_spec_pointer (GNOME_PARAM_POPT_TABLE,
			       _("Popt Table"), 
			       _("The table of options for popt"),
			       (G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_POPT_FLAGS,
	 g_param_spec_int (GNOME_PARAM_POPT_FLAGS,
			   _("Popt Flags"), 
			   _("The flags to use for popt"),
			   G_MININT, G_MAXINT, 0,
			   (G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_POPT_CONTEXT,
	 g_param_spec_pointer (GNOME_PARAM_POPT_CONTEXT,
			      _("Popt Context"), 
			      _("The popt context pointer that GnomeProgram "
				"is using"),
			       (G_PARAM_READABLE)));

    g_object_class_install_property
	(object_class,
	 PROP_HUMAN_READABLE_NAME,
	 g_param_spec_string (GNOME_PARAM_HUMAN_READABLE_NAME,
			      _("Human readable name"), 
			      _("Human readable name of this application"),
			      NULL,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_GNOME_PATH,
	 g_param_spec_string (GNOME_PARAM_GNOME_PATH, 
			      _("GNOME path"), 
			      _("Path in which to look for installed files"),
			      g_getenv ("GNOME2_PATH"),
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_APP_ID,
	 g_param_spec_string (GNOME_PARAM_APP_ID,
			      _("App ID"), 
			      _("ID string to use for this application"),
			      NULL, G_PARAM_READABLE));

    g_object_class_install_property
	(object_class,
	 PROP_APP_VERSION,
	 g_param_spec_string (GNOME_PARAM_APP_VERSION, 
			      _("App version"), 
			      _("Version of this application"),
			      NULL, G_PARAM_READABLE));

    g_object_class_install_property
	(object_class,
	 PROP_GNOME_PREFIX,
	 g_param_spec_string (GNOME_PARAM_GNOME_PREFIX,
			      _("GNOME Prefix"), 
			      _("Prefix where GNOME was installed"),
			      LIBGNOME_PREFIX,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_GNOME_LIBDIR,
	 g_param_spec_string (GNOME_PARAM_GNOME_LIBDIR,
			      _("GNOME Libdir"), 
			      _("Library prefix where GNOME was installed"),
			      LIBGNOME_LIBDIR,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_GNOME_DATADIR,
	 g_param_spec_string (GNOME_PARAM_GNOME_DATADIR,
			      _("GNOME Datadir"), 
			      _("Data prefix where GNOME was installed"),
			      LIBGNOME_DATADIR,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_GNOME_SYSCONFDIR,
	 g_param_spec_string (GNOME_PARAM_GNOME_SYSCONFDIR,
			      _("GNOME Sysconfdir"), 
			      _("Configuration prefix where GNOME "
				"was installed"),
			      LIBGNOME_SYSCONFDIR,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_APP_PREFIX,
	 g_param_spec_string (GNOME_PARAM_APP_PREFIX,
			      _("GNOME App Prefix"), 
			      _("Prefix where this application was installed"),
			      NULL,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE)));

    g_object_class_install_property
	(object_class,
	 PROP_APP_LIBDIR,
	 g_param_spec_string (GNOME_PARAM_APP_LIBDIR,
			      _("GNOME App Libdir"), 
			      _("Library prefix where this application "
				"was installed"),
			      NULL,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE)));

    g_object_class_install_property
	(object_class,
	 PROP_APP_DATADIR,
	 g_param_spec_string (GNOME_PARAM_APP_DATADIR,
			      _("GNOME App Datadir"), 
			      _("Data prefix where this application "
				"was installed"),
			      NULL,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE)));

    g_object_class_install_property
	(object_class,
	 PROP_APP_SYSCONFDIR,
	 g_param_spec_string (GNOME_PARAM_APP_SYSCONFDIR,
			      _("GNOME App Sysconfdir"), 
			      _("Configuration prefix where this application "
				"was installed"),
			      NULL,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE)));

    g_object_class_install_property
	(object_class,
	 PROP_CREATE_DIRECTORIES,
	 g_param_spec_boolean (GNOME_PARAM_CREATE_DIRECTORIES,
			      _("Create Directories"), 
			      _("Create standard GNOME directories on startup"),
			       TRUE,
			       (G_PARAM_READABLE | G_PARAM_WRITABLE |
				G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_ENABLE_SOUND,
	 g_param_spec_boolean (GNOME_PARAM_ENABLE_SOUND,
			      _("Enable Sound"), 
			      _("Enable sound on startup"),
			       TRUE,
			       (G_PARAM_READABLE | G_PARAM_WRITABLE |
				G_PARAM_CONSTRUCT_ONLY)));

    g_object_class_install_property
	(object_class,
	 PROP_ESPEAKER,
	 g_param_spec_string (GNOME_PARAM_ESPEAKER,
			      _("Espeaker"), 
			      _("How to connect to esd"),
			      NULL,
			      (G_PARAM_READABLE | G_PARAM_WRITABLE |
			       G_PARAM_CONSTRUCT_ONLY)));

    object_class->finalize  = gnome_program_finalize;
}

static void
gnome_program_instance_init (GnomeProgram *program)
{
    guint i;

    program->_priv = g_new0 (GnomeProgramPrivate, 1);

    program->_priv->state = APP_CREATE_DONE;

    for (i = 0; i < program_modules->len; i++) {
	GnomeModuleInfo *a_module = g_ptr_array_index (program_modules, i);

	if (a_module && a_module->instance_init) {
#ifdef TIME_INIT
	    GTimer *timer = g_timer_new ();
	    g_timer_start (timer);
	    g_print ("Running class_init for: %s ...", a_module->name); 
#endif
	    a_module->instance_init (program, a_module);
#ifdef TIME_INIT
	    g_timer_stop (timer);
	    g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
	    g_timer_destroy (timer);
#endif
	}
    }
}

static void
gnome_program_finalize (GObject* object)
{
	GnomeProgram *self = GNOME_PROGRAM (object);

	/* no free */
	self->_priv->prop_popt_table = NULL;

	g_free (self->_priv->prop_human_readable_name);
	self->_priv->prop_human_readable_name = NULL;
	g_free (self->_priv->prop_gnome_prefix);
	self->_priv->prop_gnome_prefix = NULL;
	g_free (self->_priv->prop_gnome_libdir);
	self->_priv->prop_gnome_libdir = NULL;
	g_free (self->_priv->prop_gnome_sysconfdir);
	self->_priv->prop_gnome_sysconfdir = NULL;
	g_free (self->_priv->prop_gnome_datadir);
	self->_priv->prop_gnome_datadir = NULL;
	g_free (self->_priv->prop_app_prefix);
	self->_priv->prop_app_prefix = NULL;
	g_free (self->_priv->prop_app_libdir);
	self->_priv->prop_app_libdir = NULL;
	g_free (self->_priv->prop_app_sysconfdir);
	self->_priv->prop_app_sysconfdir = NULL;
	g_free (self->_priv->prop_app_datadir);
	self->_priv->prop_app_datadir = NULL;
	g_free (self->_priv->prop_espeaker);
	self->_priv->prop_espeaker = NULL;

	g_strfreev (self->_priv->gnome_path);
	self->_priv->gnome_path = NULL;

	g_free (self->_priv->app_id);
	self->_priv->app_id = NULL;
	g_free (self->_priv->app_version);
	self->_priv->app_version = NULL;

	g_strfreev (self->_priv->argv);
	self->_priv->argv = NULL;

	if (self->_priv->arg_context != NULL) {
		poptFreeContext (self->_priv->arg_context);
		g_dataset_destroy (self->_priv->arg_context);
	}
	self->_priv->arg_context = NULL;

	if (self->_priv->top_options_table != NULL)
		g_array_free (self->_priv->top_options_table, TRUE);
	self->_priv->top_options_table = NULL;

	g_free (self->_priv);
	self->_priv = NULL;

	GNOME_CALL_PARENT (G_OBJECT_CLASS, finalize, (object));
}

static gpointer
gnome_module_info_copy (gpointer boxed)
{
    return g_memdup (boxed, sizeof (GnomeModuleInfo));
}

static void
gnome_module_info_free (gpointer boxed)
{
    g_free (boxed);
}

GType
gnome_module_info_get_type (void)
{
    static GType module_info_type = 0;

    if (!module_info_type)
	module_info_type = g_boxed_type_register_static	("GnomeModuleInfo",
		 gnome_module_info_copy, gnome_module_info_free);


    return module_info_type;
}

/**
 * gnome_program_get:
 *
 * Retrieves an object that stored information about the application's state.
 * Other functions assume this will always return a #GnomeProgram object which
 * (if not %NULL) has already been initialized.
 *
 * Returns: The application's #GnomeProgram instance, or %NULL if it does not
 * exist.
 */

GnomeProgram *
gnome_program_get (void)
{
    return global_program;
}

/**
 * gnome_program_get_app_id
 * @program: The program object
 *
 * Description:
 * This function returns a pointer to a static string that the
 * application has provided as an identifier. This is not meant as a
 * human-readable identifier so much as a unique identifier for
 * programs and libraries.
 *
 * Returns: Application ID string.
 */
const char *
gnome_program_get_app_id (GnomeProgram *program)
{
    g_return_val_if_fail (program != NULL, NULL);
    g_return_val_if_fail (GNOME_IS_PROGRAM (program), NULL);
    g_return_val_if_fail (program->_priv->state >= APP_PREINIT_DONE, NULL);

    return program->_priv->app_id;
}

/**
 * gnome_program_get_app_version
 * @program: The application object
 *
 * Description:
 * This function returns a pointer to a static string that the
 * application has provided as a version number. This is not meant as a
 * human-readable identifier so much as a unique identifier for
 * programs and libraries.
 *
 * Returns: Application version string.
 */
const char *
gnome_program_get_app_version (GnomeProgram *program)
{
    g_return_val_if_fail (program != NULL, NULL);
    g_return_val_if_fail (GNOME_IS_PROGRAM (program), NULL);
    g_return_val_if_fail (program->_priv->state >= APP_PREINIT_DONE, NULL);

    return program->_priv->app_version;
}

/**
 * gnome_program_get_human_readable_name
 * @program: The application object
 *
 * Description:
 * This function returns a pointer to a static string that the
 * application has provided as a human readable name. The app
 * should provide the name with the #GNOME_PARAM_HUMAN_READABLE_NAME
 * init argument. Returns %NULL if no name was set.
 *
 * Returns: Application human-readable name string.
 */
const char *
gnome_program_get_human_readable_name (GnomeProgram *program)
{
    g_return_val_if_fail (program != NULL, NULL);
    g_return_val_if_fail (GNOME_IS_PROGRAM (program), NULL);
    g_return_val_if_fail (program->_priv->state >= APP_PREINIT_DONE, NULL);

    if (program->_priv->prop_human_readable_name == NULL)
      return g_get_prgname ();
    
    return program->_priv->prop_human_readable_name;
}

/**
 * gnome_program_install_property:
 * @pclass: A #GnomeProgramClass.
 * @get_fn: A function to get property values.
 * @set_fn: A function to set property values.
 * @pspec: A collection of properties.
 *
 * Install a collection of available properties, their default values and the
 * functions to set and retrieve these properties.
 *
 * Normal applications will never need to call this function, it is mostly for
 * use by other platform library authors.
 *
 * Returns: The number of properties installed.
 */
guint
gnome_program_install_property (GnomeProgramClass *pclass,
				GObjectGetPropertyFunc get_fn,
				GObjectSetPropertyFunc set_fn,
				GParamSpec *pspec)
{
    g_return_val_if_fail (pclass != NULL, -1);
    g_return_val_if_fail (GNOME_IS_PROGRAM_CLASS (pclass), -1);
    g_return_val_if_fail (pspec != NULL, -1);

    g_param_spec_set_qdata (pspec, quark_get_prop, (gpointer)get_fn);
    g_param_spec_set_qdata (pspec, quark_set_prop, (gpointer)set_fn);

    g_object_class_install_property (G_OBJECT_CLASS (pclass),
				     last_property_id, pspec);

    return last_property_id++;
}

/**
 * gnome_program_locate_file:
 * @program: A valid #GnomeProgram object or %NULL (in which case the current
 * application is used).
 * @domain: A #GnomeFileDomain.
 * @file_name: A file name or path inside the 'domain' to find.
 * @only_if_exists: Only return a full pathname if the specified file
 *                  actually exists
 * @ret_locations: If this is not %NULL, a list of all the possible locations
 *                 of the file will be returned.
 *
 * This function finds a full path to the @file_name located in the specified
 * "domain". A domain is a name for a collection of related files.
 * For example, common domains are "libdir", "pixmap", and "config".
 *
 * If @ret_locations is %NULL, only one pathname is returned. Otherwise,
 * alternative paths are returned in @ret_locations.
 *
 * User applications should store files in the GNOME_FILE_DOMAIN_APP_*
 * domains. However you MUST set the correct attributes for #GnomeProgram for
 * the APP specific prefixes (during the initialization part of the
 * application).
 *
 * The @ret_locations list and its contents should be freed by the caller, as
 * should the returned string.
 *
 * Returns: The full path to the file (if it exists or only_if_exists is
 *          %FALSE) or %NULL.
 */
gchar *
gnome_program_locate_file (GnomeProgram *program, GnomeFileDomain domain,
			   const gchar *file_name, gboolean only_if_exists,
			   GSList **ret_locations)
{
    gchar *prefix_rel = NULL, *attr_name = NULL, *attr_rel = NULL;
    gchar fnbuf [PATH_MAX], *retval = NULL, **ptr;
    gboolean search_path = TRUE;
    GValue value = { 0, };
    int len;

    if (program == NULL)
	program = gnome_program_get ();

    g_return_val_if_fail (program != NULL, NULL);
    g_return_val_if_fail (GNOME_IS_PROGRAM (program), NULL);
    g_return_val_if_fail (program->_priv->state >= APP_PREINIT_DONE, NULL);
    g_return_val_if_fail (file_name != NULL, NULL);

#define ADD_FILENAME(x) { \
	if (x != NULL) { \
		if (ret_locations != NULL) \
			*ret_locations = g_slist_append (*ret_locations, g_strdup (x)); \
		if (retval == NULL && ret_locations == NULL) \
			retval = g_strdup (x); \
	} \
}

    /* Potentially add an absolute path */
    if (g_path_is_absolute (file_name))
      {
        if (!only_if_exists || g_file_test (file_name, G_FILE_TEST_EXISTS))
          ADD_FILENAME (file_name);
      }
    
    switch (domain) {
    case GNOME_FILE_DOMAIN_LIBDIR:
	prefix_rel = "/lib";
	attr_name = GNOME_PARAM_GNOME_LIBDIR;
	attr_rel = "";
	break;
    case GNOME_FILE_DOMAIN_DATADIR:
	prefix_rel = "/share";
	attr_name = GNOME_PARAM_GNOME_DATADIR;
	attr_rel = "";
	break;
    case GNOME_FILE_DOMAIN_SOUND:
	prefix_rel = "/share/sounds";
	attr_name = GNOME_PARAM_GNOME_DATADIR;
	attr_rel = "/sounds";
	break;
    case GNOME_FILE_DOMAIN_PIXMAP:
	prefix_rel = "/share/pixmaps";
	attr_name = GNOME_PARAM_GNOME_DATADIR;
	attr_rel = "/pixmaps";
	break;
    case GNOME_FILE_DOMAIN_CONFIG:
	prefix_rel = "/etc";
	attr_name = GNOME_PARAM_GNOME_SYSCONFDIR;
	attr_rel = "";
	break;
    case GNOME_FILE_DOMAIN_HELP:
	prefix_rel = "/share/gnome/help";
	attr_name = GNOME_PARAM_GNOME_DATADIR;
	attr_rel = "/gnome/help";
	break;
    case GNOME_FILE_DOMAIN_APP_LIBDIR:
	prefix_rel = "/lib";
	attr_name = GNOME_PARAM_APP_LIBDIR;
	attr_rel = "";
	search_path = FALSE;
	break;
    case GNOME_FILE_DOMAIN_APP_DATADIR:
	prefix_rel = "/share";
	attr_name = GNOME_PARAM_APP_DATADIR;
	attr_rel = "";
	search_path = FALSE;
	break;
    case GNOME_FILE_DOMAIN_APP_SOUND:
	prefix_rel = "/share/sounds";
	attr_name = GNOME_PARAM_APP_DATADIR;
	attr_rel = "/sounds";
	search_path = FALSE;
	break;
    case GNOME_FILE_DOMAIN_APP_PIXMAP:
	prefix_rel = "/share/pixmaps";
	attr_name = GNOME_PARAM_APP_DATADIR;
	attr_rel = "/pixmaps";
	search_path = FALSE;
	break;
    case GNOME_FILE_DOMAIN_APP_CONFIG:
	prefix_rel = "/etc";
	attr_name = GNOME_PARAM_APP_SYSCONFDIR;
	attr_rel = "";
	search_path = FALSE;
	break;
    case GNOME_FILE_DOMAIN_APP_HELP:
	len = strlen ("/share/gnome/help/") + 
		strlen (program->_priv->app_id) + 1;
	prefix_rel = g_alloca (len);
	if (prefix_rel == NULL /* bad things */)
		return NULL;
	g_snprintf (prefix_rel, len, "/share/gnome/help/%s", program->_priv->app_id);

	attr_name = GNOME_PARAM_APP_DATADIR;

	len = strlen ("/gnome/help/") + 
		strlen (program->_priv->app_id) + 1;
	attr_rel = g_alloca (len);
	if (attr_rel == NULL /* bad things */)
		return NULL;
	g_snprintf (attr_rel, len, "/gnome/help/%s", program->_priv->app_id);

	search_path = FALSE;
	break;
    default:
	g_warning (G_STRLOC ": unknown file domain %d", domain);
	return NULL;
    }

    if (attr_name != NULL) {
	gchar *dir;

	g_value_init (&value, G_TYPE_STRING);
	g_object_get_property (G_OBJECT (program), attr_name, &value);
	dir = g_value_dup_string (&value);
	g_value_unset (&value);

	/* use the prefix */
	if (dir == NULL) {
		g_warning (G_STRLOC ": Directory properties not set correctly.  "
			   "Cannot locate application specific files.");
		return NULL;
	}

	if (dir != NULL) {
	    g_snprintf (fnbuf, sizeof (fnbuf), "%s%s/%s",
			dir, attr_rel, file_name);

	    g_free (dir);
	    if (!only_if_exists || g_file_test (fnbuf, G_FILE_TEST_EXISTS))
		ADD_FILENAME (fnbuf);
	}
    }
    if (retval != NULL && ret_locations == NULL)
	goto out;

    /* Now check the GNOME_PATH. */
    for (ptr = program->_priv->gnome_path; search_path && ptr && *ptr; ptr++) {
	g_snprintf (fnbuf, sizeof (fnbuf), "%s%s/%s",
		    *ptr, prefix_rel, file_name);

	if (!only_if_exists || g_file_test (fnbuf, G_FILE_TEST_EXISTS))
	    ADD_FILENAME (fnbuf);
    }
    if (retval && !ret_locations)
	goto out;

#undef ADD_FILENAME

 out:
    return retval;
}

/******** modules *******/

/* Stolen verbatim from rpm/lib/misc.c 
   RPM is Copyright (c) 1998 by Red Hat Software, Inc.,
   and may be distributed under the terms of the GPL and LGPL.
*/
/* compare alpha and numeric segments of two versions */
/* return 1: a is newer than b */
/*        0: a and b are the same version */
/*       -1: b is newer than a */
static int rpmvercmp(const char * a, const char * b) {
    char oldch1, oldch2;
    char * str1, * str2;
    char * one, * two;
    int rc;
    int isnum;
    
    /* easy comparison to see if versions are identical */
    if (!strcmp(a, b)) return 0;

    str1 = g_alloca(strlen(a) + 1);
    str2 = g_alloca(strlen(b) + 1);

    strcpy(str1, a);
    strcpy(str2, b);

    one = str1;
    two = str2;

    /* loop through each version segment of str1 and str2 and compare them */
    while (*one && *two) {
	while (*one && !g_ascii_isalnum(*one)) one++;
	while (*two && !g_ascii_isalnum(*two)) two++;

	str1 = one;
	str2 = two;

	/* grab first completely alpha or completely numeric segment */
	/* leave one and two pointing to the start of the alpha or numeric */
	/* segment and walk str1 and str2 to end of segment */
	if (g_ascii_isdigit(*str1)) {
	    while (*str1 && g_ascii_isdigit(*str1)) str1++;
	    while (*str2 && g_ascii_isdigit(*str2)) str2++;
	    isnum = 1;
	} else {
	    while (*str1 && g_ascii_isalpha(*str1)) str1++;
	    while (*str2 && g_ascii_isalpha(*str2)) str2++;
	    isnum = 0;
	}
		
	/* save character at the end of the alpha or numeric segment */
	/* so that they can be restored after the comparison */
	oldch1 = *str1;
	*str1 = '\0';
	oldch2 = *str2;
	*str2 = '\0';

	/* take care of the case where the two version segments are */
	/* different types: one numeric and one alpha */
	if (one == str1) return -1;	/* arbitrary */
	if (two == str2) return -1;

	if (isnum) {
	    /* this used to be done by converting the digit segments */
	    /* to ints using atoi() - it's changed because long  */
	    /* digit segments can overflow an int - this should fix that. */
	  
	    /* throw away any leading zeros - it's a number, right? */
	    while (*one == '0') one++;
	    while (*two == '0') two++;

	    /* whichever number has more digits wins */
	    if (strlen(one) > strlen(two)) return 1;
	    if (strlen(two) > strlen(one)) return -1;
	}

	/* strcmp will return which one is greater - even if the two */
	/* segments are alpha or if they are numeric.  don't return  */
	/* if they are equal because there might be more segments to */
	/* compare */
	rc = strcmp(one, two);
	if (rc) return rc;
	
	/* restore character that was replaced by null above */
	*str1 = oldch1;
	one = str1;
	*str2 = oldch2;
	two = str2;
    }

    /* this catches the case where all numeric and alpha segments have */
    /* compared identically but the segment sepparating characters were */
    /* different */
    if ((!*one) && (!*two)) return 0;

    /* whichever version still has characters left over wins */
    if (!*one) return -1; else return 1;
}

static gboolean
gnome_program_version_check (const char *required_version,
			     const char *provided_version)
{
    if (required_version && provided_version)
	return (rpmvercmp (provided_version, required_version) >= 0);
    else
	return TRUE;
}

/**
 * gnome_program_module_registered:
 * @module_info: A pointer to a GnomeModuleInfo structure describing the module
 *               to be queried
 *
 * Description: This method checks to see whether a specific module has been
 *              initialized in the specified program.
 *
 * Returns: A value indicating whether the specified module has been
 *          registered/initialized in the current program
 */
gboolean
gnome_program_module_registered (const GnomeModuleInfo *module_info)
{
    int i;
    GnomeModuleInfo *curmod;

    g_return_val_if_fail (module_info, FALSE);

    if (!program_modules)
	    return FALSE;

    for(i = 0; i < program_modules->len; i++) {
	curmod = g_ptr_array_index (program_modules, i);

	/* array is NULL-terminated, so break on NULL */
	if (curmod == NULL)
		break;
	
	if (curmod == module_info)
	    return TRUE;
    }

    return FALSE;
}

/**
 * gnome_program_module_register:
 * @module_info: A pointer to a GnomeModuleInfo structure describing the module
 *               to be initialized
 *
 * Description:
 * This function is used to register a module to be initialized by the
 * GNOME library framework. The memory pointed to by @module_info must be
 * valid during the whole application initialization process, and the module
 * described by @module_info must only use the @module_info pointer to
 * register itself.
 *
 */
void
gnome_program_module_register (const GnomeModuleInfo *module_info)
{
    int i;

    g_return_if_fail (module_info);

    if (program_initialized) {
	g_warning (G_STRLOC ": cannot load modules after program is initialized");
	return;
    }

    /* Check that it's not already registered. */

    if (gnome_program_module_registered (module_info))
	return;

    if (!program_modules)
	program_modules = g_ptr_array_new();

    /* if the last entry is NULL, stick it there instead */
    if (program_modules->len > 0 &&
	g_ptr_array_index (program_modules, program_modules->len - 1) == NULL) {
	    g_ptr_array_index (program_modules, program_modules->len - 1) =
		    (GnomeModuleInfo *)module_info;
    } else {
	    g_ptr_array_add (program_modules, (GnomeModuleInfo *)module_info);
    }
    /* keep array NULL terminated */
    g_ptr_array_add (program_modules, NULL);

    /* We register requirements *after* the module itself to avoid loops.
       Initialization order gets sorted out later on. */
    if (module_info->requirements) {
	for(i = 0; module_info->requirements[i].required_version; i++) {
	    const GnomeModuleInfo *dep_mod;

	    dep_mod = module_info->requirements[i].module_info;
	    if (gnome_program_version_check (module_info->requirements[i].required_version,
					     dep_mod->version))
		gnome_program_module_register (dep_mod);
	    else
		/* The required version is not installed */
		/* I18N needed */
		g_error ("Module '%s' requires version '%s' of module '%s' "
			 "to be installed, and you only have version '%s' of '%s'. "
			 "Aborting application.",
			 module_info->name,
			 module_info->requirements[i].required_version,
			 dep_mod->name,
			 dep_mod->version,
			 dep_mod->name);
	}
    }
}

static void
set_context_data (poptContext con,
		  enum poptCallbackReason reason,
		  const struct poptOption * opt,
		  const char * arg, void * data)
{
	GnomeProgram *program = data;

	if (reason == POPT_CALLBACK_REASON_PRE)
		g_dataset_set_data (con, "GnomeProgram", program);
}


/**
 * gnome_program_preinit:
 * @program: Application object
 * @app_id: application ID string
 * @app_version: application version string
 * @argc: The number of commmand line arguments contained in 'argv'
 * @argv: A string array of command line arguments
 *
 * Description:
 * This function performs the portion of application initialization that
 * needs to be done prior to command line argument parsing. The poptContext
 * returned can be used for getopt()-style option processing.
 *
 * Returns: A poptContext representing the argument parsing state.
 */
poptContext
gnome_program_preinit (GnomeProgram *program,
		       const char *app_id, const char *app_version,
		       int argc, char **argv)
{
    GnomeModuleInfo *a_module;
    poptContext argctx;
    int i;

    g_return_val_if_fail (program != NULL, NULL);
    g_return_val_if_fail (GNOME_IS_PROGRAM (program), NULL);
    g_return_val_if_fail (argv != NULL, NULL);

    if (program->_priv->state != APP_CREATE_DONE)
	return NULL;

    /* Store invocation name */
    g_set_prgname (argv[0]);
    
    /* 0. Misc setup */
    if (program->_priv->app_id)
	g_free (program->_priv->app_id);
    program->_priv->app_id = g_strdup (app_id);
    g_set_prgname (app_id);
    if (program->_priv->app_version)
	g_free (program->_priv->app_version);
    program->_priv->app_version = g_strdup (app_version);
    program->_priv->argc = argc;

    /* Make a copy of argv, the thing is that while we know the
     * original argv will live until the end of the program,
     * there are those evil people out there that modify it.
     * Also, this may be some other argv, think 'fake argv' here */
    program->_priv->argv = g_new (char *, argc + 1);
    memcpy (program->_priv->argv, argv, sizeof (char *) * argc);
    program->_priv->argv[argc] = NULL;

    if (!program_modules) {
	program_modules = g_ptr_array_new();
	/* keep array NULL terminated */
	g_ptr_array_add (program_modules, NULL);
    }

    /* Major steps in this function:
       1. Process all framework attributes in 'attrs'
       2. Order the module list for dependencies
       3. Call the preinit functions for the modules
       4. Process other attributes 
       5. Create a top-level 'struct poptOption *' for use in arg-parsing.
       6. Create a poptContext
       7. Cleanup/return
    */

    /* 3. call the pre-init functions */
    for (i = 0; (a_module = g_ptr_array_index (program_modules, i)); i++) {
	if (a_module->pre_args_parse) {
#ifdef TIME_INIT
	    GTimer *timer = g_timer_new ();
	    g_timer_start (timer);
	    g_print ("Running pre_args_parse for: %s ...", a_module->name); 
#endif
	    a_module->pre_args_parse (program, a_module);
#ifdef TIME_INIT
	    g_timer_stop (timer);
	    g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
	    g_timer_destroy (timer);
#endif
	}
    }

    /* 5. Create a top-level 'struct poptOption *' for use in arg-parsing. */
    {
	struct poptOption includer = {NULL, '\0', POPT_ARG_INCLUDE_TABLE,
				      NULL, 0, NULL, NULL};
	struct poptOption callback =
		{ NULL, '\0', POPT_ARG_CALLBACK | POPT_CBFLAG_PRE,
		  &set_context_data, 0,
		  /* GOD THIS IS EVIL!  But popt is so UTERLY FUCKED IT'S
		   * NOT EVEN FUNNY.  For some reason it passes 'descrip'
		   * as 'data' to the callback, and there is no other way
		   * to pass data.  Fun, eh? */
		  (const char *)program,
		  NULL };

	program->_priv->top_options_table = g_array_new
	    (TRUE, TRUE, sizeof (struct poptOption));

	g_array_append_val (program->_priv->top_options_table, callback);

	/* Put the special popt table in first */
	includer.arg = poptHelpOptions;
	includer.descrip = N_("Help options");
	g_array_append_val (program->_priv->top_options_table, includer);

	if (program->_priv->prop_popt_table) {
	    includer.arg = program->_priv->prop_popt_table;
	    includer.descrip = N_("Application options");
	    g_array_append_val (program->_priv->top_options_table,
				includer);
	}

	for (i = 0; (a_module = g_ptr_array_index(program_modules, i)); i++) {
	    if (a_module->options) {
		includer.arg = a_module->options;
		includer.descrip = (char *)a_module->description;

		g_array_append_val (program->_priv->top_options_table, includer);
	    }
	}

	includer.longName = "load-modules";
	includer.argInfo = POPT_ARG_STRING;
	includer.descrip = N_("Dynamic modules to load");
	includer.argDescrip = N_("MODULE1,MODULE2,...");
	g_array_append_val (program->_priv->top_options_table, includer);
    }

    argctx = program->_priv->arg_context = poptGetContext
	(program->_priv->app_id, argc, (const char **) argv,
	 (struct poptOption *) program->_priv->top_options_table->data,
	 program->_priv->prop_popt_flags);
  
    /* 7. Cleanup/return */
    program->_priv->state = APP_PREINIT_DONE;

    return argctx;
}

/**
 * gnome_program_module_load:
 * @mod_name: module name
 *
 * Loads a shared library that contains a
 * #GnomeModuleInfo dynamic_module_info structure.
 *
 * Returns: The #GnomeModuleInfo structure that was loaded, or %NULL if the
 * module could not be loaded.
 */
const GnomeModuleInfo *
gnome_program_module_load (const char *mod_name)
{
    GModule *mh;
    const GnomeModuleInfo *gmi;
    char tbuf[1024];

    g_return_val_if_fail (mod_name != NULL, NULL);

    g_snprintf (tbuf, sizeof(tbuf), "lib%s.so.0", mod_name);

    mh = g_module_open (mod_name, G_MODULE_BIND_LAZY);
    if(!mh) {
	g_snprintf (tbuf, sizeof(tbuf), "lib%s.so", mod_name);

	mh = g_module_open (mod_name, G_MODULE_BIND_LAZY);
    }

    if (!mh)
	return NULL;

    if (g_module_symbol (mh, "dynamic_module_info", (gpointer *)&gmi)) {
	gnome_program_module_register (gmi);
	g_module_make_resident (mh);
	return gmi;
    } else {
	g_module_close (mh);
	return NULL;
    }
}

/**
 * gnome_program_parse_args:
 * @program: Application object
 *
 * Description: Parses the command line arguments for the application
 */
void
gnome_program_parse_args (GnomeProgram *program)
{
    int nextopt;
    poptContext ctx;

    g_return_if_fail (program != NULL);
    g_return_if_fail (GNOME_IS_PROGRAM (program));

    if (program->_priv->state != APP_PREINIT_DONE)
	return;

    ctx = program->_priv->arg_context;
    while ((nextopt = poptGetNextOpt (ctx)) > 0 || nextopt == POPT_ERROR_BADOPT)
	/* do nothing */ ;

    if (nextopt != -1) {
	g_print ("Error on option %s: %s.\nRun '%s --help' to see a full list of available command line options.\n",
		 poptBadOption (ctx, 0),
		 poptStrerror (nextopt),
		 program->_priv->argv[0]);
	exit (1);
    }
}

static char *
find_accessibility_module (GnomeProgram *program, const char *libname)
{
	char *sub;
	char *path;
	char *fname;
	char *retval;

	fname = g_strconcat (libname, "." G_MODULE_SUFFIX, NULL);
	sub = g_strconcat ("gtk-2.0/modules", G_DIR_SEPARATOR_S, fname, NULL);

	path = gnome_program_locate_file (
		program, GNOME_FILE_DOMAIN_LIBDIR, sub, TRUE, NULL);

	g_free (sub);

	if (path)
		retval = path;
	else
		retval = gnome_program_locate_file (
			program, GNOME_FILE_DOMAIN_LIBDIR,
			fname, TRUE, NULL);

	g_free (fname);

	return retval;
}

static gboolean
accessibility_invoke_module (GnomeProgram *program,
			     const char   *libname,
			     gboolean      init)
{
	GModule    *handle;
	void      (*invoke_fn) (void);
	const char *method;
	gboolean    retval = FALSE;
	char       *module_name;

	if (init)
		method = "gnome_accessibility_module_init";
	else
		method = "gnome_accessibility_module_shutdown";

	module_name = find_accessibility_module (program, libname);

	if (!module_name) {
		g_warning ("Accessibility: failed to find module '%s' which "
			   "is needed to make this application accessible",
			   libname);

	} else if (!(handle = g_module_open (module_name, G_MODULE_BIND_LAZY))) {
		g_warning ("Accessibility: failed to load module '%s': '%s'",
			   libname, g_module_error ());

	} else if (!g_module_symbol (handle, method, (gpointer *)&invoke_fn)) {
		g_warning ("Accessibility: error library '%s' does not include "
			   "method '%s' required for accessibility support",
			   libname, method);
		g_module_close (handle);

	} else {
		retval = TRUE;
		invoke_fn ();
	}

	g_free (module_name);

	return retval;
}

static gboolean
accessibility_invoke (GnomeProgram *program, gboolean init)
{
	GSList *l;
	gboolean use_gui = FALSE;

	if (!program->_priv->accessibility_modules)
		return FALSE;

	for (l = program->_priv->accessibility_modules; l; l = l->next) {
		GnomeModuleInfo *module = l->data;
		
		if (!strcmp (module->name, "gtk")) {
			accessibility_invoke_module (program, "libgail", init);
			use_gui = TRUE;

		} else if (!strcmp (module->name, "libgnomeui")) {
			accessibility_invoke_module (program, "libgail-gnome", init);
			use_gui = TRUE;
		}
	}

	if (use_gui) {
		accessibility_invoke_module (program, "libatk-bridge", init);
	}

	return TRUE;
}


static void
accessibility_init (GnomeProgram *program)
{
	int i;
	gboolean do_init;
	const char *env_var;
	GSList *list = NULL;

	/* Seek the module list we need */
	for (i = 0; i < program_modules->len; i++) {
		GnomeModuleInfo *module = g_ptr_array_index (program_modules, i);
		
		if (!module)
			continue;

		if (!strcmp (module->name, "gtk"))
			list = g_slist_prepend (list, module);

		else if (!strcmp (module->name, "libgnomeui"))
			list = g_slist_prepend (list, module);
	}

	program->_priv->accessibility_modules = list;
	
	do_init = FALSE;

	if ((env_var = g_getenv (GNOME_ACCESSIBILITY_ENV)))
		do_init = atoi (env_var);
	else
		do_init = gconf_client_get_bool (
			gconf_client_get_default (),
			GNOME_ACCESSIBILITY_KEY, NULL);

	if (do_init)
		accessibility_invoke (program, TRUE);
}

/**
 * gnome_program_postinit:
 * @program: Application object
 *
 * Description: Called after gnome_program_parse_args(), this function
 * takes care of post-parse initialization and cleanup
 */
void
gnome_program_postinit (GnomeProgram *program)
{
    int i;
    GnomeModuleInfo *a_module;

    g_return_if_fail (program != NULL);
    g_return_if_fail (GNOME_IS_PROGRAM (program));

    if (program->_priv->state != APP_PREINIT_DONE)
	return;

    /* Call post-parse functions */
    for (i = 0; (a_module = g_ptr_array_index(program_modules, i)); i++) {
	if (a_module->post_args_parse) {
#ifdef TIME_INIT
	    GTimer *timer = g_timer_new ();
	    g_timer_start (timer);
	    g_print ("Running post_args_parse for: %s ...", a_module->name); 
#endif
	    a_module->post_args_parse (program, a_module);
#ifdef TIME_INIT
	    g_timer_stop (timer);
	    g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
	    g_timer_destroy (timer);
#endif
	}
    }

    /* Accessibility magic */
    accessibility_init (program);

    /* Free up stuff we don't need to keep around for the lifetime of the app */

    /* Note! we cannot kill the program_modules array as that
     * may be needed later */

    g_array_free (program->_priv->top_options_table, TRUE);
    program->_priv->top_options_table = NULL;

    g_blow_chunks(); /* Try to compact memory usage a bit */

    program->_priv->state = APP_POSTINIT_DONE;
}

/**
 * gnome_program_init:
 * @app_id: Application ID string.
 * @app_version: Application version string.
 * @module_info: The module to init with this program.
 * @argc: The number of commmand line arguments contained in @argv.
 * @argv: A string array of command line arguments.
 * @first_property_name: The first item in a %NULL-terminated list of attribute
 * name and value pairs (so this will be an attribute name or %NULL).
 * @...: The continuation of a %NULL-terminated list of attribute name/value
 * pairs.
 *
 * Initialises the current GNOME libraries for use by the application.
 *
 * Returns: A #GnomeProgram instance representing the current application.
 */
GnomeProgram *
gnome_program_init (const char *app_id, const char *app_version,
		    const GnomeModuleInfo *module_info,
		    int argc, char **argv,
		    const char *first_property_name, ...)
{
    GnomeProgram *program;
    va_list args;

    g_type_init ();

    va_start(args, first_property_name);
    program = gnome_program_initv (GNOME_TYPE_PROGRAM,
				   app_id, app_version, module_info,
				   argc, argv, first_property_name, args);
    va_end(args);

    return program;
}

/**
 * gnome_program_initv:
 * @type: The type of application to be initialized (usually
 * #GNOME_TYPE_PROGRAM).
 * @app_id: Application ID string.
 * @app_version: Application version string.
 * @module_info: The modules to init with the application.
 * @argc: The number of command line arguments contained in @argv.
 * @argv: A string array of command line arguments.
 * @first_property_name: The first item in a %NULL-terminated list of attribute
 * name/value.
 * @args: The remaining elements in the %NULL terminated list (of which
 * @first_property_name is the first element).
 *
 * Provides a non-varargs form of gnome_program_init(). Users will rarely need
 * to call this function directly.
 *
 * Returns: A #GnomeProgram instance representing the current application.
 */
GnomeProgram*
gnome_program_initv (GType type,
		     const char *app_id, const char *app_version,
		     const GnomeModuleInfo *module_info,
		     int argc, char **argv,
		     const char *first_property_name, va_list args)
{
    GnomeProgram *program;
    GnomeProgramClass *klass;
    int i;

#ifdef TIME_INIT
    GTimer *global_timer = g_timer_new ();

    g_timer_start (global_timer);
    g_print ("Starting gnome_program_init:\n\n");
#endif

    g_type_init ();

    klass = g_type_class_ref (type);

    if (!program_initialized) {
	const char *ctmp;
	const GnomeModuleInfo *libgnome_module;

	program_module_list = g_ptr_array_new ();
	program_modules = g_ptr_array_new ();

	/* keep array NULL terminated */
	g_ptr_array_add (program_modules, NULL);

	/* Register the requested modules. */
	gnome_program_module_register (module_info);

	/* 
	 * make sure libgnome is always registered.
	 */
	libgnome_module = libgnome_module_info_get ();
	if (!gnome_program_module_registered (libgnome_module))
		gnome_program_module_register (libgnome_module);

	/* Only load shlib modules and do all that other good
	 * stuff when not setuid/setgid, for obvious reasons */
	if (geteuid () == getuid () &&
	    getegid () == getgid ()) {
	    /* We have to handle --load-modules=foo,bar,baz specially */
	    for (i = 0; i < argc; i++) {
	        /* the --foo=bar format */
	        if (strncmp (argv[i], "--load-modules=", strlen ("--load-modules=")) == 0)
		    add_to_module_list (program_module_list, argv[i] + strlen("--load-modules="));
	        /* the --foo bar format */
	        if (strcmp (argv[i], "--load-modules") == 0 && i+1 < argc)
		    add_to_module_list (program_module_list, argv[i+1]);
	    }

	    ctmp = g_getenv ("GNOME_MODULES");
	    if (ctmp != NULL)
	        add_to_module_list (program_module_list, ctmp);
	}

	/*
	 * Load all the modules.
	 */
	for (i = 0; i < program_module_list->len; i++) {
	    gchar *modname = g_ptr_array_index (program_module_list, i);

	    gnome_program_module_load (modname);
	}

	for (i = 0; i < program_modules->len; i++) {
	    GnomeModuleInfo *a_module = g_ptr_array_index (program_modules, i);

	    if (a_module && a_module->init_pass) {
#ifdef TIME_INIT
	        GTimer *timer = g_timer_new ();
	        g_timer_start (timer);
	        g_print ("Running init_pass for: %s ...", a_module->name); 
#endif
		a_module->init_pass (a_module);
#ifdef TIME_INIT
		g_timer_stop (timer);
		g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
		g_timer_destroy (timer);
#endif
	    }
	}

	/* Order the module list for dependencies */
	gnome_program_module_list_order ();

	for (i = 0; i < program_modules->len; i++) {
	    GnomeModuleInfo *a_module = g_ptr_array_index (program_modules, i);

	    if (a_module && a_module->class_init) {
#ifdef TIME_INIT
	        GTimer *timer = g_timer_new ();
	        g_timer_start (timer);
	        g_print ("Running class_init for: %s ...", a_module->name); 
#endif
		a_module->class_init (klass, a_module);
#ifdef TIME_INIT
		g_timer_stop (timer);
		g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
		g_timer_destroy (timer);
#endif
	    }
	}
    } else if ( ! gnome_program_module_registered (module_info)) {
	/* Register the requested modules. */
	gnome_program_module_register (module_info);

	/* Init ALL modules, note that this runs the init over ALL modules
	 * even old ones.  Not really desirable, but unavoidable right now */
	for (i = 0; i < program_modules->len; i++) {
	    GnomeModuleInfo *a_module = g_ptr_array_index (program_modules, i);

	    if (a_module && a_module->init_pass) {
#ifdef TIME_INIT
	        GTimer *timer = g_timer_new ();
	        g_timer_start (timer);
	        g_print ("Running init_pass for: %s ...", a_module->name); 
#endif
		a_module->init_pass (a_module);
#ifdef TIME_INIT
		g_timer_stop (timer);
		g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
		g_timer_destroy (timer);
#endif
	    }
	}

	/* Order the module list for dependencies */
	gnome_program_module_list_order ();

	/* Same deal as for init_pass */
	for (i = 0; i < program_modules->len; i++) {
	    GnomeModuleInfo *a_module = g_ptr_array_index (program_modules, i);

	    if (a_module && a_module->class_init) {
#ifdef TIME_INIT
	        GTimer *timer = g_timer_new ();
	        g_timer_start (timer);
	        g_print ("Running class_init for: %s ...", a_module->name); 
#endif
		a_module->class_init (klass, a_module);
#ifdef TIME_INIT
		g_timer_stop (timer);
		g_print ("done (%f seconds)\n", g_timer_elapsed (timer, NULL));
		g_timer_destroy (timer);
#endif
	    }
	}
    }

    program = (GnomeProgram *)g_object_new_valist (type,
						   first_property_name, args);

    if (!program_initialized) {
	global_program = program;
	g_object_ref (G_OBJECT (global_program));

	program_initialized = TRUE;
    }

    gnome_program_preinit (program, app_id, app_version, argc, argv);
    gnome_program_parse_args (program);
    gnome_program_postinit (program);

#ifdef TIME_INIT
    g_timer_stop (global_timer);
    g_print ("\nGlobal init done in: %f seconds\n\n", g_timer_elapsed (global_timer, NULL));
    g_timer_destroy (global_timer);
#endif

    return program;
}
