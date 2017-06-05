/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */
/* choices.c - code for handling loading and saving of user choices */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>

#include "global.h"

#include "gui_support.h"

#include "choices.h"

static gboolean saving_disabled = TRUE;
static gchar **dir_list = NULL;
static gchar **xdg_dir_list = NULL;
static int     xdg_dir_count= 0 ;

static struct migration {
	const char *dir;
	const char *site;
	int symlink;
} to_migrate[]={
	{"ROX-Filer", SITE, TRUE},
	{"SendTo", SITE, TRUE},
	{"Templates", SITE, TRUE},
	{"MIME-types", SITE, TRUE},
	{"MIME-icons", SITE, TRUE},
	{"MIME-thumb", SITE, TRUE},

	{NULL, NULL, 0}
};

/* Static prototypes */
static gboolean exists(char *path);
static void migrate_choices(void);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/


/* Reads in CHOICESPATH and constructs the directory list table.
 * You must call this before using any other choices_* functions.
 *
 * If CHOICESPATH does not exist then a suitable default is used.
 */
void choices_init(void)
{
	char	*choices;
	const char *env;
	char **dirs;
	int i, n;

	g_return_if_fail(dir_list == NULL);

	/* Initialize old system */
	choices = getenv("CHOICESPATH");
	
	if (choices)
	{
		if (*choices != ':' && *choices != '\0')
			saving_disabled = FALSE;

		while (*choices == ':')
			choices++;

		if (*choices == '\0')
		{
			dir_list = g_new(char *, 1);
			dir_list[0] = NULL;
		}
		else
			dir_list = g_strsplit(choices, ":", 0);
	}
	else
	{
		saving_disabled = FALSE;
		
		dir_list = g_new(gchar *, 4);
		dir_list[0] = g_build_filename(g_get_home_dir(), "Choices",
					       NULL);
		dir_list[1] = g_strdup("/usr/local/share/Choices");
		dir_list[2] = g_strdup("/usr/share/Choices");
		dir_list[3] = NULL;
	}

	/* Initialize new system */
	env = getenv("XDG_CONFIG_DIRS");
	if (!env)
		env = "/etc/xdg";
	dirs = g_strsplit(env, ":", 0);
	g_return_if_fail(dirs != NULL);
	for (n = 0; dirs[n]; n++)
		;
	for (i = n; i > 0; i--)
		dirs[i] = dirs[i - 1];
	env = getenv("XDG_CONFIG_HOME");
	if (env)
		dirs[0] = g_strdup(env);
	else
		dirs[0] = g_build_filename(g_get_home_dir(), ".config", NULL);

	xdg_dir_list = dirs;
	xdg_dir_count = n + 1;
	
#if 0
	{
		gchar	**cdir = dir_list;

		for(i=0; i<xdg_dir_count; i++)
			g_print("[ XDG dir '%s' ]\n", xdg_dir_list[i]);

		while (*cdir)
		{
			g_print("[ choices dir '%s' ]\n", *cdir);
			cdir++;
		}

		g_print("[ saving is %s ]\n", saving_disabled ? "disabled"
							      : "enabled");
	}
#endif

}

/* If our XDG choices directory does not yet exist, offer to move the
 * old config over
 */
void choices_migrate(void)
{
	gchar *newpath;
	
	/* Attempt migration */
	newpath=choices_find_xdg_path_save(".", PROJECT, SITE, FALSE);
	if(!exists(newpath) && !saving_disabled)
		migrate_choices();
	g_free(newpath);
}

void choices_free_list(GPtrArray *list)
{
	guint	i;

	g_return_if_fail(list != NULL);

	for (i = 0; i < list->len; i++)
		g_free(g_ptr_array_index(list, i));

	g_ptr_array_free(list, TRUE);
}

/* Get the pathname of a choices file to load. Eg:
 *
 * choices_find_path_load("menus", "ROX-Filer")
 *		 		-> "/usr/local/share/Choices/ROX-Filer/menus".
 *
 * The return values may be NULL - use built-in defaults.
 * g_free() the result.
 */
static gchar *choices_find_path_load(const char *leaf, const char *dir)
{
	gchar	**cdir = dir_list;

	g_return_val_if_fail(dir_list != NULL, NULL);

	for (; *cdir; cdir++)
	{
		gchar	*path;

		path = g_build_filename(*cdir, dir, leaf, NULL);

		if (exists(path))
			return path;

		g_free(path);
	}

	return NULL;
}

/* Get the pathname of a choices file to load, using the XDG paths. Eg:
 *
 * choices_find_xdg_path_load("menus", "ROX-Filer", "rox.sourceforge.net")
 *		 		-> "/etc/xdg/rox.sourceforge.net/ROX-Filer/menus".
 *
 * Falls back on choices_find_path_load(leaf, dir) if it fails
 * The return values may be NULL - use built-in defaults.
 * g_free() the result.
 */
gchar *choices_find_xdg_path_load(const char *leaf, const char *dir,
				  const char *site)
{
	int i;

	g_return_val_if_fail(dir_list != NULL, NULL);

	for (i=0; i<xdg_dir_count; i++)
	{
		gchar	*path;

		if(site)
			path = g_build_filename(xdg_dir_list[i], site,
					   dir, leaf, NULL);
		else
			path = g_build_filename(xdg_dir_list[i], dir,
					   leaf, NULL);

		if (exists(path))
			return path;

		g_free(path);
	}

	return choices_find_path_load(leaf, dir);
}

/* Returns the pathname of a file to save to, or NULL if saving is
 * disabled. If 'create' is TRUE then intermediate directories will
 * be created (set this to FALSE if you just want to find out where
 * a saved file would go without actually altering the filesystem).
 *
 * g_free() the result.
 */
static gchar *choices_find_path_save(const char *leaf, const char *dir,
				gboolean create)
{
	gchar	*path, *retval;
	
	g_return_val_if_fail(dir_list != NULL, NULL);

	if (saving_disabled)
		return NULL;

	if (create && !exists(dir_list[0]))
	{
		if (mkdir(dir_list[0], 0777))
			g_warning("mkdir(%s): %s\n", dir_list[0],
					g_strerror(errno));
	}

	path = g_build_filename(dir_list[0], dir, NULL);
	if (create && !exists(path))
	{
		if (mkdir(path, 0777))
			g_warning("mkdir(%s): %s\n", path, g_strerror(errno));
	}

	retval = g_build_filename(path, leaf, NULL);
	g_free(path);

	return retval;
}

/* Returns the pathname of a file to save to, or NULL if saving is
 * disabled. If 'create' is TRUE then intermediate directories will
 * be created (set this to FALSE if you just want to find out where
 * a saved file would go without actually altering the filesystem).
 *
 * g_free() the result.
 */
gchar *choices_find_xdg_path_save(const char *leaf, const char *dir,
				  const char *site, gboolean create)
{
	gchar	*path, *retval, *tmp;
	
	g_return_val_if_fail(xdg_dir_list != NULL, NULL);

	if (create && !exists(xdg_dir_list[0]))
	{
		if (mkdir(xdg_dir_list[0], 0777))
			g_warning("mkdir(%s): %s\n", xdg_dir_list[0],
					g_strerror(errno));
	}

	if(site)
	{
		path = g_build_filename(xdg_dir_list[0], site, NULL);
		if (create && !exists(path))
		{
			if (mkdir(path, 0777))
				g_warning("mkdir(%s): %s\n", path,
					  g_strerror(errno));
		}
		tmp=path;
	} else {
		tmp=g_strdup(xdg_dir_list[0]);
	}
	
	path = g_build_filename(tmp, dir, NULL);
	g_free(tmp);
	if (create && !exists(path))
	{
		if (mkdir(path, 0777))
			g_warning("mkdir(%s): %s\n", path, g_strerror(errno));
	}
	
	retval = g_build_filename(path, leaf, NULL);
	g_free(path);

	return retval;
}

/*
 * Returns an array of the directories in XDG_CONFIG_HOME and XDG_CONFIG_DIRS
 * which contain a subdirectory called 'dir' (optionally in a subdirectory
 * called site).
 *
 * Lower-indexed results should override higher-indexed ones.
 *
 * Free the list using choices_free_list().
 */
GPtrArray *choices_list_xdg_dirs(char *dir, char *site)
{
	GPtrArray	*list;
	int              i;

	g_return_val_if_fail(xdg_dir_list != NULL, NULL);

	list = g_ptr_array_new();

	for (i=0; i<xdg_dir_count; i++)
	{
		guchar	*path;

		if(site)
			path = g_build_filename(xdg_dir_list[i], site,
					   dir, NULL);
		else
			path = g_build_filename(xdg_dir_list[i], dir, NULL);
		
		if (exists(path))
			g_ptr_array_add(list, path);
		else
			g_free(path);
	}

	return list;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/


/* Returns TRUE if the object exists, FALSE if it doesn't */
static gboolean exists(char *path)
{
	struct stat info;

	return stat(path, &info) == 0;
}

#include <unistd.h>
#include <gtk/gtk.h>

static void migrate_choices(void)
{
	gchar *opath, *npath;
	int failed=0;
	int i;
	gchar *src, *dest;
	gboolean migrated_something = FALSE;

	npath=choices_find_xdg_path_save("...", PROJECT, SITE, FALSE);
	opath=choices_find_path_save("...", PROJECT,FALSE);
	
	/*
	dest=choices_find_xdg_path_save(".", PROJECT, SITE, TRUE);
	g_free(dest);
	*/

	for(i=0; to_migrate[i].dir; i++) {
		src=g_build_filename(dir_list[0], to_migrate[i].dir, NULL);
		dest=choices_find_xdg_path_save(NULL, NULL,
						to_migrate[i].site, TRUE);
		g_free(dest);
		dest=choices_find_xdg_path_save(NULL,
						to_migrate[i].dir,
						to_migrate[i].site,
						FALSE);
		errno=0;
		if(exists(src)) {
			if(rename(src, dest)==0) {
				if(to_migrate[i].symlink)
					symlink(dest, src);
				migrated_something = TRUE;
			} else {
				g_warning("rename(%s, %s): %s\n",
					  src, dest,
					  g_strerror(errno));
				failed++;
			}
		} else if(to_migrate[i].symlink) {
			/*
			if(!exists(dir_list[0])) {
				if (mkdir(dir_list[0], 0777))
					g_warning("mkdir(%s): %s\n",
						  dir_list[0],
						  g_strerror(errno));
			}
			symlink(dest, src);
			*/
		}
		g_free(src);
		g_free(dest);
	}

	if (migrated_something)
	{
		gchar *failed_msg = NULL;
		if (failed)
			failed_msg = g_strdup_printf(_("%d directories could not be migrated"),
					     failed);
		info_message(_("Choices have been moved from \n"
			       "%s\n "
			       "to the new location \n"
			       "%s\n%s"),
			     opath, npath, failed_msg ? failed_msg : "");
		g_free(failed_msg);
	}
		
	g_free(opath);
	g_free(npath);
}
