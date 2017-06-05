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

/* run.c */

#include "config.h"

#include <errno.h>
#include <string.h>
#include <sys/param.h>

#include "global.h"

#include "run.h"
#include "support.h"
#include "gui_support.h"
#include "filer.h"
#include "display.h"
#include "main.h"
#include "type.h"
#include "dir.h"
#include "diritem.h"
#include "action.h"
#include "icon.h"
#include "choices.h"

/* Static prototypes */
static void write_data(gpointer data, gint fd, GdkInputCondition cond);
static gboolean follow_symlink(const char *full_path,
			       FilerWindow *filer_window,
			       FilerWindow *src_window);
static gboolean open_file(const guchar *path, MIME_type *type);
static void open_mountpoint(const guchar *full_path, DirItem *item,
			    FilerWindow *filer_window, FilerWindow *src_window,
			    gboolean edit);
static gboolean run_desktop(const char *full_path,
			    const char **args, const char *dir);
static gboolean type_open(const char *path, MIME_type *type);

typedef struct _PipedData PipedData;

struct _PipedData
{
	guchar		*data;
	gint		tag;
	gulong		sent;
	gulong		length;
};


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/


/* An application has been double-clicked (or run in some other way) */
void run_app(const char *path)
{
	GString	*apprun;
	const char *argv[] = {NULL, NULL};

	apprun = g_string_new(path);
	argv[0] = g_string_append(apprun, "/AppRun")->str;

	rox_spawn(home_dir, argv);
	
	g_string_free(apprun, TRUE);
}

/* Execute this program, passing all the URIs in the list as arguments.
 * URIs that are files on the local machine will be passed as simple
 * pathnames. The uri_list should be freed after this function returns.
 */
void run_with_files(const char *path, GList *uri_list)
{
	const char	**argv;
	int		argc = 0, i;
	struct stat 	info;
	MIME_type	*type;

	if (stat(path, &info))
	{
		delayed_error(_("Program %s not found - deleted?"), path);
		return;
	}

	argv = g_malloc(sizeof(char *) * (g_list_length(uri_list) + 2));

	if (S_ISDIR(info.st_mode))
		argv[argc++] = make_path(path, "AppRun");
	else
		argv[argc++] = path;
	
	while (uri_list)
	{
		const EscapedPath *uri = uri_list->data;
		char *local;

		local = get_local_path(uri);
		if (local) 
			argv[argc++] = local;
		else
			argv[argc++] = unescape_uri(uri);
		uri_list = uri_list->next;
	}
	
	argv[argc++] = NULL;

	type = type_from_path(argv[0]);
	if (type && type == application_x_desktop)
	{
		run_desktop(argv[0], argv + 1, home_dir);
	}
	else
	{
		rox_spawn(home_dir, argv);
	}

	for (i = 1; i < argc; i++)
		g_free((gchar *) argv[i]);
	g_free(argv);
}

/* Run the program as '<path> -', piping the data to it via stdin.
 * You can g_free() the data as soon as this returns.
 */
void run_with_data(const char *path, gpointer data, gulong length)
{
	const char	*argv[] = {NULL, "-", NULL};
	struct stat 	info;
	int		fds[2];
	PipedData	*pd;

	if (stat(path, &info))
	{
		delayed_error(_("Program %s not found - deleted?"), path);
		return;
	}

	if (S_ISDIR(info.st_mode))
		argv[0] = make_path(path, "AppRun");
	else
		argv[0] = path;
	
	if (pipe(fds))
	{
		delayed_error("pipe: %s", g_strerror(errno));
		return;
	}
	close_on_exec(fds[1], TRUE);
	close_on_exec(fds[0], TRUE);

	switch (fork())
	{
		case -1:
			delayed_error("fork: %s", g_strerror(errno));
			close(fds[1]);
			break;
		case 0:
			/* We are the child */
			chdir(home_dir);
			if (dup2(fds[0], 0) == -1)
				g_warning("dup2() failed: %s\n",
						g_strerror(errno));
			else
			{
				close_on_exec(0, FALSE);
				if (execv(argv[0], (char **) argv))
					g_warning("execv(%s) failed: %s\n",
						argv[0], g_strerror(errno));
			}
			_exit(1);
		default:
			/* We are the parent */
			set_blocking(fds[1], FALSE);
			pd = g_new(PipedData, 1);
			pd->data = g_malloc(length);
			memcpy(pd->data, data, length);
			pd->length = length;
			pd->sent = 0;
			pd->tag = gdk_input_add_full(fds[1], GDK_INPUT_WRITE,
						write_data, pd, NULL);
			break;
	}

	close(fds[0]);
}

/* Splits args into an argument vector, and runs the program. Must be
 * executable.
 */
void run_with_args(const char *path, DirItem *item, const char *args)
{
	GError *error = NULL;
	gchar **argv = NULL;
	int n_args = 0;

	if (item->base_type != TYPE_DIRECTORY && item->base_type != TYPE_FILE)
	{
		delayed_error("Arguments (%s) given for non-executable item %s",
				args, path);
		return;
	}

	if (!g_shell_parse_argv(args, &n_args, &argv, &error))
	{
		delayed_error("Failed to parse argument string '%s':\n%s",
				args, error->message);
		g_error_free(error);
		return;
	}

	g_return_if_fail(argv != NULL);
	g_return_if_fail(error == NULL);

	argv = g_realloc(argv, (n_args + 2) * sizeof(gchar *));
	memmove(argv + 1, argv, (n_args + 1) * sizeof(gchar *));

	if (item->base_type == TYPE_DIRECTORY)
		argv[0] = g_strconcat(path, "/AppRun", NULL);
	else
		argv[0] = g_strdup(path);

	rox_spawn(home_dir, (const gchar **) argv);

	g_strfreev(argv);
}

/* Load a file, open a directory or run an application. Or, if 'edit' is set:
 * edit a file, open an application, follow a symlink or mount a device.
 *
 * filer_window is the window to use for displaying a directory.
 * NULL will always use a new directory when needed.
 * src_window is the window to copy options from, or NULL.
 *
 * Returns TRUE on success.
 */
gboolean run_diritem(const guchar *full_path,
		     DirItem *item,
		     FilerWindow *filer_window,
		     FilerWindow *src_window,
		     gboolean edit)
{
	if (item->flags & ITEM_FLAG_SYMLINK && edit)
		return follow_symlink(full_path, filer_window, src_window);

	switch (item->base_type)
	{
		case TYPE_DIRECTORY:
			if (item->flags & ITEM_FLAG_APPDIR && !edit)
			{
				run_app(full_path);
				return TRUE;
			}

			if (item->flags & ITEM_FLAG_MOUNT_POINT)
			{
				open_mountpoint(full_path, item,
						filer_window, src_window, edit);
			}
			else if (filer_window)
				filer_change_to(filer_window, full_path, NULL);
			else
				filer_opendir(full_path, src_window, NULL);
			return TRUE;
		case TYPE_FILE:
			if (EXECUTABLE_FILE(item) && !edit)
			{
				const char *argv[] = {NULL, NULL};
				guchar	*dir = filer_window
						? filer_window->sym_path
						: NULL;

				if (item->mime_type == application_x_desktop)
					return run_desktop(full_path,
							   NULL, dir);
				else
					argv[0] = full_path;

				return rox_spawn(dir, argv) != 0;
			}

			return open_file(full_path, edit ? text_plain
						  : item->mime_type);
		case TYPE_ERROR:
			delayed_error(_("File doesn't exist, or I can't "
					  "access it: %s"), full_path);
			return FALSE;
		default:
		        delayed_error(
				_("I don't know how to open '%s'"), full_path);
			return FALSE;
	}
}

/* Attempt to open this item */
gboolean run_by_path(const guchar *full_path)
{
	gboolean retval;
	DirItem	*item;

	/* XXX: Loads an image - wasteful */
	item = diritem_new("");
	diritem_restat(full_path, item, NULL);
	retval = run_diritem(full_path, item, NULL, NULL, FALSE);
	diritem_free(item);
	
	return retval;
}

/* Convert uri to path and call run_by_path() */
gboolean run_by_uri(const gchar *uri, gchar **errmsg)
{
	gboolean retval;
	gchar *tmp, *tmp2;
	gchar *scheme;
	gchar *cmd;

	scheme=get_uri_scheme((EscapedPath *) uri);
	if(!scheme)
	{
		*errmsg=g_strdup_printf(_("'%s' is not a valid URI"),
						uri);
		return FALSE;
	}

	if(strcmp(scheme, "file")==0) {
		tmp=get_local_path((EscapedPath *) uri);
		if(tmp) {
			tmp2=pathdup(tmp);
			retval=run_by_path(tmp2);
			if(!retval)
				*errmsg=g_strdup_printf(_("%s not accessable"),
							tmp);
		
			g_free(tmp2);
			g_free(tmp);

		} else {
			retval=FALSE;
			*errmsg=g_strdup_printf(_("Non-local URL %s"), uri);
		}

	} else if((cmd=choices_find_xdg_path_load(scheme, "URI", SITE))) {
		DirItem *item;

		item=diritem_new(scheme);
		diritem_restat(cmd, item, NULL);

		run_with_args(cmd, item, uri);
		retval=TRUE; /* we hope... */

		diritem_free(item);
		g_free(cmd);
		
	} else {
		retval=FALSE;
		*errmsg=g_strdup_printf(_("%s: no handler for %s"),
					uri, scheme);
	}
	
	g_free(scheme);

	return retval;
}

/* Open dir/Help, or show a message if missing */
void show_help_files(const char *dir)
{
	const char	*help_dir;

	help_dir = make_path(dir, "Help");

	if (file_exists(help_dir))
		filer_opendir(help_dir, NULL, NULL);
	else
		info_message(
			_("Application:\n"
			"This is an application directory - you can "
			"run it as a program, or open it (hold down "
			"Shift while you open it). Most applications provide "
			"their own help here, but this one doesn't."));
}

/* Open a directory viewer showing this file, and wink it */
void open_to_show(const guchar *path)
{
	FilerWindow	*new;
	guchar		*dir, *slash;

	g_return_if_fail(path != NULL);

	dir = g_strdup(path);
	slash = strrchr(dir, '/');
	if (slash == dir || !slash)
	{
		/* Item in the root (or root itself!) */
		new = filer_opendir("/", NULL, NULL);
		if (new && dir[1])
			display_set_autoselect(new, dir + 1);
	}
	else
	{
		*slash = '\0';
		new = filer_opendir(dir, NULL, NULL);
		if (new)
		{
			if (slash[1] == '.')
				display_set_hidden(new, TRUE);
			display_set_autoselect(new, slash + 1);
		}
	}

	g_free(dir);
}

/* Invoked using -x, this indicates that the filesystem has been modified
 * and we should look at this item again.
 */
void examine(const guchar *path)
{
	struct stat info;

	if (mc_stat(path, &info) != 0)
	{
		/* Deleted? Do a paranoid update of everything... */
		filer_check_mounted(path);
	}
	else
	{
		/* Update directory containing this item... */
		dir_check_this(path);

		/* If this is itself a directory then rescan its contents... */
		if (S_ISDIR(info.st_mode))
			refresh_dirs(path);

		/* If it's on the pinboard or a panel, update the icon... */
		icons_may_update(path);
	}
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/


static void write_data(gpointer data, gint fd, GdkInputCondition cond)
{
	PipedData *pd = (PipedData *) data;
	
	while (pd->sent < pd->length)
	{
		int	sent;

		sent = write(fd, pd->data + pd->sent, pd->length - pd->sent);

		if (sent < 0)
		{
			if (errno == EAGAIN)
				return;
			delayed_error(_("Could not send data to program: %s"),
					g_strerror(errno));
			goto finish;
		}

		pd->sent += sent;
	}

finish:
	g_source_remove(pd->tag);
	g_free(pd->data);
	g_free(pd);
	close(fd);
}

/* Follow the link 'full_path' and display it in filer_window, or a
 * new window if that is NULL.
 */
static gboolean follow_symlink(const char *full_path,
			       FilerWindow *filer_window,
			       FilerWindow *src_window)
{
	char	*real, *slash;
	char	*new_dir;
	char	path[MAXPATHLEN + 1];
	int	got;

	got = readlink(full_path, path, MAXPATHLEN);
	if (got < 0)
	{
		delayed_error(_("Could not read link: %s"),
				  g_strerror(errno));
		return FALSE;
	}

	g_return_val_if_fail(got <= MAXPATHLEN, FALSE);
	path[got] = '\0';

	/* Make a relative path absolute */
	if (path[0] != '/')
	{
		guchar	*tmp;
		slash = strrchr(full_path, '/');
		g_return_val_if_fail(slash != NULL, FALSE);

		tmp = g_strndup(full_path, slash - full_path);
		real = pathdup(make_path(tmp, path));
		/* NB: full_path may be invalid here... */
		g_free(tmp);
	}
	else
		real = pathdup(path);

	slash = strrchr(real, '/');
	if (!slash)
	{
		g_free(real);
		delayed_error(
			_("Broken symlink (or you don't have permission "
			  "to follow it): %s"), full_path);
		return FALSE;
	}

	*slash = '\0';

	if (*real)
		new_dir = real;
	else
		new_dir = "/";

	if (filer_window)
		filer_change_to(filer_window, new_dir, slash + 1);
	else
	{
		FilerWindow *new;
		
		new = filer_opendir(new_dir, src_window, NULL);
		if (new)
			display_set_autoselect(new, slash + 1);
	}

	g_free(real);

	return TRUE;
}

/* Load this file into an appropriate editor */
static gboolean open_file(const guchar *path, MIME_type *type)
{
	g_return_val_if_fail(type != NULL, FALSE);

	if (type_open(path, type))
		return TRUE;

	report_error(
		_("No run action specified for files of this type (%s/%s) - "
		"you can set a run action by choosing `Set Run Action' "
		"from the File menu, or you can just drag the file to an "
		"application.%s"),
		type->media_type,
		type->subtype,
		type->executable ? _("\n\nNote: If this is a computer program which "
				   "you want to run, you need to set the execute bit "
				   "by choosing Permissions from the File menu.")
				: "");

	return FALSE;
}

/* Called like run_diritem, when a mount-point is opened */
static void open_mountpoint(const guchar *full_path, DirItem *item,
			    FilerWindow *filer_window, FilerWindow *src_window,
			    gboolean edit)
{
	gboolean mounted = (item->flags & ITEM_FLAG_MOUNTED) != 0;

	if (mounted == edit)
	{
		GList	*paths;

		paths = g_list_prepend(NULL, (gpointer) full_path);
		action_mount(paths, filer_window == NULL, !mounted, -1);
		g_list_free(paths);
		if (filer_window && !mounted)
			filer_change_to(filer_window, full_path, NULL);
	}
	else
	{
		if (filer_window)
			filer_change_to(filer_window, full_path, NULL);
		else
			filer_opendir(full_path, src_window, NULL);
	}
}

/* full_path is a .desktop file. Execute the application, using the Exec line
 * from the file.
 * Returns TRUE on success.
 */
static gboolean run_desktop(const char *full_path,
			    const char **args,
			    const char *dir)
{
	GError *error = NULL;
	char *exec = NULL;
	char *terminal = NULL;
	char *req_dir = NULL;
	gint argc = 0;
	gchar **argv = NULL;
	GPtrArray *expanded = NULL;
	gboolean inserted_args = FALSE;
	int i;
	gboolean success = FALSE;

	get_values_from_desktop_file(full_path,
					&error,
					"Desktop Entry", "Exec", &exec,
					"Desktop Entry", "Terminal", &terminal,
				        "Desktop Entry", "Path", &req_dir,
					NULL);
	if (error)
	{
		delayed_error("Failed to parse .desktop file '%s':\n%s",
				full_path, error->message);
		goto err;
	}

	if (!exec)
	{
		delayed_error("Can't find Exec command in .desktop file '%s'",
				full_path);
		goto err;
	}

	if (!g_shell_parse_argv(exec, &argc, &argv, &error))
	{
		delayed_error("Failed to parse '%s' from '%s':\n%s",
				exec, full_path, error->message);
		goto err;
	}

	expanded = g_ptr_array_new();

	if (terminal && g_strcasecmp(terminal, "true") == 0) {
		g_ptr_array_add(expanded, g_strdup("xterm"));
		g_ptr_array_add(expanded, g_strdup("-e"));
	}

	for (i = 0; i < argc; i++)
	{
		const char *src = argv[i];

		if (src[0] == '%' && src[1] != '\0' && src[2] == '\0')
		{
			/* We should treat these four differently. */
			if (src[1] == 'f' || src[1] == 'F' ||
			    src[1] == 'u' || src[1] == 'U')
			{
				int j;
				for (j = 0; args && args[j]; j++)
					g_ptr_array_add(expanded, g_strdup(args[j]));
				inserted_args = TRUE;
			}
			else
			{
				delayed_error("Unsupported escape character in '%s' in '%s'",
						exec, full_path);
				goto err;
			}
		}
		else
		{
			g_ptr_array_add(expanded, g_strdup(src));
		}
	}
	if (!inserted_args)
	{
		/* Many .desktop files don't include a % expansion. In that case
		 * add the arguments here.
		 */
		int j;
		for (j = 0; args && args[j]; j++)
			g_ptr_array_add(expanded, g_strdup(args[j]));
	}
	g_ptr_array_add(expanded, NULL);

	if(req_dir && req_dir[0])
		dir = req_dir;

	success = rox_spawn(dir, (const gchar **) expanded->pdata);
err:
	if (error != NULL)
		g_error_free(error);
	if (exec != NULL)
		g_free(exec);
	if (terminal != NULL)
		g_free(terminal);
	if (req_dir != NULL)
		g_free(req_dir);
	if (argv != NULL)
		g_strfreev(argv);
	if (expanded != NULL)
	{
		g_ptr_array_foreach(expanded, (GFunc) g_free, NULL);
		g_ptr_array_free(expanded, TRUE);
	}

	return success;
}

/* Returns FALSE is no run action is set for this type. */
static gboolean type_open(const char *path, MIME_type *type)
{
	gchar *argv[] = {NULL, NULL, NULL};
	char		*open;
	struct stat	info;

	argv[1] = (char *) path;

	open = handler_for(type);
	if (!open)
		return FALSE;

	if (stat(open, &info))
	{
		report_error("stat(%s): %s", open, g_strerror(errno));
		g_free(open);
		return TRUE;
	}

	if (info.st_mode & S_IWOTH)
	{
		gchar *choices_dir;
		GList *paths;

		report_error(_("Executable '%s' is world-writeable! Refusing "
			"to run. Please change the permissions now (this "
			"problem may have been caused by a bug in earlier "
			"versions of the filer).\n\n"
			"Having (non-symlink) run actions world-writeable "
			"means that other people who use your computer can "
			"replace your run actions with malicious versions.\n\n"
			"If you trust everyone who could write to these files "
			"then you needn't worry. Otherwise, you should check, "
			"or even just delete, all the existing run actions."),
			open);
		choices_dir = g_path_get_dirname(open);
		paths = g_list_append(NULL, choices_dir);
		action_chmod(paths, TRUE, _("go-w (Fix security problem)"));
		g_free(choices_dir);
		g_list_free(paths);
		g_free(open);
		return TRUE;
	}

	if (S_ISDIR(info.st_mode))
	{
		argv[0] = g_strconcat(open, "/AppRun", NULL);
		rox_spawn(home_dir, (const gchar **) argv);
	}
	else if (type_get_type(open) == application_x_desktop)
	{
		argv[0] = open;
		run_desktop(open, (const char **) (argv + 1), home_dir);
	}
	else
	{
		argv[0] = open;
		rox_spawn(home_dir, (const gchar **) argv);
	}

	if (argv[0] != open)
		g_free(argv[0]);

	g_free(open);
	
	return TRUE;
}

