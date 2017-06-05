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

/* minibuffer.c - for handling the path entry box at the bottom */

#include "config.h"

#include <fnmatch.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <glob.h>
#include <stdio.h>

#include <sys/types.h>
#include <pwd.h>
 
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "global.h"

#include "options.h"
#include "find.h"
#include "gui_support.h"
#include "support.h"
#include "minibuffer.h"
#include "filer.h"
#include "display.h"
#include "main.h"
#include "action.h"
#include "diritem.h"
#include "type.h"
#include "view_iface.h"

static GList *shell_history = NULL;

/* Static prototypes */
static gint key_press_event(GtkWidget	*widget,
			GdkEventKey	*event,
			FilerWindow	*filer_window);
static void changed(GtkEditable *mini, FilerWindow *filer_window);
static gboolean find_next_match(FilerWindow *filer_window,
				const char *pattern,
				int dir);
static gboolean find_exact_match(FilerWindow *filer_window,
				 const gchar *pattern);
static gboolean matches(ViewIter *iter, const char *pattern);
static void search_in_dir(FilerWindow *filer_window, int dir);
static const gchar *mini_contents(FilerWindow *filer_window);
static void show_help(FilerWindow *filer_window);
static gboolean grab_focus(GtkWidget *minibuffer);
static gboolean select_if_glob(ViewIter *iter, gpointer data);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

static Option o_filer_beep_fail, o_filer_beep_multi;

void minibuffer_init(void)
{
	option_add_int(&o_filer_beep_fail, "filer_beep_fail", 1);
	option_add_int(&o_filer_beep_multi, "filer_beep_multi", 1);
}

/* Creates the minibuffer widgets, setting the appropriate fields
 * in filer_window.
 */
void create_minibuffer(FilerWindow *filer_window)
{
	GtkWidget *hbox, *label, *mini;

	hbox = gtk_hbox_new(FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(hbox),
			new_help_button((HelpFunc) show_help, filer_window),
			FALSE, TRUE, 0);

	label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, TRUE, 2);

	mini = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), mini, TRUE, TRUE, 0);
	gtk_widget_set_name(mini, "fixed-style");
	g_signal_connect(mini, "key_press_event",
			G_CALLBACK(key_press_event), filer_window);
	g_signal_connect(mini, "changed", G_CALLBACK(changed), filer_window);

	/* Grabbing focus musn't select the text... */
	g_signal_connect_swapped(mini, "grab-focus",
		G_CALLBACK(grab_focus), mini);

	filer_window->minibuffer = mini;
	filer_window->minibuffer_label = label;
	filer_window->minibuffer_area = hbox;
}

void minibuffer_show(FilerWindow *filer_window, MiniType mini_type)
{
	GtkEntry	*mini;
	int		pos = -1;
	ViewIter	cursor;
	
	g_return_if_fail(filer_window != NULL);
	g_return_if_fail(filer_window->minibuffer != NULL);

	mini = GTK_ENTRY(filer_window->minibuffer);
	entry_set_error(filer_window->minibuffer, FALSE);

	filer_window->mini_type = MINI_NONE;
	gtk_label_set_text(GTK_LABEL(filer_window->minibuffer_label),
			mini_type == MINI_PATH ? _("Goto:") :
			mini_type == MINI_SHELL ? _("Shell:") :
			mini_type == MINI_SELECT_IF ? _("Select If:") :
			mini_type == MINI_SELECT_BY_NAME ? _("Select Named:") :
			mini_type == MINI_FILTER ? _("Pattern:") :
			"?");

	switch (mini_type)
	{
		case MINI_PATH:
			view_show_cursor(filer_window->view);
			view_get_cursor(filer_window->view, &cursor);
			view_set_base(filer_window->view, &cursor);

			gtk_entry_set_text(mini,
				make_path(filer_window->sym_path, ""));
			if (filer_window->temp_show_hidden)
			{
				filer_window->temp_show_hidden = FALSE;
				display_update_hidden(filer_window);
			}
			break;
		case MINI_SELECT_IF:
			gtk_entry_set_text(mini, "");
			filer_window->mini_cursor_base = -1;	/* History */
			break;
		case MINI_SELECT_BY_NAME:
			gtk_entry_set_text(mini, "*.");
			filer_window->mini_cursor_base = -1;	/* History */
			view_select_if(filer_window->view, select_if_glob, "*.");
			break;
		case MINI_FILTER:
			if(filer_window->filter!=FILER_SHOW_GLOB ||
			   !filer_window->filter_string)
				gtk_entry_set_text(mini, "*");
			else
				gtk_entry_set_text(mini,
						  filer_window->filter_string);
			break;
		case MINI_SHELL:
		{
			DirItem *item;
			view_get_cursor(filer_window->view, &cursor);
			item = cursor.peek(&cursor);
			pos = 0;
			if (view_count_selected(filer_window->view) > 0)
				gtk_entry_set_text(mini, " \"$@\"");
			else if (item)
			{
				guchar *escaped;
				guchar *tmp;

				escaped = shell_escape(item->leafname);
				tmp = g_strconcat(" ", escaped, NULL);
				g_free(escaped);
				gtk_entry_set_text(mini, tmp);
				g_free(tmp);
			}
			else
				gtk_entry_set_text(mini, "");
			filer_window->mini_cursor_base = -1;	/* History */
			break;
		}
		default:
			g_warning("Bad minibuffer type\n");
			return;
	}
	
	filer_window->mini_type = mini_type;

	gtk_editable_set_position(GTK_EDITABLE(mini), pos);

	gtk_widget_show_all(filer_window->minibuffer_area);

	gtk_widget_grab_focus(filer_window->minibuffer);
}

void minibuffer_hide(FilerWindow *filer_window)
{
	filer_window->mini_type = MINI_NONE;

	gtk_widget_hide(filer_window->minibuffer_area);

	gtk_widget_child_focus(filer_window->window, GTK_DIR_TAB_FORWARD);

	if (filer_window->temp_show_hidden)
	{
		DirItem *item;
		ViewIter iter;

		view_get_cursor(filer_window->view, &iter);
		item = iter.peek(&iter);

		if (item == NULL || item->leafname[0] != '.')
		        display_update_hidden(filer_window);
		filer_window->temp_show_hidden = FALSE;
	}
}

/* Insert this leafname at the cursor (replacing the selection, if any).
 * Must be in SHELL mode.
 */
void minibuffer_add(FilerWindow *filer_window, const gchar *leafname)
{
	guchar		*esc;
	GtkEditable 	*edit = GTK_EDITABLE(filer_window->minibuffer);
	GtkEntry 	*entry = GTK_ENTRY(edit);
	int		pos;

	g_return_if_fail(filer_window->mini_type == MINI_SHELL);

	esc = shell_escape(leafname);

	gtk_editable_delete_selection(edit);
	pos = gtk_editable_get_position(edit);

	if (pos > 0 && gtk_entry_get_text(entry)[pos - 1] != ' ')
		gtk_editable_insert_text(edit, " ", 1, &pos);

	gtk_editable_insert_text(edit, esc, strlen(esc), &pos);
	gtk_editable_set_position(edit, pos);

	g_free(esc);
}


/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void show_help(FilerWindow *filer_window)
{
	switch (filer_window->mini_type)
	{
		case MINI_PATH:
			info_message(
				_("Enter the name of a file and I'll display "
				"it for you. Press Tab to fill in the longest "
				"match. Escape to close the minibuffer."));
			break;
		case MINI_SHELL:
			info_message(
				_("Enter a shell command to execute. Click "
				"on a file to add it to the buffer."));
			break;
		case MINI_SELECT_BY_NAME:
			info_message(
				_("Enter a file name pattern to select all matching files:\n\n"
				"? means any character\n"
				"* means zero or more characters\n"
				"[aA] means 'a' or 'A'\n"
				"[a-z] means any character from a to z (lowercase)\n"
				"*.png means any name ending in '.png'"));
			break;
		case MINI_SELECT_IF:
			show_condition_help(NULL);
			break;
		case MINI_FILTER:
			info_message(
				_("Enter a pattern to match for files to "
				"be shown.  An empty filter turns the "
				  "filter off."));
			break;
		default:
			g_warning("Unknown minibuffer type!");
			break;
	}
}


/*			PATH ENTRY			*/

static void path_return_pressed(FilerWindow *filer_window, GdkEventKey *event)
{
	const gchar	*path, *pattern;
	int		flags = OPEN_FROM_MINI | OPEN_SAME_WINDOW;
	ViewIter	iter;
	DirItem		*item;

	path = gtk_entry_get_text(GTK_ENTRY(filer_window->minibuffer));
	pattern = g_basename(path);

	view_get_cursor(filer_window->view, &iter);

	item = iter.peek(&iter);
	if (item == NULL || !matches(&iter, pattern))
	{
		gdk_beep();
		return;
	}
	
	if ((event->state & GDK_SHIFT_MASK) != 0)
		flags |= OPEN_SHIFT;

	filer_openitem(filer_window, &iter, flags);
}

/* Use the cursor item to fill in the minibuffer.
 * If there are multiple matches then fill in as much as possible and
 * (possibly) beep.
 */
static void complete(FilerWindow *filer_window)
{
	GtkEntry	*entry;
	DirItem 	*item, *other;
	int		shortest_stem = -1;
	int		current_stem;
	const gchar	*text, *leaf;
	ViewIter	cursor, iter;

	view_get_cursor(filer_window->view, &cursor);
	item = cursor.peek(&cursor);

	if (!item)
	{
		gdk_beep();
		return;
	}

	entry = GTK_ENTRY(filer_window->minibuffer);

	text = gtk_entry_get_text(entry);
	leaf = strrchr(text, '/');
	if (!leaf)
	{
		gdk_beep();
		return;
	}

	leaf++;
	if (!matches(&cursor, leaf))
	{
		gdk_beep();
		return;
	}
	
	current_stem = strlen(leaf);

	/* Find the longest other match of this name. If it's longer than
	 * the currently entered text then complete only up to that length.
	 */
	view_get_iter(filer_window->view, &iter, 0);
	while ((other = iter.next(&iter)))
	{
		int	stem = 0;

		if (iter.i == cursor.i)	/* XXX */
			continue;

		while (other->leafname[stem] && item->leafname[stem])
		{
			gchar	a, b;
			/* Like the matches() function below, the comparison of
			 * leafs must be case-insensitive.
			 */
		  	a = g_ascii_tolower(item->leafname[stem]);
		  	b = g_ascii_tolower(other->leafname[stem]);
			if (a != b)
				break;
			stem++;
		}

		/* stem is the index of the first difference */
		if (stem >= current_stem &&
				(shortest_stem == -1 || stem < shortest_stem))
			shortest_stem = stem;
	}

	if (current_stem == shortest_stem)
	{
		/* We didn't add anything... */
		if (o_filer_beep_fail.int_value)
			gdk_beep();
	}
	else if (current_stem < shortest_stem)
	{
		gint tmp_pos;
		
		/* Have to replace the leafname text in the minibuffer rather
		 * than just append to it.  Here's an example:
		 * Suppose we have two dirs, /tmp and /TMP.
		 * The user enters /t in the minibuffer and hits Tab.
		 * With the g_ascii_tolower() code above, this would result
		 * in /tMP being bisplayed in the minibuffer which isn't
		 * intuitive.  Therefore all text after the / must be replaced.
		 */
		tmp_pos = leaf - text; /* index of start of leaf */
		gtk_editable_delete_text(GTK_EDITABLE(entry),
					 tmp_pos, entry->text_length);
		gtk_editable_insert_text(GTK_EDITABLE(entry),
					 item->leafname, shortest_stem,
					 &tmp_pos);
		
		gtk_editable_set_position(GTK_EDITABLE(entry), -1);

		if (o_filer_beep_multi.int_value)
			gdk_beep();
	}
	else
	{
		const guchar *new;

		new = make_path(filer_window->sym_path, item->leafname);

		if (item->base_type == TYPE_DIRECTORY &&
				(item->flags & ITEM_FLAG_APPDIR) == 0)
			new = make_path(new, "");

		gtk_entry_set_text(entry, new);
		gtk_editable_set_position(GTK_EDITABLE(entry), -1);
	}
}

static void path_changed(FilerWindow *filer_window)
{
	GtkWidget *mini = filer_window->minibuffer;
	const char	*rawnew, *leaf;
	char		*path;
	char		*new = NULL;
	gboolean	error = FALSE;

	rawnew = gtk_entry_get_text(GTK_ENTRY(mini));
	if (!*rawnew)
	{
		/* Entry may be blank because we're in the middle of changing
		 * to something else...
		 */
		entry_set_error(mini, FALSE);
		return;
	}

	switch (rawnew[0])
	{
	case '/':
		new=g_strdup(rawnew);
		break;

	case '~':
		if (!rawnew[1] || rawnew[1]=='/')
		{
			new=g_strconcat(g_get_home_dir(), "/", 
					     rawnew[1]? rawnew+2: "", "/", 
					     NULL);
		}
		else
		{
			const char *sl;
			gchar *username;
			struct passwd *passwd;

			
			/* Need to lookup user name */
			for(sl=rawnew+2; *sl && *sl!='/'; sl++)
				;
			username=g_strndup(rawnew+1, sl-rawnew-1);
			passwd=getpwnam(username);
			g_free(username);

			if(passwd)
			{
				new=g_strconcat(passwd->pw_dir, "/", 
					     sl+1, "/", 
					     NULL);
			}
			else
				new=g_strdup(rawnew);
		}
		break;

	default:
		new=g_strdup(rawnew);
		break;
	}
		

	leaf = g_basename(new);
	if (leaf == new)
		path = g_strdup("/");
	else
		path = g_path_get_dirname(new);

	if (strcmp(path, filer_window->sym_path) != 0)
	{
		/* The new path is in a different directory */
		struct stat info;

		if (stat_with_timeout(path, &info) == 0 &&
				S_ISDIR(info.st_mode))
		{
			filer_change_to(filer_window, path, leaf);
		}
		else
			error = TRUE;
	}
	else
	{
		if (*leaf == '.' && !filer_window->temp_show_hidden)
		{
			filer_window->temp_show_hidden = TRUE;
			display_update_hidden(filer_window);
		}
		
		if (find_exact_match(filer_window, leaf) == FALSE &&
		    find_next_match(filer_window, leaf, 0) == FALSE)
			error = TRUE;
	}
		
	g_free(new);
	g_free(path);

	entry_set_error(mini, error);
}

/* Look for an exact match, and move the cursor to it if found.
 * TRUE on success.
 */
static gboolean find_exact_match(FilerWindow *filer_window,
				 const gchar *pattern)
{
	DirItem		*item;
	ViewIter	iter;
	ViewIface	*view = filer_window->view;

	view_get_iter(view, &iter, 0);

	while ((item = iter.next(&iter)))
	{
		if (strcmp(item->leafname, pattern) == 0)
		{
			view_cursor_to_iter(view, &iter);
			return TRUE;
		}
	}

	return FALSE;
}

/* Find the next item in the view that matches 'pattern'. Start from
 * cursor_base and loop at either end. dir is 1 for a forward search,
 * -1 for backwards. 0 means forwards, but may stay the same.
 *
 * Does not automatically update cursor_base.
 *
 * Returns TRUE if a match is found.
 */
static gboolean find_next_match(FilerWindow *filer_window,
				const char *pattern,
				int dir)
{
	ViewIface  *view = filer_window->view;
	ViewIter   iter;

	if (view_count_items(view) < 1)
		return FALSE;

	view_get_iter(view, &iter,
		VIEW_ITER_FROM_BASE |
		(dir >= 0 ? 0 : VIEW_ITER_BACKWARDS));

	if (dir != 0)
		iter.next(&iter);	/* Don't look at the base itself */

	while (iter.next(&iter))
	{
		if (matches(&iter, pattern))
		{
			view_cursor_to_iter(view, &iter);
			return TRUE;
		}
	}

	/* No matches (except possibly base itself) */
	view_get_iter(view, &iter,
		VIEW_ITER_FROM_BASE | VIEW_ITER_ONE_ONLY |
		(dir >= 0 ? 0 : VIEW_ITER_BACKWARDS));

	view_cursor_to_iter(view, &iter);

	return FALSE;
}

static gboolean matches(ViewIter *iter, const char *pattern)
{
	DirItem *item;
	
	item = iter->peek(iter);
	
	return strncasecmp(item->leafname, pattern, strlen(pattern)) == 0;
}

/* Find next match and set base for future matches. */
static void search_in_dir(FilerWindow *filer_window, int dir)
{
	const char *path, *pattern;
	ViewIter iter;

	path = gtk_entry_get_text(GTK_ENTRY(filer_window->minibuffer));
	pattern = g_basename(path);
	
	view_get_cursor(filer_window->view, &iter);
	view_set_base(filer_window->view, &iter);
	find_next_match(filer_window, pattern, dir);
	view_get_cursor(filer_window->view, &iter);
	view_set_base(filer_window->view, &iter);
}

/*			SHELL COMMANDS			*/

static void add_to_history(const gchar *line)
{
	guchar 	*last;
	
	last = shell_history ? (guchar *) shell_history->data : NULL;

	if (last && strcmp(last, line) == 0)
		return;			/* Duplicating last entry */
	
	shell_history = g_list_prepend(shell_history, g_strdup(line));
}

static void shell_done(FilerWindow *filer_window)
{
	if (filer_exists(filer_window))
		filer_update_dir(filer_window, TRUE);
}

/* Given a list of matches, return the longest stem. g_free() the result.
 * Special chars are escaped. If there is only a single (non-dir) match
 * then a trailing space is added.
 */
static guchar *best_match(FilerWindow *filer_window, glob_t *matches)
{
	gchar	*first = matches->gl_pathv[0];
	int	i;
	int	longest, path_len;
	guchar	*path, *tmp;

	longest = strlen(first);

	for (i = 1; i < matches->gl_pathc; i++)
	{
		int	j;
		guchar	*m = matches->gl_pathv[i];

		for (j = 0; j < longest; j++)
			if (m[j] != first[j])
				longest = j;
	}

	path_len = strlen(filer_window->sym_path);
	if (strncmp(filer_window->sym_path, first, path_len) == 0 &&
			first[path_len] == '/' && first[path_len + 1])
	{
		path = g_strndup(first + path_len + 1, longest - path_len - 1);
	}
	else
		path = g_strndup(first, longest);

	tmp = shell_escape(path);
	g_free(path);

	if (matches->gl_pathc == 1 && tmp[strlen(tmp) - 1] != '/')
	{
		path = g_strdup_printf("%s ", tmp);
		g_free(tmp);
		return path;
	}
	
	return tmp;
}

static void shell_tab(FilerWindow *filer_window)
{
	const gchar	*entry;
	int	i;
	int	pos;
	GString	*leaf;
	glob_t	matches;
	int	leaf_start;
	
	entry = gtk_entry_get_text(GTK_ENTRY(filer_window->minibuffer));
	pos = gtk_editable_get_position(GTK_EDITABLE(filer_window->minibuffer));
	leaf = g_string_new(NULL);

	for (i = 0; i < pos; i++)
	{
		guchar	c = entry[i];
		
		if (leaf->len == 0)
			leaf_start = i;

		if (c == ' ')
		{
			g_string_truncate(leaf, 0);
			continue;
		}
		else if (c == '\\' && i + 1 < pos)
			c = entry[++i];
		else if (c == '"' || c == '\'')
		{
			for (++i; i < pos; i++)
			{
				guchar cc = entry[i];

				if (cc == '\\' && i + 1 < pos)
					cc = entry[++i];
				else if (cc == c)
					break;
				g_string_append_c(leaf, cc);
			}
			continue;
		}
		
		g_string_append_c(leaf, c);
	}

	if (leaf->len == 0)
		leaf_start = pos;

	if (leaf->str[0] != '/' && leaf->str[0] != '~')
	{
		g_string_prepend_c(leaf, '/');
		g_string_prepend(leaf, filer_window->sym_path);
	}

	g_string_append_c(leaf, '*');

	if (glob(leaf->str,
#ifdef GLOB_TILDE
			GLOB_TILDE |
#endif
			GLOB_MARK, NULL, &matches) == 0)
	{
		if (matches.gl_pathc > 0)
		{
			guchar		*best;
			GtkEditable 	*edit =
				GTK_EDITABLE(filer_window->minibuffer);

			best = best_match(filer_window, &matches);

			gtk_editable_delete_text(edit, leaf_start, pos);
			gtk_editable_insert_text(edit, best, strlen(best),
						&leaf_start);
			gtk_editable_set_position(edit, leaf_start);

			g_free(best);
		}
		if (matches.gl_pathc != 1)
			gdk_beep();

		globfree(&matches);
	}

	g_string_free(leaf, TRUE);
}

static void run_child(gpointer unused)
{
	/* Ensure output is noticed - send stdout to stderr */
	dup2(STDERR_FILENO, STDOUT_FILENO);
	close_on_exec(STDOUT_FILENO, FALSE);
}

/* Either execute the command or make it the default run action */
static void shell_return_pressed(FilerWindow *filer_window)
{
	GPtrArray	*argv;
	const gchar	*entry;
	GError		*error = NULL;
	pid_t		child;
	DirItem		*item;
	ViewIter	iter;

	entry = mini_contents(filer_window);

	if (!entry)
		goto out;

	add_to_history(entry);

	argv = g_ptr_array_new();
	g_ptr_array_add(argv, "sh");
	g_ptr_array_add(argv, "-c");
	g_ptr_array_add(argv, (gchar *) entry);
	g_ptr_array_add(argv, "sh");

	view_get_iter(filer_window->view, &iter, 0);
	while ((item = iter.next(&iter)))
	{
		if (view_get_selected(filer_window->view, &iter))
			g_ptr_array_add(argv, item->leafname);
	}
	
	g_ptr_array_add(argv, NULL);

	if (!g_spawn_async_with_pipes(filer_window->sym_path,
			(gchar **) argv->pdata, NULL,
			G_SPAWN_DO_NOT_REAP_CHILD |
			G_SPAWN_SEARCH_PATH,
			run_child, NULL,	/* Child setup fn */
			&child,			/* Child PID */
			NULL, NULL, NULL,	/* Standard pipes */
			&error))
	{
		delayed_error("%s", error ? error->message : "(null)");
		g_error_free(error);
	}
	else
		on_child_death(child, (CallbackFn) shell_done, filer_window);

	g_ptr_array_free(argv, TRUE);

out:
	minibuffer_hide(filer_window);
}

/* Move through the shell history */
static void shell_recall(FilerWindow *filer_window, int dir)
{
	guchar	*command;
	int	pos = filer_window->mini_cursor_base;

	pos += dir;
	if (pos >= 0)
	{
		command = g_list_nth_data(shell_history, pos);
		if (!command)
			return;
	}
	else
		command = "";

	if (pos < -1)
		pos = -1;
	filer_window->mini_cursor_base = pos;

	gtk_entry_set_text(GTK_ENTRY(filer_window->minibuffer), command);
}

/*			SELECT IF			*/

typedef struct {
	FindInfo info;
	FilerWindow *filer_window;
	FindCondition *cond;
} SelectData;

static gboolean select_if_test(ViewIter *iter, gpointer user_data)
{
	DirItem *item;
	SelectData *data = user_data;

	item = iter->peek(iter);
	g_return_val_if_fail(item != NULL, FALSE);

	data->info.leaf = item->leafname;
	data->info.fullpath = make_path(data->filer_window->sym_path,
					data->info.leaf);

	return mc_lstat(data->info.fullpath, &data->info.stats) == 0 &&
			find_test_condition(data->cond, &data->info);
}

static void select_return_pressed(FilerWindow *filer_window, guint etime)
{
	const gchar	*entry;
	SelectData	data;

	entry = mini_contents(filer_window);

	if (!entry)
		goto out;

	add_to_history(entry);

	data.cond = find_compile(entry);
	if (!data.cond)
	{
		delayed_error(_("Invalid Find condition"));
		return;
	}

	data.info.now = time(NULL);
	data.info.prune = FALSE;	/* (don't care) */
	data.filer_window = filer_window;

	view_select_if(filer_window->view, select_if_test, &data);

	find_condition_free(data.cond);
out:
	minibuffer_hide(filer_window);
}

static void filter_return_pressed(FilerWindow *filer_window, guint etime)
{
	const gchar	*entry;

	entry = mini_contents(filer_window);

	if (entry && *entry && strcmp(entry, "*")!=0) {
		display_set_filter(filer_window, FILER_SHOW_GLOB,
				   entry);
	} else {
		display_set_filter(filer_window, FILER_SHOW_ALL, NULL);
	}
	minibuffer_hide(filer_window);
}


/*			EVENT HANDLERS			*/

static gint key_press_event(GtkWidget	*widget,
			GdkEventKey	*event,
			FilerWindow	*filer_window)
{
	if (event->keyval == GDK_Escape)
	{
		if (filer_window->mini_type == MINI_SHELL)
		{
			const gchar *line;
			
			line = mini_contents(filer_window);
			if (line)
				add_to_history(line);
		}

		minibuffer_hide(filer_window);
		return TRUE;
	}

	switch (filer_window->mini_type)
	{
		case MINI_PATH:
			switch (event->keyval)
			{
				case GDK_Up:
					search_in_dir(filer_window, -1);
					break;
				case GDK_Down:
					search_in_dir(filer_window, 1);
					break;
				case GDK_Return:
				case GDK_KP_Enter:
					path_return_pressed(filer_window,
								event);
					break;
				case GDK_Tab:
					complete(filer_window);
					break;
				default:
					return FALSE;
			}
			break;

		case MINI_SHELL:
			switch (event->keyval)
			{
				case GDK_Up:
					shell_recall(filer_window, 1);
					break;
				case GDK_Down:
					shell_recall(filer_window, -1);
					break;
				case GDK_Tab:
					shell_tab(filer_window);
					break;
				case GDK_Return:
				case GDK_KP_Enter:
					shell_return_pressed(filer_window);
					break;
				default:
					return FALSE;
			}
			break;
		case MINI_SELECT_IF:
			switch (event->keyval)
			{
				case GDK_Up:
					shell_recall(filer_window, 1);
					break;
				case GDK_Down:
					shell_recall(filer_window, -1);
					break;
				case GDK_Tab:
					break;
				case GDK_Return:
				case GDK_KP_Enter:
					select_return_pressed(filer_window,
								event->time);
					break;
				default:
					return FALSE;
			}
			break;
		case MINI_SELECT_BY_NAME:
			switch (event->keyval)
			{
				case GDK_Up:
					filer_next_selected(filer_window, -1);
					break;
				case GDK_Down:
					filer_next_selected(filer_window, 1);
					break;
				case GDK_Tab:
					break;
				case GDK_Return:
				case GDK_KP_Enter:
					minibuffer_hide(filer_window);
					break;
				default:
					return FALSE;
			}
			break;

	        case MINI_FILTER:
			switch (event->keyval)
			{
				case GDK_Return:
				case GDK_KP_Enter:
					filter_return_pressed(filer_window,
								event->time);
					break;
				default:
					return FALSE;
			}
			break;
		default:
			break;
	}

	return TRUE;
}

static gboolean select_if_glob(ViewIter *iter, gpointer data)
{
	DirItem *item;
	const char *pattern = (char *) data;

	item = iter->peek(iter);
	g_return_val_if_fail(item != NULL, FALSE);

	return fnmatch(pattern, item->leafname, 0) == 0;
}

static void changed(GtkEditable *mini, FilerWindow *filer_window)
{
	switch (filer_window->mini_type)
	{
		case MINI_PATH:
			path_changed(filer_window);
			return;
		case MINI_SELECT_IF:
			set_find_string_colour(GTK_WIDGET(mini), 
				gtk_entry_get_text(
					GTK_ENTRY(filer_window->minibuffer)));
			return;
		case MINI_SELECT_BY_NAME:
			view_select_if(filer_window->view,
					select_if_glob,
					(gpointer) gtk_entry_get_text(
					      GTK_ENTRY(filer_window->minibuffer)));
			return;
		default:
			break;
	}
}

/* Returns a string (which must NOT be freed), or NULL if the buffer
 * is blank (whitespace only).
 */
static const gchar *mini_contents(FilerWindow *filer_window)
{
	const gchar *entry, *c;

	entry = gtk_entry_get_text(GTK_ENTRY(filer_window->minibuffer));

	for (c = entry; *c; c++)
		if (!g_ascii_isspace(*c))
			return entry;

	return NULL;
}

/* This is a really ugly hack to get around Gtk+-2.0's broken auto-select
 * behaviour.
 */
static gboolean grab_focus(GtkWidget *minibuffer)
{
	GtkWidgetClass *class;
	
	class = GTK_WIDGET_CLASS(gtk_type_class(GTK_TYPE_WIDGET));

	class->grab_focus(minibuffer);

	g_signal_stop_emission(minibuffer,
		g_signal_lookup("grab_focus", G_OBJECT_TYPE(minibuffer)), 0);


	return 1;
}
