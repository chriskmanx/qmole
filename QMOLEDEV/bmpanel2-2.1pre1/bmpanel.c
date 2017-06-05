#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <dirent.h>
#include "gui.h"
#include "config-parser.h"
#include "xdg.h"
#include "settings.h"
#include "widget-utils.h"
#include "builtin-widgets.h"
#include "args.h"

/**************************************************************************
  Listing themes
**************************************************************************/

static void list_theme(const char *themefile, const char *shortname)
{
	if (!is_file_exists(themefile))
		return;

	struct config_format_tree tree;
	if (0 != load_config_format_tree(&tree, themefile))
		return;

	const char *longname = 0;
	const char *author = 0;
	struct config_format_entry *e = find_config_format_entry(&tree.root, 
								 "theme");
	if (e) {
		longname = find_config_format_entry_value(e, "name");
		author = find_config_format_entry_value(e, "author");
	}

	printf(" * %s", shortname);
	if (longname || author) {
		printf(" (");
		if (longname) {
			printf("name: %s", longname);
			if (author)
				printf(", ");
		}
		if (author)
			printf("author: %s", author);
		printf(")");
	}
	printf("\n");
	free_config_format_tree(&tree);
}

static void list_themes_in_dir(DIR *d, const char *dirpath)
{
	char buf[4096];
	struct dirent *de;
	int len;

	while ((de = readdir(d)) != 0) {
		len = strlen(de->d_name);
		switch (len) {
			/* skip current dir and parent dir */
		case 1:
			if (de->d_name[0] == '.')
				continue;
		case 2:
			if (de->d_name[0] == '.' && de->d_name[1] == '.')
				continue;
		default:
			break;
		}

		snprintf(buf, sizeof(buf), "%s/%s/theme", dirpath, de->d_name);
		list_theme(buf, de->d_name);
	}
}

static void list_themes()
{
	char buf[4096];
	size_t data_dirs_len;
	char **data_dirs = get_XDG_DATA_DIRS(&data_dirs_len);
	DIR *d;

	size_t i;
	for (i = 0; i < data_dirs_len; ++i) {
		snprintf(buf, sizeof(buf), "%s/bmpanel2/themes", data_dirs[i]);
		buf[sizeof(buf)-1] = '\0';

		printf("listing themes in \"%s\":\n", buf);
		d = opendir(buf);
		if (d) {
			list_themes_in_dir(d, buf);
			closedir(d);
		} else {
			printf(" - none\n");
		}
	}
}

/**************************************************************************
  Theme loading
**************************************************************************/

static int try_load_theme(struct config_format_tree *tree, const char *name)
{
	char buf[4096];
	size_t data_dirs_len;
	char **data_dirs;	
	int found = 0;

	/* try to load it in-place */
	snprintf(buf, sizeof(buf), "%s/theme", name);
	if (is_file_exists(buf) && 0 == load_config_format_tree(tree, buf))
		return 0;

	/* scan XDG dirs */
	data_dirs = get_XDG_DATA_DIRS(&data_dirs_len);

	size_t i;
	for (i = 0; i < data_dirs_len; ++i) {
		snprintf(buf, sizeof(buf), "%s/bmpanel2/themes/%s/theme",
			 data_dirs[i], name);
		buf[sizeof(buf)-1] = '\0';
		if (is_file_exists(buf)) {
			found = 1;
			break;
		}
	}
	free_XDG(data_dirs);

	if (!found)
		return -1;

	if (0 != load_config_format_tree(tree, buf))
		return -1;

	return 0;
}

static int load_theme(struct config_format_tree *theme, const char *theme_override)
{
	int theme_load_status = -1;
	const char *theme_name;

	if (theme_override)
		theme_name = theme_override;
	else
		theme_name = find_config_format_entry_value(&g_settings.root,
							    "theme");

	if (theme_name)
		theme_load_status = try_load_theme(theme, theme_name);

	if (theme_load_status < 0) {
		if (theme_name)
			XWARNING("Failed to load theme: \"%s\", "
				 "trying default \"native\"", theme_name);
		else
			XWARNING("Missing theme parameter, trying default \"native\"");
		theme_load_status = try_load_theme(theme, "native");
	}

	return theme_load_status;
}

/*************************************************************************/
	
static struct config_format_tree theme;
static struct panel p;

/* options */
static int show_usage;
static int show_version;
static int show_list;
static const char *theme_override;

#define BMPANEL2_VERSION_STR "bmpanel2 version 2.1\n"
#define BMPANEL2_USAGE \
"usage: bmpanel2 [-h|--help] [--version] [--usage] [--list] [--theme=<theme>]\n"

static const char *bmpanel2_version_str = BMPANEL2_VERSION_STR BMPANEL2_USAGE;

static const char *get_theme_name()
{
	if (theme_override)
		return theme_override;
	else {
		const char *theme;
		theme = find_config_format_entry_value(&g_settings.root,
						       "theme");
		if (theme)
			return theme;
		else
			return "native";
	}
}

static void reload_config()
{
	char *previous_theme = xstrdup(get_theme_name());

	free_settings();
	load_settings();

	if (strcmp(get_theme_name(), previous_theme) != 0) {
		struct widget_stash ws;
		/* free theme */
		free_config_format_tree(&theme);
		reconfigure_free_panel(&p, &ws);

		/* reload */
		if (load_theme(&theme, theme_override) < 0)
			XDIE("Failed to load theme");

		reconfigure_panel(&p, &theme, &ws);
		clean_image_cache(0);
	} else {
		reconfigure_panel_config(&p);
		reconfigure_widgets(&p);
	}
	xfree(previous_theme);
}

static void sigint_handler(int xxx)
{
	XWARNING("sigint signal received, stopping main loop...");
	g_main_loop_quit(p.loop);
}

static void sigterm_handler(int xxx)
{
	XWARNING("sigterm signal received, stopping main loop...");
	g_main_loop_quit(p.loop);
}

static gboolean reload_config_event(gpointer data)
{
	reload_config();
	return 0;
}

static void sigusr1_handler(int xxx)
{
	g_idle_add(reload_config_event, 0);
}

static void mysignal(int sig, void (*handler)(int))
{
	struct sigaction sa;
	sa.sa_handler = handler;
	sa.sa_flags = 0;
	sigaction(sig, &sa, 0);
}

static void parse_bmpanel2_args(int argc, char **argv)
{
	struct argument args[] = {
		ARG_BOOLEAN("usage", &show_usage, "show usage reminder", 0),
		ARG_BOOLEAN("version", &show_version, "print bmpanel2 version", 0),
		ARG_BOOLEAN("list", &show_list, "list available themes", 0),
		ARG_STRING("theme", &theme_override, "override config theme parameter", 0),
		ARG_END
	};
	parse_args(args, argc, argv, bmpanel2_version_str);

	if (show_usage) {
		printf(BMPANEL2_USAGE);
		exit(0);
	}
	if (show_version) {
		printf(BMPANEL2_VERSION_STR);
		exit(0);
	}
	if (show_list) {
		list_themes();
		exit(0);
	}
}

int main(int argc, char **argv)
{
	g_thread_init(0);
	if (!g_thread_supported())
		XDIE("bmpanel2 requires glib with thread support enabled");
	parse_bmpanel2_args(argc, argv);
	load_settings();
	if (load_theme(&theme, theme_override) < 0)
		XDIE("Failed to load theme");
	clean_image_cache(0);
	
	register_widget_interface(&desktops_interface);
	register_widget_interface(&taskbar_interface);
	register_widget_interface(&clock_interface);
	register_widget_interface(&decor_interface);
	register_widget_interface(&systray_interface);
	register_widget_interface(&launchbar_interface);
	register_widget_interface(&empty_interface);

	init_panel(&p, &theme, -1, -1, -1);

	mysignal(SIGINT, sigint_handler);
	mysignal(SIGTERM, sigterm_handler);
	mysignal(SIGUSR1, sigusr1_handler);

	panel_main_loop(&p);
	
	free_panel(&p);
	free_config_format_tree(&theme);
	clean_static_buf();
	clean_image_cache(1);
	free_settings();
	xmemstat(0, 0, 1);
	return EXIT_SUCCESS;
}
