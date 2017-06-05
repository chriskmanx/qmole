#include <stdio.h>
#include "xdg.h"
#include "settings.h"

struct config_format_tree g_settings;

#define BMPANEL2_CONFIG_FILE "bmpanel2/bmpanel2rc"

void load_settings()
{
	char buf[4096];
	size_t config_dirs_len;
	char **config_dirs = get_XDG_CONFIG_DIRS(&config_dirs_len);
	int found = 0;

	size_t i;
	for (i = 0; i < config_dirs_len; ++i) {
		snprintf(buf, sizeof(buf), "%s/" BMPANEL2_CONFIG_FILE, 
			 config_dirs[i]);
		buf[sizeof(buf)-1] = '\0';
		if (is_file_exists(buf)) {
			found = 1;
			break;
		}
	}
	free_XDG(config_dirs);

	if (found)
		load_config_format_tree(&g_settings, buf);
}

void free_settings()
{
	if (g_settings.buf)
		free_config_format_tree(&g_settings);
}
