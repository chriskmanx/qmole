#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "xdg.h"

static char **get_XDG_DIRS(size_t *len, const char *home_env, 
			   const char *home_default, const char *dirs_env, 
			   const char *dirs_default)
{
	char *dir_home;
	size_t dir_home_len;

	/* get dir_home */
	const char *xdg_dir_home = getenv(home_env);

	if (xdg_dir_home && xdg_dir_home[0] != '\0') {
		dir_home = xstrdup(xdg_dir_home);
		dir_home_len = strlen(dir_home);
	} else {
		const char *home = getenv("HOME");
		ENSURE(home != 0, "You must have HOME environment variable set");
		dir_home_len = strlen(home) + 1 + strlen(home_default) + 1;
		dir_home = xmalloc(dir_home_len);
		sprintf(dir_home, "%s/%s", home, home_default);
	}

	char *dirs;
	size_t dirs_len;

	/* get data_dirs */
	const char *xdg_dirs = getenv(dirs_env);

	if (xdg_dirs && xdg_dirs[0] != '\0') {
		dirs_len = dir_home_len + 1 + strlen(xdg_dirs) + 1;
		dirs = xmalloc(dirs_len);
		sprintf(dirs, "%s:%s", dir_home, xdg_dirs);
	} else {
		dirs_len = dir_home_len + 1 +
			strlen(dirs_default) + 1;
		dirs = xmalloc(dirs_len);
		sprintf(dirs, "%s:%s", dir_home, dirs_default);
	}

	xfree(dir_home);

	/* count dirs and change ':' to '\0' */
	size_t dirs_count = 1;
	char *tmp = dirs;
	while (*tmp) {
		if (*tmp == ':') {
			*tmp = '\0';
			dirs_count++;
		}
		tmp++;
	}

	/* alloc char pointer array and puts locations */
	size_t i;
	char **dirs_array = xmalloc(dirs_count * sizeof(char*));
	for (i = 0; i < dirs_count; ++i) {
		dirs_array[i] = dirs;
		while (*dirs) {
			dirs++;
		}
		dirs++;
	}

	*len = dirs_count;
	return dirs_array;
}

char **get_XDG_DATA_DIRS(size_t *len)
{
	return get_XDG_DIRS(len, "XDG_DATA_HOME", ".local/share", "XDG_DATA_DIRS",
			    "/usr/local/share:/usr/share");
}

char **get_XDG_CONFIG_DIRS(size_t *len)
{
	return get_XDG_DIRS(len, "XDG_CONFIG_HOME", ".config", "XDG_CONFIG_DIRS",
			    "/etc/xdg");
}

void free_XDG(char **ptrs)
{
	xfree(ptrs[0]);
	xfree(ptrs);
}
