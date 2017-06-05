#pragma once

char **get_XDG_DATA_DIRS(size_t *len);
char **get_XDG_CONFIG_DIRS(size_t *len);

void free_XDG(char **ptrs);
