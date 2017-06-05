#pragma once

#include "config-parser.h"

extern struct config_format_tree g_settings;

void load_settings();
void free_settings();
