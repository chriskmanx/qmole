/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Authors: Vladimir Nadvornik, Laurent Monin
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef _DEBUG_H
#define _DEBUG_H

#include <glib.h>

#define DOMAIN_DEBUG "debug"
#define DOMAIN_INFO  "info"

void log_domain_printf(const gchar *domain, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
#define log_printf(...) log_domain_printf(DOMAIN_INFO, __VA_ARGS__)

#ifdef DEBUG

#define DEBUG_LEVEL_MIN 0
#define DEBUG_LEVEL_MAX 4

gint get_debug_level(void);
void set_debug_level(gint new_level);
void debug_level_add(gint delta);
gint required_debug_level(gint level);
const gchar *get_exec_time(void);
void init_exec_time(void);

#define DEBUG_N(n, ...) do \
				{ \
				gint debug_level = get_debug_level(); \
				if (debug_level >= (n)) 	\
					{ 		\
					if (debug_level != 1) log_domain_printf(DOMAIN_DEBUG, "%s:%d: ", __FILE__, __LINE__); \
					log_domain_printf(DOMAIN_DEBUG, __VA_ARGS__); \
					log_domain_printf(DOMAIN_DEBUG, "\n"); \
					} \
				} while (0)

#else /* DEBUG */

#define get_debug_level() (0)
#define set_debug_level(new_level) do { } while(0)
#define debug_level_add(delta) do { } while(0)
#define required_debug_level(level) (0)
#define get_exec_time() ""
#define init_exec_time() do { } while(0)

#define DEBUG_N(n, ...)  do { } while(0)

#endif /* DEBUG */

#define DEBUG_0(...) DEBUG_N(0, __VA_ARGS__)
#define DEBUG_1(...) DEBUG_N(1, __VA_ARGS__)
#define DEBUG_2(...) DEBUG_N(2, __VA_ARGS__)
#define DEBUG_3(...) DEBUG_N(3, __VA_ARGS__)
#define DEBUG_4(...) DEBUG_N(4, __VA_ARGS__)


#endif /* _DEBUG_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
