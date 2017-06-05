/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-debug.c: A runtime-controllable debugging API.
 *
 * Author:
 *   Jaka Mocnik  <jaka@gnu.org>
 */

#include <stdio.h>
#include <unistd.h>
#include <glib.h>
#include <bonobo/bonobo-debug.h>

BonoboDebugFlags _bonobo_debug_flags;
static FILE *_bonobo_debug_file;

void
bonobo_debug_init()
{
	static GDebugKey debug_keys[] = {
		{ "refs",       BONOBO_DEBUG_REFS },
		{ "aggregate",  BONOBO_DEBUG_AGGREGATE },
		{ "lifecycle",  BONOBO_DEBUG_LIFECYCLE },
		{ "running",    BONOBO_DEBUG_RUNNING },
		{ "object",     BONOBO_DEBUG_OBJECT },
	};
	const char *env_string;

	_bonobo_debug_flags = BONOBO_DEBUG_NONE;
	env_string = g_getenv ("BONOBO_DEBUG");
	if (env_string)
	  _bonobo_debug_flags |=
			  g_parse_debug_string (env_string,
						      debug_keys,
						      G_N_ELEMENTS (debug_keys));
	_bonobo_debug_file = NULL;
	env_string = g_getenv ("BONOBO_DEBUG_DIR");
	if(env_string) {
	  gchar *dbg_filename;
	  dbg_filename = g_strdup_printf("%s/bonobo-debug-%d", env_string, getpid());
	  _bonobo_debug_file = fopen(dbg_filename, "w");
	  g_free(dbg_filename);
	}
	if(_bonobo_debug_file == NULL)
	  _bonobo_debug_file = stdout;
}

void
bonobo_debug_print (char *name, char *fmt, ...)
{
	va_list args;
           
	va_start (args, fmt);
	
	fprintf (_bonobo_debug_file, "[%06d]:%-15s ", getpid (), name); 
	vfprintf (_bonobo_debug_file, fmt, args);
	fprintf (_bonobo_debug_file, "\n"); 
	fflush (_bonobo_debug_file);

	va_end (args);
}
