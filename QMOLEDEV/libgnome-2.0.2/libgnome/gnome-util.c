/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * Copyright (C) 1999, 2000 Red Hat, Inc.
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

/*
 *
 * Gnome utility routines.
 * (C)  1997, 1998, 1999 the Free Software Foundation.
 *
 * Author: Miguel de Icaza, 
 */
#include <config.h>

/* needed for S_ISLNK with 'gcc -ansi -pedantic' on GNU/Linux */
#ifndef _BSD_SOURCE
#  define _BSD_SOURCE 1
#endif
#include <sys/types.h>

#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <glib.h>
#include <sys/stat.h>
#include <pwd.h>
#include <limits.h>
#include <libgnome/gnome-program.h>
#include <libgnome/gnome-util.h>

/**
 * gnome_util_user_shell:
 *
 * Retrieves the user's preferred shell.
 *
 * Returns: A newly allocated string that is the path to the shell.
 */
char *
gnome_util_user_shell (void)
{
	struct passwd *pw;
	int i;
	const char *shell;
	static char *shells [] = {
		/* Note that on some systems shells can also
		 * be installed in /usr/bin */
		"/bin/bash", "/usr/bin/bash",
		"/bin/zsh", "/usr/bin/zsh",
		"/bin/tcsh", "/usr/bin/tcsh",
		"/bin/ksh", "/usr/bin/ksh",
		"/bin/csh", "/bin/sh", 0
	};

	if (geteuid () == getuid () &&
	    getegid () == getgid ()) {
		/* only in non-setuid */
		if ((shell = g_getenv ("SHELL"))){
			return g_strdup (shell);
		}
	}
	pw = getpwuid(getuid());
	if (pw && pw->pw_shell) {
		return g_strdup (pw->pw_shell);
	} 

	for (i = 0; shells [i]; i++) {
		if (access (shells [i], X_OK) == 0) {
			return g_strdup (shells[i]);
		}
	}

	/* If /bin/sh doesn't exist, your system is truly broken.  */
	abort ();

	/* Placate compiler.  */
	return NULL;
}

/**
 * g_extension_pointer:
 * @path: A filename or file path.
 *
 * Extracts the extension from the end of a filename (the part after the final
 * '.' in the filename).
 *
 * Returns: A pointer to the extension part of the filename, or a
 * pointer to the end of the string if the filename does not
 * have an extension.
 */
const char *
g_extension_pointer (const char * path)
{
	char * s, * t;
	
	g_return_val_if_fail(path != NULL, NULL);

	/* get the dot in the last element of the path */
	t = strrchr(path, G_DIR_SEPARATOR);
	if (t != NULL)
		s = strrchr(t, '.');
	else
		s = strrchr(path, '.');
	
	if (s == NULL)
		return path + strlen(path); /* There is no extension. */
	else {
		++s;      /* pass the . */
		return s;
	}
}

/**
 * gnome_setenv:
 * @name: An environment variable name.
 * @value: The value to assign to the environment variable.
 * @overwrite: If %TRUE, overwrite the existing @name variable in the
 * environment.
 * 
 * Adds "@name=@value" to the environment. Note that on systems without setenv,
 * this leaks memory so please do not use inside a loop or anything like that.
 * The semantics are the same as the glibc setenv() (if setenv() exists, it is
 * used).
 *
 * If @overwrite is %FALSE and the variable already exists in the environment,
 * then %0 is returned and the value is not changed.
 *
 * Returns: %0 on success, %-1 on error
 * 
 **/
int
gnome_setenv (const char *name, const char *value, gboolean overwrite)
{
#if defined (HAVE_SETENV)
	return setenv (name, value != NULL ? value : "", overwrite);
#else
	char *string;
	
	if (! overwrite && g_getenv (name) != NULL) {
		return 0;
	}
	
	/* This results in a leak when you overwrite existing
	 * settings. It would be fairly easy to fix this by keeping
	 * our own parallel array or hash table.
	 */
	string = g_strconcat (name, "=", value, NULL);
	return putenv (string);
#endif
}

/**
 * gnome_unsetenv:
 * @name: The environment variable to unset.
 * 
 * Description: Removes @name from the environment.
 * In case there is no native implementation of unsetenv,
 * this could cause leaks depending on the implementation of
 * environment.
 * 
 **/
void
gnome_unsetenv (const char *name)
{
#if defined (HAVE_SETENV)
	unsetenv (name);
#else
	extern char **environ;
	int i, len;

	len = strlen (name);
	
	/* Mess directly with the environ array.
	 * This seems to be the only portable way to do this.
	 */
	for (i = 0; environ[i] != NULL; i++) {
		if (strncmp (environ[i], name, len) == 0
		    && environ[i][len + 1] == '=') {
			break;
		}
	}
	while (environ[i] != NULL) {
		environ[i] = environ[i + 1];
		i++;
	}
#endif
}

/**
 * gnome_clearenv:
 * 
 * Description: Clears out the environment completely.
 * In case there is no native implementation of clearenv,
 * this could cause leaks depending on the implementation
 * of environment.
 * 
 **/
void
gnome_clearenv (void)
{
#ifdef HAVE_CLEARENV
	clearenv ();
#else
	extern char **environ;
	environ[0] = NULL;
#endif
}


/* Deprecated: */
/**
 * gnome_is_program_in_path:
 * @program: A program name.
 *
 * Deprecated, use #g_find_program_in_path
 *
 * Returns: %NULL if program is not on the path or a string 
 * allocated with g_malloc() with the full path name of the program
 * found.
 */

