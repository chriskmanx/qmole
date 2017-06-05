
#include "control/gkd-control.h"
#include "testing/testing.h"

#include <pwd.h>
#include <stdlib.h>
#include <unistd.h>

static int
run (void)
{
	const char *directory;
	const gchar *env[] = { NULL };
	gchar **envp, **e;

	directory = g_getenv ("GNOME_KEYRING_CONTROL");
	g_return_val_if_fail (directory, 1);

	envp = gkd_control_initialize (directory, "pkcs11,ssh,secret", env);
	if (envp == NULL)
		return 1;

	for (e = envp; *e; ++e)
		g_printerr ("%s\n", *e);
	g_strfreev (envp);

	return 0;
}

#include "testing/testing.c"
