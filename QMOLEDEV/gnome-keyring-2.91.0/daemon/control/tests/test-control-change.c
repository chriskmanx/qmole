
#include "control/gkd-control.h"
#include "testing/testing.h"

#include <pwd.h>
#include <unistd.h>

static int
run (void)
{
	gchar *original;
	const char *password;
	const char *directory;

	directory = g_getenv ("GNOME_KEYRING_CONTROL");
	g_return_val_if_fail (directory, 1);

	original = g_strdup (getpass ("Original: "));
	g_return_val_if_fail (original, 1);

	password = getpass ("New Password: ");
	g_return_val_if_fail (password, 1);

	gkd_control_change_lock (directory, original, password);

	g_free (original);
	return 0;
}

#include "testing/testing.c"
