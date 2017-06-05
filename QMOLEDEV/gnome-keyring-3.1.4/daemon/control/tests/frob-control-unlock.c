
#include "control/gkd-control.h"

#include "egg/egg-secure-memory.h"

#include <pwd.h>
#include <unistd.h>

EGG_SECURE_GLIB_DEFINITIONS ();

int
main (int argc, char *argv[])
{
	const char *password;
	const char *directory;

	password = getpass ("Unlock: ");
	g_return_val_if_fail (password, 1);

	directory = g_getenv ("GNOME_KEYRING_CONTROL");
	g_return_val_if_fail (directory, 1);

	gkd_control_unlock (directory, password);
	return 0;
}
