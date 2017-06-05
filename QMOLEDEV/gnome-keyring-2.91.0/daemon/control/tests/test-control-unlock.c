
#include "control/gkd-control.h"
#include "testing/testing.h"

#include <pwd.h>
#include <unistd.h>

static int
run (void)
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

#include "testing/testing.c"
