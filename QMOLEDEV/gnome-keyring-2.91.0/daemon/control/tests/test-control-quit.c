
#include "control/gkd-control.h"
#include "testing/testing.h"

#include <pwd.h>
#include <stdlib.h>
#include <unistd.h>

static int
run (void)
{
	const char *directory;

	directory = g_getenv ("GNOME_KEYRING_CONTROL");
	g_return_val_if_fail (directory, 1);

	if (!gkd_control_quit (directory))
		return 1;

	g_printerr ("success quitting daemon\n");

	return 0;
}

#include "testing/testing.c"
