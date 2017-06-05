#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include "server.h"

static GTimer *timer;

static void
test_server_info_load (void)
{
	int i;
	char *dirs [] = { SERVERINFODIR, NULL };
	Bonobo_ServerInfoList servers;
	GHashTable *hash = NULL;

	fprintf (stderr, "Testing server info load ...");

	g_timer_start (timer);
	for (i = 0; i < 10; i++)
		bonobo_server_info_load (dirs, &servers, &hash,
					 bonobo_activation_hostname_get ());

	fprintf (stderr, " %g(ms)\n",
		 g_timer_elapsed (timer, NULL) * 1000.0 / 10);
}

int
main (int argc, char *argv[])
{
	free (malloc (8));

	timer = g_timer_new ();
	g_timer_start (timer);

	add_initial_locales ();

	test_server_info_load ();

	if (g_getenv ("_MEMPROF_SOCKET")) {
		g_warning ("Waiting for memprof\n");
		g_main_context_iteration (NULL, TRUE);
	}

	return 0;
}
