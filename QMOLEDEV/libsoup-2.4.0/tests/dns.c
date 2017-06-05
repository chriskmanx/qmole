#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include "libsoup/soup-address.h"

static GMainLoop *loop;
static int nlookups = 0;

static void
resolve_callback (SoupAddress *addr, guint status, gpointer data)
{
	if (status == SOUP_STATUS_OK) {
		printf ("Name:    %s\n", soup_address_get_name (addr));
		printf ("Address: %s\n", soup_address_get_physical (addr));
	} else {
		printf ("Name:    %s\n", soup_address_get_name (addr));
		printf ("Error:   %s\n", soup_status_get_phrase (status));
	}
	printf ("\n");

	nlookups--;
	if (nlookups == 0)
		g_main_loop_quit (loop);
}

static void
usage (void)
{
	fprintf (stderr, "Usage: dns hostname ...\n");
	exit (1);
}

int
main (int argc, char **argv)
{
	SoupAddress *addr;
	int i;

	if (argc < 2)
		usage ();

	g_type_init ();
	g_thread_init (NULL);

	for (i = 1; i < argc; i++) {
		addr = soup_address_new (argv[i], 0);
		if (!addr) {
			fprintf (stderr, "Could not parse address %s\n", argv[1]);
			exit (1);
		}

		soup_address_resolve_async (addr, NULL, NULL,
					    resolve_callback, NULL);
		nlookups++;
	}

	loop = g_main_loop_new (NULL, TRUE);
	g_main_run (loop);
	g_main_loop_unref (loop);

	return 0;
}
