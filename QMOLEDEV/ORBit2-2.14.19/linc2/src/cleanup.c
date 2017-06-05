#include <config.h>

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <glib.h>

/*
 * Two timeouts - waiting while there are other
 *  ( in ms )     handles to be checked.
 *              - waiting after all other handles have
 *                been checked.
 */
#define SHORT_TIMEOUT  10
#define LONG_TIMEOUT 1000

static int total_count = 0;
static int cleaned_count = 0;

#include <linc-compat.h>

#ifdef AF_UNIX

typedef struct {
	char *name;
	int   fd;
} SocketEntry;

static SocketEntry *
new_socket_entry (const char *dir, const char *fname)
{
	SocketEntry *se = g_new0 (SocketEntry, 1);

	se->name = g_build_filename (dir, fname, NULL);
	se->fd = -1;

	return se;
}

static void
free_socket_entry (SocketEntry *se)
{
	g_free (se->name);
	if (se->fd >= 0)
		close (se->fd);
	g_free (se);
}

static GList *
read_sockets (const char *dir)
{
	DIR   *dirh;
	GList *files = NULL;
	struct dirent *dent;

	dirh = opendir (dir);
	if (!dirh)
		return NULL;

	while ((dent = readdir (dirh))) {
		if (strncmp (dent->d_name, "linc-", 5))
			continue;

		files = g_list_prepend (
			files, new_socket_entry (
				dir, dent->d_name));
	}
	closedir (dirh);

	return files;
}

typedef enum {
	SOCKET_DEAD_NOW,
	SOCKET_PENDING,
	SOCKET_AGAIN,
	SOCKET_ALIVE
} SocketStatus;

static SocketStatus
open_socket (SocketEntry *se)
{
	int saddr_len, ret;
	struct sockaddr_un saddr;

	g_return_val_if_fail (se != NULL, SOCKET_DEAD_NOW);
	g_return_val_if_fail (se->fd == -1, SOCKET_DEAD_NOW);
	g_return_val_if_fail (se->name != NULL, SOCKET_DEAD_NOW);

	saddr.sun_family = AF_UNIX;

	g_snprintf (saddr.sun_path, sizeof (saddr.sun_path),
		    "%s", se->name);

	se->fd = socket (AF_UNIX, SOCK_STREAM, 0);
	g_assert (se->fd >= 0);

	if (fcntl (se->fd, F_SETFL, O_NONBLOCK) < 0)
		g_error ("Failed to set fd non-blocking");
	
	saddr_len = sizeof (struct sockaddr_un) -
		sizeof (saddr.sun_path) + strlen (saddr.sun_path);

	do {
		ret = connect (se->fd, &saddr, saddr_len);
	} while (ret < 0 && errno == EINTR);

	if (ret >= 0)
		return SOCKET_ALIVE;
	else {
		switch (errno) {
		case EINPROGRESS:
			return SOCKET_PENDING;
		case ECONNREFUSED:
			return SOCKET_DEAD_NOW;
		case EAGAIN:
			return SOCKET_AGAIN;
		case EBADF:
			g_error ("Bad bug fd %d", se->fd);
			break;
		default:
			g_warning ("Error '%s' on socket %d",
				   g_strerror (errno), se->fd);
			break;
		}
	}

	return SOCKET_DEAD_NOW;
}

static GList *
poll_open (GList *files, int *pending, int timeout)
{
	if (!files)
		return NULL;

	g_print ("FIXME: should poll unknown descriptors for a bit");

	/* FIXME: we should really do something clever here,
	 * poll for a while, wait for nice connected / bad
	 * signals on the sockets, etc - but what we have
	 * works well for now */
	while (files && *pending > 0) {
		free_socket_entry (files->data);
		files = g_list_delete_link (files, files);
		(*pending)--;
	}

	return files;
}

static void
clean_dir (const char *dir)
{
	int open_max;
	int pending = 0;
	GList *files, *l, *next;

	open_max = sysconf (_SC_OPEN_MAX);

	files = read_sockets (dir);

	for (l = files; l; l = next) {
		SocketEntry *se = l->data;
		SocketStatus status;

		next = l->next;
		
		status = open_socket (se);

		switch (status) {
		case SOCKET_DEAD_NOW:
			cleaned_count++;
			unlink (se->name);
			/* drop through */
		case SOCKET_ALIVE:
			files = g_list_delete_link (files, l);
			free_socket_entry (se);
			total_count++;
			break;
		case SOCKET_AGAIN:
		case SOCKET_PENDING:
			pending++;
			break;
		}

		while (pending >= open_max)
			files = poll_open (
				files, &pending, SHORT_TIMEOUT);
	}

	files = poll_open (files, &pending, LONG_TIMEOUT);

	while (files) {
		free_socket_entry (files->data);
		files = g_list_delete_link (files, files);
	}
}

#endif /* AF_UNIX */

int
main (int argc, char **argv)
{
	char *dir;

	dir = g_strdup_printf ("%s/orbit-%s", g_get_tmp_dir (), g_get_user_name ());

#ifdef AF_UNIX
	clean_dir (dir);
#endif /* AF_UNIX */

	g_free (dir);

	printf ("Cleaned %d files %d still live\n",
		cleaned_count,
		total_count - cleaned_count);

	return 0;
}
