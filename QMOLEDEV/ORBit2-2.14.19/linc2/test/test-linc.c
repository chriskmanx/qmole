#include "config.h"
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#include <linc/linc.h>
#include "linc-private.h"
#include "linc-compat.h"

#define SYS_SOCKET_BUFFER_MAX (512 * 1024)
#define BUFFER_MAX 1024

static void
test_protos (void)
{
	LinkProtocolInfo *info;

	info = link_protocol_all ();

	fprintf (stderr, "Available protocols: {\n");

	while (info && info->name) {
		fprintf (stderr, "\t'%8s': %2d, %3d, %2d, 0x%.4x [%c%c%c%c%c]\n",
			 info->name, info->family, info->addr_len,
			 info->stream_proto_num, info->flags,
			 info->setup ?        's' : '-',
			 info->destroy ?      'd' : '-',
			 info->get_sockaddr ? 'a' : '-',
			 info->get_sockinfo ? 'i' : '-',
			 info->is_local ?     'l' : '-');
		info++;
	}
	
	fprintf (stderr, " }\n");
}

static void
init_tmp (void)
{
	char *dir;
	const char *user = g_get_user_name ();

	dir = g_build_filename (g_get_tmp_dir (),
				g_strconcat ("orbit-", user, NULL),
				NULL);
	  

	link_set_tmpdir (dir);

	g_free (dir);
}

static GType    test_server_cnx_type = 0;
static GType    test_client_cnx_type = 0;
static gboolean connected = FALSE;

static LinkConnection *
test_server_create_connection (LinkServer *cnx)
{
	GType t;

	t = test_server_cnx_type ? test_server_cnx_type : link_connection_get_type ();

	connected = TRUE;

	return g_object_new (t, NULL);
}

static void
create_server (LinkServer **server)
{
	LinkServerClass *klass;

	klass = g_type_class_ref (link_server_get_type ());
	klass->create_connection = test_server_create_connection;

	*server = g_object_new (link_server_get_type (), NULL);
	
#ifdef G_OS_WIN32
	g_assert (link_server_setup (*server, "IPv4", NULL, "1234",
				     LINK_CONNECTION_NONBLOCKING));
#else	
	g_assert (link_server_setup (*server, "UNIX", NULL, NULL,
				     LINK_CONNECTION_NONBLOCKING));
#endif
	g_object_add_weak_pointer (G_OBJECT (*server),
				   (gpointer *) server);
}

static void
create_client (LinkServer *server, LinkConnection **client)
{
	*client = link_connection_initiate
		(test_client_cnx_type ? test_client_cnx_type :
		 link_connection_get_type (),
#ifdef G_OS_WIN32
		 "IPv4",
#else
		 "UNIX",
#endif
		 server->local_host_info,
		 server->local_serv_info,
		 LINK_CONNECTION_NONBLOCKING,
		 NULL);
	g_assert (*client != NULL);

	g_object_add_weak_pointer (G_OBJECT (*client),
				   (gpointer *) client);
}

#ifdef HAVE_SYS_WAIT_H

static gboolean 
test_broken_cnx_handle_input (LinkConnection *cnx)
{
	glong  ret;
	guchar buffer;

	ret = link_connection_read (cnx, &buffer, 1, FALSE);

	g_assert (ret == LINK_IO_FATAL_ERROR);

	link_connection_state_changed (cnx, LINK_DISCONNECTED);

	return TRUE;
}

static void
test_broken_cnx_class_init (LinkConnectionClass *klass)
{
	klass->handle_input = test_broken_cnx_handle_input;
}

static GType
test_get_broken_cnx_type (void)
{
	static GType object_type = 0;

	if (!object_type) {
		static const GTypeInfo object_info = {
			sizeof (LinkConnectionClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) test_broken_cnx_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (LinkConnection),
			0,              /* n_preallocs */
			(GInstanceInitFunc) NULL
		};
      
		object_type = g_type_register_static (
			LINK_TYPE_CONNECTION, "TestConnection",
			&object_info, 0);
	}

	return object_type;
}

static void
broken_cb (LinkConnection *cnx, gpointer user_data)
{
	g_assert (user_data == NULL);

	exit (13);
}

static void
test_broken (void)
{
	LinkServer     *server;
	LinkConnection *client;
	pid_t           child;
	int             status;

	fprintf (stderr, "Testing 'broken' ...\n");

	create_server (&server);

	if ((child = fork ()) == 0) { /* child */
		test_client_cnx_type = test_get_broken_cnx_type ();
		create_client (server, &client);
		test_client_cnx_type = 0;

		g_signal_connect (G_OBJECT (client), "broken",
				  G_CALLBACK (broken_cb), NULL);

		g_object_unref (G_OBJECT (server));
		g_assert (server == NULL);

		link_main_loop_run ();

		g_assert_not_reached ();
	}

	while (!connected)
		link_main_iteration (FALSE);
	connected = FALSE;

	g_object_unref (G_OBJECT (server));
	g_assert (server == NULL);

	waitpid (child, &status, 0);
	g_assert (WIFEXITED (status) && WEXITSTATUS (status) == 13);
}

#endif

static GIOCondition
knobble_watch (LinkWatch *watch, GIOCondition new_cond)
{
	GIOCondition   old_cond;

	g_assert (watch != NULL);

	old_cond = ((LinkUnixWatch *) watch->link_source)->condition;

	g_assert (old_cond == ((LinkUnixWatch *) watch->main_source)->condition);
	
	link_watch_set_condition (watch, new_cond);

	return old_cond;
}

typedef struct {
	int             status;
	GIOCondition    old_cond;
	LinkConnection *s_cnx;
} BlockingData;

static void
blocking_cb (LinkConnection *cnx,
	     gulong          buffer_size,
	     gpointer        user_data)
{
	BlockingData *bd = user_data;

	if (bd->status < 3)
		fprintf (stderr, " buffer %ld\n", buffer_size);

	bd->status++;

	if (buffer_size == BUFFER_MAX) {
		knobble_watch (bd->s_cnx->priv->tag, bd->old_cond);

		/* flush the queue to other side */
		while (cnx->priv->write_queue != NULL &&
		       cnx->status == LINK_CONNECTED)
			link_main_iteration (FALSE);

		g_assert (cnx->status == LINK_CONNECTED);
	}
}

static gboolean 
test_blocking_cnx_handle_input (LinkConnection *cnx)
{
	static  gulong idx = 0;
	glong   size, i;
	guint32 buffer[1024];

	size = link_connection_read (cnx, (guchar *) buffer, 512, TRUE);
	g_assert (size != -1);
	g_assert ((size & 0x3) == 0);
	g_assert (size <= 512);

	for (i = 0; i < (size >> 2); i++)
		g_assert (buffer [i] == idx++);

	return TRUE;
}

static void
test_blocking_cnx_class_init (LinkConnectionClass *klass)
{
	klass->handle_input = test_blocking_cnx_handle_input;
}

static GType
test_get_blocking_cnx_type (void)
{
	static GType object_type = 0;

	if (!object_type) {
		static const GTypeInfo object_info = {
			sizeof (LinkConnectionClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) test_blocking_cnx_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (LinkConnection),
			0,              /* n_preallocs */
			(GInstanceInitFunc) NULL
		};
      
		object_type = g_type_register_static (
			LINK_TYPE_CONNECTION, "TestConnection",
			&object_info, 0);
	}

	return object_type;
}

static void
test_blocking (void)
{
	BlockingData    bd;
	LinkServer     *server;
	LinkConnection *client;
	LinkWriteOpts  *options;
	guint32         buffer[1024] = { 0 };
	glong           l;
	int             i;

	fprintf (stderr, "Testing blocking code ...\n");

	/* Create our own LinkConnection to verify input */
	test_server_cnx_type = test_get_blocking_cnx_type ();

	create_server (&server);
	create_client (server, &client);
	link_main_iteration (FALSE); /* connect */

	g_assert (server->priv->connections != NULL);
	bd.s_cnx = server->priv->connections->data;
	g_assert (bd.s_cnx != NULL);
	g_assert (bd.s_cnx->priv->tag != NULL);
	bd.old_cond = knobble_watch (bd.s_cnx->priv->tag, 0); /* stop it listening */

	options = link_write_options_new (FALSE);
	link_connection_set_max_buffer (client, BUFFER_MAX);
	g_signal_connect (G_OBJECT (client), "blocking",
			  G_CALLBACK (blocking_cb), &bd);
	client->options |= LINK_CONNECTION_BLOCK_SIGNAL;

	l = 0;
	bd.status = 0;
	for (i = 0; i < SYS_SOCKET_BUFFER_MAX; i+= 128) {
		int j;

		for (j = 0; j < 128/4; j++)
			buffer [j] = l++;

		link_connection_write (
			client, (guchar *) buffer, 128, options);
		if (client->status != LINK_CONNECTED)
			break;
	}

	g_assert (client->status == LINK_CONNECTED);
	g_assert (bd.status >= 3);

	link_connection_unref (client);
	g_assert (client == NULL);

	link_main_iteration (FALSE);

	g_object_unref (G_OBJECT (server));
	g_assert (server == NULL);

	test_server_cnx_type = 0;

	link_write_options_free (options);
}

static void
test_local_ipv4 (void)
{
	LinkSockLen saddr_len;
	LinkProtocolInfo *proto;
	struct sockaddr *saddr;
	struct sockaddr_in ipv4_addr = { 0 };

	fprintf (stderr, " IPv4\n");
	proto = link_protocol_find ("IPv4");
	g_assert (proto != NULL);

	ipv4_addr.sin_family = AF_INET;
	ipv4_addr.sin_port = 1234;
	memset (&ipv4_addr.sin_addr.s_addr, 0xaa, 4);
	g_assert (!link_protocol_is_local (
		proto, (struct sockaddr *)&ipv4_addr,
		sizeof (ipv4_addr)));

	saddr = link_protocol_get_sockaddr (
		proto, link_get_local_hostname (), NULL, &saddr_len);

	g_assert (link_protocol_is_local (proto, saddr, saddr_len));
	g_free (saddr);
}

static void
test_local_ipv6 (void)
{
#ifdef AF_INET6
	LinkProtocolInfo *proto;
	struct sockaddr_in6 ipv6_addr = { 0 };

	fprintf (stderr, " IPv6\n");
	proto = link_protocol_find ("IPv6");
	g_assert (proto != NULL);

	g_assert (proto != NULL);

	ipv6_addr.sin6_family = AF_INET6;
	ipv6_addr.sin6_port = 1234;
	memset (&ipv6_addr.sin6_addr.s6_addr, 0xaa, 16);
	g_assert (!link_protocol_is_local (
		proto, (struct sockaddr *)&ipv6_addr,
		sizeof (ipv6_addr)));
#else
	g_assert (link_protocol_find ("IPv6") == NULL);
#endif
}

static void
test_local (void)
{
#ifndef G_OS_WIN32
	LinkProtocolInfo *proto;
#endif

	fprintf (stderr, "Testing is_local checking ...\n");

	g_assert (!link_protocol_is_local (NULL, NULL, -1));

#ifndef G_OS_WIN32
	fprintf (stderr, " UNIX\n");
	proto = link_protocol_find ("UNIX");
	g_assert (proto != NULL);
	g_assert (link_protocol_is_local (proto, NULL, -1));
#endif
	test_local_ipv4 ();
	test_local_ipv6 ();
}

static void
verify_addr_is_loopback (guint8 *addr, int length)
{
	int i;

	if (length == 4)
		i = 0;

	else if (length == 16) {

		for (i = 0; i < 10; i++)
			if (addr [i] != 0)
				return;

		if (addr [i++] != 0xff || addr [i++] != 0xff)
			return;
	} else {
		i = 0;
		g_assert_not_reached ();
	}

	if (addr [i + 0] == 127 &&
	    addr [i + 1] == 0 &&
	    addr [i + 2] == 0 &&
	    addr [i + 3] == 1) {
		g_warning (" --- The reverse lookup of your hostname "
			   "is 127.0.0.1 you will not be able to "
			   "do inter-machine comms. ---");
		exit (0);
	}
}

static void
test_hosts_lookup (void)
{
	int i;
	struct hostent *hent;
	LinkProtocolInfo *proto;
	LinkSockLen saddr_len;
	struct sockaddr_in *addr;
		
	hent = gethostbyname (link_get_local_hostname ());
	g_assert (hent != NULL);

	fprintf (stderr, " official name '%s' aliases: ",
		 hent->h_name);

	for (i = 0; hent->h_aliases [i]; i++)
		fprintf (stderr, " '%s'", hent->h_aliases [i]);
	fprintf (stderr, "\n");

	verify_addr_is_loopback (hent->h_addr_list [0], hent->h_length);

	proto = link_protocol_find ("IPv4");
	addr = (struct sockaddr_in *)link_protocol_get_sockaddr (
		proto, "127.0.0.1", "1047", &saddr_len);
	g_assert (addr != NULL);
	g_assert (saddr_len == sizeof (struct sockaddr_in));
	
	verify_addr_is_loopback ((guint8 *) &addr->sin_addr.s_addr, saddr_len);
}

static void
test_host (void)
{
	char *portnum;
	char *hostname;
	LinkSockLen saddr_len;
	struct sockaddr *saddr;
	LinkProtocolInfo *proto;

	proto = link_protocol_find ("IPv4");
	g_assert (proto != NULL);
	g_assert (proto->get_sockinfo != NULL);

	saddr = link_protocol_get_sockaddr (
		proto, link_get_local_hostname (),
		NULL, &saddr_len);
	g_assert (saddr != NULL);

	g_assert (link_protocol_get_sockinfo (
		proto, saddr, &hostname, &portnum));

	g_free (saddr);

	fprintf (stderr, " '%s': '%s' \n",
		 link_get_local_hostname (),
		 hostname);

	g_free (hostname);
	g_free (portnum);

	test_hosts_lookup ();
}

static void
test_connected (void)
{
	LinkServer *server = NULL;
	LinkConnection *client = NULL;

	create_server (&server);
	g_assert (server != NULL);
	create_client (server, &client);
	g_assert (client != NULL);

	/* FIXME: this is horribly difficult to regression test properly: we fail */
	g_assert (link_connection_wait_connected (client) == LINK_CONNECTED);

	g_object_unref (G_OBJECT (server));
	link_connection_unref (client);
}

int
main (int argc, char **argv)
{	
	link_init (TRUE);
	init_tmp ();

	test_protos ();
	test_connected ();
#ifdef HAVE_SYS_WAIT_H
	test_broken ();
#endif
	test_blocking ();
	test_local ();
	test_host ();

	fprintf (stderr, "All tests passed successfully\n");
	
	return 0;
}
