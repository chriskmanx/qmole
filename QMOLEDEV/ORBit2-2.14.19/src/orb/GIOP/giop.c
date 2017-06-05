#include <config.h>
#include <stdio.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#ifdef HAVE_UTIME_H
#  include <utime.h>
#endif

#include "giop-private.h"
#include "giop-debug.h"
#include <orbit/util/orbit-genrand.h>
#include <glib/gstdio.h>

/* FIXME: need to clean this up at shutdown */
static int      corba_wakeup_fds[2];
#define WAKEUP_POLL  corba_wakeup_fds [0]
#define WAKEUP_WRITE corba_wakeup_fds [1]
static GSource *giop_main_source = NULL;
static GIOPThread *giop_main_thread = NULL;

/* Incoming dispatch thread pool */
static GThreadPool *giop_thread_pool    = NULL;
static GMutex      *giop_pool_hash_lock = NULL;
static GHashTable  *giop_pool_hash      = NULL;

const char giop_version_ids [GIOP_NUM_VERSIONS][2] = {
	{1,0},
	{1,1},
	{1,2}
};

#define S_PRINT(a) g_warning a

static gboolean
test_safe_socket_dir (const char *dirname)
{
	struct stat statbuf;

	if (g_stat (dirname, &statbuf) != 0) {
		S_PRINT (("Can not stat %s\n", dirname));
		return FALSE;
	}
	
#ifndef G_PLATFORM_WIN32
	if (getuid() != 0 && statbuf.st_uid != getuid ()) {
		S_PRINT (("Owner of %s is not the current user\n", dirname));
		return FALSE;
	}
	
	if ((statbuf.st_mode & (S_IRWXG|S_IRWXO)) ||
	    !S_ISDIR (statbuf.st_mode)) {
		S_PRINT (("Wrong permissions for %s\n", dirname));
		return FALSE;
	}
#endif

	return TRUE;
}

/*
 *   In the absence of being told which directory to
 * use, we have to scan /tmp/orbit-$USER-* to work out
 * which directory to use.
 */
static char *
scan_socket_dir (const char *dir, const char *prefix)
{
	int prefix_len;
	int len;
	char *cur_dir = NULL;
	GDir   *dirh;
	const char *dent;
	char *prefix_with_hyphen;

	g_return_val_if_fail (dir != NULL, NULL);
	g_return_val_if_fail (prefix != NULL, NULL);
	
	dirh = g_dir_open (dir, 0, NULL);
	if (!dirh)
		return NULL;

	prefix_with_hyphen = g_strdup_printf ("%s-", prefix);
	prefix_len = strlen (prefix_with_hyphen);

	while ((dent = g_dir_read_name (dirh))) {
		char *name;
		len = (strlen (dent) > strlen (prefix)) ? strlen (dent) : strlen (prefix);

		if (strncmp (dent, prefix, len) && 
		    strncmp (dent, prefix_with_hyphen, prefix_len))
			continue;

		name = g_build_filename (dir, dent, NULL);

		/* Check it's credentials */
		if (!test_safe_socket_dir (name)) {
			dprintf (GIOP, "DOS attack with '%s'\n", name);
			g_free (name);
			continue;
		}
		
		/* Sort into some repeatable order */
		if (!cur_dir || strcmp (cur_dir, name) > 0) {
			g_free (cur_dir);
			cur_dir = name;
		} else
			g_free (name);
	}
	g_dir_close (dirh);

	g_free (prefix_with_hyphen);

	return cur_dir;
}

static void
giop_tmpdir_init (void)
{
	const char *tmp_root;
	char *dirname;
	char *safe_dir = NULL;
	long iteration = 0;
	const gchar *env_dir;
	static gboolean inited = FALSE;

	if (inited)
		return;
	inited = TRUE;

#ifndef G_OS_WIN32
	env_dir = g_getenv("ORBIT_SOCKETDIR");
	if (env_dir && test_safe_socket_dir (env_dir)) {
		link_set_tmpdir (env_dir);
		return;
	}
#endif

	tmp_root = g_get_tmp_dir ();
	dirname = g_strdup_printf ("orbit-%s",
				   g_get_user_name ());
	while (!safe_dir) {
		char *newname;

		safe_dir = scan_socket_dir (tmp_root, dirname);
		if (safe_dir) {
			dprintf (GIOP, "Have safe dir '%s'\n", safe_dir);
			link_set_tmpdir (safe_dir);
			break;
		}

		if (iteration == 0)
			newname = g_build_filename (tmp_root, dirname, NULL);
		else {
			struct {
				guint32 a;
				guint32 b;
			} id;

			ORBit_genuid_buffer ((guint8 *)&id, sizeof (id),
					     ORBIT_GENUID_OBJECT_ID);

			newname = g_strdup_printf (
				"%s" G_DIR_SEPARATOR_S "%s-%4x",
				tmp_root, dirname, id.b);
		}

		if (g_mkdir (newname, 0700) < 0) {
			switch (errno) {
			case EACCES:
				g_error ("I can't write to '%s', ORB init failed",
					 newname);
				break;
				
			case ENAMETOOLONG:
				g_error ("Name '%s' too long your system is broken",
					 newname);
				break;

			case ENOMEM:
#ifdef ELOOP
			case ELOOP:
#endif
			case ENOSPC:
			case ENOTDIR:
			case ENOENT:
				g_error ("Resource problem creating '%s'", newname);
				break;
				
			default: /* carry on going */
				break;
			}
		}
#if defined (HAVE_UTIME_H) && !defined (G_OS_WIN32)
		/* This seems pretty useless, forget it on Win32 */

		{ /* Hide some information ( apparently ) */
			struct utimbuf utb;
			memset (&utb, 0, sizeof (utb));
			utime (newname, &utb);
		}
#endif		
		/* Possible race - so we re-scan. */

		iteration++;
		g_free (newname);

		if (iteration == 1000)
			g_error ("Cannot find a safe socket path in '%s'", tmp_root);
	}

#ifndef G_OS_WIN32
	g_setenv ("ORBIT_SOCKETDIR", safe_dir, TRUE);
#endif
	g_free (safe_dir);
	g_free (dirname);
}

gboolean
giop_thread_safe (void)
{
	return link_thread_safe ();
}

gboolean
giop_thread_io (void)
{
	return link_thread_io ();
}

void
giop_dump (FILE *out, guint8 const *ptr, guint32 len, guint32 offset)
{
	guint32 lp,lp2;
	guint32 off;

	for (lp = 0;lp<(len+15)/16;lp++) {
		fprintf (out, "0x%.4x: ", offset + lp * 16);
		for (lp2=0;lp2<16;lp2++) {
			fprintf (out, "%s", lp2%4?" ":"  ");
			off = lp2 + (lp<<4);
			off<len?fprintf (out, "%.2x", ptr[off]):fprintf (out, "XX");
		}
		fprintf (out, " | ");
		for (lp2=0;lp2<16;lp2++) {
			off = lp2 + (lp<<4);
			fprintf (out, "%c", off<len?(ptr[off]>'!'&&ptr[off]<127?ptr[off]:'.'):'*');
		}
		fprintf (out, "\n");
	}
	fprintf (out, " --- \n");
}

void
giop_dump_send (GIOPSendBuffer *send_buffer)
{
	gulong nvecs;
	struct iovec *curvec;
	guint32 offset = 0;

	g_return_if_fail (send_buffer != NULL);

	nvecs = send_buffer->num_used;
	curvec = (struct iovec *) send_buffer->iovecs;

	fprintf (stderr, "Outgoing IIOP data:\n");
	while (nvecs-- > 0) {
		giop_dump (stderr, curvec->iov_base, curvec->iov_len, offset);
		offset += curvec->iov_len;
		curvec++;
	}
}

void
giop_dump_recv (GIOPRecvBuffer *recv_buffer)
{
	const char *status;

	g_return_if_fail (recv_buffer != NULL);

	if (recv_buffer->connection &&
	    LINK_CONNECTION (recv_buffer->connection)->status == LINK_CONNECTED)
		status = "connected";
	else
		status = "not connected";

	fprintf (stderr, "Incoming IIOP data: %s\n", status);

	giop_dump (stderr, (guint8 *)recv_buffer, sizeof (GIOPMsgHeader), 0);

	giop_dump (stderr, recv_buffer->message_body + 12,
		   recv_buffer->msg.header.message_size, 12);
}

static GIOPThread *
giop_thread_new (GMainContext *context)
{
	GIOPThread *tdata = g_new0 (GIOPThread, 1);

	tdata->lock = g_mutex_new ();
	tdata->incoming = g_cond_new ();
	tdata->wake_context = context;
	tdata->keys = NULL;
	tdata->async_ents = NULL;
	tdata->request_queue = NULL;

	if (giop_main_thread)
		tdata->request_handler = giop_main_thread->request_handler;

	return tdata;
}

static void
giop_thread_key_add_T (GIOPThread *tdata, gpointer key)
{
	/* We don't allow a key to be reused */
	gpointer reused = g_hash_table_lookup (giop_pool_hash, key);
	g_assert (!reused);
  
	tdata->keys = g_list_prepend (tdata->keys, key);
	
	g_hash_table_insert (giop_pool_hash, key, tdata);
}

static void
giop_thread_key_release_T (gpointer key)
{
	g_hash_table_remove (giop_pool_hash, key);
}

static void
giop_thread_free (GIOPThread *tdata)
{
	GList *l;

	if (tdata == giop_main_thread)
		giop_main_thread = NULL;
	
	if (giop_thread_safe ()) {
		g_mutex_lock (giop_pool_hash_lock);
		for (l = tdata->keys; l != NULL; l = l->next) {
			giop_thread_key_release_T (l->data);
		}
		g_mutex_unlock (giop_pool_hash_lock);
	}
	
	g_list_free (tdata->keys);
	tdata->keys = NULL;
	
	g_mutex_free (tdata->lock);
	tdata->lock = NULL;
	g_cond_free (tdata->incoming);
	tdata->incoming = NULL;

#ifdef G_ENABLE_DEBUG
	if (tdata->async_ents)
		g_warning ("Leaked async ents");
	if (tdata->request_queue)
		g_warning ("Leaked request queue");
#endif
	if (tdata->invoke_policies) {
		g_queue_free (tdata->invoke_policies);
		tdata->invoke_policies = NULL;
	}
	
	g_free (tdata);
}

static GPrivate *giop_tdata_private = NULL;

GIOPThread *
giop_thread_self (void)
{
	GIOPThread *tdata;

	if (!giop_thread_safe ())
		return NULL;

	if (!(tdata = g_private_get (giop_tdata_private))) {
		tdata = giop_thread_new (NULL);
		g_private_set (giop_tdata_private, tdata);
	}

	return tdata;
}


void
giop_thread_key_add (GIOPThread *tdata, gpointer key)
{
  g_mutex_lock (giop_pool_hash_lock);
  LINK_MUTEX_LOCK (tdata->lock);

  giop_thread_key_add_T (tdata, key);
  
  LINK_MUTEX_UNLOCK (tdata->lock);
  g_mutex_unlock (giop_pool_hash_lock);
}

void
giop_thread_key_release (gpointer key)
{
	GIOPThread *tdata;
	
	if (giop_thread_safe ()) {
		g_mutex_lock (giop_pool_hash_lock);
		tdata = g_hash_table_lookup (giop_pool_hash, key);
		if (tdata != NULL) {
			tdata->keys = g_list_remove (tdata->keys, key);
			giop_thread_key_release_T (key);
		}
		g_mutex_unlock (giop_pool_hash_lock);
	}
}

void
giop_thread_request_push_key (gpointer  key,
			      gpointer *poa_object,
			      gpointer *recv_buffer)
{
	GIOPThread *tdata, *new_tdata = NULL;

	g_mutex_lock (giop_pool_hash_lock);

	if (!(tdata = g_hash_table_lookup (giop_pool_hash, key))) {
		new_tdata = giop_thread_new (NULL);
		tdata = new_tdata;
		if (key)
			giop_thread_key_add_T (tdata, key);
		dprintf (GIOP, "Create new thread %p for op\n", tdata);
	} else
		dprintf (GIOP, "Re-use thread %p for op\n", tdata);

	giop_thread_request_push (tdata, poa_object, recv_buffer);

	if (new_tdata) 
		g_thread_pool_push (giop_thread_pool, tdata, NULL);

	g_mutex_unlock (giop_pool_hash_lock);
}

gboolean
giop_thread_same_key (gpointer key, gboolean no_key_default)
{
	gboolean same;
	GIOPThread *tdata;

	g_mutex_lock (giop_pool_hash_lock);

	if (!(tdata = g_hash_table_lookup (giop_pool_hash, key)))
		same = no_key_default;
	else
		same = tdata == giop_thread_self ();

	g_mutex_unlock (giop_pool_hash_lock);

	return same;
}

static gboolean
giop_mainloop_handle_input (GIOChannel     *source,
			    GIOCondition    condition,
			    gpointer        data)
{
	char c;
	GIOPThread *tdata = giop_thread_self ();

#ifdef HAVE_WINSOCK2_H
	recv (WAKEUP_POLL, &c, sizeof (c), 0);
#else
	read (WAKEUP_POLL, &c, sizeof (c));
#endif

	LINK_MUTEX_LOCK (tdata->lock);
	while (!giop_thread_queue_empty_T (tdata)) {
		LINK_MUTEX_UNLOCK (tdata->lock);
		giop_thread_queue_process (tdata);
		LINK_MUTEX_LOCK (tdata->lock);
	}
	LINK_MUTEX_UNLOCK (tdata->lock);

	return TRUE;
}

static void
giop_request_handler_thread (gpointer data, gpointer user_data)
{
	gboolean done;
	GList *l;
	GIOPThread *tdata = data;

	g_private_set (giop_tdata_private, tdata);

	dprintf (GIOP, "Thread %p woken to handle request\n", tdata);

	do {
		giop_thread_queue_process (tdata);

		g_mutex_lock (giop_pool_hash_lock);
		LINK_MUTEX_LOCK (tdata->lock);

		if ((done = giop_thread_queue_empty_T (tdata))) {
			for (l = tdata->keys; l != NULL; l = l->next)
				giop_thread_key_release_T (l->data);
			g_list_free (tdata->keys);
			tdata->keys = NULL;
		}

		LINK_MUTEX_UNLOCK (tdata->lock);
		g_mutex_unlock (giop_pool_hash_lock);

	} while (!done);

	dprintf (GIOP, "Thread %p returning to pool\n", tdata);

	giop_thread_free (tdata);
	g_private_set (giop_tdata_private, NULL);
}

const char *
ORBit_get_safe_tmp (void)
{
	giop_tmpdir_init ();
	return link_get_tmpdir ();
}

void
giop_init (gboolean thread_safe, gboolean blank_wire_data)
{
	link_init (thread_safe);

	if (giop_thread_safe ()) {
		GIOPThread *tdata;

		/* We need a destructor to clean up if giopthreads are used
		 * outside of ORBit controlled threads */
		giop_tdata_private = g_private_new ((GDestroyNotify)giop_thread_free);

		giop_main_thread = tdata = giop_thread_new (
			g_main_context_default ()); /* main thread */

		if (link_pipe (corba_wakeup_fds) < 0) /* cf. g_main_context_init_pipe */
			g_error ("Can't create CORBA main-thread wakeup pipe");

#ifdef HAVE_WINSOCK2_H
		{
			u_long yes = 1;
			ioctlsocket (WAKEUP_WRITE, FIONBIO, &yes);
		}
#else
		fcntl (WAKEUP_WRITE, F_SETFL, O_NONBLOCK);
#endif
		giop_main_source = link_source_create_watch (
			g_main_context_default (), WAKEUP_POLL,
			NULL, (G_IO_IN | G_IO_PRI),
			giop_mainloop_handle_input, NULL);
		
		g_private_set (giop_tdata_private, tdata);

		/* Setup thread pool for incoming requests */
		giop_thread_pool = g_thread_pool_new
			(giop_request_handler_thread, NULL, -1, FALSE, NULL);
		giop_pool_hash_lock = link_mutex_new ();
		giop_pool_hash = g_hash_table_new (NULL, NULL);
	}

	giop_tmpdir_init ();

	giop_send_buffer_init (blank_wire_data);
	giop_recv_buffer_init ();
}

static void
wakeup_mainloop (void)
{
	char c = 'A'; /* magic */
	int  res;
#ifdef HAVE_WINSOCK2_H
	if ((res = send (WAKEUP_WRITE, &c, sizeof (c), 0)) == SOCKET_ERROR) {
		res = -1;
		link_map_winsock_error_to_errno ();
	}
#else
	while ((res = write (WAKEUP_WRITE, &c, sizeof (c))) < 0  &&
	       errno == EINTR );
#endif
	if (res < 0 && errno == EAGAIN)
		return;
	if (res < 0)
		g_warning ("Failed to write to GIOP mainloop wakeup "
			   "pipe %d 0x%x(%d) (%d)",
			   res, errno, errno, WAKEUP_WRITE);
}

void
giop_incoming_signal_T (GIOPThread *tdata, GIOPMsgType t)
{
	g_cond_signal (tdata->incoming);

	if (t != GIOP_REPLY && tdata->wake_context)
		wakeup_mainloop ();
}

void
giop_invoke_async (GIOPMessageQueueEntry *ent)
{
	GIOPRecvBuffer *buf = ent->buffer;

	dprintf (GIOP, "About to invoke %p:%p (%d) (%p:%p)\n",
		 ent, ent->async_cb, giop_thread_io(),
		 ent->src_thread, giop_main_thread);

	if (!giop_thread_io ())
		ent->async_cb (ent);

	else if (ent->src_thread == giop_thread_self ())
		ent->async_cb (ent);

	else {
		GIOPThread *tdata = ent->src_thread;
		
		g_mutex_lock (tdata->lock); /* ent_lock */

		buf = NULL;
		tdata->async_ents = g_list_prepend (tdata->async_ents, ent);
		giop_incoming_signal_T (tdata, GIOP_REQUEST);
		
		g_mutex_unlock (tdata->lock); /* ent_unlock */
	}

	/* NB. At the tail end of async_cb 'Ent' is invalid / freed */
	giop_recv_buffer_unuse (buf);
}

static GMainLoop *giop_main_loop = NULL;

void
giop_main_run (void)
{
	if (giop_thread_io ()) {
		g_assert (giop_main_loop == NULL);
		giop_main_loop = g_main_loop_new (NULL, TRUE);
		g_main_loop_run (giop_main_loop);
		g_main_loop_unref (giop_main_loop);
		giop_main_loop = NULL;
	} else
		link_main_loop_run ();
}

void
giop_shutdown (void)
{
	link_connections_close ();

	if (link_loop) /* break into the linc loop */
		g_main_loop_quit (link_loop);
	if (giop_main_loop)
		g_main_loop_quit (giop_main_loop);

	if (giop_thread_safe ()) {
		if (giop_main_source) {
			g_source_destroy (giop_main_source);
			g_source_unref (giop_main_source);
			giop_main_source = NULL;
		}

		if (WAKEUP_WRITE >= 0) {
#ifdef HAVE_WINSOCK2_H
			closesocket (WAKEUP_WRITE);
			closesocket (WAKEUP_POLL);
#else
			close (WAKEUP_WRITE);
			close (WAKEUP_POLL);
#endif
			WAKEUP_WRITE = -1;
			WAKEUP_POLL = -1;
		}
	}
}

typedef struct {
	gpointer poa_object;
	gpointer recv_buffer;
} GIOPQueueEntry;

/* this sucks, we need a wider scale re-factor */
#include "../orb-core/orbit-policy.h"
#include "orbit/poa/poa-types.h"

static GList *
first_valid_request (GIOPThread *tdata, gboolean *no_policy)
{
	GList *l;
	ORBitPolicy *policy;

	if (!tdata->invoke_policies || !tdata->invoke_policies->head) {
		*no_policy = TRUE;
		return NULL;
	}

	*no_policy = FALSE;
	policy = g_queue_peek_head (tdata->invoke_policies);

	for (l = tdata->request_queue; l; l = l->next) {
		int i;
		GIOPQueueEntry *qe = l->data;
		ORBit_POAObject pobj = qe->poa_object;

		for (i = 0; i < policy->allowed_poas->len; i++)
			if (g_ptr_array_index (policy->allowed_poas, i) == pobj->poa)
				return l;
	}

	return NULL;
}

gboolean
giop_thread_queue_empty_T (GIOPThread *tdata)
{
	gboolean no_policy;
	
	if (first_valid_request (tdata, &no_policy))
		return FALSE;

	else if (no_policy)
		return (!tdata->request_queue &&
			!tdata->async_ents);

	else
		return TRUE;
}

static gpointer
giop_list_pop (GList **list)
{
	gpointer p;

	if (!*list)
		return NULL;
	
	p = (*list)->data;
	*list = g_list_delete_link (*list, *list);

	return p;
}

void
giop_thread_queue_process (GIOPThread *tdata)
{
	GIOPMessageQueueEntry *ent;
	GIOPQueueEntry *qe = NULL;
	GList   *request;
	gboolean no_policy;

	if (!tdata)
		tdata = giop_thread_self ();

	request = first_valid_request (tdata, &no_policy);

	dprintf (MESSAGES, "handle queued input [%p], (%d)\n", request, no_policy);

	LINK_MUTEX_LOCK (tdata->lock); /* ent_lock */

	if (no_policy)
		ent = giop_list_pop (&tdata->async_ents);
	else
		ent = NULL;

	if (!ent) {
		if (no_policy)
			qe = giop_list_pop (&tdata->request_queue);

		else if (request) {
			qe = request->data;
			tdata->request_queue = g_list_delete_link (tdata->request_queue, request);
		}
	}

	dprintf (MESSAGES, "Queue pop %p, %p, %d", ent, qe, no_policy);
	
	LINK_MUTEX_UNLOCK (tdata->lock); /* ent_unlock */

	if (ent)
		giop_invoke_async (ent);

	if (qe) {
		tdata->request_handler (qe->poa_object, qe->recv_buffer, NULL);
		g_free (qe);
	}
}

void
giop_thread_queue_tail_wakeup (GIOPThread *tdata)
{
	if (!tdata)
		return; /* FIXME: no I/O thread */

	LINK_MUTEX_LOCK (tdata->lock); /* ent_lock */

	if ((tdata->request_queue || tdata->async_ents) && tdata->wake_context)
		wakeup_mainloop ();

	LINK_MUTEX_UNLOCK (tdata->lock); /* ent_unlock */
}

void
giop_thread_request_push (GIOPThread *tdata,
			  gpointer   *poa_object,
			  gpointer   *recv_buffer)
{
	GIOPQueueEntry *qe;

	g_return_if_fail (tdata != NULL);
	g_return_if_fail (poa_object != NULL);
	g_return_if_fail (recv_buffer != NULL);

	qe = g_new (GIOPQueueEntry, 1);

	qe->poa_object  = *poa_object;
	qe->recv_buffer = *recv_buffer;
	*poa_object = NULL;
	*recv_buffer = NULL;

	LINK_MUTEX_LOCK (tdata->lock);

	tdata->request_queue = g_list_append (tdata->request_queue, qe);
	giop_incoming_signal_T (tdata, GIOP_REQUEST);

	LINK_MUTEX_UNLOCK (tdata->lock);
}

GIOPThread *
giop_thread_get_main (void)
{
	return giop_main_thread;
}

void
giop_thread_set_main_handler (gpointer request_handler)
{
	if (!giop_thread_safe ())
		return;
	g_assert (giop_main_thread != NULL);

	giop_main_thread->request_handler = request_handler;
}

void
giop_thread_new_check (GIOPThread *opt_self)
{
	if (!link_thread_safe ())
		return;

	if (!opt_self)
		opt_self = giop_thread_self ();

	if (opt_self &&
	    opt_self != giop_thread_get_main () &&
	    !link_thread_io ())
		link_set_io_thread (TRUE);
}
