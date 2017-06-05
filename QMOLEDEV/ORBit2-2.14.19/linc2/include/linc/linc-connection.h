/*
 * linc-connection.h: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#ifndef _LINK_CONNECTION_H_
#define _LINK_CONNECTION_H_

#include <glib.h>

G_BEGIN_DECLS

#ifdef G_OS_WIN32
#  include <winsock2.h>
#  undef interface		/* #defined as struct! */

#  define iovec _WSABUF
#  define iov_len len
#  define iov_base buf

#else
#  include <sys/uio.h>
#  include <netdb.h>
#endif

#include <linc/linc-types.h>
#include <linc/linc-protocol.h>

#define LINK_TYPE_CONNECTION            (link_connection_get_type())
#define LINK_TYPE_IS_CONNECTION(type)   (G_TYPE_FUNDAMENTAL (type) == LINK_TYPE_CONNECTION)
#define LINK_CONNECTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), LINK_TYPE_CONNECTION, LinkConnection))
#define LINK_CONNECTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), LINK_TYPE_CONNECTION, LinkConnectionClass))
#define LINK_IS_CONNECTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), LINK_TYPE_CONNECTION))
#define LINK_IS_CONNECTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), LINK_TYPE_CONNECTION))

typedef enum { LINK_CONNECTING, LINK_CONNECTED, LINK_DISCONNECTED, LINK_TIMEOUT } LinkConnectionStatus;
typedef enum { LINK_TIMEOUT_UNKNOWN, LINK_TIMEOUT_YES, LINK_TIMEOUT_NO } LinkTimeoutStatus;

typedef struct _LinkWriteOpts         LinkWriteOpts;
typedef struct _LinkConnectionPrivate LinkConnectionPrivate;

typedef struct {
	GObject                 parent;

	const LinkProtocolInfo *proto;

	LinkConnectionStatus    status;
	LinkConnectionOptions   options;
	guint                   was_initiated : 1;
	guint                   is_auth : 1;
	guint                   inhibit_reconnect : 1;

	gchar                  *remote_host_info;
	gchar                  *remote_serv_info;

	LinkConnectionPrivate  *priv;

	GSList                 *idle_broken_callbacks;

	GMutex                 *timeout_mutex;
	guint                   timeout_msec;
	guint                   timeout_source_id; // protected by timeout_mutex
	LinkTimeoutStatus       timeout_status;    // protected by timeout_mutex
	void                   *tdata;             // "do not pollute the namespace"-hack (it's a GIOPThread*)
} LinkConnection;

typedef struct {
	GObjectClass parent_class;

	void     (* state_changed) (LinkConnection      *cnx,
				    LinkConnectionStatus status);
	gboolean (* handle_input)  (LinkConnection      *cnx);

	/* signals */
	void     (* broken)        (LinkConnection      *cnx);
	/*
	 * Emitted when the buffer is emptied, half full or
	 * before disconnect
	 */
	void     (* blocking)      (LinkConnection      *cnx,
				    gulong               buffer_size);
} LinkConnectionClass;

GType    link_connection_get_type (void) G_GNUC_CONST;

void     link_connection_from_fd  (LinkConnection       *cnx,
				   int                   fd,
				   const LinkProtocolInfo *proto,
				   gchar                *remote_host_info,
				   gchar                *remote_serv_info,
				   gboolean              was_initiated,
				   LinkConnectionStatus  status,
				   LinkConnectionOptions options);

LinkConnection *link_connection_initiate (GType                 derived_type,
					  const char           *proto_name,
					  const char           *remote_host_info,
					  const char           *remote_serv_info,
					  LinkConnectionOptions options,
					  const char           *first_property,
					  ...);
LinkConnectionStatus link_connection_try_reconnect (LinkConnection *cnx);

LinkConnection *link_connection_ref   (LinkConnection *cnx);
void            link_connection_unref (LinkConnection *cnx);

typedef enum {
	LINK_IO_OK = 0,
	LINK_IO_FATAL_ERROR = -1,
	LINK_IO_QUEUED_DATA = -2
} LinkIOStatus;

glong        link_connection_read     (LinkConnection       *cnx,
				       guchar               *buf,
				       int                   len,
				       gboolean              block_for_full_read);

/* Return values from these functions are going to be "abnormal",
   since they make sure to write all the data out */
LinkIOStatus link_connection_write    (LinkConnection       *cnx,
				       const guchar         *buf,
				       gulong                len,
				       const LinkWriteOpts  *opt_write_opts);

LinkIOStatus link_connection_writev   (LinkConnection       *cnx,
				       struct iovec         *vecs,
				       int                   nvecs,
				       const LinkWriteOpts  *opt_write_opts);

void         link_connection_state_changed (LinkConnection      *cnx,
					    LinkConnectionStatus status);

LinkConnectionStatus link_connection_get_status     (LinkConnection *cnx);
void                 link_connection_disconnect     (LinkConnection *cnx);
LinkConnectionStatus link_connection_wait_connected (LinkConnection *cnx);

/*
 * Proposed new blocking API ...
 */
void           link_connection_set_max_buffer    (LinkConnection *cnx,
						  gulong          max_buffer_bytes);
LinkWriteOpts *link_write_options_new            (gboolean        block_on_write);
/* Space for future expansion: timeout, individual msg. buffering constraints etc. */
void           link_write_options_free           (LinkWriteOpts  *write_opts);


typedef void (*LinkBrokenCallback) (LinkConnection *, gpointer user_data);
void           link_connection_add_broken_cb     (LinkConnection    *cnx,
						  LinkBrokenCallback fn,
						  gpointer           user_data);
void           link_connection_remove_broken_cb  (LinkConnection    *cnx,
						  LinkBrokenCallback opt_fn,
						  gpointer           opt_user_data);

void           link_connections_close            (void);

/* set the link timeout in miliseconds */
extern void link_set_timeout (guint msec);

G_END_DECLS

#endif /* _LINK_CONNECTION_H */
